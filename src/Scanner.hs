module Scanner where
    import TokenTypes
    import Error ( ErrInfo, makeErr )
    -- import Control.Applicative -- for Applicative instance
    import Control.Monad (liftM, ap, when) --for functor and applicative instances
    import qualified Data.Char as C
    import qualified Data.Map as M


    data ParseState = ParseState {
        contents :: String,
        start    :: Int,
        current  :: Int,
        line     :: Int,
        tokens   :: [Token],
        filename :: String
    } -- deriving (Show)

    instance Show ParseState where
        show (ParseState _ _ c l toks fn) =
            "filename: "++fn++"\n"++"index: "++ show l++"\n" ++ "current: "++ show c ++ "\n"++ concatMap (\t -> show t ++ "\n") toks

    newtype Parse a = Parse {
        runParse :: ParseState -> Either ErrInfo (a, ParseState)
    }

    -- scanTokens :: ParseState -> (a, ParseState)
    -- scanTokens = undefined

    identity :: a -> Parse a
    identity a = Parse (\s -> Right (a, s))

    getState :: Parse ParseState
    getState = Parse (\s -> Right (s,s))

    putState :: ParseState -> Parse ()
    putState s = Parse (\_ -> Right ((), s))

    bail :: String -> Parse a
    bail err = Parse $ \s -> Left (makeErr (filename s) (line s) $ err ++ "\n buffer: \'" ++slice (contents s) (start s) (current s)++"\'")

    instance Functor Parse where
        fmap = liftM

    instance Applicative Parse where
        pure    = identity
        (<*>)   = ap

    instance Monad Parse where
        return = pure
        m >>= g = Parse $ \x ->
            case runParse m x of
                Left err      -> Left err
                Right (res,y) -> runParse (g res) y

    ifM :: Monad m => m Bool -> m a -> m a -> m a
    ifM bt m_t m_f = do
        v <- bt
        if v then
            m_t
        else m_f


    parse :: String -> String -> Either ErrInfo [Token]
    parse fn initState =
        case runParse scanTokens (ParseState initState 0 0 1 [] fn) of
            Left err          -> Left err
            Right (result, _) -> Right result

    scanTokens :: Parse [Token]
    scanTokens = ifM isAtEnd addeof next
        where
            addeof = do
                updateStart
                _  <- addToken EOF
                reverse . tokens <$> getState
            next   = do
                updateStart -- we only update start when scanning a new token
                _ <- scanTok
                scanTokens

    updateStart :: Parse ()
    updateStart = do
        p <- getState
        putState (p {start = current p})

    incLine :: Parse ()
    incLine = do
        p <- getState
        putState (p {line = line p + 1})

    incCurrent :: Parse ()
    incCurrent = getState >>= \p ->
        putState (p {current = current p +1})

    isAtEnd :: Parse Bool
    isAtEnd = do
        p <- getState
        if current p >= length (contents p) then
            return True
        else
            return False

    read1 :: Parse Char
    read1 = do
        p <- getState
        let c = contents p !! current p in
            do
                incCurrent
                return c

    peek :: Parse Char
    peek = ifM isAtEnd (return '\0') (getState >>= \p ->  return (contents p !! current p))

    slice :: String -> Int -> Int -> String
    slice s from to = take (to-from) (drop from s)

    addToken :: TokenType -> Parse Token
    addToken tok = do
        p <- getState
        let token = Token tok (slice (contents p) (start p) (current p)) (start p) (current p - start p) in
            do
                putState (p {tokens = token: tokens p})
                return token

    -- might need peek and peekNext
    scanTok :: Parse Token
    scanTok = do
        c <- read1
        case c of
            '('  -> addToken LEFT_PAREN
            ')'  -> addToken RIGHT_PAREN
            '{'  -> addToken LEFT_BRACE
            '}'  -> addToken RIGHT_BRACE
            ','  -> addToken COMMA
            '.'  -> addToken DOT
            '-'  -> addToken MINUS
            '+'  -> addToken PLUS
            ';'  -> addToken SEMICOLON
            '*'  -> addToken STAR
            '!'  -> ifM (checkNext '=') (read1 >> addToken BANG_EQUAL) (addToken BANG)
            '='  -> ifM (checkNext '=') (read1 >> addToken EQUAL_EQUAL) (addToken EQUAL)
            '>'  -> ifM (checkNext '=') (read1 >> addToken GREATER_EQUAL) (addToken GREATER)
            '<'  -> ifM (checkNext '=') (read1 >> addToken LESS_EQUAL) (addToken LESS)
            '/'  -> ifM (checkNext '/') (scanWhile (/= '\n') >> return throwaway) (addToken SLASH)
            ' '  -> return throwaway
            '\n' -> do
                incLine
                return throwaway
            '\r' -> return throwaway
            '\t' -> return throwaway
            '"'  -> updateStart >> string >> return throwaway -- this weird way is for avoiding outer ""
            _    -> cond [(C.isAlpha c, identifier),
                            (C.isDigit c, number)] (bail $ "Illegal char \'"++[c]++"\'")

    identifier :: Parse Token
    identifier = scanWhile C.isAlphaNum >> do
        st <- getState
        let s = slice (contents st) (start st) (current st) in
            case M.lookup s keywords of
                Just x  -> addToken x
                Nothing -> addToken IDENT

    number :: Parse Token
    number = readNum1 >> addToken NUMBER
        where
            readNum1 = do
                c <- peek
                if C.isDigit c then do
                    _ <- read1
                    readNum1
                else when (c == '.') $ do
                    _ <- read1
                    readNum2
            readNum2 = do
                c <- peek
                when (C.isDigit c) $ do
                    _ <- read1
                    readNum2

    -- this function assumes that the first caharacter is already parsed as '"'
    string :: Parse String
    string = ifM isAtEnd (bail "Unterminated String") (do
        ch <- peek
        if ch == '\n' then do
            incLine
            c  <- read1
            cs <- string
            return (c:cs)
        else if ch == '"' then do
            _ <- addToken STRING
            _ <- read1 -- read and throway the outer '"'
            return ""
        else do
            c <- read1
            cs <- string
            return (c:cs))

    throwaway :: Token
    throwaway = Token WS "" 0 0


    scanWhile :: (Char -> Bool) -> Parse String
    scanWhile check = do
        ch <- peek
        if check ch
            then do
                c <- read1
                cs <- scanWhile check
                return (c:cs)
            else return ""
        -- when (check ch) $ read1 >> scanWhile check

    cond :: [(Bool, Parse Token)] -> Parse Token -> Parse Token
    cond []    d      = d -- default action
    cond ((c,p):xs) d = if c then p else cond xs d

    checkNext :: Char -> Parse Bool
    -- Checks if the next character in the buffer is c
    checkNext c = do
        a <- peek
        return (a == c)


    keywords :: M.Map String TokenType
    keywords = M.fromList [("var", VAR),
                ("while", WHILE),
                ("and", AND),
                ("class", CLASS),
                ("else", ELSE),
                ("false", FALSE),
                ("fun", FUN),
                ("for", FOR),
                ("if", IF),
                ("nil", NIL),
                ("or", OR),
                ("print", PRINT),
                ("return", RETURN),
                ("super", SUPER),
                ("this", THIS),
                ("true", TRUE),
                ("var", VAR),
                ("while", WHILE)]
