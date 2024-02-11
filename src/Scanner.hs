module Scanner (parse) where
    import TokenTypes ( Token(Token), TokenType(..), makeEOF )
    import Error ( InterpreterError(ScannerError), makeErr )
    -- import Control.Applicative -- for Applicative instance
    import Control.Monad (liftM, ap, when) --for functor and applicative instances
    import qualified Data.Char as C
    import qualified Data.Map as M


    data ScanState = ScanState {
        contents :: String,
        start    :: Int,
        current  :: Int,
        line     :: Int,
        tokens   :: [Token],
        filename :: String,
        errors   :: [InterpreterError]
    } -- deriving (Show)

    instance Show ScanState where
        show (ScanState _ _ c l toks fn _) =
            "filename: "++fn++"\n"++"index: "++ show l++"\n" ++ "current: "++ show c ++ "\n"++ concatMap (\t -> show t ++ "\n") toks

    newtype Lexer a = Lexer {
        runLexer :: ScanState -> Either InterpreterError (a, ScanState)
    }

    -- scanTokens :: ScanState -> (a, ScanState)
    -- scanTokens = undefined

    identity :: a -> Lexer a
    identity a = Lexer (\s -> Right (a, s))

    getState :: Lexer ScanState
    getState = Lexer (\s -> Right (s,s))

    putState :: ScanState -> Lexer ()
    putState s = Lexer (\_ -> Right ((), s))

    -- doing throw catch with custom implementation
    bail :: String -> Lexer a --works like throwError
    bail err = Lexer $ \s -> Left  $ ScannerError (makeErr (filename s) (line s) $ err ++ "\n buffer: \'" ++slice (contents s) (start s) (current s)++"\'")

    mycatch :: Lexer a -> (InterpreterError -> Lexer a) -> Lexer a
    mycatch m f = Lexer $ \s ->
        case runLexer m s of
            Left err -> runLexer (f err) s
            Right (x,st) -> Right (x,st)

    instance Functor Lexer where
        fmap = liftM

    instance Applicative Lexer where
        pure    = identity
        (<*>)   = ap

    instance Monad Lexer where
        return = pure
        m >>= g = Lexer $ \x ->
            case runLexer m x of
                Left err      -> Left err
                Right (res,y) -> runLexer (g res) y

    ifM :: Monad m => m Bool -> m a -> m a -> m a
    ifM bt m_t m_f = do
        v <- bt
        if v then
            m_t
        else m_f


    parse :: String -> String -> Either [InterpreterError] [Token]
    parse fn initState =
        case runLexer scanTokens (ScanState initState 0 0 1 [] fn []) of
            Left err          -> Left [err]
            Right (result, s) -> handle result s
        where 
            handle r s
                | not (null (errors s)) = Left (errors s)
                | otherwise = Right r

    scanTokens :: Lexer [Token]
    scanTokens = ifM isAtEnd addeof next
        where
            addeof = do
                updateStart
                _  <- addToken EOF
                reverse . tokens <$> getState
            next   = do
                updateStart -- we only update start when scanning a new token
                _ <- mycatch scanTok (\e -> do
                    x <- getState
                    putState (x {errors= e: errors x})
                    incCurrent -- skip the illegal char
                    return makeEOF
                    )
                scanTokens

    updateStart :: Lexer ()
    updateStart = do
        p <- getState
        putState (p {start = current p})

    incLine :: Lexer ()
    incLine = do
        p <- getState
        putState (p {line = line p + 1})

    incCurrent :: Lexer ()
    incCurrent = getState >>= \p ->
        putState (p {current = current p +1})

    isAtEnd :: Lexer Bool
    isAtEnd = do
        p <- getState
        if current p >= length (contents p) then
            return True
        else
            return False

    -- in the book this is called the advance method
    read1 :: Lexer Char
    read1 = do
        p <- getState
        let c = contents p !! current p in
            do
                incCurrent
                return c

    peek :: Lexer Char
    peek = ifM isAtEnd (return '\0') (getState >>= \p ->  return (contents p !! current p))

    slice :: String -> Int -> Int -> String
    slice s from to = take (to-from) (drop from s)

    addToken :: TokenType -> Lexer Token
    addToken tok = do
        p <- getState
        let token = Token tok (slice (contents p) (start p) (current p)) (start p) (current p - start p) in
            do
                putState (p {tokens = token: tokens p})
                return token

    scanTok :: Lexer Token
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
            _    -> cond [(C.isAlpha c || c == '_', identifier),
                            (C.isDigit c, number)] (bail $ "Illegal char \'"++[c]++"\'")

    identifier :: Lexer Token
    identifier = scanWhile (\x->C.isAlphaNum x || x == '_') >> do
        st <- getState
        let s = slice (contents st) (start st) (current st) in
            case M.lookup s keywords of
                Just x  -> addToken x
                Nothing -> addToken IDENT

    number :: Lexer Token
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
    string :: Lexer String
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


    scanWhile :: (Char -> Bool) -> Lexer String
    scanWhile check = do
        ch <- peek
        if check ch
            then do
                c <- read1
                cs <- scanWhile check
                return (c:cs)
            else return ""
        -- when (check ch) $ read1 >> scanWhile check

    cond :: [(Bool, Lexer Token)] -> Lexer Token -> Lexer Token
    cond []    d      = d -- default action
    cond ((c,p):xs) d = if c then p else cond xs d

    checkNext :: Char -> Lexer Bool
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
