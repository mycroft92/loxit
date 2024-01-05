module Scanner where
    import TokenTypes
    import Error
    -- import Control.Applicative -- for Applicative instance
    import Control.Monad (liftM, ap) --for functor and applicative instances
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

    newtype Parse a = Parse {
        runParse :: ParseState -> (a, ParseState)
    }

    newtype ParseRes a = Either ErrInfo (Parse a)
    -- scanTokens :: ParseState -> (a, ParseState)
    -- scanTokens = undefined

    identity :: a -> Parse a
    identity a = Parse (\s -> (a, s))

    identityRes :: a -> ParseRes a
    identityRes = Right . identity

    getState :: Parse ParseState
    getState = Parse (\s -> (s,s))

    -- getStateRes :: ParseRes ParseState
    -- getStateRes = Right . getState

    putState :: ParseState -> Parse ()
    putState s = Parse (\_ -> ((), s))

    instance Functor Parse where
        fmap = liftM
    
    instance Functor ParseRes where
        fmap = liftM

    instance Applicative Parse where
        pure    = identity
        (<*>)   = ap

    instance Applicative ParseRes where
        pure    = identityRes
        (<*>)   = ap

    instance Monad Parse where
        return = pure
        m >>= g = Parse $ \x ->
            let (res, st) = runParse m x in
                runParse (g res) st

    instance Monad ParseRes where
        return = pure
        m >>= g = case m of
            Left err -> Left err
            Right p  -> Right p

    -- instance MonadFail Parse where

    ifM :: Monad m => m Bool -> m a -> m a -> m a
    ifM bt m_t m_f = do
        v <- bt
        if v then
            m_t
        else m_f


    -- parse :: String -> String -> Either ErrInfo [Token]
    -- parse initState fn =
    --     case runParse scanTokens (ParseState initState 0 0 1 [] fn) of
    --         Left err          -> Left err
    --         Right (result, _) -> Right result

    scanTokens :: Parse [Token]
    scanTokens = ifM isAtEnd addeof next
        where
            addeof = do
                _  <- addToken EOF
                reverse . tokens <$> getState
            next   = do
                updateStart
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
            '(' -> addToken LEFT_PAREN
            ')' -> addToken RIGHT_PAREN
            '{' -> addToken LEFT_BRACE
            '}' -> addToken RIGHT_BRACE
            ',' -> addToken COMMA
            '.' -> addToken DOT
            '-' -> addToken MINUS
            '+' -> addToken PLUS
            ';' -> addToken SEMICOLON
            '/' -> addToken SLASH
            '*' -> addToken STAR
            '!' -> ifM (checkNext '=') (read1 >>= \_ -> addToken BANG_EQUAL) (addToken BANG)
            '=' -> ifM (checkNext '=') (read1 >>= \_ -> addToken EQUAL_EQUAL) (addToken EQUAL)
            '>' -> ifM (checkNext '=') (read1 >>= \_ -> addToken GREATER_EQUAL) (addToken GREATER)
            '<' -> ifM (checkNext '=') (read1 >>= \_ -> addToken LESS_EQUAL) (addToken LESS)
            -- _   -> (\_ -> do 
            --     p <- getState
            --     Left $ makeErr (filename p) (line p) ("Illegal Character: "++[c]))

    cond :: [(Bool, Parse Token)] -> Parse Token -> Parse Token
    cond []    d      = d
    cond ((c,p):xs) d = if c then p else cond xs d

    checkNext :: Char -> Parse Bool
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
