module Scanner where
    import TokenTypes
    import Error
    -- import Control.Applicative -- for Applicative instance
    import Control.Monad (liftM, ap) --for functor and applicative instances
    import qualified Data.Char as C


    data ParseState = ParseState {
        contents :: String,
        start    :: Int,
        current  :: Int,
        line     :: Int,
        tokens   :: [Token]
    } deriving (Show)

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
    
    -- beauty of types
    parse :: String -> Either ErrInfo [Token]
    parse initState =
        case runParse scanTokens (ParseState initState 0 0 1 []) of
            Left err          -> Left err
            Right (result, _) -> Right result

    scanTokens :: Parse [Token]
    scanTokens = ifM isAtEnd addeof next
        where
            addeof = do
                _  <- addToken EOF
                tokens <$> getState
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
