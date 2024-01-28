module Parser where

    import TokenTypes ( Token (tokenType), makeEOF, TokenType (BANG_EQUAL, EQUAL_EQUAL)  )
    import Error (ErrInfo)
    import Expr
    import Control.Monad
    import Control.Monad.State
    import Control.Monad.Except


    data ParserState = ParserState {
        tokens:: [Token],
        index :: Int
    } deriving (Show, Eq)

    type Parser a = ExceptT ErrInfo (State ParserState) a
    -- newtype Parser a = Parser {runParse :: ParserState -> Either ErrInfo (a, ParserState)}

    -- parses a == b != c form of expressions too.
    equality :: Parser Expr
    equality = do
        expr <- comparison
        read_loop expr
        where
            read_loop ex = ifM (match [BANG_EQUAL, EQUAL_EQUAL]) (do
                tok  <- previous
                Log ex tok <$> equality) (return ex)



    comparison :: Parser Expr
    comparison = undefined

    ifM :: Monad m => m Bool -> m a -> m a -> m a
    ifM bt m_t m_f = do
        v <- bt
        if v then
            m_t
        else m_f

    peek :: Parser Token
    peek = ifM isAtEnd (return makeEOF) (lift get >>= \p -> return (tokens p !! index p))


    isAtEnd :: Parser Bool
    isAtEnd = do
        st <- lift get
        return (index st >= length (tokens st))

    incPointer :: Parser ()
    incPointer = lift get >>= \p -> lift $ put (p {index = index p +1})

    readToken :: Parser Token
    readToken = do
        st <- lift get
        incPointer
        return (tokens st !! index st)

    previous :: Parser Token
    previous = do
        st <- lift get
        return (tokens st !! (index st - 1))

    match :: [TokenType] -> Parser Bool
    match []     = return False
    match (x:xs) =
        do
            tok <- peek
            if x == tokenType tok then
                do
                    _ <- readToken
                    return True
            else match xs

    