module Parser where

    import TokenTypes ( Token (tokenType, lexeme), makeEOF, TokenType (..)  )
    import Error 
    import Expr
    -- import Control.Monad
    import Control.Monad.State
    import Control.Monad.Except ( ExceptT(..), runExceptT, throwError) 



    data ParserState = ParserState {
        tokens:: [Token],
        index :: Int
    } deriving (Show, Eq)

    type Parser a = ExceptT InterpreterError (State ParserState) a
    -- newtype Parser a = Parser {runParse :: ParserState -> Either ErrInfo (a, ParserState)}

    parse :: [Token] -> Either InterpreterError Expr
    parse ls = 
        case runState (runExceptT expression) (ParserState ls 0) of
            (Left err,_) -> Left err
            (Right x,_)  -> Right x

    statements :: Parser [Expr]
    statements = ifM isAtEnd (return []) (do
        x  <- statement
        xs <- statements
        return (x:xs) )
    
    statement :: Parser Expr
    statement = undefined

    expression :: Parser Expr
    expression = equality

    -- Common function to parse binary expressions
    binexp :: Parser Expr -> [TokenType] -> (Expr -> Token -> Expr -> Expr) -> Parser Expr
    binexp next ts constr = do
        expr <- next
        read_loop expr
        where
            read_loop ex = ifM (match ts) (do
                tok <- previous
                ex2 <- next
                read_loop $ constr ex tok ex2) (return ex)

    -- parses a == b != c form of expressions too.
    equality :: Parser Expr
    equality  = binexp comparison [BANG_EQUAL, EQUAL_EQUAL] Log
    

    comparison :: Parser Expr
    comparison = binexp term [GREATER, GREATER_EQUAL, LESS, LESS_EQUAL] Log

    term :: Parser Expr
    term = binexp factor [MINUS, PLUS] Binary

    factor :: Parser Expr
    factor = binexp unary [SLASH, STAR] Binary

    unary :: Parser Expr
    unary = ifM (match [BANG, MINUS]) (do
        op    <- previous
        Unary op <$> unary) primary

    primary :: Parser Expr
    primary = do
        st <- peek
        let tt = tokenType st in
            case tt of
                FALSE  -> do { incPointer; return $ Literal $ Bool False}
                TRUE   -> do { incPointer; return $ Literal $ Bool True}
                NIL    -> do { incPointer; return $ Literal   Nil}
                NUMBER -> do { incPointer; return $ Literal $ Number $ read (lexeme st)}
                STRING -> do { incPointer; return $ Literal $ String $ lexeme st}
                LEFT_PAREN -> do
                    _    <- incPointer
                    expr <- expression
                    _    <- consume RIGHT_PAREN $ "Expected ')' after expression: "++ show expr
                    return $ Group expr
                _      -> throwError $ ParserError $ "Expected Expression: `"++ show st ++ "`."
            

    consume :: TokenType -> String -> Parser ()
    consume t s = ifM (match [t]) (return ()) (throwError $ ParserError s)


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

    synchronize :: Parser ()
    synchronize = do
        incPointer
        x <- previous 
        if tokenType x == SEMICOLON then 
            return ()
        else ifM isAtEnd (return ()) (do
            tt <- peek
            case tokenType tt of
                CLASS  -> return ()
                FUN    -> return ()
                VAR    -> return ()
                FOR    -> return ()
                IF     -> return ()
                WHILE  -> return ()
                PRINT  -> return ()
                RETURN -> return ()
                _ -> synchronize)
