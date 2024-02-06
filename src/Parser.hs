module Parser where

    import TokenTypes ( Token (tokenType, lexeme), TokenType (..)  )
    import Error
    import Expr
    -- import Control.Monad
    import Control.Monad.State
    import Control.Monad.Except ( ExceptT(..), runExceptT, throwError, catchError)




    data ParserState = ParserState {
        tokens :: [Token],
        index  :: Int,
        errors :: [InterpreterError]
    } deriving (Show, Eq)

    type Parser a = ExceptT InterpreterError (State ParserState) a
    -- newtype Parser a = Parser {runParse :: ParserState -> Either ErrInfo (a, ParserState)}

    parse :: [Token] -> Either [InterpreterError] [Decl]
    parse ls =
        case runState (runExceptT declarations) (ParserState ls 0 []) of
            (Left err,_) -> Left [err]
            (Right x,s)  -> handle x s
        where
            handle x s
                | not (null (errors s)) = Left $ errors s
                | otherwise = Right x

    declarations :: Parser [Decl]
    declarations = ifM isAtEnd (return []) (do
        x  <- declaration
        xs <- declarations
        return (x:xs) )

    declaration :: Parser Decl
    declaration = catchError par (\e -> addError e >> Statement . Expression . Literal <$> synchronize)
        where par = ifM (match [VAR]) (Decl <$> varDecl) (Statement <$> statement)

    varDecl :: Parser VarDecl
    varDecl = do
        x <- varDecl'
        _ <- consume SEMICOLON "Expected ';' after var decl and before: "
        return x

    varDecl' :: Parser VarDecl
    varDecl' = do
        x <- consume IDENT "Expected variable name"
        ifM (match [EQUAL]) (do
            DeclE x <$> expression) (return $ OnlyDecl x)


    -- statements :: Parser [Stmt]
    -- statements = ifM isAtEnd (return []) (do
    --     x  <- statement
    --     xs <- statements
    --     return (x:xs) )
    addError :: InterpreterError -> Parser ()
    addError e = do
        st <- lift get
        lift $ put (st {errors = e:errors st})

    statement :: Parser Stmt
    statement = ifM (match [PRINT]) printStmt exprStmt

    printStmt :: Parser Stmt
    printStmt = do
        x <- Print <$> expression
        consumeSemi x

    consumeSemi :: a -> Parser a
    consumeSemi x = do
        _ <- consume SEMICOLON "Expected ';' after value: "
        return x

    exprStmt :: Parser Stmt
    exprStmt = do
        x <- expression
        consumeSemi (Expression x)

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
                IDENT  -> do { incPointer; return $ Var st}
                LEFT_PAREN -> do
                    _    <- incPointer
                    expr <- expression
                    _    <- consume RIGHT_PAREN $ "Primary Expected ')' after expression: "++ show expr
                    return $ Group expr
                _      -> throwError $ ParserError $ "Primary Expected Expression: `"++ show st ++ "`."


    consume :: TokenType -> String -> Parser Token
    consume t s = ifM (match [t]) previous (do
        x <- current
        throwError $ ParserError $ s ++ " Token: "++ show x)


    ifM :: Monad m => m Bool -> m a -> m a -> m a
    ifM bt m_t m_f = do
        v <- bt
        if v then
            m_t
        else m_f

    peek :: Parser Token
    peek = current

    current :: Parser Token
    current = lift get >>= \p -> return (tokens p !! index p)


    isAtEnd :: Parser Bool
    isAtEnd = do
        st <- peek
        if tokenType st == EOF then return True else return False


    incPointer :: Parser ()
    incPointer = ifM isAtEnd (return ()) (lift get >>= \p -> lift $ put (p {index = index p +1}))

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

    synchronize :: Parser Value
    synchronize = do
        incPointer
        x <- previous
        if tokenType x == SEMICOLON then
            return Nil
        else ifM isAtEnd (return Nil) (do
            tt <- peek
            case tokenType tt of
                CLASS  -> return Nil
                FUN    -> return Nil
                VAR    -> return Nil
                FOR    -> return Nil
                IF     -> return Nil
                WHILE  -> return Nil
                PRINT  -> return Nil
                RETURN -> return Nil
                _ -> synchronize)
