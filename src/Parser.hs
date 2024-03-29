module Parser where

    import TokenTypes ( Token (tokenType, lexeme), TokenType (..)  )
    import Error
    import Expr
    -- import Control.Monad
    import Control.Monad.State
        ( State,
      runState,
      MonadState(get, put),
      MonadTrans(lift) )
    import Control.Monad.Except ( ExceptT(..), runExceptT, throwError, catchError)

    data ParserState = ParserState {
        tokens :: [Token],
        index  :: Int,
        errors :: [InterpreterError]
    } deriving (Show, Eq)

    type Parser a = ExceptT InterpreterError (State ParserState) a

    parse :: [Token] -> Either [InterpreterError] [Decl]
    parse ls =
        case runState (runExceptT declarations) (ParserState ls 0 []) of
            (Left err, _)  -> Left [err]
            (Right  x, s)  -> handle x s
        where
            handle x s
                | not (null (errors s)) = Left $ errors s
                | otherwise = Right x

    declarations :: Parser [Decl]
    declarations = ifM isAtEnd (return []) (do
        x  <- declaration
        xs <- declarations
        return (x:xs))

    declaration :: Parser Decl
    declaration = catchError par (\e -> addError e >> Statement . Expression . Literal <$> synchronize)
        where par = cond [
                (match [VAR], Decl <$> varDecl),
                (match [FUN], functionStmt)] (Statement <$> statement)

    functionStmt :: Parser Decl
    functionStmt = do
        name <- consume IDENT "Expect function name."
        _    <- consume LEFT_PAREN "Expect '(' after function name."
        ifM (match [RIGHT_PAREN]) (parseBody name []) (do
            args <- parseArgs name
            parseBody name args)
        where
            parseArgs :: Token -> Parser [Token]
            parseArgs name = do
                arg <- consume IDENT $ "Expect parameter name in function def: "++show name
                args <- loop name [arg]
                if length args > 255 
                    then throwError $ ParserError $ "Cannot have more than 255 args, function declaration: "++show name
                    else return args
            loop :: Token -> [Token] -> Parser [Token]
            loop name args = ifM (match [COMMA]) (do
                arg <- consume IDENT $ "Expect parameter name in function def: "++show name
                loop name (arg:args)) (do
                    _ <- consume RIGHT_PAREN $ "Expect ')' after parameters in function def: "++show name
                    return args)
            parseBody :: Token -> [Token] -> Parser Decl
            parseBody name args = do
                _ <- consume LEFT_BRACE $ "Expect '{' before function: "++show name ++ " body" -- block assumes '{' is consumed
                Fn name args <$> block



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

    addError :: InterpreterError -> Parser ()
    addError e = do
        st <- lift get
        lift $ put (st {errors = e:errors st})

    statement :: Parser Stmt
    statement = cond [(match [PRINT], printStmt),
        (match [RETURN], retStmt),
        (match [LEFT_BRACE], block),
        (match [IF], ite),
        (match [WHILE], whileStmt),
        (match [FOR], forStmt)] exprStmt



    forStmt :: Parser Stmt
    forStmt = do
        _ <- consume LEFT_PAREN "Expected '(' after 'for'."
        ifM (match [SEMICOLON]) (parseCond . Statement . Expression . Literal$ Nil) (
            ifM (match [VAR]) (
                do
                    init <- varDecl -- semicolon parsing is subsumed here
                    parseCond $ Decl init
            ) (do
                init <- exprStmt -- semicolon parsing is subsumed here
                parseCond $ Statement init))
        where
            parseCond init = ifM (match [SEMICOLON]) (parseIncr init $ Literal Nil) (do
                condition <- expression
                _ <- consume SEMICOLON "Expect ';' after 'for' loop condition."
                parseIncr init condition)
            parseIncr init condition = ifM (match [RIGHT_PAREN]) (parseBody init condition (Literal Nil)) (do
                incr <- expression
                _ <- consume RIGHT_PAREN "Expect ')' after for clauses."
                parseBody init condition incr)
            parseBody init condition incr = do
                body <- statement
                handler init condition incr body
            -- write the desugaring logic here
            handler init condition incr body =
                case incr of
                    (Literal Nil) -> handleCond init condition body
                    _ -> handleCond init condition $ Block [Statement body, Statement $ Expression incr]
            handleCond init condition body =
                case condition of
                    (Literal Nil) -> handleInit init (While (Literal $ Bool True)  body)
                    _             -> handleInit init (While condition body)
            handleInit init body =
                case init of
                    (Statement (Expression  (Literal Nil))) -> return body
                    _ -> return $ Block [init, Statement body]


    whileStmt :: Parser Stmt
    whileStmt = do
        _ <- consume LEFT_PAREN "Expected '(' after 'while'."
        condition <- expression
        _ <- consume RIGHT_PAREN "Expected ')' after 'while' condition."

        While condition <$> statement

    ite :: Parser Stmt
    ite = do
        _ <- consume LEFT_PAREN "Expected '(' after 'if'. "
        condition <- expression
        _ <- consume RIGHT_PAREN "Expected ')' after 'if' condition. "
        stmt <- statement
        ifM (match [ELSE]) (ITE condition stmt . Just <$> statement) (return $ ITE condition stmt Nothing)

    block :: Parser Stmt
    block = do
        xs <- loop []
        _ <- consume RIGHT_BRACE "Expected '}' before value:"
        return $ Block xs
        where
            loop xs = do
                end <- isAtEnd
                b   <- check RIGHT_BRACE
                if not (b ||end)
                    then do
                        x <- declaration
                        loop $ xs ++ [x]
                    else return xs


    cond :: [(Parser Bool, Parser a)] -> Parser a -> Parser a
    cond [] d = d
    cond ((c,a):xs) d = ifM c a (cond xs d)

    printStmt :: Parser Stmt
    printStmt = do
        x <- Print <$> expression
        consumeSemi x
    
    retStmt :: Parser Stmt
    retStmt = do
        x <- Return <$> expression
        consumeSemi x

    consumeSemi :: a -> Parser a
    consumeSemi x = do
        _ <- consume SEMICOLON "Expected ';' before value: "
        return x

    exprStmt :: Parser Stmt
    exprStmt = do
        x <- expression
        consumeSemi (Expression x)

    assignment :: Parser Expr
    assignment = do
        x <- logicalor
        ifM (match [EQUAL]) (do
            eq <- previous
            v  <- assignment
            case x of
                Var z -> return $ Assign z v
                _     -> throwError $ ParserError $ "Invalid assign l-value: "++ show x ++" before: `"++ show eq ++ "`." )
            (return x)

    logicalor :: Parser Expr
    logicalor = binexp logicaland [OR] Log

    logicaland :: Parser Expr
    logicaland = binexp equality [AND] Log

    expression :: Parser Expr
    expression = assignment

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
        Unary op <$> unary) call

    call :: Parser Expr
    call = do
        prim <- primary
        loop prim
        where
            loop expr = ifM (match [LEFT_PAREN]) (finishCall expr) (return expr)
            finishCall expr = ifM (match [RIGHT_PAREN]) (return $ Call expr []) (do
                first <- expression
                args <- commaCollector [first]
                _  <- consume RIGHT_PAREN "Expect ')' after call arguments."
                return $ Call expr args)
            commaCollector args = ifM (match [COMMA]) (do
                expr <- expression
                if length args >= 255
                    then throwError $ ParserError "Can't have more than 255 call arguments!"
                    else commaCollector (expr:args))  (return args)


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

    peek :: Parser Token
    peek = current

    current :: Parser Token
    current = lift get >>= \p -> return (tokens p !! index p)

    check :: TokenType -> Parser Bool
    check tt = do
        x <- peek
        if tokenType x == tt
            then return True
            else return False

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