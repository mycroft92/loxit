module Evaluator where
    import Error (InterpreterError(..))
    import Expr (Decl(..),
      Expr(..),
      Stmt(..),
      Value(..),
      VarDecl(..) )
    import TokenTypes (Token(tokenType, lexeme),
      TokenType(..))
    import Control.Monad.State (StateT(runStateT),
      MonadIO(liftIO),
      MonadState(get, put),
      MonadTrans(lift))
    import Control.Monad.Except (ExceptT(..), runExceptT, throwError, catchError)
    import Environment (Env (..), define, getVar, assign, createChildEnv)
    import Data.IORef  

    data InterpreterState = InterpreterState {
        env :: IORef Env,
        errors :: [InterpreterError]
    }

    type Interpreter a = ExceptT InterpreterError (StateT InterpreterState IO) a


    declEvaluator:: [Decl] -> Interpreter Value
    declEvaluator []     = return Nil
    declEvaluator [x]    = declEval x
    declEvaluator (x:xs) = do
        _ <- declEval x
        declEvaluator xs

    declEval :: Decl -> Interpreter Value
    declEval (Statement s) = stmtEval s
    declEval (Decl v)      = varDeclEval v

    getEnv :: Interpreter Env
    getEnv = do
        en  <- lift get
        liftIO $ readIORef (env en)

    putEnv :: Env -> Interpreter ()
    putEnv ev = do
        x <- lift get
        liftIO $ writeIORef (env x) ev    

    varDeclEval :: VarDecl -> Interpreter Value
    varDeclEval (OnlyDecl v) = do
        x <- getEnv
        _ <- liftIO $ define (lexeme v) Nil x
        return Nil

    varDeclEval (DeclE v e) = do
        x   <- getEnv
        val <- evaluate e
        _ <- liftIO $ define (lexeme v) val x
        _ <- liftIO $ print $ "var "++ lexeme v ++ " = " ++ show val
        return val

    stmtEval :: Stmt -> Interpreter Value
    stmtEval (Print e) = do
        x <- evaluate e
        _ <- liftIO $ print x
        return Nil

    stmtEval (Expression e) = evaluate e

    stmtEval (Block ds)  = do
        previous <- getEnv
        new_env <- liftIO $ createChildEnv previous
        putEnv new_env
        catchError (do 
            v <- declEvaluator ds 
            putEnv previous
            return v) (\e -> do
                liftIO $ print e -- this would be stack trace
                putEnv previous
                throwError e)

    evaluate :: Expr -> Interpreter Value
    evaluate (Literal x) = return x
    evaluate (Log ex1 tok ex2) = do
        l <- evaluate ex1
        r <- evaluate ex2
        case tokenType tok of
            GREATER     -> matchOp (>) l r
            GREATER_EQUAL -> matchOp (>=) l r
            LESS        -> matchOp (<) l r
            LESS_EQUAL  -> matchOp (<=) l r
            EQUAL_EQUAL -> return $ Bool $ l==r
            BANG_EQUAL  -> return $ Bool $ l/=r
            _           -> raiseError Unexpected
        where
            matchOp f (Number x) (Number y) = return $ Bool (f x y)
            matchOp _ x y = raiseError $ RuntimeError $ "Illegal comparison op on: "++show x ++ " "++show y

    evaluate (Binary ex1 tok ex2) = do
        l <- evaluate ex1
        r <- evaluate ex2
        case tokenType tok of
            PLUS  -> plus l r ex1 ex2
            MINUS -> minus l r ex1 ex2
            SLASH -> divide l r ex1 ex2
            STAR  -> mult l r ex1 ex2
            _    -> raiseError Unexpected
        where
            plus (Number x) (Number y) _ _ = return $ Number (x+y)
            plus (String x) (String y) _ _ = return $ String (x++y)
            plus _ _ x y = raiseError $ RuntimeError $ "Undefined operation + on: "++show x ++ ", "++show y

            minus (Number x) (Number y) _ _= return $ Number (x-y)
            minus _ _ x y = raiseError $ RuntimeError $ "Undefined operation - on: "++show x ++ ", "++show y

            divide (Number x) (Number y) x1 y1
                | y /= 0    = return $ Number (x/y)
                | otherwise = raiseError $ RuntimeError $ "Division by 0: "++show x1 ++ ","++show y1
            divide _ _ x y  = raiseError $ RuntimeError $ "Undefined operation / on: "++show x ++ ", "++show y

            mult (Number x) (Number y) _ _ = return $ Number (x*y)
            mult _ _ x y = raiseError $ RuntimeError $ "Undefined operation * on: "++show x ++ ", "++show y
    evaluate (Group expr) = evaluate expr
    evaluate (Unary tok expr) = do
        case tokenType tok of
            MINUS -> do
                v <- evaluate expr
                case v of
                    Number x -> return $ Number (- x)
                    _ -> raiseError $ RuntimeError $ "Undefined operation - on: "++ show expr
            BANG -> do
                v <- evaluate expr
                case v of --ruby style for bang conversion
                    Bool b -> return $ Bool (not b)
                    Nil    -> return $ Bool False
                    _      -> return $ Bool True --raiseError $ RuntimeError $ "Undefined operation ! on: "++ show expr 
            _     -> raiseError $ RuntimeError $ "Undefined operation" ++ show (tokenType tok) ++" on: "++ show expr

    evaluate (Assign x e) = do
        en <- getEnv
        v  <- evaluate e
        b  <- liftIO $ assign (lexeme x) v en 
        case b of
            Nothing -> throwError $ RuntimeError $ "Undefined variable: "++ lexeme x ++ " in assign expression: "++ show (Assign x e)
            Just _  -> return v

    evaluate (Var x) = do
        en <- getEnv
        v  <- liftIO $ getVar (lexeme x) en
        case v of
            Just val -> return val
            Nothing  -> throwError $ RuntimeError $ "Undefined variable: "++ lexeme x ++ " in "++ show x

    evaluate _ = undefined

    raiseError :: InterpreterError -> Interpreter Value
    raiseError = ExceptT . return . Left

    runInterpreter :: [Decl] -> Env -> IO (Either InterpreterError Value)
    runInterpreter e ev = do
        nv <- newIORef ev
        let x = runStateT (runExceptT $ declEvaluator e) $ InterpreterState nv [] in
            do
                res <- x
                case res of
                    (Left err, _) -> return $ Left err
                    (Right  x, _) -> return $ Right x