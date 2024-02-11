module Evaluator where
    import Error (InterpreterError(..), ifM)
    import Expr (Decl(..),
      Expr(..),
      Stmt(..),
      Value(..),
      VarDecl(..), FType (..) )
    import TokenTypes (Token(tokenType, lexeme),
      TokenType(..))
    import Control.Monad.State (StateT(runStateT),
      MonadIO(liftIO),
      MonadState(get, put),
      MonadTrans(lift))
    import Control.Monad.Except (ExceptT(..), runExceptT, throwError, catchError)
    import Environment (Env (..), define, getVar, assign, createChildEnv, newEnv)
    import Data.IORef
    import Data.Foldable (foldrM)
    import Data.Time.Clock.POSIX (getPOSIXTime)

    
    data InterpreterState = InterpreterState {
        env :: IORef Env,
        globals :: IORef Env
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

    getGlobal :: Interpreter Env
    getGlobal = do
        en  <- lift get
        liftIO $ readIORef (globals en)

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

    stmtEval (ITE bc t mf) =
        ifM (isTruthy bc) (stmtEval t) (
            case mf of
                Nothing -> return Nil
                Just s  -> stmtEval s)
    -- this doesn't return last statement value for now
    stmtEval (While bc st) = ifM (isTruthy bc) (stmtEval st >> stmtEval (While bc st)) (return Nil)


    isTruthy :: Expr -> Interpreter Bool
    isTruthy e = do
        x <- evaluate e
        case x of
            Nil -> return False
            Bool b -> return b
            _   -> return True

    evaluate :: Expr -> Interpreter Value
    evaluate (Literal x) = return x
    evaluate (Log ex1 tok ex2) =
        case tokenType tok of
            GREATER     -> readExps ex1 ex2 matchOp (>)
            GREATER_EQUAL -> readExps ex1 ex2 matchOp (>=)
            LESS        -> readExps ex1 ex2 matchOp (<)
            LESS_EQUAL  -> readExps ex1 ex2 matchOp (<=)
            EQUAL_EQUAL -> readExps ex1 ex2 runComp (==)
            BANG_EQUAL  -> readExps ex1 ex2 runComp (/=)
            AND         -> andTruth ex1 ex2
            OR          -> orTruth  ex1 ex2
            _           -> raiseError Unexpected
        where
            readExps ex1 ex2 f op = do
                l <- evaluate ex1
                r <- evaluate ex2
                f op l r
            runComp op l r = return $ Bool $ l `op` r
            -- I don't like this evaluation but it is what it is
            andTruth ex1 ex2 = ifM (isTruthy ex1) (ifM (isTruthy ex2) (evaluate ex2) (return Nil)) (return Nil)
            orTruth  ex1 ex2 = ifM (isTruthy ex1) (evaluate ex1) (evaluate ex2)

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
        en   <- getEnv
        glob <- getGlobal
        catchError (findVar x en) (\e -> findVar x glob)

        where
            findVar :: Token -> Env -> Interpreter Value
            findVar x e = do
                v <- liftIO $ getVar (lexeme x) e
                case v of
                    Just val -> return val
                    Nothing  -> throwError $ RuntimeError $ "Undefined variable: "++ lexeme x ++ " in "++ show x

    evaluate (Call cl args) = do
        fn   <- evaluate cl
        case fn of
            LoxFn n arity FFI -> if length args == arity 
                then do
                    argV <- evalArgs args
                    ffiCall fn argV
                else throwError $ RuntimeError $ show fn ++ " has arity different from num args supplied! \n"++ show (Call cl args)
            LoxFn n arity UserDef -> undefined
            _ -> throwError $ RuntimeError $ show fn ++ " is not a callable!"

        -- return Nil
        where
            evalArgs = foldrM (\x acc -> do
                v <- evaluate x
                return (v:acc)) []
            -- evalArgs [] xs = return $ reverse xs
            -- evalArgs (arg:args) xs = do
            --     x <- evaluate arg
            --     evalArgs args (x:xs)
    evaluate _ = undefined

    --this can be improved to be  a lookup in the env of FFIs but this works for now
    ffiCall :: Value -> [Value] -> Interpreter Value
    ffiCall f@(LoxFn n a _) _ = 
        case n of
            "clock" -> do
                mtime <- liftIO getPOSIXTime
                return $ Number $ fromIntegral $ (round . (*1000)) mtime
            _  -> throwError $ RuntimeError $ show f ++ " is not a defined ffi!"
    ffiCall f _ = throwError $ RuntimeError $ show f ++ " Invalid value for ffi!"

    call :: Value -> [Value] -> Interpreter Value
    call = undefined

    raiseError :: InterpreterError -> Interpreter Value
    raiseError = ExceptT . return . Left

    mkGlobals :: IO Env
    mkGlobals = do
        ev <- newEnv
        _ <- define "clock" (LoxFn "clock" 0 FFI) ev --define clock as  a global
        return ev

    initState :: IO InterpreterState
    initState = do
        nv   <- newEnv
        nve <- newIORef nv
        glob <- mkGlobals
        globe <- newIORef glob
        return $ InterpreterState nve globe

    runInterpreter :: [Decl] -> InterpreterState -> IO (Either InterpreterError Value)
    runInterpreter e st = do
        let x = runStateT (runExceptT $ declEvaluator e) st in
            do
                res <- x
                case res of
                    (Left err, _) -> return $ Left err
                    (Right  x, _) -> return $ Right x