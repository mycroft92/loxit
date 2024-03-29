{-# OPTIONS_GHC -Wno-unused-do-bind #-}
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
      MonadState(get,put),
      MonadTrans(lift))
    import Control.Monad.Except (ExceptT(..), runExceptT, throwError, catchError)
    import Environment (Env (..), define, getVar, getVarEnv, assign, createChildEnv, newEnv, readEnvAt, printEnv)
    import Data.IORef
    import Data.Foldable (foldrM)
    import Data.Time.Clock.POSIX (getPOSIXTime)
    import Data.Map.Strict as Map


    data InterpreterState = InterpreterState {
        env          :: IORef Env,
        globals      :: Env,   -- why is this problematic with IOREFs?
        functionEnvs :: Map.Map Value (Env, Decl),
        locals       :: Map.Map Expr Int
    }

    type Interpreter a = ExceptT InterpreterError (StateT InterpreterState IO) a


    declEvaluator :: [Decl] -> Interpreter Value
    declEvaluator []     = return Nil
    declEvaluator [x]    = declEval x
    declEvaluator (x:xs) = do
        _ <- declEval x
        declEvaluator xs

    declEval :: Decl -> Interpreter Value
    declEval (Statement s) = stmtEval s
    declEval (Decl v)      = varDeclEval v

    declEval f@(Fn name args _) = do
        st <- lift get
        env <- _getEnv
        let func = LoxFn (lexeme name) (length args) UserDef
        _  <- liftIO $ define (lexeme name) func env
        let fenv' =  Map.insert func (env, f) (functionEnvs st)
        _  <- lift $ put $ st {functionEnvs = fenv'}
        return func

    _getEnv :: Interpreter Env
    _getEnv = do
        en  <- lift get
        liftIO $ readIORef (env en)

    _getGlobal :: Interpreter Env
    _getGlobal = do
        en  <- lift get
        return (globals en) -- This actually works! You can hold a changing map and if the pointer updates it, this value changes as well
        -- liftIO $ readIORef (globals en)

    _getLocals :: Interpreter (Map.Map Expr Int)
    _getLocals = do
        st <- lift get
        return (locals st)

    _getFenv :: Interpreter (Map.Map Value (Env, Decl))
    _getFenv = do
        st <- lift get
        return (functionEnvs st)

    _putEnv :: Env -> Interpreter ()
    _putEnv ev = do
        x <- lift get
        liftIO $ writeIORef (env x) ev

    varDeclEval :: VarDecl -> Interpreter Value
    varDeclEval (OnlyDecl v) = do
        x <- _getEnv
        _ <- liftIO $ define (lexeme v) Nil x
        return Nil

    varDeclEval (DeclE v e) = do
        x   <- _getEnv
        val <- evaluate e
        _ <- liftIO $ define (lexeme v) val x
        return val

    stmtEval :: Stmt -> Interpreter Value
    stmtEval (Print e) = do
        x <- evaluate e
        _ <- liftIO $ print x
        return Nil

    stmtEval (Expression e) = evaluate e

    stmtEval (ITE bc t mf) =
        ifM  (isTruthy bc) (stmtEval t) (
            case mf of
                Nothing -> return Nil
                Just s  -> stmtEval s)
    -- this doesn't return last statement value for now
    stmtEval (While bc st) = ifM (isTruthy bc) (stmtEval st >> stmtEval (While bc st)) (return Nil)

    stmtEval (Return e) = do
        v <- evaluate e
        throwError $ ReturnException v
    
    stmtEval (Block ds)  = do
        previous <- _getEnv
        new_env <- liftIO $ createChildEnv previous
        executeBlock ds new_env
        -- _putEnv new_env
        -- catchError (do
        --     v <- declEvaluator ds
        --     _putEnv previous
        --     return v) (\e -> do
        --         -- liftIO $ print e -- this would be stack trace
        --         _putEnv previous
        --         throwError e)

    -- This is a helper function, called from both function calls and stmt eval
    executeBlock :: [Decl] -> Env -> Interpreter Value
    executeBlock ds env' = do
        prev <- _getEnv
        _putEnv env'
        catchError (do
            v <- declEvaluator ds
            _putEnv prev
            return v) (\e -> do
                -- liftIO $ print e -- this would be stack trace
                _putEnv prev
                throwError e)


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

    evaluate e'@(Assign x e) = do
        v    <- evaluate e
        dist <- lookupLocal e'
        en   <- _getEnv
        handle dist (lexeme x) v en
        where
            handle :: Maybe Int -> String -> Value -> Env -> Interpreter Value
            handle dist x v e = 
                case dist of
                    Nothing -> _getGlobal >>= \s -> liftIO $ assign x v s >> return v
                    Just d  -> do
                        maybenv' <- liftIO $ readEnvAt d e
                        case maybenv' of
                            Nothing -> throwError $ RuntimeError $ "No env at dist:" ++ show dist ++" while searching for var"++show x
                            Just e' -> liftIO $ assign x v e' >> return v 

    evaluate e'@(Var x) = do
        en   <- _getEnv
        g    <- _getGlobal
        dist <- lookupLocal e'
        case dist of
            Just d  -> findVarAt x d en
            Nothing -> findVar x g

    evaluate (Call cl args) = do
        fn   <- evaluate cl
        case fn of
            LoxFn _ arity FFI -> if length args == arity
                then do
                    argV <- evalArgs args
                    ffiCall fn argV
                else throwError $ RuntimeError $ show fn ++ " has arity different from num args supplied! \n"++ show (Call cl args)
            LoxFn _ arity UserDef -> if length args == arity
                then do
                    argV <- evalArgs args
                    -- liftIO $ print $ "Args to :"++show cl
                    -- liftIO $ mapM_ print argV
                    call fn argV
                else throwError $ RuntimeError $ show fn ++ " has arity different from num args supplied! \n"++ show (Call cl args)
            _ -> throwError $ RuntimeError $ show fn ++ " is not a callable!"

        -- return Nil
        where
            evalArgs = foldrM (\x acc -> do
                v <- evaluate x
                return (v:acc)) []

    evaluate _ = undefined
    
    findVar :: Token -> Env -> Interpreter Value
    findVar x env' = do
        v <- liftIO $ getVarEnv (lexeme x) env'
        case v of
            Just val -> return val
            Nothing  -> throwError $ RuntimeError $ "Undefined variable: "++ lexeme x ++ " in "++ show x
    findVarAt :: Token -> Int -> Env -> Interpreter Value
    findVarAt x dist env' = do
        env'' <- liftIO $ readEnvAt dist env'
        -- liftIO $ print $ "find var " ++ lexeme x ++ " in env:"
        -- liftIO $ printEnv env'
        case env'' of
            Nothing -> throwError $ RuntimeError $ "No env at dist:" ++ show dist ++" while searching for var"++show x
            Just env'' -> do
                findVar x env''
    
    lookupLocal :: Expr -> Interpreter (Maybe Int)
    lookupLocal e = Map.lookup e <$> _getLocals

    --this can be improved to be  a lookup in the env of FFIs but this works for now
    ffiCall :: Value -> [Value] -> Interpreter Value
    ffiCall f@(LoxFn n _ _) _ =
        case n of
            "clock" -> do
                mtime <- liftIO getPOSIXTime
                return $ Number $ fromIntegral $ (round . (*1000)) mtime
            _  -> throwError $ RuntimeError $ show f ++ " is not a defined ffi!"
    ffiCall f _ = throwError $ RuntimeError $ show f ++ " Invalid value for ffi!"

    call :: Value -> [Value] -> Interpreter Value
    call f@(LoxFn n a _) argV = do
        fenv <- _getFenv
        case Map.lookup f fenv of
            Nothing -> throwError $ RuntimeError $ "Undefined function: "++show f
            Just (e, Fn _ argT (Block ds)) -> do
                e' <- liftIO $ createChildEnv e -- create a child of defined env
                _  <- defineArgs argT argV e'
                --The call to executeBlock replaces the original env (en) properly
                catchError (executeBlock ds e') (\e ->
                    case e of 
                        ReturnException v -> return v
                        _ -> throwError e)
            Just (e, Fn _ argT stmt) -> do
                en <- _getEnv
                e' <- liftIO $ createChildEnv e -- we are creating 2 sets of environments (here and in block for every call, which is throwing off the resolver)
                _  <- _putEnv e'
                _  <- defineArgs argT argV e'
                catchError (do  --if it is a return we need to handle it here and restore the env
                    v <- stmtEval stmt 
                    _ <- _putEnv en
                    return v) (\e ->
                        case e of
                            ReturnException v -> do
                                _ <- _putEnv en
                                return v
                            _ -> do
                                _putEnv en
                                throwError e)
            Just (_, s) -> throwError $ RuntimeError $ "Illegal values in function env for: "++show f ++ " val: " ++ show s
            where
                defineArgs :: [Token] -> [Value] -> Env -> Interpreter ()
                defineArgs (t:ts) (v:vs) e = do
                    _ <- liftIO $ define (lexeme t) v e
                    defineArgs ts vs e
                defineArgs [] [] _ = return ()
                defineArgs _ _ _ = throwError $ RuntimeError "Impossible situation, number of args not same as values"

    call e _ = throwError $ RuntimeError $ "Invalid callable: "++show e++"!"


    raiseError :: InterpreterError -> Interpreter Value
    raiseError = ExceptT . return . Left

    mkGlobals :: IO Env
    mkGlobals = do
        ev <- newEnv
        _  <- define "clock" (LoxFn "clock" 0 FFI) ev --define clock as  a global
        return ev

    initState :: Map.Map Expr Int -> IO InterpreterState
    initState m = do
        glob <- mkGlobals
        globe <- newIORef glob
        return $ InterpreterState globe glob Map.empty m

    runInterpreter :: [Decl] -> InterpreterState -> IO (Either InterpreterError Value)
    runInterpreter e st = do
        let x = runStateT (runExceptT $ declEvaluator e) st in
            do
                res <- x
                case res of
                    (Left err, _) -> return $ Left err
                    (Right  x, _) -> return $ Right x

    interpret :: [Decl] -> InterpreterState -> IO InterpreterState
    interpret decl st = do
        let x = runStateT (runExceptT $ declEvaluator decl) st in
            do
                res <- x
                case res of
                    (Left err, st) -> do
                        print err
                        return st
                    (Right  x, st) -> do
                        print x
                        return st
