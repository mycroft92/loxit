module Evaluator where
    import Error
    import Expr
    import TokenTypes
    import Control.Monad.State
    import Control.Monad.Except

    
    data InterpreterState = InterpreterState {

    }

    type Interpreter a = ExceptT InterpreterError (StateT InterpreterState IO) a


    declEvaluator:: [Decl] -> Interpreter Value
    declEvaluator []  = return Nil
    declEvaluator [x] = declEval x
    declEvaluator (x:xs) = do
        _ <- declEval x
        declEvaluator xs

    declEval :: Decl -> Interpreter Value
    declEval (Statement s) = stmtEval s
    declEval (Decl v)   = undefined

    stmtEval :: Stmt -> Interpreter Value
    stmtEval (Print e) = do
        x <- evaluate e
        _ <- liftIO $ print x
        return Nil
    
    stmtEval (Expression e) = evaluate e

    evaluate :: Expr -> Interpreter Value
    evaluate (Literal x) = return x
    evaluate (Log ex1 tok ex2) = do
        l <- evaluate ex1
        r <- evaluate ex2
        case tokenType tok of
            GREATER    -> matchOp (>) l r
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
            divide _ _ x y = raiseError $ RuntimeError $ "Undefined operation / on: "++show x ++ ", "++show y

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
    evaluate _ = undefined

    raiseError :: InterpreterError -> Interpreter Value
    raiseError = ExceptT . return . Left 

    -- runInterpreter :: Expr -> IO (Either InterpreterError Value)
    -- runInterpreter e = 
    --     let x = runStateT (runExceptT $ evaluate e) InterpreterState in
    --         do
    --             res <- x
    --             case res of
    --                 (Left err,_) -> return $ Left err
    --                 (Right x,_)  -> return $ Right x
    runInterpreter :: [Decl] -> IO (Either InterpreterError Value)
    runInterpreter e = 
        let x = runStateT (runExceptT $ declEvaluator e) InterpreterState in
            do
                res <- x
                case res of
                    (Left err,_) -> return $ Left err
                    (Right x,_)  -> return $ Right x