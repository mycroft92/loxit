module Evaluator where
    import Error
    import Expr
    import TokenTypes
    import Control.Monad.State
    import Control.Monad.Except

    
    data InterpreterState = InterpreterState {

    }

    type Interpreter a = ExceptT InterpreterError (StateT InterpreterState IO) a

    evaluate :: Expr -> Interpreter Value
    evaluate (Literal x) = return x
    evaluate (Binary ex1 tok ex2) = do
        l <- evaluate ex1
        r <- evaluate ex2
        case tokenType tok of
            PLUS  -> plus l r
            MINUS -> minus l r
            SLASH -> divide l r
            STAR  -> mult l r
            _    -> raiseError Unexpected
        where
            plus (Number x) (Number y) = return $ Number (x+y)
            plus (String x) (String y) = return $ String (x++y)
            plus x y = raiseError $ RuntimeError $ "Undefined operation + on: "++show x ++ ","++show y

            minus (Number x) (Number y) = return $ Number (x-y)
            minus x y = raiseError $ RuntimeError $ "Undefined operation - on: "++show x ++ ","++show y

            divide (Number x) (Number y) 
                | y /= 0    = return $ Number (x/y)
                | otherwise = raiseError $ RuntimeError $ "Division by 0: "++show x ++ ","++show y
            divide x y = raiseError $ RuntimeError $ "Undefined operation / on: "++show x ++ ","++show y

            mult (Number x) (Number y) = return $ Number (x*y)
            mult x y = raiseError $ RuntimeError $ "Undefined operation * on: "++show x ++ ","++show y
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
                case v of
                    Bool b -> return $ Bool (not b)
                    _      -> raiseError $ RuntimeError $ "Undefined operation ! on: "++ show expr 
            _     -> raiseError $ RuntimeError $ "Undefined operation" ++ show (tokenType tok) ++" on: "++ show expr 
    evaluate _ = undefined

    raiseError :: InterpreterError -> Interpreter Value
    raiseError = ExceptT . return . Left 

    runInterpreter :: Expr -> IO (Either InterpreterError Value)
    runInterpreter e = 
        let x = runStateT (runExceptT $ evaluate e) InterpreterState in
            do
                res <- x
                case res of
                    (Left err,_) -> return $ Left err
                    (Right x,_)  -> return $ Right x