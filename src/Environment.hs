module Environment where
    import Error
    import Data.IORef
    import Data.Map.Strict as Map
    import Expr (Value)

    data Env = Env {
        e_values :: IORef (Map.Map String Value)
    }

    newEnv :: IO Env
    newEnv = do
        ev <- newIORef Map.empty
        return $ Env ev

    define :: String -> Value -> Env -> IO ()
    define name val env = modifyIORef' (e_values env)  (Map.insert name val)

    getVar :: String -> Env -> IO (Maybe Value)
    getVar vnam env = do
        m <- readIORef (e_values env)
        return (Map.lookup vnam m)
