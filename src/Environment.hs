module Environment where
    import Error
    import Data.IORef ( IORef, modifyIORef', newIORef, readIORef )
    import Data.Map.Strict as Map ( Map, empty, lookup, insert, member)
    import Expr (Value)

    data Env = Env {
        e_values :: IORef (Map.Map String Value)
    }

    printEnv :: Env -> IO String
    printEnv e = do
        ev <- readIORef (e_values e)
        print ev
        return $ show ev

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

    isMember :: String -> Env -> IO Bool
    isMember x e = do
        m <- readIORef (e_values e)
        return (Map.member x m)