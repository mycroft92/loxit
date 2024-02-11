module Environment where
    import Error
    import Data.IORef ( IORef, modifyIORef', newIORef, readIORef )
    import Data.Map.Strict as Map ( Map, empty, lookup, insert, member)
    import Expr (Value (..))

    data Env = Env {
        e_values  :: IORef (Map.Map String Value),
        enclosing :: IORef (Maybe Env) -- stack of envs
    }




    printEnv :: Env -> IO String
    printEnv e = do
        ev <- readIORef (e_values e)
        print ev
        return $ show ev

    newEnv :: IO Env
    newEnv = do
        ev  <- newIORef Map.empty
        enc <- newIORef Nothing 
        return $ Env ev enc

    createChildEnv :: Env -> IO Env
    createChildEnv env = do
        ev  <- newIORef Map.empty
        enc <- newIORef $ Just env
        return $ Env ev enc

    define :: String -> Value -> Env -> IO ()
    define name val env = modifyIORef' (e_values env)  (Map.insert name val)

    assign :: String -> Value -> Env -> IO (Maybe ())
    assign name val env = ifM (isMember name env) (Just <$> define name val env) (do
        enc <- readIORef (enclosing env)
        case enc of
            Nothing -> return Nothing
            Just ev -> assign name val ev)

    -- updated to use nested environments
    getVar :: String -> Env -> IO (Maybe Value)
    getVar vnam env = do
        m   <- readIORef (e_values env)
        enc <- readIORef (enclosing env)
        case Map.lookup vnam m of
            Nothing -> case enc of
                            Nothing -> return Nothing
                            Just ev -> getVar vnam ev
            Just x  -> return $ Just x 

    isMember :: String -> Env -> IO Bool
    isMember x e = do
        m <- readIORef (e_values e)
        return (Map.member x m)