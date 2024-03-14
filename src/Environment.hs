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
        enc <- readIORef (enclosing e)
        case enc of
            Nothing -> return $ show ev
            Just e' -> do
                p <- printEnv e'
                return $ p ++ "\n\t"++show ev

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
    
    nestEnvUnder :: Env -> Env -> IO Env
    nestEnvUnder s p = do
        ev <- readIORef (enclosing s)
        case ev of 
            Just s' -> nestEnvUnder s' p
            Nothing -> do
                enc <- newIORef (Just p)
                return $ Env (e_values s) enc

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
        -- print "Searching current"
        -- s <- printEnv env
        -- print s
        case Map.lookup vnam m of
            Nothing -> case enc of
                            Nothing -> return Nothing
                            Just ev -> getVar vnam ev
            Just x  -> return $ Just x 

    readEnvAt :: Int -> Env -> IO (Maybe Env)
    readEnvAt 0 e = return . Just $ e
    readEnvAt d e = do
        enc <- readIORef (enclosing e)
        case enc of
            Nothing   -> return Nothing
            Just enc' -> readEnvAt (d-1) enc'


    isMember :: String -> Env -> IO Bool
    isMember x e = do
        m <- readIORef (e_values e)
        return (Map.member x m)
