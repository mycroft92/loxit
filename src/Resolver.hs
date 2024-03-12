module Resolver where
    import TokenTypes
    import Expr
    import Error
    import Control.Monad.Except (ExceptT(..), runExceptT, throwError, catchError)
    import Control.Monad.State (StateT(runStateT),
        MonadIO(liftIO),
        MonadState(get,put),
        MonadTrans(lift))
    import Data.Map.Strict as Map
    import Data.Foldable (forM_)
    import Stack
    import Data.Maybe (fromJust)
    
    type Locals =  Stack (Map.Map String Bool)

    data ResolverState = ResolverState {
        locals :: Locals
    }

    -- running runExceptT on this gives StateT ResolverState IO (Either InterpreterError a)  
    type Resolver a = ExceptT InterpreterError (StateT ResolverState IO) a

    _getLocals :: Resolver Locals
    _getLocals = do
        st <- lift get
        return (locals st)
    
    _isEmpty   :: Resolver Bool
    _isEmpty = isEmpty <$> _getLocals

    _putLocals :: Locals -> Resolver ()
    _putLocals l = do
        st <- lift get
        lift $ put (st {locals = l})

    _beginScope :: Resolver ()
    _beginScope = do
        lc <- _getLocals
        _putLocals (push Map.empty lc)

    _endScope :: Resolver ()
    _endScope = do
        lc <- _getLocals
        (_,lc') <- handle (pop lc)
        _putLocals lc'
        where
            handle :: Maybe a -> Resolver a
            handle (Just x) = return x
            handle Nothing  = throwError $ ResolverError "Empty stack pop called"


    resolveDecls :: [Decl] -> Resolver ()
    resolveDecls = mapM_ resolveDecl

    resolveDecl :: Decl -> Resolver ()
    resolveDecl (Statement s)   = resolveStmt s
    resolveDecl (Decl d)        = resolveVarDecl d
    resolveDecl f@(Fn name args stmts)
                                = undefined

    resolveVarDecl :: VarDecl -> Resolver ()
    resolveVarDecl (DeclE x e) = do
        declare x
        resolveExpr e
        define x
    resolveVarDecl (OnlyDecl x) = declare x >> define x


    currentScope :: Resolver (Maybe (Map.Map String Bool))
    currentScope = do
        lc <- _getLocals
        case pop lc of
            Just (sc, st) -> return $ Just sc
            Nothing   -> return Nothing


    -- Not empty safe
    putInScope :: String -> Bool -> Resolver ()
    putInScope x p = do
        lc <- _getLocals
        let (scope, stack) = (fromJust . pop) lc in
            let scope' = Map.insert x p scope in
                _putLocals (push scope' stack)
    

    peekInScope :: String -> Resolver (Maybe Bool)
    peekInScope x = do
        current <- currentScope
        case current of
            Just c  -> return (Map.lookup x c)
            Nothing -> return Nothing

    declare :: Token -> Resolver ()
    declare tk = ifM _isEmpty (return ()) (putInScope (lexeme tk) False)

    define :: Token -> Resolver ()
    define tk = ifM _isEmpty (return ()) (putInScope (lexeme tk) True)


    resolveStmt :: Stmt -> Resolver ()
    resolveStmt (Block d) = do
        _beginScope
        resolveDecls d
        _endScope
    resolveStmt (While e s)   = resolveExpr e >> resolveStmt s
    resolveStmt (ITE e s1 s2) = resolveExpr e >> (resolveStmt s1 >> forM_ s2 resolveStmt)
    resolveStmt _ = undefined


    resolveExpr :: Expr -> Resolver ()
    resolveExpr (Binary e1 _ e2) = resolveExpr e1 >> resolveExpr e2
    resolveExpr (Log e1 _ e2)    = resolveExpr e1 >> resolveExpr e2
    resolveExpr (Unary _ e)      = resolveExpr e
    resolveExpr (Var tk)         =
        do
            s <- peekInScope (lexeme tk)
            case s of
                Just False -> throwError $ ResolverError $ "Can't read local variable in its own initializer: "++ show tk
                _          -> resolveLocal tk

    resolveLocal :: Token -> Resolver ()
    resolveLocal tk = undefined