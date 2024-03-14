module Resolver (runResolver) where
    import TokenTypes
    import Expr
    import Error
    import Control.Monad.Except (ExceptT(..), runExceptT, throwError, catchError)
    import Control.Monad.State  (State,
      runState,
      MonadState(get, put),
      MonadTrans(lift) )
    import Data.Map.Strict as Map
    import Data.Foldable (forM_, foldr)
    import Stack
    import Data.Maybe (fromJust)

    type Scopes =  Stack (Map.Map String Bool)
    type Locals = Map.Map Expr Int

    data ResolverState = ResolverState {
        scopes :: Scopes,
        locals :: Locals
    }

    -- running runExceptT on this gives StateT ResolverState IO (Either InterpreterError a)  
    type Resolver a = ExceptT InterpreterError (State ResolverState) a

    _getScopes :: Resolver Scopes
    _getScopes = do
        st <- lift get
        return (scopes st)

    _getLocals :: Resolver Locals
    _getLocals = do
        st <- lift get
        return (locals st)

    _putLocals :: Locals -> Resolver ()
    _putLocals lc = do
        st <- lift get
        lift $ put (st {locals = lc})

    _isEmpty   :: Resolver Bool
    _isEmpty = isEmpty <$> _getScopes

    _putScopes :: Scopes -> Resolver ()
    _putScopes l = do
        st <- lift get
        lift $ put (st {scopes = l})

    _beginScope :: Resolver ()
    _beginScope = do
        lc <- _getScopes
        _putScopes (push Map.empty lc)

    _endScope :: Resolver ()
    _endScope = do
        lc <- _getScopes
        (_,lc') <- handle (pop lc)
        _putScopes lc'
        where
            handle :: Maybe a -> Resolver a
            handle (Just x) = return x
            handle Nothing  = throwError $ ResolverError "Empty stack pop called"

    _addExpr :: Expr -> Int -> Resolver ()
    _addExpr e v = do
        lc <- _getLocals
        let lc' = Map.insert e v lc in
            _putLocals lc'
    -- A single map works because the tokens in Expr have unique start and length tags.

    resolveDecls :: [Decl] -> Resolver ()
    resolveDecls = mapM_ resolveDecl

    resolveDecl :: Decl -> Resolver ()
    resolveDecl (Statement s)   = resolveStmt s
    resolveDecl (Decl d)        = resolveVarDecl d
    resolveDecl f@(Fn name args stmts)
                                = do
        declare name
        define name
        _beginScope
        mapM_ declare args
        mapM_ define args
        resolveStmt stmts
        _endScope

    resolveVarDecl :: VarDecl -> Resolver ()
    resolveVarDecl (DeclE x e) = do
        declare x
        resolveExpr e
        define x
    resolveVarDecl (OnlyDecl x) = declare x >> define x


    currentScope :: Resolver (Maybe (Map.Map String Bool))
    currentScope = do
        lc <- _getScopes
        case pop lc of
            Just (sc, st) -> return $ Just sc
            Nothing   -> return Nothing


    -- Not empty safe
    putInScope :: String -> Bool -> Resolver ()
    putInScope x p = do
        lc <- _getScopes
        let (scope, stack) = (fromJust . pop) lc in
            let scope' = Map.insert x p scope in
                _putScopes (push scope' stack)


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

    checkKey :: String -> Resolver (Maybe Int)
    checkKey x =  snd . Data.Foldable.foldr handle (0,Nothing) <$> _getScopes

        where
            handle :: Map.Map String Bool -> (Int, Maybe Int) -> (Int, Maybe Int)
            handle scope acc@(n,acc') =
                case acc' of
                    Nothing -> case Map.lookup x scope of
                        Just _  -> (n, Just n)
                        Nothing -> (n+1, Nothing)
                    _ -> acc

    resolveStmt :: Stmt -> Resolver ()
    resolveStmt (Block d) = do
        _beginScope
        resolveDecls d
        _endScope
    resolveStmt (While e s)    = resolveExpr e >> resolveStmt s
    resolveStmt (ITE e s1 s2)  = resolveExpr e >> (resolveStmt s1 >> forM_ s2 resolveStmt)
    resolveStmt (Expression e) = resolveExpr e
    resolveStmt (Print e)      = resolveExpr e
    resolveStmt (Return e)     = resolveExpr e


    resolveExpr :: Expr -> Resolver ()
    resolveExpr (Binary e1 _ e2) = resolveExpr e1 >> resolveExpr e2
    resolveExpr (Log e1 _ e2)    = resolveExpr e1 >> resolveExpr e2
    resolveExpr (Unary _ e)      = resolveExpr e
    resolveExpr (Literal _)      = return ()
    resolveExpr (Call e1 es)     = do
        resolveExpr e1
        mapM_ resolveExpr es
    resolveExpr (Group e)        = resolveExpr e
    resolveExpr e@(Var tk)       =
        do
            s <- peekInScope (lexeme tk)
            case s of
                Just False -> throwError $ ResolverError $ "Can't read local variable in its own initializer: "++ show tk
                _          -> resolveLocal e tk
    resolveExpr a@(Assign tk ex) = do
        resolveExpr ex
        resolveLocal a tk
    resolveExpr _ = undefined -- undefined for This and Super

    resolveLocal :: Expr -> Token -> Resolver ()
    resolveLocal expr tk = do
        val <- checkKey (lexeme tk)
        forM_ val (_addExpr expr)
    

    runResolver :: [Decl] -> Either InterpreterError (Map.Map Expr Int)
    runResolver decls = 
        case  runState (runExceptT $ resolveDecls decls) (ResolverState [] Map.empty) of
            (Left err, _) -> Left err
            (Right _, st) -> Right (locals st)
    -- lookupVariable :: Token -> Expr -> 