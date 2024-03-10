module Resolver where
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

_putLocals :: Locals -> Resolver ()
_putLocals l = do
    st <- lift get
    lift $ put (st {locals = l})

_beginScope :: Resolver ()
_beginScope = do
    lc <- _getLocals
    _putLocals (Push Map.empty lc)

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
resolveVarDecl v = undefined

resolveStmt :: Stmt -> Resolver ()
resolveStmt (Block d) = do
    _beginScope
    resolveDecls d
    _endScope
resolveStmt (While _ s)   = resolveStmt s
resolveStmt (ITE _ s1 s2) = resolveStmt s1 >> forM_ s2 resolveStmt
resolveStmt _ = undefined


resolveExpr :: Expr -> Resolver ()
resolveExpr _ = return ()