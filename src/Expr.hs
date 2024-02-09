module Expr where
    import TokenTypes (Token (tokenType, lexeme))

    data Value = 
        Number Double
        | String String
        | Bool Bool
        | Nil
        deriving (Eq)

    data Expr = 
        Binary Expr Token Expr
        | Log  Expr Token Expr
        | Unary Token Expr
        | Literal Value
        | Var Token
        | Assign Token Expr
        | This
        | Super     
        | Group Expr deriving (Eq)

    data Stmt = 
        Expression Expr
        | Print Expr 
        | Block [Decl] 
        | ITE Expr Stmt (Maybe Stmt) deriving (Eq, Show)

    data Decl =
        Statement Stmt
        | Decl VarDecl deriving (Eq, Show)

    data VarDecl = OnlyDecl Token | DeclE Token Expr deriving (Show, Eq) 
    
    instance Show Value where
        show (Number x) = show x
        show (String s) = "\"" ++ s ++ "\""
        show (Bool b)   = show b
        show Nil        = "nil"

    instance Show Expr where
        show (Binary l t r) = show l ++" " ++show (tokenType t)++ " " ++ show r
        show (Log l t r)    = show l++" " ++ show (tokenType t)++" " ++ show r
        show (Unary t e)    = show (tokenType t) ++" "++ show e
        show (Literal v)    = show v
        show (Var t)        = show (lexeme t)
        show (Assign v e)   = lexeme v ++ " = "++ show e
        show  This          = "this"
        show  Super         = "super"
        show (Group e)      = "("++show e ++")"
        

    