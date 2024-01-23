module Expr where
    import TokenTypes (Token)

    data Object = 
        Number Double
        | String String
        | Bool Bool
        deriving (Eq, Show)

    data Expr = 
        Binary Expr Token Expr
        | Unary Token Expr
        | Literal Object
        | Var Token        
        | Group Expr deriving (Eq, Show)
        