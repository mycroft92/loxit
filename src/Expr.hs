module Expr where
    import TokenTypes (Token)

    data Value = 
        Number Double
        | String String
        | Bool Bool
        | Nil
        deriving (Eq, Show)

    data Expr = 
        Binary Expr Token Expr
        | Log  Expr Token Expr
        | Unary Token Expr
        | Literal Value
        | Var Token
        | This
        | Super     
        | Group Expr deriving (Eq, Show)
        

    