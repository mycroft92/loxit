module Expr where
    import TokenTypes (Token)

    data Lit = 
        Number Double
        | String String
        | Bool Bool
        | Nil
        deriving (Eq, Show)

    data Expr = 
        Binary Expr Token Expr
        | Log  Expr Token Expr
        | Unary Token Expr
        | Literal Lit
        | Var Token
        | This
        | Super     
        | Group Expr deriving (Eq, Show)
        