module Expr where
    import TokenTypes (Token)
    data Expr = 
        Binary Expr Token Expr
        | Unary Token Expr
