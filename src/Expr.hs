module Expr where

    data Expr = 
        Binary Expr Token Expr
        | Unary Token Expr
