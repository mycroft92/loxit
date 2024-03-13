module TokenTypes where

    data TokenType
        = LEFT_PAREN
        | RIGHT_PAREN
        | LEFT_BRACE
        | RIGHT_BRACE
        | COMMA
        | DOT
        | MINUS
        | PLUS
        | SEMICOLON
        | SLASH
        | STAR
        -- one or two character toks
        | BANG
        | BANG_EQUAL
        | EQUAL
        | EQUAL_EQUAL
        | GREATER
        | GREATER_EQUAL
        | LESS
        | LESS_EQUAL
        -- idents
        | IDENT
        | STRING
        | NUMBER
        -- keywords
        | AND
        | CLASS
        | ELSE
        | FALSE
        | FUN
        | FOR
        | IF
        | NIL
        | OR
        | PRINT
        | RETURN
        | SUPER
        | THIS
        | TRUE
        | VAR
        | WHILE
        | EOF 
        -- -- Throwaway token for comment/whitespace
        | WS deriving (Show, Eq, Ord)

    data Token = Token {
        tokenType :: TokenType,
        lexeme    :: String,
        st        :: Int,
        len       :: Int
    } deriving (Show, Eq, Ord)
    
    makeEOF :: Token
    makeEOF = Token { tokenType = EOF, lexeme ="\0", st = 0, len=0}