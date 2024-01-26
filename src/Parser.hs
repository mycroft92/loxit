module Parser where
    
    import TokenTypes

    newtype ParserState = ParserState {
        tokens:: [Token]
    }