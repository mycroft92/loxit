module Parser where
    
    import TokenTypes ( Token )
    import Error (ErrInfo)
    import Expr

    data ParserState = ParserState {
        tokens:: [Token],
        index :: Int
    } deriving (Show, Eq)

    newtype Parser a = Parser {runParse :: ParserState -> Either ErrInfo (a, ParserState)}

    