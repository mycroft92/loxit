module Error where
    import TokenTypes (Token)
    data LineInfo = LineCol Int Int String | Line Int String deriving (Eq)
    data ErrInfo  = ErrInfo LineInfo String deriving (Show, Eq)

    instance Show LineInfo where
        show (LineCol l c s) = "line: " ++ show l ++ " col: "  ++ show c ++ " file: "++ s
        show (Line l s)      = "line: " ++ show l ++ " file: " ++ s

    makeErr :: String -> Int -> String -> ErrInfo
    makeErr fn l = ErrInfo (Line l fn)

    makeErrCol :: String -> Int -> Int -> String -> ErrInfo
    makeErrCol fn l c = ErrInfo (LineCol l c fn)

    data InterpreterError = 
        Unexpected 
        | RuntimeError String 
        | InterpreterError Token String deriving (Show, Eq)