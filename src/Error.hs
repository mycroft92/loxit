module Error where

    data LineInfo = LineCol Int Int String | Line Int String deriving (Eq)
    data ErrInfo  = ErrInfo LineInfo String deriving (Show, Eq)

    instance Show LineInfo where
        show (LineCol l c s) = "line: " ++ show l ++ " col: "++ show c ++ " "++ s
        show (Line l s)      = "line: " ++ show l ++ s

    makeErr :: String -> Int -> String -> ErrInfo
    makeErr fn l = ErrInfo (Line l fn)

    makeErrCol :: String -> Int -> Int -> String -> ErrInfo
    makeErrCol fn l c = ErrInfo (LineCol l c fn)