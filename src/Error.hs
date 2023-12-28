module Error where

    data LineInfo = LineCol Int Int String | Line Int String deriving (Show, Eq)
    data ErrInfo  = ErrInfo LineInfo String deriving (Show, Eq)