module SchemeHS.Types where

data LispVal = LispAtom String
             | LispList [LispVal]
             | LispDottedList [LispVal] LispVal
             | LispNumber Integer
             | LispString String
             | LispBool Bool
             | LispChar Char
             | LispFloat Double
             deriving (Show)
