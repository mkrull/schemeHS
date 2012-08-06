-----------------------------------------------------------------------------
--
-- Module      :  SchemeHS.Parser.Types
-- Copyright   :
-- License     :  AllRightsReserved
--
-- Maintainer  :
-- Stability   :
-- Portability :
--
-- |
--
-----------------------------------------------------------------------------

module SchemeHS.Parser.Types where

import Text.Show

data LispVal = LispAtom String
             | LispList [LispVal]
             | LispDottedList [LispVal] LispVal
             | LispNumber Integer
             | LispString String
             | LispBool Bool
             | LispChar Char
             | LispFloat Double
             deriving (Show)
