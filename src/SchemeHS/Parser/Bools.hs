-----------------------------------------------------------------------------
--
-- Module      :  SchemeHS.Parser.Bools
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

module SchemeHS.Parser.Bools (
    parseLispBool
) where

import Text.ParserCombinators.Parsec
import SchemeHS.Parser.Types

parseLispBool :: Parser LispVal
parseLispBool = do
    _ <- char '#'
    x <- oneOf "tf"
    return $ case x of
        't' -> LispBool True
        'f' -> LispBool False
