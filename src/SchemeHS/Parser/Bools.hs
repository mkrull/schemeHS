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

import           SchemeHS.Parser.Types
import           Text.ParserCombinators.Parsec

parseLispBool :: Parser LispVal
parseLispBool = do
    _ <- char '#'
    x <- oneOf "tf"
    return $ case x of
        't' -> LispBool True
        'f' -> LispBool False
