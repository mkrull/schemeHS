-----------------------------------------------------------------------------
--
-- Module      :  SchemeHS.Parser
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

module SchemeHS.Parser (
    readExpr
) where

import Text.ParserCombinators.Parsec hiding (spaces)
import SchemeHS.Parser.Types
import SchemeHS.Parser.Numbers
import SchemeHS.Parser.Bools
import SchemeHS.Parser.Strings

symbol :: Parser Char
symbol = oneOf "!$%&|*+-/:<=?>@^_~"

spaces :: Parser ()
spaces = skipMany1 space

parseLispAtom :: Parser LispVal
parseLispAtom = do first <- letter <|> symbol
                   rest <- many (letter <|> digit <|> symbol)
                   let atom = [first] ++ rest
                   return $ LispAtom atom

parseExpr ::  Parser LispVal
parseExpr =  parseLispAtom
         <|> parseLispString
         <|> parseLispChar
         <|> parseLispNumber
         <|> parseLispBool

readExpr :: String -> String
readExpr input = case parse parseExpr "lisp" input of
    Left err -> "No match: " ++ show err
    Right val -> "Found value: " ++ show val
