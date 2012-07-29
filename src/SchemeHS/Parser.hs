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

symbol :: Parser Char
symbol = oneOf "!$%&|*+-/:<=?>@^_~"

spaces :: Parser ()
spaces = skipMany1 space

parseLispString :: Parser LispVal
parseLispString = do char '"'
                     val <- many $ many1 (noneOf "\\\"") <|> escapedChars
                     char '"'
                     (return . LispString . concat) val

parseLispAtom :: Parser LispVal
parseLispAtom = do first <- letter <|> symbol
                   rest <- many (letter <|> digit <|> symbol)
                   let atom = [first] ++ rest
                   return $ LispAtom atom



escapedChars :: Parser String
escapedChars = do char '\\'
                  char <- oneOf "\\\"ntr"
                  case char of
                     '\\' -> do return [char]
                     '"' -> do return [char]
                     't' -> do return "\t"
                     'n' -> do return "\n"
                     'r' -> do return "\r"

parseExpr ::  Parser LispVal
parseExpr =  parseLispAtom
         <|> parseLispString
         <|> parseLispNumber
         <|> parseLispBool

readExpr :: String -> String
readExpr input = case parse parseExpr "lisp" input of
    Left err -> "No match: " ++ show err
    Right val -> "Found value: " ++ show val

