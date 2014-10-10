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

import           Control.Monad
import           SchemeHS.Parser.Bools
import           SchemeHS.Parser.Numbers
import           SchemeHS.Parser.Strings
import           SchemeHS.Parser.Types
import           Text.ParserCombinators.Parsec hiding (spaces)

symbol :: Parser Char
symbol = oneOf "!$%&|*+-/:<=?>@^_~"

spaces :: Parser ()
spaces = skipMany1 space

parseLispAtom :: Parser LispVal
parseLispAtom = do first <- letter <|> symbol
                   rest <- many (letter <|> digit <|> symbol)
                   let atom = first:rest
                   return $ LispAtom atom

parseExpr ::  Parser LispVal
parseExpr =  parseLispAtom
         <|> parseLispString
         <|> parseLispChar
         <|> parseLispNumber
         <|> parseLispBool
         <|> parseQuoted
         <|> do
             char '('
             x <- (try parseLispList) <|> parseLispDottedList
             char ')'
             return x

parseLispList :: Parser LispVal
parseLispList = liftM LispList $ sepBy parseExpr spaces

parseQuoted :: Parser LispVal
parseQuoted = do
    char '\''
    x <- parseExpr
    return $ LispList [LispAtom "quote", x]

parseLispDottedList :: Parser LispVal
parseLispDottedList = do
    head <- endBy parseExpr spaces
    tail <- char '.' >> spaces >> parseExpr
    return $ LispDottedList head tail

readExpr :: String -> String
readExpr input = case parse parseExpr "lisp" input of
    Left err -> "No match: " ++ show err
    Right val -> "Found value: " ++ show val
