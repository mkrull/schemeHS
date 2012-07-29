-----------------------------------------------------------------------------
--
-- Module      :  SchemeHS.Parser.Numbers
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

module SchemeHS.Parser.Numbers (
    parseLispNumber
) where

import Text.ParserCombinators.Parsec
import SchemeHS.Parser.Types
import Numeric (readHex, readOct)

parseLispNumber :: Parser LispVal
parseLispNumber = do
    num <- parseNumber
           <|> parseDigital2
           <|> parseHex
           <|> parseOct
           <|> parseBin
    return $ num

parseNumber :: Parser LispVal
parseNumber = many1 digit >>= (return . LispNumber . read)

parseDigital2 :: Parser LispVal
parseDigital2 = do
    _ <- try $ string "#d"
    many1 digit >>= (return . LispNumber . read)

parseHex :: Parser LispVal
parseHex = do _ <- try $ string "#x"
              x <- many1 hexDigit
              return $ LispNumber $ hex2dig x

hex2dig :: (Eq a, Num a) => String -> a
hex2dig x = fst $ readHex x !! 0

parseOct :: Parser LispVal
parseOct = do _ <- try $ string "#o"
              o <- many1 octDigit
              return $ LispNumber $ oct2dig o

oct2dig :: (Eq a, Num a) => String -> a
oct2dig o = fst $ readOct o !! 0


parseBin :: Parser LispVal
parseBin = do _ <- try $ string "#b"
              b <- many1 $ oneOf "10"
              return $ LispNumber $ bin2dig b

bin2dig :: [Char] -> Integer
bin2dig = bin2dig' 0

bin2dig' :: Num a => a -> [Char] -> a
bin2dig' digint "" = digint
bin2dig' digint (x:xs) =
    let old = 2 * digint + (if x == '0' then 0 else 1)
    in bin2dig' old xs
