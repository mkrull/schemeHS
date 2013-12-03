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
import Numeric (readHex, readOct, readFloat)

parseLispNumber :: Parser LispVal
parseLispNumber = try parseLispFloat
           <|> parseDigital
           <|> parseDigitalPrefix
           <|> parseHex
           <|> parseOct
           <|> parseBin

parseDigital :: Parser LispVal
parseDigital = many1 digit >>= (return . LispNumber . read)

parseDigitalPrefix :: Parser LispVal
parseDigitalPrefix = do
    _ <- try $ string "#d"
    many1 digit >>= (return . LispNumber . read)

parseHex :: Parser LispVal
parseHex = do _ <- try $ string "#x"
              x <- many1 hexDigit
              return $ LispNumber $ hex2dig x

hex2dig :: (Eq a, Num a) => String -> a
hex2dig x = fst $ head $ readHex x

parseOct :: Parser LispVal
parseOct = do _ <- try $ string "#o"
              o <- many1 octDigit
              return $ LispNumber $ oct2dig o

oct2dig :: (Eq a, Num a) => String -> a
oct2dig o = fst $ head $ readOct o

parseBin :: Parser LispVal
parseBin = do _ <- try $ string "#b"
              b <- many1 $ oneOf "10"
              return $ LispNumber $ bin2dig b

bin2dig :: String -> Integer
bin2dig = bin2dig' 0

bin2dig' :: Num a => a -> String -> a
bin2dig' digint "" = digint
bin2dig' digint (x:xs) =
    let old = 2 * digint + (if x == '0' then 0 else 1)
    in bin2dig' old xs

parseLispFloat :: Parser LispVal
parseLispFloat = do x <- many1 digit
                    _ <- char '.'
                    y <- many1 digit
                    return $ LispFloat $ fst $ head $ readFloat $ x ++ "." ++ y
