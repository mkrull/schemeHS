module SchemeIn48h.Parser (
    readExpr
) where

import Text.ParserCombinators.Parsec hiding (spaces)
import Control.Monad (liftM)
import Numeric (readHex, readOct)

symbol :: Parser Char
symbol = oneOf "!$%&|*+-/:<=?>@^_~#"

spaces :: Parser ()
spaces = skipMany1 space

data LispVal = Atom String
             | List [LispVal]
             | DottedList [LispVal] LispVal
             | Number Integer
             | String String
             | Bool Bool

escapedChars :: Parser String
escapedChars = do
    _ <- char '\\'
    x <- oneOf "\\\"ntr"
    case x of
        '\\' -> do return [x]
        '"' -> do return [x]
        't' -> do return "\t"
        'n' -> do return "\n"
        'r' -> do return "\r"

parseString :: Parser LispVal
parseString = do
    _ <- char '"'
    x <- many $ many1 (noneOf "\"\\") <|> escapedChars
    _ <- char '"'
    return $ String (concat x)

parseAtom :: Parser LispVal
parseAtom = do
    first <- letter <|> symbol
    rest <- many (letter <|> digit <|> symbol)
    let atom = [first] ++ rest
    return $ case atom of
                "#t" -> Bool True
                "#f" -> Bool False
                _    -> Atom atom

parseBool :: Parser LispVal
parseBool = do
    _ <- char '#'
    x <- oneOf "tf"
    return $ case x of
        't' -> Bool True
        'f' -> Bool False

parseNumber :: Parser LispVal
parseNumber = many1 digit >>= (return . Number . read)

parseNumber' :: Parser LispVal
parseNumber' = do
    num <- parseDigital1
            <|> parseDigital2
            <|> parseHex
            <|> parseOct
            <|> parseBin
    return $ num

parseDigital1 :: Parser LispVal
parseDigital1 = many1 digit >>= (return . Number . read)

parseDigital2 :: Parser LispVal
parseDigital2 = do
    _ <- try $ string "#d"
    many1 digit >>= (return . Number . read)

parseHex :: Parser LispVal
parseHex = do
    _ <- try $ string "#x"
    x <- many1 hexDigit
    return $ Number $ hex2dig x

hex2dig :: Num a => String -> a
hex2dig x = fst $ readHex x !! 0

parseOct :: Parser LispVal
parseOct = do
    _ <- try $ string "#o"
    o <- many1 octDigit
    return $ Number $ oct2dig o

oct2dig :: Num a => String -> a
oct2dig o = fst $ readOct o !! 0

parseBin :: Parser LispVal
parseBin = do
    _ <- try $ string "#b"
    b <- many1 $ oneOf "10"
    return $ Number $ bin2dig b

bin2dig = undefined

parseExpr :: Parser LispVal
parseExpr =
    parseAtom
    <|> parseString
    <|> parseNumber
    <|> parseBool

readExpr :: String -> String
readExpr input = case parse parseExpr "lisp" input of
    Left err -> "No match: " ++ show err
    Right _ -> "Found value"
