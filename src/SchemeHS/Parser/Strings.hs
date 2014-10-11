module SchemeHS.Parser.Strings (
    parseLispString,
    parseLispChar
) where

import           SchemeHS.Types
import           Text.ParserCombinators.Parsec

parseLispString :: Parser LispVal
parseLispString = do char '"'
                     val <- many $ many1 (noneOf "\\\"") <|> escapedChars
                     char '"'
                     (return . LispString . concat) val

escapedChars :: Parser String
escapedChars = do char '\\'
                  char <- oneOf "\\\"ntr"
                  case char of
                     '\\' -> return [char]
                     '"' -> return [char]
                     't' -> return "\t"
                     'n' -> return "\n"
                     'r' -> return "\r"

parseLispChar :: Parser LispVal
parseLispChar = do _ <- try $ string "#\\"
                   char <- parseCharName <|> anyChar
                   return $ LispChar char

parseCharName :: GenParser Char st Char
parseCharName = do charname <- try (string "space" <|> string "newline")
                   case charname of
                            "space" -> return ' '
                            "newline" -> return '\n'
