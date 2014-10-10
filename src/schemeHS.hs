module Main where

import           SchemeHS.Parser    (readExpr)
import           System.Environment (getArgs)
import           System.IO          (readFile)

main :: IO ()
main = do
    args <- getArgs
    content <- readFile $ head args
    putStrLn $ readExpr content
