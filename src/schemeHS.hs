module Main where

import           SchemeHS.Parser    (readExpr)
import           SchemeHS.Evaluator    (evalLisp)
import           System.Environment (getArgs)
import           System.IO          (readFile)

main :: IO ()
main = do
    args <- getArgs
    content <- readFile $ head args
    print $ evalLisp $ readExpr content
