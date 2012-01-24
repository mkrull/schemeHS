module Main where

import System.Environment
import SchemeIn48h.Parser

main :: IO ()
main = do
    args <- getArgs
    putStrLn $ readExpr $ args !! 0

