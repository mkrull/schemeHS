module Main where

import System.Environment
import SchemeHS.Parser

main :: IO ()
main = do expression <- getArgs
          putStrLn ( readExpr (expression !! 0))

