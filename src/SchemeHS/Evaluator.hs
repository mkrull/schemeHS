module SchemeHS.Evaluator (
    evalLisp
) where

import           SchemeHS.Parser
import           SchemeHS.Types

evalLisp :: LispVal -> LispVal
evalLisp val@(LispString _) = val
evalLisp val@(LispNumber _) = val
evalLisp val@(LispBool _) = val
evalLisp (LispList [LispAtom "quote", val]) = val
evalLisp (LispList (LispAtom func : args)) = apply func $ map evalLisp args

apply :: String -> [LispVal] -> LispVal
apply func args = maybe (LispBool False) ($ args) $ lookup func primitives

primitives :: [(String, [LispVal] -> LispVal)]
primitives = [("+", numericBinop (+))]

numericBinop :: (Integer -> Integer -> Integer) -> [LispVal] -> LispVal
numericBinop op params = LispNumber $ foldl1 op $ map unpackNum params

unpackNum :: LispVal -> Integer
unpackNum (LispNumber n) = n
unpackNum (LispList [n]) = unpackNum n
unpackNum _ = 0