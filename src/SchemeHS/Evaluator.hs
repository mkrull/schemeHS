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
primitives = [("+", numericBinop (+)),
              ("-", numericBinop (-)),
              ("*", numericBinop (*)),
              ("/", numericBinop (div)),
              ("mod", numericBinop (mod)),
              ("quotient", numericBinop (quot)),
              ("remainder", numericBinop (rem))]

numericBinop :: (Integer -> Integer -> Integer) -> [LispVal] -> LispVal
numericBinop op params = LispNumber $ foldl1 op $ map unpackNum params

unpackNum :: LispVal -> Integer
unpackNum (LispNumber n) = n
unpackNum (LispList [n]) = unpackNum n
unpackNum (LispString n) = let parsed = reads n in
                               if null parsed
                                 then 0
                                 else fst $ head parsed
unpackNum _ = 0