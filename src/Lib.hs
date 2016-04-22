module Lib (
    Operation,
    rpnRun,
    rpnParse,
    rpnParseRun
) where

import Data.Char (isDigit)
import Data.List (intercalate)
import Data.Either (isRight, rights, lefts)

data Operation = Push Int | Add | Sub | Mul | Div | Switch | Dup deriving Show

rpnOp :: Operation -> [Int] -> Either String [Int]
rpnOp (Push x) xs = Right (x:xs)
rpnOp Add (x:y:xs) = Right ((x+y):xs)
rpnOp Sub (x:y:xs) = Right ((y-x):xs)
rpnOp Mul (x:y:xs) = Right ((x*y):xs)
rpnOp Div (x:y:xs) = Right ((x`div`y):xs)
rpnOp Dup (x:xs) = Right (x:x:xs)
rpnOp Switch (x:y:xs) = Right (y:x:xs)
rpnOp o _ = Left ("not enough items on the stack for operation "++(show o))

-- applying operators in order

rpnRun :: [Operation] -> Either String [Int]
rpnRun ops = foldl (>>=) (return []) $ (map rpnOp ops)


-- getting operations from strings

rpnParseOp :: String -> Either String Operation
rpnParseOp s
  | all isDigit s = Right $ Push (read s :: Int)
  | s == "+"  = Right Add
  | s == "-"  = Right Sub
  | s == "*"  = Right Mul
  | s == "/"  = Right Div
  | s == ":"  = Right Dup
  | s == "\\" = Right Switch
  | otherwise = Left $ "invalid token "++s

rpnFixOps :: [Either String Operation] -> Either String [Operation]
rpnFixOps ops
  | all isRight ops = Right $ rights ops
  | otherwise = Left $ "parse failed:\n\t"++(intercalate "\n\t" $ lefts ops)

rpnParse :: String -> Either String [Operation]
rpnParse s = rpnFixOps $ map rpnParseOp (words s)

rpnParseRun :: String -> Either String [Int]
rpnParseRun = (rpnRun =<<) . rpnParse


