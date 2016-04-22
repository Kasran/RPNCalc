module Lib (
    Operation,
    rpnRun,
    rpnRunEmpty,
    rpnParse,
    rpnParseRun,
    rpnParseRunEmpty
) where

import Data.Char (isDigit)
import Data.List (intercalate)
import Data.Either (isRight, rights, lefts)
import Control.Monad ((<=<))

data Operation = Push Int | Add | Sub | Mul | Div | Mod | Switch | Dup | Pop | Clear deriving Show

rpnOp :: Operation -> [Int] -> Either String [Int]
rpnOp (Push x) xs = Right (x:xs)
rpnOp Add (x:y:xs) = Right ((x+y):xs)
rpnOp Sub (x:y:xs) = Right ((y-x):xs)
rpnOp Mul (x:y:xs) = Right ((x*y):xs)
rpnOp Div (x:y:xs) = Right ((y`div`x):xs)
rpnOp Mod (x:y:xs) = Right ((y`mod`x):xs)
rpnOp Dup (x:xs) = Right (x:x:xs)
rpnOp Switch (x:y:xs) = Right (y:x:xs)
rpnOp Pop (x:xs) = Right xs
rpnOp Clear _ = Right []
rpnOp o _ = Left ("not enough items on the stack for operation "++(show o))

-- applying operators in order

rpnRun :: [Int] -> [Operation] ->  Either String [Int]
rpnRun stk ops = foldl (>>=) (return stk) $ (map rpnOp ops)

rpnRunEmpty :: [Operation] -> Either String [Int]
rpnRunEmpty = rpnRun []

-- getting operations from strings

rpnParseOp :: String -> Either String Operation
rpnParseOp s
  | all isDigit s = Right $ Push (read s :: Int)
  | s == "+"  = Right Add
  | s == "-"  = Right Sub
  | s == "*"  = Right Mul
  | s == "/"  = Right Div
  | s == "%"  = Right Mod
  | s == ":"  = Right Dup
  | s == "\\" = Right Switch
  | s == "$"  = Right Pop
  | s == "C"  = Right Clear
  | otherwise = Left $ "invalid token "++s

rpnFixOps :: [Either String Operation] -> Either String [Operation]
rpnFixOps ops
  | all isRight ops = Right $ rights ops
  | otherwise = Left $ "parse failed:\n\t"++(intercalate "\n\t" $ lefts ops)

rpnParse :: String -> Either String [Operation]
rpnParse s = rpnFixOps $ map rpnParseOp (words s)

rpnParseRun :: [Int] -> String -> Either String [Int]
rpnParseRun stk = (rpnRun stk) <=< rpnParse

rpnParseRunEmpty :: String -> Either String [Int]
rpnParseRunEmpty = rpnParseRun []



