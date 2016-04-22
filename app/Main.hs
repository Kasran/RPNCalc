module Main where

import Lib
import Data.List (intercalate)
import GHC.IO.Handle (hFlush)
import GHC.IO.Handle.FD (stdout)
import Control.Monad (when)

showStack :: [Int] -> String
showStack xs = ("-> "++) $ if (xs==[]) then "(empty stack)" else (intercalate " " $ map show $ reverse xs)

showResult :: Either String [Int] -> String
showResult = either ("There was an error:\n"++) showStack

procRun :: [Int] -> String -> (String, [Int])
procRun stk = either (\s -> ("There was an error:\n"++s, stk)) (\r -> ("", r)) . rpnParseRun stk

prompt :: String -> IO String
prompt str = do {
    putStr str;
    hFlush stdout;
    getLine;
}

runRepl :: [Int] -> IO ()
runRepl stk = do {
    cmd <- prompt "<- ";
    when (cmd /= []) $ do {
        (err, newStk) <- return $ procRun stk cmd;
        when (err /= []) $ putStrLn err;
        putStrLn $ showStack newStk;
        runRepl newStk;
    }
}

main :: IO ()
main = runRepl []