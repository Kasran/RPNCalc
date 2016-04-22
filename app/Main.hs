module Main where

import Lib
import Data.List (intercalate)
import GHC.IO.Handle (hFlush)
import GHC.IO.Handle.FD (stdout)
import Control.Monad (when)

showStack :: [Int] -> String
showStack xs = "-> "++(intercalate " " $ map show $ reverse xs)

showResult :: Either String [Int] -> String
showResult = either ("There was an error:\n"++) showStack

prompt :: String -> IO String
prompt str = do {
    putStr str;
    hFlush stdout;
    getLine;
}

main :: IO ()
main = do {
    str <- prompt "<- ";
    when (str /= []) $ do {
        putStrLn $ showResult (rpnParseRun str);
        main;
    }
}