module Main (main) where

import Lib
import System.Environment

main :: IO ()
main = do
    args <- getArgs
    let i = read $ args !! 0 :: Int
    prob i

