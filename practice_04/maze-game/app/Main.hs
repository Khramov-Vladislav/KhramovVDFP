module Main where

import System.Environment (getArgs)
import Game (runGameFromFile)

main :: IO ()
main = do
    args <- getArgs
    case args of
        [path] -> runGameFromFile path
        _ -> putStrLn "Usage: maze-game <path-to-maze-file>"