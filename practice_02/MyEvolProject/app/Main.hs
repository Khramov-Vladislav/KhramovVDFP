module Main where

import MyEvolModule
import Data.List (sort)

main :: IO ()
main = putStrLn $ show $ sort ([minBound .. maxBound] :: [MyEvolution])
