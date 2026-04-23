module Main where

import Types
import Search
import qualified Data.Map as M

maze :: Maze
maze = M.fromList
    [ ("старт", ["a", "b"])
    , ("a", ["c"])
    , ("b", ["d"])
    , ("c", ["финиш"])
    , ("d", [])
    , ("финиш", [])
    ]

main :: IO ()
main =
    let (result, log) = runSearch maze
    in
        case result of
            Nothing ->
                putStrLn "Маршрут не найден"
            Just path ->
                putStrLn ("Маршрут: " ++ show path)
        >>
        putStrLn "Лог перемещений:"
        >>
        mapM_ putStrLn log