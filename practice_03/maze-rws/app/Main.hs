module Main (main) where

import Types
import Search
import qualified Data.Map as M

maze :: Maze
maze = M.fromList
    [ ("старт", ["коридор_1", "тупик_1"])
    , ("коридор_1", ["комната_2", "комната_3"])
    , ("тупик_1", [])                     
    , ("комната_2", ["коридор_4"])
    , ("комната_3", ["коридор_4"])
    , ("коридор_4", ["зал_5"])
    , ("зал_5", ["лестница_6"])
    , ("лестница_6", ["коридор_7", "подвал"])
    , ("подвал", [])                     
    , ("коридор_7", ["финиш", "балкон"])
    , ("балкон", [])                    
    , ("финиш", [])
    ]
main :: IO ()
main =
    let (result, logs) = runSearch maze
    in
        (case result of
            Nothing -> putStrLn "Маршрут не найден"
            Just path -> putStrLn ("Маршрут: " ++ show path)
        ) >>
        putStrLn "Лог перемещений:" >>
        mapM_ putStrLn logs