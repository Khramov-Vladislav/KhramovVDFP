module Main (main) where 

import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.FilePath (takeBaseName)
import Control.Monad (foldM)  
import qualified Data.ByteString.Char8 as BC
import Glitcher.Effects (randomReplaceByte, randomSortSection)

glitchActions :: [BC.ByteString -> IO BC.ByteString]
glitchActions =
    [ randomReplaceByte
    , randomSortSection
    , randomReplaceByte
    , randomSortSection
    , randomReplaceByte
    ]

saveStep :: String -> String -> BC.ByteString -> IO ()
saveStep baseName suffix content = do
    let fileName = "glitched_" ++ baseName ++ "_" ++ suffix ++ ".jpg"
    BC.writeFile fileName content
    putStrLn $ "Saved: " ++ fileName

main :: IO ()
main = do
    args <- getArgs
    case args of
        [] -> do
            putStrLn "Usage: glitcher <image-file>"
            exitFailure
        (fileName:_) -> do
            let baseName = takeBaseName fileName
            imageFile <- BC.readFile fileName
            putStrLn $ "Loaded: " ++ fileName

            let steps = zip glitchActions ([1..] :: [Int])
            final <- foldM (\current (action, stepNum) -> do
                                new <- action current
                                saveStep baseName (show stepNum) new
                                return new
                          ) imageFile steps

            let finalName = "glitched_" ++ fileName
            BC.writeFile finalName final
            putStrLn $ "Final image saved as: " ++ finalName