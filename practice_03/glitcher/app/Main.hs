import System.Environment (getArgs)
import System.Random (randomRIO)
import qualified Data.ByteString.Char8 as BC
import Control.Monad (foldM)
import Glitcher (replaceByte, sortSection)

randomReplaceByte :: BC.ByteString -> IO BC.ByteString
randomReplaceByte bytes = do
    let bytesLength = BC.length bytes
    if bytesLength == 0
        then return bytes
        else do
            location <- randomRIO (1, bytesLength)
            chV <- randomRIO (0, 255)
            return (replaceByte location chV bytes)

randomSortSection :: BC.ByteString -> IO BC.ByteString
randomSortSection bytes = do
    let sectionSize = 25
    let bytesLength = BC.length bytes
    if bytesLength < sectionSize
        then return bytes
        else do
            start <- randomRIO (0, bytesLength - sectionSize)
            return (sortSection start sectionSize bytes)

glitchActions :: [BC.ByteString -> IO BC.ByteString]
glitchActions =
  [ randomReplaceByte
  , randomSortSection
  , randomReplaceByte
  , randomSortSection
  , randomReplaceByte
  ]

takeBaseName :: String -> String
takeBaseName = takeWhile (/= '.')

takeExtension :: String -> String
takeExtension = dropWhile (/= '.')

main :: IO ()
main = do
    args <- getArgs
    case args of
        [] -> putStrLn "Usage: glitcher <filename>"
        (fileName:_) -> do
            let base = takeBaseName fileName
                ext  = takeExtension fileName

            imageFile <- BC.readFile fileName

            _ <- foldM
                  (\(step, bytes) action -> do
                      newBytes <- action bytes
                      let outName = "glitched_" ++ show (step :: Int) ++ "_" ++ base ++ ext
                      BC.writeFile outName newBytes
                      putStrLn $ "Saved: " ++ outName
                      return (step + 1, newBytes))
                  (1 :: Int, imageFile)
                  glitchActions

            putStrLn "All done!"