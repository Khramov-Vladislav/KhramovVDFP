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
            location <- randomRIO (0, bytesLength - 1)
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
glitchActions = [randomReplaceByte, randomSortSection, randomReplaceByte, randomSortSection, randomReplaceByte]

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
                  (\(step, prevBytes) action -> do
                      newBytes <- action prevBytes
                      let outName = "glitched_" ++ show step ++ "_" ++ base ++ ext
                      BC.writeFile outName newBytes
                      putStrLn $ "Saved: " ++ outName
                      return (step + 1, newBytes))
                  (1 :: Int, imageFile)
                  glitchActions

            putStrLn "All done!"
{-
    Работа выполнена с использованием DeepSeek-V3

    Промт:
    1)
        - Создайте проект по генерации глитчей на изображениях согласно Уроку 25.2 из "Программируй на HASKELL" Уилл Курт
        - Вынесите все чистые функции в src.
        - Сохраните отдельную версию файла для каждого изменения изображения
        проект создай в stack, код должен быть взят из книги (файлы книги были приложены к запросу)
     
    2) ошибки пр сборке и запуске, нужно убрать warnings
-}
