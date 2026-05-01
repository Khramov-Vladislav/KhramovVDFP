{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Game ( runGameFromFile, runGame ) where

import Control.Monad (unless)
import Control.Monad.Reader (MonadReader(ask), ReaderT, runReaderT)
import Control.Monad.Writer (MonadWriter(tell), WriterT, runWriterT)
import Control.Monad.IO.Class (MonadIO(liftIO))
import Control.Monad.Trans (lift)
import qualified Data.Map as M
import qualified Data.Set as S
import System.IO (hFlush, stdout)
import MyStateT (MyStateT, getMy, putMy, runMyStateT, modifyMy)

-- Типы
type Room = String
type Maze = M.Map Room [Room]
type Log = [String]

-- Стек трансформеров: ReaderT (окружение) + WriterT (лог) + MyStateT (текущая комната) + IO
type GameMonad a = ReaderT Maze (WriterT Log (MyStateT Room IO)) a

-- Запуск игры: карта, начальная комната -> IO ((), итоговый лог)
runGame :: Maze -> Room -> IO ((), Log)
runGame maze start = do
    ((result, log), _) <- runMyStateT (runWriterT (runReaderT gameLoop maze)) start
    return (result, log)
  where
    gameLoop :: GameMonad ()
    gameLoop = do
        current <- lift $ lift getMy
        if current == "финиш"
            then return ()
            else do
                liftIO $ putStrLn $ "\nВы находитесь в комнате: " ++ current
                neighbours <- ask >>= return . M.findWithDefault [] current
                if null neighbours
                    then liftIO $ putStrLn "Здесь нет выходов. Вы застряли!"
                    else do
                        liftIO $ putStrLn $ "Соседние комнаты: " ++ show neighbours
                        liftIO $ putStr "Куда идём? (введите название, или 'выйти' для завершения): "
                        liftIO $ hFlush stdout
                        cmd <- liftIO getLine
                        case cmd of
                            "выйти" -> return ()
                            _ | cmd `elem` neighbours -> do
                                lift $ lift $ putMy cmd
                                lift $ tell ["Переход из " ++ current ++ " в " ++ cmd]
                                gameLoop
                              | otherwise -> do
                                liftIO $ putStrLn "Неверное направление. Попробуйте снова."
                                gameLoop

-- Загрузка карты из файла (формат: комната: сосед1,сосед2,...)
loadMazeFromFile :: FilePath -> IO Maze
loadMazeFromFile path = do
    contents <- readFile path
    let lines' = filter (not . null) (lines contents)
        splitOn _ "" = []
        splitOn c s = case break (== c) s of
            (a, b) -> a : if null b then [] else splitOn c (tail b)
        trim = dropWhile (== ' ') . reverse . dropWhile (== ' ') . reverse
        parseLine l = case break (== ':') l of
            (room, rest) -> (trim room, map trim (splitOn ',' (dropWhile (== ':') rest)))
    return $ M.fromList $ map parseLine lines'

-- Запуск игры из файла
runGameFromFile :: FilePath -> IO ()
runGameFromFile path = do
    maze <- loadMazeFromFile path
    let startRoom = "старт"
    unless (M.member startRoom maze) $
        fail "В лабиринте нет комнаты 'старт'"
    (_, log) <- runGame maze startRoom
    putStrLn "\n=== Игра завершена ==="
    putStrLn "Лог перемещений:"
    mapM_ putStrLn log