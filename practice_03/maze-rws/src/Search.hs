module Search where

import Types
import Control.Monad.RWS
import qualified Data.Map as M
import qualified Data.Set as S

type MazeRWS = RWS Maze Log Visited

search :: Room -> MazeRWS (Maybe [Room])
search current =
    get >>= \visited ->
    if S.member current visited
    then return Nothing
    else
        modify (S.insert current) >>
        tell ["Переход в: " ++ current] >>
        if current == "финиш"
        then return (Just [current])
        else
            ask >>= \maze ->
            case M.lookup current maze of
                Nothing -> return Nothing
                Just nextRooms ->
                    searchList nextRooms >>= \res ->
                    case res of
                        Nothing -> return Nothing
                        Just path -> return (Just (current:path))

searchList :: [Room] -> MazeRWS (Maybe [Room])
searchList [] = return Nothing
searchList (x:xs) =
    search x >>= \res ->
    case res of
        Just path -> return (Just path)
        Nothing   -> searchList xs

runSearch :: Maze -> (Maybe [Room], Log)
runSearch maze =
    let (result, _, log) = runRWS (search "старт") maze S.empty
    in (result, log)