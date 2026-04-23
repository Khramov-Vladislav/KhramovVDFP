module Types where

import qualified Data.Map as M
import qualified Data.Set as S

type Room = String
type Maze = M.Map Room [Room]

type Log = [String]
type Visited = S.Set Room