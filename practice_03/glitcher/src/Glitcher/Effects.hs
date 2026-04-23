module Glitcher.Effects
  ( randomReplaceByte
  , randomSortSection
  ) where

import System.Random (randomRIO)
import qualified Data.ByteString.Char8 as BC
import Glitcher.Core (replaceByte, sortSection)

randomReplaceByte :: BC.ByteString -> IO BC.ByteString
randomReplaceByte bytes = do
    let bytesLength = BC.length bytes
    location <- randomRIO (0, bytesLength - 1)
    newValue <- randomRIO (0, 255)
    return (replaceByte location newValue bytes)

randomSortSection :: BC.ByteString -> IO BC.ByteString
randomSortSection bytes = do
    let sectionSize = 25
    let bytesLength = BC.length bytes
    start <- randomRIO (0, bytesLength - sectionSize)
    return (sortSection start sectionSize bytes)