module Glitcher
  ( intToChar
  , intToBC
  , replaceByte
  , sortSection
  ) where

import qualified Data.ByteString.Char8 as BC

-- | Преобразует Int в Char (диапазон 0..254 как в книге)
intToChar :: Int -> Char
intToChar int = toEnum safeInt
  where safeInt = int `mod` 255

-- | Преобразует Int в односимвольную ByteString
intToBC :: Int -> BC.ByteString
intToBC int = BC.pack [intToChar int]

-- | Заменяет байт на указанной позиции (позиция от 0)
replaceByte :: Int -> Int -> BC.ByteString -> BC.ByteString
replaceByte loc chV bytes = mconcat [before, newChar, after]
  where
    (before, rest) = BC.splitAt loc bytes
    after = BC.drop 1 rest
    newChar = intToBC chV

-- | Сортирует фрагмент заданного размера, начиная с позиции start
sortSection :: Int -> Int -> BC.ByteString -> BC.ByteString
sortSection start size bytes = mconcat [before, changed, after]
  where
    (before, rest) = BC.splitAt start bytes
    (target, after) = BC.splitAt size rest
    changed = BC.reverse (BC.sort target)