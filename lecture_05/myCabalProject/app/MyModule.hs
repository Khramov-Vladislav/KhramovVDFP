module MyModule (myFun) where

import qualified Data.Text as T

myFun :: IO ()
myFun = putStrLn $ T.unpack $ T.pack "Hello, Haskell!"

