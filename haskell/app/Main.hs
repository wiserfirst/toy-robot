module Main where

import Lib

main :: IO ()
main = do content <- getContents
          simulate Nothing $ parse $ lines content
