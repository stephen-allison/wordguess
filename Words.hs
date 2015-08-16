module Words (randomWord) where

import Data.Char
import System.IO
import System.Random

wordfile = "/usr/share/dict/words"

randomWord :: (String -> Bool) -> IO String
randomWord filterFn = withFile wordfile ReadMode $ \h -> do
  content <- hGetContents h
  let words = filter filterFn $ lines content
  index <- randomRIO (0, length words)
  return $ map toLower $ words !! index
