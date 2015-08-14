-- dictionary loader
-- load the OSX dictionary file

import System.IO
import System.Random
import Data.Char

wordFile = "/usr/share/dict/words"

main = do
  word <- chooseWord
  putStrLn "I have thought of a word!"
  guessWord word []

guessWord word guessed = do
  let maskedWord = maskWord word guessed
  if not (elem '_' maskedWord) then
    do
      putStrLn "Yes! The word was " ++ word
      return ()
  else if (length guessed) > 12 then
    do
      putStrLn $ "Bad Luck!  The word was " ++ word
      return ()
  else
    do
      putStrLn $ maskedWord
      putStrLn $ "Guess a letter!"
      letter <- getLine
      guessWord word $ letter ++ guessed

chooseWord = withFile wordFile ReadMode $ \h -> do
  content <- hGetContents h
  let words = gameWords 4 8 $ lines content
  index <- randomRIO (0, length words)
  return $ map toLower $ words !! index

gameWords :: Int -> Int -> [String] -> [String]
gameWords min max words =
  filter (\w -> length w >= min && length w <= max) words

maskWord word guessed =
  map (maskChar guessed) word

maskChar guessed c
    | elem c guessed = c
    | otherwise = '_'
