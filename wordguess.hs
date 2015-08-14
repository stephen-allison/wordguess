-- Simple word guessing game for helping learn Haskell

import System.IO
import System.Process
import System.Random
import Data.Char

-- location of the word fileon OSX
wordFile = "/usr/share/dict/words"

main = do
  word <- chooseWord 4 8
  putStrLn "I have thought of a word!"
  guessWord word [] 12
  definition <- readProcess "./definition.py" [word] []
  putStrLn definition

-- loop until word guessed or all guesses used
guessWord word guessed triesAllowed = do
  if (wordGuessed word guessed) then do
    putStrLn $ "Yes! The word was " ++ word
    return ()
  else if (guessesUsed guessed triesAllowed) then do
    putStrLn $ "Bad Luck!  The word was " ++ word
    return ()
  else do
    putStrLn $ maskWord word guessed
    putStrLn $ turnStatus guessed triesAllowed
    putStrLn $ "Guess a letter!"
    line <- getLine
    let letter = toLower $ firstChar line
    if (elem letter guessed) then do
      putStrLn $ "You have already tried " ++ [letter]
      guessWord word guessed triesAllowed
    else if (not $ isLetter letter) then do
      putStrLn "That wasn't a letter."
      guessWord word guessed triesAllowed
    else do
      guessWord word ([letter] ++ guessed) triesAllowed

firstChar :: [Char] -> Char
firstChar [] = '*'
firstChar (x:xs) = x

-- load the word file and pick a random word
chooseWord min max = withFile wordFile ReadMode $ \h -> do
  content <- hGetContents h
  let words = gameWords min max $ lines content
  index <- randomRIO (0, length words)
  return $ map toLower $ words !! index

turnStatus :: [Char] -> Int -> String
turnStatus [] allowed = remaining ++ " guesses remaining!"
  where remaining = show allowed

turnStatus guesses allowed = "You have already guessed " ++ guesses ++ ", " ++ remaining ++ " remaining!"
  where remaining = show $ allowed - length guesses

gameWords :: Int -> Int -> [String] -> [String]
gameWords min max words =
  filter (\w -> length w >= min && length w <= max) words

maskWord :: String -> [Char] -> String
maskWord word guessed =
  map (maskChar guessed) word

maskChar :: [Char] -> Char -> Char
maskChar guessed c
    | elem c guessed = c
    | otherwise = '_'

wordGuessed :: String -> [Char] -> Bool
wordGuessed word guessed = all (\c -> elem c guessed) word

guessesUsed :: [a] -> Int -> Bool
guessesUsed guesses allowed = (length guesses) >= allowed

