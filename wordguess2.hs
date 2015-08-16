import Data.Char
import System.IO
import System.Process
import System.Random

data GameStatus = Won | Lost | Playing deriving (Show, Eq)


data Game = Game {wordToGuess :: String,
                  lettersGuessed :: [Char],
                  guessesLeft :: Int} deriving (Show)

newGame :: String -> Int -> Game
newGame word guesses = Game { wordToGuess = word,
                              lettersGuessed = [],
                              guessesLeft = guesses}



wordFile = "/usr/share/dict/words"

chooseWord :: Int -> Int -> IO String
chooseWord min max = withFile wordFile ReadMode $ \h -> do
  content <- hGetContents h
  let words = gameWords min max $ lines content
  index <- randomRIO (0, length words)
  return $ map toLower $ words !! index



gameWords :: Int -> Int -> [String] -> [String]
gameWords min max words =
  filter (\w -> length w >= min && length w <= max) words



makeGame :: Int -> Int -> Int -> IO Game
makeGame minLen maxLen maxGuesses = do
  word <- chooseWord minLen maxLen
  putStrLn "I have chosen a word!"
  putStrLn $ (take (length word)) $ repeat '_'
  return $ newGame word maxGuesses


main = do
  game <- makeGame 4 8 12
  play game




play :: Game -> IO ()
play g = do
  putStrLn "Pick a letter"
  line <- getLine
  letter <- inputFeedback . alreadyGuessed g . letterFromLine $ line
  let updatedGame = guessLetter g letter
  let turnResult = feedback updatedGame
  putStrLn $ unlines turnResult
  case gameStatus updatedGame of Won -> endGame updatedGame
                                 Lost -> endGame updatedGame
                                 Playing -> play updatedGame



endGame :: Game -> IO ()
endGame g = do
  definition <- readProcess "./definition.py" [wordToGuess g] []
  putStrLn definition
  return ()



inputFeedback :: Maybe Char -> IO (Maybe Char)
inputFeedback Nothing = do
  putStrLn "That wasn't a letter, or you already tried it"
  return Nothing

inputFeedback (Just c) = do
  return (Just c)



letterFromLine :: String -> Maybe Char
letterFromLine [] = Nothing
letterFromLine s
  | isLetter c = Just c
  | otherwise = Nothing
  where c = toLower . (!! 0) $ s



alreadyGuessed :: Game -> Maybe Char -> Maybe Char
alreadyGuessed g Nothing = Nothing
alreadyGuessed g (Just c)
  | elem c guessed = Nothing
  | otherwise = Just c
  where guessed = lettersGuessed g



guessLetter :: Game -> Maybe Char -> Game
guessLetter game Nothing = game
guessLetter game (Just letter) = Game{ wordToGuess = wordToGuess game,
                                       lettersGuessed = [letter] ++ lettersGuessed game,
                                       guessesLeft = (guessesLeft game) - 1 }



gameStatus :: Game -> GameStatus
gameStatus g
  | wordGuessed g = Won
  | guessesLeft g == 0 = Lost
  | otherwise = Playing



wordGuessed :: Game -> Bool
wordGuessed g = all (\c -> elem c guessed) word
  where word = wordToGuess g
        guessed = lettersGuessed g



maskedWord :: Game -> String
maskedWord g = map (maskChar guessed) word
  where word = wordToGuess g
        guessed = lettersGuessed g



maskChar :: [Char] -> Char -> Char
maskChar guessed c
    | elem c guessed = c
    | otherwise = '_'



feedback :: Game -> [String]
feedback g
    | gs == Won = wonFeedback g
    | gs == Lost = lostFeedback g
    | gs == Playing = playingFeedback g
    where gs = gameStatus g



playingFeedback :: Game -> [String]
playingFeedback g = [maskedWord g,
                    "You have guessed " ++ lettersGuessed g,
                    show (guessesLeft g) ++ " guesses remaining!"]

lostFeedback :: Game -> [String]
lostFeedback g = ["Bad Luck!  No more guesses!", "The word was " ++ wordToGuess g]

wonFeedback :: Game -> [String]
wonFeedback g = ["Yes!  You guessed the word was " ++ wordToGuess g]
