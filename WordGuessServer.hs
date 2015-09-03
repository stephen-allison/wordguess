import Network
import Control.Concurrent
import System.IO

import WordGuessGame

data Action = New | Play (Int, Char)

type GameStore = MVar [(Int,Game)]

main = withSocketsDo $ do
  sock <- listenOn $ PortNumber 5000
  gameStore <- newEmptyMVar
  putMVar gameStore []
  acceptConnection gameStore sock

acceptConnection gameStore sock = do
  (h,_,_) <- accept sock
  forkIO $ serveGame gameStore h
  acceptConnection gameStore sock

serveGame gameStore h = do
  input <- hGetLine h
  (gid,g) <- handleInput gameStore input
  let messages = fmap (hPutStrLn h) $ gameMessages g
  hPutStrLn h $ "Playing Game #" ++ show gid
  sequence messages
  putStrLn $ show g
  hFlush h
  hClose h

handleInput :: GameStore -> String -> IO (Int, Maybe Game)
handleInput gameStore input = do
  case parseInput input of New -> startGame gameStore
                           Play (gameId, letter) -> updateGame gameStore gameId letter

updateGame gameStore gameId letter = do
  game <- findGame gameStore gameId
  let updated = fmap ((flip guessLetter) (Just letter)) game
  storeMaybeGame gameStore gameId updated
  return (gameId, updated)

startGame gameStore = do
  g <- makeGame 4 8 12
  gameId <- storeNewGame gameStore g
  return (gameId, Just g)

gameMessages Nothing = []
gameMessages (Just game) = feedback game

parseInput :: String -> Action
parseInput input
  | trim input == "new" = New
  | otherwise = Play (gameId, letter)
    where
    inputWords = words input
    gameId = read (inputWords !! 0) :: Int
    letter = (inputWords !! 1) !! 0

findGame :: GameStore -> Int -> IO (Maybe Game)
findGame gameStore gameId = do
  gameList <- takeMVar gameStore
  putStrLn $ "find game " ++ show gameId

  let game = lookup gameId gameList
  putMVar gameStore gameList
  putStrLn $ "find game " ++ show game
  return game


storeMaybeGame :: GameStore -> Int -> Maybe Game -> IO ()
storeMaybeGame gameStore gameId (Just game) = storeGame gameStore gameId game
storeMaybeGame _ _ Nothing = return ()

storeGame :: GameStore -> Int -> Game -> IO ()
storeGame gameStore gameId game = do
  gameList <- takeMVar gameStore
  let gameList' = filter (\(gid,g) -> gid /= gameId) gameList
  putMVar gameStore $ (gameId, game) : gameList'

storeNewGame :: GameStore -> Game -> IO Int
storeNewGame gameStore game = do
  gameList <- takeMVar gameStore
  let gameId = length gameList
  putMVar gameStore $ (gameId, game) : gameList
  return gameId

trim = unwords . words
