import Network
import Control.Concurrent
import System.IO

import WordGuessGame

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

handleInput :: MVar [(Int,Game)] -> String -> IO (Int, Maybe Game)
handleInput gameStore input
  | trim input == "new" = do
                    g <- makeGame 4 8 12
                    gameId <- storeNewGame gameStore g
                    return $ (gameId, Just g)
  | otherwise = do
                let (gameId, letter) = parseInput input
                game <- findGame gameStore gameId
                let updated = fmap ((flip guessLetter) (letterFromLine letter)) game
                storeMaybeGame gameStore gameId updated
                return (gameId, updated)

gameMessages Nothing = []
gameMessages (Just game) = feedback game

parseInput :: String -> (Int, String)
parseInput input = (gameId, letter)
  where
    inputWords = words input
    gameId = read (inputWords !! 0) :: Int
    letter = inputWords !! 1

findGame :: MVar [(Int,Game)] -> Int -> IO (Maybe Game)
findGame gameStore gameId = do
  gameList <- takeMVar gameStore
  putStrLn $ "find game " ++ show gameId

  let game = lookup gameId gameList
  putMVar gameStore gameList
  putStrLn $ "find game " ++ show game
  return game


storeMaybeGame :: MVar [(Int,Game)] -> Int -> Maybe Game -> IO ()
storeMaybeGame gameStore gameId (Just game) = storeGame gameStore gameId game
storeMaybeGame _ _ Nothing = return ()

storeGame :: MVar [(Int,Game)] -> Int -> Game -> IO ()
storeGame gameStore gameId game = do
  gameList <- takeMVar gameStore
  let gameList' = filter (\(gid,g) -> gid /= gameId) gameList
  putMVar gameStore $ (gameId, game) : gameList'

storeNewGame :: MVar [(Int,Game)] -> Game -> IO Int
storeNewGame gameStore game = do
  gameList <- takeMVar gameStore
  let gameId = length gameList
  putMVar gameStore $ (gameId, game) : gameList
  return gameId

trim = unwords . words
