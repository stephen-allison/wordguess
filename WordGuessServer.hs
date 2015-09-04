import Data.Char
import Network
import Control.Concurrent
import System.IO

import WordGuessGame

data Action = New | Play (Int, Char)

type GameInfo = (Int,Game)
type GameStore = MVar [GameInfo]

main = withSocketsDo $ do
  sock <- listenOn $ PortNumber 5000
  gameStore <- newMVar []
  acceptConnection gameStore sock


acceptConnection gameStore sock = do
  (h,_,_) <- accept sock
  forkIO $ serveGame gameStore h
  acceptConnection gameStore sock


serveGame gameStore h = do
  input <- hGetLine h
  gameInfo <- handleInput gameStore input
  showGame h gameInfo
  cleanup gameStore gameInfo
  hFlush h
  hClose h


cleanup :: GameStore -> Maybe GameInfo -> IO ()
cleanup _ Nothing = return ()
cleanup gameStore (Just (gid, g)) = do
  case gameStatus g of Won -> removeGame gameStore gid
                       Lost -> removeGame gameStore gid
                       _ -> return ()


showGame :: Handle -> Maybe GameInfo -> IO ()
showGame h (Just (gid, g)) = do
  let messages = fmap (hPutStrLn h) $ feedback g
  hPutStrLn h $ "Playing Game #" ++ show gid
  sequence messages
  putStrLn $ show g


showGame h Nothing = do
  hPutStrLn h "No game found"


handleInput :: GameStore -> String -> IO (Maybe GameInfo)
handleInput gameStore input = do
  case parseInput input of
    Just New -> startGame gameStore
    Just (Play (gameId, letter)) -> updateGame gameStore gameId letter
    _ -> return Nothing


updateGame :: GameStore -> Int -> Char -> IO (Maybe GameInfo)
updateGame gameStore gameId letter = do
  gameInfo <- findGame gameStore gameId
  case gameInfo of Just (gid, game) -> doUpdate gameStore gid game letter
                   Nothing -> return Nothing


doUpdate :: GameStore -> Int -> Game -> Char -> IO (Maybe GameInfo)
doUpdate gameStore gameId game letter = do
  let updated = guessLetter game (Just letter)
  storeGame gameStore gameId updated
  return $ Just (gameId, updated)


startGame :: GameStore -> IO (Maybe GameInfo)
startGame gameStore = do
  g <- makeGame 4 8 12
  gameId <- storeNewGame gameStore g
  return $ Just (0, g)


parseInput :: String -> Maybe Action
parseInput input = turnParams $ words input
  where
  turnParams (g:c:rest)
    | all isDigit g = Just $ Play ((read g) :: Int, c !! 0)
    | otherwise = Nothing
  turnParams (n:[])
    | n == "new" = Just New
    | otherwise = Nothing


findGame :: GameStore -> Int -> IO (Maybe GameInfo)
findGame gameStore gameId = do
  gameList <- readMVar gameStore
  return $ (,) <$> Just gameId <*> lookup gameId gameList


removeGame :: GameStore -> Int -> IO ()
removeGame gameStore gameId = do
  modifyMVar_ gameStore $ \gameList -> do
    return $ filter (\(gid,g) -> gid /= gameId) gameList


storeGame :: GameStore -> Int -> Game -> IO ()
storeGame gameStore gameId game = do
  modifyMVar_ gameStore $ \gameList -> do
    let gameList' = filter (\(gid,g) -> gid /= gameId) gameList
    return $ (gameId, game) : gameList'


storeNewGame :: GameStore -> Game -> IO Int
storeNewGame gameStore game = do
  gameId <- modifyMVar gameStore $ \gameList -> do
      let gameId = length gameList
      return $ ((gameId, game) : gameList, gameId)
  return gameId

trim = unwords . words
