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
  hPutStrLn h input
  g <- handleInput gameStore input
  putStrLn $ show g
  hPutStrLn h $ show g
  hFlush h
  hClose h


handleInput gameStore input
  | trim input == "new" = do
                    g <- makeGame 4 8 12
                    storeGame gameStore g
                    return $ Just g
  | otherwise = do
                game <- findGame gameStore $ words input
                let letter =  (words input) !! 1
                let updated = fmap ((flip guessLetter) (letterFromLine letter)) game
                storeMaybeGame gameStore updated
                return updated



findGame gameStore inputWords = do
  gameList <- takeMVar gameStore
  let gameId = read (inputWords !! 0) :: Int
  putStrLn $ "find game " ++ show inputWords
  putStrLn $ "find game " ++ show gameList

  if gameId < (length gameList) then do
    let game = snd (gameList !! gameId)
    putStrLn $ "find game " ++ show game
    putMVar gameStore gameList
    return $ Just game
  else do
    putMVar gameStore gameList
    return Nothing


storeMaybeGame gameStore (Just game) = storeGame gameStore game
storeMaybeGame _ Nothing = return ()

storeGame gameStore game = do
  gameList <- takeMVar gameStore
  putMVar gameStore $ (length gameList, game) : gameList

trim = unwords . words
