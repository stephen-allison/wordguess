import WordGuessGame

main :: IO ()
main = do
  game <- makeGame 4 8 12
  play game
