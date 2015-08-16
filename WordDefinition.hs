module WordDefinition (definition) where

import System.Process

definition :: String -> IO String
definition word = do
  def <- readProcess "./definition.py" [word] []
  return def
