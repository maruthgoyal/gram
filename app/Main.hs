module Main where

import           Infer
import           Pretty
import           System.Environment

main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> error "Usage: stack run <file_name>"
    (x:[]) -> do
      program <- readFile x
      tpprint $ infer program
