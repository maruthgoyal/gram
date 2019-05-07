module Main where

import           Eval
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
      putStrLn "================================================\n \t ANNOTATION \n==============================================="
      tpprint $ infer program
      putStrLn "================================================\n \t EVALUATION \n==============================================="
      runProgram program
