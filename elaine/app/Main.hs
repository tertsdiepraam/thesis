module Main where

import System.Environment
import Elaine.Exec ( exec )

main :: IO ()
main = do
  args <- getArgs
  case args of
    [file] -> exec "run" file
    [cmd, file] -> exec cmd file
    _ -> error "Invalid usage"

