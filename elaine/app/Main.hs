module Main where
import System.Environment

import Parse

main :: IO ()
main = do
    args <- getArgs
    case args of
        [file] -> exec "parse" file
        [cmd, file] -> exec cmd file    
        _ -> error "Invalid usage" 

exec :: String -> String -> IO ()
exec cmd file = case cmd of 
    "parse" -> parseFile file
    _ -> error ("Unknown command: " ++ cmd)