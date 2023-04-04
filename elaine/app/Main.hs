module Main where

import Elaine.Parse
import Elaine.Pretty
import System.Environment
import Text.Pretty.Simple (pPrint)

main :: IO ()
main = do
  args <- getArgs
  case args of
    [file] -> exec "parse" file
    [cmd, file] -> exec cmd file
    _ -> error "Invalid usage"

exec :: String -> String -> IO ()
exec cmd file = case cmd of
  "parse" -> printPretty file
  "ast" -> printAST file
  _ -> error ("Unknown command: " ++ cmd)

printPretty :: FilePath -> IO ()
printPretty filename = do
  x <- parseFile filename
  case x of
    Just a -> putStrLn $ pretty a
    Nothing -> return ()

printAST :: FilePath -> IO ()
printAST filename = do
  x <- parseFile filename
  case x of
    Just a -> pPrint a
    Nothing -> return ()