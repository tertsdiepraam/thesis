module Elaine.Exec where

import Data.Bifunctor (first)
import Data.Text (Text, pack, unpack)
import Elaine.AST (Program, Value, ValueType)
import Elaine.ElabTransform (elabTrans)
import Elaine.Eval (eval)
import Elaine.Parse (parseProgram)
import Elaine.TypeCheck (typeCheck, getMain)
import Prelude hiding (last, lookup)
import Elaine.Pretty (pretty)
import Control.Monad ((>=>))

last :: [a] -> Maybe a
last = foldl (\_ x -> Just x) Nothing

data Command
  = Parse
  | Check
  | Run

data Error
  = ParseError String
  | TypeError String
  | EvalError String
  deriving (Eq)

instance Show Error where
  show (ParseError x) = "Parse error:\n" ++ x
  show (TypeError x) = "Type error:\n" ++ x
  show (EvalError x) = "Eval error:\n" ++ x

isParseError :: Result a -> Bool
isParseError (Left (ParseError _)) = True
isParseError _ = False

isTypeError :: Result a -> Bool
isTypeError (Left (TypeError _)) = True
isTypeError _ = False

isEvalError :: Result a -> Bool
isEvalError (Left (ParseError _)) = True
isEvalError _ = False

type Result a = Either Error a

eval' :: Program -> Result Value
eval' = first EvalError . eval

read' :: Text -> IO (Text, Text)
read' filename = do
  contents <- readFile (unpack filename)
  return (filename, pack contents)

parse' :: (Text, Text) -> Result Program
parse' = first ParseError . parseProgram

typeCheck' :: Program -> Result Program
typeCheck' x = case typeCheck x of
  Left a -> Left $ TypeError a
  Right _ -> Right x

transform' :: Program -> Result Program
transform' = Right . elabTrans

pretty' :: Program -> Result Text
pretty' = Right . pack . pretty

show' :: Show a => a -> Result String
show' = Right . show

execParse :: (Text, Text) -> Either Error Program
execParse = parse'

execCheck :: (Text, Text) -> Either Error ValueType
execCheck = parse' >=> \x -> case typeCheck x of
  Left a -> Left $ TypeError a
  Right env -> Right $ getMain env

execRun :: (Text, Text) -> Either Error Value
execRun = parse' >=> typeCheck' >=> eval'

cmd :: String -> (Text, Text) -> Either Error String
cmd "parse" = execParse >=> show'
cmd "check" = execCheck >=> show'
cmd "run" = execRun >=> show'
cmd _ = error "unrecognized command"

pack' :: (String, String) -> (Text, Text)
pack' (a, b) = (pack a, pack b)

exec :: String -> String -> IO ()
exec command filename = do
  x <- read' (pack filename)
  case cmd command x of
    Left a -> print a
    Right a -> print a
    