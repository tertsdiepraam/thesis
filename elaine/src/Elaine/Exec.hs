module Elaine.Exec where

import Data.Bifunctor (first)
import Data.Text (Text, pack, unpack)
import qualified Data.Text.Lazy as L
import Elaine.AST (Program, Value)
import Elaine.Transform (elabToHandle, makeElabExplicit)
import Elaine.Eval (eval)
import Elaine.Parse (parseProgram, Spans)
import Elaine.TypeCheck (typeCheck, getMain, CheckState (stateElabs))
import Prelude hiding (last, lookup)
import Elaine.Pretty (pretty)
import Control.Monad ((>=>))
import Text.Pretty.Simple (pShow)
import Elaine.Types (CompType, TypeScheme (TypeScheme))
import Data.Map (Map)

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

parse' :: (Text, Text) -> Result (Program, Spans)
parse' a = first ParseError $ parseProgram a

parseNoSpans :: (Text, Text) -> Result Program
parseNoSpans a = case parse' a of
  Left b -> Left b
  Right b -> Right $ fst b

parseSpans :: (Text, Text) -> Result Spans
parseSpans a = case parse' a of
  Left b -> Left b
  Right b -> Right $ snd b

typeCheck' :: Program -> Result (Program, Map Int [String])
typeCheck' x = case typeCheck x of
  Left a -> Left $ TypeError a
  Right (_, state) -> Right (x, stateElabs state)

makeElabExplicit' :: (Program, Map Int [String]) -> Result Program
makeElabExplicit' (p, m) = Right $ makeElabExplicit m p

transform' :: Program -> Result Program
transform' = Right . elabToHandle

pretty' :: Program -> Result String
pretty' = Right . pretty

show' :: Show a => a -> Result String
show' = Right . show

pShow' :: Show a => a -> Result String
pShow' = Right . L.unpack . pShow

execParse :: (Text, Text) -> Either Error Program
execParse = parseNoSpans

execSpans :: (Text, Text) -> Either Error Spans
execSpans = parseSpans

execCheck :: (Text, Text) -> Either Error CompType
execCheck = parseNoSpans >=> \x -> case typeCheck x of
  Left a -> Left $ TypeError a
  Right (env, _) -> case getMain env of
    TypeScheme _ _ t -> Right t

execExplicit :: (Text, Text) -> Either Error String
execExplicit = parseNoSpans >=> typeCheck' >=> makeElabExplicit' >=> pretty'

execRun :: (Text, Text) -> Either Error Value
execRun = parseNoSpans >=> typeCheck' >=> makeElabExplicit' >=> eval'

cmd :: String -> (Text, Text) -> Either Error String
cmd "parse" = execParse >=> show'
cmd "spans" = execSpans >=> pShow'
cmd "check" = execCheck >=> show'
cmd "run" = execRun >=> show'
cmd "explicit" = execExplicit
cmd _ = error "unrecognized command"

pack' :: (String, String) -> (Text, Text)
pack' (a, b) = (pack a, pack b)

exec :: String -> String -> IO ()
exec command filename = do
  x <- read' (pack filename)
  case cmd command x of
    Left a -> print a
    Right a -> putStrLn a
