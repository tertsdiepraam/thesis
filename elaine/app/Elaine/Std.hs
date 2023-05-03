{-# LANGUAGE LambdaCase #-}

module Elaine.Std (stdEnv) where

import Data.Map (Map, fromList)

import Elaine.AST
import Elaine.Eval (Env, envBindings, newEnv)

newBuiltIn :: Ident -> ([Value] -> Maybe Value) -> BuiltIn
newBuiltIn name f = BuiltIn name $ \x -> case f x of
  Just a -> a
  Nothing -> error ("incorrect arguments for <" ++ name ++ ">")

stdEnv :: Env
stdEnv = newEnv { envBindings = bindings }

bindings :: Map Ident Value
bindings =
  fromList $
    map
      (\b@(BuiltIn x _) -> (x, Constant b))
      [ bAdd,
        bSub,
        bNeg,
        bMul,
        bDiv,
        bMod,
        bPow,
        bEq,
        bNeq,
        bGt,
        bLt,
        bGeq,
        bLeq,
        bNot,
        bAnd,
        bOr,
        bConcat,
        bShow
      ]

intBinOp :: (Int -> Int -> Int) -> [Value] -> Maybe Value
intBinOp op = \case
  [Int x, Int y] -> Just $ Int $ op x y
  _ -> Nothing

intCmp :: (Int -> Int -> Bool) -> [Value] -> Maybe Value
intCmp op = \case
  [Int x, Int y] -> Just $ Bool $ op x y
  _ -> Nothing

bAdd :: BuiltIn
bAdd = newBuiltIn "add" $ intBinOp (+)

bSub :: BuiltIn
bSub = newBuiltIn "sub" $ intBinOp (-)

bNeg :: BuiltIn
bNeg = newBuiltIn "neg" $ \case
  [Int x] -> Just $ Int $ - x
  _ -> Nothing

bMul :: BuiltIn
bMul = newBuiltIn "mul" $ intBinOp (*)

bDiv :: BuiltIn
bDiv = newBuiltIn "div" $ intBinOp div

bMod :: BuiltIn
bMod = newBuiltIn "mod" $ intBinOp mod

bPow :: BuiltIn
bPow = newBuiltIn "pow" $ intBinOp (^)

bEq :: BuiltIn
bEq = newBuiltIn "eq" $ \case
  [Int x, Int y] -> Just $ Bool $ x == y
  [Bool x, Bool y] -> Just $ Bool $ x == y
  [String x, String y] -> Just $ Bool $ x == y
  _ -> Nothing

bNeq :: BuiltIn
bNeq = newBuiltIn "neq" $ \case
  [Int x, Int y] -> Just $ Bool $ x /= y
  [Bool x, Bool y] -> Just $ Bool $ x /= y
  [String x, String y] -> Just $ Bool $ x /= y
  _ -> Nothing

bGt :: BuiltIn
bGt = newBuiltIn "gt" $ intCmp (>)

bLt :: BuiltIn
bLt = newBuiltIn "lt" $ intCmp (<)

bGeq :: BuiltIn
bGeq = newBuiltIn "geq" $ intCmp (>=)

bLeq :: BuiltIn
bLeq = newBuiltIn "leq" $ intCmp (<=)

bNot :: BuiltIn
bNot = newBuiltIn "not" $ \case
  [Bool x] -> Just $ Bool $ not x
  _ -> Nothing

bAnd :: BuiltIn
bAnd = newBuiltIn "and" $ \case
  [Bool x, Bool y] -> Just $ Bool $ x && y
  _ -> Nothing

bOr :: BuiltIn
bOr = newBuiltIn "or" $ \case
  [Bool x, Bool y] -> Just $ Bool $ x || y
  _ -> Nothing

bConcat :: BuiltIn
bConcat = newBuiltIn "concat" $ \case
  [String x, String y] -> Just $ String $ x ++ y
  _ -> Nothing

bShow :: BuiltIn
bShow = newBuiltIn "show" $ \case
  [Int x] -> Just $ String $ show x
  [Bool x] -> Just $ String $ show x
  [String x] -> Just $ String $ show x
  _ -> Nothing