{-# LANGUAGE LambdaCase #-}

module Elaine.Std where

import Data.Map (Map, fromList)

import Elaine.AST
import Elaine.Eval (Env, envBindings, newEnv)

newBuiltIn :: Ident -> ([Value] -> Value) -> Expr
newBuiltIn name = Val . Constant . BuiltIn name

stdEnv :: Env
stdEnv = (newEnv "std") { envBindings = bindings }

bindings :: Map Ident Value
bindings =
  fromList $
    map
      (\b@(BuiltIn x _) -> (x, Constant b))
      [ bAdd,
        bSub,
        bMul,
        bDiv,
        bNot,
        bAnd,
        bOr,
        bConcat
      ]

bAdd :: BuiltIn
bAdd = BuiltIn "add" $ \case
  [Int x, Int y] -> Int $ x + y
  _ -> error "incorrect arguments for <add>"

bSub :: BuiltIn
bSub = BuiltIn "sub" $ \case
  [Int x, Int y] -> Int $ x - y
  _ -> error "incorrect arguments for <sub>"

bMul :: BuiltIn
bMul = BuiltIn "mul" $ \case
  [Int x, Int y] -> Int $ x * y
  _ -> error "incorrect arguments for <sub>"

bDiv :: BuiltIn
bDiv = BuiltIn "div" $ \case
  [Int x, Int y] -> Int $ x `div` y
  _ -> error "incorrect arguments for <div>"

bNot :: BuiltIn
bNot = BuiltIn "not" $ \case
  [Bool x] -> Bool $ Prelude.not x
  _ -> error "incorrect arguments for <not>"

bAnd :: BuiltIn
bAnd = BuiltIn "and" $ \case
  [Bool x, Bool y] -> Bool $ x && y
  _ -> error "incorrect arguments for <and>"

bOr :: BuiltIn
bOr = BuiltIn "and" $ \case
  [Bool x, Bool y] -> Bool $ x || y
  _ -> error "incorrect arguments for <and>"

bConcat :: BuiltIn
bConcat = BuiltIn "concat" $ \case
  [String x, String y] -> String $ x ++ y
  _ -> error "incorrect arguments for <concat>"
