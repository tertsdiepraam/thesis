{-# LANGUAGE LambdaCase #-}

module Elaine.ElabTransform where

import Data.List (isSuffixOf)
import Elaine.AST
import Elaine.Eval (subst)

elabTrans :: Program -> Program
elabTrans = map elabTransDec

elabTransDec :: Declaration -> Declaration
elabTransDec (Declaration vis decType) = Declaration vis $ case decType of
  DecLet x t e -> DecLet x t (elabTransExpr e)
  Module x decs -> Module x (elabTrans decs)
  x -> x

-- TODO define some generic fold over the syntax tree to make this easier
elabTransExpr :: Expr -> Expr
elabTransExpr = \case
  -- This is all we're actually changing:
  Elab e1 e2 -> Handle (f e1) (f e2)
  App (Var x) args
    | isHigherOrder x ->
        App (App (Var x) (map (Val . lam [] . f) args)) []
  Val (Elb (Elaboration _ _ clauses)) ->
    let mapping params = zip params (map (\p -> App (Var p) []) params)
        f' (OperationClause x params e) = OperationClause x params (App (Var "resume") [Val $ lam [] (subst (mapping params) e)])
        clauses' = map f' clauses
     in Val $
          Hdl $
            Handler
              (HandleReturn "x" (Var "x"))
              clauses'
  -- This is just the rest of the fold
  App e1 e2 -> App (f e1) (map f e2)
  If e1 e2 e3 -> If (f e1) (f e2) (f e3)
  Handle e1 e2 -> Handle (f e1) (f e2)
  Match e1 arms -> Match (f e1) (map (\(MatchArm p e) -> MatchArm p (f e)) arms)
  Var x -> Var x
  Let x t e1 e2 -> Let x t (f e1) (f e2)
  Val v -> Val $ elabTransVal v
  _ -> error "Invalid expression"
  where
    f = elabTransExpr

elabTransVal :: Value -> Value
elabTransVal = \case
  Fn (Function params ret body) ->
    Fn $ Function params ret (elabTransExpr body)
  Hdl (Handler (HandleReturn xRet eRet) clauses) ->
    Hdl $
      Handler
        (HandleReturn xRet (elabTransExpr eRet))
        (transClauses clauses)
  Elb (Elaboration from to clauses) ->
    Elb $
      Elaboration from to (transClauses clauses)
  x -> x
  where
    transClauses = map (\(OperationClause x params e) -> OperationClause x params (elabTransExpr e))

isAlgebraic :: Ident -> Bool
isAlgebraic = not . isHigherOrder

isHigherOrder :: Ident -> Bool
isHigherOrder x = "!" `isSuffixOf` x