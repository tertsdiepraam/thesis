{-# LANGUAGE LambdaCase #-}

module Elaine.Desugar (desugar) where

import Elaine.AST

desugar :: Program -> Program
desugar = map desugarMod

desugarMod :: Module -> Module
desugarMod (Mod x decs) = Mod x (map desugarDec decs)

desugarDec :: Declaration -> Declaration
desugarDec (Declaration vis decType) = Declaration vis $ case decType of
  DecLet x e -> DecLet x (desugarExpr e)
  x -> x

desugarExpr :: Expr -> Expr
desugarExpr = \case
  App e1 e2 -> App (f e1) (map f e2)
  If e1 e2 e3 -> If (f e1) (f e2) (f e3)
  Fn (Function params _ e) -> Val $ Lam (map fst params) (f e)
  Handle e1 e2 -> Handle (f e1) (f e2)
  Match e1 arms -> Match (f e1) (map (\(MatchArm p e) -> MatchArm p (f e)) arms)
  Elab e1 e2 -> Elab (f e1) (f e2)
  Var x -> Var x
  Let x e1 e2 -> Let x (f e1) (f e2)
  Val v -> Val $ desugarVal v
  _ -> error "Invalid expression"
  where
    f = desugarExpr

desugarVal :: Value -> Value
desugarVal = \case
  Lam xs e -> Lam xs (desugarExpr e)
  Hdl (Handler (HandleReturn xRet eRet) clauses) ->
    Hdl $
      Handler
        (HandleReturn xRet (desugarExpr eRet))
        (desugarClauses clauses)
  Elb (Elaboration from to clauses) ->
    Elb $
      Elaboration from to (desugarClauses clauses)
  x -> x
  where
    desugarClauses = map (\(OperationClause x params e) -> OperationClause x params (desugarExpr e))