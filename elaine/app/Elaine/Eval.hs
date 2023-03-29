{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Avoid lambda using `infix`" #-}

module Elaine.Eval (eval0) where

import Control.Applicative ((<|>))
import Data.List (isSuffixOf)
import Data.Maybe (fromJust, fromMaybe, isJust)
import Elaine.AST
import Prelude hiding (exp)

-- The decomposition is a list of functions that plug an expression into
-- another expression. Composing them gives back the original expression
type Decomposition = [Expr -> Expr]

-- We have a lot of this combination and usually treat them in tandem.
type State = (Expr, Decomposition)

-- We treat a context fairly abstractly, essentially, it is a function that
-- can generate a decomposition.
type Ctx = Expr -> Maybe (Expr, Expr -> Expr)

-- We have 3 main operations:
--  - Applying a reduction
--  - Stepping into an expression, pushing it to the context
--  - Stepping out of an expression, popping it from the context

-- Reduce an expression if possible
--
-- The reductions are not recursive. Instead they only check the outermost
-- expression. However, they may require the operands to be values.
reduce :: Expr -> Maybe Expr
reduce = \case
  Fn (Function params _ e) -> Just $ Val $ Lam (map fst params) e
  If (Val v) e1 e2 -> Just $ case v of
    Bool b -> if b then e1 else e2
    _ -> error "Invalid condition for if expression"
  App (Val v) args | all isVal args ->
    Just $ case v of
      Lam params body -> subst (zip params args) body
      _ -> error "Tried to call a non-function"
  Let x (Val v) e -> Just $ subst [(x, Val v)] e
  Handle (Val v) e ->
    let h = case v of
          Hdl h' -> h'
          _ -> error "First argument to handle must be a handler"
     in reduceHandler h e
  Elab (Val v) -> Just $ Val v
  _ -> Nothing

reduceState :: State -> Maybe State
reduceState (e, c) = case reduce e of
  Just e' -> Just (e', c)
  Nothing -> Nothing

-- Step into an expression yielding a new state with a
-- new expression and context.
decompose1 :: Ctx -> State -> Maybe State
decompose1 ctx (e, decomp) = do
  (e', d') <- ctx e
  return (e', d' : decomp)

-- Decompose as much as possible
-- Beware if implemented poorly this right go beyond reductions
decompose :: Ctx -> Expr -> State
decompose ctx s = f (s, [])
  where
    f s' = maybe s' f (decompose1 ctx s')

ctxCommon :: Ctx
ctxCommon (If e1 e2 e3) = Just (e1, \x -> If x e2 e3)
ctxCommon (App (Val v) args) = case span isVal args of
  (vals, e : es) -> Just (e, \x -> App (Val v) (vals ++ [x] ++ es))
  (_, []) -> error "ICE at function application should have been reduced"
ctxCommon (App e args) = Just (e, \x -> App x args)
ctxCommon (Let var e1 e2) = Just (e1, \x -> Let var x e2)
ctxCommon _ = Nothing

ctxE :: Ctx
ctxE exp = ctxCommon exp <|> ctxE' exp
  where
    ctxE' = \case
      Handle (Val h) e' -> Just (e', Handle (Val h))
      Handle e1 e2 -> Just (e1, \x -> Handle x e2)
      Elab e' -> Just (e', Elab)
      _ -> Nothing

ctxX :: Ident -> Ctx
ctxX op exp = ctxCommon exp <|> ctxX' exp
  where
    ctxX' = \case
      Handle (Val (Hdl h)) e ->
        if not (op `isOpIn` h)
          then Just (e, Handle $ Val $ Hdl h)
          else Nothing
      Handle e1 e2 -> Just (e1, \x -> Handle x e2)
      Elab e -> if isAlgebraic op then Just (e, Elab) else Nothing
      _ -> Nothing

-- Step out of the current expression, by popping the head of the context
-- and applying the current expression. This is the inverse of decompose.
compose1 :: State -> State
compose1 (_, []) = error "ICE at composition"
compose1 (e, c : cs) = (c e, cs)

-- Compose everything back into an expression
compose :: State -> Expr
compose (e, []) = e
compose (e, c : cs) = compose (c e, cs)

-- First version of evaluation functions using the functions above.
-- The idea here is simple:
--  - If we have a value, stop
--  - If not decompose until we have something to reduce and compose again
--  - Repeat
-- Obviously this is not very efficient, because we traverse the AST much more
-- than we need to, but that's OK. Because we have the "atomic" operations above,
-- we can define more complex schemes if necessary.
step0 :: State -> State
step0 s = fromMaybe ((compose1 . step0 . fromJust . decompose1 ctxE) s) (reduceState s)

eval0 :: Expr -> Value
eval0 (Val v) = v
eval0 e = eval0 . fst $ step0 (e, [])

-------------------------
-- Helper functions
-------------------------

isAlgebraic :: Ident -> Bool
isAlgebraic x = "!" `isSuffixOf` x

isOpIn :: Ident -> Handler -> Bool
isOpIn x h = x `elem` map opName (ops h)

isVal :: Expr -> Bool
isVal = isJust . toVal

toVal :: Expr -> Maybe Value
toVal (Val v) = Just v
toVal _ = Nothing

-- Substitution of multiple variables at the same time
subst :: [(String, Expr)] -> Expr -> Expr
subst subs e = foldl (flip subst1) e subs

subst1 :: (String, Expr) -> Expr -> Expr
subst1 (x, new) = \case
  Var y -> if x == y then new else Var y
  App e es -> App (f e) (map f es)
  If e1 e2 e3 -> If (f e1) (f e2) (f e3)
  Handle e1 e2 -> Handle (f e1) (f e2)
  Elab e -> Elab (f e)
  Let y e1 e2 -> if x == y then Let x (f e1) e2 else Let x (f e1) (f e2)
  Val (Lam params body) ->
    if x `elem` params
      then Val $ Lam params body
      else Val $ Lam params $ f body
  Val y -> Val y
  _ -> error "Whoops"
  where
    f = subst1 (x, new)

ops :: Handler -> [OperationClause]
ops (Handler _ clauses) = clauses

opName :: OperationClause -> Ident
opName (OperationClause x _ _) = x

-- decompose to find an operation and then we can reduce
reduceHandler :: Handler -> Expr -> Maybe Expr
reduceHandler h e = case e of
  Val v' -> Just $ reduceRet h v'
  _ -> applyOps (ops h)
  where
    reduceRet (Handler (HandleReturn x body) _) v = subst [(x, Val v)] body

    applyOps [] = Nothing
    applyOps (o : os) = case applyOp o of
      Just e' -> Just e'
      Nothing -> applyOps os

    applyOp (OperationClause op params body) = case decompose (ctxX op) e of
      (App (Var x) args, cs) ->
        if x == op
          then Just $ subst (zip params args ++ cont) body
          else Nothing
        where
          -- We could be more careful with the name of "y", but I think it's fine
          -- Because we don't want the handler body to affect it anyway, so if they
          -- use y it works and the function will have to be applied before we can
          -- do anything with it.

          k = Val $ Lam ["y"] $ Handle (Val $ Hdl h) $ compose (Var "y", cs)
          cont = [("resume", k)]
      _ -> Nothing

---------------------
-- Tests
---------------------