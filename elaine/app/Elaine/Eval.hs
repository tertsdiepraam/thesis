{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Avoid lambda using `infix`" #-}

module Elaine.Eval (evalExpr, privateEnv, evalModule, envBindings, envModules, newEnv, Env, subst) where

import Control.Applicative ((<|>))
import Data.Bifunctor (second)
import Data.List (find)
import Data.Map (Map, assocs, empty, fromList, lookup, union, singleton)
import Data.Maybe (fromJust, fromMaybe, isJust)
import Elaine.AST
import Elaine.Pretty (pretty)
import Prelude hiding (exp, lookup)

-- The decomposition is a list of functions that plug an expression into
-- another expression. Composing them gives back the original expression
type Decomposition = [Expr -> Expr]

-- We have a lot of this combination and usually treat them in tandem.
type State = (Expr, Decomposition)

-- We treat a context fairly abstractly, essentially, it is a function that
-- can generate a decomposition.
type Ctx = Expr -> Maybe (Expr, Expr -> Expr)

-- This is the environment for the evaluation
data Env = Env
  { envBindings :: Map Ident Value,
    envModules :: Map Ident Env
  }
  deriving (Show)

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
  If (Val v) e1 e2 -> Just $ case v of
    Bool b -> if b then e1 else e2
    _ -> error "Invalid condition for if expression"
  App (Val v) args | all isVal args ->
    Just $ case v of
      Lam params body -> subst (zip params args) body
      Constant (BuiltIn _ body) -> Val $ body (map (fromJust . toVal) args)
      _ -> error ("Tried to call a non-function: " ++ pretty v)
  Let x (Val v) e -> Just $ subst [(x, Val v)] e
  Handle (Val v) e ->
    let h = case v of
          Hdl h' -> h'
          _ -> error "First argument to handle must be a handler"
        reduced = reduceHandler h e
     in reduced
  Elab (Val _) (Val v) -> Just $ Val v
  Elab (Val (Elb elab)) e -> Elab (Val $ Elb elab) <$> reduceElab elab e
  Match (Val v) arms ->
    -- so what do we want to do:
    --  - find the arm that matches the expression
    --  - that gives us two lists, which we then match
    --  - that yields new bindings for in the arm expression
    let (variant, args) =
          case v of
            Data _ i p -> (i, p)
            _ -> error "Can only match on custom data types"
        matchingArm = find (\(MatchArm (Pattern x _) _) -> x == variant) arms
        MatchArm (Pattern _ params) exp = case matchingArm of
          Just arm -> arm
          Nothing -> error "Expression did not match"
     in if length params == length args
          then Just $ subst (zip params args) exp
          else error "Number of arguments in pattern do not match expression"
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
-- Beware if implemented poorly this might go beyond reductions
decompose :: Ctx -> Expr -> State
decompose ctx s = f (s, [])
  where
    f s' = maybe s' f (decompose1 ctx s')

ctxCommon :: Ctx
ctxCommon (If e1 e2 e3) = Just (e1, \x -> If x e2 e3)
ctxCommon (App (Val v) args) = case span isVal args of
  (vals, e : es) -> Just (e, \x -> App (Val v) (vals ++ [x] ++ es))
  (_, []) -> Nothing
ctxCommon (App e args) = Just (e, \x -> App x args)
ctxCommon (Let var e1 e2) = Just (e1, \x -> Let var x e2)
ctxCommon (Match e arms) = case e of
  Val _ -> Nothing
  _ -> Just (e, \x -> Match x arms)
ctxCommon _ = Nothing

ctxE :: Ctx
ctxE exp = ctxCommon exp <|> ctxE' exp
  where
    ctxE' = \case
      Handle (Val h) e' -> Just (e', Handle $ Val h)
      Handle e1 e2 -> Just (e1, \x -> Handle x e2)
      Elab e1' e2' -> Just (e2', Elab e1')
      _ -> Nothing

ctxHandler :: Ident -> Ctx
ctxHandler op exp = ctxCommon exp <|> ctxHandler' exp
  where
    ctxHandler' = \case
      Handle (Val (Hdl h)) e ->
        if not (op `isOpIn` h)
          then Just (e, Handle $ Val $ Hdl h)
          else Nothing
      Handle e1 e2 -> Just (e1, \x -> Handle x e2)
      Elab (Val elab) e -> Just (e, Elab (Val elab))
      Elab e1 e2 -> Just (e1, \x -> Elab x e2)
      _ -> Nothing

-- The context for an elaboration can go into handles, but not other elabs.
-- This assumes that each elab elaborates **all** higher-order effects, which
-- should be verified by the type system.
ctxElab :: Ctx
ctxElab exp = ctxCommon exp <|> ctxElab' exp
  where
    ctxElab' = \case
      Handle (Val h) e -> Just (e, Handle $ Val h)
      Handle e1 e2 -> Just (e1, \x -> Handle x e2)
      Elab _ _ -> Nothing
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
step :: Env -> State -> State
step env s =
  -- trace ("====== Step ======\n" ++ pretty (compose s))
  fromMaybe ((compose1 . step env . f . decompose1 ctxE) s) (reduceState s)
  where
    f (Just a) = a
    f Nothing = error ("could not reduce or decompose: " ++ show (fst s) ++ "\n" ++ show (compose s))

evalExpr :: Env -> Expr -> Value
evalExpr _ (Val v) = v
evalExpr env e = evalExpr env . fst $ step env (e, [])

-------------------------
-- Helper functions
-------------------------

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
  Elab e1 e2 -> Elab (f e1) (f e2)
  Let y e1 e2 -> if x == y then Let y (f e1) e2 else Let y (f e1) (f e2)
  -- TODO prevent name shadowing in match arms
  Match e arms -> Match (f e) (map mapArms arms)
    where
      mapArms (MatchArm (Pattern y params) exp) = MatchArm (Pattern y params) (if x `elem` params then exp else f exp)
  fun@(Fn (Function params ret body)) ->
    if x `elem` map fst params
      then fun
      else Fn $ Function params ret (f body)
  Val (Lam params body) ->
    if x `elem` params
      then Val $ Lam params body
      else Val $ Lam params $ f body
  Val (Hdl (Handler (HandleReturn retVar retExpr) hClauses)) ->
    let ret' =
          if x == retVar
            then retExpr
            else f retExpr
        clauses' =
          map
            ( \c@(OperationClause name params body) ->
                if (x `elem` params) || (x == "resume")
                  then c
                  else OperationClause name params (f body)
            )
            hClauses
     in Val $ Hdl $ Handler (HandleReturn retVar ret') clauses'
  Val (Elb (Elaboration eff row elabClauses)) ->
    Val $
      Elb $
        Elaboration
          eff
          row
          ( map
              ( \c@(OperationClause name params body) ->
                  if x `elem` params
                    then c
                    else OperationClause name params (f body)
              )
              elabClauses
          )
  Val (Data y i args) -> Val $ Data y i (map f args)
  Val y -> Val y
  -- x' -> error ("Could not substitute" ++ show x')
  ImplicitElab _ -> error "Implicit elab should have been made explicit"
  where
    f = subst1 (x, new)

ops :: Handler -> [OperationClause]
ops (Handler _ c) = c

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

    applyOp (OperationClause op params body) = case decompose (ctxHandler op) e of
      (Var x, c : cs) | x == op ->
        -- We need to check the parent expression of the operation identifier
        -- because we need access to the arguments of the application
        case c (Var x) of
          App (Var _) args -> Just $ subst (zip params args ++ cont) body
          _ -> Nothing
        where
          -- We could be more careful with the name of "y", but I think it's fine
          -- Because we don't want the handler body to affect it anyway, so if they
          -- use y it works and the function will have to be applied before we can
          -- do anything with it.
          k = Val $ Lam ["y"] $ Handle (Val $ Hdl h) $ compose (Var "y", cs)
          cont = [("resume", k)]
      _ -> Nothing

reduceElab :: Elaboration -> Expr -> Maybe Expr
reduceElab elab e = case decompose ctxElab e of
  (Var x, c : cs) -> do
    let Elaboration _ _ clauses = elab
    OperationClause _ params body <- find (\c' -> clauseName c' == x) clauses
    case c (Var x) of
      App (Var _) args ->
        Just $ compose (subst (zip params (map (Elab (Val $ Elb elab)) args)) body, cs)
      _ -> Nothing
  _ -> Nothing

-- And environment represents a module
newEnv :: Env
newEnv =
  Env
    { envBindings = empty,
      envModules = empty
    }

-- Returns an environment that should be merged with the existing environment
updateEnv :: Env -> DeclarationType -> Env
-- Import adds the imported module to the current environment
updateEnv m (Use x) = case lookup x (envModules m) of
  Just imported -> imported
  Nothing -> error $ "Could not import module " ++ x
-- Type adds type constructors
updateEnv _ (DecType typeIdent decs) =
  let -- We create a function for every constructor returning a Data value
      -- The parameters are called param0, param1, param2, etc.
      lamParams params = map (\i -> "param" ++ show i) (take (length params) [0 ..] :: [Int])
      f (Constructor x params) = (x, Lam (lamParams params) (Val $ Data typeIdent x (map Var (lamParams params))))
      decBindings = fromList $ map f decs
   in newEnv {envBindings = decBindings}
-- Let adds a single binding
updateEnv env (DecLet x expr) =
  let bindings = assocs $ envBindings env
      exprBindings = map (second Val) bindings
      substituted = subst exprBindings expr
      value = evalExpr env substituted
   in newEnv {envBindings = singleton x value}
-- Effects mostly matter for type checking, not in evaluation, so skip 'em
updateEnv m (DecEffect _ _) = m
updateEnv m (Module x decs) =
  let xEnv = publicEnv $ evalModule m decs
   in newEnv {envModules = singleton x xEnv}

data EvalResult = EvalResult
  { publicEnv :: Env,
    privateEnv :: Env
  }

mergeEnv :: Env -> Env -> Env
mergeEnv a b =
  newEnv
    { envBindings = envBindings a `union` envBindings b,
      envModules = envModules a `union` envModules b
    }

evalModule :: Env -> [Declaration] -> EvalResult
evalModule env = foldl updateResult initialResult
  where
    initialResult = EvalResult {publicEnv = env, privateEnv = env}
    updateResult result dec =
      let Declaration vis decType = dec
          envDelta = updateEnv (privateEnv result) decType
       in EvalResult
            { publicEnv =
                if vis == Public
                  then mergeEnv (publicEnv result) envDelta
                  else publicEnv result,
              privateEnv = mergeEnv (privateEnv result) envDelta
            }