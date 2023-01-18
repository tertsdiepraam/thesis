{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleInstances #-}
module Effen where

import Data.Set (Set)
import qualified Data.Map as Map

data ValT
  = BoolT
  | FunT ValT CompT
  | HanT CompT CompT

type Op = String

data CompT = CompT {
  val :: ValT,
  effects :: Set Op
}

data Val
  = Var String
  | Unit
  | StringV String
  | IntV Int
  | BoolV Bool
  | Pair Val Val
  | Fun String Comp
  | Han Handler
  | BuiltInV BuiltIn
  deriving (Show, Eq)

class IntoVal a where
  intoVal :: a -> Val

instance IntoVal Val where
  intoVal = id

instance IntoVal Bool where 
  intoVal = BoolV

instance IntoVal Int where
  intoVal = IntV

instance IntoVal [Char] where
  intoVal = StringV

instance IntoVal () where
  intoVal () = Unit

data Handler = Handler {
  ret :: (String, Comp),
  ops :: Map.Map Op OpHandle
}
  deriving (Show, Eq)

data OpHandle = OpHandle {
  var :: String,
  cont :: String,
  comp :: Comp
}
  deriving (Show, Eq)

data Comp
  = Return Val
  | Op Op Val String Comp
  | Do String Comp Comp
  | If Val Comp Comp
  | App Val Val
  | Handle Val Comp
  deriving (Show, Eq)

data BuiltIn = BuiltIn {
  biName :: String,
  biComp :: Val -> Comp
}

instance Show BuiltIn where
  show = show . biName

instance Eq BuiltIn where
  a == b = biName a == biName b

unaryBuiltIn :: String -> (Val -> Comp) -> Val
unaryBuiltIn name comp = BuiltInV $ BuiltIn name comp

binaryBuiltIn :: String -> (Val -> Val -> Comp) -> Val
binaryBuiltIn name comp = BuiltInV $ BuiltIn name $ \x -> Return $ BuiltInV $ BuiltIn (name ++ "'") $ comp x

not' :: Val
not' = BuiltInV $ BuiltIn "not" $ \case
  BoolV x -> Return $ BoolV (not x)
  x -> error $ "Called not on " ++ show x

plus' :: Val
plus' = binaryBuiltIn "plus" $ \x y -> case (x, y) of
  (IntV a, IntV b) -> Return $ IntV (a + b)
  x -> error $ "Called plus on " ++ show x

concat' :: Val
concat' = binaryBuiltIn "concat" $ \x y -> case (x, y) of
  (StringV a, StringV b) -> Return $ StringV (a ++ b)
  x -> error $ "Called concat on " ++ show x

fst' :: Val
fst' = unaryBuiltIn "fst" $ \case
  Pair a _ -> Return a
  x -> error $ "Called fst on " ++ show x

snd' :: Val
snd' = unaryBuiltIn "snd" $ \case
  Pair _ b -> Return b
  x -> error $ "Called snd on " ++ show x

op' :: IntoVal a => String -> a -> Comp
op' name v = Op name (intoVal v) "_" (Return (Var "_"))

return' :: IntoVal a => a -> Comp
return' = Return . intoVal

app' :: (IntoVal a, IntoVal b) => a -> b -> Comp
app' a b = App (intoVal a) (intoVal b)

-- Small-step semantics
-- Just means that the computation was updated
-- Nothing means that the computation is stuck
step :: Comp -> Maybe Comp

step (Do x (Return v) c2)
  = Just $ substitute1 x v c2

step (Do x c1 c2)
  | Just c1' <- step c1
  = Just $ Do x c1' c2

step (Do x (Op op v y c1) c2)
  = Just $ Op op v y (Do x c1 c2)

step (If (BoolV True) c _) = Just c
step (If (BoolV False) _ c) = Just c

step (App (Fun x c) v) = Just $ substitute1 x v c
step (App (BuiltInV bi) v) = Just $ biComp bi v

step (Handle h c)
  | Just c' <- step c
  = Just $ Handle h c'

step (Handle (Han h) (Return v)) = Just $ substitute1 x v c
  where (x, c) = ret h

step (Handle (Han h) (Op op v y c))
  | Just (OpHandle x k c_op) <- Map.lookup op (ops h) 
  = Just $ substitute [(x, v), (k, Fun y (Handle (Han h) c))] c_op

step x = Nothing

-- Step until stuck
multistep :: (a -> Maybe a) -> a -> a
multistep step c = maybe c (multistep step) (step c)

multistep' :: Show a => (a -> Maybe a) -> a -> IO a
multistep' step c = case step c of
  Just new_c -> do
    putStrLn ("Step: " ++ show new_c)
    multistep' step new_c
  Nothing -> pure c

-- Evaluate and get the final value (might fail)
eval :: Comp -> Val
eval c = case multistep step c of
  Return v -> v
  c -> error ("Evaluation got stuck at " ++ show c)

eval' :: Comp -> IO Val
eval' c = do
  steps <- multistep' step c
  case steps of
    Return v -> pure v
    c -> error ("Evaluation got stuck at " ++ show c)

-- Type check
-- type Context = Map.Map String ValT

-- typeV :: Context -> Val -> ValT
-- typeV c TT = BoolT
-- typeV c FF = BoolT
-- typeV c (Var x) = case Map.lookup x c of
--   Just t -> t
--   Nothing -> error ("No type for variable " ++ show x)
-- typeV c (Fun x c) = _

-- Utils

-- Variable substitution:
-- TODO: Make sure that variable shadowing doesn't mess things up
substitute :: [(String, Val)] -> Comp -> Comp
substitute subs c = foldr (\(x,v) c -> substitute1 x v c) c subs

substitute1 :: String -> Val -> Comp -> Comp
substitute1 x v = \case
  Return v'    -> Return (subV v')
  Op s1 v s2 c -> Op s1 (subV v) s2 (subC c)
  Do s c1 c2   ->
    -- We want to allow shadowing
    Do s (subC c1) (if s == x then c2 else subC c2)
  If v' c1 c2  -> If (subV v') (subC c1) (subC c2)
  App v1 v2    -> App (subV v1) (subV v2)
  Handle v' c   -> Handle (subV v') (subC c)
  where 
    subV = substituteV x v
    subC = substitute1 x v

substituteV :: String -> Val -> Val -> Val
substituteV x v (Fun s c) = 
  -- We want to allow shadowing
  if x == s then Fun s c else Fun s (substitute1 x v c)
substituteV x v (Var s) | x == s = v
substituteV x v (Pair a b) = Pair (substituteV x v a) (substituteV x v b)
substituteV _ _ v = v

