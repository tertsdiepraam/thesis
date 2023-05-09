{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}

module Elaine.TypeCheck where

import Control.Lens (Lens', over, view, (^.), set)
import Control.Lens.TH (makeLenses)
import Control.Monad.Except (ExceptT, MonadError (throwError), runExceptT)
import Control.Monad.State
  ( MonadState (get, put),
    State,
    evalState,
    (<=<),
  )
import Data.Bifunctor (second)
import Data.Foldable (foldlM)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import Elaine.AST
import Elaine.Std (stdTypes)
import Control.Monad (forM_)

-- Variable names to type
data TypeEnv = TypeEnv
  { _vars :: Map String ValueType,
    _mods :: Map String TypeEnv
  }
  deriving (Show)

makeLenses ''TypeEnv

-- A version of lookup/! that uses the Infer monad in case of an error
get' :: Map String b -> String -> Infer b
get' m a = case Map.lookup a m of
  Just b -> return b
  Nothing -> throwError $ "undefined identifier: " ++ a

union :: TypeEnv -> TypeEnv -> TypeEnv
union a = unionLens vars a . unionLens mods a
  where
    unionLens :: Lens' TypeEnv (Map String b) -> TypeEnv -> TypeEnv -> TypeEnv
    unionLens l a' = over l (Map.union $ a' ^. l)

getVar :: TypeEnv -> String -> Infer ValueType
getVar = get' . view vars

extendVars :: TypeEnv -> [(String, ValueType)] -> TypeEnv
extendVars env newVars = over vars (Map.union $ Map.fromList newVars) env

getMod :: TypeEnv -> String -> Infer TypeEnv
getMod = get' . view mods

insertVar :: String -> ValueType -> TypeEnv -> TypeEnv
insertVar k v = over vars $ Map.insert k v

insertMod :: String -> TypeEnv -> TypeEnv -> TypeEnv
insertMod k v = over mods $ Map.insert k v

singletonVar :: String -> ValueType -> TypeEnv
singletonVar k v = insertVar k v empty

singletonMod :: String -> TypeEnv -> TypeEnv
singletonMod k v = insertMod k v empty

empty :: TypeEnv
empty =
  TypeEnv
    { _vars = Map.empty,
      _mods = Map.empty
    }

fresh :: Infer ValueType
fresh = do
  (i, subs) <- get
  put (i + 1, subs)
  return $ TypeVar i

subM :: Substitutable a => a -> Infer a
subM a = do
  (_, subs) <- get
  return $ sub subs a

addSub :: ValueType -> ValueType -> Infer ()
addSub k v = do
  (i, Substitutions subs) <- get
  put (i, Substitutions $ Map.insert k v subs)

runInfer :: Infer a -> Either String a
runInfer a = evalState (runExceptT a) (0, Substitutions Map.empty)

typeCheck :: [Declaration] -> Either String TypeEnv
typeCheck decs = second fst $ runInfer (typeCheckMod initialEnv decs)
  where stdTypeEnv = set vars stdTypes empty
        initialEnv = singletonMod "std" stdTypeEnv

getMain :: TypeEnv -> ValueType
getMain = flip (Map.!) "main" . view vars

-- Returns both the private and public TypeEnv
typeCheckMod :: TypeEnv -> [Declaration] -> Infer (TypeEnv, TypeEnv)
typeCheckMod env = foldlM f (env, empty)
  where
    f (private, public) dec = do
      (private', public') <- typeCheckDec private dec
      return (private `union` private', public `union` public')

typeCheckDec :: TypeEnv -> Declaration -> Infer (TypeEnv, TypeEnv)
typeCheckDec env (Declaration vis dec') = do
  res <- typeCheckDec' env dec'
  return $ if vis == Public then (res, res) else (res, empty)

typeCheckDec' :: TypeEnv -> DeclarationType -> Infer TypeEnv
typeCheckDec' env = \case
  Use x -> getMod env x
  Module x decs -> do
    modEnv <- typeCheckMod env decs
    return $ singletonMod x (snd modEnv)
  DecType _ _ -> throwError "Not implemented"
  DecEffect _ _ -> throwError "Not implemented"
  DecLet x mt expr -> do
    tExpr <- infer env expr
    () <- forM_ mt (unify tExpr)
    return $ singletonVar x tExpr

-- TypeVar to Type
newtype Substitutions = Substitutions (Map ValueType ValueType)

class Substitutable a where
  sub :: Substitutions -> a -> a

instance Substitutable ValueType where
  sub (Substitutions subs) vt = fromMaybe vt $ Map.lookup vt subs

instance Substitutable TypeEnv where
  sub subs = over vars (Map.map $ sub subs) . over mods (Map.map $ sub subs)

type Infer = ExceptT String (State (Int, Substitutions))

unify :: ValueType -> ValueType -> Infer ()
unify a b = do
  a' <- subM a
  b' <- subM b
  unify' a' b'
  where
    unify' a' b' | a' == b' = return ()
    unify' v@(TypeVar _) t = addSub v t
    unify' t v@(TypeVar _) = addSub v t
    unify' (TypeArrow args1 ret1) (TypeArrow args2 ret2) = do
      () <- mapM_ (uncurry unify) (zip args1 args2)
      unify ret1 ret2
    unify' _ _ = throwError "Failed to unify: type error"

class Inferable a where
  infer :: TypeEnv -> a -> Infer ValueType

-- Algorithm W
instance Inferable Expr where
  infer env =
    subM <=< \case
      Val v -> infer env v
      Var x -> getVar env x
      If e1 e2 e3 -> do
        t1 <- infer env e1
        t2 <- infer env e2
        t3 <- infer env e3
        () <- unify t1 TypeBool
        () <- unify t3 t2
        return t3
      App f args -> do
        tf <- infer env f
        tArgs <- inferMany env args
        tRet <- fresh
        () <- unify tf (TypeArrow tArgs tRet)
        return tRet
      Let x mt e1 e2 -> do
        t1 <- infer env e1
        () <- forM_ mt (unify t1)
        infer (insertVar x t1 env) e2
      x -> error $ "Not implemented: " ++ show x

extractVal :: ComputationType -> ValueType
extractVal (ComputationType v _) = v

typeOrFresh :: Maybe ValueType -> Infer ValueType
typeOrFresh Nothing = fresh
typeOrFresh (Just t) = return t

-- inferExpr env (Fn (Function args _ body)) = do
--   tArgs <- map (const fresh) args
--   tRet <- fresh

--   return (a, b)

inferMany :: Inferable a => TypeEnv -> [a] -> Infer [ValueType]
inferMany env = mapM (infer env)

instance Inferable Value where
  infer _ (Int _) = return TypeInt
  infer _ (String _) = return TypeString
  infer _ (Bool _) = return TypeBool
  infer _ Unit = return TypeUnit
  infer env (Fn (Function args tRet body)) = do
    let args' = map fst args
    tArgs <- mapM (typeOrFresh . fmap extractVal . snd ) args
    tRetInferred <- infer (extendVars env $ zip args' tArgs) body
    () <- forM_ (fmap extractVal tRet) (unify tRetInferred)
    tArgs' <- mapM subM tArgs
    tRet' <- subM tRetInferred
    return $ TypeArrow tArgs' tRet'
  infer _ _ = error "Not implemented yet"