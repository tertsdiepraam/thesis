{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}

module Elaine.TypeCheck where

import Control.Lens (Lens', over, set, view, (^.))
import Control.Lens.TH (makeLenses)
import Control.Monad (forM_)
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
import Data.Set (Set)
import qualified Data.Set as Set
import Elaine.AST
import Elaine.Std (stdTypes)

type Infer = ExceptT String (State (Int, Substitutions))

-- TypeVar to Type
data Substitutions = Substitutions
  { subTypeVars :: Map ValueType ValueType,
    subEffectVars :: Map EffectRow EffectRow
  }

-- Variable names to type
data TypeEnv = TypeEnv
  { _vars :: Map String TypeScheme,
    _mods :: Map String TypeEnv,
    _bound :: Set TypeVar
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

getVar :: TypeEnv -> String -> Infer TypeScheme
getVar = get' . view vars

extendVars :: [(String, TypeScheme)] -> TypeEnv -> TypeEnv
extendVars newVars = over vars (Map.union $ Map.fromList newVars)

addTypeVars :: Set TypeVar -> TypeEnv -> TypeEnv
addTypeVars newBound = over bound (Set.union newBound)

getMod :: TypeEnv -> String -> Infer TypeEnv
getMod = get' . view mods

insertVar :: String -> TypeScheme -> TypeEnv -> TypeEnv
insertVar k v = over vars $ Map.insert k v

insertMod :: String -> TypeEnv -> TypeEnv -> TypeEnv
insertMod k v = over mods $ Map.insert k v

singletonVar :: String -> TypeScheme -> TypeEnv
singletonVar k v = insertVar k v empty

singletonMod :: String -> TypeEnv -> TypeEnv
singletonMod k v = insertMod k v empty

empty :: TypeEnv
empty =
  TypeEnv
    { _vars = Map.empty,
      _mods = Map.empty,
      _bound = Set.empty
    }

inst :: TypeScheme -> Infer ComputationType
inst (TypeScheme {typeVars, effectVars, typ}) = do
  freshTypeVars <- freshes typeVars
  let typeVarMap = Map.fromList $ zip (map TypeVar typeVars) (map TypeVar freshTypeVars)

  freshEffectVar <- freshes effectVars
  let effectVarMap = Map.fromList $ zip (map Extend effectVars) (map Extend freshEffectVar)
  return $
    sub
      ( Substitutions {subTypeVars = typeVarMap, subEffectVars = effectVarMap}
      )
      typ

gen :: TypeEnv -> ComputationType -> Infer TypeScheme
gen env type' = do
  let typeVars = Set.toList $ freeTypeVars env type'
  freshTypeVars <- freshes typeVars
  let typeVarMap = zip (map TypeVar typeVars) (map TypeVar freshTypeVars)

  let effectVars = Set.toList $ freeEffectVars env type'
  freshEffectVars <- freshes effectVars
  let effectVarMap = zip (map Extend effectVars) (map Extend freshEffectVars)

  let subs =
        Substitutions
          { subTypeVars = Map.fromList typeVarMap,
            subEffectVars = Map.fromList effectVarMap
          }

  let typ = sub subs type'
  return $
    TypeScheme
      { typeVars = freshTypeVars,
        effectVars = freshEffectVars,
        typ
      }

freshes :: [a] -> Infer [TypeVar]
freshes = mapM (const fresh)

freeTypeVars :: TypeEnv -> ComputationType -> Set TypeVar
freeTypeVars env t = allTypeVars t Set.\\ view bound env
  where
    allTypeVars (ComputationType _ typ) = allTypeVarsV typ

    allTypeVarsV (TypeVar v) = Set.singleton v
    allTypeVarsV (TypeArrow args ret) = Set.unions (map allTypeVars (args ++ [ret]))
    allTypeVarsV _ = Set.empty

freeEffectVars :: TypeEnv -> ComputationType -> Set TypeVar
freeEffectVars env t = allEffectVars t Set.\\ view bound env
  where
    allEffectVars (ComputationType row typ) =
      Set.union
        (allEffectVarsR row)
        (allEffectVarsV typ)

    allEffectVarsV (TypeArrow args ret) = Set.unions (map allEffectVars (args ++ [ret]))
    allEffectVarsV _ = Set.empty

    allEffectVarsR (Extend v) = Set.singleton v
    allEffectVarsR (Cons _ r) = allEffectVarsR r
    allEffectVarsR Empty = Set.empty

fresh :: Infer TypeVar
fresh = do
  (i, subs) <- get
  put (i + 1, subs)
  return $ ImplicitVar i

freshV :: Infer ValueType
freshV = TypeVar <$> fresh

freshR :: Infer EffectRow
freshR = Extend <$> fresh

freshC :: Infer ComputationType
freshC = do
  r <- freshR
  ComputationType r <$> freshV

subM :: Substitutable a => a -> Infer a
subM a = do
  (_, subs) <- get
  return $ sub subs a

emptySubs :: Substitutions
emptySubs = Substitutions Map.empty Map.empty

addTypeSub :: ValueType -> ValueType -> Infer ()
addTypeSub k v = do
  (i, subs) <- get
  put (i, subs {subTypeVars = Map.insert k v (subTypeVars subs)})

addEffectSub :: EffectRow -> EffectRow -> Infer ()
addEffectSub k v = do
  (i, subs) <- get
  put (i, subs {subEffectVars = Map.insert k v (subEffectVars subs)})

runInfer :: Infer a -> Either String a
runInfer a = evalState (runExceptT a) (0, emptySubs)

typeCheck :: [Declaration] -> Either String TypeEnv
typeCheck decs = runInfer $ do
  (env, _) <- typeCheckMod initialEnv decs

  -- We're forcing main to not have any effects
  -- Most of the type, it will be polymorphic over some effects, but
  -- we don't have any more handlers, to forcing to empty is ok.
  mainType <- inst $ view vars env Map.! "main"
  v <- freshV
  let newMainType = ComputationType Empty v
  () <- unify mainType newMainType
  mainType' <- subM mainType
  mainType'' <- gen env mainType'
  subM (insertVar "main" mainType'' env)
  where
    stdTypeEnv = set vars stdTypes empty
    initialEnv = singletonMod "std" stdTypeEnv

getMain :: TypeEnv -> TypeScheme
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
    tExpr' <- subM tExpr
    tExpr'' <- gen env tExpr'
    () <- forM_ mt (isInstanceOf tExpr')
    return $ singletonVar x tExpr''

class Substitutable a where
  sub :: Substitutions -> a -> a

instance Substitutable ValueType where
  sub subs vt = fromMaybe vt $ Map.lookup vt (subTypeVars subs)

instance Substitutable TypeEnv where
  sub subs = over vars (Map.map $ sub subs) . over mods (Map.map $ sub subs)

instance Substitutable TypeScheme where
  sub subs (TypeScheme v e t) = TypeScheme v e (sub subs t)

instance Substitutable ComputationType where
  sub subs (ComputationType row typ) = ComputationType (sub subs row) (sub subs typ)

instance Substitutable EffectRow where
  sub subs e = case Map.lookup e (subEffectVars subs) of
    Just e' -> e'
    Nothing -> case e of
      Cons eff row -> Cons eff (sub subs row)
      _ -> e

unify :: ComputationType -> ComputationType -> Infer ()
unify a b = do
  a' <- subM a
  b' <- subM b
  unify' a' b'
  where
    unify' (ComputationType rowA typA) (ComputationType rowB typB) = do
      () <- unifyR rowA rowB
      unifyV typA typB

    unifyR a' b' | a' == b' = return ()
    unifyR v@(Extend _) row = addEffectSub v row
    unifyR row v@(Extend _) = addEffectSub v row
    unifyR aRow@(Cons aEff aRest) bRow@(Cons bEff bRest)
      | Just b' <- removeEffect aEff bRow = unifyR aRest b'
      | Just a' <- removeEffect bEff aRow = unifyR a' bRest
    unifyR _ _ = throwError "failed to unify effect rows"
    
    removeEffect a' (Cons b' rest) | a' == b' = Just rest
    removeEffect a' (Cons b' rest) = do
      rest' <- removeEffect a' rest
      return $ Cons b' rest'
    removeEffect _ _ = Nothing

    unifyV a' b' | a' == b' = return ()
    unifyV v@(TypeVar _) t = addTypeSub v t
    unifyV t v@(TypeVar _) = addTypeSub v t
    unifyV (TypeArrow args1 ret1) (TypeArrow args2 ret2) = do
      () <- mapM_ (uncurry unify) (zip args1 args2)
      unify ret1 ret2
    unifyV _ _ = throwError "Failed to unify: type error"

class Inferable a where
  infer :: TypeEnv -> a -> Infer ComputationType

-- Algorithm W
instance Inferable Expr where
  infer env =
    subM <=< \case
      Val v -> infer env v
      Var x -> do
        t <- getVar env x
        inst t
      If e1 e2 e3 -> do
        t1 <- infer env e1
        t2 <- infer env e2
        t3 <- infer env e3
        row <- freshR
        () <- unify t1 (ComputationType row TypeBool)
        () <- unify t3 t2
        return t3
      App f args -> do
        tf <- infer env f
        tArgs <- inferMany env args
        tRet <- freshC
        row <- freshR
        () <- unify tf (ComputationType row (TypeArrow tArgs tRet))
        return tRet
      Let x mt e1 e2 -> do
        t1 <- infer env e1
        t1' <- gen env t1
        () <- forM_ mt (isInstanceOf t1)
        infer (insertVar x t1' env) e2
      x -> error $ "Not implemented: " ++ show x

extractVal :: ComputationType -> ValueType
extractVal (ComputationType _ v) = v

typeOrFresh :: Maybe ComputationType -> Infer ComputationType
typeOrFresh Nothing = freshC
typeOrFresh (Just t) = return t

isInstanceOf :: ComputationType -> ComputationType -> Infer ()
isInstanceOf a b = do
  () <- unify a b
  b' <- subM b
  if b == b' then return () else throwError "type error"

inferMany :: Inferable a => TypeEnv -> [a] -> Infer [ComputationType]
inferMany env = mapM (infer env)

vToC :: ValueType -> Infer ComputationType
vToC v = do
  row <- fresh
  return $ ComputationType (Extend row) v

instance Inferable Value where
  infer _ (Int _) = vToC TypeInt
  infer _ (String _) = vToC TypeString
  infer _ (Bool _) = vToC TypeBool
  infer _ Unit = vToC TypeUnit
  infer env (Fn (Function args tRet body)) = do
    -- Extract the argument names
    let args' = map fst args

    -- Extract types for arguments or give them a fresh type var
    tArgs <- mapM (typeOrFresh . snd) args

    -- Any type vars in the signature need to be bound inside
    -- so we need to extract them
    let typeVars = Set.unions $ map (freeTypeVars env) tArgs

    let tArgs' = map (TypeScheme [] []) tArgs

    -- The body needs the variables and the bound type vars
    let bodyEnv = addTypeVars typeVars $ extendVars (zip args' tArgs') env

    tRetInferred <- infer bodyEnv body
    () <- forM_ tRet (isInstanceOf tRetInferred)

    tArgs'' <- mapM subM tArgs
    tRet' <- subM tRetInferred
    row <- freshR
    return $ ComputationType row $ TypeArrow tArgs'' tRet'
  infer _ _ = error "Not implemented yet"