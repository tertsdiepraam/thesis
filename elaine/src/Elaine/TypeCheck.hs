{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}

module Elaine.TypeCheck where

import Control.Lens (Lens', over, set, view, (^.))
import Control.Lens.TH (makeLenses)
import Control.Monad (forM_, when)
import Control.Monad.Except (ExceptT, MonadError (throwError), runExceptT)
import Control.Monad.State
  ( MonadState (get, put),
    State,
    evalState,
    (<=<),
  )
import Data.Foldable (foldlM)
import Data.List (find)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text.Lazy (unpack)
import Elaine.AST
import Elaine.Std (stdTypes)
import Text.Pretty.Simple (pShow)

type Infer = ExceptT String (State (Int, Substitutions))

-- TypeVar to Type
data Substitutions = Substitutions
  { subTypeVars :: Map ValueType ValueType,
    subEffectVars :: Map EffectRow EffectRow
  }
  deriving (Show)

-- Variable names to type
data TypeEnv = TypeEnv
  { _vars :: Map String TypeScheme,
    _effects :: Map String [OperationSignature],
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
union a = unionLens vars a . unionLens mods a . unionLens effects a
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

insertEffect :: String -> [OperationSignature] -> TypeEnv -> TypeEnv
insertEffect k v = over effects $ Map.insert k v

singletonVar :: String -> TypeScheme -> TypeEnv
singletonVar k v = insertVar k v empty

singletonMod :: String -> TypeEnv -> TypeEnv
singletonMod k v = insertMod k v empty

singletonEffect :: String -> [OperationSignature] -> TypeEnv
singletonEffect k v = insertEffect k v empty

empty :: TypeEnv
empty =
  TypeEnv
    { _vars = Map.empty,
      _effects = Map.empty,
      _mods = Map.empty,
      _bound = Set.empty
    }

inst :: TypeScheme -> Infer ComputationType
inst (TypeScheme {typeVars, effectVars, typ}) = do
  freshTypeVars <- freshes typeVars
  let subTypeVars = Map.fromList $ zip (map TypeVar typeVars) (map TypeVar freshTypeVars)

  freshEffectVar <- freshes effectVars
  let subEffectVars = Map.fromList $ zip (map Extend effectVars) (map Extend freshEffectVar)
  return $
    sub
      ( Substitutions {subTypeVars, subEffectVars}
      )
      typ

gen :: TypeEnv -> ComputationType -> Infer TypeScheme
gen env typ = do
  let typeVars = Set.toList $ freeTypeVars env typ
  freshTypeVars <- freshes typeVars
  let typeVarMap = zip (map TypeVar typeVars) (map TypeVar freshTypeVars)

  let effectVars = Set.toList $ freeEffectVars env typ
  freshEffectVars <- freshes effectVars
  let effectVarMap = zip (map Extend effectVars) (map Extend freshEffectVars)

  let subs =
        Substitutions
          { subTypeVars = Map.fromList typeVarMap,
            subEffectVars = Map.fromList effectVarMap
          }

  let typ' = sub subs typ

  return $
    TypeScheme
      { typeVars = freshTypeVars,
        effectVars = freshEffectVars,
        typ = typ'
      }

freshes :: [a] -> Infer [TypeVar]
freshes = mapM (const fresh)

freeTypeVars :: TypeEnv -> ComputationType -> Set TypeVar
freeTypeVars env t = allTypeVars t Set.\\ view bound env
  where
    allTypeVars (ComputationType _ typ) = allTypeVarsV typ

    allTypeVarsV (TypeVar v) = Set.singleton v
    allTypeVarsV (TypeArrow args ret) = Set.unions (map allTypeVars (args ++ [ret]))
    allTypeVarsV (TypeHandler _ from to) = Set.delete from $ allTypeVarsV to
    allTypeVarsV _ = Set.empty

freeEffectVars :: TypeEnv -> ComputationType -> Set TypeVar
freeEffectVars env t = allEffectVars t Set.\\ view bound env
  where
    allEffectVars (ComputationType row typ) =
      Set.union
        (allEffectVarsR row)
        (allEffectVarsV typ)

    allEffectVarsV (TypeArrow args ret) = Set.unions (map allEffectVars (args ++ [ret]))
    allEffectVarsV (TypeHandler _ _ to) = allEffectVarsV to
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
  put (i, updateSubs $ subs {subTypeVars = Map.insert k v (subTypeVars subs)})

addEffectSub :: EffectRow -> EffectRow -> Infer ()
addEffectSub k v = do
  (i, subs) <- get
  put (i, updateSubs $ subs {subEffectVars = Map.insert k v (subEffectVars subs)})

updateSubs :: Substitutions -> Substitutions
updateSubs subs =
  Substitutions
    { subTypeVars = Map.map (sub subs) (subTypeVars subs),
      subEffectVars = Map.map (sub subs) (subEffectVars subs)
    }

runInfer :: Infer a -> Either String a
runInfer a = evalState (runExceptT a) (0, emptySubs)

typeCheck :: [Declaration] -> Either String TypeEnv
typeCheck decs = runInfer $ do
  (env, _) <- typeCheckMod (initialEnv stdTypes) decs

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
    stdTypeEnv types = set vars types empty
    initialEnv types = singletonMod "std" (stdTypeEnv types)

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
  DecEffect name signatures ->
    let sigsAsFunctions =
          map
            ( \(OperationSignature f args ret) ->
                ( f,
                  TypeScheme [] [ExplicitVar "a", ExplicitVar "b"] $
                    ComputationType (Extend $ ExplicitVar "a") $
                      TypeArrow
                        (map (ComputationType Empty) args)
                        (ComputationType (Cons name $ Extend $ ExplicitVar "b") ret)
                )
            )
            signatures
        newEnv =
          empty
            { _vars = Map.fromList sigsAsFunctions,
              _effects = Map.singleton name signatures
            }
     in return newEnv
  DecLet x mt expr -> do
    tExpr <- infer env expr >>= subM
    () <- forM_ mt (isInstanceOf tExpr)
    tExpr' <- subM tExpr >>= gen env
    return $ singletonVar x tExpr'

class Substitutable a where
  sub :: Substitutions -> a -> a

instance Substitutable ValueType where
  sub subs vt | Just vt' <- Map.lookup vt (subTypeVars subs) = vt'
  sub subs (TypeArrow args ret) = TypeArrow (map (sub subs) args) (sub subs ret)
  sub subs (TypeHandler name from to) = TypeHandler name from (sub subs to)
  sub _ vt = vt

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
      () <- unifyRows rowA rowB
      unifyV typA typB

    unifyV a' b' | a' == b' = return ()
    unifyV v@(TypeVar _) t = addTypeSub v t
    unifyV t v@(TypeVar _) = addTypeSub v t
    unifyV (TypeArrow args1 ret1) (TypeArrow args2 ret2) = do
      () <- mapM_ (uncurry unify) (zip args1 args2)
      unify ret1 ret2
    unifyV (TypeHandler name1 from1 to1) (TypeHandler name2 from2 to2) = do
      () <- when (name1 /= name2) $ throwError "failed to unify handlers for different effects"
      () <- unify (ComputationType Empty (TypeVar from1)) (ComputationType Empty (TypeVar from2))
      () <- unify (ComputationType Empty to1) (ComputationType Empty to2)
      return ()
    unifyV _ _ = throwError "Failed to unify: type error"

unifyRows :: EffectRow -> EffectRow -> Infer ()
unifyRows a' b' | a' == b' = return ()
unifyRows v@(Extend _) row = addEffectSub v row
unifyRows row v@(Extend _) = addEffectSub v row
unifyRows aRow@(Cons aEff aRest) bRow@(Cons bEff bRest)
  | Just b' <- removeEffect aEff bRow = unifyRows aRest b'
  | Just a' <- removeEffect bEff aRow = unifyRows a' bRest
  where
    removeEffect a' (Cons b' rest) | a' == b' = Just rest
    removeEffect a' (Cons b' rest) = do
      rest' <- removeEffect a' rest
      return $ Cons b' rest'
    removeEffect _ _ = Nothing
unifyRows a' b' =
  throwError
    ( "failed to unify effect rows "
        ++ show a'
        ++ " -- "
        ++ show b'
    )

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
        subM t3
      App f args -> do
        tf <- infer env f
        tArgs <- inferMany env args
        tRet <- freshC
        row <- freshR
        () <- forM_ (map getRow tArgs) (unifyRows row)
        () <- unify tf (ComputationType row (TypeArrow (map emptyEff tArgs) tRet))
        subM tRet
        where
          getRow (ComputationType r _) = r
          emptyEff (ComputationType _ v) = ComputationType Empty v
      Let x mt e1 e2 -> do
        t1 <- infer env e1
        t1' <- subM t1 >>= gen env
        () <- forM_ mt (isInstanceOf t1)
        infer (insertVar x t1' env) e2
      Handle e1 e2 -> do
        t1 <- infer env e1
        restRow <- freshR
        vt <- freshV
        () <- unify (ComputationType restRow vt) t1

        vt' <- subM vt
        (name, from, to) <- case vt' of
          TypeHandler name from to -> return (name, from, to)
          _ -> throwError "handle did not get a handler"

        t2 <- infer env e2
        () <- unify (ComputationType (Cons name restRow) (TypeVar from)) t2

        subM $ ComputationType restRow to
      x -> error $ "Not implemented: " ++ show x

extractVal :: ComputationType -> ValueType
extractVal (ComputationType _ v) = v

typeOrFresh :: Maybe ComputationType -> Infer ComputationType
typeOrFresh Nothing = freshC
typeOrFresh (Just t) = return t

isInstanceOf :: ComputationType -> ComputationType -> Infer ()
isInstanceOf a b = do
  -- This was necessary, not sure why
  a' <- subM a
  b' <- subM b
  () <- unify a' b'
  b'' <- subM b'
  if b' == b''
    then return ()
    else do
      (_, subs) <- get
      throwError ("isInstanceOf failed: " ++ unpack (pShow (a', b', subs)))

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
    vToC $ TypeArrow tArgs'' tRet'
  infer env (Hdl (Handler ret clauses)) = do
    -- Find the matching effect if available
    let maybeEffect = find matchesClauses (Map.toList $ view effects env)
    -- TODO: the name must be the full path to the effect to avoid conflicts
    (name, signatures) <- case maybeEffect of
      Just a -> return a
      Nothing -> throwError "could not match handler with an effect"

    -- Figure out the type of the return case
    -- TODO: honor type declarations
    from <- fresh
    let fromV = TypeVar from
    to <- freshV

    (x, body) <- case ret of
      Function [(x, Nothing)] Nothing body -> return (x, body)
      _ -> throwError "return case cannot have type annotations"

    toInferred <- infer (insertVar x (TypeScheme [] [] $ ComputationType Empty fromV) env) body

    () <- unify toInferred (ComputationType Empty to)
    to' <- subM to
    let toC' = ComputationType Empty to'

    () <-
      mapM_
        ( \(OperationClause cName args body') -> do
            let OperationSignature _ sigArgs sigRet = case findSig cName signatures of
                  Just sig -> sig
                  Nothing -> error "ICE"
            let sigArgs' = map (Just . ComputationType Empty) sigArgs
            let sigRet' = ComputationType Empty sigRet
            let clauseFunc = Function (zip args sigArgs') (Just toC') body'
            infer
              ( insertVar
                  "resume"
                  (TypeScheme [] [] $ ComputationType Empty (TypeArrow [sigRet'] toC'))
                  env
              )
              (Fn clauseFunc)
        )
        clauses

    to'' <- subM to'
    fromV' <- subM fromV
    let from' = case fromV' of
          TypeVar v -> v
          _ -> error "handler must be generic"
    vToC $ TypeHandler name from' to''
    where
      sigNames = map (\(OperationSignature x _ _) -> x)
      clauseNames = map (\(OperationClause x _ _) -> x) clauses
      matchesClauses (_, signatures) = Set.fromList clauseNames == Set.fromList (sigNames signatures)
      findSig x = find (\(OperationSignature y _ _) -> y == x)
  infer _ _ = error "Not implemented yet"