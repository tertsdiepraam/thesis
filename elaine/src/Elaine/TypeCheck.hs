{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}

module Elaine.TypeCheck where

import Control.Lens (Lens', over, set, view, (^.))
import Control.Lens.TH (makeLenses)
import Control.Monad (forM_, when)
import Control.Monad.Except (ExceptT, MonadError (throwError), runExceptT, withExceptT)
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
import qualified Data.MultiSet as MS
import Data.Set (Set)
import qualified Data.Set as Set
import Elaine.AST
import Elaine.Pretty (pretty, Pretty)
import Elaine.Row (Row (Row))
import qualified Elaine.Row as Row
import Elaine.Std (stdTypes)
import Elaine.TypeVar (TypeVar (ExplicitVar, ImplicitVar))

type Infer = ExceptT String (State (Int, Substitutions))

addStackTrace :: String -> Infer a -> Infer a
addStackTrace s =
  withExceptT
    ( \e ->
        e
          ++ "\n"
          ++ "while "
          ++ s
    )

-- TypeVar to Type
data Substitutions = Substitutions
  { subTypeVars :: Map ValueType ValueType,
    subEffectVars :: Map TypeVar Row
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
  let subTypeVars = Map.fromList $ zip (map TypeV typeVars) (map TypeV freshTypeVars)

  freshEffectVar <- freshes effectVars
  let subEffectVars = Map.fromList $ zip effectVars (map Row.var freshEffectVar)
  return $
    sub
      ( Substitutions {subTypeVars, subEffectVars}
      )
      typ

gen :: TypeEnv -> ComputationType -> Infer TypeScheme
gen env typ = do
  let typeVars = Set.toList $ freeTypeVars env typ
  freshTypeVars <- freshes typeVars
  let typeVarMap = zip (map TypeV typeVars) (map TypeV freshTypeVars)

  let effectVars = Set.toList $ freeEffectVars env typ
  freshEffectVars <- freshes effectVars
  let effectVarMap = zip effectVars (map Row.var freshEffectVars)

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

    allTypeVarsV (TypeV v) = Set.singleton v
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

    allEffectVarsR row = maybe Set.empty Set.singleton $ Row.extend row

fresh :: Infer TypeVar
fresh = do
  (i, subs) <- get
  put (i + 1, subs)
  return $ ImplicitVar i

freshV :: Infer ValueType
freshV = TypeV <$> fresh

freshR :: Infer Row
freshR = Row.var <$> fresh

freshC :: Infer ComputationType
freshC = ComputationType <$> freshR <*> freshV

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

addEffectSub :: TypeVar -> Row -> Infer ()
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
  let newMainType = ComputationType Row.empty v
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
                    ComputationType (Row.var $ ExplicitVar "a") $
                      TypeArrow
                        (map (ComputationType Row.empty) args)
                        (ComputationType (Row.open name $ ExplicitVar "b") ret)
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
    () <- forM_ mt (force tExpr)
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

instance Substitutable Row where
  sub subs row = case Row.extend row of
    Just var -> case Map.lookup var (subEffectVars subs) of
      Just row' -> Row.update row row'
      Nothing -> row
    Nothing -> row

unify :: ComputationType -> ComputationType -> Infer ()
unify a b = addStackTrace ("unifying" ++ pretty a ++ pretty b) do
  a' <- subM a
  b' <- subM b
  unify' a' b'
  where
    unify' (ComputationType rowA typA) (ComputationType rowB typB) = do
      () <- unifyRows rowA rowB
      unifyV typA typB

    unifyV a' b' | a' == b' = return ()
    unifyV v@(TypeV _) t = addTypeSub v t
    unifyV t v@(TypeV _) = addTypeSub v t
    unifyV (TypeArrow args1 ret1) (TypeArrow args2 ret2) = do
      () <- mapM_ (uncurry unify) (zip args1 args2)
      unify ret1 ret2
    unifyV (TypeHandler name1 from1 to1) (TypeHandler name2 from2 to2) = do
      () <- when (name1 /= name2) $ throwError "failed to unify handlers for different effects"
      () <- unify (ComputationType Row.empty (TypeV from1)) (ComputationType Row.empty (TypeV from2))
      () <- unify (ComputationType Row.empty to1) (ComputationType Row.empty to2)
      return ()
    unifyV _ _ = throwError "Failed to unify: type error"

unifyRows :: Row -> Row -> Infer ()
unifyRows a@(Row effsA maybeExA) b@(Row effsB maybeExB)
  -- Short circuit on equality: nothing left to unify
  | a == b = return ()
  -- We have to check for the existence of extend variables
  | otherwise = case (maybeExA, maybeExB) of
      -- Rows are not equal and cannot be extended, so cannot be unified
      (Nothing, Nothing) -> throwError err
      -- If the variables are equal and the effects are different, they
      -- cannot be unified
      (Just exA, Just exB) | exA == exB -> throwError err
      -- If a is a subset of b, we can unify the extend of a with (b-a)
      (Just exA, Nothing)
        | MS.isSubsetOf effsA effsB ->
            addEffectSub
              exA
              (Row.closed $ effsB MS.\\ effsA)
      -- If b is a subset of a, we can unify the extend of a with (a-b)
      (Nothing, Just exB)
        | MS.isSubsetOf effsB effsA ->
            addEffectSub
              exB
              (Row.closed $ effsA MS.\\ effsB)
      (Just exA, Just exB) -> do
        -- Create a new extend that's gonna be the extend of the combined row
        exC <- fresh
        -- We need to express both exA and exB in terms of exC much like above
        -- but we don't have the subset requirement anymore
        () <-
          addEffectSub
            exA
            (Row.open (effsB MS.\\ effsA) exC)
        () <-
          addEffectSub
            exB
            (Row.open (effsA MS.\\ effsB) exC)
        return ()
      _ -> throwError err
  where
    err = "Cannot unify rows: " ++ show a ++ " and " ++ show b

class Pretty a => Inferable a where
  infer :: TypeEnv -> a -> Infer ComputationType
  infer env a = addStackTrace ("inferring " ++ pretty a) (infer' env a)

  infer' :: TypeEnv -> a -> Infer ComputationType

-- Algorithm W
instance Inferable Expr where
  infer' env =
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
        vt <- freshV
        () <- unify t1 (ComputationType row TypeBool)
        () <- unify t2 (ComputationType row vt)
        () <- unify t3 (ComputationType row vt)
        subM (ComputationType row vt)
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
          emptyEff (ComputationType _ v) = ComputationType Row.empty v
      Let x mt e1 e2 -> do
        t1 <- infer env e1
        t1' <- subM t1 >>= gen env
        () <- forM_ mt (force t1)
        infer (insertVar x t1' env) e2
      Handle e1 e2 -> do
        t1 <- infer env e1
        rowVar <- fresh
        let restRow = Row.var rowVar
        vt <- freshV
        () <- unify (ComputationType restRow vt) t1

        vt' <- subM vt
        (name, from, to) <- case vt' of
          TypeHandler name from to -> return (name, from, to)
          _ -> throwError "handle did not get a handler"

        t2 <- infer env e2
        () <- unify (ComputationType (Row.open name rowVar) (TypeV from)) t2

        subM $ ComputationType restRow to
      x -> error $ "Not implemented: " ++ show x

extractVal :: ComputationType -> ValueType
extractVal (ComputationType _ v) = v

typeOrFresh :: Maybe ComputationType -> Infer ComputationType
typeOrFresh Nothing = freshC
typeOrFresh (Just t) = return t

-- Force a type to another type
-- Essentially a one-way unify where we force the first argument to the second
force :: ComputationType -> ComputationType -> Infer ()
force a b = addStackTrace ("forcing " ++ pretty a ++ " to " ++ pretty b) do
  ComputationType rowA valueA <- subM a
  ComputationType rowB valueB <- subM b
  () <- forceRow rowA rowB
  () <- forceValue valueA valueB
  return ()

forceRow :: Row -> Row -> Infer ()
forceRow a@(Row effsA maybeExA) b@(Row effsB maybeExB) = case (maybeExA, maybeExB) of
  -- If both are present, set exA to exB and the missing effects
  -- If exB is missing, set it to just the missing effects
  (Just exA, _) ->
    if MS.isSubsetOf effsA effsB
      then
        addEffectSub
          exA
          (Row (effsB MS.\\ effsA) maybeExB)
      else throwError err
  -- b is polymorphic, a is not, therefore cannot be forced
  (Nothing, Just _) -> throwError err
  (Nothing, Nothing) ->
    if effsA == effsB
      then -- Nothing to do, they are already equal
        return ()
      else throwError err
      --
  where
    err = "Cannot force rows: " ++ pretty a ++ " to " ++ pretty b

forceValue :: ValueType -> ValueType -> Infer ()
forceValue a b | a == b = return ()
forceValue v@(TypeV _) b = addTypeSub v b
forceValue (TypeArrow args1 ret1) (TypeArrow args2 ret2) = do
  () <- mapM_ (uncurry force) (zip args1 args2)
  force ret1 ret2
forceValue (TypeHandler name1 from1 to1) (TypeHandler name2 from2 to2) = do
  () <- when (name1 /= name2) $ throwError "failed to unify handlers for different effects"
  () <- force (ComputationType Row.empty (TypeV from1)) (ComputationType Row.empty (TypeV from2))
  () <- force (ComputationType Row.empty to1) (ComputationType Row.empty to2)
  return ()
forceValue a b = throwError $ "Failed to force: " ++ pretty a ++ " to " ++ pretty b

inferMany :: Inferable a => TypeEnv -> [a] -> Infer [ComputationType]
inferMany env = mapM (infer env)

vToC :: ValueType -> Infer ComputationType
vToC v = do
  row <- fresh
  return $ ComputationType (Row.var row) v

instance Inferable Value where
  infer' env = \case
    Int _ -> vToC TypeInt
    String _ -> vToC TypeString
    Bool _ -> vToC TypeBool
    Unit -> vToC TypeUnit
    Fn (Function args tRet body) -> do
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
      () <- forM_ tRet (force tRetInferred)

      tArgs'' <- mapM subM tArgs
      tRet' <- subM tRetInferred
      vToC $ TypeArrow tArgs'' tRet'
    Hdl (Handler ret clauses) -> do
      -- Find the matching effect if available
      let maybeEffect = find matchesClauses (Map.toList $ view effects env)
      -- TODO: the name must be the full path to the effect to avoid conflicts
      (name, signatures) <- case maybeEffect of
        Just a -> return a
        Nothing -> throwError "could not match handler with an effect"

      -- Figure out the type of the return case
      -- TODO: honor type declarations
      from <- fresh
      let fromV = TypeV from
      to <- freshV

      (x, body) <- case ret of
        Function [(x, Nothing)] Nothing body -> return (x, body)
        _ -> throwError "return case cannot have type annotations"

      toInferred <- infer (insertVar x (TypeScheme [] [] $ ComputationType Row.empty fromV) env) body

      () <- unify toInferred (ComputationType Row.empty to)
      to' <- subM to
      let toC' = ComputationType Row.empty to'

      () <-
        mapM_
          ( \(OperationClause cName args body') -> do
              let OperationSignature _ sigArgs sigRet = case findSig cName signatures of
                    Just sig -> sig
                    Nothing -> error "ICE"
              let sigArgs' = map (Just . ComputationType Row.empty) sigArgs
              let sigRet' = ComputationType Row.empty sigRet
              let clauseFunc = Function (zip args sigArgs') (Just toC') body'
              infer
                ( insertVar
                    "resume"
                    (TypeScheme [] [] $ ComputationType Row.empty (TypeArrow [sigRet'] toC'))
                    env
                )
                (Fn clauseFunc)
          )
          clauses

      to'' <- subM to'
      fromV' <- subM fromV
      let from' = case fromV' of
            TypeV v -> v
            _ -> error "handler must be generic"
      vToC $ TypeHandler name from' to''
      where
        sigNames = map (\(OperationSignature x _ _) -> x)
        clauseNames = map (\(OperationClause x _ _) -> x) clauses
        matchesClauses (_, signatures) = Set.fromList clauseNames == Set.fromList (sigNames signatures)
        findSig x = find (\(OperationSignature y _ _) -> y == x)
    _ -> error "Not implemented yet"