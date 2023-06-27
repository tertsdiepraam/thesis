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
    runState,
    (<=<),
  )
import Data.Aeson (ToJSON)
import Data.Char (isLower)
import Data.Foldable (foldlM)
import Data.List (find, intercalate, sortOn)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (fromJust)
import qualified Data.MultiSet as MS
import Data.Set (Set)
import qualified Data.Set as Set
import Elaine.AST (ASTValueType)
import Elaine.AST hiding (Constructor (..), ASTValueType (..), Row)
import qualified Elaine.AST as AST
import Elaine.Ident (Ident (Ident, idText), Location (LocBuiltIn, LocNone))
import Elaine.Pretty (Pretty, pretty)
import Elaine.Std (stdTypes)
import Elaine.TypeVar (TypeVar (ExplicitVar, ImplicitVar))
import Elaine.Types (Arrow (Arrow), CompType (CompType), Effect (Effect), Row (..), TypeScheme (TypeScheme, effectVars, typ, typeVars), ValType (..), rowEmpty, rowIsEmpty, rowMaybe, rowOpen, rowUpdate, rowVar, rowMerge, DataType (..), Constructor (..))
import GHC.Generics (Generic)
import Prelude hiding (pure)

-- TypeVar to Type
data Substitutions = Substitutions
  { subTypeVars :: Map ValType ValType,
    subEffectVars :: Map TypeVar Row
  }
  deriving (Show)

-- Variable names to type
data TypeEnv = TypeEnv
  { _currentPath :: [Ident],
    _vars :: Map Ident TypeScheme,
    _types :: Map Ident DataType,
    _effects :: Map Ident Effect,
    _mods :: Map Ident TypeEnv,
    _bound :: Set TypeVar
  }
  deriving (Show)

makeLenses ''TypeEnv

type Infer = ExceptT String (State CheckState)

addStackTrace :: String -> Infer a -> Infer a
addStackTrace s =
  withExceptT
    ( ++
        "\n"
          ++ "while "
          ++ s
    )

data Metadata = Metadata
  { -- left: current span, right: definition of that span
    definitions :: [(Ident, Ident)],
    elabs :: Map Int [Ident]
    -- types :: [(Ident, TypeScheme)],
    -- handlers :: [(Ident, Ident)],
    -- elaborations :: [(Ident, Ident)]
  }
  deriving (Show, Generic)

instance ToJSON Metadata

metaEmpty :: Metadata
metaEmpty = Metadata [] Map.empty

data CheckState = CheckState
  { stateCounter :: Int,
    stateSubs :: Substitutions,
    stateMetadata :: Metadata
  }

stateEmpty :: CheckState
stateEmpty = CheckState 0 emptySubs metaEmpty

recordMeta :: (Metadata -> Metadata) -> Infer ()
recordMeta f = do
  s <- get
  let meta = stateMetadata s
  let meta' = f meta
  put $ s {stateMetadata = meta'}

recordMetaDefinition :: Ident -> Ident -> Infer ()
recordMetaDefinition x y = recordMeta f
  where
    f meta = meta {definitions = (x, y) : definitions meta}

recordMetaElab :: Int -> [Ident] -> Infer ()
recordMetaElab x y = recordMeta f
  where
    f meta = meta {elabs = Map.insert x y $ elabs meta}

-- A version of lookup/! that uses the Infer monad in case of an error
-- It also returns the key, because with identifiers a lookup might succeed
-- but the identifiers could still have different location that we want to know
-- about.
get' :: Map Ident b -> Ident -> Infer (Ident, b)
get' m a = case Map.lookupIndex a m of
  Just index -> return (Map.elemAt index m)
  Nothing -> throwError $ "undefined identifier: " ++ show a

union :: TypeEnv -> TypeEnv -> TypeEnv
union a = unionLens vars a . unionLens mods a . unionLens effects a . unionLens types a
  where
    unionLens :: Ord a => Lens' TypeEnv (Map a b) -> TypeEnv -> TypeEnv -> TypeEnv
    unionLens l a' = over l (Map.union $ a' ^. l)

getVar :: TypeEnv -> Ident -> Infer TypeScheme
getVar env x = do
  (defId, defType) <- get' (view vars env) x
  () <- recordMetaDefinition x defId
  return defType

extendVars :: [(Ident, TypeScheme)] -> TypeEnv -> TypeEnv
extendVars newVars = over vars (Map.union $ Map.fromList newVars)

addTypeVars :: Set TypeVar -> TypeEnv -> TypeEnv
addTypeVars newBound = over bound (Set.union newBound)

getMod :: TypeEnv -> Ident -> Infer TypeEnv
getMod env x = do
  (defId, env') <- get' (view mods env) x
  () <- recordMetaDefinition x defId
  return env'

insertVar :: Ident -> TypeScheme -> TypeEnv -> TypeEnv
insertVar k v = over vars $ Map.insert k v

insertMod :: Ident -> TypeEnv -> TypeEnv -> TypeEnv
insertMod k v = over mods $ Map.insert k v

singletonVar :: Ident -> TypeScheme -> TypeEnv
singletonVar k v = insertVar k v empty

singletonMod :: Ident -> TypeEnv -> TypeEnv
singletonMod k v = insertMod k v empty

empty :: TypeEnv
empty =
  TypeEnv
    { _currentPath = [],
      _vars = Map.empty,
      _types = Map.empty,
      _effects = Map.empty,
      _mods = Map.empty,
      _bound = Set.empty
    }

inst :: TypeScheme -> Infer CompType
inst (TypeScheme {typeVars, effectVars, typ}) = do
  freshTypeVars <- freshes typeVars
  let subTypeVars = Map.fromList $ zip (map TypeV typeVars) (map TypeV freshTypeVars)

  freshEffectVar <- freshes effectVars
  let subEffectVars = Map.fromList $ zip effectVars (map rowVar freshEffectVar)
  return $
    sub
      ( Substitutions {subTypeVars, subEffectVars}
      )
      typ

gen :: TypeEnv -> CompType -> Infer TypeScheme
gen env typ = do
  let typeVars = Set.toList $ freeTypeVars env typ
  freshTypeVars <- freshes typeVars
  let typeVarMap = zip (map TypeV typeVars) (map TypeV freshTypeVars)

  let effectVars = Set.toList $ freeEffectVars env typ
  freshEffectVars <- freshes effectVars
  let effectVarMap = zip effectVars (map rowVar freshEffectVars)

  let subs =
        Substitutions
          { subTypeVars = Map.fromList typeVarMap,
            subEffectVars = Map.fromList effectVarMap
          }

  let typ' = sub subs typ

  return $
    TypeScheme freshTypeVars freshEffectVars typ'

freshes :: [a] -> Infer [TypeVar]
freshes = mapM (const fresh)

freeTypeVars :: TypeEnv -> CompType -> Set TypeVar
freeTypeVars env t = allTypeVars t Set.\\ view bound env
  where
    allTypeVars (CompType _ typ) = allTypeVarsV typ

    allTypeVarsV (TypeV v) = Set.singleton v
    allTypeVarsV (TypeArrow (Arrow args ret)) = Set.unions (map allTypeVars (args ++ [ret]))
    allTypeVarsV (TypeData _ params) = Set.unions (map allTypeVarsV params)
    allTypeVarsV (TypeHandler _ from to) = Set.union (allTypeVarsV from) (allTypeVarsV to)
    allTypeVarsV _ = Set.empty

freeEffectVars :: TypeEnv -> CompType -> Set TypeVar
freeEffectVars env t = allEffectVars t Set.\\ view bound env
  where
    allEffectVars (CompType row typ) =
      Set.union
        (allEffectVarsR row)
        (allEffectVarsV typ)

    allEffectVarsV (TypeArrow (Arrow args ret)) = Set.unions (map allEffectVars (args ++ [ret]))
    allEffectVarsV (TypeHandler _ _ to) = allEffectVarsV to
    allEffectVarsV _ = Set.empty

    allEffectVarsR row = maybe Set.empty Set.singleton $ rowExtension row

fresh :: Infer TypeVar
fresh = do
  s <- get
  let i = stateCounter s
  put (s {stateCounter = i + 1})
  return $ ImplicitVar i

freshV :: Infer ValType
freshV = TypeV <$> fresh

freshR :: Infer Row
freshR = rowVar <$> fresh

freshC :: Infer CompType
freshC = CompType <$> freshR <*> freshV

subM :: Substitutable a => a -> Infer a
subM a = do
  s <- get
  return $ sub (stateSubs s) a

emptySubs :: Substitutions
emptySubs = Substitutions Map.empty Map.empty

addTypeSub :: ValType -> ValType -> Infer ()
addTypeSub k v = do
  s <- get
  let subs = stateSubs s
  let newSubs = updateSubs $ subs {subTypeVars = Map.insert k v (subTypeVars subs)}
  put $ s {stateSubs = newSubs}

addEffectSub :: TypeVar -> Row -> Infer ()
addEffectSub k v = do
  s <- get
  let subs = stateSubs s
  let newSubs = updateSubs $ subs {subEffectVars = Map.insert k v (subEffectVars subs)}
  put $ s {stateSubs = newSubs}

updateSubs :: Substitutions -> Substitutions
updateSubs subs =
  Substitutions
    { subTypeVars = Map.map (sub subs) (subTypeVars subs),
      subEffectVars = Map.map (sub subs) (subEffectVars subs)
    }

runInfer :: Infer a -> Either String (a, CheckState)
runInfer a = case runState (runExceptT a) stateEmpty of
  (Left x, _) -> Left x
  (Right x, y) -> Right (x, y)

typeCheck :: [Declaration] -> Either String (TypeEnv, CheckState)
typeCheck decs = runInfer $ do
  (env, _) <- typeCheckMod (initialEnv stdTypes) decs

  -- We're forcing main to not have any effects
  -- Most of the type, it will be polymorphic over some effects, but
  -- we don't have any more handlers, to forcing to empty is ok.
  mainType <- inst $ view vars env Map.! (Ident "main" LocNone)
  v <- freshV
  let newMainType = CompType rowEmpty v
  () <- unify mainType newMainType
  mainType' <- subM mainType
  mainType'' <- gen env mainType'
  subM (insertVar (Ident "main" LocNone) mainType'' env)
  where
    stdTypeEnv types = set vars types empty
    initialEnv types = singletonMod (Ident "std" LocBuiltIn) (stdTypeEnv types)

getMain :: TypeEnv -> TypeScheme
getMain = flip (Map.!) (Ident "main" LocNone) . view vars

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
    let path = view currentPath env ++ [x]
    let env' = set currentPath path env
    modEnv <- typeCheckMod env' decs
    return $ singletonMod x (snd modEnv)
  DecType name params constructors -> do
    -- Create the data type
    let path = view currentPath env ++ [name]
    constructors' <- mapM
      ( \(AST.Constructor name params) -> do
        params' <- mapM (resolveCompType env) params
        return $ Constructor name params'
      )
      constructors
    let dataType = DataType path params constructors'

    -- Create the functions for the constructors
    functions <- mapM
      ( \(Constructor name' params') -> do
          ret <- pure $ TypeData dataType (map TypeV params)
          typ <- pure $ TypeArrow $ Arrow params' ret
          genType <- gen env typ
          return (name', genType)
      )
          constructors'
    return $ empty {
      _types = Map.singleton name dataType,
      _vars = Map.fromList functions
    }
  DecEffect name signatures -> do
    let fullPath = view currentPath env ++ [name]
    arrows <-
      mapM
        ( \(OperationSignature f args ret) -> do
            args' <- mapM (resolveCompType env) args
            ret' <- resolveCompType env ret
            return (f, Arrow args' ret')
        )
        signatures

    let eff = Effect fullPath (Map.fromList arrows)

    sigsAsFunctions <-
      mapM
        ( \(f, Arrow args ret) -> do
            let a = ExplicitVar (Ident "a" LocNone)
            let b = ExplicitVar (Ident "b" LocNone)
            ret' <- case ret of
              CompType row retVal | rowIsEmpty row -> return $ CompType (rowOpen [eff] b) retVal
              _ -> throwError "effect operation cannot have any effects"
            return
              ( f,
                TypeScheme [] [a, b] $
                  CompType (rowVar a) $
                    TypeArrow $
                      Arrow args ret'
              )
        )
        arrows
    let newEnv =
          empty
            { _vars = Map.fromList sigsAsFunctions,
              _effects = Map.singleton name eff
            }
    return newEnv
  DecLet x mt expr -> do
    tExpr <- infer env expr >>= subM
    () <- forM_ mt (force tExpr <=< resolveCompType env)
    tExpr' <- subM tExpr >>= gen env
    return $ singletonVar x tExpr'

resolveValType :: TypeEnv -> ASTValueType -> Infer ValType
resolveValType env = \case
  AST.TypeUnit -> return TypeUnit
  AST.TypeConstructor (Ident "Bool" _) [] -> return TypeBool
  AST.TypeConstructor (Ident "String" _) [] -> return TypeString
  AST.TypeConstructor (Ident "Int" _) [] -> return TypeInt
  AST.TypeConstructor x params | isLower (head $ idText x) ->
    if null params then
      return $ TypeV (ExplicitVar x)
    else
      throwError "type variable cannot have type parameters"
  AST.TypeConstructor x params -> do
    dt@(DataType _ dtVars _) <- resolveDataType env x
    params' <- mapM (resolveValType env) params

    () <- when (length params' /= length dtVars) $
      throwError "number of type arguments must match number of type vars of data type"
  
    return $ TypeData dt params'
  AST.TypeTuple params -> do
    params' <- mapM (resolveValType env) params
    return $ TypeTuple params'
  AST.TypeArrow args ret -> do
    args' <- mapM (resolveCompType env) args
    ret' <- resolveCompType env ret
    return $ TypeArrow (Arrow args' ret')
  AST.TypeHandler {} -> throwError "not implemented"
  AST.TypeElaboration {} -> throwError "not implemented"

resolveDataType :: TypeEnv -> Ident -> Infer DataType
resolveDataType env t = case Map.lookup t (view types env) of
  Just t'@(DataType p _ _) -> do
    recordMetaDefinition t (last p)
    return t'
  Nothing -> throwError $ "Could not find data type " ++ pretty t


resolveCompType :: TypeEnv -> ASTComputationType -> Infer CompType
resolveCompType env (ASTComputationType row valType) = do
  row' <- resolveRow env row
  valType' <- resolveValType env valType
  return $ CompType row' valType'

resolveRow :: TypeEnv -> AST.Row -> Infer Row
resolveRow env (AST.Row effs maybeExtend) = do
  effs' <- mapM (resolveEffect env) effs
  return $ rowMaybe effs' (fmap ExplicitVar maybeExtend)

resolveEffect :: TypeEnv -> Ident -> Infer Effect
resolveEffect env eff = case Map.lookup eff (view effects env) of
  Just e' -> do
    let (Effect p _) = e'
    recordMetaDefinition eff (last p)
    return e'
  Nothing -> throwError $ "Could not find effect " ++ pretty eff

class Substitutable a where
  sub :: Substitutions -> a -> a

instance Substitutable ValType where
  sub subs vt | Just vt' <- Map.lookup vt (subTypeVars subs) = vt'
  sub subs (TypeArrow arr) = TypeArrow (sub subs arr)
  sub subs (TypeData dt params) = TypeData dt $ map (sub subs) params
  sub subs (TypeTuple params) = TypeTuple $ map (sub subs) params
  sub subs (TypeHandler name from to) = TypeHandler name (sub subs from) (sub subs to)
  sub _ vt = vt

instance Substitutable Arrow where
  sub subs (Arrow args ret) = Arrow (map (sub subs) args) (sub subs ret)

instance Substitutable TypeEnv where
  sub subs = over vars (Map.map $ sub subs) . over mods (Map.map $ sub subs)

instance Substitutable TypeScheme where
  sub subs (TypeScheme v e t) = TypeScheme v e (sub subs t)

instance Substitutable CompType where
  sub subs (CompType row typ) = CompType (sub subs row) (sub subs typ)

instance Substitutable Row where
  sub subs row = case rowExtension row of
    Just var -> case Map.lookup var (subEffectVars subs) of
      Just row' -> rowUpdate row row'
      Nothing -> row
    Nothing -> row

unify :: CompType -> CompType -> Infer ()
unify a b = do
  a' <- subM a
  b' <- subM b
  addStackTrace
    ( "unifying "
        ++ "\n    "
        ++ pretty a'
        ++ "\n    "
        ++ pretty b'
    )
    $ unify' a' b'
  where
    unify' (CompType rowA typA) (CompType rowB typB) = do
      () <- unifyRows rowA rowB
      unifyV typA typB

    unifyV a' b' | a' == b' = return ()
    -- We give explicit vars priority to make it more likely that those
    -- show up in error messages instead of implicit vars.
    unifyV v@(TypeV (ExplicitVar _)) t = addTypeSub v t
    unifyV t v@(TypeV (ExplicitVar _)) = addTypeSub v t
    unifyV v@(TypeV _) t = addTypeSub v t
    unifyV t v@(TypeV _) = addTypeSub v t
    unifyV (TypeArrow (Arrow args1 ret1)) (TypeArrow (Arrow args2 ret2)) = do
      () <-
        when (length args1 /= length args2) $
          throwError "function does not have the right number of arguments"
      () <- mapM_ (uncurry unify) (zip args1 args2)
      unify ret1 ret2
    unifyV (TypeHandler name1 from1 to1) (TypeHandler name2 from2 to2) = do
      () <- when (name1 /= name2) $ throwError "failed to unify handlers for different effects"
      () <- unifyV from1 from2
      () <- unifyV to1 to2
      return ()
    unifyV (TypeData d1 params1) (TypeData d2 params2) | d1 == d2 =
      mapM_ (uncurry unifyV) (zip params1 params2)
    unifyV (TypeTuple params1) (TypeTuple params2) =
      mapM_ (uncurry unifyV) (zip params1 params2)
    unifyV _ _ = throwError "Failed to unify: type error"

unifyRows :: Row -> Row -> Infer ()
unifyRows a b = do
  a' <- subM a
  b' <- subM b
  unifyRows' a' b'

unifyRows' :: Row -> Row -> Infer ()
unifyRows' a@(Row effsA maybeExA) b@(Row effsB maybeExB)
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
              (Row (effsB MS.\\ effsA) Nothing)
      -- If b is a subset of a, we can unify the extend of a with (a-b)
      (Nothing, Just exB)
        | MS.isSubsetOf effsB effsA ->
            addEffectSub
              exB
              (Row (effsA MS.\\ effsB) Nothing)
      (Just exA, Just exB) -> do
        -- Create a new extend that's gonna be the extend of the combined row
        exC <- fresh
        -- We need to express both exA and exB in terms of exC much like above
        -- but we don't have the subset requirement anymore
        () <-
          addEffectSub
            exA
            (Row (effsB MS.\\ effsA) (Just exC))
        () <-
          addEffectSub
            exB
            (Row (effsA MS.\\ effsB) (Just exC))
        return ()
      _ -> throwError err
  where
    err = "Cannot unify rows: " ++ pretty a ++ " and " ++ pretty b

class Pretty a => Inferable a where
  infer :: TypeEnv -> a -> Infer CompType
  infer env a = addStackTrace ("inferring " ++ pretty a) (infer' env a)

  infer' :: TypeEnv -> a -> Infer CompType

-- Algorithm W
instance Inferable Expr where
  infer' env =
    subM <=< \case
      Val v -> infer env v
      Var x -> do
        t <- getVar env x
        openRow <=< inst $ t
      If e1 e2 e3 -> do
        t1 <- infer env e1
        t2 <- infer env e2
        t3 <- infer env e3
        row <- freshR
        vt <- freshV
        () <- unify t1 (CompType row TypeBool)
        () <- unify t2 (CompType row vt)
        () <- unify t3 (CompType row vt)
        subM (CompType row vt)
      App f args -> do
        -- First we find the function type, the argument types and the return type
        tf <- infer env f
        tArgs <- inferMany env args
        tRet@(CompType row _) <- freshC

        -- The effect rows for the function value, arguments and return type need
        -- to be unified.
        () <- mapM_ (unifyRows row . getRow <=< openRow) tArgs
        row' <- subM row

        -- We might need to open up the return type of a function
        -- to ensure it can be unified.
        tf' <- case tf of
          CompType fRow (TypeArrow (Arrow fArgs fRet)) -> do
            fRet' <- openRow fRet
            return $ CompType fRow (TypeArrow (Arrow fArgs fRet'))
          tf' -> return tf'

        () <- unify tf' (CompType row' (TypeArrow $ Arrow (map emptyEff tArgs) tRet))
        openRow <=< subM $ tRet
        where
          getRow (CompType r _) = r
          emptyEff (CompType _ v) = CompType rowEmpty v
      Let x mt e1 e2 -> do
        t1 <- infer env e1
        t1' <- subM t1 >>= gen env
        () <- forM_ mt (force t1 <=< resolveCompType env)
        case x of
          Just ident -> infer (insertVar ident t1' env) e2
          Nothing -> infer env e2
      Handle e1 e2 -> do
        -- Type of the handler
        t1 <- infer env e1

        -- The effect row of the handle is the merged effect row of e1 and e2
        -- Hence we need a new variable for it.
        rVar <- fresh
        let restRow = rowVar rVar

        -- Unify restRow with t1
        vt <- freshV
        () <- unify (CompType restRow vt) t1

        -- Get the parameters from the handler type
        vt' <- subM vt
        (name, from, to) <- case vt' of
          TypeHandler name from to -> return (name, from, to)
          _ -> throwError "handle did not get a handler"

        -- Make sure that the computation can be handled by unifying
        -- from and t2. The effect row in the computation is the name of the effect
        -- handled and the restRow.
        t2 <- infer env e2
        () <- unify (CompType (rowOpen [name] rVar) from) t2

        subM $ CompType restRow to
      Elab e1 e2 -> do
        t1 <- infer env e1
        rVar <- fresh
        let restRow = rowVar rVar
        vt <- freshV
        () <- unify (CompType restRow vt) t1

        vt' <- subM vt
        (name, row) <- case vt' of
          TypeElaboration name row -> return (name, row)
          _ -> throwError "elab did not get an elaboration"

        let Row expandEffs ex = row

        () <- case ex of
          Just _ -> throwError "Cannot elaborate into an open effect row"
          Nothing -> return ()

        -- Assume that the elaboration is from A! to <B,C,D>
        -- then the inner computation should have row <A!,B,C,D|e>
        let expandEffs' = MS.toList expandEffs
        let innerRow = rowOpen (name : expandEffs') rVar

        CompType row2 vt2 <- infer env e2
        () <- unifyRows innerRow row2

        -- The outer computation should then have <B,C,D|e>
        subM $ CompType (rowOpen expandEffs' rVar) vt2
      ImplicitElab i e1 -> do
        CompType row vt1 <- infer env e1
        let Row effs extend = row
        let (hEffs, aEffs) = MS.partition (\(Effect path _) -> last (idText $ last path) == '!') effs
        elabs <- mapM findElab (MS.toList hEffs)
        let elabIdents = map fst elabs
        () <- recordMetaElab i elabIdents
        let elabRow = foldr (rowMerge . snd) rowEmpty elabs

        let row' = rowMerge elabRow (Row aEffs extend)
        return $ CompType row' vt1
        where
          findElab (Effect path _) = case concatMap (f path) (Map.toList (view vars env)) of
            [(x, e)] -> return (x, e)
            [] -> throwError $ "Could not find elaboration for: " ++ intercalate "::" (map pretty path)
            _ -> throwError $ "Multiple elaborations found for: " ++ intercalate "::" (map pretty path)

          f path1 (x, TypeScheme _ _ (CompType _ (TypeElaboration (Effect path2 _) row))) | path1 == path2 = [(x, row)]
          f _ _ = []
      Tuple es -> do
        row <- freshR
        ts <- mapM (infer env) es
        () <- mapM_ (unifyRows row . getRow <=< openRow) ts

        ts' <- mapM subM ts
        return $ CompType row $ TypeTuple (map getVal ts')
        where
          getRow (CompType r _) = r
          getVal (CompType _ v) = v
      Match e arms -> do
        -- In a match, the arms should define the type
        -- the other way around might lead to an undefined
        -- type.
        --
        -- So we do the following:
        --  - Get the functions corresponding to the arms
        --  - Check that it matches a datatype in scope
        --  - Check that the numbers of arguments match
        --  - Check that the expr matches that type
        --  - Check that all the arms have the same type.
        let patternNames = map (getPatternName . getPattern) arms
        
        dataType@(DataType _ dtParams constructors) <- case find (constructorsMatch patternNames) (view types env) of
              Just dt -> return dt
              Nothing -> throwError $ "Could not find datatype with constructors: " ++ show patternNames
        
        -- We now know the type but not yet the type parameters of the type
        -- so, we have to add those.
        vars' <- mapM (const freshV) dtParams
        let typeData = TypeData dataType vars' 
        final@(CompType finalRow _) <- freshC

        -- Infer and unify the type from e
        te <- infer env e
        () <- unify te (CompType finalRow typeData)

        -- Infer and unify the arms
        () <- 
          mapM_ 
            (
              \(MatchArm (Pattern name args) body) -> do
                -- Find the corresponding constructor
                let Constructor _ params = fromJust $ find (\c' -> name == getConstructorName c') constructors
                
                -- Substitute the type params of the data type in the params of the
                -- constructor 
                let params' = map (sub $ Substitutions {subTypeVars=Map.fromList (zip (map TypeV dtParams) vars'), subEffectVars=Map.empty}) params
                tBody <- inferFunctionLike env args params' Nothing body
                let ret = case tBody of
                      CompType _ (TypeArrow (Arrow _ r)) -> r
                      _ -> error "ICE"
                unify final ret
            ) 
            arms

        subM final
        where
          getPattern (MatchArm p _) = p
          getPatternName (Pattern n _) = n
          getConstructorName (Constructor n _) = n
          constructorsMatch names (DataType _ _ cs) =
            Set.fromList names == Set.fromList (map getConstructorName cs)
      -- _ -> error "Not implemented"


extractVal :: CompType -> ValType
extractVal (CompType _ v) = v

typeOrEmpty :: TypeEnv -> Maybe ASTComputationType -> Infer CompType
typeOrEmpty _ Nothing = CompType rowEmpty <$> freshV
typeOrEmpty env (Just t) = resolveCompType env t

-- Force a type to another type
-- Essentially a one-way unify where we force the first argument to the second
force :: CompType -> CompType -> Infer ()
force a b = addStackTrace ("forcing " ++ pretty a ++ " to " ++ pretty b) do
  CompType rowA valueA <- subM a
  CompType rowB valueB <- subM b
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

forceValue :: ValType -> ValType -> Infer ()
forceValue a b | a == b = return ()
forceValue v@(TypeV _) b = addTypeSub v b
forceValue (TypeArrow (Arrow args1 ret1)) (TypeArrow (Arrow args2 ret2)) = do
  () <-
    when (length args1 /= length args2) $
      throwError "function does not have the right number of arguments"
  () <- mapM_ (uncurry force) (zip args1 args2)
  force ret1 ret2
forceValue (TypeHandler name1 from1 to1) (TypeHandler name2 from2 to2) = do
  () <- when (name1 /= name2) $ throwError "failed to unify handlers for different effects"
  () <- forceValue from1 from2
  () <- forceValue to1 to2
  return ()
forceValue (TypeData d1 params1) (TypeData d2 params2) | d1 == d2 =
  mapM_ (uncurry forceValue) (zip params1 params2)
forceValue (TypeTuple params1) (TypeTuple params2) =
  mapM_ (uncurry forceValue) (zip params1 params2)
forceValue a b = throwError $ "Failed to force: " ++ pretty a ++ " to " ++ pretty b

inferMany :: Inferable a => TypeEnv -> [a] -> Infer [CompType]
inferMany env = mapM (infer env)

pure :: ValType -> Infer CompType
pure v = do
  row <- fresh
  return $ CompType (rowVar row) v

instance Inferable Value where
  infer' env = \case
    Int _ -> pure TypeInt
    String _ -> pure TypeString
    Bool _ -> pure TypeBool
    Unit -> pure TypeUnit
    Fn (Function args ret body) -> do
      -- Extract the argument names
      let (argNames, argTypes) = unzip args

      -- Extract types for arguments or give them a fresh type var
      tArgs <- mapM (typeOrEmpty env) argTypes
      tRet <- mapM (resolveCompType env) ret

      inferFunctionLike env argNames tArgs tRet body
    Hdl (Handler ret clauses) -> do
      -- Find the matching effect if available
      let maybeEffect = find matchesClauses (Map.elems $ view effects env)
      eff@(Effect _ signatures) <- case maybeEffect of
        Just a -> return a
        Nothing -> throwError "could not match handler with an effect"

      -- Figure out the type of the return case
      -- TODO: honor type declarations
      from <- fresh
      let fromV = TypeV from

      CompType toRow to <- case ret of
        Just f -> case f of 
          Function [(x, Nothing)] Nothing body -> 
            infer (insertVar x (TypeScheme [] [] $ CompType rowEmpty fromV) env) body
          _ -> throwError "return case cannot have type annotations"
        Nothing -> do
          r <- freshR
          return $ CompType r fromV

      () <- forceRow toRow rowEmpty
      let toC' = CompType rowEmpty to

      () <-
        mapM_
          ( \op@(OperationClause cName args body') -> addStackTrace ("inferring " ++ pretty op) do
              let Arrow sigArgs sigRet = signatures Map.! cName
                  env' =
                    insertVar
                      (Ident "resume" LocNone)
                      (TypeScheme [] [] $ CompType rowEmpty (TypeArrow $ Arrow [sigRet] toC'))
                      env
               in inferFunctionLike env' args sigArgs (Just toC') body'
          )
          clauses

      fromV' <- subM fromV
      pure $ TypeHandler eff fromV' to
      where
        clauseNames = map (\(OperationClause x _ _) -> x) clauses
        matchesClauses (Effect _ signatures) = Set.fromList clauseNames == Set.fromList (Map.keys signatures)
    Elb (Elaboration name row clauses) -> do
      -- Find the matching effect if available
      let maybeEffect = Map.lookup name (view effects env)
      -- TODO: the name must be resolved to the full path to the effect to avoid conflicts
      eff@(Effect _ sigs) <- case maybeEffect of
        Just a -> return a
        Nothing -> throwError "could not match elaboration with an effect"

      -- Match up the clauses and signatures
      let clauses' = sortOn (\(OperationClause x _ _) -> x) clauses
      let sigs' = sortOn fst (Map.toList sigs)

      row' <- resolveRow env row
      () <-
        mapM_
          ( \(OperationClause cName args body', (sName, Arrow sigArgs sigRet)) -> do
              when (cName /= sName) $ throwError "clauses do not match signatures in elaboration"
              let CompType retRow retVal = sigRet
              when (retRow /= rowEmpty) $ throwError "ICE signature cannot have a row on the return type"
              inferFunctionLike env args sigArgs (Just $ CompType row' retVal) body'
          )
          (zip clauses' sigs')
      pure $ TypeElaboration eff row'
    _ -> error "Not implemented yet"

inferFunctionLike :: TypeEnv -> [Ident] -> [CompType] -> Maybe CompType -> Expr -> Infer CompType
inferFunctionLike env argNames argTypes tRet body = do
  -- Any type vars in the signature need to be bound inside
  -- so we need to extract them
  let typeVars = Set.unions $ map (freeTypeVars env) argTypes

  -- Argument must be used
  let argSchemes = map (TypeScheme [] []) argTypes

  () <-
    when (length argNames /= length argSchemes) $
      throwError "function does not have the right number of arguments"

  -- The body needs the variables and the bound type vars
  let bodyEnv = addTypeVars typeVars $ extendVars (zip argNames argSchemes) env

  tRetInferred <- infer bodyEnv body
  () <- mapM_ (force tRetInferred) tRet

  argTypes' <- mapM subM argTypes
  tRet' <- subM tRetInferred
  pure $ TypeArrow $ Arrow argTypes' tRet'

findSig :: Ident -> [OperationSignature] -> OperationSignature
findSig x sig = fromJust $ find (\(OperationSignature y _ _) -> y == x) sig

openRow :: CompType -> Infer CompType
openRow (CompType row t) = f row >>= \x -> return $ CompType x t
  where
    f (Row effs Nothing) = Row effs . Just <$> fresh
    f r = return r
