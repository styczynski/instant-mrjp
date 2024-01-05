{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
module Typings.Env where

import Control.Lens

import Utils.Similarity
import qualified Program.Syntax as Syntax
import qualified Typings.Types as Type
import qualified Data.Text as T
import qualified Data.FuzzySet.Simple as Fuzz
import Reporting.Errors.Position
import Data.Tuple.Append
import Data.List
import Data.Maybe

import Control.Monad.Except hiding (void)
import Control.Monad.Reader hiding (void)
import qualified Data.Map as M
import qualified Data.List.NonEmpty as NE
import qualified Utils.Graphs as G
import Control.DeepSeq

import Control.Monad.Trans.Class

data Inheritance = ClassExtends String String deriving (Show)
data Hierarchy = Hierarchy (G.Graph String Type.Class Inheritance) (M.Map String [Type.Class])

type FunctEnv = M.Map String Type.Function

type ClassEnv = M.Map String Type.Class
type VarEnv = M.Map String (Type.Name, Type.Type)

--data InferDebug = InferDebug (M.Map Position (Type.Type, [Position])) ([Position])

data InferTrace = InferTrace
  {
    _infInferTypes :: M.Map Position Type.Type
    , _infInferChildren :: M.Map Position [Position]
    , _infInferStack :: [Position]
  } deriving (Show)

data TypeCheckerEnv = TypeCheckerEnv
  {
  _teDefinedFuns      :: FunctEnv
  , _teDefinedClasses   :: Hierarchy
  , _teDefinedFunsFuzz :: Fuzz.FuzzySet
  , _teCurrentScopeName :: Maybe String
  , _teCurrentClass     :: Maybe Type.Class
  , _teCurrentFunction  :: Maybe Type.Function
  , _teClassEnv         :: ClassEnv
  , _teLoc              :: Maybe Position
  , _teRetType          :: Maybe Type.Type
  , _teCurrentScopeVars :: VarEnv
  , _teParentScopes     :: [(Position, VarEnv)]
  , _tePreviousScopes   :: [(Position, VarEnv)]
  , _teInferTrace     :: InferTrace
  }

instance NFData (TypeCheckerEnv) where rnf x = seq x ()

makeLensesWith abbreviatedFields ''TypeCheckerEnv
makeLensesWith abbreviatedFields ''InferTrace

instance Show TypeCheckerEnv where
  show env =
    let parentScopesStr = intercalate "\n" $ map (showScope 1 "[Parent scope]") (env^.parentScopes) in
    "TypeCheckerEnv:\n" ++ parentScopesStr ++ "\n" ++ (showScope 1 "[Current scope]" $ (Undefined, env^.currentScopeVars))
    where
      showVar :: String -> Type.Name -> Type.Type -> String
      showVar name id varType =
        "'" ++ name ++ "' declared at " ++ (show id) ++ " :: " ++ (printi 0 varType)
      showScope :: Int -> String -> (Position, VarEnv) -> String
      showScope i label (pos, env) =
        let indent = concat $ replicate i "  " in
        let scopeStr = intercalate ("  \n" ++ indent) $ map (\(name, (id, varType)) -> "  " ++ showVar name id varType) $ M.assocs env in
        indent ++ "Scope " ++ label ++ " defined at " ++ show pos ++ " {\n" ++ indent ++ scopeStr ++ "\n" ++ indent ++ "}"
        --M.foldWithKey (\name (id, varType) -> showVar name id varType) "" env

initialTrace :: InferTrace 
initialTrace = InferTrace {
  _infInferTypes = M.empty
  , _infInferChildren = M.empty
  , _infInferStack = []
}

initialEnv :: TypeCheckerEnv
initialEnv = TypeCheckerEnv
  { _teDefinedFuns      = M.empty
  , _teDefinedClasses   = Hierarchy G.empty M.empty
  ,  _teDefinedFunsFuzz = Fuzz.defaultSet
  , _teCurrentScopeName = Nothing
  , _teCurrentClass     = Nothing
  , _teCurrentFunction  = Nothing
  , _teClassEnv         = M.empty
  , _teLoc              = Nothing
  , _teRetType          = Nothing
  , _teCurrentScopeVars = M.empty
  , _teParentScopes     = []
  , _tePreviousScopes   = []
  , _teInferTrace       = initialTrace
  }


findFunction :: TypeCheckerEnv -> String -> Maybe Type.Function
findFunction env = (flip M.lookup) (env^.definedFuns)

findClassInEnv :: TypeCheckerEnv -> String -> Maybe Type.Class
findClassInEnv env = let (Hierarchy g _) = env^.definedClasses in G.getNode g

findMember :: TypeCheckerEnv -> String -> Maybe Type.Member
findMember env name = (Type.findClassMember name) =<< env^.currentClass

findClassInheritanceChain :: TypeCheckerEnv -> String -> Maybe [Type.Class]
findClassInheritanceChain env className = 
  let (Hierarchy _ m) = env^.definedClasses in M.lookup className m

findMemberOf :: TypeCheckerEnv ->  Bool -> String -> String -> Either String (Maybe (Type.Class, Type.Member))
findMemberOf env isOptional classId memberId = do
    chain <- maybe (Left "No such class") return $ findClassInheritanceChain env classId
    cls <- maybe (Left "No such class") return $ listToMaybe chain
    maybe (if not isOptional then Left "No such member" else return $ Nothing) (\member -> return $ Just $ (cls, member)) $ listToMaybe $ mapMaybe (Type.findClassMember memberId) chain

setupDefEnv :: FunctEnv -> ClassEnv -> Hierarchy -> TypeCheckerEnv -> TypeCheckerEnv
setupDefEnv fns cls classHierarchy env =
    env {
        _teDefinedFuns = fns
        , _teDefinedFunsFuzz = Fuzz.fromList $ map T.pack $ M.keys fns
        , _teDefinedClasses = classHierarchy
    }


--addVar :: Type.Name -> Type.Type -> TypeCheckerEnv -> TypeCheckerEnv
--addVar name t env = env & currentScopeVars %~ M.insert (Type.stringName name) (name, t)

addVar :: Type.Name -> Type.Type -> TypeCheckerEnv -> Either (Type.Name, Type.Name, Type.Type) TypeCheckerEnv
addVar name t env = (\(old, newEnv) -> maybe (Right newEnv) (Left . (<++) name) $ M.lookup (Type.stringName name) old) $ env & currentScopeVars <<%~ M.insert (Type.stringName name) (name, t)

--addVar name t env = (\(old, newEnv) -> maybe (Right newEnv) (\(prevName, prevType) -> Left (name, prevName, prevType)) $ M.lookup (Type.stringName name) old) $ env & currentScopeVars <<%~ M.insert (Type.stringName name) (name, t)

lookupVar :: String -> TypeCheckerEnv -> Maybe (Type.Name, Type.Type)
lookupVar name env = listToMaybe $ mapMaybe (M.lookup name) ([env^.currentScopeVars] ++ (map snd $ env^.parentScopes))

separateScope :: Position -> TypeCheckerEnv -> TypeCheckerEnv
separateScope pos env = env
  & parentScopes %~ (:) (pos, env^.currentScopeVars)
  & currentScopeVars .~ M.empty

revertScope :: TypeCheckerEnv -> TypeCheckerEnv
revertScope env = let currentParentScopes = env^.parentScopes in env
  & previousScopes %~ (:) (fst $ head currentParentScopes, env^.currentScopeVars)
  & parentScopes .~ (tail currentParentScopes)
  & currentScopeVars .~ (snd $ head currentParentScopes)


-- data InferTrace = InferTrace
--   {
--     _infTypes :: M.Map Position Type.Type
--     , _infChildren :: M.Map Position [Position]
--     , _infStack :: [Position]
--   }

inferTraceEnter :: Position -> TypeCheckerEnv -> TypeCheckerEnv
inferTraceEnter pos env =
  env
  & inferTrace . inferChildren %~ (\p -> maybe p (mapAppend pos p) $ listToMaybe $ env^.inferTrace.inferStack)
  & inferTrace . inferStack %~ (:) pos
  where
    mapAppend :: (Ord k) => a -> M.Map k [a] -> k -> M.Map k [a]
    mapAppend val m key = M.insert key (val:M.findWithDefault [] key m) m

inferTraceQuit :: Position -> Type.Type -> TypeCheckerEnv -> TypeCheckerEnv
inferTraceQuit pos varType env =
  env
  & inferTrace . inferTypes %~ M.insert pos varType
  & inferTrace . inferStack %~ (\stack -> if length stack <= 1 then stack else tail stack)

