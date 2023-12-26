{-# LANGUAGE FlexibleInstances #-}
module Typings.Debug where

import Control.Lens

import qualified Data.Text as T
import qualified Data.Map as M
import qualified Data.FuzzySet.Simple as Fuzz
import qualified Data.Set as S
import Data.Maybe

import Typings.Env
import Data.List
import qualified Typings.Types as Type

import Utils.Similarity
import Reporting.Errors.Position

data QueryFn = QueryFn String (Maybe (Type.Type, [Type.Type]))
data QueryVar =
    QueryVarExact String
    | QueryVar String

class TCEnvQuerable q r | q -> r where
    query :: TypeCheckerEnv -> q -> [r]
    (<--?) :: TypeCheckerEnv -> q -> [r]
    (<--?) = query

instance TCEnvQuerable QueryFn Type.Function where
    query env q@(QueryFn name _) = filter (matchFn q) $ mapMaybe ((findFunction env) . T.unpack . snd) $ Fuzz.find (T.pack name) (env^.definedFunsFuzz)
        where
            matchFn :: QueryFn -> Type.Function -> Bool
            matchFn (QueryFn _ (Just (ret, args))) fn@(Type.Fun _ fnRet fnArgs _) = (similar ret fnRet) && (similar args $ Type.funcArgsTypes fn)
            matchFn _ _ = True

instance TCEnvQuerable QueryVar (Bool, Position, Type.Name, Type.Type) where
    query env query = case query of
        (QueryVarExact name) -> exactQuery env name
        (QueryVar name) ->
            let fuzzNames = fuzzyQuery env name in
            (S.toList . S.fromList) $ concatMap (exactQuery env) fuzzNames
        where
            exactQuery :: TypeCheckerEnv -> String -> [(Bool, Position, Type.Name, Type.Type)]
            exactQuery env name =
                let mapVar flag pos = Just . (uncurry $ (,,,) flag pos) in
                let currentVars = maybeToList $ mapVar True Undefined =<< (M.lookup name $ env^.currentScopeVars) in
                let previousVars = mapMaybe (\(start, scope) -> mapVar False start =<< M.lookup name scope) (env^.previousScopes) in
                currentVars ++ previousVars
            fuzzyQuery :: TypeCheckerEnv -> String -> [String]
            fuzzyQuery env name = 
                let allVarsFuzz = Fuzz.fromList $ concatMap (map (T.pack . Type.stringName . fst) . M.elems) ([env^.currentScopeVars] ++ (map snd $ env^.previousScopes)) in
                map (T.unpack . snd) $ Fuzz.find (T.pack name) allVarsFuzz

printVars :: String -> [(Type.Name, Type.Type)] -> String
printVars header vars = showVars 0 header vars
    where
        showVars :: Int -> String -> [(Type.Name, Type.Type)] -> String
        showVars i label vars =
            let indent = concat $ replicate i "  " in
            let varsStr = intercalate ("  \n" ++ indent) $ map (\(id, varType) -> "  " ++ showVar id varType) vars in
            indent ++ label ++ "\n" ++ indent ++ varsStr ++ "\n"
        showVar :: Type.Name -> Type.Type -> String
        showVar id varType =
            "'" ++ Type.stringName id ++ "' declared at " ++ (show id) ++ " :: " ++ (printi 0 varType)

--a^.file