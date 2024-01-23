{-# LANGUAGE FlexibleInstances #-}
module Typings.Debug where

import Control.Lens

import qualified Data.Text as T
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.FuzzySet.Simple as Fuzz
import qualified Data.Set as S
import Data.Maybe

import Typings.Env
import Data.List
import qualified Typings.Types as Type

import Utils.Similarity
import Reporting.Errors.Position
import Reporting.Errors.Base as Errors

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

formatInternalErrorContext :: String -> Errors.DebugContextMarker
formatInternalErrorContext msg = Errors.MarkMultiple "Internals context" [Errors.MarkNothing msg]

formatFunctionContext :: TypeCheckerEnv -> Errors.DebugContextMarker
formatFunctionContext env = case env^.currentFunction of
    Nothing -> Errors.NoMarker
    (Just fn) -> let p = Type.location fn in Errors.MarkMultiple "Function context" [Errors.MarkSegment ("The error occurred in function '" ++ Type.stringName fn ++ "'") [(p, p, "Location of function '" ++ Type.stringName fn ++ "'")]]

formatInferenceTrace :: TypeCheckerEnv -> Errors.DebugContextMarker
formatInferenceTrace env =
    if null $ env^.inferTrace.inferStack then Errors.NoMarker else maybe NoMarker id $ formatTraceFrom (env^.inferTrace) (last $ env^.inferTrace.inferStack)
    where
        formatTraceFrom :: InferTrace -> Position -> Maybe Errors.DebugContextMarker
        formatTraceFrom tr currentRoot =
            let (endPositions, _) = measureEndPos tr Undefined (S.singleton currentRoot) currentRoot in
            case filterTraceLayers Nothing $ constructTraceLayers tr [currentRoot] (S.singleton currentRoot) [] of
                rawTraceLayers@(((Position _ expectedFileName _ _, _):_):_) ->
                    let (traceLayers, limitMsg) = limitLayers $ filterTraceLayers (Just expectedFileName) rawTraceLayers in
                    if null traceLayers then Nothing else (
                        let markers = map (normalizeTraceLayer tr endPositions) traceLayers in
                        let allMarkers = markers ++ (maybe [] (\msg -> [MarkNothing msg]) limitMsg) in
                        Just $ MarkMultiple ("Type context") $ allMarkers
                    )
                _ -> Nothing
        limitLayers :: [[(Position, Type.Type)]] -> ([[(Position, Type.Type)]], Maybe String)
        limitLayers layers =
            let count = length layers in
            case layers of
                [] -> ([], Nothing)
                _ | count > 0 && count <= 5 -> (layers, Nothing)
                _ | count > 5 && count <= 10 -> ([head layers, layers!!2, layers!!4, last layers], Nothing)
                _ ->
                    let m | count > 30 = div count 8
                          | count > 20 = div count 6
                          | otherwise = div count 3 in
                    let filteredLayers = [head layers] ++ (map snd . filter (\(x,y) -> (mod x m) == 0 && x /= 1 && x /= count) . zip [1..] $ layers) ++ [last layers] in
                    let diffCount = count - length filteredLayers in
                    let message = "There was additional " ++ (show diffCount) ++ " intermediate inference steps made. They're not shown, because that would be too clunky to display." in
                    (filteredLayers, if diffCount > 2 then Just message else Nothing)
                    --(filteredLayers, Just $ " " ++ (show count) ++ " -> " ++ (show $ length filteredLayers) ++ " by " ++ (show m))

        normalizeTraceLayer :: InferTrace -> M.Map Position Position -> [(Position, Type.Type)] -> Errors.DebugContextMarker
        normalizeTraceLayer tr endPositions layer =
            Errors.MarkSegment "Infer types: " $ map (\(p, t) -> (p, M.findWithDefault p p endPositions, "Type: " ++ (printi 0 t))) layer
        filterTraceLayers :: (Maybe String) -> [[(Position, Type.Type)]] -> [[(Position, Type.Type)]]
        filterTraceLayers expectedFileName = filter (not . null) . map (reverse . sort . filter (isPositionFrom expectedFileName . fst) . M.toList . M.fromList)
        constructTraceLayers :: InferTrace -> [Position] -> S.Set Position -> [[(Position, Type.Type)]] -> [[(Position, Type.Type)]]
        constructTraceLayers _ [] _ layers = layers
        constructTraceLayers tr currentLayer visited layers =
            let nextLayer = filter (not . (flip S.member) visited) $ concatMap (\e -> M.findWithDefault [] e $ tr^.inferChildren) currentLayer in
            let shouldAddLayer = length nextLayer /= length currentLayer in
            let fullNextLayer = mapMaybe (\e -> (\t -> Just (e, t)) =<< (M.lookup e $ tr^.inferTypes)) nextLayer in
            let newLayers = (if shouldAddLayer then (fullNextLayer:layers) else layers) in
            constructTraceLayers tr nextLayer (S.union visited $ S.fromList nextLayer) newLayers
        measureEndPos :: InferTrace -> Position -> S.Set Position -> Position -> ((M.Map Position Position), Position)
        --measureEndPos tr parent pos = (M.fromList [(parent, pos)], pos)
        measureEndPos tr parent visited pos =
            let children = filter (not . (flip S.member) visited) $ M.findWithDefault [] pos $ tr^.inferChildren in
            if length children == 0 then (M.fromList [(parent, pos)], pos) else (
                let subresults = map (measureEndPos tr pos (S.union visited $ S.fromList children)) children in
                let currentEndPos = maximum $ map snd subresults in
                let allResults = M.unions $ map fst subresults in
                (M.insert parent currentEndPos allResults, currentEndPos)
            )

            --map (masureEndPos tr) $ M.findWithDefault [] pos tr^.inferChildren

--a^.file