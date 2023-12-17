module Typings.InheritanceHierarchy where

import qualified Data.Map as M
import qualified Data.Key as K
import qualified Data.Text as T
import Reporting.Errors.Def as Errors
import Data.Maybe
import Data.List
import Data.Tuple
import qualified Typings.Types as Type
import qualified Program.Syntax as Syntax
import qualified StackedDag.Base as DagPrinter

import qualified Utils.Graphs as G

import Typings.Def
import Reporting.Logs
import Language.Haskell.TH.Lens (HasName(name))

data Inheritance = ClassExtends String String deriving (Show)
data Hierarchy = Hierarchy (G.Graph String Type.Class Inheritance)

-- getParentEdges_ :: M.Map String Int -> Int -> String -> Type.Class -> [(Int, Int, Inheritance)]
-- getParentEdges_ ids selfId selfName class =
--     catMaybes $ map (\parentName -> (\parentId -> (selfId, parentId, Inheritance selfName parentName)) <$> lookup parentName ids) $ getParents class

-- getClassEdges :: M.Map String Int -> (Int, String, Type.Class) -> [Edge]
-- getClassEdges ids (selfId, selfName, selfClass) =
--     catMaybes $ map (\parentName -> (\parentId -> (selfId, parentId, ClassExtends selfName parentName)) <$> M.lookup parentName ids) $ getParents selfClass

-- getClassNode :: M.Map String Int -> (Int, String, Type.Class) -> Node
-- getClassNode _ (selfId, selfName, _) = (selfId, selfName)

constructInheritanceHierarchy :: M.Map String Type.Class -> TypeChecker Hierarchy
constructInheritanceHierarchy classMap = do
    liftPipelineTC $ printLogInfo $ "Construct inheritance graph"
    case G.create edgeConstructor edgeLabeller classMap of
        g@(G.Graph _ _ _) -> return $ Hierarchy g
        G.UnknownNodeOnEdge _ cls parent -> failure $ Errors.UnknownParent cls parent
    where
        edgeConstructor :: Type.Class -> [String]
        edgeConstructor (Type.Class _ (Syntax.Name _ (Syntax.Ident _ name)) _ _) = [name]
        edgeConstructor (Type.Class _ (Syntax.NoName _) _ _) = []

        edgeLabeller :: (String, Type.Class) -> (String, Type.Class) -> Inheritance
        edgeLabeller (name1, cls1) (name2, cls2) = ClassExtends name1 name2

-- getFragmentNode :: (Int, String, [(Int, String)], [(Int,String)]) -> (Int, String)
-- getFragmentNode (newId, name, _, skips) = 
--     case skips of
--         [] -> (newId, " " ++ name)
--         l -> (newId, name ++ " extends cyclically " ++ (concat $ map (\(_, name) -> name ++ " ") skips))

-- getFragmentEdge :: (Int, String, [(Int, String)], [(Int,String)]) -> (Int, [Int])
-- getFragmentEdge (newId, name, targets, skips) =
--     case skips of
--         [] -> (newId, map (\(i, _) -> i) targets)
--         _ -> (newId, map (\(i, _) -> i) targets)

-- enrichFragment :: Hierarchy -> [Int] -> ([Int], Int) -> Int -> ([Int], Int)
-- enrichFragment (Hierarchy ids graph) sccNodes (allNodes, prevNode) currentNode =
--     (allNodes ++ (filter (\e -> not $ elem e sccNodes ) $ G.pre graph currentNode) ++ [currentNode], currentNode)

-- formatFragment :: Hierarchy -> String -> LattePipeline String
-- formatFragment h@(Hierarchy ids graph) start = do
--     bfsNodes <- return $ let (Just id) = M.lookup start ids in  BFS.bfs id graph
--     printLogInfo $ T.pack $ show $ map (\id -> let (Just name) = G.lab graph id in name) bfsNodes
--     (bfsNodes, _) <- return $ foldl (enrichFragment h bfsNodes) ([], -1) bfsNodes
--     printLogInfo $ T.pack $ show $ map (\id -> let (Just name) = G.lab graph id in name) bfsNodes
--     newIds <- return $ map (\(newId, id) -> let (Just name) = G.lab graph id in (newId, id, name)) $ zip [0..] bfsNodes
--     newMapping <- return $ M.fromList $ map (\(newId, id, name) -> (id, newId)) newIds
--     allEdges <- return $ map (\(newId, id, name) -> let sucs = map (\oldId -> let (Just id) = M.lookup oldId newMapping in let (Just sucName) = G.lab graph oldId in (id, sucName)) (G.suc graph id) in (newId, name, filter (\(i, n) -> i > newId) sucs, filter (\(i, n) -> i <= newId) sucs)) newIds
--     (conflictId, _, _, _) <- return $ head $ filter (\(_, _, _, s) -> length s > 0) allEdges
--     nodes <- return $ map getFragmentNode allEdges
--     edges <- return $ filter (\(_, l) -> length l > 0) $ map getFragmentEdge allEdges
--     edges <- return $ (let (id, e) = head edges in [(id, e ++ [conflictId])]) ++ (tail edges)
--     printLogInfo $ T.pack $ show (nodes, edges)
--     return $ DagPrinter.edgesToText (DagPrinter.mkLabels nodes) (DagPrinter.mkEdges edges)

getErrorOnCycle :: Hierarchy -> [String] -> Maybe Errors.Error
getErrorOnCycle _ [] = Nothing
getErrorOnCycle (Hierarchy graph) [clsName] = Errors.CyclicInheritanceSelf <$> G.getNode graph clsName
getErrorOnCycle h@(Hierarchy graph) classNames@(firstClassName:_) = 
    case G.getNodes graph classNames of
        firstCls : pathCls -> return $ Errors.CyclicInheritance firstCls pathCls (G.prettyFragment graph printNode firstClassName)
        _ -> Nothing
    where 
        printNode :: String -> Type.Class -> [(String, Type.Class)] -> String
        printNode name cls skips = 
            case skips of
                [] -> name
                l -> name ++ " extends cyclically " ++ (concat $ map (\(name, _) -> name ++ " ") skips)


checkLoops :: Hierarchy -> M.Map String Type.Class -> LattePipeline (Maybe Errors.Error)
checkLoops h@(Hierarchy graph) cls = do
    printLogInfo $ "Resolve cycles ..."
    return $ (getErrorOnCycle h) =<< G.cycles graph
    -- cycle <- return $ listToMaybe $ sortBy (\a b -> compare (length a) (length b)) $ filter ((> 1) . length) $ DFS.scc graph
    -- case (z :: Maybe [G.Node]) of
    --     Nothing -> return Nothing
    --     Just nodes | length nodes > 0 -> do
    --         printLogInfo $ "Print ..."
    --         --msg <- return $ G.prettify $ G.subgraph nodes graph
    --         msg <- formatFragment h (let (Just name) = G.lab graph (last nodes) in name)
    --         (firstCls: pathCls) <- return $ map (\id -> let (Just name) = G.lab graph id in let (Just cl) = M.lookup name cls in cl) nodes
    --         return $ Just $ Errors.CyclicInheritance firstCls pathCls msg
    --     Just _ -> return Nothing