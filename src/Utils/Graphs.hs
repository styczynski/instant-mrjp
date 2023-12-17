module Utils.Graphs(
    create
    , Graph(..)
    , getNodes
    , prettyFragment
    , getNode
    , cycles
) where

import qualified Data.Graph.Inductive.Graph as G
import qualified Data.Graph.Inductive.PatriciaTree as G
import qualified Data.Graph.Inductive.Query.DFS as DFS
import qualified Data.Graph.Inductive.Query.BFS as BFS

import qualified StackedDag.Base as DagPrinter

import Data.Maybe
import Data.List
import Data.Tuple
import qualified Data.Map as M
import qualified Data.Key as K
import qualified Data.Text as T

type Node nodeL = (Int, nodeL)
type Edge edgeL = (Int, Int, edgeL)
type NodeContext edgeL = ([(edgeL, Int)], Int, [(edgeL, Int)])

type EdgeConstructor key nodeL = nodeL -> [key]
type EdgeLabeller key nodeL edgeL = (key, nodeL) -> (key, nodeL) -> edgeL

type FragmentNodePrinter key nodeL = (key -> nodeL -> [(key, nodeL)] -> String)

data (Ord key) => Graph key nodeL edgeL =
    UnknownNodeOnEdge key nodeL key
    | Graph (M.Map key nodeL) (M.Map key Int) (G.Gr key edgeL)

assignIds :: M.Map key nodeL -> [(Int, key, nodeL)]
assignIds = zipWith (curry (\(id, (key, node)) -> (id, key, node))) [0..] . M.toList

createNodeEdges :: (Ord key) => M.Map key nodeL -> M.Map key Int -> EdgeConstructor key nodeL -> EdgeLabeller key nodeL edgeL -> (Int, key, nodeL) -> [Edge edgeL]
createNodeEdges nodeMap nodeIds getNodeSucs getEdgeLabel (id, key, node) =
    --catMaybes =<< map (\childKey -> (\childId -> (id, )) =<< (M.lookup childKey nodeIds)) =<< getNodeSucs <$> (M.lookup key nodeMap) -- Maybe [Int]
    mapMaybe (\childKey -> (\childId childNode -> (id, childId, getEdgeLabel (key, node) (childKey, childNode))) <$> M.lookup childKey nodeIds <*> M.lookup childKey nodeMap) (getNodeSucs node)

create :: (Ord key) => EdgeConstructor key nodeL -> EdgeLabeller key nodeL edgeL -> M.Map key nodeL -> Graph key nodeL edgeL
create getNodeSucs getEdgeLabel nodeMap =
    case checkSucs getNodeSucs nodeMap of
        (Just (key, node, childKey)) -> UnknownNodeOnEdge key node childKey
        Nothing ->
            let allNodes = assignIds nodeMap in
            let nodeIds = M.fromList $ map (\(i, k, t) -> (k, i)) allNodes in
            let edges = concatMap (createNodeEdges nodeMap nodeIds getNodeSucs getEdgeLabel) allNodes in
            Graph nodeMap nodeIds (G.mkGraph (map (\(i, k, t) -> (i, k)) allNodes) edges)
    where
        checkSucs :: (Ord key) => EdgeConstructor key nodeL -> M.Map key nodeL -> Maybe (key, nodeL, key)
        checkSucs getNodeSucs nodeMap =
            find (\(_, _, childName) -> not $ M.member childName nodeMap) $ M.foldrWithKey (\key node acc -> acc ++ (map (\childName -> (key, node, childName)) $ getNodeSucs node)) [] nodeMap
            
            --map (\(key, node) -> (key, node, getNodeSucs node)) $ M.toList nodeMap

-- lookupNode :: Graph key nodeL edgeL -> Int -> nodeL
-- lookupNode (Graph nodeMap nodeIds graph) id = ((flip M.lookup) ) =<< G.lab graph id

prettyFragment :: (Ord key) => Graph key nodeL edgeL -> (FragmentNodePrinter key nodeL) -> key -> String
prettyFragment g@(Graph nodeMap nodeIds graph) nodePrinter startKey =
    let bfsNodes = ((flip BFS.bfs graph <$> M.lookup startKey nodeIds) :: Maybe [Int]) in
    let fragmentNodes = ((zip [0..] <$> fst) . foldl (enrichFragment g bfsNodes) ([], -1) <$> bfsNodes) in
    let newIds = (mapMaybe (\(newId, id) -> ((,,) newId id) <$> G.lab graph id) <$> fragmentNodes) in
    let newMapping = M.fromList . map swap <$> fragmentNodes in
    -- filter (\(i, n) -> i > newId) sucs, filter (\(i, n) -> i <= newId) sucs
    let allEdges = map (getEdges g newMapping) <$> newIds in
    let conflictId = (listToMaybe . map (\(a, _, _, _) -> a) . filter (\(_, _, _, s) -> length s > 0) =<< allEdges) in
    let printerNodes = (DagPrinter.mkLabels . map (getNodeDescription g nodePrinter) <$> allEdges) in
    let edges = (filter (\(_, l) -> length l > 0) . map (\(newId, _, targets, _) -> (newId, map fst targets)) <$> allEdges) in
    let printerEdges = (DagPrinter.mkEdges . mapHead (\(id, e) -> (\c -> (id, e ++ [c])) <$> conflictId) <$> edges) in
    fromMaybe "" $ DagPrinter.edgesToText <$> printerNodes <*> printerEdges
    where
        mapHead :: (a -> Maybe a) -> [a] -> [a]
        mapHead _ [] = []
        mapHead fn l = case fn $ head l of
            Nothing -> l
            (Just e) -> e:tail l

        getEdges :: (Ord key) => Graph key nodeL edgeL -> Maybe (M.Map Int Int) -> (Int, Int, key) -> (Int, key, [(Int, key)], [(Int, key)])
        getEdges (Graph nodeMap nodeIds graph) (Just newMapping) (newId, id, name) =
            let sucs = mapMaybe (\oldId -> (,) <$> M.lookup oldId newMapping <*> G.lab graph oldId) (G.suc graph id) in
            let targets = filter (\(i, n) -> i > newId) sucs in
            let skips = filter (\(i, n) -> i <= newId) sucs in
            (newId, name, targets, skips)
        getEdges _ _ (newId, id, name) = (newId, name, [], [])

        enrichFragment :: (Ord key) => Graph key nodeL edgeL -> Maybe [Int] -> ([Int], Int) -> Int -> ([Int], Int)
        enrichFragment (Graph _ _ graph) (Just sccNodes) (allNodes, prevNode) currentNode =
            (allNodes ++ filter (\e -> notElem e sccNodes ) (G.pre graph currentNode) ++ [currentNode], currentNode)
        enrichFragment _ _ acc _ = acc

        getNodeDescription ::  (Ord key) => Graph key nodeL edgeL -> (FragmentNodePrinter key nodeL) -> (Int, key, [(Int, key)], [(Int, key)]) -> (Int, String)
        getNodeDescription g@(Graph _ _ graph) nodePrinter (newId, name, _, skips) =
            --  (key -> nodeL -> [(String, nodeL)] -> String)
            case getNode g name of
                Nothing -> (newId, " ")
                (Just node) -> (newId, " " ++ (nodePrinter name node (zip (map snd skips) (getNodes g (map snd skips)))))


    -- nodes <- return $ map getFragmentNode allEdges
    -- edges <- return $ filter (\(_, l) -> length l > 0) $ map getFragmentEdge allEdges
    -- edges <- return $ (let (id, e) = head edges in [(id, e ++ [conflictId])]) ++ (tail edges)
    -- printLogInfo $ T.pack $ show (nodes, edges)
    -- return $ DagPrinter.edgesToText (DagPrinter.mkLabels nodes) (DagPrinter.mkEdges edges)

getNode :: (Ord key) => Graph key nodeL edgeL -> key -> Maybe nodeL
getNode (Graph nodeMap nodeIds graph) = flip M.lookup nodeMap

getNodes :: (Ord key) => Graph key nodeL edgeL -> [key] -> [nodeL]
getNodes graph = mapMaybe (getNode graph)

    --catMaybes . (map $ G.lab graph)

cycles :: (Ord key) => Graph key nodeL edgeL -> Maybe [key]
cycles g@(Graph nodeMap nodeIds graph) =
    let scss = map (mapMaybe (G.lab graph)) $ sortBy (\a b -> compare (length a) (length b)) $ filter ((> 1) . length) $ DFS.scc graph in
    let loops = mapMaybe (((\e -> Just [e]) =<<) . G.lab graph) (filter (\id -> G.hasEdge graph (id, id)) $ G.nodes graph) in
    listToMaybe $ scss ++ loops
