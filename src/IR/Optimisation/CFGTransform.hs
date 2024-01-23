module IR.Optimisation.CFGTransform (inlineTrivialBlocks, removeUnreachable) where

import           Data.List
import qualified Data.Map                 as Map
import qualified Data.Set                 as Set
import           IR.Flow.CFG
import           IR.Syntax.Syntax
import           IR.Utils

import Control.Lens hiding (Const)

import Reporting.Logs



removeUnreachable :: CFG a d -> Method a -> (CFG a (), Method a)
removeUnreachable cfg_ (Mthd pos t qi ps _) =
    let mthd = Mthd pos t qi ps (map (fmap fst) $ linearise cfg_)
    in  (cfg mthd, mthd)

inlineTrivialBlocks :: CFG a d -> LattePipeline (CFG a d)
inlineTrivialBlocks = fixpointByM (\(CFG g) -> Map.keys g) collapseOnce


collapseOnce :: CFG a d -> LattePipeline (CFG a d)
collapseOnce cfg_@(CFG g) = do
    let edges = cfgEdges cfg_
    let eligibleEdge = find (\(v, u) -> uniqueOutgoing v && uniqueIncoming u) edges
    printLogInfoStr $ "collapseOnce: Try collapsing CFG nodes in CFG [\n" ++ (show cfg_) ++ "\n]"
    case eligibleEdge of
        Just (v, u) -> do
            -- There is edge v -> u -> [w] = ws
            printLogInfoStr $ "collapseOnce: Collapse eligible edge " ++ (show v) ++ " == to => " ++ (show u)
            let v' = u `inlineInto` v
            let g' = Map.insert (v' ^. nodeLabel) v' $ Map.delete (u ^. nodeLabel) g
            let ws = map snd $ filter (\(u', w') -> u' ^. nodeLabel == u ^. nodeLabel) edges
            let ws' = map (\w -> w & nodeIn %~ (Set.insert (v' ^. nodeLabel) . Set.delete (u ^. nodeLabel))) ws
            let g'' = foldl (\g' w' -> Map.insert (w' ^.nodeLabel) w' g') g' ws'
            return $ CFG g''
        Nothing     -> return cfg_

cfgEdges :: CFG a d -> [(Node a d, Node a d)]
cfgEdges cfg@(CFG g) =
    concatMap (\v -> map (\ui -> (v, lookupCFGNode cfg ui)) (Set.elems $ v ^. nodeOut)) (Map.elems g)


uniqueOutgoing :: Node a d -> Bool
uniqueOutgoing n = Set.size (n ^. nodeOut) == 1

uniqueIncoming :: Node a d -> Bool
uniqueIncoming n = Set.size (n ^. nodeIn) == 1

inlineInto :: Node a d -> Node a d -> Node a d
inlineInto u v =
    let vInstrs = filter (\i -> (fmap (const ()) i) /= IJmp () (u ^. nodeLabel)) (v ^. nodeBody)
        uInstrs = dropWhile isLabel (u ^. nodeBody)
    in v & nodeBody .~ (vInstrs ++ uInstrs) & nodeOut .~ (u ^. nodeOut)
