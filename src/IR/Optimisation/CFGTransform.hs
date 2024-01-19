module IR.Optimisation.CFGTransform (inlineTrivialBlocks, removeUnreachable) where

import           Data.List
import qualified Data.Map                 as Map
import qualified Data.Set                 as Set
import           IR.Flow.CFG
import           IR.Syntax.Syntax
import           IR.Utils

import Control.Lens hiding (Const)

-- Some labels and jumps between them might be trivial and unnecessary,
-- namely when there are node (v, u) such that v -> u and there are no
-- other outgoing edges from v or incoming edges into u, they can be
-- collapsed to a single node.
inlineTrivialBlocks :: CFG a d -> CFG a d
inlineTrivialBlocks = fixpointBy (\(CFG g) -> Map.keys g) collapseOnce

removeUnreachable :: CFG a d -> Method a -> (CFG a (), Method a)
removeUnreachable cfg_ (Mthd pos t qi ps _) =
    let mthd = Mthd pos t qi ps (map (fmap fst) $ linearise cfg_)
    in  (cfg mthd, mthd)

collapseOnce :: CFG a d -> CFG a d
collapseOnce cfg_@(CFG g) =
    let edges = cfgEdges cfg_
        eligibleEdge = find (\(v, u) -> uniqueOutgoing v && uniqueIncoming u) edges
    in  case eligibleEdge of
            Just (v, u) -> let v' = u `inlineInto` v
                           in  CFG (Map.insert (v' ^. nodeLabel) v' $ Map.delete (u ^. nodeLabel) g)
            Nothing     -> cfg_

cfgEdges :: CFG a d -> [(Node a d, Node a d)]
cfgEdges (CFG g) = concatMap (\v -> map (\ui -> (v, g Map.! ui)) (Set.elems $ v ^. nodeOut)) (Map.elems g)

uniqueOutgoing :: Node a d -> Bool
uniqueOutgoing n = Set.size (n ^. nodeOut) == 1

uniqueIncoming :: Node a d -> Bool
uniqueIncoming n = Set.size (n ^. nodeOut) == 1

inlineInto :: Node a d -> Node a d -> Node a d
inlineInto u v =
    let vInstrs = filter (\i -> (fmap (const ()) i) /= IJmp () (u ^. nodeLabel)) (v ^. nodeBody)
        uInstrs = dropWhile isLabel (u ^. nodeBody)
    in v & nodeBody .~ (vInstrs ++ uInstrs) & nodeOut .~ (u ^. nodeOut)
