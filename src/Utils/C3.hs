module Utils.C3(
    linearizeNode
    , linearizeNodeContent
) where

import qualified Utils.Graphs as G

import qualified Data.Map as M

import Data.List (nub)
import Control.Monad (when)
import Control.Monad.Except hiding (void)
--import Control.Monad.Identity(Identity, runIdentity)
--import Control.Monad.Error -- used for testing

groupByKey :: (Ord k) => (v -> k) -> [v] -> M.Map k [v]
groupByKey getkey
  = M.fromListWith (++) . fmap (\val -> (getkey val, [val]))

-- | Returns the a linearization using C3 algorithm. Takes a function
-- and an element. We can apply the function in this element to obtain
-- its parents.
linearize :: (MonadError String m, Eq a) => (a -> m [a]) -> a -> m [a]
linearize = linearize' []

-- | Implementation behind linearize. Keeps a list of seen elements to
-- detect loops in the hierarchy.
linearize' :: (MonadError String m, Eq a) => [a] -> (a -> m [a]) -> a -> m [a]
linearize' seen p root = do
    when (root `elem` seen) $ throwError "loop detected in hierarchy"
    root_ps <- p root
    gran_ps <- mapM (linearize' (root : seen) p) root_ps
    let root_ps' = map (\x -> [x]) root_ps 
        gran_ps' = filter (not . null) gran_ps
    a <- merge (gran_ps' ++ root_ps')
    return (root : a)

-- | The merge operation from C3.
merge :: (MonadError String m, Eq a) => [[a]] -> m [a]
merge []    = return []
merge l     = merge_round candidates l
    where
    candidates = nub (map head l)

-- | Auxiliary function for the merge operation, given a candidate list,
-- find a good candidate, return 'Nothing' if none of them can be used,
-- meaning an impossible merge due conflict. If it finds one, calls
-- 'merge' to find next element in the linearization.
merge_round :: (MonadError String m, Eq a) => [a] -> [[a]] -> m [a]
merge_round _  [] = return []
merge_round [] _  = throwError "merge conflict"
merge_round (c:cs) l
    | good c l = do
        a <- merge clean_list
        return (c:a)
    | otherwise = merge_round cs l
    where
    clean_list      = filter (not . null) (merge_clean c l)
    merge_clean c   = map (filter ((/=) c))

-- |Returns 'True' if a candidate element isn't present in the tail
-- of each list.
good :: Eq a => a -> [[a]] -> Bool
good _ []     = True
good c (x:xs)
    | c `elem` (tail x) = False
    | otherwise         = good c xs

linearizeNode :: (Ord key) => G.Graph key nodeL edgeL -> key -> Either String [nodeL]
linearizeNode g@(G.Graph nodeMap nodeIds graph) startKey =
    G.getNodes g <$> (runExcept $ linearize (return . G.childrenKeys g) startKey)
    -- case G.getNode g startKey of
    --     Just startNode -> linearize () startNode

linearizeNodeContent :: (Ord key, Ord contentKey) => G.Graph key nodeL edgeL -> (nodeL -> [contentT]) -> (contentT -> contentKey) -> key -> Either String (M.Map contentKey [(nodeL, contentT)])
linearizeNodeContent g getContent contentToKey startKey =
    (groupByKey (contentToKey . snd) . concatMap (\node -> map (\content -> (node, content)) $ getContent node)) <$> (linearizeNode g startKey)