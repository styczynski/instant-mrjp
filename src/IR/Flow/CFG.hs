-- Generation of Control Flow Graphs for methods.
{-# LANGUAGE DeriveFoldable   #-}
{-# LANGUAGE DeriveFunctor    #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell #-}
module IR.Flow.CFG where

import Control.Lens hiding (Const)

import Data.Foldable
import Data.Maybe
import           Control.Monad.State
import qualified Data.Map            as Map
import qualified Data.Set            as Set
import           IR.Syntax.Syntax

newtype CFG a d = CFG (Map.Map IRLabelName (Node a d)) deriving (Functor, Foldable)

data Node a d = Node {
    _nNodeLabel :: IRLabelName,
    _nNodeBody  :: [Instr (a, d)],
    _nNodeOut   :: Set.Set IRLabelName,
    _nNodeIn    :: Set.Set IRLabelName
} deriving (Functor, Foldable)

makeLensesWith abbreviatedFields ''Node

mapCFGPos :: (a -> b) -> CFG a d -> CFG b d
mapCFGPos f (CFG m) = CFG $ Map.map (mapNodePos f) m

mapCFG :: ((a, d) -> (b, e)) -> CFG a d -> CFG b e
mapCFG f (CFG m) = CFG $ Map.map (mapNode f) m

mapNodePos :: (a -> b) -> Node a d -> Node b d
mapNodePos f node = node { _nNodeBody = map (fmap (\(p, d) -> (f p, d))) (node^.nodeBody)}

mapNode :: ((a, d) -> (b, e)) -> Node a d -> Node b e
mapNode f node = node { _nNodeBody = map (fmap f) (node^.nodeBody)}

lookupCFGNode :: (CFG a d) -> IRLabelName -> Node a d
lookupCFGNode cfg@(CFG m) name = case Map.lookup name m of 
    Nothing -> error $ "lookupCFGNode: Cannot find CFG Node labelled " ++ (show name) ++ " in CFG: " ++ (show $ mapCFG (const ((), ())) cfg)
    (Just node) -> node


instance (Eq d) => Eq (CFG a d) where
    (==) cfg1 cfg2 =
        let (CFG m1) = cfg1 in
        let (CFG m2) = cfg2 in
        m1 == m2

instance (Eq d) => Eq (Node a d) where
    (==) (Node l1 b1 o1 i1) (Node l2 b2 o2 i2) =
        let b1' = map (fmap (snd)) b1 in
        let b2' = map (fmap (snd)) b2 in
        (l1 == l2) && (b1' == b2') && (o1 == o2) && (i1 == i2)


nodeCode :: Node a d -> [Instr d]
nodeCode = map (fmap snd) . (^. nodeBody)

instance Show (Node a d) where
    show = toStr . (^. nodeLabel)

isJump :: Instr a -> Bool
isJump instr = case instr of
    IJmp {}     -> True
    ICondJmp {} -> True
    IVRet {}    -> True
    IRet {}     -> True
    _           -> False


cfg :: Method a -> CFG a ()
cfg (Mthd pos _ _ _ instrs) =
    let basicBlocks = splitBasicBlocks instrs
        initial = Map.fromList $ map (\(l, is) -> (l, Node l is Set.empty Set.empty)) basicBlocks
    in  execState (construct basicBlocks) (CFG initial)

linearMap :: (Node a d -> Node b e) -> CFG a d -> CFG b e
linearMap f g = CFG (Map.fromList $ map (\n -> (n ^. nodeLabel, f n)) $ lineariseNodes g)


linearise :: CFG a d -> [Instr (a, d)]
linearise = concatMap (^. nodeBody) . lineariseNodes

splitBasicBlocks :: [Instr a] -> [(IRLabelName, [Instr (a, ())])]
splitBasicBlocks = map (\(lab, instrs) -> (lab, map (fmap (\d -> (d, ()))) instrs)) . go []
    where
        go bbs []                         = map finalizeBlock bbs
        go bbs (i@(ILabel _ l):is)        = go ((l, [i]):bbs) is
        go bbs (i@(ILabelAnn _ l _ _):is) = go ((l, [i]):bbs) is
        go ((l, x):bbs) (j:is) | isJump j = go ((l, j:x):bbs) (dropWhile (not . isLabel) is)
        go ((l, x):bbs) (i:is)            = go ((l, i:x):bbs) is
        go [] _                           = error "first instruction is not a label"
        finalizeBlock (l, []) = error ("empty basic block: " ++ toStr l)
        finalizeBlock (l, is@(i:_)) | isJump i = (l, reverse is)
        finalizeBlock (l, _)  = error ("basic block not ending with a jump: " ++ toStr l)

lineariseNodes :: CFG a d -> [Node a d]
lineariseNodes (CFG g) =
    case Map.lookup entryLabel g of
        Just entry -> evalState (go entry) Set.empty
        Nothing    -> error "internal error. malformed graph, no entry label"
    where
        go node = do
            rest <- mapM expand (Set.elems $ node ^. nodeOut)
            return $ node : concat rest
        expand l = do
            case Map.lookup l g of
                Just node -> do
                    wasVisited <- gets (Set.member l)
                    if wasVisited then return [] else do
                        modify (Set.insert l)
                        go node
                Nothing -> error $ "internal error. malformed graph, no " ++ toStr l ++ " node"


construct :: [(IRLabelName, [Instr (a, d)])] -> State (CFG a d) ()
construct = mapM_ fromJumps
    where fromJumps (_, [])      = return ()
          fromJumps (from, i:is) = fromInstr from i >> fromJumps (from, is)
          fromInstr from i = case i of
              IJmp _ to            -> addEdge from to
              ICondJmp _ _ to1 to2 -> addEdge from to1 >> addEdge from to2
              _                    -> return ()

addEdge :: IRLabelName -> IRLabelName -> State (CFG a d) ()
addEdge from to = do
    mbfromNode <- gets (\(CFG g) -> Map.lookup from g)
    mbtoNode <- gets (\(CFG g) -> Map.lookup to g)
    let fromNode' = case mbfromNode of
            Just fromNode -> fromNode & nodeOut %~ (Set.insert to)
            Nothing -> error $ "internal error. no src label " ++ toStr from
        toNode' = case mbtoNode of
            Just toNode -> toNode & nodeIn %~ (Set.insert from)
            Nothing     -> error $ "internal error. no dest label " ++ toStr to
    modify (\(CFG g) -> CFG $ Map.insert from fromNode' g)
    modify (\(CFG g) -> CFG $ Map.insert to toNode' g)

instance Show (CFG a d) where
    show (CFG g) = unlines (nodes:map edges (Map.elems g))
        where nodes = show (map toStr $ Map.keys g)
              edges node = show (toStr $ node ^. nodeLabel) ++ " -> " ++ show (node ^. nodeOut) ++ " <- " ++ show (node ^. nodeIn)

