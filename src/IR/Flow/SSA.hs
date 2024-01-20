{-# LANGUAGE DeriveFunctor    #-}
{-# LANGUAGE FlexibleContexts #-}
module IR.Flow.SSA (transformToSSA, unwrapSSA, SSA(..)) where

import Control.Lens hiding (Const)

import           Control.Monad.State
import           Data.Bifunctor
import qualified Data.HashMap.Strict           as HashMap
import           Data.List
import qualified Data.Map                      as Map
import           Data.Maybe
import qualified Data.Set                      as Set
import           IR.Flow.CFG
import           IR.Flow.Liveness
import           IR.Flow.Phi
import           IR.Syntax.Syntax
--import           Identifiers

import IR.Utils

newtype RenameDictionary = Dict {
    dict :: Map.Map ValIdent ValIdent
} deriving Eq

newtype SSA a d = SSA (CFG a d) deriving (Eq, Functor, Show)

data RenamedValIdent = NewValIdent {_ident :: ValIdent, version :: Integer}

unwrapSSA :: SSA a d -> CFG a d
unwrapSSA (SSA g) = g

transformToSSA :: CFG a Liveness -> Method a -> SSA a ()
transformToSSA g (Mthd _ _ _ params _) =
    let g' = insertEmptyPhis g
        (g'', dicts) = transformLocally g' params
    in SSA $ removeTrivialPhis $ fillPhis g'' dicts

-- For each node u and its predecessors v1, ..., vn insert
-- empty phi instructions for every variable x alive at the start of u:
--- x = phi(v1: x, ..., vn: x)
insertEmptyPhis :: CFG a Liveness -> CFG a ()
insertEmptyPhis = linearMap insertInNode
    where
        removeData :: (Functor f) => f (a, b) -> f (a, ())
        removeData = fmap (\(pos, _) -> (pos, ()))
        insertInNode :: Node a Liveness -> Node a ()
        insertInNode node =
            let (pos, _) = single $ single (node ^. nodeBody)
                (ls, code) = partition isLabel (node ^. nodeBody)
                ls' = map (removeData) ls
                code' = map (removeData) code
                phis' = map (removeData) (phis pos node)
                endPhi = [IEndPhi (pos, ()) | not (any isEndPhi code)]
            in  node {_nNodeBody = ls' ++ phis' ++ endPhi ++ code'}
        phis :: a -> Node a Liveness -> [Instr (a, ())]
        phis pos node = map fst $ foldl' (addPhiVars pos) (emptyPhis pos node) (Set.elems $ node ^. nodeIn)
        addPhiVars :: a -> [(Instr (a, ()), SType (a, ()))] -> LabIdent -> [(Instr (a, ()), SType (a, ()))]
        addPhiVars pos xs n = map (\(IPhi p vi phiVars, t) -> (IPhi p vi (PhiVar p n (VVal p t vi):(phiVars)), t)) xs
        emptyPhis :: a -> Node a Liveness -> [(Instr (a, ()), SType (a, ()))]
        emptyPhis pos node = map (\(vi, (_, t)) -> (IPhi (pos, ()) (ValIdent vi) [], fmap (const (pos, ())) t)) (HashMap.toList $ liveIn $ nodeLiveness node)

-- Given the translations for each incoming label, replace the empty phi instruction
-- with the actual values from each node.
fillPhis :: CFG a () -> Map.Map LabIdent RenameDictionary -> CFG a ()
fillPhis g dicts = linearMap fillInNode g
    where
        fillInNode node =
            let (ls, x) = partition isLabel (node ^. nodeBody)
                (phis, code) = partition isPhi x
                phis' = map fillPhi phis
            in  node { _nNodeBody = (ls ++ phis' ++ code) }
        fillPhi (IPhi p vi phiVars) =
            IPhi p vi (map fillPhiVar phiVars)
        fillPhi _ = error "impossible"
        fillPhiVar (PhiVar p n (VVal _ t vi)) =
            let vi' = fromMaybe vi (Map.lookup vi (dict $ dicts Map.! n))
            in  PhiVar p n (VVal p t vi')
        fillPhiVar _ = error "impossible"

-- Perform SSA translation without touching the contents of phi instructions.
-- Each assignment generates new version of the value.
-- That version is valid until next static assignment.
-- The resulting RenameDictionary is the version of the value at end of a block.
type TransformState = (Map.Map ValIdent ValIdent, Map.Map ValIdent RenamedValIdent)
transformLocally :: CFG a d -> [Param a] -> (CFG a d, Map.Map LabIdent RenameDictionary)
transformLocally g params =
    let paramMap = Map.fromList (map (\(Param _ _ vi) -> (vi, NewValIdent vi 1)) params)
        g' = Map.fromList $ evalState (mapM transformNode (lineariseNodes g)) (Map.empty, paramMap)
    in (CFG $ Map.map fst g', Map.map snd g')
    where
        transformNode :: Node a d -> State TransformState (LabIdent, (Node a d, RenameDictionary))
        transformNode node = do
            instrs <- mapM transformInstr (node ^. nodeBody)
            (d, _) <- get
            modify (first (const Map.empty))
            return (node^.nodeLabel, (node { _nNodeBody = instrs}, Dict d))
        transformInstr :: Instr a -> State TransformState (Instr a)
        transformInstr instr = case instr of
            IRet a val -> do
                x <- renameVal val
                return $ IRet a x
            IOp a vi val1 op val2 -> do
                x1 <- renameVal val1
                x2 <- renameVal val2
                vi' <- newVersion vi
                return $ IOp a vi' x1 op x2
            ISet a vi val -> do
                x <- renameVal val
                vi' <- newVersion vi
                return $ ISet a vi' x
            ISwap a t vi1 vi2 -> do
                mbvi1' <- gets (Map.lookup vi1 . fst)
                let vi1' = fromMaybe vi1 mbvi1'
                mbvi2' <- gets (Map.lookup vi2 . fst)
                let vi2' = fromMaybe vi2 mbvi2'
                return $ ISwap a t vi1' vi2'
            IUnOp a vi op val -> do
                x <- renameVal val
                vi' <- newVersion vi
                return $ IUnOp a vi' op x
            IVCall a call -> do
                call' <- transformCall call
                return $ IVCall a call'
            ICall a vi call -> do
                call' <- transformCall call
                vi' <- newVersion vi
                return $ ICall a vi' call'
            INew a vi t -> do
                vi' <- newVersion vi
                return $ INew a vi' t
            INewArr a vi t val -> do
                x <- renameVal val
                vi' <- newVersion vi
                return $ INewArr a vi' t x
            ICondJmp a val l1 l2 -> do
                x <- renameVal val
                return $ ICondJmp a x l1 l2
            ILoad a vi ptr -> do
                x <- renamePtr ptr
                vi' <- newVersion vi
                return $ ILoad a vi' x
            IStore a val ptr -> do
                x1 <- renameVal val
                x2 <- renamePtr ptr
                return $ IStore a x1 x2
            IPhi a vi phiVars -> do
                vi' <- newVersion vi
                return $ IPhi a vi' phiVars
            _ -> return instr
        transformCall call = case call of
            Call a t qi vals labs -> do
                xs <- mapM renameVal vals
                return $ Call a t qi xs labs
            CallVirt a t qi vals -> do
                xs <- mapM renameVal vals
                return $ CallVirt a t qi xs
        renameVal val = case val of
            VVal a t vi -> do
                mbvi' <- gets (Map.lookup vi . fst)
                let vi' = fromMaybe vi mbvi'
                return $ VVal a t vi'
            _ -> return val
        renamePtr ptr = case ptr of
            PArrLen a val -> do
                x <- renameVal val
                return $ PArrLen a x
            PElem a t val1 val2 -> do
                x1 <- renameVal val1
                x2 <- renameVal val2
                return $ PElem a t x1 x2
            PFld a t val qi -> do
                x <- renameVal val
                return $ PFld a t x qi
            PLocal {} -> return ptr
            PParam {} -> return ptr
        newVersion vi = do
            mbvi' <- gets (Map.lookup vi . snd)
            let version' = case mbvi' of
                    Just vi' -> version vi' + 1
                    Nothing  -> 1
                ident' = ValIdent $ toStr vi ++ if version' == 1 then "" else "~" ++ show version'
            modify (first $ Map.insert vi ident')
            modify (second $ Map.insert vi (NewValIdent ident' version'))
            return ident'
