-- Implementation of Global Common Subexpression Elimination for Espresso.
-- Since this step has proven to be computationally costly, hashmaps are utilised.
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleInstances #-}
module IR.Optimisation.CommonSubexpressions where

import Control.Lens hiding (Const)

import           Control.Monad.State
import qualified Data.HashMap.Strict           as Map
import           Data.Hashable
import           Data.Int
import           IR.Flow.CFG
import           IR.Flow.Liveness
import           IR.Flow.SSA
import           IR.Syntax.Syntax
import           IR.Utils

data Subexpression a = SOp (Val a) (Op a) (Val a)
    | SStr String
    | SUnOp (Val a) (UnOp a)
    | SPhi [PhiVariant a]
    deriving (Eq, Functor, Foldable)

instance Hashable (Subexpression ()) where
    hashWithSalt salt val = case val of
        SOp v1 op v2          -> hashWithSalt salt (0 :: Int8, v1, op, v2)
        SStr s                -> hashWithSalt salt (1 :: Int8, s, False, False)
        SUnOp v op            -> hashWithSalt salt (2 :: Int8, v, op, False)
        SPhi vs               -> hashWithSalt salt (3 :: Int8, map (\(PhiVar _ _ v) -> v) vs, False, False)

-- Run GCSE.
globalCommonSubexpressionElimination :: SSA a Liveness -> SSA a ()
globalCommonSubexpressionElimination (SSA g) = SSA $ linearMap (\n -> n {_nNodeBody = eliminate $ n ^. nodeBody}) g

data GCSEState = St {
    subexprs :: Map.HashMap (Subexpression ()) ValIdent,
    dict     :: Map.HashMap ValIdent ValIdent
}

-- Since the input is in SSA the operation is a simple static replacement of identical right-hand-sides.
-- The induced copies will be eliminated later.
eliminate :: [Instr (a, Liveness)] -> [Instr (a, ())]
eliminate instrs = evalState (mapM go instrs >>= mapM replaceInInstr >>= mapM (\instr -> return $ fmap (\(pos, _) -> (pos, ())) instr)) (St Map.empty Map.empty)
    where
        go :: Instr (a, Liveness) -> State (GCSEState) (Instr (a, Liveness))
        go instr = do
            let liveData = snd $ single instr
            let pos = single instr
            let live = liveOut liveData
            instr' <- (replaceInInstr instr)
            case split (() <$ instr') of
                Just (vi, subexpr) -> do
                    mbvi <- gets (Map.lookup subexpr . subexprs)
                    case mbvi of
                        Just vi' -> do
                            let (_, t) = live Map.! toStr vi
                            modify (\st -> st {dict = Map.insert vi vi' (dict st)})
                            return $ ISet pos vi (VVal pos (pos <$ t) vi')
                        Nothing -> do
                            modify (\st -> st {subexprs = Map.insert subexpr vi (subexprs st)})
                            return instr'
                Nothing -> return instr'


replaceInInstr :: Instr a -> State (GCSEState) (Instr a)
replaceInInstr instr = case instr of
    IRet pos v -> do
        x <- replaceInVal v
        return $ IRet pos x
    IOp pos vi v1 op v2 -> do
        x1 <- replaceInVal v1
        x2 <- replaceInVal v2
        return $ IOp pos vi x1 op x2
    ISet pos vi v -> do
        x <- replaceInVal v
        return $ ISet pos vi x
    IUnOp pos vi op v -> do
        x <- replaceInVal v
        return $ IUnOp pos vi op x
    IVCall pos call -> do
        call' <- replaceInCall call
        return $ IVCall pos call'
    ICall pos vi call -> do
        call' <- replaceInCall call
        return $ ICall pos vi call'
    INewArr pos vi t v -> do
        x <- replaceInVal v
        return $ INewArr pos vi t x
    ICondJmp pos v l1 l2 -> do
        x <- replaceInVal v
        return $ ICondJmp pos x l1 l2
    ILoad pos vi ptr -> do
        ptr' <- replaceInPtr ptr
        return $ ILoad pos vi ptr'
    IStore pos v ptr -> do
        x <- replaceInVal v
        ptr' <- replaceInPtr ptr
        return $ IStore pos x ptr'
    IPhi pos vi phiVars -> do
        phiVars' <- mapM replaceInPhiVar phiVars
        return $ IPhi pos vi phiVars'
    _ -> return instr

replaceInCall :: Call a -> State (GCSEState) (Call a)
replaceInCall call = case call of
    Call pos t qi vs labs -> do
        xs <- mapM replaceInVal vs
        return $ Call pos t qi xs labs
    CallVirt pos t qi vs -> do
        xs <- mapM replaceInVal vs
        return $ CallVirt pos t qi xs

replaceInPhiVar :: PhiVariant a -> State (GCSEState) (PhiVariant a)
replaceInPhiVar (PhiVar pos l val) = PhiVar pos l <$> replaceInVal val

replaceInPtr :: Ptr a -> State (GCSEState) (Ptr a)
replaceInPtr ptr = case ptr of
    PArrLen pos v -> do
        x <- replaceInVal v
        return $ PArrLen pos x
    PElem pos t v1 v2 -> do
        x1 <- replaceInVal v1
        x2 <- replaceInVal v2
        return $ PElem pos t x1 x2
    PFld pos t v qi -> do
        x <- replaceInVal v
        return $ PFld pos t x qi
    PLocal {} -> return ptr
    PParam {} -> return ptr

replaceInVal :: Val a -> State (GCSEState) (Val a)
replaceInVal val = case val of
    VVal pos t vi -> do
        mbvi <- gets (Map.lookup vi . dict)
        return $ case mbvi of
            Just vi' -> VVal pos t vi'
            Nothing  -> VVal pos t vi
    _ -> return val

-- If the instruction is a valid one to propagate, split it into
-- the left-hand-side (identifier) and the right-hand-side (subexpression).
split :: Instr a -> Maybe (ValIdent, Subexpression ())
split instr = case (() <$ instr) of
    IOp _ vi v1 op v2 -> Just (vi, SOp v1 op v2)
    INewStr _ vi s    -> Just (vi, SStr s)
    IUnOp _ vi op v   -> Just (vi, SUnOp v op)
    IPhi _ vi vs      -> Just (vi, SPhi vs)
    -- The following are not valid to propagate.
    ILabel {}         -> Nothing -- Not an assignment.
    ILabelAnn {}      -> Nothing -- Not an assignment.
    IVRet {}          -> Nothing -- Not an assignment.
    IRet {}           -> Nothing -- Not an assignment.
    ISet {}           -> Nothing -- Handled by propagation.
    ISwap {}          -> Nothing -- Nontrivial memory operation.
    IVCall {}         -> Nothing -- Not an assignment.
    ICall {}          -> Nothing -- Can have sideffects
    INew {}           -> Nothing -- Has sideffects.
    INewArr {}        -> Nothing -- Has sideffects.
    IJmp {}           -> Nothing -- Not an assignment.
    ICondJmp {}       -> Nothing -- Not an assignment.
    ILoad {}          -> Nothing -- Nontrivial memory operation.
    IStore {}         -> Nothing -- Nontrivial memory operation.
    IEndPhi {}        -> Nothing -- Not an assignment.

fuse :: a -> ValIdent -> Subexpression () -> Instr a
fuse pos vi subexpr = case (pos <$ subexpr) of
    SOp v1 op v2 -> IOp pos vi v1 op v2
    SStr s       -> INewStr pos vi s
    SUnOp v op   -> IUnOp pos vi op v
    SPhi vs      -> IPhi pos vi vs
