-- Implementation of copy and constant propagation for Espresso.
{-# LANGUAGE DeriveTraversable #-}
module IR.Optimisation.Propagation where

import           Control.Monad.State
import           Data.Bifunctor
import qualified Data.Map                 as Map
import           Data.Maybe
import           IR.Flow.CFG
import           IR.Flow.Phi
import           IR.Flow.SSA
import           IR.Syntax.Syntax
import           IR.Utils

data ValKind a = ValSimple (Val a)   -- Value is a simple copy of another value.
             | ValString String     -- Value is a string constant.
             | ValComplex           -- Value is computed by a complex expression and irreducible.
    deriving (Eq, Functor, Foldable)

newtype PropagationState a = St {
    values :: Map.Map ValIdent (ValKind a)
} deriving Eq

propagateCopiesAndConsts :: SSA a () -> Method a -> SSA a ()
propagateCopiesAndConsts (SSA g) (Mthd pos t qi ps _) =
    let instrs = linearise g
        psKinds = map (\(Param _ _ vi) -> (vi, ValComplex)) ps
        f (is, s) = first concat $ runState (mapM propagate is) s
        (instrs', _) = fixpointBy (fmap (const ()). values . snd) f (instrs, St $ Map.fromList psKinds)
    in  SSA $ cfg (Mthd pos t qi ps (map (fmap fst) instrs'))

propagate :: Instr a -> State (PropagationState a) [Instr a]
propagate instr = case instr of
    IRet pos val -> do
        x <- tryPropagate val
        return [IRet pos x]
    IOp pos vi val1 op val2 -> do
        x1 <- tryPropagate val1
        x2 <- tryPropagate val2
        mbstr1 <- tryString x1
        mbstr2 <- tryString x2
        case (mbstr1, mbstr2) of
            (Just s1, Just s2) -> do
                setString vi (s1 ++ s2)
                return [INewStr pos vi (s1 ++ s2)]
            _ -> do
                let simplified = trySimplifyBinOp x1 op x2
                case simplified of
                    Just val -> do
                        setSimple pos vi val
                        return []
                    Nothing  -> do
                        setComplex vi
                        return [IOp pos vi x1 op x2]
    ISet pos vi val -> do
        x <- tryPropagate val
        setSimple pos vi x
        return []
    IUnOp pos vi unOp val -> do
        x <- tryPropagate val
        let simplified = trySimplifyUnOp x unOp
        case simplified of
            Just val' -> do
                setSimple pos vi val'
                return []
            Nothing -> do
                setComplex vi
                return [IUnOp pos vi unOp x]
    IVCall pos call -> do
        call' <- propagateInCall call
        return [IVCall pos call']
    ICall pos vi call -> do
        call' <- propagateInCall call
        setComplex vi
        return [ICall pos vi call']
    INew pos vi t -> do
        setComplex vi
        return [INew pos vi t]
    INewArr pos vi t val -> do
        x <- tryPropagate val
        setComplex vi
        return [INewArr pos vi t x]
    INewStr pos vi str -> do
        setString vi str
        return [INewStr pos vi str]
    ICondJmp pos val l1 l2 -> do
        x <- tryPropagate val
        return $ case x of
            VTrue _  -> [IJmp pos l1]
            VFalse _ -> [IJmp pos l2]
            _        -> [ICondJmp pos x l1 l2]
    ILoad pos vi ptr -> do
        ptr' <- propagateInPtr ptr
        setComplex vi
        return [ILoad pos vi ptr']
    IStore pos val ptr -> do
        x <- tryPropagate val
        ptr' <- propagateInPtr ptr
        return [IStore pos x ptr']
    IPhi pos vi phiVar -> do
        setComplex vi
        phiVar' <- mapM propagateInPhiVar phiVar
        let mbphi = unfoldTrivialPhi (IPhi pos vi phiVar')
        return $ maybeToList mbphi
    _ -> return [instr]

propagateInPtr :: Ptr a -> State (PropagationState a) (Ptr a)
propagateInPtr ptr = case ptr of
    PArrLen pos val -> do
        x <- tryPropagate val
        return $ PArrLen pos x
    PElem pos t val1 val2 -> do
        x1 <- tryPropagate val1
        x2 <- tryPropagate val2
        return $ PElem pos t x1 x2
    PFld pos t val qi -> do
        x <- tryPropagate val
        return $ PFld pos t x qi
    PLocal {} -> return ptr
    PParam {} -> return ptr

propagateInCall :: Call a -> State (PropagationState a) (Call a)
propagateInCall call = case call of
    Call p t qi vals labs -> do
        xs <- mapM tryPropagate vals
        return $ Call p t qi xs labs
    CallVirt p t qi vals -> do
        xs <- mapM tryPropagate vals
        return $ CallVirt p t qi xs

propagateInPhiVar :: PhiVariant a -> State (PropagationState a) (PhiVariant a)
propagateInPhiVar (PhiVar pos l val) = do
    x <- tryPropagate val
    return $ PhiVar pos l x

setComplex :: ValIdent -> State (PropagationState a) ()
setComplex vi = modify (St . Map.insert vi ValComplex . values)

setString :: ValIdent -> String -> State (PropagationState a) ()
setString vi str = modify (St . Map.insert vi (ValString str) . values)

setSimple :: a -> ValIdent -> Val a -> State (PropagationState a) ()
setSimple pos vi val = do
    x <- tryPropagate val
    let vk = ValSimple x
    modify (St . Map.insert vi vk . values)

tryPropagate :: Val a -> State (PropagationState a) (Val a)
tryPropagate val = case val of
    (VVal _ _ vi) -> do
        mbvk <- gets (Map.lookup vi . values)
        case mbvk of
            Just (ValSimple val') -> return val'
            Just ValComplex       -> return val
            Just (ValString _)    -> return val
            Nothing               -> return val
    _ -> return val

tryString :: Val a -> State (PropagationState a) (Maybe String)
tryString val = case val of
    (VVal _ _ vi) -> do
        mbvk <- gets (Map.lookup vi . values)
        case mbvk of
            Just (ValString s) -> return $ Just s
            _                  -> return Nothing
    _             -> return Nothing

trySimplifyBinOp :: Val a -> Op a -> Val a -> Maybe (Val a)
trySimplifyBinOp v1 op v2 = case op of
    OpAdd pos -> simplifyIntBinOp pos (+)
    OpSub pos -> simplifyIntBinOp pos (-)
    OpMul pos -> simplifyIntBinOp pos (*)
    OpDiv pos -> simplifyIntBinOp pos div
    OpMod pos -> simplifyIntBinOp pos mod
    OpLTH pos -> simplifyRelBinOp pos (<)
    OpLE pos  -> simplifyRelBinOp pos (<=)
    OpGTH pos -> simplifyRelBinOp pos (>)
    OpGE pos  -> simplifyRelBinOp pos (>=)
    OpEQU pos -> simplifyEqBinOp pos id
    OpNE pos  -> simplifyEqBinOp pos not
    where
        simplifyIntBinOp pos intOp = case (v1, v2) of
            (VInt _ n1, VInt _ n2)       -> Just $ VInt pos (n1 `intOp` n2)
            (VInt _ n1, VNegInt _ n2)    -> Just $ VInt pos (n1 `intOp` (-n2))
            (VNegInt _ n1, VInt _ n2)    -> Just $ VInt pos ((-n1) `intOp` n2)
            (VNegInt _ n1, VNegInt _ n2) -> Just $ VInt pos ((-n1) `intOp` (-n2))
            _                            -> Nothing
        simplifyRelBinOp pos relOp = case (v1, v2) of
            (VInt _ n1, VInt _ n2)       -> Just $ boolToVal pos (n1 `relOp` n2)
            (VInt _ n1, VNegInt _ n2)    -> Just $ boolToVal pos (n1 `relOp` (-n2))
            (VNegInt _ n1, VInt _ n2)    -> Just $ boolToVal pos ((-n1) `relOp` n2)
            (VNegInt _ n1, VNegInt _ n2) -> Just $ boolToVal pos ((-n1) `relOp` (-n2))
            (VFalse _, VFalse _)         -> Just $ boolToVal pos (0 `relOp` 0)
            (VFalse _, VTrue _)          -> Just $ boolToVal pos (0 `relOp` 1)
            (VTrue _, VFalse _)          -> Just $ boolToVal pos (1 `relOp` 0)
            (VTrue _, VTrue _)           -> Just $ boolToVal pos (1 `relOp` 1)
            (VVal _ _ vi1, VVal _ _ vi2)
                | vi1 == vi2             -> Just $ boolToVal pos (1 `relOp` 1)
            _                            -> Nothing
        simplifyEqBinOp pos modif = case (v1, v2) of
            (VInt _ n1, VInt _ n2)       -> Just $ boolToVal pos $ modif $ n1 == n2
            (VInt _ n1, VNegInt _ n2)    -> Just $ boolToVal pos $ modif $ n1 == (-n2)
            (VNegInt _ n1, VInt _ n2)    -> Just $ boolToVal pos $ modif $ (-n1) == n2
            (VNegInt _ n1, VNegInt _ n2) -> Just $ boolToVal pos $ modif $ (-n1) == (-n2)
            (VFalse _, VFalse _)         -> Just $ boolToVal pos $ modif True
            (VTrue _, VTrue _)           -> Just $ boolToVal pos $ modif True
            (VTrue _, VFalse _)          -> Just $ boolToVal pos $ modif False
            (VFalse _, VTrue _)          -> Just $ boolToVal pos $ modif False
            (VNull _ _, VNull _ _)       -> Just $ boolToVal pos $ modif True
            (VVal _ _ vi1, VVal _ _ vi2)
                | vi1 == vi2             -> Just $ boolToVal pos $ modif True
            _                            -> Nothing

trySimplifyUnOp :: Val a -> UnOp a -> Maybe (Val a)
trySimplifyUnOp val unOp = case unOp of
    UnOpNeg pos -> case val of
        VInt _ n    -> Just $ VInt pos (-n)
        VNegInt _ n -> Just $ VInt pos n
        _           -> Nothing
    UnOpNot pos -> case val of
        VTrue _  -> Just $ VFalse pos
        VFalse _ -> Just $ VTrue pos
        _        -> Nothing

boolToVal :: a -> Bool -> Val a
boolToVal pos True  = VTrue pos
boolToVal pos False = VFalse pos
