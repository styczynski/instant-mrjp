{-# LANGUAGE FlexibleInstances #-}
module Backend.X64.Generator where

import qualified Backend.X64.Parser.Constructor as X64


import           Control.Monad.Reader
import           Control.Monad.State
import           Data.Bifunctor                (Bifunctor (second))
import           Data.Int
import           Data.List                     (partition)
import qualified Data.Map                      as Map
import           IR.Flow.CFG      (CFG (..), Node (..))
import           IR.Flow.Liveness
import           IR.Syntax.Syntax
import           IR.Types                (deref, isInt, isStr, ptrType,
                                                strType, valType)
import           IR.Utils                     (isPowerOfTwo, log2, single)
import           IR.Class
import IR.Identifiers
import           IR.CodeGen.Consts
import           IR.CodeGen.Epilogue
import           IR.CodeGen.Module
import           IR.CodeGen.Prologue
import           IR.CodeGen.Stack
import           IR.Loc
import           IR.RegisterAllocation.RegisterAllocation
import           IR.Registers
import           IR.Size

import Backend.X64.Env
import Backend.X64.Def

import qualified Backend.X64.Parser.Constructor as X64

genInstr :: Instr Liveness -> Generator ()
genInstr instr = do
    liftGenerator $ X64.mov () X64.Size64 (X64.LocReg X64.RAX) (X64.LocReg X64.RDX)

-- generate :: Metadata () -> [(CFG Liveness, Method a, RegisterAllocation)] -> String
-- generate (Meta () clDefs) mthds =
--     let (mthds', cs) = foldr go ([], constsEmpty) mthds
--     in  generateModule cls mthds' cs
--     where
--     cls = map compileClass clDefs
--     clMap = Map.fromList $ map (\cl -> (clName cl, cl)) cls
--     go (CFG g, Mthd _ _ qi _ _, rs) (xs, cs) =
--         let initStack = stackNew (numLocals rs)
--             initState = St [] [] cs initStack Map.empty emptyLiveness 0
--             st = runReader (execStateT goOne initState) (Env (labelFor qi) rs clMap)
--             rawMthd = CmpMthd (toStr $ labelFor qi entryLabel) [] (reverse $ allCode st) []
--             mthd = withEpilogue rs $ withPrologue qi rs rawMthd
--         in (mthd:xs, consts st)
--         where
--             goOne = do
--                 traceM' ("register allocation: " ++ show (Map.toList $ regAlloc rs))
--                 traceM' ("========== starting method: " ++ toStr (labelFor qi (LabIdent "")))
--                 let nodes = Map.elems g
--                     entryNode = single $ filter ((== entryLabel) . nodeLabel) nodes
--                     exitNode = single $ filter (any isRet . nodeCode) nodes
--                     otherNodes = filter ((\l -> l /= nodeLabel entryNode && l /= nodeLabel exitNode) . nodeLabel) nodes
--                 genNode entryNode
--                 mapM_ genNode otherNodes
--                 when (nodeLabel entryNode /= nodeLabel exitNode) (genNode exitNode)
--             genNode node = do
--                 traceM' ("===== starting block: " ++ toStr (nodeLabel node))
--                 mapM_ genInstr (nodeCode node)
--                 modify (\st -> st{allCode = bbCode st ++ allCode st, bbCode = []})
--             isRet instr = case instr of
--                 IRet _ _ -> True
--                 IVRet _  -> True
--                 _        -> False

-- genInstr :: Instr Liveness -> Generator ()
-- genInstr instr =
--     let instrLiveness = single instr
--     in do
--         updateLive instrLiveness
--         traceM' (show instr)
--         fullTrace
--         case instr of
--             ILabel _ l -> do
--                 l' <- label l
--                 Emit.label l' ""
--             ILabelAnn _ l f t -> do
--                 l' <- label l
--                 Emit.label l' $ "lines " ++ show f ++ "-" ++ show t
--             IVRet _ -> do
--                 resetStack
--             IRet _ val -> do
--                 loc <- getValLoc val
--                 Emit.mov (valSize val) loc (convertReg rax) "move return value"
--                 resetStack
--             IOp _ vi v1 op v2 -> do
--                 dest <- getLoc vi
--                 src1 <- getValLoc v1
--                 src2 <- getValLoc v2
--                 let size = valSize v1
--                 case op of
--                     OpAdd _ | isInt (valType v1) -> do
--                         if dest == src1 then
--                           Emit.add src2 dest ""
--                         else if dest == src2 then
--                           Emit.add src1 dest ""
--                         else case (src1, src2) of
--                             (LocReg r1, LocReg r2) ->
--                                 Emit.lea Double (LocPtrCmplx r1 r2 0 Byte) dest ("addition " ++ toStr vi)
--                             (LocImm n1, LocReg r2) ->
--                                 Emit.lea Double (LocPtr r2 (fromIntegral n1)) dest ("addition " ++ toStr vi)
--                             (LocReg r1, LocImm n2) ->
--                                 Emit.lea Double (LocPtr r1 (fromIntegral n2)) dest ("addition " ++ toStr vi)
--                             _ -> error "internal error. invalid src locs in add"
--                     OpAdd _ | isStr (valType v1) -> do
--                         genCall (CallDirect "lat_cat_strings") [v1, v2] [] (Emit.mov Quadruple (convertReg rax) dest "")
--                     OpAdd _ -> error "internal error. invalid operand types for add."
--                     OpSub _ -> do
--                         if dest == src1 then
--                           Emit.sub src2 dest
--                         else if dest == src2 then do
--                           Emit.sub src1 dest
--                           Emit.neg dest
--                         else do
--                           Emit.mov Double src1 dest ""
--                           Emit.sub src2 dest
--                     OpMul _ -> do
--                         case (src1, src2) of
--                             (LocImm n, _) | isPowerOfTwo n -> do
--                                 when (dest /= src2) (Emit.mov Double src2 dest "")
--                                 Emit.sal (log2 n) dest ("multiply by " ++ show n)
--                             (_, LocImm n) | isPowerOfTwo n -> do
--                                 when (dest == src1) (Emit.mov Double src1 dest "")
--                                 Emit.sal (log2 n) dest ("multiply by " ++ show n)
--                             _ -> do
--                                 if dest == src1 then
--                                   Emit.imul src2 dest
--                                 else if dest == src2 then
--                                   Emit.imul src1 dest
--                                 else do
--                                   Emit.mov Double src1 dest ""
--                                   Emit.imul src2 dest
--                     OpDiv _ -> do
--                         case src2 of
--                             LocImm n | isPowerOfTwo n -> do
--                                 Emit.mov Double src1 dest ""
--                                 Emit.sar (log2 n) dest ("divide by " ++ show n)
--                             LocImm {} -> do
--                                 Emit.mov Double src1 (convertReg rax) ""
--                                 Emit.cdq
--                                 Emit.mov Double src2 src1 ""
--                                 Emit.idiv Double src1
--                                 Emit.mov Double (convertReg rax) dest ""
--                             _ -> do
--                                 Emit.mov Double src1 (convertReg rax) ""
--                                 Emit.cdq
--                                 Emit.idiv Double src2
--                                 Emit.mov Double (convertReg rax) dest ""
--                     OpMod _ -> do
--                         case src2 of
--                             LocImm n | isPowerOfTwo n -> do
--                                 -- n % 2^k
--                                 -- is the same as
--                                 -- n AND (2^k - 1)
--                                 Emit.mov Double src1 dest ""
--                                 Emit.and Double (LocImm (n - 1)) dest ("modulo by " ++ show n)
--                             LocImm {} -> do
--                                 Emit.mov Double src1 (convertReg rax) ""
--                                 Emit.cdq
--                                 Emit.mov Double src2 src1 ""
--                                 Emit.idiv Double src1
--                                 Emit.mov Double (convertReg rdx) dest ""
--                             _ -> do
--                                 Emit.mov Double src1 (convertReg rax) ""
--                                 Emit.cdq
--                                 Emit.idiv Double src2
--                                 Emit.mov Double (convertReg rdx) dest ""
--                     OpLTH _ -> emitCmpBin op dest src1 src2 size
--                     OpLE _  -> emitCmpBin op dest src1 src2 size
--                     OpGTH _ -> emitCmpBin op dest src1 src2 size
--                     OpGE _  -> emitCmpBin op dest src1 src2 size
--                     OpEQU _ -> emitCmpBin op dest src1 src2 size
--                     OpNE _  -> emitCmpBin op dest src1 src2 size
--             ISet _ vi v -> do
--                 let t = valType v
--                 dest <- getLoc vi
--                 src <- getValLoc v
--                 Emit.mov (typeSize t) src dest $ "setting " ++ toStr vi
--             ISwap _ t vi1 vi2 -> do
--                 loc1 <- getLoc vi1
--                 loc2 <- getLoc vi2
--                 Emit.xchg (typeSize t) loc1 loc2
--             IUnOp _ vi op v -> do
--                 let t = valType v
--                 src <- getValLoc v
--                 dest <- getLoc vi
--                 Emit.mov (typeSize t) src dest $ "setting " ++ toStr vi
--                 case op of
--                     UnOpNeg _ -> Emit.neg dest
--                     UnOpNot _ -> Emit.xor Byte (LocImm 1) dest
--             IVCall _ call -> case call of
--                     Call _ _ qi args largs    -> genCall (CallDirect $ getCallTarget qi) args largs (return ())
--                     CallVirt _ _ qi args -> genCallVirt qi args (return ())
--             ICall _ vi call -> do
--                 dest <- getLoc vi
--                 case call of
--                         Call _ t qi args largs    -> genCall (CallDirect $ getCallTarget qi) args largs (Emit.mov (typeSize t) (LocReg rax) dest "")
--                         CallVirt _ t qi args -> genCallVirt qi args (Emit.mov (typeSize t) (LocReg rax) dest "")
--             ILoad _ vi ptr -> do
--                 let t = () <$ deref (ptrType ptr)
--                 src <- getPtrLoc ptr
--                 dest <- getLoc vi
--                 Emit.mov (typeSize t) src dest ("load " ++ toStr vi)
--             IStore _ v ptr -> do
--                 let t = valType v
--                 src <- getValLoc v
--                 dest <- getPtrLoc ptr
--                 Emit.mov (typeSize t) src dest ""
--             INew _ vi t -> case t of
--                 Cl _ clIdent -> do
--                     cl <- getClass clIdent
--                     dest <- getLoc vi
--                     let sizeArg = VInt () (toInteger $ clSize cl)
--                         (LocReg tmpReg) = argLoc 0
--                     let clLabel = classDefIdent clIdent
--                     genCall (CallDirect "__new") [] [clLabel] (do
--                         --Emit.leaOfConst (toStr $ vTableLabIdent clIdent) tmpReg
--                         --Emit.mov Quadruple (LocReg tmpReg) (LocPtr rax 0) "store vtable"
--                         Emit.mov Quadruple (convertReg rax) dest "")
--                 _ -> error $ "internal error. new on nonclass " ++ show t
--             INewStr _ vi str -> do
--                 let t = Ref () strType
--                 dest <- getLoc vi
--                 strConst <- newStrConst str
--                 case dest of
--                     LocReg reg_ -> Emit.leaOfConst (constName strConst) reg_
--                     _ -> error $ "internal error. invalid dest loc " ++ show dest
--                 genCall (CallDirect "__createString") [VVal () t vi] [] (Emit.mov Quadruple (convertReg rax) dest "")
--             INewArr _ vi t val -> do
--                 dest <- getLoc vi
--                 --let sizeArg = VInt () (toInteger $ sizeInBytes $ typeSize t)
--                 --() <$ val, 
--                 -- | = Int a
--                 -- | Bool a
--                 -- | Void a
--                 -- | Arr a (SType a)
--                 -- | Cl a SymIdent
--                 -- | Ref a (SType a)
--                 case t of 
--                     (Int _) -> genCall (CallDirect "__newIntArray") [() <$ val] [] (Emit.mov Quadruple (convertReg rax) dest "")
--                     (Bool _) -> genCall (CallDirect "__newByteArray") [() <$ val] [] (Emit.mov Quadruple (convertReg rax) dest "")
--                     (Void _) -> genCall (CallDirect "__newArray") [VInt () 0, () <$ val] [] (Emit.mov Quadruple (convertReg rax) dest "")
--                     (Cl _ name) -> error $ "internal error. cannot create array of class type " ++ show name
--                     (Ref _ _) -> genCall (CallDirect "__newRefArray") [() <$ val] [] (Emit.mov Quadruple (convertReg rax) dest "")
--                     _ -> error $ "internal error. invalid array type " ++ show t
--             IJmp _ li -> do
--                 li' <- label li
--                 resetStack
--                 Emit.jmp li'
--             ICondJmp _ v l1 l2 -> do
--                 loc <- getValLoc v
--                 l1' <- label l1
--                 l2' <- label l2
--                 resetStack
--                 case loc of
--                     LocImm 0 -> do
--                         Emit.jmp l2'
--                     LocImm 1 -> do
--                         Emit.jmp l1'
--                     _ -> do
--                         Emit.test Byte loc loc
--                         Emit.jz l2'
--                         Emit.jmp l1'
--             IPhi {} -> error "internal error. phi should be eliminated before assembly codegen"
--             IEndPhi {} -> return ()
--         fullTrace
--         return ()

-- data CallTarget = CallDirect String | CallVirtual Int64 String

-- data CallArg a = CallArgVal (Val a) | CallArgLabel (LabIdent)

-- genCall :: CallTarget -> [Val a] -> [LabIdent] -> Generator () -> Generator ()
-- genCall target varArgs labelArgs cont = do
--         let args = (map CallArgVal varArgs) ++ (map CallArgLabel labelArgs)
--         regs_ <- getPreservedRegs
--         let argsWithLocs = zip args (map argLoc [0..])
--             (argsWithLocReg,  argsWithLocStack) = partition (isReg . snd) argsWithLocs
--             argsInRegs = map (second asReg) argsWithLocReg
--             argsOnStack = map fst argsWithLocStack
--             savedRegs = filter ((== CallerSaved) . regType) regs_
--         forM_ savedRegs (\r -> Emit.push (convertReg r) "save caller saved")
--         forM_ argsInRegs (uncurry passInReg)
--         stackBefore <- gets (stackOverheadSize . stack)
--         locs <- mapM prepOnStack (reverse argsOnStack)
--         alignStack
--         stackAfter <- gets (stackOverheadSize . stack)
--         forM_ locs (`Emit.push` "passing arg")
--         case target of
--             CallDirect l              -> Emit.callDirect l
--             CallVirtual offset s -> do
--                 let self@(convertReg selfReg) = argLoc 0
--                 Emit.test Quadruple self self
--                 Emit.jz nullrefLabel
--                 Emit.mov Quadruple (LocPtr selfReg 20) (convertReg rax) "load address of vtable"
--                 --Emit.mov Quadruple (LocPtr rax 12) (LocReg selfReg) "load address of vtable"
--                 Emit.callAddress rax offset ("call " ++ s)
--         Emit.decrStack (stackAfter - stackBefore)
--         modify (\st -> st{stack = (stack st){stackOverheadSize = stackBefore}})
--         cont
--         forM_ (reverse savedRegs) (Emit.pop . LocReg)
--     where
--           passInReg :: CallArg a -> Reg -> Generator ()
--           passInReg (CallArgLabel l) reg_ = do
--             Emit.leaOfConst (toStr l) reg_
--             --Emit.mov (Quadruple) (LocLabel l) (LocReg reg_) "passing label arg"
--           passInReg (CallArgVal val) reg_ = do
--             loc <- getValLoc val
--             Emit.mov (valSize val) loc (convertReg reg_) "passing arg"
--           prepOnStack :: CallArg a -> Generator Loc
--           prepOnStack ((CallArgLabel (LabIdent l))) = do
--               s <- gets stack
--               let s' = stackPush Quadruple s
--               setStack s'
--               return (LocLabel l)
--           prepOnStack (CallArgVal val) = do
--               s <- gets stack
--               loc <- getValLoc val
--               let s' = stackPush Quadruple s
--               setStack s'
--               return loc
--           alignStack = do
--               (misalignment, s) <- gets (stackAlign16 . stack)
--               Emit.incrStack misalignment "16 bytes alignment"
--               setStack s

-- genCallVirt :: QIdent a -> [Val a] -> Generator () -> Generator ()
-- genCallVirt _ [] _ = error "internal error. callvirt with no args"
-- genCallVirt (QIdent _ cli i) args cont = do
--     cl <- getClass cli
--     let offset = case Map.lookup i $ vtabMthdMap $ clVTable cl of
--                     Just (_, n) -> n
--                     Nothing     -> error ""
--     genCall (CallVirtual offset (toStr i)) args [] cont

-- emitCmpBin :: Op a -> Loc -> Loc -> Loc -> Size -> Generator ()
-- emitCmpBin op dest src1 src2 size = do
--     case src1 of
--         LocImm {} -> do
--             Emit.cmp size src1 src2
--             revCmpEmitter op dest
--         LocImm64 {} -> do
--             Emit.cmp size src1 src2
--             revCmpEmitter op dest
--         _ -> do
--             Emit.cmp size src2 src1
--             cmpEmitter op dest

-- cmpEmitter :: Op a -> Loc -> Generator ()
-- cmpEmitter op = case op of
--     OpLTH _ -> Emit.setl
--     OpLE _  -> Emit.setle
--     OpGTH _ -> Emit.setg
--     OpGE _  -> Emit.setge
--     OpEQU _ -> Emit.sete
--     OpNE _  -> Emit.setne
--     _       -> error "internal error. invalid op to cmpEmitter."

-- revCmpEmitter :: Op a -> Loc -> Generator ()
-- revCmpEmitter op = case op of
--     OpLTH _ -> Emit.setge
--     OpLE _  -> Emit.setg
--     OpGTH _ -> Emit.setle
--     OpGE _  -> Emit.setl
--     OpEQU _ -> Emit.sete
--     OpNE _  -> Emit.setne
--     _       -> error "internal error. invalid op to cmpEmitter."

-- resetStack :: Generator ()
-- resetStack = do
--     s <- gets stack
--     let (n, s') = stackClearOverhead s
--     Emit.decrStack n
--     setStack s'

-- getPtrLoc :: Ptr a -> Generator Loc
-- getPtrLoc ptr = case ptr of
--     PArrLen _ val -> do
--         src <- getValLoc val
--         case src of
--             LocReg reg_ -> return $ LocPtr reg_ 0
--             _ -> error $ "internal error. invalid src loc for arrlen " ++ show src
--     PElem _ elemT arrVal idxVal -> do
--         let elemSize = typeSize $ deref elemT
--             idxOffset = 8
--             -- idxOffset = if elemSize < Double
--             --               then sizeInBytes Double
--             --               else sizeInBytes elemSize
--         arrSrc <- getValLoc arrVal
--         idxSrc <- getValLoc idxVal
--         case (arrSrc, idxSrc) of
--             (LocReg arrReg, LocReg idxReg) -> do
--                 let (LocReg tmpReg) = argLoc 0
--                 Emit.mov Quadruple (LocPtr arrReg 0x08) (LocReg tmpReg) ("load data (indirect)")
--                 return $ LocPtrCmplx tmpReg idxReg idxOffset elemSize
--             (LocReg arrReg, LocImm idx) -> do
--                 let (LocReg tmpReg) = argLoc 0
--                 Emit.mov Quadruple (LocPtr arrReg 0x08) (LocReg tmpReg) ("load data (indirect)")
--                 return $ LocPtr tmpReg (fromIntegral idx * sizeInBytes elemSize + idxOffset)
--             _ -> error $ "internal error. invalid src loc for elemptr " ++ show arrSrc ++ ", " ++ show idxSrc
--     PFld _ _ val (QIdent _ cli fldi) -> do
--         src <- getValLoc val
--         cl <- getClass cli
--         case Map.lookup fldi (clFlds cl) of
--             Just fld -> case src of
--                 LocReg reg_ -> do
--                     Emit.mov Quadruple (LocPtr reg_ 0x08) (LocReg rax) ("load data (indirect)")
--                     return $ LocPtr rax (fldOffset fld)
--                 _ -> error $ "internal error. invalid src loc for fldptr " ++ show src
--             Nothing -> error $ "internal error. no such field " ++ toStr cli ++ "." ++ toStr fldi
--     PParam _ _ n _ -> return $ argLoc n
--     PLocal _ _ n   -> return $ LocPtr rbp (fromInteger $ (n + 1) * (-8))

