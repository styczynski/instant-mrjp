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

-- genInstr :: Instr Liveness -> Generator ()
-- genInstr instr = do
--     liftGenerator $ X64.mov () X64.Size64 (X64.LocReg X64.RAX) (X64.LocReg X64.RDX)

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
--                 liftGenerator $ X64.label () l' ""
--             ILabelAnn _ l f t -> do
--                 l' <- label l
--                 liftGenerator $ X64.label () l' $ "lines " ++ show f ++ "-" ++ show t
--             IVRet _ -> do
--                 resetStack
--             IRet _ val -> do
--                 loc <- getValLoc val
--                 liftGenerator $ X64.mov () (valSize val) loc (convertReg rax) "move return value"
--                 resetStack
--             IOp _ vi v1 op v2 -> do
--                 dest <- getLoc vi
--                 src1 <- getValLoc v1
--                 src2 <- getValLoc v2
--                 let size = valSize v1
--                 case op of
--                     OpAdd _ | isInt (valType v1) -> do
--                         if dest == src1 then
--                           liftGenerator $ X64.add () src2 dest ""
--                         else if dest == src2 then
--                           liftGenerator $ X64.add () src1 dest ""
--                         else case (src1, src2) of
--                             (LocReg r1, LocReg r2) ->
--                                 liftGenerator $ X64.lea () Double (LocPtrCmplx r1 r2 0 Byte) dest ("addition " ++ toStr vi)
--                             (LocImm n1, LocReg r2) ->
--                                 liftGenerator $ X64.lea () Double (LocPtr r2 (fromIntegral n1)) dest ("addition " ++ toStr vi)
--                             (LocReg r1, LocImm n2) ->
--                                 liftGenerator $ X64.lea () Double (LocPtr r1 (fromIntegral n2)) dest ("addition " ++ toStr vi)
--                             _ -> error "internal error. invalid src locs in add"
--                     OpAdd _ | isStr (valType v1) -> do
--                         genCall (CallDirect "lat_cat_strings") [v1, v2] [] (liftGenerator $ X64.mov () Quadruple (convertReg rax) dest "")
--                     OpAdd _ -> error "internal error. invalid operand types for add."
--                     OpSub _ -> do
--                         if dest == src1 then
--                           liftGenerator $ X64.sub () src2 dest
--                         else if dest == src2 then do
--                           liftGenerator $ X64.sub () src1 dest
--                           liftGenerator $ X64.neg () dest
--                         else do
--                           liftGenerator $ X64.mov () Double src1 dest ""
--                           liftGenerator $ X64.sub () src2 dest
--                     OpMul _ -> do
--                         case (src1, src2) of
--                             (LocImm n, _) | isPowerOfTwo n -> do
--                                 when (dest /= src2) (liftGenerator $ X64.mov () Double src2 dest "")
--                                 liftGenerator $ X64.sal () (log2 n) dest ("multiply by " ++ show n)
--                             (_, LocImm n) | isPowerOfTwo n -> do
--                                 when (dest == src1) (liftGenerator $ X64.mov () Double src1 dest "")
--                                 liftGenerator $ X64.sal () (log2 n) dest ("multiply by " ++ show n)
--                             _ -> do
--                                 if dest == src1 then
--                                   liftGenerator $ X64.imul () src2 dest
--                                 else if dest == src2 then
--                                   liftGenerator $ X64.imul () src1 dest
--                                 else do
--                                   liftGenerator $ X64.mov () Double src1 dest ""
--                                   liftGenerator $ X64.imul () src2 dest
--                     OpDiv _ -> do
--                         case src2 of
--                             LocImm n | isPowerOfTwo n -> do
--                                 liftGenerator $ X64.mov () Double src1 dest ""
--                                 liftGenerator $ X64.sar () (log2 n) dest ("divide by " ++ show n)
--                             LocImm {} -> do
--                                 liftGenerator $ X64.mov () Double src1 (convertReg rax) ""
--                                 liftGenerator $ X64.cdq ()
--                                 liftGenerator $ X64.mov () Double src2 src1 ""
--                                 liftGenerator $ X64.idiv () Double src1
--                                 liftGenerator $ X64.mov () Double (convertReg rax) dest ""
--                             _ -> do
--                                 liftGenerator $ X64.mov () Double src1 (convertReg rax) ""
--                                 liftGenerator $ X64.cdq ()
--                                 liftGenerator $ X64.idiv () Double src2
--                                 liftGenerator $ X64.mov () Double (convertReg rax) dest ""
--                     OpMod _ -> do
--                         case src2 of
--                             LocImm n | isPowerOfTwo n -> do
--                                 -- n % 2^k
--                                 -- is the same as
--                                 -- n AND (2^k - 1)
--                                 liftGenerator $ X64.mov () Double src1 dest ""
--                                 liftGenerator $ X64.and () Double (LocImm (n - 1)) dest ("modulo by " ++ show n)
--                             LocImm {} -> do
--                                 liftGenerator $ X64.mov () Double src1 (convertReg rax) ""
--                                 liftGenerator $ X64.cdq ()
--                                 liftGenerator $ X64.mov () Double src2 src1 ""
--                                 liftGenerator $ X64.idiv () Double src1
--                                 liftGenerator $ X64.mov () Double (convertReg rdx) dest ""
--                             _ -> do
--                                 liftGenerator $ X64.mov () Double src1 (convertReg rax) ""
--                                 liftGenerator $ X64.cdq ()
--                                 liftGenerator $ X64.idiv () Double src2
--                                 liftGenerator $ X64.mov () Double (convertReg rdx) dest ""
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
--                 liftGenerator $ X64.mov () (typeSize t) src dest $ "setting " ++ toStr vi
--             ISwap _ t vi1 vi2 -> do
--                 loc1 <- getLoc vi1
--                 loc2 <- getLoc vi2
--                 liftGenerator $ X64.xchg () (typeSize t) loc1 loc2
--             IUnOp _ vi op v -> do
--                 let t = valType v
--                 src <- getValLoc v
--                 dest <- getLoc vi
--                 liftGenerator $ X64.mov () (typeSize t) src dest $ "setting " ++ toStr vi
--                 case op of
--                     UnOpNeg _ -> liftGenerator $ X64.neg () dest
--                     UnOpNot _ -> liftGenerator $ X64.xor () Byte (LocImm 1) dest
--             IVCall _ call -> case call of
--                     Call _ _ qi args largs    -> genCall (CallDirect $ getCallTarget qi) args largs (return ())
--                     CallVirt _ _ qi args -> genCallVirt qi args (return ())
--             ICall _ vi call -> do
--                 dest <- getLoc vi
--                 case call of
--                         Call _ t qi args largs    -> genCall (CallDirect $ getCallTarget qi) args largs (liftGenerator $ X64.mov () (typeSize t) (LocReg rax) dest "")
--                         CallVirt _ t qi args -> genCallVirt qi args (liftGenerator $ X64.mov () (typeSize t) (LocReg rax) dest "")
--             ILoad _ vi ptr -> do
--                 let t = () <$ deref (ptrType ptr)
--                 src <- getPtrLoc ptr
--                 dest <- getLoc vi
--                 liftGenerator $ X64.mov () (typeSize t) src dest ("load " ++ toStr vi)
--             IStore _ v ptr -> do
--                 let t = valType v
--                 src <- getValLoc v
--                 dest <- getPtrLoc ptr
--                 liftGenerator $ X64.mov () (typeSize t) src dest ""
--             INew _ vi t -> case t of
--                 Cl _ clIdent -> do
--                     cl <- getClass clIdent
--                     dest <- getLoc vi
--                     let sizeArg = VInt () (toInteger $ clSize cl)
--                         (LocReg tmpReg) = argLoc 0
--                     let clLabel = classDefIdent clIdent
--                     genCall (CallDirect "__new") [] [clLabel] (do
--                         --liftGenerator $ X64.leaOfConst () (toStr $ vTableLabIdent clIdent) tmpReg
--                         --liftGenerator $ X64.mov () Quadruple (LocReg tmpReg) (LocPtr rax 0) "store vtable"
--                         liftGenerator $ X64.mov () Quadruple (convertReg rax) dest "")
--                 _ -> error $ "internal error. new on nonclass " ++ show t
--             INewStr _ vi str -> do
--                 let t = Ref () strType
--                 dest <- getLoc vi
--                 strConst <- newStrConst str
--                 case dest of
--                     LocReg reg_ -> liftGenerator $ X64.leaOfConst () (constName strConst) reg_
--                     _ -> error $ "internal error. invalid dest loc " ++ show dest
--                 genCall (CallDirect "__createString") [VVal () t vi] [] (liftGenerator $ X64.mov () Quadruple (convertReg rax) dest "")
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
--                     (Int _) -> genCall (CallDirect "__newIntArray") [() <$ val] [] (liftGenerator $ X64.mov () Quadruple (convertReg rax) dest "")
--                     (Bool _) -> genCall (CallDirect "__newByteArray") [() <$ val] [] (liftGenerator $ X64.mov () Quadruple (convertReg rax) dest "")
--                     (Void _) -> genCall (CallDirect "__newArray") [VInt () 0, () <$ val] [] (liftGenerator $ X64.mov () Quadruple (convertReg rax) dest "")
--                     (Cl _ name) -> error $ "internal error. cannot create array of class type " ++ show name
--                     (Ref _ _) -> genCall (CallDirect "__newRefArray") [() <$ val] [] (liftGenerator $ X64.mov () Quadruple (convertReg rax) dest "")
--                     _ -> error $ "internal error. invalid array type " ++ show t
--             IJmp _ li -> do
--                 li' <- label li
--                 resetStack
--                 liftGenerator $ X64.jmp () li'
--             ICondJmp _ v l1 l2 -> do
--                 loc <- getValLoc v
--                 l1' <- label l1
--                 l2' <- label l2
--                 resetStack
--                 case loc of
--                     LocImm 0 -> do
--                         liftGenerator $ X64.jmp () l2'
--                     LocImm 1 -> do
--                         liftGenerator $ X64.jmp () l1'
--                     _ -> do
--                         liftGenerator $ X64.test () Byte loc loc
--                         liftGenerator $ X64.jz () l2'
--                         liftGenerator $ X64.jmp () l1'
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
--         forM_ savedRegs (\r -> liftGenerator $ X64.push () (convertReg r) "save caller saved")
--         forM_ argsInRegs (uncurry passInReg)
--         stackBefore <- gets (stackOverheadSize . stack)
--         locs <- mapM prepOnStack (reverse argsOnStack)
--         alignStack
--         stackAfter <- gets (stackOverheadSize . stack)
--         forM_ locs (`liftGenerator $ X64.push ()` "passing arg")
--         case target of
--             CallDirect l              -> liftGenerator $ X64.callDirect () l
--             CallVirtual offset s -> do
--                 let self@(convertReg selfReg) = argLoc 0
--                 liftGenerator $ X64.test () Quadruple self self
--                 liftGenerator $ X64.jz () nullrefLabel
--                 liftGenerator $ X64.mov () Quadruple (LocPtr selfReg 20) (convertReg rax) "load address of vtable"
--                 --liftGenerator $ X64.mov () Quadruple (LocPtr rax 12) (LocReg selfReg) "load address of vtable"
--                 liftGenerator $ X64.callAddress () rax offset ("call " ++ s)
--         liftGenerator $ X64.decrStack () (stackAfter - stackBefore)
--         modify (\st -> st{stack = (stack st){stackOverheadSize = stackBefore}})
--         cont
--         forM_ (reverse savedRegs) (liftGenerator $ X64.pop () . LocReg)
--     where
--           passInReg :: CallArg a -> Reg -> Generator ()
--           passInReg (CallArgLabel l) reg_ = do
--             liftGenerator $ X64.leaOfConst () (toStr l) reg_
--             --liftGenerator $ X64.mov () (Quadruple) (LocLabel l) (LocReg reg_) "passing label arg"
--           passInReg (CallArgVal val) reg_ = do
--             loc <- getValLoc val
--             liftGenerator $ X64.mov () (valSize val) loc (convertReg reg_) "passing arg"
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
--               liftGenerator $ X64.incrStack () misalignment "16 bytes alignment"
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
--             liftGenerator $ X64.cmp () size src1 src2
--             revCmpEmitter op dest
--         LocImm64 {} -> do
--             liftGenerator $ X64.cmp () size src1 src2
--             revCmpEmitter op dest
--         _ -> do
--             liftGenerator $ X64.cmp () size src2 src1
--             cmpEmitter op dest

-- cmpEmitter :: Op a -> Loc -> Generator ()
-- cmpEmitter op = case op of
--     OpLTH _ -> liftGenerator $ X64.setl ()
--     OpLE _  -> liftGenerator $ X64.setle ()
--     OpGTH _ -> liftGenerator $ X64.setg ()
--     OpGE _  -> liftGenerator $ X64.setge ()
--     OpEQU _ -> liftGenerator $ X64.sete ()
--     OpNE _  -> liftGenerator $ X64.setne ()
--     _       -> error "internal error. invalid op to cmpEmitter."

-- revCmpEmitter :: Op a -> Loc -> Generator ()
-- revCmpEmitter op = case op of
--     OpLTH _ -> liftGenerator $ X64.setge ()
--     OpLE _  -> liftGenerator $ X64.setg ()
--     OpGTH _ -> liftGenerator $ X64.setle ()
--     OpGE _  -> liftGenerator $ X64.setl ()
--     OpEQU _ -> liftGenerator $ X64.sete ()
--     OpNE _  -> liftGenerator $ X64.setne ()
--     _       -> error "internal error. invalid op to cmpEmitter."

-- resetStack :: Generator ()
-- resetStack = do
--     s <- gets stack
--     let (n, s') = stackClearOverhead s
--     liftGenerator $ X64.decrStack () n
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
--                 liftGenerator $ X64.mov () Quadruple (LocPtr arrReg 0x08) (LocReg tmpReg) ("load data (indirect)")
--                 return $ LocPtrCmplx tmpReg idxReg idxOffset elemSize
--             (LocReg arrReg, LocImm idx) -> do
--                 let (LocReg tmpReg) = argLoc 0
--                 liftGenerator $ X64.mov () Quadruple (LocPtr arrReg 0x08) (LocReg tmpReg) ("load data (indirect)")
--                 return $ LocPtr tmpReg (fromIntegral idx * sizeInBytes elemSize + idxOffset)
--             _ -> error $ "internal error. invalid src loc for elemptr " ++ show arrSrc ++ ", " ++ show idxSrc
--     PFld _ _ val (QIdent _ cli fldi) -> do
--         src <- getValLoc val
--         cl <- getClass cli
--         case Map.lookup fldi (clFlds cl) of
--             Just fld -> case src of
--                 LocReg reg_ -> do
--                     liftGenerator $ X64.mov () Quadruple (LocPtr reg_ 0x08) (LocReg rax) ("load data (indirect)")
--                     return $ LocPtr rax (fldOffset fld)
--                 _ -> error $ "internal error. invalid src loc for fldptr " ++ show src
--             Nothing -> error $ "internal error. no such field " ++ toStr cli ++ "." ++ toStr fldi
--     PParam _ _ n _ -> return $ argLoc n
--     PLocal _ _ n   -> return $ LocPtr rbp (fromInteger $ (n + 1) * (-8))

