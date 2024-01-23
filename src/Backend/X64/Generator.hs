{-# LANGUAGE FlexibleInstances #-}
module Backend.X64.Generator where

import qualified Backend.X64.Parser.Constructor as X64

import Control.Lens hiding (Const)

import Data.List
import Data.Ord
import           Control.Monad.Reader
import           Control.Monad.State
import           Data.Bifunctor                (Bifunctor (second))
import           Data.Int
import           Data.List                     (partition)
import qualified Data.Map                      as Map
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
import           IR.RegisterAllocation.RegisterAllocation
import           IR.Size
import qualified Data.Map as M

import Backend.X64.Env
import Backend.X64.Def

import qualified Backend.X64.Parser.Constructor as X64
import Control.Monad.Except
import qualified Reporting.Errors.Def as Errors
import Reporting.Logs
import Control.Monad.Trans.Writer
import Data.Foldable

import IR.Flow.CFG

import qualified Backend.X64.Optimizer as Optimizer

generate :: (Show a) => Metadata a -> [(CFG a Liveness, Method a, RegisterAllocation)] -> LattePipeline String
generate meta methods = do
    let externs = runtimeSymbols
    result <- X64.runASMGeneratorT (runExceptT $ runReaderT (execStateT (genProgram meta methods) emptyGeneratorEnv) emptyGeneratorContext) externs Optimizer.optimizeASM 
    case result of
        (Left err) -> do
            printLogInfoStr $ "generate failure. ASM generator reported an error: " ++ (show err)
            return ""
        (Right (_, Left err)) -> do
            printLogInfoStr $ "generate failure. ASM generator reported an error: " ++ (show err)
            return ""
        (Right (compiledCodeStr, Right _)) -> do
            return compiledCodeStr


genProgram :: (Show a) => Metadata a -> [(CFG a Liveness, Method a, RegisterAllocation)] -> Generator a ()
genProgram (Meta pos clDefs) mthds = do
    compiledClasses <- mapM compileClass clDefs
    let compiledClassesMap = Map.fromList $ map (\cl -> (clName cl, cl)) compiledClasses
    cs <- foldrM (emitMethod compiledClassesMap) (constsEmpty) mthds
    genData pos compiledClasses cs
    where 
        compileClass :: ClassDef a -> Generator a CompiledClass
        compileClass (ClDef _ i@(IRTargetRefName className) chain fldDefs mthdDefs) = do
            let (flds, unalignedSize) = layoutFields fldDefs
            vTable <- generateVTable className mthdDefs chain
            return $ CompiledCl i (Map.fromList $ map (\f -> (fldName f, f)) flds) (alignSize unalignedSize) vTable chain
        getVTableRelPos :: [String] -> [String] -> String -> String -> Int
        getVTableRelPos _ _ _ "equals" = 3
        getVTableRelPos _ _ _ "getHashCode" = 2
        getVTableRelPos _ _ _ "toString" = 1
        getVTableRelPos allNames chainNames cls m =
            case (cls, elemIndex cls (reverse chainNames), elemIndex m allNames) of
                (_, Just pos, Just mpos) -> 1000000+pos*1000+mpos
                ("", _, Just mpos) -> 1000000+mpos
                ("", _, _) -> 1000000
                (_, Nothing, _) -> error $ "Cannot correctly layout missing class in class inheritance hierarchy: Class "++show cls++" for linearized hierarchy "++show chainNames
        generateVTable :: String -> [MethodDef a] -> [IRTargetRefName] -> Generator a VTable
        generateVTable className mthdDefs chain = do
            gen $ lift $ lift $ printLogInfoStr $ "generateVTable " ++ show className ++ " START\n " ++ (show $ map (\(MthdDef _ parName _ qi@(QIdent _ (IRTargetRefName clsName) si@(IRTargetRefName methodName))) -> (parName, clsName, methodName)) mthdDefs) ++ " \n START for chain: \n"++(show chain) ++ "\n====\n"
            let chainNames = map (\(IRTargetRefName name) -> name) chain
            let allNames = map (\(MthdDef _ parName _ qi@(QIdent _ (IRTargetRefName clsName) si@(IRTargetRefName methodName))) -> methodName) mthdDefs
            let lookupList = zipWith (\(MthdDef _ parName _ qi@(QIdent _ (IRTargetRefName clsName) si@(IRTargetRefName methodName))) idx -> let (ct, cls, mtd) = getCallTarget qi in (si, (getVTableRelPos allNames chainNames parName methodName, ct, idx))) mthdDefs [0..] 
            gen $ lift $ lift $ printLogInfoStr $ "generateVTable fields: " ++ (show lookupList) ++ " for chain: "++(show chain)
            let lookupList' = map (\(newIndex, (si, (score, name, _))) -> (si, (name, newIndex * 8))) $ zip [0..] $ sortBy (\(si, (score, name, idx)) (si2, (score2, name2, idx2)) -> if score == score2 then compare name name2 else compare score score2) lookupList
            gen $ lift $ lift $ printLogInfoStr $ "generateVTable " ++ show className ++ " RESULT IS = " ++ (show lookupList') 
            return $ VTab (map snd lookupList') (Map.fromList lookupList')
        alignSize :: Int64 -> Int64
        alignSize n = if n `mod` 8 == 0
                        then n
                        else n + (8 - n `mod` 8)
        layoutFields :: [FieldDef a] -> ([CompiledField], Int64)
        layoutFields fldDefs =
            let fldBase = map (\(FldDef _ t sym) -> Fld sym (() <$ t) 0) fldDefs
            in  foldl' go ([], 0) fldBase
            where
                go (flds, offset) fld =
                    let fldSize = X64.toBytes (typeSize (fldType fld))
                        padding = if offset `mod` fldSize == 0
                                    then 0
                                    else fldSize - (offset `mod` fldSize)
                    in (fld{fldOffset = offset + padding}:flds, offset + padding + fldSize)


genData :: (Show a) => a -> [CompiledClass] -> ConstSet -> Generator a ()
genData pos cls allConsts = do
        mapM_ (\cns -> gen $ X64.dataDef pos $ X64.DataDef (constName cns) [X64.DataStr $ constValue cns]) $ constsElems allConsts
        mapM_ (genClassDef pos) cls
        genNullRefCheck pos
        gen $ X64.dataDef pos $ X64.DataGlobal "main"
        where
            genConst :: a -> Const -> Generator a ()
            genConst pos const = do
                gen $ X64.dataDef pos $ X64.DataDef (constName const) [X64.DataStr $ constValue const]
                return ()
            genNullRefCheck :: a -> Generator a ()
            genNullRefCheck pos = do
                let (IRLabelName nullrefLabelStr) = nullrefLabel
                gen $ X64.label pos nullrefLabelStr $ comment "runtime error on null dereference"
                gen $ X64.and pos X64.Size64 (X64.LocConst (-16)) (X64.LocReg X64.RSP) $ comment "16 bytes allign"
                gen $ X64.call pos "__errorNull" Nothing
                return ()
            genClassDef :: a -> CompiledClass -> Generator a ()
            genClassDef pos cls = do
                let clsName = clName cls
                case toStr clsName of
                    "~cl_TopLevel" -> return ()
                    className -> do
                        let chain = clChain cls
                        let (IRLabelName classDefName) = classDefIdent clsName
                        let (IRLabelName methodsTableName) = vTableIRLabelName clsName
                        let (IRLabelName parentName) = if length chain == 1 then (IRLabelName "0") else classDefIdent $ chain!!1
                        gen $ X64.dataDef pos $ X64.DataGlobal classDefName
                        gen $ X64.dataDef pos $ X64.DataDef classDefName [X64.Data64From parentName, X64.Data32I $ fromIntegral $ clSize cls, X64.Data64From $ methodsTableName, X64.Data32I 0, X64.Data64I 0]
                        gen $ X64.dataDef pos $ X64.DataGlobal methodsTableName
                        gen $ X64.dataDef pos $ X64.DataDef methodsTableName $ map (\(name, _) -> X64.Data64From name)  (vtabMthds $ clVTable cls)
                        return ()

emitMethod :: (Show a) => (M.Map IRTargetRefName CompiledClass) -> (CFG a Liveness, Method a, RegisterAllocation) -> (ConstSet) -> Generator a ConstSet
emitMethod classMap (CFG g, m@(Mthd pos _ qi _ _), rs) cs = do
    let initStack = stackNew (numLocals rs)
        initState = GeneratorEnv [] [] cs initStack Map.empty emptyLiveness 0
    result <- gen $ lift $ lift $ X64.execASMGeneratorT (runExceptT $ runReaderT (runStateT (genMethod pos g qi rs) initState) (GeneratorContext (labelFor qi) rs classMap))
    case result of 
        (Left err) -> do
            gen $ lift $ lift $ printLogInfoStr $ "emitMethod failure. ASM generator reported an error: " ++ (show err)
            return cs
        (Right ((Left err), _)) -> do
            gen $ lift $ lift $ printLogInfoStr $ "emitMethod failure. ASM generator reported an error: " ++ (show err)
            return cs
        (Right ((Right (offset, env)), cnt)) -> do
            -- FIX METHOD OFFSET
            gen $ lift $ lift $ printLogInfoStr $ "emitMethod Fix method stack offsets " ++ show offset
            let cnt' = X64.mapLoc (fixMethodOffsetsStack offset) cnt
            gen $ X64.continueASMGeneratorT cnt'
            return $ env ^. consts
    where
        fixMethodOffsetsStack :: Int -> X64.Loc -> X64.Loc
        fixMethodOffsetsStack offset (X64.LocMem (X64.RBP, addrOffset)) = (X64.LocMem (X64.RBP, fromIntegral offset + addrOffset))
        fixMethodOffsetsStack _ loc = loc
        genMethod :: (Show a) => a -> (Map.Map IRLabelName (Node a Liveness)) -> QIdent a -> RegisterAllocation -> Generator a Int
        genMethod pos g qi rs = do
            traceM' ("register allocation: " ++ show (Map.toList $ regAlloc rs))
            traceM' ("========== starting method: " ++ toStr (labelFor qi (IRLabelName "")))
            let nodes = Map.elems g
                entryNode = single $ filter ((== entryLabel) . (^.nodeLabel)) nodes
                exitNode = single $ filter (any isRet . (^.nodeBody)) nodes
                otherNodes = filter ((\l -> l /= (entryNode^.nodeLabel) && l /= (exitNode^.nodeLabel)) . (^.nodeLabel)) nodes
            -- Prologue
            let savedRegs = sort $ filter (\r -> X64.regType r == X64.CalleeSaved) $ usedRegs rs
            let needsAlignment = odd $ length savedRegs
            let locs = fromIntegral $ numLocals rs * 8
            let (IRLabelName mainEntryStr) = labelFor (QIdent () (IRTargetRefName "~cl_TopLevel") (IRTargetRefName "main")) entryLabel
            let (IRLabelName entryStr) = labelFor qi entryLabel
            let (IRLabelName labStr) = labelFor qi (IRLabelName "")
            when (entryStr == mainEntryStr) (gen $ X64.label pos "main" Nothing)
            gen $ X64.label pos labStr Nothing
            mapM_ (\r -> gen $ X64.push pos X64.Size64 (X64.LocReg r) Nothing) savedRegs
            when (needsAlignment) (incrStack pos 8 "16 bytes alignment")
            gen $ X64.push pos X64.Size64 (X64.LocReg X64.RBP) Nothing
            gen $ X64.mov pos X64.Size64 (X64.LocReg X64.RSP) (X64.LocReg X64.RBP) Nothing
            incrStack pos locs "space for locals"
            -- FIXME: IMPORTANT OFFSET FIX
            let paramOffset = 8 * (length savedRegs + 1 + if needsAlignment then 1 else 0)
            -- End
            genNode entryNode
            mapM_ genNode otherNodes
            when ((entryNode^.nodeLabel) /= (exitNode^.nodeLabel)) (genNode exitNode)
            -- Epilogue
            gen $ X64.leave pos Nothing
            when (needsAlignment) (decrStack pos 8)
            mapM_ (\r -> gen $ X64.pop pos (X64.LocReg r) Nothing) (reverse savedRegs)
            gen $ X64.ret pos Nothing
            return paramOffset
        genNode :: (Show a) => Node a Liveness -> Generator a ()
        genNode node = do
            traceM' ("===== starting block: " ++ toStr (node ^. nodeLabel))
            mapM_ genInstr (node ^. nodeBody)
            gEnvSet (\env -> env & allCode %~ ((++) (env ^. code)) & code .~ [])
        isRet instr = case instr of
            IRet _ _ -> True
            IVRet _  -> True
            _        -> False

genInstr :: (Show a) => Instr (a, Liveness) -> Generator a ()
genInstr baseInstr =
    let pos = fst $ single baseInstr in
    let instr = fmap (fst) baseInstr in
    let instrLiveness = snd $ single baseInstr
    in do
        updateLive instrLiveness
        traceM' (show instr)
        fullTrace
        case instr of
            ILabel _ l -> do
                (IRLabelName l') <- label l
                gen $ X64.label pos l' Nothing
            ILabelAnn _ l f t -> do
                (IRLabelName l') <- label l
                gen $ X64.label pos l' $ comment $ "lines " ++ show f ++ "-" ++ show t
            IVRet _ -> do
                resetStack pos
            IRet _ val -> do
                loc <- getValLoc val
                gen $ X64.mov pos (valSize val) loc (X64.LocReg X64.RAX) $ comment "move return value"
                resetStack pos
            IOp _ vi v1 op v2 -> do
                dest <- getLoc vi
                src1 <- getValLoc v1
                src2 <- getValLoc v2
                let size = valSize v1
                case op of
                    OpAdd _ | isInt (valType v1) -> do
                        if dest == src1 then
                          gen $ X64.add pos X64.Size32 src2 dest Nothing
                        else if dest == src2 then
                          gen $ X64.add pos X64.Size32 src1 dest Nothing
                        else case (src1, src2) of
                            (X64.LocReg r1, X64.LocReg r2) ->
                                gen $ X64.lea pos X64.Size32 (X64.LocMemOffset r1 r2 0 X64.Size8) dest (comment $ "addition " ++ toStr vi)
                            (X64.LocConst n1, X64.LocReg r2) ->
                                gen $ X64.lea pos X64.Size32 ( X64.LocMem (r2, fromIntegral n1)) dest (comment $ "addition " ++ toStr vi)
                            (X64.LocReg r1, X64.LocConst n2) ->
                                gen $ X64.lea pos X64.Size32 ( X64.LocMem (r1, fromIntegral n2)) dest (comment $ " addition " ++ toStr vi)
                            _ -> error "internal error. invalid src locs in add"
                    OpAdd _ | isStr (valType v1) -> do
                        genCall pos (CallDirect "lat_cat_strings") [v1, v2] [] (gen $ X64.mov pos X64.Size64 (X64.LocReg X64.RAX) dest Nothing)
                    OpAdd _ -> error "internal error. invalid operand types for add."
                    OpSub _ -> do
                        if dest == src1 then
                          gen $ X64.sub pos X64.Size32 src2 dest Nothing
                        else if dest == src2 then do
                          gen $ X64.sub pos X64.Size32 src1 dest Nothing
                          gen $ X64.neg pos X64.Size32 dest Nothing
                        else do
                          gen $ X64.mov pos X64.Size32 src1 dest Nothing
                          gen $ X64.sub pos X64.Size32 src2 dest Nothing
                    OpMul _ -> do
                        case (src1, src2) of
                            (X64.LocConst n, _) | isPowerOfTwo n -> do
                                when (dest /= src2) (gen $ X64.mov pos X64.Size32 src2 dest Nothing)
                                gen $ X64.sal pos X64.Size32 (X64.LocConst $ fromIntegral $ log2 $ fromIntegral n) dest (comment $ "multiply by " ++ show n ++ " src1="++show src1++" src2="++show src2++" dest="++show dest)
                            (_, X64.LocConst n) | isPowerOfTwo n -> do
                                when (dest /= src1) (gen $ X64.mov pos X64.Size32 src1 dest Nothing)
                                gen $ X64.sal pos X64.Size32 (X64.LocConst $ fromIntegral $ log2 $ fromIntegral n) dest (comment $ "multiply by " ++ show n ++ " src1="++show src1++" src2="++show src2++" dest="++show dest)
                            _ -> do
                                if dest == src1 then
                                  gen $ X64.imul pos X64.Size32 src2 dest Nothing
                                else if dest == src2 then
                                  gen $ X64.imul pos X64.Size32 src1 dest Nothing
                                else do
                                  gen $ X64.mov pos X64.Size32 src1 dest Nothing
                                  gen $ X64.imul pos X64.Size32 src2 dest Nothing
                    OpDiv _ -> do
                        case src2 of
                            X64.LocConst n | isPowerOfTwo n -> do
                                gen $ X64.mov pos X64.Size32 src1 dest Nothing
                                gen $ X64.sar pos X64.Size32 (X64.LocConst $ fromIntegral $ log2 $ fromIntegral n) dest (comment $ "divide by " ++ show n)
                            X64.LocConst {} -> do
                                gen $ X64.mov pos X64.Size32 src1 (X64.LocReg X64.RAX) Nothing
                                gen $ X64.cdq pos Nothing
                                gen $ X64.push pos X64.Size64 src1 Nothing
                                gen $ X64.mov pos X64.Size32 src2 src1 Nothing
                                gen $ X64.idiv pos X64.Size32 src1 Nothing
                                gen $ X64.pop pos src1 Nothing
                                gen $ X64.mov pos X64.Size32 (X64.LocReg X64.RAX) dest Nothing
                            _ -> do
                                gen $ X64.mov pos X64.Size32 src1 (X64.LocReg X64.RAX) Nothing
                                gen $ X64.cdq pos Nothing
                                gen $ X64.idiv pos X64.Size32 src2 Nothing
                                gen $ X64.mov pos X64.Size32 (X64.LocReg X64.RAX) dest Nothing
                    OpMod _ -> do
                        case src2 of
                            X64.LocConst n | isPowerOfTwo n -> do
                                gen $ X64.mov pos X64.Size32 src1 dest Nothing
                                gen $ X64.and pos X64.Size32 (X64.LocConst (n - 1)) dest (comment $ "modulo by " ++ show n)
                            X64.LocConst {} -> do
                                gen $ X64.mov pos X64.Size32 src1 (X64.LocReg X64.RAX) Nothing
                                gen $ X64.cdq pos Nothing
                                gen $ X64.push pos X64.Size64 src1 Nothing
                                gen $ X64.mov pos X64.Size32 src2 src1 Nothing
                                gen $ X64.idiv pos X64.Size32 src1 Nothing
                                gen $ X64.pop pos  src1 Nothing
                                gen $ X64.mov pos X64.Size32 (X64.LocReg X64.RDX) dest Nothing
                            _ -> do
                                gen $ X64.mov pos X64.Size32 src1 (X64.LocReg X64.RAX) Nothing
                                gen $ X64.cdq pos Nothing
                                gen $ X64.idiv pos X64.Size32 src2 Nothing
                                gen $ X64.mov pos X64.Size32 (X64.LocReg X64.RDX) dest Nothing
                    OpLTH _ -> emitCmpBin pos op dest src1 src2 size
                    OpLE _  -> emitCmpBin pos op dest src1 src2 size
                    OpGTH _ -> emitCmpBin pos op dest src1 src2 size
                    OpGE _  -> emitCmpBin pos op dest src1 src2 size
                    OpEQU _ -> emitCmpBin pos op dest src1 src2 size
                    OpNE _  -> emitCmpBin pos op dest src1 src2 size
            ISet _ vi v -> do
                let t = valType v
                dest <- getLoc vi
                src <- getValLoc v
                gen $ X64.mov pos (typeSize t) src dest $ comment $ "setting " ++ toStr vi
            ISwap _ t vi1 vi2 -> do
                loc1 <- getLoc vi1
                loc2 <- getLoc vi2
                gen $ X64.xchg pos (typeSize t) loc1 loc2 Nothing
            IUnOp _ vi op v -> do
                let t = valType v
                src <- getValLoc v
                dest <- getLoc vi
                gen $ X64.mov pos (typeSize t) src dest $ comment $ "setting " ++ toStr vi
                case op of
                    UnOpNeg _ -> gen $ X64.neg pos X64.Size32 dest Nothing
                    UnOpNot _ -> gen $ X64.xor pos X64.Size8 (X64.LocConst 1) dest Nothing
            IVCall _ call -> case call of
                    Call _ _ qi args largs    -> genCall pos (CallDirect $ getCallTargetStr qi) args largs (return ())
                    CallVirt _ _ qi args -> genCallVirt qi args (return ())
            ICall _ vi call -> do
                dest <- getLoc vi
                case call of
                        Call _ t qi args largs    -> genCall pos (CallDirect $ getCallTargetStr qi) args largs (gen $ X64.mov pos (typeSize t) (X64.LocReg X64.RAX) dest Nothing)
                        CallVirt _ t qi args -> genCallVirt qi args (gen $ X64.mov pos (typeSize t) (X64.LocReg X64.RAX) dest Nothing)
            ILoad _ vi ptr -> do
                let t = () <$ deref (ptrType ptr)
                --let (X64.LocReg tmpReg) = if dest /= X64.argLoc 0 then X64.argLoc 0 else X64.argLoc 1
                dest <- getLoc vi
                (X64.LocReg tmpReg, doPush) <- case dest of
                    X64.LocReg r -> return (X64.LocReg r, False)
                    d | d /= X64.argLoc 0 -> return (X64.argLoc 0, True)
                    _ -> return (X64.argLoc 1, True)
                (src, finalizeLoad) <- getPtrLoc tmpReg doPush ptr
                gen $ X64.mov pos (typeSize t) src dest (comment $ "load " ++ toStr vi)
                finalizeLoad
            IStore _ v ptr -> do
                let t = valType v
                src <- getValLoc v
                (X64.LocReg tmpReg, doPush) <- case src of
                    d | d /= X64.argLoc 0 -> return (X64.argLoc 0, True)
                    _ -> return (X64.argLoc 1, True)
                (dest, finalizeLoad) <- getPtrLoc tmpReg doPush ptr
                gen $ X64.mov pos (typeSize t) src dest Nothing
                finalizeLoad
            INew _ vi t -> case t of
                Cl _ clIdent -> do
                    cl <- getClass clIdent
                    dest <- getLoc vi
                    let sizeArg = VInt () (toInteger $ clSize cl)
                        (X64.LocReg tmpReg) = X64.argLoc 0
                    let clLabel = classDefIdent clIdent
                    genCall pos (CallDirect "__new") [] [clLabel] (do
                        gen $ X64.mov pos X64.Size64 (X64.LocReg X64.RAX) dest Nothing)
                _ -> error $ "internal error. new on nonclass " ++ show t
            INewStr _ vi str -> do
                let t = Ref () strType
                dest <- getLoc vi
                strConst <- newStrConst str
                case dest of
                    X64.LocReg reg_ -> gen $ X64.lea pos X64.Size64 (X64.LocLabelPIC $ constName strConst) (X64.LocReg reg_) Nothing
                    _ -> error $ "internal error. invalid dest loc " ++ show dest
                genCall pos (CallDirect "__createString") [VVal () t vi] [] (gen $ X64.mov pos X64.Size64 (X64.LocReg X64.RAX) dest Nothing)
            INewArr _ vi t val -> do
                dest <- getLoc vi
                case t of 
                    (Int _) -> genCall pos (CallDirect "__newIntArray") [() <$ val] [] (gen $  X64.mov pos X64.Size64 (X64.LocReg X64.RAX) dest Nothing)
                    (Bool _) -> genCall pos (CallDirect "__newByteArray") [() <$ val] [] (gen $ X64.mov pos X64.Size64 (X64.LocReg X64.RAX) dest Nothing)
                    (Void _) -> genCall pos (CallDirect "__newArray") [VInt () 0, () <$ val] [] (gen $ X64.mov pos X64.Size64 (X64.LocReg X64.RAX) dest Nothing)
                    (Cl _ name) -> error $ "internal error. cannot create array of class type " ++ show name
                    (Ref _ _) -> genCall pos (CallDirect "__newRefArray") [() <$ val] [] (gen $ X64.mov pos X64.Size64 (X64.LocReg X64.RAX) dest Nothing)
                    _ -> error $ "internal error. invalid array type " ++ show t
            IJmp _ li -> do
                (IRLabelName li') <- label li
                resetStack pos
                gen $ X64.jmp pos li' Nothing
            ICondJmp _ v l1 l2 -> do
                loc <- getValLoc v
                (IRLabelName l1') <- label l1
                (IRLabelName l2') <- label l2
                resetStack pos
                case loc of
                    X64.LocConst 0 -> do
                        gen $ X64.jmp pos l2' Nothing
                    X64.LocConst 1 -> do
                        gen $ X64.jmp pos l1' Nothing
                    _ -> do
                        gen $ X64.test pos X64.Size8 loc loc Nothing
                        gen $ X64.jz pos l2' Nothing
                        gen $ X64.jmp pos l1' Nothing
            IPhi {} -> error "internal error. phi should be eliminated before assembly codegen"
            IEndPhi {} -> return ()
        fullTrace
        return ()

data CallTarget = CallDirect String | CallVirtual Int64 String

data CallArg a = CallArgVal (Val a) | CallArgLabel (IRLabelName)

incrStack :: a -> Int64 -> String -> Generator a ()
incrStack pos n comment_ = gen $ X64.sub pos X64.Size64 (X64.LocConst $ fromIntegral n) (X64.LocReg X64.RSP) Nothing

decrStack :: a -> Int64 -> Generator a ()
decrStack pos n = gen $ X64.add pos X64.Size64 (X64.LocConst $ fromIntegral n) (X64.LocReg X64.RSP) Nothing

genCall :: a -> CallTarget -> [Val b] -> [IRLabelName] -> Generator a () -> Generator a ()
genCall pos target varArgs labelArgs cont = do
        let args = (map CallArgVal varArgs) ++ (map CallArgLabel labelArgs)
        regs_ <- getPreservedRegs
        let argsWithLocs = zip args (map X64.argLoc [0..])
            (argsWithLocReg,  argsWithLocStack) = partition (X64.isReg . snd) argsWithLocs
            argsInRegs = map (second X64.asReg) argsWithLocReg
            argsOnStack = map fst argsWithLocStack
            savedRegs = filter ((== X64.CallerSaved) . X64.regType) regs_
        forM_ savedRegs (\r -> gen $ X64.push pos X64.Size64 (X64.LocReg r) $ comment "save caller saved")
        forM_ argsInRegs (uncurry (passInReg pos))
        stackBefore <- gEnvGet (stackOverheadSize .  (^. stack))
        locs <- mapM prepOnStack (reverse argsOnStack)
        alignStack
        stackAfter <- gEnvGet (stackOverheadSize . (^. stack))
        forM_ locs (\l -> gen $ X64.push pos X64.Size64 l (comment "passing arg"))
        let (IRLabelName nullRefLabelStr) = nullrefLabel
        case target of
            CallDirect l              -> gen $ X64.call pos l Nothing
            CallVirtual offset s -> do
                let self@(X64.LocReg selfReg) = X64.argLoc 0
                gen $ X64.test pos X64.Size64 self self Nothing
                gen $ X64.jz pos nullRefLabelStr Nothing
                gen $ X64.mov pos X64.Size64 (X64.LocMem (selfReg, 20)) (X64.LocReg X64.RAX) $ comment $ "load address of vtable"
                --X64.mov X64.Size64 ( X64.LocMem (X64.RAX 12) (X64.LocReg selfReg) "load address of vtable"
                gen $ X64.callIndirect pos X64.RAX (fromIntegral offset) (comment $ "call " ++ s)
        decrStack pos (stackAfter - stackBefore)
        gEnvSet (\env -> env & stack %~ (\s -> s {stackOverheadSize = stackBefore}))
        cont
        forM_ (reverse savedRegs) (\r -> gen $ X64.pop pos (X64.LocReg r) Nothing)
    where
          passInReg :: a -> CallArg b -> X64.Reg -> Generator a ()
          passInReg pos (CallArgLabel l) reg_ = do
            gen $ X64.lea pos X64.Size64 (X64.LocLabelPIC $ toStr l) (X64.LocReg reg_) Nothing
            --X64.mov (Quadruple) (LocLabel l) (X64.LocReg reg_) "passing label arg"
          passInReg pos (CallArgVal val) reg_ = do
            loc <- getValLoc val
            gen $ X64.mov pos (valSize val) loc (X64.LocReg reg_) $ comment $ "passing arg"
          prepOnStack :: CallArg b -> Generator a X64.Loc
          prepOnStack ((CallArgLabel (IRLabelName l))) = do
              s <- gEnvGet (^. stack)
              let s' = stackPush X64.Size64 s
              setStack s'
              return (X64.LocLabel l)
          prepOnStack (CallArgVal val) = do
              s <- gEnvGet (^. stack)
              loc <- getValLoc val
              let s' = stackPush X64.Size64 s
              setStack s'
              return loc
          alignStack = do
              (misalignment, s) <- gEnvGet (stackAlign16 . (^. stack))
              incrStack pos misalignment "16 bytes alignment"
              setStack s

genCallVirt :: QIdent a -> [Val a] -> Generator a () -> Generator a ()
genCallVirt _ [] _ = error "internal error. callvirt with no args"
genCallVirt (QIdent pos cli i) args cont = do
    cl <- getClass cli
    let offset = case Map.lookup i $ vtabMthdMap $ clVTable cl of
                    Just (_, n) -> n
                    Nothing     -> error ""
    genCall pos (CallVirtual offset (toStr i)) args [] cont

emitCmpBin :: a -> Op a -> X64.Loc -> X64.Loc -> X64.Loc -> X64.Size -> Generator a ()
emitCmpBin pos op dest src1 src2 size = do
    case src1 of
        X64.LocConst {} -> do
            gen $ X64.cmp pos size src1 src2 Nothing
            revCmpEmitter op dest  Nothing
        _ -> do
            gen $ X64.cmp pos size src2 src1 Nothing
            cmpEmitter op dest Nothing

cmpEmitter :: Op a -> X64.Loc -> Maybe ASMAnno -> Generator a ()
cmpEmitter op = case op of
    OpLTH pos -> \l a -> gen $ X64.setl pos l a
    OpLE pos  -> \l a -> gen $ X64.setle pos l a
    OpGTH pos -> \l a -> gen $ X64.setg pos l a
    OpGE pos  -> \l a -> gen $ X64.setge pos l a
    OpEQU pos -> \l a -> gen $ X64.sete pos l a
    OpNE pos  -> \l a -> gen $ X64.setne pos l a
    _       -> error "internal error. invalid op to cmpEmitter."

revCmpEmitter :: Op a -> X64.Loc -> Maybe ASMAnno -> Generator a ()
revCmpEmitter op = case op of
    OpLTH pos -> \l a -> gen $ X64.setge pos l a
    OpLE pos  -> \l a -> gen $ X64.setg pos l a
    OpGTH pos -> \l a -> gen $ X64.setle pos l a
    OpGE pos  -> \l a -> gen $ X64.setl pos l a
    OpEQU pos -> \l a -> gen $ X64.sete pos l a -- FIXME: ????
    OpNE pos  -> \l a -> gen $ X64.setne pos l a
    _       -> error "internal error. invalid op to cmpEmitter."

resetStack :: a -> Generator a ()
resetStack pos = do
    s <- gEnvGet (^. stack)
    let (n, s') = stackClearOverhead s
    decrStack pos n
    setStack s'

getPtrLoc :: X64.Reg -> Bool -> Ptr a -> Generator a (X64.Loc, Generator a ())
getPtrLoc tmpReg doPush ptr = case ptr of
    PArrLen _ val -> do
        src <- getValLoc val
        case src of
            X64.LocReg reg_ -> return (X64.LocMem (reg_, 32), return ())
            _ -> error $ "internal error. invalid src loc for arrlen " ++ show src
    PElem pos elemT arrVal idxVal -> do
        let elemSize = typeSize $ deref elemT
            idxOffset = 0
        arrSrc <- getValLoc arrVal
        idxSrc <- getValLoc idxVal
        case (arrSrc, idxSrc) of
            (X64.LocReg arrReg, X64.LocReg idxReg) -> do
                when (doPush) (gen $ X64.push pos X64.Size64 (X64.LocReg tmpReg) Nothing)
                gen $ X64.mov pos X64.Size64 ( X64.LocMem (arrReg, 8)) (X64.LocReg tmpReg) (comment $ "load obj->data")
                return (X64.LocMemOffset tmpReg idxReg idxOffset elemSize, when (doPush) (gen $ X64.pop pos (X64.LocReg tmpReg) Nothing))
            (X64.LocReg arrReg, X64.LocConst idx) -> do
                when (doPush) (gen $ X64.push pos X64.Size64 (X64.LocReg tmpReg) Nothing)
                gen $ X64.mov pos X64.Size64 ( X64.LocMem (arrReg, 8)) (X64.LocReg tmpReg) (comment $ "load obj->data")
                return (X64.LocMem (tmpReg, fromIntegral idx * X64.toBytes elemSize + idxOffset), when (doPush) (gen $ X64.pop pos (X64.LocReg tmpReg) Nothing))
            _ -> error $ "internal error. invalid src loc for elemptr " ++ show arrSrc ++ ", " ++ show idxSrc
    PFld pos _ val (QIdent _ cli fldi) -> do
        src <- getValLoc val
        cl <- getClass cli
        case Map.lookup fldi (clFlds cl) of
            Just fld -> case src of
                X64.LocReg reg_ -> do
                    let offset = if(fldName fld) == (IRTargetRefName "length") then 32 else 36 + fldOffset fld
                    --gen $ X64.mov pos X64.Size64 ( X64.LocMem (reg_, 0x08)) (X64.LocReg X64.RAX) (comment $ "load data (indirect)")
                    --return $  X64.LocMem (X64.RAX, fldOffset fld)
                    return $ (X64.LocMem (reg_, offset), return ())
                _ -> error $ "internal error. invalid src loc for fldptr " ++ show src
            Nothing -> error $ "internal error. no such field " ++ toStr cli ++ "." ++ toStr fldi
    PParam _ _ n _ -> return $ (X64.argLoc n, return ())
    PLocal _ _ n   -> return $ (X64.LocMem (X64.RBP, fromInteger $ (n + 1) * (-8)), return ())
