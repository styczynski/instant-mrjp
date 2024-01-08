{-# LANGUAGE FlexibleContexts #-}
module Backend.X86.Emitter(emit) where

import Data.List (nub, (\\), sort)
import Data.Char
import Data.Maybe (fromJust, fromMaybe, isJust)
import Data.ByteString (unpack)
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)
import Data.Word (Word8)
import Data.Functor ((<$>))
import Control.Monad.Writer
import Control.Monad.State

import Debug.Trace

import qualified Utils.Containers.IDMap as IM

import qualified Backend.X86.Syntax as ASM
import Linearized.Syntax
import Linearized.Optimizer.Values
import Linearized.Optimizer.Liveness
import Backend.X86.RegisterAllocator

emit :: Program IRPosition -> ASM.Program IRPosition
emit p@(Program pos _ _ _) = ASM.Program pos $ execWriter (emitP p)

emitP :: Program IRPosition -> Writer [ASM.Instruction IRPosition] ()
emitP (Program p structs funcs strs) = do
    tell [ASM.Section p "rodata"]
    mapM_ emitS structs
    mapM_ emitData strs
    tell [ASM.Section p "text"]
    mapM_ emitF funcs

isRef (Reference _) = True
isRef _ = False

emitS :: Structure IRPosition -> Writer [ASM.Instruction IRPosition] ()
emitS (Struct pos (Label lp l) par s fs ms) = do
    let referenceFields = filter (\(_,t,_)->isRef t) $ IM.mapList (\_ v -> v) fs -- TODO: ? Reference
        refsLength = fromIntegral $ length referenceFields
    tell [
        ASM.Global lp l,
        ASM.SetLabel lp l,
        ASM.DQ pos (fromMaybe (ASM.Constant pos 0) (par >>= return . (\(Label p s) -> ASM.Label p s))),
        ASM.DD pos (ASM.Constant pos s),
        ASM.DQ pos (ASM.Label pos (l++"_methods")),
        ASM.DD pos (ASM.Constant pos refsLength),
        ASM.DQ pos (if refsLength > 0 then ASM.Label pos (l++"_refs") else ASM.Constant pos 0),
        ASM.SetLabel pos (l++"_methods")
          ]
    tell $ map (\m -> ASM.DQ pos (ASM.Label pos m)) $ IM.mapList (\v _ -> v) ms
    if refsLength > 0 then do
        tell [ASM.SetLabel pos (l++"_refs")]
        tell $ map (\(_,_,o)-> ASM.DD pos (ASM.Constant pos o)) referenceFields
    else return ()

data StringRep = Char Word8 | Str String

emitData :: DataDef IRPosition -> Writer [ASM.Instruction IRPosition] ()
emitData (DataString p s (Label p' l)) = tell $ (ASM.SetLabel p l) : divideString p s ++ [ASM.DB p' (ASM.Constant p' 0)]
    where
        divideString :: IRPosition -> String -> [ASM.Instruction IRPosition]
        divideString p s = map (toInst p) $ groupS s [] []
        groupS (s:ss) lacc gacc =
            if isAscii s && isAlphaNum s then
                groupS ss (s:lacc) gacc
            else
                let ng = Str (reverse lacc) : gacc
                    chars = reverse $ map Char $ unpack $ encodeUtf8 $ T.pack [s]
                in groupS ss [] (chars ++ ng)
        groupS [] [] gacc = filter empt $ reverse gacc
        groupS [] lacc gacc = groupS [] [] (Str (reverse lacc) : gacc)

        toInst p (Str s) = ASM.DB p (ASM.Label p (show s))
        toInst p (Char c) = ASM.DB p (ASM.Constant p (fromIntegral c))
        empt (Str "") = False
        empt _ = True 

emitF :: Function IRPosition -> Writer [ASM.Instruction IRPosition] ()
emitF (Fun p (Label p' l) _ args body) = do
    tell [ASM.Global p l, ASM.SetLabel p' l]
    emitB p args body

emitB :: IRPosition -> [(Type IRPosition, Name IRPosition)] -> [Stmt IRPosition] -> Writer [ASM.Instruction IRPosition] ()
emitB p args body = do
    let liveness = analize body
        regMap = mapArgs args
        regState = allocateRegisters liveness args regMap
        zippedBody = zip [1..] body
    emitI p zippedBody regState

mapArgs :: [(Type IRPosition, Name IRPosition)] -> [(Name IRPosition, [ASM.Value IRPosition])]
mapArgs as = map (\((_,n),v)->(n,[v])) zas
  where
    zas = argZip as regs 32
    argZip (a@(t,(Name p _)):as) (r:rs) i = (a,ASM.Register p $ ASM.regSize t r) : argZip as rs i
    argZip (a@(t,(Name p _)):as) [] i = (a, ASM.Memory p (ASM.RBP p) Nothing (Just (i+8)) (Just t)) : argZip as [] (i+8)
    argZip [] _ _ = []
    regs = [ASM.RDI noPosIR, ASM.RSI noPosIR, ASM.RDX noPosIR, ASM.RCX noPosIR, ASM.R8 noPosIR, ASM.R9 noPosIR]

st (IntT _) = 0x04
st (ByteT _) = 0x01
st (Reference _) = 0x08

emitI :: IRPosition -> [(Integer, Stmt IRPosition)] -> RegState -> Writer [ASM.Instruction IRPosition] ()
emitI pos stmts (regInts, stackSize, umap) = do
    entry pos stackSize
    loadArgs pos vmap
    body stmts
  where
    entry :: IRPosition -> StackSize -> Writer [ASM.Instruction IRPosition] ()
    entry pos s = do
        tell [ASM.PUSH pos (ASM.Register pos $ ASM.RBP pos), ASM.PUSH pos (ASM.Register pos $ ASM.RBX pos)]
        if r12 then tell [ASM.PUSH pos (ASM.Register pos $ ASM.R12 pos)]
        else return ()
        if r13 then tell [ASM.PUSH pos (ASM.Register pos $ ASM.R13 pos)]
        else return ()
        tell [ASM.MOV pos (ASM.Register pos $ ASM.RBP pos) (ASM.Register pos $ ASM.RSP pos), ASM.SUB pos (ASM.Register pos $ ASM.RSP pos) (ASM.Constant pos (padding + if s > 0 then ceil16 s else 0))]

    ceil16 x = case x `mod` 16 of
                0 -> x
                _ -> x + (16 - (x `mod` 16))
    body ss = mapM_ emitStmt ss

    padding = if (r12 && r13) || (not r12 && not r13) then 8 else 0

    r12 = any arrayOrObject $ map snd stmts
    r13 = any arrayOrObjectAssignment $ map snd stmts

    vmap = map fixArgPos umap
      where
        diff = (val r12) + (val r13)
        val b = if b then 0 else 8
        fixArgPos (n, vs) = (n, map fixMem vs)
        fixMem (ASM.Memory p r f (Just o) t) | o > 0 = (ASM.Memory p r f (Just (o-diff)) t)
        fixMem m = m

    loadArgs :: IRPosition -> ValMap -> Writer [ASM.Instruction IRPosition] ()
    loadArgs p vmap = do
        -- TODO: This seems to be very broken D:
        let argsToLoad = filter (\(n,vals) -> length vals == 2) vmap
        tell $ map (\(_,vals) -> ASM.MOV p (reg vals) (mem vals)) argsToLoad
        where
            reg [r@(ASM.Register _ _),_] = r
            reg [_,r@(ASM.Register _ _)] = r
            mem [m@(ASM.Memory _ _ _ _ _),_] = m
            mem [_,m@(ASM.Memory _ _ _ _ _)] = m

    exit :: IRPosition -> Writer [ASM.Instruction IRPosition] ()
    exit p = do
        tell [ASM.MOV p (ASM.Register p $ ASM.RSP p) (ASM.Register p $ ASM.RBP p)]
        if r13 then tell [ASM.POP p (ASM.Register p $ ASM.R13 p)]
        else return ()
        if r12 then tell [ASM.POP p (ASM.Register p $ ASM.R12 p)]
        else return ()
        tell [ASM.POP p (ASM.Register p $ ASM.RBX p),
                ASM.POP p (ASM.Register p $ ASM.RBP p),
                ASM.RET p
                ]

    moverr dest src = 
        let srcSize = ASM.regSizeR src
        in ASM.MOV noPosIR (ASM.Register noPosIR (ASM.regSize srcSize (ASM.topReg dest))) (ASM.Register noPosIR src)

    setupCallArgs args fr = do
        let sourceArgs = map (\a -> valueConv a ) args
            (regArgs,stackArgs) = splitAt 6 sourceArgs
            destinationRegs = [ASM.RDI noPosIR, ASM.RSI noPosIR, ASM.RDX noPosIR, ASM.RCX noPosIR, ASM.R8 noPosIR, ASM.R9 noPosIR]
            fromToRegArgs = zip regArgs (map (ASM.Register noPosIR) destinationRegs)
        moveAround fromToRegArgs (reverse stackArgs)
        where
            moveAround :: [(ASM.Value IRPosition,ASM.Value IRPosition)] -> [ASM.Value IRPosition] -> Writer [ASM.Instruction IRPosition] ()
            moveAround ((ASM.Register p rfrom, ASM.Register p' rto):xs) stack =
                if ASM.topReg rfrom == rto then moveAround xs stack
                else do
                    if elem rto fr then do
                        tell [moverr rto rfrom]
                        moveAround xs stack
                    else do
                        tell [  moverr (ASM.RBX p) (ASM.topReg rto),
                                moverr rto rfrom,
                                moverr (ASM.topReg rfrom) (ASM.RBX p') ]
                        moveAround (replace (ASM.Register p rto) (ASM.Register p' rfrom) xs) (replace2 (ASM.Register p rto) (ASM.Register p' rfrom) stack)
            moveAround ((v, reg@(ASM.Register p rto)):xs) stack =
                if elem rto fr then do
                    tell [ASM.MOV p (ASM.Register p rto) v]
                    moveAround xs stack
                else do
                    moveAround xs stack
                    tell [ASM.MOV p (ASM.Register p rto) v]
            moveAround [] stack = do
                tell [moverr (ASM.RBX noPosIR) (ASM.RSP noPosIR)] -- quick pop arguments
                mapM_ pushArg stack
            pushArg v@(ASM.Memory p _ _ _ t) =
                tell [ASM.MOV p (ASM.Register p (ASM.regSize (fromJust t) (ASM.R13 p))) v,ASM.PUSH p (ASM.Register p $ ASM.R13 p)]
            pushArg v@(ASM.Register p r) = tell [ASM.PUSH p (ASM.Register p (ASM.topReg r))]
            pushArg v = tell [ASM.PUSH noPosIR v]
            replace what with = map (\(a,b) -> if ASM.valueEq (ASM.topRegV a) (ASM.topRegV what) then (ASM.regSizeV (ASM.regSizeRV a) with,b) else (a,b))
            replace2 what with = map (\a -> if ASM.valueEq (ASM.topRegV a) (ASM.topRegV what) then ASM.regSizeV (ASM.regSizeRV a) with else a)
    call f = tell [ ASM.CALL noPosIR f, moverr (ASM.RSP noPosIR) (ASM.RBX noPosIR) ]
    valueConv (Var _ a) = getVal vmap a
    valueConv (Const p  (IntC _ i)) = ASM.Constant p i
    valueConv (Const p (ByteC _ i)) = ASM.Constant p i
    valueConv (Const p (Null _)) = ASM.Constant p 0
    valueConv (Const p (StringC _ (Label _ s))) = ASM.Label p s
    getVal umap n = fromJust $ getmVal umap n
    getmVal umap n =
        case lookup n umap of
            Nothing -> Nothing
            Just mapping -> case filter ASM.isReg mapping of
                                (h:_) -> Just h
                                [] -> Just $ head mapping    

    getReg umap n = case getmVal umap n of
                        Just r@(ASM.Register _ _) -> Just r
                        _ -> Nothing


    emitExpr :: (Maybe (Type IRPosition)) -> (Expr IRPosition) -> (ASM.Value IRPosition) -> Integer -> Writer [ASM.Instruction IRPosition] ()
    emitExpr t (Val p v) target i = do
        case v of
            Var p' n -> 
                case getVal vmap n of
                    ASM.Register _ r ->
                        case target of
                            ASM.Register _ q ->
                                if ASM.topReg r == ASM.topReg q then return ()
                                else tell [moverr q r]
                            _ -> tell [ASM.MOV p target (ASM.Register p r)]
                    m@(ASM.Memory _ _ _ _ (Just t)) -> 
                        case target of
                            ASM.Register _ q ->
                                tell [ASM.MOV p target m]
                            mm -> 
                                tell [ASM.MOV p (ASM.Register p (ASM.regSize t $ ASM.RBX p)) m,
                                      ASM.MOV p mm (ASM.Register p (ASM.regSize t $ ASM.RBX p))]
            Const p' c ->
                case target of
                    ASM.Register _ _ -> 
                        case c of
                            IntC p'' i -> tell [ASM.MOV p (ASM.regSizeV (IntT p'') target) (ASM.Constant p' i)]
                            ByteC p'' i -> tell [ASM.MOV p (ASM.regSizeV (ByteT p'') target) (ASM.Constant p' i)]
                            Null p'' ->tell [ASM.XOR p target target]
                    _ -> case c of
                            IntC _ i -> tell [ASM.MOV p (ASM.Register p $ ASM.EBX p) (ASM.Constant p i),
                                        ASM.MOV p target (ASM.Register p $ ASM.EBX p)]
                            ByteC _ i -> tell [ASM.MOV p (ASM.Register p $ ASM.BL p) (ASM.Constant p i),
                                        ASM.MOV p target (ASM.Register p $ ASM.BL p)]
                            Null _ -> tell [ASM.XOR p (ASM.Register p $ ASM.RBX p) (ASM.Register p $ ASM.RBX p),
                                        ASM.MOV p target (ASM.Register p $ ASM.RBX p)]
    emitExpr t (Call p (Label _ l)vs) target i =
        emitCall t (ASM.Label p l) vs target i
    emitExpr t (Cast p l v) target i =
        emitCall t (ASM.Label p "__cast") [v, Const p (StringC p l)] target i
    emitExpr t (MCall p n idx vs) target i = do
        emitExpr Nothing (Val p (Var p n)) (ASM.Register p $ ASM.RBX p) i
        checkIfNull (ASM.Register p $ ASM.RBX p)
        tell [
            ASM.MOV p (ASM.Register p $ ASM.R12 p) (ASM.Memory p (ASM.RBX p) Nothing Nothing Nothing),
            --get pointer to type
            ASM.MOV p (ASM.Register p $ ASM.R12 p) (ASM.Memory p (ASM.R12 p) Nothing (Just 12) Nothing),
            --get method array pointer
            ASM.MOV p (ASM.Register p $ ASM.R12 p) (ASM.Memory p (ASM.R12 p) Nothing (Just (idx*0x08)) Nothing)
            --get method pointer
              ]
        emitCall t (ASM.Register p $ ASM.R12 p) vs target i
    emitExpr t (NewObj p l) target i = do
        emitCall t (ASM.Label p "__new") [Const p (StringC p l)] target i
    emitExpr tp (NewArray p t v) target i = do
        case t of
            (IntT _) -> emitCall tp (ASM.Label p "__newIntArray") [v] target i
            (ByteT _) -> emitCall tp (ASM.Label p "__newByteArray") [v] target i
            (Reference _) -> emitCall tp (ASM.Label p "__newRefArray") [v] target i
    emitExpr t (ArrAccess p n v) target i = do
        emitCall t (ASM.Label p "__getelementptr") [Var p n, v] (ASM.Register p $ ASM.R12 p) i
        case target of
            ASM.Register _ r -> tell [ASM.MOV p target (ASM.Memory p (ASM.R12 p) Nothing Nothing Nothing)]
            _ -> let rbx = ASM.regSize (fromJust t) $ ASM.RBX p in
                 tell [ASM.MOV p (ASM.Register p rbx) (ASM.Memory p (ASM.R12 p) Nothing Nothing Nothing),
                       ASM.MOV p target (ASM.Register p rbx)]
    emitExpr t (MemberAccess p n off) target i = do
        r <- regOrEmit n (ASM.R12 p) i
        case r of
            (ASM.Register _ reg) -> do
                checkIfNull r
                case target of
                    ASM.Register p' _ ->
                        tell [
                            ASM.MOV p (ASM.Register p $ ASM.R12 p) (ASM.Memory p reg Nothing (Just 0x08) Nothing),
                            --get pointer to data
                            ASM.MOV p target (ASM.Memory p (ASM.R12 p) Nothing (Just off) Nothing)
                            ]
                    _ -> do 
                        let rbx = ASM.Register p $ ASM.regSize (fromJust t) $ ASM.RBX p
                        tell [
                            ASM.MOV p (ASM.Register p $ ASM.R12 p) (ASM.Memory p reg Nothing (Just 0x08) Nothing),
                            --get pointer to data
                            ASM.MOV p rbx (ASM.Memory p (ASM.R12 p) Nothing (Just off) Nothing),
                            ASM.MOV p target rbx
                            ]
            _ -> return () -- TODO: Handle failure here!
    emitExpr t (IntToByte p v) target i =
        emitExpr t (Val p v) target i
    emitExpr t (ByteToInt p v) target i = do
        case target of
            ASM.Register _ _ -> do
                tell [ASM.XOR p target target]
                emitExpr t (Val p v) target i
            _ -> do
                tell [ASM.XOR p (ASM.Register p $ ASM.EBX p) (ASM.Register p $ ASM.EBX p)]
                emitExpr t (Val p v) (ASM.Register p $ ASM.EBX p) i
                tell [ASM.MOV p target (ASM.Register p $ ASM.EBX p)]
    emitExpr t (Not p v) target' i =
        let target = ASM.regSizeV (ByteT p) target' in
        case v of
            Var _ n -> do
                let src = getVal vmap n
                r <- case src of
                        ASM.Register _ r -> return r
                        _ -> do
                            tell [ASM.MOV p (ASM.Register p $ ASM.BL p) src]
                            return $ ASM.BL p
                case target of
                    ASM.Register _ q -> do
                        if r /= q then
                            tell [moverr q r]
                        else return ()
                        tell [
                            ASM.TEST p (ASM.Register p q) (ASM.Register p q),
                            ASM.SETZ p (ASM.Register p q)]
                    _ -> tell [
                            ASM.TEST p (ASM.Register p r) (ASM.Register p r),
                            ASM.SETZ p target
                               ]
            Const _ (ByteC _ x) ->
                case x of
                    0 -> tell [ASM.MOV p target (ASM.Constant p 1)]
                    1 -> tell [ASM.MOV p target (ASM.Constant p 0)]
    emitExpr t (BinOp p op v1 v2) target i = do
        let vl = valueConv v1
            vr = valueConv v2
            size = fromMaybe (opSize op) t
        case op of
            Div _ -> do
                done <- divide vl vr i
                tell [moverr (ASM.EBX p) (ASM.EAX p)]
                done
                tell [ASM.MOV p target (ASM.Register p $ ASM.EBX p)]
            Mod _ -> do
                done <- divide vl vr i
                tell [moverr (ASM.EBX p) (ASM.EDX p)]
                done
                tell [ASM.MOV p target (ASM.Register p $ ASM.EBX p)]
            _ ->
                let x = (ASM.Register p (ASM.regSize size $ ASM.RBX p)) in
                tell [
                    ASM.MOV p x vl,
                    (opcode op) x vr,
                    ASM.MOV p target x
                      ]
    emitExpr t (NewString p l) target i = 
        emitCall t (ASM.Label p "__createString") [Const p (StringC p l)] target i


    emitStmt :: (Integer, Stmt IRPosition) -> Writer [ASM.Instruction IRPosition] ()
    emitStmt (i, VarDecl p t n e) = do
        let rbx = ASM.Register p (ASM.regSize t $ ASM.RBX p)
        let tgt = case lookup n vmap of
                    Nothing -> rbx
                    _ -> case getVal vmap n of
                            r@(ASM.Register _ _) ->
                                if elem n $ alive (i+1) then r else rbx
                            m -> m
        emitExpr (Just t) e tgt i
    emitStmt (i, Assign p t tg e) =
        case tg of
            Variable p' n -> do
                let rbx = ASM.Register p (ASM.regSize t $ ASM.RBX p)
                let tgt = case lookup n vmap of
                            Nothing -> rbx
                            _ -> case getVal vmap n of
                                    r@(ASM.Register _ _) ->
                                        if elem n $ alive (i+1) then r else rbx
                                    m -> m
                emitExpr (Just t) e tgt i
            Array p' a idx -> do
                emitExpr (Just t) e (ASM.Register p (ASM.regSize t $ ASM.R12 p)) i
                emitCall (Just t) (ASM.Label p "__getelementptr") [Var p a, idx] (ASM.Register p $ ASM.R13 p) i
                tell [ASM.MOV p (ASM.Memory p (ASM.R13 p) Nothing Nothing Nothing) (ASM.Register p (ASM.regSize t $ ASM.R12 p))]
            Member p' m off -> do
                emitExpr (Just t) e (ASM.Register p (ASM.regSize t $ ASM.R12 p)) i
                r <- regOrEmit m (ASM.R13 p) i
                case r of
                    (ASM.Register _ reg) -> do
                        checkIfNull r
                        tell [ASM.MOV p (ASM.Register p $ ASM.R13 p) (ASM.Memory p reg Nothing (Just 0x08) Nothing),
                            ASM.MOV p (ASM.Memory p (ASM.R13 p) Nothing (Just off) Nothing) (ASM.Register p (ASM.regSize t $ ASM.R12 p))]
                    _ -> return () -- TODO: Implement error handling here!
    emitStmt (i, IncrCounter p n) = do
        emitExpr Nothing (Val p (Var p n)) (ASM.Register p $ ASM.R13 p) i
        incr
    emitStmt (i, DecrCounter p n) = do
        emitCall Nothing (ASM.Label p "__decRef") [Var p n] (ASM.Register p $ ASM.RBX p) i
    emitStmt (i, ReturnVal p t e) = do
        emitExpr (Just t) e (ASM.Register p (ASM.regSize t $ ASM.RAX p)) i
        exit p
    emitStmt (i, Return p) = do
        exit p
    emitStmt (i, SetLabel p (Label _ l)) = do
        tell [ASM.SetLabel p l]
    emitStmt (i, Jump p (Label _ l)) = do
        tell [ASM.JMP p (ASM.Label p l)]
    emitStmt (i, JumpCmp pos cmp (Label _ lbl) vl vr) = do
        let vlc = valueConv vl
        let vrc = valueConv vr
        (l,r,c) <- case (vlc, vrc) of
            (ASM.Constant p i, ASM.Register _ _) ->
                return (vrc, vlc, reverseSide cmp)
            (ASM.Constant p i, ASM.Memory _ _ _ _ _) ->
                return (vrc, vlc, reverseSide cmp)
            (ASM.Memory _ _ _ _ (Just t), ASM.Memory _ _ _ _ _) -> do
                tell [ASM.MOV pos (ASM.Register pos $ ASM.regSize t $ ASM.RBX pos) vlc]
                return (ASM.Register pos $ ASM.regSize t (ASM.RBX pos), vrc, cmp)
            (_, _) -> return (vlc, vrc, cmp)
        case (r, cmp) of
            (ASM.Constant _ 0, Eq _) -> tell [ASM.TEST pos l l, makeJump cmp lbl]
            (ASM.Constant _ 0, Ne _) -> tell [ASM.TEST pos l l, makeJump cmp lbl]
            _ -> tell [ASM.CMP pos l r, makeJump cmp lbl]

        where
            reverseSide (Ge p) = Le p
            reverseSide (Le p) = Ge p
            reverseSide (Gt p) = Lt p
            reverseSide (Lt p) = Gt p
            reverseSide op = op


    prepareCall free = do
        let callerSaved = [ASM.R11 noPosIR, ASM.R10 noPosIR, ASM.R9 noPosIR, ASM.R8 noPosIR, ASM.RDX noPosIR, ASM.RCX noPosIR, ASM.RAX noPosIR, ASM.RSI noPosIR, ASM.RDI noPosIR]
        prepare free callerSaved True
    prepareDiv free = do
        let callerSaved = [ASM.RAX noPosIR, ASM.RDX noPosIR]
        prepare free callerSaved False
    prepare free saved align = do
        let used = saved \\ free
            usedAsVal = map (ASM.Register noPosIR) used
            (alignstack, dealignstack) = if not align || (length used) `mod` 2 == 0 then ([],[]) else ([ASM.SUB noPosIR (ASM.Register noPosIR $ ASM.RSP noPosIR) (ASM.Constant noPosIR 8)], [ASM.ADD noPosIR (ASM.Register noPosIR $ ASM.RSP noPosIR) (ASM.Constant noPosIR 8)])
        tell (alignstack ++ map (ASM.PUSH noPosIR) usedAsVal)
        return (tell (map (ASM.POP noPosIR) (reverse usedAsVal) ++ dealignstack))

    divide :: (ASM.Value IRPosition) -> (ASM.Value IRPosition) -> Integer -> Writer [ASM.Instruction IRPosition] (Writer [ASM.Instruction IRPosition] ())
    divide vl vr i = do
        let fr = freeAt i
        done <- prepareDiv fr
        case vr of 
            (ASM.Register p (ASM.EDX _)) -> tell [ASM.MOV p (ASM.Register p $ ASM.EBX p) (ASM.Register p $ ASM.EDX p)]
            _ -> return ()
        tell [ASM.MOV noPosIR (ASM.Register noPosIR $ ASM.EAX noPosIR) vl, ASM.CDQ noPosIR]
        case vr of
            ASM.Constant p _ -> 
                tell [ASM.MOV p (ASM.Register p $ ASM.EBX p) vr,
                      ASM.IDIV p (ASM.Register p $ ASM.EBX p)]
            ASM.Register p (ASM.EDX _) -> tell [ASM.IDIV p (ASM.Register p $ ASM.EBX p)]
            _ -> tell [ASM.IDIV noPosIR vr]
        return done

    opcode (Add p) = ASM.ADD p
    opcode (Sub p) = ASM.SUB p
    opcode (Mul p) = ASM.IMUL p
    opcode (And p) = ASM.AND p
    opcode (Or p) = ASM.OR p

    opSize (And p) = ByteT p
    opSize (Or p) = ByteT p
    opSize _ = IntT noPosIR

    checkIfNull :: (ASM.Value IRPosition) -> Writer [ASM.Instruction IRPosition] ()
    checkIfNull r@(ASM.Register p _) = 
        tell [
            ASM.TEST p r r,
            ASM.JNZ p (ASM.Local p (2+5)),
            ASM.CALL p (ASM.Label p "__errorNull") 
        ]

    incr :: Writer [ASM.Instruction IRPosition] ()
    incr = 
        let p = noPosIR in
        let r = (ASM.Register p $ ASM.R13 p) in
        tell [
            ASM.TEST p r r,
            ASM.JZ p (ASM.Local p (2+4+2+4)), --TODO change value after emitting
            ASM.MOV p (ASM.Register p $ ASM.EBX p) (ASM.Memory p (ASM.R13 p) Nothing (Just 16) Nothing),
            ASM.INC p (ASM.Register p $ ASM.EBX p),
            ASM.MOV p (ASM.Memory p (ASM.R13 p) Nothing (Just 16) Nothing) (ASM.Register p $ ASM.EBX p)
        ]

    emitCall t fun vs target i = do
        let fr = freeAt (i + 1)
        let p = noPosIR
        doneCall <- prepareCall fr
        setupCallArgs vs (freeAt i)
        call fun
        tell [moverr (ASM.RBX p) (ASM.RAX p)]
        doneCall
        case target of
            ASM.Register p' r -> tell [moverr r (ASM.RBX p')]
            _ -> tell [ASM.MOV p target (ASM.Register p $ ASM.regSize (fromJust t) (ASM.RBX p))]

    makeJump (Eq p) l = ASM.JE p (ASM.Label p l)
    makeJump (Ne p) l = ASM.JNE p (ASM.Label p l)
    makeJump (Le p) l = ASM.JLE p (ASM.Label p l)
    makeJump (Lt p) l = ASM.JL p (ASM.Label p l)
    makeJump (Ge p) l = ASM.JGE p (ASM.Label p l)
    makeJump (Gt p) l = ASM.JG p (ASM.Label p l)
        
    freeAt i = map fst $ filter (\(r,is) -> any (\(b,f,u) -> b == Free && f <= i && i <= u) is) regInts

    alive i = map (\[(Busy b, _,_)] -> b) $ filter (\l -> length l == 1) $ map (\(r,is) -> let l = filter (\(b,f,u) -> b /= Free && f <= i && i <= u) is in l) regInts

    regOrEmit :: Name IRPosition -> ASM.Reg IRPosition -> Integer -> Writer [ASM.Instruction IRPosition] (ASM.Value IRPosition)
    regOrEmit n r i = do
        let p = noPosIR
        let m = getReg vmap n
        case m of
            Just x -> return x
            Nothing -> do
                emitExpr Nothing (Val p (Var p n)) (ASM.Register p r) i
                return (ASM.Register p r)

    arrayOrObject s = arrayOrObjectAssign s || arrayOrObjectExpression s
    arrayOrObjectAssignment s = arrayOrObjectAssign s || isIncr s
    arrayOrObjectAssign (Assign _ _ (Array _ _ _) _) = True
    arrayOrObjectAssign (Assign _ _ (Member _ _ _) _) = True
    arrayOrObjectAssign (VarDecl _ _ _ e) = longCall e
    arrayOrObjectAssign (Assign _ _ _ e) = longCall e
    arrayOrObjectAssign (ReturnVal _ _ e) = longCall e
    arrayOrObjectAssign _ = False
    isIncr (IncrCounter _ _) = True
    isIncr _ = False
    longCall (Call _ _ vs) = length vs > 6
    longCall (MCall _ _ _ vs) = length vs > 6
    longCall _ = False
    arrayOrObjectExpression (VarDecl _ _ _ e) = aOOE e
    arrayOrObjectExpression (Assign _ _ _ e) = aOOE e
    arrayOrObjectExpression (ReturnVal _ _ e) = aOOE e
    arrayOrObjectExpression _ = False
    aOOE (MCall _ _ _ _) = True
    aOOE (ArrAccess _ _ _) = True
    aOOE (MemberAccess _ _ _) = True
    aOOE _ = False
