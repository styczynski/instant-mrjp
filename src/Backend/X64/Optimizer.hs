module Backend.X64.Optimizer(optimizeASM) where

import qualified Backend.X64.Parser.Constructor as ASM

import Reporting.Logs
import IR.Utils

type ASMOptRule a anno = [ASM.Instr a anno] -> [ASM.Instr a anno] -> [ASM.Instr a anno]

optimizeASM :: [ASM.Instr a anno] -> LattePipeline [ASM.Instr a anno]
optimizeASM instrs = do 
        optimizedInstrs <- fixpointByM (\instrs -> map (ASM.mapInstrData (const ()) (const ())) instrs) (applyRulesPass applyRulesSingle) instrs
        return optimizedInstrs
    where
        applyRulesPass :: (ASMOptRule a anno) -> [ASM.Instr a anno] -> LattePipeline [ASM.Instr a anno]
        applyRulesPass rule instrs = do
            printLogInfoStr $ "optimizeASM.applyRulePass: Perform another rules pass..."
            applyRulesPass' rule [] instrs
        applyRulesPass' :: (ASMOptRule a anno) -> [ASM.Instr a anno] -> [ASM.Instr a anno] -> LattePipeline [ASM.Instr a anno]
        applyRulesPass' rule prevInstrs [] = return prevInstrs
        applyRulesPass' rule prevInstrs instrs = do 
            appliedRule <- return $ rule prevInstrs instrs
            case appliedRule of 
                [] -> return prevInstrs
                (newInstr:otherNewInstrs) -> applyRulesPass' rule (prevInstrs ++ [newInstr]) otherNewInstrs

applyRulesSingle :: ASMOptRule a anno
-- mov $0, a => xor a, a
applyRulesSingle prevCode (ASM.MOV pos size (ASM.LocConst 0) target anno : code) = ASM.XOR pos size target target anno : code
-- add $1, a => inc a
applyRulesSingle prevCode (ASM.ADD pos size (ASM.LocConst 1) target anno : code) = ASM.INC pos size target anno : code
-- sub $1, a => dec a
applyRulesSingle prevCode (ASM.SUB pos size (ASM.LocConst 1) target anno : code) = ASM.DEC pos size target anno : code
-- add $0, a => X
applyRulesSingle prevCode (ASM.ADD pos size (ASM.LocConst 0) target anno : code) = code
-- sub $0, a => X
applyRulesSingle prevCode (ASM.SUB pos size (ASM.LocConst 0) target anno : code) = code
-- cmp $0, a => test a
applyRulesSingle prevCode (ASM.CMP pos size (ASM.LocConst 0) target anno : code) = ASM.TEST pos size target target anno : code
-- mov a, a => X
applyRulesSingle prevCode (ASM.MOV pos size target1 target2 anno : code) | target1 == target2 = code
-- xchg a, a => X
applyRulesSingle prevCode (ASM.XCHG pos size target1 target2 anno : code) | target1 == target2 = code
-- { mov b, a | [add|sub|mul] c, a | mov a, b } => { add c, b | mov b, a }
applyRulesSingle prevCode (ASM.MOV pos1 size1 b1 a1 anno1 : ASM.ADD pos2 size2 c1 a2 anno2 : ASM.MOV pos3 size3 a3 b2 anno3 : code) | size1 == size2 && size2 == size3 && a1 == a2 && a2 == a3 && b1 == b2 =
    ASM.ADD pos2 size1 c1 b1 (ASM.combineAnn anno2 anno1) : ASM.MOV pos1 size1 b1 a1 (ASM.combineAnn anno1 anno2) : code
applyRulesSingle prevCode (ASM.MOV pos1 size1 b1 a1 anno1 : ASM.SUB pos2 size2 c1 a2 anno2 : ASM.MOV pos3 size3 a3 b2 anno3 : code) | size1 == size2 && size2 == size3 && a1 == a2 && a2 == a3 && b1 == b2 =
    ASM.SUB pos2 size1 c1 b1 (ASM.combineAnn anno2 anno1) : ASM.MOV pos1 size1 b1 a1 (ASM.combineAnn anno1 anno2) : code
applyRulesSingle prevCode (ASM.MOV pos1 size1 b1 a1 anno1 : ASM.IMUL pos2 size2 c1 a2 anno2 : ASM.MOV pos3 size3 a3 b2 anno3 : code) | size1 == size2 && size2 == size3 && a1 == a2 && a2 == a3 && b1 == b2 =
    ASM.IMUL pos2 size1 c1 b1 (ASM.combineAnn anno2 anno1) : ASM.MOV pos1 size1 b1 a1 (ASM.combineAnn anno1 anno2) : code
-- { set[ord] a | test a, a | j(z|e) A } => { j[!ord] A }
-- { set[ord] a | test a, a | j(nz|ne) A } => { j[ord] A }
applyRulesSingle prevCode (ASM.SET pos1 ord1 loc1 anno1 : ASM.TEST pos2 size2 loc2 loc2' anno2 : ASM.J pos3 ord2 lab anno3 : code) | (ord2 == ASM.OrdZ || ord2 == ASM.OrdE) && ASM.Size8 == size2 && loc2 == loc2' && loc1 == loc2 = ASM.J pos1 (ASM.negValOrd ord1) lab anno3 : code
applyRulesSingle prevCode (ASM.SET pos1 ord1 loc1 anno1 : ASM.TEST pos2 size2 loc2 loc2' anno2 : ASM.J pos3 ord2 lab anno3 : code) | (ord2 == ASM.OrdNZ || ord2 == ASM.OrdNE) && ASM.Size8 == size2 && loc2 == loc2' && loc1 == loc2 = ASM.J pos1 ord1 lab anno3 : code
-- { j[ord] A | jmp B | label A: } => { j[!ord] B | label A: }
applyRulesSingle prevCode (ASM.J pos1 ord1 lab1 anno1 : ASM.JMP pos2 lab2 anno2 : ASM.Label pos3 lab3 anno3 : code) | lab1 == lab3 = ASM.J pos1 (ASM.negValOrd ord1) lab2 (ASM.combineAnn anno1 anno2) : ASM.Label pos3 lab3 anno3 : code
-- { movx a, c | movx b, a | movx c, b } => xchgx a, b if a, b, c are registers (consider doing that for mem?)
applyRulesSingle prevCode (ASM.MOV pos1 size1 fromA toC anno1 : ASM.MOV pos2 size2 fromB toA anno2 : ASM.MOV pos3 size3 fromC toB anno3 : code) | ASM.isReg fromA && ASM.isReg fromB && ASM.isReg fromC && size1 == size2 && size2 == size3 && fromA == toA && fromB == toB = 
    ASM.XCHG pos1 size1 fromA toB (ASM.combineAnn anno1 anno3) : code
-- {jmp A | label A:} => X if there is no reference to A in the code elsewhere
applyRulesSingle prevCode orig@(ASM.JMP pos1 jmpLabel anno1 : ASM.Label pos2 declLabel anno2 : code) | jmpLabel == declLabel && (not $ elem declLabel $ concatMap ASM.getInstrUsedLabels (prevCode ++ code)) = code
-- No optimization rule was found
applyRulesSingle prevCode code = code




--applyRulesSingle (ASM.MOV p size bx x anno : ASM.MOV p' size' y bx' anno' : is) | size == size' && bx == bx' && isTemp bx && (ASM.isReg x || ASM.isReg y) = applyRulesSingle (ASM.MOV size p y x (ASM.combineAnn anno anno'): is)

-- applyRulesSingle (ASM.MOV p bx x : ASM.CMP p' bx' y : is) | bx == bx' && isTemp bx = applyRulesSingle (ASM.CMP p' x y : is)

-- applyRulesSingle (ASM.MOV p x bx : ASM.TEST p' bx' bx'' : is) | bx == bx' && bx == bx'' && isTemp bx = applyRulesSingle (ASM.MOV p x bx : ASM.TEST p' x x : is)

-- applyRulesSingle (ASM.MOV _ a b : is) | a == b = applyRulesSingle is
-- applyRulesSingle (ASM.MOV p x y : ASM.MOV p' x' z : is) | x == x' && notDependent x z = applyRulesSingle (ASM.MOV p x z : is)
-- applyRulesSingle (ASM.MOV p x y : ASM.MOV p' y' x' : is) | x == x' && y == y' = applyRulesSingle (ASM.MOV p x y : is)

-- applyRulesSingle (ASM.MOV _ bx x : ASM.SUB p bx' y : ASM.MOV _ x' bx'' : is) | bx == bx' && bx == bx'' && x == x' && isTemp bx && ASM.isReg x = applyRulesSingle (ASM.SUB p x y : is)
-- applyRulesSingle (ASM.MOV _ bx x : ASM.SUB p bx' y : ASM.MOV _ z bx'' : is) | bx == bx' && bx == bx'' && z /= y && isTemp bx && ASM.isReg z = applyRulesSingle (ASM.MOV p z x : ASM.SUB p z y : is)


-- applyRulesSingle (ASM.MOV _ bx x : ASM.ADD p bx' y : ASM.MOV _ x' bx'' : is) | bx == bx' && bx == bx'' && x == x' && isTemp bx && ASM.isReg x = applyRulesSingle (ASM.ADD p x y : is)
-- applyRulesSingle (ASM.MOV _ bx x : ASM.ADD p bx' y : ASM.MOV _ y' bx'' : is) | bx == bx' && bx == bx'' && y == y' && isTemp bx && ASM.isReg y = applyRulesSingle (ASM.ADD p y x : is)
-- applyRulesSingle (ASM.MOV _ bx x : ASM.ADD p bx' y : ASM.MOV _ z bx'' : is) | bx == bx' && bx == bx'' && z /= y && isTemp bx && ASM.isReg z = applyRulesSingle (ASM.MOV p z x : ASM.ADD p z y : is)

-- applyRulesSingle (ASM.MOV _ bx x : ASM.IMUL p bx' y : ASM.MOV _ x' bx'' : is) | bx == bx' && bx == bx'' && x == x' && isTemp bx && ASM.isReg x = applyRulesSingle (ASM.IMUL p x y : is)
-- applyRulesSingle (ASM.MOV _ bx x : ASM.IMUL p bx' y : ASM.MOV _ y' bx'' : is) | bx == bx' && bx == bx'' && y == y' && isTemp bx && ASM.isReg y = applyRulesSingle (ASM.IMUL p y x : is)
-- applyRulesSingle (ASM.MOV _ bx x : ASM.IMUL p bx' y : ASM.MOV _ z bx'' : is) | bx == bx' && bx == bx'' && z /= y && isTemp bx && ASM.isReg z = applyRulesSingle (ASM.MOV p z x : ASM.IMUL p z y : is)

-- applyRulesSingle (ASM.JMP _ (ASM.Label _ v) : ASM.SetLabel p el : ASM.SetLabel p' en : is) | v == en = applyRulesSingle (ASM.SetLabel p el : ASM.SetLabel p' en : is)


-- stackOptim xs = let xs' = opt xs in if xs == xs' then xs else stackOptim xs'
--     where
--         opt (ASM.MOV _ bx sp : ASM.CALL p l : ASM.MOV _ sp' bx' : is) | bx == bx' && isTemp bx && sp == sp' && isStack sp = opt (ASM.CALL p l : is)
--         opt (ASM.ADD _ rsp v : ASM.SUB _ rsp' v' : is) | rsp == rsp' && v == v' && isStack rsp = opt is
--         opt (ASM.ADD _ rsp v : i : ASM.SUB _ rsp' v' : is) | rsp == rsp' && v == v' && isStack rsp = opt (i:is)
--         opt (i:is) = i : opt is
--         opt [] = []

-- notDependent (ASM.Register _ r) (ASM.Memory _ r' _ _ _) | r == r' = False
-- notDependent (ASM.Register _ r) (ASM.Memory _ _ (Just (r', _)) _ _) | r == r' = False
-- notDependent (ASM.Memory _ r' _ _ _) (ASM.Register _ r) | r == r' = False
-- notDependent (ASM.Memory _ _ (Just (r',_)) _ _) (ASM.Register _ r) | r == r' = False
-- notDependent _ _ = True

-- isTemp (ASM.Register _ x) = case ASM.topReg x of
--     (ASM.RBX _) -> True
--     _ -> False
-- isTemp _ = False

-- isStack (ASM.Register _ (ASM.RSP _)) = True
-- isStack _ = False