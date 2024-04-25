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
applyRulesSingle prevCode (ASM.MOV pos size (ASM.LocConst 0) target anno : code) | ASM.isReg target = ASM.XOR pos size target target anno : code
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


