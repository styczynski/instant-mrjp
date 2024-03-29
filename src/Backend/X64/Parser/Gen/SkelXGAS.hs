-- File generated by the BNF Converter (bnfc 2.9.4).

-- Templates for pattern matching on abstract syntax

{-# OPTIONS_GHC -fno-warn-unused-matches #-}

module Backend.X64.Parser.Gen.SkelXGAS where

import Prelude (($), Either(..), String, (++), Show, show)
import qualified Backend.X64.Parser.Gen.AbsXGAS

type Err = Either String
type Result = Err String

failure :: Show a => a -> Result
failure x = Left $ "Undefined case: " ++ show x

transCommentLike :: Backend.X64.Parser.Gen.AbsXGAS.CommentLike -> Result
transCommentLike x = case x of
  Backend.X64.Parser.Gen.AbsXGAS.CommentLike string -> failure x

transConstIntRef :: Backend.X64.Parser.Gen.AbsXGAS.ConstIntRef -> Result
transConstIntRef x = case x of
  Backend.X64.Parser.Gen.AbsXGAS.ConstIntRef string -> failure x

transLabel :: Backend.X64.Parser.Gen.AbsXGAS.Label -> Result
transLabel x = case x of
  Backend.X64.Parser.Gen.AbsXGAS.Label string -> failure x

transAsmProgram :: Show a => Backend.X64.Parser.Gen.AbsXGAS.AsmProgram' a -> Result
transAsmProgram x = case x of
  Backend.X64.Parser.Gen.AbsXGAS.AsmProgram _ directives sectiondata sectioncode -> failure x

transSectionData :: Show a => Backend.X64.Parser.Gen.AbsXGAS.SectionData' a -> Result
transSectionData x = case x of
  Backend.X64.Parser.Gen.AbsXGAS.SectionData _ asmdatadefs -> failure x

transSectionCode :: Show a => Backend.X64.Parser.Gen.AbsXGAS.SectionCode' a -> Result
transSectionCode x = case x of
  Backend.X64.Parser.Gen.AbsXGAS.SectionCode _ asminstrs -> failure x

transAsmDataDef :: Show a => Backend.X64.Parser.Gen.AbsXGAS.AsmDataDef' a -> Result
transAsmDataDef x = case x of
  Backend.X64.Parser.Gen.AbsXGAS.AsmDataGlobal _ label -> failure x
  Backend.X64.Parser.Gen.AbsXGAS.AsmDataDef _ label datas -> failure x

transCommentAnn :: Show a => Backend.X64.Parser.Gen.AbsXGAS.CommentAnn' a -> Result
transCommentAnn x = case x of
  Backend.X64.Parser.Gen.AbsXGAS.Comment _ commentlike -> failure x
  Backend.X64.Parser.Gen.AbsXGAS.NoComment _ -> failure x

transData :: Show a => Backend.X64.Parser.Gen.AbsXGAS.Data' a -> Result
transData x = case x of
  Backend.X64.Parser.Gen.AbsXGAS.DataString _ string -> failure x
  Backend.X64.Parser.Gen.AbsXGAS.Data64 _ dataconst -> failure x
  Backend.X64.Parser.Gen.AbsXGAS.Data32 _ dataconst -> failure x

transDataConst :: Show a => Backend.X64.Parser.Gen.AbsXGAS.DataConst' a -> Result
transDataConst x = case x of
  Backend.X64.Parser.Gen.AbsXGAS.ConstInt _ integer -> failure x
  Backend.X64.Parser.Gen.AbsXGAS.ConstLabel _ label -> failure x

transDirective :: Show a => Backend.X64.Parser.Gen.AbsXGAS.Directive' a -> Result
transDirective x = case x of
  Backend.X64.Parser.Gen.AbsXGAS.Extern _ label -> failure x

transAsmInstr :: Show a => Backend.X64.Parser.Gen.AbsXGAS.AsmInstr' a -> Result
transAsmInstr x = case x of
  Backend.X64.Parser.Gen.AbsXGAS.LabelDef _ label commentann -> failure x
  Backend.X64.Parser.Gen.AbsXGAS.ADD64 _ source target commentann -> failure x
  Backend.X64.Parser.Gen.AbsXGAS.AND64 _ source target commentann -> failure x
  Backend.X64.Parser.Gen.AbsXGAS.CMP64 _ source target commentann -> failure x
  Backend.X64.Parser.Gen.AbsXGAS.IMUL64 _ source target commentann -> failure x
  Backend.X64.Parser.Gen.AbsXGAS.LEA64 _ source target commentann -> failure x
  Backend.X64.Parser.Gen.AbsXGAS.MOV64 _ source target commentann -> failure x
  Backend.X64.Parser.Gen.AbsXGAS.SUB64 _ source target commentann -> failure x
  Backend.X64.Parser.Gen.AbsXGAS.TEST64 _ source target commentann -> failure x
  Backend.X64.Parser.Gen.AbsXGAS.XOR64 _ source target commentann -> failure x
  Backend.X64.Parser.Gen.AbsXGAS.XCHG64 _ source target commentann -> failure x
  Backend.X64.Parser.Gen.AbsXGAS.SAL64 _ source target commentann -> failure x
  Backend.X64.Parser.Gen.AbsXGAS.SAR64 _ source target commentann -> failure x
  Backend.X64.Parser.Gen.AbsXGAS.ADD32 _ source target commentann -> failure x
  Backend.X64.Parser.Gen.AbsXGAS.AND32 _ source target commentann -> failure x
  Backend.X64.Parser.Gen.AbsXGAS.CMP32 _ source target commentann -> failure x
  Backend.X64.Parser.Gen.AbsXGAS.IMUL32 _ source target commentann -> failure x
  Backend.X64.Parser.Gen.AbsXGAS.LEA32 _ source target commentann -> failure x
  Backend.X64.Parser.Gen.AbsXGAS.MOV32 _ source target commentann -> failure x
  Backend.X64.Parser.Gen.AbsXGAS.SUB32 _ source target commentann -> failure x
  Backend.X64.Parser.Gen.AbsXGAS.TEST32 _ source target commentann -> failure x
  Backend.X64.Parser.Gen.AbsXGAS.XOR32 _ source target commentann -> failure x
  Backend.X64.Parser.Gen.AbsXGAS.XCHG32 _ source target commentann -> failure x
  Backend.X64.Parser.Gen.AbsXGAS.SAL32 _ source target commentann -> failure x
  Backend.X64.Parser.Gen.AbsXGAS.SAR32 _ source target commentann -> failure x
  Backend.X64.Parser.Gen.AbsXGAS.ADD16 _ source target commentann -> failure x
  Backend.X64.Parser.Gen.AbsXGAS.AND16 _ source target commentann -> failure x
  Backend.X64.Parser.Gen.AbsXGAS.CMP16 _ source target commentann -> failure x
  Backend.X64.Parser.Gen.AbsXGAS.IMUL16 _ source target commentann -> failure x
  Backend.X64.Parser.Gen.AbsXGAS.LEA16 _ source target commentann -> failure x
  Backend.X64.Parser.Gen.AbsXGAS.MOV16 _ source target commentann -> failure x
  Backend.X64.Parser.Gen.AbsXGAS.SUB16 _ source target commentann -> failure x
  Backend.X64.Parser.Gen.AbsXGAS.TEST16 _ source target commentann -> failure x
  Backend.X64.Parser.Gen.AbsXGAS.XOR16 _ source target commentann -> failure x
  Backend.X64.Parser.Gen.AbsXGAS.XCHG16 _ source target commentann -> failure x
  Backend.X64.Parser.Gen.AbsXGAS.SAL16 _ source target commentann -> failure x
  Backend.X64.Parser.Gen.AbsXGAS.SAR16 _ source target commentann -> failure x
  Backend.X64.Parser.Gen.AbsXGAS.ADD8 _ source target commentann -> failure x
  Backend.X64.Parser.Gen.AbsXGAS.AND8 _ source target commentann -> failure x
  Backend.X64.Parser.Gen.AbsXGAS.CMP8 _ source target commentann -> failure x
  Backend.X64.Parser.Gen.AbsXGAS.IMUL8 _ source target commentann -> failure x
  Backend.X64.Parser.Gen.AbsXGAS.LEA8 _ source target commentann -> failure x
  Backend.X64.Parser.Gen.AbsXGAS.MOV8 _ source target commentann -> failure x
  Backend.X64.Parser.Gen.AbsXGAS.SUB8 _ source target commentann -> failure x
  Backend.X64.Parser.Gen.AbsXGAS.TEST8 _ source target commentann -> failure x
  Backend.X64.Parser.Gen.AbsXGAS.XOR8 _ source target commentann -> failure x
  Backend.X64.Parser.Gen.AbsXGAS.XCHG8 _ source target commentann -> failure x
  Backend.X64.Parser.Gen.AbsXGAS.SAL8 _ source target commentann -> failure x
  Backend.X64.Parser.Gen.AbsXGAS.SAR8 _ source target commentann -> failure x
  Backend.X64.Parser.Gen.AbsXGAS.NEG64 _ target commentann -> failure x
  Backend.X64.Parser.Gen.AbsXGAS.IDIV64 _ target commentann -> failure x
  Backend.X64.Parser.Gen.AbsXGAS.INC64 _ target commentann -> failure x
  Backend.X64.Parser.Gen.AbsXGAS.DEC64 _ target commentann -> failure x
  Backend.X64.Parser.Gen.AbsXGAS.NEG32 _ target commentann -> failure x
  Backend.X64.Parser.Gen.AbsXGAS.IDIV32 _ target commentann -> failure x
  Backend.X64.Parser.Gen.AbsXGAS.INC32 _ target commentann -> failure x
  Backend.X64.Parser.Gen.AbsXGAS.DEC32 _ target commentann -> failure x
  Backend.X64.Parser.Gen.AbsXGAS.NEG16 _ target commentann -> failure x
  Backend.X64.Parser.Gen.AbsXGAS.IDIV16 _ target commentann -> failure x
  Backend.X64.Parser.Gen.AbsXGAS.INC16 _ target commentann -> failure x
  Backend.X64.Parser.Gen.AbsXGAS.DEC16 _ target commentann -> failure x
  Backend.X64.Parser.Gen.AbsXGAS.NEG8 _ target commentann -> failure x
  Backend.X64.Parser.Gen.AbsXGAS.IDIV8 _ target commentann -> failure x
  Backend.X64.Parser.Gen.AbsXGAS.INC8 _ target commentann -> failure x
  Backend.X64.Parser.Gen.AbsXGAS.DEC8 _ target commentann -> failure x
  Backend.X64.Parser.Gen.AbsXGAS.PUSH64 _ source commentann -> failure x
  Backend.X64.Parser.Gen.AbsXGAS.PUSH32 _ source commentann -> failure x
  Backend.X64.Parser.Gen.AbsXGAS.PUSH16 _ source commentann -> failure x
  Backend.X64.Parser.Gen.AbsXGAS.PUSH8 _ source commentann -> failure x
  Backend.X64.Parser.Gen.AbsXGAS.CALL _ label commentann -> failure x
  Backend.X64.Parser.Gen.AbsXGAS.CALLINDIRECT _ integer reg commentann -> failure x
  Backend.X64.Parser.Gen.AbsXGAS.POP _ reg commentann -> failure x
  Backend.X64.Parser.Gen.AbsXGAS.LEAVE _ commentann -> failure x
  Backend.X64.Parser.Gen.AbsXGAS.RET _ commentann -> failure x
  Backend.X64.Parser.Gen.AbsXGAS.CDQ _ commentann -> failure x
  Backend.X64.Parser.Gen.AbsXGAS.SETE _ reg commentann -> failure x
  Backend.X64.Parser.Gen.AbsXGAS.SETG _ reg commentann -> failure x
  Backend.X64.Parser.Gen.AbsXGAS.SETGE _ reg commentann -> failure x
  Backend.X64.Parser.Gen.AbsXGAS.SETL _ reg commentann -> failure x
  Backend.X64.Parser.Gen.AbsXGAS.SETLE _ reg commentann -> failure x
  Backend.X64.Parser.Gen.AbsXGAS.SETNE _ reg commentann -> failure x
  Backend.X64.Parser.Gen.AbsXGAS.SETZ _ reg commentann -> failure x
  Backend.X64.Parser.Gen.AbsXGAS.SETNZ _ reg commentann -> failure x
  Backend.X64.Parser.Gen.AbsXGAS.JMP _ label commentann -> failure x
  Backend.X64.Parser.Gen.AbsXGAS.JE _ label commentann -> failure x
  Backend.X64.Parser.Gen.AbsXGAS.JG _ label commentann -> failure x
  Backend.X64.Parser.Gen.AbsXGAS.JGE _ label commentann -> failure x
  Backend.X64.Parser.Gen.AbsXGAS.JL _ label commentann -> failure x
  Backend.X64.Parser.Gen.AbsXGAS.JLE _ label commentann -> failure x
  Backend.X64.Parser.Gen.AbsXGAS.JNE _ label commentann -> failure x
  Backend.X64.Parser.Gen.AbsXGAS.JZ _ label commentann -> failure x
  Backend.X64.Parser.Gen.AbsXGAS.JNZ _ label commentann -> failure x

transSource :: Show a => Backend.X64.Parser.Gen.AbsXGAS.Source' a -> Result
transSource x = case x of
  Backend.X64.Parser.Gen.AbsXGAS.FromConst _ constintref -> failure x
  Backend.X64.Parser.Gen.AbsXGAS.FromReg64 _ reg -> failure x
  Backend.X64.Parser.Gen.AbsXGAS.FromMem64 _ integer reg -> failure x
  Backend.X64.Parser.Gen.AbsXGAS.FromLabel64 _ label -> failure x
  Backend.X64.Parser.Gen.AbsXGAS.FromLabelOffset64 _ label -> failure x
  Backend.X64.Parser.Gen.AbsXGAS.FromMemComplex64 _ integer1 reg1 reg2 integer2 -> failure x
  Backend.X64.Parser.Gen.AbsXGAS.FromReg32 _ reg -> failure x
  Backend.X64.Parser.Gen.AbsXGAS.FromMem32 _ integer reg -> failure x
  Backend.X64.Parser.Gen.AbsXGAS.FromLabel32 _ label -> failure x
  Backend.X64.Parser.Gen.AbsXGAS.FromLabelOffset32 _ label -> failure x
  Backend.X64.Parser.Gen.AbsXGAS.FromMemComplex32 _ integer1 reg1 reg2 integer2 -> failure x
  Backend.X64.Parser.Gen.AbsXGAS.FromReg16 _ reg -> failure x
  Backend.X64.Parser.Gen.AbsXGAS.FromMem16 _ integer reg -> failure x
  Backend.X64.Parser.Gen.AbsXGAS.FromLabel16 _ label -> failure x
  Backend.X64.Parser.Gen.AbsXGAS.FromLabelOffset16 _ label -> failure x
  Backend.X64.Parser.Gen.AbsXGAS.FromMemComplex16 _ integer1 reg1 reg2 integer2 -> failure x
  Backend.X64.Parser.Gen.AbsXGAS.FromReg8 _ reg -> failure x
  Backend.X64.Parser.Gen.AbsXGAS.FromMem8 _ integer reg -> failure x
  Backend.X64.Parser.Gen.AbsXGAS.FromLabel8 _ label -> failure x
  Backend.X64.Parser.Gen.AbsXGAS.FromLabelOffset8 _ label -> failure x
  Backend.X64.Parser.Gen.AbsXGAS.FromMemComplex8 _ integer1 reg1 reg2 integer2 -> failure x

transTarget :: Show a => Backend.X64.Parser.Gen.AbsXGAS.Target' a -> Result
transTarget x = case x of
  Backend.X64.Parser.Gen.AbsXGAS.ToReg64 _ reg -> failure x
  Backend.X64.Parser.Gen.AbsXGAS.ToMem64 _ integer reg -> failure x
  Backend.X64.Parser.Gen.AbsXGAS.ToMemComplex64 _ integer1 reg1 reg2 integer2 -> failure x
  Backend.X64.Parser.Gen.AbsXGAS.ToReg32 _ reg -> failure x
  Backend.X64.Parser.Gen.AbsXGAS.ToMem32 _ integer reg -> failure x
  Backend.X64.Parser.Gen.AbsXGAS.ToMemComplex32 _ integer1 reg1 reg2 integer2 -> failure x
  Backend.X64.Parser.Gen.AbsXGAS.ToReg16 _ reg -> failure x
  Backend.X64.Parser.Gen.AbsXGAS.ToMem16 _ integer reg -> failure x
  Backend.X64.Parser.Gen.AbsXGAS.ToMemComplex16 _ integer1 reg1 reg2 integer2 -> failure x
  Backend.X64.Parser.Gen.AbsXGAS.ToReg8 _ reg -> failure x
  Backend.X64.Parser.Gen.AbsXGAS.ToMem8 _ integer reg -> failure x
  Backend.X64.Parser.Gen.AbsXGAS.ToMemComplex8 _ integer1 reg1 reg2 integer2 -> failure x

transReg :: Show a => Backend.X64.Parser.Gen.AbsXGAS.Reg' a -> Result
transReg x = case x of
  Backend.X64.Parser.Gen.AbsXGAS.RAX _ -> failure x
  Backend.X64.Parser.Gen.AbsXGAS.RBX _ -> failure x
  Backend.X64.Parser.Gen.AbsXGAS.RCX _ -> failure x
  Backend.X64.Parser.Gen.AbsXGAS.RDX _ -> failure x
  Backend.X64.Parser.Gen.AbsXGAS.RDI _ -> failure x
  Backend.X64.Parser.Gen.AbsXGAS.RSI _ -> failure x
  Backend.X64.Parser.Gen.AbsXGAS.RSP _ -> failure x
  Backend.X64.Parser.Gen.AbsXGAS.RBP _ -> failure x
  Backend.X64.Parser.Gen.AbsXGAS.R8 _ -> failure x
  Backend.X64.Parser.Gen.AbsXGAS.R9 _ -> failure x
  Backend.X64.Parser.Gen.AbsXGAS.R10 _ -> failure x
  Backend.X64.Parser.Gen.AbsXGAS.R11 _ -> failure x
  Backend.X64.Parser.Gen.AbsXGAS.R12 _ -> failure x
  Backend.X64.Parser.Gen.AbsXGAS.R13 _ -> failure x
  Backend.X64.Parser.Gen.AbsXGAS.R14 _ -> failure x
  Backend.X64.Parser.Gen.AbsXGAS.R15 _ -> failure x
  Backend.X64.Parser.Gen.AbsXGAS.EAX _ -> failure x
  Backend.X64.Parser.Gen.AbsXGAS.EBX _ -> failure x
  Backend.X64.Parser.Gen.AbsXGAS.ECX _ -> failure x
  Backend.X64.Parser.Gen.AbsXGAS.EDX _ -> failure x
  Backend.X64.Parser.Gen.AbsXGAS.EDI _ -> failure x
  Backend.X64.Parser.Gen.AbsXGAS.ESI _ -> failure x
  Backend.X64.Parser.Gen.AbsXGAS.ESP _ -> failure x
  Backend.X64.Parser.Gen.AbsXGAS.EBP _ -> failure x
  Backend.X64.Parser.Gen.AbsXGAS.R8D _ -> failure x
  Backend.X64.Parser.Gen.AbsXGAS.R9D _ -> failure x
  Backend.X64.Parser.Gen.AbsXGAS.R10D _ -> failure x
  Backend.X64.Parser.Gen.AbsXGAS.R11D _ -> failure x
  Backend.X64.Parser.Gen.AbsXGAS.R12D _ -> failure x
  Backend.X64.Parser.Gen.AbsXGAS.R13D _ -> failure x
  Backend.X64.Parser.Gen.AbsXGAS.R14D _ -> failure x
  Backend.X64.Parser.Gen.AbsXGAS.R15D _ -> failure x
  Backend.X64.Parser.Gen.AbsXGAS.AX _ -> failure x
  Backend.X64.Parser.Gen.AbsXGAS.BX _ -> failure x
  Backend.X64.Parser.Gen.AbsXGAS.CX _ -> failure x
  Backend.X64.Parser.Gen.AbsXGAS.DX _ -> failure x
  Backend.X64.Parser.Gen.AbsXGAS.DI _ -> failure x
  Backend.X64.Parser.Gen.AbsXGAS.SI _ -> failure x
  Backend.X64.Parser.Gen.AbsXGAS.SP _ -> failure x
  Backend.X64.Parser.Gen.AbsXGAS.BP _ -> failure x
  Backend.X64.Parser.Gen.AbsXGAS.R8W _ -> failure x
  Backend.X64.Parser.Gen.AbsXGAS.R9W _ -> failure x
  Backend.X64.Parser.Gen.AbsXGAS.R10W _ -> failure x
  Backend.X64.Parser.Gen.AbsXGAS.R11W _ -> failure x
  Backend.X64.Parser.Gen.AbsXGAS.R12W _ -> failure x
  Backend.X64.Parser.Gen.AbsXGAS.R13W _ -> failure x
  Backend.X64.Parser.Gen.AbsXGAS.R14W _ -> failure x
  Backend.X64.Parser.Gen.AbsXGAS.R15W _ -> failure x
  Backend.X64.Parser.Gen.AbsXGAS.AL _ -> failure x
  Backend.X64.Parser.Gen.AbsXGAS.BL _ -> failure x
  Backend.X64.Parser.Gen.AbsXGAS.CL _ -> failure x
  Backend.X64.Parser.Gen.AbsXGAS.DL _ -> failure x
  Backend.X64.Parser.Gen.AbsXGAS.DIL _ -> failure x
  Backend.X64.Parser.Gen.AbsXGAS.SIL _ -> failure x
  Backend.X64.Parser.Gen.AbsXGAS.SPL _ -> failure x
  Backend.X64.Parser.Gen.AbsXGAS.BPL _ -> failure x
  Backend.X64.Parser.Gen.AbsXGAS.R8B _ -> failure x
  Backend.X64.Parser.Gen.AbsXGAS.R9B _ -> failure x
  Backend.X64.Parser.Gen.AbsXGAS.R10B _ -> failure x
  Backend.X64.Parser.Gen.AbsXGAS.R11B _ -> failure x
  Backend.X64.Parser.Gen.AbsXGAS.R12B _ -> failure x
  Backend.X64.Parser.Gen.AbsXGAS.R13B _ -> failure x
  Backend.X64.Parser.Gen.AbsXGAS.R14B _ -> failure x
  Backend.X64.Parser.Gen.AbsXGAS.R15B _ -> failure x
