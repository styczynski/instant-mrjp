import os, sys
sys.path.append(os.path.join(os.path.dirname(__file__)))

from defs import *

def rule(nameStr, typeStr, *defs):
    return f"{nameStr}. {typeStr} ::= {' '.join(defs)} ;\n"

def lit(text):
    return f'"{text}"'

comma = '","'

def embed(rules):
    return "".join(rules)

def generate_reg_defs():
    rules = []
    for size in REGISTERS:
        regs = REGISTERS[size]
        rules = rules + [
            rule(f"FromConst", f"Source{size}", f"ConstIntRef"),
            rule(f"ToReg{size}", f"Target{size}", f"Reg{size}"),
            rule(f"ToMem{size}", f"Target{size}", "Integer", lit("("), f"Reg{size}", lit(")")),
            rule(f"FromReg{size}", f"Source{size}", f"Reg{size}"),
            rule(f"FromMem{size}", f"Source{size}", "Integer", lit("("), f"Reg{size}", lit(")")),
            rule(f"FromLabel{size}", f"Source{size}", f"Label"),
            rule(f"FromLabelOffset{size}", f"Source{size}", f"Label", lit("(%RIP)")),
            #--offset(%baseLoc, %idxLoc, 2)
            rule(f"FromMemComplex{size}", f"Source{size}", "Integer", lit("("), f"Reg{size}", lit(","), f"Reg{size}", lit(","), "Integer", lit(")")),
            rule(f"ToMemComplex{size}", f"Target{size}", "Integer", lit("("), f"Reg{size}", lit(","), f"Reg{size}", lit(","), "Integer", lit(")")),
        ] + [rule(f"{reg.upper()}", f"Reg{size}", lit(f"%{reg.upper()}")) for reg in regs]
    return rules

def generate_grammar(output_grammar_path):
    newline = lit("<ENDL>")
    grammar = f"""
        entrypoints AsmProgram ;

        AsmProgram.   AsmProgram ::= [Directive] {newline} SectionData {newline} SectionCode ;
        separator  AsmInstr "" ;

        SectionData. SectionData ::= ".section" ".rodata" {newline} [AsmDataDef];
        SectionCode. SectionCode ::= ".section" ".text" {newline} [AsmInstr] ;

        AsmDataGlobal .AsmDataDef ::= ".global" Label {newline} ;
        AsmDataDef. AsmDataDef ::= Label ":" {newline} [Data] {newline} ;
        separator AsmDataDef "" ;

        Comment. CommentAnn ::= CommentLike ;
        NoComment. CommentAnn ::= ;

        DataString. Data ::= ".string" String {newline} ;
        Data64. Data ::= ".quad" DataConst {newline} ;
        Data32. Data ::= ".long" DataConst {newline} ;
        separator Data "" ;

        ConstInt. DataConst ::= Integer ;
        ConstLabel. DataConst ::= Label ;

        Extern. Directive ::= ".extern " Label {newline} ;
        separator Directive "" ;

        token CommentLike '#' '-' '-' ((char - ["\\"\\\\"]) | ('\\\\' ["\\"\\\\tnrf"]))* '-' '-' '#';

        token ConstIntRef '$' (digit)+ ;
        token Label (letter | digit | '_' | '\\\'')+ ;

        LabelDef. AsmInstr ::= Label ":" CommentAnn {newline};
        separator AsmInstr "" ;

        -- 2-operand arithmetics
        {embed([rule(instr.upper()+"64", "AsmInstr", lit(instr+"q"), "Source64", comma, "Target64", "CommentAnn", newline) for instr in INSTR_ARITM_2OP])}
        {embed([rule(instr.upper()+"32", "AsmInstr", lit(instr+"l"), "Source32", comma, "Target32", "CommentAnn", newline) for instr in INSTR_ARITM_2OP])}
        {embed([rule(instr.upper()+"16", "AsmInstr", lit(instr+"w"), "Source16", comma, "Target16", "CommentAnn", newline) for instr in INSTR_ARITM_2OP])}
        {embed([rule(instr.upper()+"8", "AsmInstr", lit(instr+"b"), "Source8", comma, "Target8", "CommentAnn", newline) for instr in INSTR_ARITM_2OP])}

        -- 1-operand arithmetics
        {embed([rule(instr.upper()+"64", "AsmInstr", lit(instr+"q"), "Target64", "CommentAnn", newline) for instr in INSTR_ARITM_1OP])}
        {embed([rule(instr.upper()+"32", "AsmInstr", lit(instr+"l"), "Target32", "CommentAnn", newline) for instr in INSTR_ARITM_1OP])}
        {embed([rule(instr.upper()+"16", "AsmInstr", lit(instr+"w"), "Target16", "CommentAnn", newline) for instr in INSTR_ARITM_1OP])}
        {embed([rule(instr.upper()+"8", "AsmInstr", lit(instr+"b"), "Target16", "CommentAnn", newline) for instr in INSTR_ARITM_1OP])}

        -- Calls
        {embed([
            rule("CALL", "AsmInstr", lit("call"), "Label", "CommentAnn", newline),
            rule("CALLINDIRECT", "AsmInstr", lit("call"), lit("*"), "Integer", lit("("), "Reg64", lit(")"), "CommentAnn", newline),
        ])}

        -- Stack operations
        {embed([rule(instr.upper(), "AsmInstr", lit(instr), "Reg64", "CommentAnn", newline) for instr in INSTR_STACK])}

        -- Zero arg instructions
        {embed([rule(instr.upper(), "AsmInstr", lit(instr), "CommentAnn", newline) for instr in INSTR_NOARG])}

        -- Set instructions
        {embed([rule(instr.upper(), "AsmInstr", lit(instr), "Reg8", "CommentAnn", newline) for instr in INSTR_SET])}

        -- Jumps
        {embed([rule(instr.upper(), "AsmInstr", lit(instr), "Label", "CommentAnn", newline) for instr in INSTR_JMP])}

        -- Registers
        {embed(generate_reg_defs())}

        -- comment    "#" ;
    """
    with open(output_grammar_path, "w") as f:
        f.write("\n".join([line.strip() for line in grammar.splitlines()]))

if __name__ == "__main__":
    generate_grammar(sys.argv[1])