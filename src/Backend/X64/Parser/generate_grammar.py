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
            rule(f"FromConst", f"Source{size}", f"Integer"),
            rule(f"ToReg{size}", f"Target{size}", f"Reg{size}"),
            rule(f"ToMem{size}", f"Target{size}", "Integer", lit("("), f"Reg{size}", lit(")")),
            rule(f"FromReg{size}", f"Source{size}", f"Reg{size}"),
            rule(f"FromMem{size}", f"Source{size}", "Integer", lit("("), f"Reg{size}", lit(")")),
        ] + [rule(f"{reg.upper()}", f"Reg{size}", lit(f"%{reg.upper()}")) for reg in regs]
    return rules

def generate_grammar(output_grammar_path):
    grammar = f"""
        entrypoints AsmProgram ;

        AsmProgram.   AsmProgram ::= [Directive] SectionData SectionCode ;
        separator  AsmInstr "" ;

        SectionData. SectionData ::= ".section" ".rodata" [AsmDataDef];
        SectionCode. SectionCode ::= ".section" ".text" [AsmInstr] ;

        AsmDataGlobal .AsmDataDef ::= ".global" Label ;
        AsmDataDef. AsmDataDef ::= Label ":" [Data] ;
        separator AsmDataDef "\\n" ;

        DataString. Data ::= ".string" String ;
        Data64. Data ::= ".quad" DataConst ;
        Data32. Data ::= ".long" DataConst ;
        separator Data "\\n" ;

        ConstInt. DataConst ::= "$" Integer ;
        ConstLabel. DataConst ::= Label ;

        Extern. Directive ::= ".extern " Label ;
        separator Directive "\\n" ;

        token Label (letter | digit | '_' | '\\\'')+ ;

        LabelDef. AsmInstr ::= Label ":" ;
        separator AsmInstr "\\n" ;

        -- 2-operand arithmetics
        {embed([rule(instr.upper()+"64", "AsmInstr", lit(instr+"q"), "Source64", comma, "Target64") for instr in INSTR_ARITM_2OP])}
        {embed([rule(instr.upper()+"32", "AsmInstr", lit(instr+"l"), "Source32", comma, "Target32") for instr in INSTR_ARITM_2OP])}
        {embed([rule(instr.upper()+"16", "AsmInstr", lit(instr+"b"), "Source16", comma, "Target16") for instr in INSTR_ARITM_2OP])}

        -- 1-operand arithmetics
        {embed([rule(instr.upper()+"64", "AsmInstr", lit(instr+"q"), "Target64") for instr in INSTR_ARITM_1OP])}
        {embed([rule(instr.upper()+"32", "AsmInstr", lit(instr+"l"), "Target32") for instr in INSTR_ARITM_1OP])}
        {embed([rule(instr.upper()+"16", "AsmInstr", lit(instr+"b"), "Target16") for instr in INSTR_ARITM_1OP])}

        -- Stack operations
        {embed([rule(instr.upper(), "AsmInstr", lit(instr), "Reg64") for instr in INSTR_STACK])}

        -- Zero arg instructions
        {embed([rule(instr.upper(), "AsmInstr", lit(instr)) for instr in INSTR_NOARG])}

        -- Set instructions
        {embed([rule(instr.upper(), "AsmInstr", lit(instr), "Reg8") for instr in INSTR_SET])}

        -- Jumps
        {embed([rule(instr.upper(), "AsmInstr", lit(instr), "Label") for instr in INSTR_JMP])}

        -- Registers
        {embed(generate_reg_defs())}

        comment    "#" ;
    """
    with open(output_grammar_path, "w") as f:
        f.write("\n".join([line.strip() for line in grammar.splitlines()]))

if __name__ == "__main__":
    generate_grammar(sys.argv[1])