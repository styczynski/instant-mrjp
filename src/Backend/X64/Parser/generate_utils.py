import os, sys
sys.path.append(os.path.join(os.path.dirname(__file__)))

from defs import *

def create_data_types():
    ret = []
    for size in REGISTERS:
        regs = REGISTERS[size]
        ret = ret + [
            f"""
                data Reg{size} = {"| ".join([reg.upper() for reg in regs])}
                    deriving (Eq, Ord, Show, Read, Generic)

                
            """
        ]
    return "\n".join(ret)

def create_instr_wrappers():
    ret = []
    for instr in INSTR_ARITM_2OP:
        for size in REGISTERS:
            ret = ret+[
                f"""
                    {instr}{size} :: (IsTarget{size} t t1 t2 t3) => a -> t -> t -> AsmInstr' a
                    {instr}{size} pos arg1 arg2 = Syntax.{instr.upper()}{size} pos (asSource pos arg1) (asTarget pos arg2)
                """
            ]

    return ret

def generate_size_classes():
    ret = []
    for size in REGISTERS:
        rest_sizes = [nsize for nsize in REGISTERS if nsize != size]
        class_args = [f"target{rsize}" for rsize in rest_sizes]
        instance_args = [f"Reg{rsize}" for rsize in rest_sizes]
        convert_methods = [f"to{rsize} :: target -> target{rsize}" for rsize in rest_sizes]
        self_convert = f"""
            to{size} :: target -> target
            default to{size} :: target -> target
            to{size} = id
        """
        instance_convert_methods = []
        for rsize in rest_sizes:
            for (index, rreg) in enumerate(REGISTERS[rsize]):
                original_reg = REGISTERS[size][index]
                instance_convert_methods = instance_convert_methods + [
                    f"to{rsize} {original_reg.upper()} = {rreg.upper()}"
                ]
        source_target_convert_methods = []
        for reg in REGISTERS[size]:
            source_target_convert_methods = source_target_convert_methods + [
                f"asSource pos {reg.upper()} = Syntax.FromReg{size} pos $ Syntax.{reg.upper()} pos "
            ]
        for reg in REGISTERS[size]:
            source_target_convert_methods = source_target_convert_methods + [
                f"asTarget pos {reg.upper()} = Syntax.ToReg{size} pos $ Syntax.{reg.upper()} pos "
            ]
        ident = "\n   "
        ret = ret + [
            f"""
            class IsTarget{size} target {" ".join(class_args)} | target -> {" ".join(class_args)} where
                {ident.join(convert_methods)}
                {self_convert}
                asSource :: a -> target -> Syntax.Source' a
                asTarget :: a -> target -> Syntax.Target' a
            instance IsTarget{size} Reg{size} {" ".join(instance_args)} where
                {ident.join(instance_convert_methods)}
                {ident.join(source_target_convert_methods)}
            """,
        ]
    return "\n".join(ret)

def generate_utils(module, module_name, syntax_postfix, output_haskell_path):
    ident="\n"
    code = f"""
    module {module}.{module_name} where

    import qualified {module}.Abs{syntax_postfix} as Syntax

    # Registers
    {create_data_types()}

    # Size classes
    {generate_size_classes()}

    # Instruction wrappers
    {ident.join(create_instr_wrappers())}
    """
    with open(output_haskell_path, "w") as f:
            f.write("\n".join([line.strip() for line in code.splitlines()]))


if __name__ == "__main__":
    generate_utils(sys.argv[1], sys.argv[2], sys.argv[3], sys.argv[4])