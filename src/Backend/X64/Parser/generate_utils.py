import os, sys
sys.path.append(os.path.join(os.path.dirname(__file__)))

from defs import *

INDENT="<INDENT>"
INDENTNL = "\n<INDENT>"

def create_data_types():
    ret = []
    for size in REGISTERS:
        regs = REGISTERS[size]
        ret = ret + [
            f"""
                data Reg{size} = {"| ".join([reg.upper() for reg in regs])}
                {INDENT}deriving (Eq, Ord, Show, Read, Generic)
            """
        ]
    return "\n".join(ret)

def create_instr_wrappers():
    ret = []
    rest_cls_params = " ".join([f"t{size}" for size in REGISTERS])
    cls_spec = f"IsTarget t size {rest_cls_params}"
    ret = ret + [
        f"""
            data Instr a = Instr a (Syntax.AsmInstr' a) (Maybe (String))
            {INDENT}deriving (Eq, Ord, Show, Read, Generic, Foldable, Traversable, Functor)

            _wrap :: a -> Syntax.AsmInstr' a -> Instr a
            _wrap pos instr = Instr pos instr Nothing
        """
    ]
    # ret = ret + [
    #     f"""
    #         data _Instr a = {"| ".join(["_"+instr.upper()+" a" for instr in INSTR_ALL])}
    #             {INDENT}deriving (Eq, Ord, Show, Read, Generic, Foldable, Traversable, Functor)
    #     """
    # ]
    for instr in INSTR_ARITM_2OP:
        ret = ret + [
            f"""_instr_{instr} :: Int -> (a -> Syntax.Source' a -> Syntax.Target' a -> Syntax.AsmInstr' a)"""
        ]
        for size in REGISTERS:
            if size != "8":
                ret = ret + [
                    f"""_instr_{instr} {size} = Syntax.{instr.upper()}{size}"""
                ]
        #ret = ret + f"""_instr_{instr} _ = -- Invalid size"""

    for instr in INSTR_ARITM_2OP:
        ret = ret+[
            f"""
                {instr} :: ({cls_spec}) => a -> t -> t -> ASMGenerator a ()
                {instr} pos arg1 arg2 = do
                    {INDENT}_emitInstr pos [_wrap pos $ (_instr_{instr} (sizeOf arg1)) pos (asSource pos arg1) (asTarget pos arg2)]
            """
        ]

    return ret

def generate_size_classes():
    class_args = [f"target{rsize}" for rsize in REGISTERS]
    convert_methods = [f"to{rsize} :: target -> target{rsize}" for rsize in REGISTERS]
    ret = []
    for size in REGISTERS:
        ret = ret + [
            f"""
                data SIZE{size} = SIZE{size}
                {INDENT}deriving (Eq, Ord, Show, Read, Generic)
            """
        ]
    ret = ret + [
        f"""
            class IsTarget target size {" ".join(class_args)} | target -> size {" ".join(class_args)} where
                {INDENT}{INDENTNL.join(convert_methods)}
                {INDENT}asSource :: a -> target -> Syntax.Source' a
                {INDENT}asTarget :: a -> target -> Syntax.Target' a
                {INDENT}sizeOf :: target -> Int
        """
    ]
    for size in REGISTERS:
        rest_sizes = [nsize for nsize in REGISTERS if nsize != size]
        instance_args = [f"Reg{rsize}" for rsize in REGISTERS]
        # self_convert = f"""
        #     {INDENT}size{size}to{size} :: target -> target
        #     {INDENT}default size{size}to{size} :: target -> target
        #     {INDENT}size{size}to{size} = id
        # """
        instance_convert_methods = []
        for rsize in REGISTERS:
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
        ret = ret + [
            f"""
            instance IsTarget Reg{size} SIZE{size} {" ".join(instance_args)} where
                {INDENT}{INDENTNL.join(instance_convert_methods)}
                {INDENT}{INDENTNL.join(source_target_convert_methods)}
                {INDENT}sizeOf _ = {size}
            """,
        ]
    return "\n".join(ret)

def generate_utils(module, module_name, syntax_module, syntax_postfix, output_haskell_path):
    newline="\n"
    langs = [
        "DeriveTraversable",
        "TemplateHaskell",
        "DeriveGeneric",
        "DefaultSignatures",
    ]
    directives = ["{-# LANGUAGE "+lang+" #-}" for lang in langs]
    code = f"""
    {newline.join(directives)}
    module {module}.{module_name} where

    import Data.Generics.Product
    import Data.Generics.Sum
    import GHC.Generics (Generic)

    import Control.Monad.Except
    import Control.Monad.Reader
    import Control.Monad.Writer.Lazy

    import qualified {syntax_module}.Abs{syntax_postfix} as Syntax

    -- Assembly generator definition
    type ASMGenerator a v = (WriterT (GeneratorOut a) (Except String)) v

    data GeneratorOut a = GeneratorOut [Instr a] [Syntax.AsmDataDef' a]

    instance Semigroup (GeneratorOut a) where
    GeneratorOut instr1 defs1 <> GeneratorOut instr2 defs2 = GeneratorOut (instr1 <> instr2) (defs1 <> defs2)

    instance Monoid (GeneratorOut a) where
    mempty = GeneratorOut [] []

    _emitInstr :: a -> Instr a -> ASMGenerator a ()
    _emitInstr pos instr = tell $ GeneratorOut [instr] []

    _emitDef :: a -> Syntax.AsmDataDef' a -> ASMGenerator a ()
    _emitDef pos def = tell $ GeneratorOut [] [def]

    -- Registers
    {create_data_types()}

    -- Size classes
    {generate_size_classes()}

    -- Instruction wrappers
    {newline.join(create_instr_wrappers())}
    """
    with open(output_haskell_path, "w") as f:
            f.write("\n".join([line.strip().replace(INDENT, "\t") for line in code.splitlines()]))


if __name__ == "__main__":
    generate_utils(sys.argv[1], sys.argv[2], sys.argv[3], sys.argv[4], sys.argv[5])