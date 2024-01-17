import os, sys
sys.path.append(os.path.join(os.path.dirname(__file__)))

from defs import *

INDENT="<INDENT>"
INDENTNL = "\n<INDENT>"

def create_data_types():
    ret = []
    syms = []
    for size in REGISTERS:
        regs = REGISTERS[size]
        syms = syms + [f"Reg{size}(..)"]
        ret = ret + [
            f"""
                data Reg{size} = {"| ".join([reg.upper() for reg in regs])}
                {INDENT}deriving (Eq, Ord, Show, Read, Generic, Typeable)
            """
        ]
    return syms, "\n".join(ret)

def create_instr_wrappers():
    ret = []
    syms = []
    rest_cls_params = " ".join([f"t{size}" for size in REGISTERS])
    cls_spec = f"IsTarget t size {rest_cls_params}"
    stack_spec = f"IsTarget t SIZE64 {rest_cls_params}"
    set_spec = f"IsTarget t SIZE8 {rest_cls_params}"
    syms = syms + [f"Instr(..)"]
    ret = ret + [
        f"""
            data Instr a = Instr a (Syntax.AsmInstr' a) (Maybe (String))
            {INDENT}deriving (Eq, Ord, Show, Read, Generic, Foldable, Traversable, Functor, Typeable)

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
    for instr in INSTR_ARITM_1OP:
        ret = ret + [
            f"""_instr_{instr} :: Int -> (a -> Syntax.Target' a -> Syntax.AsmInstr' a)"""
        ]
        for size in REGISTERS:
            if size != "8":
                ret = ret + [
                    f"""_instr_{instr} {size} = Syntax.{instr.upper()}{size}"""
                ]

    for instr in INSTR_ARITM_2OP:
        syms = syms + [f"{instr}"]
        ret = ret+[
            f"""
                {instr} :: ({cls_spec}) => a -> t -> t -> ASMGenerator a ()
                {instr} pos arg1 arg2 = do
                    {INDENT}_emitInstr pos $ _wrap pos $ (_instr_{instr} (sizeOf arg1)) pos (asSource pos arg1) (asTarget pos arg2)
            """
        ]
    for instr in INSTR_ARITM_1OP:
        syms = syms + [f"{instr}"]
        ret = ret+[
            f"""
                {instr} :: ({cls_spec}) => a -> t -> ASMGenerator a ()
                {instr} pos arg1 = do
                    {INDENT}_emitInstr pos $ _wrap pos $ (_instr_{instr} (sizeOf arg1)) pos (asTarget pos arg1)
            """
        ]
    for instr in INSTR_JMP:
        syms = syms + [f"{instr}"]
        ret = ret+[
            f"""
                {instr} :: a -> String -> ASMGenerator a ()
                {instr} pos label = do
                    {INDENT}_emitInstr pos $ _wrap pos $ Syntax.{instr.upper()} pos (Syntax.Label label)
            """
        ]
    for instr in INSTR_STACK:
        syms = syms + [f"{instr}"]
        ret = ret+[
            f"""
                {instr} :: a -> Reg64 -> ASMGenerator a ()
                {instr} pos reg = do
                    {INDENT}_emitInstr pos $ _wrap pos $ Syntax.{instr.upper()} pos $ _reg64ToSReg pos reg
            """
        ]
    for instr in INSTR_NOARG:
        syms = syms + [f"{instr}"]
        ret = ret+[
            f"""
                {instr} :: a -> ASMGenerator a ()
                {instr} pos = do
                    {INDENT}_emitInstr pos $ _wrap pos $ Syntax.{instr.upper()} pos
            """
        ]
    for instr in INSTR_SET:
        syms = syms + [f"{instr}"]
        ret = ret+[
            f"""
                {instr} :: a -> Reg8 -> ASMGenerator a ()
                {instr} pos reg = do
                    {INDENT}_emitInstr pos $ _wrap pos $ Syntax.{instr.upper()} pos $ _reg8ToSReg pos reg
            """
        ]
    return syms, ret

def generate_size_classes():
    class_args = [f"target{rsize}" for rsize in REGISTERS]
    convert_methods = [f"to{rsize} :: target -> target{rsize}" for rsize in REGISTERS]
    ret = []
    for size in REGISTERS:
        ret = ret + [
            f"""
                data SIZE{size} = SIZE{size}
                {INDENT}deriving (Eq, Ord, Show, Read, Generic, Typeable)
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
        reg_conversions = [
            f"""_reg{size}ToSReg :: a -> Reg{size} -> Syntax.Reg' a"""
        ]
        for reg in REGISTERS[size]:
            reg_conversions = reg_conversions + [
                f"""_reg{size}ToSReg pos {reg.upper()} = Syntax.{reg.upper()} pos"""
            ]

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
        ret = ret + reg_conversions + [
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
        "DeriveDataTypeable",
    ]
    directives = ["{-# LANGUAGE "+lang+" #-}" for lang in langs]
    syms1, data_types_code = create_data_types()
    syms2, instr_wrappers_code = create_instr_wrappers()
    instr_wrappers_code = newline.join(instr_wrappers_code)
    
    exports = [
        "run",
        "ASMGenerator",
        "dataDef",
        "DataDef(..)",
        "Data(..)",
    ] + [sym for sym in (syms1 + syms2)]

    code = f"""
    {newline.join(directives)}
    module {module}.{module_name}({(newline+', ').join([exportName for exportName in exports])}) where

    import Data.Generics.Product
    import Data.Generics.Sum
    import GHC.Generics (Generic)
    import Data.Typeable
    import qualified Data.Data as D

    import Control.Monad.Except
    import Control.Monad.Reader
    import Control.Monad.Writer.Lazy

    import Prelude hiding (and)

    import qualified {syntax_module}.Abs{syntax_postfix} as Syntax
    import qualified {syntax_module}.Print{syntax_postfix} as Printer

    -- Data defintions
    data DataDef
    {INDENT}= DataGlobal String | DataDef String [Data]
    {INDENT}deriving (Eq, Ord, Show, Read, D.Data, Typeable, Generic)

    data Data
    {INDENT}= DataStr String
    {INDENT}| Data64I Integer
    {INDENT}| Data32I Integer
    {INDENT}| Data64From String
    {INDENT}| Data32From String
    {INDENT}deriving (Eq, Ord, Show, Read, D.Data, Typeable, Generic)

    convertDataDef :: a -> DataDef -> Syntax.AsmDataDef' a
    convertDataDef pos (DataGlobal label) = Syntax.AsmDataGlobal pos $ Syntax.Label label
    convertDataDef pos (DataDef label datas) = Syntax.AsmDataDef pos (Syntax.Label label) (map (convertData pos) datas)
        {INDENT}where
            {INDENT}{INDENT}convertData :: a -> Data -> Syntax.Data' a
            {INDENT}{INDENT}convertData pos (DataStr str) = Syntax.DataString pos str
            {INDENT}{INDENT}convertData pos (Data64I val) = Syntax.Data64 pos $ Syntax.ConstInt pos val
            {INDENT}{INDENT}convertData pos (Data32I val) = Syntax.Data32 pos $ Syntax.ConstInt pos val
            {INDENT}{INDENT}convertData pos (Data64From label) = Syntax.Data64 pos $ Syntax.ConstLabel pos $ Syntax.Label label
            {INDENT}{INDENT}convertData pos (Data32From label) = Syntax.Data32 pos $ Syntax.ConstLabel pos $ Syntax.Label label


    -- Assembly generator definition
    type ASMGenerator a v = (WriterT (GeneratorOut a) (Except String)) v

    data GeneratorOut a = GeneratorOut (Maybe a) [Instr a] [Syntax.AsmDataDef' a]

    instance Semigroup (GeneratorOut a) where
        {INDENT}(<>) (GeneratorOut Nothing instr1 defs1) (GeneratorOut (Just firstPos2) instr2 defs2) = GeneratorOut (Just firstPos2) (instr1 <> instr2) (defs1 <> defs2)
        {INDENT}(<>) (GeneratorOut (Just firstPos1) instr1 defs1) (GeneratorOut Nothing instr2 defs2) = GeneratorOut (Just firstPos1) (instr1 <> instr2) (defs1 <> defs2)
        {INDENT}(<>) (GeneratorOut firstPos1 instr1 defs1) (GeneratorOut _ instr2 defs2) = GeneratorOut firstPos1 (instr1 <> instr2) (defs1 <> defs2)

    instance Monoid (GeneratorOut a) where
    mempty = GeneratorOut Nothing [] []

    _emitInstr :: a -> Instr a -> ASMGenerator a ()
    _emitInstr pos instr = tell $ GeneratorOut (Just pos) [instr] []

    _emitDef :: a -> Syntax.AsmDataDef' a -> ASMGenerator a ()
    _emitDef pos def = tell $ GeneratorOut (Just pos) [] [def]

    run :: (ASMGenerator a v) -> [String] -> Either String (String, v)
    run generator externs =
    {INDENT}case runExcept (runWriterT generator) of
    {INDENT}{INDENT}Left err -> Left err
    {INDENT}{INDENT}Right (_, GeneratorOut Nothing _ _) -> Left "Nothing generated"
    {INDENT}{INDENT}Right (result, GeneratorOut (Just pos) instrs defs) ->
    {INDENT}{INDENT}{INDENT}let topDirectives = map (Syntax.Extern pos . Syntax.Label) externs in
    {INDENT}{INDENT}{INDENT}let fullProg = Syntax.AsmProgram pos topDirectives (Syntax.SectionData pos defs) (Syntax.SectionCode pos $ map (\(Instr _ ins _) -> ins) instrs) in
    {INDENT}{INDENT}{INDENT}let assemblyCodeStr = Printer.printTree fullProg in
    {INDENT}{INDENT}{INDENT}Right (assemblyCodeStr, result)

    -- Registers
    {data_types_code}

    -- Size classes
    {generate_size_classes()}

    -- Instruction wrappers
    {instr_wrappers_code}

    -- Data wrappers
    dataDef :: a -> DataDef -> ASMGenerator a ()
    dataDef pos def = _emitDef pos $ convertDataDef pos def
    """
    with open(output_haskell_path, "w") as f:
            f.write("\n".join([line.strip().replace(INDENT, "\t") for line in code.splitlines()]))


if __name__ == "__main__":
    generate_utils(sys.argv[1], sys.argv[2], sys.argv[3], sys.argv[4], sys.argv[5])