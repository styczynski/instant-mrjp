import os, sys
sys.path.append(os.path.join(os.path.dirname(__file__)))

from defs import *

INDENT="<INDENT>"
INDENTNL = "\n<INDENT>"

def create_data_types():
    ret = []
    syms = []
    # for size in REGISTERS:
    #     regs = REGISTERS[size]
    #     syms = syms + [f"Reg{size}(..)"]
    #     ret = ret + [
    #         f"""
    #             data Reg{size} = {"| ".join([reg.upper() for reg in regs])}
    #             {INDENT}deriving (Eq, Ord, Show, Read, Generic, Typeable)
    #         """
    #     ]
    syms = syms + ["CommentProvider(..)", "Reg(..)", "Loc(..)", "RegType(..)", "isReg", "asReg", "argLoc", "allRegs", "regType", "asLoc", "showReg"]
    allocable_registers = [reg.upper() for reg in REGISTERS['64'] if REGISTERS_ALLOCABLE[reg.upper()]]
    ret = ret + [
        f"""
            data Reg = {"| ".join([reg.upper() for reg in REGISTERS['64']])}
            {INDENT}deriving (Eq, Ord, Show, Read, Generic, Typeable)

            data RegType = CallerSaved | CalleeSaved deriving (Eq, Show)

            -- Caller saved registers are preferred over callee saved.
            instance Ord RegType where
                {INDENT}compare rt1 rt2 = case (rt1, rt2) of
                    {INDENT}{INDENT}(CallerSaved, CallerSaved) -> EQ
                    {INDENT}{INDENT}(CalleeSaved, CalleeSaved) -> EQ
                    {INDENT}{INDENT}(CalleeSaved, CallerSaved) -> LT
                    {INDENT}{INDENT}(CallerSaved, CalleeSaved) -> GT

            allRegs :: [Reg]
            allRegs = [{", ".join(allocable_registers)}]

            asLoc :: Reg -> Loc
            asLoc reg = LocReg reg

            sanitizeLabel :: String -> String
            sanitizeLabel s = case s of
                {INDENT}[]     -> []
                {INDENT}'~':xs -> '_':'_':sanitizeLabel xs
                {INDENT}x:xs   -> x:sanitizeLabel xs

        """
    ]
    ret = ret + [f"""regType :: Reg -> RegType"""]
    for reg in REGISTERS['64']:
        reg_type = REGISTERS_TYPES[reg.upper()]
        ret = ret + [f"""regType {reg.upper()} = {reg_type}"""]

    ret = ret + [f"""showReg :: Size -> Reg -> String"""]
    for size in REGISTERS:
        for (index, reg) in enumerate(REGISTERS[size]):
            reg64 = REGISTERS['64'][index]
            ret = ret + [f"""showReg Size{size.upper()} {reg64.upper()} = \"{reg.upper()}\""""]

    ret = ret + [
        f"""

            data Loc = LocLabel String | LocLabelPIC String | LocConst Integer | LocReg Reg | LocMem (Reg, Int64) | LocMemOffset {{ ptrBase :: Reg, ptrIdx :: Reg, ptrOffset :: Int64, ptrScale :: Size }}
            {INDENT}deriving (Eq, Ord, Show, Read, Generic, Typeable)

            isReg :: Loc -> Bool
            isReg loc = case loc of
                {INDENT}LocReg _ -> True
                {INDENT}_        -> False

            asReg :: Loc -> Reg
            asReg loc = case loc of
                {INDENT}LocReg r -> r
                {INDENT}_        -> error "asReg: not a reg"

            data Annotation a anno = NoAnnotation a | Annotation a anno
            {INDENT}deriving (Eq, Ord, Show, Read, Generic, Foldable, Traversable, Functor, Typeable)

            class CommentProvider a anno | a -> anno where
            {INDENT}toComment :: a -> anno -> String
            {INDENT}defaultComment :: a -> Maybe String
            {INDENT}defaultComment _ = Nothing

            _convert_annotation :: (CommentProvider a anno) => Annotation a anno -> Syntax.CommentAnn' a 
            _convert_annotation (NoAnnotation pos) = case (defaultComment pos) of
            {INDENT}Nothing -> Syntax.NoComment pos
            {INDENT}(Just comment) -> Syntax.Comment pos $ Syntax.CommentLike $ "#-- " ++ comment ++ " --#"
            _convert_annotation (Annotation pos ann) = Syntax.Comment pos $ Syntax.CommentLike $ "#-- " ++ (toComment pos ann) ++ " --#"

        """
    ]
    ret = ret + [f"""argLoc :: Integer -> Loc"""]
    for (index, arg_reg) in enumerate(ARG_REGISTERS):
        ret = ret + [
            f"""argLoc {index} = LocReg {arg_reg.upper()}"""
        ]
    ret = ret + [
        f"""argLoc argIndex = LocMem (RBP, (fromInteger argIndex - 6) * 8 + 8)
        """,
    ]
    to_source_defs = [
        f"""_locToSource :: a -> Size -> Loc -> Syntax.Source' a""",
        f"""_locToSource pos _ (LocConst val) = Syntax.FromConst pos $ Syntax.ConstIntRef $ "$" ++ show val"""
    ]
    to_target_defs = [
        f"""_locToTarget :: a -> Size -> Loc -> Syntax.Target' a""",
    ]
    for size in REGISTERS:
        to_source_defs = to_source_defs + [
            f"""_locToSource pos Size{size} (LocLabel l) = Syntax.FromLabel{size} pos $ Syntax.Label $ sanitizeLabel l""",
            f"""_locToSource pos Size{size} (LocLabelPIC l) = Syntax.FromLabelOffset{size} pos $ Syntax.Label $ sanitizeLabel l""",
            f"""_locToSource pos Size{size} (LocMemOffset ptrBase ptrIdx ptrOffset ptrScale) =
                {INDENT}let (Syntax.FromMem{size} _ _ rBase) = _locToSource pos Size{size} (LocMem (ptrBase, 0)) in
                {INDENT}let (Syntax.FromMem{size} _ _ rIdx) =  _locToSource pos Size{size} (LocMem (ptrIdx, 0)) in
                {INDENT}Syntax.FromMemComplex{size} pos (fromIntegral ptrOffset) rBase rIdx (fromIntegral $ toBytes $ ptrScale)""",
        ]
        to_target_defs = to_target_defs + [
            f"""_locToTarget pos Size{size} (LocMemOffset ptrBase ptrIdx ptrOffset ptrScale) =
                {INDENT}let (Syntax.ToMem{size} _ _ rBase) = _locToTarget pos Size{size} (LocMem (ptrBase, 0)) in
                {INDENT}let (Syntax.ToMem{size} _ _ rIdx) =  _locToTarget pos Size{size} (LocMem (ptrIdx, 0)) in
                {INDENT}Syntax.ToMemComplex{size} pos (fromIntegral ptrOffset) rBase rIdx (fromIntegral $ toBytes $ ptrScale)""",
        ]
        for (index, reg) in enumerate(REGISTERS['64']):
            reg_resized = REGISTERS[size][index]
            to_source_defs = to_source_defs + [
                f"""_locToSource pos Size{size} (LocReg {reg.upper()}) = Syntax.FromReg{size} pos $ Syntax.{reg_resized.upper()} pos""",
                f"""_locToSource pos Size{size} (LocMem ({reg.upper()}, offset)) = Syntax.FromMem{size} pos (fromIntegral offset) $ Syntax.{reg.upper()} pos""",
                #baseLoc offset idxLoc scale offset(%baseLoc, %idxLoc, 2)
            ]
            to_target_defs = to_target_defs + [
                f"""_locToTarget pos Size{size} (LocReg {reg.upper()}) = Syntax.ToReg{size} pos $ Syntax.{reg_resized.upper()} pos""",
                f"""_locToTarget pos Size{size} (LocMem ({reg.upper()}, offset)) = Syntax.ToMem{size} pos (fromIntegral offset) $ Syntax.{reg.upper()} pos""",
            ]
    ret = ret + to_source_defs + to_target_defs
    return syms, "\n".join(ret)

def create_instr_wrappers():
    ret = []
    syms = []
    syms = syms + [f"Instr(..)"]
    instr_variants = []
    for instr in INSTR_ARITM_2OP:
        instr_variants = instr_variants + [
            f"""{instr.upper()} a Size (Loc) (Loc) (Annotation a anno)""",
        ]
    for instr in INSTR_ARITM_1OP:
        instr_variants = instr_variants + [
            f"""{instr.upper()} a Size (Loc) (Annotation a anno)""",
        ]
    for instr in INSTR_JMP:
        instr_variants = instr_variants + [
            f"""{instr.upper()} a (String) (Annotation a anno)""",
        ]
    for instr in INSTR_SET:
        instr_variants = instr_variants + [
            f"""{instr.upper()} a (Loc) (Annotation a anno)""",
        ]
    for instr in INSTR_STACK:
        instr_variants = instr_variants + [
            f"""{instr.upper()} a (Loc) (Annotation a anno)""",
        ]
    for instr in INSTR_NOARG:
        instr_variants = instr_variants + [
            f"""{instr.upper()} a (Annotation a anno)""",
        ]
    ret = ret + [
        f"""
            data Instr a anno = CALL a String (Annotation a anno) | CALL_INDIRECT a Reg Integer (Annotation a anno) | Label a String (Annotation a anno) | {' | '.join(instr_variants)}
            {INDENT}deriving (Eq, Ord, Show, Read, Generic, Foldable, Traversable, Functor, Typeable)

        """
    ]
    # ret = ret + [
    #     f"""
    #         data _Instr a = {"| ".join(["_"+instr.upper()+" a" for instr in INSTR_ALL])}
    #             {INDENT}deriving (Eq, Ord, Show, Read, Generic, Foldable, Traversable, Functor)
    #     """
    # ]
    # for instr in INSTR_ARITM_2OP:
    #     ret = ret + [
    #         f"""_instr_{instr} :: Int -> (a -> Syntax.Source' a -> Syntax.Target' a -> Syntax.AsmInstr' a)"""
    #     ]
    #     for size in REGISTERS:
    #         if size != "8":
    #             ret = ret + [
    #                 f"""_instr_{instr} {size} = Syntax.{instr.upper()}{size}"""
    #             ]
    # for instr in INSTR_ARITM_1OP:
    #     ret = ret + [
    #         f"""_instr_{instr} :: Int -> (a -> Syntax.Target' a -> Syntax.AsmInstr' a)"""
    #     ]
    #     for size in REGISTERS:
    #         if size != "8":
    #             ret = ret + [
    #                 f"""_instr_{instr} {size} = Syntax.{instr.upper()}{size}"""
    #            
    syms = syms + ["label"]
    ret = ret+[
        f"""
        label :: (Monad m) => a -> String -> (Maybe anno) -> ASMGeneratorT a anno m ()
        label pos l ann =
            {INDENT}_emitInstr pos $ Label pos (sanitizeLabel l) $ maybe (NoAnnotation pos) (Annotation pos) ann
        """
    ]
    for instr in INSTR_ARITM_2OP:
        syms = syms + [f"{instr}"]
        ret = ret+[
            f"""
                {instr} :: (Monad m) => a -> Size -> Loc -> Loc -> (Maybe anno) -> ASMGeneratorT a anno m ()
                {instr} pos size loc1 loc2 ann =
                    {INDENT}_emitInstr pos $ {instr.upper()} pos size loc1 loc2 $ maybe (NoAnnotation pos) (Annotation pos) ann
            """
        ]
    for instr in INSTR_ARITM_1OP:
        syms = syms + [f"{instr}"]
        ret = ret+[
            f"""
                {instr} :: (Monad m) => a -> Size -> Loc -> (Maybe anno) -> ASMGeneratorT a anno m ()
                {instr} pos size loc ann =
                    {INDENT}_emitInstr pos $ {instr.upper()} pos size loc $ maybe (NoAnnotation pos) (Annotation pos) ann
            """
        ]
    for instr in INSTR_SET:
        syms = syms + [f"{instr}"]
        ret = ret+[
            f"""
                {instr} :: (Monad m) => a -> Loc -> (Maybe anno) -> ASMGeneratorT a anno m ()
                {instr} pos loc ann =
                    {INDENT}_emitInstr pos $ {instr.upper()} pos loc $ maybe (NoAnnotation pos) (Annotation pos) ann
            """
        ]
    for instr in INSTR_STACK:
        syms = syms + [f"{instr}"]
        ret = ret+[
            f"""
                {instr} :: (Monad m) => a -> Loc -> (Maybe anno) -> ASMGeneratorT a anno m ()
                {instr} pos loc ann =
                    {INDENT}_emitInstr pos $ {instr.upper()} pos loc $ maybe (NoAnnotation pos) (Annotation pos) ann
            """
        ]
    for instr in INSTR_NOARG:
        syms = syms + [f"{instr}"]
        ret = ret+[
            f"""
                {instr} :: (Monad m) => a -> (Maybe anno) -> ASMGeneratorT a anno m ()
                {instr} pos ann =
                    {INDENT}_emitInstr pos $ {instr.upper()} pos $ maybe (NoAnnotation pos) (Annotation pos) ann
            """
        ]
    for instr in INSTR_JMP:
        syms = syms + [f"{instr}"]
        ret = ret+[
            f"""
                {instr} :: (Monad m) => a -> String -> (Maybe anno) -> ASMGeneratorT a anno m ()
                {instr} pos label ann =
                    {INDENT}_emitInstr pos $ {instr.upper()} pos label $ maybe (NoAnnotation pos) (Annotation pos) ann
            """
        ]
    # Calls
    syms = syms + [f"call", "callIndirect"]
    ret = ret+[
        f"""
            call :: (Monad m) => a -> String -> (Maybe anno) -> ASMGeneratorT a anno m ()
            call pos label ann =
                {INDENT}_emitInstr pos $ CALL pos label $ maybe (NoAnnotation pos) (Annotation pos) ann
            callIndirect :: (Monad m) => a -> Reg -> Integer -> (Maybe anno) -> ASMGeneratorT a anno m ()
            callIndirect pos reg offset ann =
                {INDENT}_emitInstr pos $ CALL_INDIRECT pos reg offset $ maybe (NoAnnotation pos) (Annotation pos) ann
        """
    ]
    ret = ret + [
        f"""_convertInstr :: (Monad m, CommentProvider a anno) => Instr a anno -> ASMGeneratorT a anno m (Syntax.AsmInstr' a)""",
        f"""_convertInstr (Label pos l ann) = return $ Syntax.LabelDef pos (Syntax.Label $ sanitizeLabel l) (_convert_annotation ann)"""
    ]
    for instr in INSTR_ARITM_2OP:
        for size in REGISTERS:
            ret = ret+[
                f"""_convertInstr ({instr.upper()} pos Size{size} loc1 loc2 ann) = return $ Syntax.{instr.upper()}{size} pos (_locToSource pos Size{size} loc1) (_locToTarget pos Size{size} loc2) (_convert_annotation ann)""",
            ]
        ret = ret+[
            f"""_convertInstr ({instr.upper()} pos size _ _ _) = generatorFail $ EDataUnexpectedSize pos size"""
        ]
    for instr in INSTR_ARITM_1OP:
        for size in REGISTERS:
            ret = ret+[
                f"""_convertInstr ({instr.upper()} pos Size{size} loc ann) = return $ Syntax.{instr.upper()}{size} pos (_locToTarget pos Size{size} loc) (_convert_annotation ann)""",
            ]
        ret = ret+[
            f"""_convertInstr ({instr.upper()} pos size _ _) = generatorFail $ EDataUnexpectedSize pos size"""
        ]
    for instr in INSTR_SET:
        ret = ret+[
            f"""_convertInstr ({instr.upper()} pos loc ann) = let (Syntax.ToReg8 _ r) = (_locToTarget pos Size8 loc) in return $ Syntax.{instr.upper()} pos r (_convert_annotation ann)""",
            f"""_convertInstr ({instr.upper()} pos loc _) = generatorFail $ ENonRegisterLocationGiven pos loc""",
        ]
    for instr in INSTR_STACK:
        ret = ret+[
            f"""_convertInstr ({instr.upper()} pos loc@(LocReg reg) ann) = let (Syntax.ToReg64 _ r) = (_locToTarget pos Size64 loc) in return $ Syntax.{instr.upper()} pos r (_convert_annotation ann)""",
            f"""_convertInstr ({instr.upper()} pos loc _) = generatorFail $ ENonRegisterLocationGiven pos loc""",
        ]
    for instr in INSTR_JMP:
        ret = ret+[
            f"""_convertInstr ({instr.upper()} pos label ann) = return $ Syntax.{instr.upper()} pos (Syntax.Label $ sanitizeLabel label) (_convert_annotation ann)""",
        ]
    for instr in INSTR_NOARG:
        ret = ret+[
            f"""_convertInstr ({instr.upper()} pos ann) = return $ Syntax.{instr.upper()} pos (_convert_annotation ann)""",
        ]
    # Calls
    ret = ret+[
        f"""_convertInstr (CALL pos l ann) = return $ Syntax.CALL pos (Syntax.Label $ sanitizeLabel l) (_convert_annotation ann)""",
    ]
    for reg in REGISTERS['64']:
        ret = ret+[
            f"""_convertInstr (CALL_INDIRECT pos {reg.upper()} offset ann) = return $ Syntax.CALLINDIRECT pos offset (Syntax.{reg.upper()} pos) (_convert_annotation ann)""",
        ]

    # TUTAJ POZMIENIAC!!!
    # for instr in INSTR_ARITM_2OP:
    #     syms = syms + [f"{instr}"]
    #     ret = ret+[
    #         f"""
    #             {instr} :: a -> Size -> Loc -> Loc -> ASMGenerator a ()
    #         """
    #     ]
    #     for size in REGISTERS:
    #         ret = ret+[
    #             f"""{instr} pos Size{size} loc1 loc2 = Syntax.{instr.upper()}{size} pos (_locToSource pos size loc1) (_locToTarget pos size loc2)""",
    #         ]
    #     ret = ret+[
    #         f"""{instr} pos size _ _ = generatorFail $ EDataUnexpectedSize pos size"""
    #     ]
    # for instr in INSTR_ARITM_1OP:
    #     syms = syms + [f"{instr}"]
    #     ret = ret+[
    #         f"""
    #             {instr} :: a -> Size -> Loc -> ASMGenerator a ()
    #         """
    #     ]
    #     for size in REGISTERS:
    #         ret = ret+[
    #             f"""{instr} pos Size{size} loc = Syntax.{instr.upper()}{size} pos (_locToTarget pos size loc)""",
    #         ]
    #     ret = ret+[
    #         f"""{instr} pos size _ = generatorFail $ EDataUnexpectedSize pos size"""
    #     ]





    # for instr in INSTR_JMP:
    #     syms = syms + [f"{instr}"]
    #     ret = ret+[
    #         f"""
    #             {instr} :: a -> String -> ASMGenerator a ()
    #             {instr} pos label = do
    #                 {INDENT}_emitInstr pos $ _wrap pos $ Syntax.{instr.upper()} pos (Syntax.Label label)
    #         """
    #     ]
    # for instr in INSTR_ARITM_1OP:
    #     syms = syms + [f"{instr}"]
    #     ret = ret+[
    #         f"""
    #             {instr} :: ({cls_spec}) => a -> t -> ASMGenerator a ()
    #             {instr} pos arg1 = do
    #                 {INDENT}_emitInstr pos $ _wrap pos $ (_instr_{instr} (sizeOf arg1)) pos (asTarget pos arg1)
    #             {instr}' :: a -> Loc -> ASMGenerator a ()
    #         """
    #     ]
    #     for size in REGISTERS:
    #         ret = ret+[
    #             f"""{instr}' pos (LocReg{size} reg) = {instr} pos reg""",
    #             f"""{instr}' pos (LocMem{size} mem) = {instr} pos mem""",
    #         ]
    # for instr in INSTR_JMP:
    #     syms = syms + [f"{instr}"]
    #     ret = ret+[
    #         f"""
    #             {instr} :: a -> String -> ASMGenerator a ()
    #             {instr} pos label = do
    #                 {INDENT}_emitInstr pos $ _wrap pos $ Syntax.{instr.upper()} pos (Syntax.Label label)
    #         """
    #     ]
    # for instr in INSTR_STACK:
    #     syms = syms + [f"{instr}"]
    #     ret = ret+[
    #         f"""
    #             {instr} :: a -> Reg64 -> ASMGenerator a ()
    #             {instr} pos reg = do
    #                 {INDENT}_emitInstr pos $ _wrap pos $ Syntax.{instr.upper()} pos $ _reg64ToSReg pos reg
    #             {instr}' :: a -> Loc -> ASMGenerator a ()
    #             {instr}' pos (LocReg64 reg) = {instr} pos reg
    #             {instr}' pos loc@(LocMem64 _) =  generatorFail $ EDataUnexpectedLocation pos loc
    #             {instr}' pos loc = generatorFail $ EDataUnexpectedSize pos loc 64
    #         """
    #     ]
    # for instr in INSTR_NOARG:
    #     syms = syms + [f"{instr}"]
    #     ret = ret+[
    #         f"""
    #             {instr} :: a -> ASMGenerator a ()
    #             {instr} pos = do
    #                 {INDENT}_emitInstr pos $ _wrap pos $ Syntax.{instr.upper()} pos
    #         """
    #     ]
    # for instr in INSTR_SET:
    #     syms = syms + [f"{instr}"]
    #     ret = ret+[
    #         f"""
    #             {instr} :: a -> Reg8 -> ASMGenerator a ()
    #             {instr} pos reg = do
    #                 {INDENT}_emitInstr pos $ _wrap pos $ Syntax.{instr.upper()} pos $ _reg8ToSReg pos reg
    #             {instr}' :: a -> Loc -> ASMGenerator a ()
    #             {instr}' pos (LocReg8 reg) = {instr} pos reg
    #             {instr}' pos loc@(LocMem64 _) =  generatorFail $ EDataUnexpectedLocation pos loc
    #             {instr}' pos loc = generatorFail $ EDataUnexpectedSize pos loc 8
    #         """
    #     ]
    return syms, ret

def generate_size_classes():
    #class_args = [f"target{rsize}" for rsize in REGISTERS]
    #convert_methods = [f"to{rsize} :: target -> target{rsize}" for rsize in REGISTERS]
    ret = []
    syms = ["toBytes"]
    # for size in REGISTERS:
    #     ret = ret + [
    #         f"""
    #             data SIZE{size} = SIZE{size}
    #             {INDENT}deriving (Eq, Ord, Show, Read, Generic, Typeable)
    #         """
    #     ]
    ret = ret + [
        f"""
            data Size = {' | '.join(['Size'+size for size in REGISTERS])}
            {INDENT}deriving (Eq, Ord, Show, Read, Generic, Typeable)

            toBytes :: Size -> Int64
        """
    ]
    for size in REGISTERS:
        ret = ret + [f"""toBytes Size{size} = {int(size) // 8}"""]
    # ret = ret + [
    #     f"""
    #         class IsTarget target size {" ".join(class_args)} | target -> size {" ".join(class_args)} where
    #             {INDENT}{INDENTNL.join(convert_methods)}
    #             {INDENT}asSource :: a -> target -> Syntax.Source' a
    #             {INDENT}asTarget :: a -> target -> Syntax.Target' a
    #             {INDENT}sizeOf :: target -> Int
    #     """
    # ]
    # for size in REGISTERS:
    #     rest_sizes = [nsize for nsize in REGISTERS if nsize != size]
    #     instance_args = [f"Reg{rsize}" for rsize in REGISTERS]
    #     instance_args_mem = [f"(Reg{rsize}, Integer)" for rsize in REGISTERS]
    #     instance_args_const = [f"(Integer, SIZE{rsize})" for rsize in REGISTERS]
    #     # self_convert = f"""
    #     #     {INDENT}size{size}to{size} :: target -> target
    #     #     {INDENT}default size{size}to{size} :: target -> target
    #     #     {INDENT}size{size}to{size} = id
    #     # """
    #     reg_conversions = [
    #         f"""_reg{size}ToSReg :: a -> Reg{size} -> Syntax.Reg' a"""
    #     ]
    #     for reg in REGISTERS[size]:
    #         reg_conversions = reg_conversions + [
    #             f"""_reg{size}ToSReg pos {reg.upper()} = Syntax.{reg.upper()} pos"""
    #         ]

    #     instance_convert_methods = []
    #     for rsize in REGISTERS:
    #         for (index, rreg) in enumerate(REGISTERS[rsize]):
    #             original_reg = REGISTERS[size][index]
    #             instance_convert_methods = instance_convert_methods + [
    #                 f"to{rsize} {original_reg.upper()} = {rreg.upper()}"
    #             ]
    #     source_target_convert_methods = []
    #     for reg in REGISTERS[size]:
    #         source_target_convert_methods = source_target_convert_methods + [
    #             f"asSource pos {reg.upper()} = Syntax.FromReg{size} pos $ Syntax.{reg.upper()} pos "
    #         ]
    #     for reg in REGISTERS[size]:
    #         source_target_convert_methods = source_target_convert_methods + [
    #             f"asTarget pos {reg.upper()} = Syntax.ToReg{size} pos $ Syntax.{reg.upper()} pos "
    #         ]
    #     ret = ret + reg_conversions + [
    #         f"""
    #         instance IsTarget Reg{size} SIZE{size} {" ".join(instance_args)} where
    #             {INDENT}{INDENTNL.join(instance_convert_methods)}
    #             {INDENT}{INDENTNL.join(source_target_convert_methods)}
    #             {INDENT}sizeOf _ = {size}
    #         """,
    #         f"""
    #         instance IsTarget (Reg{size}, Integer) SIZE{size} {" ".join(instance_args_mem)} where
    #             {INDENT}{INDENTNL.join(['to'+rsize+' (r, offset) = (to'+rsize+' r, offset)' for rsize in REGISTERS])}
    #             {INDENT}asSource pos (reg, offset) = let (Syntax.FromReg{size} _ reg') = asSource pos reg in Syntax.FromMem{size} pos offset reg'
    #             {INDENT}asTarget pos (reg, offset) = let (Syntax.ToReg{size} _ reg') = asTarget pos reg in Syntax.ToMem{size} pos offset reg'
    #             {INDENT}sizeOf _ = {size}
    #         """,
    #         f"""
    #         instance IsTarget (Integer, SIZE{size}) SIZE{size} {" ".join(instance_args_const)} where
    #             {INDENT}{INDENTNL.join(['to'+rsize+' (c, _) = (c, SIZE'+rsize+')' for rsize in REGISTERS])}
    #             {INDENT}asSource pos (c, _) = Syntax.FromConst pos c
    #             {INDENT}asTarget pos (c, _) = undefined
    #             {INDENT}sizeOf _ = {size}
    #         """,
    #     ]
    return syms, "\n".join(ret)

def generate_utils(module, module_name, syntax_module, syntax_postfix, output_haskell_path):
    newline="\n"
    langs = [
        "DeriveTraversable",
        "TemplateHaskell",
        "DeriveGeneric",
        "DefaultSignatures",
        "DeriveDataTypeable",
        "FlexibleInstances",
    ]
    directives = ["{-# LANGUAGE "+lang+" #-}" for lang in langs]
    syms1, data_types_code = create_data_types()
    syms2, instr_wrappers_code = create_instr_wrappers()
    syms3, size_classes_code = generate_size_classes()
    instr_wrappers_code = newline.join(instr_wrappers_code)
    
    exports = [
        "runASMGeneratorT",
        "execASMGeneratorT",
        "continueASMGeneratorT",
        "ASMGenerator",
        "ASMGeneratorT",
        "dataDef",
        "DataDef(..)",
        "Data(..)",
        "Loc(..)",
        "Size(..)",
    ] + [sym for sym in (syms1 + syms2 + syms3)]

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

    import qualified Data.Text as T

    import Prelude hiding (and)
    import Data.Int

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
    convertDataDef pos (DataGlobal label) = Syntax.AsmDataGlobal pos $ Syntax.Label $ sanitizeLabel label
    convertDataDef pos (DataDef label datas) = Syntax.AsmDataDef pos (Syntax.Label $ sanitizeLabel label) (map (convertData pos) datas)
        {INDENT}where
            {INDENT}{INDENT}convertData :: a -> Data -> Syntax.Data' a
            {INDENT}{INDENT}convertData pos (DataStr str) = Syntax.DataString pos str
            {INDENT}{INDENT}convertData pos (Data64I val) = Syntax.Data64 pos $ Syntax.ConstInt pos $ val
            {INDENT}{INDENT}convertData pos (Data32I val) = Syntax.Data32 pos $ Syntax.ConstInt pos $ val
            {INDENT}{INDENT}convertData pos (Data64From label) = Syntax.Data64 pos $ Syntax.ConstLabel pos $ Syntax.Label $ sanitizeLabel label
            {INDENT}{INDENT}convertData pos (Data32From label) = Syntax.Data32 pos $ Syntax.ConstLabel pos $ Syntax.Label $ sanitizeLabel label


    -- Error definition
    data GeneratorError a = 
    {INDENT}EDataMemToMemTransfer a Loc Loc
    {INDENT}| EDataUnexpectedSize a Size
    {INDENT}| EDataUnexpectedLocation a Loc
    {INDENT}| ENoOutputCodeGenerated
    {INDENT}| ENonRegisterLocationGiven a Loc
    {INDENT}deriving (Eq, Ord, Show, Read, Generic, Typeable)

    generatorFail :: (Monad m) => GeneratorError a -> ASMGeneratorT a anno m v
    generatorFail e = throwError e

    -- Assembly generator definition
    type ASMGenerator a anno v = (WriterT (GeneratorOut a anno) (Except (GeneratorError a))) v
    type ASMGeneratorT a anno (m :: * -> *) = (WriterT (GeneratorOut a anno) (ExceptT (GeneratorError a) m))

    data GeneratorOut a anno = GeneratorOut (Maybe a) [Instr a anno] [Syntax.AsmDataDef' a]

    instance Semigroup (GeneratorOut a anno) where
        {INDENT}(<>) (GeneratorOut Nothing instr1 defs1) (GeneratorOut (Just firstPos2) instr2 defs2) = GeneratorOut (Just firstPos2) (instr1 <> instr2) (defs1 <> defs2)
        {INDENT}(<>) (GeneratorOut (Just firstPos1) instr1 defs1) (GeneratorOut Nothing instr2 defs2) = GeneratorOut (Just firstPos1) (instr1 <> instr2) (defs1 <> defs2)
        {INDENT}(<>) (GeneratorOut firstPos1 instr1 defs1) (GeneratorOut _ instr2 defs2) = GeneratorOut firstPos1 (instr1 <> instr2) (defs1 <> defs2)

    instance Monoid (GeneratorOut a anno) where
        {INDENT}mempty = GeneratorOut Nothing [] []

    _emitInstr :: (Monad m) => a -> Instr a anno -> ASMGeneratorT a anno m ()
    _emitInstr pos instr = tell $ GeneratorOut (Just pos) [instr] []

    _emitDef :: (Monad m) => a -> Syntax.AsmDataDef' a -> ASMGeneratorT a anno m ()
    _emitDef pos def = tell $ GeneratorOut (Just pos) [] [def]

    continueASMGeneratorT :: (Monad m) => GeneratorOut a anno -> ASMGeneratorT a anno m ()
    continueASMGeneratorT out = tell out

    execASMGeneratorT :: (Monad m) => (ASMGeneratorT a anno m v) -> m (Either (GeneratorError a) (v, GeneratorOut a anno))
    execASMGeneratorT generator = do
    {INDENT}genResult <- runExceptT (runWriterT $ generator)
    {INDENT}return genResult

    runASMGeneratorT :: (Monad m, CommentProvider a anno) => (ASMGeneratorT a anno m v) -> [String] -> m (Either (GeneratorError a) (String, v))
    runASMGeneratorT generator externs = do
    {INDENT}genResult <- runExceptT (runWriterT $ generator)
    {INDENT}case genResult of 
    {INDENT}{INDENT}Left err -> return $ Left err
    {INDENT}{INDENT}Right (result, _) -> do
    {INDENT}{INDENT}{INDENT}outResult <- runExceptT (runWriterT $ generateOut externs genResult)
    {INDENT}{INDENT}{INDENT}case outResult  of
    {INDENT}{INDENT}{INDENT}{INDENT}Left err -> return $ Left err
    {INDENT}{INDENT}{INDENT}{INDENT}Right ((_, fullProg), _) -> do
    {INDENT}{INDENT}{INDENT}{INDENT}{INDENT}let assemblyCodeStr = Printer.printTree fullProg
    {INDENT}{INDENT}{INDENT}{INDENT}{INDENT}let codeLines = map (T.strip) $ T.lines $ T.replace "<ENDL>" "\\n" $ T.pack assemblyCodeStr
    {INDENT}{INDENT}{INDENT}{INDENT}{INDENT}let formattedCode = alignASMCommentsText codeLines
    {INDENT}{INDENT}{INDENT}{INDENT}{INDENT}return $ Right (formattedCode, result)
    {INDENT}where
    {INDENT}{INDENT}generateOut :: (Monad m, CommentProvider a anno) => [String] -> Either (GeneratorError a) (v, GeneratorOut a anno) -> (ASMGeneratorT a anno m (v, Syntax.AsmProgram' a))
    {INDENT}{INDENT}generateOut externs r =
    {INDENT}{INDENT}{INDENT}case r of
    {INDENT}{INDENT}{INDENT}{INDENT}Left err -> generatorFail err
    {INDENT}{INDENT}{INDENT}{INDENT}Right (_, GeneratorOut Nothing _ _) -> generatorFail ENoOutputCodeGenerated
    {INDENT}{INDENT}{INDENT}{INDENT}Right (result, GeneratorOut (Just pos) instrs defs) -> do
    {INDENT}{INDENT}{INDENT}{INDENT}{INDENT}instrs' <- mapM _convertInstr instrs
    {INDENT}{INDENT}{INDENT}{INDENT}{INDENT}let topDirectives = map (Syntax.Extern pos . Syntax.Label . sanitizeLabel) externs
    {INDENT}{INDENT}{INDENT}{INDENT}{INDENT}let fullProg = Syntax.AsmProgram pos topDirectives (Syntax.SectionData pos defs) (Syntax.SectionCode pos instrs')
    {INDENT}{INDENT}{INDENT}{INDENT}{INDENT}return (result, fullProg)
    {INDENT}{INDENT}alignASMCommentsText :: [T.Text] -> String
    {INDENT}{INDENT}alignASMCommentsText fileContents =
    {INDENT}{INDENT}{INDENT}let codeLines = map (\line -> let chunks = T.splitOn (T.pack "#") line in if null chunks then (T.pack "", T.pack "") else (head chunks, T.intercalate "#" $ tail chunks)) $ fileContents in
    {INDENT}{INDENT}{INDENT}let codeWidth = maximum $ map (\(code, _) -> T.length code) codeLines in
    {INDENT}{INDENT}{INDENT}let formattedCodeLines = map (\(code, comment) -> let c' = paddingTo code codeWidth in if T.null comment then code else c' <> (T.pack " #") <> comment) codeLines in
    {INDENT}{INDENT}{INDENT}T.unpack $ T.unlines formattedCodeLines
    {INDENT}{INDENT}paddingTo :: T.Text -> Int -> T.Text
    {INDENT}{INDENT}paddingTo line width = line <> (T.pack (concat $ replicate (width - (T.length line)) " "))


    -- Registers
    {data_types_code}

    -- Size classes
    {size_classes_code}

    -- Instruction wrappers
    {instr_wrappers_code}

    -- Data wrappers
    dataDef :: (Monad m) => a -> DataDef -> ASMGeneratorT a anno m ()
    dataDef pos def = _emitDef pos $ convertDataDef pos def
    """
    with open(output_haskell_path, "w") as f:
            f.write("\n".join([line.strip().replace(INDENT, "\t") for line in code.splitlines()]))


if __name__ == "__main__":
    generate_utils(sys.argv[1], sys.argv[2], sys.argv[3], sys.argv[4], sys.argv[5])