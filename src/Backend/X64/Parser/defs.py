ARG_REGISTERS = [
    "RDI", "RSI", "RDX", "RCX", "R8", "R9"
]

REGISTERS_ALLOCABLE = {
    "RAX": True,
    "RDX": True,
    "RBX": True,
    "RCX": True,
    "RSI": True,
    "RDI": True,
    "RSP": False,
    "RBP": False,
    "R8": True,
    "R9": True,
    "R10": True,
    "R11": True,
    "R12": True,
    "R13": True,
    "R14": True,
    "R15": True,
}

REGISTERS = {
    '64': ["RAX", "RBX", "RCX", "RDX", "RDI", "RSI", "RSP", "RBP"] + [f"R{i}"  for i in range(8, 16)],
    '32': ["EAX", "EBX", "ECX", "EDX", "EDI", "ESI", "ESP", "EBP"] + [f"R{i}D" for i in range(8, 16)],
    '16': ["AX",  "BX",  "CX",  "DX",  "DI",  "SI",  "SP",  "BP" ] + [f"R{i}W" for i in range(8, 16)],
    '8': ["AL",  "BL",  "CL",  "DL",  "DIL", "SIL", "SPL", "BPL"] + [f"R{i}B" for i in range(8, 16)],
}

REGISTERS_TYPES = {
    "RAX": "CallerSaved",
    "RDX": "CallerSaved",
    "RBX": "CalleeSaved",
    "RCX": "CallerSaved",
    "RSI": "CallerSaved",
    "RDI": "CallerSaved",
    "RSP": "CallerSaved",
    "RBP": "CalleeSaved",
    "R8": "CallerSaved",
    "R9": "CallerSaved",
    "R10": "CallerSaved",
    "R11": "CallerSaved",
    "R12": "CalleeSaved",
    "R13": "CalleeSaved",
    "R14": "CalleeSaved",
    "R15": "CalleeSaved",
}

INSTR_ARITM_2OP = [
    "add",
    "and",
    "cmp",
    "imul",
    "lea",
    "mov",
    "sub",
    "test",
    "xor",
    "xchg",
    "sal",
    "sar",
]

INSTR_JMP_ORD = [
    "j",
]

INSTR_JMP = [
    "jmp",
]

INSTR_ARITM_1OP = [
    "neg",
    "idiv",
    "inc",
    "dec",
]

INSTR_STACK = [
    "pop",
    "push",
]

INSTR_NOARG = [
    "leave",
    "ret",
    "cdq",
]

INSTR_SET = [
    "set",
]

ORDS = {
    "e": "ne",
    "g": "le",
    "ge": "l",
    "l": "ge",
    "le": "g",
    "ne": "e",
    "z": "nz",
    "nz": "z",
}

ORDS_REPR = {
    "e": "==",
    "g": ">",
    "ge": ">=",
    "l": "<",
    "le": "<=",
    "ne": "/=",
    "z": "==",
    "nz": "/=",
}

INSTR_ALL = INSTR_ARITM_2OP + INSTR_JMP + INSTR_ARITM_1OP + INSTR_STACK + INSTR_NOARG + INSTR_SET
