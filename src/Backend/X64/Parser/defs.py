ARG_REGISTERS = [
    "RDI", "RSI", "RDX", "RCX", "R8", "R9"
]

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

INSTR_JMP = [
    "jmp",
    "jz",
]

INSTR_ARITM_1OP = [
    "neg",
    "idiv",
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
    "sete",
    "setg",
    "setge",
    "setl",
    "setle",
    "setne",
]

INSTR_ALL = INSTR_ARITM_2OP + INSTR_JMP + INSTR_ARITM_1OP + INSTR_STACK + INSTR_NOARG + INSTR_SET
