REGISTERS = {
    '64': ["RAX", "RBX", "RCX", "RDX", "RDI", "RSI", "RSP", "RBP"] + [f"R{i}"  for i in range(8, 16)],
    '32': ["EAX", "EBX", "ECX", "EDX", "EDI", "ESI", "ESP", "EBP"] + [f"R{i}D" for i in range(8, 16)],
    '16': ["AX",  "BX",  "CX",  "DX",  "DI",  "SI",  "SP",  "BP" ] + [f"R{i}W" for i in range(8, 16)],
    '8': ["AL",  "BL",  "CL",  "DL",  "DIL", "SIL", "SPL", "BPL"] + [f"R{i}B" for i in range(8, 16)],
}

INSTR_ARITM_2OP = [
    "add",
    "and",
    "cmp",
    "idiv",
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
