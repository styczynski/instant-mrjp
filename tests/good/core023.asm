;  Generated by Latte compiler latc_x86
;  Source file: core023.asm
%include 'lib/runtime.ext'
section .rodata          ; "./tests/good/core023.lat": 1,1
global _class_Array
_class_Array:
DQ _class_Object
DD 16
DQ _class_Array_methods
DD 1
DQ _class_Array_refs
_class_Array_methods:
DQ _Object_equals
DQ _Object_getHashCode
DQ _Array_toString
_class_Array_refs:
DD 0
global _class_Object
_class_Object:
DQ 0
DD 0
DQ _class_Object_methods
DD 0
DQ 0
_class_Object_methods:
DQ _Object_equals
DQ _Object_getHashCode
DQ _Object_toString
global _class_String
_class_String:
DQ _class_Object
DD 0
DQ _class_String_methods
DD 0
DQ 0
_class_String_methods:
DQ _String_charAt
DQ _String_equals
DQ _String_concat
DQ _String_startsWith
DQ _String_endsWith
DQ _String_getBytes
DQ _String_indexOf
DQ _String_length
DQ _String_substring
DQ _String_toString
DQ _String_getHashCode
section .text            ; "./tests/good/core023.lat": 1,1
global main              ; "./tests/good/core023.lat": 1,1
main:                    ; "./tests/good/core023.lat": 1,5
PUSH RBP                 ; "./tests/good/core023.lat": 1,1
PUSH RBX
PUSH R12
PUSH R13
MOV RBP, RSP
SUB RSP, 8
SUB RSP, 8
PUSH R11
MOV RDI, 1
MOV RSI, 2
MOV RDX, 1
MOV RCX, 2
MOV R8, 1
MOV R9, 2
MOV RBX, RSP
PUSH 2
PUSH 1
PUSH 2
PUSH 1
PUSH 2
PUSH 1
PUSH 2
PUSH 1
CALL foo
MOV RSP, RBX
MOV RBX, RAX
POP R11
ADD RSP, 8
MOV R11, RBX
MOV EAX, R11D
MOV RSP, RBP             ; "./tests/good/core023.lat": 3,3
POP R13
POP R12
POP RBX
POP RBP
RET
global foo               ; "./tests/good/core023.lat": 7,1
foo:                     ; "./tests/good/core023.lat": 7,5
PUSH RBP                 ; "./tests/good/core023.lat": 7,1
PUSH RBX
MOV RBP, RSP
SUB RSP, 24
MOV R11D, [RBP + 24]
MOV R10D, [RBP + 32]
MOV EAX, [RBP + 40]
ADD EDX, ECX             ; "./tests/good/core023.lat": 10,43
ADD R8D, EDX
ADD R9D, R8D
ADD R11D, R9D
ADD R11D, R10D
ADD R11D, EAX
ADD R11D, [RBP + 56]
ADD R11D, [RBP + 64]
ADD R11D, [RBP + 72]
ADD R11D, [RBP + 80]
MOV R10D, EDI            ; "./tests/good/core023.lat": 10,13
IMUL R10D, 2
ADD R10D, R11D           ; "./tests/good/core023.lat": 10,43
MOV EAX, ESI
CDQ
MOV EBX, 2               ; "./tests/good/core023.lat": 10,17
IDIV EBX
MOV R11D, EAX
ADD R11D, R10D           ; "./tests/good/core023.lat": 10,43
MOV EAX, [RBP + 48]
CDQ
MOV EBX, 2               ; "./tests/good/core023.lat": 10,35
IDIV EBX
MOV R10D, EAX
ADD R11D, R10D           ; "./tests/good/core023.lat": 10,43
MOV EAX, R11D
CDQ
MOV EBX, 10              ; "./tests/good/core023.lat": 10,46
IDIV EBX
MOV R11D, EDX
MOV [RBP - 16], R11D     ; "./tests/good/core023.lat": 10,7
MOV RDI, [RBP - 16]
CALL printInt
MOV RBX, RAX
MOV EAX, [RBP - 16]      ; "./tests/good/core023.lat": 12,3
MOV RSP, RBP
POP RBX
POP RBP
RET