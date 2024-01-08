%include 'lib/runtime.ext'
section .rodata ; "./tests/bad/bad013.lat": 1,1
global _class_Array
_class_Array:
DQ _class_Object
DD 4
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
_S4: ; "./tests/bad/bad013.lat": 3,11
DB "pi" ; "./tests/bad/bad013.lat": 3,11
DB 0
section .text ; "./tests/bad/bad013.lat": 1,1
global main ; "./tests/bad/bad013.lat": 1,1
main: ; "./tests/bad/bad013.lat": 1,5
PUSH RBP ; "./tests/bad/bad013.lat": 1,1
PUSH RBX ; "./tests/bad/bad013.lat": 1,1
PUSH R12 ; "./tests/bad/bad013.lat": 1,1
PUSH R13 ; "./tests/bad/bad013.lat": 1,1
MOV RBP, RSP ; "./tests/bad/bad013.lat": 1,1
SUB RSP, 8 ; "./tests/bad/bad013.lat": 1,1
SUB RSP, 8
PUSH R10
MOV RDI, 1
CALL byteToString
MOV RBX, RAX
POP R10
ADD RSP, 8
MOV R10, RBX
PUSH R11
PUSH R10
MOV RDI, _S4
CALL __createString
MOV RBX, RAX
POP R10
POP R11
MOV R11, RBX
MOV R13, R11
TEST R13, R13
JZ $+12
MOV EBX, [R13 + 16]
INC EBX
MOV [R13 + 16], EBX
MOV RBX, R11
TEST RBX, RBX ; "./tests/bad/bad013.lat": 3,11
JNZ $+7 ; "./tests/bad/bad013.lat": 3,11
CALL __errorNull ; "./tests/bad/bad013.lat": 3,11
MOV R12, [RBX] ; "./tests/bad/bad013.lat": 3,11
MOV R12, [R12 + 12] ; "./tests/bad/bad013.lat": 3,11
MOV R12, [R12 + 16] ; "./tests/bad/bad013.lat": 3,11
SUB RSP, 8
PUSH R11
PUSH R10
PUSH R9
MOV RDI, R11
MOV RSI, R10
CALL R12
MOV RBX, RAX
POP R9
POP R10
POP R11
ADD RSP, 8
MOV R9, RBX
PUSH R11
PUSH R10
MOV RDI, R9
CALL __decRef
MOV RBX, RAX
POP R10
POP R11
SUB RSP, 8
PUSH R11
MOV RDI, R10
CALL __decRef
MOV RBX, RAX
POP R11
ADD RSP, 8
MOV RDI, R11
CALL __decRef
MOV RBX, RAX
MOV EAX, 0 ; "./tests/bad/bad013.lat": 4,7
MOV RSP, RBP ; "./tests/bad/bad013.lat": 4,7
POP R13 ; "./tests/bad/bad013.lat": 4,7
POP R12 ; "./tests/bad/bad013.lat": 4,7
POP RBX ; "./tests/bad/bad013.lat": 4,7
POP RBP ; "./tests/bad/bad013.lat": 4,7
RET ; "./tests/bad/bad013.lat": 4,7
