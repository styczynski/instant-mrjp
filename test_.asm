;  Generated by Latte compiler latc_x86
;  Source file: test.asm
%include 'lib/runtime.ext'
section .rodata          ; "./test.lat": 1,1
global _class_A          ; "./test.lat": 1,7
_class_A:
DQ _class_Object
DD 4
DQ _class_A_methods
DD 0
DQ 0
_class_A_methods:
DQ _Object_equals
DQ _Object_getHashCode
DQ _Object_toString
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
section .text            ; "./test.lat": 1,1
global main              ; "./test.lat": 5,1
main:                    ; "./test.lat": 5,5
PUSH RBP                 ; "./test.lat": 5,1
PUSH RBX
MOV RBP, RSP
SUB RSP, 8
MOV RDI, 2
CALL printInt
MOV RBX, RAX
MOV EAX, 0               ; "./test.lat": 9,3
MOV RSP, RBP
POP RBX
POP RBP
RET