.extern _Array_toString
.extern _Object_toString
.extern _Object_getHashCode
.extern _Object_equals
.extern _String_equals
.extern _String_getHashCode
.extern _String_toString
.extern _String_substring
.extern _String_length
.extern _String_indexOf
.extern _String_getBytes
.extern _String_endsWith
.extern _String_startsWith
.extern _String_concat
.extern _String_charAt
.extern printString
.extern printInt
.extern printByte
.extern printBoolean
.extern printBinArray
.extern byteToString
.extern boolToString
.extern intToString
.extern print
.extern error
.extern readInt
.extern readString
.extern __cast

.section .rodata
__const_1 :
.string "!!!"

.global _class_A
_class_A :
.quad _class_Object
.long 8
.quad _class_A_methods
.long 0
.quad 0

.global _class_A_methods
_class_A_methods :
.quad _Object_equals
.quad _Object_getHashCode
.quad _Object_toString
.quad A.bar

.global _class_Array
_class_Array :
.quad _class_Object
.long 16
.quad _class_Array_methods
.long 0
.quad 0

.global _class_Array_methods
_class_Array_methods :
.quad _Object_equals
.quad _Object_getHashCode
.quad _Array_toString

.global _class_B
_class_B :
.quad _class_A
.long 8
.quad _class_B_methods
.long 0
.quad 0

.global _class_B_methods
_class_B_methods :
.quad _Object_equals
.quad _Object_getHashCode
.quad _Object_toString
.quad B.bar

.global _class_Object
_class_Object :
.quad 0
.long 0
.quad _class_Object_methods
.long 0
.quad 0

.global _class_Object_methods
_class_Object_methods :
.quad _Object_equals
.quad _Object_getHashCode
.quad _Object_toString

.global _class_String
_class_String :
.quad _class_Object
.long 0
.quad _class_String_methods
.long 0
.quad 0

.global _class_String_methods
_class_String_methods :
.quad _String_charAt
.quad _String_equals
.quad _String_concat
.quad _String_startsWith
.quad _String_endsWith
.quad _String_getBytes
.quad _String_indexOf
.quad _String_length
.quad _String_substring
.quad _String_toString
.quad _String_getHashCode

.global main

.section .text
main :
__cl_TopLevel.main :
push %R12
push %RBX
push %RBP
movq %RSP, %RBP
subq $0, %RSP
__cl_TopLevel.main.L_entry :
leaq _class_A (%RIP), %RDI
subq $0, %RSP
call __new
addq $0, %RSP
movq %RAX, %R12
leaq _class_B (%RIP), %RDI
subq $0, %RSP
call __new
addq $0, %RSP
movq %RAX, %RAX
movq %RAX, %RDI               #-- passing arg --#
leaq _class_A (%RIP), %RSI
subq $0, %RSP
call __cast
addq $0, %RSP
movq %RAX, %RBX
movq 8 (%R12), %RAX           #-- load data (indirect) --#
movl $42, 0 (%RAX)
movq 8 (%RBX), %RAX           #-- load data (indirect) --#
movl $42, 0 (%RAX)
movq %R12, %RDI               #-- passing arg --#
movl $15, %ESI                #-- passing arg --#
subq $0, %RSP
testq %RDI, %RDI
jz __errorNull
movq 20 (%RDI), %RAX          #-- load address of vtable --#
call * 24 (%RAX)              #-- call bar --#
addq $0, %RSP
movl %EAX, %EAX
movl %EAX, %EDI               #-- passing arg --#
subq $0, %RSP
call printInt
addq $0, %RSP
movq %RBX, %RDI               #-- passing arg --#
movl $15, %ESI                #-- passing arg --#
subq $0, %RSP
testq %RDI, %RDI
jz __errorNull
movq 20 (%RDI), %RAX          #-- load address of vtable --#
call * 24 (%RAX)              #-- call bar --#
addq $0, %RSP
movl %EAX, %EAX
movl %EAX, %EDI               #-- passing arg --#
subq $0, %RSP
call printInt
addq $0, %RSP
movl $0, %EAX                 #-- move return value --#
addq $0, %RSP
leave
pop %R12
pop %RBX
ret
__cl_TopLevel.bar :
push %RBP
movq %RSP, %RBP
subq $0, %RSP
__cl_TopLevel.bar.L_entry :
movl %EDI, %EAX               #-- load %v_t_19 --#
movl %EAX, %EAX
sall $1, %EAX                 #-- multiply by 2 --#
movl %EAX, %EAX               #-- move return value --#
addq $0, %RSP
leave
ret
__cl_TopLevel.foo :
push %RBP
movq %RSP, %RBP
subq $0, %RSP
__cl_TopLevel.foo.L_entry :
movq %RDI, %RCX               #-- load %v_t_15 --#
leaq __const_1 (%RIP), %RAX
push %RCX                     #-- save caller saved --#
movq %RAX, %RDI               #-- passing arg --#
subq $0, %RSP
call __createString
addq $0, %RSP
movq %RAX, %RAX
pop %RCX
movq %RCX, %RDI               #-- passing arg --#
movq %RAX, %RSI               #-- passing arg --#
subq $0, %RSP
testq %RDI, %RDI
jz __errorNull
movq 20 (%RDI), %RAX          #-- load address of vtable --#
call * 16 (%RAX)              #-- call concat --#
addq $0, %RSP
movq %RAX, %RAX
movq %RAX, %RAX               #-- move return value --#
addq $0, %RSP
leave
ret
B.bar :
push %R12
push %RBX
push %RBP
movq %RSP, %RBP
subq $0, %RSP
B.bar.L_entry :
movq %RDI, %RBX               #-- load %v_t_4 --#
movl %ESI, %R12D              #-- load %v_t_5 --#
movq 8 (%RBX), %RAX           #-- load data (indirect) --#
movl 0 (%RAX), %EAX           #-- load %v_t_6 --#
movl %EAX, %EDI               #-- passing arg --#
subq $0, %RSP
call printInt
addq $0, %RSP
movl %R12D, %EDI              #-- passing arg --#
subq $0, %RSP
call printInt
addq $0, %RSP
leal 2 (%R12), %ECX           #--  addition %v_t_10 --#
movq 8 (%RBX), %RAX           #-- load data (indirect) --#
movl 0 (%RAX), %EAX           #-- load %v_t_12 --#
movl %EAX, %EAX
sall $1, %EAX                 #-- multiply by 2 --#
addl %ECX, %EAX
movl %EAX, %EAX               #-- move return value --#
addq $0, %RSP
leave
pop %R12
pop %RBX
ret
A.bar :
push %RBP
movq %RSP, %RBP
subq $0, %RSP
A.bar.L_entry :
movl %ESI, %EAX               #-- load %v_t_1 --#
movl %EAX, %EAX
sall $1, %EAX                 #-- multiply by 2 --#
movl %EAX, %EAX               #-- move return value --#
addq $0, %RSP
leave
ret
__errorNull :                 #-- runtime error on null dereference --#
andq $-16, %RSP               #-- 16 bytes allign --#
call __errorNull
