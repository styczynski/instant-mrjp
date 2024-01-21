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
main :                         #-- ./booltest.lat:1:1 --#
__cl_TopLevel.main :           #-- ./booltest.lat:1:1 --#
push %RBX                      #-- ./booltest.lat:1:1 --#
subq $8, %RSP                  #-- ./booltest.lat:1:1 --#
push %RBP                      #-- ./booltest.lat:1:1 --#
movq %RSP, %RBP                #-- ./booltest.lat:1:1 --#
subq $0, %RSP                  #-- ./booltest.lat:1:1 --#
__cl_TopLevel.main.L_entry :   #-- ./booltest.lat:1:1 --#
movl $0, %ECX                  #-- setting %v_t_1~2 at ./booltest.lat:3:5 --#
addq $0, %RSP                  #-- ./booltest.lat:3:5 --#
jmp __cl_TopLevel.main_WCOND2  #-- ./booltest.lat:3:5 --#
__cl_TopLevel.main_CAND8 :     #-- ./booltest.lat:3:13 --#
cmpl $8, %ECX                  #-- ./booltest.lat:3:43 --#
setge %AL                      #-- ./booltest.lat:3:44 --#
addq $0, %RSP                  #-- ./booltest.lat:3:43 --#
testb %AL, %AL                 #-- ./booltest.lat:3:43 --#
jz __cl_TopLevel.main_WBEG3    #-- ./booltest.lat:3:43 --#
jmp __cl_TopLevel.main_WEND4   #-- ./booltest.lat:3:43 --#
__cl_TopLevel.main_COR9 :      #-- ./booltest.lat:3:14 --#
movl %ECX, %EAX                #-- ./booltest.lat:3:30 --#
cdq                            #-- ./booltest.lat:3:30 --#
push %RCX                      #-- ./booltest.lat:3:30 --#
movl $5, %ECX                  #-- ./booltest.lat:3:30 --#
idivl %ECX                     #-- ./booltest.lat:3:30 --#
pop %RCX                       #-- ./booltest.lat:3:30 --#
movl %EDX, %EDX                #-- ./booltest.lat:3:30 --#
cmpl $1, %EDX                  #-- ./booltest.lat:3:28 --#
sete %AL                       #-- ./booltest.lat:3:34 --#
addq $0, %RSP                  #-- ./booltest.lat:3:28 --#
testb %AL, %AL                 #-- ./booltest.lat:3:28 --#
jz __cl_TopLevel.main_WBEG3    #-- ./booltest.lat:3:28 --#
jmp __cl_TopLevel.main_CAND8   #-- ./booltest.lat:3:28 --#
__cl_TopLevel.main_WBEG3 :     #-- ./booltest.lat:3:5 --#
leal 1 (%RCX), %EBX            #--  addition %v_t_6 at ./booltest.lat:4:14 --#
movl %EBX, %EDI                #-- passing arg at ./booltest.lat:5:9 --#
subq $0, %RSP                  #-- ./booltest.lat:5:9 --#
call printInt                  #-- ./booltest.lat:5:9 --#
addq $0, %RSP                  #-- ./booltest.lat:5:9 --#
xchgl %ECX, %EBX               #-- ./booltest.lat:3:5 --#
addq $0, %RSP                  #-- ./booltest.lat:3:5 --#
jmp __cl_TopLevel.main_WCOND2  #-- ./booltest.lat:3:5 --#
__cl_TopLevel.main_WCOND2 :    #-- ./booltest.lat:3:5 --#
movl %ECX, %EAX                #-- ./booltest.lat:3:16 --#
cdq                            #-- ./booltest.lat:3:16 --#
push %RCX                      #-- ./booltest.lat:3:16 --#
movl $5, %ECX                  #-- ./booltest.lat:3:16 --#
idivl %ECX                     #-- ./booltest.lat:3:16 --#
pop %RCX                       #-- ./booltest.lat:3:16 --#
movl %EDX, %EDX                #-- ./booltest.lat:3:16 --#
cmpl $0, %EDX                  #-- ./booltest.lat:3:14 --#
sete %AL                       #-- ./booltest.lat:3:20 --#
addq $0, %RSP                  #-- ./booltest.lat:3:14 --#
testb %AL, %AL                 #-- ./booltest.lat:3:14 --#
jz __cl_TopLevel.main_COR9     #-- ./booltest.lat:3:14 --#
jmp __cl_TopLevel.main_CAND8   #-- ./booltest.lat:3:14 --#
__cl_TopLevel.main_WEND4 :     #-- ./booltest.lat:3:5 --#
movl %ECX, %EDI                #-- passing arg at ./booltest.lat:7:5 --#
subq $0, %RSP                  #-- ./booltest.lat:7:5 --#
call printInt                  #-- ./booltest.lat:7:5 --#
addq $0, %RSP                  #-- ./booltest.lat:7:5 --#
movl $0, %EAX                  #-- move return value at ./booltest.lat:1:1 --#
addq $0, %RSP                  #-- ./booltest.lat:1:1 --#
leave                          #-- ./booltest.lat:1:1 --#
addq $8, %RSP                  #-- ./booltest.lat:1:1 --#
pop %RBX                       #-- ./booltest.lat:1:1 --#
ret                            #-- ./booltest.lat:1:1 --#
__errorNull :                  #-- runtime error on null dereference at ./booltest.lat:1:1 --#
andq $-16, %RSP                #-- 16 bytes allign at ./booltest.lat:1:1 --#
call __errorNull               #-- ./booltest.lat:1:1 --#