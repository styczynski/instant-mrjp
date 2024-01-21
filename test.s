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
__cl_TopLevel.f :             #-- ./test.lat:7:1 --#
push %RBP                     #-- ./test.lat:7:1 --#
movq %RSP, %RBP               #-- ./test.lat:7:1 --#
subq $0, %RSP                 #-- ./test.lat:7:1 --#
__cl_TopLevel.f.L_entry :     #-- ./test.lat:7:1 --#
movl %EDI, %EAX               #-- load %v_t_3 at ./test.lat:7:1 --#
movl %ESI, %ECX               #-- load %v_t_4 at ./test.lat:7:1 --#
cmpl $0, %EAX                 #-- ./test.lat:8:7 --#
setg %DL                      #-- ./test.lat:8:8 --#
addq $0, %RSP                 #-- ./test.lat:8:7 --#
testb %DL, %DL                #-- ./test.lat:8:7 --#
jz __cl_TopLevel.f_COR8       #-- ./test.lat:8:7 --#
jmp __cl_TopLevel.f_CAND9     #-- ./test.lat:8:7 --#
__cl_TopLevel.f_CAND12 :      #-- ./test.lat:8:23 --#
cmpl $0, %ECX                 #-- ./test.lat:8:30 --#
setl %AL                      #-- ./test.lat:8:32 --#
addq $0, %RSP                 #-- ./test.lat:8:30 --#
testb %AL, %AL                #-- ./test.lat:8:30 --#
jz __cl_TopLevel.f_IELSE6     #-- ./test.lat:8:30 --#
jmp __cl_TopLevel.f_IIF5      #-- ./test.lat:8:30 --#
__cl_TopLevel.f_CAND9 :       #-- ./test.lat:8:7 --#
cmpl $0, %ECX                 #-- ./test.lat:8:13 --#
setg %DL                      #-- ./test.lat:8:15 --#
addq $0, %RSP                 #-- ./test.lat:8:13 --#
testb %DL, %DL                #-- ./test.lat:8:13 --#
jz __cl_TopLevel.f_COR8       #-- ./test.lat:8:13 --#
jmp __cl_TopLevel.f_IIF5      #-- ./test.lat:8:13 --#
__cl_TopLevel.f_COR8 :        #-- ./test.lat:8:6 --#
cmpl $0, %EAX                 #-- ./test.lat:8:23 --#
setl %AL                      #-- ./test.lat:8:24 --#
addq $0, %RSP                 #-- ./test.lat:8:23 --#
testb %AL, %AL                #-- ./test.lat:8:23 --#
jz __cl_TopLevel.f_IELSE6     #-- ./test.lat:8:23 --#
jmp __cl_TopLevel.f_CAND12    #-- ./test.lat:8:23 --#
__cl_TopLevel.f_IELSE6 :      #-- ./test.lat:8:3 --#
movl $42, %EAX                #-- setting %v_return~2 at ./test.lat:7:1 --#
addq $0, %RSP                 #-- ./test.lat:7:1 --#
jmp __cl_TopLevel.f.L_exit    #-- ./test.lat:7:1 --#
__cl_TopLevel.f_IIF5 :        #-- ./test.lat:8:3 --#
movl $7, %EAX                 #-- setting %v_return~2 at ./test.lat:7:1 --#
addq $0, %RSP                 #-- ./test.lat:7:1 --#
jmp __cl_TopLevel.f.L_exit    #-- ./test.lat:7:1 --#
__cl_TopLevel.f.L_exit :      #-- ./test.lat:7:1 --#
movl %EAX, %EAX               #-- move return value at ./test.lat:7:1 --#
addq $0, %RSP                 #-- ./test.lat:7:1 --#
leave                         #-- ./test.lat:7:1 --#
ret                           #-- ./test.lat:7:1 --#
main :                        #-- ./test.lat:1:1 --#
__cl_TopLevel.main :          #-- ./test.lat:1:1 --#
push %RBP                     #-- ./test.lat:1:1 --#
movq %RSP, %RBP               #-- ./test.lat:1:1 --#
subq $0, %RSP                 #-- ./test.lat:1:1 --#
__cl_TopLevel.main.L_entry :  #-- ./test.lat:1:1 --#
subq $0, %RSP                 #-- ./test.lat:3:13 --#
call __cl_TopLevel.f          #-- ./test.lat:3:13 --#
addq $0, %RSP                 #-- ./test.lat:3:13 --#
movl %EAX, %EAX               #-- ./test.lat:3:13 --#
movl %EAX, %EDI               #-- passing arg at ./test.lat:3:4 --#
subq $0, %RSP                 #-- ./test.lat:3:4 --#
call printInt                 #-- ./test.lat:3:4 --#
addq $0, %RSP                 #-- ./test.lat:3:4 --#
movl $0, %EAX                 #-- move return value at ./test.lat:1:1 --#
addq $0, %RSP                 #-- ./test.lat:1:1 --#
leave                         #-- ./test.lat:1:1 --#
ret                           #-- ./test.lat:1:1 --#
__errorNull :                 #-- runtime error on null dereference at ./test.lat:1:1 --#
andq $-16, %RSP               #-- 16 bytes allign at ./test.lat:1:1 --#
call __errorNull              #-- ./test.lat:1:1 --#
