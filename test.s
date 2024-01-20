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
.string "foo"

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
main :                          #-- ./test.lat:1:1 --#
__cl_TopLevel.main :            #-- ./test.lat:1:1 --#
push %RBX                       #-- ./test.lat:1:1 --#
subq $8, %RSP                   #-- ./test.lat:1:1 --#
push %RBP                       #-- ./test.lat:1:1 --#
movq %RSP, %RBP                 #-- ./test.lat:1:1 --#
__cl_TopLevel.main.L_entry :    #-- ./test.lat:1:1 --#
movl $1, %EDI                   #-- passing arg at ./test.lat:5:5 --#
call printInt                   #-- ./test.lat:5:5 --#
movl $78, %EDI                  #-- passing arg at ./test.lat:7:3 --#
call printInt                   #-- ./test.lat:7:3 --#
movl $78, %EBX                  #-- setting %v_t_1~2 at ./test.lat:8:3 --#
jmp __cl_TopLevel.main_WCOND8   #-- ./test.lat:8:3 --#
__cl_TopLevel.main_IELSE21 :    #-- ./test.lat:17:3 --#
leaq __const_1 (%RIP), %RAX     #-- ./test.lat:21:17 --#
movq %RAX, %RDI                 #-- passing arg at ./test.lat:21:17 --#
call __createString             #-- ./test.lat:21:17 --#
movq %RAX, %RDI                 #-- passing arg at ./test.lat:21:5 --#
call printString                #-- ./test.lat:21:5 --#
jmp __cl_TopLevel.main_IEND22   #-- ./test.lat:17:3 --#
__cl_TopLevel.main_IIF20 :      #-- ./test.lat:17:3 --#
movl $4, %EDI                   #-- passing arg at ./test.lat:19:5 --#
call printInt                   #-- ./test.lat:19:5 --#
jmp __cl_TopLevel.main_IEND22   #-- ./test.lat:17:3 --#
__cl_TopLevel.main_WBEG9 :      #-- ./test.lat:8:3 --#
decl %EBX                       #-- ./test.lat:9:5 --#
movl %EBX, %EDI                 #-- passing arg at ./test.lat:10:5 --#
call printInt                   #-- ./test.lat:10:5 --#
leal 7 (%RBX), %EAX             #--  addition %v_t_15 at ./test.lat:13:14 --#
movl %EAX, %EDI                 #-- passing arg at ./test.lat:14:4 --#
call printInt                   #-- ./test.lat:14:4 --#
jmp __cl_TopLevel.main_WCOND8   #-- ./test.lat:8:3 --#
__cl_TopLevel.main_WCOND8 :     #-- ./test.lat:8:3 --#
cmpl $76, %EBX                  #-- ./test.lat:8:10 --#
jle __cl_TopLevel.main_WEND10   #-- ./test.lat:8:10 --#
jmp __cl_TopLevel.main_WBEG9    #-- ./test.lat:8:10 --#
__cl_TopLevel.main_WEND10 :     #-- ./test.lat:8:3 --#
movl %EBX, %EDI                 #-- passing arg at ./test.lat:16:3 --#
call printInt                   #-- ./test.lat:16:3 --#
cmpl $4, %EBX                   #-- ./test.lat:17:7 --#
jg __cl_TopLevel.main_IIF20     #-- ./test.lat:17:7 --#
jmp __cl_TopLevel.main_IELSE21  #-- ./test.lat:17:7 --#
__cl_TopLevel.main_IEND22 :     #-- ./test.lat:17:3 --#
movl %EBX, %EDI                 #-- passing arg at ./test.lat:23:3 --#
call printInt                   #-- ./test.lat:23:3 --#
xorl %EAX, %EAX                 #-- move return value at ./test.lat:1:1 --#
leave                           #-- ./test.lat:1:1 --#
addq $8, %RSP                   #-- ./test.lat:1:1 --#
pop %RBX                        #-- ./test.lat:1:1 --#
ret                             #-- ./test.lat:1:1 --#
__errorNull :                   #-- runtime error on null dereference at ./test.lat:1:1 --#
andq $-16, %RSP                 #-- 16 bytes allign at ./test.lat:1:1 --#
call __errorNull                #-- ./test.lat:1:1 --#
