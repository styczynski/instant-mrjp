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
.string "4"

__const_2 :
.string "6"

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
main :                        #-- ./play.lat:1:1 --#
__cl_TopLevel.main :          #-- ./play.lat:1:1 --#
pushq %RBX                    #-- ./play.lat:1:1 --#
subq $8, %RSP                 #-- ./play.lat:1:1 --#
pushq %RBP                    #-- ./play.lat:1:1 --#
movq %RSP, %RBP               #-- ./play.lat:1:1 --#
subq $0, %RSP                 #-- ./play.lat:1:1 --#
__cl_TopLevel.main.L_entry :  #-- ./play.lat:1:1 --#
leaq __const_1 (%RIP), %RBX   #-- ./play.lat:3:29 --#
movq %RBX, %RDI               #-- passing arg at ./play.lat:3:29 --#
subq $0, %RSP                 #-- ./play.lat:3:29 --#
call __createString           #-- ./play.lat:3:29 --#
addq $0, %RSP                 #-- ./play.lat:3:29 --#
movq %RAX, %RBX               #-- ./play.lat:3:29 --#
movq %RBX, %RDI               #-- passing arg at ./play.lat:3:17 --#
subq $0, %RSP                 #-- ./play.lat:3:17 --#
call printString              #-- ./play.lat:3:17 --#
addq $0, %RSP                 #-- ./play.lat:3:17 --#
movq %RBX, %RDI               #-- passing arg at ./play.lat:5:17 --#
subq $0, %RSP                 #-- ./play.lat:5:17 --#
call printString              #-- ./play.lat:5:17 --#
addq $0, %RSP                 #-- ./play.lat:5:17 --#
leaq __const_2 (%RIP), %RBX   #-- ./play.lat:11:29 --#
movq %RBX, %RDI               #-- passing arg at ./play.lat:11:29 --#
subq $0, %RSP                 #-- ./play.lat:11:29 --#
call __createString           #-- ./play.lat:11:29 --#
addq $0, %RSP                 #-- ./play.lat:11:29 --#
movq %RAX, %RBX               #-- ./play.lat:11:29 --#
movq %RBX, %RDI               #-- passing arg at ./play.lat:11:17 --#
subq $0, %RSP                 #-- ./play.lat:11:17 --#
call printString              #-- ./play.lat:11:17 --#
addq $0, %RSP                 #-- ./play.lat:11:17 --#
movq %RBX, %RDI               #-- passing arg at ./play.lat:13:17 --#
subq $0, %RSP                 #-- ./play.lat:13:17 --#
call printString              #-- ./play.lat:13:17 --#
addq $0, %RSP                 #-- ./play.lat:13:17 --#
movl $0, %EAX                 #-- move return value at ./play.lat:1:1 --#
addq $0, %RSP                 #-- ./play.lat:1:1 --#
leave                         #-- ./play.lat:1:1 --#
addq $8, %RSP                 #-- ./play.lat:1:1 --#
pop %RBX                      #-- ./play.lat:1:1 --#
ret                           #-- ./play.lat:1:1 --#
__errorNull :                 #-- runtime error on null dereference at ./play.lat:1:1 --#
andq $-16, %RSP               #-- 16 bytes allign at ./play.lat:1:1 --#
call __errorNull              #-- ./play.lat:1:1 --#
