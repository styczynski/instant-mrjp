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
main :                        #-- ./tests/all/good/add.lat:3:1 --#
__cl_TopLevel.main :          #-- ./tests/all/good/add.lat:3:1 --#
push %RBP                     #-- ./tests/all/good/add.lat:3:1 --#
movq %RSP, %RBP               #-- ./tests/all/good/add.lat:3:1 --#
subq $0, %RSP                 #-- ./tests/all/good/add.lat:3:1 --#
__cl_TopLevel.main.L_entry :  #-- ./tests/all/good/add.lat:3:1 --#
movl $2, %EDI                 #-- passing arg at ./tests/all/good/add.lat:4:5 --#
subq $0, %RSP                 #-- ./tests/all/good/add.lat:4:5 --#
call printInt                 #-- ./tests/all/good/add.lat:4:5 --#
addq $0, %RSP                 #-- ./tests/all/good/add.lat:4:5 --#
movl $0, %EAX                 #-- move return value at ./tests/all/good/add.lat:3:1 --#
addq $0, %RSP                 #-- ./tests/all/good/add.lat:3:1 --#
leave                         #-- ./tests/all/good/add.lat:3:1 --#
ret                           #-- ./tests/all/good/add.lat:3:1 --#
__errorNull :                 #-- runtime error on null dereference at ./tests/all/good/add.lat:3:1 --#
andq $-16, %RSP               #-- 16 bytes allign at ./tests/all/good/add.lat:3:1 --#
call __errorNull              #-- ./tests/all/good/add.lat:3:1 --#