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
main :                         #-- simple.lat:5:1 --#
__cl_TopLevel.main :           #-- simple.lat:5:1 --#
push %RBP                      #-- simple.lat:5:1 --#
movq %RSP, %RBP                #-- simple.lat:5:1 --#
subq $0, %RSP                  #-- simple.lat:5:1 --#
__cl_TopLevel.main.L_entry :   #-- simple.lat:5:1 --#
movl $4, %EDI                  #-- passing arg at simple.lat:6:18 --#
subq $0, %RSP                  #-- simple.lat:6:18 --#
call __cl_TopLevel.foo         #-- simple.lat:6:18 --#
addq $0, %RSP                  #-- simple.lat:6:18 --#
movl %EAX, %EAX                #-- simple.lat:6:18 --#
movl %EAX, %EDI                #-- passing arg at simple.lat:6:9 --#
subq $0, %RSP                  #-- simple.lat:6:9 --#
call printInt                  #-- simple.lat:6:9 --#
addq $0, %RSP                  #-- simple.lat:6:9 --#
addq $0, %RSP                  #-- simple.lat:7:9 --#
jmp __cl_TopLevel.main.L_exit  #-- simple.lat:7:9 --#
__cl_TopLevel.main.L_exit :    #-- simple.lat:5:1 --#
movl $0, %EAX                  #-- move return value at simple.lat:5:1 --#
addq $0, %RSP                  #-- simple.lat:5:1 --#
leave                          #-- simple.lat:5:1 --#
ret                            #-- simple.lat:5:1 --#
__cl_TopLevel.foo :            #-- simple.lat:1:1 --#
push %RBP                      #-- simple.lat:1:1 --#
movq %RSP, %RBP                #-- simple.lat:1:1 --#
subq $0, %RSP                  #-- simple.lat:1:1 --#
__cl_TopLevel.foo.L_entry :    #-- simple.lat:1:1 --#
movl %EDI, %EAX                #-- load %v_t_0 at simple.lat:1:1 --#
addl $2, %EAX                  #-- simple.lat:2:17 --#
addq $0, %RSP                  #-- simple.lat:2:9 --#
jmp __cl_TopLevel.foo.L_exit   #-- simple.lat:2:9 --#
__cl_TopLevel.foo.L_exit :     #-- simple.lat:1:1 --#
movl %EAX, %EAX                #-- move return value at simple.lat:1:1 --#
addq $0, %RSP                  #-- simple.lat:1:1 --#
leave                          #-- simple.lat:1:1 --#
ret                            #-- simple.lat:1:1 --#
__errorNull :                  #-- runtime error on null dereference at simple.lat:1:1 --#
andq $-16, %RSP                #-- 16 bytes allign at simple.lat:1:1 --#
call __errorNull               #-- simple.lat:1:1 --#
