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
__cl_TopLevel.foo :           #-- ./play.lat:7:1 --#
pushq %RBP                    #-- ./play.lat:7:1 --#
movq %RSP, %RBP               #-- ./play.lat:7:1 --#
subq $0, %RSP                 #-- ./play.lat:7:1 --#
__cl_TopLevel.foo.L_entry :   #-- ./play.lat:7:1 --#
movl %EDI, %R11D              #-- load %v_t_43 at ./play.lat:7:1 --#
movl %ESI, %R10D              #-- load %v_t_44 at ./play.lat:7:1 --#
movl %EDX, %ESI               #-- load %v_t_45 at ./play.lat:7:1 --#
movl %ECX, %EDI               #-- load %v_t_46 at ./play.lat:7:1 --#
movl %R8D, %EDX               #-- load %v_t_47 at ./play.lat:7:1 --#
movl %R9D, %EAX               #-- load %v_t_48 at ./play.lat:7:1 --#
movl 8 (%RBP), %ECX           #-- load %v_t_49 at ./play.lat:7:1 --#
addl %R10D, %ESI              #-- ./play.lat:15:30 --#
addl %ESI, %EDI               #-- ./play.lat:15:30 --#
addl %EDI, %EDX               #-- ./play.lat:15:30 --#
addl %EDX, %EAX               #-- ./play.lat:15:30 --#
addl %ECX, %EAX               #-- ./play.lat:15:30 --#
sall $1, %ECX                 #-- multiply by 2 at ./play.lat:15:18 --#
addl %ECX, %EAX               #-- ./play.lat:15:30 --#
movl %EAX, %EDI               #-- passing arg at ./play.lat:17:3 --#
subq $0, %RSP                 #-- ./play.lat:17:3 --#
call printInt                 #-- ./play.lat:17:3 --#
addq $0, %RSP                 #-- ./play.lat:17:3 --#
movl $0, %EAX                 #-- move return value at ./play.lat:7:1 --#
addq $0, %RSP                 #-- ./play.lat:7:1 --#
leave                         #-- ./play.lat:7:1 --#
ret                           #-- ./play.lat:7:1 --#
main :                        #-- ./play.lat:1:1 --#
__cl_TopLevel.main :          #-- ./play.lat:1:1 --#
pushq %RBP                    #-- ./play.lat:1:1 --#
movq %RSP, %RBP               #-- ./play.lat:1:1 --#
subq $0, %RSP                 #-- ./play.lat:1:1 --#
__cl_TopLevel.main.L_entry :  #-- ./play.lat:1:1 --#
movl $1, %EDI                 #-- passing arg at ./play.lat:3:10 --#
movl $2, %ESI                 #-- passing arg at ./play.lat:3:10 --#
movl $1, %EDX                 #-- passing arg at ./play.lat:3:10 --#
movl $2, %ECX                 #-- passing arg at ./play.lat:3:10 --#
movl $1, %R8D                 #-- passing arg at ./play.lat:3:10 --#
movl $2, %R9D                 #-- passing arg at ./play.lat:3:10 --#
subq $8, %RSP                 #-- ./play.lat:3:10 --#
pushq $1                      #-- passing arg at ./play.lat:3:10 --#
call __cl_TopLevel.foo        #-- ./play.lat:3:10 --#
addq $16, %RSP                #-- ./play.lat:3:10 --#
movl %EAX, %EAX               #-- ./play.lat:3:10 --#
movl %EAX, %EAX               #-- move return value at ./play.lat:1:1 --#
addq $0, %RSP                 #-- ./play.lat:1:1 --#
leave                         #-- ./play.lat:1:1 --#
ret                           #-- ./play.lat:1:1 --#
__errorNull :                 #-- runtime error on null dereference at ./play.lat:1:1 --#
andq $-16, %RSP               #-- 16 bytes allign at ./play.lat:1:1 --#
call __errorNull              #-- ./play.lat:1:1 --#
