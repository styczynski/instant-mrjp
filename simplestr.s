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
main :                        #-- ./simplestr.lat:24:1 --#
__cl_TopLevel.main :          #-- ./simplestr.lat:24:1 --#
push %R12                     #-- ./simplestr.lat:24:1 --#
push %RBX                     #-- ./simplestr.lat:24:1 --#
push %RBP                     #-- ./simplestr.lat:24:1 --#
movq %RSP, %RBP               #-- ./simplestr.lat:24:1 --#
__cl_TopLevel.main.L_entry :  #-- ./simplestr.lat:24:1 --#
leaq _class_A (%RIP), %RDI    #-- ./simplestr.lat:25:9 --#
call __new                    #-- ./simplestr.lat:25:9 --#
movq %RAX, %R12               #-- ./simplestr.lat:25:9 --#
leaq _class_B (%RIP), %RDI    #-- ./simplestr.lat:26:9 --#
call __new                    #-- ./simplestr.lat:26:9 --#
movq %RAX, %RDI               #-- passing arg at ./simplestr.lat:26:3 --#
leaq _class_A (%RIP), %RSI    #-- ./simplestr.lat:26:3 --#
call __cast                   #-- ./simplestr.lat:26:3 --#
movq %RAX, %RBX               #-- ./simplestr.lat:26:3 --#
movq 8 (%R12), %RAX           #-- load data (indirect) at ./simplestr.lat:27:3 --#
movl $42, 0 (%RAX)            #-- ./simplestr.lat:27:3 --#
movq 8 (%RBX), %RAX           #-- load data (indirect) at ./simplestr.lat:28:3 --#
movl $42, 0 (%RAX)            #-- ./simplestr.lat:28:3 --#
movq %R12, %RDI               #-- passing arg at ./simplestr.lat:31:12 --#
movl $15, %ESI                #-- passing arg at ./simplestr.lat:31:12 --#
testq %RDI, %RDI              #-- ./simplestr.lat:31:12 --#
jz __errorNull                #-- ./simplestr.lat:31:12 --#
movq 20 (%RDI), %RAX          #-- load address of vtable at ./simplestr.lat:31:12 --#
call * 24 (%RAX)              #-- call bar at ./simplestr.lat:31:12 --#
movl %EAX, %EDI               #-- passing arg at ./simplestr.lat:31:3 --#
call printInt                 #-- ./simplestr.lat:31:3 --#
movq %RBX, %RDI               #-- passing arg at ./simplestr.lat:32:12 --#
movl $15, %ESI                #-- passing arg at ./simplestr.lat:32:12 --#
testq %RDI, %RDI              #-- ./simplestr.lat:32:12 --#
jz __errorNull                #-- ./simplestr.lat:32:12 --#
movq 20 (%RDI), %RAX          #-- load address of vtable at ./simplestr.lat:32:12 --#
call * 24 (%RAX)              #-- call bar at ./simplestr.lat:32:12 --#
movl %EAX, %EDI               #-- passing arg at ./simplestr.lat:32:3 --#
call printInt                 #-- ./simplestr.lat:32:3 --#
xorl %EAX, %EAX               #-- move return value at ./simplestr.lat:24:1 --#
leave                         #-- ./simplestr.lat:24:1 --#
pop %R12                      #-- ./simplestr.lat:24:1 --#
pop %RBX                      #-- ./simplestr.lat:24:1 --#
ret                           #-- ./simplestr.lat:24:1 --#
__cl_TopLevel.bar :           #-- ./simplestr.lat:20:1 --#
push %RBP                     #-- ./simplestr.lat:20:1 --#
movq %RSP, %RBP               #-- ./simplestr.lat:20:1 --#
__cl_TopLevel.bar.L_entry :   #-- ./simplestr.lat:20:1 --#
movl %EDI, %EAX               #-- load %v_t_19 at ./simplestr.lat:20:1 --#
sall $1, %EAX                 #-- multiply by 2 at ./simplestr.lat:21:11 --#
leave                         #-- ./simplestr.lat:20:1 --#
ret                           #-- ./simplestr.lat:20:1 --#
__cl_TopLevel.foo :           #-- ./simplestr.lat:16:1 --#
push %RBP                     #-- ./simplestr.lat:16:1 --#
movq %RSP, %RBP               #-- ./simplestr.lat:16:1 --#
__cl_TopLevel.foo.L_entry :   #-- ./simplestr.lat:16:1 --#
movq %RDI, %RCX               #-- load %v_t_15 at ./simplestr.lat:16:1 --#
leaq __const_1 (%RIP), %RAX   #-- ./simplestr.lat:17:14 --#
push %RCX                     #-- save caller saved at ./simplestr.lat:17:14 --#
movq %RAX, %RDI               #-- passing arg at ./simplestr.lat:17:14 --#
call __createString           #-- ./simplestr.lat:17:14 --#
pop %RCX                      #-- ./simplestr.lat:17:14 --#
movq %RCX, %RDI               #-- passing arg at ./simplestr.lat:17:10 --#
movq %RAX, %RSI               #-- passing arg at ./simplestr.lat:17:10 --#
testq %RDI, %RDI              #-- ./simplestr.lat:17:10 --#
jz __errorNull                #-- ./simplestr.lat:17:10 --#
movq 20 (%RDI), %RAX          #-- load address of vtable at ./simplestr.lat:17:10 --#
call * 16 (%RAX)              #-- call concat at ./simplestr.lat:17:10 --#
leave                         #-- ./simplestr.lat:16:1 --#
ret                           #-- ./simplestr.lat:16:1 --#
B.bar :                       #-- ./simplestr.lat:9:3 --#
push %R12                     #-- ./simplestr.lat:9:3 --#
push %RBX                     #-- ./simplestr.lat:9:3 --#
push %RBP                     #-- ./simplestr.lat:9:3 --#
movq %RSP, %RBP               #-- ./simplestr.lat:9:3 --#
B.bar.L_entry :               #-- ./simplestr.lat:9:3 --#
movq %RDI, %RBX               #-- load %v_t_4 at ./simplestr.lat:9:3 --#
movl %ESI, %R12D              #-- load %v_t_5 at ./simplestr.lat:9:3 --#
movq 8 (%RBX), %RAX           #-- load data (indirect) at ./simplestr.lat:10:14 --#
movl 0 (%RAX), %EAX           #-- load %v_t_6 at ./simplestr.lat:10:14 --#
movl %EAX, %EDI               #-- passing arg at ./simplestr.lat:10:5 --#
call printInt                 #-- ./simplestr.lat:10:5 --#
movl %R12D, %EDI              #-- passing arg at ./simplestr.lat:11:5 --#
call printInt                 #-- ./simplestr.lat:11:5 --#
leal 2 (%R12), %ECX           #--  addition %v_t_10 at ./simplestr.lat:12:17 --#
movq 8 (%RBX), %RAX           #-- load data (indirect) at ./simplestr.lat:12:12 --#
movl 0 (%RAX), %EAX           #-- load %v_t_12 at ./simplestr.lat:12:12 --#
sall $1, %EAX                 #-- multiply by 2 at ./simplestr.lat:12:13 --#
addl %ECX, %EAX               #-- ./simplestr.lat:12:17 --#
leave                         #-- ./simplestr.lat:9:3 --#
pop %R12                      #-- ./simplestr.lat:9:3 --#
pop %RBX                      #-- ./simplestr.lat:9:3 --#
ret                           #-- ./simplestr.lat:9:3 --#
A.bar :                       #-- ./simplestr.lat:3:3 --#
push %RBP                     #-- ./simplestr.lat:3:3 --#
movq %RSP, %RBP               #-- ./simplestr.lat:3:3 --#
A.bar.L_entry :               #-- ./simplestr.lat:3:3 --#
movl %ESI, %EAX               #-- load %v_t_1 at ./simplestr.lat:3:3 --#
sall $1, %EAX                 #-- multiply by 2 at ./simplestr.lat:4:13 --#
leave                         #-- ./simplestr.lat:3:3 --#
ret                           #-- ./simplestr.lat:3:3 --#
__errorNull :                 #-- runtime error on null dereference at ./simplestr.lat:1:1 --#
andq $-16, %RSP               #-- 16 bytes allign at ./simplestr.lat:1:1 --#
call __errorNull              #-- ./simplestr.lat:1:1 --#
