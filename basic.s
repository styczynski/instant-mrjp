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
.quad _Array_toString
.quad _Object_getHashCode
.quad _Object_equals

.global _class_Counter
_class_Counter :
.quad _class_Object
.long 8
.quad _class_Counter_methods
.long 0
.quad 0

.global _class_Counter_methods
_class_Counter_methods :
.quad _Object_toString
.quad _Object_getHashCode
.quad _Object_equals
.quad Counter.incr
.quad Counter.value

.global _class_Object
_class_Object :
.quad 0
.long 0
.quad _class_Object_methods
.long 0
.quad 0

.global _class_Object_methods
_class_Object_methods :
.quad _Object_toString
.quad _Object_getHashCode
.quad _Object_equals

.global _class_String
_class_String :
.quad _class_Object
.long 0
.quad _class_String_methods
.long 0
.quad 0

.global _class_String_methods
_class_String_methods :
.quad _String_toString
.quad _String_getHashCode
.quad _String_equals
.quad _String_charAt
.quad _String_concat
.quad _String_startsWith
.quad _String_endsWith
.quad _String_getBytes
.quad _String_indexOf
.quad _String_length
.quad _String_substring

.global main

.section .text
Counter.value :                   #-- ./basic.lat:14:13 --#
pushq %RBP                        #-- ./basic.lat:14:13 --#
movq %RSP, %RBP                   #-- ./basic.lat:14:13 --#
subq $0, %RSP                     #-- ./basic.lat:14:13 --#
Counter.value.L_entry :           #-- ./basic.lat:14:13 --#
movq %RDI, %RAX                   #-- load %v_t_16 at ./basic.lat:14:13 --#
movl 36 (%RAX), %EAX              #-- load %v_t_17 at ./basic.lat:14:34 --#
movl %EAX, %EAX                   #-- move return value at ./basic.lat:14:13 --#
addq $0, %RSP                     #-- ./basic.lat:14:13 --#
leave                             #-- ./basic.lat:14:13 --#
ret                               #-- ./basic.lat:14:13 --#
Counter.incr :                    #-- ./basic.lat:13:13 --#
pushq %RBP                        #-- ./basic.lat:13:13 --#
movq %RSP, %RBP                   #-- ./basic.lat:13:13 --#
subq $0, %RSP                     #-- ./basic.lat:13:13 --#
Counter.incr.L_entry :            #-- ./basic.lat:13:13 --#
movq %RDI, %RDX                   #-- load %v_t_12 at ./basic.lat:13:13 --#
movl 36 (%RDX), %EAX              #-- load %v_t_14 at ./basic.lat:13:27 --#
addl $1, %EAX                     #-- ./basic.lat:13:27 --#
movl %EAX, 36 (%RDX)              #-- ./basic.lat:13:27 --#
movl %ECX, %EAX                   #-- move return value at ./basic.lat:13:13 --#
addq $0, %RSP                     #-- ./basic.lat:13:13 --#
leave                             #-- ./basic.lat:13:13 --#
ret                               #-- ./basic.lat:13:13 --#
main :                            #-- ./basic.lat:1:1 --#
__cl_TopLevel.main :              #-- ./basic.lat:1:1 --#
pushq %RBX                        #-- ./basic.lat:1:1 --#
subq $8, %RSP                     #-- ./basic.lat:1:1 --#
pushq %RBP                        #-- ./basic.lat:1:1 --#
movq %RSP, %RBP                   #-- ./basic.lat:1:1 --#
subq $0, %RSP                     #-- ./basic.lat:1:1 --#
__cl_TopLevel.main.L_entry :      #-- ./basic.lat:1:1 --#
leaq _class_Counter (%RIP), %RDI  #-- ./basic.lat:3:17 --#
subq $0, %RSP                     #-- ./basic.lat:3:17 --#
call __new                        #-- ./basic.lat:3:17 --#
addq $0, %RSP                     #-- ./basic.lat:3:17 --#
movq %RAX, %RBX                   #-- ./basic.lat:3:17 --#
movq %RBX, %RDI                   #-- passing arg at ./basic.lat:4:13 --#
subq $0, %RSP                     #-- ./basic.lat:4:13 --#
testq %RDI, %RDI                  #-- ./basic.lat:4:13 --#
jz __errorNull                    #-- ./basic.lat:4:13 --#
movq 20 (%RDI), %RAX              #-- load address of vtable at ./basic.lat:4:13 --#
call * 24 (%RAX)                  #-- call incr at ./basic.lat:4:13 --#
addq $0, %RSP                     #-- ./basic.lat:4:13 --#
movq %RBX, %RDI                   #-- passing arg at ./basic.lat:5:13 --#
subq $0, %RSP                     #-- ./basic.lat:5:13 --#
testq %RDI, %RDI                  #-- ./basic.lat:5:13 --#
jz __errorNull                    #-- ./basic.lat:5:13 --#
movq 20 (%RDI), %RAX              #-- load address of vtable at ./basic.lat:5:13 --#
call * 24 (%RAX)                  #-- call incr at ./basic.lat:5:13 --#
addq $0, %RSP                     #-- ./basic.lat:5:13 --#
movq %RBX, %RDI                   #-- passing arg at ./basic.lat:6:13 --#
subq $0, %RSP                     #-- ./basic.lat:6:13 --#
testq %RDI, %RDI                  #-- ./basic.lat:6:13 --#
jz __errorNull                    #-- ./basic.lat:6:13 --#
movq 20 (%RDI), %RAX              #-- load address of vtable at ./basic.lat:6:13 --#
call * 24 (%RAX)                  #-- call incr at ./basic.lat:6:13 --#
addq $0, %RSP                     #-- ./basic.lat:6:13 --#
movq %RBX, %RDI                   #-- passing arg at ./basic.lat:7:21 --#
subq $0, %RSP                     #-- ./basic.lat:7:21 --#
testq %RDI, %RDI                  #-- ./basic.lat:7:21 --#
jz __errorNull                    #-- ./basic.lat:7:21 --#
movq 20 (%RDI), %RAX              #-- load address of vtable at ./basic.lat:7:21 --#
call * 32 (%RAX)                  #-- call value at ./basic.lat:7:21 --#
addq $0, %RSP                     #-- ./basic.lat:7:21 --#
movl %EAX, %EAX                   #-- ./basic.lat:7:21 --#
movl %EAX, %EDI                   #-- passing arg at ./basic.lat:8:13 --#
subq $0, %RSP                     #-- ./basic.lat:8:13 --#
call printInt                     #-- ./basic.lat:8:13 --#
addq $0, %RSP                     #-- ./basic.lat:8:13 --#
movl $0, %EAX                     #-- move return value at ./basic.lat:1:1 --#
addq $0, %RSP                     #-- ./basic.lat:1:1 --#
leave                             #-- ./basic.lat:1:1 --#
addq $8, %RSP                     #-- ./basic.lat:1:1 --#
pop %RBX                          #-- ./basic.lat:1:1 --#
ret                               #-- ./basic.lat:1:1 --#
__errorNull :                     #-- runtime error on null dereference at ./basic.lat:1:1 --#
andq $-16, %RSP                   #-- 16 bytes allign at ./basic.lat:1:1 --#
call __errorNull                  #-- ./basic.lat:1:1 --#
