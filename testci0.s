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
__cl_TopLevel.foo :           #-- ./trolololo.lat:5:1 --#
pushq %RBX                    #-- ./trolololo.lat:5:1 --#
pushq %R12                    #-- ./trolololo.lat:5:1 --#
pushq %R13                    #-- ./trolololo.lat:5:1 --#
pushq %R14                    #-- ./trolololo.lat:5:1 --#
pushq %R15                    #-- ./trolololo.lat:5:1 --#
subq $8, %RSP                 #-- ./trolololo.lat:5:1 --#
pushq %RBP                    #-- ./trolololo.lat:5:1 --#
movq %RSP, %RBP               #-- ./trolololo.lat:5:1 --#
__cl_TopLevel.foo.L_entry :   #-- ./trolololo.lat:5:1 --#
movl %EDI, %R15D              #-- load %v_t_57 at ./trolololo.lat:5:1 --#
movl %ESI, %EDI               #-- load %v_t_58 at ./trolololo.lat:5:1 --#
movl %EDX, %R14D              #-- load %v_t_59 at ./trolololo.lat:5:1 --#
movl %ECX, %R13D              #-- load %v_t_60 at ./trolololo.lat:5:1 --#
movl %R8D, %R12D              #-- load %v_t_61 at ./trolololo.lat:5:1 --#
movl %R9D, %EBX               #-- load %v_t_62 at ./trolololo.lat:5:1 --#
movl 64 (%RBP), %R11D         #-- load %v_t_63 at ./trolololo.lat:5:1 --#
movl 72 (%RBP), %R10D         #-- load %v_t_64 at ./trolololo.lat:5:1 --#
movl 80 (%RBP), %R9D          #-- load %v_t_65 at ./trolololo.lat:5:1 --#
movl 88 (%RBP), %EAX          #-- load %v_t_66 at ./trolololo.lat:5:1 --#
movl 96 (%RBP), %R8D          #-- load %v_t_67 at ./trolololo.lat:5:1 --#
movl 104 (%RBP), %ESI         #-- load %v_t_68 at ./trolololo.lat:5:1 --#
movl 112 (%RBP), %EDX         #-- load %v_t_69 at ./trolololo.lat:5:1 --#
movl 120 (%RBP), %ECX         #-- load %v_t_70 at ./trolololo.lat:5:1 --#
addl %R14D, %R13D             #-- ./trolololo.lat:7:45 --#
addl %R13D, %R12D             #-- ./trolololo.lat:7:45 --#
addl %R12D, %EBX              #-- ./trolololo.lat:7:45 --#
addl %EBX, %R11D              #-- ./trolololo.lat:7:45 --#
addl %R11D, %R10D             #-- ./trolololo.lat:7:45 --#
addl %R10D, %R9D              #-- ./trolololo.lat:7:45 --#
addl %R9D, %R8D               #-- ./trolololo.lat:7:45 --#
addl %R8D, %ESI               #-- ./trolololo.lat:7:45 --#
addl %ESI, %EDX               #-- ./trolololo.lat:7:45 --#
addl %EDX, %ECX               #-- ./trolololo.lat:7:45 --#
movl %R15D, %EDX              #-- ./trolololo.lat:7:15 --#
sall $1, %EDX                 #-- multiply by 2 src1=LocReg R15 src2=LocConst 2 dest=LocReg RDX at ./trolololo.lat:7:15 --#
addl %ECX, %EDX               #-- ./trolololo.lat:7:45 --#
movl %EDI, %ECX               #-- ./trolololo.lat:7:19 --#
sarl $1, %ECX                 #-- divide by 2 at ./trolololo.lat:7:19 --#
addl %EDX, %ECX               #-- ./trolololo.lat:7:45 --#
sarl $1, %EAX                 #-- divide by 2 at ./trolololo.lat:7:37 --#
addl %EAX, %ECX               #-- ./trolololo.lat:7:45 --#
movl %ECX, %EAX               #-- ./trolololo.lat:7:48 --#
cdq                           #-- ./trolololo.lat:7:48 --#
pushq %RCX                    #-- ./trolololo.lat:7:48 --#
movl $10, %ECX                #-- ./trolololo.lat:7:48 --#
idivl %ECX                    #-- ./trolololo.lat:7:48 --#
pop %RCX                      #-- ./trolololo.lat:7:48 --#
pushq %RDX                    #-- save caller saved at ./trolololo.lat:8:5 --#
movl %EDX, %EDI               #-- passing arg at ./trolololo.lat:8:5 --#
call printInt                 #-- ./trolololo.lat:8:5 --#
pop %RDX                      #-- ./trolololo.lat:8:5 --#
movl %EDX, %EAX               #-- move return value at ./trolololo.lat:5:1 --#
leave                         #-- ./trolololo.lat:5:1 --#
addq $8, %RSP                 #-- ./trolololo.lat:5:1 --#
pop %R15                      #-- ./trolololo.lat:5:1 --#
pop %R14                      #-- ./trolololo.lat:5:1 --#
pop %R13                      #-- ./trolololo.lat:5:1 --#
pop %R12                      #-- ./trolololo.lat:5:1 --#
pop %RBX                      #-- ./trolololo.lat:5:1 --#
ret                           #-- ./trolololo.lat:5:1 --#
main :                        #-- ./trolololo.lat:1:1 --#
__cl_TopLevel.main :          #-- ./trolololo.lat:1:1 --#
pushq %RBP                    #-- ./trolololo.lat:1:1 --#
movq %RSP, %RBP               #-- ./trolololo.lat:1:1 --#
__cl_TopLevel.main.L_entry :  #-- ./trolololo.lat:1:1 --#
movl $1, %EDI                 #-- passing arg at ./trolololo.lat:3:12 --#
movl $2, %ESI                 #-- passing arg at ./trolololo.lat:3:12 --#
movl $1, %EDX                 #-- passing arg at ./trolololo.lat:3:12 --#
movl $2, %ECX                 #-- passing arg at ./trolololo.lat:3:12 --#
movl $1, %R8D                 #-- passing arg at ./trolololo.lat:3:12 --#
movl $2, %R9D                 #-- passing arg at ./trolololo.lat:3:12 --#
pushq $2                      #-- passing arg at ./trolololo.lat:3:12 --#
pushq $1                      #-- passing arg at ./trolololo.lat:3:12 --#
pushq $2                      #-- passing arg at ./trolololo.lat:3:12 --#
pushq $1                      #-- passing arg at ./trolololo.lat:3:12 --#
pushq $2                      #-- passing arg at ./trolololo.lat:3:12 --#
pushq $1                      #-- passing arg at ./trolololo.lat:3:12 --#
pushq $2                      #-- passing arg at ./trolololo.lat:3:12 --#
pushq $1                      #-- passing arg at ./trolololo.lat:3:12 --#
# call __cl_TopLevel.foo        #-- ./trolololo.lat:3:12 --#
addq $64, %RSP                #-- ./trolololo.lat:3:12 --#
xor %edi, %edi
mov $231, %eax
syscall
leave                         #-- ./trolololo.lat:1:1 --#
ret                           #-- ./trolololo.lat:1:1 --#
__errorNull :                 #-- runtime error on null dereference at ./trolololo.lat:1:1 --#
andq $-16, %RSP               #-- 16 bytes allign at ./trolololo.lat:1:1 --#
call __errorNull              #-- ./trolololo.lat:1:1 --#