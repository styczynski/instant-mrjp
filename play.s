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
pushq %R15                    #-- ./play.lat:7:1 --#
pushq %R14                    #-- ./play.lat:7:1 --#
pushq %R13                    #-- ./play.lat:7:1 --#
pushq %R12                    #-- ./play.lat:7:1 --#
pushq %RBX                    #-- ./play.lat:7:1 --#
subq $8, %RSP                 #-- ./play.lat:7:1 --#
pushq %RBP                    #-- ./play.lat:7:1 --#
movq %RSP, %RBP               #-- ./play.lat:7:1 --#
subq $0, %RSP                 #-- ./play.lat:7:1 --#
__cl_TopLevel.foo.L_entry :   #-- ./play.lat:7:1 --#
movl %EDI, %R15D              #-- load %v_t_57 at ./play.lat:7:1 --#
movl %ESI, %EDI               #-- load %v_t_58 at ./play.lat:7:1 --#
movl %EDX, %R14D              #-- load %v_t_59 at ./play.lat:7:1 --#
movl %ECX, %R13D              #-- load %v_t_60 at ./play.lat:7:1 --#
movl %R8D, %R12D              #-- load %v_t_61 at ./play.lat:7:1 --#
movl %R9D, %EBX               #-- load %v_t_62 at ./play.lat:7:1 --#
movl 64 (%RBP), %R11D         #-- load %v_t_63 at ./play.lat:7:1 --#
movl 72 (%RBP), %R10D         #-- load %v_t_64 at ./play.lat:7:1 --#
movl 80 (%RBP), %R9D          #-- load %v_t_65 at ./play.lat:7:1 --#
movl 88 (%RBP), %EAX          #-- load %v_t_66 at ./play.lat:7:1 --#
movl 96 (%RBP), %R8D          #-- load %v_t_67 at ./play.lat:7:1 --#
movl 104 (%RBP), %ESI         #-- load %v_t_68 at ./play.lat:7:1 --#
movl 112 (%RBP), %EDX         #-- load %v_t_69 at ./play.lat:7:1 --#
movl 120 (%RBP), %ECX         #-- load %v_t_70 at ./play.lat:7:1 --#
addl %R14D, %R13D             #-- ./play.lat:10:43 --#
addl %R13D, %R12D             #-- ./play.lat:10:43 --#
addl %R12D, %EBX              #-- ./play.lat:10:43 --#
addl %EBX, %R11D              #-- ./play.lat:10:43 --#
addl %R11D, %R10D             #-- ./play.lat:10:43 --#
addl %R10D, %R9D              #-- ./play.lat:10:43 --#
addl %R9D, %R8D               #-- ./play.lat:10:43 --#
addl %R8D, %ESI               #-- ./play.lat:10:43 --#
addl %ESI, %EDX               #-- ./play.lat:10:43 --#
addl %EDX, %ECX               #-- ./play.lat:10:43 --#
movl %R15D, %EDX              #-- ./play.lat:10:13 --#
sall $1, %EDX                 #-- multiply by 2 src1=LocReg R15 src2=LocConst 2 dest=LocReg RDX at ./play.lat:10:13 --#
addl %ECX, %EDX               #-- ./play.lat:10:43 --#
movl %EDI, %ECX               #-- ./play.lat:10:17 --#
sarl $1, %ECX                 #-- divide by 2 at ./play.lat:10:17 --#
addl %EDX, %ECX               #-- ./play.lat:10:43 --#
movl %EAX, %EAX               #-- ./play.lat:10:35 --#
sarl $1, %EAX                 #-- divide by 2 at ./play.lat:10:35 --#
addl %EAX, %ECX               #-- ./play.lat:10:43 --#
movl %ECX, %EAX               #-- ./play.lat:10:46 --#
cdq                           #-- ./play.lat:10:46 --#
pushq %RCX                    #-- ./play.lat:10:46 --#
movl $10, %ECX                #-- ./play.lat:10:46 --#
idivl %ECX                    #-- ./play.lat:10:46 --#
pop %RCX                      #-- ./play.lat:10:46 --#
movl %EDX, %EDX               #-- ./play.lat:10:46 --#
pushq %RDX                    #-- save caller saved at ./play.lat:11:3 --#
movl %EDX, %EDI               #-- passing arg at ./play.lat:11:3 --#
subq $0, %RSP                 #-- ./play.lat:11:3 --#
call printInt                 #-- ./play.lat:11:3 --#
addq $0, %RSP                 #-- ./play.lat:11:3 --#
pop %RDX                      #-- ./play.lat:11:3 --#
movl %EDX, %EAX               #-- move return value at ./play.lat:7:1 --#
addq $0, %RSP                 #-- ./play.lat:7:1 --#
leave                         #-- ./play.lat:7:1 --#
addq $8, %RSP                 #-- ./play.lat:7:1 --#
pop %R15                      #-- ./play.lat:7:1 --#
pop %R14                      #-- ./play.lat:7:1 --#
pop %R13                      #-- ./play.lat:7:1 --#
pop %R12                      #-- ./play.lat:7:1 --#
pop %RBX                      #-- ./play.lat:7:1 --#
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
subq $0, %RSP                 #-- ./play.lat:3:10 --#
pushq $2                      #-- passing arg at ./play.lat:3:10 --#
pushq $1                      #-- passing arg at ./play.lat:3:10 --#
pushq $2                      #-- passing arg at ./play.lat:3:10 --#
pushq $1                      #-- passing arg at ./play.lat:3:10 --#
pushq $2                      #-- passing arg at ./play.lat:3:10 --#
pushq $1                      #-- passing arg at ./play.lat:3:10 --#
pushq $2                      #-- passing arg at ./play.lat:3:10 --#
pushq $1                      #-- passing arg at ./play.lat:3:10 --#
call __cl_TopLevel.foo        #-- ./play.lat:3:10 --#
addq $64, %RSP                #-- ./play.lat:3:10 --#
movl %EAX, %EAX               #-- ./play.lat:3:10 --#
movl %EAX, %EAX               #-- move return value at ./play.lat:1:1 --#
addq $0, %RSP                 #-- ./play.lat:1:1 --#
leave                         #-- ./play.lat:1:1 --#
ret                           #-- ./play.lat:1:1 --#
__errorNull :                 #-- runtime error on null dereference at ./play.lat:1:1 --#
andq $-16, %RSP               #-- 16 bytes allign at ./play.lat:1:1 --#
call __errorNull              #-- ./play.lat:1:1 --#
