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
.string "CALL"

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
__cl_TopLevel.ifac2f :           #-- ./play.lat:29:1 --#
pushq %RBX                       #-- ./play.lat:29:1 --#
pushq %R12                       #-- ./play.lat:29:1 --#
pushq %R13                       #-- ./play.lat:29:1 --#
pushq %R14                       #-- ./play.lat:29:1 --#
pushq %RBP                       #-- ./play.lat:29:1 --#
movq %RSP, %RBP                  #-- ./play.lat:29:1 --#
subq $0, %RSP                    #-- ./play.lat:29:1 --#
__cl_TopLevel.ifac2f.L_entry :   #-- ./play.lat:29:1 --#
movl %EDI, %R13D                 #-- load %v_t_7 at ./play.lat:29:1 --#
movl %ESI, %R12D                 #-- load %v_t_8 at ./play.lat:29:1 --#
leaq __const_1 (%RIP), %RAX      #-- ./play.lat:30:15 --#
movq %RAX, %RDI                  #-- passing arg at ./play.lat:30:15 --#
subq $0, %RSP                    #-- ./play.lat:30:15 --#
call __createString              #-- ./play.lat:30:15 --#
addq $0, %RSP                    #-- ./play.lat:30:15 --#
movq %RAX, %RAX                  #-- ./play.lat:30:15 --#
movq %RAX, %RDI                  #-- passing arg at ./play.lat:30:3 --#
subq $0, %RSP                    #-- ./play.lat:30:3 --#
call printString                 #-- ./play.lat:30:3 --#
addq $0, %RSP                    #-- ./play.lat:30:3 --#
movl %R13D, %EDI                 #-- passing arg at ./play.lat:31:3 --#
subq $0, %RSP                    #-- ./play.lat:31:3 --#
call printInt                    #-- ./play.lat:31:3 --#
addq $0, %RSP                    #-- ./play.lat:31:3 --#
movl %R12D, %EDI                 #-- passing arg at ./play.lat:32:3 --#
subq $0, %RSP                    #-- ./play.lat:32:3 --#
call printInt                    #-- ./play.lat:32:3 --#
addq $0, %RSP                    #-- ./play.lat:32:3 --#
cmpl %R12D, %R13D                #-- ./play.lat:33:13 --#
sete %AL                         #-- ./play.lat:33:15 --#
addq $0, %RSP                    #-- ./play.lat:33:13 --#
testb %AL, %AL                   #-- ./play.lat:33:13 --#
jz __cl_TopLevel.ifac2f_IELSE15  #-- ./play.lat:33:13 --#
jmp __cl_TopLevel.ifac2f_IIF14   #-- ./play.lat:33:13 --#
__cl_TopLevel.ifac2f_IELSE15 :   #-- ./play.lat:33:9 --#
cmpl %R12D, %R13D                #-- ./play.lat:35:13 --#
setg %AL                         #-- ./play.lat:35:15 --#
addq $0, %RSP                    #-- ./play.lat:35:13 --#
testb %AL, %AL                   #-- ./play.lat:35:13 --#
jz __cl_TopLevel.ifac2f_IELSE18  #-- ./play.lat:35:13 --#
jmp __cl_TopLevel.ifac2f_IIF17   #-- ./play.lat:35:13 --#
__cl_TopLevel.ifac2f_IELSE18 :   #-- ./play.lat:35:9 --#
leal 0 (%R13, %R12, 1), %EAX     #-- addition %v_t_23 at ./play.lat:38:16 --#
movl %EAX, %R14D                 #-- ./play.lat:38:21 --#
sarl $1, %R14D                   #-- divide by 2 at ./play.lat:38:21 --#
movl %R14D, %EDI                 #-- passing arg at ./play.lat:39:9 --#
subq $0, %RSP                    #-- ./play.lat:39:9 --#
call printInt                    #-- ./play.lat:39:9 --#
addq $0, %RSP                    #-- ./play.lat:39:9 --#
movl %R13D, %EDI                 #-- passing arg at ./play.lat:40:16 --#
movl %R14D, %ESI                 #-- passing arg at ./play.lat:40:16 --#
subq $0, %RSP                    #-- ./play.lat:40:16 --#
call __cl_TopLevel.ifac2f        #-- ./play.lat:40:16 --#
addq $0, %RSP                    #-- ./play.lat:40:16 --#
movl %EAX, %EBX                  #-- ./play.lat:40:16 --#
leal 1 (%R14), %EAX              #--  addition %v_t_29 at ./play.lat:40:38 --#
movl %EAX, %EDI                  #-- passing arg at ./play.lat:40:30 --#
movl %R12D, %ESI                 #-- passing arg at ./play.lat:40:30 --#
subq $0, %RSP                    #-- ./play.lat:40:30 --#
call __cl_TopLevel.ifac2f        #-- ./play.lat:40:30 --#
addq $0, %RSP                    #-- ./play.lat:40:30 --#
movl %EAX, %EAX                  #-- ./play.lat:40:30 --#
imull %EAX, %EBX                 #-- ./play.lat:40:28 --#
xchgl %EAX, %EBX                 #-- ./play.lat:29:1 --#
addq $0, %RSP                    #-- ./play.lat:29:1 --#
jmp __cl_TopLevel.ifac2f.L_exit  #-- ./play.lat:29:1 --#
__cl_TopLevel.ifac2f_IIF14 :     #-- ./play.lat:33:9 --#
xchgl %EAX, %R13D                #-- ./play.lat:29:1 --#
addq $0, %RSP                    #-- ./play.lat:29:1 --#
jmp __cl_TopLevel.ifac2f.L_exit  #-- ./play.lat:29:1 --#
__cl_TopLevel.ifac2f_IIF17 :     #-- ./play.lat:35:9 --#
movl $1, %EAX                    #-- setting %v_return~2 at ./play.lat:29:1 --#
addq $0, %RSP                    #-- ./play.lat:29:1 --#
jmp __cl_TopLevel.ifac2f.L_exit  #-- ./play.lat:29:1 --#
__cl_TopLevel.ifac2f.L_exit :    #-- ./play.lat:29:1 --#
movl %EAX, %EAX                  #-- move return value at ./play.lat:29:1 --#
addq $0, %RSP                    #-- ./play.lat:29:1 --#
leave                            #-- ./play.lat:29:1 --#
pop %R14                         #-- ./play.lat:29:1 --#
pop %R13                         #-- ./play.lat:29:1 --#
pop %R12                         #-- ./play.lat:29:1 --#
pop %RBX                         #-- ./play.lat:29:1 --#
ret                              #-- ./play.lat:29:1 --#
main :                           #-- ./play.lat:1:1 --#
__cl_TopLevel.main :             #-- ./play.lat:1:1 --#
pushq %RBP                       #-- ./play.lat:1:1 --#
movq %RSP, %RBP                  #-- ./play.lat:1:1 --#
subq $0, %RSP                    #-- ./play.lat:1:1 --#
__cl_TopLevel.main.L_entry :     #-- ./play.lat:1:1 --#
movl $1, %EDI                    #-- passing arg at ./play.lat:2:18 --#
movl $10, %ESI                   #-- passing arg at ./play.lat:2:18 --#
subq $0, %RSP                    #-- ./play.lat:2:18 --#
call __cl_TopLevel.ifac2f        #-- ./play.lat:2:18 --#
addq $0, %RSP                    #-- ./play.lat:2:18 --#
movl %EAX, %EAX                  #-- ./play.lat:2:18 --#
movl %EAX, %EDI                  #-- passing arg at ./play.lat:2:9 --#
subq $0, %RSP                    #-- ./play.lat:2:9 --#
call printInt                    #-- ./play.lat:2:9 --#
addq $0, %RSP                    #-- ./play.lat:2:9 --#
movl $0, %EAX                    #-- move return value at ./play.lat:1:1 --#
addq $0, %RSP                    #-- ./play.lat:1:1 --#
leave                            #-- ./play.lat:1:1 --#
ret                              #-- ./play.lat:1:1 --#
__errorNull :                    #-- runtime error on null dereference at ./play.lat:1:1 --#
andq $-16, %RSP                  #-- 16 bytes allign at ./play.lat:1:1 --#
call __errorNull                 #-- ./play.lat:1:1 --#
