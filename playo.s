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
__const_5 :
.string "!"

__const_3 :
.string "&&"

__const_2 :
.string "false"

__const_1 :
.string "true"

__const_4 :
.string "||"

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
__cl_TopLevel.test :                             #-- ./playo.lat:28:1 --#
push %RBX                                        #-- ./playo.lat:28:1 --#
subq $8, %RSP                                    #-- ./playo.lat:28:1 --#
push %RBP                                        #-- ./playo.lat:28:1 --#
movq %RSP, %RBP                                  #-- ./playo.lat:28:1 --#
subq $0, %RSP                                    #-- ./playo.lat:28:1 --#
__cl_TopLevel.test.L_entry :                     #-- ./playo.lat:28:1 --#
movl %EDI, %EBX                                  #-- load %v_t_98 at ./playo.lat:28:1 --#
movl %EBX, %EDI                                  #-- passing arg at ./playo.lat:29:3 --#
subq $0, %RSP                                    #-- ./playo.lat:29:3 --#
call printInt                                    #-- ./playo.lat:29:3 --#
addq $0, %RSP                                    #-- ./playo.lat:29:3 --#
cmpl $0, %EBX                                    #-- ./playo.lat:30:10 --#
setg %AL                                         #-- ./playo.lat:30:12 --#
addq $0, %RSP                                    #-- ./playo.lat:30:10 --#
testb %AL, %AL                                   #-- ./playo.lat:30:10 --#
jz __cl_TopLevel.test_IELSE101                   #-- ./playo.lat:30:10 --#
jmp __cl_TopLevel.test_IIF100                    #-- ./playo.lat:30:10 --#
__cl_TopLevel.test_IELSE101 :                    #-- ./playo.lat:30:3 --#
movl $0, %EAX                                    #-- setting %v_return~2 at ./playo.lat:28:1 --#
addq $0, %RSP                                    #-- ./playo.lat:28:1 --#
jmp __cl_TopLevel.test.L_exit                    #-- ./playo.lat:28:1 --#
__cl_TopLevel.test_IIF100 :                      #-- ./playo.lat:30:3 --#
movl $1, %EAX                                    #-- setting %v_return~2 at ./playo.lat:28:1 --#
addq $0, %RSP                                    #-- ./playo.lat:28:1 --#
jmp __cl_TopLevel.test.L_exit                    #-- ./playo.lat:28:1 --#
__cl_TopLevel.test.L_exit :                      #-- ./playo.lat:28:1 --#
movl %EAX, %EAX                                  #-- move return value at ./playo.lat:28:1 --#
addq $0, %RSP                                    #-- ./playo.lat:28:1 --#
leave                                            #-- ./playo.lat:28:1 --#
addq $8, %RSP                                    #-- ./playo.lat:28:1 --#
pop %RBX                                         #-- ./playo.lat:28:1 --#
ret                                              #-- ./playo.lat:28:1 --#
__cl_TopLevel.printBool :                        #-- ./playo.lat:19:1 --#
push %RBX                                        #-- ./playo.lat:19:1 --#
subq $8, %RSP                                    #-- ./playo.lat:19:1 --#
push %RBP                                        #-- ./playo.lat:19:1 --#
movq %RSP, %RBP                                  #-- ./playo.lat:19:1 --#
subq $0, %RSP                                    #-- ./playo.lat:19:1 --#
__cl_TopLevel.printBool.L_entry :                #-- ./playo.lat:19:1 --#
movl %EDI, %EAX                                  #-- load %v_t_88 at ./playo.lat:19:1 --#
cmpl $0, %EAX                                    #-- ./playo.lat:20:8 --#
sete %AL                                         #-- ./playo.lat:20:8 --#
addq $0, %RSP                                    #-- ./playo.lat:20:8 --#
testb %AL, %AL                                   #-- ./playo.lat:20:8 --#
jz __cl_TopLevel.printBool_IELSE90               #-- ./playo.lat:20:8 --#
jmp __cl_TopLevel.printBool_IIF89                #-- ./playo.lat:20:8 --#
__cl_TopLevel.printBool_IELSE90 :                #-- ./playo.lat:20:3 --#
leaq __const_1 (%RIP), %RAX                      #-- ./playo.lat:23:17 --#
movq %RAX, %RDI                                  #-- passing arg at ./playo.lat:23:17 --#
subq $0, %RSP                                    #-- ./playo.lat:23:17 --#
call __createString                              #-- ./playo.lat:23:17 --#
addq $0, %RSP                                    #-- ./playo.lat:23:17 --#
movq %RAX, %RAX                                  #-- ./playo.lat:23:17 --#
movq %RAX, %RDI                                  #-- passing arg at ./playo.lat:23:5 --#
subq $0, %RSP                                    #-- ./playo.lat:23:5 --#
call printString                                 #-- ./playo.lat:23:5 --#
addq $0, %RSP                                    #-- ./playo.lat:23:5 --#
addq $0, %RSP                                    #-- ./playo.lat:20:3 --#
jmp __cl_TopLevel.printBool_IEND91               #-- ./playo.lat:20:3 --#
__cl_TopLevel.printBool_IIF89 :                  #-- ./playo.lat:20:3 --#
leaq __const_2 (%RIP), %RAX                      #-- ./playo.lat:21:17 --#
movq %RAX, %RDI                                  #-- passing arg at ./playo.lat:21:17 --#
subq $0, %RSP                                    #-- ./playo.lat:21:17 --#
call __createString                              #-- ./playo.lat:21:17 --#
addq $0, %RSP                                    #-- ./playo.lat:21:17 --#
movq %RAX, %RAX                                  #-- ./playo.lat:21:17 --#
movq %RAX, %RDI                                  #-- passing arg at ./playo.lat:21:5 --#
subq $0, %RSP                                    #-- ./playo.lat:21:5 --#
call printString                                 #-- ./playo.lat:21:5 --#
addq $0, %RSP                                    #-- ./playo.lat:21:5 --#
addq $0, %RSP                                    #-- ./playo.lat:20:3 --#
jmp __cl_TopLevel.printBool_IEND91               #-- ./playo.lat:20:3 --#
__cl_TopLevel.printBool_IEND91 :                 #-- ./playo.lat:20:3 --#
movl %EBX, %EAX                                  #-- move return value at ./playo.lat:19:1 --#
addq $0, %RSP                                    #-- ./playo.lat:19:1 --#
leave                                            #-- ./playo.lat:19:1 --#
addq $8, %RSP                                    #-- ./playo.lat:19:1 --#
pop %RBX                                         #-- ./playo.lat:19:1 --#
ret                                              #-- ./playo.lat:19:1 --#
main :                                           #-- ./playo.lat:1:1 --#
__cl_TopLevel.main :                             #-- ./playo.lat:1:1 --#
push %R15                                        #-- ./playo.lat:1:1 --#
push %R14                                        #-- ./playo.lat:1:1 --#
push %R13                                        #-- ./playo.lat:1:1 --#
push %R12                                        #-- ./playo.lat:1:1 --#
push %RBX                                        #-- ./playo.lat:1:1 --#
subq $8, %RSP                                    #-- ./playo.lat:1:1 --#
push %RBP                                        #-- ./playo.lat:1:1 --#
movq %RSP, %RBP                                  #-- ./playo.lat:1:1 --#
subq $24, %RSP                                   #-- ./playo.lat:1:1 --#
__cl_TopLevel.main.L_entry :                     #-- ./playo.lat:1:1 --#
leaq __const_3 (%RIP), %RAX                      #-- ./playo.lat:2:15 --#
push %R11                                        #-- save caller saved at ./playo.lat:2:15 --#
push %RCX                                        #-- save caller saved at ./playo.lat:2:15 --#
push %RDX                                        #-- save caller saved at ./playo.lat:2:15 --#
push %RDI                                        #-- save caller saved at ./playo.lat:2:15 --#
push %RSI                                        #-- save caller saved at ./playo.lat:2:15 --#
push %R8                                         #-- save caller saved at ./playo.lat:2:15 --#
push %R9                                         #-- save caller saved at ./playo.lat:2:15 --#
push %R10                                        #-- save caller saved at ./playo.lat:2:15 --#
movq %RAX, %RDI                                  #-- passing arg at ./playo.lat:2:15 --#
subq $8, %RSP                                    #-- ./playo.lat:2:15 --#
call __createString                              #-- ./playo.lat:2:15 --#
addq $8, %RSP                                    #-- ./playo.lat:2:15 --#
movq %RAX, %RAX                                  #-- ./playo.lat:2:15 --#
pop %R10                                         #-- ./playo.lat:2:15 --#
pop %R9                                          #-- ./playo.lat:2:15 --#
pop %R8                                          #-- ./playo.lat:2:15 --#
pop %RSI                                         #-- ./playo.lat:2:15 --#
pop %RDI                                         #-- ./playo.lat:2:15 --#
pop %RDX                                         #-- ./playo.lat:2:15 --#
pop %RCX                                         #-- ./playo.lat:2:15 --#
pop %R11                                         #-- ./playo.lat:2:15 --#
push %R11                                        #-- save caller saved at ./playo.lat:2:3 --#
push %RCX                                        #-- save caller saved at ./playo.lat:2:3 --#
push %RDX                                        #-- save caller saved at ./playo.lat:2:3 --#
push %RDI                                        #-- save caller saved at ./playo.lat:2:3 --#
push %RSI                                        #-- save caller saved at ./playo.lat:2:3 --#
push %R8                                         #-- save caller saved at ./playo.lat:2:3 --#
push %R9                                         #-- save caller saved at ./playo.lat:2:3 --#
push %R10                                        #-- save caller saved at ./playo.lat:2:3 --#
movq %RAX, %RDI                                  #-- passing arg at ./playo.lat:2:3 --#
subq $8, %RSP                                    #-- ./playo.lat:2:3 --#
call printString                                 #-- ./playo.lat:2:3 --#
addq $8, %RSP                                    #-- ./playo.lat:2:3 --#
pop %R10                                         #-- ./playo.lat:2:3 --#
pop %R9                                          #-- ./playo.lat:2:3 --#
pop %R8                                          #-- ./playo.lat:2:3 --#
pop %RSI                                         #-- ./playo.lat:2:3 --#
pop %RDI                                         #-- ./playo.lat:2:3 --#
pop %RDX                                         #-- ./playo.lat:2:3 --#
pop %RCX                                         #-- ./playo.lat:2:3 --#
pop %R11                                         #-- ./playo.lat:2:3 --#
cmpl $0, %R11D                                   #-- ./playo.lat:3:13 --#
sete %AL                                         #-- ./playo.lat:3:13 --#
addq $0, %RSP                                    #-- ./playo.lat:3:13 --#
testb %AL, %AL                                   #-- ./playo.lat:3:13 --#
jz __cl_TopLevel.main_CAND8                      #-- ./playo.lat:3:13 --#
jmp __cl_TopLevel.main_CFALSE7__from_entry       #-- ./playo.lat:3:13 --#
__cl_TopLevel.main_CAND17 :                      #-- ./playo.lat:4:13 --#
cmpl $0, %R13D                                   #-- ./playo.lat:4:25 --#
sete %AL                                         #-- ./playo.lat:4:25 --#
addq $0, %RSP                                    #-- ./playo.lat:4:25 --#
testb %AL, %AL                                   #-- ./playo.lat:4:25 --#
jz __cl_TopLevel.main_CTRUE15                    #-- ./playo.lat:4:25 --#
jmp __cl_TopLevel.main_CFALSE16__from__CAND17    #-- ./playo.lat:4:25 --#
__cl_TopLevel.main_CAND26 :                      #-- ./playo.lat:5:13 --#
cmpl $0, %R15D                                   #-- ./playo.lat:5:24 --#
sete %AL                                         #-- ./playo.lat:5:24 --#
addq $0, %RSP                                    #-- ./playo.lat:5:24 --#
testb %AL, %AL                                   #-- ./playo.lat:5:24 --#
jz __cl_TopLevel.main_CTRUE24                    #-- ./playo.lat:5:24 --#
jmp __cl_TopLevel.main_CFALSE25__from__CAND26    #-- ./playo.lat:5:24 --#
__cl_TopLevel.main_CAND35 :                      #-- ./playo.lat:6:13 --#
cmpl $0, %EDX                                    #-- ./playo.lat:6:29 --#
sete %AL                                         #-- ./playo.lat:6:29 --#
addq $0, %RSP                                    #-- ./playo.lat:6:29 --#
testb %AL, %AL                                   #-- ./playo.lat:6:29 --#
jz __cl_TopLevel.main_CTRUE33                    #-- ./playo.lat:6:29 --#
jmp __cl_TopLevel.main_CFALSE34__from__CAND35    #-- ./playo.lat:6:29 --#
__cl_TopLevel.main_CAND8 :                       #-- ./playo.lat:3:13 --#
cmpl $0, %EBX                                    #-- ./playo.lat:3:25 --#
sete %AL                                         #-- ./playo.lat:3:25 --#
addq $0, %RSP                                    #-- ./playo.lat:3:25 --#
testb %AL, %AL                                   #-- ./playo.lat:3:25 --#
jz __cl_TopLevel.main_CTRUE6                     #-- ./playo.lat:3:25 --#
jmp __cl_TopLevel.main_CFALSE7__from__CAND8      #-- ./playo.lat:3:25 --#
__cl_TopLevel.main_CFALSE16 :                    #-- ./playo.lat:4:13 --#
push %RCX                                        #-- save caller saved at ./playo.lat:4:3 --#
push %RDX                                        #-- save caller saved at ./playo.lat:4:3 --#
push %RDI                                        #-- save caller saved at ./playo.lat:4:3 --#
push %RSI                                        #-- save caller saved at ./playo.lat:4:3 --#
push %R8                                         #-- save caller saved at ./playo.lat:4:3 --#
push %R9                                         #-- save caller saved at ./playo.lat:4:3 --#
push %R10                                        #-- save caller saved at ./playo.lat:4:3 --#
movl %EAX, %EDI                                  #-- passing arg at ./playo.lat:4:3 --#
subq $8, %RSP                                    #-- ./playo.lat:4:3 --#
call __cl_TopLevel.printBool                     #-- ./playo.lat:4:3 --#
addq $8, %RSP                                    #-- ./playo.lat:4:3 --#
pop %R10                                         #-- ./playo.lat:4:3 --#
pop %R9                                          #-- ./playo.lat:4:3 --#
pop %R8                                          #-- ./playo.lat:4:3 --#
pop %RSI                                         #-- ./playo.lat:4:3 --#
pop %RDI                                         #-- ./playo.lat:4:3 --#
pop %RDX                                         #-- ./playo.lat:4:3 --#
pop %RCX                                         #-- ./playo.lat:4:3 --#
cmpl $0, %R14D                                   #-- ./playo.lat:5:13 --#
sete %AL                                         #-- ./playo.lat:5:13 --#
addq $0, %RSP                                    #-- ./playo.lat:5:13 --#
testb %AL, %AL                                   #-- ./playo.lat:5:13 --#
jz __cl_TopLevel.main_CAND26                     #-- ./playo.lat:5:13 --#
jmp __cl_TopLevel.main_CFALSE25__from__CFALSE16  #-- ./playo.lat:5:13 --#
__cl_TopLevel.main_CFALSE16__from__CAND17 :      #-- ./playo.lat:4:13 --#
movl $0, %EAX                                    #-- setting %v_t_14~3 at ./playo.lat:4:13 --#
addq $0, %RSP                                    #-- ./playo.lat:4:13 --#
jmp __cl_TopLevel.main_CFALSE16                  #-- ./playo.lat:4:13 --#
__cl_TopLevel.main_CFALSE16__from__CFALSE7 :     #-- ./playo.lat:4:13 --#
movl $0, %EAX                                    #-- setting %v_t_14~3 at ./playo.lat:4:13 --#
addq $0, %RSP                                    #-- ./playo.lat:4:13 --#
jmp __cl_TopLevel.main_CFALSE16                  #-- ./playo.lat:4:13 --#
__cl_TopLevel.main_CFALSE25 :                    #-- ./playo.lat:5:13 --#
push %RCX                                        #-- save caller saved at ./playo.lat:5:3 --#
push %RDX                                        #-- save caller saved at ./playo.lat:5:3 --#
push %RDI                                        #-- save caller saved at ./playo.lat:5:3 --#
push %RSI                                        #-- save caller saved at ./playo.lat:5:3 --#
push %R8                                         #-- save caller saved at ./playo.lat:5:3 --#
push %R9                                         #-- save caller saved at ./playo.lat:5:3 --#
push %R10                                        #-- save caller saved at ./playo.lat:5:3 --#
movl %EAX, %EDI                                  #-- passing arg at ./playo.lat:5:3 --#
subq $8, %RSP                                    #-- ./playo.lat:5:3 --#
call __cl_TopLevel.printBool                     #-- ./playo.lat:5:3 --#
addq $8, %RSP                                    #-- ./playo.lat:5:3 --#
pop %R10                                         #-- ./playo.lat:5:3 --#
pop %R9                                          #-- ./playo.lat:5:3 --#
pop %R8                                          #-- ./playo.lat:5:3 --#
pop %RSI                                         #-- ./playo.lat:5:3 --#
pop %RDI                                         #-- ./playo.lat:5:3 --#
pop %RDX                                         #-- ./playo.lat:5:3 --#
pop %RCX                                         #-- ./playo.lat:5:3 --#
cmpl $0, %ECX                                    #-- ./playo.lat:6:13 --#
sete %AL                                         #-- ./playo.lat:6:13 --#
addq $0, %RSP                                    #-- ./playo.lat:6:13 --#
testb %AL, %AL                                   #-- ./playo.lat:6:13 --#
jz __cl_TopLevel.main_CAND35                     #-- ./playo.lat:6:13 --#
jmp __cl_TopLevel.main_CFALSE34__from__CFALSE25  #-- ./playo.lat:6:13 --#
__cl_TopLevel.main_CFALSE25__from__CAND26 :      #-- ./playo.lat:5:13 --#
movl $0, %EAX                                    #-- setting %v_t_23~3 at ./playo.lat:5:13 --#
addq $0, %RSP                                    #-- ./playo.lat:5:13 --#
jmp __cl_TopLevel.main_CFALSE25                  #-- ./playo.lat:5:13 --#
__cl_TopLevel.main_CFALSE25__from__CFALSE16 :    #-- ./playo.lat:5:13 --#
movl $0, %EAX                                    #-- setting %v_t_23~3 at ./playo.lat:5:13 --#
addq $0, %RSP                                    #-- ./playo.lat:5:13 --#
jmp __cl_TopLevel.main_CFALSE25                  #-- ./playo.lat:5:13 --#
__cl_TopLevel.main_CFALSE34 :                    #-- ./playo.lat:6:13 --#
push %RDI                                        #-- save caller saved at ./playo.lat:6:3 --#
push %RSI                                        #-- save caller saved at ./playo.lat:6:3 --#
push %R8                                         #-- save caller saved at ./playo.lat:6:3 --#
push %R9                                         #-- save caller saved at ./playo.lat:6:3 --#
push %R10                                        #-- save caller saved at ./playo.lat:6:3 --#
movl %EAX, %EDI                                  #-- passing arg at ./playo.lat:6:3 --#
subq $8, %RSP                                    #-- ./playo.lat:6:3 --#
call __cl_TopLevel.printBool                     #-- ./playo.lat:6:3 --#
addq $8, %RSP                                    #-- ./playo.lat:6:3 --#
pop %R10                                         #-- ./playo.lat:6:3 --#
pop %R9                                          #-- ./playo.lat:6:3 --#
pop %R8                                          #-- ./playo.lat:6:3 --#
pop %RSI                                         #-- ./playo.lat:6:3 --#
pop %RDI                                         #-- ./playo.lat:6:3 --#
leaq __const_4 (%RIP), %RAX                      #-- ./playo.lat:7:15 --#
push %RDI                                        #-- save caller saved at ./playo.lat:7:15 --#
push %RSI                                        #-- save caller saved at ./playo.lat:7:15 --#
push %R8                                         #-- save caller saved at ./playo.lat:7:15 --#
push %R9                                         #-- save caller saved at ./playo.lat:7:15 --#
push %R10                                        #-- save caller saved at ./playo.lat:7:15 --#
movq %RAX, %RDI                                  #-- passing arg at ./playo.lat:7:15 --#
subq $8, %RSP                                    #-- ./playo.lat:7:15 --#
call __createString                              #-- ./playo.lat:7:15 --#
addq $8, %RSP                                    #-- ./playo.lat:7:15 --#
movq %RAX, %RAX                                  #-- ./playo.lat:7:15 --#
pop %R10                                         #-- ./playo.lat:7:15 --#
pop %R9                                          #-- ./playo.lat:7:15 --#
pop %R8                                          #-- ./playo.lat:7:15 --#
pop %RSI                                         #-- ./playo.lat:7:15 --#
pop %RDI                                         #-- ./playo.lat:7:15 --#
push %RDI                                        #-- save caller saved at ./playo.lat:7:3 --#
push %RSI                                        #-- save caller saved at ./playo.lat:7:3 --#
push %R8                                         #-- save caller saved at ./playo.lat:7:3 --#
push %R9                                         #-- save caller saved at ./playo.lat:7:3 --#
push %R10                                        #-- save caller saved at ./playo.lat:7:3 --#
movq %RAX, %RDI                                  #-- passing arg at ./playo.lat:7:3 --#
subq $8, %RSP                                    #-- ./playo.lat:7:3 --#
call printString                                 #-- ./playo.lat:7:3 --#
addq $8, %RSP                                    #-- ./playo.lat:7:3 --#
pop %R10                                         #-- ./playo.lat:7:3 --#
pop %R9                                          #-- ./playo.lat:7:3 --#
pop %R8                                          #-- ./playo.lat:7:3 --#
pop %RSI                                         #-- ./playo.lat:7:3 --#
pop %RDI                                         #-- ./playo.lat:7:3 --#
cmpl $0, %EDI                                    #-- ./playo.lat:8:13 --#
sete %AL                                         #-- ./playo.lat:8:13 --#
addq $0, %RSP                                    #-- ./playo.lat:8:13 --#
testb %AL, %AL                                   #-- ./playo.lat:8:13 --#
jz __cl_TopLevel.main_CTRUE45                    #-- ./playo.lat:8:13 --#
jmp __cl_TopLevel.main_COR47                     #-- ./playo.lat:8:13 --#
__cl_TopLevel.main_CFALSE34__from__CAND35 :      #-- ./playo.lat:6:13 --#
movl $0, %EAX                                    #-- setting %v_t_32~3 at ./playo.lat:6:13 --#
addq $0, %RSP                                    #-- ./playo.lat:6:13 --#
jmp __cl_TopLevel.main_CFALSE34                  #-- ./playo.lat:6:13 --#
__cl_TopLevel.main_CFALSE34__from__CFALSE25 :    #-- ./playo.lat:6:13 --#
movl $0, %EAX                                    #-- setting %v_t_32~3 at ./playo.lat:6:13 --#
addq $0, %RSP                                    #-- ./playo.lat:6:13 --#
jmp __cl_TopLevel.main_CFALSE34                  #-- ./playo.lat:6:13 --#
__cl_TopLevel.main_CFALSE46 :                    #-- ./playo.lat:8:13 --#
push %R8                                         #-- save caller saved at ./playo.lat:8:3 --#
push %R9                                         #-- save caller saved at ./playo.lat:8:3 --#
push %R10                                        #-- save caller saved at ./playo.lat:8:3 --#
movl %EAX, %EDI                                  #-- passing arg at ./playo.lat:8:3 --#
subq $8, %RSP                                    #-- ./playo.lat:8:3 --#
call __cl_TopLevel.printBool                     #-- ./playo.lat:8:3 --#
addq $8, %RSP                                    #-- ./playo.lat:8:3 --#
pop %R10                                         #-- ./playo.lat:8:3 --#
pop %R9                                          #-- ./playo.lat:8:3 --#
pop %R8                                          #-- ./playo.lat:8:3 --#
cmpl $0, %R8D                                    #-- ./playo.lat:9:13 --#
sete %AL                                         #-- ./playo.lat:9:13 --#
addq $0, %RSP                                    #-- ./playo.lat:9:13 --#
testb %AL, %AL                                   #-- ./playo.lat:9:13 --#
jz __cl_TopLevel.main_CTRUE54                    #-- ./playo.lat:9:13 --#
jmp __cl_TopLevel.main_COR56                     #-- ./playo.lat:9:13 --#
__cl_TopLevel.main_CFALSE46__from__COR47 :       #-- ./playo.lat:8:13 --#
movl $0, %EAX                                    #-- setting %v_t_44~3 at ./playo.lat:8:13 --#
addq $0, %RSP                                    #-- ./playo.lat:8:13 --#
jmp __cl_TopLevel.main_CFALSE46                  #-- ./playo.lat:8:13 --#
__cl_TopLevel.main_CFALSE55 :                    #-- ./playo.lat:9:13 --#
push %R10                                        #-- save caller saved at ./playo.lat:9:3 --#
movl %EAX, %EDI                                  #-- passing arg at ./playo.lat:9:3 --#
subq $8, %RSP                                    #-- ./playo.lat:9:3 --#
call __cl_TopLevel.printBool                     #-- ./playo.lat:9:3 --#
addq $8, %RSP                                    #-- ./playo.lat:9:3 --#
pop %R10                                         #-- ./playo.lat:9:3 --#
cmpl $0, %R10D                                   #-- ./playo.lat:10:13 --#
sete %AL                                         #-- ./playo.lat:10:13 --#
addq $0, %RSP                                    #-- ./playo.lat:10:13 --#
testb %AL, %AL                                   #-- ./playo.lat:10:13 --#
jz __cl_TopLevel.main_CTRUE63                    #-- ./playo.lat:10:13 --#
jmp __cl_TopLevel.main_COR65                     #-- ./playo.lat:10:13 --#
__cl_TopLevel.main_CFALSE55__from__COR56 :       #-- ./playo.lat:9:13 --#
movl $0, %EAX                                    #-- setting %v_t_53~3 at ./playo.lat:9:13 --#
addq $0, %RSP                                    #-- ./playo.lat:9:13 --#
jmp __cl_TopLevel.main_CFALSE55                  #-- ./playo.lat:9:13 --#
__cl_TopLevel.main_CFALSE64 :                    #-- ./playo.lat:10:13 --#
movl %EAX, %EDI                                  #-- passing arg at ./playo.lat:10:3 --#
subq $8, %RSP                                    #-- ./playo.lat:10:3 --#
call __cl_TopLevel.printBool                     #-- ./playo.lat:10:3 --#
addq $8, %RSP                                    #-- ./playo.lat:10:3 --#
movl -16 (%RBP), %EAX                            #-- load %v_t_76~loc_1_0 at ./playo.lat:11:13 --#
cmpl $0, %EAX                                    #-- ./playo.lat:11:13 --#
sete %AL                                         #-- ./playo.lat:11:13 --#
addq $0, %RSP                                    #-- ./playo.lat:11:13 --#
testb %AL, %AL                                   #-- ./playo.lat:11:13 --#
jz __cl_TopLevel.main_CTRUE72                    #-- ./playo.lat:11:13 --#
jmp __cl_TopLevel.main_COR74                     #-- ./playo.lat:11:13 --#
__cl_TopLevel.main_CFALSE64__from__COR65 :       #-- ./playo.lat:10:13 --#
movl $0, %EAX                                    #-- setting %v_t_62~3 at ./playo.lat:10:13 --#
addq $0, %RSP                                    #-- ./playo.lat:10:13 --#
jmp __cl_TopLevel.main_CFALSE64                  #-- ./playo.lat:10:13 --#
__cl_TopLevel.main_CFALSE7 :                     #-- ./playo.lat:3:13 --#
push %RCX                                        #-- save caller saved at ./playo.lat:3:3 --#
push %RDX                                        #-- save caller saved at ./playo.lat:3:3 --#
push %RDI                                        #-- save caller saved at ./playo.lat:3:3 --#
push %RSI                                        #-- save caller saved at ./playo.lat:3:3 --#
push %R8                                         #-- save caller saved at ./playo.lat:3:3 --#
push %R9                                         #-- save caller saved at ./playo.lat:3:3 --#
push %R10                                        #-- save caller saved at ./playo.lat:3:3 --#
movl %EAX, %EDI                                  #-- passing arg at ./playo.lat:3:3 --#
subq $8, %RSP                                    #-- ./playo.lat:3:3 --#
call __cl_TopLevel.printBool                     #-- ./playo.lat:3:3 --#
addq $8, %RSP                                    #-- ./playo.lat:3:3 --#
pop %R10                                         #-- ./playo.lat:3:3 --#
pop %R9                                          #-- ./playo.lat:3:3 --#
pop %R8                                          #-- ./playo.lat:3:3 --#
pop %RSI                                         #-- ./playo.lat:3:3 --#
pop %RDI                                         #-- ./playo.lat:3:3 --#
pop %RDX                                         #-- ./playo.lat:3:3 --#
pop %RCX                                         #-- ./playo.lat:3:3 --#
cmpl $0, %R12D                                   #-- ./playo.lat:4:13 --#
sete %AL                                         #-- ./playo.lat:4:13 --#
addq $0, %RSP                                    #-- ./playo.lat:4:13 --#
testb %AL, %AL                                   #-- ./playo.lat:4:13 --#
jz __cl_TopLevel.main_CAND17                     #-- ./playo.lat:4:13 --#
jmp __cl_TopLevel.main_CFALSE16__from__CFALSE7   #-- ./playo.lat:4:13 --#
__cl_TopLevel.main_CFALSE73__from__COR74 :       #-- ./playo.lat:11:13 --#
movl $0, %EAX                                    #-- setting %v_t_71~3 at ./playo.lat:11:13 --#
addq $0, %RSP                                    #-- ./playo.lat:11:13 --#
jmp __cl_TopLevel.main_CFALSE73                  #-- ./playo.lat:11:13 --#
__cl_TopLevel.main_CFALSE7__from__CAND8 :        #-- ./playo.lat:3:13 --#
movl $0, %EAX                                    #-- setting %v_t_5~3 at ./playo.lat:3:13 --#
addq $0, %RSP                                    #-- ./playo.lat:3:13 --#
jmp __cl_TopLevel.main_CFALSE7                   #-- ./playo.lat:3:13 --#
__cl_TopLevel.main_CFALSE7__from_entry :         #-- ./playo.lat:3:13 --#
movl $0, %EAX                                    #-- setting %v_t_5~3 at ./playo.lat:3:13 --#
addq $0, %RSP                                    #-- ./playo.lat:3:13 --#
jmp __cl_TopLevel.main_CFALSE7                   #-- ./playo.lat:3:13 --#
__cl_TopLevel.main_COR47 :                       #-- ./playo.lat:8:13 --#
cmpl $0, %ESI                                    #-- ./playo.lat:8:25 --#
sete %AL                                         #-- ./playo.lat:8:25 --#
addq $0, %RSP                                    #-- ./playo.lat:8:25 --#
testb %AL, %AL                                   #-- ./playo.lat:8:25 --#
jz __cl_TopLevel.main_CTRUE45                    #-- ./playo.lat:8:25 --#
jmp __cl_TopLevel.main_CFALSE46__from__COR47     #-- ./playo.lat:8:25 --#
__cl_TopLevel.main_COR56 :                       #-- ./playo.lat:9:13 --#
cmpl $0, %R9D                                    #-- ./playo.lat:9:25 --#
sete %AL                                         #-- ./playo.lat:9:25 --#
addq $0, %RSP                                    #-- ./playo.lat:9:25 --#
testb %AL, %AL                                   #-- ./playo.lat:9:25 --#
jz __cl_TopLevel.main_CTRUE54                    #-- ./playo.lat:9:25 --#
jmp __cl_TopLevel.main_CFALSE55__from__COR56     #-- ./playo.lat:9:25 --#
__cl_TopLevel.main_COR65 :                       #-- ./playo.lat:10:13 --#
movl -24 (%RBP), %EAX                            #-- load %v_t_69~loc_2_0 at ./playo.lat:10:24 --#
cmpl $0, %EAX                                    #-- ./playo.lat:10:24 --#
sete %AL                                         #-- ./playo.lat:10:24 --#
addq $0, %RSP                                    #-- ./playo.lat:10:24 --#
testb %AL, %AL                                   #-- ./playo.lat:10:24 --#
jz __cl_TopLevel.main_CTRUE63                    #-- ./playo.lat:10:24 --#
jmp __cl_TopLevel.main_CFALSE64__from__COR65     #-- ./playo.lat:10:24 --#
__cl_TopLevel.main_COR74 :                       #-- ./playo.lat:11:13 --#
movl -8 (%RBP), %EAX                             #-- load %v_t_78~loc_0_0 at ./playo.lat:11:29 --#
cmpl $0, %EAX                                    #-- ./playo.lat:11:29 --#
sete %AL                                         #-- ./playo.lat:11:29 --#
addq $0, %RSP                                    #-- ./playo.lat:11:29 --#
testb %AL, %AL                                   #-- ./playo.lat:11:29 --#
jz __cl_TopLevel.main_CTRUE72                    #-- ./playo.lat:11:29 --#
jmp __cl_TopLevel.main_CFALSE73__from__COR74     #-- ./playo.lat:11:29 --#
__cl_TopLevel.main_CTRUE15 :                     #-- ./playo.lat:4:13 --#
movl $1, %EAX                                    #-- setting %v_t_14~3 at ./playo.lat:4:13 --#
addq $0, %RSP                                    #-- ./playo.lat:4:13 --#
jmp __cl_TopLevel.main_CFALSE16                  #-- ./playo.lat:4:13 --#
__cl_TopLevel.main_CTRUE24 :                     #-- ./playo.lat:5:13 --#
movl $1, %EAX                                    #-- setting %v_t_23~3 at ./playo.lat:5:13 --#
addq $0, %RSP                                    #-- ./playo.lat:5:13 --#
jmp __cl_TopLevel.main_CFALSE25                  #-- ./playo.lat:5:13 --#
__cl_TopLevel.main_CTRUE33 :                     #-- ./playo.lat:6:13 --#
movl $1, %EAX                                    #-- setting %v_t_32~3 at ./playo.lat:6:13 --#
addq $0, %RSP                                    #-- ./playo.lat:6:13 --#
jmp __cl_TopLevel.main_CFALSE34                  #-- ./playo.lat:6:13 --#
__cl_TopLevel.main_CTRUE45 :                     #-- ./playo.lat:8:13 --#
movl $1, %EAX                                    #-- setting %v_t_44~3 at ./playo.lat:8:13 --#
addq $0, %RSP                                    #-- ./playo.lat:8:13 --#
jmp __cl_TopLevel.main_CFALSE46                  #-- ./playo.lat:8:13 --#
__cl_TopLevel.main_CTRUE54 :                     #-- ./playo.lat:9:13 --#
movl $1, %EAX                                    #-- setting %v_t_53~3 at ./playo.lat:9:13 --#
addq $0, %RSP                                    #-- ./playo.lat:9:13 --#
jmp __cl_TopLevel.main_CFALSE55                  #-- ./playo.lat:9:13 --#
__cl_TopLevel.main_CTRUE6 :                      #-- ./playo.lat:3:13 --#
movl $1, %EAX                                    #-- setting %v_t_5~3 at ./playo.lat:3:13 --#
addq $0, %RSP                                    #-- ./playo.lat:3:13 --#
jmp __cl_TopLevel.main_CFALSE7                   #-- ./playo.lat:3:13 --#
__cl_TopLevel.main_CTRUE63 :                     #-- ./playo.lat:10:13 --#
movl $1, %EAX                                    #-- setting %v_t_62~3 at ./playo.lat:10:13 --#
addq $0, %RSP                                    #-- ./playo.lat:10:13 --#
jmp __cl_TopLevel.main_CFALSE64                  #-- ./playo.lat:10:13 --#
__cl_TopLevel.main_CTRUE72 :                     #-- ./playo.lat:11:13 --#
movl $1, %EAX                                    #-- setting %v_t_71~3 at ./playo.lat:11:13 --#
addq $0, %RSP                                    #-- ./playo.lat:11:13 --#
jmp __cl_TopLevel.main_CFALSE73                  #-- ./playo.lat:11:13 --#
__cl_TopLevel.main_CFALSE73 :                    #-- ./playo.lat:11:13 --#
movl %EAX, %EDI                                  #-- passing arg at ./playo.lat:11:3 --#
subq $8, %RSP                                    #-- ./playo.lat:11:3 --#
call __cl_TopLevel.printBool                     #-- ./playo.lat:11:3 --#
addq $8, %RSP                                    #-- ./playo.lat:11:3 --#
leaq __const_5 (%RIP), %RAX                      #-- ./playo.lat:12:15 --#
movq %RAX, %RDI                                  #-- passing arg at ./playo.lat:12:15 --#
subq $8, %RSP                                    #-- ./playo.lat:12:15 --#
call __createString                              #-- ./playo.lat:12:15 --#
addq $8, %RSP                                    #-- ./playo.lat:12:15 --#
movq %RAX, %RAX                                  #-- ./playo.lat:12:15 --#
movq %RAX, %RDI                                  #-- passing arg at ./playo.lat:12:3 --#
subq $8, %RSP                                    #-- ./playo.lat:12:3 --#
call printString                                 #-- ./playo.lat:12:3 --#
addq $8, %RSP                                    #-- ./playo.lat:12:3 --#
movl $1, %EDI                                    #-- passing arg at ./playo.lat:13:3 --#
subq $8, %RSP                                    #-- ./playo.lat:13:3 --#
call __cl_TopLevel.printBool                     #-- ./playo.lat:13:3 --#
addq $8, %RSP                                    #-- ./playo.lat:13:3 --#
movl $0, %EDI                                    #-- passing arg at ./playo.lat:14:3 --#
subq $8, %RSP                                    #-- ./playo.lat:14:3 --#
call __cl_TopLevel.printBool                     #-- ./playo.lat:14:3 --#
addq $8, %RSP                                    #-- ./playo.lat:14:3 --#
movl $0, %EAX                                    #-- move return value at ./playo.lat:1:1 --#
addq $0, %RSP                                    #-- ./playo.lat:1:1 --#
leave                                            #-- ./playo.lat:1:1 --#
addq $8, %RSP                                    #-- ./playo.lat:1:1 --#
pop %R15                                         #-- ./playo.lat:1:1 --#
pop %R14                                         #-- ./playo.lat:1:1 --#
pop %R13                                         #-- ./playo.lat:1:1 --#
pop %R12                                         #-- ./playo.lat:1:1 --#
pop %RBX                                         #-- ./playo.lat:1:1 --#
ret                                              #-- ./playo.lat:1:1 --#
__errorNull :                                    #-- runtime error on null dereference at ./playo.lat:1:1 --#
andq $-16, %RSP                                  #-- 16 bytes allign at ./playo.lat:1:1 --#
call __errorNull                                 #-- ./playo.lat:1:1 --#
