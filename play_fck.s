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
__const_3 :
.string "&&"

__const_2 :
.string "false"

__const_1 :
.string "true"

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
__cl_TopLevel.test :                             #-- ./play_fck.lat:20:1 --#
push %RBX                                        #-- ./play_fck.lat:20:1 --#
subq $8, %RSP                                    #-- ./play_fck.lat:20:1 --#
push %RBP                                        #-- ./play_fck.lat:20:1 --#
movq %RSP, %RBP                                  #-- ./play_fck.lat:20:1 --#
subq $0, %RSP                                    #-- ./play_fck.lat:20:1 --#
__cl_TopLevel.test.L_entry :                     #-- ./play_fck.lat:20:1 --#
movl %EDI, %EBX                                  #-- load %v_t_43 at ./play_fck.lat:20:1 --#
movl %EBX, %EDI                                  #-- passing arg at ./play_fck.lat:21:3 --#
subq $0, %RSP                                    #-- ./play_fck.lat:21:3 --#
call printInt                                    #-- ./play_fck.lat:21:3 --#
addq $0, %RSP                                    #-- ./play_fck.lat:21:3 --#
cmpl $0, %EBX                                    #-- ./play_fck.lat:22:10 --#
setg %AL                                         #-- ./play_fck.lat:22:12 --#
addq $0, %RSP                                    #-- ./play_fck.lat:22:10 --#
testb %AL, %AL                                   #-- ./play_fck.lat:22:10 --#
jz __cl_TopLevel.test_IELSE46                    #-- ./play_fck.lat:22:10 --#
jmp __cl_TopLevel.test_IIF45                     #-- ./play_fck.lat:22:10 --#
__cl_TopLevel.test_IELSE46 :                     #-- ./play_fck.lat:22:3 --#
movl $0, %EAX                                    #-- setting %v_return~2 at ./play_fck.lat:20:1 --#
addq $0, %RSP                                    #-- ./play_fck.lat:20:1 --#
jmp __cl_TopLevel.test.L_exit                    #-- ./play_fck.lat:20:1 --#
__cl_TopLevel.test_IIF45 :                       #-- ./play_fck.lat:22:3 --#
movl $1, %EAX                                    #-- setting %v_return~2 at ./play_fck.lat:20:1 --#
addq $0, %RSP                                    #-- ./play_fck.lat:20:1 --#
jmp __cl_TopLevel.test.L_exit                    #-- ./play_fck.lat:20:1 --#
__cl_TopLevel.test.L_exit :                      #-- ./play_fck.lat:20:1 --#
movl %EAX, %EAX                                  #-- move return value at ./play_fck.lat:20:1 --#
addq $0, %RSP                                    #-- ./play_fck.lat:20:1 --#
leave                                            #-- ./play_fck.lat:20:1 --#
addq $8, %RSP                                    #-- ./play_fck.lat:20:1 --#
pop %RBX                                         #-- ./play_fck.lat:20:1 --#
ret                                              #-- ./play_fck.lat:20:1 --#
__cl_TopLevel.printBool :                        #-- ./play_fck.lat:11:1 --#
push %RBX                                        #-- ./play_fck.lat:11:1 --#
subq $8, %RSP                                    #-- ./play_fck.lat:11:1 --#
push %RBP                                        #-- ./play_fck.lat:11:1 --#
movq %RSP, %RBP                                  #-- ./play_fck.lat:11:1 --#
subq $0, %RSP                                    #-- ./play_fck.lat:11:1 --#
__cl_TopLevel.printBool.L_entry :                #-- ./play_fck.lat:11:1 --#
movl %EDI, %EAX                                  #-- load %v_t_33 at ./play_fck.lat:11:1 --#
cmpl $0, %EAX                                    #-- ./play_fck.lat:12:8 --#
sete %AL                                         #-- ./play_fck.lat:12:8 --#
addq $0, %RSP                                    #-- ./play_fck.lat:12:8 --#
testb %AL, %AL                                   #-- ./play_fck.lat:12:8 --#
jz __cl_TopLevel.printBool_IELSE35               #-- ./play_fck.lat:12:8 --#
jmp __cl_TopLevel.printBool_IIF34                #-- ./play_fck.lat:12:8 --#
__cl_TopLevel.printBool_IELSE35 :                #-- ./play_fck.lat:12:3 --#
leaq __const_1 (%RIP), %RAX                      #-- ./play_fck.lat:15:17 --#
movq %RAX, %RDI                                  #-- passing arg at ./play_fck.lat:15:17 --#
subq $0, %RSP                                    #-- ./play_fck.lat:15:17 --#
call __createString                              #-- ./play_fck.lat:15:17 --#
addq $0, %RSP                                    #-- ./play_fck.lat:15:17 --#
movq %RAX, %RAX                                  #-- ./play_fck.lat:15:17 --#
movq %RAX, %RDI                                  #-- passing arg at ./play_fck.lat:15:5 --#
subq $0, %RSP                                    #-- ./play_fck.lat:15:5 --#
call printString                                 #-- ./play_fck.lat:15:5 --#
addq $0, %RSP                                    #-- ./play_fck.lat:15:5 --#
addq $0, %RSP                                    #-- ./play_fck.lat:12:3 --#
jmp __cl_TopLevel.printBool_IEND36               #-- ./play_fck.lat:12:3 --#
__cl_TopLevel.printBool_IIF34 :                  #-- ./play_fck.lat:12:3 --#
leaq __const_2 (%RIP), %RAX                      #-- ./play_fck.lat:13:17 --#
movq %RAX, %RDI                                  #-- passing arg at ./play_fck.lat:13:17 --#
subq $0, %RSP                                    #-- ./play_fck.lat:13:17 --#
call __createString                              #-- ./play_fck.lat:13:17 --#
addq $0, %RSP                                    #-- ./play_fck.lat:13:17 --#
movq %RAX, %RAX                                  #-- ./play_fck.lat:13:17 --#
movq %RAX, %RDI                                  #-- passing arg at ./play_fck.lat:13:5 --#
subq $0, %RSP                                    #-- ./play_fck.lat:13:5 --#
call printString                                 #-- ./play_fck.lat:13:5 --#
addq $0, %RSP                                    #-- ./play_fck.lat:13:5 --#
addq $0, %RSP                                    #-- ./play_fck.lat:12:3 --#
jmp __cl_TopLevel.printBool_IEND36               #-- ./play_fck.lat:12:3 --#
__cl_TopLevel.printBool_IEND36 :                 #-- ./play_fck.lat:12:3 --#
movl %EBX, %EAX                                  #-- move return value at ./play_fck.lat:11:1 --#
addq $0, %RSP                                    #-- ./play_fck.lat:11:1 --#
leave                                            #-- ./play_fck.lat:11:1 --#
addq $8, %RSP                                    #-- ./play_fck.lat:11:1 --#
pop %RBX                                         #-- ./play_fck.lat:11:1 --#
ret                                              #-- ./play_fck.lat:11:1 --#
main :                                           #-- ./play_fck.lat:1:1 --#
__cl_TopLevel.main :                             #-- ./play_fck.lat:1:1 --#
push %R15                                        #-- ./play_fck.lat:1:1 --#
push %R14                                        #-- ./play_fck.lat:1:1 --#
push %R13                                        #-- ./play_fck.lat:1:1 --#
push %R12                                        #-- ./play_fck.lat:1:1 --#
push %RBX                                        #-- ./play_fck.lat:1:1 --#
subq $64, %RSP                                    #-- ./play_fck.lat:1:1 --#
push %RBP                                        #-- ./play_fck.lat:1:1 --#
movq %RSP, %RBP                                  #-- ./play_fck.lat:1:1 --#
subq $0, %RSP                                    #-- ./play_fck.lat:1:1 --#
__cl_TopLevel.main.L_entry :                     #-- ./play_fck.lat:1:1 --#
leaq __const_3 (%RIP), %RAX                      #-- ./play_fck.lat:2:15 --#
push %RCX                                        #-- save caller saved at ./play_fck.lat:2:15 --#
movq %RAX, %RDI                                  #-- passing arg at ./play_fck.lat:2:15 --#
subq $0, %RSP                                    #-- ./play_fck.lat:2:15 --#
call __createString                              #-- ./play_fck.lat:2:15 --#
addq $0, %RSP                                    #-- ./play_fck.lat:2:15 --#
movq %RAX, %RAX                                  #-- ./play_fck.lat:2:15 --#
pop %RCX                                         #-- ./play_fck.lat:2:15 --#
push %RCX                                        #-- save caller saved at ./play_fck.lat:2:3 --#
movq %RAX, %RDI                                  #-- passing arg at ./play_fck.lat:2:3 --#
subq $0, %RSP                                    #-- ./play_fck.lat:2:3 --#
call printString                                 #-- ./play_fck.lat:2:3 --#
addq $0, %RSP                                    #-- ./play_fck.lat:2:3 --#
pop %RCX                                         #-- ./play_fck.lat:2:3 --#
cmpl $0, %ECX                                    #-- ./play_fck.lat:3:13 --#
sete %AL                                         #-- ./play_fck.lat:3:13 --#
addq $0, %RSP                                    #-- ./play_fck.lat:3:13 --#
testb %AL, %AL                                   #-- ./play_fck.lat:3:13 --#
jz __cl_TopLevel.main_CAND8                      #-- ./play_fck.lat:3:13 --#
jmp __cl_TopLevel.main_CFALSE7__from_entry       #-- ./play_fck.lat:3:13 --#
__cl_TopLevel.main_CAND17 :                      #-- ./play_fck.lat:4:13 --#
cmpl $0, %R13D                                   #-- ./play_fck.lat:4:25 --#
sete %AL                                         #-- ./play_fck.lat:4:25 --#
addq $0, %RSP                                    #-- ./play_fck.lat:4:25 --#
testb %AL, %AL                                   #-- ./play_fck.lat:4:25 --#
jz __cl_TopLevel.main_CTRUE15                    #-- ./play_fck.lat:4:25 --#
jmp __cl_TopLevel.main_CFALSE16__from__CAND17    #-- ./play_fck.lat:4:25 --#
__cl_TopLevel.main_CAND26 :                      #-- ./play_fck.lat:5:13 --#
cmpl $0, %R15D                                   #-- ./play_fck.lat:5:24 --#
sete %AL                                         #-- ./play_fck.lat:5:24 --#
addq $0, %RSP                                    #-- ./play_fck.lat:5:24 --#
testb %AL, %AL                                   #-- ./play_fck.lat:5:24 --#
jz __cl_TopLevel.main_CTRUE24                    #-- ./play_fck.lat:5:24 --#
jmp __cl_TopLevel.main_CFALSE25__from__CAND26    #-- ./play_fck.lat:5:24 --#
__cl_TopLevel.main_CAND8 :                       #-- ./play_fck.lat:3:13 --#
cmpl $0, %EBX                                    #-- ./play_fck.lat:3:25 --#
sete %AL                                         #-- ./play_fck.lat:3:25 --#
addq $0, %RSP                                    #-- ./play_fck.lat:3:25 --#
testb %AL, %AL                                   #-- ./play_fck.lat:3:25 --#
jz __cl_TopLevel.main_CTRUE6                     #-- ./play_fck.lat:3:25 --#
jmp __cl_TopLevel.main_CFALSE7__from__CAND8      #-- ./play_fck.lat:3:25 --#
__cl_TopLevel.main_CFALSE16 :                    #-- ./play_fck.lat:4:13 --#
movl %EAX, %EDI                                  #-- passing arg at ./play_fck.lat:4:3 --#
subq $0, %RSP                                    #-- ./play_fck.lat:4:3 --#
call __cl_TopLevel.printBool                     #-- ./play_fck.lat:4:3 --#
addq $0, %RSP                                    #-- ./play_fck.lat:4:3 --#
cmpl $0, %R14D                                   #-- ./play_fck.lat:5:13 --#
sete %AL                                         #-- ./play_fck.lat:5:13 --#
addq $0, %RSP                                    #-- ./play_fck.lat:5:13 --#
testb %AL, %AL                                   #-- ./play_fck.lat:5:13 --#
jz __cl_TopLevel.main_CAND26                     #-- ./play_fck.lat:5:13 --#
jmp __cl_TopLevel.main_CFALSE25__from__CFALSE16  #-- ./play_fck.lat:5:13 --#
__cl_TopLevel.main_CFALSE16__from__CAND17 :      #-- ./play_fck.lat:4:13 --#
movl $0, %EAX                                    #-- setting %v_t_14~3 at ./play_fck.lat:4:13 --#
addq $0, %RSP                                    #-- ./play_fck.lat:4:13 --#
jmp __cl_TopLevel.main_CFALSE16                  #-- ./play_fck.lat:4:13 --#
__cl_TopLevel.main_CFALSE16__from__CFALSE7 :     #-- ./play_fck.lat:4:13 --#
movl $0, %EAX                                    #-- setting %v_t_14~3 at ./play_fck.lat:4:13 --#
addq $0, %RSP                                    #-- ./play_fck.lat:4:13 --#
jmp __cl_TopLevel.main_CFALSE16                  #-- ./play_fck.lat:4:13 --#
__cl_TopLevel.main_CFALSE25__from__CAND26 :      #-- ./play_fck.lat:5:13 --#
movl $0, %EAX                                    #-- setting %v_t_23~3 at ./play_fck.lat:5:13 --#
addq $0, %RSP                                    #-- ./play_fck.lat:5:13 --#
jmp __cl_TopLevel.main_CFALSE25                  #-- ./play_fck.lat:5:13 --#
__cl_TopLevel.main_CFALSE25__from__CFALSE16 :    #-- ./play_fck.lat:5:13 --#
movl $0, %EAX                                    #-- setting %v_t_23~3 at ./play_fck.lat:5:13 --#
addq $0, %RSP                                    #-- ./play_fck.lat:5:13 --#
jmp __cl_TopLevel.main_CFALSE25                  #-- ./play_fck.lat:5:13 --#
__cl_TopLevel.main_CFALSE7 :                     #-- ./play_fck.lat:3:13 --#
movl %EAX, %EDI                                  #-- passing arg at ./play_fck.lat:3:3 --#
subq $0, %RSP                                    #-- ./play_fck.lat:3:3 --#
call __cl_TopLevel.printBool                     #-- ./play_fck.lat:3:3 --#
addq $0, %RSP                                    #-- ./play_fck.lat:3:3 --#
cmpl $0, %R12D                                   #-- ./play_fck.lat:4:13 --#
sete %AL                                         #-- ./play_fck.lat:4:13 --#
addq $0, %RSP                                    #-- ./play_fck.lat:4:13 --#
testb %AL, %AL                                   #-- ./play_fck.lat:4:13 --#
jz __cl_TopLevel.main_CAND17                     #-- ./play_fck.lat:4:13 --#
jmp __cl_TopLevel.main_CFALSE16__from__CFALSE7   #-- ./play_fck.lat:4:13 --#
__cl_TopLevel.main_CFALSE7__from__CAND8 :        #-- ./play_fck.lat:3:13 --#
movl $0, %EAX                                    #-- setting %v_t_5~3 at ./play_fck.lat:3:13 --#
addq $0, %RSP                                    #-- ./play_fck.lat:3:13 --#
jmp __cl_TopLevel.main_CFALSE7                   #-- ./play_fck.lat:3:13 --#
__cl_TopLevel.main_CFALSE7__from_entry :         #-- ./play_fck.lat:3:13 --#
movl $0, %EAX                                    #-- setting %v_t_5~3 at ./play_fck.lat:3:13 --#
addq $0, %RSP                                    #-- ./play_fck.lat:3:13 --#
jmp __cl_TopLevel.main_CFALSE7                   #-- ./play_fck.lat:3:13 --#
__cl_TopLevel.main_CTRUE15 :                     #-- ./play_fck.lat:4:13 --#
movl $1, %EAX                                    #-- setting %v_t_14~3 at ./play_fck.lat:4:13 --#
addq $0, %RSP                                    #-- ./play_fck.lat:4:13 --#
jmp __cl_TopLevel.main_CFALSE16                  #-- ./play_fck.lat:4:13 --#
__cl_TopLevel.main_CTRUE24 :                     #-- ./play_fck.lat:5:13 --#
movl $1, %EAX                                    #-- setting %v_t_23~3 at ./play_fck.lat:5:13 --#
addq $0, %RSP                                    #-- ./play_fck.lat:5:13 --#
jmp __cl_TopLevel.main_CFALSE25                  #-- ./play_fck.lat:5:13 --#
__cl_TopLevel.main_CTRUE6 :                      #-- ./play_fck.lat:3:13 --#
movl $1, %EAX                                    #-- setting %v_t_5~3 at ./play_fck.lat:3:13 --#
addq $0, %RSP                                    #-- ./play_fck.lat:3:13 --#
jmp __cl_TopLevel.main_CFALSE7                   #-- ./play_fck.lat:3:13 --#
__cl_TopLevel.main_CFALSE25 :                    #-- ./play_fck.lat:5:13 --#
movl %EAX, %EDI                                  #-- passing arg at ./play_fck.lat:5:3 --#
subq $0, %RSP                                    #-- ./play_fck.lat:5:3 --#
call __cl_TopLevel.printBool                     #-- ./play_fck.lat:5:3 --#
addq $0, %RSP                                    #-- ./play_fck.lat:5:3 --#
movl $0, %EAX                                    #-- move return value at ./play_fck.lat:1:1 --#
addq $0, %RSP                                    #-- ./play_fck.lat:1:1 --#
leave                                            #-- ./play_fck.lat:1:1 --#
addq $64, %RSP                                    #-- ./play_fck.lat:1:1 --#
pop %R15                                         #-- ./play_fck.lat:1:1 --#
pop %R14                                         #-- ./play_fck.lat:1:1 --#
pop %R13                                         #-- ./play_fck.lat:1:1 --#
pop %R12                                         #-- ./play_fck.lat:1:1 --#
pop %RBX                                         #-- ./play_fck.lat:1:1 --#
ret                                              #-- ./play_fck.lat:1:1 --#
__errorNull :                                    #-- runtime error on null dereference at ./play_fck.lat:1:1 --#
andq $-16, %RSP                                  #-- 16 bytes allign at ./play_fck.lat:1:1 --#
call __errorNull                                 #-- ./play_fck.lat:1:1 --#
