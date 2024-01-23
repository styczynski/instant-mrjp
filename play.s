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
__const_2 :
.string "*"

__const_4 :
.string "+"

__const_3 :
.string "-"

__const_1 :
.string "/"

__const_6 :
.string "Node.value not implemented"

__const_5 :
.string "Operator.operator not implemented"

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

.global _class_Liczba
_class_Liczba :
.quad _class_Node
.long 8
.quad _class_Liczba_methods
.long 0
.quad 0

.global _class_Liczba_methods
_class_Liczba_methods :
.quad _Object_equals
.quad _Object_getHashCode
.quad _Object_toString
.quad Liczba.value

.global _class_Minus
_class_Minus :
.quad _class_Operator
.long 16
.quad _class_Minus_methods
.long 0
.quad 0

.global _class_Minus_methods
_class_Minus_methods :
.quad _Object_equals
.quad _Object_getHashCode
.quad _Object_toString
.quad Operator.value
.quad Minus.operator

.global _class_Node
_class_Node :
.quad _class_Object
.long 0
.quad _class_Node_methods
.long 0
.quad 0

.global _class_Node_methods
_class_Node_methods :
.quad _Object_equals
.quad _Object_getHashCode
.quad _Object_toString
.quad Node.value

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

.global _class_Operator
_class_Operator :
.quad _class_Node
.long 16
.quad _class_Operator_methods
.long 0
.quad 0

.global _class_Operator_methods
_class_Operator_methods :
.quad _Object_equals
.quad _Object_getHashCode
.quad _Object_toString
.quad Operator.operator
.quad Operator.value

.global _class_Plus
_class_Plus :
.quad _class_Operator
.long 16
.quad _class_Plus_methods
.long 0
.quad 0

.global _class_Plus_methods
_class_Plus_methods :
.quad _Object_equals
.quad _Object_getHashCode
.quad _Object_toString
.quad Operator.value
.quad Plus.operator

.global _class_Podziel
_class_Podziel :
.quad _class_Operator
.long 16
.quad _class_Podziel_methods
.long 0
.quad 0

.global _class_Podziel_methods
_class_Podziel_methods :
.quad _Object_equals
.quad _Object_getHashCode
.quad _Object_toString
.quad Operator.value
.quad Podziel.operator

.global _class_Razy
_class_Razy :
.quad _class_Operator
.long 16
.quad _class_Razy_methods
.long 0
.quad 0

.global _class_Razy_methods
_class_Razy_methods :
.quad _Object_equals
.quad _Object_getHashCode
.quad _Object_toString
.quad Operator.value
.quad Razy.operator

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
Podziel.operator :                          #-- ./play.lat:83:5 --#
pushq %RBX                                  #-- ./play.lat:83:5 --#
pushq %R12                                  #-- ./play.lat:83:5 --#
pushq %RBP                                  #-- ./play.lat:83:5 --#
movq %RSP, %RBP                             #-- ./play.lat:83:5 --#
subq $0, %RSP                               #-- ./play.lat:83:5 --#
Podziel.operator.L_entry :                  #-- ./play.lat:83:5 --#
movl %ESI, %R12D                            #-- load %v_t_121 at ./play.lat:83:5 --#
movl %EDX, %EBX                             #-- load %v_t_122 at ./play.lat:83:5 --#
movl %R12D, %EDI                            #-- passing arg at ./play.lat:84:9 --#
subq $0, %RSP                               #-- ./play.lat:84:9 --#
call printInt                               #-- ./play.lat:84:9 --#
addq $0, %RSP                               #-- ./play.lat:84:9 --#
leaq __const_1 (%RIP), %RAX                 #-- ./play.lat:85:21 --#
movq %RAX, %RDI                             #-- passing arg at ./play.lat:85:21 --#
subq $0, %RSP                               #-- ./play.lat:85:21 --#
call __createString                         #-- ./play.lat:85:21 --#
addq $0, %RSP                               #-- ./play.lat:85:21 --#
movq %RAX, %RAX                             #-- ./play.lat:85:21 --#
movq %RAX, %RDI                             #-- passing arg at ./play.lat:85:9 --#
subq $0, %RSP                               #-- ./play.lat:85:9 --#
call printString                            #-- ./play.lat:85:9 --#
addq $0, %RSP                               #-- ./play.lat:85:9 --#
movl %EBX, %EDI                             #-- passing arg at ./play.lat:86:9 --#
subq $0, %RSP                               #-- ./play.lat:86:9 --#
call printInt                               #-- ./play.lat:86:9 --#
addq $0, %RSP                               #-- ./play.lat:86:9 --#
movl %R12D, %EAX                            #-- ./play.lat:87:18 --#
cdq                                         #-- ./play.lat:87:18 --#
idivl %EBX                                  #-- ./play.lat:87:18 --#
movl %EAX, %EAX                             #-- ./play.lat:87:18 --#
movl %EAX, %EAX                             #-- move return value at ./play.lat:83:5 --#
addq $0, %RSP                               #-- ./play.lat:83:5 --#
leave                                       #-- ./play.lat:83:5 --#
pop %R12                                    #-- ./play.lat:83:5 --#
pop %RBX                                    #-- ./play.lat:83:5 --#
ret                                         #-- ./play.lat:83:5 --#
Razy.operator :                             #-- ./play.lat:75:5 --#
pushq %RBX                                  #-- ./play.lat:75:5 --#
pushq %R12                                  #-- ./play.lat:75:5 --#
pushq %RBP                                  #-- ./play.lat:75:5 --#
movq %RSP, %RBP                             #-- ./play.lat:75:5 --#
subq $0, %RSP                               #-- ./play.lat:75:5 --#
Razy.operator.L_entry :                     #-- ./play.lat:75:5 --#
movl %ESI, %R12D                            #-- load %v_t_112 at ./play.lat:75:5 --#
movl %EDX, %EBX                             #-- load %v_t_113 at ./play.lat:75:5 --#
movl %R12D, %EDI                            #-- passing arg at ./play.lat:76:9 --#
subq $0, %RSP                               #-- ./play.lat:76:9 --#
call printInt                               #-- ./play.lat:76:9 --#
addq $0, %RSP                               #-- ./play.lat:76:9 --#
leaq __const_2 (%RIP), %RAX                 #-- ./play.lat:77:21 --#
movq %RAX, %RDI                             #-- passing arg at ./play.lat:77:21 --#
subq $0, %RSP                               #-- ./play.lat:77:21 --#
call __createString                         #-- ./play.lat:77:21 --#
addq $0, %RSP                               #-- ./play.lat:77:21 --#
movq %RAX, %RAX                             #-- ./play.lat:77:21 --#
movq %RAX, %RDI                             #-- passing arg at ./play.lat:77:9 --#
subq $0, %RSP                               #-- ./play.lat:77:9 --#
call printString                            #-- ./play.lat:77:9 --#
addq $0, %RSP                               #-- ./play.lat:77:9 --#
movl %EBX, %EDI                             #-- passing arg at ./play.lat:78:9 --#
subq $0, %RSP                               #-- ./play.lat:78:9 --#
call printInt                               #-- ./play.lat:78:9 --#
addq $0, %RSP                               #-- ./play.lat:78:9 --#
movl %R12D, %EAX                            #-- ./play.lat:79:18 --#
imull %EBX, %EAX                            #-- ./play.lat:79:18 --#
movl %EAX, %EAX                             #-- move return value at ./play.lat:75:5 --#
addq $0, %RSP                               #-- ./play.lat:75:5 --#
leave                                       #-- ./play.lat:75:5 --#
pop %R12                                    #-- ./play.lat:75:5 --#
pop %RBX                                    #-- ./play.lat:75:5 --#
ret                                         #-- ./play.lat:75:5 --#
Minus.operator :                            #-- ./play.lat:67:5 --#
pushq %RBX                                  #-- ./play.lat:67:5 --#
pushq %R12                                  #-- ./play.lat:67:5 --#
pushq %RBP                                  #-- ./play.lat:67:5 --#
movq %RSP, %RBP                             #-- ./play.lat:67:5 --#
subq $0, %RSP                               #-- ./play.lat:67:5 --#
Minus.operator.L_entry :                    #-- ./play.lat:67:5 --#
movl %ESI, %R12D                            #-- load %v_t_103 at ./play.lat:67:5 --#
movl %EDX, %EBX                             #-- load %v_t_104 at ./play.lat:67:5 --#
movl %R12D, %EDI                            #-- passing arg at ./play.lat:68:9 --#
subq $0, %RSP                               #-- ./play.lat:68:9 --#
call printInt                               #-- ./play.lat:68:9 --#
addq $0, %RSP                               #-- ./play.lat:68:9 --#
leaq __const_3 (%RIP), %RAX                 #-- ./play.lat:69:21 --#
movq %RAX, %RDI                             #-- passing arg at ./play.lat:69:21 --#
subq $0, %RSP                               #-- ./play.lat:69:21 --#
call __createString                         #-- ./play.lat:69:21 --#
addq $0, %RSP                               #-- ./play.lat:69:21 --#
movq %RAX, %RAX                             #-- ./play.lat:69:21 --#
movq %RAX, %RDI                             #-- passing arg at ./play.lat:69:9 --#
subq $0, %RSP                               #-- ./play.lat:69:9 --#
call printString                            #-- ./play.lat:69:9 --#
addq $0, %RSP                               #-- ./play.lat:69:9 --#
movl %EBX, %EDI                             #-- passing arg at ./play.lat:70:9 --#
subq $0, %RSP                               #-- ./play.lat:70:9 --#
call printInt                               #-- ./play.lat:70:9 --#
addq $0, %RSP                               #-- ./play.lat:70:9 --#
movl %R12D, %EAX                            #-- ./play.lat:71:18 --#
subl %EBX, %EAX                             #-- ./play.lat:71:18 --#
movl %EAX, %EAX                             #-- move return value at ./play.lat:67:5 --#
addq $0, %RSP                               #-- ./play.lat:67:5 --#
leave                                       #-- ./play.lat:67:5 --#
pop %R12                                    #-- ./play.lat:67:5 --#
pop %RBX                                    #-- ./play.lat:67:5 --#
ret                                         #-- ./play.lat:67:5 --#
Plus.operator :                             #-- ./play.lat:59:5 --#
pushq %RBX                                  #-- ./play.lat:59:5 --#
pushq %R12                                  #-- ./play.lat:59:5 --#
pushq %RBP                                  #-- ./play.lat:59:5 --#
movq %RSP, %RBP                             #-- ./play.lat:59:5 --#
subq $0, %RSP                               #-- ./play.lat:59:5 --#
Plus.operator.L_entry :                     #-- ./play.lat:59:5 --#
movl %ESI, %R12D                            #-- load %v_t_94 at ./play.lat:59:5 --#
movl %EDX, %EBX                             #-- load %v_t_95 at ./play.lat:59:5 --#
movl %R12D, %EDI                            #-- passing arg at ./play.lat:60:9 --#
subq $0, %RSP                               #-- ./play.lat:60:9 --#
call printInt                               #-- ./play.lat:60:9 --#
addq $0, %RSP                               #-- ./play.lat:60:9 --#
leaq __const_4 (%RIP), %RAX                 #-- ./play.lat:61:21 --#
movq %RAX, %RDI                             #-- passing arg at ./play.lat:61:21 --#
subq $0, %RSP                               #-- ./play.lat:61:21 --#
call __createString                         #-- ./play.lat:61:21 --#
addq $0, %RSP                               #-- ./play.lat:61:21 --#
movq %RAX, %RAX                             #-- ./play.lat:61:21 --#
movq %RAX, %RDI                             #-- passing arg at ./play.lat:61:9 --#
subq $0, %RSP                               #-- ./play.lat:61:9 --#
call printString                            #-- ./play.lat:61:9 --#
addq $0, %RSP                               #-- ./play.lat:61:9 --#
movl %EBX, %EDI                             #-- passing arg at ./play.lat:62:9 --#
subq $0, %RSP                               #-- ./play.lat:62:9 --#
call printInt                               #-- ./play.lat:62:9 --#
addq $0, %RSP                               #-- ./play.lat:62:9 --#
leal 0 (%R12, %RBX, 1), %EAX                #-- addition %v_t_101 at ./play.lat:63:18 --#
movl %EAX, %EAX                             #-- move return value at ./play.lat:59:5 --#
addq $0, %RSP                               #-- ./play.lat:59:5 --#
leave                                       #-- ./play.lat:59:5 --#
pop %R12                                    #-- ./play.lat:59:5 --#
pop %RBX                                    #-- ./play.lat:59:5 --#
ret                                         #-- ./play.lat:59:5 --#
Operator.value :                            #-- ./play.lat:54:5 --#
pushq %RBX                                  #-- ./play.lat:54:5 --#
pushq %R12                                  #-- ./play.lat:54:5 --#
pushq %RBP                                  #-- ./play.lat:54:5 --#
movq %RSP, %RBP                             #-- ./play.lat:54:5 --#
subq $0, %RSP                               #-- ./play.lat:54:5 --#
Operator.value.L_entry :                    #-- ./play.lat:54:5 --#
movq %RDI, %R12                             #-- load %v_t_87 at ./play.lat:54:5 --#
movq 8 (%R12), %RAX                         #-- load data (indirect) at ./play.lat:55:30 --#
movq 0 (%RAX), %RAX                         #-- load %v_t_88 at ./play.lat:55:30 --#
movq %RAX, %RDI                             #-- passing arg at ./play.lat:55:30 --#
subq $0, %RSP                               #-- ./play.lat:55:30 --#
testq %RDI, %RDI                            #-- ./play.lat:55:30 --#
jz __errorNull                              #-- ./play.lat:55:30 --#
movq 20 (%RDI), %RAX                        #-- load address of vtable at ./play.lat:55:30 --#
call * 24 (%RAX)                            #-- call value at ./play.lat:55:30 --#
addq $0, %RSP                               #-- ./play.lat:55:30 --#
movl %EAX, %EBX                             #-- ./play.lat:55:30 --#
movq 8 (%R12), %RAX                         #-- load data (indirect) at ./play.lat:55:44 --#
movq 8 (%RAX), %RAX                         #-- load %v_t_90 at ./play.lat:55:44 --#
movq %RAX, %RDI                             #-- passing arg at ./play.lat:55:44 --#
subq $0, %RSP                               #-- ./play.lat:55:44 --#
testq %RDI, %RDI                            #-- ./play.lat:55:44 --#
jz __errorNull                              #-- ./play.lat:55:44 --#
movq 20 (%RDI), %RAX                        #-- load address of vtable at ./play.lat:55:44 --#
call * 24 (%RAX)                            #-- call value at ./play.lat:55:44 --#
addq $0, %RSP                               #-- ./play.lat:55:44 --#
movl %EAX, %EAX                             #-- ./play.lat:55:44 --#
movq %R12, %RDI                             #-- passing arg at ./play.lat:55:16 --#
movl %EBX, %ESI                             #-- passing arg at ./play.lat:55:16 --#
movl %EAX, %EDX                             #-- passing arg at ./play.lat:55:16 --#
subq $0, %RSP                               #-- ./play.lat:55:16 --#
testq %RDI, %RDI                            #-- ./play.lat:55:16 --#
jz __errorNull                              #-- ./play.lat:55:16 --#
movq 20 (%RDI), %RAX                        #-- load address of vtable at ./play.lat:55:16 --#
call * 24 (%RAX)                            #-- call operator at ./play.lat:55:16 --#
addq $0, %RSP                               #-- ./play.lat:55:16 --#
movl %EAX, %EAX                             #-- ./play.lat:55:16 --#
movl %EAX, %EAX                             #-- move return value at ./play.lat:54:5 --#
addq $0, %RSP                               #-- ./play.lat:54:5 --#
leave                                       #-- ./play.lat:54:5 --#
pop %R12                                    #-- ./play.lat:54:5 --#
pop %RBX                                    #-- ./play.lat:54:5 --#
ret                                         #-- ./play.lat:54:5 --#
Operator.operator :                         #-- ./play.lat:50:5 --#
pushq %RBP                                  #-- ./play.lat:50:5 --#
movq %RSP, %RBP                             #-- ./play.lat:50:5 --#
subq $0, %RSP                               #-- ./play.lat:50:5 --#
Operator.operator.L_entry :                 #-- ./play.lat:50:5 --#
leaq __const_5 (%RIP), %RAX                 #-- ./play.lat:51:21 --#
movq %RAX, %RDI                             #-- passing arg at ./play.lat:51:21 --#
subq $0, %RSP                               #-- ./play.lat:51:21 --#
call __createString                         #-- ./play.lat:51:21 --#
addq $0, %RSP                               #-- ./play.lat:51:21 --#
movq %RAX, %RAX                             #-- ./play.lat:51:21 --#
movq %RAX, %RDI                             #-- passing arg at ./play.lat:51:9 --#
subq $0, %RSP                               #-- ./play.lat:51:9 --#
call printString                            #-- ./play.lat:51:9 --#
addq $0, %RSP                               #-- ./play.lat:51:9 --#
movl $0, %EAX                               #-- move return value at ./play.lat:50:5 --#
addq $0, %RSP                               #-- ./play.lat:50:5 --#
leave                                       #-- ./play.lat:50:5 --#
ret                                         #-- ./play.lat:50:5 --#
Liczba.value :                              #-- ./play.lat:43:5 --#
pushq %RBP                                  #-- ./play.lat:43:5 --#
movq %RSP, %RBP                             #-- ./play.lat:43:5 --#
subq $0, %RSP                               #-- ./play.lat:43:5 --#
Liczba.value.L_entry :                      #-- ./play.lat:43:5 --#
movq %RDI, %RAX                             #-- load %v_t_78 at ./play.lat:43:5 --#
movq 8 (%RAX), %RAX                         #-- load data (indirect) at ./play.lat:44:16 --#
movl 0 (%RAX), %EAX                         #-- load %v_t_79 at ./play.lat:44:16 --#
movl %EAX, %EAX                             #-- move return value at ./play.lat:43:5 --#
addq $0, %RSP                               #-- ./play.lat:43:5 --#
leave                                       #-- ./play.lat:43:5 --#
ret                                         #-- ./play.lat:43:5 --#
Node.value :                                #-- ./play.lat:36:5 --#
pushq %RBP                                  #-- ./play.lat:36:5 --#
movq %RSP, %RBP                             #-- ./play.lat:36:5 --#
subq $0, %RSP                               #-- ./play.lat:36:5 --#
Node.value.L_entry :                        #-- ./play.lat:36:5 --#
leaq __const_6 (%RIP), %RAX                 #-- ./play.lat:37:21 --#
movq %RAX, %RDI                             #-- passing arg at ./play.lat:37:21 --#
subq $0, %RSP                               #-- ./play.lat:37:21 --#
call __createString                         #-- ./play.lat:37:21 --#
addq $0, %RSP                               #-- ./play.lat:37:21 --#
movq %RAX, %RAX                             #-- ./play.lat:37:21 --#
movq %RAX, %RDI                             #-- passing arg at ./play.lat:37:9 --#
subq $0, %RSP                               #-- ./play.lat:37:9 --#
call printString                            #-- ./play.lat:37:9 --#
addq $0, %RSP                               #-- ./play.lat:37:9 --#
movl $0, %EAX                               #-- move return value at ./play.lat:36:5 --#
addq $0, %RSP                               #-- ./play.lat:36:5 --#
leave                                       #-- ./play.lat:36:5 --#
ret                                         #-- ./play.lat:36:5 --#
__cl_TopLevel.liczba :                      #-- ./play.lat:30:1 --#
pushq %RBP                                  #-- ./play.lat:30:1 --#
movq %RSP, %RBP                             #-- ./play.lat:30:1 --#
subq $0, %RSP                               #-- ./play.lat:30:1 --#
__cl_TopLevel.liczba.L_entry :              #-- ./play.lat:30:1 --#
movl %EDI, %ECX                             #-- load %v_t_69 at ./play.lat:30:1 --#
pushq %RCX                                  #-- save caller saved at ./play.lat:31:18 --#
subq $8, %RSP 
leaq _class_Liczba (%RIP), %RDI             #-- ./play.lat:31:18 --#
subq $0, %RSP                               #-- ./play.lat:31:18 --#
call __new                                  #-- ./play.lat:31:18 --#
addq $8, %RSP                               #-- ./play.lat:31:18 --#
movq %RAX, %RAX                             #-- ./play.lat:31:18 --#
pop %RCX                                    #-- ./play.lat:31:18 --#
movq 8 (%RAX), %RAX                         #-- load data (indirect) at ./play.lat:32:5 --#
movl %ECX, 0 (%RAX)                         #-- ./play.lat:32:5 --#
movq %RAX, %RDI                             #-- passing arg at ./play.lat:30:1 --#
leaq _class_Node (%RIP), %RSI               #-- ./play.lat:30:1 --#
subq $16, %RSP                               #-- ./play.lat:30:1 --#
call __cast                                 #-- ./play.lat:30:1 --#
addq $16, %RSP                               #-- ./play.lat:30:1 --#
movq %RAX, %RAX                             #-- ./play.lat:30:1 --#
movq %RAX, %RAX                             #-- move return value at ./play.lat:30:1 --#
addq $0, %RSP                               #-- ./play.lat:30:1 --#
leave                                       #-- ./play.lat:30:1 --#
ret                                         #-- ./play.lat:30:1 --#
__cl_TopLevel.minus :                       #-- ./play.lat:24:1 --#
pushq %RBX                                  #-- ./play.lat:24:1 --#
pushq %R12                                  #-- ./play.lat:24:1 --#
pushq %RBP                                  #-- ./play.lat:24:1 --#
movq %RSP, %RBP                             #-- ./play.lat:24:1 --#
subq $0, %RSP                               #-- ./play.lat:24:1 --#
__cl_TopLevel.minus.L_entry :               #-- ./play.lat:24:1 --#
movq %RDI, %R12                             #-- load %v_t_63 at ./play.lat:24:1 --#
movq %RSI, %RBX                             #-- load %v_t_64 at ./play.lat:24:1 --#
leaq _class_Minus (%RIP), %RDI              #-- ./play.lat:25:20 --#
subq $0, %RSP                               #-- ./play.lat:25:20 --#
call __new                                  #-- ./play.lat:25:20 --#
addq $0, %RSP                               #-- ./play.lat:25:20 --#
movq %RAX, %RAX                             #-- ./play.lat:25:20 --#
movq %RAX, %RDI                             #-- passing arg at ./play.lat:25:5 --#
leaq _class_Operator (%RIP), %RSI           #-- ./play.lat:25:5 --#
subq $0, %RSP                               #-- ./play.lat:25:5 --#
call __cast                                 #-- ./play.lat:25:5 --#
addq $0, %RSP                               #-- ./play.lat:25:5 --#
movq %RAX, %RAX                             #-- ./play.lat:25:5 --#
movq 8 (%RAX), %RAX                         #-- load data (indirect) at ./play.lat:26:5 --#
movq %R12, 0 (%RAX)                         #-- ./play.lat:26:5 --#
movq 8 (%RAX), %RAX                         #-- load data (indirect) at ./play.lat:27:5 --#
movq %RBX, 8 (%RAX)                         #-- ./play.lat:27:5 --#
movq %RAX, %RDI                             #-- passing arg at ./play.lat:24:1 --#
leaq _class_Node (%RIP), %RSI               #-- ./play.lat:24:1 --#
subq $0, %RSP                               #-- ./play.lat:24:1 --#
call __cast                                 #-- ./play.lat:24:1 --#
addq $0, %RSP                               #-- ./play.lat:24:1 --#
movq %RAX, %RAX                             #-- ./play.lat:24:1 --#
movq %RAX, %RAX                             #-- move return value at ./play.lat:24:1 --#
addq $0, %RSP                               #-- ./play.lat:24:1 --#
leave                                       #-- ./play.lat:24:1 --#
pop %R12                                    #-- ./play.lat:24:1 --#
pop %RBX                                    #-- ./play.lat:24:1 --#
ret                                         #-- ./play.lat:24:1 --#
__cl_TopLevel.podziel :                     #-- ./play.lat:18:1 --#
pushq %RBX                                  #-- ./play.lat:18:1 --#
pushq %R12                                  #-- ./play.lat:18:1 --#
pushq %RBP                                  #-- ./play.lat:18:1 --#
movq %RSP, %RBP                             #-- ./play.lat:18:1 --#
subq $0, %RSP                               #-- ./play.lat:18:1 --#
__cl_TopLevel.podziel.L_entry :             #-- ./play.lat:18:1 --#
movq %RDI, %R12                             #-- load %v_t_57 at ./play.lat:18:1 --#
movq %RSI, %RBX                             #-- load %v_t_58 at ./play.lat:18:1 --#
leaq _class_Podziel (%RIP), %RDI            #-- ./play.lat:19:20 --#
subq $0, %RSP                               #-- ./play.lat:19:20 --#
call __new                                  #-- ./play.lat:19:20 --#
addq $0, %RSP                               #-- ./play.lat:19:20 --#
movq %RAX, %RAX                             #-- ./play.lat:19:20 --#
movq %RAX, %RDI                             #-- passing arg at ./play.lat:19:5 --#
leaq _class_Operator (%RIP), %RSI           #-- ./play.lat:19:5 --#
subq $0, %RSP                               #-- ./play.lat:19:5 --#
call __cast                                 #-- ./play.lat:19:5 --#
addq $0, %RSP                               #-- ./play.lat:19:5 --#
movq %RAX, %RAX                             #-- ./play.lat:19:5 --#
movq 8 (%RAX), %RAX                         #-- load data (indirect) at ./play.lat:20:5 --#
movq %R12, 0 (%RAX)                         #-- ./play.lat:20:5 --#
movq 8 (%RAX), %RAX                         #-- load data (indirect) at ./play.lat:21:5 --#
movq %RBX, 8 (%RAX)                         #-- ./play.lat:21:5 --#
movq %RAX, %RDI                             #-- passing arg at ./play.lat:18:1 --#
leaq _class_Node (%RIP), %RSI               #-- ./play.lat:18:1 --#
subq $0, %RSP                               #-- ./play.lat:18:1 --#
call __cast                                 #-- ./play.lat:18:1 --#
addq $0, %RSP                               #-- ./play.lat:18:1 --#
movq %RAX, %RAX                             #-- ./play.lat:18:1 --#
movq %RAX, %RAX                             #-- move return value at ./play.lat:18:1 --#
addq $0, %RSP                               #-- ./play.lat:18:1 --#
leave                                       #-- ./play.lat:18:1 --#
pop %R12                                    #-- ./play.lat:18:1 --#
pop %RBX                                    #-- ./play.lat:18:1 --#
ret                                         #-- ./play.lat:18:1 --#
__cl_TopLevel.razy :                        #-- ./play.lat:12:1 --#
pushq %RBX                                  #-- ./play.lat:12:1 --#
pushq %R12                                  #-- ./play.lat:12:1 --#
pushq %RBP                                  #-- ./play.lat:12:1 --#
movq %RSP, %RBP                             #-- ./play.lat:12:1 --#
subq $0, %RSP                               #-- ./play.lat:12:1 --#
__cl_TopLevel.razy.L_entry :                #-- ./play.lat:12:1 --#
movq %RDI, %R12                             #-- load %v_t_51 at ./play.lat:12:1 --#
movq %RSI, %RBX                             #-- load %v_t_52 at ./play.lat:12:1 --#
leaq _class_Razy (%RIP), %RDI               #-- ./play.lat:13:20 --#
subq $0, %RSP                               #-- ./play.lat:13:20 --#
call __new                                  #-- ./play.lat:13:20 --#
addq $0, %RSP                               #-- ./play.lat:13:20 --#
movq %RAX, %RAX                             #-- ./play.lat:13:20 --#
movq %RAX, %RDI                             #-- passing arg at ./play.lat:13:5 --#
leaq _class_Operator (%RIP), %RSI           #-- ./play.lat:13:5 --#
subq $0, %RSP                               #-- ./play.lat:13:5 --#
call __cast                                 #-- ./play.lat:13:5 --#
addq $0, %RSP                               #-- ./play.lat:13:5 --#
movq %RAX, %RAX                             #-- ./play.lat:13:5 --#
movq 8 (%RAX), %RAX                         #-- load data (indirect) at ./play.lat:14:5 --#
movq %R12, 0 (%RAX)                         #-- ./play.lat:14:5 --#
movq 8 (%RAX), %RAX                         #-- load data (indirect) at ./play.lat:15:5 --#
movq %RBX, 8 (%RAX)                         #-- ./play.lat:15:5 --#
movq %RAX, %RDI                             #-- passing arg at ./play.lat:12:1 --#
leaq _class_Node (%RIP), %RSI               #-- ./play.lat:12:1 --#
subq $0, %RSP                               #-- ./play.lat:12:1 --#
call __cast                                 #-- ./play.lat:12:1 --#
addq $0, %RSP                               #-- ./play.lat:12:1 --#
movq %RAX, %RAX                             #-- ./play.lat:12:1 --#
movq %RAX, %RAX                             #-- move return value at ./play.lat:12:1 --#
addq $0, %RSP                               #-- ./play.lat:12:1 --#
leave                                       #-- ./play.lat:12:1 --#
pop %R12                                    #-- ./play.lat:12:1 --#
pop %RBX                                    #-- ./play.lat:12:1 --#
ret                                         #-- ./play.lat:12:1 --#
__cl_TopLevel.plus :                        #-- ./play.lat:6:1 --#
pushq %RBX                                  #-- ./play.lat:6:1 --#
pushq %R12                                  #-- ./play.lat:6:1 --#
pushq %RBP                                  #-- ./play.lat:6:1 --#
movq %RSP, %RBP                             #-- ./play.lat:6:1 --#
subq $0, %RSP                               #-- ./play.lat:6:1 --#
__cl_TopLevel.plus.L_entry :                #-- ./play.lat:6:1 --#
movq %RDI, %R12                             #-- load %v_t_45 at ./play.lat:6:1 --#
movq %RSI, %RBX                             #-- load %v_t_46 at ./play.lat:6:1 --#
leaq _class_Plus (%RIP), %RDI               #-- ./play.lat:7:20 --#
subq $0, %RSP                               #-- ./play.lat:7:20 --#
call __new                                  #-- ./play.lat:7:20 --#
addq $0, %RSP                               #-- ./play.lat:7:20 --#
movq %RAX, %RAX                             #-- ./play.lat:7:20 --#
movq %RAX, %RDI                             #-- passing arg at ./play.lat:7:5 --#
leaq _class_Operator (%RIP), %RSI           #-- ./play.lat:7:5 --#
subq $0, %RSP                               #-- ./play.lat:7:5 --#
call __cast                                 #-- ./play.lat:7:5 --#
addq $0, %RSP                               #-- ./play.lat:7:5 --#
movq %RAX, %RAX                             #-- ./play.lat:7:5 --#
movq 8 (%RAX), %RAX                         #-- load data (indirect) at ./play.lat:8:5 --#
movq %R12, 0 (%RAX)                         #-- ./play.lat:8:5 --#
movq 8 (%RAX), %RAX                         #-- load data (indirect) at ./play.lat:9:5 --#
movq %RBX, 8 (%RAX)                         #-- ./play.lat:9:5 --#
movq %RAX, %RDI                             #-- passing arg at ./play.lat:6:1 --#
leaq _class_Node (%RIP), %RSI               #-- ./play.lat:6:1 --#
subq $0, %RSP                               #-- ./play.lat:6:1 --#
call __cast                                 #-- ./play.lat:6:1 --#
addq $0, %RSP                               #-- ./play.lat:6:1 --#
movq %RAX, %RAX                             #-- ./play.lat:6:1 --#
movq %RAX, %RAX                             #-- move return value at ./play.lat:6:1 --#
addq $0, %RSP                               #-- ./play.lat:6:1 --#
leave                                       #-- ./play.lat:6:1 --#
pop %R12                                    #-- ./play.lat:6:1 --#
pop %RBX                                    #-- ./play.lat:6:1 --#
ret                                         #-- ./play.lat:6:1 --#
main :                                      #-- ./play.lat:1:1 --#
__cl_TopLevel.main :                        #-- ./play.lat:1:1 --#
pushq %RBX                                  #-- ./play.lat:1:1 --#
pushq %R12                                  #-- ./play.lat:1:1 --#
pushq %R13                                  #-- ./play.lat:1:1 --#
subq $8, %RSP                               #-- ./play.lat:1:1 --#
pushq %RBP                                  #-- ./play.lat:1:1 --#
movq %RSP, %RBP                             #-- ./play.lat:1:1 --#
subq $0, %RSP                               #-- ./play.lat:1:1 --#
__cl_TopLevel.main.L_entry :                #-- ./play.lat:1:1 --#
movl $4, %EDI                               #-- passing arg at ./play.lat:2:25 --#
subq $0, %RSP                               #-- ./play.lat:2:25 --#
call __cl_TopLevel.liczba                   #-- ./play.lat:2:25 --#
addq $0, %RSP                               #-- ./play.lat:2:25 --#
movq %RAX, %RBX                             #-- ./play.lat:2:25 --#
movl $3, %EDI                               #-- passing arg at ./play.lat:2:36 --#
subq $0, %RSP                               #-- ./play.lat:2:36 --#
call __cl_TopLevel.liczba                   #-- ./play.lat:2:36 --#
addq $0, %RSP                               #-- ./play.lat:2:36 --#
movq %RAX, %RAX                             #-- ./play.lat:2:36 --#
movq %RBX, %RDI                             #-- passing arg at ./play.lat:2:19 --#
movq %RAX, %RSI                             #-- passing arg at ./play.lat:2:19 --#
subq $0, %RSP                               #-- ./play.lat:2:19 --#
call __cl_TopLevel.minus                    #-- ./play.lat:2:19 --#
addq $0, %RSP                               #-- ./play.lat:2:19 --#
movq %RAX, %R13                             #-- ./play.lat:2:19 --#
movl $2, %EDI                               #-- passing arg at ./play.lat:2:53 --#
subq $0, %RSP                               #-- ./play.lat:2:53 --#
call __cl_TopLevel.liczba                   #-- ./play.lat:2:53 --#
addq $0, %RSP                               #-- ./play.lat:2:53 --#
movq %RAX, %R12                             #-- ./play.lat:2:53 --#
movl $4, %EDI                               #-- passing arg at ./play.lat:2:72 --#
subq $0, %RSP                               #-- ./play.lat:2:72 --#
call __cl_TopLevel.liczba                   #-- ./play.lat:2:72 --#
addq $0, %RSP                               #-- ./play.lat:2:72 --#
movq %RAX, %RBX                             #-- ./play.lat:2:72 --#
movl $2, %EDI                               #-- passing arg at ./play.lat:2:83 --#
subq $0, %RSP                               #-- ./play.lat:2:83 --#
call __cl_TopLevel.liczba                   #-- ./play.lat:2:83 --#
addq $0, %RSP                               #-- ./play.lat:2:83 --#
movq %RAX, %RAX                             #-- ./play.lat:2:83 --#
movq %RBX, %RDI                             #-- passing arg at ./play.lat:2:64 --#
movq %RAX, %RSI                             #-- passing arg at ./play.lat:2:64 --#
subq $0, %RSP                               #-- ./play.lat:2:64 --#
call __cl_TopLevel.podziel                  #-- ./play.lat:2:64 --#
addq $0, %RSP                               #-- ./play.lat:2:64 --#
movq %RAX, %RAX                             #-- ./play.lat:2:64 --#
movq %R12, %RDI                             #-- passing arg at ./play.lat:2:48 --#
movq %RAX, %RSI                             #-- passing arg at ./play.lat:2:48 --#
subq $0, %RSP                               #-- ./play.lat:2:48 --#
call __cl_TopLevel.razy                     #-- ./play.lat:2:48 --#
addq $0, %RSP                               #-- ./play.lat:2:48 --#
movq %RAX, %RAX                             #-- ./play.lat:2:48 --#
movq %R13, %RDI                             #-- passing arg at ./play.lat:2:14 --#
movq %RAX, %RSI                             #-- passing arg at ./play.lat:2:14 --#
subq $0, %RSP                               #-- ./play.lat:2:14 --#
call __cl_TopLevel.plus                     #-- ./play.lat:2:14 --#
addq $0, %RSP                               #-- ./play.lat:2:14 --#
movq %RAX, %RAX                             #-- ./play.lat:2:14 --#
movq %RAX, %RDI                             #-- passing arg at ./play.lat:3:14 --#
subq $0, %RSP                               #-- ./play.lat:3:14 --#
testq %RDI, %RDI                            #-- ./play.lat:3:14 --#
jz __errorNull                              #-- ./play.lat:3:14 --#
movq 20 (%RDI), %RAX                        #-- load address of vtable at ./play.lat:3:14 --#
call * 24 (%RAX)                            #-- call value at ./play.lat:3:14 --#
addq $0, %RSP                               #-- ./play.lat:3:14 --#
movl %EAX, %EAX                             #-- ./play.lat:3:14 --#
movl %EAX, %EDI                             #-- passing arg at ./play.lat:3:5 --#
subq $0, %RSP                               #-- ./play.lat:3:5 --#
call printInt                               #-- ./play.lat:3:5 --#
addq $0, %RSP                               #-- ./play.lat:3:5 --#
movl $0, %EAX                               #-- move return value at ./play.lat:1:1 --#
addq $0, %RSP                               #-- ./play.lat:1:1 --#
leave                                       #-- ./play.lat:1:1 --#
addq $8, %RSP                               #-- ./play.lat:1:1 --#
pop %R13                                    #-- ./play.lat:1:1 --#
pop %R12                                    #-- ./play.lat:1:1 --#
pop %RBX                                    #-- ./play.lat:1:1 --#
ret                                         #-- ./play.lat:1:1 --#
__errorNull :                               #-- runtime error on null dereference at ./play.lat:1:1 --#
andq $-16, %RSP                             #-- 16 bytes allign at ./play.lat:1:1 --#
call __errorNull                            #-- ./play.lat:1:1 --#
