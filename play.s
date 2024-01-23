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
__const_4 :
.string " "

__const_1 :
.string "+"

__const_6 :
.string "?"

__const_9 :
.string "Liczba{"

__const_8 :
.string "Node.operator()"

__const_7 :
.string "Node.value()"

__const_10 :
.string "Node{}"

__const_5 :
.string "Operator{"

__const_2 :
.string "Plus.operator()"

__const_3 :
.string "}"

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

.global _class_Liczba
_class_Liczba :
.quad _class_Node
.long 8
.quad _class_Liczba_methods
.long 0
.quad 0

.global _class_Liczba_methods
_class_Liczba_methods :
.quad Liczba.toString
.quad _Object_getHashCode
.quad _Object_equals
.quad Liczba.value

.global _class_Node
_class_Node :
.quad _class_Object
.long 0
.quad _class_Node_methods
.long 0
.quad 0

.global _class_Node_methods
_class_Node_methods :
.quad Node.toString
.quad _Object_getHashCode
.quad _Object_equals
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
.quad _Object_toString
.quad _Object_getHashCode
.quad _Object_equals

.global _class_Operator
_class_Operator :
.quad _class_Node
.long 16
.quad _class_Operator_methods
.long 0
.quad 0

.global _class_Operator_methods
_class_Operator_methods :
.quad Operator.toString
.quad _Object_getHashCode
.quad _Object_equals
.quad Operator.value
.quad Operator.operator
.quad Operator.toStringFor

.global _class_Plus
_class_Plus :
.quad _class_Operator
.long 16
.quad _class_Plus_methods
.long 0
.quad 0

.global _class_Plus_methods
_class_Plus_methods :
.quad Plus.toString
.quad _Object_getHashCode
.quad _Object_equals
.quad Operator.value
.quad Operator.toStringFor
.quad Plus.operator

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
.quad _String_endsWith
.quad _String_getBytes
.quad _String_indexOf
.quad _String_length
.quad _String_startsWith
.quad _String_substring

.global main

.section .text
Plus.toString :                    #-- ./play.lat:62:5 --#
pushq %RBP                         #-- ./play.lat:62:5 --#
movq %RSP, %RBP                    #-- ./play.lat:62:5 --#
subq $0, %RSP                      #-- ./play.lat:62:5 --#
Plus.toString.L_entry :            #-- ./play.lat:62:5 --#
movq %RDI, %RCX                    #-- load %v_t_94 at ./play.lat:62:5 --#
leaq __const_1 (%RIP), %RAX        #-- ./play.lat:63:33 --#
pushq %RCX                         #-- save caller saved at ./play.lat:63:33 --#
movq %RAX, %RDI                    #-- passing arg at ./play.lat:63:33 --#
subq $0, %RSP                      #-- ./play.lat:63:33 --#
call __createString                #-- ./play.lat:63:33 --#
addq $0, %RSP                      #-- ./play.lat:63:33 --#
movq %RAX, %RAX                    #-- ./play.lat:63:33 --#
pop %RCX                           #-- ./play.lat:63:33 --#
movq %RCX, %RDI                    #-- passing arg at ./play.lat:63:16 --#
movq %RAX, %RSI                    #-- passing arg at ./play.lat:63:16 --#
subq $0, %RSP                      #-- ./play.lat:63:16 --#
testq %RDI, %RDI                   #-- ./play.lat:63:16 --#
jz __errorNull                     #-- ./play.lat:63:16 --#
movq 20 (%RDI), %RAX               #-- load address of vtable at ./play.lat:63:16 --#
call * 32 (%RAX)                   #-- call toStringFor at ./play.lat:63:16 --#
addq $0, %RSP                      #-- ./play.lat:63:16 --#
movq %RAX, %RAX                    #-- ./play.lat:63:16 --#
movq %RAX, %RAX                    #-- move return value at ./play.lat:62:5 --#
addq $0, %RSP                      #-- ./play.lat:62:5 --#
leave                              #-- ./play.lat:62:5 --#
ret                                #-- ./play.lat:62:5 --#
Plus.operator :                    #-- ./play.lat:58:5 --#
pushq %RBX                         #-- ./play.lat:58:5 --#
pushq %R12                         #-- ./play.lat:58:5 --#
pushq %RBP                         #-- ./play.lat:58:5 --#
movq %RSP, %RBP                    #-- ./play.lat:58:5 --#
subq $0, %RSP                      #-- ./play.lat:58:5 --#
Plus.operator.L_entry :            #-- ./play.lat:58:5 --#
movl %ESI, %R12D                   #-- load %v_t_88 at ./play.lat:58:5 --#
movl %EDX, %EBX                    #-- load %v_t_89 at ./play.lat:58:5 --#
leaq __const_2 (%RIP), %RAX        #-- ./play.lat:59:21 --#
movq %RAX, %RDI                    #-- passing arg at ./play.lat:59:21 --#
subq $0, %RSP                      #-- ./play.lat:59:21 --#
call __createString                #-- ./play.lat:59:21 --#
addq $0, %RSP                      #-- ./play.lat:59:21 --#
movq %RAX, %RAX                    #-- ./play.lat:59:21 --#
movq %RAX, %RDI                    #-- passing arg at ./play.lat:59:9 --#
subq $0, %RSP                      #-- ./play.lat:59:9 --#
call printString                   #-- ./play.lat:59:9 --#
addq $0, %RSP                      #-- ./play.lat:59:9 --#
leal 0 (%R12, %RBX, 1), %EAX       #-- addition %v_t_93 at ./play.lat:60:18 --#
movl %EAX, %EAX                    #-- move return value at ./play.lat:58:5 --#
addq $0, %RSP                      #-- ./play.lat:58:5 --#
leave                              #-- ./play.lat:58:5 --#
pop %R12                           #-- ./play.lat:58:5 --#
pop %RBX                           #-- ./play.lat:58:5 --#
ret                                #-- ./play.lat:58:5 --#
Operator.toStringFor :             #-- ./play.lat:53:5 --#
pushq %RBX                         #-- ./play.lat:53:5 --#
pushq %R12                         #-- ./play.lat:53:5 --#
pushq %R13                         #-- ./play.lat:53:5 --#
pushq %R14                         #-- ./play.lat:53:5 --#
pushq %R15                         #-- ./play.lat:53:5 --#
subq $8, %RSP                      #-- ./play.lat:53:5 --#
pushq %RBP                         #-- ./play.lat:53:5 --#
movq %RSP, %RBP                    #-- ./play.lat:53:5 --#
subq $0, %RSP                      #-- ./play.lat:53:5 --#
Operator.toStringFor.L_entry :     #-- ./play.lat:53:5 --#
movq %RDI, %R15                    #-- load %v_t_69 at ./play.lat:53:5 --#
movq %RSI, %RBX                    #-- load %v_t_70 at ./play.lat:53:5 --#
leaq __const_3 (%RIP), %R14        #-- ./play.lat:54:90 --#
movq %R14, %RDI                    #-- passing arg at ./play.lat:54:90 --#
subq $0, %RSP                      #-- ./play.lat:54:90 --#
call __createString                #-- ./play.lat:54:90 --#
addq $0, %RSP                      #-- ./play.lat:54:90 --#
movq %RAX, %R14                    #-- ./play.lat:54:90 --#
movq 44 (%R15), %RAX               #-- load %v_t_82 at ./play.lat:54:67 --#
movq %RAX, %RDI                    #-- passing arg at ./play.lat:54:67 --#
subq $0, %RSP                      #-- ./play.lat:54:67 --#
testq %RDI, %RDI                   #-- ./play.lat:54:67 --#
jz __errorNull                     #-- ./play.lat:54:67 --#
movq 20 (%RDI), %RAX               #-- load address of vtable at ./play.lat:54:67 --#
call * 0 (%RAX)                    #-- call toString at ./play.lat:54:67 --#
addq $0, %RSP                      #-- ./play.lat:54:67 --#
movq %RAX, %R13                    #-- ./play.lat:54:67 --#
leaq __const_4 (%RIP), %R12        #-- ./play.lat:54:62 --#
movq %R12, %RDI                    #-- passing arg at ./play.lat:54:62 --#
subq $0, %RSP                      #-- ./play.lat:54:62 --#
call __createString                #-- ./play.lat:54:62 --#
addq $0, %RSP                      #-- ./play.lat:54:62 --#
movq %RAX, %R12                    #-- ./play.lat:54:62 --#
movq 36 (%R15), %RAX               #-- load %v_t_73 at ./play.lat:54:29 --#
movq %RAX, %RDI                    #-- passing arg at ./play.lat:54:29 --#
subq $0, %RSP                      #-- ./play.lat:54:29 --#
testq %RDI, %RDI                   #-- ./play.lat:54:29 --#
jz __errorNull                     #-- ./play.lat:54:29 --#
movq 20 (%RDI), %RAX               #-- load address of vtable at ./play.lat:54:29 --#
call * 0 (%RAX)                    #-- call toString at ./play.lat:54:29 --#
addq $0, %RSP                      #-- ./play.lat:54:29 --#
movq %RAX, %RAX                    #-- ./play.lat:54:29 --#
leaq __const_5 (%RIP), %RCX        #-- ./play.lat:54:16 --#
pushq %RAX                         #-- save caller saved at ./play.lat:54:16 --#
movq %RCX, %RDI                    #-- passing arg at ./play.lat:54:16 --#
subq $0, %RSP                      #-- ./play.lat:54:16 --#
call __createString                #-- ./play.lat:54:16 --#
addq $0, %RSP                      #-- ./play.lat:54:16 --#
movq %RAX, %RCX                    #-- ./play.lat:54:16 --#
pop %RAX                           #-- ./play.lat:54:16 --#
movq %RCX, %RDI                    #-- passing arg at ./play.lat:54:16 --#
movq %RAX, %RSI                    #-- passing arg at ./play.lat:54:16 --#
subq $0, %RSP                      #-- ./play.lat:54:16 --#
testq %RDI, %RDI                   #-- ./play.lat:54:16 --#
jz __errorNull                     #-- ./play.lat:54:16 --#
movq 20 (%RDI), %RAX               #-- load address of vtable at ./play.lat:54:16 --#
call * 32 (%RAX)                   #-- call concat at ./play.lat:54:16 --#
addq $0, %RSP                      #-- ./play.lat:54:16 --#
movq %RAX, %RAX                    #-- ./play.lat:54:16 --#
movq %RAX, %RDI                    #-- passing arg at ./play.lat:54:16 --#
movq %R12, %RSI                    #-- passing arg at ./play.lat:54:16 --#
subq $0, %RSP                      #-- ./play.lat:54:16 --#
testq %RDI, %RDI                   #-- ./play.lat:54:16 --#
jz __errorNull                     #-- ./play.lat:54:16 --#
movq 20 (%RDI), %RAX               #-- load address of vtable at ./play.lat:54:16 --#
call * 32 (%RAX)                   #-- call concat at ./play.lat:54:16 --#
addq $0, %RSP                      #-- ./play.lat:54:16 --#
movq %RAX, %RAX                    #-- ./play.lat:54:16 --#
movq %RAX, %RDI                    #-- passing arg at ./play.lat:54:16 --#
movq %RBX, %RSI                    #-- passing arg at ./play.lat:54:16 --#
subq $0, %RSP                      #-- ./play.lat:54:16 --#
testq %RDI, %RDI                   #-- ./play.lat:54:16 --#
jz __errorNull                     #-- ./play.lat:54:16 --#
movq 20 (%RDI), %RAX               #-- load address of vtable at ./play.lat:54:16 --#
call * 32 (%RAX)                   #-- call concat at ./play.lat:54:16 --#
addq $0, %RSP                      #-- ./play.lat:54:16 --#
movq %RAX, %RAX                    #-- ./play.lat:54:16 --#
movq %RAX, %RDI                    #-- passing arg at ./play.lat:54:16 --#
movq %R12, %RSI                    #-- passing arg at ./play.lat:54:16 --#
subq $0, %RSP                      #-- ./play.lat:54:16 --#
testq %RDI, %RDI                   #-- ./play.lat:54:16 --#
jz __errorNull                     #-- ./play.lat:54:16 --#
movq 20 (%RDI), %RAX               #-- load address of vtable at ./play.lat:54:16 --#
call * 32 (%RAX)                   #-- call concat at ./play.lat:54:16 --#
addq $0, %RSP                      #-- ./play.lat:54:16 --#
movq %RAX, %RAX                    #-- ./play.lat:54:16 --#
movq %RAX, %RDI                    #-- passing arg at ./play.lat:54:16 --#
movq %R13, %RSI                    #-- passing arg at ./play.lat:54:16 --#
subq $0, %RSP                      #-- ./play.lat:54:16 --#
testq %RDI, %RDI                   #-- ./play.lat:54:16 --#
jz __errorNull                     #-- ./play.lat:54:16 --#
movq 20 (%RDI), %RAX               #-- load address of vtable at ./play.lat:54:16 --#
call * 32 (%RAX)                   #-- call concat at ./play.lat:54:16 --#
addq $0, %RSP                      #-- ./play.lat:54:16 --#
movq %RAX, %RAX                    #-- ./play.lat:54:16 --#
movq %RAX, %RDI                    #-- passing arg at ./play.lat:54:16 --#
movq %R14, %RSI                    #-- passing arg at ./play.lat:54:16 --#
subq $0, %RSP                      #-- ./play.lat:54:16 --#
testq %RDI, %RDI                   #-- ./play.lat:54:16 --#
jz __errorNull                     #-- ./play.lat:54:16 --#
movq 20 (%RDI), %RAX               #-- load address of vtable at ./play.lat:54:16 --#
call * 32 (%RAX)                   #-- call concat at ./play.lat:54:16 --#
addq $0, %RSP                      #-- ./play.lat:54:16 --#
movq %RAX, %RAX                    #-- ./play.lat:54:16 --#
movq %RAX, %RAX                    #-- move return value at ./play.lat:53:5 --#
addq $0, %RSP                      #-- ./play.lat:53:5 --#
leave                              #-- ./play.lat:53:5 --#
addq $8, %RSP                      #-- ./play.lat:53:5 --#
pop %R15                           #-- ./play.lat:53:5 --#
pop %R14                           #-- ./play.lat:53:5 --#
pop %R13                           #-- ./play.lat:53:5 --#
pop %R12                           #-- ./play.lat:53:5 --#
pop %RBX                           #-- ./play.lat:53:5 --#
ret                                #-- ./play.lat:53:5 --#
Operator.toString :                #-- ./play.lat:50:5 --#
pushq %RBP                         #-- ./play.lat:50:5 --#
movq %RSP, %RBP                    #-- ./play.lat:50:5 --#
subq $0, %RSP                      #-- ./play.lat:50:5 --#
Operator.toString.L_entry :        #-- ./play.lat:50:5 --#
movq %RDI, %RCX                    #-- load %v_t_65 at ./play.lat:50:5 --#
leaq __const_6 (%RIP), %RAX        #-- ./play.lat:51:33 --#
pushq %RCX                         #-- save caller saved at ./play.lat:51:33 --#
movq %RAX, %RDI                    #-- passing arg at ./play.lat:51:33 --#
subq $0, %RSP                      #-- ./play.lat:51:33 --#
call __createString                #-- ./play.lat:51:33 --#
addq $0, %RSP                      #-- ./play.lat:51:33 --#
movq %RAX, %RAX                    #-- ./play.lat:51:33 --#
pop %RCX                           #-- ./play.lat:51:33 --#
movq %RCX, %RDI                    #-- passing arg at ./play.lat:51:16 --#
movq %RAX, %RSI                    #-- passing arg at ./play.lat:51:16 --#
subq $0, %RSP                      #-- ./play.lat:51:16 --#
testq %RDI, %RDI                   #-- ./play.lat:51:16 --#
jz __errorNull                     #-- ./play.lat:51:16 --#
movq 20 (%RDI), %RAX               #-- load address of vtable at ./play.lat:51:16 --#
call * 40 (%RAX)                   #-- call toStringFor at ./play.lat:51:16 --#
addq $0, %RSP                      #-- ./play.lat:51:16 --#
movq %RAX, %RAX                    #-- ./play.lat:51:16 --#
movq %RAX, %RAX                    #-- move return value at ./play.lat:50:5 --#
addq $0, %RSP                      #-- ./play.lat:50:5 --#
leave                              #-- ./play.lat:50:5 --#
ret                                #-- ./play.lat:50:5 --#
Operator.value :                   #-- ./play.lat:42:5 --#
pushq %RBP                         #-- ./play.lat:42:5 --#
movq %RSP, %RBP                    #-- ./play.lat:42:5 --#
subq $0, %RSP                      #-- ./play.lat:42:5 --#
Operator.value.L_entry :           #-- ./play.lat:42:5 --#
leaq __const_7 (%RIP), %RAX        #-- ./play.lat:43:21 --#
movq %RAX, %RDI                    #-- passing arg at ./play.lat:43:21 --#
subq $0, %RSP                      #-- ./play.lat:43:21 --#
call __createString                #-- ./play.lat:43:21 --#
addq $0, %RSP                      #-- ./play.lat:43:21 --#
movq %RAX, %RAX                    #-- ./play.lat:43:21 --#
movq %RAX, %RDI                    #-- passing arg at ./play.lat:43:9 --#
subq $0, %RSP                      #-- ./play.lat:43:9 --#
call printString                   #-- ./play.lat:43:9 --#
addq $0, %RSP                      #-- ./play.lat:43:9 --#
movl $0, %EAX                      #-- move return value at ./play.lat:42:5 --#
addq $0, %RSP                      #-- ./play.lat:42:5 --#
leave                              #-- ./play.lat:42:5 --#
ret                                #-- ./play.lat:42:5 --#
Operator.operator :                #-- ./play.lat:38:5 --#
pushq %RBP                         #-- ./play.lat:38:5 --#
movq %RSP, %RBP                    #-- ./play.lat:38:5 --#
subq $0, %RSP                      #-- ./play.lat:38:5 --#
Operator.operator.L_entry :        #-- ./play.lat:38:5 --#
leaq __const_8 (%RIP), %RAX        #-- ./play.lat:39:21 --#
movq %RAX, %RDI                    #-- passing arg at ./play.lat:39:21 --#
subq $0, %RSP                      #-- ./play.lat:39:21 --#
call __createString                #-- ./play.lat:39:21 --#
addq $0, %RSP                      #-- ./play.lat:39:21 --#
movq %RAX, %RAX                    #-- ./play.lat:39:21 --#
movq %RAX, %RDI                    #-- passing arg at ./play.lat:39:9 --#
subq $0, %RSP                      #-- ./play.lat:39:9 --#
call printString                   #-- ./play.lat:39:9 --#
addq $0, %RSP                      #-- ./play.lat:39:9 --#
movl $0, %EAX                      #-- move return value at ./play.lat:38:5 --#
addq $0, %RSP                      #-- ./play.lat:38:5 --#
leave                              #-- ./play.lat:38:5 --#
ret                                #-- ./play.lat:38:5 --#
Liczba.toString :                  #-- ./play.lat:31:5 --#
pushq %RBX                         #-- ./play.lat:31:5 --#
subq $8, %RSP                      #-- ./play.lat:31:5 --#
pushq %RBP                         #-- ./play.lat:31:5 --#
movq %RSP, %RBP                    #-- ./play.lat:31:5 --#
subq $0, %RSP                      #-- ./play.lat:31:5 --#
Liczba.toString.L_entry :          #-- ./play.lat:31:5 --#
movq %RDI, %RAX                    #-- load %v_t_44 at ./play.lat:31:5 --#
leaq __const_3 (%RIP), %RBX        #-- ./play.lat:32:33 --#
pushq %RAX                         #-- save caller saved at ./play.lat:32:33 --#
movq %RBX, %RDI                    #-- passing arg at ./play.lat:32:33 --#
subq $0, %RSP                      #-- ./play.lat:32:33 --#
call __createString                #-- ./play.lat:32:33 --#
addq $0, %RSP                      #-- ./play.lat:32:33 --#
movq %RAX, %RBX                    #-- ./play.lat:32:33 --#
pop %RAX                           #-- ./play.lat:32:33 --#
movl 36 (%RAX), %EAX               #-- load %v_t_47 at ./play.lat:32:26 --#
movl %EAX, %EDI                    #-- passing arg at inside standard library --#
subq $0, %RSP                      #-- inside standard library --#
call intToString                   #-- inside standard library --#
addq $0, %RSP                      #-- inside standard library --#
movq %RAX, %RAX                    #-- inside standard library --#
leaq __const_9 (%RIP), %RCX        #-- ./play.lat:32:16 --#
pushq %RAX                         #-- save caller saved at ./play.lat:32:16 --#
movq %RCX, %RDI                    #-- passing arg at ./play.lat:32:16 --#
subq $0, %RSP                      #-- ./play.lat:32:16 --#
call __createString                #-- ./play.lat:32:16 --#
addq $0, %RSP                      #-- ./play.lat:32:16 --#
movq %RAX, %RCX                    #-- ./play.lat:32:16 --#
pop %RAX                           #-- ./play.lat:32:16 --#
movq %RCX, %RDI                    #-- passing arg at ./play.lat:32:16 --#
movq %RAX, %RSI                    #-- passing arg at ./play.lat:32:16 --#
subq $0, %RSP                      #-- ./play.lat:32:16 --#
testq %RDI, %RDI                   #-- ./play.lat:32:16 --#
jz __errorNull                     #-- ./play.lat:32:16 --#
movq 20 (%RDI), %RAX               #-- load address of vtable at ./play.lat:32:16 --#
call * 32 (%RAX)                   #-- call concat at ./play.lat:32:16 --#
addq $0, %RSP                      #-- ./play.lat:32:16 --#
movq %RAX, %RAX                    #-- ./play.lat:32:16 --#
movq %RAX, %RDI                    #-- passing arg at ./play.lat:32:16 --#
movq %RBX, %RSI                    #-- passing arg at ./play.lat:32:16 --#
subq $0, %RSP                      #-- ./play.lat:32:16 --#
testq %RDI, %RDI                   #-- ./play.lat:32:16 --#
jz __errorNull                     #-- ./play.lat:32:16 --#
movq 20 (%RDI), %RAX               #-- load address of vtable at ./play.lat:32:16 --#
call * 32 (%RAX)                   #-- call concat at ./play.lat:32:16 --#
addq $0, %RSP                      #-- ./play.lat:32:16 --#
movq %RAX, %RAX                    #-- ./play.lat:32:16 --#
movq %RAX, %RAX                    #-- move return value at ./play.lat:31:5 --#
addq $0, %RSP                      #-- ./play.lat:31:5 --#
leave                              #-- ./play.lat:31:5 --#
addq $8, %RSP                      #-- ./play.lat:31:5 --#
pop %RBX                           #-- ./play.lat:31:5 --#
ret                                #-- ./play.lat:31:5 --#
Liczba.value :                     #-- ./play.lat:28:5 --#
pushq %RBP                         #-- ./play.lat:28:5 --#
movq %RSP, %RBP                    #-- ./play.lat:28:5 --#
subq $0, %RSP                      #-- ./play.lat:28:5 --#
Liczba.value.L_entry :             #-- ./play.lat:28:5 --#
movl $42, %EAX                     #-- move return value at ./play.lat:28:5 --#
addq $0, %RSP                      #-- ./play.lat:28:5 --#
leave                              #-- ./play.lat:28:5 --#
ret                                #-- ./play.lat:28:5 --#
Node.toString :                    #-- ./play.lat:22:5 --#
pushq %RBP                         #-- ./play.lat:22:5 --#
movq %RSP, %RBP                    #-- ./play.lat:22:5 --#
subq $0, %RSP                      #-- ./play.lat:22:5 --#
Node.toString.L_entry :            #-- ./play.lat:22:5 --#
leaq __const_10 (%RIP), %RAX       #-- ./play.lat:23:16 --#
movq %RAX, %RDI                    #-- passing arg at ./play.lat:23:16 --#
subq $0, %RSP                      #-- ./play.lat:23:16 --#
call __createString                #-- ./play.lat:23:16 --#
addq $0, %RSP                      #-- ./play.lat:23:16 --#
movq %RAX, %RAX                    #-- ./play.lat:23:16 --#
movq %RAX, %RAX                    #-- move return value at ./play.lat:22:5 --#
addq $0, %RSP                      #-- ./play.lat:22:5 --#
leave                              #-- ./play.lat:22:5 --#
ret                                #-- ./play.lat:22:5 --#
Node.value :                       #-- ./play.lat:19:5 --#
pushq %RBP                         #-- ./play.lat:19:5 --#
movq %RSP, %RBP                    #-- ./play.lat:19:5 --#
subq $0, %RSP                      #-- ./play.lat:19:5 --#
Node.value.L_entry :               #-- ./play.lat:19:5 --#
movl $0, %EAX                      #-- move return value at ./play.lat:19:5 --#
addq $0, %RSP                      #-- ./play.lat:19:5 --#
leave                              #-- ./play.lat:19:5 --#
ret                                #-- ./play.lat:19:5 --#
__cl_TopLevel.liczba :             #-- ./play.lat:13:1 --#
pushq %RBP                         #-- ./play.lat:13:1 --#
movq %RSP, %RBP                    #-- ./play.lat:13:1 --#
subq $0, %RSP                      #-- ./play.lat:13:1 --#
__cl_TopLevel.liczba.L_entry :     #-- ./play.lat:13:1 --#
movl %EDI, %ECX                    #-- load %v_t_33 at ./play.lat:13:1 --#
pushq %RCX                         #-- save caller saved at ./play.lat:14:18 --#
leaq _class_Liczba (%RIP), %RDI    #-- ./play.lat:14:18 --#
subq $0, %RSP                      #-- ./play.lat:14:18 --#
call __new                         #-- ./play.lat:14:18 --#
addq $0, %RSP                      #-- ./play.lat:14:18 --#
movq %RAX, %RAX                    #-- ./play.lat:14:18 --#
pop %RCX                           #-- ./play.lat:14:18 --#
movl %ECX, 36 (%RAX)               #-- ./play.lat:15:5 --#
movq %RAX, %RDI                    #-- passing arg at ./play.lat:13:1 --#
leaq _class_Node (%RIP), %RSI      #-- ./play.lat:13:1 --#
subq $0, %RSP                      #-- ./play.lat:13:1 --#
call __cast                        #-- ./play.lat:13:1 --#
addq $0, %RSP                      #-- ./play.lat:13:1 --#
movq %RAX, %RAX                    #-- ./play.lat:13:1 --#
movq %RAX, %RAX                    #-- move return value at ./play.lat:13:1 --#
addq $0, %RSP                      #-- ./play.lat:13:1 --#
leave                              #-- ./play.lat:13:1 --#
ret                                #-- ./play.lat:13:1 --#
__cl_TopLevel.plus :               #-- ./play.lat:7:1 --#
pushq %RBX                         #-- ./play.lat:7:1 --#
pushq %R12                         #-- ./play.lat:7:1 --#
pushq %RBP                         #-- ./play.lat:7:1 --#
movq %RSP, %RBP                    #-- ./play.lat:7:1 --#
subq $0, %RSP                      #-- ./play.lat:7:1 --#
__cl_TopLevel.plus.L_entry :       #-- ./play.lat:7:1 --#
movq %RDI, %R12                    #-- load %v_t_27 at ./play.lat:7:1 --#
movq %RSI, %RBX                    #-- load %v_t_28 at ./play.lat:7:1 --#
leaq _class_Plus (%RIP), %RDI      #-- ./play.lat:8:20 --#
subq $0, %RSP                      #-- ./play.lat:8:20 --#
call __new                         #-- ./play.lat:8:20 --#
addq $0, %RSP                      #-- ./play.lat:8:20 --#
movq %RAX, %RAX                    #-- ./play.lat:8:20 --#
movq %RAX, %RDI                    #-- passing arg at ./play.lat:8:5 --#
leaq _class_Operator (%RIP), %RSI  #-- ./play.lat:8:5 --#
subq $0, %RSP                      #-- ./play.lat:8:5 --#
call __cast                        #-- ./play.lat:8:5 --#
addq $0, %RSP                      #-- ./play.lat:8:5 --#
movq %RAX, %RAX                    #-- ./play.lat:8:5 --#
movq %R12, 36 (%RAX)               #-- ./play.lat:9:5 --#
movq %RBX, 44 (%RAX)               #-- ./play.lat:10:5 --#
movq %RAX, %RDI                    #-- passing arg at ./play.lat:7:1 --#
leaq _class_Node (%RIP), %RSI      #-- ./play.lat:7:1 --#
subq $0, %RSP                      #-- ./play.lat:7:1 --#
call __cast                        #-- ./play.lat:7:1 --#
addq $0, %RSP                      #-- ./play.lat:7:1 --#
movq %RAX, %RAX                    #-- ./play.lat:7:1 --#
movq %RAX, %RAX                    #-- move return value at ./play.lat:7:1 --#
addq $0, %RSP                      #-- ./play.lat:7:1 --#
leave                              #-- ./play.lat:7:1 --#
pop %R12                           #-- ./play.lat:7:1 --#
pop %RBX                           #-- ./play.lat:7:1 --#
ret                                #-- ./play.lat:7:1 --#
main :                             #-- ./play.lat:1:1 --#
__cl_TopLevel.main :               #-- ./play.lat:1:1 --#
pushq %RBX                         #-- ./play.lat:1:1 --#
subq $8, %RSP                      #-- ./play.lat:1:1 --#
pushq %RBP                         #-- ./play.lat:1:1 --#
movq %RSP, %RBP                    #-- ./play.lat:1:1 --#
subq $0, %RSP                      #-- ./play.lat:1:1 --#
__cl_TopLevel.main.L_entry :       #-- ./play.lat:1:1 --#
movl $69, %EDI                     #-- passing arg at ./play.lat:2:19 --#
subq $0, %RSP                      #-- ./play.lat:2:19 --#
call __cl_TopLevel.liczba          #-- ./play.lat:2:19 --#
addq $0, %RSP                      #-- ./play.lat:2:19 --#
movq %RAX, %RBX                    #-- ./play.lat:2:19 --#
movl $42, %EDI                     #-- passing arg at ./play.lat:2:31 --#
subq $0, %RSP                      #-- ./play.lat:2:31 --#
call __cl_TopLevel.liczba          #-- ./play.lat:2:31 --#
addq $0, %RSP                      #-- ./play.lat:2:31 --#
movq %RAX, %RAX                    #-- ./play.lat:2:31 --#
movq %RBX, %RDI                    #-- passing arg at ./play.lat:2:14 --#
movq %RAX, %RSI                    #-- passing arg at ./play.lat:2:14 --#
subq $0, %RSP                      #-- ./play.lat:2:14 --#
call __cl_TopLevel.plus            #-- ./play.lat:2:14 --#
addq $0, %RSP                      #-- ./play.lat:2:14 --#
movq %RAX, %RAX                    #-- ./play.lat:2:14 --#
movq %RAX, %RDI                    #-- passing arg at ./play.lat:4:15 --#
subq $0, %RSP                      #-- ./play.lat:4:15 --#
testq %RDI, %RDI                   #-- ./play.lat:4:15 --#
jz __errorNull                     #-- ./play.lat:4:15 --#
movq 20 (%RDI), %RAX               #-- load address of vtable at ./play.lat:4:15 --#
call * 24 (%RAX)                   #-- call value at ./play.lat:4:15 --#
addq $0, %RSP                      #-- ./play.lat:4:15 --#
movl $0, %EAX                      #-- move return value at ./play.lat:1:1 --#
addq $0, %RSP                      #-- ./play.lat:1:1 --#
leave                              #-- ./play.lat:1:1 --#
addq $8, %RSP                      #-- ./play.lat:1:1 --#
pop %RBX                           #-- ./play.lat:1:1 --#
ret                                #-- ./play.lat:1:1 --#
__errorNull :                      #-- runtime error on null dereference at ./play.lat:1:1 --#
andq $-16, %RSP                    #-- 16 bytes allign at ./play.lat:1:1 --#
call __errorNull                   #-- ./play.lat:1:1 --#
