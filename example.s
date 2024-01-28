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
.string "I'm a shape"

__const_4 :
.string "I'm just a shape"

__const_2 :
.string "I'm really a circle"

__const_3 :
.string "I'm really a rectangle"

__const_1 :
.string "I'm really a square"

.global _class_Array
_class_Array :
.quad _class_Object
.quad _class_Array_initializer
.long 16
.quad _class_Array_methods
.long 0
.quad 0

.global _class_Array_methods
_class_Array_methods :
.quad _Array_toString
.quad _Object_getHashCode
.quad _Object_equals

.global _class_Array_initializer
_class_Array_initializer :
.long 0
.long 0
.quad _LAT_NULL

.global _class_Circle
_class_Circle :
.quad _class_Shape
.quad _class_Circle_initializer
.long 0
.quad _class_Circle_methods
.long 0
.quad 0

.global _class_Circle_methods
_class_Circle_methods :
.quad _Object_toString
.quad _Object_getHashCode
.quad _Object_equals
.quad Shape.tell
.quad Circle.tellAgain

.global _class_Circle_initializer
_class_Circle_initializer :

.global _class_Node
_class_Node :
.quad _class_Object
.quad _class_Node_initializer
.long 16
.quad _class_Node_methods
.long 0
.quad 0

.global _class_Node_methods
_class_Node_methods :
.quad _Object_toString
.quad _Object_getHashCode
.quad _Object_equals
.quad Node.setElem
.quad Node.setNext
.quad Node.getElem
.quad Node.getNext

.global _class_Node_initializer
_class_Node_initializer :
.quad _LAT_NULL
.quad _LAT_NULL

.global _class_Object
_class_Object :
.quad 0
.quad _class_Object_initializer
.long 0
.quad _class_Object_methods
.long 0
.quad 0

.global _class_Object_methods
_class_Object_methods :
.quad _Object_toString
.quad _Object_getHashCode
.quad _Object_equals

.global _class_Object_initializer
_class_Object_initializer :

.global _class_Rectangle
_class_Rectangle :
.quad _class_Shape
.quad _class_Rectangle_initializer
.long 0
.quad _class_Rectangle_methods
.long 0
.quad 0

.global _class_Rectangle_methods
_class_Rectangle_methods :
.quad _Object_toString
.quad _Object_getHashCode
.quad _Object_equals
.quad Shape.tell
.quad Rectangle.tellAgain

.global _class_Rectangle_initializer
_class_Rectangle_initializer :

.global _class_Shape
_class_Shape :
.quad _class_Object
.quad _class_Shape_initializer
.long 0
.quad _class_Shape_methods
.long 0
.quad 0

.global _class_Shape_methods
_class_Shape_methods :
.quad _Object_toString
.quad _Object_getHashCode
.quad _Object_equals
.quad Shape.tell
.quad Shape.tellAgain

.global _class_Shape_initializer
_class_Shape_initializer :

.global _class_Square
_class_Square :
.quad _class_Rectangle
.quad _class_Square_initializer
.long 0
.quad _class_Square_methods
.long 0
.quad 0

.global _class_Square_methods
_class_Square_methods :
.quad _Object_toString
.quad _Object_getHashCode
.quad _Object_equals
.quad Shape.tell
.quad Square.tellAgain

.global _class_Square_initializer
_class_Square_initializer :

.global _class_Stack
_class_Stack :
.quad _class_Object
.quad _class_Stack_initializer
.long 8
.quad _class_Stack_methods
.long 0
.quad 0

.global _class_Stack_methods
_class_Stack_methods :
.quad _Object_toString
.quad _Object_getHashCode
.quad _Object_equals
.quad Stack.push
.quad Stack.isEmpty
.quad Stack.top
.quad Stack.pop

.global _class_Stack_initializer
_class_Stack_initializer :
.quad _LAT_NULL

.global _class_String
_class_String :
.quad _class_Object
.quad _class_String_initializer
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

.global _class_String_initializer
_class_String_initializer :

.global main

.section .data
.global _LAT_NULL
_LAT_NULL :
.quad 0
.quad 0
.long 0
.quad 0
.long 0
.long 0

_LAT_NULL_ADDR :
.quad _LAT_NULL


.section .text
main :                               #-- example.lat:55:1 --#
__cl_TopLevel.main :                 #-- example.lat:55:1 --#
pushq %RBX                           #-- example.lat:55:1 --#
subq $8, %RSP                        #-- example.lat:55:1 --#
pushq %RBP                           #-- example.lat:55:1 --#
movq %RSP, %RBP                      #-- example.lat:55:1 --#
__cl_TopLevel.main.L_entry :         #-- example.lat:55:1 --#
call __cl_TopLevel.genNull           #-- example.lat:56:16 --#
movq %RAX, %RBX                      #-- example.lat:56:16 --#
incl 16 (%RBX)                       #-- incr ref count on VVal example.lat:56:10 (Ref example.lat:56:10 (Cl example.lat:56:10 (IRTargetRefName "Node"))) (IRValueName "%v_t_66") at example.lat:56:10 --#
decl 16 (%RBX)                       #-- incr ref count on VVal example.lat:56:10 (Ref example.lat:51:1 (Cl example.lat:51:1 (IRTargetRefName "Node"))) (IRValueName "%v_t_66") at example.lat:56:10 --#
movq %RBX, %RDI                      #-- passing arg at example.lat:57:5 --#
call print                           #-- example.lat:57:5 --#
decl 16 (%RBX)                       #-- incr ref count on VVal example.lat:57:5 (Ref example.lat:57:5 (Cl example.lat:57:5 (IRTargetRefName "Node"))) (IRValueName "%v_t_66") at example.lat:57:5 --#
xorl %EAX, %EAX                      #-- move return value at example.lat:55:1 --#
leave                                #-- example.lat:55:1 --#
addq $8, %RSP                        #-- example.lat:55:1 --#
pop %RBX                             #-- example.lat:55:1 --#
ret                                  #-- example.lat:55:1 --#
__cl_TopLevel.genNull :              #-- example.lat:51:1 --#
pushq %RBP                           #-- example.lat:51:1 --#
movq %RSP, %RBP                      #-- example.lat:51:1 --#
__cl_TopLevel.genNull.L_entry :      #-- example.lat:51:1 --#
movq _LAT_NULL_ADDR (%RIP), %RAX     #-- move return value at example.lat:51:1 --#
leave                                #-- example.lat:51:1 --#
ret                                  #-- example.lat:51:1 --#
Square.tellAgain :                   #-- example.lat:46:5 --#
pushq %RBX                           #-- example.lat:46:5 --#
pushq %R12                           #-- example.lat:46:5 --#
pushq %RBP                           #-- example.lat:46:5 --#
movq %RSP, %RBP                      #-- example.lat:46:5 --#
Square.tellAgain.L_entry :           #-- example.lat:46:5 --#
leaq __const_1 (%RIP), %RBX          #-- example.lat:47:21 --#
movq %RBX, %RDI                      #-- passing arg at example.lat:47:21 --#
call __createString                  #-- example.lat:47:21 --#
movq %RAX, %RBX                      #-- example.lat:47:21 --#
incl 16 (%RBX)                       #-- incr ref count on VVal example.lat:47:21 (Ref example.lat:47:21 (Cl example.lat:47:21 (IRTargetRefName "String"))) (IRValueName "%v_t_63") at example.lat:47:21 --#
movq %RBX, %RDI                      #-- passing arg at example.lat:47:9 --#
call printString                     #-- example.lat:47:9 --#
decl 16 (%RBX)                       #-- incr ref count on VVal example.lat:47:9 (Ref example.lat:47:21 (Cl example.lat:47:21 (IRTargetRefName "String"))) (IRValueName "%v_t_63") at example.lat:47:9 --#
movl %R12D, %EAX                     #-- move return value at example.lat:46:5 --#
leave                                #-- example.lat:46:5 --#
pop %R12                             #-- example.lat:46:5 --#
pop %RBX                             #-- example.lat:46:5 --#
ret                                  #-- example.lat:46:5 --#
Circle.tellAgain :                   #-- example.lat:41:5 --#
pushq %RBX                           #-- example.lat:41:5 --#
pushq %R12                           #-- example.lat:41:5 --#
pushq %RBP                           #-- example.lat:41:5 --#
movq %RSP, %RBP                      #-- example.lat:41:5 --#
Circle.tellAgain.L_entry :           #-- example.lat:41:5 --#
leaq __const_2 (%RIP), %RBX          #-- example.lat:42:21 --#
movq %RBX, %RDI                      #-- passing arg at example.lat:42:21 --#
call __createString                  #-- example.lat:42:21 --#
movq %RAX, %RBX                      #-- example.lat:42:21 --#
incl 16 (%RBX)                       #-- incr ref count on VVal example.lat:42:21 (Ref example.lat:42:21 (Cl example.lat:42:21 (IRTargetRefName "String"))) (IRValueName "%v_t_59") at example.lat:42:21 --#
movq %RBX, %RDI                      #-- passing arg at example.lat:42:9 --#
call printString                     #-- example.lat:42:9 --#
decl 16 (%RBX)                       #-- incr ref count on VVal example.lat:42:9 (Ref example.lat:42:21 (Cl example.lat:42:21 (IRTargetRefName "String"))) (IRValueName "%v_t_59") at example.lat:42:9 --#
movl %R12D, %EAX                     #-- move return value at example.lat:41:5 --#
leave                                #-- example.lat:41:5 --#
pop %R12                             #-- example.lat:41:5 --#
pop %RBX                             #-- example.lat:41:5 --#
ret                                  #-- example.lat:41:5 --#
Rectangle.tellAgain :                #-- example.lat:36:5 --#
pushq %RBX                           #-- example.lat:36:5 --#
pushq %R12                           #-- example.lat:36:5 --#
pushq %RBP                           #-- example.lat:36:5 --#
movq %RSP, %RBP                      #-- example.lat:36:5 --#
Rectangle.tellAgain.L_entry :        #-- example.lat:36:5 --#
leaq __const_3 (%RIP), %RBX          #-- example.lat:37:21 --#
movq %RBX, %RDI                      #-- passing arg at example.lat:37:21 --#
call __createString                  #-- example.lat:37:21 --#
movq %RAX, %RBX                      #-- example.lat:37:21 --#
incl 16 (%RBX)                       #-- incr ref count on VVal example.lat:37:21 (Ref example.lat:37:21 (Cl example.lat:37:21 (IRTargetRefName "String"))) (IRValueName "%v_t_55") at example.lat:37:21 --#
movq %RBX, %RDI                      #-- passing arg at example.lat:37:9 --#
call printString                     #-- example.lat:37:9 --#
decl 16 (%RBX)                       #-- incr ref count on VVal example.lat:37:9 (Ref example.lat:37:21 (Cl example.lat:37:21 (IRTargetRefName "String"))) (IRValueName "%v_t_55") at example.lat:37:9 --#
movl %R12D, %EAX                     #-- move return value at example.lat:36:5 --#
leave                                #-- example.lat:36:5 --#
pop %R12                             #-- example.lat:36:5 --#
pop %RBX                             #-- example.lat:36:5 --#
ret                                  #-- example.lat:36:5 --#
Shape.tellAgain :                    #-- example.lat:31:5 --#
pushq %RBX                           #-- example.lat:31:5 --#
pushq %R12                           #-- example.lat:31:5 --#
pushq %RBP                           #-- example.lat:31:5 --#
movq %RSP, %RBP                      #-- example.lat:31:5 --#
Shape.tellAgain.L_entry :            #-- example.lat:31:5 --#
leaq __const_4 (%RIP), %RBX          #-- example.lat:32:21 --#
movq %RBX, %RDI                      #-- passing arg at example.lat:32:21 --#
call __createString                  #-- example.lat:32:21 --#
movq %RAX, %RBX                      #-- example.lat:32:21 --#
incl 16 (%RBX)                       #-- incr ref count on VVal example.lat:32:21 (Ref example.lat:32:21 (Cl example.lat:32:21 (IRTargetRefName "String"))) (IRValueName "%v_t_51") at example.lat:32:21 --#
movq %RBX, %RDI                      #-- passing arg at example.lat:32:9 --#
call printString                     #-- example.lat:32:9 --#
decl 16 (%RBX)                       #-- incr ref count on VVal example.lat:32:9 (Ref example.lat:32:21 (Cl example.lat:32:21 (IRTargetRefName "String"))) (IRValueName "%v_t_51") at example.lat:32:9 --#
movl %R12D, %EAX                     #-- move return value at example.lat:31:5 --#
leave                                #-- example.lat:31:5 --#
pop %R12                             #-- example.lat:31:5 --#
pop %RBX                             #-- example.lat:31:5 --#
ret                                  #-- example.lat:31:5 --#
Shape.tell :                         #-- example.lat:28:5 --#
pushq %RBX                           #-- example.lat:28:5 --#
pushq %R12                           #-- example.lat:28:5 --#
pushq %RBP                           #-- example.lat:28:5 --#
movq %RSP, %RBP                      #-- example.lat:28:5 --#
Shape.tell.L_entry :                 #-- example.lat:28:5 --#
leaq __const_5 (%RIP), %RBX          #-- example.lat:29:21 --#
movq %RBX, %RDI                      #-- passing arg at example.lat:29:21 --#
call __createString                  #-- example.lat:29:21 --#
movq %RAX, %RBX                      #-- example.lat:29:21 --#
incl 16 (%RBX)                       #-- incr ref count on VVal example.lat:29:21 (Ref example.lat:29:21 (Cl example.lat:29:21 (IRTargetRefName "String"))) (IRValueName "%v_t_47") at example.lat:29:21 --#
movq %RBX, %RDI                      #-- passing arg at example.lat:29:9 --#
call printString                     #-- example.lat:29:9 --#
decl 16 (%RBX)                       #-- incr ref count on VVal example.lat:29:9 (Ref example.lat:29:21 (Cl example.lat:29:21 (IRTargetRefName "String"))) (IRValueName "%v_t_47") at example.lat:29:9 --#
movl %R12D, %EAX                     #-- move return value at example.lat:28:5 --#
leave                                #-- example.lat:28:5 --#
pop %R12                             #-- example.lat:28:5 --#
pop %RBX                             #-- example.lat:28:5 --#
ret                                  #-- example.lat:28:5 --#
Stack.pop :                          #-- example.lat:23:5 --#
pushq %RBX                           #-- example.lat:23:5 --#
pushq %R12                           #-- example.lat:23:5 --#
pushq %R13                           #-- example.lat:23:5 --#
subq $8, %RSP                        #-- example.lat:23:5 --#
pushq %RBP                           #-- example.lat:23:5 --#
movq %RSP, %RBP                      #-- example.lat:23:5 --#
Stack.pop.L_entry :                  #-- example.lat:23:5 --#
movq %RDI, %R12                      #-- load %v_t_42 at example.lat:23:5 --#
movq 36 (%R12), %R13                 #-- load %v_t_43 at example.lat:24:16 --#
incl 16 (%R13)                       #-- incr ref count on VVal example.lat:24:16 (Ref example.lat:10:5 (Cl example.lat:10:5 (IRTargetRefName "Node"))) (IRValueName "%v_t_43") at example.lat:24:16 --#
decl 16 (%R12)                       #-- incr ref count on VVal example.lat:24:16 (Ref  (Cl  (IRTargetRefName "Stack"))) (IRValueName "%v_t_42") at example.lat:24:16 --#
movq %R13, %RDI                      #-- passing arg at example.lat:24:16 --#
cmpq $0, 0 (%RDI)                    #-- example.lat:24:16 --#
jz __handleErrorNull                 #-- example.lat:24:16 --#
movq 20 (%RDI), %RAX                 #-- load address of vtable at example.lat:24:16 --#
call * 48 (%RAX)                     #-- call getNext at example.lat:24:16 --#
decl 16 (%R13)                       #-- incr ref count on VVal example.lat:24:16 (Ref example.lat:10:5 (Cl example.lat:10:5 (IRTargetRefName "Node"))) (IRValueName "%v_t_43") at example.lat:24:16 --#
movq 36 (%R12), %RCX                 #-- load %v_c6 at example.lat:24:9 --#
decl 16 (%RCX)                       #-- incr ref count on VVal example.lat:24:9 (Ref example.lat:10:5 (Cl example.lat:10:5 (IRTargetRefName "Node"))) (IRValueName "%v_c6") at example.lat:24:9 --#
incl 16 (%RAX)                       #-- incr ref count on VVal example.lat:24:9 (Ref example.lat:24:9 (Cl example.lat:24:9 (IRTargetRefName "Node"))) (IRValueName "%v_t_44") at example.lat:24:9 --#
movq %RAX, 36 (%R12)                 #-- example.lat:24:9 --#
decl 16 (%R12)                       #-- incr ref count on VVal example.lat:24:9 (Ref  (Cl  (IRTargetRefName "Stack"))) (IRValueName "%v_t_42") at example.lat:24:9 --#
decl 16 (%RAX)                       #-- incr ref count on VVal example.lat:24:9 (Ref example.lat:7:5 (Cl example.lat:7:5 (IRTargetRefName "Node"))) (IRValueName "%v_t_44") at example.lat:24:9 --#
movl %EBX, %EAX                      #-- move return value at example.lat:23:5 --#
leave                                #-- example.lat:23:5 --#
addq $8, %RSP                        #-- example.lat:23:5 --#
pop %R13                             #-- example.lat:23:5 --#
pop %R12                             #-- example.lat:23:5 --#
pop %RBX                             #-- example.lat:23:5 --#
ret                                  #-- example.lat:23:5 --#
Stack.top :                          #-- example.lat:20:5 --#
pushq %RBX                           #-- example.lat:20:5 --#
subq $8, %RSP                        #-- example.lat:20:5 --#
pushq %RBP                           #-- example.lat:20:5 --#
movq %RSP, %RBP                      #-- example.lat:20:5 --#
Stack.top.L_entry :                  #-- example.lat:20:5 --#
movq %RDI, %RAX                      #-- load %v_t_39 at example.lat:20:5 --#
movq 36 (%RAX), %RBX                 #-- load %v_t_40 at example.lat:21:16 --#
incl 16 (%RBX)                       #-- incr ref count on VVal example.lat:21:16 (Ref example.lat:10:5 (Cl example.lat:10:5 (IRTargetRefName "Node"))) (IRValueName "%v_t_40") at example.lat:21:16 --#
decl 16 (%RAX)                       #-- incr ref count on VVal example.lat:21:16 (Ref  (Cl  (IRTargetRefName "Stack"))) (IRValueName "%v_t_39") at example.lat:21:16 --#
movq %RBX, %RDI                      #-- passing arg at example.lat:21:16 --#
cmpq $0, 0 (%RDI)                    #-- example.lat:21:16 --#
jz __handleErrorNull                 #-- example.lat:21:16 --#
movq 20 (%RDI), %RAX                 #-- load address of vtable at example.lat:21:16 --#
call * 40 (%RAX)                     #-- call getElem at example.lat:21:16 --#
decl 16 (%RBX)                       #-- incr ref count on VVal example.lat:21:16 (Ref example.lat:10:5 (Cl example.lat:10:5 (IRTargetRefName "Node"))) (IRValueName "%v_t_40") at example.lat:21:16 --#
incl 16 (%RAX)                       #-- incr ref count on VVal example.lat:21:9 (Ref example.lat:21:9 (Cl example.lat:21:9 (IRTargetRefName "Shape"))) (IRValueName "%v_t_41") at example.lat:21:9 --#
decl 16 (%RAX)                       #-- incr ref count on VVal example.lat:21:9 (Ref example.lat:6:5 (Cl example.lat:6:5 (IRTargetRefName "Shape"))) (IRValueName "%v_t_41") at example.lat:21:9 --#
leave                                #-- example.lat:20:5 --#
addq $8, %RSP                        #-- example.lat:20:5 --#
pop %RBX                             #-- example.lat:20:5 --#
ret                                  #-- example.lat:20:5 --#
Stack.isEmpty :                      #-- example.lat:17:5 --#
pushq %RBP                           #-- example.lat:17:5 --#
movq %RSP, %RBP                      #-- example.lat:17:5 --#
Stack.isEmpty.L_entry :              #-- example.lat:17:5 --#
movq %RDI, %RCX                      #-- load %v_t_31 at example.lat:17:5 --#
movq 36 (%RCX), %RAX                 #-- load %v_t_35 at example.lat:18:16 --#
incl 16 (%RAX)                       #-- incr ref count on VVal example.lat:18:16 (Ref example.lat:10:5 (Cl example.lat:10:5 (IRTargetRefName "Node"))) (IRValueName "%v_t_35") at example.lat:18:16 --#
decl 16 (%RCX)                       #-- incr ref count on VVal example.lat:18:16 (Ref  (Cl  (IRTargetRefName "Stack"))) (IRValueName "%v_t_31") at example.lat:18:16 --#
decl 16 (%RAX)                       #-- incr ref count on VVal example.lat:18:16 (Ref example.lat:10:5 (Cl example.lat:10:5 (IRTargetRefName "Node"))) (IRValueName "%v_t_35") at example.lat:18:16 --#
cmpq _LAT_NULL_ADDR (%RIP), %RAX     #-- example.lat:18:16 --#
je Stack.isEmpty_IIF32               #-- example.lat:18:16 --#
Stack.isEmpty_IELSE33 :              #-- example.lat:18:9 --#
xorl %EAX, %EAX                      #-- setting %v_return~2 at example.lat:17:5 --#
jmp Stack.isEmpty.L_exit             #-- example.lat:17:5 --#
Stack.isEmpty_IIF32 :                #-- example.lat:18:9 --#
movl $1, %EAX                        #-- setting %v_return~2 at example.lat:17:5 --#
jmp Stack.isEmpty.L_exit             #-- example.lat:17:5 --#
Stack.isEmpty.L_exit :               #-- example.lat:17:5 --#
leave                                #-- example.lat:17:5 --#
ret                                  #-- example.lat:17:5 --#
Stack.push :                         #-- example.lat:11:5 --#
pushq %RBX                           #-- example.lat:11:5 --#
pushq %R12                           #-- example.lat:11:5 --#
pushq %R13                           #-- example.lat:11:5 --#
pushq %R14                           #-- example.lat:11:5 --#
pushq %RBP                           #-- example.lat:11:5 --#
movq %RSP, %RBP                      #-- example.lat:11:5 --#
Stack.push.L_entry :                 #-- example.lat:11:5 --#
movq %RDI, %RBX                      #-- load %v_t_24 at example.lat:11:5 --#
movq %RSI, %R14                      #-- load %v_t_25 at example.lat:11:5 --#
leaq _class_Node (%RIP), %RDI        #-- example.lat:12:24 --#
call __new                           #-- example.lat:12:24 --#
movq %RAX, %R12                      #-- example.lat:12:24 --#
incl 16 (%R12)                       #-- incr ref count on VVal example.lat:12:24 (Ref example.lat:12:28 (Cl example.lat:12:28 (IRTargetRefName "Node"))) (IRValueName "%v_t_26") at example.lat:12:24 --#
incl 16 (%R12)                       #-- incr ref count on VVal example.lat:12:14 (Ref example.lat:12:14 (Cl example.lat:12:14 (IRTargetRefName "Node"))) (IRValueName "%v_t_26") at example.lat:12:14 --#
decl 16 (%R12)                       #-- incr ref count on VVal example.lat:12:14 (Ref example.lat:12:28 (Cl example.lat:12:28 (IRTargetRefName "Node"))) (IRValueName "%v_t_26") at example.lat:12:14 --#
movq %R12, %RDI                      #-- passing arg at example.lat:13:9 --#
movq %R14, %RSI                      #-- passing arg at example.lat:13:9 --#
cmpq $0, 0 (%RDI)                    #-- example.lat:13:9 --#
jz __handleErrorNull                 #-- example.lat:13:9 --#
movq 20 (%RDI), %RAX                 #-- load address of vtable at example.lat:13:9 --#
call * 24 (%RAX)                     #-- call setElem at example.lat:13:9 --#
decl 16 (%R12)                       #-- incr ref count on VVal example.lat:13:9 (Ref example.lat:13:9 (Cl example.lat:13:9 (IRTargetRefName "Node"))) (IRValueName "%v_t_26") at example.lat:13:9 --#
decl 16 (%R14)                       #-- incr ref count on VVal example.lat:13:9 (Ref example.lat:11:15 (Cl example.lat:11:15 (IRTargetRefName "Shape"))) (IRValueName "%v_t_25") at example.lat:13:9 --#
movq 36 (%RBX), %R14                 #-- load %v_t_29 at example.lat:14:25 --#
incl 16 (%R14)                       #-- incr ref count on VVal example.lat:14:25 (Ref example.lat:10:5 (Cl example.lat:10:5 (IRTargetRefName "Node"))) (IRValueName "%v_t_29") at example.lat:14:25 --#
decl 16 (%RBX)                       #-- incr ref count on VVal example.lat:14:25 (Ref  (Cl  (IRTargetRefName "Stack"))) (IRValueName "%v_t_24") at example.lat:14:25 --#
movq %R12, %RDI                      #-- passing arg at example.lat:14:9 --#
movq %R14, %RSI                      #-- passing arg at example.lat:14:9 --#
cmpq $0, 0 (%RDI)                    #-- example.lat:14:9 --#
jz __handleErrorNull                 #-- example.lat:14:9 --#
movq 20 (%RDI), %RAX                 #-- load address of vtable at example.lat:14:9 --#
call * 32 (%RAX)                     #-- call setNext at example.lat:14:9 --#
decl 16 (%R12)                       #-- incr ref count on VVal example.lat:14:9 (Ref example.lat:14:9 (Cl example.lat:14:9 (IRTargetRefName "Node"))) (IRValueName "%v_t_26") at example.lat:14:9 --#
decl 16 (%R14)                       #-- incr ref count on VVal example.lat:14:9 (Ref example.lat:10:5 (Cl example.lat:10:5 (IRTargetRefName "Node"))) (IRValueName "%v_t_29") at example.lat:14:9 --#
movq 36 (%RBX), %RAX                 #-- load %v_c4 at example.lat:15:9 --#
decl 16 (%RAX)                       #-- incr ref count on VVal example.lat:15:9 (Ref example.lat:10:5 (Cl example.lat:10:5 (IRTargetRefName "Node"))) (IRValueName "%v_c4") at example.lat:15:9 --#
incl 16 (%R12)                       #-- incr ref count on VVal example.lat:15:9 (Ref example.lat:15:9 (Cl example.lat:15:9 (IRTargetRefName "Node"))) (IRValueName "%v_t_26") at example.lat:15:9 --#
movq %R12, 36 (%RBX)                 #-- example.lat:15:9 --#
decl 16 (%R12)                       #-- incr ref count on VVal example.lat:15:9 (Ref example.lat:15:9 (Cl example.lat:15:9 (IRTargetRefName "Node"))) (IRValueName "%v_t_26") at example.lat:15:9 --#
decl 16 (%RBX)                       #-- incr ref count on VVal example.lat:15:9 (Ref  (Cl  (IRTargetRefName "Stack"))) (IRValueName "%v_t_24") at example.lat:15:9 --#
movl %R13D, %EAX                     #-- move return value at example.lat:11:5 --#
leave                                #-- example.lat:11:5 --#
pop %R14                             #-- example.lat:11:5 --#
pop %R13                             #-- example.lat:11:5 --#
pop %R12                             #-- example.lat:11:5 --#
pop %RBX                             #-- example.lat:11:5 --#
ret                                  #-- example.lat:11:5 --#
Node.getNext :                       #-- example.lat:7:5 --#
pushq %RBP                           #-- example.lat:7:5 --#
movq %RSP, %RBP                      #-- example.lat:7:5 --#
Node.getNext.L_entry :               #-- example.lat:7:5 --#
movq %RDI, %RCX                      #-- load %v_t_22 at example.lat:7:5 --#
movq 44 (%RCX), %RAX                 #-- load %v_t_23 at example.lat:7:29 --#
incl 16 (%RAX)                       #-- incr ref count on VVal example.lat:7:29 (Ref example.lat:3:5 (Cl example.lat:3:5 (IRTargetRefName "Node"))) (IRValueName "%v_t_23") at example.lat:7:29 --#
decl 16 (%RCX)                       #-- incr ref count on VVal example.lat:7:29 (Ref  (Cl  (IRTargetRefName "Node"))) (IRValueName "%v_t_22") at example.lat:7:29 --#
incl 16 (%RAX)                       #-- incr ref count on VVal example.lat:7:22 (Ref example.lat:7:22 (Cl example.lat:7:22 (IRTargetRefName "Node"))) (IRValueName "%v_t_23") at example.lat:7:22 --#
decl 16 (%RAX)                       #-- incr ref count on VVal example.lat:7:22 (Ref example.lat:3:5 (Cl example.lat:3:5 (IRTargetRefName "Node"))) (IRValueName "%v_t_23") at example.lat:7:22 --#
leave                                #-- example.lat:7:5 --#
ret                                  #-- example.lat:7:5 --#
Node.getElem :                       #-- example.lat:6:5 --#
pushq %RBP                           #-- example.lat:6:5 --#
movq %RSP, %RBP                      #-- example.lat:6:5 --#
Node.getElem.L_entry :               #-- example.lat:6:5 --#
movq %RDI, %RCX                      #-- load %v_t_20 at example.lat:6:5 --#
movq 36 (%RCX), %RAX                 #-- load %v_t_21 at example.lat:6:30 --#
incl 16 (%RAX)                       #-- incr ref count on VVal example.lat:6:30 (Ref example.lat:2:5 (Cl example.lat:2:5 (IRTargetRefName "Shape"))) (IRValueName "%v_t_21") at example.lat:6:30 --#
decl 16 (%RCX)                       #-- incr ref count on VVal example.lat:6:30 (Ref  (Cl  (IRTargetRefName "Node"))) (IRValueName "%v_t_20") at example.lat:6:30 --#
incl 16 (%RAX)                       #-- incr ref count on VVal example.lat:6:23 (Ref example.lat:6:23 (Cl example.lat:6:23 (IRTargetRefName "Shape"))) (IRValueName "%v_t_21") at example.lat:6:23 --#
decl 16 (%RAX)                       #-- incr ref count on VVal example.lat:6:23 (Ref example.lat:2:5 (Cl example.lat:2:5 (IRTargetRefName "Shape"))) (IRValueName "%v_t_21") at example.lat:6:23 --#
leave                                #-- example.lat:6:5 --#
ret                                  #-- example.lat:6:5 --#
Node.setNext :                       #-- example.lat:5:5 --#
pushq %RBP                           #-- example.lat:5:5 --#
movq %RSP, %RBP                      #-- example.lat:5:5 --#
Node.setNext.L_entry :               #-- example.lat:5:5 --#
movq %RDI, %RDX                      #-- load %v_t_18 at example.lat:5:5 --#
movq %RSI, %RAX                      #-- load %v_t_19 at example.lat:5:5 --#
movq 44 (%RDX), %RDI                 #-- load %v_c1 at example.lat:5:28 --#
decl 16 (%RDI)                       #-- incr ref count on VVal example.lat:5:28 (Ref example.lat:3:5 (Cl example.lat:3:5 (IRTargetRefName "Node"))) (IRValueName "%v_c1") at example.lat:5:28 --#
incl 16 (%RAX)                       #-- incr ref count on VVal example.lat:5:28 (Ref example.lat:5:28 (Cl example.lat:5:28 (IRTargetRefName "Node"))) (IRValueName "%v_t_19") at example.lat:5:28 --#
movq %RAX, 44 (%RDX)                 #-- example.lat:5:28 --#
decl 16 (%RAX)                       #-- incr ref count on VVal example.lat:5:28 (Ref example.lat:5:18 (Cl example.lat:5:18 (IRTargetRefName "Node"))) (IRValueName "%v_t_19") at example.lat:5:28 --#
decl 16 (%RDX)                       #-- incr ref count on VVal example.lat:5:28 (Ref  (Cl  (IRTargetRefName "Node"))) (IRValueName "%v_t_18") at example.lat:5:28 --#
movl %ECX, %EAX                      #-- move return value at example.lat:5:5 --#
leave                                #-- example.lat:5:5 --#
ret                                  #-- example.lat:5:5 --#
Node.setElem :                       #-- example.lat:4:5 --#
pushq %RBP                           #-- example.lat:4:5 --#
movq %RSP, %RBP                      #-- example.lat:4:5 --#
Node.setElem.L_entry :               #-- example.lat:4:5 --#
movq %RDI, %RDX                      #-- load %v_t_16 at example.lat:4:5 --#
movq %RSI, %RAX                      #-- load %v_t_17 at example.lat:4:5 --#
movq 36 (%RDX), %RDI                 #-- load %v_c0 at example.lat:4:29 --#
decl 16 (%RDI)                       #-- incr ref count on VVal example.lat:4:29 (Ref example.lat:2:5 (Cl example.lat:2:5 (IRTargetRefName "Shape"))) (IRValueName "%v_c0") at example.lat:4:29 --#
incl 16 (%RAX)                       #-- incr ref count on VVal example.lat:4:29 (Ref example.lat:4:29 (Cl example.lat:4:29 (IRTargetRefName "Shape"))) (IRValueName "%v_t_17") at example.lat:4:29 --#
movq %RAX, 36 (%RDX)                 #-- example.lat:4:29 --#
decl 16 (%RAX)                       #-- incr ref count on VVal example.lat:4:29 (Ref example.lat:4:18 (Cl example.lat:4:18 (IRTargetRefName "Shape"))) (IRValueName "%v_t_17") at example.lat:4:29 --#
decl 16 (%RDX)                       #-- incr ref count on VVal example.lat:4:29 (Ref  (Cl  (IRTargetRefName "Node"))) (IRValueName "%v_t_16") at example.lat:4:29 --#
movl %ECX, %EAX                      #-- move return value at example.lat:4:5 --#
leave                                #-- example.lat:4:5 --#
ret                                  #-- example.lat:4:5 --#
__handleErrorNull :                  #-- runtime error on null dereference at example.lat:1:1 --#
andq $-16, %RSP                      #-- 16 bytes allign at example.lat:1:1 --#
call __errorNull                     #-- example.lat:1:1 --#
