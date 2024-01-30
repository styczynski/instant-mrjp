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
.extern run_gc

.section .rodata
.global _class_Array
_class_Array :
.quad _class_Object
.quad _class_Array_initializer
.long 16
.quad _class_Array_methods
.long 0
.quad 0
.quad _class_Array_fields_info
.quad _class_Array_fields_info_offsets
.quad _class_Array_fields_info_pos
.long 3
.quad _class_Array_name

.global _class_Array_methods
_class_Array_methods :
.quad _Array_toString
.quad _Object_getHashCode
.quad _Object_equals

.global _class_Array_initializer
_class_Array_initializer :
.quad _LAT_NULL
.long 0
.long 0

.global _class_Array_fields_info
_class_Array_fields_info :
.string "IelementSize"
.string "Ilength"
.string "Celements"

.global _class_Array_fields_info_offsets
_class_Array_fields_info_offsets :
.long 0
.long 13
.long 21

.global _class_Array_fields_info_pos
_class_Array_fields_info_pos :
.long 12
.long 8
.long 0

.global _class_Array_name
_class_Array_name :
.string "Array"

.global _class_List
_class_List :
.quad _class_Object
.quad _class_List_initializer
.long 16
.quad _class_List_methods
.long 0
.quad 0
.quad _class_List_fields_info
.quad _class_List_fields_info_offsets
.quad _class_List_fields_info_pos
.long 2
.quad _class_List_name

.global _class_List_methods
_class_List_methods :
.quad _Object_toString
.quad _Object_getHashCode
.quad _Object_equals
.quad List.makeSingleton
.quad List.getHead
.quad List.getTail
.quad List.cons

.global _class_List_initializer
_class_List_initializer :
.quad _LAT_NULL
.quad _LAT_NULL

.global _class_List_fields_info
_class_List_fields_info :
.string "Ctail"
.string "Chead"

.global _class_List_fields_info_offsets
_class_List_fields_info_offsets :
.long 0
.long 6

.global _class_List_fields_info_pos
_class_List_fields_info_pos :
.long 8
.long 0

.global _class_List_name
_class_List_name :
.string "List"

.global _class_Node
_class_Node :
.quad _class_Object
.quad _class_Node_initializer
.long 16
.quad _class_Node_methods
.long 0
.quad 0
.quad _class_Node_fields_info
.quad _class_Node_fields_info_offsets
.quad _class_Node_fields_info_pos
.long 3
.quad _class_Node_name

.global _class_Node_methods
_class_Node_methods :
.quad _Object_toString
.quad _Object_getHashCode
.quad _Object_equals
.quad Node.init
.quad Node.isVisited
.quad Node.markAsVisited
.quad Node.getValue
.quad Node.getNeighbours
.quad Node.addNeighbour

.global _class_Node_initializer
_class_Node_initializer :
.byte 0
.long 0
.quad _LAT_NULL

.global _class_Node_fields_info
_class_Node_fields_info :
.string "Cneighbours"
.string "Ivalue"
.string "Bvisited"

.global _class_Node_fields_info_offsets
_class_Node_fields_info_offsets :
.long 0
.long 12
.long 19

.global _class_Node_fields_info_pos
_class_Node_fields_info_pos :
.long 5
.long 1
.long 0

.global _class_Node_name
_class_Node_name :
.string "Node"

.global _class_Object
_class_Object :
.quad 0
.quad _class_Object_initializer
.long 0
.quad _class_Object_methods
.long 0
.quad 0
.quad _class_Object_fields_info
.quad _class_Object_fields_info_offsets
.quad _class_Object_fields_info_pos
.long 0
.quad _class_Object_name

.global _class_Object_methods
_class_Object_methods :
.quad _Object_toString
.quad _Object_getHashCode
.quad _Object_equals

.global _class_Object_initializer
_class_Object_initializer :

.global _class_Object_fields_info
_class_Object_fields_info :

.global _class_Object_fields_info_offsets
_class_Object_fields_info_offsets :

.global _class_Object_fields_info_pos
_class_Object_fields_info_pos :

.global _class_Object_name
_class_Object_name :
.string "Object"

.global _class_Queue
_class_Queue :
.quad _class_Object
.quad _class_Queue_initializer
.long 16
.quad _class_Queue_methods
.long 0
.quad 0
.quad _class_Queue_fields_info
.quad _class_Queue_fields_info_offsets
.quad _class_Queue_fields_info_pos
.long 2
.quad _class_Queue_name

.global _class_Queue_methods
_class_Queue_methods :
.quad _Object_toString
.quad _Object_getHashCode
.quad _Object_equals
.quad Queue.get
.quad Queue.put
.quad Queue.isEmpty

.global _class_Queue_initializer
_class_Queue_initializer :
.quad _LAT_NULL
.quad _LAT_NULL

.global _class_Queue_fields_info
_class_Queue_fields_info :
.string "Clast"
.string "Cfirst"

.global _class_Queue_fields_info_offsets
_class_Queue_fields_info_offsets :
.long 0
.long 6

.global _class_Queue_fields_info_pos
_class_Queue_fields_info_pos :
.long 8
.long 0

.global _class_Queue_name
_class_Queue_name :
.string "Queue"

.global _class_String
_class_String :
.quad _class_Object
.quad _class_String_initializer
.long 0
.quad _class_String_methods
.long 0
.quad 0
.quad _class_String_fields_info
.quad _class_String_fields_info_offsets
.quad _class_String_fields_info_pos
.long 0
.quad _class_String_name

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

.global _class_String_fields_info
_class_String_fields_info :

.global _class_String_fields_info_offsets
_class_String_fields_info_offsets :

.global _class_String_fields_info_pos
_class_String_fields_info_pos :

.global _class_String_name
_class_String_name :
.string "String"

.global main

.section .data
.global _LAT_NULL
_LAT_NULL :
.quad _class_Object
.quad 0
.long 0
.quad _LAT_NULL_METHODS
.long 0
.long 0

_LAT_NULL_METHODS :
.quad __handleErrorNull
.quad __handleErrorNull
.quad __handleErrorNull
.quad __handleErrorNull
.quad __handleErrorNull
.quad __handleErrorNull
.quad __handleErrorNull
.quad __handleErrorNull
.quad __handleErrorNull
.quad __handleErrorNull
.quad __handleErrorNull

_LAT_NULL_ADDR :
.quad _LAT_NULL


.section .text
__cl_TopLevel.bfs :                       #-- example.lat:120:9 --#
pushq %RBX                                #-- example.lat:120:9 --#
pushq %R12                                #-- example.lat:120:9 --#
pushq %R13                                #-- example.lat:120:9 --#
pushq %R14                                #-- example.lat:120:9 --#
pushq %R15                                #-- example.lat:120:9 --#
subq $8, %RSP                             #-- example.lat:120:9 --#
pushq %RBP                                #-- example.lat:120:9 --#
movq %RSP, %RBP                           #-- example.lat:120:9 --#
__cl_TopLevel.bfs.L_entry :               #-- example.lat:120:9 --#
movq %RDI, %R14                           #-- load %v_t_151 at example.lat:120:9 --#
jmp __cl_TopLevel.bfs_WCOND152            #-- example.lat:121:13 --#
__cl_TopLevel.bfs_IELSE167 :              #-- example.lat:127:21 --#
jmp __cl_TopLevel.bfs_IEND168             #-- example.lat:127:21 --#
__cl_TopLevel.bfs_IEND168 :               #-- example.lat:127:21 --#
pushq %RAX                                #-- save caller saved at example.lat:131:29 --#
movq %R12, %RDI                           #-- passing arg at example.lat:131:29 --#
cmpq $0, 0 (%RDI)                         #-- example.lat:131:29 --#
jz __handleErrorNull                      #-- example.lat:131:29 --#
movq 20 (%RDI), %RAX                      #-- load address of vtable at example.lat:131:29 --#
call * 40 (%RAX)                          #-- call getTail at example.lat:131:29 --#
movq %RAX, %R15                           #-- example.lat:131:29 --#
pop %RAX                                  #-- example.lat:131:29 --#
incl 16 (%R15)                            #-- incr ref count on VVal example.lat:131:21 (Ref example.lat:131:21 (Cl example.lat:131:21 (IRTargetRefName "List"))) (IRValueName "%v_t_172") at example.lat:131:21 --#
pushq %RAX                                #-- save caller saved at example.lat:131:21 --#
call run_gc                               #-- example.lat:131:21 --#
pop %RAX                                  #-- example.lat:131:21 --#
decl 16 (%R15)                            #-- incr ref count on VVal example.lat:131:21 (Ref example.lat:44:13 (Cl example.lat:44:13 (IRTargetRefName "List"))) (IRValueName "%v_t_172") at example.lat:131:21 --#
xchgq %R12, %R15                          #-- example.lat:125:17 --#
jmp __cl_TopLevel.bfs_WCOND161            #-- example.lat:125:17 --#
__cl_TopLevel.bfs_IIF166 :                #-- example.lat:127:21 --#
pushq %RAX                                #-- save caller saved at example.lat:128:25 --#
movq %RBX, %RDI                           #-- passing arg at example.lat:128:25 --#
cmpq $0, 0 (%RDI)                         #-- example.lat:128:25 --#
jz __handleErrorNull                      #-- example.lat:128:25 --#
movq 20 (%RDI), %RAX                      #-- load address of vtable at example.lat:128:25 --#
call * 40 (%RAX)                          #-- call markAsVisited at example.lat:128:25 --#
pop %RAX                                  #-- example.lat:128:25 --#
pushq %RAX                                #-- save caller saved at example.lat:129:25 --#
movq %R14, %RDI                           #-- passing arg at example.lat:129:25 --#
movq %RBX, %RSI                           #-- passing arg at example.lat:129:25 --#
cmpq $0, 0 (%RDI)                         #-- example.lat:129:25 --#
jz __handleErrorNull                      #-- example.lat:129:25 --#
movq 20 (%RDI), %RAX                      #-- load address of vtable at example.lat:129:25 --#
call * 32 (%RAX)                          #-- call put at example.lat:129:25 --#
pop %RAX                                  #-- example.lat:129:25 --#
jmp __cl_TopLevel.bfs_IEND168             #-- example.lat:127:21 --#
__cl_TopLevel.bfs_WBEG153 :               #-- example.lat:121:13 --#
pushq %RAX                                #-- save caller saved at example.lat:122:27 --#
movq %R14, %RDI                           #-- passing arg at example.lat:122:27 --#
cmpq $0, 0 (%RDI)                         #-- example.lat:122:27 --#
jz __handleErrorNull                      #-- example.lat:122:27 --#
movq 20 (%RDI), %RAX                      #-- load address of vtable at example.lat:122:27 --#
call * 24 (%RAX)                          #-- call get at example.lat:122:27 --#
movq %RAX, %R12                           #-- example.lat:122:27 --#
pop %RAX                                  #-- example.lat:122:27 --#
pushq %RAX                                #-- save caller saved at example.lat:122:27 --#
call run_gc                               #-- example.lat:122:27 --#
pop %RAX                                  #-- example.lat:122:27 --#
decl 16 (%R14)                            #-- incr ref count on VVal example.lat:122:27 (Ref example.lat:122:27 (Cl example.lat:122:27 (IRTargetRefName "Queue"))) (IRValueName "%v_t_151") at example.lat:122:27 --#
incl 16 (%R12)                            #-- incr ref count on VVal example.lat:122:22 (Ref example.lat:122:22 (Cl example.lat:122:22 (IRTargetRefName "Node"))) (IRValueName "%v_t_155") at example.lat:122:22 --#
pushq %RAX                                #-- save caller saved at example.lat:122:22 --#
call run_gc                               #-- example.lat:122:22 --#
pop %RAX                                  #-- example.lat:122:22 --#
decl 16 (%R12)                            #-- incr ref count on VVal example.lat:122:22 (Ref example.lat:55:13 (Cl example.lat:55:13 (IRTargetRefName "Node"))) (IRValueName "%v_t_155") at example.lat:122:22 --#
pushq %RAX                                #-- save caller saved at example.lat:123:26 --#
movq %R12, %RDI                           #-- passing arg at example.lat:123:26 --#
cmpq $0, 0 (%RDI)                         #-- example.lat:123:26 --#
jz __handleErrorNull                      #-- example.lat:123:26 --#
movq 20 (%RDI), %RAX                      #-- load address of vtable at example.lat:123:26 --#
call * 48 (%RAX)                          #-- call getValue at example.lat:123:26 --#
movl %EAX, %EBX                           #-- example.lat:123:26 --#
pop %RAX                                  #-- example.lat:123:26 --#
pushq %RAX                                #-- save caller saved at example.lat:123:26 --#
call run_gc                               #-- example.lat:123:26 --#
pop %RAX                                  #-- example.lat:123:26 --#
decl 16 (%R12)                            #-- incr ref count on VVal example.lat:123:26 (Ref example.lat:123:26 (Cl example.lat:123:26 (IRTargetRefName "Node"))) (IRValueName "%v_t_155") at example.lat:123:26 --#
pushq %RAX                                #-- save caller saved at example.lat:123:17 --#
movl %EBX, %EDI                           #-- passing arg at example.lat:123:17 --#
call printInt                             #-- example.lat:123:17 --#
pop %RAX                                  #-- example.lat:123:17 --#
pushq %RAX                                #-- save caller saved at example.lat:124:30 --#
movq %R12, %RDI                           #-- passing arg at example.lat:124:30 --#
cmpq $0, 0 (%RDI)                         #-- example.lat:124:30 --#
jz __handleErrorNull                      #-- example.lat:124:30 --#
movq 20 (%RDI), %RAX                      #-- load address of vtable at example.lat:124:30 --#
call * 56 (%RAX)                          #-- call getNeighbours at example.lat:124:30 --#
movq %RAX, %R13                           #-- example.lat:124:30 --#
pop %RAX                                  #-- example.lat:124:30 --#
pushq %RAX                                #-- save caller saved at example.lat:124:30 --#
call run_gc                               #-- example.lat:124:30 --#
pop %RAX                                  #-- example.lat:124:30 --#
decl 16 (%R12)                            #-- incr ref count on VVal example.lat:124:30 (Ref example.lat:124:30 (Cl example.lat:124:30 (IRTargetRefName "Node"))) (IRValueName "%v_t_155") at example.lat:124:30 --#
incl 16 (%R13)                            #-- incr ref count on VVal example.lat:124:22 (Ref example.lat:124:22 (Cl example.lat:124:22 (IRTargetRefName "List"))) (IRValueName "%v_t_159") at example.lat:124:22 --#
pushq %RAX                                #-- save caller saved at example.lat:124:22 --#
call run_gc                               #-- example.lat:124:22 --#
pop %RAX                                  #-- example.lat:124:22 --#
decl 16 (%R13)                            #-- incr ref count on VVal example.lat:124:22 (Ref example.lat:19:13 (Cl example.lat:19:13 (IRTargetRefName "List"))) (IRValueName "%v_t_159") at example.lat:124:22 --#
xchgq %R12, %R13                          #-- example.lat:125:17 --#
jmp __cl_TopLevel.bfs_WCOND161            #-- example.lat:125:17 --#
__cl_TopLevel.bfs_WBEG162 :               #-- example.lat:125:17 --#
pushq %RAX                                #-- save caller saved at example.lat:126:30 --#
movq %R12, %RDI                           #-- passing arg at example.lat:126:30 --#
cmpq $0, 0 (%RDI)                         #-- example.lat:126:30 --#
jz __handleErrorNull                      #-- example.lat:126:30 --#
movq 20 (%RDI), %RAX                      #-- load address of vtable at example.lat:126:30 --#
call * 32 (%RAX)                          #-- call getHead at example.lat:126:30 --#
movq %RAX, %RBX                           #-- example.lat:126:30 --#
pop %RAX                                  #-- example.lat:126:30 --#
incl 16 (%RBX)                            #-- incr ref count on VVal example.lat:126:26 (Ref example.lat:126:26 (Cl example.lat:126:26 (IRTargetRefName "Node"))) (IRValueName "%v_t_164") at example.lat:126:26 --#
pushq %RAX                                #-- save caller saved at example.lat:126:26 --#
call run_gc                               #-- example.lat:126:26 --#
pop %RAX                                  #-- example.lat:126:26 --#
decl 16 (%RBX)                            #-- incr ref count on VVal example.lat:126:26 (Ref example.lat:41:13 (Cl example.lat:41:13 (IRTargetRefName "Node"))) (IRValueName "%v_t_164") at example.lat:126:26 --#
pushq %RAX                                #-- save caller saved at example.lat:127:26 --#
movq %RBX, %RDI                           #-- passing arg at example.lat:127:26 --#
cmpq $0, 0 (%RDI)                         #-- example.lat:127:26 --#
jz __handleErrorNull                      #-- example.lat:127:26 --#
movq 20 (%RDI), %RAX                      #-- load address of vtable at example.lat:127:26 --#
call * 32 (%RAX)                          #-- call isVisited at example.lat:127:26 --#
movb %AL, %CL                             #-- example.lat:127:26 --#
pop %RAX                                  #-- example.lat:127:26 --#
testb %CL, %CL                            #-- example.lat:127:26 --#
jne __cl_TopLevel.bfs_IELSE167            #-- example.lat:127:26 --#
jmp __cl_TopLevel.bfs_IIF166              #-- example.lat:127:26 --#
__cl_TopLevel.bfs_WCOND152 :              #-- example.lat:121:13 --#
pushq %RAX                                #-- save caller saved at example.lat:121:22 --#
movq %R14, %RDI                           #-- passing arg at example.lat:121:22 --#
cmpq $0, 0 (%RDI)                         #-- example.lat:121:22 --#
jz __handleErrorNull                      #-- example.lat:121:22 --#
movq 20 (%RDI), %RAX                      #-- load address of vtable at example.lat:121:22 --#
call * 40 (%RAX)                          #-- call isEmpty at example.lat:121:22 --#
movb %AL, %BL                             #-- example.lat:121:22 --#
pop %RAX                                  #-- example.lat:121:22 --#
pushq %RAX                                #-- save caller saved at example.lat:121:22 --#
call run_gc                               #-- example.lat:121:22 --#
pop %RAX                                  #-- example.lat:121:22 --#
decl 16 (%R14)                            #-- incr ref count on VVal example.lat:121:22 (Ref example.lat:121:22 (Cl example.lat:121:22 (IRTargetRefName "Queue"))) (IRValueName "%v_t_151") at example.lat:121:22 --#
testb %BL, %BL                            #-- example.lat:121:22 --#
jne __cl_TopLevel.bfs_WEND154             #-- example.lat:121:22 --#
jmp __cl_TopLevel.bfs_WBEG153             #-- example.lat:121:22 --#
__cl_TopLevel.bfs_WCOND161 :              #-- example.lat:125:17 --#
cmpq _LAT_NULL_ADDR (%RIP), %R12          #-- example.lat:125:23 --#
je __cl_TopLevel.bfs_WEND163              #-- example.lat:125:23 --#
jmp __cl_TopLevel.bfs_WBEG162             #-- example.lat:125:23 --#
__cl_TopLevel.bfs_WEND163 :               #-- example.lat:125:17 --#
jmp __cl_TopLevel.bfs_WCOND152            #-- example.lat:121:13 --#
__cl_TopLevel.bfs_WEND154 :               #-- example.lat:121:13 --#
leave                                     #-- example.lat:120:9 --#
addq $8, %RSP                             #-- example.lat:120:9 --#
pop %R15                                  #-- example.lat:120:9 --#
pop %R14                                  #-- example.lat:120:9 --#
pop %R13                                  #-- example.lat:120:9 --#
pop %R12                                  #-- example.lat:120:9 --#
pop %RBX                                  #-- example.lat:120:9 --#
ret                                       #-- example.lat:120:9 --#
__cl_TopLevel.prepareData :               #-- example.lat:88:9 --#
pushq %RBX                                #-- example.lat:88:9 --#
pushq %R12                                #-- example.lat:88:9 --#
pushq %R13                                #-- example.lat:88:9 --#
pushq %R14                                #-- example.lat:88:9 --#
pushq %R15                                #-- example.lat:88:9 --#
subq $8, %RSP                             #-- example.lat:88:9 --#
pushq %RBP                                #-- example.lat:88:9 --#
movq %RSP, %RBP                           #-- example.lat:88:9 --#
__cl_TopLevel.prepareData.L_entry :       #-- example.lat:88:9 --#
leaq _class_Node (%RIP), %RDI             #-- example.lat:89:23 --#
call __new                                #-- example.lat:89:23 --#
movq %RAX, %RDI                           #-- example.lat:89:23 --#
addl $2, 16 (%RDI)                        #-- incr ref count on VVal example.lat:89:23 (Ref example.lat:89:27 (Cl example.lat:89:27 (IRTargetRefName "Node"))) (IRValueName "%v_t_104") at example.lat:89:23 --#
pushq %RDI                                #-- save caller saved at example.lat:89:18 --#
call run_gc                               #-- example.lat:89:18 --#
pop %RDI                                  #-- example.lat:89:18 --#
decl 16 (%RDI)                            #-- incr ref count on VVal example.lat:89:18 (Ref example.lat:89:27 (Cl example.lat:89:27 (IRTargetRefName "Node"))) (IRValueName "%v_t_104") at example.lat:89:18 --#
pushq %RDI                                #-- save caller saved at example.lat:90:13 --#
movl $1, %ESI                             #-- passing arg at example.lat:90:13 --#
cmpq $0, 0 (%RDI)                         #-- example.lat:90:13 --#
jz __handleErrorNull                      #-- example.lat:90:13 --#
movq 20 (%RDI), %RAX                      #-- load address of vtable at example.lat:90:13 --#
call * 24 (%RAX)                          #-- call init at example.lat:90:13 --#
pop %RDI                                  #-- example.lat:90:13 --#
pushq %RDI                                #-- save caller saved at example.lat:90:13 --#
call run_gc                               #-- example.lat:90:13 --#
pop %RDI                                  #-- example.lat:90:13 --#
decl 16 (%RDI)                            #-- incr ref count on VVal example.lat:90:13 (Ref example.lat:90:13 (Cl example.lat:90:13 (IRTargetRefName "Node"))) (IRValueName "%v_t_104") at example.lat:90:13 --#
pushq %RDI                                #-- save caller saved at example.lat:91:23 --#
leaq _class_Node (%RIP), %RDI             #-- example.lat:91:23 --#
call __new                                #-- example.lat:91:23 --#
movq %RAX, %R13                           #-- example.lat:91:23 --#
pop %RDI                                  #-- example.lat:91:23 --#
addl $2, 16 (%R13)                        #-- incr ref count on VVal example.lat:91:23 (Ref example.lat:91:27 (Cl example.lat:91:27 (IRTargetRefName "Node"))) (IRValueName "%v_t_108") at example.lat:91:23 --#
pushq %RDI                                #-- save caller saved at example.lat:91:18 --#
call run_gc                               #-- example.lat:91:18 --#
pop %RDI                                  #-- example.lat:91:18 --#
decl 16 (%R13)                            #-- incr ref count on VVal example.lat:91:18 (Ref example.lat:91:27 (Cl example.lat:91:27 (IRTargetRefName "Node"))) (IRValueName "%v_t_108") at example.lat:91:18 --#
pushq %RDI                                #-- save caller saved at example.lat:92:13 --#
movq %R13, %RDI                           #-- passing arg at example.lat:92:13 --#
movl $2, %ESI                             #-- passing arg at example.lat:92:13 --#
cmpq $0, 0 (%RDI)                         #-- example.lat:92:13 --#
jz __handleErrorNull                      #-- example.lat:92:13 --#
movq 20 (%RDI), %RAX                      #-- load address of vtable at example.lat:92:13 --#
call * 24 (%RAX)                          #-- call init at example.lat:92:13 --#
pop %RDI                                  #-- example.lat:92:13 --#
pushq %RDI                                #-- save caller saved at example.lat:92:13 --#
call run_gc                               #-- example.lat:92:13 --#
pop %RDI                                  #-- example.lat:92:13 --#
decl 16 (%R13)                            #-- incr ref count on VVal example.lat:92:13 (Ref example.lat:92:13 (Cl example.lat:92:13 (IRTargetRefName "Node"))) (IRValueName "%v_t_108") at example.lat:92:13 --#
pushq %RDI                                #-- save caller saved at example.lat:93:23 --#
leaq _class_Node (%RIP), %RDI             #-- example.lat:93:23 --#
call __new                                #-- example.lat:93:23 --#
movq %RAX, %R14                           #-- example.lat:93:23 --#
pop %RDI                                  #-- example.lat:93:23 --#
addl $2, 16 (%R14)                        #-- incr ref count on VVal example.lat:93:23 (Ref example.lat:93:27 (Cl example.lat:93:27 (IRTargetRefName "Node"))) (IRValueName "%v_t_112") at example.lat:93:23 --#
pushq %RDI                                #-- save caller saved at example.lat:93:18 --#
call run_gc                               #-- example.lat:93:18 --#
pop %RDI                                  #-- example.lat:93:18 --#
decl 16 (%R14)                            #-- incr ref count on VVal example.lat:93:18 (Ref example.lat:93:27 (Cl example.lat:93:27 (IRTargetRefName "Node"))) (IRValueName "%v_t_112") at example.lat:93:18 --#
pushq %RDI                                #-- save caller saved at example.lat:94:13 --#
movq %R14, %RDI                           #-- passing arg at example.lat:94:13 --#
movl $3, %ESI                             #-- passing arg at example.lat:94:13 --#
cmpq $0, 0 (%RDI)                         #-- example.lat:94:13 --#
jz __handleErrorNull                      #-- example.lat:94:13 --#
movq 20 (%RDI), %RAX                      #-- load address of vtable at example.lat:94:13 --#
call * 24 (%RAX)                          #-- call init at example.lat:94:13 --#
pop %RDI                                  #-- example.lat:94:13 --#
pushq %RDI                                #-- save caller saved at example.lat:94:13 --#
call run_gc                               #-- example.lat:94:13 --#
pop %RDI                                  #-- example.lat:94:13 --#
decl 16 (%R14)                            #-- incr ref count on VVal example.lat:94:13 (Ref example.lat:94:13 (Cl example.lat:94:13 (IRTargetRefName "Node"))) (IRValueName "%v_t_112") at example.lat:94:13 --#
pushq %RDI                                #-- save caller saved at example.lat:95:23 --#
leaq _class_Node (%RIP), %RDI             #-- example.lat:95:23 --#
call __new                                #-- example.lat:95:23 --#
movq %RAX, %RBX                           #-- example.lat:95:23 --#
pop %RDI                                  #-- example.lat:95:23 --#
addl $2, 16 (%RBX)                        #-- incr ref count on VVal example.lat:95:23 (Ref example.lat:95:27 (Cl example.lat:95:27 (IRTargetRefName "Node"))) (IRValueName "%v_t_116") at example.lat:95:23 --#
pushq %RDI                                #-- save caller saved at example.lat:95:18 --#
call run_gc                               #-- example.lat:95:18 --#
pop %RDI                                  #-- example.lat:95:18 --#
decl 16 (%RBX)                            #-- incr ref count on VVal example.lat:95:18 (Ref example.lat:95:27 (Cl example.lat:95:27 (IRTargetRefName "Node"))) (IRValueName "%v_t_116") at example.lat:95:18 --#
pushq %RDI                                #-- save caller saved at example.lat:96:13 --#
movq %RBX, %RDI                           #-- passing arg at example.lat:96:13 --#
movl $4, %ESI                             #-- passing arg at example.lat:96:13 --#
cmpq $0, 0 (%RDI)                         #-- example.lat:96:13 --#
jz __handleErrorNull                      #-- example.lat:96:13 --#
movq 20 (%RDI), %RAX                      #-- load address of vtable at example.lat:96:13 --#
call * 24 (%RAX)                          #-- call init at example.lat:96:13 --#
pop %RDI                                  #-- example.lat:96:13 --#
pushq %RDI                                #-- save caller saved at example.lat:96:13 --#
call run_gc                               #-- example.lat:96:13 --#
pop %RDI                                  #-- example.lat:96:13 --#
decl 16 (%RBX)                            #-- incr ref count on VVal example.lat:96:13 (Ref example.lat:96:13 (Cl example.lat:96:13 (IRTargetRefName "Node"))) (IRValueName "%v_t_116") at example.lat:96:13 --#
pushq %RDI                                #-- save caller saved at example.lat:97:23 --#
leaq _class_Node (%RIP), %RDI             #-- example.lat:97:23 --#
call __new                                #-- example.lat:97:23 --#
movq %RAX, %R12                           #-- example.lat:97:23 --#
pop %RDI                                  #-- example.lat:97:23 --#
addl $2, 16 (%R12)                        #-- incr ref count on VVal example.lat:97:23 (Ref example.lat:97:27 (Cl example.lat:97:27 (IRTargetRefName "Node"))) (IRValueName "%v_t_120") at example.lat:97:23 --#
pushq %RDI                                #-- save caller saved at example.lat:97:18 --#
call run_gc                               #-- example.lat:97:18 --#
pop %RDI                                  #-- example.lat:97:18 --#
decl 16 (%R12)                            #-- incr ref count on VVal example.lat:97:18 (Ref example.lat:97:27 (Cl example.lat:97:27 (IRTargetRefName "Node"))) (IRValueName "%v_t_120") at example.lat:97:18 --#
pushq %RDI                                #-- save caller saved at example.lat:98:13 --#
movq %R12, %RDI                           #-- passing arg at example.lat:98:13 --#
movl $5, %ESI                             #-- passing arg at example.lat:98:13 --#
cmpq $0, 0 (%RDI)                         #-- example.lat:98:13 --#
jz __handleErrorNull                      #-- example.lat:98:13 --#
movq 20 (%RDI), %RAX                      #-- load address of vtable at example.lat:98:13 --#
call * 24 (%RAX)                          #-- call init at example.lat:98:13 --#
pop %RDI                                  #-- example.lat:98:13 --#
pushq %RDI                                #-- save caller saved at example.lat:98:13 --#
call run_gc                               #-- example.lat:98:13 --#
pop %RDI                                  #-- example.lat:98:13 --#
decl 16 (%R12)                            #-- incr ref count on VVal example.lat:98:13 (Ref example.lat:98:13 (Cl example.lat:98:13 (IRTargetRefName "Node"))) (IRValueName "%v_t_120") at example.lat:98:13 --#
pushq %RDI                                #-- save caller saved at example.lat:99:23 --#
leaq _class_Node (%RIP), %RDI             #-- example.lat:99:23 --#
call __new                                #-- example.lat:99:23 --#
movq %RAX, %RDX                           #-- example.lat:99:23 --#
pop %RDI                                  #-- example.lat:99:23 --#
addl $2, 16 (%RDX)                        #-- incr ref count on VVal example.lat:99:23 (Ref example.lat:99:27 (Cl example.lat:99:27 (IRTargetRefName "Node"))) (IRValueName "%v_t_124") at example.lat:99:23 --#
pushq %RDI                                #-- save caller saved at example.lat:99:18 --#
pushq %RDX                                #-- save caller saved at example.lat:99:18 --#
call run_gc                               #-- example.lat:99:18 --#
pop %RDX                                  #-- example.lat:99:18 --#
pop %RDI                                  #-- example.lat:99:18 --#
decl 16 (%RDX)                            #-- incr ref count on VVal example.lat:99:18 (Ref example.lat:99:27 (Cl example.lat:99:27 (IRTargetRefName "Node"))) (IRValueName "%v_t_124") at example.lat:99:18 --#
pushq %RDI                                #-- save caller saved at example.lat:100:13 --#
pushq %RDX                                #-- save caller saved at example.lat:100:13 --#
movq %RDX, %RDI                           #-- passing arg at example.lat:100:13 --#
movl $6, %ESI                             #-- passing arg at example.lat:100:13 --#
cmpq $0, 0 (%RDI)                         #-- example.lat:100:13 --#
jz __handleErrorNull                      #-- example.lat:100:13 --#
movq 20 (%RDI), %RAX                      #-- load address of vtable at example.lat:100:13 --#
call * 24 (%RAX)                          #-- call init at example.lat:100:13 --#
pop %RDX                                  #-- example.lat:100:13 --#
pop %RDI                                  #-- example.lat:100:13 --#
pushq %RDI                                #-- save caller saved at example.lat:100:13 --#
pushq %RDX                                #-- save caller saved at example.lat:100:13 --#
call run_gc                               #-- example.lat:100:13 --#
pop %RDX                                  #-- example.lat:100:13 --#
pop %RDI                                  #-- example.lat:100:13 --#
decl 16 (%RDX)                            #-- incr ref count on VVal example.lat:100:13 (Ref example.lat:100:13 (Cl example.lat:100:13 (IRTargetRefName "Node"))) (IRValueName "%v_t_124") at example.lat:100:13 --#
pushq %RDI                                #-- save caller saved at example.lat:101:23 --#
pushq %RDX                                #-- save caller saved at example.lat:101:23 --#
leaq _class_Node (%RIP), %RDI             #-- example.lat:101:23 --#
call __new                                #-- example.lat:101:23 --#
movq %RAX, %RCX                           #-- example.lat:101:23 --#
pop %RDX                                  #-- example.lat:101:23 --#
pop %RDI                                  #-- example.lat:101:23 --#
addl $2, 16 (%RCX)                        #-- incr ref count on VVal example.lat:101:23 (Ref example.lat:101:27 (Cl example.lat:101:27 (IRTargetRefName "Node"))) (IRValueName "%v_t_128") at example.lat:101:23 --#
pushq %RDI                                #-- save caller saved at example.lat:101:18 --#
pushq %RDX                                #-- save caller saved at example.lat:101:18 --#
pushq %RCX                                #-- save caller saved at example.lat:101:18 --#
call run_gc                               #-- example.lat:101:18 --#
pop %RCX                                  #-- example.lat:101:18 --#
pop %RDX                                  #-- example.lat:101:18 --#
pop %RDI                                  #-- example.lat:101:18 --#
decl 16 (%RCX)                            #-- incr ref count on VVal example.lat:101:18 (Ref example.lat:101:27 (Cl example.lat:101:27 (IRTargetRefName "Node"))) (IRValueName "%v_t_128") at example.lat:101:18 --#
pushq %RDI                                #-- save caller saved at example.lat:102:13 --#
pushq %RDX                                #-- save caller saved at example.lat:102:13 --#
pushq %RCX                                #-- save caller saved at example.lat:102:13 --#
movq %RCX, %RDI                           #-- passing arg at example.lat:102:13 --#
movl $7, %ESI                             #-- passing arg at example.lat:102:13 --#
cmpq $0, 0 (%RDI)                         #-- example.lat:102:13 --#
jz __handleErrorNull                      #-- example.lat:102:13 --#
movq 20 (%RDI), %RAX                      #-- load address of vtable at example.lat:102:13 --#
call * 24 (%RAX)                          #-- call init at example.lat:102:13 --#
pop %RCX                                  #-- example.lat:102:13 --#
pop %RDX                                  #-- example.lat:102:13 --#
pop %RDI                                  #-- example.lat:102:13 --#
pushq %RDI                                #-- save caller saved at example.lat:102:13 --#
pushq %RDX                                #-- save caller saved at example.lat:102:13 --#
pushq %RCX                                #-- save caller saved at example.lat:102:13 --#
call run_gc                               #-- example.lat:102:13 --#
pop %RCX                                  #-- example.lat:102:13 --#
pop %RDX                                  #-- example.lat:102:13 --#
pop %RDI                                  #-- example.lat:102:13 --#
decl 16 (%RCX)                            #-- incr ref count on VVal example.lat:102:13 (Ref example.lat:102:13 (Cl example.lat:102:13 (IRTargetRefName "Node"))) (IRValueName "%v_t_128") at example.lat:102:13 --#
pushq %RDI                                #-- save caller saved at example.lat:103:23 --#
pushq %RDX                                #-- save caller saved at example.lat:103:23 --#
pushq %RCX                                #-- save caller saved at example.lat:103:23 --#
leaq _class_Node (%RIP), %RDI             #-- example.lat:103:23 --#
call __new                                #-- example.lat:103:23 --#
pop %RCX                                  #-- example.lat:103:23 --#
pop %RDX                                  #-- example.lat:103:23 --#
pop %RDI                                  #-- example.lat:103:23 --#
addl $2, 16 (%RAX)                        #-- incr ref count on VVal example.lat:103:23 (Ref example.lat:103:27 (Cl example.lat:103:27 (IRTargetRefName "Node"))) (IRValueName "%v_t_132") at example.lat:103:23 --#
pushq %RDI                                #-- save caller saved at example.lat:103:18 --#
pushq %RDX                                #-- save caller saved at example.lat:103:18 --#
pushq %RCX                                #-- save caller saved at example.lat:103:18 --#
pushq %RAX                                #-- save caller saved at example.lat:103:18 --#
call run_gc                               #-- example.lat:103:18 --#
pop %RAX                                  #-- example.lat:103:18 --#
pop %RCX                                  #-- example.lat:103:18 --#
pop %RDX                                  #-- example.lat:103:18 --#
pop %RDI                                  #-- example.lat:103:18 --#
decl 16 (%RAX)                            #-- incr ref count on VVal example.lat:103:18 (Ref example.lat:103:27 (Cl example.lat:103:27 (IRTargetRefName "Node"))) (IRValueName "%v_t_132") at example.lat:103:18 --#
pushq %RDI                                #-- save caller saved at example.lat:104:13 --#
pushq %RDX                                #-- save caller saved at example.lat:104:13 --#
pushq %RCX                                #-- save caller saved at example.lat:104:13 --#
pushq %RAX                                #-- save caller saved at example.lat:104:13 --#
movq %RAX, %RDI                           #-- passing arg at example.lat:104:13 --#
movl $8, %ESI                             #-- passing arg at example.lat:104:13 --#
cmpq $0, 0 (%RDI)                         #-- example.lat:104:13 --#
jz __handleErrorNull                      #-- example.lat:104:13 --#
movq 20 (%RDI), %RAX                      #-- load address of vtable at example.lat:104:13 --#
call * 24 (%RAX)                          #-- call init at example.lat:104:13 --#
pop %RAX                                  #-- example.lat:104:13 --#
pop %RCX                                  #-- example.lat:104:13 --#
pop %RDX                                  #-- example.lat:104:13 --#
pop %RDI                                  #-- example.lat:104:13 --#
pushq %RDI                                #-- save caller saved at example.lat:104:13 --#
pushq %RDX                                #-- save caller saved at example.lat:104:13 --#
pushq %RCX                                #-- save caller saved at example.lat:104:13 --#
pushq %RAX                                #-- save caller saved at example.lat:104:13 --#
call run_gc                               #-- example.lat:104:13 --#
pop %RAX                                  #-- example.lat:104:13 --#
pop %RCX                                  #-- example.lat:104:13 --#
pop %RDX                                  #-- example.lat:104:13 --#
pop %RDI                                  #-- example.lat:104:13 --#
decl 16 (%RAX)                            #-- incr ref count on VVal example.lat:104:13 (Ref example.lat:104:13 (Cl example.lat:104:13 (IRTargetRefName "Node"))) (IRValueName "%v_t_132") at example.lat:104:13 --#
pushq %RDI                                #-- save caller saved at example.lat:105:23 --#
pushq %RDX                                #-- save caller saved at example.lat:105:23 --#
pushq %RCX                                #-- save caller saved at example.lat:105:23 --#
pushq %RAX                                #-- save caller saved at example.lat:105:23 --#
leaq _class_Node (%RIP), %RDI             #-- example.lat:105:23 --#
call __new                                #-- example.lat:105:23 --#
movq %RAX, %R15                           #-- example.lat:105:23 --#
pop %RAX                                  #-- example.lat:105:23 --#
pop %RCX                                  #-- example.lat:105:23 --#
pop %RDX                                  #-- example.lat:105:23 --#
pop %RDI                                  #-- example.lat:105:23 --#
addl $2, 16 (%R15)                        #-- incr ref count on VVal example.lat:105:23 (Ref example.lat:105:27 (Cl example.lat:105:27 (IRTargetRefName "Node"))) (IRValueName "%v_t_136") at example.lat:105:23 --#
pushq %RDI                                #-- save caller saved at example.lat:105:18 --#
pushq %RDX                                #-- save caller saved at example.lat:105:18 --#
pushq %RCX                                #-- save caller saved at example.lat:105:18 --#
pushq %RAX                                #-- save caller saved at example.lat:105:18 --#
call run_gc                               #-- example.lat:105:18 --#
pop %RAX                                  #-- example.lat:105:18 --#
pop %RCX                                  #-- example.lat:105:18 --#
pop %RDX                                  #-- example.lat:105:18 --#
pop %RDI                                  #-- example.lat:105:18 --#
decl 16 (%R15)                            #-- incr ref count on VVal example.lat:105:18 (Ref example.lat:105:27 (Cl example.lat:105:27 (IRTargetRefName "Node"))) (IRValueName "%v_t_136") at example.lat:105:18 --#
pushq %RDI                                #-- save caller saved at example.lat:106:13 --#
pushq %RDX                                #-- save caller saved at example.lat:106:13 --#
pushq %RCX                                #-- save caller saved at example.lat:106:13 --#
pushq %RAX                                #-- save caller saved at example.lat:106:13 --#
movq %R15, %RDI                           #-- passing arg at example.lat:106:13 --#
movl $9, %ESI                             #-- passing arg at example.lat:106:13 --#
cmpq $0, 0 (%RDI)                         #-- example.lat:106:13 --#
jz __handleErrorNull                      #-- example.lat:106:13 --#
movq 20 (%RDI), %RAX                      #-- load address of vtable at example.lat:106:13 --#
call * 24 (%RAX)                          #-- call init at example.lat:106:13 --#
pop %RAX                                  #-- example.lat:106:13 --#
pop %RCX                                  #-- example.lat:106:13 --#
pop %RDX                                  #-- example.lat:106:13 --#
pop %RDI                                  #-- example.lat:106:13 --#
pushq %RDI                                #-- save caller saved at example.lat:106:13 --#
pushq %RDX                                #-- save caller saved at example.lat:106:13 --#
pushq %RCX                                #-- save caller saved at example.lat:106:13 --#
pushq %RAX                                #-- save caller saved at example.lat:106:13 --#
call run_gc                               #-- example.lat:106:13 --#
pop %RAX                                  #-- example.lat:106:13 --#
pop %RCX                                  #-- example.lat:106:13 --#
pop %RDX                                  #-- example.lat:106:13 --#
pop %RDI                                  #-- example.lat:106:13 --#
decl 16 (%R15)                            #-- incr ref count on VVal example.lat:106:13 (Ref example.lat:106:13 (Cl example.lat:106:13 (IRTargetRefName "Node"))) (IRValueName "%v_t_136") at example.lat:106:13 --#
pushq %RDI                                #-- save caller saved at example.lat:107:13 --#
pushq %RDX                                #-- save caller saved at example.lat:107:13 --#
pushq %RCX                                #-- save caller saved at example.lat:107:13 --#
pushq %RAX                                #-- save caller saved at example.lat:107:13 --#
movq %R14, %RSI                           #-- passing arg at example.lat:107:13 --#
cmpq $0, 0 (%RDI)                         #-- example.lat:107:13 --#
jz __handleErrorNull                      #-- example.lat:107:13 --#
movq 20 (%RDI), %RAX                      #-- load address of vtable at example.lat:107:13 --#
call * 64 (%RAX)                          #-- call addNeighbour at example.lat:107:13 --#
pop %RAX                                  #-- example.lat:107:13 --#
pop %RCX                                  #-- example.lat:107:13 --#
pop %RDX                                  #-- example.lat:107:13 --#
pop %RDI                                  #-- example.lat:107:13 --#
pushq %RDI                                #-- save caller saved at example.lat:107:13 --#
pushq %RDX                                #-- save caller saved at example.lat:107:13 --#
pushq %RCX                                #-- save caller saved at example.lat:107:13 --#
pushq %RAX                                #-- save caller saved at example.lat:107:13 --#
call run_gc                               #-- example.lat:107:13 --#
pop %RAX                                  #-- example.lat:107:13 --#
pop %RCX                                  #-- example.lat:107:13 --#
pop %RDX                                  #-- example.lat:107:13 --#
pop %RDI                                  #-- example.lat:107:13 --#
decl 16 (%RDI)                            #-- incr ref count on VVal example.lat:107:13 (Ref example.lat:107:13 (Cl example.lat:107:13 (IRTargetRefName "Node"))) (IRValueName "%v_t_104") at example.lat:107:13 --#
pushq %RDI                                #-- save caller saved at example.lat:107:13 --#
pushq %RDX                                #-- save caller saved at example.lat:107:13 --#
pushq %RCX                                #-- save caller saved at example.lat:107:13 --#
pushq %RAX                                #-- save caller saved at example.lat:107:13 --#
call run_gc                               #-- example.lat:107:13 --#
pop %RAX                                  #-- example.lat:107:13 --#
pop %RCX                                  #-- example.lat:107:13 --#
pop %RDX                                  #-- example.lat:107:13 --#
pop %RDI                                  #-- example.lat:107:13 --#
decl 16 (%R14)                            #-- incr ref count on VVal example.lat:107:13 (Ref example.lat:107:13 (Cl example.lat:107:13 (IRTargetRefName "Node"))) (IRValueName "%v_t_112") at example.lat:107:13 --#
pushq %RDI                                #-- save caller saved at example.lat:108:13 --#
pushq %RDX                                #-- save caller saved at example.lat:108:13 --#
pushq %RCX                                #-- save caller saved at example.lat:108:13 --#
pushq %RAX                                #-- save caller saved at example.lat:108:13 --#
movq %R13, %RSI                           #-- passing arg at example.lat:108:13 --#
cmpq $0, 0 (%RDI)                         #-- example.lat:108:13 --#
jz __handleErrorNull                      #-- example.lat:108:13 --#
movq 20 (%RDI), %RAX                      #-- load address of vtable at example.lat:108:13 --#
call * 64 (%RAX)                          #-- call addNeighbour at example.lat:108:13 --#
pop %RAX                                  #-- example.lat:108:13 --#
pop %RCX                                  #-- example.lat:108:13 --#
pop %RDX                                  #-- example.lat:108:13 --#
pop %RDI                                  #-- example.lat:108:13 --#
pushq %RDI                                #-- save caller saved at example.lat:108:13 --#
pushq %RDX                                #-- save caller saved at example.lat:108:13 --#
pushq %RCX                                #-- save caller saved at example.lat:108:13 --#
pushq %RAX                                #-- save caller saved at example.lat:108:13 --#
call run_gc                               #-- example.lat:108:13 --#
pop %RAX                                  #-- example.lat:108:13 --#
pop %RCX                                  #-- example.lat:108:13 --#
pop %RDX                                  #-- example.lat:108:13 --#
pop %RDI                                  #-- example.lat:108:13 --#
decl 16 (%RDI)                            #-- incr ref count on VVal example.lat:108:13 (Ref example.lat:108:13 (Cl example.lat:108:13 (IRTargetRefName "Node"))) (IRValueName "%v_t_104") at example.lat:108:13 --#
pushq %RDI                                #-- save caller saved at example.lat:108:13 --#
pushq %RDX                                #-- save caller saved at example.lat:108:13 --#
pushq %RCX                                #-- save caller saved at example.lat:108:13 --#
pushq %RAX                                #-- save caller saved at example.lat:108:13 --#
call run_gc                               #-- example.lat:108:13 --#
pop %RAX                                  #-- example.lat:108:13 --#
pop %RCX                                  #-- example.lat:108:13 --#
pop %RDX                                  #-- example.lat:108:13 --#
pop %RDI                                  #-- example.lat:108:13 --#
decl 16 (%R13)                            #-- incr ref count on VVal example.lat:108:13 (Ref example.lat:108:13 (Cl example.lat:108:13 (IRTargetRefName "Node"))) (IRValueName "%v_t_108") at example.lat:108:13 --#
pushq %RDI                                #-- save caller saved at example.lat:109:13 --#
pushq %RDX                                #-- save caller saved at example.lat:109:13 --#
pushq %RCX                                #-- save caller saved at example.lat:109:13 --#
pushq %RAX                                #-- save caller saved at example.lat:109:13 --#
movq %R13, %RDI                           #-- passing arg at example.lat:109:13 --#
movq %R14, %RSI                           #-- passing arg at example.lat:109:13 --#
cmpq $0, 0 (%RDI)                         #-- example.lat:109:13 --#
jz __handleErrorNull                      #-- example.lat:109:13 --#
movq 20 (%RDI), %RAX                      #-- load address of vtable at example.lat:109:13 --#
call * 64 (%RAX)                          #-- call addNeighbour at example.lat:109:13 --#
pop %RAX                                  #-- example.lat:109:13 --#
pop %RCX                                  #-- example.lat:109:13 --#
pop %RDX                                  #-- example.lat:109:13 --#
pop %RDI                                  #-- example.lat:109:13 --#
pushq %RDI                                #-- save caller saved at example.lat:109:13 --#
pushq %RDX                                #-- save caller saved at example.lat:109:13 --#
pushq %RCX                                #-- save caller saved at example.lat:109:13 --#
pushq %RAX                                #-- save caller saved at example.lat:109:13 --#
call run_gc                               #-- example.lat:109:13 --#
pop %RAX                                  #-- example.lat:109:13 --#
pop %RCX                                  #-- example.lat:109:13 --#
pop %RDX                                  #-- example.lat:109:13 --#
pop %RDI                                  #-- example.lat:109:13 --#
decl 16 (%R13)                            #-- incr ref count on VVal example.lat:109:13 (Ref example.lat:109:13 (Cl example.lat:109:13 (IRTargetRefName "Node"))) (IRValueName "%v_t_108") at example.lat:109:13 --#
pushq %RDI                                #-- save caller saved at example.lat:109:13 --#
pushq %RDX                                #-- save caller saved at example.lat:109:13 --#
pushq %RCX                                #-- save caller saved at example.lat:109:13 --#
pushq %RAX                                #-- save caller saved at example.lat:109:13 --#
call run_gc                               #-- example.lat:109:13 --#
pop %RAX                                  #-- example.lat:109:13 --#
pop %RCX                                  #-- example.lat:109:13 --#
pop %RDX                                  #-- example.lat:109:13 --#
pop %RDI                                  #-- example.lat:109:13 --#
decl 16 (%R14)                            #-- incr ref count on VVal example.lat:109:13 (Ref example.lat:109:13 (Cl example.lat:109:13 (IRTargetRefName "Node"))) (IRValueName "%v_t_112") at example.lat:109:13 --#
pushq %RDI                                #-- save caller saved at example.lat:110:13 --#
pushq %RDX                                #-- save caller saved at example.lat:110:13 --#
pushq %RCX                                #-- save caller saved at example.lat:110:13 --#
pushq %RAX                                #-- save caller saved at example.lat:110:13 --#
movq %R14, %RDI                           #-- passing arg at example.lat:110:13 --#
movq %RDX, %RSI                           #-- passing arg at example.lat:110:13 --#
cmpq $0, 0 (%RDI)                         #-- example.lat:110:13 --#
jz __handleErrorNull                      #-- example.lat:110:13 --#
movq 20 (%RDI), %RAX                      #-- load address of vtable at example.lat:110:13 --#
call * 64 (%RAX)                          #-- call addNeighbour at example.lat:110:13 --#
pop %RAX                                  #-- example.lat:110:13 --#
pop %RCX                                  #-- example.lat:110:13 --#
pop %RDX                                  #-- example.lat:110:13 --#
pop %RDI                                  #-- example.lat:110:13 --#
pushq %RDI                                #-- save caller saved at example.lat:110:13 --#
pushq %RDX                                #-- save caller saved at example.lat:110:13 --#
pushq %RCX                                #-- save caller saved at example.lat:110:13 --#
pushq %RAX                                #-- save caller saved at example.lat:110:13 --#
call run_gc                               #-- example.lat:110:13 --#
pop %RAX                                  #-- example.lat:110:13 --#
pop %RCX                                  #-- example.lat:110:13 --#
pop %RDX                                  #-- example.lat:110:13 --#
pop %RDI                                  #-- example.lat:110:13 --#
decl 16 (%R14)                            #-- incr ref count on VVal example.lat:110:13 (Ref example.lat:110:13 (Cl example.lat:110:13 (IRTargetRefName "Node"))) (IRValueName "%v_t_112") at example.lat:110:13 --#
pushq %RDI                                #-- save caller saved at example.lat:110:13 --#
pushq %RDX                                #-- save caller saved at example.lat:110:13 --#
pushq %RCX                                #-- save caller saved at example.lat:110:13 --#
pushq %RAX                                #-- save caller saved at example.lat:110:13 --#
call run_gc                               #-- example.lat:110:13 --#
pop %RAX                                  #-- example.lat:110:13 --#
pop %RCX                                  #-- example.lat:110:13 --#
pop %RDX                                  #-- example.lat:110:13 --#
pop %RDI                                  #-- example.lat:110:13 --#
decl 16 (%RDX)                            #-- incr ref count on VVal example.lat:110:13 (Ref example.lat:110:13 (Cl example.lat:110:13 (IRTargetRefName "Node"))) (IRValueName "%v_t_124") at example.lat:110:13 --#
pushq %RDI                                #-- save caller saved at example.lat:111:13 --#
pushq %RCX                                #-- save caller saved at example.lat:111:13 --#
pushq %RAX                                #-- save caller saved at example.lat:111:13 --#
movq %R14, %RDI                           #-- passing arg at example.lat:111:13 --#
movq %R12, %RSI                           #-- passing arg at example.lat:111:13 --#
cmpq $0, 0 (%RDI)                         #-- example.lat:111:13 --#
jz __handleErrorNull                      #-- example.lat:111:13 --#
movq 20 (%RDI), %RAX                      #-- load address of vtable at example.lat:111:13 --#
call * 64 (%RAX)                          #-- call addNeighbour at example.lat:111:13 --#
pop %RAX                                  #-- example.lat:111:13 --#
pop %RCX                                  #-- example.lat:111:13 --#
pop %RDI                                  #-- example.lat:111:13 --#
pushq %RDI                                #-- save caller saved at example.lat:111:13 --#
pushq %RCX                                #-- save caller saved at example.lat:111:13 --#
pushq %RAX                                #-- save caller saved at example.lat:111:13 --#
call run_gc                               #-- example.lat:111:13 --#
pop %RAX                                  #-- example.lat:111:13 --#
pop %RCX                                  #-- example.lat:111:13 --#
pop %RDI                                  #-- example.lat:111:13 --#
decl 16 (%R12)                            #-- incr ref count on VVal example.lat:111:13 (Ref example.lat:111:13 (Cl example.lat:111:13 (IRTargetRefName "Node"))) (IRValueName "%v_t_120") at example.lat:111:13 --#
pushq %RDI                                #-- save caller saved at example.lat:111:13 --#
pushq %RCX                                #-- save caller saved at example.lat:111:13 --#
pushq %RAX                                #-- save caller saved at example.lat:111:13 --#
call run_gc                               #-- example.lat:111:13 --#
pop %RAX                                  #-- example.lat:111:13 --#
pop %RCX                                  #-- example.lat:111:13 --#
pop %RDI                                  #-- example.lat:111:13 --#
decl 16 (%R14)                            #-- incr ref count on VVal example.lat:111:13 (Ref example.lat:111:13 (Cl example.lat:111:13 (IRTargetRefName "Node"))) (IRValueName "%v_t_112") at example.lat:111:13 --#
pushq %RDI                                #-- save caller saved at example.lat:112:13 --#
pushq %RCX                                #-- save caller saved at example.lat:112:13 --#
pushq %RAX                                #-- save caller saved at example.lat:112:13 --#
movq %R14, %RDI                           #-- passing arg at example.lat:112:13 --#
movq %RBX, %RSI                           #-- passing arg at example.lat:112:13 --#
cmpq $0, 0 (%RDI)                         #-- example.lat:112:13 --#
jz __handleErrorNull                      #-- example.lat:112:13 --#
movq 20 (%RDI), %RAX                      #-- load address of vtable at example.lat:112:13 --#
call * 64 (%RAX)                          #-- call addNeighbour at example.lat:112:13 --#
pop %RAX                                  #-- example.lat:112:13 --#
pop %RCX                                  #-- example.lat:112:13 --#
pop %RDI                                  #-- example.lat:112:13 --#
pushq %RDI                                #-- save caller saved at example.lat:112:13 --#
pushq %RCX                                #-- save caller saved at example.lat:112:13 --#
pushq %RAX                                #-- save caller saved at example.lat:112:13 --#
call run_gc                               #-- example.lat:112:13 --#
pop %RAX                                  #-- example.lat:112:13 --#
pop %RCX                                  #-- example.lat:112:13 --#
pop %RDI                                  #-- example.lat:112:13 --#
decl 16 (%RBX)                            #-- incr ref count on VVal example.lat:112:13 (Ref example.lat:112:13 (Cl example.lat:112:13 (IRTargetRefName "Node"))) (IRValueName "%v_t_116") at example.lat:112:13 --#
pushq %RDI                                #-- save caller saved at example.lat:112:13 --#
pushq %RCX                                #-- save caller saved at example.lat:112:13 --#
pushq %RAX                                #-- save caller saved at example.lat:112:13 --#
call run_gc                               #-- example.lat:112:13 --#
pop %RAX                                  #-- example.lat:112:13 --#
pop %RCX                                  #-- example.lat:112:13 --#
pop %RDI                                  #-- example.lat:112:13 --#
decl 16 (%R14)                            #-- incr ref count on VVal example.lat:112:13 (Ref example.lat:112:13 (Cl example.lat:112:13 (IRTargetRefName "Node"))) (IRValueName "%v_t_112") at example.lat:112:13 --#
pushq %RDI                                #-- save caller saved at example.lat:113:13 --#
pushq %RCX                                #-- save caller saved at example.lat:113:13 --#
pushq %RAX                                #-- save caller saved at example.lat:113:13 --#
movq %RBX, %RDI                           #-- passing arg at example.lat:113:13 --#
movq %R13, %RSI                           #-- passing arg at example.lat:113:13 --#
cmpq $0, 0 (%RDI)                         #-- example.lat:113:13 --#
jz __handleErrorNull                      #-- example.lat:113:13 --#
movq 20 (%RDI), %RAX                      #-- load address of vtable at example.lat:113:13 --#
call * 64 (%RAX)                          #-- call addNeighbour at example.lat:113:13 --#
pop %RAX                                  #-- example.lat:113:13 --#
pop %RCX                                  #-- example.lat:113:13 --#
pop %RDI                                  #-- example.lat:113:13 --#
pushq %RDI                                #-- save caller saved at example.lat:113:13 --#
pushq %RCX                                #-- save caller saved at example.lat:113:13 --#
pushq %RAX                                #-- save caller saved at example.lat:113:13 --#
call run_gc                               #-- example.lat:113:13 --#
pop %RAX                                  #-- example.lat:113:13 --#
pop %RCX                                  #-- example.lat:113:13 --#
pop %RDI                                  #-- example.lat:113:13 --#
decl 16 (%R13)                            #-- incr ref count on VVal example.lat:113:13 (Ref example.lat:113:13 (Cl example.lat:113:13 (IRTargetRefName "Node"))) (IRValueName "%v_t_108") at example.lat:113:13 --#
pushq %RDI                                #-- save caller saved at example.lat:113:13 --#
pushq %RCX                                #-- save caller saved at example.lat:113:13 --#
pushq %RAX                                #-- save caller saved at example.lat:113:13 --#
call run_gc                               #-- example.lat:113:13 --#
pop %RAX                                  #-- example.lat:113:13 --#
pop %RCX                                  #-- example.lat:113:13 --#
pop %RDI                                  #-- example.lat:113:13 --#
decl 16 (%RBX)                            #-- incr ref count on VVal example.lat:113:13 (Ref example.lat:113:13 (Cl example.lat:113:13 (IRTargetRefName "Node"))) (IRValueName "%v_t_116") at example.lat:113:13 --#
pushq %RDI                                #-- save caller saved at example.lat:114:13 --#
pushq %RCX                                #-- save caller saved at example.lat:114:13 --#
pushq %RAX                                #-- save caller saved at example.lat:114:13 --#
movq %R12, %RDI                           #-- passing arg at example.lat:114:13 --#
movq %RCX, %RSI                           #-- passing arg at example.lat:114:13 --#
cmpq $0, 0 (%RDI)                         #-- example.lat:114:13 --#
jz __handleErrorNull                      #-- example.lat:114:13 --#
movq 20 (%RDI), %RAX                      #-- load address of vtable at example.lat:114:13 --#
call * 64 (%RAX)                          #-- call addNeighbour at example.lat:114:13 --#
pop %RAX                                  #-- example.lat:114:13 --#
pop %RCX                                  #-- example.lat:114:13 --#
pop %RDI                                  #-- example.lat:114:13 --#
pushq %RDI                                #-- save caller saved at example.lat:114:13 --#
pushq %RCX                                #-- save caller saved at example.lat:114:13 --#
pushq %RAX                                #-- save caller saved at example.lat:114:13 --#
call run_gc                               #-- example.lat:114:13 --#
pop %RAX                                  #-- example.lat:114:13 --#
pop %RCX                                  #-- example.lat:114:13 --#
pop %RDI                                  #-- example.lat:114:13 --#
decl 16 (%R12)                            #-- incr ref count on VVal example.lat:114:13 (Ref example.lat:114:13 (Cl example.lat:114:13 (IRTargetRefName "Node"))) (IRValueName "%v_t_120") at example.lat:114:13 --#
pushq %RDI                                #-- save caller saved at example.lat:114:13 --#
pushq %RCX                                #-- save caller saved at example.lat:114:13 --#
pushq %RAX                                #-- save caller saved at example.lat:114:13 --#
call run_gc                               #-- example.lat:114:13 --#
pop %RAX                                  #-- example.lat:114:13 --#
pop %RCX                                  #-- example.lat:114:13 --#
pop %RDI                                  #-- example.lat:114:13 --#
decl 16 (%RCX)                            #-- incr ref count on VVal example.lat:114:13 (Ref example.lat:114:13 (Cl example.lat:114:13 (IRTargetRefName "Node"))) (IRValueName "%v_t_128") at example.lat:114:13 --#
pushq %RDI                                #-- save caller saved at example.lat:115:13 --#
pushq %RCX                                #-- save caller saved at example.lat:115:13 --#
pushq %RAX                                #-- save caller saved at example.lat:115:13 --#
movq %RCX, %RDI                           #-- passing arg at example.lat:115:13 --#
movq %RAX, %RSI                           #-- passing arg at example.lat:115:13 --#
cmpq $0, 0 (%RDI)                         #-- example.lat:115:13 --#
jz __handleErrorNull                      #-- example.lat:115:13 --#
movq 20 (%RDI), %RAX                      #-- load address of vtable at example.lat:115:13 --#
call * 64 (%RAX)                          #-- call addNeighbour at example.lat:115:13 --#
pop %RAX                                  #-- example.lat:115:13 --#
pop %RCX                                  #-- example.lat:115:13 --#
pop %RDI                                  #-- example.lat:115:13 --#
pushq %RDI                                #-- save caller saved at example.lat:115:13 --#
pushq %RCX                                #-- save caller saved at example.lat:115:13 --#
pushq %RAX                                #-- save caller saved at example.lat:115:13 --#
call run_gc                               #-- example.lat:115:13 --#
pop %RAX                                  #-- example.lat:115:13 --#
pop %RCX                                  #-- example.lat:115:13 --#
pop %RDI                                  #-- example.lat:115:13 --#
decl 16 (%RAX)                            #-- incr ref count on VVal example.lat:115:13 (Ref example.lat:115:13 (Cl example.lat:115:13 (IRTargetRefName "Node"))) (IRValueName "%v_t_132") at example.lat:115:13 --#
pushq %RDI                                #-- save caller saved at example.lat:115:13 --#
pushq %RCX                                #-- save caller saved at example.lat:115:13 --#
pushq %RAX                                #-- save caller saved at example.lat:115:13 --#
call run_gc                               #-- example.lat:115:13 --#
pop %RAX                                  #-- example.lat:115:13 --#
pop %RCX                                  #-- example.lat:115:13 --#
pop %RDI                                  #-- example.lat:115:13 --#
decl 16 (%RCX)                            #-- incr ref count on VVal example.lat:115:13 (Ref example.lat:115:13 (Cl example.lat:115:13 (IRTargetRefName "Node"))) (IRValueName "%v_t_128") at example.lat:115:13 --#
pushq %RDI                                #-- save caller saved at example.lat:116:13 --#
pushq %RAX                                #-- save caller saved at example.lat:116:13 --#
movq %RAX, %RDI                           #-- passing arg at example.lat:116:13 --#
movq %R15, %RSI                           #-- passing arg at example.lat:116:13 --#
cmpq $0, 0 (%RDI)                         #-- example.lat:116:13 --#
jz __handleErrorNull                      #-- example.lat:116:13 --#
movq 20 (%RDI), %RAX                      #-- load address of vtable at example.lat:116:13 --#
call * 64 (%RAX)                          #-- call addNeighbour at example.lat:116:13 --#
pop %RAX                                  #-- example.lat:116:13 --#
pop %RDI                                  #-- example.lat:116:13 --#
pushq %RDI                                #-- save caller saved at example.lat:116:13 --#
pushq %RAX                                #-- save caller saved at example.lat:116:13 --#
call run_gc                               #-- example.lat:116:13 --#
pop %RAX                                  #-- example.lat:116:13 --#
pop %RDI                                  #-- example.lat:116:13 --#
decl 16 (%R15)                            #-- incr ref count on VVal example.lat:116:13 (Ref example.lat:116:13 (Cl example.lat:116:13 (IRTargetRefName "Node"))) (IRValueName "%v_t_136") at example.lat:116:13 --#
pushq %RDI                                #-- save caller saved at example.lat:116:13 --#
pushq %RAX                                #-- save caller saved at example.lat:116:13 --#
call run_gc                               #-- example.lat:116:13 --#
pop %RAX                                  #-- example.lat:116:13 --#
pop %RDI                                  #-- example.lat:116:13 --#
decl 16 (%RAX)                            #-- incr ref count on VVal example.lat:116:13 (Ref example.lat:116:13 (Cl example.lat:116:13 (IRTargetRefName "Node"))) (IRValueName "%v_t_132") at example.lat:116:13 --#
pushq %RDI                                #-- save caller saved at example.lat:117:13 --#
movq %R15, %RDI                           #-- passing arg at example.lat:117:13 --#
movq %R12, %RSI                           #-- passing arg at example.lat:117:13 --#
cmpq $0, 0 (%RDI)                         #-- example.lat:117:13 --#
jz __handleErrorNull                      #-- example.lat:117:13 --#
movq 20 (%RDI), %RAX                      #-- load address of vtable at example.lat:117:13 --#
call * 64 (%RAX)                          #-- call addNeighbour at example.lat:117:13 --#
pop %RDI                                  #-- example.lat:117:13 --#
pushq %RDI                                #-- save caller saved at example.lat:117:13 --#
call run_gc                               #-- example.lat:117:13 --#
pop %RDI                                  #-- example.lat:117:13 --#
decl 16 (%R12)                            #-- incr ref count on VVal example.lat:117:13 (Ref example.lat:117:13 (Cl example.lat:117:13 (IRTargetRefName "Node"))) (IRValueName "%v_t_120") at example.lat:117:13 --#
pushq %RDI                                #-- save caller saved at example.lat:117:13 --#
call run_gc                               #-- example.lat:117:13 --#
pop %RDI                                  #-- example.lat:117:13 --#
decl 16 (%R15)                            #-- incr ref count on VVal example.lat:117:13 (Ref example.lat:117:13 (Cl example.lat:117:13 (IRTargetRefName "Node"))) (IRValueName "%v_t_136") at example.lat:117:13 --#
incl 16 (%RDI)                            #-- incr ref count on VVal example.lat:118:13 (Ref example.lat:118:13 (Cl example.lat:118:13 (IRTargetRefName "Node"))) (IRValueName "%v_t_104") at example.lat:118:13 --#
pushq %RDI                                #-- save caller saved at example.lat:118:13 --#
call run_gc                               #-- example.lat:118:13 --#
pop %RDI                                  #-- example.lat:118:13 --#
decl 16 (%RDI)                            #-- incr ref count on VVal example.lat:118:13 (Ref example.lat:118:13 (Cl example.lat:118:13 (IRTargetRefName "Node"))) (IRValueName "%v_t_104") at example.lat:118:13 --#
movq %RDI, %RAX                           #-- move return value at example.lat:88:9 --#
leave                                     #-- example.lat:88:9 --#
addq $8, %RSP                             #-- example.lat:88:9 --#
pop %R15                                  #-- example.lat:88:9 --#
pop %R14                                  #-- example.lat:88:9 --#
pop %R13                                  #-- example.lat:88:9 --#
pop %R12                                  #-- example.lat:88:9 --#
pop %RBX                                  #-- example.lat:88:9 --#
ret                                       #-- example.lat:88:9 --#
main :                                    #-- example.lat:80:9 --#
__cl_TopLevel.main :                      #-- example.lat:80:9 --#
pushq %RBX                                #-- example.lat:80:9 --#
pushq %R12                                #-- example.lat:80:9 --#
pushq %RBP                                #-- example.lat:80:9 --#
movq %RSP, %RBP                           #-- example.lat:80:9 --#
__cl_TopLevel.main.L_entry :              #-- example.lat:80:9 --#
call __cl_TopLevel.prepareData            #-- example.lat:81:26 --#
movq %RAX, %R12                           #-- example.lat:81:26 --#
incl 16 (%R12)                            #-- incr ref count on VVal example.lat:81:18 (Ref example.lat:81:18 (Cl example.lat:81:18 (IRTargetRefName "Node"))) (IRValueName "%v_t_96") at example.lat:81:18 --#
call run_gc                               #-- example.lat:81:18 --#
decl 16 (%R12)                            #-- incr ref count on VVal example.lat:81:18 (Ref example.lat:88:9 (Cl example.lat:88:9 (IRTargetRefName "Node"))) (IRValueName "%v_t_96") at example.lat:81:18 --#
movq %R12, %RDI                           #-- passing arg at example.lat:82:13 --#
cmpq $0, 0 (%RDI)                         #-- example.lat:82:13 --#
jz __handleErrorNull                      #-- example.lat:82:13 --#
movq 20 (%RDI), %RAX                      #-- load address of vtable at example.lat:82:13 --#
call * 40 (%RAX)                          #-- call markAsVisited at example.lat:82:13 --#
call run_gc                               #-- example.lat:82:13 --#
decl 16 (%R12)                            #-- incr ref count on VVal example.lat:82:13 (Ref example.lat:82:13 (Cl example.lat:82:13 (IRTargetRefName "Node"))) (IRValueName "%v_t_96") at example.lat:82:13 --#
leaq _class_Queue (%RIP), %RDI            #-- example.lat:83:23 --#
call __new                                #-- example.lat:83:23 --#
movq %RAX, %RBX                           #-- example.lat:83:23 --#
addl $2, 16 (%RBX)                        #-- incr ref count on VVal example.lat:83:23 (Ref example.lat:83:27 (Cl example.lat:83:27 (IRTargetRefName "Queue"))) (IRValueName "%v_t_99") at example.lat:83:23 --#
call run_gc                               #-- example.lat:83:19 --#
decl 16 (%RBX)                            #-- incr ref count on VVal example.lat:83:19 (Ref example.lat:83:27 (Cl example.lat:83:27 (IRTargetRefName "Queue"))) (IRValueName "%v_t_99") at example.lat:83:19 --#
movq %RBX, %RDI                           #-- passing arg at example.lat:84:13 --#
movq %R12, %RSI                           #-- passing arg at example.lat:84:13 --#
cmpq $0, 0 (%RDI)                         #-- example.lat:84:13 --#
jz __handleErrorNull                      #-- example.lat:84:13 --#
movq 20 (%RDI), %RAX                      #-- load address of vtable at example.lat:84:13 --#
call * 32 (%RAX)                          #-- call put at example.lat:84:13 --#
call run_gc                               #-- example.lat:84:13 --#
decl 16 (%RBX)                            #-- incr ref count on VVal example.lat:84:13 (Ref example.lat:84:13 (Cl example.lat:84:13 (IRTargetRefName "Queue"))) (IRValueName "%v_t_99") at example.lat:84:13 --#
call run_gc                               #-- example.lat:84:13 --#
decl 16 (%R12)                            #-- incr ref count on VVal example.lat:84:13 (Ref example.lat:84:13 (Cl example.lat:84:13 (IRTargetRefName "Node"))) (IRValueName "%v_t_96") at example.lat:84:13 --#
movq %RBX, %RDI                           #-- passing arg at example.lat:85:13 --#
call __cl_TopLevel.bfs                    #-- example.lat:85:13 --#
call run_gc                               #-- example.lat:85:13 --#
decl 16 (%RBX)                            #-- incr ref count on VVal example.lat:85:13 (Ref example.lat:85:13 (Cl example.lat:85:13 (IRTargetRefName "Queue"))) (IRValueName "%v_t_99") at example.lat:85:13 --#
xorl %EAX, %EAX                           #-- move return value at example.lat:80:9 --#
leave                                     #-- example.lat:80:9 --#
pop %R12                                  #-- example.lat:80:9 --#
pop %RBX                                  #-- example.lat:80:9 --#
ret                                       #-- example.lat:80:9 --#
Queue.isEmpty :                           #-- example.lat:76:13 --#
pushq %RBX                                #-- example.lat:76:13 --#
pushq %R12                                #-- example.lat:76:13 --#
pushq %RBP                                #-- example.lat:76:13 --#
movq %RSP, %RBP                           #-- example.lat:76:13 --#
Queue.isEmpty.L_entry :                   #-- example.lat:76:13 --#
movq %RDI, %R12                           #-- load %v_t_88 at example.lat:76:13 --#
movq 36 (%R12), %RBX                      #-- load %v_t_92 at example.lat:77:25 --#
incl 16 (%RBX)                            #-- incr ref count on VVal example.lat:77:25 (Ref example.lat:53:13 (Cl example.lat:53:13 (IRTargetRefName "List"))) (IRValueName "%v_t_92") at example.lat:77:25 --#
call run_gc                               #-- example.lat:77:25 --#
decl 16 (%R12)                            #-- incr ref count on VVal example.lat:77:25 (Ref  (Cl  (IRTargetRefName "Queue"))) (IRValueName "%v_t_88") at example.lat:77:25 --#
call run_gc                               #-- example.lat:77:25 --#
decl 16 (%RBX)                            #-- incr ref count on VVal example.lat:77:25 (Ref example.lat:53:13 (Cl example.lat:53:13 (IRTargetRefName "List"))) (IRValueName "%v_t_92") at example.lat:77:25 --#
cmpq _LAT_NULL_ADDR (%RIP), %RBX          #-- example.lat:77:25 --#
je Queue.isEmpty_IIF89                    #-- example.lat:77:25 --#
Queue.isEmpty_IELSE90 :                   #-- example.lat:77:17 --#
xorl %EAX, %EAX                           #-- setting %v_return~2 at example.lat:76:13 --#
jmp Queue.isEmpty.L_exit                  #-- example.lat:76:13 --#
Queue.isEmpty_IIF89 :                     #-- example.lat:77:17 --#
movl $1, %EAX                             #-- setting %v_return~2 at example.lat:76:13 --#
jmp Queue.isEmpty.L_exit                  #-- example.lat:76:13 --#
Queue.isEmpty.L_exit :                    #-- example.lat:76:13 --#
leave                                     #-- example.lat:76:13 --#
pop %R12                                  #-- example.lat:76:13 --#
pop %RBX                                  #-- example.lat:76:13 --#
ret                                       #-- example.lat:76:13 --#
Queue.put :                               #-- example.lat:64:13 --#
pushq %RBX                                #-- example.lat:64:13 --#
pushq %R12                                #-- example.lat:64:13 --#
pushq %R13                                #-- example.lat:64:13 --#
pushq %R14                                #-- example.lat:64:13 --#
pushq %R15                                #-- example.lat:64:13 --#
subq $8, %RSP                             #-- example.lat:64:13 --#
pushq %RBP                                #-- example.lat:64:13 --#
movq %RSP, %RBP                           #-- example.lat:64:13 --#
Queue.put.L_entry :                       #-- example.lat:64:13 --#
movq %RDI, %R12                           #-- load %v_t_74 at example.lat:64:13 --#
movq %RSI, %RBX                           #-- load %v_t_75 at example.lat:64:13 --#
leaq _class_List (%RIP), %RDI             #-- example.lat:65:32 --#
call __new                                #-- example.lat:65:32 --#
movq %RAX, %R13                           #-- example.lat:65:32 --#
addl $2, 16 (%R13)                        #-- incr ref count on VVal example.lat:65:32 (Ref example.lat:65:36 (Cl example.lat:65:36 (IRTargetRefName "List"))) (IRValueName "%v_t_76") at example.lat:65:32 --#
call run_gc                               #-- example.lat:65:22 --#
decl 16 (%R13)                            #-- incr ref count on VVal example.lat:65:22 (Ref example.lat:65:36 (Cl example.lat:65:36 (IRTargetRefName "List"))) (IRValueName "%v_t_76") at example.lat:65:22 --#
movq %R13, %RDI                           #-- passing arg at example.lat:66:17 --#
movq %RBX, %RSI                           #-- passing arg at example.lat:66:17 --#
cmpq $0, 0 (%RDI)                         #-- example.lat:66:17 --#
jz __handleErrorNull                      #-- example.lat:66:17 --#
movq 20 (%RDI), %RAX                      #-- load address of vtable at example.lat:66:17 --#
call * 24 (%RAX)                          #-- call makeSingleton at example.lat:66:17 --#
call run_gc                               #-- example.lat:66:17 --#
decl 16 (%R13)                            #-- incr ref count on VVal example.lat:66:17 (Ref example.lat:66:17 (Cl example.lat:66:17 (IRTargetRefName "List"))) (IRValueName "%v_t_76") at example.lat:66:17 --#
call run_gc                               #-- example.lat:66:17 --#
decl 16 (%RBX)                            #-- incr ref count on VVal example.lat:66:17 (Ref example.lat:64:22 (Cl example.lat:64:22 (IRTargetRefName "Node"))) (IRValueName "%v_t_75") at example.lat:66:17 --#
movq 36 (%R12), %RBX                      #-- load %v_t_82 at example.lat:67:21 --#
incl 16 (%RBX)                            #-- incr ref count on VVal example.lat:67:21 (Ref example.lat:53:13 (Cl example.lat:53:13 (IRTargetRefName "List"))) (IRValueName "%v_t_82") at example.lat:67:21 --#
call run_gc                               #-- example.lat:67:21 --#
decl 16 (%R12)                            #-- incr ref count on VVal example.lat:67:21 (Ref  (Cl  (IRTargetRefName "Queue"))) (IRValueName "%v_t_74") at example.lat:67:21 --#
call run_gc                               #-- example.lat:67:21 --#
decl 16 (%RBX)                            #-- incr ref count on VVal example.lat:67:21 (Ref example.lat:53:13 (Cl example.lat:53:13 (IRTargetRefName "List"))) (IRValueName "%v_t_82") at example.lat:67:21 --#
cmpq _LAT_NULL_ADDR (%RIP), %RBX          #-- example.lat:67:21 --#
je Queue.put_IIF79                        #-- example.lat:67:21 --#
Queue.put_IELSE80 :                       #-- example.lat:67:17 --#
movq 44 (%R12), %R15                      #-- load %v_t_85 at example.lat:72:31 --#
incl 16 (%R15)                            #-- incr ref count on VVal example.lat:72:31 (Ref example.lat:54:13 (Cl example.lat:54:13 (IRTargetRefName "List"))) (IRValueName "%v_t_85") at example.lat:72:31 --#
call run_gc                               #-- example.lat:72:31 --#
decl 16 (%R12)                            #-- incr ref count on VVal example.lat:72:31 (Cl example.lat:72:31 (IRTargetRefName "Queue")) (IRValueName "%v_t_74") at example.lat:72:31 --#
movq %R15, %RDI                           #-- passing arg at example.lat:72:31 --#
cmpq $0, 0 (%RDI)                         #-- example.lat:72:31 --#
jz __handleErrorNull                      #-- example.lat:72:31 --#
movq 20 (%RDI), %RAX                      #-- load address of vtable at example.lat:72:31 --#
call * 32 (%RAX)                          #-- call getHead at example.lat:72:31 --#
movq %RAX, %RBX                           #-- example.lat:72:31 --#
call run_gc                               #-- example.lat:72:31 --#
decl 16 (%R15)                            #-- incr ref count on VVal example.lat:72:31 (Ref example.lat:54:13 (Cl example.lat:54:13 (IRTargetRefName "List"))) (IRValueName "%v_t_85") at example.lat:72:31 --#
movq 44 (%R12), %R15                      #-- load %v_t_84 at example.lat:72:21 --#
incl 16 (%R15)                            #-- incr ref count on VVal example.lat:72:21 (Ref example.lat:54:13 (Cl example.lat:54:13 (IRTargetRefName "List"))) (IRValueName "%v_t_84") at example.lat:72:21 --#
call run_gc                               #-- example.lat:72:21 --#
decl 16 (%R12)                            #-- incr ref count on VVal example.lat:72:21 (Cl example.lat:72:21 (IRTargetRefName "Queue")) (IRValueName "%v_t_74") at example.lat:72:21 --#
movq %R15, %RDI                           #-- passing arg at example.lat:72:21 --#
movq %RBX, %RSI                           #-- passing arg at example.lat:72:21 --#
movq %R13, %RDX                           #-- passing arg at example.lat:72:21 --#
cmpq $0, 0 (%RDI)                         #-- example.lat:72:21 --#
jz __handleErrorNull                      #-- example.lat:72:21 --#
movq 20 (%RDI), %RAX                      #-- load address of vtable at example.lat:72:21 --#
call * 48 (%RAX)                          #-- call cons at example.lat:72:21 --#
call run_gc                               #-- example.lat:72:21 --#
decl 16 (%R13)                            #-- incr ref count on VVal example.lat:72:21 (Ref example.lat:72:21 (Cl example.lat:72:21 (IRTargetRefName "List"))) (IRValueName "%v_t_76") at example.lat:72:21 --#
call run_gc                               #-- example.lat:72:21 --#
decl 16 (%R15)                            #-- incr ref count on VVal example.lat:72:21 (Ref example.lat:54:13 (Cl example.lat:54:13 (IRTargetRefName "List"))) (IRValueName "%v_t_84") at example.lat:72:21 --#
call run_gc                               #-- example.lat:72:21 --#
decl 16 (%RBX)                            #-- incr ref count on VVal example.lat:72:21 (Ref example.lat:41:13 (Cl example.lat:41:13 (IRTargetRefName "Node"))) (IRValueName "%v_t_86") at example.lat:72:21 --#
movq 44 (%R12), %RBX                      #-- load %v_c15 at example.lat:73:21 --#
call run_gc                               #-- example.lat:73:21 --#
decl 16 (%RBX)                            #-- incr ref count on VVal example.lat:73:21 (Ref example.lat:54:13 (Cl example.lat:54:13 (IRTargetRefName "List"))) (IRValueName "%v_c15") at example.lat:73:21 --#
incl 16 (%R13)                            #-- incr ref count on VVal example.lat:73:21 (Ref example.lat:73:21 (Cl example.lat:73:21 (IRTargetRefName "List"))) (IRValueName "%v_t_76") at example.lat:73:21 --#
movq %R13, 44 (%R12)                      #-- example.lat:73:21 --#
call run_gc                               #-- example.lat:73:21 --#
decl 16 (%R13)                            #-- incr ref count on VVal example.lat:73:21 (Ref example.lat:73:21 (Cl example.lat:73:21 (IRTargetRefName "List"))) (IRValueName "%v_t_76") at example.lat:73:21 --#
call run_gc                               #-- example.lat:73:21 --#
decl 16 (%R12)                            #-- incr ref count on VVal example.lat:73:21 (Cl example.lat:73:21 (IRTargetRefName "Queue")) (IRValueName "%v_t_74") at example.lat:73:21 --#
jmp Queue.put_IEND81                      #-- example.lat:67:17 --#
Queue.put_IIF79 :                         #-- example.lat:67:17 --#
movq 36 (%R12), %RBX                      #-- load %v_c13 at example.lat:68:21 --#
call run_gc                               #-- example.lat:68:21 --#
decl 16 (%RBX)                            #-- incr ref count on VVal example.lat:68:21 (Ref example.lat:53:13 (Cl example.lat:53:13 (IRTargetRefName "List"))) (IRValueName "%v_c13") at example.lat:68:21 --#
incl 16 (%R13)                            #-- incr ref count on VVal example.lat:68:21 (Ref example.lat:68:21 (Cl example.lat:68:21 (IRTargetRefName "List"))) (IRValueName "%v_t_76") at example.lat:68:21 --#
movq %R13, 36 (%R12)                      #-- example.lat:68:21 --#
call run_gc                               #-- example.lat:68:21 --#
decl 16 (%R13)                            #-- incr ref count on VVal example.lat:68:21 (Ref example.lat:68:21 (Cl example.lat:68:21 (IRTargetRefName "List"))) (IRValueName "%v_t_76") at example.lat:68:21 --#
call run_gc                               #-- example.lat:68:21 --#
decl 16 (%R12)                            #-- incr ref count on VVal example.lat:68:21 (Cl example.lat:68:21 (IRTargetRefName "Queue")) (IRValueName "%v_t_74") at example.lat:68:21 --#
movq 44 (%R12), %RBX                      #-- load %v_c14 at example.lat:69:21 --#
call run_gc                               #-- example.lat:69:21 --#
decl 16 (%RBX)                            #-- incr ref count on VVal example.lat:69:21 (Ref example.lat:54:13 (Cl example.lat:54:13 (IRTargetRefName "List"))) (IRValueName "%v_c14") at example.lat:69:21 --#
incl 16 (%R13)                            #-- incr ref count on VVal example.lat:69:21 (Ref example.lat:69:21 (Cl example.lat:69:21 (IRTargetRefName "List"))) (IRValueName "%v_t_76") at example.lat:69:21 --#
movq %R13, 44 (%R12)                      #-- example.lat:69:21 --#
call run_gc                               #-- example.lat:69:21 --#
decl 16 (%R13)                            #-- incr ref count on VVal example.lat:69:21 (Ref example.lat:69:21 (Cl example.lat:69:21 (IRTargetRefName "List"))) (IRValueName "%v_t_76") at example.lat:69:21 --#
call run_gc                               #-- example.lat:69:21 --#
decl 16 (%R12)                            #-- incr ref count on VVal example.lat:69:21 (Cl example.lat:69:21 (IRTargetRefName "Queue")) (IRValueName "%v_t_74") at example.lat:69:21 --#
jmp Queue.put_IEND81                      #-- example.lat:67:17 --#
Queue.put_IEND81 :                        #-- example.lat:67:17 --#
movl %R14D, %EAX                          #-- move return value at example.lat:64:13 --#
leave                                     #-- example.lat:64:13 --#
addq $8, %RSP                             #-- example.lat:64:13 --#
pop %R15                                  #-- example.lat:64:13 --#
pop %R14                                  #-- example.lat:64:13 --#
pop %R13                                  #-- example.lat:64:13 --#
pop %R12                                  #-- example.lat:64:13 --#
pop %RBX                                  #-- example.lat:64:13 --#
ret                                       #-- example.lat:64:13 --#
Queue.get :                               #-- example.lat:55:13 --#
pushq %RBX                                #-- example.lat:55:13 --#
pushq %R12                                #-- example.lat:55:13 --#
pushq %R13                                #-- example.lat:55:13 --#
pushq %R14                                #-- example.lat:55:13 --#
pushq %RBP                                #-- example.lat:55:13 --#
movq %RSP, %RBP                           #-- example.lat:55:13 --#
Queue.get.L_entry :                       #-- example.lat:55:13 --#
movq %RDI, %R12                           #-- load %v_t_56 at example.lat:55:13 --#
movq 36 (%R12), %RBX                      #-- load %v_t_60 at example.lat:56:21 --#
incl 16 (%RBX)                            #-- incr ref count on VVal example.lat:56:21 (Ref example.lat:53:13 (Cl example.lat:53:13 (IRTargetRefName "List"))) (IRValueName "%v_t_60") at example.lat:56:21 --#
call run_gc                               #-- example.lat:56:21 --#
decl 16 (%R12)                            #-- incr ref count on VVal example.lat:56:21 (Ref  (Cl  (IRTargetRefName "Queue"))) (IRValueName "%v_t_56") at example.lat:56:21 --#
call run_gc                               #-- example.lat:56:21 --#
decl 16 (%RBX)                            #-- incr ref count on VVal example.lat:56:21 (Ref example.lat:53:13 (Cl example.lat:53:13 (IRTargetRefName "List"))) (IRValueName "%v_t_60") at example.lat:56:21 --#
cmpq _LAT_NULL_ADDR (%RIP), %RBX          #-- example.lat:56:21 --#
je Queue.get_IIF57                        #-- example.lat:56:21 --#
Queue.get_IELSE58 :                       #-- example.lat:56:17 --#
movq 36 (%R12), %RBX                      #-- load %v_t_63 at example.lat:58:30 --#
incl 16 (%RBX)                            #-- incr ref count on VVal example.lat:58:30 (Ref example.lat:53:13 (Cl example.lat:53:13 (IRTargetRefName "List"))) (IRValueName "%v_t_63") at example.lat:58:30 --#
call run_gc                               #-- example.lat:58:30 --#
decl 16 (%R12)                            #-- incr ref count on VVal example.lat:58:30 (Cl example.lat:58:30 (IRTargetRefName "Queue")) (IRValueName "%v_t_56") at example.lat:58:30 --#
movq 36 (%RBX), %R13                      #-- load %v_t_64 at example.lat:58:30 --#
incl 16 (%R13)                            #-- incr ref count on VVal example.lat:58:30 (Ref example.lat:35:13 (Cl example.lat:35:13 (IRTargetRefName "Node"))) (IRValueName "%v_t_64") at example.lat:58:30 --#
call run_gc                               #-- example.lat:58:30 --#
decl 16 (%RBX)                            #-- incr ref count on VVal example.lat:58:30 (Ref example.lat:53:13 (Cl example.lat:53:13 (IRTargetRefName "List"))) (IRValueName "%v_t_63") at example.lat:58:30 --#
incl 16 (%R13)                            #-- incr ref count on VVal example.lat:58:22 (Ref example.lat:58:22 (Cl example.lat:58:22 (IRTargetRefName "Node"))) (IRValueName "%v_t_64") at example.lat:58:22 --#
call run_gc                               #-- example.lat:58:22 --#
decl 16 (%R13)                            #-- incr ref count on VVal example.lat:58:22 (Ref example.lat:35:13 (Cl example.lat:35:13 (IRTargetRefName "Node"))) (IRValueName "%v_t_64") at example.lat:58:22 --#
movq 36 (%R12), %R14                      #-- load %v_t_66 at example.lat:59:25 --#
incl 16 (%R14)                            #-- incr ref count on VVal example.lat:59:25 (Ref example.lat:53:13 (Cl example.lat:53:13 (IRTargetRefName "List"))) (IRValueName "%v_t_66") at example.lat:59:25 --#
call run_gc                               #-- example.lat:59:25 --#
decl 16 (%R12)                            #-- incr ref count on VVal example.lat:59:25 (Cl example.lat:59:25 (IRTargetRefName "Queue")) (IRValueName "%v_t_56") at example.lat:59:25 --#
movq 44 (%R14), %RBX                      #-- load %v_t_67 at example.lat:59:25 --#
incl 16 (%RBX)                            #-- incr ref count on VVal example.lat:59:25 (Ref example.lat:36:13 (Cl example.lat:36:13 (IRTargetRefName "List"))) (IRValueName "%v_t_67") at example.lat:59:25 --#
call run_gc                               #-- example.lat:59:25 --#
decl 16 (%R14)                            #-- incr ref count on VVal example.lat:59:25 (Ref example.lat:53:13 (Cl example.lat:53:13 (IRTargetRefName "List"))) (IRValueName "%v_t_66") at example.lat:59:25 --#
movq 36 (%R12), %R14                      #-- load %v_c10 at example.lat:59:17 --#
call run_gc                               #-- example.lat:59:17 --#
decl 16 (%R14)                            #-- incr ref count on VVal example.lat:59:17 (Ref example.lat:53:13 (Cl example.lat:53:13 (IRTargetRefName "List"))) (IRValueName "%v_c10") at example.lat:59:17 --#
incl 16 (%RBX)                            #-- incr ref count on VVal example.lat:59:17 (Ref example.lat:59:17 (Cl example.lat:59:17 (IRTargetRefName "List"))) (IRValueName "%v_t_67") at example.lat:59:17 --#
movq %RBX, 36 (%R12)                      #-- example.lat:59:17 --#
call run_gc                               #-- example.lat:59:17 --#
decl 16 (%R12)                            #-- incr ref count on VVal example.lat:59:17 (Cl example.lat:59:17 (IRTargetRefName "Queue")) (IRValueName "%v_t_56") at example.lat:59:17 --#
call run_gc                               #-- example.lat:59:17 --#
decl 16 (%RBX)                            #-- incr ref count on VVal example.lat:59:17 (Ref example.lat:36:13 (Cl example.lat:36:13 (IRTargetRefName "List"))) (IRValueName "%v_t_67") at example.lat:59:17 --#
movq 36 (%R12), %RBX                      #-- load %v_t_71 at example.lat:60:21 --#
incl 16 (%RBX)                            #-- incr ref count on VVal example.lat:60:21 (Ref example.lat:53:13 (Cl example.lat:53:13 (IRTargetRefName "List"))) (IRValueName "%v_t_71") at example.lat:60:21 --#
call run_gc                               #-- example.lat:60:21 --#
decl 16 (%R12)                            #-- incr ref count on VVal example.lat:60:21 (Cl example.lat:60:21 (IRTargetRefName "Queue")) (IRValueName "%v_t_56") at example.lat:60:21 --#
call run_gc                               #-- example.lat:60:21 --#
decl 16 (%RBX)                            #-- incr ref count on VVal example.lat:60:21 (Ref example.lat:53:13 (Cl example.lat:53:13 (IRTargetRefName "List"))) (IRValueName "%v_t_71") at example.lat:60:21 --#
cmpq _LAT_NULL_ADDR (%RIP), %RBX          #-- example.lat:60:21 --#
je Queue.get_IIF68                        #-- example.lat:60:21 --#
Queue.get_IELSE69 :                       #-- example.lat:60:17 --#
jmp Queue.get_IEND70                      #-- example.lat:60:17 --#
Queue.get_IEND70 :                        #-- example.lat:60:17 --#
incl 16 (%R13)                            #-- incr ref count on VVal example.lat:62:17 (Ref example.lat:62:17 (Cl example.lat:62:17 (IRTargetRefName "Node"))) (IRValueName "%v_t_64") at example.lat:62:17 --#
call run_gc                               #-- example.lat:62:17 --#
decl 16 (%R13)                            #-- incr ref count on VVal example.lat:62:17 (Ref example.lat:62:17 (Cl example.lat:62:17 (IRTargetRefName "Node"))) (IRValueName "%v_t_64") at example.lat:62:17 --#
xchgq %RAX, %R13                          #-- example.lat:55:13 --#
jmp Queue.get.L_exit                      #-- example.lat:55:13 --#
Queue.get_IIF57 :                         #-- example.lat:56:17 --#
movq _LAT_NULL_ADDR (%RIP), %RAX          #-- setting %v_return~2 at example.lat:55:13 --#
jmp Queue.get.L_exit                      #-- example.lat:55:13 --#
Queue.get_IIF68 :                         #-- example.lat:60:17 --#
movq 44 (%R12), %RBX                      #-- load %v_c11 at example.lat:61:21 --#
call run_gc                               #-- example.lat:61:21 --#
decl 16 (%RBX)                            #-- incr ref count on VVal example.lat:61:21 (Ref example.lat:54:13 (Cl example.lat:54:13 (IRTargetRefName "List"))) (IRValueName "%v_c11") at example.lat:61:21 --#
movq _LAT_NULL_ADDR (%RIP), 44 (%R12)     #-- example.lat:61:21 --#
call run_gc                               #-- example.lat:61:21 --#
decl 16 (%R12)                            #-- incr ref count on VVal example.lat:61:21 (Cl example.lat:61:21 (IRTargetRefName "Queue")) (IRValueName "%v_t_56") at example.lat:61:21 --#
jmp Queue.get_IEND70                      #-- example.lat:60:17 --#
Queue.get.L_exit :                        #-- example.lat:55:13 --#
leave                                     #-- example.lat:55:13 --#
pop %R14                                  #-- example.lat:55:13 --#
pop %R13                                  #-- example.lat:55:13 --#
pop %R12                                  #-- example.lat:55:13 --#
pop %RBX                                  #-- example.lat:55:13 --#
ret                                       #-- example.lat:55:13 --#
List.cons :                               #-- example.lat:47:13 --#
pushq %RBX                                #-- example.lat:47:13 --#
pushq %R12                                #-- example.lat:47:13 --#
pushq %R13                                #-- example.lat:47:13 --#
pushq %R14                                #-- example.lat:47:13 --#
pushq %R15                                #-- example.lat:47:13 --#
subq $8, %RSP                             #-- example.lat:47:13 --#
pushq %RBP                                #-- example.lat:47:13 --#
movq %RSP, %RBP                           #-- example.lat:47:13 --#
List.cons.L_entry :                       #-- example.lat:47:13 --#
movq %RDI, %R13                           #-- load %v_t_53 at example.lat:47:13 --#
movq %RSI, %R14                           #-- load %v_t_54 at example.lat:47:13 --#
movq %RDX, %RBX                           #-- load %v_t_55 at example.lat:47:13 --#
movq 36 (%R13), %R15                      #-- load %v_c8 at example.lat:48:17 --#
call run_gc                               #-- example.lat:48:17 --#
decl 16 (%R15)                            #-- incr ref count on VVal example.lat:48:17 (Ref example.lat:35:13 (Cl example.lat:35:13 (IRTargetRefName "Node"))) (IRValueName "%v_c8") at example.lat:48:17 --#
incl 16 (%R14)                            #-- incr ref count on VVal example.lat:48:17 (Ref example.lat:48:17 (Cl example.lat:48:17 (IRTargetRefName "Node"))) (IRValueName "%v_t_54") at example.lat:48:17 --#
movq %R14, 36 (%R13)                      #-- example.lat:48:17 --#
call run_gc                               #-- example.lat:48:17 --#
decl 16 (%R13)                            #-- incr ref count on VVal example.lat:48:17 (Ref  (Cl  (IRTargetRefName "List"))) (IRValueName "%v_t_53") at example.lat:48:17 --#
call run_gc                               #-- example.lat:48:17 --#
decl 16 (%R14)                            #-- incr ref count on VVal example.lat:48:17 (Ref example.lat:47:23 (Cl example.lat:47:23 (IRTargetRefName "Node"))) (IRValueName "%v_t_54") at example.lat:48:17 --#
movq 44 (%R13), %R14                      #-- load %v_c9 at example.lat:49:17 --#
call run_gc                               #-- example.lat:49:17 --#
decl 16 (%R14)                            #-- incr ref count on VVal example.lat:49:17 (Ref example.lat:36:13 (Cl example.lat:36:13 (IRTargetRefName "List"))) (IRValueName "%v_c9") at example.lat:49:17 --#
incl 16 (%RBX)                            #-- incr ref count on VVal example.lat:49:17 (Ref example.lat:49:17 (Cl example.lat:49:17 (IRTargetRefName "List"))) (IRValueName "%v_t_55") at example.lat:49:17 --#
movq %RBX, 44 (%R13)                      #-- example.lat:49:17 --#
call run_gc                               #-- example.lat:49:17 --#
decl 16 (%RBX)                            #-- incr ref count on VVal example.lat:49:17 (Ref example.lat:47:37 (Cl example.lat:47:37 (IRTargetRefName "List"))) (IRValueName "%v_t_55") at example.lat:49:17 --#
call run_gc                               #-- example.lat:49:17 --#
decl 16 (%R13)                            #-- incr ref count on VVal example.lat:49:17 (Ref  (Cl  (IRTargetRefName "List"))) (IRValueName "%v_t_53") at example.lat:49:17 --#
movl %R12D, %EAX                          #-- move return value at example.lat:47:13 --#
leave                                     #-- example.lat:47:13 --#
addq $8, %RSP                             #-- example.lat:47:13 --#
pop %R15                                  #-- example.lat:47:13 --#
pop %R14                                  #-- example.lat:47:13 --#
pop %R13                                  #-- example.lat:47:13 --#
pop %R12                                  #-- example.lat:47:13 --#
pop %RBX                                  #-- example.lat:47:13 --#
ret                                       #-- example.lat:47:13 --#
List.getTail :                            #-- example.lat:44:13 --#
pushq %RBX                                #-- example.lat:44:13 --#
pushq %R12                                #-- example.lat:44:13 --#
pushq %RBP                                #-- example.lat:44:13 --#
movq %RSP, %RBP                           #-- example.lat:44:13 --#
List.getTail.L_entry :                    #-- example.lat:44:13 --#
movq %RDI, %R12                           #-- load %v_t_51 at example.lat:44:13 --#
movq 44 (%R12), %RBX                      #-- load %v_t_52 at example.lat:45:24 --#
incl 16 (%RBX)                            #-- incr ref count on VVal example.lat:45:24 (Ref example.lat:36:13 (Cl example.lat:36:13 (IRTargetRefName "List"))) (IRValueName "%v_t_52") at example.lat:45:24 --#
call run_gc                               #-- example.lat:45:24 --#
decl 16 (%R12)                            #-- incr ref count on VVal example.lat:45:24 (Ref  (Cl  (IRTargetRefName "List"))) (IRValueName "%v_t_51") at example.lat:45:24 --#
incl 16 (%RBX)                            #-- incr ref count on VVal example.lat:45:17 (Ref example.lat:45:17 (Cl example.lat:45:17 (IRTargetRefName "List"))) (IRValueName "%v_t_52") at example.lat:45:17 --#
call run_gc                               #-- example.lat:45:17 --#
decl 16 (%RBX)                            #-- incr ref count on VVal example.lat:45:17 (Ref example.lat:36:13 (Cl example.lat:36:13 (IRTargetRefName "List"))) (IRValueName "%v_t_52") at example.lat:45:17 --#
movq %RBX, %RAX                           #-- move return value at example.lat:44:13 --#
leave                                     #-- example.lat:44:13 --#
pop %R12                                  #-- example.lat:44:13 --#
pop %RBX                                  #-- example.lat:44:13 --#
ret                                       #-- example.lat:44:13 --#
List.getHead :                            #-- example.lat:41:13 --#
pushq %RBX                                #-- example.lat:41:13 --#
pushq %R12                                #-- example.lat:41:13 --#
pushq %RBP                                #-- example.lat:41:13 --#
movq %RSP, %RBP                           #-- example.lat:41:13 --#
List.getHead.L_entry :                    #-- example.lat:41:13 --#
movq %RDI, %R12                           #-- load %v_t_49 at example.lat:41:13 --#
movq 36 (%R12), %RBX                      #-- load %v_t_50 at example.lat:42:24 --#
incl 16 (%RBX)                            #-- incr ref count on VVal example.lat:42:24 (Ref example.lat:35:13 (Cl example.lat:35:13 (IRTargetRefName "Node"))) (IRValueName "%v_t_50") at example.lat:42:24 --#
call run_gc                               #-- example.lat:42:24 --#
decl 16 (%R12)                            #-- incr ref count on VVal example.lat:42:24 (Ref  (Cl  (IRTargetRefName "List"))) (IRValueName "%v_t_49") at example.lat:42:24 --#
incl 16 (%RBX)                            #-- incr ref count on VVal example.lat:42:17 (Ref example.lat:42:17 (Cl example.lat:42:17 (IRTargetRefName "Node"))) (IRValueName "%v_t_50") at example.lat:42:17 --#
call run_gc                               #-- example.lat:42:17 --#
decl 16 (%RBX)                            #-- incr ref count on VVal example.lat:42:17 (Ref example.lat:35:13 (Cl example.lat:35:13 (IRTargetRefName "Node"))) (IRValueName "%v_t_50") at example.lat:42:17 --#
movq %RBX, %RAX                           #-- move return value at example.lat:41:13 --#
leave                                     #-- example.lat:41:13 --#
pop %R12                                  #-- example.lat:41:13 --#
pop %RBX                                  #-- example.lat:41:13 --#
ret                                       #-- example.lat:41:13 --#
List.makeSingleton :                      #-- example.lat:37:13 --#
pushq %RBX                                #-- example.lat:37:13 --#
pushq %R12                                #-- example.lat:37:13 --#
pushq %R13                                #-- example.lat:37:13 --#
pushq %R14                                #-- example.lat:37:13 --#
pushq %RBP                                #-- example.lat:37:13 --#
movq %RSP, %RBP                           #-- example.lat:37:13 --#
List.makeSingleton.L_entry :              #-- example.lat:37:13 --#
movq %RDI, %R12                           #-- load %v_t_46 at example.lat:37:13 --#
movq %RSI, %RBX                           #-- load %v_t_47 at example.lat:37:13 --#
movq 36 (%R12), %R14                      #-- load %v_c4 at example.lat:38:17 --#
call run_gc                               #-- example.lat:38:17 --#
decl 16 (%R14)                            #-- incr ref count on VVal example.lat:38:17 (Ref example.lat:35:13 (Cl example.lat:35:13 (IRTargetRefName "Node"))) (IRValueName "%v_c4") at example.lat:38:17 --#
incl 16 (%RBX)                            #-- incr ref count on VVal example.lat:38:17 (Ref example.lat:38:17 (Cl example.lat:38:17 (IRTargetRefName "Node"))) (IRValueName "%v_t_47") at example.lat:38:17 --#
movq %RBX, 36 (%R12)                      #-- example.lat:38:17 --#
call run_gc                               #-- example.lat:38:17 --#
decl 16 (%R12)                            #-- incr ref count on VVal example.lat:38:17 (Ref  (Cl  (IRTargetRefName "List"))) (IRValueName "%v_t_46") at example.lat:38:17 --#
call run_gc                               #-- example.lat:38:17 --#
decl 16 (%RBX)                            #-- incr ref count on VVal example.lat:38:17 (Ref example.lat:37:32 (Cl example.lat:37:32 (IRTargetRefName "Node"))) (IRValueName "%v_t_47") at example.lat:38:17 --#
movq 44 (%R12), %RBX                      #-- load %v_c5 at example.lat:39:17 --#
call run_gc                               #-- example.lat:39:17 --#
decl 16 (%RBX)                            #-- incr ref count on VVal example.lat:39:17 (Ref example.lat:36:13 (Cl example.lat:36:13 (IRTargetRefName "List"))) (IRValueName "%v_c5") at example.lat:39:17 --#
movq _LAT_NULL_ADDR (%RIP), 44 (%R12)     #-- example.lat:39:17 --#
call run_gc                               #-- example.lat:39:17 --#
decl 16 (%R12)                            #-- incr ref count on VVal example.lat:39:17 (Ref  (Cl  (IRTargetRefName "List"))) (IRValueName "%v_t_46") at example.lat:39:17 --#
movl %R13D, %EAX                          #-- move return value at example.lat:37:13 --#
leave                                     #-- example.lat:37:13 --#
pop %R14                                  #-- example.lat:37:13 --#
pop %R13                                  #-- example.lat:37:13 --#
pop %R12                                  #-- example.lat:37:13 --#
pop %RBX                                  #-- example.lat:37:13 --#
ret                                       #-- example.lat:37:13 --#
Node.addNeighbour :                       #-- example.lat:22:13 --#
pushq %RBX                                #-- example.lat:22:13 --#
pushq %R12                                #-- example.lat:22:13 --#
pushq %R13                                #-- example.lat:22:13 --#
pushq %R14                                #-- example.lat:22:13 --#
pushq %R15                                #-- example.lat:22:13 --#
subq $8, %RSP                             #-- example.lat:22:13 --#
pushq %RBP                                #-- example.lat:22:13 --#
movq %RSP, %RBP                           #-- example.lat:22:13 --#
Node.addNeighbour.L_entry :               #-- example.lat:22:13 --#
movq %RDI, %R13                           #-- load %v_t_32 at example.lat:22:13 --#
movq %RSI, %R12                           #-- load %v_t_33 at example.lat:22:13 --#
movq 41 (%R13), %RBX                      #-- load %v_t_37 at example.lat:23:21 --#
incl 16 (%RBX)                            #-- incr ref count on VVal example.lat:23:21 (Ref example.lat:4:13 (Cl example.lat:4:13 (IRTargetRefName "List"))) (IRValueName "%v_t_37") at example.lat:23:21 --#
call run_gc                               #-- example.lat:23:21 --#
decl 16 (%R13)                            #-- incr ref count on VVal example.lat:23:21 (Ref  (Cl  (IRTargetRefName "Node"))) (IRValueName "%v_t_32") at example.lat:23:21 --#
call run_gc                               #-- example.lat:23:21 --#
decl 16 (%RBX)                            #-- incr ref count on VVal example.lat:23:21 (Ref example.lat:4:13 (Cl example.lat:4:13 (IRTargetRefName "List"))) (IRValueName "%v_t_37") at example.lat:23:21 --#
cmpq _LAT_NULL_ADDR (%RIP), %RBX          #-- example.lat:23:21 --#
je Node.addNeighbour_IIF34                #-- example.lat:23:21 --#
Node.addNeighbour_IELSE35 :               #-- example.lat:23:17 --#
leaq _class_List (%RIP), %RDI             #-- example.lat:28:42 --#
call __new                                #-- example.lat:28:42 --#
movq %RAX, %RBX                           #-- example.lat:28:42 --#
addl $2, 16 (%RBX)                        #-- incr ref count on VVal example.lat:28:42 (Ref example.lat:28:46 (Cl example.lat:28:46 (IRTargetRefName "List"))) (IRValueName "%v_t_42") at example.lat:28:42 --#
call run_gc                               #-- example.lat:28:26 --#
decl 16 (%RBX)                            #-- incr ref count on VVal example.lat:28:26 (Ref example.lat:28:46 (Cl example.lat:28:46 (IRTargetRefName "List"))) (IRValueName "%v_t_42") at example.lat:28:26 --#
movq 41 (%R13), %R15                      #-- load %v_t_44 at example.lat:29:43 --#
incl 16 (%R15)                            #-- incr ref count on VVal example.lat:29:43 (Ref example.lat:4:13 (Cl example.lat:4:13 (IRTargetRefName "List"))) (IRValueName "%v_t_44") at example.lat:29:43 --#
call run_gc                               #-- example.lat:29:43 --#
decl 16 (%R13)                            #-- incr ref count on VVal example.lat:29:43 (Cl example.lat:29:43 (IRTargetRefName "Node")) (IRValueName "%v_t_32") at example.lat:29:43 --#
movq %RBX, %RDI                           #-- passing arg at example.lat:29:21 --#
movq %R12, %RSI                           #-- passing arg at example.lat:29:21 --#
movq %R15, %RDX                           #-- passing arg at example.lat:29:21 --#
cmpq $0, 0 (%RDI)                         #-- example.lat:29:21 --#
jz __handleErrorNull                      #-- example.lat:29:21 --#
movq 20 (%RDI), %RAX                      #-- load address of vtable at example.lat:29:21 --#
call * 48 (%RAX)                          #-- call cons at example.lat:29:21 --#
call run_gc                               #-- example.lat:29:21 --#
decl 16 (%RBX)                            #-- incr ref count on VVal example.lat:29:21 (Ref example.lat:29:21 (Cl example.lat:29:21 (IRTargetRefName "List"))) (IRValueName "%v_t_42") at example.lat:29:21 --#
call run_gc                               #-- example.lat:29:21 --#
decl 16 (%R12)                            #-- incr ref count on VVal example.lat:29:21 (Ref example.lat:29:21 (Cl example.lat:29:21 (IRTargetRefName "Node"))) (IRValueName "%v_t_33") at example.lat:29:21 --#
call run_gc                               #-- example.lat:29:21 --#
decl 16 (%R15)                            #-- incr ref count on VVal example.lat:29:21 (Ref example.lat:4:13 (Cl example.lat:4:13 (IRTargetRefName "List"))) (IRValueName "%v_t_44") at example.lat:29:21 --#
movq 41 (%R13), %R12                      #-- load %v_c3 at example.lat:30:21 --#
call run_gc                               #-- example.lat:30:21 --#
decl 16 (%R12)                            #-- incr ref count on VVal example.lat:30:21 (Ref example.lat:4:13 (Cl example.lat:4:13 (IRTargetRefName "List"))) (IRValueName "%v_c3") at example.lat:30:21 --#
incl 16 (%RBX)                            #-- incr ref count on VVal example.lat:30:21 (Ref example.lat:30:21 (Cl example.lat:30:21 (IRTargetRefName "List"))) (IRValueName "%v_t_42") at example.lat:30:21 --#
movq %RBX, 41 (%R13)                      #-- example.lat:30:21 --#
call run_gc                               #-- example.lat:30:21 --#
decl 16 (%RBX)                            #-- incr ref count on VVal example.lat:30:21 (Ref example.lat:30:21 (Cl example.lat:30:21 (IRTargetRefName "List"))) (IRValueName "%v_t_42") at example.lat:30:21 --#
call run_gc                               #-- example.lat:30:21 --#
decl 16 (%R13)                            #-- incr ref count on VVal example.lat:30:21 (Cl example.lat:30:21 (IRTargetRefName "Node")) (IRValueName "%v_t_32") at example.lat:30:21 --#
jmp Node.addNeighbour_IEND36              #-- example.lat:23:17 --#
Node.addNeighbour_IIF34 :                 #-- example.lat:23:17 --#
leaq _class_List (%RIP), %RDI             #-- example.lat:24:34 --#
call __new                                #-- example.lat:24:34 --#
movq %RAX, %RBX                           #-- example.lat:24:34 --#
incl 16 (%RBX)                            #-- incr ref count on VVal example.lat:24:34 (Ref example.lat:24:38 (Cl example.lat:24:38 (IRTargetRefName "List"))) (IRValueName "%v_t_39") at example.lat:24:34 --#
movq 41 (%R13), %R15                      #-- load %v_c2 at example.lat:24:21 --#
call run_gc                               #-- example.lat:24:21 --#
decl 16 (%R15)                            #-- incr ref count on VVal example.lat:24:21 (Ref example.lat:4:13 (Cl example.lat:4:13 (IRTargetRefName "List"))) (IRValueName "%v_c2") at example.lat:24:21 --#
incl 16 (%RBX)                            #-- incr ref count on VVal example.lat:24:21 (Ref example.lat:24:21 (Cl example.lat:24:21 (IRTargetRefName "List"))) (IRValueName "%v_t_39") at example.lat:24:21 --#
movq %RBX, 41 (%R13)                      #-- example.lat:24:21 --#
call run_gc                               #-- example.lat:24:21 --#
decl 16 (%R13)                            #-- incr ref count on VVal example.lat:24:21 (Cl example.lat:24:21 (IRTargetRefName "Node")) (IRValueName "%v_t_32") at example.lat:24:21 --#
call run_gc                               #-- example.lat:24:21 --#
decl 16 (%RBX)                            #-- incr ref count on VVal example.lat:24:21 (Ref example.lat:24:38 (Cl example.lat:24:38 (IRTargetRefName "List"))) (IRValueName "%v_t_39") at example.lat:24:21 --#
movq 41 (%R13), %RBX                      #-- load %v_t_40 at example.lat:25:21 --#
incl 16 (%RBX)                            #-- incr ref count on VVal example.lat:25:21 (Ref example.lat:4:13 (Cl example.lat:4:13 (IRTargetRefName "List"))) (IRValueName "%v_t_40") at example.lat:25:21 --#
call run_gc                               #-- example.lat:25:21 --#
decl 16 (%R13)                            #-- incr ref count on VVal example.lat:25:21 (Cl example.lat:25:21 (IRTargetRefName "Node")) (IRValueName "%v_t_32") at example.lat:25:21 --#
movq %RBX, %RDI                           #-- passing arg at example.lat:25:21 --#
movq %R12, %RSI                           #-- passing arg at example.lat:25:21 --#
cmpq $0, 0 (%RDI)                         #-- example.lat:25:21 --#
jz __handleErrorNull                      #-- example.lat:25:21 --#
movq 20 (%RDI), %RAX                      #-- load address of vtable at example.lat:25:21 --#
call * 24 (%RAX)                          #-- call makeSingleton at example.lat:25:21 --#
call run_gc                               #-- example.lat:25:21 --#
decl 16 (%R12)                            #-- incr ref count on VVal example.lat:25:21 (Ref example.lat:25:21 (Cl example.lat:25:21 (IRTargetRefName "Node"))) (IRValueName "%v_t_33") at example.lat:25:21 --#
call run_gc                               #-- example.lat:25:21 --#
decl 16 (%RBX)                            #-- incr ref count on VVal example.lat:25:21 (Ref example.lat:4:13 (Cl example.lat:4:13 (IRTargetRefName "List"))) (IRValueName "%v_t_40") at example.lat:25:21 --#
jmp Node.addNeighbour_IEND36              #-- example.lat:23:17 --#
Node.addNeighbour_IEND36 :                #-- example.lat:23:17 --#
movl %R14D, %EAX                          #-- move return value at example.lat:22:13 --#
leave                                     #-- example.lat:22:13 --#
addq $8, %RSP                             #-- example.lat:22:13 --#
pop %R15                                  #-- example.lat:22:13 --#
pop %R14                                  #-- example.lat:22:13 --#
pop %R13                                  #-- example.lat:22:13 --#
pop %R12                                  #-- example.lat:22:13 --#
pop %RBX                                  #-- example.lat:22:13 --#
ret                                       #-- example.lat:22:13 --#
Node.getNeighbours :                      #-- example.lat:19:13 --#
pushq %RBX                                #-- example.lat:19:13 --#
pushq %R12                                #-- example.lat:19:13 --#
pushq %RBP                                #-- example.lat:19:13 --#
movq %RSP, %RBP                           #-- example.lat:19:13 --#
Node.getNeighbours.L_entry :              #-- example.lat:19:13 --#
movq %RDI, %R12                           #-- load %v_t_30 at example.lat:19:13 --#
movq 41 (%R12), %RBX                      #-- load %v_t_31 at example.lat:20:24 --#
incl 16 (%RBX)                            #-- incr ref count on VVal example.lat:20:24 (Ref example.lat:4:13 (Cl example.lat:4:13 (IRTargetRefName "List"))) (IRValueName "%v_t_31") at example.lat:20:24 --#
call run_gc                               #-- example.lat:20:24 --#
decl 16 (%R12)                            #-- incr ref count on VVal example.lat:20:24 (Ref  (Cl  (IRTargetRefName "Node"))) (IRValueName "%v_t_30") at example.lat:20:24 --#
incl 16 (%RBX)                            #-- incr ref count on VVal example.lat:20:17 (Ref example.lat:20:17 (Cl example.lat:20:17 (IRTargetRefName "List"))) (IRValueName "%v_t_31") at example.lat:20:17 --#
call run_gc                               #-- example.lat:20:17 --#
decl 16 (%RBX)                            #-- incr ref count on VVal example.lat:20:17 (Ref example.lat:4:13 (Cl example.lat:4:13 (IRTargetRefName "List"))) (IRValueName "%v_t_31") at example.lat:20:17 --#
movq %RBX, %RAX                           #-- move return value at example.lat:19:13 --#
leave                                     #-- example.lat:19:13 --#
pop %R12                                  #-- example.lat:19:13 --#
pop %RBX                                  #-- example.lat:19:13 --#
ret                                       #-- example.lat:19:13 --#
Node.getValue :                           #-- example.lat:16:13 --#
pushq %RBX                                #-- example.lat:16:13 --#
pushq %R12                                #-- example.lat:16:13 --#
pushq %RBP                                #-- example.lat:16:13 --#
movq %RSP, %RBP                           #-- example.lat:16:13 --#
Node.getValue.L_entry :                   #-- example.lat:16:13 --#
movq %RDI, %R12                           #-- load %v_t_28 at example.lat:16:13 --#
movl 37 (%R12), %EBX                      #-- load %v_t_29 at example.lat:17:24 --#
call run_gc                               #-- example.lat:17:24 --#
decl 16 (%R12)                            #-- incr ref count on VVal example.lat:17:24 (Ref  (Cl  (IRTargetRefName "Node"))) (IRValueName "%v_t_28") at example.lat:17:24 --#
movl %EBX, %EAX                           #-- move return value at example.lat:16:13 --#
leave                                     #-- example.lat:16:13 --#
pop %R12                                  #-- example.lat:16:13 --#
pop %RBX                                  #-- example.lat:16:13 --#
ret                                       #-- example.lat:16:13 --#
Node.markAsVisited :                      #-- example.lat:13:13 --#
pushq %RBX                                #-- example.lat:13:13 --#
pushq %R12                                #-- example.lat:13:13 --#
pushq %RBP                                #-- example.lat:13:13 --#
movq %RSP, %RBP                           #-- example.lat:13:13 --#
Node.markAsVisited.L_entry :              #-- example.lat:13:13 --#
movq %RDI, %RBX                           #-- load %v_t_26 at example.lat:13:13 --#
movl $1, 36 (%RBX)                        #-- example.lat:14:17 --#
call run_gc                               #-- example.lat:14:17 --#
decl 16 (%RBX)                            #-- incr ref count on VVal example.lat:14:17 (Ref  (Cl  (IRTargetRefName "Node"))) (IRValueName "%v_t_26") at example.lat:14:17 --#
movl %R12D, %EAX                          #-- move return value at example.lat:13:13 --#
leave                                     #-- example.lat:13:13 --#
pop %R12                                  #-- example.lat:13:13 --#
pop %RBX                                  #-- example.lat:13:13 --#
ret                                       #-- example.lat:13:13 --#
Node.isVisited :                          #-- example.lat:10:13 --#
pushq %RBX                                #-- example.lat:10:13 --#
pushq %R12                                #-- example.lat:10:13 --#
pushq %RBP                                #-- example.lat:10:13 --#
movq %RSP, %RBP                           #-- example.lat:10:13 --#
Node.isVisited.L_entry :                  #-- example.lat:10:13 --#
movq %RDI, %R12                           #-- load %v_t_24 at example.lat:10:13 --#
movb 36 (%R12), %BL                       #-- load %v_t_25 at example.lat:11:24 --#
call run_gc                               #-- example.lat:11:24 --#
decl 16 (%R12)                            #-- incr ref count on VVal example.lat:11:24 (Ref  (Cl  (IRTargetRefName "Node"))) (IRValueName "%v_t_24") at example.lat:11:24 --#
movb %BL, %AL                             #-- move return value at example.lat:10:13 --#
leave                                     #-- example.lat:10:13 --#
pop %R12                                  #-- example.lat:10:13 --#
pop %RBX                                  #-- example.lat:10:13 --#
ret                                       #-- example.lat:10:13 --#
Node.init :                               #-- example.lat:5:13 --#
pushq %RBX                                #-- example.lat:5:13 --#
pushq %R12                                #-- example.lat:5:13 --#
pushq %R13                                #-- example.lat:5:13 --#
subq $8, %RSP                             #-- example.lat:5:13 --#
pushq %RBP                                #-- example.lat:5:13 --#
movq %RSP, %RBP                           #-- example.lat:5:13 --#
Node.init.L_entry :                       #-- example.lat:5:13 --#
movq %RDI, %R12                           #-- load %v_t_20 at example.lat:5:13 --#
movl %ESI, %EBX                           #-- load %v_t_21 at example.lat:5:13 --#
movl $0, 36 (%R12)                        #-- example.lat:6:17 --#
call run_gc                               #-- example.lat:6:17 --#
decl 16 (%R12)                            #-- incr ref count on VVal example.lat:6:17 (Ref  (Cl  (IRTargetRefName "Node"))) (IRValueName "%v_t_20") at example.lat:6:17 --#
movl %EBX, 37 (%R12)                      #-- example.lat:7:17 --#
call run_gc                               #-- example.lat:7:17 --#
decl 16 (%R12)                            #-- incr ref count on VVal example.lat:7:17 (Ref  (Cl  (IRTargetRefName "Node"))) (IRValueName "%v_t_20") at example.lat:7:17 --#
movq 41 (%R12), %RBX                      #-- load %v_c0 at example.lat:8:17 --#
call run_gc                               #-- example.lat:8:17 --#
decl 16 (%RBX)                            #-- incr ref count on VVal example.lat:8:17 (Ref example.lat:4:13 (Cl example.lat:4:13 (IRTargetRefName "List"))) (IRValueName "%v_c0") at example.lat:8:17 --#
movq _LAT_NULL_ADDR (%RIP), 41 (%R12)     #-- example.lat:8:17 --#
call run_gc                               #-- example.lat:8:17 --#
decl 16 (%R12)                            #-- incr ref count on VVal example.lat:8:17 (Ref  (Cl  (IRTargetRefName "Node"))) (IRValueName "%v_t_20") at example.lat:8:17 --#
movl %R13D, %EAX                          #-- move return value at example.lat:5:13 --#
leave                                     #-- example.lat:5:13 --#
addq $8, %RSP                             #-- example.lat:5:13 --#
pop %R13                                  #-- example.lat:5:13 --#
pop %R12                                  #-- example.lat:5:13 --#
pop %RBX                                  #-- example.lat:5:13 --#
ret                                       #-- example.lat:5:13 --#
__handleErrorNull :                       #-- runtime error on null dereference at example.lat:1:1 --#
andq $-16, %RSP                           #-- 16 bytes allign at example.lat:1:1 --#
call __errorNull                          #-- example.lat:1:1 --#
