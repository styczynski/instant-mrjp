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
.string ""

__const_3 :
.string "/* world"

__const_1 :
.string "="

__const_2 :
.string "hello */"

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
__cl_TopLevel.ifac2f :            #-- ./play.lat:70:1 --#
pushq %R14                        #-- ./play.lat:70:1 --#
pushq %R13                        #-- ./play.lat:70:1 --#
pushq %R12                        #-- ./play.lat:70:1 --#
pushq %RBX                        #-- ./play.lat:70:1 --#
pushq %RBP                        #-- ./play.lat:70:1 --#
movq %RSP, %RBP                   #-- ./play.lat:70:1 --#
subq $0, %RSP                     #-- ./play.lat:70:1 --#
__cl_TopLevel.ifac2f.L_entry :    #-- ./play.lat:70:1 --#
movl %EDI, %R13D                  #-- load %v_t_107 at ./play.lat:70:1 --#
movl %ESI, %R12D                  #-- load %v_t_108 at ./play.lat:70:1 --#
cmpl %R12D, %R13D                 #-- ./play.lat:71:13 --#
sete %CL                          #-- ./play.lat:71:15 --#
addq $0, %RSP                     #-- ./play.lat:71:13 --#
testb %CL, %CL                    #-- ./play.lat:71:13 --#
jz __cl_TopLevel.ifac2f_IELSE110  #-- ./play.lat:71:13 --#
jmp __cl_TopLevel.ifac2f_IIF109   #-- ./play.lat:71:13 --#
__cl_TopLevel.ifac2f_IELSE110 :   #-- ./play.lat:71:9 --#
cmpl %R12D, %R13D                 #-- ./play.lat:73:13 --#
setg %CL                          #-- ./play.lat:73:15 --#
addq $0, %RSP                     #-- ./play.lat:73:13 --#
testb %CL, %CL                    #-- ./play.lat:73:13 --#
jz __cl_TopLevel.ifac2f_IELSE113  #-- ./play.lat:73:13 --#
jmp __cl_TopLevel.ifac2f_IIF112   #-- ./play.lat:73:13 --#
__cl_TopLevel.ifac2f_IELSE113 :   #-- ./play.lat:73:9 --#
leal 0 (%R13, %R12, 1), %EAX      #-- addition %v_t_118 at ./play.lat:76:16 --#
movl %EAX, %R14D                  #-- ./play.lat:76:21 --#
sarl $1, %R14D                    #-- divide by 2 at ./play.lat:76:21 --#
movl %R13D, %EDI                  #-- passing arg at ./play.lat:77:16 --#
movl %R14D, %ESI                  #-- passing arg at ./play.lat:77:16 --#
subq $0, %RSP                     #-- ./play.lat:77:16 --#
call __cl_TopLevel.ifac2f         #-- ./play.lat:77:16 --#
addq $0, %RSP                     #-- ./play.lat:77:16 --#
movl %EAX, %EBX                   #-- ./play.lat:77:16 --#
leal 1 (%R14), %EAX               #--  addition %v_t_123 at ./play.lat:77:38 --#
movl %EAX, %EDI                   #-- passing arg at ./play.lat:77:30 --#
movl %R12D, %ESI                  #-- passing arg at ./play.lat:77:30 --#
subq $0, %RSP                     #-- ./play.lat:77:30 --#
call __cl_TopLevel.ifac2f         #-- ./play.lat:77:30 --#
addq $0, %RSP                     #-- ./play.lat:77:30 --#
movl %EAX, %EAX                   #-- ./play.lat:77:30 --#
imull %EBX, %EAX                  #-- ./play.lat:77:28 --#
xchgl %EAX, %EAX                  #-- ./play.lat:70:1 --#
addq $0, %RSP                     #-- ./play.lat:70:1 --#
jmp __cl_TopLevel.ifac2f.L_exit   #-- ./play.lat:70:1 --#
__cl_TopLevel.ifac2f_IIF109 :     #-- ./play.lat:71:9 --#
xchgl %EAX, %R13D                 #-- ./play.lat:70:1 --#
addq $0, %RSP                     #-- ./play.lat:70:1 --#
jmp __cl_TopLevel.ifac2f.L_exit   #-- ./play.lat:70:1 --#
__cl_TopLevel.ifac2f_IIF112 :     #-- ./play.lat:73:9 --#
movl $1, %EAX                     #-- setting %v_return~2 at ./play.lat:70:1 --#
addq $0, %RSP                     #-- ./play.lat:70:1 --#
jmp __cl_TopLevel.ifac2f.L_exit   #-- ./play.lat:70:1 --#
__cl_TopLevel.ifac2f.L_exit :     #-- ./play.lat:70:1 --#
movl %EAX, %EAX                   #-- move return value at ./play.lat:70:1 --#
addq $0, %RSP                     #-- ./play.lat:70:1 --#
leave                             #-- ./play.lat:70:1 --#
pop %R14                          #-- ./play.lat:70:1 --#
pop %R13                          #-- ./play.lat:70:1 --#
pop %R12                          #-- ./play.lat:70:1 --#
pop %RBX                          #-- ./play.lat:70:1 --#
ret                               #-- ./play.lat:70:1 --#
__cl_TopLevel.ifac :              #-- ./play.lat:68:1 --#
pushq %RBP                        #-- ./play.lat:68:1 --#
movq %RSP, %RBP                   #-- ./play.lat:68:1 --#
subq $0, %RSP                     #-- ./play.lat:68:1 --#
__cl_TopLevel.ifac.L_entry :      #-- ./play.lat:68:1 --#
movl %EDI, %EAX                   #-- load %v_t_104 at ./play.lat:68:1 --#
movl $1, %EDI                     #-- passing arg at ./play.lat:68:26 --#
movl %EAX, %ESI                   #-- passing arg at ./play.lat:68:26 --#
subq $0, %RSP                     #-- ./play.lat:68:26 --#
call __cl_TopLevel.ifac2f         #-- ./play.lat:68:26 --#
addq $0, %RSP                     #-- ./play.lat:68:26 --#
movl %EAX, %EAX                   #-- ./play.lat:68:26 --#
movl %EAX, %EAX                   #-- move return value at ./play.lat:68:1 --#
addq $0, %RSP                     #-- ./play.lat:68:1 --#
leave                             #-- ./play.lat:68:1 --#
ret                               #-- ./play.lat:68:1 --#
__cl_TopLevel.nfac :              #-- ./play.lat:61:1 --#
pushq %RBX                        #-- ./play.lat:61:1 --#
subq $8, %RSP                     #-- ./play.lat:61:1 --#
pushq %RBP                        #-- ./play.lat:61:1 --#
movq %RSP, %RBP                   #-- ./play.lat:61:1 --#
subq $0, %RSP                     #-- ./play.lat:61:1 --#
__cl_TopLevel.nfac.L_entry :      #-- ./play.lat:61:1 --#
movl %EDI, %EBX                   #-- load %v_t_94 at ./play.lat:61:1 --#
cmpl $0, %EBX                     #-- ./play.lat:62:13 --#
setne %CL                         #-- ./play.lat:62:15 --#
addq $0, %RSP                     #-- ./play.lat:62:13 --#
testb %CL, %CL                    #-- ./play.lat:62:13 --#
jz __cl_TopLevel.nfac_IELSE96     #-- ./play.lat:62:13 --#
jmp __cl_TopLevel.nfac_IIF95      #-- ./play.lat:62:13 --#
__cl_TopLevel.nfac_IELSE96 :      #-- ./play.lat:62:9 --#
movl $1, %EAX                     #-- setting %v_return~2 at ./play.lat:61:1 --#
addq $0, %RSP                     #-- ./play.lat:61:1 --#
jmp __cl_TopLevel.nfac.L_exit     #-- ./play.lat:61:1 --#
__cl_TopLevel.nfac_IIF95 :        #-- ./play.lat:62:9 --#
leal -1 (%RBX), %EAX              #--  addition %v_t_100 at ./play.lat:63:24 --#
movl %EAX, %EDI                   #-- passing arg at ./play.lat:63:18 --#
subq $0, %RSP                     #-- ./play.lat:63:18 --#
call __cl_TopLevel.mfac           #-- ./play.lat:63:18 --#
addq $0, %RSP                     #-- ./play.lat:63:18 --#
movl %EAX, %EAX                   #-- ./play.lat:63:18 --#
imull %EBX, %EAX                  #-- ./play.lat:63:28 --#
xchgl %EAX, %EAX                  #-- ./play.lat:61:1 --#
addq $0, %RSP                     #-- ./play.lat:61:1 --#
jmp __cl_TopLevel.nfac.L_exit     #-- ./play.lat:61:1 --#
__cl_TopLevel.nfac.L_exit :       #-- ./play.lat:61:1 --#
movl %EAX, %EAX                   #-- move return value at ./play.lat:61:1 --#
addq $0, %RSP                     #-- ./play.lat:61:1 --#
leave                             #-- ./play.lat:61:1 --#
addq $8, %RSP                     #-- ./play.lat:61:1 --#
pop %RBX                          #-- ./play.lat:61:1 --#
ret                               #-- ./play.lat:61:1 --#
__cl_TopLevel.mfac :              #-- ./play.lat:54:1 --#
pushq %RBX                        #-- ./play.lat:54:1 --#
subq $8, %RSP                     #-- ./play.lat:54:1 --#
pushq %RBP                        #-- ./play.lat:54:1 --#
movq %RSP, %RBP                   #-- ./play.lat:54:1 --#
subq $0, %RSP                     #-- ./play.lat:54:1 --#
__cl_TopLevel.mfac.L_entry :      #-- ./play.lat:54:1 --#
movl %EDI, %EBX                   #-- load %v_t_84 at ./play.lat:54:1 --#
cmpl $0, %EBX                     #-- ./play.lat:55:13 --#
sete %CL                          #-- ./play.lat:55:15 --#
addq $0, %RSP                     #-- ./play.lat:55:13 --#
testb %CL, %CL                    #-- ./play.lat:55:13 --#
jz __cl_TopLevel.mfac_IELSE86     #-- ./play.lat:55:13 --#
jmp __cl_TopLevel.mfac_IIF85      #-- ./play.lat:55:13 --#
__cl_TopLevel.mfac_IELSE86 :      #-- ./play.lat:55:9 --#
leal -1 (%RBX), %EAX              #--  addition %v_t_91 at ./play.lat:58:28 --#
movl %EAX, %EDI                   #-- passing arg at ./play.lat:58:22 --#
subq $0, %RSP                     #-- ./play.lat:58:22 --#
call __cl_TopLevel.nfac           #-- ./play.lat:58:22 --#
addq $0, %RSP                     #-- ./play.lat:58:22 --#
movl %EAX, %EAX                   #-- ./play.lat:58:22 --#
imull %EBX, %EAX                  #-- ./play.lat:58:20 --#
xchgl %EAX, %EAX                  #-- ./play.lat:54:1 --#
addq $0, %RSP                     #-- ./play.lat:54:1 --#
jmp __cl_TopLevel.mfac.L_exit     #-- ./play.lat:54:1 --#
__cl_TopLevel.mfac_IIF85 :        #-- ./play.lat:55:9 --#
movl $1, %EAX                     #-- setting %v_return~2 at ./play.lat:54:1 --#
addq $0, %RSP                     #-- ./play.lat:54:1 --#
jmp __cl_TopLevel.mfac.L_exit     #-- ./play.lat:54:1 --#
__cl_TopLevel.mfac.L_exit :       #-- ./play.lat:54:1 --#
movl %EAX, %EAX                   #-- move return value at ./play.lat:54:1 --#
addq $0, %RSP                     #-- ./play.lat:54:1 --#
leave                             #-- ./play.lat:54:1 --#
addq $8, %RSP                     #-- ./play.lat:54:1 --#
pop %RBX                          #-- ./play.lat:54:1 --#
ret                               #-- ./play.lat:54:1 --#
__cl_TopLevel.rfac :              #-- ./play.lat:47:1 --#
pushq %RBX                        #-- ./play.lat:47:1 --#
subq $8, %RSP                     #-- ./play.lat:47:1 --#
pushq %RBP                        #-- ./play.lat:47:1 --#
movq %RSP, %RBP                   #-- ./play.lat:47:1 --#
subq $0, %RSP                     #-- ./play.lat:47:1 --#
__cl_TopLevel.rfac.L_entry :      #-- ./play.lat:47:1 --#
movl %EDI, %EBX                   #-- load %v_t_74 at ./play.lat:47:1 --#
cmpl $0, %EBX                     #-- ./play.lat:48:13 --#
sete %CL                          #-- ./play.lat:48:15 --#
addq $0, %RSP                     #-- ./play.lat:48:13 --#
testb %CL, %CL                    #-- ./play.lat:48:13 --#
jz __cl_TopLevel.rfac_IELSE76     #-- ./play.lat:48:13 --#
jmp __cl_TopLevel.rfac_IIF75      #-- ./play.lat:48:13 --#
__cl_TopLevel.rfac_IELSE76 :      #-- ./play.lat:48:9 --#
leal -1 (%RBX), %EAX              #--  addition %v_t_81 at ./play.lat:51:28 --#
movl %EAX, %EDI                   #-- passing arg at ./play.lat:51:22 --#
subq $0, %RSP                     #-- ./play.lat:51:22 --#
call __cl_TopLevel.rfac           #-- ./play.lat:51:22 --#
addq $0, %RSP                     #-- ./play.lat:51:22 --#
movl %EAX, %EAX                   #-- ./play.lat:51:22 --#
imull %EBX, %EAX                  #-- ./play.lat:51:20 --#
xchgl %EAX, %EAX                  #-- ./play.lat:47:1 --#
addq $0, %RSP                     #-- ./play.lat:47:1 --#
jmp __cl_TopLevel.rfac.L_exit     #-- ./play.lat:47:1 --#
__cl_TopLevel.rfac_IIF75 :        #-- ./play.lat:48:9 --#
movl $1, %EAX                     #-- setting %v_return~2 at ./play.lat:47:1 --#
addq $0, %RSP                     #-- ./play.lat:47:1 --#
jmp __cl_TopLevel.rfac.L_exit     #-- ./play.lat:47:1 --#
__cl_TopLevel.rfac.L_exit :       #-- ./play.lat:47:1 --#
movl %EAX, %EAX                   #-- move return value at ./play.lat:47:1 --#
addq $0, %RSP                     #-- ./play.lat:47:1 --#
leave                             #-- ./play.lat:47:1 --#
addq $8, %RSP                     #-- ./play.lat:47:1 --#
pop %RBX                          #-- ./play.lat:47:1 --#
ret                               #-- ./play.lat:47:1 --#
__cl_TopLevel.fac :               #-- ./play.lat:34:1 --#
pushq %RBP                        #-- ./play.lat:34:1 --#
movq %RSP, %RBP                   #-- ./play.lat:34:1 --#
subq $0, %RSP                     #-- ./play.lat:34:1 --#
__cl_TopLevel.fac.L_entry :       #-- ./play.lat:34:1 --#
movl %EDI, %EDX                   #-- load %v_t_61 at ./play.lat:34:1 --#
xchgl %EAX, %EDX                  #-- ./play.lat:40:9 --#
movl $1, %ECX                     #-- setting %v_t_63~3 at ./play.lat:40:9 --#
addq $0, %RSP                     #-- ./play.lat:40:9 --#
jmp __cl_TopLevel.fac_WCOND67     #-- ./play.lat:40:9 --#
__cl_TopLevel.fac_WBEG68 :        #-- ./play.lat:40:9 --#
imull %EAX, %ECX                  #-- ./play.lat:41:17 --#
addl $-1, %EAX                    #-- ./play.lat:42:17 --#
xchgl %ECX, %ECX                  #-- ./play.lat:40:9 --#
xchgl %EAX, %EAX                  #-- ./play.lat:40:9 --#
addq $0, %RSP                     #-- ./play.lat:40:9 --#
jmp __cl_TopLevel.fac_WCOND67     #-- ./play.lat:40:9 --#
__cl_TopLevel.fac_WCOND67 :       #-- ./play.lat:40:9 --#
cmpl $0, %EAX                     #-- ./play.lat:40:16 --#
setg %DIL                         #-- ./play.lat:40:18 --#
addq $0, %RSP                     #-- ./play.lat:40:16 --#
testb %DIL, %DIL                  #-- ./play.lat:40:16 --#
jz __cl_TopLevel.fac_WEND69       #-- ./play.lat:40:16 --#
jmp __cl_TopLevel.fac_WBEG68      #-- ./play.lat:40:16 --#
__cl_TopLevel.fac_WEND69 :        #-- ./play.lat:40:9 --#
movl %ECX, %EAX                   #-- move return value at ./play.lat:34:1 --#
addq $0, %RSP                     #-- ./play.lat:34:1 --#
leave                             #-- ./play.lat:34:1 --#
ret                               #-- ./play.lat:34:1 --#
main :                            #-- ./play.lat:12:1 --#
__cl_TopLevel.main :              #-- ./play.lat:12:1 --#
pushq %R12                        #-- ./play.lat:12:1 --#
pushq %RBX                        #-- ./play.lat:12:1 --#
pushq %RBP                        #-- ./play.lat:12:1 --#
movq %RSP, %RBP                   #-- ./play.lat:12:1 --#
subq $0, %RSP                     #-- ./play.lat:12:1 --#
__cl_TopLevel.main.L_entry :      #-- ./play.lat:12:1 --#
movl $10, %EDI                    #-- passing arg at ./play.lat:13:18 --#
subq $0, %RSP                     #-- ./play.lat:13:18 --#
call __cl_TopLevel.fac            #-- ./play.lat:13:18 --#
addq $0, %RSP                     #-- ./play.lat:13:18 --#
movl %EAX, %EAX                   #-- ./play.lat:13:18 --#
movl %EAX, %EDI                   #-- passing arg at ./play.lat:13:9 --#
subq $0, %RSP                     #-- ./play.lat:13:9 --#
call printInt                     #-- ./play.lat:13:9 --#
addq $0, %RSP                     #-- ./play.lat:13:9 --#
movl $10, %EDI                    #-- passing arg at ./play.lat:14:18 --#
subq $0, %RSP                     #-- ./play.lat:14:18 --#
call __cl_TopLevel.rfac           #-- ./play.lat:14:18 --#
addq $0, %RSP                     #-- ./play.lat:14:18 --#
movl %EAX, %EAX                   #-- ./play.lat:14:18 --#
movl %EAX, %EDI                   #-- passing arg at ./play.lat:14:9 --#
subq $0, %RSP                     #-- ./play.lat:14:9 --#
call printInt                     #-- ./play.lat:14:9 --#
addq $0, %RSP                     #-- ./play.lat:14:9 --#
movl $10, %EDI                    #-- passing arg at ./play.lat:15:18 --#
subq $0, %RSP                     #-- ./play.lat:15:18 --#
call __cl_TopLevel.mfac           #-- ./play.lat:15:18 --#
addq $0, %RSP                     #-- ./play.lat:15:18 --#
movl %EAX, %EAX                   #-- ./play.lat:15:18 --#
movl %EAX, %EDI                   #-- passing arg at ./play.lat:15:9 --#
subq $0, %RSP                     #-- ./play.lat:15:9 --#
call printInt                     #-- ./play.lat:15:9 --#
addq $0, %RSP                     #-- ./play.lat:15:9 --#
movl $10, %EDI                    #-- passing arg at ./play.lat:16:18 --#
subq $0, %RSP                     #-- ./play.lat:16:18 --#
call __cl_TopLevel.ifac           #-- ./play.lat:16:18 --#
addq $0, %RSP                     #-- ./play.lat:16:18 --#
movl %EAX, %EAX                   #-- ./play.lat:16:18 --#
movl %EAX, %EDI                   #-- passing arg at ./play.lat:16:9 --#
subq $0, %RSP                     #-- ./play.lat:16:9 --#
call printInt                     #-- ./play.lat:16:9 --#
addq $0, %RSP                     #-- ./play.lat:16:9 --#
movl $1, %EAX                     #-- setting %v_t_39~2 at ./play.lat:21:11 --#
movl $10, %ECX                    #-- setting %v_t_37~2 at ./play.lat:21:11 --#
addq $0, %RSP                     #-- ./play.lat:21:11 --#
jmp __cl_TopLevel.main_WCOND40    #-- ./play.lat:21:11 --#
__cl_TopLevel.main_WBEG41 :       #-- ./play.lat:21:11 --#
movl %EAX, %R12D                  #-- ./play.lat:22:19 --#
imull %ECX, %R12D                 #-- ./play.lat:22:19 --#
movl %ECX, %EBX                   #-- ./play.lat:23:13 --#
subl $1, %EBX                     #-- ./play.lat:23:13 --#
xchgl %ECX, %EBX                  #-- ./play.lat:21:11 --#
xchgl %EAX, %R12D                 #-- ./play.lat:21:11 --#
addq $0, %RSP                     #-- ./play.lat:21:11 --#
jmp __cl_TopLevel.main_WCOND40    #-- ./play.lat:21:11 --#
__cl_TopLevel.main_WCOND40 :      #-- ./play.lat:21:11 --#
cmpl $0, %ECX                     #-- ./play.lat:21:18 --#
setg %DL                          #-- ./play.lat:21:19 --#
addq $0, %RSP                     #-- ./play.lat:21:18 --#
testb %DL, %DL                    #-- ./play.lat:21:18 --#
jz __cl_TopLevel.main_WEND42      #-- ./play.lat:21:18 --#
jmp __cl_TopLevel.main_WBEG41     #-- ./play.lat:21:18 --#
__cl_TopLevel.main_WEND42 :       #-- ./play.lat:21:11 --#
movl %EAX, %EDI                   #-- passing arg at ./play.lat:25:11 --#
subq $0, %RSP                     #-- ./play.lat:25:11 --#
call printInt                     #-- ./play.lat:25:11 --#
addq $0, %RSP                     #-- ./play.lat:25:11 --#
leaq __const_1 (%RIP), %RAX       #-- ./play.lat:27:22 --#
movq %RAX, %RDI                   #-- passing arg at ./play.lat:27:22 --#
subq $0, %RSP                     #-- ./play.lat:27:22 --#
call __createString               #-- ./play.lat:27:22 --#
addq $0, %RSP                     #-- ./play.lat:27:22 --#
movq %RAX, %RAX                   #-- ./play.lat:27:22 --#
movq %RAX, %RDI                   #-- passing arg at ./play.lat:27:15 --#
movl $60, %ESI                    #-- passing arg at ./play.lat:27:15 --#
subq $0, %RSP                     #-- ./play.lat:27:15 --#
call __cl_TopLevel.repStr         #-- ./play.lat:27:15 --#
addq $0, %RSP                     #-- ./play.lat:27:15 --#
movq %RAX, %RAX                   #-- ./play.lat:27:15 --#
movq %RAX, %RDI                   #-- passing arg at ./play.lat:28:9 --#
subq $0, %RSP                     #-- ./play.lat:28:9 --#
call printString                  #-- ./play.lat:28:9 --#
addq $0, %RSP                     #-- ./play.lat:28:9 --#
leaq __const_2 (%RIP), %RAX       #-- ./play.lat:29:22 --#
movq %RAX, %RDI                   #-- passing arg at ./play.lat:29:22 --#
subq $0, %RSP                     #-- ./play.lat:29:22 --#
call __createString               #-- ./play.lat:29:22 --#
addq $0, %RSP                     #-- ./play.lat:29:22 --#
movq %RAX, %RAX                   #-- ./play.lat:29:22 --#
movq %RAX, %RDI                   #-- passing arg at ./play.lat:29:9 --#
subq $0, %RSP                     #-- ./play.lat:29:9 --#
call printString                  #-- ./play.lat:29:9 --#
addq $0, %RSP                     #-- ./play.lat:29:9 --#
leaq __const_3 (%RIP), %RAX       #-- ./play.lat:30:22 --#
movq %RAX, %RDI                   #-- passing arg at ./play.lat:30:22 --#
subq $0, %RSP                     #-- ./play.lat:30:22 --#
call __createString               #-- ./play.lat:30:22 --#
addq $0, %RSP                     #-- ./play.lat:30:22 --#
movq %RAX, %RAX                   #-- ./play.lat:30:22 --#
movq %RAX, %RDI                   #-- passing arg at ./play.lat:30:9 --#
subq $0, %RSP                     #-- ./play.lat:30:9 --#
call printString                  #-- ./play.lat:30:9 --#
addq $0, %RSP                     #-- ./play.lat:30:9 --#
movl $0, %EAX                     #-- move return value at ./play.lat:12:1 --#
addq $0, %RSP                     #-- ./play.lat:12:1 --#
leave                             #-- ./play.lat:12:1 --#
pop %R12                          #-- ./play.lat:12:1 --#
pop %RBX                          #-- ./play.lat:12:1 --#
ret                               #-- ./play.lat:12:1 --#
__cl_TopLevel.repStr :            #-- ./play.lat:2:1 --#
pushq %R14                        #-- ./play.lat:2:1 --#
pushq %R13                        #-- ./play.lat:2:1 --#
pushq %R12                        #-- ./play.lat:2:1 --#
pushq %RBX                        #-- ./play.lat:2:1 --#
pushq %RBP                        #-- ./play.lat:2:1 --#
movq %RSP, %RBP                   #-- ./play.lat:2:1 --#
subq $0, %RSP                     #-- ./play.lat:2:1 --#
__cl_TopLevel.repStr.L_entry :    #-- ./play.lat:2:1 --#
movq %RDI, %RBX                   #-- load %v_t_9 at ./play.lat:2:1 --#
movl %ESI, %R14D                  #-- load %v_t_10 at ./play.lat:2:1 --#
leaq __const_4 (%RIP), %R13       #-- ./play.lat:3:14 --#
movq %R13, %RDI                   #-- passing arg at ./play.lat:3:14 --#
subq $0, %RSP                     #-- ./play.lat:3:14 --#
call __createString               #-- ./play.lat:3:14 --#
addq $0, %RSP                     #-- ./play.lat:3:14 --#
movq %RAX, %R13                   #-- ./play.lat:3:14 --#
xchgq %RAX, %R13                  #-- ./play.lat:5:3 --#
movl $0, %R12D                    #-- setting %v_t_15~2 at ./play.lat:5:3 --#
addq $0, %RSP                     #-- ./play.lat:5:3 --#
jmp __cl_TopLevel.repStr_WCOND16  #-- ./play.lat:5:3 --#
__cl_TopLevel.repStr_WBEG17 :     #-- ./play.lat:5:3 --#
movq %RAX, %RDI                   #-- passing arg at ./play.lat:6:9 --#
movq %RBX, %RSI                   #-- passing arg at ./play.lat:6:9 --#
subq $0, %RSP                     #-- ./play.lat:6:9 --#
testq %RDI, %RDI                  #-- ./play.lat:6:9 --#
jz __errorNull                    #-- ./play.lat:6:9 --#
movq 20 (%RDI), %RAX              #-- load address of vtable at ./play.lat:6:9 --#
call * 16 (%RAX)                  #-- call concat at ./play.lat:6:9 --#
addq $0, %RSP                     #-- ./play.lat:6:9 --#
movq %RAX, %RCX                   #-- ./play.lat:6:9 --#
leal 1 (%R12), %EAX               #--  addition %v_t_21 at ./play.lat:7:5 --#
xchgl %R12D, %EAX                 #-- ./play.lat:5:3 --#
xchgq %RAX, %RCX                  #-- ./play.lat:5:3 --#
addq $0, %RSP                     #-- ./play.lat:5:3 --#
jmp __cl_TopLevel.repStr_WCOND16  #-- ./play.lat:5:3 --#
__cl_TopLevel.repStr_WCOND16 :    #-- ./play.lat:5:3 --#
cmpl %R14D, %R12D                 #-- ./play.lat:5:9 --#
setl %CL                          #-- ./play.lat:5:10 --#
addq $0, %RSP                     #-- ./play.lat:5:9 --#
testb %CL, %CL                    #-- ./play.lat:5:9 --#
jz __cl_TopLevel.repStr_WEND18    #-- ./play.lat:5:9 --#
jmp __cl_TopLevel.repStr_WBEG17   #-- ./play.lat:5:9 --#
__cl_TopLevel.repStr_WEND18 :     #-- ./play.lat:5:3 --#
movq %RAX, %RAX                   #-- move return value at ./play.lat:2:1 --#
addq $0, %RSP                     #-- ./play.lat:2:1 --#
leave                             #-- ./play.lat:2:1 --#
pop %R14                          #-- ./play.lat:2:1 --#
pop %R13                          #-- ./play.lat:2:1 --#
pop %R12                          #-- ./play.lat:2:1 --#
pop %RBX                          #-- ./play.lat:2:1 --#
ret                               #-- ./play.lat:2:1 --#
__errorNull :                     #-- runtime error on null dereference at ./play.lat:2:1 --#
andq $-16, %RSP                   #-- 16 bytes allign at ./play.lat:2:1 --#
call __errorNull                  #-- ./play.lat:2:1 --#
