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
.global main
__const_1:
  .string "!!!"
 # Class metadata:
 # Class A:
 #   Fields:
 #     Field name:   a
 #     Field type:   Int ()
 #     Field offset: 0
 #     Field size:   4
 # Class Array:
 #   Fields:
 #     Field name:   elements
 #     Field type:   Ref () (Cl () (SymIdent "Object"))
 #     Field offset: 0
 #     Field size:   8
 #     Field name:   length
 #     Field type:   Int ()
 #     Field offset: 8
 #     Field size:   4
 #     Field name:   elementSize
 #     Field type:   Int ()
 #     Field offset: 12
 #     Field size:   4
 # Class B:
 #   Fields:
 #     Field name:   a
 #     Field type:   Int ()
 #     Field offset: 0
 #     Field size:   4
 # Class Object:
 #   Fields:
 # Class String:
 #   Fields:
 # Class ~cl_TopLevel:
 #   Fields:

.global _class_A
.global _class_A_methods
_class_A:
  .quad _class_Object
  .long 8
  .quad _class_A_methods
  .long 0
  .quad 0
_class_A_methods:
  .quad _Object_equals
  .quad _Object_getHashCode
  .quad _Object_toString
  .quad A.bar
.global _class_Array
.global _class_Array_methods
_class_Array:
  .quad _class_Object
  .long 16
  .quad _class_Array_methods
  .long 0
  .quad 0
_class_Array_methods:
  .quad _Object_equals
  .quad _Object_getHashCode
  .quad _Array_toString
.global _class_B
.global _class_B_methods
_class_B:
  .quad _class_A
  .long 8
  .quad _class_B_methods
  .long 0
  .quad 0
_class_B_methods:
  .quad _Object_equals
  .quad _Object_getHashCode
  .quad _Object_toString
  .quad B.bar
.global _class_Object
.global _class_Object_methods
_class_Object:
  .quad 0
  .long 0
  .quad _class_Object_methods
  .long 0
  .quad 0
_class_Object_methods:
  .quad _Object_equals
  .quad _Object_getHashCode
  .quad _Object_toString
.global _class_String
.global _class_String_methods
_class_String:
  .quad _class_Object
  .long 0
  .quad _class_String_methods
  .long 0
  .quad 0
_class_String_methods:
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

__errorNull: # runtime error on null dereference
  andq $-16, %rsp # 16 bytes allign
  call __errorNull


 # Register allocation:
 # [(ValIdent "%a_t_1",rsi),(ValIdent "%v_t_1",rax),(ValIdent "%v_t_3",rax)]

A.bar:
  push %rbp
  movq %rsp, %rbp
  subq $0, %rsp # space for locals
A.bar.L_entry:
  movl %esi, %eax # load %v_t_1
  movl %eax, %eax
  sal $1, %eax # multiply by 2
  movl %eax, %eax # move return value
  addq $0, %rsp
  leave
  ret

 # Register allocation:
 # [(ValIdent "%a_t_5",rsi),(ValIdent "%v_t_5",rax),(ValIdent "%v_t_8",rax),(ValIdent "%v_t_9",rax)]

B.bar:
  push %rbp
  movq %rsp, %rbp
  subq $0, %rsp # space for locals
B.bar.L_entry:
  movl %esi, %eax # load %v_t_5
  movl %eax, %eax
  sal $1, %eax # multiply by 2
  addl $2, %eax
  movl %eax, %eax # move return value
  addq $0, %rsp
  leave
  ret

 # Register allocation:
 # [(ValIdent "%a_t_10",rdi),(ValIdent "%v_t_10",rdx),(ValIdent "%v_t_12",rax),(ValIdent "%v_t_13",rax),(ValIdent "~arg_0_rdi",rdi),(ValIdent "~arg_1_rsi",rsi)]

__cl_TopLevel.foo:
  push %rbp
  movq %rsp, %rbp
  subq $0, %rsp # space for locals
__cl_TopLevel.foo.L_entry:
  movq %rdi, %rdx # load %v_t_10
  lea __const_1(%rip), %rax
  push %rdx # save caller saved
  movq %rax, %rdi # passing arg
  subq $0, %rsp # 16 bytes alignment
  call __createString
  addq $0, %rsp
  movq %rax, %rax
  pop %rdx
  movq %rdx, %rdi # passing arg
  movq %rax, %rsi # passing arg
  subq $0, %rsp # 16 bytes alignment
  testq %rdi, %rdi
  jz __errorNull
  movq 0(%rdi), %rax # load address of vtable
  movq 12(%rax), %rdi # load address of vtable
  call *16(%rdi) # call concat
  addq $0, %rsp
  movq %rax, %rax
  movq %rax, %rax # move return value
  addq $0, %rsp
  leave
  ret

 # Register allocation:
 # [(ValIdent "%a_t_14",rdi),(ValIdent "%v_t_14",rax),(ValIdent "%v_t_16",rax)]

__cl_TopLevel.bar:
  push %rbp
  movq %rsp, %rbp
  subq $0, %rsp # space for locals
__cl_TopLevel.bar.L_entry:
  movl %edi, %eax # load %v_t_14
  movl %eax, %eax
  sal $1, %eax # multiply by 2
  movl %eax, %eax # move return value
  addq $0, %rsp
  leave
  ret

main:
 # Register allocation:
 # [(ValIdent "%v_t_17",r12),(ValIdent "%v_t_19",rax),(ValIdent "%v_t_20",rbx),(ValIdent "%v_t_27",rax),(ValIdent "%v_t_30",rax),(ValIdent "~arg_0_rdi",rdi),(ValIdent "~arg_1_rdi",rdi),(ValIdent "~arg_2_rsi",rsi),(ValIdent "~arg_3_rdi",rdi),(ValIdent "~arg_4_rdi",rdi),(ValIdent "~arg_5_rsi",rsi),(ValIdent "~arg_6_rdi",rdi)]

__cl_TopLevel.main:
  push %r12
  push %rbx
  push %rbp
  movq %rsp, %rbp
  subq $0, %rsp # space for locals
__cl_TopLevel.main.L_entry:
  lea _class_A(%rip), %rdi
  subq $0, %rsp # 16 bytes alignment
  call __new
  addq $0, %rsp
  movq %rax, %r12
  lea _class_B(%rip), %rdi
  subq $0, %rsp # 16 bytes alignment
  call __new
  addq $0, %rsp
  movq %rax, %rax
  movq %rax, %rdi # passing arg
  lea _class_A(%rip), %rsi
  subq $0, %rsp # 16 bytes alignment
  call __cast
  addq $0, %rsp
  movq %rax, %rbx
  movq 8(%r12), %rdi # load data (indirect)
  movl $42, 0(%rdi)
  movq 8(%rbx), %rdi # load data (indirect)
  movl $42, 0(%rdi)
  movq %r12, %rdi # passing arg
  movl $15, %esi # passing arg
  subq $0, %rsp # 16 bytes alignment
  testq %rdi, %rdi
  jz __errorNull
  movq 0(%rdi), %rax # load address of vtable
  movq 12(%rax), %rdi # load address of vtable
  call *24(%rdi) # call bar
  addq $0, %rsp
  movl %eax, %eax
  movl %eax, %edi # passing arg
  subq $0, %rsp # 16 bytes alignment
  call printInt
  addq $0, %rsp
  movq %rbx, %rdi # passing arg
  movl $15, %esi # passing arg
  subq $0, %rsp # 16 bytes alignment
  testq %rdi, %rdi
  jz __errorNull
  movq 0(%rdi), %rax # load address of vtable
  movq 12(%rax), %rdi # load address of vtable
  call *24(%rdi) # call bar
  addq $0, %rsp
  movl %eax, %eax
  movl %eax, %edi # passing arg
  subq $0, %rsp # 16 bytes alignment
  call printInt
  addq $0, %rsp
  movl $0, %eax # move return value
  addq $0, %rsp
  leave
  pop %rbx
  pop %r12
  ret