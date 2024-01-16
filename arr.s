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
 # Class metadata:
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
 # Class Object:
 #   Fields:
 # Class String:
 #   Fields:
 # Class ~cl_TopLevel:
 #   Fields:

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


main:
 # Register allocation:
 # [(ValIdent "%v___temp_0",rdx),(ValIdent "%v_t_1",rax),(ValIdent "%v_t_6",rax),(ValIdent "~arg_0_rdi",rdi)]

__cl_TopLevel.main:
  push %rbx
  subq $8, %rsp # 16 bytes alignment
  push %rbp
  movq %rsp, %rbp
  subq $0, %rsp # space for locals
__cl_TopLevel.main.L_entry:
  movl $2, %edi # passing arg
  subq $0, %rsp # 16 bytes alignment
  call __newIntArray
  addq $0, %rsp
  movq %rax, %rbx
  movq 8(%rbx), %rdi # load data (indirect)
  movl 8(%rdi), %eax # load %v_t_4
  movl %eax, %edi # passing arg
  subq $0, %rsp # 16 bytes alignment
  call printInt
  addq $0, %rsp
  movq 8(%rbx), %rdi # load data (indirect)
  movl 12(%rdi), %eax # load %v_t_7
  movl %eax, %edi # passing arg
  subq $0, %rsp # 16 bytes alignment
  call printInt
  addq $0, %rsp
  movq 8(%rbx), %rdi # load data (indirect)
  movl $2, 12(%rdi)
  movq 8(%rbx), %rdi # load data (indirect)
  movl 8(%rdi), %eax # load %v_t_12
  movl %eax, %edi # passing arg
  subq $0, %rsp # 16 bytes alignment
  call printInt
  addq $0, %rsp
  movq 8(%rbx), %rdi # load data (indirect)
  movl 12(%rdi), %eax # load %v_t_15
  movl %eax, %edi # passing arg
  subq $0, %rsp # 16 bytes alignment
  call printInt
  addq $0, %rsp
  movq 8(%rbx), %rdi # load data (indirect)
  movl 12(%rdi), %eax # load %v_t_19
  imull $5, %eax
  movq 8(%rbx), %rdi # load data (indirect)
  movl %eax, 8(%rdi)
  movq 8(%rbx), %rdi # load data (indirect)
  movl 8(%rdi), %eax # load %v_t_23
  movl %eax, %edi # passing arg
  subq $0, %rsp # 16 bytes alignment
  call printInt
  addq $0, %rsp
  movq 8(%rbx), %rdi # load data (indirect)
  movl 12(%rdi), %eax # load %v_t_26
  movl %eax, %edi # passing arg
  subq $0, %rsp # 16 bytes alignment
  call printInt
  addq $0, %rsp
  movl $0, %eax # move return value
  addq $0, %rsp
  leave
  addq $8, %rsp
  pop %rbx
  ret

