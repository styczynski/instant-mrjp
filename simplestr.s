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
.global main

.global _class_Array
_class_Array:
.quad _class_Object
.long 16
.quad _class_Array_methods
.long 1
.quad _class_Array_refs
_class_Array_methods:
.quad _Object_equals
.quad _Object_getHashCode
.quad _Array_toString
_class_Array_refs:
.long 0
.global _class_Object
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

__const_2:
  .string "!!!\0"
__const_1:
  .string "ala ma kota\0"
 # Class metadata:
 # Class _class_Array:
 #   Fields:
 #     Field name:   elements
 #     Field type:   Ref () (Cl () (SymIdent "Object"))
 #     Field offset: 8
 #     Field size:   8
 #     Field name:   length
 #     Field type:   Int ()
 #     Field offset: 16
 #     Field size:   4
 #     Field name:   elementSize
 #     Field type:   Int ()
 #     Field offset: 20
 #     Field size:   4
 # Class _class_Object:
 #   Fields:
 # Class _class_String:
 #   Fields:
 # Class ~cl_TopLevel:
 #   Fields:

__vtable__class_Array:
__vtable__class_Object:
__vtable__class_String:

__nullref: # runtime error on null dereference
  andq $-16, %rsp # 16 bytes allign
  # call lat_nullref


 # Register allocation:
 # [(ValIdent "%a_t_0",rdi),(ValIdent "%v_t_0",rdx),(ValIdent "%v_t_2",rax),(ValIdent "%v_t_3",rax),(ValIdent "~arg_0_rdi",rdi),(ValIdent "~arg_1_rsi",rsi)]

__cl_TopLevel.foo:
  push %rbp
  movq %rsp, %rbp
  subq $0, %rsp # space for locals
__cl_TopLevel.foo.L_entry:
  movq %rdi, %rdx # load %v_t_0
  lea __const_2(%rip), %rax
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
  call _String_concat
  addq $0, %rsp
  movq %rax, %rax
  movq %rax, %rax # move return value
  addq $0, %rsp
  leave
  ret

 # Register allocation:
 # [(ValIdent "%a_t_4",rdi),(ValIdent "%v_t_4",rax),(ValIdent "%v_t_6",rax)]

__cl_TopLevel.bar:
  push %rbp
  movq %rsp, %rbp
  subq $0, %rsp # space for locals
__cl_TopLevel.bar.L_entry:
  movl %edi, %eax # load %v_t_4
  movl %eax, %eax
  sal $1, %eax # multiply by 2
  movl %eax, %eax # move return value
  addq $0, %rsp
  leave
  ret

main:
 # Register allocation:
 # [(ValIdent "%v_t_10",rax),(ValIdent "%v_t_11",rax),(ValIdent "~arg_0_rdi",rdi),(ValIdent "~arg_1_rdi",rdi)]

__cl_TopLevel.main:
  push %rbp
  movq %rsp, %rbp
  subq $0, %rsp # space for locals
__cl_TopLevel.main.L_entry:
  lea __const_1(%rip), %rax
  movq %rax, %rdi # passing arg
  subq $0, %rsp # 16 bytes alignment
  call __createString
  addq $0, %rsp
  movq %rax, %rax
  movq %rax, %rdi # passing arg
  subq $0, %rsp # 16 bytes alignment
  call __cl_TopLevel.foo
  addq $0, %rsp
  movq %rax, %rax
  movq %rax, %rdi # passing arg
  subq $0, %rsp # 16 bytes alignment
  call printString
  addq $0, %rsp
  movl $0, %eax # move return value
  addq $0, %rsp
  leave
  ret