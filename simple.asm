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
 .global _class_Array 
 _class_Array : 
 .quad _class_Object 
 .long $16 
 .quad _class_Array_methods 
 .long $0 
 .quad $0 
 
 .global _class_Array_methods 
 _class_Array_methods : 
 .quad _Object_equals 
 .quad _Object_getHashCode 
 .quad _Array_toString 
 
 .global _class_Object 
 _class_Object : 
 .quad 0 
 .long $0 
 .quad _class_Object_methods 
 .long $0 
 .quad $0 
 
 .global _class_Object_methods 
 _class_Object_methods : 
 .quad _Object_equals 
 .quad _Object_getHashCode 
 .quad _Object_toString 
 
 .global _class_String 
 _class_String : 
 .quad _class_Object 
 .long $0 
 .quad _class_String_methods 
 .long $0 
 .quad $0 
 
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
 
 
 .section .text 
 __cl_TopLevel.main : 
 push %RBP 
 movq %RSP, %RBP 
 subq 0, %RSP 
 __cl_TopLevel.main.L_entry : 
 movl 4, %EDI 
 subq 0, %RSP 
 CALL __cl_TopLevel.foo 
 addq 0, %RSP 
 movl %EAX, %EAX 
 movl %EAX, %EDI 
 subq 0, %RSP 
 CALL printInt 
 addq 0, %RSP 
 movl 0, %EAX 
 addq 0, %RSP 
 leave 
 ret 
 __cl_TopLevel.foo : 
 push %RBP 
 movq %RSP, %RBP 
 subq 0, %RSP 
 __cl_TopLevel.foo.L_entry : 
 movl %EDI, %EAX 
 addl 2, %EAX 
 movl %EAX, %EAX 
 addq 0, %RSP 
 leave 
 ret 
 __errorNull : 
 andq -16, %RSP 
 CALL __errorNull 
