	.file	"runtime.c"
# GNU C17 (Debian 12.2.0-14) version 12.2.0 (x86_64-linux-gnu)
#	compiled by GNU C version 12.2.0, GMP version 6.2.1, MPFR version 4.1.1-p1, MPC version 1.3.1, isl version isl-0.25-GMP

# warning: MPFR header version 4.1.1-p1 differs from library version 4.2.0.
# GGC heuristics: --param ggc-min-expand=100 --param ggc-min-heapsize=131072
# options passed: -mtune=generic -march=x86-64 -O0 -fPIE -fasynchronous-unwind-tables
	.text
	.globl	errMsg
	.bss
	.align 8
	.type	errMsg, @object
	.size	errMsg, 8
errMsg:
	.zero	8
	.globl	emptyString
	.type	emptyString, @object
	.size	emptyString, 1
emptyString:
	.zero	1
	.text
	.globl	__new
	.type	__new, @function
__new:
.LFB6:
	.cfi_startproc
	pushq	%rbp	#
	.cfi_def_cfa_offset 16
	.cfi_offset 6, -16
	movq	%rsp, %rbp	#,
	.cfi_def_cfa_register 6
	subq	$32, %rsp	#,
	movq	%rdi, -24(%rbp)	# t, t
# src/Runtime/runtime.c:28:     obj r = malloc(sizeof(struct Reference)+10);
	movl	$38, %edi	#,
	call	malloc@PLT	#
	movq	%rax, -8(%rbp)	# tmp93, r
# src/Runtime/runtime.c:30:     r->type = t;
	movq	-8(%rbp), %rax	# r, tmp94
	movq	-24(%rbp), %rdx	# t, tmp95
	movq	%rdx, (%rax)	# tmp95, r_21->type
# src/Runtime/runtime.c:31:     r->counter = 0;
	movq	-8(%rbp), %rax	# r, tmp96
	movl	$0, 16(%rax)	#, r_21->counter
# src/Runtime/runtime.c:33:     if (t->dataSize > 0 && t != &_class_Array && t != &_class_String) {
	movq	-24(%rbp), %rax	# t, tmp97
	movl	8(%rax), %eax	# t_22(D)->dataSize, _1
# src/Runtime/runtime.c:33:     if (t->dataSize > 0 && t != &_class_Array && t != &_class_String) {
	testl	%eax, %eax	# _1
	jle	.L2	#,
# src/Runtime/runtime.c:33:     if (t->dataSize > 0 && t != &_class_Array && t != &_class_String) {
	leaq	_class_Array(%rip), %rax	#, tmp98
	cmpq	%rax, -24(%rbp)	# tmp98, t
	je	.L2	#,
# src/Runtime/runtime.c:33:     if (t->dataSize > 0 && t != &_class_Array && t != &_class_String) {
	leaq	_class_String(%rip), %rax	#, tmp99
	cmpq	%rax, -24(%rbp)	# tmp99, t
	je	.L2	#,
# src/Runtime/runtime.c:35:         r->data = malloc(t->dataSize);
	movq	-24(%rbp), %rax	# t, tmp100
	movl	8(%rax), %eax	# t_22(D)->dataSize, _6
# src/Runtime/runtime.c:35:         r->data = malloc(t->dataSize);
	cltq
	movq	%rax, %rdi	# _7,
	call	malloc@PLT	#
	movq	%rax, %rdx	# tmp101, _8
# src/Runtime/runtime.c:35:         r->data = malloc(t->dataSize);
	movq	-8(%rbp), %rax	# r, tmp102
	movq	%rdx, 8(%rax)	# _8, r_21->data
# src/Runtime/runtime.c:36:         bzero(r->data, t->dataSize);
	movq	-24(%rbp), %rax	# t, tmp103
	movl	8(%rax), %eax	# t_22(D)->dataSize, _9
# src/Runtime/runtime.c:36:         bzero(r->data, t->dataSize);
	movslq	%eax, %rdx	# _9, _10
	movq	-8(%rbp), %rax	# r, tmp104
	movq	8(%rax), %rax	# r_21->data, _11
	movl	$0, %esi	#,
	movq	%rax, %rdi	# tmp105,
	call	memset@PLT	#
	jmp	.L3	#
.L2:
# src/Runtime/runtime.c:39:         r->data = NULL;
	movq	-8(%rbp), %rax	# r, tmp109
	movq	$0, 8(%rax)	#, r_21->data
.L3:
# src/Runtime/runtime.c:41:     r->methods = r->type->methods;
	movq	-8(%rbp), %rax	# r, tmp110
	movq	(%rax), %rax	# r_21->type, _16
# src/Runtime/runtime.c:41:     r->methods = r->type->methods;
	movq	12(%rax), %rdx	# _16->methods, _17
# src/Runtime/runtime.c:41:     r->methods = r->type->methods;
	movq	-8(%rbp), %rax	# r, tmp111
	movq	%rdx, 20(%rax)	# _17, r_21->methods
# src/Runtime/runtime.c:43:     return r;
	movq	-8(%rbp), %rax	# r, _5
# src/Runtime/runtime.c:44: }
	leave	
	.cfi_def_cfa 7, 8
	ret	
	.cfi_endproc
.LFE6:
	.size	__new, .-__new
	.globl	__free
	.type	__free, @function
__free:
.LFB7:
	.cfi_startproc
	pushq	%rbp	#
	.cfi_def_cfa_offset 16
	.cfi_offset 6, -16
	movq	%rsp, %rbp	#,
	.cfi_def_cfa_register 6
	subq	$48, %rsp	#,
	movq	%rdi, -40(%rbp)	# r, r
# src/Runtime/runtime.c:48:     if (r->type == &_class_Array) {
	movq	-40(%rbp), %rax	# r, tmp93
	movq	(%rax), %rdx	# r_18(D)->type, _5
# src/Runtime/runtime.c:48:     if (r->type == &_class_Array) {
	leaq	_class_Array(%rip), %rax	#, tmp94
	cmpq	%rax, %rdx	# tmp94, _5
	jne	.L6	#,
# src/Runtime/runtime.c:49:         struct Array *arr = r->data;
	movq	-40(%rbp), %rax	# r, tmp95
	movq	8(%rax), %rax	# r_18(D)->data, tmp96
	movq	%rax, -24(%rbp)	# tmp96, arr
# src/Runtime/runtime.c:50:         void **els = arr->elements;
	movq	-24(%rbp), %rax	# arr, tmp97
	movq	8(%rax), %rax	# arr_21->elements, tmp98
	movq	%rax, -32(%rbp)	# tmp98, els
# src/Runtime/runtime.c:51:         if (arr->elementSize == sizeof(void *)) {
	movq	-24(%rbp), %rax	# arr, tmp99
	movl	(%rax), %eax	# arr_21->elementSize, _6
# src/Runtime/runtime.c:51:         if (arr->elementSize == sizeof(void *)) {
	cmpl	$8, %eax	#, _6
	jne	.L7	#,
# src/Runtime/runtime.c:52:             for (int i = 0; i < arr->length; i++)
	movl	$0, -4(%rbp)	#, i
# src/Runtime/runtime.c:52:             for (int i = 0; i < arr->length; i++)
	jmp	.L8	#
.L9:
# src/Runtime/runtime.c:53:                 __decRef(els[i]);
	movl	-4(%rbp), %eax	# i, tmp100
	cltq
	leaq	0(,%rax,8), %rdx	#, _8
	movq	-32(%rbp), %rax	# els, tmp101
	addq	%rdx, %rax	# _8, _9
# src/Runtime/runtime.c:53:                 __decRef(els[i]);
	movq	(%rax), %rax	# *_9, _10
	movq	%rax, %rdi	# _10,
	call	__decRef	#
# src/Runtime/runtime.c:52:             for (int i = 0; i < arr->length; i++)
	addl	$1, -4(%rbp)	#, i
.L8:
# src/Runtime/runtime.c:52:             for (int i = 0; i < arr->length; i++)
	movq	-24(%rbp), %rax	# arr, tmp102
	movl	4(%rax), %eax	# arr_21->length, _11
# src/Runtime/runtime.c:52:             for (int i = 0; i < arr->length; i++)
	cmpl	%eax, -4(%rbp)	# _11, i
	jl	.L9	#,
.L7:
# src/Runtime/runtime.c:55:         if (els != NULL)
	cmpq	$0, -32(%rbp)	#, els
	je	.L10	#,
# src/Runtime/runtime.c:56:             free(els);
	movq	-32(%rbp), %rax	# els, tmp103
	movq	%rax, %rdi	# tmp103,
	call	free@PLT	#
	jmp	.L10	#
.L6:
# src/Runtime/runtime.c:57:     } else if (r->type == &_class_String) {
	movq	-40(%rbp), %rax	# r, tmp104
	movq	(%rax), %rdx	# r_18(D)->type, _12
# src/Runtime/runtime.c:57:     } else if (r->type == &_class_String) {
	leaq	_class_String(%rip), %rax	#, tmp105
	cmpq	%rax, %rdx	# tmp105, _12
	jne	.L10	#,
# src/Runtime/runtime.c:58:         void *els = ((struct String *)r->data)->data;
	movq	-40(%rbp), %rax	# r, tmp106
	movq	8(%rax), %rax	# r_18(D)->data, _13
# src/Runtime/runtime.c:58:         void *els = ((struct String *)r->data)->data;
	movq	8(%rax), %rax	# MEM[(struct String *)_13].data, tmp107
	movq	%rax, -16(%rbp)	# tmp107, els
# src/Runtime/runtime.c:59:         if (els != NULL && els != emptyString)
	cmpq	$0, -16(%rbp)	#, els
	je	.L10	#,
# src/Runtime/runtime.c:59:         if (els != NULL && els != emptyString)
	leaq	emptyString(%rip), %rax	#, tmp108
	cmpq	%rax, -16(%rbp)	# tmp108, els
	je	.L10	#,
# src/Runtime/runtime.c:60:             free(els);
	movq	-16(%rbp), %rax	# els, tmp109
	movq	%rax, %rdi	# tmp109,
	call	free@PLT	#
.L10:
# src/Runtime/runtime.c:62:     if (r->data != NULL)
	movq	-40(%rbp), %rax	# r, tmp110
	movq	8(%rax), %rax	# r_18(D)->data, _14
# src/Runtime/runtime.c:62:     if (r->data != NULL)
	testq	%rax, %rax	# _14
	je	.L11	#,
# src/Runtime/runtime.c:63:         free(r->data);
	movq	-40(%rbp), %rax	# r, tmp111
	movq	8(%rax), %rax	# r_18(D)->data, _15
	movq	%rax, %rdi	# _15,
	call	free@PLT	#
.L11:
# src/Runtime/runtime.c:64:     free(r);
	movq	-40(%rbp), %rax	# r, tmp112
	movq	%rax, %rdi	# tmp112,
	call	free@PLT	#
# src/Runtime/runtime.c:65: }
	nop	
	leave	
	.cfi_def_cfa 7, 8
	ret	
	.cfi_endproc
.LFE7:
	.size	__free, .-__free
	.globl	__incRef
	.type	__incRef, @function
__incRef:
.LFB8:
	.cfi_startproc
	pushq	%rbp	#
	.cfi_def_cfa_offset 16
	.cfi_offset 6, -16
	movq	%rsp, %rbp	#,
	.cfi_def_cfa_register 6
	movq	%rdi, -8(%rbp)	# r, r
# src/Runtime/runtime.c:69:     if (r != NULL) {
	cmpq	$0, -8(%rbp)	#, r
	je	.L14	#,
# src/Runtime/runtime.c:70:         r->counter++;
	movq	-8(%rbp), %rax	# r, tmp84
	movl	16(%rax), %eax	# r_2(D)->counter, _5
# src/Runtime/runtime.c:70:         r->counter++;
	leal	1(%rax), %edx	#, _6
	movq	-8(%rbp), %rax	# r, tmp85
	movl	%edx, 16(%rax)	# _6, r_2(D)->counter
.L14:
# src/Runtime/runtime.c:72: }
	nop	
	popq	%rbp	#
	.cfi_def_cfa 7, 8
	ret	
	.cfi_endproc
.LFE8:
	.size	__incRef, .-__incRef
	.globl	__decRef
	.type	__decRef, @function
__decRef:
.LFB9:
	.cfi_startproc
	pushq	%rbp	#
	.cfi_def_cfa_offset 16
	.cfi_offset 6, -16
	movq	%rsp, %rbp	#,
	.cfi_def_cfa_register 6
	subq	$32, %rsp	#,
	movq	%rdi, -24(%rbp)	# r, r
# src/Runtime/runtime.c:75:     if (r != NULL) {
	cmpq	$0, -24(%rbp)	#, r
	je	.L20	#,
# src/Runtime/runtime.c:76:         r->counter--;
	movq	-24(%rbp), %rax	# r, tmp98
	movl	16(%rax), %eax	# r_1(D)->counter, _5
# src/Runtime/runtime.c:76:         r->counter--;
	leal	-1(%rax), %edx	#, _6
	movq	-24(%rbp), %rax	# r, tmp99
	movl	%edx, 16(%rax)	# _6, r_1(D)->counter
# src/Runtime/runtime.c:77:         if (r->counter <= 0) {
	movq	-24(%rbp), %rax	# r, tmp100
	movl	16(%rax), %eax	# r_1(D)->counter, _7
# src/Runtime/runtime.c:77:         if (r->counter <= 0) {
	testl	%eax, %eax	# _7
	jg	.L20	#,
# src/Runtime/runtime.c:78:             if (r->type != &_class_Array) {
	movq	-24(%rbp), %rax	# r, tmp101
	movq	(%rax), %rdx	# r_1(D)->type, _8
# src/Runtime/runtime.c:78:             if (r->type != &_class_Array) {
	leaq	_class_Array(%rip), %rax	#, tmp102
	cmpq	%rax, %rdx	# tmp102, _8
	je	.L17	#,
# src/Runtime/runtime.c:79:                 for (int i = 0; i < r->type->referenceOffsetsSize; i++)
	movl	$0, -4(%rbp)	#, i
# src/Runtime/runtime.c:79:                 for (int i = 0; i < r->type->referenceOffsetsSize; i++)
	jmp	.L18	#
.L19:
# src/Runtime/runtime.c:80:                     __decRef(*(obj *)(r->data + r->type->referenceOffsets[i]));
	movq	-24(%rbp), %rax	# r, tmp103
	movq	8(%rax), %rdx	# r_1(D)->data, _9
# src/Runtime/runtime.c:80:                     __decRef(*(obj *)(r->data + r->type->referenceOffsets[i]));
	movq	-24(%rbp), %rax	# r, tmp104
	movq	(%rax), %rax	# r_1(D)->type, _10
# src/Runtime/runtime.c:80:                     __decRef(*(obj *)(r->data + r->type->referenceOffsets[i]));
	movq	24(%rax), %rax	# _10->referenceOffsets, _11
# src/Runtime/runtime.c:80:                     __decRef(*(obj *)(r->data + r->type->referenceOffsets[i]));
	movl	-4(%rbp), %ecx	# i, tmp105
	movslq	%ecx, %rcx	# tmp105, _12
	salq	$2, %rcx	#, _13
	addq	%rcx, %rax	# _13, _14
	movl	(%rax), %eax	# *_14, _15
	cltq
# src/Runtime/runtime.c:80:                     __decRef(*(obj *)(r->data + r->type->referenceOffsets[i]));
	addq	%rdx, %rax	# _9, _17
# src/Runtime/runtime.c:80:                     __decRef(*(obj *)(r->data + r->type->referenceOffsets[i]));
	movq	(%rax), %rax	# MEM[(struct Reference * *)_17], _18
	movq	%rax, %rdi	# _18,
	call	__decRef	#
# src/Runtime/runtime.c:79:                 for (int i = 0; i < r->type->referenceOffsetsSize; i++)
	addl	$1, -4(%rbp)	#, i
.L18:
# src/Runtime/runtime.c:79:                 for (int i = 0; i < r->type->referenceOffsetsSize; i++)
	movq	-24(%rbp), %rax	# r, tmp106
	movq	(%rax), %rax	# r_1(D)->type, _19
# src/Runtime/runtime.c:79:                 for (int i = 0; i < r->type->referenceOffsetsSize; i++)
	movl	20(%rax), %eax	# _19->referenceOffsetsSize, _20
# src/Runtime/runtime.c:79:                 for (int i = 0; i < r->type->referenceOffsetsSize; i++)
	cmpl	%eax, -4(%rbp)	# _20, i
	jl	.L19	#,
.L17:
# src/Runtime/runtime.c:82:             __free(r);
	movq	-24(%rbp), %rax	# r, tmp107
	movq	%rax, %rdi	# tmp107,
	call	__free	#
.L20:
# src/Runtime/runtime.c:86: }
	nop	
	leave	
	.cfi_def_cfa 7, 8
	ret	
	.cfi_endproc
.LFE9:
	.size	__decRef, .-__decRef
	.globl	__newRefArray
	.type	__newRefArray, @function
__newRefArray:
.LFB10:
	.cfi_startproc
	pushq	%rbp	#
	.cfi_def_cfa_offset 16
	.cfi_offset 6, -16
	movq	%rsp, %rbp	#,
	.cfi_def_cfa_register 6
	subq	$16, %rsp	#,
	movl	%edi, -4(%rbp)	# length, length
# src/Runtime/runtime.c:88: obj __newRefArray(int32_t length) { return __newArray(sizeof(obj), length); }
	movl	-4(%rbp), %eax	# length, tmp84
	movl	%eax, %esi	# tmp84,
	movl	$8, %edi	#,
	call	__newArray	#
# src/Runtime/runtime.c:88: obj __newRefArray(int32_t length) { return __newArray(sizeof(obj), length); }
	leave	
	.cfi_def_cfa 7, 8
	ret	
	.cfi_endproc
.LFE10:
	.size	__newRefArray, .-__newRefArray
	.globl	__newIntArray
	.type	__newIntArray, @function
__newIntArray:
.LFB11:
	.cfi_startproc
	pushq	%rbp	#
	.cfi_def_cfa_offset 16
	.cfi_offset 6, -16
	movq	%rsp, %rbp	#,
	.cfi_def_cfa_register 6
	subq	$16, %rsp	#,
	movl	%edi, -4(%rbp)	# length, length
# src/Runtime/runtime.c:90:     return __newArray(sizeof(int32_t), length);
	movl	-4(%rbp), %eax	# length, tmp84
	movl	%eax, %esi	# tmp84,
	movl	$4, %edi	#,
	call	__newArray	#
# src/Runtime/runtime.c:91: }
	leave	
	.cfi_def_cfa 7, 8
	ret	
	.cfi_endproc
.LFE11:
	.size	__newIntArray, .-__newIntArray
	.globl	__newByteArray
	.type	__newByteArray, @function
__newByteArray:
.LFB12:
	.cfi_startproc
	pushq	%rbp	#
	.cfi_def_cfa_offset 16
	.cfi_offset 6, -16
	movq	%rsp, %rbp	#,
	.cfi_def_cfa_register 6
	subq	$16, %rsp	#,
	movl	%edi, -4(%rbp)	# length, length
# src/Runtime/runtime.c:93:     return __newArray(sizeof(int8_t), length);
	movl	-4(%rbp), %eax	# length, tmp84
	movl	%eax, %esi	# tmp84,
	movl	$1, %edi	#,
	call	__newArray	#
# src/Runtime/runtime.c:94: }
	leave	
	.cfi_def_cfa 7, 8
	ret	
	.cfi_endproc
.LFE12:
	.size	__newByteArray, .-__newByteArray
	.globl	__newArray
	.type	__newArray, @function
__newArray:
.LFB13:
	.cfi_startproc
	pushq	%rbp	#
	.cfi_def_cfa_offset 16
	.cfi_offset 6, -16
	movq	%rsp, %rbp	#,
	.cfi_def_cfa_register 6
	subq	$32, %rsp	#,
	movl	%edi, -20(%rbp)	# size, size
	movl	%esi, -24(%rbp)	# length, length
# src/Runtime/runtime.c:96:     obj r = __new(&_class_Array);
	leaq	_class_Array(%rip), %rax	#, tmp90
	movq	%rax, %rdi	# tmp90,
	call	__new	#
	movq	%rax, -8(%rbp)	# tmp91, r
# src/Runtime/runtime.c:97:     struct Array *arr = malloc(sizeof(struct Array)+size * length);
	movl	-20(%rbp), %eax	# size, tmp92
	imull	-24(%rbp), %eax	# length, _1
	cltq
# src/Runtime/runtime.c:97:     struct Array *arr = malloc(sizeof(struct Array)+size * length);
	addq	$16, %rax	#, _3
	movq	%rax, %rdi	# _3,
	call	malloc@PLT	#
	movq	%rax, -16(%rbp)	# tmp93, arr
# src/Runtime/runtime.c:98:     r->data = arr;
	movq	-8(%rbp), %rax	# r, tmp94
	movq	-16(%rbp), %rdx	# arr, tmp95
	movq	%rdx, 8(%rax)	# tmp95, r_10->data
# src/Runtime/runtime.c:99:     arr->elementSize = size;
	movq	-16(%rbp), %rax	# arr, tmp96
	movl	-20(%rbp), %edx	# size, tmp97
	movl	%edx, (%rax)	# tmp97, arr_14->elementSize
# src/Runtime/runtime.c:100:     arr->length = length;
	movq	-16(%rbp), %rax	# arr, tmp98
	movl	-24(%rbp), %edx	# length, tmp99
	movl	%edx, 4(%rax)	# tmp99, arr_14->length
# src/Runtime/runtime.c:101:     if (length > 0) {
	cmpl	$0, -24(%rbp)	#, length
	jle	.L28	#,
# src/Runtime/runtime.c:103:         bzero(&(arr->elements), size * length);
	movl	-20(%rbp), %eax	# size, tmp100
	imull	-24(%rbp), %eax	# length, _4
# src/Runtime/runtime.c:103:         bzero(&(arr->elements), size * length);
	cltq
# src/Runtime/runtime.c:103:         bzero(&(arr->elements), size * length);
	movq	-16(%rbp), %rdx	# arr, tmp101
	addq	$8, %rdx	#, _6
# src/Runtime/runtime.c:103:         bzero(&(arr->elements), size * length);
	movq	%rdx, %rcx	# _6, tmp102
	movq	%rax, %rdx	# tmp103,
	movl	$0, %esi	#,
	movq	%rcx, %rdi	# tmp102,
	call	memset@PLT	#
	jmp	.L29	#
.L28:
# src/Runtime/runtime.c:105:         arr->elements = NULL;
	movq	-16(%rbp), %rax	# arr, tmp106
	movq	$0, 8(%rax)	#, arr_14->elements
.L29:
# src/Runtime/runtime.c:107:     return r;
	movq	-8(%rbp), %rax	# r, _20
# src/Runtime/runtime.c:108: }
	leave	
	.cfi_def_cfa 7, 8
	ret	
	.cfi_endproc
.LFE13:
	.size	__newArray, .-__newArray
	.section	.rodata
.LC0:
	.string	"ERROR: Array is null."
	.align 8
.LC1:
	.string	"ERROR: Array index out of range."
.LC2:
	.string	"%d, %d\n"
	.text
	.globl	__getelementptr
	.type	__getelementptr, @function
__getelementptr:
.LFB14:
	.cfi_startproc
	pushq	%rbp	#
	.cfi_def_cfa_offset 16
	.cfi_offset 6, -16
	movq	%rsp, %rbp	#,
	.cfi_def_cfa_register 6
	subq	$32, %rsp	#,
	movq	%rdi, -24(%rbp)	# array, array
	movl	%esi, -28(%rbp)	# index, index
# src/Runtime/runtime.c:111:     if (array == NULL) {
	cmpq	$0, -24(%rbp)	#, array
	jne	.L32	#,
# src/Runtime/runtime.c:112:         errMsg = "ERROR: Array is null.";
	leaq	.LC0(%rip), %rax	#, tmp91
	movq	%rax, errMsg(%rip)	# tmp91, errMsg
# src/Runtime/runtime.c:113:         error();
	movl	$0, %eax	#,
	call	error	#
.L32:
# src/Runtime/runtime.c:115:     struct Array *arr = ((struct Array *)array->data);
	movq	-24(%rbp), %rax	# array, tmp92
	movq	8(%rax), %rax	# array_10(D)->data, tmp93
	movq	%rax, -8(%rbp)	# tmp93, arr
# src/Runtime/runtime.c:116:     if (index >= arr->length || index < 0) {
	movq	-8(%rbp), %rax	# arr, tmp94
	movl	4(%rax), %eax	# arr_14->length, _1
# src/Runtime/runtime.c:116:     if (index >= arr->length || index < 0) {
	cmpl	%eax, -28(%rbp)	# _1, index
	jge	.L33	#,
# src/Runtime/runtime.c:116:     if (index >= arr->length || index < 0) {
	cmpl	$0, -28(%rbp)	#, index
	jns	.L34	#,
.L33:
# src/Runtime/runtime.c:117:         errMsg = "ERROR: Array index out of range.";
	leaq	.LC1(%rip), %rax	#, tmp95
	movq	%rax, errMsg(%rip)	# tmp95, errMsg
# src/Runtime/runtime.c:118:         fprintf(stderr, "%d, %d\n", index, arr->length);
	movq	-8(%rbp), %rax	# arr, tmp96
	movl	4(%rax), %ecx	# arr_14->length, _2
	movq	stderr(%rip), %rax	# stderr, stderr.40_3
	movl	-28(%rbp), %edx	# index, tmp97
	leaq	.LC2(%rip), %rsi	#, tmp98
	movq	%rax, %rdi	# stderr.40_3,
	movl	$0, %eax	#,
	call	fprintf@PLT	#
# src/Runtime/runtime.c:119:         error();
	movl	$0, %eax	#,
	call	error	#
.L34:
# src/Runtime/runtime.c:121:     return arr->elements + index * arr->elementSize;
	movq	-8(%rbp), %rax	# arr, tmp99
	movq	8(%rax), %rdx	# arr_14->elements, _4
# src/Runtime/runtime.c:121:     return arr->elements + index * arr->elementSize;
	movq	-8(%rbp), %rax	# arr, tmp100
	movl	(%rax), %eax	# arr_14->elementSize, _5
# src/Runtime/runtime.c:121:     return arr->elements + index * arr->elementSize;
	imull	-28(%rbp), %eax	# index, _6
	cltq
# src/Runtime/runtime.c:121:     return arr->elements + index * arr->elementSize;
	addq	%rdx, %rax	# _4, _19
# src/Runtime/runtime.c:122: }
	leave	
	.cfi_def_cfa 7, 8
	ret	
	.cfi_endproc
.LFE14:
	.size	__getelementptr, .-__getelementptr
	.globl	__cast
	.type	__cast, @function
__cast:
.LFB15:
	.cfi_startproc
	pushq	%rbp	#
	.cfi_def_cfa_offset 16
	.cfi_offset 6, -16
	movq	%rsp, %rbp	#,
	.cfi_def_cfa_register 6
	movq	%rdi, -24(%rbp)	# o, o
	movq	%rsi, -32(%rbp)	# t, t
# src/Runtime/runtime.c:127:     if (o == NULL) {
	cmpq	$0, -24(%rbp)	#, o
	jne	.L37	#,
# src/Runtime/runtime.c:129:         return NULL;
	movl	$0, %eax	#, _3
	jmp	.L38	#
.L37:
# src/Runtime/runtime.c:132:     struct Type *to = o->type;
	movq	-24(%rbp), %rax	# o, tmp84
	movq	(%rax), %rax	# o_10(D)->type, tmp85
	movq	%rax, -8(%rbp)	# tmp85, to
# src/Runtime/runtime.c:133:     while (to != NULL) {
	jmp	.L39	#
.L42:
# src/Runtime/runtime.c:135:         if (t == to) {
	movq	-32(%rbp), %rax	# t, tmp86
	cmpq	-8(%rbp), %rax	# to, tmp86
	jne	.L40	#,
# src/Runtime/runtime.c:137:             return o;
	movq	-24(%rbp), %rax	# o, _3
	jmp	.L38	#
.L40:
# src/Runtime/runtime.c:139:         struct Type *prev = to;
	movq	-8(%rbp), %rax	# to, tmp87
	movq	%rax, -16(%rbp)	# tmp87, prev
# src/Runtime/runtime.c:140:         to = to->parent;
	movq	-8(%rbp), %rax	# to, tmp88
	movq	(%rax), %rax	# to_8->parent, tmp89
	movq	%rax, -8(%rbp)	# tmp89, to
# src/Runtime/runtime.c:141:         if (prev == to) {
	movq	-16(%rbp), %rax	# prev, tmp90
	cmpq	-8(%rbp), %rax	# to, tmp90
	je	.L44	#,
.L39:
# src/Runtime/runtime.c:133:     while (to != NULL) {
	cmpq	$0, -8(%rbp)	#, to
	jne	.L42	#,
	jmp	.L43	#
.L44:
# src/Runtime/runtime.c:143:             break;
	nop	
.L43:
# src/Runtime/runtime.c:147:     return NULL;
	movl	$0, %eax	#, _3
.L38:
# src/Runtime/runtime.c:148: }
	popq	%rbp	#
	.cfi_def_cfa 7, 8
	ret	
	.cfi_endproc
.LFE15:
	.size	__cast, .-__cast
	.section	.rodata
	.align 8
.LC3:
	.string	"ERROR: Null pointer reference."
	.text
	.globl	__errorNull
	.type	__errorNull, @function
__errorNull:
.LFB16:
	.cfi_startproc
	pushq	%rbp	#
	.cfi_def_cfa_offset 16
	.cfi_offset 6, -16
	movq	%rsp, %rbp	#,
	.cfi_def_cfa_register 6
# src/Runtime/runtime.c:151:     errMsg = "ERROR: Null pointer reference.";
	leaq	.LC3(%rip), %rax	#, tmp82
	movq	%rax, errMsg(%rip)	# tmp82, errMsg
# src/Runtime/runtime.c:152:     error();
	movl	$0, %eax	#,
	call	error	#
# src/Runtime/runtime.c:153: }
	nop	
	popq	%rbp	#
	.cfi_def_cfa 7, 8
	ret	
	.cfi_endproc
.LFE16:
	.size	__errorNull, .-__errorNull
	.section	.rodata
	.align 8
.LC4:
	.string	"ERROR: Non-unicode string encoding."
	.text
	.globl	__createString
	.type	__createString, @function
__createString:
.LFB17:
	.cfi_startproc
	pushq	%rbp	#
	.cfi_def_cfa_offset 16
	.cfi_offset 6, -16
	movq	%rsp, %rbp	#,
	.cfi_def_cfa_register 6
	subq	$48, %rsp	#,
	movq	%rdi, -40(%rbp)	# c, c
# src/Runtime/runtime.c:158:     if (c == NULL) {
	cmpq	$0, -40(%rbp)	#, c
	jne	.L47	#,
# src/Runtime/runtime.c:160:         return __createString(emptyString);
	leaq	emptyString(%rip), %rax	#, tmp98
	movq	%rax, %rdi	# tmp98,
	call	__createString	#
	jmp	.L48	#
.L47:
# src/Runtime/runtime.c:163:     obj r = __new(&_class_String);
	leaq	_class_String(%rip), %rax	#, tmp99
	movq	%rax, %rdi	# tmp99,
	call	__new	#
	movq	%rax, -24(%rbp)	# tmp100, r
# src/Runtime/runtime.c:165:     struct String *str = malloc(sizeof(struct String));
	movl	$16, %edi	#,
	call	malloc@PLT	#
	movq	%rax, -16(%rbp)	# tmp101, str
# src/Runtime/runtime.c:166:     r->data = str;
	movq	-24(%rbp), %rax	# r, tmp102
	movq	-16(%rbp), %rdx	# str, tmp103
	movq	%rdx, 8(%rax)	# tmp103, r_24->data
# src/Runtime/runtime.c:168:     str->length = u8_strlen(c);
	movq	-40(%rbp), %rax	# c, tmp104
	movq	%rax, %rdi	# tmp104,
	call	u8_strlen@PLT	#
# src/Runtime/runtime.c:168:     str->length = u8_strlen(c);
	movl	%eax, %edx	# _13, _14
	movq	-16(%rbp), %rax	# str, tmp105
	movl	%edx, (%rax)	# _14, str_15->length
# src/Runtime/runtime.c:170:     if (u8_check(c, str->length) != NULL) {
	movq	-16(%rbp), %rax	# str, tmp106
	movl	(%rax), %eax	# str_15->length, _19
# src/Runtime/runtime.c:170:     if (u8_check(c, str->length) != NULL) {
	movslq	%eax, %rdx	# _19, _20
	movq	-40(%rbp), %rax	# c, tmp107
	movq	%rdx, %rsi	# _20,
	movq	%rax, %rdi	# tmp107,
	call	u8_check@PLT	#
# src/Runtime/runtime.c:170:     if (u8_check(c, str->length) != NULL) {
	testq	%rax, %rax	# _21
	je	.L49	#,
# src/Runtime/runtime.c:172:         errMsg = "ERROR: Non-unicode string encoding.";
	leaq	.LC4(%rip), %rax	#, tmp108
	movq	%rax, errMsg(%rip)	# tmp108, errMsg
# src/Runtime/runtime.c:173:         error();
	movl	$0, %eax	#,
	call	error	#
.L49:
# src/Runtime/runtime.c:175:     if (str->length > 0) {
	movq	-16(%rbp), %rax	# str, tmp109
	movl	(%rax), %eax	# str_15->length, _26
# src/Runtime/runtime.c:175:     if (str->length > 0) {
	testl	%eax, %eax	# _26
	jle	.L50	#,
# src/Runtime/runtime.c:176:         int len = str->length;
	movq	-16(%rbp), %rax	# str, tmp110
	movl	(%rax), %eax	# str_15->length, tmp111
	movl	%eax, -4(%rbp)	# tmp111, len
# src/Runtime/runtime.c:177:         str->data = malloc(len + 1);
	movl	-4(%rbp), %eax	# len, tmp112
	addl	$1, %eax	#, _27
# src/Runtime/runtime.c:177:         str->data = malloc(len + 1);
	cltq
	movq	%rax, %rdi	# _28,
	call	malloc@PLT	#
	movq	%rax, %rdx	# tmp113, _29
# src/Runtime/runtime.c:177:         str->data = malloc(len + 1);
	movq	-16(%rbp), %rax	# str, tmp114
	movq	%rdx, 8(%rax)	# _29, str_15->data
# src/Runtime/runtime.c:178:         memcpy(str->data, c, len);
	movl	-4(%rbp), %eax	# len, tmp115
	movslq	%eax, %rdx	# tmp115, _30
# src/Runtime/runtime.c:178:         memcpy(str->data, c, len);
	movq	-16(%rbp), %rax	# str, tmp116
	movq	8(%rax), %rax	# str_15->data, _31
# src/Runtime/runtime.c:178:         memcpy(str->data, c, len);
	movq	-40(%rbp), %rcx	# c, tmp117
	movq	%rcx, %rsi	# tmp117,
	movq	%rax, %rdi	# _31,
	call	memcpy@PLT	#
# src/Runtime/runtime.c:179:         str->data[len] = 0;
	movq	-16(%rbp), %rax	# str, tmp118
	movq	8(%rax), %rdx	# str_15->data, _32
# src/Runtime/runtime.c:179:         str->data[len] = 0;
	movl	-4(%rbp), %eax	# len, tmp119
	cltq
	addq	%rdx, %rax	# _32, _34
# src/Runtime/runtime.c:179:         str->data[len] = 0;
	movb	$0, (%rax)	#, *_34
# src/Runtime/runtime.c:185:     str->length = -1;
	movq	-16(%rbp), %rax	# str, tmp122
	movl	$-1, (%rax)	#, str_15->length
# src/Runtime/runtime.c:186:     return r;
	movq	-24(%rbp), %rax	# r, _4
	jmp	.L48	#
.L50:
# src/Runtime/runtime.c:181:         str->data = emptyString;
	movq	-16(%rbp), %rax	# str, tmp120
	leaq	emptyString(%rip), %rdx	#, tmp121
	movq	%rdx, 8(%rax)	# tmp121, str_15->data
# src/Runtime/runtime.c:182:         return r;
	movq	-24(%rbp), %rax	# r, _4
.L48:
# src/Runtime/runtime.c:187: }
	leave	
	.cfi_def_cfa 7, 8
	ret	
	.cfi_endproc
.LFE17:
	.size	__createString, .-__createString
	.section	.rodata
.LC5:
	.string	"Object"
	.text
	.globl	_Object_toString
	.type	_Object_toString, @function
_Object_toString:
.LFB18:
	.cfi_startproc
	pushq	%rbp	#
	.cfi_def_cfa_offset 16
	.cfi_offset 6, -16
	movq	%rsp, %rbp	#,
	.cfi_def_cfa_register 6
	subq	$32, %rsp	#,
	movq	%rdi, -24(%rbp)	# o, o
# src/Runtime/runtime.c:191:     obj ret = __createString("Object");
	leaq	.LC5(%rip), %rax	#, tmp84
	movq	%rax, %rdi	# tmp84,
	call	__createString	#
	movq	%rax, -8(%rbp)	# tmp85, ret
# src/Runtime/runtime.c:192:     __incRef(ret);
	movq	-8(%rbp), %rax	# ret, tmp86
	movq	%rax, %rdi	# tmp86,
	call	__incRef	#
# src/Runtime/runtime.c:193:     return ret;
	movq	-8(%rbp), %rax	# ret, _5
# src/Runtime/runtime.c:194: }
	leave	
	.cfi_def_cfa 7, 8
	ret	
	.cfi_endproc
.LFE18:
	.size	_Object_toString, .-_Object_toString
	.globl	_Object_getHashCode
	.type	_Object_getHashCode, @function
_Object_getHashCode:
.LFB19:
	.cfi_startproc
	pushq	%rbp	#
	.cfi_def_cfa_offset 16
	.cfi_offset 6, -16
	movq	%rsp, %rbp	#,
	.cfi_def_cfa_register 6
	movq	%rdi, -8(%rbp)	# o, o
# src/Runtime/runtime.c:195: int32_t _Object_getHashCode(obj o) { return (int32_t)(int64_t)o; }
	movq	-8(%rbp), %rax	# o, o.109_1
# src/Runtime/runtime.c:195: int32_t _Object_getHashCode(obj o) { return (int32_t)(int64_t)o; }
	popq	%rbp	#
	.cfi_def_cfa 7, 8
	ret	
	.cfi_endproc
.LFE19:
	.size	_Object_getHashCode, .-_Object_getHashCode
	.globl	_Object_equals
	.type	_Object_equals, @function
_Object_equals:
.LFB20:
	.cfi_startproc
	pushq	%rbp	#
	.cfi_def_cfa_offset 16
	.cfi_offset 6, -16
	movq	%rsp, %rbp	#,
	.cfi_def_cfa_register 6
	movq	%rdi, -8(%rbp)	# o1, o1
	movq	%rsi, -16(%rbp)	# o2, o2
# src/Runtime/runtime.c:196: int8_t _Object_equals(obj o1, obj o2) { return o1 == o2; }
	movq	-8(%rbp), %rax	# o1, tmp85
	cmpq	-16(%rbp), %rax	# o2, tmp85
	sete	%al	#, _1
# src/Runtime/runtime.c:196: int8_t _Object_equals(obj o1, obj o2) { return o1 == o2; }
	popq	%rbp	#
	.cfi_def_cfa 7, 8
	ret	
	.cfi_endproc
.LFE20:
	.size	_Object_equals, .-_Object_equals
	.section	.rodata
.LC6:
	.string	"null"
	.text
	.globl	_Array_toString
	.type	_Array_toString, @function
_Array_toString:
.LFB21:
	.cfi_startproc
	pushq	%rbp	#
	.cfi_def_cfa_offset 16
	.cfi_offset 6, -16
	movq	%rsp, %rbp	#,
	.cfi_def_cfa_register 6
	pushq	%r12	#
	pushq	%rbx	#
	addq	$-128, %rsp	#,
	.cfi_offset 12, -24
	.cfi_offset 3, -32
	movq	%rdi, -136(%rbp)	# arr, arr
# src/Runtime/runtime.c:199:     char start[] = "[";
	movw	$91, -122(%rbp)	#, start
# src/Runtime/runtime.c:200:     char delim[] = ", ";
	movw	$8236, -125(%rbp)	#, delim
	movb	$0, -123(%rbp)	#, delim
# src/Runtime/runtime.c:201:     char end[] = "]";
	movw	$93, -127(%rbp)	#, end
# src/Runtime/runtime.c:202:     struct Array *array = arr->data;
	movq	-136(%rbp), %rax	# arr, tmp188
	movq	8(%rax), %rax	# arr_118(D)->data, tmp189
	movq	%rax, -40(%rbp)	# tmp189, array
# src/Runtime/runtime.c:204:     obj *strings = malloc(sizeof(obj) * array->length);
	movq	-40(%rbp), %rax	# array, tmp190
	movl	4(%rax), %eax	# array_119->length, _1
	cltq
# src/Runtime/runtime.c:204:     obj *strings = malloc(sizeof(obj) * array->length);
	salq	$3, %rax	#, _3
	movq	%rax, %rdi	# _3,
	call	malloc@PLT	#
	movq	%rax, -48(%rbp)	# tmp191, strings
# src/Runtime/runtime.c:205:     int32_t *lenghts = malloc(sizeof(int32_t) * array->length);
	movq	-40(%rbp), %rax	# array, tmp192
	movl	4(%rax), %eax	# array_119->length, _4
	cltq
# src/Runtime/runtime.c:205:     int32_t *lenghts = malloc(sizeof(int32_t) * array->length);
	salq	$2, %rax	#, _6
	movq	%rax, %rdi	# _6,
	call	malloc@PLT	#
	movq	%rax, -56(%rbp)	# tmp193, lenghts
# src/Runtime/runtime.c:206:     int32_t totalLenght = 0;
	movl	$0, -20(%rbp)	#, totalLenght
# src/Runtime/runtime.c:208:     for (int i = 0; i < array->length; i++) {
	movl	$0, -24(%rbp)	#, i
# src/Runtime/runtime.c:208:     for (int i = 0; i < array->length; i++) {
	jmp	.L59	#
.L64:
# src/Runtime/runtime.c:209:         if (array->elementSize == sizeof(int32_t)) {
	movq	-40(%rbp), %rax	# array, tmp194
	movl	(%rax), %eax	# array_119->elementSize, _7
# src/Runtime/runtime.c:209:         if (array->elementSize == sizeof(int32_t)) {
	cmpl	$4, %eax	#, _7
	jne	.L60	#,
# src/Runtime/runtime.c:210:             int32_t *elements = array->elements;
	movq	-40(%rbp), %rax	# array, tmp195
	movq	8(%rax), %rax	# array_119->elements, tmp196
	movq	%rax, -120(%rbp)	# tmp196, elements
# src/Runtime/runtime.c:211:             strings[i] = intToString(elements[i]);
	movl	-24(%rbp), %eax	# i, tmp197
	cltq
	leaq	0(,%rax,4), %rdx	#, _9
	movq	-120(%rbp), %rax	# elements, tmp198
	addq	%rdx, %rax	# _9, _10
# src/Runtime/runtime.c:211:             strings[i] = intToString(elements[i]);
	movl	(%rax), %eax	# *_10, _11
# src/Runtime/runtime.c:211:             strings[i] = intToString(elements[i]);
	movl	-24(%rbp), %edx	# i, tmp199
	movslq	%edx, %rdx	# tmp199, _12
	leaq	0(,%rdx,8), %rcx	#, _13
	movq	-48(%rbp), %rdx	# strings, tmp200
	leaq	(%rcx,%rdx), %rbx	#, _14
# src/Runtime/runtime.c:211:             strings[i] = intToString(elements[i]);
	movl	%eax, %edi	# _11,
	call	intToString	#
# src/Runtime/runtime.c:211:             strings[i] = intToString(elements[i]);
	movq	%rax, (%rbx)	# _15, *_14
	jmp	.L61	#
.L60:
# src/Runtime/runtime.c:212:         } else if (array->elementSize == sizeof(int8_t)) {
	movq	-40(%rbp), %rax	# array, tmp201
	movl	(%rax), %eax	# array_119->elementSize, _16
# src/Runtime/runtime.c:212:         } else if (array->elementSize == sizeof(int8_t)) {
	cmpl	$1, %eax	#, _16
	jne	.L62	#,
# src/Runtime/runtime.c:213:             int8_t *elements = array->elements;
	movq	-40(%rbp), %rax	# array, tmp202
	movq	8(%rax), %rax	# array_119->elements, tmp203
	movq	%rax, -112(%rbp)	# tmp203, elements
# src/Runtime/runtime.c:214:             strings[i] = byteToString(elements[i]);
	movl	-24(%rbp), %eax	# i, tmp204
	movslq	%eax, %rdx	# tmp204, _17
	movq	-112(%rbp), %rax	# elements, tmp205
	addq	%rdx, %rax	# _17, _18
	movzbl	(%rax), %eax	# *_18, _19
# src/Runtime/runtime.c:214:             strings[i] = byteToString(elements[i]);
	movzbl	%al, %eax	# _20, _21
# src/Runtime/runtime.c:214:             strings[i] = byteToString(elements[i]);
	movl	-24(%rbp), %edx	# i, tmp206
	movslq	%edx, %rdx	# tmp206, _22
	leaq	0(,%rdx,8), %rcx	#, _23
	movq	-48(%rbp), %rdx	# strings, tmp207
	leaq	(%rcx,%rdx), %rbx	#, _24
# src/Runtime/runtime.c:214:             strings[i] = byteToString(elements[i]);
	movl	%eax, %edi	# _21,
	call	byteToString	#
# src/Runtime/runtime.c:214:             strings[i] = byteToString(elements[i]);
	movq	%rax, (%rbx)	# _25, *_24
	jmp	.L61	#
.L62:
# src/Runtime/runtime.c:216:             obj *elements = array->elements;
	movq	-40(%rbp), %rax	# array, tmp208
	movq	8(%rax), %rax	# array_119->elements, tmp209
	movq	%rax, -88(%rbp)	# tmp209, elements
# src/Runtime/runtime.c:217:             obj element = elements[i];
	movl	-24(%rbp), %eax	# i, tmp210
	cltq
	leaq	0(,%rax,8), %rdx	#, _27
	movq	-88(%rbp), %rax	# elements, tmp211
	addq	%rdx, %rax	# _27, _28
# src/Runtime/runtime.c:217:             obj element = elements[i];
	movq	(%rax), %rax	# *_28, tmp212
	movq	%rax, -96(%rbp)	# tmp212, element
# src/Runtime/runtime.c:218:             if (element == NULL) {
	cmpq	$0, -96(%rbp)	#, element
	jne	.L63	#,
# src/Runtime/runtime.c:219:                 strings[i] = __createString("null");
	movl	-24(%rbp), %eax	# i, tmp213
	cltq
	leaq	0(,%rax,8), %rdx	#, _30
	movq	-48(%rbp), %rax	# strings, tmp214
	leaq	(%rdx,%rax), %rbx	#, _31
# src/Runtime/runtime.c:219:                 strings[i] = __createString("null");
	leaq	.LC6(%rip), %rax	#, tmp215
	movq	%rax, %rdi	# tmp215,
	call	__createString	#
# src/Runtime/runtime.c:219:                 strings[i] = __createString("null");
	movq	%rax, (%rbx)	# _32, *_31
# src/Runtime/runtime.c:220:                 __incRef(strings[i]);
	movl	-24(%rbp), %eax	# i, tmp216
	cltq
	leaq	0(,%rax,8), %rdx	#, _34
	movq	-48(%rbp), %rax	# strings, tmp217
	addq	%rdx, %rax	# _34, _35
# src/Runtime/runtime.c:220:                 __incRef(strings[i]);
	movq	(%rax), %rax	# *_35, _36
	movq	%rax, %rdi	# _36,
	call	__incRef	#
	jmp	.L61	#
.L63:
# src/Runtime/runtime.c:222:                 obj (*toString)(obj) = ((void **)element->type->methods)[0];
	movq	-96(%rbp), %rax	# element, tmp218
	movq	(%rax), %rax	# element_152->type, _37
# src/Runtime/runtime.c:222:                 obj (*toString)(obj) = ((void **)element->type->methods)[0];
	movq	12(%rax), %rax	# _37->methods, _38
# src/Runtime/runtime.c:222:                 obj (*toString)(obj) = ((void **)element->type->methods)[0];
	movq	(%rax), %rax	# MEM[(void * *)_38], _39
# src/Runtime/runtime.c:222:                 obj (*toString)(obj) = ((void **)element->type->methods)[0];
	movq	%rax, -104(%rbp)	# _39, toString
# src/Runtime/runtime.c:223:                 strings[i] = toString(element);
	movl	-24(%rbp), %eax	# i, tmp219
	cltq
	leaq	0(,%rax,8), %rdx	#, _41
	movq	-48(%rbp), %rax	# strings, tmp220
	leaq	(%rdx,%rax), %rbx	#, _42
# src/Runtime/runtime.c:223:                 strings[i] = toString(element);
	movq	-96(%rbp), %rax	# element, tmp221
	movq	-104(%rbp), %rdx	# toString, tmp222
	movq	%rax, %rdi	# tmp221,
	call	*%rdx	# tmp222
# src/Runtime/runtime.c:223:                 strings[i] = toString(element);
	movq	%rax, (%rbx)	# _43, *_42
.L61:
# src/Runtime/runtime.c:226:         lenghts[i] = u8_strlen(((struct String *)strings[i]->data)->data);
	movl	-24(%rbp), %eax	# i, tmp223
	cltq
	leaq	0(,%rax,8), %rdx	#, _45
	movq	-48(%rbp), %rax	# strings, tmp224
	addq	%rdx, %rax	# _45, _46
	movq	(%rax), %rax	# *_46, _47
# src/Runtime/runtime.c:226:         lenghts[i] = u8_strlen(((struct String *)strings[i]->data)->data);
	movq	8(%rax), %rax	# _47->data, _48
# src/Runtime/runtime.c:226:         lenghts[i] = u8_strlen(((struct String *)strings[i]->data)->data);
	movq	8(%rax), %rax	# MEM[(struct String *)_48].data, _49
# src/Runtime/runtime.c:226:         lenghts[i] = u8_strlen(((struct String *)strings[i]->data)->data);
	movq	%rax, %rdi	# _49,
	call	u8_strlen@PLT	#
# src/Runtime/runtime.c:226:         lenghts[i] = u8_strlen(((struct String *)strings[i]->data)->data);
	movl	-24(%rbp), %edx	# i, tmp225
	movslq	%edx, %rdx	# tmp225, _51
	leaq	0(,%rdx,4), %rcx	#, _52
	movq	-56(%rbp), %rdx	# lenghts, tmp226
	addq	%rcx, %rdx	# _52, _53
# src/Runtime/runtime.c:226:         lenghts[i] = u8_strlen(((struct String *)strings[i]->data)->data);
	movl	%eax, (%rdx)	# _54, *_53
# src/Runtime/runtime.c:227:         totalLenght += lenghts[i];
	movl	-24(%rbp), %eax	# i, tmp227
	cltq
	leaq	0(,%rax,4), %rdx	#, _56
	movq	-56(%rbp), %rax	# lenghts, tmp228
	addq	%rdx, %rax	# _56, _57
	movl	(%rax), %eax	# *_57, _58
# src/Runtime/runtime.c:227:         totalLenght += lenghts[i];
	addl	%eax, -20(%rbp)	# _58, totalLenght
# src/Runtime/runtime.c:208:     for (int i = 0; i < array->length; i++) {
	addl	$1, -24(%rbp)	#, i
.L59:
# src/Runtime/runtime.c:208:     for (int i = 0; i < array->length; i++) {
	movq	-40(%rbp), %rax	# array, tmp229
	movl	4(%rax), %eax	# array_119->length, _59
# src/Runtime/runtime.c:208:     for (int i = 0; i < array->length; i++) {
	cmpl	%eax, -24(%rbp)	# _59, i
	jl	.L64	#,
# src/Runtime/runtime.c:230:     int32_t bufferSize = u8_strlen(start) + totalLenght +
	leaq	-122(%rbp), %rax	#, tmp230
	movq	%rax, %rdi	# tmp230,
	call	u8_strlen@PLT	#
# src/Runtime/runtime.c:230:     int32_t bufferSize = u8_strlen(start) + totalLenght +
	movl	%eax, %edx	# _60, _61
	movl	-20(%rbp), %eax	# totalLenght, totalLenght.110_62
	leal	(%rdx,%rax), %ebx	#, _63
# src/Runtime/runtime.c:231:                          (array->length - 1) * u8_strlen(delim) +
	movq	-40(%rbp), %rax	# array, tmp231
	movl	4(%rax), %eax	# array_119->length, _64
# src/Runtime/runtime.c:231:                          (array->length - 1) * u8_strlen(delim) +
	subl	$1, %eax	#, _65
	cltq
# src/Runtime/runtime.c:230:     int32_t bufferSize = u8_strlen(start) + totalLenght +
	movl	%eax, %r12d	# _66, _67
# src/Runtime/runtime.c:231:                          (array->length - 1) * u8_strlen(delim) +
	leaq	-125(%rbp), %rax	#, tmp232
	movq	%rax, %rdi	# tmp232,
	call	u8_strlen@PLT	#
# src/Runtime/runtime.c:230:     int32_t bufferSize = u8_strlen(start) + totalLenght +
	imull	%r12d, %eax	# _67, _70
	addl	%eax, %ebx	# _70, _71
# src/Runtime/runtime.c:232:                          u8_strlen(end) + 1;
	leaq	-127(%rbp), %rax	#, tmp233
	movq	%rax, %rdi	# tmp233,
	call	u8_strlen@PLT	#
# src/Runtime/runtime.c:231:                          (array->length - 1) * u8_strlen(delim) +
	addl	%ebx, %eax	# _71, _74
# src/Runtime/runtime.c:232:                          u8_strlen(end) + 1;
	addl	$1, %eax	#, _75
# src/Runtime/runtime.c:230:     int32_t bufferSize = u8_strlen(start) + totalLenght +
	movl	%eax, -60(%rbp)	# _75, bufferSize
# src/Runtime/runtime.c:233:     uint8_t *buffer = malloc(bufferSize);
	movl	-60(%rbp), %eax	# bufferSize, tmp234
	cltq
	movq	%rax, %rdi	# _76,
	call	malloc@PLT	#
	movq	%rax, -72(%rbp)	# tmp235, buffer
# src/Runtime/runtime.c:234:     int32_t index = 0;
	movl	$0, -28(%rbp)	#, index
# src/Runtime/runtime.c:235:     u8_strcpy(buffer + index, start);
	movl	-28(%rbp), %eax	# index, tmp236
	movslq	%eax, %rdx	# tmp236, _77
	movq	-72(%rbp), %rax	# buffer, tmp237
	addq	%rax, %rdx	# tmp237, _78
	leaq	-122(%rbp), %rax	#, tmp238
	movq	%rax, %rsi	# tmp238,
	movq	%rdx, %rdi	# _78,
	call	u8_strcpy@PLT	#
# src/Runtime/runtime.c:236:     index++;
	addl	$1, -28(%rbp)	#, index
# src/Runtime/runtime.c:237:     for (int i = 0; i < array->length; i++) {
	movl	$0, -32(%rbp)	#, i
# src/Runtime/runtime.c:237:     for (int i = 0; i < array->length; i++) {
	jmp	.L65	#
.L67:
# src/Runtime/runtime.c:238:         u8_strcpy(buffer + index, ((struct String *)strings[i]->data)->data);
	movl	-32(%rbp), %eax	# i, tmp239
	cltq
	leaq	0(,%rax,8), %rdx	#, _80
	movq	-48(%rbp), %rax	# strings, tmp240
	addq	%rdx, %rax	# _80, _81
	movq	(%rax), %rax	# *_81, _82
# src/Runtime/runtime.c:238:         u8_strcpy(buffer + index, ((struct String *)strings[i]->data)->data);
	movq	8(%rax), %rax	# _82->data, _83
# src/Runtime/runtime.c:238:         u8_strcpy(buffer + index, ((struct String *)strings[i]->data)->data);
	movq	8(%rax), %rax	# MEM[(struct String *)_83].data, _84
# src/Runtime/runtime.c:238:         u8_strcpy(buffer + index, ((struct String *)strings[i]->data)->data);
	movl	-28(%rbp), %edx	# index, tmp241
	movslq	%edx, %rcx	# tmp241, _85
	movq	-72(%rbp), %rdx	# buffer, tmp242
	addq	%rcx, %rdx	# _85, _86
	movq	%rax, %rsi	# _84,
	movq	%rdx, %rdi	# _86,
	call	u8_strcpy@PLT	#
# src/Runtime/runtime.c:239:         index += lenghts[i];
	movl	-32(%rbp), %eax	# i, tmp243
	cltq
	leaq	0(,%rax,4), %rdx	#, _88
	movq	-56(%rbp), %rax	# lenghts, tmp244
	addq	%rdx, %rax	# _88, _89
	movl	(%rax), %eax	# *_89, _90
# src/Runtime/runtime.c:239:         index += lenghts[i];
	addl	%eax, -28(%rbp)	# _90, index
# src/Runtime/runtime.c:240:         if (i != array->length - 1) {
	movq	-40(%rbp), %rax	# array, tmp245
	movl	4(%rax), %eax	# array_119->length, _91
# src/Runtime/runtime.c:240:         if (i != array->length - 1) {
	subl	$1, %eax	#, _92
# src/Runtime/runtime.c:240:         if (i != array->length - 1) {
	cmpl	%eax, -32(%rbp)	# _92, i
	je	.L66	#,
# src/Runtime/runtime.c:241:             u8_strcpy(buffer + index, delim);
	movl	-28(%rbp), %eax	# index, tmp246
	movslq	%eax, %rdx	# tmp246, _93
	movq	-72(%rbp), %rax	# buffer, tmp247
	addq	%rax, %rdx	# tmp247, _94
	leaq	-125(%rbp), %rax	#, tmp248
	movq	%rax, %rsi	# tmp248,
	movq	%rdx, %rdi	# _94,
	call	u8_strcpy@PLT	#
# src/Runtime/runtime.c:242:             index += 2;
	addl	$2, -28(%rbp)	#, index
.L66:
# src/Runtime/runtime.c:244:         __decRef(strings[i]);
	movl	-32(%rbp), %eax	# i, tmp249
	cltq
	leaq	0(,%rax,8), %rdx	#, _96
	movq	-48(%rbp), %rax	# strings, tmp250
	addq	%rdx, %rax	# _96, _97
# src/Runtime/runtime.c:244:         __decRef(strings[i]);
	movq	(%rax), %rax	# *_97, _98
	movq	%rax, %rdi	# _98,
	call	__decRef	#
# src/Runtime/runtime.c:237:     for (int i = 0; i < array->length; i++) {
	addl	$1, -32(%rbp)	#, i
.L65:
# src/Runtime/runtime.c:237:     for (int i = 0; i < array->length; i++) {
	movq	-40(%rbp), %rax	# array, tmp251
	movl	4(%rax), %eax	# array_119->length, _99
# src/Runtime/runtime.c:237:     for (int i = 0; i < array->length; i++) {
	cmpl	%eax, -32(%rbp)	# _99, i
	jl	.L67	#,
# src/Runtime/runtime.c:246:     u8_strcpy(buffer + index, end);
	movl	-28(%rbp), %eax	# index, tmp252
	movslq	%eax, %rdx	# tmp252, _100
	movq	-72(%rbp), %rax	# buffer, tmp253
	addq	%rax, %rdx	# tmp253, _101
	leaq	-127(%rbp), %rax	#, tmp254
	movq	%rax, %rsi	# tmp254,
	movq	%rdx, %rdi	# _101,
	call	u8_strcpy@PLT	#
# src/Runtime/runtime.c:247:     buffer[bufferSize - 1] = 0;
	movl	-60(%rbp), %eax	# bufferSize, tmp255
	cltq
	leaq	-1(%rax), %rdx	#, _103
	movq	-72(%rbp), %rax	# buffer, tmp256
	addq	%rdx, %rax	# _103, _104
# src/Runtime/runtime.c:247:     buffer[bufferSize - 1] = 0;
	movb	$0, (%rax)	#, *_104
# src/Runtime/runtime.c:248:     obj ret = __createString(buffer);
	movq	-72(%rbp), %rax	# buffer, tmp257
	movq	%rax, %rdi	# tmp257,
	call	__createString	#
	movq	%rax, -80(%rbp)	# tmp258, ret
# src/Runtime/runtime.c:249:     __incRef(ret);
	movq	-80(%rbp), %rax	# ret, tmp259
	movq	%rax, %rdi	# tmp259,
	call	__incRef	#
# src/Runtime/runtime.c:250:     free(lenghts);
	movq	-56(%rbp), %rax	# lenghts, tmp260
	movq	%rax, %rdi	# tmp260,
	call	free@PLT	#
# src/Runtime/runtime.c:251:     free(strings);
	movq	-48(%rbp), %rax	# strings, tmp261
	movq	%rax, %rdi	# tmp261,
	call	free@PLT	#
# src/Runtime/runtime.c:252:     free(buffer);
	movq	-72(%rbp), %rax	# buffer, tmp262
	movq	%rax, %rdi	# tmp262,
	call	free@PLT	#
# src/Runtime/runtime.c:253:     return ret;
	movq	-80(%rbp), %rax	# ret, _141
# src/Runtime/runtime.c:254: }
	subq	$-128, %rsp	#,
	popq	%rbx	#
	popq	%r12	#
	popq	%rbp	#
	.cfi_def_cfa 7, 8
	ret	
	.cfi_endproc
.LFE21:
	.size	_Array_toString, .-_Array_toString
	.globl	_String_toString
	.type	_String_toString, @function
_String_toString:
.LFB22:
	.cfi_startproc
	pushq	%rbp	#
	.cfi_def_cfa_offset 16
	.cfi_offset 6, -16
	movq	%rsp, %rbp	#,
	.cfi_def_cfa_register 6
	subq	$8, %rsp	#,
	movq	%rdi, -8(%rbp)	# str, str
# src/Runtime/runtime.c:257:     __incRef(str);
	movq	-8(%rbp), %rax	# str, tmp84
	movq	%rax, %rdi	# tmp84,
	call	__incRef	#
# src/Runtime/runtime.c:258:     return str;
	movq	-8(%rbp), %rax	# str, _4
# src/Runtime/runtime.c:259: }
	leave	
	.cfi_def_cfa 7, 8
	ret	
	.cfi_endproc
.LFE22:
	.size	_String_toString, .-_String_toString
	.globl	_String_getHashCode
	.type	_String_getHashCode, @function
_String_getHashCode:
.LFB23:
	.cfi_startproc
	pushq	%rbp	#
	.cfi_def_cfa_offset 16
	.cfi_offset 6, -16
	movq	%rsp, %rbp	#,
	.cfi_def_cfa_register 6
	subq	$48, %rsp	#,
	movq	%rdi, -40(%rbp)	# str, str
# src/Runtime/runtime.c:261:     int32_t hash = 0x811c9dc5;
	movl	$-2128831035, -4(%rbp)	#, hash
# src/Runtime/runtime.c:262:     uint8_t *rawstring = ((struct String *)str->data)->data;
	movq	-40(%rbp), %rax	# str, tmp90
	movq	8(%rax), %rax	# str_11(D)->data, _1
# src/Runtime/runtime.c:262:     uint8_t *rawstring = ((struct String *)str->data)->data;
	movq	8(%rax), %rax	# MEM[(struct String *)_1].data, tmp91
	movq	%rax, -16(%rbp)	# tmp91, rawstring
# src/Runtime/runtime.c:263:     int32_t strlen = u8_strlen(rawstring);
	movq	-16(%rbp), %rax	# rawstring, tmp92
	movq	%rax, %rdi	# tmp92,
	call	u8_strlen@PLT	#
# src/Runtime/runtime.c:263:     int32_t strlen = u8_strlen(rawstring);
	movl	%eax, -20(%rbp)	# _2, strlen
# src/Runtime/runtime.c:264:     for (int i = 0; i < strlen; i++) {
	movl	$0, -8(%rbp)	#, i
# src/Runtime/runtime.c:264:     for (int i = 0; i < strlen; i++) {
	jmp	.L72	#
.L73:
# src/Runtime/runtime.c:265:         hash ^= rawstring[i];
	movl	-8(%rbp), %eax	# i, tmp93
	movslq	%eax, %rdx	# tmp93, _3
	movq	-16(%rbp), %rax	# rawstring, tmp94
	addq	%rdx, %rax	# _3, _4
	movzbl	(%rax), %eax	# *_4, _5
	movzbl	%al, %eax	# _5, _6
# src/Runtime/runtime.c:265:         hash ^= rawstring[i];
	xorl	%eax, -4(%rbp)	# _6, hash
# src/Runtime/runtime.c:266:         hash *= 0x01000193;
	movl	-4(%rbp), %eax	# hash, tmp96
	imull	$16777619, %eax, %eax	#, tmp96, tmp95
	movl	%eax, -4(%rbp)	# tmp95, hash
# src/Runtime/runtime.c:264:     for (int i = 0; i < strlen; i++) {
	addl	$1, -8(%rbp)	#, i
.L72:
# src/Runtime/runtime.c:264:     for (int i = 0; i < strlen; i++) {
	movl	-8(%rbp), %eax	# i, tmp97
	cmpl	-20(%rbp), %eax	# strlen, tmp97
	jl	.L73	#,
# src/Runtime/runtime.c:268:     return hash;
	movl	-4(%rbp), %eax	# hash, _15
# src/Runtime/runtime.c:269: }
	leave	
	.cfi_def_cfa 7, 8
	ret	
	.cfi_endproc
.LFE23:
	.size	_String_getHashCode, .-_String_getHashCode
	.globl	_String_equals
	.type	_String_equals, @function
_String_equals:
.LFB24:
	.cfi_startproc
	pushq	%rbp	#
	.cfi_def_cfa_offset 16
	.cfi_offset 6, -16
	movq	%rsp, %rbp	#,
	.cfi_def_cfa_register 6
	pushq	%rbx	#
	subq	$40, %rsp	#,
	.cfi_offset 3, -24
	movq	%rdi, -40(%rbp)	# o1, o1
	movq	%rsi, -48(%rbp)	# o2, o2
# src/Runtime/runtime.c:271:     if (o2 == NULL)
	cmpq	$0, -48(%rbp)	#, o2
	jne	.L76	#,
# src/Runtime/runtime.c:272:         return false;
	movl	$0, %eax	#, _8
	jmp	.L77	#
.L76:
# src/Runtime/runtime.c:273:     if (o2->type != &_class_String)
	movq	-48(%rbp), %rax	# o2, tmp91
	movq	(%rax), %rdx	# o2_10(D)->type, _1
# src/Runtime/runtime.c:273:     if (o2->type != &_class_String)
	leaq	_class_String(%rip), %rax	#, tmp92
	cmpq	%rax, %rdx	# tmp92, _1
	je	.L78	#,
# src/Runtime/runtime.c:274:         return false;
	movl	$0, %eax	#, _8
	jmp	.L77	#
.L78:
# src/Runtime/runtime.c:275:     if (_String_length(o1) != _String_length(o2))
	movq	-40(%rbp), %rax	# o1, tmp93
	movq	%rax, %rdi	# tmp93,
	call	_String_length	#
	movl	%eax, %ebx	#, _2
# src/Runtime/runtime.c:275:     if (_String_length(o1) != _String_length(o2))
	movq	-48(%rbp), %rax	# o2, tmp94
	movq	%rax, %rdi	# tmp94,
	call	_String_length	#
# src/Runtime/runtime.c:275:     if (_String_length(o1) != _String_length(o2))
	cmpl	%eax, %ebx	# _3, _2
	je	.L79	#,
# src/Runtime/runtime.c:276:         return false;
	movl	$0, %eax	#, _8
	jmp	.L77	#
.L79:
# src/Runtime/runtime.c:277:     uint8_t *rs1 = ((struct String *)o1->data)->data;
	movq	-40(%rbp), %rax	# o1, tmp95
	movq	8(%rax), %rax	# o1_12(D)->data, _4
# src/Runtime/runtime.c:277:     uint8_t *rs1 = ((struct String *)o1->data)->data;
	movq	8(%rax), %rax	# MEM[(struct String *)_4].data, tmp96
	movq	%rax, -24(%rbp)	# tmp96, rs1
# src/Runtime/runtime.c:278:     uint8_t *rs2 = ((struct String *)o2->data)->data;
	movq	-48(%rbp), %rax	# o2, tmp97
	movq	8(%rax), %rax	# o2_10(D)->data, _5
# src/Runtime/runtime.c:278:     uint8_t *rs2 = ((struct String *)o2->data)->data;
	movq	8(%rax), %rax	# MEM[(struct String *)_5].data, tmp98
	movq	%rax, -32(%rbp)	# tmp98, rs2
# src/Runtime/runtime.c:279:     return u8_strcmp(rs1, rs2) == 0;
	movq	-32(%rbp), %rdx	# rs2, tmp99
	movq	-24(%rbp), %rax	# rs1, tmp100
	movq	%rdx, %rsi	# tmp99,
	movq	%rax, %rdi	# tmp100,
	call	u8_strcmp@PLT	#
# src/Runtime/runtime.c:279:     return u8_strcmp(rs1, rs2) == 0;
	testl	%eax, %eax	# _6
	sete	%al	#, _7
.L77:
# src/Runtime/runtime.c:280: }
	movq	-8(%rbp), %rbx	#,
	leave	
	.cfi_def_cfa 7, 8
	ret	
	.cfi_endproc
.LFE24:
	.size	_String_equals, .-_String_equals
	.section	.rodata
	.align 8
.LC7:
	.string	"ERROR: Substring with negative length."
.LC8:
	.string	""
	.align 8
.LC9:
	.string	"ERROR: Substring starting index is too big."
	.align 8
.LC10:
	.string	"ERROR: Substring reached end of string."
	.text
	.globl	_String_substring
	.type	_String_substring, @function
_String_substring:
.LFB25:
	.cfi_startproc
	pushq	%rbp	#
	.cfi_def_cfa_offset 16
	.cfi_offset 6, -16
	movq	%rsp, %rbp	#,
	.cfi_def_cfa_register 6
	subq	$80, %rsp	#,
	movq	%rdi, -72(%rbp)	# str, str
	movl	%esi, -76(%rbp)	# startIndex, startIndex
	movl	%edx, -80(%rbp)	# length, length
# src/Runtime/runtime.c:282:     if (length < 0) {
	cmpl	$0, -80(%rbp)	#, length
	jns	.L81	#,
# src/Runtime/runtime.c:283:         errMsg = "ERROR: Substring with negative length.";
	leaq	.LC7(%rip), %rax	#, tmp102
	movq	%rax, errMsg(%rip)	# tmp102, errMsg
# src/Runtime/runtime.c:284:         error();
	movl	$0, %eax	#,
	call	error	#
.L81:
# src/Runtime/runtime.c:286:     if (length == 0)
	cmpl	$0, -80(%rbp)	#, length
	jne	.L82	#,
# src/Runtime/runtime.c:287:         return __createString("");
	leaq	.LC8(%rip), %rax	#, tmp103
	movq	%rax, %rdi	# tmp103,
	call	__createString	#
	jmp	.L90	#
.L82:
# src/Runtime/runtime.c:288:     if (startIndex >= _String_length(str)) {
	movq	-72(%rbp), %rax	# str, tmp104
	movq	%rax, %rdi	# tmp104,
	call	_String_length	#
# src/Runtime/runtime.c:288:     if (startIndex >= _String_length(str)) {
	cmpl	%eax, -76(%rbp)	# _1, startIndex
	jl	.L84	#,
# src/Runtime/runtime.c:289:         errMsg = "ERROR: Substring starting index is too big.";
	leaq	.LC9(%rip), %rax	#, tmp105
	movq	%rax, errMsg(%rip)	# tmp105, errMsg
# src/Runtime/runtime.c:290:         error();
	movl	$0, %eax	#,
	call	error	#
.L84:
# src/Runtime/runtime.c:292:     uint8_t *rs = ((struct String *)str->data)->data;
	movq	-72(%rbp), %rax	# str, tmp106
	movq	8(%rax), %rax	# str_32(D)->data, _2
# src/Runtime/runtime.c:292:     uint8_t *rs = ((struct String *)str->data)->data;
	movq	8(%rax), %rax	# MEM[(struct String *)_2].data, tmp107
	movq	%rax, -32(%rbp)	# tmp107, rs
# src/Runtime/runtime.c:293:     uint8_t *offset_str = rs;
	movq	-32(%rbp), %rax	# rs, tmp108
	movq	%rax, -8(%rbp)	# tmp108, offset_str
# src/Runtime/runtime.c:295:     while (startIndex-- > 0)
	jmp	.L85	#
.L86:
# src/Runtime/runtime.c:296:         offset_str += u8_next(&character, offset_str) - offset_str;
	movq	-8(%rbp), %rdx	# offset_str, tmp109
	leaq	-60(%rbp), %rax	#, tmp110
	movq	%rdx, %rsi	# tmp109,
	movq	%rax, %rdi	# tmp110,
	call	u8_next@PLT	#
# src/Runtime/runtime.c:296:         offset_str += u8_next(&character, offset_str) - offset_str;
	subq	-8(%rbp), %rax	# offset_str, _60
# src/Runtime/runtime.c:296:         offset_str += u8_next(&character, offset_str) - offset_str;
	addq	%rax, -8(%rbp)	# _4, offset_str
.L85:
# src/Runtime/runtime.c:295:     while (startIndex-- > 0)
	movl	-76(%rbp), %eax	# startIndex, startIndex.111_5
	leal	-1(%rax), %edx	#, tmp111
	movl	%edx, -76(%rbp)	# tmp111, startIndex
# src/Runtime/runtime.c:295:     while (startIndex-- > 0)
	testl	%eax, %eax	# startIndex.111_5
	jg	.L86	#,
# src/Runtime/runtime.c:297:     uint8_t *end = offset_str;
	movq	-8(%rbp), %rax	# offset_str, tmp112
	movq	%rax, -16(%rbp)	# tmp112, end
# src/Runtime/runtime.c:298:     int32_t counter = 0;
	movl	$0, -20(%rbp)	#, counter
# src/Runtime/runtime.c:299:     while (counter < length) {
	jmp	.L87	#
.L89:
# src/Runtime/runtime.c:300:         if (u8_next(&character, end) == NULL) {
	movq	-16(%rbp), %rdx	# end, tmp113
	leaq	-60(%rbp), %rax	#, tmp114
	movq	%rdx, %rsi	# tmp113,
	movq	%rax, %rdi	# tmp114,
	call	u8_next@PLT	#
# src/Runtime/runtime.c:300:         if (u8_next(&character, end) == NULL) {
	testq	%rax, %rax	# _6
	jne	.L88	#,
# src/Runtime/runtime.c:301:             errMsg = "ERROR: Substring reached end of string.";
	leaq	.LC10(%rip), %rax	#, tmp115
	movq	%rax, errMsg(%rip)	# tmp115, errMsg
# src/Runtime/runtime.c:302:             error();
	movl	$0, %eax	#,
	call	error	#
.L88:
# src/Runtime/runtime.c:304:         end += u8_next(&character, end) - end;
	movq	-16(%rbp), %rdx	# end, tmp116
	leaq	-60(%rbp), %rax	#, tmp117
	movq	%rdx, %rsi	# tmp116,
	movq	%rax, %rdi	# tmp117,
	call	u8_next@PLT	#
# src/Runtime/runtime.c:304:         end += u8_next(&character, end) - end;
	subq	-16(%rbp), %rax	# end, _56
# src/Runtime/runtime.c:304:         end += u8_next(&character, end) - end;
	addq	%rax, -16(%rbp)	# _8, end
# src/Runtime/runtime.c:305:         counter++;
	addl	$1, -20(%rbp)	#, counter
.L87:
# src/Runtime/runtime.c:299:     while (counter < length) {
	movl	-20(%rbp), %eax	# counter, tmp118
	cmpl	-80(%rbp), %eax	# length, tmp118
	jl	.L89	#,
# src/Runtime/runtime.c:307:     int32_t bufferSize = end - offset_str + 1;
	movq	-16(%rbp), %rax	# end, tmp119
	subq	-8(%rbp), %rax	# offset_str, _9
# src/Runtime/runtime.c:307:     int32_t bufferSize = end - offset_str + 1;
	addl	$1, %eax	#, _11
# src/Runtime/runtime.c:307:     int32_t bufferSize = end - offset_str + 1;
	movl	%eax, -36(%rbp)	# _11, bufferSize
# src/Runtime/runtime.c:308:     uint8_t *buffer = malloc(bufferSize);
	movl	-36(%rbp), %eax	# bufferSize, tmp120
	cltq
	movq	%rax, %rdi	# _12,
	call	malloc@PLT	#
	movq	%rax, -48(%rbp)	# tmp121, buffer
# src/Runtime/runtime.c:309:     u8_strncpy(buffer, offset_str, bufferSize);
	movl	-36(%rbp), %eax	# bufferSize, tmp122
	movslq	%eax, %rdx	# tmp122, _13
	movq	-8(%rbp), %rcx	# offset_str, tmp123
	movq	-48(%rbp), %rax	# buffer, tmp124
	movq	%rcx, %rsi	# tmp123,
	movq	%rax, %rdi	# tmp124,
	call	u8_strncpy@PLT	#
# src/Runtime/runtime.c:310:     buffer[bufferSize - 1] = 0;
	movl	-36(%rbp), %eax	# bufferSize, tmp125
	cltq
	leaq	-1(%rax), %rdx	#, _15
	movq	-48(%rbp), %rax	# buffer, tmp126
	addq	%rdx, %rax	# _15, _16
# src/Runtime/runtime.c:310:     buffer[bufferSize - 1] = 0;
	movb	$0, (%rax)	#, *_16
# src/Runtime/runtime.c:311:     obj ret = __createString(buffer);
	movq	-48(%rbp), %rax	# buffer, tmp127
	movq	%rax, %rdi	# tmp127,
	call	__createString	#
	movq	%rax, -56(%rbp)	# tmp128, ret
# src/Runtime/runtime.c:312:     __incRef(ret);
	movq	-56(%rbp), %rax	# ret, tmp129
	movq	%rax, %rdi	# tmp129,
	call	__incRef	#
# src/Runtime/runtime.c:313:     free(buffer);
	movq	-48(%rbp), %rax	# buffer, tmp130
	movq	%rax, %rdi	# tmp130,
	call	free@PLT	#
# src/Runtime/runtime.c:314:     return ret;
	movq	-56(%rbp), %rax	# ret, _21
.L90:
# src/Runtime/runtime.c:315: }
	leave	
	.cfi_def_cfa 7, 8
	ret	
	.cfi_endproc
.LFE25:
	.size	_String_substring, .-_String_substring
	.globl	_String_length
	.type	_String_length, @function
_String_length:
.LFB26:
	.cfi_startproc
	pushq	%rbp	#
	.cfi_def_cfa_offset 16
	.cfi_offset 6, -16
	movq	%rsp, %rbp	#,
	.cfi_def_cfa_register 6
	subq	$32, %rsp	#,
	movq	%rdi, -24(%rbp)	# str, str
# src/Runtime/runtime.c:317:     struct String *string = str->data;
	movq	-24(%rbp), %rax	# str, tmp90
	movq	8(%rax), %rax	# str_9(D)->data, tmp91
	movq	%rax, -8(%rbp)	# tmp91, string
# src/Runtime/runtime.c:318:     if (string->length < 0) {
	movq	-8(%rbp), %rax	# string, tmp92
	movl	(%rax), %eax	# string_10->length, _1
# src/Runtime/runtime.c:318:     if (string->length < 0) {
	testl	%eax, %eax	# _1
	jns	.L92	#,
# src/Runtime/runtime.c:319:         string->length = u8_mbsnlen(string->data, u8_strlen(string->data));
	movq	-8(%rbp), %rax	# string, tmp93
	movq	8(%rax), %rax	# string_10->data, _2
# src/Runtime/runtime.c:319:         string->length = u8_mbsnlen(string->data, u8_strlen(string->data));
	movq	%rax, %rdi	# _2,
	call	u8_strlen@PLT	#
	movq	%rax, %rdx	#, _3
# src/Runtime/runtime.c:319:         string->length = u8_mbsnlen(string->data, u8_strlen(string->data));
	movq	-8(%rbp), %rax	# string, tmp94
	movq	8(%rax), %rax	# string_10->data, _4
# src/Runtime/runtime.c:319:         string->length = u8_mbsnlen(string->data, u8_strlen(string->data));
	movq	%rdx, %rsi	# _3,
	movq	%rax, %rdi	# _4,
	call	u8_mbsnlen@PLT	#
# src/Runtime/runtime.c:319:         string->length = u8_mbsnlen(string->data, u8_strlen(string->data));
	movl	%eax, %edx	# _5, _6
	movq	-8(%rbp), %rax	# string, tmp95
	movl	%edx, (%rax)	# _6, string_10->length
.L92:
# src/Runtime/runtime.c:321:     return string->length;
	movq	-8(%rbp), %rax	# string, tmp96
	movl	(%rax), %eax	# string_10->length, _12
# src/Runtime/runtime.c:322: }
	leave	
	.cfi_def_cfa 7, 8
	ret	
	.cfi_endproc
.LFE26:
	.size	_String_length, .-_String_length
	.section	.rodata
	.align 8
.LC11:
	.string	"ERROR: IndexOf null substring argument."
	.align 8
.LC12:
	.string	"ERROR: IndexOf starting index is too big."
	.text
	.globl	_String_indexOf
	.type	_String_indexOf, @function
_String_indexOf:
.LFB27:
	.cfi_startproc
	pushq	%rbp	#
	.cfi_def_cfa_offset 16
	.cfi_offset 6, -16
	movq	%rsp, %rbp	#,
	.cfi_def_cfa_register 6
	pushq	%rbx	#
	subq	$88, %rsp	#,
	.cfi_offset 3, -24
	movq	%rdi, -72(%rbp)	# str, str
	movq	%rsi, -80(%rbp)	# substr, substr
	movl	%edx, -84(%rbp)	# startFrom, startFrom
# src/Runtime/runtime.c:324:     if (substr == NULL) {
	cmpq	$0, -80(%rbp)	#, substr
	jne	.L95	#,
# src/Runtime/runtime.c:325:         errMsg = "ERROR: IndexOf null substring argument.";
	leaq	.LC11(%rip), %rax	#, tmp97
	movq	%rax, errMsg(%rip)	# tmp97, errMsg
# src/Runtime/runtime.c:326:         error();
	movl	$0, %eax	#,
	call	error	#
.L95:
# src/Runtime/runtime.c:328:     if (startFrom >= _String_length(str)) {
	movq	-72(%rbp), %rax	# str, tmp98
	movq	%rax, %rdi	# tmp98,
	call	_String_length	#
# src/Runtime/runtime.c:328:     if (startFrom >= _String_length(str)) {
	cmpl	%eax, -84(%rbp)	# _1, startFrom
	jl	.L96	#,
# src/Runtime/runtime.c:329:         errMsg = "ERROR: IndexOf starting index is too big.";
	leaq	.LC12(%rip), %rax	#, tmp99
	movq	%rax, errMsg(%rip)	# tmp99, errMsg
# src/Runtime/runtime.c:330:         error();
	movl	$0, %eax	#,
	call	error	#
.L96:
# src/Runtime/runtime.c:332:     if (_String_length(str) < _String_length(substr))
	movq	-72(%rbp), %rax	# str, tmp100
	movq	%rax, %rdi	# tmp100,
	call	_String_length	#
	movl	%eax, %ebx	#, _2
# src/Runtime/runtime.c:332:     if (_String_length(str) < _String_length(substr))
	movq	-80(%rbp), %rax	# substr, tmp101
	movq	%rax, %rdi	# tmp101,
	call	_String_length	#
# src/Runtime/runtime.c:332:     if (_String_length(str) < _String_length(substr))
	cmpl	%eax, %ebx	# _3, _2
	jge	.L97	#,
# src/Runtime/runtime.c:333:         return -1;
	movl	$-1, %eax	#, _16
	jmp	.L104	#
.L97:
# src/Runtime/runtime.c:334:     uint8_t *rs = ((struct String *)str->data)->data;
	movq	-72(%rbp), %rax	# str, tmp102
	movq	8(%rax), %rax	# str_26(D)->data, _4
# src/Runtime/runtime.c:334:     uint8_t *rs = ((struct String *)str->data)->data;
	movq	8(%rax), %rax	# MEM[(struct String *)_4].data, tmp103
	movq	%rax, -24(%rbp)	# tmp103, rs
# src/Runtime/runtime.c:335:     uint8_t *rsub = ((struct String *)substr->data)->data;
	movq	-80(%rbp), %rax	# substr, tmp104
	movq	8(%rax), %rax	# substr_22(D)->data, _5
# src/Runtime/runtime.c:335:     uint8_t *rsub = ((struct String *)substr->data)->data;
	movq	8(%rax), %rax	# MEM[(struct String *)_5].data, tmp105
	movq	%rax, -48(%rbp)	# tmp105, rsub
# src/Runtime/runtime.c:336:     uint8_t *start = rs;
	movq	-24(%rbp), %rax	# rs, tmp106
	movq	%rax, -32(%rbp)	# tmp106, start
# src/Runtime/runtime.c:338:     while (startFrom-- > 0) {
	jmp	.L99	#
.L101:
# src/Runtime/runtime.c:339:         if (u8_next(&c, start) == NULL)
	movq	-32(%rbp), %rdx	# start, tmp107
	leaq	-60(%rbp), %rax	#, tmp108
	movq	%rdx, %rsi	# tmp107,
	movq	%rax, %rdi	# tmp108,
	call	u8_next@PLT	#
# src/Runtime/runtime.c:339:         if (u8_next(&c, start) == NULL)
	testq	%rax, %rax	# _6
	jne	.L100	#,
# src/Runtime/runtime.c:340:             return -1;
	movl	$-1, %eax	#, _16
	jmp	.L104	#
.L100:
# src/Runtime/runtime.c:341:         start += u8_next(&c, start) - start;
	movq	-32(%rbp), %rdx	# start, tmp109
	leaq	-60(%rbp), %rax	#, tmp110
	movq	%rdx, %rsi	# tmp109,
	movq	%rax, %rdi	# tmp110,
	call	u8_next@PLT	#
# src/Runtime/runtime.c:341:         start += u8_next(&c, start) - start;
	subq	-32(%rbp), %rax	# start, _46
# src/Runtime/runtime.c:341:         start += u8_next(&c, start) - start;
	addq	%rax, -32(%rbp)	# _8, start
.L99:
# src/Runtime/runtime.c:338:     while (startFrom-- > 0) {
	movl	-84(%rbp), %eax	# startFrom, startFrom.112_9
	leal	-1(%rax), %edx	#, tmp111
	movl	%edx, -84(%rbp)	# tmp111, startFrom
# src/Runtime/runtime.c:338:     while (startFrom-- > 0) {
	testl	%eax, %eax	# startFrom.112_9
	jg	.L101	#,
# src/Runtime/runtime.c:343:     uint8_t *index = u8_strstr(start, rsub);
	movq	-48(%rbp), %rdx	# rsub, tmp112
	movq	-32(%rbp), %rax	# start, tmp113
	movq	%rdx, %rsi	# tmp112,
	movq	%rax, %rdi	# tmp113,
	call	u8_strstr@PLT	#
	movq	%rax, -56(%rbp)	# tmp114, index
# src/Runtime/runtime.c:344:     uint32_t counter = 0;
	movl	$0, -36(%rbp)	#, counter
# src/Runtime/runtime.c:345:     while ((rs += u8_next(&c, rs) - rs) != index)
	jmp	.L102	#
.L103:
# src/Runtime/runtime.c:346:         counter++;
	addl	$1, -36(%rbp)	#, counter
.L102:
# src/Runtime/runtime.c:345:     while ((rs += u8_next(&c, rs) - rs) != index)
	movq	-24(%rbp), %rdx	# rs, tmp115
	leaq	-60(%rbp), %rax	#, tmp116
	movq	%rdx, %rsi	# tmp115,
	movq	%rax, %rdi	# tmp116,
	call	u8_next@PLT	#
# src/Runtime/runtime.c:345:     while ((rs += u8_next(&c, rs) - rs) != index)
	subq	-24(%rbp), %rax	# rs, _40
# src/Runtime/runtime.c:345:     while ((rs += u8_next(&c, rs) - rs) != index)
	addq	%rax, -24(%rbp)	# _11, rs
# src/Runtime/runtime.c:345:     while ((rs += u8_next(&c, rs) - rs) != index)
	movq	-24(%rbp), %rax	# rs, tmp117
	cmpq	-56(%rbp), %rax	# index, tmp117
	jne	.L103	#,
# src/Runtime/runtime.c:347:     return counter;
	movl	-36(%rbp), %eax	# counter, _16
.L104:
# src/Runtime/runtime.c:348: }
	movq	-8(%rbp), %rbx	#,
	leave	
	.cfi_def_cfa 7, 8
	ret	
	.cfi_endproc
.LFE27:
	.size	_String_indexOf, .-_String_indexOf
	.globl	_String_getBytes
	.type	_String_getBytes, @function
_String_getBytes:
.LFB28:
	.cfi_startproc
	pushq	%rbp	#
	.cfi_def_cfa_offset 16
	.cfi_offset 6, -16
	movq	%rsp, %rbp	#,
	.cfi_def_cfa_register 6
	subq	$48, %rsp	#,
	movq	%rdi, -40(%rbp)	# str, str
# src/Runtime/runtime.c:350:     uint8_t *rs = ((struct String *)str->data)->data;
	movq	-40(%rbp), %rax	# str, tmp91
	movq	8(%rax), %rax	# str_9(D)->data, _1
# src/Runtime/runtime.c:350:     uint8_t *rs = ((struct String *)str->data)->data;
	movq	8(%rax), %rax	# MEM[(struct String *)_1].data, tmp92
	movq	%rax, -8(%rbp)	# tmp92, rs
# src/Runtime/runtime.c:351:     int32_t len = rs == NULL ? 0 : u8_strlen(rs);
	cmpq	$0, -8(%rbp)	#, rs
	je	.L106	#,
# src/Runtime/runtime.c:351:     int32_t len = rs == NULL ? 0 : u8_strlen(rs);
	movq	-8(%rbp), %rax	# rs, tmp93
	movq	%rax, %rdi	# tmp93,
	call	u8_strlen@PLT	#
	jmp	.L107	#
.L106:
# src/Runtime/runtime.c:351:     int32_t len = rs == NULL ? 0 : u8_strlen(rs);
	movl	$0, %eax	#, iftmp.113_7
.L107:
# src/Runtime/runtime.c:351:     int32_t len = rs == NULL ? 0 : u8_strlen(rs);
	movl	%eax, -12(%rbp)	# iftmp.113_7, len
# src/Runtime/runtime.c:352:     obj arr = __newByteArray(len + 1);
	movl	-12(%rbp), %eax	# len, tmp94
	addl	$1, %eax	#, _3
	movl	%eax, %edi	# _3,
	call	__newByteArray	#
	movq	%rax, -24(%rbp)	# tmp95, arr
# src/Runtime/runtime.c:353:     memcpy(((struct Array *)arr->data)->elements, rs, len);
	movl	-12(%rbp), %eax	# len, tmp96
	movslq	%eax, %rdx	# tmp96, _4
# src/Runtime/runtime.c:353:     memcpy(((struct Array *)arr->data)->elements, rs, len);
	movq	-24(%rbp), %rax	# arr, tmp97
	movq	8(%rax), %rax	# arr_15->data, _5
# src/Runtime/runtime.c:353:     memcpy(((struct Array *)arr->data)->elements, rs, len);
	movq	8(%rax), %rax	# MEM[(struct Array *)_5].elements, _6
	movq	-8(%rbp), %rcx	# rs, tmp98
	movq	%rcx, %rsi	# tmp98,
	movq	%rax, %rdi	# _6,
	call	memcpy@PLT	#
# src/Runtime/runtime.c:354:     __incRef(arr);
	movq	-24(%rbp), %rax	# arr, tmp99
	movq	%rax, %rdi	# tmp99,
	call	__incRef	#
# src/Runtime/runtime.c:355:     return arr;
	movq	-24(%rbp), %rax	# arr, _18
# src/Runtime/runtime.c:356: }
	leave	
	.cfi_def_cfa 7, 8
	ret	
	.cfi_endproc
.LFE28:
	.size	_String_getBytes, .-_String_getBytes
	.section	.rodata
	.align 8
.LC13:
	.string	"ERROR: EndsWith null substring argument."
	.text
	.globl	_String_endsWith
	.type	_String_endsWith, @function
_String_endsWith:
.LFB29:
	.cfi_startproc
	pushq	%rbp	#
	.cfi_def_cfa_offset 16
	.cfi_offset 6, -16
	movq	%rsp, %rbp	#,
	.cfi_def_cfa_register 6
	subq	$32, %rsp	#,
	movq	%rdi, -24(%rbp)	# str, str
	movq	%rsi, -32(%rbp)	# substr, substr
# src/Runtime/runtime.c:358:     if (substr == NULL) {
	cmpq	$0, -32(%rbp)	#, substr
	jne	.L110	#,
# src/Runtime/runtime.c:359:         errMsg = "ERROR: EndsWith null substring argument.";
	leaq	.LC13(%rip), %rax	#, tmp87
	movq	%rax, errMsg(%rip)	# tmp87, errMsg
# src/Runtime/runtime.c:360:         error();
	movl	$0, %eax	#,
	call	error	#
.L110:
# src/Runtime/runtime.c:362:     uint8_t *rs = ((struct String *)str->data)->data;
	movq	-24(%rbp), %rax	# str, tmp88
	movq	8(%rax), %rax	# str_9(D)->data, _1
# src/Runtime/runtime.c:362:     uint8_t *rs = ((struct String *)str->data)->data;
	movq	8(%rax), %rax	# MEM[(struct String *)_1].data, tmp89
	movq	%rax, -8(%rbp)	# tmp89, rs
# src/Runtime/runtime.c:363:     uint8_t *rsub = ((struct String *)substr->data)->data;
	movq	-32(%rbp), %rax	# substr, tmp90
	movq	8(%rax), %rax	# substr_5(D)->data, _2
# src/Runtime/runtime.c:363:     uint8_t *rsub = ((struct String *)substr->data)->data;
	movq	8(%rax), %rax	# MEM[(struct String *)_2].data, tmp91
	movq	%rax, -16(%rbp)	# tmp91, rsub
# src/Runtime/runtime.c:364:     return u8_endswith(rs, rsub);
	movq	-16(%rbp), %rdx	# rsub, tmp92
	movq	-8(%rbp), %rax	# rs, tmp93
	movq	%rdx, %rsi	# tmp92,
	movq	%rax, %rdi	# tmp93,
	call	u8_endswith@PLT	#
# src/Runtime/runtime.c:365: }
	leave	
	.cfi_def_cfa 7, 8
	ret	
	.cfi_endproc
.LFE29:
	.size	_String_endsWith, .-_String_endsWith
	.section	.rodata
	.align 8
.LC14:
	.string	"ERROR: StartsWith null substring argument."
	.text
	.globl	_String_startsWith
	.type	_String_startsWith, @function
_String_startsWith:
.LFB30:
	.cfi_startproc
	pushq	%rbp	#
	.cfi_def_cfa_offset 16
	.cfi_offset 6, -16
	movq	%rsp, %rbp	#,
	.cfi_def_cfa_register 6
	subq	$32, %rsp	#,
	movq	%rdi, -24(%rbp)	# str, str
	movq	%rsi, -32(%rbp)	# substr, substr
# src/Runtime/runtime.c:367:     if (substr == NULL) {
	cmpq	$0, -32(%rbp)	#, substr
	jne	.L113	#,
# src/Runtime/runtime.c:368:         errMsg = "ERROR: StartsWith null substring argument.";
	leaq	.LC14(%rip), %rax	#, tmp87
	movq	%rax, errMsg(%rip)	# tmp87, errMsg
# src/Runtime/runtime.c:369:         error();
	movl	$0, %eax	#,
	call	error	#
.L113:
# src/Runtime/runtime.c:371:     uint8_t *rs = ((struct String *)str->data)->data;
	movq	-24(%rbp), %rax	# str, tmp88
	movq	8(%rax), %rax	# str_9(D)->data, _1
# src/Runtime/runtime.c:371:     uint8_t *rs = ((struct String *)str->data)->data;
	movq	8(%rax), %rax	# MEM[(struct String *)_1].data, tmp89
	movq	%rax, -8(%rbp)	# tmp89, rs
# src/Runtime/runtime.c:372:     uint8_t *rsub = ((struct String *)substr->data)->data;
	movq	-32(%rbp), %rax	# substr, tmp90
	movq	8(%rax), %rax	# substr_5(D)->data, _2
# src/Runtime/runtime.c:372:     uint8_t *rsub = ((struct String *)substr->data)->data;
	movq	8(%rax), %rax	# MEM[(struct String *)_2].data, tmp91
	movq	%rax, -16(%rbp)	# tmp91, rsub
# src/Runtime/runtime.c:373:     return u8_startswith(rs, rsub);
	movq	-16(%rbp), %rdx	# rsub, tmp92
	movq	-8(%rbp), %rax	# rs, tmp93
	movq	%rdx, %rsi	# tmp92,
	movq	%rax, %rdi	# tmp93,
	call	u8_startswith@PLT	#
# src/Runtime/runtime.c:374: }
	leave	
	.cfi_def_cfa 7, 8
	ret	
	.cfi_endproc
.LFE30:
	.size	_String_startsWith, .-_String_startsWith
	.globl	_String_concat
	.type	_String_concat, @function
_String_concat:
.LFB31:
	.cfi_startproc
	pushq	%rbp	#
	.cfi_def_cfa_offset 16
	.cfi_offset 6, -16
	movq	%rsp, %rbp	#,
	.cfi_def_cfa_register 6
	subq	$64, %rsp	#,
	movq	%rdi, -56(%rbp)	# str, str
	movq	%rsi, -64(%rbp)	# secondstr, secondstr
# src/Runtime/runtime.c:377:     if (secondstr == NULL) {
	cmpq	$0, -64(%rbp)	#, secondstr
	jne	.L116	#,
# src/Runtime/runtime.c:378:         __incRef(str);
	movq	-56(%rbp), %rax	# str, tmp96
	movq	%rax, %rdi	# tmp96,
	call	__incRef	#
# src/Runtime/runtime.c:379:         return str;
	movq	-56(%rbp), %rax	# str, _29
	jmp	.L117	#
.L116:
# src/Runtime/runtime.c:381:     uint8_t *rs1 = ((struct String *)str->data)->data;
	movq	-56(%rbp), %rax	# str, tmp97
	movq	8(%rax), %rax	# str_25(D)->data, _5
# src/Runtime/runtime.c:381:     uint8_t *rs1 = ((struct String *)str->data)->data;
	movq	8(%rax), %rax	# MEM[(struct String *)_5].data, tmp98
	movq	%rax, -32(%rbp)	# tmp98, rs1
# src/Runtime/runtime.c:382:     uint8_t *rs2 = ((struct String *)secondstr->data)->data;
	movq	-64(%rbp), %rax	# secondstr, tmp99
	movq	8(%rax), %rax	# secondstr_31(D)->data, _6
# src/Runtime/runtime.c:382:     uint8_t *rs2 = ((struct String *)secondstr->data)->data;
	movq	8(%rax), %rax	# MEM[(struct String *)_6].data, tmp100
	movq	%rax, -40(%rbp)	# tmp100, rs2
# src/Runtime/runtime.c:384:     int32_t len1 = u8_strlen(rs1);
	movq	-32(%rbp), %rax	# rs1, tmp101
	movq	%rax, %rdi	# tmp101,
	call	u8_strlen@PLT	#
# src/Runtime/runtime.c:384:     int32_t len1 = u8_strlen(rs1);
	movl	%eax, -44(%rbp)	# _11, len1
# src/Runtime/runtime.c:385:     int32_t len2 = u8_strlen(rs2);
	movq	-40(%rbp), %rax	# rs2, tmp102
	movq	%rax, %rdi	# tmp102,
	call	u8_strlen@PLT	#
# src/Runtime/runtime.c:385:     int32_t len2 = u8_strlen(rs2);
	movl	%eax, -12(%rbp)	# _12, len2
# src/Runtime/runtime.c:386:     uint8_t *buffer = malloc(len1 + len2 + 1);
	movl	-44(%rbp), %edx	# len1, tmp103
	movl	-12(%rbp), %eax	# len2, tmp104
	addl	%edx, %eax	# tmp103, _13
# src/Runtime/runtime.c:386:     uint8_t *buffer = malloc(len1 + len2 + 1);
	addl	$1, %eax	#, _14
# src/Runtime/runtime.c:386:     uint8_t *buffer = malloc(len1 + len2 + 1);
	cltq
	movq	%rax, %rdi	# _15,
	call	malloc@PLT	#
	movq	%rax, -24(%rbp)	# tmp105, buffer
# src/Runtime/runtime.c:388:     u8_strcpy(buffer, rs1);
	movq	-32(%rbp), %rdx	# rs1, tmp106
	movq	-24(%rbp), %rax	# buffer, tmp107
	movq	%rdx, %rsi	# tmp106,
	movq	%rax, %rdi	# tmp107,
	call	u8_strcpy@PLT	#
# src/Runtime/runtime.c:389:     u8_strcpy(buffer + len1, rs2);
	movl	-44(%rbp), %eax	# len1, tmp108
	movslq	%eax, %rdx	# tmp108, _20
	movq	-24(%rbp), %rax	# buffer, tmp109
	addq	%rax, %rdx	# tmp109, _21
	movq	-40(%rbp), %rax	# rs2, tmp110
	movq	%rax, %rsi	# tmp110,
	movq	%rdx, %rdi	# _21,
	call	u8_strcpy@PLT	#
# src/Runtime/runtime.c:390:     buffer[len1 + len2] = 0;
	movl	-44(%rbp), %edx	# len1, tmp111
	movl	-12(%rbp), %eax	# len2, tmp112
	addl	%edx, %eax	# tmp111, _22
	movslq	%eax, %rdx	# _22, _23
# src/Runtime/runtime.c:390:     buffer[len1 + len2] = 0;
	movq	-24(%rbp), %rax	# buffer, tmp113
	addq	%rdx, %rax	# _23, _24
# src/Runtime/runtime.c:390:     buffer[len1 + len2] = 0;
	movb	$0, (%rax)	#, *_24
# src/Runtime/runtime.c:392:     obj ret = __createString(buffer);
	movq	-24(%rbp), %rax	# buffer, tmp114
	movq	%rax, %rdi	# tmp114,
	call	__createString	#
	movq	%rax, -8(%rbp)	# tmp115, ret
# src/Runtime/runtime.c:393:     __incRef(ret);
	movq	-8(%rbp), %rax	# ret, tmp116
	movq	%rax, %rdi	# tmp116,
	call	__incRef	#
# src/Runtime/runtime.c:394:     free(buffer);
	movq	-24(%rbp), %rax	# buffer, tmp117
	movq	%rax, %rdi	# tmp117,
	call	free@PLT	#
# src/Runtime/runtime.c:396:     return ret;
	movq	-8(%rbp), %rax	# ret, _29
.L117:
# src/Runtime/runtime.c:397: }
	leave	
	.cfi_def_cfa 7, 8
	ret	
	.cfi_endproc
.LFE31:
	.size	_String_concat, .-_String_concat
	.globl	charAtErr
	.data
	.align 16
	.type	charAtErr, @object
	.size	charAtErr, 25
charAtErr:
	.string	"ERROR: String too short."
	.text
	.globl	_String_charAt
	.type	_String_charAt, @function
_String_charAt:
.LFB32:
	.cfi_startproc
	pushq	%rbp	#
	.cfi_def_cfa_offset 16
	.cfi_offset 6, -16
	movq	%rsp, %rbp	#,
	.cfi_def_cfa_register 6
	subq	$32, %rsp	#,
	movq	%rdi, -24(%rbp)	# str, str
	movl	%esi, -28(%rbp)	# index, index
# src/Runtime/runtime.c:401:     uint8_t *rs = ((struct String *)str->data)->data;
	movq	-24(%rbp), %rax	# str, tmp92
	movq	8(%rax), %rax	# str_14(D)->data, _1
# src/Runtime/runtime.c:401:     uint8_t *rs = ((struct String *)str->data)->data;
	movq	8(%rax), %rax	# MEM[(struct String *)_1].data, tmp93
	movq	%rax, -8(%rbp)	# tmp93, rs
# src/Runtime/runtime.c:403:     while (index-- > 0) {
	jmp	.L119	#
.L121:
# src/Runtime/runtime.c:404:         if (u8_next(&c, rs) == NULL) {
	movq	-8(%rbp), %rdx	# rs, tmp94
	leaq	-12(%rbp), %rax	#, tmp95
	movq	%rdx, %rsi	# tmp94,
	movq	%rax, %rdi	# tmp95,
	call	u8_next@PLT	#
# src/Runtime/runtime.c:404:         if (u8_next(&c, rs) == NULL) {
	testq	%rax, %rax	# _2
	jne	.L120	#,
# src/Runtime/runtime.c:405:             errMsg = charAtErr;
	leaq	charAtErr(%rip), %rax	#, tmp96
	movq	%rax, errMsg(%rip)	# tmp96, errMsg
# src/Runtime/runtime.c:406:             error();
	movl	$0, %eax	#,
	call	error	#
.L120:
# src/Runtime/runtime.c:408:         rs += u8_next(&c, rs) - rs;
	movq	-8(%rbp), %rdx	# rs, tmp97
	leaq	-12(%rbp), %rax	#, tmp98
	movq	%rdx, %rsi	# tmp97,
	movq	%rax, %rdi	# tmp98,
	call	u8_next@PLT	#
# src/Runtime/runtime.c:408:         rs += u8_next(&c, rs) - rs;
	subq	-8(%rbp), %rax	# rs, _27
# src/Runtime/runtime.c:408:         rs += u8_next(&c, rs) - rs;
	addq	%rax, -8(%rbp)	# _4, rs
.L119:
# src/Runtime/runtime.c:403:     while (index-- > 0) {
	movl	-28(%rbp), %eax	# index, index.134_5
	leal	-1(%rax), %edx	#, tmp99
	movl	%edx, -28(%rbp)	# tmp99, index
# src/Runtime/runtime.c:403:     while (index-- > 0) {
	testl	%eax, %eax	# index.134_5
	jg	.L121	#,
# src/Runtime/runtime.c:410:     if (u8_strmbtouc(&c, rs) <= 0) {
	movq	-8(%rbp), %rdx	# rs, tmp100
	leaq	-12(%rbp), %rax	#, tmp101
	movq	%rdx, %rsi	# tmp100,
	movq	%rax, %rdi	# tmp101,
	call	u8_strmbtouc@PLT	#
# src/Runtime/runtime.c:410:     if (u8_strmbtouc(&c, rs) <= 0) {
	testl	%eax, %eax	# _6
	jg	.L122	#,
# src/Runtime/runtime.c:411:         errMsg = charAtErr;
	leaq	charAtErr(%rip), %rax	#, tmp102
	movq	%rax, errMsg(%rip)	# tmp102, errMsg
# src/Runtime/runtime.c:412:         error();
	movl	$0, %eax	#,
	call	error	#
.L122:
# src/Runtime/runtime.c:414:     return c;
	movl	-12(%rbp), %eax	# c, c.135_7
# src/Runtime/runtime.c:415: }
	leave	
	.cfi_def_cfa 7, 8
	ret	
	.cfi_endproc
.LFE32:
	.size	_String_charAt, .-_String_charAt
	.globl	ddd
	.type	ddd, @function
ddd:
.LFB33:
	.cfi_startproc
	pushq	%rbp	#
	.cfi_def_cfa_offset 16
	.cfi_offset 6, -16
	movq	%rsp, %rbp	#,
	.cfi_def_cfa_register 6
	movq	%rdi, -8(%rbp)	# str, str
# src/Runtime/runtime.c:419: }
	nop	
	popq	%rbp	#
	.cfi_def_cfa 7, 8
	ret	
	.cfi_endproc
.LFE33:
	.size	ddd, .-ddd
	.globl	printString
	.type	printString, @function
printString:
.LFB34:
	.cfi_startproc
	pushq	%rbp	#
	.cfi_def_cfa_offset 16
	.cfi_offset 6, -16
	movq	%rsp, %rbp	#,
	.cfi_def_cfa_register 6
	subq	$32, %rsp	#,
	movq	%rdi, -24(%rbp)	# str, str
# src/Runtime/runtime.c:424:     if (str == NULL)
	cmpq	$0, -24(%rbp)	#, str
	jne	.L126	#,
# src/Runtime/runtime.c:425:         str = __createString("null");
	leaq	.LC6(%rip), %rax	#, tmp85
	movq	%rax, %rdi	# tmp85,
	call	__createString	#
	movq	%rax, -24(%rbp)	# tmp86, str
.L126:
# src/Runtime/runtime.c:426:     __incRef(str);
	movq	-24(%rbp), %rax	# str, tmp87
	movq	%rax, %rdi	# tmp87,
	call	__incRef	#
# src/Runtime/runtime.c:427:     uint8_t *rs = ((struct String *)str->data)->data;
	movq	-24(%rbp), %rax	# str, tmp88
	movq	8(%rax), %rax	# str_6->data, _5
# src/Runtime/runtime.c:427:     uint8_t *rs = ((struct String *)str->data)->data;
	movq	8(%rax), %rax	# MEM[(struct String *)_5].data, tmp89
	movq	%rax, -8(%rbp)	# tmp89, rs
# src/Runtime/runtime.c:428:     printf("%s\n", rs);
	movq	-8(%rbp), %rax	# rs, tmp90
	movq	%rax, %rdi	# tmp90,
	call	puts@PLT	#
# src/Runtime/runtime.c:429:     __decRef(str);
	movq	-24(%rbp), %rax	# str, tmp91
	movq	%rax, %rdi	# tmp91,
	call	__decRef	#
# src/Runtime/runtime.c:431:     return 0;
	movl	$0, %eax	#, _12
# src/Runtime/runtime.c:432: }
	leave	
	.cfi_def_cfa 7, 8
	ret	
	.cfi_endproc
.LFE34:
	.size	printString, .-printString
	.section	.rodata
.LC15:
	.string	"%d\n"
	.text
	.globl	printInt
	.type	printInt, @function
printInt:
.LFB35:
	.cfi_startproc
	pushq	%rbp	#
	.cfi_def_cfa_offset 16
	.cfi_offset 6, -16
	movq	%rsp, %rbp	#,
	.cfi_def_cfa_register 6
	subq	$16, %rsp	#,
	movl	%edi, -4(%rbp)	# i, i
# src/Runtime/runtime.c:435:     printf("%d\n", i);
	movl	-4(%rbp), %eax	# i, tmp84
	movl	%eax, %esi	# tmp84,
	leaq	.LC15(%rip), %rax	#, tmp85
	movq	%rax, %rdi	# tmp85,
	movl	$0, %eax	#,
	call	printf@PLT	#
# src/Runtime/runtime.c:437:     return 0;
	movl	$0, %eax	#, _4
# src/Runtime/runtime.c:438: }
	leave	
	.cfi_def_cfa 7, 8
	ret	
	.cfi_endproc
.LFE35:
	.size	printInt, .-printInt
	.section	.rodata
.LC16:
	.string	"true"
.LC17:
	.string	"false"
	.text
	.globl	printBoolean
	.type	printBoolean, @function
printBoolean:
.LFB36:
	.cfi_startproc
	pushq	%rbp	#
	.cfi_def_cfa_offset 16
	.cfi_offset 6, -16
	movq	%rsp, %rbp	#,
	.cfi_def_cfa_register 6
	subq	$16, %rsp	#,
	movl	%edi, %eax	# b, tmp84
	movb	%al, -4(%rbp)	# tmp85, b
# src/Runtime/runtime.c:441:     if (b)
	cmpb	$0, -4(%rbp)	#, b
	je	.L131	#,
# src/Runtime/runtime.c:442:         printf("true\n");
	leaq	.LC16(%rip), %rax	#, tmp86
	movq	%rax, %rdi	# tmp86,
	call	puts@PLT	#
	jmp	.L132	#
.L131:
# src/Runtime/runtime.c:444:         printf("false\n");
	leaq	.LC17(%rip), %rax	#, tmp87
	movq	%rax, %rdi	# tmp87,
	call	puts@PLT	#
.L132:
# src/Runtime/runtime.c:446:     return 0;
	movl	$0, %eax	#, _2
# src/Runtime/runtime.c:447: }
	leave	
	.cfi_def_cfa 7, 8
	ret	
	.cfi_endproc
.LFE36:
	.size	printBoolean, .-printBoolean
	.section	.rodata
.LC18:
	.string	"%d"
	.text
	.globl	intToString
	.type	intToString, @function
intToString:
.LFB37:
	.cfi_startproc
	pushq	%rbp	#
	.cfi_def_cfa_offset 16
	.cfi_offset 6, -16
	movq	%rsp, %rbp	#,
	.cfi_def_cfa_register 6
	subq	$48, %rsp	#,
	movl	%edi, -36(%rbp)	# i, i
# src/Runtime/runtime.c:451:     sprintf(buffer, "%d", i);
	movl	-36(%rbp), %edx	# i, tmp84
	leaq	-19(%rbp), %rax	#, tmp85
	leaq	.LC18(%rip), %rcx	#, tmp86
	movq	%rcx, %rsi	# tmp86,
	movq	%rax, %rdi	# tmp85,
	movl	$0, %eax	#,
	call	sprintf@PLT	#
# src/Runtime/runtime.c:452:     obj ret = __createString(buffer);
	leaq	-19(%rbp), %rax	#, tmp87
	movq	%rax, %rdi	# tmp87,
	call	__createString	#
	movq	%rax, -8(%rbp)	# tmp88, ret
# src/Runtime/runtime.c:453:     __incRef(ret);
	movq	-8(%rbp), %rax	# ret, tmp89
	movq	%rax, %rdi	# tmp89,
	call	__incRef	#
# src/Runtime/runtime.c:455:     return ret;
	movq	-8(%rbp), %rax	# ret, _3
# src/Runtime/runtime.c:456: }
	leave	
	.cfi_def_cfa 7, 8
	ret	
	.cfi_endproc
.LFE37:
	.size	intToString, .-intToString
	.section	.rodata
.LC19:
	.string	"%u"
	.text
	.globl	byteToString
	.type	byteToString, @function
byteToString:
.LFB38:
	.cfi_startproc
	pushq	%rbp	#
	.cfi_def_cfa_offset 16
	.cfi_offset 6, -16
	movq	%rsp, %rbp	#,
	.cfi_def_cfa_register 6
	subq	$48, %rsp	#,
	movl	%edi, %eax	# i, tmp85
	movb	%al, -36(%rbp)	# tmp86, i
# src/Runtime/runtime.c:461:     sprintf(buffer, "%u", i);
	movzbl	-36(%rbp), %edx	# i, _5
	leaq	-19(%rbp), %rax	#, tmp87
	leaq	.LC19(%rip), %rcx	#, tmp88
	movq	%rcx, %rsi	# tmp88,
	movq	%rax, %rdi	# tmp87,
	movl	$0, %eax	#,
	call	sprintf@PLT	#
# src/Runtime/runtime.c:462:     obj ret = __createString(buffer);
	leaq	-19(%rbp), %rax	#, tmp89
	movq	%rax, %rdi	# tmp89,
	call	__createString	#
	movq	%rax, -8(%rbp)	# tmp90, ret
# src/Runtime/runtime.c:463:     __incRef(ret);
	movq	-8(%rbp), %rax	# ret, tmp91
	movq	%rax, %rdi	# tmp91,
	call	__incRef	#
# src/Runtime/runtime.c:465:     return ret;
	movq	-8(%rbp), %rax	# ret, _3
# src/Runtime/runtime.c:466: }
	leave	
	.cfi_def_cfa 7, 8
	ret	
	.cfi_endproc
.LFE38:
	.size	byteToString, .-byteToString
	.globl	boolToString
	.type	boolToString, @function
boolToString:
.LFB39:
	.cfi_startproc
	pushq	%rbp	#
	.cfi_def_cfa_offset 16
	.cfi_offset 6, -16
	movq	%rsp, %rbp	#,
	.cfi_def_cfa_register 6
	subq	$32, %rsp	#,
	movl	%edi, %eax	# b, tmp84
	movb	%al, -20(%rbp)	# tmp85, b
# src/Runtime/runtime.c:471:     if (b)
	cmpb	$0, -20(%rbp)	#, b
	je	.L139	#,
# src/Runtime/runtime.c:472:         ret = __createString("true");
	leaq	.LC16(%rip), %rax	#, tmp86
	movq	%rax, %rdi	# tmp86,
	call	__createString	#
	movq	%rax, -8(%rbp)	# tmp87, ret
	jmp	.L140	#
.L139:
# src/Runtime/runtime.c:474:         ret = __createString("false");
	leaq	.LC17(%rip), %rax	#, tmp88
	movq	%rax, %rdi	# tmp88,
	call	__createString	#
	movq	%rax, -8(%rbp)	# tmp89, ret
.L140:
# src/Runtime/runtime.c:475:     __incRef(ret);
	movq	-8(%rbp), %rax	# ret, tmp90
	movq	%rax, %rdi	# tmp90,
	call	__incRef	#
# src/Runtime/runtime.c:477:     return ret;
	movq	-8(%rbp), %rax	# ret, _10
# src/Runtime/runtime.c:478: }
	leave	
	.cfi_def_cfa 7, 8
	ret	
	.cfi_endproc
.LFE39:
	.size	boolToString, .-boolToString
	.globl	print
	.type	print, @function
print:
.LFB40:
	.cfi_startproc
	pushq	%rbp	#
	.cfi_def_cfa_offset 16
	.cfi_offset 6, -16
	movq	%rsp, %rbp	#,
	.cfi_def_cfa_register 6
	subq	$32, %rsp	#,
	movq	%rdi, -24(%rbp)	# o, o
# src/Runtime/runtime.c:482:     if (o == NULL)
	cmpq	$0, -24(%rbp)	#, o
	jne	.L143	#,
# src/Runtime/runtime.c:483:         o = __createString("null");
	leaq	.LC6(%rip), %rax	#, tmp87
	movq	%rax, %rdi	# tmp87,
	call	__createString	#
	movq	%rax, -24(%rbp)	# tmp88, o
.L143:
# src/Runtime/runtime.c:484:     __incRef(o);
	movq	-24(%rbp), %rax	# o, tmp89
	movq	%rax, %rdi	# tmp89,
	call	__incRef	#
# src/Runtime/runtime.c:485:     obj (*toStr)(obj) = ((void **)o->type->methods)[0];
	movq	-24(%rbp), %rax	# o, tmp90
	movq	(%rax), %rax	# o_12->type, _5
# src/Runtime/runtime.c:485:     obj (*toStr)(obj) = ((void **)o->type->methods)[0];
	movq	12(%rax), %rax	# _5->methods, _6
# src/Runtime/runtime.c:485:     obj (*toStr)(obj) = ((void **)o->type->methods)[0];
	movq	(%rax), %rax	# MEM[(void * *)_6], _7
# src/Runtime/runtime.c:485:     obj (*toStr)(obj) = ((void **)o->type->methods)[0];
	movq	%rax, -16(%rbp)	# _7, toStr
# src/Runtime/runtime.c:486:     obj str = toStr(o);
	movq	-24(%rbp), %rax	# o, tmp91
	movq	-16(%rbp), %rdx	# toStr, tmp92
	movq	%rax, %rdi	# tmp91,
	call	*%rdx	# tmp92
	movq	%rax, -8(%rbp)	# tmp93, str
# src/Runtime/runtime.c:488:     printString(str);
	movq	-8(%rbp), %rax	# str, tmp94
	movq	%rax, %rdi	# tmp94,
	call	printString	#
# src/Runtime/runtime.c:489:     __decRef(str);
	movq	-8(%rbp), %rax	# str, tmp95
	movq	%rax, %rdi	# tmp95,
	call	__decRef	#
# src/Runtime/runtime.c:490:     __decRef(o);
	movq	-24(%rbp), %rax	# o, tmp96
	movq	%rax, %rdi	# tmp96,
	call	__decRef	#
# src/Runtime/runtime.c:492:     return 0;
	movl	$0, %eax	#, _17
# src/Runtime/runtime.c:493: }
	leave	
	.cfi_def_cfa 7, 8
	ret	
	.cfi_endproc
.LFE40:
	.size	print, .-print
	.globl	printBinArray
	.type	printBinArray, @function
printBinArray:
.LFB41:
	.cfi_startproc
	pushq	%rbp	#
	.cfi_def_cfa_offset 16
	.cfi_offset 6, -16
	movq	%rsp, %rbp	#,
	.cfi_def_cfa_register 6
	subq	$32, %rsp	#,
	movq	%rdi, -24(%rbp)	# arr, arr
# src/Runtime/runtime.c:497:     if (arr == NULL){
	cmpq	$0, -24(%rbp)	#, arr
	jne	.L146	#,
# src/Runtime/runtime.c:498:         print(arr);
	movq	-24(%rbp), %rax	# arr, tmp88
	movq	%rax, %rdi	# tmp88,
	call	print	#
# src/Runtime/runtime.c:499:         return 0;
	movl	$0, %eax	#, _9
	jmp	.L147	#
.L146:
# src/Runtime/runtime.c:501:     __incRef(arr);
	movq	-24(%rbp), %rax	# arr, tmp89
	movq	%rax, %rdi	# tmp89,
	call	__incRef	#
# src/Runtime/runtime.c:502:     struct Array *array = arr->data;
	movq	-24(%rbp), %rax	# arr, tmp90
	movq	8(%rax), %rax	# arr_11(D)->data, tmp91
	movq	%rax, -8(%rbp)	# tmp91, array
# src/Runtime/runtime.c:503:     fwrite(array->elements, sizeof(int8_t), array->length, stdout);
	movq	stdout(%rip), %rcx	# stdout, stdout.204_5
# src/Runtime/runtime.c:503:     fwrite(array->elements, sizeof(int8_t), array->length, stdout);
	movq	-8(%rbp), %rax	# array, tmp92
	movl	4(%rax), %eax	# array_2->length, _6
# src/Runtime/runtime.c:503:     fwrite(array->elements, sizeof(int8_t), array->length, stdout);
	movslq	%eax, %rdx	# _6, _7
# src/Runtime/runtime.c:503:     fwrite(array->elements, sizeof(int8_t), array->length, stdout);
	movq	-8(%rbp), %rax	# array, tmp93
	movq	8(%rax), %rax	# array_2->elements, _8
# src/Runtime/runtime.c:503:     fwrite(array->elements, sizeof(int8_t), array->length, stdout);
	movl	$1, %esi	#,
	movq	%rax, %rdi	# _8,
	call	fwrite@PLT	#
# src/Runtime/runtime.c:504:     __decRef(arr);
	movq	-24(%rbp), %rax	# arr, tmp94
	movq	%rax, %rdi	# tmp94,
	call	__decRef	#
# src/Runtime/runtime.c:506:     return 0;
	movl	$0, %eax	#, _9
.L147:
# src/Runtime/runtime.c:507: }
	leave	
	.cfi_def_cfa 7, 8
	ret	
	.cfi_endproc
.LFE41:
	.size	printBinArray, .-printBinArray
	.section	.rodata
.LC20:
	.string	"%s\n"
.LC21:
	.string	"ERROR: User error."
	.text
	.globl	error
	.type	error, @function
error:
.LFB42:
	.cfi_startproc
	pushq	%rbp	#
	.cfi_def_cfa_offset 16
	.cfi_offset 6, -16
	movq	%rsp, %rbp	#,
	.cfi_def_cfa_register 6
# src/Runtime/runtime.c:511:     if (errMsg != NULL)
	movq	errMsg(%rip), %rax	# errMsg, errMsg.213_2
# src/Runtime/runtime.c:511:     if (errMsg != NULL)
	testq	%rax, %rax	# errMsg.213_2
	je	.L149	#,
# src/Runtime/runtime.c:512:         fprintf(stderr, "%s\n", errMsg);
	movq	errMsg(%rip), %rdx	# errMsg, errMsg.214_3
	movq	stderr(%rip), %rax	# stderr, stderr.215_4
	leaq	.LC20(%rip), %rcx	#, tmp87
	movq	%rcx, %rsi	# tmp87,
	movq	%rax, %rdi	# stderr.215_4,
	movl	$0, %eax	#,
	call	fprintf@PLT	#
	jmp	.L150	#
.L149:
# src/Runtime/runtime.c:514:         fprintf(stderr, "%s\n", "ERROR: User error.");
	movq	stderr(%rip), %rax	# stderr, stderr.216_5
	leaq	.LC21(%rip), %rdx	#, tmp88
	leaq	.LC20(%rip), %rcx	#, tmp89
	movq	%rcx, %rsi	# tmp89,
	movq	%rax, %rdi	# stderr.216_5,
	movl	$0, %eax	#,
	call	fprintf@PLT	#
.L150:
# src/Runtime/runtime.c:516:     exit(1);
	movl	$1, %edi	#,
	call	exit@PLT	#
	.cfi_endproc
.LFE42:
	.size	error, .-error
	.section	.rodata
.LC22:
	.string	"%d "
	.text
	.globl	readInt
	.type	readInt, @function
readInt:
.LFB43:
	.cfi_startproc
	pushq	%rbp	#
	.cfi_def_cfa_offset 16
	.cfi_offset 6, -16
	movq	%rsp, %rbp	#,
	.cfi_def_cfa_register 6
	subq	$32, %rsp	#,
# src/Runtime/runtime.c:522:     char *line = NULL;
	movq	$0, -24(%rbp)	#, line
# src/Runtime/runtime.c:523:     size_t size = 0;
	movq	$0, -32(%rbp)	#, size
# src/Runtime/runtime.c:524:     ssize_t unused = getline(&line, &size, stdin);
	movq	stdin(%rip), %rdx	# stdin, stdin.221_1
	leaq	-32(%rbp), %rcx	#, tmp87
	leaq	-24(%rbp), %rax	#, tmp88
	movq	%rcx, %rsi	# tmp87,
	movq	%rax, %rdi	# tmp88,
	call	getline@PLT	#
	movq	%rax, -8(%rbp)	# tmp89, unused
# src/Runtime/runtime.c:525:     int unused2 = sscanf(line, "%d ", &i);
	movq	-24(%rbp), %rax	# line, line.222_2
	leaq	-16(%rbp), %rdx	#, tmp90
	leaq	.LC22(%rip), %rcx	#, tmp91
	movq	%rcx, %rsi	# tmp91,
	movq	%rax, %rdi	# line.222_2,
	movl	$0, %eax	#,
	call	__isoc99_sscanf@PLT	#
	movl	%eax, -12(%rbp)	# tmp92, unused2
# src/Runtime/runtime.c:526:     free(line);
	movq	-24(%rbp), %rax	# line, line.223_3
	movq	%rax, %rdi	# line.223_3,
	call	free@PLT	#
# src/Runtime/runtime.c:527:     return i;
	movl	-16(%rbp), %eax	# i, _12
# src/Runtime/runtime.c:528: }
	leave	
	.cfi_def_cfa 7, 8
	ret	
	.cfi_endproc
.LFE43:
	.size	readInt, .-readInt
	.globl	readString
	.type	readString, @function
readString:
.LFB44:
	.cfi_startproc
	pushq	%rbp	#
	.cfi_def_cfa_offset 16
	.cfi_offset 6, -16
	movq	%rsp, %rbp	#,
	.cfi_def_cfa_register 6
	subq	$32, %rsp	#,
# src/Runtime/runtime.c:530:     char *line = NULL;
	movq	$0, -24(%rbp)	#, line
# src/Runtime/runtime.c:531:     size_t size = 0;
	movq	$0, -32(%rbp)	#, size
# src/Runtime/runtime.c:532:     ssize_t unused = getline(&line, &size, stdin);
	movq	stdin(%rip), %rdx	# stdin, stdin.224_1
	leaq	-32(%rbp), %rcx	#, tmp93
	leaq	-24(%rbp), %rax	#, tmp94
	movq	%rcx, %rsi	# tmp93,
	movq	%rax, %rdi	# tmp94,
	call	getline@PLT	#
	movq	%rax, -8(%rbp)	# tmp95, unused
# src/Runtime/runtime.c:533:     size = u8_strlen(line);
	movq	-24(%rbp), %rax	# line, line.225_2
	movq	%rax, %rdi	# line.225_2,
	call	u8_strlen@PLT	#
# src/Runtime/runtime.c:533:     size = u8_strlen(line);
	movq	%rax, -32(%rbp)	# _3, size
# src/Runtime/runtime.c:534:     line[size - 1] = 0; // remove newline
	movq	-24(%rbp), %rax	# line, line.226_4
	movq	-32(%rbp), %rdx	# size, size.227_5
	subq	$1, %rdx	#, _6
	addq	%rdx, %rax	# _6, _7
# src/Runtime/runtime.c:534:     line[size - 1] = 0; // remove newline
	movb	$0, (%rax)	#, *_7
# src/Runtime/runtime.c:535:     obj l = __createString(line);
	movq	-24(%rbp), %rax	# line, line.228_8
	movq	%rax, %rdi	# line.228_8,
	call	__createString	#
	movq	%rax, -16(%rbp)	# tmp96, l
# src/Runtime/runtime.c:536:     __incRef(l);
	movq	-16(%rbp), %rax	# l, tmp97
	movq	%rax, %rdi	# tmp97,
	call	__incRef	#
# src/Runtime/runtime.c:537:     free(line);
	movq	-24(%rbp), %rax	# line, line.229_9
	movq	%rax, %rdi	# line.229_9,
	call	free@PLT	#
# src/Runtime/runtime.c:538:     return l;
	movq	-16(%rbp), %rax	# l, _21
# src/Runtime/runtime.c:539: }
	leave	
	.cfi_def_cfa 7, 8
	ret	
	.cfi_endproc
.LFE44:
	.size	readString, .-readString
	.ident	"GCC: (Debian 12.2.0-14) 12.2.0"
	.section	.note.GNU-stack,"",@progbits
