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
# src/Runtime/runtime.c:31:     obj r = malloc(sizeof(struct Reference)+t->dataSize);
	movq	-24(%rbp), %rax	# t, tmp93
	movl	8(%rax), %eax	# t_14(D)->dataSize, _1
	cltq
# src/Runtime/runtime.c:31:     obj r = malloc(sizeof(struct Reference)+t->dataSize);
	addq	$36, %rax	#, _3
	movq	%rax, %rdi	# _3,
	call	malloc@PLT	#
	movq	%rax, -8(%rbp)	# tmp94, r
# src/Runtime/runtime.c:33:     r->type = t;
	movq	-8(%rbp), %rax	# r, tmp95
	movq	-24(%rbp), %rdx	# t, tmp96
	movq	%rdx, (%rax)	# tmp96, r_16->type
# src/Runtime/runtime.c:34:     r->counter = 0;
	movq	-8(%rbp), %rax	# r, tmp97
	movl	$0, 16(%rax)	#, r_16->counter
# src/Runtime/runtime.c:35:     r->data = 0;
	movq	-8(%rbp), %rax	# r, tmp98
	movq	$0, 8(%rax)	#, r_16->data
# src/Runtime/runtime.c:37:     if (t->dataSize > 0) {
	movq	-24(%rbp), %rax	# t, tmp99
	movl	8(%rax), %eax	# t_14(D)->dataSize, _6
# src/Runtime/runtime.c:37:     if (t->dataSize > 0) {
	testl	%eax, %eax	# _6
	jle	.L2	#,
# src/Runtime/runtime.c:38:         bzero((r+36), t->dataSize);
	movq	-24(%rbp), %rax	# t, tmp100
	movl	8(%rax), %eax	# t_14(D)->dataSize, _7
# src/Runtime/runtime.c:38:         bzero((r+36), t->dataSize);
	cltq
# src/Runtime/runtime.c:38:         bzero((r+36), t->dataSize);
	movq	-8(%rbp), %rdx	# r, tmp101
	addq	$1296, %rdx	#, _9
# src/Runtime/runtime.c:38:         bzero((r+36), t->dataSize);
	movq	%rdx, %rcx	# _9, tmp102
	movq	%rax, %rdx	# tmp103,
	movl	$0, %esi	#,
	movq	%rcx, %rdi	# tmp102,
	call	memset@PLT	#
.L2:
# src/Runtime/runtime.c:48:     r->methods = r->type->methods;
	movq	-8(%rbp), %rax	# r, tmp106
	movq	(%rax), %rax	# r_16->type, _10
# src/Runtime/runtime.c:48:     r->methods = r->type->methods;
	movq	12(%rax), %rdx	# _10->methods, _11
# src/Runtime/runtime.c:48:     r->methods = r->type->methods;
	movq	-8(%rbp), %rax	# r, tmp107
	movq	%rdx, 20(%rax)	# _11, r_16->methods
# src/Runtime/runtime.c:50:     return r;
	movq	-8(%rbp), %rax	# r, _5
# src/Runtime/runtime.c:51: }
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
# src/Runtime/runtime.c:55:     if (r->type == &_class_Array) {
	movq	-40(%rbp), %rax	# r, tmp92
	movq	(%rax), %rdx	# r_17(D)->type, _5
# src/Runtime/runtime.c:55:     if (r->type == &_class_Array) {
	leaq	_class_Array(%rip), %rax	#, tmp93
	cmpq	%rax, %rdx	# tmp93, _5
	jne	.L5	#,
# src/Runtime/runtime.c:57:         void **els = r->data;
	movq	-40(%rbp), %rax	# r, tmp94
	movq	8(%rax), %rax	# r_17(D)->data, tmp95
	movq	%rax, -24(%rbp)	# tmp95, els
# src/Runtime/runtime.c:58:         if (r->elementSize == sizeof(void *)) {
	movq	-40(%rbp), %rax	# r, tmp96
	movl	28(%rax), %eax	# r_17(D)->elementSize, _6
# src/Runtime/runtime.c:58:         if (r->elementSize == sizeof(void *)) {
	cmpl	$8, %eax	#, _6
	jne	.L6	#,
# src/Runtime/runtime.c:59:             for (int i = 0; i < r->length; i++)
	movl	$0, -4(%rbp)	#, i
# src/Runtime/runtime.c:59:             for (int i = 0; i < r->length; i++)
	jmp	.L7	#
.L8:
# src/Runtime/runtime.c:60:                 __decRef(els[i]);
	movl	-4(%rbp), %eax	# i, tmp97
	cltq
	leaq	0(,%rax,8), %rdx	#, _8
	movq	-24(%rbp), %rax	# els, tmp98
	addq	%rdx, %rax	# _8, _9
# src/Runtime/runtime.c:60:                 __decRef(els[i]);
	movq	(%rax), %rax	# *_9, _10
	movq	%rax, %rdi	# _10,
	call	__decRef	#
# src/Runtime/runtime.c:59:             for (int i = 0; i < r->length; i++)
	addl	$1, -4(%rbp)	#, i
.L7:
# src/Runtime/runtime.c:59:             for (int i = 0; i < r->length; i++)
	movq	-40(%rbp), %rax	# r, tmp99
	movl	32(%rax), %eax	# r_17(D)->length, _11
# src/Runtime/runtime.c:59:             for (int i = 0; i < r->length; i++)
	cmpl	%eax, -4(%rbp)	# _11, i
	jl	.L8	#,
.L6:
# src/Runtime/runtime.c:62:         if (els != NULL)
	cmpq	$0, -24(%rbp)	#, els
	je	.L9	#,
# src/Runtime/runtime.c:63:             free(els);
	movq	-24(%rbp), %rax	# els, tmp100
	movq	%rax, %rdi	# tmp100,
	call	free@PLT	#
	jmp	.L9	#
.L5:
# src/Runtime/runtime.c:64:     } else if (r->type == &_class_String) {
	movq	-40(%rbp), %rax	# r, tmp101
	movq	(%rax), %rdx	# r_17(D)->type, _12
# src/Runtime/runtime.c:64:     } else if (r->type == &_class_String) {
	leaq	_class_String(%rip), %rax	#, tmp102
	cmpq	%rax, %rdx	# tmp102, _12
	jne	.L9	#,
# src/Runtime/runtime.c:65:         void *els = (r->data);
	movq	-40(%rbp), %rax	# r, tmp103
	movq	8(%rax), %rax	# r_17(D)->data, tmp104
	movq	%rax, -16(%rbp)	# tmp104, els
# src/Runtime/runtime.c:66:         if (els != NULL && els != emptyString)
	cmpq	$0, -16(%rbp)	#, els
	je	.L9	#,
# src/Runtime/runtime.c:66:         if (els != NULL && els != emptyString)
	leaq	emptyString(%rip), %rax	#, tmp105
	cmpq	%rax, -16(%rbp)	# tmp105, els
	je	.L9	#,
# src/Runtime/runtime.c:67:             free(els);
	movq	-16(%rbp), %rax	# els, tmp106
	movq	%rax, %rdi	# tmp106,
	call	free@PLT	#
.L9:
# src/Runtime/runtime.c:69:     if (r->data != NULL)
	movq	-40(%rbp), %rax	# r, tmp107
	movq	8(%rax), %rax	# r_17(D)->data, _13
# src/Runtime/runtime.c:69:     if (r->data != NULL)
	testq	%rax, %rax	# _13
	je	.L10	#,
# src/Runtime/runtime.c:70:         free(r->data);
	movq	-40(%rbp), %rax	# r, tmp108
	movq	8(%rax), %rax	# r_17(D)->data, _14
	movq	%rax, %rdi	# _14,
	call	free@PLT	#
.L10:
# src/Runtime/runtime.c:71:     free(r);
	movq	-40(%rbp), %rax	# r, tmp109
	movq	%rax, %rdi	# tmp109,
	call	free@PLT	#
# src/Runtime/runtime.c:72: }
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
# src/Runtime/runtime.c:75:     if (!__LATTE_RUNTIME_GC_ENABLED) return;
	nop	
# src/Runtime/runtime.c:81: }
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
	movq	%rdi, -8(%rbp)	# r, r
# src/Runtime/runtime.c:84:     if (!__LATTE_RUNTIME_GC_ENABLED) return;
	nop	
# src/Runtime/runtime.c:96: }
	popq	%rbp	#
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
# src/Runtime/runtime.c:98: obj __newRefArray(int32_t length) { return __newArray(sizeof(obj), length); }
	movl	-4(%rbp), %eax	# length, tmp84
	movl	%eax, %esi	# tmp84,
	movl	$8, %edi	#,
	call	__newArray	#
# src/Runtime/runtime.c:98: obj __newRefArray(int32_t length) { return __newArray(sizeof(obj), length); }
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
# src/Runtime/runtime.c:100:     return __newArray(sizeof(int32_t), length);
	movl	-4(%rbp), %eax	# length, tmp84
	movl	%eax, %esi	# tmp84,
	movl	$4, %edi	#,
	call	__newArray	#
# src/Runtime/runtime.c:101: }
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
# src/Runtime/runtime.c:103:     return __newArray(sizeof(int8_t), length);
	movl	-4(%rbp), %eax	# length, tmp84
	movl	%eax, %esi	# tmp84,
	movl	$1, %edi	#,
	call	__newArray	#
# src/Runtime/runtime.c:104: }
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
# src/Runtime/runtime.c:106:     obj r = __new(&_class_Array);
	leaq	_class_Array(%rip), %rax	#, tmp88
	movq	%rax, %rdi	# tmp88,
	call	__new	#
	movq	%rax, -8(%rbp)	# tmp89, r
# src/Runtime/runtime.c:107:     void* arr = malloc(size * length);
	movl	-20(%rbp), %eax	# size, tmp90
	imull	-24(%rbp), %eax	# length, _1
# src/Runtime/runtime.c:107:     void* arr = malloc(size * length);
	cltq
	movq	%rax, %rdi	# _2,
	call	malloc@PLT	#
	movq	%rax, -16(%rbp)	# tmp91, arr
# src/Runtime/runtime.c:108:     r->data = arr;
	movq	-8(%rbp), %rax	# r, tmp92
	movq	-16(%rbp), %rdx	# arr, tmp93
	movq	%rdx, 8(%rax)	# tmp93, r_8->data
# src/Runtime/runtime.c:109:     r->elementSize = size;
	movq	-8(%rbp), %rax	# r, tmp94
	movl	-20(%rbp), %edx	# size, tmp95
	movl	%edx, 28(%rax)	# tmp95, r_8->elementSize
# src/Runtime/runtime.c:110:     r->length = length;
	movq	-8(%rbp), %rax	# r, tmp96
	movl	-24(%rbp), %edx	# length, tmp97
	movl	%edx, 32(%rax)	# tmp97, r_8->length
# src/Runtime/runtime.c:111:     if (length > 0) {
	cmpl	$0, -24(%rbp)	#, length
	jle	.L22	#,
# src/Runtime/runtime.c:113:         bzero(arr, size * length);
	movl	-20(%rbp), %eax	# size, tmp98
	imull	-24(%rbp), %eax	# length, _3
# src/Runtime/runtime.c:113:         bzero(arr, size * length);
	cltq
	movq	-16(%rbp), %rdx	# arr, tmp99
	movq	%rdx, %rcx	# tmp99, tmp100
	movq	%rax, %rdx	# tmp101,
	movl	$0, %esi	#,
	movq	%rcx, %rdi	# tmp100,
	call	memset@PLT	#
.L22:
# src/Runtime/runtime.c:115:     return r;
	movq	-8(%rbp), %rax	# r, _17
# src/Runtime/runtime.c:116: }
	leave	
	.cfi_def_cfa 7, 8
	ret	
	.cfi_endproc
.LFE13:
	.size	__newArray, .-__newArray
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
	movq	%rdi, -8(%rbp)	# array, array
	movl	%esi, -12(%rbp)	# index, index
# src/Runtime/runtime.c:130:     return NULL;
	movl	$0, %eax	#, _1
# src/Runtime/runtime.c:131: }
	popq	%rbp	#
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
# src/Runtime/runtime.c:136:     if (o == NULL) {
	cmpq	$0, -24(%rbp)	#, o
	jne	.L27	#,
# src/Runtime/runtime.c:138:         return NULL;
	movl	$0, %eax	#, _3
	jmp	.L28	#
.L27:
# src/Runtime/runtime.c:141:     struct Type *to = o->type;
	movq	-24(%rbp), %rax	# o, tmp84
	movq	(%rax), %rax	# o_10(D)->type, tmp85
	movq	%rax, -8(%rbp)	# tmp85, to
# src/Runtime/runtime.c:142:     while (to != NULL) {
	jmp	.L29	#
.L32:
# src/Runtime/runtime.c:144:         if (t == to) {
	movq	-32(%rbp), %rax	# t, tmp86
	cmpq	-8(%rbp), %rax	# to, tmp86
	jne	.L30	#,
# src/Runtime/runtime.c:146:             return o;
	movq	-24(%rbp), %rax	# o, _3
	jmp	.L28	#
.L30:
# src/Runtime/runtime.c:148:         struct Type *prev = to;
	movq	-8(%rbp), %rax	# to, tmp87
	movq	%rax, -16(%rbp)	# tmp87, prev
# src/Runtime/runtime.c:149:         to = to->parent;
	movq	-8(%rbp), %rax	# to, tmp88
	movq	(%rax), %rax	# to_8->parent, tmp89
	movq	%rax, -8(%rbp)	# tmp89, to
# src/Runtime/runtime.c:150:         if (prev == to) {
	movq	-16(%rbp), %rax	# prev, tmp90
	cmpq	-8(%rbp), %rax	# to, tmp90
	je	.L34	#,
.L29:
# src/Runtime/runtime.c:142:     while (to != NULL) {
	cmpq	$0, -8(%rbp)	#, to
	jne	.L32	#,
	jmp	.L33	#
.L34:
# src/Runtime/runtime.c:152:             break;
	nop	
.L33:
# src/Runtime/runtime.c:156:     return NULL;
	movl	$0, %eax	#, _3
.L28:
# src/Runtime/runtime.c:157: }
	popq	%rbp	#
	.cfi_def_cfa 7, 8
	ret	
	.cfi_endproc
.LFE15:
	.size	__cast, .-__cast
	.section	.rodata
	.align 8
.LC0:
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
# src/Runtime/runtime.c:160:     errMsg = "ERROR: Null pointer reference.";
	leaq	.LC0(%rip), %rax	#, tmp82
	movq	%rax, errMsg(%rip)	# tmp82, errMsg
# src/Runtime/runtime.c:161:     error();
	movl	$0, %eax	#,
	call	error	#
# src/Runtime/runtime.c:162: }
	nop	
	popq	%rbp	#
	.cfi_def_cfa 7, 8
	ret	
	.cfi_endproc
.LFE16:
	.size	__errorNull, .-__errorNull
	.section	.rodata
	.align 8
.LC1:
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
# src/Runtime/runtime.c:167:     if (c == NULL) {
	cmpq	$0, -40(%rbp)	#, c
	jne	.L37	#,
# src/Runtime/runtime.c:169:         return __createString(emptyString);
	leaq	emptyString(%rip), %rax	#, tmp97
	movq	%rax, %rdi	# tmp97,
	call	__createString	#
	jmp	.L38	#
.L37:
# src/Runtime/runtime.c:172:     obj r = __new(&_class_String);
	leaq	_class_String(%rip), %rax	#, tmp98
	movq	%rax, %rdi	# tmp98,
	call	__new	#
	movq	%rax, -32(%rbp)	# tmp99, r
# src/Runtime/runtime.c:175:     r->length = u8_strlen(c);
	movq	-40(%rbp), %rax	# c, tmp100
	movq	%rax, %rdi	# tmp100,
	call	u8_strlen@PLT	#
# src/Runtime/runtime.c:175:     r->length = u8_strlen(c);
	movl	%eax, %edx	# _3, _4
	movq	-32(%rbp), %rax	# r, tmp101
	movl	%edx, 32(%rax)	# _4, r_35->length
# src/Runtime/runtime.c:177:     uint8_t* invalid_unit = u8_check(c, r->length);
	movq	-32(%rbp), %rax	# r, tmp102
	movl	32(%rax), %eax	# r_35->length, _10
# src/Runtime/runtime.c:177:     uint8_t* invalid_unit = u8_check(c, r->length);
	movslq	%eax, %rdx	# _10, _11
	movq	-40(%rbp), %rax	# c, tmp103
	movq	%rdx, %rsi	# _11,
	movq	%rax, %rdi	# tmp103,
	call	u8_check@PLT	#
	movq	%rax, -16(%rbp)	# tmp104, invalid_unit
# src/Runtime/runtime.c:178:     if (u8_check(c, r->length) != NULL) {
	movq	-32(%rbp), %rax	# r, tmp105
	movl	32(%rax), %eax	# r_35->length, _12
# src/Runtime/runtime.c:178:     if (u8_check(c, r->length) != NULL) {
	movslq	%eax, %rdx	# _12, _13
	movq	-40(%rbp), %rax	# c, tmp106
	movq	%rdx, %rsi	# _13,
	movq	%rax, %rdi	# tmp106,
	call	u8_check@PLT	#
# src/Runtime/runtime.c:178:     if (u8_check(c, r->length) != NULL) {
	testq	%rax, %rax	# _14
	je	.L39	#,
# src/Runtime/runtime.c:180:         errMsg = "ERROR: Non-unicode string encoding.";
	leaq	.LC1(%rip), %rax	#, tmp107
	movq	%rax, errMsg(%rip)	# tmp107, errMsg
# src/Runtime/runtime.c:181:         error();
	movl	$0, %eax	#,
	call	error	#
.L39:
# src/Runtime/runtime.c:183:     if (r->length > 0) {
	movq	-32(%rbp), %rax	# r, tmp108
	movl	32(%rax), %eax	# r_35->length, _23
# src/Runtime/runtime.c:183:     if (r->length > 0) {
	testl	%eax, %eax	# _23
	jle	.L40	#,
# src/Runtime/runtime.c:184:         int len = r->length;
	movq	-32(%rbp), %rax	# r, tmp109
	movl	32(%rax), %eax	# r_35->length, tmp110
	movl	%eax, -20(%rbp)	# tmp110, len
# src/Runtime/runtime.c:185:         uint8_t *str = malloc(len + 1);
	movl	-20(%rbp), %eax	# len, tmp111
	addl	$1, %eax	#, _24
# src/Runtime/runtime.c:185:         uint8_t *str = malloc(len + 1);
	cltq
	movq	%rax, %rdi	# _25,
	call	malloc@PLT	#
	movq	%rax, -8(%rbp)	# tmp112, str
# src/Runtime/runtime.c:186:         r->data = str;
	movq	-32(%rbp), %rax	# r, tmp113
	movq	-8(%rbp), %rdx	# str, tmp114
	movq	%rdx, 8(%rax)	# tmp114, r_35->data
# src/Runtime/runtime.c:187:         memcpy(str, c, len);
	movl	-20(%rbp), %eax	# len, tmp115
	movslq	%eax, %rdx	# tmp115, _26
	movq	-40(%rbp), %rcx	# c, tmp116
	movq	-8(%rbp), %rax	# str, tmp117
	movq	%rcx, %rsi	# tmp116,
	movq	%rax, %rdi	# tmp117,
	call	memcpy@PLT	#
# src/Runtime/runtime.c:188:         str[len] = 0;
	movl	-20(%rbp), %eax	# len, tmp118
	movslq	%eax, %rdx	# tmp118, _27
	movq	-8(%rbp), %rax	# str, tmp119
	addq	%rdx, %rax	# _27, _28
# src/Runtime/runtime.c:188:         str[len] = 0;
	movb	$0, (%rax)	#, *_28
# src/Runtime/runtime.c:194:     return r;
	movq	-32(%rbp), %rax	# r, _2
	jmp	.L38	#
.L40:
# src/Runtime/runtime.c:190:         r->data = emptyString;
	movq	-32(%rbp), %rax	# r, tmp120
	leaq	emptyString(%rip), %rdx	#, tmp121
	movq	%rdx, 8(%rax)	# tmp121, r_35->data
# src/Runtime/runtime.c:191:         return r;
	movq	-32(%rbp), %rax	# r, _2
.L38:
# src/Runtime/runtime.c:195: }
	leave	
	.cfi_def_cfa 7, 8
	ret	
	.cfi_endproc
.LFE17:
	.size	__createString, .-__createString
	.section	.rodata
.LC2:
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
# src/Runtime/runtime.c:199:     obj ret = __createString("Object");
	leaq	.LC2(%rip), %rax	#, tmp84
	movq	%rax, %rdi	# tmp84,
	call	__createString	#
	movq	%rax, -8(%rbp)	# tmp85, ret
# src/Runtime/runtime.c:200:     __incRef(ret);
	movq	-8(%rbp), %rax	# ret, tmp86
	movq	%rax, %rdi	# tmp86,
	call	__incRef	#
# src/Runtime/runtime.c:201:     return ret;
	movq	-8(%rbp), %rax	# ret, _5
# src/Runtime/runtime.c:202: }
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
# src/Runtime/runtime.c:203: int32_t _Object_getHashCode(obj o) { return (int32_t)(int64_t)o; }
	movq	-8(%rbp), %rax	# o, o.114_1
# src/Runtime/runtime.c:203: int32_t _Object_getHashCode(obj o) { return (int32_t)(int64_t)o; }
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
# src/Runtime/runtime.c:204: int8_t _Object_equals(obj o1, obj o2) { return o1 == o2; }
	movq	-8(%rbp), %rax	# o1, tmp85
	cmpq	-16(%rbp), %rax	# o2, tmp85
	sete	%al	#, _1
# src/Runtime/runtime.c:204: int8_t _Object_equals(obj o1, obj o2) { return o1 == o2; }
	popq	%rbp	#
	.cfi_def_cfa 7, 8
	ret	
	.cfi_endproc
.LFE20:
	.size	_Object_equals, .-_Object_equals
	.section	.rodata
.LC3:
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
# src/Runtime/runtime.c:207:     char start[] = "[";
	movw	$91, -114(%rbp)	#, start
# src/Runtime/runtime.c:208:     char delim[] = ", ";
	movw	$8236, -117(%rbp)	#, delim
	movb	$0, -115(%rbp)	#, delim
# src/Runtime/runtime.c:209:     char end[] = "]";
	movw	$93, -119(%rbp)	#, end
# src/Runtime/runtime.c:211:     obj *strings = malloc(sizeof(obj) * arr->length);
	movq	-136(%rbp), %rax	# arr, tmp186
	movl	32(%rax), %eax	# arr_116(D)->length, _1
	cltq
# src/Runtime/runtime.c:211:     obj *strings = malloc(sizeof(obj) * arr->length);
	salq	$3, %rax	#, _3
	movq	%rax, %rdi	# _3,
	call	malloc@PLT	#
	movq	%rax, -40(%rbp)	# tmp187, strings
# src/Runtime/runtime.c:212:     int32_t *lenghts = malloc(sizeof(int32_t) * arr->length);
	movq	-136(%rbp), %rax	# arr, tmp188
	movl	32(%rax), %eax	# arr_116(D)->length, _4
	cltq
# src/Runtime/runtime.c:212:     int32_t *lenghts = malloc(sizeof(int32_t) * arr->length);
	salq	$2, %rax	#, _6
	movq	%rax, %rdi	# _6,
	call	malloc@PLT	#
	movq	%rax, -48(%rbp)	# tmp189, lenghts
# src/Runtime/runtime.c:213:     int32_t totalLenght = 0;
	movl	$0, -20(%rbp)	#, totalLenght
# src/Runtime/runtime.c:215:     for (int i = 0; i < arr->length; i++) {
	movl	$0, -24(%rbp)	#, i
# src/Runtime/runtime.c:215:     for (int i = 0; i < arr->length; i++) {
	jmp	.L49	#
.L54:
# src/Runtime/runtime.c:216:         if (arr->elementSize == sizeof(int32_t)) {
	movq	-136(%rbp), %rax	# arr, tmp190
	movl	28(%rax), %eax	# arr_116(D)->elementSize, _7
# src/Runtime/runtime.c:216:         if (arr->elementSize == sizeof(int32_t)) {
	cmpl	$4, %eax	#, _7
	jne	.L50	#,
# src/Runtime/runtime.c:217:             int32_t *elements = arr->data;
	movq	-136(%rbp), %rax	# arr, tmp191
	movq	8(%rax), %rax	# arr_116(D)->data, tmp192
	movq	%rax, -112(%rbp)	# tmp192, elements
# src/Runtime/runtime.c:218:             strings[i] = intToString(elements[i]);
	movl	-24(%rbp), %eax	# i, tmp193
	cltq
	leaq	0(,%rax,4), %rdx	#, _9
	movq	-112(%rbp), %rax	# elements, tmp194
	addq	%rdx, %rax	# _9, _10
# src/Runtime/runtime.c:218:             strings[i] = intToString(elements[i]);
	movl	(%rax), %eax	# *_10, _11
# src/Runtime/runtime.c:218:             strings[i] = intToString(elements[i]);
	movl	-24(%rbp), %edx	# i, tmp195
	movslq	%edx, %rdx	# tmp195, _12
	leaq	0(,%rdx,8), %rcx	#, _13
	movq	-40(%rbp), %rdx	# strings, tmp196
	leaq	(%rcx,%rdx), %rbx	#, _14
# src/Runtime/runtime.c:218:             strings[i] = intToString(elements[i]);
	movl	%eax, %edi	# _11,
	call	intToString	#
# src/Runtime/runtime.c:218:             strings[i] = intToString(elements[i]);
	movq	%rax, (%rbx)	# _15, *_14
	jmp	.L51	#
.L50:
# src/Runtime/runtime.c:219:         } else if (arr->elementSize == sizeof(int8_t)) {
	movq	-136(%rbp), %rax	# arr, tmp197
	movl	28(%rax), %eax	# arr_116(D)->elementSize, _16
# src/Runtime/runtime.c:219:         } else if (arr->elementSize == sizeof(int8_t)) {
	cmpl	$1, %eax	#, _16
	jne	.L52	#,
# src/Runtime/runtime.c:220:             int8_t *elements = arr->data;
	movq	-136(%rbp), %rax	# arr, tmp198
	movq	8(%rax), %rax	# arr_116(D)->data, tmp199
	movq	%rax, -104(%rbp)	# tmp199, elements
# src/Runtime/runtime.c:221:             strings[i] = byteToString(elements[i]);
	movl	-24(%rbp), %eax	# i, tmp200
	movslq	%eax, %rdx	# tmp200, _17
	movq	-104(%rbp), %rax	# elements, tmp201
	addq	%rdx, %rax	# _17, _18
	movzbl	(%rax), %eax	# *_18, _19
# src/Runtime/runtime.c:221:             strings[i] = byteToString(elements[i]);
	movzbl	%al, %eax	# _20, _21
# src/Runtime/runtime.c:221:             strings[i] = byteToString(elements[i]);
	movl	-24(%rbp), %edx	# i, tmp202
	movslq	%edx, %rdx	# tmp202, _22
	leaq	0(,%rdx,8), %rcx	#, _23
	movq	-40(%rbp), %rdx	# strings, tmp203
	leaq	(%rcx,%rdx), %rbx	#, _24
# src/Runtime/runtime.c:221:             strings[i] = byteToString(elements[i]);
	movl	%eax, %edi	# _21,
	call	byteToString	#
# src/Runtime/runtime.c:221:             strings[i] = byteToString(elements[i]);
	movq	%rax, (%rbx)	# _25, *_24
	jmp	.L51	#
.L52:
# src/Runtime/runtime.c:223:             obj *elements = arr->data;
	movq	-136(%rbp), %rax	# arr, tmp204
	movq	8(%rax), %rax	# arr_116(D)->data, tmp205
	movq	%rax, -80(%rbp)	# tmp205, elements
# src/Runtime/runtime.c:224:             obj element = elements[i];
	movl	-24(%rbp), %eax	# i, tmp206
	cltq
	leaq	0(,%rax,8), %rdx	#, _27
	movq	-80(%rbp), %rax	# elements, tmp207
	addq	%rdx, %rax	# _27, _28
# src/Runtime/runtime.c:224:             obj element = elements[i];
	movq	(%rax), %rax	# *_28, tmp208
	movq	%rax, -88(%rbp)	# tmp208, element
# src/Runtime/runtime.c:225:             if (element == NULL) {
	cmpq	$0, -88(%rbp)	#, element
	jne	.L53	#,
# src/Runtime/runtime.c:226:                 strings[i] = __createString("null");
	movl	-24(%rbp), %eax	# i, tmp209
	cltq
	leaq	0(,%rax,8), %rdx	#, _30
	movq	-40(%rbp), %rax	# strings, tmp210
	leaq	(%rdx,%rax), %rbx	#, _31
# src/Runtime/runtime.c:226:                 strings[i] = __createString("null");
	leaq	.LC3(%rip), %rax	#, tmp211
	movq	%rax, %rdi	# tmp211,
	call	__createString	#
# src/Runtime/runtime.c:226:                 strings[i] = __createString("null");
	movq	%rax, (%rbx)	# _32, *_31
# src/Runtime/runtime.c:227:                 __incRef(strings[i]);
	movl	-24(%rbp), %eax	# i, tmp212
	cltq
	leaq	0(,%rax,8), %rdx	#, _34
	movq	-40(%rbp), %rax	# strings, tmp213
	addq	%rdx, %rax	# _34, _35
# src/Runtime/runtime.c:227:                 __incRef(strings[i]);
	movq	(%rax), %rax	# *_35, _36
	movq	%rax, %rdi	# _36,
	call	__incRef	#
	jmp	.L51	#
.L53:
# src/Runtime/runtime.c:229:                 obj (*toString)(obj) = ((void **)element->type->methods)[0];
	movq	-88(%rbp), %rax	# element, tmp214
	movq	(%rax), %rax	# element_149->type, _37
# src/Runtime/runtime.c:229:                 obj (*toString)(obj) = ((void **)element->type->methods)[0];
	movq	12(%rax), %rax	# _37->methods, _38
# src/Runtime/runtime.c:229:                 obj (*toString)(obj) = ((void **)element->type->methods)[0];
	movq	(%rax), %rax	# MEM[(void * *)_38], _39
# src/Runtime/runtime.c:229:                 obj (*toString)(obj) = ((void **)element->type->methods)[0];
	movq	%rax, -96(%rbp)	# _39, toString
# src/Runtime/runtime.c:230:                 strings[i] = toString(element);
	movl	-24(%rbp), %eax	# i, tmp215
	cltq
	leaq	0(,%rax,8), %rdx	#, _41
	movq	-40(%rbp), %rax	# strings, tmp216
	leaq	(%rdx,%rax), %rbx	#, _42
# src/Runtime/runtime.c:230:                 strings[i] = toString(element);
	movq	-88(%rbp), %rax	# element, tmp217
	movq	-96(%rbp), %rdx	# toString, tmp218
	movq	%rax, %rdi	# tmp217,
	call	*%rdx	# tmp218
# src/Runtime/runtime.c:230:                 strings[i] = toString(element);
	movq	%rax, (%rbx)	# _43, *_42
.L51:
# src/Runtime/runtime.c:233:         lenghts[i] = u8_strlen((strings[i]->data));
	movl	-24(%rbp), %eax	# i, tmp219
	cltq
	leaq	0(,%rax,8), %rdx	#, _45
	movq	-40(%rbp), %rax	# strings, tmp220
	addq	%rdx, %rax	# _45, _46
	movq	(%rax), %rax	# *_46, _47
# src/Runtime/runtime.c:233:         lenghts[i] = u8_strlen((strings[i]->data));
	movq	8(%rax), %rax	# _47->data, _48
# src/Runtime/runtime.c:233:         lenghts[i] = u8_strlen((strings[i]->data));
	movq	%rax, %rdi	# _48,
	call	u8_strlen@PLT	#
# src/Runtime/runtime.c:233:         lenghts[i] = u8_strlen((strings[i]->data));
	movl	-24(%rbp), %edx	# i, tmp221
	movslq	%edx, %rdx	# tmp221, _50
	leaq	0(,%rdx,4), %rcx	#, _51
	movq	-48(%rbp), %rdx	# lenghts, tmp222
	addq	%rcx, %rdx	# _51, _52
# src/Runtime/runtime.c:233:         lenghts[i] = u8_strlen((strings[i]->data));
	movl	%eax, (%rdx)	# _53, *_52
# src/Runtime/runtime.c:234:         totalLenght += lenghts[i];
	movl	-24(%rbp), %eax	# i, tmp223
	cltq
	leaq	0(,%rax,4), %rdx	#, _55
	movq	-48(%rbp), %rax	# lenghts, tmp224
	addq	%rdx, %rax	# _55, _56
	movl	(%rax), %eax	# *_56, _57
# src/Runtime/runtime.c:234:         totalLenght += lenghts[i];
	addl	%eax, -20(%rbp)	# _57, totalLenght
# src/Runtime/runtime.c:215:     for (int i = 0; i < arr->length; i++) {
	addl	$1, -24(%rbp)	#, i
.L49:
# src/Runtime/runtime.c:215:     for (int i = 0; i < arr->length; i++) {
	movq	-136(%rbp), %rax	# arr, tmp225
	movl	32(%rax), %eax	# arr_116(D)->length, _58
# src/Runtime/runtime.c:215:     for (int i = 0; i < arr->length; i++) {
	cmpl	%eax, -24(%rbp)	# _58, i
	jl	.L54	#,
# src/Runtime/runtime.c:237:     int32_t bufferSize = u8_strlen(start) + totalLenght +
	leaq	-114(%rbp), %rax	#, tmp226
	movq	%rax, %rdi	# tmp226,
	call	u8_strlen@PLT	#
# src/Runtime/runtime.c:237:     int32_t bufferSize = u8_strlen(start) + totalLenght +
	movl	%eax, %edx	# _59, _60
	movl	-20(%rbp), %eax	# totalLenght, totalLenght.115_61
	leal	(%rdx,%rax), %ebx	#, _62
# src/Runtime/runtime.c:238:                          (arr->length - 1) * u8_strlen(delim) +
	movq	-136(%rbp), %rax	# arr, tmp227
	movl	32(%rax), %eax	# arr_116(D)->length, _63
# src/Runtime/runtime.c:238:                          (arr->length - 1) * u8_strlen(delim) +
	subl	$1, %eax	#, _64
	cltq
# src/Runtime/runtime.c:237:     int32_t bufferSize = u8_strlen(start) + totalLenght +
	movl	%eax, %r12d	# _65, _66
# src/Runtime/runtime.c:238:                          (arr->length - 1) * u8_strlen(delim) +
	leaq	-117(%rbp), %rax	#, tmp228
	movq	%rax, %rdi	# tmp228,
	call	u8_strlen@PLT	#
# src/Runtime/runtime.c:237:     int32_t bufferSize = u8_strlen(start) + totalLenght +
	imull	%r12d, %eax	# _66, _69
	addl	%eax, %ebx	# _69, _70
# src/Runtime/runtime.c:239:                          u8_strlen(end) + 1;
	leaq	-119(%rbp), %rax	#, tmp229
	movq	%rax, %rdi	# tmp229,
	call	u8_strlen@PLT	#
# src/Runtime/runtime.c:238:                          (arr->length - 1) * u8_strlen(delim) +
	addl	%ebx, %eax	# _70, _73
# src/Runtime/runtime.c:239:                          u8_strlen(end) + 1;
	addl	$1, %eax	#, _74
# src/Runtime/runtime.c:237:     int32_t bufferSize = u8_strlen(start) + totalLenght +
	movl	%eax, -52(%rbp)	# _74, bufferSize
# src/Runtime/runtime.c:240:     uint8_t *buffer = malloc(bufferSize);
	movl	-52(%rbp), %eax	# bufferSize, tmp230
	cltq
	movq	%rax, %rdi	# _75,
	call	malloc@PLT	#
	movq	%rax, -64(%rbp)	# tmp231, buffer
# src/Runtime/runtime.c:241:     int32_t index = 0;
	movl	$0, -28(%rbp)	#, index
# src/Runtime/runtime.c:242:     u8_strcpy(buffer + index, start);
	movl	-28(%rbp), %eax	# index, tmp232
	movslq	%eax, %rdx	# tmp232, _76
	movq	-64(%rbp), %rax	# buffer, tmp233
	addq	%rax, %rdx	# tmp233, _77
	leaq	-114(%rbp), %rax	#, tmp234
	movq	%rax, %rsi	# tmp234,
	movq	%rdx, %rdi	# _77,
	call	u8_strcpy@PLT	#
# src/Runtime/runtime.c:243:     index++;
	addl	$1, -28(%rbp)	#, index
# src/Runtime/runtime.c:244:     for (int i = 0; i < arr->length; i++) {
	movl	$0, -32(%rbp)	#, i
# src/Runtime/runtime.c:244:     for (int i = 0; i < arr->length; i++) {
	jmp	.L55	#
.L57:
# src/Runtime/runtime.c:245:         u8_strcpy(buffer + index, (strings[i]->data));
	movl	-32(%rbp), %eax	# i, tmp235
	cltq
	leaq	0(,%rax,8), %rdx	#, _79
	movq	-40(%rbp), %rax	# strings, tmp236
	addq	%rdx, %rax	# _79, _80
	movq	(%rax), %rax	# *_80, _81
# src/Runtime/runtime.c:245:         u8_strcpy(buffer + index, (strings[i]->data));
	movq	8(%rax), %rax	# _81->data, _82
# src/Runtime/runtime.c:245:         u8_strcpy(buffer + index, (strings[i]->data));
	movl	-28(%rbp), %edx	# index, tmp237
	movslq	%edx, %rcx	# tmp237, _83
	movq	-64(%rbp), %rdx	# buffer, tmp238
	addq	%rcx, %rdx	# _83, _84
	movq	%rax, %rsi	# _82,
	movq	%rdx, %rdi	# _84,
	call	u8_strcpy@PLT	#
# src/Runtime/runtime.c:246:         index += lenghts[i];
	movl	-32(%rbp), %eax	# i, tmp239
	cltq
	leaq	0(,%rax,4), %rdx	#, _86
	movq	-48(%rbp), %rax	# lenghts, tmp240
	addq	%rdx, %rax	# _86, _87
	movl	(%rax), %eax	# *_87, _88
# src/Runtime/runtime.c:246:         index += lenghts[i];
	addl	%eax, -28(%rbp)	# _88, index
# src/Runtime/runtime.c:247:         if (i != arr->length - 1) {
	movq	-136(%rbp), %rax	# arr, tmp241
	movl	32(%rax), %eax	# arr_116(D)->length, _89
# src/Runtime/runtime.c:247:         if (i != arr->length - 1) {
	subl	$1, %eax	#, _90
# src/Runtime/runtime.c:247:         if (i != arr->length - 1) {
	cmpl	%eax, -32(%rbp)	# _90, i
	je	.L56	#,
# src/Runtime/runtime.c:248:             u8_strcpy(buffer + index, delim);
	movl	-28(%rbp), %eax	# index, tmp242
	movslq	%eax, %rdx	# tmp242, _91
	movq	-64(%rbp), %rax	# buffer, tmp243
	addq	%rax, %rdx	# tmp243, _92
	leaq	-117(%rbp), %rax	#, tmp244
	movq	%rax, %rsi	# tmp244,
	movq	%rdx, %rdi	# _92,
	call	u8_strcpy@PLT	#
# src/Runtime/runtime.c:249:             index += 2;
	addl	$2, -28(%rbp)	#, index
.L56:
# src/Runtime/runtime.c:251:         __decRef(strings[i]);
	movl	-32(%rbp), %eax	# i, tmp245
	cltq
	leaq	0(,%rax,8), %rdx	#, _94
	movq	-40(%rbp), %rax	# strings, tmp246
	addq	%rdx, %rax	# _94, _95
# src/Runtime/runtime.c:251:         __decRef(strings[i]);
	movq	(%rax), %rax	# *_95, _96
	movq	%rax, %rdi	# _96,
	call	__decRef	#
# src/Runtime/runtime.c:244:     for (int i = 0; i < arr->length; i++) {
	addl	$1, -32(%rbp)	#, i
.L55:
# src/Runtime/runtime.c:244:     for (int i = 0; i < arr->length; i++) {
	movq	-136(%rbp), %rax	# arr, tmp247
	movl	32(%rax), %eax	# arr_116(D)->length, _97
# src/Runtime/runtime.c:244:     for (int i = 0; i < arr->length; i++) {
	cmpl	%eax, -32(%rbp)	# _97, i
	jl	.L57	#,
# src/Runtime/runtime.c:253:     u8_strcpy(buffer + index, end);
	movl	-28(%rbp), %eax	# index, tmp248
	movslq	%eax, %rdx	# tmp248, _98
	movq	-64(%rbp), %rax	# buffer, tmp249
	addq	%rax, %rdx	# tmp249, _99
	leaq	-119(%rbp), %rax	#, tmp250
	movq	%rax, %rsi	# tmp250,
	movq	%rdx, %rdi	# _99,
	call	u8_strcpy@PLT	#
# src/Runtime/runtime.c:254:     buffer[bufferSize - 1] = 0;
	movl	-52(%rbp), %eax	# bufferSize, tmp251
	cltq
	leaq	-1(%rax), %rdx	#, _101
	movq	-64(%rbp), %rax	# buffer, tmp252
	addq	%rdx, %rax	# _101, _102
# src/Runtime/runtime.c:254:     buffer[bufferSize - 1] = 0;
	movb	$0, (%rax)	#, *_102
# src/Runtime/runtime.c:255:     obj ret = __createString(buffer);
	movq	-64(%rbp), %rax	# buffer, tmp253
	movq	%rax, %rdi	# tmp253,
	call	__createString	#
	movq	%rax, -72(%rbp)	# tmp254, ret
# src/Runtime/runtime.c:256:     __incRef(ret);
	movq	-72(%rbp), %rax	# ret, tmp255
	movq	%rax, %rdi	# tmp255,
	call	__incRef	#
# src/Runtime/runtime.c:257:     free(lenghts);
	movq	-48(%rbp), %rax	# lenghts, tmp256
	movq	%rax, %rdi	# tmp256,
	call	free@PLT	#
# src/Runtime/runtime.c:258:     free(strings);
	movq	-40(%rbp), %rax	# strings, tmp257
	movq	%rax, %rdi	# tmp257,
	call	free@PLT	#
# src/Runtime/runtime.c:259:     free(buffer);
	movq	-64(%rbp), %rax	# buffer, tmp258
	movq	%rax, %rdi	# tmp258,
	call	free@PLT	#
# src/Runtime/runtime.c:260:     return ret;
	movq	-72(%rbp), %rax	# ret, _138
# src/Runtime/runtime.c:261: }
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
# src/Runtime/runtime.c:264:     __incRef(str);
	movq	-8(%rbp), %rax	# str, tmp84
	movq	%rax, %rdi	# tmp84,
	call	__incRef	#
# src/Runtime/runtime.c:265:     return str;
	movq	-8(%rbp), %rax	# str, _4
# src/Runtime/runtime.c:266: }
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
# src/Runtime/runtime.c:268:     int32_t hash = 0x811c9dc5;
	movl	$-2128831035, -4(%rbp)	#, hash
# src/Runtime/runtime.c:269:     uint8_t *rawstring = str->data;
	movq	-40(%rbp), %rax	# str, tmp89
	movq	8(%rax), %rax	# str_10(D)->data, tmp90
	movq	%rax, -16(%rbp)	# tmp90, rawstring
# src/Runtime/runtime.c:270:     int32_t strlen = u8_strlen(rawstring);
	movq	-16(%rbp), %rax	# rawstring, tmp91
	movq	%rax, %rdi	# tmp91,
	call	u8_strlen@PLT	#
# src/Runtime/runtime.c:270:     int32_t strlen = u8_strlen(rawstring);
	movl	%eax, -20(%rbp)	# _1, strlen
# src/Runtime/runtime.c:271:     for (int i = 0; i < strlen; i++) {
	movl	$0, -8(%rbp)	#, i
# src/Runtime/runtime.c:271:     for (int i = 0; i < strlen; i++) {
	jmp	.L62	#
.L63:
# src/Runtime/runtime.c:272:         hash ^= rawstring[i];
	movl	-8(%rbp), %eax	# i, tmp92
	movslq	%eax, %rdx	# tmp92, _2
	movq	-16(%rbp), %rax	# rawstring, tmp93
	addq	%rdx, %rax	# _2, _3
	movzbl	(%rax), %eax	# *_3, _4
	movzbl	%al, %eax	# _4, _5
# src/Runtime/runtime.c:272:         hash ^= rawstring[i];
	xorl	%eax, -4(%rbp)	# _5, hash
# src/Runtime/runtime.c:273:         hash *= 0x01000193;
	movl	-4(%rbp), %eax	# hash, tmp95
	imull	$16777619, %eax, %eax	#, tmp95, tmp94
	movl	%eax, -4(%rbp)	# tmp94, hash
# src/Runtime/runtime.c:271:     for (int i = 0; i < strlen; i++) {
	addl	$1, -8(%rbp)	#, i
.L62:
# src/Runtime/runtime.c:271:     for (int i = 0; i < strlen; i++) {
	movl	-8(%rbp), %eax	# i, tmp96
	cmpl	-20(%rbp), %eax	# strlen, tmp96
	jl	.L63	#,
# src/Runtime/runtime.c:275:     return hash;
	movl	-4(%rbp), %eax	# hash, _14
# src/Runtime/runtime.c:276: }
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
# src/Runtime/runtime.c:278:     if (o2 == NULL)
	cmpq	$0, -48(%rbp)	#, o2
	jne	.L66	#,
# src/Runtime/runtime.c:279:         return false;
	movl	$0, %eax	#, _6
	jmp	.L67	#
.L66:
# src/Runtime/runtime.c:280:     if (o2->type != &_class_String)
	movq	-48(%rbp), %rax	# o2, tmp89
	movq	(%rax), %rdx	# o2_8(D)->type, _1
# src/Runtime/runtime.c:280:     if (o2->type != &_class_String)
	leaq	_class_String(%rip), %rax	#, tmp90
	cmpq	%rax, %rdx	# tmp90, _1
	je	.L68	#,
# src/Runtime/runtime.c:281:         return false;
	movl	$0, %eax	#, _6
	jmp	.L67	#
.L68:
# src/Runtime/runtime.c:282:     if (_String_length(o1) != _String_length(o2))
	movq	-40(%rbp), %rax	# o1, tmp91
	movq	%rax, %rdi	# tmp91,
	call	_String_length	#
	movl	%eax, %ebx	#, _2
# src/Runtime/runtime.c:282:     if (_String_length(o1) != _String_length(o2))
	movq	-48(%rbp), %rax	# o2, tmp92
	movq	%rax, %rdi	# tmp92,
	call	_String_length	#
# src/Runtime/runtime.c:282:     if (_String_length(o1) != _String_length(o2))
	cmpl	%eax, %ebx	# _3, _2
	je	.L69	#,
# src/Runtime/runtime.c:283:         return false;
	movl	$0, %eax	#, _6
	jmp	.L67	#
.L69:
# src/Runtime/runtime.c:284:     uint8_t *rs1 = (o1->data);
	movq	-40(%rbp), %rax	# o1, tmp93
	movq	8(%rax), %rax	# o1_10(D)->data, tmp94
	movq	%rax, -24(%rbp)	# tmp94, rs1
# src/Runtime/runtime.c:285:     uint8_t *rs2 = (o2->data);
	movq	-48(%rbp), %rax	# o2, tmp95
	movq	8(%rax), %rax	# o2_8(D)->data, tmp96
	movq	%rax, -32(%rbp)	# tmp96, rs2
# src/Runtime/runtime.c:286:     return u8_strcmp(rs1, rs2) == 0;
	movq	-32(%rbp), %rdx	# rs2, tmp97
	movq	-24(%rbp), %rax	# rs1, tmp98
	movq	%rdx, %rsi	# tmp97,
	movq	%rax, %rdi	# tmp98,
	call	u8_strcmp@PLT	#
# src/Runtime/runtime.c:286:     return u8_strcmp(rs1, rs2) == 0;
	testl	%eax, %eax	# _4
	sete	%al	#, _5
.L67:
# src/Runtime/runtime.c:287: }
	movq	-8(%rbp), %rbx	#,
	leave	
	.cfi_def_cfa 7, 8
	ret	
	.cfi_endproc
.LFE24:
	.size	_String_equals, .-_String_equals
	.section	.rodata
	.align 8
.LC4:
	.string	"ERROR: Substring with negative length."
.LC5:
	.string	""
	.align 8
.LC6:
	.string	"ERROR: Substring starting index is too big."
	.align 8
.LC7:
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
# src/Runtime/runtime.c:289:     if (length < 0) {
	cmpl	$0, -80(%rbp)	#, length
	jns	.L71	#,
# src/Runtime/runtime.c:290:         errMsg = "ERROR: Substring with negative length.";
	leaq	.LC4(%rip), %rax	#, tmp101
	movq	%rax, errMsg(%rip)	# tmp101, errMsg
# src/Runtime/runtime.c:291:         error();
	movl	$0, %eax	#,
	call	error	#
.L71:
# src/Runtime/runtime.c:293:     if (length == 0)
	cmpl	$0, -80(%rbp)	#, length
	jne	.L72	#,
# src/Runtime/runtime.c:294:         return __createString("");
	leaq	.LC5(%rip), %rax	#, tmp102
	movq	%rax, %rdi	# tmp102,
	call	__createString	#
	jmp	.L80	#
.L72:
# src/Runtime/runtime.c:295:     if (startIndex >= _String_length(str)) {
	movq	-72(%rbp), %rax	# str, tmp103
	movq	%rax, %rdi	# tmp103,
	call	_String_length	#
# src/Runtime/runtime.c:295:     if (startIndex >= _String_length(str)) {
	cmpl	%eax, -76(%rbp)	# _1, startIndex
	jl	.L74	#,
# src/Runtime/runtime.c:296:         errMsg = "ERROR: Substring starting index is too big.";
	leaq	.LC6(%rip), %rax	#, tmp104
	movq	%rax, errMsg(%rip)	# tmp104, errMsg
# src/Runtime/runtime.c:297:         error();
	movl	$0, %eax	#,
	call	error	#
.L74:
# src/Runtime/runtime.c:299:     uint8_t *rs = (str->data);
	movq	-72(%rbp), %rax	# str, tmp105
	movq	8(%rax), %rax	# str_31(D)->data, tmp106
	movq	%rax, -32(%rbp)	# tmp106, rs
# src/Runtime/runtime.c:300:     uint8_t *offset_str = rs;
	movq	-32(%rbp), %rax	# rs, tmp107
	movq	%rax, -8(%rbp)	# tmp107, offset_str
# src/Runtime/runtime.c:302:     while (startIndex-- > 0)
	jmp	.L75	#
.L76:
# src/Runtime/runtime.c:303:         offset_str += u8_next(&character, offset_str) - offset_str;
	movq	-8(%rbp), %rdx	# offset_str, tmp108
	leaq	-60(%rbp), %rax	#, tmp109
	movq	%rdx, %rsi	# tmp108,
	movq	%rax, %rdi	# tmp109,
	call	u8_next@PLT	#
# src/Runtime/runtime.c:303:         offset_str += u8_next(&character, offset_str) - offset_str;
	subq	-8(%rbp), %rax	# offset_str, _59
# src/Runtime/runtime.c:303:         offset_str += u8_next(&character, offset_str) - offset_str;
	addq	%rax, -8(%rbp)	# _3, offset_str
.L75:
# src/Runtime/runtime.c:302:     while (startIndex-- > 0)
	movl	-76(%rbp), %eax	# startIndex, startIndex.116_4
	leal	-1(%rax), %edx	#, tmp110
	movl	%edx, -76(%rbp)	# tmp110, startIndex
# src/Runtime/runtime.c:302:     while (startIndex-- > 0)
	testl	%eax, %eax	# startIndex.116_4
	jg	.L76	#,
# src/Runtime/runtime.c:304:     uint8_t *end = offset_str;
	movq	-8(%rbp), %rax	# offset_str, tmp111
	movq	%rax, -16(%rbp)	# tmp111, end
# src/Runtime/runtime.c:305:     int32_t counter = 0;
	movl	$0, -20(%rbp)	#, counter
# src/Runtime/runtime.c:306:     while (counter < length) {
	jmp	.L77	#
.L79:
# src/Runtime/runtime.c:307:         if (u8_next(&character, end) == NULL) {
	movq	-16(%rbp), %rdx	# end, tmp112
	leaq	-60(%rbp), %rax	#, tmp113
	movq	%rdx, %rsi	# tmp112,
	movq	%rax, %rdi	# tmp113,
	call	u8_next@PLT	#
# src/Runtime/runtime.c:307:         if (u8_next(&character, end) == NULL) {
	testq	%rax, %rax	# _5
	jne	.L78	#,
# src/Runtime/runtime.c:308:             errMsg = "ERROR: Substring reached end of string.";
	leaq	.LC7(%rip), %rax	#, tmp114
	movq	%rax, errMsg(%rip)	# tmp114, errMsg
# src/Runtime/runtime.c:309:             error();
	movl	$0, %eax	#,
	call	error	#
.L78:
# src/Runtime/runtime.c:311:         end += u8_next(&character, end) - end;
	movq	-16(%rbp), %rdx	# end, tmp115
	leaq	-60(%rbp), %rax	#, tmp116
	movq	%rdx, %rsi	# tmp115,
	movq	%rax, %rdi	# tmp116,
	call	u8_next@PLT	#
# src/Runtime/runtime.c:311:         end += u8_next(&character, end) - end;
	subq	-16(%rbp), %rax	# end, _55
# src/Runtime/runtime.c:311:         end += u8_next(&character, end) - end;
	addq	%rax, -16(%rbp)	# _7, end
# src/Runtime/runtime.c:312:         counter++;
	addl	$1, -20(%rbp)	#, counter
.L77:
# src/Runtime/runtime.c:306:     while (counter < length) {
	movl	-20(%rbp), %eax	# counter, tmp117
	cmpl	-80(%rbp), %eax	# length, tmp117
	jl	.L79	#,
# src/Runtime/runtime.c:314:     int32_t bufferSize = end - offset_str + 1;
	movq	-16(%rbp), %rax	# end, tmp118
	subq	-8(%rbp), %rax	# offset_str, _8
# src/Runtime/runtime.c:314:     int32_t bufferSize = end - offset_str + 1;
	addl	$1, %eax	#, _10
# src/Runtime/runtime.c:314:     int32_t bufferSize = end - offset_str + 1;
	movl	%eax, -36(%rbp)	# _10, bufferSize
# src/Runtime/runtime.c:315:     uint8_t *buffer = malloc(bufferSize);
	movl	-36(%rbp), %eax	# bufferSize, tmp119
	cltq
	movq	%rax, %rdi	# _11,
	call	malloc@PLT	#
	movq	%rax, -48(%rbp)	# tmp120, buffer
# src/Runtime/runtime.c:316:     u8_strncpy(buffer, offset_str, bufferSize);
	movl	-36(%rbp), %eax	# bufferSize, tmp121
	movslq	%eax, %rdx	# tmp121, _12
	movq	-8(%rbp), %rcx	# offset_str, tmp122
	movq	-48(%rbp), %rax	# buffer, tmp123
	movq	%rcx, %rsi	# tmp122,
	movq	%rax, %rdi	# tmp123,
	call	u8_strncpy@PLT	#
# src/Runtime/runtime.c:317:     buffer[bufferSize - 1] = 0;
	movl	-36(%rbp), %eax	# bufferSize, tmp124
	cltq
	leaq	-1(%rax), %rdx	#, _14
	movq	-48(%rbp), %rax	# buffer, tmp125
	addq	%rdx, %rax	# _14, _15
# src/Runtime/runtime.c:317:     buffer[bufferSize - 1] = 0;
	movb	$0, (%rax)	#, *_15
# src/Runtime/runtime.c:318:     obj ret = __createString(buffer);
	movq	-48(%rbp), %rax	# buffer, tmp126
	movq	%rax, %rdi	# tmp126,
	call	__createString	#
	movq	%rax, -56(%rbp)	# tmp127, ret
# src/Runtime/runtime.c:319:     __incRef(ret);
	movq	-56(%rbp), %rax	# ret, tmp128
	movq	%rax, %rdi	# tmp128,
	call	__incRef	#
# src/Runtime/runtime.c:320:     free(buffer);
	movq	-48(%rbp), %rax	# buffer, tmp129
	movq	%rax, %rdi	# tmp129,
	call	free@PLT	#
# src/Runtime/runtime.c:321:     return ret;
	movq	-56(%rbp), %rax	# ret, _20
.L80:
# src/Runtime/runtime.c:322: }
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
	subq	$16, %rsp	#,
	movq	%rdi, -8(%rbp)	# str, str
# src/Runtime/runtime.c:324:     if (str->length < 0) {
	movq	-8(%rbp), %rax	# str, tmp90
	movl	32(%rax), %eax	# str_9(D)->length, _1
# src/Runtime/runtime.c:324:     if (str->length < 0) {
	testl	%eax, %eax	# _1
	jns	.L82	#,
# src/Runtime/runtime.c:325:         str->length = u8_mbsnlen(str->data, u8_strlen(str->data));
	movq	-8(%rbp), %rax	# str, tmp91
	movq	8(%rax), %rax	# str_9(D)->data, _2
# src/Runtime/runtime.c:325:         str->length = u8_mbsnlen(str->data, u8_strlen(str->data));
	movq	%rax, %rdi	# _2,
	call	u8_strlen@PLT	#
	movq	%rax, %rdx	#, _3
# src/Runtime/runtime.c:325:         str->length = u8_mbsnlen(str->data, u8_strlen(str->data));
	movq	-8(%rbp), %rax	# str, tmp92
	movq	8(%rax), %rax	# str_9(D)->data, _4
# src/Runtime/runtime.c:325:         str->length = u8_mbsnlen(str->data, u8_strlen(str->data));
	movq	%rdx, %rsi	# _3,
	movq	%rax, %rdi	# _4,
	call	u8_mbsnlen@PLT	#
# src/Runtime/runtime.c:325:         str->length = u8_mbsnlen(str->data, u8_strlen(str->data));
	movl	%eax, %edx	# _5, _6
	movq	-8(%rbp), %rax	# str, tmp93
	movl	%edx, 32(%rax)	# _6, str_9(D)->length
.L82:
# src/Runtime/runtime.c:327:     return str->length;
	movq	-8(%rbp), %rax	# str, tmp94
	movl	32(%rax), %eax	# str_9(D)->length, _11
# src/Runtime/runtime.c:328: }
	leave	
	.cfi_def_cfa 7, 8
	ret	
	.cfi_endproc
.LFE26:
	.size	_String_length, .-_String_length
	.section	.rodata
	.align 8
.LC8:
	.string	"ERROR: IndexOf null substring argument."
	.align 8
.LC9:
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
# src/Runtime/runtime.c:330:     if (substr == NULL) {
	cmpq	$0, -80(%rbp)	#, substr
	jne	.L85	#,
# src/Runtime/runtime.c:331:         errMsg = "ERROR: IndexOf null substring argument.";
	leaq	.LC8(%rip), %rax	#, tmp95
	movq	%rax, errMsg(%rip)	# tmp95, errMsg
# src/Runtime/runtime.c:332:         error();
	movl	$0, %eax	#,
	call	error	#
.L85:
# src/Runtime/runtime.c:334:     if (startFrom >= _String_length(str)) {
	movq	-72(%rbp), %rax	# str, tmp96
	movq	%rax, %rdi	# tmp96,
	call	_String_length	#
# src/Runtime/runtime.c:334:     if (startFrom >= _String_length(str)) {
	cmpl	%eax, -84(%rbp)	# _1, startFrom
	jl	.L86	#,
# src/Runtime/runtime.c:335:         errMsg = "ERROR: IndexOf starting index is too big.";
	leaq	.LC9(%rip), %rax	#, tmp97
	movq	%rax, errMsg(%rip)	# tmp97, errMsg
# src/Runtime/runtime.c:336:         error();
	movl	$0, %eax	#,
	call	error	#
.L86:
# src/Runtime/runtime.c:338:     if (_String_length(str) < _String_length(substr))
	movq	-72(%rbp), %rax	# str, tmp98
	movq	%rax, %rdi	# tmp98,
	call	_String_length	#
	movl	%eax, %ebx	#, _2
# src/Runtime/runtime.c:338:     if (_String_length(str) < _String_length(substr))
	movq	-80(%rbp), %rax	# substr, tmp99
	movq	%rax, %rdi	# tmp99,
	call	_String_length	#
# src/Runtime/runtime.c:338:     if (_String_length(str) < _String_length(substr))
	cmpl	%eax, %ebx	# _3, _2
	jge	.L87	#,
# src/Runtime/runtime.c:339:         return -1;
	movl	$-1, %eax	#, _14
	jmp	.L94	#
.L87:
# src/Runtime/runtime.c:340:     uint8_t *rs = (str->data);
	movq	-72(%rbp), %rax	# str, tmp100
	movq	8(%rax), %rax	# str_24(D)->data, tmp101
	movq	%rax, -24(%rbp)	# tmp101, rs
# src/Runtime/runtime.c:341:     uint8_t *rsub = (substr->data);
	movq	-80(%rbp), %rax	# substr, tmp102
	movq	8(%rax), %rax	# substr_20(D)->data, tmp103
	movq	%rax, -48(%rbp)	# tmp103, rsub
# src/Runtime/runtime.c:342:     uint8_t *start = rs;
	movq	-24(%rbp), %rax	# rs, tmp104
	movq	%rax, -32(%rbp)	# tmp104, start
# src/Runtime/runtime.c:344:     while (startFrom-- > 0) {
	jmp	.L89	#
.L91:
# src/Runtime/runtime.c:345:         if (u8_next(&c, start) == NULL)
	movq	-32(%rbp), %rdx	# start, tmp105
	leaq	-60(%rbp), %rax	#, tmp106
	movq	%rdx, %rsi	# tmp105,
	movq	%rax, %rdi	# tmp106,
	call	u8_next@PLT	#
# src/Runtime/runtime.c:345:         if (u8_next(&c, start) == NULL)
	testq	%rax, %rax	# _4
	jne	.L90	#,
# src/Runtime/runtime.c:346:             return -1;
	movl	$-1, %eax	#, _14
	jmp	.L94	#
.L90:
# src/Runtime/runtime.c:347:         start += u8_next(&c, start) - start;
	movq	-32(%rbp), %rdx	# start, tmp107
	leaq	-60(%rbp), %rax	#, tmp108
	movq	%rdx, %rsi	# tmp107,
	movq	%rax, %rdi	# tmp108,
	call	u8_next@PLT	#
# src/Runtime/runtime.c:347:         start += u8_next(&c, start) - start;
	subq	-32(%rbp), %rax	# start, _44
# src/Runtime/runtime.c:347:         start += u8_next(&c, start) - start;
	addq	%rax, -32(%rbp)	# _6, start
.L89:
# src/Runtime/runtime.c:344:     while (startFrom-- > 0) {
	movl	-84(%rbp), %eax	# startFrom, startFrom.117_7
	leal	-1(%rax), %edx	#, tmp109
	movl	%edx, -84(%rbp)	# tmp109, startFrom
# src/Runtime/runtime.c:344:     while (startFrom-- > 0) {
	testl	%eax, %eax	# startFrom.117_7
	jg	.L91	#,
# src/Runtime/runtime.c:349:     uint8_t *index = u8_strstr(start, rsub);
	movq	-48(%rbp), %rdx	# rsub, tmp110
	movq	-32(%rbp), %rax	# start, tmp111
	movq	%rdx, %rsi	# tmp110,
	movq	%rax, %rdi	# tmp111,
	call	u8_strstr@PLT	#
	movq	%rax, -56(%rbp)	# tmp112, index
# src/Runtime/runtime.c:350:     uint32_t counter = 0;
	movl	$0, -36(%rbp)	#, counter
# src/Runtime/runtime.c:351:     while ((rs += u8_next(&c, rs) - rs) != index)
	jmp	.L92	#
.L93:
# src/Runtime/runtime.c:352:         counter++;
	addl	$1, -36(%rbp)	#, counter
.L92:
# src/Runtime/runtime.c:351:     while ((rs += u8_next(&c, rs) - rs) != index)
	movq	-24(%rbp), %rdx	# rs, tmp113
	leaq	-60(%rbp), %rax	#, tmp114
	movq	%rdx, %rsi	# tmp113,
	movq	%rax, %rdi	# tmp114,
	call	u8_next@PLT	#
# src/Runtime/runtime.c:351:     while ((rs += u8_next(&c, rs) - rs) != index)
	subq	-24(%rbp), %rax	# rs, _38
# src/Runtime/runtime.c:351:     while ((rs += u8_next(&c, rs) - rs) != index)
	addq	%rax, -24(%rbp)	# _9, rs
# src/Runtime/runtime.c:351:     while ((rs += u8_next(&c, rs) - rs) != index)
	movq	-24(%rbp), %rax	# rs, tmp115
	cmpq	-56(%rbp), %rax	# index, tmp115
	jne	.L93	#,
# src/Runtime/runtime.c:353:     return counter;
	movl	-36(%rbp), %eax	# counter, _14
.L94:
# src/Runtime/runtime.c:354: }
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
# src/Runtime/runtime.c:356:     uint8_t *rs = (str->data);
	movq	-40(%rbp), %rax	# str, tmp89
	movq	8(%rax), %rax	# str_7(D)->data, tmp90
	movq	%rax, -8(%rbp)	# tmp90, rs
# src/Runtime/runtime.c:357:     int32_t len = rs == NULL ? 0 : u8_strlen(rs);
	cmpq	$0, -8(%rbp)	#, rs
	je	.L96	#,
# src/Runtime/runtime.c:357:     int32_t len = rs == NULL ? 0 : u8_strlen(rs);
	movq	-8(%rbp), %rax	# rs, tmp91
	movq	%rax, %rdi	# tmp91,
	call	u8_strlen@PLT	#
	jmp	.L97	#
.L96:
# src/Runtime/runtime.c:357:     int32_t len = rs == NULL ? 0 : u8_strlen(rs);
	movl	$0, %eax	#, iftmp.118_5
.L97:
# src/Runtime/runtime.c:357:     int32_t len = rs == NULL ? 0 : u8_strlen(rs);
	movl	%eax, -12(%rbp)	# iftmp.118_5, len
# src/Runtime/runtime.c:358:     obj arr = __newByteArray(len + 1);
	movl	-12(%rbp), %eax	# len, tmp92
	addl	$1, %eax	#, _2
	movl	%eax, %edi	# _2,
	call	__newByteArray	#
	movq	%rax, -24(%rbp)	# tmp93, arr
# src/Runtime/runtime.c:359:     memcpy((arr->data), rs, len);
	movl	-12(%rbp), %eax	# len, tmp94
	movslq	%eax, %rdx	# tmp94, _3
	movq	-24(%rbp), %rax	# arr, tmp95
	movq	8(%rax), %rax	# arr_13->data, _4
	movq	-8(%rbp), %rcx	# rs, tmp96
	movq	%rcx, %rsi	# tmp96,
	movq	%rax, %rdi	# _4,
	call	memcpy@PLT	#
# src/Runtime/runtime.c:360:     __incRef(arr);
	movq	-24(%rbp), %rax	# arr, tmp97
	movq	%rax, %rdi	# tmp97,
	call	__incRef	#
# src/Runtime/runtime.c:361:     return arr;
	movq	-24(%rbp), %rax	# arr, _16
# src/Runtime/runtime.c:362: }
	leave	
	.cfi_def_cfa 7, 8
	ret	
	.cfi_endproc
.LFE28:
	.size	_String_getBytes, .-_String_getBytes
	.section	.rodata
	.align 8
.LC10:
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
# src/Runtime/runtime.c:364:     if (substr == NULL) {
	cmpq	$0, -32(%rbp)	#, substr
	jne	.L100	#,
# src/Runtime/runtime.c:365:         errMsg = "ERROR: EndsWith null substring argument.";
	leaq	.LC10(%rip), %rax	#, tmp85
	movq	%rax, errMsg(%rip)	# tmp85, errMsg
# src/Runtime/runtime.c:366:         error();
	movl	$0, %eax	#,
	call	error	#
.L100:
# src/Runtime/runtime.c:368:     uint8_t *rs = (str->data);
	movq	-24(%rbp), %rax	# str, tmp86
	movq	8(%rax), %rax	# str_7(D)->data, tmp87
	movq	%rax, -8(%rbp)	# tmp87, rs
# src/Runtime/runtime.c:369:     uint8_t *rsub = (substr->data);
	movq	-32(%rbp), %rax	# substr, tmp88
	movq	8(%rax), %rax	# substr_3(D)->data, tmp89
	movq	%rax, -16(%rbp)	# tmp89, rsub
# src/Runtime/runtime.c:370:     return u8_endswith(rs, rsub);
	movq	-16(%rbp), %rdx	# rsub, tmp90
	movq	-8(%rbp), %rax	# rs, tmp91
	movq	%rdx, %rsi	# tmp90,
	movq	%rax, %rdi	# tmp91,
	call	u8_endswith@PLT	#
# src/Runtime/runtime.c:371: }
	leave	
	.cfi_def_cfa 7, 8
	ret	
	.cfi_endproc
.LFE29:
	.size	_String_endsWith, .-_String_endsWith
	.section	.rodata
	.align 8
.LC11:
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
# src/Runtime/runtime.c:373:     if (substr == NULL) {
	cmpq	$0, -32(%rbp)	#, substr
	jne	.L103	#,
# src/Runtime/runtime.c:374:         errMsg = "ERROR: StartsWith null substring argument.";
	leaq	.LC11(%rip), %rax	#, tmp85
	movq	%rax, errMsg(%rip)	# tmp85, errMsg
# src/Runtime/runtime.c:375:         error();
	movl	$0, %eax	#,
	call	error	#
.L103:
# src/Runtime/runtime.c:377:     uint8_t *rs = (str->data);
	movq	-24(%rbp), %rax	# str, tmp86
	movq	8(%rax), %rax	# str_7(D)->data, tmp87
	movq	%rax, -8(%rbp)	# tmp87, rs
# src/Runtime/runtime.c:378:     uint8_t *rsub = (substr->data);
	movq	-32(%rbp), %rax	# substr, tmp88
	movq	8(%rax), %rax	# substr_3(D)->data, tmp89
	movq	%rax, -16(%rbp)	# tmp89, rsub
# src/Runtime/runtime.c:379:     return u8_startswith(rs, rsub);
	movq	-16(%rbp), %rdx	# rsub, tmp90
	movq	-8(%rbp), %rax	# rs, tmp91
	movq	%rdx, %rsi	# tmp90,
	movq	%rax, %rdi	# tmp91,
	call	u8_startswith@PLT	#
# src/Runtime/runtime.c:380: }
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
# src/Runtime/runtime.c:383:     if (secondstr == NULL) {
	cmpq	$0, -64(%rbp)	#, secondstr
	jne	.L106	#,
# src/Runtime/runtime.c:384:         __incRef(str);
	movq	-56(%rbp), %rax	# str, tmp94
	movq	%rax, %rdi	# tmp94,
	call	__incRef	#
# src/Runtime/runtime.c:385:         return str;
	movq	-56(%rbp), %rax	# str, _27
	jmp	.L107	#
.L106:
# src/Runtime/runtime.c:387:     uint8_t *rs1 = (str->data);
	movq	-56(%rbp), %rax	# str, tmp95
	movq	8(%rax), %rax	# str_23(D)->data, tmp96
	movq	%rax, -32(%rbp)	# tmp96, rs1
# src/Runtime/runtime.c:388:     uint8_t *rs2 = (secondstr->data);
	movq	-64(%rbp), %rax	# secondstr, tmp97
	movq	8(%rax), %rax	# secondstr_29(D)->data, tmp98
	movq	%rax, -40(%rbp)	# tmp98, rs2
# src/Runtime/runtime.c:390:     int32_t len1 = u8_strlen(rs1);
	movq	-32(%rbp), %rax	# rs1, tmp99
	movq	%rax, %rdi	# tmp99,
	call	u8_strlen@PLT	#
# src/Runtime/runtime.c:390:     int32_t len1 = u8_strlen(rs1);
	movl	%eax, -44(%rbp)	# _9, len1
# src/Runtime/runtime.c:391:     int32_t len2 = u8_strlen(rs2);
	movq	-40(%rbp), %rax	# rs2, tmp100
	movq	%rax, %rdi	# tmp100,
	call	u8_strlen@PLT	#
# src/Runtime/runtime.c:391:     int32_t len2 = u8_strlen(rs2);
	movl	%eax, -12(%rbp)	# _10, len2
# src/Runtime/runtime.c:392:     uint8_t *buffer = malloc(len1 + len2 + 1);
	movl	-44(%rbp), %edx	# len1, tmp101
	movl	-12(%rbp), %eax	# len2, tmp102
	addl	%edx, %eax	# tmp101, _11
# src/Runtime/runtime.c:392:     uint8_t *buffer = malloc(len1 + len2 + 1);
	addl	$1, %eax	#, _12
# src/Runtime/runtime.c:392:     uint8_t *buffer = malloc(len1 + len2 + 1);
	cltq
	movq	%rax, %rdi	# _13,
	call	malloc@PLT	#
	movq	%rax, -24(%rbp)	# tmp103, buffer
# src/Runtime/runtime.c:394:     u8_strcpy(buffer, rs1);
	movq	-32(%rbp), %rdx	# rs1, tmp104
	movq	-24(%rbp), %rax	# buffer, tmp105
	movq	%rdx, %rsi	# tmp104,
	movq	%rax, %rdi	# tmp105,
	call	u8_strcpy@PLT	#
# src/Runtime/runtime.c:395:     u8_strcpy(buffer + len1, rs2);
	movl	-44(%rbp), %eax	# len1, tmp106
	movslq	%eax, %rdx	# tmp106, _18
	movq	-24(%rbp), %rax	# buffer, tmp107
	addq	%rax, %rdx	# tmp107, _19
	movq	-40(%rbp), %rax	# rs2, tmp108
	movq	%rax, %rsi	# tmp108,
	movq	%rdx, %rdi	# _19,
	call	u8_strcpy@PLT	#
# src/Runtime/runtime.c:396:     buffer[len1 + len2] = 0;
	movl	-44(%rbp), %edx	# len1, tmp109
	movl	-12(%rbp), %eax	# len2, tmp110
	addl	%edx, %eax	# tmp109, _20
	movslq	%eax, %rdx	# _20, _21
# src/Runtime/runtime.c:396:     buffer[len1 + len2] = 0;
	movq	-24(%rbp), %rax	# buffer, tmp111
	addq	%rdx, %rax	# _21, _22
# src/Runtime/runtime.c:396:     buffer[len1 + len2] = 0;
	movb	$0, (%rax)	#, *_22
# src/Runtime/runtime.c:398:     obj ret = __createString(buffer);
	movq	-24(%rbp), %rax	# buffer, tmp112
	movq	%rax, %rdi	# tmp112,
	call	__createString	#
	movq	%rax, -8(%rbp)	# tmp113, ret
# src/Runtime/runtime.c:399:     __incRef(ret);
	movq	-8(%rbp), %rax	# ret, tmp114
	movq	%rax, %rdi	# tmp114,
	call	__incRef	#
# src/Runtime/runtime.c:400:     free(buffer);
	movq	-24(%rbp), %rax	# buffer, tmp115
	movq	%rax, %rdi	# tmp115,
	call	free@PLT	#
# src/Runtime/runtime.c:402:     return ret;
	movq	-8(%rbp), %rax	# ret, _27
.L107:
# src/Runtime/runtime.c:403: }
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
# src/Runtime/runtime.c:407:     uint8_t *rs = (str->data);
	movq	-24(%rbp), %rax	# str, tmp91
	movq	8(%rax), %rax	# str_13(D)->data, tmp92
	movq	%rax, -8(%rbp)	# tmp92, rs
# src/Runtime/runtime.c:409:     while (index-- > 0) {
	jmp	.L109	#
.L111:
# src/Runtime/runtime.c:410:         if (u8_next(&c, rs) == NULL) {
	movq	-8(%rbp), %rdx	# rs, tmp93
	leaq	-12(%rbp), %rax	#, tmp94
	movq	%rdx, %rsi	# tmp93,
	movq	%rax, %rdi	# tmp94,
	call	u8_next@PLT	#
# src/Runtime/runtime.c:410:         if (u8_next(&c, rs) == NULL) {
	testq	%rax, %rax	# _1
	jne	.L110	#,
# src/Runtime/runtime.c:411:             errMsg = charAtErr;
	leaq	charAtErr(%rip), %rax	#, tmp95
	movq	%rax, errMsg(%rip)	# tmp95, errMsg
# src/Runtime/runtime.c:412:             error();
	movl	$0, %eax	#,
	call	error	#
.L110:
# src/Runtime/runtime.c:414:         rs += u8_next(&c, rs) - rs;
	movq	-8(%rbp), %rdx	# rs, tmp96
	leaq	-12(%rbp), %rax	#, tmp97
	movq	%rdx, %rsi	# tmp96,
	movq	%rax, %rdi	# tmp97,
	call	u8_next@PLT	#
# src/Runtime/runtime.c:414:         rs += u8_next(&c, rs) - rs;
	subq	-8(%rbp), %rax	# rs, _26
# src/Runtime/runtime.c:414:         rs += u8_next(&c, rs) - rs;
	addq	%rax, -8(%rbp)	# _3, rs
.L109:
# src/Runtime/runtime.c:409:     while (index-- > 0) {
	movl	-28(%rbp), %eax	# index, index.139_4
	leal	-1(%rax), %edx	#, tmp98
	movl	%edx, -28(%rbp)	# tmp98, index
# src/Runtime/runtime.c:409:     while (index-- > 0) {
	testl	%eax, %eax	# index.139_4
	jg	.L111	#,
# src/Runtime/runtime.c:416:     if (u8_strmbtouc(&c, rs) <= 0) {
	movq	-8(%rbp), %rdx	# rs, tmp99
	leaq	-12(%rbp), %rax	#, tmp100
	movq	%rdx, %rsi	# tmp99,
	movq	%rax, %rdi	# tmp100,
	call	u8_strmbtouc@PLT	#
# src/Runtime/runtime.c:416:     if (u8_strmbtouc(&c, rs) <= 0) {
	testl	%eax, %eax	# _5
	jg	.L112	#,
# src/Runtime/runtime.c:417:         errMsg = charAtErr;
	leaq	charAtErr(%rip), %rax	#, tmp101
	movq	%rax, errMsg(%rip)	# tmp101, errMsg
# src/Runtime/runtime.c:418:         error();
	movl	$0, %eax	#,
	call	error	#
.L112:
# src/Runtime/runtime.c:420:     return c;
	movl	-12(%rbp), %eax	# c, c.140_6
# src/Runtime/runtime.c:421: }
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
# src/Runtime/runtime.c:425: }
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
# src/Runtime/runtime.c:431:     if (str == NULL)
	cmpq	$0, -24(%rbp)	#, str
	jne	.L116	#,
# src/Runtime/runtime.c:432:         str = __createString("null");
	leaq	.LC3(%rip), %rax	#, tmp84
	movq	%rax, %rdi	# tmp84,
	call	__createString	#
	movq	%rax, -24(%rbp)	# tmp85, str
.L116:
# src/Runtime/runtime.c:433:     __incRef(str);
	movq	-24(%rbp), %rax	# str, tmp86
	movq	%rax, %rdi	# tmp86,
	call	__incRef	#
# src/Runtime/runtime.c:434:     uint8_t *rs = (str->data);
	movq	-24(%rbp), %rax	# str, tmp87
	movq	8(%rax), %rax	# str_8->data, tmp88
	movq	%rax, -8(%rbp)	# tmp88, rs
# src/Runtime/runtime.c:436:     printf("%s\n", rs);
	movq	-8(%rbp), %rax	# rs, tmp89
	movq	%rax, %rdi	# tmp89,
	call	puts@PLT	#
# src/Runtime/runtime.c:437:     __decRef(str);
	movq	-24(%rbp), %rax	# str, tmp90
	movq	%rax, %rdi	# tmp90,
	call	__decRef	#
# src/Runtime/runtime.c:439:     return 0;
	movl	$0, %eax	#, _3
# src/Runtime/runtime.c:440: }
	leave	
	.cfi_def_cfa 7, 8
	ret	
	.cfi_endproc
.LFE34:
	.size	printString, .-printString
	.section	.rodata
.LC12:
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
# src/Runtime/runtime.c:443:     printf("%d\n", i);
	movl	-4(%rbp), %eax	# i, tmp84
	movl	%eax, %esi	# tmp84,
	leaq	.LC12(%rip), %rax	#, tmp85
	movq	%rax, %rdi	# tmp85,
	movl	$0, %eax	#,
	call	printf@PLT	#
# src/Runtime/runtime.c:445:     return 0;
	movl	$0, %eax	#, _4
# src/Runtime/runtime.c:446: }
	leave	
	.cfi_def_cfa 7, 8
	ret	
	.cfi_endproc
.LFE35:
	.size	printInt, .-printInt
	.section	.rodata
.LC13:
	.string	"true"
.LC14:
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
# src/Runtime/runtime.c:449:     if (b)
	cmpb	$0, -4(%rbp)	#, b
	je	.L121	#,
# src/Runtime/runtime.c:450:         printf("true\n");
	leaq	.LC13(%rip), %rax	#, tmp86
	movq	%rax, %rdi	# tmp86,
	call	puts@PLT	#
	jmp	.L122	#
.L121:
# src/Runtime/runtime.c:452:         printf("false\n");
	leaq	.LC14(%rip), %rax	#, tmp87
	movq	%rax, %rdi	# tmp87,
	call	puts@PLT	#
.L122:
# src/Runtime/runtime.c:454:     return 0;
	movl	$0, %eax	#, _2
# src/Runtime/runtime.c:455: }
	leave	
	.cfi_def_cfa 7, 8
	ret	
	.cfi_endproc
.LFE36:
	.size	printBoolean, .-printBoolean
	.section	.rodata
.LC15:
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
# src/Runtime/runtime.c:459:     sprintf(buffer, "%d", i);
	movl	-36(%rbp), %edx	# i, tmp84
	leaq	-19(%rbp), %rax	#, tmp85
	leaq	.LC15(%rip), %rcx	#, tmp86
	movq	%rcx, %rsi	# tmp86,
	movq	%rax, %rdi	# tmp85,
	movl	$0, %eax	#,
	call	sprintf@PLT	#
# src/Runtime/runtime.c:460:     obj ret = __createString(buffer);
	leaq	-19(%rbp), %rax	#, tmp87
	movq	%rax, %rdi	# tmp87,
	call	__createString	#
	movq	%rax, -8(%rbp)	# tmp88, ret
# src/Runtime/runtime.c:461:     __incRef(ret);
	movq	-8(%rbp), %rax	# ret, tmp89
	movq	%rax, %rdi	# tmp89,
	call	__incRef	#
# src/Runtime/runtime.c:463:     return ret;
	movq	-8(%rbp), %rax	# ret, _3
# src/Runtime/runtime.c:464: }
	leave	
	.cfi_def_cfa 7, 8
	ret	
	.cfi_endproc
.LFE37:
	.size	intToString, .-intToString
	.section	.rodata
.LC16:
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
# src/Runtime/runtime.c:469:     sprintf(buffer, "%u", i);
	movzbl	-36(%rbp), %edx	# i, _5
	leaq	-19(%rbp), %rax	#, tmp87
	leaq	.LC16(%rip), %rcx	#, tmp88
	movq	%rcx, %rsi	# tmp88,
	movq	%rax, %rdi	# tmp87,
	movl	$0, %eax	#,
	call	sprintf@PLT	#
# src/Runtime/runtime.c:470:     obj ret = __createString(buffer);
	leaq	-19(%rbp), %rax	#, tmp89
	movq	%rax, %rdi	# tmp89,
	call	__createString	#
	movq	%rax, -8(%rbp)	# tmp90, ret
# src/Runtime/runtime.c:471:     __incRef(ret);
	movq	-8(%rbp), %rax	# ret, tmp91
	movq	%rax, %rdi	# tmp91,
	call	__incRef	#
# src/Runtime/runtime.c:473:     return ret;
	movq	-8(%rbp), %rax	# ret, _3
# src/Runtime/runtime.c:474: }
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
# src/Runtime/runtime.c:479:     if (b)
	cmpb	$0, -20(%rbp)	#, b
	je	.L129	#,
# src/Runtime/runtime.c:480:         ret = __createString("true");
	leaq	.LC13(%rip), %rax	#, tmp86
	movq	%rax, %rdi	# tmp86,
	call	__createString	#
	movq	%rax, -8(%rbp)	# tmp87, ret
	jmp	.L130	#
.L129:
# src/Runtime/runtime.c:482:         ret = __createString("false");
	leaq	.LC14(%rip), %rax	#, tmp88
	movq	%rax, %rdi	# tmp88,
	call	__createString	#
	movq	%rax, -8(%rbp)	# tmp89, ret
.L130:
# src/Runtime/runtime.c:483:     __incRef(ret);
	movq	-8(%rbp), %rax	# ret, tmp90
	movq	%rax, %rdi	# tmp90,
	call	__incRef	#
# src/Runtime/runtime.c:485:     return ret;
	movq	-8(%rbp), %rax	# ret, _10
# src/Runtime/runtime.c:486: }
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
# src/Runtime/runtime.c:490:     if (o == NULL)
	cmpq	$0, -24(%rbp)	#, o
	jne	.L133	#,
# src/Runtime/runtime.c:491:         o = __createString("null");
	leaq	.LC3(%rip), %rax	#, tmp87
	movq	%rax, %rdi	# tmp87,
	call	__createString	#
	movq	%rax, -24(%rbp)	# tmp88, o
.L133:
# src/Runtime/runtime.c:492:     __incRef(o);
	movq	-24(%rbp), %rax	# o, tmp89
	movq	%rax, %rdi	# tmp89,
	call	__incRef	#
# src/Runtime/runtime.c:493:     obj (*toStr)(obj) = ((void **)o->type->methods)[0];
	movq	-24(%rbp), %rax	# o, tmp90
	movq	(%rax), %rax	# o_12->type, _5
# src/Runtime/runtime.c:493:     obj (*toStr)(obj) = ((void **)o->type->methods)[0];
	movq	12(%rax), %rax	# _5->methods, _6
# src/Runtime/runtime.c:493:     obj (*toStr)(obj) = ((void **)o->type->methods)[0];
	movq	(%rax), %rax	# MEM[(void * *)_6], _7
# src/Runtime/runtime.c:493:     obj (*toStr)(obj) = ((void **)o->type->methods)[0];
	movq	%rax, -16(%rbp)	# _7, toStr
# src/Runtime/runtime.c:494:     obj str = toStr(o);
	movq	-24(%rbp), %rax	# o, tmp91
	movq	-16(%rbp), %rdx	# toStr, tmp92
	movq	%rax, %rdi	# tmp91,
	call	*%rdx	# tmp92
	movq	%rax, -8(%rbp)	# tmp93, str
# src/Runtime/runtime.c:496:     printString(str);
	movq	-8(%rbp), %rax	# str, tmp94
	movq	%rax, %rdi	# tmp94,
	call	printString	#
# src/Runtime/runtime.c:497:     __decRef(str);
	movq	-8(%rbp), %rax	# str, tmp95
	movq	%rax, %rdi	# tmp95,
	call	__decRef	#
# src/Runtime/runtime.c:498:     __decRef(o);
	movq	-24(%rbp), %rax	# o, tmp96
	movq	%rax, %rdi	# tmp96,
	call	__decRef	#
# src/Runtime/runtime.c:500:     return 0;
	movl	$0, %eax	#, _17
# src/Runtime/runtime.c:501: }
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
	subq	$16, %rsp	#,
	movq	%rdi, -8(%rbp)	# arr, arr
# src/Runtime/runtime.c:505:     if (arr == NULL){
	cmpq	$0, -8(%rbp)	#, arr
	jne	.L136	#,
# src/Runtime/runtime.c:506:         print(arr);
	movq	-8(%rbp), %rax	# arr, tmp88
	movq	%rax, %rdi	# tmp88,
	call	print	#
# src/Runtime/runtime.c:507:         return 0;
	movl	$0, %eax	#, _9
	jmp	.L137	#
.L136:
# src/Runtime/runtime.c:509:     __incRef(arr);
	movq	-8(%rbp), %rax	# arr, tmp89
	movq	%rax, %rdi	# tmp89,
	call	__incRef	#
# src/Runtime/runtime.c:510:     fwrite(arr->data, sizeof(int8_t), arr->length, stdout);
	movq	stdout(%rip), %rcx	# stdout, stdout.217_5
# src/Runtime/runtime.c:510:     fwrite(arr->data, sizeof(int8_t), arr->length, stdout);
	movq	-8(%rbp), %rax	# arr, tmp90
	movl	32(%rax), %eax	# arr_11(D)->length, _6
# src/Runtime/runtime.c:510:     fwrite(arr->data, sizeof(int8_t), arr->length, stdout);
	movslq	%eax, %rdx	# _6, _7
# src/Runtime/runtime.c:510:     fwrite(arr->data, sizeof(int8_t), arr->length, stdout);
	movq	-8(%rbp), %rax	# arr, tmp91
	movq	8(%rax), %rax	# arr_11(D)->data, _8
# src/Runtime/runtime.c:510:     fwrite(arr->data, sizeof(int8_t), arr->length, stdout);
	movl	$1, %esi	#,
	movq	%rax, %rdi	# _8,
	call	fwrite@PLT	#
# src/Runtime/runtime.c:511:     __decRef(arr);
	movq	-8(%rbp), %rax	# arr, tmp92
	movq	%rax, %rdi	# tmp92,
	call	__decRef	#
# src/Runtime/runtime.c:513:     return 0;
	movl	$0, %eax	#, _9
.L137:
# src/Runtime/runtime.c:514: }
	leave	
	.cfi_def_cfa 7, 8
	ret	
	.cfi_endproc
.LFE41:
	.size	printBinArray, .-printBinArray
	.section	.rodata
.LC17:
	.string	"%s\n"
.LC18:
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
# src/Runtime/runtime.c:518:     if (errMsg != NULL)
	movq	errMsg(%rip), %rax	# errMsg, errMsg.226_2
# src/Runtime/runtime.c:518:     if (errMsg != NULL)
	testq	%rax, %rax	# errMsg.226_2
	je	.L139	#,
# src/Runtime/runtime.c:519:         fprintf(stderr, "%s\n", errMsg);
	movq	errMsg(%rip), %rdx	# errMsg, errMsg.227_3
	movq	stderr(%rip), %rax	# stderr, stderr.228_4
	leaq	.LC17(%rip), %rcx	#, tmp87
	movq	%rcx, %rsi	# tmp87,
	movq	%rax, %rdi	# stderr.228_4,
	movl	$0, %eax	#,
	call	fprintf@PLT	#
	jmp	.L140	#
.L139:
# src/Runtime/runtime.c:521:         fprintf(stderr, "%s\n", "ERROR: User error.");
	movq	stderr(%rip), %rax	# stderr, stderr.229_5
	leaq	.LC18(%rip), %rdx	#, tmp88
	leaq	.LC17(%rip), %rcx	#, tmp89
	movq	%rcx, %rsi	# tmp89,
	movq	%rax, %rdi	# stderr.229_5,
	movl	$0, %eax	#,
	call	fprintf@PLT	#
.L140:
# src/Runtime/runtime.c:523:     exit(1);
	movl	$1, %edi	#,
	call	exit@PLT	#
	.cfi_endproc
.LFE42:
	.size	error, .-error
	.section	.rodata
.LC19:
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
# src/Runtime/runtime.c:529:     char *line = NULL;
	movq	$0, -24(%rbp)	#, line
# src/Runtime/runtime.c:530:     size_t size = 0;
	movq	$0, -32(%rbp)	#, size
# src/Runtime/runtime.c:531:     ssize_t unused = getline(&line, &size, stdin);
	movq	stdin(%rip), %rdx	# stdin, stdin.234_1
	leaq	-32(%rbp), %rcx	#, tmp87
	leaq	-24(%rbp), %rax	#, tmp88
	movq	%rcx, %rsi	# tmp87,
	movq	%rax, %rdi	# tmp88,
	call	getline@PLT	#
	movq	%rax, -8(%rbp)	# tmp89, unused
# src/Runtime/runtime.c:532:     int unused2 = sscanf(line, "%d ", &i);
	movq	-24(%rbp), %rax	# line, line.235_2
	leaq	-16(%rbp), %rdx	#, tmp90
	leaq	.LC19(%rip), %rcx	#, tmp91
	movq	%rcx, %rsi	# tmp91,
	movq	%rax, %rdi	# line.235_2,
	movl	$0, %eax	#,
	call	__isoc99_sscanf@PLT	#
	movl	%eax, -12(%rbp)	# tmp92, unused2
# src/Runtime/runtime.c:533:     free(line);
	movq	-24(%rbp), %rax	# line, line.236_3
	movq	%rax, %rdi	# line.236_3,
	call	free@PLT	#
# src/Runtime/runtime.c:534:     return i;
	movl	-16(%rbp), %eax	# i, _12
# src/Runtime/runtime.c:535: }
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
# src/Runtime/runtime.c:537:     char *line = NULL;
	movq	$0, -24(%rbp)	#, line
# src/Runtime/runtime.c:538:     size_t size = 0;
	movq	$0, -32(%rbp)	#, size
# src/Runtime/runtime.c:539:     ssize_t unused = getline(&line, &size, stdin);
	movq	stdin(%rip), %rdx	# stdin, stdin.237_1
	leaq	-32(%rbp), %rcx	#, tmp93
	leaq	-24(%rbp), %rax	#, tmp94
	movq	%rcx, %rsi	# tmp93,
	movq	%rax, %rdi	# tmp94,
	call	getline@PLT	#
	movq	%rax, -8(%rbp)	# tmp95, unused
# src/Runtime/runtime.c:540:     size = u8_strlen(line);
	movq	-24(%rbp), %rax	# line, line.238_2
	movq	%rax, %rdi	# line.238_2,
	call	u8_strlen@PLT	#
# src/Runtime/runtime.c:540:     size = u8_strlen(line);
	movq	%rax, -32(%rbp)	# _3, size
# src/Runtime/runtime.c:541:     line[size - 1] = 0; // remove newline
	movq	-24(%rbp), %rax	# line, line.239_4
	movq	-32(%rbp), %rdx	# size, size.240_5
	subq	$1, %rdx	#, _6
	addq	%rdx, %rax	# _6, _7
# src/Runtime/runtime.c:541:     line[size - 1] = 0; // remove newline
	movb	$0, (%rax)	#, *_7
# src/Runtime/runtime.c:542:     obj l = __createString(line);
	movq	-24(%rbp), %rax	# line, line.241_8
	movq	%rax, %rdi	# line.241_8,
	call	__createString	#
	movq	%rax, -16(%rbp)	# tmp96, l
# src/Runtime/runtime.c:543:     __incRef(l);
	movq	-16(%rbp), %rax	# l, tmp97
	movq	%rax, %rdi	# tmp97,
	call	__incRef	#
# src/Runtime/runtime.c:544:     free(line);
	movq	-24(%rbp), %rax	# line, line.242_9
	movq	%rax, %rdi	# line.242_9,
	call	free@PLT	#
# src/Runtime/runtime.c:545:     return l;
	movq	-16(%rbp), %rax	# l, _21
# src/Runtime/runtime.c:546: }
	leave	
	.cfi_def_cfa 7, 8
	ret	
	.cfi_endproc
.LFE44:
	.size	readString, .-readString
	.ident	"GCC: (Debian 12.2.0-14) 12.2.0"
	.section	.note.GNU-stack,"",@progbits
