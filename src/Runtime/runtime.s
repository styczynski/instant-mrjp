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
	.section	.rodata
.LC0:
	.string	"~#LATCINSTR#~ "
.LC1:
	.string	"Calling __new v3"
	.align 8
.LC2:
	.string	"Perform reference malloc type=%p"
	.align 8
.LC3:
	.string	"__new examine type: <type parent=%p> [%d]"
.LC4:
	.string	"Set __new type/counter"
	.align 8
.LC5:
	.string	"Do init for non array non string?"
.LC6:
	.string	"Perform init"
.LC7:
	.string	"Just set data to NULL"
	.align 8
.LC8:
	.string	"Completed __new %p <type %p, par %p> inner data=%p size=%d"
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
# src/Runtime/runtime.c:28:     DEBUG("Calling __new v3");
	movq	stderr(%rip), %rax	# stderr, stderr.0_1
	movq	%rax, %rcx	# stderr.0_1,
	movl	$14, %edx	#,
	movl	$1, %esi	#,
	leaq	.LC0(%rip), %rax	#, tmp132
	movq	%rax, %rdi	# tmp132,
	call	fwrite@PLT	#
	movq	stderr(%rip), %rax	# stderr, stderr.1_2
	movq	%rax, %rcx	# stderr.1_2,
	movl	$16, %edx	#,
	movl	$1, %esi	#,
	leaq	.LC1(%rip), %rax	#, tmp133
	movq	%rax, %rdi	# tmp133,
	call	fwrite@PLT	#
	movq	stderr(%rip), %rax	# stderr, stderr.2_3
	movq	%rax, %rsi	# stderr.2_3,
	movl	$10, %edi	#,
	call	fputc@PLT	#
	movq	stderr(%rip), %rax	# stderr, stderr.3_4
	movq	%rax, %rdi	# stderr.3_4,
	call	fflush@PLT	#
# src/Runtime/runtime.c:29:     DEBUG("Perform reference malloc type=%p", FORMAT_PTR(t));
	movq	stderr(%rip), %rax	# stderr, stderr.4_5
	movq	%rax, %rcx	# stderr.4_5,
	movl	$14, %edx	#,
	movl	$1, %esi	#,
	leaq	.LC0(%rip), %rax	#, tmp134
	movq	%rax, %rdi	# tmp134,
	call	fwrite@PLT	#
	movq	stderr(%rip), %rax	# stderr, stderr.5_6
	movq	-24(%rbp), %rdx	# t, tmp135
	leaq	.LC2(%rip), %rcx	#, tmp136
	movq	%rcx, %rsi	# tmp136,
	movq	%rax, %rdi	# stderr.5_6,
	movl	$0, %eax	#,
	call	fprintf@PLT	#
	movq	stderr(%rip), %rax	# stderr, stderr.6_7
	movq	%rax, %rsi	# stderr.6_7,
	movl	$10, %edi	#,
	call	fputc@PLT	#
	movq	stderr(%rip), %rax	# stderr, stderr.7_8
	movq	%rax, %rdi	# stderr.7_8,
	call	fflush@PLT	#
# src/Runtime/runtime.c:30:     DEBUG("__new examine type: <type parent=%p> [%d]", FORMAT_PTR(t->parent), t->dataSize);
	movq	stderr(%rip), %rax	# stderr, stderr.8_9
	movq	%rax, %rcx	# stderr.8_9,
	movl	$14, %edx	#,
	movl	$1, %esi	#,
	leaq	.LC0(%rip), %rax	#, tmp137
	movq	%rax, %rdi	# tmp137,
	call	fwrite@PLT	#
	movq	-24(%rbp), %rax	# t, tmp138
	movl	8(%rax), %ecx	# t_56(D)->dataSize, _10
	movq	-24(%rbp), %rax	# t, tmp139
	movq	(%rax), %rdx	# t_56(D)->parent, _11
	movq	stderr(%rip), %rax	# stderr, stderr.9_12
	leaq	.LC3(%rip), %rsi	#, tmp140
	movq	%rax, %rdi	# stderr.9_12,
	movl	$0, %eax	#,
	call	fprintf@PLT	#
	movq	stderr(%rip), %rax	# stderr, stderr.10_13
	movq	%rax, %rsi	# stderr.10_13,
	movl	$10, %edi	#,
	call	fputc@PLT	#
	movq	stderr(%rip), %rax	# stderr, stderr.11_14
	movq	%rax, %rdi	# stderr.11_14,
	call	fflush@PLT	#
# src/Runtime/runtime.c:31:     obj r = malloc(sizeof(struct Reference)+10);
	movl	$38, %edi	#,
	call	malloc@PLT	#
	movq	%rax, -8(%rbp)	# tmp141, r
# src/Runtime/runtime.c:32:     DEBUG("Set __new type/counter");
	movq	stderr(%rip), %rax	# stderr, stderr.12_15
	movq	%rax, %rcx	# stderr.12_15,
	movl	$14, %edx	#,
	movl	$1, %esi	#,
	leaq	.LC0(%rip), %rax	#, tmp142
	movq	%rax, %rdi	# tmp142,
	call	fwrite@PLT	#
	movq	stderr(%rip), %rax	# stderr, stderr.13_16
	movq	%rax, %rcx	# stderr.13_16,
	movl	$22, %edx	#,
	movl	$1, %esi	#,
	leaq	.LC4(%rip), %rax	#, tmp143
	movq	%rax, %rdi	# tmp143,
	call	fwrite@PLT	#
	movq	stderr(%rip), %rax	# stderr, stderr.14_17
	movq	%rax, %rsi	# stderr.14_17,
	movl	$10, %edi	#,
	call	fputc@PLT	#
	movq	stderr(%rip), %rax	# stderr, stderr.15_18
	movq	%rax, %rdi	# stderr.15_18,
	call	fflush@PLT	#
# src/Runtime/runtime.c:33:     r->type = t;
	movq	-8(%rbp), %rax	# r, tmp144
	movq	-24(%rbp), %rdx	# t, tmp145
	movq	%rdx, (%rax)	# tmp145, r_65->type
# src/Runtime/runtime.c:34:     r->counter = 0;
	movq	-8(%rbp), %rax	# r, tmp146
	movl	$0, 16(%rax)	#, r_65->counter
# src/Runtime/runtime.c:35:     DEBUG("Do init for non array non string?");
	movq	stderr(%rip), %rax	# stderr, stderr.16_19
	movq	%rax, %rcx	# stderr.16_19,
	movl	$14, %edx	#,
	movl	$1, %esi	#,
	leaq	.LC0(%rip), %rax	#, tmp147
	movq	%rax, %rdi	# tmp147,
	call	fwrite@PLT	#
	movq	stderr(%rip), %rax	# stderr, stderr.17_20
	movq	%rax, %rcx	# stderr.17_20,
	movl	$33, %edx	#,
	movl	$1, %esi	#,
	leaq	.LC5(%rip), %rax	#, tmp148
	movq	%rax, %rdi	# tmp148,
	call	fwrite@PLT	#
	movq	stderr(%rip), %rax	# stderr, stderr.18_21
	movq	%rax, %rsi	# stderr.18_21,
	movl	$10, %edi	#,
	call	fputc@PLT	#
	movq	stderr(%rip), %rax	# stderr, stderr.19_22
	movq	%rax, %rdi	# stderr.19_22,
	call	fflush@PLT	#
# src/Runtime/runtime.c:36:     if (t->dataSize > 0 && t != &_class_Array && t != &_class_String) {
	movq	-24(%rbp), %rax	# t, tmp149
	movl	8(%rax), %eax	# t_56(D)->dataSize, _23
# src/Runtime/runtime.c:36:     if (t->dataSize > 0 && t != &_class_Array && t != &_class_String) {
	testl	%eax, %eax	# _23
	jle	.L2	#,
# src/Runtime/runtime.c:36:     if (t->dataSize > 0 && t != &_class_Array && t != &_class_String) {
	leaq	_class_Array(%rip), %rax	#, tmp150
	cmpq	%rax, -24(%rbp)	# tmp150, t
	je	.L2	#,
# src/Runtime/runtime.c:36:     if (t->dataSize > 0 && t != &_class_Array && t != &_class_String) {
	leaq	_class_String(%rip), %rax	#, tmp151
	cmpq	%rax, -24(%rbp)	# tmp151, t
	je	.L2	#,
# src/Runtime/runtime.c:37:         DEBUG("Perform init");
	movq	stderr(%rip), %rax	# stderr, stderr.20_24
	movq	%rax, %rcx	# stderr.20_24,
	movl	$14, %edx	#,
	movl	$1, %esi	#,
	leaq	.LC0(%rip), %rax	#, tmp152
	movq	%rax, %rdi	# tmp152,
	call	fwrite@PLT	#
	movq	stderr(%rip), %rax	# stderr, stderr.21_25
	movq	%rax, %rcx	# stderr.21_25,
	movl	$12, %edx	#,
	movl	$1, %esi	#,
	leaq	.LC6(%rip), %rax	#, tmp153
	movq	%rax, %rdi	# tmp153,
	call	fwrite@PLT	#
	movq	stderr(%rip), %rax	# stderr, stderr.22_26
	movq	%rax, %rsi	# stderr.22_26,
	movl	$10, %edi	#,
	call	fputc@PLT	#
	movq	stderr(%rip), %rax	# stderr, stderr.23_27
	movq	%rax, %rdi	# stderr.23_27,
	call	fflush@PLT	#
# src/Runtime/runtime.c:38:         r->data = malloc(t->dataSize);
	movq	-24(%rbp), %rax	# t, tmp154
	movl	8(%rax), %eax	# t_56(D)->dataSize, _28
# src/Runtime/runtime.c:38:         r->data = malloc(t->dataSize);
	cltq
	movq	%rax, %rdi	# _29,
	call	malloc@PLT	#
	movq	%rax, %rdx	# tmp155, _30
# src/Runtime/runtime.c:38:         r->data = malloc(t->dataSize);
	movq	-8(%rbp), %rax	# r, tmp156
	movq	%rdx, 8(%rax)	# _30, r_65->data
# src/Runtime/runtime.c:39:         bzero(r->data, t->dataSize);
	movq	-24(%rbp), %rax	# t, tmp157
	movl	8(%rax), %eax	# t_56(D)->dataSize, _31
# src/Runtime/runtime.c:39:         bzero(r->data, t->dataSize);
	movslq	%eax, %rdx	# _31, _32
	movq	-8(%rbp), %rax	# r, tmp158
	movq	8(%rax), %rax	# r_65->data, _33
	movl	$0, %esi	#,
	movq	%rax, %rdi	# tmp159,
	call	memset@PLT	#
	jmp	.L3	#
.L2:
# src/Runtime/runtime.c:41:         DEBUG("Just set data to NULL");
	movq	stderr(%rip), %rax	# stderr, stderr.24_34
	movq	%rax, %rcx	# stderr.24_34,
	movl	$14, %edx	#,
	movl	$1, %esi	#,
	leaq	.LC0(%rip), %rax	#, tmp163
	movq	%rax, %rdi	# tmp163,
	call	fwrite@PLT	#
	movq	stderr(%rip), %rax	# stderr, stderr.25_35
	movq	%rax, %rcx	# stderr.25_35,
	movl	$21, %edx	#,
	movl	$1, %esi	#,
	leaq	.LC7(%rip), %rax	#, tmp164
	movq	%rax, %rdi	# tmp164,
	call	fwrite@PLT	#
	movq	stderr(%rip), %rax	# stderr, stderr.26_36
	movq	%rax, %rsi	# stderr.26_36,
	movl	$10, %edi	#,
	call	fputc@PLT	#
	movq	stderr(%rip), %rax	# stderr, stderr.27_37
	movq	%rax, %rdi	# stderr.27_37,
	call	fflush@PLT	#
# src/Runtime/runtime.c:42:         r->data = NULL;
	movq	-8(%rbp), %rax	# r, tmp165
	movq	$0, 8(%rax)	#, r_65->data
.L3:
# src/Runtime/runtime.c:44:     r->methods = r->type->methods;
	movq	-8(%rbp), %rax	# r, tmp166
	movq	(%rax), %rax	# r_65->type, _38
# src/Runtime/runtime.c:44:     r->methods = r->type->methods;
	movq	12(%rax), %rdx	# _38->methods, _39
# src/Runtime/runtime.c:44:     r->methods = r->type->methods;
	movq	-8(%rbp), %rax	# r, tmp167
	movq	%rdx, 20(%rax)	# _39, r_65->methods
# src/Runtime/runtime.c:45:     DEBUG("Completed __new %p <type %p, par %p> inner data=%p size=%d", FORMAT_PTR(r), FORMAT_PTR(r->type), FORMAT_PTR(r->type->parent), FORMAT_PTR(r->data), t->dataSize);
	movq	stderr(%rip), %rax	# stderr, stderr.28_40
	movq	%rax, %rcx	# stderr.28_40,
	movl	$14, %edx	#,
	movl	$1, %esi	#,
	leaq	.LC0(%rip), %rax	#, tmp168
	movq	%rax, %rdi	# tmp168,
	call	fwrite@PLT	#
	movq	-24(%rbp), %rax	# t, tmp169
	movl	8(%rax), %esi	# t_56(D)->dataSize, _41
	movq	-8(%rbp), %rax	# r, tmp170
	movq	8(%rax), %r8	# r_65->data, _42
	movq	-8(%rbp), %rax	# r, tmp171
	movq	(%rax), %rax	# r_65->type, _43
	movq	(%rax), %rdi	# _43->parent, _44
	movq	-8(%rbp), %rax	# r, tmp172
	movq	(%rax), %rcx	# r_65->type, _45
	movq	stderr(%rip), %rax	# stderr, stderr.29_46
	movq	-8(%rbp), %rdx	# r, tmp173
	subq	$8, %rsp	#,
	pushq	%rsi	# _41
	movq	%r8, %r9	# _42,
	movq	%rdi, %r8	# _44,
	leaq	.LC8(%rip), %rsi	#, tmp174
	movq	%rax, %rdi	# stderr.29_46,
	movl	$0, %eax	#,
	call	fprintf@PLT	#
	addq	$16, %rsp	#,
	movq	stderr(%rip), %rax	# stderr, stderr.30_47
	movq	%rax, %rsi	# stderr.30_47,
	movl	$10, %edi	#,
	call	fputc@PLT	#
	movq	stderr(%rip), %rax	# stderr, stderr.31_48
	movq	%rax, %rdi	# stderr.31_48,
	call	fflush@PLT	#
# src/Runtime/runtime.c:46:     return r;
	movq	-8(%rbp), %rax	# r, _93
# src/Runtime/runtime.c:47: }
	leave	
	.cfi_def_cfa 7, 8
	ret	
	.cfi_endproc
.LFE6:
	.size	__new, .-__new
	.section	.rodata
.LC9:
	.string	"__free %p"
	.text
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
# src/Runtime/runtime.c:50:     DEBUG("__free %p", FORMAT_PTR(r));
	movq	stderr(%rip), %rax	# stderr, stderr.32_1
	movq	%rax, %rcx	# stderr.32_1,
	movl	$14, %edx	#,
	movl	$1, %esi	#,
	leaq	.LC0(%rip), %rax	#, tmp97
	movq	%rax, %rdi	# tmp97,
	call	fwrite@PLT	#
	movq	stderr(%rip), %rax	# stderr, stderr.33_2
	movq	-40(%rbp), %rdx	# r, tmp98
	leaq	.LC9(%rip), %rcx	#, tmp99
	movq	%rcx, %rsi	# tmp99,
	movq	%rax, %rdi	# stderr.33_2,
	movl	$0, %eax	#,
	call	fprintf@PLT	#
	movq	stderr(%rip), %rax	# stderr, stderr.34_3
	movq	%rax, %rsi	# stderr.34_3,
	movl	$10, %edi	#,
	call	fputc@PLT	#
	movq	stderr(%rip), %rax	# stderr, stderr.35_4
	movq	%rax, %rdi	# stderr.35_4,
	call	fflush@PLT	#
# src/Runtime/runtime.c:51:     if (r->type == &_class_Array) {
	movq	-40(%rbp), %rax	# r, tmp100
	movq	(%rax), %rdx	# r_23(D)->type, _5
# src/Runtime/runtime.c:51:     if (r->type == &_class_Array) {
	leaq	_class_Array(%rip), %rax	#, tmp101
	cmpq	%rax, %rdx	# tmp101, _5
	jne	.L6	#,
# src/Runtime/runtime.c:52:         struct Array *arr = r->data;
	movq	-40(%rbp), %rax	# r, tmp102
	movq	8(%rax), %rax	# r_23(D)->data, tmp103
	movq	%rax, -24(%rbp)	# tmp103, arr
# src/Runtime/runtime.c:53:         void **els = arr->elements;
	movq	-24(%rbp), %rax	# arr, tmp104
	movq	8(%rax), %rax	# arr_29->elements, tmp105
	movq	%rax, -32(%rbp)	# tmp105, els
# src/Runtime/runtime.c:54:         if (arr->elementSize == sizeof(void *)) {
	movq	-24(%rbp), %rax	# arr, tmp106
	movl	(%rax), %eax	# arr_29->elementSize, _6
# src/Runtime/runtime.c:54:         if (arr->elementSize == sizeof(void *)) {
	cmpl	$8, %eax	#, _6
	jne	.L7	#,
# src/Runtime/runtime.c:55:             for (int i = 0; i < arr->length; i++)
	movl	$0, -4(%rbp)	#, i
# src/Runtime/runtime.c:55:             for (int i = 0; i < arr->length; i++)
	jmp	.L8	#
.L9:
# src/Runtime/runtime.c:56:                 __decRef(els[i]);
	movl	-4(%rbp), %eax	# i, tmp107
	cltq
	leaq	0(,%rax,8), %rdx	#, _8
	movq	-32(%rbp), %rax	# els, tmp108
	addq	%rdx, %rax	# _8, _9
# src/Runtime/runtime.c:56:                 __decRef(els[i]);
	movq	(%rax), %rax	# *_9, _10
	movq	%rax, %rdi	# _10,
	call	__decRef	#
# src/Runtime/runtime.c:55:             for (int i = 0; i < arr->length; i++)
	addl	$1, -4(%rbp)	#, i
.L8:
# src/Runtime/runtime.c:55:             for (int i = 0; i < arr->length; i++)
	movq	-24(%rbp), %rax	# arr, tmp109
	movl	4(%rax), %eax	# arr_29->length, _11
# src/Runtime/runtime.c:55:             for (int i = 0; i < arr->length; i++)
	cmpl	%eax, -4(%rbp)	# _11, i
	jl	.L9	#,
.L7:
# src/Runtime/runtime.c:58:         if (els != NULL)
	cmpq	$0, -32(%rbp)	#, els
	je	.L10	#,
# src/Runtime/runtime.c:59:             free(els);
	movq	-32(%rbp), %rax	# els, tmp110
	movq	%rax, %rdi	# tmp110,
	call	free@PLT	#
	jmp	.L10	#
.L6:
# src/Runtime/runtime.c:60:     } else if (r->type == &_class_String) {
	movq	-40(%rbp), %rax	# r, tmp111
	movq	(%rax), %rdx	# r_23(D)->type, _12
# src/Runtime/runtime.c:60:     } else if (r->type == &_class_String) {
	leaq	_class_String(%rip), %rax	#, tmp112
	cmpq	%rax, %rdx	# tmp112, _12
	jne	.L10	#,
# src/Runtime/runtime.c:61:         void *els = ((struct String *)r->data)->data;
	movq	-40(%rbp), %rax	# r, tmp113
	movq	8(%rax), %rax	# r_23(D)->data, _13
# src/Runtime/runtime.c:61:         void *els = ((struct String *)r->data)->data;
	movq	8(%rax), %rax	# MEM[(struct String *)_13].data, tmp114
	movq	%rax, -16(%rbp)	# tmp114, els
# src/Runtime/runtime.c:62:         if (els != NULL && els != emptyString)
	cmpq	$0, -16(%rbp)	#, els
	je	.L10	#,
# src/Runtime/runtime.c:62:         if (els != NULL && els != emptyString)
	leaq	emptyString(%rip), %rax	#, tmp115
	cmpq	%rax, -16(%rbp)	# tmp115, els
	je	.L10	#,
# src/Runtime/runtime.c:63:             free(els);
	movq	-16(%rbp), %rax	# els, tmp116
	movq	%rax, %rdi	# tmp116,
	call	free@PLT	#
.L10:
# src/Runtime/runtime.c:65:     if (r->data != NULL)
	movq	-40(%rbp), %rax	# r, tmp117
	movq	8(%rax), %rax	# r_23(D)->data, _14
# src/Runtime/runtime.c:65:     if (r->data != NULL)
	testq	%rax, %rax	# _14
	je	.L11	#,
# src/Runtime/runtime.c:66:         free(r->data);
	movq	-40(%rbp), %rax	# r, tmp118
	movq	8(%rax), %rax	# r_23(D)->data, _15
	movq	%rax, %rdi	# _15,
	call	free@PLT	#
.L11:
# src/Runtime/runtime.c:67:     free(r);
	movq	-40(%rbp), %rax	# r, tmp119
	movq	%rax, %rdi	# tmp119,
	call	free@PLT	#
# src/Runtime/runtime.c:68: }
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
# src/Runtime/runtime.c:71:     if (!__LATTE_RUNTIME_GC_ENABLED) return;
	nop	
# src/Runtime/runtime.c:77: }
	popq	%rbp	#
	.cfi_def_cfa 7, 8
	ret	
	.cfi_endproc
.LFE8:
	.size	__incRef, .-__incRef
	.section	.rodata
.LC10:
	.string	"__decRef %p"
	.text
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
	subq	$16, %rsp	#,
	movq	%rdi, -8(%rbp)	# r, r
# src/Runtime/runtime.c:79:     DEBUG("__decRef %p", FORMAT_PTR(r));
	movq	stderr(%rip), %rax	# stderr, stderr.44_1
	movq	%rax, %rcx	# stderr.44_1,
	movl	$14, %edx	#,
	movl	$1, %esi	#,
	leaq	.LC0(%rip), %rax	#, tmp86
	movq	%rax, %rdi	# tmp86,
	call	fwrite@PLT	#
	movq	stderr(%rip), %rax	# stderr, stderr.45_2
	movq	-8(%rbp), %rdx	# r, tmp87
	leaq	.LC10(%rip), %rcx	#, tmp88
	movq	%rcx, %rsi	# tmp88,
	movq	%rax, %rdi	# stderr.45_2,
	movl	$0, %eax	#,
	call	fprintf@PLT	#
	movq	stderr(%rip), %rax	# stderr, stderr.46_3
	movq	%rax, %rsi	# stderr.46_3,
	movl	$10, %edi	#,
	call	fputc@PLT	#
	movq	stderr(%rip), %rax	# stderr, stderr.47_4
	movq	%rax, %rdi	# stderr.47_4,
	call	fflush@PLT	#
# src/Runtime/runtime.c:80:     if (!__LATTE_RUNTIME_GC_ENABLED) return;
	nop	
# src/Runtime/runtime.c:92: }
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
# src/Runtime/runtime.c:94: obj __newRefArray(int32_t length) { return __newArray(sizeof(obj), length); }
	movl	-4(%rbp), %eax	# length, tmp84
	movl	%eax, %esi	# tmp84,
	movl	$8, %edi	#,
	call	__newArray	#
# src/Runtime/runtime.c:94: obj __newRefArray(int32_t length) { return __newArray(sizeof(obj), length); }
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
# src/Runtime/runtime.c:96:     return __newArray(sizeof(int32_t), length);
	movl	-4(%rbp), %eax	# length, tmp84
	movl	%eax, %esi	# tmp84,
	movl	$4, %edi	#,
	call	__newArray	#
# src/Runtime/runtime.c:97: }
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
# src/Runtime/runtime.c:99:     return __newArray(sizeof(int8_t), length);
	movl	-4(%rbp), %eax	# length, tmp84
	movl	%eax, %esi	# tmp84,
	movl	$1, %edi	#,
	call	__newArray	#
# src/Runtime/runtime.c:100: }
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
# src/Runtime/runtime.c:102:     obj r = __new(&_class_Array);
	leaq	_class_Array(%rip), %rax	#, tmp90
	movq	%rax, %rdi	# tmp90,
	call	__new	#
	movq	%rax, -8(%rbp)	# tmp91, r
# src/Runtime/runtime.c:103:     struct Array *arr = malloc(sizeof(struct Array)+size * length);
	movl	-20(%rbp), %eax	# size, tmp92
	imull	-24(%rbp), %eax	# length, _1
	cltq
# src/Runtime/runtime.c:103:     struct Array *arr = malloc(sizeof(struct Array)+size * length);
	addq	$16, %rax	#, _3
	movq	%rax, %rdi	# _3,
	call	malloc@PLT	#
	movq	%rax, -16(%rbp)	# tmp93, arr
# src/Runtime/runtime.c:104:     r->data = arr;
	movq	-8(%rbp), %rax	# r, tmp94
	movq	-16(%rbp), %rdx	# arr, tmp95
	movq	%rdx, 8(%rax)	# tmp95, r_10->data
# src/Runtime/runtime.c:105:     arr->elementSize = size;
	movq	-16(%rbp), %rax	# arr, tmp96
	movl	-20(%rbp), %edx	# size, tmp97
	movl	%edx, (%rax)	# tmp97, arr_14->elementSize
# src/Runtime/runtime.c:106:     arr->length = length;
	movq	-16(%rbp), %rax	# arr, tmp98
	movl	-24(%rbp), %edx	# length, tmp99
	movl	%edx, 4(%rax)	# tmp99, arr_14->length
# src/Runtime/runtime.c:107:     if (length > 0) {
	cmpl	$0, -24(%rbp)	#, length
	jle	.L23	#,
# src/Runtime/runtime.c:109:         bzero(&(arr->elements), size * length);
	movl	-20(%rbp), %eax	# size, tmp100
	imull	-24(%rbp), %eax	# length, _4
# src/Runtime/runtime.c:109:         bzero(&(arr->elements), size * length);
	cltq
# src/Runtime/runtime.c:109:         bzero(&(arr->elements), size * length);
	movq	-16(%rbp), %rdx	# arr, tmp101
	addq	$8, %rdx	#, _6
# src/Runtime/runtime.c:109:         bzero(&(arr->elements), size * length);
	movq	%rdx, %rcx	# _6, tmp102
	movq	%rax, %rdx	# tmp103,
	movl	$0, %esi	#,
	movq	%rcx, %rdi	# tmp102,
	call	memset@PLT	#
	jmp	.L24	#
.L23:
# src/Runtime/runtime.c:111:         arr->elements = NULL;
	movq	-16(%rbp), %rax	# arr, tmp106
	movq	$0, 8(%rax)	#, arr_14->elements
.L24:
# src/Runtime/runtime.c:113:     return r;
	movq	-8(%rbp), %rax	# r, _20
# src/Runtime/runtime.c:114: }
	leave	
	.cfi_def_cfa 7, 8
	ret	
	.cfi_endproc
.LFE13:
	.size	__newArray, .-__newArray
	.section	.rodata
.LC11:
	.string	"ERROR: Array is null."
	.align 8
.LC12:
	.string	"ERROR: Array index out of range."
.LC13:
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
# src/Runtime/runtime.c:117:     if (array == NULL) {
	cmpq	$0, -24(%rbp)	#, array
	jne	.L27	#,
# src/Runtime/runtime.c:118:         errMsg = "ERROR: Array is null.";
	leaq	.LC11(%rip), %rax	#, tmp91
	movq	%rax, errMsg(%rip)	# tmp91, errMsg
# src/Runtime/runtime.c:119:         error();
	movl	$0, %eax	#,
	call	error	#
.L27:
# src/Runtime/runtime.c:121:     struct Array *arr = ((struct Array *)array->data);
	movq	-24(%rbp), %rax	# array, tmp92
	movq	8(%rax), %rax	# array_10(D)->data, tmp93
	movq	%rax, -8(%rbp)	# tmp93, arr
# src/Runtime/runtime.c:122:     if (index >= arr->length || index < 0) {
	movq	-8(%rbp), %rax	# arr, tmp94
	movl	4(%rax), %eax	# arr_14->length, _1
# src/Runtime/runtime.c:122:     if (index >= arr->length || index < 0) {
	cmpl	%eax, -28(%rbp)	# _1, index
	jge	.L28	#,
# src/Runtime/runtime.c:122:     if (index >= arr->length || index < 0) {
	cmpl	$0, -28(%rbp)	#, index
	jns	.L29	#,
.L28:
# src/Runtime/runtime.c:123:         errMsg = "ERROR: Array index out of range.";
	leaq	.LC12(%rip), %rax	#, tmp95
	movq	%rax, errMsg(%rip)	# tmp95, errMsg
# src/Runtime/runtime.c:124:         fprintf(stderr, "%d, %d\n", index, arr->length);
	movq	-8(%rbp), %rax	# arr, tmp96
	movl	4(%rax), %ecx	# arr_14->length, _2
	movq	stderr(%rip), %rax	# stderr, stderr.52_3
	movl	-28(%rbp), %edx	# index, tmp97
	leaq	.LC13(%rip), %rsi	#, tmp98
	movq	%rax, %rdi	# stderr.52_3,
	movl	$0, %eax	#,
	call	fprintf@PLT	#
# src/Runtime/runtime.c:125:         error();
	movl	$0, %eax	#,
	call	error	#
.L29:
# src/Runtime/runtime.c:127:     return arr->elements + index * arr->elementSize;
	movq	-8(%rbp), %rax	# arr, tmp99
	movq	8(%rax), %rdx	# arr_14->elements, _4
# src/Runtime/runtime.c:127:     return arr->elements + index * arr->elementSize;
	movq	-8(%rbp), %rax	# arr, tmp100
	movl	(%rax), %eax	# arr_14->elementSize, _5
# src/Runtime/runtime.c:127:     return arr->elements + index * arr->elementSize;
	imull	-28(%rbp), %eax	# index, _6
	cltq
# src/Runtime/runtime.c:127:     return arr->elements + index * arr->elementSize;
	addq	%rdx, %rax	# _4, _19
# src/Runtime/runtime.c:128: }
	leave	
	.cfi_def_cfa 7, 8
	ret	
	.cfi_endproc
.LFE14:
	.size	__getelementptr, .-__getelementptr
	.section	.rodata
.LC14:
	.string	"__cast"
.LC15:
	.string	"__cast %p %p [%d]"
.LC16:
	.string	"__cast object is null"
.LC17:
	.string	"__cast get underlying type"
	.align 8
.LC18:
	.string	"__cast iterate parent upward %p [%d]"
	.align 8
.LC19:
	.string	"__cast found correct parent: %p"
.LC20:
	.string	"__cast loop, break %p"
.LC21:
	.string	"finished the cast (FAILED)"
	.text
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
	subq	$32, %rsp	#,
	movq	%rdi, -24(%rbp)	# o, o
	movq	%rsi, -32(%rbp)	# t, t
# src/Runtime/runtime.c:131:     DEBUG("__cast");
	movq	stderr(%rip), %rax	# stderr, stderr.53_1
	movq	%rax, %rcx	# stderr.53_1,
	movl	$14, %edx	#,
	movl	$1, %esi	#,
	leaq	.LC0(%rip), %rax	#, tmp118
	movq	%rax, %rdi	# tmp118,
	call	fwrite@PLT	#
	movq	stderr(%rip), %rax	# stderr, stderr.54_2
	movq	%rax, %rcx	# stderr.54_2,
	movl	$6, %edx	#,
	movl	$1, %esi	#,
	leaq	.LC14(%rip), %rax	#, tmp119
	movq	%rax, %rdi	# tmp119,
	call	fwrite@PLT	#
	movq	stderr(%rip), %rax	# stderr, stderr.55_3
	movq	%rax, %rsi	# stderr.55_3,
	movl	$10, %edi	#,
	call	fputc@PLT	#
	movq	stderr(%rip), %rax	# stderr, stderr.56_4
	movq	%rax, %rdi	# stderr.56_4,
	call	fflush@PLT	#
# src/Runtime/runtime.c:132:     DEBUG("__cast %p %p [%d]", FORMAT_PTR(o), FORMAT_PTR(t), t->dataSize);
	movq	stderr(%rip), %rax	# stderr, stderr.57_5
	movq	%rax, %rcx	# stderr.57_5,
	movl	$14, %edx	#,
	movl	$1, %esi	#,
	leaq	.LC0(%rip), %rax	#, tmp120
	movq	%rax, %rdi	# tmp120,
	call	fwrite@PLT	#
	movq	-32(%rbp), %rax	# t, tmp121
	movl	8(%rax), %esi	# t_46(D)->dataSize, _6
	movq	stderr(%rip), %rax	# stderr, stderr.58_7
	movq	-32(%rbp), %rcx	# t, tmp122
	movq	-24(%rbp), %rdx	# o, tmp123
	movl	%esi, %r8d	# _6,
	leaq	.LC15(%rip), %rsi	#, tmp124
	movq	%rax, %rdi	# stderr.58_7,
	movl	$0, %eax	#,
	call	fprintf@PLT	#
	movq	stderr(%rip), %rax	# stderr, stderr.59_8
	movq	%rax, %rsi	# stderr.59_8,
	movl	$10, %edi	#,
	call	fputc@PLT	#
	movq	stderr(%rip), %rax	# stderr, stderr.60_9
	movq	%rax, %rdi	# stderr.60_9,
	call	fflush@PLT	#
# src/Runtime/runtime.c:133:     if (o == NULL) {
	cmpq	$0, -24(%rbp)	#, o
	jne	.L32	#,
# src/Runtime/runtime.c:134:         DEBUG("__cast object is null");
	movq	stderr(%rip), %rax	# stderr, stderr.61_10
	movq	%rax, %rcx	# stderr.61_10,
	movl	$14, %edx	#,
	movl	$1, %esi	#,
	leaq	.LC0(%rip), %rax	#, tmp125
	movq	%rax, %rdi	# tmp125,
	call	fwrite@PLT	#
	movq	stderr(%rip), %rax	# stderr, stderr.62_11
	movq	%rax, %rcx	# stderr.62_11,
	movl	$21, %edx	#,
	movl	$1, %esi	#,
	leaq	.LC16(%rip), %rax	#, tmp126
	movq	%rax, %rdi	# tmp126,
	call	fwrite@PLT	#
	movq	stderr(%rip), %rax	# stderr, stderr.63_12
	movq	%rax, %rsi	# stderr.63_12,
	movl	$10, %edi	#,
	call	fputc@PLT	#
	movq	stderr(%rip), %rax	# stderr, stderr.64_13
	movq	%rax, %rdi	# stderr.64_13,
	call	fflush@PLT	#
# src/Runtime/runtime.c:135:         return NULL;
	movl	$0, %eax	#, _36
	jmp	.L33	#
.L32:
# src/Runtime/runtime.c:137:     DEBUG("__cast get underlying type");
	movq	stderr(%rip), %rax	# stderr, stderr.65_14
	movq	%rax, %rcx	# stderr.65_14,
	movl	$14, %edx	#,
	movl	$1, %esi	#,
	leaq	.LC0(%rip), %rax	#, tmp127
	movq	%rax, %rdi	# tmp127,
	call	fwrite@PLT	#
	movq	stderr(%rip), %rax	# stderr, stderr.66_15
	movq	%rax, %rcx	# stderr.66_15,
	movl	$26, %edx	#,
	movl	$1, %esi	#,
	leaq	.LC17(%rip), %rax	#, tmp128
	movq	%rax, %rdi	# tmp128,
	call	fwrite@PLT	#
	movq	stderr(%rip), %rax	# stderr, stderr.67_16
	movq	%rax, %rsi	# stderr.67_16,
	movl	$10, %edi	#,
	call	fputc@PLT	#
	movq	stderr(%rip), %rax	# stderr, stderr.68_17
	movq	%rax, %rdi	# stderr.68_17,
	call	fflush@PLT	#
# src/Runtime/runtime.c:138:     struct Type *to = o->type;
	movq	-24(%rbp), %rax	# o, tmp129
	movq	(%rax), %rax	# o_47(D)->type, tmp130
	movq	%rax, -8(%rbp)	# tmp130, to
# src/Runtime/runtime.c:139:     while (to != NULL) {
	jmp	.L34	#
.L37:
# src/Runtime/runtime.c:140:         DEBUG("__cast iterate parent upward %p [%d]", FORMAT_PTR(to), to->dataSize);
	movq	stderr(%rip), %rax	# stderr, stderr.69_18
	movq	%rax, %rcx	# stderr.69_18,
	movl	$14, %edx	#,
	movl	$1, %esi	#,
	leaq	.LC0(%rip), %rax	#, tmp131
	movq	%rax, %rdi	# tmp131,
	call	fwrite@PLT	#
	movq	-8(%rbp), %rax	# to, tmp132
	movl	8(%rax), %ecx	# to_35->dataSize, _19
	movq	stderr(%rip), %rax	# stderr, stderr.70_20
	movq	-8(%rbp), %rdx	# to, tmp133
	leaq	.LC18(%rip), %rsi	#, tmp134
	movq	%rax, %rdi	# stderr.70_20,
	movl	$0, %eax	#,
	call	fprintf@PLT	#
	movq	stderr(%rip), %rax	# stderr, stderr.71_21
	movq	%rax, %rsi	# stderr.71_21,
	movl	$10, %edi	#,
	call	fputc@PLT	#
	movq	stderr(%rip), %rax	# stderr, stderr.72_22
	movq	%rax, %rdi	# stderr.72_22,
	call	fflush@PLT	#
# src/Runtime/runtime.c:141:         if (t == to) {
	movq	-32(%rbp), %rax	# t, tmp135
	cmpq	-8(%rbp), %rax	# to, tmp135
	jne	.L35	#,
# src/Runtime/runtime.c:142:             DEBUG("__cast found correct parent: %p", FORMAT_PTR(to));
	movq	stderr(%rip), %rax	# stderr, stderr.73_23
	movq	%rax, %rcx	# stderr.73_23,
	movl	$14, %edx	#,
	movl	$1, %esi	#,
	leaq	.LC0(%rip), %rax	#, tmp136
	movq	%rax, %rdi	# tmp136,
	call	fwrite@PLT	#
	movq	stderr(%rip), %rax	# stderr, stderr.74_24
	movq	-8(%rbp), %rdx	# to, tmp137
	leaq	.LC19(%rip), %rcx	#, tmp138
	movq	%rcx, %rsi	# tmp138,
	movq	%rax, %rdi	# stderr.74_24,
	movl	$0, %eax	#,
	call	fprintf@PLT	#
	movq	stderr(%rip), %rax	# stderr, stderr.75_25
	movq	%rax, %rsi	# stderr.75_25,
	movl	$10, %edi	#,
	call	fputc@PLT	#
	movq	stderr(%rip), %rax	# stderr, stderr.76_26
	movq	%rax, %rdi	# stderr.76_26,
	call	fflush@PLT	#
# src/Runtime/runtime.c:143:             return o;
	movq	-24(%rbp), %rax	# o, _36
	jmp	.L33	#
.L35:
# src/Runtime/runtime.c:145:         struct Type *prev = to;
	movq	-8(%rbp), %rax	# to, tmp139
	movq	%rax, -16(%rbp)	# tmp139, prev
# src/Runtime/runtime.c:146:         to = to->parent;
	movq	-8(%rbp), %rax	# to, tmp140
	movq	(%rax), %rax	# to_35->parent, tmp141
	movq	%rax, -8(%rbp)	# tmp141, to
# src/Runtime/runtime.c:147:         if (prev == to) {
	movq	-16(%rbp), %rax	# prev, tmp142
	cmpq	-8(%rbp), %rax	# to, tmp142
	jne	.L34	#,
# src/Runtime/runtime.c:148:             DEBUG("__cast loop, break %p", FORMAT_PTR(to));
	movq	stderr(%rip), %rax	# stderr, stderr.77_27
	movq	%rax, %rcx	# stderr.77_27,
	movl	$14, %edx	#,
	movl	$1, %esi	#,
	leaq	.LC0(%rip), %rax	#, tmp143
	movq	%rax, %rdi	# tmp143,
	call	fwrite@PLT	#
	movq	stderr(%rip), %rax	# stderr, stderr.78_28
	movq	-8(%rbp), %rdx	# to, tmp144
	leaq	.LC20(%rip), %rcx	#, tmp145
	movq	%rcx, %rsi	# tmp145,
	movq	%rax, %rdi	# stderr.78_28,
	movl	$0, %eax	#,
	call	fprintf@PLT	#
	movq	stderr(%rip), %rax	# stderr, stderr.79_29
	movq	%rax, %rsi	# stderr.79_29,
	movl	$10, %edi	#,
	call	fputc@PLT	#
	movq	stderr(%rip), %rax	# stderr, stderr.80_30
	movq	%rax, %rdi	# stderr.80_30,
	call	fflush@PLT	#
# src/Runtime/runtime.c:149:             break;
	jmp	.L38	#
.L34:
# src/Runtime/runtime.c:139:     while (to != NULL) {
	cmpq	$0, -8(%rbp)	#, to
	jne	.L37	#,
.L38:
# src/Runtime/runtime.c:152:     DEBUG("finished the cast (FAILED)");
	movq	stderr(%rip), %rax	# stderr, stderr.81_31
	movq	%rax, %rcx	# stderr.81_31,
	movl	$14, %edx	#,
	movl	$1, %esi	#,
	leaq	.LC0(%rip), %rax	#, tmp146
	movq	%rax, %rdi	# tmp146,
	call	fwrite@PLT	#
	movq	stderr(%rip), %rax	# stderr, stderr.82_32
	movq	%rax, %rcx	# stderr.82_32,
	movl	$26, %edx	#,
	movl	$1, %esi	#,
	leaq	.LC21(%rip), %rax	#, tmp147
	movq	%rax, %rdi	# tmp147,
	call	fwrite@PLT	#
	movq	stderr(%rip), %rax	# stderr, stderr.83_33
	movq	%rax, %rsi	# stderr.83_33,
	movl	$10, %edi	#,
	call	fputc@PLT	#
	movq	stderr(%rip), %rax	# stderr, stderr.84_34
	movq	%rax, %rdi	# stderr.84_34,
	call	fflush@PLT	#
# src/Runtime/runtime.c:153:     return NULL;
	movl	$0, %eax	#, _36
.L33:
# src/Runtime/runtime.c:154: }
	leave	
	.cfi_def_cfa 7, 8
	ret	
	.cfi_endproc
.LFE15:
	.size	__cast, .-__cast
	.section	.rodata
	.align 8
.LC22:
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
# src/Runtime/runtime.c:157:     errMsg = "ERROR: Null pointer reference.";
	leaq	.LC22(%rip), %rax	#, tmp82
	movq	%rax, errMsg(%rip)	# tmp82, errMsg
# src/Runtime/runtime.c:158:     error();
	movl	$0, %eax	#,
	call	error	#
# src/Runtime/runtime.c:159: }
	nop	
	popq	%rbp	#
	.cfi_def_cfa 7, 8
	ret	
	.cfi_endproc
.LFE16:
	.size	__errorNull, .-__errorNull
	.section	.rodata
.LC23:
	.string	"Call __createString() v2"
.LC24:
	.string	"Try to create string from %p"
.LC25:
	.string	"C is NULL exit"
.LC26:
	.string	"Perform new on _class_String"
.LC27:
	.string	"String allocated"
.LC28:
	.string	"Measure strlen"
.LC29:
	.string	"Check unicode"
.LC30:
	.string	"Non-unicode string encoding"
	.align 8
.LC31:
	.string	"ERROR: Non-unicode string encoding."
	.align 8
.LC32:
	.string	"Str init completed (with data=%p, data_of_str=%p, size=%d)"
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
# src/Runtime/runtime.c:162:     DEBUG("Call __createString() v2");
	movq	stderr(%rip), %rax	# stderr, stderr.85_1
	movq	%rax, %rcx	# stderr.85_1,
	movl	$14, %edx	#,
	movl	$1, %esi	#,
	leaq	.LC0(%rip), %rax	#, tmp137
	movq	%rax, %rdi	# tmp137,
	call	fwrite@PLT	#
	movq	stderr(%rip), %rax	# stderr, stderr.86_2
	movq	%rax, %rcx	# stderr.86_2,
	movl	$24, %edx	#,
	movl	$1, %esi	#,
	leaq	.LC23(%rip), %rax	#, tmp138
	movq	%rax, %rdi	# tmp138,
	call	fwrite@PLT	#
	movq	stderr(%rip), %rax	# stderr, stderr.87_3
	movq	%rax, %rsi	# stderr.87_3,
	movl	$10, %edi	#,
	call	fputc@PLT	#
	movq	stderr(%rip), %rax	# stderr, stderr.88_4
	movq	%rax, %rdi	# stderr.88_4,
	call	fflush@PLT	#
# src/Runtime/runtime.c:163:     DEBUG("Try to create string from %p", FORMAT_PTR(c));
	movq	stderr(%rip), %rax	# stderr, stderr.89_5
	movq	%rax, %rcx	# stderr.89_5,
	movl	$14, %edx	#,
	movl	$1, %esi	#,
	leaq	.LC0(%rip), %rax	#, tmp139
	movq	%rax, %rdi	# tmp139,
	call	fwrite@PLT	#
	movq	stderr(%rip), %rax	# stderr, stderr.90_6
	movq	-40(%rbp), %rdx	# c, tmp140
	leaq	.LC24(%rip), %rcx	#, tmp141
	movq	%rcx, %rsi	# tmp141,
	movq	%rax, %rdi	# stderr.90_6,
	movl	$0, %eax	#,
	call	fprintf@PLT	#
	movq	stderr(%rip), %rax	# stderr, stderr.91_7
	movq	%rax, %rsi	# stderr.91_7,
	movl	$10, %edi	#,
	call	fputc@PLT	#
	movq	stderr(%rip), %rax	# stderr, stderr.92_8
	movq	%rax, %rdi	# stderr.92_8,
	call	fflush@PLT	#
# src/Runtime/runtime.c:164:     if (c == NULL) {
	cmpq	$0, -40(%rbp)	#, c
	jne	.L41	#,
# src/Runtime/runtime.c:165:         DEBUG("C is NULL exit");
	movq	stderr(%rip), %rax	# stderr, stderr.93_9
	movq	%rax, %rcx	# stderr.93_9,
	movl	$14, %edx	#,
	movl	$1, %esi	#,
	leaq	.LC0(%rip), %rax	#, tmp142
	movq	%rax, %rdi	# tmp142,
	call	fwrite@PLT	#
	movq	stderr(%rip), %rax	# stderr, stderr.94_10
	movq	%rax, %rcx	# stderr.94_10,
	movl	$14, %edx	#,
	movl	$1, %esi	#,
	leaq	.LC25(%rip), %rax	#, tmp143
	movq	%rax, %rdi	# tmp143,
	call	fwrite@PLT	#
	movq	stderr(%rip), %rax	# stderr, stderr.95_11
	movq	%rax, %rsi	# stderr.95_11,
	movl	$10, %edi	#,
	call	fputc@PLT	#
	movq	stderr(%rip), %rax	# stderr, stderr.96_12
	movq	%rax, %rdi	# stderr.96_12,
	call	fflush@PLT	#
# src/Runtime/runtime.c:166:         return __createString(emptyString);
	leaq	emptyString(%rip), %rax	#, tmp144
	movq	%rax, %rdi	# tmp144,
	call	__createString	#
	jmp	.L42	#
.L41:
# src/Runtime/runtime.c:168:     DEBUG("Perform new on _class_String");
	movq	stderr(%rip), %rax	# stderr, stderr.97_13
	movq	%rax, %rcx	# stderr.97_13,
	movl	$14, %edx	#,
	movl	$1, %esi	#,
	leaq	.LC0(%rip), %rax	#, tmp145
	movq	%rax, %rdi	# tmp145,
	call	fwrite@PLT	#
	movq	stderr(%rip), %rax	# stderr, stderr.98_14
	movq	%rax, %rcx	# stderr.98_14,
	movl	$28, %edx	#,
	movl	$1, %esi	#,
	leaq	.LC26(%rip), %rax	#, tmp146
	movq	%rax, %rdi	# tmp146,
	call	fwrite@PLT	#
	movq	stderr(%rip), %rax	# stderr, stderr.99_15
	movq	%rax, %rsi	# stderr.99_15,
	movl	$10, %edi	#,
	call	fputc@PLT	#
	movq	stderr(%rip), %rax	# stderr, stderr.100_16
	movq	%rax, %rdi	# stderr.100_16,
	call	fflush@PLT	#
# src/Runtime/runtime.c:169:     obj r = __new(&_class_String);
	leaq	_class_String(%rip), %rax	#, tmp147
	movq	%rax, %rdi	# tmp147,
	call	__new	#
	movq	%rax, -8(%rbp)	# tmp148, r
# src/Runtime/runtime.c:170:     DEBUG("String allocated");
	movq	stderr(%rip), %rax	# stderr, stderr.101_17
	movq	%rax, %rcx	# stderr.101_17,
	movl	$14, %edx	#,
	movl	$1, %esi	#,
	leaq	.LC0(%rip), %rax	#, tmp149
	movq	%rax, %rdi	# tmp149,
	call	fwrite@PLT	#
	movq	stderr(%rip), %rax	# stderr, stderr.102_18
	movq	%rax, %rcx	# stderr.102_18,
	movl	$16, %edx	#,
	movl	$1, %esi	#,
	leaq	.LC27(%rip), %rax	#, tmp150
	movq	%rax, %rdi	# tmp150,
	call	fwrite@PLT	#
	movq	stderr(%rip), %rax	# stderr, stderr.103_19
	movq	%rax, %rsi	# stderr.103_19,
	movl	$10, %edi	#,
	call	fputc@PLT	#
	movq	stderr(%rip), %rax	# stderr, stderr.104_20
	movq	%rax, %rdi	# stderr.104_20,
	call	fflush@PLT	#
# src/Runtime/runtime.c:171:     struct String *str = malloc(sizeof(struct String));
	movl	$16, %edi	#,
	call	malloc@PLT	#
	movq	%rax, -16(%rbp)	# tmp151, str
# src/Runtime/runtime.c:172:     r->data = str;
	movq	-8(%rbp), %rax	# r, tmp152
	movq	-16(%rbp), %rdx	# str, tmp153
	movq	%rdx, 8(%rax)	# tmp153, r_72->data
# src/Runtime/runtime.c:173:     DEBUG("Measure strlen");
	movq	stderr(%rip), %rax	# stderr, stderr.105_21
	movq	%rax, %rcx	# stderr.105_21,
	movl	$14, %edx	#,
	movl	$1, %esi	#,
	leaq	.LC0(%rip), %rax	#, tmp154
	movq	%rax, %rdi	# tmp154,
	call	fwrite@PLT	#
	movq	stderr(%rip), %rax	# stderr, stderr.106_22
	movq	%rax, %rcx	# stderr.106_22,
	movl	$14, %edx	#,
	movl	$1, %esi	#,
	leaq	.LC28(%rip), %rax	#, tmp155
	movq	%rax, %rdi	# tmp155,
	call	fwrite@PLT	#
	movq	stderr(%rip), %rax	# stderr, stderr.107_23
	movq	%rax, %rsi	# stderr.107_23,
	movl	$10, %edi	#,
	call	fputc@PLT	#
	movq	stderr(%rip), %rax	# stderr, stderr.108_24
	movq	%rax, %rdi	# stderr.108_24,
	call	fflush@PLT	#
# src/Runtime/runtime.c:174:     str->length = u8_strlen(c);
	movq	-40(%rbp), %rax	# c, tmp156
	movq	%rax, %rdi	# tmp156,
	call	u8_strlen@PLT	#
# src/Runtime/runtime.c:174:     str->length = u8_strlen(c);
	movl	%eax, %edx	# _25, _26
	movq	-16(%rbp), %rax	# str, tmp157
	movl	%edx, (%rax)	# _26, str_78->length
# src/Runtime/runtime.c:175:     DEBUG("Check unicode");
	movq	stderr(%rip), %rax	# stderr, stderr.109_27
	movq	%rax, %rcx	# stderr.109_27,
	movl	$14, %edx	#,
	movl	$1, %esi	#,
	leaq	.LC0(%rip), %rax	#, tmp158
	movq	%rax, %rdi	# tmp158,
	call	fwrite@PLT	#
	movq	stderr(%rip), %rax	# stderr, stderr.110_28
	movq	%rax, %rcx	# stderr.110_28,
	movl	$13, %edx	#,
	movl	$1, %esi	#,
	leaq	.LC29(%rip), %rax	#, tmp159
	movq	%rax, %rdi	# tmp159,
	call	fwrite@PLT	#
	movq	stderr(%rip), %rax	# stderr, stderr.111_29
	movq	%rax, %rsi	# stderr.111_29,
	movl	$10, %edi	#,
	call	fputc@PLT	#
	movq	stderr(%rip), %rax	# stderr, stderr.112_30
	movq	%rax, %rdi	# stderr.112_30,
	call	fflush@PLT	#
# src/Runtime/runtime.c:176:     if (u8_check(c, str->length) != NULL) {
	movq	-16(%rbp), %rax	# str, tmp160
	movl	(%rax), %eax	# str_78->length, _31
# src/Runtime/runtime.c:176:     if (u8_check(c, str->length) != NULL) {
	movslq	%eax, %rdx	# _31, _32
	movq	-40(%rbp), %rax	# c, tmp161
	movq	%rdx, %rsi	# _32,
	movq	%rax, %rdi	# tmp161,
	call	u8_check@PLT	#
# src/Runtime/runtime.c:176:     if (u8_check(c, str->length) != NULL) {
	testq	%rax, %rax	# _33
	je	.L43	#,
# src/Runtime/runtime.c:177:         DEBUG("Non-unicode string encoding", c);
	movq	stderr(%rip), %rax	# stderr, stderr.113_34
	movq	%rax, %rcx	# stderr.113_34,
	movl	$14, %edx	#,
	movl	$1, %esi	#,
	leaq	.LC0(%rip), %rax	#, tmp162
	movq	%rax, %rdi	# tmp162,
	call	fwrite@PLT	#
	movq	stderr(%rip), %rax	# stderr, stderr.114_35
	movq	-40(%rbp), %rdx	# c, tmp163
	leaq	.LC30(%rip), %rcx	#, tmp164
	movq	%rcx, %rsi	# tmp164,
	movq	%rax, %rdi	# stderr.114_35,
	movl	$0, %eax	#,
	call	fprintf@PLT	#
	movq	stderr(%rip), %rax	# stderr, stderr.115_36
	movq	%rax, %rsi	# stderr.115_36,
	movl	$10, %edi	#,
	call	fputc@PLT	#
	movq	stderr(%rip), %rax	# stderr, stderr.116_37
	movq	%rax, %rdi	# stderr.116_37,
	call	fflush@PLT	#
# src/Runtime/runtime.c:178:         errMsg = "ERROR: Non-unicode string encoding.";
	leaq	.LC31(%rip), %rax	#, tmp165
	movq	%rax, errMsg(%rip)	# tmp165, errMsg
# src/Runtime/runtime.c:179:         error();
	movl	$0, %eax	#,
	call	error	#
.L43:
# src/Runtime/runtime.c:181:     if (str->length > 0) {
	movq	-16(%rbp), %rax	# str, tmp166
	movl	(%rax), %eax	# str_78->length, _38
# src/Runtime/runtime.c:181:     if (str->length > 0) {
	testl	%eax, %eax	# _38
	jle	.L44	#,
# src/Runtime/runtime.c:182:         int len = str->length;
	movq	-16(%rbp), %rax	# str, tmp167
	movl	(%rax), %eax	# str_78->length, tmp168
	movl	%eax, -20(%rbp)	# tmp168, len
# src/Runtime/runtime.c:183:         str->data = malloc(len + 1);
	movl	-20(%rbp), %eax	# len, tmp169
	addl	$1, %eax	#, _39
# src/Runtime/runtime.c:183:         str->data = malloc(len + 1);
	cltq
	movq	%rax, %rdi	# _40,
	call	malloc@PLT	#
	movq	%rax, %rdx	# tmp170, _41
# src/Runtime/runtime.c:183:         str->data = malloc(len + 1);
	movq	-16(%rbp), %rax	# str, tmp171
	movq	%rdx, 8(%rax)	# _41, str_78->data
# src/Runtime/runtime.c:184:         memcpy(str->data, c, len);
	movl	-20(%rbp), %eax	# len, tmp172
	movslq	%eax, %rdx	# tmp172, _42
# src/Runtime/runtime.c:184:         memcpy(str->data, c, len);
	movq	-16(%rbp), %rax	# str, tmp173
	movq	8(%rax), %rax	# str_78->data, _43
# src/Runtime/runtime.c:184:         memcpy(str->data, c, len);
	movq	-40(%rbp), %rcx	# c, tmp174
	movq	%rcx, %rsi	# tmp174,
	movq	%rax, %rdi	# _43,
	call	memcpy@PLT	#
# src/Runtime/runtime.c:185:         str->data[len] = 0;
	movq	-16(%rbp), %rax	# str, tmp175
	movq	8(%rax), %rdx	# str_78->data, _44
# src/Runtime/runtime.c:185:         str->data[len] = 0;
	movl	-20(%rbp), %eax	# len, tmp176
	cltq
	addq	%rdx, %rax	# _44, _46
# src/Runtime/runtime.c:185:         str->data[len] = 0;
	movb	$0, (%rax)	#, *_46
# src/Runtime/runtime.c:190:     DEBUG("Str init completed (with data=%p, data_of_str=%p, size=%d)", FORMAT_PTR(str->data), FORMAT_PTR(r->data), str->length);
	movq	stderr(%rip), %rax	# stderr, stderr.117_47
	movq	%rax, %rcx	# stderr.117_47,
	movl	$14, %edx	#,
	movl	$1, %esi	#,
	leaq	.LC0(%rip), %rax	#, tmp179
	movq	%rax, %rdi	# tmp179,
	call	fwrite@PLT	#
	movq	-16(%rbp), %rax	# str, tmp180
	movl	(%rax), %esi	# str_78->length, _48
	movq	-8(%rbp), %rax	# r, tmp181
	movq	8(%rax), %rcx	# r_72->data, _49
	movq	-16(%rbp), %rax	# str, tmp182
	movq	8(%rax), %rdx	# str_78->data, _50
	movq	stderr(%rip), %rax	# stderr, stderr.118_51
	movl	%esi, %r8d	# _48,
	leaq	.LC32(%rip), %rsi	#, tmp183
	movq	%rax, %rdi	# stderr.118_51,
	movl	$0, %eax	#,
	call	fprintf@PLT	#
	movq	stderr(%rip), %rax	# stderr, stderr.119_52
	movq	%rax, %rsi	# stderr.119_52,
	movl	$10, %edi	#,
	call	fputc@PLT	#
	movq	stderr(%rip), %rax	# stderr, stderr.120_53
	movq	%rax, %rdi	# stderr.120_53,
	call	fflush@PLT	#
# src/Runtime/runtime.c:191:     str->length = -1;
	movq	-16(%rbp), %rax	# str, tmp184
	movl	$-1, (%rax)	#, str_78->length
# src/Runtime/runtime.c:192:     return r;
	movq	-8(%rbp), %rax	# r, _54
	jmp	.L42	#
.L44:
# src/Runtime/runtime.c:187:         str->data = emptyString;
	movq	-16(%rbp), %rax	# str, tmp177
	leaq	emptyString(%rip), %rdx	#, tmp178
	movq	%rdx, 8(%rax)	# tmp178, str_78->data
# src/Runtime/runtime.c:188:         return r;
	movq	-8(%rbp), %rax	# r, _54
.L42:
# src/Runtime/runtime.c:193: }
	leave	
	.cfi_def_cfa 7, 8
	ret	
	.cfi_endproc
.LFE17:
	.size	__createString, .-__createString
	.section	.rodata
.LC33:
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
# src/Runtime/runtime.c:197:     obj ret = __createString("Object");
	leaq	.LC33(%rip), %rax	#, tmp84
	movq	%rax, %rdi	# tmp84,
	call	__createString	#
	movq	%rax, -8(%rbp)	# tmp85, ret
# src/Runtime/runtime.c:198:     __incRef(ret);
	movq	-8(%rbp), %rax	# ret, tmp86
	movq	%rax, %rdi	# tmp86,
	call	__incRef	#
# src/Runtime/runtime.c:199:     return ret;
	movq	-8(%rbp), %rax	# ret, _5
# src/Runtime/runtime.c:200: }
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
# src/Runtime/runtime.c:201: int32_t _Object_getHashCode(obj o) { return (int32_t)(int64_t)o; }
	movq	-8(%rbp), %rax	# o, o.121_1
# src/Runtime/runtime.c:201: int32_t _Object_getHashCode(obj o) { return (int32_t)(int64_t)o; }
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
# src/Runtime/runtime.c:202: int8_t _Object_equals(obj o1, obj o2) { return o1 == o2; }
	movq	-8(%rbp), %rax	# o1, tmp85
	cmpq	-16(%rbp), %rax	# o2, tmp85
	sete	%al	#, _1
# src/Runtime/runtime.c:202: int8_t _Object_equals(obj o1, obj o2) { return o1 == o2; }
	popq	%rbp	#
	.cfi_def_cfa 7, 8
	ret	
	.cfi_endproc
.LFE20:
	.size	_Object_equals, .-_Object_equals
	.section	.rodata
.LC34:
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
# src/Runtime/runtime.c:205:     char start[] = "[";
	movw	$91, -122(%rbp)	#, start
# src/Runtime/runtime.c:206:     char delim[] = ", ";
	movw	$8236, -125(%rbp)	#, delim
	movb	$0, -123(%rbp)	#, delim
# src/Runtime/runtime.c:207:     char end[] = "]";
	movw	$93, -127(%rbp)	#, end
# src/Runtime/runtime.c:208:     struct Array *array = arr->data;
	movq	-136(%rbp), %rax	# arr, tmp188
	movq	8(%rax), %rax	# arr_118(D)->data, tmp189
	movq	%rax, -40(%rbp)	# tmp189, array
# src/Runtime/runtime.c:210:     obj *strings = malloc(sizeof(obj) * array->length);
	movq	-40(%rbp), %rax	# array, tmp190
	movl	4(%rax), %eax	# array_119->length, _1
	cltq
# src/Runtime/runtime.c:210:     obj *strings = malloc(sizeof(obj) * array->length);
	salq	$3, %rax	#, _3
	movq	%rax, %rdi	# _3,
	call	malloc@PLT	#
	movq	%rax, -48(%rbp)	# tmp191, strings
# src/Runtime/runtime.c:211:     int32_t *lenghts = malloc(sizeof(int32_t) * array->length);
	movq	-40(%rbp), %rax	# array, tmp192
	movl	4(%rax), %eax	# array_119->length, _4
	cltq
# src/Runtime/runtime.c:211:     int32_t *lenghts = malloc(sizeof(int32_t) * array->length);
	salq	$2, %rax	#, _6
	movq	%rax, %rdi	# _6,
	call	malloc@PLT	#
	movq	%rax, -56(%rbp)	# tmp193, lenghts
# src/Runtime/runtime.c:212:     int32_t totalLenght = 0;
	movl	$0, -20(%rbp)	#, totalLenght
# src/Runtime/runtime.c:214:     for (int i = 0; i < array->length; i++) {
	movl	$0, -24(%rbp)	#, i
# src/Runtime/runtime.c:214:     for (int i = 0; i < array->length; i++) {
	jmp	.L53	#
.L58:
# src/Runtime/runtime.c:215:         if (array->elementSize == sizeof(int32_t)) {
	movq	-40(%rbp), %rax	# array, tmp194
	movl	(%rax), %eax	# array_119->elementSize, _7
# src/Runtime/runtime.c:215:         if (array->elementSize == sizeof(int32_t)) {
	cmpl	$4, %eax	#, _7
	jne	.L54	#,
# src/Runtime/runtime.c:216:             int32_t *elements = array->elements;
	movq	-40(%rbp), %rax	# array, tmp195
	movq	8(%rax), %rax	# array_119->elements, tmp196
	movq	%rax, -120(%rbp)	# tmp196, elements
# src/Runtime/runtime.c:217:             strings[i] = intToString(elements[i]);
	movl	-24(%rbp), %eax	# i, tmp197
	cltq
	leaq	0(,%rax,4), %rdx	#, _9
	movq	-120(%rbp), %rax	# elements, tmp198
	addq	%rdx, %rax	# _9, _10
# src/Runtime/runtime.c:217:             strings[i] = intToString(elements[i]);
	movl	(%rax), %eax	# *_10, _11
# src/Runtime/runtime.c:217:             strings[i] = intToString(elements[i]);
	movl	-24(%rbp), %edx	# i, tmp199
	movslq	%edx, %rdx	# tmp199, _12
	leaq	0(,%rdx,8), %rcx	#, _13
	movq	-48(%rbp), %rdx	# strings, tmp200
	leaq	(%rcx,%rdx), %rbx	#, _14
# src/Runtime/runtime.c:217:             strings[i] = intToString(elements[i]);
	movl	%eax, %edi	# _11,
	call	intToString	#
# src/Runtime/runtime.c:217:             strings[i] = intToString(elements[i]);
	movq	%rax, (%rbx)	# _15, *_14
	jmp	.L55	#
.L54:
# src/Runtime/runtime.c:218:         } else if (array->elementSize == sizeof(int8_t)) {
	movq	-40(%rbp), %rax	# array, tmp201
	movl	(%rax), %eax	# array_119->elementSize, _16
# src/Runtime/runtime.c:218:         } else if (array->elementSize == sizeof(int8_t)) {
	cmpl	$1, %eax	#, _16
	jne	.L56	#,
# src/Runtime/runtime.c:219:             int8_t *elements = array->elements;
	movq	-40(%rbp), %rax	# array, tmp202
	movq	8(%rax), %rax	# array_119->elements, tmp203
	movq	%rax, -112(%rbp)	# tmp203, elements
# src/Runtime/runtime.c:220:             strings[i] = byteToString(elements[i]);
	movl	-24(%rbp), %eax	# i, tmp204
	movslq	%eax, %rdx	# tmp204, _17
	movq	-112(%rbp), %rax	# elements, tmp205
	addq	%rdx, %rax	# _17, _18
	movzbl	(%rax), %eax	# *_18, _19
# src/Runtime/runtime.c:220:             strings[i] = byteToString(elements[i]);
	movzbl	%al, %eax	# _20, _21
# src/Runtime/runtime.c:220:             strings[i] = byteToString(elements[i]);
	movl	-24(%rbp), %edx	# i, tmp206
	movslq	%edx, %rdx	# tmp206, _22
	leaq	0(,%rdx,8), %rcx	#, _23
	movq	-48(%rbp), %rdx	# strings, tmp207
	leaq	(%rcx,%rdx), %rbx	#, _24
# src/Runtime/runtime.c:220:             strings[i] = byteToString(elements[i]);
	movl	%eax, %edi	# _21,
	call	byteToString	#
# src/Runtime/runtime.c:220:             strings[i] = byteToString(elements[i]);
	movq	%rax, (%rbx)	# _25, *_24
	jmp	.L55	#
.L56:
# src/Runtime/runtime.c:222:             obj *elements = array->elements;
	movq	-40(%rbp), %rax	# array, tmp208
	movq	8(%rax), %rax	# array_119->elements, tmp209
	movq	%rax, -88(%rbp)	# tmp209, elements
# src/Runtime/runtime.c:223:             obj element = elements[i];
	movl	-24(%rbp), %eax	# i, tmp210
	cltq
	leaq	0(,%rax,8), %rdx	#, _27
	movq	-88(%rbp), %rax	# elements, tmp211
	addq	%rdx, %rax	# _27, _28
# src/Runtime/runtime.c:223:             obj element = elements[i];
	movq	(%rax), %rax	# *_28, tmp212
	movq	%rax, -96(%rbp)	# tmp212, element
# src/Runtime/runtime.c:224:             if (element == NULL) {
	cmpq	$0, -96(%rbp)	#, element
	jne	.L57	#,
# src/Runtime/runtime.c:225:                 strings[i] = __createString("null");
	movl	-24(%rbp), %eax	# i, tmp213
	cltq
	leaq	0(,%rax,8), %rdx	#, _30
	movq	-48(%rbp), %rax	# strings, tmp214
	leaq	(%rdx,%rax), %rbx	#, _31
# src/Runtime/runtime.c:225:                 strings[i] = __createString("null");
	leaq	.LC34(%rip), %rax	#, tmp215
	movq	%rax, %rdi	# tmp215,
	call	__createString	#
# src/Runtime/runtime.c:225:                 strings[i] = __createString("null");
	movq	%rax, (%rbx)	# _32, *_31
# src/Runtime/runtime.c:226:                 __incRef(strings[i]);
	movl	-24(%rbp), %eax	# i, tmp216
	cltq
	leaq	0(,%rax,8), %rdx	#, _34
	movq	-48(%rbp), %rax	# strings, tmp217
	addq	%rdx, %rax	# _34, _35
# src/Runtime/runtime.c:226:                 __incRef(strings[i]);
	movq	(%rax), %rax	# *_35, _36
	movq	%rax, %rdi	# _36,
	call	__incRef	#
	jmp	.L55	#
.L57:
# src/Runtime/runtime.c:228:                 obj (*toString)(obj) = ((void **)element->type->methods)[0];
	movq	-96(%rbp), %rax	# element, tmp218
	movq	(%rax), %rax	# element_152->type, _37
# src/Runtime/runtime.c:228:                 obj (*toString)(obj) = ((void **)element->type->methods)[0];
	movq	12(%rax), %rax	# _37->methods, _38
# src/Runtime/runtime.c:228:                 obj (*toString)(obj) = ((void **)element->type->methods)[0];
	movq	(%rax), %rax	# MEM[(void * *)_38], _39
# src/Runtime/runtime.c:228:                 obj (*toString)(obj) = ((void **)element->type->methods)[0];
	movq	%rax, -104(%rbp)	# _39, toString
# src/Runtime/runtime.c:229:                 strings[i] = toString(element);
	movl	-24(%rbp), %eax	# i, tmp219
	cltq
	leaq	0(,%rax,8), %rdx	#, _41
	movq	-48(%rbp), %rax	# strings, tmp220
	leaq	(%rdx,%rax), %rbx	#, _42
# src/Runtime/runtime.c:229:                 strings[i] = toString(element);
	movq	-96(%rbp), %rax	# element, tmp221
	movq	-104(%rbp), %rdx	# toString, tmp222
	movq	%rax, %rdi	# tmp221,
	call	*%rdx	# tmp222
# src/Runtime/runtime.c:229:                 strings[i] = toString(element);
	movq	%rax, (%rbx)	# _43, *_42
.L55:
# src/Runtime/runtime.c:232:         lenghts[i] = u8_strlen(((struct String *)strings[i]->data)->data);
	movl	-24(%rbp), %eax	# i, tmp223
	cltq
	leaq	0(,%rax,8), %rdx	#, _45
	movq	-48(%rbp), %rax	# strings, tmp224
	addq	%rdx, %rax	# _45, _46
	movq	(%rax), %rax	# *_46, _47
# src/Runtime/runtime.c:232:         lenghts[i] = u8_strlen(((struct String *)strings[i]->data)->data);
	movq	8(%rax), %rax	# _47->data, _48
# src/Runtime/runtime.c:232:         lenghts[i] = u8_strlen(((struct String *)strings[i]->data)->data);
	movq	8(%rax), %rax	# MEM[(struct String *)_48].data, _49
# src/Runtime/runtime.c:232:         lenghts[i] = u8_strlen(((struct String *)strings[i]->data)->data);
	movq	%rax, %rdi	# _49,
	call	u8_strlen@PLT	#
# src/Runtime/runtime.c:232:         lenghts[i] = u8_strlen(((struct String *)strings[i]->data)->data);
	movl	-24(%rbp), %edx	# i, tmp225
	movslq	%edx, %rdx	# tmp225, _51
	leaq	0(,%rdx,4), %rcx	#, _52
	movq	-56(%rbp), %rdx	# lenghts, tmp226
	addq	%rcx, %rdx	# _52, _53
# src/Runtime/runtime.c:232:         lenghts[i] = u8_strlen(((struct String *)strings[i]->data)->data);
	movl	%eax, (%rdx)	# _54, *_53
# src/Runtime/runtime.c:233:         totalLenght += lenghts[i];
	movl	-24(%rbp), %eax	# i, tmp227
	cltq
	leaq	0(,%rax,4), %rdx	#, _56
	movq	-56(%rbp), %rax	# lenghts, tmp228
	addq	%rdx, %rax	# _56, _57
	movl	(%rax), %eax	# *_57, _58
# src/Runtime/runtime.c:233:         totalLenght += lenghts[i];
	addl	%eax, -20(%rbp)	# _58, totalLenght
# src/Runtime/runtime.c:214:     for (int i = 0; i < array->length; i++) {
	addl	$1, -24(%rbp)	#, i
.L53:
# src/Runtime/runtime.c:214:     for (int i = 0; i < array->length; i++) {
	movq	-40(%rbp), %rax	# array, tmp229
	movl	4(%rax), %eax	# array_119->length, _59
# src/Runtime/runtime.c:214:     for (int i = 0; i < array->length; i++) {
	cmpl	%eax, -24(%rbp)	# _59, i
	jl	.L58	#,
# src/Runtime/runtime.c:236:     int32_t bufferSize = u8_strlen(start) + totalLenght +
	leaq	-122(%rbp), %rax	#, tmp230
	movq	%rax, %rdi	# tmp230,
	call	u8_strlen@PLT	#
# src/Runtime/runtime.c:236:     int32_t bufferSize = u8_strlen(start) + totalLenght +
	movl	%eax, %edx	# _60, _61
	movl	-20(%rbp), %eax	# totalLenght, totalLenght.122_62
	leal	(%rdx,%rax), %ebx	#, _63
# src/Runtime/runtime.c:237:                          (array->length - 1) * u8_strlen(delim) +
	movq	-40(%rbp), %rax	# array, tmp231
	movl	4(%rax), %eax	# array_119->length, _64
# src/Runtime/runtime.c:237:                          (array->length - 1) * u8_strlen(delim) +
	subl	$1, %eax	#, _65
	cltq
# src/Runtime/runtime.c:236:     int32_t bufferSize = u8_strlen(start) + totalLenght +
	movl	%eax, %r12d	# _66, _67
# src/Runtime/runtime.c:237:                          (array->length - 1) * u8_strlen(delim) +
	leaq	-125(%rbp), %rax	#, tmp232
	movq	%rax, %rdi	# tmp232,
	call	u8_strlen@PLT	#
# src/Runtime/runtime.c:236:     int32_t bufferSize = u8_strlen(start) + totalLenght +
	imull	%r12d, %eax	# _67, _70
	addl	%eax, %ebx	# _70, _71
# src/Runtime/runtime.c:238:                          u8_strlen(end) + 1;
	leaq	-127(%rbp), %rax	#, tmp233
	movq	%rax, %rdi	# tmp233,
	call	u8_strlen@PLT	#
# src/Runtime/runtime.c:237:                          (array->length - 1) * u8_strlen(delim) +
	addl	%ebx, %eax	# _71, _74
# src/Runtime/runtime.c:238:                          u8_strlen(end) + 1;
	addl	$1, %eax	#, _75
# src/Runtime/runtime.c:236:     int32_t bufferSize = u8_strlen(start) + totalLenght +
	movl	%eax, -60(%rbp)	# _75, bufferSize
# src/Runtime/runtime.c:239:     uint8_t *buffer = malloc(bufferSize);
	movl	-60(%rbp), %eax	# bufferSize, tmp234
	cltq
	movq	%rax, %rdi	# _76,
	call	malloc@PLT	#
	movq	%rax, -72(%rbp)	# tmp235, buffer
# src/Runtime/runtime.c:240:     int32_t index = 0;
	movl	$0, -28(%rbp)	#, index
# src/Runtime/runtime.c:241:     u8_strcpy(buffer + index, start);
	movl	-28(%rbp), %eax	# index, tmp236
	movslq	%eax, %rdx	# tmp236, _77
	movq	-72(%rbp), %rax	# buffer, tmp237
	addq	%rax, %rdx	# tmp237, _78
	leaq	-122(%rbp), %rax	#, tmp238
	movq	%rax, %rsi	# tmp238,
	movq	%rdx, %rdi	# _78,
	call	u8_strcpy@PLT	#
# src/Runtime/runtime.c:242:     index++;
	addl	$1, -28(%rbp)	#, index
# src/Runtime/runtime.c:243:     for (int i = 0; i < array->length; i++) {
	movl	$0, -32(%rbp)	#, i
# src/Runtime/runtime.c:243:     for (int i = 0; i < array->length; i++) {
	jmp	.L59	#
.L61:
# src/Runtime/runtime.c:244:         u8_strcpy(buffer + index, ((struct String *)strings[i]->data)->data);
	movl	-32(%rbp), %eax	# i, tmp239
	cltq
	leaq	0(,%rax,8), %rdx	#, _80
	movq	-48(%rbp), %rax	# strings, tmp240
	addq	%rdx, %rax	# _80, _81
	movq	(%rax), %rax	# *_81, _82
# src/Runtime/runtime.c:244:         u8_strcpy(buffer + index, ((struct String *)strings[i]->data)->data);
	movq	8(%rax), %rax	# _82->data, _83
# src/Runtime/runtime.c:244:         u8_strcpy(buffer + index, ((struct String *)strings[i]->data)->data);
	movq	8(%rax), %rax	# MEM[(struct String *)_83].data, _84
# src/Runtime/runtime.c:244:         u8_strcpy(buffer + index, ((struct String *)strings[i]->data)->data);
	movl	-28(%rbp), %edx	# index, tmp241
	movslq	%edx, %rcx	# tmp241, _85
	movq	-72(%rbp), %rdx	# buffer, tmp242
	addq	%rcx, %rdx	# _85, _86
	movq	%rax, %rsi	# _84,
	movq	%rdx, %rdi	# _86,
	call	u8_strcpy@PLT	#
# src/Runtime/runtime.c:245:         index += lenghts[i];
	movl	-32(%rbp), %eax	# i, tmp243
	cltq
	leaq	0(,%rax,4), %rdx	#, _88
	movq	-56(%rbp), %rax	# lenghts, tmp244
	addq	%rdx, %rax	# _88, _89
	movl	(%rax), %eax	# *_89, _90
# src/Runtime/runtime.c:245:         index += lenghts[i];
	addl	%eax, -28(%rbp)	# _90, index
# src/Runtime/runtime.c:246:         if (i != array->length - 1) {
	movq	-40(%rbp), %rax	# array, tmp245
	movl	4(%rax), %eax	# array_119->length, _91
# src/Runtime/runtime.c:246:         if (i != array->length - 1) {
	subl	$1, %eax	#, _92
# src/Runtime/runtime.c:246:         if (i != array->length - 1) {
	cmpl	%eax, -32(%rbp)	# _92, i
	je	.L60	#,
# src/Runtime/runtime.c:247:             u8_strcpy(buffer + index, delim);
	movl	-28(%rbp), %eax	# index, tmp246
	movslq	%eax, %rdx	# tmp246, _93
	movq	-72(%rbp), %rax	# buffer, tmp247
	addq	%rax, %rdx	# tmp247, _94
	leaq	-125(%rbp), %rax	#, tmp248
	movq	%rax, %rsi	# tmp248,
	movq	%rdx, %rdi	# _94,
	call	u8_strcpy@PLT	#
# src/Runtime/runtime.c:248:             index += 2;
	addl	$2, -28(%rbp)	#, index
.L60:
# src/Runtime/runtime.c:250:         __decRef(strings[i]);
	movl	-32(%rbp), %eax	# i, tmp249
	cltq
	leaq	0(,%rax,8), %rdx	#, _96
	movq	-48(%rbp), %rax	# strings, tmp250
	addq	%rdx, %rax	# _96, _97
# src/Runtime/runtime.c:250:         __decRef(strings[i]);
	movq	(%rax), %rax	# *_97, _98
	movq	%rax, %rdi	# _98,
	call	__decRef	#
# src/Runtime/runtime.c:243:     for (int i = 0; i < array->length; i++) {
	addl	$1, -32(%rbp)	#, i
.L59:
# src/Runtime/runtime.c:243:     for (int i = 0; i < array->length; i++) {
	movq	-40(%rbp), %rax	# array, tmp251
	movl	4(%rax), %eax	# array_119->length, _99
# src/Runtime/runtime.c:243:     for (int i = 0; i < array->length; i++) {
	cmpl	%eax, -32(%rbp)	# _99, i
	jl	.L61	#,
# src/Runtime/runtime.c:252:     u8_strcpy(buffer + index, end);
	movl	-28(%rbp), %eax	# index, tmp252
	movslq	%eax, %rdx	# tmp252, _100
	movq	-72(%rbp), %rax	# buffer, tmp253
	addq	%rax, %rdx	# tmp253, _101
	leaq	-127(%rbp), %rax	#, tmp254
	movq	%rax, %rsi	# tmp254,
	movq	%rdx, %rdi	# _101,
	call	u8_strcpy@PLT	#
# src/Runtime/runtime.c:253:     buffer[bufferSize - 1] = 0;
	movl	-60(%rbp), %eax	# bufferSize, tmp255
	cltq
	leaq	-1(%rax), %rdx	#, _103
	movq	-72(%rbp), %rax	# buffer, tmp256
	addq	%rdx, %rax	# _103, _104
# src/Runtime/runtime.c:253:     buffer[bufferSize - 1] = 0;
	movb	$0, (%rax)	#, *_104
# src/Runtime/runtime.c:254:     obj ret = __createString(buffer);
	movq	-72(%rbp), %rax	# buffer, tmp257
	movq	%rax, %rdi	# tmp257,
	call	__createString	#
	movq	%rax, -80(%rbp)	# tmp258, ret
# src/Runtime/runtime.c:255:     __incRef(ret);
	movq	-80(%rbp), %rax	# ret, tmp259
	movq	%rax, %rdi	# tmp259,
	call	__incRef	#
# src/Runtime/runtime.c:256:     free(lenghts);
	movq	-56(%rbp), %rax	# lenghts, tmp260
	movq	%rax, %rdi	# tmp260,
	call	free@PLT	#
# src/Runtime/runtime.c:257:     free(strings);
	movq	-48(%rbp), %rax	# strings, tmp261
	movq	%rax, %rdi	# tmp261,
	call	free@PLT	#
# src/Runtime/runtime.c:258:     free(buffer);
	movq	-72(%rbp), %rax	# buffer, tmp262
	movq	%rax, %rdi	# tmp262,
	call	free@PLT	#
# src/Runtime/runtime.c:259:     return ret;
	movq	-80(%rbp), %rax	# ret, _141
# src/Runtime/runtime.c:260: }
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
# src/Runtime/runtime.c:263:     __incRef(str);
	movq	-8(%rbp), %rax	# str, tmp84
	movq	%rax, %rdi	# tmp84,
	call	__incRef	#
# src/Runtime/runtime.c:264:     return str;
	movq	-8(%rbp), %rax	# str, _4
# src/Runtime/runtime.c:265: }
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
# src/Runtime/runtime.c:267:     int32_t hash = 0x811c9dc5;
	movl	$-2128831035, -4(%rbp)	#, hash
# src/Runtime/runtime.c:268:     uint8_t *rawstring = ((struct String *)str->data)->data;
	movq	-40(%rbp), %rax	# str, tmp90
	movq	8(%rax), %rax	# str_11(D)->data, _1
# src/Runtime/runtime.c:268:     uint8_t *rawstring = ((struct String *)str->data)->data;
	movq	8(%rax), %rax	# MEM[(struct String *)_1].data, tmp91
	movq	%rax, -16(%rbp)	# tmp91, rawstring
# src/Runtime/runtime.c:269:     int32_t strlen = u8_strlen(rawstring);
	movq	-16(%rbp), %rax	# rawstring, tmp92
	movq	%rax, %rdi	# tmp92,
	call	u8_strlen@PLT	#
# src/Runtime/runtime.c:269:     int32_t strlen = u8_strlen(rawstring);
	movl	%eax, -20(%rbp)	# _2, strlen
# src/Runtime/runtime.c:270:     for (int i = 0; i < strlen; i++) {
	movl	$0, -8(%rbp)	#, i
# src/Runtime/runtime.c:270:     for (int i = 0; i < strlen; i++) {
	jmp	.L66	#
.L67:
# src/Runtime/runtime.c:271:         hash ^= rawstring[i];
	movl	-8(%rbp), %eax	# i, tmp93
	movslq	%eax, %rdx	# tmp93, _3
	movq	-16(%rbp), %rax	# rawstring, tmp94
	addq	%rdx, %rax	# _3, _4
	movzbl	(%rax), %eax	# *_4, _5
	movzbl	%al, %eax	# _5, _6
# src/Runtime/runtime.c:271:         hash ^= rawstring[i];
	xorl	%eax, -4(%rbp)	# _6, hash
# src/Runtime/runtime.c:272:         hash *= 0x01000193;
	movl	-4(%rbp), %eax	# hash, tmp96
	imull	$16777619, %eax, %eax	#, tmp96, tmp95
	movl	%eax, -4(%rbp)	# tmp95, hash
# src/Runtime/runtime.c:270:     for (int i = 0; i < strlen; i++) {
	addl	$1, -8(%rbp)	#, i
.L66:
# src/Runtime/runtime.c:270:     for (int i = 0; i < strlen; i++) {
	movl	-8(%rbp), %eax	# i, tmp97
	cmpl	-20(%rbp), %eax	# strlen, tmp97
	jl	.L67	#,
# src/Runtime/runtime.c:274:     return hash;
	movl	-4(%rbp), %eax	# hash, _15
# src/Runtime/runtime.c:275: }
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
# src/Runtime/runtime.c:277:     if (o2 == NULL)
	cmpq	$0, -48(%rbp)	#, o2
	jne	.L70	#,
# src/Runtime/runtime.c:278:         return false;
	movl	$0, %eax	#, _8
	jmp	.L71	#
.L70:
# src/Runtime/runtime.c:279:     if (o2->type != &_class_String)
	movq	-48(%rbp), %rax	# o2, tmp91
	movq	(%rax), %rdx	# o2_10(D)->type, _1
# src/Runtime/runtime.c:279:     if (o2->type != &_class_String)
	leaq	_class_String(%rip), %rax	#, tmp92
	cmpq	%rax, %rdx	# tmp92, _1
	je	.L72	#,
# src/Runtime/runtime.c:280:         return false;
	movl	$0, %eax	#, _8
	jmp	.L71	#
.L72:
# src/Runtime/runtime.c:281:     if (_String_length(o1) != _String_length(o2))
	movq	-40(%rbp), %rax	# o1, tmp93
	movq	%rax, %rdi	# tmp93,
	call	_String_length	#
	movl	%eax, %ebx	#, _2
# src/Runtime/runtime.c:281:     if (_String_length(o1) != _String_length(o2))
	movq	-48(%rbp), %rax	# o2, tmp94
	movq	%rax, %rdi	# tmp94,
	call	_String_length	#
# src/Runtime/runtime.c:281:     if (_String_length(o1) != _String_length(o2))
	cmpl	%eax, %ebx	# _3, _2
	je	.L73	#,
# src/Runtime/runtime.c:282:         return false;
	movl	$0, %eax	#, _8
	jmp	.L71	#
.L73:
# src/Runtime/runtime.c:283:     uint8_t *rs1 = ((struct String *)o1->data)->data;
	movq	-40(%rbp), %rax	# o1, tmp95
	movq	8(%rax), %rax	# o1_12(D)->data, _4
# src/Runtime/runtime.c:283:     uint8_t *rs1 = ((struct String *)o1->data)->data;
	movq	8(%rax), %rax	# MEM[(struct String *)_4].data, tmp96
	movq	%rax, -24(%rbp)	# tmp96, rs1
# src/Runtime/runtime.c:284:     uint8_t *rs2 = ((struct String *)o2->data)->data;
	movq	-48(%rbp), %rax	# o2, tmp97
	movq	8(%rax), %rax	# o2_10(D)->data, _5
# src/Runtime/runtime.c:284:     uint8_t *rs2 = ((struct String *)o2->data)->data;
	movq	8(%rax), %rax	# MEM[(struct String *)_5].data, tmp98
	movq	%rax, -32(%rbp)	# tmp98, rs2
# src/Runtime/runtime.c:285:     return u8_strcmp(rs1, rs2) == 0;
	movq	-32(%rbp), %rdx	# rs2, tmp99
	movq	-24(%rbp), %rax	# rs1, tmp100
	movq	%rdx, %rsi	# tmp99,
	movq	%rax, %rdi	# tmp100,
	call	u8_strcmp@PLT	#
# src/Runtime/runtime.c:285:     return u8_strcmp(rs1, rs2) == 0;
	testl	%eax, %eax	# _6
	sete	%al	#, _7
.L71:
# src/Runtime/runtime.c:286: }
	movq	-8(%rbp), %rbx	#,
	leave	
	.cfi_def_cfa 7, 8
	ret	
	.cfi_endproc
.LFE24:
	.size	_String_equals, .-_String_equals
	.section	.rodata
	.align 8
.LC35:
	.string	"ERROR: Substring with negative length."
.LC36:
	.string	""
	.align 8
.LC37:
	.string	"ERROR: Substring starting index is too big."
	.align 8
.LC38:
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
# src/Runtime/runtime.c:288:     if (length < 0) {
	cmpl	$0, -80(%rbp)	#, length
	jns	.L75	#,
# src/Runtime/runtime.c:289:         errMsg = "ERROR: Substring with negative length.";
	leaq	.LC35(%rip), %rax	#, tmp102
	movq	%rax, errMsg(%rip)	# tmp102, errMsg
# src/Runtime/runtime.c:290:         error();
	movl	$0, %eax	#,
	call	error	#
.L75:
# src/Runtime/runtime.c:292:     if (length == 0)
	cmpl	$0, -80(%rbp)	#, length
	jne	.L76	#,
# src/Runtime/runtime.c:293:         return __createString("");
	leaq	.LC36(%rip), %rax	#, tmp103
	movq	%rax, %rdi	# tmp103,
	call	__createString	#
	jmp	.L84	#
.L76:
# src/Runtime/runtime.c:294:     if (startIndex >= _String_length(str)) {
	movq	-72(%rbp), %rax	# str, tmp104
	movq	%rax, %rdi	# tmp104,
	call	_String_length	#
# src/Runtime/runtime.c:294:     if (startIndex >= _String_length(str)) {
	cmpl	%eax, -76(%rbp)	# _1, startIndex
	jl	.L78	#,
# src/Runtime/runtime.c:295:         errMsg = "ERROR: Substring starting index is too big.";
	leaq	.LC37(%rip), %rax	#, tmp105
	movq	%rax, errMsg(%rip)	# tmp105, errMsg
# src/Runtime/runtime.c:296:         error();
	movl	$0, %eax	#,
	call	error	#
.L78:
# src/Runtime/runtime.c:298:     uint8_t *rs = ((struct String *)str->data)->data;
	movq	-72(%rbp), %rax	# str, tmp106
	movq	8(%rax), %rax	# str_32(D)->data, _2
# src/Runtime/runtime.c:298:     uint8_t *rs = ((struct String *)str->data)->data;
	movq	8(%rax), %rax	# MEM[(struct String *)_2].data, tmp107
	movq	%rax, -32(%rbp)	# tmp107, rs
# src/Runtime/runtime.c:299:     uint8_t *offset_str = rs;
	movq	-32(%rbp), %rax	# rs, tmp108
	movq	%rax, -8(%rbp)	# tmp108, offset_str
# src/Runtime/runtime.c:301:     while (startIndex-- > 0)
	jmp	.L79	#
.L80:
# src/Runtime/runtime.c:302:         offset_str += u8_next(&character, offset_str) - offset_str;
	movq	-8(%rbp), %rdx	# offset_str, tmp109
	leaq	-60(%rbp), %rax	#, tmp110
	movq	%rdx, %rsi	# tmp109,
	movq	%rax, %rdi	# tmp110,
	call	u8_next@PLT	#
# src/Runtime/runtime.c:302:         offset_str += u8_next(&character, offset_str) - offset_str;
	subq	-8(%rbp), %rax	# offset_str, _60
# src/Runtime/runtime.c:302:         offset_str += u8_next(&character, offset_str) - offset_str;
	addq	%rax, -8(%rbp)	# _4, offset_str
.L79:
# src/Runtime/runtime.c:301:     while (startIndex-- > 0)
	movl	-76(%rbp), %eax	# startIndex, startIndex.123_5
	leal	-1(%rax), %edx	#, tmp111
	movl	%edx, -76(%rbp)	# tmp111, startIndex
# src/Runtime/runtime.c:301:     while (startIndex-- > 0)
	testl	%eax, %eax	# startIndex.123_5
	jg	.L80	#,
# src/Runtime/runtime.c:303:     uint8_t *end = offset_str;
	movq	-8(%rbp), %rax	# offset_str, tmp112
	movq	%rax, -16(%rbp)	# tmp112, end
# src/Runtime/runtime.c:304:     int32_t counter = 0;
	movl	$0, -20(%rbp)	#, counter
# src/Runtime/runtime.c:305:     while (counter < length) {
	jmp	.L81	#
.L83:
# src/Runtime/runtime.c:306:         if (u8_next(&character, end) == NULL) {
	movq	-16(%rbp), %rdx	# end, tmp113
	leaq	-60(%rbp), %rax	#, tmp114
	movq	%rdx, %rsi	# tmp113,
	movq	%rax, %rdi	# tmp114,
	call	u8_next@PLT	#
# src/Runtime/runtime.c:306:         if (u8_next(&character, end) == NULL) {
	testq	%rax, %rax	# _6
	jne	.L82	#,
# src/Runtime/runtime.c:307:             errMsg = "ERROR: Substring reached end of string.";
	leaq	.LC38(%rip), %rax	#, tmp115
	movq	%rax, errMsg(%rip)	# tmp115, errMsg
# src/Runtime/runtime.c:308:             error();
	movl	$0, %eax	#,
	call	error	#
.L82:
# src/Runtime/runtime.c:310:         end += u8_next(&character, end) - end;
	movq	-16(%rbp), %rdx	# end, tmp116
	leaq	-60(%rbp), %rax	#, tmp117
	movq	%rdx, %rsi	# tmp116,
	movq	%rax, %rdi	# tmp117,
	call	u8_next@PLT	#
# src/Runtime/runtime.c:310:         end += u8_next(&character, end) - end;
	subq	-16(%rbp), %rax	# end, _56
# src/Runtime/runtime.c:310:         end += u8_next(&character, end) - end;
	addq	%rax, -16(%rbp)	# _8, end
# src/Runtime/runtime.c:311:         counter++;
	addl	$1, -20(%rbp)	#, counter
.L81:
# src/Runtime/runtime.c:305:     while (counter < length) {
	movl	-20(%rbp), %eax	# counter, tmp118
	cmpl	-80(%rbp), %eax	# length, tmp118
	jl	.L83	#,
# src/Runtime/runtime.c:313:     int32_t bufferSize = end - offset_str + 1;
	movq	-16(%rbp), %rax	# end, tmp119
	subq	-8(%rbp), %rax	# offset_str, _9
# src/Runtime/runtime.c:313:     int32_t bufferSize = end - offset_str + 1;
	addl	$1, %eax	#, _11
# src/Runtime/runtime.c:313:     int32_t bufferSize = end - offset_str + 1;
	movl	%eax, -36(%rbp)	# _11, bufferSize
# src/Runtime/runtime.c:314:     uint8_t *buffer = malloc(bufferSize);
	movl	-36(%rbp), %eax	# bufferSize, tmp120
	cltq
	movq	%rax, %rdi	# _12,
	call	malloc@PLT	#
	movq	%rax, -48(%rbp)	# tmp121, buffer
# src/Runtime/runtime.c:315:     u8_strncpy(buffer, offset_str, bufferSize);
	movl	-36(%rbp), %eax	# bufferSize, tmp122
	movslq	%eax, %rdx	# tmp122, _13
	movq	-8(%rbp), %rcx	# offset_str, tmp123
	movq	-48(%rbp), %rax	# buffer, tmp124
	movq	%rcx, %rsi	# tmp123,
	movq	%rax, %rdi	# tmp124,
	call	u8_strncpy@PLT	#
# src/Runtime/runtime.c:316:     buffer[bufferSize - 1] = 0;
	movl	-36(%rbp), %eax	# bufferSize, tmp125
	cltq
	leaq	-1(%rax), %rdx	#, _15
	movq	-48(%rbp), %rax	# buffer, tmp126
	addq	%rdx, %rax	# _15, _16
# src/Runtime/runtime.c:316:     buffer[bufferSize - 1] = 0;
	movb	$0, (%rax)	#, *_16
# src/Runtime/runtime.c:317:     obj ret = __createString(buffer);
	movq	-48(%rbp), %rax	# buffer, tmp127
	movq	%rax, %rdi	# tmp127,
	call	__createString	#
	movq	%rax, -56(%rbp)	# tmp128, ret
# src/Runtime/runtime.c:318:     __incRef(ret);
	movq	-56(%rbp), %rax	# ret, tmp129
	movq	%rax, %rdi	# tmp129,
	call	__incRef	#
# src/Runtime/runtime.c:319:     free(buffer);
	movq	-48(%rbp), %rax	# buffer, tmp130
	movq	%rax, %rdi	# tmp130,
	call	free@PLT	#
# src/Runtime/runtime.c:320:     return ret;
	movq	-56(%rbp), %rax	# ret, _21
.L84:
# src/Runtime/runtime.c:321: }
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
# src/Runtime/runtime.c:323:     struct String *string = str->data;
	movq	-24(%rbp), %rax	# str, tmp90
	movq	8(%rax), %rax	# str_9(D)->data, tmp91
	movq	%rax, -8(%rbp)	# tmp91, string
# src/Runtime/runtime.c:324:     if (string->length < 0) {
	movq	-8(%rbp), %rax	# string, tmp92
	movl	(%rax), %eax	# string_10->length, _1
# src/Runtime/runtime.c:324:     if (string->length < 0) {
	testl	%eax, %eax	# _1
	jns	.L86	#,
# src/Runtime/runtime.c:325:         string->length = u8_mbsnlen(string->data, u8_strlen(string->data));
	movq	-8(%rbp), %rax	# string, tmp93
	movq	8(%rax), %rax	# string_10->data, _2
# src/Runtime/runtime.c:325:         string->length = u8_mbsnlen(string->data, u8_strlen(string->data));
	movq	%rax, %rdi	# _2,
	call	u8_strlen@PLT	#
	movq	%rax, %rdx	#, _3
# src/Runtime/runtime.c:325:         string->length = u8_mbsnlen(string->data, u8_strlen(string->data));
	movq	-8(%rbp), %rax	# string, tmp94
	movq	8(%rax), %rax	# string_10->data, _4
# src/Runtime/runtime.c:325:         string->length = u8_mbsnlen(string->data, u8_strlen(string->data));
	movq	%rdx, %rsi	# _3,
	movq	%rax, %rdi	# _4,
	call	u8_mbsnlen@PLT	#
# src/Runtime/runtime.c:325:         string->length = u8_mbsnlen(string->data, u8_strlen(string->data));
	movl	%eax, %edx	# _5, _6
	movq	-8(%rbp), %rax	# string, tmp95
	movl	%edx, (%rax)	# _6, string_10->length
.L86:
# src/Runtime/runtime.c:327:     return string->length;
	movq	-8(%rbp), %rax	# string, tmp96
	movl	(%rax), %eax	# string_10->length, _12
# src/Runtime/runtime.c:328: }
	leave	
	.cfi_def_cfa 7, 8
	ret	
	.cfi_endproc
.LFE26:
	.size	_String_length, .-_String_length
	.section	.rodata
	.align 8
.LC39:
	.string	"ERROR: IndexOf null substring argument."
	.align 8
.LC40:
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
	jne	.L89	#,
# src/Runtime/runtime.c:331:         errMsg = "ERROR: IndexOf null substring argument.";
	leaq	.LC39(%rip), %rax	#, tmp97
	movq	%rax, errMsg(%rip)	# tmp97, errMsg
# src/Runtime/runtime.c:332:         error();
	movl	$0, %eax	#,
	call	error	#
.L89:
# src/Runtime/runtime.c:334:     if (startFrom >= _String_length(str)) {
	movq	-72(%rbp), %rax	# str, tmp98
	movq	%rax, %rdi	# tmp98,
	call	_String_length	#
# src/Runtime/runtime.c:334:     if (startFrom >= _String_length(str)) {
	cmpl	%eax, -84(%rbp)	# _1, startFrom
	jl	.L90	#,
# src/Runtime/runtime.c:335:         errMsg = "ERROR: IndexOf starting index is too big.";
	leaq	.LC40(%rip), %rax	#, tmp99
	movq	%rax, errMsg(%rip)	# tmp99, errMsg
# src/Runtime/runtime.c:336:         error();
	movl	$0, %eax	#,
	call	error	#
.L90:
# src/Runtime/runtime.c:338:     if (_String_length(str) < _String_length(substr))
	movq	-72(%rbp), %rax	# str, tmp100
	movq	%rax, %rdi	# tmp100,
	call	_String_length	#
	movl	%eax, %ebx	#, _2
# src/Runtime/runtime.c:338:     if (_String_length(str) < _String_length(substr))
	movq	-80(%rbp), %rax	# substr, tmp101
	movq	%rax, %rdi	# tmp101,
	call	_String_length	#
# src/Runtime/runtime.c:338:     if (_String_length(str) < _String_length(substr))
	cmpl	%eax, %ebx	# _3, _2
	jge	.L91	#,
# src/Runtime/runtime.c:339:         return -1;
	movl	$-1, %eax	#, _16
	jmp	.L98	#
.L91:
# src/Runtime/runtime.c:340:     uint8_t *rs = ((struct String *)str->data)->data;
	movq	-72(%rbp), %rax	# str, tmp102
	movq	8(%rax), %rax	# str_26(D)->data, _4
# src/Runtime/runtime.c:340:     uint8_t *rs = ((struct String *)str->data)->data;
	movq	8(%rax), %rax	# MEM[(struct String *)_4].data, tmp103
	movq	%rax, -24(%rbp)	# tmp103, rs
# src/Runtime/runtime.c:341:     uint8_t *rsub = ((struct String *)substr->data)->data;
	movq	-80(%rbp), %rax	# substr, tmp104
	movq	8(%rax), %rax	# substr_22(D)->data, _5
# src/Runtime/runtime.c:341:     uint8_t *rsub = ((struct String *)substr->data)->data;
	movq	8(%rax), %rax	# MEM[(struct String *)_5].data, tmp105
	movq	%rax, -48(%rbp)	# tmp105, rsub
# src/Runtime/runtime.c:342:     uint8_t *start = rs;
	movq	-24(%rbp), %rax	# rs, tmp106
	movq	%rax, -32(%rbp)	# tmp106, start
# src/Runtime/runtime.c:344:     while (startFrom-- > 0) {
	jmp	.L93	#
.L95:
# src/Runtime/runtime.c:345:         if (u8_next(&c, start) == NULL)
	movq	-32(%rbp), %rdx	# start, tmp107
	leaq	-60(%rbp), %rax	#, tmp108
	movq	%rdx, %rsi	# tmp107,
	movq	%rax, %rdi	# tmp108,
	call	u8_next@PLT	#
# src/Runtime/runtime.c:345:         if (u8_next(&c, start) == NULL)
	testq	%rax, %rax	# _6
	jne	.L94	#,
# src/Runtime/runtime.c:346:             return -1;
	movl	$-1, %eax	#, _16
	jmp	.L98	#
.L94:
# src/Runtime/runtime.c:347:         start += u8_next(&c, start) - start;
	movq	-32(%rbp), %rdx	# start, tmp109
	leaq	-60(%rbp), %rax	#, tmp110
	movq	%rdx, %rsi	# tmp109,
	movq	%rax, %rdi	# tmp110,
	call	u8_next@PLT	#
# src/Runtime/runtime.c:347:         start += u8_next(&c, start) - start;
	subq	-32(%rbp), %rax	# start, _46
# src/Runtime/runtime.c:347:         start += u8_next(&c, start) - start;
	addq	%rax, -32(%rbp)	# _8, start
.L93:
# src/Runtime/runtime.c:344:     while (startFrom-- > 0) {
	movl	-84(%rbp), %eax	# startFrom, startFrom.124_9
	leal	-1(%rax), %edx	#, tmp111
	movl	%edx, -84(%rbp)	# tmp111, startFrom
# src/Runtime/runtime.c:344:     while (startFrom-- > 0) {
	testl	%eax, %eax	# startFrom.124_9
	jg	.L95	#,
# src/Runtime/runtime.c:349:     uint8_t *index = u8_strstr(start, rsub);
	movq	-48(%rbp), %rdx	# rsub, tmp112
	movq	-32(%rbp), %rax	# start, tmp113
	movq	%rdx, %rsi	# tmp112,
	movq	%rax, %rdi	# tmp113,
	call	u8_strstr@PLT	#
	movq	%rax, -56(%rbp)	# tmp114, index
# src/Runtime/runtime.c:350:     uint32_t counter = 0;
	movl	$0, -36(%rbp)	#, counter
# src/Runtime/runtime.c:351:     while ((rs += u8_next(&c, rs) - rs) != index)
	jmp	.L96	#
.L97:
# src/Runtime/runtime.c:352:         counter++;
	addl	$1, -36(%rbp)	#, counter
.L96:
# src/Runtime/runtime.c:351:     while ((rs += u8_next(&c, rs) - rs) != index)
	movq	-24(%rbp), %rdx	# rs, tmp115
	leaq	-60(%rbp), %rax	#, tmp116
	movq	%rdx, %rsi	# tmp115,
	movq	%rax, %rdi	# tmp116,
	call	u8_next@PLT	#
# src/Runtime/runtime.c:351:     while ((rs += u8_next(&c, rs) - rs) != index)
	subq	-24(%rbp), %rax	# rs, _40
# src/Runtime/runtime.c:351:     while ((rs += u8_next(&c, rs) - rs) != index)
	addq	%rax, -24(%rbp)	# _11, rs
# src/Runtime/runtime.c:351:     while ((rs += u8_next(&c, rs) - rs) != index)
	movq	-24(%rbp), %rax	# rs, tmp117
	cmpq	-56(%rbp), %rax	# index, tmp117
	jne	.L97	#,
# src/Runtime/runtime.c:353:     return counter;
	movl	-36(%rbp), %eax	# counter, _16
.L98:
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
# src/Runtime/runtime.c:356:     uint8_t *rs = ((struct String *)str->data)->data;
	movq	-40(%rbp), %rax	# str, tmp91
	movq	8(%rax), %rax	# str_9(D)->data, _1
# src/Runtime/runtime.c:356:     uint8_t *rs = ((struct String *)str->data)->data;
	movq	8(%rax), %rax	# MEM[(struct String *)_1].data, tmp92
	movq	%rax, -8(%rbp)	# tmp92, rs
# src/Runtime/runtime.c:357:     int32_t len = rs == NULL ? 0 : u8_strlen(rs);
	cmpq	$0, -8(%rbp)	#, rs
	je	.L100	#,
# src/Runtime/runtime.c:357:     int32_t len = rs == NULL ? 0 : u8_strlen(rs);
	movq	-8(%rbp), %rax	# rs, tmp93
	movq	%rax, %rdi	# tmp93,
	call	u8_strlen@PLT	#
	jmp	.L101	#
.L100:
# src/Runtime/runtime.c:357:     int32_t len = rs == NULL ? 0 : u8_strlen(rs);
	movl	$0, %eax	#, iftmp.125_7
.L101:
# src/Runtime/runtime.c:357:     int32_t len = rs == NULL ? 0 : u8_strlen(rs);
	movl	%eax, -12(%rbp)	# iftmp.125_7, len
# src/Runtime/runtime.c:358:     obj arr = __newByteArray(len + 1);
	movl	-12(%rbp), %eax	# len, tmp94
	addl	$1, %eax	#, _3
	movl	%eax, %edi	# _3,
	call	__newByteArray	#
	movq	%rax, -24(%rbp)	# tmp95, arr
# src/Runtime/runtime.c:359:     memcpy(((struct Array *)arr->data)->elements, rs, len);
	movl	-12(%rbp), %eax	# len, tmp96
	movslq	%eax, %rdx	# tmp96, _4
# src/Runtime/runtime.c:359:     memcpy(((struct Array *)arr->data)->elements, rs, len);
	movq	-24(%rbp), %rax	# arr, tmp97
	movq	8(%rax), %rax	# arr_15->data, _5
# src/Runtime/runtime.c:359:     memcpy(((struct Array *)arr->data)->elements, rs, len);
	movq	8(%rax), %rax	# MEM[(struct Array *)_5].elements, _6
	movq	-8(%rbp), %rcx	# rs, tmp98
	movq	%rcx, %rsi	# tmp98,
	movq	%rax, %rdi	# _6,
	call	memcpy@PLT	#
# src/Runtime/runtime.c:360:     __incRef(arr);
	movq	-24(%rbp), %rax	# arr, tmp99
	movq	%rax, %rdi	# tmp99,
	call	__incRef	#
# src/Runtime/runtime.c:361:     return arr;
	movq	-24(%rbp), %rax	# arr, _18
# src/Runtime/runtime.c:362: }
	leave	
	.cfi_def_cfa 7, 8
	ret	
	.cfi_endproc
.LFE28:
	.size	_String_getBytes, .-_String_getBytes
	.section	.rodata
	.align 8
.LC41:
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
	jne	.L104	#,
# src/Runtime/runtime.c:365:         errMsg = "ERROR: EndsWith null substring argument.";
	leaq	.LC41(%rip), %rax	#, tmp87
	movq	%rax, errMsg(%rip)	# tmp87, errMsg
# src/Runtime/runtime.c:366:         error();
	movl	$0, %eax	#,
	call	error	#
.L104:
# src/Runtime/runtime.c:368:     uint8_t *rs = ((struct String *)str->data)->data;
	movq	-24(%rbp), %rax	# str, tmp88
	movq	8(%rax), %rax	# str_9(D)->data, _1
# src/Runtime/runtime.c:368:     uint8_t *rs = ((struct String *)str->data)->data;
	movq	8(%rax), %rax	# MEM[(struct String *)_1].data, tmp89
	movq	%rax, -8(%rbp)	# tmp89, rs
# src/Runtime/runtime.c:369:     uint8_t *rsub = ((struct String *)substr->data)->data;
	movq	-32(%rbp), %rax	# substr, tmp90
	movq	8(%rax), %rax	# substr_5(D)->data, _2
# src/Runtime/runtime.c:369:     uint8_t *rsub = ((struct String *)substr->data)->data;
	movq	8(%rax), %rax	# MEM[(struct String *)_2].data, tmp91
	movq	%rax, -16(%rbp)	# tmp91, rsub
# src/Runtime/runtime.c:370:     return u8_endswith(rs, rsub);
	movq	-16(%rbp), %rdx	# rsub, tmp92
	movq	-8(%rbp), %rax	# rs, tmp93
	movq	%rdx, %rsi	# tmp92,
	movq	%rax, %rdi	# tmp93,
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
.LC42:
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
	jne	.L107	#,
# src/Runtime/runtime.c:374:         errMsg = "ERROR: StartsWith null substring argument.";
	leaq	.LC42(%rip), %rax	#, tmp87
	movq	%rax, errMsg(%rip)	# tmp87, errMsg
# src/Runtime/runtime.c:375:         error();
	movl	$0, %eax	#,
	call	error	#
.L107:
# src/Runtime/runtime.c:377:     uint8_t *rs = ((struct String *)str->data)->data;
	movq	-24(%rbp), %rax	# str, tmp88
	movq	8(%rax), %rax	# str_9(D)->data, _1
# src/Runtime/runtime.c:377:     uint8_t *rs = ((struct String *)str->data)->data;
	movq	8(%rax), %rax	# MEM[(struct String *)_1].data, tmp89
	movq	%rax, -8(%rbp)	# tmp89, rs
# src/Runtime/runtime.c:378:     uint8_t *rsub = ((struct String *)substr->data)->data;
	movq	-32(%rbp), %rax	# substr, tmp90
	movq	8(%rax), %rax	# substr_5(D)->data, _2
# src/Runtime/runtime.c:378:     uint8_t *rsub = ((struct String *)substr->data)->data;
	movq	8(%rax), %rax	# MEM[(struct String *)_2].data, tmp91
	movq	%rax, -16(%rbp)	# tmp91, rsub
# src/Runtime/runtime.c:379:     return u8_startswith(rs, rsub);
	movq	-16(%rbp), %rdx	# rsub, tmp92
	movq	-8(%rbp), %rax	# rs, tmp93
	movq	%rdx, %rsi	# tmp92,
	movq	%rax, %rdi	# tmp93,
	call	u8_startswith@PLT	#
# src/Runtime/runtime.c:380: }
	leave	
	.cfi_def_cfa 7, 8
	ret	
	.cfi_endproc
.LFE30:
	.size	_String_startsWith, .-_String_startsWith
	.section	.rodata
.LC43:
	.string	"String concat on %p and %p"
.LC44:
	.string	"Take strlen"
.LC45:
	.string	"perform strcpy"
.LC46:
	.string	"Create final string"
.LC47:
	.string	"String concat completed"
	.text
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
# src/Runtime/runtime.c:382:     DEBUG("String concat on %p and %p", str, secondstr);
	movq	stderr(%rip), %rax	# stderr, stderr.126_1
	movq	%rax, %rcx	# stderr.126_1,
	movl	$14, %edx	#,
	movl	$1, %esi	#,
	leaq	.LC0(%rip), %rax	#, tmp116
	movq	%rax, %rdi	# tmp116,
	call	fwrite@PLT	#
	movq	stderr(%rip), %rax	# stderr, stderr.127_2
	movq	-64(%rbp), %rcx	# secondstr, tmp117
	movq	-56(%rbp), %rdx	# str, tmp118
	leaq	.LC43(%rip), %rsi	#, tmp119
	movq	%rax, %rdi	# stderr.127_2,
	movl	$0, %eax	#,
	call	fprintf@PLT	#
	movq	stderr(%rip), %rax	# stderr, stderr.128_3
	movq	%rax, %rsi	# stderr.128_3,
	movl	$10, %edi	#,
	call	fputc@PLT	#
	movq	stderr(%rip), %rax	# stderr, stderr.129_4
	movq	%rax, %rdi	# stderr.129_4,
	call	fflush@PLT	#
# src/Runtime/runtime.c:383:     if (secondstr == NULL) {
	cmpq	$0, -64(%rbp)	#, secondstr
	jne	.L110	#,
# src/Runtime/runtime.c:384:         __incRef(str);
	movq	-56(%rbp), %rax	# str, tmp120
	movq	%rax, %rdi	# tmp120,
	call	__incRef	#
# src/Runtime/runtime.c:385:         return str;
	movq	-56(%rbp), %rax	# str, _33
	jmp	.L111	#
.L110:
# src/Runtime/runtime.c:387:     uint8_t *rs1 = ((struct String *)str->data)->data;
	movq	-56(%rbp), %rax	# str, tmp121
	movq	8(%rax), %rax	# str_37(D)->data, _5
# src/Runtime/runtime.c:387:     uint8_t *rs1 = ((struct String *)str->data)->data;
	movq	8(%rax), %rax	# MEM[(struct String *)_5].data, tmp122
	movq	%rax, -8(%rbp)	# tmp122, rs1
# src/Runtime/runtime.c:388:     uint8_t *rs2 = ((struct String *)secondstr->data)->data;
	movq	-64(%rbp), %rax	# secondstr, tmp123
	movq	8(%rax), %rax	# secondstr_38(D)->data, _6
# src/Runtime/runtime.c:388:     uint8_t *rs2 = ((struct String *)secondstr->data)->data;
	movq	8(%rax), %rax	# MEM[(struct String *)_6].data, tmp124
	movq	%rax, -16(%rbp)	# tmp124, rs2
# src/Runtime/runtime.c:389:     DEBUG("Take strlen");
	movq	stderr(%rip), %rax	# stderr, stderr.130_7
	movq	%rax, %rcx	# stderr.130_7,
	movl	$14, %edx	#,
	movl	$1, %esi	#,
	leaq	.LC0(%rip), %rax	#, tmp125
	movq	%rax, %rdi	# tmp125,
	call	fwrite@PLT	#
	movq	stderr(%rip), %rax	# stderr, stderr.131_8
	movq	%rax, %rcx	# stderr.131_8,
	movl	$11, %edx	#,
	movl	$1, %esi	#,
	leaq	.LC44(%rip), %rax	#, tmp126
	movq	%rax, %rdi	# tmp126,
	call	fwrite@PLT	#
	movq	stderr(%rip), %rax	# stderr, stderr.132_9
	movq	%rax, %rsi	# stderr.132_9,
	movl	$10, %edi	#,
	call	fputc@PLT	#
	movq	stderr(%rip), %rax	# stderr, stderr.133_10
	movq	%rax, %rdi	# stderr.133_10,
	call	fflush@PLT	#
# src/Runtime/runtime.c:390:     int32_t len1 = u8_strlen(rs1);
	movq	-8(%rbp), %rax	# rs1, tmp127
	movq	%rax, %rdi	# tmp127,
	call	u8_strlen@PLT	#
# src/Runtime/runtime.c:390:     int32_t len1 = u8_strlen(rs1);
	movl	%eax, -20(%rbp)	# _11, len1
# src/Runtime/runtime.c:391:     int32_t len2 = u8_strlen(rs2);
	movq	-16(%rbp), %rax	# rs2, tmp128
	movq	%rax, %rdi	# tmp128,
	call	u8_strlen@PLT	#
# src/Runtime/runtime.c:391:     int32_t len2 = u8_strlen(rs2);
	movl	%eax, -24(%rbp)	# _12, len2
# src/Runtime/runtime.c:392:     uint8_t *buffer = malloc(len1 + len2 + 1);
	movl	-20(%rbp), %edx	# len1, tmp129
	movl	-24(%rbp), %eax	# len2, tmp130
	addl	%edx, %eax	# tmp129, _13
# src/Runtime/runtime.c:392:     uint8_t *buffer = malloc(len1 + len2 + 1);
	addl	$1, %eax	#, _14
# src/Runtime/runtime.c:392:     uint8_t *buffer = malloc(len1 + len2 + 1);
	cltq
	movq	%rax, %rdi	# _15,
	call	malloc@PLT	#
	movq	%rax, -32(%rbp)	# tmp131, buffer
# src/Runtime/runtime.c:393:     DEBUG("perform strcpy");
	movq	stderr(%rip), %rax	# stderr, stderr.134_16
	movq	%rax, %rcx	# stderr.134_16,
	movl	$14, %edx	#,
	movl	$1, %esi	#,
	leaq	.LC0(%rip), %rax	#, tmp132
	movq	%rax, %rdi	# tmp132,
	call	fwrite@PLT	#
	movq	stderr(%rip), %rax	# stderr, stderr.135_17
	movq	%rax, %rcx	# stderr.135_17,
	movl	$14, %edx	#,
	movl	$1, %esi	#,
	leaq	.LC45(%rip), %rax	#, tmp133
	movq	%rax, %rdi	# tmp133,
	call	fwrite@PLT	#
	movq	stderr(%rip), %rax	# stderr, stderr.136_18
	movq	%rax, %rsi	# stderr.136_18,
	movl	$10, %edi	#,
	call	fputc@PLT	#
	movq	stderr(%rip), %rax	# stderr, stderr.137_19
	movq	%rax, %rdi	# stderr.137_19,
	call	fflush@PLT	#
# src/Runtime/runtime.c:394:     u8_strcpy(buffer, rs1);
	movq	-8(%rbp), %rdx	# rs1, tmp134
	movq	-32(%rbp), %rax	# buffer, tmp135
	movq	%rdx, %rsi	# tmp134,
	movq	%rax, %rdi	# tmp135,
	call	u8_strcpy@PLT	#
# src/Runtime/runtime.c:395:     u8_strcpy(buffer + len1, rs2);
	movl	-20(%rbp), %eax	# len1, tmp136
	movslq	%eax, %rdx	# tmp136, _20
	movq	-32(%rbp), %rax	# buffer, tmp137
	addq	%rax, %rdx	# tmp137, _21
	movq	-16(%rbp), %rax	# rs2, tmp138
	movq	%rax, %rsi	# tmp138,
	movq	%rdx, %rdi	# _21,
	call	u8_strcpy@PLT	#
# src/Runtime/runtime.c:396:     buffer[len1 + len2] = 0;
	movl	-20(%rbp), %edx	# len1, tmp139
	movl	-24(%rbp), %eax	# len2, tmp140
	addl	%edx, %eax	# tmp139, _22
	movslq	%eax, %rdx	# _22, _23
# src/Runtime/runtime.c:396:     buffer[len1 + len2] = 0;
	movq	-32(%rbp), %rax	# buffer, tmp141
	addq	%rdx, %rax	# _23, _24
# src/Runtime/runtime.c:396:     buffer[len1 + len2] = 0;
	movb	$0, (%rax)	#, *_24
# src/Runtime/runtime.c:397:     DEBUG("Create final string");
	movq	stderr(%rip), %rax	# stderr, stderr.138_25
	movq	%rax, %rcx	# stderr.138_25,
	movl	$14, %edx	#,
	movl	$1, %esi	#,
	leaq	.LC0(%rip), %rax	#, tmp142
	movq	%rax, %rdi	# tmp142,
	call	fwrite@PLT	#
	movq	stderr(%rip), %rax	# stderr, stderr.139_26
	movq	%rax, %rcx	# stderr.139_26,
	movl	$19, %edx	#,
	movl	$1, %esi	#,
	leaq	.LC46(%rip), %rax	#, tmp143
	movq	%rax, %rdi	# tmp143,
	call	fwrite@PLT	#
	movq	stderr(%rip), %rax	# stderr, stderr.140_27
	movq	%rax, %rsi	# stderr.140_27,
	movl	$10, %edi	#,
	call	fputc@PLT	#
	movq	stderr(%rip), %rax	# stderr, stderr.141_28
	movq	%rax, %rdi	# stderr.141_28,
	call	fflush@PLT	#
# src/Runtime/runtime.c:398:     obj ret = __createString(buffer);
	movq	-32(%rbp), %rax	# buffer, tmp144
	movq	%rax, %rdi	# tmp144,
	call	__createString	#
	movq	%rax, -40(%rbp)	# tmp145, ret
# src/Runtime/runtime.c:399:     __incRef(ret);
	movq	-40(%rbp), %rax	# ret, tmp146
	movq	%rax, %rdi	# tmp146,
	call	__incRef	#
# src/Runtime/runtime.c:400:     free(buffer);
	movq	-32(%rbp), %rax	# buffer, tmp147
	movq	%rax, %rdi	# tmp147,
	call	free@PLT	#
# src/Runtime/runtime.c:401:     DEBUG("String concat completed");
	movq	stderr(%rip), %rax	# stderr, stderr.142_29
	movq	%rax, %rcx	# stderr.142_29,
	movl	$14, %edx	#,
	movl	$1, %esi	#,
	leaq	.LC0(%rip), %rax	#, tmp148
	movq	%rax, %rdi	# tmp148,
	call	fwrite@PLT	#
	movq	stderr(%rip), %rax	# stderr, stderr.143_30
	movq	%rax, %rcx	# stderr.143_30,
	movl	$23, %edx	#,
	movl	$1, %esi	#,
	leaq	.LC47(%rip), %rax	#, tmp149
	movq	%rax, %rdi	# tmp149,
	call	fwrite@PLT	#
	movq	stderr(%rip), %rax	# stderr, stderr.144_31
	movq	%rax, %rsi	# stderr.144_31,
	movl	$10, %edi	#,
	call	fputc@PLT	#
	movq	stderr(%rip), %rax	# stderr, stderr.145_32
	movq	%rax, %rdi	# stderr.145_32,
	call	fflush@PLT	#
# src/Runtime/runtime.c:402:     return ret;
	movq	-40(%rbp), %rax	# ret, _33
.L111:
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
# src/Runtime/runtime.c:407:     uint8_t *rs = ((struct String *)str->data)->data;
	movq	-24(%rbp), %rax	# str, tmp92
	movq	8(%rax), %rax	# str_14(D)->data, _1
# src/Runtime/runtime.c:407:     uint8_t *rs = ((struct String *)str->data)->data;
	movq	8(%rax), %rax	# MEM[(struct String *)_1].data, tmp93
	movq	%rax, -8(%rbp)	# tmp93, rs
# src/Runtime/runtime.c:409:     while (index-- > 0) {
	jmp	.L113	#
.L115:
# src/Runtime/runtime.c:410:         if (u8_next(&c, rs) == NULL) {
	movq	-8(%rbp), %rdx	# rs, tmp94
	leaq	-12(%rbp), %rax	#, tmp95
	movq	%rdx, %rsi	# tmp94,
	movq	%rax, %rdi	# tmp95,
	call	u8_next@PLT	#
# src/Runtime/runtime.c:410:         if (u8_next(&c, rs) == NULL) {
	testq	%rax, %rax	# _2
	jne	.L114	#,
# src/Runtime/runtime.c:411:             errMsg = charAtErr;
	leaq	charAtErr(%rip), %rax	#, tmp96
	movq	%rax, errMsg(%rip)	# tmp96, errMsg
# src/Runtime/runtime.c:412:             error();
	movl	$0, %eax	#,
	call	error	#
.L114:
# src/Runtime/runtime.c:414:         rs += u8_next(&c, rs) - rs;
	movq	-8(%rbp), %rdx	# rs, tmp97
	leaq	-12(%rbp), %rax	#, tmp98
	movq	%rdx, %rsi	# tmp97,
	movq	%rax, %rdi	# tmp98,
	call	u8_next@PLT	#
# src/Runtime/runtime.c:414:         rs += u8_next(&c, rs) - rs;
	subq	-8(%rbp), %rax	# rs, _27
# src/Runtime/runtime.c:414:         rs += u8_next(&c, rs) - rs;
	addq	%rax, -8(%rbp)	# _4, rs
.L113:
# src/Runtime/runtime.c:409:     while (index-- > 0) {
	movl	-28(%rbp), %eax	# index, index.146_5
	leal	-1(%rax), %edx	#, tmp99
	movl	%edx, -28(%rbp)	# tmp99, index
# src/Runtime/runtime.c:409:     while (index-- > 0) {
	testl	%eax, %eax	# index.146_5
	jg	.L115	#,
# src/Runtime/runtime.c:416:     if (u8_strmbtouc(&c, rs) <= 0) {
	movq	-8(%rbp), %rdx	# rs, tmp100
	leaq	-12(%rbp), %rax	#, tmp101
	movq	%rdx, %rsi	# tmp100,
	movq	%rax, %rdi	# tmp101,
	call	u8_strmbtouc@PLT	#
# src/Runtime/runtime.c:416:     if (u8_strmbtouc(&c, rs) <= 0) {
	testl	%eax, %eax	# _6
	jg	.L116	#,
# src/Runtime/runtime.c:417:         errMsg = charAtErr;
	leaq	charAtErr(%rip), %rax	#, tmp102
	movq	%rax, errMsg(%rip)	# tmp102, errMsg
# src/Runtime/runtime.c:418:         error();
	movl	$0, %eax	#,
	call	error	#
.L116:
# src/Runtime/runtime.c:420:     return c;
	movl	-12(%rbp), %eax	# c, c.147_7
# src/Runtime/runtime.c:421: }
	leave	
	.cfi_def_cfa 7, 8
	ret	
	.cfi_endproc
.LFE32:
	.size	_String_charAt, .-_String_charAt
	.section	.rodata
.LC48:
	.string	"%p"
	.text
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
	subq	$16, %rsp	#,
	movq	%rdi, -8(%rbp)	# str, str
# src/Runtime/runtime.c:424:     DEBUG("%p", str);
	movq	stderr(%rip), %rax	# stderr, stderr.148_1
	movq	%rax, %rcx	# stderr.148_1,
	movl	$14, %edx	#,
	movl	$1, %esi	#,
	leaq	.LC0(%rip), %rax	#, tmp86
	movq	%rax, %rdi	# tmp86,
	call	fwrite@PLT	#
	movq	stderr(%rip), %rax	# stderr, stderr.149_2
	movq	-8(%rbp), %rdx	# str, tmp87
	leaq	.LC48(%rip), %rcx	#, tmp88
	movq	%rcx, %rsi	# tmp88,
	movq	%rax, %rdi	# stderr.149_2,
	movl	$0, %eax	#,
	call	fprintf@PLT	#
	movq	stderr(%rip), %rax	# stderr, stderr.150_3
	movq	%rax, %rsi	# stderr.150_3,
	movl	$10, %edi	#,
	call	fputc@PLT	#
	movq	stderr(%rip), %rax	# stderr, stderr.151_4
	movq	%rax, %rdi	# stderr.151_4,
	call	fflush@PLT	#
# src/Runtime/runtime.c:425: }
	nop	
	leave	
	.cfi_def_cfa 7, 8
	ret	
	.cfi_endproc
.LFE33:
	.size	ddd, .-ddd
	.section	.rodata
.LC49:
	.string	"Calling printString(%p)"
.LC50:
	.string	"Str data is %p"
.LC51:
	.string	"Str inner data %p"
.LC52:
	.string	"printString(%p) completed"
	.text
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
# src/Runtime/runtime.c:429:     DEBUG("Calling printString(%p)", FORMAT_PTR(str));
	movq	stderr(%rip), %rax	# stderr, stderr.152_1
	movq	%rax, %rcx	# stderr.152_1,
	movl	$14, %edx	#,
	movl	$1, %esi	#,
	leaq	.LC0(%rip), %rax	#, tmp102
	movq	%rax, %rdi	# tmp102,
	call	fwrite@PLT	#
	movq	stderr(%rip), %rax	# stderr, stderr.153_2
	movq	-24(%rbp), %rdx	# str, tmp103
	leaq	.LC49(%rip), %rcx	#, tmp104
	movq	%rcx, %rsi	# tmp104,
	movq	%rax, %rdi	# stderr.153_2,
	movl	$0, %eax	#,
	call	fprintf@PLT	#
	movq	stderr(%rip), %rax	# stderr, stderr.154_3
	movq	%rax, %rsi	# stderr.154_3,
	movl	$10, %edi	#,
	call	fputc@PLT	#
	movq	stderr(%rip), %rax	# stderr, stderr.155_4
	movq	%rax, %rdi	# stderr.155_4,
	call	fflush@PLT	#
# src/Runtime/runtime.c:430:     DEBUG("Str data is %p", FORMAT_PTR(str->data));
	movq	stderr(%rip), %rax	# stderr, stderr.156_5
	movq	%rax, %rcx	# stderr.156_5,
	movl	$14, %edx	#,
	movl	$1, %esi	#,
	leaq	.LC0(%rip), %rax	#, tmp105
	movq	%rax, %rdi	# tmp105,
	call	fwrite@PLT	#
	movq	-24(%rbp), %rax	# str, tmp106
	movq	8(%rax), %rdx	# str_23(D)->data, _6
	movq	stderr(%rip), %rax	# stderr, stderr.157_7
	leaq	.LC50(%rip), %rcx	#, tmp107
	movq	%rcx, %rsi	# tmp107,
	movq	%rax, %rdi	# stderr.157_7,
	movl	$0, %eax	#,
	call	fprintf@PLT	#
	movq	stderr(%rip), %rax	# stderr, stderr.158_8
	movq	%rax, %rsi	# stderr.158_8,
	movl	$10, %edi	#,
	call	fputc@PLT	#
	movq	stderr(%rip), %rax	# stderr, stderr.159_9
	movq	%rax, %rdi	# stderr.159_9,
	call	fflush@PLT	#
# src/Runtime/runtime.c:431:     if (str == NULL)
	cmpq	$0, -24(%rbp)	#, str
	jne	.L120	#,
# src/Runtime/runtime.c:432:         str = __createString("null");
	leaq	.LC34(%rip), %rax	#, tmp108
	movq	%rax, %rdi	# tmp108,
	call	__createString	#
	movq	%rax, -24(%rbp)	# tmp109, str
.L120:
# src/Runtime/runtime.c:433:     __incRef(str);
	movq	-24(%rbp), %rax	# str, tmp110
	movq	%rax, %rdi	# tmp110,
	call	__incRef	#
# src/Runtime/runtime.c:434:     uint8_t *rs = ((struct String *)str->data)->data;
	movq	-24(%rbp), %rax	# str, tmp111
	movq	8(%rax), %rax	# str_19->data, _10
# src/Runtime/runtime.c:434:     uint8_t *rs = ((struct String *)str->data)->data;
	movq	8(%rax), %rax	# MEM[(struct String *)_10].data, tmp112
	movq	%rax, -8(%rbp)	# tmp112, rs
# src/Runtime/runtime.c:435:     DEBUG("Str inner data %p", FORMAT_PTR(rs));
	movq	stderr(%rip), %rax	# stderr, stderr.160_11
	movq	%rax, %rcx	# stderr.160_11,
	movl	$14, %edx	#,
	movl	$1, %esi	#,
	leaq	.LC0(%rip), %rax	#, tmp113
	movq	%rax, %rdi	# tmp113,
	call	fwrite@PLT	#
	movq	stderr(%rip), %rax	# stderr, stderr.161_12
	movq	-8(%rbp), %rdx	# rs, tmp114
	leaq	.LC51(%rip), %rcx	#, tmp115
	movq	%rcx, %rsi	# tmp115,
	movq	%rax, %rdi	# stderr.161_12,
	movl	$0, %eax	#,
	call	fprintf@PLT	#
	movq	stderr(%rip), %rax	# stderr, stderr.162_13
	movq	%rax, %rsi	# stderr.162_13,
	movl	$10, %edi	#,
	call	fputc@PLT	#
	movq	stderr(%rip), %rax	# stderr, stderr.163_14
	movq	%rax, %rdi	# stderr.163_14,
	call	fflush@PLT	#
# src/Runtime/runtime.c:436:     printf("%s\n", rs);
	movq	-8(%rbp), %rax	# rs, tmp116
	movq	%rax, %rdi	# tmp116,
	call	puts@PLT	#
# src/Runtime/runtime.c:437:     __decRef(str);
	movq	-24(%rbp), %rax	# str, tmp117
	movq	%rax, %rdi	# tmp117,
	call	__decRef	#
# src/Runtime/runtime.c:438:     DEBUG("printString(%p) completed", FORMAT_PTR(str))
	movq	stderr(%rip), %rax	# stderr, stderr.164_15
	movq	%rax, %rcx	# stderr.164_15,
	movl	$14, %edx	#,
	movl	$1, %esi	#,
	leaq	.LC0(%rip), %rax	#, tmp118
	movq	%rax, %rdi	# tmp118,
	call	fwrite@PLT	#
	movq	stderr(%rip), %rax	# stderr, stderr.165_16
	movq	-24(%rbp), %rdx	# str, tmp119
	leaq	.LC52(%rip), %rcx	#, tmp120
	movq	%rcx, %rsi	# tmp120,
	movq	%rax, %rdi	# stderr.165_16,
	movl	$0, %eax	#,
	call	fprintf@PLT	#
	movq	stderr(%rip), %rax	# stderr, stderr.166_17
	movq	%rax, %rsi	# stderr.166_17,
	movl	$10, %edi	#,
	call	fputc@PLT	#
	movq	stderr(%rip), %rax	# stderr, stderr.167_18
	movq	%rax, %rdi	# stderr.167_18,
	call	fflush@PLT	#
# src/Runtime/runtime.c:439:     return 0;
	movl	$0, %eax	#, _45
# src/Runtime/runtime.c:440: }
	leave	
	.cfi_def_cfa 7, 8
	ret	
	.cfi_endproc
.LFE34:
	.size	printString, .-printString
	.section	.rodata
.LC53:
	.string	"Calling printInt()"
.LC54:
	.string	"%d\n"
.LC55:
	.string	"printInt() completed"
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
# src/Runtime/runtime.c:442:     DEBUG("Calling printInt()")
	movq	stderr(%rip), %rax	# stderr, stderr.168_1
	movq	%rax, %rcx	# stderr.168_1,
	movl	$14, %edx	#,
	movl	$1, %esi	#,
	leaq	.LC0(%rip), %rax	#, tmp92
	movq	%rax, %rdi	# tmp92,
	call	fwrite@PLT	#
	movq	stderr(%rip), %rax	# stderr, stderr.169_2
	movq	%rax, %rcx	# stderr.169_2,
	movl	$18, %edx	#,
	movl	$1, %esi	#,
	leaq	.LC53(%rip), %rax	#, tmp93
	movq	%rax, %rdi	# tmp93,
	call	fwrite@PLT	#
	movq	stderr(%rip), %rax	# stderr, stderr.170_3
	movq	%rax, %rsi	# stderr.170_3,
	movl	$10, %edi	#,
	call	fputc@PLT	#
	movq	stderr(%rip), %rax	# stderr, stderr.171_4
	movq	%rax, %rdi	# stderr.171_4,
	call	fflush@PLT	#
# src/Runtime/runtime.c:443:     printf("%d\n", i);
	movl	-4(%rbp), %eax	# i, tmp94
	movl	%eax, %esi	# tmp94,
	leaq	.LC54(%rip), %rax	#, tmp95
	movq	%rax, %rdi	# tmp95,
	movl	$0, %eax	#,
	call	printf@PLT	#
# src/Runtime/runtime.c:444:     DEBUG("printInt() completed")
	movq	stderr(%rip), %rax	# stderr, stderr.172_5
	movq	%rax, %rcx	# stderr.172_5,
	movl	$14, %edx	#,
	movl	$1, %esi	#,
	leaq	.LC0(%rip), %rax	#, tmp96
	movq	%rax, %rdi	# tmp96,
	call	fwrite@PLT	#
	movq	stderr(%rip), %rax	# stderr, stderr.173_6
	movq	%rax, %rcx	# stderr.173_6,
	movl	$20, %edx	#,
	movl	$1, %esi	#,
	leaq	.LC55(%rip), %rax	#, tmp97
	movq	%rax, %rdi	# tmp97,
	call	fwrite@PLT	#
	movq	stderr(%rip), %rax	# stderr, stderr.174_7
	movq	%rax, %rsi	# stderr.174_7,
	movl	$10, %edi	#,
	call	fputc@PLT	#
	movq	stderr(%rip), %rax	# stderr, stderr.175_8
	movq	%rax, %rdi	# stderr.175_8,
	call	fflush@PLT	#
# src/Runtime/runtime.c:445:     return 0;
	movl	$0, %eax	#, _20
# src/Runtime/runtime.c:446: }
	leave	
	.cfi_def_cfa 7, 8
	ret	
	.cfi_endproc
.LFE35:
	.size	printInt, .-printInt
	.section	.rodata
.LC56:
	.string	"Calling printBoolean()"
.LC57:
	.string	"true"
.LC58:
	.string	"false"
.LC59:
	.string	"printBoolean() completed"
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
	movl	%edi, %eax	# b, tmp92
	movb	%al, -4(%rbp)	# tmp93, b
# src/Runtime/runtime.c:448:     DEBUG("Calling printBoolean()")
	movq	stderr(%rip), %rax	# stderr, stderr.176_1
	movq	%rax, %rcx	# stderr.176_1,
	movl	$14, %edx	#,
	movl	$1, %esi	#,
	leaq	.LC0(%rip), %rax	#, tmp94
	movq	%rax, %rdi	# tmp94,
	call	fwrite@PLT	#
	movq	stderr(%rip), %rax	# stderr, stderr.177_2
	movq	%rax, %rcx	# stderr.177_2,
	movl	$22, %edx	#,
	movl	$1, %esi	#,
	leaq	.LC56(%rip), %rax	#, tmp95
	movq	%rax, %rdi	# tmp95,
	call	fwrite@PLT	#
	movq	stderr(%rip), %rax	# stderr, stderr.178_3
	movq	%rax, %rsi	# stderr.178_3,
	movl	$10, %edi	#,
	call	fputc@PLT	#
	movq	stderr(%rip), %rax	# stderr, stderr.179_4
	movq	%rax, %rdi	# stderr.179_4,
	call	fflush@PLT	#
# src/Runtime/runtime.c:449:     if (b)
	cmpb	$0, -4(%rbp)	#, b
	je	.L125	#,
# src/Runtime/runtime.c:450:         printf("true\n");
	leaq	.LC57(%rip), %rax	#, tmp96
	movq	%rax, %rdi	# tmp96,
	call	puts@PLT	#
	jmp	.L126	#
.L125:
# src/Runtime/runtime.c:452:         printf("false\n");
	leaq	.LC58(%rip), %rax	#, tmp97
	movq	%rax, %rdi	# tmp97,
	call	puts@PLT	#
.L126:
# src/Runtime/runtime.c:453:     DEBUG("printBoolean() completed")
	movq	stderr(%rip), %rax	# stderr, stderr.180_5
	movq	%rax, %rcx	# stderr.180_5,
	movl	$14, %edx	#,
	movl	$1, %esi	#,
	leaq	.LC0(%rip), %rax	#, tmp98
	movq	%rax, %rdi	# tmp98,
	call	fwrite@PLT	#
	movq	stderr(%rip), %rax	# stderr, stderr.181_6
	movq	%rax, %rcx	# stderr.181_6,
	movl	$24, %edx	#,
	movl	$1, %esi	#,
	leaq	.LC59(%rip), %rax	#, tmp99
	movq	%rax, %rdi	# tmp99,
	call	fwrite@PLT	#
	movq	stderr(%rip), %rax	# stderr, stderr.182_7
	movq	%rax, %rsi	# stderr.182_7,
	movl	$10, %edi	#,
	call	fputc@PLT	#
	movq	stderr(%rip), %rax	# stderr, stderr.183_8
	movq	%rax, %rdi	# stderr.183_8,
	call	fflush@PLT	#
# src/Runtime/runtime.c:454:     return 0;
	movl	$0, %eax	#, _22
# src/Runtime/runtime.c:455: }
	leave	
	.cfi_def_cfa 7, 8
	ret	
	.cfi_endproc
.LFE36:
	.size	printBoolean, .-printBoolean
	.section	.rodata
	.align 8
.LC60:
	.string	"Calling intToString() conversion"
.LC61:
	.string	"%d"
	.align 8
.LC62:
	.string	"intToString() conversion completed"
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
# src/Runtime/runtime.c:457:     DEBUG("Calling intToString() conversion")
	movq	stderr(%rip), %rax	# stderr, stderr.184_1
	movq	%rax, %rcx	# stderr.184_1,
	movl	$14, %edx	#,
	movl	$1, %esi	#,
	leaq	.LC0(%rip), %rax	#, tmp92
	movq	%rax, %rdi	# tmp92,
	call	fwrite@PLT	#
	movq	stderr(%rip), %rax	# stderr, stderr.185_2
	movq	%rax, %rcx	# stderr.185_2,
	movl	$32, %edx	#,
	movl	$1, %esi	#,
	leaq	.LC60(%rip), %rax	#, tmp93
	movq	%rax, %rdi	# tmp93,
	call	fwrite@PLT	#
	movq	stderr(%rip), %rax	# stderr, stderr.186_3
	movq	%rax, %rsi	# stderr.186_3,
	movl	$10, %edi	#,
	call	fputc@PLT	#
	movq	stderr(%rip), %rax	# stderr, stderr.187_4
	movq	%rax, %rdi	# stderr.187_4,
	call	fflush@PLT	#
# src/Runtime/runtime.c:459:     sprintf(buffer, "%d", i);
	movl	-36(%rbp), %edx	# i, tmp94
	leaq	-19(%rbp), %rax	#, tmp95
	leaq	.LC61(%rip), %rcx	#, tmp96
	movq	%rcx, %rsi	# tmp96,
	movq	%rax, %rdi	# tmp95,
	movl	$0, %eax	#,
	call	sprintf@PLT	#
# src/Runtime/runtime.c:460:     obj ret = __createString(buffer);
	leaq	-19(%rbp), %rax	#, tmp97
	movq	%rax, %rdi	# tmp97,
	call	__createString	#
	movq	%rax, -8(%rbp)	# tmp98, ret
# src/Runtime/runtime.c:461:     __incRef(ret);
	movq	-8(%rbp), %rax	# ret, tmp99
	movq	%rax, %rdi	# tmp99,
	call	__incRef	#
# src/Runtime/runtime.c:462:     DEBUG("intToString() conversion completed")
	movq	stderr(%rip), %rax	# stderr, stderr.188_5
	movq	%rax, %rcx	# stderr.188_5,
	movl	$14, %edx	#,
	movl	$1, %esi	#,
	leaq	.LC0(%rip), %rax	#, tmp100
	movq	%rax, %rdi	# tmp100,
	call	fwrite@PLT	#
	movq	stderr(%rip), %rax	# stderr, stderr.189_6
	movq	%rax, %rcx	# stderr.189_6,
	movl	$34, %edx	#,
	movl	$1, %esi	#,
	leaq	.LC62(%rip), %rax	#, tmp101
	movq	%rax, %rdi	# tmp101,
	call	fwrite@PLT	#
	movq	stderr(%rip), %rax	# stderr, stderr.190_7
	movq	%rax, %rsi	# stderr.190_7,
	movl	$10, %edi	#,
	call	fputc@PLT	#
	movq	stderr(%rip), %rax	# stderr, stderr.191_8
	movq	%rax, %rdi	# stderr.191_8,
	call	fflush@PLT	#
# src/Runtime/runtime.c:463:     return ret;
	movq	-8(%rbp), %rax	# ret, _23
# src/Runtime/runtime.c:464: }
	leave	
	.cfi_def_cfa 7, 8
	ret	
	.cfi_endproc
.LFE37:
	.size	intToString, .-intToString
	.section	.rodata
	.align 8
.LC63:
	.string	"Calling byteToString() conversion"
.LC64:
	.string	"%u"
	.align 8
.LC65:
	.string	"byteToString() conversion completed"
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
	movl	%edi, %eax	# i, tmp93
	movb	%al, -36(%rbp)	# tmp94, i
# src/Runtime/runtime.c:467:     DEBUG("Calling byteToString() conversion")
	movq	stderr(%rip), %rax	# stderr, stderr.192_1
	movq	%rax, %rcx	# stderr.192_1,
	movl	$14, %edx	#,
	movl	$1, %esi	#,
	leaq	.LC0(%rip), %rax	#, tmp95
	movq	%rax, %rdi	# tmp95,
	call	fwrite@PLT	#
	movq	stderr(%rip), %rax	# stderr, stderr.193_2
	movq	%rax, %rcx	# stderr.193_2,
	movl	$33, %edx	#,
	movl	$1, %esi	#,
	leaq	.LC63(%rip), %rax	#, tmp96
	movq	%rax, %rdi	# tmp96,
	call	fwrite@PLT	#
	movq	stderr(%rip), %rax	# stderr, stderr.194_3
	movq	%rax, %rsi	# stderr.194_3,
	movl	$10, %edi	#,
	call	fputc@PLT	#
	movq	stderr(%rip), %rax	# stderr, stderr.195_4
	movq	%rax, %rdi	# stderr.195_4,
	call	fflush@PLT	#
# src/Runtime/runtime.c:469:     sprintf(buffer, "%u", i);
	movzbl	-36(%rbp), %edx	# i, _5
	leaq	-19(%rbp), %rax	#, tmp97
	leaq	.LC64(%rip), %rcx	#, tmp98
	movq	%rcx, %rsi	# tmp98,
	movq	%rax, %rdi	# tmp97,
	movl	$0, %eax	#,
	call	sprintf@PLT	#
# src/Runtime/runtime.c:470:     obj ret = __createString(buffer);
	leaq	-19(%rbp), %rax	#, tmp99
	movq	%rax, %rdi	# tmp99,
	call	__createString	#
	movq	%rax, -8(%rbp)	# tmp100, ret
# src/Runtime/runtime.c:471:     __incRef(ret);
	movq	-8(%rbp), %rax	# ret, tmp101
	movq	%rax, %rdi	# tmp101,
	call	__incRef	#
# src/Runtime/runtime.c:472:     DEBUG("byteToString() conversion completed")
	movq	stderr(%rip), %rax	# stderr, stderr.196_6
	movq	%rax, %rcx	# stderr.196_6,
	movl	$14, %edx	#,
	movl	$1, %esi	#,
	leaq	.LC0(%rip), %rax	#, tmp102
	movq	%rax, %rdi	# tmp102,
	call	fwrite@PLT	#
	movq	stderr(%rip), %rax	# stderr, stderr.197_7
	movq	%rax, %rcx	# stderr.197_7,
	movl	$35, %edx	#,
	movl	$1, %esi	#,
	leaq	.LC65(%rip), %rax	#, tmp103
	movq	%rax, %rdi	# tmp103,
	call	fwrite@PLT	#
	movq	stderr(%rip), %rax	# stderr, stderr.198_8
	movq	%rax, %rsi	# stderr.198_8,
	movl	$10, %edi	#,
	call	fputc@PLT	#
	movq	stderr(%rip), %rax	# stderr, stderr.199_9
	movq	%rax, %rdi	# stderr.199_9,
	call	fflush@PLT	#
# src/Runtime/runtime.c:473:     return ret;
	movq	-8(%rbp), %rax	# ret, _24
# src/Runtime/runtime.c:474: }
	leave	
	.cfi_def_cfa 7, 8
	ret	
	.cfi_endproc
.LFE38:
	.size	byteToString, .-byteToString
	.section	.rodata
	.align 8
.LC66:
	.string	"boolToString() conversion completed"
	.text
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
	movl	%edi, %eax	# b, tmp92
	movb	%al, -20(%rbp)	# tmp93, b
# src/Runtime/runtime.c:477:     DEBUG("boolToString() conversion completed")
	movq	stderr(%rip), %rax	# stderr, stderr.200_1
	movq	%rax, %rcx	# stderr.200_1,
	movl	$14, %edx	#,
	movl	$1, %esi	#,
	leaq	.LC0(%rip), %rax	#, tmp94
	movq	%rax, %rdi	# tmp94,
	call	fwrite@PLT	#
	movq	stderr(%rip), %rax	# stderr, stderr.201_2
	movq	%rax, %rcx	# stderr.201_2,
	movl	$35, %edx	#,
	movl	$1, %esi	#,
	leaq	.LC66(%rip), %rax	#, tmp95
	movq	%rax, %rdi	# tmp95,
	call	fwrite@PLT	#
	movq	stderr(%rip), %rax	# stderr, stderr.202_3
	movq	%rax, %rsi	# stderr.202_3,
	movl	$10, %edi	#,
	call	fputc@PLT	#
	movq	stderr(%rip), %rax	# stderr, stderr.203_4
	movq	%rax, %rdi	# stderr.203_4,
	call	fflush@PLT	#
# src/Runtime/runtime.c:479:     if (b)
	cmpb	$0, -20(%rbp)	#, b
	je	.L133	#,
# src/Runtime/runtime.c:480:         ret = __createString("true");
	leaq	.LC57(%rip), %rax	#, tmp96
	movq	%rax, %rdi	# tmp96,
	call	__createString	#
	movq	%rax, -8(%rbp)	# tmp97, ret
	jmp	.L134	#
.L133:
# src/Runtime/runtime.c:482:         ret = __createString("false");
	leaq	.LC58(%rip), %rax	#, tmp98
	movq	%rax, %rdi	# tmp98,
	call	__createString	#
	movq	%rax, -8(%rbp)	# tmp99, ret
.L134:
# src/Runtime/runtime.c:483:     __incRef(ret);
	movq	-8(%rbp), %rax	# ret, tmp100
	movq	%rax, %rdi	# tmp100,
	call	__incRef	#
# src/Runtime/runtime.c:484:     DEBUG("boolToString() conversion completed")
	movq	stderr(%rip), %rax	# stderr, stderr.204_5
	movq	%rax, %rcx	# stderr.204_5,
	movl	$14, %edx	#,
	movl	$1, %esi	#,
	leaq	.LC0(%rip), %rax	#, tmp101
	movq	%rax, %rdi	# tmp101,
	call	fwrite@PLT	#
	movq	stderr(%rip), %rax	# stderr, stderr.205_6
	movq	%rax, %rcx	# stderr.205_6,
	movl	$35, %edx	#,
	movl	$1, %esi	#,
	leaq	.LC66(%rip), %rax	#, tmp102
	movq	%rax, %rdi	# tmp102,
	call	fwrite@PLT	#
	movq	stderr(%rip), %rax	# stderr, stderr.206_7
	movq	%rax, %rsi	# stderr.206_7,
	movl	$10, %edi	#,
	call	fputc@PLT	#
	movq	stderr(%rip), %rax	# stderr, stderr.207_8
	movq	%rax, %rdi	# stderr.207_8,
	call	fflush@PLT	#
# src/Runtime/runtime.c:485:     return ret;
	movq	-8(%rbp), %rax	# ret, _26
# src/Runtime/runtime.c:486: }
	leave	
	.cfi_def_cfa 7, 8
	ret	
	.cfi_endproc
.LFE39:
	.size	boolToString, .-boolToString
	.section	.rodata
.LC67:
	.string	"Calling generic print(obj)"
	.align 8
.LC68:
	.string	"Subcall to internal printString() method"
.LC69:
	.string	"Generic print(obj) completed"
	.text
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
# src/Runtime/runtime.c:489:     DEBUG("Calling generic print(obj)")
	movq	stderr(%rip), %rax	# stderr, stderr.208_1
	movq	%rax, %rcx	# stderr.208_1,
	movl	$14, %edx	#,
	movl	$1, %esi	#,
	leaq	.LC0(%rip), %rax	#, tmp99
	movq	%rax, %rdi	# tmp99,
	call	fwrite@PLT	#
	movq	stderr(%rip), %rax	# stderr, stderr.209_2
	movq	%rax, %rcx	# stderr.209_2,
	movl	$26, %edx	#,
	movl	$1, %esi	#,
	leaq	.LC67(%rip), %rax	#, tmp100
	movq	%rax, %rdi	# tmp100,
	call	fwrite@PLT	#
	movq	stderr(%rip), %rax	# stderr, stderr.210_3
	movq	%rax, %rsi	# stderr.210_3,
	movl	$10, %edi	#,
	call	fputc@PLT	#
	movq	stderr(%rip), %rax	# stderr, stderr.211_4
	movq	%rax, %rdi	# stderr.211_4,
	call	fflush@PLT	#
# src/Runtime/runtime.c:490:     if (o == NULL)
	cmpq	$0, -24(%rbp)	#, o
	jne	.L137	#,
# src/Runtime/runtime.c:491:         o = __createString("null");
	leaq	.LC34(%rip), %rax	#, tmp101
	movq	%rax, %rdi	# tmp101,
	call	__createString	#
	movq	%rax, -24(%rbp)	# tmp102, o
.L137:
# src/Runtime/runtime.c:492:     __incRef(o);
	movq	-24(%rbp), %rax	# o, tmp103
	movq	%rax, %rdi	# tmp103,
	call	__incRef	#
# src/Runtime/runtime.c:493:     obj (*toStr)(obj) = ((void **)o->type->methods)[0];
	movq	-24(%rbp), %rax	# o, tmp104
	movq	(%rax), %rax	# o_16->type, _5
# src/Runtime/runtime.c:493:     obj (*toStr)(obj) = ((void **)o->type->methods)[0];
	movq	12(%rax), %rax	# _5->methods, _6
# src/Runtime/runtime.c:493:     obj (*toStr)(obj) = ((void **)o->type->methods)[0];
	movq	(%rax), %rax	# MEM[(void * *)_6], _7
# src/Runtime/runtime.c:493:     obj (*toStr)(obj) = ((void **)o->type->methods)[0];
	movq	%rax, -8(%rbp)	# _7, toStr
# src/Runtime/runtime.c:494:     obj str = toStr(o);
	movq	-24(%rbp), %rax	# o, tmp105
	movq	-8(%rbp), %rdx	# toStr, tmp106
	movq	%rax, %rdi	# tmp105,
	call	*%rdx	# tmp106
	movq	%rax, -16(%rbp)	# tmp107, str
# src/Runtime/runtime.c:495:     DEBUG("Subcall to internal printString() method")
	movq	stderr(%rip), %rax	# stderr, stderr.212_8
	movq	%rax, %rcx	# stderr.212_8,
	movl	$14, %edx	#,
	movl	$1, %esi	#,
	leaq	.LC0(%rip), %rax	#, tmp108
	movq	%rax, %rdi	# tmp108,
	call	fwrite@PLT	#
	movq	stderr(%rip), %rax	# stderr, stderr.213_9
	movq	%rax, %rcx	# stderr.213_9,
	movl	$40, %edx	#,
	movl	$1, %esi	#,
	leaq	.LC68(%rip), %rax	#, tmp109
	movq	%rax, %rdi	# tmp109,
	call	fwrite@PLT	#
	movq	stderr(%rip), %rax	# stderr, stderr.214_10
	movq	%rax, %rsi	# stderr.214_10,
	movl	$10, %edi	#,
	call	fputc@PLT	#
	movq	stderr(%rip), %rax	# stderr, stderr.215_11
	movq	%rax, %rdi	# stderr.215_11,
	call	fflush@PLT	#
# src/Runtime/runtime.c:496:     printString(str);
	movq	-16(%rbp), %rax	# str, tmp110
	movq	%rax, %rdi	# tmp110,
	call	printString	#
# src/Runtime/runtime.c:497:     __decRef(str);
	movq	-16(%rbp), %rax	# str, tmp111
	movq	%rax, %rdi	# tmp111,
	call	__decRef	#
# src/Runtime/runtime.c:498:     __decRef(o);
	movq	-24(%rbp), %rax	# o, tmp112
	movq	%rax, %rdi	# tmp112,
	call	__decRef	#
# src/Runtime/runtime.c:499:     DEBUG("Generic print(obj) completed")
	movq	stderr(%rip), %rax	# stderr, stderr.216_12
	movq	%rax, %rcx	# stderr.216_12,
	movl	$14, %edx	#,
	movl	$1, %esi	#,
	leaq	.LC0(%rip), %rax	#, tmp113
	movq	%rax, %rdi	# tmp113,
	call	fwrite@PLT	#
	movq	stderr(%rip), %rax	# stderr, stderr.217_13
	movq	%rax, %rcx	# stderr.217_13,
	movl	$28, %edx	#,
	movl	$1, %esi	#,
	leaq	.LC69(%rip), %rax	#, tmp114
	movq	%rax, %rdi	# tmp114,
	call	fwrite@PLT	#
	movq	stderr(%rip), %rax	# stderr, stderr.218_14
	movq	%rax, %rsi	# stderr.218_14,
	movl	$10, %edi	#,
	call	fputc@PLT	#
	movq	stderr(%rip), %rax	# stderr, stderr.219_15
	movq	%rax, %rdi	# stderr.219_15,
	call	fflush@PLT	#
# src/Runtime/runtime.c:500:     return 0;
	movl	$0, %eax	#, _41
# src/Runtime/runtime.c:501: }
	leave	
	.cfi_def_cfa 7, 8
	ret	
	.cfi_endproc
.LFE40:
	.size	print, .-print
	.section	.rodata
.LC70:
	.string	"Calling printBinArray(arr)"
	.align 8
.LC71:
	.string	"printBinArray(arr) call completed"
	.text
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
# src/Runtime/runtime.c:504:     DEBUG("Calling printBinArray(arr)")
	movq	stderr(%rip), %rax	# stderr, stderr.220_1
	movq	%rax, %rcx	# stderr.220_1,
	movl	$14, %edx	#,
	movl	$1, %esi	#,
	leaq	.LC0(%rip), %rax	#, tmp96
	movq	%rax, %rdi	# tmp96,
	call	fwrite@PLT	#
	movq	stderr(%rip), %rax	# stderr, stderr.221_2
	movq	%rax, %rcx	# stderr.221_2,
	movl	$26, %edx	#,
	movl	$1, %esi	#,
	leaq	.LC70(%rip), %rax	#, tmp97
	movq	%rax, %rdi	# tmp97,
	call	fwrite@PLT	#
	movq	stderr(%rip), %rax	# stderr, stderr.222_3
	movq	%rax, %rsi	# stderr.222_3,
	movl	$10, %edi	#,
	call	fputc@PLT	#
	movq	stderr(%rip), %rax	# stderr, stderr.223_4
	movq	%rax, %rdi	# stderr.223_4,
	call	fflush@PLT	#
# src/Runtime/runtime.c:505:     if (arr == NULL){
	cmpq	$0, -24(%rbp)	#, arr
	jne	.L140	#,
# src/Runtime/runtime.c:506:         print(arr);
	movq	-24(%rbp), %rax	# arr, tmp98
	movq	%rax, %rdi	# tmp98,
	call	print	#
# src/Runtime/runtime.c:507:         return 0;
	movl	$0, %eax	#, _13
	jmp	.L141	#
.L140:
# src/Runtime/runtime.c:509:     __incRef(arr);
	movq	-24(%rbp), %rax	# arr, tmp99
	movq	%rax, %rdi	# tmp99,
	call	__incRef	#
# src/Runtime/runtime.c:510:     struct Array *array = arr->data;
	movq	-24(%rbp), %rax	# arr, tmp100
	movq	8(%rax), %rax	# arr_20(D)->data, tmp101
	movq	%rax, -8(%rbp)	# tmp101, array
# src/Runtime/runtime.c:511:     fwrite(array->elements, sizeof(int8_t), array->length, stdout);
	movq	stdout(%rip), %rcx	# stdout, stdout.224_5
# src/Runtime/runtime.c:511:     fwrite(array->elements, sizeof(int8_t), array->length, stdout);
	movq	-8(%rbp), %rax	# array, tmp102
	movl	4(%rax), %eax	# array_22->length, _6
# src/Runtime/runtime.c:511:     fwrite(array->elements, sizeof(int8_t), array->length, stdout);
	movslq	%eax, %rdx	# _6, _7
# src/Runtime/runtime.c:511:     fwrite(array->elements, sizeof(int8_t), array->length, stdout);
	movq	-8(%rbp), %rax	# array, tmp103
	movq	8(%rax), %rax	# array_22->elements, _8
# src/Runtime/runtime.c:511:     fwrite(array->elements, sizeof(int8_t), array->length, stdout);
	movl	$1, %esi	#,
	movq	%rax, %rdi	# _8,
	call	fwrite@PLT	#
# src/Runtime/runtime.c:512:     __decRef(arr);
	movq	-24(%rbp), %rax	# arr, tmp104
	movq	%rax, %rdi	# tmp104,
	call	__decRef	#
# src/Runtime/runtime.c:513:     DEBUG("printBinArray(arr) call completed")
	movq	stderr(%rip), %rax	# stderr, stderr.225_9
	movq	%rax, %rcx	# stderr.225_9,
	movl	$14, %edx	#,
	movl	$1, %esi	#,
	leaq	.LC0(%rip), %rax	#, tmp105
	movq	%rax, %rdi	# tmp105,
	call	fwrite@PLT	#
	movq	stderr(%rip), %rax	# stderr, stderr.226_10
	movq	%rax, %rcx	# stderr.226_10,
	movl	$33, %edx	#,
	movl	$1, %esi	#,
	leaq	.LC71(%rip), %rax	#, tmp106
	movq	%rax, %rdi	# tmp106,
	call	fwrite@PLT	#
	movq	stderr(%rip), %rax	# stderr, stderr.227_11
	movq	%rax, %rsi	# stderr.227_11,
	movl	$10, %edi	#,
	call	fputc@PLT	#
	movq	stderr(%rip), %rax	# stderr, stderr.228_12
	movq	%rax, %rdi	# stderr.228_12,
	call	fflush@PLT	#
# src/Runtime/runtime.c:514:     return 0;
	movl	$0, %eax	#, _13
.L141:
# src/Runtime/runtime.c:515: }
	leave	
	.cfi_def_cfa 7, 8
	ret	
	.cfi_endproc
.LFE41:
	.size	printBinArray, .-printBinArray
	.section	.rodata
.LC72:
	.string	"Calling error()"
.LC73:
	.string	"%s\n"
.LC74:
	.string	"ERROR: User error."
.LC75:
	.string	"Exiting via error() (exit=1)"
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
# src/Runtime/runtime.c:518:     DEBUG("Calling error()")
	movq	stderr(%rip), %rax	# stderr, stderr.229_1
	movq	%rax, %rcx	# stderr.229_1,
	movl	$14, %edx	#,
	movl	$1, %esi	#,
	leaq	.LC0(%rip), %rax	#, tmp95
	movq	%rax, %rdi	# tmp95,
	call	fwrite@PLT	#
	movq	stderr(%rip), %rax	# stderr, stderr.230_2
	movq	%rax, %rcx	# stderr.230_2,
	movl	$15, %edx	#,
	movl	$1, %esi	#,
	leaq	.LC72(%rip), %rax	#, tmp96
	movq	%rax, %rdi	# tmp96,
	call	fwrite@PLT	#
	movq	stderr(%rip), %rax	# stderr, stderr.231_3
	movq	%rax, %rsi	# stderr.231_3,
	movl	$10, %edi	#,
	call	fputc@PLT	#
	movq	stderr(%rip), %rax	# stderr, stderr.232_4
	movq	%rax, %rdi	# stderr.232_4,
	call	fflush@PLT	#
# src/Runtime/runtime.c:519:     if (errMsg != NULL)
	movq	errMsg(%rip), %rax	# errMsg, errMsg.233_5
# src/Runtime/runtime.c:519:     if (errMsg != NULL)
	testq	%rax, %rax	# errMsg.233_5
	je	.L143	#,
# src/Runtime/runtime.c:520:         fprintf(stderr, "%s\n", errMsg);
	movq	errMsg(%rip), %rdx	# errMsg, errMsg.234_6
	movq	stderr(%rip), %rax	# stderr, stderr.235_7
	leaq	.LC73(%rip), %rcx	#, tmp97
	movq	%rcx, %rsi	# tmp97,
	movq	%rax, %rdi	# stderr.235_7,
	movl	$0, %eax	#,
	call	fprintf@PLT	#
	jmp	.L144	#
.L143:
# src/Runtime/runtime.c:522:         fprintf(stderr, "%s\n", "ERROR: User error.");
	movq	stderr(%rip), %rax	# stderr, stderr.236_8
	leaq	.LC74(%rip), %rdx	#, tmp98
	leaq	.LC73(%rip), %rcx	#, tmp99
	movq	%rcx, %rsi	# tmp99,
	movq	%rax, %rdi	# stderr.236_8,
	movl	$0, %eax	#,
	call	fprintf@PLT	#
.L144:
# src/Runtime/runtime.c:523:     DEBUG("Exiting via error() (exit=1)")
	movq	stderr(%rip), %rax	# stderr, stderr.237_9
	movq	%rax, %rcx	# stderr.237_9,
	movl	$14, %edx	#,
	movl	$1, %esi	#,
	leaq	.LC0(%rip), %rax	#, tmp100
	movq	%rax, %rdi	# tmp100,
	call	fwrite@PLT	#
	movq	stderr(%rip), %rax	# stderr, stderr.238_10
	movq	%rax, %rcx	# stderr.238_10,
	movl	$28, %edx	#,
	movl	$1, %esi	#,
	leaq	.LC75(%rip), %rax	#, tmp101
	movq	%rax, %rdi	# tmp101,
	call	fwrite@PLT	#
	movq	stderr(%rip), %rax	# stderr, stderr.239_11
	movq	%rax, %rsi	# stderr.239_11,
	movl	$10, %edi	#,
	call	fputc@PLT	#
	movq	stderr(%rip), %rax	# stderr, stderr.240_12
	movq	%rax, %rdi	# stderr.240_12,
	call	fflush@PLT	#
# src/Runtime/runtime.c:524:     exit(1);
	movl	$1, %edi	#,
	call	exit@PLT	#
	.cfi_endproc
.LFE42:
	.size	error, .-error
	.section	.rodata
.LC76:
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
# src/Runtime/runtime.c:530:     char *line = NULL;
	movq	$0, -24(%rbp)	#, line
# src/Runtime/runtime.c:531:     size_t size = 0;
	movq	$0, -32(%rbp)	#, size
# src/Runtime/runtime.c:532:     ssize_t unused = getline(&line, &size, stdin);
	movq	stdin(%rip), %rdx	# stdin, stdin.241_1
	leaq	-32(%rbp), %rcx	#, tmp87
	leaq	-24(%rbp), %rax	#, tmp88
	movq	%rcx, %rsi	# tmp87,
	movq	%rax, %rdi	# tmp88,
	call	getline@PLT	#
	movq	%rax, -8(%rbp)	# tmp89, unused
# src/Runtime/runtime.c:533:     int unused2 = sscanf(line, "%d ", &i);
	movq	-24(%rbp), %rax	# line, line.242_2
	leaq	-16(%rbp), %rdx	#, tmp90
	leaq	.LC76(%rip), %rcx	#, tmp91
	movq	%rcx, %rsi	# tmp91,
	movq	%rax, %rdi	# line.242_2,
	movl	$0, %eax	#,
	call	__isoc99_sscanf@PLT	#
	movl	%eax, -12(%rbp)	# tmp92, unused2
# src/Runtime/runtime.c:534:     free(line);
	movq	-24(%rbp), %rax	# line, line.243_3
	movq	%rax, %rdi	# line.243_3,
	call	free@PLT	#
# src/Runtime/runtime.c:535:     return i;
	movl	-16(%rbp), %eax	# i, _12
# src/Runtime/runtime.c:536: }
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
# src/Runtime/runtime.c:538:     char *line = NULL;
	movq	$0, -24(%rbp)	#, line
# src/Runtime/runtime.c:539:     size_t size = 0;
	movq	$0, -32(%rbp)	#, size
# src/Runtime/runtime.c:540:     ssize_t unused = getline(&line, &size, stdin);
	movq	stdin(%rip), %rdx	# stdin, stdin.244_1
	leaq	-32(%rbp), %rcx	#, tmp93
	leaq	-24(%rbp), %rax	#, tmp94
	movq	%rcx, %rsi	# tmp93,
	movq	%rax, %rdi	# tmp94,
	call	getline@PLT	#
	movq	%rax, -8(%rbp)	# tmp95, unused
# src/Runtime/runtime.c:541:     size = u8_strlen(line);
	movq	-24(%rbp), %rax	# line, line.245_2
	movq	%rax, %rdi	# line.245_2,
	call	u8_strlen@PLT	#
# src/Runtime/runtime.c:541:     size = u8_strlen(line);
	movq	%rax, -32(%rbp)	# _3, size
# src/Runtime/runtime.c:542:     line[size - 1] = 0; // remove newline
	movq	-24(%rbp), %rax	# line, line.246_4
	movq	-32(%rbp), %rdx	# size, size.247_5
	subq	$1, %rdx	#, _6
	addq	%rdx, %rax	# _6, _7
# src/Runtime/runtime.c:542:     line[size - 1] = 0; // remove newline
	movb	$0, (%rax)	#, *_7
# src/Runtime/runtime.c:543:     obj l = __createString(line);
	movq	-24(%rbp), %rax	# line, line.248_8
	movq	%rax, %rdi	# line.248_8,
	call	__createString	#
	movq	%rax, -16(%rbp)	# tmp96, l
# src/Runtime/runtime.c:544:     __incRef(l);
	movq	-16(%rbp), %rax	# l, tmp97
	movq	%rax, %rdi	# tmp97,
	call	__incRef	#
# src/Runtime/runtime.c:545:     free(line);
	movq	-24(%rbp), %rax	# line, line.249_9
	movq	%rax, %rdi	# line.249_9,
	call	free@PLT	#
# src/Runtime/runtime.c:546:     return l;
	movq	-16(%rbp), %rax	# l, _21
# src/Runtime/runtime.c:547: }
	leave	
	.cfi_def_cfa 7, 8
	ret	
	.cfi_endproc
.LFE44:
	.size	readString, .-readString
	.ident	"GCC: (Debian 12.2.0-14) 12.2.0"
	.section	.note.GNU-stack,"",@progbits
