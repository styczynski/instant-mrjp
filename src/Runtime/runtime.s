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
	.align 8
.LC6:
	.string	"Completed __new %p <type %p, par %p> inner data=%p size=%d"
	.align 8
.LC7:
	.string	"Just check. R+36=%p (is null=%d)"
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
# src/Runtime/runtime.c:31:     DEBUG("Calling __new v3");
	movq	stderr(%rip), %rax	# stderr, stderr.0_1
	movq	%rax, %rcx	# stderr.0_1,
	movl	$14, %edx	#,
	movl	$1, %esi	#,
	leaq	.LC0(%rip), %rax	#, tmp133
	movq	%rax, %rdi	# tmp133,
	call	fwrite@PLT	#
	movq	stderr(%rip), %rax	# stderr, stderr.1_2
	movq	%rax, %rcx	# stderr.1_2,
	movl	$16, %edx	#,
	movl	$1, %esi	#,
	leaq	.LC1(%rip), %rax	#, tmp134
	movq	%rax, %rdi	# tmp134,
	call	fwrite@PLT	#
	movq	stderr(%rip), %rax	# stderr, stderr.2_3
	movq	%rax, %rsi	# stderr.2_3,
	movl	$10, %edi	#,
	call	fputc@PLT	#
	movq	stderr(%rip), %rax	# stderr, stderr.3_4
	movq	%rax, %rdi	# stderr.3_4,
	call	fflush@PLT	#
# src/Runtime/runtime.c:32:     DEBUG("Perform reference malloc type=%p", FORMAT_PTR(t));
	movq	stderr(%rip), %rax	# stderr, stderr.4_5
	movq	%rax, %rcx	# stderr.4_5,
	movl	$14, %edx	#,
	movl	$1, %esi	#,
	leaq	.LC0(%rip), %rax	#, tmp135
	movq	%rax, %rdi	# tmp135,
	call	fwrite@PLT	#
	movq	stderr(%rip), %rax	# stderr, stderr.5_6
	movq	-24(%rbp), %rdx	# t, tmp136
	leaq	.LC2(%rip), %rcx	#, tmp137
	movq	%rcx, %rsi	# tmp137,
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
# src/Runtime/runtime.c:33:     DEBUG("__new examine type: <type parent=%p> [%d]", FORMAT_PTR(t->parent), t->dataSize);
	movq	stderr(%rip), %rax	# stderr, stderr.8_9
	movq	%rax, %rcx	# stderr.8_9,
	movl	$14, %edx	#,
	movl	$1, %esi	#,
	leaq	.LC0(%rip), %rax	#, tmp138
	movq	%rax, %rdi	# tmp138,
	call	fwrite@PLT	#
	movq	-24(%rbp), %rax	# t, tmp139
	movl	16(%rax), %ecx	# t_57(D)->dataSize, _10
	movq	-24(%rbp), %rax	# t, tmp140
	movq	(%rax), %rdx	# t_57(D)->parent, _11
	movq	stderr(%rip), %rax	# stderr, stderr.9_12
	leaq	.LC3(%rip), %rsi	#, tmp141
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
# src/Runtime/runtime.c:34:     obj r = malloc(sizeof(struct Reference)+t->dataSize+10);
	movq	-24(%rbp), %rax	# t, tmp142
	movl	16(%rax), %eax	# t_57(D)->dataSize, _15
	cltq
# src/Runtime/runtime.c:34:     obj r = malloc(sizeof(struct Reference)+t->dataSize+10);
	addq	$54, %rax	#, _17
	movq	%rax, %rdi	# _17,
	call	malloc@PLT	#
	movq	%rax, -8(%rbp)	# tmp143, r
# src/Runtime/runtime.c:35:     DEBUG("Set __new type/counter");
	movq	stderr(%rip), %rax	# stderr, stderr.12_18
	movq	%rax, %rcx	# stderr.12_18,
	movl	$14, %edx	#,
	movl	$1, %esi	#,
	leaq	.LC0(%rip), %rax	#, tmp144
	movq	%rax, %rdi	# tmp144,
	call	fwrite@PLT	#
	movq	stderr(%rip), %rax	# stderr, stderr.13_19
	movq	%rax, %rcx	# stderr.13_19,
	movl	$22, %edx	#,
	movl	$1, %esi	#,
	leaq	.LC4(%rip), %rax	#, tmp145
	movq	%rax, %rdi	# tmp145,
	call	fwrite@PLT	#
	movq	stderr(%rip), %rax	# stderr, stderr.14_20
	movq	%rax, %rsi	# stderr.14_20,
	movl	$10, %edi	#,
	call	fputc@PLT	#
	movq	stderr(%rip), %rax	# stderr, stderr.15_21
	movq	%rax, %rdi	# stderr.15_21,
	call	fflush@PLT	#
# src/Runtime/runtime.c:36:     r->type = t;
	movq	-8(%rbp), %rax	# r, tmp146
	movq	-24(%rbp), %rdx	# t, tmp147
	movq	%rdx, (%rax)	# tmp147, r_66->type
# src/Runtime/runtime.c:37:     r->counter = 0;
	movq	-8(%rbp), %rax	# r, tmp148
	movl	$0, 16(%rax)	#, r_66->counter
# src/Runtime/runtime.c:38:     r->data = NULL;
	movq	-8(%rbp), %rax	# r, tmp149
	movq	$0, 8(%rax)	#, r_66->data
# src/Runtime/runtime.c:39:     r->methods = r->type->methods;
	movq	-8(%rbp), %rax	# r, tmp150
	movq	(%rax), %rax	# r_66->type, _22
# src/Runtime/runtime.c:39:     r->methods = r->type->methods;
	movq	20(%rax), %rdx	# _22->methods, _23
# src/Runtime/runtime.c:39:     r->methods = r->type->methods;
	movq	-8(%rbp), %rax	# r, tmp151
	movq	%rdx, 20(%rax)	# _23, r_66->methods
# src/Runtime/runtime.c:40:     r->length=0;
	movq	-8(%rbp), %rax	# r, tmp152
	movl	$0, 32(%rax)	#, r_66->length
# src/Runtime/runtime.c:41:     DEBUG("Do init for non array non string?");
	movq	stderr(%rip), %rax	# stderr, stderr.16_24
	movq	%rax, %rcx	# stderr.16_24,
	movl	$14, %edx	#,
	movl	$1, %esi	#,
	leaq	.LC0(%rip), %rax	#, tmp153
	movq	%rax, %rdi	# tmp153,
	call	fwrite@PLT	#
	movq	stderr(%rip), %rax	# stderr, stderr.17_25
	movq	%rax, %rcx	# stderr.17_25,
	movl	$33, %edx	#,
	movl	$1, %esi	#,
	leaq	.LC5(%rip), %rax	#, tmp154
	movq	%rax, %rdi	# tmp154,
	call	fwrite@PLT	#
	movq	stderr(%rip), %rax	# stderr, stderr.18_26
	movq	%rax, %rsi	# stderr.18_26,
	movl	$10, %edi	#,
	call	fputc@PLT	#
	movq	stderr(%rip), %rax	# stderr, stderr.19_27
	movq	%rax, %rdi	# stderr.19_27,
	call	fflush@PLT	#
# src/Runtime/runtime.c:42:     if (t->dataSize > 0) {
	movq	-24(%rbp), %rax	# t, tmp155
	movl	16(%rax), %eax	# t_57(D)->dataSize, _28
# src/Runtime/runtime.c:42:     if (t->dataSize > 0) {
	testl	%eax, %eax	# _28
	jle	.L2	#,
# src/Runtime/runtime.c:44:         memcpy(&(r->others), t->initializer, t->dataSize);
	movq	-24(%rbp), %rax	# t, tmp156
	movl	16(%rax), %eax	# t_57(D)->dataSize, _29
# src/Runtime/runtime.c:44:         memcpy(&(r->others), t->initializer, t->dataSize);
	movslq	%eax, %rdx	# _29, _30
# src/Runtime/runtime.c:44:         memcpy(&(r->others), t->initializer, t->dataSize);
	movq	-24(%rbp), %rax	# t, tmp157
	movq	8(%rax), %rax	# t_57(D)->initializer, _31
# src/Runtime/runtime.c:44:         memcpy(&(r->others), t->initializer, t->dataSize);
	movq	-8(%rbp), %rcx	# r, tmp158
	addq	$36, %rcx	#, _32
# src/Runtime/runtime.c:44:         memcpy(&(r->others), t->initializer, t->dataSize);
	movq	%rax, %rsi	# _31,
	movq	%rcx, %rdi	# _32,
	call	memcpy@PLT	#
.L2:
# src/Runtime/runtime.c:54:     DEBUG("Completed __new %p <type %p, par %p> inner data=%p size=%d", FORMAT_PTR(r), FORMAT_PTR(r->type), FORMAT_PTR(r->type->parent), FORMAT_PTR(r->data), t->dataSize);
	movq	stderr(%rip), %rax	# stderr, stderr.20_33
	movq	%rax, %rcx	# stderr.20_33,
	movl	$14, %edx	#,
	movl	$1, %esi	#,
	leaq	.LC0(%rip), %rax	#, tmp159
	movq	%rax, %rdi	# tmp159,
	call	fwrite@PLT	#
	movq	-24(%rbp), %rax	# t, tmp160
	movl	16(%rax), %esi	# t_57(D)->dataSize, _34
	movq	-8(%rbp), %rax	# r, tmp161
	movq	8(%rax), %r8	# r_66->data, _35
	movq	-8(%rbp), %rax	# r, tmp162
	movq	(%rax), %rax	# r_66->type, _36
	movq	(%rax), %rdi	# _36->parent, _37
	movq	-8(%rbp), %rax	# r, tmp163
	movq	(%rax), %rcx	# r_66->type, _38
	movq	stderr(%rip), %rax	# stderr, stderr.21_39
	movq	-8(%rbp), %rdx	# r, tmp164
	subq	$8, %rsp	#,
	pushq	%rsi	# _34
	movq	%r8, %r9	# _35,
	movq	%rdi, %r8	# _37,
	leaq	.LC6(%rip), %rsi	#, tmp165
	movq	%rax, %rdi	# stderr.21_39,
	movl	$0, %eax	#,
	call	fprintf@PLT	#
	addq	$16, %rsp	#,
	movq	stderr(%rip), %rax	# stderr, stderr.22_40
	movq	%rax, %rsi	# stderr.22_40,
	movl	$10, %edi	#,
	call	fputc@PLT	#
	movq	stderr(%rip), %rax	# stderr, stderr.23_41
	movq	%rax, %rdi	# stderr.23_41,
	call	fflush@PLT	#
# src/Runtime/runtime.c:55:     uint64_t test = *((uint64_t*)(r+36));DEBUG("Just check. R+36=%p (is null=%d)", FORMAT_PTR(test), IS_NULL(((void*)test)));
	movq	-8(%rbp), %rax	# r, tmp166
	movq	1584(%rax), %rax	# MEM[(uint64_t *)r_66 + 1584B], tmp167
	movq	%rax, -16(%rbp)	# tmp167, test
# src/Runtime/runtime.c:55:     uint64_t test = *((uint64_t*)(r+36));DEBUG("Just check. R+36=%p (is null=%d)", FORMAT_PTR(test), IS_NULL(((void*)test)));
	movq	stderr(%rip), %rax	# stderr, stderr.24_42
	movq	%rax, %rcx	# stderr.24_42,
	movl	$14, %edx	#,
	movl	$1, %esi	#,
	leaq	.LC0(%rip), %rax	#, tmp168
	movq	%rax, %rdi	# tmp168,
	call	fwrite@PLT	#
	movq	-16(%rbp), %rdx	# test, test.25_43
	leaq	_LAT_NULL(%rip), %rax	#, tmp169
	cmpq	%rax, %rdx	# tmp169, test.25_43
	sete	%al	#, _44
	movzbl	%al, %ecx	# _44, _45
	movq	-16(%rbp), %rdx	# test, test.26_46
	movq	stderr(%rip), %rax	# stderr, stderr.27_47
	leaq	.LC7(%rip), %rsi	#, tmp170
	movq	%rax, %rdi	# stderr.27_47,
	movl	$0, %eax	#,
	call	fprintf@PLT	#
	movq	stderr(%rip), %rax	# stderr, stderr.28_48
	movq	%rax, %rsi	# stderr.28_48,
	movl	$10, %edi	#,
	call	fputc@PLT	#
	movq	stderr(%rip), %rax	# stderr, stderr.29_49
	movq	%rax, %rdi	# stderr.29_49,
	call	fflush@PLT	#
# src/Runtime/runtime.c:56:     return r;
	movq	-8(%rbp), %rax	# r, _90
# src/Runtime/runtime.c:57: }
	leave	
	.cfi_def_cfa 7, 8
	ret	
	.cfi_endproc
.LFE6:
	.size	__new, .-__new
	.section	.rodata
.LC8:
	.string	"__free %p"
	.align 8
.LC9:
	.string	"Free completed. Value is nullish"
.LC10:
	.string	"Free got Array"
.LC11:
	.string	"Free got String"
.LC12:
	.string	"Free underlying String data"
.LC13:
	.string	"Free got other"
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
# src/Runtime/runtime.c:60:     DEBUG("__free %p", FORMAT_PTR(r));
	movq	stderr(%rip), %rax	# stderr, stderr.30_1
	movq	%rax, %rcx	# stderr.30_1,
	movl	$14, %edx	#,
	movl	$1, %esi	#,
	leaq	.LC0(%rip), %rax	#, tmp116
	movq	%rax, %rdi	# tmp116,
	call	fwrite@PLT	#
	movq	stderr(%rip), %rax	# stderr, stderr.31_2
	movq	-40(%rbp), %rdx	# r, tmp117
	leaq	.LC8(%rip), %rcx	#, tmp118
	movq	%rcx, %rsi	# tmp118,
	movq	%rax, %rdi	# stderr.31_2,
	movl	$0, %eax	#,
	call	fprintf@PLT	#
	movq	stderr(%rip), %rax	# stderr, stderr.32_3
	movq	%rax, %rsi	# stderr.32_3,
	movl	$10, %edi	#,
	call	fputc@PLT	#
	movq	stderr(%rip), %rax	# stderr, stderr.33_4
	movq	%rax, %rdi	# stderr.33_4,
	call	fflush@PLT	#
# src/Runtime/runtime.c:61:     if (IS_NULL(r) || r == NULL) {
	leaq	_LAT_NULL(%rip), %rax	#, tmp119
	cmpq	%rax, -40(%rbp)	# tmp119, r
	je	.L5	#,
# src/Runtime/runtime.c:61:     if (IS_NULL(r) || r == NULL) {
	cmpq	$0, -40(%rbp)	#, r
	jne	.L6	#,
.L5:
# src/Runtime/runtime.c:62:         DEBUG("Free completed. Value is nullish");
	movq	stderr(%rip), %rax	# stderr, stderr.34_5
	movq	%rax, %rcx	# stderr.34_5,
	movl	$14, %edx	#,
	movl	$1, %esi	#,
	leaq	.LC0(%rip), %rax	#, tmp120
	movq	%rax, %rdi	# tmp120,
	call	fwrite@PLT	#
	movq	stderr(%rip), %rax	# stderr, stderr.35_6
	movq	%rax, %rcx	# stderr.35_6,
	movl	$32, %edx	#,
	movl	$1, %esi	#,
	leaq	.LC9(%rip), %rax	#, tmp121
	movq	%rax, %rdi	# tmp121,
	call	fwrite@PLT	#
	movq	stderr(%rip), %rax	# stderr, stderr.36_7
	movq	%rax, %rsi	# stderr.36_7,
	movl	$10, %edi	#,
	call	fputc@PLT	#
	movq	stderr(%rip), %rax	# stderr, stderr.37_8
	movq	%rax, %rdi	# stderr.37_8,
	call	fflush@PLT	#
# src/Runtime/runtime.c:63:         return;
	jmp	.L4	#
.L6:
# src/Runtime/runtime.c:65:     if (r->type == &_class_Array) {
	movq	-40(%rbp), %rax	# r, tmp122
	movq	(%rax), %rdx	# r_42(D)->type, _9
# src/Runtime/runtime.c:65:     if (r->type == &_class_Array) {
	leaq	_class_Array(%rip), %rax	#, tmp123
	cmpq	%rax, %rdx	# tmp123, _9
	jne	.L8	#,
# src/Runtime/runtime.c:66:         DEBUG("Free got Array");
	movq	stderr(%rip), %rax	# stderr, stderr.38_10
	movq	%rax, %rcx	# stderr.38_10,
	movl	$14, %edx	#,
	movl	$1, %esi	#,
	leaq	.LC0(%rip), %rax	#, tmp124
	movq	%rax, %rdi	# tmp124,
	call	fwrite@PLT	#
	movq	stderr(%rip), %rax	# stderr, stderr.39_11
	movq	%rax, %rcx	# stderr.39_11,
	movl	$14, %edx	#,
	movl	$1, %esi	#,
	leaq	.LC10(%rip), %rax	#, tmp125
	movq	%rax, %rdi	# tmp125,
	call	fwrite@PLT	#
	movq	stderr(%rip), %rax	# stderr, stderr.40_12
	movq	%rax, %rsi	# stderr.40_12,
	movl	$10, %edi	#,
	call	fputc@PLT	#
	movq	stderr(%rip), %rax	# stderr, stderr.41_13
	movq	%rax, %rdi	# stderr.41_13,
	call	fflush@PLT	#
# src/Runtime/runtime.c:68:         void **els = r->data;
	movq	-40(%rbp), %rax	# r, tmp126
	movq	8(%rax), %rax	# r_42(D)->data, tmp127
	movq	%rax, -24(%rbp)	# tmp127, els
# src/Runtime/runtime.c:69:         if (r->elementSize == sizeof(void *)) {
	movq	-40(%rbp), %rax	# r, tmp128
	movl	28(%rax), %eax	# r_42(D)->elementSize, _14
# src/Runtime/runtime.c:69:         if (r->elementSize == sizeof(void *)) {
	cmpl	$8, %eax	#, _14
	jne	.L9	#,
# src/Runtime/runtime.c:70:             for (int i = 0; i < r->length; i++)
	movl	$0, -4(%rbp)	#, i
# src/Runtime/runtime.c:70:             for (int i = 0; i < r->length; i++)
	jmp	.L10	#
.L11:
# src/Runtime/runtime.c:71:                 __decRef(els[i]);
	movl	-4(%rbp), %eax	# i, tmp129
	cltq
	leaq	0(,%rax,8), %rdx	#, _16
	movq	-24(%rbp), %rax	# els, tmp130
	addq	%rdx, %rax	# _16, _17
# src/Runtime/runtime.c:71:                 __decRef(els[i]);
	movq	(%rax), %rax	# *_17, _18
	movq	%rax, %rdi	# _18,
	call	__decRef	#
# src/Runtime/runtime.c:70:             for (int i = 0; i < r->length; i++)
	addl	$1, -4(%rbp)	#, i
.L10:
# src/Runtime/runtime.c:70:             for (int i = 0; i < r->length; i++)
	movq	-40(%rbp), %rax	# r, tmp131
	movl	32(%rax), %eax	# r_42(D)->length, _19
# src/Runtime/runtime.c:70:             for (int i = 0; i < r->length; i++)
	cmpl	%eax, -4(%rbp)	# _19, i
	jl	.L11	#,
.L9:
# src/Runtime/runtime.c:73:         if (els != NULL)
	cmpq	$0, -24(%rbp)	#, els
	je	.L12	#,
# src/Runtime/runtime.c:74:             free(els);
	movq	-24(%rbp), %rax	# els, tmp132
	movq	%rax, %rdi	# tmp132,
	call	free@PLT	#
	jmp	.L12	#
.L8:
# src/Runtime/runtime.c:75:     } else if (r->type == &_class_String) {
	movq	-40(%rbp), %rax	# r, tmp133
	movq	(%rax), %rdx	# r_42(D)->type, _20
# src/Runtime/runtime.c:75:     } else if (r->type == &_class_String) {
	leaq	_class_String(%rip), %rax	#, tmp134
	cmpq	%rax, %rdx	# tmp134, _20
	jne	.L13	#,
# src/Runtime/runtime.c:76:         DEBUG("Free got String");
	movq	stderr(%rip), %rax	# stderr, stderr.42_21
	movq	%rax, %rcx	# stderr.42_21,
	movl	$14, %edx	#,
	movl	$1, %esi	#,
	leaq	.LC0(%rip), %rax	#, tmp135
	movq	%rax, %rdi	# tmp135,
	call	fwrite@PLT	#
	movq	stderr(%rip), %rax	# stderr, stderr.43_22
	movq	%rax, %rcx	# stderr.43_22,
	movl	$15, %edx	#,
	movl	$1, %esi	#,
	leaq	.LC11(%rip), %rax	#, tmp136
	movq	%rax, %rdi	# tmp136,
	call	fwrite@PLT	#
	movq	stderr(%rip), %rax	# stderr, stderr.44_23
	movq	%rax, %rsi	# stderr.44_23,
	movl	$10, %edi	#,
	call	fputc@PLT	#
	movq	stderr(%rip), %rax	# stderr, stderr.45_24
	movq	%rax, %rdi	# stderr.45_24,
	call	fflush@PLT	#
# src/Runtime/runtime.c:77:         void *els = (r->data);
	movq	-40(%rbp), %rax	# r, tmp137
	movq	8(%rax), %rax	# r_42(D)->data, tmp138
	movq	%rax, -16(%rbp)	# tmp138, els
# src/Runtime/runtime.c:78:         if (els != NULL && els != emptyString) {
	cmpq	$0, -16(%rbp)	#, els
	je	.L12	#,
# src/Runtime/runtime.c:78:         if (els != NULL && els != emptyString) {
	leaq	emptyString(%rip), %rax	#, tmp139
	cmpq	%rax, -16(%rbp)	# tmp139, els
	je	.L12	#,
# src/Runtime/runtime.c:79:             DEBUG("Free underlying String data");
	movq	stderr(%rip), %rax	# stderr, stderr.46_25
	movq	%rax, %rcx	# stderr.46_25,
	movl	$14, %edx	#,
	movl	$1, %esi	#,
	leaq	.LC0(%rip), %rax	#, tmp140
	movq	%rax, %rdi	# tmp140,
	call	fwrite@PLT	#
	movq	stderr(%rip), %rax	# stderr, stderr.47_26
	movq	%rax, %rcx	# stderr.47_26,
	movl	$27, %edx	#,
	movl	$1, %esi	#,
	leaq	.LC12(%rip), %rax	#, tmp141
	movq	%rax, %rdi	# tmp141,
	call	fwrite@PLT	#
	movq	stderr(%rip), %rax	# stderr, stderr.48_27
	movq	%rax, %rsi	# stderr.48_27,
	movl	$10, %edi	#,
	call	fputc@PLT	#
	movq	stderr(%rip), %rax	# stderr, stderr.49_28
	movq	%rax, %rdi	# stderr.49_28,
	call	fflush@PLT	#
# src/Runtime/runtime.c:80:             free(els);
	movq	-16(%rbp), %rax	# els, tmp142
	movq	%rax, %rdi	# tmp142,
	call	free@PLT	#
	jmp	.L12	#
.L13:
# src/Runtime/runtime.c:82:     } else if (r->data != NULL) {
	movq	-40(%rbp), %rax	# r, tmp143
	movq	8(%rax), %rax	# r_42(D)->data, _29
# src/Runtime/runtime.c:82:     } else if (r->data != NULL) {
	testq	%rax, %rax	# _29
	je	.L12	#,
# src/Runtime/runtime.c:83:         DEBUG("Free got other");
	movq	stderr(%rip), %rax	# stderr, stderr.50_30
	movq	%rax, %rcx	# stderr.50_30,
	movl	$14, %edx	#,
	movl	$1, %esi	#,
	leaq	.LC0(%rip), %rax	#, tmp144
	movq	%rax, %rdi	# tmp144,
	call	fwrite@PLT	#
	movq	stderr(%rip), %rax	# stderr, stderr.51_31
	movq	%rax, %rcx	# stderr.51_31,
	movl	$14, %edx	#,
	movl	$1, %esi	#,
	leaq	.LC13(%rip), %rax	#, tmp145
	movq	%rax, %rdi	# tmp145,
	call	fwrite@PLT	#
	movq	stderr(%rip), %rax	# stderr, stderr.52_32
	movq	%rax, %rsi	# stderr.52_32,
	movl	$10, %edi	#,
	call	fputc@PLT	#
	movq	stderr(%rip), %rax	# stderr, stderr.53_33
	movq	%rax, %rdi	# stderr.53_33,
	call	fflush@PLT	#
# src/Runtime/runtime.c:84:         free(r->data);
	movq	-40(%rbp), %rax	# r, tmp146
	movq	8(%rax), %rax	# r_42(D)->data, _34
	movq	%rax, %rdi	# _34,
	call	free@PLT	#
.L12:
# src/Runtime/runtime.c:86:     free(r);
	movq	-40(%rbp), %rax	# r, tmp147
	movq	%rax, %rdi	# tmp147,
	call	free@PLT	#
.L4:
# src/Runtime/runtime.c:87: }
	leave	
	.cfi_def_cfa 7, 8
	ret	
	.cfi_endproc
.LFE7:
	.size	__free, .-__free
	.section	.rodata
.LC14:
	.string	"__incRef %p"
.LC15:
	.string	"__incRef end %p"
	.text
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
	subq	$16, %rsp	#,
	movq	%rdi, -8(%rbp)	# r, r
# src/Runtime/runtime.c:91:     DEBUG("__incRef %p", FORMAT_PTR(r));
	movq	stderr(%rip), %rax	# stderr, stderr.54_1
	movq	%rax, %rcx	# stderr.54_1,
	movl	$14, %edx	#,
	movl	$1, %esi	#,
	leaq	.LC0(%rip), %rax	#, tmp92
	movq	%rax, %rdi	# tmp92,
	call	fwrite@PLT	#
	movq	stderr(%rip), %rax	# stderr, stderr.55_2
	movq	-8(%rbp), %rdx	# r, tmp93
	leaq	.LC14(%rip), %rcx	#, tmp94
	movq	%rcx, %rsi	# tmp94,
	movq	%rax, %rdi	# stderr.55_2,
	movl	$0, %eax	#,
	call	fprintf@PLT	#
	movq	stderr(%rip), %rax	# stderr, stderr.56_3
	movq	%rax, %rsi	# stderr.56_3,
	movl	$10, %edi	#,
	call	fputc@PLT	#
	movq	stderr(%rip), %rax	# stderr, stderr.57_4
	movq	%rax, %rdi	# stderr.57_4,
	call	fflush@PLT	#
# src/Runtime/runtime.c:92:     if (!IS_NULL(r)) {
	leaq	_LAT_NULL(%rip), %rax	#, tmp95
	cmpq	%rax, -8(%rbp)	# tmp95, r
	je	.L15	#,
# src/Runtime/runtime.c:93:         r->counter++;
	movq	-8(%rbp), %rax	# r, tmp96
	movl	16(%rax), %eax	# r_14(D)->counter, _5
# src/Runtime/runtime.c:93:         r->counter++;
	leal	1(%rax), %edx	#, _6
	movq	-8(%rbp), %rax	# r, tmp97
	movl	%edx, 16(%rax)	# _6, r_14(D)->counter
.L15:
# src/Runtime/runtime.c:95:     DEBUG("__incRef end %p", FORMAT_PTR(r));
	movq	stderr(%rip), %rax	# stderr, stderr.58_7
	movq	%rax, %rcx	# stderr.58_7,
	movl	$14, %edx	#,
	movl	$1, %esi	#,
	leaq	.LC0(%rip), %rax	#, tmp98
	movq	%rax, %rdi	# tmp98,
	call	fwrite@PLT	#
	movq	stderr(%rip), %rax	# stderr, stderr.59_8
	movq	-8(%rbp), %rdx	# r, tmp99
	leaq	.LC15(%rip), %rcx	#, tmp100
	movq	%rcx, %rsi	# tmp100,
	movq	%rax, %rdi	# stderr.59_8,
	movl	$0, %eax	#,
	call	fprintf@PLT	#
	movq	stderr(%rip), %rax	# stderr, stderr.60_9
	movq	%rax, %rsi	# stderr.60_9,
	movl	$10, %edi	#,
	call	fputc@PLT	#
	movq	stderr(%rip), %rax	# stderr, stderr.61_10
	movq	%rax, %rdi	# stderr.61_10,
	call	fflush@PLT	#
# src/Runtime/runtime.c:96: }
	leave	
	.cfi_def_cfa 7, 8
	ret	
	.cfi_endproc
.LFE8:
	.size	__incRef, .-__incRef
	.section	.rodata
.LC16:
	.string	"__decRef %p"
.LC17:
	.string	"__decRef end %p"
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
# src/Runtime/runtime.c:98:     DEBUG("__decRef %p", FORMAT_PTR(r));
	movq	stderr(%rip), %rax	# stderr, stderr.62_1
	movq	%rax, %rcx	# stderr.62_1,
	movl	$14, %edx	#,
	movl	$1, %esi	#,
	leaq	.LC0(%rip), %rax	#, tmp93
	movq	%rax, %rdi	# tmp93,
	call	fwrite@PLT	#
	movq	stderr(%rip), %rax	# stderr, stderr.63_2
	movq	-8(%rbp), %rdx	# r, tmp94
	leaq	.LC16(%rip), %rcx	#, tmp95
	movq	%rcx, %rsi	# tmp95,
	movq	%rax, %rdi	# stderr.63_2,
	movl	$0, %eax	#,
	call	fprintf@PLT	#
	movq	stderr(%rip), %rax	# stderr, stderr.64_3
	movq	%rax, %rsi	# stderr.64_3,
	movl	$10, %edi	#,
	call	fputc@PLT	#
	movq	stderr(%rip), %rax	# stderr, stderr.65_4
	movq	%rax, %rdi	# stderr.65_4,
	call	fflush@PLT	#
# src/Runtime/runtime.c:100:     if (!IS_NULL(r)) {
	leaq	_LAT_NULL(%rip), %rax	#, tmp96
	cmpq	%rax, -8(%rbp)	# tmp96, r
	je	.L18	#,
# src/Runtime/runtime.c:101:         r->counter--;
	movq	-8(%rbp), %rax	# r, tmp97
	movl	16(%rax), %eax	# r_15(D)->counter, _5
# src/Runtime/runtime.c:101:         r->counter--;
	leal	-1(%rax), %edx	#, _6
	movq	-8(%rbp), %rax	# r, tmp98
	movl	%edx, 16(%rax)	# _6, r_15(D)->counter
# src/Runtime/runtime.c:102:         if (r->counter <=0) {
	movq	-8(%rbp), %rax	# r, tmp99
	movl	16(%rax), %eax	# r_15(D)->counter, _7
# src/Runtime/runtime.c:102:         if (r->counter <=0) {
	testl	%eax, %eax	# _7
	jg	.L18	#,
# src/Runtime/runtime.c:107:             __free(r);
	movq	-8(%rbp), %rax	# r, tmp100
	movq	%rax, %rdi	# tmp100,
	call	__free	#
.L18:
# src/Runtime/runtime.c:110:     DEBUG("__decRef end %p", FORMAT_PTR(r));
	movq	stderr(%rip), %rax	# stderr, stderr.66_8
	movq	%rax, %rcx	# stderr.66_8,
	movl	$14, %edx	#,
	movl	$1, %esi	#,
	leaq	.LC0(%rip), %rax	#, tmp101
	movq	%rax, %rdi	# tmp101,
	call	fwrite@PLT	#
	movq	stderr(%rip), %rax	# stderr, stderr.67_9
	movq	-8(%rbp), %rdx	# r, tmp102
	leaq	.LC17(%rip), %rcx	#, tmp103
	movq	%rcx, %rsi	# tmp103,
	movq	%rax, %rdi	# stderr.67_9,
	movl	$0, %eax	#,
	call	fprintf@PLT	#
	movq	stderr(%rip), %rax	# stderr, stderr.68_10
	movq	%rax, %rsi	# stderr.68_10,
	movl	$10, %edi	#,
	call	fputc@PLT	#
	movq	stderr(%rip), %rax	# stderr, stderr.69_11
	movq	%rax, %rdi	# stderr.69_11,
	call	fflush@PLT	#
# src/Runtime/runtime.c:111: }
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
# src/Runtime/runtime.c:113: obj __newRefArray(int32_t length) { return __newArray(sizeof(obj), length); }
	movl	-4(%rbp), %eax	# length, tmp84
	movl	%eax, %esi	# tmp84,
	movl	$8, %edi	#,
	call	__newArray	#
# src/Runtime/runtime.c:113: obj __newRefArray(int32_t length) { return __newArray(sizeof(obj), length); }
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
# src/Runtime/runtime.c:115:     return __newArray(sizeof(int32_t), length);
	movl	-4(%rbp), %eax	# length, tmp84
	movl	%eax, %esi	# tmp84,
	movl	$4, %edi	#,
	call	__newArray	#
# src/Runtime/runtime.c:116: }
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
# src/Runtime/runtime.c:118:     return __newArray(sizeof(int8_t), length);
	movl	-4(%rbp), %eax	# length, tmp84
	movl	%eax, %esi	# tmp84,
	movl	$1, %edi	#,
	call	__newArray	#
# src/Runtime/runtime.c:119: }
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
# src/Runtime/runtime.c:121:     obj r = __new(&_class_Array);
	leaq	_class_Array(%rip), %rax	#, tmp88
	movq	%rax, %rdi	# tmp88,
	call	__new	#
	movq	%rax, -8(%rbp)	# tmp89, r
# src/Runtime/runtime.c:122:     void* arr = malloc(size * length);
	movl	-20(%rbp), %eax	# size, tmp90
	imull	-24(%rbp), %eax	# length, _1
# src/Runtime/runtime.c:122:     void* arr = malloc(size * length);
	cltq
	movq	%rax, %rdi	# _2,
	call	malloc@PLT	#
	movq	%rax, -16(%rbp)	# tmp91, arr
# src/Runtime/runtime.c:123:     r->data = arr;
	movq	-8(%rbp), %rax	# r, tmp92
	movq	-16(%rbp), %rdx	# arr, tmp93
	movq	%rdx, 8(%rax)	# tmp93, r_8->data
# src/Runtime/runtime.c:124:     r->elementSize = size;
	movq	-8(%rbp), %rax	# r, tmp94
	movl	-20(%rbp), %edx	# size, tmp95
	movl	%edx, 28(%rax)	# tmp95, r_8->elementSize
# src/Runtime/runtime.c:125:     r->length = length;
	movq	-8(%rbp), %rax	# r, tmp96
	movl	-24(%rbp), %edx	# length, tmp97
	movl	%edx, 32(%rax)	# tmp97, r_8->length
# src/Runtime/runtime.c:126:     if (length > 0) {
	cmpl	$0, -24(%rbp)	#, length
	jle	.L27	#,
# src/Runtime/runtime.c:128:         bzero(arr, size * length);
	movl	-20(%rbp), %eax	# size, tmp98
	imull	-24(%rbp), %eax	# length, _3
# src/Runtime/runtime.c:128:         bzero(arr, size * length);
	cltq
	movq	-16(%rbp), %rdx	# arr, tmp99
	movq	%rdx, %rcx	# tmp99, tmp100
	movq	%rax, %rdx	# tmp101,
	movl	$0, %esi	#,
	movq	%rcx, %rdi	# tmp100,
	call	memset@PLT	#
.L27:
# src/Runtime/runtime.c:130:     return r;
	movq	-8(%rbp), %rax	# r, _17
# src/Runtime/runtime.c:131: }
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
# src/Runtime/runtime.c:145:     return NULL;
	movl	$0, %eax	#, _1
# src/Runtime/runtime.c:146: }
	popq	%rbp	#
	.cfi_def_cfa 7, 8
	ret	
	.cfi_endproc
.LFE14:
	.size	__getelementptr, .-__getelementptr
	.section	.rodata
.LC18:
	.string	"__cast"
.LC19:
	.string	"__cast %p %p [%d]"
.LC20:
	.string	"__cast object is null"
.LC21:
	.string	"__cast get underlying type"
	.align 8
.LC22:
	.string	"__cast iterate parent upward %p [%d]"
	.align 8
.LC23:
	.string	"__cast found correct parent: %p"
.LC24:
	.string	"__cast loop, break %p"
.LC25:
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
# src/Runtime/runtime.c:149:     DEBUG("__cast");
	movq	stderr(%rip), %rax	# stderr, stderr.70_1
	movq	%rax, %rcx	# stderr.70_1,
	movl	$14, %edx	#,
	movl	$1, %esi	#,
	leaq	.LC0(%rip), %rax	#, tmp118
	movq	%rax, %rdi	# tmp118,
	call	fwrite@PLT	#
	movq	stderr(%rip), %rax	# stderr, stderr.71_2
	movq	%rax, %rcx	# stderr.71_2,
	movl	$6, %edx	#,
	movl	$1, %esi	#,
	leaq	.LC18(%rip), %rax	#, tmp119
	movq	%rax, %rdi	# tmp119,
	call	fwrite@PLT	#
	movq	stderr(%rip), %rax	# stderr, stderr.72_3
	movq	%rax, %rsi	# stderr.72_3,
	movl	$10, %edi	#,
	call	fputc@PLT	#
	movq	stderr(%rip), %rax	# stderr, stderr.73_4
	movq	%rax, %rdi	# stderr.73_4,
	call	fflush@PLT	#
# src/Runtime/runtime.c:150:     DEBUG("__cast %p %p [%d]", FORMAT_PTR(o), FORMAT_PTR(t), t->dataSize);
	movq	stderr(%rip), %rax	# stderr, stderr.74_5
	movq	%rax, %rcx	# stderr.74_5,
	movl	$14, %edx	#,
	movl	$1, %esi	#,
	leaq	.LC0(%rip), %rax	#, tmp120
	movq	%rax, %rdi	# tmp120,
	call	fwrite@PLT	#
	movq	-32(%rbp), %rax	# t, tmp121
	movl	16(%rax), %esi	# t_46(D)->dataSize, _6
	movq	stderr(%rip), %rax	# stderr, stderr.75_7
	movq	-32(%rbp), %rcx	# t, tmp122
	movq	-24(%rbp), %rdx	# o, tmp123
	movl	%esi, %r8d	# _6,
	leaq	.LC19(%rip), %rsi	#, tmp124
	movq	%rax, %rdi	# stderr.75_7,
	movl	$0, %eax	#,
	call	fprintf@PLT	#
	movq	stderr(%rip), %rax	# stderr, stderr.76_8
	movq	%rax, %rsi	# stderr.76_8,
	movl	$10, %edi	#,
	call	fputc@PLT	#
	movq	stderr(%rip), %rax	# stderr, stderr.77_9
	movq	%rax, %rdi	# stderr.77_9,
	call	fflush@PLT	#
# src/Runtime/runtime.c:151:     if (IS_NULL(o)) {
	leaq	_LAT_NULL(%rip), %rax	#, tmp125
	cmpq	%rax, -24(%rbp)	# tmp125, o
	jne	.L32	#,
# src/Runtime/runtime.c:152:         DEBUG("__cast object is null");
	movq	stderr(%rip), %rax	# stderr, stderr.78_10
	movq	%rax, %rcx	# stderr.78_10,
	movl	$14, %edx	#,
	movl	$1, %esi	#,
	leaq	.LC0(%rip), %rax	#, tmp126
	movq	%rax, %rdi	# tmp126,
	call	fwrite@PLT	#
	movq	stderr(%rip), %rax	# stderr, stderr.79_11
	movq	%rax, %rcx	# stderr.79_11,
	movl	$21, %edx	#,
	movl	$1, %esi	#,
	leaq	.LC20(%rip), %rax	#, tmp127
	movq	%rax, %rdi	# tmp127,
	call	fwrite@PLT	#
	movq	stderr(%rip), %rax	# stderr, stderr.80_12
	movq	%rax, %rsi	# stderr.80_12,
	movl	$10, %edi	#,
	call	fputc@PLT	#
	movq	stderr(%rip), %rax	# stderr, stderr.81_13
	movq	%rax, %rdi	# stderr.81_13,
	call	fflush@PLT	#
# src/Runtime/runtime.c:153:         return VAL_NULL;
	leaq	_LAT_NULL(%rip), %rax	#, _36
	jmp	.L33	#
.L32:
# src/Runtime/runtime.c:155:     DEBUG("__cast get underlying type");
	movq	stderr(%rip), %rax	# stderr, stderr.82_14
	movq	%rax, %rcx	# stderr.82_14,
	movl	$14, %edx	#,
	movl	$1, %esi	#,
	leaq	.LC0(%rip), %rax	#, tmp128
	movq	%rax, %rdi	# tmp128,
	call	fwrite@PLT	#
	movq	stderr(%rip), %rax	# stderr, stderr.83_15
	movq	%rax, %rcx	# stderr.83_15,
	movl	$26, %edx	#,
	movl	$1, %esi	#,
	leaq	.LC21(%rip), %rax	#, tmp129
	movq	%rax, %rdi	# tmp129,
	call	fwrite@PLT	#
	movq	stderr(%rip), %rax	# stderr, stderr.84_16
	movq	%rax, %rsi	# stderr.84_16,
	movl	$10, %edi	#,
	call	fputc@PLT	#
	movq	stderr(%rip), %rax	# stderr, stderr.85_17
	movq	%rax, %rdi	# stderr.85_17,
	call	fflush@PLT	#
# src/Runtime/runtime.c:156:     struct Type *to = o->type;
	movq	-24(%rbp), %rax	# o, tmp130
	movq	(%rax), %rax	# o_47(D)->type, tmp131
	movq	%rax, -8(%rbp)	# tmp131, to
# src/Runtime/runtime.c:157:     while (to != NULL) {
	jmp	.L34	#
.L37:
# src/Runtime/runtime.c:158:         DEBUG("__cast iterate parent upward %p [%d]", FORMAT_PTR(to), to->dataSize);
	movq	stderr(%rip), %rax	# stderr, stderr.86_18
	movq	%rax, %rcx	# stderr.86_18,
	movl	$14, %edx	#,
	movl	$1, %esi	#,
	leaq	.LC0(%rip), %rax	#, tmp132
	movq	%rax, %rdi	# tmp132,
	call	fwrite@PLT	#
	movq	-8(%rbp), %rax	# to, tmp133
	movl	16(%rax), %ecx	# to_35->dataSize, _19
	movq	stderr(%rip), %rax	# stderr, stderr.87_20
	movq	-8(%rbp), %rdx	# to, tmp134
	leaq	.LC22(%rip), %rsi	#, tmp135
	movq	%rax, %rdi	# stderr.87_20,
	movl	$0, %eax	#,
	call	fprintf@PLT	#
	movq	stderr(%rip), %rax	# stderr, stderr.88_21
	movq	%rax, %rsi	# stderr.88_21,
	movl	$10, %edi	#,
	call	fputc@PLT	#
	movq	stderr(%rip), %rax	# stderr, stderr.89_22
	movq	%rax, %rdi	# stderr.89_22,
	call	fflush@PLT	#
# src/Runtime/runtime.c:159:         if (t == to) {
	movq	-32(%rbp), %rax	# t, tmp136
	cmpq	-8(%rbp), %rax	# to, tmp136
	jne	.L35	#,
# src/Runtime/runtime.c:160:             DEBUG("__cast found correct parent: %p", FORMAT_PTR(to));
	movq	stderr(%rip), %rax	# stderr, stderr.90_23
	movq	%rax, %rcx	# stderr.90_23,
	movl	$14, %edx	#,
	movl	$1, %esi	#,
	leaq	.LC0(%rip), %rax	#, tmp137
	movq	%rax, %rdi	# tmp137,
	call	fwrite@PLT	#
	movq	stderr(%rip), %rax	# stderr, stderr.91_24
	movq	-8(%rbp), %rdx	# to, tmp138
	leaq	.LC23(%rip), %rcx	#, tmp139
	movq	%rcx, %rsi	# tmp139,
	movq	%rax, %rdi	# stderr.91_24,
	movl	$0, %eax	#,
	call	fprintf@PLT	#
	movq	stderr(%rip), %rax	# stderr, stderr.92_25
	movq	%rax, %rsi	# stderr.92_25,
	movl	$10, %edi	#,
	call	fputc@PLT	#
	movq	stderr(%rip), %rax	# stderr, stderr.93_26
	movq	%rax, %rdi	# stderr.93_26,
	call	fflush@PLT	#
# src/Runtime/runtime.c:161:             return o;
	movq	-24(%rbp), %rax	# o, _36
	jmp	.L33	#
.L35:
# src/Runtime/runtime.c:163:         struct Type *prev = to;
	movq	-8(%rbp), %rax	# to, tmp140
	movq	%rax, -16(%rbp)	# tmp140, prev
# src/Runtime/runtime.c:164:         to = to->parent;
	movq	-8(%rbp), %rax	# to, tmp141
	movq	(%rax), %rax	# to_35->parent, tmp142
	movq	%rax, -8(%rbp)	# tmp142, to
# src/Runtime/runtime.c:165:         if (prev == to) {
	movq	-16(%rbp), %rax	# prev, tmp143
	cmpq	-8(%rbp), %rax	# to, tmp143
	jne	.L34	#,
# src/Runtime/runtime.c:166:             DEBUG("__cast loop, break %p", FORMAT_PTR(to));
	movq	stderr(%rip), %rax	# stderr, stderr.94_27
	movq	%rax, %rcx	# stderr.94_27,
	movl	$14, %edx	#,
	movl	$1, %esi	#,
	leaq	.LC0(%rip), %rax	#, tmp144
	movq	%rax, %rdi	# tmp144,
	call	fwrite@PLT	#
	movq	stderr(%rip), %rax	# stderr, stderr.95_28
	movq	-8(%rbp), %rdx	# to, tmp145
	leaq	.LC24(%rip), %rcx	#, tmp146
	movq	%rcx, %rsi	# tmp146,
	movq	%rax, %rdi	# stderr.95_28,
	movl	$0, %eax	#,
	call	fprintf@PLT	#
	movq	stderr(%rip), %rax	# stderr, stderr.96_29
	movq	%rax, %rsi	# stderr.96_29,
	movl	$10, %edi	#,
	call	fputc@PLT	#
	movq	stderr(%rip), %rax	# stderr, stderr.97_30
	movq	%rax, %rdi	# stderr.97_30,
	call	fflush@PLT	#
# src/Runtime/runtime.c:167:             break;
	jmp	.L38	#
.L34:
# src/Runtime/runtime.c:157:     while (to != NULL) {
	cmpq	$0, -8(%rbp)	#, to
	jne	.L37	#,
.L38:
# src/Runtime/runtime.c:170:     DEBUG("finished the cast (FAILED)");
	movq	stderr(%rip), %rax	# stderr, stderr.98_31
	movq	%rax, %rcx	# stderr.98_31,
	movl	$14, %edx	#,
	movl	$1, %esi	#,
	leaq	.LC0(%rip), %rax	#, tmp147
	movq	%rax, %rdi	# tmp147,
	call	fwrite@PLT	#
	movq	stderr(%rip), %rax	# stderr, stderr.99_32
	movq	%rax, %rcx	# stderr.99_32,
	movl	$26, %edx	#,
	movl	$1, %esi	#,
	leaq	.LC25(%rip), %rax	#, tmp148
	movq	%rax, %rdi	# tmp148,
	call	fwrite@PLT	#
	movq	stderr(%rip), %rax	# stderr, stderr.100_33
	movq	%rax, %rsi	# stderr.100_33,
	movl	$10, %edi	#,
	call	fputc@PLT	#
	movq	stderr(%rip), %rax	# stderr, stderr.101_34
	movq	%rax, %rdi	# stderr.101_34,
	call	fflush@PLT	#
# src/Runtime/runtime.c:171:     return VAL_NULL;
	leaq	_LAT_NULL(%rip), %rax	#, _36
.L33:
# src/Runtime/runtime.c:172: }
	leave	
	.cfi_def_cfa 7, 8
	ret	
	.cfi_endproc
.LFE15:
	.size	__cast, .-__cast
	.section	.rodata
	.align 8
.LC26:
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
# src/Runtime/runtime.c:175:     errMsg = "ERROR: Null pointer reference.";
	leaq	.LC26(%rip), %rax	#, tmp82
	movq	%rax, errMsg(%rip)	# tmp82, errMsg
# src/Runtime/runtime.c:176:     error();
	movl	$0, %eax	#,
	call	error	#
# src/Runtime/runtime.c:177: }
	nop	
	popq	%rbp	#
	.cfi_def_cfa 7, 8
	ret	
	.cfi_endproc
.LFE16:
	.size	__errorNull, .-__errorNull
	.section	.rodata
.LC27:
	.string	"Call __createString() v2"
.LC28:
	.string	"C is NULL exit"
.LC29:
	.string	"Perform new on _class_String"
.LC30:
	.string	"String allocated"
.LC31:
	.string	"Measure strlen"
.LC32:
	.string	"Check unicode, len=%d"
	.align 8
.LC33:
	.string	"Non-unicode string encoding at byte '%s' #%d"
	.align 8
.LC34:
	.string	"ERROR: Non-unicode string encoding."
	.align 8
.LC35:
	.string	"Str init completed %p (with data=%p, size=%d, str='%s')"
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
# src/Runtime/runtime.c:180:     DEBUG("Call __createString() v2");
	movq	stderr(%rip), %rax	# stderr, stderr.102_1
	movq	%rax, %rcx	# stderr.102_1,
	movl	$14, %edx	#,
	movl	$1, %esi	#,
	leaq	.LC0(%rip), %rax	#, tmp137
	movq	%rax, %rdi	# tmp137,
	call	fwrite@PLT	#
	movq	stderr(%rip), %rax	# stderr, stderr.103_2
	movq	%rax, %rcx	# stderr.103_2,
	movl	$24, %edx	#,
	movl	$1, %esi	#,
	leaq	.LC27(%rip), %rax	#, tmp138
	movq	%rax, %rdi	# tmp138,
	call	fwrite@PLT	#
	movq	stderr(%rip), %rax	# stderr, stderr.104_3
	movq	%rax, %rsi	# stderr.104_3,
	movl	$10, %edi	#,
	call	fputc@PLT	#
	movq	stderr(%rip), %rax	# stderr, stderr.105_4
	movq	%rax, %rdi	# stderr.105_4,
	call	fflush@PLT	#
# src/Runtime/runtime.c:182:     if (IS_NULL(c)) {
	leaq	_LAT_NULL(%rip), %rax	#, tmp139
	cmpq	%rax, -40(%rbp)	# tmp139, c
	jne	.L41	#,
# src/Runtime/runtime.c:183:         DEBUG("C is NULL exit");
	movq	stderr(%rip), %rax	# stderr, stderr.106_5
	movq	%rax, %rcx	# stderr.106_5,
	movl	$14, %edx	#,
	movl	$1, %esi	#,
	leaq	.LC0(%rip), %rax	#, tmp140
	movq	%rax, %rdi	# tmp140,
	call	fwrite@PLT	#
	movq	stderr(%rip), %rax	# stderr, stderr.107_6
	movq	%rax, %rcx	# stderr.107_6,
	movl	$14, %edx	#,
	movl	$1, %esi	#,
	leaq	.LC28(%rip), %rax	#, tmp141
	movq	%rax, %rdi	# tmp141,
	call	fwrite@PLT	#
	movq	stderr(%rip), %rax	# stderr, stderr.108_7
	movq	%rax, %rsi	# stderr.108_7,
	movl	$10, %edi	#,
	call	fputc@PLT	#
	movq	stderr(%rip), %rax	# stderr, stderr.109_8
	movq	%rax, %rdi	# stderr.109_8,
	call	fflush@PLT	#
# src/Runtime/runtime.c:184:         return __createString(emptyString);
	leaq	emptyString(%rip), %rax	#, tmp142
	movq	%rax, %rdi	# tmp142,
	call	__createString	#
	jmp	.L42	#
.L41:
# src/Runtime/runtime.c:186:     DEBUG("Perform new on _class_String");
	movq	stderr(%rip), %rax	# stderr, stderr.110_9
	movq	%rax, %rcx	# stderr.110_9,
	movl	$14, %edx	#,
	movl	$1, %esi	#,
	leaq	.LC0(%rip), %rax	#, tmp143
	movq	%rax, %rdi	# tmp143,
	call	fwrite@PLT	#
	movq	stderr(%rip), %rax	# stderr, stderr.111_10
	movq	%rax, %rcx	# stderr.111_10,
	movl	$28, %edx	#,
	movl	$1, %esi	#,
	leaq	.LC29(%rip), %rax	#, tmp144
	movq	%rax, %rdi	# tmp144,
	call	fwrite@PLT	#
	movq	stderr(%rip), %rax	# stderr, stderr.112_11
	movq	%rax, %rsi	# stderr.112_11,
	movl	$10, %edi	#,
	call	fputc@PLT	#
	movq	stderr(%rip), %rax	# stderr, stderr.113_12
	movq	%rax, %rdi	# stderr.113_12,
	call	fflush@PLT	#
# src/Runtime/runtime.c:187:     obj r = __new(&_class_String);
	leaq	_class_String(%rip), %rax	#, tmp145
	movq	%rax, %rdi	# tmp145,
	call	__new	#
	movq	%rax, -8(%rbp)	# tmp146, r
# src/Runtime/runtime.c:188:     DEBUG("String allocated");
	movq	stderr(%rip), %rax	# stderr, stderr.114_13
	movq	%rax, %rcx	# stderr.114_13,
	movl	$14, %edx	#,
	movl	$1, %esi	#,
	leaq	.LC0(%rip), %rax	#, tmp147
	movq	%rax, %rdi	# tmp147,
	call	fwrite@PLT	#
	movq	stderr(%rip), %rax	# stderr, stderr.115_14
	movq	%rax, %rcx	# stderr.115_14,
	movl	$16, %edx	#,
	movl	$1, %esi	#,
	leaq	.LC30(%rip), %rax	#, tmp148
	movq	%rax, %rdi	# tmp148,
	call	fwrite@PLT	#
	movq	stderr(%rip), %rax	# stderr, stderr.116_15
	movq	%rax, %rsi	# stderr.116_15,
	movl	$10, %edi	#,
	call	fputc@PLT	#
	movq	stderr(%rip), %rax	# stderr, stderr.117_16
	movq	%rax, %rdi	# stderr.117_16,
	call	fflush@PLT	#
# src/Runtime/runtime.c:189:     DEBUG("Measure strlen");
	movq	stderr(%rip), %rax	# stderr, stderr.118_17
	movq	%rax, %rcx	# stderr.118_17,
	movl	$14, %edx	#,
	movl	$1, %esi	#,
	leaq	.LC0(%rip), %rax	#, tmp149
	movq	%rax, %rdi	# tmp149,
	call	fwrite@PLT	#
	movq	stderr(%rip), %rax	# stderr, stderr.119_18
	movq	%rax, %rcx	# stderr.119_18,
	movl	$14, %edx	#,
	movl	$1, %esi	#,
	leaq	.LC31(%rip), %rax	#, tmp150
	movq	%rax, %rdi	# tmp150,
	call	fwrite@PLT	#
	movq	stderr(%rip), %rax	# stderr, stderr.120_19
	movq	%rax, %rsi	# stderr.120_19,
	movl	$10, %edi	#,
	call	fputc@PLT	#
	movq	stderr(%rip), %rax	# stderr, stderr.121_20
	movq	%rax, %rdi	# stderr.121_20,
	call	fflush@PLT	#
# src/Runtime/runtime.c:190:     r->length = u8_strlen(c);
	movq	-40(%rbp), %rax	# c, tmp151
	movq	%rax, %rdi	# tmp151,
	call	u8_strlen@PLT	#
# src/Runtime/runtime.c:190:     r->length = u8_strlen(c);
	movl	%eax, %edx	# _21, _22
	movq	-8(%rbp), %rax	# r, tmp152
	movl	%edx, 32(%rax)	# _22, r_69->length
# src/Runtime/runtime.c:191:     DEBUG("Check unicode, len=%d", r->length);
	movq	stderr(%rip), %rax	# stderr, stderr.122_23
	movq	%rax, %rcx	# stderr.122_23,
	movl	$14, %edx	#,
	movl	$1, %esi	#,
	leaq	.LC0(%rip), %rax	#, tmp153
	movq	%rax, %rdi	# tmp153,
	call	fwrite@PLT	#
	movq	-8(%rbp), %rax	# r, tmp154
	movl	32(%rax), %edx	# r_69->length, _24
	movq	stderr(%rip), %rax	# stderr, stderr.123_25
	leaq	.LC32(%rip), %rcx	#, tmp155
	movq	%rcx, %rsi	# tmp155,
	movq	%rax, %rdi	# stderr.123_25,
	movl	$0, %eax	#,
	call	fprintf@PLT	#
	movq	stderr(%rip), %rax	# stderr, stderr.124_26
	movq	%rax, %rsi	# stderr.124_26,
	movl	$10, %edi	#,
	call	fputc@PLT	#
	movq	stderr(%rip), %rax	# stderr, stderr.125_27
	movq	%rax, %rdi	# stderr.125_27,
	call	fflush@PLT	#
# src/Runtime/runtime.c:192:     uint8_t* invalid_unit = u8_check(c, r->length);
	movq	-8(%rbp), %rax	# r, tmp156
	movl	32(%rax), %eax	# r_69->length, _28
# src/Runtime/runtime.c:192:     uint8_t* invalid_unit = u8_check(c, r->length);
	movslq	%eax, %rdx	# _28, _29
	movq	-40(%rbp), %rax	# c, tmp157
	movq	%rdx, %rsi	# _29,
	movq	%rax, %rdi	# tmp157,
	call	u8_check@PLT	#
	movq	%rax, -16(%rbp)	# tmp158, invalid_unit
# src/Runtime/runtime.c:193:     if (u8_check(c, r->length) != NULL) {
	movq	-8(%rbp), %rax	# r, tmp159
	movl	32(%rax), %eax	# r_69->length, _30
# src/Runtime/runtime.c:193:     if (u8_check(c, r->length) != NULL) {
	movslq	%eax, %rdx	# _30, _31
	movq	-40(%rbp), %rax	# c, tmp160
	movq	%rdx, %rsi	# _31,
	movq	%rax, %rdi	# tmp160,
	call	u8_check@PLT	#
# src/Runtime/runtime.c:193:     if (u8_check(c, r->length) != NULL) {
	testq	%rax, %rax	# _32
	je	.L43	#,
# src/Runtime/runtime.c:194:         DEBUG("Non-unicode string encoding at byte '%s' #%d", c, (int)(((uint64_t)invalid_unit)-((uint64_t)c)));
	movq	stderr(%rip), %rax	# stderr, stderr.126_33
	movq	%rax, %rcx	# stderr.126_33,
	movl	$14, %edx	#,
	movl	$1, %esi	#,
	leaq	.LC0(%rip), %rax	#, tmp161
	movq	%rax, %rdi	# tmp161,
	call	fwrite@PLT	#
	movq	-16(%rbp), %rdx	# invalid_unit, invalid_unit.127_34
	movq	-40(%rbp), %rax	# c, c.128_35
	subq	%rax, %rdx	# c.128_35, _36
	movl	%edx, %ecx	# _36, _37
	movq	stderr(%rip), %rax	# stderr, stderr.129_38
	movq	-40(%rbp), %rdx	# c, tmp162
	leaq	.LC33(%rip), %rsi	#, tmp163
	movq	%rax, %rdi	# stderr.129_38,
	movl	$0, %eax	#,
	call	fprintf@PLT	#
	movq	stderr(%rip), %rax	# stderr, stderr.130_39
	movq	%rax, %rsi	# stderr.130_39,
	movl	$10, %edi	#,
	call	fputc@PLT	#
	movq	stderr(%rip), %rax	# stderr, stderr.131_40
	movq	%rax, %rdi	# stderr.131_40,
	call	fflush@PLT	#
# src/Runtime/runtime.c:195:         errMsg = "ERROR: Non-unicode string encoding.";
	leaq	.LC34(%rip), %rax	#, tmp164
	movq	%rax, errMsg(%rip)	# tmp164, errMsg
# src/Runtime/runtime.c:196:         error();
	movl	$0, %eax	#,
	call	error	#
.L43:
# src/Runtime/runtime.c:198:     if (r->length > 0) {
	movq	-8(%rbp), %rax	# r, tmp165
	movl	32(%rax), %eax	# r_69->length, _41
# src/Runtime/runtime.c:198:     if (r->length > 0) {
	testl	%eax, %eax	# _41
	jle	.L44	#,
# src/Runtime/runtime.c:199:         int len = r->length;
	movq	-8(%rbp), %rax	# r, tmp166
	movl	32(%rax), %eax	# r_69->length, tmp167
	movl	%eax, -20(%rbp)	# tmp167, len
# src/Runtime/runtime.c:200:         uint8_t *str = malloc(len + 1);
	movl	-20(%rbp), %eax	# len, tmp168
	addl	$1, %eax	#, _42
# src/Runtime/runtime.c:200:         uint8_t *str = malloc(len + 1);
	cltq
	movq	%rax, %rdi	# _43,
	call	malloc@PLT	#
	movq	%rax, -32(%rbp)	# tmp169, str
# src/Runtime/runtime.c:201:         r->data = str;
	movq	-8(%rbp), %rax	# r, tmp170
	movq	-32(%rbp), %rdx	# str, tmp171
	movq	%rdx, 8(%rax)	# tmp171, r_69->data
# src/Runtime/runtime.c:202:         memcpy(str, c, len);
	movl	-20(%rbp), %eax	# len, tmp172
	movslq	%eax, %rdx	# tmp172, _44
	movq	-40(%rbp), %rcx	# c, tmp173
	movq	-32(%rbp), %rax	# str, tmp174
	movq	%rcx, %rsi	# tmp173,
	movq	%rax, %rdi	# tmp174,
	call	memcpy@PLT	#
# src/Runtime/runtime.c:203:         str[len] = 0;
	movl	-20(%rbp), %eax	# len, tmp175
	movslq	%eax, %rdx	# tmp175, _45
	movq	-32(%rbp), %rax	# str, tmp176
	addq	%rdx, %rax	# _45, _46
# src/Runtime/runtime.c:203:         str[len] = 0;
	movb	$0, (%rax)	#, *_46
	jmp	.L45	#
.L44:
# src/Runtime/runtime.c:205:         r->data = emptyString;
	movq	-8(%rbp), %rax	# r, tmp177
	leaq	emptyString(%rip), %rdx	#, tmp178
	movq	%rdx, 8(%rax)	# tmp178, r_69->data
.L45:
# src/Runtime/runtime.c:207:     DEBUG("Str init completed %p (with data=%p, size=%d, str='%s')", FORMAT_PTR(r), FORMAT_PTR(r->data), r->length, r->data);
	movq	stderr(%rip), %rax	# stderr, stderr.132_47
	movq	%rax, %rcx	# stderr.132_47,
	movl	$14, %edx	#,
	movl	$1, %esi	#,
	leaq	.LC0(%rip), %rax	#, tmp179
	movq	%rax, %rdi	# tmp179,
	call	fwrite@PLT	#
	movq	-8(%rbp), %rax	# r, tmp180
	movq	8(%rax), %rdi	# r_69->data, _48
	movq	-8(%rbp), %rax	# r, tmp181
	movl	32(%rax), %esi	# r_69->length, _49
	movq	-8(%rbp), %rax	# r, tmp182
	movq	8(%rax), %rcx	# r_69->data, _50
	movq	stderr(%rip), %rax	# stderr, stderr.133_51
	movq	-8(%rbp), %rdx	# r, tmp183
	movq	%rdi, %r9	# _48,
	movl	%esi, %r8d	# _49,
	leaq	.LC35(%rip), %rsi	#, tmp184
	movq	%rax, %rdi	# stderr.133_51,
	movl	$0, %eax	#,
	call	fprintf@PLT	#
	movq	stderr(%rip), %rax	# stderr, stderr.134_52
	movq	%rax, %rsi	# stderr.134_52,
	movl	$10, %edi	#,
	call	fputc@PLT	#
	movq	stderr(%rip), %rax	# stderr, stderr.135_53
	movq	%rax, %rdi	# stderr.135_53,
	call	fflush@PLT	#
# src/Runtime/runtime.c:208:     return r;
	movq	-8(%rbp), %rax	# r, _54
.L42:
# src/Runtime/runtime.c:209: }
	leave	
	.cfi_def_cfa 7, 8
	ret	
	.cfi_endproc
.LFE17:
	.size	__createString, .-__createString
	.section	.rodata
.LC36:
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
# src/Runtime/runtime.c:213:     obj ret = __createString("Object");
	leaq	.LC36(%rip), %rax	#, tmp84
	movq	%rax, %rdi	# tmp84,
	call	__createString	#
	movq	%rax, -8(%rbp)	# tmp85, ret
# src/Runtime/runtime.c:214:     __incRef(ret);
	movq	-8(%rbp), %rax	# ret, tmp86
	movq	%rax, %rdi	# tmp86,
	call	__incRef	#
# src/Runtime/runtime.c:215:     return ret;
	movq	-8(%rbp), %rax	# ret, _5
# src/Runtime/runtime.c:216: }
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
# src/Runtime/runtime.c:217: int32_t _Object_getHashCode(obj o) { return (int32_t)(int64_t)o; }
	movq	-8(%rbp), %rax	# o, o.136_1
# src/Runtime/runtime.c:217: int32_t _Object_getHashCode(obj o) { return (int32_t)(int64_t)o; }
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
# src/Runtime/runtime.c:218: int8_t _Object_equals(obj o1, obj o2) { return o1 == o2; }
	movq	-8(%rbp), %rax	# o1, tmp85
	cmpq	-16(%rbp), %rax	# o2, tmp85
	sete	%al	#, _1
# src/Runtime/runtime.c:218: int8_t _Object_equals(obj o1, obj o2) { return o1 == o2; }
	popq	%rbp	#
	.cfi_def_cfa 7, 8
	ret	
	.cfi_endproc
.LFE20:
	.size	_Object_equals, .-_Object_equals
	.section	.rodata
.LC37:
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
# src/Runtime/runtime.c:221:     char start[] = "[";
	movw	$91, -114(%rbp)	#, start
# src/Runtime/runtime.c:222:     char delim[] = ", ";
	movw	$8236, -117(%rbp)	#, delim
	movb	$0, -115(%rbp)	#, delim
# src/Runtime/runtime.c:223:     char end[] = "]";
	movw	$93, -119(%rbp)	#, end
# src/Runtime/runtime.c:225:     obj *strings = malloc(sizeof(obj) * arr->length);
	movq	-136(%rbp), %rax	# arr, tmp186
	movl	32(%rax), %eax	# arr_116(D)->length, _1
	cltq
# src/Runtime/runtime.c:225:     obj *strings = malloc(sizeof(obj) * arr->length);
	salq	$3, %rax	#, _3
	movq	%rax, %rdi	# _3,
	call	malloc@PLT	#
	movq	%rax, -40(%rbp)	# tmp187, strings
# src/Runtime/runtime.c:226:     int32_t *lenghts = malloc(sizeof(int32_t) * arr->length);
	movq	-136(%rbp), %rax	# arr, tmp188
	movl	32(%rax), %eax	# arr_116(D)->length, _4
	cltq
# src/Runtime/runtime.c:226:     int32_t *lenghts = malloc(sizeof(int32_t) * arr->length);
	salq	$2, %rax	#, _6
	movq	%rax, %rdi	# _6,
	call	malloc@PLT	#
	movq	%rax, -48(%rbp)	# tmp189, lenghts
# src/Runtime/runtime.c:227:     int32_t totalLenght = 0;
	movl	$0, -20(%rbp)	#, totalLenght
# src/Runtime/runtime.c:229:     for (int i = 0; i < arr->length; i++) {
	movl	$0, -24(%rbp)	#, i
# src/Runtime/runtime.c:229:     for (int i = 0; i < arr->length; i++) {
	jmp	.L53	#
.L58:
# src/Runtime/runtime.c:230:         if (arr->elementSize == sizeof(int32_t)) {
	movq	-136(%rbp), %rax	# arr, tmp190
	movl	28(%rax), %eax	# arr_116(D)->elementSize, _7
# src/Runtime/runtime.c:230:         if (arr->elementSize == sizeof(int32_t)) {
	cmpl	$4, %eax	#, _7
	jne	.L54	#,
# src/Runtime/runtime.c:231:             int32_t *elements = arr->data;
	movq	-136(%rbp), %rax	# arr, tmp191
	movq	8(%rax), %rax	# arr_116(D)->data, tmp192
	movq	%rax, -112(%rbp)	# tmp192, elements
# src/Runtime/runtime.c:232:             strings[i] = intToString(elements[i]);
	movl	-24(%rbp), %eax	# i, tmp193
	cltq
	leaq	0(,%rax,4), %rdx	#, _9
	movq	-112(%rbp), %rax	# elements, tmp194
	addq	%rdx, %rax	# _9, _10
# src/Runtime/runtime.c:232:             strings[i] = intToString(elements[i]);
	movl	(%rax), %eax	# *_10, _11
# src/Runtime/runtime.c:232:             strings[i] = intToString(elements[i]);
	movl	-24(%rbp), %edx	# i, tmp195
	movslq	%edx, %rdx	# tmp195, _12
	leaq	0(,%rdx,8), %rcx	#, _13
	movq	-40(%rbp), %rdx	# strings, tmp196
	leaq	(%rcx,%rdx), %rbx	#, _14
# src/Runtime/runtime.c:232:             strings[i] = intToString(elements[i]);
	movl	%eax, %edi	# _11,
	call	intToString	#
# src/Runtime/runtime.c:232:             strings[i] = intToString(elements[i]);
	movq	%rax, (%rbx)	# _15, *_14
	jmp	.L55	#
.L54:
# src/Runtime/runtime.c:233:         } else if (arr->elementSize == sizeof(int8_t)) {
	movq	-136(%rbp), %rax	# arr, tmp197
	movl	28(%rax), %eax	# arr_116(D)->elementSize, _16
# src/Runtime/runtime.c:233:         } else if (arr->elementSize == sizeof(int8_t)) {
	cmpl	$1, %eax	#, _16
	jne	.L56	#,
# src/Runtime/runtime.c:234:             int8_t *elements = arr->data;
	movq	-136(%rbp), %rax	# arr, tmp198
	movq	8(%rax), %rax	# arr_116(D)->data, tmp199
	movq	%rax, -104(%rbp)	# tmp199, elements
# src/Runtime/runtime.c:235:             strings[i] = byteToString(elements[i]);
	movl	-24(%rbp), %eax	# i, tmp200
	movslq	%eax, %rdx	# tmp200, _17
	movq	-104(%rbp), %rax	# elements, tmp201
	addq	%rdx, %rax	# _17, _18
	movzbl	(%rax), %eax	# *_18, _19
# src/Runtime/runtime.c:235:             strings[i] = byteToString(elements[i]);
	movzbl	%al, %eax	# _20, _21
# src/Runtime/runtime.c:235:             strings[i] = byteToString(elements[i]);
	movl	-24(%rbp), %edx	# i, tmp202
	movslq	%edx, %rdx	# tmp202, _22
	leaq	0(,%rdx,8), %rcx	#, _23
	movq	-40(%rbp), %rdx	# strings, tmp203
	leaq	(%rcx,%rdx), %rbx	#, _24
# src/Runtime/runtime.c:235:             strings[i] = byteToString(elements[i]);
	movl	%eax, %edi	# _21,
	call	byteToString	#
# src/Runtime/runtime.c:235:             strings[i] = byteToString(elements[i]);
	movq	%rax, (%rbx)	# _25, *_24
	jmp	.L55	#
.L56:
# src/Runtime/runtime.c:237:             obj *elements = arr->data;
	movq	-136(%rbp), %rax	# arr, tmp204
	movq	8(%rax), %rax	# arr_116(D)->data, tmp205
	movq	%rax, -80(%rbp)	# tmp205, elements
# src/Runtime/runtime.c:238:             obj element = elements[i];
	movl	-24(%rbp), %eax	# i, tmp206
	cltq
	leaq	0(,%rax,8), %rdx	#, _27
	movq	-80(%rbp), %rax	# elements, tmp207
	addq	%rdx, %rax	# _27, _28
# src/Runtime/runtime.c:238:             obj element = elements[i];
	movq	(%rax), %rax	# *_28, tmp208
	movq	%rax, -88(%rbp)	# tmp208, element
# src/Runtime/runtime.c:239:             if (IS_NULL(element)) {
	leaq	_LAT_NULL(%rip), %rax	#, tmp209
	cmpq	%rax, -88(%rbp)	# tmp209, element
	jne	.L57	#,
# src/Runtime/runtime.c:240:                 strings[i] = __createString("null");
	movl	-24(%rbp), %eax	# i, tmp210
	cltq
	leaq	0(,%rax,8), %rdx	#, _30
	movq	-40(%rbp), %rax	# strings, tmp211
	leaq	(%rdx,%rax), %rbx	#, _31
# src/Runtime/runtime.c:240:                 strings[i] = __createString("null");
	leaq	.LC37(%rip), %rax	#, tmp212
	movq	%rax, %rdi	# tmp212,
	call	__createString	#
# src/Runtime/runtime.c:240:                 strings[i] = __createString("null");
	movq	%rax, (%rbx)	# _32, *_31
# src/Runtime/runtime.c:241:                 __incRef(strings[i]);
	movl	-24(%rbp), %eax	# i, tmp213
	cltq
	leaq	0(,%rax,8), %rdx	#, _34
	movq	-40(%rbp), %rax	# strings, tmp214
	addq	%rdx, %rax	# _34, _35
# src/Runtime/runtime.c:241:                 __incRef(strings[i]);
	movq	(%rax), %rax	# *_35, _36
	movq	%rax, %rdi	# _36,
	call	__incRef	#
	jmp	.L55	#
.L57:
# src/Runtime/runtime.c:243:                 obj (*toString)(obj) = ((void **)element->type->methods)[0];
	movq	-88(%rbp), %rax	# element, tmp215
	movq	(%rax), %rax	# element_149->type, _37
# src/Runtime/runtime.c:243:                 obj (*toString)(obj) = ((void **)element->type->methods)[0];
	movq	20(%rax), %rax	# _37->methods, _38
# src/Runtime/runtime.c:243:                 obj (*toString)(obj) = ((void **)element->type->methods)[0];
	movq	(%rax), %rax	# MEM[(void * *)_38], _39
# src/Runtime/runtime.c:243:                 obj (*toString)(obj) = ((void **)element->type->methods)[0];
	movq	%rax, -96(%rbp)	# _39, toString
# src/Runtime/runtime.c:244:                 strings[i] = toString(element);
	movl	-24(%rbp), %eax	# i, tmp216
	cltq
	leaq	0(,%rax,8), %rdx	#, _41
	movq	-40(%rbp), %rax	# strings, tmp217
	leaq	(%rdx,%rax), %rbx	#, _42
# src/Runtime/runtime.c:244:                 strings[i] = toString(element);
	movq	-88(%rbp), %rax	# element, tmp218
	movq	-96(%rbp), %rdx	# toString, tmp219
	movq	%rax, %rdi	# tmp218,
	call	*%rdx	# tmp219
# src/Runtime/runtime.c:244:                 strings[i] = toString(element);
	movq	%rax, (%rbx)	# _43, *_42
.L55:
# src/Runtime/runtime.c:247:         lenghts[i] = u8_strlen((strings[i]->data));
	movl	-24(%rbp), %eax	# i, tmp220
	cltq
	leaq	0(,%rax,8), %rdx	#, _45
	movq	-40(%rbp), %rax	# strings, tmp221
	addq	%rdx, %rax	# _45, _46
	movq	(%rax), %rax	# *_46, _47
# src/Runtime/runtime.c:247:         lenghts[i] = u8_strlen((strings[i]->data));
	movq	8(%rax), %rax	# _47->data, _48
# src/Runtime/runtime.c:247:         lenghts[i] = u8_strlen((strings[i]->data));
	movq	%rax, %rdi	# _48,
	call	u8_strlen@PLT	#
# src/Runtime/runtime.c:247:         lenghts[i] = u8_strlen((strings[i]->data));
	movl	-24(%rbp), %edx	# i, tmp222
	movslq	%edx, %rdx	# tmp222, _50
	leaq	0(,%rdx,4), %rcx	#, _51
	movq	-48(%rbp), %rdx	# lenghts, tmp223
	addq	%rcx, %rdx	# _51, _52
# src/Runtime/runtime.c:247:         lenghts[i] = u8_strlen((strings[i]->data));
	movl	%eax, (%rdx)	# _53, *_52
# src/Runtime/runtime.c:248:         totalLenght += lenghts[i];
	movl	-24(%rbp), %eax	# i, tmp224
	cltq
	leaq	0(,%rax,4), %rdx	#, _55
	movq	-48(%rbp), %rax	# lenghts, tmp225
	addq	%rdx, %rax	# _55, _56
	movl	(%rax), %eax	# *_56, _57
# src/Runtime/runtime.c:248:         totalLenght += lenghts[i];
	addl	%eax, -20(%rbp)	# _57, totalLenght
# src/Runtime/runtime.c:229:     for (int i = 0; i < arr->length; i++) {
	addl	$1, -24(%rbp)	#, i
.L53:
# src/Runtime/runtime.c:229:     for (int i = 0; i < arr->length; i++) {
	movq	-136(%rbp), %rax	# arr, tmp226
	movl	32(%rax), %eax	# arr_116(D)->length, _58
# src/Runtime/runtime.c:229:     for (int i = 0; i < arr->length; i++) {
	cmpl	%eax, -24(%rbp)	# _58, i
	jl	.L58	#,
# src/Runtime/runtime.c:251:     int32_t bufferSize = u8_strlen(start) + totalLenght +
	leaq	-114(%rbp), %rax	#, tmp227
	movq	%rax, %rdi	# tmp227,
	call	u8_strlen@PLT	#
# src/Runtime/runtime.c:251:     int32_t bufferSize = u8_strlen(start) + totalLenght +
	movl	%eax, %edx	# _59, _60
	movl	-20(%rbp), %eax	# totalLenght, totalLenght.137_61
	leal	(%rdx,%rax), %ebx	#, _62
# src/Runtime/runtime.c:252:                          (arr->length - 1) * u8_strlen(delim) +
	movq	-136(%rbp), %rax	# arr, tmp228
	movl	32(%rax), %eax	# arr_116(D)->length, _63
# src/Runtime/runtime.c:252:                          (arr->length - 1) * u8_strlen(delim) +
	subl	$1, %eax	#, _64
	cltq
# src/Runtime/runtime.c:251:     int32_t bufferSize = u8_strlen(start) + totalLenght +
	movl	%eax, %r12d	# _65, _66
# src/Runtime/runtime.c:252:                          (arr->length - 1) * u8_strlen(delim) +
	leaq	-117(%rbp), %rax	#, tmp229
	movq	%rax, %rdi	# tmp229,
	call	u8_strlen@PLT	#
# src/Runtime/runtime.c:251:     int32_t bufferSize = u8_strlen(start) + totalLenght +
	imull	%r12d, %eax	# _66, _69
	addl	%eax, %ebx	# _69, _70
# src/Runtime/runtime.c:253:                          u8_strlen(end) + 1;
	leaq	-119(%rbp), %rax	#, tmp230
	movq	%rax, %rdi	# tmp230,
	call	u8_strlen@PLT	#
# src/Runtime/runtime.c:252:                          (arr->length - 1) * u8_strlen(delim) +
	addl	%ebx, %eax	# _70, _73
# src/Runtime/runtime.c:253:                          u8_strlen(end) + 1;
	addl	$1, %eax	#, _74
# src/Runtime/runtime.c:251:     int32_t bufferSize = u8_strlen(start) + totalLenght +
	movl	%eax, -52(%rbp)	# _74, bufferSize
# src/Runtime/runtime.c:254:     uint8_t *buffer = malloc(bufferSize);
	movl	-52(%rbp), %eax	# bufferSize, tmp231
	cltq
	movq	%rax, %rdi	# _75,
	call	malloc@PLT	#
	movq	%rax, -64(%rbp)	# tmp232, buffer
# src/Runtime/runtime.c:255:     int32_t index = 0;
	movl	$0, -28(%rbp)	#, index
# src/Runtime/runtime.c:256:     u8_strcpy(buffer + index, start);
	movl	-28(%rbp), %eax	# index, tmp233
	movslq	%eax, %rdx	# tmp233, _76
	movq	-64(%rbp), %rax	# buffer, tmp234
	addq	%rax, %rdx	# tmp234, _77
	leaq	-114(%rbp), %rax	#, tmp235
	movq	%rax, %rsi	# tmp235,
	movq	%rdx, %rdi	# _77,
	call	u8_strcpy@PLT	#
# src/Runtime/runtime.c:257:     index++;
	addl	$1, -28(%rbp)	#, index
# src/Runtime/runtime.c:258:     for (int i = 0; i < arr->length; i++) {
	movl	$0, -32(%rbp)	#, i
# src/Runtime/runtime.c:258:     for (int i = 0; i < arr->length; i++) {
	jmp	.L59	#
.L61:
# src/Runtime/runtime.c:259:         u8_strcpy(buffer + index, (strings[i]->data));
	movl	-32(%rbp), %eax	# i, tmp236
	cltq
	leaq	0(,%rax,8), %rdx	#, _79
	movq	-40(%rbp), %rax	# strings, tmp237
	addq	%rdx, %rax	# _79, _80
	movq	(%rax), %rax	# *_80, _81
# src/Runtime/runtime.c:259:         u8_strcpy(buffer + index, (strings[i]->data));
	movq	8(%rax), %rax	# _81->data, _82
# src/Runtime/runtime.c:259:         u8_strcpy(buffer + index, (strings[i]->data));
	movl	-28(%rbp), %edx	# index, tmp238
	movslq	%edx, %rcx	# tmp238, _83
	movq	-64(%rbp), %rdx	# buffer, tmp239
	addq	%rcx, %rdx	# _83, _84
	movq	%rax, %rsi	# _82,
	movq	%rdx, %rdi	# _84,
	call	u8_strcpy@PLT	#
# src/Runtime/runtime.c:260:         index += lenghts[i];
	movl	-32(%rbp), %eax	# i, tmp240
	cltq
	leaq	0(,%rax,4), %rdx	#, _86
	movq	-48(%rbp), %rax	# lenghts, tmp241
	addq	%rdx, %rax	# _86, _87
	movl	(%rax), %eax	# *_87, _88
# src/Runtime/runtime.c:260:         index += lenghts[i];
	addl	%eax, -28(%rbp)	# _88, index
# src/Runtime/runtime.c:261:         if (i != arr->length - 1) {
	movq	-136(%rbp), %rax	# arr, tmp242
	movl	32(%rax), %eax	# arr_116(D)->length, _89
# src/Runtime/runtime.c:261:         if (i != arr->length - 1) {
	subl	$1, %eax	#, _90
# src/Runtime/runtime.c:261:         if (i != arr->length - 1) {
	cmpl	%eax, -32(%rbp)	# _90, i
	je	.L60	#,
# src/Runtime/runtime.c:262:             u8_strcpy(buffer + index, delim);
	movl	-28(%rbp), %eax	# index, tmp243
	movslq	%eax, %rdx	# tmp243, _91
	movq	-64(%rbp), %rax	# buffer, tmp244
	addq	%rax, %rdx	# tmp244, _92
	leaq	-117(%rbp), %rax	#, tmp245
	movq	%rax, %rsi	# tmp245,
	movq	%rdx, %rdi	# _92,
	call	u8_strcpy@PLT	#
# src/Runtime/runtime.c:263:             index += 2;
	addl	$2, -28(%rbp)	#, index
.L60:
# src/Runtime/runtime.c:265:         __decRef(strings[i]);
	movl	-32(%rbp), %eax	# i, tmp246
	cltq
	leaq	0(,%rax,8), %rdx	#, _94
	movq	-40(%rbp), %rax	# strings, tmp247
	addq	%rdx, %rax	# _94, _95
# src/Runtime/runtime.c:265:         __decRef(strings[i]);
	movq	(%rax), %rax	# *_95, _96
	movq	%rax, %rdi	# _96,
	call	__decRef	#
# src/Runtime/runtime.c:258:     for (int i = 0; i < arr->length; i++) {
	addl	$1, -32(%rbp)	#, i
.L59:
# src/Runtime/runtime.c:258:     for (int i = 0; i < arr->length; i++) {
	movq	-136(%rbp), %rax	# arr, tmp248
	movl	32(%rax), %eax	# arr_116(D)->length, _97
# src/Runtime/runtime.c:258:     for (int i = 0; i < arr->length; i++) {
	cmpl	%eax, -32(%rbp)	# _97, i
	jl	.L61	#,
# src/Runtime/runtime.c:267:     u8_strcpy(buffer + index, end);
	movl	-28(%rbp), %eax	# index, tmp249
	movslq	%eax, %rdx	# tmp249, _98
	movq	-64(%rbp), %rax	# buffer, tmp250
	addq	%rax, %rdx	# tmp250, _99
	leaq	-119(%rbp), %rax	#, tmp251
	movq	%rax, %rsi	# tmp251,
	movq	%rdx, %rdi	# _99,
	call	u8_strcpy@PLT	#
# src/Runtime/runtime.c:268:     buffer[bufferSize - 1] = 0;
	movl	-52(%rbp), %eax	# bufferSize, tmp252
	cltq
	leaq	-1(%rax), %rdx	#, _101
	movq	-64(%rbp), %rax	# buffer, tmp253
	addq	%rdx, %rax	# _101, _102
# src/Runtime/runtime.c:268:     buffer[bufferSize - 1] = 0;
	movb	$0, (%rax)	#, *_102
# src/Runtime/runtime.c:269:     obj ret = __createString(buffer);
	movq	-64(%rbp), %rax	# buffer, tmp254
	movq	%rax, %rdi	# tmp254,
	call	__createString	#
	movq	%rax, -72(%rbp)	# tmp255, ret
# src/Runtime/runtime.c:270:     __incRef(ret);
	movq	-72(%rbp), %rax	# ret, tmp256
	movq	%rax, %rdi	# tmp256,
	call	__incRef	#
# src/Runtime/runtime.c:271:     free(lenghts);
	movq	-48(%rbp), %rax	# lenghts, tmp257
	movq	%rax, %rdi	# tmp257,
	call	free@PLT	#
# src/Runtime/runtime.c:272:     free(strings);
	movq	-40(%rbp), %rax	# strings, tmp258
	movq	%rax, %rdi	# tmp258,
	call	free@PLT	#
# src/Runtime/runtime.c:273:     free(buffer);
	movq	-64(%rbp), %rax	# buffer, tmp259
	movq	%rax, %rdi	# tmp259,
	call	free@PLT	#
# src/Runtime/runtime.c:274:     return ret;
	movq	-72(%rbp), %rax	# ret, _138
# src/Runtime/runtime.c:275: }
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
	subq	$16, %rsp	#,
	movq	%rdi, -8(%rbp)	# str, str
# src/Runtime/runtime.c:278:     __incRef(str);
	movq	-8(%rbp), %rax	# str, tmp84
	movq	%rax, %rdi	# tmp84,
	call	__incRef	#
# src/Runtime/runtime.c:279:     return str;
	movq	-8(%rbp), %rax	# str, _4
# src/Runtime/runtime.c:280: }
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
# src/Runtime/runtime.c:282:     int32_t hash = 0x811c9dc5;
	movl	$-2128831035, -4(%rbp)	#, hash
# src/Runtime/runtime.c:283:     uint8_t *rawstring = str->data;
	movq	-40(%rbp), %rax	# str, tmp89
	movq	8(%rax), %rax	# str_10(D)->data, tmp90
	movq	%rax, -16(%rbp)	# tmp90, rawstring
# src/Runtime/runtime.c:284:     int32_t strlen = u8_strlen(rawstring);
	movq	-16(%rbp), %rax	# rawstring, tmp91
	movq	%rax, %rdi	# tmp91,
	call	u8_strlen@PLT	#
# src/Runtime/runtime.c:284:     int32_t strlen = u8_strlen(rawstring);
	movl	%eax, -20(%rbp)	# _1, strlen
# src/Runtime/runtime.c:285:     for (int i = 0; i < strlen; i++) {
	movl	$0, -8(%rbp)	#, i
# src/Runtime/runtime.c:285:     for (int i = 0; i < strlen; i++) {
	jmp	.L66	#
.L67:
# src/Runtime/runtime.c:286:         hash ^= rawstring[i];
	movl	-8(%rbp), %eax	# i, tmp92
	movslq	%eax, %rdx	# tmp92, _2
	movq	-16(%rbp), %rax	# rawstring, tmp93
	addq	%rdx, %rax	# _2, _3
	movzbl	(%rax), %eax	# *_3, _4
	movzbl	%al, %eax	# _4, _5
# src/Runtime/runtime.c:286:         hash ^= rawstring[i];
	xorl	%eax, -4(%rbp)	# _5, hash
# src/Runtime/runtime.c:287:         hash *= 0x01000193;
	movl	-4(%rbp), %eax	# hash, tmp95
	imull	$16777619, %eax, %eax	#, tmp95, tmp94
	movl	%eax, -4(%rbp)	# tmp94, hash
# src/Runtime/runtime.c:285:     for (int i = 0; i < strlen; i++) {
	addl	$1, -8(%rbp)	#, i
.L66:
# src/Runtime/runtime.c:285:     for (int i = 0; i < strlen; i++) {
	movl	-8(%rbp), %eax	# i, tmp96
	cmpl	-20(%rbp), %eax	# strlen, tmp96
	jl	.L67	#,
# src/Runtime/runtime.c:289:     return hash;
	movl	-4(%rbp), %eax	# hash, _14
# src/Runtime/runtime.c:290: }
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
# src/Runtime/runtime.c:292:     if (IS_NULL(o2))
	leaq	_LAT_NULL(%rip), %rax	#, tmp89
	cmpq	%rax, -48(%rbp)	# tmp89, o2
	jne	.L70	#,
# src/Runtime/runtime.c:293:         return false;
	movl	$0, %eax	#, _6
	jmp	.L71	#
.L70:
# src/Runtime/runtime.c:294:     if (o2->type != &_class_String)
	movq	-48(%rbp), %rax	# o2, tmp90
	movq	(%rax), %rdx	# o2_8(D)->type, _1
# src/Runtime/runtime.c:294:     if (o2->type != &_class_String)
	leaq	_class_String(%rip), %rax	#, tmp91
	cmpq	%rax, %rdx	# tmp91, _1
	je	.L72	#,
# src/Runtime/runtime.c:295:         return false;
	movl	$0, %eax	#, _6
	jmp	.L71	#
.L72:
# src/Runtime/runtime.c:296:     if (_String_length(o1) != _String_length(o2))
	movq	-40(%rbp), %rax	# o1, tmp92
	movq	%rax, %rdi	# tmp92,
	call	_String_length	#
	movl	%eax, %ebx	#, _2
# src/Runtime/runtime.c:296:     if (_String_length(o1) != _String_length(o2))
	movq	-48(%rbp), %rax	# o2, tmp93
	movq	%rax, %rdi	# tmp93,
	call	_String_length	#
# src/Runtime/runtime.c:296:     if (_String_length(o1) != _String_length(o2))
	cmpl	%eax, %ebx	# _3, _2
	je	.L73	#,
# src/Runtime/runtime.c:297:         return false;
	movl	$0, %eax	#, _6
	jmp	.L71	#
.L73:
# src/Runtime/runtime.c:298:     uint8_t *rs1 = (o1->data);
	movq	-40(%rbp), %rax	# o1, tmp94
	movq	8(%rax), %rax	# o1_10(D)->data, tmp95
	movq	%rax, -24(%rbp)	# tmp95, rs1
# src/Runtime/runtime.c:299:     uint8_t *rs2 = (o2->data);
	movq	-48(%rbp), %rax	# o2, tmp96
	movq	8(%rax), %rax	# o2_8(D)->data, tmp97
	movq	%rax, -32(%rbp)	# tmp97, rs2
# src/Runtime/runtime.c:300:     return u8_strcmp(rs1, rs2) == 0;
	movq	-32(%rbp), %rdx	# rs2, tmp98
	movq	-24(%rbp), %rax	# rs1, tmp99
	movq	%rdx, %rsi	# tmp98,
	movq	%rax, %rdi	# tmp99,
	call	u8_strcmp@PLT	#
# src/Runtime/runtime.c:300:     return u8_strcmp(rs1, rs2) == 0;
	testl	%eax, %eax	# _4
	sete	%al	#, _5
.L71:
# src/Runtime/runtime.c:301: }
	movq	-8(%rbp), %rbx	#,
	leave	
	.cfi_def_cfa 7, 8
	ret	
	.cfi_endproc
.LFE24:
	.size	_String_equals, .-_String_equals
	.section	.rodata
	.align 8
.LC38:
	.string	"ERROR: Substring with negative length."
.LC39:
	.string	""
	.align 8
.LC40:
	.string	"ERROR: Substring starting index is too big."
	.align 8
.LC41:
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
# src/Runtime/runtime.c:303:     if (length < 0) {
	cmpl	$0, -80(%rbp)	#, length
	jns	.L75	#,
# src/Runtime/runtime.c:304:         errMsg = "ERROR: Substring with negative length.";
	leaq	.LC38(%rip), %rax	#, tmp101
	movq	%rax, errMsg(%rip)	# tmp101, errMsg
# src/Runtime/runtime.c:305:         error();
	movl	$0, %eax	#,
	call	error	#
.L75:
# src/Runtime/runtime.c:307:     if (length == 0)
	cmpl	$0, -80(%rbp)	#, length
	jne	.L76	#,
# src/Runtime/runtime.c:308:         return __createString("");
	leaq	.LC39(%rip), %rax	#, tmp102
	movq	%rax, %rdi	# tmp102,
	call	__createString	#
	jmp	.L84	#
.L76:
# src/Runtime/runtime.c:309:     if (startIndex >= _String_length(str)) {
	movq	-72(%rbp), %rax	# str, tmp103
	movq	%rax, %rdi	# tmp103,
	call	_String_length	#
# src/Runtime/runtime.c:309:     if (startIndex >= _String_length(str)) {
	cmpl	%eax, -76(%rbp)	# _1, startIndex
	jl	.L78	#,
# src/Runtime/runtime.c:310:         errMsg = "ERROR: Substring starting index is too big.";
	leaq	.LC40(%rip), %rax	#, tmp104
	movq	%rax, errMsg(%rip)	# tmp104, errMsg
# src/Runtime/runtime.c:311:         error();
	movl	$0, %eax	#,
	call	error	#
.L78:
# src/Runtime/runtime.c:313:     uint8_t *rs = (str->data);
	movq	-72(%rbp), %rax	# str, tmp105
	movq	8(%rax), %rax	# str_31(D)->data, tmp106
	movq	%rax, -32(%rbp)	# tmp106, rs
# src/Runtime/runtime.c:314:     uint8_t *offset_str = rs;
	movq	-32(%rbp), %rax	# rs, tmp107
	movq	%rax, -8(%rbp)	# tmp107, offset_str
# src/Runtime/runtime.c:316:     while (startIndex-- > 0)
	jmp	.L79	#
.L80:
# src/Runtime/runtime.c:317:         offset_str += u8_next(&character, offset_str) - offset_str;
	movq	-8(%rbp), %rdx	# offset_str, tmp108
	leaq	-60(%rbp), %rax	#, tmp109
	movq	%rdx, %rsi	# tmp108,
	movq	%rax, %rdi	# tmp109,
	call	u8_next@PLT	#
# src/Runtime/runtime.c:317:         offset_str += u8_next(&character, offset_str) - offset_str;
	subq	-8(%rbp), %rax	# offset_str, _59
# src/Runtime/runtime.c:317:         offset_str += u8_next(&character, offset_str) - offset_str;
	addq	%rax, -8(%rbp)	# _3, offset_str
.L79:
# src/Runtime/runtime.c:316:     while (startIndex-- > 0)
	movl	-76(%rbp), %eax	# startIndex, startIndex.138_4
	leal	-1(%rax), %edx	#, tmp110
	movl	%edx, -76(%rbp)	# tmp110, startIndex
# src/Runtime/runtime.c:316:     while (startIndex-- > 0)
	testl	%eax, %eax	# startIndex.138_4
	jg	.L80	#,
# src/Runtime/runtime.c:318:     uint8_t *end = offset_str;
	movq	-8(%rbp), %rax	# offset_str, tmp111
	movq	%rax, -16(%rbp)	# tmp111, end
# src/Runtime/runtime.c:319:     int32_t counter = 0;
	movl	$0, -20(%rbp)	#, counter
# src/Runtime/runtime.c:320:     while (counter < length) {
	jmp	.L81	#
.L83:
# src/Runtime/runtime.c:321:         if (u8_next(&character, end) == NULL) {
	movq	-16(%rbp), %rdx	# end, tmp112
	leaq	-60(%rbp), %rax	#, tmp113
	movq	%rdx, %rsi	# tmp112,
	movq	%rax, %rdi	# tmp113,
	call	u8_next@PLT	#
# src/Runtime/runtime.c:321:         if (u8_next(&character, end) == NULL) {
	testq	%rax, %rax	# _5
	jne	.L82	#,
# src/Runtime/runtime.c:322:             errMsg = "ERROR: Substring reached end of string.";
	leaq	.LC41(%rip), %rax	#, tmp114
	movq	%rax, errMsg(%rip)	# tmp114, errMsg
# src/Runtime/runtime.c:323:             error();
	movl	$0, %eax	#,
	call	error	#
.L82:
# src/Runtime/runtime.c:325:         end += u8_next(&character, end) - end;
	movq	-16(%rbp), %rdx	# end, tmp115
	leaq	-60(%rbp), %rax	#, tmp116
	movq	%rdx, %rsi	# tmp115,
	movq	%rax, %rdi	# tmp116,
	call	u8_next@PLT	#
# src/Runtime/runtime.c:325:         end += u8_next(&character, end) - end;
	subq	-16(%rbp), %rax	# end, _55
# src/Runtime/runtime.c:325:         end += u8_next(&character, end) - end;
	addq	%rax, -16(%rbp)	# _7, end
# src/Runtime/runtime.c:326:         counter++;
	addl	$1, -20(%rbp)	#, counter
.L81:
# src/Runtime/runtime.c:320:     while (counter < length) {
	movl	-20(%rbp), %eax	# counter, tmp117
	cmpl	-80(%rbp), %eax	# length, tmp117
	jl	.L83	#,
# src/Runtime/runtime.c:328:     int32_t bufferSize = end - offset_str + 1;
	movq	-16(%rbp), %rax	# end, tmp118
	subq	-8(%rbp), %rax	# offset_str, _8
# src/Runtime/runtime.c:328:     int32_t bufferSize = end - offset_str + 1;
	addl	$1, %eax	#, _10
# src/Runtime/runtime.c:328:     int32_t bufferSize = end - offset_str + 1;
	movl	%eax, -36(%rbp)	# _10, bufferSize
# src/Runtime/runtime.c:329:     uint8_t *buffer = malloc(bufferSize);
	movl	-36(%rbp), %eax	# bufferSize, tmp119
	cltq
	movq	%rax, %rdi	# _11,
	call	malloc@PLT	#
	movq	%rax, -48(%rbp)	# tmp120, buffer
# src/Runtime/runtime.c:330:     u8_strncpy(buffer, offset_str, bufferSize);
	movl	-36(%rbp), %eax	# bufferSize, tmp121
	movslq	%eax, %rdx	# tmp121, _12
	movq	-8(%rbp), %rcx	# offset_str, tmp122
	movq	-48(%rbp), %rax	# buffer, tmp123
	movq	%rcx, %rsi	# tmp122,
	movq	%rax, %rdi	# tmp123,
	call	u8_strncpy@PLT	#
# src/Runtime/runtime.c:331:     buffer[bufferSize - 1] = 0;
	movl	-36(%rbp), %eax	# bufferSize, tmp124
	cltq
	leaq	-1(%rax), %rdx	#, _14
	movq	-48(%rbp), %rax	# buffer, tmp125
	addq	%rdx, %rax	# _14, _15
# src/Runtime/runtime.c:331:     buffer[bufferSize - 1] = 0;
	movb	$0, (%rax)	#, *_15
# src/Runtime/runtime.c:332:     obj ret = __createString(buffer);
	movq	-48(%rbp), %rax	# buffer, tmp126
	movq	%rax, %rdi	# tmp126,
	call	__createString	#
	movq	%rax, -56(%rbp)	# tmp127, ret
# src/Runtime/runtime.c:333:     __incRef(ret);
	movq	-56(%rbp), %rax	# ret, tmp128
	movq	%rax, %rdi	# tmp128,
	call	__incRef	#
# src/Runtime/runtime.c:334:     free(buffer);
	movq	-48(%rbp), %rax	# buffer, tmp129
	movq	%rax, %rdi	# tmp129,
	call	free@PLT	#
# src/Runtime/runtime.c:335:     return ret;
	movq	-56(%rbp), %rax	# ret, _20
.L84:
# src/Runtime/runtime.c:336: }
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
# src/Runtime/runtime.c:338:     if (str->length < 0) {
	movq	-8(%rbp), %rax	# str, tmp90
	movl	32(%rax), %eax	# str_9(D)->length, _1
# src/Runtime/runtime.c:338:     if (str->length < 0) {
	testl	%eax, %eax	# _1
	jns	.L86	#,
# src/Runtime/runtime.c:339:         str->length = u8_mbsnlen(str->data, u8_strlen(str->data));
	movq	-8(%rbp), %rax	# str, tmp91
	movq	8(%rax), %rax	# str_9(D)->data, _2
# src/Runtime/runtime.c:339:         str->length = u8_mbsnlen(str->data, u8_strlen(str->data));
	movq	%rax, %rdi	# _2,
	call	u8_strlen@PLT	#
	movq	%rax, %rdx	#, _3
# src/Runtime/runtime.c:339:         str->length = u8_mbsnlen(str->data, u8_strlen(str->data));
	movq	-8(%rbp), %rax	# str, tmp92
	movq	8(%rax), %rax	# str_9(D)->data, _4
# src/Runtime/runtime.c:339:         str->length = u8_mbsnlen(str->data, u8_strlen(str->data));
	movq	%rdx, %rsi	# _3,
	movq	%rax, %rdi	# _4,
	call	u8_mbsnlen@PLT	#
# src/Runtime/runtime.c:339:         str->length = u8_mbsnlen(str->data, u8_strlen(str->data));
	movl	%eax, %edx	# _5, _6
	movq	-8(%rbp), %rax	# str, tmp93
	movl	%edx, 32(%rax)	# _6, str_9(D)->length
.L86:
# src/Runtime/runtime.c:341:     return str->length;
	movq	-8(%rbp), %rax	# str, tmp94
	movl	32(%rax), %eax	# str_9(D)->length, _11
# src/Runtime/runtime.c:342: }
	leave	
	.cfi_def_cfa 7, 8
	ret	
	.cfi_endproc
.LFE26:
	.size	_String_length, .-_String_length
	.section	.rodata
	.align 8
.LC42:
	.string	"ERROR: IndexOf null substring argument."
	.align 8
.LC43:
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
# src/Runtime/runtime.c:344:     if (IS_NULL(substr)) {
	leaq	_LAT_NULL(%rip), %rax	#, tmp95
	cmpq	%rax, -80(%rbp)	# tmp95, substr
	jne	.L89	#,
# src/Runtime/runtime.c:345:         errMsg = "ERROR: IndexOf null substring argument.";
	leaq	.LC42(%rip), %rax	#, tmp96
	movq	%rax, errMsg(%rip)	# tmp96, errMsg
# src/Runtime/runtime.c:346:         error();
	movl	$0, %eax	#,
	call	error	#
.L89:
# src/Runtime/runtime.c:348:     if (startFrom >= _String_length(str)) {
	movq	-72(%rbp), %rax	# str, tmp97
	movq	%rax, %rdi	# tmp97,
	call	_String_length	#
# src/Runtime/runtime.c:348:     if (startFrom >= _String_length(str)) {
	cmpl	%eax, -84(%rbp)	# _1, startFrom
	jl	.L90	#,
# src/Runtime/runtime.c:349:         errMsg = "ERROR: IndexOf starting index is too big.";
	leaq	.LC43(%rip), %rax	#, tmp98
	movq	%rax, errMsg(%rip)	# tmp98, errMsg
# src/Runtime/runtime.c:350:         error();
	movl	$0, %eax	#,
	call	error	#
.L90:
# src/Runtime/runtime.c:352:     if (_String_length(str) < _String_length(substr))
	movq	-72(%rbp), %rax	# str, tmp99
	movq	%rax, %rdi	# tmp99,
	call	_String_length	#
	movl	%eax, %ebx	#, _2
# src/Runtime/runtime.c:352:     if (_String_length(str) < _String_length(substr))
	movq	-80(%rbp), %rax	# substr, tmp100
	movq	%rax, %rdi	# tmp100,
	call	_String_length	#
# src/Runtime/runtime.c:352:     if (_String_length(str) < _String_length(substr))
	cmpl	%eax, %ebx	# _3, _2
	jge	.L91	#,
# src/Runtime/runtime.c:353:         return -1;
	movl	$-1, %eax	#, _14
	jmp	.L98	#
.L91:
# src/Runtime/runtime.c:354:     uint8_t *rs = (str->data);
	movq	-72(%rbp), %rax	# str, tmp101
	movq	8(%rax), %rax	# str_24(D)->data, tmp102
	movq	%rax, -24(%rbp)	# tmp102, rs
# src/Runtime/runtime.c:355:     uint8_t *rsub = (substr->data);
	movq	-80(%rbp), %rax	# substr, tmp103
	movq	8(%rax), %rax	# substr_20(D)->data, tmp104
	movq	%rax, -48(%rbp)	# tmp104, rsub
# src/Runtime/runtime.c:356:     uint8_t *start = rs;
	movq	-24(%rbp), %rax	# rs, tmp105
	movq	%rax, -32(%rbp)	# tmp105, start
# src/Runtime/runtime.c:358:     while (startFrom-- > 0) {
	jmp	.L93	#
.L95:
# src/Runtime/runtime.c:359:         if (u8_next(&c, start) == NULL)
	movq	-32(%rbp), %rdx	# start, tmp106
	leaq	-60(%rbp), %rax	#, tmp107
	movq	%rdx, %rsi	# tmp106,
	movq	%rax, %rdi	# tmp107,
	call	u8_next@PLT	#
# src/Runtime/runtime.c:359:         if (u8_next(&c, start) == NULL)
	testq	%rax, %rax	# _4
	jne	.L94	#,
# src/Runtime/runtime.c:360:             return -1;
	movl	$-1, %eax	#, _14
	jmp	.L98	#
.L94:
# src/Runtime/runtime.c:361:         start += u8_next(&c, start) - start;
	movq	-32(%rbp), %rdx	# start, tmp108
	leaq	-60(%rbp), %rax	#, tmp109
	movq	%rdx, %rsi	# tmp108,
	movq	%rax, %rdi	# tmp109,
	call	u8_next@PLT	#
# src/Runtime/runtime.c:361:         start += u8_next(&c, start) - start;
	subq	-32(%rbp), %rax	# start, _44
# src/Runtime/runtime.c:361:         start += u8_next(&c, start) - start;
	addq	%rax, -32(%rbp)	# _6, start
.L93:
# src/Runtime/runtime.c:358:     while (startFrom-- > 0) {
	movl	-84(%rbp), %eax	# startFrom, startFrom.139_7
	leal	-1(%rax), %edx	#, tmp110
	movl	%edx, -84(%rbp)	# tmp110, startFrom
# src/Runtime/runtime.c:358:     while (startFrom-- > 0) {
	testl	%eax, %eax	# startFrom.139_7
	jg	.L95	#,
# src/Runtime/runtime.c:363:     uint8_t *index = u8_strstr(start, rsub);
	movq	-48(%rbp), %rdx	# rsub, tmp111
	movq	-32(%rbp), %rax	# start, tmp112
	movq	%rdx, %rsi	# tmp111,
	movq	%rax, %rdi	# tmp112,
	call	u8_strstr@PLT	#
	movq	%rax, -56(%rbp)	# tmp113, index
# src/Runtime/runtime.c:364:     uint32_t counter = 0;
	movl	$0, -36(%rbp)	#, counter
# src/Runtime/runtime.c:365:     while ((rs += u8_next(&c, rs) - rs) != index)
	jmp	.L96	#
.L97:
# src/Runtime/runtime.c:366:         counter++;
	addl	$1, -36(%rbp)	#, counter
.L96:
# src/Runtime/runtime.c:365:     while ((rs += u8_next(&c, rs) - rs) != index)
	movq	-24(%rbp), %rdx	# rs, tmp114
	leaq	-60(%rbp), %rax	#, tmp115
	movq	%rdx, %rsi	# tmp114,
	movq	%rax, %rdi	# tmp115,
	call	u8_next@PLT	#
# src/Runtime/runtime.c:365:     while ((rs += u8_next(&c, rs) - rs) != index)
	subq	-24(%rbp), %rax	# rs, _38
# src/Runtime/runtime.c:365:     while ((rs += u8_next(&c, rs) - rs) != index)
	addq	%rax, -24(%rbp)	# _9, rs
# src/Runtime/runtime.c:365:     while ((rs += u8_next(&c, rs) - rs) != index)
	movq	-24(%rbp), %rax	# rs, tmp116
	cmpq	-56(%rbp), %rax	# index, tmp116
	jne	.L97	#,
# src/Runtime/runtime.c:367:     return counter;
	movl	-36(%rbp), %eax	# counter, _14
.L98:
# src/Runtime/runtime.c:368: }
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
# src/Runtime/runtime.c:370:     uint8_t *rs = (str->data);
	movq	-40(%rbp), %rax	# str, tmp89
	movq	8(%rax), %rax	# str_7(D)->data, tmp90
	movq	%rax, -8(%rbp)	# tmp90, rs
# src/Runtime/runtime.c:371:     int32_t len = rs == NULL ? 0 : u8_strlen(rs);
	cmpq	$0, -8(%rbp)	#, rs
	je	.L100	#,
# src/Runtime/runtime.c:371:     int32_t len = rs == NULL ? 0 : u8_strlen(rs);
	movq	-8(%rbp), %rax	# rs, tmp91
	movq	%rax, %rdi	# tmp91,
	call	u8_strlen@PLT	#
	jmp	.L101	#
.L100:
# src/Runtime/runtime.c:371:     int32_t len = rs == NULL ? 0 : u8_strlen(rs);
	movl	$0, %eax	#, iftmp.140_5
.L101:
# src/Runtime/runtime.c:371:     int32_t len = rs == NULL ? 0 : u8_strlen(rs);
	movl	%eax, -12(%rbp)	# iftmp.140_5, len
# src/Runtime/runtime.c:372:     obj arr = __newByteArray(len + 1);
	movl	-12(%rbp), %eax	# len, tmp92
	addl	$1, %eax	#, _2
	movl	%eax, %edi	# _2,
	call	__newByteArray	#
	movq	%rax, -24(%rbp)	# tmp93, arr
# src/Runtime/runtime.c:373:     memcpy((arr->data), rs, len);
	movl	-12(%rbp), %eax	# len, tmp94
	movslq	%eax, %rdx	# tmp94, _3
	movq	-24(%rbp), %rax	# arr, tmp95
	movq	8(%rax), %rax	# arr_13->data, _4
	movq	-8(%rbp), %rcx	# rs, tmp96
	movq	%rcx, %rsi	# tmp96,
	movq	%rax, %rdi	# _4,
	call	memcpy@PLT	#
# src/Runtime/runtime.c:374:     __incRef(arr);
	movq	-24(%rbp), %rax	# arr, tmp97
	movq	%rax, %rdi	# tmp97,
	call	__incRef	#
# src/Runtime/runtime.c:375:     return arr;
	movq	-24(%rbp), %rax	# arr, _16
# src/Runtime/runtime.c:376: }
	leave	
	.cfi_def_cfa 7, 8
	ret	
	.cfi_endproc
.LFE28:
	.size	_String_getBytes, .-_String_getBytes
	.section	.rodata
	.align 8
.LC44:
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
# src/Runtime/runtime.c:378:     if (IS_NULL(substr)) {
	leaq	_LAT_NULL(%rip), %rax	#, tmp85
	cmpq	%rax, -32(%rbp)	# tmp85, substr
	jne	.L104	#,
# src/Runtime/runtime.c:379:         errMsg = "ERROR: EndsWith null substring argument.";
	leaq	.LC44(%rip), %rax	#, tmp86
	movq	%rax, errMsg(%rip)	# tmp86, errMsg
# src/Runtime/runtime.c:380:         error();
	movl	$0, %eax	#,
	call	error	#
.L104:
# src/Runtime/runtime.c:382:     uint8_t *rs = (str->data);
	movq	-24(%rbp), %rax	# str, tmp87
	movq	8(%rax), %rax	# str_7(D)->data, tmp88
	movq	%rax, -8(%rbp)	# tmp88, rs
# src/Runtime/runtime.c:383:     uint8_t *rsub = (substr->data);
	movq	-32(%rbp), %rax	# substr, tmp89
	movq	8(%rax), %rax	# substr_3(D)->data, tmp90
	movq	%rax, -16(%rbp)	# tmp90, rsub
# src/Runtime/runtime.c:384:     return u8_endswith(rs, rsub);
	movq	-16(%rbp), %rdx	# rsub, tmp91
	movq	-8(%rbp), %rax	# rs, tmp92
	movq	%rdx, %rsi	# tmp91,
	movq	%rax, %rdi	# tmp92,
	call	u8_endswith@PLT	#
# src/Runtime/runtime.c:385: }
	leave	
	.cfi_def_cfa 7, 8
	ret	
	.cfi_endproc
.LFE29:
	.size	_String_endsWith, .-_String_endsWith
	.section	.rodata
	.align 8
.LC45:
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
# src/Runtime/runtime.c:387:     if (IS_NULL(substr)) {
	leaq	_LAT_NULL(%rip), %rax	#, tmp85
	cmpq	%rax, -32(%rbp)	# tmp85, substr
	jne	.L107	#,
# src/Runtime/runtime.c:388:         errMsg = "ERROR: StartsWith null substring argument.";
	leaq	.LC45(%rip), %rax	#, tmp86
	movq	%rax, errMsg(%rip)	# tmp86, errMsg
# src/Runtime/runtime.c:389:         error();
	movl	$0, %eax	#,
	call	error	#
.L107:
# src/Runtime/runtime.c:391:     uint8_t *rs = (str->data);
	movq	-24(%rbp), %rax	# str, tmp87
	movq	8(%rax), %rax	# str_7(D)->data, tmp88
	movq	%rax, -8(%rbp)	# tmp88, rs
# src/Runtime/runtime.c:392:     uint8_t *rsub = (substr->data);
	movq	-32(%rbp), %rax	# substr, tmp89
	movq	8(%rax), %rax	# substr_3(D)->data, tmp90
	movq	%rax, -16(%rbp)	# tmp90, rsub
# src/Runtime/runtime.c:393:     return u8_startswith(rs, rsub);
	movq	-16(%rbp), %rdx	# rsub, tmp91
	movq	-8(%rbp), %rax	# rs, tmp92
	movq	%rdx, %rsi	# tmp91,
	movq	%rax, %rdi	# tmp92,
	call	u8_startswith@PLT	#
# src/Runtime/runtime.c:394: }
	leave	
	.cfi_def_cfa 7, 8
	ret	
	.cfi_endproc
.LFE30:
	.size	_String_startsWith, .-_String_startsWith
	.section	.rodata
.LC46:
	.string	"String concat on %p and %p"
.LC47:
	.string	"Take strlen"
.LC48:
	.string	"perform strcpy"
	.align 8
.LC49:
	.string	"Create final string result='%s' (len1=%d, len2=%d, str1='%s', str2='%s')"
.LC50:
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
# src/Runtime/runtime.c:396:     DEBUG("String concat on %p and %p", str, secondstr);
	movq	stderr(%rip), %rax	# stderr, stderr.141_1
	movq	%rax, %rcx	# stderr.141_1,
	movl	$14, %edx	#,
	movl	$1, %esi	#,
	leaq	.LC0(%rip), %rax	#, tmp114
	movq	%rax, %rdi	# tmp114,
	call	fwrite@PLT	#
	movq	stderr(%rip), %rax	# stderr, stderr.142_2
	movq	-64(%rbp), %rcx	# secondstr, tmp115
	movq	-56(%rbp), %rdx	# str, tmp116
	leaq	.LC46(%rip), %rsi	#, tmp117
	movq	%rax, %rdi	# stderr.142_2,
	movl	$0, %eax	#,
	call	fprintf@PLT	#
	movq	stderr(%rip), %rax	# stderr, stderr.143_3
	movq	%rax, %rsi	# stderr.143_3,
	movl	$10, %edi	#,
	call	fputc@PLT	#
	movq	stderr(%rip), %rax	# stderr, stderr.144_4
	movq	%rax, %rdi	# stderr.144_4,
	call	fflush@PLT	#
# src/Runtime/runtime.c:397:     if (IS_NULL(secondstr)) {
	leaq	_LAT_NULL(%rip), %rax	#, tmp118
	cmpq	%rax, -64(%rbp)	# tmp118, secondstr
	jne	.L110	#,
# src/Runtime/runtime.c:398:         __incRef(str);
	movq	-56(%rbp), %rax	# str, tmp119
	movq	%rax, %rdi	# tmp119,
	call	__incRef	#
# src/Runtime/runtime.c:399:         return str;
	movq	-56(%rbp), %rax	# str, _31
	jmp	.L111	#
.L110:
# src/Runtime/runtime.c:401:     uint8_t *rs1 = (str->data);
	movq	-56(%rbp), %rax	# str, tmp120
	movq	8(%rax), %rax	# str_35(D)->data, tmp121
	movq	%rax, -8(%rbp)	# tmp121, rs1
# src/Runtime/runtime.c:402:     uint8_t *rs2 = (secondstr->data);
	movq	-64(%rbp), %rax	# secondstr, tmp122
	movq	8(%rax), %rax	# secondstr_36(D)->data, tmp123
	movq	%rax, -16(%rbp)	# tmp123, rs2
# src/Runtime/runtime.c:403:     DEBUG("Take strlen");
	movq	stderr(%rip), %rax	# stderr, stderr.145_5
	movq	%rax, %rcx	# stderr.145_5,
	movl	$14, %edx	#,
	movl	$1, %esi	#,
	leaq	.LC0(%rip), %rax	#, tmp124
	movq	%rax, %rdi	# tmp124,
	call	fwrite@PLT	#
	movq	stderr(%rip), %rax	# stderr, stderr.146_6
	movq	%rax, %rcx	# stderr.146_6,
	movl	$11, %edx	#,
	movl	$1, %esi	#,
	leaq	.LC47(%rip), %rax	#, tmp125
	movq	%rax, %rdi	# tmp125,
	call	fwrite@PLT	#
	movq	stderr(%rip), %rax	# stderr, stderr.147_7
	movq	%rax, %rsi	# stderr.147_7,
	movl	$10, %edi	#,
	call	fputc@PLT	#
	movq	stderr(%rip), %rax	# stderr, stderr.148_8
	movq	%rax, %rdi	# stderr.148_8,
	call	fflush@PLT	#
# src/Runtime/runtime.c:404:     int32_t len1 = u8_strlen(rs1);
	movq	-8(%rbp), %rax	# rs1, tmp126
	movq	%rax, %rdi	# tmp126,
	call	u8_strlen@PLT	#
# src/Runtime/runtime.c:404:     int32_t len1 = u8_strlen(rs1);
	movl	%eax, -20(%rbp)	# _9, len1
# src/Runtime/runtime.c:405:     int32_t len2 = u8_strlen(rs2);
	movq	-16(%rbp), %rax	# rs2, tmp127
	movq	%rax, %rdi	# tmp127,
	call	u8_strlen@PLT	#
# src/Runtime/runtime.c:405:     int32_t len2 = u8_strlen(rs2);
	movl	%eax, -24(%rbp)	# _10, len2
# src/Runtime/runtime.c:406:     uint8_t *buffer = malloc(len1 + len2 + 1);
	movl	-20(%rbp), %edx	# len1, tmp128
	movl	-24(%rbp), %eax	# len2, tmp129
	addl	%edx, %eax	# tmp128, _11
# src/Runtime/runtime.c:406:     uint8_t *buffer = malloc(len1 + len2 + 1);
	addl	$1, %eax	#, _12
# src/Runtime/runtime.c:406:     uint8_t *buffer = malloc(len1 + len2 + 1);
	cltq
	movq	%rax, %rdi	# _13,
	call	malloc@PLT	#
	movq	%rax, -32(%rbp)	# tmp130, buffer
# src/Runtime/runtime.c:407:     DEBUG("perform strcpy");
	movq	stderr(%rip), %rax	# stderr, stderr.149_14
	movq	%rax, %rcx	# stderr.149_14,
	movl	$14, %edx	#,
	movl	$1, %esi	#,
	leaq	.LC0(%rip), %rax	#, tmp131
	movq	%rax, %rdi	# tmp131,
	call	fwrite@PLT	#
	movq	stderr(%rip), %rax	# stderr, stderr.150_15
	movq	%rax, %rcx	# stderr.150_15,
	movl	$14, %edx	#,
	movl	$1, %esi	#,
	leaq	.LC48(%rip), %rax	#, tmp132
	movq	%rax, %rdi	# tmp132,
	call	fwrite@PLT	#
	movq	stderr(%rip), %rax	# stderr, stderr.151_16
	movq	%rax, %rsi	# stderr.151_16,
	movl	$10, %edi	#,
	call	fputc@PLT	#
	movq	stderr(%rip), %rax	# stderr, stderr.152_17
	movq	%rax, %rdi	# stderr.152_17,
	call	fflush@PLT	#
# src/Runtime/runtime.c:408:     u8_strcpy(buffer, rs1);
	movq	-8(%rbp), %rdx	# rs1, tmp133
	movq	-32(%rbp), %rax	# buffer, tmp134
	movq	%rdx, %rsi	# tmp133,
	movq	%rax, %rdi	# tmp134,
	call	u8_strcpy@PLT	#
# src/Runtime/runtime.c:409:     u8_strcpy(buffer + len1, rs2);
	movl	-20(%rbp), %eax	# len1, tmp135
	movslq	%eax, %rdx	# tmp135, _18
	movq	-32(%rbp), %rax	# buffer, tmp136
	addq	%rax, %rdx	# tmp136, _19
	movq	-16(%rbp), %rax	# rs2, tmp137
	movq	%rax, %rsi	# tmp137,
	movq	%rdx, %rdi	# _19,
	call	u8_strcpy@PLT	#
# src/Runtime/runtime.c:410:     buffer[len1 + len2] = 0;
	movl	-20(%rbp), %edx	# len1, tmp138
	movl	-24(%rbp), %eax	# len2, tmp139
	addl	%edx, %eax	# tmp138, _20
	movslq	%eax, %rdx	# _20, _21
# src/Runtime/runtime.c:410:     buffer[len1 + len2] = 0;
	movq	-32(%rbp), %rax	# buffer, tmp140
	addq	%rdx, %rax	# _21, _22
# src/Runtime/runtime.c:410:     buffer[len1 + len2] = 0;
	movb	$0, (%rax)	#, *_22
# src/Runtime/runtime.c:411:     DEBUG("Create final string result='%s' (len1=%d, len2=%d, str1='%s', str2='%s')", buffer, len1, len2, rs1, rs2);
	movq	stderr(%rip), %rax	# stderr, stderr.153_23
	movq	%rax, %rcx	# stderr.153_23,
	movl	$14, %edx	#,
	movl	$1, %esi	#,
	leaq	.LC0(%rip), %rax	#, tmp141
	movq	%rax, %rdi	# tmp141,
	call	fwrite@PLT	#
	movq	stderr(%rip), %rax	# stderr, stderr.154_24
	movq	-8(%rbp), %rdi	# rs1, tmp142
	movl	-24(%rbp), %esi	# len2, tmp143
	movl	-20(%rbp), %ecx	# len1, tmp144
	movq	-32(%rbp), %rdx	# buffer, tmp145
	subq	$8, %rsp	#,
	pushq	-16(%rbp)	# rs2
	movq	%rdi, %r9	# tmp142,
	movl	%esi, %r8d	# tmp143,
	leaq	.LC49(%rip), %rsi	#, tmp146
	movq	%rax, %rdi	# stderr.154_24,
	movl	$0, %eax	#,
	call	fprintf@PLT	#
	addq	$16, %rsp	#,
	movq	stderr(%rip), %rax	# stderr, stderr.155_25
	movq	%rax, %rsi	# stderr.155_25,
	movl	$10, %edi	#,
	call	fputc@PLT	#
	movq	stderr(%rip), %rax	# stderr, stderr.156_26
	movq	%rax, %rdi	# stderr.156_26,
	call	fflush@PLT	#
# src/Runtime/runtime.c:412:     obj ret = __createString(buffer);
	movq	-32(%rbp), %rax	# buffer, tmp147
	movq	%rax, %rdi	# tmp147,
	call	__createString	#
	movq	%rax, -40(%rbp)	# tmp148, ret
# src/Runtime/runtime.c:413:     __incRef(ret);
	movq	-40(%rbp), %rax	# ret, tmp149
	movq	%rax, %rdi	# tmp149,
	call	__incRef	#
# src/Runtime/runtime.c:414:     free(buffer);
	movq	-32(%rbp), %rax	# buffer, tmp150
	movq	%rax, %rdi	# tmp150,
	call	free@PLT	#
# src/Runtime/runtime.c:415:     DEBUG("String concat completed");
	movq	stderr(%rip), %rax	# stderr, stderr.157_27
	movq	%rax, %rcx	# stderr.157_27,
	movl	$14, %edx	#,
	movl	$1, %esi	#,
	leaq	.LC0(%rip), %rax	#, tmp151
	movq	%rax, %rdi	# tmp151,
	call	fwrite@PLT	#
	movq	stderr(%rip), %rax	# stderr, stderr.158_28
	movq	%rax, %rcx	# stderr.158_28,
	movl	$23, %edx	#,
	movl	$1, %esi	#,
	leaq	.LC50(%rip), %rax	#, tmp152
	movq	%rax, %rdi	# tmp152,
	call	fwrite@PLT	#
	movq	stderr(%rip), %rax	# stderr, stderr.159_29
	movq	%rax, %rsi	# stderr.159_29,
	movl	$10, %edi	#,
	call	fputc@PLT	#
	movq	stderr(%rip), %rax	# stderr, stderr.160_30
	movq	%rax, %rdi	# stderr.160_30,
	call	fflush@PLT	#
# src/Runtime/runtime.c:416:     return ret;
	movq	-40(%rbp), %rax	# ret, _31
.L111:
# src/Runtime/runtime.c:417: }
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
# src/Runtime/runtime.c:421:     uint8_t *rs = (str->data);
	movq	-24(%rbp), %rax	# str, tmp91
	movq	8(%rax), %rax	# str_13(D)->data, tmp92
	movq	%rax, -8(%rbp)	# tmp92, rs
# src/Runtime/runtime.c:423:     while (index-- > 0) {
	jmp	.L113	#
.L115:
# src/Runtime/runtime.c:424:         if (u8_next(&c, rs) == NULL) {
	movq	-8(%rbp), %rdx	# rs, tmp93
	leaq	-12(%rbp), %rax	#, tmp94
	movq	%rdx, %rsi	# tmp93,
	movq	%rax, %rdi	# tmp94,
	call	u8_next@PLT	#
# src/Runtime/runtime.c:424:         if (u8_next(&c, rs) == NULL) {
	testq	%rax, %rax	# _1
	jne	.L114	#,
# src/Runtime/runtime.c:425:             errMsg = charAtErr;
	leaq	charAtErr(%rip), %rax	#, tmp95
	movq	%rax, errMsg(%rip)	# tmp95, errMsg
# src/Runtime/runtime.c:426:             error();
	movl	$0, %eax	#,
	call	error	#
.L114:
# src/Runtime/runtime.c:428:         rs += u8_next(&c, rs) - rs;
	movq	-8(%rbp), %rdx	# rs, tmp96
	leaq	-12(%rbp), %rax	#, tmp97
	movq	%rdx, %rsi	# tmp96,
	movq	%rax, %rdi	# tmp97,
	call	u8_next@PLT	#
# src/Runtime/runtime.c:428:         rs += u8_next(&c, rs) - rs;
	subq	-8(%rbp), %rax	# rs, _26
# src/Runtime/runtime.c:428:         rs += u8_next(&c, rs) - rs;
	addq	%rax, -8(%rbp)	# _3, rs
.L113:
# src/Runtime/runtime.c:423:     while (index-- > 0) {
	movl	-28(%rbp), %eax	# index, index.161_4
	leal	-1(%rax), %edx	#, tmp98
	movl	%edx, -28(%rbp)	# tmp98, index
# src/Runtime/runtime.c:423:     while (index-- > 0) {
	testl	%eax, %eax	# index.161_4
	jg	.L115	#,
# src/Runtime/runtime.c:430:     if (u8_strmbtouc(&c, rs) <= 0) {
	movq	-8(%rbp), %rdx	# rs, tmp99
	leaq	-12(%rbp), %rax	#, tmp100
	movq	%rdx, %rsi	# tmp99,
	movq	%rax, %rdi	# tmp100,
	call	u8_strmbtouc@PLT	#
# src/Runtime/runtime.c:430:     if (u8_strmbtouc(&c, rs) <= 0) {
	testl	%eax, %eax	# _5
	jg	.L116	#,
# src/Runtime/runtime.c:431:         errMsg = charAtErr;
	leaq	charAtErr(%rip), %rax	#, tmp101
	movq	%rax, errMsg(%rip)	# tmp101, errMsg
# src/Runtime/runtime.c:432:         error();
	movl	$0, %eax	#,
	call	error	#
.L116:
# src/Runtime/runtime.c:434:     return c;
	movl	-12(%rbp), %eax	# c, c.162_6
# src/Runtime/runtime.c:435: }
	leave	
	.cfi_def_cfa 7, 8
	ret	
	.cfi_endproc
.LFE32:
	.size	_String_charAt, .-_String_charAt
	.section	.rodata
.LC51:
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
# src/Runtime/runtime.c:438:     DEBUG("%p", str);
	movq	stderr(%rip), %rax	# stderr, stderr.163_1
	movq	%rax, %rcx	# stderr.163_1,
	movl	$14, %edx	#,
	movl	$1, %esi	#,
	leaq	.LC0(%rip), %rax	#, tmp86
	movq	%rax, %rdi	# tmp86,
	call	fwrite@PLT	#
	movq	stderr(%rip), %rax	# stderr, stderr.164_2
	movq	-8(%rbp), %rdx	# str, tmp87
	leaq	.LC51(%rip), %rcx	#, tmp88
	movq	%rcx, %rsi	# tmp88,
	movq	%rax, %rdi	# stderr.164_2,
	movl	$0, %eax	#,
	call	fprintf@PLT	#
	movq	stderr(%rip), %rax	# stderr, stderr.165_3
	movq	%rax, %rsi	# stderr.165_3,
	movl	$10, %edi	#,
	call	fputc@PLT	#
	movq	stderr(%rip), %rax	# stderr, stderr.166_4
	movq	%rax, %rdi	# stderr.166_4,
	call	fflush@PLT	#
# src/Runtime/runtime.c:439: }
	nop	
	leave	
	.cfi_def_cfa 7, 8
	ret	
	.cfi_endproc
.LFE33:
	.size	ddd, .-ddd
	.section	.rodata
.LC52:
	.string	"Calling printString(%p)"
.LC53:
	.string	"Str data is %p"
.LC54:
	.string	"Str inner data %p"
.LC55:
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
# src/Runtime/runtime.c:443:     DEBUG("Calling printString(%p)", FORMAT_PTR(str));
	movq	stderr(%rip), %rax	# stderr, stderr.167_1
	movq	%rax, %rcx	# stderr.167_1,
	movl	$14, %edx	#,
	movl	$1, %esi	#,
	leaq	.LC0(%rip), %rax	#, tmp101
	movq	%rax, %rdi	# tmp101,
	call	fwrite@PLT	#
	movq	stderr(%rip), %rax	# stderr, stderr.168_2
	movq	-24(%rbp), %rdx	# str, tmp102
	leaq	.LC52(%rip), %rcx	#, tmp103
	movq	%rcx, %rsi	# tmp103,
	movq	%rax, %rdi	# stderr.168_2,
	movl	$0, %eax	#,
	call	fprintf@PLT	#
	movq	stderr(%rip), %rax	# stderr, stderr.169_3
	movq	%rax, %rsi	# stderr.169_3,
	movl	$10, %edi	#,
	call	fputc@PLT	#
	movq	stderr(%rip), %rax	# stderr, stderr.170_4
	movq	%rax, %rdi	# stderr.170_4,
	call	fflush@PLT	#
# src/Runtime/runtime.c:444:     DEBUG("Str data is %p", FORMAT_PTR(str->data));
	movq	stderr(%rip), %rax	# stderr, stderr.171_5
	movq	%rax, %rcx	# stderr.171_5,
	movl	$14, %edx	#,
	movl	$1, %esi	#,
	leaq	.LC0(%rip), %rax	#, tmp104
	movq	%rax, %rdi	# tmp104,
	call	fwrite@PLT	#
	movq	-24(%rbp), %rax	# str, tmp105
	movq	8(%rax), %rdx	# str_22(D)->data, _6
	movq	stderr(%rip), %rax	# stderr, stderr.172_7
	leaq	.LC53(%rip), %rcx	#, tmp106
	movq	%rcx, %rsi	# tmp106,
	movq	%rax, %rdi	# stderr.172_7,
	movl	$0, %eax	#,
	call	fprintf@PLT	#
	movq	stderr(%rip), %rax	# stderr, stderr.173_8
	movq	%rax, %rsi	# stderr.173_8,
	movl	$10, %edi	#,
	call	fputc@PLT	#
	movq	stderr(%rip), %rax	# stderr, stderr.174_9
	movq	%rax, %rdi	# stderr.174_9,
	call	fflush@PLT	#
# src/Runtime/runtime.c:445:     if (IS_NULL(str))
	leaq	_LAT_NULL(%rip), %rax	#, tmp107
	cmpq	%rax, -24(%rbp)	# tmp107, str
	jne	.L120	#,
# src/Runtime/runtime.c:446:         str = __createString("null");
	leaq	.LC37(%rip), %rax	#, tmp108
	movq	%rax, %rdi	# tmp108,
	call	__createString	#
	movq	%rax, -24(%rbp)	# tmp109, str
.L120:
# src/Runtime/runtime.c:447:     __incRef(str);
	movq	-24(%rbp), %rax	# str, tmp110
	movq	%rax, %rdi	# tmp110,
	call	__incRef	#
# src/Runtime/runtime.c:448:     uint8_t *rs = (str->data);
	movq	-24(%rbp), %rax	# str, tmp111
	movq	8(%rax), %rax	# str_18->data, tmp112
	movq	%rax, -8(%rbp)	# tmp112, rs
# src/Runtime/runtime.c:449:     DEBUG("Str inner data %p", FORMAT_PTR(rs));
	movq	stderr(%rip), %rax	# stderr, stderr.175_10
	movq	%rax, %rcx	# stderr.175_10,
	movl	$14, %edx	#,
	movl	$1, %esi	#,
	leaq	.LC0(%rip), %rax	#, tmp113
	movq	%rax, %rdi	# tmp113,
	call	fwrite@PLT	#
	movq	stderr(%rip), %rax	# stderr, stderr.176_11
	movq	-8(%rbp), %rdx	# rs, tmp114
	leaq	.LC54(%rip), %rcx	#, tmp115
	movq	%rcx, %rsi	# tmp115,
	movq	%rax, %rdi	# stderr.176_11,
	movl	$0, %eax	#,
	call	fprintf@PLT	#
	movq	stderr(%rip), %rax	# stderr, stderr.177_12
	movq	%rax, %rsi	# stderr.177_12,
	movl	$10, %edi	#,
	call	fputc@PLT	#
	movq	stderr(%rip), %rax	# stderr, stderr.178_13
	movq	%rax, %rdi	# stderr.178_13,
	call	fflush@PLT	#
# src/Runtime/runtime.c:450:     printf("%s\n", rs);
	movq	-8(%rbp), %rax	# rs, tmp116
	movq	%rax, %rdi	# tmp116,
	call	puts@PLT	#
# src/Runtime/runtime.c:451:     __decRef(str);
	movq	-24(%rbp), %rax	# str, tmp117
	movq	%rax, %rdi	# tmp117,
	call	__decRef	#
# src/Runtime/runtime.c:452:     DEBUG("printString(%p) completed", FORMAT_PTR(str))
	movq	stderr(%rip), %rax	# stderr, stderr.179_14
	movq	%rax, %rcx	# stderr.179_14,
	movl	$14, %edx	#,
	movl	$1, %esi	#,
	leaq	.LC0(%rip), %rax	#, tmp118
	movq	%rax, %rdi	# tmp118,
	call	fwrite@PLT	#
	movq	stderr(%rip), %rax	# stderr, stderr.180_15
	movq	-24(%rbp), %rdx	# str, tmp119
	leaq	.LC55(%rip), %rcx	#, tmp120
	movq	%rcx, %rsi	# tmp120,
	movq	%rax, %rdi	# stderr.180_15,
	movl	$0, %eax	#,
	call	fprintf@PLT	#
	movq	stderr(%rip), %rax	# stderr, stderr.181_16
	movq	%rax, %rsi	# stderr.181_16,
	movl	$10, %edi	#,
	call	fputc@PLT	#
	movq	stderr(%rip), %rax	# stderr, stderr.182_17
	movq	%rax, %rdi	# stderr.182_17,
	call	fflush@PLT	#
# src/Runtime/runtime.c:453:     return 0;
	movl	$0, %eax	#, _44
# src/Runtime/runtime.c:454: }
	leave	
	.cfi_def_cfa 7, 8
	ret	
	.cfi_endproc
.LFE34:
	.size	printString, .-printString
	.section	.rodata
.LC56:
	.string	"Calling printInt()"
.LC57:
	.string	"%p\n"
.LC58:
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
	movq	%rdi, -8(%rbp)	# i, i
# src/Runtime/runtime.c:456:     DEBUG("Calling printInt()")
	movq	stderr(%rip), %rax	# stderr, stderr.183_1
	movq	%rax, %rcx	# stderr.183_1,
	movl	$14, %edx	#,
	movl	$1, %esi	#,
	leaq	.LC0(%rip), %rax	#, tmp92
	movq	%rax, %rdi	# tmp92,
	call	fwrite@PLT	#
	movq	stderr(%rip), %rax	# stderr, stderr.184_2
	movq	%rax, %rcx	# stderr.184_2,
	movl	$18, %edx	#,
	movl	$1, %esi	#,
	leaq	.LC56(%rip), %rax	#, tmp93
	movq	%rax, %rdi	# tmp93,
	call	fwrite@PLT	#
	movq	stderr(%rip), %rax	# stderr, stderr.185_3
	movq	%rax, %rsi	# stderr.185_3,
	movl	$10, %edi	#,
	call	fputc@PLT	#
	movq	stderr(%rip), %rax	# stderr, stderr.186_4
	movq	%rax, %rdi	# stderr.186_4,
	call	fflush@PLT	#
# src/Runtime/runtime.c:457:     printf("%p\n", i);
	movq	-8(%rbp), %rax	# i, tmp94
	movq	%rax, %rsi	# tmp94,
	leaq	.LC57(%rip), %rax	#, tmp95
	movq	%rax, %rdi	# tmp95,
	movl	$0, %eax	#,
	call	printf@PLT	#
# src/Runtime/runtime.c:458:     DEBUG("printInt() completed")
	movq	stderr(%rip), %rax	# stderr, stderr.187_5
	movq	%rax, %rcx	# stderr.187_5,
	movl	$14, %edx	#,
	movl	$1, %esi	#,
	leaq	.LC0(%rip), %rax	#, tmp96
	movq	%rax, %rdi	# tmp96,
	call	fwrite@PLT	#
	movq	stderr(%rip), %rax	# stderr, stderr.188_6
	movq	%rax, %rcx	# stderr.188_6,
	movl	$20, %edx	#,
	movl	$1, %esi	#,
	leaq	.LC58(%rip), %rax	#, tmp97
	movq	%rax, %rdi	# tmp97,
	call	fwrite@PLT	#
	movq	stderr(%rip), %rax	# stderr, stderr.189_7
	movq	%rax, %rsi	# stderr.189_7,
	movl	$10, %edi	#,
	call	fputc@PLT	#
	movq	stderr(%rip), %rax	# stderr, stderr.190_8
	movq	%rax, %rdi	# stderr.190_8,
	call	fflush@PLT	#
# src/Runtime/runtime.c:459:     return 0;
	movl	$0, %eax	#, _20
# src/Runtime/runtime.c:460: }
	leave	
	.cfi_def_cfa 7, 8
	ret	
	.cfi_endproc
.LFE35:
	.size	printInt, .-printInt
	.section	.rodata
.LC59:
	.string	"Calling printBoolean()"
.LC60:
	.string	"true"
.LC61:
	.string	"false"
.LC62:
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
# src/Runtime/runtime.c:462:     DEBUG("Calling printBoolean()")
	movq	stderr(%rip), %rax	# stderr, stderr.191_1
	movq	%rax, %rcx	# stderr.191_1,
	movl	$14, %edx	#,
	movl	$1, %esi	#,
	leaq	.LC0(%rip), %rax	#, tmp94
	movq	%rax, %rdi	# tmp94,
	call	fwrite@PLT	#
	movq	stderr(%rip), %rax	# stderr, stderr.192_2
	movq	%rax, %rcx	# stderr.192_2,
	movl	$22, %edx	#,
	movl	$1, %esi	#,
	leaq	.LC59(%rip), %rax	#, tmp95
	movq	%rax, %rdi	# tmp95,
	call	fwrite@PLT	#
	movq	stderr(%rip), %rax	# stderr, stderr.193_3
	movq	%rax, %rsi	# stderr.193_3,
	movl	$10, %edi	#,
	call	fputc@PLT	#
	movq	stderr(%rip), %rax	# stderr, stderr.194_4
	movq	%rax, %rdi	# stderr.194_4,
	call	fflush@PLT	#
# src/Runtime/runtime.c:463:     if (b)
	cmpb	$0, -4(%rbp)	#, b
	je	.L125	#,
# src/Runtime/runtime.c:464:         printf("true\n");
	leaq	.LC60(%rip), %rax	#, tmp96
	movq	%rax, %rdi	# tmp96,
	call	puts@PLT	#
	jmp	.L126	#
.L125:
# src/Runtime/runtime.c:466:         printf("false\n");
	leaq	.LC61(%rip), %rax	#, tmp97
	movq	%rax, %rdi	# tmp97,
	call	puts@PLT	#
.L126:
# src/Runtime/runtime.c:467:     DEBUG("printBoolean() completed")
	movq	stderr(%rip), %rax	# stderr, stderr.195_5
	movq	%rax, %rcx	# stderr.195_5,
	movl	$14, %edx	#,
	movl	$1, %esi	#,
	leaq	.LC0(%rip), %rax	#, tmp98
	movq	%rax, %rdi	# tmp98,
	call	fwrite@PLT	#
	movq	stderr(%rip), %rax	# stderr, stderr.196_6
	movq	%rax, %rcx	# stderr.196_6,
	movl	$24, %edx	#,
	movl	$1, %esi	#,
	leaq	.LC62(%rip), %rax	#, tmp99
	movq	%rax, %rdi	# tmp99,
	call	fwrite@PLT	#
	movq	stderr(%rip), %rax	# stderr, stderr.197_7
	movq	%rax, %rsi	# stderr.197_7,
	movl	$10, %edi	#,
	call	fputc@PLT	#
	movq	stderr(%rip), %rax	# stderr, stderr.198_8
	movq	%rax, %rdi	# stderr.198_8,
	call	fflush@PLT	#
# src/Runtime/runtime.c:468:     return 0;
	movl	$0, %eax	#, _22
# src/Runtime/runtime.c:469: }
	leave	
	.cfi_def_cfa 7, 8
	ret	
	.cfi_endproc
.LFE36:
	.size	printBoolean, .-printBoolean
	.section	.rodata
	.align 8
.LC63:
	.string	"Calling intToString() conversion"
.LC64:
	.string	"%d"
	.align 8
.LC65:
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
# src/Runtime/runtime.c:471:     DEBUG("Calling intToString() conversion")
	movq	stderr(%rip), %rax	# stderr, stderr.199_1
	movq	%rax, %rcx	# stderr.199_1,
	movl	$14, %edx	#,
	movl	$1, %esi	#,
	leaq	.LC0(%rip), %rax	#, tmp92
	movq	%rax, %rdi	# tmp92,
	call	fwrite@PLT	#
	movq	stderr(%rip), %rax	# stderr, stderr.200_2
	movq	%rax, %rcx	# stderr.200_2,
	movl	$32, %edx	#,
	movl	$1, %esi	#,
	leaq	.LC63(%rip), %rax	#, tmp93
	movq	%rax, %rdi	# tmp93,
	call	fwrite@PLT	#
	movq	stderr(%rip), %rax	# stderr, stderr.201_3
	movq	%rax, %rsi	# stderr.201_3,
	movl	$10, %edi	#,
	call	fputc@PLT	#
	movq	stderr(%rip), %rax	# stderr, stderr.202_4
	movq	%rax, %rdi	# stderr.202_4,
	call	fflush@PLT	#
# src/Runtime/runtime.c:473:     sprintf(buffer, "%d", i);
	movl	-36(%rbp), %edx	# i, tmp94
	leaq	-19(%rbp), %rax	#, tmp95
	leaq	.LC64(%rip), %rcx	#, tmp96
	movq	%rcx, %rsi	# tmp96,
	movq	%rax, %rdi	# tmp95,
	movl	$0, %eax	#,
	call	sprintf@PLT	#
# src/Runtime/runtime.c:474:     obj ret = __createString(buffer);
	leaq	-19(%rbp), %rax	#, tmp97
	movq	%rax, %rdi	# tmp97,
	call	__createString	#
	movq	%rax, -8(%rbp)	# tmp98, ret
# src/Runtime/runtime.c:475:     __incRef(ret);
	movq	-8(%rbp), %rax	# ret, tmp99
	movq	%rax, %rdi	# tmp99,
	call	__incRef	#
# src/Runtime/runtime.c:476:     DEBUG("intToString() conversion completed")
	movq	stderr(%rip), %rax	# stderr, stderr.203_5
	movq	%rax, %rcx	# stderr.203_5,
	movl	$14, %edx	#,
	movl	$1, %esi	#,
	leaq	.LC0(%rip), %rax	#, tmp100
	movq	%rax, %rdi	# tmp100,
	call	fwrite@PLT	#
	movq	stderr(%rip), %rax	# stderr, stderr.204_6
	movq	%rax, %rcx	# stderr.204_6,
	movl	$34, %edx	#,
	movl	$1, %esi	#,
	leaq	.LC65(%rip), %rax	#, tmp101
	movq	%rax, %rdi	# tmp101,
	call	fwrite@PLT	#
	movq	stderr(%rip), %rax	# stderr, stderr.205_7
	movq	%rax, %rsi	# stderr.205_7,
	movl	$10, %edi	#,
	call	fputc@PLT	#
	movq	stderr(%rip), %rax	# stderr, stderr.206_8
	movq	%rax, %rdi	# stderr.206_8,
	call	fflush@PLT	#
# src/Runtime/runtime.c:477:     return ret;
	movq	-8(%rbp), %rax	# ret, _23
# src/Runtime/runtime.c:478: }
	leave	
	.cfi_def_cfa 7, 8
	ret	
	.cfi_endproc
.LFE37:
	.size	intToString, .-intToString
	.section	.rodata
	.align 8
.LC66:
	.string	"Calling byteToString() conversion"
.LC67:
	.string	"%u"
	.align 8
.LC68:
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
# src/Runtime/runtime.c:481:     DEBUG("Calling byteToString() conversion")
	movq	stderr(%rip), %rax	# stderr, stderr.207_1
	movq	%rax, %rcx	# stderr.207_1,
	movl	$14, %edx	#,
	movl	$1, %esi	#,
	leaq	.LC0(%rip), %rax	#, tmp95
	movq	%rax, %rdi	# tmp95,
	call	fwrite@PLT	#
	movq	stderr(%rip), %rax	# stderr, stderr.208_2
	movq	%rax, %rcx	# stderr.208_2,
	movl	$33, %edx	#,
	movl	$1, %esi	#,
	leaq	.LC66(%rip), %rax	#, tmp96
	movq	%rax, %rdi	# tmp96,
	call	fwrite@PLT	#
	movq	stderr(%rip), %rax	# stderr, stderr.209_3
	movq	%rax, %rsi	# stderr.209_3,
	movl	$10, %edi	#,
	call	fputc@PLT	#
	movq	stderr(%rip), %rax	# stderr, stderr.210_4
	movq	%rax, %rdi	# stderr.210_4,
	call	fflush@PLT	#
# src/Runtime/runtime.c:483:     sprintf(buffer, "%u", i);
	movzbl	-36(%rbp), %edx	# i, _5
	leaq	-19(%rbp), %rax	#, tmp97
	leaq	.LC67(%rip), %rcx	#, tmp98
	movq	%rcx, %rsi	# tmp98,
	movq	%rax, %rdi	# tmp97,
	movl	$0, %eax	#,
	call	sprintf@PLT	#
# src/Runtime/runtime.c:484:     obj ret = __createString(buffer);
	leaq	-19(%rbp), %rax	#, tmp99
	movq	%rax, %rdi	# tmp99,
	call	__createString	#
	movq	%rax, -8(%rbp)	# tmp100, ret
# src/Runtime/runtime.c:485:     __incRef(ret);
	movq	-8(%rbp), %rax	# ret, tmp101
	movq	%rax, %rdi	# tmp101,
	call	__incRef	#
# src/Runtime/runtime.c:486:     DEBUG("byteToString() conversion completed")
	movq	stderr(%rip), %rax	# stderr, stderr.211_6
	movq	%rax, %rcx	# stderr.211_6,
	movl	$14, %edx	#,
	movl	$1, %esi	#,
	leaq	.LC0(%rip), %rax	#, tmp102
	movq	%rax, %rdi	# tmp102,
	call	fwrite@PLT	#
	movq	stderr(%rip), %rax	# stderr, stderr.212_7
	movq	%rax, %rcx	# stderr.212_7,
	movl	$35, %edx	#,
	movl	$1, %esi	#,
	leaq	.LC68(%rip), %rax	#, tmp103
	movq	%rax, %rdi	# tmp103,
	call	fwrite@PLT	#
	movq	stderr(%rip), %rax	# stderr, stderr.213_8
	movq	%rax, %rsi	# stderr.213_8,
	movl	$10, %edi	#,
	call	fputc@PLT	#
	movq	stderr(%rip), %rax	# stderr, stderr.214_9
	movq	%rax, %rdi	# stderr.214_9,
	call	fflush@PLT	#
# src/Runtime/runtime.c:487:     return ret;
	movq	-8(%rbp), %rax	# ret, _24
# src/Runtime/runtime.c:488: }
	leave	
	.cfi_def_cfa 7, 8
	ret	
	.cfi_endproc
.LFE38:
	.size	byteToString, .-byteToString
	.section	.rodata
	.align 8
.LC69:
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
# src/Runtime/runtime.c:491:     DEBUG("boolToString() conversion completed")
	movq	stderr(%rip), %rax	# stderr, stderr.215_1
	movq	%rax, %rcx	# stderr.215_1,
	movl	$14, %edx	#,
	movl	$1, %esi	#,
	leaq	.LC0(%rip), %rax	#, tmp94
	movq	%rax, %rdi	# tmp94,
	call	fwrite@PLT	#
	movq	stderr(%rip), %rax	# stderr, stderr.216_2
	movq	%rax, %rcx	# stderr.216_2,
	movl	$35, %edx	#,
	movl	$1, %esi	#,
	leaq	.LC69(%rip), %rax	#, tmp95
	movq	%rax, %rdi	# tmp95,
	call	fwrite@PLT	#
	movq	stderr(%rip), %rax	# stderr, stderr.217_3
	movq	%rax, %rsi	# stderr.217_3,
	movl	$10, %edi	#,
	call	fputc@PLT	#
	movq	stderr(%rip), %rax	# stderr, stderr.218_4
	movq	%rax, %rdi	# stderr.218_4,
	call	fflush@PLT	#
# src/Runtime/runtime.c:493:     if (b)
	cmpb	$0, -20(%rbp)	#, b
	je	.L133	#,
# src/Runtime/runtime.c:494:         ret = __createString("true");
	leaq	.LC60(%rip), %rax	#, tmp96
	movq	%rax, %rdi	# tmp96,
	call	__createString	#
	movq	%rax, -8(%rbp)	# tmp97, ret
	jmp	.L134	#
.L133:
# src/Runtime/runtime.c:496:         ret = __createString("false");
	leaq	.LC61(%rip), %rax	#, tmp98
	movq	%rax, %rdi	# tmp98,
	call	__createString	#
	movq	%rax, -8(%rbp)	# tmp99, ret
.L134:
# src/Runtime/runtime.c:497:     __incRef(ret);
	movq	-8(%rbp), %rax	# ret, tmp100
	movq	%rax, %rdi	# tmp100,
	call	__incRef	#
# src/Runtime/runtime.c:498:     DEBUG("boolToString() conversion completed")
	movq	stderr(%rip), %rax	# stderr, stderr.219_5
	movq	%rax, %rcx	# stderr.219_5,
	movl	$14, %edx	#,
	movl	$1, %esi	#,
	leaq	.LC0(%rip), %rax	#, tmp101
	movq	%rax, %rdi	# tmp101,
	call	fwrite@PLT	#
	movq	stderr(%rip), %rax	# stderr, stderr.220_6
	movq	%rax, %rcx	# stderr.220_6,
	movl	$35, %edx	#,
	movl	$1, %esi	#,
	leaq	.LC69(%rip), %rax	#, tmp102
	movq	%rax, %rdi	# tmp102,
	call	fwrite@PLT	#
	movq	stderr(%rip), %rax	# stderr, stderr.221_7
	movq	%rax, %rsi	# stderr.221_7,
	movl	$10, %edi	#,
	call	fputc@PLT	#
	movq	stderr(%rip), %rax	# stderr, stderr.222_8
	movq	%rax, %rdi	# stderr.222_8,
	call	fflush@PLT	#
# src/Runtime/runtime.c:499:     return ret;
	movq	-8(%rbp), %rax	# ret, _26
# src/Runtime/runtime.c:500: }
	leave	
	.cfi_def_cfa 7, 8
	ret	
	.cfi_endproc
.LFE39:
	.size	boolToString, .-boolToString
	.section	.rodata
.LC70:
	.string	"Calling generic print(obj)"
.LC71:
	.string	"Calling generic obj.type=%p"
.LC72:
	.string	"DETECTED obj IS NULL"
	.align 8
.LC73:
	.string	"Subcall to internal printString() method"
.LC74:
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
# src/Runtime/runtime.c:503:     DEBUG("Calling generic print(obj)");
	movq	stderr(%rip), %rax	# stderr, stderr.223_1
	movq	%rax, %rcx	# stderr.223_1,
	movl	$14, %edx	#,
	movl	$1, %esi	#,
	leaq	.LC0(%rip), %rax	#, tmp108
	movq	%rax, %rdi	# tmp108,
	call	fwrite@PLT	#
	movq	stderr(%rip), %rax	# stderr, stderr.224_2
	movq	%rax, %rcx	# stderr.224_2,
	movl	$26, %edx	#,
	movl	$1, %esi	#,
	leaq	.LC70(%rip), %rax	#, tmp109
	movq	%rax, %rdi	# tmp109,
	call	fwrite@PLT	#
	movq	stderr(%rip), %rax	# stderr, stderr.225_3
	movq	%rax, %rsi	# stderr.225_3,
	movl	$10, %edi	#,
	call	fputc@PLT	#
	movq	stderr(%rip), %rax	# stderr, stderr.226_4
	movq	%rax, %rdi	# stderr.226_4,
	call	fflush@PLT	#
# src/Runtime/runtime.c:504:     DEBUG("Calling generic obj.type=%p", FORMAT_PTR(o->type));
	movq	stderr(%rip), %rax	# stderr, stderr.227_5
	movq	%rax, %rcx	# stderr.227_5,
	movl	$14, %edx	#,
	movl	$1, %esi	#,
	leaq	.LC0(%rip), %rax	#, tmp110
	movq	%rax, %rdi	# tmp110,
	call	fwrite@PLT	#
	movq	-24(%rbp), %rax	# o, tmp111
	movq	(%rax), %rdx	# o_33(D)->type, _6
	movq	stderr(%rip), %rax	# stderr, stderr.228_7
	leaq	.LC71(%rip), %rcx	#, tmp112
	movq	%rcx, %rsi	# tmp112,
	movq	%rax, %rdi	# stderr.228_7,
	movl	$0, %eax	#,
	call	fprintf@PLT	#
	movq	stderr(%rip), %rax	# stderr, stderr.229_8
	movq	%rax, %rsi	# stderr.229_8,
	movl	$10, %edi	#,
	call	fputc@PLT	#
	movq	stderr(%rip), %rax	# stderr, stderr.230_9
	movq	%rax, %rdi	# stderr.230_9,
	call	fflush@PLT	#
# src/Runtime/runtime.c:505:     if (IS_NULL(o)) {
	leaq	_LAT_NULL(%rip), %rax	#, tmp113
	cmpq	%rax, -24(%rbp)	# tmp113, o
	jne	.L137	#,
# src/Runtime/runtime.c:506:         DEBUG("DETECTED obj IS NULL");
	movq	stderr(%rip), %rax	# stderr, stderr.231_10
	movq	%rax, %rcx	# stderr.231_10,
	movl	$14, %edx	#,
	movl	$1, %esi	#,
	leaq	.LC0(%rip), %rax	#, tmp114
	movq	%rax, %rdi	# tmp114,
	call	fwrite@PLT	#
	movq	stderr(%rip), %rax	# stderr, stderr.232_11
	movq	%rax, %rcx	# stderr.232_11,
	movl	$20, %edx	#,
	movl	$1, %esi	#,
	leaq	.LC72(%rip), %rax	#, tmp115
	movq	%rax, %rdi	# tmp115,
	call	fwrite@PLT	#
	movq	stderr(%rip), %rax	# stderr, stderr.233_12
	movq	%rax, %rsi	# stderr.233_12,
	movl	$10, %edi	#,
	call	fputc@PLT	#
	movq	stderr(%rip), %rax	# stderr, stderr.234_13
	movq	%rax, %rdi	# stderr.234_13,
	call	fflush@PLT	#
# src/Runtime/runtime.c:507:         o = __createString("null");
	leaq	.LC37(%rip), %rax	#, tmp116
	movq	%rax, %rdi	# tmp116,
	call	__createString	#
	movq	%rax, -24(%rbp)	# tmp117, o
.L137:
# src/Runtime/runtime.c:509:     __incRef(o);
	movq	-24(%rbp), %rax	# o, tmp118
	movq	%rax, %rdi	# tmp118,
	call	__incRef	#
# src/Runtime/runtime.c:510:     obj (*toStr)(obj) = ((void **)o->type->methods)[0];
	movq	-24(%rbp), %rax	# o, tmp119
	movq	(%rax), %rax	# o_25->type, _14
# src/Runtime/runtime.c:510:     obj (*toStr)(obj) = ((void **)o->type->methods)[0];
	movq	20(%rax), %rax	# _14->methods, _15
# src/Runtime/runtime.c:510:     obj (*toStr)(obj) = ((void **)o->type->methods)[0];
	movq	(%rax), %rax	# MEM[(void * *)_15], _16
# src/Runtime/runtime.c:510:     obj (*toStr)(obj) = ((void **)o->type->methods)[0];
	movq	%rax, -8(%rbp)	# _16, toStr
# src/Runtime/runtime.c:511:     obj str = toStr(o);
	movq	-24(%rbp), %rax	# o, tmp120
	movq	-8(%rbp), %rdx	# toStr, tmp121
	movq	%rax, %rdi	# tmp120,
	call	*%rdx	# tmp121
	movq	%rax, -16(%rbp)	# tmp122, str
# src/Runtime/runtime.c:512:     DEBUG("Subcall to internal printString() method")
	movq	stderr(%rip), %rax	# stderr, stderr.235_17
	movq	%rax, %rcx	# stderr.235_17,
	movl	$14, %edx	#,
	movl	$1, %esi	#,
	leaq	.LC0(%rip), %rax	#, tmp123
	movq	%rax, %rdi	# tmp123,
	call	fwrite@PLT	#
	movq	stderr(%rip), %rax	# stderr, stderr.236_18
	movq	%rax, %rcx	# stderr.236_18,
	movl	$40, %edx	#,
	movl	$1, %esi	#,
	leaq	.LC73(%rip), %rax	#, tmp124
	movq	%rax, %rdi	# tmp124,
	call	fwrite@PLT	#
	movq	stderr(%rip), %rax	# stderr, stderr.237_19
	movq	%rax, %rsi	# stderr.237_19,
	movl	$10, %edi	#,
	call	fputc@PLT	#
	movq	stderr(%rip), %rax	# stderr, stderr.238_20
	movq	%rax, %rdi	# stderr.238_20,
	call	fflush@PLT	#
# src/Runtime/runtime.c:513:     printString(str);
	movq	-16(%rbp), %rax	# str, tmp125
	movq	%rax, %rdi	# tmp125,
	call	printString	#
# src/Runtime/runtime.c:514:     __decRef(str);
	movq	-16(%rbp), %rax	# str, tmp126
	movq	%rax, %rdi	# tmp126,
	call	__decRef	#
# src/Runtime/runtime.c:515:     __decRef(o);
	movq	-24(%rbp), %rax	# o, tmp127
	movq	%rax, %rdi	# tmp127,
	call	__decRef	#
# src/Runtime/runtime.c:516:     DEBUG("Generic print(obj) completed")
	movq	stderr(%rip), %rax	# stderr, stderr.239_21
	movq	%rax, %rcx	# stderr.239_21,
	movl	$14, %edx	#,
	movl	$1, %esi	#,
	leaq	.LC0(%rip), %rax	#, tmp128
	movq	%rax, %rdi	# tmp128,
	call	fwrite@PLT	#
	movq	stderr(%rip), %rax	# stderr, stderr.240_22
	movq	%rax, %rcx	# stderr.240_22,
	movl	$28, %edx	#,
	movl	$1, %esi	#,
	leaq	.LC74(%rip), %rax	#, tmp129
	movq	%rax, %rdi	# tmp129,
	call	fwrite@PLT	#
	movq	stderr(%rip), %rax	# stderr, stderr.241_23
	movq	%rax, %rsi	# stderr.241_23,
	movl	$10, %edi	#,
	call	fputc@PLT	#
	movq	stderr(%rip), %rax	# stderr, stderr.242_24
	movq	%rax, %rdi	# stderr.242_24,
	call	fflush@PLT	#
# src/Runtime/runtime.c:517:     return 0;
	movl	$0, %eax	#, _58
# src/Runtime/runtime.c:518: }
	leave	
	.cfi_def_cfa 7, 8
	ret	
	.cfi_endproc
.LFE40:
	.size	print, .-print
	.section	.rodata
.LC75:
	.string	"Calling printBinArray(arr)"
	.align 8
.LC76:
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
	subq	$16, %rsp	#,
	movq	%rdi, -8(%rbp)	# arr, arr
# src/Runtime/runtime.c:521:     DEBUG("Calling printBinArray(arr)")
	movq	stderr(%rip), %rax	# stderr, stderr.243_1
	movq	%rax, %rcx	# stderr.243_1,
	movl	$14, %edx	#,
	movl	$1, %esi	#,
	leaq	.LC0(%rip), %rax	#, tmp96
	movq	%rax, %rdi	# tmp96,
	call	fwrite@PLT	#
	movq	stderr(%rip), %rax	# stderr, stderr.244_2
	movq	%rax, %rcx	# stderr.244_2,
	movl	$26, %edx	#,
	movl	$1, %esi	#,
	leaq	.LC75(%rip), %rax	#, tmp97
	movq	%rax, %rdi	# tmp97,
	call	fwrite@PLT	#
	movq	stderr(%rip), %rax	# stderr, stderr.245_3
	movq	%rax, %rsi	# stderr.245_3,
	movl	$10, %edi	#,
	call	fputc@PLT	#
	movq	stderr(%rip), %rax	# stderr, stderr.246_4
	movq	%rax, %rdi	# stderr.246_4,
	call	fflush@PLT	#
# src/Runtime/runtime.c:522:     if (IS_NULL(arr)){
	leaq	_LAT_NULL(%rip), %rax	#, tmp98
	cmpq	%rax, -8(%rbp)	# tmp98, arr
	jne	.L140	#,
# src/Runtime/runtime.c:523:         print(arr);
	movq	-8(%rbp), %rax	# arr, tmp99
	movq	%rax, %rdi	# tmp99,
	call	print	#
# src/Runtime/runtime.c:524:         return 0;
	movl	$0, %eax	#, _13
	jmp	.L141	#
.L140:
# src/Runtime/runtime.c:526:     __incRef(arr);
	movq	-8(%rbp), %rax	# arr, tmp100
	movq	%rax, %rdi	# tmp100,
	call	__incRef	#
# src/Runtime/runtime.c:527:     fwrite(arr->data, sizeof(int8_t), arr->length, stdout);
	movq	stdout(%rip), %rcx	# stdout, stdout.247_5
# src/Runtime/runtime.c:527:     fwrite(arr->data, sizeof(int8_t), arr->length, stdout);
	movq	-8(%rbp), %rax	# arr, tmp101
	movl	32(%rax), %eax	# arr_20(D)->length, _6
# src/Runtime/runtime.c:527:     fwrite(arr->data, sizeof(int8_t), arr->length, stdout);
	movslq	%eax, %rdx	# _6, _7
# src/Runtime/runtime.c:527:     fwrite(arr->data, sizeof(int8_t), arr->length, stdout);
	movq	-8(%rbp), %rax	# arr, tmp102
	movq	8(%rax), %rax	# arr_20(D)->data, _8
# src/Runtime/runtime.c:527:     fwrite(arr->data, sizeof(int8_t), arr->length, stdout);
	movl	$1, %esi	#,
	movq	%rax, %rdi	# _8,
	call	fwrite@PLT	#
# src/Runtime/runtime.c:528:     __decRef(arr);
	movq	-8(%rbp), %rax	# arr, tmp103
	movq	%rax, %rdi	# tmp103,
	call	__decRef	#
# src/Runtime/runtime.c:529:     DEBUG("printBinArray(arr) call completed")
	movq	stderr(%rip), %rax	# stderr, stderr.248_9
	movq	%rax, %rcx	# stderr.248_9,
	movl	$14, %edx	#,
	movl	$1, %esi	#,
	leaq	.LC0(%rip), %rax	#, tmp104
	movq	%rax, %rdi	# tmp104,
	call	fwrite@PLT	#
	movq	stderr(%rip), %rax	# stderr, stderr.249_10
	movq	%rax, %rcx	# stderr.249_10,
	movl	$33, %edx	#,
	movl	$1, %esi	#,
	leaq	.LC76(%rip), %rax	#, tmp105
	movq	%rax, %rdi	# tmp105,
	call	fwrite@PLT	#
	movq	stderr(%rip), %rax	# stderr, stderr.250_11
	movq	%rax, %rsi	# stderr.250_11,
	movl	$10, %edi	#,
	call	fputc@PLT	#
	movq	stderr(%rip), %rax	# stderr, stderr.251_12
	movq	%rax, %rdi	# stderr.251_12,
	call	fflush@PLT	#
# src/Runtime/runtime.c:530:     return 0;
	movl	$0, %eax	#, _13
.L141:
# src/Runtime/runtime.c:531: }
	leave	
	.cfi_def_cfa 7, 8
	ret	
	.cfi_endproc
.LFE41:
	.size	printBinArray, .-printBinArray
	.section	.rodata
.LC77:
	.string	"Calling error()"
.LC78:
	.string	"%s\n"
.LC79:
	.string	"ERROR: User error."
.LC80:
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
# src/Runtime/runtime.c:534:     DEBUG("Calling error()")
	movq	stderr(%rip), %rax	# stderr, stderr.252_1
	movq	%rax, %rcx	# stderr.252_1,
	movl	$14, %edx	#,
	movl	$1, %esi	#,
	leaq	.LC0(%rip), %rax	#, tmp95
	movq	%rax, %rdi	# tmp95,
	call	fwrite@PLT	#
	movq	stderr(%rip), %rax	# stderr, stderr.253_2
	movq	%rax, %rcx	# stderr.253_2,
	movl	$15, %edx	#,
	movl	$1, %esi	#,
	leaq	.LC77(%rip), %rax	#, tmp96
	movq	%rax, %rdi	# tmp96,
	call	fwrite@PLT	#
	movq	stderr(%rip), %rax	# stderr, stderr.254_3
	movq	%rax, %rsi	# stderr.254_3,
	movl	$10, %edi	#,
	call	fputc@PLT	#
	movq	stderr(%rip), %rax	# stderr, stderr.255_4
	movq	%rax, %rdi	# stderr.255_4,
	call	fflush@PLT	#
# src/Runtime/runtime.c:535:     if (errMsg != NULL)
	movq	errMsg(%rip), %rax	# errMsg, errMsg.256_5
# src/Runtime/runtime.c:535:     if (errMsg != NULL)
	testq	%rax, %rax	# errMsg.256_5
	je	.L143	#,
# src/Runtime/runtime.c:536:         fprintf(stderr, "%s\n", errMsg);
	movq	errMsg(%rip), %rdx	# errMsg, errMsg.257_6
	movq	stderr(%rip), %rax	# stderr, stderr.258_7
	leaq	.LC78(%rip), %rcx	#, tmp97
	movq	%rcx, %rsi	# tmp97,
	movq	%rax, %rdi	# stderr.258_7,
	movl	$0, %eax	#,
	call	fprintf@PLT	#
	jmp	.L144	#
.L143:
# src/Runtime/runtime.c:538:         fprintf(stderr, "%s\n", "ERROR: User error.");
	movq	stderr(%rip), %rax	# stderr, stderr.259_8
	leaq	.LC79(%rip), %rdx	#, tmp98
	leaq	.LC78(%rip), %rcx	#, tmp99
	movq	%rcx, %rsi	# tmp99,
	movq	%rax, %rdi	# stderr.259_8,
	movl	$0, %eax	#,
	call	fprintf@PLT	#
.L144:
# src/Runtime/runtime.c:539:     DEBUG("Exiting via error() (exit=1)")
	movq	stderr(%rip), %rax	# stderr, stderr.260_9
	movq	%rax, %rcx	# stderr.260_9,
	movl	$14, %edx	#,
	movl	$1, %esi	#,
	leaq	.LC0(%rip), %rax	#, tmp100
	movq	%rax, %rdi	# tmp100,
	call	fwrite@PLT	#
	movq	stderr(%rip), %rax	# stderr, stderr.261_10
	movq	%rax, %rcx	# stderr.261_10,
	movl	$28, %edx	#,
	movl	$1, %esi	#,
	leaq	.LC80(%rip), %rax	#, tmp101
	movq	%rax, %rdi	# tmp101,
	call	fwrite@PLT	#
	movq	stderr(%rip), %rax	# stderr, stderr.262_11
	movq	%rax, %rsi	# stderr.262_11,
	movl	$10, %edi	#,
	call	fputc@PLT	#
	movq	stderr(%rip), %rax	# stderr, stderr.263_12
	movq	%rax, %rdi	# stderr.263_12,
	call	fflush@PLT	#
# src/Runtime/runtime.c:540:     exit(1);
	movl	$1, %edi	#,
	call	exit@PLT	#
	.cfi_endproc
.LFE42:
	.size	error, .-error
	.section	.rodata
.LC81:
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
# src/Runtime/runtime.c:546:     char *line = NULL;
	movq	$0, -24(%rbp)	#, line
# src/Runtime/runtime.c:547:     size_t size = 0;
	movq	$0, -32(%rbp)	#, size
# src/Runtime/runtime.c:548:     ssize_t unused = getline(&line, &size, stdin);
	movq	stdin(%rip), %rdx	# stdin, stdin.264_1
	leaq	-32(%rbp), %rcx	#, tmp87
	leaq	-24(%rbp), %rax	#, tmp88
	movq	%rcx, %rsi	# tmp87,
	movq	%rax, %rdi	# tmp88,
	call	getline@PLT	#
	movq	%rax, -8(%rbp)	# tmp89, unused
# src/Runtime/runtime.c:549:     int unused2 = sscanf(line, "%d ", &i);
	movq	-24(%rbp), %rax	# line, line.265_2
	leaq	-16(%rbp), %rdx	#, tmp90
	leaq	.LC81(%rip), %rcx	#, tmp91
	movq	%rcx, %rsi	# tmp91,
	movq	%rax, %rdi	# line.265_2,
	movl	$0, %eax	#,
	call	__isoc99_sscanf@PLT	#
	movl	%eax, -12(%rbp)	# tmp92, unused2
# src/Runtime/runtime.c:550:     free(line);
	movq	-24(%rbp), %rax	# line, line.266_3
	movq	%rax, %rdi	# line.266_3,
	call	free@PLT	#
# src/Runtime/runtime.c:551:     return i;
	movl	-16(%rbp), %eax	# i, _12
# src/Runtime/runtime.c:552: }
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
# src/Runtime/runtime.c:554:     char *line = NULL;
	movq	$0, -24(%rbp)	#, line
# src/Runtime/runtime.c:555:     size_t size = 0;
	movq	$0, -32(%rbp)	#, size
# src/Runtime/runtime.c:556:     ssize_t unused = getline(&line, &size, stdin);
	movq	stdin(%rip), %rdx	# stdin, stdin.267_1
	leaq	-32(%rbp), %rcx	#, tmp93
	leaq	-24(%rbp), %rax	#, tmp94
	movq	%rcx, %rsi	# tmp93,
	movq	%rax, %rdi	# tmp94,
	call	getline@PLT	#
	movq	%rax, -8(%rbp)	# tmp95, unused
# src/Runtime/runtime.c:557:     size = u8_strlen(line);
	movq	-24(%rbp), %rax	# line, line.268_2
	movq	%rax, %rdi	# line.268_2,
	call	u8_strlen@PLT	#
# src/Runtime/runtime.c:557:     size = u8_strlen(line);
	movq	%rax, -32(%rbp)	# _3, size
# src/Runtime/runtime.c:558:     line[size - 1] = 0; // remove newline
	movq	-24(%rbp), %rax	# line, line.269_4
	movq	-32(%rbp), %rdx	# size, size.270_5
	subq	$1, %rdx	#, _6
	addq	%rdx, %rax	# _6, _7
# src/Runtime/runtime.c:558:     line[size - 1] = 0; // remove newline
	movb	$0, (%rax)	#, *_7
# src/Runtime/runtime.c:559:     obj l = __createString(line);
	movq	-24(%rbp), %rax	# line, line.271_8
	movq	%rax, %rdi	# line.271_8,
	call	__createString	#
	movq	%rax, -16(%rbp)	# tmp96, l
# src/Runtime/runtime.c:560:     __incRef(l);
	movq	-16(%rbp), %rax	# l, tmp97
	movq	%rax, %rdi	# tmp97,
	call	__incRef	#
# src/Runtime/runtime.c:561:     free(line);
	movq	-24(%rbp), %rax	# line, line.272_9
	movq	%rax, %rdi	# line.272_9,
	call	free@PLT	#
# src/Runtime/runtime.c:562:     return l;
	movq	-16(%rbp), %rax	# l, _21
# src/Runtime/runtime.c:563: }
	leave	
	.cfi_def_cfa 7, 8
	ret	
	.cfi_endproc
.LFE44:
	.size	readString, .-readString
	.ident	"GCC: (Debian 12.2.0-14) 12.2.0"
	.section	.note.GNU-stack,"",@progbits
