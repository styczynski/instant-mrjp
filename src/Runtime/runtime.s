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
	.globl	__incRef
	.type	__incRef, @function
__incRef:
.LFB6:
	.cfi_startproc
	pushq	%rbp	#
	.cfi_def_cfa_offset 16
	.cfi_offset 6, -16
	movq	%rsp, %rbp	#,
	.cfi_def_cfa_register 6
	movq	%rdi, -8(%rbp)	# r, r
# src/Runtime/runtime.c:34:     if (!IS_NULL(r)) {
	leaq	_LAT_NULL(%rip), %rax	#, tmp84
	cmpq	%rax, -8(%rbp)	# tmp84, r
	je	.L1	#,
# src/Runtime/runtime.c:35:         r->counter++;
	movq	-8(%rbp), %rax	# r, tmp85
	movl	16(%rax), %eax	# r_4(D)->counter, _1
# src/Runtime/runtime.c:35:         r->counter++;
	leal	1(%rax), %edx	#, _2
	movq	-8(%rbp), %rax	# r, tmp86
	movl	%edx, 16(%rax)	# _2, r_4(D)->counter
.L1:
# src/Runtime/runtime.c:38: }
	popq	%rbp	#
	.cfi_def_cfa 7, 8
	ret	
	.cfi_endproc
.LFE6:
	.size	__incRef, .-__incRef
	.local	gc_level
	.comm	gc_level,4,4
	.local	gc_visited
	.comm	gc_visited,8000000,32
	.local	gc_visited_length
	.comm	gc_visited_length,4,4
	.globl	__newRefArray
	.type	__newRefArray, @function
__newRefArray:
.LFB7:
	.cfi_startproc
	pushq	%rbp	#
	.cfi_def_cfa_offset 16
	.cfi_offset 6, -16
	movq	%rsp, %rbp	#,
	.cfi_def_cfa_register 6
	subq	$16, %rsp	#,
	movl	%edi, -4(%rbp)	# length, length
# src/Runtime/runtime.c:45:     return __newArray(sizeof(obj), length, true);
	movl	-4(%rbp), %eax	# length, tmp84
	movl	$1, %edx	#,
	movl	%eax, %esi	# tmp84,
	movl	$8, %edi	#,
	call	__newArray	#
# src/Runtime/runtime.c:46: }
	leave	
	.cfi_def_cfa 7, 8
	ret	
	.cfi_endproc
.LFE7:
	.size	__newRefArray, .-__newRefArray
	.globl	__newIntArray
	.type	__newIntArray, @function
__newIntArray:
.LFB8:
	.cfi_startproc
	pushq	%rbp	#
	.cfi_def_cfa_offset 16
	.cfi_offset 6, -16
	movq	%rsp, %rbp	#,
	.cfi_def_cfa_register 6
	subq	$16, %rsp	#,
	movl	%edi, -4(%rbp)	# length, length
# src/Runtime/runtime.c:48:     return __newArray(sizeof(int32_t), length, false);
	movl	-4(%rbp), %eax	# length, tmp84
	movl	$0, %edx	#,
	movl	%eax, %esi	# tmp84,
	movl	$4, %edi	#,
	call	__newArray	#
# src/Runtime/runtime.c:49: }
	leave	
	.cfi_def_cfa 7, 8
	ret	
	.cfi_endproc
.LFE8:
	.size	__newIntArray, .-__newIntArray
	.globl	__newByteArray
	.type	__newByteArray, @function
__newByteArray:
.LFB9:
	.cfi_startproc
	pushq	%rbp	#
	.cfi_def_cfa_offset 16
	.cfi_offset 6, -16
	movq	%rsp, %rbp	#,
	.cfi_def_cfa_register 6
	subq	$16, %rsp	#,
	movl	%edi, -4(%rbp)	# length, length
# src/Runtime/runtime.c:51:     return __newArray(sizeof(int8_t), length, false);
	movl	-4(%rbp), %eax	# length, tmp84
	movl	$0, %edx	#,
	movl	%eax, %esi	# tmp84,
	movl	$1, %edi	#,
	call	__newArray	#
# src/Runtime/runtime.c:52: }
	leave	
	.cfi_def_cfa 7, 8
	ret	
	.cfi_endproc
.LFE9:
	.size	__newByteArray, .-__newByteArray
	.globl	__newArray
	.type	__newArray, @function
__newArray:
.LFB10:
	.cfi_startproc
	pushq	%rbp	#
	.cfi_def_cfa_offset 16
	.cfi_offset 6, -16
	movq	%rsp, %rbp	#,
	.cfi_def_cfa_register 6
	subq	$48, %rsp	#,
	movl	%edi, -36(%rbp)	# size, size
	movl	%esi, -40(%rbp)	# length, length
	movl	%edx, %eax	# use_null_initializer, tmp91
	movb	%al, -44(%rbp)	# tmp92, use_null_initializer
# src/Runtime/runtime.c:54:     obj r = __new(&_class_Array);
	leaq	_class_Array(%rip), %rax	#, tmp93
	movq	%rax, %rdi	# tmp93,
	call	__new	#
	movq	%rax, -16(%rbp)	# tmp94, r
# src/Runtime/runtime.c:55:     void* arr = malloc(size * length);
	movl	-36(%rbp), %eax	# size, tmp95
	imull	-40(%rbp), %eax	# length, _1
# src/Runtime/runtime.c:55:     void* arr = malloc(size * length);
	cltq
	movq	%rax, %rdi	# _2,
	call	malloc@PLT	#
	movq	%rax, -24(%rbp)	# tmp96, arr
# src/Runtime/runtime.c:56:     r->data = arr;
	movq	-16(%rbp), %rax	# r, tmp97
	movq	-24(%rbp), %rdx	# arr, tmp98
	movq	%rdx, 8(%rax)	# tmp98, r_13->data
# src/Runtime/runtime.c:57:     r->elementSize = size;
	movq	-16(%rbp), %rax	# r, tmp99
	movl	-36(%rbp), %edx	# size, tmp100
	movl	%edx, 28(%rax)	# tmp100, r_13->elementSize
# src/Runtime/runtime.c:58:     r->length = length;
	movq	-16(%rbp), %rax	# r, tmp101
	movl	-40(%rbp), %edx	# length, tmp102
	movl	%edx, 32(%rax)	# tmp102, r_13->length
# src/Runtime/runtime.c:59:     if (length > 0) {
	cmpl	$0, -40(%rbp)	#, length
	jle	.L10	#,
# src/Runtime/runtime.c:60:         if (use_null_initializer) {
	cmpb	$0, -44(%rbp)	#, use_null_initializer
	je	.L11	#,
# src/Runtime/runtime.c:61:             obj* arr_obj = (obj*) arr;
	movq	-24(%rbp), %rax	# arr, tmp103
	movq	%rax, -32(%rbp)	# tmp103, arr_obj
# src/Runtime/runtime.c:62:             for (int i=0; i<length; ++i) {
	movl	$0, -4(%rbp)	#, i
# src/Runtime/runtime.c:62:             for (int i=0; i<length; ++i) {
	jmp	.L12	#
.L13:
# src/Runtime/runtime.c:63:                 arr_obj[i] = VAL_NULL;
	movl	-4(%rbp), %eax	# i, tmp104
	cltq
	leaq	0(,%rax,8), %rdx	#, _4
	movq	-32(%rbp), %rax	# arr_obj, tmp105
	addq	%rdx, %rax	# _4, _5
# src/Runtime/runtime.c:63:                 arr_obj[i] = VAL_NULL;
	leaq	_LAT_NULL(%rip), %rdx	#, tmp106
	movq	%rdx, (%rax)	# tmp106, *_5
# src/Runtime/runtime.c:62:             for (int i=0; i<length; ++i) {
	addl	$1, -4(%rbp)	#, i
.L12:
# src/Runtime/runtime.c:62:             for (int i=0; i<length; ++i) {
	movl	-4(%rbp), %eax	# i, tmp107
	cmpl	-40(%rbp), %eax	# length, tmp107
	jl	.L13	#,
	jmp	.L10	#
.L11:
# src/Runtime/runtime.c:66:             bzero(arr, size * length);
	movl	-36(%rbp), %eax	# size, tmp108
	imull	-40(%rbp), %eax	# length, _6
# src/Runtime/runtime.c:66:             bzero(arr, size * length);
	cltq
	movq	-24(%rbp), %rdx	# arr, tmp109
	movq	%rdx, %rcx	# tmp109, tmp110
	movq	%rax, %rdx	# tmp111,
	movl	$0, %esi	#,
	movq	%rcx, %rdi	# tmp110,
	call	memset@PLT	#
.L10:
# src/Runtime/runtime.c:69:     return r;
	movq	-16(%rbp), %rax	# r, _27
# src/Runtime/runtime.c:70: }
	leave	
	.cfi_def_cfa 7, 8
	ret	
	.cfi_endproc
.LFE10:
	.size	__newArray, .-__newArray
	.globl	__getelementptr
	.type	__getelementptr, @function
__getelementptr:
.LFB11:
	.cfi_startproc
	pushq	%rbp	#
	.cfi_def_cfa_offset 16
	.cfi_offset 6, -16
	movq	%rsp, %rbp	#,
	.cfi_def_cfa_register 6
	movq	%rdi, -8(%rbp)	# array, array
	movl	%esi, -12(%rbp)	# index, index
# src/Runtime/runtime.c:84:     return NULL;
	movl	$0, %eax	#, _1
# src/Runtime/runtime.c:85: }
	popq	%rbp	#
	.cfi_def_cfa 7, 8
	ret	
	.cfi_endproc
.LFE11:
	.size	__getelementptr, .-__getelementptr
	.globl	__cast
	.type	__cast, @function
__cast:
.LFB12:
	.cfi_startproc
	pushq	%rbp	#
	.cfi_def_cfa_offset 16
	.cfi_offset 6, -16
	movq	%rsp, %rbp	#,
	.cfi_def_cfa_register 6
	movq	%rdi, -24(%rbp)	# o, o
	movq	%rsi, -32(%rbp)	# t, t
# src/Runtime/runtime.c:90:     if (IS_NULL(o)) {
	leaq	_LAT_NULL(%rip), %rax	#, tmp84
	cmpq	%rax, -24(%rbp)	# tmp84, o
	jne	.L18	#,
# src/Runtime/runtime.c:92:         return VAL_NULL;
	leaq	_LAT_NULL(%rip), %rax	#, _3
	jmp	.L19	#
.L18:
# src/Runtime/runtime.c:95:     struct Type *to = o->type;
	movq	-24(%rbp), %rax	# o, tmp85
	movq	(%rax), %rax	# o_10(D)->type, tmp86
	movq	%rax, -8(%rbp)	# tmp86, to
# src/Runtime/runtime.c:96:     while (to != NULL) {
	jmp	.L20	#
.L23:
# src/Runtime/runtime.c:98:         if (t == to) {
	movq	-32(%rbp), %rax	# t, tmp87
	cmpq	-8(%rbp), %rax	# to, tmp87
	jne	.L21	#,
# src/Runtime/runtime.c:100:             return o;
	movq	-24(%rbp), %rax	# o, _3
	jmp	.L19	#
.L21:
# src/Runtime/runtime.c:102:         struct Type *prev = to;
	movq	-8(%rbp), %rax	# to, tmp88
	movq	%rax, -16(%rbp)	# tmp88, prev
# src/Runtime/runtime.c:103:         to = to->parent;
	movq	-8(%rbp), %rax	# to, tmp89
	movq	(%rax), %rax	# to_8->parent, tmp90
	movq	%rax, -8(%rbp)	# tmp90, to
# src/Runtime/runtime.c:104:         if (prev == to) {
	movq	-16(%rbp), %rax	# prev, tmp91
	cmpq	-8(%rbp), %rax	# to, tmp91
	je	.L25	#,
.L20:
# src/Runtime/runtime.c:96:     while (to != NULL) {
	cmpq	$0, -8(%rbp)	#, to
	jne	.L23	#,
	jmp	.L24	#
.L25:
# src/Runtime/runtime.c:106:             break;
	nop	
.L24:
# src/Runtime/runtime.c:110:     return VAL_NULL;
	leaq	_LAT_NULL(%rip), %rax	#, _3
.L19:
# src/Runtime/runtime.c:111: }
	popq	%rbp	#
	.cfi_def_cfa 7, 8
	ret	
	.cfi_endproc
.LFE12:
	.size	__cast, .-__cast
	.section	.rodata
	.align 8
.LC0:
	.string	"ERROR: Null pointer reference."
	.text
	.globl	__errorNull
	.type	__errorNull, @function
__errorNull:
.LFB13:
	.cfi_startproc
	pushq	%rbp	#
	.cfi_def_cfa_offset 16
	.cfi_offset 6, -16
	movq	%rsp, %rbp	#,
	.cfi_def_cfa_register 6
# src/Runtime/runtime.c:114:     errMsg = "ERROR: Null pointer reference.";
	leaq	.LC0(%rip), %rax	#, tmp82
	movq	%rax, errMsg(%rip)	# tmp82, errMsg
# src/Runtime/runtime.c:115:     error();
	movl	$0, %eax	#,
	call	error	#
# src/Runtime/runtime.c:116: }
	nop	
	popq	%rbp	#
	.cfi_def_cfa 7, 8
	ret	
	.cfi_endproc
.LFE13:
	.size	__errorNull, .-__errorNull
	.section	.rodata
	.align 8
.LC1:
	.string	"ERROR: Non-unicode string encoding."
	.text
	.globl	__createString
	.type	__createString, @function
__createString:
.LFB14:
	.cfi_startproc
	pushq	%rbp	#
	.cfi_def_cfa_offset 16
	.cfi_offset 6, -16
	movq	%rsp, %rbp	#,
	.cfi_def_cfa_register 6
	subq	$48, %rsp	#,
	movq	%rdi, -40(%rbp)	# c, c
# src/Runtime/runtime.c:121:     if (IS_NULL(c)) {
	leaq	_LAT_NULL(%rip), %rax	#, tmp97
	cmpq	%rax, -40(%rbp)	# tmp97, c
	jne	.L28	#,
# src/Runtime/runtime.c:123:         return __createString(emptyString);
	leaq	emptyString(%rip), %rax	#, tmp98
	movq	%rax, %rdi	# tmp98,
	call	__createString	#
	jmp	.L29	#
.L28:
# src/Runtime/runtime.c:126:     obj r = __new(&_class_String);
	leaq	_class_String(%rip), %rax	#, tmp99
	movq	%rax, %rdi	# tmp99,
	call	__new	#
	movq	%rax, -16(%rbp)	# tmp100, r
# src/Runtime/runtime.c:129:     r->length = u8_strlen(c);
	movq	-40(%rbp), %rax	# c, tmp101
	movq	%rax, %rdi	# tmp101,
	call	u8_strlen@PLT	#
# src/Runtime/runtime.c:129:     r->length = u8_strlen(c);
	movl	%eax, %edx	# _3, _4
	movq	-16(%rbp), %rax	# r, tmp102
	movl	%edx, 32(%rax)	# _4, r_15->length
# src/Runtime/runtime.c:131:     uint8_t* invalid_unit = u8_check(c, r->length);
	movq	-16(%rbp), %rax	# r, tmp103
	movl	32(%rax), %eax	# r_15->length, _10
# src/Runtime/runtime.c:131:     uint8_t* invalid_unit = u8_check(c, r->length);
	movslq	%eax, %rdx	# _10, _11
	movq	-40(%rbp), %rax	# c, tmp104
	movq	%rdx, %rsi	# _11,
	movq	%rax, %rdi	# tmp104,
	call	u8_check@PLT	#
	movq	%rax, -24(%rbp)	# tmp105, invalid_unit
# src/Runtime/runtime.c:132:     if (u8_check(c, r->length) != NULL) {
	movq	-16(%rbp), %rax	# r, tmp106
	movl	32(%rax), %eax	# r_15->length, _12
# src/Runtime/runtime.c:132:     if (u8_check(c, r->length) != NULL) {
	movslq	%eax, %rdx	# _12, _13
	movq	-40(%rbp), %rax	# c, tmp107
	movq	%rdx, %rsi	# _13,
	movq	%rax, %rdi	# tmp107,
	call	u8_check@PLT	#
# src/Runtime/runtime.c:132:     if (u8_check(c, r->length) != NULL) {
	testq	%rax, %rax	# _14
	je	.L30	#,
# src/Runtime/runtime.c:134:         errMsg = "ERROR: Non-unicode string encoding.";
	leaq	.LC1(%rip), %rax	#, tmp108
	movq	%rax, errMsg(%rip)	# tmp108, errMsg
# src/Runtime/runtime.c:135:         error();
	movl	$0, %eax	#,
	call	error	#
.L30:
# src/Runtime/runtime.c:137:     if (r->length > 0) {
	movq	-16(%rbp), %rax	# r, tmp109
	movl	32(%rax), %eax	# r_15->length, _23
# src/Runtime/runtime.c:137:     if (r->length > 0) {
	testl	%eax, %eax	# _23
	jle	.L31	#,
# src/Runtime/runtime.c:138:         int len = r->length;
	movq	-16(%rbp), %rax	# r, tmp110
	movl	32(%rax), %eax	# r_15->length, tmp111
	movl	%eax, -28(%rbp)	# tmp111, len
# src/Runtime/runtime.c:139:         uint8_t *str = malloc(len + 1);
	movl	-28(%rbp), %eax	# len, tmp112
	addl	$1, %eax	#, _24
# src/Runtime/runtime.c:139:         uint8_t *str = malloc(len + 1);
	cltq
	movq	%rax, %rdi	# _25,
	call	malloc@PLT	#
	movq	%rax, -8(%rbp)	# tmp113, str
# src/Runtime/runtime.c:140:         r->data = str;
	movq	-16(%rbp), %rax	# r, tmp114
	movq	-8(%rbp), %rdx	# str, tmp115
	movq	%rdx, 8(%rax)	# tmp115, r_15->data
# src/Runtime/runtime.c:141:         memcpy(str, c, len);
	movl	-28(%rbp), %eax	# len, tmp116
	movslq	%eax, %rdx	# tmp116, _26
	movq	-40(%rbp), %rcx	# c, tmp117
	movq	-8(%rbp), %rax	# str, tmp118
	movq	%rcx, %rsi	# tmp117,
	movq	%rax, %rdi	# tmp118,
	call	memcpy@PLT	#
# src/Runtime/runtime.c:142:         str[len] = 0;
	movl	-28(%rbp), %eax	# len, tmp119
	movslq	%eax, %rdx	# tmp119, _27
	movq	-8(%rbp), %rax	# str, tmp120
	addq	%rdx, %rax	# _27, _28
# src/Runtime/runtime.c:142:         str[len] = 0;
	movb	$0, (%rax)	#, *_28
	jmp	.L32	#
.L31:
# src/Runtime/runtime.c:144:         r->data = emptyString;
	movq	-16(%rbp), %rax	# r, tmp121
	leaq	emptyString(%rip), %rdx	#, tmp122
	movq	%rdx, 8(%rax)	# tmp122, r_15->data
.L32:
# src/Runtime/runtime.c:147:     return r;
	movq	-16(%rbp), %rax	# r, _2
.L29:
# src/Runtime/runtime.c:148: }
	leave	
	.cfi_def_cfa 7, 8
	ret	
	.cfi_endproc
.LFE14:
	.size	__createString, .-__createString
	.local	type_info_string_repr_level
	.comm	type_info_string_repr_level,4,4
	.local	type_info_string_repr_visited
	.comm	type_info_string_repr_visited,8000000,32
	.local	type_info_string_repr_visited_length
	.comm	type_info_string_repr_visited_length,4,4
	.section	.rodata
.LC2:
	.string	"null"
.LC3:
	.string	"\""
.LC4:
	.string	"<recursive>"
.LC5:
	.string	"{"
.LC6:
	.string	": "
.LC7:
	.string	"%d"
.LC8:
	.string	"true"
.LC9:
	.string	"false"
.LC10:
	.string	", "
.LC11:
	.string	"}"
	.text
	.globl	_typeInfoStringRepr
	.type	_typeInfoStringRepr, @function
_typeInfoStringRepr:
.LFB15:
	.cfi_startproc
	pushq	%rbp	#
	.cfi_def_cfa_offset 16
	.cfi_offset 6, -16
	movq	%rsp, %rbp	#,
	.cfi_def_cfa_register 6
	pushq	%rbx	#
	subq	$152, %rsp	#,
	.cfi_offset 3, -24
	movq	%rdi, -152(%rbp)	# o, o
# src/Runtime/runtime.c:155:     if (IS_NULL(o)) {
	leaq	_LAT_NULL(%rip), %rax	#, tmp146
	cmpq	%rax, -152(%rbp)	# tmp146, o
	jne	.L34	#,
# src/Runtime/runtime.c:156:         return __createString("null");
	leaq	.LC2(%rip), %rax	#, tmp147
	movq	%rax, %rdi	# tmp147,
	call	__createString	#
	jmp	.L35	#
.L34:
# src/Runtime/runtime.c:158:     if (o->type == &_class_Array) {
	movq	-152(%rbp), %rax	# o, tmp148
	movq	(%rax), %rdx	# o_81(D)->type, _1
# src/Runtime/runtime.c:158:     if (o->type == &_class_Array) {
	leaq	_class_Array(%rip), %rax	#, tmp149
	cmpq	%rax, %rdx	# tmp149, _1
	jne	.L36	#,
# src/Runtime/runtime.c:159:         return _Array_toString(o);
	movq	-152(%rbp), %rax	# o, tmp150
	movq	%rax, %rdi	# tmp150,
	call	_Array_toString	#
	jmp	.L35	#
.L36:
# src/Runtime/runtime.c:161:     if (o->type == &_class_String) {
	movq	-152(%rbp), %rax	# o, tmp151
	movq	(%rax), %rdx	# o_81(D)->type, _2
# src/Runtime/runtime.c:161:     if (o->type == &_class_String) {
	leaq	_class_String(%rip), %rax	#, tmp152
	cmpq	%rax, %rdx	# tmp152, _2
	jne	.L37	#,
# src/Runtime/runtime.c:162:         return _String_concat(__createString("\""), _String_concat(o, __createString("\"")));
	leaq	.LC3(%rip), %rax	#, tmp153
	movq	%rax, %rdi	# tmp153,
	call	__createString	#
	movq	%rax, %rdx	#, _3
	movq	-152(%rbp), %rax	# o, tmp154
	movq	%rdx, %rsi	# _3,
	movq	%rax, %rdi	# tmp154,
	call	_String_concat	#
	movq	%rax, %rbx	#, _4
	leaq	.LC3(%rip), %rax	#, tmp155
	movq	%rax, %rdi	# tmp155,
	call	__createString	#
	movq	%rbx, %rsi	# _4,
	movq	%rax, %rdi	# _5,
	call	_String_concat	#
	jmp	.L35	#
.L37:
# src/Runtime/runtime.c:164:     for (int i=0;i<type_info_string_repr_visited_length;++i) {
	movl	$0, -20(%rbp)	#, i
# src/Runtime/runtime.c:164:     for (int i=0;i<type_info_string_repr_visited_length;++i) {
	jmp	.L38	#
.L40:
# src/Runtime/runtime.c:165:         if (((uint64_t)o) == type_info_string_repr_visited[i]) {
	movl	-20(%rbp), %eax	# i, tmp157
	cltq
	leaq	0(,%rax,8), %rdx	#, tmp158
	leaq	type_info_string_repr_visited(%rip), %rax	#, tmp159
	movq	(%rdx,%rax), %rdx	# type_info_string_repr_visited[i_64], _6
# src/Runtime/runtime.c:165:         if (((uint64_t)o) == type_info_string_repr_visited[i]) {
	movq	-152(%rbp), %rax	# o, o.74_7
# src/Runtime/runtime.c:165:         if (((uint64_t)o) == type_info_string_repr_visited[i]) {
	cmpq	%rax, %rdx	# o.74_7, _6
	jne	.L39	#,
# src/Runtime/runtime.c:167:             return __createString("<recursive>");
	leaq	.LC4(%rip), %rax	#, tmp160
	movq	%rax, %rdi	# tmp160,
	call	__createString	#
	jmp	.L35	#
.L39:
# src/Runtime/runtime.c:164:     for (int i=0;i<type_info_string_repr_visited_length;++i) {
	addl	$1, -20(%rbp)	#, i
.L38:
# src/Runtime/runtime.c:164:     for (int i=0;i<type_info_string_repr_visited_length;++i) {
	movl	-20(%rbp), %edx	# i, i.75_8
	movl	type_info_string_repr_visited_length(%rip), %eax	# type_info_string_repr_visited_length, type_info_string_repr_visited_length.76_9
	cmpl	%eax, %edx	# type_info_string_repr_visited_length.76_9, i.75_8
	jb	.L40	#,
# src/Runtime/runtime.c:170:     type_info_string_repr_visited[type_info_string_repr_visited_length++] = (uint64_t)o;
	movl	type_info_string_repr_visited_length(%rip), %eax	# type_info_string_repr_visited_length, type_info_string_repr_visited_length.77_10
	leal	1(%rax), %edx	#, _12
	movl	%edx, type_info_string_repr_visited_length(%rip)	# _12, type_info_string_repr_visited_length
# src/Runtime/runtime.c:170:     type_info_string_repr_visited[type_info_string_repr_visited_length++] = (uint64_t)o;
	movq	-152(%rbp), %rdx	# o, o.79_13
# src/Runtime/runtime.c:170:     type_info_string_repr_visited[type_info_string_repr_visited_length++] = (uint64_t)o;
	movl	%eax, %eax	# type_info_string_repr_visited_length.77_10, tmp161
	leaq	0(,%rax,8), %rcx	#, tmp162
	leaq	type_info_string_repr_visited(%rip), %rax	#, tmp163
	movq	%rdx, (%rcx,%rax)	# o.79_13, type_info_string_repr_visited[type_info_string_repr_visited_length.78_11]
# src/Runtime/runtime.c:172:     uint8_t *name_buffer = malloc(200);
	movl	$200, %edi	#,
	call	malloc@PLT	#
	movq	%rax, -48(%rbp)	# tmp164, name_buffer
# src/Runtime/runtime.c:173:     name_buffer[0] = '\0';
	movq	-48(%rbp), %rax	# name_buffer, tmp165
	movb	$0, (%rax)	#, *name_buffer_87
# src/Runtime/runtime.c:174:     u8_strcat(name_buffer, o->type->typeName);
	movq	-152(%rbp), %rax	# o, tmp166
	movq	(%rax), %rax	# o_81(D)->type, _14
# src/Runtime/runtime.c:174:     u8_strcat(name_buffer, o->type->typeName);
	movq	68(%rax), %rdx	# _14->typeName, _15
# src/Runtime/runtime.c:174:     u8_strcat(name_buffer, o->type->typeName);
	movq	-48(%rbp), %rax	# name_buffer, tmp167
	movq	%rdx, %rsi	# _15,
	movq	%rax, %rdi	# tmp167,
	call	u8_strcat@PLT	#
# src/Runtime/runtime.c:175:     u8_strcat(name_buffer, "{");
	movq	-48(%rbp), %rax	# name_buffer, tmp168
	leaq	.LC5(%rip), %rdx	#, tmp169
	movq	%rdx, %rsi	# tmp169,
	movq	%rax, %rdi	# tmp168,
	call	u8_strcat@PLT	#
# src/Runtime/runtime.c:176:     obj result = __createString(name_buffer);
	movq	-48(%rbp), %rax	# name_buffer, tmp170
	movq	%rax, %rdi	# tmp170,
	call	__createString	#
	movq	%rax, -32(%rbp)	# tmp171, result
# src/Runtime/runtime.c:177:     free(name_buffer);
	movq	-48(%rbp), %rax	# name_buffer, tmp172
	movq	%rax, %rdi	# tmp172,
	call	free@PLT	#
# src/Runtime/runtime.c:179:     const int fieldsInfoLength = o->type->fieldsInfoLength;
	movq	-152(%rbp), %rax	# o, tmp173
	movq	(%rax), %rax	# o_81(D)->type, _16
# src/Runtime/runtime.c:179:     const int fieldsInfoLength = o->type->fieldsInfoLength;
	movl	64(%rax), %eax	# _16->fieldsInfoLength, tmp174
	movl	%eax, -52(%rbp)	# tmp174, fieldsInfoLength
# src/Runtime/runtime.c:180:     for(int i=0;i<fieldsInfoLength;++i) {
	movl	$0, -36(%rbp)	#, i
# src/Runtime/runtime.c:180:     for(int i=0;i<fieldsInfoLength;++i) {
	jmp	.L41	#
.L58:
# src/Runtime/runtime.c:181:         char* fieldInfo = (char*) (((uint64_t)o->type->fieldsInfo) + ((uint64_t)o->type->fieldsInfoOffsets[i]));
	movq	-152(%rbp), %rax	# o, tmp175
	movq	(%rax), %rax	# o_81(D)->type, _17
# src/Runtime/runtime.c:181:         char* fieldInfo = (char*) (((uint64_t)o->type->fieldsInfo) + ((uint64_t)o->type->fieldsInfoOffsets[i]));
	movq	40(%rax), %rax	# _17->fieldsInfo, _18
# src/Runtime/runtime.c:181:         char* fieldInfo = (char*) (((uint64_t)o->type->fieldsInfo) + ((uint64_t)o->type->fieldsInfoOffsets[i]));
	movq	%rax, %rcx	# _18, _19
# src/Runtime/runtime.c:181:         char* fieldInfo = (char*) (((uint64_t)o->type->fieldsInfo) + ((uint64_t)o->type->fieldsInfoOffsets[i]));
	movq	-152(%rbp), %rax	# o, tmp176
	movq	(%rax), %rax	# o_81(D)->type, _20
# src/Runtime/runtime.c:181:         char* fieldInfo = (char*) (((uint64_t)o->type->fieldsInfo) + ((uint64_t)o->type->fieldsInfoOffsets[i]));
	movq	48(%rax), %rax	# _20->fieldsInfoOffsets, _21
# src/Runtime/runtime.c:181:         char* fieldInfo = (char*) (((uint64_t)o->type->fieldsInfo) + ((uint64_t)o->type->fieldsInfoOffsets[i]));
	movl	-36(%rbp), %edx	# i, tmp177
	movslq	%edx, %rdx	# tmp177, _22
	salq	$2, %rdx	#, _23
	addq	%rdx, %rax	# _23, _24
	movl	(%rax), %eax	# *_24, _25
# src/Runtime/runtime.c:181:         char* fieldInfo = (char*) (((uint64_t)o->type->fieldsInfo) + ((uint64_t)o->type->fieldsInfoOffsets[i]));
	cltq
# src/Runtime/runtime.c:181:         char* fieldInfo = (char*) (((uint64_t)o->type->fieldsInfo) + ((uint64_t)o->type->fieldsInfoOffsets[i]));
	addq	%rcx, %rax	# _19, _27
# src/Runtime/runtime.c:181:         char* fieldInfo = (char*) (((uint64_t)o->type->fieldsInfo) + ((uint64_t)o->type->fieldsInfoOffsets[i]));
	movq	%rax, -64(%rbp)	# _27, fieldInfo
# src/Runtime/runtime.c:182:         char type = fieldInfo[0];
	movq	-64(%rbp), %rax	# fieldInfo, tmp178
	movzbl	(%rax), %eax	# *fieldInfo_100, tmp179
	movb	%al, -65(%rbp)	# tmp179, type
# src/Runtime/runtime.c:183:         char* name = fieldInfo+1;
	movq	-64(%rbp), %rax	# fieldInfo, tmp183
	addq	$1, %rax	#, tmp182
	movq	%rax, -80(%rbp)	# tmp182, name
# src/Runtime/runtime.c:184:         obj* fieldValue = (obj*)(((uint64_t)(&o->others)) + ((uint64_t)(o->type->fieldsDataOffsets[i])));
	movq	-152(%rbp), %rax	# o, tmp184
	addq	$36, %rax	#, _28
# src/Runtime/runtime.c:184:         obj* fieldValue = (obj*)(((uint64_t)(&o->others)) + ((uint64_t)(o->type->fieldsDataOffsets[i])));
	movq	%rax, %rcx	# _28, _29
# src/Runtime/runtime.c:184:         obj* fieldValue = (obj*)(((uint64_t)(&o->others)) + ((uint64_t)(o->type->fieldsDataOffsets[i])));
	movq	-152(%rbp), %rax	# o, tmp185
	movq	(%rax), %rax	# o_81(D)->type, _30
# src/Runtime/runtime.c:184:         obj* fieldValue = (obj*)(((uint64_t)(&o->others)) + ((uint64_t)(o->type->fieldsDataOffsets[i])));
	movq	56(%rax), %rax	# _30->fieldsDataOffsets, _31
# src/Runtime/runtime.c:184:         obj* fieldValue = (obj*)(((uint64_t)(&o->others)) + ((uint64_t)(o->type->fieldsDataOffsets[i])));
	movl	-36(%rbp), %edx	# i, tmp186
	movslq	%edx, %rdx	# tmp186, _32
	salq	$2, %rdx	#, _33
	addq	%rdx, %rax	# _33, _34
	movl	(%rax), %eax	# *_34, _35
# src/Runtime/runtime.c:184:         obj* fieldValue = (obj*)(((uint64_t)(&o->others)) + ((uint64_t)(o->type->fieldsDataOffsets[i])));
	cltq
# src/Runtime/runtime.c:184:         obj* fieldValue = (obj*)(((uint64_t)(&o->others)) + ((uint64_t)(o->type->fieldsDataOffsets[i])));
	addq	%rcx, %rax	# _29, _37
# src/Runtime/runtime.c:184:         obj* fieldValue = (obj*)(((uint64_t)(&o->others)) + ((uint64_t)(o->type->fieldsDataOffsets[i])));
	movq	%rax, -88(%rbp)	# _37, fieldValue
# src/Runtime/runtime.c:188:         uint8_t *buffer = malloc(200);
	movl	$200, %edi	#,
	call	malloc@PLT	#
	movq	%rax, -96(%rbp)	# tmp187, buffer
# src/Runtime/runtime.c:189:         buffer[0] = '\0';
	movq	-96(%rbp), %rax	# buffer, tmp188
	movb	$0, (%rax)	#, *buffer_105
# src/Runtime/runtime.c:190:         u8_strcat(buffer, name);
	movq	-80(%rbp), %rdx	# name, tmp189
	movq	-96(%rbp), %rax	# buffer, tmp190
	movq	%rdx, %rsi	# tmp189,
	movq	%rax, %rdi	# tmp190,
	call	u8_strcat@PLT	#
# src/Runtime/runtime.c:191:         u8_strcat(buffer, ": ");
	movq	-96(%rbp), %rax	# buffer, tmp191
	leaq	.LC6(%rip), %rdx	#, tmp192
	movq	%rdx, %rsi	# tmp192,
	movq	%rax, %rdi	# tmp191,
	call	u8_strcat@PLT	#
# src/Runtime/runtime.c:192:         result = _String_concat(result, __createString(buffer));
	movq	-96(%rbp), %rax	# buffer, tmp193
	movq	%rax, %rdi	# tmp193,
	call	__createString	#
	movq	%rax, %rdx	#, _38
	movq	-32(%rbp), %rax	# result, tmp194
	movq	%rdx, %rsi	# _38,
	movq	%rax, %rdi	# tmp194,
	call	_String_concat	#
	movq	%rax, -32(%rbp)	# tmp195, result
# src/Runtime/runtime.c:193:         free(buffer);
	movq	-96(%rbp), %rax	# buffer, tmp196
	movq	%rax, %rdi	# tmp196,
	call	free@PLT	#
# src/Runtime/runtime.c:195:         switch(type) {
	movsbl	-65(%rbp), %eax	# type, _39
	subl	$65, %eax	#, tmp197
	cmpl	$18, %eax	#, tmp197
	ja	.L42	#,
	movl	%eax, %eax	# tmp197, tmp198
	leaq	0(,%rax,4), %rdx	#, tmp199
	leaq	.L44(%rip), %rax	#, tmp200
	movl	(%rdx,%rax), %eax	#, tmp201
	cltq
	leaq	.L44(%rip), %rdx	#, tmp204
	addq	%rdx, %rax	# tmp204, tmp203
	jmp	*%rax	# tmp203
	.section	.rodata
	.align 4
	.align 4
.L44:
	.long	.L48-.L44
	.long	.L47-.L44
	.long	.L46-.L44
	.long	.L42-.L44
	.long	.L42-.L44
	.long	.L42-.L44
	.long	.L42-.L44
	.long	.L42-.L44
	.long	.L45-.L44
	.long	.L42-.L44
	.long	.L42-.L44
	.long	.L42-.L44
	.long	.L42-.L44
	.long	.L42-.L44
	.long	.L42-.L44
	.long	.L42-.L44
	.long	.L42-.L44
	.long	.L42-.L44
	.long	.L43-.L44
	.text
.L48:
# src/Runtime/runtime.c:197:                 if (IS_NULL(*fieldValue)) {
	movq	-88(%rbp), %rax	# fieldValue, tmp205
	movq	(%rax), %rdx	# *fieldValue_103, _40
# src/Runtime/runtime.c:197:                 if (IS_NULL(*fieldValue)) {
	leaq	_LAT_NULL(%rip), %rax	#, tmp206
	cmpq	%rax, %rdx	# tmp206, _40
	jne	.L49	#,
# src/Runtime/runtime.c:198:                     result = _String_concat(result, __createString("null"));
	leaq	.LC2(%rip), %rax	#, tmp207
	movq	%rax, %rdi	# tmp207,
	call	__createString	#
	movq	%rax, %rdx	#, _41
	movq	-32(%rbp), %rax	# result, tmp208
	movq	%rdx, %rsi	# _41,
	movq	%rax, %rdi	# tmp208,
	call	_String_concat	#
	movq	%rax, -32(%rbp)	# tmp209, result
# src/Runtime/runtime.c:202:                 break;
	jmp	.L42	#
.L49:
# src/Runtime/runtime.c:200:                     result = _String_concat(result, _Array_toString(*fieldValue));
	movq	-88(%rbp), %rax	# fieldValue, tmp210
	movq	(%rax), %rax	# *fieldValue_103, _42
	movq	%rax, %rdi	# _42,
	call	_Array_toString	#
	movq	%rax, %rdx	#, _43
	movq	-32(%rbp), %rax	# result, tmp211
	movq	%rdx, %rsi	# _43,
	movq	%rax, %rdi	# tmp211,
	call	_String_concat	#
	movq	%rax, -32(%rbp)	# tmp212, result
# src/Runtime/runtime.c:202:                 break;
	jmp	.L42	#
.L43:
# src/Runtime/runtime.c:205:                 if (IS_NULL(*fieldValue)) {
	movq	-88(%rbp), %rax	# fieldValue, tmp213
	movq	(%rax), %rdx	# *fieldValue_103, _44
# src/Runtime/runtime.c:205:                 if (IS_NULL(*fieldValue)) {
	leaq	_LAT_NULL(%rip), %rax	#, tmp214
	cmpq	%rax, %rdx	# tmp214, _44
	jne	.L51	#,
# src/Runtime/runtime.c:206:                     result = _String_concat(result, __createString("null"));
	leaq	.LC2(%rip), %rax	#, tmp215
	movq	%rax, %rdi	# tmp215,
	call	__createString	#
	movq	%rax, %rdx	#, _45
	movq	-32(%rbp), %rax	# result, tmp216
	movq	%rdx, %rsi	# _45,
	movq	%rax, %rdi	# tmp216,
	call	_String_concat	#
	movq	%rax, -32(%rbp)	# tmp217, result
# src/Runtime/runtime.c:212:                 break;
	jmp	.L42	#
.L51:
# src/Runtime/runtime.c:208:                     result = _String_concat(result, __createString("\""));
	leaq	.LC3(%rip), %rax	#, tmp218
	movq	%rax, %rdi	# tmp218,
	call	__createString	#
	movq	%rax, %rdx	#, _46
	movq	-32(%rbp), %rax	# result, tmp219
	movq	%rdx, %rsi	# _46,
	movq	%rax, %rdi	# tmp219,
	call	_String_concat	#
	movq	%rax, -32(%rbp)	# tmp220, result
# src/Runtime/runtime.c:209:                     result = _String_concat(result, *fieldValue);
	movq	-88(%rbp), %rax	# fieldValue, tmp221
	movq	(%rax), %rdx	# *fieldValue_103, _47
	movq	-32(%rbp), %rax	# result, tmp222
	movq	%rdx, %rsi	# _47,
	movq	%rax, %rdi	# tmp222,
	call	_String_concat	#
	movq	%rax, -32(%rbp)	# tmp223, result
# src/Runtime/runtime.c:210:                     result = _String_concat(result, __createString("\""));
	leaq	.LC3(%rip), %rax	#, tmp224
	movq	%rax, %rdi	# tmp224,
	call	__createString	#
	movq	%rax, %rdx	#, _48
	movq	-32(%rbp), %rax	# result, tmp225
	movq	%rdx, %rsi	# _48,
	movq	%rax, %rdi	# tmp225,
	call	_String_concat	#
	movq	%rax, -32(%rbp)	# tmp226, result
# src/Runtime/runtime.c:212:                 break;
	jmp	.L42	#
.L46:
# src/Runtime/runtime.c:215:                 if (IS_NULL(*fieldValue)) {
	movq	-88(%rbp), %rax	# fieldValue, tmp227
	movq	(%rax), %rdx	# *fieldValue_103, _49
# src/Runtime/runtime.c:215:                 if (IS_NULL(*fieldValue)) {
	leaq	_LAT_NULL(%rip), %rax	#, tmp228
	cmpq	%rax, %rdx	# tmp228, _49
	jne	.L53	#,
# src/Runtime/runtime.c:216:                     result = _String_concat(result, __createString("null"));
	leaq	.LC2(%rip), %rax	#, tmp229
	movq	%rax, %rdi	# tmp229,
	call	__createString	#
	movq	%rax, %rdx	#, _50
	movq	-32(%rbp), %rax	# result, tmp230
	movq	%rdx, %rsi	# _50,
	movq	%rax, %rdi	# tmp230,
	call	_String_concat	#
	movq	%rax, -32(%rbp)	# tmp231, result
# src/Runtime/runtime.c:221:                 break;
	jmp	.L42	#
.L53:
# src/Runtime/runtime.c:218:                     obj (*toString)(obj) = ((void **)(*fieldValue)->type->methods)[0];
	movq	-88(%rbp), %rax	# fieldValue, tmp232
	movq	(%rax), %rax	# *fieldValue_103, _51
# src/Runtime/runtime.c:218:                     obj (*toString)(obj) = ((void **)(*fieldValue)->type->methods)[0];
	movq	(%rax), %rax	# _51->type, _52
# src/Runtime/runtime.c:218:                     obj (*toString)(obj) = ((void **)(*fieldValue)->type->methods)[0];
	movq	20(%rax), %rax	# _52->methods, _53
# src/Runtime/runtime.c:218:                     obj (*toString)(obj) = ((void **)(*fieldValue)->type->methods)[0];
	movq	(%rax), %rax	# MEM[(void * *)_53], _54
# src/Runtime/runtime.c:218:                     obj (*toString)(obj) = ((void **)(*fieldValue)->type->methods)[0];
	movq	%rax, -120(%rbp)	# _54, toString
# src/Runtime/runtime.c:219:                     result = _String_concat(result, toString(*fieldValue));
	movq	-88(%rbp), %rax	# fieldValue, tmp233
	movq	(%rax), %rax	# *fieldValue_103, _55
	movq	-120(%rbp), %rdx	# toString, tmp234
	movq	%rax, %rdi	# _55,
	call	*%rdx	# tmp234
	movq	%rax, %rdx	#, _56
	movq	-32(%rbp), %rax	# result, tmp235
	movq	%rdx, %rsi	# _56,
	movq	%rax, %rdi	# tmp235,
	call	_String_concat	#
	movq	%rax, -32(%rbp)	# tmp236, result
# src/Runtime/runtime.c:221:                 break;
	jmp	.L42	#
.L45:
# src/Runtime/runtime.c:224:                 uint32_t* fVal = (uint32_t*)fieldValue;
	movq	-88(%rbp), %rax	# fieldValue, tmp237
	movq	%rax, -104(%rbp)	# tmp237, fVal
# src/Runtime/runtime.c:225:                 char *val_buf = (char*)malloc(50 * sizeof(char));
	movl	$50, %edi	#,
	call	malloc@PLT	#
	movq	%rax, -112(%rbp)	# tmp238, val_buf
# src/Runtime/runtime.c:226:                 sprintf(val_buf, "%d", *fVal);
	movq	-104(%rbp), %rax	# fVal, tmp239
	movl	(%rax), %edx	# *fVal_124, _57
	movq	-112(%rbp), %rax	# val_buf, tmp240
	leaq	.LC7(%rip), %rcx	#, tmp241
	movq	%rcx, %rsi	# tmp241,
	movq	%rax, %rdi	# tmp240,
	movl	$0, %eax	#,
	call	sprintf@PLT	#
# src/Runtime/runtime.c:227:                 result = _String_concat(result, __createString(val_buf));
	movq	-112(%rbp), %rax	# val_buf, tmp242
	movq	%rax, %rdi	# tmp242,
	call	__createString	#
	movq	%rax, %rdx	#, _58
	movq	-32(%rbp), %rax	# result, tmp243
	movq	%rdx, %rsi	# _58,
	movq	%rax, %rdi	# tmp243,
	call	_String_concat	#
	movq	%rax, -32(%rbp)	# tmp244, result
# src/Runtime/runtime.c:228:                 free(val_buf);
	movq	-112(%rbp), %rax	# val_buf, tmp245
	movq	%rax, %rdi	# tmp245,
	call	free@PLT	#
# src/Runtime/runtime.c:229:                 break;
	jmp	.L42	#
.L47:
# src/Runtime/runtime.c:233:                 uint8_t* fVal = (uint8_t*)fieldValue;
	movq	-88(%rbp), %rax	# fieldValue, tmp246
	movq	%rax, -128(%rbp)	# tmp246, fVal
# src/Runtime/runtime.c:234:                 char *val_buf = (char*)malloc(13 * sizeof(char));
	movl	$13, %edi	#,
	call	malloc@PLT	#
	movq	%rax, -136(%rbp)	# tmp247, val_buf
# src/Runtime/runtime.c:235:                 if (*fVal) {
	movq	-128(%rbp), %rax	# fVal, tmp248
	movzbl	(%rax), %eax	# *fVal_139, _59
# src/Runtime/runtime.c:235:                 if (*fVal) {
	testb	%al, %al	# _59
	je	.L55	#,
# src/Runtime/runtime.c:236:                     sprintf(val_buf, "true");
	movq	-136(%rbp), %rax	# val_buf, tmp249
	leaq	.LC8(%rip), %rdx	#, tmp250
	movq	%rdx, %rsi	# tmp250,
	movq	%rax, %rdi	# tmp249,
	movl	$0, %eax	#,
	call	sprintf@PLT	#
	jmp	.L56	#
.L55:
# src/Runtime/runtime.c:238:                     sprintf(val_buf, "false");
	movq	-136(%rbp), %rax	# val_buf, tmp251
	leaq	.LC9(%rip), %rdx	#, tmp252
	movq	%rdx, %rsi	# tmp252,
	movq	%rax, %rdi	# tmp251,
	movl	$0, %eax	#,
	call	sprintf@PLT	#
.L56:
# src/Runtime/runtime.c:240:                 result = _String_concat(result, __createString(val_buf));
	movq	-136(%rbp), %rax	# val_buf, tmp253
	movq	%rax, %rdi	# tmp253,
	call	__createString	#
	movq	%rax, %rdx	#, _60
	movq	-32(%rbp), %rax	# result, tmp254
	movq	%rdx, %rsi	# _60,
	movq	%rax, %rdi	# tmp254,
	call	_String_concat	#
	movq	%rax, -32(%rbp)	# tmp255, result
# src/Runtime/runtime.c:241:                 free(val_buf);
	movq	-136(%rbp), %rax	# val_buf, tmp256
	movq	%rax, %rdi	# tmp256,
	call	free@PLT	#
# src/Runtime/runtime.c:242:                 break;
	nop	
.L42:
# src/Runtime/runtime.c:246:         if (i!=fieldsInfoLength-1) {
	movl	-52(%rbp), %eax	# fieldsInfoLength, tmp257
	subl	$1, %eax	#, _61
# src/Runtime/runtime.c:246:         if (i!=fieldsInfoLength-1) {
	cmpl	%eax, -36(%rbp)	# _61, i
	je	.L57	#,
# src/Runtime/runtime.c:247:             result = _String_concat(result, __createString(", "));
	leaq	.LC10(%rip), %rax	#, tmp258
	movq	%rax, %rdi	# tmp258,
	call	__createString	#
	movq	%rax, %rdx	#, _62
	movq	-32(%rbp), %rax	# result, tmp259
	movq	%rdx, %rsi	# _62,
	movq	%rax, %rdi	# tmp259,
	call	_String_concat	#
	movq	%rax, -32(%rbp)	# tmp260, result
.L57:
# src/Runtime/runtime.c:180:     for(int i=0;i<fieldsInfoLength;++i) {
	addl	$1, -36(%rbp)	#, i
.L41:
# src/Runtime/runtime.c:180:     for(int i=0;i<fieldsInfoLength;++i) {
	movl	-36(%rbp), %eax	# i, tmp261
	cmpl	-52(%rbp), %eax	# fieldsInfoLength, tmp261
	jl	.L58	#,
# src/Runtime/runtime.c:250:     result = _String_concat(result, __createString("}"));
	leaq	.LC11(%rip), %rax	#, tmp262
	movq	%rax, %rdi	# tmp262,
	call	__createString	#
	movq	%rax, %rdx	#, _63
	movq	-32(%rbp), %rax	# result, tmp263
	movq	%rdx, %rsi	# _63,
	movq	%rax, %rdi	# tmp263,
	call	_String_concat	#
	movq	%rax, -32(%rbp)	# tmp264, result
# src/Runtime/runtime.c:251:     return result;
	movq	-32(%rbp), %rax	# result, _72
.L35:
# src/Runtime/runtime.c:252: }
	movq	-8(%rbp), %rbx	#,
	leave	
	.cfi_def_cfa 7, 8
	ret	
	.cfi_endproc
.LFE15:
	.size	_typeInfoStringRepr, .-_typeInfoStringRepr
	.globl	typeInfoStringRepr
	.type	typeInfoStringRepr, @function
typeInfoStringRepr:
.LFB16:
	.cfi_startproc
	pushq	%rbp	#
	.cfi_def_cfa_offset 16
	.cfi_offset 6, -16
	movq	%rsp, %rbp	#,
	.cfi_def_cfa_register 6
	subq	$32, %rsp	#,
	movq	%rdi, -24(%rbp)	# o, o
# src/Runtime/runtime.c:255:     if (type_info_string_repr_level == 0) {
	movl	type_info_string_repr_level(%rip), %eax	# type_info_string_repr_level, type_info_string_repr_level.80_1
# src/Runtime/runtime.c:255:     if (type_info_string_repr_level == 0) {
	testl	%eax, %eax	# type_info_string_repr_level.80_1
	jne	.L60	#,
# src/Runtime/runtime.c:256:         type_info_string_repr_visited_length = 0;
	movl	$0, type_info_string_repr_visited_length(%rip)	#, type_info_string_repr_visited_length
.L60:
# src/Runtime/runtime.c:258:     type_info_string_repr_level++;
	movl	type_info_string_repr_level(%rip), %eax	# type_info_string_repr_level, type_info_string_repr_level.81_2
	addl	$1, %eax	#, _3
	movl	%eax, type_info_string_repr_level(%rip)	# _3, type_info_string_repr_level
# src/Runtime/runtime.c:260:     obj ret = _typeInfoStringRepr(o);
	movq	-24(%rbp), %rax	# o, tmp90
	movq	%rax, %rdi	# tmp90,
	call	_typeInfoStringRepr	#
	movq	%rax, -8(%rbp)	# tmp91, ret
# src/Runtime/runtime.c:261:     __incRef(ret);
	movq	-8(%rbp), %rax	# ret, tmp92
	movq	%rax, %rdi	# tmp92,
	call	__incRef	#
# src/Runtime/runtime.c:263:     type_info_string_repr_level--;
	movl	type_info_string_repr_level(%rip), %eax	# type_info_string_repr_level, type_info_string_repr_level.82_4
	subl	$1, %eax	#, _5
	movl	%eax, type_info_string_repr_level(%rip)	# _5, type_info_string_repr_level
# src/Runtime/runtime.c:264:     if (type_info_string_repr_level == 0) {
	movl	type_info_string_repr_level(%rip), %eax	# type_info_string_repr_level, type_info_string_repr_level.83_6
# src/Runtime/runtime.c:264:     if (type_info_string_repr_level == 0) {
	testl	%eax, %eax	# type_info_string_repr_level.83_6
	jne	.L61	#,
# src/Runtime/runtime.c:265:         type_info_string_repr_visited_length = 0;
	movl	$0, type_info_string_repr_visited_length(%rip)	#, type_info_string_repr_visited_length
.L61:
# src/Runtime/runtime.c:267:     return ret;
	movq	-8(%rbp), %rax	# ret, _18
# src/Runtime/runtime.c:268: }
	leave	
	.cfi_def_cfa 7, 8
	ret	
	.cfi_endproc
.LFE16:
	.size	typeInfoStringRepr, .-typeInfoStringRepr
	.globl	_Object_toString
	.type	_Object_toString, @function
_Object_toString:
.LFB17:
	.cfi_startproc
	pushq	%rbp	#
	.cfi_def_cfa_offset 16
	.cfi_offset 6, -16
	movq	%rsp, %rbp	#,
	.cfi_def_cfa_register 6
	subq	$16, %rsp	#,
	movq	%rdi, -8(%rbp)	# o, o
# src/Runtime/runtime.c:275:     return typeInfoStringRepr(o);
	movq	-8(%rbp), %rax	# o, tmp84
	movq	%rax, %rdi	# tmp84,
	call	typeInfoStringRepr	#
# src/Runtime/runtime.c:276: }
	leave	
	.cfi_def_cfa 7, 8
	ret	
	.cfi_endproc
.LFE17:
	.size	_Object_toString, .-_Object_toString
	.globl	_Object_getHashCode
	.type	_Object_getHashCode, @function
_Object_getHashCode:
.LFB18:
	.cfi_startproc
	pushq	%rbp	#
	.cfi_def_cfa_offset 16
	.cfi_offset 6, -16
	movq	%rsp, %rbp	#,
	.cfi_def_cfa_register 6
	movq	%rdi, -8(%rbp)	# o, o
# src/Runtime/runtime.c:277: int32_t _Object_getHashCode(obj o) { return (int32_t)(int64_t)o; }
	movq	-8(%rbp), %rax	# o, o.84_1
# src/Runtime/runtime.c:277: int32_t _Object_getHashCode(obj o) { return (int32_t)(int64_t)o; }
	popq	%rbp	#
	.cfi_def_cfa 7, 8
	ret	
	.cfi_endproc
.LFE18:
	.size	_Object_getHashCode, .-_Object_getHashCode
	.globl	_Object_equals
	.type	_Object_equals, @function
_Object_equals:
.LFB19:
	.cfi_startproc
	pushq	%rbp	#
	.cfi_def_cfa_offset 16
	.cfi_offset 6, -16
	movq	%rsp, %rbp	#,
	.cfi_def_cfa_register 6
	movq	%rdi, -8(%rbp)	# o1, o1
	movq	%rsi, -16(%rbp)	# o2, o2
# src/Runtime/runtime.c:278: int8_t _Object_equals(obj o1, obj o2) { return o1 == o2; }
	movq	-8(%rbp), %rax	# o1, tmp85
	cmpq	-16(%rbp), %rax	# o2, tmp85
	sete	%al	#, _1
# src/Runtime/runtime.c:278: int8_t _Object_equals(obj o1, obj o2) { return o1 == o2; }
	popq	%rbp	#
	.cfi_def_cfa 7, 8
	ret	
	.cfi_endproc
.LFE19:
	.size	_Object_equals, .-_Object_equals
	.section	.rodata
.LC12:
	.string	"[]"
	.text
	.globl	_Array_toString
	.type	_Array_toString, @function
_Array_toString:
.LFB20:
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
# src/Runtime/runtime.c:283:     if (arr->length == 0) {
	movq	-136(%rbp), %rax	# arr, tmp187
	movl	32(%rax), %eax	# arr_116(D)->length, _1
# src/Runtime/runtime.c:283:     if (arr->length == 0) {
	testl	%eax, %eax	# _1
	jne	.L70	#,
# src/Runtime/runtime.c:284:         obj ret = __createString("[]");
	leaq	.LC12(%rip), %rax	#, tmp188
	movq	%rax, %rdi	# tmp188,
	call	__createString	#
	movq	%rax, -120(%rbp)	# tmp189, ret
# src/Runtime/runtime.c:285:         __incRef(ret);
	movq	-120(%rbp), %rax	# ret, tmp190
	movq	%rax, %rdi	# tmp190,
	call	__incRef	#
# src/Runtime/runtime.c:286:         return ret;
	movq	-120(%rbp), %rax	# ret, _109
	jmp	.L81	#
.L70:
# src/Runtime/runtime.c:289:     char start[] = "[";
	movw	$91, -122(%rbp)	#, start
# src/Runtime/runtime.c:290:     char delim[] = ", ";
	movw	$8236, -125(%rbp)	#, delim
	movb	$0, -123(%rbp)	#, delim
# src/Runtime/runtime.c:291:     char end[] = "]";
	movw	$93, -127(%rbp)	#, end
# src/Runtime/runtime.c:293:     obj *strings = malloc(sizeof(obj) * arr->length);
	movq	-136(%rbp), %rax	# arr, tmp191
	movl	32(%rax), %eax	# arr_116(D)->length, _2
	cltq
# src/Runtime/runtime.c:293:     obj *strings = malloc(sizeof(obj) * arr->length);
	salq	$3, %rax	#, _4
	movq	%rax, %rdi	# _4,
	call	malloc@PLT	#
	movq	%rax, -40(%rbp)	# tmp192, strings
# src/Runtime/runtime.c:294:     int32_t *lenghts = malloc(sizeof(int32_t) * arr->length);
	movq	-136(%rbp), %rax	# arr, tmp193
	movl	32(%rax), %eax	# arr_116(D)->length, _5
	cltq
# src/Runtime/runtime.c:294:     int32_t *lenghts = malloc(sizeof(int32_t) * arr->length);
	salq	$2, %rax	#, _7
	movq	%rax, %rdi	# _7,
	call	malloc@PLT	#
	movq	%rax, -48(%rbp)	# tmp194, lenghts
# src/Runtime/runtime.c:295:     int32_t totalLenght = 0;
	movl	$0, -20(%rbp)	#, totalLenght
# src/Runtime/runtime.c:297:     for (int i = 0; i < arr->length; i++) {
	movl	$0, -24(%rbp)	#, i
# src/Runtime/runtime.c:297:     for (int i = 0; i < arr->length; i++) {
	jmp	.L72	#
.L77:
# src/Runtime/runtime.c:298:         if (arr->elementSize == sizeof(int32_t)) {
	movq	-136(%rbp), %rax	# arr, tmp195
	movl	28(%rax), %eax	# arr_116(D)->elementSize, _8
# src/Runtime/runtime.c:298:         if (arr->elementSize == sizeof(int32_t)) {
	cmpl	$4, %eax	#, _8
	jne	.L73	#,
# src/Runtime/runtime.c:299:             int32_t *elements = arr->data;
	movq	-136(%rbp), %rax	# arr, tmp196
	movq	8(%rax), %rax	# arr_116(D)->data, tmp197
	movq	%rax, -112(%rbp)	# tmp197, elements
# src/Runtime/runtime.c:300:             strings[i] = intToString(elements[i]);
	movl	-24(%rbp), %eax	# i, tmp198
	cltq
	leaq	0(,%rax,4), %rdx	#, _10
	movq	-112(%rbp), %rax	# elements, tmp199
	addq	%rdx, %rax	# _10, _11
# src/Runtime/runtime.c:300:             strings[i] = intToString(elements[i]);
	movl	(%rax), %eax	# *_11, _12
# src/Runtime/runtime.c:300:             strings[i] = intToString(elements[i]);
	movl	-24(%rbp), %edx	# i, tmp200
	movslq	%edx, %rdx	# tmp200, _13
	leaq	0(,%rdx,8), %rcx	#, _14
	movq	-40(%rbp), %rdx	# strings, tmp201
	leaq	(%rcx,%rdx), %rbx	#, _15
# src/Runtime/runtime.c:300:             strings[i] = intToString(elements[i]);
	movl	%eax, %edi	# _12,
	call	intToString	#
# src/Runtime/runtime.c:300:             strings[i] = intToString(elements[i]);
	movq	%rax, (%rbx)	# _16, *_15
	jmp	.L74	#
.L73:
# src/Runtime/runtime.c:301:         } else if (arr->elementSize == sizeof(int8_t)) {
	movq	-136(%rbp), %rax	# arr, tmp202
	movl	28(%rax), %eax	# arr_116(D)->elementSize, _17
# src/Runtime/runtime.c:301:         } else if (arr->elementSize == sizeof(int8_t)) {
	cmpl	$1, %eax	#, _17
	jne	.L75	#,
# src/Runtime/runtime.c:302:             int8_t *elements = arr->data;
	movq	-136(%rbp), %rax	# arr, tmp203
	movq	8(%rax), %rax	# arr_116(D)->data, tmp204
	movq	%rax, -104(%rbp)	# tmp204, elements
# src/Runtime/runtime.c:303:             strings[i] = byteToString(elements[i]);
	movl	-24(%rbp), %eax	# i, tmp205
	movslq	%eax, %rdx	# tmp205, _18
	movq	-104(%rbp), %rax	# elements, tmp206
	addq	%rdx, %rax	# _18, _19
	movzbl	(%rax), %eax	# *_19, _20
# src/Runtime/runtime.c:303:             strings[i] = byteToString(elements[i]);
	movzbl	%al, %eax	# _21, _22
# src/Runtime/runtime.c:303:             strings[i] = byteToString(elements[i]);
	movl	-24(%rbp), %edx	# i, tmp207
	movslq	%edx, %rdx	# tmp207, _23
	leaq	0(,%rdx,8), %rcx	#, _24
	movq	-40(%rbp), %rdx	# strings, tmp208
	leaq	(%rcx,%rdx), %rbx	#, _25
# src/Runtime/runtime.c:303:             strings[i] = byteToString(elements[i]);
	movl	%eax, %edi	# _22,
	call	byteToString	#
# src/Runtime/runtime.c:303:             strings[i] = byteToString(elements[i]);
	movq	%rax, (%rbx)	# _26, *_25
	jmp	.L74	#
.L75:
# src/Runtime/runtime.c:305:             obj *elements = arr->data;
	movq	-136(%rbp), %rax	# arr, tmp209
	movq	8(%rax), %rax	# arr_116(D)->data, tmp210
	movq	%rax, -80(%rbp)	# tmp210, elements
# src/Runtime/runtime.c:306:             obj element = elements[i];
	movl	-24(%rbp), %eax	# i, tmp211
	cltq
	leaq	0(,%rax,8), %rdx	#, _28
	movq	-80(%rbp), %rax	# elements, tmp212
	addq	%rdx, %rax	# _28, _29
# src/Runtime/runtime.c:306:             obj element = elements[i];
	movq	(%rax), %rax	# *_29, tmp213
	movq	%rax, -88(%rbp)	# tmp213, element
# src/Runtime/runtime.c:307:             if (IS_NULL(element)) {
	leaq	_LAT_NULL(%rip), %rax	#, tmp214
	cmpq	%rax, -88(%rbp)	# tmp214, element
	jne	.L76	#,
# src/Runtime/runtime.c:308:                 strings[i] = __createString("null");
	movl	-24(%rbp), %eax	# i, tmp215
	cltq
	leaq	0(,%rax,8), %rdx	#, _31
	movq	-40(%rbp), %rax	# strings, tmp216
	leaq	(%rdx,%rax), %rbx	#, _32
# src/Runtime/runtime.c:308:                 strings[i] = __createString("null");
	leaq	.LC2(%rip), %rax	#, tmp217
	movq	%rax, %rdi	# tmp217,
	call	__createString	#
# src/Runtime/runtime.c:308:                 strings[i] = __createString("null");
	movq	%rax, (%rbx)	# _33, *_32
# src/Runtime/runtime.c:309:                 __incRef(strings[i]);
	movl	-24(%rbp), %eax	# i, tmp218
	cltq
	leaq	0(,%rax,8), %rdx	#, _35
	movq	-40(%rbp), %rax	# strings, tmp219
	addq	%rdx, %rax	# _35, _36
# src/Runtime/runtime.c:309:                 __incRef(strings[i]);
	movq	(%rax), %rax	# *_36, _37
	movq	%rax, %rdi	# _37,
	call	__incRef	#
	jmp	.L74	#
.L76:
# src/Runtime/runtime.c:311:                 obj (*toString)(obj) = ((void **)element->type->methods)[0];
	movq	-88(%rbp), %rax	# element, tmp220
	movq	(%rax), %rax	# element_149->type, _38
# src/Runtime/runtime.c:311:                 obj (*toString)(obj) = ((void **)element->type->methods)[0];
	movq	20(%rax), %rax	# _38->methods, _39
# src/Runtime/runtime.c:311:                 obj (*toString)(obj) = ((void **)element->type->methods)[0];
	movq	(%rax), %rax	# MEM[(void * *)_39], _40
# src/Runtime/runtime.c:311:                 obj (*toString)(obj) = ((void **)element->type->methods)[0];
	movq	%rax, -96(%rbp)	# _40, toString
# src/Runtime/runtime.c:312:                 strings[i] = toString(element);
	movl	-24(%rbp), %eax	# i, tmp221
	cltq
	leaq	0(,%rax,8), %rdx	#, _42
	movq	-40(%rbp), %rax	# strings, tmp222
	leaq	(%rdx,%rax), %rbx	#, _43
# src/Runtime/runtime.c:312:                 strings[i] = toString(element);
	movq	-88(%rbp), %rax	# element, tmp223
	movq	-96(%rbp), %rdx	# toString, tmp224
	movq	%rax, %rdi	# tmp223,
	call	*%rdx	# tmp224
# src/Runtime/runtime.c:312:                 strings[i] = toString(element);
	movq	%rax, (%rbx)	# _44, *_43
.L74:
# src/Runtime/runtime.c:315:         lenghts[i] = u8_strlen((strings[i]->data));
	movl	-24(%rbp), %eax	# i, tmp225
	cltq
	leaq	0(,%rax,8), %rdx	#, _46
	movq	-40(%rbp), %rax	# strings, tmp226
	addq	%rdx, %rax	# _46, _47
	movq	(%rax), %rax	# *_47, _48
# src/Runtime/runtime.c:315:         lenghts[i] = u8_strlen((strings[i]->data));
	movq	8(%rax), %rax	# _48->data, _49
# src/Runtime/runtime.c:315:         lenghts[i] = u8_strlen((strings[i]->data));
	movq	%rax, %rdi	# _49,
	call	u8_strlen@PLT	#
# src/Runtime/runtime.c:315:         lenghts[i] = u8_strlen((strings[i]->data));
	movl	-24(%rbp), %edx	# i, tmp227
	movslq	%edx, %rdx	# tmp227, _51
	leaq	0(,%rdx,4), %rcx	#, _52
	movq	-48(%rbp), %rdx	# lenghts, tmp228
	addq	%rcx, %rdx	# _52, _53
# src/Runtime/runtime.c:315:         lenghts[i] = u8_strlen((strings[i]->data));
	movl	%eax, (%rdx)	# _54, *_53
# src/Runtime/runtime.c:316:         totalLenght += lenghts[i];
	movl	-24(%rbp), %eax	# i, tmp229
	cltq
	leaq	0(,%rax,4), %rdx	#, _56
	movq	-48(%rbp), %rax	# lenghts, tmp230
	addq	%rdx, %rax	# _56, _57
	movl	(%rax), %eax	# *_57, _58
# src/Runtime/runtime.c:316:         totalLenght += lenghts[i];
	addl	%eax, -20(%rbp)	# _58, totalLenght
# src/Runtime/runtime.c:297:     for (int i = 0; i < arr->length; i++) {
	addl	$1, -24(%rbp)	#, i
.L72:
# src/Runtime/runtime.c:297:     for (int i = 0; i < arr->length; i++) {
	movq	-136(%rbp), %rax	# arr, tmp231
	movl	32(%rax), %eax	# arr_116(D)->length, _59
# src/Runtime/runtime.c:297:     for (int i = 0; i < arr->length; i++) {
	cmpl	%eax, -24(%rbp)	# _59, i
	jl	.L77	#,
# src/Runtime/runtime.c:319:     int32_t bufferSize = u8_strlen(start) + totalLenght +
	leaq	-122(%rbp), %rax	#, tmp232
	movq	%rax, %rdi	# tmp232,
	call	u8_strlen@PLT	#
# src/Runtime/runtime.c:319:     int32_t bufferSize = u8_strlen(start) + totalLenght +
	movl	%eax, %edx	# _60, _61
	movl	-20(%rbp), %eax	# totalLenght, totalLenght.85_62
	leal	(%rdx,%rax), %ebx	#, _63
# src/Runtime/runtime.c:320:                          (arr->length - 1) * u8_strlen(delim) +
	movq	-136(%rbp), %rax	# arr, tmp233
	movl	32(%rax), %eax	# arr_116(D)->length, _64
# src/Runtime/runtime.c:320:                          (arr->length - 1) * u8_strlen(delim) +
	subl	$1, %eax	#, _65
	cltq
# src/Runtime/runtime.c:319:     int32_t bufferSize = u8_strlen(start) + totalLenght +
	movl	%eax, %r12d	# _66, _67
# src/Runtime/runtime.c:320:                          (arr->length - 1) * u8_strlen(delim) +
	leaq	-125(%rbp), %rax	#, tmp234
	movq	%rax, %rdi	# tmp234,
	call	u8_strlen@PLT	#
# src/Runtime/runtime.c:319:     int32_t bufferSize = u8_strlen(start) + totalLenght +
	imull	%r12d, %eax	# _67, _70
	addl	%eax, %ebx	# _70, _71
# src/Runtime/runtime.c:321:                          u8_strlen(end) + 1;
	leaq	-127(%rbp), %rax	#, tmp235
	movq	%rax, %rdi	# tmp235,
	call	u8_strlen@PLT	#
# src/Runtime/runtime.c:320:                          (arr->length - 1) * u8_strlen(delim) +
	addl	%ebx, %eax	# _71, _74
# src/Runtime/runtime.c:321:                          u8_strlen(end) + 1;
	addl	$1, %eax	#, _75
# src/Runtime/runtime.c:319:     int32_t bufferSize = u8_strlen(start) + totalLenght +
	movl	%eax, -52(%rbp)	# _75, bufferSize
# src/Runtime/runtime.c:322:     uint8_t *buffer = malloc(bufferSize);
	movl	-52(%rbp), %eax	# bufferSize, tmp236
	cltq
	movq	%rax, %rdi	# _76,
	call	malloc@PLT	#
	movq	%rax, -64(%rbp)	# tmp237, buffer
# src/Runtime/runtime.c:323:     int32_t index = 0;
	movl	$0, -28(%rbp)	#, index
# src/Runtime/runtime.c:324:     u8_strcpy(buffer + index, start);
	movl	-28(%rbp), %eax	# index, tmp238
	movslq	%eax, %rdx	# tmp238, _77
	movq	-64(%rbp), %rax	# buffer, tmp239
	addq	%rax, %rdx	# tmp239, _78
	leaq	-122(%rbp), %rax	#, tmp240
	movq	%rax, %rsi	# tmp240,
	movq	%rdx, %rdi	# _78,
	call	u8_strcpy@PLT	#
# src/Runtime/runtime.c:325:     index++;
	addl	$1, -28(%rbp)	#, index
# src/Runtime/runtime.c:326:     for (int i = 0; i < arr->length; i++) {
	movl	$0, -32(%rbp)	#, i
# src/Runtime/runtime.c:326:     for (int i = 0; i < arr->length; i++) {
	jmp	.L78	#
.L80:
# src/Runtime/runtime.c:327:         u8_strcpy(buffer + index, (strings[i]->data));
	movl	-32(%rbp), %eax	# i, tmp241
	cltq
	leaq	0(,%rax,8), %rdx	#, _80
	movq	-40(%rbp), %rax	# strings, tmp242
	addq	%rdx, %rax	# _80, _81
	movq	(%rax), %rax	# *_81, _82
# src/Runtime/runtime.c:327:         u8_strcpy(buffer + index, (strings[i]->data));
	movq	8(%rax), %rax	# _82->data, _83
# src/Runtime/runtime.c:327:         u8_strcpy(buffer + index, (strings[i]->data));
	movl	-28(%rbp), %edx	# index, tmp243
	movslq	%edx, %rcx	# tmp243, _84
	movq	-64(%rbp), %rdx	# buffer, tmp244
	addq	%rcx, %rdx	# _84, _85
	movq	%rax, %rsi	# _83,
	movq	%rdx, %rdi	# _85,
	call	u8_strcpy@PLT	#
# src/Runtime/runtime.c:328:         index += lenghts[i];
	movl	-32(%rbp), %eax	# i, tmp245
	cltq
	leaq	0(,%rax,4), %rdx	#, _87
	movq	-48(%rbp), %rax	# lenghts, tmp246
	addq	%rdx, %rax	# _87, _88
	movl	(%rax), %eax	# *_88, _89
# src/Runtime/runtime.c:328:         index += lenghts[i];
	addl	%eax, -28(%rbp)	# _89, index
# src/Runtime/runtime.c:329:         if (i != arr->length - 1) {
	movq	-136(%rbp), %rax	# arr, tmp247
	movl	32(%rax), %eax	# arr_116(D)->length, _90
# src/Runtime/runtime.c:329:         if (i != arr->length - 1) {
	subl	$1, %eax	#, _91
# src/Runtime/runtime.c:329:         if (i != arr->length - 1) {
	cmpl	%eax, -32(%rbp)	# _91, i
	je	.L79	#,
# src/Runtime/runtime.c:330:             u8_strcpy(buffer + index, delim);
	movl	-28(%rbp), %eax	# index, tmp248
	movslq	%eax, %rdx	# tmp248, _92
	movq	-64(%rbp), %rax	# buffer, tmp249
	addq	%rax, %rdx	# tmp249, _93
	leaq	-125(%rbp), %rax	#, tmp250
	movq	%rax, %rsi	# tmp250,
	movq	%rdx, %rdi	# _93,
	call	u8_strcpy@PLT	#
# src/Runtime/runtime.c:331:             index += 2;
	addl	$2, -28(%rbp)	#, index
.L79:
# src/Runtime/runtime.c:333:         __decRef(strings[i]);
	movl	-32(%rbp), %eax	# i, tmp251
	cltq
	leaq	0(,%rax,8), %rdx	#, _95
	movq	-40(%rbp), %rax	# strings, tmp252
	addq	%rdx, %rax	# _95, _96
# src/Runtime/runtime.c:333:         __decRef(strings[i]);
	movq	(%rax), %rax	# *_96, _97
	movq	%rax, %rdi	# _97,
	call	__decRef	#
# src/Runtime/runtime.c:326:     for (int i = 0; i < arr->length; i++) {
	addl	$1, -32(%rbp)	#, i
.L78:
# src/Runtime/runtime.c:326:     for (int i = 0; i < arr->length; i++) {
	movq	-136(%rbp), %rax	# arr, tmp253
	movl	32(%rax), %eax	# arr_116(D)->length, _98
# src/Runtime/runtime.c:326:     for (int i = 0; i < arr->length; i++) {
	cmpl	%eax, -32(%rbp)	# _98, i
	jl	.L80	#,
# src/Runtime/runtime.c:335:     u8_strcpy(buffer + index, end);
	movl	-28(%rbp), %eax	# index, tmp254
	movslq	%eax, %rdx	# tmp254, _99
	movq	-64(%rbp), %rax	# buffer, tmp255
	addq	%rax, %rdx	# tmp255, _100
	leaq	-127(%rbp), %rax	#, tmp256
	movq	%rax, %rsi	# tmp256,
	movq	%rdx, %rdi	# _100,
	call	u8_strcpy@PLT	#
# src/Runtime/runtime.c:336:     buffer[bufferSize - 1] = 0;
	movl	-52(%rbp), %eax	# bufferSize, tmp257
	cltq
	leaq	-1(%rax), %rdx	#, _102
	movq	-64(%rbp), %rax	# buffer, tmp258
	addq	%rdx, %rax	# _102, _103
# src/Runtime/runtime.c:336:     buffer[bufferSize - 1] = 0;
	movb	$0, (%rax)	#, *_103
# src/Runtime/runtime.c:337:     obj ret = __createString(buffer);
	movq	-64(%rbp), %rax	# buffer, tmp259
	movq	%rax, %rdi	# tmp259,
	call	__createString	#
	movq	%rax, -72(%rbp)	# tmp260, ret
# src/Runtime/runtime.c:338:     __incRef(ret);
	movq	-72(%rbp), %rax	# ret, tmp261
	movq	%rax, %rdi	# tmp261,
	call	__incRef	#
# src/Runtime/runtime.c:339:     free(lenghts);
	movq	-48(%rbp), %rax	# lenghts, tmp262
	movq	%rax, %rdi	# tmp262,
	call	free@PLT	#
# src/Runtime/runtime.c:340:     free(strings);
	movq	-40(%rbp), %rax	# strings, tmp263
	movq	%rax, %rdi	# tmp263,
	call	free@PLT	#
# src/Runtime/runtime.c:341:     free(buffer);
	movq	-64(%rbp), %rax	# buffer, tmp264
	movq	%rax, %rdi	# tmp264,
	call	free@PLT	#
# src/Runtime/runtime.c:342:     return ret;
	movq	-72(%rbp), %rax	# ret, _109
.L81:
# src/Runtime/runtime.c:343: }
	subq	$-128, %rsp	#,
	popq	%rbx	#
	popq	%r12	#
	popq	%rbp	#
	.cfi_def_cfa 7, 8
	ret	
	.cfi_endproc
.LFE20:
	.size	_Array_toString, .-_Array_toString
	.globl	_String_toString
	.type	_String_toString, @function
_String_toString:
.LFB21:
	.cfi_startproc
	pushq	%rbp	#
	.cfi_def_cfa_offset 16
	.cfi_offset 6, -16
	movq	%rsp, %rbp	#,
	.cfi_def_cfa_register 6
	subq	$8, %rsp	#,
	movq	%rdi, -8(%rbp)	# str, str
# src/Runtime/runtime.c:346:     __incRef(str);
	movq	-8(%rbp), %rax	# str, tmp84
	movq	%rax, %rdi	# tmp84,
	call	__incRef	#
# src/Runtime/runtime.c:347:     return str;
	movq	-8(%rbp), %rax	# str, _4
# src/Runtime/runtime.c:348: }
	leave	
	.cfi_def_cfa 7, 8
	ret	
	.cfi_endproc
.LFE21:
	.size	_String_toString, .-_String_toString
	.globl	_String_getHashCode
	.type	_String_getHashCode, @function
_String_getHashCode:
.LFB22:
	.cfi_startproc
	pushq	%rbp	#
	.cfi_def_cfa_offset 16
	.cfi_offset 6, -16
	movq	%rsp, %rbp	#,
	.cfi_def_cfa_register 6
	subq	$48, %rsp	#,
	movq	%rdi, -40(%rbp)	# str, str
# src/Runtime/runtime.c:350:     int32_t hash = 0x811c9dc5;
	movl	$-2128831035, -4(%rbp)	#, hash
# src/Runtime/runtime.c:351:     uint8_t *rawstring = str->data;
	movq	-40(%rbp), %rax	# str, tmp89
	movq	8(%rax), %rax	# str_10(D)->data, tmp90
	movq	%rax, -16(%rbp)	# tmp90, rawstring
# src/Runtime/runtime.c:352:     int32_t strlen = u8_strlen(rawstring);
	movq	-16(%rbp), %rax	# rawstring, tmp91
	movq	%rax, %rdi	# tmp91,
	call	u8_strlen@PLT	#
# src/Runtime/runtime.c:352:     int32_t strlen = u8_strlen(rawstring);
	movl	%eax, -20(%rbp)	# _1, strlen
# src/Runtime/runtime.c:353:     for (int i = 0; i < strlen; i++) {
	movl	$0, -8(%rbp)	#, i
# src/Runtime/runtime.c:353:     for (int i = 0; i < strlen; i++) {
	jmp	.L85	#
.L86:
# src/Runtime/runtime.c:354:         hash ^= rawstring[i];
	movl	-8(%rbp), %eax	# i, tmp92
	movslq	%eax, %rdx	# tmp92, _2
	movq	-16(%rbp), %rax	# rawstring, tmp93
	addq	%rdx, %rax	# _2, _3
	movzbl	(%rax), %eax	# *_3, _4
	movzbl	%al, %eax	# _4, _5
# src/Runtime/runtime.c:354:         hash ^= rawstring[i];
	xorl	%eax, -4(%rbp)	# _5, hash
# src/Runtime/runtime.c:355:         hash *= 0x01000193;
	movl	-4(%rbp), %eax	# hash, tmp95
	imull	$16777619, %eax, %eax	#, tmp95, tmp94
	movl	%eax, -4(%rbp)	# tmp94, hash
# src/Runtime/runtime.c:353:     for (int i = 0; i < strlen; i++) {
	addl	$1, -8(%rbp)	#, i
.L85:
# src/Runtime/runtime.c:353:     for (int i = 0; i < strlen; i++) {
	movl	-8(%rbp), %eax	# i, tmp96
	cmpl	-20(%rbp), %eax	# strlen, tmp96
	jl	.L86	#,
# src/Runtime/runtime.c:357:     return hash;
	movl	-4(%rbp), %eax	# hash, _14
# src/Runtime/runtime.c:358: }
	leave	
	.cfi_def_cfa 7, 8
	ret	
	.cfi_endproc
.LFE22:
	.size	_String_getHashCode, .-_String_getHashCode
	.globl	_String_equals
	.type	_String_equals, @function
_String_equals:
.LFB23:
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
# src/Runtime/runtime.c:360:     if (IS_NULL(o2))
	leaq	_LAT_NULL(%rip), %rax	#, tmp89
	cmpq	%rax, -48(%rbp)	# tmp89, o2
	jne	.L89	#,
# src/Runtime/runtime.c:361:         return false;
	movl	$0, %eax	#, _6
	jmp	.L90	#
.L89:
# src/Runtime/runtime.c:362:     if (o2->type != &_class_String)
	movq	-48(%rbp), %rax	# o2, tmp90
	movq	(%rax), %rdx	# o2_8(D)->type, _1
# src/Runtime/runtime.c:362:     if (o2->type != &_class_String)
	leaq	_class_String(%rip), %rax	#, tmp91
	cmpq	%rax, %rdx	# tmp91, _1
	je	.L91	#,
# src/Runtime/runtime.c:363:         return false;
	movl	$0, %eax	#, _6
	jmp	.L90	#
.L91:
# src/Runtime/runtime.c:364:     if (_String_length(o1) != _String_length(o2))
	movq	-40(%rbp), %rax	# o1, tmp92
	movq	%rax, %rdi	# tmp92,
	call	_String_length	#
	movl	%eax, %ebx	#, _2
# src/Runtime/runtime.c:364:     if (_String_length(o1) != _String_length(o2))
	movq	-48(%rbp), %rax	# o2, tmp93
	movq	%rax, %rdi	# tmp93,
	call	_String_length	#
# src/Runtime/runtime.c:364:     if (_String_length(o1) != _String_length(o2))
	cmpl	%eax, %ebx	# _3, _2
	je	.L92	#,
# src/Runtime/runtime.c:365:         return false;
	movl	$0, %eax	#, _6
	jmp	.L90	#
.L92:
# src/Runtime/runtime.c:366:     uint8_t *rs1 = (o1->data);
	movq	-40(%rbp), %rax	# o1, tmp94
	movq	8(%rax), %rax	# o1_10(D)->data, tmp95
	movq	%rax, -24(%rbp)	# tmp95, rs1
# src/Runtime/runtime.c:367:     uint8_t *rs2 = (o2->data);
	movq	-48(%rbp), %rax	# o2, tmp96
	movq	8(%rax), %rax	# o2_8(D)->data, tmp97
	movq	%rax, -32(%rbp)	# tmp97, rs2
# src/Runtime/runtime.c:368:     return u8_strcmp(rs1, rs2) == 0;
	movq	-32(%rbp), %rdx	# rs2, tmp98
	movq	-24(%rbp), %rax	# rs1, tmp99
	movq	%rdx, %rsi	# tmp98,
	movq	%rax, %rdi	# tmp99,
	call	u8_strcmp@PLT	#
# src/Runtime/runtime.c:368:     return u8_strcmp(rs1, rs2) == 0;
	testl	%eax, %eax	# _4
	sete	%al	#, _5
.L90:
# src/Runtime/runtime.c:369: }
	movq	-8(%rbp), %rbx	#,
	leave	
	.cfi_def_cfa 7, 8
	ret	
	.cfi_endproc
.LFE23:
	.size	_String_equals, .-_String_equals
	.section	.rodata
	.align 8
.LC13:
	.string	"ERROR: Substring with negative length."
.LC14:
	.string	""
	.align 8
.LC15:
	.string	"ERROR: Substring starting index is too big."
	.align 8
.LC16:
	.string	"ERROR: Substring reached end of string."
	.text
	.globl	_String_substring
	.type	_String_substring, @function
_String_substring:
.LFB24:
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
# src/Runtime/runtime.c:371:     if (length < 0) {
	cmpl	$0, -80(%rbp)	#, length
	jns	.L94	#,
# src/Runtime/runtime.c:372:         errMsg = "ERROR: Substring with negative length.";
	leaq	.LC13(%rip), %rax	#, tmp101
	movq	%rax, errMsg(%rip)	# tmp101, errMsg
# src/Runtime/runtime.c:373:         error();
	movl	$0, %eax	#,
	call	error	#
.L94:
# src/Runtime/runtime.c:375:     if (length == 0)
	cmpl	$0, -80(%rbp)	#, length
	jne	.L95	#,
# src/Runtime/runtime.c:376:         return __createString("");
	leaq	.LC14(%rip), %rax	#, tmp102
	movq	%rax, %rdi	# tmp102,
	call	__createString	#
	jmp	.L103	#
.L95:
# src/Runtime/runtime.c:377:     if (startIndex >= _String_length(str)) {
	movq	-72(%rbp), %rax	# str, tmp103
	movq	%rax, %rdi	# tmp103,
	call	_String_length	#
# src/Runtime/runtime.c:377:     if (startIndex >= _String_length(str)) {
	cmpl	%eax, -76(%rbp)	# _1, startIndex
	jl	.L97	#,
# src/Runtime/runtime.c:378:         errMsg = "ERROR: Substring starting index is too big.";
	leaq	.LC15(%rip), %rax	#, tmp104
	movq	%rax, errMsg(%rip)	# tmp104, errMsg
# src/Runtime/runtime.c:379:         error();
	movl	$0, %eax	#,
	call	error	#
.L97:
# src/Runtime/runtime.c:381:     uint8_t *rs = (str->data);
	movq	-72(%rbp), %rax	# str, tmp105
	movq	8(%rax), %rax	# str_31(D)->data, tmp106
	movq	%rax, -32(%rbp)	# tmp106, rs
# src/Runtime/runtime.c:382:     uint8_t *offset_str = rs;
	movq	-32(%rbp), %rax	# rs, tmp107
	movq	%rax, -8(%rbp)	# tmp107, offset_str
# src/Runtime/runtime.c:384:     while (startIndex-- > 0)
	jmp	.L98	#
.L99:
# src/Runtime/runtime.c:385:         offset_str += u8_next(&character, offset_str) - offset_str;
	movq	-8(%rbp), %rdx	# offset_str, tmp108
	leaq	-60(%rbp), %rax	#, tmp109
	movq	%rdx, %rsi	# tmp108,
	movq	%rax, %rdi	# tmp109,
	call	u8_next@PLT	#
# src/Runtime/runtime.c:385:         offset_str += u8_next(&character, offset_str) - offset_str;
	subq	-8(%rbp), %rax	# offset_str, _59
# src/Runtime/runtime.c:385:         offset_str += u8_next(&character, offset_str) - offset_str;
	addq	%rax, -8(%rbp)	# _3, offset_str
.L98:
# src/Runtime/runtime.c:384:     while (startIndex-- > 0)
	movl	-76(%rbp), %eax	# startIndex, startIndex.86_4
	leal	-1(%rax), %edx	#, tmp110
	movl	%edx, -76(%rbp)	# tmp110, startIndex
# src/Runtime/runtime.c:384:     while (startIndex-- > 0)
	testl	%eax, %eax	# startIndex.86_4
	jg	.L99	#,
# src/Runtime/runtime.c:386:     uint8_t *end = offset_str;
	movq	-8(%rbp), %rax	# offset_str, tmp111
	movq	%rax, -16(%rbp)	# tmp111, end
# src/Runtime/runtime.c:387:     int32_t counter = 0;
	movl	$0, -20(%rbp)	#, counter
# src/Runtime/runtime.c:388:     while (counter < length) {
	jmp	.L100	#
.L102:
# src/Runtime/runtime.c:389:         if (u8_next(&character, end) == NULL) {
	movq	-16(%rbp), %rdx	# end, tmp112
	leaq	-60(%rbp), %rax	#, tmp113
	movq	%rdx, %rsi	# tmp112,
	movq	%rax, %rdi	# tmp113,
	call	u8_next@PLT	#
# src/Runtime/runtime.c:389:         if (u8_next(&character, end) == NULL) {
	testq	%rax, %rax	# _5
	jne	.L101	#,
# src/Runtime/runtime.c:390:             errMsg = "ERROR: Substring reached end of string.";
	leaq	.LC16(%rip), %rax	#, tmp114
	movq	%rax, errMsg(%rip)	# tmp114, errMsg
# src/Runtime/runtime.c:391:             error();
	movl	$0, %eax	#,
	call	error	#
.L101:
# src/Runtime/runtime.c:393:         end += u8_next(&character, end) - end;
	movq	-16(%rbp), %rdx	# end, tmp115
	leaq	-60(%rbp), %rax	#, tmp116
	movq	%rdx, %rsi	# tmp115,
	movq	%rax, %rdi	# tmp116,
	call	u8_next@PLT	#
# src/Runtime/runtime.c:393:         end += u8_next(&character, end) - end;
	subq	-16(%rbp), %rax	# end, _55
# src/Runtime/runtime.c:393:         end += u8_next(&character, end) - end;
	addq	%rax, -16(%rbp)	# _7, end
# src/Runtime/runtime.c:394:         counter++;
	addl	$1, -20(%rbp)	#, counter
.L100:
# src/Runtime/runtime.c:388:     while (counter < length) {
	movl	-20(%rbp), %eax	# counter, tmp117
	cmpl	-80(%rbp), %eax	# length, tmp117
	jl	.L102	#,
# src/Runtime/runtime.c:396:     int32_t bufferSize = end - offset_str + 1;
	movq	-16(%rbp), %rax	# end, tmp118
	subq	-8(%rbp), %rax	# offset_str, _8
# src/Runtime/runtime.c:396:     int32_t bufferSize = end - offset_str + 1;
	addl	$1, %eax	#, _10
# src/Runtime/runtime.c:396:     int32_t bufferSize = end - offset_str + 1;
	movl	%eax, -36(%rbp)	# _10, bufferSize
# src/Runtime/runtime.c:397:     uint8_t *buffer = malloc(bufferSize);
	movl	-36(%rbp), %eax	# bufferSize, tmp119
	cltq
	movq	%rax, %rdi	# _11,
	call	malloc@PLT	#
	movq	%rax, -48(%rbp)	# tmp120, buffer
# src/Runtime/runtime.c:398:     u8_strncpy(buffer, offset_str, bufferSize);
	movl	-36(%rbp), %eax	# bufferSize, tmp121
	movslq	%eax, %rdx	# tmp121, _12
	movq	-8(%rbp), %rcx	# offset_str, tmp122
	movq	-48(%rbp), %rax	# buffer, tmp123
	movq	%rcx, %rsi	# tmp122,
	movq	%rax, %rdi	# tmp123,
	call	u8_strncpy@PLT	#
# src/Runtime/runtime.c:399:     buffer[bufferSize - 1] = 0;
	movl	-36(%rbp), %eax	# bufferSize, tmp124
	cltq
	leaq	-1(%rax), %rdx	#, _14
	movq	-48(%rbp), %rax	# buffer, tmp125
	addq	%rdx, %rax	# _14, _15
# src/Runtime/runtime.c:399:     buffer[bufferSize - 1] = 0;
	movb	$0, (%rax)	#, *_15
# src/Runtime/runtime.c:400:     obj ret = __createString(buffer);
	movq	-48(%rbp), %rax	# buffer, tmp126
	movq	%rax, %rdi	# tmp126,
	call	__createString	#
	movq	%rax, -56(%rbp)	# tmp127, ret
# src/Runtime/runtime.c:401:     __incRef(ret);
	movq	-56(%rbp), %rax	# ret, tmp128
	movq	%rax, %rdi	# tmp128,
	call	__incRef	#
# src/Runtime/runtime.c:402:     free(buffer);
	movq	-48(%rbp), %rax	# buffer, tmp129
	movq	%rax, %rdi	# tmp129,
	call	free@PLT	#
# src/Runtime/runtime.c:403:     return ret;
	movq	-56(%rbp), %rax	# ret, _20
.L103:
# src/Runtime/runtime.c:404: }
	leave	
	.cfi_def_cfa 7, 8
	ret	
	.cfi_endproc
.LFE24:
	.size	_String_substring, .-_String_substring
	.globl	_String_length
	.type	_String_length, @function
_String_length:
.LFB25:
	.cfi_startproc
	pushq	%rbp	#
	.cfi_def_cfa_offset 16
	.cfi_offset 6, -16
	movq	%rsp, %rbp	#,
	.cfi_def_cfa_register 6
	subq	$16, %rsp	#,
	movq	%rdi, -8(%rbp)	# str, str
# src/Runtime/runtime.c:406:     if (str->length < 0) {
	movq	-8(%rbp), %rax	# str, tmp90
	movl	32(%rax), %eax	# str_9(D)->length, _1
# src/Runtime/runtime.c:406:     if (str->length < 0) {
	testl	%eax, %eax	# _1
	jns	.L105	#,
# src/Runtime/runtime.c:407:         str->length = u8_mbsnlen(str->data, u8_strlen(str->data));
	movq	-8(%rbp), %rax	# str, tmp91
	movq	8(%rax), %rax	# str_9(D)->data, _2
# src/Runtime/runtime.c:407:         str->length = u8_mbsnlen(str->data, u8_strlen(str->data));
	movq	%rax, %rdi	# _2,
	call	u8_strlen@PLT	#
	movq	%rax, %rdx	#, _3
# src/Runtime/runtime.c:407:         str->length = u8_mbsnlen(str->data, u8_strlen(str->data));
	movq	-8(%rbp), %rax	# str, tmp92
	movq	8(%rax), %rax	# str_9(D)->data, _4
# src/Runtime/runtime.c:407:         str->length = u8_mbsnlen(str->data, u8_strlen(str->data));
	movq	%rdx, %rsi	# _3,
	movq	%rax, %rdi	# _4,
	call	u8_mbsnlen@PLT	#
# src/Runtime/runtime.c:407:         str->length = u8_mbsnlen(str->data, u8_strlen(str->data));
	movl	%eax, %edx	# _5, _6
	movq	-8(%rbp), %rax	# str, tmp93
	movl	%edx, 32(%rax)	# _6, str_9(D)->length
.L105:
# src/Runtime/runtime.c:409:     return str->length;
	movq	-8(%rbp), %rax	# str, tmp94
	movl	32(%rax), %eax	# str_9(D)->length, _11
# src/Runtime/runtime.c:410: }
	leave	
	.cfi_def_cfa 7, 8
	ret	
	.cfi_endproc
.LFE25:
	.size	_String_length, .-_String_length
	.section	.rodata
	.align 8
.LC17:
	.string	"ERROR: IndexOf null substring argument."
	.align 8
.LC18:
	.string	"ERROR: IndexOf starting index is too big."
	.text
	.globl	_String_indexOf
	.type	_String_indexOf, @function
_String_indexOf:
.LFB26:
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
# src/Runtime/runtime.c:412:     if (IS_NULL(substr)) {
	leaq	_LAT_NULL(%rip), %rax	#, tmp95
	cmpq	%rax, -80(%rbp)	# tmp95, substr
	jne	.L108	#,
# src/Runtime/runtime.c:413:         errMsg = "ERROR: IndexOf null substring argument.";
	leaq	.LC17(%rip), %rax	#, tmp96
	movq	%rax, errMsg(%rip)	# tmp96, errMsg
# src/Runtime/runtime.c:414:         error();
	movl	$0, %eax	#,
	call	error	#
.L108:
# src/Runtime/runtime.c:416:     if (startFrom >= _String_length(str)) {
	movq	-72(%rbp), %rax	# str, tmp97
	movq	%rax, %rdi	# tmp97,
	call	_String_length	#
# src/Runtime/runtime.c:416:     if (startFrom >= _String_length(str)) {
	cmpl	%eax, -84(%rbp)	# _1, startFrom
	jl	.L109	#,
# src/Runtime/runtime.c:417:         errMsg = "ERROR: IndexOf starting index is too big.";
	leaq	.LC18(%rip), %rax	#, tmp98
	movq	%rax, errMsg(%rip)	# tmp98, errMsg
# src/Runtime/runtime.c:418:         error();
	movl	$0, %eax	#,
	call	error	#
.L109:
# src/Runtime/runtime.c:420:     if (_String_length(str) < _String_length(substr))
	movq	-72(%rbp), %rax	# str, tmp99
	movq	%rax, %rdi	# tmp99,
	call	_String_length	#
	movl	%eax, %ebx	#, _2
# src/Runtime/runtime.c:420:     if (_String_length(str) < _String_length(substr))
	movq	-80(%rbp), %rax	# substr, tmp100
	movq	%rax, %rdi	# tmp100,
	call	_String_length	#
# src/Runtime/runtime.c:420:     if (_String_length(str) < _String_length(substr))
	cmpl	%eax, %ebx	# _3, _2
	jge	.L110	#,
# src/Runtime/runtime.c:421:         return -1;
	movl	$-1, %eax	#, _14
	jmp	.L117	#
.L110:
# src/Runtime/runtime.c:422:     uint8_t *rs = (str->data);
	movq	-72(%rbp), %rax	# str, tmp101
	movq	8(%rax), %rax	# str_24(D)->data, tmp102
	movq	%rax, -24(%rbp)	# tmp102, rs
# src/Runtime/runtime.c:423:     uint8_t *rsub = (substr->data);
	movq	-80(%rbp), %rax	# substr, tmp103
	movq	8(%rax), %rax	# substr_20(D)->data, tmp104
	movq	%rax, -48(%rbp)	# tmp104, rsub
# src/Runtime/runtime.c:424:     uint8_t *start = rs;
	movq	-24(%rbp), %rax	# rs, tmp105
	movq	%rax, -32(%rbp)	# tmp105, start
# src/Runtime/runtime.c:426:     while (startFrom-- > 0) {
	jmp	.L112	#
.L114:
# src/Runtime/runtime.c:427:         if (u8_next(&c, start) == NULL)
	movq	-32(%rbp), %rdx	# start, tmp106
	leaq	-60(%rbp), %rax	#, tmp107
	movq	%rdx, %rsi	# tmp106,
	movq	%rax, %rdi	# tmp107,
	call	u8_next@PLT	#
# src/Runtime/runtime.c:427:         if (u8_next(&c, start) == NULL)
	testq	%rax, %rax	# _4
	jne	.L113	#,
# src/Runtime/runtime.c:428:             return -1;
	movl	$-1, %eax	#, _14
	jmp	.L117	#
.L113:
# src/Runtime/runtime.c:429:         start += u8_next(&c, start) - start;
	movq	-32(%rbp), %rdx	# start, tmp108
	leaq	-60(%rbp), %rax	#, tmp109
	movq	%rdx, %rsi	# tmp108,
	movq	%rax, %rdi	# tmp109,
	call	u8_next@PLT	#
# src/Runtime/runtime.c:429:         start += u8_next(&c, start) - start;
	subq	-32(%rbp), %rax	# start, _44
# src/Runtime/runtime.c:429:         start += u8_next(&c, start) - start;
	addq	%rax, -32(%rbp)	# _6, start
.L112:
# src/Runtime/runtime.c:426:     while (startFrom-- > 0) {
	movl	-84(%rbp), %eax	# startFrom, startFrom.87_7
	leal	-1(%rax), %edx	#, tmp110
	movl	%edx, -84(%rbp)	# tmp110, startFrom
# src/Runtime/runtime.c:426:     while (startFrom-- > 0) {
	testl	%eax, %eax	# startFrom.87_7
	jg	.L114	#,
# src/Runtime/runtime.c:431:     uint8_t *index = u8_strstr(start, rsub);
	movq	-48(%rbp), %rdx	# rsub, tmp111
	movq	-32(%rbp), %rax	# start, tmp112
	movq	%rdx, %rsi	# tmp111,
	movq	%rax, %rdi	# tmp112,
	call	u8_strstr@PLT	#
	movq	%rax, -56(%rbp)	# tmp113, index
# src/Runtime/runtime.c:432:     uint32_t counter = 0;
	movl	$0, -36(%rbp)	#, counter
# src/Runtime/runtime.c:433:     while ((rs += u8_next(&c, rs) - rs) != index)
	jmp	.L115	#
.L116:
# src/Runtime/runtime.c:434:         counter++;
	addl	$1, -36(%rbp)	#, counter
.L115:
# src/Runtime/runtime.c:433:     while ((rs += u8_next(&c, rs) - rs) != index)
	movq	-24(%rbp), %rdx	# rs, tmp114
	leaq	-60(%rbp), %rax	#, tmp115
	movq	%rdx, %rsi	# tmp114,
	movq	%rax, %rdi	# tmp115,
	call	u8_next@PLT	#
# src/Runtime/runtime.c:433:     while ((rs += u8_next(&c, rs) - rs) != index)
	subq	-24(%rbp), %rax	# rs, _38
# src/Runtime/runtime.c:433:     while ((rs += u8_next(&c, rs) - rs) != index)
	addq	%rax, -24(%rbp)	# _9, rs
# src/Runtime/runtime.c:433:     while ((rs += u8_next(&c, rs) - rs) != index)
	movq	-24(%rbp), %rax	# rs, tmp116
	cmpq	-56(%rbp), %rax	# index, tmp116
	jne	.L116	#,
# src/Runtime/runtime.c:435:     return counter;
	movl	-36(%rbp), %eax	# counter, _14
.L117:
# src/Runtime/runtime.c:436: }
	movq	-8(%rbp), %rbx	#,
	leave	
	.cfi_def_cfa 7, 8
	ret	
	.cfi_endproc
.LFE26:
	.size	_String_indexOf, .-_String_indexOf
	.globl	_String_getBytes
	.type	_String_getBytes, @function
_String_getBytes:
.LFB27:
	.cfi_startproc
	pushq	%rbp	#
	.cfi_def_cfa_offset 16
	.cfi_offset 6, -16
	movq	%rsp, %rbp	#,
	.cfi_def_cfa_register 6
	subq	$48, %rsp	#,
	movq	%rdi, -40(%rbp)	# str, str
# src/Runtime/runtime.c:438:     uint8_t *rs = (str->data);
	movq	-40(%rbp), %rax	# str, tmp89
	movq	8(%rax), %rax	# str_7(D)->data, tmp90
	movq	%rax, -8(%rbp)	# tmp90, rs
# src/Runtime/runtime.c:439:     int32_t len = rs == NULL ? 0 : u8_strlen(rs);
	cmpq	$0, -8(%rbp)	#, rs
	je	.L119	#,
# src/Runtime/runtime.c:439:     int32_t len = rs == NULL ? 0 : u8_strlen(rs);
	movq	-8(%rbp), %rax	# rs, tmp91
	movq	%rax, %rdi	# tmp91,
	call	u8_strlen@PLT	#
	jmp	.L120	#
.L119:
# src/Runtime/runtime.c:439:     int32_t len = rs == NULL ? 0 : u8_strlen(rs);
	movl	$0, %eax	#, iftmp.88_5
.L120:
# src/Runtime/runtime.c:439:     int32_t len = rs == NULL ? 0 : u8_strlen(rs);
	movl	%eax, -12(%rbp)	# iftmp.88_5, len
# src/Runtime/runtime.c:440:     obj arr = __newByteArray(len + 1);
	movl	-12(%rbp), %eax	# len, tmp92
	addl	$1, %eax	#, _2
	movl	%eax, %edi	# _2,
	call	__newByteArray	#
	movq	%rax, -24(%rbp)	# tmp93, arr
# src/Runtime/runtime.c:441:     memcpy((arr->data), rs, len);
	movl	-12(%rbp), %eax	# len, tmp94
	movslq	%eax, %rdx	# tmp94, _3
	movq	-24(%rbp), %rax	# arr, tmp95
	movq	8(%rax), %rax	# arr_13->data, _4
	movq	-8(%rbp), %rcx	# rs, tmp96
	movq	%rcx, %rsi	# tmp96,
	movq	%rax, %rdi	# _4,
	call	memcpy@PLT	#
# src/Runtime/runtime.c:442:     __incRef(arr);
	movq	-24(%rbp), %rax	# arr, tmp97
	movq	%rax, %rdi	# tmp97,
	call	__incRef	#
# src/Runtime/runtime.c:443:     return arr;
	movq	-24(%rbp), %rax	# arr, _16
# src/Runtime/runtime.c:444: }
	leave	
	.cfi_def_cfa 7, 8
	ret	
	.cfi_endproc
.LFE27:
	.size	_String_getBytes, .-_String_getBytes
	.section	.rodata
	.align 8
.LC19:
	.string	"ERROR: EndsWith null substring argument."
	.text
	.globl	_String_endsWith
	.type	_String_endsWith, @function
_String_endsWith:
.LFB28:
	.cfi_startproc
	pushq	%rbp	#
	.cfi_def_cfa_offset 16
	.cfi_offset 6, -16
	movq	%rsp, %rbp	#,
	.cfi_def_cfa_register 6
	subq	$32, %rsp	#,
	movq	%rdi, -24(%rbp)	# str, str
	movq	%rsi, -32(%rbp)	# substr, substr
# src/Runtime/runtime.c:446:     if (IS_NULL(substr)) {
	leaq	_LAT_NULL(%rip), %rax	#, tmp85
	cmpq	%rax, -32(%rbp)	# tmp85, substr
	jne	.L123	#,
# src/Runtime/runtime.c:447:         errMsg = "ERROR: EndsWith null substring argument.";
	leaq	.LC19(%rip), %rax	#, tmp86
	movq	%rax, errMsg(%rip)	# tmp86, errMsg
# src/Runtime/runtime.c:448:         error();
	movl	$0, %eax	#,
	call	error	#
.L123:
# src/Runtime/runtime.c:450:     uint8_t *rs = (str->data);
	movq	-24(%rbp), %rax	# str, tmp87
	movq	8(%rax), %rax	# str_7(D)->data, tmp88
	movq	%rax, -8(%rbp)	# tmp88, rs
# src/Runtime/runtime.c:451:     uint8_t *rsub = (substr->data);
	movq	-32(%rbp), %rax	# substr, tmp89
	movq	8(%rax), %rax	# substr_3(D)->data, tmp90
	movq	%rax, -16(%rbp)	# tmp90, rsub
# src/Runtime/runtime.c:452:     return u8_endswith(rs, rsub);
	movq	-16(%rbp), %rdx	# rsub, tmp91
	movq	-8(%rbp), %rax	# rs, tmp92
	movq	%rdx, %rsi	# tmp91,
	movq	%rax, %rdi	# tmp92,
	call	u8_endswith@PLT	#
# src/Runtime/runtime.c:453: }
	leave	
	.cfi_def_cfa 7, 8
	ret	
	.cfi_endproc
.LFE28:
	.size	_String_endsWith, .-_String_endsWith
	.section	.rodata
	.align 8
.LC20:
	.string	"ERROR: StartsWith null substring argument."
	.text
	.globl	_String_startsWith
	.type	_String_startsWith, @function
_String_startsWith:
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
# src/Runtime/runtime.c:455:     if (IS_NULL(substr)) {
	leaq	_LAT_NULL(%rip), %rax	#, tmp85
	cmpq	%rax, -32(%rbp)	# tmp85, substr
	jne	.L126	#,
# src/Runtime/runtime.c:456:         errMsg = "ERROR: StartsWith null substring argument.";
	leaq	.LC20(%rip), %rax	#, tmp86
	movq	%rax, errMsg(%rip)	# tmp86, errMsg
# src/Runtime/runtime.c:457:         error();
	movl	$0, %eax	#,
	call	error	#
.L126:
# src/Runtime/runtime.c:459:     uint8_t *rs = (str->data);
	movq	-24(%rbp), %rax	# str, tmp87
	movq	8(%rax), %rax	# str_7(D)->data, tmp88
	movq	%rax, -8(%rbp)	# tmp88, rs
# src/Runtime/runtime.c:460:     uint8_t *rsub = (substr->data);
	movq	-32(%rbp), %rax	# substr, tmp89
	movq	8(%rax), %rax	# substr_3(D)->data, tmp90
	movq	%rax, -16(%rbp)	# tmp90, rsub
# src/Runtime/runtime.c:461:     return u8_startswith(rs, rsub);
	movq	-16(%rbp), %rdx	# rsub, tmp91
	movq	-8(%rbp), %rax	# rs, tmp92
	movq	%rdx, %rsi	# tmp91,
	movq	%rax, %rdi	# tmp92,
	call	u8_startswith@PLT	#
# src/Runtime/runtime.c:462: }
	leave	
	.cfi_def_cfa 7, 8
	ret	
	.cfi_endproc
.LFE29:
	.size	_String_startsWith, .-_String_startsWith
	.globl	_String_concat
	.type	_String_concat, @function
_String_concat:
.LFB30:
	.cfi_startproc
	pushq	%rbp	#
	.cfi_def_cfa_offset 16
	.cfi_offset 6, -16
	movq	%rsp, %rbp	#,
	.cfi_def_cfa_register 6
	subq	$64, %rsp	#,
	movq	%rdi, -56(%rbp)	# str, str
	movq	%rsi, -64(%rbp)	# secondstr, secondstr
# src/Runtime/runtime.c:465:     if (IS_NULL(secondstr)) {
	leaq	_LAT_NULL(%rip), %rax	#, tmp94
	cmpq	%rax, -64(%rbp)	# tmp94, secondstr
	jne	.L129	#,
# src/Runtime/runtime.c:466:         __incRef(str);
	movq	-56(%rbp), %rax	# str, tmp95
	movq	%rax, %rdi	# tmp95,
	call	__incRef	#
# src/Runtime/runtime.c:467:         return str;
	movq	-56(%rbp), %rax	# str, _27
	jmp	.L130	#
.L129:
# src/Runtime/runtime.c:469:     uint8_t *rs1 = (str->data);
	movq	-56(%rbp), %rax	# str, tmp96
	movq	8(%rax), %rax	# str_23(D)->data, tmp97
	movq	%rax, -32(%rbp)	# tmp97, rs1
# src/Runtime/runtime.c:470:     uint8_t *rs2 = (secondstr->data);
	movq	-64(%rbp), %rax	# secondstr, tmp98
	movq	8(%rax), %rax	# secondstr_29(D)->data, tmp99
	movq	%rax, -40(%rbp)	# tmp99, rs2
# src/Runtime/runtime.c:472:     int32_t len1 = u8_strlen(rs1);
	movq	-32(%rbp), %rax	# rs1, tmp100
	movq	%rax, %rdi	# tmp100,
	call	u8_strlen@PLT	#
# src/Runtime/runtime.c:472:     int32_t len1 = u8_strlen(rs1);
	movl	%eax, -44(%rbp)	# _9, len1
# src/Runtime/runtime.c:473:     int32_t len2 = u8_strlen(rs2);
	movq	-40(%rbp), %rax	# rs2, tmp101
	movq	%rax, %rdi	# tmp101,
	call	u8_strlen@PLT	#
# src/Runtime/runtime.c:473:     int32_t len2 = u8_strlen(rs2);
	movl	%eax, -12(%rbp)	# _10, len2
# src/Runtime/runtime.c:474:     uint8_t *buffer = malloc(len1 + len2 + 1);
	movl	-44(%rbp), %edx	# len1, tmp102
	movl	-12(%rbp), %eax	# len2, tmp103
	addl	%edx, %eax	# tmp102, _11
# src/Runtime/runtime.c:474:     uint8_t *buffer = malloc(len1 + len2 + 1);
	addl	$1, %eax	#, _12
# src/Runtime/runtime.c:474:     uint8_t *buffer = malloc(len1 + len2 + 1);
	cltq
	movq	%rax, %rdi	# _13,
	call	malloc@PLT	#
	movq	%rax, -24(%rbp)	# tmp104, buffer
# src/Runtime/runtime.c:476:     u8_strcpy(buffer, rs1);
	movq	-32(%rbp), %rdx	# rs1, tmp105
	movq	-24(%rbp), %rax	# buffer, tmp106
	movq	%rdx, %rsi	# tmp105,
	movq	%rax, %rdi	# tmp106,
	call	u8_strcpy@PLT	#
# src/Runtime/runtime.c:477:     u8_strcpy(buffer + len1, rs2);
	movl	-44(%rbp), %eax	# len1, tmp107
	movslq	%eax, %rdx	# tmp107, _18
	movq	-24(%rbp), %rax	# buffer, tmp108
	addq	%rax, %rdx	# tmp108, _19
	movq	-40(%rbp), %rax	# rs2, tmp109
	movq	%rax, %rsi	# tmp109,
	movq	%rdx, %rdi	# _19,
	call	u8_strcpy@PLT	#
# src/Runtime/runtime.c:478:     buffer[len1 + len2] = 0;
	movl	-44(%rbp), %edx	# len1, tmp110
	movl	-12(%rbp), %eax	# len2, tmp111
	addl	%edx, %eax	# tmp110, _20
	movslq	%eax, %rdx	# _20, _21
# src/Runtime/runtime.c:478:     buffer[len1 + len2] = 0;
	movq	-24(%rbp), %rax	# buffer, tmp112
	addq	%rdx, %rax	# _21, _22
# src/Runtime/runtime.c:478:     buffer[len1 + len2] = 0;
	movb	$0, (%rax)	#, *_22
# src/Runtime/runtime.c:480:     obj ret = __createString(buffer);
	movq	-24(%rbp), %rax	# buffer, tmp113
	movq	%rax, %rdi	# tmp113,
	call	__createString	#
	movq	%rax, -8(%rbp)	# tmp114, ret
# src/Runtime/runtime.c:481:     __incRef(ret);
	movq	-8(%rbp), %rax	# ret, tmp115
	movq	%rax, %rdi	# tmp115,
	call	__incRef	#
# src/Runtime/runtime.c:482:     free(buffer);
	movq	-24(%rbp), %rax	# buffer, tmp116
	movq	%rax, %rdi	# tmp116,
	call	free@PLT	#
# src/Runtime/runtime.c:484:     return ret;
	movq	-8(%rbp), %rax	# ret, _27
.L130:
# src/Runtime/runtime.c:485: }
	leave	
	.cfi_def_cfa 7, 8
	ret	
	.cfi_endproc
.LFE30:
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
.LFB31:
	.cfi_startproc
	pushq	%rbp	#
	.cfi_def_cfa_offset 16
	.cfi_offset 6, -16
	movq	%rsp, %rbp	#,
	.cfi_def_cfa_register 6
	subq	$32, %rsp	#,
	movq	%rdi, -24(%rbp)	# str, str
	movl	%esi, -28(%rbp)	# index, index
# src/Runtime/runtime.c:489:     uint8_t *rs = (str->data);
	movq	-24(%rbp), %rax	# str, tmp91
	movq	8(%rax), %rax	# str_13(D)->data, tmp92
	movq	%rax, -8(%rbp)	# tmp92, rs
# src/Runtime/runtime.c:491:     while (index-- > 0) {
	jmp	.L132	#
.L134:
# src/Runtime/runtime.c:492:         if (u8_next(&c, rs) == NULL) {
	movq	-8(%rbp), %rdx	# rs, tmp93
	leaq	-12(%rbp), %rax	#, tmp94
	movq	%rdx, %rsi	# tmp93,
	movq	%rax, %rdi	# tmp94,
	call	u8_next@PLT	#
# src/Runtime/runtime.c:492:         if (u8_next(&c, rs) == NULL) {
	testq	%rax, %rax	# _1
	jne	.L133	#,
# src/Runtime/runtime.c:493:             errMsg = charAtErr;
	leaq	charAtErr(%rip), %rax	#, tmp95
	movq	%rax, errMsg(%rip)	# tmp95, errMsg
# src/Runtime/runtime.c:494:             error();
	movl	$0, %eax	#,
	call	error	#
.L133:
# src/Runtime/runtime.c:496:         rs += u8_next(&c, rs) - rs;
	movq	-8(%rbp), %rdx	# rs, tmp96
	leaq	-12(%rbp), %rax	#, tmp97
	movq	%rdx, %rsi	# tmp96,
	movq	%rax, %rdi	# tmp97,
	call	u8_next@PLT	#
# src/Runtime/runtime.c:496:         rs += u8_next(&c, rs) - rs;
	subq	-8(%rbp), %rax	# rs, _26
# src/Runtime/runtime.c:496:         rs += u8_next(&c, rs) - rs;
	addq	%rax, -8(%rbp)	# _3, rs
.L132:
# src/Runtime/runtime.c:491:     while (index-- > 0) {
	movl	-28(%rbp), %eax	# index, index.109_4
	leal	-1(%rax), %edx	#, tmp98
	movl	%edx, -28(%rbp)	# tmp98, index
# src/Runtime/runtime.c:491:     while (index-- > 0) {
	testl	%eax, %eax	# index.109_4
	jg	.L134	#,
# src/Runtime/runtime.c:498:     if (u8_strmbtouc(&c, rs) <= 0) {
	movq	-8(%rbp), %rdx	# rs, tmp99
	leaq	-12(%rbp), %rax	#, tmp100
	movq	%rdx, %rsi	# tmp99,
	movq	%rax, %rdi	# tmp100,
	call	u8_strmbtouc@PLT	#
# src/Runtime/runtime.c:498:     if (u8_strmbtouc(&c, rs) <= 0) {
	testl	%eax, %eax	# _5
	jg	.L135	#,
# src/Runtime/runtime.c:499:         errMsg = charAtErr;
	leaq	charAtErr(%rip), %rax	#, tmp101
	movq	%rax, errMsg(%rip)	# tmp101, errMsg
# src/Runtime/runtime.c:500:         error();
	movl	$0, %eax	#,
	call	error	#
.L135:
# src/Runtime/runtime.c:502:     return c;
	movl	-12(%rbp), %eax	# c, c.110_6
# src/Runtime/runtime.c:503: }
	leave	
	.cfi_def_cfa 7, 8
	ret	
	.cfi_endproc
.LFE31:
	.size	_String_charAt, .-_String_charAt
	.globl	ddd
	.type	ddd, @function
ddd:
.LFB32:
	.cfi_startproc
	pushq	%rbp	#
	.cfi_def_cfa_offset 16
	.cfi_offset 6, -16
	movq	%rsp, %rbp	#,
	.cfi_def_cfa_register 6
	movq	%rdi, -8(%rbp)	# str, str
# src/Runtime/runtime.c:507: }
	nop	
	popq	%rbp	#
	.cfi_def_cfa 7, 8
	ret	
	.cfi_endproc
.LFE32:
	.size	ddd, .-ddd
	.globl	printString
	.type	printString, @function
printString:
.LFB33:
	.cfi_startproc
	pushq	%rbp	#
	.cfi_def_cfa_offset 16
	.cfi_offset 6, -16
	movq	%rsp, %rbp	#,
	.cfi_def_cfa_register 6
	subq	$32, %rsp	#,
	movq	%rdi, -24(%rbp)	# str, str
# src/Runtime/runtime.c:513:     if (IS_NULL(str))
	leaq	_LAT_NULL(%rip), %rax	#, tmp84
	cmpq	%rax, -24(%rbp)	# tmp84, str
	jne	.L139	#,
# src/Runtime/runtime.c:514:         str = __createString("null");
	leaq	.LC2(%rip), %rax	#, tmp85
	movq	%rax, %rdi	# tmp85,
	call	__createString	#
	movq	%rax, -24(%rbp)	# tmp86, str
.L139:
# src/Runtime/runtime.c:515:     __incRef(str);
	movq	-24(%rbp), %rax	# str, tmp87
	movq	%rax, %rdi	# tmp87,
	call	__incRef	#
# src/Runtime/runtime.c:516:     uint8_t *rs = (str->data);
	movq	-24(%rbp), %rax	# str, tmp88
	movq	8(%rax), %rax	# str_8->data, tmp89
	movq	%rax, -8(%rbp)	# tmp89, rs
# src/Runtime/runtime.c:518:     printf("%s\n", rs);
	movq	-8(%rbp), %rax	# rs, tmp90
	movq	%rax, %rdi	# tmp90,
	call	puts@PLT	#
# src/Runtime/runtime.c:519:     __decRef(str);
	movq	-24(%rbp), %rax	# str, tmp91
	movq	%rax, %rdi	# tmp91,
	call	__decRef	#
# src/Runtime/runtime.c:521:     return 0;
	movl	$0, %eax	#, _3
# src/Runtime/runtime.c:522: }
	leave	
	.cfi_def_cfa 7, 8
	ret	
	.cfi_endproc
.LFE33:
	.size	printString, .-printString
	.section	.rodata
.LC21:
	.string	"%d\n"
	.text
	.globl	printInt
	.type	printInt, @function
printInt:
.LFB34:
	.cfi_startproc
	pushq	%rbp	#
	.cfi_def_cfa_offset 16
	.cfi_offset 6, -16
	movq	%rsp, %rbp	#,
	.cfi_def_cfa_register 6
	subq	$16, %rsp	#,
	movl	%edi, -4(%rbp)	# i, i
# src/Runtime/runtime.c:525:     printf("%d\n", i);
	movl	-4(%rbp), %eax	# i, tmp84
	movl	%eax, %esi	# tmp84,
	leaq	.LC21(%rip), %rax	#, tmp85
	movq	%rax, %rdi	# tmp85,
	movl	$0, %eax	#,
	call	printf@PLT	#
# src/Runtime/runtime.c:527:     return 0;
	movl	$0, %eax	#, _4
# src/Runtime/runtime.c:528: }
	leave	
	.cfi_def_cfa 7, 8
	ret	
	.cfi_endproc
.LFE34:
	.size	printInt, .-printInt
	.globl	printBoolean
	.type	printBoolean, @function
printBoolean:
.LFB35:
	.cfi_startproc
	pushq	%rbp	#
	.cfi_def_cfa_offset 16
	.cfi_offset 6, -16
	movq	%rsp, %rbp	#,
	.cfi_def_cfa_register 6
	subq	$16, %rsp	#,
	movl	%edi, %eax	# b, tmp84
	movb	%al, -4(%rbp)	# tmp85, b
# src/Runtime/runtime.c:531:     if (b)
	cmpb	$0, -4(%rbp)	#, b
	je	.L144	#,
# src/Runtime/runtime.c:532:         printf("true\n");
	leaq	.LC8(%rip), %rax	#, tmp86
	movq	%rax, %rdi	# tmp86,
	call	puts@PLT	#
	jmp	.L145	#
.L144:
# src/Runtime/runtime.c:534:         printf("false\n");
	leaq	.LC9(%rip), %rax	#, tmp87
	movq	%rax, %rdi	# tmp87,
	call	puts@PLT	#
.L145:
# src/Runtime/runtime.c:536:     return 0;
	movl	$0, %eax	#, _2
# src/Runtime/runtime.c:537: }
	leave	
	.cfi_def_cfa 7, 8
	ret	
	.cfi_endproc
.LFE35:
	.size	printBoolean, .-printBoolean
	.globl	intToString
	.type	intToString, @function
intToString:
.LFB36:
	.cfi_startproc
	pushq	%rbp	#
	.cfi_def_cfa_offset 16
	.cfi_offset 6, -16
	movq	%rsp, %rbp	#,
	.cfi_def_cfa_register 6
	subq	$48, %rsp	#,
	movl	%edi, -36(%rbp)	# i, i
# src/Runtime/runtime.c:541:     sprintf(buffer, "%d", i);
	movl	-36(%rbp), %edx	# i, tmp84
	leaq	-19(%rbp), %rax	#, tmp85
	leaq	.LC7(%rip), %rcx	#, tmp86
	movq	%rcx, %rsi	# tmp86,
	movq	%rax, %rdi	# tmp85,
	movl	$0, %eax	#,
	call	sprintf@PLT	#
# src/Runtime/runtime.c:542:     obj ret = __createString(buffer);
	leaq	-19(%rbp), %rax	#, tmp87
	movq	%rax, %rdi	# tmp87,
	call	__createString	#
	movq	%rax, -8(%rbp)	# tmp88, ret
# src/Runtime/runtime.c:543:     __incRef(ret);
	movq	-8(%rbp), %rax	# ret, tmp89
	movq	%rax, %rdi	# tmp89,
	call	__incRef	#
# src/Runtime/runtime.c:545:     return ret;
	movq	-8(%rbp), %rax	# ret, _3
# src/Runtime/runtime.c:546: }
	leave	
	.cfi_def_cfa 7, 8
	ret	
	.cfi_endproc
.LFE36:
	.size	intToString, .-intToString
	.section	.rodata
.LC22:
	.string	"%u"
	.text
	.globl	byteToString
	.type	byteToString, @function
byteToString:
.LFB37:
	.cfi_startproc
	pushq	%rbp	#
	.cfi_def_cfa_offset 16
	.cfi_offset 6, -16
	movq	%rsp, %rbp	#,
	.cfi_def_cfa_register 6
	subq	$48, %rsp	#,
	movl	%edi, %eax	# i, tmp85
	movb	%al, -36(%rbp)	# tmp86, i
# src/Runtime/runtime.c:551:     sprintf(buffer, "%u", i);
	movzbl	-36(%rbp), %edx	# i, _5
	leaq	-19(%rbp), %rax	#, tmp87
	leaq	.LC22(%rip), %rcx	#, tmp88
	movq	%rcx, %rsi	# tmp88,
	movq	%rax, %rdi	# tmp87,
	movl	$0, %eax	#,
	call	sprintf@PLT	#
# src/Runtime/runtime.c:552:     obj ret = __createString(buffer);
	leaq	-19(%rbp), %rax	#, tmp89
	movq	%rax, %rdi	# tmp89,
	call	__createString	#
	movq	%rax, -8(%rbp)	# tmp90, ret
# src/Runtime/runtime.c:553:     __incRef(ret);
	movq	-8(%rbp), %rax	# ret, tmp91
	movq	%rax, %rdi	# tmp91,
	call	__incRef	#
# src/Runtime/runtime.c:555:     return ret;
	movq	-8(%rbp), %rax	# ret, _3
# src/Runtime/runtime.c:556: }
	leave	
	.cfi_def_cfa 7, 8
	ret	
	.cfi_endproc
.LFE37:
	.size	byteToString, .-byteToString
	.globl	boolToString
	.type	boolToString, @function
boolToString:
.LFB38:
	.cfi_startproc
	pushq	%rbp	#
	.cfi_def_cfa_offset 16
	.cfi_offset 6, -16
	movq	%rsp, %rbp	#,
	.cfi_def_cfa_register 6
	subq	$32, %rsp	#,
	movl	%edi, %eax	# b, tmp84
	movb	%al, -20(%rbp)	# tmp85, b
# src/Runtime/runtime.c:561:     if (b)
	cmpb	$0, -20(%rbp)	#, b
	je	.L152	#,
# src/Runtime/runtime.c:562:         ret = __createString("true");
	leaq	.LC8(%rip), %rax	#, tmp86
	movq	%rax, %rdi	# tmp86,
	call	__createString	#
	movq	%rax, -8(%rbp)	# tmp87, ret
	jmp	.L153	#
.L152:
# src/Runtime/runtime.c:564:         ret = __createString("false");
	leaq	.LC9(%rip), %rax	#, tmp88
	movq	%rax, %rdi	# tmp88,
	call	__createString	#
	movq	%rax, -8(%rbp)	# tmp89, ret
.L153:
# src/Runtime/runtime.c:565:     __incRef(ret);
	movq	-8(%rbp), %rax	# ret, tmp90
	movq	%rax, %rdi	# tmp90,
	call	__incRef	#
# src/Runtime/runtime.c:567:     return ret;
	movq	-8(%rbp), %rax	# ret, _10
# src/Runtime/runtime.c:568: }
	leave	
	.cfi_def_cfa 7, 8
	ret	
	.cfi_endproc
.LFE38:
	.size	boolToString, .-boolToString
	.globl	print
	.type	print, @function
print:
.LFB39:
	.cfi_startproc
	pushq	%rbp	#
	.cfi_def_cfa_offset 16
	.cfi_offset 6, -16
	movq	%rsp, %rbp	#,
	.cfi_def_cfa_register 6
	subq	$32, %rsp	#,
	movq	%rdi, -24(%rbp)	# o, o
# src/Runtime/runtime.c:573:     if (IS_NULL(o)) {
	leaq	_LAT_NULL(%rip), %rax	#, tmp87
	cmpq	%rax, -24(%rbp)	# tmp87, o
	jne	.L156	#,
# src/Runtime/runtime.c:575:         o = __createString("null");
	leaq	.LC2(%rip), %rax	#, tmp88
	movq	%rax, %rdi	# tmp88,
	call	__createString	#
	movq	%rax, -24(%rbp)	# tmp89, o
.L156:
# src/Runtime/runtime.c:578:     obj (*toStr)(obj) = ((void **)o->type->methods)[0];
	movq	-24(%rbp), %rax	# o, tmp90
	movq	(%rax), %rax	# o_11->type, _4
# src/Runtime/runtime.c:578:     obj (*toStr)(obj) = ((void **)o->type->methods)[0];
	movq	20(%rax), %rax	# _4->methods, _5
# src/Runtime/runtime.c:578:     obj (*toStr)(obj) = ((void **)o->type->methods)[0];
	movq	(%rax), %rax	# MEM[(void * *)_5], _6
# src/Runtime/runtime.c:578:     obj (*toStr)(obj) = ((void **)o->type->methods)[0];
	movq	%rax, -16(%rbp)	# _6, toStr
# src/Runtime/runtime.c:579:     obj str = toStr(o);
	movq	-24(%rbp), %rax	# o, tmp91
	movq	-16(%rbp), %rdx	# toStr, tmp92
	movq	%rax, %rdi	# tmp91,
	call	*%rdx	# tmp92
	movq	%rax, -8(%rbp)	# tmp93, str
# src/Runtime/runtime.c:581:     printString(str);
	movq	-8(%rbp), %rax	# str, tmp94
	movq	%rax, %rdi	# tmp94,
	call	printString	#
# src/Runtime/runtime.c:585:     return 0;
	movl	$0, %eax	#, _3
# src/Runtime/runtime.c:586: }
	leave	
	.cfi_def_cfa 7, 8
	ret	
	.cfi_endproc
.LFE39:
	.size	print, .-print
	.globl	printBinArray
	.type	printBinArray, @function
printBinArray:
.LFB40:
	.cfi_startproc
	pushq	%rbp	#
	.cfi_def_cfa_offset 16
	.cfi_offset 6, -16
	movq	%rsp, %rbp	#,
	.cfi_def_cfa_register 6
	subq	$16, %rsp	#,
	movq	%rdi, -8(%rbp)	# arr, arr
# src/Runtime/runtime.c:590:     if (IS_NULL(arr)){
	leaq	_LAT_NULL(%rip), %rax	#, tmp88
	cmpq	%rax, -8(%rbp)	# tmp88, arr
	jne	.L159	#,
# src/Runtime/runtime.c:591:         print(arr);
	movq	-8(%rbp), %rax	# arr, tmp89
	movq	%rax, %rdi	# tmp89,
	call	print	#
# src/Runtime/runtime.c:592:         return 0;
	movl	$0, %eax	#, _9
	jmp	.L160	#
.L159:
# src/Runtime/runtime.c:594:     __incRef(arr);
	movq	-8(%rbp), %rax	# arr, tmp90
	movq	%rax, %rdi	# tmp90,
	call	__incRef	#
# src/Runtime/runtime.c:595:     fwrite(arr->data, sizeof(int8_t), arr->length, stdout);
	movq	stdout(%rip), %rcx	# stdout, stdout.195_5
# src/Runtime/runtime.c:595:     fwrite(arr->data, sizeof(int8_t), arr->length, stdout);
	movq	-8(%rbp), %rax	# arr, tmp91
	movl	32(%rax), %eax	# arr_11(D)->length, _6
# src/Runtime/runtime.c:595:     fwrite(arr->data, sizeof(int8_t), arr->length, stdout);
	movslq	%eax, %rdx	# _6, _7
# src/Runtime/runtime.c:595:     fwrite(arr->data, sizeof(int8_t), arr->length, stdout);
	movq	-8(%rbp), %rax	# arr, tmp92
	movq	8(%rax), %rax	# arr_11(D)->data, _8
# src/Runtime/runtime.c:595:     fwrite(arr->data, sizeof(int8_t), arr->length, stdout);
	movl	$1, %esi	#,
	movq	%rax, %rdi	# _8,
	call	fwrite@PLT	#
# src/Runtime/runtime.c:596:     __decRef(arr);
	movq	-8(%rbp), %rax	# arr, tmp93
	movq	%rax, %rdi	# tmp93,
	call	__decRef	#
# src/Runtime/runtime.c:598:     return 0;
	movl	$0, %eax	#, _9
.L160:
# src/Runtime/runtime.c:599: }
	leave	
	.cfi_def_cfa 7, 8
	ret	
	.cfi_endproc
.LFE40:
	.size	printBinArray, .-printBinArray
	.section	.rodata
.LC23:
	.string	"%s\n"
.LC24:
	.string	"ERROR: User error."
	.text
	.globl	error
	.type	error, @function
error:
.LFB41:
	.cfi_startproc
	pushq	%rbp	#
	.cfi_def_cfa_offset 16
	.cfi_offset 6, -16
	movq	%rsp, %rbp	#,
	.cfi_def_cfa_register 6
# src/Runtime/runtime.c:603:     if (errMsg != NULL)
	movq	errMsg(%rip), %rax	# errMsg, errMsg.204_2
# src/Runtime/runtime.c:603:     if (errMsg != NULL)
	testq	%rax, %rax	# errMsg.204_2
	je	.L162	#,
# src/Runtime/runtime.c:604:         fprintf(stderr, "%s\n", errMsg);
	movq	errMsg(%rip), %rdx	# errMsg, errMsg.205_3
	movq	stderr(%rip), %rax	# stderr, stderr.206_4
	leaq	.LC23(%rip), %rcx	#, tmp87
	movq	%rcx, %rsi	# tmp87,
	movq	%rax, %rdi	# stderr.206_4,
	movl	$0, %eax	#,
	call	fprintf@PLT	#
	jmp	.L163	#
.L162:
# src/Runtime/runtime.c:606:         fprintf(stderr, "%s\n", "ERROR: User error.");
	movq	stderr(%rip), %rax	# stderr, stderr.207_5
	leaq	.LC24(%rip), %rdx	#, tmp88
	leaq	.LC23(%rip), %rcx	#, tmp89
	movq	%rcx, %rsi	# tmp89,
	movq	%rax, %rdi	# stderr.207_5,
	movl	$0, %eax	#,
	call	fprintf@PLT	#
.L163:
# src/Runtime/runtime.c:608:     exit(1);
	movl	$1, %edi	#,
	call	exit@PLT	#
	.cfi_endproc
.LFE41:
	.size	error, .-error
	.section	.rodata
.LC25:
	.string	"%d "
	.text
	.globl	readInt
	.type	readInt, @function
readInt:
.LFB42:
	.cfi_startproc
	pushq	%rbp	#
	.cfi_def_cfa_offset 16
	.cfi_offset 6, -16
	movq	%rsp, %rbp	#,
	.cfi_def_cfa_register 6
	subq	$32, %rsp	#,
# src/Runtime/runtime.c:614:     char *line = NULL;
	movq	$0, -24(%rbp)	#, line
# src/Runtime/runtime.c:615:     size_t size = 0;
	movq	$0, -32(%rbp)	#, size
# src/Runtime/runtime.c:616:     ssize_t unused = getline(&line, &size, stdin);
	movq	stdin(%rip), %rdx	# stdin, stdin.212_1
	leaq	-32(%rbp), %rcx	#, tmp87
	leaq	-24(%rbp), %rax	#, tmp88
	movq	%rcx, %rsi	# tmp87,
	movq	%rax, %rdi	# tmp88,
	call	getline@PLT	#
	movq	%rax, -8(%rbp)	# tmp89, unused
# src/Runtime/runtime.c:617:     int unused2 = sscanf(line, "%d ", &i);
	movq	-24(%rbp), %rax	# line, line.213_2
	leaq	-16(%rbp), %rdx	#, tmp90
	leaq	.LC25(%rip), %rcx	#, tmp91
	movq	%rcx, %rsi	# tmp91,
	movq	%rax, %rdi	# line.213_2,
	movl	$0, %eax	#,
	call	__isoc99_sscanf@PLT	#
	movl	%eax, -12(%rbp)	# tmp92, unused2
# src/Runtime/runtime.c:618:     free(line);
	movq	-24(%rbp), %rax	# line, line.214_3
	movq	%rax, %rdi	# line.214_3,
	call	free@PLT	#
# src/Runtime/runtime.c:619:     return i;
	movl	-16(%rbp), %eax	# i, _12
# src/Runtime/runtime.c:620: }
	leave	
	.cfi_def_cfa 7, 8
	ret	
	.cfi_endproc
.LFE42:
	.size	readInt, .-readInt
	.globl	readString
	.type	readString, @function
readString:
.LFB43:
	.cfi_startproc
	pushq	%rbp	#
	.cfi_def_cfa_offset 16
	.cfi_offset 6, -16
	movq	%rsp, %rbp	#,
	.cfi_def_cfa_register 6
	subq	$32, %rsp	#,
# src/Runtime/runtime.c:622:     char *line = NULL;
	movq	$0, -24(%rbp)	#, line
# src/Runtime/runtime.c:623:     size_t size = 0;
	movq	$0, -32(%rbp)	#, size
# src/Runtime/runtime.c:624:     ssize_t unused = getline(&line, &size, stdin);
	movq	stdin(%rip), %rdx	# stdin, stdin.215_1
	leaq	-32(%rbp), %rcx	#, tmp93
	leaq	-24(%rbp), %rax	#, tmp94
	movq	%rcx, %rsi	# tmp93,
	movq	%rax, %rdi	# tmp94,
	call	getline@PLT	#
	movq	%rax, -8(%rbp)	# tmp95, unused
# src/Runtime/runtime.c:625:     size = u8_strlen(line);
	movq	-24(%rbp), %rax	# line, line.216_2
	movq	%rax, %rdi	# line.216_2,
	call	u8_strlen@PLT	#
# src/Runtime/runtime.c:625:     size = u8_strlen(line);
	movq	%rax, -32(%rbp)	# _3, size
# src/Runtime/runtime.c:626:     line[size - 1] = 0; // remove newline
	movq	-24(%rbp), %rax	# line, line.217_4
	movq	-32(%rbp), %rdx	# size, size.218_5
	subq	$1, %rdx	#, _6
	addq	%rdx, %rax	# _6, _7
# src/Runtime/runtime.c:626:     line[size - 1] = 0; // remove newline
	movb	$0, (%rax)	#, *_7
# src/Runtime/runtime.c:627:     obj l = __createString(line);
	movq	-24(%rbp), %rax	# line, line.219_8
	movq	%rax, %rdi	# line.219_8,
	call	__createString	#
	movq	%rax, -16(%rbp)	# tmp96, l
# src/Runtime/runtime.c:628:     __incRef(l);
	movq	-16(%rbp), %rax	# l, tmp97
	movq	%rax, %rdi	# tmp97,
	call	__incRef	#
# src/Runtime/runtime.c:629:     free(line);
	movq	-24(%rbp), %rax	# line, line.220_9
	movq	%rax, %rdi	# line.220_9,
	call	free@PLT	#
# src/Runtime/runtime.c:630:     return l;
	movq	-16(%rbp), %rax	# l, _21
# src/Runtime/runtime.c:631: }
	leave	
	.cfi_def_cfa 7, 8
	ret	
	.cfi_endproc
.LFE43:
	.size	readString, .-readString
	.local	gc_ref
	.comm	gc_ref,8,8
	.local	gc_ref_cap
	.comm	gc_ref_cap,4,4
	.local	gc_ref_length
	.comm	gc_ref_length,4,4
	.globl	__new
	.type	__new, @function
__new:
.LFB44:
	.cfi_startproc
	pushq	%rbp	#
	.cfi_def_cfa_offset 16
	.cfi_offset 6, -16
	movq	%rsp, %rbp	#,
	.cfi_def_cfa_register 6
	subq	$64, %rsp	#,
	movq	%rdi, -56(%rbp)	# t, t
# src/Runtime/runtime.c:641:     obj r = malloc(sizeof(struct Reference)+t->dataSize+10);
	movq	-56(%rbp), %rax	# t, tmp120
	movl	16(%rax), %eax	# t_67(D)->dataSize, _1
	cltq
# src/Runtime/runtime.c:641:     obj r = malloc(sizeof(struct Reference)+t->dataSize+10);
	addq	$54, %rax	#, _3
	movq	%rax, %rdi	# _3,
	call	malloc@PLT	#
	movq	%rax, -24(%rbp)	# tmp121, r
# src/Runtime/runtime.c:643:     if (gc_ref_length+1 >= gc_ref_cap) {
	movl	gc_ref_length(%rip), %eax	# gc_ref_length, gc_ref_length.233_4
	leal	1(%rax), %edx	#, _5
# src/Runtime/runtime.c:643:     if (gc_ref_length+1 >= gc_ref_cap) {
	movl	gc_ref_cap(%rip), %eax	# gc_ref_cap, gc_ref_cap.234_6
# src/Runtime/runtime.c:643:     if (gc_ref_length+1 >= gc_ref_cap) {
	cmpl	%eax, %edx	# gc_ref_cap.234_6, _5
	jb	.L169	#,
# src/Runtime/runtime.c:645:         const uint32_t new_gc_ref_cap = gc_ref_cap > 100 ? gc_ref_cap*2 : 100;
	movl	gc_ref_cap(%rip), %eax	# gc_ref_cap, gc_ref_cap.236_7
# src/Runtime/runtime.c:645:         const uint32_t new_gc_ref_cap = gc_ref_cap > 100 ? gc_ref_cap*2 : 100;
	cmpl	$100, %eax	#, gc_ref_cap.236_7
	jbe	.L170	#,
# src/Runtime/runtime.c:645:         const uint32_t new_gc_ref_cap = gc_ref_cap > 100 ? gc_ref_cap*2 : 100;
	movl	gc_ref_cap(%rip), %eax	# gc_ref_cap, gc_ref_cap.237_8
# src/Runtime/runtime.c:645:         const uint32_t new_gc_ref_cap = gc_ref_cap > 100 ? gc_ref_cap*2 : 100;
	addl	%eax, %eax	# iftmp.235_54
	jmp	.L171	#
.L170:
# src/Runtime/runtime.c:645:         const uint32_t new_gc_ref_cap = gc_ref_cap > 100 ? gc_ref_cap*2 : 100;
	movl	$100, %eax	#, iftmp.235_54
.L171:
# src/Runtime/runtime.c:645:         const uint32_t new_gc_ref_cap = gc_ref_cap > 100 ? gc_ref_cap*2 : 100;
	movl	%eax, -28(%rbp)	# iftmp.235_54, new_gc_ref_cap
# src/Runtime/runtime.c:646:         void** new_gc_ref = malloc(sizeof(obj) * new_gc_ref_cap);
	movl	-28(%rbp), %eax	# new_gc_ref_cap, _9
	salq	$3, %rax	#, _10
	movq	%rax, %rdi	# _10,
	call	malloc@PLT	#
	movq	%rax, -40(%rbp)	# tmp122, new_gc_ref
# src/Runtime/runtime.c:647:         for(int i=0;i<gc_ref_length;++i) {
	movl	$0, -12(%rbp)	#, i
# src/Runtime/runtime.c:647:         for(int i=0;i<gc_ref_length;++i) {
	jmp	.L172	#
.L173:
# src/Runtime/runtime.c:648:             new_gc_ref[i] = gc_ref[i];
	movq	gc_ref(%rip), %rax	# gc_ref, gc_ref.238_11
	movl	-12(%rbp), %edx	# i, tmp123
	movslq	%edx, %rdx	# tmp123, _12
	salq	$3, %rdx	#, _13
	addq	%rdx, %rax	# _13, _14
# src/Runtime/runtime.c:648:             new_gc_ref[i] = gc_ref[i];
	movl	-12(%rbp), %edx	# i, tmp124
	movslq	%edx, %rdx	# tmp124, _15
	leaq	0(,%rdx,8), %rcx	#, _16
	movq	-40(%rbp), %rdx	# new_gc_ref, tmp125
	addq	%rcx, %rdx	# _16, _17
# src/Runtime/runtime.c:648:             new_gc_ref[i] = gc_ref[i];
	movq	(%rax), %rax	# *_14, _18
# src/Runtime/runtime.c:648:             new_gc_ref[i] = gc_ref[i];
	movq	%rax, (%rdx)	# _18, *_17
# src/Runtime/runtime.c:647:         for(int i=0;i<gc_ref_length;++i) {
	addl	$1, -12(%rbp)	#, i
.L172:
# src/Runtime/runtime.c:647:         for(int i=0;i<gc_ref_length;++i) {
	movl	-12(%rbp), %edx	# i, i.239_19
	movl	gc_ref_length(%rip), %eax	# gc_ref_length, gc_ref_length.240_20
	cmpl	%eax, %edx	# gc_ref_length.240_20, i.239_19
	jb	.L173	#,
# src/Runtime/runtime.c:650:         if (gc_ref != NULL) {
	movq	gc_ref(%rip), %rax	# gc_ref, gc_ref.241_21
# src/Runtime/runtime.c:650:         if (gc_ref != NULL) {
	testq	%rax, %rax	# gc_ref.241_21
	je	.L174	#,
# src/Runtime/runtime.c:651:             free(gc_ref);
	movq	gc_ref(%rip), %rax	# gc_ref, gc_ref.242_22
	movq	%rax, %rdi	# gc_ref.242_22,
	call	free@PLT	#
.L174:
# src/Runtime/runtime.c:653:         gc_ref = new_gc_ref;
	movq	-40(%rbp), %rax	# new_gc_ref, tmp126
	movq	%rax, gc_ref(%rip)	# tmp126, gc_ref
# src/Runtime/runtime.c:654:         gc_ref_cap = new_gc_ref_cap;
	movl	-28(%rbp), %eax	# new_gc_ref_cap, tmp127
	movl	%eax, gc_ref_cap(%rip)	# tmp127, gc_ref_cap
.L169:
# src/Runtime/runtime.c:658:     gc_ref[gc_ref_length++] = (void*)r;
	movq	gc_ref(%rip), %rcx	# gc_ref, gc_ref.250_29
# src/Runtime/runtime.c:658:     gc_ref[gc_ref_length++] = (void*)r;
	movl	gc_ref_length(%rip), %eax	# gc_ref_length, gc_ref_length.251_30
	leal	1(%rax), %edx	#, _32
	movl	%edx, gc_ref_length(%rip)	# _32, gc_ref_length
	movl	%eax, %eax	# gc_ref_length.251_30, _33
# src/Runtime/runtime.c:658:     gc_ref[gc_ref_length++] = (void*)r;
	salq	$3, %rax	#, _34
	leaq	(%rcx,%rax), %rdx	#, _35
# src/Runtime/runtime.c:658:     gc_ref[gc_ref_length++] = (void*)r;
	movq	-24(%rbp), %rax	# r, tmp128
	movq	%rax, (%rdx)	# tmp128, *_35
# src/Runtime/runtime.c:661:     r->type = t;
	movq	-24(%rbp), %rax	# r, tmp129
	movq	-56(%rbp), %rdx	# t, tmp130
	movq	%rdx, (%rax)	# tmp130, r_52->type
# src/Runtime/runtime.c:662:     r->counter = 0;
	movq	-24(%rbp), %rax	# r, tmp131
	movl	$0, 16(%rax)	#, r_52->counter
# src/Runtime/runtime.c:663:     r->data = NULL;
	movq	-24(%rbp), %rax	# r, tmp132
	movq	$0, 8(%rax)	#, r_52->data
# src/Runtime/runtime.c:664:     r->methods = r->type->methods;
	movq	-24(%rbp), %rax	# r, tmp133
	movq	(%rax), %rax	# r_52->type, _40
# src/Runtime/runtime.c:664:     r->methods = r->type->methods;
	movq	20(%rax), %rdx	# _40->methods, _41
# src/Runtime/runtime.c:664:     r->methods = r->type->methods;
	movq	-24(%rbp), %rax	# r, tmp134
	movq	%rdx, 20(%rax)	# _41, r_52->methods
# src/Runtime/runtime.c:665:     r->length=0;
	movq	-24(%rbp), %rax	# r, tmp135
	movl	$0, 32(%rax)	#, r_52->length
# src/Runtime/runtime.c:667:     if (t->dataSize > 0) {
	movq	-56(%rbp), %rax	# t, tmp136
	movl	16(%rax), %eax	# t_67(D)->dataSize, _46
# src/Runtime/runtime.c:667:     if (t->dataSize > 0) {
	testl	%eax, %eax	# _46
	jle	.L175	#,
# src/Runtime/runtime.c:669:         memcpy(&(r->others), t->initializer, t->dataSize);
	movq	-56(%rbp), %rax	# t, tmp137
	movl	16(%rax), %eax	# t_67(D)->dataSize, _47
# src/Runtime/runtime.c:669:         memcpy(&(r->others), t->initializer, t->dataSize);
	movslq	%eax, %rdx	# _47, _48
# src/Runtime/runtime.c:669:         memcpy(&(r->others), t->initializer, t->dataSize);
	movq	-56(%rbp), %rax	# t, tmp138
	movq	8(%rax), %rax	# t_67(D)->initializer, _49
# src/Runtime/runtime.c:669:         memcpy(&(r->others), t->initializer, t->dataSize);
	movq	-24(%rbp), %rcx	# r, tmp139
	addq	$36, %rcx	#, _50
# src/Runtime/runtime.c:669:         memcpy(&(r->others), t->initializer, t->dataSize);
	movq	%rax, %rsi	# _49,
	movq	%rcx, %rdi	# _50,
	call	memcpy@PLT	#
.L175:
# src/Runtime/runtime.c:680:     uint64_t test = *((uint64_t*)(r+36));DEBUG("Just check. R+36=%p (is null=%d)", FORMAT_PTR(test), IS_NULL(((void*)test)));
	movq	-24(%rbp), %rax	# r, tmp140
	movq	1584(%rax), %rax	# MEM[(uint64_t *)r_52 + 1584B], tmp141
	movq	%rax, -8(%rbp)	# tmp141, test
# src/Runtime/runtime.c:681:     return r;
	movq	-24(%rbp), %rax	# r, _28
# src/Runtime/runtime.c:682: }
	leave	
	.cfi_def_cfa 7, 8
	ret	
	.cfi_endproc
.LFE44:
	.size	__new, .-__new
	.local	gc_trigger_n
	.comm	gc_trigger_n,4,4
	.globl	run_gc
	.type	run_gc, @function
run_gc:
.LFB45:
	.cfi_startproc
	pushq	%rbp	#
	.cfi_def_cfa_offset 16
	.cfi_offset 6, -16
	movq	%rsp, %rbp	#,
	.cfi_def_cfa_register 6
	subq	$16, %rsp	#,
# src/Runtime/runtime.c:687:     gc_trigger_n++;
	movl	gc_trigger_n(%rip), %eax	# gc_trigger_n, gc_trigger_n.271_1
	addl	$1, %eax	#, _2
	movl	%eax, gc_trigger_n(%rip)	# _2, gc_trigger_n
# src/Runtime/runtime.c:688:     if (gc_trigger_n < 10) {
	movl	gc_trigger_n(%rip), %eax	# gc_trigger_n, gc_trigger_n.272_3
# src/Runtime/runtime.c:688:     if (gc_trigger_n < 10) {
	cmpl	$9, %eax	#, gc_trigger_n.272_3
	jle	.L184	#,
# src/Runtime/runtime.c:691:     gc_trigger_n = 0;
	movl	$0, gc_trigger_n(%rip)	#, gc_trigger_n
# src/Runtime/runtime.c:693:     for(int i=0;i<gc_ref_length;++i) {
	movl	$0, -4(%rbp)	#, i
# src/Runtime/runtime.c:693:     for(int i=0;i<gc_ref_length;++i) {
	jmp	.L180	#
.L183:
# src/Runtime/runtime.c:694:         if(gc_ref[i] != NULL && !IS_NULL(gc_ref[i]) && gc_ref[i] != emptyString) {
	movq	gc_ref(%rip), %rax	# gc_ref, gc_ref.277_8
	movl	-4(%rbp), %edx	# i, tmp111
	movslq	%edx, %rdx	# tmp111, _9
	salq	$3, %rdx	#, _10
	addq	%rdx, %rax	# _10, _11
	movq	(%rax), %rax	# *_11, _12
# src/Runtime/runtime.c:694:         if(gc_ref[i] != NULL && !IS_NULL(gc_ref[i]) && gc_ref[i] != emptyString) {
	testq	%rax, %rax	# _12
	je	.L181	#,
# src/Runtime/runtime.c:694:         if(gc_ref[i] != NULL && !IS_NULL(gc_ref[i]) && gc_ref[i] != emptyString) {
	movq	gc_ref(%rip), %rax	# gc_ref, gc_ref.278_13
	movl	-4(%rbp), %edx	# i, tmp112
	movslq	%edx, %rdx	# tmp112, _14
	salq	$3, %rdx	#, _15
	addq	%rdx, %rax	# _15, _16
	movq	(%rax), %rdx	# *_16, _17
# src/Runtime/runtime.c:694:         if(gc_ref[i] != NULL && !IS_NULL(gc_ref[i]) && gc_ref[i] != emptyString) {
	leaq	_LAT_NULL(%rip), %rax	#, tmp113
	cmpq	%rax, %rdx	# tmp113, _17
	je	.L181	#,
# src/Runtime/runtime.c:694:         if(gc_ref[i] != NULL && !IS_NULL(gc_ref[i]) && gc_ref[i] != emptyString) {
	movq	gc_ref(%rip), %rax	# gc_ref, gc_ref.279_18
	movl	-4(%rbp), %edx	# i, tmp114
	movslq	%edx, %rdx	# tmp114, _19
	salq	$3, %rdx	#, _20
	addq	%rdx, %rax	# _20, _21
	movq	(%rax), %rdx	# *_21, _22
# src/Runtime/runtime.c:694:         if(gc_ref[i] != NULL && !IS_NULL(gc_ref[i]) && gc_ref[i] != emptyString) {
	leaq	emptyString(%rip), %rax	#, tmp115
	cmpq	%rax, %rdx	# tmp115, _22
	je	.L181	#,
# src/Runtime/runtime.c:695:             obj p = (obj)gc_ref[i];
	movq	gc_ref(%rip), %rax	# gc_ref, gc_ref.280_23
	movl	-4(%rbp), %edx	# i, tmp116
	movslq	%edx, %rdx	# tmp116, _24
	salq	$3, %rdx	#, _25
	addq	%rdx, %rax	# _25, _26
# src/Runtime/runtime.c:695:             obj p = (obj)gc_ref[i];
	movq	(%rax), %rax	# *_26, tmp117
	movq	%rax, -16(%rbp)	# tmp117, p
# src/Runtime/runtime.c:696:             if (p->counter <= 0) {
	movq	-16(%rbp), %rax	# p, tmp118
	movl	16(%rax), %eax	# p_39->counter, _27
# src/Runtime/runtime.c:696:             if (p->counter <= 0) {
	testl	%eax, %eax	# _27
	jg	.L182	#,
# src/Runtime/runtime.c:697:                 __free(p, 0);
	movq	-16(%rbp), %rax	# p, tmp119
	movl	$0, %esi	#,
	movq	%rax, %rdi	# tmp119,
	call	__free	#
.L182:
# src/Runtime/runtime.c:699:             gc_ref[i] = NULL;
	movq	gc_ref(%rip), %rax	# gc_ref, gc_ref.281_28
	movl	-4(%rbp), %edx	# i, tmp120
	movslq	%edx, %rdx	# tmp120, _29
	salq	$3, %rdx	#, _30
	addq	%rdx, %rax	# _30, _31
# src/Runtime/runtime.c:699:             gc_ref[i] = NULL;
	movq	$0, (%rax)	#, *_31
.L181:
# src/Runtime/runtime.c:693:     for(int i=0;i<gc_ref_length;++i) {
	addl	$1, -4(%rbp)	#, i
.L180:
# src/Runtime/runtime.c:693:     for(int i=0;i<gc_ref_length;++i) {
	movl	-4(%rbp), %edx	# i, i.282_32
	movl	gc_ref_length(%rip), %eax	# gc_ref_length, gc_ref_length.283_33
	cmpl	%eax, %edx	# gc_ref_length.283_33, i.282_32
	jb	.L183	#,
	jmp	.L177	#
.L184:
# src/Runtime/runtime.c:689:         return;
	nop	
.L177:
# src/Runtime/runtime.c:702: }
	leave	
	.cfi_def_cfa 7, 8
	ret	
	.cfi_endproc
.LFE45:
	.size	run_gc, .-run_gc
	.globl	__free
	.type	__free, @function
__free:
.LFB46:
	.cfi_startproc
	pushq	%rbp	#
	.cfi_def_cfa_offset 16
	.cfi_offset 6, -16
	movq	%rsp, %rbp	#,
	.cfi_def_cfa_register 6
	subq	$48, %rsp	#,
	movq	%rdi, -40(%rbp)	# r, r
	movl	%esi, -44(%rbp)	# gc_pos, gc_pos
# src/Runtime/runtime.c:705:     if (gc_pos == 0) {
	cmpl	$0, -44(%rbp)	#, gc_pos
	je	.L195	#,
# src/Runtime/runtime.c:708:     if (IS_NULL(r) || r == NULL) {
	leaq	_LAT_NULL(%rip), %rax	#, tmp95
	cmpq	%rax, -40(%rbp)	# tmp95, r
	je	.L196	#,
# src/Runtime/runtime.c:708:     if (IS_NULL(r) || r == NULL) {
	cmpq	$0, -40(%rbp)	#, r
	je	.L196	#,
# src/Runtime/runtime.c:713:     if (r->type == &_class_Array) {
	movq	-40(%rbp), %rax	# r, tmp96
	movq	(%rax), %rdx	# r_20(D)->type, _1
# src/Runtime/runtime.c:713:     if (r->type == &_class_Array) {
	leaq	_class_Array(%rip), %rax	#, tmp97
	cmpq	%rax, %rdx	# tmp97, _1
	jne	.L190	#,
# src/Runtime/runtime.c:716:         void **els = r->data;
	movq	-40(%rbp), %rax	# r, tmp98
	movq	8(%rax), %rax	# r_20(D)->data, tmp99
	movq	%rax, -24(%rbp)	# tmp99, els
# src/Runtime/runtime.c:717:         if (r->elementSize == sizeof(void *)) {
	movq	-40(%rbp), %rax	# r, tmp100
	movl	28(%rax), %eax	# r_20(D)->elementSize, _3
# src/Runtime/runtime.c:717:         if (r->elementSize == sizeof(void *)) {
	cmpl	$8, %eax	#, _3
	jne	.L191	#,
# src/Runtime/runtime.c:718:             for (int i = 0; i < r->length; i++)
	movl	$0, -4(%rbp)	#, i
# src/Runtime/runtime.c:718:             for (int i = 0; i < r->length; i++)
	jmp	.L192	#
.L193:
# src/Runtime/runtime.c:719:                 __decRef(els[i]);
	movl	-4(%rbp), %eax	# i, tmp101
	cltq
	leaq	0(,%rax,8), %rdx	#, _5
	movq	-24(%rbp), %rax	# els, tmp102
	addq	%rdx, %rax	# _5, _6
# src/Runtime/runtime.c:719:                 __decRef(els[i]);
	movq	(%rax), %rax	# *_6, _7
	movq	%rax, %rdi	# _7,
	call	__decRef	#
# src/Runtime/runtime.c:718:             for (int i = 0; i < r->length; i++)
	addl	$1, -4(%rbp)	#, i
.L192:
# src/Runtime/runtime.c:718:             for (int i = 0; i < r->length; i++)
	movq	-40(%rbp), %rax	# r, tmp103
	movl	32(%rax), %eax	# r_20(D)->length, _8
# src/Runtime/runtime.c:718:             for (int i = 0; i < r->length; i++)
	cmpl	%eax, -4(%rbp)	# _8, i
	jl	.L193	#,
.L191:
# src/Runtime/runtime.c:721:         if (els != NULL) {
	cmpq	$0, -24(%rbp)	#, els
	je	.L194	#,
# src/Runtime/runtime.c:722:             free(els);
	movq	-24(%rbp), %rax	# els, tmp104
	movq	%rax, %rdi	# tmp104,
	call	free@PLT	#
	jmp	.L194	#
.L190:
# src/Runtime/runtime.c:724:     } else if (r->type == &_class_String) {
	movq	-40(%rbp), %rax	# r, tmp105
	movq	(%rax), %rdx	# r_20(D)->type, _9
# src/Runtime/runtime.c:724:     } else if (r->type == &_class_String) {
	leaq	_class_String(%rip), %rax	#, tmp106
	cmpq	%rax, %rdx	# tmp106, _9
	jne	.L194	#,
# src/Runtime/runtime.c:726:         void *els = (r->data);
	movq	-40(%rbp), %rax	# r, tmp107
	movq	8(%rax), %rax	# r_20(D)->data, tmp108
	movq	%rax, -16(%rbp)	# tmp108, els
# src/Runtime/runtime.c:727:         if (els != NULL && els != emptyString) {
	cmpq	$0, -16(%rbp)	#, els
	je	.L194	#,
# src/Runtime/runtime.c:727:         if (els != NULL && els != emptyString) {
	leaq	emptyString(%rip), %rax	#, tmp109
	cmpq	%rax, -16(%rbp)	# tmp109, els
	je	.L194	#,
# src/Runtime/runtime.c:729:             free(els);
	movq	-16(%rbp), %rax	# els, tmp110
	movq	%rax, %rdi	# tmp110,
	call	free@PLT	#
.L194:
# src/Runtime/runtime.c:736:     free(r);
	movq	-40(%rbp), %rax	# r, tmp111
	movq	%rax, %rdi	# tmp111,
	call	free@PLT	#
# src/Runtime/runtime.c:737:     gc_ref[gc_pos-1] = NULL;
	movq	gc_ref(%rip), %rax	# gc_ref, gc_ref.308_26
# src/Runtime/runtime.c:737:     gc_ref[gc_pos-1] = NULL;
	movl	-44(%rbp), %edx	# gc_pos, tmp112
	subl	$1, %edx	#, _27
	movl	%edx, %edx	# _27, _28
# src/Runtime/runtime.c:737:     gc_ref[gc_pos-1] = NULL;
	salq	$3, %rdx	#, _29
	addq	%rdx, %rax	# _29, _30
# src/Runtime/runtime.c:737:     gc_ref[gc_pos-1] = NULL;
	movq	$0, (%rax)	#, *_30
	jmp	.L185	#
.L195:
# src/Runtime/runtime.c:706:         return;
	nop	
	jmp	.L185	#
.L196:
# src/Runtime/runtime.c:710:         return;
	nop	
.L185:
# src/Runtime/runtime.c:738: }
	leave	
	.cfi_def_cfa 7, 8
	ret	
	.cfi_endproc
.LFE46:
	.size	__free, .-__free
	.globl	__decRef
	.type	__decRef, @function
__decRef:
.LFB47:
	.cfi_startproc
	pushq	%rbp	#
	.cfi_def_cfa_offset 16
	.cfi_offset 6, -16
	movq	%rsp, %rbp	#,
	.cfi_def_cfa_register 6
	subq	$96, %rsp	#,
	movq	%rdi, -88(%rbp)	# r, r
# src/Runtime/runtime.c:742:     if (gc_level == 0) {
	movl	gc_level(%rip), %eax	# gc_level, gc_level.309_1
# src/Runtime/runtime.c:742:     if (gc_level == 0) {
	testl	%eax, %eax	# gc_level.309_1
	jne	.L198	#,
# src/Runtime/runtime.c:743:         gc_visited_length = 0;
	movl	$0, gc_visited_length(%rip)	#, gc_visited_length
.L198:
# src/Runtime/runtime.c:745:     ++gc_level;
	movl	gc_level(%rip), %eax	# gc_level, gc_level.310_2
	addl	$1, %eax	#, _3
	movl	%eax, gc_level(%rip)	# _3, gc_level
# src/Runtime/runtime.c:746:     obj rrepr = typeInfoStringRepr(r);
	movq	-88(%rbp), %rax	# r, tmp131
	movq	%rax, %rdi	# tmp131,
	call	typeInfoStringRepr	#
	movq	%rax, -24(%rbp)	# tmp132, rrepr
# src/Runtime/runtime.c:749:     if (!IS_NULL(r) && r != NULL) {
	leaq	_LAT_NULL(%rip), %rax	#, tmp133
	cmpq	%rax, -88(%rbp)	# tmp133, r
	je	.L199	#,
# src/Runtime/runtime.c:749:     if (!IS_NULL(r) && r != NULL) {
	cmpq	$0, -88(%rbp)	#, r
	je	.L199	#,
# src/Runtime/runtime.c:750:         for (int i=0;i<gc_visited_length;++i) {
	movl	$0, -4(%rbp)	#, i
# src/Runtime/runtime.c:750:         for (int i=0;i<gc_visited_length;++i) {
	jmp	.L200	#
.L203:
# src/Runtime/runtime.c:751:             if (((uint64_t)r) == gc_visited[i]) {
	movl	-4(%rbp), %eax	# i, tmp135
	cltq
	leaq	0(,%rax,8), %rdx	#, tmp136
	leaq	gc_visited(%rip), %rax	#, tmp137
	movq	(%rdx,%rax), %rdx	# gc_visited[i_53], _9
# src/Runtime/runtime.c:751:             if (((uint64_t)r) == gc_visited[i]) {
	movq	-88(%rbp), %rax	# r, r.315_10
# src/Runtime/runtime.c:751:             if (((uint64_t)r) == gc_visited[i]) {
	cmpq	%rax, %rdx	# r.315_10, _9
	je	.L215	#,
# src/Runtime/runtime.c:750:         for (int i=0;i<gc_visited_length;++i) {
	addl	$1, -4(%rbp)	#, i
.L200:
# src/Runtime/runtime.c:750:         for (int i=0;i<gc_visited_length;++i) {
	movl	-4(%rbp), %edx	# i, i.316_11
	movl	gc_visited_length(%rip), %eax	# gc_visited_length, gc_visited_length.317_12
	cmpl	%eax, %edx	# gc_visited_length.317_12, i.316_11
	jb	.L203	#,
# src/Runtime/runtime.c:756:         gc_visited[gc_visited_length++] = (uint64_t)r;
	movl	gc_visited_length(%rip), %eax	# gc_visited_length, gc_visited_length.318_13
	leal	1(%rax), %edx	#, _15
	movl	%edx, gc_visited_length(%rip)	# _15, gc_visited_length
# src/Runtime/runtime.c:756:         gc_visited[gc_visited_length++] = (uint64_t)r;
	movq	-88(%rbp), %rdx	# r, r.320_16
# src/Runtime/runtime.c:756:         gc_visited[gc_visited_length++] = (uint64_t)r;
	movl	%eax, %eax	# gc_visited_length.318_13, tmp138
	leaq	0(,%rax,8), %rcx	#, tmp139
	leaq	gc_visited(%rip), %rax	#, tmp140
	movq	%rdx, (%rcx,%rax)	# r.320_16, gc_visited[gc_visited_length.319_14]
# src/Runtime/runtime.c:757:         r->counter--;
	movq	-88(%rbp), %rax	# r, tmp141
	movl	16(%rax), %eax	# r_63(D)->counter, _17
# src/Runtime/runtime.c:757:         r->counter--;
	leal	-1(%rax), %edx	#, _18
	movq	-88(%rbp), %rax	# r, tmp142
	movl	%edx, 16(%rax)	# _18, r_63(D)->counter
# src/Runtime/runtime.c:758:         if (r->counter <= 0) {
	movq	-88(%rbp), %rax	# r, tmp143
	movl	16(%rax), %eax	# r_63(D)->counter, _19
# src/Runtime/runtime.c:758:         if (r->counter <= 0) {
	testl	%eax, %eax	# _19
	jg	.L199	#,
# src/Runtime/runtime.c:760:                 const int fieldsInfoLength = r->type->fieldsInfoLength;
	movq	-88(%rbp), %rax	# r, tmp144
	movq	(%rax), %rax	# r_63(D)->type, _20
# src/Runtime/runtime.c:760:                 const int fieldsInfoLength = r->type->fieldsInfoLength;
	movl	64(%rax), %eax	# _20->fieldsInfoLength, tmp145
	movl	%eax, -28(%rbp)	# tmp145, fieldsInfoLength
# src/Runtime/runtime.c:761:                 for(int i=0;i<fieldsInfoLength;++i) {
	movl	$0, -8(%rbp)	#, i
# src/Runtime/runtime.c:761:                 for(int i=0;i<fieldsInfoLength;++i) {
	jmp	.L204	#
.L212:
# src/Runtime/runtime.c:762:                     char* fieldInfo = (char*) (((uint64_t)r->type->fieldsInfo) + ((uint64_t)r->type->fieldsInfoOffsets[i]));
	movq	-88(%rbp), %rax	# r, tmp146
	movq	(%rax), %rax	# r_63(D)->type, _21
# src/Runtime/runtime.c:762:                     char* fieldInfo = (char*) (((uint64_t)r->type->fieldsInfo) + ((uint64_t)r->type->fieldsInfoOffsets[i]));
	movq	40(%rax), %rax	# _21->fieldsInfo, _22
# src/Runtime/runtime.c:762:                     char* fieldInfo = (char*) (((uint64_t)r->type->fieldsInfo) + ((uint64_t)r->type->fieldsInfoOffsets[i]));
	movq	%rax, %rcx	# _22, _23
# src/Runtime/runtime.c:762:                     char* fieldInfo = (char*) (((uint64_t)r->type->fieldsInfo) + ((uint64_t)r->type->fieldsInfoOffsets[i]));
	movq	-88(%rbp), %rax	# r, tmp147
	movq	(%rax), %rax	# r_63(D)->type, _24
# src/Runtime/runtime.c:762:                     char* fieldInfo = (char*) (((uint64_t)r->type->fieldsInfo) + ((uint64_t)r->type->fieldsInfoOffsets[i]));
	movq	48(%rax), %rax	# _24->fieldsInfoOffsets, _25
# src/Runtime/runtime.c:762:                     char* fieldInfo = (char*) (((uint64_t)r->type->fieldsInfo) + ((uint64_t)r->type->fieldsInfoOffsets[i]));
	movl	-8(%rbp), %edx	# i, tmp148
	movslq	%edx, %rdx	# tmp148, _26
	salq	$2, %rdx	#, _27
	addq	%rdx, %rax	# _27, _28
	movl	(%rax), %eax	# *_28, _29
# src/Runtime/runtime.c:762:                     char* fieldInfo = (char*) (((uint64_t)r->type->fieldsInfo) + ((uint64_t)r->type->fieldsInfoOffsets[i]));
	cltq
# src/Runtime/runtime.c:762:                     char* fieldInfo = (char*) (((uint64_t)r->type->fieldsInfo) + ((uint64_t)r->type->fieldsInfoOffsets[i]));
	addq	%rcx, %rax	# _23, _31
# src/Runtime/runtime.c:762:                     char* fieldInfo = (char*) (((uint64_t)r->type->fieldsInfo) + ((uint64_t)r->type->fieldsInfoOffsets[i]));
	movq	%rax, -48(%rbp)	# _31, fieldInfo
# src/Runtime/runtime.c:763:                     char type = fieldInfo[0];
	movq	-48(%rbp), %rax	# fieldInfo, tmp149
	movzbl	(%rax), %eax	# *fieldInfo_76, tmp150
	movb	%al, -49(%rbp)	# tmp150, type
# src/Runtime/runtime.c:764:                     char* name = fieldInfo+1;
	movq	-48(%rbp), %rax	# fieldInfo, tmp154
	addq	$1, %rax	#, tmp153
	movq	%rax, -64(%rbp)	# tmp153, name
# src/Runtime/runtime.c:765:                     obj* fieldValue = (obj*)(((uint64_t)(&r->others)) + ((uint64_t)(r->type->fieldsDataOffsets[i])));
	movq	-88(%rbp), %rax	# r, tmp155
	addq	$36, %rax	#, _32
# src/Runtime/runtime.c:765:                     obj* fieldValue = (obj*)(((uint64_t)(&r->others)) + ((uint64_t)(r->type->fieldsDataOffsets[i])));
	movq	%rax, %rcx	# _32, _33
# src/Runtime/runtime.c:765:                     obj* fieldValue = (obj*)(((uint64_t)(&r->others)) + ((uint64_t)(r->type->fieldsDataOffsets[i])));
	movq	-88(%rbp), %rax	# r, tmp156
	movq	(%rax), %rax	# r_63(D)->type, _34
# src/Runtime/runtime.c:765:                     obj* fieldValue = (obj*)(((uint64_t)(&r->others)) + ((uint64_t)(r->type->fieldsDataOffsets[i])));
	movq	56(%rax), %rax	# _34->fieldsDataOffsets, _35
# src/Runtime/runtime.c:765:                     obj* fieldValue = (obj*)(((uint64_t)(&r->others)) + ((uint64_t)(r->type->fieldsDataOffsets[i])));
	movl	-8(%rbp), %edx	# i, tmp157
	movslq	%edx, %rdx	# tmp157, _36
	salq	$2, %rdx	#, _37
	addq	%rdx, %rax	# _37, _38
	movl	(%rax), %eax	# *_38, _39
# src/Runtime/runtime.c:765:                     obj* fieldValue = (obj*)(((uint64_t)(&r->others)) + ((uint64_t)(r->type->fieldsDataOffsets[i])));
	cltq
# src/Runtime/runtime.c:765:                     obj* fieldValue = (obj*)(((uint64_t)(&r->others)) + ((uint64_t)(r->type->fieldsDataOffsets[i])));
	addq	%rcx, %rax	# _33, _41
# src/Runtime/runtime.c:765:                     obj* fieldValue = (obj*)(((uint64_t)(&r->others)) + ((uint64_t)(r->type->fieldsDataOffsets[i])));
	movq	%rax, -72(%rbp)	# _41, fieldValue
# src/Runtime/runtime.c:766:                     switch(type) {
	movsbl	-49(%rbp), %eax	# type, _42
	subl	$65, %eax	#, tmp158
	cmpl	$18, %eax	#, tmp158
	ja	.L205	#,
	movl	%eax, %eax	# tmp158, tmp159
	leaq	0(,%rax,4), %rdx	#, tmp160
	leaq	.L207(%rip), %rax	#, tmp161
	movl	(%rdx,%rax), %eax	#, tmp162
	cltq
	leaq	.L207(%rip), %rdx	#, tmp165
	addq	%rdx, %rax	# tmp165, tmp164
	jmp	*%rax	# tmp164
	.section	.rodata
	.align 4
	.align 4
.L207:
	.long	.L211-.L207
	.long	.L205-.L207
	.long	.L209-.L207
	.long	.L205-.L207
	.long	.L205-.L207
	.long	.L205-.L207
	.long	.L205-.L207
	.long	.L205-.L207
	.long	.L205-.L207
	.long	.L205-.L207
	.long	.L205-.L207
	.long	.L205-.L207
	.long	.L205-.L207
	.long	.L205-.L207
	.long	.L205-.L207
	.long	.L205-.L207
	.long	.L205-.L207
	.long	.L205-.L207
	.long	.L206-.L207
	.text
.L211:
# src/Runtime/runtime.c:768:                             __decRef(*fieldValue);
	movq	-72(%rbp), %rax	# fieldValue, tmp166
	movq	(%rax), %rax	# *fieldValue_79, _43
	movq	%rax, %rdi	# _43,
	call	__decRef	#
# src/Runtime/runtime.c:769:                             break;
	jmp	.L205	#
.L206:
# src/Runtime/runtime.c:772:                             __decRef(*fieldValue);
	movq	-72(%rbp), %rax	# fieldValue, tmp167
	movq	(%rax), %rax	# *fieldValue_79, _44
	movq	%rax, %rdi	# _44,
	call	__decRef	#
# src/Runtime/runtime.c:773:                             break;
	jmp	.L205	#
.L209:
# src/Runtime/runtime.c:776:                             __decRef(*fieldValue);
	movq	-72(%rbp), %rax	# fieldValue, tmp168
	movq	(%rax), %rax	# *fieldValue_79, _45
	movq	%rax, %rdi	# _45,
	call	__decRef	#
# src/Runtime/runtime.c:777:                             break;
	nop	
.L205:
# src/Runtime/runtime.c:761:                 for(int i=0;i<fieldsInfoLength;++i) {
	addl	$1, -8(%rbp)	#, i
.L204:
# src/Runtime/runtime.c:761:                 for(int i=0;i<fieldsInfoLength;++i) {
	movl	-8(%rbp), %eax	# i, tmp169
	cmpl	-28(%rbp), %eax	# fieldsInfoLength, tmp169
	jl	.L212	#,
# src/Runtime/runtime.c:790:             if (r->type == &_class_Array) {
	movq	-88(%rbp), %rax	# r, tmp170
	movq	(%rax), %rdx	# r_63(D)->type, _46
# src/Runtime/runtime.c:790:             if (r->type == &_class_Array) {
	leaq	_class_Array(%rip), %rax	#, tmp171
	cmpq	%rax, %rdx	# tmp171, _46
	jne	.L199	#,
# src/Runtime/runtime.c:791:                 if (r->elementSize == sizeof(void *)) {
	movq	-88(%rbp), %rax	# r, tmp172
	movl	28(%rax), %eax	# r_63(D)->elementSize, _47
# src/Runtime/runtime.c:791:                 if (r->elementSize == sizeof(void *)) {
	cmpl	$8, %eax	#, _47
	jne	.L199	#,
# src/Runtime/runtime.c:792:                     void **els = r->data;
	movq	-88(%rbp), %rax	# r, tmp173
	movq	8(%rax), %rax	# r_63(D)->data, tmp174
	movq	%rax, -40(%rbp)	# tmp174, els
# src/Runtime/runtime.c:793:                     for (int i = 0; i < r->length; i++) {
	movl	$0, -12(%rbp)	#, i
# src/Runtime/runtime.c:793:                     for (int i = 0; i < r->length; i++) {
	jmp	.L213	#
.L214:
# src/Runtime/runtime.c:794:                         __decRef(els[i]);
	movl	-12(%rbp), %eax	# i, tmp175
	cltq
	leaq	0(,%rax,8), %rdx	#, _49
	movq	-40(%rbp), %rax	# els, tmp176
	addq	%rdx, %rax	# _49, _50
# src/Runtime/runtime.c:794:                         __decRef(els[i]);
	movq	(%rax), %rax	# *_50, _51
	movq	%rax, %rdi	# _51,
	call	__decRef	#
# src/Runtime/runtime.c:793:                     for (int i = 0; i < r->length; i++) {
	addl	$1, -12(%rbp)	#, i
.L213:
# src/Runtime/runtime.c:793:                     for (int i = 0; i < r->length; i++) {
	movq	-88(%rbp), %rax	# r, tmp177
	movl	32(%rax), %eax	# r_63(D)->length, _52
# src/Runtime/runtime.c:793:                     for (int i = 0; i < r->length; i++) {
	cmpl	%eax, -12(%rbp)	# _52, i
	jl	.L214	#,
.L199:
# src/Runtime/runtime.c:809:     --gc_level;
	movl	gc_level(%rip), %eax	# gc_level, gc_level.326_59
	subl	$1, %eax	#, _60
	movl	%eax, gc_level(%rip)	# _60, gc_level
# src/Runtime/runtime.c:810:     if (gc_level == 0) {
	movl	gc_level(%rip), %eax	# gc_level, gc_level.327_61
# src/Runtime/runtime.c:810:     if (gc_level == 0) {
	testl	%eax, %eax	# gc_level.327_61
	jne	.L197	#,
# src/Runtime/runtime.c:811:         gc_visited_length = 0;
	movl	$0, gc_visited_length(%rip)	#, gc_visited_length
	jmp	.L197	#
.L215:
# src/Runtime/runtime.c:753:                 return;
	nop	
.L197:
# src/Runtime/runtime.c:813: }
	leave	
	.cfi_def_cfa 7, 8
	ret	
	.cfi_endproc
.LFE47:
	.size	__decRef, .-__decRef
	.ident	"GCC: (Debian 12.2.0-14) 12.2.0"
	.section	.note.GNU-stack,"",@progbits
