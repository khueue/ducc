# Testruns

### ducc -e suite/quiet/lexer/l05.c gives:

		.text
		.globl	Lf
	Lf:
		subu	$sp, $sp, 12
		sw	$fp, 8($sp)
		sw	$ra, 4($sp)
		addu	$fp, $sp, 12
		lw	$t0, 0($fp)
		lw	$t1, 4($fp)
		add	$t2, $t0, $t1
		sw	$t2, 0($sp)
		lw	$t0, 0($sp)
		move	$v0, $t0
		j	L100_f_end
	L100_f_end:
		lw	$ra, 4($sp)
		lw	$fp, 8($sp)
		addu	$sp, $sp, 12
		jr	$ra

		.text
		.globl	main
	main:
		subu	$sp, $sp, 144
		sw	$fp, 140($sp)
		sw	$ra, 136($sp)
		addu	$fp, $sp, 144
		li	$t0, 1
		sw	$t0, 0($sp)
		li	$t0, 0
		sw	$t0, 4($sp)
		li	$t0, 3
		sw	$t0, 8($sp)
		lw	$t0, 4($sp)
		lw	$t1, 8($sp)
		seq	$t2, $t0, $t1
		sw	$t2, 12($sp)
		lw	$t0, 0($sp)
		lw	$t1, 12($sp)
		sne	$t2, $t0, $t1
		sw	$t2, 16($sp)
		li	$t0, 4
		sw	$t0, 20($sp)
		lw	$t0, 20($sp)
		beqz	$t0, L102_and_false
		li	$t0, 6
		sw	$t0, 24($sp)
		lw	$t0, 24($sp)
		beqz	$t0, L102_and_false
		li	$t0, 1
		sw	$t0, 28($sp)
		j	L103_and_end
	L102_and_false:
		li	$t0, 0
		sw	$t0, 28($sp)
	L103_and_end:
		li	$t0, 7
		sw	$t0, 32($sp)
		li	$t0, 8
		sw	$t0, 36($sp)
		lw	$t0, 32($sp)
		lw	$t1, 36($sp)
		mul	$t2, $t0, $t1
		sw	$t2, 40($sp)
		li	$t0, 10
		sw	$t0, 44($sp)
		lw	$t0, 40($sp)
		lw	$t1, 44($sp)
		add	$t2, $t0, $t1
		sw	$t2, 48($sp)
		li	$t0, 11
		sw	$t0, 52($sp)
		li	$t0, 12
		sw	$t0, 56($sp)
		lw	$t0, 52($sp)
		lw	$t1, 56($sp)
		sub	$t2, $t0, $t1
		sw	$t2, 60($sp)
		li	$t0, 12
		sw	$t0, 64($sp)
		li	$t0, 16
		sw	$t0, 68($sp)
		lw	$t0, 64($sp)
		lw	$t1, 68($sp)
		div	$t2, $t0, $t1
		sw	$t2, 72($sp)
		subu	$sp, $sp, 8
		lw	$t0, 68($sp)
		sw	$t0, 0($sp)
		lw	$t0, 80($sp)
		sw	$t0, 4($sp)
		jal	Lf
		sw	$v0, 84($sp)
		addu	$sp, $sp, 8
		li	$t0, 17
		sw	$t0, 80($sp)
		li	$t0, 18
		sw	$t0, 84($sp)
		lw	$t0, 80($sp)
		lw	$t1, 84($sp)
		sle	$t2, $t0, $t1
		sw	$t2, 88($sp)
		li	$t0, 20
		sw	$t0, 92($sp)
		lw	$t0, 88($sp)
		lw	$t1, 92($sp)
		slt	$t2, $t0, $t1
		sw	$t2, 96($sp)
		li	$t0, 21
		sw	$t0, 100($sp)
		li	$t0, 22
		sw	$t0, 104($sp)
		lw	$t0, 100($sp)
		lw	$t1, 104($sp)
		seq	$t2, $t0, $t1
		sw	$t2, 108($sp)
		lw	$t0, 108($sp)
		sw	$t0, 112($sp)
		li	$t0, 25
		sw	$t0, 116($sp)
		li	$t0, 27
		sw	$t0, 120($sp)
		lw	$t0, 116($sp)
		lw	$t1, 120($sp)
		sge	$t2, $t0, $t1
		sw	$t2, 124($sp)
		li	$t0, 28
		sw	$t0, 128($sp)
		lw	$t0, 124($sp)
		lw	$t1, 128($sp)
		sgt	$t2, $t0, $t1
		sw	$t2, 132($sp)
	L101_main_end:
		lw	$ra, 136($sp)
		lw	$fp, 140($sp)
		addu	$sp, $sp, 144
		jr	$ra

		.text
		.globl	Sputint
	Sputint:
		subu	$sp, $sp, 8
		sw	$fp, 4($sp)
		sw	$ra, 0($sp)
		addu	$fp, $sp, 8
		lw	$a0, 0($fp)
		li	$v0, 1
		syscall
		lw	$ra, 0($sp)
		lw	$fp, 4($sp)
		addu	$sp, $sp, 8
		jr	$ra

		.text
		.globl	Sgetint
	Sgetint:
		subu	$sp, $sp, 8
		sw	$fp, 4($sp)
		sw	$ra, 0($sp)
		addu	$fp, $sp, 8
		li	$v0, 5
		syscall
		lw	$ra, 0($sp)
		lw	$fp, 4($sp)
		addu	$sp, $sp, 8
		jr	$ra

		.text
		.globl	Sputstring
	Sputstring:
		subu	$sp, $sp, 8
		sw	$fp, 4($sp)
		sw	$ra, 0($sp)
		addu	$fp, $sp, 8
		lw	$a0, 0($fp)
		li	$v0, 4
		syscall
		lw	$ra, 0($sp)
		lw	$fp, 4($sp)
		addu	$sp, $sp, 8
		jr	$ra

		.text
		.globl	Sgetstring
	Sgetstring:
		subu	$sp, $sp, 8
		sw	$fp, 4($sp)
		sw	$ra, 0($sp)
		addu	$fp, $sp, 8
		lw	$a0, 0($fp)
		li	$a1, 1024
		li	$v0, 8
		syscall
		lw	$ra, 0($sp)
		lw	$fp, 4($sp)
		addu	$sp, $sp, 8
		jr	$ra

### ducc -e suite/quiet/parser/p03.c gives:

		.text
		.globl	main
	main:
		subu	$sp, $sp, 32
		sw	$fp, 28($sp)
		sw	$ra, 24($sp)
		addu	$fp, $sp, 32
		li	$t0, 1
		sw	$t0, 0($sp)
		li	$t0, 2
		sw	$t0, 4($sp)
		lw	$t0, 0($sp)
		lw	$t1, 4($sp)
		slt	$t2, $t0, $t1
		sw	$t2, 8($sp)
		lw	$t0, 8($sp)
		beqz	$t0, L101_if_else
		li	$t0, 1
		sw	$t0, 12($sp)
		lw	$t0, 12($sp)
		sw	$t0, 16($sp)
		j	L102_if_end
	L101_if_else:
		li	$t0, 2
		sw	$t0, 20($sp)
		lw	$t0, 20($sp)
		sw	$t0, 16($sp)
	L102_if_end:
	L100_main_end:
		lw	$ra, 24($sp)
		lw	$fp, 28($sp)
		addu	$sp, $sp, 32
		jr	$ra

		.text
		.globl	Sputint
	Sputint:
		subu	$sp, $sp, 8
		sw	$fp, 4($sp)
		sw	$ra, 0($sp)
		addu	$fp, $sp, 8
		lw	$a0, 0($fp)
		li	$v0, 1
		syscall
		lw	$ra, 0($sp)
		lw	$fp, 4($sp)
		addu	$sp, $sp, 8
		jr	$ra

		.text
		.globl	Sgetint
	Sgetint:
		subu	$sp, $sp, 8
		sw	$fp, 4($sp)
		sw	$ra, 0($sp)
		addu	$fp, $sp, 8
		li	$v0, 5
		syscall
		lw	$ra, 0($sp)
		lw	$fp, 4($sp)
		addu	$sp, $sp, 8
		jr	$ra

		.text
		.globl	Sputstring
	Sputstring:
		subu	$sp, $sp, 8
		sw	$fp, 4($sp)
		sw	$ra, 0($sp)
		addu	$fp, $sp, 8
		lw	$a0, 0($fp)
		li	$v0, 4
		syscall
		lw	$ra, 0($sp)
		lw	$fp, 4($sp)
		addu	$sp, $sp, 8
		jr	$ra

		.text
		.globl	Sgetstring
	Sgetstring:
		subu	$sp, $sp, 8
		sw	$fp, 4($sp)
		sw	$ra, 0($sp)
		addu	$fp, $sp, 8
		lw	$a0, 0($fp)
		li	$a1, 1024
		li	$v0, 8
		syscall
		lw	$ra, 0($sp)
		lw	$fp, 4($sp)
		addu	$sp, $sp, 8
		jr	$ra

### ducc -e suite/quiet/rtl/r02.c gives:

		.text
		.globl	Lf
	Lf:
		subu	$sp, $sp, 8
		sw	$fp, 4($sp)
		sw	$ra, 0($sp)
		addu	$fp, $sp, 8
	L100_f_end:
		lw	$ra, 0($sp)
		lw	$fp, 4($sp)
		addu	$sp, $sp, 8
		jr	$ra

		.data
		.align	4
	La:
		.space	40

		.text
		.globl	main
	main:
		subu	$sp, $sp, 16
		sw	$fp, 12($sp)
		sw	$ra, 8($sp)
		addu	$fp, $sp, 16
		la	$t0, La
		sw	$t0, 0($sp)
		subu	$sp, $sp, 4
		lw	$t0, 4($sp)
		sw	$t0, 0($sp)
		jal	Lf
		sw	$v0, 8($sp)
		addu	$sp, $sp, 4
	L101_main_end:
		lw	$ra, 8($sp)
		lw	$fp, 12($sp)
		addu	$sp, $sp, 16
		jr	$ra

		.text
		.globl	Sputint
	Sputint:
		subu	$sp, $sp, 8
		sw	$fp, 4($sp)
		sw	$ra, 0($sp)
		addu	$fp, $sp, 8
		lw	$a0, 0($fp)
		li	$v0, 1
		syscall
		lw	$ra, 0($sp)
		lw	$fp, 4($sp)
		addu	$sp, $sp, 8
		jr	$ra

		.text
		.globl	Sgetint
	Sgetint:
		subu	$sp, $sp, 8
		sw	$fp, 4($sp)
		sw	$ra, 0($sp)
		addu	$fp, $sp, 8
		li	$v0, 5
		syscall
		lw	$ra, 0($sp)
		lw	$fp, 4($sp)
		addu	$sp, $sp, 8
		jr	$ra

		.text
		.globl	Sputstring
	Sputstring:
		subu	$sp, $sp, 8
		sw	$fp, 4($sp)
		sw	$ra, 0($sp)
		addu	$fp, $sp, 8
		lw	$a0, 0($fp)
		li	$v0, 4
		syscall
		lw	$ra, 0($sp)
		lw	$fp, 4($sp)
		addu	$sp, $sp, 8
		jr	$ra

		.text
		.globl	Sgetstring
	Sgetstring:
		subu	$sp, $sp, 8
		sw	$fp, 4($sp)
		sw	$ra, 0($sp)
		addu	$fp, $sp, 8
		lw	$a0, 0($fp)
		li	$a1, 1024
		li	$v0, 8
		syscall
		lw	$ra, 0($sp)
		lw	$fp, 4($sp)
		addu	$sp, $sp, 8
		jr	$ra

### ducc -e suite/quiet/rtl/r03.c gives:

		.text
		.globl	Lf
	Lf:
		subu	$sp, $sp, 8
		sw	$fp, 4($sp)
		sw	$ra, 0($sp)
		addu	$fp, $sp, 8
	L100_f_end:
		lw	$ra, 0($sp)
		lw	$fp, 4($sp)
		addu	$sp, $sp, 8
		jr	$ra

		.text
		.globl	main
	main:
		subu	$sp, $sp, 76
		sw	$fp, 44($sp)
		sw	$ra, 40($sp)
		addu	$fp, $sp, 76
		li	$t0, 5
		sw	$t0, 0($sp)
		li	$t0, 0
		sw	$t0, 4($sp)
		li	$t0, 4
		sw	$t0, 8($sp)
		lw	$t0, 4($sp)
		lw	$t1, 8($sp)
		mul	$t2, $t0, $t1
		sw	$t2, 12($sp)
		li	$t0, 0
		sw	$t0, 16($sp)
		lw	$t0, 16($sp)
		lw	$t1, 12($sp)
		add	$t2, $t0, $t1
		sw	$t2, 20($sp)
		subu	$t0, $fp, 28
		lw	$t1, 20($sp)
		add	$t0, $t0, $t1
		sw	$t0, 24($sp)
		lw	$t0, 24($sp)
		lw	$t1, 0($sp)
		sw	$t1, 0($t0)
		li	$t0, 0
		sw	$t0, 28($sp)
		subu	$t0, $fp, 28
		lw	$t1, 28($sp)
		add	$t0, $t0, $t1
		sw	$t0, 32($sp)
		subu	$sp, $sp, 4
		lw	$t0, 36($sp)
		sw	$t0, 0($sp)
		jal	Lf
		sw	$v0, 40($sp)
		addu	$sp, $sp, 4
	L101_main_end:
		lw	$ra, 40($sp)
		lw	$fp, 44($sp)
		addu	$sp, $sp, 76
		jr	$ra

		.text
		.globl	Sputint
	Sputint:
		subu	$sp, $sp, 8
		sw	$fp, 4($sp)
		sw	$ra, 0($sp)
		addu	$fp, $sp, 8
		lw	$a0, 0($fp)
		li	$v0, 1
		syscall
		lw	$ra, 0($sp)
		lw	$fp, 4($sp)
		addu	$sp, $sp, 8
		jr	$ra

		.text
		.globl	Sgetint
	Sgetint:
		subu	$sp, $sp, 8
		sw	$fp, 4($sp)
		sw	$ra, 0($sp)
		addu	$fp, $sp, 8
		li	$v0, 5
		syscall
		lw	$ra, 0($sp)
		lw	$fp, 4($sp)
		addu	$sp, $sp, 8
		jr	$ra

		.text
		.globl	Sputstring
	Sputstring:
		subu	$sp, $sp, 8
		sw	$fp, 4($sp)
		sw	$ra, 0($sp)
		addu	$fp, $sp, 8
		lw	$a0, 0($fp)
		li	$v0, 4
		syscall
		lw	$ra, 0($sp)
		lw	$fp, 4($sp)
		addu	$sp, $sp, 8
		jr	$ra

		.text
		.globl	Sgetstring
	Sgetstring:
		subu	$sp, $sp, 8
		sw	$fp, 4($sp)
		sw	$ra, 0($sp)
		addu	$fp, $sp, 8
		lw	$a0, 0($fp)
		li	$a1, 1024
		li	$v0, 8
		syscall
		lw	$ra, 0($sp)
		lw	$fp, 4($sp)
		addu	$sp, $sp, 8
		jr	$ra

### ducc -e suite/quiet/rtl/r04.c gives:

		.text
		.globl	Lf
	Lf:
		subu	$sp, $sp, 52
		sw	$fp, 48($sp)
		sw	$ra, 44($sp)
		addu	$fp, $sp, 52
		li	$t0, 0
		sw	$t0, 0($sp)
		li	$t0, 1
		sw	$t0, 4($sp)
		lw	$t0, 0($sp)
		lw	$t1, 4($sp)
		mul	$t2, $t0, $t1
		sw	$t2, 8($sp)
		lw	$t0, 0($fp)
		lw	$t1, 8($sp)
		add	$t2, $t0, $t1
		sw	$t2, 12($sp)
		lw	$t0, 12($sp)
		lb	$t1, 0($t0)
		sb	$t1, 16($sp)
		li	$t0, 7
		sw	$t0, 20($sp)
		lw	$t0, 16($sp)
		lw	$t1, 20($sp)
		add	$t2, $t0, $t1
		sw	$t2, 24($sp)
		li	$t0, 1
		sw	$t0, 28($sp)
		li	$t0, 1
		sw	$t0, 32($sp)
		lw	$t0, 28($sp)
		lw	$t1, 32($sp)
		mul	$t2, $t0, $t1
		sw	$t2, 36($sp)
		lw	$t0, 0($fp)
		lw	$t1, 36($sp)
		add	$t2, $t0, $t1
		sw	$t2, 40($sp)
		lw	$t0, 40($sp)
		lb	$t1, 24($sp)
		sb	$t1, 0($t0)
	L100_f_end:
		lw	$ra, 44($sp)
		lw	$fp, 48($sp)
		addu	$sp, $sp, 52
		jr	$ra

		.data
		.align	4
	La:
		.space	7

		.text
		.globl	main
	main:
		subu	$sp, $sp, 40
		sw	$fp, 36($sp)
		sw	$ra, 32($sp)
		addu	$fp, $sp, 40
		li	$t0, 5
		sw	$t0, 0($sp)
		li	$t0, 0
		sw	$t0, 4($sp)
		li	$t0, 1
		sw	$t0, 8($sp)
		lw	$t0, 4($sp)
		lw	$t1, 8($sp)
		mul	$t2, $t0, $t1
		sw	$t2, 12($sp)
		la	$t0, La
		sw	$t0, 16($sp)
		lw	$t0, 16($sp)
		lw	$t1, 12($sp)
		add	$t2, $t0, $t1
		sw	$t2, 20($sp)
		lw	$t0, 20($sp)
		lb	$t1, 0($sp)
		sb	$t1, 0($t0)
		la	$t0, La
		sw	$t0, 24($sp)
		subu	$sp, $sp, 4
		lw	$t0, 28($sp)
		sw	$t0, 0($sp)
		jal	Lf
		sw	$v0, 32($sp)
		addu	$sp, $sp, 4
	L101_main_end:
		lw	$ra, 32($sp)
		lw	$fp, 36($sp)
		addu	$sp, $sp, 40
		jr	$ra

		.text
		.globl	Sputint
	Sputint:
		subu	$sp, $sp, 8
		sw	$fp, 4($sp)
		sw	$ra, 0($sp)
		addu	$fp, $sp, 8
		lw	$a0, 0($fp)
		li	$v0, 1
		syscall
		lw	$ra, 0($sp)
		lw	$fp, 4($sp)
		addu	$sp, $sp, 8
		jr	$ra

		.text
		.globl	Sgetint
	Sgetint:
		subu	$sp, $sp, 8
		sw	$fp, 4($sp)
		sw	$ra, 0($sp)
		addu	$fp, $sp, 8
		li	$v0, 5
		syscall
		lw	$ra, 0($sp)
		lw	$fp, 4($sp)
		addu	$sp, $sp, 8
		jr	$ra

		.text
		.globl	Sputstring
	Sputstring:
		subu	$sp, $sp, 8
		sw	$fp, 4($sp)
		sw	$ra, 0($sp)
		addu	$fp, $sp, 8
		lw	$a0, 0($fp)
		li	$v0, 4
		syscall
		lw	$ra, 0($sp)
		lw	$fp, 4($sp)
		addu	$sp, $sp, 8
		jr	$ra

		.text
		.globl	Sgetstring
	Sgetstring:
		subu	$sp, $sp, 8
		sw	$fp, 4($sp)
		sw	$ra, 0($sp)
		addu	$fp, $sp, 8
		lw	$a0, 0($fp)
		li	$a1, 1024
		li	$v0, 8
		syscall
		lw	$ra, 0($sp)
		lw	$fp, 4($sp)
		addu	$sp, $sp, 8
		jr	$ra

### ducc -e suite/quiet/rtl/r05.c gives:

		.text
		.globl	Lf
	Lf:
		subu	$sp, $sp, 52
		sw	$fp, 48($sp)
		sw	$ra, 44($sp)
		addu	$fp, $sp, 52
		li	$t0, 0
		sw	$t0, 0($sp)
		li	$t0, 1
		sw	$t0, 4($sp)
		lw	$t0, 0($sp)
		lw	$t1, 4($sp)
		mul	$t2, $t0, $t1
		sw	$t2, 8($sp)
		lw	$t0, 0($fp)
		lw	$t1, 8($sp)
		add	$t2, $t0, $t1
		sw	$t2, 12($sp)
		lw	$t0, 12($sp)
		lb	$t1, 0($t0)
		sb	$t1, 16($sp)
		li	$t0, 7
		sw	$t0, 20($sp)
		lw	$t0, 16($sp)
		lw	$t1, 20($sp)
		add	$t2, $t0, $t1
		sw	$t2, 24($sp)
		li	$t0, 1
		sw	$t0, 28($sp)
		li	$t0, 1
		sw	$t0, 32($sp)
		lw	$t0, 28($sp)
		lw	$t1, 32($sp)
		mul	$t2, $t0, $t1
		sw	$t2, 36($sp)
		lw	$t0, 0($fp)
		lw	$t1, 36($sp)
		add	$t2, $t0, $t1
		sw	$t2, 40($sp)
		lw	$t0, 40($sp)
		lb	$t1, 24($sp)
		sb	$t1, 0($t0)
	L100_f_end:
		lw	$ra, 44($sp)
		lw	$fp, 48($sp)
		addu	$sp, $sp, 52
		jr	$ra

		.text
		.globl	main
	main:
		subu	$sp, $sp, 60
		sw	$fp, 44($sp)
		sw	$ra, 40($sp)
		addu	$fp, $sp, 60
		li	$t0, 5
		sw	$t0, 0($sp)
		li	$t0, 0
		sw	$t0, 4($sp)
		li	$t0, 1
		sw	$t0, 8($sp)
		lw	$t0, 4($sp)
		lw	$t1, 8($sp)
		mul	$t2, $t0, $t1
		sw	$t2, 12($sp)
		li	$t0, 0
		sw	$t0, 16($sp)
		lw	$t0, 16($sp)
		lw	$t1, 12($sp)
		add	$t2, $t0, $t1
		sw	$t2, 20($sp)
		subu	$t0, $fp, 12
		lw	$t1, 20($sp)
		add	$t0, $t0, $t1
		sw	$t0, 24($sp)
		lw	$t0, 24($sp)
		lb	$t1, 0($sp)
		sb	$t1, 0($t0)
		li	$t0, 0
		sw	$t0, 28($sp)
		subu	$t0, $fp, 12
		lw	$t1, 28($sp)
		add	$t0, $t0, $t1
		sw	$t0, 32($sp)
		subu	$sp, $sp, 4
		lw	$t0, 36($sp)
		sw	$t0, 0($sp)
		jal	Lf
		sw	$v0, 40($sp)
		addu	$sp, $sp, 4
	L101_main_end:
		lw	$ra, 40($sp)
		lw	$fp, 44($sp)
		addu	$sp, $sp, 60
		jr	$ra

		.text
		.globl	Sputint
	Sputint:
		subu	$sp, $sp, 8
		sw	$fp, 4($sp)
		sw	$ra, 0($sp)
		addu	$fp, $sp, 8
		lw	$a0, 0($fp)
		li	$v0, 1
		syscall
		lw	$ra, 0($sp)
		lw	$fp, 4($sp)
		addu	$sp, $sp, 8
		jr	$ra

		.text
		.globl	Sgetint
	Sgetint:
		subu	$sp, $sp, 8
		sw	$fp, 4($sp)
		sw	$ra, 0($sp)
		addu	$fp, $sp, 8
		li	$v0, 5
		syscall
		lw	$ra, 0($sp)
		lw	$fp, 4($sp)
		addu	$sp, $sp, 8
		jr	$ra

		.text
		.globl	Sputstring
	Sputstring:
		subu	$sp, $sp, 8
		sw	$fp, 4($sp)
		sw	$ra, 0($sp)
		addu	$fp, $sp, 8
		lw	$a0, 0($fp)
		li	$v0, 4
		syscall
		lw	$ra, 0($sp)
		lw	$fp, 4($sp)
		addu	$sp, $sp, 8
		jr	$ra

		.text
		.globl	Sgetstring
	Sgetstring:
		subu	$sp, $sp, 8
		sw	$fp, 4($sp)
		sw	$ra, 0($sp)
		addu	$fp, $sp, 8
		lw	$a0, 0($fp)
		li	$a1, 1024
		li	$v0, 8
		syscall
		lw	$ra, 0($sp)
		lw	$fp, 4($sp)
		addu	$sp, $sp, 8
		jr	$ra

### ducc suite/noisy/simple/sim04.c > test.s && spim -f test.s gives:

    Hello
    Good bye

### ducc suite/noisy/simple/sim06.c > test.s && spim -f test.s gives:

    9876543210

### ducc suite/noisy/simple/sim07.c > test.s && spim -f test.s gives:

    0 XY
    1
    2 X
    3 Y
    4 X
    5
    6 XY
    7
    8 X
    9 Y
    10 X
    11 W
    12 XY
    13 W
    14 XW
    15 Y
    16 XW
    17 W
    18 XY
    19 W
    20 XW

### ducc suite/noisy/simple/sim10.c > test.s && spim -f test.s gives:

    Your name? Emil & Sebastian
    Your age 53
    You are: Emil & Sebastian

    You are: 53

### ducc suite/noisy/medium/fac.c > test.s && spim -f test.s, with input 5 gives:

    120

### ducc suite/noisy/medium/fac.c > test.s && spim -f test.s, with input 10 gives:

    3628800

### ducc suite/noisy/medium/fib.c > test.s && spim -f test.s gives:

    0 1
    1 1
    2 2
    3 3
    4 5
    5 8
    6 13
    7 21
    8 34
    9 55
    10 89
    11 144
    12 233

### ducc suite/noisy/advanced/8queens.c > test.s && spim -f test.s gives:

    04752613

### ducc suite/noisy/advanced/quick.c > test.s && spim -f test.s gives:

    lctkbsjarizqhypgxofwnevmdu
    dcefbgharizqjypsxokwntvmlu
    dceabghfrizqjypsxokwntvmlu
    dcbaeghfrizqjypsxokwntvmlu
    abcdeghfrizqjypsxokwntvmlu
    abcdeghfrizqjypsxokwntvmlu
    abcdeghfrizqjypsxokwntvmlu
    abcdegfhrizqjypsxokwntvmlu
    abcdefghrizqjypsxokwntvmlu
    abcdefghriuqjlpsmokwntvxyz
    abcdefghrinqjlpkmoswutvxyz
    abcdefghjinqrlpkmoswutvxyz
    abcdefghijnqrlpkmoswutvxyz
    abcdefghijklrqpnmoswutvxyz
    abcdefghijklrqpnmoswutvxyz
    abcdefghijklomnpqrswutvxyz
    abcdefghijklmonpqrswutvxyz
    abcdefghijklmnopqrswutvxyz
    abcdefghijklmnopqrswutvxyz
    abcdefghijklmnopqrstuwvxyz
    abcdefghijklmnopqrstuwvxyz
    abcdefghijklmnopqrstuvwxyz
    abcdefghijklmnopqrstuvwxyz
    abcdefghijklmnopqrstuvwxyz
