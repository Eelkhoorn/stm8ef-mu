                                      1 ;--------------------------------------------------------
                                      2 ; File Created by SDCC : free open source ANSI-C Compiler
                                      3 ; Version 4.2.0 #13081 (Linux)
                                      4 ;--------------------------------------------------------
                                      5 	.module main
                                      6 	.optsdcc -mstm8
                                      7 	
                                      8 ;--------------------------------------------------------
                                      9 ; Public variables in this module
                                     10 ;--------------------------------------------------------
                                     11 	.globl _main
                                     12 	.globl _forth
                                     13 	.globl _forthData
                                     14 	.globl _forthUser
                                     15 ;--------------------------------------------------------
                                     16 ; ram data
                                     17 ;--------------------------------------------------------
                                     18 	.area DATA
                           000030    19 _forthUser	=	0x0030
                           000080    20 _forthData	=	0x0080
                                     21 ;--------------------------------------------------------
                                     22 ; ram data
                                     23 ;--------------------------------------------------------
                                     24 	.area INITIALIZED
                                     25 ;--------------------------------------------------------
                                     26 ; Stack segment in internal ram
                                     27 ;--------------------------------------------------------
                                     28 	.area	SSEG
      000001                         29 __start__stack:
      000001                         30 	.ds	1
                                     31 
                                     32 ;--------------------------------------------------------
                                     33 ; absolute external ram data
                                     34 ;--------------------------------------------------------
                                     35 	.area DABS (ABS)
                                     36 
                                     37 ; default segment ordering for linker
                                     38 	.area HOME
                                     39 	.area GSINIT
                                     40 	.area GSFINAL
                                     41 	.area CONST
                                     42 	.area INITIALIZER
                                     43 	.area CODE
                                     44 
                                     45 ;--------------------------------------------------------
                                     46 ; interrupt vector
                                     47 ;--------------------------------------------------------
                                     48 	.area HOME
      008000                         49 __interrupt_vect:
      008000 82 00 80 6B             50 	int s_GSINIT ; reset
      008004 82 00 00 00             51 	int 0x000000 ; trap
      008008 82 00 00 00             52 	int 0x000000 ; int0
      00800C 82 00 00 00             53 	int 0x000000 ; int1
      008010 82 00 00 00             54 	int 0x000000 ; int2
      008014 82 00 81 61             55 	int _EXTI0_IRQHandler ; int3
      008018 82 00 81 61             56 	int _EXTI1_IRQHandler ; int4
      00801C 82 00 81 61             57 	int _EXTI2_IRQHandler ; int5
      008020 82 00 81 61             58 	int _EXTI3_IRQHandler ; int6
      008024 82 00 81 61             59 	int _EXTI4_IRQHandler ; int7
      008028 82 00 00 00             60 	int 0x000000 ; int8
      00802C 82 00 00 00             61 	int 0x000000 ; int9
      008030 82 00 00 00             62 	int 0x000000 ; int10
      008034 82 00 81 61             63 	int _TIM1_IRQHandler ; int11
      008038 82 00 00 00             64 	int 0x000000 ; int12
      00803C 82 00 81 61             65 	int _TIM2_IRQHandler ; int13
      008040 82 00 00 00             66 	int 0x000000 ; int14
      008044 82 00 81 61             67 	int _TIM3_IRQHandler ; int15
      008048 82 00 00 00             68 	int 0x000000 ; int16
      00804C 82 00 00 00             69 	int 0x000000 ; int17
      008050 82 00 00 00             70 	int 0x000000 ; int18
      008054 82 00 00 00             71 	int 0x000000 ; int19
      008058 82 00 00 00             72 	int 0x000000 ; int20
      00805C 82 00 00 00             73 	int 0x000000 ; int21
      008060 82 00 00 00             74 	int 0x000000 ; int22
      008064 82 00 81 61             75 	int _TIM4_IRQHandler ; int23
                                     76 ;--------------------------------------------------------
                                     77 ; global & static initialisations
                                     78 ;--------------------------------------------------------
                                     79 	.area HOME
                                     80 	.area GSINIT
                                     81 	.area GSFINAL
                                     82 	.area GSINIT
      00806B                         83 __sdcc_init_data:
                                     84 ; stm8_genXINIT() start
      00806B AE 00 00         [ 2]   85 	ldw x, #l_DATA
      00806E 27 07            [ 1]   86 	jreq	00002$
      008070                         87 00001$:
      008070 72 4F 00 00      [ 1]   88 	clr (s_DATA - 1, x)
      008074 5A               [ 2]   89 	decw x
      008075 26 F9            [ 1]   90 	jrne	00001$
      008077                         91 00002$:
      008077 AE 00 00         [ 2]   92 	ldw	x, #l_INITIALIZER
      00807A 27 09            [ 1]   93 	jreq	00004$
      00807C                         94 00003$:
      00807C D6 80 87         [ 1]   95 	ld	a, (s_INITIALIZER - 1, x)
      00807F D7 00 00         [ 1]   96 	ld	(s_INITIALIZED - 1, x), a
      008082 5A               [ 2]   97 	decw	x
      008083 26 F7            [ 1]   98 	jrne	00003$
      008085                         99 00004$:
                                    100 ; stm8_genXINIT() end
                                    101 	.area GSFINAL
      008085 CC 80 68         [ 2]  102 	jp	__sdcc_program_startup
                                    103 ;--------------------------------------------------------
                                    104 ; Home
                                    105 ;--------------------------------------------------------
                                    106 	.area HOME
                                    107 	.area HOME
      008068                        108 __sdcc_program_startup:
      008068 CC 80 88         [ 2]  109 	jp	_main
                                    110 ;	return from main will return to caller
                                    111 ;--------------------------------------------------------
                                    112 ; code
                                    113 ;--------------------------------------------------------
                                    114 	.area CODE
                                    115 ;	main.c: 20: void main(void)
                                    116 ;	-----------------------------------------
                                    117 ;	 function main
                                    118 ;	-----------------------------------------
      008088                        119 _main:
                                    120 ;	main.c: 24: forth();              // the Forth REPL never returns
                                    121 ;	main.c: 28: }
      008088 CC 81 9D         [ 2]  122 	jp	_forth
                                    123 	.area CODE
                                    124 	.area CONST
                                    125 	.area INITIALIZER
                                    126 	.area CABS (ABS)
