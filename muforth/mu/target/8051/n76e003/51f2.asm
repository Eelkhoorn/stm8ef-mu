hex
forth
: EXIT 22 \m c, ;
__meta

: SWAP  ( n1 \ n2 -- n2 \ n1 )   
  [ 	C583 ,		 ( XCH A, DPH )
	C6 c,		 ( XCH A, @R0 )
	C583 ,		 ( XCH A, DPH )
	08 c,		 ( INC R0 )
	C582 ,		 ( XCH A, DPL )
	C6 c,		 ( XCH A, @R0 )
	C582 ,		 ( XCH A, DPL )
	18 c,	] ; 	 ( DEC R0 )  
: DUP  ( n -- n \ n )   
  [ 	18 c,		 ( DEC R0 )
	A682 ,		 ( MOV @R0, DPL )
	18 c,		 ( DEC R0 )
	A683 ,	] ;	 ( MOV @R0, DPH ) 
: OVER  ( n1 \ n2 -- n1 \ n2 \ n1 )   
  [ 	A900 ,	]	 ( MOV R1, R0 )
	DUP  
  [	8783 ,		 ( MOV DPH, R1 )
	09 c,		 ( INC R1 )
	8782 ,	] ;	 ( MOV DPL, R1 )
: ?branch  ( f -- )  [ __asm
		DPH a mov
		0= if
		DPL a mov 
		0= if ]
		DROP [ __asm
		 2 c,  
		then then ;c
label branch
	

: DROP  ( n -- )   
  [ 	8683 ,		 ( MOV DPH, R0 )
	08 c,		 ( INC R0 )
	8682 ,		 ( MOV DPL, R0 )
	08 c,	] ;	 ( INC R0 )  
: NIP  ( n1 \ n2 -- n2 )   
  [ 	08 c,		 ( INC R0 )
	08 c,	] ;	 ( INC R0 )  

( ==== Memory primitives ==== )
: @   ( a -- n ) 
  [ 	E0 c,		 ( MOVX A, @DPTR )
	F9 c,		 ( MOV R1, A )
	A3 c,		 ( INC DPTR )
	E0 c,		 ( MOVX A, @DPTR )
	F582 ,		 ( MOV DPL, A )
	8983 ,	] ;	 ( MOV DPH, R1 ) 
: !   ( n \ a -- ) 
  [ 	E6 c,		 ( MOV A, @R0 )
	F0 c,		 ( MOVX @DPTR, A )
	08 c,		 ( INC R0 )
	A3 c,		 ( INC DPTR )
	E6 c,		 ( MOV A, @R0 )
	F0 c,		 ( MOVX @DPTR, A )
	08 c,	]	 ( INC R0 )
	DROP 		 ;
: C@  ( a -- c ) 
  [ 	E0 c,		 ( MOVX A, @DPTR )
	F582 ,		 ( MOV DPL, A )
	75 c, 8300 ,  ] ;	( MOV DPH, #0 ) 
: C!  ( c \ a -- ) 
  [ 	08 c,		 ( INC R0 )
	E6 c,		 ( MOV A, @R0 )
	F0 c,		 ( MOVX @DPTR, A )
	08 c,	]	 ( INC R0 )
	DROP 	 ;

( ==== Arithmetic operators ==== )
: +   ( n1 \ n2 -- n1-n2 )   
  [ 	86F0 ,		 ( MOV B, @R0 )
	08 c,		 ( INC R0 )
	E6 c,		 ( MOV A, @R0 )
	08 c,		 ( INC R0 )
	2582 ,		 ( ADD A, DPL )
	F582 ,		 ( MOV DPL, A )
	E5F0 ,		 ( MOV A, B )
	3583 ,		 ( ADDC A, DPH )
	F583 ,	 ] ;	 ( MOV DPH, A ) 
: -   ( n1 \ n2 -- n1-n2 )   
  [ 	86F0 ,		 ( MOV B, @R0 )
	08 c,		 ( INC R0 )
	E6 c,		 ( MOV A, @R0 )
	08 c,		 ( INC R0 )
	C3 c,		 ( CLR C )
	9582 ,		 ( SUBB A, DPL )
	F582 ,		 ( MOV DPL, A )
	E5F0 ,		 ( MOV A, B )
	9583 ,		 ( SUBB A, DPH )
	F583 ,	] ;	 ( MOV DPH, A ) 
: NOT  ( n1 -- _n2 )   
  [ 	74FF ,		 ( MOV A, #FFH )
	6283 ,		 ( XRL DPH, A )
	6282 ,	] ;	 ( XRL DPL, A ) 
: AND  ( n1 \ n2 -- n1.and.n2 )   
  [ 	E6 c,		 ( MOV A, @R0 )
	08 c,		 ( INC R0 )
	5283 ,		 ( ANL DPH, A )
	E6 c,		 ( MOV A, @R0 )
	08 c,		 ( INC R0 )
	5282 ,	] ;	 ( ANL DPL, A ) 
: OR  ( n1 \ n2 -- n1.or.n2 )  
  [ 	E6 c,		 ( MOV A, @R0 )
	08 c,		 ( INC R0 )
	4283 ,		 ( ORL DPH, A )
	E6 c,		 ( MOV A, @R0 )
	08 c,		 ( INC R0 )
	4282 ,	] ;	 ( ORL DPL, A ) 
: XOR  ( n1 \ n2 -- n1.xor.n2 )  
  [ 	E6 c,		 ( MOV A, @R0 )
	08 c,		 ( INC R0 )
	6283 ,		 ( XRL DPH, A )
	E6 c,		 ( MOV A, @R0 )
	08 c,		 ( INC R0 )
	6282 ,	] ;	 ( XRL DPL, A ) 
: 2*  ( n -- 2n* )   
  [ 	C3 c,		 ( CLR C )
	E582 ,		 ( MOV A, DPL )
	33 c,		 ( RLC A )
	F582 ,		 ( MOV DPL, A )
	E583 ,		 ( MOV A, DPH )
	33 c,		 ( RLC A )
	F583 ,	] ;	 ( MOV DPH, A ) 
: 2/  ( n -- n2/ )   
  [ 	E583 ,		 ( MOV A, DPH )
	A2E7 ,		 ( MOV C, ACC.7 )
	13 c,		 ( RRC A )
	F583 ,		 ( MOV DPH, A )
	E582 ,		 ( MOV A, DPL )
	13 c,		 ( RRC A )
	F582 ,	] ;	 ( MOV DPL, A ) 
: U2/  ( u -- u2/ )   
  [ 	E583 ,		 ( MOV A, DPH )
	C3 c,		 ( CLR C )
	13 c,		 ( RRC A )
	F583 ,		 ( MOV DPH, A )
	E582 ,		 ( MOV A, DPL )
	13 c,		 ( RRC A )
	F582 ,	] ;	 ( MOV DPL, A ) 
label NEGATE ;c
: NEGATE  ( n -- _n )   
  [ 	C3 c,		 ( CLR C )
	E4 c,		 ( CLR A )
	9582 ,		 ( SUBB A, DPL )
	F582 ,		 ( MOV DPL, A )
	E4 c,		 ( CLR A )
	9583 ,		 ( SUBB A, DPH )
	F583 ,	] ;	 ( MOV DPH, A )
	label ABS ( n -- n )
	DPH a mov
	ACC .7 bclr? NEGATE rel ret ;c
	
: 0<  ( n -- flag )   
  [ 	E583 ,		( MOV A, DPH )
	20 c, E704 ,	( JB ACC.7, 4 )
	90 c, 0000 ,	( MOV DPTR, #0 )
	22 c,		( RET )
	90 c, FFFF ,  ] ;	( MOV DPTR, #FFFFH ) 
: <  ( n \ m -- flag )  2DUP XOR 0< if DROP 0< EXIT then       - 0< ;
: >  ( n \ m -- flag )  2DUP XOR 0< if  NIP 0< EXIT then  SWAP - 0< ;
: U<  ( n \ m -- flag )  
  [ 	E6 c,		 ( MOV A, @R0 )
	08 c,		 ( INC R0 )
	B5 c, 8304 ,  ( CJNE A, DPH, 4 )
	E6 c,		 ( MOV A, @R0 )
	B5 c, 8200 ,  ( CJNE A, DPL, 0 )
	08 c,		 ( INC R0 )
	4004 ,		 ( JC 4 )
	90 c, 0000 ,  ( MOV DPTR, #0 )
	22 c,		 ( RET )
--	90 c, FFFF ,  ] ;	( MOV DPTR, #FFFFH )
 
: U>  ( n \ m -- flag )  SWAP  U<  ;
: 0=  ( n -- flag )       if NO EXIT then YES ;
: =  ( n \ m -- flag )  - if NO EXIT then YES ;
  [ 	20 c,		 CONSTANT BL

( ==== Return stack primitives ==== )
: R>DROP  ( -- )   
  [ 	D002 ,		 ( POP R2 )
	D003 ,		 ( POP R3 )
	1581 ,		 ( DEC SP )
	1581 ,		 ( DEC SP )
	C003 ,		 ( PUSH R3 )
	C002 ,	] ;	 ( PUSH R2 )  
: >R  ( a -- )   
  [ 	D002 ,		 ( POP R2 )
	D003 ,		 ( POP R3 )
	C082 ,		 ( PUSH DPL )
	C083 ,		 ( PUSH DPH )
	C003 ,		 ( PUSH R3 )
	C002 ,	]	 ( PUSH R2 )
	DROP 	 ;
: R>  ( -- a )
  DUP  
  [ 	D002 ,		 ( POP R2 )
	D003 ,		 ( POP R3 )
	D083 ,		 ( POP DPH )
	D082 ,		 ( POP DPL )
	C003 ,		 ( PUSH R3 )
	C002 ,	 ] ;	 ( PUSH R2 )  
: R  ( -- a )
  DUP
  [ 	1581 ,		 ( DEC SP )
	1581 ,		 ( DEC SP )
	D083 ,		 ( POP DPH )
	D082 ,		 ( POP DPL )
	0581 ,		 ( INC SP )
	0581 ,		 ( INC SP )
	0581 ,		 ( INC SP )
	0581 ,	 ] ;	 ( INC SP )  
: I  ( -- a )
  DUP
  [ 	1581 ,		 ( DEC SP )
	1581 ,		 ( DEC SP )
	D083 ,		 ( POP DPH )
	D082 ,		 ( POP DPL )
	0581 ,		 ( INC SP )
	0581 ,		 ( INC SP )
	0581 ,		 ( INC SP )
	0581 ,	] ;	 ( INC SP )  
: DUP>R  ( n -- n )   
  [ 	D002 ,		 ( POP R2 )
	D003 ,		 ( POP R3 )
	C082 ,		 ( PUSH DPL )
	C083 ,		 ( PUSH DPH )
	C003 ,		 ( POP R3 )
	C002 ,	 ] ;	 ( POP R2 )
  

: (VAR)  ( -- addr )  R>  ;
: (CONST)  ( -- val )  R>  @  ;
: EXECUTE ( a -- )   >R  ;
: @+  ( a -- n\ a+ )  DUP @ SWAP 2+ ;
: @-  ( a -- n\ a- )  DUP @ SWAP 2 - ;
: C@+ ( a -- c\ a+ )  DUP C@ SWAP 1+ ;
: C@- ( a -- c\ a- )  DUP C@ SWAP 1 - ;
: !+  ( n \ a -- a+ )  SWAP OVER ! 2+ ;
: !-  ( n \ a -- a- )  SWAP OVER ! 2 - ;
: C!+ ( c \ a -- a+ )  SWAP OVER C! 1+ ;
: C!- ( c \ a -- a- )  SWAP OVER C! 1 - ;
: TUCK  ( a \ b -- b \ a \ b )  SWAP OVER ;
: ROT  ( a \ b \ c -- b \ c \ a )  >R SWAP  R> SWAP ;
: 2DUP  ( a \ b -- a \ b \ a \ b )  OVER OVER ;
: 2DROP  ( a \ b -- )  DROP DROP ;


: (IRAM)  ( addr -- )  R 1+  C! ;
: (IRAMD)  ( addr -- space )  DUP  R 1+  C! ;
: (IRAM2)  ( c \ addr -- )  R 1+ C!  R 2+ C! ;


comment ===
: (C@I)  ( addr -- c )  ' (IRAMD) COMPILE  
  [ 	8500 ,		 , 82 C, ( MOV ?, DPL )
	7583 ,		 , 0 C, ; IMMEDIATE ( MOV DPH, #0 )
: (C!I)  ( c \ addr -- )  ' (IRAM2) COMPILE  
  [ 	75 c,		 C, 0 , ; IMMEDIATE ( MOV ?, ? )
: (BSETI)  ( bit addr -- )  ' (IRAM) COMPILE 
  [ 	D200 ,		 , ; IMMEDIATE ( SETB ? )
: (BCLRI)  ( bit addr -- )  ' (IRAM) COMPILE 
  [ 	C200 ,		 , ; IMMEDIATE ( CLR ? )
: (BCPLI)  ( bit addr -- )  ' (IRAM) COMPILE 
  [ 	B200 ,		 , ; IMMEDIATE ( CPL ? )
: (B?I)  ( bit addr -- flag )  ' (IRAMD) COMPILE 
  [ 	2000 ,		 , 04 C, ( JB ?, 4 )
	9000 ,		 , 00 C, ( MOV DPTR, #0 )
	22 c,		 C, ( RET )
	90FF ,		 , FF C, ; IMMEDIATE ( MOV DPTR, #FFFFH )

: C@I  ( addr -- c )  (C@I) ;
: C!I  ( c \ addr -- )  (C!I) ;
: BSETI  ( bit addr -- )  (BSETI) ;
: BCLRI  ( bit addr -- )  (BCLRI) ;
: BCPLI  ( bit addr -- )  (BCPLI) ;
: B?I  ( bit addr -- flag )  (B?I) ;
  [ 	-1 c,		 CONSTANT YES


: COUNT  ( addr -- addr+ \ count )  C@+ SWAP ;
: CMOVE  ( src \ dest \ count -- )  for >R COUNT R> C!+ next 2DROP ;
: <CMOVE  ( src \ dest \ count -- )  >R  SWAP R 1 - +  SWAP R 1 - +
  [ 	R> c,		 for >R C@- SWAP R> C!- next 2DROP ;
: MOVE  ( src \ dest \ count -- )  for >R @+ SWAP R> !+ next 2DROP ;
: FILL  ( addr \ count \ char -- )
  [ 	SWAP ,		 >R SWAP R> for 2DUP C! 1+ next 2DROP ;
: ERASE ( addr \ count -- )  0 FILL ;
: +!  ( n \ addr -- )  DUP>R @ + R> ! ;
: 1+   ( n1 \ 1+n1 )   
  [ 	A3 c,		 ] ; IMMEDIATE ( INC DPTR )
: 2+   ( n1 \ 2+n1 )   
  [ 	A3 c,		] ( INC DPTR )
	A3 c,		] ; IMMEDIATE ( INC DPTR )


: +BITS  ( bits \ addr -- )  TUCK  C@ OR  SWAP C! ;
: -BITS  ( bits \ addr -- )  >R  NOT R C@ AND  R> C! ;
: /MOD  ( n \ m -- r \ q )   
  [ 	7B00 ,		 ( MOV R3, #0 )
	7A00 ,		 ( MOV R2, #0 )
	7911 ,		 ( MOV R1, #11H )
	C3 c,		 ( CLR C )
	8014 ,		 ( SJMP 14H )
	AD03 ,		 ( MOV R5, R3 )
	AC02 ,		 ( MOV R4, R2 )
	C3 c,		 ( CLR C )
	EA c,		 ( MOV A, R2 )
	9582 ,		 ( SUBB A, DPL )
	FA c,		 ( MOV R2, A )
	EB c,		 ( MOV A, R3 )
	9583 ,		 ( SUBB A, DPH )
	FB c,		 ( MOV R3, A )
	5004 ,		 ( JNC 4 )
	AB05 ,		 ( MOV R3, R5 )
	AA04 ,		 ( MOV R2, R4 )
	B3 c,		 ( CPL C )
	08 c,		 ( INC R0 )
	E6 c,		 ( MOV A, @R0 )
	33 c,		 ( RLC A )
	F6 c,		 ( MOV @R0, A )
	18 c,		 ( DEC R0 )
	E6 c,		 ( MOV A, @R0 )
	33 c,		 ( RLC A )
	F6 c,		 ( MOV @R0, A )
	9200 ,		 ( MOV 0, C )
	D902 ,		 ( DJNZ R1, 2)
	800A ,		 ( SJMP 0AH )
	A200 ,		 ( MOV C, 0 )
	EA c,		 ( MOV A, R2 )
	33 c,		 ( RLC A )
	FA c,		 ( MOV R2, A )
	EB c,		 ( MOV A, R3 )
	33 c,		 ( RLC A )
	FB c,		 ( MOV R3, A )
	80D4 ,		 ( SJMP D4H )
	8683 ,		 ( MOV DPH, @R0 )
	08 c,		 ( INC R0 )
	8682 ,		 ( MOV DPL, @R0 )
	A602 ,		 ( MOV @R0, R2 )
	18 c,		 ( DEC R0 )
	A603 ,	 ] ;	 ( MOV @R0, R3 ) 
: /  ( n \ m -- quot )  /MOD  NIP ;
: MOD  ( n \ m -- rem )  /MOD  DROP ;
: U*  ( n \ m -- nm* )   
  [ 	E583 ,		 ( MOV A, DPH )
	08 c,		 ( INC R0 )
	86F0 ,		 ( MOV B, @R0 )
	A4 c,		 ( MUL A, B )
	F583 ,		 ( MOV DPH, A )
	E582 ,		 ( MOV A, DPL )
	18 c,		 ( DEC R0 )
	86F0 ,		 ( MOV B, @R0 )
	A4 c,		 ( MUL A, B )
	2583 ,		 ( ADD A, DPH )
	F583 ,		 ( MOV DPH, A )
	E582 ,		 ( MOV A, DPL )
	08 c,		 ( INC R0 )
	86F0 ,		 ( MOV B, @R0 )
	A4 c,		 ( MUL A, B )
	F582 ,		 ( MOV DPL, A )
	E5F0 ,		 ( MOV A, B )
	2583 ,		 ( ADD A, DPH )
	F583 ,		 ( MOV DPH, A )
	08 c,	 ] ;	 ( INC R0 )  

: MAX  ( n \ m -- p )  2DUP < if NIP EXIT then DROP ;
: MIN  ( n \ m -- p )  2DUP > if NIP EXIT then DROP ;

: >Q  ( n \ queue -- )    DUP>R  @  !-  DUP 4 - R - if R> ! EXIT then  @ R> ! ;
: Q>  ( queue -- n )  2+ DUP>R  @  @-  DUP 2 - R - if R> ! EXIT then  @ R> ! ;
: Q  ( queue -- n )  2+ @ @ ;
: 0Q  ( queue -- )  @+ ! ;
: Q?  ( queue -- n )
  [ 	@+ c,		 @+ >R SWAP - DUP 0< if R @ R> - + 2/ EXIT then R>DROP 2/ ;

: TX  ( char -- )
  [ 	C299 ,		 ( CLR TI )
	85 c, 8299 ,  ( MOV SBUF, DPL )
	DROP ,		 ;
: RX  ( -- char )
  [ 	C298 ,		 ; ( CLR RI )

( ==== Sio port servicing ==== )
0 VARIABLE sioa-in	( points to queue used to hold input for port a )
0 VARIABLE sioa-out	( points to queue used to hold output for port a )


: HEX  ( -- )  10 base ! ;
: BIN  ( -- )  2 base ! ;
: DECIMAL  ( -- )  A base ! ;
: PAD ( -- addr )  HERE 50 + ;
: HOLD  ( char -- )  -1 PAD +!  PAD @ C! ;
: <#  ( -- )  PAD DUP ! ;
: #>  ( n -- addr \ count )  DROP PAD @ PAD OVER - ;
: SIGN  ( m \ n -- n )  SWAP 0< if 2D HOLD then ;
: #  ( n -- n )  base @ /MOD  SWAP  9 OVER <  if 7 + then  30 +  HOLD ;
: #S  ( n -- n )  BEGIN # DUP 0= UNTIL ;
: DEPTH  ( -- )  0 C@I 7E SWAP - 2/ ;
: .R  ( n \ m -- )  >R  <# base @ A = if DUP ABS #S SIGN ELSE #S then #>
  [ 	R> c,		 OVER - SPACES TYPE ;
: .  ( n -- )  DEPTH if 0 .R SPACE then ;
: KEY?  ( -- flag )  keyq Q? ;
: KEY   ( -- char )  BEGIN  KEY? 0=  WHILE  BARON  REPEAT  keyq Q> ;
: INPUT  ( -- addr )  tib @ in @ + ;
: +IN  ( addr -- )  INPUT - in +! ;
: SKIP  ( char -- )  >R  INPUT BEGIN COUNT  DUP R -  SWAP 0=  OR UNTIL  1 - +IN
: SCAN  ( char -- )  >R  INPUT BEGIN COUNT  DUP R =  SWAP 0=  OR UNTIL  R>DROP
: +CHAR  ( -- )  INPUT C@+  SWAP if +IN  ELSE DROP then ;
: PARSE  ( char -- )  >R  INPUT  HERE 1+
  [ 	>R c,		 C@+ SWAP R> C!+ ( copy from input to here )
	RE c, PEAT ,  R>DROP HERE 1+ - HERE C! +IN +CHAR ;

: WORD  ( char -- )  DUP SKIP PARSE ;
: (  ( -- )  29 SCAN ; IMMEDIATE
: COMPILE  ( addr -- )  
  [ 	12 c,		 C, , ; ( LCALL addr )

( ==== Strings ==== )
: QUOTE  ( -- )  22 PARSE  HERE C@ 1+ ALLOT  ;
: (")  ( -- addr )  R>  DUP  C@+ + >R ;
: "  ( -- )  ' (") COMPILE  QUOTE ; IMMEDIATE
: (.")  ( -- )  R>  C@+  2DUP + >R  SWAP TYPE ;
: ."  ( -- )  compile @ if ' (.") COMPILE  QUOTE
  [ 	ELSE ,		 22 PARSE HERE COUNT TYPE then ; IMMEDIATE

( ==== Errors ==== )
: SP!  ( ? -- )  
  [ 	7880 ,		 , ; IMMEDIATE ( MOV R0, #80H )
: RP!  ( -- )  
  [ 	7581 ,		 , 09 C, ; IMMEDIATE ( MOV SP, #9 )

: ABORT  ( -- )  RP!  ;
: ERROR  ( -- )  HERE COUNT TYPE  ." <- bakarashii" ABORT ;
: ?ERROR  ( flag -- )  if ERROR then ;
: DIGIT  ( char -- n \ flag )
  [ 	30 c,		 - DUP 9 > if 7 - DUP A < OR then DUP base @ U< ;
: NUMBER  ( string -- n )  COUNT >R  COUNT 2D = TUCK
  [ 	if c,		 R> 1 - >R ELSE 1 - then 0 SWAP
	R> c,		 for C@+ >R DIGIT NOT ?ERROR SWAP base @ U* + R> next DROP
	SWAP ,		 if NEGATE then ;
: XQUOTE  ( -- )  22 WORD  HERE C@+ SWAP 2/ DUP C,
: X"  ( -- )  ' (") COMPILE  XQUOTE ;  IMMEDIATE
: T"  ( -- )  20 WORD  HERE C@+ SWAP 2/
: C>LINK  ( code -- link )  BEGIN 1 - DUP C@ #nfa AND UNTIL  2 - ;
: L>CODE  ( link -- code )  2+  COUNT 1F AND +  ;
: COMPARE  ( string \ name -- flag )  C@+ >R 3F AND >R  COUNT DUP R> =
  [ 	if c,		 BEGIN ?DUP WHILE SWAP 
	 COUNT  R> C@+ >R  - if 2DROP R>DROP NO EXIT then
	SWAP ,		 1 - REPEAT YES
	ELSE ,		 DROP NO then NIP R>DROP ;
: SEARCH?  ( string \ >list -- addr \ nc | -- string \ no )  
  [ 	if c,		 NIP DUP L>CODE SWAP 2+ C@ EXIT then
	RE c, PEAT ,  NO ;

: FIND?  ( -- addr \ nc | -- string \ no )  BL WORD  HERE latest SEARCH? ;
: ?FIND  ( -- addr )  FIND? 0= ?ERROR ;
: LITERAL  ( n -- )  compile @ if ' DUP COMPILE  
  [ 	90 c,		 C, , then ; IMMEDIATE ( MOV DPTR, number )
: '  ( -- pfa )  ?FIND  ' LITERAL EXECUTE ; IMMEDIATE
: INTERPRET  ( -- )
: PROMPT  ( -- )  out @ if CR then compile @ if ."   : " ELSE ." osu: " then ;
: PREPARE  ( key -- key )  DUP 1B <
  [ 	if c,		 DUP D = if DROP 0 0 INPUT C! ELSE
      DUP 8 = if in @ ?DUP  if 1 - in !  ELSE DROP 7 then
	ELSE ,		 DROP 7 then then
	ELSE ,		 out @ 4E < if DUP INPUT C!+ +IN ELSE DROP 7 then
   then ;
: COLLECTOR  ( -- )
  [ 	KEY? ,		 if KEY PREPARE ?DUP
	if c,		 EMIT ELSE SPACE 0 in ! INTERPRET 0 in ! PROMPT then
        then  ' COLLECTOR >BARON ;

( ==== New control loop ==== )
: RESET-INPUT  ( -- )  keyq 0Q  0 in !  tib 2+ tib !
  [ 	keyq ,		 sioa-in ! emitq sioa-out !
   ' POLL-SIO KILL  ' COLLECTOR KILL  ' POLL-SIO >BARON  ' COLLECTOR >BARON  ;

: QUIT  ( -- )  RP!  R>DROP  ' QUIT >R  SP!  ' [ EXECUTE
: TOG  ( -- )  
  [ 	B290 ,		 ( CPL 90 )
	B291 ,		 ] ;	( CPL 91 )  

: LOAD  ( -- )  
  [ 	D290 ,		 ( SETB P1.0 )
	7F80 ,		 ( MOV R7, #80H )
	7E00 ,		 ( MOV R6, #0 )
	E0 c,		 ( MOVX A, @DPTR )
	C290 ,		 ( CLR P1.0 )
	F0 c,		 ( MOVX @DPTR, A )
	D290 ,		 ( SETB P1.0 )
	A3 c,		 ( INC DPTR )
	DEF7 ,		 ( DJNZ R6, F7H )
	DFF3 ,		 ] ;	( DJNZ R7, F3H ) 

: SAVE  ( -- )  
  [ 	C290 ,		 ( CLR P1.0 )
	7F80 ,		 ( MOV R7, #80H )
	7E00 ,		 ( MOV R6, #0 )
	E0 c,		 ( MOVX A, @DPTR )
	D290 ,		 ( SETB P1.0 )
	F0 c,		 ( MOVX @DPTR, A )
	C290 ,		 ( CLR P1.0 )
	A3 c,		 ( INC DPTR )
	DEF7 ,		 ( DJNZ R6, F7H )
	DFF3 ,		 ] ;	( DJNZ R7, F3H ) 

: MAP?  ( -- )  90 B?I ."   DATA = " if ." BBRAM" ELSE ." RAM" then
  [ 	91 c,		 B?I ." PROG = " if ." BBRAM" ELSE ." RAM" then ;

: INIT  ( -- )  
  [ 	7880 ,		 ( MOV R0, #80H )
	LOAD ,		 TOG QUIT ;

( ==== Conditionals ==== )
: (DO)  ( limit \ index -- )  SWAP  R>  SWAP >R  SWAP >R  >R ;
: (LOOP)  ( -- )  R>  R> 1+ DUP R <
  [ 	if c,		 >R @ ELSE R> 2DROP 2+ then >R ;
: (+LOOP)  ( n -- )  R> SWAP  DUP R> +  SWAP 0<  OVER R <  XOR
  [ 	if c,		 >R @ ELSE R> 2DROP 2+ then >R ;

: then  ( addr -- )  HERE SWAP ! ; IMMEDIATE
: ELSE  ( addr -- addr )  
  [ 	SWAP ,		 ' then EXECUTE ; IMMEDIATE
: THEN  ( addr -- )  ' then EXECUTE ; IMMEDIATE
: BEGIN  ( -- addr )   HERE ; IMMEDIATE
: WHILE  ( -- addr )  ' if EXECUTE ; IMMEDIATE
: AGAIN  ( addr -- )   
: REPEAT  ( addr \ addr -- )  SWAP  ' AGAIN EXECUTE  ' then EXECUTE ; IMMEDIATE
: UNTIL  ( addr -- )  
  [ 	E583 ,		 , ( MOV A, DPH )
	700A ,		 , ( JNZ AH )
	E582 ,		 , ( MOV A, DPL )
	7006 ,		 , ( JNZ 6H )
	' DROP COMPILE  
	2 C,  ,			( LJMP ?? )
	  ' DROP COMPILE ;  IMMEDIATE

: for  ( -- addr \ addr )  ' >R COMPILE  
  [ 	HERE ,		 ; IMMEDIATE
: (next)   ( addr -- )  R> R> DUP  if 1 - >R @  else DROP 2+  then  >R  ;
: next  ( addr \ addr -- )  HERE ROT ! ' (next) COMPILE  ,  ; IMMEDIATE
: DO  ( -- addr )  ' (DO) COMPILE  HERE ; IMMEDIATE
: LOOP  ( addr -- )  ' (LOOP) COMPILE   , ; IMMEDIATE
: +LOOP  ( addr -- )  ' (+LOOP) COMPILE  , ; IMMEDIATE
: LEAVE  ( -- )  R>  R>DROP  R >R  >R ;
     
===
