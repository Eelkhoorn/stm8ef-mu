hex
flash
__meta

: SWAP  ( n1 \ n2 -- n2 \ n1 )   
	C583 		( XCH A, DPH )
	C6 		( XCH A, @R0 )
	C583 		( XCH A, DPH )
	08 		( INC R0 )
	C582		( XCH A, DPL )
	C6 		( XCH A, @R0 )
	C582 		( XCH A, DPL )
	18 ;		( DEC R0 )
: DUP  ( n -- n \ n )   
	18 		( DEC R0 )
	A682 		( MOV @R0, DPL )
	18 		( DEC R0 )
	A683 ;		( MOV @R0, DPH )
: OVER  ( n1 \ n2 -- n1 \ n2 \ n1 )   
	A900 		( MOV R1, R0 )
	DUP  
	8783 		( MOV DPH, R1 )
	09		( INC R1 )
	8782 ;		( MOV DPL, R1 )
: DROP  ( n -- )   
	8683 		( MOV DPH, R0 )
	08 		( INC R0 )
	8682 		( MOV DPL, R0 )
	08 ;		( INC R0 )
: NIP  ( n1 \ n2 -- n2 )   
	08 		( INC R0 )
	08 ;		( INC R0 )

( ==== Memory primitives ==== )
: @   ( a -- n ) 
	E0 		( MOVX A, @DPTR )
	F9 		( MOV R1, A )
	A3 		( INC DPTR )
	E0 		( MOVX A, @DPTR )
	F582 		( MOV DPL, A )
	8983 ;		( MOV DPH, R1 )
: !   ( n \ a -- ) 
	E6 		( MOV A, @R0 )
	F0 		( MOVX @DPTR, A )
	08 		( INC R0 )
	A3 		( INC DPTR )
	E6 		( MOV A, @R0 )
	F0 		( MOVX @DPTR, A )
	08 		( INC R0 )
	DROP  ;
: C@  ( a -- c ) 
	E0 		( MOVX A, @DPTR )
	F582 		( MOV DPL, A )
	758300 ;	( MOV DPH, #0 )
: C!  ( c \ a -- ) 
	08 		( INC R0 )
	E6 		( MOV A, @R0 )
	F0 		( MOVX @DPTR, A )
	08 		( INC R0 )
	DROP  ;

( ==== Arithmetic operators ==== )
: +   ( n1 \ n2 -- n1-n2 )   
	86F0 		( MOV B, @R0 )
	08 		( INC R0 )
	E6 		( MOV A, @R0 )
	08 		( INC R0 )
	2582		( ADD A, DPL )
	F582 		( MOV DPL, A )
	E5F0 		( MOV A, B )
	3583 		( ADDC A, DPH )
	F583 ;		( MOV DPH, A )
: -   ( n1 \ n2 -- n1-n2 )   
	86F0 		( MOV B, @R0 )
	08 		( INC R0 )
	E6 		( MOV A, @R0 )
	08 		( INC R0 )
	C3 		( CLR C )
	9582 		( SUBB A, DPL )
	F582 		( MOV DPL, A )
	E5F0 		( MOV A, B )
	9583 		( SUBB A, DPH )
	F583 ;		( MOV DPH, A )
: NOT  ( n1 -- _n2 )   
	74FF 		( MOV A, #FFH )
	6283 		( XRL DPH, A )
	6282 ;		( XRL DPL, A )
: AND  ( n1 \ n2 -- n1.and.n2 )   
	E6 		( MOV A, @R0 )
	08 		( INC R0 )
	5283 		( ANL DPH, A )
	E6 		( MOV A, @R0 )
	08 		( INC R0 )
	5282 ;		( ANL DPL, A )
: OR  ( n1 \ n2 -- n1.or.n2 )  
	E6 		( MOV A, @R0 )
	08 		( INC R0 )
	4283 		( ORL DPH, A )
	E6 		( MOV A, @R0 )
	08 		( INC R0 )
	4282 ;		( ORL DPL, A )
: XOR  ( n1 \ n2 -- n1.xor.n2 )  
	E6 		( MOV A, @R0 )
	08 		( INC R0 )
	6283 		( XRL DPH, A )
	E6 		( MOV A, @R0 )
	08 		( INC R0 )
	6282 ;		( XRL DPL, A )
: 2*  ( n -- 2n* )   
	C3 		( CLR C )
	E582 		( MOV A, DPL )
	33 		( RLC A )
	F582 		( MOV DPL, A )
	E583 		( MOV A, DPH )
	33 		( RLC A )
	F583 ;		( MOV DPH, A )
: 2/  ( n -- n2/ )   
	E583 		( MOV A, DPH )
	A2E7 		( MOV C, ACC.7 )
	13 		( RRC A )
	F583 		( MOV DPH, A )
	E582 		( MOV A, DPL )
	13 		( RRC A )
	F582 ;		( MOV DPL, A )
: U2/  ( u -- u2/ )   
	E583 		( MOV A, DPH )
	C3 		( CLR C )
	13 		( RRC A )
	F583 		( MOV DPH, A )
	E582 		( MOV A, DPL )
	13 		( RRC A )
	F582 ;		( MOV DPL, A )
: NEGATE  ( n -- _n )   
	C3 		( CLR C )
	E4 		( CLR A )
	9582 		( SUBB A, DPL )
	F582 		( MOV DPL, A )
	E4 		( CLR A )
	9583 		( SUBB A, DPH )
	F583 ;		( MOV DPH, A )
: 0<  ( n -- flag )   
	E583 		( MOV A, DPH )
	20E704 		( JB ACC.7, 4 )
	900000 		( MOV DPTR, #0 )
	22 		( RET )
	90FFFF ;	( MOV DPTR, #FFFH )

( ==== Return stack primitives ==== )
: R>DROP  ( -- )   
	D002 		( POP R2 )
	D003 		( POP R3 )
	1581 		( DEC SP )
	1581 		( DEC SP )
	C003 		( PUSH R3 )
	C002 ;		( PUSH R2 )
: >R  ( a -- )   
	D002 		( POP R2 )
	D003 		( POP R3 )
	C082 		( PUSH DPL )
	C083 		( PUSH DPH )
	C003 		( PUSH R3 )
	C002 		( PUSH R2 )
	DROP  ;
: R>  ( -- a )   DUP  
	D002 		( POP R2 )
	D003 		( POP R3 )
	D083 		( POP DPH )
	D082 		( POP DPL )
	C003 		( PUSH R3 )
	C002 ;		( PUSH R2 )
: R  ( -- a )   DUP  
	1581 		( DEC SP )
	1581 		( DEC SP )
	D083 		( POP DPH )
	D082 		( POP DPL )
	0581 		( INC SP )
	0581 		( INC SP )
	0581 		( INC SP )
	0581 ;		( INC SP )
: I  ( -- a )   DUP  
	1581 		( DEC SP )
	1581 		( DEC SP )
	D083 		( POP DPH )
	D082 		( POP DPL )
	0581 		( INC SP )
	0581 		( INC SP )
	0581 		( INC SP )
	0581 ;		( INC SP )
: DUP>R  ( n -- n )   
	D002 		( POP R2 )
	D003 		( POP R3 )
	C082 		( PUSH DPL )
	C083 		( PUSH DPH )
	C003 		( POP R3 )
	C002 ;		( POP R2 )

: (VAR)  ( -- addr )  R>  ;
: (CONST)  ( -- val )  R>  @  ;

: (NEXT)   ( addr -- )  R> R> DUP  IF 1 - >R @  ELSE DROP 2+  ENDIF  >R  ;

( ==== Control flow primitives ==== )
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
: ?DUP  ( n -- [n \] n )  DUP  IF DUP ENDIF  ;

( ==== Internal Access ==== )
: (IRAM)  ( addr -- )  R 1+  C! ;
: (IRAMD)  ( addr -- space )  DUP  R 1+  C! ;
: (IRAM2)  ( c \ addr -- )  R 1+ C!  R 2+ C! ;

: (C@I)  ( addr -- c )  ' (IRAMD) COMPILE  
	8500 , 82 C, 		( MOV ?, DPL )
	7583 , 0 C, ;  IMMEDIATE	( MOV DPH, #0 )
: (C!I)  ( c \ addr -- )  ' (IRAM2) COMPILE  
	75 C, 0 , ;  IMMEDIATE		( MOV ?, ? )
: (BSETI)  ( bit addr -- )  ' (IRAM) COMPILE 
	D200 , ; IMMEDIATE		( SETB ? )
: (BCLRI)  ( bit addr -- )  ' (IRAM) COMPILE 
	C200 , ; IMMEDIATE		( CLR ? )
: (BCPLI)  ( bit addr -- )  ' (IRAM) COMPILE 
	B200 , ; IMMEDIATE		( CPL ? )
: (B?I)  ( bit addr -- flag )  ' (IRAMD) COMPILE 
	2000 , 04 C,		( JB ?, 4 )
	9000 , 00 C, 		( MOV DPTR, #0 )
	22 C, 			( RET )
	90FF , FF C, ; IMMEDIATE		( MOV DPTR, #FFFFH )

: C@I  ( addr -- c )  (C@I) ;
: C!I  ( c \ addr -- )  (C!I) ;
: BSETI  ( bit addr -- )  (BSETI) ;
: BCLRI  ( bit addr -- )  (BCLRI) ;
: BCPLI  ( bit addr -- )  (BCPLI) ;
: B?I  ( bit addr -- flag )  (B?I) ;

( ==== Comparison ==== )
 0 CONSTANT NO
-1 CONSTANT YES

: <  ( n \ m -- flag )  2DUP XOR 0< IF DROP 0< EXIT ENDIF       - 0< ;
: >  ( n \ m -- flag )  2DUP XOR 0< IF  NIP 0< EXIT ENDIF  SWAP - 0< ;
: U<  ( n \ m -- flag )  
	E6 		( MOV A, @R0 )
	08 		( INC R0 )
	B58304 		( CJNE A, DPH, 4 )
	E6 		( MOV A, @R0 )
	B58200 		( CJNE A, DPL, 0 )
	08 		( INC R0 )
	4004 		( JC 4 )
	900000 		( MOV DPTR, #0 )
	22 		( RET )
	90FFFF ;	( MOV DPTR, #FFFFH )
: U>  ( n \ m -- flag )  SWAP  U<  ;
: 0=  ( n -- flag )       IF NO EXIT ENDIF YES ;
: =  ( n \ m -- flag )  - IF NO EXIT ENDIF YES ;

( ==== Memory Manipulation ==== )
20 CONSTANT BL

: COUNT  ( addr -- addr+ \ count )  C@+ SWAP ;
: CMOVE  ( src \ dest \ count -- )  FOR >R COUNT R> C!+ NEXT 2DROP ;
: <CMOVE  ( src \ dest \ count -- )  >R  SWAP R 1 - +  SWAP R 1 - +
	R> FOR >R  C@- SWAP  R> C!-  NEXT 2DROP ;
: MOVE  ( src \ dest \ count -- )  FOR >R @+ SWAP R> !+ NEXT 2DROP ;
: FILL  ( addr \ count \ char -- )
	SWAP >R SWAP R> FOR  2DUP C! 1+ NEXT  2DROP ;
: ERASE ( addr \ count -- )  0 FILL ;
: BLANKS ( addr \ count -- )  BL FILL ;

( ==== Memory Management ==== )
  origin# CONSTANT dp	  	  ( dictionary pointer )

: +!  ( n \ addr -- )  DUP>R @ + R> ! ;

: HERE  ( -- addr )  dp @ ;
: ALLOT  ( n -- )  dp +! ;

: ,  ( n -- )  HERE !  2 ALLOT ;
: C,  ( n -- )  HERE C!  1 ALLOT ;

: 1+   ( n1 \ 1+n1 )   
	A3 C,  ;  IMMEDIATE		( INC DPTR )
: 2+   ( n1 \ 2+n1 )   
	A3 C,  				( INC DPTR )
	A3 C,  ;  IMMEDIATE		( INC DPTR )

( ==== Header Status Bits ==== )
	   80 CONSTANT #nfa
	   40 CONSTANT #immediate
	   20 CONSTANT #smudge
  origin# 2 + CONSTANT latest

: +BITS  ( bits \ addr -- )  TUCK  C@ OR  SWAP C! ;
: -BITS  ( bits \ addr -- )  >R  NOT R C@ AND  R> C! ;

: LATEST ( -- nfa )  latest @ ;
: IMMEDIATE ( -- )  #immediate LATEST 2+ +BITS ;
: SMUDGE    ( -- )  #smudge    LATEST 2+ +BITS ;
: RECURSE   ( -- )  #smudge    LATEST 2+ -BITS ; IMMEDIATE

( ==== Arithmetic operators ==== )
: /MOD  ( n \ m -- r \ q )   
	7B00 		( MOV R3, #0 )
	7A00 		( MOV R2, #0 )
	7911 		( MOV R1, #11H )
	C3 		( CLR C )
	8014 		( SJMP 14H )
	AD03 		( MOV R5, R3 )
	AC02 		( MOV R4, R2 )
	C3 		( CLR C )
	EA 		( MOV A, R2 )
	9582 		( SUBB A, DPL )
	FA 		( MOV R2, A )
	EB 		( MOV A, R3 )
	9583 		( SUBB A, DPH )
	FB 		( MOV R3, A )
	5004 		( JNC 4 )
	AB05 		( MOV R3, R5 )
	AA04 		( MOV R2, R4 )
	B3 		( CPL C )
	08 		( INC R0 )
	E6 		( MOV A, @R0 )
	33 		( RLC A )
	F6 		( MOV @R0, A )
	18 		( DEC R0 )
	E6 		( MOV A, @R0 )
	33 		( RLC A )
	F6  		( MOV @R0, A )
	9200 		( MOV 0, C )
	D902 		( DJNZ R1, 2)
	800A 		( SJMP 0AH )
	A200 		( MOV C, 0 )
	EA 		( MOV A, R2 )
	33 		( RLC A )
	FA 		( MOV R2, A )
	EB 		( MOV A, R3 )
	33 		( RLC A )
	FB 		( MOV R3, A )
	80D4 		( SJMP D4H )
	8683 		( MOV DPH, @R0 )
	08 		( INC R0 )
	8682 		( MOV DPL, @R0 )
	A602 		( MOV @R0, R2 )
	18 		( DEC R0 )
	A603 ;		( MOV @R0, R3 )
: /  ( n \ m -- quot )  /MOD  NIP ;
: MOD  ( n \ m -- rem )  /MOD  DROP ;

: U*  ( n \ m -- nm* )   
	E583 		( MOV A, DPH )
	08 		( INC R0 )
	86F0 		( MOV B, @R0 )
	A4 		( MUL A, B )
	F583 		( MOV DPH, A )
	E582 		( MOV A, DPL )
	18 		( DEC R0 )
	86F0 		( MOV B, @R0 )
	A4 		( MUL A, B )
	2583 		( ADD A, DPH )
	F583 		( MOV DPH, A )
	E582 		( MOV A, DPL )
	08 		( INC R0 )
	86F0 		( MOV B, @R0 )
	A4 		( MUL A, B )
	F582 		( MOV DPL, A )
	E5F0 		( MOV A, B )
	2583 		( ADD A, DPH )
	F583 		( MOV DPH, A )
	08 ;		( INC R0 )

: ABS  ( n -- n )  DUP 0< IF NEGATE EXIT ENDIF ;
: MAX  ( n \ m -- p )  2DUP < IF NIP EXIT ENDIF DROP ;
: MIN  ( n \ m -- p )  2DUP > IF NIP EXIT ENDIF DROP ;

( ==== State ==== )
 0 VARIABLE compile
C0 CONSTANT #compile	( #nfa #immediate OR )

: ]  ( -- )  #compile compile ! ;
: [  ( -- )  0 compile ! ; IMMEDIATE

( ==== BARON Tasker ==== )
 40 QUEUE peasantq	( maximum of 40H peasants )

: >Q  ( n \ queue -- )    DUP>R  @  !-  DUP 4 - R - IF R> ! EXIT ENDIF  @ R> ! ;
: Q>  ( queue -- n )  2+ DUP>R  @  @-  DUP 2 - R - IF R> ! EXIT ENDIF  @ R> ! ;
: Q  ( queue -- n )  2+ @ @ ;

: 0Q  ( queue -- )  @+ ! ;
: Q?  ( queue -- n )
   @+ @+ >R  SWAP -  DUP 0< IF  R @  R> -  + 2/ EXIT ENDIF  R>DROP  2/ ;

: INLINE_NOP  ( -- )  ;
: INLINE  ( inst -- )  R 1+ ! ;
: BARON  ( -- )  peasantq Q? IF peasantq Q> INLINE INLINE_NOP ENDIF  ;

: >BARON  ( cfa -- )  peasantq >Q ;
: KILL  ( cfa -- )  peasantq  DUP Q?
   FOR  DUP>R Q>  2DUP - IF R >Q  ELSE DROP ENDIF  R>  NEXT  2DROP ;

( ==== Command port primitives ==== )
: RX?  ( -- flag )
	DUP  
	209804 		( JB RI, 4 )
	900000 		( MOV DPTR, #0 )
	22 		( RET )
	90FFFF ;	( MOV DPTR, #FFFFH )
: TX?  ( -- flag )
	DUP  
	209904 		( JB TI, 4 )
	900000 		( MOV DPTR, #0 )
	22 		( RET )
	90FFFF ;	( MOV DPTR, #FFFFH )
: TX  ( char -- )
	C299 		( CLR TI )
	858299 		( MOV SBUF, DPL )
	DROP  ;
: RX  ( -- char )
	DUP  
	859982 		( MOV DPL, SBUF )
	758300 		( MOV DPH, #0 )
	C298 ;		( CLR RI )

( ==== Sio port servicing ==== )
0 VARIABLE sioa-in	( points to queue used to hold input for port a )
0 VARIABLE sioa-out	( points to queue used to hold output for port a )

: SIO-PORTA  ( -- )  RX? IF RX sioa-in @ >Q ENDIF
   sioa-out @ Q? IF  TX? IF  sioa-out @ Q>  TX  ENDIF ENDIF ;
: POLL-SIO  ( -- )  SIO-PORTA  ' POLL-SIO >BARON ;

( ==== Terminal Output ==== )
  0 VARIABLE out
100 QUEUE emitq  ( 256 enough for one string )

: ?WAIT  ( -- )  BEGIN emitq Q? 100 = WHILE BARON  REPEAT ;
: EMIT  ( char -- )  >R  ?WAIT  out @
	R A -  IF  R 8 =  IF  1 -  ELSE  R D =  IF  DROP 0  ELSE  1+  ENDIF
	ENDIF  ENDIF  out !  R> emitq >Q ;

: CR  ( -- )  D EMIT  A EMIT ;
: SPACE ( -- )  BL EMIT ;
: SPACES  ( n -- )  0 MAX  FOR SPACE NEXT ;
: TYPE ( addr \ count -- )  FOR  C@+  SWAP EMIT  NEXT  DROP ;

( ==== Numerical Output ==== )
10 VARIABLE base

: HEX  ( -- )  10 base ! ;
: BIN  ( -- )  2 base ! ;
: DECIMAL  ( -- )  A base ! ;

: PAD ( -- addr )  HERE 50 + ;
: HOLD  ( char -- )  -1 PAD +!  PAD @ C! ;

: <#  ( -- )  PAD DUP ! ;
: #>  ( n -- addr \ count )  DROP PAD @ PAD OVER - ;
: SIGN  ( m \ n -- n )  SWAP 0< IF 2D HOLD ENDIF ;
: #  ( n -- n )  base @ /MOD  SWAP  9 OVER <  IF 7 + ENDIF  30 +  HOLD ;
: #S  ( n -- n )  BEGIN # DUP 0= UNTIL ;

: DEPTH  ( -- )  0 C@I 7E SWAP - 2/ ;
: .R  ( n \ m -- )  >R  <# base @ A = IF DUP ABS #S SIGN ELSE #S ENDIF #>
   R> OVER - SPACES TYPE ;
: .  ( n -- )  DEPTH IF 0 .R SPACE ENDIF ;

( ==== Terminal Input ==== )
100 QUEUE keyq   	     ( 256 enough for one string )

: KEY?  ( -- flag )  keyq Q? ;
: KEY   ( -- char )  BEGIN  KEY? 0=  WHILE  BARON  REPEAT  keyq Q> ;
 
( ==== Parser ==== )
  0 VARIABLE tib  52 TALLOT  ( tib points to terminal input buffer )
  0 VARIABLE in		     ( index into TIB )

: INPUT  ( -- addr )  tib @ in @ + ;
: +IN  ( addr -- )  INPUT - in +! ;

: SKIP  ( char -- )  >R  INPUT BEGIN COUNT  DUP R -  SWAP 0=  OR UNTIL  1 - +IN
   R>DROP ;
: SCAN  ( char -- )  >R  INPUT BEGIN COUNT  DUP R =  SWAP 0=  OR UNTIL  R>DROP
   DUP 1 - C@ 0= +  +IN ;

: +CHAR  ( -- )  INPUT C@+  SWAP IF +IN  ELSE DROP ENDIF ;
: PARSE  ( char -- )  >R  INPUT  HERE 1+
   BEGIN OVER C@  DUP 0=  SWAP R =  OR NOT  WHILE
     >R  C@+ SWAP  R> C!+  ( copy from input to here )
   REPEAT  R>DROP  HERE 1+ -  HERE C!  +IN  +CHAR ;

: WORD  ( char -- )  DUP SKIP PARSE ;
: (  ( -- )  29 SCAN ; IMMEDIATE
: COMPILE  ( addr -- )  
	12 C,  ,  ;	( LCALL addr )

( ==== Strings ==== )
: QUOTE  ( -- )  22 PARSE  HERE C@ 1+ ALLOT  ;
: (")  ( -- addr )  R>  DUP  C@+ + >R ;
: "  ( -- )  ' (") COMPILE  QUOTE ; IMMEDIATE
: (.")  ( -- )  R>  C@+  2DUP + >R  SWAP TYPE ;
: ."  ( -- )  compile @ IF ' (.") COMPILE  QUOTE
   ELSE 22 PARSE  HERE COUNT TYPE  ENDIF ; IMMEDIATE

( ==== Errors ==== )
: SP!  ( ? -- )  
	7880 ,  ;  IMMEDIATE		( MOV R0, #80H )
: RP!  ( -- )  
	7581 , 09 C,  ; IMMEDIATE	( MOV SP, #9 )

: ABORT  ( -- )  RP!  ;
: ERROR  ( -- )  HERE COUNT TYPE  ." <- bakarashii" ABORT ;
: ?ERROR  ( flag -- )  IF ERROR ENDIF ;

( ==== Number Conversion ==== )
: DIGIT  ( char -- n \ flag )
   30 -  DUP 9 >  IF 7 -  DUP A <  OR ENDIF  DUP base @ U< ;
: NUMBER  ( string -- n )  COUNT >R  COUNT 2D = TUCK
   IF R> 1 - >R  ELSE 1 -  ENDIF  0 SWAP
   R> FOR C@+ >R  DIGIT NOT ?ERROR  SWAP base @ U* +  R> NEXT DROP
   SWAP IF NEGATE ENDIF ;
: XQUOTE  ( -- )  22 WORD  HERE C@+ SWAP 2/ DUP C,
    FOR C@+ SWAP DIGIT DROP 10 U* SWAP C@+ SWAP DIGIT DROP ROT OR C, NEXT DROP ;
: X"  ( -- )  ' (") COMPILE  XQUOTE ;  IMMEDIATE
: T"  ( -- )  20 WORD  HERE C@+ SWAP 2/
    FOR C@+ SWAP DIGIT DROP 10 U* SWAP C@+ SWAP DIGIT DROP ROT OR C, NEXT DROP ;

( ==== Dictionary Searching ==== )
: C>LINK  ( code -- link )  BEGIN 1 - DUP C@ #nfa AND UNTIL  2 - ;
: L>CODE  ( link -- code )  2+  COUNT 1F AND +  ;

: COMPARE  ( string \ name -- flag )  C@+ >R 3F AND >R  COUNT DUP R> =
   IF  BEGIN ?DUP WHILE SWAP 
	 COUNT  R> C@+ >R  - IF 2DROP R>DROP NO EXIT ENDIF
       SWAP 1 - REPEAT YES
   ELSE DROP NO ENDIF NIP  R>DROP ;
: SEARCH?  ( string \ >list -- addr \ nc | -- string \ no )  
   BEGIN  @ ?DUP WHILE  2DUP 2+ COMPARE
     IF NIP  DUP L>CODE  SWAP 2+ C@  EXIT ENDIF
   REPEAT NO ;

: FIND?  ( -- addr \ nc | -- string \ no )  BL WORD  HERE latest SEARCH? ;
: ?FIND  ( -- addr )  FIND? 0= ?ERROR ;

( ==== Interpreter ==== )

: LITERAL  ( n -- )  compile @ IF ' DUP COMPILE  
	90 C, , ENDIF ; IMMEDIATE	( MOV DPTR, number )
: '  ( -- pfa )  ?FIND  ' LITERAL EXECUTE ; IMMEDIATE

: INTERPRET  ( -- )
   BEGIN
       BEGIN FIND? ?DUP WHILE
         compile @ TUCK  AND = IF EXECUTE  ELSE COMPILE ENDIF 
       REPEAT
   DUP C@ WHILE  NUMBER ' LITERAL EXECUTE  REPEAT  DROP ;

( ==== Key collector ==== )
: PROMPT  ( -- )  out @ IF CR ENDIF compile @ IF ."   : " ELSE ." osu: " ENDIF ;
: PREPARE  ( key -- key )  DUP 1B <
   IF DUP D = IF DROP 0  0 INPUT C!  ELSE
      DUP 8 = IF in @ ?DUP  IF 1 - in !  ELSE DROP 7 ENDIF
              ELSE DROP 7  ENDIF ENDIF
   ELSE  out @ 4E <  IF DUP INPUT C!+ +IN  ELSE DROP 7 ENDIF
   ENDIF ;
: COLLECTOR  ( -- )
   KEY? IF KEY PREPARE  ?DUP
           IF EMIT  ELSE  SPACE  0 in !  INTERPRET  0 in !  PROMPT  ENDIF
        ENDIF  ' COLLECTOR >BARON ;

( ==== New control loop ==== )
: RESET-INPUT  ( -- )  keyq 0Q  0 in !  tib 2+ tib !
   keyq sioa-in !  emitq sioa-out !
   ' POLL-SIO KILL  ' COLLECTOR KILL  ' POLL-SIO >BARON  ' COLLECTOR >BARON  ;

: QUIT  ( -- )  RP!  R>DROP  ' QUIT >R  SP!  ' [ EXECUTE
   RESET-INPUT  CR PROMPT  BEGIN BARON AGAIN ;

: TOG  ( -- )  
	B290 		( CPL 90 )
	B291 ;		( CPL 91 )

: LOAD  ( -- )  
	900000		( MOV DPTR, #0 )
	D290 		( SETB P1.0 )
	7F80 		( MOV R7, #80H )
	7E00 		( MOV R6, #0 )
	E0 		( MOVX A, @DPTR )
	C290 		( CLR P1.0 )
	F0 		( MOVX @DPTR, A )
	D290 		( SETB P1.0 )
	A3 		( INC DPTR )
	DEF7 		( DJNZ R6, F7H )
	DFF3 ;		( DJNZ R7, F3H )

: SAVE  ( -- )  
	900000		( MOV DPTR, #0 )
	C290 		( CLR P1.0 )
	7F80 		( MOV R7, #80H )
	7E00 		( MOV R6, #0 )
	E0 		( MOVX A, @DPTR )
	D290 		( SETB P1.0 )
	F0 		( MOVX @DPTR, A )
	C290 		( CLR P1.0 )
	A3 		( INC DPTR )
	DEF7 		( DJNZ R6, F7H )
	DFF3 ;		( DJNZ R7, F3H )

: MAP?  ( -- )  90 B?I ."   DATA = " IF ." BBRAM" ELSE ." RAM" ENDIF
		91 B?I ."   PROG = " IF ." BBRAM" ELSE ." RAM" ENDIF ;

: INIT  ( -- )  
	758DFD 		( MOV TH1, #FDH )
	758845 		( MOV TCON, #45H )
	759850 		( MOV SCON, #50H )
	758920 		( MOV TMOD, #20H )
	759907 		( MOV SP, #7 )
	7880 		( MOV R0, #80H )
	LOAD TOG  QUIT  ;

( ==== Conditionals ==== )
: (DO)  ( limit \ index -- )  SWAP  R>  SWAP >R  SWAP >R  >R ;
: (LOOP)  ( -- )  R>  R> 1+ DUP R <
   IF >R  @  ELSE R> 2DROP  2+ ENDIF  >R ;
: (+LOOP)  ( n -- )  R> SWAP  DUP R> +  SWAP 0<  OVER R <  XOR
   IF >R  @  ELSE R> 2DROP  2+ ENDIF  >R ;

: IF  ( -- addr )  
	E583 ,  		( MOV A, DPH )
	700A ,  		( JNZ AH )
	E582 ,  		( MOV A, DPL )
	7006 ,  ' DROP COMPILE  		( JNZ 6H )
	2 C,  HERE 0 ,  ' DROP COMPILE ;  IMMEDIATE	( LJMP ?? )
: ENDIF  ( addr -- )  HERE SWAP ! ; IMMEDIATE
: ELSE  ( addr -- addr )  
	2 C, HERE 0 , 		( LJMP ?? )
	SWAP ' ENDIF EXECUTE ; IMMEDIATE
: THEN  ( addr -- )  ' ENDIF EXECUTE ; IMMEDIATE

: BEGIN  ( -- addr )   HERE ; IMMEDIATE
: WHILE  ( -- addr )  ' IF EXECUTE ; IMMEDIATE
: AGAIN  ( addr -- )   
	2 C,  ,  ; IMMEDIATE		( LJMP ?? )
: REPEAT  ( addr \ addr -- )  SWAP  ' AGAIN EXECUTE  ' ENDIF EXECUTE ; IMMEDIATE
: UNTIL  ( addr -- )  
	E583 ,  		( MOV A, DPH )
	700A ,  		( JNZ AH )
	E582 ,  		( MOV A, DPL )
	7006 ,  		( JNZ 6H )
	' DROP COMPILE  
	2 C,  ,			( LJMP ?? )
	  ' DROP COMPILE ;  IMMEDIATE

: FOR  ( -- addr \ addr )  ' >R COMPILE  
	2 C, HERE 0 ,  		( LJMP ?? )
	HERE ; IMMEDIATE
: NEXT  ( addr \ addr -- )  HERE ROT ! ' (NEXT) COMPILE  ,  ; IMMEDIATE

: DO  ( -- addr )  ' (DO) COMPILE  HERE ; IMMEDIATE
: LOOP  ( addr -- )  ' (LOOP) COMPILE   , ; IMMEDIATE
: +LOOP  ( addr -- )  ' (+LOOP) COMPILE  , ; IMMEDIATE
: LEAVE  ( -- )  R>  R>DROP  R >R  >R ;

: CASE  ( -- )  0 ; IMMEDIATE
: {  ( -- )  ' OVER COMPILE  ' = COMPILE  ' IF EXECUTE  
	' DROP COMPILE ; IMMEDIATE
: }  ( -- )  ' ELSE EXECUTE ; IMMEDIATE
: ENDCASE  ( -- )  ' DROP COMPILE BEGIN ?DUP WHILE ' ENDIF EXECUTE 
	REPEAT ; IMMEDIATE

( ==== Defining Words ==== )
: EXIT  
	22 C,  ; IMMEDIATE	( RET )
: FORGET ( -- )  ?FIND C>LINK  DUP @ latest !  HERE - ALLOT ;
: ?UNIQUE  ( -- )  FIND? IF HERE COUNT TYPE ."  is not unique.  "
   ELSE  DUP C@ 0= ?ERROR  ENDIF  DROP ;
: CREATE  ( -- )  HERE  latest @ ,  ?UNIQUE  latest !
   HERE DUP C@  DUP 1+ ALLOT #nfa OR SWAP C! ;

: <BUILDS  ( -- )  CREATE ' (VAR) COMPILE ;
: DOES  ( -- )  R>  LATEST L>CODE 1+ ! ;
: DOES>  ( -- addr )  ' DOES COMPILE  ' R> COMPILE ; IMMEDIATE

: VARIABLE  ( val -- )  CREATE  ' (VAR) COMPILE , ;
: CONSTANT  ( val -- )  CREATE  ' (CONST) COMPILE , ;
: TABLE  ( -- )  <BUILDS   T" ;

: :  ( -- )  CREATE SMUDGE  ] ;
: ;  ( -- )  ' EXIT EXECUTE   ' [ EXECUTE  ' RECURSE EXECUTE  ; IMMEDIATE

: QUEUE  ( #words -- ) ( Queues: | >insert | >remove | >end | queue... | )
   <BUILDS  HERE 6 + ,  HERE 4 + ,  1+ 2*  DUP HERE + ,  ALLOT ;

( ==== File Loader: FF emitted to request a line of input ==== )
: INPUT-LINE  ( -- )  FF EMIT  ( signal for input )
   0 in !  INPUT  BEGIN KEY DUP D - WHILE SWAP C!+ REPEAT DROP  0 SWAP C! ;
: LD  ( -- )  BEGIN  INPUT-LINE INTERPRET AGAIN ;

( ==== Version ==== )
: VERSION  ( -- )  ." 51-Forth V0.1 " ;

( ==== End of kernel ==== )

( ==== Tools ==== )
0 VARIABLE blatest    ( latest branch address )
: IN-WHAT  ( addr -- lfa )  latest @ BEGIN 2DUP < WHILE @ REPEAT NIP ;
: .NAME  ( lfa -- )  2+ COUNT 1F AND TYPE 2 SPACES ;
: .WORD  ( addr -- addr+ )  @+ SWAP DUP ' (.")   = IF DROP 2E EMIT 22 EMIT C@+
					2DUP SWAP TYPE 22 EMIT 2 SPACES +
			       ELSE DUP ' (")    = IF DROP 22 EMIT C@+
					2DUP SWAP TYPE 22 EMIT 2 SPACES +
			       ELSE DUP ' (NEXT) = IF DROP ." NEXT  " 2+
			       ELSE IN-WHAT DUP>R L>CODE ' DUP = OVER C@ 90 =
			       AND IF R>DROP 1+ @+ SWAP . EXIT ELSE R>
			       ENDIF .NAME  ENDIF ENDIF ENDIF ;
: ?DIR  ( addr -- addr+ )  @+ TUCK  OVER blatest ! 
	U< IF ." -- " ELSE ." ++ " ENDIF ;
: .CODE  ( n \ addr -- addr+ )  SWAP DUP  2 = IF DROP ." branch" ?DIR
				ELSE DUP E5 = IF DROP ." 0branch" B + ?DIR 3 +
				ELSE DUP 22 = IF DROP ." EXIT  "
				ELSE .  ENDIF ENDIF ENDIF ;
: SEE  ( -- )  0 blatest !  CR ." :"  ' ' EXECUTE HERE COUNT TYPE ."   ( ? )  " 
	BEGIN  C@+ OVER 22 - OVER blatest @ > NOT OR WHILE
		OVER 12 = IF NIP .WORD  ELSE .CODE  ENDIF
	REPEAT  2DROP ." ;" ;
: WORDS  ( -- )  CR LATEST BEGIN DUP .NAME @ DUP 0= UNTIL DROP ;
: WHERE  ( addr -- )  IN-WHAT .NAME ;

: .S  ( -- )  DEPTH  DUP 0= IF ." Stack Empty !" DROP EXIT ENDIF
	DUP 6 > IF CR . ." items" 6 ENDIF DUP
	BEGIN DUP WHILE ROT >R 1 - REPEAT DROP ." ... "
	BEGIN DUP WHILE R> DUP . SWAP 1 - REPEAT DROP ;

: .H  ( n -- )  base @ SWAP HEX <# BL HOLD # # # # #> TYPE base ! ;
: C.H  ( b -- )  base @ SWAP HEX <# BL HOLD # # #> TYPE base ! ;
: ?ASCII  ( b -- b )  BL OVER > OVER 7E > OR IF DROP 2E ENDIF ;
: DUMP  ( addr \ n -- )  DUP IF 1 - 10 / ENDIF 1+
	FOR CR DUP .H ." : " DUP 10 FOR C@+ SWAP C.H         NEXT
		DROP 2 SPACES    10 FOR C@+ SWAP ?ASCII EMIT NEXT
	NEXT DROP  ;

----------------------------------------------------------


