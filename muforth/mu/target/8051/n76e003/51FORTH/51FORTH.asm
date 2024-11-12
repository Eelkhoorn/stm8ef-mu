Date: Tue, 7 Nov 89 14:10:09 MST
From: scott%idacom%watmath@uunet.uu.NET
To: G.Moretti@massey.ac.nz
Status: RO


Dear Giovanni					7 Nov 89

	The following is what I have been able to put together sofar. It is
not yet complete but I thought I would send a copy to you anyway. If you
have questions please let me know, however I will be away from 10 Nov until
19 Nov. I will try my best to answer any questions you have. 

					Scott Gehmlich
					scott@idacom.uucp


----------------------------------------------------


                                   51-FORTH
                                Scott Gehmlich
                               07 November 1989

       80
          -------------------
  PSP --> |   H    |   L    | 7F
          |        |        |
          |        v        |
          |    grows down   |
          |                 |
          |                 |
          |                 |
          |                 |
          |                 |
          |    grows up     |
          |        ^        |
       0A |   L    |   H    | <-- RSP
       08 | QUIT(L)| QUIT(H)| 09
          -------------------
          |   R6   |   R7   | 07
          |   R4   |   R5   |
          |   R2   |   R3   |
       00 |   R0   |   R1   | 01
          -------------------
R1-R7 - Temporary Storage Registers/Counters
R0 - Parameter Stack Pointer

Return  Stack Pointer ( RSP ) is initialized to 09 and points to the high byte
    of  the  top item of the return stack.  Low byte is pushed first.  RSP  is
    pre-incremented.   The  word 'QUIT' puts its own address at the bottom  of
    the return stack.

Parameter  Stack  Pointer ( PSP ) is initialized to 80 and points to the  high
    byte  of  the 2nd item on the parameter stack.  The top item is stored  in
    the DPTR.  Low byte is pushed first.  PSP is pre-decremented.

The kernel  is  started  compiling at 3H saving the first 3 bytes for  a  LJMP
    INIT.   No space is reserved for interrupt area, the present FORTH  kernel
    uses polling rather than interrupts.

The dictionary is setup as follows:  
| dp | latest | word | word | ...

dp  -  dictionary  pointer:   points  to  the   next  available  spot  in  the
    dictionary.   16  bits 

latest  - latest word:  points to the link field of the last compiled word  in
    the dictionary.  16 bits

The word is setup as follows:
| link | name | code |

link  - points to the word compiled previous to this word.  Used for searching
    the dictionary.  16 bits

code - executable code.  x bytes terminated with a RET instruction.

The name consists of the following:
| control bits, name count | name characters |

control bits - 3 bits.

name count - 5 bits.

name characters - ascii character set, 8 bits per char.  512 char max.

Sub-routine  threading  is used.  i.e.  there is not an inner  interpreter  to
    decide  whether  the  next  address is a constant,  address,  etc.   Every
    address will have a LCALL before it.  This means that after the name field
    all the code is executable.

Example word:
: SWAP --- [ C583 , C6 C, C583 , 08 C, C582 , C6 C, C582 , 18 C, ] ;

compiled as: 0000 8453574150 C583C6C58308C582C6C58218 22
lfa - 0000  first word in dictionary, 0000 signifies the end.
name field - 8453574150
	control bits 100 - in the dictionary, non-immediate, ???
	name count 00100 - 4 characters
	name 53574150 - SWAP
code field - C583C6C58308C582C6C58218  executable, performs actual swap.
end - 22 return from subroutine


: C!+ --- SWAP OVER C! 1+ ;

compiled as: 0296 8343212B 12000E 12002F 12007E A3 22
lfa - 0296 points to previously compiled word ( !- )
name field - 8343212B
	control bits 100 - in the dictionary, non-immediate, ???
	name count 00100 - 4 characters
	name 43212B - C!+
code field - 12000E 12002F 12007E A3
	12000E - call subroutine SWAP; code field addr 000E
	12002F - call subroutine OVER; code field addr 002F
	12007E - call subroutine C!; code field addr 007E
	A3 - executable code for 1+ ( INC DPTR )
end - 22 return from subroutine

----------------------------------------------------------

51-FORTH Kernel
	- does not include the 8051 Assembler
	- words written in 8051 code are called 'primitives'

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

: ,  ( n -- )  HERE !  2 ALLOT ;
: C,  ( n -- )  HERE C!  1 ALLOT ;

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

: MAP?  ( -- )  90 B?I ."   DATA = " IF ." BBRAM" ELSE ." RAM" ENDIF
		91 B?I ."   PROG = " IF ." BBRAM" ELSE ." RAM" ENDIF ;


( ==== Conditionals ==== )
: CASE  ( -- )  0 ; IMMEDIATE
: {  ( -- )  ' OVER COMPILE  ' = COMPILE  ' IF EXECUTE  
	' DROP COMPILE ; IMMEDIATE
: }  ( -- )  ' ELSE EXECUTE ; IMMEDIATE
: ENDCASE  ( -- )  ' DROP COMPILE BEGIN ?DUP WHILE ' ENDIF EXECUTE 
	REPEAT ; IMMEDIATE

( ==== Defining Words ==== )
: CREATE  ( -- )  HERE  latest @ ,  ?UNIQUE  latest !
   HERE DUP C@  DUP 1+ ALLOT #nfa OR SWAP C! ;

: <BUILDS  ( -- )  CREATE ' (VAR) COMPILE ;
: DOES  ( -- )  R>  LATEST L>CODE 1+ ! ;
: DOES>  ( -- addr )  ' DOES COMPILE  ' R> COMPILE ; IMMEDIATE

: TABLE  ( -- )  <BUILDS   T" ;

: QUEUE  ( #words -- ) ( Queues: | >insert | >remove | >end | queue... | )
   <BUILDS  HERE 6 + ,  HERE 4 + ,  1+ 2*  DUP HERE + ,  ALLOT ;

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
 WHERE  ( addr -- )  IN-WHAT .NAME ;


: .H  ( n -- )  base @ SWAP HEX <# BL HOLD # # # # #> TYPE base ! ;
: C.H  ( b -- )  base @ SWAP HEX <# BL HOLD # # #> TYPE base ! ;
: ?ASCII  ( b -- b )  BL OVER > OVER 7E > OR IF DROP 2E ENDIF ;

----------------------------------------------------------


