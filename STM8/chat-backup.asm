VERSION:
	.db #12
	.ascii "STM8EF2.2.29"

.ifdef W1209
; receive byte in A
RXA:
	BTJF USR_6,#0,RXA
	CLR A
	EXG A,TIM4RXBUF
	CLR USR_6		; clear rxa flag
	RET
.else
RXA:
	JP QRX
.endif

; receive byte
TOB:
	CLR USR_6
	CALLR RXA
	DECW X
	LD (X),A
	DECW X
	CLR (X)
	RET

;  receive word
TOW:
	CALLR TOB
	CALLR RXA
	LD (X),A
	RET

; send byte from A
ATO:
	LD A,(X)
	INCW X
	JP TXASTOR

; send word from tos
WTO:
	CALL EXG
	CALLR ATO
	JRA ATO


; send bytes from memory pointed to by m
SENDBYTES:
	CALLR TOB
	INCW X
	LDW Y,m
1$:
	LD A,(Y)
	CALL TXASTOR
	INCW Y
	DEC(X)
	JRNE 1$
	INCW X
	RET

SETADDR:
	CALLR TOW
	LDW Y,X
	LDW Y,(Y)
	LDW m,Y
	INCW X
	INCW X
	RET

GETSP:
	CALL SPAT
	JRA WTO

WRITEBS:
	CALLR TOB	; count
1$:	LDW Y,m		; memory pointer in Y
	CALLR RXA	; 
	LD (Y),A
	INCW Y
	LDW m,Y
	CLR USR_6
	LDW Y,X
	LDW Y,(Y)
	DECW Y
	LDW (X),Y
	JRNE 1$
	INCW X
	INCW X
	RET

SETSP:
	CALLR TOW
	LDW X,(X)
	RET

RUN:
	CALLR TOW
	LDW X,(X)
	CALLR TOW
	JP EXECU

VERS:
	DECW X
	DECW X
	LDW Y,#VERSION
	LDW (X),Y
	JRA WTO

FLASH:
	DoLitW TIBB
	CALL TOW
	CALL TOW
	CALL CMOVE
	LD A,#0xAB
	JP TXASTOR

TABLE:
	.dw VERS
	.dw SETADDR
	.dw SENDBYTES
	.dw WRITEBS
	.dw GETSP
	.dw SETSP
	.dw RUN
	.dw FLASH

lower=0xf
upper=0x18
offset=0x10

CHAT:
	CLR USR_6		; clear rxa flag
;	CALL RXA
	CLR A
	EXG A,TIM4RXBUF
	CP A,#lower
	JRMI 1$
	CP A,#upper
	JRSGT 1$
	SUB A,#offset
	SLL A
	ADD A,#TABLE
	LD YL,A
	CLR A
	ADC A,#>TABLE   ; MSB of TABLE
	LD YH,A
	LDW Y,(Y)
	JP (Y)
1$:
RETADDR:
	RET





