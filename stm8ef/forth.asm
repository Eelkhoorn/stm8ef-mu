; STM8EF for STM8S (Value line and Access Line devices)
;
; This is derived work based on
; http://www.forth.org/svfig/kk/07-2010.html
;
; Please refer to LICENSE.md for more information.
;
;--------------------------------------------------------
; Original author, and copyright:
;       STM8EF, Version 2.1, 13jul10cht
;       Copyright (c) 2000
;       Dr. C. H. Ting
;       156 14th Avenue
;       San Mateo, CA 94402
;       (650) 571-7639
;
; Original main description:
;       FORTH Virtual Machine:
;       Subroutine threaded model
;       SP Return stack pointer
;       X Data stack pointer
;       A,Y Scratch pad registers
;
;--------------------------------------------------------
; The latest version of this code is available at
; https://github.com/TG9541/stm8ef
;
;
; Docs for the SDCC integrated assembler are scarce, thus
; SDCC was used to write the skeleton for this file.
; However, the code in this file isn't SDCC code.
;--------------------------------------------------------
; File Created by SDCC : free open source ANSI-C Compiler
; Version 3.6.0 #9615 (Linux)
;--------------------------------------------------------

        .module forth
        .optsdcc -mstm8

;--------------------------------------------------------
; Public variables in this module
;--------------------------------------------------------

;       .globl _TRAP_Handler
        .globl _forth

;--------------------------------------------------------
; ram data
;--------------------------------------------------------
        .area DATA

;--------------------------------------------------------
; ram data
;--------------------------------------------------------
        .area INITIALIZED
;--------------------------------------------------------
; absolute external ram data
;--------------------------------------------------------
        .area DABS (ABS)
;--------------------------------------------------------
; set RESET vector to COLD
;--------------------------------------------------------
         .org 0x8000
         .dw 0x8200
         .dw _forth
;--------------------------------------------------------
; restore broken interrupt vector table of STM8L
;--------------------------------------------------------
         .ifeq (FAMILY - STM8L)
          .org 0x8070
          .dw 0x8200
          .dw _forth
          .dw 0x8200
          .dw _forth
          .dw 0x8200
          .dw _forth
          .dw 0x8200
          .dw _forth
         .if   HAS_RXUART*HAS_TXUART
          .org 0x8008   + ITC_IX_USART1_RXD * 4
          .dw 0x8200
          .dw UART_INT
         .endif
         .endif
;--------------------------------------------------------
; restore broken interrupt vector table of STM8S
;--------------------------------------------------------
         .ifeq (FAMILY - STM8S)
          .org 0x8068
          .dw 0x8200
          .dw 0x0
         .if   HAS_RXUART*HAS_TXUART
          .org 0x8008   + ITC_IX_UART1RX * 4
          .dw 0x8200
          .dw UART_INT
         .endif
         .endif

;.area CODE

;--------------------------------------------------------
; global & static initialisations
;--------------------------------------------------------
;        .area HOME
;        .area GSINIT
;        .area GSFINAL
;        .area GSINIT
;--------------------------------------------------------
; Home
;--------------------------------------------------------
;        .area HOME
;        .area HOME
;--------------------------------------------------------
; code
;--------------------------------------------------------
;        .area CODE
        .org CODE_START

        ;************************************
        ;******  1) General Constants  ******
        ;************************************

        COMPO   =     0x40      ; "COMPO" lexicon compile only bit

        CELLL   =      2        ; size of a cell
        TIC     =     39        ; tick

        EXIT_OPC =    0x81      ; RET opcode
        BRAN_OPC =    0xCC      ; JP opcode
        CALL_OPC =    0xCD      ; CALL opcode

        ; Chip type (set of peripheral addresses and features)
        STM8S_LOD        = 103  ; STM8S Low Density
        STM8S_MED        = 105  ; STM8S Medium Density
        STM8S_HID        = 207  ; STM8S High Density
        STM8L_LOD        = 051  ; STM8L Low Density, RM0031 family
        STM8L_101        = 101  ; STM8L Low Density, RM0013 family
        STM8L_MHD        = 152  ; STM8L Medium and High Density

        ; STM8 family flags
        STM8S            = 0    ; FAMILY: STM8S device
        STM8L            = 1    ; FAMILY: STM8L device

        ; legacy chip type (deprecated - preferably use the chip type constants)
        STM8L101F3 = STM8L_101  ; L core, 8K flash incl EEPROM, 1.5K RAM, UART1
        STM8L051F3 = STM8L_LOD  ; L core, 8K flash, 1K RAM, 256 EEPROM, UART1
        STM8L151K4 = STM8L_MHD  ; L core, 32K flash, 2K RAM, 1K EEPROM, UART1
        STM8L152C6 = STM8L_MHD  ; L core, 32K flash, 2K RAM, 1K EEPROM, UART1
        STM8L152R8 = STM8L_MHD  ; L core, 64K flash, 4K RAM, 2K EEPROM, UART1
        STM8S003F3 = STM8S_LOD  ; 8K flash, 1K RAM, 128 EEPROM, UART1
        STM8S103F3 = STM8S_LOD  ; like STM8S003F3, 640 EEPROM
        STM8S105K4 = STM8S_MED  ; 16K/32K flash, 2K RAM, 1K EEPROM, UART2
        STM8S207RB = STM8S_HID  ; 32K+96K flash, 6K RAM, 2K EEPROM, UART1 or UART2

        DEFOSCFREQ     = 16000  ; default oscillator frequency in kHz (HSI)

        ;********************************************
        ;******  2) Device hardware addresses  ******
        ;********************************************

        ;******  STM8 memory addresses ******
        RAMBASE =       0x0000  ; STM8 RAM start

        ; STM8 device specific include (provided by file in board folder)
        ; sets "TARGET" and memory layout
        .include        "target.inc"

        ; STM8 Flash Block Size (depends on "TARGET")
        .ifeq   (TARGET - STM8S_LOD) * (TARGET - STM8L_101) * (TARGET - STM8L_LOD)
          PAGESIZE   =     0x40      ; "PAGESIZE" STM8 Low Density: 64 byte page size
        .else
          PAGESIZE   =     0x80      ; "PAGESIZE" STM8 M/H Density: 128 byte page size
        .endif

        ; STM8 family register addresses (depends on "TARGET")
        .ifeq   (TARGET - STM8S_LOD) * (TARGET - STM8S_MED) * (TARGET - STM8S_HID)
          FAMILY = STM8S
          .include  "stm8device.inc"
        .endif
        .ifeq   (TARGET - STM8L_101) * (TARGET - STM8L_LOD) * (TARGET - STM8L_MHD)
          FAMILY = STM8L
          .include  "stm8ldevice.inc"
        .endif


        ;**********************************
        ;******  3) Global defaults  ******
        ;**********************************
        ; Note: add defaults for new features here
        ;       and configure them in globconf.inc

        .include  "defconf.inc"

        ;********************************************
        ;******  4) Device dependent features  ******
        ;********************************************
        ; Define memory location for device dependent features here

        .include "globconf.inc"

        ; .include "linkopts.inc"

        ; console configuration: check if TX simulation has priority over UART
        .ifge   HAS_TXSIM - HAS_TXUART
        .ifeq  PNTX-PNRX
        CONSOLE_HALF_DUPLEX = 1 ; single wire RX/TX simulation is half duplex
        .else
        CONSOLE_HALF_DUPLEX = 0 ; RX/TX simulation supports full duplex
        .endif
        .else
        CONSOLE_HALF_DUPLEX = HALF_DUPLEX ; use hardware UART settings
        .endif

        OSCFREQ   = DEFOSCFREQ  ; "OSCFREQ" oscillator frequency in kHz
        CRAMLEN   = FORTHRAM    ; "CRAMLEN" RAM starting from 0 not used by Forth

        ;**************************************
        ;******  5) Board Driver Memory  ******
        ;**************************************
        ; Memory for board related code, e.g. interrupt routines

        RAMPOOL =    FORTHRAM   ; RAM for variables (growing up)

        .macro  RamByte varname
        varname = RAMPOOL
        RAMPOOL = RAMPOOL + 1
        .endm

        .macro  RamWord varname
        varname = RAMPOOL
        RAMPOOL = RAMPOOL + 2
        .endm

        .macro  RamBlck varname, size
        varname = RAMPOOL
        RAMPOOL = RAMPOOL + size
        .endm


        ;**************************************************
        ;******  6) General User & System Variables  ******
        ;**************************************************

        ; ****** Indirect variables for code in NVM *****
        .ifne   HAS_CPNVM
        ISPPSIZE  =     16      ; Size of data stack for interrupt tasks
        .else
        ISPPSIZE  =     0       ; no interrupt tasks without NVM
        .endif

        SPP   = ISPP-ISPPSIZE   ; "SPP"  data stack, growing down (with SPP-1 first)
        ISPP  = SPPLOC-BSPPSIZE
        BSPP  = SPPLOC          ; "BSPP" Background data stack, growing down
        RPP   = RPPLOC          ; "RPP"  constant addr. return stack, growing down

        ; Core variables (same order as 'BOOT initializer block)

;        USRRAMINIT = USREMIT

        RamWord USREMIT         ; "'EMIT" execution vector of EMIT
        RamWord USRQKEY         ; "'?KEY" execution vector of QKEY
.if  HAS_RXSIM
        RamByte USR_5           ; chat variables
        RamByte USR_6           ;
.endif
        RamWord MP              ; memory pointer for mu-chat

        ; More core variables in zero page (instead of assigning fixed addresses)
        RamWord USRHLD          ; "HLD" hold a pointer of output string
        RamByte XREG0           ; extra working register for core words
        RamByte XREG1           ; extra working register for core words
        RamByte XREG2           ; extra working register for core words
        RamByte XREG3           ; extra working register for core words
        RamWord BITAT           ; reserve space for BTJF
        RAMPOOL = RAMPOOL + 8
        RamWord BITSTO          ; reserve space for BSET/BRES
        RAMPOOL = RAMPOOL + 3

        ;***********************
        ;******  7) Code  ******
        ;***********************

;        ==============================================
;        Forth header macros
;        Macro support in SDCC's assembler "SDAS" has some quirks:
;          * strings with "," and ";" aren't allowed in parameters
;          * after include files, the first macro call may fail
;            unless it's preceded by unconditional code
;         ==============================================

        LINK =          0       ;

        .macro  HEADER Label wName
        .endm

        .macro  HEADFLG Label wName wFlag

        .endm

;       ==============================================
;               Low level code
;       ==============================================
;       Macro for inline literals using the TRAP approach
        .ifeq  USE_CALLDOLIT
        
        .macro DoLitW w
        TRAP
        .dw     w
        .endm
        
        .else
        
;       Macro for inline literals using the DOLIT approach

        .macro DoLitW w
        DECW X
        DECW X
        LDW Y,#w
        LDW (X),Y
        .endm
        
        .endif

; Alternative for DOXCODE
        .macro LDW_Y_CONTENT_X
        LDW Y,X
        LDW Y,(Y)		; tos in Y
        .endm
;	actual operation on Y
;       LDW (X),Y        

;       ==============================================
;               UART chat code
;       ==============================================

;       send byte from A 
        HEADER  RXA "RXA"
RXA:
.if HAS_RXUART
        BTJF    UART_SR,#5,RXA
        LD      A,UART_DR      ; get char in A
.else
        BTJF USR_6,#0,RXA
        LD A,TIM4RXBUF
        CLR USR_6		; clear rxa flag
.endif
        RET

; receive byte in tos 
        HEADER  TOB "TOB"
TOB:
.if HAS_RXSIM
        CLR USR_6
.endif
        CALLR RXA
        DECW X
        LD (X),A
        DECW X
        CLR (X)
        RET

; receive cell in tos 
        HEADER  TOW "TOW"
TOW:
        CALLR TOB
        CALLR RXA
        LD (X),A
        RET

; send byte from tos 
        HEADER  ATO "ATO"
ATO:
        LD A,(X)
        INCW X
        JP TXASTOR

; send cell from tos 
        HEADER  WTO "WTO"
WTO:
        CALL EXG
        CALLR ATO
        JRA ATO


; send bytes from memory pointed to by MP 
        HEADER  SENDBYTES "SENDBYTES"
SENDBYTES:
        CALLR TOB
        INCW X
        LDW Y,MP
1$:
        LD A,(Y)
        CALL TXASTOR
        INCW Y
        DEC(X)
        JRNE 1$
        INCW X
        RET

;       receive byte and store in memory pointer MP 
        HEADER  SETADDR "SETADDR"
SETADDR:
        CALLR TOW
        LDW Y,X
        LDW Y,(Y)
        LDW MP,Y
        INCW X
        INCW X
        RET

;       
        HEADER  GETSP "GETSP"
GETSP:
        CALL SPAT
        JRA WTO

;       
        HEADER  WRITEBS "WRITEBS"
WRITEBS:
        CALLR TOB	; count
1$:	LDW Y,MP		; memory pointer in Y
        CALLR RXA        ; 
        LD (Y),A
        INCW Y
        LDW MP,Y
.if HAS_RXSIM
        CLR USR_6
.endif
        LDW Y,X
        LDW Y,(Y)
        DECW Y
        LDW (X),Y
        JRNE 1$
        INCW X
        INCW X
        RET

        HEADER  SETSP "SETSP"
SETSP:
        CALLR TOW
        LDW X,(X)
        RET

        HEADER  RUN "RUN"
RUN:
        CALLR TOW
        LDW X,(X)
        CALLR TOW
        JP EXECU

        HEADER  FLASH "FLASH"
FLASH:
        DoLitW FLASHBUF_ADDR
        CALLR TOW
        CALLR TOW
        CALL CMOVE
        LD A,#0xAB
        JP TXASTOR

        HEADER  TABLE "TABLE"
TABLE:
        .dw SETADDR
        .dw SENDBYTES
        .dw WRITEBS
        .dw GETSP
        .dw SETSP
        .dw RUN
        .dw FLASH
NOP     ; for disaasembling purpose
lower=0xf
upper=0x18
offset=0x10

        HEADER  CHAT "CHAT"
CHAT:
.if HAS_RXSIM
        LD A,TIM4RXBUF
        CLR USR_6
.else
        CALL RXA
.endif
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
        RET
        
; ==============================================
;       Getbit and Setbit routines to be moved 
;       to ram during reset ( -- )
; ==============================================

        HEADER  COLD1 "COLD1"
COLD1:
        CLR A
        BTJF XREG0,#0,1$
        INC A
1$:     LD (1,X),A
        RET
        BSET 0x100,#0
        RET
; ==============================================

; ==============================================

;       Includes for board support code
;       Board I/O initialization and E/E mapping code
;       Hardware dependent words, e.g.  BKEY, OUT!
        .include "boardcore.inc"

;       ADC routines depending on STM8 family
        .include "stm8_adc.inc"

;       Generic board I/O: 7S-LED rendering, board key mapping
        .include "board_io.inc"

;       Simulate serial interface code
        .include "sser.inc"

;       Background Task: context switch with wakeup unit or timer
        .include "bgtask.inc"
; UPPLOC = RAMPOOL + 30  ; PAD in Background task, growing down, 32 bytes

; ==============================================
        HEADER  RETURN "RETURN"
RETURN: RET

;       Configuation table with shadow data for RESET

;       Main entry points and COLD start data


_forth:                         ; SDCC entry
;       Note: no return to main.c possible unless RAMEND equals SP,
;       and RPP init skipped

;       COLD    ( -- )
;       The hilevel cold start sequence.

        HEADER  COLD "COLD"
COLD:
        SIM                     ; disable interrupts
        MOV     CLK_CKDIVR,#0   ; Clock divider register

        LDW     X,#(RAMEND-FORTHRAM)
1$:     CLR     (FORTHRAM,X)
        DECW    X
        JRPL    1$

        LDW     X,#RPP          ; return stack, growing down
        LDW     SP,X            ; initialize return stack

        ; see "boardcore.inc")
        CALL    BOARDINIT       ; "PC_BOARDINIT" Board initialization

        BGTASK_Init             ; macro for init of BG task timer, refer to bgtask.inc

        .ifne   HAS_RXUART+HAS_TXUART
        ; Init RS232 communication port
        ; STM8S[01]003F3 init UART
        LDW     X,#CUARTBRR      ; "UARTBRR" def. $6803 / 9600 baud
        LDW     UART_BRR1,X
        .ifne   HAS_RXUART*HAS_TXUART
        MOV     UART_CR2,#0x2C  ; Use UART1 full duplex + RXNE interrupt
.ifeq (FAMILY - STM8S)
        MOV    ITC_SPR5,#0xCF   ; enable TIM2 interrupts while chatting
.else
        MOV    ITC_SPR8,#0xC   ; enable TIM2 interrupts while chatting
.endif
        .ifne   HALF_DUPLEX
        .ifeq   (FAMILY - STM8S)
        .ifeq   (HALF_DUPLEX - 1)
        ; STM8S UART1, UART4: pull-up for PD5 single-wire UART
        BRES    PD_DDR,#5       ; PD5 GPIO input high
        BSET    PD_CR1,#5       ; PD5 GPIO pull-up
        .endif
        .if HAS_RXSIM
        MOV    ITC_SPR6,#0x3F   ; enable interrupts while chatting
        .endif
        .ifeq   (HALF_DUPLEX - 2)
        ; STM8S903 type Low Density devices can re-map UART-TX to PA3
        LD      A,OPT2
        AND     A,#0x03
        CP      A,#0x03
        JREQ    $1
        ; pull-up for PD5 single-wire UART
        BRES    PD_DDR,#5       ; PD5 GPIO input high
        BSET    PD_CR1,#5       ; PD5 GPIO pull-up
        JRA     $2
$1:
        ; pull-up for PA3 single-wire UART
        BRES    PA_DDR,#3       ; PA3 GPIO input high
        BSET    PA_CR1,#3       ; PA3 GPIO pull-up
$2:
        .endif
        .endif
        MOV     UART_CR5,#0x08 ; UART1 Half-Duplex
        .endif
        .else
        .ifne   HAS_TXUART
        MOV     UART_CR2,#0x08  ; UART1 enable tx
        .endif
        .ifne   HAS_RXUART
        MOV     UART_CR2,#0x04  ; UART1 enable rx
        .endif
        .endif
        .endif

        SSER_Init               ; macro for init of simulated serial, refer to sser.inc

        Board_IO_Init           ; macro board_io initialization (7S-LED)

        LDW     X,#SPP          ; initialize data stack, TIB
        
        DECW X                  ; initialise get bit / set bit routines in ram
        DECW X  
        LDW Y,#COLD1
        LDW (X),Y
        DECW X  
        DECW X  
        LDW Y,#BITAT  
        LDW (X),Y
        DECW X  
        DECW X  
        LDW Y,#15
        LDW (X),Y
        CALL CMOVE


.if  HAS_RXSIM
MOV USR_5,#255
.endif

        ; Hardware initialization complete
        RIM                     ; enable interrupts

TBOOT:  CALL    RETURN       ; application boot, can be changed with ' appl 'BOOT flash!
SLEEP:     WFI
.if HAS_RXSIM
        TNZ USR_6
        JREQ $1
        CALL CHAT
$1:    JRA      SLEEP
.endif

.if    HAS_RXUART*HAS_TXUART       
        JRA      SLEEP
UART_INT:
        CALL CHAT                 ; during chat data SP is communicated by muforth
        LDW (#3,SP),X             ; X (data SP) is popped from return stack during IRET
        IRET
.endif

; ==============================================

;       Device dependent I/O

        .ifne   HAS_RXUART
;       ?RX     ( -- c T | F )  ( TOS STM8: -- Y,Z,N )
;       Return serial interface input char from and true, or false.

        HEADER  QRX "?RX"
QRX:
        CLR     A               ; A: flag false
        BTJF    UART_SR,#5,1$
        LD      A,UART_DR      ; get char in A
1$:
     JP      ASTOR          ; push char
        .endif

        .ifne   HAS_TXUART
;       TX!     ( c -- )
;       Send character c to the serial interface.

        HEADER  TXSTOR "TX!"
TXSTOR:
        INCW    X
        LD      A,(X)
        INCW    X

        HEADER  TXASTOR "TXA!"
TXASTOR:
        .ifne   HALF_DUPLEX
        ; HALF_DUPLEX with normal UART (e.g. wired-or Rx and Tx)
1$:     BTJF    UART_SR,#7,1$  ; loop until tdre
        BRES    UART_CR2,#2    ; disable rx
        LD      UART_DR,A      ; send A
2$:     BTJF    UART_SR,#6,2$  ; loop until tc
        BSET    UART_CR2,#2    ; enable rx
        .else                  ; not HALF_DUPLEX
1$:     BTJF    UART_SR,#7,1$  ; loop until tdre
        LD      UART_DR,A      ; send A
        .endif
        RET
        .endif

; ==============================================

;       Device independent I/O

;       ?KEY    ( -- c T | F )  ( TOS STM8: -- Y,Z,N )
;       Return input char and true, or false.
        HEADER  QKEY "?KEY"
QKEY:
        JP      [USRQKEY]

;       EMIT    ( c -- )
;       Send character c to output device.

        HEADER  EMIT "EMIT"
EMIT:
        JP      [USREMIT]

; ==============================================
; The kernel

;       ?branch ( f -- )
;       Branch if flag is zero.

        HEADFLG QBRAN "?branch" COMPO
QBRAN:
        LDW     Y,X
        INCW    X
        INCW    X
        LDW     Y,(Y)
        JREQ    BRAN
WSKIPRET:
        POPW    Y
        JP      (2,Y)


;       branch  ( -- )
;       Branch to an inline address.

        HEADFLG BRAN "branch" COMPO    ; NOALIAS
BRAN:
        POPW    Y
        LDW     Y,(Y)
        JP      (Y)


;       EXECUTE ( ca -- )
;       Execute word at ca.

        HEADER  EXECU "EXECUTE"
EXECU:
        LDW     Y,X
        INCW    X
        INCW    X
        LDW     Y,(Y)
        JP      (Y)

        .ifeq   BOOTSTRAP
;       2!      ( d a -- )      ( TOS STM8: -- Y,Z,N )
;       Store double integer to address a.

        HEADER  DSTOR "2!"
DSTOR:
        LDW Y,X
        LDW Y,(Y)
        LD A,(4,X)
        LD (Y),A
        LD A,(5,X)
        LD (1,Y),A
        LD A,(2,X)
        LD (2,Y),A
        LD A,(3,X)
        LD (3,Y),A
        ADDW X,#6
        RET
        .endif

;       2@      ( a -- d )
;       Fetch double integer from address a.

        HEADER  DAT "2@"
DAT:
        LDW Y,X
        LDW X,(X)
        LD A,(X)
        EXGW X,Y
        LD (X),A
        LD A,(1,Y)
        LD (1,X),A
        DECW X
        LD A,(3,Y)
        LD (X),A
        DECW X
        LD A,(2,Y)
        LD (X),A
        RET

;       2C!  ( n a -- )
;       Store word C-wise to 16 bit HW registers "MSB first"

        HEADER  DCSTOR "2C!"
DCSTOR:
        LDW     Y,X
        INCW    X
        INCW    X
        LDW     Y,(Y)
        LD      A,(X)
        LD      (Y),A           ; write MSB(n) to a
        INCW    X
        LD      A,(X)
        LD      (1,Y),A         ; write LSB(n) to a+1
        INCW    X
        RET

;       2C@  ( a -- n )
;       Fetch word C-wise from 16 bit HW config. registers "MSB first"

        HEADER  DCAT "2C@"
DCAT:
        LDW     Y,X
        LDW     X,(X)
        LD      A,(X)
        LD      (Y),A
        LD      A,(1,X)
        EXGW    X,Y
        LD      (1,X),A
        RET

;       BF@ ( a u -- 0|1)
;       Read bit #u (0..2047) in a cell array (16 bit words) at address a
;       Note: fills BITAT RAM-routine with stack values and jumps to BITAT
        HEADER  BFAT "BF@"
BFAT:
        LDW Y, X
        LDW X, (X)
        LD A,#8
        DIV X,A
        SLL A
        INC A
        LD BITAT+2,A
        LD A,XL
        XOR A,#01
        LD XREG0+1,A
        CLR XREG0
        LDW X,Y
        LDW X,(02,X)
        ADDW X,XREG0
        LDW BITAT+3,X
        EXGW X,Y
        INCW X
        INCW X
        JP BITAT

;       BF! ( a u -- 0|1)
;       Write bit to a bitfield stored in one or more cells (16 bit words)
;       Set/reset bit #u (0..8191) in a cell array starting at address a to bool t
        HEADER  BFSTO "BF!"
BFSTO:
        LDW Y,X
        LDW X,(X)
        LD A,XL
        AND A,#7
        PUSH A
        LD A,XL
        SRA A
        SRA A
        SRA A
        XOR A,#1
        LDW X,Y
        ADD A,(3,X)
        LD (3,X),A
        LD A,#0
        ADC A,(2,X)
        LD (2,X),A
        POP A
        CLR (X)
        LD (1,X),A         ; fall through

;       B! ( t a u -- )
;       Set/reset bit #u (0..7) in the byte at address a to bool t
;       Note: executes BSER/BRES + RET code in RAM
        HEADER  BRSS "B!"
BRSS:
        LD A,(1,X)
        SLL A                                     
        OR A,#16                                
        ld BITSTO+1,A                                  
        LD A,(05,X)                             
        JRNE 1$                       
        INC BITSTO+1                                   
1$:     LD A,(02,X)                             
        LD BITSTO+2,A                                  
        LD A,(03,X)                             
        LD BITSTO+3,A                                  
        ADDW X,#6                               
        JP BITSTO                                    

;       B@ ( a u -- )
;       Get bit #u (0..7) in the byte at address a
;       Note: executes BSER/BRES + RET code in RAM
        HEADER  BAT "B@"
BAT:
        LD A,(1,X)
        SLA A
        INC A
        LD BITAT+2,A
        LD A,(2,X)
        LD BITAT+3,A
        LD A,(3,X)
        LD BITAT+4,A
        INCW X
        INCW X
        CLR (X)
        JP BITAT

;       @       ( a -- w )      ( TOS STM8: -- Y,Z,N )
;       Push memory location to stack.

        HEADER  AT "@"
AT:
        LDW     Y,X
        LDW     X,(X)
        LDW     X,(X)
        EXGW    X,Y
        LDW     (X),Y
        RET

;       !       ( w a -- )      ( TOS STM8: -- Y,Z,N )
;       Pop data stack to memory.

        HEADER  STORE "!"
STORE:
        LDW     Y,X             ; (14 bytes, 16 cy)
        INCW    X
        INCW    X
        LDW     Y,(Y)
        PUSHW   X
        LDW     X,(X)           ; w
        LDW     (Y),X
        POPW    X
        INCW    X
        INCW    X
        RET

;       C@      ( a -- c )      ( TOS STM8: -- A,Z,N )
;       Push byte in memory to stack.
;       STM8: Z,N

        HEADER  CAT "C@"
CAT:
        LDW     Y,X             ; Y=a
        LDW     Y,(Y)
YCAT:
        LD      A,(Y)
        CLR     (X)
        LD      (1,X),A
        RET

;       C!      ( c a -- )
;       Pop     data stack to byte memory.

        HEADER  CSTOR "C!"
CSTOR:
        LDW     Y,X
        INCW    X
        INCW    X
        LDW     Y,(Y)
        INCW    X
        LD      A,(X)
        LD      (Y),A
        INCW    X
        RET

;       OVER    ( w1 w2 -- w1 w2 w1 ) ( TOS STM8: -- Y,Z,N )
;       Copy second stack item to top.

        HEADER  OVER "OVER"
OVER:
        LD A,(3,X)
        DECW X
        LD (X),A
        LD A,(3,X)
        DECW X
        LD (X),A
        RET

;       NIP     ( n1 n2 -- n2 )
;       Drop 2nd item on the stack.

        HEADER  NIP "NIP"
NIP:
        LD A,(X)  
        LD (2,X),A  
        LD A,(1,X)  
        LD (3,X),A  ; fall through

;       DROP     ( n1 -- )
;       Drop top stack item.

        HEADER  DROP "DROP"
DROP:   INCW    X
        INCW    X
        RET

;       DUP     ( w -- w w )    ( TOS STM8: -- Y,Z,N )
;       Duplicate top stack item.

        HEADER  DUPP "DUP"
DUPP:
        LDW     Y,X
        LDW     Y,(Y)
        DECW    X               ; SUBW  X,#2
        DECW    X
        LDW     (X),Y           ; push on stack
        RET

;       SWAP ( w1 w2 -- w2 w1 ) ( TOS STM8: -- Y,Z,N )
;       Exchange top two stack items.

        HEADER  SWAPP "SWAP"
SWAPP:
        LDW     Y,X
        LDW     X,(2,X)
        PUSHW   X
        LDW     X,Y
        LDW     X,(X)
        EXGW    X,Y
        LDW     (2,X),Y
        POPW    Y
        LDW     (X),Y
        RET

;       UM+     ( u u -- udsum )
;       Add two unsigned single
;       and return a double sum.

        HEADER  UPLUS "UM+"
UPLUS:
        CALLR   PLUS
        CLR     A
        RLC     A
        JP      ASTOR

;       +       ( w w -- sum ) ( TOS STM8: -- Y,Z,N )
;       Add top two items.

        HEADER  PLUS "+"

PLUS:
        LD      A,(1,X) ;D=w
        ADD     A,(3,X)
        LD      (3,X),A
        LD      A,(X)
        ADC     A,(2,X)
LDADROP:
        INCW    X
        INCW    X
        LD      (X),A
        RET

;       XOR     ( w w -- w )    ( TOS STM8: -- Y,Z,N )
;       Bitwise exclusive OR.

        HEADER  XORR "XOR"
XORR:
        LD      A,(1,X)         ; D=w
        XOR     A,(3,X)
        LD      (3,X),A
        LD      A,(X)
        XOR     A,(2,X)
        JRA     LDADROP

;       AND     ( w w -- w )    ( TOS STM8: -- Y,Z,N )
;       Bitwise AND.

        HEADER  ANDD "AND"
ANDD:
        LD      A,(1,X)         ; D=w
        AND     A,(3,X)
        LD      (3,X),A
        LD      A,(X)
        AND     A,(2,X)
        JRA     LDADROP

;       OR      ( w w -- w )    ( TOS STM8: -- immediate Y,Z,N )
;       Bitwise inclusive OR.

        HEADER  ORR "OR"
ORR:
        LD      A,(1,X)         ; D=w
        OR      A,(3,X)
        LD      (3,X),A
        LD      A,(X)
        OR      A,(2,X)
        JRA     LDADROP

;       0<      ( n -- t ) ( TOS STM8: -- A,Z )
;       Return true if n is negative.

        HEADER  ZLESS "0<"
ZLESS:
        CLR     A
        TNZ     (X)
        JRPL    ZL1
        CPL     A               ; true
ZL1:    LD      (X),A
        LD      (1,X),A
        RET

;       -   ( n1 n2 -- n1-n2 )  ( TOS STM8: -- Y,Z,N )
;       Subtraction.

        HEADER  SUBB "-"

SUBB:
        .ifeq   SPEEDOVERSIZE
        CALL    NEGAT           ; (15 cy)
        JRA     PLUS            ; 25 cy (15+10)
        .else
        LDW     Y,X
        LDW     Y,(Y)
        LDW     XREG0,Y
        INCW    X
        INCW    X
        LDW     Y,X
        LDW     Y,(Y)
        SUBW    Y,XREG0
        LDW     (X),Y
        RET                     ; 18 cy
        .endif

ZERO:
        CLR A

;       A>  ( -- n )     ( TOS STM8: - Y,Z,N )
;       push A to stack

        HEADER  ASTOR "A>"
ASTOR:
        CLRW    Y
        LD      YL,A
AYSTOR:
        DECW    X               ; SUBW  X,#2
        DECW    X
        LDW     (X),Y           ; push on stack
        RET

;       ATOKEY core ( -- c T | f )    ( TOS STM8: - Y,Z,N )
;       Return input char and true, or false.

        HEADER  ATOKEY "A>KEY"
ATOKEY:
        TNZ     A
        JREQ    ASTOR
        CALLR   ASTOR              ; push char
        DECW X
        DECW X
        LDW Y,#0XFFF
        LDW (X),Y
        RET

; Common functions

;       ?DUP    ( w -- w w | 0 )   ( TOS STM8: -- Y,Z,N )
;       Dup tos if its not zero.
        HEADER  QDUP "?DUP"
QDUP:
        LDW     Y,X
        LDW     Y,(Y)
        JREQ    QDUP1
        DECW    X
        DECW    X
        LDW     (X),Y
QDUP1:  RET

;       ROT     ( w1 w2 w3 -- w2 w3 w1 ) ( TOS STM8: -- Y,Z,N )
;       Rot 3rd item to top.

        HEADER  ROT "ROT"
ROT:
.if 1
; 31 bytes, 20 cy
LD A,(X)
LD XREG0,A
LD A,(1,X)
LD XREG0+1,A
LD A,(4,X)
LD (X),A
LD A,(5,X)
LD (1,X),A
LD A,(2,X)
LD (4,X),A
LD A,(3,X)
LD (5,X),A
LD A,XREG0
LD (2,X),A
LD A,XREG0+1
LD (3,X),A
RET
.else
        .ifeq   SPEEDOVERSIZE
        LD A,(1,X)              ; (17 bytes, 36 cy)
        PUSH A
        LD A,(X)
        PUSH A
        INCW X
        INCW X
        CALLR   1$
        POPW Y
        DECW X
        DECW X
        LDW (X),Y
1$:     JP      SWAPP
        .else
        LDW     Y,X             ; (22 bytes, 28 cy)
        LDW     X,(4,X)
        PUSHW   X
        LDW     X,Y
        LDW     X,(2,X)
        PUSHW   X
        LDW     X,Y
        LDW     X,(X)
        EXGW    X,Y
        LDW     (2,X),Y
        POPW    Y
        LDW     (4,X),Y
        POPW    Y
        LDW     (X),Y
        RET
        .endif
.endif

;       2DUP    ( w1 w2 -- w1 w2 w1 w2 )
;       Duplicate top two items.

        HEADER  DDUP "2DUP"
DDUP:
        CALLR    1$
1$:
        JP      OVER

        .ifeq   UNLINKCORE
;       DNEGATE ( d -- -d )     ( TOS STM8: -- Y,Z,N )
;       Two's complement of top double.

        HEADER  DNEGA "DNEGATE"
DNEGA:
        LDW     Y,X
        LDW     Y,(2,Y)
        NEGW    Y
        PUSH    CC
        LDW     (2,X),Y
        LDW     Y,X
        LDW     Y,(Y)
        CPLW    Y
        POP     CC
        JRC     DN1
        INCW    Y
DN1:    LDW     (X),Y
        RET
        .endif

;       =       ( w w -- t )    ( TOS STM8: -- Y,Z,N )
;       Return true if top two are equal.

        HEADER  EQUAL "="
EQUAL:
        .ifeq   SPEEDOVERSIZE
        CALL    XORR
        JP      ZEQUAL                 ; 31 cy= (18+13)
        .else
        CLRW Y                          ; (19 bytes, 17 cy)                
        LD A,(X)                              
        SUB A,(02,X)                         
        JRNE 1$                      
        LD A,(01,X)                          
        SUB A,(03,X)                         
        JRNE 1$                      
        CPLW Y                                 
1$:     INCW X                                 
        INCW X                                 
        LDW (X),Y                              
        RET
        .endif                          ; 17 cy, 19 bytes


;       U<      ( u u -- t )    ( TOS STM8: -- Y,Z,N )
;       Unsigned compare of top two items.

        HEADER  ULESS "U<"
ULESS:
        CLR     A
        CALLR   XREG0CMP
        JRUGE   1$
        CPL     A
1$:     LD      YL,A
        LD      YH,A
        LDW     (X),Y
        RET

        .ifeq   BOOTSTRAP
;       <       ( n1 n2 -- t )
;       Signed compare of top two items.

        HEADER  LESS "<"
LESS:
        .ifeq   SPEEDOVERSIZE
        CALL    SUBB             ; (29cy)
        JP      ZLESS            ; 41 cy (12+29)
        .else
        CLR     A
        LDW     Y,X
        LDW     Y,(Y)
        LDW     XREG0,Y
        INCW    X
        INCW    X
        LDW     Y,X
        LDW     Y,(Y)
        CPW     Y,XREG0
        JRSGE   1$
        CPL     A
1$:     LD      (X),A
        LD      (1,X),A
        LDW     Y,X
        LDW     Y,(Y)
        RET                      ; 26 cy
        .endif
        .endif

;       XREG0CMP       ( n n - n )      ( TOS STM8: - Y,Z,N )
;       Load (TOS) to XREG0 and (TOS-1) to Y, DROP, CMP to STM8 flags
XREG0CMP:
        LDW     Y,X
        INCW    X
        INCW    X
        EXGW    X,Y
        LDW     X,(X)
        LDW     XREG0,X
        LDW     X,Y
        LDW     X,(X)
        CPW     X,XREG0
        EXGW    X,Y
        RET

        .ifeq   BOOTSTRAP
;       MAX     ( n n -- n )    ( TOS STM8: -- Y,Z,N )
;       Return greater of two top items.

        HEADER  MAX "MAX"
MAX:
        CALLR   XREG0CMP
        JRSGT   MMEXIT
XREG0TOS:
        LDW     Y,XREG0
        LDW     (X),Y
MMEXIT:
        RET
        .endif

        .ifeq   BOOTSTRAP
;       MIN     ( n n -- n )    ( TOS STM8: -- Y,Z,N )
;       Return smaller of top two items.

        HEADER  MIN "MIN"
MIN:
        CALLR   XREG0CMP
        JRSLT   MMEXIT
        JRA     XREG0TOS
        .endif

;       WITHIN ( u ul uh -- t ) ( TOS STM8: -- Y,Z,N )
;       Return true if u is within
;       range of ul and uh. ( ul <= u < uh )

        HEADER  WITHI "WITHIN"
WITHI:
        CALL    OVER
        CALL    SUBB
        LD A,(1,X)
        PUSH A
        LD A,(X)
        PUSH A
        INCW X
        INCW X
        CALL    SUBB
        POPW Y
        DECW X
        DECW X
        LDW (X),Y
        JRA     ULESS

; Divide

;       UM/MOD  ( udl udh un -- ur uq )
;       Unsigned divide of a double by a
;       single. Return mod and quotient.

        HEADER  UMMOD "UM/MOD"
UMMOD:
        LDW     Y,X             ; stack pointer to Y
        LDW     X,(X)           ; un
        LDW     XREG0,X         ; save un
        LDW     X,Y
        INCW    X               ; drop un
        INCW    X
        PUSHW   X               ; save stack pointer
        LDW     X,(X)           ; X=udh
        LDW     Y,(4,Y)         ; Y=udl (offset before drop)
        CPW     X,XREG0
        JRULT   MMSM1           ; X is still on the R-stack
        POPW    X               ; restore stack pointer
        CLRW    Y
        LDW     (2,X),Y         ; remainder 0
        DECW    Y
        LDW     (X),Y           ; quotient max. 16 bit value
        RET
MMSM1:
        LD      A,#16           ; loop count
        SLLW    Y               ; udl shift udl into udh
MMSM3:
        RLCW    X               ; rotate udl bit into uhdh (= remainder)
        JRC     MMSMa           ; if carry out of rotate
        CPW     X,XREG0         ; compare udh to un
        JRULT   MMSM4           ; can't subtract
MMSMa:
        SUBW    X,XREG0         ; can subtract
        RCF
MMSM4:
        CCF                     ; quotient bit
        RLCW    Y               ; rotate into quotient, rotate out udl
        DEC     A               ; repeat
        JRNE    MMSM3           ; if A == 0
MMSMb:
        LDW     XREG0,X         ; done, save remainder
        POPW    X               ; restore stack pointer
        LDW     (X),Y           ; save quotient
        LDW     Y,XREG0         ; remainder onto stack
        LDW     (2,X),Y
        RET

        .ifeq   UNLINKCORE
;       M/MOD   ( d n -- r q )
;       Signed floored divide of double by
;       single. Return mod and quotient.

        HEADER  MSMOD "M/MOD"
MSMOD:
        LD      A,(X)           ; DUPP ZLESS
        PUSH    A               ; DUPP TOR
        JRPL    MMOD1           ; QBRAN
        CALL    NEGAT
        LD A,(1,X)
        PUSH A
        LD A,(X)
        PUSH A
        INCW X
        INCW X
        CALL    DNEGA
        POPW Y
        DECW X
        DECW X
        LDW (X),Y
MMOD1:
        LD A,(1,X)
        PUSH A
        LD A,(X)
        PUSH A
        INCW X
        INCW X
        LDW     Y,X
        LDW     Y,(Y)
        JRPL    MMOD2           ; DUPP ZLESS QBRAN
        POPW Y
        DECW X
        DECW X
        LDW (X),Y
        PUSHW Y
        CALL    PLUS
MMOD2:  
        POPW Y
        DECW X
        DECW X
        LDW (X),Y
        CALLR   UMMOD
        POP     A               ; RFROM
        TNZ     A
        JRPL    MMOD3           ; QBRAN
        CALL    SWAPP
        CALL    NEGAT
        JP      SWAPP
MMOD3:  RET

;       /MOD    ( n n -- r q )
;       Signed divide. Return mod and quotient.

        HEADER  SLMOD "/MOD"
SLMOD:
        CALL    OVER
        CALL    ZLESS
        CALL    SWAPP
        JRA     MSMOD

;       MOD     ( n n -- r )    ( TOS STM8: -- Y,Z,N )
;       Signed divide. Return mod only.

        HEADER  MMOD "MOD"
MMOD:
        CALLR   SLMOD
        INCW    X
        INCW    X
        RET

;       /       ( n n -- q )    ( TOS STM8: -- Y,Z,N )
;       Signed divide. Return quotient only.

        HEADER  SLASH "/"
SLASH:
        CALLR   SLMOD
        JP      NIP
        .endif

; Multiply

;       UM*     ( u u -- ud )
;       Unsigned multiply. Return double product.

        HEADER  UMSTA "UM*"
UMSTA:                          ; stack have 4 bytes u1=a,b u2=c,d
        LD      A,(2,X)         ; b
        LD      YL,A
        LD      A,(X)           ; d
        MUL     Y,A
        PUSHW   Y               ; PROD1 temp storage
        LD      A,(3,X)         ; a
        LD      YL,A
        LD      A,(X)           ; d
        MUL     Y,A
        PUSHW   Y               ; PROD2 temp storage
        LD      A,(2,X)         ; b
        LD      YL,A
        LD      A,(1,X)         ; c
        MUL     Y,A
        PUSHW   Y               ; PROD3,CARRY temp storage
        LD      A,(3,X)         ; a
        LD      YL,A
        LD      A,(1,X)         ; c
        MUL     Y,A             ; least signifiant product
        CLR     A
        RRWA    Y
        LD      (3,X),A         ; store least significant byte
        ADDW    Y,(1,SP)        ; PROD3
        CLR     A
        RLC     A               ; save carry
        LD      (1,SP),A        ; CARRY
        ADDW    Y,(3,SP)        ; PROD2
        LD      A,(1,SP)        ; CARRY
        ADC     A,#0            ; add 2nd carry
        LD      (1,SP),A        ; CARRY
        CLR     A
        RRWA    Y
        LD      (2,X),A         ; 2nd product byte
        ADDW    Y,(5,SP)        ; PROD1
        RRWA    Y
        LD      (1,X),A         ; 3rd product byte
        RRWA    Y               ; 4th product byte now in A
        ADC     A,(1,SP)        ; CARRY
        LD      (X),A
        ADDW    SP,#6           ; drop temp storage
        RET

;       *       ( n n -- n )    ( TOS STM8: -- Y,Z,N )
;       Signed multiply. Return single product.

        HEADER  STAR "*"
STAR:
        CALLR   UMSTA
        INCW    X
        INCW    X
        RET
        .ifeq   UNLINKCORE
;       M*      ( n n -- d )
;       Signed multiply. Return double product.
        HEADER  MSTAR "M*"
MSTAR:
        LD      A,(2,X)         ; DDUP
        XOR     A,(X)           ; XORR
        PUSH    A               ; TOR
        CALL    ABSS
        CALL    SWAPP
        CALL    ABSS
        CALLR   UMSTA
        POP     A               ; RFROM
        TNZ     A
        JRPL    MSTA1           ; QBRAN
        JP      DNEGA
MSTA1:  RET

;       */MOD   ( n1 n2 n3 -- r q )
;       Multiply n1 and n2, then divide
;       by n3. Return mod and quotient.
        HEADER  SSMOD "*/MOD"
SSMOD:
        LD A,(1,X)
        PUSH A
        LD A,(X)
        PUSH A
        INCW X
        INCW X
        CALLR   MSTAR
        POPW Y
        DECW X
        DECW X
        LDW (X),Y
        JP      MSMOD

;       */      ( n1 n2 n3 -- q )    ( TOS STM8: -- Y,Z,N )
;       Multiply n1 by n2, then divide
;       by n3. Return quotient only.
        HEADER  STASL "*/"
STASL:
        CALLR   SSMOD
        JP      NIP
        .endif

; Miscellaneous


        .ifeq   BAREBONES
;       EXG      ( n -- n )      ( TOS STM8: -- Y,Z,N )
;       Exchange high with low byte of n.

        HEADER  EXG "EXG"
EXG:
        LDW_Y_CONTENT_X
        SWAPW   Y
        LDW (X),Y
        RET
        .endif

;        .ifeq   BOOTSTRAP

;       2+      ( a -- a )      ( TOS STM8: -- Y,Z,N )
;       Add 2 to tos.

        HEADER  CELLP "2+"
CELLP:
        LDW_Y_CONTENT_X
        INCW    Y
        INCW    Y
        LDW (X),Y
        RET

;       NEGATE  ( n -- -n )     ( TOS STM8: -- Y,Z,N )
;       Two's complement of TOS.

        HEADER  NEGAT "NEGATE"
NEGAT:
        LDW_Y_CONTENT_X
        NEGW    Y
        LDW (X),Y
        RET

;       ABS     ( n -- n )      ( TOS STM8: -- Y,Z,N )
;       Return  absolute value of n.

        HEADER  ABSS "ABS"
ABSS:
        LDW_Y_CONTENT_X
        JRPL    1$              ; positive?
        NEGW    Y               ; else negate
        LDW (X),Y
1$:     RET

;       0=      ( n -- t )      ( TOS STM8: -- Y,Z,N ))
;       Return true if n is equal to 0

        HEADER  ZEQUAL "0="
ZEQUAL:
        LDW_Y_CONTENT_X
        JREQ    CPLW
CLRW:   CLRW    Y
        LDW (X),Y
        RET
CPLW:   CPLW    Y               ; else -1
        LDW (X),Y
        RET

;       !0=      ( n -- t )      ( TOS STM8: -- Y,Z,N ))
;       Return true if n is not equal to 0

        HEADER  ZEQUAL "!0="
NZEQUAL:
        LDW_Y_CONTENT_X
        JRNE    1$
        JRA     CLRW
1$:     CLRW    Y
        JRA     CPLW

;       PICK    ( ... +n -- ... w )      ( TOS STM8: -- Y,Z,N )
;       Copy    nth stack item to tos.

        HEADER  PICK "PICK"
PICK:
        LDW_Y_CONTENT_X
        INCW    Y
        SLAW    Y
        LDW     XREG0,X
        ADDW    Y,XREG0
        LDW     Y,(Y)
        LDW (X),Y
        RET

;       DEPTH   ( -- n )      ( TOS STM8: -- Y,Z,N )
;       Return  depth of data stack.

        HEADER  DEPTH "DEPTH"
DEPTH:
        LDW     Y,X
        NEGW    X
        ADDW    X,#SPP
        SRAW    X
XSTOR:  EXGW    X,Y
        DECW    X               ; SUBW  X,#2
        DECW    X
        LDW     (X),Y           ; push on stack
        RET                     ; go to RET of EXEC

; Memory access

;       +!      ( n a -- )      ( TOS STM8: -- Y,Z,N )
;       Add n to contents at address a.

        HEADER  PSTOR "+!"
PSTOR:
        LDW XREG0,X
        LDW X,(2,X)
        PUSHW X
        LDW X,XREG0
        LDW X,(X)        ; addr
        LDW Y,X
        LDW X,(X)
        ADDW X,(1,SP)
        LDW (Y),X
        POPW X
ENDPP:	LDW X,XREG0
        HEADER  DDROP "2DROP"
DDROP:
        ADDW    X,#4
        RET

;       +C!      ( n a -- )      ( TOS STM8: -- Y,Z,N )
;       Add n to contents at address a.

        HEADER  PCSTOR "+C!"
PCSTOR:
        LDW XREG0,X
        LDW X,(X)        ; addr
        LDW Y,X
        LD A,(X)
        LDW X,XREG0
        ADD A,(3,X)
        LD (Y),A
        JRA ENDPP

;       @EXECUTE        ( a -- )  ( TOS STM8: undefined )
;       Execute vector stored in address a.

        HEADER  ATEXE "@EXECUTE"
ATEXE:
        LDW     Y,X
        INCW    X
        INCW    X
        LDW     Y,(Y)
        LDW     Y,(Y)
        JREQ    1$
        JP      (Y)
1$:     RET

;       21+     ( u u -- u+1 u+1)
 
        HEADER  TWOONEPLUS "21+"
TWOONEPLUS:
        LDW Y,X 
        LDW X,(X) 
        INCW X  
        LDW (Y),X 
        LDW X,Y  
        LDW X,(2,X) 
        INCW X 
        LDW (2,Y),X 
        LDW X,Y
        RET

;       CMOVE   ( b1 b2 u -- )
;       Copy u bytes from b1 to b2.
        HEADER  CMOVE "CMOVE"
CMOVE:
        LD A,(1,X)
        PUSH A
        INCW X
        INCW X
CMOVE1: LDW Y,X
        LDW X,(2,X)
        LD A,(X)
        LDW X,Y
        LDW X,(X)
        LD (X),A
        LDW X,Y
        CALLR TWOONEPLUS
        POP A
        DEC A
        PUSH A
        JRNE CMOVE1
        POP A
        ADDW X,#4
        RET


; Basic I/O

;       KEY     ( -- c )
;       Wait for and return an
;       input character.

        HEADER  KEY "KEY"
KEY:
KEY1:   CALL    [USRQKEY]
        LDW     Y,X
        INCW    X
        INCW    X
        LDW     Y,(Y)
        JRNE    RETIDLE
        JRA     KEY1
RETIDLE:
        RET

;       QUIT    ( -- )
;       Reset return stack pointer
;       and start text interpreter.

        HEADER  QUIT "QUIT"
QUIT:
        LDW     Y,#RPP          ; initialize return stack
        LDW     SP,Y
        JP      SLEEP


;       SP!     ( a -- )
;       Set data stack pointer.

        HEADER  SPSTO "sp!"
SPSTO:
        LDW     X,(X)   ;X = a
        RET

;       SP@     ( -- a )        ( TOS STM8: -- Y,Z,N )
;       Push current stack pointer.

        HEADER  SPAT "sp@"
SPAT:
        LDW     Y,X
        DECW    X               ; SUBW  X,#2
        DECW    X
        LDW     (X),Y           ; push on stack
        RET                     ; go to RET of EXEC

;       RP@     ( -- a )     ( TOS STM8: -- Y,Z,N )
;       Push current RP to data stack.

        HEADER  RPAT "rp@"
RPAT:
        LDW     Y,SP            ; save return addr
        DECW    X               ; SUBW  X,#2
        DECW    X
        LDW     (X),Y           ; push on stack
        RET                     ; go to RET of EXEC

;       RP!     ( a -- )
;       Set return stack pointer.

        HEADFLG RPSTO "rp!" COMPO
RPSTO:
        POPW    Y
        LDW     XREG0,Y
        LDW     Y,X
        INCW    X
        INCW    X
        LDW     Y,(Y)
        LDW     SP,Y
        JP      [XREG0]

;===============================================================
        HEADFLG DO "DO" COMPO
DO:
        LDW Y,X
        POPW X
        PUSHW X
        PUSHW X
        PUSHW X
        LDW X,Y
        LDW X,(X)
        LDW (3,SP),X
        LDW X,Y
        LDW X,(2,X)
        LDW (5,SP),X
        LDW X,Y
        ADDW X,#4
        RET

;===============================================================
        .ifne   WORDS_EXTRAEEPR
;       ULOCK  ( -- )
;       Unlock EEPROM (STM8S)

        HEADER  ULOCK "ULOCK"
ULOCK:
        MOV     FLASH_DUKR,#0xAE
        MOV     FLASH_DUKR,#0x56
1$:     BTJF    FLASH_IAPSR,#3,1$    ; PM0051 4.1 requires polling bit3=1 before writing
        RET


;       LOCK  ( -- )
;       Lock EEPROM (STM8S)

        HEADER  LOCK "LOCK"
LOCK:
        BRES    FLASH_IAPSR,#3
        RET
        .endif

;       ULOCKF  ( -- )
;       Unlock Flash (STM8S)

        HEADER  UNLOCK_FLASH "ULOCKF"
UNLOCK_FLASH:
        MOV     FLASH_PUKR,#0x56
        MOV     FLASH_PUKR,#0xAE
1$:     BTJF    FLASH_IAPSR,#1,1$    ; PM0051 4.1 requires polling bit1=1 before writing
        RET

;       LOCKF  ( -- )
;       Lock Flash (STM8S)

        HEADER  LOCK_FLASH "LOCKF"
LOCK_FLASH:
        BRES    FLASH_IAPSR,#1
        RET

;       SAVEC ( -- )
;       Minimal context switch for low level interrupt code
;       This should be the first word called in the interrupt handler

        HEADER  SAVEC "SAVEC"
SAVEC:
        POPW    Y
        LDW     X,XREG0
        PUSHW   X
        LDW     X,#ISPP         ; "PC_ISPP" const. top of int. data stack
        JP      (Y)

;       IRET ( -- )
;       Restore context and return from low level interrupt code
;       This should be the last word called in the interrupt handler

        HEADER  RESTC "IRET"
RESTC:
        POPW    X
        LDW     XREG0,X         ; restore context
        IRET                    ; resturn from interrupt

;===============================================================

        LASTN   =       LINK    ;last name defined
        END_SDCC_FLASH = .
        USERRAM = RAMPOOL

        .area CODE
        .area INITIALIZER
        .area CABS (ABS)
