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

        .globl _TRAP_Handler
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
         .ifne STM8L
          .org 0x8070
          .dw 0x8200
          .dw _forth
          .dw 0x8200
          .dw _forth
          .dw 0x8200
          .dw _forth
          .dw 0x8200
          .dw _forth
         .endif
;--------------------------------------------------------
; restore broken interrupt vector table of STM8S
;--------------------------------------------------------
         .ifne STM8S
          .org 0x8068
          .dw 0x8200
          .dw 0x0
         .endif
         .if   HAS_RXUART*HAS_TXUART
          .org 0x8008   + ITC_IX_UART1RX * 4
          .dw 0x8200
          .dw UART_INT
         .endif

.area CODE

;--------------------------------------------------------
; global & static initialisations
;--------------------------------------------------------
        .area HOME
        .area GSINIT
        .area GSFINAL
        .area GSINIT
;--------------------------------------------------------
; Home
;--------------------------------------------------------
        .area HOME
        .area HOME
;--------------------------------------------------------
; code
;--------------------------------------------------------
        .area CODE

        ;************************************
        ;******  1) General Constants  ******
        ;************************************

        TRUEE   =     0xFFFF    ; true flag
        COMPO   =     0x40      ; "COMPO" lexicon compile only bit
        IMEDD   =     0x80      ; "IMEDD" lexicon immediate bit
        MASKK   =     0x1F7F    ; "MASKK" lexicon bit mask

        TIBLENGTH =   80        ; size of TIB (starting at TIBOFFS)
        PADOFFS =     80        ; "PADOFFS" offset text buffer above dictionary
        CELLL   =      2        ; size of a cell
        BASEE   =     10        ; default radix
        BKSPP   =      8        ; backspace
        LF      =     10        ; line feed
        PACE    =     11        ; pace character for host handshake (ASCII VT)
        CRR     =     13        ; carriage return
        ERR     =     27        ; error escape
        BLNK    =     32        ; blank char
        TIC     =     39        ; tick

        EXIT_OPC =    0x81      ; RET opcode
        DOLIT_OPC =   0x83      ; TRAP opcode as DOLIT
        CALLR_OPC =   0xAD      ; CALLR opcode for relative addressing
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
        STM8S            = 1    ; FAMILY: STM8S device
        STM8L            = 0    ; FAMILY: STM8L device

        ; legacy chip type (deprecated - preferably use the chip type constants)
        STM8L101F3 = STM8L_101  ; L core, 8K flash incl EEPROM, 1.5K RAM, UART1
        STM8L051F3 = STM8L_LOD  ; L core, 8K flash, 1K RAM, 256 EEPROM, UART1
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
        .include        "muforth-inc/target.inc"

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

        .include  "muforth-inc/defconf.inc"

        ;********************************************
        ;******  4) Device dependent features  ******
        ;********************************************
        ; Define memory location for device dependent features here

        .include "globconf.inc"

        .include "linkopts.inc"

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

        UPP   = UPPLOC          ; "C_UPP" constant offset user area
        PADBG = UPPLOC          ; PAD in background task growing down from here
        CTOP  = CTOPLOC         ; dictionary start, growing up
                                ; note: PAD is inbetween CTOP and SPP
        SPP   = ISPP-ISPPSIZE   ; "SPP"  data stack, growing down (with SPP-1 first)
        ISPP  = SPPLOC-BSPPSIZE ; "ISPP" Interrupt data stack, growing down
        BSPP  = SPPLOC          ; "BSPP" Background data stack, growing down
        TIBB  = SPPLOC          ; "TIB"  Term. Input Buf. TIBLENGTH between SPPLOC and RPP
        RPP   = RPPLOC          ; "RPP"  constant addr. return stack, growing down

        ; Core variables (same order as 'BOOT initializer block)

        USRRAMINIT = USREMIT

        USREMIT  =   UPP+0      ; "'EMIT" execution vector of EMIT
        USRQKEY =    UPP+2      ; "'?KEY" execution vector of QKEY
.if  HAS_RXSIM
        USR_5   =    UPP+4      ; chat variables
        USR_6   =    UPP+5      ;
.endif
        RamWord MP              ; memory pointer for mu-chat

        ; More core variables in zero page (instead of assigning fixed addresses)
        RamWord USRHLD          ; "HLD" hold a pointer of output string
        RamWord YTEMP           ; extra working register for core words
        RamWord USRIDLE         ; "'IDLE" idle routine in KEY

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

;       TRAP handler for DOLIT
;       Push the inline literal following the TRAP instruction
_TRAP_Handler:
        .ifeq  USE_CALLDOLIT
        DECW    X
        DECW    X
        LDW     (3,SP),X               ; XH,XL
        EXGW    X,Y
        LDW     X,(8,SP)               ; PC MSB/LSB
        LDW     X,(X)
        LDW     (Y),X
        LDW     (5,SP),X               ; YH,YL
        LDW     X,(8,SP)
        INCW    X
        INCW    X
        LDW     (8,SP),X
        IRET

;       Macros for inline literals using the TRAP approach

        .macro DoLitC c
        TRAP
        .dw     c
        .endm

        .macro DoLitW w
        TRAP
        .dw     w
        .endm

        .else

;       Macros for inline literals using CALL DOLIT / CALL DOLITC
        .macro DoLitC c
        call    DOLITC
        .db     c
        .endm

        .macro DoLitW w
        call    DOLIT
        .dw     w
        .endm

        .endif

;       Macros for compiling inline literals using CALL COMPILIT
        .macro ComLit w
        call    COMPILIT
        .dw     w
        .endm

        .macro ComLitR w
        callr   COMPILIT
        .dw     w
        .endm


; ==============================================

;       Includes for board support code
;       Board I/O initialization and E/E mapping code
;       Hardware dependent words, e.g.  BKEY, OUT!
        .include "muforth-inc/boardcore.inc"

;       ADC routines depending on STM8 family
        .include "stm8_adc.inc"

;       Generic board I/O: 7S-LED rendering, board key mapping
        .include "board_io.inc"

;       Simulate serial interface code
        .include "muforth-inc/sser.inc"

;       Background Task: context switch with wakeup unit or timer
        .include "muforth-inc/bgtask.inc"
UPPLOC = RAMPOOL + 30  ; PAD in Background task, growing down, 32 bytes

; ==============================================

;       Configuation table with shadow data for RESET

;       'BOOT   ( -- a )
;       The application startup vector and NVM USR setting array

        HEADER  TBOOT "'BOOT"
TBOOT:
        CALL    DOVAR
        UBOOT = . 
        .dw     . + 2           ; start-up code, just return for now. (Can be changed with 'BOOT !)
        RET

;       Main entry points and COLD start data

;       COLD    ( -- )
;       The hilevel cold start sequence.
        HEADER  COLD "COLD"

_forth:                         ; SDCC entry
        ; Note: no return to main.c possible unless RAMEND equals SP,
        ; and RPP init skipped

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
        MOV     UART_CR2,#0x0C  ; Use UART1 full duplex
        BSET    UART_CR2,#5     ; RIEN, enable RXNE interrupt 

        .ifne   HALF_DUPLEX
        .ifeq   (FAMILY - STM8S)
        .ifeq   (HALF_DUPLEX - 1)
        ; STM8S UART1, UART4: pull-up for PD5 single-wire UART
        BRES    PD_DDR,#5       ; PD5 GPIO input high
        BSET    PD_CR1,#5       ; PD5 GPIO pull-up
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

        CALL    PRESE           ; initialize data stack, TIB

.if  HAS_RXSIM
MOV USR_5,#255
.endif

        ; Hardware initialization complete
        RIM                     ; enable interrupts

        CALL    [TBOOT+3]       ; application boot

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
        CALL CHAT
        LDW (#3,SP),X             ; X is popped from return stack during IRET
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

        .ifne   HAS_DOLOOP
;       (+loop) ( +n -- )
;       Add n to index R@ and test for lower than limit (R-CELL)@.

        HEADFLG DOPLOOP "(+loop)" COMPO
DOPLOOP:
        LDW     Y,(5,SP)
        LDW     YTEMP,Y
        LDW     Y,X
        LDW     Y,(Y)
        LD      A,YH
        INCW    X
        INCW    X
        ADDW    Y,(3,SP)
        CPW     Y,YTEMP
        PUSH    CC
        TNZ     A
        JRMI    1$
        POP     CC
        JRSGE   LEAVE
        JRA     2$
1$:     POP     CC
        JRSLT   LEAVE
2$:     LDW     (3,SP),Y
        JRA     BRAN

;       LEAVE   ( -- )
;       Leave a DO .. LOOP/+LOOP loop.

        HEADFLG LEAVE "LEAVE" COMPO
LEAVE:
        ADDW    SP,#6
        POPW    Y               ; DO leaves the address of +loop on the R-stack
        JP      (2,Y)
        .endif

;       donext    ( -- )
;       Code for single index loop.

        HEADFLG DONXT "donxt" COMPO
DONXT:
        LDW     Y,(3,SP)
        DECW    Y
        JRPL    NEX1
        POPW    Y
        POP     A
        POP     A
        JP      (2,Y)
NEX1:   LDW     (3,SP),Y
        JRA     BRAN

;       ?branch ( f -- )
;       Branch if flag is zero.

        HEADFLG QBRAN "?branch" COMPO
QBRAN:
        CALL    YFLAGS          ; Pull TOS to Y, flags
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
        CALL    YFLAGS          ; Pull TOS to Y, flags
        JP      (Y)


        .ifeq   REMOVE_EXIT
;       EXIT    ( -- )
;       Terminate a colon definition.

        HEADER  EXIT "EXIT"
EXIT:
        POPW    Y
        RET
        .endif

        .ifeq   BOOTSTRAP
;       2!      ( d a -- )      ( TOS STM8: -- Y,Z,N )
;       Store double integer to address a.

        HEADER  DSTOR "2!"
DSTOR:
        CALL    SWAPP
        CALL    OVER
        CALLR   STORE
        CALL    CELLP
        JRA     STORE
        .endif

        .ifeq   BOOTSTRAP
;       2@      ( a -- d )
;       Fetch double integer from address a.

        HEADER  DAT "2@"
DAT:
        CALL    DUPP
        CALL    CELLP
        CALLR   AT
        CALL    SWAPP
        JRA     AT
        .endif


        .ifne   WORDS_EXTRAMEM
;       2C!  ( n a -- )
;       Store word C-wise to 16 bit HW registers "MSB first"

        HEADER  DCSTOR "2C!"
DCSTOR:
        CALL    YFLAGS          ; a
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


;       B! ( t a u -- )
;       Set/reset bit #u (0..7) in the byte at address a to bool t
;       Note: creates/executes BSER/BRES + RET code on Data Stack
        HEADER  BRSS "B!"
BRSS:
        LD      A,#0x72         ; Opcode BSET/BRES
        LD      (X),A
        LD      A,(1,X)         ; 2nd byte of BSET/BRES
        SLA     A               ; n *= 2 -> A
        OR      A,#0x10
        LDW     Y,X
        LDW     Y,(4,Y)         ; bool b (0..15) -> Z
        JRNE    1$              ; b!=0: BSET
        INC     A               ; b==0: BRES
1$:     LD      (1,X),A
        LD      A,#EXIT_OPC     ; Opcode RET
        LD      (4,X),A
        LDW     Y,X
        ADDW    X,#6
        JP      (Y)

        .endif


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
        .ifeq   SPEEDOVERSIZE
        PUSHW   X               ; (16 bytes)
        LDW     Y,X
        LDW     X,(X)           ; a
        PUSHW   X
        LDW     X,Y
        LDW     X,(2,X)         ; w
        EXGW    X,Y
        POPW    X
        LDW     (X),Y
        POPW    X
        ADDW    X,#4
        RET
        .else
        CALL    YFLAGS          ; a  (10 bytes)
        PUSHW   X
        LDW     X,(X)           ; w
        LDW     (Y),X
        POPW    X
        JRA     DROP
        .endif

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
        CALL    YFLAGS
        INCW    X
        LD      A,(X)
        LD      (Y),A
        INCW    X
        RET


;       R>      ( -- w )     ( TOS STM8: -- Y,Z,N )
;       Pop return stack to data stack.

        HEADFLG RFROM "R>" COMPO
RFROM:
        POPW    Y               ; save return addr
        LDW     YTEMP,Y
        POPW    Y
        DECW    X
        DECW    X
        LDW     (X),Y
        JP      [YTEMP]


        .ifne  HAS_CPNVM
;       doVarPtr ( -- a )    ( TOS STM8: -- Y,Z,N )

        HEADFLG DOVARPTR "doVarPtr" COMPO
DOVARPTR:
        POPW    Y               ; get return addr (pfa)
        JP      YAT
        .endif

;       doVAR   ( -- a )     ( TOS STM8: -- Y,Z,N )
;       Code for VARIABLE and CREATE.

        HEADFLG DOVAR "doVar" COMPO
DOVAR:
        POPW    Y               ; get return addr (pfa)
        ; fall through

;       Y>  ( -- n )     ( TOS STM8: - Y,Z,N )
;       push Y to stack

        HEADER  YSTOR "Y>"
YSTOR:
        DECW    X               ; SUBW  X,#2
        DECW    X
        LDW     (X),Y           ; push on stack
        RET                     ; go to RET of EXEC

;       R@      ( -- w )        ( TOS STM8: -- Y,Z,N )
;       Copy top of return stack to stack (or the FOR - NEXT index value).

        HEADER  RAT "R@"
RAT:
        LDW     Y,(3,SP)
        JRA     YSTOR

;       >R      ( w -- )      ( TOS STM8: -- Y,Z,N )
;       Push data stack to return stack.

        HEADFLG TOR ">R" COMPO
TOR:
        EXGW    X,Y
        LDW     X,(1,SP)
        PUSHW   X
        LDW     X,Y
        LDW     X,(X)
        EXGW    X,Y
        LDW     (3,SP),Y
        JRA     DROP


;       NIP     ( n1 n2 -- n2 )
;       Drop 2nd item on the stack

        HEADER  NIP "NIP"
NIP:
        CALLR   SWAPP
        JRA     DROP

;       DROP    ( w -- )        ( TOS STM8: -- Y,Z,N )
;       Discard top stack item.

        HEADER  DROP "DROP"
DROP:
        INCW    X               ; ADDW   X,#2
        INCW    X
        LDW     Y,X
        LDW     Y,(Y)
        RET

;       2DROP   ( w w -- )       ( TOS STM8: -- Y,Z,N )
;       Discard two items on stack.

        HEADER  DDROP "2DROP"
DDROP:
        ADDW    X,#4
        RET

;       DUP     ( w -- w w )    ( TOS STM8: -- Y,Z,N )
;       Duplicate top stack item.

        HEADER  DUPP "DUP"
DUPP:
        LDW     Y,X
        JP      YAT

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

;       OVER    ( w1 w2 -- w1 w2 w1 ) ( TOS STM8: -- Y,Z,N )
;       Copy second stack item to top.

        HEADER  OVER "OVER"
OVER:
        LDW     Y,X
        LDW     Y,(2,Y)
        JRA     YSTOR

        .ifne   WORDS_EXTRACORE
;       I       ( -- n )     ( TOS STM8: -- Y,Z,N )
;       Get inner FOR-NEXT or DO-LOOP index value
        HEADER  IGET "I"
IGET:
        .ifne   HAS_ALIAS
        JP      RAT             ; CF JP: NAME> resolves I as ' R@"
        .else
        JRA     RAT
        .endif
        .endif

        .ifeq   BOOTSTRAP
;       UM+     ( u u -- udsum )
;       Add two unsigned single
;       and return a double sum.

        HEADER  UPLUS "UM+"
UPLUS:
        CALLR   PLUS
        CLR     A
        RLC     A
        JP      ASTOR
        .endif

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
        LD      (2,X),A
        JRA     DROP

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

        .ifeq   BOOTSTRAP
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
        .endif

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
        LDW     YTEMP,Y
        INCW    X
        INCW    X
        LDW     Y,X
        LDW     Y,(Y)
        SUBW    Y,YTEMP
        LDW     (X),Y
        RET                     ; 18 cy
        .endif

        HEADER  ASTOR "A>"

;       A>  ( -- n )     ( TOS STM8: - Y,Z,N )
;       push A to stack

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
        JREQ    1$
        CALLR   1$              ; push char
        JRA     MONE            ; flag true
1$:     JRA     ASTOR           ; push char or flag false


;       TIB     ( -- a )     ( TOS STM8: -- Y,Z,N )
;       Return address of terminal input buffer.

        .ifeq   UNLINK_TIB
        HEADER  TIB "TIB"
TIB:
        DoLitW  TIBB
        RET
        .endif

; Constants

;       BL      ( -- 32 )     ( TOS STM8: -- Y,Z,N )
;       Return blank character.

        HEADER  BLANK "BL"
BLANK:
        LD      A,#(BLNK)
        JRA     ASTOR

;       0       ( -- 0)     ( TOS STM8: -- Y,Z,N )
;       Return 0.

        HEADER  ZERO "0"
ZERO:
        CLR     A
        JRA     ASTOR

;       1       ( -- 1)     ( TOS STM8: -- Y,Z,N )
;       Return 1.

        HEADER  ONE "1"
ONE:
        LD      A,#1
        JRA     ASTOR

;       -1      ( -- -1)     ( TOS STM8: -- Y,Z,N )
;       Return -1

        HEADER  MONE "-1"
MONE:
        LDW     Y,#0xFFFF
        JRA     AYSTOR

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
        .ifeq   SPEEDOVERSIZE
        CALL    TOR
        CALLR   1$
        CALL    RFROM
1$:     JP      SWAPP
        .else
        LDW     Y,X
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

        ; .ifeq   BOOTSTRAP
;       =       ( w w -- t )    ( TOS STM8: -- Y,Z,N )
;       Return true if top two are equal.

        HEADER  EQUAL "="
EQUAL:
        .ifeq   SPEEDOVERSIZE
        CALL    XORR
        JP      ZEQUAL                 ; 31 cy= (18+13)
        .else
        LD      A,#0x0FF         ; true
        LDW     Y,X              ; D = n2
        LDW     Y,(Y)
        LDW     YTEMP,Y
        INCW    X
        INCW    X
        LDW     Y,X
        LDW     Y,(Y)
        CPW     Y,YTEMP ;if n2 <> n1
        JREQ    EQ1
        CLR     A
EQ1:    LD      (X),A
        LD      (1,X),A
        LDW     Y,X
        LDW     Y,(Y)
        RET                            ; 24 cy
       .endif
        ; .endif


;       U<      ( u u -- t )    ( TOS STM8: -- Y,Z,N )
;       Unsigned compare of top two items.

        HEADER  ULESS "U<"
ULESS:
        CLR     A
        CALLR   YTEMPCMP
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
        LDW     YTEMP,Y
        INCW    X
        INCW    X
        LDW     Y,X
        LDW     Y,(Y)
        CPW     Y,YTEMP
        JRSGE   1$
        CPL     A
1$:     LD      (X),A
        LD      (1,X),A
        LDW     Y,X
        LDW     Y,(Y)
        RET                      ; 26 cy
        .endif
        .endif

;       YTEMPCMP       ( n n - n )      ( TOS STM8: - Y,Z,N )
;       Load (TOS) to YTEMP and (TOS-1) to Y, DROP, CMP to STM8 flags
YTEMPCMP:
        LDW     Y,X
        INCW    X
        INCW    X
        EXGW    X,Y
        LDW     X,(X)
        LDW     YTEMP,X
        LDW     X,Y
        LDW     X,(X)
        CPW     X,YTEMP
        EXGW    X,Y
        RET

        .ifeq   BOOTSTRAP
;       MAX     ( n n -- n )    ( TOS STM8: -- Y,Z,N )
;       Return greater of two top items.

        HEADER  MAX "MAX"
MAX:
        CALLR   YTEMPCMP
        JRSGT   MMEXIT
YTEMPTOS:
        LDW     Y,YTEMP
        LDW     (X),Y
MMEXIT:
        RET
        .endif

        .ifeq   BOOTSTRAP
;       MIN     ( n n -- n )    ( TOS STM8: -- Y,Z,N )
;       Return smaller of top two items.

        HEADER  MIN "MIN"
MIN:
        CALLR   YTEMPCMP
        JRSLT   MMEXIT
        JRA     YTEMPTOS
        .endif

        .ifeq   UNLINK_WITHI
;       WITHIN ( u ul uh -- t ) ( TOS STM8: -- Y,Z,N )
;       Return true if u is within
;       range of ul and uh. ( ul <= u < uh )

        HEADER  WITHI "WITHIN"
WITHI:
        CALL    OVER
        CALL    SUBB
        CALL    TOR
        CALL    SUBB
        CALL    RFROM
        JRA     ULESS
        .endif

; Divide

;       UM/MOD  ( udl udh un -- ur uq )
;       Unsigned divide of a double by a
;       single. Return mod and quotient.

        HEADER  UMMOD "UM/MOD"
UMMOD:
        LDW     Y,X             ; stack pointer to Y
        LDW     X,(X)           ; un
        LDW     YTEMP,X         ; save un
        LDW     X,Y
        INCW    X               ; drop un
        INCW    X
        PUSHW   X               ; save stack pointer
        LDW     X,(X)           ; X=udh
        LDW     Y,(4,Y)         ; Y=udl (offset before drop)
        CPW     X,YTEMP
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
        CPW     X,YTEMP         ; compare udh to un
        JRULT   MMSM4           ; can't subtract
MMSMa:
        SUBW    X,YTEMP         ; can subtract
        RCF
MMSM4:
        CCF                     ; quotient bit
        RLCW    Y               ; rotate into quotient, rotate out udl
        DEC     A               ; repeat
        JRNE    MMSM3           ; if A == 0
MMSMb:
        LDW     YTEMP,X         ; done, save remainder
        POPW    X               ; restore stack pointer
        LDW     (X),Y           ; save quotient
        LDW     Y,YTEMP         ; remainder onto stack
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
        CALL    TOR
        CALL    DNEGA
        CALL    RFROM
MMOD1:
        CALL    TOR
        JRPL    MMOD2           ; DUPP ZLESS QBRAN
        CALL    RAT
        CALL    PLUS
MMOD2:  CALL    RFROM
        CALLR   UMMOD
        POP     A               ; RFROM
        TNZ     A
        JRPL    MMOD3           ; QBRAN
        CALL    SWAPP
        CALL    NEGAT
        CALL    SWAPP
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
        JP      DROP

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
        JP      DROP

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
        CALL    DNEGA
MSTA1:  RET

;       */MOD   ( n1 n2 n3 -- r q )
;       Multiply n1 and n2, then divide
;       by n3. Return mod and quotient.
        HEADER  SSMOD "*/MOD"
SSMOD:
        CALL    TOR
        CALLR   MSTAR
        CALL    RFROM
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
        CALLR   DOXCODE
        SWAPW   X
        RET
        .endif

        .ifeq   BOOTSTRAP
;       2/      ( n -- n )      ( TOS STM8: -- Y,Z,N )
;       Divide tos by 2.

        HEADER  TWOSL "2/"
TWOSL:
        CALLR   DOXCODE
        SRAW    X
        RET

;       2*      ( n -- n )      ( TOS STM8: -- Y,Z,N )
;       Multiply tos by 2.

        HEADER  CELLS "2*"
CELLS:
        CALLR   DOXCODE
        SLAW    X
        RET
        .endif

;       2-      ( a -- a )      ( TOS STM8: -- Y,Z,N )
;       Subtract 2 from tos.

        HEADER  CELLM "2-"
CELLM:
        CALLR   DOXCODE
        DECW    X
        DECW    X
        RET

;       2+      ( a -- a )      ( TOS STM8: -- Y,Z,N )
;       Add 2 to tos.

        HEADER  CELLP "2+"
CELLP:
        CALLR   DOXCODE
        INCW    X
        INCW    X
        RET

;       1-      ( n -- n )      ( TOS STM8: -- Y,Z,N )
;       Subtract 1 from tos.

        HEADER  ONEM "1-"
ONEM:
        CALLR   DOXCODE
        DECW    X
        RET

;       1+      ( n -- n )      ( TOS STM8: -- Y,Z,N )
;       Add 1 to tos.

        HEADER  ONEP "1+"
ONEP:
        CALLR   DOXCODE
        INCW    X
        RET


;       DOXCODE   ( n -- n )   ( TOS STM8: - Y,Z,N )
;       precede assembly code for a primitive word
;       Caution: no other Forth word can be called from assembly!
;       In the assembly code: X=(TOS), YTEMP=TOS. (TOS)=X after RET

        HEADER  DOXCODE "DOXCODE"

DOXCODE:
        POPW    Y
        LDW     YTEMP,X
        LDW     X,(X)
        CALL    (Y)
        EXGW    X,Y
        LDW     X,YTEMP
        LDW     (X),Y
        RET

;       NOT     ( w -- w )     ( TOS STM8: -- Y,Z,N )
;       One's complement of TOS.

        HEADER  INVER "NOT"
INVER:
        CALLR   DOXCODE
        CPLW    X
        RET

;       NEGATE  ( n -- -n )     ( TOS STM8: -- Y,Z,N )
;       Two's complement of TOS.

        HEADER  NEGAT "NEGATE"
NEGAT:
        CALLR   DOXCODE
        NEGW    X
        RET

;       ABS     ( n -- n )      ( TOS STM8: -- Y,Z,N )
;       Return  absolute value of n.

        HEADER  ABSS "ABS"
ABSS:
        CALLR   DOXCODE
        JRPL    1$              ; positive?
        NEGW    X               ; else negate
1$:     RET

;       0=      ( n -- t )      ( TOS STM8: -- Y,Z,N ))
;       Return true if n is equal to 0

        HEADER  ZEQUAL "0="
ZEQUAL:
        CALLR   DOXCODE
        JREQ    1$
        CLRW    X
        RET
1$:     CPLW    X               ; else -1
        RET

;       PICK    ( ... +n -- ... w )      ( TOS STM8: -- Y,Z,N )
;       Copy    nth stack item to tos.

        HEADER  PICK "PICK"
PICK:
        CALLR   DOXCODE
        INCW    X
        SLAW    X
        ADDW    X,YTEMP
        LDW     X,(X)
        RET



;       DEPTH   ( -- n )      ( TOS STM8: -- Y,Z,N )
;       Return  depth of data stack.

        HEADER  DEPTH "DEPTH"
DEPTH:
        LDW     Y,X
        NEGW    X
        ADDW    X,PRSPP+1        ; use SPP constant from PC_SPP in PRESET
        SRAW    X
        ;DECW    X               ; fixed: off-by-one to compensate error in "rp!"
XSTOR:  EXGW    X,Y
        JP      YSTOR

; Memory access

;       +!      ( n a -- )      ( TOS STM8: -- Y,Z,N )
;       Add n to contents at address a.

        HEADER  PSTOR "+!"
PSTOR:
        LDW     Y,X
        LDW     X,(X)
        LDW     YTEMP,X
        LDW     X,Y
        LDW     X,(2,X)
        PUSHW   X
        LDW     X,[YTEMP]
        ADDW    X,(1,SP)
        LDW     [YTEMP],X
        POPW    X
        EXGW    X,Y
        JP      DDROP

;       COUNT   ( b -- b +n )      ( TOS STM8: -- A,Z,N )
;       Return count byte of a string
;       and add 1 to byte address.

        HEADER  COUNT "COUNT"
COUNT:
        CALL    DUPP
        CALL    ONEP
        CALL    SWAPP
        JP      CAT

        .ifeq   UNLINK_ATEXE
;       @EXECUTE        ( a -- )  ( TOS STM8: undefined )
;       Execute vector stored in address a.

        HEADER  ATEXE "@EXECUTE"
ATEXE:
        CALL    YFLAGS
        LDW     Y,(Y)
        JREQ    1$
        JP      (Y)
1$:     RET
        .endif

;       CMOVE   ( b1 b2 u -- )
;       Copy u bytes from b1 to b2.

        HEADER  CMOVE "CMOVE"
CMOVE:
        CALL    TOR
        JRA     CMOV2
CMOV1:  CALL    TOR
        CALL    DUPPCAT
        CALL    RAT
        CALL    CSTOR
        CALL    ONEP
        CALL    RFROM
        CALL    ONEP
CMOV2:  CALL    DONXT
        .dw     CMOV1
        JP      DDROP

;       FILL    ( b u c -- )
;       Fill u bytes of character c
;       to area beginning at b.

        HEADER  FILL "FILL"
FILL:
        CALL    SWAPP
        CALL    TOR
        CALL    SWAPP
        JRA     FILL2
FILL1:  CALL    DDUP
        CALL    CSTOR
        CALL    ONEP
FILL2:  CALL    DONXT
        .dw     FILL1
        JP      DDROP

        .ifeq   BAREBONES
;       ERASE   ( b u -- )
;       Erase u bytes beginning at b.

        HEADER  ERASE "ERASE"
ERASE:
        CALL    ZERO
        JRA     FILL
        .endif

; Basic I/O

;       KEY     ( -- c )
;       Wait for and return an
;       input character.

        HEADER  KEY "KEY"
KEY:
KEY1:   CALL    [USRQKEY]
        CALL    YFLAGS
        JRNE    RETIDLE
        LD      A,USRIDLE
        OR      A,USRIDLE+1
        JREQ    KEY2
        CALL    [USRIDLE]       ; IDLE must be fast (unless ?RX is buffered) and stack neutral
KEY2:
        JRA     KEY1
RETIDLE:
        RET

;       >Y  ( n -- )       ( TOS STM8: - Y,Z,N )
;       Consume TOS to CPU Y and Flags

        HEADER  YFLAGS ">Y"

YFLAGS:
        LDW     Y,X
        INCW    X
        INCW    X
        LDW     Y,(Y)
        RET


;       >A   ( c -- )       ( TOS STM8: - A,Z,N )
;       Consume TOS to CPU A and Flags

        HEADER  AFLAGS "A>"
AFLAGS:
        INCW    X
        LD      A,(X)
        INCW    X
        TNZ     A
        RET

;       ABORT   ( -- )
;       Reset data stack and
;       jump to QUIT.

        HEADER  ABORT "ABORT"
ABORT:
        CALLR   PRESE
        JP      QUIT

;       PRESET  ( -- )
;       Reset data stack pointer and
;       terminal input buffer.

        HEADER  PRESE "PRESET"
PRESE:

PRSPP:  LDW     X,#SPP          ; "PC_SPP" addr. const. top of data stack
        RET

;       ?STACK  ( -- )
;       Abort if stack underflows.

        HEADER  QSTAC "?STACK"
QSTAC:
        CALL    DEPTH
        CALL    ZLESS           ; check only for underflow
        JRA    ABORT        

;       QUIT    ( -- )
;       Reset return stack pointer
;       and start text interpreter.

        HEADER  QUIT "QUIT"
QUIT:
        LDW     Y,#RPP          ; initialize return stack
        LDW     SP,Y
        JP      SLEEP

        HEADER  CCOMMA ^/"C,"/
CCOMMA:
        CALL    AFLAGS
        ; fall through

        HEADER  thisvar "thisvar" 

THISVAR:
        LDW     Y,(3,SP)
        DECW    X               ; YSTOR
        DECW    X
        LDW     (X),Y
        POPW    Y
        POP     A               ; RDROP
        POP     A
        JP      (Y)

;       A@   ( A:shortAddr -- n )
;       push contents of A:shortAddr on stack

        HEADER  AAT "A@"
AAT:
        CLRW    Y
        LD      YL,A
        ; fall through

;       Y@   ( Y:Addr -- n )
;       push contents of Y:Addr on stack

        HEADER  YAT "Y@"
YAT:
        LDW     Y,(Y)
        JP      YSTOR


;       docon ( -- )
;       state dependent action code of constant

        HEADER  DOCON "docon"
DOCON:
        POPW    Y
        JRA   YAT             ; R> AT push constant in interpreter mode

DUPPCAT:
        CALL    DUPP
        JP      CAT

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
        JP      YSTOR

;       RP@     ( -- a )     ( TOS STM8: -- Y,Z,N )
;       Push current RP to data stack.

        HEADER  RPAT "rp@"
RPAT:
        LDW     Y,SP            ; save return addr
        JP      YSTOR

;       RP!     ( a -- )
;       Set return stack pointer.

        HEADFLG RPSTO "rp!" COMPO
RPSTO:
        POPW    Y
        LDW     YTEMP,Y
        CALL    YFLAGS          ; fixed error: TOS not consumed
        LDW     SP,Y
        JP      [YTEMP]

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
        LDW     X,YTEMP
        PUSHW   X
        LDW     X,#ISPP         ; "PC_ISPP" const. top of int. data stack
        JP      (Y)

;       IRET ( -- )
;       Restore context and return from low level interrupt code
;       This should be the last word called in the interrupt handler

        HEADER  RESTC "IRET"
RESTC:
        POPW    X
        LDW     YTEMP,X         ; restore context
        IRET                    ; resturn from interrupt

VERSION:
	.db #12
	.ascii "STM8EF2.2.29"

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


; send bytes from memory pointed to by MP
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

SETADDR:
	CALLR TOW
	LDW Y,X
	LDW Y,(Y)
	LDW MP,Y
	INCW X
	INCW X
	RET

GETSP:
	CALL SPAT
	JRA WTO

WRITEBS:
	CALLR TOB	; count
1$:	LDW Y,MP		; memory pointer in Y
	CALLR RXA	; 
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
	DoLitW FLASHBUF_ADDR
	CALLR TOW
	CALLR TOW
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

;===============================================================

        LASTN   =       LINK    ;last name defined
        END_SDCC_FLASH = .

        .area CODE
        .area INITIALIZER
        .area CABS (ABS)
