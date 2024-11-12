;--------------------------------------------------------
;       STM8 eForth for STM8S and STM8L devices
;       Default settings for all options
;--------------------------------------------------------
        RELVER1          = 2    ; Revision digit 1
        RELVER0          = 9   ; Revision digit 0
        PRE_REL          = 0    ; Pre Release digit 0 (0: release)

        TERM_LINUX       = 1    ; LF terminates line

        HALF_DUPLEX      = 0    ; Use the STM8S Low Density UART in half duplex mode (1: PD5, 2: PA3)
        USE_UART2        = 0    ; Use the 2nd UART for the console (STM8S207: optional, STM8S105: default, STM8L152: optional)
        USE_UART3        = 0    ; Use the 3rd UART for the console (STM8L152: optional)
        ALT_USART_STM8L  = 0    ; Alternative STM8L USART GPIO mapping (e.g. PA3/PA2 instead of PC2/PC3)
;        CUARTBRR    = 0x6803    ; default value for UARTBRR 9600 baud (refer to mcu/UART_DIV)
        CUARTBRR    = 0x080B    ; default value for UARTBRR 115200 baud (refer to mcu/UART_DIV)        
        HAS_RXUART       = 1    ; Enable UART RXD, word ?RX
        HAS_TXUART       = 1    ; Enable UART TXD, word TX!
;        FLASHBUF_ADDR   = 0x340 ; buffer address for muforth

        HAS_RXSIM        = 0    ; Enable RxD via GPIO/TIM4, word ?RXGP
        HAS_TXSIM        = 0    ; like HAS_RXSIM, word TXGP!, use for console if > HAS_TXUART
        PSIM         = PORTX    ; Port for UART simulation
        PNRX             = 1    ; Port GPIO# for HAS_RXDSIM
        PNTX             = 1    ; Port GPIO# for HAS_TXDSIM
;        CTIM4ARR      = 0xCF    ; reload 104 µs (9600 baud) @ CTIM4PSCR=3
        CTIM4ARR      = 0x45    ; reload 17.4 µs (57600 baud) @ CTIM4PSCR=2
;        CTIM4PSCR        = 3    ; TIM4 prescaler 1/(2^n), n: (STM8S 0..7), (STM8L: 0..15)
        CTIM4PSCR        = 2    ; TIM4 prescaler 1/(2^n), n: (STM8S 0..7), (STM8L: 0..15)
        SERPRIOTIM       = 0    ; PSIM-PRNX priority to anything that uses that GPIO -> TIMRELOAD

        EMIT_BG  = DROP         ; vectored NUL background EMIT vector
        QKEY_BG  = ZERO         ; NUL background QKEY vector

        HAS_LED7SEG      = 0    ; 7-seg LED display, number of groups (0: none)
        LEN_7SGROUP      = 3    ; default: 3 dig. 7-seg LED

        HAS_KEYS         = 0    ; Board has keys
        HAS_OUTPUTS      = 0    ; Board outputs, e.g. relays
        HAS_INPUTS       = 0    ; Board digital inputs
        HAS_ADC          = 0    ; Board analog inputs

        HAS_BACKGROUND   = 1    ; Background Forth task (TIM1, TIM2 or TIM3 ticker)
        BG_USE_TIM1      = 0    ; Use TIM1 for the Background Task (instead of TIM2)
        BG_USE_TIM3      = 0    ; Use TIM3 for the Background Task (instead of TIM2)
        BG_TIM_REL  = 0x26DE    ; Reload value for Background Task timer (default for 5ms @ HSI 16MHz/8)
        BG_RUNMASK       = 0    ; BG task runs if "(BG_RUNMASK AND TICKCNT) equals 0"
        BSPPSIZE         = 32   ; Default size in bytes of data stack for background tasks

        HAS_CPNVM        = 0    ; Can compile to Flash, always interpret to RAM
        HAS_DOES         = 0    ; DOES> extension
        HAS_DOLOOP       = 0    ; DO .. LOOP extension: DO LEAVE LOOP +LOOP
        HAS_ALIAS        = 1    ; NAME> resolves "alias" (RigTig style), aliases can be in RAM
        HAS_FILEHAND     = 0    ; FILE and HAND for file upload
        HAS_OLDOK        = 0    ; Disable e4thcom file upload support

        USE_CALLDOLIT    = 1    ; use CALL DOLIT instead of the DOLIT TRAP handler (deprecated)
        CASEINSENSITIVE  = 0    ; Case insensitive dictionary search
        EXTNUMPREFIX     = 1    ; Extended number prefix: %: binary, &: decimal
        SPEEDOVERSIZE    = 0    ; Speed-over-size in core words ROT - = < -1 0 1
        MINIDICT         = 0    ; Dictionary in Flash and EEPROM, marks words for unlinking
        BAREBONES        = 0    ; Removes words: '?KEY 'EMIT EXIT EXG @EXECUTE ERASE
                                ;   Drops headers: ?RX TX! ?RXP ?RX TXP! TX! LAST DEPTH COUNT
                                ;     SPACES .R NAME> ABORT" AHEAD
                                ; Drops support for entry of binary (%) and decimal (&)
        BOOTSTRAP        = 0    ; Remove words: (+loop) EXIT 2! 2/ UM+ OR = MAX MIN U. . ? .(
                                ;  [COMPILE] FOR DO BEGIN WHILE ABORT" ." _TYPE dm+ DUMP .S
                                ;  .ID >CHAR <
        UNLINKCORE       = 0    ; Drops headers on everything except: (TODO)
                                ;  ABORT" AFT AGAIN AHEAD BEGIN DO DUMP ELSE EXG FOR IF LEAVE
                                ;  LOOP MAX MIN NEXT OR REPEAT SEE SPACES THEN U. U.R UM+
                                ;  UNTIL WHILE WORDS [COMPILE] _TYPE dm+
