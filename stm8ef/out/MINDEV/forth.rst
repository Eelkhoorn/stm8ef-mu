                                      1 ; STM8EF for STM8S (Value line and Access Line devices)
                                      2 ;
                                      3 ; This is derived work based on
                                      4 ; http://www.forth.org/svfig/kk/07-2010.html
                                      5 ;
                                      6 ; Please refer to LICENSE.md for more information.
                                      7 ;
                                      8 ;--------------------------------------------------------
                                      9 ; Original author, and copyright:
                                     10 ;       STM8EF, Version 2.1, 13jul10cht
                                     11 ;       Copyright (c) 2000
                                     12 ;       Dr. C. H. Ting
                                     13 ;       156 14th Avenue
                                     14 ;       San Mateo, CA 94402
                                     15 ;       (650) 571-7639
                                     16 ;
                                     17 ; Original main description:
                                     18 ;       FORTH Virtual Machine:
                                     19 ;       Subroutine threaded model
                                     20 ;       SP Return stack pointer
                                     21 ;       X Data stack pointer
                                     22 ;       A,Y Scratch pad registers
                                     23 ;
                                     24 ;--------------------------------------------------------
                                     25 ; The latest version of this code is available at
                                     26 ; https://github.com/TG9541/stm8ef
                                     27 ;
                                     28 ;
                                     29 ; Docs for the SDCC integrated assembler are scarce, thus
                                     30 ; SDCC was used to write the skeleton for this file.
                                     31 ; However, the code in this file isn't SDCC code.
                                     32 ;--------------------------------------------------------
                                     33 ; File Created by SDCC : free open source ANSI-C Compiler
                                     34 ; Version 3.6.0 #9615 (Linux)
                                     35 ;--------------------------------------------------------
                                     36 
                                     37         .module forth
                                     38         .optsdcc -mstm8
                                     39 
                                     40 ;--------------------------------------------------------
                                     41 ; Public variables in this module
                                     42 ;--------------------------------------------------------
                                     43 
                                     44 ;       .globl _TRAP_Handler
                                     45         .globl _forth
                                     46 
                                     47 ;--------------------------------------------------------
                                     48 ; ram data
                                     49 ;--------------------------------------------------------
                                     50         .area DATA
                                     51 
                                     52 ;--------------------------------------------------------
                                     53 ; ram data
                                     54 ;--------------------------------------------------------
                                     55         .area INITIALIZED
                                     56 ;--------------------------------------------------------
                                     57 ; absolute external ram data
                                     58 ;--------------------------------------------------------
                                     59         .area DABS (ABS)
                                     60 ;--------------------------------------------------------
                                     61 ; set RESET vector to COLD
                                     62 ;--------------------------------------------------------
      008000                         63          .org 0x8000
      008000 82 00                   64          .dw 0x8200
      008002 81 9D                   65          .dw _forth
                                     66 ;--------------------------------------------------------
                                     67 ; restore broken interrupt vector table of STM8L
                                     68 ;--------------------------------------------------------
                           000000    69          .ifeq (FAMILY - STM8L)
                                     70           .org 0x8070
                                     71           .dw 0x8200
                                     72           .dw _forth
                                     73           .dw 0x8200
                                     74           .dw _forth
                                     75           .dw 0x8200
                                     76           .dw _forth
                                     77           .dw 0x8200
                                     78           .dw _forth
                                     79          .if   HAS_RXUART*HAS_TXUART
                                     80           .org 0x8008   + ITC_IX_USART1_RXD * 4
                                     81           .dw 0x8200
                                     82           .dw UART_INT
                                     83          .endif
                                     84          .endif
                                     85 ;--------------------------------------------------------
                                     86 ; restore broken interrupt vector table of STM8S
                                     87 ;--------------------------------------------------------
                           000001    88          .ifeq (FAMILY - STM8S)
      008068                         89           .org 0x8068
      008068 82 00                   90           .dw 0x8200
      00806A 00 00                   91           .dw 0x0
                           000001    92          .if   HAS_RXUART*HAS_TXUART
      008050                         93           .org 0x8008   + ITC_IX_UART1RX * 4
      008050 82 00                   94           .dw 0x8200
      008052 81 F9                   95           .dw UART_INT
                                     96          .endif
                                     97          .endif
                                     98 
                                     99 ;.area CODE
                                    100 
                                    101 ;--------------------------------------------------------
                                    102 ; global & static initialisations
                                    103 ;--------------------------------------------------------
                                    104 ;        .area HOME
                                    105 ;        .area GSINIT
                                    106 ;        .area GSFINAL
                                    107 ;        .area GSINIT
                                    108 ;--------------------------------------------------------
                                    109 ; Home
                                    110 ;--------------------------------------------------------
                                    111 ;        .area HOME
                                    112 ;        .area HOME
                                    113 ;--------------------------------------------------------
                                    114 ; code
                                    115 ;--------------------------------------------------------
                                    116 ;        .area CODE
      00806C                        117         .org CODE_START
                                    118 
                                    119         ;************************************
                                    120         ;******  1) General Constants  ******
                                    121         ;************************************
                                    122 
                           000040   123         COMPO   =     0x40      ; "COMPO" lexicon compile only bit
                                    124 
                           000002   125         CELLL   =      2        ; size of a cell
                           000027   126         TIC     =     39        ; tick
                                    127 
                           000081   128         EXIT_OPC =    0x81      ; RET opcode
                           0000CC   129         BRAN_OPC =    0xCC      ; JP opcode
                           0000CD   130         CALL_OPC =    0xCD      ; CALL opcode
                                    131 
                                    132         ; Chip type (set of peripheral addresses and features)
                           000067   133         STM8S_LOD        = 103  ; STM8S Low Density
                           000069   134         STM8S_MED        = 105  ; STM8S Medium Density
                           0000CF   135         STM8S_HID        = 207  ; STM8S High Density
                           000033   136         STM8L_LOD        = 051  ; STM8L Low Density, RM0031 family
                           000065   137         STM8L_101        = 101  ; STM8L Low Density, RM0013 family
                           000098   138         STM8L_MHD        = 152  ; STM8L Medium and High Density
                                    139 
                                    140         ; STM8 family flags
                           000000   141         STM8S            = 0    ; FAMILY: STM8S device
                           000001   142         STM8L            = 1    ; FAMILY: STM8L device
                                    143 
                                    144         ; legacy chip type (deprecated - preferably use the chip type constants)
                           000065   145         STM8L101F3 = STM8L_101  ; L core, 8K flash incl EEPROM, 1.5K RAM, UART1
                           000033   146         STM8L051F3 = STM8L_LOD  ; L core, 8K flash, 1K RAM, 256 EEPROM, UART1
                           000098   147         STM8L151K4 = STM8L_MHD  ; L core, 32K flash, 2K RAM, 1K EEPROM, UART1
                           000098   148         STM8L152C6 = STM8L_MHD  ; L core, 32K flash, 2K RAM, 1K EEPROM, UART1
                           000098   149         STM8L152R8 = STM8L_MHD  ; L core, 64K flash, 4K RAM, 2K EEPROM, UART1
                           000067   150         STM8S003F3 = STM8S_LOD  ; 8K flash, 1K RAM, 128 EEPROM, UART1
                           000067   151         STM8S103F3 = STM8S_LOD  ; like STM8S003F3, 640 EEPROM
                           000069   152         STM8S105K4 = STM8S_MED  ; 16K/32K flash, 2K RAM, 1K EEPROM, UART2
                           0000CF   153         STM8S207RB = STM8S_HID  ; 32K+96K flash, 6K RAM, 2K EEPROM, UART1 or UART2
                                    154 
                           003E80   155         DEFOSCFREQ     = 16000  ; default oscillator frequency in kHz (HSI)
                                    156 
                                    157         ;********************************************
                                    158         ;******  2) Device hardware addresses  ******
                                    159         ;********************************************
                                    160 
                                    161         ;******  STM8 memory addresses ******
                           000000   162         RAMBASE =       0x0000  ; STM8 RAM start
                                    163 
                                    164         ; STM8 device specific include (provided by file in board folder)
                                    165         ; sets "TARGET" and memory layout
                                    166         .include        "target.inc"
                                      1 ;       STM8S103F3 device and memory layout configuration
                                      2 
                           000067     3         TARGET = STM8S103F3
                                      4 
                           0003FF     5         RAMEND =        0x03FF  ; "RAMEND" system (return) stack, growing down
                           004000     6         EEPROMBASE =    0x4000  ; "EESTART" EEPROM start address
                           00427F     7         EEPROMEND =     0x427F  ; "EEEND" 640 bytes EEPROM
                           00806C     8         CODE_START =	0x806C	; End of interrupt vector area
                           009FFF     9         FLASHEND =      0x9FFF  ; "FLASHEND" 8K devices
                           000340    10         FLASHBUF_ADDR = 0x0340  ; flash buffer address for muforth flash routine
                           000000    11         FORTHRAM =      0x0000  ; Start of RAM controlled by Forth
                           000000    12         UPPLOC  =       0x0000  ; UPP (user/system area) location for 1K RAM
                           0003D0    13         SPPLOC  =       0x03D0  ; SPP (data stack top), TIB start
                           0003FF    14         RPPLOC  =       RAMEND  ; RPP (return stack top)
                                     15 
                                    167 
                                    168         ; STM8 Flash Block Size (depends on "TARGET")
                           000001   169         .ifeq   (TARGET - STM8S_LOD) * (TARGET - STM8L_101) * (TARGET - STM8L_LOD)
                           000040   170           PAGESIZE   =     0x40      ; "PAGESIZE" STM8 Low Density: 64 byte page size
                           000000   171         .else
                                    172           PAGESIZE   =     0x80      ; "PAGESIZE" STM8 M/H Density: 128 byte page size
                                    173         .endif
                                    174 
                                    175         ; STM8 family register addresses (depends on "TARGET")
                           000001   176         .ifeq   (TARGET - STM8S_LOD) * (TARGET - STM8S_MED) * (TARGET - STM8S_HID)
                           000000   177           FAMILY = STM8S
                                    178           .include  "stm8device.inc"
                                      1 ; STM8S register addresses
                                      2 
                           005000     3         PORTA        = PA_ODR
                           005005     4         PORTB        = PB_ODR
                           00500A     5         PORTC        = PC_ODR
                           00500F     6         PORTD        = PD_ODR
                           005014     7         PORTE        = PE_ODR
                           005019     8         PORTF        = PF_ODR
                           00501E     9         PORTG        = PG_ODR
                           005023    10         PORTH        = PH_ODR
                           005028    11         PORTI        = PI_ODR
                           000000    12         PORTX        = 0
                                     13 
                           000000    14         ODR          = 0
                           000001    15         IDR          = 1
                           000002    16         DDR          = 2
                           000003    17         CR1          = 3
                           000004    18         CR2          = 4
                                     19 
                                     20 ; ***** Option Bytes in EEPROM
                                     21 
                           004800    22         OPT0         = 0x4800   ; Read-out protection                   (0x00)
                           004801    23         OPT1         = 0x4801   ; User Boot Code (UBC)                  (0x00)
                           004802    24         NOPT1        = 0x4802   ;                                       (0xFF)
                           004803    25         OPT2         = 0x4803   ; Alternate Function Mapping            (0x00)
                           004804    26         NOPT2        = 0x4804   ;                                       (0xFF)
                           004805    27         OPT3         = 0x4805   ; Watchdog option                       (0x00)
                           004806    28         NOPT3        = 0x4806   ;                                       (0xFF)
                           004807    29         OPT4         = 0x4807   ; Clock option                          (0x00)
                           004808    30         NOPT4        = 0x4808   ;                                       (0xFF)
                           004809    31         OPT5         = 0x4809   ; HSE Clock startup                     (0x00)
                           00480A    32         NOPT5        = 0x480A   ;                                       (0xFF)
                           00480B    33         OPT6         = 0x480B   ; TMU (Reserved)                        (0x00)
                           00480C    34         NOPT6        = 0x480C   ;                                       (0xFF)
                           00480D    35         OPT7         = 0x480D   ; Alternate Function Mapping            (0x00)
                           00480E    36         NOPT7        = 0x480E   ;                                       (0xFF)
                                     37 
                                     38 ;       OPT8         = 0x4810   ; TMU_KEY 1
                                     39 ;       ...
                                     40 ;       OPT16        = 0x4818   ; TMU_KEY 8
                                     41 
                           00487E    42         OPTBL        = 0x487E   ; Bootloader                            (0x00)
                           00487F    43         NOPTBL       = 0x487F   ;                                       (0xFF)
                                     44 
                                     45 ; ***** Port A
                                     46 
                           005000    47         PA_ODR       = 0x5000   ; Port A data output latch register     (0x00)
                           005001    48         PA_IDR       = 0x5001   ; Port A input pin value register       (0xXX)
                           005002    49         PA_DDR       = 0x5002   ; Port A data direction register        (0x00)
                           005003    50         PA_CR1       = 0x5003   ; Port A control register 1             (0x00)
                           005004    51         PA_CR2       = 0x5004   ; Port A control register 2             (0x00)
                                     52 
                                     53 ; ***** Port B
                                     54 
                           005005    55         PB_ODR       = 0x5005   ; Port B data output latch register     (0x00)
                           005006    56         PB_IDR       = 0x5006   ; Port B input pin value register       (0xXX)
                           005007    57         PB_DDR       = 0x5007   ; Port B data direction register        (0x00)
                           005008    58         PB_CR1       = 0x5008   ; Port B control register 1             (0x00)
                           005009    59         PB_CR2       = 0x5009   ; Port B control register 2             (0x00)
                                     60 
                                     61 ; ***** Port C
                                     62 
                           00500A    63         PC_ODR       = 0x500A   ; Port C data output latch register     (0x00)
                           00500B    64         PC_IDR       = 0x500B   ; Port C input pin value register       (0xXX)
                           00500C    65         PC_DDR       = 0x500C   ; Port C data direction register        (0x00)
                           00500D    66         PC_CR1       = 0x500D   ; Port C control register 1             (0x00)
                           00500E    67         PC_CR2       = 0x500E   ; Port C control register 2             (0x00)
                                     68 
                                     69 ; ***** Port D
                                     70 
                           00500F    71         PD_ODR       = 0x500F   ; Port D data output latch register     (0x00)
                           005010    72         PD_IDR       = 0x5010   ; Port D input pin value register       (0xXX)
                           005011    73         PD_DDR       = 0x5011   ; Port D data direction register        (0x00)
                           005012    74         PD_CR1       = 0x5012   ; Port D control register 1             (0x02)
                           005013    75         PD_CR2       = 0x5013   ; Port D control register 2             (0x00)
                                     76 
                                     77 ; ***** Port E
                                     78 ;.ifne  0
                           005014    79         PE_ODR       = 0x5014   ; Port E data output latch register     (0x00)
                           005015    80         PE_IDR       = 0x5015   ; Port E input pin value register       (0xXX)
                           005016    81         PE_DDR       = 0x5016   ; Port E data direction register        (0x00)
                           005017    82         PE_CR1       = 0x5017   ; Port E control register 1             (0x00)
                           005018    83         PE_CR2       = 0x5018   ; Port E control register 2             (0x00)
                                     84 ;.endif
                                     85 
                                     86 ; ***** Port F
                                     87 ;.ifne  0
                           005019    88         PF_ODR       = 0x5019   ; Port F data output latch register     (0x00)
                           00501A    89         PF_IDR       = 0x501A   ; Port F input pin value register       (0xXX)
                           00501B    90         PF_DDR       = 0x501B   ; Port F data direction register        (0x00)
                           00501C    91         PF_CR1       = 0x501C   ; Port F control register 1             (0x00)
                           00501D    92         PF_CR2       = 0x501D   ; Port F control register 2             (0x00)
                                     93 ;.endif
                                     94 
                                     95 ; ***** Port G
                                     96 ;.ifne  0
                           00501E    97         PG_ODR       = 0x501E   ; Port G data output latch register     (0x00)
                           00501F    98         PG_IDR       = 0x501F   ; Port G input pin value register       (0xXX)
                           005020    99         PG_DDR       = 0x5020   ; Port G data direction register        (0x00)
                           005021   100         PG_CR1       = 0x5021   ; Port G control register 1             (0x00)
                           005022   101         PG_CR2       = 0x5022   ; Port G control register 2             (0x00)
                                    102 ;.endif
                                    103 
                                    104 ; ***** Port H
                                    105 ;.ifne  0
                           005023   106         PH_ODR       = 0x5023   ; Port H data output latch register     (0x00)
                           005024   107         PH_IDR       = 0x5024   ; Port H input pin value register       (0xXX)
                           005025   108         PH_DDR       = 0x5025   ; Port H data direction register        (0x00)
                           005026   109         PH_CR1       = 0x5026   ; Port H control register 1             (0x00)
                           005027   110         PH_CR2       = 0x5027   ; Port H control register 2             (0x00)
                                    111 ;.endif
                                    112 
                                    113 ; ***** Port I
                                    114 ;.ifne  0
                           005028   115         PI_ODR       = 0x5028   ; Port I data output latch register     (0x00)
                           005029   116         PI_IDR       = 0x5029   ; Port I input pin value register       (0xXX)
                           00502A   117         PI_DDR       = 0x502A   ; Port I data direction register        (0x00)
                           00502B   118         PI_CR1       = 0x502B   ; Port I control register 1             (0x00)
                           00502C   119         PI_CR2       = 0x502C   ; Port I control register 2             (0x00)
                                    120 ;.endif
                                    121 
                                    122 ; ***** 6.2.2 General hardware register map
                                    123 
                                    124 ; ***** Flash
                                    125 ;.ifne  0
                           00505A   126         FLASH_CR1    = 0x505A   ; Flash control register 1              (0x00)
                           00505B   127         FLASH_CR2    = 0x505B   ; Flash control register 2              (0x00)
                           00505C   128         FLASH_NCR2   = 0x505C   ; Flash complementary control register 2 (0xFF)
                           00505D   129         FLASH_FPR    = 0x505D   ; Flash protection register             (0x00)
                           00505E   130         FLASH_NFPR   = 0x505E   ; Flash complementary protection register (0xFF)
                           00505F   131         FLASH_IAPSR  = 0x505F   ; Flash in-application programming status register (0x00)
                           005062   132         FLASH_PUKR   = 0x5062   ; Flash Program memory unprotection register (0x00)
                           005064   133         FLASH_DUKR   = 0x5064   ; Data EEPROM unprotection register     (0x00)
                                    134 ;.endif
                                    135 
                                    136 ; ***** ITC
                                    137 
                                    138 ;.ifne  0
                           0050A0   139         EXTI_CR1     = 0x50A0   ; External interrupt control register 1 (0x00)
                           0050A1   140         EXTI_CR2     = 0x50A1   ; External interrupt control register 2 (0x00)
                                    141 ;.endif
                                    142 
                                    143 ; ***** RST
                                    144 
                           0050B3   145         RST_SR       = 0x50B3   ; Reset status register                 (0xXX)
                                    146 
                                    147 ; ***** CLK
                                    148 
                                    149 ;.ifne  0
                           0050C0   150         CLK_ICKR     = 0x50C0   ; Internal clock control register       (0x01)
                           0050C1   151         CLK_ECKR     = 0x50C1   ; External clock control register       (0x00)
                           0050C3   152         CLK_CMSR     = 0x50C3   ; Clock master status register          (0xE1)
                           0050C4   153         CLK_SWR      = 0x50C4   ; Clock master switch register          (0xE1)
                           0050C5   154         CLK_SWCR     = 0x50C5   ; Clock switch control register         (0xXX)
                           0050C6   155         CLK_CKDIVR   = 0x50C6   ; Clock divider register                (0x18)
                           0050C7   156         CLK_PCKENR1  = 0x50C7   ; Peripheral clock gating register 1    (0xFF)
                           0050C8   157         CLK_CSSR     = 0x50C8   ; Clock security system register        (0x00)
                           0050C9   158         CLK_CCOR     = 0x50C9   ; Configurable clock control register   (0x00)
                           0050CA   159         CLK_PCKENR2  = 0x50CA   ; Peripheral clock gating register 2    (0xFF)
                           0050CB   160         CLK_CANCCR   = 0x50CB   ; CAN clock control register            (0x00)
                           0050CC   161         CLK_HSITRIMR = 0x50CC   ; HSI clock calibration trimming register (0x00)
                           0050CD   162         CLK_SWIMCCR  = 0x50CD   ; SWIM clock control register     (0bXXXXXXX0)
                                    163 ;.endif
                                    164 
                                    165 ; ***** SWIM
                                    166 
                                    167 ;.ifne  0
                           0050CD   168         CLK_SWIMCCR  = 0x50CD   ; clock control register                (0bXXXXXXX0)
                                    169 ;.endif
                                    170 
                                    171 ; ***** WWDG
                                    172 
                                    173 ;.ifne  0
                           0050D1   174         WWDG_CR      = 0x50D1   ; WWDG control register                 (0x7F)
                           0050D2   175         WWDG_WR      = 0x50D2   ; WWDR window register                  (0x7F)
                                    176 ;.endif
                                    177 
                                    178 ; ***** IWDG
                                    179 
                           0050E0   180         IWDG_KR      = 0x50E0   ; IWDG key register                     (0xXX)
                           0050E1   181         IWDG_PR      = 0x50E1   ; IWDG prescaler register               (0x00)
                           0050E2   182         IWDG_RLR     = 0x50E2   ; IWDG reload register                  (0xFF)
                                    183 
                                    184 ; ***** AWU
                                    185 
                           0050F0   186         AWU_CSR1     = 0x50F0   ; AWU control/status register 1         (0x00)
                           0050F1   187         AWU_APR      = 0x50F1   ; AWU asynchronous prescaler buffer register (0x3F)
                           0050F2   188         AWU_TBR      = 0x50F2   ; AWU timebase selection register       (0x00)
                                    189 
                                    190 ; ***** BEEP
                                    191 
                           0050F3   192         BEEP_CSR     = 0x50F3   ; BEEP control/status register          (0x1F)
                                    193 
                                    194 ; ***** SPI
                                    195 
                           005200   196         SPI_CR1      = 0x5200   ; SPI control register 1                (0x00)
                           005201   197         SPI_CR2      = 0x5201   ; SPI control register 2                (0x00)
                           005202   198         SPI_ICR      = 0x5202   ; SPI interrupt control register        (0x00)
                           005203   199         SPI_SR       = 0x5203   ; SPI status register                   (0x02)
                           005204   200         SPI_DR       = 0x5204   ; SPI data register                     (0x00)
                           005205   201         SPI_CRCPR    = 0x5205   ; SPI CRC polynomial register           (0x07)
                           005206   202         SPI_RXCRCR   = 0x5206   ; SPI Rx CRC register                   (0x00)
                           005207   203         SPI_TXCRCR   = 0x5207   ; SPI Tx CRC register                   (0x00)
                                    204 
                                    205 ; ***** I2C
                                    206 
                           005210   207         I2C_CR1      = 0x5210   ; I2C control register 1                (0x00)
                           005211   208         I2C_CR2      = 0x5211   ; I2C control register 2                (0x00)
                           005212   209         I2C_FREQR    = 0x5212   ; I2C frequency register                (0x00)
                           005213   210         I2C_OARL     = 0x5213   ; I2C own address register low          (0x00)
                           005214   211         I2C_OARH     = 0x5214   ; I2C own address register high         (0x00)
                           005216   212         I2C_DR       = 0x5216   ; I2C data register                     (0x00)
                           005217   213         I2C_SR1      = 0x5217   ; I2C status register 1                 (0x00)
                           005218   214         I2C_SR2      = 0x5218   ; I2C status register 2                 (0x00)
                           005219   215         I2C_SR3      = 0x5219   ; I2C status register 3                 (0x00)
                           00521A   216         I2C_ITR      = 0x521A   ; I2C interrupt control register        (0x00)
                           00521B   217         I2C_CCRL     = 0x521B   ; I2C clock control register low        (0x00)
                           00521C   218         I2C_CCRH     = 0x521C   ; I2C clock control register high       (0x00)
                           00521D   219         I2C_TRISER   = 0x521D   ; I2C TRISE register                    (0x02)
                           00521E   220         I2C_PECR     = 0x521E   ; I2C packet error checking register    (0x00)
                                    221 
                                    222 ; ***** UART1
                                    223 ;.ifeq HAS_TXUART+HAS_RXUART
                                    224 
                           005230   225         UART1_SR     = 0x5230   ; UART1 status register                 (0xC0)
                           005231   226         UART1_DR     = 0x5231   ; UART1 data register                   (0xXX)
                           005232   227         UART1_BRR1   = 0x5232   ; UART1 baud rate register 1            (0x00)
                           005233   228         UART1_BRR2   = 0x5233   ; UART1 baud rate register 2            (0x00)
                           005234   229         UART1_CR1    = 0x5234   ; UART1 control register 1              (0x00)
                           005235   230         UART1_CR2    = 0x5235   ; UART1 control register 2              (0x00)
                           005236   231         UART1_CR3    = 0x5236   ; UART1 control register 3              (0x00)
                           005237   232         UART1_CR4    = 0x5237   ; UART1 control register 4              (0x00)
                           005238   233         UART1_CR5    = 0x5238   ; UART1 control register 5              (0x00)
                           005239   234         UART1_GTR    = 0x5239   ; UART1 guard time register             (0x00)
                           00523A   235         UART1_PSCR   = 0x523A   ; UART1 prescaler register              (0x00)
                                    236 
                           005240   237         UART2_SR     = 0x5240   ; UART2 status register                 (0xC0)
                           005241   238         UART2_DR     = 0x5241   ; UART2 data register                   (0xXX)
                           005242   239         UART2_BRR1   = 0x5242   ; UART2 baud rate register 1            (0x00)
                           005243   240         UART2_BRR2   = 0x5243   ; UART2 baud rate register 2            (0x00)
                           005244   241         UART2_CR1    = 0x5244   ; UART2 control register 1              (0x00)
                           005245   242         UART2_CR2    = 0x5245   ; UART2 control register 2              (0x00)
                           005246   243         UART2_CR3    = 0x5246   ; UART2 control register 3              (0x00)
                           005247   244         UART2_CR4    = 0x5247   ; UART2 control register 4              (0x00)
                           005248   245         UART2_CR5    = 0x5248   ; UART2 control register 5              (0x00)
                           005249   246         UART2_CR6    = 0x5249   ; UART2 control register 6              (0x00)
                           00524A   247         UART2_GTR    = 0x524A   ; UART2 guard time register             (0x00)
                           00524B   248         UART2_PSCR   = 0x524B   ; UART2 prescaler register              (0x00)
                                    249 
                           000000   250         .ifeq   (TARGET - STM8S_MED) * (USE_UART2 - 1)
                                    251         UART_DR   = UART2_DR    ; STM8S105 or STM8S207
                                    252         UART_SR   = UART2_SR
                                    253         UART_BRR1 = UART2_BRR1
                                    254         UART_CR2  = UART2_CR2
                                    255         UART_CR5  = UART2_CR5
                           000001   256         .else
                           005231   257         UART_DR   = UART1_DR    ; STM8S103 or STM8S207
                           005230   258         UART_SR   = UART1_SR
                           005232   259         UART_BRR1 = UART1_BRR1
                           005235   260         UART_CR2  = UART1_CR2
                           005238   261         UART_CR5  = UART1_CR5
                                    262         .endif
                                    263 ;.endif
                                    264 
                                    265 ; ***** TIM1
                                    266 
                           005250   267         TIM1_CR1     = 0x5250   ; TIM1 control register 1               (0x00)
                           005251   268         TIM1_CR2     = 0x5251   ; TIM1 control register 2               (0x00)
                           005252   269         TIM1_SMCR    = 0x5252   ; TIM1 slave mode control register      (0x00)
                           005253   270         TIM1_ETR     = 0x5253   ; TIM1 external trigger register        (0x00)
                           005254   271         TIM1_IER     = 0x5254   ; TIM1 Interrupt enable register        (0x00)
                           005255   272         TIM1_SR1     = 0x5255   ; TIM1 status register 1                (0x00)
                           005256   273         TIM1_SR2     = 0x5256   ; TIM1 status register 2                (0x00)
                           005257   274         TIM1_EGR     = 0x5257   ; TIM1 event generation register        (0x00)
                           005258   275         TIM1_CCMR1   = 0x5258   ; TIM1 capture/compare mode register 1  (0x00)
                           005259   276         TIM1_CCMR2   = 0x5259   ; TIM1 capture/compare mode register 2  (0x00)
                           00525A   277         TIM1_CCMR3   = 0x525A   ; TIM1 capture/compare mode register 3  (0x00)
                           00525B   278         TIM1_CCMR4   = 0x525B   ; TIM1 capture/compare mode register 4  (0x00)
                           00525C   279         TIM1_CCER1   = 0x525C   ; TIM1 capture/compare enable register 1 (0x00)
                           00525D   280         TIM1_CCER2   = 0x525D   ; TIM1 capture/compare enable register 2 (0x00)
                           00525E   281         TIM1_CNTRH   = 0x525E   ; TIM1 counter high                     (0x00)
                           00525F   282         TIM1_CNTRL   = 0x525F   ; TIM1 counter low                      (0x00)
                           005260   283         TIM1_PSCRH   = 0x5260   ; TIM1 prescaler register high          (0x00)
                           005261   284         TIM1_PSCRL   = 0x5261   ; TIM1 prescaler register low           (0x00)
                           005262   285         TIM1_ARRH    = 0x5262   ; TIM1 auto-reload register high        (0xFF)
                           005263   286         TIM1_ARRL    = 0x5263   ; TIM1 auto-reload register low         (0xFF)
                           005264   287         TIM1_RCR     = 0x5264   ; TIM1 repetition counter register      (0x00)
                           005265   288         TIM1_CCR1H   = 0x5265   ; TIM1 capture/compare register 1 high  (0x00)
                           005266   289         TIM1_CCR1L   = 0x5266   ; TIM1 capture/compare register 1 low   (0x00)
                           005267   290         TIM1_CCR2H   = 0x5267   ; TIM1 capture/compare register 2 high  (0x00)
                           005268   291         TIM1_CCR2L   = 0x5268   ; TIM1 capture/compare register 2 low   (0x00)
                           005269   292         TIM1_CCR3H   = 0x5269   ; TIM1 capture/compare register 3 high  (0x00)
                           00526A   293         TIM1_CCR3L   = 0x526A   ; TIM1 capture/compare register 3 low   (0x00)
                           00526B   294         TIM1_CCR4H   = 0x526B   ; TIM1 capture/compare register 4 high  (0x00)
                           00526C   295         TIM1_CCR4L   = 0x526C   ; TIM1 capture/compare register 4 low   (0x00)
                           00526D   296         TIM1_BKR     = 0x526D   ; TIM1 break register                   (0x00)
                           00526E   297         TIM1_DTR     = 0x526E   ; TIM1 dead-time register               (0x00)
                           00526F   298         TIM1_OISR    = 0x526F   ; TIM1 output idle state register       (0x00)
                                    299 
                                    300 ; ***** TIM2
                                    301 
                           005300   302         TIM2_CR1     = 0x5300   ; TIM2 control register 1               (0x00)
                           000000   303         .ifne   (TARGET - STM8S_LOD)
                                    304         TIM2_IER     = 0x5301   ; TIM2 interrupt enable register        (0x00)
                                    305         TIM2_SR1     = 0x5302   ; TIM2 status register 1                (0x00)
                                    306         TIM2_SR2     = 0x5303   ; TIM2 status register 2                (0x00)
                                    307         TIM2_EGR     = 0x5304   ; TIM2 event generation register        (0x00)
                                    308         TIM2_CCMR1   = 0x5305   ; TIM2 capture/compare mode register 1  (0x00)
                                    309         TIM2_CCMR2   = 0x5306   ; TIM2 capture/compare mode register 2  (0x00)
                                    310         TIM2_CCMR3   = 0x5307   ; TIM2 capture/compare mode register 3  (0x00)
                                    311         TIM2_CCER1   = 0x5308   ; TIM2 capture/compare enable register 1 (0x00)
                                    312         TIM2_CCER2   = 0x5309   ; TIM2 capture/compare enable register 2 (0x00)
                                    313         TIM2_CNTRH   = 0x530A   ; TIM2 counter high                     (0x00)
                                    314         TIM2_CNTRL   = 0x530B   ; TIM2 counter low                      (0x00)
                                    315         TIM2_PSCR    = 0x530C   ; TIM2 prescaler register               (0x00)
                                    316         TIM2_ARRH    = 0x530D   ; TIM2 auto-reload register high        (0xFF)
                                    317         TIM2_ARRL    = 0x530E   ; TIM2 auto-reload register low         (0xFF)
                                    318         TIM2_CCR1H   = 0x530F   ; TIM2 capture/compare register 1 high  (0x00)
                                    319         TIM2_CCR1L   = 0x5310   ; TIM2 capture/compare register 1 low   (0x00)
                                    320         TIM2_CCR2H   = 0x5311   ; TIM2 capture/compare reg. 2 high      (0x00)
                                    321         TIM2_CCR2L   = 0x5312   ; TIM2 capture/compare register 2 low   (0x00)
                                    322         TIM2_CCR3H   = 0x5313   ; TIM2 capture/compare register 3 high  (0x00)
                                    323         TIM2_CCR3L   = 0x5314   ; TIM2 capture/compare register 3 low   (0x00)
                           000001   324         .else
                           005303   325         TIM2_IER     = 0x5303   ; TIM2 interrupt enable register        (0x00)
                           005304   326         TIM2_SR1     = 0x5304   ; TIM2 status register 1                (0x00)
                           005305   327         TIM2_SR2     = 0x5305   ; TIM2 status register 2                (0x00)
                           005306   328         TIM2_EGR     = 0x5306   ; TIM2 event generation register        (0x00)
                           005307   329         TIM2_CCMR1   = 0x5307   ; TIM2 capture/compare mode register 1  (0x00)
                           005308   330         TIM2_CCMR2   = 0x5308   ; TIM2 capture/compare mode register 2  (0x00)
                           005309   331         TIM2_CCMR3   = 0x5309   ; TIM2 capture/compare mode register 3  (0x00)
                           00530A   332         TIM2_CCER1   = 0x530A   ; TIM2 capture/compare enable register 1 (0x00)
                           00530B   333         TIM2_CCER2   = 0x530B   ; TIM2 capture/compare enable register 2 (0x00)
                           00530C   334         TIM2_CNTRH   = 0x530C   ; TIM2 counter high                     (0x00)
                           00530D   335         TIM2_CNTRL   = 0x530D   ; TIM2 counter low                      (0x00)
                           00530E   336         TIM2_PSCR    = 0x530E   ; TIM2 prescaler register               (0x00)
                           00530F   337         TIM2_ARRH    = 0x530F   ; TIM2 auto-reload register high        (0xFF)
                           005310   338         TIM2_ARRL    = 0x5310   ; TIM2 auto-reload register low         (0xFF)
                           005311   339         TIM2_CCR1H   = 0x5311   ; TIM2 capture/compare register 1 high  (0x00)
                           005312   340         TIM2_CCR1L   = 0x5312   ; TIM2 capture/compare register 1 low   (0x00)
                           005313   341         TIM2_CCR2H   = 0x5313   ; TIM2 capture/compare reg. 2 high      (0x00)
                           005314   342         TIM2_CCR2L   = 0x5314   ; TIM2 capture/compare register 2 low   (0x00)
                           005315   343         TIM2_CCR3H   = 0x5315   ; TIM2 capture/compare register 3 high  (0x00)
                           005316   344         TIM2_CCR3L   = 0x5316   ; TIM2 capture/compare register 3 low   (0x00)
                                    345         .endif
                                    346 
                                    347 ; ***** TIM3 (High Density)
                                    348 
                           005320   349         TIM3_CR1     = 0x5320   ; TIM3 control register 1               (0x00)
                           005321   350         TIM3_IER     = 0x5321   ; TIM3 interrupt enable register        (0x00)
                           005322   351         TIM3_SR1     = 0x5322   ; TIM3 status register 1                (0x00)
                           005323   352         TIM3_SR2     = 0x5323   ; TIM3 status register 2                (0x00)
                           005324   353         TIM3_EGR     = 0x5324   ; TIM3 event generation register        (0x00)
                           005325   354         TIM3_CCMR1   = 0x5325   ; TIM3 capture/compare mode register 1  (0x00)
                           005326   355         TIM3_CCMR2   = 0x5326   ; TIM3 capture/compare mode register 2  (0x00)
                           005327   356         TIM3_CCER1   = 0x5327   ; TIM3 capture/compare enable register 1 (0x00)
                           005328   357         TIM3_CNTRH   = 0x5328   ; TIM3 counter high                     (0x00)
                           005329   358         TIM3_CNTRL   = 0x5329   ; TIM3 counter low                      (0x00)
                           00532A   359         TIM3_PSCR    = 0x532A   ; TIM3 prescaler register               (0x00)
                           00532B   360         TIM3_ARRH    = 0x532B   ; TIM3 auto-reload register high        (0xFF)
                           00532C   361         TIM3_ARRL    = 0x532C   ; TIM3 auto-reload register low         (0xFF)
                           00532D   362         TIM3_CCR1H   = 0x532D   ; TIM3 capture/compare register 1 high  (0x00)
                           00532E   363         TIM3_CCR1L   = 0x532E   ; TIM3 capture/compare register 1 low   (0x00)
                           00532F   364         TIM3_CCR2H   = 0x532F   ; TIM3 capture/compare reg. 2 high      (0x00)
                           005330   365         TIM3_CCR2L   = 0x5330   ; TIM3 capture/compare register 2 low   (0x00)
                                    366 
                                    367 ; ***** TIM4
                                    368 
                                    369 ;.ifeq HAS_TXSIM+HAS_RXSIM
                                    370 
                           005340   371         TIM4_CR1     = 0x5340   ; TIM4 control register 1               (0x00)
                           000000   372         .ifeq   (TARGET - STM8S_MED) * (TARGET - STM8S_HID)
                                    373         TIM4_IER     = 0x5341   ; TIM4 interrupt enable register        (0x00)
                                    374         TIM4_SR      = 0x5342   ; TIM4 status register                  (0x00)
                                    375         TIM4_EGR     = 0x5343   ; TIM4 event generation register        (0x00)
                                    376         TIM4_CNTR    = 0x5344   ; TIM4 counter                          (0x00)
                                    377         TIM4_PSCR    = 0x5345   ; TIM4 prescaler register               (0x00)
                                    378         TIM4_ARR     = 0x5346   ; TIM4 auto-reload register             (0xFF)
                           000001   379         .else
                           005343   380         TIM4_IER     = 0x5343   ; TIM4 interrupt enable register        (0x00)
                           005344   381         TIM4_SR      = 0x5344   ; TIM4 status register                  (0x00)
                           005345   382         TIM4_EGR     = 0x5345   ; TIM4 event generation register        (0x00)
                           005346   383         TIM4_CNTR    = 0x5346   ; TIM4 counter                          (0x00)
                           005347   384         TIM4_PSCR    = 0x5347   ; TIM4 prescaler register               (0x00)
                           005348   385         TIM4_ARR     = 0x5348   ; TIM4 auto-reload register             (0xFF)
                                    386         .endif
                                    387 ;.endif
                                    388 
                                    389 ; define symbols for the background task timer
                           000000   390         .ifne   BG_USE_TIM1
                                    391         .ifne   BG_USE_TIM3
                                    392         Error: either BG_USE_TIM1 or BG_USE_TIM3 can be selected but not both
                                    393         .else
                                    394         BG_TIM_CR1   = TIM1_CR1
                                    395         BG_TIM_IER   = TIM1_IER
                                    396         BG_TIM_SR1   = TIM1_SR1
                                    397         BG_TIM_ARRH  = TIM1_ARRH
                                    398         BG_TIM_ARRL  = TIM1_ARRL
                                    399         .endif
                           000001   400         .else
                           000000   401         .ifne   BG_USE_TIM3
                                    402         BG_TIM_CR1   = TIM3_CR1
                                    403         BG_TIM_IER   = TIM3_IER
                                    404         BG_TIM_SR1   = TIM3_SR1
                                    405         BG_TIM_ARRH  = TIM3_ARRH
                                    406         BG_TIM_ARRL  = TIM3_ARRL
                                    407         BG_TIM_PSCR  = TIM3_PSCR
                           000001   408         .else
                           005300   409         BG_TIM_CR1   = TIM2_CR1
                           005303   410         BG_TIM_IER   = TIM2_IER
                           005304   411         BG_TIM_SR1   = TIM2_SR1
                           00530F   412         BG_TIM_ARRH  = TIM2_ARRH
                           005310   413         BG_TIM_ARRL  = TIM2_ARRL
                           00530E   414         BG_TIM_PSCR  = TIM2_PSCR
                                    415         .endif
                                    416         .endif
                                    417 
                                    418 ; ***** ADC1
                                    419 
                                    420 ;.ifeq HAS_ADC
                                    421 
                           0053E0   422         ADC_DBxR     = 0x53E0   ; ADC data buffer registers 0x53E0 to 0x53F3 (0x00)
                                    423         ; High Density ADC2 start
                           005400   424         ADC_CSR      = 0x5400   ; ADC control/status register           (0x00)
                           005401   425         ADC_CR1      = 0x5401   ; ADC configuration register 1          (0x00)
                           005402   426         ADC_CR2      = 0x5402   ; ADC configuration register 2          (0x00)
                           005403   427         ADC_CR3      = 0x5403   ; ADC configuration register 3          (0x00)
                           005404   428         ADC_DRH      = 0x5404   ; ADC data register high                (0xXX)
                           005405   429         ADC_DRL      = 0x5405   ; ADC data register low                 (0xXX)
                           005406   430         ADC_TDRH     = 0x5406   ; ADC Schmitt trigger disable register high (0x00)
                           005407   431         ADC_TDRL     = 0x5407   ; ADC Schmitt trigger disable register low (0x00)
                                    432         ; High Density ADC2 end
                           005408   433         ADC_HTRH     = 0x5408   ; ADC high threshold register high      (0x03)
                           005409   434         ADC_HTRL     = 0x5409   ; ADC high threshold register low       (0xFF)
                           00540A   435         ADC_LTRH     = 0x540A   ; ADC low threshold register high       (0x00)
                           00540B   436         ADC_LTRL     = 0x540B   ; ADC low threshold register low        (0x00)
                           00540C   437         ADC_AWSRH    = 0x540C   ; ADC analog watchdog status register high (0x00)
                           00540D   438         ADC_AWSRL    = 0x540D   ; ADC analog watchdog status register low (0x00)
                           00540E   439         ADC_AWCRH    = 0x540E   ; ADC analog watchdog control register high (0x00)
                           00540F   440         ADC_AWCRL    = 0x540F   ; ADC analog watchdog control register low (0x00)
                                    441 ;.endif
                                    442 
                           005420   443         CAN_MCR      = 0x5420 ; CAN master control register 0x02
                           005421   444         CAN_MSR      = 0x5421 ; CAN master status register 0x02
                           005422   445         CAN_TSR      = 0x5422 ; CAN transmit status register 0x00
                           005423   446         CAN_TPR      = 0x5423 ; CAN transmit priority register 0x0C
                           005424   447         CAN_RFR      = 0x5424 ; CAN receive FIFO register 0x00
                           005425   448         CAN_IER      = 0x5425 ; CAN interrupt enable register 0x00
                           005426   449         CAN_DGR      = 0x5426 ; CAN diagnosis register 0x0C
                           005427   450         CAN_FPSR     = 0x5427 ; CAN page selection register 0x00
                                    451 
                           005428   452         CAN_P0       = 0x5428 ; CAN paged register 0 0xXX (3)
                           005429   453         CAN_P1       = 0x5429 ; CAN paged register 1 0xXX (3)
                           00542A   454         CAN_P2       = 0x542A ; CAN paged register 2 0xXX (3)
                           00542B   455         CAN_P3       = 0x542B ; CAN paged register 3 0xXX (3)
                           00542C   456         CAN_P4       = 0x542C ; CAN paged register 4 0xXX (3)
                           00542D   457         CAN_P5       = 0x542D ; CAN paged register 5 0xXX (3)
                           00542E   458         CAN_P6       = 0x542E ; CAN paged register 6 0xXX (3)
                           00542F   459         CAN_P7       = 0x542F ; CAN paged register 7 0xXX (3)
                           005430   460         CAN_P8       = 0x5430 ; CAN paged register 8 0xXX (3)
                           005431   461         CAN_P9       = 0x5431 ; CAN paged register 9 0xXX (3)
                           005432   462         CAN_PA       = 0x5432 ; CAN paged register A 0xXX (3)
                           005433   463         CAN_PB       = 0x5433 ; CAN paged register B 0xXX (3)
                           005434   464         CAN_PC       = 0x5434 ; CAN paged register C 0xXX (3)
                           005435   465         CAN_PD       = 0x5435 ; CAN paged register D 0xXX (3)
                           005436   466         CAN_PE       = 0x5436 ; CAN paged register E 0xXX (3)
                           005437   467         CAN_PF       = 0x5437 ; CAN paged register F 0xXX (3)
                                    468 
                                    469 ; ***** 6.2.3 CPU/SWIM/debug module/interrupt controller registers
                                    470 
                                    471 
                                    472 ; ***** CPU
                                    473 
                                    474 ;.ifne  0
                           007F00   475         CPU_A        = 0x7F00   ; Accumulator                           (0x00)
                           007F01   476         CPU_PCE      = 0x7F01   ; Program counter extended              (0x00)
                           007F02   477         CPU_PCH      = 0x7F02   ; Program counter high                  (0x00)
                           007F03   478         CPU_PCL      = 0x7F03   ; Program counter low                   (0x00)
                           007F04   479         CPU_XH       = 0x7F04   ; X index register high                 (0x00)
                           007F05   480         CPU_XL       = 0x7F05   ; X index register low                  (0x00)
                           007F06   481         CPU_YH       = 0x7F06   ; Y index register high                 (0x00)
                           007F07   482         CPU_YL       = 0x7F07   ; Y index register low                  (0x00)
                           007F08   483         CPU_SPH      = 0x7F08   ; Stack pointer high                    (0x03)
                           007F09   484         CPU_SPL      = 0x7F09   ; Stack pointer low                     (0xFF)
                           007F0A   485         CPU_CCR      = 0x7F0A   ; Condition code register               (0x28)
                                    486 ;.endif
                                    487 
                                    488 ; ***** CFG
                                    489 
                           007F60   490         CFG_GCR      = 0x7F60   ; Global configuration register         (0x00)
                                    491 
                                    492 ; ***** ITC
                                    493 
                           000000   494         ITC_IX_TLI   = 0        ; External top level interrupt
                           000001   495         ITC_IX_AWU   = 1        ; Auto wake up from halt
                           000002   496         ITC_IX_CLK   = 2        ; Clock controller
                           000003   497         ITC_IX_EXTI0 = 3        ; Port A external interrupts
                           000004   498         ITC_IX_EXTI1 = 4        ; Port B external interrupts
                           000005   499         ITC_IX_EXTI2 = 5        ; Port C external interrupts
                           000006   500         ITC_IX_EXTI3 = 6        ; Port D external interrupts
                           000007   501         ITC_IX_EXTI4 = 7        ; Port E external interrupts
                           000008   502         ITC_IX_CANRX = 8        ; beCAN RX
                           000009   503         ITC_IX_CANTX = 9        ; beCAN TX
                           00000A   504         ITC_IX_SPI   = 10       ; End of transfer
                           00000B   505         ITC_IX_TIM1  = 11       ; TIM1 update/overflow/underflow/ trigger/break
                           00000C   506         ITC_IX_TIM1CC = 12      ; TIM1 capture/compare
                           00000D   507         ITC_IX_TIM2  = 13       ; TIM2 update /overflow
                           00000E   508         ITC_IX_TIM2CC = 14      ; TIM2 capture/compare
                           00000F   509         ITC_IX_TIM3  = 15       ; TIM3 update /overflow
                           000010   510         ITC_IX_TIM3CC = 16      ; TIM3 capture/compare
                           000011   511         ITC_IX_UART1TX = 17     ; LD/HD UART1 Tx complete
                           000012   512         ITC_IX_UART1RX = 18     ; LD/HD UART1 Receive register DATA FULL
                           000013   513         ITC_IX_I2C   = 19       ; I2C interrupt
                           000014   514         ITC_IX_UART2TX = 20     ; MD/HD UART Tx complete
                           000015   515         ITC_IX_UART2RX = 21     ; MD/HD UART Receive register DATA FULL
                           000016   516         ITC_IX_ADC1  = 22       ; ADC1 end of conversion/analog watchdog interrupt
                           000017   517         ITC_IX_TIM4  = 23       ; TIM4 update/overflow
                           000018   518         ITC_IX_FLASH = 24       ; Flash EOP/WR_PG_DIS
                                    519 
                                    520 ;.ifne  HAS_CPNVM
                           007F70   521         ITC_SPR1     = 0x7F70   ; Interrupt software priority register 1 (0xFF)
                           007F71   522         ITC_SPR2     = 0x7F71   ; Interrupt software priority register 2 (0xFF)
                           007F72   523         ITC_SPR3     = 0x7F72   ; Interrupt software priority register 3 (0xFF)
                           007F73   524         ITC_SPR4     = 0x7F73   ; Interrupt software priority register 4 (0xFF)
                           007F74   525         ITC_SPR5     = 0x7F74   ; Interrupt software priority register 5 (0xFF)
                           007F75   526         ITC_SPR6     = 0x7F75   ; Interrupt software priority register 6 (0xFF)
                           007F76   527         ITC_SPR7     = 0x7F76   ; Interrupt software priority register 7 (0xFF)
                           007F77   528         ITC_SPR8     = 0x7F77   ; Interrupt software priority register 8 (0xFF)
                                    529 ;.endif
                                    530 
                                    531 ; ***** SWIM
                                    532 
                                    533 ;.ifne  0
                           007F80   534         SWIM_CSR     = 0x7F80   ; SWIM control status register          (0x00)
                                    535 ;.endif
                                    536 
                                    537 ; ***** DM
                                    538 
                                    539 ;.ifne  0
                           007F90   540         DM_BK1RE     = 0x7F90   ; DM breakpoint 1 register extended byte (0xFF)
                           007F91   541         DM_BK1RH     = 0x7F91   ; DM breakpoint 1 register high byte    (0xFF)
                           007F92   542         DM_BK1RL     = 0x7F92   ; DM breakpoint 1 register low byte     (0xFF)
                           007F93   543         DM_BK2RE     = 0x7F93   ; DM breakpoint 2 register extended byte (0xFF)
                           007F94   544         DM_BK2RH     = 0x7F94   ; DM breakpoint 2 register high byte    (0xFF)
                           007F95   545         DM_BK2RL     = 0x7F95   ; DM breakpoint 2 register low byte     (0xFF)
                           007F96   546         DM_CR1       = 0x7F96   ; DM debug module control register 1    (0x00)
                           007F97   547         DM_CR2       = 0x7F97   ; DM debug module control register 2    (0x00)
                           007F98   548         DM_CSR1      = 0x7F98   ; DM debug module control/status register 1 (0x10)
                           007F99   549         DM_CSR2      = 0x7F99   ; DM debug module control/status register 2 (0x00)
                           007F9A   550         DM_ENFCTR    = 0x7F9A   ; DM enable function register           (0xFF)
                                    551 ;.endif
                                    179         .endif
                           000000   180         .ifeq   (TARGET - STM8L_101) * (TARGET - STM8L_LOD) * (TARGET - STM8L_MHD)
                                    181           FAMILY = STM8L
                                    182           .include  "stm8ldevice.inc"
                                    183         .endif
                                    184 
                                    185 
                                    186         ;**********************************
                                    187         ;******  3) Global defaults  ******
                                    188         ;**********************************
                                    189         ; Note: add defaults for new features here
                                    190         ;       and configure them in globconf.inc
                                    191 
                                    192         .include  "defconf.inc"
                                      1 ;--------------------------------------------------------
                                      2 ;       STM8 eForth for STM8S and STM8L devices
                                      3 ;       Default settings for all options
                                      4 ;--------------------------------------------------------
                           000002     5         RELVER1          = 2    ; Revision digit 1
                           000009     6         RELVER0          = 9   ; Revision digit 0
                           000000     7         PRE_REL          = 0    ; Pre Release digit 0 (0: release)
                                      8 
                           000001     9         TERM_LINUX       = 1    ; LF terminates line
                                     10 
                           000000    11         HALF_DUPLEX      = 0    ; Use the STM8S Low Density UART in half duplex mode (1: PD5, 2: PA3)
                           000000    12         USE_UART2        = 0    ; Use the 2nd UART for the console (STM8S207: optional, STM8S105: default, STM8L152: optional)
                           000000    13         USE_UART3        = 0    ; Use the 3rd UART for the console (STM8L152: optional)
                           000000    14         ALT_USART_STM8L  = 0    ; Alternative STM8L USART GPIO mapping (e.g. PA3/PA2 instead of PC2/PC3)
                                     15 ;        CUARTBRR    = 0x6803    ; default value for UARTBRR 9600 baud (refer to mcu/UART_DIV)
                           00080B    16         CUARTBRR    = 0x080B    ; default value for UARTBRR 115200 baud (refer to mcu/UART_DIV)        
                           000001    17         HAS_RXUART       = 1    ; Enable UART RXD, word ?RX
                           000001    18         HAS_TXUART       = 1    ; Enable UART TXD, word TX!
                                     19 ;        FLASHBUF_ADDR   = 0x340 ; buffer address for muforth
                                     20 
                           000000    21         HAS_RXSIM        = 0    ; Enable RxD via GPIO/TIM4, word ?RXGP
                           000000    22         HAS_TXSIM        = 0    ; like HAS_RXSIM, word TXGP!, use for console if > HAS_TXUART
                           000000    23         PSIM         = PORTX    ; Port for UART simulation
                           000001    24         PNRX             = 1    ; Port GPIO# for HAS_RXDSIM
                           000001    25         PNTX             = 1    ; Port GPIO# for HAS_TXDSIM
                                     26 ;        CTIM4ARR      = 0xCF    ; reload 104 µs (9600 baud) @ CTIM4PSCR=3
                           000045    27         CTIM4ARR      = 0x45    ; reload 17.4 µs (57600 baud) @ CTIM4PSCR=2
                                     28 ;        CTIM4PSCR        = 3    ; TIM4 prescaler 1/(2^n), n: (STM8S 0..7), (STM8L: 0..15)
                           000002    29         CTIM4PSCR        = 2    ; TIM4 prescaler 1/(2^n), n: (STM8S 0..7), (STM8L: 0..15)
                           000000    30         SERPRIOTIM       = 0    ; PSIM-PRNX priority to anything that uses that GPIO -> TIMRELOAD
                                     31 
                           00832B    32         EMIT_BG  = DROP         ; vectored NUL background EMIT vector
                           008395    33         QKEY_BG  = ZERO         ; NUL background QKEY vector
                                     34 
                           000000    35         HAS_LED7SEG      = 0    ; 7-seg LED display, number of groups (0: none)
                           000003    36         LEN_7SGROUP      = 3    ; default: 3 dig. 7-seg LED
                                     37 
                           000000    38         HAS_KEYS         = 0    ; Board has keys
                           000000    39         HAS_OUTPUTS      = 0    ; Board outputs, e.g. relays
                           000000    40         HAS_INPUTS       = 0    ; Board digital inputs
                           000000    41         HAS_ADC          = 0    ; Board analog inputs
                                     42 
                           000001    43         HAS_BACKGROUND   = 1    ; Background Forth task (TIM1, TIM2 or TIM3 ticker)
                           000000    44         BG_USE_TIM1      = 0    ; Use TIM1 for the Background Task (instead of TIM2)
                           000000    45         BG_USE_TIM3      = 0    ; Use TIM3 for the Background Task (instead of TIM2)
                           0026DE    46         BG_TIM_REL  = 0x26DE    ; Reload value for Background Task timer (default for 5ms @ HSI 16MHz/8)
                           000000    47         BG_RUNMASK       = 0    ; BG task runs if "(BG_RUNMASK AND TICKCNT) equals 0"
                           000020    48         BSPPSIZE         = 32   ; Default size in bytes of data stack for background tasks
                                     49 
                           000000    50         HAS_CPNVM        = 0    ; Can compile to Flash, always interpret to RAM
                           000000    51         HAS_DOES         = 0    ; DOES> extension
                           000000    52         HAS_DOLOOP       = 0    ; DO .. LOOP extension: DO LEAVE LOOP +LOOP
                           000001    53         HAS_ALIAS        = 1    ; NAME> resolves "alias" (RigTig style), aliases can be in RAM
                           000000    54         HAS_FILEHAND     = 0    ; FILE and HAND for file upload
                           000000    55         HAS_OLDOK        = 0    ; Disable e4thcom file upload support
                                     56 
                           000001    57         USE_CALLDOLIT    = 1    ; use CALL DOLIT instead of the DOLIT TRAP handler (deprecated)
                           000000    58         CASEINSENSITIVE  = 0    ; Case insensitive dictionary search
                           000001    59         EXTNUMPREFIX     = 1    ; Extended number prefix: %: binary, &: decimal
                           000000    60         SPEEDOVERSIZE    = 0    ; Speed-over-size in core words ROT - = < -1 0 1
                           000000    61         MINIDICT         = 0    ; Dictionary in Flash and EEPROM, marks words for unlinking
                           000000    62         BAREBONES        = 0    ; Removes words: '?KEY 'EMIT EXIT EXG @EXECUTE ERASE
                                     63                                 ;   Drops headers: ?RX TX! ?RXP ?RX TXP! TX! LAST DEPTH COUNT
                                     64                                 ;     SPACES .R NAME> ABORT" AHEAD
                                     65                                 ; Drops support for entry of binary (%) and decimal (&)
                           000000    66         BOOTSTRAP        = 0    ; Remove words: (+loop) EXIT 2! 2/ UM+ OR = MAX MIN U. . ? .(
                                     67                                 ;  [COMPILE] FOR DO BEGIN WHILE ABORT" ." _TYPE dm+ DUMP .S
                                     68                                 ;  .ID >CHAR <
                           000000    69         UNLINKCORE       = 0    ; Drops headers on everything except: (TODO)
                                     70                                 ;  ABORT" AFT AGAIN AHEAD BEGIN DO DUMP ELSE EXG FOR IF LEAVE
                                     71                                 ;  LOOP MAX MIN NEXT OR REPEAT SEE SPACES THEN U. U.R UM+
                                     72                                 ;  UNTIL WHILE WORDS [COMPILE] _TYPE dm+
                                    193 
                                    194         ;********************************************
                                    195         ;******  4) Device dependent features  ******
                                    196         ;********************************************
                                    197         ; Define memory location for device dependent features here
                                    198 
                                    199         .include "globconf.inc"
                                      1 ; STM8EF Global Configuration File
                                      2 ; Config for STM8S103F3P6 Minmal Development Board
                                      3 ; Clock: HSI (no crystal)
                                      4 
                           000001     5         MUFORTH          = 1    ; No compiler nor interpreter
                           000000     6         HALF_DUPLEX      = 0    ; Use UART in half duplex mode
                           000001     7         HAS_TXUART       = 1    ; No UART TXD, word TX!
                           000001     8         HAS_RXUART       = 1    ; No UART RXD, word ?RX
                           000000     9         HAS_TXSIM        = 0    ; Enable TxD via GPIO/TIM4, word TXGP!
                           000000    10         HAS_RXSIM        = 0    ; Enable RxD via GPIO/TIM4, word ?RXGP
                           000000    11         PNRX             = 0    ; Port GPIO# for HAS_RXDSIM
                           000000    12         PNTX             = 0    ; Port GPIO# for HAS_TXDSIM
                                     13 
                           00832B    14         EMIT_BG  = DROP         ; 7S-LED background EMIT vector
                           008395    15         QKEY_BG  = ZERO         ; Board keys background QKEY vector
                                     16 
                           000000    17         HAS_LED7SEG      = 0    ; no 7S-Display
                           000000    18         HAS_KEYS         = 0    ; no keys on module
                           000001    19         HAS_OUTPUTS      = 1    ; yes, one LED
                           000001    20         HAS_ADC          = 1    ; Analog input words
                                     21 
                           000001    22         HAS_BACKGROUND   = 1    ; Background Forth task (TIM2 ticker)
                           000001    23         HAS_CPNVM        = 1    ; Can compile to Flash, always interpret to RAM
                           000001    24         HAS_DOES         = 1    ; CREATE-DOES> extension
                           000001    25         HAS_DOLOOP       = 1    ; DO .. LOOP extension: DO LEAVE LOOP +LOOP
                                     26 
                                     27 
                           000001    28         CASEINSENSITIVE  = 1    ; Case insensitive dictionary search
                           000001    29         SPEEDOVERSIZE    = 1    ; Speed-over-size in core words: ROT - = <
                           000000    30         BAREBONES        = 0    ; Remove or unlink some more: hi HERE .R U.R SPACES @EXECUTE AHEAD CALL, EXIT COMPILE [COMPILE]
                                     31 
                           000000    32         WORDS_LINKINTER  = 0    ; Link interpreter words: ACCEPT QUERY TAP kTAP hi 'BOOT tmp >IN 'TIB #TIB eval CONTEXT pars PARSE NUMBER? DIGIT? WORD TOKEN NAME> SAME? find ABORT aborq $INTERPRET INTER? .OK ?STACK EVAL PRESET QUIT $COMPILE
                           000000    33         WORDS_LINKCOMP   = 0    ; Link compiler words: cp last OVERT $"| ."| $,n
                           000000    34         WORDS_LINKRUNTI  = 0    ; Link runtime words: doLit do$ doVAR donxt dodoes ?branch branch
                           000001    35         WORDS_LINKCHAR   = 1    ; Link char out words: DIGIT <# # #S SIGN #> str hld HOLD
                           000000    36         WORDS_LINKMISC   = 0    ; Link composing words of SEE DUMP WORDS: >CHAR _TYPE dm+ .ID >NAME
                                     37 
                           000000    38         WORDS_EXTRASTACK = 0    ; Link/include stack core words: rp@ rp! sp! sp@ DEPTH
                           000000    39         WORDS_EXTRADEBUG = 0    ; Extra debug words: SEE
                           000001    40         WORDS_EXTRACORE  = 1    ; Extra core words: =0 I
                           000001    41         WORDS_EXTRAMEM   = 1    ; Extra memory words: B! 2C@ 2C!
                           000001    42         WORDS_EXTRAEEPR  = 1    ; Extra EEPROM lock/unlock words: LOCK ULOCK ULOCKF LOCKF
                                    200 
                                    201         ; .include "linkopts.inc"
                                    202 
                                    203         ; console configuration: check if TX simulation has priority over UART
                           000000   204         .ifge   HAS_TXSIM - HAS_TXUART
                                    205         .ifeq  PNTX-PNRX
                                    206         CONSOLE_HALF_DUPLEX = 1 ; single wire RX/TX simulation is half duplex
                                    207         .else
                                    208         CONSOLE_HALF_DUPLEX = 0 ; RX/TX simulation supports full duplex
                                    209         .endif
                           000001   210         .else
                           000000   211         CONSOLE_HALF_DUPLEX = HALF_DUPLEX ; use hardware UART settings
                                    212         .endif
                                    213 
                           003E80   214         OSCFREQ   = DEFOSCFREQ  ; "OSCFREQ" oscillator frequency in kHz
                           000000   215         CRAMLEN   = FORTHRAM    ; "CRAMLEN" RAM starting from 0 not used by Forth
                                    216 
                                    217         ;**************************************
                                    218         ;******  5) Board Driver Memory  ******
                                    219         ;**************************************
                                    220         ; Memory for board related code, e.g. interrupt routines
                                    221 
                           000000   222         RAMPOOL =    FORTHRAM   ; RAM for variables (growing up)
                                    223 
                                    224         .macro  RamByte varname
                                    225         varname = RAMPOOL
                                    226         RAMPOOL = RAMPOOL + 1
                                    227         .endm
                                    228 
                                    229         .macro  RamWord varname
                                    230         varname = RAMPOOL
                                    231         RAMPOOL = RAMPOOL + 2
                                    232         .endm
                                    233 
                                    234         .macro  RamBlck varname, size
                                    235         varname = RAMPOOL
                                    236         RAMPOOL = RAMPOOL + size
                                    237         .endm
                                    238 
                                    239 
                                    240         ;**************************************************
                                    241         ;******  6) General User & System Variables  ******
                                    242         ;**************************************************
                                    243 
                                    244         ; ****** Indirect variables for code in NVM *****
                           000001   245         .ifne   HAS_CPNVM
                           000010   246         ISPPSIZE  =     16      ; Size of data stack for interrupt tasks
                           000000   247         .else
                                    248         ISPPSIZE  =     0       ; no interrupt tasks without NVM
                                    249         .endif
                                    250 
                           0003A0   251         SPP   = ISPP-ISPPSIZE   ; "SPP"  data stack, growing down (with SPP-1 first)
                           0003B0   252         ISPP  = SPPLOC-BSPPSIZE
                           0003D0   253         BSPP  = SPPLOC          ; "BSPP" Background data stack, growing down
                           0003FF   254         RPP   = RPPLOC          ; "RPP"  constant addr. return stack, growing down
                                    255 
                                    256         ; Core variables (same order as 'BOOT initializer block)
                                    257 
                                    258 ;        USRRAMINIT = USREMIT
                                    259 
      00806C                        260         RamWord USREMIT         ; "'EMIT" execution vector of EMIT
                           000000     1         USREMIT = RAMPOOL
                           000002     2         RAMPOOL = RAMPOOL + 2
      00806C                        261         RamWord USRQKEY         ; "'?KEY" execution vector of QKEY
                           000002     1         USRQKEY = RAMPOOL
                           000004     2         RAMPOOL = RAMPOOL + 2
                           000000   262 .if  HAS_RXSIM
                                    263         RamByte USR_5           ; chat variables
                                    264         RamByte USR_6           ;
                                    265 .endif
      00806C                        266         RamWord MP              ; memory pointer for mu-chat
                           000004     1         MP = RAMPOOL
                           000006     2         RAMPOOL = RAMPOOL + 2
                                    267 
                                    268         ; More core variables in zero page (instead of assigning fixed addresses)
      00806C                        269         RamWord USRHLD          ; "HLD" hold a pointer of output string
                           000006     1         USRHLD = RAMPOOL
                           000008     2         RAMPOOL = RAMPOOL + 2
      00806C                        270         RamByte XREG0           ; extra working register for core words
                           000008     1         XREG0 = RAMPOOL
                           000009     2         RAMPOOL = RAMPOOL + 1
      00806C                        271         RamByte XREG1           ; extra working register for core words
                           000009     1         XREG1 = RAMPOOL
                           00000A     2         RAMPOOL = RAMPOOL + 1
      00806C                        272         RamByte XREG2           ; extra working register for core words
                           00000A     1         XREG2 = RAMPOOL
                           00000B     2         RAMPOOL = RAMPOOL + 1
      00806C                        273         RamByte XREG3           ; extra working register for core words
                           00000B     1         XREG3 = RAMPOOL
                           00000C     2         RAMPOOL = RAMPOOL + 1
      00806C                        274         RamWord BITAT           ; reserve space for BTJF
                           00000C     1         BITAT = RAMPOOL
                           00000E     2         RAMPOOL = RAMPOOL + 2
                           000016   275         RAMPOOL = RAMPOOL + 8
      00806C                        276         RamWord BITSTO          ; reserve space for BSET/BRES
                           000016     1         BITSTO = RAMPOOL
                           000018     2         RAMPOOL = RAMPOOL + 2
                           00001B   277         RAMPOOL = RAMPOOL + 3
                                    278 
                                    279         ;***********************
                                    280         ;******  7) Code  ******
                                    281         ;***********************
                                    282 
                                    283 ;        ==============================================
                                    284 ;        Forth header macros
                                    285 ;        Macro support in SDCC's assembler "SDAS" has some quirks:
                                    286 ;          * strings with "," and ";" aren't allowed in parameters
                                    287 ;          * after include files, the first macro call may fail
                                    288 ;            unless it's preceded by unconditional code
                                    289 ;         ==============================================
                                    290 
                           000000   291         LINK =          0       ;
                                    292 
                                    293         .macro  HEADER Label wName
                                    294         .endm
                                    295 
                                    296         .macro  HEADFLG Label wName wFlag
                                    297 
                                    298         .endm
                                    299 
                                    300 ;       ==============================================
                                    301 ;               Low level code
                                    302 ;       ==============================================
                                    303 ;       Macro for inline literals using the TRAP approach
                           000000   304         .ifeq  USE_CALLDOLIT
                                    305         
                                    306         .macro DoLitW w
                                    307         TRAP
                                    308         .dw     w
                                    309         .endm
                                    310         
                           000001   311         .else
                                    312         
                                    313 ;       Macro for inline literals using the DOLIT approach
                                    314 
                                    315         .macro DoLitW w
                                    316         DECW X
                                    317         DECW X
                                    318         LDW Y,#w
                                    319         LDW (X),Y
                                    320         .endm
                                    321         
                                    322         .endif
                                    323 
                                    324 ; Alternative for DOXCODE
                                    325         .macro LDW_Y_CONTENT_X
                                    326         LDW Y,X
                                    327         LDW Y,(Y)		; tos in Y
                                    328         .endm
                                    329 ;	actual operation on Y
                                    330 ;       LDW (X),Y        
                                    331 
                                    332 ;       ==============================================
                                    333 ;               UART chat code
                                    334 ;       ==============================================
                                    335 
                                    336 ;       send byte from A 
      00806C                        337         HEADER  RXA "RXA"
      00806C                        338 RXA:
                           000001   339 .if HAS_RXUART
      00806C 72 0B 52 30 FB   [ 2]  340         BTJF    UART_SR,#5,RXA
      008071 C6 52 31         [ 1]  341         LD      A,UART_DR      ; get char in A
                           000000   342 .else
                                    343         BTJF USR_6,#0,RXA
                                    344         LD A,TIM4RXBUF
                                    345         CLR USR_6		; clear rxa flag
                                    346 .endif
      008074 81               [ 4]  347         RET
                                    348 
                                    349 ; receive byte in tos 
      008075                        350         HEADER  TOB "TOB"
      008075                        351 TOB:
                           000000   352 .if HAS_RXSIM
                                    353         CLR USR_6
                                    354 .endif
      008075 AD F5            [ 4]  355         CALLR RXA
      008077 5A               [ 2]  356         DECW X
      008078 F7               [ 1]  357         LD (X),A
      008079 5A               [ 2]  358         DECW X
      00807A 7F               [ 1]  359         CLR (X)
      00807B 81               [ 4]  360         RET
                                    361 
                                    362 ; receive cell in tos 
      00807C                        363         HEADER  TOW "TOW"
      00807C                        364 TOW:
      00807C AD F7            [ 4]  365         CALLR TOB
      00807E AD EC            [ 4]  366         CALLR RXA
      008080 F7               [ 1]  367         LD (X),A
      008081 81               [ 4]  368         RET
                                    369 
                                    370 ; send byte from tos 
      008082                        371         HEADER  ATO "ATO"
      008082                        372 ATO:
      008082 F6               [ 1]  373         LD A,(X)
      008083 5C               [ 1]  374         INCW X
      008084 CC 82 0E         [ 2]  375         JP TXASTOR
                                    376 
                                    377 ; send cell from tos 
      008087                        378         HEADER  WTO "WTO"
      008087                        379 WTO:
      008087 CD 85 6F         [ 4]  380         CALL EXG
      00808A AD F6            [ 4]  381         CALLR ATO
      00808C 20 F4            [ 2]  382         JRA ATO
                                    383 
                                    384 
                                    385 ; send bytes from memory pointed to by MP 
      00808E                        386         HEADER  SENDBYTES "SENDBYTES"
      00808E                        387 SENDBYTES:
      00808E AD E5            [ 4]  388         CALLR TOB
      008090 5C               [ 1]  389         INCW X
      008091 90 BE 04         [ 2]  390         LDW Y,MP
      008094                        391 1$:
      008094 90 F6            [ 1]  392         LD A,(Y)
      008096 CD 82 0E         [ 4]  393         CALL TXASTOR
      008099 90 5C            [ 1]  394         INCW Y
      00809B 7A               [ 1]  395         DEC(X)
      00809C 26 F6            [ 1]  396         JRNE 1$
      00809E 5C               [ 1]  397         INCW X
      00809F 81               [ 4]  398         RET
                                    399 
                                    400 ;       receive byte and store in memory pointer MP 
      0080A0                        401         HEADER  SETADDR "SETADDR"
      0080A0                        402 SETADDR:
      0080A0 AD DA            [ 4]  403         CALLR TOW
      0080A2 90 93            [ 1]  404         LDW Y,X
      0080A4 90 FE            [ 2]  405         LDW Y,(Y)
      0080A6 90 BF 04         [ 2]  406         LDW MP,Y
      0080A9 5C               [ 1]  407         INCW X
      0080AA 5C               [ 1]  408         INCW X
      0080AB 81               [ 4]  409         RET
                                    410 
                                    411 ;       
      0080AC                        412         HEADER  GETSP "GETSP"
      0080AC                        413 GETSP:
      0080AC CD 86 3F         [ 4]  414         CALL SPAT
      0080AF 20 D6            [ 2]  415         JRA WTO
                                    416 
                                    417 ;       
      0080B1                        418         HEADER  WRITEBS "WRITEBS"
      0080B1                        419 WRITEBS:
      0080B1 AD C2            [ 4]  420         CALLR TOB	; count
      0080B3 90 BE 04         [ 2]  421 1$:	LDW Y,MP		; memory pointer in Y
      0080B6 AD B4            [ 4]  422         CALLR RXA        ; 
      0080B8 90 F7            [ 1]  423         LD (Y),A
      0080BA 90 5C            [ 1]  424         INCW Y
      0080BC 90 BF 04         [ 2]  425         LDW MP,Y
                           000000   426 .if HAS_RXSIM
                                    427         CLR USR_6
                                    428 .endif
      0080BF 90 93            [ 1]  429         LDW Y,X
      0080C1 90 FE            [ 2]  430         LDW Y,(Y)
      0080C3 90 5A            [ 2]  431         DECW Y
      0080C5 FF               [ 2]  432         LDW (X),Y
      0080C6 26 EB            [ 1]  433         JRNE 1$
      0080C8 5C               [ 1]  434         INCW X
      0080C9 5C               [ 1]  435         INCW X
      0080CA 81               [ 4]  436         RET
                                    437 
      0080CB                        438         HEADER  SETSP "SETSP"
      0080CB                        439 SETSP:
      0080CB AD AF            [ 4]  440         CALLR TOW
      0080CD FE               [ 2]  441         LDW X,(X)
      0080CE 81               [ 4]  442         RET
                                    443 
      0080CF                        444         HEADER  RUN "RUN"
      0080CF                        445 RUN:
      0080CF AD AB            [ 4]  446         CALLR TOW
      0080D1 FE               [ 2]  447         LDW X,(X)
      0080D2 AD A8            [ 4]  448         CALLR TOW
      0080D4 CC 82 30         [ 2]  449         JP EXECU
                                    450 
      0080D7                        451         HEADER  FLASH "FLASH"
      0080D7                        452 FLASH:
      0080D7                        453         DoLitW FLASHBUF_ADDR
      0080D7 5A               [ 2]    1         DECW X
      0080D8 5A               [ 2]    2         DECW X
      0080D9 90 AE 03 40      [ 2]    3         LDW Y,#FLASHBUF_ADDR
      0080DD FF               [ 2]    4         LDW (X),Y
      0080DE AD 9C            [ 4]  454         CALLR TOW
      0080E0 AD 9A            [ 4]  455         CALLR TOW
      0080E2 CD 86 0C         [ 4]  456         CALL CMOVE
      0080E5 A6 AB            [ 1]  457         LD A,#0xAB
      0080E7 CC 82 0E         [ 2]  458         JP TXASTOR
                                    459 
      0080EA                        460         HEADER  TABLE "TABLE"
      0080EA                        461 TABLE:
      0080EA 80 A0                  462         .dw SETADDR
      0080EC 80 8E                  463         .dw SENDBYTES
      0080EE 80 B1                  464         .dw WRITEBS
      0080F0 80 AC                  465         .dw GETSP
      0080F2 80 CB                  466         .dw SETSP
      0080F4 80 CF                  467         .dw RUN
      0080F6 80 D7                  468         .dw FLASH
      0080F8 9D               [ 1]  469 NOP     ; for disaasembling purpose
                           00000F   470 lower=0xf
                           000018   471 upper=0x18
                           000010   472 offset=0x10
                                    473 
      0080F9                        474         HEADER  CHAT "CHAT"
      0080F9                        475 CHAT:
                           000000   476 .if HAS_RXSIM
                                    477         LD A,TIM4RXBUF
                                    478         CLR USR_6
                           000001   479 .else
      0080F9 CD 80 6C         [ 4]  480         CALL RXA
                                    481 .endif
      0080FC A1 0F            [ 1]  482         CP A,#lower
      0080FE 2B 14            [ 1]  483         JRMI 1$
      008100 A1 18            [ 1]  484         CP A,#upper
      008102 2C 10            [ 1]  485         JRSGT 1$
      008104 A0 10            [ 1]  486         SUB A,#offset
      008106 48               [ 1]  487         SLL A
      008107 AB EA            [ 1]  488         ADD A,#TABLE
      008109 90 97            [ 1]  489         LD YL,A
      00810B 4F               [ 1]  490         CLR A
      00810C A9 80            [ 1]  491         ADC A,#>TABLE   ; MSB of TABLE
      00810E 90 95            [ 1]  492         LD YH,A
      008110 90 FE            [ 2]  493         LDW Y,(Y)
      008112 90 FC            [ 2]  494         JP (Y)
      008114                        495 1$:
      008114 81               [ 4]  496         RET
                                    497         
                                    498 ; ==============================================
                                    499 ;       Getbit and Setbit routines to be moved 
                                    500 ;       to ram during reset ( -- )
                                    501 ; ==============================================
                                    502 
      008115                        503         HEADER  COLD1 "COLD1"
      008115                        504 COLD1:
      008115 4F               [ 1]  505         CLR A
      008116 72 01 00 08 01   [ 2]  506         BTJF XREG0,#0,1$
      00811B 4C               [ 1]  507         INC A
      00811C E7 01            [ 1]  508 1$:     LD (1,X),A
      00811E 81               [ 4]  509         RET
      00811F 72 10 01 00      [ 1]  510         BSET 0x100,#0
      008123 81               [ 4]  511         RET
                                    512 ; ==============================================
                                    513 
                                    514 ; ==============================================
                                    515 
                                    516 ;       Includes for board support code
                                    517 ;       Board I/O initialization and E/E mapping code
                                    518 ;       Hardware dependent words, e.g.  BKEY, OUT!
                                    519         .include "boardcore.inc"
                                      1 ; STM8S103F3P6 "Minimal System Board" STM8S device dependent routines
                                      2 
                                      3 
                                      4 ;       BOARDINIT  ( -- )
                                      5 ;       Init board GPIO (except COM ports)
                                      6 
      008124                          7 BOARDINIT:
                                      8         ; Board I/O initialization
      008124 72 1A 50 07      [ 1]    9         BSET    PB_DDR,#5
      008128 72 1A 50 08      [ 1]   10         BSET    PB_CR1,#5
                                     11 
                           000001    12         .ifne   HAS_OUTPUTS
      00812C 4F               [ 1]   13         CLR     A
      00812D 20 03            [ 2]   14         JRA     AOUTSTOR
                           000000    15         .else
                                     16         RET
                                     17         .endif
                                     18 
                                     19 ;===============================================================
                                     20 
                           000000    21         .ifne   HAS_LED7SEG
                                     22 ;       LED_MPX driver ( -- )
                                     23 ;       Code called from ISR for LED MPX
                                     24 
                                     25 LED_MPX:
                                     26         RET
                                     27         .endif
                                     28 
                                     29 
                                     30 ;===============================================================
                                     31 
                           000001    32         .ifne   HAS_OUTPUTS
      00812F                         33         RamWord OUTPUTS         ; "OUT", e.g. relays, LEDs, etc. (16 bit)
                           00001B     1         OUTPUTS = RAMPOOL
                           00001D     2         RAMPOOL = RAMPOOL + 2
                                     34 
                                     35 ;       OUT!  ( c -- )
                                     36 ;       Put c to board outputs, storing a copy in OUTPUTS
                                     37 
      00812F                         38         HEADER  OUTSTOR "OUT!"
                                     39 
      00812F                         40 OUTSTOR:
      00812F 5C               [ 1]   41         INCW    X
      008130 F6               [ 1]   42         LD      A,(X)
      008131 5C               [ 1]   43         INCW    X
      008132                         44 AOUTSTOR:
      008132 B7 1C            [ 1]   45         LD      OUTPUTS+1,A
      008134 46               [ 1]   46         RRC     A
      008135 8C               [ 1]   47         CCF
      008136 90 1B 50 05      [ 1]   48         BCCM    PB_ODR,#5       ; PB5 LED
      00813A 81               [ 4]   49         RET
                                     50         .endif
                                     51 
                                     52 ;===============================================================
                                     53 
                           000000    54         .ifne   HAS_KEYS
                                     55 
                                     56 ;       BKEY  ( -- f )     ( TOS STM8: -- A,Z,N )
                                     57 ;       Read board key state as a bitfield
                                     58 
                                     59         .dw     LINK
                                     60 
                                     61         LINK =  .
                                     62         .db     (4)
                                     63         .ascii  "BKEY"
                                     64 BKEY:
                                     65         CLR     A
                                     66         JP      ASTOR
                                     67 
                                     68 
                                     69 ;       BKEYC  (  -- c )   ( TOS STM8: -- A,Z,N )
                                     70 ;       Read and translate board dependent key bitmap into char
                                     71 
                                     72 BKEYCHAR:
                                     73         JRA     BKEY            ; Dummy: get "no key" and leave it as it is
                                     74 
                                     75        .endif
                                     76 
                                    520 
                                    521 ;       ADC routines depending on STM8 family
                                    522         .include "stm8_adc.inc"
                                      1 ;--------------------------------------------------------
                                      2 ;       STM8 eForth           STM8S/L Family ADC code
                                      3 ;--------------------------------------------------------
                                      4 
                           000001     5         .ifne   HAS_ADC
                                      6 ;       ADC!  ( c -- )
                                      7 ;       Init ADC, select channel for conversion
                                      8 
                           000000     9         .ifeq   (FAMILY - STM8L)
                                     10 
                                     11 ;       ADC for the STM8L family
                                     12 
                                     13 ;       RI settings for analog signal routing should be done
                                     14 ;       on the board configuration level
                                     15 
                                     16         HEADER  ADCSTOR "ADC!"
                                     17 ADCSTOR:
                                     18         BSET    CLK_PCKENR2,#0  ; enable clock for ADC
                                     19         BSET    ADC1_CR1,#0     ; enable ADC
                                     20         LD      A,#4
                                     21         LDW     Y,#ADC1_LTRL
                                     22 0$:     INCW    Y               ; clear DAC1_SQRx
                                     23         CLR     (Y)
                                     24         DEC     A
                                     25         JRUGT   0$
                                     26         INCW    X
                                     27         LD      A,(X)          ; A = adc channel#
                                     28         INCW    X
                                     29         TNZ     A
                                     30         INC     A
                                     31         LDW     Y,#4
                                     32 1$:     DECW    Y               ; select appropriate ADC1-SQR#
                                     33         SUB     A,#8
                                     34         JRUGT   1$
                                     35         ADD     A,#7
                                     36         DECW    X               ; offset to DAC_SQR1 -> tos
                                     37         DECW    X
                                     38         LDW     (X),Y           ; push on stack
                                     39         CLRW    Y
                                     40         LD      YL,A
                                     41         LD      A,#1
                                     42         JP      3$
                                     43 2$:     SLL     A
                                     44 3$:     DECW    Y
                                     45         JRSGE   2$
                                     46         LDW     Y,X
                                     47         INCW    X
                                     48         INCW    X
                                     49         LDW     Y,(Y)
                                     50         LD      (ADC1_SQR1,Y),A ; set channel bit
                                     51         BSET    ADC1_SQR1,#7    ; DMA disabled for single conversion
                                     52         BRES    ADC1_CR1,#0     ; disable ADC
                                     53         RET
                                     54 
                                     55 ;       ADC@  ( -- w )
                                     56 ;       start ADC conversion, read result
                                     57 
                                     58         HEADER  ADCAT "ADC@"
                                     59 ADCAT:
                                     60         BRES    ADC1_SR,#0      ; reset EOC
                                     61         BSET    ADC1_CR1,#0     ; enable ADC
                                     62         BSET    ADC1_CR1,#1     ; start ADC
                                     63 1$:     BTJF    ADC1_SR,#0,1$   ; wait until EOC
                                     64         LDW     Y,ADC1_DRH      ; read ADC
                                     65         BRES    ADC1_CR1,#0     ; disable ADC
                                     66         DECW    X               ; SUBW  X,#2
                                     67         DECW    X
                                     68         LDW     (X),Y           ; push on stack
                                     69         RET                     ; go to RET of EXEC
                                     70 
                           000001    71         .else
                                     72 
                                     73 ;       ADC for the STM8S family
                                     74 
                                     75 ;       ADC!  ( c -- )
                                     76 ;       Init ADC, select channel for conversion
                                     77 
      00813B                         78         HEADER  ADCSTOR "ADC!"
      00813B                         79 ADCSTOR:
      00813B 5C               [ 1]   80         INCW    X
      00813C F6               [ 1]   81         LD      A,(X)
      00813D 5C               [ 1]   82         INCW    X
      00813E A4 0F            [ 1]   83         AND     A,#0x0F
      008140 C7 54 00         [ 1]   84         LD      ADC_CSR,A       ; select channel
      008143 72 16 54 02      [ 1]   85         BSET    ADC_CR2,#3      ; align ADC to LSB
      008147 72 10 54 01      [ 1]   86         BSET    ADC_CR1,#0      ; enable ADC
      00814B 81               [ 4]   87         RET
                                     88 
                                     89 ;       ADC@  ( -- w )
                                     90 ;       start ADC conversion, read result
                                     91 
      00814C                         92         HEADER  ADCAT "ADC@"
      00814C                         93 ADCAT:
      00814C 72 1F 54 00      [ 1]   94         BRES    ADC_CSR,#7      ; reset EOC
      008150 72 10 54 01      [ 1]   95         BSET    ADC_CR1,#0      ; start ADC
      008154 72 0F 54 00 FB   [ 2]   96 1$:     BTJF    ADC_CSR,#7,1$   ; wait until EOC
      008159 90 CE 54 04      [ 2]   97         LDW     Y,ADC_DRH       ; read ADC
      00815D 5A               [ 2]   98         DECW    X               ; SUBW  X,#2
      00815E 5A               [ 2]   99         DECW    X
      00815F FF               [ 2]  100         LDW     (X),Y           ; push on stack
      008160 81               [ 4]  101         RET                     ; go to RET of EXEC
                                    102         .endif
                                    103         .endif
                                    523 
                                    524 ;       Generic board I/O: 7S-LED rendering, board key mapping
                                    525         .include "board_io.inc"
                                      1 ;--------------------------------------------------------
                                      2 ;       STM8 eForth Board I/O
                                      3 ;       key input, and LED 7-segment LED display output
                                      4 ;--------------------------------------------------------
                                      5 
                           000000     6         .ifne   HAS_KEYS
                                      7 
                                      8         RamByte KEYREPET        ; board key repetition control (8 bit)
                                      9 
                                     10 ;       ?KEYB   ( -- c T | F )  ( TOS STM8: -- Y,Z,N )
                                     11 ;       Return keyboard char and true, or false if no key pressed.
                                     12 
                                     13         HEADER  QKEYB "?KEYB"
                                     14 QKEYB:
                                     15         CALL    BKEYCHAR        ; Read char from keyboard (option: vectored code)
                                     16         INCW    X
                                     17         LD      A,(X)
                                     18         INCW    X
                                     19         TNZ     A
                                     20 
                                     21         JRNE    KEYBPRESS
                                     22         ; Bit7: flag press + 100*5ms hold before repetition
                                     23         MOV     KEYREPET,#(0x80 + 100)
                                     24         JRA     ZERO1
                                     25 KEYBPRESS:
                                     26         BTJF    KEYREPET,#7,KEYBHOLD
                                     27         BRES    KEYREPET,#7
                                     28         JRA     ATOKEYB
                                     29 KEYBHOLD:
                                     30         DEC     KEYREPET
                                     31         JRNE    ZERO1
                                     32         MOV     KEYREPET,#30    ; repetition time: n*5ms
                                     33 ATOKEYB:
                                     34         JP      ATOKEY          ; push char and flag true
                                     35 ZERO1:   JP      ZERO
                                     36 
                                     37         .endif
                                     38 
                           000000    39         .ifne   HAS_LED7SEG
                                     40 
                                     41         .ifeq  USE_CALLDOLIT
                                     42 ;       Macro for inline literals using the TRAP approach
                                     43         .macro DoLitC c
                                     44         TRAP
                                     45         .db     c
                                     46         .endm
                                     47 
                                     48         .else
                                     49 
                                     50 ;       Macro for inline literals
                                     51         .macro DoLitC c
                                     52         DECW X
                                     53         LD A,#c
                                     54         LD (X),A
                                     55         DECW X
                                     56         CLR (X)
                                     57         .endm
                                     58 
                                     59         .endif
                                     60 
                                     61         .if     gt,(HAS_LED7SEG-1)
                                     62         RamByte LED7GROUP       ; byte index of 7-SEG digit group
                                     63         .endif
                                     64 
                                     65         DIGITS = HAS_LED7SEG*LEN_7SGROUP
                                     66         RamBlck LED7FIRST,DIGITS ; leftmost 7S-LED digit
                                     67         LED7LAST = RAMPOOL-1    ; save memory location of rightmost 7S-LED digit
                                     68 
                                     69 
                                     70 ;       7-seg LED patterns, "70s chique"
                                     71 PAT7SM9:
                                     72         .db     0x00, 0x40, 0x80, 0x52 ; , - . / (',' as blank)
                                     73         .db     0x3F, 0x06, 0x5B, 0x4F ; 0,1,2,3
                                     74         .db     0x66, 0x6D, 0x7D, 0x07 ; 4,5,6,7
                                     75         .db     0x7F, 0x6F             ; 8,9
                                     76 PAT7SAZ:
                                     77         .db           0x77, 0x7C, 0x39 ;   A,B,C
                                     78         .db     0x5E, 0x79, 0x71, 0x3D ; D,E,F,G
                                     79         .db     0x74, 0x30, 0x1E, 0x7A ; H,I,J,K
                                     80         .db     0x38, 0x55, 0x54, 0x5C ; L,M,N,O
                                     81         .db     0x73, 0x67, 0x50, 0x6D ; P,Q,R,S
                                     82         .db     0x78, 0x3E, 0x1C, 0x1D ; T,U,V,W
                                     83         .db     0x76, 0x6E, 0x5B       ; X,Y,Z
                                     84 
                                     85 ;       E7S  ( c -- )
                                     86 ;       Convert char to 7-seg LED pattern, and insert it in display buffer
                                     87 
                                     88         HEADER  EMIT7S "E7S"
                                     89 EMIT7S:
                                     90         LD      A,(1,X)         ; c to A
                                     91 
                                     92         CP      A,#' '
                                     93         JRNE    E7SNOBLK
                                     94 
                                     95         .if     gt,(HAS_LED7SEG-1)
                                     96         LD      A,LED7GROUP
                                     97         JRMI    2$              ; test LED7GROUP.7 "no-tab flag"
                                     98         INC     A
                                     99         CP      A,#HAS_LED7SEG
                                    100         JRULT   1$
                                    101         CLR     A
                                    102 1$:     OR      A,#0x80         ; only one tab action, set "no-tab flag"
                                    103         LD      LED7GROUP,A
                                    104 
                                    105 2$:     CALLR   XLEDGROUP
                                    106         EXGW    X,Y             ; restore X/Y after XLEDGROUP
                                    107 ;        .else
                                    108 ;        LDW     Y,#LED7FIRST    ; DROP DOLIT LED7FIRST
                                    109         .endif
                                    110 ;        LDW     (X),Y
                                    111 ;        DoLitC  LEN_7SGROUP
                                    112 ;        JP      ERASE
                                    113         RET
                                    114 
                                    115 E7SNOBLK:
                                    116 
                                    117         .if     gt,(HAS_LED7SEG-1)
                                    118         CP      A,#LF           ; test for c ~ /[<CR><LF>]/
                                    119         JRNE    E7SNOLF
                                    120         MOV     LED7GROUP,#0x80 ; go to first LED group, set "no-tab flag"
                                    121         JRA     E7END
                                    122         .endif
                                    123 
                                    124 E7SNOLF:
                                    125         .if     gt,(HAS_LED7SEG-1)
                                    126         BRES    LED7GROUP,#7    ; on char output: clear "no-tab flag"
                                    127         .endif
                                    128 
                                    129         CP      A,#'.'
                                    130         JREQ    E7DOT
                                    131         CP      A,#','
                                    132         JRMI    E7END
                                    133         CP      A,#'z'
                                    134         JRPL    E7END
                                    135         CP      A,#'A'
                                    136         JRUGE   E7ALPH
                                    137 
                                    138         ; '-'--'9' (and '@')
                                    139         SUB     A,#','
                                    140         LD      (1,X),A
                                    141         DoLitW  PAT7SM9
                                    142         JRA     E7LOOKA
                                    143 E7ALPH:
                                    144         ; 'A'--'z'
                                    145         AND     A,#0x5F         ; convert to uppercase
                                    146         SUB     A,#'A'
                                    147         LD      (1,X),A
                                    148         DoLitW  PAT7SAZ
                                    149 E7LOOKA:
                                    150         CALL    PLUS
                                    151         CALL    CAT
                                    152         JP      PUT7S
                                    153 
                                    154 E7DOT:
                                    155         .if     gt,(HAS_LED7SEG-1)
                                    156         CALL    XLEDGROUP
                                    157         LD      A,((LEN_7SGROUP-1),X)
                                    158         OR      A,#0x80
                                    159         LD      ((LEN_7SGROUP-1),X),A
                                    160         EXGW    X,Y             ; restore X/Y after XLEDGROUP
                                    161         ; fall trough
                                    162 
                                    163         .else
                                    164         LD      A,#0x80         ; 7-seg P (dot)
                                    165         OR      A,LED7LAST
                                    166         LD      LED7LAST,A
                                    167         .endif
                                    168         ; fall trough
                                    169 
                                    170 E7END:
                                    171         JP      DROP
                                    172 
                                    173         .if     gt,(HAS_LED7SEG-1)
                                    174 ;       Helper routine for calculating LED group start adress
                                    175 ;       return: X: LED group addr, Y: DSP, A: LEN_7SGROUP
                                    176 ;       caution: caller must restore X/Y!
                                    177 XLEDGROUP:
                                    178         EXGW    X,Y             ; use X to save memory
                                    179         LD      A,LED7GROUP
                                    180         AND     A,#0x7F         ; ignore "no-tab flag"
                                    181         LD      XL,A
                                    182         LD      A,#LEN_7SGROUP
                                    183         MUL     X,A
                                    184         ADDW    X,#LED7FIRST
                                    185         RET
                                    186         .endif
                                    187 
                                    188 ;       P7S  ( c -- )
                                    189 ;       Right aligned 7S-LED pattern output, rotates LED group buffer
                                    190 
                                    191         HEADER  PUT7S "P7S"
                                    192 PUT7S:
                                    193         .if     gt,(HAS_LED7SEG-1)
                                    194         CALLR   XLEDGROUP
                                    195         DEC     A
                                    196         PUSH    A
                                    197 1$:     LD      A,(1,X)
                                    198         LD      (X),A
                                    199         INCW    X
                                    200         DEC     (1,SP)
                                    201         JRNE    1$
                                    202         POP     A
                                    203 
                                    204         EXGW    X,Y             ; restore X/Y after XLEDGROUP
                                    205         INCW    X
                                    206         LD      A,(X)
                                    207         INCW    X
                                    208         TNZ     A
                                    209         LD      (Y),A
                                    210         .else
                                    211         DoLitC  LED7FIRST+1
                                    212         DoLitC  LED7FIRST
                                    213         DoLitC  (LEN_7SGROUP-1)
                                    214         CALL    CMOVE
                                    215         INCW    X
                                    216         LD      A,(X)
                                    217         INCW    X
                                    218         TNZ     A
                                    219         LD      LED7LAST,A
                                    220         .endif
                                    221         RET
                                    222 
                                    223         .macro  Board_IO_Init
                                    224         .if     gt,(HAS_LED7SEG-1)
                                    225         MOV     LED7GROUP,#0     ; one of position HAS_LED7SEG 7-SEG digit groups
                                    226         .endif
                                    227         MOV     LED7FIRST  ,#0x66 ; 7S LEDs 4..
                                    228         MOV     LED7FIRST+1,#0x78 ; 7S LEDs .t.
                                    229         MOV     LED7FIRST+2,#0x74 ; 7S LEDs ..h
                                    230         .endm
                                    231 
                           000001   232         .else
                                    233         .macro  Board_IO_Init
                                    234         ; no LED-7Seg
                                    235         .endm
                                    236 
                                    237         .endif
                                    526 
                                    527 ;       Simulate serial interface code
                                    528         .include "sser.inc"
                                      1 ;--------------------------------------------------------
                                      2 ;       STM8 eForth Simulated Serial I/O
                                      3 ;       Init and default code
                                      4 ;--------------------------------------------------------
                                      5 
                                      6 ;       Simulated serial I/O
                                      7 ;       either full or half duplex
                                      8 
                           000001     9         .ifeq  HAS_TXSIM + HAS_RXSIM
                                     10 
                                     11         .macro SSER_Init
                                     12         .endm
                                     13 
                                     14         .globl _TIM4_IRQHandler
                                     15         .globl _EXTI0_IRQHandler
                                     16         .globl _EXTI1_IRQHandler
                                     17         .globl _EXTI2_IRQHandler
                                     18         .globl _EXTI3_IRQHandler
                                     19         .globl _EXTI4_IRQHandler
                                     20         .globl _EXTI5_IRQHandler
                                     21         .globl _EXTI6_IRQHandler
                                     22         .globl _EXTI7_IRQHandler
                                     23 
                                     24         ; dummy for linker - can be overwritten by Forth application
                                     25 
      008161                         26 _EXTI0_IRQHandler:
      008161                         27 _EXTI1_IRQHandler:
      008161                         28 _EXTI2_IRQHandler:
      008161                         29 _EXTI3_IRQHandler:
      008161                         30 _EXTI4_IRQHandler:
      008161                         31 _EXTI5_IRQHandler:
      008161                         32 _EXTI6_IRQHandler:
      008161                         33 _EXTI7_IRQHandler:
      008161                         34 _TIM4_IRQHandler:
                                     35 
                           000000    36         .else
                                     37 
                                     38         .macro SSER_Init
                                     39         .ifne   HAS_RXSIM+HAS_TXSIM
                                     40 
                                     41           .ifeq   (FAMILY - STM8L)
                                     42         BSET    CLK_PCKENR1,#2  ; STM8L clock tree: enable TIM4
                                     43           .endif
                                     44 
                                     45          ; TIM4 based RXD or TXD: initialize timer
                                     46         MOV     TIM4_ARR,#CTIM4ARR
                                     47 
                                     48         MOV     TIM4_PSCR,#CTIM4PSCR ; prescaler 1/(2^n)
                                     49         MOV     TIM4_CR1,#0x01  ; enable TIM4
                                     50         .endif
                                     51 
                                     52         .ifne   HAS_TXSIM*((PNRX-PNTX)+(1-HAS_RXSIM))
                                     53         ; init TxD through GPIO if not shared pin with PNRX
                                     54         BSET    PSIM+ODR,#PNTX  ; PNTX GPIO high
                                     55         BSET    PSIM+DDR,#PNTX  ; PNTX GPIO output
                                     56         BSET    PSIM+CR1,#PNTX  ; enable PNTX push-pull
                                     57         .endif
                                     58 
                                     59         .ifne   (HAS_RXSIM)
                                     60           ; init RxD EXTI for GPIO
                                     61           .ifeq   (FAMILY - STM8L)
                                     62             ; STM8L EXTI for port bit 0..7
                                     63             .ifeq   (PNRX / 4)
                                     64         BSET    EXTI_CR1,#1+PNRX*2     ; ext. int. port bit 0..3 falling edge
                                     65             .else
                                     66         BSET    EXTI_CR2,#1+(PNRX-4)*2 ; ext. int. port bit 4..7 falling edge
                                     67             .endif
                                     68           .else
                                     69             ; STM8S EXTI for 8 bit port
                                     70             .ifeq   (PSIM-PORTA)
                                     71         BSET    EXTI_CR1,#1     ; External interrupt Port A falling edge
                                     72             .else
                                     73               .ifeq   (PSIM-PORTB)
                                     74         BSET    EXTI_CR1,#3     ; External interrupt Port B falling edge
                                     75               .else
                                     76                 .ifeq   (PSIM-PORTC)
                                     77         BSET    EXTI_CR1,#5     ; External interrupt Port C falling edge
                                     78               .else
                                     79         BSET    EXTI_CR1,#7     ; External interrupt Port D falling edge
                                     80               .endif
                                     81             .endif
                                     82           .endif
                                     83         .endif
                                     84         BRES    PSIM+DDR,#PNRX    ; 0: input (default)
                                     85         BSET    PSIM+CR1,#PNRX    ; enable PNRX pull-up
                                     86         BSET    PSIM+CR2,#PNRX    ; enable PNRX external interrupt
                                     87         .endif
                                     88         .endm
                                     89 
                                     90         ; include required serial I/O code
                                     91           .ifne  PNRX^PNTX
                                     92                .include"sser_fdx.inc"
                                     93           .else
                                     94             .include "sser_hdx.inc" ; Half Duplex serial
                                     95           .endif
                                     96         .endif
                                     97 
                                    529 
                                    530 ;       Background Task: context switch with wakeup unit or timer
                                    531         .include "bgtask.inc"
                                      1 ;--------------------------------------------------------
                                      2 ; Public variables in this module
                                      3 ;--------------------------------------------------------
                                      4 
                                      5         .globl _TIM1_IRQHandler
                                      6         .globl _TIM2_IRQHandler
                                      7         .globl _TIM3_IRQHandler
                                      8 
                                      9         ;******  Board variables  ******
                                     10 
                           000001    11         .ifne   HAS_BACKGROUND
      008161                         12         RamWord BGADDR          ; address of background routine (0: off)
                           00001D     1         BGADDR = RAMPOOL
                           00001F     2         RAMPOOL = RAMPOOL + 2
      008161                         13         RamWord TICKCNT         ; "TICKCNT" 16 bit ticker (counts up)
                           00001F     1         TICKCNT = RAMPOOL
                           000021     2         RAMPOOL = RAMPOOL + 2
                                     14        .endif
                                     15 
                                     16        ;******  timer macro  ******
                                     17 
                                     18         ; init BG timer interrupt
                                     19         .macro BGTASK_Init
                                     20         .ifne   HAS_BACKGROUND
                                     21 
                                     22         .ifne   BG_USE_TIM1
                                     23         BG_INT = ITC_IX_TIM1
                                     24         MOV     TIM1_PSCRL,#7   ; prescaler 1/(7+1) = 1/8
                                     25         .else
                                     26         .ifne   BG_USE_TIM3
                                     27         BG_INT = ITC_IX_TIM3
                                     28         .else
                                     29         BG_INT = ITC_IX_TIM2
                                     30         .endif
                                     31         MOV     BG_TIM_PSCR,#3  ; prescaler 1/(2^3) = 1/8
                                     32         .endif
                                     33         BRES    ITC_SPR1+(BG_INT/4),#((BG_INT%4)*2+1)  ; Interrupt prio. low
                                     34 
                                     35         MOV     BG_TIM_ARRH,#(BG_TIM_REL/256)  ; reload H
                                     36         MOV     BG_TIM_ARRL,#(BG_TIM_REL%256)  ;        L
                                     37         MOV     BG_TIM_CR1,#0x01 ; enable background timer
                                     38         MOV     BG_TIM_IER,#0x01 ; enable background timer interrupt
                                     39         .endif
                                     40         .endm
                                     41 
                                     42        ;******  ISR handler  ******
                                     43 
                                     44 ;       TIM1 or TIM2 interrupt handler for background task
      008161                         45 _TIM1_IRQHandler:
      008161                         46 _TIM2_IRQHandler:
      008161                         47 _TIM3_IRQHandler:
                                     48         ; STM8 DIV/DIVW erratum "Workaround 2: keep bit6 cleared"
      008161 4B 08            [ 1]   49         PUSH    #0x08           ; BG task fixed priority (I0=1, I1=0)
      008163 86               [ 1]   50         POP     CC
                                     51 
                           000001    52         .ifne   (HAS_LED7SEG + HAS_BACKGROUND)
      008164 72 11 53 04      [ 1]   53         BRES    BG_TIM_SR1,#0   ; clear TIMx UIF
                                     54 
                           000000    55         .ifne   HAS_LED7SEG
                                     56         CALL    LED_MPX         ; "PC_LEDMPX" board dependent code for 7Seg-LED-Displays
                                     57         .endif
                                     58 
                                     59 ;       Background operation saves & restores the context of the interactive task
                                     60 ;       Cyclic context reset of Forth background task (stack, BASE, HLD, I/O vector)
                           000001    61         .ifne   HAS_BACKGROUND
      008168 BE 1F            [ 2]   62         LDW     X,TICKCNT
      00816A 5C               [ 1]   63         INCW    X
      00816B BF 1F            [ 2]   64         LDW     TICKCNT,X
                                     65         ; fall through
                                     66 
                           000000    67         .ifne   BG_RUNMASK
                                     68         LD      A,XL            ; Background task runs if "(BG_RUNMASK AND TICKCNT) equals 0"
                                     69         AND     A,#BG_RUNMASK
                                     70         JRNE    TIM2IRET
                                     71         .endif
                                     72 
      00816D 90 BE 1D         [ 2]   73         LDW     Y,BGADDR        ; address of background task
      008170 90 5D            [ 2]   74         TNZW    Y               ; 0: background operation off
      008172 27 21            [ 1]   75         JREQ    TIM2IRET
                                     76 
      008174 BE 08            [ 2]   77         LDW     X,XREG0         ; Save context
      008176 89               [ 2]   78         PUSHW   X
                                     79 
      008177 BE 00            [ 2]   80         LDW     X,USREMIT       ; save EMIT exection vector
      008179 89               [ 2]   81         PUSHW   X
      00817A AE 83 2B         [ 2]   82         LDW     X,#EMIT_BG      ; "'BGEMIT" xt of EMIT for BG task
      00817D BF 00            [ 2]   83         LDW     USREMIT,X
                                     84 
      00817F BE 02            [ 2]   85         LDW     X,USRQKEY       ; save QKEY exection vector
      008181 89               [ 2]   86         PUSHW   X
      008182 AE 83 95         [ 2]   87         LDW     X,#QKEY_BG      ; "'?BGKEY" xt of ?KEY for BG task
      008185 BF 02            [ 2]   88         LDW     USRQKEY,X
                                     89 
                                     90 ;        LDW     X,USRHLD
                                     91 ;        PUSHW   X
                                     92 ;        LDW     X,#PADBG        ; "BGPAD" empty PAD for BG task
                                     93 ;        LDW     USRHLD,X
                                     94 
      008187 AE 03 D0         [ 2]   95         LDW     X,#BSPP         ; "BGSPP" data stack for BG task
      00818A 90 FD            [ 4]   96         CALL    (Y)
                                     97 
                                     98 ;        POPW    X
                                     99 ;        LDW     USRHLD,X
                                    100 
      00818C 85               [ 2]  101         POPW    X
      00818D BF 02            [ 2]  102         LDW     USRQKEY,X
                                    103 
      00818F 85               [ 2]  104         POPW    X
      008190 BF 00            [ 2]  105         LDW     USREMIT,X
                                    106 
      008192 85               [ 2]  107         POPW    X
      008193 BF 08            [ 2]  108         LDW     XREG0,X
      008195                        109 TIM2IRET:
                                    110         .endif
                                    111 
      008195 80               [11]  112         IRET
                                    113         .endif
                                    114 
                                    115        ;******  BG User Words  ******
                                    116 
                           000001   117         .ifne   HAS_BACKGROUND
                                    118 ;       TIM     ( -- T)     ( TOS STM8: -- Y,Z,N )
                                    119 ;       Return TICKCNT as timer
                                    120 
      008196                        121         HEADER  TIMM "TIM"
      008196                        122 TIMM:
      008196 90 BE 1F         [ 2]  123         LDW     Y,TICKCNT
      008199 CC 83 9A         [ 2]  124         JP      AYSTOR
                                    125 
                                    126         .endif
                                    532 ; UPPLOC = RAMPOOL + 30  ; PAD in Background task, growing down, 32 bytes
                                    533 
                                    534 ; ==============================================
      00819C                        535         HEADER  RETURN "RETURN"
      00819C 81               [ 4]  536 RETURN: RET
                                    537 
                                    538 ;       Configuation table with shadow data for RESET
                                    539 
                                    540 ;       Main entry points and COLD start data
                                    541 
                                    542 
      00819D                        543 _forth:                         ; SDCC entry
                                    544 ;       Note: no return to main.c possible unless RAMEND equals SP,
                                    545 ;       and RPP init skipped
                                    546 
                                    547 ;       COLD    ( -- )
                                    548 ;       The hilevel cold start sequence.
                                    549 
      00819D                        550         HEADER  COLD "COLD"
      00819D                        551 COLD:
      00819D 9B               [ 1]  552         SIM                     ; disable interrupts
      00819E 35 00 50 C6      [ 1]  553         MOV     CLK_CKDIVR,#0   ; Clock divider register
                                    554 
      0081A2 AE 03 FF         [ 2]  555         LDW     X,#(RAMEND-FORTHRAM)
      0081A5 6F 00            [ 1]  556 1$:     CLR     (FORTHRAM,X)
      0081A7 5A               [ 2]  557         DECW    X
      0081A8 2A FB            [ 1]  558         JRPL    1$
                                    559 
      0081AA AE 03 FF         [ 2]  560         LDW     X,#RPP          ; return stack, growing down
      0081AD 94               [ 1]  561         LDW     SP,X            ; initialize return stack
                                    562 
                                    563         ; see "boardcore.inc")
      0081AE CD 81 24         [ 4]  564         CALL    BOARDINIT       ; "PC_BOARDINIT" Board initialization
                                    565 
      0081B1                        566         BGTASK_Init             ; macro for init of BG task timer, refer to bgtask.inc
                           000001     1         .ifne   HAS_BACKGROUND
                                      2 
                           000000     3         .ifne   BG_USE_TIM1
                                      4         BG_INT = ITC_IX_TIM1
                                      5         MOV     TIM1_PSCRL,#7   ; prescaler 1/(7+1) = 1/8
                           000001     6         .else
                           000000     7         .ifne   BG_USE_TIM3
                                      8         BG_INT = ITC_IX_TIM3
                           000001     9         .else
                           00000D    10         BG_INT = ITC_IX_TIM2
                                     11         .endif
      0081B1 35 03 53 0E      [ 1]   12         MOV     BG_TIM_PSCR,#3  ; prescaler 1/(2^3) = 1/8
                                     13         .endif
      0081B5 72 17 7F 73      [ 1]   14         BRES    ITC_SPR1+(BG_INT/4),#((BG_INT%4)*2+1)  ; Interrupt prio. low
                                     15 
      0081B9 35 26 53 0F      [ 1]   16         MOV     BG_TIM_ARRH,#(BG_TIM_REL/256)  ; reload H
      0081BD 35 DE 53 10      [ 1]   17         MOV     BG_TIM_ARRL,#(BG_TIM_REL%256)  ;        L
      0081C1 35 01 53 00      [ 1]   18         MOV     BG_TIM_CR1,#0x01 ; enable background timer
      0081C5 35 01 53 03      [ 1]   19         MOV     BG_TIM_IER,#0x01 ; enable background timer interrupt
                                     20         .endif
                                    567 
                           000001   568         .ifne   HAS_RXUART+HAS_TXUART
                                    569         ; Init RS232 communication port
                                    570         ; STM8S[01]003F3 init UART
      0081C9 AE 08 0B         [ 2]  571         LDW     X,#CUARTBRR      ; "UARTBRR" def. $6803 / 9600 baud
      0081CC CF 52 32         [ 2]  572         LDW     UART_BRR1,X
                           000001   573         .ifne   HAS_RXUART*HAS_TXUART
      0081CF 35 2C 52 35      [ 1]  574         MOV     UART_CR2,#0x2C  ; Use UART1 full duplex + RXNE interrupt
                           000001   575 .ifeq (FAMILY - STM8S)
      0081D3 35 CF 7F 74      [ 1]  576         MOV    ITC_SPR5,#0xCF   ; enable TIM2 interrupts while chatting
                           000000   577 .else
                                    578         MOV    ITC_SPR8,#0xC   ; enable TIM2 interrupts while chatting
                                    579 .endif
                           000000   580         .ifne   HALF_DUPLEX
                                    581         .ifeq   (FAMILY - STM8S)
                                    582         .ifeq   (HALF_DUPLEX - 1)
                                    583         ; STM8S UART1, UART4: pull-up for PD5 single-wire UART
                                    584         BRES    PD_DDR,#5       ; PD5 GPIO input high
                                    585         BSET    PD_CR1,#5       ; PD5 GPIO pull-up
                                    586         .endif
                                    587         .if HAS_RXSIM
                                    588         MOV    ITC_SPR6,#0x3F   ; enable interrupts while chatting
                                    589         .endif
                                    590         .ifeq   (HALF_DUPLEX - 2)
                                    591         ; STM8S903 type Low Density devices can re-map UART-TX to PA3
                                    592         LD      A,OPT2
                                    593         AND     A,#0x03
                                    594         CP      A,#0x03
                                    595         JREQ    $1
                                    596         ; pull-up for PD5 single-wire UART
                                    597         BRES    PD_DDR,#5       ; PD5 GPIO input high
                                    598         BSET    PD_CR1,#5       ; PD5 GPIO pull-up
                                    599         JRA     $2
                                    600 $1:
                                    601         ; pull-up for PA3 single-wire UART
                                    602         BRES    PA_DDR,#3       ; PA3 GPIO input high
                                    603         BSET    PA_CR1,#3       ; PA3 GPIO pull-up
                                    604 $2:
                                    605         .endif
                                    606         .endif
                                    607         MOV     UART_CR5,#0x08 ; UART1 Half-Duplex
                                    608         .endif
                           000000   609         .else
                                    610         .ifne   HAS_TXUART
                                    611         MOV     UART_CR2,#0x08  ; UART1 enable tx
                                    612         .endif
                                    613         .ifne   HAS_RXUART
                                    614         MOV     UART_CR2,#0x04  ; UART1 enable rx
                                    615         .endif
                                    616         .endif
                                    617         .endif
                                    618 
      0081D7                        619         SSER_Init               ; macro for init of simulated serial, refer to sser.inc
                                    620 
      0081D7                        621         Board_IO_Init           ; macro board_io initialization (7S-LED)
                                      1         ; no LED-7Seg
                                    622 
      0081D7 AE 03 A0         [ 2]  623         LDW     X,#SPP          ; initialize data stack, TIB
                                    624         
      0081DA 5A               [ 2]  625         DECW X                  ; initialise get bit / set bit routines in ram
      0081DB 5A               [ 2]  626         DECW X  
      0081DC 90 AE 81 15      [ 2]  627         LDW Y,#COLD1
      0081E0 FF               [ 2]  628         LDW (X),Y
      0081E1 5A               [ 2]  629         DECW X  
      0081E2 5A               [ 2]  630         DECW X  
      0081E3 90 AE 00 0C      [ 2]  631         LDW Y,#BITAT  
      0081E7 FF               [ 2]  632         LDW (X),Y
      0081E8 5A               [ 2]  633         DECW X  
      0081E9 5A               [ 2]  634         DECW X  
      0081EA 90 AE 00 0F      [ 2]  635         LDW Y,#15
      0081EE FF               [ 2]  636         LDW (X),Y
      0081EF CD 86 0C         [ 4]  637         CALL CMOVE
                                    638 
                                    639 
                           000000   640 .if  HAS_RXSIM
                                    641 MOV USR_5,#255
                                    642 .endif
                                    643 
                                    644         ; Hardware initialization complete
      0081F2 9A               [ 1]  645         RIM                     ; enable interrupts
                                    646 
      0081F3 CD 81 9C         [ 4]  647 TBOOT:  CALL    RETURN       ; application boot, can be changed with ' appl 'BOOT flash!
      0081F6 8F               [10]  648 SLEEP:     WFI
                           000000   649 .if HAS_RXSIM
                                    650         TNZ USR_6
                                    651         JREQ $1
                                    652         CALL CHAT
                                    653 $1:    JRA      SLEEP
                                    654 .endif
                                    655 
                           000001   656 .if    HAS_RXUART*HAS_TXUART       
      0081F7 20 FD            [ 2]  657         JRA      SLEEP
      0081F9                        658 UART_INT:
      0081F9 CD 80 F9         [ 4]  659         CALL CHAT                 ; during chat data SP is communicated by muforth
      0081FC 1F 03            [ 2]  660         LDW (#3,SP),X             ; X (data SP) is popped from return stack during IRET
      0081FE 80               [11]  661         IRET
                                    662 .endif
                                    663 
                                    664 ; ==============================================
                                    665 
                                    666 ;       Device dependent I/O
                                    667 
                           000001   668         .ifne   HAS_RXUART
                                    669 ;       ?RX     ( -- c T | F )  ( TOS STM8: -- Y,Z,N )
                                    670 ;       Return serial interface input char from and true, or false.
                                    671 
      0081FF                        672         HEADER  QRX "?RX"
      0081FF                        673 QRX:
      0081FF 4F               [ 1]  674         CLR     A               ; A: flag false
      008200 72 0B 52 30 03   [ 2]  675         BTJF    UART_SR,#5,1$
      008205 C6 52 31         [ 1]  676         LD      A,UART_DR      ; get char in A
      008208                        677 1$:
      008208 CC 83 96         [ 2]  678      JP      ASTOR          ; push char
                                    679         .endif
                                    680 
                           000001   681         .ifne   HAS_TXUART
                                    682 ;       TX!     ( c -- )
                                    683 ;       Send character c to the serial interface.
                                    684 
      00820B                        685         HEADER  TXSTOR "TX!"
      00820B                        686 TXSTOR:
      00820B 5C               [ 1]  687         INCW    X
      00820C F6               [ 1]  688         LD      A,(X)
      00820D 5C               [ 1]  689         INCW    X
                                    690 
      00820E                        691         HEADER  TXASTOR "TXA!"
      00820E                        692 TXASTOR:
                           000000   693         .ifne   HALF_DUPLEX
                                    694         ; HALF_DUPLEX with normal UART (e.g. wired-or Rx and Tx)
                                    695 1$:     BTJF    UART_SR,#7,1$  ; loop until tdre
                                    696         BRES    UART_CR2,#2    ; disable rx
                                    697         LD      UART_DR,A      ; send A
                                    698 2$:     BTJF    UART_SR,#6,2$  ; loop until tc
                                    699         BSET    UART_CR2,#2    ; enable rx
                           000001   700         .else                  ; not HALF_DUPLEX
      00820E 72 0F 52 30 FB   [ 2]  701 1$:     BTJF    UART_SR,#7,1$  ; loop until tdre
      008213 C7 52 31         [ 1]  702         LD      UART_DR,A      ; send A
                                    703         .endif
      008216 81               [ 4]  704         RET
                                    705         .endif
                                    706 
                                    707 ; ==============================================
                                    708 
                                    709 ;       Device independent I/O
                                    710 
                                    711 ;       ?KEY    ( -- c T | F )  ( TOS STM8: -- Y,Z,N )
                                    712 ;       Return input char and true, or false.
      008217                        713         HEADER  QKEY "?KEY"
      008217                        714 QKEY:
      008217 92 CC 02         [ 5]  715         JP      [USRQKEY]
                                    716 
                                    717 ;       EMIT    ( c -- )
                                    718 ;       Send character c to output device.
                                    719 
      00821A                        720         HEADER  EMIT "EMIT"
      00821A                        721 EMIT:
      00821A 92 CC 00         [ 5]  722         JP      [USREMIT]
                                    723 
                                    724 ; ==============================================
                                    725 ; The kernel
                                    726 
                                    727 ;       ?branch ( f -- )
                                    728 ;       Branch if flag is zero.
                                    729 
      00821D                        730         HEADFLG QBRAN "?branch" COMPO
                                      1 
      00821D                        731 QBRAN:
      00821D 90 93            [ 1]  732         LDW     Y,X
      00821F 5C               [ 1]  733         INCW    X
      008220 5C               [ 1]  734         INCW    X
      008221 90 FE            [ 2]  735         LDW     Y,(Y)
      008223 27 05            [ 1]  736         JREQ    BRAN
      008225                        737 WSKIPRET:
      008225 90 85            [ 2]  738         POPW    Y
      008227 90 EC 02         [ 2]  739         JP      (2,Y)
                                    740 
                                    741 
                                    742 ;       branch  ( -- )
                                    743 ;       Branch to an inline address.
                                    744 
      00822A                        745         HEADFLG BRAN "branch" COMPO    ; NOALIAS
                                      1 
      00822A                        746 BRAN:
      00822A 90 85            [ 2]  747         POPW    Y
      00822C 90 FE            [ 2]  748         LDW     Y,(Y)
      00822E 90 FC            [ 2]  749         JP      (Y)
                                    750 
                                    751 
                                    752 ;       EXECUTE ( ca -- )
                                    753 ;       Execute word at ca.
                                    754 
      008230                        755         HEADER  EXECU "EXECUTE"
      008230                        756 EXECU:
      008230 90 93            [ 1]  757         LDW     Y,X
      008232 5C               [ 1]  758         INCW    X
      008233 5C               [ 1]  759         INCW    X
      008234 90 FE            [ 2]  760         LDW     Y,(Y)
      008236 90 FC            [ 2]  761         JP      (Y)
                                    762 
                           000001   763         .ifeq   BOOTSTRAP
                                    764 ;       2!      ( d a -- )      ( TOS STM8: -- Y,Z,N )
                                    765 ;       Store double integer to address a.
                                    766 
      008238                        767         HEADER  DSTOR "2!"
      008238                        768 DSTOR:
      008238 90 93            [ 1]  769         LDW Y,X
      00823A 90 FE            [ 2]  770         LDW Y,(Y)
      00823C E6 04            [ 1]  771         LD A,(4,X)
      00823E 90 F7            [ 1]  772         LD (Y),A
      008240 E6 05            [ 1]  773         LD A,(5,X)
      008242 90 E7 01         [ 1]  774         LD (1,Y),A
      008245 E6 02            [ 1]  775         LD A,(2,X)
      008247 90 E7 02         [ 1]  776         LD (2,Y),A
      00824A E6 03            [ 1]  777         LD A,(3,X)
      00824C 90 E7 03         [ 1]  778         LD (3,Y),A
      00824F 1C 00 06         [ 2]  779         ADDW X,#6
      008252 81               [ 4]  780         RET
                                    781         .endif
                                    782 
                                    783 ;       2@      ( a -- d )
                                    784 ;       Fetch double integer from address a.
                                    785 
      008253                        786         HEADER  DAT "2@"
      008253                        787 DAT:
      008253 90 93            [ 1]  788         LDW Y,X
      008255 FE               [ 2]  789         LDW X,(X)
      008256 F6               [ 1]  790         LD A,(X)
      008257 51               [ 1]  791         EXGW X,Y
      008258 F7               [ 1]  792         LD (X),A
      008259 90 E6 01         [ 1]  793         LD A,(1,Y)
      00825C E7 01            [ 1]  794         LD (1,X),A
      00825E 5A               [ 2]  795         DECW X
      00825F 90 E6 03         [ 1]  796         LD A,(3,Y)
      008262 F7               [ 1]  797         LD (X),A
      008263 5A               [ 2]  798         DECW X
      008264 90 E6 02         [ 1]  799         LD A,(2,Y)
      008267 F7               [ 1]  800         LD (X),A
      008268 81               [ 4]  801         RET
                                    802 
                                    803 ;       2C!  ( n a -- )
                                    804 ;       Store word C-wise to 16 bit HW registers "MSB first"
                                    805 
      008269                        806         HEADER  DCSTOR "2C!"
      008269                        807 DCSTOR:
      008269 90 93            [ 1]  808         LDW     Y,X
      00826B 5C               [ 1]  809         INCW    X
      00826C 5C               [ 1]  810         INCW    X
      00826D 90 FE            [ 2]  811         LDW     Y,(Y)
      00826F F6               [ 1]  812         LD      A,(X)
      008270 90 F7            [ 1]  813         LD      (Y),A           ; write MSB(n) to a
      008272 5C               [ 1]  814         INCW    X
      008273 F6               [ 1]  815         LD      A,(X)
      008274 90 E7 01         [ 1]  816         LD      (1,Y),A         ; write LSB(n) to a+1
      008277 5C               [ 1]  817         INCW    X
      008278 81               [ 4]  818         RET
                                    819 
                                    820 ;       2C@  ( a -- n )
                                    821 ;       Fetch word C-wise from 16 bit HW config. registers "MSB first"
                                    822 
      008279                        823         HEADER  DCAT "2C@"
      008279                        824 DCAT:
      008279 90 93            [ 1]  825         LDW     Y,X
      00827B FE               [ 2]  826         LDW     X,(X)
      00827C F6               [ 1]  827         LD      A,(X)
      00827D 90 F7            [ 1]  828         LD      (Y),A
      00827F E6 01            [ 1]  829         LD      A,(1,X)
      008281 51               [ 1]  830         EXGW    X,Y
      008282 E7 01            [ 1]  831         LD      (1,X),A
      008284 81               [ 4]  832         RET
                                    833 
                                    834 ;       BF@ ( a u -- 0|1)
                                    835 ;       Read bit #u (0..2047) in a cell array (16 bit words) at address a
                                    836 ;       Note: fills BITAT RAM-routine with stack values and jumps to BITAT
      008285                        837         HEADER  BFAT "BF@"
      008285                        838 BFAT:
      008285 90 93            [ 1]  839         LDW Y, X
      008287 FE               [ 2]  840         LDW X, (X)
      008288 A6 08            [ 1]  841         LD A,#8
      00828A 62               [ 2]  842         DIV X,A
      00828B 48               [ 1]  843         SLL A
      00828C 4C               [ 1]  844         INC A
      00828D B7 0E            [ 1]  845         LD BITAT+2,A
      00828F 9F               [ 1]  846         LD A,XL
      008290 A8 01            [ 1]  847         XOR A,#01
      008292 B7 09            [ 1]  848         LD XREG0+1,A
      008294 3F 08            [ 1]  849         CLR XREG0
      008296 93               [ 1]  850         LDW X,Y
      008297 EE 02            [ 2]  851         LDW X,(02,X)
      008299 72 BB 00 08      [ 2]  852         ADDW X,XREG0
      00829D BF 0F            [ 2]  853         LDW BITAT+3,X
      00829F 51               [ 1]  854         EXGW X,Y
      0082A0 5C               [ 1]  855         INCW X
      0082A1 5C               [ 1]  856         INCW X
      0082A2 CC 00 0C         [ 2]  857         JP BITAT
                                    858 
                                    859 ;       BF! ( a u -- 0|1)
                                    860 ;       Write bit to a bitfield stored in one or more cells (16 bit words)
                                    861 ;       Set/reset bit #u (0..8191) in a cell array starting at address a to bool t
      0082A5                        862         HEADER  BFSTO "BF!"
      0082A5                        863 BFSTO:
      0082A5 90 93            [ 1]  864         LDW Y,X
      0082A7 FE               [ 2]  865         LDW X,(X)
      0082A8 9F               [ 1]  866         LD A,XL
      0082A9 A4 07            [ 1]  867         AND A,#7
      0082AB 88               [ 1]  868         PUSH A
      0082AC 9F               [ 1]  869         LD A,XL
      0082AD 47               [ 1]  870         SRA A
      0082AE 47               [ 1]  871         SRA A
      0082AF 47               [ 1]  872         SRA A
      0082B0 A8 01            [ 1]  873         XOR A,#1
      0082B2 93               [ 1]  874         LDW X,Y
      0082B3 EB 03            [ 1]  875         ADD A,(3,X)
      0082B5 E7 03            [ 1]  876         LD (3,X),A
      0082B7 A6 00            [ 1]  877         LD A,#0
      0082B9 E9 02            [ 1]  878         ADC A,(2,X)
      0082BB E7 02            [ 1]  879         LD (2,X),A
      0082BD 84               [ 1]  880         POP A
      0082BE 7F               [ 1]  881         CLR (X)
      0082BF E7 01            [ 1]  882         LD (1,X),A         ; fall through
                                    883 
                                    884 ;       B! ( t a u -- )
                                    885 ;       Set/reset bit #u (0..7) in the byte at address a to bool t
                                    886 ;       Note: executes BSER/BRES + RET code in RAM
      0082C1                        887         HEADER  BRSS "B!"
      0082C1                        888 BRSS:
      0082C1 E6 01            [ 1]  889         LD A,(1,X)
      0082C3 48               [ 1]  890         SLL A                                     
      0082C4 AA 10            [ 1]  891         OR A,#16                                
      0082C6 B7 17            [ 1]  892         ld BITSTO+1,A                                  
      0082C8 E6 05            [ 1]  893         LD A,(05,X)                             
      0082CA 26 02            [ 1]  894         JRNE 1$                       
      0082CC 3C 17            [ 1]  895         INC BITSTO+1                                   
      0082CE E6 02            [ 1]  896 1$:     LD A,(02,X)                             
      0082D0 B7 18            [ 1]  897         LD BITSTO+2,A                                  
      0082D2 E6 03            [ 1]  898         LD A,(03,X)                             
      0082D4 B7 19            [ 1]  899         LD BITSTO+3,A                                  
      0082D6 1C 00 06         [ 2]  900         ADDW X,#6                               
      0082D9 CC 00 16         [ 2]  901         JP BITSTO                                    
                                    902 
                                    903 ;       B@ ( a u -- )
                                    904 ;       Get bit #u (0..7) in the byte at address a
                                    905 ;       Note: executes BSER/BRES + RET code in RAM
      0082DC                        906         HEADER  BAT "B@"
      0082DC                        907 BAT:
      0082DC E6 01            [ 1]  908         LD A,(1,X)
      0082DE 48               [ 1]  909         SLA A
      0082DF 4C               [ 1]  910         INC A
      0082E0 B7 0E            [ 1]  911         LD BITAT+2,A
      0082E2 E6 02            [ 1]  912         LD A,(2,X)
      0082E4 B7 0F            [ 1]  913         LD BITAT+3,A
      0082E6 E6 03            [ 1]  914         LD A,(3,X)
      0082E8 B7 10            [ 1]  915         LD BITAT+4,A
      0082EA 5C               [ 1]  916         INCW X
      0082EB 5C               [ 1]  917         INCW X
      0082EC 7F               [ 1]  918         CLR (X)
      0082ED CC 00 0C         [ 2]  919         JP BITAT
                                    920 
                                    921 ;       @       ( a -- w )      ( TOS STM8: -- Y,Z,N )
                                    922 ;       Push memory location to stack.
                                    923 
      0082F0                        924         HEADER  AT "@"
      0082F0                        925 AT:
      0082F0 90 93            [ 1]  926         LDW     Y,X
      0082F2 FE               [ 2]  927         LDW     X,(X)
      0082F3 FE               [ 2]  928         LDW     X,(X)
      0082F4 51               [ 1]  929         EXGW    X,Y
      0082F5 FF               [ 2]  930         LDW     (X),Y
      0082F6 81               [ 4]  931         RET
                                    932 
                                    933 ;       !       ( w a -- )      ( TOS STM8: -- Y,Z,N )
                                    934 ;       Pop data stack to memory.
                                    935 
      0082F7                        936         HEADER  STORE "!"
      0082F7                        937 STORE:
      0082F7 90 93            [ 1]  938         LDW     Y,X             ; (14 bytes, 16 cy)
      0082F9 5C               [ 1]  939         INCW    X
      0082FA 5C               [ 1]  940         INCW    X
      0082FB 90 FE            [ 2]  941         LDW     Y,(Y)
      0082FD 89               [ 2]  942         PUSHW   X
      0082FE FE               [ 2]  943         LDW     X,(X)           ; w
      0082FF 90 FF            [ 2]  944         LDW     (Y),X
      008301 85               [ 2]  945         POPW    X
      008302 5C               [ 1]  946         INCW    X
      008303 5C               [ 1]  947         INCW    X
      008304 81               [ 4]  948         RET
                                    949 
                                    950 ;       C@      ( a -- c )      ( TOS STM8: -- A,Z,N )
                                    951 ;       Push byte in memory to stack.
                                    952 ;       STM8: Z,N
                                    953 
      008305                        954         HEADER  CAT "C@"
      008305                        955 CAT:
      008305 90 93            [ 1]  956         LDW     Y,X             ; Y=a
      008307 90 FE            [ 2]  957         LDW     Y,(Y)
      008309                        958 YCAT:
      008309 90 F6            [ 1]  959         LD      A,(Y)
      00830B 7F               [ 1]  960         CLR     (X)
      00830C E7 01            [ 1]  961         LD      (1,X),A
      00830E 81               [ 4]  962         RET
                                    963 
                                    964 ;       C!      ( c a -- )
                                    965 ;       Pop     data stack to byte memory.
                                    966 
      00830F                        967         HEADER  CSTOR "C!"
      00830F                        968 CSTOR:
      00830F 90 93            [ 1]  969         LDW     Y,X
      008311 5C               [ 1]  970         INCW    X
      008312 5C               [ 1]  971         INCW    X
      008313 90 FE            [ 2]  972         LDW     Y,(Y)
      008315 5C               [ 1]  973         INCW    X
      008316 F6               [ 1]  974         LD      A,(X)
      008317 90 F7            [ 1]  975         LD      (Y),A
      008319 5C               [ 1]  976         INCW    X
      00831A 81               [ 4]  977         RET
                                    978 
                                    979 ;       OVER    ( w1 w2 -- w1 w2 w1 ) ( TOS STM8: -- Y,Z,N )
                                    980 ;       Copy second stack item to top.
                                    981 
      00831B                        982         HEADER  OVER "OVER"
      00831B                        983 OVER:
      00831B E6 03            [ 1]  984         LD A,(3,X)
      00831D 5A               [ 2]  985         DECW X
      00831E F7               [ 1]  986         LD (X),A
      00831F E6 03            [ 1]  987         LD A,(3,X)
      008321 5A               [ 2]  988         DECW X
      008322 F7               [ 1]  989         LD (X),A
      008323 81               [ 4]  990         RET
                                    991 
                                    992 ;       NIP     ( n1 n2 -- n2 )
                                    993 ;       Drop 2nd item on the stack.
                                    994 
      008324                        995         HEADER  NIP "NIP"
      008324                        996 NIP:
      008324 F6               [ 1]  997         LD A,(X)  
      008325 E7 02            [ 1]  998         LD (2,X),A  
      008327 E6 01            [ 1]  999         LD A,(1,X)  
      008329 E7 03            [ 1] 1000         LD (3,X),A  ; fall through
                                   1001 
                                   1002 ;       DROP     ( n1 -- )
                                   1003 ;       Drop top stack item.
                                   1004 
      00832B                       1005         HEADER  DROP "DROP"
      00832B 5C               [ 1] 1006 DROP:   INCW    X
      00832C 5C               [ 1] 1007         INCW    X
      00832D 81               [ 4] 1008         RET
                                   1009 
                                   1010 ;       DUP     ( w -- w w )    ( TOS STM8: -- Y,Z,N )
                                   1011 ;       Duplicate top stack item.
                                   1012 
      00832E                       1013         HEADER  DUPP "DUP"
      00832E                       1014 DUPP:
      00832E 90 93            [ 1] 1015         LDW     Y,X
      008330 90 FE            [ 2] 1016         LDW     Y,(Y)
      008332 5A               [ 2] 1017         DECW    X               ; SUBW  X,#2
      008333 5A               [ 2] 1018         DECW    X
      008334 FF               [ 2] 1019         LDW     (X),Y           ; push on stack
      008335 81               [ 4] 1020         RET
                                   1021 
                                   1022 ;       SWAP ( w1 w2 -- w2 w1 ) ( TOS STM8: -- Y,Z,N )
                                   1023 ;       Exchange top two stack items.
                                   1024 
      008336                       1025         HEADER  SWAPP "SWAP"
      008336                       1026 SWAPP:
      008336 90 93            [ 1] 1027         LDW     Y,X
      008338 EE 02            [ 2] 1028         LDW     X,(2,X)
      00833A 89               [ 2] 1029         PUSHW   X
      00833B 93               [ 1] 1030         LDW     X,Y
      00833C FE               [ 2] 1031         LDW     X,(X)
      00833D 51               [ 1] 1032         EXGW    X,Y
      00833E EF 02            [ 2] 1033         LDW     (2,X),Y
      008340 90 85            [ 2] 1034         POPW    Y
      008342 FF               [ 2] 1035         LDW     (X),Y
      008343 81               [ 4] 1036         RET
                                   1037 
                                   1038 ;       UM+     ( u u -- udsum )
                                   1039 ;       Add two unsigned single
                                   1040 ;       and return a double sum.
                                   1041 
      008344                       1042         HEADER  UPLUS "UM+"
      008344                       1043 UPLUS:
      008344 AD 05            [ 4] 1044         CALLR   PLUS
      008346 4F               [ 1] 1045         CLR     A
      008347 49               [ 1] 1046         RLC     A
      008348 CC 83 96         [ 2] 1047         JP      ASTOR
                                   1048 
                                   1049 ;       +       ( w w -- sum ) ( TOS STM8: -- Y,Z,N )
                                   1050 ;       Add top two items.
                                   1051 
      00834B                       1052         HEADER  PLUS "+"
                                   1053 
      00834B                       1054 PLUS:
      00834B E6 01            [ 1] 1055         LD      A,(1,X) ;D=w
      00834D EB 03            [ 1] 1056         ADD     A,(3,X)
      00834F E7 03            [ 1] 1057         LD      (3,X),A
      008351 F6               [ 1] 1058         LD      A,(X)
      008352 E9 02            [ 1] 1059         ADC     A,(2,X)
      008354                       1060 LDADROP:
      008354 5C               [ 1] 1061         INCW    X
      008355 5C               [ 1] 1062         INCW    X
      008356 F7               [ 1] 1063         LD      (X),A
      008357 81               [ 4] 1064         RET
                                   1065 
                                   1066 ;       XOR     ( w w -- w )    ( TOS STM8: -- Y,Z,N )
                                   1067 ;       Bitwise exclusive OR.
                                   1068 
      008358                       1069         HEADER  XORR "XOR"
      008358                       1070 XORR:
      008358 E6 01            [ 1] 1071         LD      A,(1,X)         ; D=w
      00835A E8 03            [ 1] 1072         XOR     A,(3,X)
      00835C E7 03            [ 1] 1073         LD      (3,X),A
      00835E F6               [ 1] 1074         LD      A,(X)
      00835F E8 02            [ 1] 1075         XOR     A,(2,X)
      008361 20 F1            [ 2] 1076         JRA     LDADROP
                                   1077 
                                   1078 ;       AND     ( w w -- w )    ( TOS STM8: -- Y,Z,N )
                                   1079 ;       Bitwise AND.
                                   1080 
      008363                       1081         HEADER  ANDD "AND"
      008363                       1082 ANDD:
      008363 E6 01            [ 1] 1083         LD      A,(1,X)         ; D=w
      008365 E4 03            [ 1] 1084         AND     A,(3,X)
      008367 E7 03            [ 1] 1085         LD      (3,X),A
      008369 F6               [ 1] 1086         LD      A,(X)
      00836A E4 02            [ 1] 1087         AND     A,(2,X)
      00836C 20 E6            [ 2] 1088         JRA     LDADROP
                                   1089 
                                   1090 ;       OR      ( w w -- w )    ( TOS STM8: -- immediate Y,Z,N )
                                   1091 ;       Bitwise inclusive OR.
                                   1092 
      00836E                       1093         HEADER  ORR "OR"
      00836E                       1094 ORR:
      00836E E6 01            [ 1] 1095         LD      A,(1,X)         ; D=w
      008370 EA 03            [ 1] 1096         OR      A,(3,X)
      008372 E7 03            [ 1] 1097         LD      (3,X),A
      008374 F6               [ 1] 1098         LD      A,(X)
      008375 EA 02            [ 1] 1099         OR      A,(2,X)
      008377 20 DB            [ 2] 1100         JRA     LDADROP
                                   1101 
                                   1102 ;       0<      ( n -- t ) ( TOS STM8: -- A,Z )
                                   1103 ;       Return true if n is negative.
                                   1104 
      008379                       1105         HEADER  ZLESS "0<"
      008379                       1106 ZLESS:
      008379 4F               [ 1] 1107         CLR     A
      00837A 7D               [ 1] 1108         TNZ     (X)
      00837B 2A 01            [ 1] 1109         JRPL    ZL1
      00837D 43               [ 1] 1110         CPL     A               ; true
      00837E F7               [ 1] 1111 ZL1:    LD      (X),A
      00837F E7 01            [ 1] 1112         LD      (1,X),A
      008381 81               [ 4] 1113         RET
                                   1114 
                                   1115 ;       -   ( n1 n2 -- n1-n2 )  ( TOS STM8: -- Y,Z,N )
                                   1116 ;       Subtraction.
                                   1117 
      008382                       1118         HEADER  SUBB "-"
                                   1119 
      008382                       1120 SUBB:
                           000000  1121         .ifeq   SPEEDOVERSIZE
                                   1122         CALL    NEGAT           ; (15 cy)
                                   1123         JRA     PLUS            ; 25 cy (15+10)
                           000001  1124         .else
      008382 90 93            [ 1] 1125         LDW     Y,X
      008384 90 FE            [ 2] 1126         LDW     Y,(Y)
      008386 90 BF 08         [ 2] 1127         LDW     XREG0,Y
      008389 5C               [ 1] 1128         INCW    X
      00838A 5C               [ 1] 1129         INCW    X
      00838B 90 93            [ 1] 1130         LDW     Y,X
      00838D 90 FE            [ 2] 1131         LDW     Y,(Y)
      00838F 72 B2 00 08      [ 2] 1132         SUBW    Y,XREG0
      008393 FF               [ 2] 1133         LDW     (X),Y
      008394 81               [ 4] 1134         RET                     ; 18 cy
                                   1135         .endif
                                   1136 
      008395                       1137 ZERO:
      008395 4F               [ 1] 1138         CLR A
                                   1139 
                                   1140 ;       A>  ( -- n )     ( TOS STM8: - Y,Z,N )
                                   1141 ;       push A to stack
                                   1142 
      008396                       1143         HEADER  ASTOR "A>"
      008396                       1144 ASTOR:
      008396 90 5F            [ 1] 1145         CLRW    Y
      008398 90 97            [ 1] 1146         LD      YL,A
      00839A                       1147 AYSTOR:
      00839A 5A               [ 2] 1148         DECW    X               ; SUBW  X,#2
      00839B 5A               [ 2] 1149         DECW    X
      00839C FF               [ 2] 1150         LDW     (X),Y           ; push on stack
      00839D 81               [ 4] 1151         RET
                                   1152 
                                   1153 ;       ATOKEY core ( -- c T | f )    ( TOS STM8: - Y,Z,N )
                                   1154 ;       Return input char and true, or false.
                                   1155 
      00839E                       1156         HEADER  ATOKEY "A>KEY"
      00839E                       1157 ATOKEY:
      00839E 4D               [ 1] 1158         TNZ     A
      00839F 27 F5            [ 1] 1159         JREQ    ASTOR
      0083A1 AD F3            [ 4] 1160         CALLR   ASTOR              ; push char
      0083A3 5A               [ 2] 1161         DECW X
      0083A4 5A               [ 2] 1162         DECW X
      0083A5 90 AE 0F FF      [ 2] 1163         LDW Y,#0XFFF
      0083A9 FF               [ 2] 1164         LDW (X),Y
      0083AA 81               [ 4] 1165         RET
                                   1166 
                                   1167 ; Common functions
                                   1168 
                                   1169 ;       ?DUP    ( w -- w w | 0 )   ( TOS STM8: -- Y,Z,N )
                                   1170 ;       Dup tos if its not zero.
      0083AB                       1171         HEADER  QDUP "?DUP"
      0083AB                       1172 QDUP:
      0083AB 90 93            [ 1] 1173         LDW     Y,X
      0083AD 90 FE            [ 2] 1174         LDW     Y,(Y)
      0083AF 27 03            [ 1] 1175         JREQ    QDUP1
      0083B1 5A               [ 2] 1176         DECW    X
      0083B2 5A               [ 2] 1177         DECW    X
      0083B3 FF               [ 2] 1178         LDW     (X),Y
      0083B4 81               [ 4] 1179 QDUP1:  RET
                                   1180 
                                   1181 ;       ROT     ( w1 w2 w3 -- w2 w3 w1 ) ( TOS STM8: -- Y,Z,N )
                                   1182 ;       Rot 3rd item to top.
                                   1183 
      0083B5                       1184         HEADER  ROT "ROT"
      0083B5                       1185 ROT:
                           000001  1186 .if 1
                                   1187 ; 31 bytes, 20 cy
      0083B5 F6               [ 1] 1188 LD A,(X)
      0083B6 B7 08            [ 1] 1189 LD XREG0,A
      0083B8 E6 01            [ 1] 1190 LD A,(1,X)
      0083BA B7 09            [ 1] 1191 LD XREG0+1,A
      0083BC E6 04            [ 1] 1192 LD A,(4,X)
      0083BE F7               [ 1] 1193 LD (X),A
      0083BF E6 05            [ 1] 1194 LD A,(5,X)
      0083C1 E7 01            [ 1] 1195 LD (1,X),A
      0083C3 E6 02            [ 1] 1196 LD A,(2,X)
      0083C5 E7 04            [ 1] 1197 LD (4,X),A
      0083C7 E6 03            [ 1] 1198 LD A,(3,X)
      0083C9 E7 05            [ 1] 1199 LD (5,X),A
      0083CB B6 08            [ 1] 1200 LD A,XREG0
      0083CD E7 02            [ 1] 1201 LD (2,X),A
      0083CF B6 09            [ 1] 1202 LD A,XREG0+1
      0083D1 E7 03            [ 1] 1203 LD (3,X),A
      0083D3 81               [ 4] 1204 RET
                           000000  1205 .else
                                   1206         .ifeq   SPEEDOVERSIZE
                                   1207         LD A,(1,X)              ; (17 bytes, 36 cy)
                                   1208         PUSH A
                                   1209         LD A,(X)
                                   1210         PUSH A
                                   1211         INCW X
                                   1212         INCW X
                                   1213         CALLR   1$
                                   1214         POPW Y
                                   1215         DECW X
                                   1216         DECW X
                                   1217         LDW (X),Y
                                   1218 1$:     JP      SWAPP
                                   1219         .else
                                   1220         LDW     Y,X             ; (22 bytes, 28 cy)
                                   1221         LDW     X,(4,X)
                                   1222         PUSHW   X
                                   1223         LDW     X,Y
                                   1224         LDW     X,(2,X)
                                   1225         PUSHW   X
                                   1226         LDW     X,Y
                                   1227         LDW     X,(X)
                                   1228         EXGW    X,Y
                                   1229         LDW     (2,X),Y
                                   1230         POPW    Y
                                   1231         LDW     (4,X),Y
                                   1232         POPW    Y
                                   1233         LDW     (X),Y
                                   1234         RET
                                   1235         .endif
                                   1236 .endif
                                   1237 
                                   1238 ;       2DUP    ( w1 w2 -- w1 w2 w1 w2 )
                                   1239 ;       Duplicate top two items.
                                   1240 
      0083D4                       1241         HEADER  DDUP "2DUP"
      0083D4                       1242 DDUP:
      0083D4 AD 00            [ 4] 1243         CALLR    1$
      0083D6                       1244 1$:
      0083D6 CC 83 1B         [ 2] 1245         JP      OVER
                                   1246 
                           000001  1247         .ifeq   UNLINKCORE
                                   1248 ;       DNEGATE ( d -- -d )     ( TOS STM8: -- Y,Z,N )
                                   1249 ;       Two's complement of top double.
                                   1250 
      0083D9                       1251         HEADER  DNEGA "DNEGATE"
      0083D9                       1252 DNEGA:
      0083D9 90 93            [ 1] 1253         LDW     Y,X
      0083DB 90 EE 02         [ 2] 1254         LDW     Y,(2,Y)
      0083DE 90 50            [ 2] 1255         NEGW    Y
      0083E0 8A               [ 1] 1256         PUSH    CC
      0083E1 EF 02            [ 2] 1257         LDW     (2,X),Y
      0083E3 90 93            [ 1] 1258         LDW     Y,X
      0083E5 90 FE            [ 2] 1259         LDW     Y,(Y)
      0083E7 90 53            [ 2] 1260         CPLW    Y
      0083E9 86               [ 1] 1261         POP     CC
      0083EA 25 02            [ 1] 1262         JRC     DN1
      0083EC 90 5C            [ 1] 1263         INCW    Y
      0083EE FF               [ 2] 1264 DN1:    LDW     (X),Y
      0083EF 81               [ 4] 1265         RET
                                   1266         .endif
                                   1267 
                                   1268 ;       =       ( w w -- t )    ( TOS STM8: -- Y,Z,N )
                                   1269 ;       Return true if top two are equal.
                                   1270 
      0083F0                       1271         HEADER  EQUAL "="
      0083F0                       1272 EQUAL:
                           000000  1273         .ifeq   SPEEDOVERSIZE
                                   1274         CALL    XORR
                                   1275         JP      ZEQUAL                 ; 31 cy= (18+13)
                           000001  1276         .else
      0083F0 90 5F            [ 1] 1277         CLRW Y                          ; (19 bytes, 17 cy)                
      0083F2 F6               [ 1] 1278         LD A,(X)                              
      0083F3 E0 02            [ 1] 1279         SUB A,(02,X)                         
      0083F5 26 08            [ 1] 1280         JRNE 1$                      
      0083F7 E6 01            [ 1] 1281         LD A,(01,X)                          
      0083F9 E0 03            [ 1] 1282         SUB A,(03,X)                         
      0083FB 26 02            [ 1] 1283         JRNE 1$                      
      0083FD 90 53            [ 2] 1284         CPLW Y                                 
      0083FF 5C               [ 1] 1285 1$:     INCW X                                 
      008400 5C               [ 1] 1286         INCW X                                 
      008401 FF               [ 2] 1287         LDW (X),Y                              
      008402 81               [ 4] 1288         RET
                                   1289         .endif                          ; 17 cy, 19 bytes
                                   1290 
                                   1291 
                                   1292 ;       U<      ( u u -- t )    ( TOS STM8: -- Y,Z,N )
                                   1293 ;       Unsigned compare of top two items.
                                   1294 
      008403                       1295         HEADER  ULESS "U<"
      008403                       1296 ULESS:
      008403 4F               [ 1] 1297         CLR     A
      008404 AD 25            [ 4] 1298         CALLR   XREG0CMP
      008406 24 01            [ 1] 1299         JRUGE   1$
      008408 43               [ 1] 1300         CPL     A
      008409 90 97            [ 1] 1301 1$:     LD      YL,A
      00840B 90 95            [ 1] 1302         LD      YH,A
      00840D FF               [ 2] 1303         LDW     (X),Y
      00840E 81               [ 4] 1304         RET
                                   1305 
                           000001  1306         .ifeq   BOOTSTRAP
                                   1307 ;       <       ( n1 n2 -- t )
                                   1308 ;       Signed compare of top two items.
                                   1309 
      00840F                       1310         HEADER  LESS "<"
      00840F                       1311 LESS:
                           000000  1312         .ifeq   SPEEDOVERSIZE
                                   1313         CALL    SUBB             ; (29cy)
                                   1314         JP      ZLESS            ; 41 cy (12+29)
                           000001  1315         .else
      00840F 4F               [ 1] 1316         CLR     A
      008410 90 93            [ 1] 1317         LDW     Y,X
      008412 90 FE            [ 2] 1318         LDW     Y,(Y)
      008414 90 BF 08         [ 2] 1319         LDW     XREG0,Y
      008417 5C               [ 1] 1320         INCW    X
      008418 5C               [ 1] 1321         INCW    X
      008419 90 93            [ 1] 1322         LDW     Y,X
      00841B 90 FE            [ 2] 1323         LDW     Y,(Y)
      00841D 90 B3 08         [ 2] 1324         CPW     Y,XREG0
      008420 2E 01            [ 1] 1325         JRSGE   1$
      008422 43               [ 1] 1326         CPL     A
      008423 F7               [ 1] 1327 1$:     LD      (X),A
      008424 E7 01            [ 1] 1328         LD      (1,X),A
      008426 90 93            [ 1] 1329         LDW     Y,X
      008428 90 FE            [ 2] 1330         LDW     Y,(Y)
      00842A 81               [ 4] 1331         RET                      ; 26 cy
                                   1332         .endif
                                   1333         .endif
                                   1334 
                                   1335 ;       XREG0CMP       ( n n - n )      ( TOS STM8: - Y,Z,N )
                                   1336 ;       Load (TOS) to XREG0 and (TOS-1) to Y, DROP, CMP to STM8 flags
      00842B                       1337 XREG0CMP:
      00842B 90 93            [ 1] 1338         LDW     Y,X
      00842D 5C               [ 1] 1339         INCW    X
      00842E 5C               [ 1] 1340         INCW    X
      00842F 51               [ 1] 1341         EXGW    X,Y
      008430 FE               [ 2] 1342         LDW     X,(X)
      008431 BF 08            [ 2] 1343         LDW     XREG0,X
      008433 93               [ 1] 1344         LDW     X,Y
      008434 FE               [ 2] 1345         LDW     X,(X)
      008435 B3 08            [ 2] 1346         CPW     X,XREG0
      008437 51               [ 1] 1347         EXGW    X,Y
      008438 81               [ 4] 1348         RET
                                   1349 
                           000001  1350         .ifeq   BOOTSTRAP
                                   1351 ;       MAX     ( n n -- n )    ( TOS STM8: -- Y,Z,N )
                                   1352 ;       Return greater of two top items.
                                   1353 
      008439                       1354         HEADER  MAX "MAX"
      008439                       1355 MAX:
      008439 AD F0            [ 4] 1356         CALLR   XREG0CMP
      00843B 2C 04            [ 1] 1357         JRSGT   MMEXIT
      00843D                       1358 XREG0TOS:
      00843D 90 BE 08         [ 2] 1359         LDW     Y,XREG0
      008440 FF               [ 2] 1360         LDW     (X),Y
      008441                       1361 MMEXIT:
      008441 81               [ 4] 1362         RET
                                   1363         .endif
                                   1364 
                           000001  1365         .ifeq   BOOTSTRAP
                                   1366 ;       MIN     ( n n -- n )    ( TOS STM8: -- Y,Z,N )
                                   1367 ;       Return smaller of top two items.
                                   1368 
      008442                       1369         HEADER  MIN "MIN"
      008442                       1370 MIN:
      008442 AD E7            [ 4] 1371         CALLR   XREG0CMP
      008444 2F FB            [ 1] 1372         JRSLT   MMEXIT
      008446 20 F5            [ 2] 1373         JRA     XREG0TOS
                                   1374         .endif
                                   1375 
                                   1376 ;       WITHIN ( u ul uh -- t ) ( TOS STM8: -- Y,Z,N )
                                   1377 ;       Return true if u is within
                                   1378 ;       range of ul and uh. ( ul <= u < uh )
                                   1379 
      008448                       1380         HEADER  WITHI "WITHIN"
      008448                       1381 WITHI:
      008448 CD 83 1B         [ 4] 1382         CALL    OVER
      00844B CD 83 82         [ 4] 1383         CALL    SUBB
      00844E E6 01            [ 1] 1384         LD A,(1,X)
      008450 88               [ 1] 1385         PUSH A
      008451 F6               [ 1] 1386         LD A,(X)
      008452 88               [ 1] 1387         PUSH A
      008453 5C               [ 1] 1388         INCW X
      008454 5C               [ 1] 1389         INCW X
      008455 CD 83 82         [ 4] 1390         CALL    SUBB
      008458 90 85            [ 2] 1391         POPW Y
      00845A 5A               [ 2] 1392         DECW X
      00845B 5A               [ 2] 1393         DECW X
      00845C FF               [ 2] 1394         LDW (X),Y
      00845D 20 A4            [ 2] 1395         JRA     ULESS
                                   1396 
                                   1397 ; Divide
                                   1398 
                                   1399 ;       UM/MOD  ( udl udh un -- ur uq )
                                   1400 ;       Unsigned divide of a double by a
                                   1401 ;       single. Return mod and quotient.
                                   1402 
      00845F                       1403         HEADER  UMMOD "UM/MOD"
      00845F                       1404 UMMOD:
      00845F 90 93            [ 1] 1405         LDW     Y,X             ; stack pointer to Y
      008461 FE               [ 2] 1406         LDW     X,(X)           ; un
      008462 BF 08            [ 2] 1407         LDW     XREG0,X         ; save un
      008464 93               [ 1] 1408         LDW     X,Y
      008465 5C               [ 1] 1409         INCW    X               ; drop un
      008466 5C               [ 1] 1410         INCW    X
      008467 89               [ 2] 1411         PUSHW   X               ; save stack pointer
      008468 FE               [ 2] 1412         LDW     X,(X)           ; X=udh
      008469 90 EE 04         [ 2] 1413         LDW     Y,(4,Y)         ; Y=udl (offset before drop)
      00846C B3 08            [ 2] 1414         CPW     X,XREG0
      00846E 25 09            [ 1] 1415         JRULT   MMSM1           ; X is still on the R-stack
      008470 85               [ 2] 1416         POPW    X               ; restore stack pointer
      008471 90 5F            [ 1] 1417         CLRW    Y
      008473 EF 02            [ 2] 1418         LDW     (2,X),Y         ; remainder 0
      008475 90 5A            [ 2] 1419         DECW    Y
      008477 FF               [ 2] 1420         LDW     (X),Y           ; quotient max. 16 bit value
      008478 81               [ 4] 1421         RET
      008479                       1422 MMSM1:
      008479 A6 10            [ 1] 1423         LD      A,#16           ; loop count
      00847B 90 58            [ 2] 1424         SLLW    Y               ; udl shift udl into udh
      00847D                       1425 MMSM3:
      00847D 59               [ 2] 1426         RLCW    X               ; rotate udl bit into uhdh (= remainder)
      00847E 25 04            [ 1] 1427         JRC     MMSMa           ; if carry out of rotate
      008480 B3 08            [ 2] 1428         CPW     X,XREG0         ; compare udh to un
      008482 25 05            [ 1] 1429         JRULT   MMSM4           ; can't subtract
      008484                       1430 MMSMa:
      008484 72 B0 00 08      [ 2] 1431         SUBW    X,XREG0         ; can subtract
      008488 98               [ 1] 1432         RCF
      008489                       1433 MMSM4:
      008489 8C               [ 1] 1434         CCF                     ; quotient bit
      00848A 90 59            [ 2] 1435         RLCW    Y               ; rotate into quotient, rotate out udl
      00848C 4A               [ 1] 1436         DEC     A               ; repeat
      00848D 26 EE            [ 1] 1437         JRNE    MMSM3           ; if A == 0
      00848F                       1438 MMSMb:
      00848F BF 08            [ 2] 1439         LDW     XREG0,X         ; done, save remainder
      008491 85               [ 2] 1440         POPW    X               ; restore stack pointer
      008492 FF               [ 2] 1441         LDW     (X),Y           ; save quotient
      008493 90 BE 08         [ 2] 1442         LDW     Y,XREG0         ; remainder onto stack
      008496 EF 02            [ 2] 1443         LDW     (2,X),Y
      008498 81               [ 4] 1444         RET
                                   1445 
                           000001  1446         .ifeq   UNLINKCORE
                                   1447 ;       M/MOD   ( d n -- r q )
                                   1448 ;       Signed floored divide of double by
                                   1449 ;       single. Return mod and quotient.
                                   1450 
      008499                       1451         HEADER  MSMOD "M/MOD"
      008499                       1452 MSMOD:
      008499 F6               [ 1] 1453         LD      A,(X)           ; DUPP ZLESS
      00849A 88               [ 1] 1454         PUSH    A               ; DUPP TOR
      00849B 2A 12            [ 1] 1455         JRPL    MMOD1           ; QBRAN
      00849D CD 85 81         [ 4] 1456         CALL    NEGAT
      0084A0 E6 01            [ 1] 1457         LD A,(1,X)
      0084A2 88               [ 1] 1458         PUSH A
      0084A3 F6               [ 1] 1459         LD A,(X)
      0084A4 88               [ 1] 1460         PUSH A
      0084A5 5C               [ 1] 1461         INCW X
      0084A6 5C               [ 1] 1462         INCW X
      0084A7 CD 83 D9         [ 4] 1463         CALL    DNEGA
      0084AA 90 85            [ 2] 1464         POPW Y
      0084AC 5A               [ 2] 1465         DECW X
      0084AD 5A               [ 2] 1466         DECW X
      0084AE FF               [ 2] 1467         LDW (X),Y
      0084AF                       1468 MMOD1:
      0084AF E6 01            [ 1] 1469         LD A,(1,X)
      0084B1 88               [ 1] 1470         PUSH A
      0084B2 F6               [ 1] 1471         LD A,(X)
      0084B3 88               [ 1] 1472         PUSH A
      0084B4 5C               [ 1] 1473         INCW X
      0084B5 5C               [ 1] 1474         INCW X
      0084B6 90 93            [ 1] 1475         LDW     Y,X
      0084B8 90 FE            [ 2] 1476         LDW     Y,(Y)
      0084BA 2A 0A            [ 1] 1477         JRPL    MMOD2           ; DUPP ZLESS QBRAN
      0084BC 90 85            [ 2] 1478         POPW Y
      0084BE 5A               [ 2] 1479         DECW X
      0084BF 5A               [ 2] 1480         DECW X
      0084C0 FF               [ 2] 1481         LDW (X),Y
      0084C1 90 89            [ 2] 1482         PUSHW Y
      0084C3 CD 83 4B         [ 4] 1483         CALL    PLUS
      0084C6                       1484 MMOD2:  
      0084C6 90 85            [ 2] 1485         POPW Y
      0084C8 5A               [ 2] 1486         DECW X
      0084C9 5A               [ 2] 1487         DECW X
      0084CA FF               [ 2] 1488         LDW (X),Y
      0084CB AD 92            [ 4] 1489         CALLR   UMMOD
      0084CD 84               [ 1] 1490         POP     A               ; RFROM
      0084CE 4D               [ 1] 1491         TNZ     A
      0084CF 2A 09            [ 1] 1492         JRPL    MMOD3           ; QBRAN
      0084D1 CD 83 36         [ 4] 1493         CALL    SWAPP
      0084D4 CD 85 81         [ 4] 1494         CALL    NEGAT
      0084D7 CC 83 36         [ 2] 1495         JP      SWAPP
      0084DA 81               [ 4] 1496 MMOD3:  RET
                                   1497 
                                   1498 ;       /MOD    ( n n -- r q )
                                   1499 ;       Signed divide. Return mod and quotient.
                                   1500 
      0084DB                       1501         HEADER  SLMOD "/MOD"
      0084DB                       1502 SLMOD:
      0084DB CD 83 1B         [ 4] 1503         CALL    OVER
      0084DE CD 83 79         [ 4] 1504         CALL    ZLESS
      0084E1 CD 83 36         [ 4] 1505         CALL    SWAPP
      0084E4 20 B3            [ 2] 1506         JRA     MSMOD
                                   1507 
                                   1508 ;       MOD     ( n n -- r )    ( TOS STM8: -- Y,Z,N )
                                   1509 ;       Signed divide. Return mod only.
                                   1510 
      0084E6                       1511         HEADER  MMOD "MOD"
      0084E6                       1512 MMOD:
      0084E6 AD F3            [ 4] 1513         CALLR   SLMOD
      0084E8 5C               [ 1] 1514         INCW    X
      0084E9 5C               [ 1] 1515         INCW    X
      0084EA 81               [ 4] 1516         RET
                                   1517 
                                   1518 ;       /       ( n n -- q )    ( TOS STM8: -- Y,Z,N )
                                   1519 ;       Signed divide. Return quotient only.
                                   1520 
      0084EB                       1521         HEADER  SLASH "/"
      0084EB                       1522 SLASH:
      0084EB AD EE            [ 4] 1523         CALLR   SLMOD
      0084ED CC 83 24         [ 2] 1524         JP      NIP
                                   1525         .endif
                                   1526 
                                   1527 ; Multiply
                                   1528 
                                   1529 ;       UM*     ( u u -- ud )
                                   1530 ;       Unsigned multiply. Return double product.
                                   1531 
      0084F0                       1532         HEADER  UMSTA "UM*"
      0084F0                       1533 UMSTA:                          ; stack have 4 bytes u1=a,b u2=c,d
      0084F0 E6 02            [ 1] 1534         LD      A,(2,X)         ; b
      0084F2 90 97            [ 1] 1535         LD      YL,A
      0084F4 F6               [ 1] 1536         LD      A,(X)           ; d
      0084F5 90 42            [ 4] 1537         MUL     Y,A
      0084F7 90 89            [ 2] 1538         PUSHW   Y               ; PROD1 temp storage
      0084F9 E6 03            [ 1] 1539         LD      A,(3,X)         ; a
      0084FB 90 97            [ 1] 1540         LD      YL,A
      0084FD F6               [ 1] 1541         LD      A,(X)           ; d
      0084FE 90 42            [ 4] 1542         MUL     Y,A
      008500 90 89            [ 2] 1543         PUSHW   Y               ; PROD2 temp storage
      008502 E6 02            [ 1] 1544         LD      A,(2,X)         ; b
      008504 90 97            [ 1] 1545         LD      YL,A
      008506 E6 01            [ 1] 1546         LD      A,(1,X)         ; c
      008508 90 42            [ 4] 1547         MUL     Y,A
      00850A 90 89            [ 2] 1548         PUSHW   Y               ; PROD3,CARRY temp storage
      00850C E6 03            [ 1] 1549         LD      A,(3,X)         ; a
      00850E 90 97            [ 1] 1550         LD      YL,A
      008510 E6 01            [ 1] 1551         LD      A,(1,X)         ; c
      008512 90 42            [ 4] 1552         MUL     Y,A             ; least signifiant product
      008514 4F               [ 1] 1553         CLR     A
      008515 90 01            [ 1] 1554         RRWA    Y
      008517 E7 03            [ 1] 1555         LD      (3,X),A         ; store least significant byte
      008519 72 F9 01         [ 2] 1556         ADDW    Y,(1,SP)        ; PROD3
      00851C 4F               [ 1] 1557         CLR     A
      00851D 49               [ 1] 1558         RLC     A               ; save carry
      00851E 6B 01            [ 1] 1559         LD      (1,SP),A        ; CARRY
      008520 72 F9 03         [ 2] 1560         ADDW    Y,(3,SP)        ; PROD2
      008523 7B 01            [ 1] 1561         LD      A,(1,SP)        ; CARRY
      008525 A9 00            [ 1] 1562         ADC     A,#0            ; add 2nd carry
      008527 6B 01            [ 1] 1563         LD      (1,SP),A        ; CARRY
      008529 4F               [ 1] 1564         CLR     A
      00852A 90 01            [ 1] 1565         RRWA    Y
      00852C E7 02            [ 1] 1566         LD      (2,X),A         ; 2nd product byte
      00852E 72 F9 05         [ 2] 1567         ADDW    Y,(5,SP)        ; PROD1
      008531 90 01            [ 1] 1568         RRWA    Y
      008533 E7 01            [ 1] 1569         LD      (1,X),A         ; 3rd product byte
      008535 90 01            [ 1] 1570         RRWA    Y               ; 4th product byte now in A
      008537 19 01            [ 1] 1571         ADC     A,(1,SP)        ; CARRY
      008539 F7               [ 1] 1572         LD      (X),A
      00853A 5B 06            [ 2] 1573         ADDW    SP,#6           ; drop temp storage
      00853C 81               [ 4] 1574         RET
                                   1575 
                                   1576 ;       *       ( n n -- n )    ( TOS STM8: -- Y,Z,N )
                                   1577 ;       Signed multiply. Return single product.
                                   1578 
      00853D                       1579         HEADER  STAR "*"
      00853D                       1580 STAR:
      00853D AD B1            [ 4] 1581         CALLR   UMSTA
      00853F 5C               [ 1] 1582         INCW    X
      008540 5C               [ 1] 1583         INCW    X
      008541 81               [ 4] 1584         RET
                           000001  1585         .ifeq   UNLINKCORE
                                   1586 ;       M*      ( n n -- d )
                                   1587 ;       Signed multiply. Return double product.
      008542                       1588         HEADER  MSTAR "M*"
      008542                       1589 MSTAR:
      008542 E6 02            [ 1] 1590         LD      A,(2,X)         ; DDUP
      008544 F8               [ 1] 1591         XOR     A,(X)           ; XORR
      008545 88               [ 1] 1592         PUSH    A               ; TOR
      008546 CD 85 89         [ 4] 1593         CALL    ABSS
      008549 CD 83 36         [ 4] 1594         CALL    SWAPP
      00854C CD 85 89         [ 4] 1595         CALL    ABSS
      00854F AD 9F            [ 4] 1596         CALLR   UMSTA
      008551 84               [ 1] 1597         POP     A               ; RFROM
      008552 4D               [ 1] 1598         TNZ     A
      008553 2A 03            [ 1] 1599         JRPL    MSTA1           ; QBRAN
      008555 CC 83 D9         [ 2] 1600         JP      DNEGA
      008558 81               [ 4] 1601 MSTA1:  RET
                                   1602 
                                   1603 ;       */MOD   ( n1 n2 n3 -- r q )
                                   1604 ;       Multiply n1 and n2, then divide
                                   1605 ;       by n3. Return mod and quotient.
      008559                       1606         HEADER  SSMOD "*/MOD"
      008559                       1607 SSMOD:
      008559 E6 01            [ 1] 1608         LD A,(1,X)
      00855B 88               [ 1] 1609         PUSH A
      00855C F6               [ 1] 1610         LD A,(X)
      00855D 88               [ 1] 1611         PUSH A
      00855E 5C               [ 1] 1612         INCW X
      00855F 5C               [ 1] 1613         INCW X
      008560 AD E0            [ 4] 1614         CALLR   MSTAR
      008562 90 85            [ 2] 1615         POPW Y
      008564 5A               [ 2] 1616         DECW X
      008565 5A               [ 2] 1617         DECW X
      008566 FF               [ 2] 1618         LDW (X),Y
      008567 CC 84 99         [ 2] 1619         JP      MSMOD
                                   1620 
                                   1621 ;       */      ( n1 n2 n3 -- q )    ( TOS STM8: -- Y,Z,N )
                                   1622 ;       Multiply n1 by n2, then divide
                                   1623 ;       by n3. Return quotient only.
      00856A                       1624         HEADER  STASL "*/"
      00856A                       1625 STASL:
      00856A AD ED            [ 4] 1626         CALLR   SSMOD
      00856C CC 83 24         [ 2] 1627         JP      NIP
                                   1628         .endif
                                   1629 
                                   1630 ; Miscellaneous
                                   1631 
                                   1632 
                           000001  1633         .ifeq   BAREBONES
                                   1634 ;       EXG      ( n -- n )      ( TOS STM8: -- Y,Z,N )
                                   1635 ;       Exchange high with low byte of n.
                                   1636 
      00856F                       1637         HEADER  EXG "EXG"
      00856F                       1638 EXG:
      00856F                       1639         LDW_Y_CONTENT_X
      00856F 90 93            [ 1]    1         LDW Y,X
      008571 90 FE            [ 2]    2         LDW Y,(Y)		; tos in Y
      008573 90 5E            [ 1] 1640         SWAPW   Y
      008575 FF               [ 2] 1641         LDW (X),Y
      008576 81               [ 4] 1642         RET
                                   1643         .endif
                                   1644 
                                   1645 ;        .ifeq   BOOTSTRAP
                                   1646 
                                   1647 ;       2+      ( a -- a )      ( TOS STM8: -- Y,Z,N )
                                   1648 ;       Add 2 to tos.
                                   1649 
      008577                       1650         HEADER  CELLP "2+"
      008577                       1651 CELLP:
      008577                       1652         LDW_Y_CONTENT_X
      008577 90 93            [ 1]    1         LDW Y,X
      008579 90 FE            [ 2]    2         LDW Y,(Y)		; tos in Y
      00857B 90 5C            [ 1] 1653         INCW    Y
      00857D 90 5C            [ 1] 1654         INCW    Y
      00857F FF               [ 2] 1655         LDW (X),Y
      008580 81               [ 4] 1656         RET
                                   1657 
                                   1658 ;       NEGATE  ( n -- -n )     ( TOS STM8: -- Y,Z,N )
                                   1659 ;       Two's complement of TOS.
                                   1660 
      008581                       1661         HEADER  NEGAT "NEGATE"
      008581                       1662 NEGAT:
      008581                       1663         LDW_Y_CONTENT_X
      008581 90 93            [ 1]    1         LDW Y,X
      008583 90 FE            [ 2]    2         LDW Y,(Y)		; tos in Y
      008585 90 50            [ 2] 1664         NEGW    Y
      008587 FF               [ 2] 1665         LDW (X),Y
      008588 81               [ 4] 1666         RET
                                   1667 
                                   1668 ;       ABS     ( n -- n )      ( TOS STM8: -- Y,Z,N )
                                   1669 ;       Return  absolute value of n.
                                   1670 
      008589                       1671         HEADER  ABSS "ABS"
      008589                       1672 ABSS:
      008589                       1673         LDW_Y_CONTENT_X
      008589 90 93            [ 1]    1         LDW Y,X
      00858B 90 FE            [ 2]    2         LDW Y,(Y)		; tos in Y
      00858D 2A 03            [ 1] 1674         JRPL    1$              ; positive?
      00858F 90 50            [ 2] 1675         NEGW    Y               ; else negate
      008591 FF               [ 2] 1676         LDW (X),Y
      008592 81               [ 4] 1677 1$:     RET
                                   1678 
                                   1679 ;       0=      ( n -- t )      ( TOS STM8: -- Y,Z,N ))
                                   1680 ;       Return true if n is equal to 0
                                   1681 
      008593                       1682         HEADER  ZEQUAL "0="
      008593                       1683 ZEQUAL:
      008593                       1684         LDW_Y_CONTENT_X
      008593 90 93            [ 1]    1         LDW Y,X
      008595 90 FE            [ 2]    2         LDW Y,(Y)		; tos in Y
      008597 27 04            [ 1] 1685         JREQ    CPLW
      008599 90 5F            [ 1] 1686 CLRW:   CLRW    Y
      00859B FF               [ 2] 1687         LDW (X),Y
      00859C 81               [ 4] 1688         RET
      00859D 90 53            [ 2] 1689 CPLW:   CPLW    Y               ; else -1
      00859F FF               [ 2] 1690         LDW (X),Y
      0085A0 81               [ 4] 1691         RET
                                   1692 
                                   1693 ;       !0=      ( n -- t )      ( TOS STM8: -- Y,Z,N ))
                                   1694 ;       Return true if n is not equal to 0
                                   1695 
      0085A1                       1696         HEADER  ZEQUAL "!0="
      0085A1                       1697 NZEQUAL:
      0085A1                       1698         LDW_Y_CONTENT_X
      0085A1 90 93            [ 1]    1         LDW Y,X
      0085A3 90 FE            [ 2]    2         LDW Y,(Y)		; tos in Y
      0085A5 26 02            [ 1] 1699         JRNE    1$
      0085A7 20 F0            [ 2] 1700         JRA     CLRW
      0085A9 90 5F            [ 1] 1701 1$:     CLRW    Y
      0085AB 20 F0            [ 2] 1702         JRA     CPLW
                                   1703 
                                   1704 ;       PICK    ( ... +n -- ... w )      ( TOS STM8: -- Y,Z,N )
                                   1705 ;       Copy    nth stack item to tos.
                                   1706 
      0085AD                       1707         HEADER  PICK "PICK"
      0085AD                       1708 PICK:
      0085AD                       1709         LDW_Y_CONTENT_X
      0085AD 90 93            [ 1]    1         LDW Y,X
      0085AF 90 FE            [ 2]    2         LDW Y,(Y)		; tos in Y
      0085B1 90 5C            [ 1] 1710         INCW    Y
      0085B3 90 58            [ 2] 1711         SLAW    Y
      0085B5 BF 08            [ 2] 1712         LDW     XREG0,X
      0085B7 72 B9 00 08      [ 2] 1713         ADDW    Y,XREG0
      0085BB 90 FE            [ 2] 1714         LDW     Y,(Y)
      0085BD FF               [ 2] 1715         LDW (X),Y
      0085BE 81               [ 4] 1716         RET
                                   1717 
                                   1718 ;       DEPTH   ( -- n )      ( TOS STM8: -- Y,Z,N )
                                   1719 ;       Return  depth of data stack.
                                   1720 
      0085BF                       1721         HEADER  DEPTH "DEPTH"
      0085BF                       1722 DEPTH:
      0085BF 90 93            [ 1] 1723         LDW     Y,X
      0085C1 50               [ 2] 1724         NEGW    X
      0085C2 1C 03 A0         [ 2] 1725         ADDW    X,#SPP
      0085C5 57               [ 2] 1726         SRAW    X
      0085C6 51               [ 1] 1727 XSTOR:  EXGW    X,Y
      0085C7 5A               [ 2] 1728         DECW    X               ; SUBW  X,#2
      0085C8 5A               [ 2] 1729         DECW    X
      0085C9 FF               [ 2] 1730         LDW     (X),Y           ; push on stack
      0085CA 81               [ 4] 1731         RET                     ; go to RET of EXEC
                                   1732 
                                   1733 ; Memory access
                                   1734 
                                   1735 ;       +!      ( n a -- )      ( TOS STM8: -- Y,Z,N )
                                   1736 ;       Add n to contents at address a.
                                   1737 
      0085CB                       1738         HEADER  PSTOR "+!"
      0085CB                       1739 PSTOR:
      0085CB BF 08            [ 2] 1740         LDW XREG0,X
      0085CD EE 02            [ 2] 1741         LDW X,(2,X)
      0085CF 89               [ 2] 1742         PUSHW X
      0085D0 BE 08            [ 2] 1743         LDW X,XREG0
      0085D2 FE               [ 2] 1744         LDW X,(X)        ; addr
      0085D3 90 93            [ 1] 1745         LDW Y,X
      0085D5 FE               [ 2] 1746         LDW X,(X)
      0085D6 72 FB 01         [ 2] 1747         ADDW X,(1,SP)
      0085D9 90 FF            [ 2] 1748         LDW (Y),X
      0085DB 85               [ 2] 1749         POPW X
      0085DC BE 08            [ 2] 1750 ENDPP:	LDW X,XREG0
      0085DE                       1751         HEADER  DDROP "2DROP"
      0085DE                       1752 DDROP:
      0085DE 1C 00 04         [ 2] 1753         ADDW    X,#4
      0085E1 81               [ 4] 1754         RET
                                   1755 
                                   1756 ;       +C!      ( n a -- )      ( TOS STM8: -- Y,Z,N )
                                   1757 ;       Add n to contents at address a.
                                   1758 
      0085E2                       1759         HEADER  PCSTOR "+C!"
      0085E2                       1760 PCSTOR:
      0085E2 BF 08            [ 2] 1761         LDW XREG0,X
      0085E4 FE               [ 2] 1762         LDW X,(X)        ; addr
      0085E5 90 93            [ 1] 1763         LDW Y,X
      0085E7 F6               [ 1] 1764         LD A,(X)
      0085E8 BE 08            [ 2] 1765         LDW X,XREG0
      0085EA EB 03            [ 1] 1766         ADD A,(3,X)
      0085EC 90 F7            [ 1] 1767         LD (Y),A
      0085EE 20 EC            [ 2] 1768         JRA ENDPP
                                   1769 
                                   1770 ;       @EXECUTE        ( a -- )  ( TOS STM8: undefined )
                                   1771 ;       Execute vector stored in address a.
                                   1772 
      0085F0                       1773         HEADER  ATEXE "@EXECUTE"
      0085F0                       1774 ATEXE:
      0085F0 90 93            [ 1] 1775         LDW     Y,X
      0085F2 5C               [ 1] 1776         INCW    X
      0085F3 5C               [ 1] 1777         INCW    X
      0085F4 90 FE            [ 2] 1778         LDW     Y,(Y)
      0085F6 90 FE            [ 2] 1779         LDW     Y,(Y)
      0085F8 27 02            [ 1] 1780         JREQ    1$
      0085FA 90 FC            [ 2] 1781         JP      (Y)
      0085FC 81               [ 4] 1782 1$:     RET
                                   1783 
                                   1784 ;       21+     ( u u -- u+1 u+1)
                                   1785  
      0085FD                       1786         HEADER  TWOONEPLUS "21+"
      0085FD                       1787 TWOONEPLUS:
      0085FD 90 93            [ 1] 1788         LDW Y,X 
      0085FF FE               [ 2] 1789         LDW X,(X) 
      008600 5C               [ 1] 1790         INCW X  
      008601 90 FF            [ 2] 1791         LDW (Y),X 
      008603 93               [ 1] 1792         LDW X,Y  
      008604 EE 02            [ 2] 1793         LDW X,(2,X) 
      008606 5C               [ 1] 1794         INCW X 
      008607 90 EF 02         [ 2] 1795         LDW (2,Y),X 
      00860A 93               [ 1] 1796         LDW X,Y
      00860B 81               [ 4] 1797         RET
                                   1798 
                                   1799 ;       CMOVE   ( b1 b2 u -- )
                                   1800 ;       Copy u bytes from b1 to b2.
      00860C                       1801         HEADER  CMOVE "CMOVE"
      00860C                       1802 CMOVE:
      00860C E6 01            [ 1] 1803         LD A,(1,X)
      00860E 88               [ 1] 1804         PUSH A
      00860F 5C               [ 1] 1805         INCW X
      008610 5C               [ 1] 1806         INCW X
      008611 90 93            [ 1] 1807 CMOVE1: LDW Y,X
      008613 EE 02            [ 2] 1808         LDW X,(2,X)
      008615 F6               [ 1] 1809         LD A,(X)
      008616 93               [ 1] 1810         LDW X,Y
      008617 FE               [ 2] 1811         LDW X,(X)
      008618 F7               [ 1] 1812         LD (X),A
      008619 93               [ 1] 1813         LDW X,Y
      00861A AD E1            [ 4] 1814         CALLR TWOONEPLUS
      00861C 84               [ 1] 1815         POP A
      00861D 4A               [ 1] 1816         DEC A
      00861E 88               [ 1] 1817         PUSH A
      00861F 26 F0            [ 1] 1818         JRNE CMOVE1
      008621 84               [ 1] 1819         POP A
      008622 1C 00 04         [ 2] 1820         ADDW X,#4
      008625 81               [ 4] 1821         RET
                                   1822 
                                   1823 
                                   1824 ; Basic I/O
                                   1825 
                                   1826 ;       KEY     ( -- c )
                                   1827 ;       Wait for and return an
                                   1828 ;       input character.
                                   1829 
      008626                       1830         HEADER  KEY "KEY"
      008626                       1831 KEY:
      008626 92 CD 02         [ 6] 1832 KEY1:   CALL    [USRQKEY]
      008629 90 93            [ 1] 1833         LDW     Y,X
      00862B 5C               [ 1] 1834         INCW    X
      00862C 5C               [ 1] 1835         INCW    X
      00862D 90 FE            [ 2] 1836         LDW     Y,(Y)
      00862F 26 02            [ 1] 1837         JRNE    RETIDLE
      008631 20 F3            [ 2] 1838         JRA     KEY1
      008633                       1839 RETIDLE:
      008633 81               [ 4] 1840         RET
                                   1841 
                                   1842 ;       QUIT    ( -- )
                                   1843 ;       Reset return stack pointer
                                   1844 ;       and start text interpreter.
                                   1845 
      008634                       1846         HEADER  QUIT "QUIT"
      008634                       1847 QUIT:
      008634 90 AE 03 FF      [ 2] 1848         LDW     Y,#RPP          ; initialize return stack
      008638 90 94            [ 1] 1849         LDW     SP,Y
      00863A CC 81 F6         [ 2] 1850         JP      SLEEP
                                   1851 
                                   1852 
                                   1853 ;       SP!     ( a -- )
                                   1854 ;       Set data stack pointer.
                                   1855 
      00863D                       1856         HEADER  SPSTO "sp!"
      00863D                       1857 SPSTO:
      00863D FE               [ 2] 1858         LDW     X,(X)   ;X = a
      00863E 81               [ 4] 1859         RET
                                   1860 
                                   1861 ;       SP@     ( -- a )        ( TOS STM8: -- Y,Z,N )
                                   1862 ;       Push current stack pointer.
                                   1863 
      00863F                       1864         HEADER  SPAT "sp@"
      00863F                       1865 SPAT:
      00863F 90 93            [ 1] 1866         LDW     Y,X
      008641 5A               [ 2] 1867         DECW    X               ; SUBW  X,#2
      008642 5A               [ 2] 1868         DECW    X
      008643 FF               [ 2] 1869         LDW     (X),Y           ; push on stack
      008644 81               [ 4] 1870         RET                     ; go to RET of EXEC
                                   1871 
                                   1872 ;       RP@     ( -- a )     ( TOS STM8: -- Y,Z,N )
                                   1873 ;       Push current RP to data stack.
                                   1874 
      008645                       1875         HEADER  RPAT "rp@"
      008645                       1876 RPAT:
      008645 90 96            [ 1] 1877         LDW     Y,SP            ; save return addr
      008647 5A               [ 2] 1878         DECW    X               ; SUBW  X,#2
      008648 5A               [ 2] 1879         DECW    X
      008649 FF               [ 2] 1880         LDW     (X),Y           ; push on stack
      00864A 81               [ 4] 1881         RET                     ; go to RET of EXEC
                                   1882 
                                   1883 ;       RP!     ( a -- )
                                   1884 ;       Set return stack pointer.
                                   1885 
      00864B                       1886         HEADFLG RPSTO "rp!" COMPO
                                      1 
      00864B                       1887 RPSTO:
      00864B 90 85            [ 2] 1888         POPW    Y
      00864D 90 BF 08         [ 2] 1889         LDW     XREG0,Y
      008650 90 93            [ 1] 1890         LDW     Y,X
      008652 5C               [ 1] 1891         INCW    X
      008653 5C               [ 1] 1892         INCW    X
      008654 90 FE            [ 2] 1893         LDW     Y,(Y)
      008656 90 94            [ 1] 1894         LDW     SP,Y
      008658 92 CC 08         [ 5] 1895         JP      [XREG0]
                                   1896 
                                   1897 ;===============================================================
      00865B                       1898         HEADFLG DO "DO" COMPO
                                      1 
      00865B                       1899 DO:
      00865B 90 93            [ 1] 1900         LDW Y,X
      00865D 85               [ 2] 1901         POPW X
      00865E 89               [ 2] 1902         PUSHW X
      00865F 89               [ 2] 1903         PUSHW X
      008660 89               [ 2] 1904         PUSHW X
      008661 93               [ 1] 1905         LDW X,Y
      008662 FE               [ 2] 1906         LDW X,(X)
      008663 1F 03            [ 2] 1907         LDW (3,SP),X
      008665 93               [ 1] 1908         LDW X,Y
      008666 EE 02            [ 2] 1909         LDW X,(2,X)
      008668 1F 05            [ 2] 1910         LDW (5,SP),X
      00866A 93               [ 1] 1911         LDW X,Y
      00866B 1C 00 04         [ 2] 1912         ADDW X,#4
      00866E 81               [ 4] 1913         RET
                                   1914 
                                   1915 ;===============================================================
                           000001  1916         .ifne   WORDS_EXTRAEEPR
                                   1917 ;       ULOCK  ( -- )
                                   1918 ;       Unlock EEPROM (STM8S)
                                   1919 
      00866F                       1920         HEADER  ULOCK "ULOCK"
      00866F                       1921 ULOCK:
      00866F 35 AE 50 64      [ 1] 1922         MOV     FLASH_DUKR,#0xAE
      008673 35 56 50 64      [ 1] 1923         MOV     FLASH_DUKR,#0x56
      008677 72 07 50 5F FB   [ 2] 1924 1$:     BTJF    FLASH_IAPSR,#3,1$    ; PM0051 4.1 requires polling bit3=1 before writing
      00867C 81               [ 4] 1925         RET
                                   1926 
                                   1927 
                                   1928 ;       LOCK  ( -- )
                                   1929 ;       Lock EEPROM (STM8S)
                                   1930 
      00867D                       1931         HEADER  LOCK "LOCK"
      00867D                       1932 LOCK:
      00867D 72 17 50 5F      [ 1] 1933         BRES    FLASH_IAPSR,#3
      008681 81               [ 4] 1934         RET
                                   1935         .endif
                                   1936 
                                   1937 ;       ULOCKF  ( -- )
                                   1938 ;       Unlock Flash (STM8S)
                                   1939 
      008682                       1940         HEADER  UNLOCK_FLASH "ULOCKF"
      008682                       1941 UNLOCK_FLASH:
      008682 35 56 50 62      [ 1] 1942         MOV     FLASH_PUKR,#0x56
      008686 35 AE 50 62      [ 1] 1943         MOV     FLASH_PUKR,#0xAE
      00868A 72 03 50 5F FB   [ 2] 1944 1$:     BTJF    FLASH_IAPSR,#1,1$    ; PM0051 4.1 requires polling bit1=1 before writing
      00868F 81               [ 4] 1945         RET
                                   1946 
                                   1947 ;       LOCKF  ( -- )
                                   1948 ;       Lock Flash (STM8S)
                                   1949 
      008690                       1950         HEADER  LOCK_FLASH "LOCKF"
      008690                       1951 LOCK_FLASH:
      008690 72 13 50 5F      [ 1] 1952         BRES    FLASH_IAPSR,#1
      008694 81               [ 4] 1953         RET
                                   1954 
                                   1955 ;       SAVEC ( -- )
                                   1956 ;       Minimal context switch for low level interrupt code
                                   1957 ;       This should be the first word called in the interrupt handler
                                   1958 
      008695                       1959         HEADER  SAVEC "SAVEC"
      008695                       1960 SAVEC:
      008695 90 85            [ 2] 1961         POPW    Y
      008697 BE 08            [ 2] 1962         LDW     X,XREG0
      008699 89               [ 2] 1963         PUSHW   X
      00869A AE 03 B0         [ 2] 1964         LDW     X,#ISPP         ; "PC_ISPP" const. top of int. data stack
      00869D 90 FC            [ 2] 1965         JP      (Y)
                                   1966 
                                   1967 ;       IRET ( -- )
                                   1968 ;       Restore context and return from low level interrupt code
                                   1969 ;       This should be the last word called in the interrupt handler
                                   1970 
      00869F                       1971         HEADER  RESTC "IRET"
      00869F                       1972 RESTC:
      00869F 85               [ 2] 1973         POPW    X
      0086A0 BF 08            [ 2] 1974         LDW     XREG0,X         ; restore context
      0086A2 80               [11] 1975         IRET                    ; resturn from interrupt
                                   1976 
                                   1977 ;===============================================================
                                   1978 
                           000000  1979         LASTN   =       LINK    ;last name defined
                           0086A3  1980         END_SDCC_FLASH = .
                           000021  1981         USERRAM = RAMPOOL
                                   1982 
                                   1983         .area CODE
                                   1984         .area INITIALIZER
                                   1985         .area CABS (ABS)
