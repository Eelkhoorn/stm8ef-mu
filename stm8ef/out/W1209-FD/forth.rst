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
      008002 83 4B                   65          .dw _forth
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
                           000000    92          .if   HAS_RXUART*HAS_TXUART
                                     93           .org 0x8008   + ITC_IX_UART1RX * 4
                                     94           .dw 0x8200
                                     95           .dw UART_INT
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
                                      1 ;       STM8S003F3 device and memory layout configuration
                                      2 
                           000067     3         TARGET = STM8S103F3     ; Complatible with STM8S003F3 - specs differ
                                      4 
                           0003FF     5         RAMEND =        0x03FF  ; "RAMEND" system (return) stack, growing down
                           004000     6         EEPROMBASE =    0x4000  ; "EESTART" EEPROM start address
                           00427F     7         EEPROMEND =     0x427F  ; "EEEND" 640 bytes EEPROM
                           00806C     8 	CODE_START =	0x806C	; End of interrupt vector area
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
                           0084EC    32         EMIT_BG  = DROP         ; vectored NUL background EMIT vector
                           008556    33         QKEY_BG  = ZERO         ; NUL background QKEY vector
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
                                      2 ; Config for W1209 Thermostat Module
                                      3 ; Clock: HSI (no crystal)
                                      4 
                           000001     5         MUFORTH          = 1    ; No compiler nor interpreter
                           000000     6         HALF_DUPLEX      = 0    ; Use UART in half duplex mode
                           000000     7         HAS_TXUART       = 0    ; No UART TXD, word TX!
                           000000     8         HAS_RXUART       = 0    ; No UART RXD, word ?RX
                           000001     9         HAS_TXSIM        = 1    ; Enable TxD via GPIO/TIM4, word TXGP!
                           000001    10         HAS_RXSIM        = 1    ; Enable RxD via GPIO/TIM4, word ?RXGP
                           00500A    11         PSIM     = PORTC        ; Port for UART simulation
                           000004    12         PNRX             = 4    ; Port GPIO# for HAS_RXDSIM
                           000005    13         PNTX             = 5    ; Port GPIO# for HAS_TXDSIM
                                     14 
                           008239    15         EMIT_BG  = EMIT7S       ; 7S-LED background EMIT vector
                           0081E9    16         QKEY_BG  = QKEYB        ; Board keys background QKEY vector
                                     17 
                           000001    18         HAS_LED7SEG      = 1    ; yes, 1*3 dig. 7-seg LED on module
                                     19 
                           000003    20         HAS_KEYS         = 3    ; yes, 3 keys on module
                           000001    21         HAS_OUTPUTS      = 1    ; yes, one LED
                           000001    22         HAS_ADC          = 1    ; Analog input words
                                     23 
                           000001    24         HAS_BACKGROUND   = 1    ; Background Forth task (TIM2 ticker)
                           000001    25         HAS_CPNVM        = 1    ; Can compile to Flash, always interpret to RAM
                           000001    26         HAS_DOES         = 1    ; CREATE-DOES> extension
                           000001    27         HAS_DOLOOP       = 1    ; DO .. LOOP extension: DO LEAVE LOOP +LOOP
                                     28 
                                     29 
                           000001    30         CASEINSENSITIVE  = 1    ; Case insensitive dictionary search
                           000001    31         SPEEDOVERSIZE    = 1    ; Speed-over-size in core words: ROT - = <
                           000000    32         BAREBONES        = 0    ; Remove or unlink some more: hi HERE .R U.R SPACES @EXECUTE AHEAD CALL, EXIT COMPILE [COMPILE]
                                     33 
                           000000    34         WORDS_LINKINTER  = 0    ; Link interpreter words: ACCEPT QUERY TAP kTAP hi 'BOOT tmp >IN 'TIB #TIB eval CONTEXT pars PARSE NUMBER? DIGIT? WORD TOKEN NAME> SAME? find ABORT aborq $INTERPRET INTER? .OK ?STACK EVAL PRESET QUIT $COMPILE
                           000000    35         WORDS_LINKCOMP   = 0    ; Link compiler words: cp last OVERT $"| ."| $,n
                           000000    36         WORDS_LINKRUNTI  = 0    ; Link runtime words: doLit do$ doVAR donxt dodoes ?branch branch
                           000001    37         WORDS_LINKCHAR   = 1    ; Link char out words: DIGIT <# # #S SIGN #> str hld HOLD
                           000000    38         WORDS_LINKMISC   = 0    ; Link composing words of SEE DUMP WORDS: >CHAR _TYPE dm+ .ID >NAME
                                     39 
                           000000    40         WORDS_EXTRASTACK = 0    ; Link/include stack core words: rp@ rp! sp! sp@ DEPTH
                           000000    41         WORDS_EXTRADEBUG = 0    ; Extra debug words: SEE
                           000001    42         WORDS_EXTRACORE  = 1    ; Extra core words: =0 I
                           000001    43         WORDS_EXTRAMEM   = 1    ; Extra memory words: B! 2C@ 2C!
                           000001    44         WORDS_EXTRAEEPR  = 1    ; Extra EEPROM lock/unlock words: LOCK ULOCK ULOCKF LOCKF
                                    200 
                                    201         ; .include "linkopts.inc"
                                    202 
                                    203         ; console configuration: check if TX simulation has priority over UART
                           000001   204         .ifge   HAS_TXSIM - HAS_TXUART
                           000000   205         .ifeq  PNTX-PNRX
                                    206         CONSOLE_HALF_DUPLEX = 1 ; single wire RX/TX simulation is half duplex
                           000001   207         .else
                           000000   208         CONSOLE_HALF_DUPLEX = 0 ; RX/TX simulation supports full duplex
                                    209         .endif
                           000000   210         .else
                                    211         CONSOLE_HALF_DUPLEX = HALF_DUPLEX ; use hardware UART settings
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
                           000001   262 .if  HAS_RXSIM
      00806C                        263         RamByte USR_5           ; chat variables
                           000004     1         USR_5 = RAMPOOL
                           000005     2         RAMPOOL = RAMPOOL + 1
      00806C                        264         RamByte USR_6           ;
                           000005     1         USR_6 = RAMPOOL
                           000006     2         RAMPOOL = RAMPOOL + 1
                                    265 .endif
      00806C                        266         RamWord MP              ; memory pointer for mu-chat
                           000006     1         MP = RAMPOOL
                           000008     2         RAMPOOL = RAMPOOL + 2
                                    267 
                                    268         ; More core variables in zero page (instead of assigning fixed addresses)
      00806C                        269         RamWord USRHLD          ; "HLD" hold a pointer of output string
                           000008     1         USRHLD = RAMPOOL
                           00000A     2         RAMPOOL = RAMPOOL + 2
      00806C                        270         RamByte XREG0           ; extra working register for core words
                           00000A     1         XREG0 = RAMPOOL
                           00000B     2         RAMPOOL = RAMPOOL + 1
      00806C                        271         RamByte XREG1           ; extra working register for core words
                           00000B     1         XREG1 = RAMPOOL
                           00000C     2         RAMPOOL = RAMPOOL + 1
      00806C                        272         RamByte XREG2           ; extra working register for core words
                           00000C     1         XREG2 = RAMPOOL
                           00000D     2         RAMPOOL = RAMPOOL + 1
      00806C                        273         RamByte XREG3           ; extra working register for core words
                           00000D     1         XREG3 = RAMPOOL
                           00000E     2         RAMPOOL = RAMPOOL + 1
      00806C                        274         RamWord BITAT           ; reserve space for BTJF
                           00000E     1         BITAT = RAMPOOL
                           000010     2         RAMPOOL = RAMPOOL + 2
                           000018   275         RAMPOOL = RAMPOOL + 8
      00806C                        276         RamWord BITSTO          ; reserve space for BSET/BRES
                           000018     1         BITSTO = RAMPOOL
                           00001A     2         RAMPOOL = RAMPOOL + 2
                           00001D   277         RAMPOOL = RAMPOOL + 3
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
                           000000   339 .if HAS_RXUART
                                    340         BTJF    UART_SR,#5,RXA
                                    341         LD      A,UART_DR      ; get char in A
                           000001   342 .else
      00806C 72 01 00 05 FB   [ 2]  343         BTJF USR_6,#0,RXA
      008071 B6 27            [ 1]  344         LD A,TIM4RXBUF
      008073 3F 05            [ 1]  345         CLR USR_6		; clear rxa flag
                                    346 .endif
      008075 81               [ 4]  347         RET
                                    348 
                                    349 ; receive byte in tos 
      008076                        350         HEADER  TOB "TOB"
      008076                        351 TOB:
                           000001   352 .if HAS_RXSIM
      008076 3F 05            [ 1]  353         CLR USR_6
                                    354 .endif
      008078 AD F2            [ 4]  355         CALLR RXA
      00807A 5A               [ 2]  356         DECW X
      00807B F7               [ 1]  357         LD (X),A
      00807C 5A               [ 2]  358         DECW X
      00807D 7F               [ 1]  359         CLR (X)
      00807E 81               [ 4]  360         RET
                                    361 
                                    362 ; receive cell in tos 
      00807F                        363         HEADER  TOW "TOW"
      00807F                        364 TOW:
      00807F AD F5            [ 4]  365         CALLR TOB
      008081 AD E9            [ 4]  366         CALLR RXA
      008083 F7               [ 1]  367         LD (X),A
      008084 81               [ 4]  368         RET
                                    369 
                                    370 ; send byte from tos 
      008085                        371         HEADER  ATO "ATO"
      008085                        372 ATO:
      008085 F6               [ 1]  373         LD A,(X)
      008086 5C               [ 1]  374         INCW X
      008087 CC 82 A2         [ 2]  375         JP TXASTOR
                                    376 
                                    377 ; send cell from tos 
      00808A                        378         HEADER  WTO "WTO"
      00808A                        379 WTO:
      00808A CD 87 30         [ 4]  380         CALL EXG
      00808D AD F6            [ 4]  381         CALLR ATO
      00808F 20 F4            [ 2]  382         JRA ATO
                                    383 
                                    384 
                                    385 ; send bytes from memory pointed to by MP 
      008091                        386         HEADER  SENDBYTES "SENDBYTES"
      008091                        387 SENDBYTES:
      008091 AD E3            [ 4]  388         CALLR TOB
      008093 5C               [ 1]  389         INCW X
      008094 90 BE 06         [ 2]  390         LDW Y,MP
      008097                        391 1$:
      008097 90 F6            [ 1]  392         LD A,(Y)
      008099 CD 82 A2         [ 4]  393         CALL TXASTOR
      00809C 90 5C            [ 1]  394         INCW Y
      00809E 7A               [ 1]  395         DEC(X)
      00809F 26 F6            [ 1]  396         JRNE 1$
      0080A1 5C               [ 1]  397         INCW X
      0080A2 81               [ 4]  398         RET
                                    399 
                                    400 ;       receive byte and store in memory pointer MP 
      0080A3                        401         HEADER  SETADDR "SETADDR"
      0080A3                        402 SETADDR:
      0080A3 AD DA            [ 4]  403         CALLR TOW
      0080A5 90 93            [ 1]  404         LDW Y,X
      0080A7 90 FE            [ 2]  405         LDW Y,(Y)
      0080A9 90 BF 06         [ 2]  406         LDW MP,Y
      0080AC 5C               [ 1]  407         INCW X
      0080AD 5C               [ 1]  408         INCW X
      0080AE 81               [ 4]  409         RET
                                    410 
                                    411 ;       
      0080AF                        412         HEADER  GETSP "GETSP"
      0080AF                        413 GETSP:
      0080AF CD 88 00         [ 4]  414         CALL SPAT
      0080B2 20 D6            [ 2]  415         JRA WTO
                                    416 
                                    417 ;       
      0080B4                        418         HEADER  WRITEBS "WRITEBS"
      0080B4                        419 WRITEBS:
      0080B4 AD C0            [ 4]  420         CALLR TOB	; count
      0080B6 90 BE 06         [ 2]  421 1$:	LDW Y,MP		; memory pointer in Y
      0080B9 AD B1            [ 4]  422         CALLR RXA        ; 
      0080BB 90 F7            [ 1]  423         LD (Y),A
      0080BD 90 5C            [ 1]  424         INCW Y
      0080BF 90 BF 06         [ 2]  425         LDW MP,Y
                           000001   426 .if HAS_RXSIM
      0080C2 3F 05            [ 1]  427         CLR USR_6
                                    428 .endif
      0080C4 90 93            [ 1]  429         LDW Y,X
      0080C6 90 FE            [ 2]  430         LDW Y,(Y)
      0080C8 90 5A            [ 2]  431         DECW Y
      0080CA FF               [ 2]  432         LDW (X),Y
      0080CB 26 E9            [ 1]  433         JRNE 1$
      0080CD 5C               [ 1]  434         INCW X
      0080CE 5C               [ 1]  435         INCW X
      0080CF 81               [ 4]  436         RET
                                    437 
      0080D0                        438         HEADER  SETSP "SETSP"
      0080D0                        439 SETSP:
      0080D0 AD AD            [ 4]  440         CALLR TOW
      0080D2 FE               [ 2]  441         LDW X,(X)
      0080D3 81               [ 4]  442         RET
                                    443 
      0080D4                        444         HEADER  RUN "RUN"
      0080D4                        445 RUN:
      0080D4 AD A9            [ 4]  446         CALLR TOW
      0080D6 FE               [ 2]  447         LDW X,(X)
      0080D7 AD A6            [ 4]  448         CALLR TOW
      0080D9 CC 83 F1         [ 2]  449         JP EXECU
                                    450 
      0080DC                        451         HEADER  FLASH "FLASH"
      0080DC                        452 FLASH:
      0080DC                        453         DoLitW FLASHBUF_ADDR
      0080DC 5A               [ 2]    1         DECW X
      0080DD 5A               [ 2]    2         DECW X
      0080DE 90 AE 03 40      [ 2]    3         LDW Y,#FLASHBUF_ADDR
      0080E2 FF               [ 2]    4         LDW (X),Y
      0080E3 AD 9A            [ 4]  454         CALLR TOW
      0080E5 AD 98            [ 4]  455         CALLR TOW
      0080E7 CD 87 CD         [ 4]  456         CALL CMOVE
      0080EA A6 AB            [ 1]  457         LD A,#0xAB
      0080EC CC 82 A2         [ 2]  458         JP TXASTOR
                                    459 
      0080EF                        460         HEADER  TABLE "TABLE"
      0080EF                        461 TABLE:
      0080EF 80 A3                  462         .dw SETADDR
      0080F1 80 91                  463         .dw SENDBYTES
      0080F3 80 B4                  464         .dw WRITEBS
      0080F5 80 AF                  465         .dw GETSP
      0080F7 80 D0                  466         .dw SETSP
      0080F9 80 D4                  467         .dw RUN
      0080FB 80 DC                  468         .dw FLASH
      0080FD 9D               [ 1]  469 NOP     ; for disaasembling purpose
                           00000F   470 lower=0xf
                           000018   471 upper=0x18
                           000010   472 offset=0x10
                                    473 
      0080FE                        474         HEADER  CHAT "CHAT"
      0080FE                        475 CHAT:
                           000001   476 .if HAS_RXSIM
      0080FE B6 27            [ 1]  477         LD A,TIM4RXBUF
      008100 3F 05            [ 1]  478         CLR USR_6
                           000000   479 .else
                                    480         CALL RXA
                                    481 .endif
      008102 A1 0F            [ 1]  482         CP A,#lower
      008104 2B 14            [ 1]  483         JRMI 1$
      008106 A1 18            [ 1]  484         CP A,#upper
      008108 2C 10            [ 1]  485         JRSGT 1$
      00810A A0 10            [ 1]  486         SUB A,#offset
      00810C 48               [ 1]  487         SLL A
      00810D AB EF            [ 1]  488         ADD A,#TABLE
      00810F 90 97            [ 1]  489         LD YL,A
      008111 4F               [ 1]  490         CLR A
      008112 A9 80            [ 1]  491         ADC A,#>TABLE   ; MSB of TABLE
      008114 90 95            [ 1]  492         LD YH,A
      008116 90 FE            [ 2]  493         LDW Y,(Y)
      008118 90 FC            [ 2]  494         JP (Y)
      00811A                        495 1$:
      00811A 81               [ 4]  496         RET
                                    497         
                                    498 ; ==============================================
                                    499 ;       Getbit and Setbit routines to be moved 
                                    500 ;       to ram during reset ( -- )
                                    501 ; ==============================================
                                    502 
      00811B                        503         HEADER  COLD1 "COLD1"
      00811B                        504 COLD1:
      00811B 4F               [ 1]  505         CLR A
      00811C 72 01 00 0A 01   [ 2]  506         BTJF XREG0,#0,1$
      008121 4C               [ 1]  507         INC A
      008122 E7 01            [ 1]  508 1$:     LD (1,X),A
      008124 81               [ 4]  509         RET
      008125 72 10 01 00      [ 1]  510         BSET 0x100,#0
      008129 81               [ 4]  511         RET
                                    512 ; ==============================================
                                    513 
                                    514 ; ==============================================
                                    515 
                                    516 ;       Includes for board support code
                                    517 ;       Board I/O initialization and E/E mapping code
                                    518 ;       Hardware dependent words, e.g.  BKEY, OUT!
                                    519         .include "boardcore.inc"
                                      1 ; XH-W1209 STM8S device dependent HW routines
                                      2 
                                      3 
                                      4 ;       BOARDINIT  ( -- )
                                      5 ;       Init board GPIO (except COM ports)
      00812A                          6 BOARDINIT:
                                      7         ; Board I/O initialization
                                      8 
                                      9         ; W1209 STM8S003F3 init GPIO
      00812A 35 0E 50 02      [ 1]   10         MOV     PA_DDR,#0b00001110 ; relay,B,F
      00812E 35 0E 50 03      [ 1]   11         MOV     PA_CR1,#0b00001110
      008132 35 30 50 07      [ 1]   12         MOV     PB_DDR,#0b00110000 ; d2,d3
      008136 35 30 50 08      [ 1]   13         MOV     PB_CR1,#0b00110000
      00813A 35 C0 50 0C      [ 1]   14         MOV     PC_DDR,#0b11000000 ; G,C
      00813E 35 F8 50 0D      [ 1]   15         MOV     PC_CR1,#0b11111000 ; G,C-+S... Key pullups
      008142 35 3E 50 11      [ 1]   16         MOV     PD_DDR,#0b00111110 ; A,DP,D,d1,A
      008146 35 3E 50 12      [ 1]   17         MOV     PD_CR1,#0b00111110
                                     18 
                           000001    19         .ifne   HAS_OUTPUTS
      00814A 4F               [ 1]   20         CLR     A
      00814B 20 5A            [ 2]   21         JRA     AOUTSTOR
                           000000    22         .else
                                     23         RET
                                     24         .endif
                                     25 
                                     26 ;===============================================================
                                     27 
                           000001    28         .ifne   HAS_LED7SEG
                                     29 ;       LED_MPX driver ( -- )
                                     30 ;       Output bit pattern in A to 7S-LED digit hardware
                                     31 
      00814D                         32 LED_MPX:
      00814D 72 18 50 0F      [ 1]   33         BSET    PD_ODR,#4       ; Digit .3..
      008151 72 1A 50 05      [ 1]   34         BSET    PB_ODR,#5       ; Digit ..2.
      008155 72 18 50 05      [ 1]   35         BSET    PB_ODR,#4       ; Digit ...1
                                     36 
      008159 B6 2B            [ 1]   37         LD      A,TICKCNT+1
      00815B A4 03            [ 1]   38         AND     A,#0x03         ; 3 digits MPX
                                     39 
      00815D 26 06            [ 1]   40         JRNE    1$
      00815F 72 19 50 0F      [ 1]   41         BRES    PD_ODR,#4       ; digit .3..
      008163 20 12            [ 2]   42         JRA     3$
                                     43 
      008165 A1 01            [ 1]   44 1$:     CP      A,#1
      008167 26 06            [ 1]   45         JRNE    2$
      008169 72 1B 50 05      [ 1]   46         BRES    PB_ODR,#5       ; digit ..2.
      00816D 20 08            [ 2]   47         JRA     3$
                                     48 
      00816F A1 02            [ 1]   49 2$:     CP      A,#2
      008171 26 30            [ 1]   50         JRNE    4$
      008173 72 19 50 05      [ 1]   51         BRES    PB_ODR,#4       ; digit ...1
                                     52         ; fall through
                                     53 
      008177 5F               [ 1]   54 3$:     CLRW    X
      008178 97               [ 1]   55         LD      XL,A
      008179 E6 20            [ 1]   56         LD      A,(LED7LAST-2,X)
                                     57 
                                     58         ; W1209 7S LED display row
                                     59         ; bit 76453210 input (parameter A)
                                     60         ;  PA .....FB.
                                     61         ;  PC CG......
                                     62         ;  PD ..A.DPE.
      00817B 46               [ 1]   63         RRC     A
      00817C 90 1B 50 0F      [ 1]   64         BCCM    PD_ODR,#5       ; A
      008180 46               [ 1]   65         RRC     A
      008181 90 15 50 00      [ 1]   66         BCCM    PA_ODR,#2       ; B
      008185 46               [ 1]   67         RRC     A
      008186 90 1F 50 0A      [ 1]   68         BCCM    PC_ODR,#7       ; C
      00818A 46               [ 1]   69         RRC     A
      00818B 90 17 50 0F      [ 1]   70         BCCM    PD_ODR,#3       ; D
      00818F 46               [ 1]   71         RRC     A
      008190 90 13 50 0F      [ 1]   72         BCCM    PD_ODR,#1       ; E
      008194 46               [ 1]   73         RRC     A
      008195 90 13 50 00      [ 1]   74         BCCM    PA_ODR,#1       ; F
      008199 46               [ 1]   75         RRC     A
      00819A 90 1D 50 0A      [ 1]   76         BCCM    PC_ODR,#6       ; G
      00819E 46               [ 1]   77         RRC     A
      00819F 90 15 50 0F      [ 1]   78         BCCM    PD_ODR,#2       ; P
                                     79 
      0081A3 81               [ 4]   80 4$:     RET
                                     81         .endif
                                     82 
                                     83 ;===============================================================
                                     84 
                           000001    85         .ifne   HAS_OUTPUTS
      0081A4                         86         RamWord OUTPUTS         ; "OUT", e.g. relays, LEDs, etc. (16 bit)
                           00001D     1         OUTPUTS = RAMPOOL
                           00001F     2         RAMPOOL = RAMPOOL + 2
                                     87 
                                     88 ;       OUT!  ( c -- )
                                     89 ;       Put c to board outputs, storing a copy in OUTPUTS
                                     90 
                                     91 
                           000001    92 .if MUFORTH
      0081A4                         93         HEADER  OUTSTOR "OUT!"
                           000000    94 .else
                                     95         .dw     LINK
                                     96         LINK =  .
                                     97         .db     (4)
                                     98         .ascii  "OUT!"
                                     99 .endif
                                    100 
      0081A4                        101 OUTSTOR:
      0081A4 5C               [ 1]  102         INCW    X
      0081A5 F6               [ 1]  103         LD      A,(X)
      0081A6 5C               [ 1]  104         INCW    X
      0081A7                        105 AOUTSTOR:
      0081A7 B7 1E            [ 1]  106         LD      OUTPUTS+1,A
      0081A9 46               [ 1]  107         RRC     A
      0081AA 90 17 50 00      [ 1]  108         BCCM    PA_ODR,#3       ; W1209 relay
      0081AE 81               [ 4]  109         RET
                                    110         .endif
                                    111 
                                    112 ;===============================================================
                                    113 
                           000001   114         .ifne   HAS_KEYS
                                    115 ;       BKEY  ( -- c )     ( TOS STM8: -- A,Z,N )
                                    116 ;       Read board key state as a bitfield
                                    117 
                           000001   118 .if MUFORTH
      0081AF                        119         HEADER  BKEY "BKEY"
                           000000   120 .else
                                    121         .dw     LINK
                                    122         LINK =  .
                                    123         .db     (4)
                                    124         .ascii  "BKEY"
                                    125 .endif
                                    126 
      0081AF                        127 BKEY:
                                    128         ; Keys "set" (1), "+" (2), and "-" (4) on PC.3:5
      0081AF C6 50 0B         [ 1]  129         LD      A,PC_IDR
      0081B2 48               [ 1]  130         SLA     A
      0081B3 4E               [ 1]  131         SWAP    A
      0081B4 43               [ 1]  132         CPL     A
      0081B5 A4 07            [ 1]  133         AND     A,#0x07
      0081B7 CC 85 57         [ 2]  134         JP      ASTOR
                                    135 
                                    136 ;       BKEYC  ( -- c )   ( TOS STM8: -- A,Z,N )
                                    137 ;       Read and translate board dependent key bitmap into char
                                    138 
      0081BA                        139 BKEYCHAR:
      0081BA AD F3            [ 4]  140         CALLR   BKEY
      0081BC 27 04            [ 1]  141         JREQ    1$
      0081BE AB 40            [ 1]  142         ADD     A,#'@'
      0081C0 E7 01            [ 1]  143         LD      (1,X),A
      0081C2 81               [ 4]  144 1$:     RET
                                    145        .endif
                                    146 
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
      0081C3                         78         HEADER  ADCSTOR "ADC!"
      0081C3                         79 ADCSTOR:
      0081C3 5C               [ 1]   80         INCW    X
      0081C4 F6               [ 1]   81         LD      A,(X)
      0081C5 5C               [ 1]   82         INCW    X
      0081C6 A4 0F            [ 1]   83         AND     A,#0x0F
      0081C8 C7 54 00         [ 1]   84         LD      ADC_CSR,A       ; select channel
      0081CB 72 16 54 02      [ 1]   85         BSET    ADC_CR2,#3      ; align ADC to LSB
      0081CF 72 10 54 01      [ 1]   86         BSET    ADC_CR1,#0      ; enable ADC
      0081D3 81               [ 4]   87         RET
                                     88 
                                     89 ;       ADC@  ( -- w )
                                     90 ;       start ADC conversion, read result
                                     91 
      0081D4                         92         HEADER  ADCAT "ADC@"
      0081D4                         93 ADCAT:
      0081D4 72 1F 54 00      [ 1]   94         BRES    ADC_CSR,#7      ; reset EOC
      0081D8 72 10 54 01      [ 1]   95         BSET    ADC_CR1,#0      ; start ADC
      0081DC 72 0F 54 00 FB   [ 2]   96 1$:     BTJF    ADC_CSR,#7,1$   ; wait until EOC
      0081E1 90 CE 54 04      [ 2]   97         LDW     Y,ADC_DRH       ; read ADC
      0081E5 5A               [ 2]   98         DECW    X               ; SUBW  X,#2
      0081E6 5A               [ 2]   99         DECW    X
      0081E7 FF               [ 2]  100         LDW     (X),Y           ; push on stack
      0081E8 81               [ 4]  101         RET                     ; go to RET of EXEC
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
                           000001     6         .ifne   HAS_KEYS
                                      7 
      0081E9                          8         RamByte KEYREPET        ; board key repetition control (8 bit)
                           00001F     1         KEYREPET = RAMPOOL
                           000020     2         RAMPOOL = RAMPOOL + 1
                                      9 
                                     10 ;       ?KEYB   ( -- c T | F )  ( TOS STM8: -- Y,Z,N )
                                     11 ;       Return keyboard char and true, or false if no key pressed.
                                     12 
      0081E9                         13         HEADER  QKEYB "?KEYB"
      0081E9                         14 QKEYB:
      0081E9 CD 81 BA         [ 4]   15         CALL    BKEYCHAR        ; Read char from keyboard (option: vectored code)
      0081EC 5C               [ 1]   16         INCW    X
      0081ED F6               [ 1]   17         LD      A,(X)
      0081EE 5C               [ 1]   18         INCW    X
      0081EF 4D               [ 1]   19         TNZ     A
                                     20 
      0081F0 26 06            [ 1]   21         JRNE    KEYBPRESS
                                     22         ; Bit7: flag press + 100*5ms hold before repetition
      0081F2 35 E4 00 1F      [ 1]   23         MOV     KEYREPET,#(0x80 + 100)
      0081F6 20 16            [ 2]   24         JRA     ZERO1
      0081F8                         25 KEYBPRESS:
      0081F8 72 0F 00 1F 06   [ 2]   26         BTJF    KEYREPET,#7,KEYBHOLD
      0081FD 72 1F 00 1F      [ 1]   27         BRES    KEYREPET,#7
      008201 20 08            [ 2]   28         JRA     ATOKEYB
      008203                         29 KEYBHOLD:
      008203 3A 1F            [ 1]   30         DEC     KEYREPET
      008205 26 07            [ 1]   31         JRNE    ZERO1
      008207 35 1E 00 1F      [ 1]   32         MOV     KEYREPET,#30    ; repetition time: n*5ms
      00820B                         33 ATOKEYB:
      00820B CC 85 5F         [ 2]   34         JP      ATOKEY          ; push char and flag true
      00820E CC 85 56         [ 2]   35 ZERO1:   JP      ZERO
                                     36 
                                     37         .endif
                                     38 
                           000001    39         .ifne   HAS_LED7SEG
                                     40 
                           000000    41         .ifeq  USE_CALLDOLIT
                                     42 ;       Macro for inline literals using the TRAP approach
                                     43         .macro DoLitC c
                                     44         TRAP
                                     45         .db     c
                                     46         .endm
                                     47 
                           000001    48         .else
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
                           000000    61         .if     gt,(HAS_LED7SEG-1)
                                     62         RamByte LED7GROUP       ; byte index of 7-SEG digit group
                                     63         .endif
                                     64 
                           000003    65         DIGITS = HAS_LED7SEG*LEN_7SGROUP
      008211                         66         RamBlck LED7FIRST,DIGITS ; leftmost 7S-LED digit
                           000020     1         LED7FIRST = RAMPOOL
                           000023     2         RAMPOOL = RAMPOOL + DIGITS
                           000022    67         LED7LAST = RAMPOOL-1    ; save memory location of rightmost 7S-LED digit
                                     68 
                                     69 
                                     70 ;       7-seg LED patterns, "70s chique"
      008211                         71 PAT7SM9:
      008211 00 40 80 52             72         .db     0x00, 0x40, 0x80, 0x52 ; , - . / (',' as blank)
      008215 3F 06 5B 4F             73         .db     0x3F, 0x06, 0x5B, 0x4F ; 0,1,2,3
      008219 66 6D 7D 07             74         .db     0x66, 0x6D, 0x7D, 0x07 ; 4,5,6,7
      00821D 7F 6F                   75         .db     0x7F, 0x6F             ; 8,9
      00821F                         76 PAT7SAZ:
      00821F 77 7C 39                77         .db           0x77, 0x7C, 0x39 ;   A,B,C
      008222 5E 79 71 3D             78         .db     0x5E, 0x79, 0x71, 0x3D ; D,E,F,G
      008226 74 30 1E 7A             79         .db     0x74, 0x30, 0x1E, 0x7A ; H,I,J,K
      00822A 38 55 54 5C             80         .db     0x38, 0x55, 0x54, 0x5C ; L,M,N,O
      00822E 73 67 50 6D             81         .db     0x73, 0x67, 0x50, 0x6D ; P,Q,R,S
      008232 78 3E 1C 1D             82         .db     0x78, 0x3E, 0x1C, 0x1D ; T,U,V,W
      008236 76 6E 5B                83         .db     0x76, 0x6E, 0x5B       ; X,Y,Z
                                     84 
                                     85 ;       E7S  ( c -- )
                                     86 ;       Convert char to 7-seg LED pattern, and insert it in display buffer
                                     87 
      008239                         88         HEADER  EMIT7S "E7S"
      008239                         89 EMIT7S:
      008239 E6 01            [ 1]   90         LD      A,(1,X)         ; c to A
                                     91 
      00823B A1 20            [ 1]   92         CP      A,#' '
      00823D 26 01            [ 1]   93         JRNE    E7SNOBLK
                                     94 
                           000000    95         .if     gt,(HAS_LED7SEG-1)
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
      00823F 81               [ 4]  113         RET
                                    114 
      008240                        115 E7SNOBLK:
                                    116 
                           000000   117         .if     gt,(HAS_LED7SEG-1)
                                    118         CP      A,#LF           ; test for c ~ /[<CR><LF>]/
                                    119         JRNE    E7SNOLF
                                    120         MOV     LED7GROUP,#0x80 ; go to first LED group, set "no-tab flag"
                                    121         JRA     E7END
                                    122         .endif
                                    123 
      008240                        124 E7SNOLF:
                           000000   125         .if     gt,(HAS_LED7SEG-1)
                                    126         BRES    LED7GROUP,#7    ; on char output: clear "no-tab flag"
                                    127         .endif
                                    128 
      008240 A1 2E            [ 1]  129         CP      A,#'.'
      008242 27 2F            [ 1]  130         JREQ    E7DOT
      008244 A1 2C            [ 1]  131         CP      A,#','
      008246 2B 31            [ 1]  132         JRMI    E7END
      008248 A1 7A            [ 1]  133         CP      A,#'z'
      00824A 2A 2D            [ 1]  134         JRPL    E7END
      00824C A1 41            [ 1]  135         CP      A,#'A'
      00824E 24 0D            [ 1]  136         JRUGE   E7ALPH
                                    137 
                                    138         ; '-'--'9' (and '@')
      008250 A0 2C            [ 1]  139         SUB     A,#','
      008252 E7 01            [ 1]  140         LD      (1,X),A
      008254                        141         DoLitW  PAT7SM9
      008254 5A               [ 2]    1         DECW X
      008255 5A               [ 2]    2         DECW X
      008256 90 AE 82 11      [ 2]    3         LDW Y,#PAT7SM9
      00825A FF               [ 2]    4         LDW (X),Y
      00825B 20 0D            [ 2]  142         JRA     E7LOOKA
      00825D                        143 E7ALPH:
                                    144         ; 'A'--'z'
      00825D A4 5F            [ 1]  145         AND     A,#0x5F         ; convert to uppercase
      00825F A0 41            [ 1]  146         SUB     A,#'A'
      008261 E7 01            [ 1]  147         LD      (1,X),A
      008263                        148         DoLitW  PAT7SAZ
      008263 5A               [ 2]    1         DECW X
      008264 5A               [ 2]    2         DECW X
      008265 90 AE 82 1F      [ 2]    3         LDW Y,#PAT7SAZ
      008269 FF               [ 2]    4         LDW (X),Y
      00826A                        149 E7LOOKA:
      00826A CD 85 0C         [ 4]  150         CALL    PLUS
      00826D CD 84 C6         [ 4]  151         CALL    CAT
      008270 CC 82 7C         [ 2]  152         JP      PUT7S
                                    153 
      008273                        154 E7DOT:
                           000000   155         .if     gt,(HAS_LED7SEG-1)
                                    156         CALL    XLEDGROUP
                                    157         LD      A,((LEN_7SGROUP-1),X)
                                    158         OR      A,#0x80
                                    159         LD      ((LEN_7SGROUP-1),X),A
                                    160         EXGW    X,Y             ; restore X/Y after XLEDGROUP
                                    161         ; fall trough
                                    162 
                           000001   163         .else
      008273 A6 80            [ 1]  164         LD      A,#0x80         ; 7-seg P (dot)
      008275 BA 22            [ 1]  165         OR      A,LED7LAST
      008277 B7 22            [ 1]  166         LD      LED7LAST,A
                                    167         .endif
                                    168         ; fall trough
                                    169 
      008279                        170 E7END:
      008279 CC 84 EC         [ 2]  171         JP      DROP
                                    172 
                           000000   173         .if     gt,(HAS_LED7SEG-1)
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
      00827C                        191         HEADER  PUT7S "P7S"
      00827C                        192 PUT7S:
                           000000   193         .if     gt,(HAS_LED7SEG-1)
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
                           000001   210         .else
      00827C                        211         DoLitC  LED7FIRST+1
      00827C 5A               [ 2]    1         DECW X
      00827D A6 21            [ 1]    2         LD A,#LED7FIRST+1
      00827F F7               [ 1]    3         LD (X),A
      008280 5A               [ 2]    4         DECW X
      008281 7F               [ 1]    5         CLR (X)
      008282                        212         DoLitC  LED7FIRST
      008282 5A               [ 2]    1         DECW X
      008283 A6 20            [ 1]    2         LD A,#LED7FIRST
      008285 F7               [ 1]    3         LD (X),A
      008286 5A               [ 2]    4         DECW X
      008287 7F               [ 1]    5         CLR (X)
      008288                        213         DoLitC  (LEN_7SGROUP-1)
      008288 5A               [ 2]    1         DECW X
      008289 A6 02            [ 1]    2         LD A,#(LEN_7SGROUP-1)
      00828B F7               [ 1]    3         LD (X),A
      00828C 5A               [ 2]    4         DECW X
      00828D 7F               [ 1]    5         CLR (X)
      00828E CD 87 CD         [ 4]  214         CALL    CMOVE
      008291 5C               [ 1]  215         INCW    X
      008292 F6               [ 1]  216         LD      A,(X)
      008293 5C               [ 1]  217         INCW    X
      008294 4D               [ 1]  218         TNZ     A
      008295 B7 22            [ 1]  219         LD      LED7LAST,A
                                    220         .endif
      008297 81               [ 4]  221         RET
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
                           000000   232         .else
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
                           000000     9         .ifeq  HAS_TXSIM + HAS_RXSIM
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
                                     26 _EXTI0_IRQHandler:
                                     27 _EXTI1_IRQHandler:
                                     28 _EXTI2_IRQHandler:
                                     29 _EXTI3_IRQHandler:
                                     30 _EXTI4_IRQHandler:
                                     31 _EXTI5_IRQHandler:
                                     32 _EXTI6_IRQHandler:
                                     33 _EXTI7_IRQHandler:
                                     34 _TIM4_IRQHandler:
                                     35 
                           000001    36         .else
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
                           000001    91           .ifne  PNRX^PNTX
                                     92                .include"sser_fdx.inc"
                                      1 ;--------------------------------------------------------
                                      2 ;       STM8EF for STM8S (Value line and Access Line devices)
                                      3 ;       Simulated serial I/O - two GPIOs - Full Duplex
                                      4 ;--------------------------------------------------------
                                      5 
                           000001     6         .ifne   HAS_TXSIM ;+ HAS_RXSIM
                                      7         
      008298                          8         RamByte TIM4RCNT        ; TIM4 RX interrupt counter
                           000023     1         TIM4RCNT = RAMPOOL
                           000024     2         RAMPOOL = RAMPOOL + 1
      008298                          9         RamByte TIM4TCNT        ; TIM4 TX interrupt counter
                           000024     1         TIM4TCNT = RAMPOOL
                           000025     2         RAMPOOL = RAMPOOL + 1
      008298                         10         RamByte TIM4TXREG       ; TIM4 TX transmit buffer and shift register
                           000025     1         TIM4TXREG = RAMPOOL
                           000026     2         RAMPOOL = RAMPOOL + 1
      008298                         11         RamByte TIM4RXREG       ; TIM4 RX shift register
                           000026     1         TIM4RXREG = RAMPOOL
                           000027     2         RAMPOOL = RAMPOOL + 1
      008298                         12         RamByte TIM4RXBUF       ; TIM4 RX receive buffer
                           000027     1         TIM4RXBUF = RAMPOOL
                           000028     2         RAMPOOL = RAMPOOL + 1
                                     13         .endif
                                     14 
                           000000    15         .ifne   SERPRIOTIM
                                     16         RamWord TIMSERIAL       ; "TIMSERIAL" Timer: use PSIM,PNRX for serial while not 0
                                     17         RamByte TIMRELOAD       ; Reload value for TIMSERIAL, set by receive interrupt, reset by timer
                                     18         .endif
                                     19 
                           000001    20         .ifne   HAS_RXSIM
                                     21 ;       ?RXP     ( -- c T | F )  ( TOS STM8: -- Y,Z,N )
                                     22 ;       Return char from a simulated serial interface and true, or false.
                                     23 
                           000001    24         .ifeq   BAREBONES
                           000000    25         .ifne   HAS_RXUART
                                     26         HEADER  QRXP "?RXP"
                           000001    27         .else
      008298                         28         HEADER  QRX "?RX"
                                     29         .endif
                                     30         .endif
                           000001    31         .ifeq   HAS_RXUART
      008298                         32 QRX:
                                     33         .endif
      008298                         34 QRXP:
      008298 4F               [ 1]   35         CLR     A
      008299 31 00 27         [ 3]   36         EXG     A,TIM4RXBUF     ; read and consume char
      00829C CC 85 57         [ 2]   37         JP      ASTOR
                                     38         .endif
                                     39 
                           000001    40         .ifne   HAS_TXSIM
                                     41 ;       TXP!     ( c -- )
                                     42 ;       Send character c to a simulated serial interface.
                                     43 
                           000001    44         .ifeq   BAREBONES
                           000000    45         .ifne   HAS_TXUART
                                     46         HEADER  TXPSTOR "TXP!"
                           000001    47         .else
      00829F                         48         HEADER  TXSTOR "TX!"
                                     49         .endif
                                     50         .endif
                                     51 
                           000001    52         .ifeq   HAS_TXUART
      00829F                         53 TXSTOR:
                                     54         .endif
      00829F                         55 TXPSTOR:
                           000000    56         .ifne   SERPRIOTIM
                                     57         MOV     TIMSERIAL,TIMRELOAD ; load MSB msec ticker
                                     58         .endif
                                     59 
      00829F 5C               [ 1]   60         INCW    X
      0082A0 F6               [ 1]   61         LD      A,(X)
      0082A1 5C               [ 1]   62         INCW    X
                                     63 
      0082A2                         64 TXASTOR:
      0082A2 3D 24            [ 1]   65 1$:     TNZ     TIM4TCNT
      0082A4 26 FC            [ 1]   66         JRNE    1$              ; wait for TIM4 TX complete
                                     67 
      0082A6 B7 25            [ 1]   68         LD      TIM4TXREG,A     ; char to TXSIM output register
      0082A8 35 0A 00 24      [ 1]   69         MOV     TIM4TCNT,#10    ; init next transfer
      0082AC 3D 23            [ 1]   70         TNZ     TIM4RCNT        ; test if RX already uses TIM4
      0082AE 26 08            [ 1]   71         JRNE    2$
      0082B0 72 5F 53 46      [ 1]   72         CLR     TIM4_CNTR       ; reset TIM4, trigger update interrupt
      0082B4 72 10 53 43      [ 1]   73         BSET    TIM4_IER,#0     ; enable TIM4 interrupt
      0082B8                         74 2$:
      0082B8 81               [ 4]   75         RET
                                     76         .endif
                                     77 
                                     78 ;       RxD through GPIO start-bit interrupt handler
                                     79 
                                     80 
                           000001    81         .ifne   HAS_RXSIM
                                     82         .globl _EXTI0_IRQHandler
                                     83         .globl _EXTI1_IRQHandler
                                     84         .globl _EXTI2_IRQHandler
                                     85         .globl _EXTI3_IRQHandler
                                     86         .globl _EXTI4_IRQHandler
                                     87         .globl _EXTI5_IRQHandler
                                     88         .globl _EXTI6_IRQHandler
                                     89         .globl _EXTI7_IRQHandler
                                     90 
      0082B9                         91 _EXTI0_IRQHandler:
      0082B9                         92 _EXTI1_IRQHandler:
      0082B9                         93 _EXTI2_IRQHandler:
      0082B9                         94 _EXTI3_IRQHandler:
      0082B9                         95 _EXTI4_IRQHandler:
      0082B9                         96 _EXTI5_IRQHandler:
      0082B9                         97 _EXTI6_IRQHandler:
      0082B9                         98 _EXTI7_IRQHandler:
                                     99 
      0082B9 72 19 50 0E      [ 1]  100         BRES    PSIM+CR2,#PNRX  ; disable PNRX external interrupt
                                    101 
                           000000   102         .ifeq   (FAMILY - STM8L)
                                    103         BSET    EXTI_SR1,#PNRX  ; STM8L: clear interrupt
                                    104         .endif
                                    105 
                           000000   106         .ifne   SERPRIOTIM
                                    107         MOV     TIMRELOAD,#(SERPRIOTIM/256) ; load MSB msec reload
                                    108         MOV     TIMSERIAL,TIMRELOAD ; load MSB msec ticker
                                    109         .endif
                                    110 
      0082BD 35 09 00 23      [ 1]  111         MOV     TIM4RCNT,#9     ; set sequence counter for RX
                                    112 
                                    113         ; Set-up Rx sampling at quarter bit time (compromise with TX)
      0082C1 35 11 53 46      [ 1]  114         MOV     TIM4_CNTR,#(CTIM4ARR/4)
                                    115         ; MOV     TIM4_CNTR,#0x53 ; gives better results at 57600 baud
      0082C5 72 11 53 44      [ 1]  116         BRES    TIM4_SR,#0      ; clear TIM4 UIF
      0082C9 72 10 53 43      [ 1]  117         BSET    TIM4_IER,#0     ; enable TIM4 interrupt
      0082CD 80               [11]  118         IRET
                                    119         .endif
                                    120 
                                    121         .globl _TIM4_IRQHandler
      0082CE                        122 _TIM4_IRQHandler:
                                    123         ; TIM4 interrupt handler for software Rx/Tx
      0082CE 72 11 53 44      [ 1]  124         BRES    TIM4_SR,#0      ; clear TIM4 UIF
                                    125 
      0082D2 B6 23            [ 1]  126         LD      A,TIM4RCNT      ; test receive step counter
      0082D4 27 15            [ 1]  127         JREQ    TIM4_TESTTRANS  ; nothing to do - check for transmit
                                    128 
                                    129         ; Receive a bit
      0082D6 72 08 50 0B 00   [ 2]  130         BTJT    PSIM+IDR,#PNRX,1$ ; dummy branch, copy GPIO to CF
      0082DB 36 26            [ 1]  131 1$:     RRC     TIM4RXREG
      0082DD 3A 23            [ 1]  132         DEC     TIM4RCNT
      0082DF 26 0A            [ 1]  133         JRNE    TIM4_TESTTRANS
                                    134 
                                    135         ; Receive sequence complete
      0082E1 45 26 27         [ 1]  136         MOV     TIM4RXBUF,TIM4RXREG ; save result
      0082E4 45 04 05         [ 1]  137         MOV     USR_6,USR_5      ; set flag
      0082E7 72 18 50 0E      [ 1]  138         BSET    PSIM+CR2,#PNRX  ; enable PNRX external interrupt
                                    139         ; fall through
                                    140 
      0082EB                        141 TIM4_TESTTRANS:
      0082EB B6 24            [ 1]  142         LD      A,TIM4TCNT      ; test transmit step counter
      0082ED 27 10            [ 1]  143         JREQ    TIM4_TESTOFF
                                    144         ; fall through
                                    145 
      0082EF                        146 TIM4_TRANS:
      0082EF A1 0A            [ 1]  147         CP      A,#10           ; startbit? (also sets CF)
      0082F1 26 02            [ 1]  148         JRNE    TIM4_TRANSSER
      0082F3 20 02            [ 2]  149         JRA     TIM4_TRANSBIT   ; emit start bit (CF=0 from "CP A")
                                    150 
      0082F5                        151 TIM4_TRANSSER:
      0082F5 36 25            [ 1]  152         RRC     TIM4TXREG       ; get data bit, shift in stop bit (CF=1 from "CP A")
                                    153         ; fall through
                                    154 
      0082F7                        155 TIM4_TRANSBIT:
      0082F7 90 1B 50 0A      [ 1]  156         BCCM    PSIM+ODR,#PNTX  ; Set GPIO to CF
      0082FB 3A 24            [ 1]  157         DEC     TIM4TCNT        ; next TXD TIM4 state
      0082FD 26 0C            [ 1]  158         JRNE    TIM4_END        ; not complete unless TIM4TCNT is zero
                                    159         ; fall through
                                    160 
      0082FF                        161 TIM4_TESTOFF:
      0082FF B6 23            [ 1]  162         LD      A,TIM4RCNT
      008301 26 08            [ 1]  163         JRNE    TIM4_END
      008303 72 1A 50 0A      [ 1]  164         BSET    PSIM+ODR,#PNTX  ; set TX GPIO to STOP
      008307 72 11 53 43      [ 1]  165         BRES    TIM4_IER,#0     ; disable TIM4 interrupt
                                    166         ; fall through
                                    167 
      00830B                        168 TIM4_END:
      00830B 80               [11]  169         IRET
                                    170 
                           000000    93           .else
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
      00830C                         12         RamWord BGADDR          ; address of background routine (0: off)
                           000028     1         BGADDR = RAMPOOL
                           00002A     2         RAMPOOL = RAMPOOL + 2
      00830C                         13         RamWord TICKCNT         ; "TICKCNT" 16 bit ticker (counts up)
                           00002A     1         TICKCNT = RAMPOOL
                           00002C     2         RAMPOOL = RAMPOOL + 2
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
      00830C                         45 _TIM1_IRQHandler:
      00830C                         46 _TIM2_IRQHandler:
      00830C                         47 _TIM3_IRQHandler:
                                     48         ; STM8 DIV/DIVW erratum "Workaround 2: keep bit6 cleared"
      00830C 4B 08            [ 1]   49         PUSH    #0x08           ; BG task fixed priority (I0=1, I1=0)
      00830E 86               [ 1]   50         POP     CC
                                     51 
                           000001    52         .ifne   (HAS_LED7SEG + HAS_BACKGROUND)
      00830F 72 11 53 04      [ 1]   53         BRES    BG_TIM_SR1,#0   ; clear TIMx UIF
                                     54 
                           000001    55         .ifne   HAS_LED7SEG
      008313 CD 81 4D         [ 4]   56         CALL    LED_MPX         ; "PC_LEDMPX" board dependent code for 7Seg-LED-Displays
                                     57         .endif
                                     58 
                                     59 ;       Background operation saves & restores the context of the interactive task
                                     60 ;       Cyclic context reset of Forth background task (stack, BASE, HLD, I/O vector)
                           000001    61         .ifne   HAS_BACKGROUND
      008316 BE 2A            [ 2]   62         LDW     X,TICKCNT
      008318 5C               [ 1]   63         INCW    X
      008319 BF 2A            [ 2]   64         LDW     TICKCNT,X
                                     65         ; fall through
                                     66 
                           000000    67         .ifne   BG_RUNMASK
                                     68         LD      A,XL            ; Background task runs if "(BG_RUNMASK AND TICKCNT) equals 0"
                                     69         AND     A,#BG_RUNMASK
                                     70         JRNE    TIM2IRET
                                     71         .endif
                                     72 
      00831B 90 BE 28         [ 2]   73         LDW     Y,BGADDR        ; address of background task
      00831E 90 5D            [ 2]   74         TNZW    Y               ; 0: background operation off
      008320 27 21            [ 1]   75         JREQ    TIM2IRET
                                     76 
      008322 BE 0A            [ 2]   77         LDW     X,XREG0         ; Save context
      008324 89               [ 2]   78         PUSHW   X
                                     79 
      008325 BE 00            [ 2]   80         LDW     X,USREMIT       ; save EMIT exection vector
      008327 89               [ 2]   81         PUSHW   X
      008328 AE 82 39         [ 2]   82         LDW     X,#EMIT_BG      ; "'BGEMIT" xt of EMIT for BG task
      00832B BF 00            [ 2]   83         LDW     USREMIT,X
                                     84 
      00832D BE 02            [ 2]   85         LDW     X,USRQKEY       ; save QKEY exection vector
      00832F 89               [ 2]   86         PUSHW   X
      008330 AE 81 E9         [ 2]   87         LDW     X,#QKEY_BG      ; "'?BGKEY" xt of ?KEY for BG task
      008333 BF 02            [ 2]   88         LDW     USRQKEY,X
                                     89 
                                     90 ;        LDW     X,USRHLD
                                     91 ;        PUSHW   X
                                     92 ;        LDW     X,#PADBG        ; "BGPAD" empty PAD for BG task
                                     93 ;        LDW     USRHLD,X
                                     94 
      008335 AE 03 D0         [ 2]   95         LDW     X,#BSPP         ; "BGSPP" data stack for BG task
      008338 90 FD            [ 4]   96         CALL    (Y)
                                     97 
                                     98 ;        POPW    X
                                     99 ;        LDW     USRHLD,X
                                    100 
      00833A 85               [ 2]  101         POPW    X
      00833B BF 02            [ 2]  102         LDW     USRQKEY,X
                                    103 
      00833D 85               [ 2]  104         POPW    X
      00833E BF 00            [ 2]  105         LDW     USREMIT,X
                                    106 
      008340 85               [ 2]  107         POPW    X
      008341 BF 0A            [ 2]  108         LDW     XREG0,X
      008343                        109 TIM2IRET:
                                    110         .endif
                                    111 
      008343 80               [11]  112         IRET
                                    113         .endif
                                    114 
                                    115        ;******  BG User Words  ******
                                    116 
                           000001   117         .ifne   HAS_BACKGROUND
                                    118 ;       TIM     ( -- T)     ( TOS STM8: -- Y,Z,N )
                                    119 ;       Return TICKCNT as timer
                                    120 
      008344                        121         HEADER  TIMM "TIM"
      008344                        122 TIMM:
      008344 90 BE 2A         [ 2]  123         LDW     Y,TICKCNT
      008347 CC 85 5B         [ 2]  124         JP      AYSTOR
                                    125 
                                    126         .endif
                                    532 ; UPPLOC = RAMPOOL + 30  ; PAD in Background task, growing down, 32 bytes
                                    533 
                                    534 ; ==============================================
      00834A                        535         HEADER  RETURN "RETURN"
      00834A 81               [ 4]  536 RETURN: RET
                                    537 
                                    538 ;       Configuation table with shadow data for RESET
                                    539 
                                    540 ;       Main entry points and COLD start data
                                    541 
                                    542 
      00834B                        543 _forth:                         ; SDCC entry
                                    544 ;       Note: no return to main.c possible unless RAMEND equals SP,
                                    545 ;       and RPP init skipped
                                    546 
                                    547 ;       COLD    ( -- )
                                    548 ;       The hilevel cold start sequence.
                                    549 
      00834B                        550         HEADER  COLD "COLD"
      00834B                        551 COLD:
      00834B 9B               [ 1]  552         SIM                     ; disable interrupts
      00834C 35 00 50 C6      [ 1]  553         MOV     CLK_CKDIVR,#0   ; Clock divider register
                                    554 
      008350 AE 03 FF         [ 2]  555         LDW     X,#(RAMEND-FORTHRAM)
      008353 6F 00            [ 1]  556 1$:     CLR     (FORTHRAM,X)
      008355 5A               [ 2]  557         DECW    X
      008356 2A FB            [ 1]  558         JRPL    1$
                                    559 
      008358 AE 03 FF         [ 2]  560         LDW     X,#RPP          ; return stack, growing down
      00835B 94               [ 1]  561         LDW     SP,X            ; initialize return stack
                                    562 
                                    563         ; see "boardcore.inc")
      00835C CD 81 2A         [ 4]  564         CALL    BOARDINIT       ; "PC_BOARDINIT" Board initialization
                                    565 
      00835F                        566         BGTASK_Init             ; macro for init of BG task timer, refer to bgtask.inc
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
      00835F 35 03 53 0E      [ 1]   12         MOV     BG_TIM_PSCR,#3  ; prescaler 1/(2^3) = 1/8
                                     13         .endif
      008363 72 17 7F 73      [ 1]   14         BRES    ITC_SPR1+(BG_INT/4),#((BG_INT%4)*2+1)  ; Interrupt prio. low
                                     15 
      008367 35 26 53 0F      [ 1]   16         MOV     BG_TIM_ARRH,#(BG_TIM_REL/256)  ; reload H
      00836B 35 DE 53 10      [ 1]   17         MOV     BG_TIM_ARRL,#(BG_TIM_REL%256)  ;        L
      00836F 35 01 53 00      [ 1]   18         MOV     BG_TIM_CR1,#0x01 ; enable background timer
      008373 35 01 53 03      [ 1]   19         MOV     BG_TIM_IER,#0x01 ; enable background timer interrupt
                                     20         .endif
                                    567 
                           000000   568         .ifne   HAS_RXUART+HAS_TXUART
                                    569         ; Init RS232 communication port
                                    570         ; STM8S[01]003F3 init UART
                                    571         LDW     X,#CUARTBRR      ; "UARTBRR" def. $6803 / 9600 baud
                                    572         LDW     UART_BRR1,X
                                    573         .ifne   HAS_RXUART*HAS_TXUART
                                    574         MOV     UART_CR2,#0x2C  ; Use UART1 full duplex + RXNE interrupt
                                    575 .ifeq (FAMILY - STM8S)
                                    576         MOV    ITC_SPR5,#0xCF   ; enable TIM2 interrupts while chatting
                                    577 .else
                                    578         MOV    ITC_SPR8,#0xC   ; enable TIM2 interrupts while chatting
                                    579 .endif
                                    580         .ifne   HALF_DUPLEX
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
                                    609         .else
                                    610         .ifne   HAS_TXUART
                                    611         MOV     UART_CR2,#0x08  ; UART1 enable tx
                                    612         .endif
                                    613         .ifne   HAS_RXUART
                                    614         MOV     UART_CR2,#0x04  ; UART1 enable rx
                                    615         .endif
                                    616         .endif
                                    617         .endif
                                    618 
      008377                        619         SSER_Init               ; macro for init of simulated serial, refer to sser.inc
                           000001     1         .ifne   HAS_RXSIM+HAS_TXSIM
                                      2 
                           000000     3           .ifeq   (FAMILY - STM8L)
                                      4         BSET    CLK_PCKENR1,#2  ; STM8L clock tree: enable TIM4
                                      5           .endif
                                      6 
                                      7          ; TIM4 based RXD or TXD: initialize timer
      008377 35 45 53 48      [ 1]    8         MOV     TIM4_ARR,#CTIM4ARR
                                      9 
      00837B 35 02 53 47      [ 1]   10         MOV     TIM4_PSCR,#CTIM4PSCR ; prescaler 1/(2^n)
      00837F 35 01 53 40      [ 1]   11         MOV     TIM4_CR1,#0x01  ; enable TIM4
                                     12         .endif
                                     13 
                           000001    14         .ifne   HAS_TXSIM*((PNRX-PNTX)+(1-HAS_RXSIM))
                                     15         ; init TxD through GPIO if not shared pin with PNRX
      008383 72 1A 50 0A      [ 1]   16         BSET    PSIM+ODR,#PNTX  ; PNTX GPIO high
      008387 72 1A 50 0C      [ 1]   17         BSET    PSIM+DDR,#PNTX  ; PNTX GPIO output
      00838B 72 1A 50 0D      [ 1]   18         BSET    PSIM+CR1,#PNTX  ; enable PNTX push-pull
                                     19         .endif
                                     20 
                           000001    21         .ifne   (HAS_RXSIM)
                                     22           ; init RxD EXTI for GPIO
                           000000    23           .ifeq   (FAMILY - STM8L)
                                     24             ; STM8L EXTI for port bit 0..7
                                     25             .ifeq   (PNRX / 4)
                                     26         BSET    EXTI_CR1,#1+PNRX*2     ; ext. int. port bit 0..3 falling edge
                                     27             .else
                                     28         BSET    EXTI_CR2,#1+(PNRX-4)*2 ; ext. int. port bit 4..7 falling edge
                                     29             .endif
                           000001    30           .else
                                     31             ; STM8S EXTI for 8 bit port
                           000000    32             .ifeq   (PSIM-PORTA)
                                     33         BSET    EXTI_CR1,#1     ; External interrupt Port A falling edge
                           000001    34             .else
                           000000    35               .ifeq   (PSIM-PORTB)
                                     36         BSET    EXTI_CR1,#3     ; External interrupt Port B falling edge
                           000001    37               .else
                           000001    38                 .ifeq   (PSIM-PORTC)
      00838F 72 1A 50 A0      [ 1]   39         BSET    EXTI_CR1,#5     ; External interrupt Port C falling edge
                           000000    40               .else
                                     41         BSET    EXTI_CR1,#7     ; External interrupt Port D falling edge
                                     42               .endif
                                     43             .endif
                                     44           .endif
                                     45         .endif
      008393 72 19 50 0C      [ 1]   46         BRES    PSIM+DDR,#PNRX    ; 0: input (default)
      008397 72 18 50 0D      [ 1]   47         BSET    PSIM+CR1,#PNRX    ; enable PNRX pull-up
      00839B 72 18 50 0E      [ 1]   48         BSET    PSIM+CR2,#PNRX    ; enable PNRX external interrupt
                                     49         .endif
                                    620 
      00839F                        621         Board_IO_Init           ; macro board_io initialization (7S-LED)
                           000000     1         .if     gt,(HAS_LED7SEG-1)
                                      2         MOV     LED7GROUP,#0     ; one of position HAS_LED7SEG 7-SEG digit groups
                                      3         .endif
      00839F 35 66 00 20      [ 1]    4         MOV     LED7FIRST  ,#0x66 ; 7S LEDs 4..
      0083A3 35 78 00 21      [ 1]    5         MOV     LED7FIRST+1,#0x78 ; 7S LEDs .t.
      0083A7 35 74 00 22      [ 1]    6         MOV     LED7FIRST+2,#0x74 ; 7S LEDs ..h
                                    622 
      0083AB AE 03 A0         [ 2]  623         LDW     X,#SPP          ; initialize data stack, TIB
                                    624         
      0083AE 5A               [ 2]  625         DECW X                  ; initialise get bit / set bit routines in ram
      0083AF 5A               [ 2]  626         DECW X  
      0083B0 90 AE 81 1B      [ 2]  627         LDW Y,#COLD1
      0083B4 FF               [ 2]  628         LDW (X),Y
      0083B5 5A               [ 2]  629         DECW X  
      0083B6 5A               [ 2]  630         DECW X  
      0083B7 90 AE 00 0E      [ 2]  631         LDW Y,#BITAT  
      0083BB FF               [ 2]  632         LDW (X),Y
      0083BC 5A               [ 2]  633         DECW X  
      0083BD 5A               [ 2]  634         DECW X  
      0083BE 90 AE 00 0F      [ 2]  635         LDW Y,#15
      0083C2 FF               [ 2]  636         LDW (X),Y
      0083C3 CD 87 CD         [ 4]  637         CALL CMOVE
                                    638 
                                    639 
                           000001   640 .if  HAS_RXSIM
      0083C6 35 FF 00 04      [ 1]  641 MOV USR_5,#255
                                    642 .endif
                                    643 
                                    644         ; Hardware initialization complete
      0083CA 9A               [ 1]  645         RIM                     ; enable interrupts
                                    646 
      0083CB CD 83 4A         [ 4]  647 TBOOT:  CALL    RETURN       ; application boot, can be changed with ' appl 'BOOT flash!
      0083CE 8F               [10]  648 SLEEP:     WFI
                           000001   649 .if HAS_RXSIM
      0083CF 3D 05            [ 1]  650         TNZ USR_6
      0083D1 27 03            [ 1]  651         JREQ $1
      0083D3 CD 80 FE         [ 4]  652         CALL CHAT
      0083D6 20 F6            [ 2]  653 $1:    JRA      SLEEP
                                    654 .endif
                                    655 
                           000000   656 .if    HAS_RXUART*HAS_TXUART       
                                    657         JRA      SLEEP
                                    658 UART_INT:
                                    659         CALL CHAT                 ; during chat data SP is communicated by muforth
                                    660         LDW (#3,SP),X             ; X (data SP) is popped from return stack during IRET
                                    661         IRET
                                    662 .endif
                                    663 
                                    664 ; ==============================================
                                    665 
                                    666 ;       Device dependent I/O
                                    667 
                           000000   668         .ifne   HAS_RXUART
                                    669 ;       ?RX     ( -- c T | F )  ( TOS STM8: -- Y,Z,N )
                                    670 ;       Return serial interface input char from and true, or false.
                                    671 
                                    672         HEADER  QRX "?RX"
                                    673 QRX:
                                    674         CLR     A               ; A: flag false
                                    675         BTJF    UART_SR,#5,1$
                                    676         LD      A,UART_DR      ; get char in A
                                    677 1$:
                                    678      JP      ASTOR          ; push char
                                    679         .endif
                                    680 
                           000000   681         .ifne   HAS_TXUART
                                    682 ;       TX!     ( c -- )
                                    683 ;       Send character c to the serial interface.
                                    684 
                                    685         HEADER  TXSTOR "TX!"
                                    686 TXSTOR:
                                    687         INCW    X
                                    688         LD      A,(X)
                                    689         INCW    X
                                    690 
                                    691         HEADER  TXASTOR "TXA!"
                                    692 TXASTOR:
                                    693         .ifne   HALF_DUPLEX
                                    694         ; HALF_DUPLEX with normal UART (e.g. wired-or Rx and Tx)
                                    695 1$:     BTJF    UART_SR,#7,1$  ; loop until tdre
                                    696         BRES    UART_CR2,#2    ; disable rx
                                    697         LD      UART_DR,A      ; send A
                                    698 2$:     BTJF    UART_SR,#6,2$  ; loop until tc
                                    699         BSET    UART_CR2,#2    ; enable rx
                                    700         .else                  ; not HALF_DUPLEX
                                    701 1$:     BTJF    UART_SR,#7,1$  ; loop until tdre
                                    702         LD      UART_DR,A      ; send A
                                    703         .endif
                                    704         RET
                                    705         .endif
                                    706 
                                    707 ; ==============================================
                                    708 
                                    709 ;       Device independent I/O
                                    710 
                                    711 ;       ?KEY    ( -- c T | F )  ( TOS STM8: -- Y,Z,N )
                                    712 ;       Return input char and true, or false.
      0083D8                        713         HEADER  QKEY "?KEY"
      0083D8                        714 QKEY:
      0083D8 92 CC 02         [ 5]  715         JP      [USRQKEY]
                                    716 
                                    717 ;       EMIT    ( c -- )
                                    718 ;       Send character c to output device.
                                    719 
      0083DB                        720         HEADER  EMIT "EMIT"
      0083DB                        721 EMIT:
      0083DB 92 CC 00         [ 5]  722         JP      [USREMIT]
                                    723 
                                    724 ; ==============================================
                                    725 ; The kernel
                                    726 
                                    727 ;       ?branch ( f -- )
                                    728 ;       Branch if flag is zero.
                                    729 
      0083DE                        730         HEADFLG QBRAN "?branch" COMPO
                                      1 
      0083DE                        731 QBRAN:
      0083DE 90 93            [ 1]  732         LDW     Y,X
      0083E0 5C               [ 1]  733         INCW    X
      0083E1 5C               [ 1]  734         INCW    X
      0083E2 90 FE            [ 2]  735         LDW     Y,(Y)
      0083E4 27 05            [ 1]  736         JREQ    BRAN
      0083E6                        737 WSKIPRET:
      0083E6 90 85            [ 2]  738         POPW    Y
      0083E8 90 EC 02         [ 2]  739         JP      (2,Y)
                                    740 
                                    741 
                                    742 ;       branch  ( -- )
                                    743 ;       Branch to an inline address.
                                    744 
      0083EB                        745         HEADFLG BRAN "branch" COMPO    ; NOALIAS
                                      1 
      0083EB                        746 BRAN:
      0083EB 90 85            [ 2]  747         POPW    Y
      0083ED 90 FE            [ 2]  748         LDW     Y,(Y)
      0083EF 90 FC            [ 2]  749         JP      (Y)
                                    750 
                                    751 
                                    752 ;       EXECUTE ( ca -- )
                                    753 ;       Execute word at ca.
                                    754 
      0083F1                        755         HEADER  EXECU "EXECUTE"
      0083F1                        756 EXECU:
      0083F1 90 93            [ 1]  757         LDW     Y,X
      0083F3 5C               [ 1]  758         INCW    X
      0083F4 5C               [ 1]  759         INCW    X
      0083F5 90 FE            [ 2]  760         LDW     Y,(Y)
      0083F7 90 FC            [ 2]  761         JP      (Y)
                                    762 
                           000001   763         .ifeq   BOOTSTRAP
                                    764 ;       2!      ( d a -- )      ( TOS STM8: -- Y,Z,N )
                                    765 ;       Store double integer to address a.
                                    766 
      0083F9                        767         HEADER  DSTOR "2!"
      0083F9                        768 DSTOR:
      0083F9 90 93            [ 1]  769         LDW Y,X
      0083FB 90 FE            [ 2]  770         LDW Y,(Y)
      0083FD E6 04            [ 1]  771         LD A,(4,X)
      0083FF 90 F7            [ 1]  772         LD (Y),A
      008401 E6 05            [ 1]  773         LD A,(5,X)
      008403 90 E7 01         [ 1]  774         LD (1,Y),A
      008406 E6 02            [ 1]  775         LD A,(2,X)
      008408 90 E7 02         [ 1]  776         LD (2,Y),A
      00840B E6 03            [ 1]  777         LD A,(3,X)
      00840D 90 E7 03         [ 1]  778         LD (3,Y),A
      008410 1C 00 06         [ 2]  779         ADDW X,#6
      008413 81               [ 4]  780         RET
                                    781         .endif
                                    782 
                                    783 ;       2@      ( a -- d )
                                    784 ;       Fetch double integer from address a.
                                    785 
      008414                        786         HEADER  DAT "2@"
      008414                        787 DAT:
      008414 90 93            [ 1]  788         LDW Y,X
      008416 FE               [ 2]  789         LDW X,(X)
      008417 F6               [ 1]  790         LD A,(X)
      008418 51               [ 1]  791         EXGW X,Y
      008419 F7               [ 1]  792         LD (X),A
      00841A 90 E6 01         [ 1]  793         LD A,(1,Y)
      00841D E7 01            [ 1]  794         LD (1,X),A
      00841F 5A               [ 2]  795         DECW X
      008420 90 E6 03         [ 1]  796         LD A,(3,Y)
      008423 F7               [ 1]  797         LD (X),A
      008424 5A               [ 2]  798         DECW X
      008425 90 E6 02         [ 1]  799         LD A,(2,Y)
      008428 F7               [ 1]  800         LD (X),A
      008429 81               [ 4]  801         RET
                                    802 
                                    803 ;       2C!  ( n a -- )
                                    804 ;       Store word C-wise to 16 bit HW registers "MSB first"
                                    805 
      00842A                        806         HEADER  DCSTOR "2C!"
      00842A                        807 DCSTOR:
      00842A 90 93            [ 1]  808         LDW     Y,X
      00842C 5C               [ 1]  809         INCW    X
      00842D 5C               [ 1]  810         INCW    X
      00842E 90 FE            [ 2]  811         LDW     Y,(Y)
      008430 F6               [ 1]  812         LD      A,(X)
      008431 90 F7            [ 1]  813         LD      (Y),A           ; write MSB(n) to a
      008433 5C               [ 1]  814         INCW    X
      008434 F6               [ 1]  815         LD      A,(X)
      008435 90 E7 01         [ 1]  816         LD      (1,Y),A         ; write LSB(n) to a+1
      008438 5C               [ 1]  817         INCW    X
      008439 81               [ 4]  818         RET
                                    819 
                                    820 ;       2C@  ( a -- n )
                                    821 ;       Fetch word C-wise from 16 bit HW config. registers "MSB first"
                                    822 
      00843A                        823         HEADER  DCAT "2C@"
      00843A                        824 DCAT:
      00843A 90 93            [ 1]  825         LDW     Y,X
      00843C FE               [ 2]  826         LDW     X,(X)
      00843D F6               [ 1]  827         LD      A,(X)
      00843E 90 F7            [ 1]  828         LD      (Y),A
      008440 E6 01            [ 1]  829         LD      A,(1,X)
      008442 51               [ 1]  830         EXGW    X,Y
      008443 E7 01            [ 1]  831         LD      (1,X),A
      008445 81               [ 4]  832         RET
                                    833 
                                    834 ;       BF@ ( a u -- 0|1)
                                    835 ;       Read bit #u (0..2047) in a cell array (16 bit words) at address a
                                    836 ;       Note: fills BITAT RAM-routine with stack values and jumps to BITAT
      008446                        837         HEADER  BFAT "BF@"
      008446                        838 BFAT:
      008446 90 93            [ 1]  839         LDW Y, X
      008448 FE               [ 2]  840         LDW X, (X)
      008449 A6 08            [ 1]  841         LD A,#8
      00844B 62               [ 2]  842         DIV X,A
      00844C 48               [ 1]  843         SLL A
      00844D 4C               [ 1]  844         INC A
      00844E B7 10            [ 1]  845         LD BITAT+2,A
      008450 9F               [ 1]  846         LD A,XL
      008451 A8 01            [ 1]  847         XOR A,#01
      008453 B7 0B            [ 1]  848         LD XREG0+1,A
      008455 3F 0A            [ 1]  849         CLR XREG0
      008457 93               [ 1]  850         LDW X,Y
      008458 EE 02            [ 2]  851         LDW X,(02,X)
      00845A 72 BB 00 0A      [ 2]  852         ADDW X,XREG0
      00845E BF 11            [ 2]  853         LDW BITAT+3,X
      008460 51               [ 1]  854         EXGW X,Y
      008461 5C               [ 1]  855         INCW X
      008462 5C               [ 1]  856         INCW X
      008463 CC 00 0E         [ 2]  857         JP BITAT
                                    858 
                                    859 ;       BF! ( a u -- 0|1)
                                    860 ;       Write bit to a bitfield stored in one or more cells (16 bit words)
                                    861 ;       Set/reset bit #u (0..8191) in a cell array starting at address a to bool t
      008466                        862         HEADER  BFSTO "BF!"
      008466                        863 BFSTO:
      008466 90 93            [ 1]  864         LDW Y,X
      008468 FE               [ 2]  865         LDW X,(X)
      008469 9F               [ 1]  866         LD A,XL
      00846A A4 07            [ 1]  867         AND A,#7
      00846C 88               [ 1]  868         PUSH A
      00846D 9F               [ 1]  869         LD A,XL
      00846E 47               [ 1]  870         SRA A
      00846F 47               [ 1]  871         SRA A
      008470 47               [ 1]  872         SRA A
      008471 A8 01            [ 1]  873         XOR A,#1
      008473 93               [ 1]  874         LDW X,Y
      008474 EB 03            [ 1]  875         ADD A,(3,X)
      008476 E7 03            [ 1]  876         LD (3,X),A
      008478 A6 00            [ 1]  877         LD A,#0
      00847A E9 02            [ 1]  878         ADC A,(2,X)
      00847C E7 02            [ 1]  879         LD (2,X),A
      00847E 84               [ 1]  880         POP A
      00847F 7F               [ 1]  881         CLR (X)
      008480 E7 01            [ 1]  882         LD (1,X),A         ; fall through
                                    883 
                                    884 ;       B! ( t a u -- )
                                    885 ;       Set/reset bit #u (0..7) in the byte at address a to bool t
                                    886 ;       Note: executes BSER/BRES + RET code in RAM
      008482                        887         HEADER  BRSS "B!"
      008482                        888 BRSS:
      008482 E6 01            [ 1]  889         LD A,(1,X)
      008484 48               [ 1]  890         SLL A                                     
      008485 AA 10            [ 1]  891         OR A,#16                                
      008487 B7 19            [ 1]  892         ld BITSTO+1,A                                  
      008489 E6 05            [ 1]  893         LD A,(05,X)                             
      00848B 26 02            [ 1]  894         JRNE 1$                       
      00848D 3C 19            [ 1]  895         INC BITSTO+1                                   
      00848F E6 02            [ 1]  896 1$:     LD A,(02,X)                             
      008491 B7 1A            [ 1]  897         LD BITSTO+2,A                                  
      008493 E6 03            [ 1]  898         LD A,(03,X)                             
      008495 B7 1B            [ 1]  899         LD BITSTO+3,A                                  
      008497 1C 00 06         [ 2]  900         ADDW X,#6                               
      00849A CC 00 18         [ 2]  901         JP BITSTO                                    
                                    902 
                                    903 ;       B@ ( a u -- )
                                    904 ;       Get bit #u (0..7) in the byte at address a
                                    905 ;       Note: executes BSER/BRES + RET code in RAM
      00849D                        906         HEADER  BAT "B@"
      00849D                        907 BAT:
      00849D E6 01            [ 1]  908         LD A,(1,X)
      00849F 48               [ 1]  909         SLA A
      0084A0 4C               [ 1]  910         INC A
      0084A1 B7 10            [ 1]  911         LD BITAT+2,A
      0084A3 E6 02            [ 1]  912         LD A,(2,X)
      0084A5 B7 11            [ 1]  913         LD BITAT+3,A
      0084A7 E6 03            [ 1]  914         LD A,(3,X)
      0084A9 B7 12            [ 1]  915         LD BITAT+4,A
      0084AB 5C               [ 1]  916         INCW X
      0084AC 5C               [ 1]  917         INCW X
      0084AD 7F               [ 1]  918         CLR (X)
      0084AE CC 00 0E         [ 2]  919         JP BITAT
                                    920 
                                    921 ;       @       ( a -- w )      ( TOS STM8: -- Y,Z,N )
                                    922 ;       Push memory location to stack.
                                    923 
      0084B1                        924         HEADER  AT "@"
      0084B1                        925 AT:
      0084B1 90 93            [ 1]  926         LDW     Y,X
      0084B3 FE               [ 2]  927         LDW     X,(X)
      0084B4 FE               [ 2]  928         LDW     X,(X)
      0084B5 51               [ 1]  929         EXGW    X,Y
      0084B6 FF               [ 2]  930         LDW     (X),Y
      0084B7 81               [ 4]  931         RET
                                    932 
                                    933 ;       !       ( w a -- )      ( TOS STM8: -- Y,Z,N )
                                    934 ;       Pop data stack to memory.
                                    935 
      0084B8                        936         HEADER  STORE "!"
      0084B8                        937 STORE:
      0084B8 90 93            [ 1]  938         LDW     Y,X             ; (14 bytes, 16 cy)
      0084BA 5C               [ 1]  939         INCW    X
      0084BB 5C               [ 1]  940         INCW    X
      0084BC 90 FE            [ 2]  941         LDW     Y,(Y)
      0084BE 89               [ 2]  942         PUSHW   X
      0084BF FE               [ 2]  943         LDW     X,(X)           ; w
      0084C0 90 FF            [ 2]  944         LDW     (Y),X
      0084C2 85               [ 2]  945         POPW    X
      0084C3 5C               [ 1]  946         INCW    X
      0084C4 5C               [ 1]  947         INCW    X
      0084C5 81               [ 4]  948         RET
                                    949 
                                    950 ;       C@      ( a -- c )      ( TOS STM8: -- A,Z,N )
                                    951 ;       Push byte in memory to stack.
                                    952 ;       STM8: Z,N
                                    953 
      0084C6                        954         HEADER  CAT "C@"
      0084C6                        955 CAT:
      0084C6 90 93            [ 1]  956         LDW     Y,X             ; Y=a
      0084C8 90 FE            [ 2]  957         LDW     Y,(Y)
      0084CA                        958 YCAT:
      0084CA 90 F6            [ 1]  959         LD      A,(Y)
      0084CC 7F               [ 1]  960         CLR     (X)
      0084CD E7 01            [ 1]  961         LD      (1,X),A
      0084CF 81               [ 4]  962         RET
                                    963 
                                    964 ;       C!      ( c a -- )
                                    965 ;       Pop     data stack to byte memory.
                                    966 
      0084D0                        967         HEADER  CSTOR "C!"
      0084D0                        968 CSTOR:
      0084D0 90 93            [ 1]  969         LDW     Y,X
      0084D2 5C               [ 1]  970         INCW    X
      0084D3 5C               [ 1]  971         INCW    X
      0084D4 90 FE            [ 2]  972         LDW     Y,(Y)
      0084D6 5C               [ 1]  973         INCW    X
      0084D7 F6               [ 1]  974         LD      A,(X)
      0084D8 90 F7            [ 1]  975         LD      (Y),A
      0084DA 5C               [ 1]  976         INCW    X
      0084DB 81               [ 4]  977         RET
                                    978 
                                    979 ;       OVER    ( w1 w2 -- w1 w2 w1 ) ( TOS STM8: -- Y,Z,N )
                                    980 ;       Copy second stack item to top.
                                    981 
      0084DC                        982         HEADER  OVER "OVER"
      0084DC                        983 OVER:
      0084DC E6 03            [ 1]  984         LD A,(3,X)
      0084DE 5A               [ 2]  985         DECW X
      0084DF F7               [ 1]  986         LD (X),A
      0084E0 E6 03            [ 1]  987         LD A,(3,X)
      0084E2 5A               [ 2]  988         DECW X
      0084E3 F7               [ 1]  989         LD (X),A
      0084E4 81               [ 4]  990         RET
                                    991 
                                    992 ;       NIP     ( n1 n2 -- n2 )
                                    993 ;       Drop 2nd item on the stack.
                                    994 
      0084E5                        995         HEADER  NIP "NIP"
      0084E5                        996 NIP:
      0084E5 F6               [ 1]  997         LD A,(X)  
      0084E6 E7 02            [ 1]  998         LD (2,X),A  
      0084E8 E6 01            [ 1]  999         LD A,(1,X)  
      0084EA E7 03            [ 1] 1000         LD (3,X),A  ; fall through
                                   1001 
                                   1002 ;       DROP     ( n1 -- )
                                   1003 ;       Drop top stack item.
                                   1004 
      0084EC                       1005         HEADER  DROP "DROP"
      0084EC 5C               [ 1] 1006 DROP:   INCW    X
      0084ED 5C               [ 1] 1007         INCW    X
      0084EE 81               [ 4] 1008         RET
                                   1009 
                                   1010 ;       DUP     ( w -- w w )    ( TOS STM8: -- Y,Z,N )
                                   1011 ;       Duplicate top stack item.
                                   1012 
      0084EF                       1013         HEADER  DUPP "DUP"
      0084EF                       1014 DUPP:
      0084EF 90 93            [ 1] 1015         LDW     Y,X
      0084F1 90 FE            [ 2] 1016         LDW     Y,(Y)
      0084F3 5A               [ 2] 1017         DECW    X               ; SUBW  X,#2
      0084F4 5A               [ 2] 1018         DECW    X
      0084F5 FF               [ 2] 1019         LDW     (X),Y           ; push on stack
      0084F6 81               [ 4] 1020         RET
                                   1021 
                                   1022 ;       SWAP ( w1 w2 -- w2 w1 ) ( TOS STM8: -- Y,Z,N )
                                   1023 ;       Exchange top two stack items.
                                   1024 
      0084F7                       1025         HEADER  SWAPP "SWAP"
      0084F7                       1026 SWAPP:
      0084F7 90 93            [ 1] 1027         LDW     Y,X
      0084F9 EE 02            [ 2] 1028         LDW     X,(2,X)
      0084FB 89               [ 2] 1029         PUSHW   X
      0084FC 93               [ 1] 1030         LDW     X,Y
      0084FD FE               [ 2] 1031         LDW     X,(X)
      0084FE 51               [ 1] 1032         EXGW    X,Y
      0084FF EF 02            [ 2] 1033         LDW     (2,X),Y
      008501 90 85            [ 2] 1034         POPW    Y
      008503 FF               [ 2] 1035         LDW     (X),Y
      008504 81               [ 4] 1036         RET
                                   1037 
                                   1038 ;       UM+     ( u u -- udsum )
                                   1039 ;       Add two unsigned single
                                   1040 ;       and return a double sum.
                                   1041 
      008505                       1042         HEADER  UPLUS "UM+"
      008505                       1043 UPLUS:
      008505 AD 05            [ 4] 1044         CALLR   PLUS
      008507 4F               [ 1] 1045         CLR     A
      008508 49               [ 1] 1046         RLC     A
      008509 CC 85 57         [ 2] 1047         JP      ASTOR
                                   1048 
                                   1049 ;       +       ( w w -- sum ) ( TOS STM8: -- Y,Z,N )
                                   1050 ;       Add top two items.
                                   1051 
      00850C                       1052         HEADER  PLUS "+"
                                   1053 
      00850C                       1054 PLUS:
      00850C E6 01            [ 1] 1055         LD      A,(1,X) ;D=w
      00850E EB 03            [ 1] 1056         ADD     A,(3,X)
      008510 E7 03            [ 1] 1057         LD      (3,X),A
      008512 F6               [ 1] 1058         LD      A,(X)
      008513 E9 02            [ 1] 1059         ADC     A,(2,X)
      008515                       1060 LDADROP:
      008515 5C               [ 1] 1061         INCW    X
      008516 5C               [ 1] 1062         INCW    X
      008517 F7               [ 1] 1063         LD      (X),A
      008518 81               [ 4] 1064         RET
                                   1065 
                                   1066 ;       XOR     ( w w -- w )    ( TOS STM8: -- Y,Z,N )
                                   1067 ;       Bitwise exclusive OR.
                                   1068 
      008519                       1069         HEADER  XORR "XOR"
      008519                       1070 XORR:
      008519 E6 01            [ 1] 1071         LD      A,(1,X)         ; D=w
      00851B E8 03            [ 1] 1072         XOR     A,(3,X)
      00851D E7 03            [ 1] 1073         LD      (3,X),A
      00851F F6               [ 1] 1074         LD      A,(X)
      008520 E8 02            [ 1] 1075         XOR     A,(2,X)
      008522 20 F1            [ 2] 1076         JRA     LDADROP
                                   1077 
                                   1078 ;       AND     ( w w -- w )    ( TOS STM8: -- Y,Z,N )
                                   1079 ;       Bitwise AND.
                                   1080 
      008524                       1081         HEADER  ANDD "AND"
      008524                       1082 ANDD:
      008524 E6 01            [ 1] 1083         LD      A,(1,X)         ; D=w
      008526 E4 03            [ 1] 1084         AND     A,(3,X)
      008528 E7 03            [ 1] 1085         LD      (3,X),A
      00852A F6               [ 1] 1086         LD      A,(X)
      00852B E4 02            [ 1] 1087         AND     A,(2,X)
      00852D 20 E6            [ 2] 1088         JRA     LDADROP
                                   1089 
                                   1090 ;       OR      ( w w -- w )    ( TOS STM8: -- immediate Y,Z,N )
                                   1091 ;       Bitwise inclusive OR.
                                   1092 
      00852F                       1093         HEADER  ORR "OR"
      00852F                       1094 ORR:
      00852F E6 01            [ 1] 1095         LD      A,(1,X)         ; D=w
      008531 EA 03            [ 1] 1096         OR      A,(3,X)
      008533 E7 03            [ 1] 1097         LD      (3,X),A
      008535 F6               [ 1] 1098         LD      A,(X)
      008536 EA 02            [ 1] 1099         OR      A,(2,X)
      008538 20 DB            [ 2] 1100         JRA     LDADROP
                                   1101 
                                   1102 ;       0<      ( n -- t ) ( TOS STM8: -- A,Z )
                                   1103 ;       Return true if n is negative.
                                   1104 
      00853A                       1105         HEADER  ZLESS "0<"
      00853A                       1106 ZLESS:
      00853A 4F               [ 1] 1107         CLR     A
      00853B 7D               [ 1] 1108         TNZ     (X)
      00853C 2A 01            [ 1] 1109         JRPL    ZL1
      00853E 43               [ 1] 1110         CPL     A               ; true
      00853F F7               [ 1] 1111 ZL1:    LD      (X),A
      008540 E7 01            [ 1] 1112         LD      (1,X),A
      008542 81               [ 4] 1113         RET
                                   1114 
                                   1115 ;       -   ( n1 n2 -- n1-n2 )  ( TOS STM8: -- Y,Z,N )
                                   1116 ;       Subtraction.
                                   1117 
      008543                       1118         HEADER  SUBB "-"
                                   1119 
      008543                       1120 SUBB:
                           000000  1121         .ifeq   SPEEDOVERSIZE
                                   1122         CALL    NEGAT           ; (15 cy)
                                   1123         JRA     PLUS            ; 25 cy (15+10)
                           000001  1124         .else
      008543 90 93            [ 1] 1125         LDW     Y,X
      008545 90 FE            [ 2] 1126         LDW     Y,(Y)
      008547 90 BF 0A         [ 2] 1127         LDW     XREG0,Y
      00854A 5C               [ 1] 1128         INCW    X
      00854B 5C               [ 1] 1129         INCW    X
      00854C 90 93            [ 1] 1130         LDW     Y,X
      00854E 90 FE            [ 2] 1131         LDW     Y,(Y)
      008550 72 B2 00 0A      [ 2] 1132         SUBW    Y,XREG0
      008554 FF               [ 2] 1133         LDW     (X),Y
      008555 81               [ 4] 1134         RET                     ; 18 cy
                                   1135         .endif
                                   1136 
      008556                       1137 ZERO:
      008556 4F               [ 1] 1138         CLR A
                                   1139 
                                   1140 ;       A>  ( -- n )     ( TOS STM8: - Y,Z,N )
                                   1141 ;       push A to stack
                                   1142 
      008557                       1143         HEADER  ASTOR "A>"
      008557                       1144 ASTOR:
      008557 90 5F            [ 1] 1145         CLRW    Y
      008559 90 97            [ 1] 1146         LD      YL,A
      00855B                       1147 AYSTOR:
      00855B 5A               [ 2] 1148         DECW    X               ; SUBW  X,#2
      00855C 5A               [ 2] 1149         DECW    X
      00855D FF               [ 2] 1150         LDW     (X),Y           ; push on stack
      00855E 81               [ 4] 1151         RET
                                   1152 
                                   1153 ;       ATOKEY core ( -- c T | f )    ( TOS STM8: - Y,Z,N )
                                   1154 ;       Return input char and true, or false.
                                   1155 
      00855F                       1156         HEADER  ATOKEY "A>KEY"
      00855F                       1157 ATOKEY:
      00855F 4D               [ 1] 1158         TNZ     A
      008560 27 F5            [ 1] 1159         JREQ    ASTOR
      008562 AD F3            [ 4] 1160         CALLR   ASTOR              ; push char
      008564 5A               [ 2] 1161         DECW X
      008565 5A               [ 2] 1162         DECW X
      008566 90 AE 0F FF      [ 2] 1163         LDW Y,#0XFFF
      00856A FF               [ 2] 1164         LDW (X),Y
      00856B 81               [ 4] 1165         RET
                                   1166 
                                   1167 ; Common functions
                                   1168 
                                   1169 ;       ?DUP    ( w -- w w | 0 )   ( TOS STM8: -- Y,Z,N )
                                   1170 ;       Dup tos if its not zero.
      00856C                       1171         HEADER  QDUP "?DUP"
      00856C                       1172 QDUP:
      00856C 90 93            [ 1] 1173         LDW     Y,X
      00856E 90 FE            [ 2] 1174         LDW     Y,(Y)
      008570 27 03            [ 1] 1175         JREQ    QDUP1
      008572 5A               [ 2] 1176         DECW    X
      008573 5A               [ 2] 1177         DECW    X
      008574 FF               [ 2] 1178         LDW     (X),Y
      008575 81               [ 4] 1179 QDUP1:  RET
                                   1180 
                                   1181 ;       ROT     ( w1 w2 w3 -- w2 w3 w1 ) ( TOS STM8: -- Y,Z,N )
                                   1182 ;       Rot 3rd item to top.
                                   1183 
      008576                       1184         HEADER  ROT "ROT"
      008576                       1185 ROT:
                           000001  1186 .if 1
                                   1187 ; 31 bytes, 20 cy
      008576 F6               [ 1] 1188 LD A,(X)
      008577 B7 0A            [ 1] 1189 LD XREG0,A
      008579 E6 01            [ 1] 1190 LD A,(1,X)
      00857B B7 0B            [ 1] 1191 LD XREG0+1,A
      00857D E6 04            [ 1] 1192 LD A,(4,X)
      00857F F7               [ 1] 1193 LD (X),A
      008580 E6 05            [ 1] 1194 LD A,(5,X)
      008582 E7 01            [ 1] 1195 LD (1,X),A
      008584 E6 02            [ 1] 1196 LD A,(2,X)
      008586 E7 04            [ 1] 1197 LD (4,X),A
      008588 E6 03            [ 1] 1198 LD A,(3,X)
      00858A E7 05            [ 1] 1199 LD (5,X),A
      00858C B6 0A            [ 1] 1200 LD A,XREG0
      00858E E7 02            [ 1] 1201 LD (2,X),A
      008590 B6 0B            [ 1] 1202 LD A,XREG0+1
      008592 E7 03            [ 1] 1203 LD (3,X),A
      008594 81               [ 4] 1204 RET
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
      008595                       1241         HEADER  DDUP "2DUP"
      008595                       1242 DDUP:
      008595 AD 00            [ 4] 1243         CALLR    1$
      008597                       1244 1$:
      008597 CC 84 DC         [ 2] 1245         JP      OVER
                                   1246 
                           000001  1247         .ifeq   UNLINKCORE
                                   1248 ;       DNEGATE ( d -- -d )     ( TOS STM8: -- Y,Z,N )
                                   1249 ;       Two's complement of top double.
                                   1250 
      00859A                       1251         HEADER  DNEGA "DNEGATE"
      00859A                       1252 DNEGA:
      00859A 90 93            [ 1] 1253         LDW     Y,X
      00859C 90 EE 02         [ 2] 1254         LDW     Y,(2,Y)
      00859F 90 50            [ 2] 1255         NEGW    Y
      0085A1 8A               [ 1] 1256         PUSH    CC
      0085A2 EF 02            [ 2] 1257         LDW     (2,X),Y
      0085A4 90 93            [ 1] 1258         LDW     Y,X
      0085A6 90 FE            [ 2] 1259         LDW     Y,(Y)
      0085A8 90 53            [ 2] 1260         CPLW    Y
      0085AA 86               [ 1] 1261         POP     CC
      0085AB 25 02            [ 1] 1262         JRC     DN1
      0085AD 90 5C            [ 1] 1263         INCW    Y
      0085AF FF               [ 2] 1264 DN1:    LDW     (X),Y
      0085B0 81               [ 4] 1265         RET
                                   1266         .endif
                                   1267 
                                   1268 ;       =       ( w w -- t )    ( TOS STM8: -- Y,Z,N )
                                   1269 ;       Return true if top two are equal.
                                   1270 
      0085B1                       1271         HEADER  EQUAL "="
      0085B1                       1272 EQUAL:
                           000000  1273         .ifeq   SPEEDOVERSIZE
                                   1274         CALL    XORR
                                   1275         JP      ZEQUAL                 ; 31 cy= (18+13)
                           000001  1276         .else
      0085B1 90 5F            [ 1] 1277         CLRW Y                          ; (19 bytes, 17 cy)                
      0085B3 F6               [ 1] 1278         LD A,(X)                              
      0085B4 E0 02            [ 1] 1279         SUB A,(02,X)                         
      0085B6 26 08            [ 1] 1280         JRNE 1$                      
      0085B8 E6 01            [ 1] 1281         LD A,(01,X)                          
      0085BA E0 03            [ 1] 1282         SUB A,(03,X)                         
      0085BC 26 02            [ 1] 1283         JRNE 1$                      
      0085BE 90 53            [ 2] 1284         CPLW Y                                 
      0085C0 5C               [ 1] 1285 1$:     INCW X                                 
      0085C1 5C               [ 1] 1286         INCW X                                 
      0085C2 FF               [ 2] 1287         LDW (X),Y                              
      0085C3 81               [ 4] 1288         RET
                                   1289         .endif                          ; 17 cy, 19 bytes
                                   1290 
                                   1291 
                                   1292 ;       U<      ( u u -- t )    ( TOS STM8: -- Y,Z,N )
                                   1293 ;       Unsigned compare of top two items.
                                   1294 
      0085C4                       1295         HEADER  ULESS "U<"
      0085C4                       1296 ULESS:
      0085C4 4F               [ 1] 1297         CLR     A
      0085C5 AD 25            [ 4] 1298         CALLR   XREG0CMP
      0085C7 24 01            [ 1] 1299         JRUGE   1$
      0085C9 43               [ 1] 1300         CPL     A
      0085CA 90 97            [ 1] 1301 1$:     LD      YL,A
      0085CC 90 95            [ 1] 1302         LD      YH,A
      0085CE FF               [ 2] 1303         LDW     (X),Y
      0085CF 81               [ 4] 1304         RET
                                   1305 
                           000001  1306         .ifeq   BOOTSTRAP
                                   1307 ;       <       ( n1 n2 -- t )
                                   1308 ;       Signed compare of top two items.
                                   1309 
      0085D0                       1310         HEADER  LESS "<"
      0085D0                       1311 LESS:
                           000000  1312         .ifeq   SPEEDOVERSIZE
                                   1313         CALL    SUBB             ; (29cy)
                                   1314         JP      ZLESS            ; 41 cy (12+29)
                           000001  1315         .else
      0085D0 4F               [ 1] 1316         CLR     A
      0085D1 90 93            [ 1] 1317         LDW     Y,X
      0085D3 90 FE            [ 2] 1318         LDW     Y,(Y)
      0085D5 90 BF 0A         [ 2] 1319         LDW     XREG0,Y
      0085D8 5C               [ 1] 1320         INCW    X
      0085D9 5C               [ 1] 1321         INCW    X
      0085DA 90 93            [ 1] 1322         LDW     Y,X
      0085DC 90 FE            [ 2] 1323         LDW     Y,(Y)
      0085DE 90 B3 0A         [ 2] 1324         CPW     Y,XREG0
      0085E1 2E 01            [ 1] 1325         JRSGE   1$
      0085E3 43               [ 1] 1326         CPL     A
      0085E4 F7               [ 1] 1327 1$:     LD      (X),A
      0085E5 E7 01            [ 1] 1328         LD      (1,X),A
      0085E7 90 93            [ 1] 1329         LDW     Y,X
      0085E9 90 FE            [ 2] 1330         LDW     Y,(Y)
      0085EB 81               [ 4] 1331         RET                      ; 26 cy
                                   1332         .endif
                                   1333         .endif
                                   1334 
                                   1335 ;       XREG0CMP       ( n n - n )      ( TOS STM8: - Y,Z,N )
                                   1336 ;       Load (TOS) to XREG0 and (TOS-1) to Y, DROP, CMP to STM8 flags
      0085EC                       1337 XREG0CMP:
      0085EC 90 93            [ 1] 1338         LDW     Y,X
      0085EE 5C               [ 1] 1339         INCW    X
      0085EF 5C               [ 1] 1340         INCW    X
      0085F0 51               [ 1] 1341         EXGW    X,Y
      0085F1 FE               [ 2] 1342         LDW     X,(X)
      0085F2 BF 0A            [ 2] 1343         LDW     XREG0,X
      0085F4 93               [ 1] 1344         LDW     X,Y
      0085F5 FE               [ 2] 1345         LDW     X,(X)
      0085F6 B3 0A            [ 2] 1346         CPW     X,XREG0
      0085F8 51               [ 1] 1347         EXGW    X,Y
      0085F9 81               [ 4] 1348         RET
                                   1349 
                           000001  1350         .ifeq   BOOTSTRAP
                                   1351 ;       MAX     ( n n -- n )    ( TOS STM8: -- Y,Z,N )
                                   1352 ;       Return greater of two top items.
                                   1353 
      0085FA                       1354         HEADER  MAX "MAX"
      0085FA                       1355 MAX:
      0085FA AD F0            [ 4] 1356         CALLR   XREG0CMP
      0085FC 2C 04            [ 1] 1357         JRSGT   MMEXIT
      0085FE                       1358 XREG0TOS:
      0085FE 90 BE 0A         [ 2] 1359         LDW     Y,XREG0
      008601 FF               [ 2] 1360         LDW     (X),Y
      008602                       1361 MMEXIT:
      008602 81               [ 4] 1362         RET
                                   1363         .endif
                                   1364 
                           000001  1365         .ifeq   BOOTSTRAP
                                   1366 ;       MIN     ( n n -- n )    ( TOS STM8: -- Y,Z,N )
                                   1367 ;       Return smaller of top two items.
                                   1368 
      008603                       1369         HEADER  MIN "MIN"
      008603                       1370 MIN:
      008603 AD E7            [ 4] 1371         CALLR   XREG0CMP
      008605 2F FB            [ 1] 1372         JRSLT   MMEXIT
      008607 20 F5            [ 2] 1373         JRA     XREG0TOS
                                   1374         .endif
                                   1375 
                                   1376 ;       WITHIN ( u ul uh -- t ) ( TOS STM8: -- Y,Z,N )
                                   1377 ;       Return true if u is within
                                   1378 ;       range of ul and uh. ( ul <= u < uh )
                                   1379 
      008609                       1380         HEADER  WITHI "WITHIN"
      008609                       1381 WITHI:
      008609 CD 84 DC         [ 4] 1382         CALL    OVER
      00860C CD 85 43         [ 4] 1383         CALL    SUBB
      00860F E6 01            [ 1] 1384         LD A,(1,X)
      008611 88               [ 1] 1385         PUSH A
      008612 F6               [ 1] 1386         LD A,(X)
      008613 88               [ 1] 1387         PUSH A
      008614 5C               [ 1] 1388         INCW X
      008615 5C               [ 1] 1389         INCW X
      008616 CD 85 43         [ 4] 1390         CALL    SUBB
      008619 90 85            [ 2] 1391         POPW Y
      00861B 5A               [ 2] 1392         DECW X
      00861C 5A               [ 2] 1393         DECW X
      00861D FF               [ 2] 1394         LDW (X),Y
      00861E 20 A4            [ 2] 1395         JRA     ULESS
                                   1396 
                                   1397 ; Divide
                                   1398 
                                   1399 ;       UM/MOD  ( udl udh un -- ur uq )
                                   1400 ;       Unsigned divide of a double by a
                                   1401 ;       single. Return mod and quotient.
                                   1402 
      008620                       1403         HEADER  UMMOD "UM/MOD"
      008620                       1404 UMMOD:
      008620 90 93            [ 1] 1405         LDW     Y,X             ; stack pointer to Y
      008622 FE               [ 2] 1406         LDW     X,(X)           ; un
      008623 BF 0A            [ 2] 1407         LDW     XREG0,X         ; save un
      008625 93               [ 1] 1408         LDW     X,Y
      008626 5C               [ 1] 1409         INCW    X               ; drop un
      008627 5C               [ 1] 1410         INCW    X
      008628 89               [ 2] 1411         PUSHW   X               ; save stack pointer
      008629 FE               [ 2] 1412         LDW     X,(X)           ; X=udh
      00862A 90 EE 04         [ 2] 1413         LDW     Y,(4,Y)         ; Y=udl (offset before drop)
      00862D B3 0A            [ 2] 1414         CPW     X,XREG0
      00862F 25 09            [ 1] 1415         JRULT   MMSM1           ; X is still on the R-stack
      008631 85               [ 2] 1416         POPW    X               ; restore stack pointer
      008632 90 5F            [ 1] 1417         CLRW    Y
      008634 EF 02            [ 2] 1418         LDW     (2,X),Y         ; remainder 0
      008636 90 5A            [ 2] 1419         DECW    Y
      008638 FF               [ 2] 1420         LDW     (X),Y           ; quotient max. 16 bit value
      008639 81               [ 4] 1421         RET
      00863A                       1422 MMSM1:
      00863A A6 10            [ 1] 1423         LD      A,#16           ; loop count
      00863C 90 58            [ 2] 1424         SLLW    Y               ; udl shift udl into udh
      00863E                       1425 MMSM3:
      00863E 59               [ 2] 1426         RLCW    X               ; rotate udl bit into uhdh (= remainder)
      00863F 25 04            [ 1] 1427         JRC     MMSMa           ; if carry out of rotate
      008641 B3 0A            [ 2] 1428         CPW     X,XREG0         ; compare udh to un
      008643 25 05            [ 1] 1429         JRULT   MMSM4           ; can't subtract
      008645                       1430 MMSMa:
      008645 72 B0 00 0A      [ 2] 1431         SUBW    X,XREG0         ; can subtract
      008649 98               [ 1] 1432         RCF
      00864A                       1433 MMSM4:
      00864A 8C               [ 1] 1434         CCF                     ; quotient bit
      00864B 90 59            [ 2] 1435         RLCW    Y               ; rotate into quotient, rotate out udl
      00864D 4A               [ 1] 1436         DEC     A               ; repeat
      00864E 26 EE            [ 1] 1437         JRNE    MMSM3           ; if A == 0
      008650                       1438 MMSMb:
      008650 BF 0A            [ 2] 1439         LDW     XREG0,X         ; done, save remainder
      008652 85               [ 2] 1440         POPW    X               ; restore stack pointer
      008653 FF               [ 2] 1441         LDW     (X),Y           ; save quotient
      008654 90 BE 0A         [ 2] 1442         LDW     Y,XREG0         ; remainder onto stack
      008657 EF 02            [ 2] 1443         LDW     (2,X),Y
      008659 81               [ 4] 1444         RET
                                   1445 
                           000001  1446         .ifeq   UNLINKCORE
                                   1447 ;       M/MOD   ( d n -- r q )
                                   1448 ;       Signed floored divide of double by
                                   1449 ;       single. Return mod and quotient.
                                   1450 
      00865A                       1451         HEADER  MSMOD "M/MOD"
      00865A                       1452 MSMOD:
      00865A F6               [ 1] 1453         LD      A,(X)           ; DUPP ZLESS
      00865B 88               [ 1] 1454         PUSH    A               ; DUPP TOR
      00865C 2A 12            [ 1] 1455         JRPL    MMOD1           ; QBRAN
      00865E CD 87 42         [ 4] 1456         CALL    NEGAT
      008661 E6 01            [ 1] 1457         LD A,(1,X)
      008663 88               [ 1] 1458         PUSH A
      008664 F6               [ 1] 1459         LD A,(X)
      008665 88               [ 1] 1460         PUSH A
      008666 5C               [ 1] 1461         INCW X
      008667 5C               [ 1] 1462         INCW X
      008668 CD 85 9A         [ 4] 1463         CALL    DNEGA
      00866B 90 85            [ 2] 1464         POPW Y
      00866D 5A               [ 2] 1465         DECW X
      00866E 5A               [ 2] 1466         DECW X
      00866F FF               [ 2] 1467         LDW (X),Y
      008670                       1468 MMOD1:
      008670 E6 01            [ 1] 1469         LD A,(1,X)
      008672 88               [ 1] 1470         PUSH A
      008673 F6               [ 1] 1471         LD A,(X)
      008674 88               [ 1] 1472         PUSH A
      008675 5C               [ 1] 1473         INCW X
      008676 5C               [ 1] 1474         INCW X
      008677 90 93            [ 1] 1475         LDW     Y,X
      008679 90 FE            [ 2] 1476         LDW     Y,(Y)
      00867B 2A 0A            [ 1] 1477         JRPL    MMOD2           ; DUPP ZLESS QBRAN
      00867D 90 85            [ 2] 1478         POPW Y
      00867F 5A               [ 2] 1479         DECW X
      008680 5A               [ 2] 1480         DECW X
      008681 FF               [ 2] 1481         LDW (X),Y
      008682 90 89            [ 2] 1482         PUSHW Y
      008684 CD 85 0C         [ 4] 1483         CALL    PLUS
      008687                       1484 MMOD2:  
      008687 90 85            [ 2] 1485         POPW Y
      008689 5A               [ 2] 1486         DECW X
      00868A 5A               [ 2] 1487         DECW X
      00868B FF               [ 2] 1488         LDW (X),Y
      00868C AD 92            [ 4] 1489         CALLR   UMMOD
      00868E 84               [ 1] 1490         POP     A               ; RFROM
      00868F 4D               [ 1] 1491         TNZ     A
      008690 2A 09            [ 1] 1492         JRPL    MMOD3           ; QBRAN
      008692 CD 84 F7         [ 4] 1493         CALL    SWAPP
      008695 CD 87 42         [ 4] 1494         CALL    NEGAT
      008698 CC 84 F7         [ 2] 1495         JP      SWAPP
      00869B 81               [ 4] 1496 MMOD3:  RET
                                   1497 
                                   1498 ;       /MOD    ( n n -- r q )
                                   1499 ;       Signed divide. Return mod and quotient.
                                   1500 
      00869C                       1501         HEADER  SLMOD "/MOD"
      00869C                       1502 SLMOD:
      00869C CD 84 DC         [ 4] 1503         CALL    OVER
      00869F CD 85 3A         [ 4] 1504         CALL    ZLESS
      0086A2 CD 84 F7         [ 4] 1505         CALL    SWAPP
      0086A5 20 B3            [ 2] 1506         JRA     MSMOD
                                   1507 
                                   1508 ;       MOD     ( n n -- r )    ( TOS STM8: -- Y,Z,N )
                                   1509 ;       Signed divide. Return mod only.
                                   1510 
      0086A7                       1511         HEADER  MMOD "MOD"
      0086A7                       1512 MMOD:
      0086A7 AD F3            [ 4] 1513         CALLR   SLMOD
      0086A9 5C               [ 1] 1514         INCW    X
      0086AA 5C               [ 1] 1515         INCW    X
      0086AB 81               [ 4] 1516         RET
                                   1517 
                                   1518 ;       /       ( n n -- q )    ( TOS STM8: -- Y,Z,N )
                                   1519 ;       Signed divide. Return quotient only.
                                   1520 
      0086AC                       1521         HEADER  SLASH "/"
      0086AC                       1522 SLASH:
      0086AC AD EE            [ 4] 1523         CALLR   SLMOD
      0086AE CC 84 E5         [ 2] 1524         JP      NIP
                                   1525         .endif
                                   1526 
                                   1527 ; Multiply
                                   1528 
                                   1529 ;       UM*     ( u u -- ud )
                                   1530 ;       Unsigned multiply. Return double product.
                                   1531 
      0086B1                       1532         HEADER  UMSTA "UM*"
      0086B1                       1533 UMSTA:                          ; stack have 4 bytes u1=a,b u2=c,d
      0086B1 E6 02            [ 1] 1534         LD      A,(2,X)         ; b
      0086B3 90 97            [ 1] 1535         LD      YL,A
      0086B5 F6               [ 1] 1536         LD      A,(X)           ; d
      0086B6 90 42            [ 4] 1537         MUL     Y,A
      0086B8 90 89            [ 2] 1538         PUSHW   Y               ; PROD1 temp storage
      0086BA E6 03            [ 1] 1539         LD      A,(3,X)         ; a
      0086BC 90 97            [ 1] 1540         LD      YL,A
      0086BE F6               [ 1] 1541         LD      A,(X)           ; d
      0086BF 90 42            [ 4] 1542         MUL     Y,A
      0086C1 90 89            [ 2] 1543         PUSHW   Y               ; PROD2 temp storage
      0086C3 E6 02            [ 1] 1544         LD      A,(2,X)         ; b
      0086C5 90 97            [ 1] 1545         LD      YL,A
      0086C7 E6 01            [ 1] 1546         LD      A,(1,X)         ; c
      0086C9 90 42            [ 4] 1547         MUL     Y,A
      0086CB 90 89            [ 2] 1548         PUSHW   Y               ; PROD3,CARRY temp storage
      0086CD E6 03            [ 1] 1549         LD      A,(3,X)         ; a
      0086CF 90 97            [ 1] 1550         LD      YL,A
      0086D1 E6 01            [ 1] 1551         LD      A,(1,X)         ; c
      0086D3 90 42            [ 4] 1552         MUL     Y,A             ; least signifiant product
      0086D5 4F               [ 1] 1553         CLR     A
      0086D6 90 01            [ 1] 1554         RRWA    Y
      0086D8 E7 03            [ 1] 1555         LD      (3,X),A         ; store least significant byte
      0086DA 72 F9 01         [ 2] 1556         ADDW    Y,(1,SP)        ; PROD3
      0086DD 4F               [ 1] 1557         CLR     A
      0086DE 49               [ 1] 1558         RLC     A               ; save carry
      0086DF 6B 01            [ 1] 1559         LD      (1,SP),A        ; CARRY
      0086E1 72 F9 03         [ 2] 1560         ADDW    Y,(3,SP)        ; PROD2
      0086E4 7B 01            [ 1] 1561         LD      A,(1,SP)        ; CARRY
      0086E6 A9 00            [ 1] 1562         ADC     A,#0            ; add 2nd carry
      0086E8 6B 01            [ 1] 1563         LD      (1,SP),A        ; CARRY
      0086EA 4F               [ 1] 1564         CLR     A
      0086EB 90 01            [ 1] 1565         RRWA    Y
      0086ED E7 02            [ 1] 1566         LD      (2,X),A         ; 2nd product byte
      0086EF 72 F9 05         [ 2] 1567         ADDW    Y,(5,SP)        ; PROD1
      0086F2 90 01            [ 1] 1568         RRWA    Y
      0086F4 E7 01            [ 1] 1569         LD      (1,X),A         ; 3rd product byte
      0086F6 90 01            [ 1] 1570         RRWA    Y               ; 4th product byte now in A
      0086F8 19 01            [ 1] 1571         ADC     A,(1,SP)        ; CARRY
      0086FA F7               [ 1] 1572         LD      (X),A
      0086FB 5B 06            [ 2] 1573         ADDW    SP,#6           ; drop temp storage
      0086FD 81               [ 4] 1574         RET
                                   1575 
                                   1576 ;       *       ( n n -- n )    ( TOS STM8: -- Y,Z,N )
                                   1577 ;       Signed multiply. Return single product.
                                   1578 
      0086FE                       1579         HEADER  STAR "*"
      0086FE                       1580 STAR:
      0086FE AD B1            [ 4] 1581         CALLR   UMSTA
      008700 5C               [ 1] 1582         INCW    X
      008701 5C               [ 1] 1583         INCW    X
      008702 81               [ 4] 1584         RET
                           000001  1585         .ifeq   UNLINKCORE
                                   1586 ;       M*      ( n n -- d )
                                   1587 ;       Signed multiply. Return double product.
      008703                       1588         HEADER  MSTAR "M*"
      008703                       1589 MSTAR:
      008703 E6 02            [ 1] 1590         LD      A,(2,X)         ; DDUP
      008705 F8               [ 1] 1591         XOR     A,(X)           ; XORR
      008706 88               [ 1] 1592         PUSH    A               ; TOR
      008707 CD 87 4A         [ 4] 1593         CALL    ABSS
      00870A CD 84 F7         [ 4] 1594         CALL    SWAPP
      00870D CD 87 4A         [ 4] 1595         CALL    ABSS
      008710 AD 9F            [ 4] 1596         CALLR   UMSTA
      008712 84               [ 1] 1597         POP     A               ; RFROM
      008713 4D               [ 1] 1598         TNZ     A
      008714 2A 03            [ 1] 1599         JRPL    MSTA1           ; QBRAN
      008716 CC 85 9A         [ 2] 1600         JP      DNEGA
      008719 81               [ 4] 1601 MSTA1:  RET
                                   1602 
                                   1603 ;       */MOD   ( n1 n2 n3 -- r q )
                                   1604 ;       Multiply n1 and n2, then divide
                                   1605 ;       by n3. Return mod and quotient.
      00871A                       1606         HEADER  SSMOD "*/MOD"
      00871A                       1607 SSMOD:
      00871A E6 01            [ 1] 1608         LD A,(1,X)
      00871C 88               [ 1] 1609         PUSH A
      00871D F6               [ 1] 1610         LD A,(X)
      00871E 88               [ 1] 1611         PUSH A
      00871F 5C               [ 1] 1612         INCW X
      008720 5C               [ 1] 1613         INCW X
      008721 AD E0            [ 4] 1614         CALLR   MSTAR
      008723 90 85            [ 2] 1615         POPW Y
      008725 5A               [ 2] 1616         DECW X
      008726 5A               [ 2] 1617         DECW X
      008727 FF               [ 2] 1618         LDW (X),Y
      008728 CC 86 5A         [ 2] 1619         JP      MSMOD
                                   1620 
                                   1621 ;       */      ( n1 n2 n3 -- q )    ( TOS STM8: -- Y,Z,N )
                                   1622 ;       Multiply n1 by n2, then divide
                                   1623 ;       by n3. Return quotient only.
      00872B                       1624         HEADER  STASL "*/"
      00872B                       1625 STASL:
      00872B AD ED            [ 4] 1626         CALLR   SSMOD
      00872D CC 84 E5         [ 2] 1627         JP      NIP
                                   1628         .endif
                                   1629 
                                   1630 ; Miscellaneous
                                   1631 
                                   1632 
                           000001  1633         .ifeq   BAREBONES
                                   1634 ;       EXG      ( n -- n )      ( TOS STM8: -- Y,Z,N )
                                   1635 ;       Exchange high with low byte of n.
                                   1636 
      008730                       1637         HEADER  EXG "EXG"
      008730                       1638 EXG:
      008730                       1639         LDW_Y_CONTENT_X
      008730 90 93            [ 1]    1         LDW Y,X
      008732 90 FE            [ 2]    2         LDW Y,(Y)		; tos in Y
      008734 90 5E            [ 1] 1640         SWAPW   Y
      008736 FF               [ 2] 1641         LDW (X),Y
      008737 81               [ 4] 1642         RET
                                   1643         .endif
                                   1644 
                                   1645 ;        .ifeq   BOOTSTRAP
                                   1646 
                                   1647 ;       2+      ( a -- a )      ( TOS STM8: -- Y,Z,N )
                                   1648 ;       Add 2 to tos.
                                   1649 
      008738                       1650         HEADER  CELLP "2+"
      008738                       1651 CELLP:
      008738                       1652         LDW_Y_CONTENT_X
      008738 90 93            [ 1]    1         LDW Y,X
      00873A 90 FE            [ 2]    2         LDW Y,(Y)		; tos in Y
      00873C 90 5C            [ 1] 1653         INCW    Y
      00873E 90 5C            [ 1] 1654         INCW    Y
      008740 FF               [ 2] 1655         LDW (X),Y
      008741 81               [ 4] 1656         RET
                                   1657 
                                   1658 ;       NEGATE  ( n -- -n )     ( TOS STM8: -- Y,Z,N )
                                   1659 ;       Two's complement of TOS.
                                   1660 
      008742                       1661         HEADER  NEGAT "NEGATE"
      008742                       1662 NEGAT:
      008742                       1663         LDW_Y_CONTENT_X
      008742 90 93            [ 1]    1         LDW Y,X
      008744 90 FE            [ 2]    2         LDW Y,(Y)		; tos in Y
      008746 90 50            [ 2] 1664         NEGW    Y
      008748 FF               [ 2] 1665         LDW (X),Y
      008749 81               [ 4] 1666         RET
                                   1667 
                                   1668 ;       ABS     ( n -- n )      ( TOS STM8: -- Y,Z,N )
                                   1669 ;       Return  absolute value of n.
                                   1670 
      00874A                       1671         HEADER  ABSS "ABS"
      00874A                       1672 ABSS:
      00874A                       1673         LDW_Y_CONTENT_X
      00874A 90 93            [ 1]    1         LDW Y,X
      00874C 90 FE            [ 2]    2         LDW Y,(Y)		; tos in Y
      00874E 2A 03            [ 1] 1674         JRPL    1$              ; positive?
      008750 90 50            [ 2] 1675         NEGW    Y               ; else negate
      008752 FF               [ 2] 1676         LDW (X),Y
      008753 81               [ 4] 1677 1$:     RET
                                   1678 
                                   1679 ;       0=      ( n -- t )      ( TOS STM8: -- Y,Z,N ))
                                   1680 ;       Return true if n is equal to 0
                                   1681 
      008754                       1682         HEADER  ZEQUAL "0="
      008754                       1683 ZEQUAL:
      008754                       1684         LDW_Y_CONTENT_X
      008754 90 93            [ 1]    1         LDW Y,X
      008756 90 FE            [ 2]    2         LDW Y,(Y)		; tos in Y
      008758 27 04            [ 1] 1685         JREQ    CPLW
      00875A 90 5F            [ 1] 1686 CLRW:   CLRW    Y
      00875C FF               [ 2] 1687         LDW (X),Y
      00875D 81               [ 4] 1688         RET
      00875E 90 53            [ 2] 1689 CPLW:   CPLW    Y               ; else -1
      008760 FF               [ 2] 1690         LDW (X),Y
      008761 81               [ 4] 1691         RET
                                   1692 
                                   1693 ;       !0=      ( n -- t )      ( TOS STM8: -- Y,Z,N ))
                                   1694 ;       Return true if n is not equal to 0
                                   1695 
      008762                       1696         HEADER  ZEQUAL "!0="
      008762                       1697 NZEQUAL:
      008762                       1698         LDW_Y_CONTENT_X
      008762 90 93            [ 1]    1         LDW Y,X
      008764 90 FE            [ 2]    2         LDW Y,(Y)		; tos in Y
      008766 26 02            [ 1] 1699         JRNE    1$
      008768 20 F0            [ 2] 1700         JRA     CLRW
      00876A 90 5F            [ 1] 1701 1$:     CLRW    Y
      00876C 20 F0            [ 2] 1702         JRA     CPLW
                                   1703 
                                   1704 ;       PICK    ( ... +n -- ... w )      ( TOS STM8: -- Y,Z,N )
                                   1705 ;       Copy    nth stack item to tos.
                                   1706 
      00876E                       1707         HEADER  PICK "PICK"
      00876E                       1708 PICK:
      00876E                       1709         LDW_Y_CONTENT_X
      00876E 90 93            [ 1]    1         LDW Y,X
      008770 90 FE            [ 2]    2         LDW Y,(Y)		; tos in Y
      008772 90 5C            [ 1] 1710         INCW    Y
      008774 90 58            [ 2] 1711         SLAW    Y
      008776 BF 0A            [ 2] 1712         LDW     XREG0,X
      008778 72 B9 00 0A      [ 2] 1713         ADDW    Y,XREG0
      00877C 90 FE            [ 2] 1714         LDW     Y,(Y)
      00877E FF               [ 2] 1715         LDW (X),Y
      00877F 81               [ 4] 1716         RET
                                   1717 
                                   1718 ;       DEPTH   ( -- n )      ( TOS STM8: -- Y,Z,N )
                                   1719 ;       Return  depth of data stack.
                                   1720 
      008780                       1721         HEADER  DEPTH "DEPTH"
      008780                       1722 DEPTH:
      008780 90 93            [ 1] 1723         LDW     Y,X
      008782 50               [ 2] 1724         NEGW    X
      008783 1C 03 A0         [ 2] 1725         ADDW    X,#SPP
      008786 57               [ 2] 1726         SRAW    X
      008787 51               [ 1] 1727 XSTOR:  EXGW    X,Y
      008788 5A               [ 2] 1728         DECW    X               ; SUBW  X,#2
      008789 5A               [ 2] 1729         DECW    X
      00878A FF               [ 2] 1730         LDW     (X),Y           ; push on stack
      00878B 81               [ 4] 1731         RET                     ; go to RET of EXEC
                                   1732 
                                   1733 ; Memory access
                                   1734 
                                   1735 ;       +!      ( n a -- )      ( TOS STM8: -- Y,Z,N )
                                   1736 ;       Add n to contents at address a.
                                   1737 
      00878C                       1738         HEADER  PSTOR "+!"
      00878C                       1739 PSTOR:
      00878C BF 0A            [ 2] 1740         LDW XREG0,X
      00878E EE 02            [ 2] 1741         LDW X,(2,X)
      008790 89               [ 2] 1742         PUSHW X
      008791 BE 0A            [ 2] 1743         LDW X,XREG0
      008793 FE               [ 2] 1744         LDW X,(X)        ; addr
      008794 90 93            [ 1] 1745         LDW Y,X
      008796 FE               [ 2] 1746         LDW X,(X)
      008797 72 FB 01         [ 2] 1747         ADDW X,(1,SP)
      00879A 90 FF            [ 2] 1748         LDW (Y),X
      00879C 85               [ 2] 1749         POPW X
      00879D BE 0A            [ 2] 1750 ENDPP:	LDW X,XREG0
      00879F                       1751         HEADER  DDROP "2DROP"
      00879F                       1752 DDROP:
      00879F 1C 00 04         [ 2] 1753         ADDW    X,#4
      0087A2 81               [ 4] 1754         RET
                                   1755 
                                   1756 ;       +C!      ( n a -- )      ( TOS STM8: -- Y,Z,N )
                                   1757 ;       Add n to contents at address a.
                                   1758 
      0087A3                       1759         HEADER  PCSTOR "+C!"
      0087A3                       1760 PCSTOR:
      0087A3 BF 0A            [ 2] 1761         LDW XREG0,X
      0087A5 FE               [ 2] 1762         LDW X,(X)        ; addr
      0087A6 90 93            [ 1] 1763         LDW Y,X
      0087A8 F6               [ 1] 1764         LD A,(X)
      0087A9 BE 0A            [ 2] 1765         LDW X,XREG0
      0087AB EB 03            [ 1] 1766         ADD A,(3,X)
      0087AD 90 F7            [ 1] 1767         LD (Y),A
      0087AF 20 EC            [ 2] 1768         JRA ENDPP
                                   1769 
                                   1770 ;       @EXECUTE        ( a -- )  ( TOS STM8: undefined )
                                   1771 ;       Execute vector stored in address a.
                                   1772 
      0087B1                       1773         HEADER  ATEXE "@EXECUTE"
      0087B1                       1774 ATEXE:
      0087B1 90 93            [ 1] 1775         LDW     Y,X
      0087B3 5C               [ 1] 1776         INCW    X
      0087B4 5C               [ 1] 1777         INCW    X
      0087B5 90 FE            [ 2] 1778         LDW     Y,(Y)
      0087B7 90 FE            [ 2] 1779         LDW     Y,(Y)
      0087B9 27 02            [ 1] 1780         JREQ    1$
      0087BB 90 FC            [ 2] 1781         JP      (Y)
      0087BD 81               [ 4] 1782 1$:     RET
                                   1783 
                                   1784 ;       21+     ( u u -- u+1 u+1)
                                   1785  
      0087BE                       1786         HEADER  TWOONEPLUS "21+"
      0087BE                       1787 TWOONEPLUS:
      0087BE 90 93            [ 1] 1788         LDW Y,X 
      0087C0 FE               [ 2] 1789         LDW X,(X) 
      0087C1 5C               [ 1] 1790         INCW X  
      0087C2 90 FF            [ 2] 1791         LDW (Y),X 
      0087C4 93               [ 1] 1792         LDW X,Y  
      0087C5 EE 02            [ 2] 1793         LDW X,(2,X) 
      0087C7 5C               [ 1] 1794         INCW X 
      0087C8 90 EF 02         [ 2] 1795         LDW (2,Y),X 
      0087CB 93               [ 1] 1796         LDW X,Y
      0087CC 81               [ 4] 1797         RET
                                   1798 
                                   1799 ;       CMOVE   ( b1 b2 u -- )
                                   1800 ;       Copy u bytes from b1 to b2.
      0087CD                       1801         HEADER  CMOVE "CMOVE"
      0087CD                       1802 CMOVE:
      0087CD E6 01            [ 1] 1803         LD A,(1,X)
      0087CF 88               [ 1] 1804         PUSH A
      0087D0 5C               [ 1] 1805         INCW X
      0087D1 5C               [ 1] 1806         INCW X
      0087D2 90 93            [ 1] 1807 CMOVE1: LDW Y,X
      0087D4 EE 02            [ 2] 1808         LDW X,(2,X)
      0087D6 F6               [ 1] 1809         LD A,(X)
      0087D7 93               [ 1] 1810         LDW X,Y
      0087D8 FE               [ 2] 1811         LDW X,(X)
      0087D9 F7               [ 1] 1812         LD (X),A
      0087DA 93               [ 1] 1813         LDW X,Y
      0087DB AD E1            [ 4] 1814         CALLR TWOONEPLUS
      0087DD 84               [ 1] 1815         POP A
      0087DE 4A               [ 1] 1816         DEC A
      0087DF 88               [ 1] 1817         PUSH A
      0087E0 26 F0            [ 1] 1818         JRNE CMOVE1
      0087E2 84               [ 1] 1819         POP A
      0087E3 1C 00 04         [ 2] 1820         ADDW X,#4
      0087E6 81               [ 4] 1821         RET
                                   1822 
                                   1823 
                                   1824 ; Basic I/O
                                   1825 
                                   1826 ;       KEY     ( -- c )
                                   1827 ;       Wait for and return an
                                   1828 ;       input character.
                                   1829 
      0087E7                       1830         HEADER  KEY "KEY"
      0087E7                       1831 KEY:
      0087E7 92 CD 02         [ 6] 1832 KEY1:   CALL    [USRQKEY]
      0087EA 90 93            [ 1] 1833         LDW     Y,X
      0087EC 5C               [ 1] 1834         INCW    X
      0087ED 5C               [ 1] 1835         INCW    X
      0087EE 90 FE            [ 2] 1836         LDW     Y,(Y)
      0087F0 26 02            [ 1] 1837         JRNE    RETIDLE
      0087F2 20 F3            [ 2] 1838         JRA     KEY1
      0087F4                       1839 RETIDLE:
      0087F4 81               [ 4] 1840         RET
                                   1841 
                                   1842 ;       QUIT    ( -- )
                                   1843 ;       Reset return stack pointer
                                   1844 ;       and start text interpreter.
                                   1845 
      0087F5                       1846         HEADER  QUIT "QUIT"
      0087F5                       1847 QUIT:
      0087F5 90 AE 03 FF      [ 2] 1848         LDW     Y,#RPP          ; initialize return stack
      0087F9 90 94            [ 1] 1849         LDW     SP,Y
      0087FB CC 83 CE         [ 2] 1850         JP      SLEEP
                                   1851 
                                   1852 
                                   1853 ;       SP!     ( a -- )
                                   1854 ;       Set data stack pointer.
                                   1855 
      0087FE                       1856         HEADER  SPSTO "sp!"
      0087FE                       1857 SPSTO:
      0087FE FE               [ 2] 1858         LDW     X,(X)   ;X = a
      0087FF 81               [ 4] 1859         RET
                                   1860 
                                   1861 ;       SP@     ( -- a )        ( TOS STM8: -- Y,Z,N )
                                   1862 ;       Push current stack pointer.
                                   1863 
      008800                       1864         HEADER  SPAT "sp@"
      008800                       1865 SPAT:
      008800 90 93            [ 1] 1866         LDW     Y,X
      008802 5A               [ 2] 1867         DECW    X               ; SUBW  X,#2
      008803 5A               [ 2] 1868         DECW    X
      008804 FF               [ 2] 1869         LDW     (X),Y           ; push on stack
      008805 81               [ 4] 1870         RET                     ; go to RET of EXEC
                                   1871 
                                   1872 ;       RP@     ( -- a )     ( TOS STM8: -- Y,Z,N )
                                   1873 ;       Push current RP to data stack.
                                   1874 
      008806                       1875         HEADER  RPAT "rp@"
      008806                       1876 RPAT:
      008806 90 96            [ 1] 1877         LDW     Y,SP            ; save return addr
      008808 5A               [ 2] 1878         DECW    X               ; SUBW  X,#2
      008809 5A               [ 2] 1879         DECW    X
      00880A FF               [ 2] 1880         LDW     (X),Y           ; push on stack
      00880B 81               [ 4] 1881         RET                     ; go to RET of EXEC
                                   1882 
                                   1883 ;       RP!     ( a -- )
                                   1884 ;       Set return stack pointer.
                                   1885 
      00880C                       1886         HEADFLG RPSTO "rp!" COMPO
                                      1 
      00880C                       1887 RPSTO:
      00880C 90 85            [ 2] 1888         POPW    Y
      00880E 90 BF 0A         [ 2] 1889         LDW     XREG0,Y
      008811 90 93            [ 1] 1890         LDW     Y,X
      008813 5C               [ 1] 1891         INCW    X
      008814 5C               [ 1] 1892         INCW    X
      008815 90 FE            [ 2] 1893         LDW     Y,(Y)
      008817 90 94            [ 1] 1894         LDW     SP,Y
      008819 92 CC 0A         [ 5] 1895         JP      [XREG0]
                                   1896 
                                   1897 ;===============================================================
      00881C                       1898         HEADFLG DO "DO" COMPO
                                      1 
      00881C                       1899 DO:
      00881C 90 93            [ 1] 1900         LDW Y,X
      00881E 85               [ 2] 1901         POPW X
      00881F 89               [ 2] 1902         PUSHW X
      008820 89               [ 2] 1903         PUSHW X
      008821 89               [ 2] 1904         PUSHW X
      008822 93               [ 1] 1905         LDW X,Y
      008823 FE               [ 2] 1906         LDW X,(X)
      008824 1F 03            [ 2] 1907         LDW (3,SP),X
      008826 93               [ 1] 1908         LDW X,Y
      008827 EE 02            [ 2] 1909         LDW X,(2,X)
      008829 1F 05            [ 2] 1910         LDW (5,SP),X
      00882B 93               [ 1] 1911         LDW X,Y
      00882C 1C 00 04         [ 2] 1912         ADDW X,#4
      00882F 81               [ 4] 1913         RET
                                   1914 
                                   1915 ;===============================================================
                           000001  1916         .ifne   WORDS_EXTRAEEPR
                                   1917 ;       ULOCK  ( -- )
                                   1918 ;       Unlock EEPROM (STM8S)
                                   1919 
      008830                       1920         HEADER  ULOCK "ULOCK"
      008830                       1921 ULOCK:
      008830 35 AE 50 64      [ 1] 1922         MOV     FLASH_DUKR,#0xAE
      008834 35 56 50 64      [ 1] 1923         MOV     FLASH_DUKR,#0x56
      008838 72 07 50 5F FB   [ 2] 1924 1$:     BTJF    FLASH_IAPSR,#3,1$    ; PM0051 4.1 requires polling bit3=1 before writing
      00883D 81               [ 4] 1925         RET
                                   1926 
                                   1927 
                                   1928 ;       LOCK  ( -- )
                                   1929 ;       Lock EEPROM (STM8S)
                                   1930 
      00883E                       1931         HEADER  LOCK "LOCK"
      00883E                       1932 LOCK:
      00883E 72 17 50 5F      [ 1] 1933         BRES    FLASH_IAPSR,#3
      008842 81               [ 4] 1934         RET
                                   1935         .endif
                                   1936 
                                   1937 ;       ULOCKF  ( -- )
                                   1938 ;       Unlock Flash (STM8S)
                                   1939 
      008843                       1940         HEADER  UNLOCK_FLASH "ULOCKF"
      008843                       1941 UNLOCK_FLASH:
      008843 35 56 50 62      [ 1] 1942         MOV     FLASH_PUKR,#0x56
      008847 35 AE 50 62      [ 1] 1943         MOV     FLASH_PUKR,#0xAE
      00884B 72 03 50 5F FB   [ 2] 1944 1$:     BTJF    FLASH_IAPSR,#1,1$    ; PM0051 4.1 requires polling bit1=1 before writing
      008850 81               [ 4] 1945         RET
                                   1946 
                                   1947 ;       LOCKF  ( -- )
                                   1948 ;       Lock Flash (STM8S)
                                   1949 
      008851                       1950         HEADER  LOCK_FLASH "LOCKF"
      008851                       1951 LOCK_FLASH:
      008851 72 13 50 5F      [ 1] 1952         BRES    FLASH_IAPSR,#1
      008855 81               [ 4] 1953         RET
                                   1954 
                                   1955 ;       SAVEC ( -- )
                                   1956 ;       Minimal context switch for low level interrupt code
                                   1957 ;       This should be the first word called in the interrupt handler
                                   1958 
      008856                       1959         HEADER  SAVEC "SAVEC"
      008856                       1960 SAVEC:
      008856 90 85            [ 2] 1961         POPW    Y
      008858 BE 0A            [ 2] 1962         LDW     X,XREG0
      00885A 89               [ 2] 1963         PUSHW   X
      00885B AE 03 B0         [ 2] 1964         LDW     X,#ISPP         ; "PC_ISPP" const. top of int. data stack
      00885E 90 FC            [ 2] 1965         JP      (Y)
                                   1966 
                                   1967 ;       IRET ( -- )
                                   1968 ;       Restore context and return from low level interrupt code
                                   1969 ;       This should be the last word called in the interrupt handler
                                   1970 
      008860                       1971         HEADER  RESTC "IRET"
      008860                       1972 RESTC:
      008860 85               [ 2] 1973         POPW    X
      008861 BF 0A            [ 2] 1974         LDW     XREG0,X         ; restore context
      008863 80               [11] 1975         IRET                    ; resturn from interrupt
                                   1976 
                                   1977 ;===============================================================
                                   1978 
                           000000  1979         LASTN   =       LINK    ;last name defined
                           008864  1980         END_SDCC_FLASH = .
                           00002C  1981         USERRAM = RAMPOOL
                                   1982 
                                   1983         .area CODE
                                   1984         .area INITIALIZER
                                   1985         .area CABS (ABS)
