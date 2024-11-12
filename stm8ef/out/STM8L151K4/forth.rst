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
      008002 81 85                   65          .dw _forth
                                     66 ;--------------------------------------------------------
                                     67 ; restore broken interrupt vector table of STM8L
                                     68 ;--------------------------------------------------------
                           000001    69          .ifeq (FAMILY - STM8L)
      008070                         70           .org 0x8070
      008070 82 00                   71           .dw 0x8200
      008072 81 85                   72           .dw _forth
      008074 82 00                   73           .dw 0x8200
      008076 81 85                   74           .dw _forth
      008078 82 00                   75           .dw 0x8200
      00807A 81 85                   76           .dw _forth
      00807C 82 00                   77           .dw 0x8200
      00807E 81 85                   78           .dw _forth
                           000001    79          .if   HAS_RXUART*HAS_TXUART
      008078                         80           .org 0x8008   + ITC_IX_USART1_RXD * 4
      008078 82 00                   81           .dw 0x8200
      00807A 81 E1                   82           .dw UART_INT
                                     83          .endif
                                     84          .endif
                                     85 ;--------------------------------------------------------
                                     86 ; restore broken interrupt vector table of STM8S
                                     87 ;--------------------------------------------------------
                           000000    88          .ifeq (FAMILY - STM8S)
                                     89           .org 0x8068
                                     90           .dw 0x8200
                                     91           .dw 0x0
                                     92          .if   HAS_RXUART*HAS_TXUART
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
      008080                        117         .org CODE_START
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
                                      1 ;       STM8L152K4 device and memory layout configuration
                                      2 
                           000098     3         TARGET = STM8L151K4
                                      4 
                           0007FF     5         RAMEND =        0x07FF  ; "RAMEND" system (return) stack, growing down
                           001000     6         EEPROMBASE =    0x1000  ; "EESTART" EEPROM start address
                           0013FF     7         EEPROMEND =     0x13FF  ; "EEEND" 1024 bytes EEPROM
                           008080     8 	CODE_START =	0x8080	; End of interrupt vector area
                           00FFFF     9         FLASHEND =      0xFFFF  ; 32K devices
                           000740    10         FLASHBUF_ADDR = 0x0740  ; flash buffer address for muforth flash routine
                           000000    11         FORTHRAM =      0x0000  ; Start of RAM controlled by Forth
                           000000    12         UPPLOC  =       0x0000  ; UPP (user/system area) location for 2K RAM
                           0007D0    13         SPPLOC  =       0x07D0  ; SPP (data stack top), TIB start
                           0007FF    14         RPPLOC  =       RAMEND  ; RPP (return stack top)
                                    167 
                                    168         ; STM8 Flash Block Size (depends on "TARGET")
                           000000   169         .ifeq   (TARGET - STM8S_LOD) * (TARGET - STM8L_101) * (TARGET - STM8L_LOD)
                                    170           PAGESIZE   =     0x40      ; "PAGESIZE" STM8 Low Density: 64 byte page size
                           000001   171         .else
                           000080   172           PAGESIZE   =     0x80      ; "PAGESIZE" STM8 M/H Density: 128 byte page size
                                    173         .endif
                                    174 
                                    175         ; STM8 family register addresses (depends on "TARGET")
                           000000   176         .ifeq   (TARGET - STM8S_LOD) * (TARGET - STM8S_MED) * (TARGET - STM8S_HID)
                                    177           FAMILY = STM8S
                                    178           .include  "stm8device.inc"
                                    179         .endif
                           000001   180         .ifeq   (TARGET - STM8L_101) * (TARGET - STM8L_LOD) * (TARGET - STM8L_MHD)
                           000001   181           FAMILY = STM8L
                                    182           .include  "stm8ldevice.inc"
                                      1 ; STM8L register addresses
                                      2 
                                      3 ; ***** 6.2.1 I/O port hardware register map
                                      4 
                           005000     5         PORTA        = PA_ODR
                           005005     6         PORTB        = PB_ODR
                           00500A     7         PORTC        = PC_ODR
                           00500F     8         PORTD        = PD_ODR
                           005014     9         PORTE        = PE_ODR
                           005019    10         PORTF        = PF_ODR
                           00501E    11         PORTG        = PG_ODR
                           005023    12         PORTH        = PH_ODR
                           005028    13         PORTI        = PI_ODR
                           000000    14         PORTX        = 0
                                     15 
                           000000    16         ODR          = 0
                           000001    17         IDR          = 1
                           000002    18         DDR          = 2
                           000003    19         CR1          = 3
                           000004    20         CR2          = 4
                                     21 
                                     22 ; Port A
                           005000    23  PA_ODR = 0x5000 ; Port A data output latch register 0x00
                           005001    24  PA_IDR = 0x5001 ; Port A in put pin value register 0xXX
                           005002    25  PA_DDR = 0x5002 ; Port A data direction register 0x00
                           005003    26  PA_CR1 = 0x5003 ; Port A control register 1 0x01
                           005004    27  PA_CR2 = 0x5004 ; Port A control register 2 0x00
                                     28 ; Port B
                           005005    29  PB_ODR = 0x5005 ; Port B data output latch register 0x00
                           005006    30  PB_IDR = 0x5006 ; Port B input pin value register 0xXX
                           005007    31  PB_DDR = 0x5007 ; Port B data direction register 0x00
                           005008    32  PB_CR1 = 0x5008 ; Port B control register 1 0x00
                           005009    33  PB_CR2 = 0x5009 ; Port B control register 2 0x00
                                     34 ; Port C
                           00500A    35  PC_ODR = 0x500A ; Port C data output latch register 0x00
                           00500B    36  PC_IDR = 0x500B ; Port C input pin value register 0xXX
                           00500C    37  PC_DDR = 0x500C ; Port C data direction register 0x00
                           00500D    38  PC_CR1 = 0x500D ; Port C control register 1 0x00
                           00500E    39  PC_CR2 = 0x500E ; Port C control register 2 0x00
                                     40 ; Port D
                           00500F    41  PD_ODR = 0x500F ; Port D data output latch register 0x00
                           005010    42  PD_IDR = 0x5010 ; Port D input pin value register 0xXX
                           005011    43  PD_DDR = 0x5011 ; Port D data direction register 0x00
                           005012    44  PD_CR1 = 0x5012 ; Port D control register 1 0x00
                           005013    45  PD_CR2 = 0x5013 ; Port D control register 2 0x00
                                     46 ; Port E
                           005014    47  PE_ODR = 0x5014 ; Port E data output latch register 0x00
                           005015    48  PE_IDR = 0x5015 ; Port E input pin value register 0xXX
                           005016    49  PE_DDR = 0x5016 ; Port E data direction register 0x00
                           005017    50  PE_CR1 = 0x5017 ; Port E control register 1 0x00
                           005018    51  PE_CR2 = 0x5018 ; Port E control register 2 0x00
                                     52 ; Port F
                           005019    53  PF_ODR = 0x5019 ; Port F data output latch register 0x00
                           00501A    54  PF_IDR = 0x501A ; Port F input pin value register 0xXX
                           00501B    55  PF_DDR = 0x501B ; Port F data direction register 0x00
                           00501C    56  PF_CR1 = 0x501C ; Port F control register 1 0x00
                           00501D    57  PF_CR2 = 0x501D ; Port F control register 2 0x00
                                     58 ; Port G
                           00501E    59  PG_ODR = 0x501E ; Port G data output latch register 0x00
                           00501F    60  PG_IDR = 0x501F ; Port G input pin value register 0xXX
                           005020    61  PG_DDR = 0x5020 ; Port G data direction register 0x00
                           005021    62  PG_CR1 = 0x5021 ; Port G control register 1 0x00
                           005022    63  PG_CR2 = 0x5022 ; Port G control register 2 0x00
                                     64 ; Port H
                           005023    65  PH_ODR = 0x5023 ; Port H data output latch register 0x00
                           005024    66  PH_IDR = 0x5024 ; Port H input pin value register 0xXX
                           005025    67  PH_DDR = 0x5025 ; Port H data direction register 0x00
                           005026    68  PH_CR1 = 0x5026 ; Port H control register 1 0x00
                           005027    69  PH_CR2 = 0x5027 ; Port H control register 2 0x00
                                     70 ; Port I
                           005028    71  PI_ODR = 0x5028 ; Port I data output latch register 0x00
                           005029    72  PI_IDR = 0x5029 ; Port I input pin value register 0xXX
                           00502A    73  PI_DDR = 0x502A ; Port I data direction register 0x00
                           00502B    74  PI_CR1 = 0x502B ; Port I control register 1 0x00
                           00502C    75  PI_CR2 = 0x502C ; Port I control register 2 0x00
                                     76 ; 0x00 5014 to 0x00 501D Reserved area (0 bytes)
                                     77 ; 0x00 502E to 0x00 5049 Reserved area (44 bytes)
                                     78 ; Flash
                           005050    79  FLASH_CR1 = 0x5050 ; Flash control register 1 0x00
                           005051    80  FLASH_CR2 = 0x5051 ; Flash control register 2 0x00
                           005052    81  FLASH_PUKR = 0x5052 ; Flash program memory unprotection key register 0x00
                           005053    82  FLASH_DUKR = 0x5053 ; Data EEPROM unprotection key register 0x00
                           005054    83  FLASH_IAPSR = 0x5054 ; Flash in-application programming status register 0x00
                                     84 ; 0x00 5055 to 0x00 506F Reserved area (27 bytes)
                                     85 ; DMA1
                           005070    86  DMA1_GCSR = 0x5070 ; DMA1 global configuration & status register 0xFC
                           005071    87  DMA1_GIR1 = 0x5071 ; DMA1 global interrupt register 1 0x00
                                     88 ; 0x00 5072 to 0x00 5074 Reserved area (3 bytes)
                           005075    89  DMA1_C0CR = 0x5075 ; DMA1 channel 0 configuration register 0x00
                           005076    90  DMA1_C0SPR = 0x5076 ; DMA1 channel 0 status & priority register 0x00
                           005077    91  DMA1_C0NDTR = 0x5077 ; DMA1 number of data to transfer register (channel 0) 0x00
                           005078    92  DMA1_C0PARH = 0x5078 ; DMA1 peripheral address high register (channel 0) 0x52
                           005079    93  DMA1_C0PARL = 0x5079 ; DMA1 peripheral address low register (channel 0) 0x00
                                     94 ; 0x00 507A Reserved area (1 byte)
                           00507B    95  DMA1_C0M0ARH = 0x507B ; DMA1 memory 0 address high register (channel 0) 0x00
                           00507C    96  DMA1_C0M0ARL = 0x507C ; DMA1 memory 0 address low register (channel 0) 0x00
                                     97 ; 0x00 507D to 0x00 507E Reserved area (2 bytes)
                           00507F    98  DMA1_C1CR = 0x507F ; DMA1 channel 1 configuration register 0x00
                           005080    99  DMA1_C1SPR = 0x5080 ; DMA1 channel 1 status & priority register 0x00
                           005081   100  DMA1_C1NDTR = 0x5081 ; DMA1 number of data to transfer register (channel 1) 0x00
                           005082   101  DMA1_C1PARH = 0x5082 ; DMA1 peripheral address high register (channel 1) 0x52
                           005083   102  DMA1_C1PARL = 0x5083 ; DMA1 peripheral address low register (channel 1) 0x00
                                    103 ; 0x00 5084 Reserved area (1 byte)
                           005085   104  DMA1_C1M0ARH = 0x5085 ; DMA1 memory 0 address high register (channel 1) 0x00
                           005086   105  DMA1_C1M0ARL = 0x5086 ; DMA1 memory 0 address low register (channel 1) 0x00
                                    106 ; 0x00 5087 0x00 5088 Reserved area (2 bytes)
                           005089   107  DMA1_C2CR = 0x5089 ; DMA1 channel 2 configuration register 0x00
                           00508A   108  DMA1_C2SPR = 0x508A ; DMA1 channel 2 status & priority register 0x00
                           00508B   109  DMA1_C2NDTR = 0x508B ; DMA1 number of data to transfer register (channel 2) 0x00
                           00508C   110  DMA1_C2PARH = 0x508C ; DMA1 peripheral address high register (channel 2) 0x52
                           00508D   111  DMA1_C2PARL = 0x508D ; DMA1 peripheral address low register (channel 2) 0x00
                                    112 ; 0x00 508E Reserved area (1 byte)
                           00508F   113  DMA1_C2M0ARH = 0x508F ; DMA1 memory 0 address high register (channel 2) 0x00
                           005090   114  DMA1_C2M0ARL = 0x5090 ; DMA1 memory 0 address low register (channel 2) 0x00
                                    115 ; 0x00 5091 0x00 5092 Reserved area (2 bytes)
                           005093   116  DMA1_C3CR = 0x5093 ; DMA1 channel 3 configuration register 0x00
                           005094   117  DMA1_C3SPR = 0x5094 ; DMA1 channel 3 status & priority register 0x00
                           005095   118  DMA1_C3NDTR = 0x5095 ; DMA1 number of data to transfer register (channel 3) 0x00
                           005096   119  DMA1_C3PARH_C3M1ARH = 0x5096 ; DMA1 peripheral address high register (channel 3) 0x40
                           005097   120  DMA1_C3PARL_C3M1ARL = 0x5097 ; DMA1 peripheral address low register (channel 3) 0x00
                                    121 ; DMA1
                           005098   122  DMA_C3M0EAR = 0x5098 ; DMA channel 3 memory 0 extended address register 0x00
                           005099   123  DMA1_C3M0ARH = 0x5099 ; DMA1 memory 0 address high register (channel 3) 0x00
                           00509A   124  DMA1_C3M0ARL = 0x509A ; DMA1 memory 0 address low register (channel 3) 0x00
                                    125 ; 0x00 509B to 0x00 509C Reserved area (3 bytes)
                                    126 ; SYSCFG
                           00509D   127  SYSCFG_RMPCR3 = 0x509D ; Remapping register 3 0x00
                           00509E   128  SYSCFG_RMPCR1 = 0x509E ; Remapping register 1 0x00
                           00509F   129  SYSCFG_RMPCR2 = 0x509F ; Remapping register 2 0x00
                                    130 ; ITC - EXTI
                           0050A0   131  EXTI_CR1 = 0x50A0 ; External interrupt control register 1 0x00
                           0050A1   132  EXTI_CR2 = 0x50A1 ; External interrupt control register 2 0x00
                           0050A2   133  EXTI_CR3 = 0x50A2 ; External interrupt control register 3 0x00
                           0050A3   134  EXTI_SR1 = 0x50A3 ; External interrupt status register 1 0x00
                           0050A4   135  EXTI_SR2 = 0x50A4 ; External interrupt status register 2 0x00
                           0050A5   136  EXTI_CONF1 = 0x50A5 ; External interrupt port select register 1 0x00
                                    137 ; WFE
                           0050A6   138  WFE_CR1 = 0x50A6 ; WFE control register 1 0x00
                           0050A7   139  WFE_CR2 = 0x50A7 ; WFE control register 2 0x00
                           0050A8   140  WFE_CR3 = 0x50A8 ; WFE control register 3 0x00
                           0050A9   141  WFE_CR4 = 0x50A9 ; WFE control register 4 0x00
                                    142 ; ITC - EXTI
                           0050AA   143  EXTI_CR4 = 0x50AA ; External interrupt control register 4 0x00
                           0050AB   144  EXTI_CONF2 = 0x50AB ; External interrupt port select register 2 0x00
                                    145 ; 0x00 50A9 to 0x00 50AF Reserved area (7 bytes)
                                    146 ; RST
                           0050B0   147  RST_CR = 0x50B0 ; Reset control register 0x00
                           0050B1   148  RST_SR = 0x50B1 ; Reset status register 0x01
                                    149 ; PWR
                           0050B2   150  PWR_CSR1 = 0x50B2 ; Power control and status register 1 0x00
                           0050B3   151  PWR_CSR2 = 0x50B3 ; Power contro l and status register 2 0x00
                                    152 ; 0x00 50B4 to 0x00 50BF Reserved area (12 bytes)
                                    153 ; CLK
                           0050C0   154  CLK_CKDIVR = 0x50C0 ; CLK Clock master divider register 0x03
                           0050C1   155  CLK_CRTCR = 0x50C1 ; CLK Clock RTC register 0x00 (1)
                           0050C2   156  CLK_ICKCR = 0x50C2 ; CLK Internal clock control register  0x11
                           0050C3   157  CLK_PCKENR1 = 0x50C3 ; CLK Peripheral clock gating register 1 0x00
                                    158 ; CLK
                           0050C4   159  CLK_PCKENR2 = 0x50C4 ; CLK Peripheral clock gating register 2 0x00
                           0050C5   160  CLK_CCOR = 0x50C5 ; CLK Configurable clock control register 0x00
                           0050C6   161  CLK_ECKCR = 0x50C6 ; CLK External clock control register 0x00
                           0050C7   162  CLK_SCSR = 0x50C7 ; CLK System clock status register 0x01
                           0050C8   163  CLK_SWR = 0x50C8 ; CLK System clock switch register  0x01
                           0050C9   164  CLK_SWCR = 0x50C9 ; CLK Clock switch control register  0xX0
                           0050CA   165  CLK_CSSR = 0x50CA ; CLK Clock security system register 0x00
                           0050CB   166  CLK_CBEEPR = 0x50CB ; CLK Clock BEEP register 0x00
                           0050CC   167  CLK_HSICALR = 0x50CC ; CLK HSI calibration register 0xXX
                           0050CD   168  CLK_HSITRIMR = 0x50CD ; CLK HSI clock calibration trimming register  0x00
                           0050CE   169  CLK_HSIUNLCKR = 0x50CE ; CLK HSI unlock register  0x00
                           0050CF   170  CLK_REGCSR = 0x50CF ; CLK Main regulator control status register 0bxx11100X
                           0050D0   171  CLK_PCKENR3 = 0x50D0 ; CLK Peripheral clock gating register 3 0x00
                                    172 ; 0x00 50D1 to 0x00 50D2 Reserved area (2 bytes)
                                    173 ; WWDG
                           0050D3   174  WWDG_CR = 0x50D3 ; WWDG control register 0x7F
                           0050D4   175  WWDG_WR = 0x50D4 ; WWDR window register 0x7F
                                    176 ; 0x00 50D5 to 00 50DF Reserved area (11 bytes)
                                    177 ; IWDG
                           0050E0   178  IWDG_KR = 0x50E0 ; IWDG key register 0x01
                           0050E1   179  IWDG_PR = 0x50E1 ; IWDG prescaler register 0x00
                           0050E2   180  IWDG_RLR = 0x50E2 ; IWDG reload register 0xFF
                                    181 ; 0x00 50E3 to 0x00 50EF Reserved area (13 bytes)
                                    182 ; BEEP
                           0050F0   183  BEEP_CSR1 = 0x50F0 ; BEEP control/status register 1 0x00
                                    184 ; 0x00 50F1 0x00 50F2 Reserved area (2 bytes)
                           0050F3   185  BEEP_CSR2 = 0x50F3 ; BEEP control/status register 2 0x1F
                                    186 ; 0x00 50F4 to 0x00 513F Reserved area (76 bytes)
                                    187 ; RTC
                           005140   188  RTC_TR1 = 0x5140 ; RTC Time register 1 0x00
                           005141   189  RTC_TR2 = 0x5141 ; RTC Time register 2 0x00
                           005142   190  RTC_TR3 = 0x5142 ; RTC Time register 3 0x00
                                    191 ; 0x00 5143 Reserved area (1 byte)
                                    192 ; RTC
                           005144   193  RTC_DR1 = 0x5144 ; RTC Date register 1 0x01
                           005145   194  RTC_DR2 = 0x5145 ; RTC Date register 2 0x21
                           005146   195  RTC_DR3 = 0x5146 ; RTC Date register 3 0x00
                                    196 ; 0x00 5147 Reserved area (1 byte)
                           005148   197  RTC_CR1 = 0x5148 ; RTC Control register 1 0x00 (1)
                           005149   198  RTC_CR2 = 0x5149 ; RTC Control register 2 0x00 (1)
                           00514A   199  RTC_CR3 = 0x514A ; RTC Control register 3 0x00 (1)
                                    200 ; 0x00 514B Reserved area (1 byte)
                           00514C   201  RTC_ISR1 = 0x514C ; RTC Initialization and status register 1 0x01
                           00514D   202  RTC_ISR2 = 0x514D ; RTC Initialization and Status register 2 0x00
                                    203 ; 0x00 514E 0x00 514F Reserved area (2 bytes)
                           005150   204  RTC_SPRERH = 0x5150 ; RTC Synchronous prescaler register high 0x00 (1)
                           005151   205  RTC_SPRERL = 0x5151 ; RTC Synchronous prescaler register low 0xFF (1)
                           005152   206  RTC_APRER = 0x5152 ; RTC Asynchronous prescaler register 0x7F (1)
                                    207 ; 0x00 5153 Reserved area (1 byte)
                           005154   208  RTC_WUTRH = 0x5154 ; RTC Wakeup timer register high 0xFF (1)
                           005155   209  RTC_WUTRL = 0x5155 ; RTC Wakeup timer register low 0xFF (1)
                                    210 ; 0x00 5156 Reserved area (1 byte)
                           005157   211  RTC_SSRL = 0x5157 ; RTC Subsecond register low 0x00
                           005158   212  RTC_SSRH = 0x5158 ; RTC Subsecond register high 0x00
                           005159   213  RTC_WPR = 0x5159 ; RTC Write protection register 0x00
                           005158   214  RTC_SSRH = 0x5158 ; RTC Subsecond register high 0x00
                           005159   215  RTC_WPR = 0x5159 ; RTC Write protection register 0x00
                           00515A   216  RTC_SHIFTRH = 0x515A ; RTC Shift register high 0x00
                           00515B   217  RTC_SHIFTRL = 0x515B ; RTC Shift register low 0x00
                           00515C   218  RTC_ALRMAR1 = 0x515C ; RTC Alarm A register 1 0x00 (1)
                           00515D   219  RTC_ALRMAR2 = 0x515D ; RTC Alarm A register 2 0x00 (1)
                           00515E   220  RTC_ALRMAR3 = 0x515E ; RTC Alarm A register 3 0x00 (1)
                           00515F   221  RTC_ALRMAR4 = 0x515F ; RTC Alarm A register 4 0x00 (1)
                                    222 ; 0x00 5160 to 0x00 5163 Reserved area (4 bytes)
                           005164   223  RTC_ALRMASSRH = 0x5164 ; RTC Alarm A subsecond register high  0x00 (1)
                           005165   224  RTC_ALRMASSRL = 0x5165 ; RTC Alarm A subsecond register low 0x00 (1)
                                    225 ; RTC
                           005166   226  RTC_ALRMASSMSKR = 0x5166 ; RTC Alarm A masking register  0x00 (1)
                                    227 ; 0x00 5167 to 0x00 5169 Reserved area (3 bytes)
                           00516A   228  RTC_CALRH = 0x516A ; RTC Calibration register high 0x00 (1)
                           00516B   229  RTC_CALRL = 0x516B ; RTC Calibration register low 0x00 (1)
                           00516C   230  RTC_TCR1 = 0x516C ; RTC Tamper control register 1 0x00 (1)
                           00516D   231  RTC_TCR2 = 0x516D ; RTC Tamper control register 2 0x00 (1)
                                    232 ; 0x00 516E to 0x00 518A Reserved area (36 bytes)
                           005190   233  CSSLSE_CSR = 0x5190 ; CSS on LSE control and status register 0x00 (1)
                                    234 ; 0x00 519A to 0x00 51FF Reserved area (111 bytes)
                                    235 ; SPI1
                           005200   236  SPI1_CR1 = 0x5200 ; SPI1 control register 1 0x00
                           005201   237  SPI1_CR2 = 0x5201 ; SPI1 control register 2 0x00
                           005202   238  SPI1_ICR = 0x5202 ; SPI1 interrupt control register 0x00
                           005203   239  SPI1_SR = 0x5203 ; SPI1 status register 0x02
                           005204   240  SPI1_DR = 0x5204 ; SPI1 data register 0x00
                           005205   241  SPI1_CRCPR = 0x5205 ; SPI1 CRC polynomial register 0x07
                           005206   242  SPI1_RXCRCR = 0x5206 ; SPI1 Rx CRC register 0x00
                           005207   243  SPI1_TXCRCR = 0x5207 ; SPI1 Tx CRC register 0x00
                                    244 ; 0x00 5208 to 0x00 520F Reserved area (8 bytes)
                                    245 ; I2C1
                           005210   246  I2C1_CR1 = 0x5210 ; I2C1 control register 1 0x00
                           005211   247  I2C1_CR2 = 0x5211 ; I2C1 control register 2 0x00
                           005212   248  I2C1_FREQR = 0x5212 ; I2C1 frequency register  0x00
                           005213   249  I2C1_OARL = 0x5213 ; I2C1 own address register low 0x00
                           005214   250  I2C1_OARH = 0x5214 ; I2C1 own address register high 0x00
                           005215   251  I2C1_OAR2 = 0x5215 ; I2C1 own address register for dual mode 0x00
                           005216   252  I2C1_DR = 0x5216 ; I2C1 data register 0x00
                           005217   253  I2C1_SR1 = 0x5217 ; I2C1 status register 1 0x00
                           005218   254  I2C1_SR2 = 0x5218 ; I2C1 status register 2 0x00
                           005219   255  I2C1_SR3 = 0x5219 ; I2C1 status register 3 0x0X
                           00521A   256  I2C1_ITR = 0x521A ; I2C1 interrupt control register 0x00
                           00521B   257  I2C1_CCRL = 0x521B ; I2C1 clock control register low 0x00
                           00521C   258  I2C1_CCRH = 0x521C ; I2C1 clock control register high 0x00
                                    259 ; I2C1
                           00521D   260  I2C1_TRISER = 0x521D ; I2C1 TRISE register 0x02
                           00521E   261  I2C1_PECR = 0x521E ; I2C1 packet error checking register 0x00
                                    262 ; 0x00 521F to 0x00 522F Reserved area (17 bytes)
                                    263 ; USART1
                           005230   264  USART1_SR = 0x5230 ; USART1 status register 0xC0
                           005231   265  USART1_DR = 0x5231 ; USART1 data register 0xXX
                           005232   266  USART1_BRR1 = 0x5232 ; USART1 baud rate register 1 0x00
                           005233   267  USART1_BRR2 = 0x5233 ; USART1 baud rate register 2 0x00
                           005234   268  USART1_CR1 = 0x5234 ; USART1 control register 1 0x00
                           005235   269  USART1_CR2 = 0x5235 ; USART1 control register 2 0x00
                           005236   270  USART1_CR3 = 0x5236 ; USART1 control register 3 0x00
                           005237   271  USART1_CR4 = 0x5237 ; USART1 control register 4 0x00
                           005238   272  USART1_CR5 = 0x5238 ; USART1 control register 5 0x00
                           005239   273  USART1_GTR = 0x5239 ; USART1 guard time register  0x00
                           00523A   274  USART1_PSCR = 0x523A ; USART1 prescaler register  0x00
                                    275 
                                    276         ; 0x00 523B to 0x00 524F Reserved area (21 bytes)
                                    277 ; TIM2
                           005250   278  TIM2_CR1 = 0x5250 ; TIM2 control register 1 0x00
                           005251   279  TIM2_CR2 = 0x5251 ; TIM2 control register 2 0x00
                           005252   280  TIM2_SMCR = 0x5252 ; TIM2 Slave mode control register  0x00
                           005253   281  TIM2_ETR = 0x5253 ; TIM2 external trigger register 0x00
                           000000   282         .ifeq   (TARGET - STM8L_101)
                                    283         ; STM8L101 family devices have an offset shift here
                                    284  TIM2_IER = 0x5254 ; TIM2 interrupt enable register  0x00
                                    285  TIM2_SR1 = 0x5255 ; TIM2 status register 1 0x00
                                    286  TIM2_SR2 = 0x5256 ; TIM2 status register 2 0x00
                                    287  TIM2_EGR = 0x5257 ; TIM2 event generation register  0x00
                                    288  TIM2_CCMR1 = 0x5258 ; TIM2 capture/compare mode register 1 0x00
                                    289  TIM2_CCMR2 = 0x5259 ; TIM2 capture/compare mode register 2 0x00
                                    290  TIM2_CCER1 = 0x525A ; TIM2 capture/compare enable register 1 0x00
                                    291  TIM2_CNTRH = 0x525B ; TIM2 counter high 0x00
                                    292  TIM2_CNTRL = 0x525C ; TIM2 counter low 0x00
                                    293  TIM2_PSCR = 0x525D ; TIM2 prescaler register 0x00
                                    294  TIM2_ARRH = 0x525E ; TIM2 auto-reload register high 0xFF
                                    295  TIM2_ARRL = 0x525F ; TIM2 auto-reload register low 0xFF
                                    296  TIM2_CCR1H = 0x5260 ; TIM2 capture/compare register 1 high 0x00
                                    297  TIM2_CCR1L = 0x5261 ; TIM2 capture/compare register 1 low 0x00
                                    298  TIM2_CCR2H = 0x5262 ; TIM2 capture/compare register 2 high 0x00
                                    299  TIM2_CCR2L = 0x5263 ; TIM2 capture/compare register 2 low 0x00
                                    300  TIM2_BKR = 0x5264 ; TIM2 break register 0x00
                                    301  TIM2_OISR = 0x5265 ; TIM2 output idle state register 0x00
                           000001   302         .else
                           005254   303  TIM2_DER = 0x5254 ; TIM2 DMA1 request enable register 0x00
                           005255   304  TIM2_IER = 0x5255 ; TIM2 interrupt enable register  0x00
                           005256   305  TIM2_SR1 = 0x5256 ; TIM2 status register 1 0x00
                           005257   306  TIM2_SR2 = 0x5257 ; TIM2 status register 2 0x00
                           005258   307  TIM2_EGR = 0x5258 ; TIM2 event generation register  0x00
                           005259   308  TIM2_CCMR1 = 0x5259 ; TIM2 capture/compare mode register 1 0x00
                           00525A   309  TIM2_CCMR2 = 0x525A ; TIM2 capture/compare mode register 2 0x00
                           00525B   310  TIM2_CCER1 = 0x525B ; TIM2 capture/compare enable register 1 0x00
                           00525C   311  TIM2_CNTRH = 0x525C ; TIM2 counter high 0x00
                           00525D   312  TIM2_CNTRL = 0x525D ; TIM2 counter low 0x00
                           00525E   313  TIM2_PSCR = 0x525E ; TIM2 prescaler register 0x00
                           00525F   314  TIM2_ARRH = 0x525F ; TIM2 auto-reload register high 0xFF
                           005260   315  TIM2_ARRL = 0x5260 ; TIM2 auto-reload register low 0xFF
                           005261   316  TIM2_CCR1H = 0x5261 ; TIM2 capture/compare register 1 high 0x00
                           005262   317  TIM2_CCR1L = 0x5262 ; TIM2 capture/compare register 1 low 0x00
                           005263   318  TIM2_CCR2H = 0x5263 ; TIM2 capture/compare register 2 high 0x00
                           005264   319  TIM2_CCR2L = 0x5264 ; TIM2 capture/compare register 2 low 0x00
                           005265   320  TIM2_BKR = 0x5265 ; TIM2 break register 0x00
                           005266   321  TIM2_OISR = 0x5266 ; TIM2 output idle state register 0x00
                                    322         .endif
                                    323 
                                    324 ; 0x00 5267 to 0x00 527F Reserved area (25 bytes)
                                    325 
                                    326 ; TIM3
                           005280   327  TIM3_CR1 = 0x5280 ; TIM3 control register 1 0x00
                           005281   328  TIM3_CR2 = 0x5281 ; TIM3 control register 2 0x00
                           005282   329  TIM3_SMCR = 0x5282 ; TIM3 Slave mode control register  0x00
                           005283   330  TIM3_ETR = 0x5283 ; TIM3 external trigger register 0x00
                           000000   331         .ifeq   (TARGET - STM8L_101)
                                    332         ; STM8L101 family devices have an offset shift here
                                    333  TIM3_IER = 0x5284 ; TIM3 interrupt enable register  0x00
                                    334  TIM3_SR1 = 0x5285 ; TIM3 status register 1 0x00
                                    335  TIM3_SR2 = 0x5286 ; TIM3 status register 2 0x00
                                    336  TIM3_EGR = 0x5287 ; TIM3 event generation register  0x00
                                    337  TIM3_CCMR1 = 0x5288 ; TIM3 Capture/Compare mode register 1 0x00
                                    338  TIM3_CCMR2 = 0x5289 ; TIM3 Capture/Compare mode register 2 0x00
                                    339  TIM3_CCER1 = 0x528A ; TIM3 Capture/Compare enable register 1 0x00
                                    340  TIM3_CNTRH = 0x528B ; TIM3 counter high 0x00
                                    341  TIM3_CNTRL = 0x528C ; TIM3 counter low 0x00
                                    342  TIM3_PSCR = 0x528D ; TIM3 prescaler register 0x00
                                    343  TIM3_ARRH = 0x528E ; TIM3 Auto-reload register high 0xFF
                                    344  TIM3_ARRL = 0x528F ; TIM3 Auto-reload register low 0xFF
                                    345  TIM3_CCR1H = 0x5290 ; TIM3 Capture/Compare register 1 high 0x00
                                    346  TIM3_CCR1L = 0x5291 ; TIM3 Capture/Compare register 1 low 0x00
                                    347  TIM3_CCR2H = 0x5292 ; TIM3 Capture/Compare register 2 high 0x00
                                    348  TIM3_CCR2L = 0x5293 ; TIM3 Capture/Compare register 2 low 0x00
                                    349  TIM3_BKR = 0x5294 ; TIM3 break register 0x00
                                    350  TIM3_OISR = 0x5295 ; TIM3 output idle state register 0x00
                           000001   351         .else
                           005284   352  TIM3_DER = 0x5284 ; TIM3 DMA1 request enable register 0x00
                           005285   353  TIM3_IER = 0x5285 ; TIM3 interrupt enable register  0x00
                           005286   354  TIM3_SR1 = 0x5286 ; TIM3 status register 1 0x00
                           005287   355  TIM3_SR2 = 0x5287 ; TIM3 status register 2 0x00
                           005288   356  TIM3_EGR = 0x5288 ; TIM3 event generation register  0x00
                           005289   357  TIM3_CCMR1 = 0x5289 ; TIM3 Capture/Compare mode register 1 0x00
                           00528A   358  TIM3_CCMR2 = 0x528A ; TIM3 Capture/Compare mode register 2 0x00
                           00528B   359  TIM3_CCER1 = 0x528B ; TIM3 Capture/Compare enable register 1 0x00
                           00528C   360  TIM3_CNTRH = 0x528C ; TIM3 counter high 0x00
                           00528D   361  TIM3_CNTRL = 0x528D ; TIM3 counter low 0x00
                           00528E   362  TIM3_PSCR = 0x528E ; TIM3 prescaler register 0x00
                           00528F   363  TIM3_ARRH = 0x528F ; TIM3 Auto-reload register high 0xFF
                           005290   364  TIM3_ARRL = 0x5290 ; TIM3 Auto-reload register low 0xFF
                           005291   365  TIM3_CCR1H = 0x5291 ; TIM3 Capture/Compare register 1 high 0x00
                           005292   366  TIM3_CCR1L = 0x5292 ; TIM3 Capture/Compare register 1 low 0x00
                           005293   367  TIM3_CCR2H = 0x5293 ; TIM3 Capture/Compare register 2 high 0x00
                           005294   368  TIM3_CCR2L = 0x5294 ; TIM3 Capture/Compare register 2 low 0x00
                           005295   369  TIM3_BKR = 0x5295 ; TIM3 break register 0x00
                           005296   370  TIM3_OISR = 0x5296 ; TIM3 output idle state register 0x00
                                    371         .endif
                                    372 
                           0052B0   373         TIM1_CR1 = 0x52B0
                           0052B5   374         TIM1_IER = 0x52B5
                           0052B6   375         TIM1_SR1 = 0x52B6
                           0052C2   376         TIM1_PSCRL = 0x52C2
                           0052C3   377         TIM1_ARRH = 0x52C3
                           0052C4   378         TIM1_ARRL = 0x52C4
                                    379 
                           000000   380         .ifne   BG_USE_TIM1
                                    381         .ifne   (BG_USE_TIM3 + STM8L_LOD + STM8L_101)
                                    382         Error: either BG_USE_TIM1 or BG_USE_TIM3 can be selected but not both
                                    383         Error: STM8L Low density device doesn't have TIM1
                                    384         .else
                                    385         BG_TIM_CR1   = TIM1_CR1
                                    386         BG_TIM_IER   = TIM1_IER
                                    387         BG_TIM_SR1   = TIM1_SR1
                                    388         BG_TIM_ARRH  = TIM1_ARRH
                                    389         BG_TIM_ARRL  = TIM1_ARRL
                                    390         .endif
                           000001   391         .else
                           000000   392         .ifne   BG_USE_TIM3
                                    393         BG_TIM_CR1   = TIM3_CR1
                                    394         BG_TIM_IER   = TIM3_IER
                                    395         BG_TIM_SR1   = TIM3_SR1
                                    396         BG_TIM_ARRH  = TIM3_ARRH
                                    397         BG_TIM_ARRL  = TIM3_ARRL
                                    398         BG_TIM_PSCR  = TIM3_PSCR
                           000001   399         .else
                           005250   400         BG_TIM_CR1   = TIM2_CR1
                           005255   401         BG_TIM_IER   = TIM2_IER
                           005256   402         BG_TIM_SR1   = TIM2_SR1
                           00525F   403         BG_TIM_ARRH  = TIM2_ARRH
                           005260   404         BG_TIM_ARRL  = TIM2_ARRL
                           00525E   405         BG_TIM_PSCR  = TIM2_PSCR
                                    406         .endif
                                    407         .endif
                                    408 
                                    409 ; 0x00 5297 to 0x00 52DF Reserved area (72 bytes)
                                    410 
                                    411 ; TIM4
                           0052E0   412  TIM4_CR1 = 0x52E0 ; TIM4 control register 1 0x00
                           0052E1   413  TIM4_CR2 = 0x52E1 ; TIM4 control register 2 0x00
                           0052E2   414  TIM4_SMCR = 0x52E2 ; TIM4 Slave mode control register  0x00
                           000000   415         .ifeq   (TARGET - STM8L_101)
                                    416         ; STM8L101 family devices have an offset shift here
                                    417  TIM4_IER = 0x52E3 ; TIM4 Interrupt enable register  0x00
                                    418  TIM4_SR  = 0x52E4 ; TIM4 status register 1 0x00
                                    419  TIM4_SR1 = TIM4_SR ; alias
                                    420  TIM4_EGR = 0x52E5 ; TIM4 Event generation register  0x00
                                    421  TIM4_CNTR = 0x52E6 ; TIM4 counter 0x00
                                    422  TIM4_PSCR = 0x52E7 ; TIM4 prescaler register  0x00
                                    423  TIM4_ARR = 0x52E8 ; TIM4 Auto-reload register 0x00
                           000001   424         .else
                           0052E3   425   TIM4_DER = 0x52E3 ; TIM4 DMA1 request enable register 0x00
                           0052E4   426  TIM4_IER = 0x52E4 ; TIM4 Interrupt enable register  0x00
                           0052E5   427  TIM4_SR  = 0x52E5 ; TIM4 status register 1 0x00
                           0052E5   428  TIM4_SR1 = TIM4_SR ; STM8L051F3 data sheet error
                           0052E6   429  TIM4_EGR = 0x52E6 ; TIM4 Event generation register  0x00
                           0052E7   430  TIM4_CNTR = 0x52E7 ; TIM4 counter 0x00
                           0052E8   431  TIM4_PSCR = 0x52E8 ; TIM4 prescaler register  0x00
                           0052E9   432  TIM4_ARR = 0x52E9 ; TIM4 Auto-reload register 0x00
                                    433         .endif
                                    434 
                                    435 
                                    436 ; 0x00 52EA to 0x00 52FE Reserved area (21 bytes)
                                    437 ; IRTIM
                           0052FF   438  IR_CR = 0x52FF ; Infrared control register 0x00
                                    439 ; 0x00 5317 to 0x00 533F Reserved area (41 bytes)
                                    440 ; ADC1
                           005340   441  ADC1_CR1 = 0x5340 ; ADC1 configuration register 1 0x00
                           005341   442  ADC1_CR2 = 0x5341 ; ADC1 configuration register 2 0x00
                           005342   443  ADC1_CR3 = 0x5342 ; ADC1 configuration register 3 0x1F
                           005343   444  ADC1_SR = 0x5343 ; ADC1 status register 0x00
                           005344   445  ADC1_DRH = 0x5344 ; ADC1 data register high 0x00
                           005345   446  ADC1_DRL = 0x5345 ; ADC1 data register low 0x00
                           005346   447  ADC1_HTRH = 0x5346 ; ADC1 high threshold register high 0x0F
                           005347   448  ADC1_HTRL = 0x5347 ; ADC1 high threshold register low 0xFF
                           005348   449  ADC1_LTRH = 0x5348 ; ADC1 low threshold register high 0x00
                           005349   450  ADC1_LTRL = 0x5349 ; ADC1 low threshold register low 0x00
                           00534A   451  ADC1_SQR1 = 0x534A ; ADC1 channel sequence 1 register 0x00
                           00534B   452  ADC1_SQR2 = 0x534B ; ADC1 channel sequence 2 register 0x00
                           00534C   453  ADC1_SQR3 = 0x534C ; ADC1 channel sequence 3 register 0x00
                           00534D   454  ADC1_SQR4 = 0x534D ; ADC1 channel sequence 4 register 0x00
                           00534E   455  ADC1_TRIGR1 = 0x534E ; ADC1 trigger disable 1 0x00
                           00534F   456  ADC1_TRIGR2 = 0x534F ; ADC1 trigger disable 2 0x00
                           005350   457  ADC1_TRIGR3 = 0x5350 ; ADC1 trigger disable 3 0x00
                           005351   458  ADC1_TRIGR4 = 0x5351 ; ADC1 trigger disable 4 0x00
                                    459 ; 0x00 53C8 to 0x00 53FF Reserved area
                                    460 
                                    461 ; USART2
                           0053E0   462   USART2_SR   = 0x53E0      ; USART2 status register0xC0
                           0053E1   463   USART2_DR   = 0x53E1      ; USART2 data register0xXX
                           0053E2   464   USART2_BRR1 = 0x53E2      ; USART2 baud rate register 1
                           0053E5   465   USART2_CR2  = 0x53E5      ; USART2 control register 2
                           0053E8   466   USART2_CR5  = 0x53E8      ; USART2 control register 5
                                    467 
                                    468 ; USART3
                           0053F0   469   USART3_SR   = 0x53F0      ; USART3 status register0xC0
                           0053F1   470   USART3_DR   = 0x53F1      ; USART3 data register0xXX
                           0053F2   471   USART3_BRR1 = 0x53F2      ; USART3 baud rate register 1
                           0053F5   472   USART3_CR2  = 0x53F5      ; USART3 control register 2
                           0053F8   473   USART3_CR5  = 0x53F8      ; USART3 control register 5
                                    474 
                           000000   475       .ifeq   (TARGET - STM8L_MHD) + (USE_UART3 - 1)
                                    476         UART_SR   = USART3_SR
                                    477         UART_DR   = USART3_DR
                                    478         UART_BRR1 = USART3_BRR1
                                    479         UART_CR2  = USART3_CR2
                                    480         UART_CR5  = USART3_CR5
                           000001   481       .else
                           000000   482         .ifeq   (TARGET - STM8L_MHD) + (USE_UART2 - 1)
                                    483         UART_SR   = USART2_SR
                                    484         UART_DR   = USART2_DR
                                    485         UART_BRR1 = USART2_BRR1
                                    486         UART_CR2  = USART2_CR2
                                    487         UART_CR5  = USART2_CR5
                           000001   488         .else
                           005230   489         UART_SR   = USART1_SR
                           005231   490         UART_DR   = USART1_DR
                           005232   491         UART_BRR1 = USART1_BRR1
                           005235   492         UART_CR2  = USART1_CR2
                           005238   493         UART_CR5  = USART1_CR5
                                    494         .endif
                                    495       .endif
                                    496 ;; LCD
                           005400   497  LCD_CR1 = 0x5400 ; LCD control register 1 0x00
                           005401   498  LCD_CR2 = 0x5401 ; LCD control register 2 0x00
                           005402   499  LCD_CR3 = 0x5402 ; LCD control register 3 0x00
                           005403   500  LCD_FRQ = 0x5403 ; LCD frequency selection register 0x00
                           005404   501  LCD_PM0 = 0x5404 ; LCD Port mask register 0 0x00
                           005405   502  LCD_PM1 = 0x5405 ; LCD Port mask register 1 0x00
                           005406   503  LCD_PM2 = 0x5406 ; LCD Port mask register 2 0x00
                           005407   504  LCD_PM3 = 0x5407 ; LCD Port mask register 3 0x00
                           005408   505  LCD_PM4 = 0x5408 ; LCD Port mask register 4 0x00
                           005409   506  LCD_PM5 = 0x5409 ; LCD Port mask register 5 0x00
                                    507 ; 0x00 540A to 0x00 540B Reserved area (2 bytes)
                           00540C   508  LCD_RAM0 = 0x540C ; LCD display memory 0 0x00
                                    509 ; ...
                           005421   510  LCD_RAM21 = 0x5421 ; LCD display memory 21 0x00
                                    511 ; 0x00 5422 to 0x00 542E Reserved area
                           00542F   512  LCD_CR4 = 0x542F ; LCD control register 4 0x00
                                    513 ; RI
                                    514 ; 0x00 5430 Reserved area (1 byte)
                           005431   515  RI_ICR1 = 0x5431 ; RI Timer input capture routing register 1 0x00
                           005432   516  RI_ICR2 = 0x5432 ; RI Timer input capture routing register 2 0x00
                           005433   517  RI_IOIR1 = 0x5433 ; RI I/O input register 1 0xXX
                           005434   518  RI_IOIR2 = 0x5434 ; RI I/O input register 2 0xXX
                           005435   519  RI_IOIR3 = 0x5435 ; RI I/O input register 3 0xXX
                           005436   520  RI_IOCMR1 = 0x5436 ; RI I/O control mode register 1 0x00
                           005437   521  RI_IOCMR2 = 0x5437 ; RI I/O control mode register 2 0x00
                           005438   522  RI_IOCMR3 = 0x5438 ; RI I/O control mode register 3 0x00
                           005439   523  RI_IOSR1 = 0x5439 ; RI I/O switch register 1 0x00
                           00543A   524  RI_IOSR2 = 0x543A ; RI I/O switch register 2 0x00
                           00543B   525  RI_IOSR3 = 0x543B ; RI I/O switch register 3 0x00
                           00543C   526  RI_IOGCR = 0x543C ; RI I/O group control register 0xFF
                           00543D   527  RI_ASCR1 = 0x543D ; Analog switch register 1 0x00
                           00543E   528  RI_ASCR2 = 0x543E ; RI Analog switch register 2 0x00
                           00543F   529  RI_RCR = 0x543F ; RI Resistor control register  0x00
                                    530 ; 0x00 5440 to 0x00 544F Reserved area (16 bytes)
                                    531 ; RI
                           005450   532  RI_CR = 0x5450 ; RI I/O control register 0x00
                           005451   533  RI_MASKR1 = 0x5451 ; RI I/O mask register 1 0x00
                           005452   534  RI_MASKR2  = 0x5452 ; RI I/O mask register 2 0x00
                           005453   535  RI_MASKR3 = 0x5453 ; RI I/O mask register 3 0x00
                           005454   536  RI_MASKR4 = 0x5454 ; RI I/O mask register 4 0x00
                           005455   537  RI_IOIR4 = 0x5455 ; RI I/O input register 4 0xXX
                           005456   538  RI_IOCMR4 = 0x5456 ; RI I/O control mode register 4 0x00
                           005457   539  RI_IOSR4 = 0x5457 ; RI I/O switch register 4 0x00
                                    540 ; CPU(1)
                           007F00   541  A = 0x7F00 ; Accumulator 0x00
                           007F01   542  PCE = 0x7F01 ; Program counter extended  0x00
                           007F02   543  PCH = 0x7F02 ; Program counter high 0x00
                           007F03   544  PCL = 0x7F03 ; Program counter low 0x00
                           007F04   545  XH = 0x7F04 ; X index register high 0x00
                           007F05   546  XL = 0x7F05 ; X index register low 0x00
                           007F06   547  YH = 0x7F06 ; Y index register high 0x00
                           007F07   548  YL = 0x7F07 ; Y index register low 0x00
                           007F08   549  SPH = 0x7F08 ; Stack pointer high 0x03
                           007F09   550  SPL = 0x7F09 ; Stack pointer low 0xFF
                           007F0A   551  CCR = 0x7F0A ; Condition code register 0x28
                                    552 ; CPU
                                    553 ; 0x00 7F0B to 0x00 7F5F Reserved area (85 bytes)
                           007F60   554  CFG_GCR = 0x7F60 ; Global configuration register 0x00
                                    555 ; ITC-SPR
                                    556 
                           000000   557        ITC_IX_TLI     = 0
                           000001   558        ITC_IX_FLASH   = 1
                           000002   559        ITC_IX_DMA1_01 = 2
                           000003   560        ITC_IX_DMA1_23 = 3
                           000003   561        ITC_IX_RTC     = 3
                           000005   562        ITC_IX_PVD     = 5
                           000006   563        ITC_IX_EXTIB   = 6
                           000007   564        ITC_IX_EXTID   = 7
                           000008   565        ITC_IX_EXTI0   = 8
                           000009   566        ITC_IX_EXTI1   = 9
                           00000A   567        ITC_IX_EXTI2  = 10
                           00000B   568        ITC_IX_EXTI3  = 11
                           00000C   569        ITC_IX_EXTI4  = 12
                           00000D   570        ITC_IX_EXTI5  = 13
                           00000E   571        ITC_IX_EXTI6  = 14
                           00000F   572        ITC_IX_EXTI7  = 15
                           000010   573        ITC_IX_CLK    = 16
                           000011   574        ITC_IX_LCD    = 17
                           000012   575        ITC_IX_ADC1   = 18
                           000013   576        ITC_IX_TIM2   = 19       ; TIM2 update /overflow
                           000014   577        ITC_IX_TIM2CC = 20
                           000015   578        ITC_IX_TIM3   = 21       ; TIM3 update /overflow
                           000016   579        ITC_IX_TIM3CC = 22
                           000017   580        ITC_IX_RI     = 23       ; STM8L051F3
                           000017   581        ITC_IX_TIM1   = 23       ; TIM1 update/overflow/underflow/ trigger/break
                           000018   582        ITC_IX_TIM1CC = 24
                           000019   583        ITC_IX_TIM4   = 25
                           00001A   584        ITC_IX_SPI1   = 26
                           00001B   585        ITC_IX_USART1_TXD  = 27
                           00001C   586        ITC_IX_USART1_RXD  = 28
                           00001D   587        ITC_IX_I2C1   = 29
                                    588 
                           007F70   589        ITC_SPR1 = 0x7F70 ; Interrupt Software priority register 1 0xFF
                           007F71   590        ITC_SPR2 = 0x7F71 ; Interrupt Software priority register 2 0xFF
                           007F72   591        ITC_SPR3 = 0x7F72 ; Interrupt Software priority register 3 0xFF
                           007F73   592        ITC_SPR4 = 0x7F73 ; Interrupt Software priority register 4 0xFF
                           007F74   593        ITC_SPR5 = 0x7F74 ; Interrupt Software priority register 5 0xFF
                           007F75   594        ITC_SPR6 = 0x7F75 ; Interrupt Software priority register 6 0xFF
                           007F76   595        ITC_SPR7 = 0x7F76 ; Interrupt Software priority register 7 0xFF
                           007F77   596        ITC_SPR8 = 0x7F77 ; Interrupt Software priority register 8 0xFF
                                    597 
                                    598 ; 0x00 7F78 to 0x00 7F79 Reserved area (2 bytes)
                                    599 ; SWIM
                           007F80   600  SWIM_CSR = 0x7F80 ; SWIM control status register 0x00
                                    601 ; 0x00 7F81 to 0x00 7F8F Reserved area (15 bytes)
                                    602 ; DM
                           007F90   603  DM_BK1RE = 0x7F90 ; DM breakpoint 1 register extended byte 0xFF
                           007F91   604  DM_BK1RH = 0x7F91 ; DM breakpoint 1 register high byte 0xFF
                           007F92   605  DM_BK1RL = 0x7F92 ; DM breakpoint 1 register low byte 0xFF
                           007F93   606  DM_BK2RE = 0x7F93 ; DM breakpoint 2 register extended byte 0xFF
                           007F94   607  DM_BK2RH = 0x7F94 ; DM breakpoint 2 register high byte 0xFF
                           007F95   608  DM_BK2RL = 0x7F95 ; DM breakpoint 2 register low byte 0xFF
                           007F96   609  DM_CR1 = 0x7F96 ; DM Debug module control register 1 0x00
                                    610 ; DM
                           007F97   611  DM_CR2 = 0x7F97 ; DM Debug module control register 2 0x00
                           007F98   612  DM_CSR1 = 0x7F98 ; DM Debug module control/status register 1 0x10
                           007F99   613  DM_CSR2 = 0x7F99 ; DM Debug module control/status register 2 0x00
                           007F9A   614  DM_ENFCTR = 0x7F9A ; DM enable function register 0xFF
                                    615 ; 0x00 7F9B to 0x00 7F9F Reserved area (5 bytes)
                                    616 ; 1.   Accessible by debug module only
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
                           008313    32         EMIT_BG  = DROP         ; vectored NUL background EMIT vector
                           00837D    33         QKEY_BG  = ZERO         ; NUL background QKEY vector
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
                                      2 ; Config for STM8L152K4
                                      3 ; Clock: HSI (no crystal)
                                      4 
                           000000     5         ALT_USART_STM8L  = 0    ; Alternative STM8L USART GPIO mapping (e.g. PA3/PA2 instead of PC5/PC6)
                           000000     6         HALF_DUPLEX      = 0    ; Use UART in half duplex mode
                           000001     7         HAS_TXUART       = 1    ; No UART TXD, word TX!
                           000001     8         HAS_RXUART       = 1    ; No UART RXD, word ?RX
                                      9 
                           000000    10         HAS_TXSIM        = 0    ; Enable TxD via GPIO/TIM4, word TXGP!
                           000000    11         HAS_RXSIM        = 0    ; Enable RxD via GPIO/TIM4, word ?RXGP
                           000000    12         PNRX             = 0    ; Port GPIO# for HAS_RXDSIM
                           000000    13         PNTX             = 0    ; Port GPIO# for HAS_TXDSIM
                                     14 
                           008313    15         EMIT_BG  = DROP         ; 7S-LED background EMIT vector
                           00837D    16         QKEY_BG  = ZERO         ; Board keys background QKEY vector
                                     17 
                           000000    18         HAS_LCD          = 0    ; optional internal LCD controller, Use 14 bytes of ram
                           000000    19         HAS_LED7SEG      = 0    ; no 7S-Display
                           000000    20         HAS_KEYS         = 0    ; no (STM8L-Discovery: i2c conflict)
                           000000    21         HAS_OUTPUTS      = 0    ; optional, two LEDs
                           000000    22         HAS_ADC          = 0    ; Analog input words
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
                           0007A0   251         SPP   = ISPP-ISPPSIZE   ; "SPP"  data stack, growing down (with SPP-1 first)
                           0007B0   252         ISPP  = SPPLOC-BSPPSIZE
                           0007D0   253         BSPP  = SPPLOC          ; "BSPP" Background data stack, growing down
                           0007FF   254         RPP   = RPPLOC          ; "RPP"  constant addr. return stack, growing down
                                    255 
                                    256         ; Core variables (same order as 'BOOT initializer block)
                                    257 
                                    258 ;        USRRAMINIT = USREMIT
                                    259 
      008080                        260         RamWord USREMIT         ; "'EMIT" execution vector of EMIT
                           000000     1         USREMIT = RAMPOOL
                           000002     2         RAMPOOL = RAMPOOL + 2
      008080                        261         RamWord USRQKEY         ; "'?KEY" execution vector of QKEY
                           000002     1         USRQKEY = RAMPOOL
                           000004     2         RAMPOOL = RAMPOOL + 2
                           000000   262 .if  HAS_RXSIM
                                    263         RamByte USR_5           ; chat variables
                                    264         RamByte USR_6           ;
                                    265 .endif
      008080                        266         RamWord MP              ; memory pointer for mu-chat
                           000004     1         MP = RAMPOOL
                           000006     2         RAMPOOL = RAMPOOL + 2
                                    267 
                                    268         ; More core variables in zero page (instead of assigning fixed addresses)
      008080                        269         RamWord USRHLD          ; "HLD" hold a pointer of output string
                           000006     1         USRHLD = RAMPOOL
                           000008     2         RAMPOOL = RAMPOOL + 2
      008080                        270         RamByte XREG0           ; extra working register for core words
                           000008     1         XREG0 = RAMPOOL
                           000009     2         RAMPOOL = RAMPOOL + 1
      008080                        271         RamByte XREG1           ; extra working register for core words
                           000009     1         XREG1 = RAMPOOL
                           00000A     2         RAMPOOL = RAMPOOL + 1
      008080                        272         RamByte XREG2           ; extra working register for core words
                           00000A     1         XREG2 = RAMPOOL
                           00000B     2         RAMPOOL = RAMPOOL + 1
      008080                        273         RamByte XREG3           ; extra working register for core words
                           00000B     1         XREG3 = RAMPOOL
                           00000C     2         RAMPOOL = RAMPOOL + 1
      008080                        274         RamWord BITAT           ; reserve space for BTJF
                           00000C     1         BITAT = RAMPOOL
                           00000E     2         RAMPOOL = RAMPOOL + 2
                           000016   275         RAMPOOL = RAMPOOL + 8
      008080                        276         RamWord BITSTO          ; reserve space for BSET/BRES
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
      008080                        337         HEADER  RXA "RXA"
      008080                        338 RXA:
                           000001   339 .if HAS_RXUART
      008080 72 0B 52 30 FB   [ 2]  340         BTJF    UART_SR,#5,RXA
      008085 C6 52 31         [ 1]  341         LD      A,UART_DR      ; get char in A
                           000000   342 .else
                                    343         BTJF USR_6,#0,RXA
                                    344         LD A,TIM4RXBUF
                                    345         CLR USR_6		; clear rxa flag
                                    346 .endif
      008088 81               [ 4]  347         RET
                                    348 
                                    349 ; receive byte in tos 
      008089                        350         HEADER  TOB "TOB"
      008089                        351 TOB:
                           000000   352 .if HAS_RXSIM
                                    353         CLR USR_6
                                    354 .endif
      008089 AD F5            [ 4]  355         CALLR RXA
      00808B 5A               [ 2]  356         DECW X
      00808C F7               [ 1]  357         LD (X),A
      00808D 5A               [ 2]  358         DECW X
      00808E 7F               [ 1]  359         CLR (X)
      00808F 81               [ 4]  360         RET
                                    361 
                                    362 ; receive cell in tos 
      008090                        363         HEADER  TOW "TOW"
      008090                        364 TOW:
      008090 AD F7            [ 4]  365         CALLR TOB
      008092 AD EC            [ 4]  366         CALLR RXA
      008094 F7               [ 1]  367         LD (X),A
      008095 81               [ 4]  368         RET
                                    369 
                                    370 ; send byte from tos 
      008096                        371         HEADER  ATO "ATO"
      008096                        372 ATO:
      008096 F6               [ 1]  373         LD A,(X)
      008097 5C               [ 1]  374         INCW X
      008098 CC 81 F6         [ 2]  375         JP TXASTOR
                                    376 
                                    377 ; send cell from tos 
      00809B                        378         HEADER  WTO "WTO"
      00809B                        379 WTO:
      00809B CD 85 57         [ 4]  380         CALL EXG
      00809E AD F6            [ 4]  381         CALLR ATO
      0080A0 20 F4            [ 2]  382         JRA ATO
                                    383 
                                    384 
                                    385 ; send bytes from memory pointed to by MP 
      0080A2                        386         HEADER  SENDBYTES "SENDBYTES"
      0080A2                        387 SENDBYTES:
      0080A2 AD E5            [ 4]  388         CALLR TOB
      0080A4 5C               [ 1]  389         INCW X
      0080A5 90 BE 04         [ 2]  390         LDW Y,MP
      0080A8                        391 1$:
      0080A8 90 F6            [ 1]  392         LD A,(Y)
      0080AA CD 81 F6         [ 4]  393         CALL TXASTOR
      0080AD 90 5C            [ 1]  394         INCW Y
      0080AF 7A               [ 1]  395         DEC(X)
      0080B0 26 F6            [ 1]  396         JRNE 1$
      0080B2 5C               [ 1]  397         INCW X
      0080B3 81               [ 4]  398         RET
                                    399 
                                    400 ;       receive byte and store in memory pointer MP 
      0080B4                        401         HEADER  SETADDR "SETADDR"
      0080B4                        402 SETADDR:
      0080B4 AD DA            [ 4]  403         CALLR TOW
      0080B6 90 93            [ 1]  404         LDW Y,X
      0080B8 90 FE            [ 2]  405         LDW Y,(Y)
      0080BA 90 BF 04         [ 2]  406         LDW MP,Y
      0080BD 5C               [ 1]  407         INCW X
      0080BE 5C               [ 1]  408         INCW X
      0080BF 81               [ 4]  409         RET
                                    410 
                                    411 ;       
      0080C0                        412         HEADER  GETSP "GETSP"
      0080C0                        413 GETSP:
      0080C0 CD 86 27         [ 4]  414         CALL SPAT
      0080C3 20 D6            [ 2]  415         JRA WTO
                                    416 
                                    417 ;       
      0080C5                        418         HEADER  WRITEBS "WRITEBS"
      0080C5                        419 WRITEBS:
      0080C5 AD C2            [ 4]  420         CALLR TOB	; count
      0080C7 90 BE 04         [ 2]  421 1$:	LDW Y,MP		; memory pointer in Y
      0080CA AD B4            [ 4]  422         CALLR RXA        ; 
      0080CC 90 F7            [ 1]  423         LD (Y),A
      0080CE 90 5C            [ 1]  424         INCW Y
      0080D0 90 BF 04         [ 2]  425         LDW MP,Y
                           000000   426 .if HAS_RXSIM
                                    427         CLR USR_6
                                    428 .endif
      0080D3 90 93            [ 1]  429         LDW Y,X
      0080D5 90 FE            [ 2]  430         LDW Y,(Y)
      0080D7 90 5A            [ 2]  431         DECW Y
      0080D9 FF               [ 2]  432         LDW (X),Y
      0080DA 26 EB            [ 1]  433         JRNE 1$
      0080DC 5C               [ 1]  434         INCW X
      0080DD 5C               [ 1]  435         INCW X
      0080DE 81               [ 4]  436         RET
                                    437 
      0080DF                        438         HEADER  SETSP "SETSP"
      0080DF                        439 SETSP:
      0080DF AD AF            [ 4]  440         CALLR TOW
      0080E1 FE               [ 2]  441         LDW X,(X)
      0080E2 81               [ 4]  442         RET
                                    443 
      0080E3                        444         HEADER  RUN "RUN"
      0080E3                        445 RUN:
      0080E3 AD AB            [ 4]  446         CALLR TOW
      0080E5 FE               [ 2]  447         LDW X,(X)
      0080E6 AD A8            [ 4]  448         CALLR TOW
      0080E8 CC 82 18         [ 2]  449         JP EXECU
                                    450 
      0080EB                        451         HEADER  FLASH "FLASH"
      0080EB                        452 FLASH:
      0080EB                        453         DoLitW FLASHBUF_ADDR
      0080EB 5A               [ 2]    1         DECW X
      0080EC 5A               [ 2]    2         DECW X
      0080ED 90 AE 07 40      [ 2]    3         LDW Y,#FLASHBUF_ADDR
      0080F1 FF               [ 2]    4         LDW (X),Y
      0080F2 AD 9C            [ 4]  454         CALLR TOW
      0080F4 AD 9A            [ 4]  455         CALLR TOW
      0080F6 CD 85 F4         [ 4]  456         CALL CMOVE
      0080F9 A6 AB            [ 1]  457         LD A,#0xAB
      0080FB CC 81 F6         [ 2]  458         JP TXASTOR
                                    459 
      0080FE                        460         HEADER  TABLE "TABLE"
      0080FE                        461 TABLE:
      0080FE 80 B4                  462         .dw SETADDR
      008100 80 A2                  463         .dw SENDBYTES
      008102 80 C5                  464         .dw WRITEBS
      008104 80 C0                  465         .dw GETSP
      008106 80 DF                  466         .dw SETSP
      008108 80 E3                  467         .dw RUN
      00810A 80 EB                  468         .dw FLASH
      00810C 9D               [ 1]  469 NOP     ; for disaasembling purpose
                           00000F   470 lower=0xf
                           000018   471 upper=0x18
                           000010   472 offset=0x10
                                    473 
      00810D                        474         HEADER  CHAT "CHAT"
      00810D                        475 CHAT:
                           000000   476 .if HAS_RXSIM
                                    477         LD A,TIM4RXBUF
                                    478         CLR USR_6
                           000001   479 .else
      00810D CD 80 80         [ 4]  480         CALL RXA
                                    481 .endif
      008110 A1 0F            [ 1]  482         CP A,#lower
      008112 2B 14            [ 1]  483         JRMI 1$
      008114 A1 18            [ 1]  484         CP A,#upper
      008116 2C 10            [ 1]  485         JRSGT 1$
      008118 A0 10            [ 1]  486         SUB A,#offset
      00811A 48               [ 1]  487         SLL A
      00811B AB FE            [ 1]  488         ADD A,#TABLE
      00811D 90 97            [ 1]  489         LD YL,A
      00811F 4F               [ 1]  490         CLR A
      008120 A9 80            [ 1]  491         ADC A,#>TABLE   ; MSB of TABLE
      008122 90 95            [ 1]  492         LD YH,A
      008124 90 FE            [ 2]  493         LDW Y,(Y)
      008126 90 FC            [ 2]  494         JP (Y)
      008128                        495 1$:
      008128 81               [ 4]  496         RET
                                    497         
                                    498 ; ==============================================
                                    499 ;       Getbit and Setbit routines to be moved 
                                    500 ;       to ram during reset ( -- )
                                    501 ; ==============================================
                                    502 
      008129                        503         HEADER  COLD1 "COLD1"
      008129                        504 COLD1:
      008129 4F               [ 1]  505         CLR A
      00812A 72 01 00 08 01   [ 2]  506         BTJF XREG0,#0,1$
      00812F 4C               [ 1]  507         INC A
      008130 E7 01            [ 1]  508 1$:     LD (1,X),A
      008132 81               [ 4]  509         RET
      008133 72 10 01 00      [ 1]  510         BSET 0x100,#0
      008137 81               [ 4]  511         RET
                                    512 ; ==============================================
                                    513 
                                    514 ; ==============================================
                                    515 
                                    516 ;       Includes for board support code
                                    517 ;       Board I/O initialization and E/E mapping code
                                    518 ;       Hardware dependent words, e.g.  BKEY, OUT!
                                    519         .include "boardcore.inc"
                                      1 ; STM8L151K$ "Core" STM8L device dependent routine default code
                                      2 
                                      3 ; Note: for supporting a new board create a new board configuration
                                      4 ;       folder with a "globconfig.inc" and a copy of this file.
                                      5 
                                      6 ;===============================================================
                                      7 
                                      8 ;       BOARDINIT  ( -- )
                                      9 ;       Init board GPIO (except COM ports)
                                     10 
                                     11         .macro  PSET_TX PUART, TXPIN
                                     12         .ifeq   HALF_DUPLEX
                                     13         BSET    PUART+DDR,#TXPIN ; HDSEL: USART controls the data direction
                                     14         .endif
                                     15         BSET    PUART+CR1,#TXPIN
                                     16         .endm
                                     17 
      008138                         18 BOARDINIT:
                                     19         ; Board I/O initialization
                                     20 
                           000001    21         .ifne   HAS_BACKGROUND
                           000000    22         .ifne   BG_USE_TIM1
                                     23         BSET    CLK_PCKENR2,#1   ; TIM1[1]
                           000001    24         .else
                           000000    25         .ifne   BG_USE_TIM3
                                     26         BSET    CLK_PCKENR1,#1   ; TIM3[1]
                           000001    27         .else
      008138 72 10 50 C3      [ 1]   28         BSET    CLK_PCKENR1,#0   ; TIM2[0]
                                     29         .endif
                                     30         .endif
                                     31         .endif
                                     32 
                           000001    33         .ifne   HAS_TXUART
      00813C 72 1A 50 C3      [ 1]   34         BSET    CLK_PCKENR1,#5  ; Enable USART1 clock
                                     35 
                           000001    36         .ifeq   ALT_USART_STM8L
                                     37         ; Map USART1 to PC3[TX] and PC2[RX]
      008140                         38         PSET_TX PORTC, 3
                           000001     1         .ifeq   HALF_DUPLEX
      008140 72 16 50 0C      [ 1]    2         BSET    PORTC+DDR,#3 ; HDSEL: USART controls the data direction
                                      3         .endif
      008144 72 16 50 0D      [ 1]    4         BSET    PORTC+CR1,#3
                                     39         .endif
                                     40 
                           000000    41         .ifeq   (ALT_USART_STM8L-1)
                                     42         ; Map USART1 to PA2[TX] and PA3[RX]
                                     43         BSET    SYSCFG_RMPCR1,#4
                                     44         PSET_TX PORTA, 2
                                     45         .endif
                                     46 
                           000000    47         .ifeq   (ALT_USART_STM8L-2)
                                     48         ; Map USART1 to PC5[TX] and PC6[RX]
                                     49         BSET    SYSCFG_RMPCR1,#5
                                     50         PSET_TX PORTC, 5
                                     51         .endif
                                     52         .endif
                                     53 
                                     54 
                           000000    55         .ifne   HAS_OUTPUTS
                                     56         ; Leds init
                                     57         BSET    PE_DDR,#7 ; PE7 LED green
                                     58         BSET    PE_CR1,#7
                                     59         BSET    PC_DDR,#7 ; PC7 LED blue
                                     60         BSET    PC_CR1,#7
                                     61         .endif
                                     62 
                           000000    63         .ifne   HAS_LCD
                                     64         MOV     CLK_CRTCR, #0x04    ; Eanble RTC clock drived by LSI generator.
                                     65         BSET    CLK_PCKENR2, #3     ; Enable internal LCD driver
                                     66         MOV     LCD_CR1, #0x06 ; Drive method: multiplexed 1/4 duty, 1/3 bias
                                     67         MOV     LCD_CR2, #0x0E ; Contrast
                                     68         MOV     LCD_FRQ, #0x10 ; Refresh frequence
                                     69         MOV     LCD_PM0, #0xFF ; Enable SEG00..07
                                     70         MOV     LCD_PM1, #0xFF ; Enable SEG08..15
                                     71         MOV     LCD_PM2, #0xFF ; Enable SEG16..23
                                     72         MOV     LCD_CR3, #0x40 ; Enable LCD
                                     73         ; Clean LCD
                                     74         CLR     A
                                     75         CALL    LCD_FILL
                                     76         .endif
                                     77 
                           000000    78         .ifne   HAS_OUTPUTS
                                     79         CLR     A
                                     80         JRA     AOUTSTOR
                           000001    81         .else
      008148 81               [ 4]   82         RET
                                     83         .endif
                                     84 
                                     85 
                                     86 
                                     87 
                                     88 ; ==============================================
                                     89 
                           000000    90         .ifne   HAS_LED7SEG
                                     91 ;       LED_MPX driver ( -- )
                                     92 ;       Code called from ISR for LED MPX
                                     93 
                                     94 LED_MPX:
                                     95         RET
                                     96         .endif
                                     97 
                                     98 ; ==============================================
                                     99 
                           000000   100         .ifne   HAS_OUTPUTS
                                    101         RamWord OUTPUTS         ; "OUT", e.g. relays, LEDs, etc. (16 bit)
                                    102 
                                    103 ;       OUT!  ( c -- )
                                    104 ;       Put c to board outputs, storing a copy in OUTPUTS
                                    105         .dw     LINK
                                    106 
                                    107         LINK =  .
                                    108         .db     (4)
                                    109         .ascii  "OUT!"
                                    110 OUTSTOR:
                                    111         INCW    X
                                    112         LD      A,(X)
                                    113         INCW    X
                                    114 AOUTSTOR:
                                    115         LD      OUTPUTS+1,A
                                    116         RRC     A
                                    117         BCCM    PE_ODR,#7       ; PE7 LED green
                                    118         RRC     A
                                    119         BCCM    PC_ODR,#7       ; PC7 LED blue
                                    120         RET
                                    121         .endif
                                    122 
                                    123 ;===============================================================
                                    124 
                           000000   125         .ifne   HAS_KEYS
                                    126 ;       BKEY  ( -- f )     ( TOS STM8: -- A,Z,N )
                                    127 ;       Read board key state as a bitfield
                                    128         .dw     LINK
                                    129 
                                    130         LINK =  .
                                    131         .db     (4)
                                    132         .ascii  "BKEY"
                                    133 BKEY:
                                    134         CLR     A
                                    135         JP      ASTOR
                                    136 
                                    137 
                                    138 ;       BKEYC  (  -- c )   ( TOS STM8: -- A,Z,N )
                                    139 ;       Read and translate board dependent key bitmap into char
                                    140 
                                    141 BKEYCHAR:
                                    142         JRA     BKEY            ; Dummy: get "no key" and leave it as it is
                                    143        .endif
                                    144 
                                    145 ;===============================================================
                                    146 ; LCD routines
                                    147 
                           000000   148         .ifne   HAS_LCD
                                    149 ;       LCDF  ( b -- )     ( TOS STM8: -- A,Z,N )
                                    150 ;       Fill (Clean) LCD buffer
                                    151         .dw     LINK
                                    152 
                                    153         LINK =  .
                                    154         .db     (4)
                                    155         .ascii  "LCDF"
                                    156 LCDF:
                                    157         INCW    X
                                    158         LD      A, (X)
                                    159         INCW    X
                                    160 LCD_FILL:
                                    161         PUSHW   X
                                    162         LDW     X, #LCD_RAM0
                                    163 3$:     LD      (X), A
                                    164         INCW    X
                                    165         CPW     X, #(LCD_RAM0+HAS_LCD)
                                    166         JRNE    3$
                                    167         POPW    X
                                    168         RET
                                    169 
                                    170 ;       LCD!  ( w a -- )     ( TOS STM8: -- A,Z,N )
                                    171 ;       Write data w to LCD position a[0:5]
                                    172 ;       w bits [Cl Dp Q P N K J H M G F E D C B A]
                                    173         .dw     LINK
                                    174 
                                    175         LINK =  .
                                    176         .db     (4)
                                    177         .ascii  "LCD!"
                                    178 LCD_STOR:
                                    179         ; Load word
                                    180         PUSHW   X
                                    181         LD      A, (#1, X)
                                    182         LDW     X, (#2, X)
                                    183         TNZ     A
                                    184         JRNE    1$
                                    185         CALLR   LCDS1
                                    186         JRA     6$
                                    187 1$:
                                    188         DEC     A
                                    189         JRNE    2$
                                    190         CALLR   LCDS2
                                    191         JRA     6$
                                    192 2$:
                                    193         DEC     A
                                    194         JRNE    3$
                                    195         CALL    LCDS3
                                    196         JRA     6$
                                    197 3$:
                                    198         DEC     A
                                    199         JRNE    4$
                                    200         CALL    LCDS4
                                    201         JRA     6$
                                    202 4$:
                                    203         DEC     A
                                    204         JRNE    5$
                                    205         CALL    LCDS5
                                    206         JRA     6$
                                    207 5$:
                                    208         DEC     A
                                    209         JRNE    6$
                                    210         CALL    LCDS6
                                    211 6$:
                                    212         POPW    X
                                    213         ADDW    X, #4
                                    214         RET
                                    215 
                                    216 ;       In: X [Cl Dp Q P N K J H M G F E D C B A]
                                    217 LCDS1:
                                    218         SRLW    X
                                    219         BCCM    LCD_RAM0+6, #2 ; A
                                    220         SRLW    X
                                    221         BCCM    LCD_RAM0+2, #6 ; B
                                    222         SRLW    X
                                    223         BCCM    LCD_RAM0+3, #5 ; C
                                    224         SRLW    X
                                    225         BCCM    LCD_RAM0+3, #4 ; D
                                    226         SRLW    X
                                    227         BCCM    LCD_RAM0+0, #0 ; E
                                    228         SRLW    X
                                    229         BCCM    LCD_RAM0+6, #3 ; F
                                    230         SRLW    X
                                    231         BCCM    LCD_RAM0+2, #7 ; G
                                    232         SRLW    X
                                    233         BCCM    LCD_RAM0+0, #1 ; M
                                    234         SRLW    X
                                    235         BCCM    LCD_RAM0+13, #3 ; H
                                    236         SRLW    X
                                    237         BCCM    LCD_RAM0+13, #2 ; J
                                    238         SRLW    X
                                    239         BCCM    LCD_RAM0+9, #6 ; K
                                    240         SRLW    X
                                    241         BCCM    LCD_RAM0+10, #4 ; N
                                    242         SRLW    X
                                    243         BCCM    LCD_RAM0+7, #0 ; P
                                    244         SRLW    X
                                    245         BCCM    LCD_RAM0+9, #7 ; Q
                                    246         SRLW    X
                                    247         BCCM    LCD_RAM0+10, #5 ; DP
                                    248         SRLW    X
                                    249         BCCM    LCD_RAM0+7, #1 ; COLON
                                    250         RET
                                    251 
                                    252 ;       In: X [Cl Dp Q P N K J H M G F E D C B A]
                                    253 LCDS2:
                                    254         SRLW    X
                                    255         BCCM    LCD_RAM0+6, #0 ; A
                                    256         SRLW    X
                                    257         BCCM    LCD_RAM0+2, #4 ; B
                                    258         SRLW    X
                                    259         BCCM    LCD_RAM0+3, #7 ; C
                                    260         SRLW    X
                                    261         BCCM    LCD_RAM0+3, #6 ; D
                                    262         SRLW    X
                                    263         BCCM    LCD_RAM0+0, #2 ; E
                                    264         SRLW    X
                                    265         BCCM    LCD_RAM0+6, #1 ; F
                                    266         SRLW    X
                                    267         BCCM    LCD_RAM0+2, #5 ; G
                                    268         SRLW    X
                                    269         BCCM    LCD_RAM0+0, #3 ; M
                                    270         SRLW    X
                                    271         BCCM    LCD_RAM0+13, #1 ; H
                                    272         SRLW    X
                                    273         BCCM    LCD_RAM0+13, #0 ; J
                                    274         SRLW    X
                                    275         BCCM    LCD_RAM0+9, #4 ; K
                                    276         SRLW    X
                                    277         BCCM    LCD_RAM0+10, #6 ; N
                                    278         SRLW    X
                                    279         BCCM    LCD_RAM0+7, #2 ; P
                                    280         SRLW    X
                                    281         BCCM    LCD_RAM0+9, #5 ; Q
                                    282         SRLW    X
                                    283         BCCM    LCD_RAM0+10, #7 ; DP
                                    284         SRLW    X
                                    285         BCCM    LCD_RAM0+7, #3 ; COLON
                                    286         RET
                                    287 
                                    288 ;       In: X [Cl Dp Q P N K J H M G F E D C B A]
                                    289 LCDS3:
                                    290         SRLW    X
                                    291         BCCM    LCD_RAM0+5, #6 ; A
                                    292         SRLW    X
                                    293         BCCM    LCD_RAM0+2, #2 ; B
                                    294         SRLW    X
                                    295         BCCM    LCD_RAM0+4, #1 ; C
                                    296         SRLW    X
                                    297         BCCM    LCD_RAM0+4, #0 ; D
                                    298         SRLW    X
                                    299         BCCM    LCD_RAM0+0, #4 ; E
                                    300         SRLW    X
                                    301         BCCM    LCD_RAM0+5, #7 ; F
                                    302         SRLW    X
                                    303         BCCM    LCD_RAM0+2, #3 ; G
                                    304         SRLW    X
                                    305         BCCM    LCD_RAM0+0, #5 ; M
                                    306         SRLW    X
                                    307         BCCM    LCD_RAM0+12, #7 ; H
                                    308         SRLW    X
                                    309         BCCM    LCD_RAM0+12, #6 ; J
                                    310         SRLW    X
                                    311         BCCM    LCD_RAM0+9, #2 ; K
                                    312         SRLW    X
                                    313         BCCM    LCD_RAM0+11, #0 ; N
                                    314         SRLW    X
                                    315         BCCM    LCD_RAM0+7, #4 ; P
                                    316         SRLW    X
                                    317         BCCM    LCD_RAM0+9, #3 ; Q
                                    318         SRLW    X
                                    319         BCCM    LCD_RAM0+11, #1 ; DP
                                    320         SRLW    X
                                    321         BCCM    LCD_RAM0+7, #5 ; COLON
                                    322         RET
                                    323 
                                    324 ;       In: X [Cl Dp Q P N K J H M G F E D C B A]
                                    325 LCDS4:
                                    326         SRLW    X
                                    327         BCCM    LCD_RAM0+5, #4 ; A
                                    328         SRLW    X
                                    329         BCCM    LCD_RAM0+2, #0 ; B
                                    330         SRLW    X
                                    331         BCCM    LCD_RAM0+4, #3 ; C
                                    332         SRLW    X
                                    333         BCCM    LCD_RAM0+4, #2 ; D
                                    334         SRLW    X
                                    335         BCCM    LCD_RAM0+0, #6 ; E
                                    336         SRLW    X
                                    337         BCCM    LCD_RAM0+5, #5 ; F
                                    338         SRLW    X
                                    339         BCCM    LCD_RAM0+2, #1 ; G
                                    340         SRLW    X
                                    341         BCCM    LCD_RAM0+0, #7 ; M
                                    342         SRLW    X
                                    343         BCCM    LCD_RAM0+12, #5 ; H
                                    344         SRLW    X
                                    345         BCCM    LCD_RAM0+12, #4 ; J
                                    346         SRLW    X
                                    347         BCCM    LCD_RAM0+9, #0 ; K
                                    348         SRLW    X
                                    349         BCCM    LCD_RAM0+11, #2 ; N
                                    350         SRLW    X
                                    351         BCCM    LCD_RAM0+7, #6 ; P
                                    352         SRLW    X
                                    353         BCCM    LCD_RAM0+9, #1 ; Q
                                    354         SRLW    X
                                    355         BCCM    LCD_RAM0+11, #3 ; DP
                                    356         SRLW    X
                                    357         BCCM    LCD_RAM0+7, #7 ; COLON
                                    358         RET
                                    359 
                                    360 ;       In: X [Cl Dp Q P N K J H M G F E D C B A]
                                    361 LCDS5:
                                    362         SRLW    X
                                    363         BCCM    LCD_RAM0+5, #2 ; A
                                    364         SRLW    X
                                    365         BCCM    LCD_RAM0+1, #6 ; B
                                    366         SRLW    X
                                    367         BCCM    LCD_RAM0+4, #5 ; C
                                    368         SRLW    X
                                    369         BCCM    LCD_RAM0+4, #4 ; D
                                    370         SRLW    X
                                    371         BCCM    LCD_RAM0+1, #0 ; E
                                    372         SRLW    X
                                    373         BCCM    LCD_RAM0+5, #3 ; F
                                    374         SRLW    X
                                    375         BCCM    LCD_RAM0+1, #7 ; G
                                    376         SRLW    X
                                    377         BCCM    LCD_RAM0+1, #1 ; M
                                    378         SRLW    X
                                    379         BCCM    LCD_RAM0+12, #3 ; H
                                    380         SRLW    X
                                    381         BCCM    LCD_RAM0+12, #2 ; J
                                    382         SRLW    X
                                    383         BCCM    LCD_RAM0+8, #6 ; K
                                    384         SRLW    X
                                    385         BCCM    LCD_RAM0+11, #4 ; N
                                    386         SRLW    X
                                    387         BCCM    LCD_RAM0+8, #0 ; P
                                    388         SRLW    X
                                    389         BCCM    LCD_RAM0+8, #7 ; Q
                                    390         SRLW    X
                                    391         BCCM    LCD_RAM0+11, #5 ; DP
                                    392         SRLW    X
                                    393         BCCM    LCD_RAM0+8, #1 ; COLON
                                    394         RET
                                    395 
                                    396 ;       In: X [Cl Dp Q P N K J H M G F E D C B A]
                                    397 LCDS6:
                                    398         SRLW    X
                                    399         BCCM    LCD_RAM0+5, #0 ; A
                                    400         SRLW    X
                                    401         BCCM    LCD_RAM0+1, #4 ; B
                                    402         SRLW    X
                                    403         BCCM    LCD_RAM0+4, #7 ; C
                                    404         SRLW    X
                                    405         BCCM    LCD_RAM0+4, #6 ; D
                                    406         SRLW    X
                                    407         BCCM    LCD_RAM0+1, #2 ; E
                                    408         SRLW    X
                                    409         BCCM    LCD_RAM0+5, #1 ; F
                                    410         SRLW    X
                                    411         BCCM    LCD_RAM0+1, #5 ; G
                                    412         SRLW    X
                                    413         BCCM    LCD_RAM0+1, #3 ; M
                                    414         SRLW    X
                                    415         BCCM    LCD_RAM0+12, #1 ; H
                                    416         SRLW    X
                                    417         BCCM    LCD_RAM0+12, #0 ; J
                                    418         SRLW    X
                                    419         BCCM    LCD_RAM0+8, #4 ; K
                                    420         SRLW    X
                                    421         BCCM    LCD_RAM0+11, #6 ; N
                                    422         SRLW    X
                                    423         BCCM    LCD_RAM0+8, #2 ; P
                                    424         SRLW    X
                                    425         BCCM    LCD_RAM0+8, #5 ; Q
                                    426         SRLW    X
                                    427         BCCM    LCD_RAM0+11, #7 ; DP
                                    428         SRLW    X
                                    429         BCCM    LCD_RAM0+8, #3 ; COLON
                                    430         RET
                                    431 
                                    432        .endif
                                    433 
                                    434 
                                    520 
                                    521 ;       ADC routines depending on STM8 family
                                    522         .include "stm8_adc.inc"
                                      1 ;--------------------------------------------------------
                                      2 ;       STM8 eForth           STM8S/L Family ADC code
                                      3 ;--------------------------------------------------------
                                      4 
                           000000     5         .ifne   HAS_ADC
                                      6 ;       ADC!  ( c -- )
                                      7 ;       Init ADC, select channel for conversion
                                      8 
                                      9         .ifeq   (FAMILY - STM8L)
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
                                     71         .else
                                     72 
                                     73 ;       ADC for the STM8S family
                                     74 
                                     75 ;       ADC!  ( c -- )
                                     76 ;       Init ADC, select channel for conversion
                                     77 
                                     78         HEADER  ADCSTOR "ADC!"
                                     79 ADCSTOR:
                                     80         INCW    X
                                     81         LD      A,(X)
                                     82         INCW    X
                                     83         AND     A,#0x0F
                                     84         LD      ADC_CSR,A       ; select channel
                                     85         BSET    ADC_CR2,#3      ; align ADC to LSB
                                     86         BSET    ADC_CR1,#0      ; enable ADC
                                     87         RET
                                     88 
                                     89 ;       ADC@  ( -- w )
                                     90 ;       start ADC conversion, read result
                                     91 
                                     92         HEADER  ADCAT "ADC@"
                                     93 ADCAT:
                                     94         BRES    ADC_CSR,#7      ; reset EOC
                                     95         BSET    ADC_CR1,#0      ; start ADC
                                     96 1$:     BTJF    ADC_CSR,#7,1$   ; wait until EOC
                                     97         LDW     Y,ADC_DRH       ; read ADC
                                     98         DECW    X               ; SUBW  X,#2
                                     99         DECW    X
                                    100         LDW     (X),Y           ; push on stack
                                    101         RET                     ; go to RET of EXEC
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
      008149                         26 _EXTI0_IRQHandler:
      008149                         27 _EXTI1_IRQHandler:
      008149                         28 _EXTI2_IRQHandler:
      008149                         29 _EXTI3_IRQHandler:
      008149                         30 _EXTI4_IRQHandler:
      008149                         31 _EXTI5_IRQHandler:
      008149                         32 _EXTI6_IRQHandler:
      008149                         33 _EXTI7_IRQHandler:
      008149                         34 _TIM4_IRQHandler:
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
      008149                         12         RamWord BGADDR          ; address of background routine (0: off)
                           00001B     1         BGADDR = RAMPOOL
                           00001D     2         RAMPOOL = RAMPOOL + 2
      008149                         13         RamWord TICKCNT         ; "TICKCNT" 16 bit ticker (counts up)
                           00001D     1         TICKCNT = RAMPOOL
                           00001F     2         RAMPOOL = RAMPOOL + 2
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
      008149                         45 _TIM1_IRQHandler:
      008149                         46 _TIM2_IRQHandler:
      008149                         47 _TIM3_IRQHandler:
                                     48         ; STM8 DIV/DIVW erratum "Workaround 2: keep bit6 cleared"
      008149 4B 08            [ 1]   49         PUSH    #0x08           ; BG task fixed priority (I0=1, I1=0)
      00814B 86               [ 1]   50         POP     CC
                                     51 
                           000001    52         .ifne   (HAS_LED7SEG + HAS_BACKGROUND)
      00814C 72 11 52 56      [ 1]   53         BRES    BG_TIM_SR1,#0   ; clear TIMx UIF
                                     54 
                           000000    55         .ifne   HAS_LED7SEG
                                     56         CALL    LED_MPX         ; "PC_LEDMPX" board dependent code for 7Seg-LED-Displays
                                     57         .endif
                                     58 
                                     59 ;       Background operation saves & restores the context of the interactive task
                                     60 ;       Cyclic context reset of Forth background task (stack, BASE, HLD, I/O vector)
                           000001    61         .ifne   HAS_BACKGROUND
      008150 BE 1D            [ 2]   62         LDW     X,TICKCNT
      008152 5C               [ 1]   63         INCW    X
      008153 BF 1D            [ 2]   64         LDW     TICKCNT,X
                                     65         ; fall through
                                     66 
                           000000    67         .ifne   BG_RUNMASK
                                     68         LD      A,XL            ; Background task runs if "(BG_RUNMASK AND TICKCNT) equals 0"
                                     69         AND     A,#BG_RUNMASK
                                     70         JRNE    TIM2IRET
                                     71         .endif
                                     72 
      008155 90 BE 1B         [ 2]   73         LDW     Y,BGADDR        ; address of background task
      008158 90 5D            [ 2]   74         TNZW    Y               ; 0: background operation off
      00815A 27 21            [ 1]   75         JREQ    TIM2IRET
                                     76 
      00815C BE 08            [ 2]   77         LDW     X,XREG0         ; Save context
      00815E 89               [ 2]   78         PUSHW   X
                                     79 
      00815F BE 00            [ 2]   80         LDW     X,USREMIT       ; save EMIT exection vector
      008161 89               [ 2]   81         PUSHW   X
      008162 AE 83 13         [ 2]   82         LDW     X,#EMIT_BG      ; "'BGEMIT" xt of EMIT for BG task
      008165 BF 00            [ 2]   83         LDW     USREMIT,X
                                     84 
      008167 BE 02            [ 2]   85         LDW     X,USRQKEY       ; save QKEY exection vector
      008169 89               [ 2]   86         PUSHW   X
      00816A AE 83 7D         [ 2]   87         LDW     X,#QKEY_BG      ; "'?BGKEY" xt of ?KEY for BG task
      00816D BF 02            [ 2]   88         LDW     USRQKEY,X
                                     89 
                                     90 ;        LDW     X,USRHLD
                                     91 ;        PUSHW   X
                                     92 ;        LDW     X,#PADBG        ; "BGPAD" empty PAD for BG task
                                     93 ;        LDW     USRHLD,X
                                     94 
      00816F AE 07 D0         [ 2]   95         LDW     X,#BSPP         ; "BGSPP" data stack for BG task
      008172 90 FD            [ 4]   96         CALL    (Y)
                                     97 
                                     98 ;        POPW    X
                                     99 ;        LDW     USRHLD,X
                                    100 
      008174 85               [ 2]  101         POPW    X
      008175 BF 02            [ 2]  102         LDW     USRQKEY,X
                                    103 
      008177 85               [ 2]  104         POPW    X
      008178 BF 00            [ 2]  105         LDW     USREMIT,X
                                    106 
      00817A 85               [ 2]  107         POPW    X
      00817B BF 08            [ 2]  108         LDW     XREG0,X
      00817D                        109 TIM2IRET:
                                    110         .endif
                                    111 
      00817D 80               [11]  112         IRET
                                    113         .endif
                                    114 
                                    115        ;******  BG User Words  ******
                                    116 
                           000001   117         .ifne   HAS_BACKGROUND
                                    118 ;       TIM     ( -- T)     ( TOS STM8: -- Y,Z,N )
                                    119 ;       Return TICKCNT as timer
                                    120 
      00817E                        121         HEADER  TIMM "TIM"
      00817E                        122 TIMM:
      00817E 90 BE 1D         [ 2]  123         LDW     Y,TICKCNT
      008181 CC 83 82         [ 2]  124         JP      AYSTOR
                                    125 
                                    126         .endif
                                    532 ; UPPLOC = RAMPOOL + 30  ; PAD in Background task, growing down, 32 bytes
                                    533 
                                    534 ; ==============================================
      008184                        535         HEADER  RETURN "RETURN"
      008184 81               [ 4]  536 RETURN: RET
                                    537 
                                    538 ;       Configuation table with shadow data for RESET
                                    539 
                                    540 ;       Main entry points and COLD start data
                                    541 
                                    542 
      008185                        543 _forth:                         ; SDCC entry
                                    544 ;       Note: no return to main.c possible unless RAMEND equals SP,
                                    545 ;       and RPP init skipped
                                    546 
                                    547 ;       COLD    ( -- )
                                    548 ;       The hilevel cold start sequence.
                                    549 
      008185                        550         HEADER  COLD "COLD"
      008185                        551 COLD:
      008185 9B               [ 1]  552         SIM                     ; disable interrupts
      008186 35 00 50 C0      [ 1]  553         MOV     CLK_CKDIVR,#0   ; Clock divider register
                                    554 
      00818A AE 07 FF         [ 2]  555         LDW     X,#(RAMEND-FORTHRAM)
      00818D 6F 00            [ 1]  556 1$:     CLR     (FORTHRAM,X)
      00818F 5A               [ 2]  557         DECW    X
      008190 2A FB            [ 1]  558         JRPL    1$
                                    559 
      008192 AE 07 FF         [ 2]  560         LDW     X,#RPP          ; return stack, growing down
      008195 94               [ 1]  561         LDW     SP,X            ; initialize return stack
                                    562 
                                    563         ; see "boardcore.inc")
      008196 CD 81 38         [ 4]  564         CALL    BOARDINIT       ; "PC_BOARDINIT" Board initialization
                                    565 
      008199                        566         BGTASK_Init             ; macro for init of BG task timer, refer to bgtask.inc
                           000001     1         .ifne   HAS_BACKGROUND
                                      2 
                           000000     3         .ifne   BG_USE_TIM1
                                      4         BG_INT = ITC_IX_TIM1
                                      5         MOV     TIM1_PSCRL,#7   ; prescaler 1/(7+1) = 1/8
                           000001     6         .else
                           000000     7         .ifne   BG_USE_TIM3
                                      8         BG_INT = ITC_IX_TIM3
                           000001     9         .else
                           000013    10         BG_INT = ITC_IX_TIM2
                                     11         .endif
      008199 35 03 52 5E      [ 1]   12         MOV     BG_TIM_PSCR,#3  ; prescaler 1/(2^3) = 1/8
                                     13         .endif
      00819D 72 1F 7F 74      [ 1]   14         BRES    ITC_SPR1+(BG_INT/4),#((BG_INT%4)*2+1)  ; Interrupt prio. low
                                     15 
      0081A1 35 26 52 5F      [ 1]   16         MOV     BG_TIM_ARRH,#(BG_TIM_REL/256)  ; reload H
      0081A5 35 DE 52 60      [ 1]   17         MOV     BG_TIM_ARRL,#(BG_TIM_REL%256)  ;        L
      0081A9 35 01 52 50      [ 1]   18         MOV     BG_TIM_CR1,#0x01 ; enable background timer
      0081AD 35 01 52 55      [ 1]   19         MOV     BG_TIM_IER,#0x01 ; enable background timer interrupt
                                     20         .endif
                                    567 
                           000001   568         .ifne   HAS_RXUART+HAS_TXUART
                                    569         ; Init RS232 communication port
                                    570         ; STM8S[01]003F3 init UART
      0081B1 AE 08 0B         [ 2]  571         LDW     X,#CUARTBRR      ; "UARTBRR" def. $6803 / 9600 baud
      0081B4 CF 52 32         [ 2]  572         LDW     UART_BRR1,X
                           000001   573         .ifne   HAS_RXUART*HAS_TXUART
      0081B7 35 2C 52 35      [ 1]  574         MOV     UART_CR2,#0x2C  ; Use UART1 full duplex + RXNE interrupt
                           000000   575 .ifeq (FAMILY - STM8S)
                                    576         MOV    ITC_SPR5,#0xCF   ; enable TIM2 interrupts while chatting
                           000001   577 .else
      0081BB 35 0C 7F 77      [ 1]  578         MOV    ITC_SPR8,#0xC   ; enable TIM2 interrupts while chatting
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
      0081BF                        619         SSER_Init               ; macro for init of simulated serial, refer to sser.inc
                                    620 
      0081BF                        621         Board_IO_Init           ; macro board_io initialization (7S-LED)
                                      1         ; no LED-7Seg
                                    622 
      0081BF AE 07 A0         [ 2]  623         LDW     X,#SPP          ; initialize data stack, TIB
                                    624         
      0081C2 5A               [ 2]  625         DECW X                  ; initialise get bit / set bit routines in ram
      0081C3 5A               [ 2]  626         DECW X  
      0081C4 90 AE 81 29      [ 2]  627         LDW Y,#COLD1
      0081C8 FF               [ 2]  628         LDW (X),Y
      0081C9 5A               [ 2]  629         DECW X  
      0081CA 5A               [ 2]  630         DECW X  
      0081CB 90 AE 00 0C      [ 2]  631         LDW Y,#BITAT  
      0081CF FF               [ 2]  632         LDW (X),Y
      0081D0 5A               [ 2]  633         DECW X  
      0081D1 5A               [ 2]  634         DECW X  
      0081D2 90 AE 00 0F      [ 2]  635         LDW Y,#15
      0081D6 FF               [ 2]  636         LDW (X),Y
      0081D7 CD 85 F4         [ 4]  637         CALL CMOVE
                                    638 
                                    639 
                           000000   640 .if  HAS_RXSIM
                                    641 MOV USR_5,#255
                                    642 .endif
                                    643 
                                    644         ; Hardware initialization complete
      0081DA 9A               [ 1]  645         RIM                     ; enable interrupts
                                    646 
      0081DB CD 81 84         [ 4]  647 TBOOT:  CALL    RETURN       ; application boot, can be changed with ' appl 'BOOT flash!
      0081DE 8F               [10]  648 SLEEP:     WFI
                           000000   649 .if HAS_RXSIM
                                    650         TNZ USR_6
                                    651         JREQ $1
                                    652         CALL CHAT
                                    653 $1:    JRA      SLEEP
                                    654 .endif
                                    655 
                           000001   656 .if    HAS_RXUART*HAS_TXUART       
      0081DF 20 FD            [ 2]  657         JRA      SLEEP
      0081E1                        658 UART_INT:
      0081E1 CD 81 0D         [ 4]  659         CALL CHAT                 ; during chat data SP is communicated by muforth
      0081E4 1F 03            [ 2]  660         LDW (#3,SP),X             ; X (data SP) is popped from return stack during IRET
      0081E6 80               [11]  661         IRET
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
      0081E7                        672         HEADER  QRX "?RX"
      0081E7                        673 QRX:
      0081E7 4F               [ 1]  674         CLR     A               ; A: flag false
      0081E8 72 0B 52 30 03   [ 2]  675         BTJF    UART_SR,#5,1$
      0081ED C6 52 31         [ 1]  676         LD      A,UART_DR      ; get char in A
      0081F0                        677 1$:
      0081F0 CC 83 7E         [ 2]  678      JP      ASTOR          ; push char
                                    679         .endif
                                    680 
                           000001   681         .ifne   HAS_TXUART
                                    682 ;       TX!     ( c -- )
                                    683 ;       Send character c to the serial interface.
                                    684 
      0081F3                        685         HEADER  TXSTOR "TX!"
      0081F3                        686 TXSTOR:
      0081F3 5C               [ 1]  687         INCW    X
      0081F4 F6               [ 1]  688         LD      A,(X)
      0081F5 5C               [ 1]  689         INCW    X
                                    690 
      0081F6                        691         HEADER  TXASTOR "TXA!"
      0081F6                        692 TXASTOR:
                           000000   693         .ifne   HALF_DUPLEX
                                    694         ; HALF_DUPLEX with normal UART (e.g. wired-or Rx and Tx)
                                    695 1$:     BTJF    UART_SR,#7,1$  ; loop until tdre
                                    696         BRES    UART_CR2,#2    ; disable rx
                                    697         LD      UART_DR,A      ; send A
                                    698 2$:     BTJF    UART_SR,#6,2$  ; loop until tc
                                    699         BSET    UART_CR2,#2    ; enable rx
                           000001   700         .else                  ; not HALF_DUPLEX
      0081F6 72 0F 52 30 FB   [ 2]  701 1$:     BTJF    UART_SR,#7,1$  ; loop until tdre
      0081FB C7 52 31         [ 1]  702         LD      UART_DR,A      ; send A
                                    703         .endif
      0081FE 81               [ 4]  704         RET
                                    705         .endif
                                    706 
                                    707 ; ==============================================
                                    708 
                                    709 ;       Device independent I/O
                                    710 
                                    711 ;       ?KEY    ( -- c T | F )  ( TOS STM8: -- Y,Z,N )
                                    712 ;       Return input char and true, or false.
      0081FF                        713         HEADER  QKEY "?KEY"
      0081FF                        714 QKEY:
      0081FF 92 CC 02         [ 5]  715         JP      [USRQKEY]
                                    716 
                                    717 ;       EMIT    ( c -- )
                                    718 ;       Send character c to output device.
                                    719 
      008202                        720         HEADER  EMIT "EMIT"
      008202                        721 EMIT:
      008202 92 CC 00         [ 5]  722         JP      [USREMIT]
                                    723 
                                    724 ; ==============================================
                                    725 ; The kernel
                                    726 
                                    727 ;       ?branch ( f -- )
                                    728 ;       Branch if flag is zero.
                                    729 
      008205                        730         HEADFLG QBRAN "?branch" COMPO
                                      1 
      008205                        731 QBRAN:
      008205 90 93            [ 1]  732         LDW     Y,X
      008207 5C               [ 1]  733         INCW    X
      008208 5C               [ 1]  734         INCW    X
      008209 90 FE            [ 2]  735         LDW     Y,(Y)
      00820B 27 05            [ 1]  736         JREQ    BRAN
      00820D                        737 WSKIPRET:
      00820D 90 85            [ 2]  738         POPW    Y
      00820F 90 EC 02         [ 2]  739         JP      (2,Y)
                                    740 
                                    741 
                                    742 ;       branch  ( -- )
                                    743 ;       Branch to an inline address.
                                    744 
      008212                        745         HEADFLG BRAN "branch" COMPO    ; NOALIAS
                                      1 
      008212                        746 BRAN:
      008212 90 85            [ 2]  747         POPW    Y
      008214 90 FE            [ 2]  748         LDW     Y,(Y)
      008216 90 FC            [ 2]  749         JP      (Y)
                                    750 
                                    751 
                                    752 ;       EXECUTE ( ca -- )
                                    753 ;       Execute word at ca.
                                    754 
      008218                        755         HEADER  EXECU "EXECUTE"
      008218                        756 EXECU:
      008218 90 93            [ 1]  757         LDW     Y,X
      00821A 5C               [ 1]  758         INCW    X
      00821B 5C               [ 1]  759         INCW    X
      00821C 90 FE            [ 2]  760         LDW     Y,(Y)
      00821E 90 FC            [ 2]  761         JP      (Y)
                                    762 
                           000001   763         .ifeq   BOOTSTRAP
                                    764 ;       2!      ( d a -- )      ( TOS STM8: -- Y,Z,N )
                                    765 ;       Store double integer to address a.
                                    766 
      008220                        767         HEADER  DSTOR "2!"
      008220                        768 DSTOR:
      008220 90 93            [ 1]  769         LDW Y,X
      008222 90 FE            [ 2]  770         LDW Y,(Y)
      008224 E6 04            [ 1]  771         LD A,(4,X)
      008226 90 F7            [ 1]  772         LD (Y),A
      008228 E6 05            [ 1]  773         LD A,(5,X)
      00822A 90 E7 01         [ 1]  774         LD (1,Y),A
      00822D E6 02            [ 1]  775         LD A,(2,X)
      00822F 90 E7 02         [ 1]  776         LD (2,Y),A
      008232 E6 03            [ 1]  777         LD A,(3,X)
      008234 90 E7 03         [ 1]  778         LD (3,Y),A
      008237 1C 00 06         [ 2]  779         ADDW X,#6
      00823A 81               [ 4]  780         RET
                                    781         .endif
                                    782 
                                    783 ;       2@      ( a -- d )
                                    784 ;       Fetch double integer from address a.
                                    785 
      00823B                        786         HEADER  DAT "2@"
      00823B                        787 DAT:
      00823B 90 93            [ 1]  788         LDW Y,X
      00823D FE               [ 2]  789         LDW X,(X)
      00823E F6               [ 1]  790         LD A,(X)
      00823F 51               [ 1]  791         EXGW X,Y
      008240 F7               [ 1]  792         LD (X),A
      008241 90 E6 01         [ 1]  793         LD A,(1,Y)
      008244 E7 01            [ 1]  794         LD (1,X),A
      008246 5A               [ 2]  795         DECW X
      008247 90 E6 03         [ 1]  796         LD A,(3,Y)
      00824A F7               [ 1]  797         LD (X),A
      00824B 5A               [ 2]  798         DECW X
      00824C 90 E6 02         [ 1]  799         LD A,(2,Y)
      00824F F7               [ 1]  800         LD (X),A
      008250 81               [ 4]  801         RET
                                    802 
                                    803 ;       2C!  ( n a -- )
                                    804 ;       Store word C-wise to 16 bit HW registers "MSB first"
                                    805 
      008251                        806         HEADER  DCSTOR "2C!"
      008251                        807 DCSTOR:
      008251 90 93            [ 1]  808         LDW     Y,X
      008253 5C               [ 1]  809         INCW    X
      008254 5C               [ 1]  810         INCW    X
      008255 90 FE            [ 2]  811         LDW     Y,(Y)
      008257 F6               [ 1]  812         LD      A,(X)
      008258 90 F7            [ 1]  813         LD      (Y),A           ; write MSB(n) to a
      00825A 5C               [ 1]  814         INCW    X
      00825B F6               [ 1]  815         LD      A,(X)
      00825C 90 E7 01         [ 1]  816         LD      (1,Y),A         ; write LSB(n) to a+1
      00825F 5C               [ 1]  817         INCW    X
      008260 81               [ 4]  818         RET
                                    819 
                                    820 ;       2C@  ( a -- n )
                                    821 ;       Fetch word C-wise from 16 bit HW config. registers "MSB first"
                                    822 
      008261                        823         HEADER  DCAT "2C@"
      008261                        824 DCAT:
      008261 90 93            [ 1]  825         LDW     Y,X
      008263 FE               [ 2]  826         LDW     X,(X)
      008264 F6               [ 1]  827         LD      A,(X)
      008265 90 F7            [ 1]  828         LD      (Y),A
      008267 E6 01            [ 1]  829         LD      A,(1,X)
      008269 51               [ 1]  830         EXGW    X,Y
      00826A E7 01            [ 1]  831         LD      (1,X),A
      00826C 81               [ 4]  832         RET
                                    833 
                                    834 ;       BF@ ( a u -- 0|1)
                                    835 ;       Read bit #u (0..2047) in a cell array (16 bit words) at address a
                                    836 ;       Note: fills BITAT RAM-routine with stack values and jumps to BITAT
      00826D                        837         HEADER  BFAT "BF@"
      00826D                        838 BFAT:
      00826D 90 93            [ 1]  839         LDW Y, X
      00826F FE               [ 2]  840         LDW X, (X)
      008270 A6 08            [ 1]  841         LD A,#8
      008272 62               [ 2]  842         DIV X,A
      008273 48               [ 1]  843         SLL A
      008274 4C               [ 1]  844         INC A
      008275 B7 0E            [ 1]  845         LD BITAT+2,A
      008277 9F               [ 1]  846         LD A,XL
      008278 A8 01            [ 1]  847         XOR A,#01
      00827A B7 09            [ 1]  848         LD XREG0+1,A
      00827C 3F 08            [ 1]  849         CLR XREG0
      00827E 93               [ 1]  850         LDW X,Y
      00827F EE 02            [ 2]  851         LDW X,(02,X)
      008281 72 BB 00 08      [ 2]  852         ADDW X,XREG0
      008285 BF 0F            [ 2]  853         LDW BITAT+3,X
      008287 51               [ 1]  854         EXGW X,Y
      008288 5C               [ 1]  855         INCW X
      008289 5C               [ 1]  856         INCW X
      00828A CC 00 0C         [ 2]  857         JP BITAT
                                    858 
                                    859 ;       BF! ( a u -- 0|1)
                                    860 ;       Write bit to a bitfield stored in one or more cells (16 bit words)
                                    861 ;       Set/reset bit #u (0..8191) in a cell array starting at address a to bool t
      00828D                        862         HEADER  BFSTO "BF!"
      00828D                        863 BFSTO:
      00828D 90 93            [ 1]  864         LDW Y,X
      00828F FE               [ 2]  865         LDW X,(X)
      008290 9F               [ 1]  866         LD A,XL
      008291 A4 07            [ 1]  867         AND A,#7
      008293 88               [ 1]  868         PUSH A
      008294 9F               [ 1]  869         LD A,XL
      008295 47               [ 1]  870         SRA A
      008296 47               [ 1]  871         SRA A
      008297 47               [ 1]  872         SRA A
      008298 A8 01            [ 1]  873         XOR A,#1
      00829A 93               [ 1]  874         LDW X,Y
      00829B EB 03            [ 1]  875         ADD A,(3,X)
      00829D E7 03            [ 1]  876         LD (3,X),A
      00829F A6 00            [ 1]  877         LD A,#0
      0082A1 E9 02            [ 1]  878         ADC A,(2,X)
      0082A3 E7 02            [ 1]  879         LD (2,X),A
      0082A5 84               [ 1]  880         POP A
      0082A6 7F               [ 1]  881         CLR (X)
      0082A7 E7 01            [ 1]  882         LD (1,X),A         ; fall through
                                    883 
                                    884 ;       B! ( t a u -- )
                                    885 ;       Set/reset bit #u (0..7) in the byte at address a to bool t
                                    886 ;       Note: executes BSER/BRES + RET code in RAM
      0082A9                        887         HEADER  BRSS "B!"
      0082A9                        888 BRSS:
      0082A9 E6 01            [ 1]  889         LD A,(1,X)
      0082AB 48               [ 1]  890         SLL A                                     
      0082AC AA 10            [ 1]  891         OR A,#16                                
      0082AE B7 17            [ 1]  892         ld BITSTO+1,A                                  
      0082B0 E6 05            [ 1]  893         LD A,(05,X)                             
      0082B2 26 02            [ 1]  894         JRNE 1$                       
      0082B4 3C 17            [ 1]  895         INC BITSTO+1                                   
      0082B6 E6 02            [ 1]  896 1$:     LD A,(02,X)                             
      0082B8 B7 18            [ 1]  897         LD BITSTO+2,A                                  
      0082BA E6 03            [ 1]  898         LD A,(03,X)                             
      0082BC B7 19            [ 1]  899         LD BITSTO+3,A                                  
      0082BE 1C 00 06         [ 2]  900         ADDW X,#6                               
      0082C1 CC 00 16         [ 2]  901         JP BITSTO                                    
                                    902 
                                    903 ;       B@ ( a u -- )
                                    904 ;       Get bit #u (0..7) in the byte at address a
                                    905 ;       Note: executes BSER/BRES + RET code in RAM
      0082C4                        906         HEADER  BAT "B@"
      0082C4                        907 BAT:
      0082C4 E6 01            [ 1]  908         LD A,(1,X)
      0082C6 48               [ 1]  909         SLA A
      0082C7 4C               [ 1]  910         INC A
      0082C8 B7 0E            [ 1]  911         LD BITAT+2,A
      0082CA E6 02            [ 1]  912         LD A,(2,X)
      0082CC B7 0F            [ 1]  913         LD BITAT+3,A
      0082CE E6 03            [ 1]  914         LD A,(3,X)
      0082D0 B7 10            [ 1]  915         LD BITAT+4,A
      0082D2 5C               [ 1]  916         INCW X
      0082D3 5C               [ 1]  917         INCW X
      0082D4 7F               [ 1]  918         CLR (X)
      0082D5 CC 00 0C         [ 2]  919         JP BITAT
                                    920 
                                    921 ;       @       ( a -- w )      ( TOS STM8: -- Y,Z,N )
                                    922 ;       Push memory location to stack.
                                    923 
      0082D8                        924         HEADER  AT "@"
      0082D8                        925 AT:
      0082D8 90 93            [ 1]  926         LDW     Y,X
      0082DA FE               [ 2]  927         LDW     X,(X)
      0082DB FE               [ 2]  928         LDW     X,(X)
      0082DC 51               [ 1]  929         EXGW    X,Y
      0082DD FF               [ 2]  930         LDW     (X),Y
      0082DE 81               [ 4]  931         RET
                                    932 
                                    933 ;       !       ( w a -- )      ( TOS STM8: -- Y,Z,N )
                                    934 ;       Pop data stack to memory.
                                    935 
      0082DF                        936         HEADER  STORE "!"
      0082DF                        937 STORE:
      0082DF 90 93            [ 1]  938         LDW     Y,X             ; (14 bytes, 16 cy)
      0082E1 5C               [ 1]  939         INCW    X
      0082E2 5C               [ 1]  940         INCW    X
      0082E3 90 FE            [ 2]  941         LDW     Y,(Y)
      0082E5 89               [ 2]  942         PUSHW   X
      0082E6 FE               [ 2]  943         LDW     X,(X)           ; w
      0082E7 90 FF            [ 2]  944         LDW     (Y),X
      0082E9 85               [ 2]  945         POPW    X
      0082EA 5C               [ 1]  946         INCW    X
      0082EB 5C               [ 1]  947         INCW    X
      0082EC 81               [ 4]  948         RET
                                    949 
                                    950 ;       C@      ( a -- c )      ( TOS STM8: -- A,Z,N )
                                    951 ;       Push byte in memory to stack.
                                    952 ;       STM8: Z,N
                                    953 
      0082ED                        954         HEADER  CAT "C@"
      0082ED                        955 CAT:
      0082ED 90 93            [ 1]  956         LDW     Y,X             ; Y=a
      0082EF 90 FE            [ 2]  957         LDW     Y,(Y)
      0082F1                        958 YCAT:
      0082F1 90 F6            [ 1]  959         LD      A,(Y)
      0082F3 7F               [ 1]  960         CLR     (X)
      0082F4 E7 01            [ 1]  961         LD      (1,X),A
      0082F6 81               [ 4]  962         RET
                                    963 
                                    964 ;       C!      ( c a -- )
                                    965 ;       Pop     data stack to byte memory.
                                    966 
      0082F7                        967         HEADER  CSTOR "C!"
      0082F7                        968 CSTOR:
      0082F7 90 93            [ 1]  969         LDW     Y,X
      0082F9 5C               [ 1]  970         INCW    X
      0082FA 5C               [ 1]  971         INCW    X
      0082FB 90 FE            [ 2]  972         LDW     Y,(Y)
      0082FD 5C               [ 1]  973         INCW    X
      0082FE F6               [ 1]  974         LD      A,(X)
      0082FF 90 F7            [ 1]  975         LD      (Y),A
      008301 5C               [ 1]  976         INCW    X
      008302 81               [ 4]  977         RET
                                    978 
                                    979 ;       OVER    ( w1 w2 -- w1 w2 w1 ) ( TOS STM8: -- Y,Z,N )
                                    980 ;       Copy second stack item to top.
                                    981 
      008303                        982         HEADER  OVER "OVER"
      008303                        983 OVER:
      008303 E6 03            [ 1]  984         LD A,(3,X)
      008305 5A               [ 2]  985         DECW X
      008306 F7               [ 1]  986         LD (X),A
      008307 E6 03            [ 1]  987         LD A,(3,X)
      008309 5A               [ 2]  988         DECW X
      00830A F7               [ 1]  989         LD (X),A
      00830B 81               [ 4]  990         RET
                                    991 
                                    992 ;       NIP     ( n1 n2 -- n2 )
                                    993 ;       Drop 2nd item on the stack.
                                    994 
      00830C                        995         HEADER  NIP "NIP"
      00830C                        996 NIP:
      00830C F6               [ 1]  997         LD A,(X)  
      00830D E7 02            [ 1]  998         LD (2,X),A  
      00830F E6 01            [ 1]  999         LD A,(1,X)  
      008311 E7 03            [ 1] 1000         LD (3,X),A  ; fall through
                                   1001 
                                   1002 ;       DROP     ( n1 -- )
                                   1003 ;       Drop top stack item.
                                   1004 
      008313                       1005         HEADER  DROP "DROP"
      008313 5C               [ 1] 1006 DROP:   INCW    X
      008314 5C               [ 1] 1007         INCW    X
      008315 81               [ 4] 1008         RET
                                   1009 
                                   1010 ;       DUP     ( w -- w w )    ( TOS STM8: -- Y,Z,N )
                                   1011 ;       Duplicate top stack item.
                                   1012 
      008316                       1013         HEADER  DUPP "DUP"
      008316                       1014 DUPP:
      008316 90 93            [ 1] 1015         LDW     Y,X
      008318 90 FE            [ 2] 1016         LDW     Y,(Y)
      00831A 5A               [ 2] 1017         DECW    X               ; SUBW  X,#2
      00831B 5A               [ 2] 1018         DECW    X
      00831C FF               [ 2] 1019         LDW     (X),Y           ; push on stack
      00831D 81               [ 4] 1020         RET
                                   1021 
                                   1022 ;       SWAP ( w1 w2 -- w2 w1 ) ( TOS STM8: -- Y,Z,N )
                                   1023 ;       Exchange top two stack items.
                                   1024 
      00831E                       1025         HEADER  SWAPP "SWAP"
      00831E                       1026 SWAPP:
      00831E 90 93            [ 1] 1027         LDW     Y,X
      008320 EE 02            [ 2] 1028         LDW     X,(2,X)
      008322 89               [ 2] 1029         PUSHW   X
      008323 93               [ 1] 1030         LDW     X,Y
      008324 FE               [ 2] 1031         LDW     X,(X)
      008325 51               [ 1] 1032         EXGW    X,Y
      008326 EF 02            [ 2] 1033         LDW     (2,X),Y
      008328 90 85            [ 2] 1034         POPW    Y
      00832A FF               [ 2] 1035         LDW     (X),Y
      00832B 81               [ 4] 1036         RET
                                   1037 
                                   1038 ;       UM+     ( u u -- udsum )
                                   1039 ;       Add two unsigned single
                                   1040 ;       and return a double sum.
                                   1041 
      00832C                       1042         HEADER  UPLUS "UM+"
      00832C                       1043 UPLUS:
      00832C AD 05            [ 4] 1044         CALLR   PLUS
      00832E 4F               [ 1] 1045         CLR     A
      00832F 49               [ 1] 1046         RLC     A
      008330 CC 83 7E         [ 2] 1047         JP      ASTOR
                                   1048 
                                   1049 ;       +       ( w w -- sum ) ( TOS STM8: -- Y,Z,N )
                                   1050 ;       Add top two items.
                                   1051 
      008333                       1052         HEADER  PLUS "+"
                                   1053 
      008333                       1054 PLUS:
      008333 E6 01            [ 1] 1055         LD      A,(1,X) ;D=w
      008335 EB 03            [ 1] 1056         ADD     A,(3,X)
      008337 E7 03            [ 1] 1057         LD      (3,X),A
      008339 F6               [ 1] 1058         LD      A,(X)
      00833A E9 02            [ 1] 1059         ADC     A,(2,X)
      00833C                       1060 LDADROP:
      00833C 5C               [ 1] 1061         INCW    X
      00833D 5C               [ 1] 1062         INCW    X
      00833E F7               [ 1] 1063         LD      (X),A
      00833F 81               [ 4] 1064         RET
                                   1065 
                                   1066 ;       XOR     ( w w -- w )    ( TOS STM8: -- Y,Z,N )
                                   1067 ;       Bitwise exclusive OR.
                                   1068 
      008340                       1069         HEADER  XORR "XOR"
      008340                       1070 XORR:
      008340 E6 01            [ 1] 1071         LD      A,(1,X)         ; D=w
      008342 E8 03            [ 1] 1072         XOR     A,(3,X)
      008344 E7 03            [ 1] 1073         LD      (3,X),A
      008346 F6               [ 1] 1074         LD      A,(X)
      008347 E8 02            [ 1] 1075         XOR     A,(2,X)
      008349 20 F1            [ 2] 1076         JRA     LDADROP
                                   1077 
                                   1078 ;       AND     ( w w -- w )    ( TOS STM8: -- Y,Z,N )
                                   1079 ;       Bitwise AND.
                                   1080 
      00834B                       1081         HEADER  ANDD "AND"
      00834B                       1082 ANDD:
      00834B E6 01            [ 1] 1083         LD      A,(1,X)         ; D=w
      00834D E4 03            [ 1] 1084         AND     A,(3,X)
      00834F E7 03            [ 1] 1085         LD      (3,X),A
      008351 F6               [ 1] 1086         LD      A,(X)
      008352 E4 02            [ 1] 1087         AND     A,(2,X)
      008354 20 E6            [ 2] 1088         JRA     LDADROP
                                   1089 
                                   1090 ;       OR      ( w w -- w )    ( TOS STM8: -- immediate Y,Z,N )
                                   1091 ;       Bitwise inclusive OR.
                                   1092 
      008356                       1093         HEADER  ORR "OR"
      008356                       1094 ORR:
      008356 E6 01            [ 1] 1095         LD      A,(1,X)         ; D=w
      008358 EA 03            [ 1] 1096         OR      A,(3,X)
      00835A E7 03            [ 1] 1097         LD      (3,X),A
      00835C F6               [ 1] 1098         LD      A,(X)
      00835D EA 02            [ 1] 1099         OR      A,(2,X)
      00835F 20 DB            [ 2] 1100         JRA     LDADROP
                                   1101 
                                   1102 ;       0<      ( n -- t ) ( TOS STM8: -- A,Z )
                                   1103 ;       Return true if n is negative.
                                   1104 
      008361                       1105         HEADER  ZLESS "0<"
      008361                       1106 ZLESS:
      008361 4F               [ 1] 1107         CLR     A
      008362 7D               [ 1] 1108         TNZ     (X)
      008363 2A 01            [ 1] 1109         JRPL    ZL1
      008365 43               [ 1] 1110         CPL     A               ; true
      008366 F7               [ 1] 1111 ZL1:    LD      (X),A
      008367 E7 01            [ 1] 1112         LD      (1,X),A
      008369 81               [ 4] 1113         RET
                                   1114 
                                   1115 ;       -   ( n1 n2 -- n1-n2 )  ( TOS STM8: -- Y,Z,N )
                                   1116 ;       Subtraction.
                                   1117 
      00836A                       1118         HEADER  SUBB "-"
                                   1119 
      00836A                       1120 SUBB:
                           000000  1121         .ifeq   SPEEDOVERSIZE
                                   1122         CALL    NEGAT           ; (15 cy)
                                   1123         JRA     PLUS            ; 25 cy (15+10)
                           000001  1124         .else
      00836A 90 93            [ 1] 1125         LDW     Y,X
      00836C 90 FE            [ 2] 1126         LDW     Y,(Y)
      00836E 90 BF 08         [ 2] 1127         LDW     XREG0,Y
      008371 5C               [ 1] 1128         INCW    X
      008372 5C               [ 1] 1129         INCW    X
      008373 90 93            [ 1] 1130         LDW     Y,X
      008375 90 FE            [ 2] 1131         LDW     Y,(Y)
      008377 72 B2 00 08      [ 2] 1132         SUBW    Y,XREG0
      00837B FF               [ 2] 1133         LDW     (X),Y
      00837C 81               [ 4] 1134         RET                     ; 18 cy
                                   1135         .endif
                                   1136 
      00837D                       1137 ZERO:
      00837D 4F               [ 1] 1138         CLR A
                                   1139 
                                   1140 ;       A>  ( -- n )     ( TOS STM8: - Y,Z,N )
                                   1141 ;       push A to stack
                                   1142 
      00837E                       1143         HEADER  ASTOR "A>"
      00837E                       1144 ASTOR:
      00837E 90 5F            [ 1] 1145         CLRW    Y
      008380 90 97            [ 1] 1146         LD      YL,A
      008382                       1147 AYSTOR:
      008382 5A               [ 2] 1148         DECW    X               ; SUBW  X,#2
      008383 5A               [ 2] 1149         DECW    X
      008384 FF               [ 2] 1150         LDW     (X),Y           ; push on stack
      008385 81               [ 4] 1151         RET
                                   1152 
                                   1153 ;       ATOKEY core ( -- c T | f )    ( TOS STM8: - Y,Z,N )
                                   1154 ;       Return input char and true, or false.
                                   1155 
      008386                       1156         HEADER  ATOKEY "A>KEY"
      008386                       1157 ATOKEY:
      008386 4D               [ 1] 1158         TNZ     A
      008387 27 F5            [ 1] 1159         JREQ    ASTOR
      008389 AD F3            [ 4] 1160         CALLR   ASTOR              ; push char
      00838B 5A               [ 2] 1161         DECW X
      00838C 5A               [ 2] 1162         DECW X
      00838D 90 AE 0F FF      [ 2] 1163         LDW Y,#0XFFF
      008391 FF               [ 2] 1164         LDW (X),Y
      008392 81               [ 4] 1165         RET
                                   1166 
                                   1167 ; Common functions
                                   1168 
                                   1169 ;       ?DUP    ( w -- w w | 0 )   ( TOS STM8: -- Y,Z,N )
                                   1170 ;       Dup tos if its not zero.
      008393                       1171         HEADER  QDUP "?DUP"
      008393                       1172 QDUP:
      008393 90 93            [ 1] 1173         LDW     Y,X
      008395 90 FE            [ 2] 1174         LDW     Y,(Y)
      008397 27 03            [ 1] 1175         JREQ    QDUP1
      008399 5A               [ 2] 1176         DECW    X
      00839A 5A               [ 2] 1177         DECW    X
      00839B FF               [ 2] 1178         LDW     (X),Y
      00839C 81               [ 4] 1179 QDUP1:  RET
                                   1180 
                                   1181 ;       ROT     ( w1 w2 w3 -- w2 w3 w1 ) ( TOS STM8: -- Y,Z,N )
                                   1182 ;       Rot 3rd item to top.
                                   1183 
      00839D                       1184         HEADER  ROT "ROT"
      00839D                       1185 ROT:
                           000001  1186 .if 1
                                   1187 ; 31 bytes, 20 cy
      00839D F6               [ 1] 1188 LD A,(X)
      00839E B7 08            [ 1] 1189 LD XREG0,A
      0083A0 E6 01            [ 1] 1190 LD A,(1,X)
      0083A2 B7 09            [ 1] 1191 LD XREG0+1,A
      0083A4 E6 04            [ 1] 1192 LD A,(4,X)
      0083A6 F7               [ 1] 1193 LD (X),A
      0083A7 E6 05            [ 1] 1194 LD A,(5,X)
      0083A9 E7 01            [ 1] 1195 LD (1,X),A
      0083AB E6 02            [ 1] 1196 LD A,(2,X)
      0083AD E7 04            [ 1] 1197 LD (4,X),A
      0083AF E6 03            [ 1] 1198 LD A,(3,X)
      0083B1 E7 05            [ 1] 1199 LD (5,X),A
      0083B3 B6 08            [ 1] 1200 LD A,XREG0
      0083B5 E7 02            [ 1] 1201 LD (2,X),A
      0083B7 B6 09            [ 1] 1202 LD A,XREG0+1
      0083B9 E7 03            [ 1] 1203 LD (3,X),A
      0083BB 81               [ 4] 1204 RET
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
      0083BC                       1241         HEADER  DDUP "2DUP"
      0083BC                       1242 DDUP:
      0083BC AD 00            [ 4] 1243         CALLR    1$
      0083BE                       1244 1$:
      0083BE CC 83 03         [ 2] 1245         JP      OVER
                                   1246 
                           000001  1247         .ifeq   UNLINKCORE
                                   1248 ;       DNEGATE ( d -- -d )     ( TOS STM8: -- Y,Z,N )
                                   1249 ;       Two's complement of top double.
                                   1250 
      0083C1                       1251         HEADER  DNEGA "DNEGATE"
      0083C1                       1252 DNEGA:
      0083C1 90 93            [ 1] 1253         LDW     Y,X
      0083C3 90 EE 02         [ 2] 1254         LDW     Y,(2,Y)
      0083C6 90 50            [ 2] 1255         NEGW    Y
      0083C8 8A               [ 1] 1256         PUSH    CC
      0083C9 EF 02            [ 2] 1257         LDW     (2,X),Y
      0083CB 90 93            [ 1] 1258         LDW     Y,X
      0083CD 90 FE            [ 2] 1259         LDW     Y,(Y)
      0083CF 90 53            [ 2] 1260         CPLW    Y
      0083D1 86               [ 1] 1261         POP     CC
      0083D2 25 02            [ 1] 1262         JRC     DN1
      0083D4 90 5C            [ 1] 1263         INCW    Y
      0083D6 FF               [ 2] 1264 DN1:    LDW     (X),Y
      0083D7 81               [ 4] 1265         RET
                                   1266         .endif
                                   1267 
                                   1268 ;       =       ( w w -- t )    ( TOS STM8: -- Y,Z,N )
                                   1269 ;       Return true if top two are equal.
                                   1270 
      0083D8                       1271         HEADER  EQUAL "="
      0083D8                       1272 EQUAL:
                           000000  1273         .ifeq   SPEEDOVERSIZE
                                   1274         CALL    XORR
                                   1275         JP      ZEQUAL                 ; 31 cy= (18+13)
                           000001  1276         .else
      0083D8 90 5F            [ 1] 1277         CLRW Y                          ; (19 bytes, 17 cy)                
      0083DA F6               [ 1] 1278         LD A,(X)                              
      0083DB E0 02            [ 1] 1279         SUB A,(02,X)                         
      0083DD 26 08            [ 1] 1280         JRNE 1$                      
      0083DF E6 01            [ 1] 1281         LD A,(01,X)                          
      0083E1 E0 03            [ 1] 1282         SUB A,(03,X)                         
      0083E3 26 02            [ 1] 1283         JRNE 1$                      
      0083E5 90 53            [ 2] 1284         CPLW Y                                 
      0083E7 5C               [ 1] 1285 1$:     INCW X                                 
      0083E8 5C               [ 1] 1286         INCW X                                 
      0083E9 FF               [ 2] 1287         LDW (X),Y                              
      0083EA 81               [ 4] 1288         RET
                                   1289         .endif                          ; 17 cy, 19 bytes
                                   1290 
                                   1291 
                                   1292 ;       U<      ( u u -- t )    ( TOS STM8: -- Y,Z,N )
                                   1293 ;       Unsigned compare of top two items.
                                   1294 
      0083EB                       1295         HEADER  ULESS "U<"
      0083EB                       1296 ULESS:
      0083EB 4F               [ 1] 1297         CLR     A
      0083EC AD 25            [ 4] 1298         CALLR   XREG0CMP
      0083EE 24 01            [ 1] 1299         JRUGE   1$
      0083F0 43               [ 1] 1300         CPL     A
      0083F1 90 97            [ 1] 1301 1$:     LD      YL,A
      0083F3 90 95            [ 1] 1302         LD      YH,A
      0083F5 FF               [ 2] 1303         LDW     (X),Y
      0083F6 81               [ 4] 1304         RET
                                   1305 
                           000001  1306         .ifeq   BOOTSTRAP
                                   1307 ;       <       ( n1 n2 -- t )
                                   1308 ;       Signed compare of top two items.
                                   1309 
      0083F7                       1310         HEADER  LESS "<"
      0083F7                       1311 LESS:
                           000000  1312         .ifeq   SPEEDOVERSIZE
                                   1313         CALL    SUBB             ; (29cy)
                                   1314         JP      ZLESS            ; 41 cy (12+29)
                           000001  1315         .else
      0083F7 4F               [ 1] 1316         CLR     A
      0083F8 90 93            [ 1] 1317         LDW     Y,X
      0083FA 90 FE            [ 2] 1318         LDW     Y,(Y)
      0083FC 90 BF 08         [ 2] 1319         LDW     XREG0,Y
      0083FF 5C               [ 1] 1320         INCW    X
      008400 5C               [ 1] 1321         INCW    X
      008401 90 93            [ 1] 1322         LDW     Y,X
      008403 90 FE            [ 2] 1323         LDW     Y,(Y)
      008405 90 B3 08         [ 2] 1324         CPW     Y,XREG0
      008408 2E 01            [ 1] 1325         JRSGE   1$
      00840A 43               [ 1] 1326         CPL     A
      00840B F7               [ 1] 1327 1$:     LD      (X),A
      00840C E7 01            [ 1] 1328         LD      (1,X),A
      00840E 90 93            [ 1] 1329         LDW     Y,X
      008410 90 FE            [ 2] 1330         LDW     Y,(Y)
      008412 81               [ 4] 1331         RET                      ; 26 cy
                                   1332         .endif
                                   1333         .endif
                                   1334 
                                   1335 ;       XREG0CMP       ( n n - n )      ( TOS STM8: - Y,Z,N )
                                   1336 ;       Load (TOS) to XREG0 and (TOS-1) to Y, DROP, CMP to STM8 flags
      008413                       1337 XREG0CMP:
      008413 90 93            [ 1] 1338         LDW     Y,X
      008415 5C               [ 1] 1339         INCW    X
      008416 5C               [ 1] 1340         INCW    X
      008417 51               [ 1] 1341         EXGW    X,Y
      008418 FE               [ 2] 1342         LDW     X,(X)
      008419 BF 08            [ 2] 1343         LDW     XREG0,X
      00841B 93               [ 1] 1344         LDW     X,Y
      00841C FE               [ 2] 1345         LDW     X,(X)
      00841D B3 08            [ 2] 1346         CPW     X,XREG0
      00841F 51               [ 1] 1347         EXGW    X,Y
      008420 81               [ 4] 1348         RET
                                   1349 
                           000001  1350         .ifeq   BOOTSTRAP
                                   1351 ;       MAX     ( n n -- n )    ( TOS STM8: -- Y,Z,N )
                                   1352 ;       Return greater of two top items.
                                   1353 
      008421                       1354         HEADER  MAX "MAX"
      008421                       1355 MAX:
      008421 AD F0            [ 4] 1356         CALLR   XREG0CMP
      008423 2C 04            [ 1] 1357         JRSGT   MMEXIT
      008425                       1358 XREG0TOS:
      008425 90 BE 08         [ 2] 1359         LDW     Y,XREG0
      008428 FF               [ 2] 1360         LDW     (X),Y
      008429                       1361 MMEXIT:
      008429 81               [ 4] 1362         RET
                                   1363         .endif
                                   1364 
                           000001  1365         .ifeq   BOOTSTRAP
                                   1366 ;       MIN     ( n n -- n )    ( TOS STM8: -- Y,Z,N )
                                   1367 ;       Return smaller of top two items.
                                   1368 
      00842A                       1369         HEADER  MIN "MIN"
      00842A                       1370 MIN:
      00842A AD E7            [ 4] 1371         CALLR   XREG0CMP
      00842C 2F FB            [ 1] 1372         JRSLT   MMEXIT
      00842E 20 F5            [ 2] 1373         JRA     XREG0TOS
                                   1374         .endif
                                   1375 
                                   1376 ;       WITHIN ( u ul uh -- t ) ( TOS STM8: -- Y,Z,N )
                                   1377 ;       Return true if u is within
                                   1378 ;       range of ul and uh. ( ul <= u < uh )
                                   1379 
      008430                       1380         HEADER  WITHI "WITHIN"
      008430                       1381 WITHI:
      008430 CD 83 03         [ 4] 1382         CALL    OVER
      008433 CD 83 6A         [ 4] 1383         CALL    SUBB
      008436 E6 01            [ 1] 1384         LD A,(1,X)
      008438 88               [ 1] 1385         PUSH A
      008439 F6               [ 1] 1386         LD A,(X)
      00843A 88               [ 1] 1387         PUSH A
      00843B 5C               [ 1] 1388         INCW X
      00843C 5C               [ 1] 1389         INCW X
      00843D CD 83 6A         [ 4] 1390         CALL    SUBB
      008440 90 85            [ 2] 1391         POPW Y
      008442 5A               [ 2] 1392         DECW X
      008443 5A               [ 2] 1393         DECW X
      008444 FF               [ 2] 1394         LDW (X),Y
      008445 20 A4            [ 2] 1395         JRA     ULESS
                                   1396 
                                   1397 ; Divide
                                   1398 
                                   1399 ;       UM/MOD  ( udl udh un -- ur uq )
                                   1400 ;       Unsigned divide of a double by a
                                   1401 ;       single. Return mod and quotient.
                                   1402 
      008447                       1403         HEADER  UMMOD "UM/MOD"
      008447                       1404 UMMOD:
      008447 90 93            [ 1] 1405         LDW     Y,X             ; stack pointer to Y
      008449 FE               [ 2] 1406         LDW     X,(X)           ; un
      00844A BF 08            [ 2] 1407         LDW     XREG0,X         ; save un
      00844C 93               [ 1] 1408         LDW     X,Y
      00844D 5C               [ 1] 1409         INCW    X               ; drop un
      00844E 5C               [ 1] 1410         INCW    X
      00844F 89               [ 2] 1411         PUSHW   X               ; save stack pointer
      008450 FE               [ 2] 1412         LDW     X,(X)           ; X=udh
      008451 90 EE 04         [ 2] 1413         LDW     Y,(4,Y)         ; Y=udl (offset before drop)
      008454 B3 08            [ 2] 1414         CPW     X,XREG0
      008456 25 09            [ 1] 1415         JRULT   MMSM1           ; X is still on the R-stack
      008458 85               [ 2] 1416         POPW    X               ; restore stack pointer
      008459 90 5F            [ 1] 1417         CLRW    Y
      00845B EF 02            [ 2] 1418         LDW     (2,X),Y         ; remainder 0
      00845D 90 5A            [ 2] 1419         DECW    Y
      00845F FF               [ 2] 1420         LDW     (X),Y           ; quotient max. 16 bit value
      008460 81               [ 4] 1421         RET
      008461                       1422 MMSM1:
      008461 A6 10            [ 1] 1423         LD      A,#16           ; loop count
      008463 90 58            [ 2] 1424         SLLW    Y               ; udl shift udl into udh
      008465                       1425 MMSM3:
      008465 59               [ 2] 1426         RLCW    X               ; rotate udl bit into uhdh (= remainder)
      008466 25 04            [ 1] 1427         JRC     MMSMa           ; if carry out of rotate
      008468 B3 08            [ 2] 1428         CPW     X,XREG0         ; compare udh to un
      00846A 25 05            [ 1] 1429         JRULT   MMSM4           ; can't subtract
      00846C                       1430 MMSMa:
      00846C 72 B0 00 08      [ 2] 1431         SUBW    X,XREG0         ; can subtract
      008470 98               [ 1] 1432         RCF
      008471                       1433 MMSM4:
      008471 8C               [ 1] 1434         CCF                     ; quotient bit
      008472 90 59            [ 2] 1435         RLCW    Y               ; rotate into quotient, rotate out udl
      008474 4A               [ 1] 1436         DEC     A               ; repeat
      008475 26 EE            [ 1] 1437         JRNE    MMSM3           ; if A == 0
      008477                       1438 MMSMb:
      008477 BF 08            [ 2] 1439         LDW     XREG0,X         ; done, save remainder
      008479 85               [ 2] 1440         POPW    X               ; restore stack pointer
      00847A FF               [ 2] 1441         LDW     (X),Y           ; save quotient
      00847B 90 BE 08         [ 2] 1442         LDW     Y,XREG0         ; remainder onto stack
      00847E EF 02            [ 2] 1443         LDW     (2,X),Y
      008480 81               [ 4] 1444         RET
                                   1445 
                           000001  1446         .ifeq   UNLINKCORE
                                   1447 ;       M/MOD   ( d n -- r q )
                                   1448 ;       Signed floored divide of double by
                                   1449 ;       single. Return mod and quotient.
                                   1450 
      008481                       1451         HEADER  MSMOD "M/MOD"
      008481                       1452 MSMOD:
      008481 F6               [ 1] 1453         LD      A,(X)           ; DUPP ZLESS
      008482 88               [ 1] 1454         PUSH    A               ; DUPP TOR
      008483 2A 12            [ 1] 1455         JRPL    MMOD1           ; QBRAN
      008485 CD 85 69         [ 4] 1456         CALL    NEGAT
      008488 E6 01            [ 1] 1457         LD A,(1,X)
      00848A 88               [ 1] 1458         PUSH A
      00848B F6               [ 1] 1459         LD A,(X)
      00848C 88               [ 1] 1460         PUSH A
      00848D 5C               [ 1] 1461         INCW X
      00848E 5C               [ 1] 1462         INCW X
      00848F CD 83 C1         [ 4] 1463         CALL    DNEGA
      008492 90 85            [ 2] 1464         POPW Y
      008494 5A               [ 2] 1465         DECW X
      008495 5A               [ 2] 1466         DECW X
      008496 FF               [ 2] 1467         LDW (X),Y
      008497                       1468 MMOD1:
      008497 E6 01            [ 1] 1469         LD A,(1,X)
      008499 88               [ 1] 1470         PUSH A
      00849A F6               [ 1] 1471         LD A,(X)
      00849B 88               [ 1] 1472         PUSH A
      00849C 5C               [ 1] 1473         INCW X
      00849D 5C               [ 1] 1474         INCW X
      00849E 90 93            [ 1] 1475         LDW     Y,X
      0084A0 90 FE            [ 2] 1476         LDW     Y,(Y)
      0084A2 2A 0A            [ 1] 1477         JRPL    MMOD2           ; DUPP ZLESS QBRAN
      0084A4 90 85            [ 2] 1478         POPW Y
      0084A6 5A               [ 2] 1479         DECW X
      0084A7 5A               [ 2] 1480         DECW X
      0084A8 FF               [ 2] 1481         LDW (X),Y
      0084A9 90 89            [ 2] 1482         PUSHW Y
      0084AB CD 83 33         [ 4] 1483         CALL    PLUS
      0084AE                       1484 MMOD2:  
      0084AE 90 85            [ 2] 1485         POPW Y
      0084B0 5A               [ 2] 1486         DECW X
      0084B1 5A               [ 2] 1487         DECW X
      0084B2 FF               [ 2] 1488         LDW (X),Y
      0084B3 AD 92            [ 4] 1489         CALLR   UMMOD
      0084B5 84               [ 1] 1490         POP     A               ; RFROM
      0084B6 4D               [ 1] 1491         TNZ     A
      0084B7 2A 09            [ 1] 1492         JRPL    MMOD3           ; QBRAN
      0084B9 CD 83 1E         [ 4] 1493         CALL    SWAPP
      0084BC CD 85 69         [ 4] 1494         CALL    NEGAT
      0084BF CC 83 1E         [ 2] 1495         JP      SWAPP
      0084C2 81               [ 4] 1496 MMOD3:  RET
                                   1497 
                                   1498 ;       /MOD    ( n n -- r q )
                                   1499 ;       Signed divide. Return mod and quotient.
                                   1500 
      0084C3                       1501         HEADER  SLMOD "/MOD"
      0084C3                       1502 SLMOD:
      0084C3 CD 83 03         [ 4] 1503         CALL    OVER
      0084C6 CD 83 61         [ 4] 1504         CALL    ZLESS
      0084C9 CD 83 1E         [ 4] 1505         CALL    SWAPP
      0084CC 20 B3            [ 2] 1506         JRA     MSMOD
                                   1507 
                                   1508 ;       MOD     ( n n -- r )    ( TOS STM8: -- Y,Z,N )
                                   1509 ;       Signed divide. Return mod only.
                                   1510 
      0084CE                       1511         HEADER  MMOD "MOD"
      0084CE                       1512 MMOD:
      0084CE AD F3            [ 4] 1513         CALLR   SLMOD
      0084D0 5C               [ 1] 1514         INCW    X
      0084D1 5C               [ 1] 1515         INCW    X
      0084D2 81               [ 4] 1516         RET
                                   1517 
                                   1518 ;       /       ( n n -- q )    ( TOS STM8: -- Y,Z,N )
                                   1519 ;       Signed divide. Return quotient only.
                                   1520 
      0084D3                       1521         HEADER  SLASH "/"
      0084D3                       1522 SLASH:
      0084D3 AD EE            [ 4] 1523         CALLR   SLMOD
      0084D5 CC 83 0C         [ 2] 1524         JP      NIP
                                   1525         .endif
                                   1526 
                                   1527 ; Multiply
                                   1528 
                                   1529 ;       UM*     ( u u -- ud )
                                   1530 ;       Unsigned multiply. Return double product.
                                   1531 
      0084D8                       1532         HEADER  UMSTA "UM*"
      0084D8                       1533 UMSTA:                          ; stack have 4 bytes u1=a,b u2=c,d
      0084D8 E6 02            [ 1] 1534         LD      A,(2,X)         ; b
      0084DA 90 97            [ 1] 1535         LD      YL,A
      0084DC F6               [ 1] 1536         LD      A,(X)           ; d
      0084DD 90 42            [ 4] 1537         MUL     Y,A
      0084DF 90 89            [ 2] 1538         PUSHW   Y               ; PROD1 temp storage
      0084E1 E6 03            [ 1] 1539         LD      A,(3,X)         ; a
      0084E3 90 97            [ 1] 1540         LD      YL,A
      0084E5 F6               [ 1] 1541         LD      A,(X)           ; d
      0084E6 90 42            [ 4] 1542         MUL     Y,A
      0084E8 90 89            [ 2] 1543         PUSHW   Y               ; PROD2 temp storage
      0084EA E6 02            [ 1] 1544         LD      A,(2,X)         ; b
      0084EC 90 97            [ 1] 1545         LD      YL,A
      0084EE E6 01            [ 1] 1546         LD      A,(1,X)         ; c
      0084F0 90 42            [ 4] 1547         MUL     Y,A
      0084F2 90 89            [ 2] 1548         PUSHW   Y               ; PROD3,CARRY temp storage
      0084F4 E6 03            [ 1] 1549         LD      A,(3,X)         ; a
      0084F6 90 97            [ 1] 1550         LD      YL,A
      0084F8 E6 01            [ 1] 1551         LD      A,(1,X)         ; c
      0084FA 90 42            [ 4] 1552         MUL     Y,A             ; least signifiant product
      0084FC 4F               [ 1] 1553         CLR     A
      0084FD 90 01            [ 1] 1554         RRWA    Y
      0084FF E7 03            [ 1] 1555         LD      (3,X),A         ; store least significant byte
      008501 72 F9 01         [ 2] 1556         ADDW    Y,(1,SP)        ; PROD3
      008504 4F               [ 1] 1557         CLR     A
      008505 49               [ 1] 1558         RLC     A               ; save carry
      008506 6B 01            [ 1] 1559         LD      (1,SP),A        ; CARRY
      008508 72 F9 03         [ 2] 1560         ADDW    Y,(3,SP)        ; PROD2
      00850B 7B 01            [ 1] 1561         LD      A,(1,SP)        ; CARRY
      00850D A9 00            [ 1] 1562         ADC     A,#0            ; add 2nd carry
      00850F 6B 01            [ 1] 1563         LD      (1,SP),A        ; CARRY
      008511 4F               [ 1] 1564         CLR     A
      008512 90 01            [ 1] 1565         RRWA    Y
      008514 E7 02            [ 1] 1566         LD      (2,X),A         ; 2nd product byte
      008516 72 F9 05         [ 2] 1567         ADDW    Y,(5,SP)        ; PROD1
      008519 90 01            [ 1] 1568         RRWA    Y
      00851B E7 01            [ 1] 1569         LD      (1,X),A         ; 3rd product byte
      00851D 90 01            [ 1] 1570         RRWA    Y               ; 4th product byte now in A
      00851F 19 01            [ 1] 1571         ADC     A,(1,SP)        ; CARRY
      008521 F7               [ 1] 1572         LD      (X),A
      008522 5B 06            [ 2] 1573         ADDW    SP,#6           ; drop temp storage
      008524 81               [ 4] 1574         RET
                                   1575 
                                   1576 ;       *       ( n n -- n )    ( TOS STM8: -- Y,Z,N )
                                   1577 ;       Signed multiply. Return single product.
                                   1578 
      008525                       1579         HEADER  STAR "*"
      008525                       1580 STAR:
      008525 AD B1            [ 4] 1581         CALLR   UMSTA
      008527 5C               [ 1] 1582         INCW    X
      008528 5C               [ 1] 1583         INCW    X
      008529 81               [ 4] 1584         RET
                           000001  1585         .ifeq   UNLINKCORE
                                   1586 ;       M*      ( n n -- d )
                                   1587 ;       Signed multiply. Return double product.
      00852A                       1588         HEADER  MSTAR "M*"
      00852A                       1589 MSTAR:
      00852A E6 02            [ 1] 1590         LD      A,(2,X)         ; DDUP
      00852C F8               [ 1] 1591         XOR     A,(X)           ; XORR
      00852D 88               [ 1] 1592         PUSH    A               ; TOR
      00852E CD 85 71         [ 4] 1593         CALL    ABSS
      008531 CD 83 1E         [ 4] 1594         CALL    SWAPP
      008534 CD 85 71         [ 4] 1595         CALL    ABSS
      008537 AD 9F            [ 4] 1596         CALLR   UMSTA
      008539 84               [ 1] 1597         POP     A               ; RFROM
      00853A 4D               [ 1] 1598         TNZ     A
      00853B 2A 03            [ 1] 1599         JRPL    MSTA1           ; QBRAN
      00853D CC 83 C1         [ 2] 1600         JP      DNEGA
      008540 81               [ 4] 1601 MSTA1:  RET
                                   1602 
                                   1603 ;       */MOD   ( n1 n2 n3 -- r q )
                                   1604 ;       Multiply n1 and n2, then divide
                                   1605 ;       by n3. Return mod and quotient.
      008541                       1606         HEADER  SSMOD "*/MOD"
      008541                       1607 SSMOD:
      008541 E6 01            [ 1] 1608         LD A,(1,X)
      008543 88               [ 1] 1609         PUSH A
      008544 F6               [ 1] 1610         LD A,(X)
      008545 88               [ 1] 1611         PUSH A
      008546 5C               [ 1] 1612         INCW X
      008547 5C               [ 1] 1613         INCW X
      008548 AD E0            [ 4] 1614         CALLR   MSTAR
      00854A 90 85            [ 2] 1615         POPW Y
      00854C 5A               [ 2] 1616         DECW X
      00854D 5A               [ 2] 1617         DECW X
      00854E FF               [ 2] 1618         LDW (X),Y
      00854F CC 84 81         [ 2] 1619         JP      MSMOD
                                   1620 
                                   1621 ;       */      ( n1 n2 n3 -- q )    ( TOS STM8: -- Y,Z,N )
                                   1622 ;       Multiply n1 by n2, then divide
                                   1623 ;       by n3. Return quotient only.
      008552                       1624         HEADER  STASL "*/"
      008552                       1625 STASL:
      008552 AD ED            [ 4] 1626         CALLR   SSMOD
      008554 CC 83 0C         [ 2] 1627         JP      NIP
                                   1628         .endif
                                   1629 
                                   1630 ; Miscellaneous
                                   1631 
                                   1632 
                           000001  1633         .ifeq   BAREBONES
                                   1634 ;       EXG      ( n -- n )      ( TOS STM8: -- Y,Z,N )
                                   1635 ;       Exchange high with low byte of n.
                                   1636 
      008557                       1637         HEADER  EXG "EXG"
      008557                       1638 EXG:
      008557                       1639         LDW_Y_CONTENT_X
      008557 90 93            [ 1]    1         LDW Y,X
      008559 90 FE            [ 2]    2         LDW Y,(Y)		; tos in Y
      00855B 90 5E            [ 1] 1640         SWAPW   Y
      00855D FF               [ 2] 1641         LDW (X),Y
      00855E 81               [ 4] 1642         RET
                                   1643         .endif
                                   1644 
                                   1645 ;        .ifeq   BOOTSTRAP
                                   1646 
                                   1647 ;       2+      ( a -- a )      ( TOS STM8: -- Y,Z,N )
                                   1648 ;       Add 2 to tos.
                                   1649 
      00855F                       1650         HEADER  CELLP "2+"
      00855F                       1651 CELLP:
      00855F                       1652         LDW_Y_CONTENT_X
      00855F 90 93            [ 1]    1         LDW Y,X
      008561 90 FE            [ 2]    2         LDW Y,(Y)		; tos in Y
      008563 90 5C            [ 1] 1653         INCW    Y
      008565 90 5C            [ 1] 1654         INCW    Y
      008567 FF               [ 2] 1655         LDW (X),Y
      008568 81               [ 4] 1656         RET
                                   1657 
                                   1658 ;       NEGATE  ( n -- -n )     ( TOS STM8: -- Y,Z,N )
                                   1659 ;       Two's complement of TOS.
                                   1660 
      008569                       1661         HEADER  NEGAT "NEGATE"
      008569                       1662 NEGAT:
      008569                       1663         LDW_Y_CONTENT_X
      008569 90 93            [ 1]    1         LDW Y,X
      00856B 90 FE            [ 2]    2         LDW Y,(Y)		; tos in Y
      00856D 90 50            [ 2] 1664         NEGW    Y
      00856F FF               [ 2] 1665         LDW (X),Y
      008570 81               [ 4] 1666         RET
                                   1667 
                                   1668 ;       ABS     ( n -- n )      ( TOS STM8: -- Y,Z,N )
                                   1669 ;       Return  absolute value of n.
                                   1670 
      008571                       1671         HEADER  ABSS "ABS"
      008571                       1672 ABSS:
      008571                       1673         LDW_Y_CONTENT_X
      008571 90 93            [ 1]    1         LDW Y,X
      008573 90 FE            [ 2]    2         LDW Y,(Y)		; tos in Y
      008575 2A 03            [ 1] 1674         JRPL    1$              ; positive?
      008577 90 50            [ 2] 1675         NEGW    Y               ; else negate
      008579 FF               [ 2] 1676         LDW (X),Y
      00857A 81               [ 4] 1677 1$:     RET
                                   1678 
                                   1679 ;       0=      ( n -- t )      ( TOS STM8: -- Y,Z,N ))
                                   1680 ;       Return true if n is equal to 0
                                   1681 
      00857B                       1682         HEADER  ZEQUAL "0="
      00857B                       1683 ZEQUAL:
      00857B                       1684         LDW_Y_CONTENT_X
      00857B 90 93            [ 1]    1         LDW Y,X
      00857D 90 FE            [ 2]    2         LDW Y,(Y)		; tos in Y
      00857F 27 04            [ 1] 1685         JREQ    CPLW
      008581 90 5F            [ 1] 1686 CLRW:   CLRW    Y
      008583 FF               [ 2] 1687         LDW (X),Y
      008584 81               [ 4] 1688         RET
      008585 90 53            [ 2] 1689 CPLW:   CPLW    Y               ; else -1
      008587 FF               [ 2] 1690         LDW (X),Y
      008588 81               [ 4] 1691         RET
                                   1692 
                                   1693 ;       !0=      ( n -- t )      ( TOS STM8: -- Y,Z,N ))
                                   1694 ;       Return true if n is not equal to 0
                                   1695 
      008589                       1696         HEADER  ZEQUAL "!0="
      008589                       1697 NZEQUAL:
      008589                       1698         LDW_Y_CONTENT_X
      008589 90 93            [ 1]    1         LDW Y,X
      00858B 90 FE            [ 2]    2         LDW Y,(Y)		; tos in Y
      00858D 26 02            [ 1] 1699         JRNE    1$
      00858F 20 F0            [ 2] 1700         JRA     CLRW
      008591 90 5F            [ 1] 1701 1$:     CLRW    Y
      008593 20 F0            [ 2] 1702         JRA     CPLW
                                   1703 
                                   1704 ;       PICK    ( ... +n -- ... w )      ( TOS STM8: -- Y,Z,N )
                                   1705 ;       Copy    nth stack item to tos.
                                   1706 
      008595                       1707         HEADER  PICK "PICK"
      008595                       1708 PICK:
      008595                       1709         LDW_Y_CONTENT_X
      008595 90 93            [ 1]    1         LDW Y,X
      008597 90 FE            [ 2]    2         LDW Y,(Y)		; tos in Y
      008599 90 5C            [ 1] 1710         INCW    Y
      00859B 90 58            [ 2] 1711         SLAW    Y
      00859D BF 08            [ 2] 1712         LDW     XREG0,X
      00859F 72 B9 00 08      [ 2] 1713         ADDW    Y,XREG0
      0085A3 90 FE            [ 2] 1714         LDW     Y,(Y)
      0085A5 FF               [ 2] 1715         LDW (X),Y
      0085A6 81               [ 4] 1716         RET
                                   1717 
                                   1718 ;       DEPTH   ( -- n )      ( TOS STM8: -- Y,Z,N )
                                   1719 ;       Return  depth of data stack.
                                   1720 
      0085A7                       1721         HEADER  DEPTH "DEPTH"
      0085A7                       1722 DEPTH:
      0085A7 90 93            [ 1] 1723         LDW     Y,X
      0085A9 50               [ 2] 1724         NEGW    X
      0085AA 1C 07 A0         [ 2] 1725         ADDW    X,#SPP
      0085AD 57               [ 2] 1726         SRAW    X
      0085AE 51               [ 1] 1727 XSTOR:  EXGW    X,Y
      0085AF 5A               [ 2] 1728         DECW    X               ; SUBW  X,#2
      0085B0 5A               [ 2] 1729         DECW    X
      0085B1 FF               [ 2] 1730         LDW     (X),Y           ; push on stack
      0085B2 81               [ 4] 1731         RET                     ; go to RET of EXEC
                                   1732 
                                   1733 ; Memory access
                                   1734 
                                   1735 ;       +!      ( n a -- )      ( TOS STM8: -- Y,Z,N )
                                   1736 ;       Add n to contents at address a.
                                   1737 
      0085B3                       1738         HEADER  PSTOR "+!"
      0085B3                       1739 PSTOR:
      0085B3 BF 08            [ 2] 1740         LDW XREG0,X
      0085B5 EE 02            [ 2] 1741         LDW X,(2,X)
      0085B7 89               [ 2] 1742         PUSHW X
      0085B8 BE 08            [ 2] 1743         LDW X,XREG0
      0085BA FE               [ 2] 1744         LDW X,(X)        ; addr
      0085BB 90 93            [ 1] 1745         LDW Y,X
      0085BD FE               [ 2] 1746         LDW X,(X)
      0085BE 72 FB 01         [ 2] 1747         ADDW X,(1,SP)
      0085C1 90 FF            [ 2] 1748         LDW (Y),X
      0085C3 85               [ 2] 1749         POPW X
      0085C4 BE 08            [ 2] 1750 ENDPP:	LDW X,XREG0
      0085C6                       1751         HEADER  DDROP "2DROP"
      0085C6                       1752 DDROP:
      0085C6 1C 00 04         [ 2] 1753         ADDW    X,#4
      0085C9 81               [ 4] 1754         RET
                                   1755 
                                   1756 ;       +C!      ( n a -- )      ( TOS STM8: -- Y,Z,N )
                                   1757 ;       Add n to contents at address a.
                                   1758 
      0085CA                       1759         HEADER  PCSTOR "+C!"
      0085CA                       1760 PCSTOR:
      0085CA BF 08            [ 2] 1761         LDW XREG0,X
      0085CC FE               [ 2] 1762         LDW X,(X)        ; addr
      0085CD 90 93            [ 1] 1763         LDW Y,X
      0085CF F6               [ 1] 1764         LD A,(X)
      0085D0 BE 08            [ 2] 1765         LDW X,XREG0
      0085D2 EB 03            [ 1] 1766         ADD A,(3,X)
      0085D4 90 F7            [ 1] 1767         LD (Y),A
      0085D6 20 EC            [ 2] 1768         JRA ENDPP
                                   1769 
                                   1770 ;       @EXECUTE        ( a -- )  ( TOS STM8: undefined )
                                   1771 ;       Execute vector stored in address a.
                                   1772 
      0085D8                       1773         HEADER  ATEXE "@EXECUTE"
      0085D8                       1774 ATEXE:
      0085D8 90 93            [ 1] 1775         LDW     Y,X
      0085DA 5C               [ 1] 1776         INCW    X
      0085DB 5C               [ 1] 1777         INCW    X
      0085DC 90 FE            [ 2] 1778         LDW     Y,(Y)
      0085DE 90 FE            [ 2] 1779         LDW     Y,(Y)
      0085E0 27 02            [ 1] 1780         JREQ    1$
      0085E2 90 FC            [ 2] 1781         JP      (Y)
      0085E4 81               [ 4] 1782 1$:     RET
                                   1783 
                                   1784 ;       21+     ( u u -- u+1 u+1)
                                   1785  
      0085E5                       1786         HEADER  TWOONEPLUS "21+"
      0085E5                       1787 TWOONEPLUS:
      0085E5 90 93            [ 1] 1788         LDW Y,X 
      0085E7 FE               [ 2] 1789         LDW X,(X) 
      0085E8 5C               [ 1] 1790         INCW X  
      0085E9 90 FF            [ 2] 1791         LDW (Y),X 
      0085EB 93               [ 1] 1792         LDW X,Y  
      0085EC EE 02            [ 2] 1793         LDW X,(2,X) 
      0085EE 5C               [ 1] 1794         INCW X 
      0085EF 90 EF 02         [ 2] 1795         LDW (2,Y),X 
      0085F2 93               [ 1] 1796         LDW X,Y
      0085F3 81               [ 4] 1797         RET
                                   1798 
                                   1799 ;       CMOVE   ( b1 b2 u -- )
                                   1800 ;       Copy u bytes from b1 to b2.
      0085F4                       1801         HEADER  CMOVE "CMOVE"
      0085F4                       1802 CMOVE:
      0085F4 E6 01            [ 1] 1803         LD A,(1,X)
      0085F6 88               [ 1] 1804         PUSH A
      0085F7 5C               [ 1] 1805         INCW X
      0085F8 5C               [ 1] 1806         INCW X
      0085F9 90 93            [ 1] 1807 CMOVE1: LDW Y,X
      0085FB EE 02            [ 2] 1808         LDW X,(2,X)
      0085FD F6               [ 1] 1809         LD A,(X)
      0085FE 93               [ 1] 1810         LDW X,Y
      0085FF FE               [ 2] 1811         LDW X,(X)
      008600 F7               [ 1] 1812         LD (X),A
      008601 93               [ 1] 1813         LDW X,Y
      008602 AD E1            [ 4] 1814         CALLR TWOONEPLUS
      008604 84               [ 1] 1815         POP A
      008605 4A               [ 1] 1816         DEC A
      008606 88               [ 1] 1817         PUSH A
      008607 26 F0            [ 1] 1818         JRNE CMOVE1
      008609 84               [ 1] 1819         POP A
      00860A 1C 00 04         [ 2] 1820         ADDW X,#4
      00860D 81               [ 4] 1821         RET
                                   1822 
                                   1823 
                                   1824 ; Basic I/O
                                   1825 
                                   1826 ;       KEY     ( -- c )
                                   1827 ;       Wait for and return an
                                   1828 ;       input character.
                                   1829 
      00860E                       1830         HEADER  KEY "KEY"
      00860E                       1831 KEY:
      00860E 92 CD 02         [ 6] 1832 KEY1:   CALL    [USRQKEY]
      008611 90 93            [ 1] 1833         LDW     Y,X
      008613 5C               [ 1] 1834         INCW    X
      008614 5C               [ 1] 1835         INCW    X
      008615 90 FE            [ 2] 1836         LDW     Y,(Y)
      008617 26 02            [ 1] 1837         JRNE    RETIDLE
      008619 20 F3            [ 2] 1838         JRA     KEY1
      00861B                       1839 RETIDLE:
      00861B 81               [ 4] 1840         RET
                                   1841 
                                   1842 ;       QUIT    ( -- )
                                   1843 ;       Reset return stack pointer
                                   1844 ;       and start text interpreter.
                                   1845 
      00861C                       1846         HEADER  QUIT "QUIT"
      00861C                       1847 QUIT:
      00861C 90 AE 07 FF      [ 2] 1848         LDW     Y,#RPP          ; initialize return stack
      008620 90 94            [ 1] 1849         LDW     SP,Y
      008622 CC 81 DE         [ 2] 1850         JP      SLEEP
                                   1851 
                                   1852 
                                   1853 ;       SP!     ( a -- )
                                   1854 ;       Set data stack pointer.
                                   1855 
      008625                       1856         HEADER  SPSTO "sp!"
      008625                       1857 SPSTO:
      008625 FE               [ 2] 1858         LDW     X,(X)   ;X = a
      008626 81               [ 4] 1859         RET
                                   1860 
                                   1861 ;       SP@     ( -- a )        ( TOS STM8: -- Y,Z,N )
                                   1862 ;       Push current stack pointer.
                                   1863 
      008627                       1864         HEADER  SPAT "sp@"
      008627                       1865 SPAT:
      008627 90 93            [ 1] 1866         LDW     Y,X
      008629 5A               [ 2] 1867         DECW    X               ; SUBW  X,#2
      00862A 5A               [ 2] 1868         DECW    X
      00862B FF               [ 2] 1869         LDW     (X),Y           ; push on stack
      00862C 81               [ 4] 1870         RET                     ; go to RET of EXEC
                                   1871 
                                   1872 ;       RP@     ( -- a )     ( TOS STM8: -- Y,Z,N )
                                   1873 ;       Push current RP to data stack.
                                   1874 
      00862D                       1875         HEADER  RPAT "rp@"
      00862D                       1876 RPAT:
      00862D 90 96            [ 1] 1877         LDW     Y,SP            ; save return addr
      00862F 5A               [ 2] 1878         DECW    X               ; SUBW  X,#2
      008630 5A               [ 2] 1879         DECW    X
      008631 FF               [ 2] 1880         LDW     (X),Y           ; push on stack
      008632 81               [ 4] 1881         RET                     ; go to RET of EXEC
                                   1882 
                                   1883 ;       RP!     ( a -- )
                                   1884 ;       Set return stack pointer.
                                   1885 
      008633                       1886         HEADFLG RPSTO "rp!" COMPO
                                      1 
      008633                       1887 RPSTO:
      008633 90 85            [ 2] 1888         POPW    Y
      008635 90 BF 08         [ 2] 1889         LDW     XREG0,Y
      008638 90 93            [ 1] 1890         LDW     Y,X
      00863A 5C               [ 1] 1891         INCW    X
      00863B 5C               [ 1] 1892         INCW    X
      00863C 90 FE            [ 2] 1893         LDW     Y,(Y)
      00863E 90 94            [ 1] 1894         LDW     SP,Y
      008640 92 CC 08         [ 5] 1895         JP      [XREG0]
                                   1896 
                                   1897 ;===============================================================
      008643                       1898         HEADFLG DO "DO" COMPO
                                      1 
      008643                       1899 DO:
      008643 90 93            [ 1] 1900         LDW Y,X
      008645 85               [ 2] 1901         POPW X
      008646 89               [ 2] 1902         PUSHW X
      008647 89               [ 2] 1903         PUSHW X
      008648 89               [ 2] 1904         PUSHW X
      008649 93               [ 1] 1905         LDW X,Y
      00864A FE               [ 2] 1906         LDW X,(X)
      00864B 1F 03            [ 2] 1907         LDW (3,SP),X
      00864D 93               [ 1] 1908         LDW X,Y
      00864E EE 02            [ 2] 1909         LDW X,(2,X)
      008650 1F 05            [ 2] 1910         LDW (5,SP),X
      008652 93               [ 1] 1911         LDW X,Y
      008653 1C 00 04         [ 2] 1912         ADDW X,#4
      008656 81               [ 4] 1913         RET
                                   1914 
                                   1915 ;===============================================================
                           000001  1916         .ifne   WORDS_EXTRAEEPR
                                   1917 ;       ULOCK  ( -- )
                                   1918 ;       Unlock EEPROM (STM8S)
                                   1919 
      008657                       1920         HEADER  ULOCK "ULOCK"
      008657                       1921 ULOCK:
      008657 35 AE 50 53      [ 1] 1922         MOV     FLASH_DUKR,#0xAE
      00865B 35 56 50 53      [ 1] 1923         MOV     FLASH_DUKR,#0x56
      00865F 72 07 50 54 FB   [ 2] 1924 1$:     BTJF    FLASH_IAPSR,#3,1$    ; PM0051 4.1 requires polling bit3=1 before writing
      008664 81               [ 4] 1925         RET
                                   1926 
                                   1927 
                                   1928 ;       LOCK  ( -- )
                                   1929 ;       Lock EEPROM (STM8S)
                                   1930 
      008665                       1931         HEADER  LOCK "LOCK"
      008665                       1932 LOCK:
      008665 72 17 50 54      [ 1] 1933         BRES    FLASH_IAPSR,#3
      008669 81               [ 4] 1934         RET
                                   1935         .endif
                                   1936 
                                   1937 ;       ULOCKF  ( -- )
                                   1938 ;       Unlock Flash (STM8S)
                                   1939 
      00866A                       1940         HEADER  UNLOCK_FLASH "ULOCKF"
      00866A                       1941 UNLOCK_FLASH:
      00866A 35 56 50 52      [ 1] 1942         MOV     FLASH_PUKR,#0x56
      00866E 35 AE 50 52      [ 1] 1943         MOV     FLASH_PUKR,#0xAE
      008672 72 03 50 54 FB   [ 2] 1944 1$:     BTJF    FLASH_IAPSR,#1,1$    ; PM0051 4.1 requires polling bit1=1 before writing
      008677 81               [ 4] 1945         RET
                                   1946 
                                   1947 ;       LOCKF  ( -- )
                                   1948 ;       Lock Flash (STM8S)
                                   1949 
      008678                       1950         HEADER  LOCK_FLASH "LOCKF"
      008678                       1951 LOCK_FLASH:
      008678 72 13 50 54      [ 1] 1952         BRES    FLASH_IAPSR,#1
      00867C 81               [ 4] 1953         RET
                                   1954 
                                   1955 ;       SAVEC ( -- )
                                   1956 ;       Minimal context switch for low level interrupt code
                                   1957 ;       This should be the first word called in the interrupt handler
                                   1958 
      00867D                       1959         HEADER  SAVEC "SAVEC"
      00867D                       1960 SAVEC:
      00867D 90 85            [ 2] 1961         POPW    Y
      00867F BE 08            [ 2] 1962         LDW     X,XREG0
      008681 89               [ 2] 1963         PUSHW   X
      008682 AE 07 B0         [ 2] 1964         LDW     X,#ISPP         ; "PC_ISPP" const. top of int. data stack
      008685 90 FC            [ 2] 1965         JP      (Y)
                                   1966 
                                   1967 ;       IRET ( -- )
                                   1968 ;       Restore context and return from low level interrupt code
                                   1969 ;       This should be the last word called in the interrupt handler
                                   1970 
      008687                       1971         HEADER  RESTC "IRET"
      008687                       1972 RESTC:
      008687 85               [ 2] 1973         POPW    X
      008688 BF 08            [ 2] 1974         LDW     XREG0,X         ; restore context
      00868A 80               [11] 1975         IRET                    ; resturn from interrupt
                                   1976 
                                   1977 ;===============================================================
                                   1978 
                           000000  1979         LASTN   =       LINK    ;last name defined
                           00868B  1980         END_SDCC_FLASH = .
                           00001F  1981         USERRAM = RAMPOOL
                                   1982 
                                   1983         .area CODE
                                   1984         .area INITIALIZER
                                   1985         .area CABS (ABS)
