;       STM8L051F3 device and memory layout configuration

        TARGET = STM8L051F3

        RAMEND =        0x03FF  ; "RAMEND" system (return) stack, growing down
        EEPROMBASE =    0x1000  ; "EESTART" EEPROM start address
        EEPROMEND =     0x10FF  ; "EEEND" 256 bytes EEPROM
        CODE_START =	0x8080	; End of interrupt vector area
        FLASHEND =      0x9FFF  ; "FLASHEND" 8K devices
        FLASHBUF_ADDR = 0x0340  ; flash buffer address for muforth flash routine
        FORTHRAM =      0x0000  ; Start of RAM controlled by Forth
        UPPLOC  =       0x0000  ; UPP (user/system area) location for 1K RAM
        SPPLOC  =       0x03D0  ; SPP (data stack top), TIB start
        RPPLOC  =       RAMEND  ; RPP (return stack top)
