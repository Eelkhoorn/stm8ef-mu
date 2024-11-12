;       STM8L152K4 device and memory layout configuration

        TARGET = STM8L151K4

        RAMEND =        0x07FF  ; "RAMEND" system (return) stack, growing down
        EEPROMBASE =    0x1000  ; "EESTART" EEPROM start address
        EEPROMEND =     0x13FF  ; "EEEND" 1024 bytes EEPROM
	CODE_START =	0x8080	; End of interrupt vector area
        FLASHEND =      0xFFFF  ; 32K devices
        FLASHBUF_ADDR = 0x0740  ; flash buffer address for muforth flash routine
        FORTHRAM =      0x0000  ; Start of RAM controlled by Forth
        UPPLOC  =       0x0000  ; UPP (user/system area) location for 2K RAM
        SPPLOC  =       0x07D0  ; SPP (data stack top), TIB start
        RPPLOC  =       RAMEND  ; RPP (return stack top)
