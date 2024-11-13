# STM8EF-MU 

### Muforth plugin for STM8
Muforth (https://github.com/nimblemachines/muforth) is a forth console running in a terminal. This great tool is designed to build an image of a microprocessor's memory that can be flashed to a target. The traditional (self hosted) forth approach of programming a micro is flashing the target's memory with a kernel that has an interpreter and a compiler. The target's processor is then used to compile the application. In muforth the host computer is used to interpret words and compile new words (tethered approach). This has the advantage that you no longer need the link fields and name fields in the target's memory. All this administration is performed on the host computer. Here are some of the benefits of this approach:
+ Very efficient use of flash memory: no interpreter or compiler words in the kernel, no name fields or link fields needed.
+ Debugging with gdb is now easier: gdb is no longer confused by the name and link fields.
+ No need to load temporary words like equates or additional compiling words in the target's RAM during compilation. All the equates and libraries can be loaded in muforth and are available during compilation.
+ No restriction in namelength of words that otherwise would consume flash space, so we can use descriptive names to increase the readability of the program.
+ Assembling and disassembling is easy: Michael Kohn's great assembler/disassembler (https://www.mikekohn.net/micro/naken_asm.php) is integrated.

The downside is that we need to keep track of what is flashed to the target if we ever want to modify the flash memory or chat with the taget. The muforth command "cmdl," flashes the command-line options used to start muforth to the end of the flash memory. To find out what is compiled to flash we do: HI . This types the command-line options we compiled in the target. The baudrate and flashend need to be set correctly though.


### Stm8ef
Stm8ef is a forth system originally developped by Chen-Hanson Ting. Thomas@TG9541 further developed the kernel, focussing on cheap and widely available development boards with stm8 processors.  His site (https://github.com/TG9541/stm8ef) has all you need to install a self-hosted forth system on a stm8 device, and includes a thorough documentation. For stm8ef-mu Thomas' forth.asm file is modified: 
+ the interpreter words and the compiler words are stripped off
+ the kernel starts with routines for serial communication with the host computer
+ to improve speed literals are compiled inline (no doLit nor TRAP). 7 bytes, 6 cycles (inline) versus 3 bytes, 39 cycles (TRAP)
+ a few more words are compiled inline to improve speed: DROP, 2DROP, Y>, >Y, R>, >R, R@
+ there are words for fast small loops (i < 255), using 1 byte for the loop index: for1/next1 and do1/loop1, i1 (loop index)

### Procedure
* clone this repository
* Compile muforth from muforth/src: **./configure && make**
* download and install naken_asm  (https://github.com/mikeakohn/naken_asm.git)
* download and install stm8flash  (https://github.com/vdudouyt/stm8flash)
* start muforth from muforth/mu:  
+ for MINDEV : &emsp; &emsp; &emsp; **./muforth -f work/MINDEV/basic**
+ for W1209-FD: &emsp; &emsp; **./muforth -f work/W1209-FD/basic** 
+ for STM8L051F3  &emsp; **./muforth -f work/STM8L0151F3/basic**

Now you can build your application. Save the flash image with "save-image binary-name", exit Muforth ("bye") and flash the saved image to the target with stm8flash and a st-link device. Or simply use tools/fli.sh file-name, this saves the binary to /tmp/image.bin and flashes this binary to the target using stm8flash.
Once you have flashed the kernel to the target you can communicate with it in Muforth over uart: "chat" starts uart interaction.
  
## Muforth
Unfortunately muforth has little documentation, although the .mu4 files are sometimes extensively commented. Here is a brief explanation to get you started:

### Modes
Muforth can be put in different modes. A mode defines how input commands are interpreted, these definitions reside in target/STM8/chains.mu4. Here are some commands to switch mode:
+ __meta      ( create new target words)
+ __chatting  ( chat with target, create new target words)
+ __host      ( normal forth console, create host words)

### Dictionaries
Muforth has several dictionaries, ie .forth. .meta. .target. .target-runtime. .equates. These are defined in chains.mu4. The mode defines which dictionaries are searched and what happens when a word is found. Dictionaries can be chained to each other. The variable **current** points to the active dictionary. New defined words are added to the active dictionary. Immediate words are in a separate dictionary: .compiler. for immediate host words and .target-compiler. for immediate target words.

### Target memory images
Muforth keeps images of the target memories on the host. For STM8 there are three images: for ram, flash eeprom. The 2variable **dp** holds the start address and current position of the active target memory. Words to switch between images: **ram** , **flash** and **eeprom**. Copying the image to the target flash is done by **flash-image**, to eeprom by **eeprom-image**.

### Communication over uart
The word **chat** starts chatting with the target and puts muforth in chatting mode. Now words are searched in the .target. dictionary and executed on the target if found. When a new definition is started ( with **:** ) muforth is put in __target-colon mode and the new word is added to the .target. dictionary. If you want the word to be compile-only you have to execute [r] , right after the finishing **;** , this puts the last created word in .target-runtime.

### Loading files
Files can be loaded on the command line (**./muforth -f filename**) or from within muforth with **ld filename**. The path has to be relative to muforth/mu or absolute.

### Examining memory
+ **_du** ( a) inspects host memory
+ **du**  ( a) inspects the target image on the host (and the target memory if in chat mode)
+ **dis** ( a, len) disassembles (a part of) the image on the host

## Demos
There are several applications available, look in directory work.

### Count-down timer for W1209
There is a simple timer countdown application for W1209-FD included (partly based on Thomas' logging thermostat program).  
+ hookup a programming device (STLINKV2)
+ in muforth/mu : tools/fli.sh work/W1209-FD/timer/timer (this flashes the kernel + the application)
Hookup an uart device and start muforth from muforth/mu
+ **./muforth -f work/W1209-FD/timer/timer**
+ **bgs** ( to stop the background task, it would interfere with the chat process)
+ **chat**
+ **cmdl,** ( to compile " -f work/W1209-FD/timer/timer" to the end of flash)
+ **fl-int** ( to write the interrupt vectors and the boot vector)
+ **init**  ( to start the application, see muforth/mu/work/W1209-FD/timer/README.md)

To be able to chat again the background task has to be stopped: **bgs** 

### Contrloler for a ADF4351 wideband synthesizer board
The synthesizer board is controled by a rotary encoder with a push button. The settings are dispayed on an oled display.
+ hookup a programming device (STLINKV2)
+ in muforth/mu : tools/fli.sh workMINDEV/ADF/ADF (this flashes the kernel + the application)
Hookup an uart device and start muforth from muforth/mu
+ **./muforth -f work/MINDEV/ADF/ADF**
+ **chat**
+ **cmdl,** ( to compile " -f work/ADF/ADF" to the end of flash)
+ **fl-int** ( to write the interrupt vectors and the boot vector)
+ **init** ( to start the application).
