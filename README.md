# STM8EF-MU
This repository provides some files that enables muforth (https://github.com/nimblemachines/muforth) to be used in conjunction with stm8ef (https://github.com/TG9541/stm8ef). It makes use of Thomas' stm8ef kernel, but in muforth the interpreter and compiler words are no longer needed and neither are the name fields and link fields. They are stripped off, this results in a significantly smaller kernel. The muforth STM8 assembler/disassembler doesn't exist (yet), instead copies of the S08 versions are loaded. The flash area is set to start after the kernel, so muforth's **flash-image** starts flashing from there and leaves the kernel unchanged. To flash interrupt vectors, use **!flash** from aliases.  

### Procedure
* install muforth and compile from muforth/src: **./configure && make**
* install stm8ef
* make sym links to your stm8ef and muforth folders:  
**ln -s /home/$USER/muforth**  
**ln -s /home/$USER/stm8ef**
* do **./run** , this makes backup files (*_original) of certain stm8ef files and replaces them with sym links. **./undo** restores the backup files.
* in stm8ef run **make BOARD=...... flash**
* make a sym link of **stm8ef/out/BOARD/forth.rst** in **muforth/mu/work**
* check the symlink in muforth/mu/tools/kernel.sh, it should point to muforth/mu/target/STM8/words.mu4
* in mu/work run **./kernel.sh** to update the file words (containing the kernel words) and the link in muforth/mu/target/STM8
* hook up an uart interface device, make sure **muforth/mu/serial-target** points to your uart device
* start muforth from muforth/mu:  
for MINDEV : &emsp; &emsp; &emsp; **./muforth -f target/STM8/build.mu4 -f work/aliases**   
for W1209-FD: &emsp; &emsp; **./muforth -d W1209 -f target/STM8/build.mu4 -f work/aliases**  
for STM8L051F3  &emsp; **./muforth -d STM8L -f target/STM8/build.mu4 -f work/aliases**  
muforth commandline options:  
  **-d W1209** sets baud rate to 57600 in stead of 115200
  **-d STML** loads STM8L in stead of STM8S equates
* in muforth: **chat** starts uart communication with the target
  
## Muforth
Unfortunately muforth has little documentation, although the .mu4 files are sometimes extensively commented. Here is a brief explanation to get you started:

### Modes
Muforth can be put in different modes. A mode defines how input commands are interpreted. These are some commands to switch mode:  
+ __meta      ( create new target words)
+ __chatting  ( chat with target, create new target words)
+ __host      ( normal forth console, create host words)

### Dictionaries
Muforth has several dictionaries, ie .forth. .meta. .target. .target-runtime. .equates. These are defined in chains.mu4. The mode defines which dictionaries are searched and what happens when a word is found. Dictionaries can be chained to each other. The variable **current** points to the active dictionary. New defined words are added to the active dictionary. Immediate words are in a separate dictionary: .compiler. for immediate host words and .target-compiler. for immediate target words.

### Target memory images
Muforth keeps images of the target memories on the host. For STM8 there are three images: for ram, flash eeprom. The 2variable **dp** holds the start address and current pusition of the active target memory. Words to switch between images: **ram** , **flash** and **eeprom**. Copying the image to the target flash is done by **flash-image**, to eeprom by **eeprom-image**.

### Communication over uart
The word **chat** starts chatting with the target and puts muforth in chatting mode. Now words are searched in the .target. dictionary and executed on the target if found. When a new definition is started ( with **:** ) muforth is put in __target-colon mode and the new word is added to the .target. dictionary. If you want the word to be compile-only you have to execute [r] , right after the finishing **;** , this puts the last created word in .target-runtime.

### Loading files
Files can be loaded on the command line (**./muforth -f filename**) or from within muforth with **ld filename**. The path has to be relative to muforth/mu or absolute.

### Examining memory
+ **_du** ( a) inspects host memory
+ **du**  ( a) inspects the target image on the host (and the target memory if in chat mode)

## Demo
There is a simple timer application for W1209-FD included that can be flashed:  
Start muforth from muforth/mu
+ ./muforth -d W1209 -f target/STM8/build.mu4 -f work/timer -f work/aliases  

in muforth do:
+ chat  
+ flash-image  

( write the interrupt vectors:)
+ ' D6-int INT_EXTI3 !flash  
+ ' TIM1-int INT_TIM1 !flash  
+ #200 EE.BDL !eeprom  ( push button bounce delay)  
+ ' init 'BOOT !flash  
+ COLD  

To be able to chat again the background task has to be stopped: **bgs**
