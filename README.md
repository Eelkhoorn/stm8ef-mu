# STM8EF-MU
This repository provides some files that enables muforth (https://github.com/nimblemachines/muforth) to be used in conjunction with stm8ef (https://github.com/TG9541/stm8ef). It makes use of Thomas' stm8ef kernel, but with muforth the interpreter and compiler words are no longer needed and neither are the name fields and link fields. They are striped off, this results in a significantly smaller kernel. The muforth STM8 assembler/disassembler doesn't exist (yet), instead copies of the S08 versions are loaded. The flash area is set to start after the kernel, so muforth's flash-image starts flashing from there and leaves the kernel unchanged. To flash interrupt vectors, use !flash from aliases.

* install muforth and compile
* put the STM8 folder in muforth/mu/target/
* put the tmp folder in muforth/mu/
* replace forth.asm in stm8ef
* add "MUFORTH = 1" to globconf.inc of the target
* replace the .inc files in stm8ef/inc and stm8ef/BOARD
* run make BOARD=MINDEV flash or make BOARD=W1209-FD flash
* make a sym link of stm8ef/out/BOARD/forth.rst in muforth/mu/tmp
* run tmp/kernel.sh words to update the words file
* hook up an uart interface device, make sure muforth/mu/serial-target points to your uart device
* make a sym link from muforth/src/muforth in muforth/mu/tmp 
* start muforth from muforth/mu/tmp:  
       ./muforth -f target/STM8/build.mu4 -f aliases            for MINDEV  
       ./muforth -d W1209 -f target/STM8/build.mu4 -f aliases   for W1209-FD  
* in muforth: chat starts uart communication with the target

Unfortunately muforth has little documentation, although the .mu4 files are sometimes extensively commented. Here is a brief explanation to get you started:

Muforth can be put in different states. A state defines how input commands are interpreted. These are some commands to switch state:  
__building  
__chatting  
__host  
__meta

Muforth has several dictionaries, ie .forth. .meta. .target. .target-runtime. .equates. These are defined in chains.mu4. The state defines which dictionaries are searched and what happens when a word is found. Dictionaries can be chained to each other. The variable current points to the active dictionary. New defined words are added to the active dictionary.

Muforth keeps images of the target memories on the host. For STM8 there are two images: one for flash and one for ram. The variable dp is used to point to one of them. These words switches between them: ram and flash. Flashing the image to the target is done by flash-image.

The word chat starts chatting with the target and puts muforth in chatting state. Now words are searched in the .target. dictionary and executed on the target if found. When a new definition is started ( with : ) muforth is put in __target-colon mode and the new word is added to the .target. dictionary. If you want the word to be compile-only you have to execute [r] , right after the finishing ; .

I have tested this only for the MINDEV and W1209-FD boards. The processor is put in wfi state, so it is active only during interrupts. 

Files can be loaded on the command line (./muforth -f filename) or from within muforth with ld filename. There is a simple timer application for W1209 included that can be flashed:  
./muforth -d W1209 -f target/STM8/build.mu4 -f timer -f aliases  
in muforth do  
chat  
flash-image  
' D6-int INT_EXTI3 !flash  ( write the interrupt vectors)  
' TIM1-int INT_TIM1 !flash  
ULOCK 200 EE.BDL ! LOCK  ( push button bounce delay)  
' init 'BOOT !flash  
COLD  

To be able to chat again the background task has to be stopped:  bgs
