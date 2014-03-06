These are utilities which may be of importance to all aspects of the VM implementation and the assembler.

One of the reasons why SML rhymes with FML is that in order to get files from different folders one has to do this crap!

val current_dir = OS.FileSys.getDir();	(*Figure out where we are*)
OS.FileSys.chDir("Utills");				(*Move to directory*)
use "------.sml"; 						(*Import crap*)
OS.FileSys.chDir(current_dir);			(*Change back to original directory*)