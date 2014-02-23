Get working on the VM! You have the specs! Get started!

Dudes the Assembler is now fully functional! 
Some details and fancypantsy things left.

Use English for all files including diarys and logs etc. 
Lets keep the project consise.

Use structures in all files. Don't be afraid to use nested structures as well. 
You are not ugly if nobody can see you!
http://java.dzone.com/articles/why-encapsulation-matters

¡MUY IMPORTANTE!
When a return jump (RET) it must return to the address of the jump stack + 2 !!
Otherwise it would return to the address where the original subroutine jump is
and the VM would get stuck in a loop.