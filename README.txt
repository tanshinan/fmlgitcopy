:::::::::::::::CODING STANDARD:::::::::::::
This entire project is to be written in English!
Use TABS instead of spaces. http://lea.verou.me/2012/01/why-tabs-are-clearly-superior/
Use descriptive names for functions, types etc.
Always use a catch all clause if applicable.
Always leave an empty line between functions.
Use encapsulation when appropriate.
And the general philosophy should to avoid ambiguity.
:::::::::::::::::::::::::::::::::::::::::::


Dudes the Assembler is now fully functional! 
Some details and fancypantsy things left.

Use English for all files including diarys and logs etc. 
Lets keep the project consise.

Use structures in all files. Don't be afraid to use nested structures as well. 
You are not ugly if nobody can see you!
http://java.dzone.com/articles/why-encapsulation-matters

¡MUY IMPORTANTE!
When a return jump (RET) it must return to the address of the jump stack + 1 (or two if a non register argument is used) !!
Otherwise it would return to the address where the original subroutine jump is
and the VM would get stuck in a loop.