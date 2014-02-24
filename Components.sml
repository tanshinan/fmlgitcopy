exception RUNTIME

(*
	This is the signature describing the workings of the registers
*)
signature REGISTER = 
sig
	exception REGISTER
	datatype access = READ | WRITE | BOTH	(*Specifies weather or not an argument is read only or write only*)
	type reg = (access * int) 							(*acces type, value*)
	val setData : (reg * int) -> reg 				(*Set data*)
	val getData : reg -> int								(*gets data*)
	val increment : reg -> reg							(*increments register*)
	val decrement : reg -> reg							(*decrements register*)
	val dumpRegister : reg -> string				(*dumnps register to a nice formatted string*)
end;

(*
	This signature describes the workings of the stacks
*)
signature STACK =
sig
	exception STACK of string
	datatype stack = Stack of (int list) | Empty
	val push : stack * int -> stack					(*Pushes a value onto the stack*)
	val pop : stack -> stack								(*Pops a value off the stack*)
	val top : stack -> int									(*returns the value from the top of the stack*)
	val isEmpty : stack -> bool							(*returns true if the stack is empty*)
	val dumpStack : stack -> string					(*returns a pretty string of what is on the stack*)
end;

signature RAM =
sig 
	exception memory of string
	type memory = int array								
	val mem : memory															(*The actual memory*)
	val specialAddresses : int list								(*List of special "negative" addresses*)
	val specialmemory : int list									(*List containing the values of the special addresses*)
	val size : int																(*Size of the memory. Not counting special addresses*)
	val write : (memory * int * int ) -> memory	  (*Wrties to memory*)
	val read : (memory * int) -> int							(*Reads to memory*)
	val load : (int list * int) -> memory					(*Loads a list of integers into the memory*)
	val writeChunk : (memory * int * int * (int array)) -> memory	(*Writes a "chunk" of memory. To be used by prepherials"*)
	val readChunk : (memory * int * int) -> int array							(*Retrives a "chunk" of memory. To be used by prepherials"*)
	val dump : memory -> string										(*Dumps the memory into a pretty string*)
end

(*These are just place holders. Here to make sure that the PROMGAM_COUNTER signature can use the correct datatypes  
Your code can go in here if you want to. Just remeber to uncomment the :> and then add the correct data type.
*)

structure Register (*:> REGISTER*)=
struct
	datatype reg = unit
end

structure Stack (*:> STACK*)=
struct
	datatype stack = unit
end

structure Ram (*:> RAM*) =
struct
	datatype memory = unit
end



(*
	You cant really start working on this untill the Register and Stack structures are completed.
	You shold choose how to implement
*)
signature PROGRAM_COUNTER =
sig
	exception COUNTER of string
	datatype pc = Pc of (int * Stack.stack * Register.reg * Register.reg) 	(*pointer, jump stack,  IRQ1, IRQ2*)
	val incrementPointer : (pc * int) -> pc 							(*increments the pointer by an arbitrary integer*)
	val jump : (pc * int) -> pc														(*changes the pointer*)
	val subroutineJump : (pc * int) -> pc 								(*performs a subroutine jump*)
	val return : pc -> pc 																(*returns from subroutine jump*)
	val interupt : (pc * int) -> pc 											(*performs a interupt jump. The integer specifys wich IRQ register to be used. *)
end

structure ProgramCounter (*:> PROGRAM_COUNTER*) =
struct
	datatype pc = unit
end


(*
The reason for using the placeholder structures Register and Stack is because signatures can't "inherit" from eachother.
I can not write say REGISTER.reg in the PROGRAM_COUNTER signature.
There is the include keyword but that just adds all of the declarations from another signature. Thus specifying that the 
signature containing the include must when its used in a structure allso include all of the functions and values in the included
signature. 
*)
