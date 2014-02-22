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
	val dumpStack : stack -> string					(**)
end;

signature RAM =
sig 
	exception MEMMORY of string
	type memmory = int array
	val mem : memmory
	val specialAddresses : int list
	val specialMemmory : int list
	val size : int
	val write : (memmory * int * int ) -> memmory
	val read : (memmory * int) -> int
	val load : (int list * int) -> memmory
	val writeChunk : (memmory * int * int * (int array)) -> memmory
	val readChunk : (memmory * int * int) -> int array
	
	val dump : memmory -> string
end


(*
		Due to horrible and annoying reasons this cant be uncommented. There must be structures using the above signatures before this can be used :(
*)
(*
signature PROGRAM_COUNTER =
sig
	exception COUNTER of string
	include STACK
	datatype pc = Pc of (int * Stack.stack * Register.reg * Register.reg * Register.reg) 	(*pointer, jump stack, argument register, IRQ1, IRQ2*)
	val incrementPointer : (pc * int) -> pc 							(*increments the pointer by an arbitrary integer*)
	val jump : (pc * int) -> pc														(*changes the pointer*)
	val subroutineJump : (pc * int) -> pc 								(*performs a subroutine jump*)
	val return : pc -> pc 																(*returns from subroutine jump*)
	val interupt : (pc * int) -> pc 											(*performs a interupt jump. The integer specifys wich IRQ register to be used. *)
	val getArg : (pc) -> int															(*returns the value of the argument register*)
end
*)