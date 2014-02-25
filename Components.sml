exception RUNTIME

(*
	This is the signature describing the workings of the registers
*)
signature REGISTER = 
sig
	exception REGISTER
	datatype reg = Reg of int 
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
	val mem : memory							(*The actual memory*)
	val initialize : int -> memmory 							(*Creates a memmory of size specified by the integer*)
	val getSize : (memory) -> int 								(*Returns the size of the memory.*)
	val write : (memory * int * int ) -> memory	  (*Wrties to memory*)
	val read : (memory * int) -> int							(*Reads to memory*)
	val load : (int list) -> memory								(*Loads a list of integers into the memory*)
	val writeChunk : (memory * int * int * (int array)) -> memory	(*Writes a "chunk" of memory. To be used by prepherials"*)
	val readChunk : (memory * int * int) -> int array				(*Retrives a "chunk" of memory. To be used by prepherials"*)
	val dump : memory -> string										(*Dumps the memory into a pretty string*)
end

(*These are just place holders. Here to make sure that the PROMGAM_COUNTER signature can use the correct datatypes  
Your code can go in here if you want to. Just remeber to uncomment the :> and then add the correct data type.
*)

structure Register :> REGISTER=
struct
	exception REGISTER
	type reg = int
	
	fun setData (Reg(_), new) = Reg(new)

	fun getData Reg(i) = a
	
	(*
		Guys. seriously this is what you had writen:
			increment (WRITE, int) = (WRITE, int+1)
		You cant use "int" as a name for an argument. Even though the compiler doesn't care its
		verry innapropriate to mix things like this and it should be avoided at all costs.
		int is a type! and should for ever and always be nothing more than just that.
	*)
	fun increment Reg(i) = Reg(i+1)
	handle Overflow => raise RUNTIME "Register overflow"
	
	fun decrement Reg(i) = Reg(i-1)
	handle Overflow => raise RUNTIME "Register underflow"
	
	fun dumpRegister Reg(i) = Int.toString(int)
end

structure Stack :> STACK=
struct
	exception STACK of string
	datatype stack = Stack of (int list) | Empty
	
	fun push (Empty, inp) = Stack([inp])
	|push (Stack(stack), inp) = Stack(inp::stack)
	
	(*This function should return a new stack without the top stackframe*)
	fun pop (Stack(stack)) = Stack(stack)                        
	|pop (Empty) = raise STACK "Can't pop an empty stack"
		
	fun top (Stack(x::xs)) = x
	|top (Empty) = raise STACK "Can't read an empty stack"
	
	fun isEmpty (stack) = stack = Empty
	
	fun dumpStack (Empty) = "Empty"
	|dumpStack (Stack(stack)) = concat(map Int.toString (stack))
end

structure Ram (*:> RAM*) =
struct
	exception memory of string
	type memory = int array								

	fun write (ram, address, int) = Array.update(ram, address, int)
	
	fun read (ram, int) = Array.sub(ram, int)
	
	(*
		The int should not have been there in the first place. But i was unclear in the specifications and forgott to add an important thing.
		The first number in the list to be loaded is the adress in the memmory where the list is to be loaded.
		The first number in the list is the "base adress" and should not be loaded into the memmory.
	*)
	fun load (list) = Array.fromList (list)
	
	fun writeChunk (ram, adress, int, sorc) = Array.copy{src = sorc, dst = ram, di = adress}  (* vad ska int g�ra *)
	
	fun reader (ram, []) = []
	|reader (ram, x::xs) = Array.sub(ram, x)::reader(ram,xs)
	
	fun readChunk (ram, adress, length) =
		let
			val rlist = List.tabulate (length, (fn x => x+adress))
		in
			Array.fromList(reader (ram, rlist))
		end 
	
	
	(*Please use descriptive names for all functions and values.*)
	fun dump (*hahahaha*) (ram) = 
		let
			val dicklist = List.tabulate (Array.length(ram), (fn x => x))
			fun ful ([],[]) = ""
			| ful (x::xs,y::ys) = (Int.toString(x) ^ ":" ^ Int.toString(y) ^ "," ^ ful(xs,ys))
		in
			ful(dicklist, (reader (ram, dicklist)))
		end
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
