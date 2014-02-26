exception RUNTIME of string

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
	datatype stack = Stack of (int list)
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
	val initialize : int -> memory 							(*Creates a memmory of size specified by the integer*)
	val getSize : (memory) -> int 								(*Returns the size of the memory.*)
	val write : (memory * int * int ) -> memory	  (*Wrties to memory*)
	val read : (memory * int) -> int							(*Reads to memory*)
	val load : (memory* int list) -> memory								(*Loads a list of integers into the memory*)
	val writeChunk : (memory * int * (int array)) -> memory	 (*Writes a "chunk" of memory. To be used by prepherials"*)
	val readChunk : (memory * int * int) -> int array				(*Retrives a "chunk" of memory. To be used by prepherials"*)
	val dump : memory -> string										(*Dumps the memory into a pretty string*)
end

(*These are just place holders. Here to make sure that the PROMGAM_COUNTER signature can use the correct datatypes  
Your code can go in here if you want to. Just remeber to uncomment the :> and then add the correct data type.
*)

structure Register :> REGISTER=
struct
	exception REGISTER
	datatype reg = Reg of int
	
	fun setData (Reg(_), new) = Reg(new)

	fun getData (Reg(i)) = i
	
	fun increment (Reg(i)) = Reg(i+1)
	handle Overflow => raise RUNTIME "Register overflow"
	
	fun decrement (Reg(i)) = Reg(i-1)
	handle Overflow => raise RUNTIME "Register underflow"
	
	fun dumpRegister (Reg(i)) = Int.toString(i)^"\n"
end

structure Stack :> STACK=
struct
	exception STACK of string
	datatype stack = Stack of (int list)
	
	fun push (Stack(s), inp) = Stack(inp::s)
	
	(*This function should return a new stack without the top stackframe*) (* you mean like this, I hope *) (*Yes just like this!*)
	fun pop (Stack([])) = raise STACK "Can't pop an empty stack"
	  | pop (Stack(x::xs)) = Stack(xs)                        
		
	fun top (Stack([])) = raise STACK "Can't read an empty stack"
	  | top (Stack(x::xs)) = x
	
	fun isEmpty (s) = s = Stack([])
	
	fun dumpStack (Stack([])) = "Empty\n"
	  |dumpStack (Stack(s)) = String.concatWith "," (map Int.toString (s))^"\n"
end

structure Ram :> RAM =
struct

	exception memory of string

	type memory = int array

	fun initialize (i) = Array.array (i, 0)
	
	fun getSize(ram) = Array.length(ram)
	
	(*Not the slight but vital difference here. And contemplate upon it.*)
	fun write (ram, address, i) = (Array.update(ram, address, i); ram)
	
	fun read (ram, i) = Array.sub(ram, i)

	fun rlist (length) = List.tabulate (length, (fn x => x))

	fun reader (ram,[]) = []
	  |reader (ram,x::xs) = Array.sub(ram, x)::reader(ram,xs)
	
	fun load (ram, []) = ram
	|load (ram,x::xs) = load'(ram, xs, x)
	and load' (ram,[], _) = ram
	|load' (ram, x::xs, i) = (Array.update(ram, i, x); load'(ram, xs, i+1))   (* heh, update with side-effect, then recursion*)(*Yupp! Thats the way to go!*)
	
	(*I removed the integer. It was supposed to be write from here to there but the there was stupid
		Allso added the vital and magic ingredient.
	*)
	fun writeChunk (ram, adress, sorc) = (Array.copy{src = sorc, dst = ram, di = adress}; ram)
	
	fun readChunk (ram, address, length) = Array.fromList(reader (ram, map (fn x => x+address) (rlist(length))))  
	
	fun dump (ram) = 
		let
			val dumpList = List.tabulate (Array.length(ram), (fn x => x))
			fun dumpString ([],[]) = ""
			  | dumpString (x::xs, y::ys) = (Int.toString(x) ^ ":" ^ Int.toString(y) ^ "\n" ^ dumpString(xs,ys))
		in
			"RAM:\n"^dumpString(dumpList, (reader (ram,dumpList)))
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
	val incrementPointer : (pc * int) -> pc 			(*increments the pointer by an arbitrary integer*)
	val jump : (pc * int) -> pc					(*changes the pointer*)
	val subroutineJump : (pc * int) -> pc 				(*performs a subroutine jump*)
	val return : pc -> pc 						(*returns from subroutine jump*)
	val interupt : (pc * int) -> pc 				(*performs a interupt jump. The integer specifys wich IRQ register to be used. *)
end

structure ProgramCounter (*:> PROGRAM_COUNTER*) =
struct
	datatype pc = Pc of (int * Stack.stack * Register.reg * Register.reg)
	
	(*
	Guys remeber that you have to return the correct datatype nut just a tuple.
	This is very important. (i,stack,irq1,irq2) is not the same thing as Pc(i,stack,irq1,irq2)
	*)
	
	(*You missed the int. You will want to be able to increment the pointer with an arbitrary number*)
	fun incrementPointer (Pc(i,stack,irq1,irq2),a) = Pc(i+a,stack,irq1,irq2)
	
	fun jump (Pc(i,stack,irq1,irq2),jump) = Pc(jump,stack,irq1,irq2)
end


(*
The reason for using the placeholder structures Register and Stack is because signatures can't "inherit" from eachother.
I can not write say REGISTER.reg in the PROGRAM_COUNTER signature.
There is the include keyword but that just adds all of the declarations from another signature. Thus specifying that the 
signature containing the include must when its used in a structure allso include all of the functions and values in the included
signature. 
*)
