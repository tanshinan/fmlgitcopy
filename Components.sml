exception RUNTIME of string

(*
	This is the signature describing the workings of the registers
*)
signature REGISTER =
sig
	exception REGISTER
	datatype reg = Reg of int
	val setData : (reg * int) -> reg				(*Set data*)
	val getData : reg -> int						(*gets data*)
	val increment : reg -> reg						(*increments register*)
	val decrement : reg -> reg						(*decrements register*)
	val dumpRegister : reg -> string				(*dumps register to a nice formatted string*)
end;

(*
	This signature describes the workings of the stacks
*)

signature STACK =
sig
	exception STACK of string
	datatype stack = Stack of (int list)
	val empty : stack
	val push : stack * int -> stack					(*Pushes a value onto the stack*)
	val pop : stack -> stack						(*Pops a value off the stack*)
	val top : stack -> int							(*returns the value from the top of the stack*)
	val isEmpty : stack -> bool						(*returns true if the stack is empty*)
	val dumpStack : stack -> string					(*returns a pretty string of what is on the stack*)
end;
(*
	This signature describes the workings of the ram
*)

signature RAM =
sig
	exception memory of string
	type memory = int array
	val initialize : int -> memory								(*Creates a memory of size specified by the integer*)
	val getSize : (memory) -> int								(*Returns the size of the memory.*)
	val write : (memory * int * int ) -> memory					(*Writes to memory*)
	val read : (memory * int) -> int							(*Reads to memory*)
	val load : (memory* int list) -> memory						(*Loads a list of integers into the memory*)
	val writeChunk : (memory * int * (int array)) -> memory		(*Writes a "chunk" of memory. To be used by peripherals"*)
	val readChunk : (memory * int * int) -> int array			(*Retrieves a "chunk" of memory. To be used by peripherals"*)
	val dump : memory -> string									(*Dumps the memory into a pretty string*)
end

(*
This is the structures of the register. Where we assign the functions for the signature Register.
*)
structure Register :> REGISTER=
struct
	exception REGISTER

(*
DATATYPE CONVENTIONS: Where reg is an int 
DATATYPE INVARIANTS: Register can only take ints
*)
	datatype reg = Reg of int
	(*
		setData
		TYPE: Reg * int -> reg
		PRE:()
		POST: gets a new Register
		EXAMPLE:
	*)
	fun setData (Reg(_), new) = Reg(new)

	(*
		getData i
		TYPE: Reg -> int
		PRE:()
		POST: Get the data from the Register.
		EXAMPLE:
	*)
	fun getData (Reg(i)) = i

	(*
		increment i
		TYPE: Reg -> reg
		PRE:()
		POST: Moves the place of the Register Pointer to next register
		EXAMPLE:
	*)
	fun increment (Reg(i)) = Reg(i+1)
	handle Overflow => raise RUNTIME "Register overflow"

	(*
		decrement i
		TYPE: Reg -> reg
		PRE:()
		POST: Move the pointer to the previous register pointer
		EXAMPLE:
	*)
	fun decrement (Reg(i)) = Reg(i-1)
	handle Overflow => raise RUNTIME "Register underflow"

	(*
		dumpRegister i
		TYPE: Reg -> string
		PRE:()
		POST: gives a string of the Reg of i
		EXAMPLE:
	*)
	fun dumpRegister (Reg(i)) = Int.toString(i)
end
(*
This is the functions of the stack signatures
*)
structure Stack :> STACK=
struct
	exception STACK of string

(*
DATATYPE CONVENTIONS: Stack is a int list, but only the fist element can be manipulated   
DATATYPE INVARIANTS: none
*)

	datatype stack = Stack of (int list)
(* assigns empty to a Stack *)
	val empty = Stack([])
	(*
		push s
		TYPE: stack * int -> stack
		PRE:()
		POST: adds a value to the stack
		EXAMPLE:
	*)
	fun push (Stack(s), inp) = Stack(inp::s)
	(*
		pop s
		TYPE: stack -> stack
		PRE:()
		POST: Removes the first element of the stack s, until its empty
		EXAMPLE:
	*)
	fun pop (Stack([])) = raise STACK "Can't pop an empty stack"
	  | pop (Stack(x::xs)) = Stack(xs)

	(*
		top s
		TYPE: stack -> int
		PRE:()
		POST: takes the first element of stack s.
		EXAMPLE:
	*)
	fun top (Stack([])) = raise STACK "Can't read an empty stack"
	  | top (Stack(x::xs)) = x

	(*
		isEmpty s
		TYPE: stack -> bool
		PRE:()
		POST: Checks if stack s is empty
		EXAMPLE:
	*)
	fun isEmpty (s) = s = Stack([])

	(*
		dumpStack s
		TYPE: stack -> string
		PRE:()
		POST: Gives stack s as strings where the elements are divided by ,
		EXAMPLE:
	*)
	fun dumpStack (Stack([])) = "Empty"
	  |dumpStack (Stack(s)) = String.concatWith "," (map Int.toString (s))
end
(*
This is the functions of the ram signatures
*)
structure Ram :> RAM =
struct

	exception memory of string

	type memory = int array

	(*
		initialize i
		TYPE: int -> memory
		PRE:()
		POST: gives a ram of size i
		EXAMPLE:
	*)
	fun initialize (i) = Array.array (i, 0)

	(*
		getSize i
		TYPE: memory -> int
		PRE:()
		POST: gets the size of memory i
		EXAMPLE:
	*)
	fun getSize (ram) = Array.length(ram)

	(*
		write ram, address, i
		TYPE: memory * int * int -> memory
		PRE:()
		POST: Gives a ram with a new element of i at address
		EXAMPLE:
	*)
	fun write (ram, address, i) = (Array.update(ram, address, i); ram)

	(*
		read ram, i
		TYPE: memory * int -> int
		PRE:()
		POST: Gives a int from ram at i
		EXAMPLE:
	*)
	fun read (ram, i) = Array.sub(ram, i)

	(*
		reader ram,[]
		TYPE: 'a array * int list -> 'a list
		PRE:()
		POST:
		EXAMPLE:
	*)
	fun reader (ram,[]) = []
	  |reader (ram,x::xs) = Array.sub(ram, x)::reader(ram,xs)

	(*
		load (ram, l)
		TYPE:  memory * int list -> memory
		PRE:()
		POST: Adds l to ram
		EXAMPLE:
	*)
	fun load (ram, []) = ram
	|load (ram,x::xs) = load'(ram, xs, x)
	and load' (ram,[], _) = ram
	|load' (ram, x::xs, i) = (Array.update(ram, i, x); load'(ram, xs, i+1))

	(*
		writeChunk ram, address, sorc
		TYPE: memory *int *int array -> memory
		PRE:()
		POST: inserts the sorc in the ram at the start of address
		EXAMPLE:
	*)
	fun writeChunk (ram, address, sorc) = (Array.copy{src = sorc, dst = ram, di = address}; ram)

	(*
		readChunk ram, address, length
		TYPE: memory * int * int -> int arry
		PRE:()
		POST: reads the ram from address and to the length
		EXAMPLE:
	*)
	fun readChunk (ram, address, length) =
		let
			val rlist = List.tabulate (length, (fn x => x))
		in
	Array.fromList(reader (ram, map (fn x => x+address) (rlist)))
	end

	(*
		dump ram
		TYPE: ram -> string
		PRE:()
		POST: dumps the content of the memory to a string.
		EXAMPLE:
	*)
	fun dump (ram) =
		let
			val dumpList = List.tabulate (Array.length(ram), (fn x => x))
			fun dumpString ([],[]) = ""
			  | dumpString (x::xs, y::ys) = (Int.toString(x) ^ ":" ^ Int.toString(y) ^ "," ^ dumpString(xs,ys))
		in
			"RAM =>> " ^ dumpString(dumpList, (reader (ram,dumpList)))
		end

end



(*
This is the description of the PROGRAM_COUNTER
*)

signature PROGRAM_COUNTER =
sig
	exception COUNTER of string
	datatype pc = Pc of (int * Stack.stack * Register.reg * Register.reg)	(*pointer, jump stack,  IRQ1, IRQ2*)
	val incrementPointer : (pc * int) -> pc									(*increments the pointer by an arbitrary integer*)
	val jump : (pc * int) -> pc												(*changes the pointer*)
	val subroutineJump : (pc * int * int) -> pc								(*performs a subroutine jump*)
	val return : pc -> pc													(*returns from subroutine jump*)
	val interrupt : (pc * int) -> pc										(*performs an interrupt jump. The integer specifies wich IRQ register to be used. *)
	val dumpPc : pc -> string
end


(*
This is the functions of the PROGRAM_COUNTER signatures
*)

structure ProgramCounter :> PROGRAM_COUNTER =
struct
	exception COUNTER of string
(*
DATATYPE CONVENTIONS:  Pc (i, stack,irq1,irq2), where i is an integer that handles as a pointer, 
Stack as the Stack data type above and irq 1 and 2 is of data type register
DATATYPE INVARIANTS: This data type is restrictive to the cases above. 
*)

	datatype pc = Pc of (int * Stack.stack * Register.reg * Register.reg)

	(*
		incrementPointer (Pc(i,s q1,q2),a)
		TYPE: pc * int -> pc
		PRE:()
		POST: Adds a to the pointer i
		EXAMPLE:
	*)
	fun incrementPointer (Pc(i,s,q1,q2),a) = Pc(i+a,s,q1,q2)

	(*
		jump (Pc(i,s q1,q2),jump)
		TYPE: pc *int -> pc
		PRE:()
		POST: Jumps the Pc pointer i to the value of Jump
		EXAMPLE:
	*)
	fun jump (Pc(i,s,q1,q2),jump) = Pc(jump,s,q1,q2)

	(*
		subroutineJump (Pc(i,s q1,q2),jump, jumpsize)
		TYPE: pc * int * int -> pc
		PRE:()
		POST: jumps the pc pointer i the the value of jump
		EXAMPLE:
	*)
	fun subroutineJump (Pc(i, s, q1, q2), jump, l) = Pc(jump, Stack.push(s,
		i+l), q1, q2)

	(*
		return (Pc(i,s q1,q2),a)
		TYPE: pc -> pc
		PRE:()
		POST: Pops the value of í and adds it to s
		EXAMPLE:
	*)
	(* raises STACK when empty stack *)
	fun return (Pc(i, s, q1, q2)) = Pc(Stack.top(s), Stack.pop(s), q1, q2)

	(*
		interrupt  (Pc(i,s q1,q2),a)
		TYPE: pc *int -> pc
		PRE:()
		POST: if the value of a is 1 or 2, then the value of i is added to s
		EXAMPLE:
	*)
	fun interrupt (Pc(i, s, irq1 as Register.Reg(q1), irq2 as Register.Reg(q2)), x) =
		case x of
			1 => Pc(q1, Stack.push(s, i), irq1, irq2)
		  | 2 => Pc(q2, Stack.push(s, i), irq1, irq2)
		  | _ => raise COUNTER "Can't use nonexistent IRQ"

	fun dumpPc (Pc(i, stack, q1, q2)) = "PC: " ^ (Int.toString(i)) ^ ", " ^ (Stack.dumpStack(stack)) ^ ", " ^ (Register.dumpRegister(q1)) ^ ", " ^ (Register.dumpRegister(q2))

end


