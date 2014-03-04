use "Components.sml";

(*
	create the structures in the Components file before you start working on this.
*)
signature VIRTUAL_MACHINE =
sig
	exception RUNTIME
	(*DATATYPE*)
	datatype flag = HALT | INTERRUPT of int | OVERFLOW | RUNNING
	datatype vm = Vm of (ProgramCounter.pc * Register.reg * Stack.stack * Register.reg * Register.reg * Ram.memory * flag)
	
	val init : (int list * int) -> vm 	(*Creates an initialized VM. Loads int list into the memory and makes the memory have a size of the second int*)
	val step : vm -> vm					(*Takes one vm and returns the next vm. I.e it runns the virtual machine for one step.*) (*shouldn't step take more than just the vm? So we can know if it's going to move the pointer 1 or 2? *)
	val dump : vm -> unit				(*Prints the VM to stdOut. The output should be really pretty to make debugging easy.*)
	
end
(*This is the functions of the signature Virtual_Machine*)
structure Vm (*:> VIRTUAL_MACHINE *)=
struct
	exception RUNTIME
	(*DATATYPE*)
	datatype flag = HALT | INTERRUPT of int | OVERFLOW | RUNNING
	datatype vm = Vm of (ProgramCounter.pc * Register.reg * Stack.stack * Register.reg * Register.reg * Ram.memory * flag)

	(* init (intlist, integ)
		TYPE: (int list * int) -> vm
		PRE: length(intlist) < int
		POST: A VM loaded with intlist
		EXAMPLE: init ([1,2,3],5) = Vm(ProgramCounter.Pc(0, Stack.Stack([]), Register.Reg(0), Register.Reg(0)), Register.Reg(0), Stack.Stack([]), Register.Reg(0), Register.Reg(0), Ram(Array.fromList [0,2,3,0,0]), RUNNING)
	*)
	fun init (ilist, i) = 
	    let 
		val mem = Ram.initialize (i)
	    in 
		Vm(ProgramCounter.Pc(0, Stack.Stack([]), Register.Reg(0), Register.Reg(0)), Register.Reg(0), Stack.Stack([]), Register.Reg(0), Register.Reg(0), (Ram.load (mem, ilist); mem), RUNNING)
	    end
	    
	(* step vm
		TYPE: vm -> vm
		PRE: true
		POST: VM ran one cycle
		EXAMPLE: here be dragons (meaning, I'll fill it in later)
	*)
	(*
		fun step (Vm()) =  
	*)
	
	(*Odd name!?*)
	(*flagComprehension flag
			TYPE: flag -> string
			PRE: true
			POST: string corresponding to flag
			EXAMPLE: HALT = "HALT"
			INTERRUPT(4) = "4"
	*)
	fun flagComprehension (fl: flag) = 
		case fl of HALT => "HALT"
		| OVERFLOW => "OVERFLOW"
		| RUNNING => "RUNNING"
		| INTERRUPT(a) =>  Int.toString (a)

			 (* when we tried to use stdOut it crashed poly *)
	(* dumpToFile vm
		TYPE: vm -> unit
		PRE: true
		POST: file with the state of vm written in easily readable text
		EXAMPLE: here be dragons (oh god, how the hell do you write how it creates a file)
	*)	
	fun dumpToFile (Vm(pc, a, s, x, y, ram, fl)) = 
		let
			val dumpstream = TextIO.openOut "vm_dump"
		in
			(TextIO.output (dumpstream, (ProgramCounter.dumpPc(pc) ^ "\n" ^ (Register.dumpRegister(a))^ ", " ^(Stack.dumpStack(s))^ ", " ^ (Register.dumpRegister(x))^ ", " ^ (Register.dumpRegister(y))^ "\n" ^ (Ram.dump(ram)) ^ "\n" ^ flagComprehension(fl) ^ "\n")); TextIO.closeOut dumpstream)
		end
	(* dump vm
		TYPE: vm -> unit
		PRE: true
		POST: state of vm printed in prompt in easily readable text
		EXAMPLE: here be dragons
	*)	
	fun dump (Vm(pc, a, s, x, y, ram, fl)) = print  ((ProgramCounter.dumpPc(pc) ^ "\n" ^ (Register.dumpRegister(a))^ ", " ^(Stack.dumpStack(s))^ ", " ^ (Register.dumpRegister(x))^ ", " ^ (Register.dumpRegister(y))^ "\n" ^ (Ram.dump(ram)) ^ "\n" ^ flagComprehension(fl) ^ "\n"))
	
end

(*
	I'm not going to give you much more clues about how to go about writing this. Its not as hard as you think.
	It might get pretty ugly but that is not the end of the world.
*)
