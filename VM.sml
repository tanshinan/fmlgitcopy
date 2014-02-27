use "Components.sml";


(*
	create the structures in the Componenets file before you start working on this.
*)
signature VIRTUAL_MACHINE =
sig
	exception RUNTIME
	datatype flag = HALT | INTERRUPT of int | OVERFLOW | RUNNING
	datatype vm = Vm of (ProgramCounter.pc * Register.reg * Stack.stack * Register.reg * Register.reg * Ram.memory * flag)
	
	val init : (int list * int) -> vm (*Creates an initialized VM. Loads int list into the memory and makes the memory have a size of the second int*)
	val step : vm -> vm				(*Takes one vm and returns the next vm. I.e it runns the virtual machine for one step.*)
	val dump : vm -> unit				(*Prints the VM to stdOut. The output should be really pretty to make debugging easy.*)
	
end

structure Vm (*:> VIRTUAL_MACHINE*) =
struct
	exception RUNTIME
	datatype flag = HALT | INTERRUPT of int | OVERFLOW | RUNNING
	datatype vm = Vm of (ProgramCounter.pc * Register.reg * Stack.stack * Register.reg * Register.reg * Ram.memory * flag)

	fun init (ilist, i) = 
	    let 
		val mem = Ram.initialize (i)
	    in 
		Vm(ProgramCounter.Pc(0, Stack.Stack([]), Register.Reg(0), Register.Reg(0)), Register.Reg(0), Stack.Stack([]), Register.Reg(0), Register.Reg(0), (Ram.load (mem, ilist); mem), RUNNING)
	    end
(*
	fun step (Vm()) =  
*)
	fun flagComprehension (fl: flag) = case fl of HALT => "HALT"
					      | OVERFLOW => "OVERFLOW"
					      | RUNNING => "RUNNING"
					      | INTERRUPT(a) =>  Int.toString (a)

			 (* when we tried to use stdOut it crashed poly *)
	fun dump (Vm(pc, a, s, x, y, ram, fl)) = 
	    let
		val dumpstream = TextIO.openOut "vm_dump"
	    in
		(TextIO.output (dumpstream, (ProgramCounter.dumpPc(pc) ^ "\n" ^ (Register.dumpRegister(a))^ ", " ^(Stack.dumpStack(s))^ ", " ^ (Register.dumpRegister(x))^ ", " ^ (Register.dumpRegister(y))^ "\n" ^ (Ram.dump(ram)) ^ "\n" ^ flagComprehension(fl) ^ "\n")); TextIO.closeOut dumpstream)
	    end

end

(*
	I'm not going to give you much more clues about how to go about writing this. Its not as hard as you think.
	It might get pretty ugly but that is not the end of the world.
*)
