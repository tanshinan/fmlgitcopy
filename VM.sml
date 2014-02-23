use "Components.sml";


(*
	create the structures in the Componenets file before you start working on this.
*)
signature VIRTUAL_MACHINE =
sig
	exception RUNTIME
	datatype flag = HALT | INTERRUPT of int | OVERFLOW | RUNNING
	datatype vm = Vm of (ProgramCounter.pc * Register.reg * Stack.stack * Register.reg * Register.Reg * Ram.memory * flag)
	
	val init : (int list * int) -> vm	(*Creates an initialized VM. Loads int list into the memory and makes the memory have a size of the second int*)
	val step : vm -> vm								(*Takes one vm and returns the next vm. I.e it runns the virtual machine for one step.*)
	val dump : vm -> unit							(*Prints the VM to stdOut. The output should be really pretty to make debugging easy.*)
	
end

(*
	I'm not going to give you much more clues about how to go about writing this. Its not as hard as you think.
	It might get pretty ugly but that is not the end of the world.
*)