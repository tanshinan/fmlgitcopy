(* Yet to be implemented:
 * Logical operations AND, ORR, XOR, NOT
 * Adresses for memory used as read/write
 * IRQs as read/write *)

val current_dir = OS.FileSys.getDir();
OS.FileSys.chDir("Utills");
use "StringUtills.sml";
use "IO.sml";
OS.FileSys.chDir(current_dir);
use "Components.sml";

(*This signature is one of the core parts of the program*)
signature VIRTUAL_MACHINE =
sig
	exception RUNTIME
	(*
	DATATYPE CONVENTIONS: flag contains tree different flags and one special that we have an integer notation.
	The flags are HALT, INTERRUPT and the int (the special flag), Overflow, Running
	DATATYPE INVARIANTS: 
	*)
	datatype flag = HALT | INTERRUPT of int | OVERFLOW | RUNNING
	datatype vm = Vm of (ProgramCounter.pc * Register.reg * Stack.stack * Register.reg * Register.reg * Ram.memory * flag)

	val init : (int list * int) -> vm	(*Creates an initialized VM. Loads int list into the memory and makes the memory have a size of the second int*)
	val step : vm -> vm					(*Takes one vm and returns the next vm. I.e it runns the virtual machine for one step.*)
	val dump : vm -> unit				(*Prints the VM to stdOut. The output should be really pretty to make debugging easy.*)
	val loop : vm -> unit
	val dumpRam : vm -> unit

end
(*This is the functions of the signature Virtual_Machine*)
structure Vm :> VIRTUAL_MACHINE =
struct
	exception RUNTIME
	(*
	DATATYPE CONVENTIONS: The datatype of flag is the same as in the previous case.'
		the datatype vm(ProgramCounter.pc, Register.reg, Stack.stack, Register.reg ,Register.reg ,Ram.memory flag) contains program counter as the PC , 
		Virtual Register that is for future use, a stack,the first general purpose register known as "x", the second general purpose register known as "Y", and a ram known as Memory and a flag.
		All these types are listed in the order of use, and the stack use a FIFO structure.
	DATATYPE INVARIANTS: If a incorrect use of this will break the wm- to one of the listed cases of flag
	*)
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
	(* getCode i
		TYPE: int -> int list
		PRE: true
		POST: A list with every number in i as an element
		EXAMPLE: getCode 35 = [3,5]
	*)
	fun getCode (i) =
		let
			val charL = explode (Int.toString(i))
			fun charLtoIntL ([]) = []
				| charLtoIntL (x::xs) = (Char.ord(x)-(48))::charLtoIntL(xs)
		in
			charLtoIntL (charL)
	end;

	(* step vm
		TYPE: vm -> vm
		PRE: true
		POST: VM ran one cycle
	*)
	fun step (virt as Vm(p as ProgramCounter.Pc(i, stack1 as Stack.Stack(s1), irq1 as Register.Reg(q1), irq2
		as Register.Reg(q2)), ro as Register.Reg(a),stack2 as Stack.Stack(s2),rx as
		Register.Reg(x),ry as Register.Reg(y), ram: Ram.memory, fl: flag)) =
		let
			val stepCode = getCode(Ram.read (ram,i))
			val opSize = length(stepCode)
			val revd as (r::rs) = rev (stepCode)
			(* PRE: call must be with list length 5 *)
			fun check5 (l::ls) = if l > 0 andalso l < 7 then
					(if l = 1 then check51 (ls, 4) else
					if l = 2 then check52 (ls, 4) else
					if l = 3 then check52 (ls, 4) else
					if l = 4 then check52 (ls, 4) else
					if l = 5 then check51 (ls, 4) else
					check53 (ls)) else raise RUNTIME
			and check51 ([],_) = raise RUNTIME
				| check51 (l, 1) = hd(l) >= 0 andalso hd(l) < 7
				| check51 (l::ls, i1) = if l = 0 then true andalso check51
				(ls,i1-1) else false
			and check52 ([],_) = raise RUNTIME
				| check52 (l::ls, 2) = l >= 0 andalso l < 9 andalso hd(ls) >=
				0 andalso hd(ls) < 7 andalso not (l = hd(ls))
				| check52 (l::ls, i1) = if l = 0 then true andalso check52
				(ls,i1-1) else false
			and check53 (l) = List.all (fn x => x=0) l
			fun check4 (l::ls) = if l > 0 andalso l < 10 then
					(if l = 1 then check52 (ls, 3) else
					if l = 2 then check52 (ls, 3) else
					if l = 3 then check52 (ls, 3) else
					if l = 4 then check51 (ls, 3) else
					if l = 5 then check51 (ls, 3) else
					if l = 6 then check52 (ls, 3) else
					if l = 7 then check52 (ls, 3) else
					if l = 8 then check52 (ls, 3) else
					check51 (ls, 3)) else raise RUNTIME
			fun isarg (l::ls) = if l = 6 orelse hd(ls) = 8 then 2 else 1
			fun resolver (a) = case a of 0 => x
									| 1 => y
									| 2 => Stack.top(stack2)
							(* memory adress stuff to be implemented later *)
									| 6 => Ram.read(ram, i+1)
									| _ => raise RUNTIME
			fun resolvew (a) = case a of 0 => x
									| 1 => y
									| 2 => Stack.top(stack2)
							(* more later *)
									| 8 =>	Ram.read(ram, i+1)
									| _ => raise RUNTIME
			fun step' (w::ws) = case opSize of 1 => if r > 6 orelse r < 0 then raise RUNTIME else
						(case w of
						0 => (Vm(ProgramCounter.incrementPointer(p, 1), ro, stack2, rx, ry, ram, fl))
						| _ => (Vm(ProgramCounter.incrementPointer(p, isarg(revd)), ro, (if r = 2 then Stack.pop(stack2) else stack2), Register.Reg(resolver(r)), ry, ram, fl)))
					| 2 => if ((r > 6 orelse r < 0) orelse (hd(rs) > 7 orelse hd(rs) < 0)) then raise RUNTIME else
						(case w of 1 =>
								(Vm(ProgramCounter.incrementPointer(p, isarg(revd)), ro, (if r = 2 then Stack.pop(stack2) else stack2), rx, Register.Reg(resolver(r)), ram, fl))
						| 2 =>
								(Vm(ProgramCounter.incrementPointer(p, isarg(revd)), ro, Stack.push((if r=2 then raise RUNTIME else stack2), resolver(r)), rx, ry, ram, fl))
						(* fix more later *)
						| _ => raise RUNTIME )
					| 3 => if ((r > 6 orelse r < 0) orelse (hd(rs) > 8 orelse hd(rs) < 0)) then raise RUNTIME else
						(case w of 1 => if hd(rs) = 0 then
								(Vm(ProgramCounter.incrementPointer(p, 1), ro, stack2, Register.increment(rx), ry, ram, fl))
								else if hd(rs) = 1 then
								(Vm(ProgramCounter.incrementPointer(p, 1), ro, stack2, rx, Register.increment(ry), ram, fl))
								else raise RUNTIME
						| 2 => if hd(rs) = 0 then
								(Vm(ProgramCounter.incrementPointer(p, 1), ro, stack2, Register.decrement(rx), ry, ram, fl))
								else if hd(rs) = 1 then
								(Vm(ProgramCounter.incrementPointer(p, 1), ro, stack2, rx, Register.decrement(ry), ram, fl))
								else raise RUNTIME
						| 3 =>
								(Vm(ProgramCounter.incrementPointer(p, isarg(revd)), ro, Stack.push((if r=2 andalso hd(rs) = 2 then raise RUNTIME else if r = 2 orelse hd(rs) = 2 then Stack.pop(stack2) else stack2), (resolver(r) + resolvew(hd(rs)))), rx, ry, ram, fl))
						| 4 =>
								(Vm(ProgramCounter.incrementPointer(p, isarg(revd)), ro, Stack.push((if r=2 andalso hd(rs) = 2 then raise RUNTIME else if r = 2 orelse hd(rs) = 2 then Stack.pop(stack2) else stack2), (resolver(r) - resolvew(hd(rs)))), rx, ry, ram, fl))
						| 5 =>
								(Vm(ProgramCounter.incrementPointer(p, isarg(revd)), ro, Stack.push((if r=2 andalso hd(rs) = 2 then raise RUNTIME else if r = 2 orelse hd(rs) = 2 then Stack.pop(stack2) else stack2), (resolver(r) * resolvew(hd(rs)))), rx, ry, ram, fl))
						| 6 =>
								(Vm(ProgramCounter.incrementPointer(p, isarg(revd)), ro, Stack.push((if r=2 andalso hd(rs) = 2 then raise RUNTIME else if r = 2 orelse hd(rs) = 2 then Stack.pop(stack2) else stack2), (resolver(r) div resolvew(hd(rs)))), rx, ry, ram, fl))
						| 7 =>
								(Vm(ProgramCounter.incrementPointer(p, isarg(revd)), ro, Stack.push((if r=2 andalso hd(rs) = 2 then raise RUNTIME else if r = 2 orelse hd(rs) = 2 then Stack.pop(stack2) else stack2), (resolver(r) mod resolvew(hd(rs)))), rx, ry, ram, fl))
						| _ => raise RUNTIME )
					| 4 => if not (check4 (w::ws)) then raise RUNTIME else
						(case w of 1 => if resolver(r) = resolvew(hd(rs)) then
								(Vm(ProgramCounter.incrementPointer(p, isarg(revd)), ro, Stack.push((if r = 2 orelse hd(rs) = 2 then Stack.pop(stack2) else stack2), 1), rx, ry, ram, fl))
								else
								(Vm(ProgramCounter.incrementPointer(p, isarg(revd)), ro, Stack.push((if r = 2 orelse hd(rs) = 2 then Stack.pop(stack2) else stack2), 0), rx, ry, ram, fl))
						| 2 => if resolver (r) < resolvew(hd(rs)) then
								(Vm(ProgramCounter.incrementPointer(p, isarg(revd)), ro, Stack.push((if r = 2 orelse hd(rs) = 2 then Stack.pop(stack2) else stack2), 1), rx, ry, ram, fl))
								else
								(Vm(ProgramCounter.incrementPointer(p, isarg(revd)), ro, Stack.push((if r = 2 orelse hd(rs) = 2 then Stack.pop(stack2) else stack2), 0), rx, ry, ram, fl))
						| 3 => if resolver (r) > resolvew(hd(rs)) then
								(Vm(ProgramCounter.incrementPointer(p, isarg(revd)), ro, Stack.push((if r = 2 orelse hd(rs) = 2 then Stack.pop(stack2) else stack2), 1), rx, ry, ram, fl))
								else
								(Vm(ProgramCounter.incrementPointer(p, isarg(revd)), ro, Stack.push((if r = 2 orelse hd(rs) = 2 then Stack.pop(stack2) else stack2), 0), rx, ry, ram, fl))
						| 4 =>
								(Vm(ProgramCounter.incrementPointer(p, isarg(revd)), ro, Stack.push((if r=2 then Stack.pop(stack2) else stack2), (resolver(r) * 10)), rx, ry, ram, fl))
						| 5 =>
								(Vm(ProgramCounter.incrementPointer(p, isarg(revd)), ro, Stack.push((if r=2 then Stack.pop(stack2) else stack2), (resolver(r) div 10)), rx, ry, ram, fl))
						(* fix more later *)
						| _ => raise RUNTIME )
					| 5 => if not (check5 (w::ws)) then raise RUNTIME else
						(case w of 1 =>
							(Vm(ProgramCounter.jump(p, resolver(r)), ro, stack2, rx, ry, ram, fl))
						| 2 =>	if resolver(r) = resolvew(hd(rs)) then
								(Vm(ProgramCounter.incrementPointer(p, isarg(revd) + 1), ro,(if r=2 orelse hd(rs)=2 then Stack.pop(stack2) else stack2), rx, ry, ram, fl))
								else
							(Vm(ProgramCounter.incrementPointer(p, isarg(revd)), ro, (if r=2 orelse hd(rs)=2 then Stack.pop(stack2) else stack2), rx, ry, ram, fl))
						| 3 =>	if resolver(r) < resolvew(hd(rs)) then
								(Vm(ProgramCounter.incrementPointer(p, isarg(revd) + 1), ro, (if r=2 orelse hd(rs)=2 then Stack.pop(stack2) else stack2), rx, ry, ram, fl))
								else
							(Vm(ProgramCounter.incrementPointer(p, isarg(revd)), ro, (if r=2 orelse hd(rs)=2 then Stack.pop(stack2) else stack2), rx, ry, ram, fl))
						| 4 =>	if resolver(r) > resolvew(hd(rs)) then
								(Vm(ProgramCounter.incrementPointer(p, isarg(revd) + 1), ro, (if r=2 orelse hd(rs)=2 then Stack.pop(stack2) else stack2), rx, ry, ram, fl))
								else
							(Vm(ProgramCounter.incrementPointer(p, isarg(revd)),ro, (if r=2 orelse hd(rs)=2 then Stack.pop(stack2) else stack2), rx, ry, ram, fl))
						| 5 =>	(Vm(ProgramCounter.subroutineJump(p, resolver(r), isarg(revd)), ro, stack2, rx, ry, ram, fl))
						| 6 =>	(Vm(ProgramCounter.return(p), ro, stack2, rx, ry, ram, fl))
						| _ => raise RUNTIME )
					| 6 => if (List.all (fn x => x=0) ws)
										then
						(case w of 1 => (Vm(p, ro, stack2, rx, ry, ram, HALT))
						| 2 => if Stack.isEmpty (stack2) then
					(Vm(ProgramCounter.incrementPointer(p, 1), ro, Stack.push(stack2,1), rx, ry, ram, fl)) else
					(Vm(ProgramCounter.incrementPointer(p, 1), ro, Stack.push(stack2,0), rx, ry, ram, fl))
						| 3 => (Vm(ProgramCounter.incrementPointer(p,1), ro, Stack.pop(stack2), rx, ry, ram, fl))
						| _ => raise RUNTIME)
							else raise RUNTIME
					| _ => raise RUNTIME
			in
		if fl = RUNNING then step' (stepCode) else virt
	end



	(*flagToString flag
			TYPE: flag -> string
			PRE: true
			POST: string corresponding to flag
			EXAMPLE: flagToString (HALT) = "HALT"
			flagToString (INTERRUPT(4)) = "4"
	*)
	fun flagToString (fl: flag) =
		case fl of HALT => "HALT"
		| OVERFLOW => "OVERFLOW"
		| RUNNING => "RUNNING"
		| INTERRUPT(a) =>  Int.toString (a)

	(* dumpToFile vm
		TYPE: vm -> unit
		PRE: true
		POST: file with the state of vm written in easily readable text
	*)
	fun dumpToFile (Vm(pc, a, s, x, y, ram, fl)) =
		let
			val dumpstream = TextIO.openOut "vm_dump"
		in
			(TextIO.output (dumpstream, (ProgramCounter.dumpPc(pc) ^ "\n" ^
			(Register.dumpRegister(a))^ ", " ^(Stack.dumpStack(s))^ ", " ^
			(Register.dumpRegister(x))^ ", " ^ (Register.dumpRegister(y))^ "\n" ^
			(Ram.dump(ram)) ^ "\n" ^ flagToString(fl) ^ "\n")); TextIO.closeOut dumpstream)
		end
	(* dump vm
		TYPE: vm -> unit
		PRE: true
		POST: state of vm printed in prompt in easily readable text
	*)
	fun dump (Vm(pc, a, s, x, y, ram, fl)) = print  (
	(ProgramCounter.dumpPc(pc) ^ "\n" ^
	(*"A: "^Register.dumpRegister(a))*) "Stack: " ^(Stack.dumpStack(s))^ "\nX: " ^
	(Register.dumpRegister(x))^ " Y: " ^ (Register.dumpRegister(y))^ "\n"
	^ flagToString(fl) ^ "\n"))

		(* dumpRam vm
		TYPE: vm -> unit
		PRE: true
		POST: Dumps the ram to stdOut.
	*)
	fun dumpRam(Vm(pc, a, s, x, y, ram, fl)) = print((Ram.dump(ram)) ^ "\n");

		(*getFlag vm
		TYPE: vm -> flag
		PRE:None
		POST: returns the flag of the vm
	*)
	fun getFlag(Vm(_,_,_,_,_,_, flag)) = flag

	fun loop(vm as Vm(pc, a, s, x, y, ram, fl)) =
	let
		val dump = (print("\n");dump(vm))
		val new_vm = step(vm)
	in
		if fl = RUNNING then
			loop(new_vm)
		else
			dumpRam(new_vm)
	end
end

val init_vm = Vm.init(IO_Handler.fileToIntList("out.fml"),50);

Vm.loop(init_vm);

