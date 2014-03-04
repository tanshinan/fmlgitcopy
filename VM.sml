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

	val init : (int list * int) -> vm	(*Creates an initialized VM. Loads int list into the memory and makes the memory have a size of the second int*)
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
		EXAMPLE: here be dragons (meaning, I'll fill it in later)
	*)

	fun step (Vm(p as Pc(i, stack1 as Stack.Stack(s1), irq1 as Reg.Reg(q1), irq2
		as Reg.Reg(q2)), ro as Reg.Reg(a),stack2 as Stack.Stack(s2),rx as
		Register.Reg(x),ry as Register.Reg(y), ram:
		Ram.memory, fl: flag)) =
		let
			val stepCode = GetCode(Ram.read (ram,i))
			val opSize = length(stepcode)
			val revd as (r::rs) = rev (stepCode)
			(* PRE: call must be with list length 5 *)
			fun check5 (l::ls) = if l > 0 andalso l < 7 then
					(if l = 1 then check51 (ls, 4) else
					if l = 2 then check52 (ls, 4) else
					if l = 3 then check52 (ls, 4) else
					if l = 4 then check52 (ls, 4) else
					if l = 5 then check51 (ls) else
					check53 (ls, 4)) else raise RUNTIME
			and check51 ([],_) = raise RUNTIME
				| check51 (l, 1) = hd(l) >= 0 andalso hd(l) < 7
				| check51 (l::ls, i1) = if l = 0 then true andalso check51
				(ls,i1-1) else false
			and check52 ([],_) = raise RUNTIME
				| check52 (l::ls, 2) = l >= 0 andalso l < 9 andalso hd(ls) >=
				0 andalso hd(ls) < 7
				| check52 (l::ls, i1) = if l = 0 then true andalso check52
				(ls,i1-1) else false
			and check53 (l) = List.all (fn x => x=0)
			fun check4 (l::ls) = if l > 0 andalso l < 10 then
					(if l = 1 then check52 (ls, 3) else
					if l = 2 then check52 (ls, 3) else
					if l = 3 then check52 (ls, 3) else
					if l = 4 then check51 (ls, 3) else
					if l = 5 then check51 (ls, 3) else
					if l = 6 then check52 (ls, 3) else
					if l = 7 then check52 (ls, 3) else
					if l = 8 then check52 (ls, 3) else
					check51 (ls, 3)
			fun isarg (l::ls) = if l = 6 andalso hd(ls) = 8 then 3 else if l = 6
				orelse hd(ls) = 8 then 2 else 1
			fun resolver (a) = case a of 0 => rx
									| 1 => ry
									| 2 => Stack.top(stack2)
							(* fix more later *)
									| 6 => Ram.read(ram, i+1)
									| _ => raise RUNTIME
			fun resolvew (a) = case a of 0 => rx
									| 1 => ry
									| 2 => Stack.top(stack2)
							(* more later *)
									| 6 => if r = 6 then Ram.read(ram, i+2) else
										Ram.read(ram, i+1)
									| _ => raise RUNTIME
			fun step' (w::ws) = case opSize of 6 => if List.all (fn x => x=0)
										then
				(case w of 1 => (Vm(p, ro, stack2, rx, ry, HALT))
						| 2 => if Stack.isEmpty (stack2) then
					(Vm(ProgramCounter.incrementPointer(p,1), ro,
					Stack.push(stack2,1), rx, ry, ram, fl)) else
					(Vm(ProgramCounter.incrementPointer(p, 1), ro,
					Stack.push(stack2,0), rx, ry, ram, fl))
						| 3 => (Vm(ProgramCounter.incrementPointer(p,1), ro,
				Stack.pop(stack2), rx, ry, ram, fl))
						| _ => raise RUNTIME)
							else raise RUNTIME
					| 5 => if not check5 (w::ws) then raise RUNTIME else
						(case w of 1 => if not r = 6 then
						(Vm(ProgramCounter.jump(p, resolver(r)), ro, stack2, rx,
						ry, ram, fl)) else ((Vm(ProgramCounter.jump(p,
						resolver(r)), ro, Stack.pop(stack2), rx, ry, ram, fl))
						| 2 =>
							if r = hd(rs) then
								(Vm(ProgramCounter.incrementPointer(p,
								isarg(revd) + 1), ro, stack2, rx, ry, ram, fl))
								else
							(Vm(ProgramCounter.incrementPointer(p, isarg(revd)),
							ro, stack2, rx, ry, ram, fl))
						| 3 =>
							if r < hd(rs) then
								(Vm(ProgramCounter.incrementPointer(p,
								isarg(revd) + 1), ro, stack2, rx, ry, ram, fl))
								else
							(Vm(ProgramCounter.incrementPointer(p, isarg(revd)),
							ro, stack2, rx, ry, ram, fl))
						| 4 =>
							if r > hd(rs) then
								(Vm(ProgramCounter.incrementPointer(p,
								isarg(revd) + 1), ro, stack2, rx, ry, ram, fl))
								else
							(Vm(ProgramCounter.incrementPointer(p, isarg(revd)),
							ro, stack2, rx, ry, ram, fl))
						| 5 =>
							(Vm(ProgramCounter.subroutineJump(p, r,
							isarg(revd)), ro, rx, ry, ram, fl))
						| 6 =>
							(Vm(ProgramCounter.return(p), ro, rx, ry, ram, fl))
						| _ => raise RUNTIME )

					| 4 => if not check4 (w::ws) then raise RUNTIME else
						(case w of 1 =>
			in

	end;

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
		EXAMPLE: here be dragons (oh god, how the hell do you write how it creates a file)
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
		EXAMPLE:
	*)
	fun dump (Vm(pc, a, s, x, y, ram, fl)) = print  ((ProgramCounter.dumpPc(pc) ^ "\n" ^
	(Register.dumpRegister(a))^ ", " ^(Stack.dumpStack(s))^ ", " ^
	(Register.dumpRegister(x))^ ", " ^ (Register.dumpRegister(y))^ "\n" ^ (Ram.dump(ram)) ^ "\n"
	^ flagToString(fl) ^ "\n"))

end

(*
	I'm not going to give you much more clues about how to go about writing this. Its not as hard as you think.
	It might get pretty ugly but that is not the end of the world.
*)
