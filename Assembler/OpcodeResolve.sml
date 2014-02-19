use "StringUtills.sml";
structure Resolve =
struct
	exception SYNTAX of string
	exception ASSEMBLER of string

	fun mnemonic("NOP") = 000000
		|mnemonic("MOV") = 000000
		
		|mnemonic("INC") = 000100
		|mnemonic("DEC") = 000200
		|mnemonic("ADD") = 000300
		|mnemonic("SUB") = 000400
		|mnemonic("MUL") = 000500
		|mnemonic("DIV") = 000600
		|mnemonic("MOD") = 000700
		
		|mnemonic("EQL") = 001000
		|mnemonic("LES") = 002000
		|mnemonic("GRT") = 003000
		|mnemonic("BRL") = 004000
		|mnemonic("BRR") = 005000
		|mnemonic("AND") = 006000
		|mnemonic("ORR") = 007000
		|mnemonic("XOR") = 008000
		|mnemonic("NOT") = 009000
		
		|mnemonic("JMP") = 010000
		|mnemonic("JEQ") = 020000
		|mnemonic("JLE") = 030000
		|mnemonic("JGR") = 040000
		|mnemonic("JSR") = 050000
		|mnemonic("RET") = 060000
		
		|mnemonic("HLT") = 100000
		|mnemonic("SEM") = 200000
		|mnemonic(_) = raise SYNTAX "Unknown mnemonic!\n"
	
	fun read("x") = 0
		|read("y") = 1
		|read("s") = 2
		|read("[x]") = 3
		|read("[y]") = 4
		|read("") = 0
		|read(s) =
			let
				val head = Char.ord(List.hd(String.explode(s))); 
			in
				if (head = 91) orelse (head = 64) then (* 91 = "[" 64 = "@" *)
					5
				else
					if Char.isDigit(Char.chr(head)) then
						6
					else
						raise SYNTAX "Mallformed read argument\n"
			end
		
	fun write("x") = 0
		|write("y") = 10
		|write("s") = 20
		|write("[x]") = 30
		|write("[y]") = 40
		|write("q1") = 60
		|write("q2") = 70
		|write("") = 0
		|write(s) =
			let
				val head = Char.ord(List.hd(String.explode(s))); 
			in
				if (head = 91) orelse (head = 64) then (* 91 = "[" 64 = "@" *)
					50
				else 
					if Char.isDigit(Char.chr(head)) then
						80
					else
						raise SYNTAX "Mallformed write argument\n"
			end
	
	fun resolveExpression([m]) = mnemonic(m)
	|resolveExpression([m,a1]) = mnemonic(m)+read(a1)
	|resolveExpression([m,a1,a2]) = mnemonic(m)+read(a1)+write(a2)
	|resolveExpression(_) = raise ASSEMBLER "Tried to parse an malformed expression\n"
	
	fun numberOfArgs("NOP") = 0
		|numberOfArgs("MOV") = 2
		
		|numberOfArgs("INC") = 1
		|numberOfArgs("DEC") = 1
		|numberOfArgs("ADD") = 2
		|numberOfArgs("SUB") = 2
		|numberOfArgs("MUL") = 2
		|numberOfArgs("DIV") = 2
		|numberOfArgs("MOD") = 2
		
		|numberOfArgs("EQL") = 2
		|numberOfArgs("LES") = 2
		|numberOfArgs("GRT") = 2
		|numberOfArgs("BRL") = 2
		|numberOfArgs("BRR") = 2
		|numberOfArgs("AND") = 2
		|numberOfArgs("ORR") = 2
		|numberOfArgs("XOR") = 2
		|numberOfArgs("NOT") = 2
		
		|numberOfArgs("JMP") = 1
		|numberOfArgs("JEQ") = 2
		|numberOfArgs("JLE") = 2
		|numberOfArgs("JGR") = 2
		|numberOfArgs("JSR") = 1
		|numberOfArgs("RET") = 0
		
		|numberOfArgs("HLT") = 0
		|numberOfArgs("SEM") = 0
		
fun validReadArguments("NOP") = []
		|validReadArguments("MOV") = [0,1,2,3,4,5,6]
		
		|validReadArguments("INC") = []
		|validReadArguments("DEC") = []
		|validReadArguments("ADD") = [0,1,2,3,4,5,6]
		|validReadArguments("SUB") = [0,1,2,3,4,5,6]
		|validReadArguments("MUL") = [0,1,2,3,4,5,6]
		|validReadArguments("DIV") = [0,1,2,3,4,5,6]
		|validReadArguments("MOD") = [0,1,2,3,4,5,6]
		
		|validReadArguments("EQL") = [0,1,2,3,4,5,6]
		|validReadArguments("LES") = [0,1,2,3,4,5,6]
		|validReadArguments("GRT") = [0,1,2,3,4,5,6]
		|validReadArguments("BRL") = [0,1,2,3,4,5,6]
		|validReadArguments("BRR") = [0,1,2,3,4,5,6]
		|validReadArguments("AND") = [0,1,2,3,4,5,6]
		|validReadArguments("ORR") = [0,1,2,3,4,5,6]
		|validReadArguments("XOR") = [0,1,2,3,4,5,6]
		|validReadArguments("NOT") = [0,1,2,3,4,5,6]
		
		|validReadArguments("JMP") = [0,1,3,4,5,6]
		|validReadArguments("JEQ") = [0,1,2,3,4,5,6]
		|validReadArguments("JLE") = [0,1,2,3,4,5,6]
		|validReadArguments("JGR") = [0,1,2,3,4,5,6]
		|validReadArguments("JSR") = [0,1,2,3,4,5,6]
		|validReadArguments("RET") = []
		
		|validReadArguments("HLT") = []
		|validReadArguments("SEM") = []
	
end
(*
val lame_test = Resolve.resolveExpression(StringUtills.spaceSplit("NOP"));
val lame_test = Resolve.resolveExpression(StringUtills.spaceSplit("MOV x [y]"));
val lame_test = Resolve.resolveExpression(StringUtills.spaceSplit("INC x"));
val lame_test = Resolve.resolveExpression(StringUtills.spaceSplit("JEQ s @bob"));
*)
