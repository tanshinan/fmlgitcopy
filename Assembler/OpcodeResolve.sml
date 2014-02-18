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
end

val lame_test = Resolve.resolveExpression(StringUtills.spaceSplit("NOP"));
val lame_test = Resolve.resolveExpression(StringUtills.spaceSplit("MOV x [y]"));
val lame_test = Resolve.resolveExpression(StringUtills.spaceSplit("INC x"));
val lame_test = Resolve.resolveExpression(StringUtills.spaceSplit("JEQ s @bob"));

