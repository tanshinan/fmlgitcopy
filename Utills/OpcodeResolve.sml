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
		|mnemonic(_) = raise ASSEMBLER "Unknown mnemonic!\n"
	
	fun read("x") = 0
		|read("y") = 1
		|read("s") = 2
		|read("$x") = 3
		|read("$y") = 4
		|read("") = 0
		|read(s) =
			let
				val head = Char.ord(List.hd(String.explode(s))); 
			in
				if (head = 36) orelse (head = 64) then (* 36 = "$" 64 = "@" *)
					5
				else
					6

			end
		
	fun write("x") = 0
		|write("y") = 10
		|write("s") = 20
		|write("$x") = 30
		|write("$y") = 40
		|write("q1") = 60
		|write("q2") = 70
		|write("") = raise ASSEMBLER "WTF!?\n"
		|write(s) =
			let
				val head = Char.ord(List.hd(String.explode(s))); 
			in
				if (head = 36)then (* 36 = "$" 64 = "@" *)
					50
				else 
					80

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
		|numberOfArgs("BRL") = 1
		|numberOfArgs("BRR") = 1
		|numberOfArgs("AND") = 2
		|numberOfArgs("ORR") = 2
		|numberOfArgs("XOR") = 2
		|numberOfArgs("NOT") = 1
		
		|numberOfArgs("JMP") = 1
		|numberOfArgs("JEQ") = 2
		|numberOfArgs("JLE") = 2
		|numberOfArgs("JGR") = 2
		|numberOfArgs("JSR") = 1
		|numberOfArgs("RET") = 0
		
		|numberOfArgs("HLT") = 0
		|numberOfArgs("SEM") = 0
		|numberOfArgs(_) = raise ASSEMBLER "Tried to parse an malformed expression\n"
		
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
		
		|validReadArguments("JMP") = []
		|validReadArguments("JEQ") = [0,1,2,3,4,5,6]
		|validReadArguments("JLE") = [0,1,2,3,4,5,6]
		|validReadArguments("JGR") = [0,1,2,3,4,5,6]
		|validReadArguments("JSR") = [0,1,2,3,4,5,6]
		|validReadArguments("RET") = []
		
		|validReadArguments("HLT") = []
		|validReadArguments("SEM") = []
		|validReadArguments(_) = raise ASSEMBLER "Tried to parse an malformed expression\n"


		fun validWriteArguments("NOP") = []
		|validWriteArguments("MOV") = [0,10,20,30,40,50,60,70]
		
		|validWriteArguments("INC") = [0,10]
		|validWriteArguments("DEC") = [0,10]
		|validWriteArguments("ADD") = [0,10,20,30,40,50,80]
		|validWriteArguments("SUB") = [0,10,20,30,40,50,80]
		|validWriteArguments("MUL") = [0,10,20,30,40,50,80]
		|validWriteArguments("DIV") = [0,10,20,30,40,50,80]
		|validWriteArguments("MOD") = [0,10,20,30,40,50,80]
		
		|validWriteArguments("EQL") = [0,10,20,30,40,50,80]
		|validWriteArguments("LES") = [0,10,20,30,40,50,80]
		|validWriteArguments("GRT") = [0,10,20,30,40,50,80]
		|validWriteArguments("BRL") = []
		|validWriteArguments("BRR") = []
		|validWriteArguments("AND") = [0,10,20,30,40,50,80]
		|validWriteArguments("ORR") = [0,10,20,30,40,50,80]
		|validWriteArguments("XOR") = [0,10,20,30,40,50,80]
		|validWriteArguments("NOT") = []
		
		|validWriteArguments("JMP") = [0,10,20,30,40,50,80]
		|validWriteArguments("JEQ") = [0,10,20,30,40,50,80]
		|validWriteArguments("JLE") = [0,10,20,30,40,50,80]
		|validWriteArguments("JGR") = [0,10,20,30,40,50,80]
		|validWriteArguments("JSR") = [0,10,20,30,40,50,80]
		|validWriteArguments("RET") = []
		
		|validWriteArguments("HLT") = []
		|validWriteArguments("SEM") = []
		|validWriteArguments(_) = raise ASSEMBLER "Tried to parse an malformed expression\n"
		
		local
			fun find([],a) = false
			|find(x::xs,a) = (a = x) orelse find(xs,a)
		in
			fun isValidRead(m,a) = find(validReadArguments(m),a)
			fun isValidWrite(m,a) = find(validWriteArguments(m),a)
		end
		
		 
	
end
(*
val lame_test = Resolve.resolveExpression(StringUtills.spaceSplit("NOP"));
val lame_test = Resolve.resolveExpression(StringUtills.spaceSplit("MOV x [y]"));
val lame_test = Resolve.resolveExpression(StringUtills.spaceSplit("INC x"));
val lame_test = Resolve.resolveExpression(StringUtills.spaceSplit("JEQ s @bob"));
hmmmmmmmm .....*)
