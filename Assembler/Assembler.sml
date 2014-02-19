
use "OpcodeResolve.sml";

(*datatype intermediate = I of (((string*int) list) * ((string) list) * (( (string*int)*token) list))*)

structure Assembler = 
struct

	datatype pointer = Label of (string * (int option)) | Value of (string * (int option)) | Null;
	datatype token = Ic of int | Ref of string | Arg of int;
	
	exception SYNTAX of string
	exception ASSEMBLER of string
	
	val comment_flag = 37 (* Char.ord(#"%")*)
	val label_flag = 35 (*Char.ord(#"#") gets flaged with a syntax error :/*)
	val value_flag = 64 (*Char.ord(#"@")*)
	val address_flag = 36 (*Char.ord(#"$")*) 
	val base_adress = 0
	
	
		structure Inter =
		struct
			(*I(label_list,value_list,token_list,curretn_label,address)*)
			datatype inter = I of ((pointer list) * (pointer list) * ((string*int*token) list) * pointer * int)
			
			val initial = I([],[],[],Null,0)
			
			fun getLabelList(I(label_list,value_list,token_list,current_label,address)) = label_list
			fun getValueList(I(label_list,value_list,token_list,current_label,address)) = value_list
			fun getTokenList(I(label_list,value_list,token_list,current_label,address)) = token_list
			fun getCurrentLabel(I(label_list,value_list,token_list,current_label,address)) = current_label
			fun getAdress(I(label_list,value_list,token_list,current_label,address)) = address
			
			fun setLabelList(I(label_list,value_list,token_list,current_label,address), a) = I(a,value_list,token_list,current_label,address)
			fun setValueList(I(label_list,value_list,token_list,current_label,address), a) = I(label_list,a,token_list,current_label,address)
			fun setTokenList(I(label_list,value_list,token_list,current_label,address), a) = I(label_list,value_list,a,current_label,address)
			fun setCurrentLabel(I(label_list,value_list,token_list,current_label,address), a) = I(label_list,value_list,token_list,a,address)
			
			fun IncrementAdress(I(label_list,value_list,token_list,current_label,address), a) = I(label_list,value_list,token_list,current_label,address+a)
			
			fun addLabel(I(label_list,value_list,token_list,current_label,address), a) =I(a::label_list,value_list,token_list,current_label,address)
			fun addValue(I(label_list,value_list,token_list,current_label,address), a) =I(label_list,a::value_list,token_list,current_label,address)
			
			fun addToken(I(label_list,value_list,token_list,Null,address), a) = raise SYNTAX "Cant use Null pointer\n"
			|addToken(I(label_list,value_list,[],Label(current_pointer_name,i),address), a) = (*When list is empty*)
				I(label_list,value_list, [(current_pointer_name,0,a)], Label(current_pointer_name,i),address+1)
				
			|addToken(I(label_list,value_list,(pointer_name,n,t) :: rest,Label(current_pointer_name,i),address), a) =
				if current_pointer_name <> pointer_name then
					I(label_list,value_list, (current_pointer_name,0,a) :: ((pointer_name,n,t) :: rest), Label(current_pointer_name,i),address+1) (*if we change pointer*)
				else
					I(label_list,value_list, ((pointer_name,n+1,a) :: ((pointer_name,n,t) :: rest)), Label(current_pointer_name,i),address+1)
			
			fun dumpTokenList(i) = 
				let
					val token_list = List.rev(getTokenList(i))
					fun printPretty ([]) = ()
					|printPretty((p,n,Ic(i))::xs) = (print (p ^"+"^ Int.toString(n)^":\t"^ Int.toString(i)  ^"\n"); printPretty(xs))
					|printPretty((p,n,Ref(s))::xs) = (print (p ^"+"^ Int.toString(n)^":\t"^ s  ^"\n"); printPretty(xs))
					|printPretty((p,n,Arg(i))::xs) = (print (p ^"+"^ Int.toString(n)^":\t"^ Int.toString(i)  ^"\n"); printPretty(xs)) 
				in
					printPretty(token_list)
				end
			
		end
	
	val initial = Inter.initial
	
	fun dumpTokenList(i) = Inter.dumpTokenList(i)
	
	fun error(line_number,message,cause) = "\nSYNTAX ERROR!\n" ^ message ^ " at line: " ^ Int.toString(line_number) ^ "\n"
				^ "Caused by: "  ^ cause ^"\n"
	



	(*
	Scans one line and adds it to an intermediate structure
	
	Arguments:
		line = Line to be scanned
		l = Current line number
		i = Current intermediate structure
	*)
	fun scanLine(line, i, l)  =
		let
			val line = StringUtills.trim(line)
			val line_head = Char.ord(List.hd(String.explode(line)))
			val line_tail = String.implode(List.tl(String.explode(line)))
			
				fun resolveToken("x") = NONE
				|resolveToken("y") = NONE
				|resolveToken("s") = NONE
				|resolveToken("$x") = NONE
				|resolveToken("$y") = NONE
				|resolveToken("q1") = NONE
				|resolveToken("q2") = NONE
				|resolveToken(a) =
					let
						val char_list = String.explode(a)
						val assert_length = 
							if Char.ord(List.hd(char_list)) = address_flag then
								(List.length(char_list) >= 2) orelse (print (error(l,"Mallformed argument",line));raise SYNTAX "")
							else
								(List.length(char_list) >= 1) orelse (print (error(l,"Mallformed argument",line));raise SYNTAX "")
					in
						if (List.all (fn x => Char.isDigit(x)) char_list) then
							SOME(Arg(Option.valOf(Int.fromString(a))))
						else 
							if Char.ord(List.hd(char_list)) = address_flag then
								SOME(Ref(String.implode(List.tl(char_list))))
							else
								SOME(Ref(String.implode(char_list)))
					end

		in
		(
			case line_head of
				37 => i	(*This is a comment*)
				|35 => (*This is a label*)(
					let
						val assert_length = (StringUtills.words(line_tail) = 1) orelse (print (error(l,"Mallformed label assignemnt",line));raise SYNTAX "")
					in
						Inter.setCurrentLabel(Inter.addLabel(i,Label(line_tail,NONE)),Label(line_tail,NONE))
					end
					
				)
				|64 => (*this is a value*)(
					let
						val assert_length = (StringUtills.words(line_tail) = 1) orelse (print (error(l,"Mallformed value assignment",line));raise SYNTAX "")
					in
						Inter.addValue(i,Value(line_tail,NONE))
					end
				)
				|_ =>  (*this is magic*)(
					let
						(*<operation> <write> <read>*)
						val expression = StringUtills.spaceSplit(line)
						val number_of_args = Resolve.numberOfArgs(List.hd(expression))
						val assert_length = (number_of_args = (List.length(expression)-1)) orelse (print (error(l,"To many arguments",line));raise SYNTAX "")

					in
						case expression of
						[m] => Inter.addToken(i,Ic(Resolve.mnemonic(m)))
						|[m,w] => 
											let
												val assert_argument = Resolve.isValidWrite(m,Resolve.write(w)) orelse  (print (error(l,"Forbidden argument",line));raise SYNTAX "")
												val assert_no_s = (w <> "$s") orelse  (print (error(l,"Forbidden argument",line));raise SYNTAX "")
												val instruction = Inter.addToken(i,Ic(Resolve.mnemonic(m) + Resolve.write(w)))
												
											in
												if resolveToken(w) = NONE then
													instruction
												else
													Inter.addToken(instruction,Option.valOf(resolveToken(w)))
											end
						
						|[m,r,w] => (*How do we handle the moving from A to A........*)
											let
												val assert_argument_write = Resolve.isValidWrite(m,Resolve.write(w)) orelse  (print (error(l,"Write argument is forbidden",line));raise SYNTAX "")
												val assert_argument_read = Resolve.isValidRead(m,Resolve.read(r)) orelse  (print (error(l,"Read argument is forbidden",line));raise SYNTAX "")
												val assert_no_s = ((w <> "$s") andalso (w <> "$s")) orelse  (print (error(l,"Forbidden argument",line));raise SYNTAX "")
												val instruction = Inter.addToken(i,Ic(Resolve.mnemonic(m) + Resolve.write(w)+Resolve.read(r)))
												
											in
													case (resolveToken(w),resolveToken(r)) of
													(SOME(a),NONE) => Inter.addToken(instruction,Option.valOf(resolveToken(w)))
													|(NONE,SOME(a)) => Inter.addToken(instruction,Option.valOf(resolveToken(r)))
													|(NONE,NONE) => instruction
													|(_,_) => (print (error(l,"Forbidden argument",line));raise SYNTAX "")
													
											end
						|_ => raise ASSEMBLER "OMG SERIOUSLY!?!?!?!?!"
					end
				)
		)
		end
	
	fun scanList([],i,n) = i
	|scanList(x::xs,i,n) =scanList(xs,scanLine(x,i,n),n+1) 

end
(*
val lol_test = Assembler.scanLine("%utter fitta och sådant trevligt", Assembler.initial,1);
val lol_test = Assembler.scanLine("#lolTrol", lol_test,2);
val lol_test = Assembler.scanLine("@roflol", lol_test,3);
val lol_test = Assembler.scanLine("NOP", lol_test,4);
val lol_test = Assembler.scanLine("INC x", lol_test,5);
val lol_test = Assembler.scanLine("JMP lolTroll", lol_test,5);
val lol_test = Assembler.scanLine("ADD x y", lol_test,6);
val lol_test = Assembler.scanLine("JEQ x 123", lol_test,6);*)
val asm_code = [
"#start",
"MOV 10 x",
"MOV 189 y",
"@result",
"ADD x y",
"MOV s $result",
"#loop",
"MOV $result x",
"INC x",
"JEQ 1000 s",
"JMP loop",
"MOV $result s",
"HLT"
];
Assembler.dumpTokenList(Assembler.scanList(asm_code,Assembler.initial,1));