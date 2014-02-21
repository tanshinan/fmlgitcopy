OS.FileSys.chDir("Utills");
use "OpcodeResolve.sml";
OS.FileSys.chDir(".");
(*datatype intermediate = I of (((string*int) list) * ((string) list) * (( (string*int)*token) list))*)

structure Assembler = 
struct

	datatype pointer = Label of (string * (int option)) | Value of (string * (int option)) | NULL;
	datatype token = Ic of int | Ref of string | Arg of int;
	
	exception SYNTAX of string
	exception ASSEMBLER of string
	
	(*
		The SML plugin for eclipse bitches about some #"" things.
		and yes i know its ugly
		*)
	val comment_flag = 37 (* Char.ord(#"%")*)
	val label_flag = 35 (*Char.ord(#"#")*)
	val value_flag = 64 (*Char.ord(#"@")*)
	val address_flag = 36 (*Char.ord(#"$")*) 
	val base_adress = 0
	
	fun getPointerName(Label(name,_)) = name
	|getPointerName(Value(name,_)) = name
	|getPointerName(NULL) = raise ASSEMBLER "Got NULL???\n"
	
	fun getPointerAddress(Label(_,a)) = Option.valOf(a)
	|getPointerAddress(Value(_,a)) = Option.valOf(a)
	|getPointerAddress(NULL) = raise ASSEMBLER "Got NULL???\n"
	
	fun setPointerAddress(Label(name,_),a) = Label(name,SOME(a))
	|setPointerAddress(Value(name,_),a) = Value(name,SOME(a))
	|setPointerAddress(NULL,a) = raise ASSEMBLER "Got NULL???\n"
	
		(*
			Yes i know that this is ugly but life sucks without side-effects.
			
			The Intermediate structure gets built up in the wrong order. The first lines will end upp on the bottom of the lists.
			This is not the worst thing in the world. One just has to take it into account.
			
			The entries in label_list and value_list are in the same order (but reversed) as they appeared in the file.
		*)
		structure Inter =
		struct
			(*I(label_list,value_list,token_list,curretn_label,address)*)
			datatype inter = I of ((pointer list) * (pointer list) * ((string*int*token) list) * pointer * int)
			
			val initial = I([],[],[],NULL,0)
			
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
			
			fun addToken(I(label_list,value_list,token_list,NULL,address), a) = raise SYNTAX "Cant use NULL pointer\n"
			|addToken(I(label_list,value_list,[],Label(current_pointer_name,i),address), a) = (*When list is empty*)
				I(label_list,value_list, [(current_pointer_name,0,a)], Label(current_pointer_name,i),address+1)
				
			|addToken(I(label_list,value_list,(pointer_name,n,t) :: rest,Label(current_pointer_name,i),address), a) =
				if current_pointer_name <> pointer_name then
					I(label_list,value_list, (current_pointer_name,0,a) :: ((pointer_name,n,t) :: rest), Label(current_pointer_name,i),address+1) (*if we change pointer*)
				else
					I(label_list,value_list, ((pointer_name,n+1,a) :: ((pointer_name,n,t) :: rest)), Label(current_pointer_name,i),address+1)
			|addToken(_,_) = raise ASSEMBLER "Something went horribly wrong :("
			
			fun getTokenPointer(I(label_list,value_list,(name,offs,tok) :: token_list ,current_label,address)) = name
			(*|getTokenPointer(I(label_list,value_list,[(name,offs,tok)],current_label,address)) = name*)
			|getTokenPointer(I(label_list,value_list,[],current_label,address)) = raise ASSEMBLER "Tried to get name from empty token list"
			
			fun dumpTokenList(i) = 
				let
					val token_list = List.rev(getTokenList(i))
					
					fun makePretty(s) = 
						case String.size(s) of
						1 => "00000"^s
						|2 => "0000"^s
						|3 => "000"^s
						|4 => "00"^s
						|5 => "0"^s
						|_ => s
					
					fun printPretty ([]) = ()
					|printPretty((p,n,Ic(i))::xs) = (print (p ^"+"^ Int.toString(n)^": "^ makePretty(Int.toString(i))  ^"\n"); printPretty(xs))
					|printPretty((p,n,Ref(s))::xs) = (print (p ^"+"^ Int.toString(n)^": "^ s  ^"\n"); printPretty(xs))
					|printPretty((p,n,Arg(i))::xs) = (print (p ^"+"^ Int.toString(n)^": "^ makePretty(Int.toString(i))  ^"\n"); printPretty(xs)) 
				in
					printPretty(token_list)
				end
			
		end
	
	val initial = Inter.initial
	
	fun dumpTokenList(i) = Inter.dumpTokenList(i)
	
	fun error(line_number,message,cause) = "\nSYNTAX ERROR!\n" ^ message ^ " at line: " ^ Int.toString(line_number) ^ "\n"
				^ "Caused by: "  ^ cause ^"\n"
	



	(* scanLine
	Scans one line and adds it to an intermediate structure
	
	Arguments:
		line = 	Line to be scanned
		l = 		Current line number
		i = 		Current intermediate structure
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
						
						|[m,r,w] => 
							let
								val assert_argument_write = Resolve.isValidWrite(m,Resolve.write(w)) orelse 
										(print (error(l,"Write argument is forbidden",line));raise SYNTAX "")
										
								val assert_argument_read = Resolve.isValidRead(m,Resolve.read(r)) orelse  
										(print (error(l,"Read argument is forbidden",line));raise SYNTAX "")
										
								val assert_no_s = ((w <> "$s") andalso (w <> "$s")) orelse  
										(print (error(l,"Forbidden argument",line));raise SYNTAX "")
										
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
	
	(*
		Scans and tokenizes a list of strings. Creates the intermediate structure to be assembled.
	*)
	fun scanList([],i,n) = i
	|scanList(x::xs,i,n) =scanList(xs,scanLine(x,i,n),n+1) 
	
	fun resolveAddresses(i) =
		let
			
			val token_list = List.rev(Inter.getTokenList(i))
			val value_list = List.rev(Inter.getValueList(i))
			val label_list = List.rev(Inter.getLabelList(i))
			
			(*
				Assigns each label pointer its correct adress
			*)
			fun resolveLabels([],[],current_label,current_adress) = []
				|resolveLabels(label_list,[],current_label,current_adress) = []
				|resolveLabels(label_list as (label :: rest_label),token_list as ((token as (name,offs,tok)) :: rest_token),NULL,current_adress) = 
					setPointerAddress(label,current_adress) :: resolveLabels(rest_label,rest_token,label,current_adress+1)
				|resolveLabels([],token_list as ((token as (name,offs,tok)) :: rest_token),current_label,current_adress) =
					if getPointerName(current_label) = name then
							resolveLabels(label_list,rest_token,current_label,current_adress+1)
						else 
							raise ASSEMBLER "Found un initialized label :("
				|resolveLabels(label_list as (label :: rest_label),token_list as ((token as (name,offs,tok)) :: rest_token),current_label,current_adress) =
					if getPointerName(current_label)  =  name then
						resolveLabels(label_list,rest_token,current_label,current_adress+1)
					else 
						setPointerAddress(label,current_adress) :: resolveLabels(rest_label,rest_token,label,current_adress+1)
						
			val resolved_labels = resolveLabels(label_list,token_list,NULL,base_adress)

			(*
				Resolves any label pointer to its adress.
			*)
			fun firstPass(resolved_labels,[]) =[] 
			|firstPass(resolved_labels,((label_name,offs,Ref(name)) :: token_rest)) = 
				let
					val label_address = getPointerAddress(Option.valOf(List.find (fn x => (label_name = getPointerName(x))) resolved_labels))
					val arg_address = (List.find (fn x => (name = getPointerName(x))) resolved_labels)
					handle Option => raise ASSEMBLER ("Couldnt find label: " ^ label_name ^ " or " ^ name)
				in
					case arg_address of
					NONE => (label_address+offs,Ref(name)) :: firstPass(resolved_labels,token_rest)
					|SOME(a) => (label_address+offs,Ic(getPointerAddress(a))) :: firstPass(resolved_labels,token_rest) 
				end
			|firstPass(resolved_labels,((label_name,offs,a) :: token_rest)) = 
				let
					val label_address = getPointerAddress(Option.valOf(List.find (fn x => (label_name = getPointerName(x))) resolved_labels))
					handle Option => raise ASSEMBLER ("Couldnt find label    " ^ label_name)
				in
					(label_address+offs,a : token) :: firstPass(resolved_labels,token_rest) 
				end
			
			val pass1 = firstPass(resolved_labels,token_list)
			
			val max_address = #1(List.last(pass1)) 
		in
			(*resolved_labels*)
			List.last(pass1)
		end

end

val asm_code = [
"% This is just a comment",
"% Here we have a label specifying that",
"% all the instructions from here on should lie",
"% after the adress wich will be assigned",
"% to the #start pointer",
"#start",
"MOV 10 x",
"MOV 189 y",
"% This declares a adress pointer.",
"@result",
"ADD x y",
"MOV s $result",
"% Here we place a new label.",
"#loop",
"MOV $result x",
"INC x",
"JEQ 1000 s",
"JMP loop",
"MOV $result s",
"HLT",
"#troll",
"MOV $result x",
"INC x",
"JEQ 1000 s",
"JMP loop",
"MOV $result s",
"HLT"
];
val intermediate_state = Assembler.scanList(asm_code,Assembler.initial,1);
Assembler.dumpTokenList(intermediate_state);
val fitta = Assembler.resolveAddresses(intermediate_state);