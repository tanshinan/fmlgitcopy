
use "OpcodeResolve.sml";

(*datatype intermediate = I of (((string*int) list) * ((string) list) * (( (string*int)*token) list))*)

structure Assembler = 
struct

	datatype pointer = Label of (string * (int option)) | Value of (string * (int option)) | Null;
	datatype token = Ic of int | Ref of pointer | Argument of int;
	
	exception SYNTAX of string
	exception ASSEMBLER of string
	
	val comment_flag = 37 (* Char.ord(#"%")*)
	val label_flag = 35 (*Char.ord(#"#") gets flaged with a syntax error :/*)
	val value_flag = 64 (*Char.ord(#"@")*) 
	
	
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
			
			fun setLabelList(I(label_list,value_list,token_list,curretn_label,address), a) = I(a,value_list,token_list,curretn_label,address)
			fun setValueList(I(label_list,value_list,token_list,curretn_label,address), a) = I(label_list,a,token_list,curretn_label,address)
			fun setTokenList(I(label_list,value_list,token_list,curretn_label,address), a) = I(label_list,value_list,a,curretn_label,address)
			fun setCurrentLabel(I(label_list,value_list,token_list,curretn_label,address), a) = I(label_list,value_list,token_list,a,address)
			
			fun IncrementAdress(I(label_list,value_list,token_list,curretn_label,address), a) = I(label_list,value_list,token_list,curretn_label,address+a)
			
			fun addLabel(I(label_list,value_list,token_list,curretn_label,address), a) =I(a::label_list,value_list,token_list,curretn_label,address)
			fun addValue(I(label_list,value_list,token_list,curretn_label,address), a) =I(label_list,a::value_list,token_list,curretn_label,address)
			
		end
	
	val initial = Inter.initial
	
	fun error(line_number,message,cause) = "\nSYNTAX ERROR!\n" ^ message ^ " at line: " ^ Int.toString(line_number) ^ "\n"
				^ "Caused by: "  ^ cause ^"\n"
	
	fun scanLine(line, i, l)  =
		let
			val line = StringUtills.trim(line)
			val line_head = Char.ord(List.hd(String.explode(line)))
			val line_tail = String.implode(List.tl(String.explode(line)))
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
				|_ => i (*this is magic*)(
					let
						val expression = StringUtills.spaceSplit(line)
						 
					in
						
					end
				
				)
		)
		end

end
val lol_test = Assembler.scanLine("%utter fitta och sådant trevligt", Assembler.initial,1);
val lol_test = Assembler.scanLine("#lolTrol", lol_test,2);
val lol_test = Assembler.scanLine("@roflol", lol_test,3);