structure StringUtills =
struct
	exception SYNTAX of string
	
	
	val space = 32 (*Char.ord(#" ")*)
	
	fun flatten l =
	let
		fun flatten' ([],[],A) = List.rev(A)
			|flatten' (x::xs,[],A) = flatten'(xs,x,A)
			|flatten' (x,y::ys,A) = flatten'(x,ys,y::A)
	in
		flatten'(l,[],[])
	end 
	
	(*
		Splitts a string at every blankspace into a list of strings.
	*)
	fun spaceSplit("") = []
	|spaceSplit(s) = 
		let
			fun split'([],A1,A2) = List.rev(String.implode(List.rev(A1)) :: A2)
			|split'(x::xs,A1,A2)  =
				if (Char.ord(x) = 32) then
					split'(xs,[],String.implode(List.rev(A1)) :: A2)
				else
					split'(xs,x::A1,A2)
		in
			List.filter (fn x => x <> "") (split'(String.explode(s),[],[]))
		end
	
	(*
		Removes all leading and trailing spaces characters and also runns of spaces.
		This might get changed to remove all whitespace characters.
	*)
	fun trim(s) = 
		let
			fun trim'([],A1,A2,1) = List.rev(A2)
			|trim'([],A1,A2,0) = List.rev(List.rev(A1)::A2)
			|trim'(x :: xs,A1,A2,0) =
				if Char.ord(x) = space then
					trim'(xs,x::[],List.rev(A1)::A2,1)
				else
					trim'(xs,x::A1,A2,0)
			|trim'(x :: xs,A1,A2,1) =
				if Char.ord(x) = space then
					trim'(xs,A1,A2,1)
				else
					trim'(xs,x::[],List.rev(A1)::A2,0)
			|trim'(_,_,_,_) = raise SYNTAX "WTF!?!?!?!?!?!? \n" (*This should never ever happen. Just here to get rid of warning*)
		in
			String.implode(flatten(trim'(String.explode(s),[],[],1)))
		end
		
		(*
			Counts the number of words in a string.
		*)
		fun words(s) = List.length(spaceSplit(trim(s)))
		
end
(*
val test1 = StringUtills.spaceSplit("ABDSA ADSF @kesofitta [hamsterballe]");
val test2 = StringUtills.spaceSplit("NOP");
val test3 = StringUtills.spaceSplit("MOV [x] x");
val test4 = StringUtills.trim("   hej pa          dig     du lol");
*)
