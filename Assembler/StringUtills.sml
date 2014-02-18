structure StringUtills =
struct
	exception SYNTAX of string
	
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
			split'(String.explode(s),[],[])
		end 
		
end
(*
val test1 = StringUtills.spaceSplit("ABDSA ADSF @kesofitta [hamsterballe]");
val test2 = StringUtills.spaceSplit("NOP");
val test3 = StringUtills.spaceSplit("MOV [x] x");
*)