signature TEST =
sig
	type genetailia
	val coitus : genetailia*genetailia -> string
	val hasBabies : genetailia*genetailia -> bool
end;

(*This structure works*)
structure Sex :> TEST =
struct
	
	(*This structure is only visible usable within the Sex structure*)
	structure i_hide_in_the_closet =
		struct
			val good_idea = "Come out of the closet!"
		end
		
	datatype genetailia = Penis | Vagina
	fun coitus(a,b) = 
		case (a,b) of
		(Penis,Vagina) => "Straigt"
		|(Vagina,Penis) => "Straigt"
		|(Penis,Penis) => "Gay"
		|(Vagina,Vagina) => "Lesbian"
	
	fun hasBabies(Penis,Vagina) = true
	|hasBabies(Vagina,penis) = true
	|hasBabies(_,_) = false
end

(*This doesn't since hasBabies is not defined*)
(*
structure Porn :> TEST =
struct
	datatype genetailia = Penis | Vagina
	fun coitus(a,b) = 
		case (a,b) of
		(Penis,Vagina) => "Boring"
		|(Vagina,Penis) => "Boring"
		|(Penis,Penis) => "Gay buttseks"
		|(Vagina,Vagina) => "Girl on girl on Horace Engdhal"

end
*)

(*
This does not work! To make use of nested structures like this require some annoying crap!
Wich i have not figured out. 
val utter = Sex. i_hide_in_the_closet.good_idea;
*)
