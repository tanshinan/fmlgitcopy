Here goes all of the files and folders for the actuall VM implementation

Git is silly..... It makes no sense

Should we implement floating point arithmectis? No we shouldnt.

DUUUDES!
We now have a partially working assembler :) It handles the parsing and tokenization. 
There is only address resolution left to do.

This:
	% This is just a comment
	% Here we have a label specifying that
	% all the instructions from here on should lie
	% after the adress wich will be assigned
	% to the #start pointer
	#start
	MOV 10 x
	MOV 189 y
	% This declares a adress pointer.",
	@result
	ADD x y
	MOV s $result
	% Here we place a new label.
	#loop
	MOV $result x
	INC x
	JEQ 1000 s
	JMP loop
	MOV $result s
	HLT

Now becomes:
	address: data
	start+0: 000006
	start+1: 000010
	start+2: 000016
	start+3: 000189
	start+4: 000310
	start+5: 000052
	start+6: result
	loop+0: 000005
	loop+1: result
	loop+2: 000100
	loop+3: 020026
	loop+4: 001000
	loop+5: 010080
	loop+6: loop
	loop+7: 000025
	loop+8: result
	loop+9: 100000
