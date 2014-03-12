%
% 	This is the assembly test code.
% It is a little LCG random number generator
%	  which pushes them on stack.
#start
JMP run
#mod
:43267
#increment
:3367
#multiplier
:2549 
#seed
:3428
#run
MOV $seed s
MOV 1 x
#loop
MUL $multiplier s
ADD $increment s
MOD $mod s
MOV s y
MOV y s

INC x
ADD eof x
MOV s x
MOV y $x
SUB eof x
MOV s x
BGR x 100
JMP loop
HLT
@eof