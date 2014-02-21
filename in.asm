% This is a trivial program wich sets to x to 123
% increments x and then stores x at result.
% Then the sneaky function increments the value of
% The sneaky function increments the address of
% result. So each new itteration increases the
% address where x is stored.
% So the program fills 1000 memory cells with
% increasing values :)
#start
MOV 0 x
@result
INC x
MOV 1 s
ADD s result
MOV s y
MOV x $y
BEQ x 1000
JMP start
MOV $y s
HLT
