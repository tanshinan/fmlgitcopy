% This is a stupid example for the assembler.
% It it supposed to do some floating point arithmetic but lord knows if it works.

#start

MOV $x $y
#get_exp
MOV s y
DIV y 100
MUL s 100
SUB y s
RET

#get_mant
DIV s 100
RET

% mantissa is at top and exp below
#combine
MUL s 100
MOV s y
ADD s y
MOV y s
RET

%(number at top, multiplicand below)
#raise_exp
MOV s y
ADD s y
RET

%(number at top, multiplicand below)
#Lower_exp
MOV s y
SUB s y
RET

#abs
MOV s y
BLE s 0
JMP abs_less
MOV y s
RET
#abs_less
MUL y -1
RET


#fadd
@n1_add
@n2_add
@e1_add
@e2_add
@m1_add
@m2_add
% First we get the exponent and the mantissa
MOV s $n1_add
MOV s $n2_add
MOV $n1_add s
JSR get_exp
MOV s $e1_add
MOV $n2_add s
JSR get_exp
MOV s $e2_add
MOV $n1_add s
JSR get_mant
MOV s $m1_add
MOV $n2_add s
JSR get_mant
MOV s $m2_add
MOV $e1_add x
SUB x $e2_add
@difference_add
@new_e_add
MOV s $difference_add
BLE x $e2_add
JMP e2_is_min_add
% e1_add is smallest
MOV $difference_add s
MOV $e1_add s
JSR raise_exp
MOV s $new_e_add
JMP continue_add
#e2_is_min_add
MOV $difference_add s
MOV $e1_add s
JSR raise_exp
MOV s $new_e_add
#continue_add
MOV $new_e_add s
MOV $m1_add x
MOV $m2_add y
ADD x y
JSR combine

#sub
@n1_sub
@n2_sub
@e1_sub
@e2_sub
@m1_sub
@m2_sub
% First we get the exponent and the mantissa
MOV s $n1_sub
MOV s $n2_sub
MOV $n1_sub s
JSR get_exp
MOV s $e1_sub
MOV $n2_sub s
JSR get_exp
MOV s $e2_sub
MOV $n1_sub s
JSR get_mant
MOV s $m1_sub
MOV $n2_sub s
JSR get_mant
MOV s $m2_sub
MOV $e1_sub x
SUB x $e2_sub
@difference_sub
@new_e_sub
MOV s $difference_sub
BLE x $e2_sub
JMP e2_is_min_sub
% e1 is smallest
MOV $difference_sub s
MOV $e1_sub s
JSR raise_exp
MOV s $new_e_sub
JMP continue_sub
#e2_is_min_sub
MOV $difference_sub s
MOV $e1_sub s
JSR raise_exp
MOV s $new_e_sub
#continue_sub
MOV $new_e_sub s
MOV $m1_sub x
MOV $m2_sub y
SUB x y
JSR combine



@eof
