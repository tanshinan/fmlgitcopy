#start


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
#fmul_int
@multiplicand1
MOV s y
MOV s $multiplicand1
MOV y s
JSR get_mant
MOV s x
MOV y s
JSR get_exp
MOV s y
ADD $multiplicand1 y
MUL x $multiplicand1
JSR combine
RET

%(numerator at top, denominator below)
#fdiv_int
@denominator1
MOV s y
MOV s $denominator1
MOV y s
JSR get_mant
MOV s x
MOV y s
JSR get_exp
MOV s y
SUB $denominator1 y
DIV x $denominator1
JSR combine
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
@n1
@n2
@e1
@e2
@m1
@m2
% First we get the exponent and the mantissa
MOV s $n1
MOV s $n2
MOV $n1 s
JSR get_exp
MOV s $e1
MOV $n2 s
JSR get_exp
MOV s $e2
MOV $n1 s
JSR get_mant
MOV s $m1
MOV $n2 s
JSR get_mant
MOV s $m2


@eof
