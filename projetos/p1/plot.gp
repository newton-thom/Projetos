#esse script eh para o fazer xy
set terminal png size 1000,000
set output 'plano.png'

set xlabel 'X'
set ylabel 'Y'
set title 'Y -  X'
plot  [-150:150][-150:150]"dados.txt" using 1:2 w lp title'x(t)y(t)','mesaquad.txt' w l lw 3 linecolor -1



 


