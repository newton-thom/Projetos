
set terminal png size 500,500
set output 'VxX.png'
set xlabel 'X'
set ylabel 'Y'
set title 'Vx -  X'
plot  [:][:] "dados.txt" using 3:1 w lp title'Vx(t)-X(t)'

 


