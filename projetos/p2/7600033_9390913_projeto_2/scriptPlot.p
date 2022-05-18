set terminal postscript
set output "grafico.ps"

set title  "Periodo Orbital do planeta : -------"
set xlabel "x(UA)"
set ylabel 'y(UA)'
set xzeroaxis
set yzeroaxis
set border 0
set xtics axis
set ytics axis
set ticscale 0 
set size square

# if arrows are wanted only in the positive direction

f(x) = sqrt(1-x**2)

set style line 1 lc rgb "#8b0000" lw 2
set style line 2 lc 0 pt 7 ps 0.9 lw 1
plot [-3:3][-3:3]f(x) ls 2, -f(x) ls 2,'espaco.dat' u 1:2 w d
splot [-3:3][-3:3] x**2 +y**2, 'espaco.dat' u 1:2:5 w lp

p [-2:2][-2:2]'espaco.dat' u 1:2 title "Trajetoria" w d
