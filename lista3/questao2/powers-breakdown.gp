set datafile separator ";"

set terminal postscript eps enhanced color
set output "powers-breakdown.eps"

set xrange [3.50:4.35]
#set xtics 3.50,0.05,4.35
#set ytics 0,0.1,1
set key on inside right top 
set xlabel "Media"
set ylabel "Poder do Teste / Tamanho da Amostra"

plot 'powers-breakdown.csv' using 1:2 title "40" with linespoints lt 1, \
     'powers-breakdown.csv' using 1:3 title "50" with linespoints lt 2, \
     'powers-breakdown.csv' using 1:4 title "60" with linespoints lt 3, \
     'powers-breakdown.csv' using 1:5 title "70" with linespoints lt 4
