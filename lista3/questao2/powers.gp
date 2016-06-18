set datafile separator ";"

set terminal cairolatex pdf enhanced color
set output "powers.tex"

set xrange [3.50:4.35]
set yrange [0.0:1.0]
#set xtics 3.50,0.05,4.35
#set ytics 0,0.1,1
set key on inside right top 
set xlabel "Media"
set ylabel "Powder do Teste"

plot 'powers.csv' using 1:2 title "40" with linespoints lt 1, \
     'powers.csv' using 1:3 title "50" with linespoints lt 2, \
     'powers.csv' using 1:4 title "60" with linespoints lt 3, \
     'powers.csv' using 1:5 title "70" with linespoints lt 4
