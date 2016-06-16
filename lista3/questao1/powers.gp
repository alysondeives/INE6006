set terminal latex
set output "powers.tex"
#set grid nopolar
#set grid xtics nomxtics ytics nomytics noztics nomztics \
# nox2tics nomx2tics noy2tics nomy2tics nocbtics nomcbtics
#set grid layerdefault   lt 0 dt 3 linewidth 0.500,  lt 0 dt 3 linewidth 0.500

set xrange [20:34]
set yrange [0:1.05]
set xtics 1
set ytics 0,0.1,1
set key on inside right bottom 
set xlabel "$\\mu$"
set ylabel "$1-\\beta$"

set datafile separator ","
plot 'powers.csv' using (column("u")):(column("beta.b")) title "$1-\\beta_b$" with lines lt 2, \
     'powers.csv' using (column("u")):(column("beta.d")) title "$1-\\beta_d$" with lines lt 1
