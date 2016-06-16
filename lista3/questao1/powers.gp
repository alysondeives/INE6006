set terminal cairolatex pdf color
set output "questao1/powers.tex"

set xrange [20:34]
set yrange [0:1.05]
set xtics 1
set ytics 0,0.1,1
set key on inside right bottom 
set xlabel "$\\mu$"
set ylabel "$1-\\beta$"

set datafile separator ","
plot 'questao1/powers.csv' using (column("u")):(column("beta.b")) smooth mcsplines \
                  title "$1-\\beta_b$" with lines lt 1 lw 3, \
     'questao1/powers.csv' using (column("u")):(column("beta.d")) smooth mcsplines \
                  title "$1-\\beta_d$" with lines lt 2 lw 3
