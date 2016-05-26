library(stats)
options(digits = 4, width = 100) #set precision for float values and and chars by line
par(xpd=TRUE,cex.lab=1.5) #set global options for plot

#Read from csv
df = read.csv(file = "../../lista2/questao2/questao2.csv",na.strings = c("NA",""," "),header=TRUE)

aalpha <- 0.05
au0 <- 27
an <- length(df$Idade)
agl <- an - 1
axbar <- mean(df$Idade)
as <- var(df$Idade)
at <- ((axbar - au0) * sqrt(an)) / as
ap <- pt(at, agl, lower.tail = FALSE)

out <- file("vars.tex", open = "w")
cat(sprintf("\\def\\UMAalpha{\\num[round-mode=off]{%.2f}\\xspace}\n", aalpha), file = out)

cat(sprintf("\\def\\UMAu0{\\num{%d}\\xspace}\n", au0), file = out)
cat(sprintf("\\def\\UMAn{\\num{%d}\\xspace}\n", an), file = out)
cat(sprintf("\\def\\UMAgl{\\num{%d}\\xspace}\n", agl), file = out)

cat(sprintf("\\def\\UMAbarx{\\num{%.4f}\\xspace}\n", axbar), file = out)
cat(sprintf("\\def\\UMAs{\\num{%.4f}\\xspace}\n", as), file = out)
cat(sprintf("\\def\\UMAt{\\num{%.4f}\\xspace}\n", at), file = out)
cat(sprintf("\\def\\UMAp{\\num{%.4f}\\xspace}\n", ap), file = out)

close(out)

