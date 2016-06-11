library(tables)
library(xtable)
library(pwr)
#----------------------------------------------------------------------------------------------------
options(digits = 5, width = 100) #set precision for float values and and chars by line
par(xpd=TRUE,cex.lab=1.5) #set global options for plot
#----------------------------------------------------------------------------------------------------
amostra <- as.matrix(read.csv(file = "../../lista2/questao5/sampleOpiniao.csv", na.strings = c("NA",""," "), header=TRUE))
y <- length(which(amostra=="Insatisfeito"|amostra=="Muito insatisfeito"))
n = length(amostra)
pAmostral <- y/n
p0 = 0.2
yLinha = ifelse(y>n*p0,y+0.5,y-0.5)
z = (yLinha-n*p0)/sqrt(n*p0*(1-p0))

pValue = 1 - pnorm(abs(z))
#----------------------------------------------------------------------------------------------------
probList <- c(0.20, 0.21, 0.22, 0.23, 0.24, 0.25, 0.26, 0.27)

effectSize <- ES.h(probList, 0.20)

power = pwr.p.test(h = effectSize, n = 200, sig.level = 0.01, power = NULL, alternative = "greater")
print(power)
#----------------------------------------------------------------------------------------------------
effectSizeAmostra = ES.h(0.23,0.20)
tamanhoAmostra = pwr.p.test(h = effectSizeAmostra, n = NULL, sig.level = 0.01, power = 0.99, alternative = "greater")$n
tamanhoAmostraRounded = ceiling(tamanhoAmostra)

#----------------------------------------------------------------------------------------------------
# Write parameters as latex commands
#Note: open = "w" means open for writing (nothing about truncation is specified)
#      however, it does truncate the file on this line, but avoids truncation on every cat.
out <- file("vars.tex", open = "w")
cat(sprintf("\\newcommand{\\QUATROpAmostral}{\\num{%.4f}\\xspace}\n", pAmostral), file = out)
cat(sprintf("\\newcommand{\\QUATROn}{%d\\xspace}\n", n), file = out)
cat(sprintf("\\newcommand{\\QUATROy}{\\num{%.4f}\\xspace}\n", y), file = out)
cat(sprintf("\\newcommand{\\QUATROyLinha}{\\num{%.4f}\\xspace}\n", yLinha), file = out)
cat(sprintf("\\newcommand{\\QUATROz}{\\num{%.4f}\\xspace}\n", z), file = out)
cat(sprintf("\\newcommand{\\QUATROpValue}{\\num{%.4f}\\xspace}\n", pValue), file = out)

cat(sprintf("\\newcommand{\\QUATROesVinte}{\\num{%.4f}\\xspace}\n", effectSize[1]), file = out)
cat(sprintf("\\newcommand{\\QUATROesVinteUm}{\\num{%.4f}\\xspace}\n", effectSize[2]), file = out)
cat(sprintf("\\newcommand{\\QUATROesVinteDois}{\\num{%.4f}\\xspace}\n", effectSize[3]), file = out)
cat(sprintf("\\newcommand{\\QUATROesVinteTres}{\\num{%.4f}\\xspace}\n", effectSize[4]), file = out)
cat(sprintf("\\newcommand{\\QUATROesVinteQuatro}{\\num{%.4f}\\xspace}\n", effectSize[5]), file = out)
cat(sprintf("\\newcommand{\\QUATROesVinteCinco}{\\num{%.4f}\\xspace}\n", effectSize[6]), file = out)
cat(sprintf("\\newcommand{\\QUATROesVinteSeis}{\\num{%.4f}\\xspace}\n", effectSize[7]), file = out)
cat(sprintf("\\newcommand{\\QUATROesVinteSete}{\\num{%.4f}\\xspace}\n", effectSize[8]), file = out)

cat(sprintf("\\newcommand{\\QUATROpVinte}{\\num{%.4f}\\xspace}\n", power$power[1]), file = out)
cat(sprintf("\\newcommand{\\QUATROpVinteUm}{\\num{%.4f}\\xspace}\n", power$power[2]), file = out)
cat(sprintf("\\newcommand{\\QUATROpVinteDois}{\\num{%.4f}\\xspace}\n", power$power[3]), file = out)
cat(sprintf("\\newcommand{\\QUATROpVinteTres}{\\num{%.4f}\\xspace}\n", power$power[4]), file = out)
cat(sprintf("\\newcommand{\\QUATROpVinteQuatro}{\\num{%.4f}\\xspace}\n", power$power[5]), file = out)
cat(sprintf("\\newcommand{\\QUATROpVinteCinco}{\\num{%.4f}\\xspace}\n", power$power[6]), file = out)
cat(sprintf("\\newcommand{\\QUATROpVinteSeis}{\\num{%.4f}\\xspace}\n", power$power[7]), file = out)
cat(sprintf("\\newcommand{\\QUATROpVinteSete}{\\num{%.4f}\\xspace}\n", power$power[8]), file = out)

cat(sprintf("\\newcommand{\\QUATROesAmostra}{\\num{%.4f}\\xspace}\n", effectSizeAmostra), file = out)
cat(sprintf("\\newcommand{\\QUATROtamanhoAmostra}{\\num{%.4f}\\xspace}\n", tamanhoAmostra), file = out)
cat(sprintf("\\newcommand{\\QUATROtamanhoAmostraRounded}{%d\\xspace}\n", tamanhoAmostraRounded), file = out)
close(out)