library(tables)
library(xtable)
library(pwr)
#----------------------------------------------------------------------------------------------------
options(digits = 5, width = 100) #set precision for float values and and chars by line
par(xpd=TRUE,cex.lab=1.5) #set global options for plot
#----------------------------------------------------------------------------------------------------
amostra <- read.csv(file = "Questão5.csv", na.strings = c("NA",""," "), header=TRUE)
levels(amostra$Pagamento.C)
mean(amostra$Renda[which(amostra$Pagamento.C=="Incentivos federais")])
mean(amostra$Renda[which(amostra$Pagamento.C=="Outras Formas de Pagamento")])
rowSums(amostra$Renda[which(amostra$Pagamento.C=="Incentivos federais")])
sd(amostra$Renda[which(amostra$Pagamento.C=="Incentivos federais")])
sd(amostra$Renda[which(amostra$Pagamento.C=="Outras Formas de Pagamento")])
incentivos <- subset(amostra, amostra$Pagamento.C=="Incentivos federais")
outras <- subset(amostra, amostra$Pagamento.C=="Outras Formas de Pagamento")
t.test(incentivos$Renda, outras$Renda, alternative = "two.sided", conf.level = 0.95, var.equal = TRUE)
mean(amostra$Renda)
sd(amostra$Renda)
var.test(incentivos$Renda, outras$Renda, alternative = "two.sided", conf.level = 0.99)


