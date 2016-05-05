library(tables)
library(xtable)
#----------------------------------------------------------------------------------------------------
options(digits = 4, width = 100) #set precision for float values and and chars by line
par(xpd=TRUE,cex.lab=1.5) #set global options for plot
#----------------------------------------------------------------------------------------------------
#Read from csv
df = read.csv(file = "../processed.csv",na.strings = c("NA",""," "),header=TRUE)

#set columns types
df$Região = as.factor(df$Região)
df$Área = as.factor(df$Área)
df$Pagamento = as.factor(df$Pagamento)

#ordena os fatores na ordem desejada
df$Opinião = factor(df$Opinião,levels = c("Muito insatisfeito","Insatisfeito","Indiferente","Satisfeito","Muito satisfeito"))

df$Renda = as.numeric(df$Renda)
df$Idade = as.integer(df$Idade)

abbrvRegiao <- c("Arati.", "Baepe.", "Itama.", "Jaque.", "Parana.")
#----------------------------------------------------------------------------------------------------

incFed = "Incentivos federais"
n <- 200 #tamanho da amostra
# Excluir as linhas com registros perdidos
pop <- na.omit(df$Pagamento)
N <- length(pop)
# Amostra aleatória simples sem reposição de 200 elementos
amostra <- sample(pop, size = n, replace = FALSE)
pAmostral <- length(which(amostra==incFed))/n
# (a) Como 20*n < N, pAmostral é normal com média p e stdDev = sqrt(p*(1-p)/n)
up <- pAmostral
sp <- sqrt(pAmostral*(1-pAmostral)/n)
ic=c(up-1.96*sp, up+1.96*sp)
