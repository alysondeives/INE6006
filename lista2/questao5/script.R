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
df$Opinião = as.factor(df$Opinião)

#ordena os fatores na ordem desejada
df$Opinião = factor(df$Opinião,levels = c("Muito insatisfeito","Insatisfeito","Indiferente","Satisfeito","Muito satisfeito"))

df$Renda = as.numeric(df$Renda)
df$Idade = as.integer(df$Idade)

abbrvRegiao <- c("Arati.", "Baepe.", "Itama.", "Jaque.", "Parana.")
#----------------------------------------------------------------------------------------------------

insatis = "Insatisfeito"
muitoInsat = "Muito insatisfeito"
n <- 200 #tamanho da amostra
# Excluir as linhas com registros perdidos
pop <- na.omit(df$Opinião)
N <- length(pop)
# Amostra aleatória simples sem reposição de 200 elementos
amostra <- sample(pop, size = n, replace = FALSE)
qtdOpinioesNegativas <- length(which(amostra==insatis|amostra==muitoInsat))
pAmostral <- qtdOpinioesNegativas/n
# Salvar amostra em arquivo .csv
write.csv(amostra, file = "sampleOpiniao.csv", row.names = FALSE, 
          na="", fileEncoding = "UTF-8", eol="\r\n")
# (a) Como 20*n < N, pAmostral é normal com média p e stdDev = sqrt(p*(1-p)/n)
up <- pAmostral
sp <- sqrt(pAmostral*(1-pAmostral)/n)
zy <- 1.96
delta <- zy*sp
ic=c(up-delta, up+delta)

# (b) Qual o tamanho da amostra para que o E0 = 2%?
#Usando a amostra do item (a) como como uma aproximação da proporção populacional:
E0 <- 0.02
n0b <- (zy**2*pAmostral*(1-pAmostral))/E0**2
nb <- (N*n0b) / (N+n0b-1)

# Isolando n0 na fórmula que calcula n dado n0:
 #n0 = (n - n*N)/(n-N)
# https://www.wolframalpha.com/input/?i=solve+n%3D(N*m)%2F(N%2Bm-1)+for+m
# Isolando E0 na fórmula que calcula n0:
 #E0 = sqrt(zy*p*(1-p)/n0)
# Erro amostral de uma amostra de 200 alunos:
E1 <- sqrt((zy*pAmostral*(1-pAmostral)) / ((n-n*N)/(n-N)))

# (c) Qual o tamanho da amostra para que o E0 = 2%? sem amostra piloto?
E0 <- 0.02
n0c <- (zy**2) / (4*E0**2)
nc <- (N*n0c) / (N+n0c-1)

#----------------------------------------------------------------------------------------------------
# Write parameters as latex commands
#Note: open = "w" means open for writing (nothing about truncation is specified)
#      however, it does truncate the file on this line, but avoids truncation on every cat.
out <- file("vars.tex", open = "w")
cat(sprintf("\\newcommand{\\CINCOqtdOpinioesNegativas}{%d\\xspace}\n", qtdOpinioesNegativas), file = out)
cat(sprintf("\\newcommand{\\CINCOpAmostral}{%.2f\\xspace}\n", pAmostral), file = out)
cat(sprintf("\\newcommand{\\CINCON}{%d\\xspace}\n", N), file = out)
cat(sprintf("\\newcommand{\\CINCOn}{%d\\xspace}\n", n), file = out)
cat(sprintf("\\newcommand{\\CINCOzy}{%.2f\\xspace}\n", zy), file = out)
cat(sprintf("\\newcommand{\\CINCOAdelta}{%.4f\\xspace}\n", delta), file = out)
cat(sprintf("\\newcommand{\\CINCOAICinf}{%.2f\\xspace}\n", ic[1]), file = out)
cat(sprintf("\\newcommand{\\CINCOAICsup}{%.2f\\xspace}\n", ic[2]), file = out)
cat(sprintf("\\newcommand{\\CINCOBE}{%.4f\\xspace}\n", E1), file = out)
cat(sprintf("\\newcommand{\\CINCOBn}{%.4f\\xspace}\n", nb), file = out)
cat(sprintf("\\newcommand{\\CINCOBnceil}{%d\\xspace}\n", ceiling(nb)), file = out)
cat(sprintf("\\newcommand{\\CINCOBnz}{%.4f\\xspace}\n", n0b), file = out)
cat(sprintf("\\newcommand{\\CINCOCnz}{%.4f\\xspace}\n", n0c), file = out)
cat(sprintf("\\newcommand{\\CINCOCn}{%.4f\\xspace}\n", nc), file = out)
cat(sprintf("\\newcommand{\\CINCOCnceil}{%d\\xspace}\n", ceiling(nc)), file = out)
close(out)
