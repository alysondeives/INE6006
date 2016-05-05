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
zy <- 1.96
ic=c(up-zy*sp, up+zy*sp)

# (b) Qual o tamanho da amostra para que o E0 = 2%?
#Usando a amostra do item (a) como como uma aproximação da proporção populacional:
E0 <- 0.02
n0 <- (zy**2*pAmostral*(1-pAmostral))/E0**2
nb <- (N*n0) / (N+n0-1)

# Isolando n0 na fórmula que calcula n dado n0:
# n0 = (n - n*N)/(n-N)
# https://www.wolframalpha.com/input/?i=solve+n%3D(N*m)%2F(N%2Bm-1)+for+m
# Isolando E0 na fórmula que calcula n0:
# E0 = sqrt(zy*p*(1-p)/n0)
# Erro amostral de uma amostra de 200 alunos:
E1 <- sqrt((zy*pAmostral*(1-pAmostral)) / ((n-n*N)/(n-N)))

# (c) Qual o tamanho da amostra para que o E0 = 2%? sem amostra piloto?
E0 <- 0.02
n0 <- (zy**2) / (4*E0**2)
nc <- (N*n0) / (N+n0-1)
