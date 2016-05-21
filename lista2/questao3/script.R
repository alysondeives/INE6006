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

# Excluir as linhas com registros perdidos
pop <- na.omit(df$Renda)


q <- 200 #quantidade de amostras
N <- length(pop) #tamanho da população
mean_pop = mean(pop) #media da população
#[1] 2.336

n = 20
amostra <- sample(pop, size = n, replace = FALSE)

# Salvar amostras em arquivo .csv
write.csv(amostra, file   = "sample.csv", row.names = FALSE, na="", fileEncoding = "UTF-8", eol="\r\n")
#------------------------------------------------------------------------------------------------------------
amostra <- read.csv(file = "./sample.csv", na.strings = c("NA",""," "), header=TRUE)
#media amostra
x = mean(amostra$x)
#[1] 2.471

#como n = 20, t com 19 graus de liberdade e 95% de confiança = 2.093
t = 2.093
delta = t*(sd(amostra$x)/sqrt(n))
print(delta)
#[1] 0.6249
ic = c(x-delta,x+delta)
print(ic)
#[1] 1.847 3.096

#a) O IC é de média 2.471 +/- 0.6249 (1.847 a 3.096) e a média populacional é de 2.336, portanto o intervalo está dentro da média populacional

t99 = 2.861
delta99 = t99*(sd(amostra$x)/sqrt(n))
print(delta99)
#[1] 0.8542
ic99=c(x-delta,x+delta)
print(ic99)
#[1] 1.617 3.326

#b) Sim, o intervalo ficou maior. Poderia aumentar o tamanho da amostra pq...

E0 = 1.5
n0 = (t99*sd(amostra$x)/E0)*(t99*sd(amostra$x)/E0)
#[1] 25.94

#c) O numero de amostras deveria ser 26
#conferindo
amostra99 <- sample(pop, size = 26, replace = FALSE)
xb = mean(amostra99)
delta99b = 2.787*(sd(amostra99)/sqrt(26))
#[1] 0.6806
ic99b=c(xb-delta99b,xb+delta99b)
#[1] 1.731 3.093
#----------------------------------------------------------------------------------------------------

#----------------------------------------------------------------------------------------------------
# Write parameters as latex commands
#Note: open = "w" means open for writing (nothing about truncation is specified)
#      however, it does truncate the file on this line, but avoids truncation on every cat.
out <- file("vars.tex", open = "w")
cat(sprintf("\\newcommand{\\TRESicmin}{%.2f\\xspace}\n", ic[1]), file = out)
cat(sprintf("\\newcommand{\\TRESicmax}{%.2f\\xspace}\n", ic[2]), file = out)

cat(sprintf("\\newcommand{\\TRESicNoveNovemin}{%.2f\\xspace}\n", ic99[1]), file = out)
cat(sprintf("\\newcommand{\\TRESicNoveNovemax}{%.2f\\xspace}\n", ic99[2]), file = out)

cat(sprintf("\\newcommand{\\TRESnZero}{%.0f\\xspace}\n", n0), file = out)
close(out)
