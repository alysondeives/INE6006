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


q <- 1 #quantidade de amostras
N <- length(pop) #tamanho da população
X = mean(pop) #media da população
#[1] 2.336
SD = sd(pop)
#[1] 1.779

n = 20
amostra <- sample(pop, size = n, replace = FALSE)

# Salvar amostras em arquivo .csv
write.csv(amostra, file   = "sample.csv", row.names = FALSE, na="", fileEncoding = "UTF-8", eol="\r\n")
#------------------------------------------------------------------------------------------------------------
amostra <- as.matrix(read.csv(file = "./sample.csv", na.strings = c("NA",""," "), header=TRUE))
amostra = as.numeric(amostra)

x = mean(amostra)
#[1] 2.009
sd = sd(amostra)
print(sd)
#[1] 0.8088

#como n = 20, t com 19 graus de liberdade e 95% de confiança = 2.093
t = 2.093
delta = t*(sd/sqrt(n))*((N-n)/(N-1))
print(delta)
#[1] 0.3771
ic = c(x-delta,x+delta)
print(ic)
#[1] 1.631 2.386

#a) O IC é de média 2.471 +/- 0.6249 (1.847 a 3.096) e a média populacional é de 2.336, portanto o intervalo está dentro da média populacional

t99 = 2.861
delta99 = t99*(sd/sqrt(n))*((N-n)/(N-1))
print(delta99)
#[1] 0.5155
ic99=c(x-delta99,x+delta99)
print(ic99)
#[1] 1.493 2.524

#b) Sim, o intervalo ficou maior. Poderia aumentar o tamanho da amostra pq...


E0 = 1.5
n0 = (t99*sd/E0)*(t99*sd/E0)
print(n0)
#[1] 2.38
n0Rounded = ceiling(n0)
print(n0Rounded)
#[1] 3

n0corrigido = (n0*N)/(n0+N)
print(n0corrigido)
#[1] 2.38
n0corrigidoRounded = ceiling(n0corrigido)
print(n0corrigidoRounded)
#[1] 3

#----------------------------------------------------------------------------------------------------
# Write parameters as latex commands
#Note: open = "w" means open for writing (nothing about truncation is specified)
#      however, it does truncate the file on this line, but avoids truncation on every cat.
out <- file("vars.tex", open = "w")
cat(sprintf("\\newcommand{\\TRESN}{%.0f\\xspace}\n", N), file = out)
cat(sprintf("\\newcommand{\\TRESX}{%.3f\\xspace}\n", X), file = out)
cat(sprintf("\\newcommand{\\TRESSD}{%.3f\\xspace}\n", SD), file = out)

cat(sprintf("\\newcommand{\\TRESn}{%.0f\\xspace}\n", n), file = out)
cat(sprintf("\\newcommand{\\TRESx}{%.3f\\xspace}\n", x), file = out)
cat(sprintf("\\newcommand{\\TRESsd}{%.3f\\xspace}\n", sd), file = out)

cat(sprintf("\\newcommand{\\TRESdelta}{%.3f\\xspace}\n", delta), file = out)
cat(sprintf("\\newcommand{\\TRESicmin}{%.3f\\xspace}\n", ic[1]), file = out)
cat(sprintf("\\newcommand{\\TRESicmax}{%.3f\\xspace}\n", ic[2]), file = out)

cat(sprintf("\\newcommand{\\TRESdeltaNoveNove}{%.3f\\xspace}\n", delta99), file = out)
cat(sprintf("\\newcommand{\\TRESicNoveNoveMin}{%.3f\\xspace}\n", ic99[1]), file = out)
cat(sprintf("\\newcommand{\\TRESicNoveNoveMax}{%.3f\\xspace}\n", ic99[2]), file = out)

cat(sprintf("\\newcommand{\\TRESnZero}{%.2f\\xspace}\n", n0), file = out)
cat(sprintf("\\newcommand{\\TRESnZeroRounded}{%.0f\\xspace}\n", n0Rounded), file = out)
cat(sprintf("\\newcommand{\\TRESnZeroCorrigido}{%.2f\\xspace}\n", n0corrigido), file = out)
cat(sprintf("\\newcommand{\\TRESnZeroCorrigidoRounded}{%.0f\\xspace}\n", n0corrigidoRounded), file = out)

close(out)
