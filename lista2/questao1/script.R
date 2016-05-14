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

# Amostra aleatória simples sem reposição de 200 elementos
#amostra <- sample(pop, size = n, replace = FALSE)

amostra_4 = matrix(0, ncol = 200, nrow = 4)
amostra_8 = matrix(0, ncol = 200, nrow = 8)
amostra_16 = matrix(0, ncol = 200, nrow = 16)
amostra_30 = matrix(0, ncol = 200, nrow = 30)
amostra_100 = matrix(0, ncol = 200, nrow = 200)

for(i in 1:q){
  amostra_4[,i]   <- sample(pop, size = 4, replace = FALSE)
  amostra_8[,i]   <- sample(pop, size = 8, replace = FALSE)
  amostra_16[,i]  <- sample(pop, size = 16, replace = FALSE)
  amostra_30[,i]  <- sample(pop, size = 30, replace = FALSE)
  amostra_100[,i] <- sample(pop, size = 100, replace = FALSE)
}

# Salvar amostras em arquivo .csv
write.csv(amostra_4, file   = "sample_4.csv", row.names = FALSE, na="", fileEncoding = "UTF-8", eol="\r\n")
write.csv(amostra_8, file   = "sample_8.csv", row.names = FALSE, na="", fileEncoding = "UTF-8", eol="\r\n")
write.csv(amostra_16, file  = "sample_16.csv", row.names = FALSE, na="", fileEncoding = "UTF-8", eol="\r\n")
write.csv(amostra_30, file  = "sample_30.csv", row.names = FALSE, na="", fileEncoding = "UTF-8", eol="\r\n")
write.csv(amostra_100, file = "sample_100.csv", row.names = FALSE, na="", fileEncoding = "UTF-8", eol="\r\n")

#------------------------------------------------------------------------------------------------------------
#Calcular maximos e minmos para media e stddev dentre as 200 amostras

min_mean = c(Inf,Inf,Inf,Inf,Inf)
max_mean = c(0, 0, 0, 0,0)
min_sd = c(Inf,Inf,Inf,Inf,Inf)
max_sd = c(0, 0, 0, 0,0)

for(i in 1:q){
  #Get minimum mean
  if(mean(amostra_4[,i]) < min_mean[1]){
    min_mean[1] = mean(amostra_4[,i])
  }
  if(mean(amostra_8[,i]) < min_mean[2]){
    min_mean[2] = mean(amostra_8[,i])
  }
  if(mean(amostra_16[,i]) < min_mean[3]){
    min_mean[3] = mean(amostra_16[,i])
  }
  if(mean(amostra_30[,i]) < min_mean[4]){
    min_mean[4] = mean(amostra_30[,i])
  }
  if(mean(amostra_100[,i]) < min_mean[5]){
    min_mean[5] = mean(amostra_100[,i])
  }
  
  #Get minimum stddev
  if(sd(amostra_4[,i]) < min_sd[1]){
    min_sd[1] = sd(amostra_4[,i])
  }
  if(sd(amostra_8[,i]) < min_sd[2]){
    min_sd[2] = sd(amostra_8[,i])
  }
  if(sd(amostra_16[,i]) < min_sd[3]){
    min_sd[3] = sd(amostra_16[,i])
  }
  if(sd(amostra_30[,i]) < min_sd[4]){
    min_sd[4] = sd(amostra_30[,i])
  }
  if(sd(amostra_100[,i]) < min_sd[5]){
    min_sd[5] = sd(amostra_100[,i])
  }
  
  #Check for maximum mean
  if(mean(amostra_4[,i]) > max_mean[1]){
    max_mean[1] = mean(amostra_4[,i])
  }
  if(mean(amostra_8[,i]) > max_mean[2]){
    max_mean[2] = mean(amostra_8[,i])
  }
  if(mean(amostra_16[,i]) > max_mean[3]){
    max_mean[3] = mean(amostra_16[,i])
  }
  if(mean(amostra_30[,i]) > max_mean[4]){
    max_mean[4] = mean(amostra_30[,i])
  }
  if(mean(amostra_100[,i]) > max_mean[5]){
    max_mean[5] = mean(amostra_100[,i])
  }
  
  #Check for maximum sd
  if(sd(amostra_4[,i]) > max_sd[1]){
    max_sd[1] = sd(amostra_4[,i])
  }
  if(sd(amostra_8[,i]) > max_sd[2]){
    max_sd[2] = sd(amostra_8[,i])
  }
  if(sd(amostra_16[,i]) > max_sd[3]){
    max_sd[3] = sd(amostra_16[,i])
  }
  if(sd(amostra_30[,i]) > max_sd[4]){
    max_sd[4] = sd(amostra_30[,i])
  }
  if(sd(amostra_100[,i]) > max_sd[5]){
    max_sd[5] = sd(amostra_100[,i])
  }
}

# > mean(pop)
# [1] 2.336
# sd(pop)
# [1] 1.779

# > sd(pop)/sqrt(4)
# [1] 0.8894

# > sd(pop)/sqrt(8)
# [1] 0.6289

# > sd(pop)/sqrt(16)
# [1] 0.4447

# > sd(pop)/sqrt(30)
# [1] 0.3248

# > sd(pop)/sqrt(100)
# [1] 0.1779

# # > min_mean
# [1] 1.232 1.498 1.664 1.700 2.013
# 
# > max_mean
# [1] 7.803 9.397 6.562 4.273 2.965
# 
# > min_sd
# [1] 0.09469 0.23481 0.40150 0.56170 0.96773
# 
# > max_sd
# [1]  9.059 20.799 14.563 10.763  5.989
#-------------------------------------------------------------------------------------

#Calcular media amostral (Aula 6 - Slide 28)
x4 = 0
x8 = 0
x16 = 0
x30 = 0
x100 = 0
for(i in 1:q){
  x4 = x4 + 1/q * mean(amostra_4[,i])
  x8 = x8 + 1/q * mean(amostra_8[,i])
  x16 = x16 + 1/q * mean(amostra_16[,i])
  x30 = x30 + 1/q * mean(amostra_30[,i])
  x100 = x100 + 1/q * mean(amostra_100[,i])
}
print (c(x4,x8,x16,x30,x100))

#[1] 2.205 2.344 2.332 2.329 2.348

print(mean(pop))
#[1] 2.336

# a)Sim, os valores das medias amostrais aproximam-se da media populacional.

#-------------------------------------------------------------------------------------------------
var4 = 0
var8 = 0
var16 = 0
var30 = 0
var100 = 0
for(i in 1:q){
  var4 = var4 + 1/q * (mean(amostra_4[,i])- x4)*(mean(amostra_4[,i])- x4)
  var8 = var8 + 1/q * (mean(amostra_8[,i])- x8)*(mean(amostra_8[,i])- x8)
  var16 = var16 + 1/q * (mean(amostra_16[,i])- x16)*(mean(amostra_16[,i])- x16)
  var30 = var30 + 1/q * (mean(amostra_30[,i])- x30)*(mean(amostra_30[,i])- x30)
  var100 = var100 + 1/q * (mean(amostra_100[,i])- x100)*(mean(amostra_100[,i])- x100)
}
print (c(var4,var8,var16,var30,var100))
#[1] 0.41856 0.78057 0.18612 0.09513 0.03152

print (c(sqrt(var4),sqrt(var8),sqrt(var16),sqrt(var30),sqrt(var100)))
#[1] 0.6470 0.8835 0.4314 0.3084 0.1775

print (c(sd(pop)/sqrt(4),sd(pop)/sqrt(8),sd(pop)/sqrt(16),sd(pop)/sqrt(30),sd(pop)/sqrt(100)))
#[1] 0.8894 0.6289 0.4447 0.3248 0.1779

# b) Sim, é possivel observar isso.

#-----------------------------------------------------------------------------------------------

#----------------------------------------------------------------------------------------------------
# Write parameters as latex commands
#Note: open = "w" means open for writing (nothing about truncation is specified)
#      however, it does truncate the file on this line, but avoids truncation on every cat.
out <- file("vars.tex", open = "w")
cat(sprintf("\\newcommand{\\QUATROpAmostral}{%.2f\\xspace}\n", pAmostral), file = out)
cat(sprintf("\\newcommand{\\QUATRON}{%d\\xspace}\n", N), file = out)
cat(sprintf("\\newcommand{\\QUATROn}{%d\\xspace}\n", n), file = out)
cat(sprintf("\\newcommand{\\QUATROzy}{%.2f\\xspace}\n", zy), file = out)
cat(sprintf("\\newcommand{\\QUATROAdelta}{%.4f\\xspace}\n", delta), file = out)
cat(sprintf("\\newcommand{\\QUATROAICinf}{%.2f\\xspace}\n", ic[1]), file = out)
cat(sprintf("\\newcommand{\\QUATROAICsup}{%.2f\\xspace}\n", ic[2]), file = out)
cat(sprintf("\\newcommand{\\QUATROBE}{%.4f\\xspace}\n", E1), file = out)
cat(sprintf("\\newcommand{\\QUATROBn}{%.4f\\xspace}\n", nb), file = out)
cat(sprintf("\\newcommand{\\QUATROBnceil}{%d\\xspace}\n", ceil(nb)), file = out)
cat(sprintf("\\newcommand{\\QUATROBnz}{%.4f\\xspace}\n", n0b), file = out)
cat(sprintf("\\newcommand{\\QUATROCnz}{%.4f\\xspace}\n", n0c), file = out)
cat(sprintf("\\newcommand{\\QUATROCn}{%.4f\\xspace}\n", nc), file = out)
cat(sprintf("\\newcommand{\\QUATROCnceil}{%d\\xspace}\n", ceil(nc)), file = out)
close(out)
