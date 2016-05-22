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
x = mean(pop)
sd = sd(pop)
# Amostras aleatória simples sem reposição de 200 elementos

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

#------------------------------------------------------------------------------------------------
# Ler amostras geradas
amostra_4 <- as.matrix(read.csv(file = "./sample_4.csv", na.strings = c("NA",""," "), header=TRUE))
amostra_8 <- as.matrix(read.csv(file = "./sample_8.csv", na.strings = c("NA",""," "), header=TRUE))
amostra_16 <- as.matrix(read.csv(file = "./sample_16.csv", na.strings = c("NA",""," "), header=TRUE))
amostra_30 <- as.matrix(read.csv(file = "./sample_30.csv", na.strings = c("NA",""," "), header=TRUE))
amostra_100 <- as.matrix(read.csv(file = "./sample_100.csv", na.strings = c("NA",""," "), header=TRUE))

#
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
#[1] 2.299 2.309 2.291 2.319 2.339

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
#[1] 0.64776 0.25212 0.12264 0.07468 0.03138

sd4 = sqrt(var4)
sd8 = sqrt(var8)
sd16 = sqrt(var16)
sd30 = sqrt(var30)
sd100 = sqrt(var100)

print (c(sd4,sd8,sd16,sd30,sd100))
#[1] 0.8048 0.5021 0.3502 0.2733 0.1771

sde4 = sd(pop)/sqrt(4)
sde8 = sd(pop)/sqrt(8)
sde16 = sd(pop)/sqrt(16)
sde30 = sd(pop)/sqrt(30)
sde100 = sd(pop)/sqrt(100)

print (c(sde4,sde8,sde16,sde30,sde100))

#[1] 0.8894 0.6289 0.4447 0.3248 0.1779

# b) Sim, é possivel observar isso.

#----------------------------------------------------------------------------------------------
m4 = c()
m8 = c()
m16 = c()
m30 = c()
m100 = c()
for(i in 1:q){
  m4[i] = mean(amostra_4[,i])
  m8[i] = mean(amostra_8[,i])
  m16[i] = mean(amostra_16[,i])
  m30[i] = mean(amostra_30[,i])
  m100[i] = mean(amostra_100[,i])
}

#Hist 100
setEPS()
postscript(paste("../plots/histogram_renda_m100.eps"), fonts = c("serif", "Palatino"), family = "serif", pointsize = 14)
h<-hist(m100, breaks=12, density=20, col="darkgray", xlab="Renda", ylab="Frequência", main="",freq=TRUE,xpd=TRUE,axes=FALSE,cex.lab=1.5)
xfit<-seq(round(min(m100),digits=1),max(m100),length=200) 
yfit<-dnorm(xfit,mean=mean(m100),sd=sd(m100)) 
yfit <- yfit*diff(h$mids[1:2])*length(m100) 
lines(xfit, yfit, col="black", lwd=2)
axis(2,at=seq(0,1.1*max(h$counts),5))
axis(1,at=h$breaks)
dev.off()

#Hist 30
setEPS()
postscript(paste("../plots/histogram_renda_m30.eps"), fonts = c("serif", "Palatino"), family = "serif", pointsize = 14)
h<-hist(m30, breaks=12, density=20, col="darkgray", xlab="Renda", ylab = "Frequência", main="",freq=TRUE,xpd=TRUE,ylim=c(0,40),axes=FALSE,cex.lab=1.5) 
xfit<-seq(min(m30),max(m30),length=200) 
yfit<-dnorm(xfit,mean=mean(m30),sd=sd(m30)) 
yfit <- yfit*diff(h$mids[1:2])*length(m30) 
lines(xfit, yfit, col="black", lwd=2)
axis(2,at=seq(0,1.2*max(h$counts),5))
axis(1,at=h$breaks)
dev.off()

#Hist 16
setEPS()
postscript("../plots/histogram_renda_m16.eps", fonts = c("serif", "Palatino"), family = "serif", pointsize = 14)
h<-hist(m16, breaks=12, density=20, col="darkgray", xlab="Renda", ylab="Frequência",main="",freq=TRUE,xpd=TRUE,ylim=c(0,50),axes=FALSE,cex.lab=1.5 ) 
xfit<-seq(min(m16),max(m16),length=200) 
yfit<-dnorm(xfit,mean=mean(m16),sd=sd(m16)) 
yfit <- yfit*diff(h$mids[1:2])*length(m16) 
lines(xfit, yfit, col="black", lwd=2)
axis(2,at=seq(0,1.1*max(h$counts),5))
axis(1,at=h$breaks)
dev.off()

#Hist 8
setEPS()
postscript(paste("../plots/histogram_renda_m8.eps"), fonts = c("serif", "Palatino"), family = "serif", pointsize = 14)
h<-hist(m8, breaks=12, density=20, col="darkgray", xlab="Renda", ylab="Frequência", main="",freq=TRUE, xpd=TRUE, ylim=c(0,55),axes=FALSE,cex.lab=1.5) 
xfit<-seq(min(m8),max(m8),length=200) 
yfit<-dnorm(xfit,mean=mean(m8),sd=sd(m8)) 
yfit <- yfit*diff(h$mids[1:2])*length(m8) 
lines(xfit, yfit, col="black", lwd=2)
axis(2,at=seq(0,1.2*max(h$counts),5))
axis(1,at=h$breaks)
dev.off()

#Hist 4
setEPS()
postscript("../plots/histogram_renda_m4.eps", fonts = c("serif", "Palatino"), family = "serif", pointsize = 14)
h<-hist(m4, breaks=12, density=20, col="darkgray", xlab="Renda", ylab="Frequência",main="",freq=TRUE, xpd=TRUE,ylim=c(0,75),axes=FALSE,cex.lab=1.5) 
xfit<-seq(min(m4),max(m4),length=200) 
yfit<-dnorm(xfit,mean=mean(m4),sd=sd(m4)) 
yfit <- yfit*diff(h$mids[1:2])*length(m4) 
lines(xfit, yfit, col="black", lwd=2)
axis(2,at=seq(0,1.1*max(h$counts),5))
axis(1,at=h$breaks)
dev.off()

#----------------------------------------------------------------------------------------------
# Write parameters as latex commands
#Note: open = "w" means open for writing (nothing about truncation is specified)
#      however, it does truncate the file on this line, but avoids truncation on every cat.
out <- file("vars.tex", open = "w")
cat(sprintf("\\newcommand{\\UMx}{\\num{\num{%.3f}}\\xspace}\n", x), file = out)
cat(sprintf("\\newcommand{\\UMsd}{\num{%.3f}\\xspace}\n", sd), file = out)

cat(sprintf("\\newcommand{\\UMxQuatro}{\num{%.3f}\\xspace}\n", x4), file = out)
cat(sprintf("\\newcommand{\\UMxOito}{\num{%.3f}\\xspace}\n", x8), file = out)
cat(sprintf("\\newcommand{\\UMxDezesseis}{\num{%.3f}\\xspace}\n", x16), file = out)
cat(sprintf("\\newcommand{\\UMxTrinta}{\num{%.3f}\\xspace}\n", x30), file = out)
cat(sprintf("\\newcommand{\\UMxCem}{\num{%.3f}\\xspace}\n", x100), file = out)

cat(sprintf("\\newcommand{\\UMsdQuatro}{\\num{%.3f}\\xspace}\n", sd4), file = out)
cat(sprintf("\\newcommand{\\UMsdOito}{\\num{%.3f}\\xspace}\n", sd8), file = out)
cat(sprintf("\\newcommand{\\UMsdDezesseis}{\\num{%.3f}\\xspace}\n", sd16), file = out)
cat(sprintf("\\newcommand{\\UMsdTrinta}{\\num{%.3f}\\xspace}\n", sd30), file = out)
cat(sprintf("\\newcommand{\\UMsdCem}{\\num{%.3f}\\xspace}\n", sd100), file = out)

cat(sprintf("\\newcommand{\\UMsdeQuatro}{\\num{%.3f}\\xspace}\n", sde4), file = out)
cat(sprintf("\\newcommand{\\UMsdeOito}{\\num{%.3f}\\xspace}\n", sde8), file = out)
cat(sprintf("\\newcommand{\\UMsdeDezesseis}{\\num{%.3f}\\xspace}\n", sde16), file = out)
cat(sprintf("\\newcommand{\\UMsdeTrinta}{\\num{%.3f}\\xspace}\n", sde30), file = out)
cat(sprintf("\\newcommand{\\UMsdeCem}{\\num{%.3f}\\xspace}\n", sde100), file = out)

close(out)
#------------------------------------------------------------------------------------------------
#Calcular maximos e minmos para media e stddev dentre as 200 amostras
# min_mean = c(Inf,Inf,Inf,Inf,Inf)
# max_mean = c(0, 0, 0, 0,0)
# min_sd = c(Inf,Inf,Inf,Inf,Inf)
# max_sd = c(0, 0, 0, 0,0)
# 
# for(i in 1:q){
#   #Get minimum mean
#   if(mean(amostra_4[,i]) < min_mean[1]){
#     min_mean[1] = mean(amostra_4[,i])
#   }
#   if(mean(amostra_8[,i]) < min_mean[2]){
#     min_mean[2] = mean(amostra_8[,i])
#   }
#   if(mean(amostra_16[,i]) < min_mean[3]){
#     min_mean[3] = mean(amostra_16[,i])
#   }
#   if(mean(amostra_30[,i]) < min_mean[4]){
#     min_mean[4] = mean(amostra_30[,i])
#   }
#   if(mean(amostra_100[,i]) < min_mean[5]){
#     min_mean[5] = mean(amostra_100[,i])
#   }
#   
#   #Get minimum stddev
#   if(sd(amostra_4[,i]) < min_sd[1]){
#     min_sd[1] = sd(amostra_4[,i])
#   }
#   if(sd(amostra_8[,i]) < min_sd[2]){
#     min_sd[2] = sd(amostra_8[,i])
#   }
#   if(sd(amostra_16[,i]) < min_sd[3]){
#     min_sd[3] = sd(amostra_16[,i])
#   }
#   if(sd(amostra_30[,i]) < min_sd[4]){
#     min_sd[4] = sd(amostra_30[,i])
#   }
#   if(sd(amostra_100[,i]) < min_sd[5]){
#     min_sd[5] = sd(amostra_100[,i])
#   }
#   
#   #Check for maximum mean
#   if(mean(amostra_4[,i]) > max_mean[1]){
#     max_mean[1] = mean(amostra_4[,i])
#   }
#   if(mean(amostra_8[,i]) > max_mean[2]){
#     max_mean[2] = mean(amostra_8[,i])
#   }
#   if(mean(amostra_16[,i]) > max_mean[3]){
#     max_mean[3] = mean(amostra_16[,i])
#   }
#   if(mean(amostra_30[,i]) > max_mean[4]){
#     max_mean[4] = mean(amostra_30[,i])
#   }
#   if(mean(amostra_100[,i]) > max_mean[5]){
#     max_mean[5] = mean(amostra_100[,i])
#   }
#   
#   #Check for maximum sd
#   if(sd(amostra_4[,i]) > max_sd[1]){
#     max_sd[1] = sd(amostra_4[,i])
#   }
#   if(sd(amostra_8[,i]) > max_sd[2]){
#     max_sd[2] = sd(amostra_8[,i])
#   }
#   if(sd(amostra_16[,i]) > max_sd[3]){
#     max_sd[3] = sd(amostra_16[,i])
#   }
#   if(sd(amostra_30[,i]) > max_sd[4]){
#     max_sd[4] = sd(amostra_30[,i])
#   }
#   if(sd(amostra_100[,i]) > max_sd[5]){
#     max_sd[5] = sd(amostra_100[,i])
#   }
# }

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

#---------------------------------------------------------------------------------
#Histogramas toscos
# idx = 1
# data = m4
# min_data = min(data)
# max_data = max(data)  
# k = ceil(5 * log10(length(data)))
# delta = (max_data - min_data) / k
# freqs = c()
# freqs_names = c()
# for(i in 1:k) {
#   lower = min_data + (i-1)*delta
#   upper = min_data + i*delta
#   elements <- which(data>=lower & data<upper)
#   freqs <- c(freqs, length(elements))
#   freqs_names <- c(freqs_names, sprintf("%.2f-%.2f", lower, upper))
# }
# #freqs <- c(freqs, length(data))
# #freqs_names <- c(freqs_names, sprintf("R$ %.2f-%.2f", max(data)))
# names(freqs) <- freqs_names
# 
# #setEPS()
# #postscript(paste("histogram_renda_m4.eps"), fonts = c("serif", "Palatino"), family = "serif", pointsize = 14)
# par(mar=c(12, 4, 4, 2) + 0.1, mgp=c(3, 1, 0))
# barplot(freqs,xpd=TRUE,xlab = "", ylab="Frequência", ylim=c(0, 80), las=3, width=c(rep(1, k), 3))
# mtext("Faixas de Renda", side=1, line=9.5, cex = 2)
# par(mar=c(5, 4, 4, 2) + 0.1, mgp=c(3, 1, 0))
# #dev.off()
# 
# 
# data = m8
# min_data = min(data)
# max_data = max(data)  
# k = ceil(5 * log10(length(data)))
# delta = (max_data - min_data) / k
# freqs = c()
# freqs_names = c()
# for(i in 1:k) {
#   lower = min_data + (i-1)*delta
#   upper = min_data + i*delta
#   elements <- which(data>=lower & data<upper)
#   freqs <- c(freqs, length(elements))
#   freqs_names <- c(freqs_names, sprintf("%.2f-%.2f", lower, upper))
# }
# #freqs <- c(freqs, length(data))
# #freqs_names <- c(freqs_names, sprintf("R$ %.2f-%.2f", max(data)))
# names(freqs) <- freqs_names
# 
# #setEPS()
# #postscript(paste("histogram_renda_m8.eps"), fonts = c("serif", "Palatino"), family = "serif", pointsize = 14)
# par(mar=c(12, 4, 4, 2) + 0.1, mgp=c(3, 1, 0))
# barplot(freqs,xpd=TRUE,xlab = "", ylab="Frequência", ylim=c(0, 80), las=3, width=c(rep(1, k), 3))
# mtext("Faixas de Renda", side=1, line=9.5, cex = 2)
# par(mar=c(5, 4, 4, 2) + 0.1, mgp=c(3, 1, 0))
# #dev.off()
# 
# data = m16
# min_data = min(data)
# max_data = max(data)  
# k = ceil(5 * log10(length(data)))
# delta = (max_data - min_data) / k
# freqs = c()
# freqs_names = c()
# for(i in 1:k) {
#   lower = min_data + (i-1)*delta
#   upper = min_data + i*delta
#   elements <- which(data>=lower & data<upper)
#   freqs <- c(freqs, length(elements))
#   freqs_names <- c(freqs_names, sprintf("%.2f-%.2f", lower, upper))
# }
# #freqs <- c(freqs, length(data))
# #freqs_names <- c(freqs_names, sprintf("R$ %.2f-%.2f", max(data)))
# names(freqs) <- freqs_names
# 
# #setEPS()
# #postscript(paste("histogram_renda_m16.eps"), fonts = c("serif", "Palatino"), family = "serif", pointsize = 14)
# par(mar=c(12, 4, 4, 2) + 0.1, mgp=c(3, 1, 0))
# barplot(freqs,xpd=TRUE,xlab = "", ylab="Frequência", ylim=c(0, 80), las=3, width=c(rep(1, k), 3))
# mtext("Faixas de Renda", side=1, line=9.5, cex = 2)
# par(mar=c(5, 4, 4, 2) + 0.1, mgp=c(3, 1, 0))
# #dev.off()
# 
# data = m30
# min_data = min(data)
# max_data = max(data)  
# k = ceil(5 * log10(length(data)))
# delta = (max_data - min_data) / k
# freqs = c()
# freqs_names = c()
# for(i in 1:k) {
#   lower = min_data + (i-1)*delta
#   upper = min_data + i*delta
#   elements <- which(data>=lower & data<upper)
#   freqs <- c(freqs, length(elements))
#   freqs_names <- c(freqs_names, sprintf("%.2f-%.2f", lower, upper))
# }
# #freqs <- c(freqs, length(data))
# #freqs_names <- c(freqs_names, sprintf("R$ %.2f-%.2f", max(data)))
# names(freqs) <- freqs_names
# 
# #setEPS()
# #postscript(paste("histogram_renda_m30.eps"), fonts = c("serif", "Palatino"), family = "serif", pointsize = 14)
# par(mar=c(12, 4, 4, 2) + 0.1, mgp=c(3, 1, 0))
# barplot(freqs,xpd=TRUE,xlab = "", ylab="Frequência", ylim=c(0, 80), las=3, width=c(rep(1, k), 3))
# mtext("Faixas de Renda", side=1, line=9.5, cex = 2)
# par(mar=c(5, 4, 4, 2) + 0.1, mgp=c(3, 1, 0))
# #dev.off()
# 
# data = m100
# min_data = min(data)
# max_data = max(data)  
# k = ceil(10 * log10(length(data)))
# delta = (max_data - min_data) / k
# freqs = c()
# freqs_names = c()
# for(i in 1:k) {
#   lower = min_data + (i-1)*delta
#   upper = min_data + i*delta
#   elements <- which(data>=lower & data<upper)
#   freqs <- c(freqs, length(elements))
#   freqs_names <- c(freqs_names, sprintf("%.2f-%.2f", lower, upper))
# }
# #freqs <- c(freqs, length(data))
# #freqs_names <- c(freqs_names, sprintf("R$ %.2f-%.2f", max(data)))
# names(freqs) <- freqs_names
# 
# #setEPS()
# #postscript(paste("histogram_renda_m100.eps"), fonts = c("serif", "Palatino"), family = "serif", pointsize = 14)
# par(mar=c(12, 4, 4, 2) + 0.1, mgp=c(3, 1, 0))
# barplot(freqs,xpd=TRUE,xlab = "", ylab="Frequência", ylim=c(0, 80), las=3, width=c(rep(1, k), 3))
# mtext("Faixas de Renda", side=1, line=9.5, cex = 2)
# par(mar=c(5, 4, 4, 2) + 0.1, mgp=c(3, 1, 0))
# #dev.off()