#libraries
library(tables)
library(xtable)
#----------------------------------------------------------------------------------------------------
options(digits = 4, width = 100) #set precision for float values and and chars by line
par(xpd=TRUE,cex.lab=1.5) #set global options for plot
#----------------------------------------------------------------------------------------------------
#Read from csv
df = read.csv(file = "/home/bruno/PPGCC/Metodos Estatisticos/Repositorio Git/INE6006/processed.csv",na.strings = c("NA",""," "),header=TRUE)

#set columns types
df$Região = as.factor(df$Região)
df$Área = as.factor(df$Área)
df$Pagamento = as.factor(df$Pagamento)

#ordena os fatores na ordem desejada
df$Opinião = factor(df$Opinião,levels = c("Muito insatisfeito","Insatisfeito","Indiferente","Satisfeito","Muito satisfeito"))

df$Renda = as.numeric(df$Renda)
df$Idade = as.integer(df$Idade)

abbrvRegiao <- c("Arati.", "Baepe.", "Itama.", "Jaque.", "Parana.")

#--------------------------------------------------------------------------------------------------
# Questao 4
for(i in levels(df$Área)){
  print (c(i,(length(df[(which(df$Área == i)),"Área"]))/(nrow(df)-length(which(is.na(df$Área))))))
}

# [1] 0.12
# [1] 0.059
# [1] 0.068
# [1] 0.35
# [1] 0.1
# [1] 0.3

#--------------------------------------------------------------------------------------------------
# Questao 5
parts = c()
percent = cat("\\%")
for (i in levels(df$Pagamento)) {
  total = length(df$Pagamento) - length(which(is.na(df$Pagamento)))
  levelCount = length(grep(i, df$Pagamento))
  parts = c(parts, sprintf("%d (%.2f\\%%)", levelCount, levelCount/total * 100))
}
print(cat(paste(parts, collapse=" & ")))

# 257 (5.16\%) & 328 (6.58\%) & 2191 (43.98\%) & 1448 (29.06\%) & 758 (15.21\%)

#--------------------------------------------------------------------------------------------------
#Questao 6

parts = c()
percent = cat("\\%")
for (i in levels(df$Opinião)) {
  total = length(df$Opinião) - length(which(is.na(df$Opinião)))
  levelCount = length(grep(i, df$Opinião))
  parts = c(parts, sprintf("%d (%.2f\\%%)", levelCount, levelCount/total * 100))
}
print(cat(paste(parts, collapse=" & ")))

#1006 (20.20\%) & 749 (15.04\%) & 472 (9.48\%) & 1719 (34.51\%) & 1035 (20.78\%)

#------------------------------------------------------------------------------------------------
#Questao 7 - grafico
  data = na.omit(df$Renda)*880
  data.freq = table(data)
  
  setEPS()
  postscript("plots/histogram_renda_log.eps")
  plot(data.freq,xpd=TRUE,xlab = "Renda (R$)", ylab="Frequência",ylim=c(0,50),log="x")
  dev.off();
  
  setEPS()
  postscript("plots/boxplot_renda_log.eps", fonts = c("serif", "Palatino"))
  boxplot(data, log = "y", family = "serif", font=1, cex.axis = 1.3, ylab = "R$")
  dev.off()
  
  #postscript("histogram2_renda.eps")
  #barplot(data.freq,xpd=TRUE,xlab = "Renda (R$)", ylab="Frequência",ylim=c(0,50))
  #dev.off();
  
  # Histograma
  discr_sup = 4224.0
  
  min_data = min(data)
  max_data = discr_sup  
  k = ceil(5 * log10(length(which(data<discr_sup))))
  delta = (max_data - min_data) / k
  freqs = c()
  freqs_names = c()
  for(i in 1:k) {
    lower = min_data + (i-1)*delta
    upper = min_data + i*delta
    elements <- which(data>=lower & data<upper)
    freqs <- c(freqs, length(elements))
    freqs_names <- c(freqs_names, sprintf("R$ %.2f-%.2f", lower, upper))
  }
  freqs <- c(freqs, length(which(data>discr_sup)))
  freqs_names <- c(freqs_names, sprintf("R$ %.2f-%.2f", discr_sup, max(data)))
  names(freqs) <- freqs_names

  setEPS()
  postscript("plots/histogram_renda.eps", fonts = c("serif", "Palatino"), family = "serif", pointsize = 14)
  par(mar=c(12, 4, 4, 2) + 0.1, mgp=c(3, 1, 0))
  barplot(freqs,xpd=TRUE,xlab = "", ylab="Frequência", ylim=c(0, 800), las=3, width=c(rep(1, k), 3))
  mtext("Faixas Renda", side=1, line=9.5, cex = 2)
  par(mar=c(5, 4, 4, 2) + 0.1, mgp=c(3, 1, 0))
  dev.off()
  
#-----------------------------------------------------------------------------------------------
#Questao 8
  data = na.omit(df$Idade)
  data.freq = table(data)
  
  setEPS()
  postscript("plots/histogram_idade_log.eps")
  plot(data.freq,xpd=TRUE,xlab = "Idade", ylab="Frequência")
  dev.off();
  
  setEPS()
  postscript("plots/boxplot_idade.eps", fonts = c("serif", "Palatino"))
  boxplot(df$Idade, family = "serif", font=1, cex.axis=1.2)
  dev.off();
#-----------------------------------------------------------------------------------------------
#Questao 9
  tb = table(df$Opinião, df$Região)
  orderedOpiniao <- c("Muito insatisfeito", "Insatisfeito", "Indiferente", "Satisfeito", "Muito satisfeito")
  tb <- tb[orderedOpiniao,,drop=FALSE]
  colnames(tb) <- abbrvRegiao
  
  
  setEPS()
  postscript("plots/stacked_opiniao_por_regiao.eps", fonts = c("serif", "Palatino"), family="serif", pointsize = 12, pagecentre=TRUE, width=4.95, height=4.95)
  barplot(tb, xlab = "Região", ylab = "Alunos", beside = FALSE, names.arg = abbrvRegiao, ylim=c(0, 2500))
  legend("topright", orderedOpiniao,  bty="n", fill=gray.colors(5))
  dev.off();
#----------------------------------------------------------------------------------------------
#Questao 10
x = table(df$Área,df$Opinião)
#x <- x[,orderedOpiniao,drop=FALSE]
for (i in 1:nrow(x)){
  x[i,] = x[i,]/sum(x[i,])*100
}

print(x)

# Grafico
x = table(df$Opinião,df$Área)

setEPS()
postscript("plots/stacked_opiniao_por_area.eps", fonts = c("serif", "Palatino"), family="serif", pointsize = 12, pagecentre=TRUE, width=4.95, height=4.95)
par(mar=c(7, 4, 4, 2) + 0.1, mgp=c(3, 1, 0))
barplot(x, xpd=TRUE, xlab = "", ylab = "Alunos", beside = FALSE, legend.text = FALSE, horiz = FALSE, ylim=c(0, 2000), las=2, 
        names.arg = c("Adm.", "Comp. e Mat.", "Educ.", "Eng. e Prod.", "Hum.", "Júr. e Cont."))
legend("topleft", c("Muito insatisfeito", "Insatisfeito", "Indiferente", "Satisfeito", "Muito satisfeito"),  bty="n", fill=gray.colors(5))
mtext("Área", side=2, line = 6.5, cex=1)
par(mar=c(5, 4, 4, 2) + 0.1, mgp=c(3, 1, 0))
dev.off()

#                         Muito insatisfeito Insatisfeito Indiferente Satisfeito Muito satisfeito
# Administração                         0.34         3.57       11.04      28.69            56.37
# Computação e Matemática               0.68         5.08       18.98      30.85            44.41
# Educacional                           0.00         0.59        1.78       7.72            89.91
# Engenharia e Produção                 3.93        14.26       27.89      26.67            27.25
# Humanidades                           0.00         1.59        6.77      16.73            74.90
# Jurídica e Contábil                  26.45        30.17       23.65      13.29             6.45

# Na avaliação antiga  os cursos de Educacional e de Humanidades estavam muito insatisfeitos, atualmente 89.91% dos alunos da área Educacional estão muito satisfeitos e 74.9% dos alunos de Humanidades estão muitos satisfeitos.
# Na avaliação antiga os cursos de Computação e Matématica e de Administração elogiavam seus cursos, atualmente essa característica se mantem. Para o curso de Computação 44.1% estão muito satisfeitos e 30.85% estão satisfeitos e para o curso de Administração 56.37% estão muito satisfeitos e 28.69% estão satisfeitos.
# Na avaliação antiga os demais cursos (Engenharia e Produçao e Jurídica e Contábil) eram indiferentes quanto ao grau de satisfação. Atualmente essa caractéristica foi alterada. No curso de Engenharia a quantidade de pessoas satisfeitas e muito satisfeitas corresponde a um total de 53.82%, enquanto o percentual de alunos indiferentes  é de 27.89%. Já para o curso de Juridica, o percentual de alunos insatisfeitos é alta, correspondendo a um total de 56.62% do total de alunos. Do restante, 23.65% se manifestaram indiferentes e apenas 19.74% se consideram satisfeitos ou muito satisfeitos.

#-------------------------------------------------------------------------------------------------
#Questao 11
# Grafico
x = table(df$Pagamento,df$Região)

setEPS()
postscript("plots/stacked_pagamento_por_regiao.eps", fonts = c("serif", "Palatino"))
barplot(x, xlab = "Região", ylab = "Alunos", beside = FALSE, family = "serif", font=1, cex.axis = 1.1, cex.names = 1.1, cex.lab=1.1)
legend("topright", rownames(x),  bty="n", fill=gray.colors(length(rownames(x))),  text.font=1, cex = 1.0)
dev.off()

#-------------------------------------------------------------------------------------------------
#Questao 12
x = table(df$Pagamento,df$Opinião)
addmargins(x)
#                        Muito insatisfeito Insatisfeito Indiferente Satisfeito Muito satisfeito  Sum
# Auxílio de familiares                   4           47          95         78               33  257
# Bolsas de estudo                       22           67         127         75               37  328
# Financiamento bancário                  3           42         192        497             1448 2182
# Incentivos federais                   431          509         354        118               30 1442
# Recursos próprios                       9           81         238        260              166  754
# Sum                                   469          746        1006       1028             1714 4963

for (i in 1:nrow(x)){
  x[i,] = x[i,]/sum(x[i,])
}
print(x)

#                        Indiferente Insatisfeito Muito insatisfeito Muito satisfeito Satisfeito
# Auxílio de familiares        36.96        18.29               1.56            12.84      30.35
# Bolsas de estudo             38.72        20.43               6.71            11.28      22.87
# Financiamento bancário        8.80         1.92               0.14            66.36      22.78
# Incentivos federais          24.55        35.30              29.89             2.08       8.18
# Recursos próprios            31.56        10.74               1.19            22.02      34.48  

#------------------------------------------------------------------------------------------------
#Questao 13
x = sort(df$Renda)
idx_q1 = (length(x)+1)/4
q1 = mean(x[floor(idx_q1)],x[ceiling(idx_q1)])
q1_idx = which(df$Renda<=q1)

idx_q3 = (3*(length(x)+1))/4
q3 = mean(x[floor(idx_q3)],x[ceiling(idx_q3)])
q3_idx = which(df$Renda>=q3)

na_idx = which(is.na(df$Renda)) #indices dos valores NA
df$FaixaSalarial = NA
df[-c(na_idx),]$FaixaSalarial = "Intermediario"
df[q1_idx,]$FaixaSalarial = "Pobres"
df[q3_idx,]$FaixaSalarial = "Abastados"

y = table(df$FaixaSalarial,df$Opinião)
y <- y[,orderedOpiniao,drop=FALSE]
addmargins(y)
print(y)
for (i in 1:nrow(y)){
  y[i,] = y[i,]/sum(y[i,])*100
}
print(y)

y = table(df$Opinião,df$FaixaSalarial)
for (i in 1:ncol(y)) {
  y[,i] = y[,i]/sum(y[,i])*100
}
setEPS()
postscript("plots/stacked_opiniao_por_faixaRenda.eps", fonts = c("serif", "Palatino"))
par(xpd=TRUE, mar=c(5, 4, 4, 9) + 0.1)
barplot(y, width = 1,  xlab = "Faixa", ylab = "Alunos", beside = FALSE, family = "serif", font=1, cex.axis = 1.1, cex.names = 1.1, cex.lab=1.1)
legend(3.7 , 60, rownames(y),  bty="n", fill=gray.colors(length(rownames(y))),  text.font=1, cex = 1.0)
par(xpd=FALSE, mar=c(5, 4, 4, 2)+0.1)
dev.off()

#               Muito insatisfeito Insatisfeito Indiferente Satisfeito Muito satisfeito
# Abastados                    460          510         222         44               12
# Intermediario                 12          238         774        886              562
# Pobres                         0            0           6        102             1139

#               Muito insatisfeito Insatisfeito Indiferente Satisfeito Muito satisfeito
# Abastados                36.8590      40.8654     17.7885     3.5256           0.9615
# Intermediario             0.4854       9.6278     31.3107    35.8414          22.7346
# Pobres                    0.0000       0.0000      0.4812     8.1796          91.3392

#Considerou-se pobres os alunos que possuem uma renda de até 1.35 salários minimos, alunos intermediarios os que recebem entre 1.35 e 2.73 salarios minimos e como abastados os que recebem acima de 2.73 salarios minimos. Isso foi baseado no quartil inferior e superior dos dados.
#Observa-se que abastados sao mais insatisfeitos, intermediario eh indiferente e satisfeito e pobres sao muito satisfeitos.

#-----------------------------------------------------------------------------------------------
#Questao 14
for(i in levels(df$Opinião)){
  val_mean = mean(df[which(df$Opinião == i),"Idade"],na.rm=TRUE)
  #mean
  print(c(i,val_mean))
  
  #standard deviation
  val_sd = sd[i](df[which(df$Opinião == i),"Idade"],na.rm=TRUE)
  
  #missing values
  print(c(i,length(which(is.na(df[which(df$Opinião == i),"Idade"])))))
  #total values
  print(c(i,length(df[which(df$Opinião == i),"Idade"])))
}

x = table(df$Idade,df$Opinião)
for (i in 1:nrow(x)){
  x[i,] = x[i,]/sum(x[i,])*100
}
print(x)

#29 é o primeiro quartil
df$FaixaEtaria = "Alunos Velhos"
df[which(df$Idade<=29),]$FaixaEtaria = "Alunos Jovens"

x = table(df$FaixaEtaria,df$Opinião)
x <- x[,orderedOpiniao,drop=FALSE]
addmargins(x)
print(x)
for (i in 1:nrow(x)){
  x[i,] = x[i,]/sum(x[i,])*100
}
print(x)

#            Indiferente Insatisfeito Muito insatisfeito Muito satisfeito Satisfeito
# Mais Novo          122           15                  5             1111        278
# Mais Velho         884          734                467              608        757
# 
#            Indiferente Insatisfeito Muito insatisfeito Muito satisfeito Satisfeito
# Mais Novo         7.97         0.98               0.33            72.57      18.16
# Mais Velho       25.62        21.28              13.54            17.62      21.94

x = table(df$Opinião,df$FaixaEtaria)
x <- x[orderedOpiniao,,drop=FALSE]

setEPS()
postscript("plots/stacked_opiniao_por_idade.eps", fonts = c("serif", "Palatino"))
barplot(x, xlab = "Faixa Etária", ylab = "Alunos", beside = FALSE, family = "serif", font=1, cex.axis = 1.1, cex.names = 1.1, cex.lab=1.1)
legend("topleft", rownames(x),  bty="n", fill=gray.colors(length(rownames(x))),  text.font=1, cex = 1.0)
dev.off()

# ------------------------------------------------------------------------------------------------
# Questao 15
# q15 = tabular(df$Área*df$Região~df$Opinião)
# write.csv.tabular(q15,"q15.csv")

q15a = ftable(df$Área,df$Região,df$Opinião,row.vars=c(1,2),dnn=c("Área","Região","Opinião"))
q15l = xtableFtable(q15a, method="compact")
write(print.xtableFtable(q15l,booktabs=TRUE),file="tab-q15.tex")
print.xtableFtable(q15l,booktabs=TRUE)

q15p = ftable(df$Área,df$Região,df$Opinião,row.vars=c(1,2),dnn=c("Área","Região","Opinião"))
for (i in 1:nrow(q15p)) {
  if (sum(q15p[i,]) == 0) {
    q15p[i,] = 0
  } else {
    q15p[i,] = q15p[i,]/sum(q15p[i,])*100
  }
}
print(q15p)
q15l = xtableFtable(q15p, method="compact", caption = "Opinião por Área e por Região", label = "tabela:q15", caption.placement = "top")
write(print.xtableFtable(q15l,booktabs=TRUE, caption.placement = "top", display = c("%.2f\\%", "%.2f\\%", "%.2f\\%", "%.2f\\%")),file="tables/tab-q15p.tex")
print.xtableFtable(q15l,booktabs=TRUE, display = c("f", "f", "f", "f", "f"))



# q15b = ftable(df$Área,df$Região,df$Opinião,row.vars=c(2,1),dnn=c("Área","Região","Opinião"))
# q15bl = xtableFtable(q15b, method="compact")
# write(print.xtableFtable(q15bl,booktabs=TRUE),file="tab-q15b.tex")
# print.xtableFtable(q15l,booktabs=TRUE)
#-------------------------------------------------------------------------------------------------
x = sort(df$Renda)
idx_q1 = (length(x)+1)/4
q1 = mean(x[floor(idx_q1)],x[ceiling(idx_q1)])
q1_idx = which(df$Renda<=q1)

idx_q3 = (3*(length(x)+1))/4
q3 = mean(x[floor(idx_q3)],x[ceiling(idx_q3)])
q3_idx = which(df$Renda>=q3)

na_idx = which(is.na(df$Renda)) #indices dos valores NA
df$FaixaSalarial = NA
df[-c(na_idx),]$FaixaSalarial = "Intermediario"
df[q1_idx,]$FaixaSalarial = "Pobres"
df[q3_idx,]$FaixaSalarial = "Abastados"

q16 = ftable(df$FaixaSalarial,df$Região,df$Opinião,row.vars=c(1,2),dnn=c("Classe","Região","Opinião"))
addmargins(q16)

q16l = xtableFtable(q16, method="compact")
write(print.xtableFtable(q16l,booktabs=TRUE),file="tab-q16.tex")
print.xtableFtable(q15l,booktabs=TRUE)
#------------------------------------------------------------------------------------------------
#Questao 17
q17a = df[which(df$Região=="Aratibutantã" & df$Idade > 28 & df$Pagamento == "Financiamento bancário"),c("Região","Idade","Pagamento","Opinião")]
summary(q17a)

#            Região        Idade                     Pagamento                 Opinião  
# Aratibutantã :136   Min.   :29   Auxílio de familiares :  0   Indiferente       :57  
# Baependinha  :  0   1st Qu.:30   Bolsas de estudo      :  0   Insatisfeito      :21  
# Itamaracanã  :  0   Median :32   Financiamento bancário:136   Muito insatisfeito: 0  
# Jaquereçaba  :  0   Mean   :33   Incentivos federais   :  0   Muito satisfeito  :11  
# Paranapitanga:  0   3rd Qu.:34   Recursos próprios     :  0   Satisfeito        :47  
#                     Max.   :66
#-------------------------------------------------------------------------------------------------
#Questao 20
x = sort(df$Renda)
idx_q1 = (length(x)+1)/4
q1 = mean(x[floor(idx_q1)],x[ceiling(idx_q1)])
q1_idx = which(df$Renda<=q1)

idx_q3 = (3*(length(x)+1))/4
q3 = mean(x[floor(idx_q3)],x[ceiling(idx_q3)])
q3_idx = which(df$Renda>=q3)

na_idx = which(is.na(df$Renda)) #indices dos valores NA
df$FaixaSalarial = NA
df[-c(na_idx),]$FaixaSalarial = "Intermediario"
df[q1_idx,]$FaixaSalarial = "Pobres"
df[q3_idx,]$FaixaSalarial = "Abastados"

#inverte area com regiao
q20 = ftable(df$Área,df$FaixaSalarial,df$Opinião,row.vars=c(1,2),dnn=c("Área","Classe","Opinião"))
addmargins(q20)

q20l = xtableFtable(q20, method="compact")
write(print.xtableFtable(q20l,booktabs=TRUE),file="tables/tab-q20.tex")
print.xtableFtable(q20l,booktabs=TRUE)

#percentual
for (i in 1:nrow(q20)) {
  if (sum(q20[i,]) == 0) {
    q20[i,] = 0
  } else {
    q20[i,] = q20[i,]/sum(q20[i,])*100
  }
}
print(q20)

q20pl = xtableFtable(q20, method="compact")
write(print.xtableFtable(q20pl,booktabs=TRUE),file="tables/tab-q20p.tex")
print.xtableFtable(q20pl,booktabs=TRUE)
