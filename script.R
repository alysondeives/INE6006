library(tables)
library(xtable)
options(digits = 2, width = 100)
#df = read.csv(file = "TYU06.csv",na.strings = c("NA",""," "),header=TRUE)

df = read.csv(file = "processed.csv",na.strings = c("NA",""," "),header=TRUE)

#set columns types
df$Região = as.factor(df$Região)
df$Área = as.factor(df$Área)
df$Pagamento = as.factor(df$Pagamento)
df$Opinião = as.factor(df$Opinião)
df$Renda = as.numeric(df$Renda)
df$Idade = as.integer(df$Idade)

colnames = colnames(df) #get name of columns
#summary of data
summary(df)

# Região                        Área                       Pagamento   
# Baependinha  :2238   Engenharia e Produção:1695   Financiamento bancário:2131  
# Aratibutantã :1150   Jurídica e Contábil  :1481   Incentivos federais   :1408  
# Itamaracanã  : 822   Administração        : 579   Recursos próprios     : 743  
# Jaquereçaba  : 513   Humanidades          : 488   Bolsas de estudo      : 319  
# Paranapitanga: 121   Educacional          : 332   Auxílio de familiares : 249  
# (Other)      : 135   (Other)              : 404   (Other)               : 132  
# NA's         :  21   NA's                 :  21   NA's                  :  18  
#                Opinião         Renda        Idade   
#  Muito satisfeito  :1669   Min.   : 1   Min.   :18  
#  Satisfeito        :1017   1st Qu.: 1   1st Qu.:29  
#  Indiferente       : 976   Median : 2   Median :32  
#  Insatisfeito      : 732   Mean   : 2   Mean   :32  
#  Muito insatisfeito: 464   3rd Qu.: 3   3rd Qu.:35  
#  (Other)           : 123   Max.   :61   Max.   :70  
#  NA's              :  19   NA's   :14   NA's   :13
#------------------------------------------------------------------------------------------------

#get missing data
for (i in 1:ncol(df)){
  na_val <- length(which(is.na(df[,i])))
  na_percent <- (na_val/nrow(df))*100
  print(c(colnames[i],na_val,na_percent))
}

# [1] "Região" "21"     "0.42"  
# [1] "Área" "21"   "0.42"
# [1] "Pagamento" "18"        "0.36"     
# [1] "Opinião" "19"      "0.38"   
# [1] "Renda" "14"    "0.28" 
# [1] "Idade" "13"    "0.26"

#-------------------------------------------------------------------------------------------------------

#get errors in variables values
for (i in 1:ncol(df)){
  print(levels(df[,i]))
}

#-------------------------------------------------------------------------------------------------------

# [1] "Arati "        "Aratib "       "Aratibu "      "Aratibut "     "Aratibutantã"  "Baepe "       
# [7] "Baepen "       "Baepend "      "Baependi "     "Baependinha"   "Itama "        "Itamar "      
# [13] "Itamara "      "Itamarac "     "Itamaracanã"   "Jaque "        "Jaquer "       "Jaquere "
# [19] "Jaquereç "     "Jaquereçaba"   "Paranapitanga"

df[grep("Arat",df$Região),1] = "Aratibutantã"
df[grep("Baep",df$Região),1] = "Baependinha"
df[grep("Itama",df$Região),1] = "Itamaracanã"
df[grep("Jaqu",df$Região),1] = "Jaquereçaba"
df[grep("Para",df$Região),1] = "Paranapitanga"
df$Região = factor(df$Região)

summary(df$Região)
# Aratibutantã   Baependinha   Itamaracanã   Jaquereçaba Paranapitanga          NA's 
#         1185          2294           843           536           121            21 

#--------------------------------------------------------------------------------------------------------

# [1] "Admin "                  "Admini "                 "Adminis "               
# [4] "Administ "               "Administração"           "Compu "                 
# [7] "Comput "                 "Computa "                "Computaç "              
# [10] "Computação e Matemática" "Educa "                  "Educac "                
# [13] "Educaci "                "Educacio "               "Educacional"            
# [16] "Engen "                  "Engenh "                 "Engenha "               
# [19] "Engenhar "               "Engenharia e Produção"   "Human "                 
# [22] "Humani "                 "Humanid "                "Humanida "              
# [25] "Humanidades"             "Juríd "                  "Jurídi "                
# [28] "Jurídic "                "Jurídica "               "Jurídica e Contábil"  

df[grep("Admin",df$Área),2] = "Administração"
df[grep("Com",df$Área),2] = "Computação e Matemática"
df[grep("Edu",df$Área),2] = "Educacional"
df[grep("Engen",df$Área),2] = "Engenharia e Produção"
df[grep("Human",df$Área),2] = "Humanidades"
df[grep("Jur",df$Área),2] = "Jurídica e Contábil"
df$Área = factor(df$Área)

summary(df$Área)
# Administração           Computação e Matemática Educacional            Engenharia e Produção 
# 592                     296                     338                    1741 
#             Humanidades     Jurídica e Contábil                    NA's 
#                     503                    1509                      21 

#-------------------------------------------------------------------------------------------------

# [1] "Auxíl "                 "Auxíli "                "Auxílio "              
# [4] "Auxílio  "              "Auxílio de familiares"  "Bolsa "                
# [7] "Bolsas "                "Bolsas d "              "Bolsas de estudo"      
# [10] "Finan "                 "Financ "                "Financi "              
# [13] "Financia "              "Financiamento bancário" "Incen "                
# [16] "Incent "                "Incenti "               "Incentiv "             
# [19] "Incentivos federais"    "Recur "                 "Recurs "               
# [22] "Recurso "               "Recursos "              "Recursos próprios"     

df[grep("Aux",df$Pagamento),3] = "Auxílio de familiares"
df[grep("Bol",df$Pagamento),3] = "Bolsas de estudo"
df[grep("Fin",df$Pagamento),3] = "Financiamento bancário"
df[grep("Inc",df$Pagamento),3] = "Incentivos federais"
df[grep("Rec",df$Pagamento),3] = "Recursos próprios"
df$Pagamento = factor(df$Pagamento)

summary(df$Pagamento)
# Auxílio de familiares       Bolsas de estudo Financiamento bancário    Incentivos federais 
# 257                    328                   2191                   1448 
# Recursos próprios                   NA's 
#                    758                     18 

#--------------------------------------------------------------------------------------------------

# [1] "Indifer "           "Indifere "          "Indiferente"        "Insatis "          
# [5] "Insatisf "          "Insatisfeito"       "Muito i "           "Muito in "         
# [9] "Muito insatisfeito" "Muito s "           "Muito sa "          "Muito satisfeito"  
# [13] "Satisfe "           "Satisfei "          "Satisfeito"        

df[grep("Muito s",df$Opinião),4] = "Muito satisfeito"
df[grep("Sat",df$Opinião),4] = "Satisfeito"
df[grep("Ind",df$Opinião),4] = "Indiferente"
df[grep("Ins",df$Opinião),4] = "Insatisfeito"
df[grep("Muito i",df$Opinião),4] = "Muito insatisfeito"
df$Opinião = factor(df$Opinião)
summary(df$Opinião)

# Indiferente       Insatisfeito Muito insatisfeito   Muito satisfeito         Satisfeito 
# 1006                749                472               1719               1035 
# NA's 
#                 19 
#--------------------------------------------------------------------------------------------------

# print summary of processed data frame
summary(df)

#Save processed input file withouth errors
write.csv(df,file='processed.csv',row.names=F,quote=F)

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
  par(xpd=TRUE,cex.lab=1.5)
  
  data = na.omit(df$Renda)*880
  data.freq = table(data)
  
  setEPS()
  postscript("plots/histogram_renda_log.eps")
  plot(data.freq,xpd=TRUE,xlab = "Renda (R$)", ylab="Frequência",ylim=c(0,50),log="x")
  dev.off();
  
  #postscript("histogram2_renda.eps")
  #barplot(data.freq,xpd=TRUE,xlab = "Renda (R$)", ylab="Frequência",ylim=c(0,50))
  #dev.off();
  
#-----------------------------------------------------------------------------------------------
#Questao 8
  data = na.omit(df$Idade)
  data.freq = table(data)
  
  setEPS()
  postscript("plots/histogram_idade_log.eps")
  plot(data.freq,xpd=TRUE,xlab = "Idade", ylab="Frequência")
  dev.off();
#----------------------------------------------------------------------------------------------
#Questao 10
x = table(df$Área,df$Opinião)
for (i in 1:nrow(x)){
  x[i,] = x[i,]/sum(x[i,])*100
}

print(x)

#                          Indiferente Insatisfeito Muito insatisfeito Muito satisfeito Satisfeito
#  Administração                 11.04         3.57               0.34            56.37      28.69
#  Computação e Matemática       18.98         5.08               0.68            44.41      30.85
#  Educacional                    1.78         0.59               0.00            89.91       7.72
#  Engenharia e Produção         27.89        14.26               3.93            27.25      26.67
#  Humanidades                    6.77         1.59               0.00            74.90      16.73
#  Jurídica e Contábil           23.65        30.17              26.45             6.45      13.29

# Na avaliação antiga  os cursos de Educacional e de Humanidades estavam muito insatisfeitos, atualmente 89.91% dos alunos da área Educacional estão muito satisfeitos e 74.9% dos alunos de Humanidades estão muitos satisfeitos.
# Na avaliação antiga os cursos de Computação e Matématica e de Administração elogiavam seus cursos, atualmente essa característica se mantem. Para o curso de Computação 44.1% estão muito satisfeitos e 30.85% estão satisfeitos e para o curso de Administração 56.37% estão muito satisfeitos e 28.69% estão satisfeitos.
# Na avaliação antiga os demais cursos (Engenharia e Produçao e Jurídica e Contábil) eram indiferentes quanto ao grau de satisfação. Atualmente essa caractéristica foi alterada. No curso de Engenharia a quantidade de pessoas satisfeitas e muito satisfeitas corresponde a um total de 53.82%, enquanto o percentual de alunos indiferentes  é de 27.89%. Já para o curso de Juridica, o percentual de alunos insatisfeitos é alta, correspondendo a um total de 56.62% do total de alunos. Do restante, 23.65% se manifestaram indiferentes e apenas 19.74% se consideram satisfeitos ou muito satisfeitos.


#-------------------------------------------------------------------------------------------------
#Questao 12
x = table(df$Pagamento,df$Opinião)

#                        Indiferente Insatisfeito Muito insatisfeito Muito satisfeito Satisfeito
# Auxílio de familiares           95           47                  4               33         78
# Bolsas de estudo               127           67                 22               37         75
# Financiamento bancário         192           42                  3             1448        497
# Incentivos federais            354          509                431               30        118
# Recursos próprios              238           81                  9              166        260

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
q1_idx = which(x<=q1)

idx_q3 = (3*(length(x)+1))/4
q3 = mean(x[floor(idx_q3)],x[ceiling(idx_q3)])
q3_idx = which(x>=q3)

ordered_df = df[order(df$Renda),]
ordered_df$Faixa_salarial = "Intermediario"
ordered_df[q1_idx,]$Faixa_salarial = "Pobres"
ordered_df[q3_idx,]$Faixa_salarial = "Abastados"

x = table(ordered_df$Faixa_salarial,df$Opinião)
print(x)
for (i in 1:nrow(x)){
  x[i,] = x[i,]/sum(x[i,])*100
}
print(x)

#Valores
#               Indiferente Insatisfeito Muito insatisfeito Muito satisfeito Satisfeito
# Abastados             245          202                108              417        276
# Intermediario         505          369                234              874        499
# Pobres                256          178                130              428        260

#Percentuais
#               Indiferente Insatisfeito Muito insatisfeito Muito satisfeito Satisfeito
# Abastados            19.6         16.2                8.7             33.4       22.1
# Intermediario        20.4         14.9                9.4             35.2       20.1
# Pobres               20.4         14.2               10.4             34.2       20.8

#Considerou-se pobres os alunos que possuem uma renda de até 1.35 salários minimos, alunos intermediarios os que recebem entre 1.35 e 2.73 salarios minimos e como abastados os que recebem acima de 2.73 salarios minimos. Isso foi baseado no quartil inferior e superior dos dados.
#Não se observa relação entre o perfil economico dos alunos e seu grau de satisfação com a TYU, visto que a distribuição é similar para qualquer classe.

#-----------------------------------------------------------------------------------------------
#Questao 14
x_mean = c()


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
df$FaixaEtaria = "Mais Velho"
df[which(df$Idade<=29),]$FaixaEtaria = "Mais Novo"

x = table(df$FaixaEtaria,df$Opinião)
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
# ------------------------------------------------------------------------------------------------
# Questao 15
q15 = tabular(df$Área*df$Região~df$Opinião)
write.csv.tabular(q15,"q15.csv")

q15b = ftable(df$Área,df$Região,df$Opinião,row.vars=c(1,2),dnn=c("Área","Região","Opinião"))
q15l = xtableFtable(q15b, method="compact")
write(print.xtableFtable(q15l,booktabs=TRUE),file="tab-q15.tex")
print.xtableFtable(q15l,booktabs=TRUE)
#-------------------------------------------------------------------------------------------------
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

