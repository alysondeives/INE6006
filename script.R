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
  
  #setEPS()
  #postscript("plots/histogram_idade_log.eps")
  plot(data.freq,xpd=TRUE,xlab = "Idade", ylab="Frequência")
  #dev.off();


