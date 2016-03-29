options(digits = 2, width = 100)
df = read.csv(file = "TYU06.csv",na.strings = c("NA",""," "),header=TRUE)

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

#get errors in variables values
for (i in 1:ncol(df)){
  print(levels(df[,i]))
}

# [1] "Arati "        "Aratib "       "Aratibu "      "Aratibut "     "Aratibutantã"  "Baepe "       
# [7] "Baepen "       "Baepend "      "Baependi "     "Baependinha"   "Itama "        "Itamar "      
# [13] "Itamara "      "Itamarac "     "Itamaracanã"   "Jaque "        "Jaquer "       "Jaquere "
# [19] "Jaquereç "     "Jaquereçaba"   "Paranapitanga"

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

# [1] "Auxíl "                 "Auxíli "                "Auxílio "              
# [4] "Auxílio  "              "Auxílio de familiares"  "Bolsa "                
# [7] "Bolsas "                "Bolsas d "              "Bolsas de estudo"      
# [10] "Finan "                 "Financ "                "Financi "              
# [13] "Financia "              "Financiamento bancário" "Incen "                
# [16] "Incent "                "Incenti "               "Incentiv "             
# [19] "Incentivos federais"    "Recur "                 "Recurs "               
# [22] "Recurso "               "Recursos "              "Recursos próprios"     

# [1] "Indifer "           "Indifere "          "Indiferente"        "Insatis "          
# [5] "Insatisf "          "Insatisfeito"       "Muito i "           "Muito in "         
# [9] "Muito insatisfeito" "Muito s "           "Muito sa "          "Muito satisfeito"  
# [13] "Satisfe "           "Satisfei "          "Satisfeito"        
# NULL
# NULL







