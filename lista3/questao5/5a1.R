incentivos <- subset(amostra, amostra$Pagamento.C == "Incentivos federais")
outras <- subset(amostra, amostra$Pagamento.C == "Outras Formas de Pagamento")
var.test(incentivos$Renda, 
         outras$Renda, 
         alternative = "two.sided", 
         conf.level = 0.99)