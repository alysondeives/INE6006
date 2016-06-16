t.test(incentivos$Renda, 
       outras$Renda, 
       alternative = "two.sided", 
       conf.level = 0.95, 
       var.equal = FALSE)