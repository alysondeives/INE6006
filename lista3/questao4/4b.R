probList <- c(0.21, 0.22, 0.24, 0.25, 0.26, 0.27)

effectSize <- ES.h(probList, 0.20)

pwr.p.test(h = effectSize, 
           n = 200, 
           sig.level = 0.01, 
           power = NULL, 
           alternative = "greater")