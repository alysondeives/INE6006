propsList3b <- c(0.30, 0.32, 0.33, 0.34)

tmpH <- ES.h(propsList3b, 0.35)

pwr.p.test( h = tmpH, 
            n = 200, 
            sig.level = 0.05, 
            power = NULL, 
            alternative = "less" )
