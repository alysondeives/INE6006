tempH <- ES.h(0.31, 0.35)
pwr.p.test( h = tempH, 
            n = NULL, 
            sig.level = 0.05, 
            power = 0.99, 
            alternative = "less" )
