options(digits=4)

sample20 <- read.table("~/Documents/INE6006/lista3/questao2/sample20.txt", quote="\"", header=TRUE)

sample20size <- length(sample20$Renda)
sample20gl   <- sample20size - 1
sample20x    <- mean(sample20$Renda)
sample20s    <- sd(sample20$Renda)

H0 <- 4.5

#==============================================================================
#                            Teste de Hipóteses
#==============================================================================

sample20t <- ((sample20x - H0)*sqrt(sample20size))/sample20s
sample20delta <- (H0 - sample20x)/sample20s

sample20test <- pt(q=sample20t, ncp=sample20delta, df=sample20gl, lower.tail=TRUE)

print(sample20test)

#==============================================================================
#                              Poder do Teste
#==============================================================================

sample20deltas <- c((sample20x-3.5)/sample20s, (sample20x-3.75)/sample20s, (sample20x-3.85)/sample20s, (sample20x-3.95)/sample20s, (sample20x-4.15)/sample20s, (sample20x-4.25)/sample20s, (sample20x-4.35)/sample20s)

sample20powers <- pwr.t.test(n=sample20size, d=sample20deltas, sig.level=0.01, power=NULL, type="one.sample", alternative="less")

print(sample20deltas)
print(sample20powers$power)
