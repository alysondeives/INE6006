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

sample20deltas <- c((H0-3.5)/sample20s, (H0-3.75)/sample20s, (H0-3.85)/sample20s, (H0-3.95)/sample20s, (H0-4.15)/sample20s, (H0-4.25)/sample20s, (H0-4.35)/sample20s)

sample20powers <- power.t.test(n=sample20size, d=sample20deltas, sd=sample20s, sig.level=0.01, power=NULL, type="one.sample", alternative="one.sided", strict=FALSE)

print(sample20deltas)
print(sample20powers$power)

#==============================================================================
#                        Análise do Poder do Teste
#==============================================================================

medias <- c(seq(3.50, 4.35, by=0.05))
deltas <- c((H0 - medias)/sample20s)

sample40powers <- power.t.test(n=40, d=deltas, sd=sample20s, sig.level=0.01, power=NULL, type="one.sample", alternative="one.sided", strict=FALSE)
sample50powers <- power.t.test(n=50, d=deltas, sd=sample20s, sig.level=0.01, power=NULL, type="one.sample", alternative="one.sided", strict=FALSE)
sample60powers <- power.t.test(n=60, d=deltas, sd=sample20s, sig.level=0.01, power=NULL, type="one.sample", alternative="one.sided", strict=FALSE)
sample70powers <- power.t.test(n=70, d=deltas, sd=sample20s, sig.level=0.01, power=NULL, type="one.sample", alternative="one.sided", strict=FALSE)

print(sample40powers$power)
print(sample50powers$power)
print(sample60powers$power)
print(sample70powers$power)

powers <- matrix(c(medias, sample40powers$power, sample50powers$power, sample60powers$power, sample70powers$power), nrow=length(deltas), ncol=5)
colnames(powers) <- c("media", "40", "50", "60", "70")
print(powers)

write.csv(powers, file = "powers.csv", row.names = FALSE, 
          na="", fileEncoding = "UTF-8", eol="\r\n")


