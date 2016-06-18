alpha1a <- 0.05
u01a <- 27
n1a <- length(df$Idade)
gl1a <- n1a - 1
barx1a <- mean(df$Idade)
s1a <- sqrt(var(df$Idade))
t1a <- ((barx1a - u01a) * sqrt(n1a)) / s1a
p1a <- pt(t1a, gl1a, lower.tail = FALSE)
