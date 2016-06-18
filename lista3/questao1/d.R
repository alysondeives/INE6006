df1d = read.csv(file = "../../lista2/processed.csv",na.strings = c("NA",""," "),header=TRUE)
pop1d <- na.omit(df1d$Idade)
n1d <- nMin1c
sample1d <- sample(pop1d, size=nMin1c, replace = FALSE)
write.csv(sample1d, file = "sample.csv", row.names = FALSE, 
          na="", fileEncoding = "UTF-8", eol="\r\n")
s1d <- sqrt(var(sample1d))
