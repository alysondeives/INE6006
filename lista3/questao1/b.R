us1b     <- c(28, 30, 31, 32, 33, 34, 35)
deltas1b <- us1b-u01a
bpower   <- power.t.test(n           = n1a,
                         delta       = deltas1b, 
                         sd          = s1a,
                         sig.level   = alpha1a, 
                         type        = "one.sample",
                         alternative = "one.sided")
