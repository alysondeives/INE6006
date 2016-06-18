cpower <- power.t.test(delta       = abs(29-u01a),
                       sd          = s1a,
                       sig.level   = 0.05,
                       power       = 0.9,
                       type        = "one.sample",
                       alternative ="one.sided")

n1c    <- cpower$n

nMin1c <- ceiling(n1c)

cpower <- power.t.test(n           = nMin1c,
                       delta       = abs(29-u01a),
                       sd          = s1a,
                       sig.level   = 0.05,
                       type        = "one.sample",
                       alternative = "one.sided")

beta1c <- cpower$power
