cpower <- power.t.test(n=n1a, delta=abs(29-u01a), sd=s1a, sig.level=0.05,
                       type="one.sample", alternative="one.sided")
betaOld1c <- cpower$power
