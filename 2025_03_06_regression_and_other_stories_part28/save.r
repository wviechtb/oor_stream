############################################################################

n <- 500

y <- rnorm(n, mean=5, sd=1)
tmp1 <- lm(y ~ 1)
tmp2 <- lm(log(y) ~ 1)
logLik(tmp1)
logLik(tmp2)
logLik(tmp2) - sum(log(y))

sum(dnorm(y, mean=coef(tmp1), sd=sigma(tmp1), log=TRUE))
sum(dnorm(log(y), mean=coef(tmp2), sd=sigma(tmp2), log=TRUE))
sum(dnorm(log(y), mean=coef(tmp2), sd=sigma(tmp2), log=TRUE)) - sum(log(y))

library(VGAM)
tmp3 <- vglm(y ~ 1, family=lognormal)
logLik(tmp3)

y <- rlnorm(n, meanlog=0, sdlog=1)
tmp1 <- lm(y ~ 1)
tmp2 <- lm(log(y) ~ 1)
logLik(tmp1)
logLik(tmp2)
logLik(tmp2) - sum(log(y))

tmp3 <- vglm(y ~ 1, family=lognormal)
logLik(tmp3)

############################################################################
