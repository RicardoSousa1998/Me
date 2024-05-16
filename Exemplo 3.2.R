amostra <-c(8.18,8.16,8.17,8.22,8.19)
mean(amostra)


n3 <- length(amostra)


mean(amostra)-qnorm(0.95) * (0.08/sqrt(n3))


BSDA::z.test(x=amostra,sigma.x=0.08,conf.level = 0.90)