#Capitulo 5 

#exemplo 1

#Populaçao
#'normal
#'media 60
#'desvio padrao 20

amostra <- c(25.8, 76.0, 59.6, 61.4, 51.3, 66.2, 30.4, 37.5, 57.2)

#h0 =60

#h1 < 60

#zobs = ((x̅ - μ) / (σ / sqrt(n))) <=> ((mean(amostra) - 60) / (20 / sqrt(9))) =-1.243333

#RC ]- INF ,za] = ]-inf ,qnorm(0.05)] =] - inf ,  -1.644854] 
#como -1.243333 nao € na RC ent nao se rejeita H0

#P-value P(z<Zob) = p(z<-1.243333) = pnorm(-1.243333) = 0.1068726




BSDA::z.test(
  x = amostra,              # Vetor com a amostra
  sigma.x = 20,             # Desvio Padrão da População  
  alternative="less",       # Teste unilateral esquerdo
  mu=60,                    # Valor do H0
)


