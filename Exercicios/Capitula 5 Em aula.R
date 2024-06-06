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


#Exemplo2.1


#pop
  #'1% defeito = p = 0.01
  #'x~binorm
#amostra
  #'n = 1000
  #'p*=0.014

#nivel sig = 5 = alpha

#h0 : p=0.01

#h1 : p>0.01

#unilateral Dir


#zobs
#((p* - p) / sqrt(pq / n))
#((0.014 - 0.01) / sqrt(0.01*(1-0.01) / 1000))  = 1.2713


#RC ]z(1-0.05), +inf[
#]qnorm(1-0.05), +inf[
#]1.644854, + inf [
#zobs n€ RC Nao se regeita h0

#ou

#p(z>1.2713 ) = 1-pnorm(1.2713) = 0.101811

#como pvalue > alpha ent nao se rejeita h0


#COM CODIG

amostra2  <-c(rep(1,14),rep(0,1000-14))

BSDA::z.test(
  x=amostra2,
  sigma.x=sqrt(0.01*(1-0.01)), #sd da amostra como é prop é raiz de pq
  alternative ="greater",
  mu=0.01
)


#exemplo 2.2


#ho p >=0.01

#h1 p<0.01

#teste esquerdo
#alpha =0.10



#((0.014 - 0.01) / sqrt(0.01*(1-0.01) / 1000))  = 1.2713

#rc ]-inf , qnorm(0.10)]
#rc ]inf , -1.281552]

#como zbos  nao € na RC logo nao se rejeita o h0


#valor p : p (z<1.2713) = pnorm(1.2713) =0.898189
#como valor p >alpha ent nao se rejeita h0


#exemplo 2.3

#ho p =0.01

#h1 p=/=0.01

#((0.014 - 0.01) / sqrt(0.01*(1-0.01) / 1000))  = 1.2713

#RC∈]-∞,-z_{α\/2}] ∪[z_{α\/2},+∞[ 

#]-∞,-qnorm(1-(0.05/2))] ∪[qnorm(1-(0.05/2)),+∞[ 




#exemplo 3

#ho = mu1-mu2 =0
#h1 mu1-me2 |= 0

#teste bi lateral

amostra31<- c(rep(64,9),rep(72,16),rep(74,2),rep(90,5))
amostra32<- c(rep(70,8),rep(74,22),rep(76,2),rep(90,4))

length(amostra31)
length(amostra32)
mean(amostra31)
mean(amostra32)
sd(amostra31)
sd(amostra32)

#zobs = (((x̅1 - x̅2) - (μ1 - μ2)) / sqrt((σ1^2 / n1) + (σ2^2 / n2))) ~ N(0, 1)
#zobs = ((72.6875 - 75) / sqrt((8.40292^2 / 32) + (5.6669^2 / 36))) =-1.31
´
BSDA::z.test(x=amostra31,y=amostra32,sigma.x = sd(amostra31), sigma.y = sd(amostra32),mu=0,alternative = "two.sided")


