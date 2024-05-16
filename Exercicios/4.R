##### 4.1.1
alpha <- 1-0.99
z1<- 1-(alpha/2)
z1n<-qnorm(z1)


'[' <- 78.3-(z1n*0.4)

']' <- 78.3+(z1n*0.4)

###### 4.1.2

Ampliture <- (78.3+(z1n*0.4)) -  (78.3-(z1n*0.4))

###### 4.1.3
Margem_de_erro<- Ampliture /2


###### 4.1.4

##BRUTE FORCE

b<-25 #n inicial
c<-0.1 # Valor desejado da amplitude do  intervalo de conf


while (TRUE)
{
  #Mudar baseado ao intervalo de conf dado/escolhido
  Ampliture2 <- (78.3+(z1n*(2/sqrt(b)))) -  (78.3-(z1n*(2/sqrt(b))))
  
  if (Ampliture2  < c ) {
    break
  }
  
  b <- b + 1
}




#######4.1.5


alpha <- 1-0.95
z1<- 1-(alpha/2)

z1n<-qnorm(z1)


'[' <- 78.3-(z1n*0.4)

']' <- 78.3+(z1n*0.4)

Ampliture <- (78.3+(z1n*0.4)) -  (78.3-(z1n*0.4))


#######4.1.6

#diminuir o grau conf (mantendo o n ) diminui a amplitude 
#'
#'
#'





#################
####4.2

amostra <- c(983, 992, 1011, 976, 997, 1000, 1004, 983, 998)



#####4.2.1

meanamostra <- mean(amostra)

#####4.2.2

BSDA::z.test(x=amostra,sigma.x=12,conf.level = 0.90)

BSDA::z.test(x=amostra,sigma.x=12,conf.level = 0.95)

BSDA::z.test(x=amostra,sigma.x=12,conf.level = 0.99)


###### 4.2.4


b<-9 #n inicial
c<-1 # Valor desejado da amplitude do  intervalo de conf
alpha <- 1-0.95
z1<- 1-(alpha/2)
z1n<-qnorm(z1)

while (TRUE)
{
  #Mudar baseado ao intervalo de conf dado/escolhido
  margem_erro <- ((meanamostra+(z1n*(12/sqrt(b)))) -  (meanamostra-(z1n*(12/sqrt(b))))) /2
  
  if (margem_erro  < c ) {
    break
  }
  
  b <- b + 1
}
