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


######4 .3

#3 A distancia percorrida por um aviao, desde o contacto com
#o solo ate `a imobilizacao total, e uma variavel aleatoria X com distribuicao normal. Os valores para X, numa serie de 31 aterragens, foram compilados e sao
#apresentados de seguida (valores em milhares de metros):
  
#sum(xi)=54.3    sim(xi^2)=95.57

# Pop
  # X ~ N(μ, σ)
# Amostra
  # n = 31
  # sum(xi) = 54.3
  # sum(xi^2) = 95.57


#1  Calcule estimativas pontuais para a media e variancia da populacao.

#x- = (1/n) * sum(xi) <=> (1/31) * 54.3 <=> 1.7516 
#s^2 =  (1/n) * (sum(xi^2) - n * x-^2) <=> (1/31)*(95.57 - 31 *1.7516^2 ) <=>0.0148


#2 

#Determine um intervalo de confianca a 99% para a media. Acha que e possıvel
#efetuar uma aterragem segura numa pista com menos de 1500 metros? Justifique.


# 99 conf
# x- = 1.7516
# α = 1- conf = 1-0.99 =  0.01
# S = sqrt(S^2) = sqrt(0.0148) = 0.1217

# I.C.: ] x̅ - (t_(1 - (α/2)); n-1) * (s / sqrt(n)) , x̅ + (t_(1 - (α/2)); n-1) * (s / sqrt(n)) [
#I.C.:] 1.7516 - (t_(0.995)); 30) * (0.1217 / sqrt(31)) , 1.7516 + (t_(0.995)); 30) * (0.1217 / sqrt(31)) [
#I.C.:]1.7516 - 2.750 * 0.0219 ,1.7516 + 2.750 * 0.0219[
#I.C.:]1.691375 ,1.811825 [


#Com 99% de confiança, nao parece ser possivel efetuar uma aterragem segura.


#####4.4
#Com a finalidade de estimar o peso medio (em quilos) das
#criancas de 15 anos de idade em determinada regiao geografica, selecionaram-se
#aleatoriamente 10 criancas que forneceram os seguintes dados:
#(33.1, 32.1, 40.9, 37.1, 37.7, 35.1, 30.2, 45.6, 27.8, 37.3)
#Admita a normalidade.


#1 Calcule estimativas para a m´edia e o desvio padr˜ao do peso das crian¸cas.

#pop
  #'normal
#Amostra
  #n=10
  

Amostra_4.4 <- c(33.1, 32.1, 40.9, 37.1, 37.7, 35.1, 30.2, 45.6, 27.8, 37.3)

Media <- mean(Amostra_4.4)
dP <-sd(Amostra_4.4)

#2
#Determine um intervalo de confianca a 99% para o peso medio de todas as
#criancas.

#conf 99
#como temos a amostra podemos usar
t.test(
  x = Amostra_4.4,   # amostra
  mu = Media,        # Media
  conf.level = 0.99  # Confiança 
) #]30.31374 41.06626[

#sem a amostra usavamos 
# I.C.: ] x̅ - (t_(1 - (α/2)); n-1) * (s / sqrt(n)) , x̅ + (t_(1 - (α/2)); n-1) * (s / sqrt(n)) [



#3 Considerando que a estimativa para o peso medio nao e suficientemente
#precisa, dado que o intervalo de confianca e demasiado amplo, o que sugere
#fazer para diminuir a amplitude do intervalo de confianca?

#roubei ao capelas

# Para diminuir a amplitude do intervalo, podemos fazer um dos seguintes pontos:
# -- Diminuir o grau de confiança (mantendo o nº de elementos da amostra);
# -- Aumentar o nº de elementos da amostra (mantendo o grau de confiança).





#4
#Considerando que a estimativa para o peso medio nao e suficientemente
#precisa (dado que o intervalo de confianca e demasiado grande), qual deve
#ser a dimensao da amostra a recolher de modo a obter uma amplitude de 3
#quilos com um grau de confianca de 99%?




b<-11
c<-3 


while (TRUE)
{
  Ampliture2 <-  (35.69 + qt(0.995,b-1) * (5.2314 / sqrt(b))) - (35.69 - qt(0.995,b-1) * (5.2314 / sqrt(b)))
  
  if (Ampliture2  <= c ) {
    break
  }
  
  b <- b + 1
}

  