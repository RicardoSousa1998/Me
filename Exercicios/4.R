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
} <- 


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




##### 4.5

#Considere uma populacao normal com parametros desconhecidos, de onde se obteve uma amostra aleatoria com 16 observacoes, que permitiu
#construir o seguinte intervalo de confianca para a media da populacao:
#]7.05, 12.95[

#Pop
  #x~N(μ,σ)
#Amostra
  #n=16


#1. Determine a media amostral.

# x- = (7.05 + 12.95) / 2 = 10 

#2. Sabendo que, com a informacao da amostra, se obteve s = 4, qual o grau de confianca que pode atribuir ao intervalo referido?

#s = 4

# x̅ - (t_(1 - (α/2)); n-1) * (s / sqrt(n))  =  7.05
alpha<- 1
c <- 7.05

while (TRUE)
{
  Ampliture2 <-  (10 - qt(1-(alpha/2),15) *  (4 / sqrt(16)) )
  
  if (Ampliture2  <= c ) {
    break
  }
  if (alpha <= 0.01){
    break
  }
  
  alpha <- alpha - 0.01
}
#alpha = 0.01 =  conf  0.99






#x̅ + (t_(1 - (α/2)); n-1) * (s / sqrt(n))  = 12.95
alpha<- 1
c <- 12.95

while (TRUE)
{
  Ampliture2 <-  (10 + qt(1-(alpha/2),15) *  (4 / sqrt(16)) )
  
  if (Ampliture2  >= c ) {
    break
  }
  if (alpha <= 0.01){
    break
  }
  
  alpha <- alpha - 0.01
}  
#alpha = 0.01 =  conf  0.99
  

#3 . Suponha que a variancia da populacao e 44. Se pretender construir um
#intervalo de confianca a 95% para a media da populacao, cuja amplitude nao
#exceda 3.5, qual devera ser a dimensao da amostra a considerar?

#pop
  #x~N(μ,sqrt(44) = 6.6333)
#Amostra
  #n=16
  
#conf = 0.95 <=> alpha = 1-0.95 = 0.05


n<-1
c <- 3.5

while (TRUE)
{
  Ampliture <-  (10 +  1.959964 * (6.6333 / sqrt(n))) - (10 -  1.959964 * (6.6333 / sqrt(n))) 
  
  if (Ampliture  <= c ) {
    break
  }

  
  n<- n + 1
} 

#A dimensao da amostra da amostra deve ser maior que 56


##### 4.6

#A concentracao ativa de um ingrediente num detergente lıquido
#e supostamente afetada pelo catalisador usado no processo. O desvio padrao da
#concentracao ativa e 3 gramas/litro independentemente do catalisador utilizado,
#sendo o comportamento do processo normal. Foram recolhidas 10 observacoes cada
#uma com o seu catalisador:


catalizador1 <- c(57.9 ,66.2 ,65.4 ,65.2 ,62.6 ,67.6 ,63.7 ,67.2 ,71.0 ,65.4)
catalizador2 <- c(66.4, 71.7, 70.3, 69.3, 64.8, 69.6, 68.6, 69.4, 65.3, 68.8)

#1 As amostra sao independentes ou emparelhadas?

#R: sao independentes 

#2 Determine um intervalo de confiança a 95% para a diferença de medias dos
#dados obtidos pelos dois catalisadores. Em media, os dados obtidos pelos
#dois catalisadores podem ser considerados iguais?

BSDA::z.test(x=catalizador1,sigma.x = 3,y=catalizador2,sigma.y = 3,conf.level = 0.95 )
#]−5.8296, −0.5704[ com 95% conf  os dois catalizadoes nao podem ser  considerados iguais 



##### 4.7

#Pretende-se investigar o nıvel de remuneracao salarial dos
#homens e mulheres de certa categoria profissional. De duas amostras obtidas entre
#dois grupos, destacam-se os seguintes resultados (em unidades monet´arias):

#Amostra de 250 homens: x- =  33.8  s^2 = 5.7
#Amostra de 150 mulheres: x- = 31   s^2 = 10.3

#Construa um intervalo de confianca a 99% para as diferencas salariais medias
#entre os dois generos e conclua sobre a possıvel existencia de discriminacao de
#genero na atribuicao de remuneracoes.



#População
  #X1 remuneracao salarial do homens 
  #X2 remuneracao salarial das mulheres
#Amostra 
  #n1 = 250,x-1 = 33.8 ,s^2 =5.7
  #n2 = 150,x- = 31, s^2 = 10.3
  

#Conf = 99 = 0.99
#alpha = 1-0.99 = 0.01

# ] (x̅1 - x̅2) |-+| z_(1 - (α/2)) * sqrt((s1^2 / n1) + (s2^2 / n2)) [
# ] (33.8 - 31) |-+| z_(1 - (0.01/2)) * sqrt((5.7 / 250) + (10.3 / 150)) [
# ] 2.8 |-+| 2.575829 * 0.3024346 [
# ]2.02098 ,3.57902 [



##### 4.8

#Deseja-se saber se um programa de reabilitacao apos enfarte
#de miocardio diminui a frequencia cardıaca de esforco. Para tal, 10 doentes com
#enfarte do miocardio foram submetidos a uma prova de esforco antes e depois do
#programa. Os resultados, expressos em batimentos por minuto, estao na tabela
#seguinte:

antes <- c(147, 122 ,127 ,141, 150, 132, 157, 147, 157 ,155)
depois <- c(132, 117, 142, 124 ,116, 130 ,122, 118 ,135, 117)

#Suponha que a variavel em estudo segue uma distribuicao normal.

#1  As amostra sao independentes ou emparelhadas?
#sao emparelhadas pq cada doente foi submetido a duas medições uma antes e uma depois 


#2  Recorrendo a um intervalo de confianca a 95% para a diferenca media, indique se acha que o programa de reabilitacao foi eficaz.
 t.test(antes, depois, paired = TRUE, conf.level = 0.95)

 
 
#### 4.9
 #Foi estudado o grau de satisfacao (medido por questionario)
 #de varios utentes de uma clınica dentaria antes e depois de lhes ser aplicada uma
 #nova protese total removıvel. Os resultados, expressos em grau de satisfacao, foram
 #os apresentados na tabela seguinte:
 
 antes <- c(4 ,10, 8 ,13, 7, 3, 15, 7)
 depois <-c(4 ,16, 11, 17, 17, 4, 18, 11)
#1  As amostra sao independentes ou emparelhadas?
#Sao emparelhads
 
#2 Recorrendo a um intervalo de confianca a 99% para a diferenca media, 
#indique se a aplicacao da nova protese influenciou o grau de satisfacao dos
#utentes.
 t.test(depois, antes, paired = TRUE, conf.level = 0.99)
 #Com 99% de confianca, a aplicacao da nova protese influenciou o grau de satisfacao dos utentes
 
 
 ####4.10
 
 #Para comparar a eficiencia de dois metodos de ensino, uma
 #turma de 24 alunos foi dividida aleatoriamente em dois grupos. Cada grupo ´e
 #ensinado de acordo com um metodo diferente. Os resultados no fim do semestre
 #sao os seguintes (numa escala de 0 a 100):
 
# 1.o Grupo: n1 = 13 x̅1 = 74.5 s1^2 = 82.6
# 2.o Grupo: n2 = 11 x̅2 = 71.8  sum[1,11](x2- x̅2) = 1126 
 #sum[inicial,final] 
 
 #Supondo que as populacoes sao normais (com variancias iguais), obteve-se o
 #seguinte intervalo de confianca para a diferenca entre os valores esperados das duas
 #populacoes:
#   ] − 5.635, 11.035[.

 #1. Indique qual o grau de confian¸ca utilizado no calculo deste intervalo.

  
  #s2^2 =  sum[1,11](x2- x̅2)/n2-1 = 1126/10 = 112.6  

 
 #] (x̅1 - x̅2) |-+| t_(1 - (α/2); n1 + (n2 - 2)) * sqrt(((1 / n1) + (1 / n2)) * ((((n1 - 1) * s1^2) + ((n2 - 1) * s2^2)) / (n1 + (n2 - 2)))) [
 #] (74.5 - 71.8) -+ t_(1 - ((1-b)/2); 13 + (11 - 2)) * sqrt(((1 / 13) + (1 / 11)) * ((((13) * 82.6) + ((11 - 1) * 112.6)) / (13 + (11 - 2)))) [

 
 
 (74.5 - 71.8) + q(1 - (alpha/2), 13 + (11 - 2)) * sqrt(((1 / 13) + (1 / 11)) * ((((13) * 82.6) + ((11 - 1) * 112.6)) / (13 + (11 - 2)))) 
 
 
 alpha<- 1
 c <- 11.035
 
 while (TRUE)
 {
   value <-   (74.5 - 71.8) + qt(1 - (alpha/2), 13 + (11 - 2)) * sqrt(((1 / 13) + (1 / 11)) * ((((13) * 82.6) + ((11 - 1) * 112.6)) / (13 + (11 - 2)))) 
   
   if (value  >= c ) {
     break
   }
   if (alpha <= 0.01){
     break
   }
   
   alpha <- alpha - 0.01
 }  
 
 #como alpha é 0.05 o conf é 0.95 ou seja 95%
 
 #2 Com base num intervalo de confianca a 90% acha que, em media, os metodos
 #de ensino podem ser considerados iguais?
   
 # [ = (74.5 - 71.8) + qt(1 - (0.10/2), 13 + (11 - 2)) * sqrt(((1 / 13) + (1 / 11)) * ((((13) * 82.6) + ((11 - 1) * 112.6)) / (13 + (11 - 2))))
 # ] = (74.5 - 71.8) - qt(1 - (0.10/2), 13 + (11 - 2)) * sqrt(((1 / 13) + (1 / 11)) * ((((13) * 82.6) + ((11 - 1) * 112.6)) / (13 + (11 - 2))))
 
 #]-4.334361 ,9.734361[ 
 # o intervalo de confiança inclui zero, Portanto, em media, os metodos de ensino podem ser considerados iguais com base no intervalo de confiança a 90%(ppt4 slide 76)

 
 #3 Com base num intervalo de confian¸ca a 90% verifique se a suposicao das
 #variancias serem iguais e valida.
 
 
 # I.C.: ] ((1 / f_(1 - (α/2); n1 - 1; n2 - 1)) * (s1^2 / s2^2)) , 
 # I.C.:   ((1 / f_(α/2; n1 - 1; n2 - 1)) * (s1^2 / s2^2)) [ 
 
 # ] ((1 / qf(1 - (0.10/2), 12, 10)) * (82.6 / 112.6)) , ((1 / qf(0.10/2, 12, 10)) * (82.6/ 112.6)) [
 
 #] 0.2518284,2.019802[ 
 #Com 90% de confianca a suposicao das variancias serem iguais e valida pq 1 exite dentro do ic (ppt4  slide 80)