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

#'*3 A distancia percorrida por um aviao, desde o contacto com'*
#'*o solo ate `a imobilizacao total, e uma variavel aleatoria X com distribuicao normal. Os valores para X, numa serie de 31 aterragens, foram compilados e sao'*
#'*apresentados de seguida (valores em milhares de metros):'*
  
#sum(xi)=54.3    sim(xi^2)=95.57

# Pop
  # X ~ N(μ, σ)
# Amostra
  # n = 31
  # sum(xi) = 54.3
  # sum(xi^2) = 95.57


#'* 1  Calcule estimativas pontuais para a media e variancia da populacao.'*

#x- = (1/n) * sum(xi) <=> (1/31) * 54.3 <=> 1.7516 
#s^2 =  (1/n) * (sum(xi^2) - n * x-^2) <=> (1/31)*(95.57 - 31 *1.7516^2 ) <=>0.0148


#2 

#'*Determine um intervalo de confianca a 99% para a media. Acha que e possıvel'*
#'*efetuar uma aterragem segura numa pista com menos de 1500 metros? Justifique.'*


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
#'*Com a finalidade de estimar o peso medio (em quilos) das'*
#'*criancas de 15 anos de idade em determinada regiao geografica, selecionaram-se'*
#'*aleatoriamente 10 criancas que forneceram os seguintes dados:'*
#'*(33.1, 32.1, 40.9, 37.1, 37.7, 35.1, 30.2, 45.6, 27.8, 37.3)'*
#'*Admita a normalidade.'*


#'*1 Calcule estimativas para a media e o desvio padrao do peso das criancas.'*

#pop
  #'normal
#Amostra
  #n=10
  

Amostra_4.4 <- c(33.1, 32.1, 40.9, 37.1, 37.7, 35.1, 30.2, 45.6, 27.8, 37.3)

Media <- mean(Amostra_4.4)
dP <-sd(Amostra_4.4)

#2
#'*Determine um intervalo de confianca a 99% para o peso medio de todas as'*
#'*criancas.'*

#conf 99
#como temos a amostra podemos usar
t.test(
  x = Amostra_4.4,   # amostra
  mu = Media,        # Media
  conf.level = 0.99  # Confiança 
) #]30.31374 41.06626[

#sem a amostra usavamos 
# I.C.: ] x̅ - (t_(1 - (α/2)); n-1) * (s / sqrt(n)) , x̅ + (t_(1 - (α/2)); n-1) * (s / sqrt(n)) [



#3'* Considerando que a estimativa para o peso medio nao e suficientemente'*
#'*precisa, dado que o intervalo de confianca e demasiado amplo, o que sugere'*
#'*fazer para diminuir a amplitude do intervalo de confianca?'*

#roubei ao capelas

# Para diminuir a amplitude do intervalo, podemos fazer um dos seguintes pontos:
# -- Diminuir o grau de confiança (mantendo o nº de elementos da amostra);
# -- Aumentar o nº de elementos da amostra (mantendo o grau de confiança).





#4
#'*Considerando que a estimativa para o peso medio nao e suficientemente'*
#'*precisa (dado que o intervalo de confianca e demasiado grande), qual deve'*
#'*ser a dimensao da amostra a recolher de modo a obter uma amplitude de 3'*
#'*quilos com um grau de confianca de 99%?'*


n<-11
valoratestar<-3 


while (TRUE)
{
  solucao <-  (35.69 + qt(0.995,n-1) * (5.2314 / sqrt(n))) - (35.69 - qt(0.995,n-1) * (5.2314 / sqrt(n)))
  
  if (solucao  <= valoratestar ) {
    break
  }
  
  n <- n + 1
}




##### 4.5

#'*Considere uma populacao normal com parametros desconhecidos, de onde se obteve uma amostra aleatoria com 16 observacoes, que permitiu'*
#'*construir o seguinte intervalo de confianca para a media da populacao:'*
#'*]7.05, 12.95['*

#Pop
  #x~N(μ,σ)
#Amostra
  #n=16


#'*1. Determine a media amostral.'*

# x- = (7.05 + 12.95) / 2 = 10 

#'*2. Sabendo que, com a informacao da amostra, se obteve s = 4, qual o grau de confianca que pode atribuir ao intervalo referido?'*

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
  

#'*3 . Suponha que a variancia da populacao e 44. Se pretender construir um'*
#'*intervalo de confianca a 95% para a media da populacao, cuja amplitude nao'*
#'*exceda 3.5, qual devera ser a dimensao da amostra a considerar?'*

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

#'*A concentracao ativa de um ingrediente num detergente lıquido'*
#'*e supostamente afetada pelo catalisador usado no processo. O desvio padrao da'*
#'*concentracao ativa e 3 gramas/litro independentemente do catalisador utilizado,'*
#'*sendo o comportamento do processo normal. Foram recolhidas 10 observacoes cada'*
#'*uma com o seu catalisador:'*


catalizador1 <- c(57.9 ,66.2 ,65.4 ,65.2 ,62.6 ,67.6 ,63.7 ,67.2 ,71.0 ,65.4)
catalizador2 <- c(66.4, 71.7, 70.3, 69.3, 64.8, 69.6, 68.6, 69.4, 65.3, 68.8)

#'*1 As amostra sao independentes ou emparelhadas?'*

#R: sao independentes 

#'*2 Determine um intervalo de confiança a 95% para a diferença de medias dos'*
#'*dados obtidos pelos dois catalisadores. Em media, os dados obtidos pelos'*
#'*dois catalisadores podem ser considerados iguais?'*

BSDA::z.test(x=catalizador1,sigma.x = 3,y=catalizador2,sigma.y = 3,conf.level = 0.95 )
#]−5.8296, −0.5704[ com 95% conf  os dois catalizadoes nao podem ser  considerados iguais 



##### 4.7

#'*Pretende-se investigar o nıvel de remuneracao salarial dos'*
#'*homens e mulheres de certa categoria profissional. De duas amostras obtidas entre'*
#'*dois grupos, destacam-se os seguintes resultados (em unidades monet´arias):'*

#'*Amostra de 250 homens: x- =  33.8  s^2 = 5.7'*
#'*Amostra de 150 mulheres: x- = 31   s^2 = 10.3'*

#'*Construa um intervalo de confianca a 99% para as diferencas salariais medias'*
#'*entre os dois generos e conclua sobre a possıvel existencia de discriminacao de'*
#'*genero na atribuicao de remuneracoes.'*



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

#'*Deseja-se saber se um programa de reabilitacao apos enfarte'*
#'*de miocardio diminui a frequencia cardıaca de esforco. Para tal, 10 doentes com'*
#'*enfarte do miocardio foram submetidos a uma prova de esforco antes e depois do'*
#'*programa. Os resultados, expressos em batimentos por minuto, estao na tabela'*
#'*seguinte:'*

antes <- c(147, 122 ,127 ,141, 150, 132, 157, 147, 157 ,155)
depois <- c(132, 117, 142, 124 ,116, 130 ,122, 118 ,135, 117)

#'*Suponha que a variavel em estudo segue uma distribuicao normal.'*

#'*1  As amostra sao independentes ou emparelhadas?'*
#sao emparelhadas pq cada doente foi submetido a duas medições uma antes e uma depois 


#'*2  Recorrendo a um intervalo de confianca a 95% para a diferenca media, indique se acha que o programa de reabilitacao foi eficaz.'*
 t.test(depois,antes, paired = TRUE, conf.level = 0.95)

 
 
#### 4.9
 #'*Foi estudado o grau de satisfacao (medido por questionario)'*
 #'*de varios utentes de uma clınica dentaria antes e depois de lhes ser aplicada uma'*
 #'*nova protese total removıvel. Os resultados, expressos em grau de satisfacao, foram'*
 #'*os apresentados na tabela seguinte:'*
 
 antes <- c(4 ,10, 8 ,13, 7, 3, 15, 7)
 depois <-c(4 ,16, 11, 17, 17, 4, 18, 11)
#'*1  As amostra sao independentes ou emparelhadas?'*
#'*Sao emparelhads
 
#'*2 Recorrendo a um intervalo de confianca a 99% para a diferenca media, '*
#'*indique se a aplicacao da nova protese influenciou o grau de satisfacao dos'*
#'*utentes.'*
 t.test(depois, antes, paired = TRUE, conf.level = 0.99)
#Com 99% de confianca, a aplicacao da nova protese influenciou o grau de satisfacao dos utentes
 
 
####4.10
 
#'*Para comparar a eficiencia de dois metodos de ensino, uma'*
#'*turma de 24 alunos foi dividida aleatoriamente em dois grupos. Cada grupo ´e'*
#'*ensinado de acordo com um metodo diferente. Os resultados no fim do semestre'*
#'*sao os seguintes (numa escala de 0 a 100):'*
 
#'* 1.o Grupo: n1 = 13 x̅1 = 74.5 s1^2 = 82.6'*
#'* 2.o Grupo: n2 = 11 x̅2 = 71.8  sum[1,11](x2- x̅2) = 1126 '*
#'*sum[inicial,final] '*
 
#'*Supondo que as populacoes sao normais (com variancias iguais), obteve-se o'*
#'*seguinte intervalo de confianca para a diferenca entre os valores esperados das duas'*
#'*populacoes:'*
#'*   ] − 5.635, 11.035[.'*

#'*1. Indique qual o grau de confian¸ca utilizado no calculo deste intervalo.'*

  
#s2^2 =  sum[1,11](x2- x̅2)/n2-1 = 1126/10 = 112.6  

 
#] (x̅1 - x̅2) |-+| t_(1 - (α/2); n1 + (n2 - 2)) * sqrt(((1 / n1) + (1 / n2)) * ((((n1 - 1) * s1^2) + ((n2 - 1) * s2^2)) / (n1 + (n2 - 2)))) [
#] (74.5 - 71.8) -+ t_(1 - ((1-b)/2); 13 + (11 - 2)) * sqrt(((1 / 13) + (1 / 11)) * ((((13) * 82.6) + ((11 - 1) * 112.6)) / (13 + (11 - 2)))) [

 
#(74.5 - 71.8) + qt(1 - (alpha/2), 13 + (11 - 2)) * sqrt(((1 / 13) + (1 / 11)) * ((((13) * 82.6) + ((11 - 1) * 112.6)) / (13 + (11 - 2))))  = 11.035
# 2.7 + qt(1 - (alpha/2), 22) * 4.096546 = 11.035
#qt(1 - (alpha/2), 22) * 4.096546 = 11.035 -2.7
#qt(1 - (alpha/2), 22) * 4.096546 = 8.335
#qt(1 - (alpha/2), 22) = 8.335/4.096546
#qt(1 - (alpha/2), 22) = 8.335/4.096546
#qt(1 - (alpha/2), 22) = 2.034641
#1 - (alpha/2) = pt(2.034641,22) 
#'*nota METER NO RELATORIO *
#1 - (alpha/2) = 0.9729428
#- (alpha/2) = 0.9729428 -1 
#- (alpha/2) = -0.0270572
# (alpha/2) = 0.0270572
# alpha = 0.0270572 * 2 
#alpha = 0.0541144
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
 
#'*2 Com base num intervalo de confianca a 90% acha que, em media, os metodos'*
#'*de ensino podem ser considerados iguais?'*
   
# [ = (74.5 - 71.8) + qt(1 - (0.10/2), 13 + (11 - 2)) * sqrt(((1 / 13) + (1 / 11)) * ((((13) * 82.6) + ((11 - 1) * 112.6)) / (13 + (11 - 2))))
# ] = (74.5 - 71.8) - qt(1 - (0.10/2), 13 + (11 - 2)) * sqrt(((1 / 13) + (1 / 11)) * ((((13) * 82.6) + ((11 - 1) * 112.6)) / (13 + (11 - 2))))

#]-4.334361 ,9.734361[ 
# o intervalo de confiança inclui zero, Portanto, em media, os metodos de ensino podem ser considerados iguais com base no intervalo de confiança a 90%(ppt4 slide 76)

#'*3 Com base num intervalo de confian¸ca a 90% verifique se a suposicao das'*
#'*variancias serem iguais e valida.'*


# I.C.: ] ((1 / f_(1 - (α/2); n1 - 1; n2 - 1)) * (s1^2 / s2^2)) , 
# I.C.:   ((1 / f_(α/2; n1 - 1; n2 - 1)) * (s1^2 / s2^2)) [ 

# ] ((1 / qf(1 - (0.10/2), 12, 10)) * (82.6 / 112.6)) , ((1 / qf(0.10/2, 12, 10)) * (82.6/ 112.6)) [

#] 0.2518284,2.019802[ 
#Com 90% de confianca a suposicao das variancias serem iguais e valida pq 1 exite dentro do ic (ppt4  slide 80)
 
 
 
#### 4.11
 
#'*Duas marcas de comprimidos, um deles contendo aspirina,'*
#'*sao anunciados como fazendo desaparecer a dor de cabeca em tempo recorde.'*
#'*Foram feitas experiencias com cada um deles, tendo-se obtido duas amostras'*
#'*aleatorias independentes, cujos resultados (tempo em minutos) foram os seguintes:'*
   
#'*Comprimido 1 (com aspirina):'*
#'*   9.6; 9.4; 9.3; 11.2; 11.4; 12.1; 10.4; 9.6; 10.2; 8.8; 13.0'*
#'* Comprimido 2 (sem aspirina):'*
#'*  11.4; 12.1; 10.4; 9.6; 8.5; 9.7; 12.3; 12.4; 10.8; 10.8'*
 
#'*Admita que as populacoes sao normais e que os desvios padrao sao iguais.'*

#'*1 Construa um intervalo de confianca a 95% para a verdadeira diferenca das'*
#'*medias das respostas aos dois medicamentos. Acha que, em media, as respostas dos dois medicamentos podem ser consideradas iguais?'*
Comprimido_1 <- c(9.6, 9.4, 9.3, 11.2, 11.4, 12.1, 10.4, 9.6, 10.2, 8.8, 13.0)
Comprimido_2 <- c(11.4, 12.1, 10.4, 9.6, 8.5, 9.7, 12.3, 12.4, 10.8, 10.8)

t.test(Comprimido_1,Comprimido_2,paired = FALSE,conf.level = 0.95)

#IC ] 1.5371902 , 0.8462811 [ com 95% de conf podem ser consideradas iguais (inclui o 0 no ic )



#'*2 Com base num intervalo de confianca a 95% verifique se a suposicao dos'*
#'*desvios padrao serem iguais e valida.'*

var.test(Comprimido_1,Comprimido_2,conf.level=0.95)

#alpha = 1- conf = 0.05

#s1^2 = var(Comprimido_1) = 1.734727
#s1^2 = var(Comprimido_2) = 1.662222

# I.C.: ] ((1 / qf(1 - (0.05/2),10 ,9)) * (1.734727 / 1.662222)) , 
# I.C.:   ((1 / qf(0.05/2, 10,9)) * (1.734727 / 1.662222)) [ 
#]0.2632833 ,3.943798 [ com 95% de conf podem ser consideradas iguais  (inclui o 1 no ic )


####'*4.12 Obtem-se uma amostra de 15 cranios de homens egpcios'*
#'*que viveram por volta de 1850 a.c.. Mede-se a largura maxima de cada cranio,'*
#'*e obtiveram-se da amostra uma media de 134.5 mm e um desvio padrao de 3.5'*
#'*mm (com base em dados de Ancient Races of Thebaid, por Thomson e RandallMaciver ). Suponha que a largura maxima dos cranios tem um comportamento'*
#'*normal. Com esses dados amostrais, construa um intervalo de 95% de confian¸ca'*
#'*para o desvio padrao populacional.'*



#Pop
  #normal
#Amostra
  #n=15
  #x- = 134.5
  #s = 3.5
#aplha = 1- conf = 0.05

#] (((n-1) * s^2) / x^2_(1 - (α/2); n-1)) , (((n-1) * s^2) / x^2_(α/2; n-1)) [
#] (((14) * 3.5^2) / qchisq(1 - (0.05/2), 14)) , (((14) * 3.5^2) / qchisq(0.05/2, 14)) [
#]6.566114,30.46871[
#]sqrt(6.566114),sqrt(30.46871)[
#]2.562443,5.519847[



####4.13
#'*Pretende-se estudar a variabilidade do tempo de espera (em'*
#'*minutos) de clientes num dado banco, onde os clientes entram numa fila unica.'*
#'*Suponha que o tempo de espera segue uma distribuicao normal. Construa um'*
#'*intervalo de 95% de confianca para o desvio padrao populacional sabendo que se'*
#'*recolheu a seguinte amostra:'*

#'*(6.5; 6.6; 6.7; 6.8; 7.1; 7.3; 7.4; 7.7; 7.7; 7.7).'*

amostra <- c(6.5, 6.6, 6.7, 6.8, 7.1, 7.3, 7.4, 7.7, 7.7, 7.7)
s2<- var(amostra) # s^2 para s = sd(amostra)

n <- length(amostra)

#] (((n-1) * s^2) / x^2_(1 - (α/2); n-1)) , (((n-1) * s^2) / x^2_(α/2; n-1)) [
# ] ((9 *s2) / qchisq(1 - (0.05/2), 9)) , ((9 * s2) / qchisq(0.05/2, 9)) 
#]0.1075028, 0.7572982[
#]sqrt(0.1075028), sqrt(0.7572982)[
#]0.3278762,0.8702288[


####4.14
#'*Considere-se a seguinte amostra de uma populacao cuja distribuicao e Normal:'*
#'*  (9; 14; 10; 12; 7; 3; 11; 12).'*
#'*Nestas condicoes, construa o intervalo de confianca a 99% mais adequado para a'*
#'*variancia dessa populacao.'*

amostra <-c(9, 14, 10, 12, 7, 3, 11, 12)

library(EnvStats)
EnvStats::varTest(x=amostra,conf.level=0.99)
#]4.117816, 84.406894 [

####4.15

#'*Durante uma avaliacao de desempenhos das escolas A e B,'*
#'*sugeriu-se que a escola A tinha uma maior variabilidade que a escola B em termos'*
#'*das notas finais dos alunos. Fizeram-se 16 registos de classificacoes para a escola'*
#'*A e 21 registos de classificacoes para a escola B conduzindo as variancias de 6.62'*
#'*e 3.80, respetivamente. Suponha que as populacoes em estudo tem um comportamento normal. Construa um intervalo de confianca a 90% para a razao das'*
#'*verdadeiras variancias e diga se a variabilidade das escolas pode ser considerada'*
#'*diferente.'*


#pop
  #A escola
  #B eSCOLA
#Amostra
  #na=16
  #nb=21
  #sa^2 = 6.62
  #sb^2 = 3.80


# I.C.: ] ((1 / f_(1 - (α/2); n1 - 1; n2 - 1)) * (s1^2 / s2^2)) , 
# I.C.:   ((1 / f_(α/2; n1 - 1; n2 - 1)) * (s1^2 / s2^2)) [

# I.C.: ] ((1 / qf(1 - (0.10/2),15,20)) * (6.62 / 3.80)) , 
# I.C.:   ((1 / qf(0.10/2, 15, 20)) * (6.62 / 3.80)) [

#]0.7906892, 4.054811[ #com 90% as variabilidade nao pode ser considerada diferente.




#####4.16

#'*Numa regiao afetada por um surto epidemico, observou-se'*
#'*uma amostra de 2500 indivıduos, tendo-se encontrado 850 contaminados. Determine intervalos de confianca a 95% e 98% de confianca para a proporcao de'*
#'*contaminados na populacao.'*


#amostra
  #n=2500
  #nconta=850
  #p* = 850/2500 = 0.34


#] p* |-+| z_(1 - (α/2)) * sqrt((p* * q*) / n) [
#] 0.34 |-+| qnorm(1 - (0.05/2)) * sqrt((0.34 *0.66) / 2500) [
#] 0.34 - qnorm(1 - (0.05/2)) * sqrt((0.34 *0.66) / 2500) , 0.34 + qnorm(1 - (0.05/2)) * sqrt((0.34 *0.66) / 2500) [
# ]0.321431 , 0.358569[ para 95 conf
# ]0.3179598 .  0.3620402 [ para 98 conf


##### 4.17

#'*Num estudo de mercado efetuado sobre uma amostra aleatoria'*
#'*de 400 consumidores, foi encontrado o seguinte intervalo de confianca para a proporcoo de pessoas recetivas a um novo tipo de espuma de banho a lancar em breve'*
#'*no mercado:'*
#'*  ]0.5114, 0.6086['*

#'*1. Em relacao `a amostra recolhida, qual foi a percentagem de pessoas recetivas'*
#'*a um novo tipo de espuma de banho?'*
  

#p^ = (0.5114 - 0.6086)/2 = 1.12/2 = 0.56 ou seja 56%


#'*2 Mostre que o grau de confianca considerado no intervalo calculado e de 95%'*
#] 0.56 - qnorm(1 - (0.05/2)) * sqrt((0.56 * 0.44) / 400) [ ~=~ 0.5114
#] 0.56 + qnorm(1 - (0.05/2)) * sqrt((0.56 * 0.44) / 400) [ ~=~ 0.6086

#'*.3 Comente as seguintes afirma¸c˜oes, indicando se estas lhe parecem corretas ouincorretas:'*
  #'*(a) 95% das pessoas v˜ao passar a usar a nova espuma de banho.'*
  #Afirmaçao é falsa 

  #'*(b) A quota de mercado poder´a ser, com 95% de confian¸ca, de 56%.'*
  #Afirmação é falsa 

##### 4.18
#'*Recolheu-se uma amostra de 40 alunos do 1.o ano da ESTSetubal'*
#'*tendo-se verificado que 10 destes alunos frequentam os cursos que escolheram em primeira opção'*

#'*1. Calcule um intervalo de confianca a 95%, para a verdadeira proporcao de'*
#'*estudantes que esta no curso que escolheu em primeira opcao.'*

#Populaçao

#Amostra
  #n=40
  #primeiraopcao n=10
#p* = 10/40 = 0.25
#q* = 1-p* = 1-0.25 = 0.75
#α = 1- conf.level = 

#] p* |-+| z_(1 - (α/2)) * sqrt((p* * q*) / n) [

#] 0.25 |-+| qnorm(1 - (0.05/2)) * sqrt((0.25 * 0.75) / 40) [
#] 0.25 |-+| qnorm(1 - (0.05/2)) * sqrt((0.25 * 0.75) / 40) [
#] 0.25 |-+| 0.1341896 [
#] 0.1158104 ,0.3841896 [

#'*2. Se pretendesse reduzir a metade a amplitude do intervalo anterior:'*

#'*(a) e manter a dimensao da amostra, qual o grau de confianca que deveria utilizar?'*


alpha<-1
c <- (0.3841896  - 0.1158104)/2

while (TRUE)
{
  Ampliture <- ( 0.25 + qnorm(1 - (alpha/2)) * sqrt((0.25 * 0.75) / 40)) - (0.25 - qnorm(1 - (alpha/2)) * sqrt((0.25 * 0.75) / 40))
  
  if (Ampliture  >= c ) {
    break
  }
  if (alpha ==0.01){
    break
  }
  
  
  alpha<- alpha - 0.01
} 

#como alpha = 0.32 ent o nivel de conf é 68

#'*(b) e manter o grau de confianca, qual a dimensao da amostra que deveria'*
#'*utilizar? Suponha que nao ha alteracao na estimativa da proporcao.'*


n<-40
c <- (0.3841896  - 0.1158104)/2


while (TRUE)
{
  Ampliture <- ( 0.25 + qnorm(1 - (0.05/2)) * sqrt((0.25 * 0.75) / n)) - (0.25 - qnorm(1 - (0.05/2)) * sqrt((0.25 * 0.75) / n))
  
  if (Ampliture  <= c ) {
    break
  }
  
  
  n<- n + 1
} 

#n = 160


#'*(c) e manter o grau de confianca, qual a dimensao da amostra que deveria*
#'*utilizar? Suponha que nao conhece estimativas da proporcao. *



n<-40
c <- (0.3841896  - 0.1158104)/2


while (TRUE)
{

  Ampliture <- ( 0.5 + qnorm(1 - (0.05/2)) * sqrt((0.5 * 0.5) / n)) - (0.5 - qnorm(1 - (0.05/2)) * sqrt((0.5 * 0.5) / n))/2
  
  
  if (Ampliture  <= c ) {
    break
  }
  

  n<- n + 1
} 




#4.19
#'*Uma reporter da revista Byte deseja fazer uma pesquisa para'*
#'*estimar a verdadeira proporcao de todos os universitarios que tem computador'*
#'*pessoal. Nos seus resultados a reporter quer ter 95% de confianca e uma margem'*
#'*de erro de 0.04. Quantos universitarios devem ser pesquisados?'*
  
n<-1
c <-  0.04 


while (TRUE)
{
  
  Ampliture <- (( 0.5 + qnorm(1 - (0.05/2)) * sqrt((0.5 * 0.5) /  n)) - (0.5 - qnorm(1 - (0.05/2)) * sqrt((0.5 * 0.5) /  n)) )
  
  
  if (Ampliture  <= c ) {
    break
  }
  
  
  n<- n + 1
} 





#AMPLITURE =  (p* + 2z_(1 - (α/2)) * sqrt((p* * q*) / n)) - (p* - z_(1 - (α/2)) * sqrt((p* * q*) / n))
#<=> 2z_(1 - (α/2)) * sqrt((p* * q*) / n))

#Logo 
#Margem de erro = (2z_(1 - (α/2)) * sqrt((p* * q*) / n)) /2  <=>  z_(1 - (α/2)) * sqrt((p* * q*) / n)
#0.04 =  qnorm(1 - (0.05/2)) * sqrt((0.5* 0.5) / n)
#0.04 =  1.96 * sqrt((0.5* 0.5) / n)
#0.04 =  1.96 * sqrt((0.25) / n)
#0.04 / 1.96 = sqrt((0.25) / n)
#0.02040816 = sqrt((0.25) / n)
#0.02040816 ^2 = sqrt((0.25) / n)^2
#0.000416493 = (0.25) / n)
#n = 0.25 / 0.000416493
#n = 601



####4.20
#'*Pretende-se estimar o numero total de medicos que trabalham'*
#'*numa certa cidade e estao associados a planos de saude. Para isso recolheu-se uma'*
#'*amostra aleatoria com 300 medicos dessa cidade e apurou-se que entre eles 216 se'*
#'*enquadram nessa condicao. Obtenha um intervalo de confianca a 98% para a sua'*
#'*estimativa, sabendo que o numero total de medicos na cidade e 28000.'*


#Populaçao
  #28000 Medicos
#Amostra
  #n=300
  #condiçao=216

#p* = 216/300 = 0.72
#q* = 1-p* = 1-0.72 = 0.28
#α = 1- conf.lvl = 1-0.98 = 0.02

#] p* |-+| z_(1 - (α/2)) * sqrt((p* * q*) / n) [
#] 0.72* |-+| qnorm(1 - (0.02/2)) * sqrt((0.72 * 0.28) / 300) [
#] 0.72 - qnorm(1 - (0.02/2)) * sqrt((0.72 * 0.28) / 300) ,0.72 + qnorm(1 - (0.02/2)) * sqrt((0.72 * 0.28) / 300) [
#] 0.6596942, 0.7803058 [ #IC PARA 300 Medicos
#] 0.6596942 * 28000 , 0.7803058 * 28000 [ #IC PARA 28000 Medicos
#]18471.44,  21848.56[ #IC PARA 28000 Medicos

prop.test(
  x=216,
  n=300,
  p=0.72,
  conf.level = 0.98
)


###4.21
#'*Dois inqueritos realizados (em 2009 e 2019), relativamente ao'*
#'*consumo de bebidas alcoolicas, em idades entre os 15 e os 35 anos, forneceram os'*
#'*seguintes dados:'*
#
#'*Ano 2009 , nº inquiridos = 4000, Consumidores= 1750,Nao consumidores =2250  *
#'*Ano 2019 , nº inquiridos = 5000, Consumidores= 2250,Nao consumidores =2750  *

#'*Atraves de um intervalo de confianca, a 98%, indique a veracidade da afirmacao:'*
#'*”A percentagem de consumidores de bebidas alcoolicas, em indivıduos com idades'*
#'*compreendidas entre os 15 e os 35 anos, registou um grande aumento na decada'*
#'*analisada.”'*

#Populaçao
  #Alcolicos entre os 15 e os 35 anos
#Amostra
  #n1 = 4000
  #n2 = 5000

#p1* = 1750/4000 = 0.4375
#q1* = 1- p1* = 1- 0.4375 = 0.5625
#p2* = 2250/5000 = 0.45
#q2* = 1- p2* = 1 - 0.45 = 0.55
#α = 1- conf.lvl = 1-0.98 = 0.02


#] (p1* - p2*) |-+| z_(1 - (α/2)) * sqrt(((p1* * q1*) / (n1)) + ((p2* * q2*) / (n2))) [
#] (0.4375 - 0.45) |-+| z_(1 - (0.02/2)) * sqrt(((0.4375 * 0.5625) / (4000)) + ((0.45 * 0.55) / (5000))) [
#] (0.4375 - 0.45) |-+| qnorm(1 - (0.02/2)) * sqrt(((0.4375 * 0.5625) / (4000)) + ((0.45 * 0.55) / (5000))) [
#] -0.0125 |-+| 0.02451218 [
#] -0.0125 |-+| 0.02451218 [
#] -0.03701218 , 0.01201218 [ #Com 98% conf a afirmaçao nao é verdadeira porque podem ser consideradas iguais ( 0 € no IC )


prop.test(
  x=c(1750,2250),
  n=c(4000,5000),
  conf.level = 0.98
)



####4.22

#'*Com o objetivo de verificar o efeito de um novo medicamento'*
#'*no tratamento de uma dada doenca, dois grupos, A e B, foram formados, cada um'*
#'*composto por 100 indivıduos que apresentavam a tal doenca, estando todos eles'*
#'*no mesmo estagio da mesma. O grupo A recebeu o novo medicamento e o grupo B'*
#'*recebeu um placebo. Curaram-se da doenca 75 pessoas no grupo A e 65 no grupo'*
#'*B. E possıvel afirmar que o novo medicamento e eficaz no tratamento da doenca? ´'*
#'*Justifique a sua resposta recorrendo a um intervalo de confianca a 95%.'*


#População
  #Individos com tal doenca
#Amostra
  #na= 100
  #cA=75
  #nb=100
  #cb=65


#p1* = 75/100= 0.75
#q1* = 1-p1* = 0.25
#p2* = 65/100= 0.65
#q2* = 1-p1* = 0.35
#α = 1- conf.lvl = 1-0.95 = 0.05


#] (p1* - p2*) |-+| z_(1 - (α/2)) * sqrt(((p1* * q1*) / (n1)) + ((p2* * q2*) / (n2))) [
#] (0.75 - 0.65) |-+| qnorm(1 - (0.05/2)) * sqrt(((0.75 * 0.25) / (100)) + ((0.65 * 0.35) / (100))) [
#] 0.1 |-+| 0.1262618 [
#] -0.026261, 0.2262618 [ #com 95 conf nao podemos afirmar que o novo medicamento é eficaz (0 € IC)


####4.23

#'*No ficheiro EXCEL ”Obesidade” tem as respostas a um'*
#'*inquerito efetuado num estudo sobre obesidade a um grupo de indivıduos obesos. '*


#'* 1 Calcule estimativas pontuais para a altura media e para a variancia das alturas.'*
#x- = mean(obesidade$Altura) = 1.701677
#s^2 = var(obesidade$Altura) = 0.008705789


#'*2. Calcule um intervalo de confianca a 99% para a altura media.'*

BSDA::z.test(x=obesidade$Altura,sigma.x = sd(obesidade$Altura),conf.level = 0.99)

#'*3. Qual a margem de erro do intervalo da al´ınea anterior?'*

#(1.706908-1.696446) /2 =0.005231 


#'*4.Recorrendo a um intervalo de confianca a 90%, acha que, em media, os'*
#'*pesos do genero feminino podem ser considerados iguais aos pesos do genero'*
#'*masculino? Justifique.'*


masculino <- obesidade[obesidade$Genero=="Masculino",]$Peso
feminino <- obesidade[obesidade$Genero=="Feminino",]$Peso

BSDA::z.test(x=masculino,sigma.x = sd(masculino),y=feminino,sigma.y = sd(feminino),conf.level = 0.90)
#como 0 nao pertece ao ic com 90% de conf os pesos entre genros nao podem ser considerados iguais 


#'*5. Remova os ”outliers” existentes nos dados referentes `a Idade. Com os dados sem ”outliers”:'*
remover_outliers <- function (VARIAVEL) {
  boxplot_outliers_aux <- boxplot(
    VARIAVEL,
    col = "gold",
    horizontal = TRUE,
    main = "Extremos e Quartis - Sem Outliers",
    xlab = "VARIAVEL",
    type = 2,
    range = 1.5
  )
  
  outliers <- boxplot_outliers_aux$out
  
  sem_outliers <- VARIAVEL
  sem_outliers <- sem_outliers[!sem_outliers %in% outliers]
  
  return(sem_outliers)
}

#'* (a) calcule um intervalo de confian¸ca a 92% para a idade media;'*


idade <- remover_outliers(obesidade$Idade)


BSDA::z.test(x=idade,sigma.x = sd(idade),conf.level = 0.92)


#'* (b)recorrendo a um intervalo de confianca a 95%, acha que, em media, ha*
#'*diferencas na idade das pessoas que comem habitualmente alimentos'*
#'*altamente caloricos e os que nao comem? Justifique.*

idade_come <- remover_outliers(obesidade[obesidade$FAVC == 1,]$Idade)
idade_nao_come <- remover_outliers(obesidade[obesidade$FAVC == 0,]$Idade)

BSDA::z.test(x=idade_come,sigma.x = sd(idade_come),y=idade_nao_come,sigma.y = sd(idade_nao_come),conf.level = 0.95)
#'* Com 95% de confianca e com base nas amostras'* 
#'* recolhidas, em media, ha diferencas na idade das pessoas que comem habitualmente alimentos altamente caloricos e os que nao comem.'* 
