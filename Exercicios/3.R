#####3.1
#'*O peso dos indivıduos duma certa especie de bivalves tem'*
#'*distribuiçao normal de media 31g e desvio padrao 2.4g. Recolhe-se uma amostra'*
#'*aleatoria de 36 indivíduos desta especie.'*

#'*1 Qual a probabilidade da media da amostra ser inferior a 30g?'*
z1 <- (30 - 31) / (2.4 / sqrt(36))
pnorm(z1)



#'*2 Qual a probabilidade da media da amostra estar compreendida entre 30 e 32g?'*
z2 <- (32 - 31) / (2.4 / sqrt(36))
pnorm(z2)-pnorm(z1)


#maneira dif 2

#como normal é simetrica  e z1 e z2 serem parecidos   (-2.5 & 2.5 )
#podemos fazer 
1 - (pnorm(z1)+pnorm(z1))
#



#####3.3

#'*Sabe-se que a idade de determinada camada do subsolo segue'*
#'*uma distribuição Normal com media de 0.5 milhões de anos e um desvio padrão de'*
#'*20000 anos. Seleccionados ao acaso 10 amostras de subsolo, calcule a probabilidade'*
#'*da media amostral das suas idades ser superior a 490000 anos.'*


A<-0.49
B<-0.5
C<-0.02
D<-10

z <-(A - B) / (C / sqrt(D))

1-pnorm(z)



#####3.4
#'*A duração das chamadas recebidas na central telefónica de'*
#'*uma determinada empresa tem distribuição normal de media 17 minutos e desvio'*
#'*padrão 5 minutos.'*

#Popu
  #normal
  #media 17 minutos
  #dp 5

#Amostra


#'*1  Determine a probabilidade de numa amostra aleatória de 100 chamadas, a'*
#'*duração media se situar entre os 16 minutos e os 18 minutos.'*

# População Normal;
# σ Conhecido;

A<-#Valor Media Amostra = x̅
B<-#Media população = μ
C<-#Desvio padrão População = σ
D<-#Tamanho Amostra = n
z <-(A - B) / (C / sqrt(D)) 

z1 <-(16 - 17) / (5 / sqrt(100)) 
z2 <-(18 - 17) / (5 / sqrt(100)) 


#P(16< Xˉ <18) = P(16) - P(18) =  pnorm(z2) - pnorm(z1) = 0,9544 ou seja 95%

#'*2 #Se a população nao fosse Normal, qual seria a probabilidade da alınea anterior?'*

# População Qualquer;
# σ Conhecido;
# n>= 30
A<-#Valor Media Amostra = x̅
B<-#Media população = μ
C<-#Desvio padrão População = σ
D<-#Tamanho Amostra = n
 z <-(A - B) / (C / sqrt(D)) 

z1 <-(16 - 17) / (5 / sqrt(100)) 
z2 <-(18 - 17) / (5 / sqrt(100)) 

#P(16< Xˉ <18) = P(16) - P(18) = pnorm(z2) - pnorm(z1) = 0,9544 ou seja 95%
#Fica igual

#'*3 Qual o tamanho da amostra aleatoria a recolher para que nao seja superior'*
#'*a 5% a probabilidade da media da amostra diferir da media da populacao'*
#'*por mais de 5 minutos?'*





#####'*3.5 Admita-se uma população Normal com parâmetros desconhecidos. Selecionou-se ao acaso uma amostra de dimensão 20 da qual resultou uma'*
#'*variância igual a 5. Calcule a probabilidade da media amostral ser inferior a media'*
#'*populacional em mais de 1.43 unidades'*

#pop
  #Normal
#Amostra
  #n = 20
  #s^2 =5 logo s= sqrt(5) = 2.236

#P(Xˉ< μ - 1.43)

# População Normal;
# σ Desconhecido.
# D.A.: T = ((x̅ - μ) / (s / sqrt(n))) ~ t(n-1)

#T~t(20-1)
# ((μ - 1.43) - μ) / (2.236 / sqrt(20)) <=>  -1.43 / (2.236 / sqrt(20)) <=> -1.43 / 0.5 = −2.86  <=> P(T<-2.86) <=>  pt(-2.86,19) = 0.00


#####'*3.6 O tempo X, de reparação de um certo tipo de avaria, numa'*
#'*dada gama de computadores tem distribuição exponencial de media 10 dias. Para'*
#'*controlar o processo de reparação por forma a melhorar o desempenho e diminuir o'*
#'*tempo de espera dos clientes, foi recolhida uma amostra aleatória de 49 registos de'*
#'*tempos de reparação (X1, ..., X49). Calcule a probabilidade do tempo de reparação'*
#'*médio da amostra ser superior a 8'*

#Pop
  #X um certo tipo de avaria
  #exponencial
  #μ =10
  #dp = 10
#Amostra
  #n=49


A<-#Valor Media Amostra = x̅
B<-#Media população = μ
C<-#Desvio padrão População = σ
D<-#Tamanho Amostra = n
z <-(A - B) / (C / sqrt(D)) 
# (x̅ - 10) / (10 / sqrt(49))~N(0,1)  = (8 - 10) /  1.428571 = -1.4 <=> P(Z>−1.4) = 1- P(Z<=−1.4) <=> 1- pnorm(-1.4) =  0.9192


#####'*3.7Sabe-se que o nıvel de colesterol no sangue esta dependente,'*
#'*entre outras coisas, da idade das pessoas. Considere a populac˜o desses nıveis de'*
#'*colesterol em adultos com idades superiores a 15 anos, que se sabe ter distribuicao'*
#'*Normal de valor medio 275 mg/dl de sangue e desvio padrao 100 mg/dl, da qual se'*
#'*vai retirar uma amostra de dimensao 25. Considere ainda a populacao das criancas'*
#'*com idades inferiores a 15 anos, que se sabe ter uma distribuicao Normal de valor'*
#'*medio 180 mg/dl de sangue e desvio padrao 40 mg/dl, da qual se vai retirar uma'*
#'*amostra de dimensao 20, independente da anterior. Representando por X1 e X2'*
#'*as medias das amostras atras indicadas, respetivamente, calcule a probabilidade'*
#'*de:'*
  

#pop
  #X1 ~N(275,100)
  #x2 ~N(180,40)
#Amostra
  #n1=25
  #N2=20

#'*1 X̅1 ser superior a 250 mg/dl de sangue.'*


#P(x̅1 > 250) = 1- #P(x̅1 <= 250) =  1-pnorm(-1.25) =  0.8943502
#(250 - 275) / (100 / sqrt(25)) ~N(0,1) = -1.25


#'*2 X̅1 ser superior a X̅2.'*

#P(X̅2< x̅1 )  = #P(X̅2 - x̅1  < 0) 
#((X̅2 - x̅2) - (180-275)) / (sqrt( (40^2 / 20)  + (100^2 / 25) )) < (0 - (180-275)) / (sqrt( (40^2 / 20)  + (100^2 / 25) )) ~N(0,1)
#(0 - (180-275)) / (sqrt( (40^2 / 20)  + (100^2 / 25) )) <=> 95 / 21.9089 = 4.336137
#P(Z< 4.336137) = pnorm(4.336137) = 1

pnorm(4.336137)


#'*3 X̅1−X̅2 estar compreendido entre 35 mg/dl de sangue e 155 mg/dl de sangue'*

#((35- (275-180)) / 21.9089)~N(0.1) <=>  -2.738613
#((155- (275-180)) / 21.9089)~N(0.1) <=>  2.738613
 
#P(-2.73 < Z < 2.73) <=> pnorm(2.73) - pnorm(-2.73) 


#####'*3.8Suponha que temos duas populaçoes de indivıduos, a populacao 1 e a populacao 2. A populacao 1 e composta por clientes de uma agencia'*
#'*de um banco na regi˜ao central de uma cidade e a populacao 2 ´e composta por clientes de uma agencia do mesmo banco num bairro periferico da cidade. Um executivo'*
#'*do banco esta desconfiado de que as duas populacoes de clientes possuem gastos'*
#'*mensais medios com cartao de credito diferentes, acha que os clientes da agencia'*
#'*central gastam mais que os clientes da agencia periferica. Do que o executivo conhece das distribuicoes de gastos mensais com cartao de credito dos clientes das'*
#'*duas agencias, habitualmente e assumido que elas sao aproximadamente normais'*
#'*com medias e desvios padrao identicos. Com base em duas amostras aleatorias'*
#'*independentes de 15 clientes retirados das duas populacoes obtiveram-se como'*
#'*desvios padrao amostrais 182 euros em relacao `a amostra da agencia central e 165'*
#'*euros em relacao `a amostra da agencia periferica. Qual a probabilidade do executivo obter uma media amostral da agencia central que ultrapasse a media amostral'*
#'*da agencia da periferia em mais de 130 euros?'*



# População:
## Ambas Normais
## σ1 e σ2 Idênticos e Desconhecidos
## μ1 e μ2 Idênticas e Desconhecidos

# Amostra:
## na = nb = 15
## s1 = 182
## s2 = 165

# P(x̅1 > x̅2 + 130) =
# P(x̅1 - x̅2 > 130) =

# T = (((x̅1 - x̅2) - (μ1 - μ2)) / √(((1/na) + (1/nb)) * ((((na - 1) * s1^2) + ((nb - 1) * s2^2)) / (na + nb - 2))))
# T ~ t(na + nb - 2) ~ t(28)

# P( T > ((130 - 0) / sqrt(((1/15) + (1/15)) * ((((15 - 1) * 182^2) + ((15 - 1) * 165^2)) / 28))) ) =
# P(T > 2.0495) =
# 1 - P(T <= 2.0495) =
# 1 - F(2.0495) =
# 1 - pt(2.0495, 28) =
# 0.0249

#####'*3.9 Numa fabrica que produz cabos eletricos sabe-se que a proporcao de cabos defeituosos e de 0.45. Suponha que se pretende selecionar uma'*
#'*amostra aleatoria de 500 cabos dessa fabrica. Qual a probabilidade da proporcao'*
#'*de cabos defeituosos que vao calhar na amostra exceder 0.5?'*

#Pop
  # ?propoçao 0.45
#Amostra
  #n 500


#z = (p∗−p)/ sqrt ((p* * q*)/n)
#z (0.5-0.45) /( sqrt ((0.45*0.55)/500))~N(0,1) = 2.247333
#q = 1-p


#P(P^ > 0.5 ) = 1- P(Z<= 2.247333) <=> 1 - pnorm(2.247333) = 0.009211053


#####'*3.10 Suponha que esta em presenca de duas populacoes Binomiais'*
#'*onde p1 = 0.6 e p2 = 0.5. Se se retirar da primeira populacao uma amostra de 50'*
#'*observacoes e da segunda uma amostra com 40 observacoes, qual a probabilidade'*
#'*de que o desvio entre as duas proporcoes amostrais seja, em valor absoluto, superior'*
#'*a 0.2?'*


#pop
  #p1~b(n,0.6)
  #p2~b(n,0.5)
#Amostra
  #n1=50
  #n2=40





#####3.11
#'*De uma populacao Normal de variancia 64, tomou-se uma'*
#'*amostra aleatoria de dimensao 16. Qual a probabilidade da variancia amostral'*
#'*exceder 78?'*
  

#pop
  #X~N(μ,sqrt(64))
#Amostra
  #n=16

#P(s^2 > 78 ) = 1- #P(s^2 <= 78 ) <=>  1- pchisq(18.28125,15)  


#X^2 = ((n-1)S^2)/σ^2) ~X^2(n-1)
#((n-1)*78)/σ^2) ~X^2(n-1)
#((15*78)/64) ~X^2(15)<=> pchisq(18.28125,15)  

#24.1

#50.8.

#####'*3.12 Numa populacao Normal de media desconhecida e desvio'*
#'*padrao 5, calcule a probabilidade da variancia de uma amostra aleatoria de dimensao 20 dessa populacao estar compreendida entre 24.1 e 50.8.'*



#pop
  #X~N(μ,5)
#Amostra
  #n=20

#X^2 = ((n-1)S^2)/σ^2) ~X^2(n-1)

#24.1
#((19*24.1)/5^2)= 18.316
#50.8. 
#((19*50.8)/5^2) =38.608

#P( 50.8 < s^2 < 24.1) =  P(50.8 ) - p(18.316)  = pchisq(38.608,19) -pchisq(18.316,19)  = 0.4964693


#####'*3.13 Recolheu-se uma amostra de dimensão 6 de uma população'*
#'*Normal com media µ e desvio padrão σ. Determine a probabilidade da variância'*
#'*da amostra ser inferior a 3 vezes a variância da população.'*

#pop
  #X~N(μ,σ)
#Amostra
  #n=6 

#P(S^2 < 3*σ^2)

#P(((( n-1)*s^2)/σ^2) <   ((( n-1)*3σ^2)/3σ^2))
#P(((( 5)*s^2)/σ^2)   <   ((5*3σ^2)/σ^2))
#P(((( 5)*s^2)/σ^2)   <   ((5*3σ^2)/σ^2))
#P(((( 5)*s^2)/σ^2)   <   (5*3))
#P(((( 5)*s^2)/σ^2)   <   (15))
#P(X^2(5)  <   (5*3)) <=> pchisq(15,5)


#####3.14
#'*Admita a existência de duas populações nas quais sao definidas duas variaveis aleatorias X1 ∼ N (µ1, 4) e X2 ∼ N (µ2, 4) tal que'*
#'*µ1 − µ2 = −2.'*
#'*Considere que se obtém duas amostras aleatórias independentes, uma de cada'*
#'*população, com 9 e 16 elementos, respectivamente'*

#pop
  #X1~N(μ1,4)
  #X2~N(μ2,4)
  #µ1 − µ2 = -2
  #µ2 - µ1  = -2

#Amostra
  #n1=9
  #n2=16





#'*1Qual a probabilidade da media da segunda amostra exceder a media da'*
#'*primeira em mais de 3 unidades?'*
  

#P(x̅2 > x̅1 + 3) <=> #P(-3 > x̅1 - x̅2)  <=> #P( x̅1 - x̅2< -3 ) 

#P(   (((d) - (-2)) / sqrt((4^2 / 6) + (5^2 / 16))) < (((-3) - (-2)) / sqrt((4^2 / 9) + (4^2 / 16))) 
#P( Z < -0.6 ) <=> pnorm(-0.6)
 

#'*2Qual a probabilidade da variancia da primeira amostra ultrapassar o quadruplo'*
#'*da variancia da segunda amostra?'*

#P(S^2[1] > 4*S^2[2]) <=> P(S^2[1] / S^2[2]> 4)

#F(S^2[1]/S^2[2] *  (((4^2)/4) / ((4^2)/4)))

#P(F > 4)~F(8,15)<=> 1- P(F <= 4) <=> 1- pf(4, 8, 15)



