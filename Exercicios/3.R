#####3.1


#1
z1 <- (30 - 31) / (2.4 / sqrt(36))
pnorm(z1)



#2
z2 <- (32 - 31) / (2.4 / sqrt(36))
pnorm(z2)-pnorm(z1)


#Maneiora dif 2

#como normal é simetrica  e z1 e z2 serem parecidos   (-2.5 & 2.5 )
#podemos fazer 
1 - (pnorm(z1)+pnorm(z1))
#

#Formula Geral Para Media com:
# População Normal;
# σ Conhecido.
A<-#Valor Media Amostra = x̅
B<-#Media populaçao = μ
C<-#Desvio padrao Populaçao = σ
D<-#Tamanho Amostra = n
  
z <-(A - B) / (C / sqrt(D))


#####3.3
A<-0.49
B<-0.5
C<-0.02
D<-10

z <-(A - B) / (C / sqrt(D))

1-pnorm(z)



#####3.4
#Popu
  #normal
  #media 17 minutos
  #dp 5

#Amostra


#1
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

#2
# População Qualquer;
# σ Conhecido;

A<-#Valor Media Amostra = x̅
B<-#Media população = μ
C<-#Desvio padrão População = σ
D<-#Tamanho Amostra = n
 z <-(A - B) / (C / sqrt(D)) 

z1 <-(16 - 17) / (5 / sqrt(100)) 
z2 <-(18 - 17) / (5 / sqrt(100)) 

#P(16< Xˉ <18) = P(16) - P(18) = pnorm(z2) - pnorm(z1) = 0,9544 ou seja 95%
#Fica igual

#3





#####3.5

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


#####3.6

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


#####3.7

#pop
  #X1 ~N(275,100)
  #x2 ~N(180,40)
#Amostra
  #n1=25
  #N2=20

#1

#P(x̅1 > 250) = 1- #P(x̅1 <= 250) =  1-pnorm(-1.25) =  0.8943502
#(250 - 275) / (100 / sqrt(25)) ~N(0,1) = -1.25


#2
#P(X̅2< x̅1 )  = #P(X̅2 - x̅1  <0) 
#((X̅2 - x̅2) - (180-275)) / (sqrt( (40^2 / 20)  + (100^2 / 25) )) < (0 - (180-275)) / (sqrt( (40^2 / 20)  + (100^2 / 25) )) ~N(0,1)
#(0 - (180-275)) / (sqrt( (40^2 / 20)  + (100^2 / 25) )) <=> 95 / 21.9089 = 4.336137
#P(Z< 4.336137) = pnorm(4.336137) = 1

pnorm(4.336137)


#3

#((35- (275-180)) / 21.9089)~N(0.1) <=>  -2.738613
#((155- (275-180)) / 21.9089)~N(0.1) <=>  2.738613
 
#P(-2.73 < Z < 2.73) <=> pnorm(2.73) - pnorm(-2.73) 


#####3.8
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

#####3.9

#####3.10

#####3.11

#####3.12

#####3.13

#####3.14

#1

#2





