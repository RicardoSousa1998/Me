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

PS3B98VBGHRT
#####3.3
A<-0.49
B<-0.5
C<-0.02
D<-10

z <-(A - B) / (C / sqrt(D))

1-pnorm(z)


