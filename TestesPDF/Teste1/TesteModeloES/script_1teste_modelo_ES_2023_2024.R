########################
#  1.º TESTE (MODELO)  #
########################

#EXERCÍCIO 1

# 1 (c)

# se quiser fazer a tabela de fequências no R
# podia ser feita apenas na folha de teste

#amostra
PesoB <- c(rep("[0,5[", 5), rep("[5,10[", 5), rep("[10,15[", 20),
           rep("[15,20[", 30), rep("[20,25[", 20), rep("[25,30]", 10))

#definir a ordem das classes
PesoB <- factor(PesoB, levels=c("[0,5[", "[5,10[", "[10,15[", "[15,20[", "[20,25[", "[25,30]"))

# frequências absolutas
(ni.pb <- table(PesoB))
# frequências relativas
(fi.pb <- prop.table(ni.pb))
# frequências absolutas acumuladas
(Ni.pb <- cumsum(ni.pb))
# frequências relativas acumuladas
(Fi.pb <- cumsum(fi.pb))

#tabela de frequências
data.frame(xi=names(ni.pb),
           ni=as.integer(ni.pb),
           fi=round(as.numeric(fi.pb),3),
           Ni=as.integer(Ni.pb),
           Fi=round(as.numeric(Fi.pb),3))


##############################################################
##############################################################
##############################################################
##############################################################
##############################################################
##############################################################

# EXERCÍCIO 2

# dados
#ler os dados do ficheiro: File -> Import Datset -> From Text

titanic0
str(titanic0)

##############################################################
##############################################################

# 2 (a)

# dimensão da amostra

nrow(titanic0)


##############################################################
##############################################################

# 2 (b)

# passageiros que sobreviveram

sobreviventes <- titanic0[titanic0$Survived==1,]
nrow(sobreviventes)

# tabela de frequências da variável Sex
# frequências absolutas
(ni.s <- table(sobreviventes$Sex))
# frequências relativas em percentagem
fi.s <- prop.table(ni.s)
round(fi.s*100,2)

# representação gráfica
# gráfico de barras (também podia ser um diagrama circular)
barplot(fi.s, col=c("green", "red"), main="Sobreviventes por Género", 
        names.arg=c("feminino", "masculino"), ylab="frequências relativas", ylim=c(0,1))


##############################################################
##############################################################

# 2 (c)

# extremos dos dados idade
# mínimo
min(titanic0$Age)
# máximo
max(titanic0$Age)

# número de classes
k <- 4

# classes
classes.idade <- cut(titanic0$Age, breaks=c(0,18,25,65,80), right=TRUE)

# tabela de frequências da variável Age
# frequências absolutas
(ni.idade <- table(classes.idade))
# frequências relativas
(fi.idade <- round(prop.table(ni.idade),3))
# frequências absolutas acumuladas
(Ni.idade <- cumsum(ni.idade))
# frequências relativas acumuladas
(Fi.idade <- cumsum(fi.idade))

(tabela.freq.idade <- data.frame(i=1:nrow(ni.idade),
                                xi=c("crianças", "jovens","adultos","idosos"),
                                classes=names(ni.idade),
                                ni=as.integer(ni.idade),
                                fi=as.numeric(fi.idade),
                                Ni=as.integer(Ni.idade),
                                Fi=as.numeric(Fi.idade)))

# representação gráfica
# histograma -> eixo dos yy -> fi/h  -> as classes têm amplitudes diferentes
hist(titanic0$Age, breaks=c(0,18,25,65,80), right=TRUE,
     freq=FALSE,
     main="Idade dos passageiros",
     xlab="anos",
     ylab="frequências relativas / anos",
     col=4,
     xlim=c(0,80),
     ylim=c(0,0.04),
     xaxt="n")                  
axis(side=1, at=c(0,18,25,65,80))


##############################################################
##############################################################

# 2 (d)

#medidas de localização central
# moda
tab <- table(titanic0$Fare)
names(tab)[tab==max(tab)]

# média
mean(titanic0$Fare)
# mediana
median(titanic0$Fare, type=2)

#medidas de dispersão
# amplitude total
max(titanic0$Fare)-min(titanic0$Fare)
# amplitude interquartil
IQR(titanic0$Fare, type=2)
# variância
var(titanic0$Fare)
# desvio padrão
sd(titanic0$Fare)
# coeficiente de variáção
(sd(titanic0$Fare)/mean(titanic0$Fare))*100

# diagrama de extremos e quartis
boxplot(titanic0$Fare, col="yellow", horizontal=TRUE, xlab="libras", main="Preço da passagem")

# outliers
ver.m <- boxplot(titanic0$Fare, range=1.5)
ver.s <- boxplot(titanic0$Fare, range=3)
# número de "ouliers"
length(ver.m$out)
# número "ouliers" severos
length(ver.s$out)
# número de "ouliers" moderados
length(ver.m$out)-length(ver.s$out)

##############################################################
##############################################################
##############################################################
##############################################################
##############################################################
##############################################################

# EXERCÍCIO 3

# 3 (c)
# calcular os integrais

f2 <- function(y){y^2*(y-1)}
f3 <- function(y){y^2*(3-y)}

#E[X^2]
0+integrate(f2, lower=1, upper=2)$value + integrate(f3, lower=2, upper=3)$value+0


##############################################################
##############################################################
##############################################################
##############################################################
##############################################################
##############################################################

# EXERCÍCIO 4

# 4 (a)
#probabilidade
pbinom(1,20,0.1)
#resultado final
1-pbinom(1,20,0.1)

# 4 (b)

#se não quiser ver os números em notação científica, colocar 
#options(scipen=999)

#probabilidade
ppois(4,18)
#resultado final
1-ppois(4,18)

# 4 (c) (i)
#probabilidades
pnorm(27.472,25.6,1.6)
pnorm(24.256,25.6,1.6)
#resultado final
pnorm(27.472,25.6,1.6)-pnorm(24.256,25.6,1.6)

# 4 (c) (ii)
qnorm(0.0179,25.6,1.6)
