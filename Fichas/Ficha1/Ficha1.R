  
#1
#(a) Quantitativa Contínua
#(b) Qualitativa Nominal
#(c) Qualitativa Ordinal
#(d) Qualitativa Nominal
#(e) Quantitativa Contínua
#(f) Quantitativa Discreta
#(g) Quantitativa Discreta


#2

#2(a)
#Popula?ao:Hoteis na europa
#Amostra:12 hoteis 7
#Unidade Estatistica : Cada hotel
#Nome do Hotel: Qualitativa Nominal
#País: Qualitativa Nominal
#Preço do quarto: Qualitativa Ordinal
#Número de quartos:Quantitativa Discreta
#Pontuação (0 - 10): Quantitativa Contínua


#2(b)



nome <- c("Hotel Ronda", "Villad'Este", "Hotel Lisboa", "Hotel Prem", "Hotel d'Europa", "Palace Luzern", "Hotel Palace", "Hotel Arts", "Hotel Sacher", "Duc de Bourgogne", "Villa Gallici", "Hotel Vila")
pais <- c("Espanha", "It?lia", "Portugal", "Alemanha", "Fran?a", "Fran?a", "Portugal", "Espanha", "Alemanha", "Fran?a", "Fran?a", "Portugal")
preco <- c("$$", "$$$$", "$", "$", "$$", "$$", "$$$$", "$$$", "$$$", "$", "$$", "$$")
num_quartos <- c(18, 166, 81, 54, 47, 326, 185, 45, 120, 10, 22, 233)
pontuacao <- c(8.4, 8.6, 8.5, 7.7, 7.6, 8.1, 9.5, 7.3, 8.5, 7.6, 9.0, 9.1)

hotel_df <- data.frame(nome = nome,
                       pais = pais,
                       preco = preco,
                       num_quartos = num_quartos,
                       pontuacao = pontuacao)

#2(c) 
#R:12
nrow(hotel_df)


#2 (D)

tabela_pais<- DescTools::Freq(hotel_df$pais)
print(tabela_pais)


(ni.preco <- table(hotel_df$preco))          # Frequências Absolutas
(fi.preco <- round(prop.table(ni.preco), 4))  # frequências Relativas
(Ni.preco <- cumsum(ni.preco))                # frequências Absolutas Acumuladas
(Fi.preco <- round(cumsum(fi.preco), 4))      # frequências Relativas Acumuladas

(tabela_preco <- data.frame(
  i = 1:nrow(ni.preco),
  xi = names(ni.preco),
  ni = as.integer(ni.preco),
  fi = as.numeric(fi.preco),
  Ni = as.integer(Ni.preco),
  Fi = as.numeric(Fi.preco)
))

print(tabela_preco)

#2 (e)

nrow(hotel_df[hotel_df$pais %in% c("Espanha", "Portugal"), ])


#2 (f)


(sum(nchar(hotel_df$preco) == max(nchar(hotel_df$preco)))/ nrow(hotel_df))*100


#3

#3(a)
#Popula??o: Os 829 casos de acidente vascular cerebral (AVC) que ocorreram na Est?nia durante o per?odo de 1991-1993.

#Unidade estat?stica: Cada caso individual de AVC dentro da amostra de 829 casos.


#3(b)

#popula?ao : 829
#amostra: 285
nrow(stroke)

#3 (c)
#Sex: Qualitativa Nominal
#Age: Quantitativa Contínua
#DGB: Qualitativa Nominal
#Coma: Qualitativa Nominal
#Diab:Qualitativa Nominal
#Dead:Qualitativa Nominal
#Obsmonths:Quantitativa Contínua

#3 (d)
DescTools::Freq(stroke$sex)
DescTools::Freq(stroke$dgn)
DescTools::Freq(stroke$coma)
DescTools::Freq(stroke$diab)
DescTools::Freq(stroke$dead)


#3 (e)
nrow(stroke[stroke$dgn=="ID",])

#3 (f)

(nrow(stroke[stroke$dead=="TRUE",])/ nrow(stroke))*100

#3 (g)
nrow(stroke[stroke$coma==1 & stroke$diab=="YES",])

#3 (h)

nrow(stroke[stroke$obsmonths==0.1,])

#4

#4 (a)

#Genero: Qualitativa Nominal
#Idade: Quantitativa Contínua
#Altura: Quantitativa Contínua
#Peso: Quantitativa Contínua
#FAVC: Qualitativa Nominal
#FCVC: Qualitativa Ordinal
#NCP: Quantitativa Discreta
#CAEC: Qualitativa Nominal
#Fumar: Qualitativa Nominal
#CH2O: Qualitativa Ordinal
#FAF: Qualitativa Ordinal
#CALC: Qualitativa Nominal
#MTRANS: Qualitativa Nominal

#4 (b)

#R:2111
nrow(obesidade)

#4 (c)
#R:88% +/-
(nrow(obesidade[obesidade$FAVC==1,])/ nrow(obesidade))*100

#4 (d)
#R:97% +/-

(nrow(obesidade[obesidade$CAEC %in% c("S", "F","A"), ])/ nrow(obesidade))*100

#4 (e)
#R:75%
(nrow(obesidade[obesidade$MTRANS %in% c("Transportes_Publicos", "Bicicleta"), ])/ nrow(obesidade))*100


#4 (f)

DescTools::Freq(obesidade$NCP)


#4 (g)

#i.


k1 <- 8   # 8 classes
h1 <- 6   # amplitude 6 anos

# mínimo e máximo das classes
valor.min1 <- 14   # primeira classe a começar nos 14 anos
(valor.max1 <- valor.min1 + h1*k1)

#extremos das classes
(cortes1 <- seq(valor.min1, valor.max1, by=h1))

# intervalos abertos à esquerda e fechados à direita
# como o mínimo dos dados = ao primeiro valor da primeira classe
# a primeira classe tem de ser fechada dos dois lados
(classes1 <- cut(obesidade$Idade, breaks=cortes1, right=TRUE, include.lowest=TRUE))

# tabela de frequências
(ni.hi <- table(classes1))              # frequências absolutas
(fi.hi <- round(prop.table(ni.hi),4))   # frequências relativas
(Ni.hi <- cumsum(ni.hi))                # frequências absolutas acumuladas
(Fi.hi <- round(cumsum(fi.hi),4))       # frequências relativas acumuladas

(tabela.frequencias.Idade <- data.frame(i=1:nrow(ni.hi),
                                        xi=names(ni.hi),
                                        ni=as.integer(ni.hi),
                                        fi=as.numeric(fi.hi),
                                        Ni=as.integer(Ni.hi),
                                        Fi=as.numeric(Fi.hi)))


# 3 (g) ii.


#extremos das classes
cortes2 <- c(1.45,1.60,1.80,2.00)

# intervalos abertos à esquerda e fechados à direita
# a primeira classe fechada dos dois lados
(classes2 <- cut(obesidade$Altura, breaks=cortes2, right=TRUE, include.lowest=TRUE))

# tabela de frequências
(ni.hii <- table(classes2))               # frequências absolutas
(fi.hii <- round(prop.table(ni.hii),4))   # frequências relativas
(Ni.hii <- cumsum(ni.hii))                # frequências absolutas acumuladas
(Fi.hii <- round(cumsum(fi.hii),4))       # frequências relativas acumuladas

(tabela.frequencias.Altura <- data.frame(i=1:nrow(ni.hii),
                                         xi=names(ni.hii),
                                         ni=as.integer(ni.hii),
                                         fi=as.numeric(fi.hii),
                                         Ni=as.integer(Ni.hii),
                                         Fi=as.numeric(Fi.hii)))


##############################################################
# 3 (g) iii.


(k3<-trunc(1+log(n)/log(2)))   # número de classes
(h3 <-  (max(obesidade$Peso)-min(obesidade$Peso))/k3)   # amplitude das classes

# mínimo e máximo das classes
(valor.min3 <- min(obesidade$Peso))
(valor.max3 <- valor.min3 + h3*k3)

#extremos das classes
(cortes3 <- seq(valor.min3, valor.max3, by=h3))

# intervalos fechados à esquerda e abertos à direita
# como o maximo dos dados é igual ao último valor da última classe
# a última classe tem de ser fechada nos dois lados
(classes3 <- cut(obesidade$Peso, breaks=cortes3, right=FALSE, include.lowest=TRUE))

# tabela de frequências
(ni.hiii <- table(classes3))                # frequências absolutas
(fi.hiii <- round(prop.table(ni.hiii),4))   # frequências relativas
(Ni.hiii <- cumsum(ni.hiii))                # frequências absolutas acumuladas
(Fi.hiii <- round(cumsum(fi.hiii),4))       # frequências relativas acumuladas

(tabela.frequencias.Peso <- data.frame(i=1:nrow(ni.hiii),
                                       xi=names(ni.hiii),
                                       ni=as.integer(ni.hiii),
                                       fi=as.numeric(fi.hiii),
                                       Ni=as.integer(Ni.hiii),
                                       Fi=as.numeric(Fi.hiii)))


#4 (h)
#R:17
nrow(obesidade[obesidade$Idade>44,])

#4 (i)
#R:84% +/-
(nrow(obesidade[obesidade$Altura<1.80,])/ nrow(obesidade))*100
