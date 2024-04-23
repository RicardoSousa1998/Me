#'*RESUMO*

#Para ver info usar ?comando exemplo = ?c()


#Fractions 

library(MASS)
fractions(0.91176470588)

#'*VARIAVEIS* 
#Qualitativos Nominal :a ordem das categorias não tem significado
#Qualitativos Ordinal : ha uma ordem natural das categorias
#Quantitativos Discreta :  os valores podem ordenar-se, mas entre dois valores consecutivos nao pode existir um valor intermedio
#Quantitativas Contınua: pode tomar qualquer valor num certo intervalo (mediçoes)

#Vector -> c()
genero <- c("feminino", "masculino")
#Matriz -> matrix()
#Array -> array()
#Lista -> list()
#Tabela -> data.frame()
tabela <- data.frame(genero = genero,idade=c(20,18))

#'*FICHEIROS* 
#'*Import*
#'**

#ficheiros de texto (.txt, .csv) -> read.table(file,header=FALSE,sep="",dec=".",...) |read.csv(file,...)
#folhas de cálculo (.xls, .xlsx) -> library(readxl)  & read_excel(file.shet=null,range=null,col_name=true,...)
#Rstudio File -> Import Dataset -> From Excel...”

##Export##

#ficheiros de texto -> write.table(dados,file="dados.txt",quote=FALSE,row.names=FALSE)
#folhas de cálculo -> write.csv(dados,file="csv")

#'*TABELAS*

dados <- data.frame(
  i = 1:10,
  dieta = c("Sim", "Sim", "Não", "Sim", "Sim", "Não", "Sim", "Não", "Não", "Sim"),
  intensidade = c("Moderada", "Elevada", "Baixa", "Moderada", "Elevada", "Baixa", "Baixa", "Baixa", "Elevada", "Baixa"),
  suplementos = c(3, 2, 5, 6, 6, 3, 4, 4, 7, 3),
  ferro = c(14.3, 7.8, 27.0, 11.0, 9.9, 14.5, 15.4, 20.8, 10.5, 15.9)
)

#'*informaÇão da tabela*

view(dados) #Ver Tabela
head(dados) #ver 1ºs linhas
tail(dados) #ver ultimas linhas
names(dados) #ver nome dos vars
str(dados) #ver estratura da tabela
dim(dados) #nº linhas e colunas
nrow(dados) #nº linhas
ncol(dados) #nº colunas

#'*ver dados*

dados[1,2] #elemento linha 1 col 2
dados[1,] #linha 1
dados[,2] #coluna 2
dados[,c(2,4)] #coluna 2 e coluna 4
dados[,2:4] #coluna 2,3,4
dados[c(2,4),] #linha 2 e 4
dados[2:4] #linha 2,3,4


dados$dieta #variavel dieta
dados[,c("deita","ferro")]  #variaveis dieta e ferro

dados[dados$dieta=="sim",] #where dieta = sim
dados[dados$suplementos>3,] #where suplementos >3

#'*operadores lógicos no R:*
# igual : == 
# diferente: ! = 
# maior: > 
# maior ou igual: >= 
# menor: < 
# menor ou igual: <=
# pertencer: %in%
# ou: |
# e: &

#'*Vetor*

dados$dieta #cada var é um vetor
v<c(15,14,7,8) #criar um vetor


leght(dados$deita) #tamanho do vetor 


#'*NA VALUES*

dados2 <-data.frame(var1=2:5,var2=c(1,NA,6,12))

any(is.na(dados2)) #verificar se existe NA values

sum(dados2$var2 ,na.rm=TRUE)  #CONTAS sem na values

dados3<- na.omit(dados2) #Retirar linhas com u ou mais na


#'*Tabelas de frequencias*


DescTools::Freq(obesidade$NCP)

#ou


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

#'*Classes (quantitativas contınuas)*

#Regra de Sturges

#k = ⌊1 + log2 n⌋ (  =)⌊ 1(+(ln n)/ln 2⌋
#n = numero de elementos numa lista 


#amplitude da classe

H= (max(xi)-min(xi))/k


#Classes como intervalos semiabertos, abertos a esquerda e fechados a direita:
#c1 =]b0; b1] com b1 = b0 + h
#▶ c2 =]b1; b2] com b2 = b1 + h
#▶ c3 =]b2; b3] com b3 = b2 + h
#▶ c4 =]b3; b4] com b4 = b3 + h
#▶ ck =]bk−1; bk] com bk = bk−1 + h
#'Se o extremo esquerdo do primeiro intervalo for o mınimo dos dados entao o
#primeiro intervalo da tabela de frequencias e fechado a esquerda e a direita:
#'  c1 = [b0; b1] com b0 = min(xi)

#'  Classes como intervalos semiabertos, fechados `a esquerda e abertos `a direita:
#'▶ c1 = [b0; b1[ com b1 = b0 + h
#'▶ c2 = [b1; b2[ com b2 = b1 + h
#'▶ c3 = [b2; b3[ com b3 = b2 + h
#'▶ c4 = [b3; b4[ com b4 = b3 + h
#'▶ ck = [bk−1; bk[ com bk = bk−1 + h
#'Se o extremo direito do ultimo intervalo for o maximo dos dados entao o ultimo
#'intervalo da tabela de frequencias e fechado a esquerda e a direita:
#'ck = [bk−1; bk] com bk = max(xi)



#'*GRAFICOS*
#'
#'Barras − para representar graficamente dados qualitativos ou quantitativos discretos
#'Diagramas Circulares − muito usados para representar graficamente dados qualitativos, mas tambem podem ser usados para representar dados quantitativos discretos
#'Histograma − para representar graficamente dados quantitativos agrupadosem classes, principalmente os dados quantitativos contınuos.


#'*Barras*


##### variável: FCVC #####
# Se come habitualmente vegetais nas refeições: 
#        1= Nunca, 2=As vezes, 3=Sempre

(ni.FCVC <- table(obesidade$FCVC)) # frequências absolutas

#gráfico de barras das frequências absolutas
barplot(ni.FCVC, main="Se come habitualmente vegetais nas refeições", 
        xlab="FCVC", ylab="Frequências absolutas", 
        col=3:5, ylim=c(0,1200), names.arg=c("Nunca", "Às vezes", "Sempre"))


# variável CH2O

(ni.CH2O <- table(obesidade$CH2O))          # frequências absolutas
(fi.CH2O <- round(prop.table(ni.CH2O),4))   # frequências relativas

#gráfico de barras das frequências relativas
barplot(fi.CH2O, 
        main="Quantidade de água que bebe diariamente",
        xlab="CH2O", 
        ylab="Frequências relativas", 
        col=13:15, ylim=c(0,0.6), 
        names.arg=c("< 1 litro", "1 - 2 litros", "> 2 litros"))




#'*Circular*

#################
# variável Género

(ni.G <- table(obesidade$Genero))     # frequências absolutas
(fi.G <- round(prop.table(ni.G),4))   # frequências relativas

pie(ni.G, labels=paste(fi.G*100, "%"), 
    col=c("yellow", "lightblue"), 
    main="Género")

legend("topleft", 
       legend=names(ni.G), 
       fill=c("yellow", "lightblue"),
       cex = 0.7)


#'*hISTOGRAMA*



#'# variável estatística: Idade

# mínimo e máximo dos dados
min(obesidade$Idade)
max(obesidade$Idade)

k <- 8   # 8 classes
h <- 6   # amplitude 6 anos

# mínimo e máximo das classes
valor.min <- 14   # primeira classe a começar nos 14 anos
(valor.max <- valor.min + h*k)

#extremos das classes
(cortes <- seq(valor.min, valor.max, by=h))

# intervalos abertos à esquerda e fechados à direita
# como o mínimo dos dados = ao primeiro valor da primeira classe,
# a primeira classe tem de ser fechada dos dois lados

# histograma -> eixo dos yy -> frequências absolutas
hist(obesidade$Idade, breaks=cortes, right=TRUE, include.lowest=TRUE,
     freq=TRUE,
     main="Histograma",
     xlab="Idade",
     ylab="frequências absolutas",
     col=2,
     xlim=c(0,70),
     ylim=c(0,1200),
     xaxt="n")                  # para poder definir o eixo dos xx como pretendermos
axis(side=1, at=c(0,cortes,70))  # definir os valores para o eixo dos xx igual às classes


# INICIO NOTA ------------------------------------------
# NOTA: caso fosse necessário saber qual o ni de cada classe para 
#      colocar no ylim, fariamos:
(classes <- cut(obesidade$Idade, breaks=cortes, right=TRUE, include.lowest=TRUE))

# tabela de frequências
(ni.f <- table(classes))                # frexquências absolutas

# Como o maior ni é 1028, o limite do eixo dos yy foi 
# escolhido entre 0 e 200
# FIM NOTA -----------------------------------------



#'*Medidas de localização*

#'*Central*
#'moda
#'Média
#'Meaida

#'*Nao Central*
#'Quantis




#'*Derivadas*

# Instalar o pacote Deriv
install.packages("Deriv")

# Carregar o pacote Deriv
library(Deriv)

f <- function(x) (x^2)/2 + (1/3)

# Calcular a derivada de f em relação a x
f_prime <- Deriv(f, "x")

# Mostrar a função derivada
f_prime



###verificar isto
library(ggplot2)
a <- data.frame(x,p_x) #cria um dataframe para usar ggplot

ggplot(a, aes(x=x, y=p_x)) + geom_bar(stat="identity",fill="steelblue")+
  ggtitle('Distribuição Binomial - Sangue Tipo A')+
  geom_text(aes(label=p_x), vjust=1.6, color="white", size=3.5)
