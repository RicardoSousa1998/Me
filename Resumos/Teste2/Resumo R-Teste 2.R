#'*RESUMO*

#Para ver info usar ?comando exemplo = ?c()


#'*Fractions*
# Fractions ####

library(MASS)
fractions(0.91176470588)

#'*VARIAVEIS* 
#Variaveis ####
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
#Ficheiros ####
#'*Import*
#'**

#ficheiros de texto (.txt, .csv) -> read.table(file,header=FALSE,sep="",dec=".",...) |read.csv(file,...)
#folhas de cálculo (.xls, .xlsx) -> library(readxl)  & read_excel(file.shet=null,range=null,col_name=true,...)
#Rstudio File -> Import Dataset -> From Excel...”

##Export##

#ficheiros de texto -> write.table(dados,file="dados.txt",quote=FALSE,row.names=FALSE)
#folhas de cálculo -> write.csv(dados,file="csv")

#'*TABELAS*
#Tabelas ####

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


#'*Vetor*
#Vector ####

dados$dieta #cada var é um vetor
v<c(15,14,7,8) #criar um vetor


leght(dados$deita) #tamanho do vetor 


#'*operadores lógicos no R:*
#Operadores lógicos ####
# igual : == 
# diferente: ! = 
# maior: > 
# maior ou igual: >= 
# menor: < 
# menor ou igual: <=
# pertencer: %in%
# ou: |
# e: &



#'*NA VALUES*

dados2 <-data.frame(var1=2:5,var2=c(1,NA,6,12))

any(is.na(dados2)) #verificar se existe NA values

sum(dados2$var2 ,na.rm=TRUE)  #CONTAS sem na values

dados3<- na.omit(dados2) #Retirar linhas com u ou mais na


#'*Tabelas de frequencias*
#Tabelas de frequencias ####


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
#Classes (quantitativas contınuas) ####

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

#'  Classes como intervalos semiabertos, fechados a esquerda e abertos a direita:
#'▶ c1 = [b0; b1[ com b1 = b0 + h
#'▶ c2 = [b1; b2[ com b2 = b1 + h
#'▶ c3 = [b2; b3[ com b3 = b2 + h
#'▶ c4 = [b3; b4[ com b4 = b3 + h
#'▶ ck = [bk−1; bk[ com bk = bk−1 + h
#'Se o extremo direito do ultimo intervalo for o maximo dos dados entao o ultimo
#'intervalo da tabela de frequencias e fechado a esquerda e a direita:
#'ck = [bk−1; bk] com bk = max(xi)



#'*GRAFICOS*
#Graficos####
#
#'Barras − para representar graficamente dados qualitativos ou quantitativos discretos
#'Diagramas Circulares − muito usados para representar graficamente dados qualitativos, mas tambem podem ser usados para representar dados quantitativos discretos
#'Histograma − para representar graficamente dados quantitativos agrupado sem classes, principalmente os dados quantitativos contınuos.


#'*Barras*
#Barras####


##### variável: FCVC 
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
#Circular####

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
#Histograma ####



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


# INICIO NOTA 
# NOTA: caso fosse necessário saber qual o ni de cada classe para 
#      colocar no ylim, fariamos:
(classes <- cut(obesidade$Idade, breaks=cortes, right=TRUE, include.lowest=TRUE))

# tabela de frequências
(ni.f <- table(classes))                # frexquências absolutas

# Como o maior ni é 1028, o limite do eixo dos yy foi 
# escolhido entre 0 e 200
# FIM NOTA 



#'*Medidas de localização*
#Medidas de localização ####
#Variáveis Qualitativas: Só se faz a Moda
  
#'*Central*
#'
#'*moda*
# Moda ####

#'Para dados não agrupados, a moda define-se como o valor mais frequente.
#'Para dados agrupados em classes (todas as classes com a mesma amplitude),a classe com maior frequencia diz-se a classe modal.
#'Um conjunto de dados pode não ter moda e diz-se amodal.
#'Um conjunto de dados pode ter mais que uma moda. Isto acontece quando
#'ha dois ou mais valores que tem a maior frequencia e diz-se
#'bimodal se tem duas modas;
#'multimodal ou plurimodal se tem mais do que duas modas.

DescTools::Mode(obesidade$Idade)

#'*Média*
#Media ####

mean(obesidade$Idade)

#'*Mediana*
#Mediana ####
median(obesidade$Idade, type = 2)





#'*Nao Central*
#'*Quantis*
#Quantis ####

#' 1 quartil = Q1 = Q0.25
#' 2 quartil = Q2 = Q0.50 = mediana
#' 3 quartil = Q3 = Q0.75

quantile(obesidade$Idade,prob = c(0.25, 0.50, 0.75),type = 2)

#'*Diagrama de Extremos e quartis*
#Diagrama de Extremos e quartis ####
# nível de ferro
boxplot(dados$ferro, col=4, xlab="nível de ferro", 
        horizontal=TRUE, range=0) # sem indicação de outliers
boxplot(dados$ferro, col=4, xlab="nível de ferro", 
        horizontal=TRUE, range=1.5) # indicação de outliers a partir dos moderados
boxplot(dados$ferro, col=4, xlab="nível de ferro", 
        horizontal=TRUE, range=3) # indicação de outliers a partir dos severos
# só há outliers moderados

verOut <- boxplot(dados$ferro, col=4, xlab="nível de ferro", 
                  horizontal=TRUE, range=1.5)
verOut$out  #outlier moderado


#'*Medidas de dispersão absoluta*

#'*amplitude total*
#Amplitude total ####
max(dados$suplementos)-min(dados$suplementos)
#'*amplitude interquartil*
#Amplitude interquartil ####
IQR(dados$suplementos)
#'*variância*
#variância ####
var(dados$suplementos)
#'*desvio padrão*
#Desvio padrão ####
sd(dados$suplementos)





#'*Distribuições*
#Distribuições ####



#'*Binomial*
#Binomial  ####

#X~B(n,p)  
#f(x)=P(X=x) =dbinom (x,n,p)
#F(x)=P(X≤x)= pbinom (x,n,p)
#F(k)=prob<=>P(X≤k)=prob<=>k=F^(-1) (prob)= qbinom(prob,n,p)


#'*Poisson*
#Poisson  ####
#X~P(λ)
#f(x)=P(X=x)= dpois (x,λ)
#F(x)=P(X≤x)= ppois (x,λ)
#F(k)=prob<=>P(X≤k)=prob<=>k=F^(-1) (prob)= qpois(prob,λ)


#'*Exponencial *
#Exponencial   ####
#X~Exp(θ)
#f(x)=P(X=x)= dunif (x,a,b)
#F(x)=P(X≤x)  ou P(X<x)= punif (x,a,b) = (x-a)/(b-a)
#F(k)=prob<=>P(X≤k)=prob<=>k=F^(-1) (prob)= qunif(prob,a,b)

#'*Uniforme Continua *
#Uniforme Continua   ####
#X~U(a,b),Dx=[a,b]
#f(x)=P(X=x)= dexp (x,1/θ)
#F(x)=P(X≤x)  ou P(X<x)= pexp (x,1/θ )= 1-e^(-x/θ),x≥0
#F(k)=prob<=>P(X≤k)=prob<=>k=F^(-1) (prob)= qexp(prob,1/θ)

#'*NORMAL*
#NORMAL ####
#f(x)=P(X=x)= dnorm (x,μ,σ)
#F(x)=P(X≤x)  ou P(X<x)= pnorm (x,μ,σ)  
#F(k)=prob<=>P(X≤k)=prob<=>k=F^(-1) (prob)= qnorm(prob,μ,σ)

#Normal Reduzida é so nao meter o μ,σ



#'*V[X] E E[X] VAR DISCRETA*
#V[X] E E[X] VAR DISCRETA ####


#f(1) = F(1) = 0.10
#f(2) = F(2) - F(1) = 0.3
#f(3) = F(3) - F(2) = 0.3
#f(4) = F(4) - F(3) = 0.2
#f(5) = F(5) - F(4) = 0.1

x <- c(1, 2, 3, 4,5)
f_x <- c(0.10, 0.30, 0.30 ,0.20,0.10)

MediaEx <- function(x, f_x) {
  sum_result <- sum(x * f_x) #Σ x*f(x)
  return(sum_result)
}

result <- MediaEx(x, f_x)

Media2Ex <- function(x, f_x) {
  sum_result <- sum(x^2 * f_x) #Σ x^2*f(x)
  return(sum_result)
}

result2 <- Media2Ex(x, f_x)

vX <-function(x, f_x) {
  result <- MediaEx(x, f_x)
  result2 <- Media2Ex(x, f_x)
  
  vxResult <- result2 - result^2 #E[X^2] - E^2[X]
  return(vxResult)
}



vxRES <- vX (x, f_x)






#### Variáveis Aleatórias Contínuas: ####

###### Função Densidade de Probabilidade: ######

# Na função f_densidade_probabilidade, cada ifelse representa:
## Condição: Ramo conhecido;
## Verdadeiro: Valor desse ramo;
## Falso: Novo ifelse com outro ramo, ou 0 caso seja o último ramo conhecido.
f_densidade_probabilidade <- function (x) {
  ifelse(
    "LIMITE_INFERIOR_RAMO_1" <= x & x < "LIMITE_SUPERIOR_RAMO_1",
    "VALOR_RAMO_1",
    ifelse(
      "LIMITE_INFERIOR_RAMO_2" <= x & x < "LIMITE_SUPERIOR_RAMO_2",
      "VALOR_RAMO_2",
      0 # SÓ SE FOR O ÚLTIMO RAMO!!
    )
  )
}

# E[X] - Interno:
ex_continuas <- function (x) {
  # Corresponde a: x * f(x)
  f_densidade_probabilidade(x) * x
}

# E[X^2] - Interno:
ex2_continuas <- function (x) {
  # Corresponde a: x^2 * f(x)
  f_densidade_probabilidade(x) * (x^2)
}

# Valor E[X]:
valor_ex_continuas <- function () {
  # Nas contínuas, o E[X] corresponde à integral
  # de - infinito a + infinito de x * f(x).
  integrate(ex_continuas, lower = -Inf, upper = +Inf)$value
}

# Valor E[X^2]:
valor_ex2_continuas <- function () {
  # Nas contínuas, o E[X^2] corresponde à integral
  # de - infinito a + infinito de x^2 * f(x).
  integrate(ex2_continuas, lower = -Inf, upper = +Inf)$value
}

# Valor V[X]:
valor_vx_continuas <- function () {
  # Independentemente de ser ou não contínua, o V[X] corresponde
  # à diferença entre E[X^2] e E^2[X] (os integrais são calculados nos valores
  # esperados).
  valor_ex2_continuas() - (valor_ex_continuas()^2)
}

###### E[X]: ######
valor_ex_continuas()

###### E[X^2]: ######
valor_ex2_continuas()

###### V[X]: ######
valor_vx_continuas()




#'*Primitivas*
#Primitivas ####

#P(x^k) = (n^(k+1)) / k+1
#P(k) = kx



# Instalar o pacote Ryacas0 se ainda não estiver instalado
if (!require(Ryacas0)) install.packages("Ryacas0")

# Carregar o pacote Ryacas0
library(Ryacas0)

x <- Sym("x")  # Define 'x' como uma variável simbólica
expr <- Integrate(x, x)  # Calcula a integral de x com relação a x

expr






#'*Derivadas*
#Derivadas ####

# Instalar o pacote Deriv se ainda não estiver instalado
if (!require(Deriv)) install.packages("Deriv")

# Carregar o pacote Deriv
library(Deriv)

f <- function(x) (x^2)/2 + (1/3)

# Calcular a derivada de f em relação a x
f_prime <- Deriv(f, "x")

# Mostrar a função derivada
f_prime


#'*integrate*
#integrate####

f <- function(y) {
  (1/40)-(y/3200)
}

integrate(f, lower=60, upper=80)$value + 0  # 0.0625







###########################
######### TESTE 2 #########
###########################





#'*Distribuições Teste2*
#Distribuições Teste 2####


#'*Qui-Quadrado*
#Qui-Quadrado
#X~x^2(n)
#f(x)=P(X=X) = dchisq(x,n)
#F(x)=P(X<=x) = pchisq(x,n)
#F(k)=prob<=>P(X≤k)=prob<=>k=F^(-1) (prob)= qchisq(prob,n)

#'* t de Student*
# t de Student
#X~t(n)
#f(x)=P(X=X) = dt(x,n)
#F(x)=P(X<=x) =  pt(x,n)
#F(k)=prob<=>P(X≤k)=prob<=>k=F^(-1) (prob)= qt(prob, n)


#'* F de Snedecor*
# F de Snedecor
#x ∼ F(m,n) 
#f(x)=P(X=X) = df(x, m, n)
#F(x)=P(X<=x) =  pf(x, m, n)
#F(k)=prob<=>P(X≤k)=prob<=>k=F^(-1) (prob)= qf(prob, m, n)


#'*Distribuições Amostrais, Intervalos de Confiança e Testes de Hipóteses Paramétricos*
#### Distribuições Amostrais, Intervalos de Confiança e Testes de Hipóteses Paramétricos ####

###### Para a Média: ######

# População Normal;
# σ Conhecido.
 
# D.A.: Z = ((x̅ - μ) / (σ / sqrt(n))) ~ N(0, 1)
A<-#Valor Media Amostra = x̅
B<-#Media população = μ
C<-#Desvio padrão População = σ
D<-#Tamanho Amostra = n
z <-(A - B) / (C / sqrt(D)) 

# I.C.: ] x̅ - (z_(1 - (α/2))) * (σ / sqrt(n)) , x̅ + (z_(1 - (α/2))) * (σ / sqrt(n)) [
BSDA::z.test()

# População Normal;
# σ Desconhecido.
# D.A.: T = ((x̅ - μ) / (s / sqrt(n))) ~ t(n-1)
# I.C.: ] x̅ - (t_(1 - (α/2)); n-1) * (s / sqrt(n)) , x̅ + (t_(1 - (α/2)); n-1) * (s / sqrt(n)) [
t.test()

# População Qualquer;
# σ Conhecido;
# n >= 30.
# D.A.: Z = ((x̅ - μ) / (σ / sqrt(n))) ~ N(0, 1)
# I.C.: ] x̅ - (z_(1 - (α/2))) * (σ / sqrt(n)) , x̅ + (z_(1 - (α/2))) * (σ / sqrt(n)) [
BSDA::z.test()

# População Qualquer;
# σ Desconhecido;
# n >= 30.
# D.A.: Z = ((x̅ - μ) / (s / sqrt(n))) ~ N(0, 1)
# I.C.: ] x̅ - (z_(1 - (α/2))) * (s / sqrt(n)) , x̅ + (z_(1 - (α/2))) * (s / sqrt(n)) [
BSDA::z.test()

"-------------------------------"

###### Para a Diferença de 2 Médias: ######

# Populações Normais;
# σ1 e σ2 Conhecidos;
# Amostras Independentes.
# D.A.: Z = (((x̅1 - x̅2) - (μ1 - μ2)) / sqrt((σ1^2 / n1) + (σ2^2 / n2))) ~ N(0, 1)
# I.C.: ] (x̅1 - x̅2) |-+| z_(1 - (α/2)) * sqrt((σ1^2 / n1) + (σ2^2 / n2))) [
BSDA::z.test()

# Populações Normais;
# σ1 e σ2 Desconhecidos;
# σ1 = σ2;
# Amostras Independentes.
# D.A.: T = (((x̅1 - x̅2) - (μ1 - μ2)) / sqrt(((1 / n1) + (1 / n2)) * ((((n1 - 1) * s1^2) + ((n2 - 1) * s2^2)) / (n1 + (n2 - 2)))))
# T ~ t(n1 + (n2 - 2))
# I.C.: ] (x̅1 - x̅2) |-+| t_(1 - (α/2); n1 + (n2 - 2)) * sqrt(((1 / n1) + (1 / n2)) * ((((n1 - 1) * s1^2) + ((n2 - 1) * s2^2)) / (n1 + (n2 - 2)))) [
t.test()

# Populações Normais;
# σ1 e σ2 Desconhecidos;
# σ1 != σ2;
# Amostras Independentes.
# D.A.: T = (((x̅1 - x̅2) - (μ1 - μ2)) / sqrt((s1^2 / n1) + (s2^2 / n2))) ~ t(gl2)
# I.C.: ] (x̅1 - x̅2) |-+| t_(1 - (α/2); gl2) * sqrt((s1^2 / n1) + (s2^2 / n2))) [
t.test()
## gl2 ~=~ ((s1^2 / n1) + (s2^2 / n2))^2 / ((s1^4 / (n1^2 * (n1 - 1))) + (s2^4 / (n2^2 * (n2 - 1))))
## Para gl2: Considera-se o inteiro mais próximo ou faz-se a correção de Welch-Satterthwaite.

# Populações Quaiquer;
# σ1 e σ2 Conhecidos;
# Amostras Independentes;
# n1 e n2 >= 30.
# D.A.: Z = (((x̅1 - x̅2) - (μ1 - μ2)) / sqrt((σ1^2 / n1) + (σ2^2 / n2))) ~ N(0, 1)
# I.C.: ] (x̅1 - x̅2) |-+| z_(1 - (α/2)) * sqrt((σ1^2 / n1) + (σ2^2 / n2))) [
BSDA::z.test()

# Populações Quaiquer;
# σ1 e σ2 Desconhecidos;
# Amostras Independentes;
# n1 e n2 >= 30.
# D.A.: Z = (((x̅1 - x̅2) - (μ1 - μ2)) / sqrt((s1^2 / n1) + (s2^2 / n2))) ~ N(0, 1)
# I.C.: ] (x̅1 - x̅2) |-+| z_(1 - (α/2)) * sqrt((s1^2 / n1) + (s2^2 / n2))) [
BSDA::z.test()


"-------------------------------"

###### Para a Variância: ######

# População Normal.
# D.A.: X^2 = (((n-1) * s^2) / σ^2) ~ X^2(n-1)
# I.C.: ] (((n-1) * s^2) / x^2_(1 - (α/2); n-1)) , (((n-1) * s^2) / x^2_(α/2; n-1)) [
EnvStats::varTest()

"-------------------------------"

###### Para o Quociente de 2 Variâncias: ######

# Populações Normais;
# Amostras Independentes.
# D.A.: F = ((s1^2 / s2^2) * (σ2^2 / σ1^2)) ~ F(n1 - 1, n2 - 1)
# I.C.: ] ((1 / f_(1 - (α/2); n1 - 1; n2 - 1)) * (s1^2 / s2^2)) , 
# I.C.:   ((1 / f_(α/2; n1 - 1; n2 - 1)) * (s1^2 / s2^2)) [
var.test()

"-------------------------------"

###### Para a Proporção: ######

# n >= 30.
# D.A.: Z = ((p* - p) / sqrt(pq / n)) ~=~ ((p* - p) / sqrt((p* * q*) / n)) ~ N(0, 1)
# I.C.: ] p* |-+| z_(1 - (α/2)) * sqrt((p* * q*) / n) [
BSDA::z.test()

"-------------------------------"

###### Para a Diferença de 2 Proporções: ######

# Amostras Independentes;
# n1 & n2 >= 30.
# D.A.: Z = (((p1* - p2*) - (p1 - p2)) / sqrt(((p1 * q1) / (n1)) + ((p2 * q2) / (n2))))
# D.A.: Z ~=~ (((p1* - p2*) - (p1 - p2)) / sqrt(((p1* * q1*) / (n1)) + ((p2* * q2*) / (n2))))
# D.A.: z ~ N(0, 1)
# I.C.: ] (p1* - p2*) |-+| z_(1 - (α/2)) * sqrt(((p1* * q1*) / (n1)) + ((p2* * q2*) / (n2))) [
BSDA::z.test()

"-------------------------------"

###### Testes de Ajustamento: ######

# Distribuição Discreta ou Contínua com Classes:
# Qui-Quadrado
chisq.test()

# Distribuição Contínua Completamente Especificada:
# Kolmogorov-Smirnov
ks.test()

# Normal e n >= 50:
# Lilliefors
nortest::lillie.test()

# Normal e n < 50:
# Shapiro Wilk
shapiro.test()
