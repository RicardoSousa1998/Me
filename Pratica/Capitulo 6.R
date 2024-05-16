## exemplo 2
#'Seja X a variavel aleatoria que representa o numero de pacientes que respondem
#positivamente ao tratamento, num grupo de 4 pacientes
#H0 : X ∼ B(4, 0.6) vs H1 : X ≁ B(4, 0.6)


#Domınio Frequencias  Probabilidade  Frequencias
#          Observadas
#xi        Oi = ni     pi = f(xi)      Ei = n × pi
#0         0          f(0) = 0.0256   50 × 0.0256 = 1.28
#1         5          f(1) = 0.1536   50 × 0.1536 = 7.68
#2         12         f(2) = 0.3456   50 × 0.3456 = 17.28
#3         19         f(3) = 0.3456   50 × 0.3456 = 17.28
#4         14         f(4) = 0.1296   50 × 0.1296 = 6.48
#          n = 50           1         n = 50




xi <- c(0,1,2,3,4)
k<- length(xi)

oi<- c(0,5,12,19,14)

n<- sum(oi)

pi<- dbinom(xi,4,0.6)

r<-0

gl <-k-1-r # k − 1 − r = 5 − 1 − 0 = 4 graus de liberdade


teste2<- chisq.test(x=oi,p=pi)


#como pvalue <=0.05 = alpha rej h0




##############Exemplo 3

nunidades <- c(0,1,2,3,4,5)
ndias <- c(14,22,18,15,10,9)


#Seja X a variavel aleatoria que representa a procura diaria do produto
#H0 : X ∼ P(2.1) contra H1 : X ≁ P(2.1)

n<- sum(ndias)

nsig<- 0.01 #nıvel de significˆancia = α

r<-0 #numero de parametros estimados = r 
k<- 6#numero de linhas da tabela de frequencias = k 

pi=dpois(nunidades,2.1)
pi[k]<-1-ppois(nunidades[k-1],2.1)

chisq.test(x=ndias,p=pi)


######Exemplo 3 

#Seja X a variavel aleatoria que representa a procura diaria do produto
#H0 : X ∼ P(λ) contra H1 : X ≁ P(λ)
nunidades <- c(0,1,2,3,4,5)
ndias <- c(14,22,18,15,10,9)

n=88 
mean <- (0*14 + 22*1 +18*2 + 15*3 +10*4 +9*5)/n



nsig<- 0.01 #nıvel de significˆancia = α


r<-1 #numero de parametros estimados = r 
k<- 6#numero de linhas da tabela de frequencias = k 


pi=dpois(nunidades,mean)
pi[k]<-1-ppois(nunidades[k-1],mean)

chisq.test(x=ndias,p=pi)


# RC
#RC=[x^2 (1−α , k−1−r),+∞[
#[x^2 (0.99,4),+∞[

qchisq(0.99,4)
#[13,2767, + inf[

#Como Qobs = 4.7289 ∈/ RC entao nao se rejeita a hipotese H0


#Valor-P

#valor-p = P(Q ≥ Qobs) = P(Q ≥ 4.7289) = 1−P(Q < 4.7289) = 1−F(4.7289) = 0.3163
1-pchisq(4.7289,4)7

#Como valor-p > 0.01 = α entao nao se rejeita a hipotese H0






