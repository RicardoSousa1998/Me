#1 0.75
#a
#id = quantitativos discreta
#treat = qualitativos nominal
#age = quantitativos discreta
#sex qualitativa nominal
#twstrs quantitativa discreta

#Amostra 631 pacientes injetados com medicamento ou placebo
nrow(distonia)

#b 1.2

(ni.sex <- table(distonia$sex))           # Frequências Absolutas
(fi.sex <- round(prop.table(ni.sex), 4))  # frequências Relativas
(Ni.sex <- cumsum(ni.sex))                # frequências Absolutas Acumuladas
(Fi.sex <- round(cumsum(fi.sex), 4))      # frequências Relativas Acumuladas

(tabela_preco <- data.frame(
  i = 1:nrow(ni.sex),
  xi = c("Feminino","Masculino"),
  ni = as.integer(ni.sex),
  fi = as.numeric(fi.sex),
  Ni = as.integer(Ni.sex),
  Fi = as.numeric(Fi.sex)
))

#Com base na tabela podemos verificar que a moda é o Feminino (Nº1) logo a afirmação esta correta nesta amostra


#c 1.2

minAge<- min(distonia$age)

maxAge <- max(distonia$age)



hist(distonia$age, breaks=c(minAge,45,60,maxAge), right=FALSE, include.lowest=TRUE, col=c(2,3,4),
     xlab="Idade", main="Histograma", ylim=c(0,0.05),
     xaxt="n")                  
axis(side=1, at=c(0,minAge,45,60,maxAge))


#Com base no histograma podemos verificar que no intervalo [45,60[  o distúrbio ocorre com mais frequecia nesta amostra




#D 1

boxplot(twstrs~treat, data=distonia, col=c(2,3,4), ylab="pontuação",names=c("placebo", "5000 botox B", "10000 botox B"), 
        xlab="tratamento", range=1.5 )

#Com base nos diagramas de extremos e quartis podemos verfiicar que nao existe  grande diferença entre a relação das  pontuações e tratamentos,




#E 0.2
DescTools::Mode(distonia$age) #57

median(distonia$age) # 56

mean(distonia$age) #55

#como moda > mediana > media a assimetria negativa 

e1071::skewness(distonia$age) #b1



#2

#a 1
 

#f(1) = F(1) = 0.10
#f(2) = F(2) - F(1) = 0.3
#f(3) = F(3) - F(2) = 0.3
#f(4) = F(4) - F(3) = 0.2
#f(5) = F(5) - F(4) = 0.1


#b 1.5


#' P(X>=2 | X<=4) <=> P(X>=2 ^ X<=4) / P (X<=4 ) <=> P(X<=2  V X<=4) / F(4) <=> P(X<1  V X<=4) / 0.9
#' F(4) - F(1) / 0.9 <=>  0.9-0.1 /0.9  <=> 0.8/0.9 = 0.8888


#C 1.5


#v[(7-2X)/3] <=> v[7/3  +(- 2/3)X] <=> (-(2/3))^2 * V[X]  <=> 4/9 * V[X] <=> 4/9 * 1.29´<=> 0.5733^2

#V[X] = E[X^2] - E^2[X] = 9.7- 8.41  = 1.29

#E[X] = sum (x * f(x))  = 1*0.10 + 2 * 0.3 +3 * 0.3 +  4* 0.2 +  5* 0.1 = 2.9
#E[x^2] = sum (x^2 * f(x)) = 1*0.10 + 2^2 * 0.3 +3^2 * 0.3 +  4^2 * 0.2 +  5^2* 0.1 = 9.7


#E^2[X] = (E[X])^2 = 2.9^2  = 8.41



#D 1.5

#como 1h = 60 pretende-se 

#P(Y>60) = Integ [60,-inf] f(y)dy <=> Integ [60,80] (1/40 -y/3200 )dy + integ [80,+inf] 0dy 



f <- function(y) {
  (1/40)-(y/3200)
}

integrate(f, lower=60, upper=80)$value + 0  # 0.0625



#3 1.5
#X~P(2)
#X Nº de falhas por ano
#a
# se 1 - 2 
#    5 - y  
#X nº de falhas por 5 anos
# y~P(5*2 = 10)
# P(y = 3 ) <=> f(3) = dpois (3,10) = 0.0076

dpois (3,10)

#B
# 12 m = 1 ano

#T~Exp(12/2)
#P(T>6 | T>=4)    = P(T>6-4) = P(T>2) = 1 -P(T<=2) = 1-F(2)  =0.7165
#FALTA DE MEMORIA ^

#C
# x ~N(300,20)
#I 2


#P(X<280) = F(280) =pnorm (280,300,20)
p <- pnorm (280,300,20)

#B ~B(7,p)

#P(B>=1) <=> 1 - P(B<1) = 1-P(B<=0)= 1- F(0) = 1-  pbinom (0,7,p) = 0.7016

1-  pbinom (0,7,p)


#II 1
#P(x<=k) =0.05 = k=F^(-1) (0.05) <=> qnorm (0.05,300,20) = 267.1029
qnorm (0.05,300,20)



#III 1.5
#B~N(290,O)


#P(X>250) = 0.80 <=> 1-P(X<=250) = 0.8 <=> P(X<=250) = 0.2  <=> F(250) = 0.2
#(Z < ((250-290) / O) )~N(0,1)
#((250-290) / O) = 0.2 (=) qnorm(0.2) 
qnorm(0.2)
#250-290 = o * -0.8416212
290-250
#-40 = o * -0.8416212 
#-40 /-0.8416212  = 0
-40 /-0.8416212
#0 = 47.52732

