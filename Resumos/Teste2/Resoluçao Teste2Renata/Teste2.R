#Resuloçao Teste

#'*1*
#'*a)*

mean(distonia2$age)  #  Media 59.07812
sd(distonia2$age) # desvio padrao 11.22823
(nrow(distonia2[distonia2$treat==1,])/nrow(distonia2))*100  = #p* *100  =51.5625%


#'*1*
#'*b)*
#x = idade do paciente com distonia cervical

#h0 x ~ normal (57,15)
#vs
#h1 x ~/~ normal(57,15)
#
  
ks.test(x=distonia2$age,y="pnorm", mean=57, sd=15)
#Como valor−p = 0.1268 > α = 0.01, entao Nao se rejeita H0.

#com 1% de nivel de significacia e com base nesta amostra ha evidecias que segue uma distribuiçao normal (57,15)

#'*1*
#'*c)*


#x =  pessoas do genero feminino

#h0 mu <= 55 idade media
#vs 
#h1 mu > 55 

#alpha =0.03

fem = distonia2[distonia2$sex==1,]

BSDA::z.test(
  x = fem$age,                  
  sigma.x = sd(fem$age),        
  mu=55,
  alternative = "greater"
)

#zbos = 2.5165

#rc [qnorm(1-0.03), +inf[ 
#   [1.880794, +inf[

#como zbos € na rc rejeita-se h0
# com 3% de significˆancia e com base na amostra, conclui-se que existe evidencia estatıstica
#que a idade media dos pacientes do genero feminino é superior a 55 anos.


#'*1*
#'*c)*


#A -> sintomas  antes
#D -> sintomas depois 
#x = D-A 


antes = distonia2[distonia2$treat==2,]$twstrs_1
depois = distonia2[distonia2$treat==2,]$twstrs_2
#'*1*
#'*c)*
#H0 mediaanad x >=0
#vs
#h1 mediana x<0

#alpha  =0.10

#teste esquerdo 

#Como as amostras sao emparelhadas, entao o teste de hipoteses nao parametrico adequado é o teste de Wilcoxon

wilcox.test(
  x=depois, 
  y=antes, 
  alternative="less", 
  mu=0, 
  paired=TRUE
)
#como p-value = 0.05454 < alpha 0.10  ent  Rejeita-se H0.



#'*2*
  #'*a)*
    #'*i)*

#X = tempo, em segundos, que demoram a efetuar uma tarefa com um monitor colorido
#Y = tempo, em segundos, que demoram a efetuar uma tarefa com um monitor a preto e branco.

amostrax <- c(502 ,488, 494 ,481 ,497 ,488 ,494 ,489)
amostray <- c(510 ,498, 512, 497 ,494 ,495 ,508)
nx = length(amostrax)
ny = length(amostray)

#h0 x ~ normal | y ~ normal
#vs
#h1 x~/~normal | y ~/~ normal
#alpha = 0.05
shapiro.test(amostrax) #p-value 0.9002 > alpha nao se rejeita h0
shapiro.test(amostray) #p-value 0.1148 > alpha nao se rejeita h0

#Com base na amostra e para um nıvel de significancia de 5%, ha evidencia
#estatıstica que os dados podem vir de uma populacao com distribuicao Normal



#'*2*
  #'*a)*
    #'*ii)*

#h0 σx = σy         σ^2x = σ^2y         σ^2x/σ^2y = 1
#vs           <=>                 <=>
#h1 σx =/= σy       σ^2x =/= σ^2y       σ^2x/σ^2y =/= 1

#bilateral


#fobs = F = ((s1^2 / s2^2) * (σ2^2 / σ1^2)) ~ F(n1 - 1, n2 - 1)  
var.test(x=amostrax, y=amostray, ratio = 1, alternative="two.sided") # fbos = 0.71156

#rc [0, qf(0.05/2,nx-1,ny-1)] u [,qf(0.975,nx-1,ny-1) + inf[
#rc [0, 0.195366] u [5.69547 + inf[
#como zobs nao € na rc ent nao se rejeita h0



#'*2*
  #'*a)*
    #'*iii)*

#h0 µx ≤ µy      µx - µy ≤ 0
#vs          <=> 
#h1 µx > µy      µx - µy >0
 
#Populacoes Normais (alınea (a)i.)
#σx e σy desconhecidos
#σx = σy (alınea (a)ii.)
t.test(x=amostrax,
       y=amostray,
       alternative="greater",
       mu=0,
       paired=FALSE,
       var.equal=TRUE
) 
#tbos =-2.8396 | valor-p = 0.993
#Como rejeita-se H0 se valor−p ≤ α, entao para α ≥ 0.993 rejeita-se a hipotese nula.

#'*2*
  #'*a)*
    #'*iv)*

# ] sqrt(((n-1) * s^2) / x^2_(1 - (α/2); n-1)) , sqrt(((n-1) * s^2) / x^2_(α/2; n-1)) [
#ent sqrt(((n-1) * s^2) / x^2_(1 - (α/2); n-1))  = 4.8662 <=>  sqrt(((ny-1) * var(amostray)) / x^2_(1 - (α/2); ny-1)) = 4.8662<=>
#sqrt(354 / x^2_(1 - (α/2); ny-1)) = 4.8662 <=> (354 / x^2_(1 - (α/2); ny-1)) = 4.8662^2  <=>  x^2_(1 - (α/2); ny-1) = 354/23.681
# x^2_(1 - (α/2); ny-1) = 14.94869 <=> 1-(alpha/2) = F(14.94869) <=> 1-(alpha/2) = pchisq(14.94869,ny-1) <=>  1-(alpha/2) = 0.9793404
#1-0.9793404 = alpha/2 <=> 0.0206596 * 2 = alpha  <=> 0.0413192
#Entao o grau de confianca do intervalo e (1 − 0.0413) × 100% = 95.87%

#k =sqrt(((ny-1) * var(amostray)) / x^2_(α/2; n-1))  < = >  sqrt(354 / qchisq(0.0413192/2,ny-1)) = 17.55451





#'*2*
  #'*b)*
    #'*i)*
#p =.08
#Populacao Binomial e Amostra n = 250 ≥ 30, entao a Distribuicao Amostral ´

#Zobs = ((p* - p) / sqrt(pq / n)) <=>  

#P(p*>= 0.10) = 1-P(p*<0.10) <=> 1- P(Z<((0.10 - 0.08) / sqrt(0.08*(1-0.08)/ 250)) ) <=> 1- P(Z<1.1656) <=> 1- pnorm(1.1656) = 0.1218881


#'*2*
  #'*b)*
    #'*ii)*
#Pretende-se determinar a dimensao da amostra n de modo a construir um intervalo de confianca a 99% para p com uma margem de erro que nao ultrapassa os 2%.
#Populacao Binomial e Amostra n ≥ 30, entao o intervalo de confianca para a proporcao ´e

# I.C.: ] p* |-+| z_(1 - (α/2)) * sqrt((p* * q*) / n) [

# Margem de erro  = p* |-+| z_(1 - (α/2)) * sqrt((p* * q*) / n)/2 <=>  z_(1 - (α/2)) * sqrt((p* * q*) / n)

b<-30 #n inicial
c<-0.02 # Valor desejado da amplitude do  intervalo de conf


while (TRUE)
{
  margem_erro= qnorm(1-0.01/2)*sqrt(0.5*0.5 / b)
  if (margem_erro  < c ) {
    break
  }
  
  b <- b + 1
}
#r n ≥ 4147
  
  
#'*2*
  #'*b)*
    #'*iii)*

#n1 = 850 homens >30
#n2 = 2000 mulheres  >30

#p1* = 75/850 = 0.08823529
#p2* = 5/2000 = 0.0025
#q1* = 1- 0.08823529
#q2 = 1- 0.0025
#grau de confianca = 1 − α = 0.90
#nıvel de significancia = α = 0.10


#] (p1* - p2*) |-+| z_(1 - (α/2)) * sqrt(((p1* * q1*) / (n1)) + ((p2* * q2*) / (n2)))
#] (0.08823529 - 0.0025) |-+| z_(1 - (α/2)) * sqrt(((0.08823529 * 0.9117647) / (850)) + ((0.0025 * 0.9975) / (2000)))
#] 0.08573529 |-+| z_(1 - (α/2)) * 0.009792534 [
# ]0.08573529 |-+| 0.01610729[ 
#] 0.069628 ;0.01610729 [

#para intervalo de conf 0.90 para a diferen¸ca de percentagens, (p1 − p2) × 100%
#]6.96, 10.18[ e como 0 nao € entao existem diferen¸cas significativas nas percentagens de daltonismo
#entre homens e mulheres.
