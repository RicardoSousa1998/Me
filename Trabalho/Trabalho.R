library(DescTools)

# parâmetros necessários para simular os dados
n = sample(234:254,1)
m = c(10, 500, 50)
s = c(2, 150, 20)
vcr = c(sample(65:95,1)/100, sample(65:95,1)/100, sample(65:95,1)/100)
crm = matrix(c(1, -vcr[1], vcr[2], -vcr[1], 1, -vcr[3], vcr[2], -vcr[3], 1), nrow=3)
dd = MASS::mvrnorm(n, mu = m, Sigma = crm * (s %*% t(s)))
plat = sample(c("Google", "Facebook", "Instagram"), n, replace=TRUE, prob=c(0.4, 0.3, 0.3))
ta = sample(c("Banner", "Vídeo", "Texto"), n, replace=TRUE, prob=c(0.4, 0.3, 0.3))
sm = sample(c("Tecnologia", "Moda", "Alimentação"), n, replace=TRUE, prob=c(0.3, 0.3, 0.4))
# Criar uma tabela com os dados
DadosMarkDig = data.frame(ID=1:n, plataforma=plat, anuncio=ta, mercado=sm,
                          investimento=dd[,1], cliques=as.integer(dd[,2]),
                          conversoes=as.integer(dd[,3]))
# guardar os dados num ficheiro .txt (que deverá ser entregue com a análise)
write.table(DadosMarkDig, "DadosMarkDig.txt", row.names=FALSE, quote=FALSE)

##############################

#' Plataforma: Qualitativa Nominal
#' Anuncio:Qualitativa Nominal
#' Mercado:Qualitativa Nominal
#' Investimento: Quantitativa Contínua
#' Cliques:Quantitativa Discreta
#' Conversoes; Quantitativa Discreta

#'* Plataforma~Anuncio*
#' h0 :  Plataforma~Anuncio Sao independentes
#' vs
#' h1 : Plataforma~Anuncio nao Sao independentes

TabelaPlataformaAnuncio <- table(DadosMarkDig$plataforma, DadosMarkDig$anuncio)
show(TabelaPlataformaAnuncio)
testePlataformaAnuncio <-chisq.test(TabelaPlataformaAnuncio)

testePlataformaAnuncio$p.value # 0.8917984



#como  p-value = 0.8917984 > 0.05 ent nao se rejeia h0 logo Plataforma~Anuncio são independentes 
#como sao independetes nao se classifica a associação existente


#'* Plataforma~Mercado*
#' h0 :  Plataforma~Mercado Sao independentes
#' vs
#' h1 : Plataforma~Mercado nao Sao independentes

TabelaPlataformaMercado <- table(DadosMarkDig$plataforma, DadosMarkDig$mercado)
show(TabelaPlataformaMercado)
testePlataformaMercado <-chisq.test(TabelaPlataformaMercado)
testePlataformaMercado$p.value # 0.2814902


#como  p-value = 0.2814902 > 0.05 ent nao se rejeia h0 logo Plataforma~Mercado são independentes 
#como sao independetes nao se classifica a associação existente


#'* Anuncio~Mercado*
#' h0 :  Anuncio~Mercado Sao independentes
#' vs
#' h1 : Anuncio~Mercado nao Sao independentes

TabelaAnucioMercado <- table(DadosMarkDig$anuncio, DadosMarkDig$mercado)
show(TabelaAnucioMercado)
testeAnucioMercado <-chisq.test(TabelaAnucioMercado)
testeAnucioMercado$p.value #0.03834764

#coeficiente de contingência
ContCoef(TabelaAnucioMercado) # 0.1984537

#coeficiente V de Crámer
CramerV(TabelaAnucioMercado) # 0.1431757

#Coeficiente Tb de Kendall: 
#Nao se usa neste caso


#como  p-value = 0.03834764 < 0.05 ent  rejeia-se h0 logo Anuncio~Mercado nao são independentes 
#coeficiente de contingência = 0.1984537 sugere uma associação fraca entre as variaveis € [0.10,0.30[
#coeficiente V de Crámer = 0.1431757 sugere uma associação fraca entre as variaveis  € [0.07, 0.20[



#*************************************************************************
#*************************************************************************
#************************************PARTE2*******************************
#*************************************************************************
#*************************************************************************
#*************************************************************************
#TODO 
#definir a variável dependente e independente 
#indicar a equação da reta de regressão
#interpretar os coeficientes obtidos 
#avaliar a qualidade do modelo ajustado
#fazer uma análise dos resíduos.


#'*Conversoes~investimento*



#Variavel dependente (Y)é Conversoes
#Variavel independente (X) é investimento


modelo_conversoes_investimento <- lm(conversoes ~ investimento, data = DadosMarkDig)

# Resumo do modelo
summary(modelo_conversoes_investimento)

# linha de regressão
plot(DadosMarkDig$investimento, DadosMarkDig$conversoes, main="Regressão Linear Simples",
     xlab="Investimento (milhares de euros)", ylab="Conversões")
abline(modelo_conversoes_investimento, col="blue")  #Correlação linear positiva rxy > 0  e forte  forte  1 >  rxy  > 0.8

#Coeficiente de Correlacao Linear
cor(DadosMarkDig$conversoes,DadosMarkDig$investimento) #  rxy = 0.9387048

#equação da reta de regressão
A <- modelo_conversoes_investimento$coefficients[1]
B <- modelo_conversoes_investimento$coefficients[2]

equacao <- paste("conversoes = ", round(A, 2), " + ", round(B, 2), " * investimento")
print(equacao)
#quanto maior o investimento na campanha , maior o número de conversões esperadas.


# Análise dos resíduos
residuos_conversoes <- residuals(modelo_conversoes_investimento)
plot(DadosMarkDig$investimento, residuos_conversoes, main = "Resíduos do Modelo de Conversões", xlab = "Investimento (milhares de euros)", ylab = "Resíduos")
abline(h = 0, col = "red")
#o modelo de residuos  é bom pq tem a ausência de padrões claros (funil ou curva ) e parecem estar distribuídos
#aleatoriamente ao redor da linha horizontal em zero,




#'*cliques~investimento*


#Variavel dependente (Y)é cliques
#Variavel independente (X) é investimento


modelo_cliques_investimento <- lm(cliques ~ investimento, data = DadosMarkDig)

# Resumo do modelo
summary(modelo_cliques_investimento)

# linha de regressão
plot(DadosMarkDig$investimento, DadosMarkDig$cliques, main="Regressão Linear Simples",
     xlab="Investimento (milhares de euros)", ylab="Cliques")
abline(modelo_cliques_investimento, col="blue")  #Correlação linear negativa rxy < 0  e forte    -1 >  rxy  > -0.8

#Coeficiente de Correlacao Linear
cor(DadosMarkDig$cliques,DadosMarkDig$investimento) #  rxy = -0.813987

#equação da reta de regressão
A <  modelo_cliques_investimento$coefficients[1]
B <- modelo_cliques_investimento$coefficients[2]

equacao <- paste("Cliques = ", round(A, 2), " + ", round(B, 2), " * investimento")
print(equacao)

# quanto maior o investimento na campanha , menor o número de cliques esperados. 


# Análise dos resíduos
residos <- residuals(modelo_cliques_investimento)
plot(DadosMarkDig$investimento, residos, main = "Resíduos do Modelo de Conversões", xlab = "Investimento (milhares de euros)", ylab = "Resíduos")
abline(h = 0, col = "red")
#o modelo de residuos  é bom pq tem a ausência de padrões claros (funil ou curva ) e parecem estar distribuídos
#aleatoriamente ao redor da linha horizontal em zero,











