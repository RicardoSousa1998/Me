library(DescTools)

# definir a dimens~ao da amostra
n = 234 # podem alterar a dimens~ao da amostra
# par^ametros necess´arios para simular os dados
m = c(10, 500, 50)
s = c(2, 150, 20)
vcr = c(sample(65:95,1)/100, sample(65:95,1)/100, sample(65:95,1)/100)
crm = matrix(c(1, -vcr[1], vcr[2], -vcr[1], 1, -vcr[3], vcr[2], -vcr[3], 1), nrow=3)
dd = MASS::mvrnorm(n, mu = m, Sigma = crm * (s %*% t(s)))
plat = sample(c("Google", "Facebook", "Instagram"), n, replace=TRUE, prob=c(0.4, 0.3, 0.3))
ta = sample(c("Banner", "Vídeo", "Texto"), n, replace=TRUE, prob=c(0.4, 0.3, 0.3))
sm = sample(c("Tecnologia", "Moda", "Alimentação"), n, replace=TRUE, prob=c(0.3, 0.3, 0.4))
# Criar uma tabela com os dados
DadosMarkDig = data.frame(ID=1:n, plataforma=plat, anuncio=ta, mercado=sm, investimento=dd[,1], cliques=dd[,2], conversoes=dd[,3])
# guardar os dados num ficheiro .txt (que dever´a ser entregue com a an´alise)
write.table(DadosMarkDig, "DadosMarkDig.txt", row.names=FALSE, quote=FALSE)



##############################

#' Plataforma: Qualitativa Nominal
#' Anuncio:Qualitativa Nominal
#' Mercado:Qualitativa Nominal
#' Investimento: Quantitativa Contínua
#' Cliques:Quantitativa Discreta
#' Conversoes; Quantitativa Discreta
 




#' avaliar e quantificar a relação entre as variaveis qualitativas


#'* Plataforma~Anuncio*
#' h0 :  Plataforma~Anuncio Sao independentes
#' vs
#' h1 : Plataforma~Anuncio nao Sao independentes


TabelaPlataformaAnuncio <- table(DadosMarkDig$plataforma, DadosMarkDig$anuncio)
show(TabelaPlataformaAnuncio)
teste <-chisq.test(TabelaPlataformaAnuncio)

#coeficiente de contingência
ContCoef(TabelaPlataformaAnuncio)
#ou 
(sqrt(teste$statistic / (teste$statistic + sum(TabelaPlataformaAnuncio))))

#coeficiente V de Crámer
CramerV(TabelaPlataformaAnuncio)
#ou
sqrt(teste$statistic / (sum(TabelaPlataformaAnuncio) * (min(nrow(TabelaPlataformaAnuncio), ncol(TabelaPlataformaAnuncio)) - 1)))

#coeficiente τb de Kendall
#So quando  as 2 das variaveis sao qualitativas ordinais



#como  p-value = 0.4775 < 0.05 rejeitamos h0 logo Plataforma~Anuncio não são independentes 
#coeficiente de contingência = 0.1214379  sugere uma associação fraca entre as variaveis € [0.10,0.30[
#coeficiente V de Crámer = 0.08650982 sugere uma associação fraca entre as variaveis  € [0.07, 0.20[



#'* Plataforma~Mercado*
#' h0 :  Plataforma~Mercado Sao independentes
#' vs
#' h1 : Plataforma~Mercado nao Sao independentes

TabelaPlataformaMercado <- table(DadosMarkDig$plataforma, DadosMarkDig$mercado)
show(TabelaPlataformaMercado)
teste <-chisq.test(TabelaPlataformaMercado)


#coeficiente de contingência
ContCoef(TabelaPlataformaMercado)
#ou 
(sqrt(teste$statistic / (teste$statistic + sum(TabelaPlataformaMercado))))

#coeficiente V de Crámer
CramerV(TabelaPlataformaMercado)
#ou
sqrt(teste$statistic / (sum(TabelaPlataformaMercado) * (min(nrow(TabelaPlataformaMercado), ncol(TabelaPlataformaMercado)) - 1)))

#como  p-value = 0.4427 < 0.05 rejeitamos h0 logo Plataforma~Mercado não são independentes 
#coeficiente de contingência = 0.1253855 sugere uma associação fraca entre as variaveis € [0.10,0.30[
#coeficiente V de Crámer = 0.08936618 sugere uma associação fraca entre as variaveis  € [0.07, 0.20[



#'* Anuncio~Mercado*
#' h0 :  Anuncio~Mercado Sao independentes
#' vs
#' h1 : Anuncio~Mercado nao Sao independentes

TabelaAnucioMercado <- table(DadosMarkDig$anuncio, DadosMarkDig$mercado)
show(TabelaAnucioMercado)
teste <-chisq.test(TabelaPlataformaMercado)

#coeficiente de contingência
ContCoef(TabelaAnucioMercado)
#ou 
(sqrt(teste$statistic / (teste$statistic + sum(TabelaAnucioMercado))))

#coeficiente V de Crámer
CramerV(TabelaAnucioMercado)
#ou
sqrt(teste$statistic / (sum(TabelaAnucioMercado) * (min(nrow(TabelaAnucioMercado), ncol(TabelaAnucioMercado)) - 1)))

#como  p-value = 0.006494 < 0.05 rejeitamos h0 logo Anuncio~Mercado não são independentes 
#coeficiente de contingência = 0.2397089 sugere uma associação fraca entre as variaveis € [0.10,0.30[
#coeficiente V de Crámer = 0.17459 sugere uma associação fraca entre as variaveis  € [0.07, 0.20[




#*************************************************************************
#*************************************************************************
#*************************************************************************
#*************************************************************************
#*************************************************************************
#*************************************************************************

#'avaliar e modelar a relaçao entre as variaveis quantitativas .
#'*Investimento~Cliques*


#Variavel dependente é cliques
#Variavel dependente é investimento
modelo <- lm(investimento ~ cliques, data = DadosMarkDig)
summary(modelo)



plot(investimento ~ cliques, data = DadosMarkDig)
abline(modelo, col = "red")

plot(predict(modelo), residuos)
abline(h = 0, col = "blue", lty = 2)

#'*equação da reta de regressão*
#y^ = a + bx
#Y^ = 16.0707958  + (-0.0121827  * X)
#ONDE y^ é o numero de cliques previsto e X é o valor investido na campanha 
#como b é negativo a  correlação  é linear negativa


#interpretação dos coeficientes
#Mesmo quando nenhum valor é investido na campanha(X=0), esperamos cerca de 16 cliques. 
#Além disso, para cada unidade adicional de investimento na campanha, espera-se uma redução de aproximadamente 0.012 cliques

#'*avaliar a qualidade do modelo ajustado*

# R-quadrado = 0.8724071 #significa que aproximadamente 87.24% da variabilidade no
#número de cliques é explicada pelo modelo de regressão linear simples e o modelo tem um bom ajuste aos dados,
summary(modelo)$r.squared


# Erro padrão da estimativa = 0.7151493 # significa  que as previsões do modelo estão, em média, a cerca de 0.715 unidades
#do valor real do número de cliques
summary(modelo)$sigma

#Esses valores indicam que o modelo de regressão linear simples é bastante eficaz na previsão 
#do número de cliques com base no valor investido na campanha


#'* Análise dos resíduos*
hist(residuos, main = "Histograma dos Resíduos", xlab = "Resíduos")
plot(density(residuos), main = "Gráfico de Densidade dos Resíduos", xlab = "Resíduos")
plot(predict(modelo), residuos, main = "Resíduos vs. Valores Ajustados", xlab = "Valores Ajustados", ylab = "Resíduos")


#'*Investimento~Conversoes*
#'*Conversoes~Cliques*
