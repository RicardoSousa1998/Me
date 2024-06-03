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
#' Plataforma~Anuncio
#' Plataforma~Mercado
#' Anuncio~Mercado


#'avaliar e modelar a relaçao entre as variaveis quantitativas .
#'Investimento~Cliques
#'Investimento~Conversoes
#'Conversoes~~Cliques
