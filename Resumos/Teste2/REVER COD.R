# Dados da amostra
amostra <- c(23.9, 23.5, 23.8, 23.1, 23.4, 23.6, 23.4, 23.2, 23.6, 23.5)

# Parâmetros fornecidos
mu <- 23.2
n <- length(amostra)

# Calcular a média da amostra
media <- mean(amostra)

# Calcular o desvio padrão da amostra
desvio_padrao <- sd(amostra)

# Calcular o valor t
t_value <- (media - mu) / (desvio_padrao / sqrt(n))

# Graus de liberdade
df <- n - 1

# Calcular o valor p
p_value <- 2 * pt(-abs(t_value), df)

# Exibir os resultados
cat("Média da amostra:", media, "\n")
cat("Desvio padrão da amostra:", desvio_padrao, "\n")
cat("Valor t:", t_value, "\n")
cat("Valor p:", p_value, "\n")

# Tomar uma decisão com base no valor p
alpha <- 0.01
if (p_value < alpha) {
  cat("Rejeitamos H0: o produto NÃO satisfaz as especificações.\n")
} else {
  cat("Não rejeitamos H0: o produto satisfaz as especificações.\n")
}
