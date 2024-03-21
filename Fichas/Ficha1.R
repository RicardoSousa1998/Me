
#2

#2(a)
#Populaçao:Hoteis na europa
#Amostra:12 hoteis 7
#Unidade Estatistica : Cada hotel


#2(b)


nome <- c("Hotel Ronda", "Villad'Este", "Hotel Lisboa", "Hotel Prem", "Hotel d'Europa", "Palace Luzern", "Hotel Palace", "Hotel Arts", "Hotel Sacher", "Duc de Bourgogne", "Villa Gallici", "Hotel Vila")
pais <- c("Espanha", "Itália", "Portugal", "Alemanha", "França", "França", "Portugal", "Espanha", "Alemanha", "França", "França", "Portugal")
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

tabela_pais <- table(hotel_df$pais)
print(tabela_pais)
tabela_preco <- table(hotel_df$preco)
print(tabela_preco)

#2 (e)

nrow(hotel_df[hotel_df$pais %in% c("Espanha", "Portugal"), ])


#2 (f)


#2

#2(a)
#Populaçao:Hoteis na europa
#Amostra:12 hoteis 7
#Unidade Estatistica : Cada hotel


#2(b)


nome <- c("Hotel Ronda", "Villad'Este", "Hotel Lisboa", "Hotel Prem", "Hotel d'Europa", "Palace Luzern", "Hotel Palace", "Hotel Arts", "Hotel Sacher", "Duc de Bourgogne", "Villa Gallici", "Hotel Vila")
pais <- c("Espanha", "Itália", "Portugal", "Alemanha", "França", "França", "Portugal", "Espanha", "Alemanha", "França", "França", "Portugal")
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

tabela_pais <- table(hotel_df$pais)
print(tabela_pais)
tabela_preco <- table(hotel_df$preco)
print(tabela_preco)

#2 (e)

nrow(hotel_df[hotel_df$pais %in% c("Espanha", "Portugal"), ])


#2 (f)


(sum(nchar(hotel_df$preco) == max(nchar(hotel_df$preco)))/ nrow(hotel_df))*100





