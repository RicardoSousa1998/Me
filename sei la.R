amostra <-c(1476,182,246,300,499,442,98,552,20,221,1563,796,157,36,31)

#teste ajustamente de kolmogorv smirnov
ks.test(amostra,"pexp",rate=1/730)


#######################################################################################
# EXEMPLO 7
# amostra
amostra7 <- c(75, 92, 80, 80, 84, 72, 84, 77, 81,                
              77, 75, 81, 80, 92, 72, 77, 78, 76,                
              77, 86, 77, 92, 80, 78, 68, 78, 92,                
              68, 80, 81, 87, 76, 80, 87, 77, 86,                
              74, 93, 79, 81, 83, 71, 83, 78, 80,                
              76, 76, 80, 82, 91, 72, 76, 79, 75)

#############
# EXEMPLO 7.1
# H0:X segue uma distribuição Normal de média 80 e desvio padrão6.95
# contra
# H1:X Não segue uma distribuição Normal de média 80 e desvio padrão 6.95

# teste de Ajustamento de Kolmogorv-Smirnov
ks.test(amostra7, "pnorm", mean=80, sd=6.95)
#############

nortest::lillie.test(amostra7)





##############
#exemplo 8
#H0 : X ∼ Normal contra H1 : X  ~/~Normal

amostra8 <- c(75, 92 ,80, 80 ,84, 72, 84, 77 ,81,
              77 ,75 ,81 ,80 ,92, 72, 77, 78 ,76,
              77 ,86 ,77 ,92 ,80, 78, 68, 78, 92,
              68 ,80, 81 ,87 ,76, 80, 87 ,77, 86)

shapiro.test(amostra8)