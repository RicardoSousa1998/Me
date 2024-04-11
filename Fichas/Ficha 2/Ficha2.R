

#FICHA 2

#1 (A)

Stroke2<- na.omit(stroke) 


#1 (B)

(ni.sex <- table(Stroke2$sex))

barplot(
  ni.sex,                                               
  main = "Sex",      
# xlab = "Sex",                                         
  ylab = "Frequencias absolutas",                        
  col = 2:4,                                            
  ylim = c(0, 200),                                      
  names.arg = c("masculino","feminino")
)     

#1 (C)
(ni.coma <- table(Stroke2$coma))
(fi.coma <- round(prop.table(ni.coma), 4))


barplot(
fi.coma,                                               
  main = "Coma",      
  # xlab = "Sex",                                         
  ylab = "Frequencias Relativas",                        
  col = 2:4,                                            
  names.arg = c("Nao","Sim")
)   


#1 (D)

#sex,coma,diab,dead

# Coma (resto igual mas com outras vars)

(ni.coma <- table(Stroke2$coma))
(fi.coma <- round(prop.table(ni.coma), 4))


pie(
  ni.coma,
  labels=paste(fi.coma * 100, "%"),
  col=c("red", "blue"),
  main="Coma"
)

# Legenda
legend(
  "topright",
  legend=c("Nao","Sim"),
  fill=c("red", "blue"),
  cex = 1
)

#1 (E)

StrokeAfterAVC <- Stroke2[Stroke2$coma=="1",]
View(StrokeAfterAVC)


(ni.dgn <- table(StrokeAfterAVC$dgn))

barplot(
  ni.dgn,                                               
  main = " diagnostico com niveis",      
  # xlab = "Sex",                                         
  ylab = "FrequÃªncias absolutas",                        
  col = 2:5,                                            
  #ylim = c(0, 200),                                      
  names.arg = names(StrokeAfterAVC$dgn)
)     


#1 (F)
StrokeAfterAVC <- Stroke2[Stroke2$coma=="2",]
View(StrokeAfterAVC)


(ni.dgn <- table(StrokeAfterAVC$dgn))

barplot(
  ni.dgn,                                               
  main = " diagnostico com niveis",      
  # xlab = "Sex",                                         
  ylab = "Frequencias absolutas",                        
  col = 2:5,                                            
  #ylim = c(0, 200),                                      
  names.arg = names(StrokeAfterAVC$dgn)
)   


#1 (G)

## SEI LA



#1 (h) 
num_classes <- ceiling(log2(length(Stroke2$age)) + 1)

hist(Stroke2$age, breaks = num_classes, main = "Histograma de Idade", xlab = "Idade", ylab = "Frequência", col = "lightblue", border = "black")


##OU


(n <- length(Stroke2$age))
# Nº de Classes:
(k <- trunc(1 + log(n)/log(2)))
# Amplitude das Classes:
(h <- (max(Stroke2$age) - min(Stroke2$age)) / k)


(idade.min <- min(Stroke2$age))
(idade.max <- idade.min + h * k)

(idade.cortes <- seq(idade.min, idade.max, by = h))

(idade.classes <- cut(
  Stroke2$age,
  breaks = idade.cortes,
  right = FALSE,
  include.lowest = TRUE
))

hist(Stroke2$age, breaks = idade.cortes, col = "lightblue", xlab = "Idade", ylab = "Frequência", main = "Histograma de Idades")







#1 (I)

