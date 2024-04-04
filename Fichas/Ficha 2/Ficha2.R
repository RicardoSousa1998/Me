

#FICHA 2

#1 (A)

Stroke2<- na.omit(stroke) 


#1 (B)

(ni.sex <- table(Stroke2$sex))

barplot(
  ni.sex,                                               
  main = "Sex",      
# xlab = "Sex",                                         
  ylab = "Frequências absolutas",                        
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
  ylab = "Frequências Relativas",                        
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
  ylab = "Frequências absolutas",                        
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
  ylab = "Frequências absolutas",                        
  col = 2:5,                                            
  #ylim = c(0, 200),                                      
  names.arg = names(StrokeAfterAVC$dgn)
)   


#1 (G)

## SEI LA



#1 (F)

#regra de Sturges
n <- length(Stroke2$age)
k <- 1 + 3.322 * log10(n)
k <- ceiling(k)  
