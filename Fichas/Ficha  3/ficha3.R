Seminario <- data.frame(
  nome = c("Engenheiros","Professores","Analistas","Alunos"),
  numero = c(32,20,16,12)
  
)

niNomes = table(Seminario$nome)
fiNomes = prop.table(niNomes)
NINomes = cumsum(niNomes)
FINomes = cumsum(fiNomes)

tfeq <- data.frame(
  xi=table(Seminario$numero),
  ni=as.integer(niNomes),
  fi=as.numeric(fiNomes),
  NI=NINomes,
  FI=as.numeric(FINomes)
)

show(tfeq)


mean(Seminario$numero)
median(Seminario$numero,type=2)



quantile(Seminario$numero,probs = c(0.25,0.50,0.75),type=2)
summary(Seminario$numero)

boxplot(Seminario$numero,range=0,col="red",horizontal = TRUE, xlab="anos",main = "Seminario")
