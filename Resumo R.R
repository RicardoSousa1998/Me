#'*RESUMO*

#Para ver info usar ?comando exmplo = ?c()


#Vector -> c()
genero <- c("feminino", "masculino")
#Matriz -> matrix()
#Array -> array()
#Lista -> list()
#Tabela -> data.frame()
tabela <- data.frame(genero = genero,idade=c(20,18))

#'*FICHEIROS* 
#'*Import*

#ficheiros de texto (.txt, .csv) -> read.table(file,header=FALSE,sep="",dec=".",...) |read.csv(file,...)
#folhas de cálculo (.xls, .xlsx) -> library(readxl)  & read_excel(file.shet=null,range=null,col_name=true,...)
#Rstudio File -> Import Dataset -> From Excel...”

##Export##

#ficheiros de texto -> write.table(dados,file="dados.txt",quote=FALSE,row.names=FALSE)
#folhas de cálculo -> write.csv(dados,file="csv")

#'*TABELAS*

dados <- data.frame(
  i = 1:10,
  dieta = c("Sim", "Sim", "Não", "Sim", "Sim", "Não", "Sim", "Não", "Não", "Sim"),
  intensidade = c("Moderada", "Elevada", "Baixa", "Moderada", "Elevada", "Baixa", "Baixa", "Baixa", "Elevada", "Baixa"),
  suplementos = c(3, 2, 5, 6, 6, 3, 4, 4, 7, 3),
  ferro = c(14.3, 7.8, 27.0, 11.0, 9.9, 14.5, 15.4, 20.8, 10.5, 15.9)
)

#'*informaÇão da tabela*

view(dados) #Ver Tabela
head(dados) #ver 1ºs linhas
tail(dados) #ver ultimas linhas
names(dados) #ver nome dos vars
str(dados) #ver estratura da tabela
dim(dados) #nº linhas e colunas
nrow(dados) #nº linhas
ncol(dados) #nº colunas

#'*ver dados*

dados[1,2] #elemento linha 1 col 2
dados[1,] #linha 1
dados[,2] #coluna 2
dados[,c(2,4)] #coluna 2 e coluna 4
dados[,2:4] #coluna 2,3,4
dados[c(2,4),] #linha 2 e 4
dados[2:4] #linha 2,3,4


dados$dieta #variavel dieta
dados[,c("deita","ferro")]  #variaveis dieta e ferro

dados[dados$dieta=="sim",] #where dieta = sim
dados[dados$suplementos>3,] #where suplementos >3

#'*operadores lógicos no R:*
# igual : == 
# diferente: ! = 
# maior: > 
# maior ou igual: >= 
# menor: < 
# menor ou igual: <=
# pertencer: %in%
# ou: |
# e: &

#'*Vetor*

dados$dieta #cada var é um vetor
v<c(15,14,7,8) #criar um vetor


leght(dados$deita) #tamanho do vetor 


#'*NA VALUES*

dados2 <-data.frame(var1=2:5,var2=c(1,NA,6,12))

any(is.na(dados2)) #verificar se existe NA values

sum(dados2$var2 ,na.rm=TRUE)  #CONTAS sem na values

dados3<- na.omit(dados2) #Retirar linhas com u ou mais na


