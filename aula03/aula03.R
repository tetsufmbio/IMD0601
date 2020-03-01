# Aula 03 - Bioestatística

########################

# If e Else

a <- 300
if( a >= 500 ) {
  print("escopo 1")
  
} else if(a < 100) {
  print("escopo 2")
  
} else {
  print("escopo 3")
  
}

######

ifelse(variavel >= 500, 'executa essa tarefa se TRUE', 'executa outra se FALSE')

######

ifelse(1:20 >= 10, 0, 10)

######

a <- 839
c <- ifelse(a >= 10000, 'VALOR ALTO', ifelse(a < 10000 & a >= 1000, 'VALOR MEDIO', 'VALOR BAIXO'))
c

########################

# Functions

func1 <- function(par1, par2){
  # sequência de tarefas
  result <- c(par1, par2)
  return(result)
}
# chamada da função

func1("A","B")

#####

montanha_russa <- function(palavra) {
  retorno <- NULL
  for(i in 1:nchar(palavra)) {
    if(i %% 2 == 0) {
      retorno <- paste0(retorno, tolower(substr(palavra, i, i)))
    } else {
      retorno <- paste0(retorno, toupper(substr(palavra, i, i)))
    }
  }
  return(retorno)
}

montanha_russa('teste de função: letras maiúsculas e minúsculas')

####################

# Matrix

m <- matrix(1:20, 4, 5)
m

m <- c(1:20)
dim(m) <- c(4,5)
m

# juntando matrizes pelas colunas
cbind(m, m)

# juntando matrizes pelas linhas
rbind(m, m)

# adicionando rótulos nas colunas
colnames(m) <- c("A","B","C","D","E")
m

# adicionando rótulos nas linhas
rownames(m) <- c("a","b","c","d")
m

# acessando os dados
m[1,2]  # linha 1, coluna 2
m[1,]   # primeira linha
m[,1]   # primeira coluna
m[,1:3] # primeira a terceira coluna

m["a","B"]
m["a",]
m[,"A"]
m[,c("A","B","C")]
col <- colnames(m)
m[,col[1:3]]

# juntando vetor de caracteres com matriz numérica
patients <- c("Bill","Gina","Kelly","Sean")
m2 <- cbind(patients,m)
m2

# Data.frame
m2 <- data.frame(patients,m)
m2
class(m2)

# acessando dados em Data.frame
m2[2,3]  # segunda linha, terceira coluna
m2[2,]   # segunda linha
m2[,2]   # segunda coluna
m2["a",] # primeira linha
m2[,"A"] # segunda coluna

m2$A     # coluna patients
m2$B     # coluna B

####################

# Abrindo arquivos no R

MyData <- read.csv(file="iris.data", sep=",", header=FALSE)
class(MyData)