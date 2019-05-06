# Random walk de passos (+1,-1)
Walk1d <- function(n) {
  y <- vector(length=n)
  y[1] <- 0
  for (i in 2:n){ 
    y[i] <- y[(i-1)] + sample(c(-1,1),1)
  }
  y
}

# Repetindo o experimento 100 vezes:
n <- 100
y <- Walk1d(n)
plot(1:n,y,type='l',ylim=c(-30,30))

for (i in 1:100){
  y <- Walk1d(n)
  lines(1:100,y,type='l')
}

# Exercício:
# Pegue o resultado final dos 100 experimentos e plote um
# histograma.


# Random walk com duas dimensões
par(mfrow=c(2,2))
Walk2d<-function(){
  xstart <- 0
  ystart <- 0
  xmove <- sample(c(-1,1),500,repl=T)
  ymove <- sample(c(-1,1),500,repl=T)
  xmove <- xstart + cumsum(xmove)
  ymove <- ystart + cumsum(ymove)
  plot(xmove,ymove,xlim=c(-40,40),ylim=c(-40,40),xlab="x",ylab="y",type='l')
}
for (i in 1:4) Walk2d()

## Operação de matriz em R

# Declarando uma matriz
a<-matrix(c(1,2,3,4),nrow=2,ncol=2)
a

# Declarando uma segunda matriz
b<-matrix(c(4,3,2,1),nrow=2,ncol=2)
b

# Multiplicando a matriz
a%*%b

# Note que o uso a*b não é correto
a*b

# Caso simples de cadeia de markov

# Exercício:
# Crie uma matriz representando a matriz de transicao
# que represente as probabilidades dos movimentos do sapo
# nas folhas da vitória-régia



# Exercício:
# Multiplique a matriz de transição com ela mesma para 
# obter a próxima matriz de transição.


# Comportamento da matriz de transicao ao longo do intervalo
# de tempo:
frogn <- frog1
frogn <- frogn%*%frog1
frogn

# Incorporando a distribuição a priori na cadeia de markov:
start0<-matrix(c(0.5,0.5),nrow=1,ncol=2)
start0

start1<-start0%*%frog1
start1

startn <- start1
startn <- startn%*%frog1
startn

# Exercício
# Se a distribuição a priori for p(A) = 0.9 e p(B) = 0.1,
# como se comporta a matriz de transição ao longo do intervalo
# de tempo?
