# Aula 23 - Estatística Inferencial

# Teorema Central do Limite

# Distribuição da média amostral e o efeito 
# do número de repetições dos dados

# Vamos plotar um gráfico de densidade de probabilidade de uma
# distribuição exponencial
e<-seq(.1,30,by=.1)
plot(e,dexp(e))

# Criando uma matriz de n linhas e 20 colunas;
# Cada linha i representa um conjunto de dados e
# a célula ij representa a amostra j do conjunto de dados i
cols<-20
n5<-matrix(rexp(cols*5,rate=0.25),nrow=5)
n20<-matrix(rexp(cols*20,rate=0.25),nrow=20)
n50<-matrix(rexp(cols*50,rate=0.25),nrow=50)
n200<-matrix(rexp(cols*200,rate=0.25),nrow=200)

# Calculando as médias das linhas
n5means<-rowMeans(n5)
n20means<-rowMeans(n20)
n50means<-rowMeans(n50)
n200means<-rowMeans(n200)

# Plotando os resultados
par(mfrow=c(1,4))
hist(n5means,xlab="",prob=T,xlim=range(0:12),ylim=range(0,0.2,0.4,0.6,0.8,1.0),main="n=5",nclass=10)
hist(n20means,xlab="",prob=T,xlim=range(0:12),ylim=range(0,0.2,0.4,0.6,0.8,1.0),main="n=20",nclass=10)
hist(n50means,xlab="",prob=T,xlim=range(0:12),ylim=range(0,0.2,0.4,0.6,0.8,1.0),main="n=50",nclass=10)
hist(n200means,xlab="",prob=T,xlim=range(0:12),ylim=range(0,0.2,0.4,0.6,0.8,1.0),main="n=200",nclass=10)

# Exercício
# Experimente gerar dados que siga uma distribuição diferente daquele
# exemplificado acima. Veja se a distribuição da média amostral nesses casos
# também acabam seguindo uma distribuição normal quando o número de repetições é 
# suficientemente alto.



# Distribuição da média amostral e o efeito do tamanho das
# amostras

# Criando uma matriz de 50 linhas e n colunas;
# Cada linha i representa um conjunto de dados e
# a célula ij representa a amostra j do conjunto de dados i
rows<-50
n5<-matrix(rexp(50*5,rate=0.25),nrow=50)
n20<-matrix(rexp(50*20,rate=0.25),nrow=50)
n50<-matrix(rexp(50*50,rate=0.25),nrow=50)
n200<-matrix(rexp(50*200,rate=0.25),nrow=50)

# Calculando as médias das linhas
n5means<-rowMeans(n5)
n20means<-rowMeans(n20)
n50means<-rowMeans(n50)
n200means<-rowMeans(n200)

# Plotando os resultados
par(mfrow=c(1,4))
hist(n5means,xlab="",prob=T,xlim=range(0:12),ylim=range(0,0.2,0.4,0.6,0.8,1.0),main="n=5",nclass=10)
hist(n20means,xlab="",prob=T,xlim=range(0:12),ylim=range(0,0.2,0.4,0.6,0.8,1.0),main="n=20",nclass=10)
hist(n50means,xlab="",prob=T,xlim=range(0:12),ylim=range(0,0.2,0.4,0.6,0.8,1.0),main="n=50",nclass=10)
hist(n200means,xlab="",prob=T,xlim=range(0:12),ylim=range(0,0.2,0.4,0.6,0.8,1.0),main="n=200",nclass=10)
     
par(mfrow=c(1,1))


# Distribuição t de Student

# Gerando 50 dados que seguem uma distribuição exponencial
# com 200 amostras e lambda(rate) = 0.25
n200<-matrix(rexp(200*50,rate=0.25),nrow=50)

# Calculando as médias de cada conjunto de dado (linha)
n200means<-rowMeans(n200)

# Calculando o t-score para cada média amostral.
# A média de uma população que segue a distribuição exponencial
# é lambda^-1
t<-(n200means- 4)/sd(n200means)

hist(t,prob=T,main="Standardized data wtih overlay of t-distribution")

# função dt(x, df) retorna a densidade de probabilidade dos valores em x
# com df grau de liberdade.
curve(dt(x,50),add=T)

# Plotando algumas distribuições t com diferentes graus de liberdade
x <- seq(-8,8,by=.1)
par(mfrow=c(1,4))
plot(x,dnorm(x),type='l',ylab="",main="df=2")
lines(x,dt(x,df=2),lty=2)
plot(x,dnorm(x),type='l',ylab="",main="df=5")
lines(x,dt(x,df=5),lty=2)
plot(x,dnorm(x),type='l',ylab="",main="df=10")
lines(x,dt(x,df=10),lty=2)
plot(x,dnorm(x),type='l',ylab="",main="df=20")
lines(x,dt(x,df=20),lty=2)

######################

# Qui-quadrado

# 10 amostras aleatórias de N(0,1), soma do  quadrado
s <- rnorm(10)
sum(s^2)

x <- rep(NA, 1000)
for (j in 1:1000){
  x[j] <-sum(rnorm(10)^2)}
hist(x)

par(mfrow=c(1,1))
h <- hist(x, plot=F)
ylim  <- range(0, h$density, 0.10)
hist(x, freq=F, ylim=ylim)
curve(dchisq(x,df=10), add = TRUE)