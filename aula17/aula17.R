# Distribuição multinomial

rmnomial <- function(N,n,p){
  l<-length(p)
  x<-rbinom(N,n,p[1])
  if(l==2)
    {cbind(x,-x+n)} 
  else 
    {cbind(x,rmnomial(N,-x+n,p[2:l]/sum(p[2:l])))}
}

# Definir N como sendo 10 amostragens
N<-10

# Definir n como sendo 20 amostras
n<-20

# Definir um vetor p contendo as probabilidades
# pAA=0.2, pAa=0.7 e paa=0.1
p<-c(0.2,0.7,0.1)
results<-rmnomial(N,n,p)
results

# Colocar na forma de proporção
results2<-results/n
results2

results<-rmnomial(10000,20,c(0.2,0.7,0.1))
## Transformar o resultado em proporção
results2<-results/n
## Armazenar a coluna 1 em X
X<-results2[,1]
## Armazenar a coluna 2 e 3 em W
W<-(results2[,2]+results2[,3])

## calcular médias de X e W
mean(X)
mean(W)

## gerar um histograma
hist(X,nclass=10,main="Simulated prop. AA using Multinomial")

## Simular 10000 valores de uma distribuição binomial
## Simular rv para n=20 e p=0.2
B<-rbinom(10000,20,0.2)
##Converter para proporção dividindo por 20
hist(B/20,nclass=10,main="Simulated prop. AA using Binomial")

#
# Distribuição normal multivariada
#

x<-seq(-2,2,length=20)
y<-x
bvn <- function(x,y){
  (1/2*pi)*exp(-0.5*(x^2+y^2))
}
z<-x%*%t(y)
for(i in 1:20){
  for(j in 1:20){
    z[i,j]<-bvn(x[i],y[j])
  }
}
persp(x,y,z)

## gerar aleatoriamente dados de uma distribuição normal multivariada
install.packages("mvtnorm")
library(mvtnorm)
data<-rmvnorm(1000,mean=c(0,0))
data

## scatter plot
plot(data)

## distribuição marginal
hist(data[,1])
hist(data[,2])

#
# Distribuição Dirichlet
#

rDir<-function(n,a){
  l<-length(a)
  m<-matrix(nrow=n,ncol=l)
  for(i in 1:l){
    m[,i]<-rgamma(n,a[i])
  }
  sum<-m%*%rep(1,l)
  m/as.vector(sum)
}
rDir(20,c(1,1,1))

install.packages("DirichletReg")
library(DirichletReg)
x<-rdirichlet(1000,c(1,2))
mean(x[,1])
mean(x[,2])
hist(x[,1], nclass=20)
hist(x[,2], nclass=20)
plot(x)
