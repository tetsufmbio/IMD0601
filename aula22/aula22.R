# Gerador de números que segue uma distribuição
# FDA inversa

# função que gera dados que segue uma distribuição exponencial
simExp <- function(n,lambda){
  u<-runif(n)
  x<-(-1/lambda)*log(1-u)
}

x1<-simExp(1000,2)
x2<-rexp(1000,2)
par(mfrow=c(1,2))
hist(x1,30,main="Inverse CDF")
hist(x2,30,main="Direct Simulation")

# Amostragem por rejeição
x <- runif(5000,0,1) # Step 1
u <- runif(5000,0,1.5) # Step 2
fx <- dbeta(x,2,2) # Step 3
acceptx <- x[fx > u] # Step 4

par(mfrow=c(1,1))
hist(acceptx,prob=T,ylim=c(0,1.8))
points(x,fx)


plot(x,u)
points(x,fx, col="red")
plot(x[fx > u],u[fx > u])
hist(acceptx,prob=T,ylim=c(0,1.8))
points(x,fx)

######################################

# Algoritmo de Metropolis

# Exemplo 

nchain<-1000
y<-3
n<-5
theta<-vector(length=nchain)
theta[1]<-0.04

for(i in 2:nchain){
  thetastar<-runif(1)
  r<-thetastar^y*(1-thetastar)^(n-y)/(theta[i-1]^y*(1-theta[i-1])^(n-y))
  u<-runif(1)
  if(u<r){
    theta[i]<-thetastar
  }
  else {
    theta[i]<-theta[i-1]
  }
}

par(mfrow=c(3,1))
plot(1:100,theta[1:100],main="First 100 Runs")
lines(1:100,theta[1:100])
plot(1:1000,theta[1:1000],main="All Runs")
lines(1:1000,theta[1:1000])
plot(901:1000,theta[901:1000],main="Last 100 Runs")
lines(901:1000,theta[901:1000])

par(mfrow=c(1,1))
hist(theta[101:1000],nclass=25,prob=T)
xx <- (1:100)/100
lines(xx,dbeta(xx,4,3))
p<-mean(theta[101:1000])
p


#####################################

# Exemplo 2
# Em uma pesquisa genética, um pesquisador quer saber a frequência
# dos alelos de um gene. O gene de interesse possui dois alelos e 
# um deles é dominante em relação ao outro. Em 40 pessoas, observou-se
# que 30 deles possuem o fenótipo do alelo dominante. Qual a frequência
# alélica deste gene?

nchain<-10000
y<-30
n<-40
theta<-vector(length=nchain)
theta[1]<-0.04

for(i in 2:nchain){
  thetastar<-runif(1)
  r<-(thetastar^2+2*(thetastar)*(1-thetastar))^y*((1-thetastar)^2)^(n-y)/((theta[i-1]^2+2*(theta[i-1])*(1-theta[i-1]))^y*((1-theta[i-1])^2)^(n-y))
  u<-runif(1)
  if(u<r){
    theta[i]<-thetastar
  }
  else {
    theta[i]<-theta[i-1]
  }
}

par(mfrow=c(3,1))
plot(1:100,theta[1:100],main="First 100 Runs")
lines(1:100,theta[1:100])
plot(1:1000,theta[1:1000],main="All Runs")
lines(1:1000,theta[1:1000])
plot(901:1000,theta[901:1000],main="Last 100 Runs")
lines(901:1000,theta[901:1000])

par(mfrow=c(1,1))
hist(theta[101:10000],nclass=25,prob=T)
p<-mean(theta[101:10000])
p

##############################

# Exemplo 3

# O sistema ABO de sangue é codificado por um gene que possui 
# três alelos. Os alelos A e B são codominantes entre eles e 
# o alelo O é recessivo. Um pesquisador possui dados de tipo
# sanguineo de 2000 pessoas. Tendo estes dados em mãos, faça um
# modelo que permita determinar a frequência alélica de cada alelo.

nchain<-1000
A<-750
B<-250
AB<-75
O<-925
n<-2000

p<-vector(length=nchain)
q<-vector(length=nchain)
r<-vector(length=nchain)

p[1]<-0.01
q[1]<-0.01
r[1]<-0.98

prior <- function(prob){
  if((prob<=0) || (prob>=1)){  # || here means "or"
    return(0)}
  else{
    return(1)}
}

for(i in 2:nchain){
  sd<-0.1
  pstar<-rnorm(1,0,sd)+p[i-1]
  qstar<-rnorm(1,0,sd)+q[i-1]
  rstar<-1-pstar-qstar
  #pstar<-runif(1,0.001,0.998)
  #qstar<-runif(1,0.001,0.999-pstar)
  #rstar<-1-pstar-qstar
  
  if(prior(rstar) == 0 || prior(qstar) == 0 || prior(pstar) == 0 ){
    p[i]<-p[i-1]
    q[i]<-q[i-1]
    r[i]<-r[i-1]
    next
  } else {
    R<-(A*log(pstar^2+2*pstar*rstar)+B*log(qstar^2+2*qstar*rstar)+AB*log(2*pstar*qstar)+O*log(rstar^2))-(A*log(p[i-1]^2+2*p[i-1]*r[i-1])+B*log(q[i-1]^2+2*q[i-1]*r[i-1])+AB*log(2*p[i-1]*q[i-1])+O*log(r[i-1]^2))  
    u<-runif(1)
    if(log(u)<R){
      #if(R>0){
      p[i]<-pstar
      q[i]<-qstar
      r[i]<-rstar
    }
    else {
      p[i]<-p[i-1]
      q[i]<-q[i-1]
      r[i]<-r[i-1]
    }
  }
  
}

par(mfrow=c(3,1))
plot(1:100,p[1:100],main="First 100 Runs",ylim=c(0,1))
lines(1:100,p[1:100])
plot(1:1000,p[1:1000],main="All Runs",ylim=c(0,1))
lines(1:1000,p[1:1000])
plot((nchain-100):nchain,p[(nchain-100):nchain],main="Last 100 Runs",ylim=c(0,1))
lines((nchain-100):nchain,p[(nchain-100):nchain])

par(mfrow=c(1,1))
hist(p[(nchain*0.1):nchain],nclass=25,prob=T)

mean(p[100:1000])
mean(q[100:1000])
mean(r[100:1000])
