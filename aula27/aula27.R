# Aula 27 - Teste de Hipótese

# Gerando uma distribuição qui-quadrado

# O qui-quadrado é gerado a partir de uma distribuição da soma do
# quadrado de k variáveis aleatórios obtidas de uma população que
# segue uma distribuição normal padrão. Vamos gerar uma distribuição
# qui-quadrado com k graus de liberdade:

# Gere primeiro dados de população com média 0 e desvio padrão 1
pop <- rnorm(1000) 

# Faça 1000 amostragens pegando em cada amostragem 6 observações. Para 
# cada amostragem, calcule a soma do quadrado e guarde em um vetor.
k <- 6
somaqd <- vector(length=1000)
for (i in 1:1000){
  sind <- sample(c(1:1000),k)
  sample <- pop[sind]
  somaqd[i] <- sum(sample*sample)
}

# plote o gráfico da distribuição da soma do quadrado;
hist(somaqd, prob=T)

# plote o gráfico da distribuição teórica utilizando a função
# dchisq
x <- seq(0,30,0.1)
points(x, dchisq(x,k))

# Eu consigo derivar uma distribuição qui-quadrado a partir
# de uma população que segue uma distribuição normal com 
# média=45 e desvio padrão=12?
pop <- rnorm(1000, 45, 12)
hist(pop)


# Consigo gerar uma distribuição qui-quadrado a
# partir de populações que não seguem uma distribuição
# normal?
pop <- runif(1000,0,100)
hist(pop)


# Derivando a distribuição qui-quadrado a partir
# de múltiplas amostragens dos dados:

pop <- rnorm(1000, 45, 12)
hist(pop)

k <- 6
somaqd <- vector(length=1000)
for (i in 1:1000){
  sind <- sample(c(1:1000),k)
  sample <- pop[sind]
  varsample <- var(sample)
  somaqd[i] <- ((k - 1)*var(sample))/var(pop)
}

hist(somaqd, prob=T)
x <- seq(0,30,0.1)
points(x, dchisq(x,k-1))

# Verifique se os dados da amostra abaixo possui uma
# variância igual a ou diferente de 144

sample2 <- c(53.43137,44.47348,54.96736,52.95158,22.39179,61.26472)
csq <- ((6-1)*var(sample2))/144
thr <- qchisq(c(0.0275,0.975),5)
x <- seq(0,30,0.1)
points(x, dchisq(x,6-1))

#
# Teste de Qui-quadrado de Pearson
#
#                   Contagem  Prob
# Amarela lisa      315       9/16
# Amarela rugosa    101       3/16
# Verde lisa        108       3/16
# Verde rugosa      32        1/16

count <- c(315,101,108,32)
prob <- c(9/16,3/16,3/16,1/16)
chisq.test(count,p=prob)

#################################################

# Distribuição F

# Distribuição obtido a partir da razão entre duas 
# variáveis aleatórios (U1 e U2) que seguem distribuição 
# qui-quadrado com graus de liberdade que podem 
# assumir valores distintos.

pop <- rnorm(1000)
hist(pop)

m <- 10
n <- 3
f <- vector(length=1000)
for (i in 1:1000){
  sind <- sample(c(1:1000),m)
  sample1 <- pop[sind]
  sind <- sample(c(1:1000),n)
  sample2 <- pop[sind]
  f[i] <- var(sample1)/var(sample2)
}

hist(f, prob=T, nclass=700, xlim=c(0,30))
x <- seq(0,30,0.1)
points(x, df(x,m-1,n-1))

#
# plotando diferentes distribuições F
#

x <- seq(.1,5,by=.005)
m <- c(1,5,10,30)
n <- c(1,5,10,30)
par(mfrow=c(4,4))
for (i in 1:4){
  for (j in 1:4) {
    plot(x,df(x,m[i],n[j]),type='l',ylab="f(x)",cex=.6)
    title(paste(paste("dof =",m[i]),n[j],sep=","))
  }
}

par(mfrow=c(1,1))

#
# Duas amostras vieram de populações de mesma variância?
#
sample1 <- c(0.3202093,-1.1564600,-0.2154121,-0.4226296 ,2.1147922,-0.4788028,4.0566918,1.0258892,0.2956120,-2.2341215,0.6427838,-0.7048288,0.9797139,-0.3260760,0.3930750,-1.0106097,1.1050497,-2.3513204,1.5981035,-1.6609447)
sample2 <- c(18.36736,21.51649,19.29048,18.56695,18.18547,19.14600,19.81284,19.63074,20.89732,21.54153)
var.test(sample1,sample2)

var(sample1)/var(sample2)


#
# O tratamento diminui a variabilidade dos resultados?
#

sample1 <- c(477.4190,382.8152,459.8454,468.8639,483.6886,430.5384,470.0114,415.7305,422.4704,400.5516,468.5312,483.7219,344.5798,462.2482,385.4191,362.2618,370.6543,373.2517,436.3845,436.5987)
sample2 <- c(446.2297,434.9100,438.0173,448.1294,438.3108,471.7969,457.5688,421.0577,446.8331,421.8211,462.4284,474.5542,406.6292,442.9286,422.8267)

