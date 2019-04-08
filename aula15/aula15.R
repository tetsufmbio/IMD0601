# aula 15

# Distribuição normal

# função densidade de probabilidade
x<-seq(-10,10,length=100)
plot(x,dnorm(x,0,1),xlab="x", ylab="f(x)", type='l', main="Normal PDF")

# função cumulativa de probabilidade
plot(x,pnorm(x,0,1),xlab="x",ylab="f(x)", type='l', main="Normal CDF scale
     1")

# gráficos usando o ggplot
# Função densidade de probabilidade
ggplot(data = data.frame(x = c(-3, 3)), aes(x)) + 
  stat_function(fun = dnorm, n = 101, args = list(mean = 0, sd = 1)) + 
  ylab("") +
  scale_y_continuous()

# Função cumulativa de probabilidade
ggplot(data = data.frame(x = c(-3, 3)), aes(x)) + 
  stat_function(fun = pnorm, n = 101, args = list(mean = 0, sd = 1)) + 
  ylab("") +
  scale_y_continuous()

# Exercício
# 1) Gere 100 amostras de dados que segue uma distribuição normal usando 
# a função rnorm e gere uma figura qqplot para verificar se os dados 
# seguem uma normal. Repita o procedimento mais 4 vezes, e compare os 
# gráficos gerados.


# 2) Sabemos aproximadamente 95% dos dados que seguem uma distribuição
# estão entre (média - 2*desvio padrão) e (média + 2*desvio padrão). 
# Certifique-se disto usando as funções da distribuição normal disponíveis
# no R.


# Distribuição gama

x<-seq(0,30,length=100)
par(mfrow=c(1,1))
plot(x,dgamma(x,shape=1,scale=1), type='l',xlab="x", ylab="Prob")
lines(x,dgamma(x,shape=2,scale=1), type='l',xlab="x", ylab="Prob", lty=2)
lines(x,dgamma(x,shape=5,scale=1), type='l',xlab="x", ylab="Prob", lty=3)
lines(x,dgamma(x,shape=10,scale=1), type='l',xlab="x", ylab="Prob", lty=4)
legend(x=6,y=.9,paste("Shape=",c(1,2,5,10)),lty=1:4)

x <- seq(0,30,length=100)
plot(x,dgamma(x,shape=2,scale=1), type='l',xlab="x", ylab="f(x)", main="Gamma pdf's")
lines(x,dgamma(x,shape=2,scale=2),lty=2)
lines(x,dgamma(x,shape=2,scale=4),lty=3)
lines(x,dgamma(x,shape=2,scale=8),lty=4)
legend(x=20,y=.3,paste("Scale=",c(1,2,4,8)),lty=1:4)

# Exercício
# 1) Gere 100 amostras de dados que segue uma distribuição gama usando 
# a função rgamma e gere uma figura qqplot para verificar se os dados 
# seguem uma normal. Repita o procedimento mais 4 vezes, e compare os 
# gráficos gerados.


# 2) Verifique se os dados abaixo no vetor data segue uma distribuição normal
data<-c(4.75, 3.4, 1.8, 2.9, 2.2, 2.4, 5.8, 2.6, 2.4, 5.25)


# 3) Calcule os parâmetros alfa e beta da distribuição gama para os dados
# do exercício 2


# 4) Verifique os dados em um gráfico qqplot de uma distribuição gamma com 
# os parâmetros definidos no exercício anterior.
qqplot(qgamma(ppoints(100),alfa,beta), data)
qqline(data, distribution=function(p) qgamma(p,alfa,beta))


# Distribuição beta
# alterando os valores de shape1
x <- seq(0,1,length=100)
plot(x,dbeta(x,2,8), type='l',xlab="x", ylab="f(x)", main="Beta pdf's")
lines(x,dbeta(x,2,4),lty=2)
lines(x,dbeta(x,2,2),lty=3)
lines(x,dbeta(x,2,1),lty=4)
legend(x=20,y=.3,paste("Scale=",c(1,2,4,8)),lty=1:4)

# alterando os valores de shape2
x <- seq(0,1,length=100)
plot(x,dbeta(x,8,2), type='l',xlab="x", ylab="f(x)", main="Beta pdf's")
lines(x,dbeta(x,4,2),lty=2)
lines(x,dbeta(x,2,2),lty=3)
lines(x,dbeta(x,1,2),lty=4)
legend(x=20,y=.3,paste("Scale=",c(1,2,4,8)),lty=1:4)

# Exercício
# Escreva uma função que determine os parâmetros alfa e beta de
# uma distribuição beta.
betaparam <- function(data){
  # seu código aqui

  params<-c(alfa,beta)
  return(params)
}
betaparam(data)
