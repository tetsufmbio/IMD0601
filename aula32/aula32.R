# Aula 32 - Regressao Linear

# Gerando dados sintéticos que possuem correlacao

x <- runif(50,0,50)
y <- x + rnorm(50,0,5)

plot(x,y)

##################################################################

# Correlacao
?cor

# Pearson
cor(x,y)
# Spearman
cor(x,y, method="spearman")
# Kendall
cor(x,y, method="kendall")

# Teste estatistico para correlacao
?cor.test

# Pearson
cor.test(x,y)
# Spearman
cor.test(x,y, method="spearman")
# Kendall
cor.test(x,y, method="kendall")

# visualização de dados relacionado a correlação

# Carregar data iris
data(iris)

# plot de todos os pares de variáveis
pairs(iris[,1:4])

# plot de todos os pares de variáveis ressaltando as espécies
pairs(iris[,1:4],col=iris[,5],oma=c(4,4,6,12))
par(xpd=T)
legend(0.85,0.6, as.vector(unique(iris$Species)),fill=c(1,2,3))

# heatmap de correlação entre pares de variáveis
if (!require("corrplot")) install.packages("corrplot")
library(corrplot)

# calcular correlação
correlations <- cor(iris[,1:4])

# criar plot de correlação
corrplot(correlations, method="circle")

##################################################################

# Regressao Linear

res.lm <- lm(y~x)
res.lm

plot(x,y)
lines(x,fitted(res.lm))

segments(x,fitted(res.lm),x,y,lty=2)

# Escreva aqui a fórmula da regressão linear:
#
#

# Recuperando o y estimado para cada valor em x 
fit<-fitted(res.lm)
fit

# Recuperando o resíduo (y estimado - y observado) para cada x
res<-resid(res.lm)
res

# A soma de fit e res retorna os valores originais de y
sumfr <- fit+res
compare<- data.frame(sumfr,y)
compare

# sumário dos resultado do modelo
summary(res.lm)
