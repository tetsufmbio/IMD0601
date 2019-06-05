# Aula 28 - Testes de hipótese III

######################################
#
# Testes não paramétricos (Teste de Wilcoxon)
#

# Exemplo 1 (uma amostra)

A <- c(18.3,13.3,16.5,12.6,9.5,13.6,8.1,8.9,10.0,8.3,7.9,8.1,13.4)
wilcox.test(A, mu=15)


# Exemplo 2 (duas amostras pareadas)

A <- c(18.3,13.3,16.5,12.6,9.5,13.6,8.1,8.9,10.0,8.3,7.9,8.1,13.4)
N <- c(12.7,11.1,15.3,12.7,10.5,15.6,11.2,14.2,16.3,15.5,19.9,20.4,36.8)
wilcox.test(A, N, paired=T)

# Comparando com teste T
t.test(A,N, paired=T)

# Exemplo 3 (duas amostras independentes)

na <- c(8.50,9.48,8.65,8.16,8.83,7.76,8.63)
ca <- c(8.27,8.20,8.25,8.14,9.00,8.10,7.20,8.32,7.70)
wilcox.test(na,ca)

# gráficos de distribuição Wilcoxon 
x <- seq(1,100)
plot(x,dwilcox(x,10,10))
plot(x,pwilcox(x,10,10))
qwilcox(c(0.025,0.975),10,10)

######################################

#
# Análise de tabela de contingência 
#

contingencyTestData<-matrix(c(45,67,122,38),nr=2,dimnames=list("Gene"=c("Allele 1","Allele 2"),"Disease"=c("Yes","No")))
contingencyTestData

# teste qui-quadrado
chisq.test(contingencyTestData)

contingencyTestData<-matrix(c(2,2,2,2),nr=2,dimnames=list("Correct"=c("Yes","No"),"cup"=c("milk","tea")))
contingencyTestData

# teste exato de fisher
fisher.test(contingencyTestData)


