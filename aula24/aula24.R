# Aula 24 - Estatística Inferencial II

# Estimativa pela Máxima Verossimilhança

# Um uso bem comum da estimativa da MV na genética é
# estimar a frequência alélica de um gene em uma população 
# que se encontra em equilíbrio de Hardy-Weinberg.
# Para um gene com dois alelos ("A" e "a"), a frequência dos
# genótipos será p^2 (AA), 2pq (Aa) e q^2 (aa).
# A verossimilhança para este problema segue uma distribuição
# multinomial onde V(p) é proporcional a:

# p^(2nAA)*(2p(1-p))^(nAa)*(1-p)^(2naa)

# Podemos escrever esta função na forma logaritmica:

# log(V(p)) = 2nAA*log(p)+nAa*log(2p(1-p))+2naa*log(1-p)

# Suponha que nós coletamos dados de 500 indivíduos de uma
# população que está em equilíbrio de HW e obtivemos a seguinte
# quantidade de cada genótipo:
# AA - 134
# Aa - 266
# aa - 100

# Qual seria a estimativa da máxima verossimilhança para o parâmetro
# p dado os dados?

#Criando um vetor com os valores de p
p<-1:1000/1000

# Criando um vetor que armazenará os valores log da verossimilhança
lik<-vector(length=1000)

#Entrando com os dados
n1<-134 # nAA
n2<-266 # nAa
n3<-100 # naa

#Dado os dados, avaliar a verossimilhança para cada valor de p
for(i in 1:1000){
  lik[i]<-2*n1*log(p[i])+n2*log(2*p[i]*(1-p[i]))+2*n3*log(1-p[i])
}

# Utilizando a função which para determinar o valor máximo da verossimilhança
which(lik==max(lik))

# Valor máximo da verossimilhança encontrado  
lik[534]

# Estimativa da máxima verossimilhança para p
p[534]

# Plotar o resultado
plot(p,lik,xlab="p",ylab="log likelihood", main="MLE estimation for p")
abline(v=p[534],lty=2)
legend(x=0.54,y=-2000,legend="MLE p=0.534")

# Exercício

# O sistema ABO de sangue é codificado por um gene que possui 
# três alelos. Os alelos A e B são codominantes entre eles e 
# o alelo O é recessivo. Um pesquisador possui dados de tipo
# sanguineo de 2000 pessoas. Tendo estes dados em mãos, utilize
# a máxima verossimilhança para a frequência alélica de cada alelo.

A<-750
B<-250
AB<-75
O<-925

################################

# Intervalo de Confiança

# 20 amostras aleatórias de uma distribuição normal padrão
x<-rnorm(20,0,1)

# Calcular média e desvio padrão
xBar<-mean(x)
s <- sd(x)
se<- s/(20^.5)
xBar
se

# Distribuição da média de uma amostragem pequena pode 
# ser modelada utilizando a distribuição t com n-1 grau
# de liberdade (pois precisamos estimar o desvio padrão).
# Considerando alfa=0.05 e distribuindo o alfa nas duas
# extremidades da distribuição:
test<-qt(0.975,19)
test

# Calculando as margens do IC95:
clLower<-xBar-se*test
clLower
clUpper<-xBar+se*test
clUpper

##################################

# Bootstrapping

# criando dados irregulares
x<-rnorm(100,3,4)
y<-rnorm(100,18,3)
all<-c(x,y)
hist(all,nclass=20)

n<-length(all)
nsample=1000
# criar matriz que conterá as amostragens do bootstrap
# cada linha representa uma amostragem do bootstrap;
# cada coluna representa a posição das amostras em cada amostragem.
bootsamples<-matrix(nrow=nsample,ncol=n)

# realizar a amostragem do bootstrap. 
for(i in 1:nsample){
  bootsamples[i,]<-sample(all,n,replace=T)
}

# criar vetor que armazenará as medianas
bootmedstar<-vector(length=nsample)

# calcular a mediana das amostras do bootstrap
for(i in 1:nsample){
  bootmedstar[i]<-median(bootsamples[i,])
}

# verificar a distribuição das medianas
hist(bootmedstar,nclass=20,xlab="median",main="Dist. of bootstrap sample medians")

# calcular o desvio padrão das medianas
sd(bootmedstar) 
# este seria erro padrão do bootstrap para a mediana amostral;
# ele pode servir como uma medida de dispersão do valor central da
# expressão gênica dos dados originais.
