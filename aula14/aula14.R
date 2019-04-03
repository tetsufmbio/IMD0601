# aula14 - Distribuição de probabilidade univariada discreta

# no R existem funções que simulam e calculam as 
# probabilidades seguindo uma determinada distribuição.
# Para a distribuição Binomial, temos:

rbinom(10,10,0.5) 	# gera n resultados de m tentativas de 
					# probabilidade p seguindo a distribuição 
					# Binomial rbinom(n, m, p)
rbinom(10,1,0.5) 	# para simular uma distribuição Bernoulli

dbinom(0:10,10,0.16) # Calcula a probabilidade de k sucessos em
					 # n tentativas, com probabilidade p, seguindo
					 # a Binomial.
					 # dbinom(k, n, p)
					 
pbinom(0:10,10,0.16) # Calcula a probabilidade acumulada de k 
					 # sucessos, n tentativas e com probabilidade
					 # p seguindo a binomial
					 # pbinom(k, n, p)
					 
qbinom(c(0,0.25,0.5,0.75,1),10,0.16) # Retorna os q percentis da distribuição
									 # cumulativa de uma Binomial com n tentativas
									 # e probabilidade p
									 # qbinom(q, n, p)

# Exercício
#
# 1) Um a cada 20 crianças nascem com uma certa doença. Vamos supor que você
# tenha amostrado aleatoriamente 100 crianças. Apesar das crianças terem sido
# amostradas sem reposição, vamos assumir que a seleção de uma amostra é 
# independente da outra por estarmos amostrando crianças de uma população grande.
# Qual a probabilidade de encontrar 10 crianças com a doença?

# 2) Ainda em relação a questão 1, qual a probabilidade de encontrar até 10 crianças
# com a doença?

# 3) Ainda em relação a questão 1, qual a probabilidade de apenas 10 primeiras crianças 
# amostradas apresentarem a doença? 

# 4) Ainda em relação a questão 1, desenhe um gráfico da distribuição de probabilidade
# usando os parâmetros apresentados.

# 5) Ainda em relação a questão 1, desenhe um gráfico da distribuição acumulada de
# probabilidade usando os parâmetros apresentados.

#
# Distribuição de Poisson
# Da mesma forma que na distribuição binomial, podemos encontrar
# funções que simulam e calculam probabilidades de acordo com acordo
# a distribuição de Poisson, como:

n <- 2000		# número de experimentos
p <- 1/10000 	# probabilidade de erro
k <- 1 			# número de ocorrência do erro
lambda <- n*p

rpois(n, lambda)
dpois(k, lambda)
ppois(k, lambda)
qpois(c(0,0.25,0.5,0.75,1),lambda)

# Exercícios
# 1) Gere as probabilidades seguindo a distribuição binomial e Poisson
# utilizando o valor de k de 0 a 6 considerando: 
# a) n = 10, p = 0.20
# b) n = 20, p = 0.10
# c) n = 40, p = 0.05
# d) n = 80, p = 0.025

# 2) Compare os valores de probabilidades geradas no exercício anterior.