############ Regressao Linear ############
# IMD0601 - Bioestatistica
# profs. Beatriz Stransky e Tetsu Sakamoto

# Modelo de Regressao Linear para Doenca Pulmonar Obstrutiva Cronica (COPD). 
# Usando o COPD_dataset, crie um modelo de regressao multipla usando 3 variaveis preditoras, 
# sendo pelo menos 1 numerica e 1 categorica, seguindo as etapas abaixo. 
# Interprete os coeficientes, p-valor e R2 para o modelo criado.  
# Use uma variavel dependente diferente da MTW1Best.

install.packages('tidyverse')
library(tidyverse)

## 1. Importaçao e inspeçao -------------------------------------


## 2. Pre-processamento -----------------------------------------
# verificar variaveis binarias (codificaçao e referencia)
# criar novas variaveis
# ajustar tipo (taming)


## 3. Estatistica descritiva ------------------------------
# verificar variavel resposta 
# variaveis preditoras candidatas
# tabulaçao cruzada
# alguma indicaçao de distribuiçao nao-normal ou relaçao nao-linear?


# 4. Teste de Correlaçao --------------------------
# coefs. Pearson ou Spearman
# Matriz de Correlaçao
# graficos correlaçao
# identificaçao de colineriedade?


# 5. Regressao Linear Simples -----------------------
# Y = α + β*X 
# Preditor numerico
# Preditor categorico
# verificaçao das premissas (graficos)


# 6. Regressao Linear Multipla -------------------------
# Y = α + β1*X1 + β2*X2 + ε
# checar interaçao entre preditores 


#############################