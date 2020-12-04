############ Regressao Logistica ############
# IMD0601 - Bioestatistica
# profs. Beatriz Stransky e Tetsu Sakamoto

# Modelo de Regressao Logistica para Diabetes Melitus
# Diabetes dataset: http://biostat.mc.vanderbilt.edu/wiki/Main/DataSets


install.packages(c('tidyverse','DescTools'))
library(tidyverse)
library(DescTools)

# setwd()
# rm(list=ls())

## 1. Importaçao e inspeçao --------


## 2. Pre-processamento --------
# verificar variaveis binarias (codificaçao e referencia)
# criar novas variaveis
# ajustar tipo (taming)
# transformar variaveis continuas em nominais

# insurance: 0=none, 1=government, 2=private
# fh: family history of diabetes (1=yes, 0=no)
# smoking: 1=current, 2=never, 3=ex


## 3. Estatistica descritiva ------------------------------

# verificar variavel resposta (dm)
# se nao for binaria, decidir o que fazer: excluir ou combinar valores? Reg ordinal? 
# codificaçao: if HbA1c ≥ 7.5, dm=1
# tabulaçao cruzada com variavel resposta 


## 4. Regressao Logistica Simples -------------

# modelo nulo diabetes (so um coeficinte)
# como R esta codificando dm?

# Preditor numerico
# calcular log chance de diabetes pelo preditor numerico
# grafico

# Preditor categorico
# calcular log chance de diabetes pelo preditor categorico 
# grafico 

# mude a categoria de referencia do preditor categorico. 
# o que acontece?


## 5. Correlaçao entre preditores  ------------------
# coef. correlação de Pearson entre duas variaveis continuas aprox. normalmente distribuidas 

## 6. Regressao Logistica Multipla ------------------------




# Questões--------------------------------
# Quantos valores ausentes existem?
# Você acha que os erros padrão são pequenos ou grandes?
# Interprete cada um dos coeficientes nos modelos simples e multiplo. 
# Os odds ratios para alguma variavel mudou quando você incluiu outros preditores como preditores no seu modelo múltiplo, em relaçao ao valor apresentado no modelo de regressão logística simples? Porquê?