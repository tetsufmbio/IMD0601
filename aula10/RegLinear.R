############# Regressao Linear #############
# IMD0601 - Bioestatistica
# profs. Beatriz Stransky e Tetsu Sakamoto

# Modelo de Regressao Linear para Doenca Pulmonar Obstrutiva Cronica (COPD). 

install.packages(c('tidyverse','DescTools', 'gmodels', 'corrplot', 'prediction'))
library(tidyverse)

# setwd()
# rm(list=ls())


## 1. Importaçao e inspeçao -------------------------------------
copd <- read.csv('copd_dataset.csv', header=TRUE, sep=',', na.strings=c('NA', ''))

colnames(copd)
summary(copd)
view(copd)


## 2. Pre-processamento -----------------------------------------

# verificar variaveis binarias (presença=1, ausencia=0)
unique(copd$smoking)
# referencia padrao: grupo menor valor de codificaçao. Ajustar para 0 e 1.
copd$smoking[copd$smoking==2] <- 0

# criar novas variaveis
comorbid <- length(copd$Diabetes)
comorbid[copd$Diabetes==1 | copd$muscular==1 | copd$hypertension==1 | copd$AtrialFib==1 | copd$IHD==1] <- 1
comorbid[is.na(comorbid)] <- 0

DAF <- copd$Diabetes * copd$AtrialFib # necessario que ambas sejam numericas
copd$DAF <- DAF 

# ajustar tipo (taming)
copd$gender <- as.factor(copd$gender) 
copd$comorbid <- as.factor(comorbid)

levels(copd$gender)
# gender <- relevel(copd$gender, ref='1') 


## 3. Estatistica descritiva ------------------------------

# verificar variavel resposta 
# outras: FEV1, SQRQ, HAD
sum(is.na(copd$MWT1Best))
which(is.na(copd$MWT1Best))
copd <- copd %>% drop_na(MWT1Best) # na.omit(copd), complete.cases(copd)
summary(copd$MWT1Best)
hist(copd$MWT1Best)

# MWT1best - distancia caminhada
library(DescTools) 

hist(copd$MWT1Best, main='Histograma de MWT1Best', xlab='MWT1Best', breaks=12)
Desc(copd$MWT1Best)

subset(copd, MWT1Best > 650)
subset(copd, MWT1Best > 600 | MWT1Best < 150)

# FEV1 - funçao pulmonar
Desc(copd$FEV1)
plot(copd$FEV1, copd$MWT1Best, xlab='FEV1', ylab='MWT1Best') 

# AGE
Desc(copd$AGE)
plot(copd$AGE, copd$MWT1Best, xlab='AGE', ylab='MWT1Best') 

# FVC - capacidade vital forçada
Desc(copd$FVC)
plot(copd$FVC, copd$MWT1Best, xlab='FVC', ylab='MWT1Best') 

# algum dos graficos indicou uma distribuiçao nao-normal ou relaçao nao-linear?

# tabulaçao cruzada
library(gmodels)
CrossTable(copd$COPDSEVERITY) # idem var. numericas inteiras
CrossTable(copd$copd, copd$smoking, expected=TRUE)

table(copd$comorbid)
t_comorbid <- table(copd$smoking, copd$comorbid, exclude=NA)
t_comorbid
t_comorbid <- addmargins(round(100*prop.table(t_comorbid)))
t_comorbid


# 4. Teste de Correlaçao --------------------------
# o coef de Spearman considera a classificação dos valores, mas cor.test ignora as classificaçoes para encontrar os valores p. 
# para evitar o aviso “não é possível calcular o valor p exato com empates”, use exact = FALSE.
# alguma das analises indicou uma distribuiçao nao-normal?

cor.test(copd$FEV1, copd$MWT1Best, use='complete.obs', method='pearson')
cor.test(copd$FEV1, copd$MWT1Best, use='complete.obs', method='spearman', exact=FALSE)

cor.test(copd$AGE, copd$MWT1Best, use='complete.obs', method='pearson')
cor.test(copd$AGE, copd$MWT1Best, use='complete.obs', method='spearman', exact=FALSE)

# Matriz de Correlaçao
# como decidir qual das variaveis colineares incluir no modelo final e porque?
copd2 <- copd %>%
     select(c(AGE, PackHistory, FEV1, FEV1PRED, FVC, CAT, HAD, SGRQ))
cor_matrix <- round(cor(copd2, method='spearman', use='complete.obs'), 2)
cor_matrix

# graficos correlaçao
library(corrplot)

pairs(copd2)
corrplot(cor_matrix, method='circle')


# 5. Regressao Linear Simples -----------------------
# Y = α + β*X 

# Preditor numerico
Best_FEV1 <- lm(MWT1Best ~ FEV1, data=copd)
summary(Best_FEV1)
confint(Best_FEV1)

Best_FEV1
Best_FEV1$call
Best_FEV1$coefficients
Best_FEV1$residuals

plot(copd$FEV1, copd$MWT1Best, xlab='FEV1', ylab='MWT1Best')
lines(copd$FEV1, fitted(Best_FEV1), col='red')
segments(copd$FEV1, fitted(Best_FEV1),copd$FEV1, copd$MWT1Best, lty=2)

Best_FVC <- lm(MWT1Best ~ FVC, data=copd)
summary(Best_FVC)
confint(Best_FVC)
# o modelo com FEV1 explica a maior variancia dos dados (maior R2 ajustado). 
# justificativa para usar FEV1 em vez das outras 3 variaveis.

# Preditor categorico
Best_gender <- lm(MWT1Best ~ gender, data=copd)
summary(Best_gender)
confint(Best_gender)

plot(copd$gender, copd$MWT1Best, xlab='gender', ylab='MWT1Best')


## Graficos (premissas)

# Plot1, grafico de variancia constante: homogeneidade da variancia e da relaçao linear. 
# Plot2, grafico Q-Q: verifica se residuos seguem distribuiçao normal. 
# Plot3: heterogeneidade da variancia.
# Plot4: heterodasticidade e pontos que tem um grande impacto nos coef de regressao.
par(mfrow=c(2,2)) 
plot(Best_FEV1)

# se as premissas sao satisfeitas, os residuos seguem distribuiçao normal com media zero e variancia constante entre os valores do preditor.
#  Residuals ~ Normal (0, σ2)
predictVals <-  predict(Best_FEV1)
residualVals <- residuals(Best_FEV1)
hist(residualVals, main='Histogram of residuals', xlab='Residuals') 


# 6. Regressao Linear Multipla -------------------------
# pode ocorrer alteraçao no p-valor dos coeficientes entre modelo simples e multiplo.
# Y = α + β1*X1 + β2*X2 + ε

Best_FEVAGE <- lm(MWT1Best ~ FEV1 + AGE, data=copd)
summary(Best_FEVAGE)
confint(Best_FEVAGE)

Best_FAC <- lm(MWT1Best ~ FEV1 + AGE + comorbid, data=copd)
summary(Best_FAC)
confint(Best_FAC)

Best_multi <- lm(MWT1Best ~ FEV1 + AGE + gender + COPDSEVERITY + comorbid, data=copd)
summary(Best_multi)
confint(Best_multi)

# Interaçao entre preditores 
# quais outras interaçoes podem ser investigadas?
Best_DAF <- lm(MWT1Best ~ factor(Diabetes)+factor(AtrialFib)+factor(DAF), data=copd) 
summary(Best_DAF)
confint(Best_DAF)

# Coeficientes regressao
library(prediction)

list('Diabetes' = prediction(Best_DAF, at=list(Diabetes=c(0, 1))), 
     'AtrialFib' = prediction(Best_DAF, at=list(AtrialFib=c(0, 1))), 
     'DAF' = prediction(Best_DAF, at=list(Diabetes=c(0, 1), AtrialFib=c(0, 1))))

#############################