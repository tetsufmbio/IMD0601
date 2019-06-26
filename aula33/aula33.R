# Aula 33 - Regressão linear múltipla

# Problema
# Medir precisamente a porcentagem de gordura corporal é um processo
# custoso e difícil. Nestes dados, você vai encontrar as medidas de 
# porcentagem de gordura corporal em 250 homens e outras medidas que
# são facilmente obtidos. Será que é possível predizer a porcentagem
# da gordura corporal de forma precisa utilizando estas outras medidas?

# Fonte do dataset:
# https://dasl.datadescription.com/datafile/bodyfat/?_sfm_methods=Multiple+Regression&_sfm_cases=4+59943

# carregar o arquivo bodyfat.txt
data <- read.csv("bodyfat.txt", sep="\t")
summary(data)

# Verificando as correlações entre as variáveis:

cor(data)

pairs(data)

if(!require(PerformanceAnalytics)) install.packages("PerformanceAnalytics")
library(PerformanceAnalytics)
chart.Correlation(data, 
                  method="pearson",
                  histogram=TRUE,
                  pch=16)

##########################################################

# Construir o modelo de regressão linear

library(dplyr)
# retirar a coluna Density
data<-select(data,-Density)

# Devido a colinearidade entre Abdomen e Waist, vamos tirar uma das
# variáveis
data<-select(data,-Waist)
head(data)

# Gerar o modelo de regressão múltipla com as variáveis Age e Weight
regm <- lm(Pct.BF ~ Age + Weight, data=data) 
summary(regm)


# Gerar o modelo de regressão múltipla com todas as variáveis preditoras
regmAll <- lm(Pct.BF~.,data=data)
summary(regmAll)

# Analisando o modelo gerado:

# Quanto ao resíduo
res <- residuals(regm)
hist(residuals(regm))
sd(res)

res <- residuals(regmAll)
hist(residuals(regmAll))
sd(res)

# Quanto a correlação entre o estimado e os dados reais
fit <- fitted(regm)
cor(fit,data$Pct.BF)
plot(fit,data$Pct.BF)

fit <- fitted(regmAll)
cor(fit,data$Pct.BF)
plot(fit,data$Pct.BF)


##########################################################

# Seleção das veriáveis preditoras

# Nem sempre o uso de todas as variáveis é necessária para gerar um modelo
# de regressão. Podemos utilizar o procedimento stepwise para escolher um 
# conjunto de variáveis preditoras que fornecem melhores modelos de predição

# Procedimento stepwise.

model.null <- lm(Pct.BF~1, data=data) # gerando um modelo sem variável
model.full <- regmAll # gerando um modelo completo

# Iniciando o procedimento pelo model.null
model.step<- step(model.null,
     scope = list(upper=model.full),
     direction="both", # pode ser forward, backward, both
     data=data)    

# Iniciando o procedimento pelo model.full
model.step2<- step(model.full,
    scope = list(upper=model.full),
    direction="both", # pode ser forward, backward, both
    data=data)    


##########################################################

# Comparando os modelos

# Existem várias métricas que podem ser utilizadas para comparar os modelos:

# AIC
AIC(model.full)
AIC(model.step)
AIC(model.step2)

# R-adjusted e outros
summary(model.full)
summary(model.step)
summary(model.step2)

# É mais interessante analisar o R-adjusted do que o R-squared, pois no caso
# do R-squared, quanto mais preditores você coloca no modelo, maior fica o seu
# valor. O R-adjusted ajusta o valor de R-squared considerando o número de preditores
# no modelo.

# RMSE (root mean squared error)
mean((residuals(model.full))^2)^0.5
mean((residuals(model.step))^2)^0.5
mean((residuals(model.step2))^2)^0.5

# MAE (mean absolute error)
mean(abs(residuals(model.full)))
mean(abs(residuals(model.step)))
mean(abs(residuals(model.step2)))

# Histograma dos resíduos
hist(residuals(model.full))
hist(residuals(model.step))
hist(residuals(model.step2))

summary(data$Pct.BF)

##########################################################
