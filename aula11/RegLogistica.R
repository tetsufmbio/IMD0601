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
dbt <- read.csv('diabetes_data.csv', header=TRUE, sep=',', na.strings=c('NA', ''))

colnames(dbt)
summary(dbt)
view(dbt)


## 2. Pre-processamento --------

# ajuste de tipo (taming) e codificaçao
age <- dbt[ ,'age']      
gender <- as.factor(dbt[ ,'gender'])   
dm <- as.factor(dbt[ ,'dm'])  

levels(gender)
t <- table(gender)  
addmargins(t)       
round(prop.table(t), digits=2)      
round(100*prop.table(t), digits=1)      
# gender <- relevel(gender, ref='male') 
# contrasts(gender, dm)

# insurance: 0=none, 1=government, 2=private
# fh: family history of diabetes (1=yes, 0=no)
# smoking: 1=current, 2=never, 3=ex

# criaçao de variaveis
height <- dbt[ ,'height']
summary(height)
weight <- dbt[ ,'weight']
summary(weight)

height_si <- height*0.03    # inches para metros
weight_si <- weight*0.45    # pounds para kilos
bmi <- round(weight_si/height_si^2, 2)   # indice massa corporal
summary(bmi)

# transformaçao de variaveis continuas
bmi_cat <- ifelse(bmi<18.5, "underweight", 
                          ifelse(bmi>=18.5 & bmi<=25, "normal", 
                                 ifelse(bmi>25 & bmi<=30, "overweight", 
                                        ifelse(bmi>30, "obese", NA)))) 

# verificaçao  
table(bmi_cat, exclude=NULL) 
head(cbind(bmi_cat, bmi)) 


## 3. Estatistica descritiva ------------------------------

# verificar variavel resposta 
# se nao for binaria, decidir o que fazer: excluir ou combinar valores? Reg ordinal? 
# codificaçao: if HbA1c ≥ 7.5 (unidade?), dm=1
table(dm, exclude=NULL)

# tabulaçao cruzada com diabetes 
dm_bmi_cat <- table(bmi_cat, dm, exclude=NULL) 
dm_bmi_cat 
# margin=1 apresenta % por linha. margin=2 apresenta % por coluna
round(100 * prop.table(dm_bmi_cat, margin=1), digits=1) 


## 4. Regressao Logistica Simples -------------

## modelo nulo diabetes (so um coeficinte)
null <- glm(dm ~ 1, family=binomial(link=logit))
summary(null)
# como R esta codificando dm?
table(null$y)

## dm x age
lg_age <- glm(dm ~ age, family=binomial(link=logit))
summary(lg_age)

lg_age$coefficients
exp(lg_age$coefficients)

# calcular log chance de diabetes pela idade
dm_age <- table(age, dm) # tab.cuzada com diabetes
freq_age <- prop.table(dm_age, margin=1)    # prop diabetes/age
odds_age <- freq_age[, 'yes']/freq_age[, 'no'] # odds de diabetes/age
logodds_age <- log(odds_age)  # log(odds diabetes/age)

# grafico log odds diabetes por idade
plot(rownames(freq_age), logodds_age, xlab='age') 

## dm x gender
lg_gender <- glm(dm ~ gender, family=binomial(link=logit))
summary(lg_gender)

lg_gender$coefficients
exp(lg_gender$coefficients)

# “genderfemale” representa log odds de diabetes de mulheres em comparaçao com homens. 
# a estimativa eh a mesma, com sinal negativo. log(AB) = -log(BA).
# log(odds ratio diabetes homem/mulher) = log(A/B)
# log(odds ratio diabetes mulher/homem) = log(B/A) = -log(A/B) 

gender <- relevel(gender, ref='male') 
lg_female <- glm(dm ~ gender, family=binomial(link=logit))
summary(lg_female)

lg_female$coefficients
exp(lg_female$coefficients)

# calcular log chance de diabetes por gender
gender <- as.factor(dbt[,"gender"]) 
dm_by_gender <- table(gender, dm)
dm_by_gender_prop <- prop.table(dm_by_gender, margin = 1) 
odds_gender <- dm_by_gender_prop[, "yes"]/dm_by_gender_prop[, "no"] 
logodds_gender <- log(odds_gender) 

# grafico log odds diabetes por gender 
dotchart(logodds_gender, main = "Log-odds: Diabetes mellitus por gênero", xlab = "Log-odds DM") # OR
plot(as.factor(names(logodds_gender)), logodds_gender)


## 5. Correlaçao entre preditores  ------------------
# coef. correlação de Pearson entre duas variaveis continuas aprox. normalmente distribuidas 
cor.test(x=dbt$stab.glu, y=dbt$glyhb, method='pearson')
pairs(~age + gender + bmi)


## 6. Regressao Logistica Multipla ------------------------
lg_multi <- glm(dm ~ age + gender + bmi, family=binomial(link=logit))
summary(lg_multi)

exp(lg_multi$coefficients)
exp(confint(lg_multi))


#### Questões ##

# Quantos valores ausentes existem?
# Você acha que os erros padrão são pequenos ou grandes?
# O que significam os coeficientes?
# Os odds ratios para idade e gênero mudaram em comparação com quando você incluiu idade e gênero como preditores únicos nos modelos anteriores de regressão logística simples? Por que não)?
# O significado de cada uma das razões de probabilidade mudou agora que você tem três preditores no mesmo modelo?
