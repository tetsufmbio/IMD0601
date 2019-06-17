library(dplyr)

# Carregar dados ToothGrowth - contem dados de um estudo
# que avalia o efeito da vitamina C no crescimento do dente
# em porquinhos da índia. O experimento foi realizado em 60
# animais, onde cada um dos animais recebeu uma das 3 doses
# de vitamina C (0,5, 1, e 2 mg/dia) via um dos dois métodos
# (suco de laranja, OJ, e ácido ascórbico, VC). O tamanho dos
# dentes foram medidos. Neste experimento, queremos saber se
# o crescimento do dente depende da dose de vitamina C e do método
# aplicado.

my_data <- ToothGrowth
head(my_data)
str(my_data)

# Converter a coluna dose para fatores

my_data$dose <- factor(my_data$dose, 
                levels = c(0.5, 1, 2),
                labels = c("D0.5", "D1", "D2"))

str(my_data)

# Verificando a frequência de cada célula
table(my_data$supp, my_data$dose)

# Visualizando os dados
if(!require(ggpubr)) install.packages("ggpubr")
library("ggpubr")

ggboxplot(my_data, x = "dose", y = "len", color = "supp",
          palette = c("#00AFBB", "#E7B800"))

ggline(my_data, x = "dose", y = "len", color = "supp",
       add = c("mean_se", "dotplot"),
       palette = c("#00AFBB", "#E7B800"))

# Realizando o ANOVA de dois fatores:
res.aov2 <- aov(len ~ supp + dose, data = my_data)
summary(res.aov2)

res.aov3 <- aov(len ~ supp * dose, data = my_data)
summary(res.aov3)

# Teste de Tukey para encontrar as médias diferentes
# para dose
TukeyHSD(res.aov3, which = "dose")

# para supp
TukeyHSD(res.aov3, which = "supp")

# Verificando se os dados seguem os pressupostos do ANOVA

# Teste de homogeneidade
if(!require(car)) install.packages("car")
library(car)
leveneTest(len ~ supp*dose, data = my_data)

# Teste de normalidade
# QQplot
plot(res.aov3, 2)

# Shappiro-Wilk
# Extrair resíduos
aov_residuals <- residuals(object = res.aov3)
# Executar o teste Shapiro-Wilk
shapiro.test(x = aov_residuals )

# Teste ANOVA com dois fatores para desenhos não balanceados
# Existem três métodos para realizar o teste de ANOVA para 
# desenhos não balanceados: type I, type II e type III.
# Recomenda-se usar o type III.

library(car)
my_anova <- aov(len ~ supp * dose, data = my_data)
Anova(my_anova, type = "III")


# em desenhos balanceados, os três métodos apresentarão os 
# mesmos resultados.

# Exercício
my_data <- read.csv('growth2.csv')
my_anova <- aov(Growth ~ Species * Fertilizer, data = my_data)
summary(my_anova)
