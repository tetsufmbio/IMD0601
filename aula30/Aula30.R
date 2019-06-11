###########  Aula 1 - ANOVA ################

#Carregando a tabela para o ambiente R:

tab <- read.csv("growth1.csv")

head(tab)

summary(tab)

attach(tab)

boxplot(Growth~Treatment)

#Teste ANOVA:

t_anova <- aov(Growth~Treatment)

#Visualizando resultado o ANOVA

summary(t_anova)

#############################################

# T-test par a par
pairwise.t.test(Growth,Treatment)

# Teste de Tukey

TukeyHSD(t_anova)

#############################################
# Teste de normalidade dos dados

# Podemos usar o teste de Shapiro-Wilk para testar a normalidade
# dos dados de cada grupo. No entanto, esta abordagem não é adequada
# para casos em que o número de amostras é pequeno. Por isso, verificamos
# se os resíduos formam uma distribuição normal. Se este segue uma
# distribuição normal, isso já é um indicador que os grupos também
# seguem uma distribuição normal.

plot(t_anova, 2)

# Extrair os resíduos
aov_residuals <- residuals(object = t_anova )
# Realizar o teste Shapiro-Wilk
shapiro.test(x = aov_residuals )

#############################################

# Teste de homogeneidade das variâncias

# Barlett test - Para dados que seguem uma distribuição normal
bartlett.test(Growth ~ Treatment)

# Levene test - Teste Alternativo do Barlett test, mas para 
# dados que não seguem uma normal
library(car)
leveneTest(Growth ~ Treatment)

# Fligner-Killeen test - Outra alternativa para testar a homogeneidade 
# das variâncias.
fligner.test(Growth ~ Treatment)

#############################################

# Teste de Kruskal-Wallis

kruskal.test(Growth~factor(Treatment))

#############################################

# Teste de comparação múltipla de Nemenyi

if (!require("PMCMR")) install.packages("PMCMR") 
library(PMCMR)
posthoc.kruskal.nemenyi.test(Growth ~ factor(Treatment))

#############################################

# Exercício

peso <- c(128.5, 162.8, 111.4, 128.5, 205.7, 128.5, 128.5, 102.8, 102.8, 179.9, 248.5, 137.1, 162.8, 137.1, 68.6, 98.5, 77.1, 137.1, 85.7, 102.8, 111.4, 77.1, 60, 68.6)
cultivo <- c(rep("A",6),rep("B",6),rep("C",6),rep("D",6))

boxplot(peso~factor(cultivo))
