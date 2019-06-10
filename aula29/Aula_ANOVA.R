###########  Aula 1 - ANOVA ################

#Carregando a tabela para o ambiente R:

tab <- read.table("medicamentos.prn", header = T)

#Teste ANOVA:

t_anova <- aov(Horas~Medicamento, tab)

#Visualizando resultado o ANOVA

summary(t_anova)
