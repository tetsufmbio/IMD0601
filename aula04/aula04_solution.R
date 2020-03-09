# aula04 - Bioestatística

# Carregue o arquivo aula04.Rdata

# Exercício 1

# a variável dt contém a tabela com os resultados do 
# tratamento a e b em três pacientes;

dt

# Modifique a data frame de forma que ele fique 
# igual a do slide.

trt <- c(rep("a", length(dt)),rep("b", length(dt)))
result <- c(dt$trta,dt$trtb)
patients2 <- c(patients,patients)

dt2 <- data.frame(patients2,trt,result)
dt2

# Carregando as bibliotecas
library(tidyr)
library(dplyr)

# As variáveis students, students2, students3, students4, 
# passed e failed são data frames utilizados nos slides.
# Acompanhe o que estiver sendo feito na aula utilizando
# estas variáveis.

# Exercício

# utilize a variável sat para realizar o exercício

summary(sat)

sat2 <- select(sat, -contains("total"))
sat2
sat3 <- gather(sat2, part_sex, count, -score_range)
sat3
sat4 <- separate(sat3, part_sex, c("part", "sex"))
sat4
sat5 <- group_by(sat4, part, sex)
sat5
sat6 <- mutate(sat5, total = sum(count), prop = count / total)

sat2 <- sat %>%
  select(-contains("total")) %>%
  gather(part_sex, count, -score_range) %>%
  separate(part_sex, c("part", "sex")) %>%
  group_by(part, sex) %>%
  mutate(total = sum(count),
         prop = count / total
  ) 
