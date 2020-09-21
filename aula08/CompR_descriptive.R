######### Estatísticas Descritivas ###########
# IMD0601 - Bioestatistica
# Tetsu Sakamoto e Beatriz Stransky 
# R Companion Handbook Biological Statistics


## Instalando pacotes
if(!require(c("psych", "DescTools", "Rmisc"))){install.packages(c("psych", "DescTools", "Rmisc"))}

library(psych)      # psych: Procedures for Psychological, Psychometric, and Personality Research
library(DescTools)  # DescTools: Tools for Descriptive Statistics
library(Rmisc)      # Rmisc: Ryan Miscellaneous


## Estatísticas de Tendência Central --------------

# Exemplo: O Maryland Biological Stream Survey usou a pesca elétrica para contar o número de indivíduos de cada espécie de peixe em segmentos de riachos de 75 m de comprimento selecionados aleatoriamente em Maryland. 
# Aqui estão os números da Dace Blacknose, Rhinichthys atratulus, nos riachos da bacia hidrográfica de Rock Creek.
Input =("
        Stream                      Fish
        Mill_Creek_1                76
        Mill_Creek_2               102
        North_Branch_Rock_Creek_1   12
        North_Branch_Rock_Creek_2   39
        Rock_Creek_1                55
        Rock_Creek_2                93
        Rock_Creek_3                98
        Rock_Creek_4                53
        Turkey_Branch              102
        ")
Data = read.table(textConnection(Input),header=TRUE)

# Média Aritimética
# Use na.rm=TRUE para não retornar erro. Algumas funções excluem NA por padrão.
mean(Data$Fish, na.rm=TRUE)

# Mediana 
median(Data$Fish, na.rm=TRUE)

# Moda
Mode(Data$Fish)

# Resumos de estatisticas descritivas e gráficos
# funcionam com todo o dataframe ou com variáveis individuais
summary(Data$Fish)            # quartis
describe(Data$Fish, type=2)   # outras estatisticas

# Histograma
hist(Data$Fish,   
     col="gray", 
     main="Maryland Biological Stream Survey",
     xlab="Fish count")   

# Adicione uma variável numérica com os mesmos valores de Fish
Data$Fish.num = as.numeric(Data$Fish)

# Descfunction produz informações resumidas para cada tipo de variável e gráficos
Desc(Data, plotit=TRUE)


## DescTools com dados agrupados
Input =("
        Stream                     Animal  Count
        Mill_Creek_1               Fish     76
        Mill_Creek_2               Fish    102
        North_Branch_Rock_Creek_1  Fish     12
        North_Branch_Rock_Creek_2  Fish     39
        Rock_Creek_1               Fish     55
        Rock_Creek_2               Fish     93
        Rock_Creek_3               Fish     98
        Rock_Creek_4               Fish     53
        Turkey_Branch              Fish    102
        
        Mill_Creek_1               Insect   28
        Mill_Creek_2               Insect   85
        North_Branch_Rock_Creek_1  Insect   17
        North_Branch_Rock_Creek_2  Insect   20
        Rock_Creek_1               Insect   33
        Rock_Creek_2               Insect   75
        Rock_Creek_3               Insect   78
        Rock_Creek_4               Insect   25
        Turkey_Branch              Insect   87
        ")

D2 = read.table(textConnection(Input),header=TRUE)

Desc(Count ~ Animal, D2, digits=1, plotit=TRUE)    


## Estatísticas de Dispersão ------------

# Intervalo
range(Data$Fish, na.rm=TRUE)   
max(Data$Fish, na.rm=TRUE) - min(Data$Fish, na.rm=TRUE)

# Variância (amostra)
var(Data$Fish, na.rm=TRUE)

# Desvio Padrão (amostra)
sd(Data$Fish, na.rm=TRUE)
round(sd(Data$Fish, na.rm=TRUE), 2)

# Coeficiente de variaçao, como porcentagem
sd(Data$Fish, na.rm=TRUE)/mean(Data$Fish, na.rm=TRUE)*100

# Função personalizada de medidas de tendência central e dispersão
summary.list = function(x)list(
     N.with.NA.removed= length(x[!is.na(x)]),
     Count.of.NA= length(x[is.na(x)]),
     Mean=mean(x, na.rm=TRUE),
     Median=median(x, na.rm=TRUE),
     Max.Min=range(x, na.rm=TRUE),
     Range=max(Data$Fish, na.rm=TRUE) - min(Data$Fish, na.rm=TRUE),
     Variance=var(x, na.rm=TRUE),
     Std.Dev=sd(x, na.rm=TRUE),
     Coeff.Variation.Prcnt=sd(x, na.rm=TRUE)/mean(x, na.rm=TRUE)*100,
     Std.Error=sd(x, na.rm=TRUE)/sqrt(length(x[!is.na(x)])),
     Quantile=quantile(x, na.rm=TRUE)
)

summary.list(Data$Fish)


## Reference
# Mangiafico, S.S. 2015. An R Companion for the Handbook of Biological Statistics, version 1.3.2.