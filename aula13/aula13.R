#
# Teoria dos conjuntos em R
#
x <- c(1,3,5,7,13)
y <- c(3,5,7,11)

# união
union(x, y)

# interseção
intersect(x, y)

# subtração de conjunto
setdiff(x, y)
setdiff(y, x)

# testar a igualdade entre conjuntos
setequal(x, y)

# testar se um elemento pertence a um conjunto
el <- 13
set <- x
is.element(el, set)
(el %in% set)

# outros conjuntos para testar as funções:
A <- c(3, 5, 7, 11)
B <- c(5, 7, 11, 3)
C <- c(3, 4, 6, 5)
D <- c(3, 5, 7, 11, 13)
E <- c(11, 7, 5, 3)
G <- c(3, 5, 5, 7, 7, 11)

setequal(A, B)

library(dplyr)
# conjunto de todas as proteínas de drosophila 
# e suas anotações
data <- read.csv("drosophila.tab", sep="\t", na.strings="")
df <- tbl_df(data)
head(df)

# De quantas proteínas o espaço amostral é formado?


# Pegando uma proteína aleatória deste conjunto, 
# qual a probabilidade de pegar uma que seja transmembrana?

# Pegando uma proteína aleatória deste conjunto, 
# qual a probabilidade de pegar uma que tenha tamanho maior que 1500?

# Qual a probabilidade de eu pegar uma proteína que seja 
# transmembrana ou maior que 1500?

# Qual a probabilidade de eu pegar uma proteína que seja 
# transmembrana e maior que 1500?

# Qual a probabilidade de eu pegar uma proteína que seja 
# transmembrana e que não seja maior que 1500?

# Qual a probabilidade de eu pegar uma proteína que seja 
# maior que 1500 e que não seja transmembrana?

# Qual a probabilidade de eu pegar uma proteína que 
# não seja maior que 1500 ou que não seja transmembrana?

#
# Métodos de contagem em R
#

# Regra da multiplicação
# número de combinações de resultados diferentes
# em experimentos distintos é o produto do espaço 
# amostral dos experimentos.
dado <- c(1,2,3,4,5,6)
moeda <- c("cara","coroa")
length(dado)*length(moeda)

# Permutação com repetição
# Situação onde repito um experimento n vezes
# repondo a amostra sorteada. O número de permutações
# possíveis é o tamanho da amostra elevado a n
rep <- 10
length(moeda)**rep

# permutação simples (sem repetição)
# fatorial:
factorial(5)

# função gamma()
# em R, podemos obter o fatorial de um número utilizando a
# função gamma(), onde gamma(x) = (x - 1)!
n <- 5
gamma(n+1)

# número de permutações de um conjunto de 5 elementos:
factorial(5)
# número de arranjos de um conjunto de 5 elementos, 2 a 2
n <- 5
v <- 2
gamma(n+1)/gamma(n-v+1)
factorial(n)/factorial(n-v)

# número de combinações sem repetição
choose(4,3)

# número de combinações com repetição
# número de soluções possíveis para x1 + x2 + x3 = 7, considerando
# que x1, x2 e x3 sejam números inteiros.
n <- 7
r <- 3
choose(r + n - 1, r)

# Exercícios:
# 1) Quantos miRNAs diferentes de 21 nucleotídeos são
# possíveis de serem encontrados? 


# 2) IPS (induced pluripontent stem cells) é produzido de 
# uma célula adulta não pluripotente através da expressão
# forçada de 4 fatores de transcrição. Antes da descoberta
# o Prof. Yamanaka testou forçar a expressão de diferentes 
# combinações de 24 TFs. Quantas combinações possíveis existem
# para um conjunto de 4 TFs, sabendo que a ordem não importa e 
# que os TFs não se repetem?


# 3) Considerando a questão 2, qual a probabilidade de eu 
# pegar uma combinação que possua o TF Sox2 na minha combinação?


# 4) Considerando a questão 2, qual a probabilidade de eu 
# pegar uma combinação que possua os TFs Sox2 e cMyc na minha
# combinação?


# 5) Considerando a questão 2, qual a probabilidade de eu 
# pegar uma combinação que possua os TFs Sox2 ou cMyc na minha
# combinação?


# 6) Considerando a questão 2, quantas combinações possíveis
# de TFs existem para um conjunto de 4 TFs, sabendo que a ordem não
# importa e que os TFs podem se repetir?


# gerando as combinações
# as diferentes combinações podem ser geradas com o 
# auxílio das funções permutations() e combinations()
# da bibliotece gtools:
install.packages('gtools')
library(gtools)
permutations(n=length(x),r=2,v=x)
permutations(n=length(x),r=2,v=x, repeats.allowed = TRUE)

combinations(n=length(x),r=2,v=x)
combinations(n=length(x),r=2,v=x, repeats.allowed = TRUE)

# Exercício
# 1) Gere todos os 64 códons.

