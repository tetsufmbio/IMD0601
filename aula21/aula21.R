########################################

# Modelando sequência de DNA em cadeia de Markov

DNA<-matrix(c(0.3,0.1,0.2,0.1,0.2,0.2,0.2,0.8,0.2,0.4,0.2,0.1,0.3,0.3,0.4,0),nrow=4,ncol=4)
DNA
DNA2<-DNA%*%DNA
DNA4<-DNA2%*%DNA2
DNA4

DNA8<-DNA4%*%DNA4
DNA8

DNA16<-DNA8%*%DNA8
DNA16

DNA32<-DNA16%*%DNA16
DNA64<-DNA32%*%DNA32
DNA64

########################################

# cadeias periódicas
periodic<-matrix(c(0,1,0,0,0,1,1,0,0),nrow=3,ncol=3)
periodic

periodic1<-periodic%*%periodic
periodic1

periodic2<-periodic%*%periodic1
periodic2

periodic3<-periodic2%*%periodic
periodic3

# cadeias redutíveis
reductible<-matrix(c(0.4,0.3,0,0,0.6,0.7,0,0,0,0,0.4,0.3,0,0,0.6,0.7),nrow=4,ncol=4)
reductible

reductiblen <- reductible
reductiblen<-reductiblen%*%reductible
reductiblen

# cadeias reversíveis
reversible<-matrix(c(0.1,0.9,0.9,0.1),nrow=2,ncol=2)
reversible

reversiblen <- reversible
reversiblen <- reversiblen%*%reversible
reversiblen

pi <- reversiblen[1,]
pi

reverse<-reversiblen
reverse<-reverse%*%solve(reversible)
reverse


########################################

# Ilhas CpG

# matriz de transição obtida a partir de sequências de ilhas CpG:
cpg <- matrix(c(0.18,0.17,0.16,0.08,0.27,0.37,0.34,0.36,0.43,0.27,0.38,0.38,0.12,0.19,0.12,0.18),nrow=4,ncol=4)
cpg

# Rotulando as linhas e as colunas da matriz
nucl <- c("A","C","G","T")
rownames(cpg) <- nucl
colnames(cpg) <- nucl
cpg

# Probabilidade de A para T:
cpg["A","T"]

# Sequência a ser verificada no modelo
seq <- "CGTTCGACGTA"
seq <- strsplit(seq,"")[[1]]
seq

# Calculando a probabilidade dela ser de uma ilha CpG
pcpg = 1;
for (i in 2:length(seq)){
  pcpg = pcpg*cpg[seq[i-1],seq[i]]
}
pcpg

# Exercício
# Calcule a probabilidade da mesma sequência não ser de uma ilha CpG, segue abaixo
# a matriz de transição obtida 

# matriz de transição obtida a partir de sequências que não são ilhas CpG:
ncpg <- matrix(c(0.3,0.32,0.25,0.18,0.21,0.30,0.24,0.24,0.28,0.08,0.3,0.29,0.21,0.3,0.21,0.29), nrow=4,ncol=4)
ncpg


# Exercício:
# Calcule o log odds ratio entre a probabilidade da sequência ser da ilha CpG
# e de não ser da ilha CpG.


########################################

# Gerador de sequências aleatórias

# Mesma frequência para todas:
nucl <- c("A","C","G","T")
n <- 1000
DNAseq <- sample(nucl, n, replace=TRUE)
DNAseq

library(ggplot2)
data <- tbl_df(DNAseq)
ggplot(data, aes(x=value)) + geom_histogram(stat="count")

# Frequências diferentes
freq <- c(0.2,0.5,0.1,0.2)
DNAseq <- sample(nucl, n, replace=TRUE, p=freq)
DNAseq
data <- tbl_df(DNAseq)
ggplot(data, aes(x=value)) + geom_histogram(stat="count")

# Utilizando a matriz de transição do modelo +
DNAseq <- vector(length=n)
DNAseq[1] <- sample(nucl, 1, replace=TRUE)
for (i in 2:length(DNAseq)){
    DNAseq[i] <- sample(nucl, 1, replace=TRUE, p=cpg[DNAseq[i-1],])
  
}
DNAseq
data <- tbl_df(DNAseq)
ggplot(data, aes(x=value)) + geom_histogram(stat="count")

# Utilizando a matriz de transição do modelo -
DNAseq <- vector(length=n)
DNAseq[1] <- sample(nucl, 1, replace=TRUE)
for (i in 2:length(DNAseq)){
  DNAseq[i] <- sample(nucl, 1, replace=TRUE, p=ncpg[DNAseq[i-1],])
  
}
DNAseq
data <- tbl_df(DNAseq)
ggplot(data, aes(x=value)) + geom_histogram(stat="count")

# Exercício
# Crie uma função que gera uma sequência de DNA aleatória de tamanho
# n seguindo a matriz de transição que o usuário fornecer.



# Exercício
# Crie uma função que calcula a probabilidade de uma sequência de DNA
# dado a matriz de transição fornecida pelo usuário.



# Exercício
# Gere 100 sequências aleatórias de tamanho 30 utilizando o modelo + 
# e calcule o log odds ratio destas sequências entre os modelos + e -.
# Armazene os resultados no vetor dataPlus.
n <- 100
dataPlus <- vector(length=n)



# Exercício
# Faça o mesmo do exercício anterior, mas desta vez gerando as sequências 
# aleatórias utilizando o modelo -. Armazene os resultados no vetor dataMinus.
n <- 100
dataMinus <- vector(length=n)


# Gerando gráficos:
library(dplyr)
data <- data.frame(data = as.numeric(dataPlus), model = "+")
data2 <- data.frame(data = as.numeric(dataMinus), model = "-")

dataAll <- bind_rows(data, data2)
ggplot(dataAll,aes(x=data, fill=model)) + geom_histogram()
