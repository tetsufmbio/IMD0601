# vetor numérico
c(1,2,3,4)

# vetor lógico
c(TRUE, FALSE, NA)

# vetor de caracteres
c("My", "name", "is")

# ajuda
?c

# operações aritmética
c(1.1, 9, 3.14)*2+100

# reciclagem
c(1,2,3,4) + c(0,10)


# sequencia de números
1:20
15:1
seq(1,20, by=0.5)
seq(5,10,length=30)

# repetições de números
rep(0, times=40)
rep(c(0,1,2), times=10)

# tamanho do vetor
length(rep(0, times=40))

# juntar palavras
paste(c("My", "code"), collapse = " ")



# NA
x <- c(44, NA, 5, NA)
3 * x
is.na(x) # mesmo que x == NA?

# NaN
0 / 0
Inf - Inf

# subconjunto de vetores
y <- rnorm(1000)
z <- rep(NA, 1000)
x <- sample(c(y, z), 100)

x[1:10]
x[c(3,5,7)]
x[c(-3,-5,-7)]
x[-c(3,5,7)]
x[c(-3,5,7)]
x[x > 0]
x[is.na(x)]
x[!is.na(x)]

x[0]
x[3000]

# Nomeando os vetores
vect <- c(140, -50, 20, -120, 240)
days <- c("Monday","Tuesday","Wednesday","Thursday","Friday")
names(vect) <- days

vect[2]
vect["Tuesday"]
vect[c(3,5)]
vect[c("Wednesday","Friday")]

vect <- c(foo = 11, bar = 2, norf = NA)
vect["foo"]


# Exercício
MyData <- read.csv(file="iris.data", sep=",", header=FALSE)
cnames <- c("sepalL", "sepalW", "petalL", "petalW", "class")
colnames(MyData) <- cnames

class(MyData)
dim(MyData)
object.size(MyData)
str(MyData)
summary(MyData)
sum(is.na(MyData))
