library(tidyr)
library(dplyr)
library(ggplot2)
library(lubridate)
library(stringr)

# exercício 1
data <- read.table("data.tab", sep = "\t", header = TRUE, comment.char="", quote="", na.strings="")
df <- tbl_df(data)

# exercício 2
summary(df$Sequencing.Center)

dfScenter <- group_by(df,Sequencing.Center)
countScenter <- summarise(dfScenter, count=length(Sequencing.Center))
smallCount <- countScenter[order(-countScenter$count),]
bp <- ggplot(smallCount[1:10,], aes(x="", y=count, fill=Sequencing.Center)) + geom_bar(width=1, stat="identity")
bp
pie <- bp + coord_polar("y", start=0)
pie

# exercício 3
ks.test(df$Genome.Size.....assembled,"pnorm",mean=mean(df$Genome.Size.....assembled), sd=sd(df$Genome.Size.....assembled))
shapiro.test(df$Genome.Size.....assembled)
qqnorm(df$Genome.Size.....assembled)
qqline(df$Genome.Size.....assembled, col="steelblue",lwd=2)

# https://ggplot2.tidyverse.org/reference/geom_qq.html

# exercicio 4
dfGenus <- group_by(df,Genus)
countGenus <- summarise(dfGenus, count=length(Genus))
countGenus <- countGenus[order(-countGenus$count),]

bp <- ggplot(countGenus, aes(x=reorder(Genus,-count), y=count)) + geom_bar(width=1, stat="identity") + theme(axis.text.x = element_blank())
bp

# exercício 5
df_gen <- group_by(df, Genus)
df_gen_mean <-summarise(df_gen, mean=mean(Genome.Size.....assembled))
shapiro.test(df_gen_mean$mean)
ks.test(df_gen_mean$mean, "pnorm", mean=mean(df_gen_mean$mean), sd=sd(df_gen_mean$mean))
qqnorm(df_gen_mean$mean)
qqline(df_gen_mean$mean, col="steelblue",lwd=2)

# exercício 6
df$prop <- df$w.o.function.prediction....assembled/(df$w..Func.Pred.Count.....assembled + df$w.o.function.prediction....assembled)
df_sel <- select(df, Genome.Name...Sample.Name, Genus, prop)
arrange(df_sel, prop)

# exercicio 7
# selecionar colunas de interesse.
df2 <- select(df, Sequencing.Method, Release.Date, IMG.Genome.ID)

# selecionar linhas não NA
df2 <- filter(df2, !is.na(df2$Sequencing.Method), !is.na(df2$Release.Date))

# parser da data
df2$Release.Date <- mdy(df2$Release.Date)

# transformar os dados de method em colunas
df3 <- separate(df2,Sequencing.Method,into=c("method1","method2","method3","method4","method5"),sep=",")

# juntar as colunas métodos e colocá-los em uma coluna
df4 <- gather(df3, method_col, method, -Release.Date, -IMG.Genome.ID)

# substituir os dados da coluna métodos apenas para method
df4$method_col <- str_replace(df4$method_col, "method\\d","method")

# eliminar espaços a mais na coluna method
df4$method <- str_trim(df4$method)

# Retirar os modelos dos sequenciadores
df4$method <- str_replace(df4$method, "454.+","454")
df4$method <- str_replace(df4$method, "Illumina.+","Illumina")
df4$method <- str_replace(df4$method, "ABI.+","ABI")
df4$method <- str_replace(df4$method, "PacBio.+","PacBio")
df4$method <- str_replace(df4$method, "Hybrid.+","Illumina")

# transformar a coluna method em factor
df4$method <- as.factor(df4$method)

# retirar dados de genoma que foi sequenciado com vários sequenciadores, mas da mesma marca
df4 <- distinct(df4)

# adicionar uma coluna de índice da linha
df4$unique <-seq(1:nrow(df4))

# filtrar os dados faltantes
df5 <- filter(df4, !is.na(df4$method))

# criar uma coluna year
df5$year <- year(df5$Release.Date)

# agrupar por year e method
df5 <- group_by(df5, year, method)

# contar as ocorrências por year e method
df6 <- count(df5)

# adicionar contagens 0
df7 <- spread(df6,year,n)
df7 <- gather(df7,year,count,-method)
df7$count[is.na(df7$count)] <- 0
df7$year <- as.integer(df7$year)

# fazer os plots
ggplot(df7, aes(year, count, col=method)) + geom_line()
ggplot(df7, aes(year, count, fill=method)) + geom_area()




