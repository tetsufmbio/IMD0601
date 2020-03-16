##### CDR.IBPAD - ggplot.exec #####
# https://cdr.ibpad.com.br/ggplot2.html


## pacotes e bibliotecas --------
install.packages("tidyverse")
library(tidyverse)


# exercicio 1 ------------
# Crie um gráfico de dispersão utilizando a base gapminder para o ano de 2007. Mapeie uma variável para o tamanho do ponto e adicione os títulos do gráfico e dos eixos.

install.packages("gapminder")
library(gapminder)
?gapminder

str(gapminder)
View(gapminder)
summary(gapminder)

gpm.2007 <- gapminder[gapminder$year == 2007, ]
# OU     
gpm.2007 <- gapminder %>%
     filter(year == 2007)

g <- ggplot(gpm.2007, aes(pop, lifeExp)) +
     geom_point(alpha = 0.5) +
     scale_x_log10() +
     labs(title = "Expectativa de vida vs. População - 2007",
          x = "População (milhoes de habitantes)",
          y = "Expectativa de Vida (anos)")
g     


# exercicio 2 ------------
# No gráfico anterior, como você faria para que o ponto do Brasil fosse identificado com uma cor adicional? 
# Dica: se uma nova camada usar um segundo conjunto de dados é preciso informar usando o argumento data. Exemplo: geom_point(data = novo_data_frame, ...).

gpm.2007.br <- gpm.2007[gpm.2007$country == "Brazil", ]

g + geom_point(data = gpm.2007.br, col= "red") +
     annotate("text", x = 190010647, y = 72.39, label = "Brazil", col = "red")


# exercicio 3 ------------
# Crie um histograma da variável wage a partir da base de dados Wage do pacote ISLR. 
# Crie uma visualização da distribuição da variável wage por nível da variável education. Mapeie a variável education para o elemento estético fill.

install.packages("ISLR")
library(ISLR)
?Wage
str(Wage)   
View(Wage)

Wage1 <- Wage

ggplot(Wage1, aes(wage)) +
     geom_histogram()

ggplot(Wage1, aes(education, wage)) +
     geom_bar(aes(fill = education), stat = "identity") +
     labs(title = "Mid-Atlantic Wage Data")


# exercicio 4 ------------
# Crie um data.frame chamado Wage2 em que a variável education é removida. Adicione a seguinte camada no início do código: geom_histogram(data = Wage2, fill = "grey50", alpha = 0.5)
# Veja e interprete o resultado.

Wage2 <- Wage
Wage2 <- Wage %>% 
     select(-education)

ggplot(Wage2, aes(wage)) +
     geom_histogram(data = Wage2, fill = "grey50", alpha = 0.5)


# exercicio 5 ------------
# A partir do código abaixo, crie um gráfico que apresenta os dez países que tiveram maior crescimento do PIB em 2016 e aqueles dez que tiveram o menor. Use o facet_wrap para quebrar a visualização em dois gráficos. Escolha o objeto geométrico geom_col ou geom_point().

install.packages("WDI")
library(WDI)

gdp_growth <- WDI(indicator = "NY.GDP.MKTP.KD.ZG",
                  start = 2016,
                  end = 2016,
                  extra = TRUE)

# Remove regiões - ISO's com números
gdp_growth <- gdp_growth %>% 
     filter(!is.na(region) & region != "Aggregates" & !is.na(NY.GDP.MKTP.KD.ZG))
     
# 10 países com maior e 10 com menor crescimento do PIB em 2016 
gdp_growth_20 <- gdp_growth %>%
     arrange(desc(NY.GDP.MKTP.KD.ZG)) %>%
     slice(-(11:192)) %>%
     mutate(growth = ifelse(NY.GDP.MKTP.KD.ZG > 7.400000, "major", "minor"))

# gdp_growth_plus <- gdp_growth %>%
#      filter(NY.GDP.MKTP.KD.ZG > 7.400000) 
# gdp_growth_minus <- gdp_growth %>% 
#      filter(NY.GDP.MKTP.KD.ZG < -2.7692308)
# gdp_growth_20 <- rbind(gdp_growth_plus, gdp_growth_minus)

# gráfico de dispersão
ggplot(gdp_growth_20, aes(x = NY.GDP.MKTP.KD.ZG, y = reorder(country, -NY.GDP.MKTP.KD.ZG))) +
     geom_point() +
     labs(title = "Gross Domestic Product - 2016",
          x = "GDP growth (anual %)",
          y = "Country") +
     facet_wrap(~ growth)
     
# gráfico de coluna
ggplot(gdp_growth_20, aes(x = reorder(country, -NY.GDP.MKTP.KD.ZG), y = NY.GDP.MKTP.KD.ZG)) +
     geom_col(position = "identity") +
     labs(title = "Gross Domestic Product - 2016",
          x = "Country",
          y = "GDP growth (anual %)") +
     theme(axis.text.x = element_text(angle = 90, hjust = 1))
     facet_wrap(~ growth)

     
###### EXTRAS ######
     
# exercicio 6 ------------
# Utilize o mapa mundi disponível no pacote chropletrMaps e os dados que criamos no exercício anterior para plotar as variações do PIB em um mapa.
library(choroplethrMaps)
data("country.map")

# Dados - Crescimento do PIB em 2016
library(WDI)
library(dplyr)
gdp_growth <- WDI(indicator = "NY.GDP.MKTP.KD.ZG",
                  start = 2016,
                  end = 2016,
                  extra = TRUE)

# Remove regiões - ISO's com números
gdp_growth <- gdp_growth %>% 
     filter(!is.na(region) & region != "Aggregates" & !is.na(NY.GDP.MKTP.KD.ZG))

# Continue fazendo o join entre country.map e gdp_growth
# chaves: wb_a3 e iso3c
country.map <- country.map %>% 
     left_join(gdp_growth, by = c("wb_a3" = "iso3c")) 

ggplot(country.map, aes(x = long, y = lat, group = group)) +
     geom_polygon(aes(fill = NY.GDP.MKTP.KD.ZG)) +
     scale_fill_continuous("Var. % GDP") +
     coord_quickmap()


# exercicio 7 ------------
# Utilizando o código “NY.GDP.PCAP.KD” e o pacote WDI, crie um gráfico do tipo connected dot plot comparando a renda per capita entre o Brasil e mais cinco países nos anos de 1990 e 2010.


###