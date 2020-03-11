# exemplos do Rbase Graphics

plot(x = cars$speed, y = cars$dist, xlab = "Speed", ylab = "Stopping Distance", col = 2)

boxplot(formula = mpg~cyl, data =mtcars)

hist(mtcars$mpg)
 
# ggplot2

library(tidyr)
library(ggplot2)

iris$Flower <- 1:nrow(iris)
iris.wide <- gather(iris, part_measure, val, -Species, -Flower )
iris.wide <- separate(iris.wide, part_measure, c("part","measure"))
iris.wide <- spread(iris.wide, measure, val)

iris.group <- group_by(iris, Species)
iris.group <- summarise(iris.group, PLM = mean(Petal.Length), PWM = mean(Petal.Width))
ggplot(iris, aes(x = Petal.Length, y = Petal.Width, col = Species)) +
  geom_point() +
  geom_point(data = iris.group, aes(x = PLM, y = PWM), size = 5)
