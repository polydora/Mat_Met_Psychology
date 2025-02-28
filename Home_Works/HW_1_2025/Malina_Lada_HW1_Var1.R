# Задание 1
# параметр mod


# Задание 2
log(((180 / sqrt(2)) - (cos(pi) / (11^(1 / 4)))) / ((10 + exp(1))^5) , 4)


# Задание 3


install.packages("readxl")
library(readxl)
cat <- read_excel("D:/Mat_Met_Biology/data/catsM.xlsx") 
str(cat)
Model <- lm(Hwt ~ Bwt, data = cat)
par(mfrow = c(2, 2))
plot(Model, page = 1)
summary(Model)


# Задание 4

n <- round(runif(1, min = 10, max = 50),0)
vector <- round(rnorm(n, 0, 1), 0)
length(vector)
median(vector)
sd(vector)
c(max(vector), min(vector))




