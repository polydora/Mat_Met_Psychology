# Задание 1 Получение справки
# Параметр distance 


# Задание 2 R как калькулятор, математические операции

log2(((2 * asin(sqrt(1 / 2))) * (180 / pi ) - (11^(1 / 4))) / ((10 + exp(2)) * 50))

# Задание 3 Переменные и оформление кода

install.packages("readxl", dependencies = TRUE)
library(readxl)
cat <- read_excel("C:/Mat_Met_Biology/Data/catsM.xlsx")  
str(cat)
Model <- lm(Hwt ~ Bwt, data = cat)
par(mfrow = c(2, 2))
plot(Model, page = 1)
summary(Model)

# Задание 4 Векторы

n <- round(runif(1, min = 10, max = 50),0)
vector <- round(rnorm(n, 0, 1), 0)
length(vector)
mean(vector)
sd(vector)
c(max(vector), min(vector))

