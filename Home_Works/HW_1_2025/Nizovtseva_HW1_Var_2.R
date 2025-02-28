# Задание 1. Получение справки
# Параметр distance функции metaMDS() из пакета vegan отвечает за выбор меры индекса различия между сравниваемыми объектами.

# Задание 2. R как калькулятор, математические операции
log2(((2 * asin(sqrt (0.5))) * (180 / pi) - (11^(1 / 4))) / ((10 + exp(2)) * 50))

# Задание 3. Переменные и оформление кода
install.packages("readxl")
library(readxl)
cat <- read_excel("C:/Mat_Met_Biology/data/catsM.xlsx") # Здесь надо указать путь к файлу на вашем компьютере 
str(cat)
Model <- lm(Hwt ~ Bwt, data = cat)
par(mfrow = c(2, 2))
plot(Model, page = 1)
summary(Model)

# Задание 4. Векторы
n <- round(runif(1, min = 10, max = 50),0)
vector <- round(rnorm(n, 0, 1), 0)
length(vector)
mean(vector)
sd(vector)
c(max(vector),min(vector))
