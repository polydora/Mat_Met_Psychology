# Задание 1. Получение справки
# Вопрос: Какой параметр функции Anova() из пакета car определяет объект, который функция получает на входе?
# Ответ: object

# Задание 2. R как калькулятор
log((180/sqrt(2) - cos(pi)/11^(1/3)) / ((10 + exp(1))^5), base = 4)

#Задание 3. Переменные и оформление кода
install.packages("readxl")
library(readxl)
cat <- read_excel("Data/catsM.xlsx")
str (cat)
par(mfrow = c(2, 2))
Model <- lm(Hwt ~ Bwt, data = cat)
plot(Model, page = 1)

#задание 4

n <- round(runif(1, min = 10, max = 50),0)
vector <- round(rnorm(n, 0, 1), 0)

# 1. Количество чисел в векторе
length(vector)

# 2. Медиана
median(vector)

# 3. Среднеквадратичное отклонение
sd(vector)

# 4. Минимальное и максимальное значение (вместе)
range(vector)
