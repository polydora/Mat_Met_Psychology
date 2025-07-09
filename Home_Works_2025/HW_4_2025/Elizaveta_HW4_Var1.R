# Домашнее задание №4. Модель нормального распределения, как основа для тестирования статистических гипотез
# Демиденко ЕР Вариант 1. 

##### Первая часть 

library(car)
data <- read.csv("D:/Mat_Met_Biology/data/dataset_variant_1.csv", sep = ";", header = TRUE)
str(data)

# Строим квантильный график и гистограмму для каждой переменной 

qqPlot(data$Var1, id = FALSE)
hist(data$Var1)
## Var 1, отклонения в хвостах, более выраженное в правом, непрерывное распределение с толстыми хвостами

qqPlot(data$Var2, id = FALSE)
hist(data$Var2)
## Var 2, видно бимодальное распределение, два пика на гистограмме 

qqPlot(data$Var3, id = FALSE)
hist(data$Var3)
## Var 3, отклонения в хвостах, в правом более резкое и длинное по оси y, непрерывное распределение с длинным правым хвостом?

qqPlot(data$Var4, id = FALSE)
hist(data$Var4)
## Var 4, небольшие отклонения в хвостах, более выраженные и резкие в левом хвосте,но относительно симметричные,как и гистограмма, распредление похоже на нормальное

qqPlot(data$Var5, id = FALSE)
hist(data$Var5)
## Var 5, отклонения в хвостах, на гистограмме есть пропуск столбца, непрерывное распределение с толстыми хвостами


##### Вторая часть

data(iris)
setosa <- iris$Petal.Length[iris$Species == "setosa"]
versicolor <- iris$Petal.Length[iris$Species == "versicolor"]
virginica <- iris$Petal.Length[iris$Species == "virginica"]

## Среднее значение 
setosa1 <- mean(setosa)
versicolor1 <- mean(versicolor)
virginica1 <- mean(virginica)
## Девиата
raw_deviates_setosa <- setosa - setosa1
raw_deviates_versicolor <- versicolor - versicolor1
raw_deviates_virginica <- virginica - virginica1

## Среднеквадратичное/стандартное отклонение = Standard Deviation
sd_setosa <- sqrt(sum(raw_deviates_setosa^2) / (length(setosa) - 1))
sd_versicolor <- sqrt(sum(raw_deviates_versicolor^2) / (length(versicolor) - 1))
sd_virginica <- sqrt(sum(raw_deviates_virginica^2) / (length(virginica) - 1))

sd_setosa
sd_versicolor
sd_virginica

