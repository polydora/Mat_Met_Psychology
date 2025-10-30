#загружаем нужные библиотеки и датасет
library(ggplot2)
library(dplyr)
data(iris)

#смотрим датасет разными способами
str(iris)
iris

######Часть 1.######
#Поместим в переменную setosa данные о длинне лепестка соответствующего вида
setosa <- iris %>%
  filter (Species == "setosa") %>%
  pull (Petal.Length)
#посчитаем характеристики получившегося вектора
range(setosa) #показывает одновременно минимальное и максимальное значение
quantile(x = setosa, probs = c(0.25)) #показывает 1-ый квартиль
quantile(x = setosa, probs = c(0.75)) #показывает 3-ий квартиль
median(setosa)                        #показывает медиану, она же 2-ой квартиль
quantile(x = setosa, probs = c(0.25, 0.5, 0.75)) #показывает все три квартиля одновременно

#Аналогично сделаем для данных по двум другим видам: versicolor и virginica

versicolor <- iris %>%
  filter (Species == "versicolor") %>%
  pull (Petal.Length)
range(versicolor)
quantile(x = versicolor, probs = c(0.25))
quantile(x = versicolor, probs = c(0.75))
median(versicolor)
quantile(x = versicolor, probs = c(0.25, 0.5, 0.75))

virginica <- iris %>%
  filter (Species == "virginica") %>%
  pull (Petal.Length)
range(virginica)
quantile(x = virginica, probs = c(0.25))
quantile(x = virginica, probs = c(0.75))
median(virginica)
quantile(x = virginica, probs = c(0.25, 0.5, 0.75))

######Часть 2######
#Построим на одном поле "ящики с усами" для каждого из видов
ggplot(data = iris) +
  geom_boxplot(aes(x = Species, y = Petal.Length))


######Часть 3######
#Считаем файл и выведем первые 5 строк для ознакомления
dataset <- read.csv(file = 'dataset_variant_1.csv', sep = ";")
dataset [1:5, ]
#Видим данные по 5-ти переменным
#Загружаем библиотеку car
library(car)
#Чтобы определить какие из переменных подчиняются нормальному распределению, 
#построим на одном поле гистограмму и квантильный график по переменной Var1
par(mfrow = c(1, 2))                       #графический параметр, формирующий сетку графиков
hist(dataset$Var1, nclass = 30, main = '') #гистограмма
qqPlot(dataset$Var1, id = FALSE)           #квантильный график
par(mfrow = c(1, 1))
#если данные подчиняются нормальному распределению,
#то точки на квантильном графике должны лечь близко к прямой (остаться внутри выделенной области)
#в случае с переменной Var1 такого не наблюдается (хвосты выходят за допустимые границы),
#поэтому данные по этой переменной не подчиняются нормальному распределению

#Для остальных переменных проделаем аналогичную аналитику

par(mfrow = c(1, 2))
hist(dataset$Var2, nclass = 30, main = '')
qqPlot(dataset$Var2, id = FALSE)
par(mfrow = c(1, 1))
#Не является нормальным распределением, наблюдается выраженное бимодальное распределение 

par(mfrow = c(1, 2))
hist(dataset$Var3, nclass = 30, main = '')
qqPlot(dataset$Var3, id = FALSE)
par(mfrow = c(1, 1))
#Не является нормальным распределением, непрерывное распределение с правым хвостом

par(mfrow = c(1, 2))
hist(dataset$Var4, nclass = 30, main = '')
qqPlot(dataset$Var4, id = FALSE)
par(mfrow = c(1, 1))
#Наиболее близко к нормальному распределению из рассмотренных переменных, слева наблюдаютя несколько выбросов

par(mfrow = c(1, 2))
hist(dataset$Var5, nclass = 30, main = '')
qqPlot(dataset$Var5, id = FALSE)
par(mfrow = c(1, 1))
#Не является нормальным распределением, пример Т-образного распределения


######Часть 4######
#Для расчета средней величины выборки используется функция mean
#Для расчета среднеквадратичного отклонения испоьлзуется функция sd
#Для векторов, сформированных в Части 2 рассчитаем эти параметры
mean(setosa)
sd(setosa)

mean(versicolor)
sd(versicolor)

mean(virginica)
sd(virginica)


######Часть 5######
#Для векторов из Части 2 найдем доверительные интервалы по формуле x=±t⋅SE и выведем полученные значения
s <- qt(0.975, df = length(setosa) - 1) * (sd(setosa)/sqrt(length(setosa)))
s

ve <- qt(0.975, df = length(versicolor) - 1) * (sd(versicolor)/sqrt(length(versicolor)))
ve

vi <- qt(0.975, df = length(virginica) - 1) * (sd(virginica)/sqrt(length(virginica)))
vi
