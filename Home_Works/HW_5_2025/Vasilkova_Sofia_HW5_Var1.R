data("iris")
help(package ="datasets") ######проверяю что нужный дасет есть в пакетах
length(iris)
library(dplyr)
library(ggplot2)





#### здесь хочу посмотреть в принципе какие данные есть в таблице ###########
iris


########### Расчет доверительного интервала в R  ###########

ci_setosa <- iris$Sepal.Length[iris$Species == "setosa"]

mean <- mean(ci_setosa)                  # выборочное среднее
n <- length(ci_setosa)                  # объем выборки
SE <- sd(ci_setosa)/ sqrt(n)             # стандартная ошибка
t_crit <- qt(p = 0.975, df = n - 1)    # критич. зн. t для данного n и p = 0.95
err <- t_crit * SE                     # предел для границ доверительного интервала
err

# Границы доверительного интервала
mean - err
mean + err


apply(,4, ci_setosa)   ####### как-то можно через apply вывести сразу 4 столбца, но у меня не получается


ci_versicolor <- iris$Sepal.Length[iris$Species == "versicolor"]

mean <- mean(ci_versicolor)                  # выборочное среднее
n <- length(ci_versicolor)                  # объем выборки
SE <- sd(ci_versicolor)/ sqrt(n)             # стандартная ошибка
t_crit <- qt(p = 0.975, df = n - 1)    # критич. зн. t для данного n и p = 0.95
err <- t_crit * SE                     # предел для границ доверительного интервала
err
# Границы доверительного интервала
mean - err
mean + err




ci_virginica <- iris$Sepal.Length[iris$Species == "virginica"]

mean <- mean(ci_virginica)                  # выборочное среднее
n <- length(ci_virginica)                  # объем выборки
SE <- sd(ci_virginica)/ sqrt(n)             # стандартная ошибка
t_crit <- qt(p = 0.975, df = n - 1)    # критич. зн. t для данного n и p = 0.95
err <- t_crit * SE                     # предел для границ доверительного интервала
err
# Границы доверительного интервала
mean - err
mean + err



library(reshape2)

iris %>%
  melt()

iris %>%
  group_by(Species) %>%
  summarise(sample_n = n(), Mean = mean(Sepal.Length), SD = sd(Sepal.Length), SE = SD/sqrt(sample_n), t_kr = qt(p = c(0.975), df = (sample_n - 1)), CI_low = Mean - t_kr*SE, CI_up = Mean + t_kr*SE)





