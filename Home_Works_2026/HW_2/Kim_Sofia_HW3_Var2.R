# Напишите код, который будет выводить следущие значения для тех переменных, которые даны ниже (в каждом варианте будет свой набор данных).

#Одновременно минимум,максимум, 1-й, медиану и 3-й квартили
# В каждом датасете есть несколько категорий, для каждой из них надо вывести указанные выше описательные статистики (например, для каждого вида нужно дать отдельные характеристи

# Вариант 2. Вес цыплят weight на 21-й день наблюдения (переменная Time) для каждого из четырех типов диет (Diet) из датасета ChickWeight.

# Загрузка встроенного датасета
data(ChickWeight)

# Фильтровка данных для 21-го дня (переменная Time)

library(dplyr)
data_day21 <- filter(ChickWeight, Time == 21)

# Группировка по диете и вычисление нужных статистик

stats <- data_day21 %>%
  group_by(Diet) %>%
  summarise(
    Min = min(weight, na.rm = TRUE),
    Q1 = quantile(weight, 0.25, na.rm = TRUE),
    Median = median(weight, na.rm = TRUE),
    Q3 = quantile(weight, 0.75, na.rm = TRUE),
    Max = max(weight, na.rm = TRUE)
  )
# Построение графика
library(ggplot2)
ggplot(data_day21, aes(x = factor(Diet), y = weight, fill = factor(Diet))) +
  geom_boxplot() +
  scale_fill_manual(values = c("lightblue", "green", "pink", "orange")) +
  labs(title = "Вес цыплят на 21-й день по типам диет",
       x = "Тип диеты (Diet)",
       y = "Вес (weight)") +
  theme_minimal() +
  theme(legend.position = "none")
