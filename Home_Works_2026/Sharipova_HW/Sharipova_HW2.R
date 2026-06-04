# ====================Домашнее задание №2.====================


# ### ДЗ часть 1

#Загружаю датасет iris
data(iris)

#Выбираю первый вариант
iris_filtered <- iris[iris$Species == "setosa", ]

#Считаю частоту значений Sepal.Length
freq_table <- as.data.frame(table(iris_filtered$Sepal.Length))

#Меняю название колонок
colnames(freq_table) <- c("Sepal.Length", "Frequency")

#Смотрю результат
print(freq_table)


#Решила по той же схеме попробовать сделать вариант 2 и 3. Чисто из интереса!


# Вариант 2
iris_filtered <- iris[iris$Species == "versicolor", ]
freq_table <- as.data.frame(table(iris_filtered$Sepal.Length))
colnames(freq_table) <- c("Sepal.Length", "Frequency")
print(freq_table)

# Вариант 3
iris_filtered <- iris[iris$Species == "virginica", ]
freq_table <- as.data.frame(table(iris_filtered$Sepal.Length))
colnames(freq_table) <- c("Sepal.Length", "Frequency")
print(freq_table)


#ГОТОВО! Не без помощи ИИ, но иначея бы не смогла узнать по какому щаблону писать строки.



# ### ДЗ часть 2

data(iris)
iris_filtered <- iris[iris$Species == "setosa", ]
freq_table <- as.data.frame(table(iris_filtered$Sepal.Length))
colnames(freq_table) <- c("Sepal.Length", "Frequency")

#График
barplot(
  height = freq_table$Frequency,
  names.arg = freq_table$Sepal.Length,
  col = "lightgreen",
  border = "black",
  xlab = "значение Sepal.Length",
  ylab = "количество",
  main = "Распределение Sepal.Length (setosa)",
  ylim = c(0, 8),
  axes = FALSE
)
axis(2, at = seq(0, 8, by = 2))



# ====================Доп.задание 1====================
library(ggplot2)

#Здесь ввожу данные
x <- seq(-10, 10, by = 0.1)
y <- 2 * x^2 + 4 * x + 50
df <- data.frame(x = x, y = y)

#ИИ подсказал как разделить линию по цвету
df$group <- ifelse(x <= 0, "x ≤ 0", "x > 0")

#Теперь можно строить график, это я умею!
ggplot(df, aes(x = x, y = y, color = group, linetype = group)) +
  geom_line(size = 2) +
  scale_color_manual(values = c("x ≤ 0" = "blue", "x > 0" = "red")) +
  scale_linetype_manual(values = c("x ≤ 0" = "solid", "x > 0" = "dashed")) +
  labs(
    title = expression(y == 2 * x^2 + 4 * x + 50),
    x = "x",
    y = "y"
  ) +
  
  
  
  
# ====================Доп.задание 2.====================
#Здесь я честно говоря почти во всем доверилась ИИ, довольно сложное задание по сравнению с предыдущими
  
  library(ggplot2)

#Симуляция данных
set.seed(12345)
df <- data.frame(X = rnorm(1000, 1, 5), Y = rnorm(1000, 2, 5))

#График с точками поверх облака плотности
ggplot(df, aes(x = X, y = Y)) +
  stat_density_2d(
    aes(fill = after_stat(level)),
    geom = "polygon",
    alpha = 0.6
  ) +
  scale_fill_gradientn(
    colors = c("yellow", "red"),
    breaks = c(0.001, 0.002, 0.003, 0.004, 0.005, 0.006),
    labels = c("0.001", "0.002", "0.003", "0.004", "0.005", "0.006"),
    name = "Level"
  ) +
  geom_point(color = "black", alpha = 0.5, size = 1) +  #точки теперь ПОСЛЕ слоёв плотности
  labs(
    title = "Двумерное нормальное распределение",
    x = "X",
    y = "Y"
  ) +
  theme_minimal() +
  theme(
    legend.position = "left",
    legend.key.height = unit(1.5, "cm")
  )
