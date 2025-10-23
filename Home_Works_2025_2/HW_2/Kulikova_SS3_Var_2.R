#install.packages("dplyr") 
#install.packages("ggplot2")
#install.packages("readxl")
#install.packages("reshape2")


###  Задание 1

# Загружаем данные iris
data(iris)

# Фильтруем данные для вида 'versicolor'
versicolor_data <-
  iris[iris$Species == "versicolor", ]

# Создаем таблицу частот для переменной Sepal.Length
freq_table <- 
  table(versicolor_data$Sepal.Length)

# Преобразуем таблицу в датафрейм
result_df <- 
  data.frame(Sepal.Length = as.numeric(names(freq_table)), # Уникальные значения Sepal.Length
  Frequency = as.vector(freq_table)             # Частоты
)

# Сохраняем результат в CSV-файл
write.csv(result_df, "versicolor_sepal_length_freq.csv", row.names = FALSE)

# Выводим таблицу в консоль для проверки


print(result_df, row.names = FALSE)



### Задание 2


# Загружаем библиотеку ggplot2
library(ggplot2)

# Строим столбчатую диаграмму
ggplot(result_df, aes(x = factor(Sepal.Length), y = Frequency)) +
  geom_bar(
    stat = "identity",           # Используем точные значения из Frequency
    fill = "green",          # Цвет заливки как в примере
    color = "black"              # Черные границы столбцов
  ) +
  labs(
    title = "Частота значений Sepal.Length (вид: versicolor)",
    x = "Значение Sepal.Length",
    y = "Количество"
  ) +
  theme_minimal() +              # Минималистичная тема
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) # Наклон подписей оси X

### Задание 3

library(ggplot2)
library(dplyr)

# Создаем и разделяем данные
x <- seq(-10, 10, length.out = 400)
y <- 2*x^2 + 4*x + 50
df <- data.frame(x = x, y = y)

df_negative <- df %>% filter(x < 0)
df_positive <- df %>% filter(x >= 0)

# Минималистичный график
ggplot() +
  geom_line(data = df_negative, aes(x = x, y = y), 
            color = "blue", linewidth = 1.2, linetype = "solid") +
  geom_line(data = df_positive, aes(x = x, y = y), 
            color = "red", linewidth = 1.2, linetype = "dashed") +
  labs(
    title = expression(paste("y = 2x"^2, " + 4x + 50")),
    x = "x",
    y = "y"
  ) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5, size = 14))

### Задание 4

# Установка начального значения для воспроизводимости
set.seed(12345)

# Генерация выборки из двумерного нормального распределения
df <- data.frame(
  X = rnorm(1000, mean = 1, sd = 5),
  Y = rnorm(1000, mean = 2, sd = 5)
)

# Визуализация с использованием ggplot2
library(ggplot2)

ggplot(df, aes(x = X, y = Y)) +
  # Залитые контурные уровни плотности
  stat_density_2d(
    aes(fill = after_stat(level)), 
    geom = "polygon",
    alpha = 1
  ) +
  # Градиент заливки для уровней плотности
  scale_fill_gradient(
    low = "yellow",
    high = "red",
    name = "level"
  ) +
  # Точки данных (добавляем ПОД контурами, чтобы они были лучше видны)
  geom_point(color = "black", alpha = 0.2, size = 0.5) +
  # Оформление
  theme_minimal() +
  labs(
    title = "Двумерное нормальное распределение",
    x = "X",
    y = "Y"
  ) +
  theme(
    legend.position = c(0.001, 0.95),  # Легенда внутри графика (x, y в долях от
    # 0 до 1)
    legend.justification = c(0, 1),   # Выравнивание легенды (левый верхний угол)
    legend.title = element_text(size = 10),
    legend.text = element_text(size = 8),
    legend.key.size = unit(0.4, "cm")  # Размер элементов легенды
  )