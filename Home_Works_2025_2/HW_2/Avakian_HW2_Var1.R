# Добашнее задание 2 - Данные и их визуализация с использованием пакета ggplot2
# Авакян Кристина
# Группа: ББ5А23/10

# Вариант 1. Работаем с видом “setosa”

# ==================================================================
# Задание 1. Датасет iris 
# ==================================================================

# Открываем датасет iris
data(iris)

# Фильтруем данные по виду
setosa_iris <- subset(iris, Species == "setosa")

# Посчитаем сколько раз встречается значение длины
frequency_sepal_length <- table(setosa_iris$Sepal.Length)
frequency_sepal_length

# Создаем датафрейм
df <- data.frame(Sepal.Length = as.numeric(names(frequency_sepal_length)),
                 Frequency = as.integer(frequency_sepal_length))
df

write.csv(df, file = "Output/Avakian_Setosa_Iris.csv", row.names = TRUE)

# ==================================================================
# Задание 2. Изображение частотного распределения
# ==================================================================

# Подключаем пакеты
library(dplyr)
library(ggplot2)

# Устанавливаем тему
theme_set(theme_bw())

# Создаем график
df %>%
  ggplot(mapping = aes(x = factor(Sepal.Length), y = Frequency)) +
    geom_col(color = "black", fill = "green") +
    labs(x = "Значение Sepal.Length", y = "Количество") +
    ggtitle(label = "Анализ датасета Iris (Авакян)", subtitle = "Вид Setosa") -> plot_iris_histogram

# Показываем график
plot_iris_histogram

# Сохраняем график
# ggsave("Images/Avakian_Iris_Setosa.jpg", plot = plot_iris_histogram)

# ==================================================================
# Задание повышенной сложности 1: График функции
# ==================================================================

# Подключаем пакеты
library(dplyr)
library(ggplot2)

# Устанавливаем тему
theme_set(theme_bw())

# Создаем датафрейм с точками по которым будем рисовать данные
df <- data.frame(x = seq(-10, 10, by = 0.1))
df$y <- 2*df$x^2 + 4*df$x + 50

# Делим на две части (правую и левую) относительно 0
df_left  <- subset(df, x <= 0)
df_right <- subset(df, x >= 0)

# Рисуем на пустом графике две линии
ggplot() +
  geom_line(data = df_left, aes(x = x, y = y), color = "blue", linewidth = 2, linetype = "solid") +
  geom_line(data = df_right, aes(x = x, y = y), color = "red", linewidth = 2, linetype = "dashed") +
  labs(x = "x", y = "y", title = "y = 2*x^2 + 4*x + 50") +
  coord_cartesian(xlim = c(-10, 10), ylim = c(0, 300)) -> plot_hard_01

# Показываем график
plot_hard_01

# Сохраняем график
# ggsave("Images/Avakian_Hard_01.jpg", plot = plot_hard_01)

# ==================================================================
# Задание повышенной сложности 2: Двумерное нормальное распределение
# ==================================================================

# Подключаем пакет
library(ggplot2)

# Устанавливаем тему
theme_set(theme_bw())

# Создаем данные в датафрейме
set.seed(12345)
df <- data.frame(X = rnorm(1000, 1, 5), Y = rnorm(1000, 2, 5)) 

# Рисуем распределение + точки + легенду
ggplot(df, aes(X, Y)) +
  stat_density_2d(aes(fill = after_stat(level)), geom = "polygon", contour = TRUE, breaks = seq(0.001, 0.006, by = 0.001), alpha = 0.8) +
  geom_point(color = "black", size = 0.2) + 
  scale_fill_gradient(low = "yellow", high = "red") +
  coord_cartesian(xlim = c(-20, 18), ylim = c(-12, 18.5)) +
  labs(fill = "level", x = "X", y = "Y") +
  theme(legend.position = c(0.05, 0.95), legend.justification = c(0, 1)) -> plot_hard_02

# Показываем график
plot_hard_02

# Сохраняем график
# ggsave("Images/Avakian_Hard_02.jpg", plot = plot_hard_02)
