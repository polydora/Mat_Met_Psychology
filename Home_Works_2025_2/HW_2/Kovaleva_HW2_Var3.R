###############################################
# Домашнее задание по Математическим методам в биологии
# Вариант 3
# Автор: Ковалева Александра
###############################################

###############################################
# Задание 1. Каждый должен взять себе (отфильтровать) только одну часть данных.
# Вариант 3 работает с видом “virginica”.
###############################################

# Загружаю датасет iris
data("iris")

# Просматриваю первые 6 строк датасета, чтобы понять структуру
head(iris)

# Отбираю только строки, где вид ириса равен "virginica"
virginica_data <- subset(iris, Species == "virginica")

# Просматриваю первые 6 строк отфильтрованного датасета
head(virginica_data)

# Подсчитываю, сколько раз встречается каждое уникальное значение длины чашелистика (Sepal.Length)
counts <- table(virginica_data$Sepal.Length)

# Вывод результата подсчёта
counts

# Преобразую результат подсчёта в датафрейм с двумя колонками
df_counts <- as.data.frame(counts)

# Переименовываю колонки: первая — значение Sepal.Length, вторая — количество (Frequency)
colnames(df_counts) <- c("Sepal.Length", "Frequency")

# Вывожу итоговый датафрейм с подсчётом
df_counts

###############################################
# Задание 2. Построение столбчатой диаграммы по полученным данным
###############################################

# Подключаю пакет ggplot2
library(ggplot2)

# Строю столбчатую диаграмму
ggplot(df_counts, aes(x = Sepal.Length, y = Frequency)) +
  geom_bar(stat = "identity", fill = "green", color = "black") +
  labs( x = "Значение Sepal.Length",y = "Количество") +
  theme_minimal(base_size = 10)

###############################################
# Задания повышенной сложности
###############################################

###############################################
# Задание 1. Построение графика функции y = 2x² + 4x + 50
###############################################

# Значение x от -10 до 10
x <- seq(-10, 10, by = 0.1)

# Вычисляю значения y по формуле
y <- 2 * x^2 + 4 * x + 50

# Создаю датафрейм
df <- data.frame(x = x, y = y)

# Строю график функции
ggplot() +
  geom_line(data = subset(df, x <= 0), aes(x = x, y = y), color = "blue", size = 3.0) +
  geom_line(data = subset(df, x > 0), aes(x = x, y = y),
  color = "red", size = 3.0, linetype = "dashed") +labs(x = "x",y = "y",
  title = expression(y == 2*x^2 + 4*x + 50)) +
  theme_minimal(base_size = 14)

###############################################
# Задание 2. Симуляция двумерного нормального распределения и визуализация
###############################################

set.seed(12345)

# Создаю датафрейм с двумя нормально распределёнными переменными
df <- data.frame(X = rnorm(1000, 1, 5),Y = rnorm(1000, 2, 5)
)

# Строю визуализацию двумерного нормального распределения
ggplot(df, aes(x = X, y = Y)) +
  stat_density_2d(aes(fill = ..level..), geom = "polygon") +
  geom_point(color = "black", size = 0.3, alpha = 0.4) +
  scale_fill_gradient(low = "yellow", high = "red") +
  theme_minimal(base_size = 14) +
  labs(title = "Визуализация двумерного нормального распределения",x = "X",y = "Y",fill = "Уровень плотности") +
  coord_cartesian(xlim = c(-20, 20), ylim = c(-10, 20)) +
  theme(panel.border = element_rect(color = "black", fill = NA, linewidth = 0.5),
  legend.position = c(0.1, 0.75),legend.background = element_rect(fill = "white", color = "white"),
  legend.key.height = unit(0.6, "cm"),legend.key.width = unit(0.8, "cm"),
  legend.title = element_text(size = 10, face = "plain"),
  plot.title = element_text(size = 16, face = "bold"),
  axis.title.x = element_text(size = 12),axis.title.y = element_text(size = 12))
