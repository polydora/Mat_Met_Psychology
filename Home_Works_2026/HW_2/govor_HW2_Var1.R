# Загрузка датасета
data(iris)


# ВАРИАНТ 1: Вид "setosa"

iris_setosa <- iris[iris$Species == "setosa", ]
freq_setosa <- as.data.frame(table(iris_setosa$Sepal.Length))
colnames(freq_setosa) <- c("Sepal.Length", "Frequency")
print("Вариант 1 - Setosa:")
print(freq_setosa)

# Устанавливаю пакет 
install.packages("ggplot2")

library(ggplot2)

# 2. Подготовливаю данные
data(iris)
iris_setosa <- iris[iris$Species == "setosa", ]

# 3.  график
p <- ggplot(iris_setosa, aes(x = Sepal.Length)) +
  geom_histogram(fill = "lime", color = "white", binwidth = 0.1) +
  labs(x = "Значение Sepal.Length", y = "Количество") +
  theme_minimal() +
  theme(panel.grid.minor = element_blank(),
        panel.grid.major = element_line(color = "gray80"))

# 4. Отображение графика
print(p)

# 5. Сохранение в файл
ggsave("setosa_sepal_length_histogram.png", 
       plot = p, 
       width = 10, 
       height = 6, 
       dpi = 300)

# ЗАДАНИЕ 3
# вариант 1

# Загрузка данных
data(iris)
tapply(iris$Petal.Length, iris$Species, function(x) {
  c(min = min(x), 
    Q1 = quantile(x, 0.25), 
    median = median(x), 
    Q3 = quantile(x, 0.75), 
    max = max(x))
})

library(ggplot2)

# Построение boxplot
ggplot(iris, aes(x = Species, y = Petal.Length)) +
  geom_boxplot(fill = "gray", color = "black") +
  labs(x = "Category", y = "Value") +
  theme_bw() +
  theme(panel.grid.minor = element_blank())
