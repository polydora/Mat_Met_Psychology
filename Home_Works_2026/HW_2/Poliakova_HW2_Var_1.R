# Загрузка необходимых библиотек
install.packages("dplyr")
install.packages("ggplot2")
library(ggplot2)
library(dplyr)

data(iris)
result_df <- iris %>%
filter(Species == "setosa") %>%       # Фильтруем вид setosa
group_by(Sepal.Length) %>%            # Группируем по длине чашелистика
summarise(Frequency = n()) %>%        # Считаем количество встреч (частоту)
rename(Sepal.Length = Sepal.Length)   # Убеждаемся в названии колонок

print(result_df)

write.csv(result_df, "setosa_sepal_length_frequencies.csv", row.names = FALSE, fileEncoding = "UTF-8") # Сохранение в формате CSV

# Построение графика
ggplot(result_df, aes(x = as.factor(Sepal.Length), y = Frequency)) +
# Рисуем столбики: светло-зелёная заливка, тёмно-зелёный контур
geom_col(fill = "#90EE90", color = "#006400", width = 0.7) + 
# Добавляем числа над столбцами
geom_text(aes(label = Frequency), vjust = -0.5, size = 4, fontface = "bold") +
# Названия осей и заголовок
labs(
title = "Частота значений Sepal.Length для вида Setosa",
x = "Длина чашелистика (см)", 
y = "Количество (частота)"
) +
# Чистая светлая тема
theme_light() +
# Настройка, чтобы столбцы стояли прямо на линии
scale_y_continuous(expand = expansion(mult = c(0, 0.1)))

# Сохранение графика
ggsave("setosa_plot.png", width = 8, height = 5)



# Задание 1
# Создаём данные
x <- seq(-10, 10, length.out = 500)
y <- 2 * x^2 + 4 * x + 50
df1 <- data.frame(x = x, y = y)
# Рисуем график
ggplot(df1, aes(x = x, y = y)) +
# Сплошная синяя линия для x <= 0
geom_line(data = subset(df1, x <= 0), color = "blue", size = 1.5) +
# Пунктирная красная линия для x > 0
geom_line(data = subset(df1, x > 0), color = "red", size = 1.5, linetype = "dashed") +
theme_bw() +
labs(x = "x", y = "y")

# Задание 2
set.seed(12345)

df2 <- data.frame(X = rnorm(1000, 1, 5), Y = rnorm(1000, 2, 5))

ggplot(df2, aes(x = X, y = Y)) +
stat_density_2d(aes(fill = after_stat(level)), geom = "polygon") +
geom_point(size = 0.5, alpha = 0.6) +
scale_fill_gradient(low = "yellow", high = "red") +
theme_bw() +
labs(x = "X", y = "Y", fill = "level")

