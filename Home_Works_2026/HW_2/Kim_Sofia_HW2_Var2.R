# Домашнее задание №2 Вариант 2
# Первая часть ДЗ
# Откройте датасет iris. Это данные по измерению размеров нескольких частей цветка у трех видов ирисов: setosa, versicolor, virginica. Вариант 2 - работа с versicolor.

data(iris)
versicolor_data <- subset(iris, Species == "versicolor") # Отфильровка части "versicolor". Подсчёт количества значений:
freq_table <- table(versicolor_data$Sepal.Length)
freq_df <- as.data.frame(freq_table)
colnames(freq_df) <- c("Sepal_Length", "Frequency")
freq_df <- freq_df[order(freq_df$Sepal_Length), ]

# freq_table — таблица частот (результат функции table())
# freq_df — датафрейм с частотами (преобразованная таблица)
# Суффиксы _table и _df показывают, какого типа объект хранится в переменной:
# _table — для таблиц (объект класса table)
# _df — для датафреймов (объект класса data.frame)

# Сохраняем в формате CSV на компьютер
write.csv(freq_df, "D:/Text/MatMet_MIP_2026/Data/df.csv", row.names = FALSE)

# Вторая часть. Построение графика.

library(ggplot2)
ggplot(freq_df, aes(x = factor(Sepal_Length), y = Frequency)) +
  geom_col(fill = "green", color = "black", width = 0.9) +
  scale_y_continuous(breaks = seq(0, 8, by = 2),  # последовательность 0,2,4,6,8
                     limits = c(0, 8)) +
  labs(title = "Распределение длины чашелистика (versicolor)",
       x = "Значение Sepal.Length",
       y = "Количество") +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 0, hjust = 0.5),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank()
  )

