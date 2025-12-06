#Первая часть дз

#Загружаем данные
data(iris)

#Фильтруем данные для вида "setosa"
iris_setosa <- iris[iris$Species == "setosa", ]

#Создаем таблицу частот для переменной Sepal.Length
freq_table <- table(iris_setosa$Sepal.Length)

#Преобразуем таблицу в data.frame
result_df <- data.frame(Sepal.Length = as.numeric(names(freq_table)),
                        Frequency = as.vector(freq_table))

#Сортируем датафрейм по Sepal.Length
result_df <- result_df[order(result_df$Sepal.Length), ]

#Просмотрим результат
result_df

#Сохраняем в файл CSV
write.csv(result_df, "setosa_sepal_length_freq.csv", row.names = FALSE)


#Вторая часть дз

library(ggplot2)

ggplot(result_df, aes(x = factor(Sepal.Length), y = Frequency)) +
  geom_bar(stat = "identity", 
           fill = "lightpink", 
           color = "darkviolet",
           alpha = 0.4,
           width = 0.8) +
  labs(title = "Частота значений Sepal.Length для Iris setosa",
       x = "Значение Sepal.Length",
       y = "Количество") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(hjust = 0.5))
