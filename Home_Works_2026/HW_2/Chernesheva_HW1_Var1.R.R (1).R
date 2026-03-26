# #загружаем данные с лекции
install.packages("reshape2")
install.packages("dplyr")
install.packages("tidyr")

# ##задание 1
# ##загружаю датасет
data(iris)

# ##проверяю структуру данных
str(iris)
head(iris)

# ##фильтрую данные только для своего варианта, то есть только для вида "setosa"
iris_setosa <- subset(iris, Species == "setosa")

# ##проверяю, что отфильтровано правильно
table(iris_setosa$Species)

# ##создаю таблицу с помощью пакета "dplyr"
install.packages("dplyr")
library(dplyr)
freq_table <- iris_setosa %>%
  group_by(Sepal.Length) %>%
  summarise(Frequency = n())

# ##меняем названия колонок
colnames(freq_table) <- c("Sepal.Length", "Frequency")

# ##смотрим, что получилось
freq_table

# ###задание 2
# ###сохраняю файл
write.csv(freq_table, "iris_setosa_sepal_length_frequencies.csv", 
          row.names = FALSE, fileEncoding = "UTF-8")

# ###проверяю, что получился датафрейм, а не вектор, потому что, как и в прошлом задании, сначала получился вектор
class(freq_table)

# ####задание 3
# ####загружаю штуку, чтобы сделать таблицу
install.packages("ggplot2")
library(ggplot2)

# ####тут я практически все делала по презентации, дипсик подсказал, как сделать таблицу зеленой
freq_table$Sepal.Length <- as.factor(freq_table$Sepal.Length)
ggplot(data = freq_table, aes(x = Sepal.Length, y = Frequency)) + 
  theme_classic() +
  geom_col(fill = "#90EE90", color = "#006400", width = 0.8) +
  geom_text(aes(label = Frequency), 
            vjust = -0.5, 
            size = 5,
            fontface = "bold") +
  labs(x = "Длина чашелистика (см)", y = "Частота", title = "Частотное распределение длины чашелистика\nу ирисов вида Setosa") +
  theme(plot.title = element_text(hjust = 0.5))

# ####вроде все выглядит супер, сохраняем изображение
ggsave("MyPicture.wmf", plot = last_plot())  

# ####и сохраняем в формате cvs
write.csv(freq_table, "setosa_frequencies.csv", row.names = FALSE)

# #####задание повышенной сложности открыть не получилось, на экране ошибка 404
