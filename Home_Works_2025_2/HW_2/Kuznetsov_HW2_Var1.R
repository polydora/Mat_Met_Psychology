#загружаем нужные библиотеки и датасет
library(ggplot2)
library(dplyr)
data(iris)

#смотрим датасет разными способами
str(iris)
iris
View(iris)

#выбираем и смотрим нужные данные
iris_var1 <- iris %>%
  filter (Species == "setosa") %>%
  pull (Sepal.Length)
View (iris_var1)
str(iris_var1)

#сохраняем выбранные данные в формате csv
iris_var1_freq <- table(iris_var1)
write.csv(iris_var1_freq, file="iris_setosa.csv", row.names=FALSE)

#преобразовываем выбранные данные в табличку чтобы применить функцию geom_histogram и рисуем гистограмму
iris_g <- matrix(iris_var1)
colnames(iris_g) <- c("Sepal.Lenght")
ggplot(data = iris_g, mapping = aes(x = Sepal.Lenght)) +
  geom_histogram(binwidth = 0.1, color = "black", fill = "green", position = "dodge") +
  theme_classic() +
  theme(panel.grid.minor = element_line(color = "gray90", linetype = "solid"))
