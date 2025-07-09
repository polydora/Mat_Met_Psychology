# Задание 1

data(iris)
str(iris)

library(dplyr)
iris %>% 
  filter(Species == "versicolor")
 
df_versicolor <-
  iris %>% 
  filter(Species == "versicolor")

data <- df_versicolor$Sepal.Length

table <- table(data) 

print(table)

# Создание data.frame (таблицы) из объекта table
df_table <- data.frame(
  Sepal.Length = as.numeric(names(table)),   
  Frequency = as.numeric(table)             
)

print(df_table)

write.csv2(df_table, file = "D:/Mat_Met_Biology/Data/df_table_iris_1.csv", row.names=FALSE)


# Задание 2

library(ggplot2)
ggplot(df_table, aes(x = Sepal.Length, y = Frequency)) +
  geom_bar(stat = "identity", fill = "green", color = "black") +
  labs( x = "Значение Sepal.Length", y = "Количество")
