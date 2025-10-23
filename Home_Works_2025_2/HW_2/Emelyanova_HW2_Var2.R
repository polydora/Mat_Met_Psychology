##Задание 2. График###

data(iris)

versicolor <- iris[iris$"Species" %in% c("versicolor"),]
versicolor

versicolor[,1]
vsl <- versicolor[,1]

library(readxl)
vsl <- read_excel("D:/Mat_Met_2025/Data/vsl.xlsx") 
vsl

library(ggplot2)

ggplot(data = vsl, mapping = aes(x = Sepal.Length, y = Frequency)) + 
  geom_bar(stat = "identity", fill = "green", color = "black") +
  theme_bw()+
  labs(x = "Значение Sepal.Length", y = "Количество")
  
  
##Не получилось выгрузить данные в csv, криво загружались, не поняла, в чем ошибка. Сделала через xls###




