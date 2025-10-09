# Задание 1 
# object


# Задание 2
log((180 / sqrt(2) - cos(pi) / 11^(1/4)) / (10 + exp(1))^5) / log(4)


#Задание 3
install.packages("readxl")
library(readxl)
cat <- read_excel("data/catsM.xlsx") # Здесь надо указать путь к файлу на вашем компьютере 
str(cat)
Model <- lm(Hwt ~ Bwt, data = cat)
par(mfrow = c(2, 2))
plot(Model, page = 1)


#Задание 4
n <- round(runif(1, min = 10, max = 50),0)
vector <- round(rnorm(n, 0, 1), 0)

length(vector) #количество чисел
median(vector) #медиана
sd(vector)     #среднеквадратичное отклонение
range(vector)  # одновременно макс и мин значение


#Задание 5
df <- data.frame(Var_1 = rep(letters[11:19], each =2), Var_2 = seq(10, 1000, length.out = 18), Var_3 = NA)
df
colnames(df) <- c("Var_One", "Var_Two", "Var_Three") #меняем названия переменных
df

selected_rows <- df[df$Var_One %in% c("k", "s", "r"),] #выбираем строки по условию
selected_rows

subset_data <- df[, c(1, 3)]         #убираем второй столбец (решение 1)
subset_data

library(dplyr)
subset_data1 <- select(df, -Var_Two) #убираем второй столбец (решение 2)
subset_data1


#Задание 6 сохраняем датафрейм
write.csv(df, file = "data/df_hw1.csv")

