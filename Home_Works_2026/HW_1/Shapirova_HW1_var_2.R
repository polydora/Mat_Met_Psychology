#Задание 1
install.packages("vegan")
library(vegan)

#не работает


#Задание 2
result <- log2(
  (2 * asin(sqrt(0.5)) * (180 / pi) - 11^(1/4)) /
    ((10 + exp(2)) * 50)
) 
print(result)
#Ничего не поняла в примере, но вроде сработало. Ответ = -3.301603


#Задание 3
install.packages("readxl") 
library(readxl)
cat <- read_excel("Data/catsM.xlsx")
str(cat)
Model <- lm(Hwt ~ Bwt, data = cat)
par(mfrow = c(2, 2))
plot(Model, page = 1)
summary(Model)
#Ошибка как в первом задании


#Задание 4

#Скопировала код
n <- round(runif(1, min = 10, max = 50), 0)
vector <- round(rnorm(n, 0, 1), 0)

length_vector <- length(vector)
print(paste("Количество чисел в векторе:", length_vector))
#Количество чисел в векторе: 46

mean_vector <- mean(vector)
print(paste("Среднее значение:", mean_vector))
#Среднее значение: 0.0434782608695652

sd_vector <- sd(vector)
print(paste("Среднеквадратичное отклонение:", sd_vector))
#Среднеквадратичное отклонение: 1.11468025787429

max_value <- max(vector)
min_value <- min(vector)
print(paste("Максимальное значение:", max_value, "Минимальное значение:", min_value))
#"Макс.значение: 2 Мин.значение: -3"

#Задание 5
df <- data.frame(Var_1 = rep(letters[1:9], each = 2), Var_2 = seq(1, 100, length.out = 18), Var_3 = NA)
df

colnames(df) <- c("Var_One", "Var_Two", "Var_Thr")
#Переименова колонки

selected_rows <- subset(df, Var_One %in% c("c", "f", "i"))
#Сократила строки

selected_columns <- df[, c("Var_One", "Var_Thr")]
#Выбрала определенные колонки

print(df) 
print(selected_rows) 
print(selected_columns)
#Это для вывода, по другому в консоль не пишет


#Задание 6
install.packages("writexl")
library(writexl)
write_xlsx(df, "Data/df.xlsx")
#Та же ошибка, что и раньше с пакетами -- ничего не устанавливаеться(((((
