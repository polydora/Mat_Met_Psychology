# Задание 1
# distance (параметр функции metaMDS()) из пакета vegan отвечает за выбор меры индекса различия между сравниваемыми объектами.

# Задание 2
log(((180/sqrt(2)-(cos(pi)/11^(1/4)))/((10+exp(1))^5)),base=4)

# Задание 3
install.packages("readxl")       # Установка пакета
library(readxl)                  # Подключение библиотеки
cat <- read_excel("D:/Mat_met/catsM.xlsx") # Загрузка данных
str(cat)                         # Просмотр структуры
Model <- lm(Hwt ~ Bwt, data = cat) # Создание модели
summary(Model)                   # Вывод статистик
par(mfrow = c(2, 2))             # Настройка сетки графиков
plot(Model, page = 1)            # Визуализация

# Задание 4
n <- round(runif(1, min = 10, max = 50),0)
vector <- round(rnorm(n, 0, 1), 0)

length(vector) # кол-во чисел  
mean(vector) #среднее значение
sd(vector) #среднекватратичное отклонение
range(vector) #максимальное и минимальное значение

# Задание 5
df <- data.frame(Var_1 = rep(letters[1:9], each =2), Var_2 = seq(1, 100, length.out = 18), Var_3 = NA)
names(df) <- c("Var_One", "Var_Two", "Var_Thr") # Изменение названия переменных
df[df$Var_One %in% c("i", "c", "f"), ] # Выбор строк
df[, c("Var_One", "Var_Thr")] # Выбор переменных

# Задание 6
install.packages("writexl") # Установка пакета
library(writexl) # Подключение библиотеки
write_xlsx(df, "My_Data_Analysis.xlsx")
getwd()
