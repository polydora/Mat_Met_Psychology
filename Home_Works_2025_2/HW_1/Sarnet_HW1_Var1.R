#Задание 1. Получение справки
 
install.packages("car")

help(Anova, package = "car")

# Параметр функции Anova() из пакета car, определяющий объект, который функция получает на входе - это `mod` (сокращение от "model"). 
# Это первый параметр функции.

# Пример вызова функции:
# Anova(mod, ...)


#Задание 2. R как калькулятор, математические операции

log((180/sqrt(2) - cos(pi)/1) / (10 + exp(1))^5, base = 4)


#Задание 3. Переменные и оформление кода


install.packages("readxl")
library(readxl)
cat <- read_excel("Data/catsM.xlsx") #Здесь надо указать путь к файлу на вашем компьютере
str(cat)
Model <- lm(Hwt ~ Bwt, data = cat)
summary(Model)
par(mfrow = c(2, 2))
plot(Model)


#Задание 4. Векторы

n <- round(runif(1, min = 10, max = 50),0)
vector <- round(rnorm(n, 0, 1), 0)

# Количество чисел в нем
length(vector)

# Медиану в этом векторе
median(vector)

# Среднеквадратичное отклонение для данного вектора
sd(vector)

# Одновременно и максимальное и минимальное значение для данного вектора
range(vector)


#Задание 5. Датафреймы

df <- data.frame(Var_1 = rep(letters[11:19], each =2), Var_2 = seq(10, 1000, length.out = 18), Var_3 = NA)
df

# Изменение названий переменных
names(df) <- c("Var_One", "Var_Two", "Var_Three")
df

# Выбор строк, где Var_One принимает значения k, s, r
df[df$Var_One %in% c("k", "s", "r"), ]

# Выбор только переменных Var_One и Var_Three
df[, c("Var_One", "Var_Three")]


#Задание 6. Сохранение данных из датафрейма в файл

write.csv(df, "df.csv", row.names = FALSE)

