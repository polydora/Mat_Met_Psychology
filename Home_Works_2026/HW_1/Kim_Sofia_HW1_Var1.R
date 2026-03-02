# Задание 1. Получение справки
# Какой параметр функции Anova() из пакета car определяет объект, который функция получает на входе. 

?anova()

# Ответ: 
# Объект, получаемый на входе функцией Anova, определяется параметром mod, но напрямую функция не будет работать с объектом, пока не будет указана определённая модель (в основном - lm, glm).
# Например, model_1 <- lm(y ~ a * b, data = df)
# result <- Anova(mod = model_1)
# Только перед этим нужно загрузить соответствующий пакет car


# Задание 2. R как калькулятор, математические операции
# Напишите код, который позволит найти решение этого примера
# [Math Processing Error]
# Просто напишите код.

a <- 180 / sqrt(2) - cos(pi) / (11^(1/5))
b <- (10 + exp(1))^5
value <- a / b 
result <- log(value) / log(4)
print(result)



# Задание 3. Переменные и оформление кода

install.packages("readxl")
library(readxl)
cat <- read_excel("D:/Text/MatMet_MIP_2026/Data/catsM.xlsx")
str(cat)
Model <- lm(Hwt ~ Bwt, data = cat)
par(mfrow = c(2, 2))
plot(Model, page = 1)
summary(Model)






# Задание 4. Векторы

n <- round(runif(1, min = 10, max = 50),0)
vector <- round(rnorm(n, 0, 1), 0)

# Количество чисел
length(vector)
# Медиана в векторе
median(vector)
# Среднеквадратичное отклонение в векторе
sd(vector)
# Самое минимальное и максимальное значения
range(vector)


# Задание 5. Датафреймы
# Скопируйте и выполните код, который создаст датафрейм под названием df

df <- data.frame(Var_1 = rep(letters[11:19], each =2), Var_2 = seq(10, 1000, length.out = 18), Var_3 = NA)
df
##    Var_1      Var_2 Var_3
## 1      k   10.00000    NA
## 2      k   68.23529    NA
## 3      l  126.47059    NA
## 4      l  184.70588    NA
## 5      m  242.94118    NA
## 6      m  301.17647    NA
## 7      n  359.41176    NA
## 8      n  417.64706    NA
## 9      o  475.88235    NA
## 10     o  534.11765    NA
## 11     p  592.35294    NA
## 12     p  650.58824    NA
## 13     q  708.82353    NA
## 14     q  767.05882    NA
## 15     r  825.29412    NA
## 16     r  883.52941    NA
## 17     s  941.76471    NA
## 18     s 1000.00000    NA
names(df) <- c("Var_One", "Var_Two", "Var_Three")
print(head(df, 6))

df_selected <- subset(df, Var_One %in% c("k", "s", "r"))
print(df_selected)

df[, -2]

# Задание 6. Сохранение данных из датафрейма в файл
# Напишите код, который сохранит датафрейм df на ваш компьютер в формате .csv.

write.csv(df, "D:/Text/MatMet_MIP_2026/Data/df.csv", row.names = FALSE)
