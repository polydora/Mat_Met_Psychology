# Задание 1. 

# Для начала устанавливаем пакет car
install.packages("car")

# Подключаем библиотеку
library(car)

# Вызываем справку через help(Anova)
help(Anova)

# В справке указано, что сновной параметр функции Anova() — это объект модели, т. е. mod (например, результат lm()), который передается как первый аргумент функции.




# Задание 2.
# Формула вычисления логорифма log(x, base = exp(1))
# Числитель: (sqrt(8) / (pi / 2)) - (cos(pi) / sqrt(11 * pi))
# Знаменатель: (10 + exp(1))^5

log(((sqrt(8) / (pi / 2)) - (cos(pi) / sqrt(11 * pi))) / ((10 + exp(1))^5), base = 4)




# Задание 3.

# 1. Установка пакета.
# 2. Подключение библиотеки.
# 3. Чтение файла Excel.
# 4. Структура данных
# 5-7. Построение модели и графики.
# 8. Получение сводки

install.packages("readxl")
library(readxl)
cat <- read_excel("C:/media/Mat_Met_2025/Data/catsM.xlsx")
str(cat) # отсутствовали () 
Model <- lm(Hwt ~ Bwt, data = cat)
par(mfrow = c(2, 2))
plot(Model, page = 1)
summary(Model)




# Задание 4.

n <- round(runif(1, min = 10, max = 50),0)
vector <- round(rnorm(n, 0, 1), 0)

# Количество чисел в векторе.
length(vector)

# Медиана в этом векторе.
median(vector)

# Среднеквадратичное отклонение для данного вектора.
sd(vector)

# Одновременно и максимальное и минимальное значение для данного вектора.
range(vector)




# Задание 5.
df <- data.frame(Var_1 = rep(letters[11:19], each =2), Var_2 = seq(10, 1000, length.out = 18), Var_3 = NA)
df

#Напишите код, который выберет из датафрейма df только те строки, в которых переменная Var_One принимает значения k, s, r.

df_subset <- df[df$Var_1 %in% c("k", "r", "s"), ]
df_subset

# Напишите код, который выберет из датафрейма df только переменные Var_1 и Var_3

df_selected <- df[, c(1, 3)]
df_selected




# Задание 6.

# Функция для сохранения датафрейма write.csv()
write.csv(df, "C:/media/Mat_Met_2025/Data/df.csv")
