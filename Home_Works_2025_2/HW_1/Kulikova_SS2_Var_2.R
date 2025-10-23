# ===== Задание 1. Получение справки =====
# Параметр функции metaMDS(), отвечающий за выбор меры индекса различия: distance

# ===== Задание 2. R как калькулятор =====
# Вычисление выражения
result <- log2((2 * asin(sqrt(0.5)) * (180/pi) - 11^(1/4)) / 
                 ((10 + exp(2)) * 50))
print(result)

# ===== Задание 3. Переменные и оформление кода =====
# Правильная последовательность кода:
#install.packages("readxl")  # Установка пакета (выполняется один раз)
library(readxl)             # Загрузка пакета
cat <- read_excel("Data/catsM.xlsx")  # Чтение файла
str(cat)                    # Просмотр структуры данных
Model <- lm(Hwt ~ Bwt, data = cat)  # Построение линейной модели
par(mfrow = c(2, 2))        # Настройка графического устройства
plot(Model, page = 1)       # Построение графиков
summary(Model)              # Вывод статистической сводки

# ===== Задание 4. Векторы =====
set.seed(42)  # Для воспроизводимости результатов
n <- round(runif(1, min = 10, max = 50), 0)
vector <- round(rnorm(n, 0, 1), 0)  # Исправлено: rnorm вместо nnorm

# Характеристики вектора:
# 1. Количество чисел
length(vector)

# 2. Среднее значение
mean(vector)

# 3. Стандартное отклонение
sd(vector)

# 4. Минимальное и максимальное значение
range(vector)

# ===== Задание 5. Датафреймы =====
# Создание датафрейма (исправлена опечатка в Var_3)
df <- data.frame(
  Var_1 = rep(letters[1:9], each = 2),
  Var_2 = seq(1, 100, length.out = 18),
  Var_3 = NA
)
df

# 1. Изменение названий переменных
names(df) <- c("Var_One", "Var_Two", "Var_Thr")

# 2. Выбор строк с значениями i, c, f
selected_rows <- df[df$Var_One %in% c("i", "c", "f"), ]
View(selected_rows)


# 3. Выбор переменных Var_One и Var_Thr
selected_columns <- df[, c("Var_One", "Var_Thr")]
View(selected_columns)


# ===== Задание 6. Сохранение данных =====
# Установка и загрузка пакета для записи в Excel
#install.packages("writexl")
library(writexl)
# Сохранение в файл
write_xlsx(df, "Data/df.xlsx")
