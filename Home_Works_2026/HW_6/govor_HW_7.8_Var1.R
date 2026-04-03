# ============================================================================
# ДОМАШНЕЕ ЗАДАНИЕ №7
# Анализ солености (переменная S) по летним месяцам
# ============================================================================

# 1. ЗАГРУЗКА БИБЛИОТЕК

library(readxl)     # Библиотека для чтения Excel файлов
library(ggplot2)    # Библиотека для визуализации данных
library(car)        # Библиотека для статистических тестов (включая тест Левена)


# 2. ЧТЕНИЕ И ПОДГОТОВКА ДАННЫХ

# Чтение данных из Excel файла
data <- read_excel("Data/hydrology_2022.xls")

# Фильтрация данных: оставляем только летние месяцы (июнь, июль, август)
# и удаляем строки с пропущенными значениями солености (S)
summer_data <- subset(data, 
                      Month %in% c("June", "July", "August") & !is.na(S))

# Преобразование переменной Month в фактор с заданным порядком уровней
summer_data$Month <- factor(summer_data$Month, 
                            levels = c("June", "July", "August"))

# 3. ПРОВЕДЕНИЕ ANOVA

# Создание модели однофакторного дисперсионного анализа (ANOVA)
# Зависимая переменная: S (соленость)
# Фактор: Month (месяц)
anova_model <- aov(S ~ Month, data = summer_data)

# Вывод таблицы результатов ANOVA
# Показывает сумму квадратов, степени свободы, F-статистику и p-value
summary(anova_model)

# 4. ПРОВЕРКА УСЛОВИЙ ПРИМЕНИМОСТИ ANOVA

# 4.1. Проверка нормальности распределения остатков
# Тест Шапиро-Уилка для проверки нормальности остатков модели
# Нулевая гипотеза: остатки распределены нормально
shapiro.test(residuals(anova_model))

# 4.2. Проверка гомогенности (однородности) дисперсий
# Тест Левена для проверки равенства дисперсий между группами
# Нулевая гипотеза: дисперсии всех групп равны
leveneTest(S ~ Month, data = summer_data)


# 5. POST-HOC АНАЛИЗ (ТЮКИ)

# Проведение множественных попарных сравнений с помощью теста Тьюки
# Используется для определения, какие именно пары месяцев различаются
TukeyHSD(anova_model)

# 6. ВИЗУАЛИЗАЦИЯ РЕЗУЛЬТАТОВ

# 6.1. Создание boxplot (ящичковой диаграммы) с доверительными интервалами
ggplot(summer_data, aes(x = Month, y = S, fill = Month)) +
  # Добавление ящичковой диаграммы (boxplot)
  # alpha=0.6 задает прозрачность 60%
  geom_boxplot(alpha = 0.6) +
  
  # Добавление доверительных интервалов для средних значений
  # fun.data=mean_cl_normal вычисляет среднее и доверительный интервал
  # width=0.3 задает ширину усиков
  # color="red" задает красный цвет для интервалов
  stat_summary(fun.data = mean_cl_normal, 
               geom = "errorbar", 
               width = 0.3, 
               color = "red") +
  
  # Добавление точек со средними значениями
  # fun=mean вычисляет среднее значение
  # size=3 задает размер точек
  stat_summary(fun = mean, 
               geom = "point", 
               size = 3) +
  
  # Добавление заголовка и подписей осей
  labs(title = "Распределение солености по летним месяцам",
       subtitle = "Красные линии - доверительные интервалы средних",
       x = "Месяц",
       y = "Соленость (S)") +
  
  # Применение минималистичной темы
  theme_minimal() +
  
  # Удаление легенды (так как цвет соответствует месяцу)
  theme(legend.position = "none")

# 6.2. Визуализация результатов теста Тьюки
# Построение графика с доверительными интервалами различий между парами месяцев
plot(TukeyHSD(anova_model), 
     las = 2,           # Поворот подписей оси Y на 90 градусов
     cex.axis = 0.8,    # Уменьшение размера шрифта подписей
     main = "Попарные сравнения (Tukey HSD)")

# 7. ИНТЕРПРЕТАЦИЯ РЕЗУЛЬТАТОВ

# Вывод основных статистик по группам
# Показывает количество наблюдений, среднее, стандартное отклонение
# и квантили для каждого месяца
tapply(summer_data$S, summer_data$Month, function(x) {
  c(n = length(x),
    mean = mean(x),
    sd = sd(x),
    min = min(x),
    max = max(x))
})
# КОНЕЦ

## ЗАДАНИЕ 8 
# Загрузка библиотек
library(readxl); library(ggplot2); library(car)

# Чтение данных (пропускаем 2 строки заголовка)
d <- read_excel("hydrology_2022.xls", skip = 2)

# Извлечение часа и создание фактора времени суток
d$hour <- as.numeric(format(as.POSIXct(d$Date_Time, "%d.%m.%Y %H:%M"), "%H"))
d$Time <- factor(ifelse(d$hour==0,"00:00",ifelse(d$hour==6,"06:00",ifelse(d$hour==12,"12:00","18:00"))))

# Фильтрация: летние месяцы, нужное время, без NA по солености
d <- subset(d, Month %in% c("June","July","August") & Time %in% c("00:00","06:00","12:00","18:00") & !is.na(S))
d$Month <- factor(d$Month, c("June","July","August"))
d <- na.omit(d)  # Удаление всех строк с NA

# Двухфакторный ANOVA с взаимодействием
m <- aov(S ~ Month * Time, data = d)

# Результаты ANOVA
cat("===== РЕЗУЛЬТАТЫ ANOVA =====\n")


summary(m)

# Проверка нормальности остатков (Шапиро-Уилк)
cat("\n===== ТЕСТ ШАПИРО-УИЛКА =====\n")
shapiro.test(resid(m))

# Проверка гомогенности дисперсий (Левен)
cat("\n===== ТЕСТ ЛЕВЕНА =====\n")
leveneTest(S ~ Month * Time, data = d)

# Post-hoc тесты Тьюки
cat("\n===== POST-HOC (Month) =====\n")

TukeyHSD(m, "Month")
cat("\n===== POST-HOC (Time) =====\n")
TukeyHSD(m, "Time")

# Визуализация 1: Boxplot
ggplot(d, aes(Month, S, fill=Time)) + 
  geom_boxplot(alpha=0.6) + 
  stat_summary(fun.data=mean_cl_normal, geom="errorbar", width=0.2, position=position_dodge(0.8)) +
  labs(title="Соленость по месяцам и времени суток", x="Месяц", y="S") +
  theme_minimal()

# Визуализация 2: График взаимодействия
interaction.plot(d$Month, d$Time, d$S, fun=mean, col=1:4, pch=1:4, main="Взаимодействие Month×Time")

# Интерпретация результатов
cat("\n===== ИНТЕРПРЕТАЦИЯ =====\n")
p_month <- summary(m)[[1]]["Month","Pr(>F)"]
p_time <- summary(m)[[1]]["Time","Pr(>F)"]
p_inter <- summary(m)[[1]]["Month:Time","Pr(>F)"]
cat("Month: p =", p_month, ifelse(p_month<0.05,"✓ значим","✗ не значим"), "\n")
cat("Time: p =", p_time, ifelse(p_time<0.05,"✓ значим","✗ не значим"), "\n")
cat("Interaction: p =", p_inter, ifelse(p_inter<0.05,"✓ значимо","✗ не значимо"), "\n")
