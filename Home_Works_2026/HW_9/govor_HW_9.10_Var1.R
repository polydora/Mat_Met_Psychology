## ДОМАШНЕЕ ЗАДАНИЕ 9, работа с датасетом 
# Загрузка библиотеки
library(ggplot2)

# Фильтрация данных для вида virginica
data <- subset(iris, Species == "virginica")

# 1. Точечная диаграмма
plot(data$Petal.Length, data$Sepal.Length, 
     main = "Sepal.Length vs Petal.Length (virginica)",
     xlab = "Petal.Length", ylab = "Sepal.Length",
     pch = 19, col = "blue")

# 2. Построение линейной модели
model <- lm(Sepal.Length ~ Petal.Length, data = data)
summary(model)

# 3. Проверка условий применимости модели
# Проверка нормальности остатков
shapiro.test(residuals(model))

# Проверка гомоскедастичности (постоянства дисперсии)
plot(fitted(model), residuals(model),
     main = "Остатки vs Подогнанные значения",
     xlab = "Подогнанные значения", ylab = "Остатки")
abline(h = 0, col = "red")

# 4. Статистическая значимость (из summary(model))
# Смотрим p-value для Petal.Length

# 5. Интерпретация результатов
cat("Коэффициенты модели:\n")
coef(model)

# 6. График с линией регрессии и доверительным интервалом
plot(data$Petal.Length, data$Sepal.Length,
     main = "Линейная регрессия с 95% ДИ",
     xlab = "Petal.Length", ylab = "Sepal.Length",
     pch = 19, col = "blue")
abline(model, col = "red", lwd = 2)

# Добавление доверительного интервала
newdata <- data.frame(Petal.Length = seq(min(data$Petal.Length), 
                                         max(data$Petal.Length), length = 100))
pred <- predict(model, newdata, interval = "confidence")
lines(newdata$Petal.Length, pred[, "lwr"], col = "green", lty = 2)
lines(newdata$Petal.Length, pred[, "upr"], col = "green", lty = 2)

# 7. Предсказание для 30% и 99% перцентилей
p30 <- quantile(data$Petal.Length, 0.30)
p99 <- quantile(data$Petal.Length, 0.99)

cat("\n30-й перцентиль Petal.Length:", p30, "\n")
cat("99-й перцентиль Petal.Length:", p99, "\n")

pred_30 <- predict(model, data.frame(Petal.Length = p30))
pred_99 <- predict(model, data.frame(Petal.Length = p99))

cat("\nПредсказанный Sepal.Length для 30% перцентиля:", pred_30, "\n")
cat("Предсказанный Sepal.Length для 99% перцентиля:", pred_99, "\n")

## ДОМАШНЕЕ ЗАДАНИЕ 10

# Создаем данные из таблицы
data <- data.frame(
  Day = 1:30,
  Temperature = c(13, 14, 9, 9, 2, 18, 15, 12, 8, 10, 20, 10, 5, 11, -1, 17, 15, 2, 18, 12, 5, 20, 0, 1, 0, 10, 11, 2, 0, 1),
  Species = c(
    "xxxxxxxx0000x0xxxxxxx0x000x000x0x000xxx00x0",
    "x00xxxxxxx00xxx0x00xxx00x000xxx00x0xxx0xxx0x00",
    "000x0x0xxx0x00x000xxx0x000x00xxx0x0x0x0xxxxx0x0x00x0xxx0x0x0x",
    "x00x0x0x0x0xxx0x0x0x0x0x0xxx0x0x0x00xxxx0x0x0x0x0x0x0",
    "x000xxx0x0x0xxx0x00xxx0x0xxxxxx0x0x0xxxx0x00x0",
    "x000x0xxxxx00x0x0000xxx0x00xxxxx000xxxx0xxxxx",
    "x0xxx0x0xx000x0x00xx0x0x0x0x000xxx00x0x000xxxxxxx00",
    "xx0x0x000x0xxx000x0x0xxx0x0x0xxxx0x",
    "xx00xx0xx0x0x000x00xxxx00xxx0x0x000xxx00xxxxx",
    "000x00xxxxx0000x000x0x0x0x0x0x0x0xxx00xxxxxxxx0x",
    "x000x0xxx0x0xx0x0xxx0x0xxxx0x00x0x0xxxxx0xxx00x0x000",
    "000x0x000xxxxx0x0xxxx0x0x0000xxxx00x0x0x00xxx0xx",
    "xx0x0000xxx0x0x0xxx0x000x00xxxx00x00000xxxx0xxxx0xxxx0",
    "0xx00x0xxx0x0x000xxxxx0x00xx0x0x0x000xx0x0x0x",
    "x00x00x0x00000xxx0xxx00x0x00x0xxxx0x00x0xxxx0xxxxxx000xxxx0x00",
    "xx000xxxx0xxxxx0x000x0x00xx0x000x0xxxx000xxx",
    "x0x0x0xx0x0x0x0xxx0x00xxxx0x0xxxxxx0xxx00xx00",
    "0xxx00xxx0x000xxxxx0x000xx0x0x0x0x0x0xxxx00",
    "x0000xx0x0xx0x0x0x00xxxxx0xx0x0x0x0x0x0x0xxxxx0x0x0",
    "x0000000xxxxx00000xxx0x0x0xxx0x000x00x0x0000xxxx0xxx00xxxx0",
    "xx0xx00x00xxx00xxxxx0000000xxxx0xxxx0xx",
    "00x000xxxxx00xxxxx0xxxx0x0x00xx0x000x0x00xxxxx00x00x0xx0",
    "000xx0x0x0x0x0x0x00xxx0xxxxx00x0xxx000xx00x0x0x0x",
    "xxx000xxxxx00x0x0xxx0x0x0x000xxx00x000xxxxx000x0x0xxx0",
    "0x0x0xxxx0x0xxxxx00x0x0x00xxx00000x0000xxxx00x0x00x0xxxx",
    "xx00x0x00xxxxx00x0x00xxx0xxx00000x0xx000xxxxx000xx0x0x0x0",
    "0x00xxxx0x0x0xxxx0000000xxx0x000x0xxxx0xxxxxxxxx000x",
    "0x00x0xxxxx0x00000xxxxx0000x00xxxxxxxxx0x0xxx0x00x",
    "0xxx0x0000x000xxx0xxxxx0x00x0x0x0x00x0x0x0x00xxx",
    "0xxx00x0x00x0xxxxx000x0x0x0x0x0000xxxxxx0x00x0x0x0xx0"
  )
)

# Подсчитываем количество крестиков (x) и ноликов (0)
data$Count_x <- sapply(data$Species, function(s) sum(strsplit(s, "")[[1]] == "x"))
data$Count_0 <- sapply(data$Species, function(s) sum(strsplit(s, "")[[1]] == "0"))

# Просмотр данных
head(data)
str(data)

# 1. ПОСТРОЕНИЕ ЛИНЕЙНОЙ МОДЕЛИ

# Линейная модель: Count_x ~ Count_0 + Temperature
model <- lm(Count_x ~ Count_0 + Temperature, data = data)

# Результаты модели
summary(model)


# 2. ДИАГНОСТИКА МОДЕЛИ

cat("\n===== ДИАГНОСТИКА МОДЕЛИ =====\n")

# 2.1. Проверка нормальности остатков (тест Шапиро-Уилка)
shapiro_test <- shapiro.test(residuals(model))
print(shapiro_test)

# 2.2. Проверка гомоскедастичности (постоянства дисперсии остатков)
# Тест Бройша-Пагана
library(lmtest)
bp_test <- bptest(model)
print(bp_test)

# 2.3. Проверка на мультиколлинеарность
library(car)
vif_values <- vif(model)
print(vif_values)

# 2.4. Графики диагностики
par(mfrow = c(2, 2))
plot(model)

# Гистограмма остатков
hist(residuals(model), breaks = 10, 
     main = "Распределение остатков",
     xlab = "Остатки", col = "lightblue")
abline(v = 0, col = "red", lwd = 2)

# QQ-plot
qqnorm(residuals(model), main = "QQ-plot остатков")
qqline(residuals(model), col = "red")


# 3. ВИЗУАЛИЗАЦИЯ ЗАВИСИМОСТЕЙ

library(ggplot2)
library(gridExtra)

# График 1: Count_x vs Count_0
p1 <- ggplot(data, aes(x = Count_0, y = Count_x)) +
  geom_point(size = 3, color = "blue") +
  geom_smooth(method = "lm", se = TRUE, color = "red") +
  labs(title = "Зависимость крестиков от ноликов",
       x = "Количество ноликов (Count_0)",
       y = "Количество крестиков (Count_x)") +
  theme_minimal()

# График 2: Count_x vs Temperature
p2 <- ggplot(data, aes(x = Temperature, y = Count_x)) +
  geom_point(size = 3, color = "green") +
  geom_smooth(method = "lm", se = TRUE, color = "red") +
  labs(title = "Зависимость крестиков от температуры",
       x = "Температура",
       y = "Количество крестиков (Count_x)") +
  theme_minimal()

# График 3: 3D визуализация (Count_x от Count_0 и Temperature)
library(plotly)

p3 <- plot_ly(data, x = ~Count_0, y = ~Temperature, z = ~Count_x,
              type = "scatter3d", mode = "markers",
              marker = list(size = 5, color = "blue")) %>%
  add_surface(x = ~sort(unique(Count_0)), 
              y = ~sort(unique(Temperature)),
              z = ~matrix(predict(model), nrow = length(unique(Count_0))),
              showscale = FALSE,
              opacity = 0.5) %>%
  layout(title = "3D визуализация модели",
         scene = list(xaxis = list(title = "Count_0"),
                      yaxis = list(title = "Temperature"),
                      zaxis = list(title = "Count_x")))

# Отображение графиков
print(p1)
print(p2)
# Для 3D графика: p3 (откроется в браузере или Viewer)

# График остатков vs подогнанные значения
p4 <- ggplot(data, aes(x = fitted(model), y = residuals(model))) +
  geom_point(size = 3, color = "purple") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  labs(title = "Остатки vs Подогнанные значения",
       x = "Подогнанные значения",
       y = "Остатки") +
  theme_minimal()

print(p4)


# ИНТЕРПРЕТАЦИЯ РЕЗУЛЬТАТОВ

cat("\n===== ИНТЕРПРЕТАЦИЯ РЕЗУЛЬТАТОВ =====\n\n")

cat("Коэффициенты модели:\n")
print(coef(model))

cat("\nСтатистическая значимость:\n")
cat("p-value для Count_0:", summary(model)$coefficients["Count_0", "Pr(>|t|)"], "\n")
cat("p-value для Temperature:", summary(model)$coefficients["Temperature", "Pr(>|t|)"], "\n")

cat("\nПроверка условий:\n")
cat("- Нормальность остатков (Шапиро-Уилк): p-value =", shapiro_test$p.value, "\n")
if(shapiro_test$p.value < 0.05) {
  cat("  ✗ Нарушение нормальности остатков!\n")
} else {
  cat("  ✓ Остатки распределены нормально\n")
}

cat("- Гомоскедастичность (Бройш-Паган): p-value =", bp_test$p.value, "\n")
if(bp_test$p.value < 0.05) {
  cat("  ✗ Нарушение постоянства дисперсии (гетероскедастичность)!\n")
} else {
  cat("  ✓ Дисперсия остатков постоянна\n")
}

cat("- Мультиколлинеарность (VIF):\n")
if(max(vif_values) > 5) {
  cat("  ✗ Есть проблема мультиколлинеарности (VIF > 5)\n")
} else {
  cat("  ✓ Мультиколлинеарность не критична (VIF < 5)\n")
}

cat("\nВозможные проблемы модели:\n")
if(shapiro_test$p.value < 0.05) cat("- Нарушение нормальности остатков\n")
if(bp_test$p.value < 0.05) cat("- Гетероскедастичность (непостоянная дисперсия)\n")
if(max(vif_values) > 5) cat("- Мультиколлинеарность предикторов\n")

cat("\nR-squared модели:", summary(model)$r.squared, "\n")
cat("Это означает, что модель объясняет", round(summary(model)$r.squared * 100, 1), 
    "% дисперсии количества крестиков\n")

