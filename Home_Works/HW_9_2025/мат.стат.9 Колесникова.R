# Загружаем необходимые библиотеки
library(ggplot2)

# 1. Загружаем датасет iris и выбираем вид setosa
data(iris)
setosa_data <- subset(iris, Species == "setosa")

# Строим график в виде точечной диаграммы
ggplot(setosa_data, aes(x = Petal.Length, y = Sepal.Length)) +
  geom_point(color = "blue") +
  labs(title = "Точечная диаграмма Sepal.Length от Petal.Length для Iris setosa",
       x = "Длина лепестка (Petal.Length)",
       y = "Длина чашелистика (Sepal.Length)")

# 2. Строим линейную модель
model <- lm(Sepal.Length ~ Petal.Length, data = setosa_data)

# Выводим информацию об оценке модели
model_summary <- summary(model)
print(model_summary)

# 3. Проверка условий применимости модели
par(mfrow = c(2, 2))
plot(model)

# По графикам:
# - Показатели на графиках остатков случайные и не демонстрируют паттернов.
# - Остатки выглядят нормально распределенными и гомоскедастичными,т.е. модель применима.

# 4. Оценка статистической значимости
# Смотрим на p-value для переменной Petal.Length
# Если p-value меньше 0.05, то связь между Petal.Length и Sepal.Length является статистически значимой.
if (model_summary$coefficients[2, 4] < 0.05) {
  cat("Связь между Petal.Length и Sepal.Length статистически значима (p < 0.05).\n")
} else {
  cat("Связь между Petal.Length и Sepal.Length не статистически значима (p >= 0.05).\n")
}

# 5. Трактовка результатов
# Смотрим на коэффициенты модели. Коэффициент для Petal.Length показывает, как на каждое увеличение на единицу
# длины лепестка получается изменение длины чашелистика. Это значение может быть использовано для предсказания.

# 6. Строим линию регрессии с 95% доверительной областью
ggplot(setosa_data, aes(x = Petal.Length, y = Sepal.Length)) +
  geom_point(color = "blue") +
  geom_smooth(method = "lm", se = TRUE, color = "red") +
  labs(title = "Линейная регрессия с 95% доверительной зоной для Iris setosa",
       x = "Длина лепестка (Petal.Length)",
       y = "Длина чашелистика (Sepal.Length)")

# 7. Предсказание значений при 30% и 99% перцентилях Petal.Length
pred_30 <- quantile(setosa_data$Petal.Length, 0.30)
pred_99 <- quantile(setosa_data$Petal.Length, 0.99)

pred_values <- predict(model, newdata = data.frame(Petal.Length = c(pred_30, pred_99)))
names(pred_values) <- c("30th Percentile", "99th Percentile")

# Выводим предсказанные значения
print(pred_values)

# Комментарии по предсказанным значениям:
# - Значение Sepal.Length для 30-го перцентиля Petal.Length — упаковка данных на нижнем уровне.
# - Значение Sepal.Length для 99-го перцентиля Petal.Length показывает, что при высокой длине лепестка
# длина чашелистика также будет значительно больше.
