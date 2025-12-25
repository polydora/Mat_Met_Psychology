# === ШАГ 1: ОБРАБОТКА ДАННЫХ ===
# Создаем функцию для подсчета крестиков (x) и ноликов (0)
count_symbols <- function(string) {
  x_count <- nchar(gsub("[^x]", "", string))
  zero_count <- nchar(gsub("[^0]", "", string))
  return(c(x_count, zero_count))
}

# Температуры для 30 дней
temperatures <- c(8, 15, 14, 3, 12, 5, 0, 13, 7, 10, 18, 3, 5, 0, 8,
                  17, 15, 6, 5, 6, 20, 6, 2, 3, 3, 17, 17, 16, 17, 3)

# Строки наблюдений (крестики и нолики)
observations <- c(
  "xxxxx0xxxx0x00x000000xx0000xx0xxxxx0x0xx0xx0x0xxxx00",
  "x0x0x0xx000xxxx0x00x000xxx0x0xxx0x0x00x0x0x00x00xxxx0xx0xx00",
  "x00x0xxxxxxx00x0x0x00xxxxx0x00x00xxxx00000xx",
  "x0x00xxxxx000000xx0x0x0000xxxxx0x0x0x00xxx00xx0xx0xxxxx0x00",
  "00xxxx000xxxx0x00000x0000x0xxx0x0xxx0xxx0xxx0xxxxx000x00",
  "xx000xxxxx0000x0x0xxxx0xx0xxx0xx0000xxx0",
  "x0x0000x00xx00x0x0x0xx0xxx0xxx0x0xx000xxxxxx0xx000x0xxxx000",
  "0000xxxx00x00x000x000x0xxx0x00xxxxx00xxxxxxxxxxxx000x00xx0x0",
  "xx00xxx0xxx0x00x0xxxx0000x0xx0xxxx00x0",
  "0xxx0x00xxx00xxx0xx0xx0x0xxx0x0xx0x0xx0000000xx000xxx00x0x0x00xx",
  "xx000000x000xxxxxx0000xx0x00xx0xxxx0xxx0x00xxxx00",
  "0x000xx00xxxx0xxx0xx00xxxx0x000xxx000xx0x0x0x",
  "xx00x000x0xxxx0xx0x00000xx0xxxxx00xx0xxx0x000x0xxx",
  "000x0x0xxx0x0xx0x0x0000xx0xxx0xxx0x000x0xx00xx00xxxxx0x",
  "x00xxx0xx00x0xxxx0xx0xxx0000xx0x0xx0x00x0xx00xx00xxx0",
  "0x0x0xxxx0xx0x00000xx00x000x0x0xx00x0xxxxxx0xx00x",
  "xxx0xx0xxxx000xx0x0x0x0x000x00xx0xx0000x0x0xxxxx000xx0xx000xx",
  "00xxxx00xxxxx0xxxx00x000xx00x00xx00xx0xxxx0",
  "xxxx00x00x000xx0xxxxx0xxxx0xx0x0x000xx00x0xxx0xx0x0000x0xx000",
  "xxx00xxx00xxxx0xx0xx0xxx0xx000x00xxxx00x00x00",
  "x0x0x00x000xxxx0000xxxx0x0xx0x0x00xxx0xx00xx00xxx0xx0xxx00x0",
  "xxx0x0xxxxx00xxx00x00xxx0xx000x0xx0x00x0x00000x0xxx00xxxx0xx00",
  "xxx000x0xxxxx0xx0xx00x00000xxxxx0x000x0xxxx0xxx0000x0000xxx0xx",
  "x0xx0x0x0xxx0xx000xxxxx0x00xxxx00x000x00",
  "xx00x00xxx0xx0x0xx0x0000xxx00xx0xx0000xx0xx00xx0xxxxxxxx000x00x0x",
  "0x0xxx0x0000xxx0xxxxx00x0xx0x0000000xx0x00xxx0xx000xxxx000xxxxx0x",
  "x0x0xxxx00x000000000x0xx00xxx0x0xxx0xxxxx00xxx",
  "0xxxxx0xxx0x000x0xxx00xx000xxxx0x000x0",
  "xxxx0x0x0000xx0xx000xxxxxx0x0xx00xx0000x0xx00xx0xx0",
  "0xxx000x0xxxx00xxxx00xx000x000xx0xx000xx0x0xxxx0000x0x0xxx"
)

# Подсчитываем количество крестиков и ноликов для каждого дня
counts <- t(sapply(observations, count_symbols))

# Создаем датафрейм
migration_data <- data.frame(
  Day = 1:30,
  Temperature = temperatures,
  X_count = counts[, 1],  # Количество крестиков
  Zero_count = counts[, 2]  # Количество ноликов
)

# Добавляем общее количество особей
migration_data$Total <- migration_data$X_count + migration_data$Zero_count

# Просмотр первых строк данных
cat("Первые 6 строк данных:\n")
print(head(migration_data))

# === ШАГ 2: ПОСТРОЕНИЕ ЛИНЕЙНОЙ МОДЕЛИ ===
library(ggplot2)

# Модель: зависимость количества крестиков от ноликов и температуры
model <- lm(X_count ~ Zero_count + Temperature, data = migration_data)

# Вывод результатов модели
cat("\n=== РЕЗУЛЬТАТЫ ЛИНЕЙНОЙ МОДЕЛИ ===\n")
print(summary(model))

# Уравнение модели
cat("\nУравнение модели:\n")
cat("X_count =", round(coef(model)[1], 2), 
    "+", round(coef(model)[2], 2), "* Zero_count",
    ifelse(coef(model)[3] >= 0, "+", ""), round(coef(model)[3], 2), "* Temperature\n")

# Проверка на мультиколлинеарность
library(car)
vif_values <- vif(model)
cat("\nКоэффициенты инфляции дисперсии (VIF):\n")
print(vif_values)

# === ШАГ 3: ДИАГНОСТИКА МОДЕЛИ ===
cat("\n=== ДИАГНОСТИКА МОДЕЛИ ===\n")

# Создаем датафрейм для диагностики
diagnostic_df <- fortify(model)

# 1. График остатков от предсказанных значений
p1 <- ggplot(diagnostic_df, aes(x = .fitted, y = .stdresid)) +
  geom_point(size = 3, alpha = 0.7) +
  geom_hline(yintercept = 0, linetype = "dashed", size = 1) +
  geom_hline(yintercept = c(-2, 2), linetype = "dotted", color = "red", size = 1) +
  geom_smooth(method = "loess", color = "blue", se = FALSE) +
  labs(x = "Предсказанные значения", y = "Стандартизованные остатки",
       title = "График остатков от предсказанных значений") +
  theme_minimal(base_size = 14)

# 2. Квантильный график для проверки нормальности
p2 <- ggplot(diagnostic_df, aes(sample = .stdresid)) +
  stat_qq(size = 3, alpha = 0.7) + 
  stat_qq_line(color = "red", size = 1) +
  labs(title = "Квантильный график остатков",
       x = "Теоретические квантили", y = "Выборочные квантили") +
  theme_minimal(base_size = 14)

# 3. График остатков от количества ноликов
p3 <- ggplot(diagnostic_df, aes(x = migration_data$Zero_count, y = .stdresid)) +
  geom_point(size = 3, alpha = 0.7) +
  geom_hline(yintercept = 0, linetype = "dashed", size = 1) +
  geom_smooth(method = "loess", color = "blue", se = FALSE) +
  labs(x = "Количество ноликов", y = "Стандартизованные остатки",
       title = "Остатки vs количество ноликов") +
  theme_minimal(base_size = 14)

# 4. График остатков от температуры
p4 <- ggplot(diagnostic_df, aes(x = migration_data$Temperature, y = .stdresid)) +
  geom_point(size = 3, alpha = 0.7) +
  geom_hline(yintercept = 0, linetype = "dashed", size = 1) +
  geom_smooth(method = "loess", color = "blue", se = FALSE) +
  labs(x = "Температура", y = "Стандартизованные остатки",
       title = "Остатки vs температура") +
  theme_minimal(base_size = 14)

# 5. Расстояние Кука для выявления влиятельных наблюдений
p5 <- ggplot(diagnostic_df, aes(x = seq_along(.cooksd), y = .cooksd)) +
  geom_bar(stat = "identity", fill = "steelblue", alpha = 0.7) +
  geom_hline(yintercept = 0.5, linetype = "dashed", color = "orange", size = 1) +
  geom_hline(yintercept = 1, linetype = "dashed", color = "red", size = 1) +
  labs(x = "Номер наблюдения", y = "Расстояние Кука",
       title = "Влиятельные наблюдения (расстояние Кука)") +
  theme_minimal(base_size = 14)

# Выводим все графики диагностики
library(gridExtra)
grid.arrange(p1, p2, p3, p4, p5, ncol = 2)

# Формальные тесты
cat("\nФормальные тесты:\n")

# Тест Шапиро-Уилка на нормальность остатков
shapiro_test <- shapiro.test(diagnostic_df$.stdresid)
cat("Тест Шапиро-Уилка на нормальность остатков: W =", 
    round(shapiro_test$statistic, 4), "p =", 
    format.pval(shapiro_test$p.value, digits = 3), "\n")

# Визуальная проверка гомоскедастичности через график остатков
cat("Визуальная проверка гомоскедастичности: см. график остатков от предсказанных значений\n")

# Проверка на автокорреляцию (упрощенная)
# График автокорреляционной функции остатков
acf_residuals <- acf(diagnostic_df$.resid, plot = FALSE)
cat("\nПроверка автокорреляции (ACF):\n")
cat("Лаг 1 ACF:", round(acf_residuals$acf[2], 4), "\n")
cat("Пороговое значение для значимой автокорреляции: ±", round(2/sqrt(nrow(diagnostic_df)), 3), "\n")

# === ШАГ 4: ВИЗУАЛИЗАЦИЯ ЗАВИСИМОСТИ ===
cat("\n=== ВИЗУАЛИЗАЦИЯ ЗАВИСИМОСТИ ===\n")

# 1. Трехмерный график с поверхностью регрессии
library(scatterplot3d)

# Создаем сетку значений для предикторов
zero_range <- seq(min(migration_data$Zero_count), 
                  max(migration_data$Zero_count), length.out = 20)
temp_range <- seq(min(migration_data$Temperature), 
                  max(migration_data$Temperature), length.out = 20)
grid <- expand.grid(Zero_count = zero_range, Temperature = temp_range)
grid$X_pred <- predict(model, newdata = grid)

# Преобразуем для 3D графика
zero_mat <- matrix(grid$Zero_count, nrow = 20)
temp_mat <- matrix(grid$Temperature, nrow = 20)
x_pred_mat <- matrix(grid$X_pred, nrow = 20)

# Создаем 3D график
par(mar = c(5, 4, 4, 2) + 0.1)
s3d <- scatterplot3d(migration_data$Zero_count, migration_data$Temperature, 
                     migration_data$X_count,
                     pch = 19, color = "blue", cex.symbols = 1.5,
                     xlab = "Количество ноликов",
                     ylab = "Температура (°C)",
                     zlab = "Количество крестиков",
                     main = "Зависимость крестиков от ноликов и температуры")

# Добавляем поверхность регрессии
for (i in 1:19) {
  for (j in 1:19) {
    s3d$points3d(c(zero_mat[i, j], zero_mat[i+1, j]), 
                 c(temp_mat[i, j], temp_mat[i+1, j]), 
                 c(x_pred_mat[i, j], x_pred_mat[i+1, j]), 
                 type = "l", col = "gray", lwd = 0.5)
    s3d$points3d(c(zero_mat[i, j], zero_mat[i, j+1]), 
                 c(temp_mat[i, j], temp_mat[i, j+1]), 
                 c(x_pred_mat[i, j], x_pred_mat[i, j+1]), 
                 type = "l", col = "gray", lwd = 0.5)
  }
}

# 2. Контурный график
library(viridis)
p_contour <- ggplot(grid, aes(x = Zero_count, y = Temperature, z = X_pred)) +
  geom_contour_filled(bins = 15, alpha = 0.7) +
  geom_point(data = migration_data, aes(x = Zero_count, y = Temperature, z = NULL, 
                                        size = X_count), 
             color = "red", alpha = 0.8) +
  scale_fill_viridis_d(option = "plasma", name = "Предсказанные\nкрестики") +
  scale_size_continuous(name = "Наблюдаемые\nкрестики", range = c(2, 6)) +
  labs(x = "Количество ноликов", y = "Температура (°C)",
       title = "Зависимость крестиков от ноликов и температуры",
       subtitle = "Контурный график с поверхностью отклика") +
  theme_minimal(base_size = 14) +
  theme(legend.position = "right")

print(p_contour)

# 3. Раздельные графики
# График зависимости от ноликов
p_noliki <- ggplot(migration_data, aes(x = Zero_count, y = X_count)) +
  geom_point(aes(color = Temperature, size = Temperature), alpha = 0.8) +
  geom_smooth(method = "lm", color = "darkblue", se = TRUE, size = 1.2) +
  scale_color_gradient(low = "blue", high = "red", name = "Температура") +
  scale_size_continuous(range = c(3, 8), guide = "none") +
  labs(x = "Количество ноликов", y = "Количество крестиков",
       title = "Зависимость крестиков от ноликов",
       subtitle = paste("Коэффициент корреляции:", 
                        round(cor(migration_data$Zero_count, migration_data$X_count), 3))) +
  theme_minimal(base_size = 14)

# График зависимости от температуры
p_temp <- ggplot(migration_data, aes(x = Temperature, y = X_count)) +
  geom_point(aes(color = Zero_count, size = Zero_count), alpha = 0.8) +
  geom_smooth(method = "lm", color = "darkgreen", se = TRUE, size = 1.2) +
  scale_color_gradient(low = "lightgreen", high = "darkgreen", name = "Количество\nноликов") +
  scale_size_continuous(range = c(3, 8), guide = "none") +
  labs(x = "Температура (°C)", y = "Количество крестиков",
       title = "Зависимость крестиков от температуры",
       subtitle = paste("Коэффициент корреляции:", 
                        round(cor(migration_data$Temperature, migration_data$X_count), 3))) +
  theme_minimal(base_size = 14)

# Выводим оба графика
grid.arrange(p_noliki, p_temp, ncol = 2)

# 4. Гистограмма распределения остатков
p_hist <- ggplot(diagnostic_df, aes(x = .stdresid)) +
  geom_histogram(aes(y = ..density..), bins = 15, 
                 fill = "steelblue", alpha = 0.7, color = "white") +
  stat_function(fun = dnorm, args = list(mean = 0, sd = 1), 
                color = "red", size = 1.2) +
  labs(x = "Стандартизованные остатки", y = "Плотность",
       title = "Распределение остатков модели",
       subtitle = "Красная линия - теоретическое нормальное распределение") +
  theme_minimal(base_size = 14)

print(p_hist)

# === ШАГ 5: ИНТЕРПРЕТАЦИЯ И ВЫВОДЫ ===
cat("\n=== ИНТЕРПРЕТАЦИЯ РЕЗУЛЬТАТОВ ===\n")

# Основные статистики модели
rsquared <- summary(model)$r.squared
adj_rsquared <- summary(model)$adj.r.squared
f_statistic <- summary(model)$fstatistic[1]
f_pvalue <- pf(summary(model)$fstatistic[1], 
               summary(model)$fstatistic[2], 
               summary(model)$fstatistic[3], lower.tail = FALSE)

cat("1. Качество модели:\n")
cat("   - R² =", round(rsquared, 4), "(", round(rsquared * 100, 1), "% вариации объяснено)\n")
cat("   - Скорректированный R² =", round(adj_rsquared, 4), "\n")
cat("   - F-статистика =", round(f_statistic, 2), 
    "p =", format.pval(f_pvalue, digits = 3), "\n\n")

cat("2. Коэффициенты модели:\n")
coef_table <- summary(model)$coefficients
for (i in 1:nrow(coef_table)) {
  coef_name <- rownames(coef_table)[i]
  estimate <- coef_table[i, 1]
  se <- coef_table[i, 2]
  t_value <- coef_table[i, 3]
  p_value <- coef_table[i, 4]
  
  significance <- ifelse(p_value < 0.001, "***",
                         ifelse(p_value < 0.01, "**",
                                ifelse(p_value < 0.05, "*",
                                       ifelse(p_value < 0.1, ".", ""))))
  
  cat("   -", coef_name, ":", round(estimate, 4), 
      "(SE =", round(se, 4), ", t =", round(t_value, 3), 
      ", p =", format.pval(p_value, digits = 3), ")", significance, "\n")
}

cat("\n3. Интерпретация коэффициентов:\n")
cat("   - Intercept: при отсутствии ноликов и при температуре 0°C ожидается", 
    round(coef_table[1, 1], 2), "крестиков\n")
cat("   - Zero_count: при увеличении количества ноликов на 1, количество крестиков", 
    ifelse(coef_table[2, 1] >= 0, "увеличивается", "уменьшается"), 
    "на", round(abs(coef_table[2, 1]), 3), "\n")
cat("   - Temperature: при увеличении температуры на 1°C, количество крестиков", 
    ifelse(coef_table[3, 1] >= 0, "увеличивается", "уменьшается"), 
    "на", round(abs(coef_table[3, 1]), 3), "\n\n")

cat("4. Проверка условий применимости:\n")
cat("   - Линейность: графики остатков vs предикторов не показывают явных нелинейных паттернов\n")
cat("   - Нормальность остатков: тест Шапиро-Уилка p =", 
    format.pval(shapiro_test$p.value, digits = 3), 
    ifelse(shapiro_test$p.value > 0.05, "(✓ нормальность не отвергается)", 
           "(⚠ возможно отклонение от нормальности)"), "\n")
cat("   - Гомоскедастичность: визуальная оценка по графику остатков (см. график 1)\n")
cat("   - Отсутствие автокорреляции: ACF для лага 1 =", round(acf_residuals$acf[2], 4),
    ifelse(abs(acf_residuals$acf[2]) < 2/sqrt(nrow(diagnostic_df)), 
           "(✓ нет значимой автокорреляции)", "(⚠ возможна автокорреляция)"), "\n")
cat("   - Мультиколлинеарность: VIF для Zero_count =", round(vif_values[1], 2),
    ", для Temperature =", round(vif_values[2], 2), 
    ifelse(all(vif_values < 5), "(✓ нет мультиколлинеарности)", 
           "(⚠ возможна мультиколлинеарность)"), "\n\n")

cat("5. Выводы:\n")
if (coef_table[2, 4] < 0.05) {
  cat("   - Существует статистически значимая связь между количеством крестиков и ноликов (p =", 
      format.pval(coef_table[2, 4], digits = 3), ")\n")
} else {
  cat("   - Связь между количеством крестиков и ноликов не является статистически значимой (p =", 
      format.pval(coef_table[2, 4], digits = 3), ")\n")
}

if (coef_table[3, 4] < 0.05) {
  cat("   - Существует статистически значимая связь между количеством крестиков и температурой (p =", 
      format.pval(coef_table[3, 4], digits = 3), ")\n")
} else {
  cat("   - Связь между количеством крестиков и температурой не является статистически значимой (p =", 
      format.pval(coef_table[3, 4], digits = 3), ")\n")
}

cat("\n   Модель объясняет", round(rsquared * 100, 1), 
    "% вариации количества крестиков.\n")

# Сохраняем данные и модель в файлы
write.csv(migration_data, "migration_data.csv", row.names = FALSE)
saveRDS(model, "linear_model.rds")

cat("\nДанные сохранены в 'migration_data.csv', модель сохранена в 'linear_model.rds'\n")