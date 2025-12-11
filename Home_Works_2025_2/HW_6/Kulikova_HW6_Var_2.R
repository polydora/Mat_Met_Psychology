# ==============================================
# ВАРИАНТ 2: АНАЛИЗ ДАННЫХ РОСТА РИСА (RICE DATASET)
# Корректный код с правильными именами переменных
# ==============================================

# 1. ЗАГРУЗКА БИБЛИОТЕК И ДАННЫХ
# ==============================================
# Устанавливаем пакеты, если их нет
if (!require(DAAG)) install.packages("DAAG")
if (!require(ggplot2)) install.packages("ggplot2")
if (!require(car)) install.packages("car")
if (!require(dplyr)) install.packages("dplyr")

library(DAAG)
library(ggplot2)
library(car)
library(dplyr)

# Загрузка данных
data(rice)

# 2. ПРОВЕРКА И ПРЕОБРАЗОВАНИЕ ДАННЫХ
# ==============================================
cat("=== ПРОВЕРКА ДАННЫХ ===\n")

# Показываем структуру данных
cat("Структура данных rice:\n")
str(rice)

cat("\nКолонки в датасете rice:\n")
print(names(rice))

# Создаем датафрейм для анализа с нужными переменными
rice_data <- data.frame(
  dry_weight = rice$ShootDryMass,  # Сухая масса побегов
  variety = rice$variety,          # Сорт риса
  fertilizer = rice$fert           # Удобрение
)

cat("\nСтруктура созданного датафрейма:\n")
str(rice_data)

# 3. ОПИСАТЕЛЬНАЯ СТАТИСТИКА
# ==============================================
cat("\n=== ОПИСАТЕЛЬНАЯ СТАТИСТИКА ===\n")

# Общая статистика
cat("Общая статистика по сухой массе побегов:\n")
cat("Среднее:", round(mean(rice_data$dry_weight), 2), "\n")
cat("Стандартное отклонение:", round(sd(rice_data$dry_weight), 2), "\n")
cat("Минимум:", min(rice_data$dry_weight), "\n")
cat("Максимум:", max(rice_data$dry_weight), "\n")
cat("Количество наблюдений:", nrow(rice_data), "\n")

# Проверяем уровни факторов
cat("\nУровни фактора variety (сорт):\n")
print(levels(rice_data$variety))
cat("Количество уровней:", length(levels(rice_data$variety)), "\n")

cat("\nУровни фактора fertilizer (удобрение):\n")
print(levels(rice_data$fertilizer))
cat("Количество уровней:", length(levels(rice_data$fertilizer)), "\n")

# Таблица наблюдений по группам
cat("\nКоличество наблюдений по группам:\n")
table_counts <- table(rice_data$variety, rice_data$fertilizer)
print(table_counts)

# Средние по группам
cat("\nСредние значения по группам:\n")
for (v in levels(rice_data$variety)) {
  for (f in levels(rice_data$fertilizer)) {
    subset_data <- rice_data$dry_weight[rice_data$variety == v & rice_data$fertilizer == f]
    cat(v, "×", f, ": среднее =", round(mean(subset_data), 2), 
        ", n =", length(subset_data), "\n")
  }
}

# 4. ВИЗУАЛИЗАЦИЯ ДАННЫХ
# ==============================================
cat("\n=== ВИЗУАЛИЗАЦИЯ ДАННЫХ ===\n")

# График 1: Boxplot по группам
rice_data$group <- interaction(rice_data$variety, rice_data$fertilizer, sep = " × ")

p1 <- ggplot(rice_data, aes(x = group, y = dry_weight, fill = group)) +
  geom_boxplot() +
  geom_point(position = position_jitter(width = 0.1), alpha = 0.5) +
  labs(title = "Распределение сухой массы побегов риса по группам",
       x = "Группа (Сорт × Удобрение)",
       y = "Сухая масса побегов (г)") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none")

print(p1)

# График 2: Средние значения с доверительными интервалами
summary_data <- rice_data %>%
  group_by(variety, fertilizer) %>%
  summarise(
    mean = mean(dry_weight),
    se = sd(dry_weight) / sqrt(n()),
    n = n(),
    .groups = 'drop'
  )

p2 <- ggplot(summary_data, aes(x = variety, y = mean, 
                               color = fertilizer, group = fertilizer)) +
  geom_line(position = position_dodge(width = 0.3)) +
  geom_point(size = 3, position = position_dodge(width = 0.3)) +
  geom_errorbar(aes(ymin = mean - 1.96*se, ymax = mean + 1.96*se),
                width = 0.2,
                position = position_dodge(width = 0.3)) +
  labs(title = "Средние значения сухой массы побегов с 95% доверительными интервалами",
       x = "Сорт риса",
       y = "Средняя сухая масса побегов (г)",
       color = "Удобрение") +
  theme_bw()

print(p2)

# 5. ДВУХФАКТОРНЫЙ ДИСПЕРСИОННЫЙ АНАЛИЗ
# ==============================================
cat("\n=== ДВУХФАКТОРНЫЙ ДИСПЕРСИОННЫЙ АНАЛИЗ ===\n")

# Создаем модель ANOVA
rice_model <- aov(dry_weight ~ variety * fertilizer, data = rice_data)

# Выводим результаты
cat("Результаты двухфакторного ANOVA:\n")
anova_table <- summary(rice_model)
print(anova_table)

# Извлекаем p-значения
p_variety <- anova_table[[1]]$"Pr(>F)"[1]
p_fertilizer <- anova_table[[1]]$"Pr(>F)"[2]
p_interaction <- anova_table[[1]]$"Pr(>F)"[3]

# 6. ИНТЕРПРЕТАЦИЯ РЕЗУЛЬТАТОВ
# ==============================================
cat("\n=== ИНТЕРПРЕТАЦИЯ РЕЗУЛЬТАТОВ ===\n")

alpha <- 0.05

cat("При уровне значимости α =", alpha, ":\n\n")

cat("1. Взаимодействие сорта и удобрения (variety:fertilizer):\n")
cat("   p-значение:", round(p_interaction, 4), "\n")
if (p_interaction < alpha) {
  cat("   Статистически значимо (p < 0.05) ★\n")
  cat("   Это означает, что эффект удобрения зависит от сорта риса.\n")
} else {
  cat("   Не значимо (p > 0.05)\n")
  cat("   Это означает, что эффект удобрения не зависит от сорта риса.\n")
}

cat("\n2. Главный эффект сорта (variety):\n")
cat("   p-значение:", round(p_variety, 4), "\n")
if (p_variety < alpha) {
  cat("   Статистически значимо (p < 0.05) ★\n")
  cat("   Сорт риса влияет на сухую массу побегов.\n")
} else {
  cat("   Не значимо (p > 0.05)\n")
  cat("   Сорт риса не влияет на сухую массу побегов.\n")
}

cat("\n3. Главный эффект удобрения (fertilizer):\n")
cat("   p-значение:", round(p_fertilizer, 4), "\n")
if (p_fertilizer < alpha) {
  cat("   Статистически значимо (p < 0.05) ★\n")
  cat("   Удобрение влияет на сухую массу побегов.\n")
} else {
  cat("   Не значимо (p > 0.05)\n")
  cat("   Удобрение не влияет на сухую массу побегов.\n")
}

# 7. ПРОВЕРКА УСЛОВИЙ ПРИМЕНИМОСТИ ANOVA
# ==============================================
cat("\n=== ПРОВЕРКА УСЛОВИЙ ПРИМЕНИМОСТИ ANOVA ===\n")

# Проверка нормальности остатков
shapiro_test <- shapiro.test(residuals(rice_model))
cat("\n1. Проверка нормальности остатков (тест Шапиро-Уилка):\n")
cat("   p-значение:", round(shapiro_test$p.value, 4), "\n")
if (shapiro_test$p.value > alpha) {
  cat("   Остатки распределены нормально (p > 0.05) ✓\n")
} else {
  cat("   Остатки НЕ распределены нормально (p < 0.05) ✗\n")
}

# Проверка гомогенности дисперсий
levene_test <- leveneTest(dry_weight ~ variety * fertilizer, data = rice_data)
cat("\n2. Проверка гомогенности дисперсий (тест Левена):\n")
cat("   p-значение:", round(levene_test$`Pr(>F)`[1], 4), "\n")
if (levene_test$`Pr(>F)`[1] > alpha) {
  cat("   Дисперсии однородны (p > 0.05) ✓\n")
} else {
  cat("   Дисперсии НЕ однородны (p < 0.05) ✗\n")
}

# Графическая проверка
par(mfrow = c(2, 2))
plot(rice_model)
par(mfrow = c(1, 1))

# 8. ПОСТ-ХОК ТЕСТЫ (если взаимодействие значимо)
# ==============================================
cat("\n=== ПОСТ-ХОК АНАЛИЗ ===\n")

if (p_interaction < alpha) {
  cat("Взаимодействие значимо, выполняем попарные сравнения:\n")
  
  # Используем тест Тьюки
  tukey_result <- TukeyHSD(rice_model, which = "variety:fertilizer")
  
  cat("\nРезультаты теста Тьюки для взаимодействия:\n")
  print(tukey_result)
  
  # Визуализация
  tukey_df <- as.data.frame(tukey_result[[1]])
  tukey_df$comparison <- rownames(tukey_df)
  
  # Отбираем только значимые сравнения
  sig_tukey <- tukey_df[tukey_df$`p adj` < alpha, ]
  
  if (nrow(sig_tukey) > 0) {
    cat("\nЗначимые попарные сравнения (p < 0.05):\n")
    print(sig_tukey[, c("comparison", "diff", "p adj")])
    
    # График
    p_tukey <- ggplot(sig_tukey, aes(x = reorder(comparison, diff), y = diff)) +
      geom_point() +
      geom_errorbar(aes(ymin = lwr, ymax = upr), width = 0.2) +
      geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
      coord_flip() +
      labs(title = "Значимые различия между группами",
           x = "Сравнение групп",
           y = "Разница средних значений (г)") +
      theme_bw()
    
    print(p_tukey)
  } else {
    cat("Нет значимых попарных различий между группами.\n")
  }
} else if (p_fertilizer < alpha) {
  cat("Взаимодействие не значимо, но эффект удобрения значим. Тест Тьюки для удобрения:\n")
  tukey_fert <- TukeyHSD(rice_model, which = "fertilizer")
  print(tukey_fert)
} else if (p_variety < alpha) {
  cat("Взаимодействие не значимо, но эффект сорта значим. Тест Тьюки для сорта:\n")
  tukey_var <- TukeyHSD(rice_model, which = "variety")
  print(tukey_var)
}

# 9. РАСЧЕТ РАЗМЕРОВ ЭФФЕКТА
# ==============================================
cat("\n=== РАЗМЕРЫ ЭФФЕКТА ===\n")

# Вычисляем η² (eta-squared)
ss_total <- sum(anova_table[[1]]$"Sum Sq")
eta_squared <- anova_table[[1]]$"Sum Sq" / ss_total

cat("η² (eta-squared):\n")
cat("variety:", round(eta_squared[1], 3), "(", round(eta_squared[1]*100, 1), "%)\n")
cat("fertilizer:", round(eta_squared[2], 3), "(", round(eta_squared[2]*100, 1), "%)\n")
cat("interaction:", round(eta_squared[3], 3), "(", round(eta_squared[3]*100, 1), "%)\n")
cat("error:", round(eta_squared[4], 3), "(", round(eta_squared[4]*100, 1), "%)\n")

# 10. ВЫВОДЫ И РЕКОМЕНДАЦИИ
# ==============================================
cat("\n=== ВЫВОДЫ И РЕКОМЕНДАЦИИ ===\n")

if (p_interaction < alpha) {
  cat("1. Обнаружено статистически значимое взаимодействие между сортом риса и удобрением.\n")
  cat("2. Эффект удобрений на сухую массу побегов зависит от сорта риса.\n")
  cat("3. Для разных сортов риса необходимо подбирать разные удобрения.\n")
  cat("4. Нельзя интерпретировать главные эффекты отдельно - нужно анализировать конкретные комбинации.\n")
} else if (p_variety < alpha || p_fertilizer < alpha) {
  cat("1. Взаимодействие не значимо, но есть значимые главные эффекты.\n")
  if (p_variety < alpha) {
    cat("2. Существуют статистически значимые различия между сортами риса.\n")
  }
  if (p_fertilizer < alpha) {
    cat("3. Удобрения оказывают статистически значимое влияние на сухую массу побегов.\n")
  }
  cat("4. Можно выбирать лучший сорт и лучшее удобрение независимо друг от друга.\n")
} else {
  cat("1. Ни один из эффектов не является статистически значимым.\n")
  cat("2. Возможно, требуется увеличение размера выборки или контроль других факторов.\n")
}

# 11. СОХРАНЕНИЕ РЕЗУЛЬТАТОВ
# ==============================================
cat("\n=== СОХРАНЕНИЕ РЕЗУЛЬТАТОВ ===\n")

# Сохраняем данные
write.csv(rice_data, "rice_analysis_final.csv", row.names = FALSE)
cat("Данные сохранены в файл: rice_analysis_final.csv\n")

# Сохраняем графики
ggsave("rice_boxplot_final.png", p1, width = 10, height = 6, dpi = 300)
ggsave("rice_means_final.png", p2, width = 8, height = 6, dpi = 300)

if (exists("p_tukey")) {
  ggsave("rice_tukey_final.png", p_tukey, width = 10, height = 6, dpi = 300)
  cat("График Тьюки сохранен в файл: rice_tukey_final.png\n")
}

cat("\n=== АНАЛИЗ УСПЕШНО ЗАВЕРШЕН ===\n")