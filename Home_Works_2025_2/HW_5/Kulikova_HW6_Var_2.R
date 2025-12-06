# Загрузка необходимых пакетов
library(readxl)
library(ggplot2)
library(car)
library(multcomp)

# 1. ЗАГРУЗКА И ПОДГОТОВКА ДАННЫХ

# Читаем данные
data <- read_excel("Data/hydrology_2022.xls")

# Проверяем структуру
print("Структура данных:")
print(colnames(data))

# Подготовка данных
data$Air_T <- as.numeric(gsub(",", ".", data$Air_T))

# Фильтруем летние месяцы и удаляем NA
summer_data <- data[data$Month %in% c("June", "July", "August") & !is.na(data$Air_T), 
                    c("Month", "Air_T")]

# Преобразуем месяц в фактор
summer_data$Month <- factor(summer_data$Month, levels = c("June", "July", "August"))

print(paste("Наблюдений для анализа:", nrow(summer_data)))

# 2. ОПИСАТЕЛЬНАЯ СТАТИСТИКА
cat("ОПИСАТЕЛЬНЫЕ СТАТИСТИКИ:\n")
stats <- aggregate(Air_T ~ Month, data = summer_data, 
                   FUN = function(x) c(n = length(x), mean = mean(x), sd = sd(x)))
print(data.frame(Month = stats$Month, stats$Air_T))

# 3. ПРОВЕРКА УСЛОВИЙ ANOVA

# Построение модели
model <- aov(Air_T ~ Month, data = summer_data)

# Проверка нормальности остатков
shapiro_test <- shapiro.test(resid(model))
cat("\nПРОВЕРКА УСЛОВИЙ ANOVA:\n")
cat("Нормальность остатков (Шапиро-Уилк): p =", 
    ifelse(shapiro_test$p.value < 0.001, "< 0.001", round(shapiro_test$p.value, 4)), "\n")

# Проверка однородности дисперсий
levene_test <- leveneTest(Air_T ~ Month, data = summer_data)
cat("Однородность дисперсий (Левене): p =", 
    ifelse(levene_test$'Pr(>F)'[1] < 0.001, "< 0.001", round(levene_test$'Pr(>F)'[1], 4)), "\n")

# Визуальная диагностика остатков
par(mfrow = c(2, 2))
plot(model, which = 1:4)
par(mfrow = c(1, 1))

# 4. ДИСПЕРСИОННЫЙ АНАЛИЗ
cat("\nДИСПЕРСИОННЫЙ АНАЛИЗ (ANOVA):\n")
anova_result <- summary(model)
print(anova_result)

p_value <- anova_result[[1]]$'Pr(>F)'[1]

# 5. POST-HOC АНАЛИЗ (если ANOVA значим)
if (!is.na(p_value) && p_value < 0.05) {
  cat("\nPOST-HOC АНАЛИЗ (Тьюки):\n")
  tukey_result <- TukeyHSD(model)
  print(tukey_result)
  tukey_df <- as.data.frame(tukey_result$Month)
} else {
  tukey_df <- NULL
  cat("\nPOST-HOC анализ не требуется (ANOVA не значим)\n")
}

# 6. ВИЗУАЛИЗАЦИЯ

# Подготовка данных для графиков
plot_stats <- do.call(data.frame, aggregate(Air_T ~ Month, data = summer_data,
                                            FUN = function(x) c(mean = mean(x),
                                                                n = length(x),
                                                                sd = sd(x),
                                                                se = sd(x)/sqrt(length(x)))))

plot_stats$ci_lower <- plot_stats$Air_T.mean - 1.96 * plot_stats$Air_T.se
plot_stats$ci_upper <- plot_stats$Air_T.mean + 1.96 * plot_stats$Air_T.se

# ГРАФИК 1: Столбчатая диаграмма с доверительными интервалами и точками данных
p1 <- ggplot(plot_stats, aes(x = Month, y = Air_T.mean)) +
  geom_col(fill = "lightblue", alpha = 0.7, width = 0.6) +
  geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper), 
                width = 0.2, color = "darkblue", linewidth = 0.7) +
  geom_jitter(data = summer_data, aes(x = Month, y = Air_T), 
              width = 0.2, alpha = 0.4, color = "red", size = 1.5) +
  labs(title = "Средняя температура воздуха по летним месяцам в Кандалакшском заповеднике",
       subtitle = "Столбцы - средние значения, ошибки - 95% доверительные интервалы\nКрасные точки - индивидуальные измерения",
       x = "Месяц", 
       y = "Температура воздуха (°C)") +
  theme_minimal(base_size = 12) +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5),
        panel.grid.major = element_line(color = "grey90"),
        panel.grid.minor = element_blank())

# Добавляем обозначения значимых различий
if (!is.null(tukey_df)) {
  y_max <- max(plot_stats$ci_upper)
  y_step <- (max(plot_stats$Air_T.mean) - min(plot_stats$Air_T.mean)) * 0.2
  
  comparisons <- list()
  
  if ("July-June" %in% rownames(tukey_df) && tukey_df["July-June", "p adj"] < 0.05) {
    comparisons <- c(comparisons, list(c(1, 2)))
  }
  if ("August-June" %in% rownames(tukey_df) && tukey_df["August-June", "p adj"] < 0.05) {
    comparisons <- c(comparisons, list(c(1, 3)))
  }
  if ("August-July" %in% rownames(tukey_df) && tukey_df["August-July", "p adj"] < 0.05) {
    comparisons <- c(comparisons, list(c(2, 3)))
  }
  
  # Добавляем линии и звездочки
  for (i in seq_along(comparisons)) {
    comp <- comparisons[[i]]
    y_pos <- y_max + i * y_step
    
    p1 <- p1 + 
      annotate("segment", 
               x = comp[1], xend = comp[2],
               y = y_pos, yend = y_pos,
               color = "black", linewidth = 0.8) +
      annotate("text", 
               x = mean(comp), y = y_pos + y_step/3,
               label = ifelse(tukey_df[ifelse(i==1, "July-June", 
                                              ifelse(i==2, "August-June", "August-July")), "p adj"] < 0.001, "***",
                              ifelse(tukey_df[ifelse(i==1, "July-June", 
                                                     ifelse(i==2, "August-June", "August-July")), "p adj"] < 0.01, "**",
                                     ifelse(tukey_df[ifelse(i==1, "July-June", 
                                                            ifelse(i==2, "August-June", "August-July")), "p adj"] < 0.05, "*", ""))),
               size = 5, vjust = 0)
  }
}

print(p1)

# ГРАФИК 2: Boxplot с точками данных
p2 <- ggplot(summer_data, aes(x = Month, y = Air_T, fill = Month)) +
  geom_boxplot(alpha = 0.7, outlier.shape = NA) +
  geom_jitter(width = 0.2, alpha = 0.5, size = 1.2) +
  stat_summary(fun = mean, geom = "point", shape = 18, size = 3, color = "black") +
  scale_fill_brewer(palette = "Pastel1") +
  labs(title = "Распределение температуры воздуха по летним месяцам",
       subtitle = "Черные ромбики - средние значения, точки - индивидуальные измерения",
       x = "Месяц", 
       y = "Температура воздуха (°C)") +
  theme_minimal(base_size = 12) +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5),
        legend.position = "none")

print(p2)

# 7. ИТОГОВЫЕ ВЫВОДЫ
cat("\n", rep("=", 60), "\n", sep = "")
cat("ИТОГОВЫЕ ВЫВОДЫ:\n")
cat(rep("=", 60), "\n", sep = "")

if (!is.na(p_value) && p_value < 0.05) {
  cat("✓ Обнаружены статистически значимые различия в температуре\n  между летними месяцами (p", 
      ifelse(p_value < 0.001, "< 0.001", paste("=", round(p_value, 4))), ")\n")
  
  if (!is.null(tukey_df)) {
    cat("\nЗначимые попарные различия (критерий Тьюки):\n")
    for (i in 1:nrow(tukey_df)) {
      comp <- rownames(tukey_df)[i]
      p_val <- tukey_df[i, "p adj"]
      if (p_val < 0.05) {
        significance <- ifelse(p_val < 0.001, "***", 
                               ifelse(p_val < 0.01, "**", "*"))
        cat("  -", comp, ": p =", 
            ifelse(p_val < 0.001, "< 0.001", sprintf("%.4f", p_val)), significance, "\n")
      }
    }
  }
} else {
  cat("✗ Статистически значимых различий в температуре\n  между летними месяцами не обнаружено\n")
}

cat("\nПроверка условий применимости ANOVA:\n")
cat("- Нормальность остатков: p =", 
    ifelse(shapiro_test$p.value < 0.001, "< 0.001", round(shapiro_test$p.value, 4)),
    ifelse(shapiro_test$p.value > 0.05, "✓ выполнено", "✗ нарушено"), "\n")
cat("- Однородность дисперсий: p =", 
    ifelse(levene_test$'Pr(>F)'[1] < 0.001, "< 0.001", round(levene_test$'Pr(>F)'[1], 4)),
    ifelse(levene_test$'Pr(>F)'[1] > 0.05, "✓ выполнено", "✗ нарушено"), "\n")

cat(rep("=", 60), "\n", sep = "")
