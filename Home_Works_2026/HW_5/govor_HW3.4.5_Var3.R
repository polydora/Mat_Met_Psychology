# Задание 3 
# Активация библеотек 

library(dplyr)
library(ggplot2)

# Подготовка данных
data(mtcars)
mtcars$cyl <- as.factor(mtcars$cyl)  # Преобразование cyl в фактор

# Расчёт статистик 
stats <- mtcars %>%
  group_by(cyl) %>%
  summarise(
    min = min(mpg),
    max = max(mpg),
    median = median(mpg),
    q1 = quantile(mpg, 0.25),
    q3 = quantile(mpg, 0.75)
  )

# Результаты
print(stats)

# Часть 2 построение графика 
ggplot(mtcars, aes(x = cyl, y = mpg, fill = cyl)) +
  geom_boxplot(color = "black", fill = "gray70") +
  labs(x = "Категория", y = "Value") +
  theme_minimal() +
  theme(legend.position = "none")

## 4. Модель нрмального распределения, как основа для тестирования статистических гипотез

# Загрузка данных
data <- read.csv("dataset_variant_3.csv", sep = ";")

# Просмотр структуры данных
str(data)
summary(data)

# Проверка на нормальность распределения для каждой переменной

par(mfrow = c(2, 3))

# Создаем список для хранения результатов тестов
normality_results <- list()

for (var in names(data)) {
  # Тест Шапиро-Уилка
  shapiro_test <- shapiro.test(data[[var]])
  
  # Сохраняем результаты
  normality_results[[var]] <- list(
    statistic = shapiro_test$statistic,
    p.value = shapiro_test$p.value,
    is_normal = shapiro_test$p.value > 0.05
  )
  
  # Гистограмма с кривой нормального распределения
  hist(data[[var]], 
       main = paste(var, "\nShapiro-Wilk p =", round(shapiro_test$p.value, 4)),
       xlab = var,
       col = "lightblue",
       probability = TRUE)
  
  # Добавляем кривую нормального распределения
  curve(dnorm(x, mean = mean(data[[var]]), sd = sd(data[[var]])),
        add = TRUE, col = "red", lwd = 2)
}

# QQ-plot для каждой переменной
par(mfrow = c(2, 3))
for (var in names(data)) {
  qqnorm(data[[var]], main = paste("QQ-plot:", var))
  qqline(data[[var]], col = "red", lwd = 2)
}

# Вывод результатов
cat("\n=== РЕЗУЛЬТАТЫ ПРОВЕРКИ НА НОРМАЛЬНОСТЬ ===\n\n")

for (var in names(data)) {
  cat(sprintf("%s:\n", var))
  cat(sprintf("  W-статистика: %.4f\n", normality_results[[var]]$statistic))
  cat(sprintf("  p-value: %.4f\n", normality_results[[var]]$p.value))
  
  if (normality_results[[var]]$is_normal) {
    cat("  ВЫВОД: Распределение НОРМАЛЬНОЕ (p > 0.05)\n\n")
  } else {
    cat("  ВЫВОД: Распределение НЕ НОРМАЛЬНОЕ (p < 0.05)\n\n")
  }
}

# Загрузка встроенного датасета mtcars
data(mtcars)

# Преобразуем cyl в фактор (как требуется в задании)
mtcars$cyl <- as.factor(mtcars$cyl)

# Просмотр данных
cat("\n=== ЧАСТЬ 2: СТАТИСТИКИ mpg ПО ГРУППАМ cyl ===\n\n")
cat("Уровни фактора cyl:", levels(mtcars$cyl), "\n\n")

# Расчет статистик по группам
library(dplyr)

stats_part2 <- mtcars %>%
  group_by(cyl) %>%
  summarise(
    n = n(),
    mean_mpg = mean(mpg),
    sd_mpg = sd(mpg),
    .groups = 'drop'
  )

print(stats_part2)

##  Часть 3 

cat("\n=== ЧАСТЬ 3: 95% ДОВЕРИТЕЛЬНЫЕ ИНТЕРВАЛЫ ===\n\n")
# Функция для расчета доверительного интервала
calculate_ci <- function(x, conf_level = 0.95) {
  n <- length(x)
  mean_x <- mean(x)
  sd_x <- sd(x)
  
  # Стандартная ошибка: SE = sd / sqrt(n)
  se <- sd_x / sqrt(n)
  
  # Степени свободы: df = n - 1
  df <- n - 1
  
  # Критическое значение t
  alpha <- 1 - conf_level
  t_value <- qt(1 - alpha/2, df)
  
  # Доверительный интервал
  margin_error <- t_value * se
  ci_lower <- mean_x - margin_error
  ci_upper <- mean_x + margin_error
  
  # Возврат результатов
  return(list(
    n = n,
    mean = mean_x,
    sd = sd_x,
    se = se,
    df = df,
    t_value = t_value,
    ci_lower = ci_lower,
    ci_upper = ci_upper,
    margin_error = margin_error
  ))
}

# Сводная таблица
cat("\n=== СВОДНАЯ ТАБЛИЦА 95% ДОВЕРИТЕЛЬНЫХ ИНТЕРВАЛОВ ===\n\n")

summary_table <- data.frame(
  Цилиндры = cyl_levels,
  n = sapply(results, function(x) x$n),
  Среднее = sapply(results, function(x) round(x$mean, 3)),
  SE = sapply(results, function(x) round(x$se, 3)),
  t_value = sapply(results, function(x) round(x$t_value, 3)),
  Нижняя_граница = sapply(results, function(x) round(x$ci_lower, 3)),
  Верхняя_граница = sapply(results, function(x) round(x$ci_upper, 3))
)

print(summary_table, row.names = FALSE)

# Визуализация результатов
library(ggplot2)

# График доверительных интервалов
ci_plot_data <- data.frame(
  cyl = cyl_levels,
  mean = sapply(results, function(x) x$mean),
  ci_lower = sapply(results, function(x) x$ci_lower),
  ci_upper = sapply(results, function(x) x$ci_upper)
)

ggplot(ci_plot_data, aes(x = cyl, y = mean, ymin = ci_lower, ymax = ci_upper)) +
  geom_point(size = 3, color = "steelblue") +
  geom_errorbar(width = 0.2, color = "steelblue", size = 1) +
  labs(title = "95% Доверительные интервалы mpg по количеству цилиндров",
       x = "Количество цилиндров",
       y = "mpg (миль на галлон)") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, size = 14, face = "bold"))

# Boxplot для визуализации распределения
ggplot(mtcars, aes(x = cyl, y = mpg, fill = cyl)) +
  geom_boxplot(alpha = 0.7) +
  geom_jitter(width = 0.1, alpha = 0.5) +
  labs(title = "Распределение mpg по количеству цилиндров",
       x = "Количество цилиндров",
       y = "mpg") +
  theme_minimal() +
  theme(legend.position = "none",
        plot.title = element_text(hjust = 0.5, size = 14, face = "bold"))

cat("\n=== ИНТЕРПРЕТАЦИЯ РЕЗУЛЬТАТОВ ===\n")
cat("Мы можем быть на 95% уверены, что истинное среднее значение\n")
cat("расхода топлива (mpg) для автомобилей с данным количеством\n")
cat("цилиндров находится в указанном доверительном интервале.\n\n")

cat("Например:\n")
for (cyl_val in cyl_levels) {
  res <- results[[cyl_val]]
  cat(sprintf("- Для %s цилиндров: истинное среднее mpg находится в интервале\n", cyl_val))
  cat(sprintf("  от %.3f до %.3f миль на галлон\n", res$ci_lower, res$ci_upper))
}

# Посмотреть результаты нормальности
normality_results

# Посмотреть сводную таблицу
summary_table

## Домашнее задание №5
Домашнее задание по теме двухвыборочный t-критерий

# Домашнее задание №5 - Вариант 3 (компактная версия)

# Данные
elves <- c(174, 187, 185, 182, 183, 175, 187, 181, 180, 179, 189, 184, 183, 173, 189)
figures <- c(155, 151, 156, 147, 145, 143, 155, 152, 143, 148, 138, 141)

# Описательная статистика
cat("=== СТАТИСТИКА ===\n")
cat("Эльфы: n =", length(elves), ", mean =", round(mean(elves), 2), 
    ", sd =", round(sd(elves), 2), "\n")
cat("Фигуры: n =", length(figures), ", mean =", round(mean(figures), 2), 
    ", sd =", round(sd(figures), 2), "\n\n")

# Проверка нормальности
cat("=== НОРМАЛЬНОСТЬ (Шапиро-Уилк) ===\n")
cat("Эльфы: p =", round(shapiro.test(elves)$p.value, 4), "\n")
cat("Фигуры: p =", round(shapiro.test(figures)$p.value, 4), "\n\n")

# Проверка дисперсий
var_test <- var.test(elves, figures)
cat("=== РАВЕНСТВО ДИСПЕРСИЙ (F-тест) ===\n")
cat("p =", round(var_test$p.value, 4), "\n")
equal_var <- var_test$p.value > 0.05
cat("Дисперсии равны:", equal_var, "\n\n")

# T-критерий
t_test <- t.test(elves, figures, var.equal = equal_var)

cat("=== T-КРИТЕРИЙ СТЬЮДЕНТА ===\n")
cat("t =", round(t_test$statistic, 4), "\n")
cat("df =", round(t_test$parameter, 2), "\n")
cat("p-value =", t_test$p.value, "\n")
cat("95% CI: [", round(t_test$conf.int[1], 2), ";", 
    round(t_test$conf.int[2], 2), "]\n\n")

# Вывод
cat("=== ВЫВОД ===\n")
if (t_test$p.value < 0.05) {
  cat("H0 ОТВЕРГАЕМ (p < 0.05)\n")
  cat("Фигуры НЕ эльфы! Разница в росте:", round(mean(elves) - mean(figures), 2), "см\n")
} else {
  cat("H0 НЕ ОТВЕРГАЕМ (p >= 0.05)\n")
  cat("Фигуры могут быть эльфами\n")
}

# Визуализация
library(ggplot2)

df <- data.frame(
  Group = factor(c(rep("Эльфы", length(elves)), rep("Фигуры", length(figures)))),
  Height = c(elves, figures)
)

# График 1: Boxplot
ggplot(df, aes(x = Group, y = Height, fill = Group)) +
  geom_boxplot(alpha = 0.6) +
  geom_jitter(width = 0.15, alpha = 0.5) +
  labs(title = "Сравнение роста эльфов и встреченных фигур",
       x = "Группа", y = "Рост (см)") +
  theme_minimal() +
  theme(legend.position = "none", plot.title = element_text(hjust = 0.5))

# График 2: Столбчатая диаграмма с ДИ
summary_df <- df %>%
  group_by(Group) %>%
  summarise(mean = mean(Height), 
            se = sd(Height)/sqrt(n()),
            ci = qt(0.975, n()-1) * se, .groups = 'drop')

ggplot(summary_df, aes(x = Group, y = mean, fill = Group)) +
  geom_bar(stat = "identity", alpha = 0.7, width = 0.6) +
  geom_errorbar(aes(ymin = mean - ci, ymax = mean + ci), 
                width = 0.2, size = 0.8) +
  labs(title = "Средний рост с 95% доверительными интервалами",
       x = "Группа", y = "Рост (см)") +
  theme_minimal() +
  theme(legend.position = "none", plot.title = element_text(hjust = 0.5))



