###############################################
# Домашнее задание по Математическим методам в биологии
# Вариант 3
# Автор: Ковалева Александра
###############################################

# Часть 1: Описательные статистики для mtcars
# Загружаем датасет mtcars
data(mtcars)

# Преобразуем cyl в фактор для корректной группировки по числу цилиндров
mtcars$cyl <- as.factor(mtcars$cyl)

# Проверяем структуру данных
str(mtcars)

# Выводим основные описательные статистики для mpg по группам цилиндров
library(dplyr)

stat_summary <- mtcars %>%
  group_by(cyl) %>%
  summarise(
    Минимум = min(mpg),
    `1-й квартиль` = quantile(mpg, 0.25),
    Медиана = median(mpg),
    `3-й квартиль` = quantile(mpg, 0.75),
    Максимум = max(mpg))

print(stat_summary)

# Часть 2: Построение boxplot для визуализации разброса mpg по цилиндрам
library(ggplot2)

ggplot(mtcars, aes(x = cyl, y = mpg)) +
  geom_boxplot(fill = "grey", color = "black", alpha = 0.8) +
  labs(
    title = "Boxplot расхода топлива (mpg) по числу цилиндров",
    x = "Количество цилиндров (категория)",
    y = "Расход топлива (mpg)") +
  theme_minimal(base_size = 14) +
  theme(legend.position = "none")


# Часть 3: Проверка нормальности распределения внешнего датасета
library(ggplot2)
library(rlang)  # для sym()

for (col in names(data)) {
  if (is.numeric(data[[col]])) {
    cat("\n--- Переменная:", col, "---\n")
    
    # Тест Шапиро–Уилка
    test <- shapiro.test(data[[col]])
    print(test)
    
    # Визуализация распределения с современным синтаксисом
    p <- ggplot(data, aes(x = !!sym(col))) +
      geom_histogram(aes(y = ..density..), bins = 15, fill = "grey", color = "black") +
      geom_density(color = "blue", linewidth = 1) +
      ggtitle(paste("Гистограмма и плотность для переменной:", col))
    
    print(p)
  }
}

###############################################################
# Интерпретация теста Шапиро–Уилка:
# p-value > 0.05 → распределение не отличается от нормального
# p-value < 0.05 → распределение отличается от нормального
###############################################################

# Часть 4: Выборочные средние и стандартные отклонения по группам цилиндров
# Среднее — оценка истинного среднего mpg для группы
# SD — измеряет разброс значений внутри группы

mean_by_cyl <- tapply(mtcars$mpg, mtcars$cyl, mean)
sd_by_cyl <- tapply(mtcars$mpg, mtcars$cyl, sd)

print(mean_by_cyl)
print(sd_by_cyl)

# Часть 5: Построение 95% доверительного интервала

n_by_cyl <- tapply(mtcars$mpg, mtcars$cyl, length)  # размер группы
SE_by_cyl <- sd_by_cyl / sqrt(n_by_cyl)             # стандартная ошибка
t_by_cyl <- qt(0.975, df = n_by_cyl - 1)            # t-квантиль для 95% CI

lower_CI <- mean_by_cyl - t_by_cyl * SE_by_cyl
upper_CI <- mean_by_cyl + t_by_cyl * SE_by_cyl

# Объединяем результаты в таблицу
ci_table <- data.frame(
  cyl = names(mean_by_cyl),
  mean_mpg = mean_by_cyl,
  sd_mpg = sd_by_cyl,
  n = n_by_cyl,
  SE = SE_by_cyl,
  lower_CI = lower_CI,
  upper_CI = upper_CI
)

# Выводим таблицу доверительных интервалов
print(ci_table)

