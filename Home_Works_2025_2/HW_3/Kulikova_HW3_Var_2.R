# Загрузка датасета

data(ChickWeight)

# Просмотр структуры данных ChickWeight
head(ChickWeight)
str(ChickWeight)

# Фильтрация данных для 21-го дня наблюдения
chick_21_day <- ChickWeight[ChickWeight$Time == 21, ]

# Вычисление описательных статистик для каждого типа диеты
result <- aggregate(weight ~ Diet, 
                    data = chick_21_day, 
                    FUN = function(x) {
                      c(Min = min(x),
                        Max = max(x),
                        Q1 = quantile(x, 0.25),
                        Median = median(x),
                        Q3 = quantile(x, 0.75))
                    })

# Вывод результатов
print("Описательные статистики веса цыплят на 21-й день для каждого типа диеты:")
print(result)

cat("\n--- ОПИСАТЕЛЬНЫЕ СТАТИСТИКИ ВЕСА ЦЫПЛЯТ НА 21-Й ДЕНЬ ---\n")
for(i in 1:nrow(result)) {
  cat("\nДиета", result$Diet[i], ":\n")
  stats <- result$weight[i,]
  cat("  Минимум:   ", round(stats["Min"], 2), "\n")
  cat("  1-й квартиль:", round(stats["Q1.25%"], 2), "\n")
  cat("  Медиана:    ", round(stats["Median"], 2), "\n")
  cat("  3-й квартиль:", round(stats["Q3.75%"], 2), "\n")
  cat("  Максимум:   ", round(stats["Max"], 2), "\n")
}

# Визуализация
boxplot(weight ~ Diet, data = chick_21_day,
        main = "Вес цыплят на 21-й день по типам диет",
        xlab = "Тип диеты", ylab = "Вес (г)")



# Часть 3: Проверка на нормальное распределение

# Загрузка данных
data <- read.csv("dataset_variant_2.csv")

# Просмотр структуры данных
str(data)
head(data)

# Функция для проверки нормальности с визуализацией 
check_normality <- function(variable, var_name) {
  # Проверяем, является ли переменная числовой
  if (!is.numeric(variable)) {
    cat("Переменная:", var_name, "- не числовая, пропускаем проверку нормальности\n")
    cat("Тип переменной:", class(variable), "\n")
    cat("---\n")
    return()
  }
  
  # Удаляем NA значения
  variable_clean <- na.omit(variable)
  
  # Проверяем, достаточно ли данных
  if (length(variable_clean) < 3) {
    cat("Переменная:", var_name, "- недостаточно данных для анализа\n")
    cat("---\n")
    return()
  }
  
  # Гистограмма с нормальной кривой
  hist(variable_clean, prob = TRUE, main = paste("Гистограмма для", var_name), 
       xlab = var_name, col = "lightblue")
  curve(dnorm(x, mean = mean(variable_clean), sd = sd(variable_clean)), 
        add = TRUE, col = "red", lwd = 2)
  
  # Q-Q plot
  qqnorm(variable_clean, main = paste("Q-Q plot для", var_name))
  qqline(variable_clean, col = "red")
  
  # Тест Шапиро-Уилка (только для выборок от 3 до 5000 элементов)
  if (length(variable_clean) >= 3 && length(variable_clean) <= 5000) {
    shapiro_test <- shapiro.test(variable_clean)
    p_value <- shapiro_test$p.value
  } else {
    p_value <- NA
    cat("Переменная:", var_name, "- размер выборки не подходит для теста Шапиро-Уилка\n")
  }
  
  # Вывод результатов
  cat("Переменная:", var_name, "\n")
  if (!is.na(p_value)) {
    cat("p-value теста Шапиро-Уилка:", p_value, "\n")
    
    # Комментарий о нормальности распределения
    if (p_value > 0.05) {
      cat("КОММЕНТАРИЙ: Переменная", var_name, "подчиняется нормальному распределению (p > 0.05)\n")
    } else {
      cat("КОММЕНТАРИЙ: Переменная", var_name, "НЕ подчиняется нормальному распределению (p < 0.05)\n")
      cat("Отклонения от нормальности могут быть связаны с асимметрией, тяжелыми хвостами или выбросами\n")
    }
  }
  cat("Объем выборки:", length(variable_clean), "\n")
  cat("Среднее:", mean(variable_clean), "\n")
  cat("Стандартное отклонение:", sd(variable_clean), "\n")
  cat("---\n")
}

# Проверка всех переменных на нормальность (только числовых)
cat("=== ЧАСТЬ 3: Анализ нормальности распределения ===\n")
for (i in 1:ncol(data)) {
  var_name <- names(data)[i]
  variable <- data[[i]]
  
  # Проверяем тип переменной
  if (is.numeric(variable)) {
    check_normality(variable, var_name)
  } else {
    cat("Переменная:", var_name, "- тип", class(variable), ", пропускаем\n")
    cat("---\n")
  }
}

# Часть 4: Вычисление выборочных средних и стандартных отклонений 

cat("\n=== ЧАСТЬ 4: Описательная статистика ===\n")

# Загрузка датасета ChickWeight
data(ChickWeight)

# Фильтрация данных для 21-го дня наблюдения
chick_day21 <- ChickWeight[ChickWeight$Time == 21, ]

# Вычисление средних и стандартных отклонений для каждой диеты
results_part4 <- aggregate(weight ~ Diet, data = chick_day21, 
                           FUN = function(x) c(mean = mean(x), 
                                               sd = sd(x), n = length(x)))

# Преобразование результатов в удобный формат 
results_part4_df <- data.frame(
  Diet = results_part4$Diet,
  Mean = results_part4$weight[, "mean"],
  SD = results_part4$weight[, "sd"],
  N = results_part4$weight[, "n"]
)

print("Часть 4: Выборочные средние и стандартные отклонения")
print(results_part4_df)

# Визуализация
library(ggplot2)
ggplot(chick_day21, aes(x = factor(Diet), y = weight, fill = factor(Diet))) +
  geom_boxplot() +
  geom_point(position = position_jitter(width = 0.2), alpha = 0.6) +
  labs(title = "Вес цыплят на 21-й день по типам диет",
       x = "Тип диеты", y = "Вес") +
  theme_minimal()

# Часть 5: 95% доверительные интервалы

# Функция для вычисления доверительного интервала
calculate_ci <- function(mean_val, sd_val, n, confidence = 0.95) {
  # Проверяем валидность входных данных
  if (is.na(mean_val) ||
      is.na(sd_val) || n < 2) {
    return(c(lower = NA, upper = NA, SE = NA, t = NA))
  }
  
  # Стандартная ошибка
  SE <- sd_val / sqrt(n)
  
  # Квантиль t-распределения
  t_value <- qt(1 - (1 - confidence)/2, df = n - 1)
  
  # Границы доверительного интервала
  lower <- mean_val - t_value * SE
  upper <- mean_val + t_value * SE
  
  return(c(lower = lower, upper = upper, SE = SE, t = t_value))
}

# Вычисление доверительных интервалов для каждой диеты 
ci_results <- t(apply(results_part4_df, 1, function(row) {
  calculate_ci(as.numeric(row["Mean"]), as.numeric(row["SD"]), as.numeric(row["N"]))
}))

# Объединение результатов
final_results <- cbind(results_part4_df, ci_results)

print("Часть 5: 95% доверительные интервалы")
print(final_results)

# Визуализация доверительных интервалов
ggplot(final_results, aes(x = factor(Diet), y = Mean)) +
  geom_point(size = 3, color = "blue") +
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.2, color = "red") +
  labs(title = "95% доверительные интервалы для среднего веса по типам диет",
       x = "Тип диеты", y = "Средний вес") +
  theme_minimal() +
  geom_text(aes(label = paste0(round(Mean, 1), "\n(", round(lower, 1), "-", round(upper, 1), ")")), 
            vjust = -1.5, size = 3)

# Дополнительная таблица с подробными результатами
detailed_table <- final_results[, c("Diet", "Mean", "SD", "N", "SE", "t", "lower", "upper")]
print("Детальная таблица результатов:")
print(detailed_table, row.names = FALSE)