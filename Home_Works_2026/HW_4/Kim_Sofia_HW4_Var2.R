# Домашнее задание № 4
# Часть 1.

# Скачайте датасет, соответствующий вашему варианту. Это датафрейм, состоящий из пяти переменных. Найдите, какие из переменных подчиняются нормальному распределению. Ваш анализ должен быть отражен в скрипте. Ответ о том, подчиняется ли та или иная переменная нормальному распределению, приведите в виде комментария в скрипте. В этом же комментарии постарайтесь оценить, что именно не соответствует модели нормального распределения.

setwd("D:/Text/MatMet_MIP_2026/Data")



library(ggplot2)     


df <- read.csv2("dataset_variant_2.csv", header = TRUE, sep = ";", dec = ".")
df <- as.data.frame(lapply(df, function(x) as.numeric(as.character(x))))

# Функции для расчета статистик
skewness <- function(x) {
  x <- x[!is.na(x)]
  n <- length(x)
  if(n < 3) return(NA)
  (sum((x - mean(x))^3) / n) / (sd(x)^3)
}

kurtosis <- function(x) {
  x <- x[!is.na(x)]
  n <- length(x)
  if(n < 4) return(NA)
  (sum((x - mean(x))^4) / n) / (sd(x)^4) - 3
}


  
  # Гистограмма
  hist(x, main = paste(var_name, "- Гистограмма"), 
       xlab = var_name, ylab = "Плотность", 
       col = "lightblue", freq = FALSE, breaks = 15)
  curve(dnorm(x, mean(x), sd(x)), add = TRUE, col = "red", lwd = 2)
  
  # Q-Q график
  qqnorm(x, main = paste(var_name, "- Q-Q график"), 
         xlab = "Теоретические квантили", ylab = "Выборочные квантили",
         col = "blue", pch = 19)
  qqline(x, col = "red", lwd = 2)



# список с результатами анализа
results <- list()

for(i in 1:ncol(df)) {
  var_name <- names(df)[i]
  x <- df[[i]][!is.na(df[[i]])]
  
  # Соборка результатов в список
  results[[var_name]] <- list(
    mean = mean(x),
    median = median(x),
    sd = sd(x),
    skewness = skewness(x),
    kurtosis = kurtosis(x),
    shapiro = if(length(x) <= 5000) shapiro.test(x)$p.value else NA,
    n = length(x)
  )
}

# Финальный список с результатами или вывод
final_summary <- list(
  data_structure = str(df),
  summary_stats = summary(df),
  normality_results = results,
  conclusion = "Только Var2 подчиняется нормальному распределению Остальные переменные имеют отклонения: Var1 (бимодальное), Var3 (положительная асимметрия), Var4 (высокий эксцесс), Var5 (бимодальное с отрицательной асимметрией)."
)


# Часть 2.

# Используя датасеты перечисленные ниже, вычислите выборочные средние и среднеквадратичные отклонения для каждой из групп категорий, которые представлены в этих датасетах.

# Вариант 2. Вес цыплят weight на 21-й день наблюдения (переменная Time) для каждого из четырех типов диет (Diet) из датасета ChickWeight.

# Часть 2. Анализ веса цыплят на 21-й день для разных типов диет
# На основе лекционных материалов по описательной статистике

# Загружаем датасет
data("ChickWeight")

# Фильтруем данные только для 21-го дня
chicks_day21 <- ChickWeight[ChickWeight$Time == 21, ]

# Разделяем данные по типам диет
diet1 <- chicks_day21[chicks_day21$Diet == 1, "weight"]
diet2 <- chicks_day21[chicks_day21$Diet == 2, "weight"]
diet3 <- chicks_day21[chicks_day21$Diet == 3, "weight"]
diet4 <- chicks_day21[chicks_day21$Diet == 4, "weight"]

# Функция для вычисления описательных статистик (как в лекции)
calc_stats <- function(x, diet_name) {
  data.frame(
    Diet = diet_name,
    Count = length(x),
    Mean = mean(x),
    SD = sd(x),
    Min = min(x),
    Q1 = quantile(x, 0.25),
    Median = median(x),
    Q3 = quantile(x, 0.75),
    Max = max(x),
    IQR = IQR(x)
  )
}

# Вычисляем статистики для каждой диеты
stats_diet1 <- calc_stats(diet1, "Диета 1")
stats_diet2 <- calc_stats(diet2, "Диета 2")
stats_diet3 <- calc_stats(diet3, "Диета 3")
stats_diet4 <- calc_stats(diet4, "Диета 4")

# Объединяем все результаты
all_stats <- rbind(stats_diet1, stats_diet2, stats_diet3, stats_diet4)

# Сортируем по среднему значению
all_stats <- all_stats[order(-all_stats$Mean), ]

# Создаем объект с результатами (как в лекции)
results <- list(
  # Основные выборочные средние и среднеквадратичные отклонения
  sample_statistics = all_stats[, c("Diet", "Count", "Mean", "SD")],
  
  # Полные описательные статистики
  descriptive_stats = all_stats,
  
  # Детальные данные по каждой диете
  diet_data = list(
    diet1 = diet1,
    diet2 = diet2,
    diet3 = diet3,
    diet4 = diet4
  ),
  
  # Сравнение среднего и медианы (для оценки нормальности)
  mean_vs_median = data.frame(
    Diet = all_stats$Diet,
    Mean = all_stats$Mean,
    Median = all_stats$Median,
    Difference = abs(all_stats$Mean - all_stats$Median)
  ),
  
  # Правило 68-95-99.7 для каждой диеты
  empirical_rule = list(
    diet1 = c(
      within_1sd = mean(diet1 > mean(diet1) - sd(diet1) & diet1 < mean(diet1) + sd(diet1)) * 100,
      within_2sd = mean(diet1 > mean(diet1) - 2*sd(diet1) & diet1 < mean(diet1) + 2*sd(diet1)) * 100,
      within_3sd = mean(diet1 > mean(diet1) - 3*sd(diet1) & diet1 < mean(diet1) + 3*sd(diet1)) * 100
    ),
    diet2 = c(
      within_1sd = mean(diet2 > mean(diet2) - sd(diet2) & diet2 < mean(diet2) + sd(diet2)) * 100,
      within_2sd = mean(diet2 > mean(diet2) - 2*sd(diet2) & diet2 < mean(diet2) + 2*sd(diet2)) * 100,
      within_3sd = mean(diet2 > mean(diet2) - 3*sd(diet2) & diet2 < mean(diet2) + 3*sd(diet2)) * 100
    ),
    diet3 = c(
      within_1sd = mean(diet3 > mean(diet3) - sd(diet3) & diet3 < mean(diet3) + sd(diet3)) * 100,
      within_2sd = mean(diet3 > mean(diet3) - 2*sd(diet3) & diet3 < mean(diet3) + 2*sd(diet3)) * 100,
      within_3sd = mean(diet3 > mean(diet3) - 3*sd(diet3) & diet3 < mean(diet3) + 3*sd(diet3)) * 100
    ),
    diet4 = c(
      within_1sd = mean(diet4 > mean(diet4) - sd(diet4) & diet4 < mean(diet4) + sd(diet4)) * 100,
      within_2sd = mean(diet4 > mean(diet4) - 2*sd(diet4) & diet4 < mean(diet4) + 2*sd(diet4)) * 100,
      within_3sd = mean(diet4 > mean(diet4) - 3*sd(diet4) & diet4 < mean(diet4) + 3*sd(diet4)) * 100
    )
  ),
  
  # Z-оценки (стандартизация) для каждой диеты
  z_scores = list(
    diet1 = scale(diet1),
    diet2 = scale(diet2),
    diet3 = scale(diet3),
    diet4 = scale(diet4)
  )
)

# Добавляем итоговое заключение
results$conclusion <- list(
  summary = "На 21-й день наблюдения:",
  best_diet = paste("Наибольший средний вес у диеты", 
                    all_stats[1, "Diet"], 
                    ":", 
                    round(all_stats[1, "Mean"], 1), 
                    "г"),
  worst_diet = paste("Наименьший средний вес у диеты", 
                     all_stats[4, "Diet"], 
                     ":", 
                     round(all_stats[4, "Mean"], 1), 
                     "г"),
  most_variable = paste("Наибольшая вариабельность (SD =", 
                        round(all_stats[which.max(all_stats$SD), "SD"], 1), 
                        ") у", 
                        all_stats[which.max(all_stats$SD), "Diet"]),
  least_variable = paste("Наименьшая вариабельность (SD =", 
                         round(all_stats[which.min(all_stats$SD), "SD"], 1), 
                         ") у", 
                         all_stats[which.min(all_stats$SD), "Diet"])
)


# Только средние и среднеквадратичные
results$sample_statistics

# полная таблица со всеми статистиками
results$descriptive_stats


#Часть 3.

# Используя датасеты Части 2 (для своего варианта), определите границы 95% доверительного интервала для каждой из категорий.

# Доверительный интервал вычислется как x¯±t⋅SE

# Величину t
# (это 2.5% или 97.5% квантили t-распределиния Стьюдента) пока определяем по таблице. НО! Если можете определить квантили t-распределения с использованием функции qt() то будет +10%.

# В этой таблице: df=n−1, где n - объем выборки. Берем столбец для p = 0.05 (доверительная вероятность: P = 1 - p = 0.95). Зная n, находим величину t.

# Кроме того, вам нужно вычислить величину стандартной ошибки, которая вычисляется по следующей формуле: SE=Sd/√n, Sd - стандартное отклонение.


# Вариант 2: вес цыплят (weight) на 21-й день для каждого из четырех типов диет (Diet)


data("ChickWeight")

# Фильтруем данные только для 21-го дня
chicks_day21 <- ChickWeight[ChickWeight$Time == 21, ]

# Разделяем данные по типам диет
diet1 <- chicks_day21[chicks_day21$Diet == 1, "weight"]
diet2 <- chicks_day21[chicks_day21$Diet == 2, "weight"]
diet3 <- chicks_day21[chicks_day21$Diet == 3, "weight"]
diet4 <- chicks_day21[chicks_day21$Diet == 4, "weight"]

# Функция для вычисления доверительного интервала
calc_ci <- function(x) {
  n <- length(x)                    # объем выборки
  mean_x <- mean(x)                  # выборочное среднее (x̄)
  sd_x <- sd(x)                      # стандартное отклонение (Sd)
  
  # Вычисляем стандартную ошибку по формуле: SE = Sd/√n
  se_x <- sd_x / sqrt(n)
  
  # Квантиль t-распределения для 95% доверительного интервала
  t_val <- qt(0.975, df = n - 1)
  
  # Границы доверительного интервала: x̄ ± t·SE
  ci_lower <- mean_x - t_val * se_x
  ci_upper <- mean_x + t_val * se_x
  
  # Возвращаем все вычисленные значения
  list(
    n = n,
    mean = mean_x,
    sd = sd_x,
    se = se_x,
    t = t_val,
    ci_lower = ci_lower,
    ci_upper = ci_upper
  )
}

# Вычисляем доверительные интервалы для каждой диеты
ci1 <- calc_ci(diet1)
ci2 <- calc_ci(diet2)
ci3 <- calc_ci(diet3)
ci4 <- calc_ci(diet4)

# Создаем таблицу с результатами
ci_results <- data.frame(
  Diet = c("Диета 1", "Диета 2", "Диета 3", "Диета 4"),
  n = c(ci1$n, ci2$n, ci3$n, ci4$n),
  Mean = round(c(ci1$mean, ci2$mean, ci3$mean, ci4$mean), 2),
  Sd = round(c(ci1$sd, ci2$sd, ci3$sd, ci4$sd), 2),
  SE = round(c(ci1$se, ci2$se, ci3$se, ci4$se), 2),
  t = round(c(ci1$t, ci2$t, ci3$t, ci4$t), 3),
  CI_lower = round(c(ci1$ci_lower, ci2$ci_lower, ci3$ci_lower, ci4$ci_lower), 2),
  CI_upper = round(c(ci1$ci_upper, ci2$ci_upper, ci3$ci_upper, ci4$ci_upper), 2)
)

ci_results[, c("Diet", "CI_lower", "CI_upper")]
