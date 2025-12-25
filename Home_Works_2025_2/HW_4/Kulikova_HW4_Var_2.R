# Анализ роста эльфов и замеченных фигур в Средиземье

# Данные роста эльфов из вашего отряда (см)
elves <- c(179, 183, 173, 180, 174, 184, 180, 180, 185, 183, 171, 177, 179, 187, 176)

# Данные роста замеченных фигур (см)
figures <- c(195, 184, 186, 194, 191, 194)

# Проведем визуальный анализ данных
# Создаем объединенный набор данных для визуализации
group <- c(rep("Эльфы", length(elves)), rep("Фигуры", length(figures)))
height <- c(elves, figures)
data <- data.frame(group, height)

# Построим боксилоты для визуального сравнения распределений
boxplot(height ~ group, data = data, 
        main = "Сравнение роста эльфов и замеченных фигур",
        ylab = "Рост (см)", xlab = "Группа",
        col = c("lightgreen", "lightcoral"))

# Добавим точки для отображения отдельных наблюдений
points(jitter(rep(1, 
      length(elves))), elves, pch = 16,
      col = "darkgreen", cex = 0.8)
      points(jitter(rep(2,
      length(figures))), figures, pch = 16, 
      col = "darkred", cex = 0.8)
       
       # Проведем формальный статистический анализ
       # Сформулируем гипотезы:
       # H0: μ_фигуры = μ_эльфы (рост фигур не отличается от роста эльфов)
       # H1: μ_фигуры ≠ μ_эльфы (рост фигур отличается от роста эльфов)
       
       # Проверим предположения для t-теста
       
       # 1. Проверка нормальности распределения с помощью теста Шапиро-Уилка
       shapiro_elves <- shapiro.test(elves)
       shapiro_figures <- shapiro.test(figures)
       
       cat("Проверка нормальности распределения:\n")
       cat("Эльфы: W =", round(shapiro_elves$statistic, 3), 
           ", p =", round(shapiro_elves$p.value, 4), "\n")
       cat("Фигуры: W =", round(shapiro_figures$statistic, 3), 
           ", p =", round(shapiro_figures$p.value, 4), "\n\n")
       
       # 2. Проверка равенства дисперсий с помощью F-теста
       var_test <- var.test(elves, figures)
       cat("Проверка равенства дисперсий (F-тест):\n")
       cat("F =", round(var_test$statistic, 3), 
           ", p =", round(var_test$p.value, 4), "\n\n")
       
       # Поскольку выборки небольшие, дополнительно проверим нормальность 
       # с помощью Q-Q графиков
       par(mfrow = c(1, 2))
       qqnorm(elves, main = "Q-Q график: Эльфы")
       qqline(elves, col = "red")
       qqnorm(figures, main = "Q-Q график: Фигуры")
       qqline(figures, col = "red")
       par(mfrow = c(1, 1))
       
       # Проведем двухвыборочный t-тест
       # Используем t-тест с равными дисперсиями, так как p-value F-теста > 0.05
       t_test_result <- t.test(figures, elves, var.equal = TRUE)
       
       cat("Результат двухвыборочного t-теста:\n")
       cat("t(", t_test_result$parameter, ") = ", round(t_test_result$statistic, 3), 
           ", p = ", round(t_test_result$p.value, 5), "\n", sep = "")
       cat("95% доверительный интервал разницы средних: [", 
           round(t_test_result$conf.int[1], 2), ", ", 
           round(t_test_result$conf.int[2], 2), "]\n", sep = "")
       cat("Разница средних:", round(t_test_result$estimate[1] - t_test_result$estimate[2], 2), "см\n\n")
       
       # Принимаем решение на основе полученных результатов
       alpha <- 0.05  # уровень значимости
       
       if (t_test_result$p.value < alpha) {
         cat("ВЫВОД: Отвергаем нулевую гипотезу (p <", alpha, ").\n")
         cat("Замеченные фигуры статистически значимо отличаются по росту от эльфов.\n")
         cat("Рекомендуется проявить осторожность - вероятно, это ЧУЖИЕ!\n")
       } else {
         cat("ВЫВОД: Не отвергаем нулевую гипотезу (p >", alpha, ").\n")
         cat("Нет статистически значимых различий в росте между фигурами и эльфами.\n")
         cat("Вероятно, это СВОИ.\n")
       }
       
       # Дополнительная информация для интерпретации
       cat("\nДополнительная информация:\n")
       cat("Средний рост эльфов:", round(mean(elves), 2), "см\n")
       cat("Средний рост фигур:", round(mean(figures), 2), "см\n")
       cat("Разница:", round(mean(figures) - mean(elves), 2), "см\n")