arcsin_value <- asin(0.5)
sqrt_arcsin <- sqrt(arcsin_value)
constant <- 180 / pi
sqrt_11 <- sqrt(11)
e_value <- exp(1)
sqrt4_10_plus_e2 <- (10 + e_value^2)^(1/4) 

# Первая часть выражения
first_part <- 2 * sqrt_arcsin * constant

# Вторая часть выражения
second_part <- sqrt_11 * sqrt4_10_plus_e2 * 50

# Полное выражение
result <- first_part - second_part

# Печатаем результат
cat("Результат выражения:", result, "\n")

# Проверяем, можно ли взять логарифм
if (result > 0) {
  log_result <- log2(result)
  cat("log2 результата:", log_result, "\n")
} else {
  cat("Логарифм отрицательного числа не определен.\n")
}




