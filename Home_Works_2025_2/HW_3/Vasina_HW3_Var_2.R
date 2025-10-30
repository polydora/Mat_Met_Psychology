
  
# === ЧАСТЬ 1 === #

# Отберем все те записи, которые относятся на 21-й день наблюдения
weights21day <- 
  ChickWeight %>%
  filter(., Time == 21)

# Можно сделать diet1 <- filter(..., Diet == 1), и так для каждой из 4 диет
# А можно сохранить это в массив и обращаться дальше уже по индексам
# Для этого разделим полученный датафрейм по диетам
diets <- split(weights21day, weights21day$Diet)

# Получаем "общую картину" (summary) по разделенным данным по весу
#Данные по диете 1
summary(diets[[1]]$weight)
#Данные по диете 2
summary(diets[[2]]$weight)
#Данные по диете 3
summary(diets[[3]]$weight)
#Данные по диете 4
summary(diets[[4]]$weight)

# === ЧАСТЬ 2 === #

# Рисуем ящики с усами
# Чтобы убрать точки на графике можно использовать outlier.shape = NA
weights21day %>%
  ggplot() +
  geom_boxplot(aes(x = Diet, y = weight), fill = "gray", outlier.shape = NA) +
  theme_bw() +
  xlab('Диеты') +
  ylab('Вес')

# === ЧАСТЬ 3 === #
df <- read.csv("data/dataset_variant_2.csv", sep = ";")

# Будем определять при помощи квантильного графика
install.packages('car')
library(car)

qqPlot(df$Var1, id = FALSE) 
# Не подчиняется
# Соответствует только в двух местах

qqPlot(df$Var2, id = FALSE)
# Не подчиняется
# Выпадает из графика на хвостах

qqPlot(df$Var3, id = FALSE) # Подчиняется

qqPlot(df$Var4, id = FALSE)
# Не подчиняется
# Выпадение из графика в конце

qqPlot(df$Var5, id = FALSE) # Подчиняется

# === ЧАСТЬ 4 === #

# Выборочное среднее - mean()
# Среднеквадратичное отклонение - sd()

# Для 1-й диеты
mean(diets[[1]]$weight)
sd(diets[[1]]$weight)

# Для 2-й диеты
mean(diets[[2]]$weight)
sd(diets[[2]]$weight)

# Для 3-й диеты
mean(diets[[3]]$weight)
sd(diets[[3]]$weight)

# Для 4-й диеты
mean(diets[[4]]$weight)
sd(diets[[4]]$weight)

# === ЧАСТЬ 5 === #

# Для вычисления интервала предварительно нужно получить значения:
# - выборочного среднего (функция mean)
# - среднеквадратичного отклонения (функция sd)
# - стандартной ошибки ( sd(df) / sqrt(length(df)) )

# По диетам
for (i in c(1, 2, 3 ,4)) {
  .mean = mean(diets[[i]]$weight)
  .sd = sd(diets[[i]]$weight)
  .n = length(diets[[i]]$weight)
  .se = .sd / sqrt(.n)
  
  left = .mean - qt(0.975, .n - 1) * .se
  right = .mean + qt(0.975, .n - 1) * .se
  
  .interval = c(left, right)
  
  # Если не приравнять paste к text, то выводить в консоль не будет
  text = paste('Интервал для диеты', i, ':', .mean, '±', qt(0.975, .n - 1), '*', .se)
  # А так будет
  print(text)
}
