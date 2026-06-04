# Загрузка данных
library(ggplot2)
library(MASS)
data(birthwt)


# ДАННЫЕ
#Перекодируем переменные
birthwt$smoke <- factor(birthwt$smoke, labels = c("Нет", "Да"))
birthwt$race  <- factor(birthwt$race, labels = c("Белые", "Чёрные", "Другие"))

##Оставляю только белых и чёрных
bw_sub <- subset(birthwt, race %in% c("Белые", "Чёрные"))
bw_sub$race <- droplevels(bw_sub$race)

#Тут група по возрасту
bw_sub$age_group <- ifelse(bw_sub$age <= 21, "Молодые (≤21)", "Взрослые (>21)")
bw_sub$age_group <- factor(bw_sub$age_group)

cat("Размер данных после фильтрации:", nrow(bw_sub), "наблюдений\n")
cat("Белые:", sum(bw_sub$race == "Белые"), "\n")
cat("Чёрные:", sum(bw_sub$race == "Чёрные"), "\n")



# РЕЗУЛЬТАТЫ
## 1. Курение и вес новорожденного

#Описательные статистики
agg <- aggregate(bwt ~ smoke, data = birthwt, 
                 function(x) c(Mean = mean(x), SD = sd(x), N = length(x)))
print(agg)

#T-тест
tt <- t.test(bwt ~ smoke, data = birthwt)
if (tt$p.value < 0.05) {
  cat("Вес новорожденных СТАТИСТИЧЕСКИ ЗНАЧИМО различается между группами.\n")
} else {
  cat("Вес новорожденных ЗНАЧИМО НЕ различается между группами.\n")
}

ggplot(birthwt, aes(x = smoke, y = bwt, fill = smoke)) +
  geom_boxplot() +
  labs(title = "Вес новорожденных у курящих и некурящих матерей",
       x = "Курение", y = "Вес (г)") +
  theme_minimal() +
  scale_fill_manual(values = c("Нет" = "lightgreen", "Да" = "salmon")) +
  theme(legend.position = "none")


## 2. Раса и возраст — двухфакторный ANOVA

#Таблица средних
agg2 <- aggregate(bwt ~ race + age_group, data = bw_sub, mean)
names(agg2)[3] <- "Mean_bwt"
print(agg2)

#Двухфакторный ANOVA
model <- aov(bwt ~ race * age_group, data = bw_sub)
summary(model)

#Извлекаем p-value
anova_table <- summary(model)[[1]]
p_race <- anova_table[1, "Pr(>F)"]
p_age  <- anova_table[2, "Pr(>F)"]
p_int  <- anova_table[3, "Pr(>F)"]

#Проверка остатков
residuals <- residuals(model)
cat("Тест Шапиро-Уилка на остатках: p-value =", 
    round(shapiro.test(residuals)$p.value, 4), "\n")

#Диагностические графики
par(mfrow = c(2, 2))
plot(model)
par(mfrow = c(1, 1))

#График взаимодействия
interaction.plot(
  x.factor = bw_sub$age_group,
  trace.factor = bw_sub$race,
  response = bw_sub$bwt,
  col = c("blue", "red"),
  lwd = 2,
  xlab = "Возрастная группа",
  ylab = "Средний вес (г)",
  trace.label = "Раса",
  main = "Взаимодействие: Раса × Возраст"
)



# ВЫВОДЫ
cat("### Курение и вес\n")
if (tt$p.value < 0.05) {
  cat("Курение во время беременности ЗНАЧИМО снижает вес новорожденного. ")
  cat("Разница в среднем весе составляет ", 
      round(abs(diff(tt$estimate)), 1), " г.\n\n")
} else {
  cat("Значимых различий в весе новорожденных у курящих и некурящих матерей ")
  cat("не обнаружено (p = ", round(tt$p.value, 4), ").\n\n")
}

cat("### Влияние расы и возраста\n")
if (p_race < 0.05) {
  cat("Раса матери ЗНАЧИМО влияет на вес новорожденного.\n")
} else {
  cat("Раса матери ЗНАЧИМО НЕ влияет на вес новорожденного.\n")
}

if (p_age < 0.05) {
  cat("Возраст матери ЗНАЧИМО влияет на вес новорожденного.\n")
} else {
  cat("Возраст матери ЗНАЧИМО НЕ влияет на вес новорожденного.\n")
}

if (p_int < 0.05) {
  cat("Взаимодействие расы и возраста ЗНАЧИМО — влияние возраста зависит от расы.\n")
} else {
  cat("Взаимодействие расы и возраста НЕ ЗНАЧИМО.\n")
}