# Загружаем необходимые библиотеки
library(Stat2Data)
library(car)  # для leveneTest
library(ggplot2)  # для графиков
library(multcomp)  # для пост-хок тестов



# Загружаем данные
data(SandwichAnts)




# Просмотр структуры данных
str(SandwichAnts)  # Убедимся, что данные загружены правильно



# Посмотрим на первые несколько строк датасета
head(SandwichAnts)



# 1. Модель двухфакторного ANOVA
model <- aov(Ants ~ Filling * Butter, data = SandwichAnts)



# 2. Проверка нормальности остатков
shapiro_test <- shapiro.test(residuals(model))
print(shapiro_test)
# Если p-значение больше 0.05, остатки распределены нормально.
# Это одна из предпосылок ANOVA.




# Построим график Q-Q для остаточных значений
qqnorm(residuals(model))
qqline(residuals(model), col = "red")
# График Q-Q позволяет визуально оценить нормальность остатков.

qqPlot(model)




# 3. Проверка на однородность дисперсий
levene_result <- leveneTest(Ants ~ Filling * Butter, data = SandwichAnts)
print(levene_result)
# Если p-значение больше 0.05, это свидетельствует о том, что дисперсии
# однородны, что является еще одной предпосылкой ANOVA.





# 4. Проведем двухфакторный дисперсионный анализ
aov_results <- aov(Ants ~ Filling * Butter, data = SandwichAnts)
anova_table <- summary(aov_results)
anova_table


df_diag <- fortify(aov_results)

ggplot(df_diag, aes(x = .fitted, y = .stdresid)) +
  geom_point() +
  geom_hline(yintercept = 0)

# Интерпретация результатов ANOVA
# Если p-значение для любого из факторов меньше 0.05, это свидетельствует о
# значительном влиянии этого фактора на количество муравьев.
if (anova_table[[1]][["Pr(>F)"]][1] < 0.05) {
  cat("Фактор Начинка значимо влияет на количество муравьев.\n")
} else {
  cat("Фактор Начинка незначимо влияет на количество муравьев.\n")
}

if (anova_table[[1]][["Pr(>F)"]][2] < 0.05) {
  cat("Фактор Масло значимо влияет на количество муравьев.\n")
} else {
  cat("Фактор Масло незначимо влияет на количество муравьев.\n")
}

if (anova_table[[1]][["Pr(>F)"]][3] < 0.05) {
  cat("Взаимодействие факторов значимо влияет на количество муравьев.\n")
} else {
  cat("Взаимодействие факторов незначимо влияет на количество муравьев.\n")
}




# 5. Построим график взаимодействия
interaction.plot(SandwichAnts$Filling, SandwichAnts$Butter, SandwichAnts$Ants,
                 xlab="Начинка", ylab="Количество муравьев",
                 trace.label="Масло", main="Взаимодействие факторов")
# График взаимодействия поможет визуально оценить, как каждый фактор и их комбинированное действие влияют на количество муравьев.




# 6. Пост-хок тест для множественных сравнений
# Если ANOVA показала значимые результаты, мы можем выполнить тест Тюки
# для определения, какие группы отличаются друг от друга.
tukey_result <- glht(aov_results, linfct = mcp(Filling = "Tukey"))
summary(tukey_result)
# Пост-хок тест позволит выделить конкретные пары, между которыми существует статистически значимая разница.


tukey_result <- glht(aov_results, linfct = mcp(Butter = "Tukey"))
summary(tukey_result)
