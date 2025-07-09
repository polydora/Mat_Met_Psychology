
# Установка необходимых пакетов
# install.packages("ggplot2")
# install.packages("dplyr")
# install.packages("readxl")
# install.packages("multcomp")  # Для пост-хок анализа

# Загрузка пакетов

library(tidyverse)
library(ggplot2)
library(dplyr)
library(readxl)
library(multcomp)  # Для пост-хок анализа


# Чтение данных из Excel файла
data <- read_excel("data/hydrology_2022.xls")



# Просмотр первых строк данных
head(data)



# Преобразуйте переменные в нужный формат
data$Month <- as.factor(data$Month)

# Что столбец S является числовым
data$S <- as.numeric(data$S)

# Удаление NA значений
data <- na.omit(data)


# Проведение ANOVA
anova_result <- aov(S ~ Month, data = data)
summary(anova_result)



# Анализ остатков
par(mfrow=c(2,2))
plot(anova_result)

# Тест на нормальность остатков (тест Шапиро-Уилка)
shapiro.test(residuals(anova_result))

library(car)

qqPlot(anova_result)


# Тест на гомоскедастичность (тест Бартлетта)
bartlett.test(S ~ Month, data = data)

library(ggplot2)

df_diagn <- fortify(anova_result)


ggplot(df_diagn, aes(x = .fitted, y = .stdresid )) +
  geom_point()




library(tidyverse)

# Визуализация средних значений с доверительными интервалами
mean_s <- data %>%
  group_by(Month) %>%
  summarise(mean_S = mean(S),
            sd_S = sd(S),
            n = n()) %>%
  mutate(se = sd_S / sqrt(n),
         upper = mean_S + qt(0.975, df = n-1) * se,
         lower = mean_S - qt(0.975, df = n-1) * se)

ggplot(mean_s, aes(x = Month, y = mean_S)) +
  geom_bar(stat = "identity", position = "dodge", fill = "steelblue") +
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.2) +
  labs(title = "Средние значения солености с доверительными интервалами",
       x = "Месяц", y = "Средняя соленость")


# Пост-хок анализ
tukey_results <- TukeyHSD(anova_result)
print(tukey_results)

# Визуализация результатов Тьюки
plot(tukey_results)
