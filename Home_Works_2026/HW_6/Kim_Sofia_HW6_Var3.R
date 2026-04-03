# Домашнее задание 6. Дисперсионный анализ.
# Скачайте файл, в котором находятся данные по метеорологическим наблюдениям на территории Кандалакшского заповедника. Файл поместите в папку Data.

# Необходимо выяснить наблюдаются ли различия между тремя летними месяцами (нужно будет провести дисперсионный анализ) в том параметре, который будет у вашего варианта.

# Проведите анализ остатков и оцените, выполняются ли условия применимости ANOVA

# Визуализируйте с помощью столбчатой диаграммы (с доверительными интервалами) средние значения. Проведя post-hoc сравнение средних, обозначьте на рисунке те средние, которые отличаются друг от друга по критерию Тьюки.

# Вариант 3. Анализ температуры воды (переменная Water_T).


library(car)
library(dplyr)
library(ggplot2)
library(readxl)

df <- data.frame("D:/Text/MatMet_MIP_2026/Data/hydrology_2022.xls")
df <- read_excel("D:/Text/MatMet_MIP_2026/Data/hydrology_2022.xls") %>%
  mutate(Water_T = as.numeric(Water_T),
         Month = factor(Month, labels = c("Июнь","Июль","Август"))) %>%
  filter(!is.na(Water_T), Month %in% c("Июнь","Июль","Август"))

water_hyd <- aov(Water_T ~ Month, data = df)
summary(water_hyd)

df_count <- df %>% group_by(Month) %>% summarise(length = n())

wtr_ae <- aov(length ~ Month, df_count)
summary(wtr_ae)
TukeyHSD(wtr_ae)

ggplot(df_count, aes(Month, length, fill = Month)) +
  geom_col() + geom_text(aes(label = length), vjust = -0.5) +
  theme_bw() + labs(y = "Количество наблюдений") + theme(legend.position = "none")
