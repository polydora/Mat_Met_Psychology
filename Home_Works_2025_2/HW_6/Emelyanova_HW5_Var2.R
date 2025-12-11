## ДЗ 5 ##
## 1. Необходимо выяснить наблюдаются ли различия между тремя летними месяцами (нужно будет провести дисперсионный анализ) в том параметре, котоый будет у вашего варианта.Вариант 2. Анализ температуры воздуха (переменная Air_T) ###

## Запустим нужные библиотеки ###

library(readxl)
library(dplyr)

## Прочитаем файл, убедимся, что переменная месяца – фактор, очистим от пропусков ###

dat <- read_xls("D:/Mat_Met_2025/Data/hydrology_2022.xls")
dat$Month <- factor(dat$Month)
dat_clean <- dat[!is.na(hydrology$Air_T), ]

## Преобразуем данные переменной в числовые для корректной работы ANOVA ##

head(dat_clean$Air_T)
dat_clean$Air_T <- gsub(",", ".", dat_clean$Air_T)
dat_clean$Air_T <- as.numeric(dat_clean$Air_T)
str(dat_clean$Air_T)
summary(dat_clean$Air_T)

## Проведем однофакторный дисперсионный анализ (через функции aov и lm) ###

fit <- aov(Air_T ~ Month, data = dat_clean)
summary(fit)

model <- lm(Air_T ~ Month, data = dat_clean)
summary(model)
anova(model)

## Трактовка анализа - найдем квантили для F-распределения ###

qf(p = 0.95, df1 = 2, df2 = 243)

## F = 52.79, гораздо больше 3,03, различия имеются ###

## проведем анализ остатков и оценим, выполняются ли условия применимости ANOVA ###

## Запустим нужную библиотеку ###

library(ggplot2)
diagnostica <- fortify(model)
unique(diagnostica$.fitted)
ggplot(diagnostica, aes(x = .fitted, y = .stdresid)) +
  geom_point()+
  geom_hline(yintercept = 0)

ggplot(diagnostica, aes(x = Month, y = .stdresid)) +
  geom_boxplot()+
  geom_hline(yintercept = 0)

## Проверим нормальность распределения остатков ###
## Запустим нужную библиотеку ###

library(car)
qqPlot(diagnostica$.stdresid)

## По графику видно, что отклонения от нормального распределения нет ###

## Визуализируйте с помощью столбчатой диаграммы (с доверительными интервалами) средние значения. Проведя post-hoc сравнение средних, обозначьте на рисунке те средние, которые отличаются друг от друга по критерию Тьюки ###

TukeyHSD(fit)

## Запустим нужную библиотеку ###

library(multcompView)

tuk <- TukeyHSD(fit, "Month")  

## Преобразуем p‑values Тьюки в буквенные группы ###

tuk_letters <- multcompLetters(tuk$Month[,"p adj"])$Letters

groups <- data.frame(
  Month = names(tuk_letters),
  group = tuk_letters
)

## Считаем средние и SE для каждого месяца ###

sum_stat <- dat_clean |>
  group_by(Month) |>
  summarise(
    mean_Air_T = mean(Air_T, na.rm = TRUE),
    sd_Air_T   = sd(Air_T,   na.rm = TRUE),
    n          = n(),
    se_Air_T   = sd_Air_T / sqrt(n)
  )

## Соединяем статистики и буквенные группы ###

plot_dat <- left_join(sum_stat, groups, by = "Month")

## Строим столбчатую диаграмму с доверительными интервалами (mean ± SE) ###

ggplot(plot_dat, aes(x = Month, y = mean_Air_T)) +
  geom_col(fill = "grey70") +
  geom_errorbar(aes(ymin = mean_Air_T - se_Air_T,
                    ymax = mean_Air_T + se_Air_T),
                width = 0.2) +
  geom_text(aes(label = group, y = mean_Air_T + se_Air_T + 0.5),
            size = 5) +
  labs(x = "Месяц",
       y = "Средняя температура воздуха, °C",
       title = "Средние значения Air_T по месяцам\nс доверительными интервалами и группами Тьюки") +
  theme_bw()

