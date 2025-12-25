## ДЗ 6. Вариант 2 ###

# Рис

# В датасете rice из пакета DAAG содержатся данные
# о росте риса (Perrine et al., 2001; Maindonald,
# Braun, 2015).

# Если не получится загрузить данные из пакета,
# скачайте файл по ссылке
# https://www.rdocumentation.org/packages/DAAG/versions/1.24/topics/rice

# Проанализируйте, как
# зависит сухая масса побегов риса от сорта риса,
# удобрений и взаимодействия этих факторов.

library(DAAG)
data(rice)
?rice
str(rice)

## Для того, чтобы увидеть, как в таблице расположены наблюдения и переменные, выгрузила данные в файл Exel, установив нужные пакеты ###

install.packages("openxlsx", dependencies = TRUE)
install.packages("writexl")
library(openxlsx)
library(writexl)

write_xlsx(rice, "rice.xls")

## Данные в таблице: 6 типов эксперимента, в зависимости от наличия удобрений и бактерии. Block - сорт (1 или 2), по 6 особей каждого сорта в каждом эксперименте. Удобрений (trt) 3 вида, бактерия (variety) одна. Замеры сухой массы корней (RootDryMass) и ростков (ShootDryMass) ###

names(rice)

## Проверим объем выборки ###

nrow(rice)

## Сделаем факторы факторами ###

rice$trt <- factor(rice$trt,
                   levels = c("F10", "NH4Cl", "NH4NO3", "F10 +ANU843", "NH4Cl +ANU843",
                              "NH4NO3 +ANU843"))

rice$Block <- factor(rice$Block,
                     levels = c("1", "2"),
                     labels = c("Сорт 1", "Сорт 2"))

## Проверим, есть ли отсутствующие значения ###

colSums(is.na(rice))

## Отсутствующих значений нет ###

## Проверим, сколько значений в группах ###

table(rice$trt, rice$Block)

## Количество значений в группах одинаковое ###

## Посмотрим визуально, есть ли взаимное влияние факторов ###

library(ggplot2)
theme_set(theme_bw())
ggplot(data = rice, aes(x = Block, y = ShootDryMass, colour = trt))+
      stat_summary(geom = 'pointrange', fun.data = mean_cl_normal,
      position = position_dodge(width = 0.5))


## Двухфакторный дисперсионный аналиp, переменная ShootDryMass ###

anova_rice <- aov(ShootDryMass ~ Block*trt, data = rice)

summary(anova_rice)

## Проведем диагностику на возможность применения двухфакторного дисперсионного анализа, анализ остатков ###

## Тест на гетероскедастичность ###

diag_rice <- fortify(anova_rice)

library(ggplot2)
ggplot(data = diag_rice, aes(x = .fitted, y = .stdresid))+
  geom_point()+
  geom_hline(yintercept = 0)

ggplot(data = diag_rice, aes(x = trt, y = .stdresid))+
  geom_boxplot()+
  geom_hline(yintercept = 0)

## Не наблюдается гетероскедастичность, значения остатков в основном равномерно распределены вдоль 0 ###

## Проверка нормальности распределения остатков ###

library(car)

qqPlot(diag_rice$.stdresid)

## По графикам видно, что остатки нормально распределены ###

## Тест Тьюки ###

TukeyHSD(anova_rice)

## Интерпретация анализа ###

## Для модели, описывающей влияние удобрений на сорт риса, получены статистически значимые эффекты, а именно по фактору Block (сорт) F=13.60, p=0.00049, по фактору trt (Удобрение) F=52.68, p≈< 2e-16 и по их взаимодействию Block×trt (F=4.22, p=0.00238). двухфакторный дисперсионный анализ показывает значимые эффекты фактора Block (Сорт), фактора trt (Удобрение) и их взаимодействия, то есть влияние вариантов обработки удобрениями на массу побегов зависит от сорта (Тест Тьюки, p < 0.005).###
