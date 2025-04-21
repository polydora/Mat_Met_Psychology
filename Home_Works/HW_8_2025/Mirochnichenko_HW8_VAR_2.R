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


setwd("C:/Mat_Met_Biology")

library(dplyr)
library(ggplot2)
# install.packages('DAAG', dependencies = TRUE)

library(DAAG)
data(rice)
?rice

str(rice)

#### есть ли пропущенные значения
colSums(is.na(rice))

#### смотрела, что в каком столбце
rice$trt
rice$fert
rice$ShootDryMass

#### выборка
nrow(rice)

## table(rice$variety, rice$trt)

### выборка в группах
table(rice$variety, rice$fert)

## сбалансированный, потому что одинакова выборка


### рисуем график со средним значением
library(ggplot2)
library(dplyr)

rice %>%
  group_by(variety, fert) %>%
  summarise(Mean = mean(ShootDryMass), sample_n = n(), SE = sd(ShootDryMass)/sqrt(sample_n), t_kr = qt(p = c(0.975), df = (sample_n - 1)), CI_low = Mean - t_kr*SE, CI_up = Mean + t_kr*SE) -> means_shoot

theme_set(theme_bw())

ggplot(data = means_shoot, aes(x = fert, y = Mean, color = variety)) +
  geom_point(size = 3, position = position_dodge(width = 0.5)) +
  geom_errorbar(aes(ymin = CI_low, ymax = CI_up), position = position_dodge(width = 0.5))

## сильное различие при использовании удобрения F10


### дисперсионный анализ
anova_shoot <- aov(ShootDryMass ~ variety*fert, data = rice)
anova_shoot

anova_shoot2 <- aov(ShootDryMass ~ fert + variety + fert:variety, data = rice)
anova_shoot2

summary(anova_shoot) ## до post-hoc тестов не смотрим на variety и fert отдельно
summary(anova_shoot2)


### рисуем графики остатков от предикторов в модели

library(car)

df_diag <- fortify(anova_shoot)

ggplot(df_diag, aes(x = .fitted, y = .stdresid)) +
  geom_point() +
  geom_hline(yintercept = 0)

ggplot(df_diag, aes(x = fert, y = .stdresid)) +
  geom_boxplot() +
  geom_hline(yintercept = 0)

ggplot(data = df_diag, aes(x = variety, y = .stdresid)) +
  geom_boxplot() +
  geom_hline(yintercept = 0)

ggplot(data = df_diag, aes(x = fert, y = .stdresid, colour = variety)) +
  geom_boxplot() +
  geom_hline(yintercept = 0)

## маленький разброс в F10, особенно значительно у ANU843 (т.е. в модифицированном типе риса)

qqPlot(anova_shoot) ## 1 точка вне нормального распределения, других значительных отклонений не наблюдаю


Anova(anova_shoot)
Anova(anova_shoot2)

## взаимодействие значимо, отдельно факторы не смотрим


### нужно делать post-hoc test по взаимодействию факторов

TukeyHSD(anova_shoot) ## p adj должно быть < 0.05
## p adj большое у $variety:fert в ANU843:NH4Cl-wt:NH4Cl, wt:NH4NO3-wt:NH4Cl, ANU843:NH4NO3-wt:NH4Cl, ANU843:NH4NO3-wt:NH4NO3
