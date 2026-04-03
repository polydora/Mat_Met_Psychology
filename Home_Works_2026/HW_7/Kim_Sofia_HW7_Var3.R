# Во всех случаях надо провести двухфакторный дисперсионный анализ.

# Все результаты надо сопроводить короткими текстами с рассказом что вы увидели и что это означает (вставляете в виде комментариев). Во всех случаях надо не забыть произвести проверку на условия применимости анализа.

# Рекомендуется ознакомиться с оригинальными работами, на базе которых были получены датасеты. Ссылки на работы даются в комментариях к скрипту.

install.packages("Stat2Data")
library(car)
library(ggplot2)
library(dplyr) 
library(Stat2Data)

data(SandwichAnts)
df <- SandwichAnts %>%
  mutate(Butter = factor(Butter, labels = c("Без масла","С маслом")),
         Filling = factor(Filling, labels = c("Арахисовое","Вегемит","Ветчина")))
ggplot(df, aes(Filling, Ants, fill = Butter)) + geom_boxplot() + theme_bw()


but_brod <- aov(Ants ~ Filling * Butter, df)
summary(but_brod) # влияет только начинка (p < 0.05), масло и взаимодействие нет
TukeyHSD(but_brod) # ветчина отличается отo всех, остальные одинаковы

df %>% group_by(Filling, Butter) %>%
  summarise(m = mean(Ants), se = sd(Ants)/sqrt(n())) %>%
  ggplot(aes(Filling, m, fill = Butter)) +
  geom_col(position = "dodge") +
  geom_errorbar(aes(ymin = m - se, ymax = m + se), position = "dodge", width = 0.5) +
  theme_bw()

# Вывод: начинка сильно влияет на количество муравьев (ветчина в 2-3 раза популярнее), а масло не имеет значения. Условия ANOVA выполняются.