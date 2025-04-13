library(dplyr)
library(ggplot2)

# install.packages("writexl")
library(writexl)
# install.packages("readxl")
library(readxl)

hydro <- read_excel("C:/Users/galak/Desktop/mat_med_in_biology/mat_med/data/hydrology_2022.xls")
hydro


# S - соленость, в данном случае зависимая переменная 
# Month - месяц


hydro$Month <- factor(hydro$Month)
str(hydro)


hydro$S <- as.numeric(hydro$S)



colSums(is.na(hydro)) # есть ли пропущенные значения


# hydro <- hydro[!is.na(hydro$S), ]


nrow(hydro)

hydro %>%
  filter(!is.na(S)) -> hd

hd


table(hd$Month)
hd %>%
  mutate(Month2 = case_when( Month == 1 ~ "июнь",
                             Month == 2 ~ "июль", 
                             Month == 3 ~ "август")
  ) -> hd

hd$Month <- hd$Month2

library(ggplot2)

hydro %>% 
  filter(!is.na(S)) -> hd


hd$Month <- factor(hd$Month, labels = c("июнь", "июль", "август" ))

ggplot(data = hd, mapping = aes(x = Month, y = S)) + 
  geom_point(position = position_jitter(width = 0.1))



# Визуализация (с доверительными интервалами) средние значения

hd %>%
  group_by(Month) %>%
  summarise(Mean = mean(S), sample_n = n(), SE = sd(S)/sqrt(sample_n), t_kr = qt(p = c(0.975), df = (sample_n - 1)), CI_low = Mean - t_kr*SE, CI_up = Mean + t_kr*SE) %>%
  ggplot(aes(x = Month, y = Mean, color = Month)) +
  geom_point() +
  geom_errorbar(aes(ymin = CI_low, ymax = CI_up), width = 0)


ggplot(data = hd, aes(x = Month, y = S, colour = Month)) + 
  stat_summary(geom = 'pointrange', fun.data = mean_cl_normal) +
  labs(x = 'Месяца', 
       y = 'Уровень солености',
       colour = 'Месяца')

hd$S



mean(hd$S)


# дисперсионный анализ в R (показывает, есть ли влияние фактора (т.е. различаются ли средние значения зависимой переменной между группами))

model <- aov(formula = S ~ Month, data = hd) #модел - связь между переменными
qf(p=0.95, df = 2, df2 = 284)        #(Fэмп = 34, отвергаем H0)

summary(model)    #если не руками считать, то вот есть ф-ия



# Данные для анализа остатков

residuals(model)        #остатки - разница между наблюдаемыми значениями и средними (из каждого наблюдаемого значения вычитаем среднее для соотвествующей группы ) они важны, так как, например, если модель правильная, то все остатки должны быть приблизительно около нуля


hd_diag <- fortify(model)                       # дианостический дата фрейм

ggplot(hd_diag, aes(x = .fitted, y = .resid)) +        #по оси х категориальные значения(предсказанные средние), по оси y остатки
  geom_point() +
  geom_hline(yintercept = 0)      #гетероскедастичность, вроде как все равномерно??? поэтому все хорошо ???




# нормально ли распределены остатки (так как если остатки подчинаюятся нормальному распределению, то и вся группа тоже)
library(car)

qqPlot(x = residuals(model))       #а вот остатки как-то криво распределены, поэтому я бы не верила результатам дисперсионного анализа



# Пост-хок тесты показывают, какие именно из возможных пар средних значений различаются

TukeyHSD(model)     #lwr и upr - границы доверительного интервала. если перескакивает ноль - то имеет правду считать при условии нулевой гипотезы. по значению p-value: между июль-июнем и августом-июлем разница колоссальная (p-value < 0.05), а между июнем и августом незначительная (p-value > 0.05)


#как вывод, получается, что уровень солености в июне и августе значительно выше, чем в июле


























