##Однофакторный дисперсионный анализ
##Анализ солености (вариант 1)
library(readxl)
#Прочитаем данные из файла и посмотрим их структуру
salt <- read_excel("hydrology_2022.xls")
head(salt)
str(salt)
#Преобразуем данные в столбцах Month в фактор, а в столбце S в числовые
salt$Month <- factor(salt$Month)
salt$S <- as.numeric(salt$S)
#Проверим что все получилось и посмотрим есть ли na в столбце S
str(salt)
colSums(is.na(salt))
#na есть, удалим их и проверим что строки удалились
salt_clean <- salt[!is.na(salt$S), ]
str(salt_clean)


#Проведем дисперсионный анализ солености воды в летние месяцы
salt_anova <- aov(S ~ Month, data = salt_clean)
summary(salt_anova)
#Результаты анализа показывают, что соленость воды отличается в каждый из месяцев, P=3.19e-14 < 0,01

#Анализ остатков
library(ggplot2)
salt_diag <- fortify(salt_anova)
ggplot(data = salt_diag, aes(x = .fitted, y = .resid)) + 
  geom_point()
#график рассеяния остатков показывает отсутствие признаков гетероскедастичности
ggplot(data = salt_diag, aes(x = Month, y = .resid)) + geom_boxplot() + geom_hline(yintercept = 0)
#Остатки в пределах двух стандартных отклонений, средние близки к 0
library(car)
qqPlot(salt_diag$.resid)
#Остатки не подчиняются нормальному распределению, один из путей решения проблемы - логарифмическое преобразование переменной

#Проведем попарные сравнения средних с помощью теста Тьюки
salt_Tuk <- TukeyHSD(salt_anova)
salt_Tuk
#Попарное сравнение показывает различия в соленности в парах Июль-август и Июнь-Июль, а в паре Июнь-Август различий нет


#Визуализация данных
library(dplyr)
# Преобразование исходных данных и подготовка
mean_data <- salt_clean %>%
  group_by(Month) %>%
  summarise(mean_S = mean(S),
            lower_ci = mean_S - qt(0.975, df=n()-1)*sd(S)/sqrt(n()),
            upper_ci = mean_S + qt(0.975, df=n()-1)*sd(S)/sqrt(n()))

# Графическое представление
ggplot(mean_data, aes(x = reorder(Month, mean_S), y = mean_S)) +
  geom_bar(stat="identity", fill="green") + 
  geom_errorbar(aes(ymin = lower_ci, ymax = upper_ci), width=.2) + 
  labs(title = "Средняя солёность воды по месяцам",
       x = "Месяц",
       y = "Среднее значение солёности") +
  theme_minimal() +
  coord_cartesian(ylim=c(min(mean_data$lower_ci)-1, max(mean_data$upper_ci)+1))
#На графике видно что среднее значение солености в июле гораздо ниже чем в июне и августе, а июнь и август между собой практически не отличаются

