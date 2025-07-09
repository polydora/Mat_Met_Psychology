# Домашнее задание №7. Двухфакторный дисперсионный анализ
# Демиденко ЕР Вариант 1. 

# Устанавливаем и распаковываем пакеты
install.packages("palmerpenguins")
library(palmerpenguins)
library(ggplot2)
library(dplyr)

# Создаем датафрейм, проверяем, перемименовываем названия столбцов на более короткие 
peng <- as.data.frame(penguins[, c(1, 6, 7)])
str(peng)
colnames(peng) <- c('sp', 'mass', 'sex')

# Проверяем наличие пропущенных значений (их оказывается 13, но только 1 строка имеет два пропущенных)
colSums(is.na(peng))
peng_clean <- na.omit(peng)

# У нас два фактора: вид и пол
peng$sp <- factor(peng$sp)
peng$sex <- factor(peng$sex)

# Таблица с количеством пингвинов каждого пола для каждого вида, 
table(peng$sp, peng$sex)

# визуализируем среднюю массу с помощью графика типа pointrange, который отражает диапазон значений 
ggplot(data = peng, aes(x = sp, y = mass, colour = sex)) +
  stat_summary(geom = "pointrange", fun.data = mean_cl_normal,
               position = position_dodge(width = 0.5)) +
  labs(title = "Средняя масса пингвинов",
       x = "Вид пингвина", 
       y = "Вес (г)")

# Дсперсионный анализ (ANOVA)
anova_peng <- aov(mass ~ sp * sex, data = peng_clean)
summary(anova_peng)

# Визуализируем данные после дисперсионного анализа в виде боксплотов
library(car)
ggplot(data = peng_clean, aes(x = interaction(sp, sex), y = mass, fill = sex)) +
  geom_boxplot() +
  labs(title = "Сравнение массы пингвинов",
       x = "Вид и пол пингвина",
       y = "Масса (г)") +  
  theme_dark()