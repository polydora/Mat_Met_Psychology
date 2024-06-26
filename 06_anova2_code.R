# # Пример: Возраст и способы запоминания ############################
# Какие способы запоминания информации лучше
# работают для молодых и для пожилых? (Eysenck,
# 1974)
# Факторы:
# - `Age` - Возраст:
#     - `Younger` - 50 молодых
#     - `Older` - 50 пожилых (55-65 лет)
# - `Process` - тип активности:
#     - `Counting` - посчитать число букв
#     - `Rhyming` - придумать рифму к слову
#     - `Adjective` - придумать прилагательное
#     - `Imagery` - представить образ
#     - `Intentional` - запомнить слово
# Зависимая переменная - `Words` - сколько вспомнили слов
# Пример из http://www.statsci.org/data/general/eysenck.html

# ## Открываем данные
memory <- read.table(file = "data/eysenck.csv", header = TRUE, sep = "\t")
# Все ли правильно открылось?
str(memory) # Структура данных
head(memory, 2) # Первые несколько строк файла

# Делаем факторы факторами
memory$Process <- factor(memory$Process,
                         levels = c("Adjective", "Counting",
                                    "Imagery", "Intentional", "Rhyming"),
                         labels = c("Прилагательное", "Число букв", "Образ", "Запоминание", "Рифмы"))

memory$Age <- factor(memory$Age, levels = c("Older", "Younger"),
                     labels = c("Пожилой", "Молодой"))

# Есть ли пропущенные значения
# (особенно, в переменных, которые нас интересуют)?
colSums(is.na(memory))
# Каков объем выборки?
nrow(memory) # всего
table(memory$Age, memory$Process) # в группах



# ## Задание 1 -----------------------------------------------------------
#
# Дополните код, чтобы построить график, на
# котором приведено среднее число слов (`Words`)
# для каждого возраста (`Age`) и способа
# запоминания (`Process`).

library()
theme_set()
ggplot(data = , aes()) +
  stat_summary(geom = '', fun.data = ,
               position = position_dodge(width = 0.5))






# # Двухфакторный дисперсионный анализ ###################################

# ## Задаем модель со взаимодействием в R
# Взаимодействие обозначается `:` --- двоеточием
# Если есть факторы A и B, то их взаимодействие A:B

