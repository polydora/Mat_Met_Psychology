

# # Двухвыборочный t-тест ####################################################


# ## Пример: Гормоны и артериальная гипертензия
#
# Синдром Кушинга --- это нарушения уровня артериального давления, вызванные гиперсекрецией кортизола надпочечниками.
#
# В датасете `Cushings` (пакет `MASS`) записаны данные о секреции двух метаболитов при разных типах синдрома (данные из кн. Aitchison, Dunsmore, 1975).
#
# - `Tetrahydrocortisone` --- секреция тетрагидрокортизона с мочой (мг/сут.)
# - `Pregnanetriol` --- секреция прегнантриола с мочой (мг/сут.)
# - `Type` --- тип синдрома:
#   - `a` --- аденома
# - `b` --- двусторонняя гиперплазия
# - `c` --- карцинома
# - `u` --- не известно
#
# Различается ли секреция тетрагидрокортизона при аденома и двусторонней гиперплазии надпочечников?
library(MASS)
library(car)
data("Cushings")

head(Cushings)
str(Cushings)

colSums(is.na(Cushings))

table(Cushings$Type)

qqPlot(Cushings$Tetrahydrocortisone[Cushings$Type == 'a'])
qqPlot(Cushings$Tetrahydrocortisone[Cushings$Type == 'b'])

tt <- t.test(x = Cushings$Tetrahydrocortisone[Cushings$Type == 'a'],
             y = Cushings$Tetrahydrocortisone[Cushings$Type == 'b'])
tt



# Задание 3------------------------------------------------------------------
# Перепишите вызов функции t.test с использованием
# другого шаблона вызова (с использованием формулы).
tt <- t.test(formula =  ~ , data = Cushings,
             subset = Cushings$Type  c('a', 'b'))
tt



# Задание 4------------------------------------------------------------------
# Посмотрите структуру результатов (`tt`) при помощи
# функции `str()` и извлеките из них:
# - степени свободы
# - уровень значимости
# - значение t-критерия





#############################################
# Пермутационные оценки значимости различий #
#############################################

a = Cushings$Tetrahydrocortisone[Cushings$Type == 'a']
b = Cushings$Tetrahydrocortisone[Cushings$Type == 'b']

mean_a <- mean(a)
sd_a <- sd(a)
n_a <- length(a)
se_a <- sd_a/sqrt(n_a)

mean_b <- mean(b)
sd_b <- sd(b)
n_b <- length(b)
se_b <- sd_b/sqrt(n_b)

t_emp <- abs(mean_a - mean_b)/sqrt(se_a^2 + se_b^2)


# Пермутируем векторы

all_data <- sample(c(a, b))
a_perm <- all_data[1:n_a]
b_perm <- all_data[(n_a +1):length(all_data)]

mean_a <- mean(a_perm)
sd_a <- sd(a_perm)
n_a <- length(a_perm)
se_a <- sd_a/sqrt(n_a)

mean_b <- mean(b_perm)
sd_b <- sd(b_perm)
n_b <- length(b_perm)
se_b <- sd_b/sqrt(n_b)

t_perm <- abs(mean_a - mean_b)/sqrt(se_a^2 + se_b^2)

# Повторим пермутации многократно

t_perm_vector <- rep(NA, 1000)

for(i in 1:999){
  all_data <- sample(c(a, b))
  a_perm <- all_data[1:n_a]
  b_perm <- all_data[(n_a +1):length(all_data)]

  mean_a <- mean(a_perm)
  sd_a <- sd(a_perm)
  n_a <- length(a_perm)
  se_a <- sd_a/sqrt(n_a)

  mean_b <- mean(b_perm)
  sd_b <- sd(b_perm)
  n_b <- length(b_perm)
  se_b <- sd_b/sqrt(n_b)

  t_perm <- abs(mean_a - mean_b)/sqrt(se_a^2 + se_b^2)

  t_perm_vector[i] <-  t_perm
}


t_perm_vector[1000] <- t_emp


mean(t_perm_vector >= t_emp) #Доля пермутационных статистик которые больше или равны выборочной статистике

tt #Результаты парамтерического теста




# ## Задание 5 ----------------------------------------------------------------

# Файл `aml.csv` содержит данные о влиянии регулярной химиотерапии
# на продолжительность ремиссии.
# Прочитаем эти данные
rem <- read.csv(file = "data/aml.csv", header = TRUE)
str(rem)
# - В переменной `time` представлена продолжительность ремиссии в днях.
# - `group` указывает, к какой экспериментальной группе принадлежал пациент.
# В группе 1 проводилась регулярная химиотерапия, в группе 2 - нет.
#
# - Сравните эти группы с помощью t-теста.
# - Постройте график со средними и доверительными интервалами
# для продолжительности ремиссии в этих группах.


