# Добашнее задание 1 - Знакомство с R
# Авакян Кристина
# Группа: ББ5А23/10

# ==================================================================
# Задание 1. Получение справки
# ==================================================================

#' Какой параметр функции Anova() из пакета car определяет объект, который функция получает на входе.
#' Ответ пишите в файле скрипта в виде комментариев.

?Anova() # Запрашиваем справку по функции

# !!! Получаем ошибку о отсутсивии функции Anova(), так как пакет car не доступн в системе

#' Error in .helpForCall(topicExpr, parent.frame()) : 
#' no methods for ‘Anova’ and no documentation for it as a function
#' In addition: Warning messages:
#' 1: In read.dcf(file.path(p, "DESCRIPTION"), c("Package", "Version")) :
#'    cannot open compressed file '/Library/Frameworks/R.framework/Versions/4.5-arm64/Resources/library/car/DESCRIPTION',
#'    probable reason 'No such file or directory'
#' 2: In find.package(if (is.null(package)) loadedNamespaces() else package,  :
#'    there is no package called ‘car’

install.packages("car") # Устанавливаем пакет car
library(car) # Подкючаем пакет car
?Anova() # Запрашиваем справку по функции повторно

# Получаем ОТВЕТ:
#' Arguments
#' mod	
#' lm, aov, glm, multinom, polr mlm, coxph, coxme, lme, mer,
#' merMod, svyglm, svycoxph, rlm, clm, clmm, or other suitable model object.
#' Параметр mod - это первый аргумент функции, который определяет модельный объект, 
#' передаваемый на вход в Anova()

# ==================================================================
# Задание 2. R как калькулятор, математические операции
# ==================================================================
# Считаем весь пример целиком
log( ((180 / sqrt(2)) - (cos(pi) / (11^(1/4)))) / (10 + exp(1))^5 , base = 4 )

# ==================================================================
# Задание 3. Переменные и оформление кода
# ==================================================================

# Устанавливаем пакет readxl
# Подключаем пакет readxl
# Загружаем таблицу
# Строим модель и получаем данные

install.packages("readxl") 
library(readxl)
cat <- read_excel("Data/catsM.xlsx")
str(cat) # БЫЛИ ПРОПУЩЕНЫ СКОБКИ в `str cat`
Model <- lm(Hwt ~ Bwt, data = cat)
par(mfrow = c(2, 2))
plot(Model, page = 1) # << При запуске этой строки предупреждение
                      #
                      # In doTryCatch(return(expr), name, parentenv, handler) :
                      # graphical parameter "page" cannot be set
                      #
                      # На форумах рекомендуют plot(Model) или plot(Model, which = 1)

summary(Model)

# ==================================================================
# Задание 4. Векторы
# ==================================================================

n <- round(runif(1, min = 10, max = 50),0)
vector <- round(rnorm(n, 0, 1), 0)

length(vector) # Количество чисел в нем.
median(vector) # Медиану в этом векторе.
sd(vector) # Срднеквадратичное отклонение для данного вектора.
range(vector) # Одновременно и максимальное и минимальное значение для данного вектора.

# ==================================================================
# Задание 5. Датафреймы
# ==================================================================

# Создаем данные
df <- data.frame(Var_1 = rep(letters[11:19], each =2), Var_2 = seq(10, 1000, length.out = 18), Var_3 = NA)
df

# Изменяем имена переменным
names(df) <- c("Var_One", "Var_Two", "Var_Thr")
# или
colnames(df) <- c("Var_One", "Var_Two", "Var_Thr")

# Выбираем только те строки для которых Var_One равно k или s или r
df[df$Var_One %in% c("k", "s", "r"), ]

# Тоже самое через пакет dplyr
# library(dplyr)
# df %>%
#  filter(Var_One %in% c("k", "s", "r"))

# Выберем из датафрейма df только переменные Var_One и Var_Thr
df[, c("Var_One", "Var_Thr")]

# Тоже самое через пакет dplyr
# library(dplyr)
# df %>%
#  select(Var_One, Var_Thr)

# ==================================================================
# Задание 6. Сохранение данных из датафрейма в файл
# ==================================================================
write.csv(df, file = "Output/Avakian_df.csv", row.names = TRUE)
