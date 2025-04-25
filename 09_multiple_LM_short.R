#' title: "Множественная регрессия"
#'
#' 56 лесных участков:
#'
#' - ABUND - обилие птиц
#' - AREA - площадь участка
#' - YRISOL - год изоляции участка
#' - DIST - расстояние до ближайшего леса
#' - LDIST - расстояние до ближайшего большого леса
#' - GRAZE - пастбищная нагрузка (1-5)
#' - ALT - высота над уровнем моря
#'
#' </div>
#'
#' <small>Пример из кн. Quinn, Keugh, 2002, данные из Loyn, 1987)</small>
#'
#' ## Читаем данные
#'

## -------------------------------------------------------------------
bird <- read.table("data/loyn.csv", sep = ",", header = TRUE)

#' Все ли правильно открылось?
#'
## -------------------------------------------------------------------
str(bird)

#'
#' Есть ли пропущенные значения?
#'
## -------------------------------------------------------------------
colSums(is.na(bird))

#'
#'
#'
#' ## Можно ли ответить на вопрос таким методом?
#'
## -------------------------------------------------------------------
cor(bird)


#' # Разведочный анализ данных

library(car)
pairs(bird)

#' Трансформируем переменные
#'
## -------------------------------------------------------------------
bird$logAREA <- log(bird$AREA)
bird$logDIST <- log(bird$DIST)
bird$logLDIST <- log(bird$LDIST)

#'
#' ## Ищем отскоки

ggplot(bird, aes(y = 1:nrow(bird), x = ABUND)) +
  geom_point() +
  labs(y = 'Порядковый номер \nв датасете', x = 'Значения переменной')

#' ## Ищем отскоки: точечные диаграммы Кливленда {.columns-2 .smaller}

ggplot(bird, aes(y = 1:nrow(bird), x = AREA)) +
  geom_point() +
  labs(y = 'Порядковый номер \nв датасете',
       x = 'Значения переменной')

ggplot(bird, aes(y = 1:nrow(bird), x = logAREA)) +
  geom_point() +
  labs(y = 'Порядковый номер \nв датасете',
       x = 'Значения переменной')
#'
#' ## Ищем отскоки: диаграммы Кливленда для всех переменных
#'

gg_dot <- ggplot(bird, aes(y = 1:nrow(bird))) + geom_point() + ylab('index')
Pl1 <- gg_dot + aes(x = ABUND)
Pl2 <- gg_dot + aes(x = YRISOL)
Pl3 <- gg_dot + aes(x = logAREA)
Pl4 <- gg_dot + aes(x = logDIST)
Pl5 <- gg_dot + aes(x = logLDIST)
Pl6 <- gg_dot + aes(x = ALT)
Pl7 <- gg_dot + aes(x = GRAZE)

library(cowplot) # пакет для группировки графиков
theme_set(theme_bw())
plot_grid(Pl1, Pl2, Pl3, Pl4, Pl5, Pl6,
          Pl7, ncol = 3, nrow = 3)

#'
#' ## Проблемы: сильные корреляции между некоторыми предикторами
#'
## ----fig.height=6, fig.width=10, echo=FALSE-------------------------
pairs(bird[, c("ABUND", "logAREA", "YRISOL", "logDIST", "logLDIST", "GRAZE", "ALT")])

#'
## -------------------------------------------------------------------
# Строим модель

mod1 <- lm(formula = ABUND ~  logAREA + YRISOL + logDIST + logLDIST + GRAZE + ALT,
           data = bird)
# Проверяем, есть ли коллинеарность?
library(car)
vif(mod1)


mod2 <- update(mod1, . ~ . - GRAZE)
vif(mod2)

#' Мы подобрали коэффициенты и можем записать уравнение модели.
#'
## -------------------------------------------------------------------
coef(mod2)


#' Проверьте, выполняются ли условия применимости для модели `mod2`. Дополните код:

library(ggplot2)
mod2_diag <- fortify(mod2)
# 2) График остатков от предсказанных значений
gg_resid <- ggplot(data = mod2_diag, aes(x = .fitted, y = .stdresid)) + geom_point() + geom_hline(yintercept = 0)

gg_resid
# 3) Графики остатков от предикторов в модели и нет
res_1 <- gg_resid + aes(x = logAREA)
res_1
res_2 <- gg_resid + aes(x = logDIST)
res_3 <- gg_resid
res_4 <- gg_resid
res_5 <- gg_resid
res_6 <- gg_resid

gg_resid  +
  geom_smooth()

library(car)
qq <- qqPlot(mod2)

summary(mod2)


