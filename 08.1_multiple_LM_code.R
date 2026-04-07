# ---
# title: "Множественная регрессия"
# author: Марина Варфоломеева, Вадим Хайтов


# ## Пример: птицы в лесах Австралии ###############################
#
# Фрагментация лесных местообитаний - одна из важнейших проблем Австралии.
# От каких характеристик лесного участка зависит обилие птиц во фрагментированных лесных массивах? (Loyn, 1987)

# 56 лесных участков:
#
# - ABUND - обилие птиц
# - AREA - площадь участка
# - YRISOL - год изоляции участка
# - DIST - расстояние до ближайшего леса
# - LDIST - расстояние до ближайшего большого леса
# - GRAZE - пастбищная нагрузка (1-5)
# - ALT - высота над уровнем моря
#
# Пример из кн. Quinn, Keugh, 2002, данные из Loyn, 1987)


# ## Читаем данные
bird <- read.csv("data/loyn.csv")

# Все ли правильно открылось?
str(bird)

# Есть ли пропущенные значения?
colSums(is.na(bird))

# ## Можно ли ответить на вопрос таким методом?
round(cor(bird), 3)


# ## Знакомство с данными #################################
library(car)
pairs(bird)
# - обратите внимание на форму связи между переменными
# - шкалы переменных







# Трансформируем переменные
bird$logAREA <- log(bird$AREA)
bird$logDIST <- log(bird$DIST)
bird$logLDIST <- log(bird$LDIST)



pairs(bird[, c("ABUND", "logAREA", "YRISOL", "logDIST", "logLDIST", "GRAZE", "ALT")])

library(ggplot2)


# диаграмма Кливленда

ggplot(bird, aes(y = 1:nrow(bird), x = ABUND  )) + geom_point() +
  labs(y = 'Порядковый номер \nв датасете', x = 'Значения переменной')




# Продемонстрируем как работает диаграмма Кливленда

bird2 <- bird


bird2$ABUND[4] <- 250 # Например, ошибка в набивке данных


## Диаграмма Кливленда для всех переменных

ggplot(bird2, aes(y = 1:nrow(bird), x = ABUND  )) + geom_point() +
  labs(y = 'Порядковый номер \nв датасете', x = 'Значения переменной')


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


# Условия применимости линейной регрессии #######################

Mod <-



# ### Проверка на мультиколлинеарность ###########################
# ## Задание 1 ------------------------------------------------
#
# - Постройте множественную линейную регрессию для
# зависимости обилия птиц (`ABUND`) от других
# переменных (`logAREA`, `YRISOL`, `logDIST`,
# `logLDIST`, `GRAZE`, `ALT`)
#
# ABUND_i = b_0 + b_1 logAREA_i + b_2 YRISOL_i   +
#               + b_3 logDIST_i + b_4 logLDIST_i +
#               + b_5 GRAZE_i   + b_6 ALT_i      + e_i
#
# - Используйте функцию `vif()`, чтобы проверить,
# коллинеарны ли предикторы.
#
# Дополните код:
names(bird)

mod1 <- lm(formula = ABUND ~ logAREA + YRISOL + logDIST + logLDIST + GRAZE + ALT, data = bird)



vif(mod1)

mod2 <- update(mod1, .~. - GRAZE)

vif(mod2)





# ## Уравнение модели ###################################################
# Модель, с которой мы теперь работаем
# ABUND_i = b_0 + b_1 logAREA_i + b_2 YRISOL_i   +
#               + b_3 logDIST_i + b_4 logLDIST_i +
#               + b_5 ALT_i     + e_i



# ## Задание 2 ------------------------------------------------------------
# Проверьте, выполняются ли условия применимости
# для модели `mod2`. Дополните код:
library()
mod2_diag <- data.frame(fortify(mod2), bird$GRAZE)
# 1) График расстояния Кука


ggplot(data = mod2_diag, aes(x = 1:nrow(mod2_diag), y = .cooksd)) + geom_bar(stat = "identity")

ggplot(data = mod2_diag, aes(x = 1:nrow(mod2_diag), y = .cooksd)) + geom_col()



# 2) График остатков от предсказанных значений
gg_resid <- ggplot(data = mod2_diag, aes(x = .fitted, y = .stdresid)) + geom_point() + geom_hline(yintercept = 0) + geom_smooth(method = "loess")
gg_resid

names(bird)

# 3) Графики остатков от предикторов в модели и нет
res_1 <- gg_resid + aes(x = logAREA)
res_1
res_2 <- gg_resid + aes(x = YRISOL)
res_3 <- gg_resid + aes(x = logDIST)
res_4 <- gg_resid + aes(x = logLDIST)
res_5 <- gg_resid + aes(x = ALT)
res_6 <- ggplot(data = mod2_diag, aes(x = bird.GRAZE, y = .stdresid)) + geom_point() + geom_hline(yintercept = 0) + geom_smooth(method = "loess")

# все графики вместе
library(gridExtra)
grid.arrange(res_1, res_2, res_3, res_4, res_5, res_6, nrow = 2)


# 4) Квантильный график остатков
library(car)
qqPlot(mod2)



# # Сравнение силы влияния разных предикторов #################################
summary(mod2)


# ## Какой из предикторов оказывает наиболее сильное влияние?
coef(summary(mod2))

# ## Какой из предикторов оказывает наиболее сильное влияние?
mod2_scaled <- lm(ABUND ~ scale(logAREA) + scale(YRISOL) + scale(logDIST) +
                          scale(logLDIST) + scale(ALT), data = bird)

summary(mod2_scaled)

coef(summary(mod2_scaled))






