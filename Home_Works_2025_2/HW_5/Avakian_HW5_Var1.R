# Домашнее задание 5 - Однофакторный дисперсионный анализ (Задача про Гидрологические данные)
# Авакян Кристина
# Группа: ББ5А23/10

# Вариант 1. Анализ солености (переменная S)

# ЗАДАНИЕ:
# 1. Необходимо выяснить наблюдаются ли различия между тремя летними месяцами 
#    (нужно будет провести дисперсионный анализ) в том параметре, котоый будет у вашего варианта.
# 2. Проведите анализ остатков и оцените, выполняются ли условия применимости ANOVA.
# 3. Визуализируйте с помощью столбчатой диаграммы (с доверительными интервалами) средние значения.
#    Проведя post-hoc сравнение средних, обозначьте на рисунке те средние,
#    которые отличаются друг от друга по критерию Тьюки.

# ==================================================================
# 1. Однофакторный дисперсионный анализ
# ==================================================================

# Устанавливаем пакет readxl при необходимости
# install.packages("readxl")

# Подключаем пакет readxl
library(readxl)

# Загружаем таблицу
hydrology_data <- read_excel("Data/hydrology_2022.xls")

# Смотрим структуру данных
head(hydrology_data)
str(hydrology_data)

# Загружаем пакеты dplyr
library(dplyr)

# Подготовим данные для работы
hydrology <- hydrology_data %>%
  select(Month, S) %>%                                    # Оставляем нужные столбцы
  mutate(Month = na_if(Month, ""),                        # Убираем пустые строки (""), если вдруг есть
         S = na_if(S, ""),                                # Убираем пустые строки (""), если вдруг есть
         S = na_if(S, "NA")) %>%                          # Убираем "NA" строки, если вдруг есть
  filter(!is.na(Month), !is.na(S)) %>%                    # Фильтруем строки с NA в Month или S
  mutate(Month = factor(Month), S = as.numeric(S))        # Месяц делаем фактором, а соленость числом

# Проверяем структуру
str(hydrology)
head(hydrology)
summary(hydrology)

# Информация
# Month        S        
# August: 79   Min.   :11.00
# July  :113   1st Qu.:15.00
# June  : 95   Median :16.00
#              Mean   :16.21  
#              3rd Qu.:17.50  
#              Max.   :23.00

# Анализируем количество наблюдений
# По месяцам выборка немного несбалансирована, но не критично.
# Такой дисбаланс обычно допустим, так как размеры групп близкие.

# Представим данные графически
library(ggplot2)

# Строим график
hydrology %>% 
  ggplot(aes(x=Month, y=S))+
  geom_boxplot() +
  geom_point(position = position_jitter(width=0.08)) +
  labs(title = "Соленость воды по месяцам",
       x = "Месяц",
       y = "Соленость (S)")

# Построим две модели и сформулируем гипотезу
# НУЛЕВАЯ ГИПОТЕЗА (H0): Cредние значения солености S одинаковы во всех месяцах (June, July, August).

# ANOVA модель через aov
anova_model_1 <- aov(S ~ Month, data = hydrology)
summary(anova_model_1)

# ANOVA модель через линейную модель
linear_model <- lm(S ~ Month, data = hydrology) # Линейная модель 
anova_model_2 <- anova(linear_model)
anova_model_2

# Получаем наблюдаемые значения F-распределения и p-value
F_observ <- anova_model_2["Month", "F value"]
p_observ <- anova_model_2["Month", "Pr(>F)"]

# Получаем число степеней свободы
df_month <- anova_model_2["Month", "Df"]
df_resid <- anova_model_2["Residuals", "Df"]

# Получаем квантиль F-распределения для 95%, то есть F_crit 
p_crit <- 0.05
F_crit <- qf(p=(1-p_crit), df1=df_month, df2=df_resid)

# Делаем проверку и выводим ПРЕДВАРИТЕЛЬНЫЙ результат
if (F_observ > F_crit && p_observ < p_crit) {
  cat(
    "ПРЕДВАРИТЕЛЬНЫЙ РЕЗУЛЬТАТ\n",
    "Отвергаем H0: средние солёности по месяцам различаются.\n",
    "F_observ =", round(F_observ, 3), 
    ">", "F_crit =", round(F_crit, 3), "\n",
    "p_observ =", format(p_observ, scientific = TRUE, digits = 3),
    "<", "p_crit =", p_crit, "\n"
  )
} else {
  cat(
    "ПРЕДВАРИТЕЛЬНЫЙ РЕЗУЛЬТАТ\n",
    "Нет оснований отвергать H0: статистически значимых различий нет.\n",
    "F_observ =", round(F_observ, 3), 
    "<=", "F_crit =", round(F_crit, 3), "\n",
    "p_observ =", format(p_observ, scientific = TRUE, digits = 3),
    ">=", "p_crit =", p_crit, "\n"
  )
}

# ==================================================================
# 2. Анализ остатков и проверка условий применимости ANOVA
# ==================================================================

# Загружаем пакет ggplot2
library(ggplot2)

# Пробуем формировать данные для анализа остатков
# residue_analysis_data <- fortify(linear_model)
# Иногда появляется сообщение о устаревшей функции:
#
# Warning message:
#   `fortify(<lm>)` was deprecated in ggplot2 4.0.0.
# Please use `broom::augment(<lm>)` instead.
#
# поэтому воспользуемся в дальнейшем broom::augment(<lm>)

# Формируем данные для анализа остатков
residue_analysis_data <- broom::augment(linear_model)

# Если необходимо выводим средние значения по месяцам
# unique(residue_analysis_data$.fitted)

# Показываем график остатков от предсказанных значений
residue_analysis_data %>%
  ggplot(aes(x = .fitted, y = .std.resid)) +
  geom_point(alpha = 0.6) +
  geom_hline(yintercept = 0, linetype = 2) +
  labs(title = "График стандартизированных остатков от предсказанных значений",
       x = "Fitted values (Средние значения по месяцам)", 
       y = "Std. Residuals (Стандартизированные остатки)")

# Показываем график остатков от месяца
residue_analysis_data %>%
  ggplot(aes(x = Month, y = .std.resid)) +
  geom_boxplot()+
  geom_hline(yintercept = 0, linetype = 2) +
  labs(title = "График стандартизированных остатков от месяца",
       x = "Месяц", 
       y = "Стандартизированные остатки")

# Учитывая определения, что 
#
# Норма (гомоскедастичность): Облако точек примерно одинаковой высоты по всей оси X - «ровная полоса».
#
# Гетероскедастичность: виден «раскрывающийся веер» или «воронка»:
# - слева узко, справа широко (или наоборот);
# - дисперсия скачет ступеньками по X.
#
# Можно сказать, что гетероскедастичность присуща текущим данным, но её величина невысока, 
# поэтому предпосылки равенства дисперсий нарушены слабо.
#
# Классическая однофакторная ANOVA довольно устойчива к умеренной гетероскедастичности, особенно в данном случае:
# 1. Группы по размеру схожи (79 - 113 измерений)
# 2. Нет сильных выбросов
# 3. Различия между средними большие (F_observ >> F_crit, мы находимся не на критической границе)

# ВЫВОД: Небольшое нарушение не влияет на основной вывод о различиях солености между месяцами.

# ==================================================================
# Проведем дополнительную проверку на нормальность распределения
# ==================================================================

# Загружаем пакет car
library(car)

# Показываем график
qqPlot(residue_analysis_data$.std.resid)

# ГРАФИЧЕСКИ: Нормальность распределения нарушается на концах графика

# Проведем более формальные тесты нормальности остатков

# Тест Shapiro-Wilk
shapiro.test(residuals(linear_model)) # p-value = 0.001538

# Тест Anderson-Darling, который чувствительнее к хвостам графика
# install.packages("nortest") # Нужен пакет nortest
library(nortest)
ad.test(residuals(linear_model)) # p-value = 1.58e-05

# Анализируем формальные тесты
# Shapiro–Wilk: p-value = 0.0015
# Anderson–Darling: p-value = 1.58e-05 (ещё строже, особенно к хвостам)

# ФОРМАЛЬНО ПО ЧИСЛАМ: Строго по тестам нормальность отвергается.
#
# Но при большом размере выборки (287 остатков) тесты очень чувствительные,
# они могут определить даже небольшие отклонения.

# Можно подстраховаться и провести Welch ANOVA Test (более робастный к нарушениям) 
oneway.test(S ~ Month, data = hydrology, var.equal = FALSE)

# 	One-way analysis of means (not assuming equal variances)
#
# data:  S and Month
# F = 45.652, num df = 2.00, denom df = 141.04, p-value = 5.153e-16

# ВЫВОД:
# - Welch ANOVA не требует равенства дисперсий и менее чувствительна к ненормальности/гетероскедастичности.
# - Welch ANOVA подтверждает вывод обычного теста ANOVA и даже даёт ещё более сильную значимость.

# ==================================================================
# 3. Post-hoc Тьюки
# ==================================================================


# Post-hoc Тьюки
tukey <- TukeyHSD(anova_model_1)

# $Month
#                   diff       lwr        upr     p adj
# July-August -2.3820992 -3.150362 -1.6138368 0.0000000    <- ноль не перекрывается, значит H1
# June-August -0.2826116 -1.080259  0.5150359 0.6817392    <- ноль перекрывается, значит H0
# June-July    2.0994877  1.370295  2.8286805 0.0000000    <- ноль не перекрывается, значит H1

# Соленость воды в разные месяцы различается (тест Тьюки, p < 0.05).

# Подключаем ggplot2, dplyr, tidyr
library(ggplot2)
library(dplyr)
library(tidyr)

# Вычисляем средние значения и 95% ДИ
sum_df <- hydrology %>%
  group_by(Month) %>%
  summarise(mean_S = mean(S, na.rm = TRUE),
            sd_S   = sd(S, na.rm = TRUE),
            n      = n(),
            se     = sd_S / sqrt(n),
            t_crit = qt(0.975, df = n - 1),
            ci_low = mean_S - t_crit * se,
            ci_up  = mean_S + t_crit * se,
            .groups = "drop")

# Превращаем данные Tukey в датафрейм попарных сравнений
tuk_df <- as.data.frame(tukey$Month) %>%
  tibble::rownames_to_column("comparison") %>%
  separate(comparison, into = c("group1", "group2"), sep = "-") %>%
  rename(p_adj = `p adj`) %>%
  mutate(p_label = case_when(p_adj < 0.001 ~ "высокая стат.значимость", 
                             p_adj < 0.01  ~ "средняя стат.значимость",
                             p_adj < 0.05  ~ "низкая стат.значимость",
                             TRUE          ~ "различия незначимы"))

# Формируем данные для попарных скобок
y_base <- max(sum_df$ci_up + 1.5) # Отступ от столбцов
step   <- 2                     # расстояние между скобками

# Формируем скобки
brackets_df <- tuk_df %>%
  arrange(p_adj) %>%  # можно по значимости
  mutate(y_position = y_base + step * row_number())

# График скобок есть в пакете ggsignif
# install.packages("ggsignif") # установим при необходимости
# Загружаем пакет ggsignif
library(ggsignif)

# Формируем результирующий график
ggplot(sum_df, aes(x = Month, y = mean_S)) +
  geom_col(width = 0.6, fill = "grey", color = "black") +            # Столбцы
  geom_errorbar(aes(ymin = ci_low, ymax = ci_up), width = 0.15) +    # ДИ
  geom_signif(data = brackets_df,                                    # Скобки для попарного сравнения Tukey
              aes(xmin = group1,
                  xmax = group2,
                  annotations = p_label,
                  y_position = y_position),
              manual = TRUE,
              inherit.aes = FALSE,
              textsize = 4,
              vjust = 0.2,
              tip_length = 0.05) +
  labs(title = "Средняя соленость по месяцам",
       subtitle = "Столбцы - средние значения с 95% ДИ.\nСкобки - Tukey post-hoc сравнение.",
       x = "Месяц",
       y = "Средняя соленость (S)") +
  theme_minimal() -> mean_s_vs_month_plot

# Отображаем график
mean_s_vs_month_plot

# Сохраняем график
# ggsave("Images/Avakian_MeanS_vs_Month.jpg", plot = mean_s_vs_month_plot)
