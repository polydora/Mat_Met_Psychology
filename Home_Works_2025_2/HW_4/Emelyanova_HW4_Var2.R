## ДЗ 4 ##

### Представьте себе, что вы идете по лесам Средиземья с отрядом эльфов. Все участники похода были независимо и случайно отобраны из числа этого народа. И вдруг, вдалеке, вы заметили отряд неких существ. Вам необходимо принять решение о том, свои это или чужие. Вы (с помощью магии, наверное) смогли как-то измерить рост фигур из этого отряда. Сделайте обоснованный вывод на основе данных, которые у вас имеются. Результаты необходимо описать в виде текста (комментарии в скрипте с решением, в котором надо сформулировать нулевую и альтернативную гипотезы и сделать выбор между ними) и визуализировать в виде столбчатой диаграммы (необходимо написать код, который ее построит).####

## Вариант 2.##
## Создадим векторы с данными роста ###

elves <- c(179, 183, 173, 180, 174, 184, 180, 180, 185, 183, 171, 177, 179, 187, 176)
strangers <- c(195, 184, 186, 194, 191, 194)

# Статистические гипотезы ##
# H0: mu_strangers = mu_elves  (средние равны; "свои") 
# H1: mu_strangers != mu_elves (средние различаются; "чужие")

# Опишем основные параметры векторов ##
mean_elves <- mean(elves) 
sd_elves <- sd(elves)
n_elves <- length(elves)
mean_strangers <- mean(strangers)
sd_strangers <- sd(strangers)
n_strangers <- length(strangers)

## Проверим условие нормального распределения с помощью квантильного графика####
## Запустим бибилиотеку car###

library(car)
qqPlot(elves, id = FALSE)
qqPlot(strangers, id = FALSE)

## По квантильным графикам делаем вывод, что оба распределения нормальны ##
## Сделаем двухвыборочный тест с помощью функции ttest##

tt <- t.test(strangers, elves, alternative = "two.sided", var.equal = FALSE)
tt

## Вывод: так как p-value = 0.000649  и это меньше критического уровня значимости, мы можем отвергнуть нулевую гипотезу и сделать вывод, что первая группа статистически значимо отличается от второй, а значит это чужие ##

## Визуализация ##
## Запустим нужные библиотеки ##

library(ggplot2)
library(dplyr)

## Создадим датафрейм с данными ##

df <- tibble(
  group = rep(c("Эльфы", "Незнакомцы"), times = c(length(elves), length(strangers))),
  height = c(elves, strangers)
)

## Построим столбчатую диаграмму со значениями роста (это я сделала самстоятельно)##

ggplot(df, aes(x = height)) +
  geom_histogram(binwidth = 0.5)

## Визуализация - код написан с помощью GPT5 ##

# Сводка: среднее и 95% ДИ (mean ± t * SE)
summary_df <- df %>%
  group_by(group) %>%
  summarise(
    n = n(),
    mean = mean(height),
    sd = sd(height),
    se = sd/sqrt(n),
    tcrit = qt(0.975, df = n - 1),
    ymin = mean - tcrit * se,
    ymax = mean + tcrit * se,
    .groups = "drop"
  )

# График: столбцы со средними, 95% ДИ (доверительный интервал) и точки наблюдений
ggplot(summary_df, aes(x = group, y = mean, fill = group)) +
  geom_col(width = 0.6, color = "black", show.legend = FALSE) +
  geom_errorbar(aes(ymin = ymin, ymax = ymax), width = 0.15, size = 0.9) +
  geom_jitter(
    data = df,
    aes(x = group, y = height),
    width = 0.08, height = 0, alpha = 0.7, size = 2, color = "gray25", inherit.aes = FALSE
  ) +
  labs(
    x = NULL,
    y = "Рост, см",
    title = "Средний рост и 95% ДИ: эльфы vs незнакомцы"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    panel.grid.minor = element_blank(),
    axis.text.x = element_text(size = 10)
  )