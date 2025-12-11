# Домашнее задание 6 - Двухфакторный дисперсионный анализ (Задача про пингвинов)
# Авакян Кристина
# Группа: ББ5А23/10

# Вариант 1. Пингвины 

# Измерения особей пингвинов из рода Pygoscelis лежат в датасете `penguins` в пакете `palmerpenguins`.
# Исходные данные были опубликованы в работе Gorman et al., 2014.
# Помимо веса и пола животных, датасет содержит информацию об острове,
# на котором пингвины проживали, и измерения клюва.

# В анализ мы возьмём только следующие переменные:
#
# Зависимая переменная:
#
#   - `body_mass_g` --- вес в граммах
#
# Факторы:
#
#   - `species` --- вид пингвина
#   - `sex` --- пол пингвина

# ==================================================================
# Двухфакторный дисперсионный анализ
# ==================================================================

# Устанавливаем пакет palmerpenguins при необходимости
# install.packages("palmerpenguins")

# Открываем данные
library(palmerpenguins)
peng <- as.data.frame(penguins[, c(1, 6, 7)])
str(peng)

# Переименовываем столбцы
colnames(peng) <- c('sp', 'mass', 'sex')

# Загружаем пакеты dplyr и ggplot2
library(dplyr)
library(ggplot2)

# Очищаем данные от NA
peng <- peng %>%
  filter(!is.na(sp), !is.na(sex), !is.na(mass)) %>%
  mutate(sp  = factor(sp), sex = factor(sex))

# Проверим размеры групп
table(peng$sp, peng$sex)

# Смотрим, что есть наблюдения во всех комбинациях вид x пол.
# Если где-то очень мало - интерпретация взаимодействия будет ненадёжной.


# ================================================================
# 1. Визуальная разведка
# ================================================================
ggplot(peng, aes(sp, mass, fill = sex)) +
  stat_summary(fun = mean, geom = "col",
               position = position_dodge(width = 0.9), width = 0.7) +
  stat_summary(fun.data = mean_cl_normal, geom = "errorbar",
               position = position_dodge(width = 0.9), width = 0.2) +
  labs(title = "Средняя масса пингвинов по видам и полу",
       subtitle = "Столбцы — средние, усы - 95% ДИ",
       x = "Вид (sp)", y = "Масса (г)", fill = "Пол") +
  theme_minimal()

# На этом графике заранее видно, какие группы тяжелее/легче,
# и есть ли намёк на взаимодействие (например, разница полов
# неодинакова у разных видов).

# ================================================================
# 2. Модель двухфакторной ANOVA
# ================================================================
anova_2w <- aov(mass ~ sp * sex, data = peng)
summary(anova_2w)

# В выводе три ключевых строки:
# sp       - главный эффект вида (различаются ли средние массы видов)
# sex      - главный эффект пола (различаются ли средние массы полов)
# sp:sex   - взаимодействие (зависит ли эффект пола от вида)
#
# Если sp:sex значим, то интерпретацию главных эффектов делаем осторожно:
# основной смысл - в различиях между конкретными комбинациями.
#
# Вид (sp): F-value = 758.36, p-value = < 2e-16
#           Отвергаем H0: средняя масса пингвинов сильно различается между видами.
#           Это очень мощный эффект: вид объясняет большую часть вариации веса.
#
# Пол (sex): F-value = 387.46, p-value = < 2e-16
#           Отвергаем H0: самцы и самки в среднем отличаются по массе.
#
# Взаимодействие (sp:sex): F-value = 8.757, p-value = 0.000197
#                          Это ключевой момент. Взаимодействие ЗНАЧИМО, то есть:
#                          эффект пола зависит от вида.
# 
# Разница "самцы – самки" не одинаковая у разных видов.
# Например, у одного вида половая разница может быть большой, у другого — меньше.

# Поэтому главные эффекты обсуждать нельзя,
# а post-hoc тесты делаем только для взаимодействия

# ================================================================
# 3. Проверка предпосылок применимости ANOVA
# ================================================================

# Загружаем пакет car
library(car)

# Проверяем отсутствие гетероскедастичности
df_diagn <- fortify(anova_2w)

# Строим график
ggplot(df_diagn, aes(x = .fitted, y = .stdresid)) + 
  geom_point() +
  geom_hline(yintercept = 0)

# Визуально на графике гетероскедастичность отсутствует

# Проверяем нормальность распределения
qqPlot(anova_2w)

# Распределение по квантильному графику подтверждается

# Дополнительно формально проверяем нормальность распределения через тест Шапиро 
res <- residuals(anova_2w)
shapiro.test(res)

# РЕЗУЛЬТАТ:
# W = 0.99776, p-value = 0.9367

# ВЫВОД:
# p = 0.9367 > 0.05, следовательно, распределение остатков не отличается от нормального.
# Предпосылка нормальности выполнена.

# ================================================================
# 4. Post-hoc сравнения
# ================================================================

# Взаимодействие (sp:sex): F-value = 8.757, p-value = 0.000197
# Это ключевой момент.
# Взаимодействие ЗНАЧИМО, то есть: эффект пола зависит от вида.

# Делаем Tukey по комбинациям sp x sex

peng <- peng %>% 
  mutate(group = interaction(sp, sex))

anova_groups <- aov(mass ~ group, data = peng)
tuk_groups <- TukeyHSD(anova_groups)
tuk_groups

# Здесь мы сравниваем каждую комбинацию.
# Так правильно интерпретировать при значимом взаимодействии.

# =======================
# НЕ ЗНАЧИМЫЕ результаты
# =======================
#
# 1. Разница между видами самок Chinstrap.female – Adelie.female статистически неразличима.
#    Так как p_adj = 0.138, diff = 158
#    Все остальные межвидовые сравнения самок - статистически значимы.
#
# 2. Разница между видами самцов Chinstrap.male – Adelie.male статистически неразличима.
#    Так как p_adj = 0.581, diff = -105
#    Все остальные межвидовые сравнения самцов - статистически значимы.
#
# =======================
# ЗНАЧИМЫЕ результаты
# =======================
# 
# 1. Gentoo - самый тяжёлый вид (и самцы, и самки)
#    Сравнения Gentoo с любыми другими группами дают огромные diff и p_adj = 0:
#        Gentoo.female  – Adelie.female:     diff = 1311, p_adj = 0
#        Gentoo.female  – Chinstrap.female:  diff = 1153, p_adj = 0
#        Gentoo.male    – Adelie.male:       diff = 1441, p_adj = 0
#        Gentoo.male    – Chinstrap.male:    diff = 1546, p_adj = 0
#        Gentoo.male    – Adelie.female:     diff = 2116, p_adj = 0
#        Gentoo.male    – Chinstrap.female:  diff = 1958, p_adj = 0
#        Adelie.male    - Gentoo.female      diff = -636, p_adj = 0
#        Chinstrap.male - Gentoo.female      diff = -741, p_adj = 0
# 
#    Gentoo значительно тяжелее двух других видов независимо от пола.
#
# 2. Половые различия внутри каждого вида (это и есть "взаимодействие" sp x sex)
#    Смотри строки (male - female) внутри одного вида:
#        Adelie.male – Adelie.female:       diff = 675, p_adj = 0 (самцы тяжелее самок)
#        Chinstrap.male – Chinstrap.female: diff = 412, p_adj = 1.2e-06  (самцы тяжелее самок)
#        Gentoo.male – Gentoo.female:       diff = 805, p_adj = 0 (самцы тяжелее самок)
#
#    Разница между полами есть у всех видов, но её величина разная
#    (Adelie ~0.68 кг, Chinstrap ~0.41 кг, Gentoo ~0.81 кг).
#    Это ровно то, что ANOVA тест поймал как значимое взаимодействие sp x sex.

# ================================================================
# 5. Графическое сравнение
# ================================================================

# Загружаем пакеты dplyr и ggplot2
library(dplyr)
library(ggplot2)

# Считаем средние значения и 95% ДИ для 6 групп
sum_df <- peng %>%
  group_by(sp, sex) %>%
  summarise(mean_mass = mean(mass),
            sd_mass   = sd(mass),
            n         = n(),
            se        = sd_mass / sqrt(n),
            t_crit    = qt(0.975, df = n - 1),
            ci_low    = mean_mass - t_crit * se,
            ci_up     = mean_mass + t_crit * se,
            .groups = "drop") %>%
  mutate(group = interaction(sp, sex, sep = "."))

# Задаем порядок столбцов по оси X
levels_order <- c("Adelie.female","Adelie.male",
                  "Chinstrap.female","Chinstrap.male",
                  "Gentoo.female","Gentoo.male")
# Задаем факторы
sum_df$group <- factor(sum_df$group, levels = levels_order)

# Устанавливаем числовые позиции групп для скобок
pos <- setNames(seq_along(levels_order), levels_order)

# Формируем Tukey результаты и p-метки
tuk_df <- as.data.frame(tuk_groups$group) %>%
  tibble::rownames_to_column("comparison") %>%
  mutate(group1 = sub("-.*", "", comparison),
         group2 = sub(".*-", "", comparison),
         p_adj  = `p adj`,
         p_label = case_when(p_adj < 0.001 ~ "p < 0.001",
                             p_adj < 0.01  ~ "p < 0.01",
                             p_adj < 0.05  ~ "p < 0.05",
                             TRUE          ~ "различия незначимы"))

# Задаем порядок скобок на графике по оси Y
order_pairs <- c("Adelie.male-Adelie.female",
                 "Chinstrap.male-Chinstrap.female",
                 "Gentoo.male-Gentoo.female",
                 "Gentoo.female-Chinstrap.female",
                 "Gentoo.female-Adelie.female",
                 "Gentoo.male-Chinstrap.male",
                 "Gentoo.male-Adelie.male")

# Формируем данные для скобок
brackets_df <- tuk_df %>%
  filter(comparison %in% order_pairs) %>%
  mutate(order_id = match(comparison, order_pairs)) %>%
  arrange(order_id)

# Параметры высоты расположения скобок и шаг между ними по Y
y_base <- max(sum_df$ci_up)
step   <- 400

# Создаем позиции для скобок
brackets_df <- brackets_df %>%
  mutate(x1 = pos[group1],
         x2 = pos[group2],
         y  = y_base + step * row_number(),
         h  = 100,
         # Определяем тип пары для цвета скобок
         bracket_sex = case_when(grepl("\\.female$", group1) & grepl("\\.female$", group2) ~ "female",
                                 grepl("\\.male$",   group1) & grepl("\\.male$",   group2) ~ "male",
                                 TRUE ~ "mixed"))

# Создаем график
sum_df %>%
  ggplot(aes(x = group, y = mean_mass, fill = sex)) +
  geom_col(width = 0.65, color = "black") +
  geom_errorbar(aes(ymin = ci_low, ymax = ci_up), width = 0.15) +

  # скобки (цветём по bracket_sex)
  geom_segment(data = brackets_df,
               aes(x = x1, xend = x2, y = y, yend = y, color = bracket_sex),
               inherit.aes = FALSE, linewidth = 0.6) +
  geom_segment(data = brackets_df,
               aes(x = x1, xend = x1, y = y, yend = y - h, color = bracket_sex),
               inherit.aes = FALSE, linewidth = 0.6) +
  geom_segment(data = brackets_df,
               aes(x = x2, xend = x2, y = y, yend = y - h, color = bracket_sex),
               inherit.aes = FALSE, linewidth = 0.6) +
  geom_text(data = brackets_df,
            aes(x = (x1 + x2)/2, y = y + 100, label = p_label, color = bracket_sex),
            inherit.aes = FALSE, size = 4, vjust = 0.2) +
  
  # цвета столбцов по полу
  scale_fill_manual(values = c(female = "#f4a3c0", male = "#8ec5ff"),
                    name = "Пол") +
  
  # цвета скобок
  scale_color_manual(values = c(female = "#d95b8a",
                                male   = "#3b82f6",
                                mixed  = "black"),
                     guide = "none") +
  
  labs(title = "Средняя масса пингвинов по видам и полу",
       subtitle = "Столбцы - средние значения, усы - 95% ДИ.\nСкобки - выбранные сравнения Tukey.",
       x = "Группа (вид и половая принадлежность)",
       y = "Масса") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 25, hjust = 1)) -> mean_mass_plot

# Показываем график
mean_mass_plot

# Сохраняем график
# ggsave("Images/Avakian_Penguins.jpg", plot = mean_mass_plot)
