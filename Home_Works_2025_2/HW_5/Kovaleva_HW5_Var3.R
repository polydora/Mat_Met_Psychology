###  1. Пакеты 
install.packages(c("readxl", "tidyverse", "car", "rstatix", "emmeans"))

library(readxl)
library(tidyverse)
library(car)
library(rstatix)
library(emmeans)

###  2. Загрузка данных 
data <- read_excel("Data/hydrology_2022 (1).xls")

# Приведение типов
data <- data %>%
  mutate(
    Water_T = as.numeric(Water_T),
    Month = factor(Month, levels = c("June", "July", "August"))
  ) %>%
  filter(!is.na(Water_T), Month %in% c("June", "July", "August"))

### 3. ANOVA
res_aov <- aov(Water_T ~ Month, data = data)
summary(res_aov)

# Альтернативный вывод
anova_test(data, Water_T ~ Month)

###  4. Проверка предпосылок 
# Остатки
residuals_aov <- residuals(res_aov)

# Нормальность остатков
shapiro.test(residuals_aov)

# Нормальность по группам
data %>% group_by(Month) %>% shapiro_test(Water_T)

# Гомогенность дисперсий
leveneTest(Water_T ~ Month, data = data)

# Графики остатков
par(mfrow = c(1,2))
plot(res_aov, which = 1)  # остатки vs fitted
plot(res_aov, which = 2)  # QQ-plot
par(mfrow = c(1,1))

###  5. Post-hoc Tukey 
tukey_res <- TukeyHSD(res_aov)
tukey_res

###  6. Построение групп Тьюки через emmeans 
emm <- emmeans(res_aov, ~ Month)

# Получаем буквы групп Tukey
letters_df <- multcomp::cld(emm, adjust = "tukey")
# Чистим лишние пробелы
letters_df$.group <- gsub(" ", "", letters_df$.group)

letters_df

### 7. Статистика для графика 
summary_stats <- data %>%
  group_by(Month) %>%
  summarise(
    mean = mean(Water_T),
    sd = sd(Water_T),
    n = n(),
    se = sd / sqrt(n),
    ci_low = mean - qt(0.975, n - 1) * se,
    ci_high = mean + qt(0.975, n - 1) * se
  ) %>%
  left_join(letters_df %>% select(Month, .group), by = "Month") %>%
  rename(tukey_group = .group)

summary_stats

#Финальный график
ggplot(summary_stats, aes(x = Month, y = mean, fill = Month)) +
  geom_col(width = 0.6, color = "black", show.legend = FALSE) +
  geom_errorbar(aes(ymin = ci_low, ymax = ci_high), width = 0.1) +
  geom_text(aes(label = tukey_group, y = ci_high + 0.3), size = 6) +
  labs(
    title = "Средняя температура воды по летним месяцам",
    subtitle = "Кандалакшский заповедник, 2022",
    x = "",
    y = "Температура воды, °C"
  ) +
  theme_minimal(base_size = 14)

