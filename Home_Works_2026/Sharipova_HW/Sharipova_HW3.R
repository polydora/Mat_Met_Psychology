# ====================Домашнее задание №3.====================
# Вариант 2.

# ### ДЗ часть 1
data(ChickWeight)
library(dplyr)

chick_day21 <- ChickWeight %>% filter(Time == 21)

stats <- chick_day21 %>%
  group_by(Diet) %>%
  summarise(
    Minimum = min(weight),
    Q1 = quantile(weight, 0.25),
    Median = median(weight),
    Q3 = quantile(weight, 0.75),
    Maximum = max(weight)
  )

print(stats)

# ### ДЗ часть 2
library(ggplot2)

ggplot(chick_day21, aes(x = as.factor(Diet), y = weight, fill = as.factor(Diet))) +
  geom_boxplot() +
  stat_summary(fun = median, geom = "point", shape = 18, size = 3, color = "black") +
  scale_fill_manual(values = c("1" = "magenta", "2" = "red", "3" = "cyan", "4" = "yellow")) +
  labs(title = "Вес цыплят на 21-й день наблюдения", y = "Вес (г)", x = "Диета") +
  theme_minimal()