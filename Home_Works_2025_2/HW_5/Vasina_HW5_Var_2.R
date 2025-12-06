library(readxl)
data <- read_excel("./data/hydrology_2022.xls")
str(data)
# Видим, что Air_T это у нас chr. Это только потому, что там встречаются NA
# Сначала превратим в числа, а затем уберем NA
data$Air_T <- as.numeric(data$Air_T)
corr_data <- data[!is.na(data$Air_T),]
corr_data$Month <- factor(corr_data$Month)

anova <- aov(Air_T ~ Month, data = corr_data)
summary(anova)
# P у нас <2e-16, значит температура различима

# 2. Анализ остатков
library(ggplot2)
sl_air <- fortify(anova)

library(car)
qqPlot(sl_air$.resid, id = FALSE)
# Остатки распределены не нормально - в правом хвосте значения выходят за рамки

# График остатков от предсказанных значений
ggplot(data = sl_air, aes(x = .fitted, y = .resid)) + 
  geom_point()
# Признаков гетероскедастичности не наблюдается

# График остатков от значений дискретного предиктора
ggplot(data = sl_air, aes(x = Month, y = .resid)) + geom_boxplot() + geom_hline(yintercept = 0)
# Наибольшая дисперсия наблюдается в Июне, примерно одинаковая в Августе и Июле

# 3 Визиализируем средние значения с доверительными интервалами
library(dplyr)

june <- 
  corr_data %>%
  filter(., Month == 'June')

july <- 
  corr_data %>%
  filter(., Month == 'July')

august <- 
  corr_data %>%
  filter(., Month == 'August')

plot_df <- data.frame(
  groups = c('June', 'July', 'August'),
  mean = c(mean(june$Air_T), mean(july$Air_T), mean(august$Air_T)),
  sd = c(sd(june$Air_T), sd(july$Air_T), sd(august$Air_T))
)

ggplot(plot_df, aes(x = groups, y = mean)) +
  geom_bar(stat = "identity", width = 0.5) +
  geom_errorbar(aes(ymin = mean - sd, ymax = mean + sd), width=0.2) +
  theme_bw()

# Тест Тьюки
sl_Tuk <- TukeyHSD(anova)
sl_Tuk

# Июнь значительно отличается от остальных месяцев (p < 0.05)

plot_df$label <- c("1", "2", "2") # 2 - схожи, 1 - отличается от других

ggplot(plot_df, aes(x = groups, y = mean)) +
  geom_bar(stat = "identity", width = 0.5) +
  geom_errorbar(aes(ymin = mean - sd, ymax = mean + sd), width=0.2) +
  geom_text(aes(label = label, y = mean + sd + 0.5), size = 5) + # буквы сверху
  theme_bw()
