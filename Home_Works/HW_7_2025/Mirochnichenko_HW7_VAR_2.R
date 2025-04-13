getwd()
setwd("C:/Mat_Met_Biology")

library(dplyr)
library(ggplot2)

install.packages("writexl")
library(writexl)
install.packages("readxl")
library(readxl)

hydro <-read_excel("data/hydrology_2022.xls", na = "NA")

str(hydro)

hydro$Month <- factor(hydro$Month)

hydro$Air_T <- as.numeric(hydro$Air_T)

str(hydro)

hydro %>%
  filter(!is.na(Air_T)) ->hyd_1


table(hyd_1$Month)

levels(hydro$Month)

hyd_1 %>%
  mutate(Month2 = case_when(Month == 1 ~ "June",
                             Month == 2 ~ "July",
                             Month == 3 ~ "August"
                             )
  ) -> hyd_1

hyd_1$Month <- hyd_1$Month2

hydro %>%
  filter(!is.na(Air_T)) -> hyd_1

hyd_1$Air_T

hyd_1$Month <- factor(hyd_1$Month, labels = c("June", "July", "August"))

ggplot(data = hyd_1, mapping = aes(x = Month, y = Air_T)) +
  geom_point(position = position_jitter(width = 0.1))

hyd_1 %>%
  group_by(Month) %>%
  summarise(Mean = mean(Air_T), sample_n = n(), SE = sd(Air_T)/sqrt(sample_n), t_kr = qt(p = c(0.975), df = (sample_n - 1)), CI_low = Mean - t_kr*SE, CI_up = Mean + t_kr*SE) %>%
  ggplot(aes(x = Month, y = Mean, color = Month)) +
  geom_point() +
  geom_errorbar(aes(ymin = CI_low, ymax = CI_up), width = 0)

nrow(hyd_1)

model_hyd <- aov(formula = (Air_T) ~ Month, data = hyd_1)


summary(model_hyd)

residuals(model_hyd)

library(ggplot2)

hyd_diag <- fortify(model_hyd)

ggplot(hyd_diag, aes(x = .fitted, y = .resid)) +
  geom_point() +
  geom_hline(yintercept = 0)

ggplot(data = hyd_diag, aes(x = Month, y = .resid)) + geom_boxplot() + geom_hline(yintercept = 0)

library(car)
qqPlot(hyd_diag$.resid)
# qqPlot(x = residuals(model_hyd))

## Проверка по тесту Тьюки
TukeyHSD(model_hyd)

## p (july-june) > 0.05 !!, p (august-june) < 0.05, p (august-july) < 0.05

hyd_1 %>%
  group_by(Month) %>%
  summarise(Mean = mean(Air_T), sample_n = n(), SE = sd(Air_T)/sqrt(sample_n), t_kr = qt(p = c(0.975), df = (sample_n - 1)), CI_low = Mean - t_kr*SE, CI_up = Mean + t_kr*SE) %>%
  ggplot(aes(x = Month, y = Mean)) +
  geom_col() +
  geom_errorbar(aes(ymin = CI_low, ymax = CI_up))

