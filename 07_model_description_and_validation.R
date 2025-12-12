
library("tidyverse")
library("cowplot")
library("ggplot2")

## Код из прошлой лекции #################################

## Открываем данные
library(readxl)
brain <- read.csv("data/IQ_brain.csv", header = TRUE)

## Линейная модель
brain_model <- lm(PIQ ~ MRINACount, data = brain)
summary(brain_model)


library(ggplot2)
theme_set(theme_bw(base_size = 22))
ggplot(brain, aes(x = MRINACount, y = PIQ)) +
  geom_point() +
  geom_smooth(method = "lm")


summary(brain_model)




dat <- read.table('data/orly_owl_Lin_4p_5_flat.txt')
fit <- lm(V1 ~ V2 + V3 + V4 + V5, data = dat)
summary(fit)



library(car)
residualPlot(fit, pch = ".")


library(ggplot2)
brain_diag <- fortify(brain_model)
head(brain_diag, 2)


gg_resid <- ggplot(data = brain_diag, aes(x = .fitted, y = .stdresid)) +
  geom_point() +
  geom_hline(yintercept = 0)
gg_resid





library(car)

qqPlot(brain_model, id = FALSE) # из пакета car




