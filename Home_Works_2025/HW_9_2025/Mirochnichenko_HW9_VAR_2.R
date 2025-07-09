# setwd("C:/Mat_Met_Biology")

data("iris")
str(iris)

library(dplyr)
library(ggplot2)

theme_set(theme_bw())

df_versicolor <-
  iris %>% 
  filter(Species == "versicolor")

colSums(is.na(df_versicolor))


## Точечный график

versi_plot <- 
  ggplot(data = df_versicolor, aes(x = Petal.Length, y = Sepal.Length)) +
  geom_point()


## Вычисление коэф корреляции

cor(x=df_versicolor$Petal.Length, y=df_versicolor$Sepal.Length)

cor.test(x=df_versicolor$Petal.Length, y=df_versicolor$Sepal.Length)

qt(p = 0.975, df = (nrow(df_versicolor) - 1))

### t в соr тесте больше чем t фактич, cледовательно нулевая гипотеза отвергается


## Линейная модель

versi_model <- lm(formula = Sepal.Length ~ Petal.Length, data = df_versicolor)

residuals(versi_model)
hist(residuals(versi_model))

summary(versi_model)
### Sepal.Length = 2.4075 + 0.8283 * Petal.Length
### Значения статистически значимы, Sepal зависит от Petal


## Анализ валидности модели

### 1. Независимость наблюдений

### 2. Нормальность распределений

library(car)
qqPlot(versi_model) ### отклонений в норм распределении нет

### 3. Гомогенность дисперсии

diagn_df <- fortify(versi_model)

ggplot(diagn_df, aes(x = .fitted, y = .stdresid)) +
  geom_point() +
  geom_hline(yintercept = 0)


### Линейность связи
ggplot(diagn_df, aes(x = .fitted, y = .stdresid)) +
  geom_point() +
  geom_hline(yintercept = 0) +
  geom_smooth(method = "loess")


## Линия регрессии с 95% доверительной областью 

pl_alpha <-
  versi_plot +
  geom_smooth(method="lm", level=0.95) +
  ggtitle(bquote(alpha==0.05))


## Предсказания

versi_predicted_99 <- predict(versi_model, interval = "prediction", level = 0.99, se = TRUE)
versi_predicted_99_1 <- data.frame(df_versicolor, versi_predicted_99)
head(versi_predicted_99_1)

versi_predicted_30 <- predict(versi_model, interval = "prediction", level = 0.3, se = TRUE)
versi_predicted_30_1 <- data.frame(df_versicolor, versi_predicted_30)
head(versi_predicted_30_1)


## График для предсказания 99%

versi_plot +
  geom_smooth(method = "lm", aes(fill = "Conf.interval"), alpha = 0.4) +
  geom_ribbon(data = versi_predicted_99_1,  aes(ymin = fit.lwr, ymax = fit.upr, fill = "Conf. area for prediction"), alpha = 0.2) +
  scale_fill_manual(name = "Intervals", values = c("purple", "green")) +
  ggtitle("Confidence interval \n and confidence area for prediction")




