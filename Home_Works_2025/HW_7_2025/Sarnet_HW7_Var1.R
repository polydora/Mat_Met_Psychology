#Установка пакетов
install.packages("readxl")
install.packages("ggplot2")

library(readxl)
library(ggplot2)

#Загружаем данные
data <- read_excel("data_krestiki_noliki.xlsx")

#Считаем крестики и нолики
count_symbols <- function(text, symbol) {
  chars <- strsplit(as.character(text), "")[[1]]
  sum(chars == symbol)}

#Применяем функцию ко всем строкам
data$Крестики <- sapply(data$Вид, count_symbols, symbol = "x")
data$Нолики <- sapply(data$Вид, count_symbols, symbol = "0")

#Проверяем
head(data)

#Линейная модель
model <- lm(Крестики ~ Нолики + Температура, data = data)
summary(model)

#ГРАФИК 1: Крестики vs Нолики
plot1 <- ggplot(data, aes(x = Нолики, y = Крестики)) +
  geom_point(color = "blue", size = 3) +
  geom_smooth(method = "lm", color = "red", se = FALSE) +
  labs(title = "Крестики от Ноликов",
       x = "Количество ноликов",
       y = "Количество крестиков") +
  theme_bw()

print(plot1)

#ГРАФИК 2: Крестики - Температура
plot2 <- ggplot(data, aes(x = Температура, y = Крестики)) +
  geom_point(color = "green", size = 3) +
  geom_smooth(method = "lm", color = "red", se = FALSE) +
  labs(title = "Крестики от Температуры",
       x = "Температура (°C)",
       y = "Количество крестиков") +
  theme_bw()

print(plot2)

#ГРАФИК 3: Оба фактора вместе
plot3 <- ggplot(data, aes(x = Нолики, y = Крестики, color = Температура)) +
  geom_point(size = 3) +
  scale_color_gradient(low = "blue", high = "red") +
  labs(title = "Крестики от Ноликов (цвет = температура)",
       x = "Количество ноликов",
       y = "Количество крестиков") +
  theme_bw()

print(plot3)

#Диагностика модели (4 стандартных графика)
par(mfrow = c(2, 2))
plot(model)
par(mfrow = c(1, 1))
