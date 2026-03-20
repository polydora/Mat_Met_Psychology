# Задания для самостоятельной работы с пакетом ggplot2 
# Задание 1
# Нарисуйте график, соответствующий следующей функции

# y=2⋅x2+4⋅x+50

# Сначала загружаем библиотеку
library(ggplot2)


# Создаём плавную последовательность x от -10 до 10
x_vals <- seq(-10, 10, length.out = 300)

# Вычисление значения y по формуле: y = 2x² + 5x + 50
y_vals <- 2 * x_vals^2 + 5 * x_vals + 50

# Создание data frame
df <- data.frame(x = x_vals, y = y_vals)

# Разделяем на две части, так как линия переходит: до x=0 (синяя) и после x=0 (красная пунктир)

df_blue <- subset(df, x <= 0)
df_red <- subset(df, x >= 0)

# Построение графика
ggplot() +
  # Синяя сплошная часть (x ≤ 0)
  geom_line(data = df_blue, aes(x = x, y = y),
            color = "blue", linewidth = 1.5) +
  # Красная пунктирная часть (x ≥ 0)
  geom_line(data = df_red, aes(x = x, y = y),
            color = "red", linetype = "dashed", linewidth = 1.5) +
  # Настройка осей
  scale_x_continuous(limits = c(-10, 10), breaks = seq(-10, 10, 5)) +
  scale_y_continuous(limits = c(0, 350), breaks = seq(0, 350, 100)) +
  # Подписи
  labs(title = "График задания",
       x = "x",
       y = "y") +
  theme_minimal() +
  theme(panel.grid.minor = element_blank(),
        panel.grid.major = element_line(color = "gray"))



# Задание 2
# Выполните следующий код, который симулирует выборку из генеральной совокупности с двумерным нормальным распределением

# set.seed(12345)

# df <- data.frame(X = rnorm(1000, 1, 5), Y = rnorm(1000, 2, 5)) 
# Визуализируйте двумерное нормальное распределение

library(ggplot2)
library(MASS) # это подсказал дипсик, потому что мне писали ошибку в density

set.seed(12345)


df <- data.frame(X = rnorm(1000, 1, 5),
                 Y = rnorm(1000, 2, 5))

# Плотность
kde <- kde2d(df$X, df$Y, n = 300, 
             lims = c(range(df$X)[1] - 2, range(df$X)[2] + 2,
                      range(df$Y)[1] - 2, range(df$Y)[2] + 2))

df_density <- expand.grid(X = kde$x, Y = kde$y)
df_density$density <- as.vector(kde$z)

# Обрезаем низкие значения
df_density$density[df_density$density < 0.0015] <- NA

ggplot() +
  geom_raster(data = df_density, aes(x = X, y = Y, fill = density)) +
  geom_point(data = df, aes(x = X, y = Y), 
             size = 0.6, shape = 16, color = "black", alpha = 0.7) +
  
  scale_fill_gradient(low = "yellow", high = "red", 
                      name = "level",
                      breaks = c(0.001, 0.002, 0.003, 0.004, 0.005, 0.006),
                      limits = c(0, 0.006),
                      na.value = "white") +
  
  coord_cartesian(xlim = range(df$X), ylim = range(df$Y), expand = FALSE) +
  
  scale_x_continuous(breaks = c(-20, -10, 0, 10)) +
  scale_y_continuous(breaks = c(-10, 0, 10, 20)) +
  
  
  theme_bw() + 
  theme(legend.position = c(0.15, 0.85),
        legend.background = element_rect(fill = "white", color = "grey"),
        panel.grid.major = element_line(color = "gray", size = 0.3),  
        panel.grid.minor = element_line(color = "gray", size = 0.2)) +
  
  labs(x = "X", y = "Y")

