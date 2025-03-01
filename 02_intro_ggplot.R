# Визуализция данных средствами ggplot2


## Учимся читать внешние данные в форате csv
titanic <- read.table(file = 'data/Titanic.csv', sep = ';', header = TRUE)

str(titanic)

titanic$Age

titanic[ ,1]

titanic[2, 1]


head(titanic, 1)

tail(titanic)


sum(titanic$Freq)

## Длинный формат данных


library(tidyr)

long_titanic <- uncount(titanic, weights = Freq)

head(long_titanic, 10)

nrow(long_titanic)
ncol(long_titanic)






# Данные взяты из работы
# Tager, I. B., Weiss, S. T., Rosner, B., and Speizer, F. E. (1979). Effect of parental cigarette smoking on pulmonary function in children. American Journal of Epidemiology, 110, 15-26.
# Rosner, B. (1990). Fundamentals of Biostatistics, 3rd Edition. PWS-Kent, Boston, Massachusetts.
# Источник данных: http://www.statsci.org/data/general/fev.html
#
# Структура данных
# Age 	 -  	Возраст
# FEV 	 -  	Объем легких при выдохе (литры) (forced expiratory volume)
# Height 	 -  	Рост (дюймы)
# Sex 	 -  	 пол (Male or Female)
# Smoker 	 -  	некурящие (Non), курящие (Current)



#############################################################################
#Загрузка пакетов

library(readxl)
library(ggplot2)

#############################################################################

#############################################################################
# Читаем данные

fev <- read_excel("data/fev.xls", sheet = "tidy_data", col_names = TRUE, na = "NA", skip = 1 )




# Структура данных

# имена переменных, заголовки столбцов

names(fev)

# [1] "Age"    "FEV"    "Height" "Sex"    "Smoker"


str(fev) #дает информацию о структуре датафрейма

# Classes ‘tbl_df’, ‘tbl’ and 'data.frame':	654 obs. of  5 variables:
#   $ Age   : num  9 8 7 9 9 8 6 6 8 9 ...
# $ FEV   : num  1.71 1.72 1.72 1.56 1.9 ...
# $ Height: num  57 67.5 54.5 53 57 61 58 56 58.5 60 ...
# $ Sex   : chr  "Female" "Female" "Female" "Male" ...
# $ Smoker: chr  "Non" "Non" "Non" "Non" ...


fev[15:20, ]

fev[ ,2]

fev$FEV[2:3]

fev$Age

fev$Smoker [10:200]

fev[c(1, 4, 10),  ]



library(dplyr)

# dplyr::filter()

# df_female <-

fev %>%
  filter(Sex == "Male") %>%
  nrow()

df_female <-
  fev %>%
  filter(Sex == "Male")

fev %>%
  filter(Sex == "Female") -> df_female


#Необходимо заменить формат в переменных fev$Sex и fev$Smoker

fev$Sex <- factor(fev$Sex, levels = c("Female", "Male"))

fev$Sex


as.numeric(fev$Sex)

fev$Smoker <- factor(fev$Smoker)

fev %>%
  filter(is.na(Age))

fev %>%
  filter(is.na(FEV))

fev <-
  fev %>%
  filter(complete.cases(.))



#функция factor() превращает числовые или текстовые данные в дискретные факторы

# Если необходимо убрать объекты, у которых что-то не измерено (NA), то надо произвести "очистку данных". НО! не увлекайтесь

#############################################################################
#Визуализация данных (первый заход)

# aesthetics
# geom

ggplot(data = fev, mapping = aes(x = Age, y = FEV)) +
  geom_point()



#Задача: построить точечную диаграмму, где по оси OX отложен Age, а по оси OY отложен FEV

ggplot(data = fev, mapping = aes(x = Age, y = FEV)) +
  geom_point() +
  ggtitle("График, отражающий связь объема легких и возраста") +
  labs(x = "Возраст", y = "Объем легких")

#####





ggplot(data = fev, aes(x = Age, y = FEV)) + geom_point()

# Убираем серый фон

ggplot(data = fev, aes(x = Age, y = FEV)) +
  geom_point() +
  theme_bw()

ggplot(data = fev, aes(x = Age, y = FEV)) +
  geom_point() +
  theme_classic()

ggplot(data = fev, aes(x = Age, y = FEV)) +
  geom_point() +
  theme_minimal()

ggplot(data = fev, aes(x = Age, y = FEV)) +
  geom_point() +
  theme_dark()


#Устанавливаем понравившуюся тему, как основную.
theme_set(theme_bw()) # далее все графики, производимые в данной сессии, будут использовать именно эту тему

ggplot(data = fev, aes(x = Age, y = FEV)) + geom_point()

# Изменяем подписи осей

ggplot(data = fev, aes(x = Age, y = FEV)) +
  geom_point() +
  labs(x = "Возраст пациента", y = "Объем легких")




#Создаем верхний заголовок рисунка

ggplot(data = fev, aes(x = Age, y = FEV)) +
  geom_point() +
  labs(x = "Возраст", y = "Объем легких", title = "Зависимость между \nвозрастом и \nобъемом легких")

# \n
# \t

# Делаем заголовок центральным
ggplot(data = fev, aes(x = Age, y = FEV)) +
  geom_point() +
  labs(x = "Возраст", y = "Объем легких", title = "Зависимость между \n возрастом и объемом легких") +
  theme(plot.title = element_text(hjust = 0.5))


# Меняем размер точек

#Крупнее
ggplot(data = fev, aes(x = Age, y = FEV)) +
  geom_point(size = 3) +
  labs(x = "Возраст", y = "Объем легких", title = "Зависимость между \n возрастом и объемом легких") +
  theme(plot.title = element_text(hjust = 0.5))



#Мельче
ggplot(data = fev, aes(x = Age, y = FEV)) +
  geom_point(size = 0.1) +
  labs(x = "Возраст", y = "Объем легких", title = "Зависимость между \n возрастом и объемом легких") +
  theme(plot.title = element_text(hjust = 0.5, size = 12), axis.title = element_text(size = 10, angle = 0))



# Меняем цвет и форму точек

ggplot(data = fev, aes(x = Age, y = FEV)) +
  geom_point(color = "red") +
  labs(x = "Возраст", y = "Объем легких", title = "Зависимость между \n возрастом и объемом легких") +
  theme(plot.title = element_text(hjust = 0.5))


ggplot(data = fev, aes(x = Age, y = FEV)) +
  geom_point(shape = 22, color = "red", fill = "yellow", size = 2) +
  labs(x = "Возраст", y = "Объем легких", title = "Зависимость между \n возрастом и объемом легких") +
  theme(plot.title = element_text(hjust = 0.5))



#Сохраняем рисунок в файл

ggplot(data = fev, aes(x = Age, y = FEV)) +
  geom_point(shape = 22, color = "red", fill = "yellow", size = 2) +
  labs(x = "Возраст", y = "Объем легких", title = "Зависимость между \n возрастом и объемом легких") +
  theme(plot.title = element_text(hjust = 0.5))


ggsave("MyPicture.jpg", plot = last_plot())

#Рисунок можно, и это правильно, поместить в специальную переменную
Plot_1 <-
  ggplot(data = fev, aes(x = Age, y = FEV)) +
  geom_point(shape = 22, color = "red", fill = "yellow", size = 2)

Plot_1

# Далее эту переменную можно модифицировать

Plot_1 +
  labs(x = "Возраст", y = "Объем легких", title = "Зависимость между \n возрастом и объемом легких") +
  theme(plot.title = element_text(hjust = 0.5))


Plot_2 <-
  Plot_1 +
  labs(x = "Возраст", y = "Объем легких", title = "Зависимость между \n возрастом и объемом легких") +
  theme(plot.title = element_text(hjust = 0.5))


ggsave("MyPicture_2.wmf", plot = Plot_2)

getwd()

#############################################################################
#Визуализация данных (Aesthetics)
# В философии ggplot эстетики - это та информация (данные), которую можно выразить графиком.
# Минимальные эстетики - Положение на OX и положение на OY
# Однако наши данные содержат еще и информацию о поле (переменная fev$Sex). Если эти данные для нас важны, то мы должны эту информацию выразить на графике


#Отражаем данные о поле с помощью цвета
Plot_1 <-
  ggplot(data = fev, aes(x = Age, y = FEV, size = Height, color = Sex)) +
  geom_point() +
  labs(x = "Возраст", y = "Объем легких", title = "Зависимость между \n возрастом и объемом легких") + theme(plot.title = element_text(hjust = 0.5))

Plot_1

# Меняем цвет на тот, который нам нравится
Plot_1 + scale_color_manual(values = c("blue", "pink"))


# Меняем положение легенды
Plot_1 + scale_color_manual(values = c("pink","blue")) + theme(legend.position =  "bottom")

Plot_1 + scale_color_manual(values = c("pink","blue")) + theme(legend.position =  "left")

Plot_1 + scale_color_manual(values = c("pink","blue")) + theme(legend.position =  c(0.5, 0.1)) # c(0.1, 0.9) координаты указываются в долях от сторон рисунка



#Отражаем данные о поле с помощью формы точек
Plot_1 <- ggplot(data = fev, aes(x = Age, y = FEV, shape = Sex, color = Sex )) + geom_point(size = 2) + labs(x = "Возраст", y = "Объем легких", title = "Зависимость между \n возрастом и объемом легких") + theme(plot.title = element_text(hjust = 0.5))

Plot_1


# В нашем датафрейме есть еще и данные о курении.
# Если мы хотим выразить графиком одновременно данные по полу и по курению, то мы должны задать две разные эстетики


Plot_1 <- ggplot(data = fev, aes(x = Age, y = FEV, shape = Sex, color = Smoker )) + geom_point(size = 2) + labs(x = "Возраст", y = "Объем легких", title = "Зависимость между \n возрастом и объемом легких") + theme(plot.title = element_text(hjust = 0.5))

Plot_1 #в этом трудно разобраться


#Используем фасетирование

Plot_1 + facet_wrap( ~ Smoker) #уже лучше

Plot_1 + facet_grid(Sex ~ Smoker)


#В нашем датафрейме есть еще и данные по росту.

Plot_1 <- ggplot(data = fev, aes(x = Age, y = FEV, shape = Sex, color = Smoker, size = Height)) + geom_point() + labs(x = "Возраст", y = "Объем легких", title = "Зависимость между \n возрастом и объемом легких") + theme(plot.title = element_text(hjust = 0.5))

Plot_1 + facet_grid(Sex ~ Smoker)


# Шуточный пример, из которого можно почерпнуть некоторые возможности ggplot


#Запустите код, расположенный между двумя линиями

#___________________________

# Run this code
library(ggplot2)
library(dplyr)
fir1 <- dnorm(x = 1:100, mean = 50, sd = 2)
circus <- function(n, p, cos2 = 0, sin2 =0, cos3 = 0, sin3 = 0){
  # n - number of points
  # p - period
  points <- data.frame(X=c(1:n), Y=c(1:n))
  factor <- points
  k <- 0
  for (i in 1:n){
    factor$X[i] <- (i-1)/p - k
    if ((i/p - trunc(i/p))==0) k <- k + 1
  }

  factor$Y <- factor$X

  for (i in 1:n){
    points$X[i] <- cos(2*3.14*factor$X[i]) + cos(cos2/4*3.14*factor$X[i]) + cos(cos3/4*3.14*factor$X[i])
    points$Y[i] <- sin(2*3.14*factor$Y[i]) + sin(sin2/4*3.14*factor$Y[i]) + sin(sin3/4*3.14*factor$Y[i])
  }
  return(points)
}

fir <- data.frame(x = 3, y = seq(-3, 4, length.out = 100))
fir$xend <- seq(3, 5, length.out = 100)
fir$yend <- 4 - fir$xend

fir2 <- data.frame(x = 3, y = seq(4, -3, length.out = 100))
fir2$xend <- seq(3, -3, length.out = 100)
fir2$yend <- fir$yend


ray <- data.frame(x=3, y = 5, angle = runif(100, 0, 2*3.14), radius = rnorm(100, 1, 0.5))

stars <- data.frame(x = rnorm(30, 1, 10), y = rnorm(30, 11, 1), angle = runif(30, 0, 2*3.14), radius = rnorm(30, 0.1, 0.5))

snow <- data.frame(x = rnorm(3000, 3, 10),
                   y = rnorm(3000, -3, 0.1) )
snow$y[snow$y < -3] <-snow$y [snow$y < -3] + 0.3

snow$y <- cos((snow$x+4) * pi/4)


snake <- data.frame(x = seq(-6,6,0.01)) %>%
  mutate(y = sin(x))
snake_head <- circus(40, 40)


background <-
  expand.grid(x = seq(-7, 10, 0.1), y = seq(-3, 12, 0.1) ) %>%
  mutate(color = y)

ggplot() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_rect(fill="gray40")) +
  xlab("") +
  ylab("") +
  ylim(-3, 12) +
  xlim(-7, 10) +
  geom_tile(data = background, aes(x, y, fill = color)) +
  scale_fill_gradient2(low = "black", mid = "darkblue", high = "blue") +
  geom_line(data = snake, aes(x, y + 4, size = (x), color = x )) +
  scale_color_gradient2(low = "green", mid = "darkgreen",high = "black") +
  guides(size = "none") +
  guides(color = "none") +
  geom_point(data  = snow, aes(x = x, y = y - 2), color = "white", position = position_jitter(height = 2), shape = 8) +
  geom_curve(data = fir, aes(x=x, y=y, xend = xend, yend = yend), curvature = -0, color = "green") +
  geom_curve(data = fir, aes(x=x, y=y +1, xend = xend, yend = yend + 1), curvature = -0, color = "darkgreen") +
  geom_curve(data = fir, aes(x=x, y=y -1, xend = xend, yend = yend - 1), curvature = -0, color = "darkgreen") +
  geom_curve(data = fir2, aes(x=x, y=y, xend = xend, yend = yend), curvature = -0, color = "darkgreen") +
  geom_curve(data = fir2, aes(x=x, y=y +1, xend = xend, yend = yend + 1), curvature = -0, color = "darkgreen") +
  geom_curve(data = fir2, aes(x=x, y=y -1, xend = xend, yend = yend - 1), curvature = -0, color = "darkgreen") +
  geom_spoke(data = ray, aes(x = x, y = y, angle = angle, radius = radius), color = "yellow") +
  geom_point(data  = stars, aes(x = x, y = y), color = "yellow",  size = 8, shape = "*") +
  geom_point(aes(x = rnorm(100, 1, 10), y=rnorm(100, 4, 3)), shape=8, size=3, color="white") +
  guides(fill = "none") +
  geom_polygon(data = snake_head, aes(X+4.5, Y+3.5), fill = "gray20", color = "black") +
  annotate(x = 6.8, y = 4, geom = "point", color = "white", size = 4) +
  annotate(x = 6.82, y = 3.9, geom = "point", color = "blue", size = 2) +
  annotate(x = 6.1, y = 3.1, geom = "point", color = "white", size = 4)+
  annotate(x = 6.1, y = 3., geom = "point", color = "blue", size = 2)+
  annotate(x = -5, y = 8, geom = "text", label = "H", color = "yellow", size = 15) +
  annotate(x = -4, y = 7, geom = "text", label = "a", color = "yellow", size = 15) +
  annotate(x = -3, y = 8, geom = "text", label = "p", color = "yellow", size = 15) +
  annotate(x = -2, y = 7, geom = "text", label = "p", color = "yellow", size = 15) +
  annotate(x = -1, y = 8, geom = "text", label = "y", color = "yellow", size = 15) +
  annotate(x = 2, y = 9, geom = "text", label = "New", color = "yellow", size = 15) +
  annotate(x = 7, y = 7, geom = "text", label = "YeaR", color = "yellow", size = 15, angle = 10) +
  annotate(x = 8.5, y = 3.5, geom = "text", label = "2025", color = "yellow", size = 7, fontface = 'italic' ) +
  theme(axis.text = element_blank(), axis.ticks = element_blank())
#___________________________

