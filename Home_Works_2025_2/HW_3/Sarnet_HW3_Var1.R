#Часть 1: Описательные статистики для Petal.Length по видам ирисов

#Загружаем встроенный датасет iris

data(iris)

#Проверяем структуру данных

str(iris)

#Используем библиотеку dplyr для более удобной работы с данными

library(dplyr)

statistics_clean <- iris %>%
  group_by(Species) %>%  # Группируем данные по видам ирисов
  summarise(Минимум = min(Petal.Length),
    `Первый квартиль` = quantile(Petal.Length, 0.25),
    Медиана = median(Petal.Length),
    `Третий квартиль` = quantile(Petal.Length, 0.75),
    Максимум = max(Petal.Length))


#Описательные статистики (красиво оформленные):
print(statistics_clean)



#Часть 2:Построение графика распределения Petal.Length по видам ирисов

#Подключаем библиотеку ggplot2 для построения графиков
library(ggplot2)

#Создаем красивый boxplot с точками данных

ggplot(iris, aes(x = Species, y = Petal.Length, fill = Species)) +
  #Добавляем boxplot 
  geom_boxplot(alpha = 0.7,               #Прозрачность заливки
               outlier.shape = NA) +      #Добавим точки отдельно
  #Добавляем точки данных (jitter для избежания наложения)
  geom_jitter(width = 0.2,                #Случайное смещение по горизонтали
              alpha = 0.6,                #Прозрачность точек
              size = 1.5) +               #Размер точек

#Добавляем заголовок и подписи осей

  labs(title = "Распределение длины лепестка (Petal.Length) по видам ирисов",
       subtitle = "Вариант 1: Boxplot с точками данных",
       x = "Вид ириса",
       y = "Длина лепестка (см)") +
  
#Настраиваем цвета 

  scale_fill_brewer(palette = "Set1") +
  
#Используем минималистичную тему

  theme_minimal() +
  
#Дополнительные настройки темы

  theme(plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5),
        legend.position = "none")  # Убираем легенду

