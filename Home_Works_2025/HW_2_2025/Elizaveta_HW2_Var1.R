# Домашнее задание №2. Данные и их визуализация с использованием пакета ggplot2
# Демиденко ЕР Вариант 1. 

##### Первая часть 

# Открываем датасет, в нем 150 строк с 5 столбцами (Sepal L, Sepal W, PEtal L, Petal W, Species), проверяю галочку напротив ggplot2 

data("iris")

# Я работаю с видом "setosa", поэтому выделяем данные всех столбцов, относящиеся к этому Species

data <- iris[iris$Species == "setosa", ]

# По новому фильтру у нас 50 строк с данными 
# Нам нужен параметр "Sepal.Length"

iris$Sepal.Length[iris$Species == "setosa"]

SepalLength <- iris$Sepal.Length[iris$Species == "setosa"]

# Сортирую данные с помощью функции sort" и создаю матрицу на их основе, вывожу в 5 столбцов (ncol) для удобства подсчеа 
# help(sort) функция сортирует значения по возрастанию, сортировать значения по убыванию можно с помощью параметра decreasing 

SepalLengthSorted <- sort(SepalLength)
mdat1 <- matrix(SepalLengthSorted, ncol = 5)
print(mdat1)

# Считаю вручную встречаемость каждого значения
# 4.3 - 1, 4.4 - 3, 4.5 - 1, 4.6 - 4, 4.7 - 2, 4.8 - 5, 4.9 - 4, 5.0 - 8, 5.1 - 8, 5.2 - 3, 5.8 - 1

# Создаю из данных таблицу в эксель с двумя столбцами, сохраняю в формате .csv

##### Вторая часть. 

sepallength <- read.table(file = 'data/SepalLength_dataset.csv', sep = ';')
# Проверяю как читается файл
head(sepallength)
names(sepallength)

# После сохранения таблицы в формате xlsx пропадают имена столбцов, поэтому устанавливаю им имена через функицю

colnames(sepallength) <- c("Sepal.Length", "Frequency")
head(sepallength, 10)
# Проверяю что файл правильно читается
# Функция factor превращает числовые и текстовые данные в дискретные значения
sepallength$Sepal.Length <- factor(sepallength$Sepal.Length)

#Нам нужна не точечная, а столбчатая диаграмма (чтобы диграмма отображала значения из нашей data, использую функцию geom_col), столбцы-  зеленый цвет (точно узнаю его с помощью пипетки, по RGB(0,255, 65), черные границы столбцов, белый фон)
ggplot(data = sepallength, aes(x = Sepal.Length, y = Frequency)) +
  geom_col(fill = rgb(0, 255, 65, maxColorValue = 255), color = "black") + 
  labs (x = "Значение Sepal.Length", y = "Количество") + 
  theme_light()

# Данные визуализируются в виде диаграммы 
