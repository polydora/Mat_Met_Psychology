# Задание 1. 

# 1. Подключение библиотеки dplyr и датасета iris.
# 2. Создание таблицы с подсчётом значений Sepal.Length для setosa.
# 3. Для просмотра резулатата используем print(df_setosa).
# 4. Функция для сохранения датафрейма, в которой будет убрана нумерация строк write.csv(..., row.names = FALSE).

# Описание команд датафрейма.
# оператор, который передаёт результат предыдущей операции в следующую iris %>%.
# filter(Species == "setosa") выбирает только те строки, где столбец Species равен "setosa".
# group_by(Sepal.Length) группирует таблицу по значениям в колонке Sepal.Length.
# summarise(Count = n()) создаёт новую таблицу (Count — новое имя колонки, n() - сколько строк было в каждой группе.
# arrange(Sepal.Length) сортирует строки по возрастанию длины.

library(dplyr)
data(iris)
df_setosa <- iris %>% filter(Species == "setosa") %>% group_by(Sepal.Length) %>% summarise(Count = n()) %>% arrange(Sepal.Length)
print(df_setosa)
write.csv(df_setosa, "C:/media/Mat_Met_2025/Data/df_setosa.csv", row.names = FALSE)
  

# Задание 2.

# 1. Подключение библиотеки ggplot2.
# 2. Построение столбчатой диаграммы с помощью ggplot.

# Описание команд.
# отрисовка столбцов нужного цвета geom_col().
# подписи и заголовки labs()
# тема оформления theme_minimal()

library(ggplot2)

ggplot(df_setosa, aes(x = Sepal.Length, y = Count)) +
  geom_col(fill = "green", color = "black") + labs(x = "Значение Sepal.Length", y = "Количество") +
  theme_minimal(base_size = 14)


# Дополнительные задания.

# Задание 1.

# 1. Создание вектора x от -10 до 10 с шагом 0.1.
# 2. Вычисление значения функции.
# 3. Делие данных на 2 части, чтобы отрисовать график разными цветами.
# 4. Построение графика с помощью plot() и lines() .

# Описание команд графиков.
# type = "l" рисует линию
# lwd толщина линии
# xlim, ylim диапазоны осей
# lines() добавляет новые линии на существующий график
# lty варианты пуктирной
# grid() добавляет сетку

x <- seq(-10, 10, by = 0.1)
y <- 2 * x^2 + 4 * x + 50

x_1 <- x[x < 0]
y_1 <- y[x < 0]
x_2 <- x[x >= 0]
y_2 <- y[x >= 0]

plot(x_1, y_1, type = "l", col = "blue",lwd = 3, xlim = c(-10, 10), ylim = c(0, 300), xlab = "x", ylab = "y")
lines(x_2, y_2, col = "red", lwd = 3, lty = 2)
grid()
