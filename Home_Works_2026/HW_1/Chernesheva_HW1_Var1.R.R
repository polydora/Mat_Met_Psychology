#задание 1, не удается скачать папку/не удается найти функцию
library(car)
?Anova
??Anova

# #задание 2
# #числитель 
a <- 180/sqrt(2) - cos(pi)/(11^(1/4))

# #знаменатель
b <- (10 + exp(1))^5

# #искомое
c <- a / b
log(c, base = 4)

# ##задание 3, не удается скачать папку 
install.packages("readxl")

# ###задание 4
n <- round(runif(1, min = 10, max = 50),0)
vector <- round(rnorm(n, 0, 1), 0)

vector

# ###количество элементов
elements <- length(vector)
elements

# ###медиана
median_value <- median(vector)
median_value

# ###срднеквадратичное отклонение
otk <- sd(vector)
otk

# ###одновременно максимальное и минимальное значение
min_value <- min(vector)
max_value <- max(vector)
min_value
max_value

# ####задание 5
# ####исходный датафрейм
df <- data.frame(Var_1 = rep(letters[11:19], each =2), Var_2 = seq(10, 1000, length.out = 18), Var_3 = NA)
df
# ####так как в первый раз датафрейм перезаписался как вектор, проверяем
class(df)

# ####код, изменяющий названия переменных
names(df) <- c("Var_One", "Var_Two", "Var_Three")
names(df)

# ####код, который выберет из датафрейма df только те строки, в которых переменная Var_One принимает значения k, s, r
selected_rows <- df[df$Var_One %in% c("k", "s", "r"), ]
selected_rows

# ####код, который выберет из датафрейма df только переменные Var_1 и Var_3
selected_cols <- df[, c("Var_One", "Var_Three")]
selected_cols

# #####задание 6
# #####сохраняем датафрейм
write.csv(df, "df.csv", row.names = FALSE)