# Задание 1. Получение справки
# У нас еще нет пакета vegan, поэтому надо поставить:
install.packages("vegan")

# Теперь можем узнать по принципу
# ?package::function
library (vegan)
metaMDS
# distance параметр подходит

# Задание 2. R как калькулятор, математические операции
log2((2*asin(sqrt(0.5))*180/pi - 11 ^ (1 / 4))/((10 + exp(2))*50))

# Задание 3. Переменные и оформление кода
# Пакет надо точно скачать перед чтением Excel

install.packages("glue")
install.packages("readxl")
# При установки выдало ошибку
# Ошибка в .zip.unpack(pkg, tmpDir) :
#   zip-файл ‘O:\temp\RtmpCC9gbt/downloaded_packages/glue_1.8.0.zip’ не найден
# Поэтому надо еще поставить glue сначала!


install.packages("readxl")
install.packages("ggplot2")
library(readxl)
cat <- read_excel("C:/mat_met_2025/data/catsM.xlsx")
str (cat)
Model <- lm(Hwt ~ Bwt, data = cat)
summary(Model)
par(mfrow = c(2, 2))
plot(Model, page = 1)

# Задание 4

n <- round(runif(1, min = 10, max = 50),0)
vector <- round(rnorm(n, 0, 1), 0)
length(vector)
mean(vector)
sd(vector)
range(vector)


# Задание 5

df <- data.frame(Var_1 = rep(letters[1:9], each =2), Var_2 = seq(1, 100, length.out = 18), Var_3 = NA)
df

colnames(df) <- c("Var_One", "Var_Two", "Var_Thr")

extracted_values <- df[df$"Var_One" %in% c("i", "c", "f"),]
extracted_values
df[ c("Var_One","Var_Thr")]

# Задание 6

install.packages("openxlsx")
library(openxlsx)
write.xlsx(df, 'name-of-your-excel-file.xlsx')
