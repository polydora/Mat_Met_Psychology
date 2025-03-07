# Задание 1

data("iris")
str(iris)

library(dplyr)

df_versicolor <-
  iris %>% 
  filter(Species == "versicolor")
  
  data <- df_versicolor$Sepal.Length
  
  table <- table(data) 
  
  print(table)
  
  df_table <- data.frame(
    Sepal.Length = as.numeric(names(table)),  
    Frequency = as.numeric(table)             
  )
  
  print(df_table)

  write.csv2(df_table, file = "C:/Mat_Met_Biology/Data/df_table_versicolor.csv", row.names=FALSE, fileEncoding = "UTF-8")
    
  
# Задание 2
  
  library(ggplot2)
          
  ggplot(df_table, aes(x = Sepal.Length, y = Frequency)) +
    geom_col(fill = "green", color = "black") +
    labs( x = "Значение Sepal.Length", y = "Количество")
  