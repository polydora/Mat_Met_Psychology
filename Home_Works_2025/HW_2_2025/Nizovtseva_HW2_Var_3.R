data("iris")

library(dplyr)
iris %>% filter(Species == "virginica")

df_virginica <- 
  iris %>%
  filter(Species == "virginica")

  data <- df_virginica$Sepal.Length

  table <- table(data)  

  print(table)

  df_table <- data.frame(
    Sepal.Length = as.numeric(names(table)),  
    Frequency = as.numeric(table))
  
  print(df_table)  
  
  write.csv2(df_table, file = "C:/Mat_Met_Biology/Data/df_table_virginica.csv", row.names=FALSE)
  
library(ggplot2)

  ggplot(df_table, aes(x = Sepal.Length, y = Frequency)) +
    geom_bar(stat = "identity", fill = "green", color = "black") +
    labs( x = "Значение Sepal.Length", y = "Количество")  
  