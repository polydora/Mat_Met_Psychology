
data ("iris")

virginica <-
  iris %>%
  filter(Species == "virginica")

virginica

sepal_length <- virginica$Sepal.Length

write.csv(sepal_length, "C:/Users/Maria/Desktop/MatMet/Mat_Met_Learning/Data/Sepal_length.csv", row.names= FALSE )

sepal_length_sorted <- read.table(file = 'C:/Users/Maria/Desktop/MatMet/Mat_Met_Learning/Data/Sepal_length_sorted.csv', sep = ',', header = TRUE)

Iris_plot <- (ggplot(data = sepal_length_sorted, mapping = aes(x = Sep_length, y = Frequency)) +
                geom_col (fill = "green", color = "black") + theme_bw () +
                xlim(4.5, 8.0) +  
                scale_x_continuous(breaks = seq(4.5, 8.0, by = 0.5)) +
                labs(x = "Значение Sepal.Length", y = "Количество"))

Iris_plot

ggsave("Iris.wmf", plot = Iris_plot)