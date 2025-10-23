# Задание 1

iris %>%
  filter (Species == "versicolor") -> df_iris

install.packages ("dplyr")
library (dplyr)

count (df_iris,Sepal.Length) -> Sepal.Length
write.csv (Sepal.Length, "data/Sepal.Length.csv")

#Задание 2


ggplot(Sepal.Length, aes(x = Sepal.Length, y = n)) +
  geom_bar(stat = "identity", fill = "green", color = "black") +
  theme_bw()+
  labs(
       x = "Значение Sepal.Length",
       y = "Количество")

#Доп задание 1


curve(2*x^2 + 4*x +50, from=-10, to=0, xlab="x", ylab="y", xlim = c (-10,10), 
      ylim= c (0,300), col = "blue",
      lwd = 5,
      n= 1000)
  curve(2*x^2 + 4*x +50,add = TRUE, from=0, to=10, 
        xlab="x", ylab="y", col = "red",
        lwd = 5,
        lty = 2,
        n= 1000)
    

  
