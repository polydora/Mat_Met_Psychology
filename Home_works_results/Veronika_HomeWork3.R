library(ggplot2)

Sepal.Length <- c(7.0, 6.4, 6.9, 5.5, 6.5, 5.7, 6.3, 4.9, 6.6, 5.2, 5.0, 5.9, 6.0, 6.1, 5.6, 6.7, 5.6, 5.8, 6.2, 5.6, 5.9, 6.1, 6.3, 6.1, 6.4, 6.6, 6.8, 6.7, 6.0, 5.7, 5.5, 5.5, 5.8, 6.0, 5.4, 6.0, 6.7, 6.3, 5.6, 5.5, 5.5, 6.1, 5.8, 5.0, 5.6, 5.7, 5.7, 6.2, 5.1, 5.7)
Frequency <- c(1, 7, 4, 7, 5, 8, 8, 6, 2, 4, 8, 3, 6, 6, 6, 7, 6, 7, 4, 5, 1, 6, 8, 6, 6, 2, 3, 7, 5, 8, 4, 4, 4, 5, 3, 5, 7, 7, 5, 4, 4, 3, 3, 8, 5, 8, 8, 4, 8, 8)

df <- data.frame(Sepal.Length, Frequency)

ggplot(df, aes(x=Sepal.Length, y=Frequency)) +
  geom_col(fill='green') +
  theme_minimal() +
  labs(x='Sepal.Length', y='Frequency', title='Версиколор') +
  xlim(4.5, 7.5) +
  scale_x_continuous(breaks = seq(4.5, 7.5, by = 0.1)) +
  theme(axis.text.x = element_text(size = 4))


