Sepal.Length <- c(4.3, 4.4, 4.5, 4.6, 4.7, 4.8, 4.9, 5.0, 5.1, 5.2, 5.3, 5.4, 5.5, 5.7, 5.8)
Frequency <- c(1, 3, 1, 4, 2, 5, 4, 8, 8, 3, 1, 5, 2, 2, 1)

df <- data.frame(Sepal.Length, Frequency)

ggplot(df, aes(x=Sepal.Length, y=Frequency)) +
  geom_col(fill="green", color = "black") +
  theme_minimal() +
  labs(x='Sepal.Length', y='Frequency', title='Setosa') +
  xlim(10, 10) +
  scale_x_continuous(breaks = seq(10, 10, by = 1)) +
  theme(axis.text.x = element_text(size = 8))