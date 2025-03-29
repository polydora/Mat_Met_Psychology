data(ChickWeight)

library(dplyr)

###### 50 levels -> выборка: 50 куриц

x <- length(ChickWeight)

mu <- round(mean(x), 0)

n <- 50



str(ChickWeight)

ChickWeight %>%
  group_by(Diet) %>%
  summarise(sample_n = n(), Mean_diet = mean(weight), SD = sd(weight), SE = SD/sqrt(sample_n), t_kr = qt(p = c(0.975), df = (sample_n - 1)), CI_low = Mean_diet - t_kr*SE, CI_up = Mean_diet + t_kr*SE)


df = 120 - 1

qt(p = c(0.025, 0.975), df = df)


mean(ChickWeight$weight)

Chick_weight <- ChickWeight$weight

df = n - 1

# t <- 2.010 по таблице

qt(p = c(0.025, 0.975), df = df)

chick_diet_1 <- ChickWeight[ChickWeight$Diet == 1, ]

chick_diet_1 %>%
    summarise(SE_1 <- sd(weight)/sqrt(n),
    low_CI_1 <- mean(weight) - 2.010*SE,
    upper_CI_1 <- mean(weight) + 2.010*SE)

chick_diet_2 <- ChickWeight[ChickWeight$Diet == 2, ]

chick_diet_2 %>%
  summarise(SE_2 <- sd(weight)/sqrt(n),
            low_CI_2 <- mean(weight) - 2.010*SE,
            upper_CI_2 <- mean(weight) + 2.010*SE)

chick_diet_3 <- ChickWeight[ChickWeight$Diet == 3, ]

chick_diet_3 %>%
  summarise(SE_3 <- sd(weight)/sqrt(n),
            low_CI_3 <- mean(weight) - 2.010*SE,
            upper_CI_3 <- mean(weight) + 2.010*SE)

chick_diet_4 <- ChickWeight[ChickWeight$Diet == 4, ]

chick_diet_4 %>%
  summarise(SE_4 <- sd(weight)/sqrt(n),
            low_CI_4 <- mean(weight) - 2.010*SE,
            upper_CI_4 <- mean(weight) + 2.010*SE)


