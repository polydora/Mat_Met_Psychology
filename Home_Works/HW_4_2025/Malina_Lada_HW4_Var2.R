
#Часть 1

# Часть 2
data(ChickWeight)

chickweight_day21 <- ChickWeight[ChickWeight$Time == 21, ]

library(dplyr)

chickweight_day21 %>%
  group_by(Diet) %>%
  summarise(mean_weight = mean(weight),
            sd_weight = sd(weight))


