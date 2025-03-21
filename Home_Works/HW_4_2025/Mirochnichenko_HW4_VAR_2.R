######## Часть 1

### str(dataset_variant_2)

### library(dplyr)



######## Часть 2

data(ChickWeight)

chick_weight_21 <- ChickWeight[ChickWeight$Time == 21, ]

library(dplyr)

chick_weight_21 %>%
  group_by(Diet) %>%
  summarise(mean_weight = mean(weight),
            sd_weight = sd(weight))




