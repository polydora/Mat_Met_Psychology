elves <- c(181, 179, 188, 178, 175, 177, 181, 180, 166, 178, 179, 188, 182, 176, 181)
figures <- c(183, 187, 186, 209, 187, 182, 185, 183, 204, 195, 181)

## Нулевая гипотеза: figures = elves
## Альтернативная: figures != elves

tt <- t.test(x = elves,
             y = figures)

if (tt$p.value <= 0.05) {
  print("OH, HELL NO! THAT'S NOT OUR ELVES!")
} else {
  print("Thank the heavens, it's our elves!")
}


## p = 0,0062, следовательно p < 0.05
## Нулевая гипотеза опровегнулась

###### диграммы получилось только для каждего отдельно 

library(dbplyr)
library(ggplot2)

df_f <- as.data.frame(figures)

ggplot(data = df_f, aes(x = "figures", y = figures)) +
  geom_boxplot()

df_e <- as.data.frame(elves)

ggplot(data = df_e, aes(x = "elves", y = elves)) +
  geom_boxplot() 

### по диграммам видно, что elves и figures не совпадают



### 
## dat <- c(elves, figures)
## df <- as.data.frame(dat)

## ggplot(data = df, aes(x = c("elves", "figures"), y = dat))+
##  geom_boxplot() 


############## 

sample_n = 15
Mean = mean(elves)
SD = sd(elves)
SE = SD/sqrt(sample_n)
t_kr = qt(p = c(0.975), df = (sample_n - 1))
CI_low = Mean - t_kr*SE
CI_up = Mean + t_kr*SE

sample_n_f = 11
Mean_f = mean(figures)
SD_f = sd(figures)
SE_f = SD_f/sqrt(sample_n_f)
t_kr_f = qt(p = c(0.975), df = (sample_n_f - 1))
CI_low_f = Mean_f - t_kr_f*SE_f
CI_up_f = Mean_f + t_kr_f*SE_f

