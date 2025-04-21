# ## Пример: Пингвины ########################
#
# Измерения особей пингвинов из рода Pygoscelis лежат в датасете `penguins` в пакете `palmerpenguins`. Исходные данные были опубликованы в работе Gorman et al., 2014. Помимо веса и пола животных, датасет содержит информацию об острове, на котором пингвины проживали, и измерения клюва. В анализ мы возьмём только следующие переменные:
#
#
#   Зависимая переменная:
#
#   - `body_mass_g` --- вес в граммах
#
# Факторы:
#
#   - `species` --- вид пингвина
#   - `sex` --- пол пингвина

# ## Открываем данные

# install.packsexs("palmerpenguins")
library(palmerpenguins)
library(car)
peng <- as.data.frame(penguins[, c(1, 6, 7)])
str(peng)                                                 #структура данных
head(peng, 2)                                             #первые несколько строк   


#  делаем факторы факторами
peng$species <- factor(peng$species, 
                         levels = c("Adelie", "Chinstrap","Gentoo"),
                         labels = c("Пингвин Адели", "Антарктический пингвин", "Субантарктический пингвин"))

peng$sex <- factor(peng$sex, levels = c("female", "male"), 
                     labels = c("самка", "самец"))


colSums(is.na(peng))                                    #есть ли пропущенные значения




peng <- peng[!is.na(peng$sex), ]                          #удаляем где есть н'ашечки
peng <- peng[!is.na(peng$body_mass_g), ]


nrow(peng)                                                #объем выборки какой


table(peng$sex, peng$species)                           # в группах



library(ggplot2)
library(dplyr)

peng %>%
  group_by(species, sex) %>%
  summarise(Mean = mean(body_mass_g), sample_n = n(), SE = sd(body_mass_g)/sqrt(sample_n), t_kr = qt(p = c(0.975), df = (sample_n - 1)), CI_low = Mean - t_kr*SE, CI_up = Mean + t_kr*SE) -> means_body_mass_g

theme_set(theme_bw())                                                  #задает тему рисунка
ggplot(data = means_body_mass_g, aes(x = species, y = Mean, color = sex )) +
  geom_point(size = 3 ,position = position_dodge(width = 0.5)) +
  geom_errorbar(aes(ymin = CI_low, ymax =CI_up),position = position_dodge(width = 0.5))





# # Двухфакторный дисперсионный анализ ###################################

# дисперсионный анализ в R (показывает, есть ли влияние фактора (т.е. различаются ли средние значения зависимой переменной между группами))

# влияние фактора а, влияние факторра б + их взаимодействие
anova_body_mass_g <- aov(body_mass_g ~ sex + species + sex:species, data = peng)



summary(anova_body_mass_g)                   



Anova(anova_body_mass_g)



df_diagn <- fortify(anova_body_mass_g)                              # для анализа остатков

ggplot(df_diagn, aes(x = .fitted, y = .stdresid)) +            #по оси х категориальные значения(предсказанные средние), по оси y остатки
  geom_point() +
  geom_hline(yintercept = 0)                                     #гетероскедастичность, по-моему признаков нет



qqPlot(anova_body_mass_g)



# Пост-хок тесты показывают, какие именно из возможных пар средних значений различаются

#lwr и upr - границы доверительного интервала. если перескакивает ноль - то имеет правду считать при условии нулевой гипотезы.
# если p adj < 0.05, существует значимое отличие, если же p adj > 0.05, то значимого отличия не наблюдается

TukeyHSD(anova_body_mass_g)


