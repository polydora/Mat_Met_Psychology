# Рис
# В датасете rice из пакета DAAG содержатся данные
# о росте риса (Perrine et al., 2001; Maindonald,
# Braun, 2015).

# Если не получится загрузить данные из пакета,
# скачайте файл по ссылке
# https://www.rdocumentation.org/packages/DAAG/versions/1.24/topics/rice

# Проанализируйте, как
# зависит сухая масса побегов риса от сорта риса,
# удобрений и взаимодействия этих факторов.

library(ggplot2) # Для графиков
library(car)
library(DAAG)
data(rice)
?rice

# Сухая масса побегов - ShootDryMass
# Сорт риса - variety - дикий и генетически модифицированный
# Удобрения - fert
str(rice)

rice %>%
  group_by(variety, fert) %>%
  summarise(
    Mean = mean(ShootDryMass),
    sample_n = n(),
    SE = sd(ShootDryMass)/sqrt(sample_n),
    t_kr = qt(p = c(0.975), df = (sample_n - 1)),
    CI_low = Mean - t_kr*SE,
    CI_up = Mean + t_kr*SE
  ) -> means_rice

theme_set(theme_bw())

ggplot(data = means_rice, aes(x = variety, y = Mean, color = fert)) +
  geom_point(size = 3, position = position_dodge(width = 0.5)) +
  geom_errorbar(aes(ymin = CI_low, ymax = CI_up), position = position_dodge(width = 0.5))
# 

rice.aov <- aov(ShootDryMass ~ variety * fert, data=rice);
# Анализ остатков
qqPlot(rice.aov, id = FALSE)
# Отклонений от нормального распределения нет

fort_rice <- fortify(rice.aov)
ggplot(fort_rice, aes(x = .fitted, y = .stdresid)) +
  geom_point() +
  geom_hline(yintercept = 0)
# Признаков гетероскедастичности не наблюдается

# Построим график взаимодействия факторов
interaction.plot(
  rice$fert, 
  rice$variety, 
  rice$ShootDryMass,
  main = "График взаимодействия удобрений и сорта риса",
  xlab = "Удобрения",
  ylab = "Средняя масса побега риса",
  col = c("blue", "red"),
  lty = 1:1,
  trace.label = "Сорт"
)
# Видим, что взаимодействия между этими факторами имеется

anova(rice.aov)
# Взаимодействие есть, оно значимо, главные эффекты не обсуждаем
# Нужно проверить пост хок тест
tukey <- TukeyHSD(rice.aov)
tukey

# По фактору сорта видим, что wt в среднем имеет бОльшую массу нежели ANU843
# Значение P равно 0, значит мы можем это статистически значимо

# По фактору удобрений можно утверждать, что NH4NO3 повышает массу побегов риса больше всех
# Сравнение NH4Cl с F10 статистически не значимо

# В рамках взаимодействия этих факторов можно сказать следующее:
# Встречается несколько комбинаций, которых не значительны статистически
# ANU843:NH4Cl-wt:NH4Cl
# wt:NH4NO3-wt:NH4Cl
# ANU843:NH4NO3-wt:NH4Cl
# ANU843:NH4NO3-wt:NH4NO3

# wt растет лучше ANU843 практически со всеми удобрениями, исключение составляет
# ANU843:NH4NO3-wt:NH4Cl, однако значение P превышает допустимое 0.05, поэтому
# мы не можем говорить с уверенностью, что ANU843 с удобрением NH4NO3 растет лучше wt с удобрением NH4Cl
