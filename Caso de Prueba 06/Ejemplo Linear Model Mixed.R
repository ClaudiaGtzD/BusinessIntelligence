library(lme4)
library(tidyverse)
library(lmerTest)
library(gtsummary)

data("sleepstudy")
view(sleepstudy)

just_308 <- sleepstudy %>%
  filter(Subject == "308") %>% filter(Days >= 2)

ggplot(just_308, aes(x = Days, y = Reaction)) +
       geom_point() +
       scale_x_continuous(breaks = 0:9) +
       geom_smooth(method = "lm")

#Supuestos de regresión lineas
#1. Linealidad
#2. Normalidad de los errores
#3. JHomoscedasticidad
#4. Multicolinealidad
#5. Independencia

#Las observaciones son dependientes una del día de otra

ggplot(sleepstudy %>% filter(Days >= 2), aes(x = Days, y = Reaction)) +
  geom_point() +
  scale_x_continuous(breaks = 0:9) +
  facet_wrap(~Subject)

sleep2 <- sleepstudy %>%
  filter(Days >= 2L) %>%
  mutate(days_deprived = Days - 2L)

sleep2  %>%
  count(days_deprived, Days)

ggplot(sleep2, aes(x = Days, y = Reaction)) +
  geom_point() +
  scale_x_continuous(breaks = 0:7) +
  facet_wrap(~Subject) +
  labs(y = "Reaction Time", x = "Days deprived of sleep (0 = baseline)")

cp_model <- lm(Reaction ~ days_deprived, sleep2)

summary(cp_model)

gvlma::gvlma(cp_model)
cp_model %>% residuals() %>% hist()
library(nortest)
cp_model %>% residuals() %>% ad.test()

ggplot(sleep2, aes(x = days_deprived, y = Reaction)) +
  geom_abline(intercept = coef(cp_model)[1],
              slope = coef(cp_model)[2],
              color = 'blue') +
  geom_point() +
  scale_x_continuous(breaks = 0:07) +
  facet_wrap(~Subject) +
  labs(y = "Reaction Time", x = "Days deprived of sleep (0 = baseline)")

#No para todos aplica el modelo linear porque no se considera la independencia entre los sujetos  

np_model <- lm(Reaction ~ days_deprived + Subject + days_deprived:Subject, data = sleep2)

summary(np_model)

all_intercepts <-  c(coef(np_model)["(Intercept)"],
                     coef(np_model)[3:19] + coef(np_model)["(Intercept)"])

all_slopes <-  c(coef(np_model)["days_deprived"],
                     coef(np_model)[20:36] + coef(np_model)["days_deprived"])

ids <- sleep2 %>% pull(Subject)  %>% levels() %>% factor()

#make a tibble with the data extracted above
np_coef <- tibble(Subject = ids,
                  intercept = all_intercepts,
                  slope = all_slopes)

ggplot(sleep2, aes(x = days_deprived, y = Reaction)) +
  geom_abline(data = np_coef,
              mapping = aes(intercept = intercept,
                            slope = slope),
              color = 'blue') +
  geom_point() +
  scale_x_continuous(breaks = 0:07) +
  facet_wrap(~Subject) +
  labs(y = "Reaction Time", x = "Days deprived of sleep (0 = baseline)")

#Esto es básicamente hacer una regresión lineal para cada sujeto, por lo que no se podría generalizar el modelo

#Modelos lineales de efectos mixtos
#lmer en lugar de lm
#Establecer si hay Intercepto aleatorio +(1|variable)
#Pendiente aleatoria +(Varx |1)
pp_mod <- lmer(Reaction ~ days_deprived + (days_deprived | Subject), sleep2)


summary(pp_mod)

newdata <- crossing(
  Subject = sleep2 %>% pull(Subject) %>% levels() %>% factor(),
  days_deprived = 0:7)

head(newdata, 17)

newdata2 <- newdata %>%
  mutate(Reaction = predict(pp_mod, newdata))

ggplot(sleep2, aes(x = days_deprived, y = Reaction)) +
  geom_line(data = newdata2,
              color = 'blue') +
  geom_point() +
  scale_x_continuous(breaks = 0:07) +
  facet_wrap(~Subject) +
  labs(y = "Reaction Time", x = "Days deprived of sleep (0 = baseline)")

fixef(pp_mod)

confint(pp_mod)

pp_mod %>% 
  tbl_regression(intercept = T, label = days_deprived~"Days of sleep deprivation")

## Predecir los días que llevo de insomnio utilizando el tiempo de respuesta
## Poisson

ml <- glmer(days_deprived~Reaction + (1|Subject), family="poisson", data=sleep2)
summary(ml)

## Mayor tiempo de reacción, mayor número de días de insomio
ml %>% tbl_regression(exp=T, intercept = T)
