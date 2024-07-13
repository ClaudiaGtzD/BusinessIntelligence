library(lme4)
library(tidyverse)
library(lmerTest)
library(gtsummary)
library(openxlsx, quietly = TRUE)

datos <- read.csv("C:/Users/Claudia/OneDrive/Documentos/FCFM/04_Tetramestre/Modelo de Negocios/BusinessIntelligence/Caso de Prueba 06/2021 U.S. Avocado Market Demand  Elasticity.csv",header=TRUE, sep=",")


ind <- datos %>%
  filter(Region == "California")

ggplot(ind, aes(x = Average.Sales.Price, y = Units.Sold)) +
  geom_point() +
  scale_x_continuous() +
  geom_smooth(method = "lm")

ggplot(datos, aes(x = Average.Sales.Price, y = Units.Sold)) +
  geom_point() +
  scale_x_continuous() +
  facet_wrap(~Region)

cp_model <- lm(Units.Sold ~ Average.Sales.Price, datos)

summary(cp_model)

gvlma::gvlma(cp_model)
cp_model %>% residuals() %>% hist()
library(nortest)
cp_model %>% residuals() %>% ad.test()

ggplot(datos, aes(x = Average.Sales.Price, y = Units.Sold)) +
  geom_abline(intercept = coef(cp_model)[1],
              slope = coef(cp_model)[2],
              color = 'blue') +
  geom_point() +
  scale_x_continuous(breaks = 0:07) +
  facet_wrap(~Region) +
  labs(y = "Precio", x = "Demanda")

#Modelos lineales de efectos mixtos
#lmer en lugar de lm
#Establecer si hay Intercepto aleatorio +(1|variable)
#Pendiente aleatoria +(Varx |1)
pp_mod2 <- lmer(Units.Sold ~ Average.Sales.Price + (Average.Sales.Price | Region), datos)


summary(pp_mod2)

factor(unique(datos$Region))

nuevo <- crossing(
  Region = factor(unique(datos$Region)),
  Average.Sales.Price = seq(min(datos$Average.Sales.Price), max(datos$Average.Sales.Price), length.out = 11)
)

# Realizar las predicciones
nuevo2 <- nuevo %>%
  mutate(Units.Sold = predict(pp_mod2, nuevo))

# Visualización con ggplot2
ggplot(datos, aes(x = Average.Sales.Price, y = Units.Sold)) +
  geom_line(data = nuevo2, aes(x = Average.Sales.Price, y = Units.Sold, group = Region),
            color = 'blue') +
  geom_point() +
  facet_wrap(~ Region) +
  labs(y = "Units Sold", x = "Average Sales Price") +
  scale_x_continuous(breaks = seq(min(datos$Average.Sales.Price), max(datos$Average.Sales.Price), by = 1))

write.csv(nuevo2, file = "C:/Users/Claudia/OneDrive/Documentos/FCFM/04_Tetramestre/Modelo de Negocios/BusinessIntelligence/Caso de Prueba 06/Lineas.csv", row.names = FALSE)

