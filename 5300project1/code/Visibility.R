#library
library(nycflights13)
library(tidyverse)
library(ggplot2)
library(tidyr)


#Dataset
UA_flights <- flights |>
  filter(carrier == "UA") |>         # Only UA flights
  drop_na(dep_time) |>            # Remove rows with missing time_hour
  mutate(very_late = if_else(dep_delay > 30, TRUE, FALSE)) |>
  mutate(late = if_else(dep_delay > 0, TRUE, FALSE))

FlightsWeather <- UA_flights |>
  left_join(weather, by=c("month", "day", "hour", "origin")) |>
  drop_na(late, very_late, wind_speed, visib)


#Note: visib is the visibility in miles(0 is the worst, 10 is best)
#very late boxplot
ggplot(data = FlightsWeather, mapping = aes(x=visib)) +
  geom_boxplot() +
  facet_wrap(FlightsWeather$very_late, nrow=2) +
  xlim(0,10) #80% data is at visibility 10. pick a different visualization


#late boxplots
ggplot(data = FlightsWeather, mapping = aes(x=visib)) +
  geom_boxplot() +
  facet_wrap(FlightsWeather$late, nrow=2) +
  xlim(0,10) #80% data is at visibility 10. pick a different visualization


#Binary visibility values (clear = 10, limited < 10)
FlightsWeather$visib_cat <- ifelse(FlightsWeather$visib < 10, "Limited", "Clear")
FlightsWeather$visib_cat <- factor(FlightsWeather$visib_cat, levels = c("Clear", "Limited"))

# Quick check
table(FlightsWeather$visib_cat)

model1<-glm(late ~ visib_cat, data = FlightsWeather, family = binomial)
exp(coef(model1))

exp(cbind(OddsRatio = coef(model1), confint(model1)))  # odds ratios with CI

# Predicted probabilities
plogis(coef(model1)["(Intercept)"])                       # clear
plogis(coef(model1)["(Intercept)"] + coef(model1)["visib_catLimited"])  # limited

table_vis <- table(FlightsWeather$late, FlightsWeather$visib_cat)
table_vis
FlightsWeather$late_num <- as.numeric(FlightsWeather$late)
late_clear <- sum(FlightsWeather$late_num[FlightsWeather$visib_cat == "Clear"])
late_limited <- sum(FlightsWeather$late_num[FlightsWeather$visib_cat == "Limited"])
n_clear <- sum(FlightsWeather$visib_cat == "Clear")
n_limited <- sum(FlightsWeather$visib_cat == "Limited")

prop.test(
  x = c(late_clear, late_limited),
  n = c(n_clear, n_limited),
  correct = FALSE
)



#Redo visibility with very late flights
#Binary visibility values (clear = 10, limited < 10)
FlightsWeather$visib_cat <- ifelse(FlightsWeather$visib < 10, "Limited", "Clear")
FlightsWeather$visib_cat <- factor(FlightsWeather$visib_cat, levels = c("Clear", "Limited"))

# Quick check
table(FlightsWeather$visib_cat)

model1<-glm(very_late ~ visib_cat, data = FlightsWeather, family = binomial)
exp(coef(model1))

exp(cbind(OddsRatio = coef(model1), confint(model1)))  # odds ratios with CI

# Predicted probabilities
plogis(coef(model1)["(Intercept)"])                       # clear
plogis(coef(model1)["(Intercept)"] + coef(model1)["visib_catLimited"])  # limited

table_vis <- table(FlightsWeather$very_late, FlightsWeather$visib_cat)
table_vis
FlightsWeather$very_late_num <- as.numeric(FlightsWeather$very_late)
very_late_clear <- sum(FlightsWeather$very_late_num[FlightsWeather$visib_cat == "Clear"])
very_late_limited <- sum(FlightsWeather$very_late_num[FlightsWeather$visib_cat == "Limited"])
n_clear_vl <- sum(FlightsWeather$visib_cat == "Clear")
n_limited_vl <- sum(FlightsWeather$visib_cat == "Limited")

prop.test(
  x = c(very_late_clear, very_late_limited),
  n = c(n_clear_vl, n_limited_vl),
  correct = FALSE
)

