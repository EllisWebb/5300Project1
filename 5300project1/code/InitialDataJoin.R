library(nycflights13)
library(tidyverse)

UA_flights <- flights |>
  filter(carrier == "UA") |>         # Only UA flights
  drop_na(dep_time) |>            # Remove rows with missing time_hour
  mutate(very_late = if_else(dep_delay > 30, TRUE, FALSE)) |>
  mutate(late = if_else(dep_delay > 0, TRUE, FALSE))

FlightsWeather <- UA_flights |>
  left_join(weather, by=c("month", "day", "hour", "origin"))
