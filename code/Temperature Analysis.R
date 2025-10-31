library(nycflights13)
library(dplyr)
library(tidyr)
library(ggplot2)
library(tibble)

### Clean Up Data Create New Variables
UA_flights <- flights |>
  filter(carrier == "UA") |> 
  drop_na(dep_time) |>
  mutate(very_late = if_else(dep_delay > 30, TRUE, FALSE)) |>
  mutate(late = if_else(dep_delay > 0,  TRUE, FALSE))

### Join ### 
UA_wx <- UA_flights |>
  inner_join(weather, by = c("year","month","day","hour","origin")) |>
  drop_na(temp)

### Temperature Classification ### 
ua_wx <- UA_wx |>
  mutate(
    temp_cat = if_else(temp >= median(temp, na.rm = TRUE), "High", "Low")
  )

### Summaries/Box Plot ###
ua_box <- ua_wx %>%
  select(temp, late, very_late) %>%
  pivot_longer(cols = c(late, very_late),
               names_to = "delay_type",
               values_to = "is_true") %>%
  filter(is_true == TRUE)


ggplot(ua_box, aes(x = delay_type, y = temp)) +
  geom_boxplot(fill = "skyblue") +
  geom_hline(yintercept = median(ua_wx$temp, na.rm = TRUE),
             linetype = "dashed", color = "red", linewidth = 1) +
  scale_x_discrete(labels = c("late" = "Late Flights (0-30 Min)",
                              "very_late" = "Very Late Flights (>30 Min)")) + 
  labs(title = "Temperature Distribution by Delay Category (UA Flights, NYC 2013)",
       x = NULL,
       y = "Temperature (°F)") +
  theme_minimal() +
  theme(legend.position = "none",
        plot.title = element_text(size = 11, face = "bold", hjust = 0.5))


# ---------- Very Late: permutation test (difference in proportions) ---------- #
## H_0: P(Very Late | High Temp) = P(Very Late | Low Temp)
## H_a: P(Very Late | High Temp) != P(Very Late | Low Temp)

p_high    <- mean(ua_wx$very_late[ua_wx$temp_cat == "High"],  na.rm = TRUE)
p_low <- mean(ua_wx$very_late[ua_wx$temp_cat == "Low"], na.rm = TRUE)
observed   <- p_high - p_low

set.seed(1)
N <- 100000
perm <- numeric(N)

for (i in 1:N) {

  shuffled <- sample(ua_wx$temp_cat)
  p1 <- mean(ua_wx$very_late[shuffled == "High"], na.rm = TRUE)
  p0 <- mean(ua_wx$very_late[shuffled == "Low"],  na.rm = TRUE)
  perm[i] <- p1 - p0
  
}

ggplot(tibble(perm), aes(x = perm)) +
  geom_histogram(bins = 20) +
  geom_vline(xintercept = observed, color = "red")+
  labs(title = "Temperature Effect on Late Flights",
       x = "Difference in proportions (High − Low)",
       y = "Count")

## Two Sided P Value ## 
2*(sum(perm >= observed) + 1) / (N + 1)
print(observed)

# ---------- Late: permutation test (difference in proportions) ----------
## H_0: P(Late | High Temp) = P(Late | Low Temp)
## H_a: P(Late | High Temp) != P(Late | Low Temp)

p_high     <- mean(ua_wx$late[ua_wx$temp_cat == "High"],  na.rm = TRUE)
p_low <- mean(ua_wx$late[ua_wx$temp_cat == "Low"], na.rm = TRUE)
observed   <- p_high - p_low

set.seed(1)
N <- 100000
perm <- numeric(N)

for (i in 1:N) {
  
  shuffled <- sample(ua_wx$temp_cat)
  p1 <- mean(ua_wx$late[shuffled == "High"], na.rm = TRUE)
  p0 <- mean(ua_wx$late[shuffled == "Low"],  na.rm = TRUE)
  perm[i] <- p1 - p0
  
}

ggplot(tibble(perm), aes(x = perm)) +
  geom_histogram(bins = 20) +
  geom_vline(xintercept = observed, color = "red") +
  labs(title = "Temperature Effect on Very-Late Flights",
       x = "Difference in proportions (High − Low)",
       y = "Count")

## Two Sided P Value ## 
2*(sum(perm >= observed) + 1) / (N + 1)
print(observed)
