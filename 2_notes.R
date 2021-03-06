library(tidyverse)
library(corrr)

starwars %>%
  dplyr::select(height, mass, birth_year) %>%
  correlate()

starwars %>%
  dplyr::select(height, mass, birth_year) %>%
  correlate() %>%
  shave()

starwars %>%
  dplyr::select(height, mass, birth_year) %>%
  correlate() %>%
  shave() %>%
  fashion()

pairs(~ height + mass + birth_year, starwars)

starwars %>%
  filter(mass > 1200) %>%
  dplyr::select(name, mass, height, birth_year)

starwars2 <- starwars %>%
  filter(name != "Jabba Desilijic Tiure")

pairs(~ height + mass + birth_year, starwars2)

starwars2 %>%
  filter(birth_year > 800) %>%
  dplyr::select(name, height, mass, birth_year)

starwars3 <- starwars2 %>%
  filter(name != "Yoda")

pairs(~ height + mass + birth_year, starwars3)

starwars3 %>%
  dplyr::select(height, mass, birth_year) %>%
  correlate() %>%
  shave() %>%
  fashion()

starwars %>%
  select(height, mass, birth_year) %>%
  correlate(method = "spearman") %>%
  shave() %>%
  fashion() %>%
  knitr::kable()

?MASS::mvrnorm

cbind(1:3, 4:6, 7:9)

handw <- read_csv("data/heights_and_weights.csv", col_types = "dd")

ggplot(handw, aes(height_in, weight_lbs)) +
  geom_point(alpha = .2) +
  labs(x = "height (in)", y = "weight (lbs)")

handw_log <- handw %>%
  mutate(hlog = log(height_in),
         wlog = log(weight_lbs))

ggplot(handw_log, aes(hlog, wlog)) +
  geom_point(alpha = .2)

handw_log %>%
  summarise(mean(hlog))

handw_log %>%
  summarise(sd(hlog))

handw_log %>%
  summarise(mean(wlog))

handw_log %>%
  summarise(sd(wlog))

cor(x = handw_log$hlog, y = handw_log$wlog)

handw_log %>%
  correlate() %>%
  shave() %>%
  fashion()

my_cov <- .96 * .26 * .65

my_Sigma <- matrix(c(.26^2, my_cov, my_cov, .65^2), ncol = 2)
my_Sigma

set.seed(62)
log_ht_wt <- MASS::mvrnorm(6,
                           c(height = 4.11, weight = 4.74),
                           my_Sigma)
log_ht_wt

exp(log_ht_wt)

new_humans <- MASS::mvrnorm(500,
                            c(height_in = 4.11, weight_lbs = 4.74),
                            my_Sigma) %>%
  exp() %>%
  as_tibble() %>%
  mutate(type = "simulated")

alldata <- bind_rows(handw %>% mutate(type = "real"),
                     new_humans)

ggplot(alldata, aes(height_in, weight_lbs)) +
  geom_point(aes(color = type), alpha = .1)

b1 <- .96 * (.65/.26)
b1

lm(wlog ~ hlog, data = handw_log)

summary(lm(wlog ~ hlog, handw_log))

ggplot(handw_log, aes(hlog, wlog)) +
  geom_point(alpha = .2) +
  labs(x = "log(height)", y = "log(weight)") +
  geom_abline(intercept = -5.124, slope = 2.4, color = "blue") +
  geom_abline(intercept = -5.270, slope = 2.433, color = "red")
