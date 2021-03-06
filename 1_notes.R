library(tidyverse)
set.seed(2020)
parents <- rnorm(n = 50, mean = 480, sd = 40)
parents
control <- rnorm(n = 50, mean = 500, sd = 40)
dat <- tibble(group = rep(c("parent", "control"),
                          c(length(parents),
                            length(control))),
              rt = c(parents, control))
dat
