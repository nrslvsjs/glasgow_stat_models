library("corrr")
library(tidyverse)

grades <- read_csv("data/grades.csv", col_types = "ddii")

grades

grades %>%
  correlate() %>%
  shave() %>%
  fashion()

pairs(grades)
 
my_model <- lm(grade ~ lecture + nclicks, grades)

summary(my_model) 

new_data <- tribble(~lecture, ~nclicks,
                     3, 70,
                     10, 130,
                     0, 20,
                     5, 100) 

new_data 

predict(my_model, new_data)

new_data %>%
  mutate(predicted_grade = predict(my_model, new_data))

nclicks_mean <- grades %>% pull(nclicks) %>% mean()

nclicks_mean

new_lecture <- tibble(lecture = 0:10,
                      nclicks = nclicks_mean)
new_lecture

new_lecture2 <- new_lecture %>%
  mutate(grade = predict(my_model, new_lecture))

new_lecture2

ggplot(grades, aes(lecture, grade)) +
  geom_point() +
  geom_line(data = new_lecture2)

nlectures_mean <- grades %>% pull(lecture) %>% mean()

new_nclicks <- tibble(lecture = nlectures_mean,
                      nclicks = 50:150)

new_nclicks2 <- new_nclicks %>%
  mutate(grade = predict(my_model, new_nclicks))

ggplot(grades, aes(nclicks, grade)) +
  geom_point() +
  geom_line(data = new_nclicks2)


lecture_mean

min_nclicks <- grades %>% pull(nclicks) %>% min()
min_nclicks
max_nclicks <- grades %>% pull(nclicks) %>% max()
max_nclicks

new_nclicks <- tibble(lecture = lecture_mean,
                      nclicks = min_nclicks:max_nclicks)
new_nclicks

new_nclicks2 <- new_nclicks %>%
  mutate(grade = predict(my_model, new_nclicks))

new_nclicks2

ggplot(grades, aes(nclicks, grade)) +
  geom_point() +
  geom_line(data = new_nclicks2, color = "blue")

grades2 <- grades %>%
  mutate(lecture_c = (lecture - mean(lecture)) / sd(lecture),
         nclicks_c = (nclicks - mean(nclicks)) / sd(nclicks))

grades2

my_model_scaled <- lm(grade ~ lecture_c + nclicks_c, grades2)

my_model_scaled

summary(my_model_scaled)

m1 <- lm(grade ~ GPA, grades)
m2 <- lm(grade ~ GPA + lecture + nclicks, grades)

anova(m1, m2)

summary(m1)
summary(m2)
