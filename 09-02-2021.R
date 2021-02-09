# Get the Data

# Read in with tidytuesdayR package 
# Install from CRAN via: install.packages("tidytuesdayR")
# This loads the readme and all the datasets for the week of interest

# Either ISO-8601 date or year/week works!


lifetime_earn <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-02-09/lifetime_earn.csv')
student_debt <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-02-09/student_debt.csv')
retirement <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-02-09/retirement.csv')
home_owner <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-02-09/home_owner.csv')
race_wealth <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-02-09/race_wealth.csv')
income_time <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-02-09/income_time.csv')
income_limits <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-02-09/income_limits.csv')
income_aggregate <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-02-09/income_aggregate.csv')
income_distribution <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-02-09/income_distribution.csv')
income_mean <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-02-09/income_mean.csv')
lifetime_earn <- tuesdata$lifetime_earn


# LIBRARY

library(skimr)
library(tidyverse)
library(directlabels)

skim(income_distribution)

# $$ Q1: IS THE INCOME DIFFERENT DEPENDING THE RACE? $$----

income_distribution_time_series <- income_distribution %>%
  select(-income_distribution,-income_bracket) %>%
  distinct()


income_distribution_time_series %>%
  filter(race %in% list("White Alone",
                        "Black Alone",
                        "Asian Alone",
                        "Hispanic (Any Race)")) %>%
  ggplot(aes(x = year)) +
  geom_line(aes(y = income_median, color = race, size = 1.2)) +
  facet_wrap(~race)

income_distribution_time_series %>%
  filter(race %in% list("White Alone",
                        "Black Alone",
                        "Asian Alone",
                        "Hispanic (Any Race)")) %>%
  ggplot(aes(x = year)) +
  geom_line(aes(y = income_mean, color = race, size = 1.2)) +
  facet_wrap(~race)


# ASIAN people has more income than the rest
# BLACK people has the least income 

unique(retirement$race)



# $$ Q2: need black people credits for study in comparision of the rest? $$----


student_debt  %>%
  ggplot() +
  geom_line(aes(x = year, y = loan_debt_pct, color = race ))

# yes, black people need more loans than others





# $$ Q3:  ARE THERE DIFFERENT EVOLUTIONS ON THE INCOME BY PERCENTILE ALONG THE YEARS? ------

income_time %>%
  ggplot() +
  geom_line(aes(x = year, y = income_family, color = percentile ))

# top 10% has increse their income 





# $$ Q4: IS THE INCOME INCREASING EQUALLY AMONG THE RACES?----------

income_time_pivot <- income_time %>%
  pivot_wider(names_from = percentile, values_from = income_family)


income_mean_pivot <- income_mean  %>%
  filter(race %in% list("White Alone",
                        "Black Alone",
                        "Asian Alone",
                        "Hispanic (Any Race)")) %>%
  group_by(year, race) %>%
  summarise(mean_income_dollars = mean(income_dollars)) %>%
  pivot_wider(names_from = race, values_from = mean_income_dollars)


join_percentile_races_timeseries <- full_join(income_mean_pivot, income_time_pivot, by = "year") %>%
  janitor::clean_names()


colors <- c("BLACK RACE" = "green3", "WHITE RACE" = "red", "ASIAN RACE" = "orange")

join_percentile_races_timeseries %>%
  filter(year > 1968,
         year < 2018) %>%
  ggplot(aes(x = year)) +
  geom_ribbon(aes(ymin = x50th, ymax = x90th), fill = "blue", alpha = 0.2) +
  geom_ribbon(aes(ymin = x10th, ymax = x50th), fill = "blue", alpha = 0.2) +
  geom_line(
    aes(y = black_alone, color = "BLACK RACE"),size = 1.2) +
  geom_line(
    aes(y = white_alone, color = "WHITE RACE"),size = 1.2) +
  geom_line(
    aes(y = asian_alone, color = "ASIAN RACE"),size = 1.2) +
  geom_line(
    aes(y = x10th)) +
  geom_line(
    aes(y = x50th)) +
  geom_line(
    aes(y = x90th)) +
  labs(x = "YEAR", y = "INCOME") +
  labs(title = "TOP 5 % INCOME PER RACE IN MEAN",
       subtitle = "Cloud = 90, 50, 10 percentile of income",
       color = "Legend") + 
  theme_minimal() +
  scale_color_manual(values = colors)
  

geom_dl
# answer: the top 5 are increasing their income across all races. 
# Still among the top 5% there are differences between races


