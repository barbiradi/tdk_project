library(tidyverse)
library(readxl)
library(effectsize)

my_data <- read.csv("tdk_data_cleaned.csv")
View(my_data)

class(my_data$acceptance_delay)

patterns <- "COVID-19|Covid19|\\bCovid\\b|Coronavirus|Corona virus|SARS-CoV-2|\\bSARS\\b|SARS-CoV|2019-ncov"
  
my_data_covid1 <- my_data %>%
  filter(
    str_detect(title,regex(patterns,ignore_case = TRUE)) |
    str_detect(keywords,regex(patterns,ignore_case = TRUE))
  ) %>%
  mutate(
    article_date =as.Date(article_date)
  )

threshold_covid_start <- as.Date("2020-03-11")
threshold_covid_end <- as.Date("2021-12-31")

my_data_covid2 <- my_data %>%
  filter(
    str_detect(title,regex(patterns,ignore_case = TRUE)) |
      str_detect(keywords,regex(patterns,ignore_case = TRUE))
  ) %>%
  mutate(
    article_date =as.Date(article_date)
  ) %>%
  filter(
   article_date >= threshold_covid_start,
   article_date <= threshold_covid_end
  )

my_data_control_covid = my_data %>%
  filter(
    article_date >= threshold_covid_start,
    article_date <= threshold_covid_end
  ) %>%
  semi_join(my_data_covid2, by= "journal") %>%
  filter(!(str_detect(title, regex(patterns,ignore_case = TRUE)) |
             str_detect(keywords, regex(patterns, ignore_case = TRUE))
        ))

my_data_covid2 = my_data_covid2 %>%
  mutate(journal = factor(journal))
levels(my_data_covid2$journal)

my_data_control_covid = my_data_control_covid %>%
  mutate(journal = factor(journal))
levels(my_data_control_covid$journal)


my_data_control_covid_sliced = my_data_control_covid %>% 
  slice_sample(n = nrow(my_data_covid2)) %>%
  arrange(article_date)

my_data_control_covid_sliced %>%
  ggplot(aes(x = article_date, y = acceptance_delay)) +
  geom_point() +
  geom_smooth()

my_data_covid2 %>%
  ggplot(aes(x = article_date, y = acceptance_delay)) +
  geom_point() +
  geom_smooth()

t.test(my_data_covid2$acceptance_delay,my_data_control_covid_sliced$acceptance_delay)
t.test(my_data_covid2$publication_delay,my_data_control_covid_sliced$publication_delay)



patterns_2 <- "Russia-Ukraine war|Russo-Ukrainian war|Russian-Ukrainian conflict|Russian conflict|Ukrainian conflict|Russian invasion|Russian war|Ukrainian war|Russian attack|Russia|Ukraine|Donbas conflict|Donbas war|Donbas|Luhansk|war in Eastern Ukraine|Russian offensive|Ukrainian offensive|Ukraine humanitarian crisis|Crimea invasion"

my_data_RUwar1 <- my_data %>%
  filter(
    str_detect(title,regex(patterns_2,ignore_case = TRUE)) |
      str_detect(keywords,regex(patterns_2,ignore_case = TRUE))
  ) %>%
  mutate(
    article_date =as.Date(article_date)
  )

threshold_RUwar_start <- as.Date("2022-02-24")
threshold_RUwar_end <- max(my_data$article_date, na.rm = TRUE)

my_data_RUwar2 <- my_data %>%
  filter(
    str_detect(title,regex(patterns_2,ignore_case = TRUE)) |
      str_detect(keywords,regex(patterns_2,ignore_case = TRUE))
  ) %>%
  mutate(
    article_date =as.Date(article_date)
  ) %>%
  filter(
    article_date >= threshold_RUwar_start,
    article_date <= threshold_RUwar_end
  )

my_data_control_RUwar = my_data %>%
  filter(
    article_date >= threshold_RUwar_start,
    article_date <= threshold_RUwar_end
  ) %>%
  semi_join(my_data_RUwar2, by= "journal") %>%
  filter(!(str_detect(title, regex(patterns_2,ignore_case = TRUE)) |
             str_detect(keywords, regex(patterns_2, ignore_case = TRUE))
  ))

my_data_RUwar2 = my_data_RUwar2 %>%
  mutate(journal = factor(journal))
levels(my_data_RUwar2$journal)

my_data_control_RUwar = my_data_control_RUwar %>%
  mutate(journal = factor(journal))
levels(my_data_control_RUwar$journal)

my_data_control_RUwar_sliced = my_data_control_RUwar %>% 
  slice_sample(n = nrow(my_data_RUwar2)) %>%
  arrange(article_date)

my_data_control_RUwar_sliced %>%
  ggplot(aes(x = article_date, y = acceptance_delay)) +
  geom_point() +
  geom_smooth()

my_data_RUwar2 %>%
  ggplot(aes(x = article_date, y = acceptance_delay)) +
  geom_point() +
  geom_smooth()

t.test(my_data_RUwar2$acceptance_delay,my_data_control_RUwar_sliced$acceptance_delay)
t.test(my_data_RUwar2$publication_delay,my_data_control_RUwar_sliced$publication_delay)



