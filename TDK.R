install.packages("tidyverse")
library(tidyverse)
library(dplyr)
library(readr)
install.packages("readxl")
library(readxl)
install.packages("stringr")
library(stringr)
library(ggplot2)
my_data = read_excel("C:/ELTE_ST/Additional research activity/TDK/tdk_project/tdk_data_cleaned.xlsx", sheet = 1)
my_data
view(my_data)

t.test(my_data$acceptance_delay, my_data1$acceptance_delay)
effectsize::cohens_d(my_data$acceptance_delay, my_data1$acceptance_delay, pooled_sd = TRUE, mu = 0, paired = FALSE)
mean(my_data$acceptance_delay)
mean(my_data1$acceptance_delay)
t.test(my_data$is_retracted, my_data1$is_retracted)
cor.test(my_data1$article_date, my_data1$acceptance_delay)


class(my_data$acceptance_delay)

my_data_ai1_retracted = my_data_ai1 %>% 
  select(is_retracted) %>% 
  summary()
my_data_ai1_retracted

my_data_ai2_retracted = my_data_ai2 %>% 
  select(is_retracted) %>% 
  summary()
my_data_ai2_retracted  
retracted_table = merge.data.frame(my_data_ai1_retracted, my_data_ai2_retracted)
retracted_table  

patterns <- "\\bAI\\b|\\bAI-|ChatGPT|OpenAI|Generative AI|\\bLLM\\b|\\bLLMs\\b|Large language models|Chat GPT|GPT-3.5|GPT-4|\\bGPT\\b"


my_data_ai1 = my_data %>%
  filter(
    str_detect(title, regex(patterns, ignore_case = TRUE)) |
      str_detect(keywords, regex(patterns, ignore_case = TRUE))
  ) %>% 
  mutate(article_date = as.Date(article_date))


threshold <- as.Date("2022-11-30") + median(my_data$acceptance_delay)

my_data_ai2 = my_data%>% 
  filter(
    str_detect(title, regex(patterns, ignore_case = TRUE)) |
      str_detect(keywords, regex(patterns, ignore_case = TRUE))
  ) %>% 
  mutate(
    article_date = as.Date(article_date)
  ) %>% 
  filter(article_date > threshold)

my_data_control = my_data %>% 
  filter(article_date > threshold) %>% 
  semi_join(my_data_ai2, by = "journal") %>% 
  filter(!( str_detect(title, regex(patterns, ignore_case = TRUE)) |
              str_detect(keywords, regex(patterns, ignore_case = TRUE))
  ))
  
my_data_ai2 = my_data_ai2 %>% 
  mutate(journal = factor(journal))
levels(my_data_ai2$journal)

my_data_control= my_data_control %>% 
  mutate(journal = factor(journal))
levels(my_data_control$journal)

my_data_control_sliced = my_data_control %>% 
  slice_sample(n = nrow(my_data_ai2)) %>% 
  arrange(article_date)

my_data_control_sliced %>% 
ggplot()+
  aes(x = article_date, y = acceptance_delay) %>% 
  geom_point()+
  aes(x = article_date, y = acceptance_delay)+
  geom_smooth()

my_data_ai2 %>% 
  ggplot()+
  aes(x = article_date, y = acceptance_delay) %>% 
  geom_point()+
  aes(x = article_date, y = acceptance_delay)+
  geom_smooth()

t.test(my_data_ai2$acceptance_delay, my_data_control_sliced$acceptance_delay)
t.test(my_data_ai2$publication_delay, my_data_control_sliced$publication_delay)


patterns_2 = "\\bUS-election|Clinton|Donald Trump|2016-elections|presidential|president|Hillary|\\b2016 election\\b|2016-election|
voting|left party|right party|left wing|right wing|left-wing|right-wing|presidential|political|politics"
threshold_2 = as.Date("2019-12-12")

my_data_elections1 = my_data %>%
  filter(
    str_detect(title, regex(patterns_2, ignore_case = TRUE)) |
      str_detect(keywords, regex(patterns_2, ignore_case = TRUE))
  ) %>% 
  mutate(article_date = as.Date(article_date))

my_data_elections2 = my_data%>% 
  filter(
    str_detect(title, regex(patterns_2, ignore_case = TRUE)) |
      str_detect(keywords, regex(patterns_2, ignore_case = TRUE))
  ) %>% 
  mutate(
    article_date = as.Date(article_date)
  ) %>% 
  filter(article_date < threshold_2)

my_data_control_elections = my_data %>% 
  filter(article_date > threshold_2) %>% 
  semi_join(my_data_elections2, by = "journal") %>% 
  filter(!( str_detect(title, regex(patterns_2, ignore_case = TRUE)) |
              str_detect(keywords, regex(patterns_2, ignore_case = TRUE))
  ))

my_data_elections2 = my_data_elections2 %>% 
  mutate(journal = factor(journal))
levels(my_data_ai2$journal)

my_data_control_elections= my_data_control_elections %>% 
  mutate(journal = factor(journal))
levels(my_data_control_elections$journal)

my_data_control_elections_sliced = my_data_control_elections %>% 
  slice_sample(n = nrow(my_data_elections2)) %>% 
  arrange(article_date)

t.test(my_data_elections2$acceptance_delay, my_data_control_elections_sliced$acceptance_delay)
t.test(my_data_elections2$publication_delay, my_data_control_elections_sliced$publication_delay)






patterns_3 = "\\bracism\\b|\\bracial\\b|anti-racism|anti racism|protest|black lives matter|fascism|police brutality"
threshold_3 = as.Date("2020-05-25")
threshold_4 = as.Date("2023-01-01")
my_data_blm1 = my_data %>%
  filter(
    str_detect(title, regex(patterns_3, ignore_case = TRUE)) |
      str_detect(keywords, regex(patterns_3, ignore_case = TRUE))
  ) %>% 
  mutate(article_date = as.Date(article_date))

my_data_blm2 = my_data%>% 
  filter(
    str_detect(title, regex(patterns_3, ignore_case = TRUE)) |
      str_detect(keywords, regex(patterns_3, ignore_case = TRUE))
  ) %>% 
  mutate(
    article_date = as.Date(article_date)
  ) %>% 
  filter(article_date < threshold_3)

my_data_control_blm = my_data %>% 
  filter (article_date > threshold_3) %>%
  filter(article_date < threshold_4) %>% 
  semi_join(my_data_blm2, by = "journal") %>% 
  filter(!( str_detect(title, regex(patterns_3, ignore_case = TRUE)) |
              str_detect(keywords, regex(patterns_3, ignore_case = TRUE))
  ))

my_data_blm2 = my_data_blm2 %>% 
  mutate(journal = factor(journal))
levels(my_data_blm2$journal)

my_data_control_blm= my_data_control_blm %>% 
  mutate(journal = factor(journal))
levels(my_data_control_blm$journal)

my_data_control_blm_sliced = my_data_control_blm %>% 
  slice_sample(n = nrow(my_data_blm2)) %>% 
  arrange(article_date)

t.test(my_data_blm2$acceptance_delay, my_data_control_blm_sliced$acceptance_delay)
t.test(my_data_blm2$publication_delay, my_data_control_blm_sliced$publication_delay)


