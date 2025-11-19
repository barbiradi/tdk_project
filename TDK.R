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
  slice_sample(n = 527)

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

