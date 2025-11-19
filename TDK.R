install.packages("tidyverse")
library(tidyverse)
library(dplyr)
library(readr)
install.packages("readxl")
library(readxl)
install.packages("stringr")
library(stringr)
library(ggplot2)
my_data = read_excel("C:/ELTE - studies related/Additional research activity/tdk_data_cleaned.xlsx", sheet = 1)
my_data
view(my_data)


my_data1 <- my_data %>%
  filter(str_detect(title, regex("\\bAI\\b|\\bAI-\\b|ChatGPT|OpenAI|Generative AI|LLM|LLMs|Large language models")))
  filter(str_detect(keywords, regex("\\bAI\\b|\\bAI-\\b|ChatGPT|OpenAI|Generative AI|LLM|LLMs|Large language models")))



t.test(my_data$acceptance_delay, my_data1$acceptance_delay)
effectsize::cohens_d(my_data$acceptance_delay, my_data1$acceptance_delay, pooled_sd = TRUE, mu = 0, paired = FALSE)
mean(my_data$acceptance_delay)
mean(my_data1$acceptance_delay)
t.test(my_data$is_retracted, my_data1$is_retracted)
cor.test(my_data1$article_date, my_data1$acceptance_delay)


my_data_ai1 <- my_data %>%  
  mutate(
    article_date_numeric = as.numeric(article_date),
    article_date_date = as.Date(article_date)) %>% 
  filter((article_date > as.Date("2022-11-30") + median(my_data_ai1$acceptance_delay)))

class(my_data$acceptance_delay)
my_data_ai2 <- my_data1 %>% 
  mutate(
    article_date_numeric = as.numeric(article_date),
    article_date_date = as.Date(article_date)) %>% 
  filter((article_date > as.Date("2022-11-30") + median(my_data_ai1$acceptance_delay)))

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


my_data_ai1 <- my_data %>% 
  mutate(
    article_date = as.Date(article_date),
  )
my_data_ai2 <- my_data1 %>% 
  mutate(
    article_date = as.Date(article_date),
  )

threshold <- as.Date("2022-11-30") + median(my_data$acceptance_delay)

my_data_ai1 <- my_data_ai1 %>% 
  filter(article_date > threshold)
my_data_ai2 = my_data_ai2 %>% 
  filter(article_date > threshold)

ggplot()+
  aes(x = article_date, y = acceptance_delay) %>% 
  geom_smooth()

my_data_ai2 %>% 
  ggplot()+
  aes(x = article_date, y = acceptance_delay) %>% 
  geom_smooth()

