
install.packages("tidyverse")
install.packages("rstatix")
library(rstatix)
library(tidyverse)
library(dplyr)
install.packages("boot")
library(boot)
library(readr)
install.packages("readxl")
library(readxl)
install.packages("stringr")
library(stringr)
library(ggplot2)
my_data = read_excel("C:/ELTE_ST/Additional research activity/TDK/tdk_project/tdk_data_cleaned.xlsx", sheet = 1)
my_data <- read.csv("tdk_data_cleaned.csv")







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

my_data_control_AI = my_data %>% 
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

my_data_control_sliced_AI = my_data_control %>% 
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


patterns_2 = "\\b2016 election|2016 presidential election|us 2016 election|
u\\.s\\. 2016 election|2016 us presidential|2016 u\\.s\\. presidential|Donald Trump|left wing|right wing|parties|
voting|Donald J. Trump|president|presidential|political|politician|American election|United States election|US election"
threshold_2 = as.Date("2019-12-12")

my_data_elections1 <- my_data %>%
  filter(
    str_detect(title, regex(patterns_2, ignore_case = TRUE)) |
      str_detect(keywords, regex(patterns_2, ignore_case = TRUE))
  ) %>% 
  mutate(article_date = as.Date(article_date))
  

my_data_elections2 <- my_data %>% 
  filter(
    str_detect(title, regex(patterns_2, ignore_case = TRUE)) |
      str_detect(keywords, regex(patterns_2, ignore_case = TRUE))
  ) %>%
  mutate(article_date = as.Date(article_date)) %>%
  filter(article_date < threshold_2)
  



my_data_control_elections <- my_data %>%
  mutate(article_date = as.Date(article_date)) %>%
  filter(article_date < threshold_2) %>%      
  semi_join(my_data_elections2, by = "journal") %>% 
  filter(!(str_detect(title, regex(patterns_2, ignore_case = TRUE)) |str_detect(keywords, regex(patterns_2, ignore_case = TRUE))))

my_data_elections2 = my_data_elections2 %>% 
  mutate(journal = factor(journal))
levels(my_data_elections2$journal)

my_data_control_elections= my_data_control_elections %>% 
  mutate(journal = factor(journal))
levels(my_data_control_elections$journal)

my_data_control_elections_sliced = my_data_control_elections %>% 
  slice_sample(n = nrow(my_data_elections2)) %>% 
  arrange(article_date)

t.test(my_data_elections2$acceptance_delay, my_data_control_elections_sliced$acceptance_delay)
t.test(my_data_elections2$publication_delay, my_data_control_elections_sliced$publication_delay)

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



"Elemzes"
shapiro.test(my_data_ai2$acceptance_delay)
shapiro.test(my_data_elections2$acceptance_delay)

my_data_ai2 %>%
  ggplot()+
  aes(x = acceptance_delay)+
  geom_histogram(binwidth = 5)

my_data_control_AI %>% 
  ggplot()+
  aes(x = acceptance_delay)+
  geom_histogram()

library(boot)


boot_samples_AI <- replicate(1000,
                          my_data_control_AI[sample(1:nrow(my_data_control_AI), nrow(my_data_ai2), replace = TRUE), ],
                          simplify = FALSE
)


boot_means_df_AI <- data.frame(
  bootstrap_id = 1:length(boot_samples_AI),
  mean_acceptance_delay = sapply(boot_samples_AI, \(df) mean(df$acceptance_delay, na.rm = TRUE))
)
ci_AI <- quantile(boot_means_df_AI$mean_acceptance_delay,
               probs = c(0.025, 0.975))
ci_AI
mean(my_data_ai2$acceptance_delay)

boot_samples_election <- replicate(1000,
                             my_data_control_elections[sample(1:nrow(my_data_control_elections), nrow(my_data_elections2), replace = TRUE), ],
                             simplify = FALSE
)


boot_means_df_election <- data.frame(
  bootstrap_id = 1:length(boot_samples_election),
  mean_acceptance_delay = sapply(boot_samples_election, \(df) mean(df$acceptance_delay, na.rm = TRUE))
)
ci_election <- quantile(boot_means_df_election$mean_acceptance_delay,
                  probs = c(0.025, 0.975))
ci_election
mean(my_data_elections2$acceptance_delay)




ggplot(boot_means_df_AI, aes(x = mean_acceptance_delay)) +
  geom_histogram(bins = 30, fill = "skyblue", color = "black", alpha = 0.7) +
  geom_vline(xintercept = ci_AI, color = "red", linetype = "dashed", linewidth = 1) + # 95% CI
  geom_vline(xintercept = mean(my_data_ai2$acceptance_delay), color = "darkgreen", linetype = "solid", size = 1.2) +
  annotate("text", x = mean(my_data_ai2$acceptance_delay), y = max(table(cut(boot_means_df_AI$mean_acceptance_delay, breaks=30))) * 0.9,
           label = "Sample Mean", color = "darkgreen", angle = 90, vjust = -0.5) +
  labs(title = "Bootstrap Distribution of Mean Acceptance Delay",
       x = "Mean Acceptance Delay",
       y = "Frequency") +
  theme_minimal()

ggplot(boot_means_df_election, aes(x = mean_acceptance_delay)) +
  geom_histogram(bins = 30, fill = "skyblue", color = "black", alpha = 0.7) +
  geom_vline(xintercept = ci_election, color = "red", linetype = "dashed", linewidth = 1) + # 95% CI
  geom_vline(xintercept = mean(my_data_elections2$acceptance_delay), color = "darkgreen", linetype = "solid", size = 1.2) +
  annotate("text", x = mean(my_data_elections2$acceptance_delay), y = max(table(cut(boot_means_df_election$mean_acceptance_delay, breaks=30))) * 0.9,
           label = "Sample Mean", color = "darkgreen", angle = 90, vjust = -0.5) +
  labs(title = "Bootstrap Distribution of Mean Acceptance Delay",
       x = "Mean Acceptance Delay",
       y = "Frequency") +
  theme_minimal()


