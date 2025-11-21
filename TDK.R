install.packages("tidyverse")
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
as.
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
  filter(as.Date(article_date) < threshold_2) %>% 
  semi_join(my_data_elections2, by = "journal") %>% 
  filter(!( str_detect(title, regex(patterns_2, ignore_case = TRUE)) |
              str_detect(keywords, regex(patterns_2, ignore_case = TRUE))
  ))

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






patterns_3 = "black lives matter|blm|blacklivesmatter|\\bblm protest|blm movement|
george floyd|racial justice movement|racial justice protest|racial reckoning|
police brutality|police violence|systemic racism|institutional racism|
racial profiling|racialized violence|\\bracism|racial discrimination|racial inequality|black men|black people
african americans|african american|black youth|ethnic discrimination|racial/ethnic|\\bracial|other-race|black women|black student
|black children|black adolescent|black girl|black sexual minority|black american"
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
  filter(article_date < threshold_4) %>% 
  filter(article_date > threshold_3)

my_data_control_blm = my_data %>% 
  filter (as.Date(article_date) > threshold_3) %>%
  filter(as.Date(article_date) < threshold_4) %>% 
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
mean(my_data_control_blm$acceptance_delay)
mean(my_data_blm2$acceptance_delay)

"Elemzes"
shapiro.test(my_data_ai2$acceptance_delay)
shapiro.test(my_data_blm2$äcceptance_delay)
shapiro.test(my_data_elections2$acceptance_delay)

my_data_ai2 %>%
  ggplot()+
  aes(x = acceptance_delay)+
  geom_histogram(binwidth = 5)

my_data_control %>% 
  ggplot()+
  aes(x = acceptance_delay)+
  geom_histogram()

library(boot)


boot_samples_AI <- replicate(1000,
                          my_data_control[sample(1:nrow(my_data_control), nrow(my_data_ai2), replace = TRUE), ],
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


boot_samples_blm <- replicate(1000,
                             my_data_control_blm[sample(1:nrow(my_data_control_blm), nrow(my_data_blm2), replace = TRUE), ],
                             simplify = FALSE
)


boot_means_df_blm <- data.frame(
  bootstrap_id = 1:length(boot_samples_blm),
  mean_acceptance_delay = sapply(boot_samples_blm, \(df) mean(df$acceptance_delay, na.rm = TRUE))
)
ci_blm <- quantile(boot_means_df_blm$mean_acceptance_delay,
                  probs = c(0.025, 0.975))
ci_blm
mean(my_data_blm2$acceptance_delay)

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

ggplot(boot_means_df_blm, aes(x = mean_acceptance_delay)) +
  geom_histogram(bins = 30, fill = "skyblue", color = "black", alpha = 0.7) +
  geom_vline(xintercept = ci_blm, color = "red", linetype = "dashed", linewidth = 1) + # 95% CI
  geom_vline(xintercept = mean(my_data_blm2$acceptance_delay), color = "darkgreen", linetype = "solid", size = 1.2) +
  annotate("text", x = mean(my_data_elections2$acceptance_delay), y = max(table(cut(boot_means_df_blm$mean_acceptance_delay, breaks=30))) * 0.9,
           label = "Sample Mean", color = "darkgreen", angle = 90, vjust = -0.5) +
  labs(title = "Bootstrap Distribution of Mean Acceptance Delay",
       x = "Mean Acceptance Delay",
       y = "Frequency") +
  theme_minimal()
"?Bayes?"
as.

