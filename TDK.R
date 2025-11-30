
install.packages("rstatix")
library(rstatix)
library(tidyverse)
library(dplyr)
library(boot)
library(readr)
library(readxl)
library(stringr)
library(ggplot2)
my_data = read.csv("C:/ELTE_ST/Additional research activity/TDK/tdk_project/tdk_data.csv")
my_data <- read.csv("tdk_data.csv")

my_data = my_data %>% 
  mutate(article_date = as.Date(article_date))

my_data_grouped = my_data %>% 
  group_by(asjc) %>% 
  summarise()

sd(my_data$acceptance_delay)
min(my_data$article_date)
max(my_data$article_date)
mean(my_data$acceptance_delay)
median(my_data$acceptance_delay)
my_data %>% 
  ggplot()+
  aes(x = acceptance_delay)+
  geom_density()


patterns_AI <- "\\bAI\\b|\\bAI-|ChatGPT|OpenAI|Generative AI|\\bLLM\\b|\\bLLMs\\b|Large language models|Chat GPT|GPT-3.5|GPT-4|\\bGPT\\b"


my_data_ai1 = my_data %>%
  filter(
    str_detect(title, regex(patterns_AI, ignore_case = TRUE)) |
      str_detect(keywords, regex(patterns_AI, ignore_case = TRUE))
  ) %>% 
  mutate(article_date = as.Date(article_date))


threshold_AI <- as.Date("2022-11-30") + median(my_data$acceptance_delay)

my_data_ai2 = my_data%>% 
  filter(
    str_detect(title, regex(patterns_AI, ignore_case = TRUE)) |
      str_detect(keywords, regex(patterns_AI, ignore_case = TRUE))
  ) %>% 
  mutate(
    article_date = as.Date(article_date)
  ) %>% 
  filter(article_date > threshold_AI)

my_data_control_AI = my_data %>% 
  filter(article_date > threshold_AI) %>% 
  semi_join(my_data_ai2, by = "asjc") %>% 
  filter(!( str_detect(title, regex(patterns_AI, ignore_case = TRUE)) |
              str_detect(keywords, regex(patterns_AI, ignore_case = TRUE))
  ))
  
my_data_ai2 = my_data_ai2 %>% 
  mutate(journal = factor(journal))
levels(my_data_ai2$journal)



my_data_ai2 %>% 
  ggplot()+
  aes(x = article_date, y = acceptance_delay) %>% 
  geom_point()+
  aes(x = article_date, y = acceptance_delay)+
  geom_smooth()


patterns_elections = "\\b2016 election|2016 presidential election|us 2016 election|
u\\.s\\. 2016 election|2016 us presidential|2016 u\\.s\\. presidential|Donald Trump|left wing|right wing|parties|
voting|Donald J. Trump|president|presidential|American election|United States election|US election|republican|democratic party|democrats|US economy|American econom|\\bTrump\\b"
threshold_elections = as.Date("2020-01-01")
threshol_elections_upper = as.Date("2023-12-31")
my_data_elections1 <- my_data %>%
  filter(
    str_detect(title, regex(patterns_elections, ignore_case = TRUE)) |
      str_detect(keywords, regex(patterns_elections, ignore_case = TRUE))
  ) %>% 
  mutate(article_date = as.Date(article_date))
  

my_data_elections2 <- my_data %>% 
  filter(
    str_detect(title, regex(patterns_elections, ignore_case = TRUE)) |
      str_detect(keywords, regex(patterns_elections, ignore_case = TRUE))
  ) %>%
  mutate(article_date = as.Date(article_date)) %>%
  filter(article_date < threshold_elections)
  



my_data_control_elections <- my_data %>%
  mutate(article_date = as.Date(article_date)) %>%
  filter(article_date < threshold_elections) %>% 
  semi_join(my_data_elections2, by = "asjc") %>% 
  filter(!(str_detect(title, regex(patterns_elections, ignore_case = TRUE)) |str_detect(keywords, regex(patterns_elections, ignore_case = TRUE))))


my_data_control_elections_sliced = my_data_control_elections %>% 
  slice_sample(n = nrow(my_data_elections2)) %>% 
  arrange(article_date)

patterns_COVID <- "COVID-19|Covid19|\\bCovid\\b|Coronavirus|Corona virus|SARS-CoV-2|\\bSARS\\b|SARS-CoV|2019-ncov"

my_data_covid1 <- my_data %>%
  filter(
    str_detect(title,regex(patterns_COVID,ignore_case = TRUE)) |
      str_detect(keywords,regex(patterns_COVID,ignore_case = TRUE))
  ) %>%
  mutate(
    article_date =as.Date(article_date)
  )

threshold_covid_start <- as.Date("2020-03-11")
threshold_covid_end <- as.Date("2022-12-31")

my_data_covid2 <- my_data %>%
  filter(
    str_detect(title,regex(patterns_COVID,ignore_case = TRUE)) |
      str_detect(keywords,regex(patterns_COVID,ignore_case = TRUE))
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
  semi_join(my_data_covid2, by= "asjc") %>%
  filter(!(str_detect(title, regex(patterns_COVID,ignore_case = TRUE)) |
             str_detect(keywords, regex(patterns_COVID, ignore_case = TRUE))
  ))


my_data_covid2 %>%
  ggplot(aes(x = article_date, y = acceptance_delay)) +
  geom_point() +
  geom_smooth()


patterns_RUwar <- "Ukrainian-Russian|Russia-Ukraine war|Russo-Ukrainian war|Russian-Ukrainian conflict|Russian conflict|Ukrainian conflict|Russian invasion|Russian war|Ukrainian war|Russian attack|Ukraine|Donbas conflict|Donbas war|Donbas|Luhansk|war in Eastern Ukraine|Russian offensive|Ukrainian offensive|Ukraine humanitarian crisis|Crimea invasion|Russian econom|Ukrainian-|Russia authoritarian|Russian mili|Russian imperial|Russian leader|\\bPutin\\b|Zelensky"

my_data_RUwar1 <- my_data %>%
  filter(
    str_detect(title,regex(patterns_RUwar,ignore_case = TRUE)) |
      str_detect(keywords,regex(patterns_RUwar,ignore_case = TRUE))
  ) %>%
  mutate(
    article_date =as.Date(article_date)
  ) %>% 
  bind_rows(my_data %>% filter(X == 92824| X == 113224|X == 127130| X == 100514| X == 81171))

threshold_RUwar_start <- as.Date("2022-02-24")
threshold_RUwar_end <- max(my_data$article_date, na.rm = TRUE)

my_data_RUwar2 <- my_data %>%
  filter(
    str_detect(title,regex(patterns_RUwar,ignore_case = TRUE)) |
      str_detect(keywords,regex(patterns_RUwar,ignore_case = TRUE))
  ) %>%
  mutate(
    article_date =as.Date(article_date)
  ) %>%
  filter(
    article_date > threshold_RUwar_start,
    article_date < threshold_RUwar_end
  )

my_data_control_RUwar = my_data %>%
  filter(
    article_date >= threshold_RUwar_start,
    article_date <= threshold_RUwar_end
  ) %>%
  semi_join(my_data_RUwar2, by= "asjc") %>%
  filter(!(str_detect(title, regex(patterns_RUwar,ignore_case = TRUE)) |
             str_detect(keywords, regex(patterns_RUwar, ignore_case = TRUE))
  ))

my_data_RUwar2 %>%
  ggplot(aes(x = article_date, y = acceptance_delay)) +
  geom_point() +
  geom_smooth()



"Elemzes"
shapiro.test(my_data_ai2$acceptance_delay)
shapiro.test(my_data_elections2$acceptance_delay)

summary(my_data_ai2$acceptance_delay)
sd(my_data_ai2$acceptance_delay)
summary(my_data_elections2$acceptance_delay)
sd(my_data_elections2$acceptance_delay)

summary(my_data_covid2$acceptance_delay)
sd(my_data_covid2$acceptance_delay)

summary(my_data_RUwar2$acceptance_delay)
sd(my_data_RUwar2$acceptance_delay)




my_data_ai2 %>%
  ggplot()+
  aes(x = acceptance_delay)+
  geom_histogram(binwidth = 5)

my_data_control_AI %>% 
  ggplot()+
  aes(x = acceptance_delay)+
  geom_histogram()

library(boot)

"bootstrap AI"
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



"bootstrap elections"
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


"bootstrap COVID"
boot_samples_COVID <- replicate(1000,
                                   my_data_control_covid[sample(1:nrow(my_data_control_covid), nrow(my_data_covid2), replace = TRUE), ],
                                   simplify = FALSE
)


boot_means_covid_sample <- data.frame(
  bootstrap_id = 1:length(boot_samples_COVID),
  mean_acceptance_delay = sapply(boot_samples_COVID, \(df) mean(df$acceptance_delay, na.rm = TRUE))
)

ci_covid <- quantile(boot_means_covid_sample$mean_acceptance_delay,
                        probs = c(0.025, 0.975))
ci_covid
mean(my_data_covid2$acceptance_delay)


"bootstrap RUWAR"
boot_samples_RUwar <- replicate(1000,
                                   my_data_control_RUwar[sample(1:nrow(my_data_control_RUwar), nrow(my_data_RUwar2), replace = TRUE), ],
                                   simplify = FALSE
)


boot_means_RUwar <- data.frame(
  bootstrap_id = 1:length(boot_samples_RUwar),
  mean_acceptance_delay = sapply(boot_samples_RUwar, \(df) mean(df$acceptance_delay, na.rm = TRUE))
)
ci_RUwar <- quantile(boot_means_RUwar$mean_acceptance_delay,
                        probs = c(0.025, 0.975))
ci_RUwar
mean(my_data_RUwar2$acceptance_delay)



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

ggplot(boot_means_covid_sample, aes(x = mean_acceptance_delay)) +
  geom_histogram(bins = 30, fill = "skyblue", color = "black", alpha = 0.7) +
  geom_vline(xintercept = ci_covid, color = "red", linetype = "dashed", linewidth = 1) +
  geom_vline(xintercept = mean(my_data_covid2$acceptance_delay),
             color = "darkgreen", linetype = "solid", size = 1.2) +
  annotate("text",
           x = mean(my_data_covid2$acceptance_delay),
           y = max(table(cut(boot_means_covid_sample$mean_acceptance_delay, breaks = 30))) * 0.9,
           label = "Sample Mean",
           color = "darkgreen",
           angle = 90,
           vjust = -0.5) +
  labs(title = "Bootstrap Distribution of Mean Acceptance Delay (COVID)",
       x = "Mean Acceptance Delay",
       y = "Frequency") +
  theme_minimal()

ggplot(boot_means_RUwar, aes(x = mean_acceptance_delay)) +
  geom_histogram(bins = 30, fill = "skyblue", color = "black", alpha = 0.7) +
  geom_vline(xintercept = ci_RUwar, color = "red", linetype = "dashed", linewidth = 1) +
  geom_vline(xintercept = mean(my_data_RUwar2$acceptance_delay),
             color = "darkgreen", linetype = "solid", size = 1.2) +
  annotate("text",
           x = mean(my_data_RUwar2$acceptance_delay),
           y = max(table(cut(boot_means_RUwar$mean_acceptance_delay, breaks = 30))) * 0.9,
           label = "Sample Mean",
           color = "darkgreen",
           angle = 90,
           vjust = -0.5) +
  labs(title = "Bootstrap Distribution of Mean Acceptance Delay (RU War)",
       x = "Mean Acceptance Delay",
       y = "Frequency") +
  theme_minimal()

my_data_ai1 %>% 
  ggplot()+
  aes(x = article_date,  )+
  geom_histogram()

my_data_covid1 %>% 
  ggplot()+
  aes(x = article_date,  )+
  geom_histogram()

my_data_RUwar1 %>% 
  ggplot()+
  aes(x = article_date,  )+
  geom_histogram()


date_bounds_AI <- my_data_ai1 %>%
  mutate(date_num = as.numeric(article_date)) %>%
  summarise(
    lower = as.Date(quantile(date_num, 0.025, na.rm = TRUE), origin = "1970-01-01"),
    upper = as.Date(quantile(date_num, 0.975, na.rm = TRUE), origin = "1970-01-01")
  )
date_bounds_AI

date_bounds_elections <- my_data_elections1 %>%
  mutate(date_num = as.numeric(article_date)) %>%
  summarise(
    lower = as.Date(quantile(date_num, 0.025, na.rm = TRUE), origin = "1970-01-01"),
    upper = as.Date(quantile(date_num, 0.975, na.rm = TRUE), origin = "1970-01-01")
  )
date_bounds_elections

date_bounds_covid <- my_data_covid1 %>%
  mutate(date_num = as.numeric(article_date)) %>%
  summarise(
    lower = as.Date(quantile(date_num, 0.025, na.rm = TRUE), origin = "1970-01-01"),
    upper = as.Date(quantile(date_num, 0.975, na.rm = TRUE), origin = "1970-01-01")
  )
date_bounds_covid

date_bounds_RUwar <- my_data_RUwar1 %>%
  mutate(date_num = as.numeric(article_date)) %>%
  summarise(
    lower = as.Date(quantile(date_num, 0.025, na.rm = TRUE), origin = "1970-01-01"),
    upper = as.Date(quantile(date_num, 0.975, na.rm = TRUE), origin = "1970-01-01")
  )
date_bounds_RUwar

ci_bound = bind_rows(ci_AI, ci_election, ci_covid, ci_RUwar) %>% 
  add_column(Téma = c("AI", "Választások", "COVID-19", "Orosz-ukrán konfliktus"))

df_ai_articled        <- my_data_ai1        %>% select(article_date) %>% mutate(source = "AI")
df_elections_articled <- my_data_elections1 %>% select(article_date) %>% mutate(source = "Elections")
df_covid_articled     <- my_data_covid1     %>% select(article_date) %>% mutate(source = "COVID")
df_ruwar_articled     <- my_data_RUwar1     %>% select(article_date) %>% mutate(source = "RUwar")

dates_bound <- bind_rows(df_ai_articled, df_elections_articled, df_covid_articled, df_ruwar_articled)

"Density plot"
ggplot(dates_bound, aes(x = as.numeric(article_date), color = source, fill = source)) +
  geom_density(alpha = 0.3, na.rm = TRUE) +
  scale_x_continuous(
    name = "Article Date",
    labels = function(x) as.Date(x, origin = "1970-01-01")
  ) +
  labs(
    title = "Density of Article Dates by Dataset",
    y = "Density"
  ) +
  theme_minimal()

library(dplyr)



"Massed effect"
sample_combined = bind_rows(my_data_ai2, my_data_elections2, my_data_covid2, my_data_RUwar2) %>% 
  distinct() %>% 
  arrange(article_date) 

date_bounds_massed = sample_combined %>% 
  mutate(date_num = as.numeric(article_date)) %>%
  summarise(
    lower = as.Date(quantile(date_num, 0.025, na.rm = TRUE), origin = "1970-01-01"),
    upper = as.Date(quantile(date_num, 0.975, na.rm = TRUE), origin = "1970-01-01")
  )

control_combined = my_data %>% 
  semi_join(sample_combined, by = "asjc") %>% 
  filter(!(str_detect(title, regex(patterns_COVID,ignore_case = TRUE)) |
                      str_detect(keywords, regex(patterns_COVID,ignore_case = TRUE))
  )) %>% 
  filter(!(str_detect(title, regex(patterns_AI,ignore_case = TRUE)) |
             str_detect(keywords, regex(patterns_AI,ignore_case = TRUE)))) %>% 
  filter(!(str_detect(title, regex(patterns_elections,ignore_case = TRUE)) |
             str_detect(keywords, regex(patterns_elections,ignore_case = TRUE)))) %>% 
  filter(!(str_detect(title, regex(patterns_RUwar,ignore_case = TRUE)) |
             str_detect(keywords, regex(patterns_RUwar,ignore_case = TRUE)))) %>% 
  distinct() %>% 
  
  arrange(article_date)

boot_samples_combined <- replicate(1000,
                                control_combined[sample(1:nrow(control_combined), nrow(sample_combined), replace = TRUE), ],
                                simplify = FALSE
)


boot_means_combined <- data.frame(
  bootstrap_id = 1:length(boot_samples_combined),
  mean_acceptance_delay = sapply(boot_samples_combined, \(df) mean(df$acceptance_delay, na.rm = TRUE))
)
ci_combined <- quantile(boot_means_combined$mean_acceptance_delay,
                     probs = c(0.025, 0.975))
ci_combined
mean(sample_combined$acceptance_delay)
  
ggplot(boot_means_combined, aes(x = mean_acceptance_delay)) +
  geom_histogram(bins = 30, fill = "skyblue", color = "black", alpha = 0.7) +
  geom_vline(xintercept = ci_combined, color = "red", linetype = "dashed", linewidth = 1) +
  geom_vline(xintercept = mean(sample_combined$acceptance_delay),
             color = "darkgreen", linetype = "solid", size = 1.2) +
  annotate("text",
           x = mean(sample_combined$acceptance_delay),
           y = max(table(cut(boot_means_combined$mean_acceptance_delay, breaks = 30))) * 0.9,
           label = "Sample Mean",
           color = "darkgreen",
           angle = 90,
           vjust = -0.5) +
  labs(title = "Bootstrap Distribution of Mean Acceptance Delay (combined)",
       x = "Mean Acceptance Delay",
       y = "Frequency") +
  theme_minimal()


sample_combined_nocovid = bind_rows(my_data_ai2, my_data_elections2, my_data_RUwar2)

control_combined_nocovid = my_data %>% 
  semi_join(sample_combined, by = "asjc") %>% 
  filter(!(str_detect(title, regex(patterns_AI,ignore_case = TRUE)) |
             str_detect(keywords, regex(patterns_AI,ignore_case = TRUE)))) %>% 
  filter(!(str_detect(title, regex(patterns_elections,ignore_case = TRUE)) |
             str_detect(keywords, regex(patterns_elections,ignore_case = TRUE)))) %>% 
  filter(!(str_detect(title, regex(patterns_RUwar,ignore_case = TRUE)) |
             str_detect(keywords, regex(patterns_RUwar,ignore_case = TRUE)))) %>% 
  distinct() %>% 
  arrange(article_date)

boot_samples_combined_nocovid <- replicate(1000,
                                   control_combined_nocovid[sample(1:nrow(control_combined_nocovid), nrow(sample_combined_nocovid), replace = TRUE), ],
                                   simplify = FALSE
)


boot_means_combined_nocovid <- data.frame(
  bootstrap_id = 1:length(boot_samples_combined_nocovid),
  mean_acceptance_delay = sapply(boot_samples_combined_nocovid, \(df) mean(df$acceptance_delay, na.rm = TRUE))
)
ci_combined_nocovid <- quantile(boot_means_combined_nocovid$mean_acceptance_delay,
                        probs = c(0.025, 0.975))
ci_combined_nocovid
mean(sample_combined$acceptance_delay)

ggplot(boot_means_combined_nocovid, aes(x = mean_acceptance_delay)) +
  geom_histogram(bins = 30, fill = "skyblue", color = "black", alpha = 0.7) +
  geom_vline(xintercept = ci_combined_nocovid, color = "red", linetype = "dashed", linewidth = 1) +
  geom_vline(xintercept = mean(sample_combined_nocovid$acceptance_delay),
             color = "darkgreen", linetype = "solid", size = 1.2) +
  annotate("text",
           x = mean(sample_combined_nocovid$acceptance_delay),
           y = max(table(cut(boot_means_combined_nocovid$mean_acceptance_delay, breaks = 30))) * 0.9,
           label = "Sample Mean",
           color = "darkgreen",
           angle = 90,
           vjust = -0.5) +
  labs(title = "Bootstrap Distribution of Mean Acceptance Delay (combined)",
       x = "Mean Acceptance Delay",
       y = "Frequency") +
  theme_minimal()
  

t.test(sample_combined$is_retracted, control_combined$is_retracted)
t.test(sample_combined_nocovid$is_retracted, control_combined_nocovid$is_retracted)
