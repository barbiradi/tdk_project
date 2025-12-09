
install.packages("rstatix")
install.packages("ggthemes")
library(ggthemes)
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
my_data = my_data %>% 
  arrange(article_date)

my_data_grouped = my_data %>% 
  group_by(asjc) %>% 
  summarise()

sd(my_data$acceptance_delay)
min(my_data$article_date)
max(my_data$article_date)
mean(my_data$acceptance_delay)
median(my_data$acceptance_delay)


patterns_AI <- "\\bAI\\b|\\bAI-|ChatGPT|OpenAI|Generative AI|\\bLLM\\b|\\bLLMs\\b|Large language models|Chat GPT|GPT-3.5|GPT-4|\\bGPT\\b"


my_data_ai1 = my_data %>%
  filter(
    str_detect(title, regex(patterns_AI, ignore_case = TRUE)) |
      str_detect(keywords, regex(patterns_AI, ignore_case = TRUE))
  ) %>% 
  mutate(article_date = as.Date(article_date))


threshold_AI <- as.Date("2022-11-30")

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


my_data_covid2 = my_data_covid2 %>% 
  mutate(journal = factor(journal))
levels(my_data_covid2$journal)

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

"Exploratív vizualizációk"
library(lubridate)
library(ggridges)
install.packages("ggbreak")
library(ggbreak)
install.packages("ggplot2")
library(ggplot2)
article_date_month <- my_data %>%
  mutate(article_date_month = floor_date(as.Date(article_date), "month")) %>% 
  group_by(article_date_month) %>%
  summarise(
    mean_delay = mean(acceptance_delay, na.rm = TRUE),
    .groups = "drop"
  )

plot_1 = ggplot(article_date_month, aes(x = article_date_month, y = mean_delay, group = 1)) +
  geom_point(color = "darkgreen", size = 2)+
   geom_smooth(method = "loess", span = 0.35, se = FALSE, color = "red") +
  labs(
    title = "Átlagos elfogadási késés",
    x = "Hónap",
    y = "Átlagos elfogadási késés"
  ) +
  theme_stata()


ggsave("plot1.jpg", width = 7, height = 4, dpi = 300)
plot_1
plot_2 = ggplot(dates_bound, 
       aes(x = as.numeric(article_date),
           y = source,
           fill = source)) +
  geom_density_ridges(alpha = 0.6, color = "white") +
  scale_x_continuous(
    name = "Megjelenés dátuma",
    labels = function(x) as.Date(x, origin = "1970-01-01")
  ) +
  labs(
    title = "Cikkek megjelenésének időpontjai témánként",
    y = "Téma",
    fill = "Téma"
  ) +
  theme_minimal()
plot_2
ggsave("plot_2.jpg", width = 7, height = 6, dpi = 300)

"Elemzes"
summary(my_data_ai2$acceptance_delay)
sd(my_data_ai2$acceptance_delay)

summary(my_data_covid2$acceptance_delay)
sd(my_data_covid2$acceptance_delay)

summary(my_data_RUwar2$acceptance_delay)
sd(my_data_RUwar2$acceptance_delay)


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



plot_3 = ggplot(boot_means_df_AI, aes(x = mean_acceptance_delay)) +
  geom_histogram(bins = 60, fill = "skyblue", color = "black", alpha = 0.7) +
  geom_vline(xintercept = ci_AI, color = "red", linetype = "dashed", linewidth = 1) + # 95% CI
  geom_vline(xintercept = mean(my_data_ai2$acceptance_delay), color = "darkgreen", linetype = "solid", size = 1.2) +
  annotate("text", x = mean(my_data_ai2$acceptance_delay), y = 15,
           label = "Mintaátlag", color = "darkgreen", angle = 90, vjust = -0.5) +
  labs(title = "Az átlagos elfogadási késés bootstrap eloszlása (AI)",
       x = "Átlagos elfogadási késés") +
  theme_stata()
plot_3

ggsave("plot_3.jpg", width = 8, height = 8, dpi = 300)

plot_4 = ggplot(boot_means_covid_sample, aes(x = mean_acceptance_delay)) +
  geom_histogram(bins = 60, fill = "skyblue", color = "black", alpha = 0.7) +
  geom_vline(xintercept = ci_covid, color = "red", linetype = "dashed", linewidth = 1) +
  geom_vline(xintercept = mean(my_data_covid2$acceptance_delay),
             color = "darkgreen", linetype = "solid", size = 1.2) +
  annotate("text",
           x = mean(my_data_covid2$acceptance_delay),
           y = max(table(cut(boot_means_covid_sample$mean_acceptance_delay, breaks = 30))) * 0.9,
           label = "Mintaátlag",
           color = "darkgreen",
           angle = 90,
           vjust = -0.5) +
  labs(title = "Az átlagos elfogadási késés bootstrap eloszlása (COVID)",
       x = "Átlagos elfogadási késés", y = "Előfordulási gyakoriság")+
  theme_stata()
plot_4
ggsave("plot_4.jpg", width = 8, height = 8, dpi = 300)

plot_5 = ggplot(boot_means_RUwar, aes(x = mean_acceptance_delay)) +
  geom_histogram (bins = 60, fill = "skyblue", color = "black", alpha = 0.7) +
  geom_vline(xintercept = ci_RUwar, color = "red", linetype = "dashed", linewidth = 1) +
  geom_vline(xintercept = mean(my_data_RUwar2$acceptance_delay),
             color = "darkgreen", linetype = "solid", size = 1.2) +
  annotate("text",
           x = mean(my_data_RUwar2$acceptance_delay),
           y = 15,
           label = "Mintaátlag",
           color = "darkgreen",
           angle = 90,
           vjust = -0.5) +
  labs(title = "Az átlagos elfogadási késés bootstrap eloszlása (RU War)",
       x = "Átlagos elfogadási késés",
       y = "Előfordulási gyakoriság") +
  theme_stata()
plot_5
ggsave("plot_5.jpg", width = 8, height = 8, dpi = 300)


date_bounds_AI <- my_data_ai1 %>%
  mutate(date_num = as.numeric(article_date)) %>%
  summarise(
    lower = as.Date(quantile(date_num, 0.025, na.rm = TRUE), origin = "1970-01-01"),
    upper = as.Date(quantile(date_num, 0.975, na.rm = TRUE), origin = "1970-01-01")
  )
date_bounds_AI


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

ci_bound = bind_rows(ci_AI, ci_covid, ci_RUwar) %>% 
  add_column(Téma = c("AI", "COVID-19", "Orosz-ukrán konfliktus"))

df_ai_articled        <- my_data_ai1        %>% select(article_date) %>% mutate(source = "AI")
df_covid_articled     <- my_data_covid1     %>% select(article_date) %>% mutate(source = "COVID")
df_ruwar_articled     <- my_data_RUwar1     %>% select(article_date) %>% mutate(source = "RUwar")

dates_bound <- bind_rows(df_ai_articled, df_covid_articled, df_ruwar_articled)


"A visszavonás a tudomány eszköze arra vonatkozóan, hogy az esetleg torzított eredményeket, 
hibás eljárást, vagy a nem megfelelő tudományművelés egyéb más megnyilvánulási formáját tartalmazó publikáció jelenlétét jelezze
a tudományos közösség számára és eltávolítsa a hibás cikket az idézhető tartalmak halmazából (Zheng et al., 2023).
"
sample_combined = bind_rows(my_data_ai2, my_data_covid2, my_data_RUwar2) %>% 
  distinct() %>% 
  arrange(article_date) 

control_combined = my_data %>% 
  semi_join(sample_combined, by = "asjc") %>% 
  filter(!(str_detect(title, regex(patterns_COVID,ignore_case = TRUE)) |
             str_detect(keywords, regex(patterns_COVID,ignore_case = TRUE))
  )) %>% 
  filter(!(str_detect(title, regex(patterns_AI,ignore_case = TRUE)) |
             str_detect(keywords, regex(patterns_AI,ignore_case = TRUE)))) %>% 
  filter(!(str_detect(title, regex(patterns_RUwar,ignore_case = TRUE)) |
             str_detect(keywords, regex(patterns_RUwar,ignore_case = TRUE)))) %>% 
  distinct() %>% 


"A COVID-19-el foglalkozó cikkek esete speciális, abban a tekintetben,
hogy számos intézményes törekvés volt arra, hogy a globális krízishelyzetre való
tekintettel, a témával foglalkozó cikkek kisebb idő alatt kerüljenek elfogadásra, hogy
minél gyorsabban és minél  transzparensebben közölve legyenek a pandémiával kapcsolatos 
kutatások eredményei.(Sevryugina & Dicks, 2022) Számos fast-track lektorálást szorgalmazó intézkedés érvénybe lépett, azonban
ezek elsősorban az egészségtudománnyal, biológiával foglalkozó folyóiratokat és cikkeket érintette (Sevryugina & Dicks, 2022). Következésképpen,
végeztünk egy elemzést a COVID-19-el kapcsolatos cikkek elfogadási idejére vonatkozóan, az imént említett
területek kizárásával --> "
getwd()
asjc <- read_excel("ASJC1.xlsx")

asjc_grouped = asjc %>% 
  filter(Code %in% my_data_grouped$asjc)

asjc_fasstrack = asjc_grouped %>% 
  filter(!str_detect(Description, regex("Psychology|Health"))) %>% 
  mutate(Code = as.character(Code))

asjc_filtered = asjc_grouped %>% 
  filter(str_detect(Description, regex("Psychology|Health"))) %>% 
  mutate(Code = as.character(Code))

patterns_asjc = "3200|3201|3202|3203|3204|3205|3206|3207|3306"
my_data_covid1_nops <- my_data %>%
  filter(
    str_detect(title,regex(patterns_COVID,ignore_case = TRUE)) |
      str_detect(keywords,regex(patterns_COVID,ignore_case = TRUE))
  ) %>%
  mutate(
    article_date =as.Date(article_date)
  ) %>% 
  filter(!str_detect(asjc, regex(patterns_asjc)))

my_data_covid2_nops <- my_data %>%
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
  ) %>% 
  filter(!str_detect(asjc, regex(patterns_asjc)))

my_data_control_covid_nops = my_data %>%
  filter(
    article_date >= threshold_covid_start,
    article_date <= threshold_covid_end
  ) %>%
  semi_join(my_data_covid2_nops, by= "asjc") %>%
  filter(!(str_detect(title, regex(patterns_COVID,ignore_case = TRUE)) |
             str_detect(keywords, regex(patterns_COVID, ignore_case = TRUE))
  ))


boot_samples_COVID_nops <- replicate(1000,
                                my_data_control_covid_nops[sample(1:nrow(my_data_control_covid_nops), nrow(my_data_covid2_nops), replace = TRUE), ],
                                simplify = FALSE
)


boot_means_covid_sample_nops <- data.frame(
  bootstrap_id = 1:length(boot_samples_COVID_nops),
  mean_acceptance_delay = sapply(boot_samples_COVID_nops, \(df) mean(df$acceptance_delay, na.rm = TRUE))
)

ci_covid_nops <- quantile(boot_means_covid_sample_nops$mean_acceptance_delay,
                     probs = c(0.025, 0.975))
ci_covid_nops
mean(my_data_covid2_nops$acceptance_delay)


plot_6 = ggplot(boot_means_covid_sample_nops, aes(x = mean_acceptance_delay)) +
  geom_histogram(bins = 30, fill = "skyblue", color = "black", alpha = 0.7) +
  geom_vline(xintercept = ci_covid_nops, color = "red", linetype = "dashed", linewidth = 1) +
  geom_vline(xintercept = mean(my_data_covid2_nops$acceptance_delay),
             color = "darkgreen", linetype = "solid", size = 1.2) +
  annotate("text",
           x = mean(my_data_covid2_nops$acceptance_delay),
           y = 100,
           label = "Mintaátlag",
           color = "darkgreen",
           angle = 90,
           vjust = -0.5) +
  labs(title = " Az átlagos elfogadási késés bootstrap eloszlása (COVID_nops)",
       x = "Átlagos elfogadási késés",
       y = "Előfordulási gyakoriság") +
  theme_stata()
plot_6

ggsave("plot_6.jpg", width = 8, height = 8, dpi = 300)
"Ezen kívül megnéztük, hogy az összesített elemzés milyen eredményt mutat, ha a COVID-19-et kivesszük teljesen,
viszont kibővítjük az intervallumát kontrollpopulációknak a másik kettő témában, és összevonjuk a hatást."

sample_combined = bind_rows(my_data_ai2, my_data_covid2, my_data_RUwar2) %>% 
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
  filter(!(str_detect(title, regex(patterns_RUwar,ignore_case = TRUE)) |
             str_detect(keywords, regex(patterns_RUwar,ignore_case = TRUE)))) %>% 
  distinct() %>% 
  
  arrange(article_date)

boot_samples_combined <- replicate(1000,
                                control_combined[sample(1:nrow(control_combined), nrow(sample_combined), replace = TRUE), ],
                                simplify = FALSE
)


sample_combined_nocovid = bind_rows(my_data_ai2, my_data_RUwar2)


control_combined_nocovid = my_data %>% 
  semi_join(sample_combined, by = "asjc") %>% 
  filter(!(str_detect(title, regex(patterns_AI,ignore_case = TRUE)) |
             str_detect(keywords, regex(patterns_AI,ignore_case = TRUE)))) %>% 
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

plot_7 = ggplot(boot_means_combined_nocovid, aes(x = mean_acceptance_delay)) +
  geom_histogram(bins = 30, fill = "skyblue", color = "black", alpha = 0.7) +
  geom_vline(xintercept = ci_combined_nocovid, color = "red", linetype = "dashed", linewidth = 1) +
  geom_vline(xintercept = mean(sample_combined_nocovid$acceptance_delay),
             color = "darkgreen", linetype = "solid", size = 1.2) +
  annotate("text",
           x = mean(sample_combined_nocovid$acceptance_delay),
           y = max(table(cut(boot_means_combined_nocovid$mean_acceptance_delay, breaks = 30))) * 0.9,
           label = "Mintaátlag",
           color = "darkgreen",
           angle = 90,
           vjust = -0.5) +
  labs(title = "Az átlagos elfogadási késés bootstrap eloszlás (combined)",
       x = "Átlagos elfogadási késés",
       y = "Előfordulási gyakoriság") +
  theme_stata()
plot_7
ggsave("plot_7.jpg", width = 8, height = 8, dpi = 600)
"A visszavonás a tudomány eszköze arra vonatkozóan, hogy az esetleg torzított eredményeket, 
hibás eljárást, vagy a nem megfelelő tudományművelés egyéb más megnyilvánulási formáját tartalmazó publikáció jelenlétét jelezze
a tudományos közösség számára és eltávolítsa a hibás cikket az idézhető tartalmak halmazából (Zheng et al., 2023).
"

my_data_sample_retracted = sample_combined %>% 
  filter(is_retracted == TRUE)
count(my_data_sample_retracted)/count(sample_combined)
count(my_data_retracted)/count(my_data)
my_data_retracted = my_data %>% 
  filter(is_retracted == TRUE)
my_data_control_retracted = my_data %>% 
  semi_join(my_data_retracted, by = "asjc") %>% 
  filter(!(is_retracted == TRUE))

boot_samples_retracted <- replicate(1000,
                                my_data_control_retracted[sample(1:nrow(my_data_control_retracted), nrow(my_data_retracted), replace = TRUE), ],
                                simplify = FALSE
)


boot_means_retracted <- data.frame(
  bootstrap_id = 1:length(boot_samples_retracted),
  mean_acceptance_delay = sapply(boot_samples_retracted, \(df) mean(df$acceptance_delay, na.rm = TRUE))
)
ci_retracted <- quantile(boot_means_retracted$mean_acceptance_delay,
                          probs = c(0.025, 0.975))
ci_retracted
mean(my_data_retracted$acceptance_delay)
plot_8 = ggplot(boot_means_retracted, aes(x = mean_acceptance_delay)) +
  geom_histogram(bins = 60, fill = "skyblue", color = "black", alpha = 0.7) +
  geom_vline(xintercept = ci_retracted, color = "red", linetype = "dashed", linewidth = 1) +
  geom_vline(xintercept = mean(my_data_retracted$acceptance_delay),
             color = "darkgreen", linetype = "solid", size = 1.2) +
  annotate("text",
           x = mean(my_data_retracted$acceptance_delay),
           y = 50,
           label = "Mintaátlag",
           color = "darkgreen",
           angle = 90,
           vjust = -0.5) +
  labs(title = "Az átlagos elfogadási késés bootstrap eloszlása (is_retracted)",
       x = "Átlagos elfogadási késés", y = "Előfordulási gyakoriság")+
  theme_stata()
plot_8
ggsave("plot_8.jpg", width = 8, height = 8, dpi = 300)

mean(boot_means_retracted$mean_acceptance_delay)
mean(my_data_retracted$acceptance_delay)
