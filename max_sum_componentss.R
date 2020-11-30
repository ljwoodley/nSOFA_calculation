library(tidyverse)
library(vroom)
library(here)
library(lubridate)

subjects_to_exclude <- read_csv("data/subjects_to_exclude.csv") %>% 
  filter(exclude == 'y') %>% 
  select(child_mrn_uf)

nsofa <- vroom("output/nsofa_score_2020-07-10.csv", delim = ",") %>% 
  anti_join(subjects_to_exclude)

subjects_that_died <- nsofa %>% 
  filter(dischg_disposition %in% c("EXPIRED AUT", "EXPIRED NO AUT")) %>% 
  select(child_mrn_uf, q1hr, nsofa_score)

score_range <- nsofa %>%  
  group_by(child_mrn_uf, dischg_disposition) %>%
  summarise(min_nsofa = min(nsofa_score),
            max_nsofa = max(nsofa_score),
            min_date = min(q1hr),
            max_date = max(q1hr)) %>% 
  mutate(score_range = max_nsofa - min_nsofa,
         date_range_in_hours = round(difftime(max_date, min_date, units = "hours"))) %>% 
  arrange(date_range_in_hours)

write.csv(score_range, paste0("output/nsofa_range_from_birth_onwards.csv"), 
                    row.names = F, na = "")


# 3 days before death
max_score <- subjects_that_died %>% 
  group_by(child_mrn_uf) %>% 
  arrange(child_mrn_uf, q1hr) %>% 
  # every row represents an hour
  mutate(hour = 1:n(),
         last_recorded_hour = max(hour)) %>% 
  filter(last_recorded_hour - hour <= 24*14) %>%  
  summarise(min_score = min(nsofa_score),
            max_score = max(nsofa_score)) %>% 
  mutate(score_range = max_score - min_score) %>% 
  summarise(min_range = min(score_range),
            max_range = max(score_range),
            avg_range = round(mean(score_range)),
            median_range = median(score_range))  
          
  
get_score_range <- function(){}
  filter(between(hour, 24, last_recorded_hour))
  summarise(nsofa_score = max(nsofa_score), 
            number_hours_in_encounter = max(hour),
            dischg_disposition = unique(dischg_disposition))