library(dplyr)
library(magrittr)
library(tidyr)
library(readxl) 
library(gdata)
library(stringr)

# read the data frame, drop NA values
df <- read_excel("vf_data.xlsx") %>% drop_na()

# encode vector types
df$subject %<>% as.integer()
df$group %<>% dplyr::recode(`0` = "Late", `1` = "Native") %>% 
  as.factor() %>% reorder.factor(new.order = c("Native","Late"))
df$item %<>% as.integer()
df$difficulty %<>% as.factor()
df$word_nr %<>% as.integer()
df$onset %<>% as.integer()
df$offset %<>% as.integer()
df$category %<>% as.factor()
df$type %<>% as.factor()
saveRDS(df,"df.rds")

### DATA AGGREGATION FOR ANALYSIS ###
# correct responses for analysis, order by subject and onset
df_correct <- df %>% subset(type =="Correct") %>% dplyr::select(-type, -offset, -word_nr)
df_correct %<>% dplyr::arrange(subject,onset)

# calculate ms difference between responses
df %<>% group_by(subject,item) %>% mutate(difference = onset - lag(onset, default = onset[1]))
df %<>% group_by(subject,item) %>% mutate(time_interval = cumsum(difference))

df_correct %<>% group_by(subject,item) %>% mutate(difference = onset - lag(onset, default = onset[1]))
df_correct %<>% group_by(subject,item) %>% mutate(time_interval = cumsum(difference))
df_correct %<>% dplyr::select(-onset)

# calculate how many correct responses there are
df_correct %<>% group_by(subject,item) %>% mutate(ncr = n())
df_correct %<>% group_by(subject,item) %>% mutate(n_correct = 1:n())

# calculate time seconds and slots
df_correct$time_sec <- round((df_correct$time_interval/1000), digits = 0) %>% as.integer()
df_correct$time <- with(df_correct, 
                        case_when(
                          time_sec >= 0  & time_sec < 10 ~ "10",
                          time_sec >= 10 & time_sec < 20 ~ "20",
                          time_sec >= 20 & time_sec < 30 ~ "30",
                          time_sec >= 30 & time_sec < 40 ~ "40",
                          time_sec >= 40 & time_sec < 50 ~ "50",
                          T ~ "60")) %>% as.integer()

# total number of responses for each time period 
df_correct %<>% group_by(subject, item, time) %>% mutate(time_total = n()) %>% ungroup()
saveRDS(df_correct, "df_correct.rds")


### MODEL DATA FRAMES ###
# data frame for number of responses model
df_ncr <- df_correct %>% 
  dplyr::distinct(subject,item, .keep_all = T) %>% 
  dplyr::arrange(subject,item) %>%
  dplyr::select(subject, group, item, category, difficulty, ncr) %>% ungroup()
saveRDS(df_ncr, "df_ncr.rds")

# data frame for time course analysis 
df_time <- df_correct %>% 
  dplyr::distinct(subject,item, time, .keep_all = T) %>%
  dplyr::arrange(subject,item,time) %>%
  dplyr::select(-difference, -time_interval,-ncr,-n_correct) %>% ungroup()

df_time %<>% group_by(subject,item) %>% 
  mutate(time_cum = cumsum(time_total)) %>%
  dplyr::select(-time_total)

saveRDS(df_time,"df_time.rds")

























