library(tidyverse)
library(magrittr)
library(gdata)

# read the data frame, drop NA values
df <- read_csv("vf_data.csv") %>% drop_na()

# encode vector types
df$subject %<>% as.integer()
df$group %<>% dplyr::recode(`0` = "Late", `1` = "Native") %>% as.factor() %>% reorder.factor(new.order = c("Native","Late"))
df$item %<>% as.integer()
df$difficulty %<>% as.factor()
df$word_nr %<>% as.integer()
df$onset %<>% as.integer()
df$offset %<>% as.integer()
df$category %<>% as.factor()
df$type %<>% as.factor()

df$cat2 <- with(df, case_when(item %in% c(1:6) ~ "HS",
                              item %in% c(7:12) ~ "LOC",
                              item %in% c(13:18) ~ "SEM")) %>% as.factor()

saveRDS(df,"./aggregated_data/df.rds")

#### DATA AGGREGATION FOR ANALYSIS ####
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
# df_correct$time_sec <- round((df_correct$time_interval/1000), digits = 0) %>% as.integer()
df_correct$time <- with(df_correct, 
                        case_when(
                          time_interval >= 0  & time_interval < 10000 ~ "10",
                          time_interval >= 10000 & time_interval < 20000 ~ "20",
                          time_interval >= 20000 & time_interval < 30000 ~ "30",
                          time_interval >= 30000 & time_interval < 40000 ~ "40",
                          time_interval >= 40000 & time_interval < 50000 ~ "50",
                          T ~ "60")) %>% as.integer()

# total number of responses for each time period 
df_correct %<>% group_by(subject, item, time) %>% mutate(time_total = n()) %>% ungroup()
df_correct %<>% mutate(latency_ms = time_interval - (time*1000) + 10000)
saveRDS(df_correct, "./aggregated_data/df_correct.rds")

#### MODEL DATA FRAMES ####
# data frame for number of responses model
df_ncr <- df_correct %>% 
  dplyr::distinct(subject,item, .keep_all = T) %>% 
  dplyr::arrange(subject,item) %>% dplyr::select(subject, group, item, category, difficulty, ncr, cat2) %>% ungroup()
saveRDS(df_ncr, "./aggregated_data/df_ncr.rds")

# data frame for time course analysis 
df_time <- df_correct %>% dplyr::distinct(subject,item, time, .keep_all = T) %>%
  dplyr::arrange(subject,item,time) %>% dplyr::select(-difference, -time_interval, -ncr, -n_correct, -latency_ms) %>% ungroup()
df_time %<>% group_by(subject,item) %>% mutate(time_cum = cumsum(time_total))
saveRDS(df_time,"./aggregated_data/df_time.rds")

# fill empty intervals for cumulative reading
df_cum_time <- df_time %>% arrange(subject, item, time)

for (each in unique(df_cum_time$subject)){
  for(every in unique(df_cum_time$item)){
    x <- df_cum_time %>% subset(subject == each & item == every)
    for(all in c(1:6)){
      x %<>% arrange(subject,item, time)
      time = all*10
      if (time != x$time[all] || is.na(x$time[all])){
        y <- tibble(subject = x$subject[1], group = x$group[1], item = x$item[1], category = x$category[1],
                    difficulty = x$difficulty[1], cat2 = x$cat2[1], time = time, time_total = 0, time_cum = x$time_cum[all-1])
        df_cum_time %<>% rbind(y)
        x %<>% rbind(y)
      }
     next
    }
  }
}
  
df_cum_time %<>% arrange(subject,item,time) %>% drop_na()
saveRDS(df_cum_time, "./aggregated_data/df_cum_time.rds")

# mean latency ms
df_latency <- df_correct %>% group_by(subject, item) %>% slice(2) %>%
  dplyr::select(subject,item, first_ms = latency_ms)
df_latency <- left_join(df_correct, df_latency) %>% mutate(srt = (time_interval - first_ms)/1000) %>% subset(srt > 0)
saveRDS(df_latency, "./aggregated_data/df_latency.rds")


