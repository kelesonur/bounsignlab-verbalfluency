library(tidyverse)
library(magrittr)
library(gdata)
library(brms)
library(MASS)
library(bayesplot)
library(tidybayes)
library(gamlss.dist)

#### CORRECT RESPONSES ANALYSIS ####
df_ncr <- readRDS("./aggregated_data/df_ncr.rds")

### contrast coding for the predictors ###
contrasts(df_ncr$category)
contrasts(df_ncr$category) <- contr.sdif(2)
contrasts(df_ncr$category)

contrasts(df_ncr$cat2)
contrasts(df_ncr$cat2) <- contr.sum(3)/2
contrasts(df_ncr$cat2)

contrasts(df_ncr$difficulty)
contrasts(df_ncr$difficulty) <- contr.sdif(3)
contrasts(df_ncr$difficulty)

# reorder factor for contrast setting 
df_ncr$group %<>% reorder.factor(new.order = c("Late","Native"))
contrasts(df_ncr$group)
contrasts(df_ncr$group) <- contr.sdif(2)
contrasts(df_ncr$group)

# model responses
ncr_model <- brm(ncr ~ (cat2+difficulty+group)^2, 
                 family = poisson(link="log"), data = df_ncr,
                 chains = 4, cores = 4, iter= 3000, warmup = 2000, file = "./models_data/ncr_model")

# model df for plotting
ncr_model_df <- ncr_model %>%
  mcmc_intervals_data(pars = vars(starts_with("b_")), prob_outer = 0.95) %>%
  separate(col = "parameter", into = c("x","parameter"), sep = "\\_") %>%
  subset(parameter !="Intercept") %>% dplyr::select(-x)

# recode levels to make readable
ncr_model_df$parameter %<>% dplyr::recode(`cat21` = "HS",`cat22` = "LOC", `group2M1` = "Native",
                                          `difficulty2M1` = "Medium-Easy", `difficulty3M2` = "Hard-Medium",
                                          `cat21:difficulty2M1` = "HS*Med-Easy",
                                          `cat21:difficulty3M2` = "HS*Hard-Med",
                                          `cat22:difficulty2M1` = "LOC*Med-Easy",
                                          `cat22:difficulty3M2` = "LOC*Hard-Med",
                                          `cat21:group2M1`= "HS*Native",
                                          `cat22:group2M1`= "LOC*Native",
                                          `difficulty2M1:group2M1` = "Med-Easy*Native",
                                          `difficulty3M2:group2M1` = "Hard-Med*Native") %>%
  reorder.factor(new.order = c("HS", "LOC", "Native", "Medium-Easy",
                               "Hard-Medium", "HS*Med-Easy", "HS*Hard-Med", "LOC*Med-Easy", "LOC*Hard-Med",
                               "HS*Native", "LOC*Native", "Med-Easy*Native", "Hard-Med*Native"))

saveRDS(ncr_model_df, "./models_data/ncr_model_df.rds")
write.csv(ncr_model_df,"./models_data/ncr_model_results.csv")

#### TIME COURSE ANALYSIS ####
df_cum_time <- readRDS("./aggregated_data/df_cum_time.rds")

### contrast coding for the predictors ###
contrasts(df_cum_time$category)
contrasts(df_cum_time$category) <- contr.sdif(2)
contrasts(df_cum_time$category)

contrasts(df_cum_time$cat2)
contrasts(df_cum_time$cat2) <- contr.sum(3)/2
contrasts(df_cum_time$cat2)

contrasts(df_cum_time$difficulty)
contrasts(df_cum_time$difficulty) <- contr.sdif(3)
contrasts(df_cum_time$difficulty)


df_cum_time$group %<>% reorder.factor(new.order = c("Late","Native"))
contrasts(df_cum_time$group)
contrasts(df_cum_time$group) <- contr.sdif(2)
contrasts(df_cum_time$group)


df_cum_time$time %<>% as.factor()
contrasts(df_cum_time$time)
contrasts(df_cum_time$time) <- contr.sdif(6)
contrasts(df_cum_time$time)

# regression model
time_model <- brm(time_cum ~ (cat2+group+time+difficulty)^2,
                   family = poisson(link="log"), data = df_cum_time,
                   chains = 4, cores = 4, iter= 3000, warmup = 2000, file = "./models_data/time_model")

# time model df
time_model_df <- time_model %>%
  mcmc_intervals_data(pars = vars(starts_with("b_")), prob_outer = 0.95) %>%
  separate(col = "parameter", into = c("x","parameter"), sep = "\\_") %>%
  subset(parameter !="Intercept") %>% dplyr::select(-x)

# recode parameters for easy reading
time_model_df %<>% subset(parameter %in% c("group2M1","time2M1","time3M2",
                                            "time4M3","time5M4","time6M5",
                                            "group2M1:time2M1","group2M1:time3M2",
                                            "group2M1:time4M3","group2M1:time5M4",
                                            "group2M1:time6M5"))
time_model_df$parameter %<>% dplyr::recode(`group2M1` = "Native", `time2M1` = "20s-10s",`time3M2` = "30s-20s",
                                            `time4M3` = "40s-30s", `time5M4` = "50s-40s", `time6M5` = "60s-50s",
                                            `group2M1:time2M1` = "Native*20s-10s", `group2M1:time3M2` = "Native*30s-20s",
                                            `group2M1:time4M3` = "Native*40s-30s", `group2M1:time5M4` = "Native*50s-40s",
                                            `group2M1:time6M5` = "Native*60s-50s") %>%
  reorder.factor(new.order = c("Native","20s-10s","30s-20s","40s-30s","50s-40s","60s-50s",
                               "Native*20s-10s","Native*30s-20s","Native*40s-30s","Native*50s-40s","Native*60s-50s"))

saveRDS(time_model_df, "./models_data/time_model_df.rds")
write.csv(time_model_df,"./models_data/time_model_results.csv")



#### SIMULATED MEAN NCR AND SRT VALUES ####
# function for calculating %95 confidence intervals
ci <- function(x){1.96*(sd(x)/sqrt(length(x)))}

# function for simulating data points 
vffunction <- function (N, mean_latency){ 
  NCR = N*(1-exp(-60/mean_latency))
  latencies = rexGAUS(n = NCR, mu = 1, sigma = 0.5, nu = mean_latency) %>% sort()
  latencies <- latencies[latencies < 60]
  time = seq(0, 60, 5)
  correct = rep(NA, length(time))
  correct[1] = 0
  correct[2] <- correct[1] + sum(latencies <= 5) 
  correct[3] <- correct[2] + sum(latencies > 5 & latencies <= 10)
  correct[4] <- correct[3] + sum(latencies > 10 & latencies <= 15)
  correct[5] <- correct[4] + sum(latencies > 15 & latencies <= 20) 
  correct[6] <- correct[5] + sum(latencies > 20 & latencies <= 25)
  correct[7] <- correct[6] + sum(latencies > 25 & latencies <= 30)
  correct[8] <- correct[7] + sum(latencies > 30 & latencies <= 35)
  correct[9] <- correct[8] + sum(latencies > 35 & latencies <= 40) 
  correct[10] <- correct[9] + sum(latencies > 40 & latencies <= 45)
  correct[11] <- correct[10] + sum(latencies > 45 & latencies <= 50)
  correct[12] <- correct[11] + sum(latencies > 50 & latencies <= 55)
  correct[13] <- correct[12] + sum(latencies > 55 & latencies <= 60)
  
  # to get the mean subsequent response latencies
  k <- length(latencies)
  
  for (i in 2:length(latencies))
  {
    kk <- latencies[i] - latencies[1]
    k <- c(k, kk)}
  srt <- mean(k)
  
  df <- data.frame(ncr = correct, time = time, srt = srt)
  
}

# loop for different retrieval rate but similar vocab size.
# first group
group1_1_df <- data.frame()

for (i in 1:500) {
  ret <- vffunction(N = 23, mean_latency =  10)
  group1_1_df <- rbind(group1_1_df, as.data.frame(ret))
}

group1_1_df$group = "1"
group1_1_df$subject = paste("Subject ",rep(1:500,each=13),sep="")

# second group
group1_2_df <- data.frame()

for (i in 1:500) {
  ret <- vffunction(N = 24.5, mean_latency =  20)
  group1_2_df <- rbind(group1_2_df, as.data.frame(ret))
}

group1_2_df$group = "2"
group1_2_df$subject = paste("Subject ",rep(501:1000,each=13),sep="")

# combine them
df_sim_first <- rbind(group1_1_df, group1_2_df) %>% mutate(simulated = "DifRateEqSize")

# loop for similar retrieval rate but different vocab size.
# first group
group2_1_df <- data.frame()

for (i in 1:500) {
  ret <- vffunction(16, 24)
  group2_1_df <- rbind(group2_1_df, as.data.frame(ret))
}
group2_1_df$group = "1"
group2_1_df$subject = paste("Subject ",rep(1:500,each=13),sep="")

# second group
group2_2_df <- data.frame()

for (i in 1:500) {
  ret <- vffunction(24, 24)
  group2_2_df <- rbind(group2_2_df, as.data.frame(ret))
}
group2_2_df$group = "2"
group2_2_df$subject = paste("Subject ",rep(1:500,each=13),sep="")

# combine them
df_sim_second <- rbind(group2_1_df, group2_2_df) %>% mutate(simulated = "EqRateDifSize")

# combine both simulations
df_simulated <- rbind(df_sim_first, df_sim_second)

# summarize simulations
df_simulated_summary <- df_simulated %>% group_by(group, time, simulated) %>%
  summarise(mean_ncr = mean(ncr), ci_ncr = ci(ncr))
df_simulated_summary$group %<>% recode("1" = "Late", "2" = "Native")

# summarize simulated srts
df_simulated_srt_summary <- df_simulated %>% group_by(group, simulated) %>% 
  summarise(mean_srt = mean(srt), ci_srt = ci(srt))
df_simulated_srt_summary$group %<>% recode("1" = "Late", "2" = "Native") 

# save simulated data
saveRDS(df_simulated,"./models_data/df_simulated.rds")
saveRDS(df_simulated_srt_summary, "./models_data/df_simulated_srt_summary.rds")
saveRDS(df_simulated_summary,"./models_data/df_simulated_summary.rds")






