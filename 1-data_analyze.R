library(dplyr)
library(magrittr)
library(tidyr)
library(gdata)
library(stringr)
library(brms)
library(MASS)
library(bayesplot)
library(tidybayes) 


######################################### CORRECT RESPONSES ANALYSIS #########################################
df_ncr <- readRDS("df_ncr.rds")

### contrast coding for the predictors ###
contrasts(df_ncr$category)
contrasts(df_ncr$category) <- contr.sdif(2)
contrasts(df_ncr$category)

contrasts(df_ncr$difficulty)
contrasts(df_ncr$difficulty) <- contr.sdif(3)
contrasts(df_ncr$difficulty)

# reorder factor for contrast setting 
df_ncr$group %<>% reorder.factor(new.order = c("Late","Native"))
contrasts(df_ncr$group)
contrasts(df_ncr$group) <- contr.sdif(2)
contrasts(df_ncr$group)

# model responses
ncr_model <- brm(ncr ~ category*difficulty*group + (1|subject) + (1|item), 
                 family = poisson(link="log"), data = df_ncr,
                 chains = 4, cores = 4, iter= 3000, warmup = 2000, file = "ncr_model")

# model df for plotting
ncr_model_df <- ncr_model %>%
  mcmc_intervals_data(pars = vars(starts_with("b_")), prob_outer = 0.95) %>%
  separate(col = "parameter", into = c("x","parameter"), sep = "\\_") %>%
  subset(parameter !="Intercept") %>% dplyr::select(-x)

# recode levels to make readable
ncr_model_df$parameter %<>% dplyr::recode(`category2M1` = "Semantic", `group2M1` = "Native",
                                          `difficulty2M1` = "Medium-Easy", `difficulty3M2` = "Hard-Medium",
                                          `category2M1:difficulty2M1` = "Sem*Med-Easy",
                                          `category2M1:difficulty3M2` = "Sem*Hard-Med",
                                          `category2M1:group2M1`= "Sem*Native",
                                          `difficulty2M1:group2M1` = "Med-Easy*Native",
                                          `difficulty3M2:group2M1` = "Hard-Med*Native",
                                          `category2M1:difficulty2M1:group2M1` = "Sem*M-E*Native",
                                          `category2M1:difficulty3M2:group2M1` = "Sem*H-M*Native") %>%
  reorder.factor(new.order = c("Semantic","Native","Medium-Easy",
                               "Hard-Medium","Sem*Med-Easy","Sem*Hard-Med",
                               "Sem*Native","Med-Easy*Native","Hard-Med*Native",
                               "Sem*M-E*Native","Sem*H-M*Native"))

saveRDS(ncr_model_df, "ncr_model_df.rds")

############################################ TIME COURSE ANALYSIS ############################################
df_time <- readRDS("df_time.rds")

### contrast coding for the predictors ###
contrasts(df_time$category)
contrasts(df_time$category) <- contr.sdif(2)
contrasts(df_time$category)

contrasts(df_time$difficulty)
contrasts(df_time$difficulty) <- contr.sdif(3)
contrasts(df_time$difficulty)


df_time$group %<>% reorder.factor(new.order = c("Late","Native"))
contrasts(df_time$group)
contrasts(df_time$group) <- contr.sdif(2)
contrasts(df_time$group)


df_time$time %<>% as.factor()
contrasts(df_time$time)
contrasts(df_time$time) <- contr.sdif(6)
contrasts(df_time$time)

# regression model
time_model <- brm(time_cum ~ category*group*time*difficulty + (1|subject) + (1|item),
                   family = poisson(link="log"), data = df_time,
                   chains = 4, cores = 4, iter= 3000, warmup = 2000, file = "time_model")

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

saveRDS(time_model_df, "time_model_df.rds")




