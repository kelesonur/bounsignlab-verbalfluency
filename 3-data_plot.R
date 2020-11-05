library(dplyr)
library(magrittr)
library(ggplot2)
theme_set(theme_bw())
library(extrafont) 
font_import(pattern = "Times New Roman", prompt = F)
loadfonts()

# function for calculating %95 confidence intervals
ci <- function(x){1.96*(sd(x)/sqrt(length(x)))}

df <- readRDS("df.rds")
df_ncr <- readRDS("df_ncr.rds")
# recode factors for easy read
df_ncr$difficulty2 <- dplyr::recode(df_ncr$difficulty,`1` = "Easy", `2` = "Medium", `3` = "Hard") %>% 
  as.factor() %>% reorder.factor(new.order = c("Easy","Medium","Hard"))
df_correct <- readRDS("df_correct.rds")

### DATA PLOTS ###
# how many of things there are
df %>% dplyr::select(response_type = type, category, Group = group) %>%
  group_by(category,response_type,Group) %>%
  ggplot(aes(response_type, fill=Group)) + 
  geom_bar(position = "dodge") +
  stat_count(geom = "text", aes(color = Group, label = ..count..), vjust = "bottom", position = position_dodge(0.85)) +
  facet_grid(category~.) +
  ylab("Count") + xlab("Response Type") +
  theme(text=element_text(family="Times New Roman", size=12))

# mean correct responses
df_ncr %>% group_by(Group = group, difficulty2, category) %>%
  summarise(mean_response = mean(ncr), ci = ci(ncr)) %>%
  ggplot(aes(difficulty2,mean_response, group = Group, color = Group)) + 
  geom_point() + geom_line() + 
  geom_errorbar(aes(ymax = mean_response + ci, ymin = mean_response - ci), width = .14) +
  facet_grid(.~category) + xlab("Difficulty") + ylab("Mean Response") +
  theme(text=element_text(family= "Times New Roman", size=12))

# cumulative correct responses by time
diff_labs <- c("Easy","Medium","Hard")
names(diff_labs) <- c("1","2","3")
df_correct %>% group_by(Group = group, category, difficulty) %>%
  ggplot(aes(time_sec, group = Group, color = Group)) + stat_ecdf(geom = "step") +
  facet_grid(category~difficulty, labeller = labeller(difficulty = diff_labs)) + 
  ylab("% of Data") + xlab("Time (seconds)") + 
  theme(text=element_text(family="Times New Roman", size=12))



### MODEL PLOTS ###
# total number of correct responses #
ncr_model_df <- readRDS("ncr_model_df.rds")
ncr_model_df %>% ggplot(aes(m, y=factor(parameter, 
                                        levels = rev(levels(factor(parameter)))))) +
  theme(text=element_text(family="Times New Roman", size=12)) + geom_point(size = 2) +
  geom_errorbarh(aes(xmin = l, xmax = h), alpha = 1, height = 0, size = 1) +
  geom_errorbarh(aes(xmin = ll, xmax = hh, height = 0)) + vline_0() +
  xlab("Estimate(log)") + ylab("Coefficients")

# number of correct responses through time course #
time_model_df <- readRDS("time_model_df.rds")
time_model_df %>% ggplot(aes(m, y=factor(parameter, 
                                         levels = rev(levels(factor(parameter)))))) +
  theme(text=element_text(family="Times New Roman", size=12)) + geom_point(size = 2) +
  geom_errorbarh(aes(xmin = l, xmax = h), alpha = 1, height = 0, size = 1) +
  geom_errorbarh(aes(xmin = ll, xmax = hh, height = 0)) + vline_0() +
  xlab("Estimate(log)") + ylab("Coefficients")
