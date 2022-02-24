library(tidyverse)
library(ggpubr)
library(magrittr)
theme_set(theme_bw())
library(bayesplot)
bayesplot_theme_set(new = theme_bw())
library(gdata)
library(extrafont)
font_import(pattern = "Times New Roman", prompt = F)
loadfonts()

# function for calculating %95 confidence intervals
ci <- function(x){1.96*(sd(x)/sqrt(length(x)))}

df <- readRDS("./aggregated_data/df.rds")
df_ncr <- readRDS("./aggregated_data/df_ncr.rds")

# recode factors for easy read
df_ncr$difficulty2 <- dplyr::recode(df_ncr$difficulty,`1` = "Easy", `2` = "Med", `3` = "Hard") %>% 
  as.factor() %>% reorder.factor(new.order = c("Easy","Med","Hard"))
df_correct <- readRDS("./aggregated_data/df_correct.rds")
df_time <- readRDS("./aggregated_data/df_cum_time.rds")
df_correct_means <- df_time %>% group_by(Group = group, cat2, time, difficulty) %>%
  summarise(mean_response = mean(time_cum), ci = ci(time_cum)) %>% ungroup()
df_latency <- readRDS("./aggregated_data/df_latency.rds")
df_latencies <- df_latency %>% group_by(Group = group, cat2, difficulty) %>%
  summarise(mean_srt = mean(srt), ci_srt = ci(srt))

#### DATA PLOTS ####
# how many of things there are
fig1 <- df %>% dplyr::select(response_type = type, cat2, Group = group) %>%
  group_by(cat2,response_type,Group) %>%
  ggplot(aes(response_type, fill=Group)) + 
  geom_bar(position = "dodge") +
  stat_count(geom = "text", aes(color = Group, label = ..count..), vjust = "bottom", position = position_dodge(0.85)) +
  facet_grid(cat2~.) +
  ylab("Count") + xlab("Response Type") +
  theme(text=element_text(family="Times New Roman", size=12))
fig1

# mean correct responses
fig2 <- df_ncr %>% group_by(Group = group, difficulty2, cat2) %>%
  summarise(mean_response = mean(ncr), ci = ci(ncr)) %>%
  ggplot(aes(difficulty2,mean_response, group = Group, color = Group, shape=Group)) + 
  geom_point() + geom_line() + 
  geom_errorbar(aes(ymax = mean_response + ci, ymin = mean_response - ci), width = .14) +
  facet_grid(.~cat2) + xlab("Difficulty") + ylab("Mean Response") +
  theme(text=element_text(family= "Times New Roman", size=12))
fig2

# mean correct responses by 3 age groups

fig_supplementary <- df_ncr %>% group_by(Group = aoa_named, difficulty2, cat2) %>%
  summarise(mean_response = mean(ncr), ci = ci(ncr)) %>%
  ggplot(aes(difficulty2,mean_response, group = Group, color = Group, shape= Group)) + 
  geom_point(position = position_dodge(.2)) + geom_line(position = position_dodge(.2)) + 
  geom_errorbar(aes(ymax = mean_response + ci, ymin = mean_response - ci), width = .14,position = position_dodge(.2)) +
  facet_grid(.~cat2) + xlab("Difficulty") + ylab("Mean Response") +
  theme(text=element_text(family= "Times New Roman", size=12))
fig_supplementary

# cumulative mean responses through time course #
diff_labs <- c("Easy","Medium","Hard")
names(diff_labs) <- c("1","2","3")
fig5 <- ggplot() + geom_point(data = df_correct_means, aes(time, mean_response, group = Group, color = Group, shape= Group)) +
  geom_line(data = df_correct_means, aes(time, mean_response, group = Group, color = Group)) +
  geom_errorbar(data = df_correct_means, aes(x= time, y= mean_response, 
                                             ymax = mean_response + ci, ymin = mean_response - ci,
                                             group = Group, color = Group), width = .5) +
  geom_rect(data = df_latencies,aes(xmin = (mean_srt-ci_srt), xmax = (mean_srt + ci_srt),
                                    ymin = -Inf, ymax = +Inf, fill = Group, group = Group),alpha = .3) +
  geom_vline(data = df_latencies, aes(xintercept = mean_srt, group = Group, color = Group)) +
  facet_grid(cat2 ~ difficulty, labeller = labeller(difficulty = diff_labs)) +
  xlab("Time") + ylab("Cumulative Mean Response")
fig5

#### MODEL PLOTS ####
# total number of correct responses #
ncr_model_df <- readRDS("./models_data/ncr_model_df.rds")
fig3 <- ncr_model_df %>% ggplot(aes(m, y=factor(parameter, 
                                        levels = rev(levels(factor(parameter)))))) +
  theme(text=element_text(family="Times New Roman", size=12)) + geom_point(size = 2) +
  geom_errorbarh(aes(xmin = l, xmax = h), alpha = 1, height = 0, size = 1) +
  geom_errorbarh(aes(xmin = ll, xmax = hh, height = 0)) + vline_0() +
  xlab("Estimate(log)") + ylab("Coefficients")
fig3

# total number of correct responses model by 3 age groups
ncr_model2_df <- readRDS("./models_data/ncr_model_group_df.rds")
fig_model_supplementary <- ncr_model2_df %>% ggplot(aes(m, y=factor(parameter, 
                                                levels = rev(levels(factor(parameter)))))) +
  theme(text=element_text(family="Times New Roman", size=12)) + geom_point(size = 2) +
  geom_errorbarh(aes(xmin = l, xmax = h), alpha = 1, height = 0, size = 1) +
  geom_errorbarh(aes(xmin = ll, xmax = hh, height = 0)) + vline_0() +
  xlab("Estimate(log)") + ylab("Coefficients")
fig_model_supplementary

# number of correct responses through time course #
time_model_df <- readRDS("./models_data/time_model_df.rds")
fig6 <- time_model_df %>% ggplot(aes(m, y=factor(parameter, 
                                         levels = rev(levels(factor(parameter)))))) +
  theme(text=element_text(family="Times New Roman", size=12)) + geom_point(size = 2) +
  geom_errorbarh(aes(xmin = l, xmax = h), alpha = 1, height = 0, size = 1) +
  geom_errorbarh(aes(xmin = ll, xmax = hh, height = 0)) + vline_0() +
  xlab("Estimate(log)") + ylab("Coefficients")
fig6


#### SIMULATED DATA PLOTS ####
df_simulated_summary <- readRDS("./models_data/df_simulated_summary.rds")
df_simulated_srt_summary <- readRDS("./models_data/df_simulated_srt_summary.rds")
pd2 <- position_dodge(0.3)

# plotting different retrieval rate, same vocabulary size
sim_plot_1 <- df_simulated_summary %>% subset(simulated == "DifRateEqSize") %>%
  ggplot(aes(time, mean_ncr, group = group, linetype = group, color = group, shape = group)) + 
  geom_line() + geom_point(size = 3) +
  labs(x="Time (Sec)", y="Cumulative Mean Response") +
  ggtitle("Similar vocabulary size and different retrieval rate") +
  annotate(geom="text", x=df_simulated_srt_summary$mean_srt[3], y=15, label= expression("SRT"[1]), color="black", size = 3.5) +
  annotate(geom="text", x=df_simulated_srt_summary$mean_srt[1], y=16, label= expression("SRT"[2]), color="black", size = 3.5) +
  geom_segment(aes(x = df_simulated_srt_summary$mean_srt[3], y = 14.5, xend = df_simulated_srt_summary$mean_srt[3], yend = 13.5),
               arrow = arrow(length = unit(0.2, "cm")), color="black",show.legend = F) +
  geom_segment(aes(x = df_simulated_srt_summary$mean_srt[1], y = 15.5, xend = df_simulated_srt_summary$mean_srt[1], yend = 14.5),
               arrow = arrow(length = unit(0.2, "cm")), color="black",show.legend = F) +
  theme(legend.position='none') +
  theme(text=element_text(family="Times New Roman", size=12), axis.text = element_blank(), axis.ticks = element_blank())

sim_plot_2 <- df_simulated_summary %>% subset(simulated == "EqRateDifSize") %>%
  ggplot(aes(time, mean_ncr, group = group, linetype = group, color = group, shape = group)) + 
  geom_line() + geom_point(size = 3) +
  labs(x="Time (Sec)", y="") +
  ggtitle("Different vocabulary size and similar retrieval rate") +
  annotate(geom="text", x=df_simulated_srt_summary$mean_srt[4], y=14, label= expression("SRT"[1]), color="black", size = 3.5) +
  annotate(geom="text", x=df_simulated_srt_summary$mean_srt[2], y=9.5, label= expression("SRT"[2]), color="black", size = 3.5) + 
  geom_segment(aes(x = df_simulated_srt_summary$mean_srt[4], y = 13.5, xend = df_simulated_srt_summary$mean_srt[4], yend = 12.5),
               arrow = arrow(length = unit(0.2, "cm")), color="black",show.legend = F) +
  geom_segment(aes(x = df_simulated_srt_summary$mean_srt[2], y = 9, xend = df_simulated_srt_summary$mean_srt[2], yend = 8),
               arrow = arrow(length = unit(0.2, "cm")), color="black", show.legend = F) +
  theme(legend.position='none') +
  theme(text=element_text(family="Times New Roman", size=12), axis.text = element_blank(), axis.ticks = element_blank())

fig4 <- ggarrange(sim_plot_1, sim_plot_2, ncol=2, nrow=1, labels=c("A", "B"))
fig4

#### SAVE PLOTS ####
# figure 1 save
ggsave("VF-Fig_1.pdf", plot = fig1, width = 6, height = 6, device = cairo_pdf, path = "./VF-figures")
ggsave("VF-Fig_1.tiff", plot = fig1, width = 6, height = 6, path = "./VF-figures")
ggsave("VF-Fig_1.png", plot = fig1, width = 6, height = 6, path = "./VF-figures")

# figure 2 save
ggsave("VF-Fig_2.pdf", plot = fig2, width = 5, height = 2.5, device = cairo_pdf, path = "./VF-figures")
ggsave("VF-Fig_2.tiff", plot = fig2, width = 5, height = 2.5, path = "./VF-figures")
ggsave("VF-Fig_2.png", plot = fig2, width = 5, height = 2.5, path = "./VF-figures")

# figure 3 save
ggsave("VF-Fig_3.pdf", plot = fig3, width = 5, height = 4, device = cairo_pdf, path = "./VF-figures")
ggsave("VF-Fig_3.tiff", plot = fig3, width = 5, height = 4, path = "./VF-figures")
ggsave("VF-Fig_3.png", plot = fig3, width = 5, height = 4, path = "./VF-figures")

# figure 4 save
ggsave("VF-Fig_4.pdf", plot = fig4, width = 8, height = 4, device = cairo_pdf, path = "./VF-figures")
ggsave("VF-Fig_4.tiff", plot = fig4, width = 8, height = 4, path = "./VF-figures")
ggsave("VF-Fig_4.png", plot = fig4, width = 8, height = 4, path = "./VF-figures")

# figure 5 save
ggsave("VF-Fig_5.pdf", plot = fig5, width = 6, height = 4, device = cairo_pdf, path = "./VF-figures")
ggsave("VF-Fig_5.tiff", plot = fig5, width = 6, height = 4, path = "./VF-figures")
ggsave("VF-Fig_5.png", plot = fig5, width = 6, height = 4, path = "./VF-figures")

# figure 6 save
ggsave("VF-Fig_6.pdf", plot = fig6, width = 5, height = 4, device = cairo_pdf, path = "./VF-figures")
ggsave("VF-Fig_6.tiff", plot = fig6, width = 5, height = 4, path = "./VF-figures")
ggsave("VF-Fig_6.png", plot = fig6, width = 5, height = 4, path = "./VF-figures")

# figure extra means save
ggsave("VF-Fig_supplementary.pdf", plot = fig_supplementary, width = 5, height = 2.5, device = cairo_pdf, path = "./VF-figures")
ggsave("VF-Fig_supplementary.tiff", plot = fig_supplementary, width = 5, height = 2.5, path = "./VF-figures")
ggsave("VF-Fig_supplementary.png", plot = fig_supplementary, width = 5, height = 2.5, path = "./VF-figures")

# figure extra model save
ggsave("VF-Fig_fig_model_supplementary.pdf", plot = fig_model_supplementary, width = 5, height = 4, device = cairo_pdf, path = "./VF-figures")
ggsave("VF-Fig_fig_model_supplementary.tiff", plot = fig_model_supplementary, width = 5, height = 4, path = "./VF-figures")
ggsave("VF-Fig_fig_model_supplementary.png", plot = fig_model_supplementary, width = 5, height = 4, path = "./VF-figures")

