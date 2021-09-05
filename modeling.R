library(gamlss.dist)
library(tidyverse)
library(magrittr)
library(ggpubr)

# define a function
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
df1 <- rbind(group1_1_df, group1_2_df)

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
df2 <- rbind(group2_1_df, group2_2_df)

# plotting for df1

# function for calculating %95 confidence intervals
ci <- function(x){1.96*(sd(x)/sqrt(length(x)))}

# getting summaries
ncr_summary <- df1 %>% group_by(group, time) %>% 
  summarise(mean_ncr = mean(ncr), ci = ci(ncr))
ncr_summary$group %<>% recode("1" = "Late", "2" = "Native") 

srt_summary <- df1 %>% group_by(group) %>% 
  summarise(srt = mean(srt))
srt_summary$group %<>% recode("1" = "Late", "2" = "Native") 
  

pd2 <- position_dodge(0.3)

#plotting
plot1 <- ncr_summary %>%
  ggplot(aes(time, mean_ncr, group = group, linetype = group)) + 
  # geom_ribbon(aes(ymax = mean_ncr + se, ymin = mean_ncr - se), fill = "red", alpha = 0.3) +
  geom_line(position = pd2, size = .75) +
  geom_point(size = 2.5, aes(fill = group), shape = 22, colour = "black") +
  labs(x="Time (Sec)", y="Mean Response") +
  ggtitle("Similar vocabulary size and different retrieval rate") +
  annotate(geom="text", x=srt_summary$srt[2], y=15, label= expression("SRT"[1]), color="black", size = 4.5) +
  annotate(geom="text", x=srt_summary$srt[1], y=17, label= expression("SRT"[2]), color="black", size = 4.5) +
  geom_segment(aes(x = srt_summary$srt[2], y = 14.5, xend = srt_summary$srt[2], yend = 13.5), size = 1,
               arrow = arrow(length = unit(0.2, "cm"))) +
  geom_segment(aes(x = srt_summary$srt[1], y = 16.2, xend = srt_summary$srt[1], yend = 15.2), size = 1,
               arrow = arrow(length = unit(0.2, "cm"))) + theme_bw() +
    theme(text=element_text(family="Times New Roman", size=12), legend.position = "bottom",
          axis.text = element_blank(), axis.ticks = element_blank())

# plotting for df2s
# getting summaries
ncr_summary2 <- df2 %>% group_by(time, group) %>%
  summarise(mean_ncr = mean(ncr), ci = ci(ncr))
ncr_summary2$group %<>% recode("1" = "Late", "2" = "Native") 

srt_summary2 <- df2 %>% group_by(group) %>% 
  summarise(srt = mean(srt)) 
srt_summary2$group %<>% recode("1" = "Late", "2" = "Native") 

# plotting
plot2 <- ncr_summary2 %>%
  ggplot(aes(time, mean_ncr, group = group, linetype = group)) + 
  geom_line(position = pd2, size = .75) +
  geom_point(size = 2.5, aes(fill = group), shape = 22, colour = "black") +
  labs(x="Time (Sec)", y="Mean Response") +
  ggtitle("Different vocabulary size and similar retrieval rate") +
  annotate(geom="text", x=srt_summary2$srt[1], y=14.50, label= expression("SRT"[1]), color="black", size = 4.5) +
  annotate(geom="text", x=srt_summary2$srt[2], y=10.25, label= expression("SRT"[2]), color="black", size = 4.5) + 
  geom_segment(aes(x = srt_summary2$srt[1], y = 14.00, xend = srt_summary2$srt[1], yend = 13.0), size = 1,
               arrow = arrow(length = unit(0.2, "cm"))) +
  geom_segment(aes(x = srt_summary2$srt[2], y = 9.75, xend = srt_summary2$srt[2], yend = 8.75), size = 1,
               arrow = arrow(length = unit(0.2, "cm"))) + theme_bw() +
  theme(text=element_text(family="Times New Roman", size=12), legend.position = "bottom",
        axis.text = element_blank(), axis.ticks = element_blank())
 
plot2 + scale_shape_manual(values = c(0,15))

# combine two plots
combined_plot <- ggarrange(plot1, plot2,
         labels = c("A", "B"),
         ncol = 2, nrow = 1)
combined_plot + theme(text=element_text(family="Times New Roman", size=12))
print(combined_plot)
#ggsave("modeling_plot.png", combined_plot, height = 5, width = 17)
