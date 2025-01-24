#load ggplot2
library(ggplot2)
library(ggpubr)
library(blandr)
library(dplyr)
library(tidyr)
library(rstatix)

#Set the working directory to the path where your CSV files are located
setwd("C:/Users/HUSDQ4/OneDrive - cchmc/cincy_work/Confs_Courses_talks_pprs")
# linetypes: 0 = blank, 1 = solid, 2 = dashed, 3 = dotted, 4 = dotdash, 5 = longdash, 6 = twodash

################################################################################
# ##Load the data from both CSV files and arrange it to plot
data <- read.csv("./Riaz_CF_multi_visit_ppr/IRC740H_N4VDP_PFTs_2visits.csv")

# FEV1
fe1_test <- wilcox.test(data$FEV1_B, data$FEV1_Y1, paired = TRUE)
fe1_p_value <- fe1_test$p.value

# VDP
vdp_test <- wilcox.test(data$VDP_B, data$VDP_Y1, paired = TRUE)
vdp_p_value <- vdp_test$p.value

# FVC
fvc_test <- wilcox.test(data$FVC_B, data$FVC_Y1, paired = TRUE)
fvc_p_value <- fvc_test$p.value

# FEV1/FVC
fev1_fvc_test <- wilcox.test(data$FEV1oFVC_B, data$FEV1oFVC_Y1, paired = TRUE)
fev1_fvc_p_value <- fev1_fvc_test$p.value

# Prepare data for plotting
data_long <- data %>%
  pivot_longer(cols = -Subject, names_to = c("Measurement", "Time"), names_sep = "_") %>%
  filter(Measurement %in% c("VDP", "FEV1", "FVC", "FEV1oFVC"))

# Plot connected boxplots with p-values
ggplot(data_long, aes(x = Time, y = value)) +
  geom_boxplot(aes(fill = Time), alpha = 0.5) +  # Apply fill aesthetic only to boxplot
  geom_line(aes(group = Subject), color = "gray", alpha = 0.5) +  # Connect points for each subject
  facet_wrap(~ Measurement, scales = "free") +
  stat_compare_means(
    aes(group = Time),  # Explicitly specify the group aesthetic for stat_compare_means
    paired = TRUE, 
    method = "wilcox.test", 
    label = "p.format", 
    label.x = 1.25, 
    label.y = Inf, 
    vjust = 2
  ) +
  scale_y_continuous(expand = expansion(mult = 0.5)) +
  theme_bw() +
  labs(title = "Comparison of Measurements at Baseline and Year 1",
       x = "Time Point",
       y = "Values") +
  theme(legend.position = "none")

# Print p-values
cat("FEV1 p-value:", fe1_p_value, "\n")
cat("VDP p-value:", vdp_p_value, "\n")
cat("FVC p-value:", fvc_p_value, "\n")
cat("FEV1/FVC p-value:", fev1_fvc_p_value, "\n")



