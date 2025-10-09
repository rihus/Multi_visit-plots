#load ggplot2
library(ggplot2)
library(ggpubr)
library(blandr)
library(dplyr)
library(tidyr)
library(rstatix)

#Set the working directory to the path where your CSV files are located
setwd("C:/Users/HUSDQ4/OneDrive - cchmc/cincy_work/Confs_Courses_talks_pprs/Papers")
# linetypes: 0 = blank, 1 = solid, 2 = dashed, 3 = dotted, 4 = dotdash, 5 = longdash, 6 = twodash

################################################################################
# ##Load the data from both CSV files and arrange it to plot
data <- read.csv("./Riaz_CF_multi_visit_ppr/IRC740H_N4VDP_PFTs_2visits.csv")

# Age
age_test <- wilcox.test(data$Age_scan1[data$Sex == "M"], data$Age_scan1[data$Sex == "F"],
                        paired = FALSE)
age_test <- age_test$p.value
print(age_test)

# #FEV1
fe1_test <- wilcox.test(data$FEV1_B[data$Sex == "M"], data$FEV1_B[data$Sex == "F"],
                        paired = FALSE)
fe1b_p <- fe1_test$p.value

fe1_test <- wilcox.test(data$FEV1_Y1[data$Sex == "M"], data$FEV1_Y1[data$Sex == "F"],
                        paired = FALSE)
fe1y1_p <- fe1_test$p.value

fe1_test <- wilcox.test(data$FEV1_Diff[data$Sex == "M"], data$FEV1_Diff[data$Sex == "F"],
                        paired = FALSE)
fe1_test <- fe1_test$p.value
print(fe1_test)

# #VDP
vdp_test <- wilcox.test(data$VDP_B[data$Sex == "M"], data$VDP_B[data$Sex == "F"],
                        paired = FALSE)
vdpb_p <- vdp_test$p.value

vdp_test <- wilcox.test(data$VDP_Y1[data$Sex == "M"], data$VDP_Y1[data$Sex == "F"],
                        paired = FALSE)
vdpy1_p <- vdp_test$p.value

vdp_test <- wilcox.test(data$VDP_Diff[data$Sex == "M"], data$VDP_Diff[data$Sex == "F"],
                        paired = FALSE)
vdp_test <- vdp_test$p.value

vdp_test <- wilcox.test(data$VDP_Diff[data$Sex == "M"], data$VDP_Diff[data$Sex == "F"],
                        paired = FALSE)
vdp_test <- vdp_test$p.value
print(vdp_test)

# FVC
fvc_test <- wilcox.test(data$FVC_B[data$Sex == "M"], data$FVC_B[data$Sex == "F"],
                        paired = FALSE)
fvcb_p <- fvc_test$p.value

fvc_test <- wilcox.test(data$FVC_Y1[data$Sex == "M"], data$FVC_Y1[data$Sex == "F"],
                        paired = FALSE)
fvcy1_p <- fvc_test$p.value

fvc_test <- wilcox.test(data$FVC_Diff[data$Sex == "M"], data$FVC_Diff[data$Sex == "F"],
                        paired = FALSE)
fvc_test <- fvc_test$p.value
print(fvc_test)

# FEV1/FVC
fe1fvc_test <- wilcox.test(data$FEV1oFVC_B[data$Sex == "M"], data$FEV1oFVC_B[data$Sex == "F"],
                        paired = FALSE)
fe1fvcb_p <- fe1fvc_test$p.value

fe1fvc_test <- wilcox.test(data$FEV1oFVC_Y1[data$Sex == "M"], data$FEV1oFVC_Y1[data$Sex == "F"],
                        paired = FALSE)
fe1fvcy1_p <- fe1fvc_test$p.value

fe1fvc_test <- wilcox.test(data$FEV1oFVC_Diff[data$Sex == "M"], data$FEV1oFVC_Diff[data$Sex == "F"],
                        paired = FALSE)
fe1fvc_test <- fe1fvc_test$p.value
print(fe1fvc_test)

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

# Reshape the data to long format for easier plotting
data_long <- data %>%
  pivot_longer(cols = c(Age_scan1, VDP_Diff), 
               names_to = "Parameter", 
               values_to = "Value")

# Create boxplots for each parameter
ggplot(data_long, aes(x = Sex, y = Value, fill = Sex)) +
  geom_boxplot() +
  stat_compare_means(aes(group = Sex), 
                     method = "wilcox.test", 
                     label = "p.format", 
                     comparisons = list(c("M", "F")), 
                     label.y.npc = "top") +
  facet_wrap(~ Parameter, scales = "free_y") + # Create separate plots for each parameter
  labs(title = "Comparison of Parameters by Sex",
       x = "Sex",
       y = "Value") +
  scale_y_continuous(expand = expansion(mult = 0.5)) +
  theme_bw() +
  theme(legend.position = "none")

