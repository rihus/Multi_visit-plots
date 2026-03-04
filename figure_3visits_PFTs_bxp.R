#load ggplot2
library(ggplot2)
library(ggpubr)
library(blandr)
library(dplyr)
library(tidyr)
library(rstatix)
library(PMCMRplus)

#Set the working directory to the path where your CSV files are located
setwd("C:/Users/HUSDQ4/OneDrive - cchmc/cincy_work/all_projects_data_work/vdp_analysis/CFNonCF_Bronch/IRC740H_2Dspiral_CF")
# linetypes: 0 = blank, 1 = solid, 2 = dashed, 3 = dotted, 4 = dotdash, 5 = longdash, 6 = twodash

################################################################################
# ##Load the data from both CSV files and arrange it to plot
all_visits_pfts <- read.csv("./vdpN4_results_February2026_visits/IRC740H_CF_PFTS_all_visits.csv")

##Get data for all participants with 3 visits
df_3visits <- all_visits_pfts %>%
  group_by(Subject_id) %>%
  filter(all(c("Baseline", "Follow-up 1", "Follow-up 2") %in% VISIT)) %>%
  ungroup() %>%
  filter(VISIT %in% c("Baseline", "Follow-up 1", "Follow-up 2")) %>%
  filter(Subject_id != "IRC740H-021")

###############################################3 visits
# #Box plots: FEV1
friedman_result <- friedman.test(pp_FEV1 ~ VISIT | Subject_id, data = df_3visits)
friedman_p <- signif(friedman_result$p.value, 3)
y_label <- expression(bold(FEV[1] * " (%)"))
# #Box plots with p-values FEV1
fev1_3vist_bxp <- ggpaired(df_3visits, x = "VISIT", y = "pp_FEV1", fill = "VISIT",
                        palette = c("#2A52BE", "#87CEEB", "#8FD9FB"), id = "Subject_id", width = 0.5,
                        ylim = c(70, 150), line.color = "black", line.size = 0.5,
                        legend = "none", xlab = "") +
  ylab(y_label) +
  theme(panel.border = element_rect(color = "#000000", fill = NA, linewidth = 1),
        axis.text = element_text(size = 22, color = "#000000", face = "bold"),
        axis.title = element_text(size = 22, color = "#000000", face = "bold"),
        axis.line.x = element_line(linewidth = 1), axis.line.y = element_line(linewidth = 1)) +
        annotate("text", x = 2, y = 148, label = paste("Friedman p =", friedman_p),
                 size = 6, fontface = "bold")
print(fev1_3vist_bxp)
### Save the plot as a png file in the specified directory
ggsave("./zR_plots/fev1_3visits_cbxp_p.png", plot = fev1_3vist_bxp, width = 7.5, height = 5.2, dpi = 300)

# #Box plots: FVC
friedman_result <- friedman.test(pp_FVC ~ VISIT | Subject_id, data = df_3visits)
friedman_p <- signif(friedman_result$p.value, 3)
y_label <- expression(bold(FVC* " (%)"))
# #Box plots with p-values FEV1
fvc_3vist_bxp <- ggpaired(df_3visits, x = "VISIT", y = "pp_FVC", fill = "VISIT",
                           palette = c("#2A52BE", "#87CEEB", "#8FD9FB"), id = "Subject_id", width = 0.5,
                           ylim = c(70, 150), line.color = "black", line.size = 0.5,
                           legend = "none", xlab = "") +
  ylab(y_label) +
  theme(panel.border = element_rect(color = "#000000", fill = NA, linewidth = 1),
        axis.text = element_text(size = 22, color = "#000000", face = "bold"),
        axis.title = element_text(size = 22, color = "#000000", face = "bold"),
        axis.line.x = element_line(linewidth = 1), axis.line.y = element_line(linewidth = 1)) +
  annotate("text", x = 2, y = 148, label = paste("Friedman p =", friedman_p),
           size = 6, fontface = "bold")
print(fvc_3vist_bxp)
### Save the plot as a png file in the specified directory
ggsave("./zR_plots/fvc_3visits_cbxp_p.png", plot = fvc_3vist_bxp, width = 7.5, height = 5.2, dpi = 300)

# #Box plots: FEV1/FVC
friedman_result <- friedman.test(pp_FEV1_FVC ~ VISIT | Subject_id, data = df_3visits)
friedman_p <- signif(friedman_result$p.value, 3)
y_label <- expression(bold(FEV[1]/FVC * " (%)"))
# #Box plots with p-values FEV1
fev1ofvc_3vist_bxp <- ggpaired(df_3visits, x = "VISIT", y = "pp_FEV1_FVC", fill = "VISIT",
                           palette = c("#2A52BE", "#87CEEB", "#8FD9FB"), id = "Subject_id", width = 0.5,
                           ylim = c(70, 150), line.color = "black", line.size = 0.5,
                           legend = "none", xlab = "") +
  ylab(y_label) +
  theme(panel.border = element_rect(color = "#000000", fill = NA, linewidth = 1),
        axis.text = element_text(size = 22, color = "#000000", face = "bold"),
        axis.title = element_text(size = 22, color = "#000000", face = "bold"),
        axis.line.x = element_line(linewidth = 1), axis.line.y = element_line(linewidth = 1)) +
  annotate("text", x = 2, y = 148, label = paste("Friedman p =", friedman_p),
           size = 6, fontface = "bold")
print(fev1ofvc_3vist_bxp)
### Save the plot as a png file in the specified directory
ggsave("./zR_plots/fev1ofvc_3visits_cbxp_p.png", plot = fev1ofvc_3vist_bxp, width = 7.5, height = 5.2, dpi = 300)

# #Box plots: FEF25_75
friedman_result <- friedman.test(pp_FEF25_75 ~ VISIT | Subject_id, data = df_3visits)
friedman_p <- signif(friedman_result$p.value, 3)
y_label <- expression(bold(FEF[25-75] * " (%)"))
# #Box plots with p-values FEV1
fef_3vist_bxp <- ggpaired(df_3visits, x = "VISIT", y = "pp_FEF25_75", fill = "VISIT",
                           palette = c("#2A52BE", "#87CEEB", "#8FD9FB"), id = "Subject_id", width = 0.5,
                           ylim = c(50, 180), line.color = "black", line.size = 0.5,
                           legend = "none", xlab = "") +
  ylab(y_label) +
  theme(panel.border = element_rect(color = "#000000", fill = NA, linewidth = 1),
        axis.text = element_text(size = 22, color = "#000000", face = "bold"),
        axis.title = element_text(size = 22, color = "#000000", face = "bold"),
        axis.line.x = element_line(linewidth = 1), axis.line.y = element_line(linewidth = 1)) +
  annotate("text", x = 2, y = 175, label = paste("Friedman p =", friedman_p),
           size = 6, fontface = "bold")
print(fef_3vist_bxp)

##If Friedman is significant, perform post-hoc Nemenyi
frdAllPairsNemenyiTest(pp_FEF25_75 ~ VISIT | Subject_id, data = df_3visits)

### Save the plot as a png file in the specified directory
ggsave("./zR_plots/fef2575_3visits_cbxp_p.png", plot = fef_3vist_bxp, width = 7.5, height = 5.2, dpi = 300)


################################################################################
##Get data for all participants with 2 or 3 visits
df_2o3_visits <- all_visits_pfts %>%
  group_by(Subject_id) %>%
  filter(all(c("Baseline", "Follow-up 1") %in% VISIT)) %>%
  filter(any(c("Follow-up 2") %in% VISIT) | VISIT %in% c("Baseline", "Follow-up 1")) %>%
  ungroup() %>%
  filter(VISIT %in% c("Baseline", "Follow-up 1", "Follow-up 2")) %>%
  filter(Subject_id != "IRC740H-021")

###############################################Combined 2 and 3 visits (can't do stat on it)
y_label <- expression(bold(FEV[1] * " (%)"))
# #Box plots: FEV1
fev1_bxp_nop <- ggpaired(df_2o3_visits, x = "VISIT", y = "pp_FEV1", fill = "VISIT",
                         palette = c("#2A52BE", "#87CEEB", "#8FD9FB"), id = "Subject_id", width = 0.5,
                         ylim = c(70, 150), line.color = "black", line.size = 0.5,
                         legend = "none", xlab = "") +
  ylab(y_label) +
  theme(panel.border = element_rect(color = "#000000", fill = NA, linewidth = 1),
        axis.text = element_text(size = 22, color = "#000000", face = "bold"),
        axis.title = element_text(size = 22, color = "#000000", face = "bold"),
        axis.line.x = element_line(linewidth = 1), axis.line.y = element_line(linewidth = 1))
print(fev1_bxp_nop)

# Save the plot as a png file in the specified directory
ggsave("./zR_plots/fev1_2o3visits_cbxp_nop.png", plot = fev1_bxp_nop, width = 7.5, height = 5.2, dpi = 300)

#######
y_label <- expression(bold(FVC* " (%)"))
# #Box plots: FVC
fvc_bxp_nop <- ggpaired(df_2o3_visits, x = "VISIT", y = "pp_FVC", fill = "VISIT",
                        palette = c("#2A52BE", "#87CEEB", "#8FD9FB"), id = "Subject_id", width = 0.5,
                        ylim = c(70, 150), line.color = "black", line.size = 0.5,
                        legend = "none", xlab = "") +
  ylab(y_label) +
  theme(panel.border = element_rect(color = "#000000", fill = NA, linewidth = 1),
        axis.text = element_text(size = 22, color = "#000000", face = "bold"),
        axis.title = element_text(size = 22, color = "#000000", face = "bold"),
        axis.line.x = element_line(linewidth = 1), axis.line.y = element_line(linewidth = 1))
print(fvc_bxp_nop)
# Save the plot as a png file in the specified directory
ggsave("./zR_plots/fvc_2o3visits_cbxp_nop.png", plot = fvc_bxp_nop, width = 7.5, height = 5.2, dpi = 300)
