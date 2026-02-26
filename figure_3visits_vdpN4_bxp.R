#load ggplot2
library(ggplot2)
library(ggpubr)
library(blandr)
library(dplyr)
library(tidyr)
library(rstatix)

#Set the working directory to the path where your CSV files are located
setwd("C:/Users/HUSDQ4/OneDrive - cchmc/cincy_work/all_projects_data_work/vdp_analysis/CFNonCF_Bronch")
# linetypes: 0 = blank, 1 = solid, 2 = dashed, 3 = dotted, 4 = dotdash, 5 = longdash, 6 = twodash

################################################################################
# ##Load the data from both CSV files and arrange it to plot
N4corr_spir_cf <- read.csv("./IRC740H_2Dspiral_CF/vdpN4_results_February2026_visits/N4_corr_glb-mean_analysis_results.csv")

combined_spir_data <- rbind(transform(N4corr_spir_cf, Correction = "N4", Category = "CF"))

##Get data for all participants with 3 visits
vdp_3visits <- combined_spir_data %>%
  group_by(Subject_id) %>%
  filter(all(c("Baseline", "Follow-up 1", "Follow-up 2") %in% VISIT)) %>%
  ungroup() %>%
  filter(VISIT %in% c("Baseline", "Follow-up 1", "Follow-up 2")) %>%
  filter(Subject_id != "IRC740H-021")

vdp_3visits_N4 <- vdp_3visits[vdp_3visits$Correction == "N4", ]

##Get data for all participants with 2 or 3 visits
df_2o3_visits <- combined_spir_data %>%
  group_by(Subject_id) %>%
  filter(all(c("Baseline", "Follow-up 1") %in% VISIT)) %>%
  filter(any(c("Follow-up 2") %in% VISIT) | VISIT %in% c("Baseline", "Follow-up 1")) %>%
  ungroup()
df_2o3_visits_N4 <- df_2o3_visits[df_2o3_visits$Correction == "N4", ]

###############################################Both 2 and 3 visits

y_label <- expression(bold(VDP[N4] * " (%)"))
# #Box plots with p-values VDP - N4
vdp_bxp_nop <- ggpaired(df_2o3_visits_N4, x = "VISIT", y = "VDP", fill = "VISIT",
                        palette = c("#b44582", "#cc79a7", "#e1b0cb"), id = "Subject_id", width = 0.5,
                        ylim = c(0, 40), line.color = "black", line.size = 0.5,
                        legend = "none", xlab = "") +
  ylab(y_label) +
  theme(panel.border = element_rect(color = "#000000", fill = NA, linewidth = 1),
        axis.text = element_text(size = 22, color = "#000000", face = "bold"),
        axis.title = element_text(size = 22, color = "#000000", face = "bold"),
        axis.line.x = element_line(linewidth = 1), axis.line.y = element_line(linewidth = 1))
vdp_bxp_nop
# Save the plot as a png file in the specified directory
ggsave("./zR_plots_4ppr/vdp_N4_2o3visits_cbxp_nop.png", plot = vdp_bxp_nop, width = 4.5, height = 3.7, dpi = 300)


###############################################3 visits
# #Box plots with p-values VDP - N4
vdp_bxp_nop <- ggpaired(vdp_3visits_N4, x = "VISIT", y = "VDP", fill = "VISIT",
                        palette = c("#b44582", "#cc79a7", "#e1b0cb"), id = "Subject_id", width = 0.5,
                        ylim = c(0, 40), line.color = "black", line.size = 0.5,
                        legend = "none", xlab = "") +
  ylab(y_label) +
  theme(panel.border = element_rect(color = "#000000", fill = NA, linewidth = 1),
        axis.text = element_text(size = 22, color = "#000000", face = "bold"),
        axis.title = element_text(size = 22, color = "#000000", face = "bold"),
        axis.line.x = element_line(linewidth = 1), axis.line.y = element_line(linewidth = 1))
vdp_bxp_nop

# Save the plot as a png file in the specified directory
ggsave("./zR_plots_4ppr/vdp_N4_3visits_cbxp_nop.png", plot = vdp_bxp_nop, width = 4.5, height = 3.7, dpi = 300)

