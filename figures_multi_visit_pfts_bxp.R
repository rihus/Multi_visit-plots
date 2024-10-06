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
combined_pfts_cf <- read.csv("./IRC740H_2Dspiral_CF/vdp_hvp_results_August2024_visits/IRC740H_CF_PFTS_all_visits_final.csv")

##Get data for all participants with 2 visits
df_2visits <- combined_pfts_cf %>%
  group_by(Subject_id) %>%
  filter(all(c("Visit1", "Visit2") %in% VISIT)) %>%
  ungroup() %>%
  filter(VISIT %in% c("Visit1", "Visit2"))

##Get data for all participants with 3 visits
df_3visits <- combined_pfts_cf %>%
  group_by(Subject_id) %>%
  filter(all(c("Visit1", "Visit2", "Visit3") %in% VISIT)) %>%
  ungroup() %>%
  filter(VISIT %in% c("Visit1", "Visit2", "Visit3"))

# ##One visit only participants
# df_1visist <- combined_spir_data %>%
#   filter(VISIT %in% c("Visit1", "Visit2")) %>%
#   group_by(Subject_id) %>%
#   filter(n() == 2) %>%
#   ungroup()

###############################################2 visits
############# N4-corrected #################
vis_pFVC_mean <- aggregate(VDP ~  VISIT, df_2visits, mean)
visVDP_mean
vis_pFVC_sd <- aggregate(VDP ~  VISIT, df_2visits_N4, sd)
visVDP_sd
#Correction       VDP
#Visit1         9.344125
#Visit2         7.574875
#SD
#Visit1         8.546596
#Visit2         8.007886
##Statistical test
stat.test <- df_2visits_N4 %>%
  wilcox_test(VDP ~ VISIT , paired = TRUE) %>%
  add_significance()
stat.test
y_label <- expression(bold(VDP[Spiral-N4] * "(%)"))
# #Box plots with p-values VDP
vdp_bxp_nop <- ggpaired(df_2visits_N4, x = "VISIT", y = "VDP", fill = "VISIT",
                              palette = c("#b44582", "#cc79a7"), id = "Subject_id", width = 0.5,
                              ylim = c(0, 40), line.color = "black", line.size = 0.5,
                              legend = "none", xlab = "") +
  ylab(y_label) +
  geom_point(aes(color = Category), shape = 19, size = 3) +
  # geom_line(aes(group = paste(VISIT, Category)), color = "#000000", size = 0.5) +
  scale_color_manual(values = c("#000000", "#009e73")) +
  theme(panel.border = element_rect(color = "#000000", fill = NA, linewidth = 1),
        axis.text = element_text(size = 22, color = "#000000", face = "bold"),
        axis.title = element_text(size = 22, color = "#000000", face = "bold"),
        axis.line.x = element_line(linewidth = 1), axis.line.y = element_line(linewidth = 1))
vdp_bxp_nop

stat.test <- stat.test %>% add_xy_position(x = "VISIT")
vdp_bxp_p <-  vdp_bxp_nop + stat_pvalue_manual(stat.test, label = "P={scales::pvalue(p)}",
                                                           y.position = 34, label.size = 8, bracket.size = 0.8,
                                                           tip.length = 0.02, vjust=-0.35)
print(vdp_bxp_p)

# Save the plot as a png file in the specified directory
ggsave("./zR_plots_4ppr/vdp_N4_2visits_cbxp_nop.png", plot = vdp_bxp_nop, width = 4.5, height = 3.7, dpi = 300)
ggsave("./zR_plots_4ppr/vdp_N4_2visits_cbxp_p.png", plot = vdp_bxp_p, width = 4.5, height = 3.7, dpi = 300)
############ FA-corrected ##################
visVDP_mean <- aggregate(VDP ~  VISIT, df_2visits_FA, mean)
visVDP_mean
visVDP_sd <- aggregate(VDP ~  VISIT, df_2visits_FA, sd)
visVDP_sd
#Correction       VDP
#Visit1         7.718083
#Visit2         6.471625
#SD
#Visit1         7.539007
#Visit2         6.944800
##Statistical test
stat.test <- df_2visits_FA %>%
  wilcox_test(VDP ~ VISIT , paired = TRUE) %>%
  add_significance()
stat.test
y_label <- expression(bold(VDP[Spiral-FA] * "(%)"))
# #Box plots with p-values VDP
vdp_bxp_nop <- ggpaired(df_2visits_FA, x = "VISIT", y = "VDP", fill = "VISIT",
                        palette = c("#004166", "#0072b2"), id = "Subject_id", width = 0.5,
                        ylim = c(0, 40), line.color = "black", line.size = 0.5,
                        legend = "none", xlab = "") + ##
  ylab(y_label) +
  geom_point(aes(color = Category), shape = 19, size = 3) +
  # geom_line(aes(group = paste(VISIT, Category)), color = "#000000", size = 0.5) +
  scale_color_manual(values = c("#000000", "#009e73")) +
  theme(panel.border = element_rect(color = "#000000", fill = NA, linewidth = 1),
        axis.text = element_text(size = 22, color = "#000000", face = "bold"),
        axis.title = element_text(size = 22, color = "#000000", face = "bold"),
        axis.line.x = element_line(linewidth = 1), axis.line.y = element_line(linewidth = 1))
vdp_bxp_nop

stat.test <- stat.test %>% add_xy_position(x = "VISIT")
vdp_bxp_p <-  vdp_bxp_nop + stat_pvalue_manual(stat.test, label = "P={scales::pvalue(p)}",
                                               y.position = 34, label.size = 8, bracket.size = 0.8,
                                               tip.length = 0.02, vjust=-0.35)
print(vdp_bxp_p)

# Save the plot as a png file in the specified directory
ggsave("./zR_plots_4ppr/vdp_FA_2visits_cbxp_nop.png", plot = vdp_bxp_nop, width = 4.5, height = 3.7, dpi = 300)
ggsave("./zR_plots_4ppr/vdp_FA_2visits_cbxp_p.png", plot = vdp_bxp_p, width = 4.5, height = 3.7, dpi = 300)

###############################################Both 2 and 3 visits
df_2o3_visits <- combined_spir_data %>%
  group_by(Subject_id) %>%
  filter(all(c("Visit1", "Visit2") %in% VISIT)) %>%
  filter(any(c("Visit3") %in% VISIT) | VISIT %in% c("Visit1", "Visit2")) %>%
  ungroup()
df_2o3_visits_N4 <- df_2o3_visits[df_2o3_visits$Correction == "N4", ]
df_2o3_visits_FA <- df_2o3_visits[df_2o3_visits$Correction == "FA", ]
y_label <- expression(bold(VDP[Spiral-N4] * "(%)"))
# #Box plots with p-values VDP - N4
vdp_bxp_nop <- ggpaired(df_2o3_visits_N4, x = "VISIT", y = "VDP", fill = "VISIT",
                        palette = c("#b44582", "#cc79a7", "#e1b0cb"), id = "Subject_id", width = 0.5,
                        ylim = c(0, 40), line.color = "black", line.size = 0.5,
                        legend = "none", xlab = "") +
  ylab(y_label) +
  geom_point(aes(color = Category), shape = 19, size = 3) +
  # geom_line(aes(group = paste(Correction, Category)), color = "#000000", size = 0.5) +
  scale_color_manual(values = c("#000000", "#009e73")) +
  theme(panel.border = element_rect(color = "#000000", fill = NA, linewidth = 1),
        axis.text = element_text(size = 22, color = "#000000", face = "bold"),
        axis.title = element_text(size = 22, color = "#000000", face = "bold"),
        axis.line.x = element_line(linewidth = 1), axis.line.y = element_line(linewidth = 1))
vdp_bxp_nop
# Save the plot as a png file in the specified directory
ggsave("./zR_plots_4ppr/vdp_N4_2o3visits_cbxp_nop.png", plot = vdp_bxp_nop, width = 4.5, height = 3.7, dpi = 300)

################################################
# #Box plots with p-values VDP - FA
y_label <- expression(bold(VDP[Spiral-FA] * "(%)"))
vdp_bxp_nop <- ggpaired(df_2o3_visits_FA, x = "VISIT", y = "VDP", fill = "VISIT",
                        palette = c("#004166", "#0072b2", "#00a3ff"), id = "Subject_id", width = 0.5,
                        ylim = c(0, 40), line.color = "black", line.size = 0.5,
                        legend = "none", xlab = "") +
  ylab(y_label) +
  geom_point(aes(color = Category), shape = 19, size = 3) +
  # geom_line(aes(group = paste(Correction, Category)), color = "#000000", size = 0.5) +
  scale_color_manual(values = c("#000000", "#009e73")) +
  theme(panel.border = element_rect(color = "#000000", fill = NA, linewidth = 1),
        axis.text = element_text(size = 22, color = "#000000", face = "bold"),
        axis.title = element_text(size = 22, color = "#000000", face = "bold"),
        axis.line.x = element_line(linewidth = 1), axis.line.y = element_line(linewidth = 1))
vdp_bxp_nop
# Save the plot as a png file in the specified directory
ggsave("./zR_plots_4ppr/vdp_FA_2o3visits_cbxp_nop.png", plot = vdp_bxp_nop, width = 4.5, height = 3.7, dpi = 300)





###############################################3 visits
# #Box plots with p-values VDP - N4
vdp_bxp_nop <- ggpaired(df_3visits_N4, x = "VISIT", y = "VDP", fill = "VISIT",
                        palette = c("#b44582", "#cc79a7", "#e1b0cb"), id = "Subject_id", width = 0.5,
                        ylim = c(0, 40), line.color = "black", line.size = 0.5,
                        legend = "none", xlab = "") +
  ylab(y_label) +
  geom_point(aes(color = Category), shape = 19, size = 3) +
  # geom_line(aes(group = paste(Correction, Category)), color = "#000000", size = 0.5) +
  scale_color_manual(values = c("#000000", "#009e73")) +
  theme(panel.border = element_rect(color = "#000000", fill = NA, linewidth = 1),
        axis.text = element_text(size = 22, color = "#000000", face = "bold"),
        axis.title = element_text(size = 22, color = "#000000", face = "bold"),
        axis.line.x = element_line(linewidth = 1), axis.line.y = element_line(linewidth = 1))
vdp_bxp_nop

# Save the plot as a png file in the specified directory
ggsave("./zR_plots_4ppr/vdp_N4_3visits_cbxp_nop.png", plot = vdp_bxp_nop, width = 4.5, height = 3.7, dpi = 300)

# #Box plots with p-values VDP - FA
vdp_bxp_nop <- ggpaired(df_3visits_FA, x = "VISIT", y = "VDP", fill = "VISIT",
                        palette = c("#004166", "#0072b2", "#00a3ff"), id = "Subject_id", width = 0.5,
                        ylim = c(0, 40), line.color = "black", line.size = 0.5,
                        legend = "none", xlab = "") +
  ylab(y_label) +
  geom_point(aes(color = Category), shape = 19, size = 3) +
  # geom_line(aes(group = paste(Correction, Category)), color = "#000000", size = 0.5) +
  scale_color_manual(values = c("#000000", "#009e73")) +
  theme(panel.border = element_rect(color = "#000000", fill = NA, linewidth = 1),
        axis.text = element_text(size = 22, color = "#000000", face = "bold"),
        axis.title = element_text(size = 22, color = "#000000", face = "bold"),
        axis.line.x = element_line(linewidth = 1), axis.line.y = element_line(linewidth = 1))
vdp_bxp_nop

# Save the plot as a png file in the specified directory
ggsave("./zR_plots_4ppr/vdp_FA_3visits_cbxp_nop.png", plot = vdp_bxp_nop, width = 4.5, height = 3.7, dpi = 300)



