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

connected_bxp <- function(data_in, x_var, y_var, id = "Subject_id", ylim,
                          palette = NULL, xlab = "", ylab = NULL) {
  require(ggpubr)
  # Check if palette is provided
  if (is.null(palette) || is.null(ylab)) {
    palette <- c("#999999", "#555555")
    ylab <- y_var}
  
  # Create the ggpaired plot
  bxp <- ggpaired(data_in, x = x_var, y = y_var, id = id,
                  fill = x_var, palette = palette,
                  width = 0.5, ylim = ylim, line.color = "#000000",
                  line.size = 0.5, legend = "none", xlab = xlab) +
    ylab(ylab) +
    geom_point(shape = 19, size = 3) +
    theme(panel.border = element_rect(color = "#000000", fill = NA, linewidth = 1),
          axis.text = element_text(size = 22, color = "#000000", face = "bold"),
          axis.title = element_text(size = 22, color = "#000000", face = "bold"),
          axis.line.x = element_line(linewidth = 1), axis.line.y = element_line(linewidth = 1))
  print(bxp)
  return(bxp)
}

calc_pval <- function(data_in, x_var, y_var, tst = "wilcox", paired = TRUE) {
  formula <- as.formula(paste(y_var, "~", x_var))
  vis_mean <- aggregate(formula, data_in, mean)
  print("Mean")
  print(vis_mean)
  vis_sd <- aggregate(formula, data_in, sd)
  print("SD")
  print(vis_sd)
  ## Statistical test
  pval <- NULL
  if (tst == "wilcox") {
    pval <- data_in %>%
      wilcox_test(formula, paired = paired) %>%
      add_significance()
  } else if (tst == "ttest") {
    pval <- data_in %>%
      t_test(formula, paired = paired) %>%
      add_significance()
  } else {
    print("For tst only Wilcoxon (wilcox) or T-test (ttest) are accepted")
  }
  print(pval)
  return(pval)
}

calc_add_p <- function(data_in, x_var, y_var, fig_handle, py_pos,
                       tst = "wilcox", paird = TRUE, addp_eq=FALSE) {
  p_thresh <- calc_pval(data_in, x_var, y_var, tst, paird)
  p_thresh <- p_thresh %>% add_xy_position(x = x_var)
  if (addp_eq == TRUE) {plabel = "P={scales::pvalue(p)}"}
  else {plabel = "P{scales::pvalue(p)}"}
  bxp_p <- fig_handle + stat_pvalue_manual(p_thresh, label = plabel,
                                           y.position = py_pos, label.size = 8,
                                           bracket.size = 0.8,
                                           tip.length = 0.03, vjust=-0.15)
  print(bxp_p)
  return(bxp_p)
}

################################################################################
# ##Load the data from both CSV files and arrange it to plot
combined_pfts_cf <- read.csv("./IRC740H_2Dspiral_CF/vdp_hvp_results_August2024_visits/IRC740H_CF_PFTS_all_visits_final.csv")

##Get data for all participants with 2 visits
df_2visits <- combined_pfts_cf %>%
  group_by(Subject_id) %>%
  filter(all(c("Baseline", "Year 1") %in% VISIT)) %>%
  ungroup() %>%
  filter(VISIT %in% c("Baseline", "Year 1")) %>%
  filter(Subject_id != "IRC740H-021") #%>%  ##Removed: Not on modulator therapy
  # filter(Subject_id != "IRC740H-008") %>%     ##Removed: FEV1 < 90%
  # filter(Subject_id != "IRC740H-014") %>%     ##Removed: FEV1 < 90%
  # filter(Subject_id != "IRC740H-022") %>%     ##Removed: FEV1 < 90%
  # filter(Subject_id != "IRC740H-023") %>%     ##Removed: FEV1 < 90%
  # filter(Subject_id != "IRC740H-024") %>%     ##Removed: FEV1 < 90%
  # filter(Subject_id != "IRC740H-025") %>%     ##Removed: FEV1 < 90%
  # filter(Subject_id != "IRC740H-027") %>%     ##Removed: FEV1 < 90%
  # filter(Subject_id != "IRC740H-028")         ##Removed: FEV1 < 90%

##Get data for all participants with 3 visits
df_3visits <- combined_pfts_cf %>%
  group_by(Subject_id) %>%
  filter(all(c("Baseline", "Year 1", "Year 2") %in% VISIT)) %>%
  ungroup() %>%
  filter(VISIT %in% c("Baseline", "Year 1", "Year 2")) %>%
  filter(Subject_id != "IRC740H-021")

###############################################2 visits
# #Box plots with p-values
fvc_bxp_nop <- connected_bxp(df_2visits, "VISIT", "pp_FVC", id = "Subject_id",
                             c(70, 150), palette = c("coral4", "coral1"),
                             xlab = "", ylab = "%Pred FVC (%)")
fvc_bxp_p <- calc_add_p(df_2visits, "VISIT", "pp_FVC", fvc_bxp_nop,
                        140, tst = "wilcox", paird = TRUE, addp_eq=TRUE)

fev1_bxp_nop <- connected_bxp(df_2visits, "VISIT", "pp_FEV1", id = "Subject_id",
                             c(70, 150), palette = c("chocolate4", "chocolate1"),
                             xlab = "", ylab = "%Pred FEV1 (%)")
fev1_bxp_p <- calc_add_p(df_2visits, "VISIT", "pp_FEV1", fev1_bxp_nop,
                        140, tst = "wilcox", paird = TRUE, addp_eq=TRUE)

r_fev1fvc_bxp_nop <- connected_bxp(df_2visits, "VISIT", "pp_FEV1_FVC", id = "Subject_id",
                              c(70, 130), palette = c("indianred4", "indianred1"),
                              xlab = "", ylab = "ppFEV1/FVC (%)")
r_fev1fvc_bxp_p <- calc_add_p(df_2visits, "VISIT", "pp_FEV1_FVC", r_fev1fvc_bxp_nop,
                         120, tst = "wilcox", paird = TRUE, addp_eq=TRUE)

r_fef2575_bxp_nop <- connected_bxp(df_2visits, "VISIT", "pp_FEF25_75", id = "Subject_id",
                                   c(0, 210), palette = c("pink4", "pink1"),
                                   xlab = "", ylab = "ppFEF25-75 (%)") 
r_fef2575_bxp_p <- calc_add_p(df_2visits, "VISIT", "pp_FEF25_75", r_fef2575_bxp_nop,
                              190, tst = "wilcox", paird = TRUE, addp_eq=TRUE)

# Save the plots as a png file in the specified directory
ggsave("./zR_plots_4ppr/figs_multi_visit/pfts_fvc_2visits_cbxp_nop.png", plot = fvc_bxp_nop, width = 4.5, height = 3.8, dpi = 300)
ggsave("./zR_plots_4ppr/figs_multi_visit/pfts_fvc_2visits_cbxp_p.png", plot = fvc_bxp_p, width = 4.5, height = 3.8, dpi = 300)

ggsave("./zR_plots_4ppr/figs_multi_visit/pfts_fev1_2visits_cbxp_nop.png", plot = fev1_bxp_nop, width = 4.5, height = 3.8, dpi = 300)
ggsave("./zR_plots_4ppr/figs_multi_visit/pfts_fev1_2visits_cbxp_p.png", plot = fev1_bxp_p, width = 4.5, height = 3.8, dpi = 300)

ggsave("./zR_plots_4ppr/figs_multi_visit/pfts_fev1fvc_ratio_2visits_cbxp_nop.png", plot = r_fev1fvc_bxp_nop, width = 4.5, height = 3.8, dpi = 300)
ggsave("./zR_plots_4ppr/figs_multi_visit/pfts_fev1fvc_ratio_2visits_cbxp_p.png", plot = r_fev1fvc_bxp_p, width = 4.5, height = 3.8, dpi = 300)

ggsave("./zR_plots_4ppr/figs_multi_visit/pfts_fef2575_ratio_2visits_cbxp_nop.png", plot = r_fef2575_bxp_nop, width = 4.5, height = 3.8, dpi = 300)
ggsave("./zR_plots_4ppr/figs_multi_visit/pfts_fef2575_ratio_2visits_cbxp_p.png", plot = r_fef2575_bxp_p, width = 4.5, height = 3.8, dpi = 300)




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
ggsave("./zR_plots_4ppr/figs_multi_visit/vdp_N4_2visits_cbxp_nop.png", plot = vdp_bxp_nop, width = 4.5, height = 3.7, dpi = 300)

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
ggsave("./zR_plots_4ppr/figs_multi_visit/vdp_FA_2o3visits_cbxp_nop.png", plot = vdp_bxp_nop, width = 4.5, height = 3.7, dpi = 300)





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



