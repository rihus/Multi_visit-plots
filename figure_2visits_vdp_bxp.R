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
N4corr_spir_cf <- read.csv("./IRC740H_2Dspiral_CF/vdp_hvp_results_August2024_visits/N4_corr_median_analysis_results.csv")
FAcorr_spir_cf <- read.csv("./IRC740H_2Dspiral_CF/vdp_hvp_results_August2024_visits/FA_corr_median_analysis_results.csv")

combined_spir_data <- rbind(transform(N4corr_spir_cf, Correction = "N4", Category = "CF"),
                            transform(FAcorr_spir_cf, Correction = "FA", Category = "CF"))

##Get data for all participants with 2 visits
vdp_2visits <- combined_spir_data %>%
  group_by(Subject_id) %>%
  filter(all(c("Baseline", "year1") %in% VISIT)) %>%
  ungroup() %>%
  filter(VISIT %in% c("Baseline", "year1")) %>%
  filter(Subject_id != "IRC740H-021") %>%  ##Removed: Not on modulator therapy
  filter(Subject_id != "IRC740H-024")      ##Removed: Modulator therapy status unknown

vdp_2visits_N4 <- vdp_2visits[vdp_2visits$Correction == "N4", ]
vdp_2visits_FA <- vdp_2visits[vdp_2visits$Correction == "FA", ]
# vdp_2visits_FA <- vdp_2visits_FA[-c(47, 48), ]

##Get data for all participants with 3 visits
vdp_3visits <- combined_spir_data %>%
  group_by(Subject_id) %>%
  filter(all(c("Baseline", "year1", "year2") %in% VISIT)) %>%
  ungroup() %>%
  filter(VISIT %in% c("Baseline", "year1", "year2")) %>%
  filter(Subject_id != "IRC740H-021")

vdp_3visits_N4 <- vdp_3visits[vdp_3visits$Correction == "N4", ]
vdp_3visits_FA <- vdp_3visits[vdp_3visits$Correction == "FA", ]

# ##One visit only participants
# df_1visist <- combined_spir_data %>%
#   filter(VISIT %in% c("Baseline", "year1")) %>%
#   group_by(Subject_id) %>%
#   filter(n() == 2) %>%
#   ungroup()

###############################################2 visits
############# N4-corrected #################
vdp_n4_bxp_nop <- connected_bxp(vdp_2visits_N4, "VISIT", "VDP", id = "Subject_id",
                             c(0, 35), palette = c("#b44582", "#cc79a7"),
                             xlab = "", ylab = expression(bold(VDP[N4] * "(%)")))
vdp_n4_bxp_p <- calc_add_p(vdp_2visits_N4, "VISIT", "VDP", vdp_n4_bxp_nop,
                        30, tst = "wilcox", paird = TRUE, addp_eq=TRUE)

lvp_n4_bxp_nop <- connected_bxp(vdp_2visits_N4, "VISIT", "LVP", id = "Subject_id",
                                c(0, 35), palette = c("#b44582", "#cc79a7"),
                                xlab = "", ylab = expression(bold(LVP[N4] * "(%)")))
lvp_n4_bxp_p <- calc_add_p(vdp_2visits_N4, "VISIT", "LVP", lvp_n4_bxp_nop,
                           30, tst = "wilcox", paird = TRUE, addp_eq=TRUE)

nvp_n4_bxp_nop <- connected_bxp(vdp_2visits_N4, "VISIT", "NV", id = "Subject_id",
                                c(0, 90), palette = c("#b44582", "#cc79a7"),
                                xlab = "", ylab = expression(bold(NVP[N4] * "(%)")))
nvp_n4_bxp_p <- calc_add_p(vdp_2visits_N4, "VISIT", "NV", nvp_n4_bxp_nop,
                           83, tst = "wilcox", paird = TRUE, addp_eq=TRUE)

evp_n4_bxp_nop <- connected_bxp(vdp_2visits_N4, "VISIT", "EVP", id = "Subject_id",
                                c(0, 35), palette = c("#b44582", "#cc79a7"),
                                xlab = "", ylab = expression(bold(EVP[N4] * "(%)")))
evp_n4_bxp_p <- calc_add_p(vdp_2visits_N4, "VISIT", "EVP", evp_n4_bxp_nop,
                           30, tst = "wilcox", paird = TRUE, addp_eq=TRUE)

hvp_n4_bxp_nop <- connected_bxp(vdp_2visits_N4, "VISIT", "HVP", id = "Subject_id",
                                c(0, 35), palette = c("#b44582", "#cc79a7"),
                                xlab = "", ylab = expression(bold(HVP[N4] * "(%)")))
hvp_n4_bxp_p <- calc_add_p(vdp_2visits_N4, "VISIT", "HVP", hvp_n4_bxp_nop,
                           30, tst = "wilcox", paird = TRUE, addp_eq=TRUE)

# Save the plot as a png file in the specified directory
ggsave("./zR_plots_4ppr/figs_multi_visit/vdp_N4_2visits_cbxp_nop.png", plot = vdp_n4_bxp_nop, width = 4.5, height = 3.8, dpi = 300)
ggsave("./zR_plots_4ppr/figs_multi_visit/vdp_N4_2visits_cbxp_p.png", plot = vdp_n4_bxp_p, width = 4.5, height = 3.8, dpi = 300)

ggsave("./zR_plots_4ppr/figs_multi_visit/lvp_N4_2visits_cbxp_nop.png", plot = lvp_n4_bxp_nop, width = 4.5, height = 3.8, dpi = 300)
ggsave("./zR_plots_4ppr/figs_multi_visit/lvp_N4_2visits_cbxp_p.png", plot = lvp_n4_bxp_p, width = 4.5, height = 3.8, dpi = 300)

ggsave("./zR_plots_4ppr/figs_multi_visit/nvp_N4_2visits_cbxp_nop.png", plot = nvp_n4_bxp_nop, width = 4.5, height = 3.8, dpi = 300)
ggsave("./zR_plots_4ppr/figs_multi_visit/nvp_N4_2visits_cbxp_p.png", plot = nvp_n4_bxp_p, width = 4.5, height = 3.8, dpi = 300)

ggsave("./zR_plots_4ppr/figs_multi_visit/evp_N4_2visits_cbxp_nop.png", plot = evp_n4_bxp_nop, width = 4.5, height = 3.8, dpi = 300)
ggsave("./zR_plots_4ppr/figs_multi_visit/evp_N4_2visits_cbxp_p.png", plot = evp_n4_bxp_p, width = 4.5, height = 3.8, dpi = 300)

ggsave("./zR_plots_4ppr/figs_multi_visit/hvp_N4_2visits_cbxp_nop.png", plot = hvp_n4_bxp_nop, width = 4.5, height = 3.8, dpi = 300)
ggsave("./zR_plots_4ppr/figs_multi_visit/hvp_N4_2visits_cbxp_p.png", plot = hvp_n4_bxp_p, width = 4.5, height = 3.8, dpi = 300)

############# FA-corrected #################
vdp_fa_bxp_nop <- connected_bxp(vdp_2visits_FA, "VISIT", "VDP", id = "Subject_id",
                                c(0, 35), palette = c("#004166", "#0072b2"),
                                xlab = "", ylab = expression(bold(VDP[FA] * "(%)")))
vdp_fa_bxp_p <- calc_add_p(vdp_2visits_FA, "VISIT", "VDP", vdp_fa_bxp_nop,
                           30, tst = "wilcox", paird = TRUE, addp_eq=TRUE)

lvp_fa_bxp_nop <- connected_bxp(vdp_2visits_FA, "VISIT", "LVP", id = "Subject_id",
                                c(0, 35), palette = c("#004166", "#0072b2"),
                                xlab = "", ylab = expression(bold(LVP[FA] * "(%)")))
lvp_fa_bxp_p <- calc_add_p(vdp_2visits_FA, "VISIT", "LVP", lvp_fa_bxp_nop,
                           30, tst = "wilcox", paird = TRUE, addp_eq=TRUE)

nvp_fa_bxp_nop <- connected_bxp(vdp_2visits_FA, "VISIT", "NV", id = "Subject_id",
                                c(0, 99), palette = c("#004166", "#0072b2"),
                                xlab = "", ylab = expression(bold(NVP[FA] * "(%)")))
nvp_fa_bxp_p <- calc_add_p(vdp_2visits_FA, "VISIT", "NV", nvp_fa_bxp_nop,
                           91, tst = "wilcox", paird = TRUE, addp_eq=TRUE)

evp_fa_bxp_nop <- connected_bxp(vdp_2visits_FA, "VISIT", "EVP", id = "Subject_id",
                                c(0, 35), palette = c("#004166", "#0072b2"),
                                xlab = "", ylab = expression(bold(EVP[FA] * "(%)")))
evp_fa_bxp_p <- calc_add_p(vdp_2visits_FA, "VISIT", "EVP", evp_fa_bxp_nop,
                           30, tst = "wilcox", paird = TRUE, addp_eq=TRUE)

hvp_fa_bxp_nop <- connected_bxp(vdp_2visits_FA, "VISIT", "HVP", id = "Subject_id",
                                c(0, 35), palette = c("#004166", "#0072b2"),
                                xlab = "", ylab = expression(bold(HVP[FA] * "(%)")))
hvp_fa_bxp_p <- calc_add_p(vdp_2visits_FA, "VISIT", "HVP", hvp_fa_bxp_nop,
                           30, tst = "wilcox", paird = TRUE, addp_eq=TRUE)

# Save the plot as a png file in the specified directory
ggsave("./zR_plots_4ppr/figs_multi_visit/vdp_FA_2visits_cbxp_nop.png", plot = vdp_fa_bxp_nop, width = 4.5, height = 3.8, dpi = 300)
ggsave("./zR_plots_4ppr/figs_multi_visit/vdp_FA_2visits_cbxp_p.png", plot = vdp_fa_bxp_p, width = 4.5, height = 3.8, dpi = 300)

ggsave("./zR_plots_4ppr/figs_multi_visit/lvp_FA_2visits_cbxp_nop.png", plot = lvp_fa_bxp_nop, width = 4.5, height = 3.8, dpi = 300)
ggsave("./zR_plots_4ppr/figs_multi_visit/lvp_FA_2visits_cbxp_p.png", plot = lvp_fa_bxp_p, width = 4.5, height = 3.8, dpi = 300)

ggsave("./zR_plots_4ppr/figs_multi_visit/nvp_FA_2visits_cbxp_nop.png", plot = nvp_fa_bxp_nop, width = 4.5, height = 3.8, dpi = 300)
ggsave("./zR_plots_4ppr/figs_multi_visit/nvp_FA_2visits_cbxp_p.png", plot = nvp_fa_bxp_p, width = 4.5, height = 3.8, dpi = 300)

ggsave("./zR_plots_4ppr/figs_multi_visit/evp_FA_2visits_cbxp_nop.png", plot = evp_fa_bxp_nop, width = 4.5, height = 3.8, dpi = 300)
ggsave("./zR_plots_4ppr/figs_multi_visit/evp_FA_2visits_cbxp_p.png", plot = evp_fa_bxp_p, width = 4.5, height = 3.8, dpi = 300)

ggsave("./zR_plots_4ppr/figs_multi_visit/hvp_FA_2visits_cbxp_nop.png", plot = hvp_fa_bxp_nop, width = 4.5, height = 3.8, dpi = 300)
ggsave("./zR_plots_4ppr/figs_multi_visit/hvp_FA_2visits_cbxp_p.png", plot = hvp_fa_bxp_p, width = 4.5, height = 3.8, dpi = 300)
