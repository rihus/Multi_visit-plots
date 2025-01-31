#load ggplot2
library(ggplot2)
library(ggpubr)
library(blandr)
library(dplyr)
library(tidyr)
library(rstatix)

# #Set the working directory to the path where your CSV files are located
setwd("C:/Users/HUSDQ4/OneDrive - cchmc/cincy_work/Confs_Courses_talks_pprs")

###########Regression line plot function
linreg_plt <- function(df_in, x_in, y_in, y_label, x_label, x_lim = c(0, 40),
                       y_lim = c(50, 150), corrltn_method = "pearson") {
  # Check if correlation method is valid
  if (!corrltn_method %in% c("pearson", "spearman")) {
    stop("Only pearson or spearman correlations accepted.") }
  # Create formula for linear regression
  yx_formula <- as.formula(paste(y_in, "~", x_in))
  fitt <- lm(yx_formula, data = df_in)
  slope <- coef(fitt)[2]
  intercept <- coef(fitt)[1]
  print(fitt)
  # Base scatter plot with regression line
  linreg <- ggpubr::ggscatter(df_in, x = x_in, y = y_in,
                              add = "reg.line",  # Add regression line
                              xlim = x_lim, ylim = y_lim,
                              add.params = list(color = "#000000", size = 2, fill = "red", alpha = 0.15),
                              conf.int = TRUE, fullrange = TRUE,
                              cor.method = corrltn_method,
                              color = "#000000", shape = 19, size = 4, # Points color, shape and size
                              ggtheme = theme_bw(),
                              font.x = c(22, "bold", "#000000"),
                              font.y = c(22, "bold", "#000000"),
                              font.tickslab = c(22, "bold", "#000000")) +
    xlab(x_label) + ylab(y_label) +
    geom_abline(intercept = intercept, slope = slope,
                linetype = "dotted", linewidth=2, color = "#000000") +
    theme(legend.position = "none")
  print(linreg)
  # Add correlation coefficient and p-value
  if (corrltn_method == "pearson") {coeff_names <- c("r", "P")}
  else if (corrltn_method == "spearman") {coeff_names <- c("rho", "P")}
  # Linear regression equation
  fit_eq <- parse(text = sprintf("y == %.2f * x + %.2f", slope, intercept))
  # Add correlation coefficient and equation to the plot
  linreg_txt <- linreg + ggpubr::stat_cor(p.accuracy = 0.001, method = corrltn_method,
                     aes(label = paste(after_stat(rr.label), after_stat(p.label), sep = "~`,`~")),
                     label.x = 0.3*max(df_in[[x_in]]), label.y = 0.98*max(df_in[[y_in]]), size = 6) +
    annotate("text", x = 0.65*max(df_in[[x_in]]), y = 0.88*max(df_in[[y_in]]),
             label = fit_eq, color = "#000000", size = 6, parse = TRUE)
  print(linreg_txt)
  ##return handles to both plots
  return(list(linreg, linreg_txt))}

################################################################################
# ##Load the data from both CSV files and arrange it to plot
new_df <- read.csv("./Riaz_CF_multi_visit_ppr/IRC740H_N4VDP_PFTs_2visits.csv")

base_vdp_fev1 <- linreg_plt(new_df, "VDP_B", "FEV1_B", "FEV1 (%)", "VDP (%)",
                       x_lim=c(0, 30), y_lim=c(50, 140), corrltn_method="pearson")
base_vdp_fvc <- linreg_plt(new_df, "VDP_B", "FVC_B", "FVC (%)", "VDP (%)",
                       x_lim=c(0, 30), y_lim=c(50, 140), corrltn_method="pearson")
base_vdp_fev1ofvc <- linreg_plt(new_df, "VDP_B", "FEV1oFVC_B", "FEV1/FVC (%)", "VDP (%)",
                           x_lim=c(0, 30), y_lim=c(60, 100), corrltn_method="pearson")


yr1_vdp_fev1 <- linreg_plt(new_df, "VDP_Y1", "FEV1_Y1", "FEV1 (%)", "VDP (%)",
                            x_lim=c(0, 30), y_lim=c(50, 140), corrltn_method="pearson")
yr1_vdp_fvc <- linreg_plt(new_df, "VDP_Y1", "FVC_Y1", "FVC (%)", "VDP (%)",
                           x_lim=c(0, 30), y_lim=c(50, 140), corrltn_method="pearson")
yr1_vdp_fev1ofvc <- linreg_plt(new_df, "VDP_Y1", "FEV1oFVC_Y1", "FEV1/FVC (%)", "VDP (%)",
                                x_lim=c(0, 30), y_lim=c(60, 100), corrltn_method="pearson")


diff_vdp_fev1 <- linreg_plt(new_df, "VDP_Diff", "FEV1_Diff", "FEV1 (%)", "VDP (%)",
                           x_lim=c(-10, 20), y_lim=c(-40, 30), corrltn_method="pearson")
diff_vdp_fvc <- linreg_plt(new_df, "VDP_Diff", "FVC_Diff", "FVC (%)", "VDP (%)",
                          x_lim=c(-10, 20), y_lim=c(-40, 30), corrltn_method="pearson")
diff_vdp_fev1ofvc <- linreg_plt(new_df, "VDP_Diff", "FEV1oFVC_Diff", "FEV1/FVC (%)", "VDP (%)",
                               x_lim=c(-10, 20), y_lim=c(-25, 25), corrltn_method="pearson")


# Save the plot as a png file in the specified directory
ggsave("./zR_plots_4ppr/base_N4vdp_fev1_linreg_plain.png", plot = base_vdp_fev1[[1]], width = 4.5, height = 3.7, dpi = 300)
ggsave("./zR_plots_4ppr/base_N4vdp_fev1_linreg_p.png", plot = base_vdp_fev1[[2]], width = 4.5, height = 3.7, dpi = 300)

