################################################################################
# Title:    Meta-analyses moderator project - stratified analyses (for plotting)
# Author:   Jana Hermans
# Date:     Created in February 2025
################################################################################
library(metafor)
library(meta)
library(grid)
library(ggplot2)
library(tidyverse)
library(gt)
library(patchwork)
library(dplyr)
library(ggpubr)

projectpath <- dirname(file.choose())
setwd(file.path(projectpath,"output")) 
results_path <- paste0(projectpath,"/results/10112025_results")
outdir <- paste0(projectpath,"/results/plots")
filename_ALSPAC <- "res_mod_stratified_imputed_ALSPAC2025-11-10.RData"
filename_GenR <- "res_mod_stratified_imputed_GenR2025-11-07.RData"
ALSPAC_file <- readRDS(file.path(results_path,filename_ALSPAC))
GenR_file <- readRDS(file.path(results_path,filename_GenR))
#---------------------------------Meta-analysis---------------------------------
out_list <- c('exter_z', 'inter_z', 'ADHD_z')
mod_list <- c('p18w','p3y','friendship','preLE','preCR','preIR','posLE','posCR',
              'posIR','posDV','alcohol', 'smoking',  'mat_diet', 'child_diet')
results_list <- list()
dataframes <- list()

int_results <- data.frame() 
ext_results <- data.frame() 
adhd_results <- data.frame() 

for (mod in mod_list) {
  for (outcome in out_list) {
    # Exposed
    ALSPAC_exposed <- ALSPAC_file[[mod]]$exposed
    colnames(ALSPAC_exposed)[colnames(ALSPAC_exposed) == 'estimate'] <- 'preg_dep_betas'
    colnames(ALSPAC_exposed)[colnames(ALSPAC_exposed) == 'std.error'] <- 'ses'
    ALSPAC_exposed$cohort <- 'ALSPAC'
    ALSPAC_exposed$exposure <- 'exposed'
    ALSPAC_exposed <- ALSPAC_exposed[ALSPAC_exposed$outcome == outcome,]
    GenR_exposed <- GenR_file[[mod]]$exposed
    colnames(GenR_exposed)[colnames(GenR_exposed) == 'estimate'] <- 'preg_dep_betas'
    colnames(GenR_exposed)[colnames(GenR_exposed) == 'std.error'] <- 'ses'
    GenR_exposed$cohort <- 'GenR'
    GenR_exposed$exposure <- 'exposed'
    GenR_exposed <- GenR_exposed[GenR_exposed$outcome == outcome,]
    EXPOSED_results <- rbind(ALSPAC_exposed,GenR_exposed)
    m.model2 <- rma(yi = EXPOSED_results$preg_dep_betas, sei = EXPOSED_results$ses)
    for (i in 1:nrow(EXPOSED_results)) {
      EXPOSED_results$conf.low[i] <- EXPOSED_results$preg_dep_betas[i]-1.96*EXPOSED_results$ses[i]
      EXPOSED_results$conf.high[i] <- EXPOSED_results$preg_dep_betas[i]+1.96*EXPOSED_results$ses[i]
    }
    colnames(EXPOSED_results)[colnames(EXPOSED_results) == 'preg_dep_betas'] <- 'estimate'
    EXPOSED_results <- bind_rows(EXPOSED_results,
                                 data.frame(
                                   cohort = "TOTAL",
                                   term = mod,
                                   outcome = unique(EXPOSED_results$outcome),
                                   exposure = "exposed",
                                   estimate = m.model2$beta,
                                   conf.low = m.model2$ci.lb,
                                   conf.high = m.model2$ci.ub
                                 )
    ) 
    EXPOSED_results$estimate <- as.numeric(EXPOSED_results$estimate)
    EXPOSED_results$conf.low <- as.numeric(EXPOSED_results$conf.low)
    EXPOSED_results$conf.high <- as.numeric(EXPOSED_results$conf.high)
    EXPOSED_results <- EXPOSED_results[,c("cohort","term","exposure","estimate",
                                          "outcome","conf.low","conf.high")]
    row.names(EXPOSED_results) <- NULL
    
    # Unexposed
    ALSPAC_unexposed <- ALSPAC_file[[mod]]$unexposed
    colnames(ALSPAC_unexposed)[colnames(ALSPAC_unexposed) == 'estimate'] <- 'preg_dep_betas'
    colnames(ALSPAC_unexposed)[colnames(ALSPAC_unexposed) == 'std.error'] <- 'ses'
    ALSPAC_unexposed$cohort <- 'ALSPAC'
    ALSPAC_unexposed$exposure <- 'unexposed'
    ALSPAC_unexposed <- ALSPAC_unexposed[ALSPAC_unexposed$outcome == outcome,]
    GenR_unexposed <- GenR_file[[mod]]$unexposed
    colnames(GenR_unexposed)[colnames(GenR_unexposed) == 'estimate'] <- 'preg_dep_betas'
    colnames(GenR_unexposed)[colnames(GenR_unexposed) == 'std.error'] <- 'ses'
    GenR_unexposed$cohort <- 'GenR'
    GenR_unexposed$exposure <- 'unexposed'
    GenR_unexposed <- GenR_unexposed[GenR_unexposed$outcome == outcome,]
    UNEXPOSED_results <- rbind(ALSPAC_unexposed,GenR_unexposed)
    m.model2 <- rma(yi = UNEXPOSED_results$preg_dep_betas, sei = UNEXPOSED_results$ses)
    for (i in 1:nrow(UNEXPOSED_results)) {
      UNEXPOSED_results$conf.low[i] <- UNEXPOSED_results$preg_dep_betas[i]-1.96*UNEXPOSED_results$ses[i]
      UNEXPOSED_results$conf.high[i] <- UNEXPOSED_results$preg_dep_betas[i]+1.96*UNEXPOSED_results$ses[i]
    }
    colnames(UNEXPOSED_results)[colnames(UNEXPOSED_results) == 'preg_dep_betas'] <- 'estimate'
    UNEXPOSED_results <- bind_rows(UNEXPOSED_results,
                                   data.frame(
                                     cohort = "TOTAL",
                                     term = mod,
                                     outcome = unique(UNEXPOSED_results$outcome),
                                     exposure = "unexposed",
                                     estimate = m.model2$beta,
                                     conf.low = m.model2$ci.lb,
                                     conf.high = m.model2$ci.ub
                                   )
    ) 
    UNEXPOSED_results$estimate <- as.numeric(UNEXPOSED_results$estimate)
    UNEXPOSED_results$conf.low <- as.numeric(UNEXPOSED_results$conf.low)
    UNEXPOSED_results$conf.high <- as.numeric(UNEXPOSED_results$conf.high)
    UNEXPOSED_results <- UNEXPOSED_results[,c("cohort","term","exposure","estimate",
                                              "outcome","conf.low","conf.high")]
    row.names(UNEXPOSED_results) <- NULL
    
    if (outcome == 'inter_z') {
      TOTAL_results <- rbind(EXPOSED_results[EXPOSED_results$cohort == "TOTAL",],
                             UNEXPOSED_results[UNEXPOSED_results$cohort == "TOTAL",])
      int_results <- rbind(int_results, TOTAL_results)
    }
    if (outcome == 'exter_z') {
      TOTAL_results <- rbind(EXPOSED_results[EXPOSED_results$cohort == "TOTAL",],
                             UNEXPOSED_results[UNEXPOSED_results$cohort == "TOTAL",])
      ext_results <- rbind(ext_results, TOTAL_results)
    }
    if (outcome == 'ADHD_z') {
      TOTAL_results <- rbind(EXPOSED_results[EXPOSED_results$cohort == "TOTAL",],
                             UNEXPOSED_results[UNEXPOSED_results$cohort == "TOTAL",])
      adhd_results <- rbind(adhd_results, TOTAL_results)
    }
  }
  dataframes[[mod]] <- results_list
}

row.names(int_results) <- row.names(ext_results) <- row.names(adhd_results) <- NULL

#-------------------------------------PLOT--------------------------------------
# Define your labels
labels_df <- data.frame(
  term = unique(int_results$term), 
  label = c('Stratified by pren. partner depression',
            'Stratified by post. partner depression','Stratified by friendship',
            'Stratified by pren. Life Events','Stratified by pren. Contextual Risk',
            'Stratified by pren. Interpersonal Risk','Stratified by post. Life Events',
            'Stratified by post. Contextual Risk',
            'Stratified by post. Interpersonal Risk',
            'Stratified by post. Direct Victimization',
            'Stratified by pren. maternal drinking', 
            'Stratified by pren. maternal smoking','Stratified by maternal Med. diet',
            'Stratified by child Med. diet')
)
# Set order
int_results$term <- factor(int_results$term, 
                           levels = c("p18w","p3y","friendship","preLE","preCR",
                                      "preIR","posLE","posCR","posIR","posDV",
                                      "alcohol","smoking","mat_diet","child_diet")) 
int_results$term <- forcats::fct_rev(int_results$term)
ext_results$term <- factor(ext_results$term, 
                           levels = c("p18w","p3y","friendship","preLE","preCR",
                                      "preIR","posLE","posCR","posIR","posDV",
                                      "alcohol","smoking","mat_diet","child_diet"))  
ext_results$term <- forcats::fct_rev(ext_results$term)
adhd_results$term <- factor(adhd_results$term, 
                           levels = c("p18w","p3y","friendship","preLE","preCR",
                                      "preIR","posLE","posCR","posIR","posDV",
                                      "alcohol","smoking","mat_diet","child_diet")) 
adhd_results$term <- forcats::fct_rev(adhd_results$term)

# Combine all estimates and confidence intervals
all_vals <- c(
  int_results$conf.low, int_results$conf.high,
  ext_results$conf.low, ext_results$conf.high,
  adhd_results$conf.low, adhd_results$conf.high
)

# Get the overall min and max
y_min <- floor(min(all_vals, na.rm = TRUE) * 10) / 10  # rounded down to nearest 0.1
y_max <- ceiling(max(all_vals, na.rm = TRUE) * 10) / 10  # rounded up to nearest 0.1

INT_plot <- ggplot(int_results, aes(y = term, x = estimate, color = exposure)) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "#D4D4D4") +
  geom_point(position = position_dodge(width = 0.5), size = 3) +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high),
                 height = 0.2,
                 position = position_dodge(width = 0.5)) +
  scale_color_manual(values = c("exposed" = "#0D5C52", "unexposed" = "#B0D4D0")) +
  scale_y_discrete(labels = setNames(labels_df$label, labels_df$term)) +
  scale_x_continuous(
    name = "Beta Estimate (95% CI)",
    breaks = seq(-0.1, 1.0, by = 0.1)  # or whatever range fits your data
  ) +
  labs(
    title = "Internalising symptoms (z-scores)",
    y = "",
    color = "Moderator exposure group"
  ) +
  coord_cartesian(xlim = c(y_min, y_max)) +
  theme_minimal() +
  theme(
    axis.text.y = element_text(size = 11),
    axis.text.x = element_text(size = 10),
    plot.title = element_text(hjust = 0.5, size = 14),
    axis.line.x = element_line(color = "#D4D4D4", linewidth = 0.5),
    axis.ticks.x = element_line(color = "#D4D4D4", linewidth = 0.5),
    axis.ticks.length.x = unit(3, "pt"),
    axis.line.y = element_line(color = "#D4D4D4", linewidth = 0.5),
    axis.ticks.y = element_line(color = "#D4D4D4", linewidth = 0.5),
    axis.ticks.length.y = unit(3, "pt"),
    axis.title.x = element_text(margin = margin(t = 10)),
    plot.margin = margin(t = 10, r = 20, b = 20, l = 20),
    panel.grid = element_blank(),
    legend.position = "bottom",
    legend.justification = "center",
    legend.background = element_rect(fill = "white", color = NA),
    legend.text = element_text(size = 14),
    legend.title = element_text(size = 14),
    plot.background = element_rect(fill = "white", color = NA)
  )

EXT_plot <- ggplot(ext_results, aes(y = term, x = estimate, color = exposure)) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "#D4D4D4") +
  geom_point(position = position_dodge(width = 0.5), size = 3) +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high),
                 height = 0.2,
                 position = position_dodge(width = 0.5)) +
  scale_color_manual(values = c("exposed" = "#0D5C52", "unexposed" = "#B0D4D0")) +
  scale_y_discrete(labels = setNames(labels_df$label, labels_df$term)) +
  scale_x_continuous(
    name = "Beta Estimate (95% CI)",
    breaks = seq(-0.1, 1.0, by = 0.1)  # or whatever range fits your data
  ) +
  labs(
    title = "Externalising symptoms (z-scores)",
    y = "",
    color = "Moderator exposure group"
  ) +
  coord_cartesian(xlim = c(y_min, y_max)) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(size = 10),
    plot.title = element_text(hjust = 0.5, size = 14),
    axis.line.x = element_line(color = "#D4D4D4", linewidth = 0.5),
    axis.ticks.x = element_line(color = "#D4D4D4", linewidth = 0.5),
    axis.ticks.length.x = unit(3, "pt"),
    axis.line.y = element_line(color = "#D4D4D4", linewidth = 0.5),
    axis.ticks.y = element_line(color = "#D4D4D4", linewidth = 0.5),
    axis.ticks.length.y = unit(3, "pt"),
    axis.title.x = element_text(margin = margin(t = 10)),
    plot.margin = margin(t = 10, r = 20, b = 20, l = 20),
    axis.text.y = element_blank(),
    panel.grid = element_blank(),
    legend.position = "bottom",
    legend.justification = "center",
    legend.background = element_rect(fill = "white", color = NA),
    legend.text = element_text(size = 14),
    legend.title = element_text(size = 14),
    plot.background = element_rect(fill = "white", color = NA)
  )

ADHD_plot <- ggplot(adhd_results, aes(y = term, x = estimate, color = exposure)) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "#D4D4D4") +
  geom_point(position = position_dodge(width = 0.5), size = 3) +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high),
                 height = 0.2,
                 position = position_dodge(width = 0.5)) +
  scale_color_manual(values = c("exposed" = "#0D5C52", "unexposed" = "#B0D4D0")) +
  scale_y_discrete(labels = setNames(labels_df$label, labels_df$term)) +
  scale_x_continuous(
    name = "Beta Estimate (95% CI)",
    breaks = seq(-0.1, 1.0, by = 0.1)  # or whatever range fits your data
  ) +
  labs(
    title = "ADHD symptoms (z-scores)",
    y = "",
    color = "Moderator exposure group"
  ) +
  coord_cartesian(xlim = c(y_min, y_max)) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(size = 10),
    plot.title = element_text(hjust = 0.5, size = 14),
    axis.line.x = element_line(color = "#D4D4D4", linewidth = 0.5),
    axis.ticks.x = element_line(color = "#D4D4D4", linewidth = 0.5),
    axis.ticks.length.x = unit(3, "pt"),
    axis.line.y = element_line(color = "#D4D4D4", linewidth = 0.5),
    axis.ticks.y = element_line(color = "#D4D4D4", linewidth = 0.5),
    axis.ticks.length.y = unit(3, "pt"),
    axis.title.x = element_text(margin = margin(t = 10)),
    plot.margin = margin(t = 10, r = 20, b = 20, l = 20),
    axis.text.y = element_blank(),
    panel.grid = element_blank(),
    legend.position = "bottom",
    legend.justification = "center",
    legend.background = element_rect(fill = "white", color = NA),
    legend.text = element_text(size = 14),
    legend.title = element_text(size = 14),
    plot.background = element_rect(fill = "white", color = NA)
  )


FULL_PLOT <- ggarrange(INT_plot, EXT_plot, ADHD_plot, ncol = 3, nrow = 1,
                       widths = c(1.7, 1, 1),
                       common.legend = TRUE, legend="bottom") 
annotate_figure(
  FULL_PLOT,
  top = text_grob("Associations prenatal maternal depression and offspring outcomes by moderator exposure\n", 
                  face = "bold", size = 18))

ggsave("Figure3_mod_stratified_NEW.tiff", units="in", path = outdir, width = 14, height = 9, device='tiff', dpi=700)

dev.off()

