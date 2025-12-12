################################################################################
# Title:    Meta-analyses moderator project - direct associations
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
library(flextable)
library(officer)

projectpath <- dirname(file.choose())
setwd(file.path(projectpath,"output")) 
results_path <- paste0(projectpath,"/results/10112025_results")
outdir <- paste0(projectpath,"/results/plots")
filename_ALSPAC <- "res_direct_imputed_ALSPAC2025-11-10.RData"
filename_GenR <- "res_direct_imputed_GenR2025-11-07.RData"
ALSPAC_file <- readRDS(file.path(results_path,filename_ALSPAC))
GenR_file <- readRDS(file.path(results_path,filename_GenR))
#-----------------------------------Data prep-----------------------------------
out_list <- c('exter_z', 'inter_z', 'ADHD_z')
mod_list <- c('preg_dep','p18w','p3y','friendship','preLE','preCR','preIR',
              'posLE','posCR','posIR','posDV','alcohol','smoking','mat_diet',
              'child_diet')
results_list <- list()
dataframes <- list()

int_results <- data.frame() 
ext_results <- data.frame() 
adhd_results <- data.frame() 

for (mod in mod_list) {
  for (outcome in out_list) {
    ALSPAC_results <- ALSPAC_file[[mod]]
    colnames(ALSPAC_results)[colnames(ALSPAC_results) == 'estimate'] <- 'preg_dep_betas'
    colnames(ALSPAC_results)[colnames(ALSPAC_results) == 'std.error'] <- 'ses'
    ALSPAC_results$cohort <- 'ALSPAC'
    ALSPAC_results <- ALSPAC_results[ALSPAC_results$outcome == outcome,]
    GenR_results <- GenR_file[[mod]]
    colnames(GenR_results)[colnames(GenR_results) == 'estimate'] <- 'preg_dep_betas'
    colnames(GenR_results)[colnames(GenR_results) == 'std.error'] <- 'ses'
    GenR_results$cohort <- 'GenR'
    GenR_results <- GenR_results[GenR_results$outcome == outcome,]
    TOTAL_results <- rbind(ALSPAC_results,GenR_results)
    m.model2 <- rma(yi = TOTAL_results$preg_dep_betas, sei = TOTAL_results$ses)
    for (i in 1:nrow(TOTAL_results)) {
      TOTAL_results$conf.low[i] <- TOTAL_results$preg_dep_betas[i]-1.96*TOTAL_results$ses[i]
      TOTAL_results$conf.high[i] <- TOTAL_results$preg_dep_betas[i]+1.96*TOTAL_results$ses[i]
    }
    colnames(TOTAL_results)[colnames(TOTAL_results) == 'preg_dep_betas'] <- 'estimate'
    TOTAL_results <- bind_rows(TOTAL_results,
                                 data.frame(
                                   cohort = "TOTAL",
                                   term = mod,
                                   outcome = unique(TOTAL_results$outcome),
                                   estimate = m.model2$beta,
                                   conf.low = m.model2$ci.lb,
                                   conf.high = m.model2$ci.ub,
                                   p.value = m.model2$pval
                                 )
    ) 
    TOTAL_results$estimate <- as.numeric(TOTAL_results$estimate)
    TOTAL_results$conf.low <- as.numeric(TOTAL_results$conf.low)
    TOTAL_results$conf.high <- as.numeric(TOTAL_results$conf.high)
    TOTAL_results <- TOTAL_results[,c("cohort","term","estimate","outcome",
                                      "conf.low","conf.high","p.value")]
    row.names(TOTAL_results) <- NULL
  
    if (outcome == 'inter_z') {
      results_list$int_ <- TOTAL_results
      TOTAL_results <- TOTAL_results[TOTAL_results$cohort == "TOTAL",]
      int_results <- rbind(int_results, TOTAL_results)
    }
    if (outcome == 'exter_z') {
      results_list$ext_ <- TOTAL_results
      TOTAL_results <- TOTAL_results[TOTAL_results$cohort == "TOTAL",]
      ext_results <- rbind(ext_results, TOTAL_results)
    }
    if (outcome == 'ADHD_z') {
      results_list$adhd_ <- TOTAL_results
      TOTAL_results <- TOTAL_results[TOTAL_results$cohort == "TOTAL",]
      adhd_results <- rbind(adhd_results, TOTAL_results)
    }
  }
  dataframes[[mod]] <- results_list
}

row.names(int_results) <- row.names(ext_results) <- row.names(adhd_results) <- NULL

# Apply multiple testing correction
combined_total <- rbind(int_results, ext_results, adhd_results)
combined_total$p.value_adj <- p.adjust(combined_total$p.value, method = "BH")
int_results <- combined_total[combined_total$outcome == "inter_z",]
ext_results <- combined_total[combined_total$outcome == "exter_z",]
adhd_results <- combined_total[combined_total$outcome == "ADHD_z",]

# Number of eff. tests is 28 (Galwey method):
#meff_df <- 28
#int_results$p.value_adj <- 
#  ifelse(as.numeric(int_results$p.value)*meff_df>1,1,
#         as.numeric(int_results$p.value)*meff_df)
#ext_results$p.value_adj <- 
#  ifelse(as.numeric(ext_results$p.value)*meff_df>1,1,
#         as.numeric(ext_results$p.value)*meff_df)
#adhd_results$p.value_adj <- 
#  ifelse(as.numeric(adhd_results$p.value)*meff_df>1,1,
#         as.numeric(adhd_results$p.value)*meff_df)

# PLOT
# Set order
int_results$term <- factor(int_results$term, 
                           levels = c("preg_dep","p18w","p3y","friendship","preLE","preCR",
                                      "preIR","posLE","posCR","posIR","posDV",
                                      "alcohol","smoking","mat_diet","child_diet"))  
ext_results$term <- factor(ext_results$term, 
                           levels = c("preg_dep","p18w","p3y","friendship","preLE","preCR",
                                      "preIR","posLE","posCR","posIR","posDV",
                                      "alcohol","smoking","mat_diet","child_diet")) 
adhd_results$term <- factor(adhd_results$term, 
                            levels = c("preg_dep","p18w","p3y","friendship","preLE","preCR",
                                       "preIR","posLE","posCR","posIR","posDV",
                                       "alcohol","smoking","mat_diet","child_diet")) 

# Define your labels
labels_df <- data.frame(
  term = unique(int_results$term), 
  label = c('Pren. maternal depression', 'Pren. partner depression', 
            'Post. partner depression', 'Friendship', 'Pren. Life Events', 
            'Pren. Contextual Risk', 'Pren. Interpersonal Risk', 
            'Post. Life Events', 'Post. Contextual Risk', 'Post. Interpersonal Risk', 
            'Post. Direct Victimization', 'Pren. maternal drinking', 
            'Pren. maternal smoking', 'Maternal Med. diet', 'Child Med. diet'
            )
)

# Combine all estimates and confidence intervals
all_vals <- c(
  int_results$conf.low, int_results$conf.high,
  ext_results$conf.low, ext_results$conf.high,
  adhd_results$conf.low, adhd_results$conf.high
)

# Get the overall min and max
y_min <- floor(min(all_vals, na.rm = TRUE) * 10) / 10  # rounded down to nearest 0.1
y_max <- ceiling(max(all_vals, na.rm = TRUE) * 10) / 10  # rounded up to nearest 0.1

# Indicate significance
int_results$sign <- int_results$p.value < 0.05 
int_results$sign_adj <- int_results$p.value_adj < 0.05 
ext_results$sign <- ext_results$p.value < 0.05 
ext_results$sign_adj <- ext_results$p.value_adj < 0.05 
adhd_results$sign <- adhd_results$p.value < 0.05 
adhd_results$sign_adj <- adhd_results$p.value_adj < 0.05 

INT_plot <- ggplot(int_results, aes(x = term, y = estimate)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "#D4D4D4") +
  geom_point(position = position_dodge(width = 0.5), size = 3, color = "#5F9891") +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high),
                width = 0.2, color = "#5F9891",
                position = position_dodge(width = 0.5)) +
  geom_text(
    data = subset(int_results, sign),
    aes(label = "*", y = conf.high + 0.02), 
    vjust = 0,
    size = 6,
    color = "grey"
  ) +
  geom_text(
    data = subset(int_results, sign_adj),
    aes(label = "*", y = conf.high + 0.02), 
    vjust = 0,
    size = 6,
    color = "black"
  ) +
  scale_x_discrete(labels = setNames(labels_df$label, labels_df$term)) +  
  scale_y_continuous(
    name = "Beta Estimate (95% CI)",
    breaks = seq(-0.1, 1.0, by = 0.1)  
  ) +
  labs(
    title = "Associations with internalising symptoms (z-scores)",
    x = ""
  ) +
  coord_cartesian(ylim = c(y_min, y_max)) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
    plot.title = element_text(hjust = 0.5, face = "bold", size = 16),  
    panel.grid = element_blank(),
    legend.position = "inside",
    legend.position.inside = c(1, 1),  
    legend.justification = c("right", "top"),
    legend.background = element_rect(fill = "white", color = NA),
    legend.text = element_text(size = 9),
    legend.title = element_text(size = 10),
    
    axis.line.y = element_line(color = "#D4D4D4", linewidth = 0.5),
    axis.ticks.y = element_line(color = "#D4D4D4", linewidth = 0.5),
    axis.ticks.length.y = unit(3, "pt"),
    axis.title.y = element_text(margin = margin(r = 8)),
    plot.margin = margin(t = 10, r = 10, b = 10, l = 20),
    
    plot.background = element_rect(fill = "white", color = NA)
  )


EXT_plot <- ggplot(ext_results, aes(x = term, y = estimate)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "#D4D4D4") +
  geom_point(position = position_dodge(width = 0.5), size = 3, color = "#5F9891") +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high),
                width = 0.2, color = "#5F9891",
                position = position_dodge(width = 0.5)) +
  geom_text(
    data = subset(ext_results, sign),
    aes(label = "*", y = conf.high + 0.02), 
    vjust = 0,
    size = 6,
    color = "grey"
  ) +
  geom_text(
    data = subset(ext_results, sign_adj),
    aes(label = "*", y = conf.high + 0.02), 
    vjust = 0,
    size = 6,
    color = "black"
  ) +
  scale_x_discrete(labels = setNames(labels_df$label, labels_df$term)) +  
  scale_y_continuous(
    name = "Beta Estimate (95% CI)",
    breaks = seq(-0.1, 1.0, by = 0.1)  
  ) +
  labs(
    title = "Associations with externalising symptoms (z-scores)",
    x = ""
  ) +
  coord_cartesian(ylim = c(y_min, y_max)) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
    plot.title = element_text(hjust = 0.5, face = "bold", size = 16),  
    panel.grid = element_blank(),
    legend.position = "inside",
    legend.position.inside = c(1, 1),  
    legend.justification = c("right", "top"),
    legend.background = element_rect(fill = "white", color = NA),
    legend.text = element_text(size = 9),
    legend.title = element_text(size = 10),
    
    axis.line.y = element_line(color = "#D4D4D4", linewidth = 0.5),
    axis.ticks.y = element_line(color = "#D4D4D4", linewidth = 0.5),
    axis.ticks.length.y = unit(3, "pt"),
    axis.title.y = element_text(margin = margin(r = 8)),
    plot.margin = margin(t = 10, r = 10, b = 10, l = 20),
    
    plot.background = element_rect(fill = "white", color = NA)
  )

ADHD_plot <- ggplot(adhd_results, aes(x = term, y = estimate)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "#D4D4D4") +
  geom_point(position = position_dodge(width = 0.5), size = 3, color = "#5F9891") +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high),
                width = 0.2, color = "#5F9891",
                position = position_dodge(width = 0.5)) +
  geom_text(
    data = subset(adhd_results, sign),
    aes(label = "*", y = conf.high + 0.02), 
    vjust = 0,
    size = 6,
    color = "grey"
  ) +
  geom_text(
    data = subset(adhd_results, sign_adj),
    aes(label = "*", y = conf.high + 0.02), 
    vjust = 0,
    size = 6,
    color = "black"
  ) +
  scale_x_discrete(labels = setNames(labels_df$label, labels_df$term)) +  
  scale_y_continuous(
    name = "Beta Estimate (95% CI)",
    breaks = seq(-0.1, 1.0, by = 0.1)  
  ) +
  labs(
    title = "Associations with ADHD symptoms (z-scores)",
    x = ""
  ) +
  coord_cartesian(ylim = c(y_min, y_max)) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
    plot.title = element_text(hjust = 0.5, face = "bold", size = 16),  
    panel.grid = element_blank(),
    legend.position = "inside",
    legend.position.inside = c(1, 1),  
    legend.justification = c("right", "top"),
    legend.background = element_rect(fill = "white", color = NA),
    legend.text = element_text(size = 9),
    legend.title = element_text(size = 10),
    
    axis.line.y = element_line(color = "#D4D4D4", linewidth = 0.5),
    axis.ticks.y = element_line(color = "#D4D4D4", linewidth = 0.5),
    axis.ticks.length.y = unit(3, "pt"),
    axis.title.y = element_text(margin = margin(r = 8)),
    plot.margin = margin(t = 10, r = 10, b = 10, l = 20),
    
    plot.background = element_rect(fill = "white", color = NA)
  )

#INT_plot <- INT_plot +
#  theme(
#    plot.margin = margin(t = 5, r = 5, b = 5, l = 10, unit = "mm")
#  )

#EXT_plot <- EXT_plot +
#  theme(
#    plot.margin = margin(t = 5, r = 5, b = 5, l = 10, unit = "mm")
#  )

#ADHD_plot <- ADHD_plot +
#  theme(
#    plot.margin = margin(t = 5, r = 5, b = 5, l = 10, unit = "mm")
#  )

ggarrange(INT_plot, EXT_plot, ADHD_plot, ncol = 1, nrow = 3)

ggsave("Figure2_direct.tiff", units="in", path = outdir, width = 12, height = 15, device='tiff', dpi=700)

dev.off()

#----------------------------SAVE Supplementary Table---------------------------
# Obtain all total effects for supplementary table
combined_ALSPAC_GenR <- map_dfr(
  names(dataframes),
  function(mod) {
    map_dfr(
      names(dataframes[[mod]]),
      function(outcome) {
        dataframes[[mod]][[outcome]] %>%
          filter(cohort == "ALSPAC"|cohort == "GenR")
      }
    )
  }
)

# Define your labels
labels_df <- data.frame(
  term = unique(combined_ALSPAC_GenR$term), 
  label = c('Pren. maternal depression', 'Pren. partner depression', 
            'Post. partner depression', 'Friendship', 'Pren. Life Events', 
            'Pren. Contextual Risk', 'Pren. Interpersonal Risk', 
            'Post. Life Events', 'Post. Contextual Risk', 'Post. Interpersonal Risk', 
            'Post. Direct Victimization', 'Pren. maternal drinking', 
            'Pren. maternal smoking', 'Maternal Med. diet', 'Child Med. diet'
  )
)

combined_ALSPAC_GenR$term <- gsub('preg_dep_bin1: Depression:','',combined_ALSPAC_GenR$term)

combined_ALSPAC_GenR$moderator <- labels_df$label[match(combined_ALSPAC_GenR$term,
                                                        labels_df$term)]

combined_ALSPAC_GenR <- combined_ALSPAC_GenR[,c("moderator","cohort","estimate","outcome",
                                                "conf.low","conf.high","p.value")]

# Apply multiple testing correction
GenR_table <- combined_ALSPAC_GenR[combined_ALSPAC_GenR$cohort == "GenR",]
GenR_table$p.value_adj <- p.adjust(GenR_table$p.value, method = "BH")

ALSPAC_table <- combined_ALSPAC_GenR[combined_ALSPAC_GenR$cohort == "ALSPAC",]
ALSPAC_table$p.value_adj <- p.adjust(ALSPAC_table$p.value, method = "BH")

combined_ALSPAC_GenR <- rbind(GenR_table, ALSPAC_table)
# Number of eff. tests in GenR is 21 and in ALSPAC 22 (Galwey method):
#meff_df <- 28
#combined_ALSPAC_GenR$p.value_adj <- 
#  ifelse(as.numeric(combined_ALSPAC_GenR$p.value)*meff_df>1,1,
#         as.numeric(combined_ALSPAC_GenR$p.value)*meff_df)

inter_table <- combined_ALSPAC_GenR[combined_ALSPAC_GenR$outcome == "inter_z",]
exter_table <- combined_ALSPAC_GenR[combined_ALSPAC_GenR$outcome == "exter_z",]
ADHD_table <- combined_ALSPAC_GenR[combined_ALSPAC_GenR$outcome == "ADHD_z",]

# Format individual outcome tables
format_result <- function(df, outcome, moderators) {
  df %>%
    filter(moderator %in% moderators) %>%
    mutate(
      β = sprintf("%.2f", estimate),
      `95% CI` = paste0("[", sprintf("%.2f", conf.low), ", ", sprintf("%.2f", conf.high), "]"),
      p = sprintf("%.3f", p.value),
      `p (FDR)` = sprintf("%.3f", p.value_adj),
      outcome = outcome
    ) %>%
    select(Moderator = moderator, Cohort = cohort, β, `95% CI`, p, `p (FDR)`, outcome)
}

# Combine outcomes side by side, keeping cohorts stacked
combine_results_side_by_side <- function(results_list, outcome_names, moderators, moderator_order) {
  dfs <- lapply(outcome_names, function(outcome) {
    format_result(results_list[[outcome]], outcome, moderators)
  })
  
  combined_df <- bind_rows(dfs)
  combined_df$Moderator <- factor(combined_df$Moderator, levels = moderator_order)
  
  combined_df <- combined_df %>%
    arrange(Moderator, Cohort) %>%
    mutate(
      β = as.character(β),
      p = as.character(p),
      `p (FDR)` = as.character(`p (FDR)`)
    ) %>%
    pivot_longer(cols = c("β", "95% CI", "p", "p (FDR)"),
                 names_to = "metric",
                 values_to = "value") %>%
    mutate(colname = paste0(outcome, "_", metric)) %>%
    select(Moderator, Cohort, colname, value) %>%
    pivot_wider(names_from = colname, values_from = value)
  
  return(combined_df)
}

# Create a flextable with top headers and merged moderator cells
create_combined_flextable <- function(combined_df, top_labels) {
  ft <- flextable(combined_df)
  n_outcomes <- length(top_labels)
  
  colwidths <- c(1, 1, rep(4, n_outcomes))  # Moderator + Cohort + 4 metrics per outcome
  ft <- add_header_row(
    ft,
    values = c("Moderator", "Cohort", top_labels),
    colwidths = colwidths
  )
  
  pattern <- c("β", "95% CI", "p", "p (FDR)")
  subheaders <- c("Moderator", "Cohort", rep(pattern, n_outcomes))
  ft <- set_header_labels(ft, values = setNames(subheaders, names(combined_df)))
  
  for (j in seq_along(names(combined_df))) {
    col <- names(combined_df)[j]
    
    if (grepl("^p(_|$)", col)) { 
      # matches columns starting with 'p' (e.g., p_inter_table)
      ft <- compose(ft, j = col, part = "header", value = as_paragraph(as_i("p")))
    } else if (grepl("^p\\s*\\(FDR\\)(_.*|$)", col)) { 
      # matches columns like 'p (FDR)_inter_table'
      ft <- compose(ft, j = col, part = "header", value = as_paragraph(as_i("p"), as_sub("adj")))
    } 
    # β and 95% CI remain plain text
  }
  
  ft <- ft %>%
    set_table_properties(layout = "autofit") %>%
    fontsize(size = 8, part = "all") %>%
    flextable::font(fontname = "Times New Roman", part = "all") %>%
    align(j = 1, align = "left", part = "all") %>%
    align(j = 2:ncol(combined_df), align = "center", part = "all") %>%
    bold(i = 1, part = "header") %>%
    bold(i = 2, part = "header", bold = FALSE) %>%
    merge_v(j = 1)  # merge Moderator cells vertically
  
  return(ft)
}

# Write combined results to Word
write_combined_results_to_word <- function(results_list, moderators, moderator_order,
                                           filename = "combined_results.docx",
                                           top_labels = c("Internalising symptoms",
                                                          "Externalising symptoms",
                                                          "ADHD symptoms")) {
  outcome_names <- c("inter_table", "exter_table", "ADHD_table")
  combined_df <- combine_results_side_by_side(results_list, outcome_names, moderators, moderator_order)
  ft <- create_combined_flextable(combined_df, top_labels)
  doc <- read_docx() %>% body_add_flextable(ft)
  print(doc, target = filename)
}

# Example usage
moderator_order <- c(
  'Pren. maternal depression','Pren. partner depression',
  'Post. partner depression','Friendship','Pren. Life Events',
  'Pren. Contextual Risk','Pren. Interpersonal Risk', 'Post. Life Events', 
  'Post. Contextual Risk', 'Post. Interpersonal Risk',
  'Post. Direct Victimization','Pren. maternal drinking',
  'Pren. maternal smoking','Maternal Med. diet', 'Child Med. diet'
)
moderators <- unique(combined_ALSPAC_GenR$moderator)
results_list <- list(
  inter_table = inter_table,
  exter_table = exter_table,
  ADHD_table = ADHD_table
)

write_combined_results_to_word(
  results_list,
  moderators,
  moderator_order,
  filename = paste0(projectpath,'/results/tables/', "STable_direct_GenR_ALSPAC.docx")
)
