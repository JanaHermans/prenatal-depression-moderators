################################################################################
# Title:    Meta-analyses moderator project - moderation 
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
outdir <- paste0(projectpath,"/results/tables/")
filename_ALSPAC <- "res_main_imputed_ALSPAC2025-11-10.RData"
filename_GenR <- "res_main_imputed_GenR2025-11-07.RData"
ALSPAC_file <- readRDS(file.path(results_path,filename_ALSPAC))
GenR_file <- readRDS(file.path(results_path,filename_GenR))
#---------------------------------Meta-analysis---------------------------------
out_list <- c('exter_z', 'inter_z', 'ADHD_z')
mod_list <- c('p18w', 'p3y','friendship','preLE','preCR','preIR','posLE','posCR', 
              'posIR', 'posDV','alcohol','smoking','mat_diet','child_diet')
results_list <- list()
dataframes <- list()

for (mod in mod_list) {
  mod_INTER <- paste0(mod, "_INTER")
  for (outcome in out_list) {
    ALSPAC_results <- ALSPAC_file[[mod_INTER]]$results_storage
    colnames(ALSPAC_results)[colnames(ALSPAC_results) == 'estimate'] <- 'preg_dep_betas'
    colnames(ALSPAC_results)[colnames(ALSPAC_results) == 'std.error'] <- 'ses'
    ALSPAC_results$cohort <- 'ALSPAC'
    ALSPAC_results <- ALSPAC_results[ALSPAC_results$outcome == outcome,]
    GenR_results <- GenR_file[[mod_INTER]]$results_storage
    colnames(GenR_results)[colnames(GenR_results) == 'estimate'] <- 'preg_dep_betas'
    colnames(GenR_results)[colnames(GenR_results) == 'std.error'] <- 'ses'
    GenR_results$cohort <- 'GenR'
    GenR_results <- GenR_results[GenR_results$outcome == outcome,]
    INTER_results <- rbind(ALSPAC_results,GenR_results)
    m.model2 <- rma(yi = INTER_results$preg_dep_betas, sei = INTER_results$ses)
    for (i in 1:nrow(INTER_results)) {
      INTER_results$conf.low[i] <- INTER_results$preg_dep_betas[i]-1.96*INTER_results$ses[i]
      INTER_results$conf.high[i] <- INTER_results$preg_dep_betas[i]+1.96*INTER_results$ses[i]
    }
    colnames(INTER_results)[colnames(INTER_results) == 'preg_dep_betas'] <- 'estimate'
    INTER_results <- bind_rows(INTER_results,
                              data.frame(
                                cohort = "TOTAL",
                                term = unique(INTER_results$term),
                                outcome = unique(INTER_results$outcome),
                                estimate = m.model2$beta,
                                conf.low = m.model2$ci.lb,
                                conf.high = m.model2$ci.ub,
                                p.value = m.model2$pval
                              )
    ) 
    INTER_results$estimate <- as.numeric(INTER_results$estimate)
    INTER_results$conf.low <- as.numeric(INTER_results$conf.low)
    INTER_results$conf.high <- as.numeric(INTER_results$conf.high)
    INTER_results <- INTER_results[,c("cohort","term","estimate","outcome",
                                      "conf.low","conf.high","p.value")]
    row.names(INTER_results) <- NULL
    sig_df <- INTER_results %>%
      dplyr::filter((conf.low > 0 & conf.high > 0) |
                      (conf.low < 0 & conf.high < 0))
    if (nrow(sig_df) > 0) {
      message("===== Moderator: ", mod, " + Outcome: ", outcome, " =====")
      print(sig_df)
    }
    if (outcome == 'inter_z') {
      results_list$int_ <- INTER_results
    }
    if (outcome == 'exter_z') {
      results_list$ext_ <- INTER_results
    }
    if (outcome == 'ADHD_z') {
      results_list$adhd_ <- INTER_results
    }
  }
  dataframes[[mod]] <- results_list
}

labels_df <- data.frame(
  term = c("p_dep_18wg_bin1: Depression","p_dep_3y_bin1: Depression","friendship_z",
           "pre_LE_domain_score_z","pre_CR_domain_score_z","pre_IR_domain_score_z",
           "pos_LE_domain_score_z","pos_CR_domain_score_z","pos_IR_domain_score_z",
           "pos_DV_domain_score_z","preg_alc1: Any alcohol","preg_smk1: Any smoking",
           "med_diet_mat","med_diet_child"), 
  label = c('Pren. partner depression','Post. partner depression','Friendship',
            'Pren. Life Events','Pren. Contextual Risk','Pren. Interpersonal Risk', 
            'Post. Life Events','Post. Contextual Risk','Post. Interpersonal Risk', 
            'Post. Direct Victimization','Pren. maternal drinking',
            'Pren. maternal smoking','Maternal Med. diet','Child Med. diet')
)
#----------------------------------SAVE Table 2---------------------------------
# Obtain all total effects for Table 2
combined_total <- map_dfr(
  names(dataframes),
  function(mod) {
    map_dfr(
      names(dataframes[[mod]]),
      function(outcome) {
        dataframes[[mod]][[outcome]] %>%
          filter(cohort == "TOTAL")
      }
    )
  }
)

# Obtain labels
combined_total$term <- gsub('preg_dep_bin1: Depression:','',combined_total$term)

combined_total$moderator <- labels_df$label[match(combined_total$term, labels_df$term)]

combined_total <- combined_total[,c("moderator","estimate","outcome",
                                    "conf.low","conf.high","p.value")]

# Apply multiple testing correction
combined_total$p.value_adj <- p.adjust(combined_total$p.value, method = "BH")
# Number of eff. tests in GenR is 21 and in ALSPAC 22 (Galwey method):
#meff_df <- 28
#combined_total$p.value_adj <- 
#  ifelse(as.numeric(combined_total$p.value)*meff_df>1,1,
#         as.numeric(combined_total$p.value)*meff_df)

inter_table <- combined_total[combined_total$outcome == "inter_z",]
exter_table <- combined_total[combined_total$outcome == "exter_z",]
ADHD_table <- combined_total[combined_total$outcome == "ADHD_z",]

format_result <- function(df, moderators) {
  df <- df %>%
    mutate(
      `β` = sprintf("%.2f", estimate),
      `95% CI` = paste0("[", sprintf("%.2f", conf.low), ", ", sprintf("%.2f", conf.high), "]"),
      p = round(p.value, 3),
      `p (FDR)` = round(p.value_adj, 3)
    ) %>%
    select(Moderator = moderator, `β`, `95% CI`, p, `p (FDR)`)
  
  master_mods <- data.frame(Moderator = moderators)
  df <- left_join(master_mods, df, by = "Moderator")
  
  return(df)
}

combine_results_side_by_side <- function(results_list, outcome_names, moderators) {
  formatted_dfs <- list()
  
  for (outcome in outcome_names) {
    if (!is.null(results_list[[outcome]])) {
      df <- format_result(results_list[[outcome]], moderators)
      formatted_dfs[[outcome]] <- df
    }
  }
  
  # Combine all outcomes side-by-side
  combined_df <- do.call(cbind, lapply(formatted_dfs, function(x) x[, -1]))  # exclude Moderator
  combined_df <- cbind(formatted_dfs[[1]][, 1, drop = FALSE], combined_df)   # add Moderator column
  
  return(combined_df)
}

create_combined_flextable <- function(combined_df, outcome_names,
                                      top_labels = c("Internalising symptoms",
                                                     "Externalising symptoms",
                                                     "ADHD symptoms")) {
  combined_df <- as.data.frame(combined_df)
  ft <- flextable(combined_df)
  
  n_outcomes <- length(outcome_names)
  n_cols <- ncol(combined_df)
  
  # Add top header row (outcome names)
  colwidths <- c(1, rep(4, n_outcomes))
  ft <- add_header_row(
    ft,
    values = c("", top_labels),
    colwidths = colwidths
  )
  
  # Add bottom header row (subheaders)
  pattern <- c("β", "95% CI", "p", "p (FDR)")
  subheaders <- c("Moderator", rep(pattern, times = n_outcomes))
  subheaders <- subheaders[seq_len(n_cols)]
  
  ft <- set_header_labels(ft, values = setNames(subheaders, names(combined_df)))
  
  # Compose subheaders: italic p and p with subscript FDR
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
  
  # Style table
  ft <- ft %>%
    set_table_properties(layout = "autofit") %>%
    fontsize(size = 8, part = "all") %>%         # smaller font
    flextable::font(fontname = "Times New Roman", part = "all") %>%
    align(j = 1, align = "left", part = "all") %>%  # left-align Moderator
    align(j = 2:n_cols, align = "center", part = "all") %>% # center numeric columns
    bold(i = 1, part = "header") %>%             # only top header row bold
    bold(i = 2, part = "header", bold = FALSE)   # ensure bottom header row is NOT bold
  
  return(ft)
}


write_combined_results_to_word <- function(results_list, filename = "combined_results.docx", moderators) {
  outcome_names <- c("inter_table", "exter_table", "ADHD_table")
  combined_df <- combine_results_side_by_side(results_list, outcome_names, moderators)
  ft <- create_combined_flextable(combined_df, outcome_names)
  doc <- read_docx() %>%
    body_add_flextable(ft)
  print(doc, target = filename)
}

moderators <- unique(combined_total$moderator)  # or a predefined order
results_list <- list(
  inter_table = inter_table,
  exter_table = exter_table,
  ADHD_table = ADHD_table
)

write_combined_results_to_word(results_list, paste0(outdir,"Table2_main.docx"),
                               moderators)

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

combined_ALSPAC_GenR$term <- gsub('preg_dep_bin1: Depression:','',combined_ALSPAC_GenR$term)

combined_ALSPAC_GenR$moderator <- labels_df$label[match(combined_ALSPAC_GenR$term,
                                                        labels_df$term)]

combined_ALSPAC_GenR <- combined_ALSPAC_GenR[,c("moderator","cohort","estimate","outcome",
                                                "conf.low","conf.high","p.value")]

# Apply multiple testing correction
combined_ALSPAC_GenR$p.value_adj <- p.adjust(combined_ALSPAC_GenR$p.value, method = "BH")
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
      p = round(p.value, 3),
      `p (FDR)` = round(p.value_adj, 3),
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
  'Pren. partner depression','Post. partner depression','Friendship',
  'Pren. Life Events','Pren. Contextual Risk','Pren. Interpersonal Risk',
  'Post. Life Events','Post. Contextual Risk','Post. Interpersonal Risk', 
  'Post. Direct Victimization','Pren. maternal drinking',
  'Pren. maternal smoking','Maternal Med. diet','Child Med. diet'
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
  filename = paste0(outdir, "STable_main_GenR_ALSPAC.docx")
)
