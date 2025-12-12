################################################################################
# Title:    Meta-analyses moderator project - sensitivity analyses
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
#------------------------------PRE-PREG DEPRESSION------------------------------
filename_ALSPAC <- "res_prepregdep_imputed_ALSPAC2025-11-10.RData"
filename_GenR <- "res_prepregdep_imputed_GenR2025-11-07.RData"
ALSPAC_file <- readRDS(file.path(results_path,filename_ALSPAC))
GenR_file <- readRDS(file.path(results_path,filename_GenR))

# Meta-analysis
out_list <- c('exter_z', 'inter_z', 'ADHD_z')
mod_list <- c('p18w', 'p3y','friendship','preLE','preCR','preIR','posLE','posCR', 
              'posIR', 'posDV','alcohol', 'smoking','mat_diet','child_diet')
results_list <- list()
dataframes <- list()

for (mod in mod_list) {
  mod_INTER <- paste0(mod, "_INTER")
  for (outcome in out_list) {
    ALSPAC_results <- ALSPAC_file[[mod_INTER]]
    colnames(ALSPAC_results)[colnames(ALSPAC_results) == 'estimate'] <- 'preg_dep_betas'
    colnames(ALSPAC_results)[colnames(ALSPAC_results) == 'std.error'] <- 'ses'
    ALSPAC_results$cohort <- 'ALSPAC'
    ALSPAC_results <- ALSPAC_results[ALSPAC_results$outcome == outcome,]
    GenR_results <- GenR_file[[mod_INTER]]
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
           "pos_DV_domain_score_z","preg_alc1: Any alcohol",
           "preg_smk1: Any smoking","med_diet_mat","med_diet_child"), 
  label = c('Pren. partner depression','Post. partner depression','Friendship',
            'Pren. Life Events', 'Pren. Contextual Risk','Pren. Interpersonal Risk',
            'Post. Life Events','Post. Contextual Risk','Post. Interpersonal Risk', 
            'Post. Direct Victimization','Pren. maternal drinking',
            'Pren. maternal smoking','Maternal Med. diet','Child Med. diet')
)

# Obtain all total effects for Supplementary Table
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

inter_table <- combined_total[combined_total$outcome == "inter_z",]
exter_table <- combined_total[combined_total$outcome == "exter_z",]
ADHD_table <- combined_total[combined_total$outcome == "ADHD_z",]

format_result <- function(df, moderators) {
  df <- df %>%
    mutate(
      `β` = sprintf("%.2f", estimate),
      `95% CI` = paste0("[", sprintf("%.2f", conf.low), ", ", sprintf("%.2f", conf.high), "]"),
      p = round(p.value, 3)
    ) %>%
    select(Moderator = moderator, `β`, `95% CI`, p)
  
  # Keep all moderators, even if missing in this table
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
  colwidths <- c(1, rep(3, n_outcomes))
  ft <- add_header_row(
    ft,
    values = c("", top_labels),
    colwidths = colwidths
  )
  
  # Add bottom header row (subheaders)
  pattern <- c("β", "95% CI", "p")
  subheaders <- c("Moderator", rep(pattern, times = n_outcomes))
  subheaders <- subheaders[seq_len(n_cols)]
  
  ft <- set_header_labels(ft, values = setNames(subheaders, names(combined_df)))
  
  # Compose subheaders: italic p
  for (j in seq_along(names(combined_df))) {
    col <- names(combined_df)[j]
    header_name <- subheaders[j]
    
    if (header_name == "p") {
      ft <- compose(ft, j = col, part = "header", value = as_paragraph(as_i("p")))
    } 
    # β and 95% CI remain plain text
  }
  
  # Style table
  ft <- ft %>%
    set_table_properties(layout = "autofit") %>%
    fontsize(size = 8, part = "all") %>%         # smaller font
    font(fontname = "Times New Roman", part = "all") %>%
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

write_combined_results_to_word(results_list, paste0(outdir,"STable_prepregdep.docx"),
                               moderators)

#-----------------------------CONTINUOUS DEPRESSION-----------------------------
filename_ALSPAC <- "res_continuous_imputed_ALSPAC2025-11-10.RData"
filename_GenR <- "res_continuous_imputed_GenR2025-11-07.RData"
ALSPAC_file <- readRDS(file.path(results_path,filename_ALSPAC))
GenR_file <- readRDS(file.path(results_path,filename_GenR))

# Meta-analysis
out_list <- c('exter_z', 'inter_z', 'ADHD_z')
mod_list <- c('p18w','p3y','friendship','preLE','preCR','preIR','posLE','posCR', 
              'posIR','posDV','alcohol','smoking','mat_diet','child_diet')
results_list <- list()
dataframes <- list()

for (mod in mod_list) {
  mod_INTER <- paste0(mod, "_INTER")
  for (outcome in out_list) {
    ALSPAC_results <- ALSPAC_file[[mod_INTER]]
    colnames(ALSPAC_results)[colnames(ALSPAC_results) == 'estimate'] <- 'preg_dep_betas'
    colnames(ALSPAC_results)[colnames(ALSPAC_results) == 'std.error'] <- 'ses'
    ALSPAC_results$cohort <- 'ALSPAC'
    ALSPAC_results <- ALSPAC_results[ALSPAC_results$outcome == outcome,]
    GenR_results <- GenR_file[[mod_INTER]]
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
           "pos_DV_domain_score_z","preg_alc1: Any alcohol",
           "preg_smk1: Any smoking","med_diet_mat","med_diet_child"), 
  label = c('Pren. partner depression','Post. partner depression','Friendship',
            'Pren. Life Events', 'Pren. Contextual Risk','Pren. Interpersonal Risk',
            'Post. Life Events','Post. Contextual Risk','Post. Interpersonal Risk', 
            'Post. Direct Victimization','Pren. maternal drinking',
            'Pren. maternal smoking','Maternal Med. diet','Child Med. diet')
)

# Obtain all total effects for Supplementary Table
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
combined_total$term <- gsub('preg_dep:','',combined_total$term)

combined_total$moderator <- labels_df$label[match(combined_total$term, labels_df$term)]

combined_total <- combined_total[,c("moderator","estimate","outcome",
                                    "conf.low","conf.high","p.value")]

inter_table <- combined_total[combined_total$outcome == "inter_z",]
exter_table <- combined_total[combined_total$outcome == "exter_z",]
ADHD_table <- combined_total[combined_total$outcome == "ADHD_z",]

format_result <- function(df, moderators) {
  df <- df %>%
    mutate(
      `β` = sprintf("%.2f", estimate),
      `95% CI` = paste0("[", sprintf("%.2f", conf.low), ", ", sprintf("%.2f", conf.high), "]"),
      p = round(p.value, 3)
    ) %>%
    select(Moderator = moderator, `β`, `95% CI`, p)
  
  # Keep all moderators, even if missing in this table
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
  colwidths <- c(1, rep(3, n_outcomes))
  ft <- add_header_row(
    ft,
    values = c("", top_labels),
    colwidths = colwidths
  )
  
  # Add bottom header row (subheaders)
  pattern <- c("β", "95% CI", "p")
  subheaders <- c("Moderator", rep(pattern, times = n_outcomes))
  subheaders <- subheaders[seq_len(n_cols)]
  
  ft <- set_header_labels(ft, values = setNames(subheaders, names(combined_df)))
  
  # Compose subheaders: italic p
  for (j in seq_along(names(combined_df))) {
    col <- names(combined_df)[j]
    header_name <- subheaders[j]
    
    if (header_name == "p") {
      ft <- compose(ft, j = col, part = "header", value = as_paragraph(as_i("p")))
    } 
    # β and 95% CI remain plain text
  }
  
  # Style table
  ft <- ft %>%
    set_table_properties(layout = "autofit") %>%
    fontsize(size = 8, part = "all") %>%         # smaller font
    font(fontname = "Times New Roman", part = "all") %>%
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

write_combined_results_to_word(results_list, paste0(outdir,"STable_continuous.docx"),
                               moderators)

#-------------------------------YOUTH SELF-REPORT-------------------------------
filename_GenR <- "res_YSR_imputed_GenR2025-11-07.RData"
GenR_file <- readRDS(file.path(results_path,filename_GenR))

# Meta-analysis
out_list <- c('exter_YSR_z', 'inter_YSR_z', 'ADHD_YSR_z')
mod_list <- c('p18w', 'p3y','friendship','preLE','preCR','preIR','posLE','posCR', 
              'posIR', 'posDV','alcohol', 'smoking','mat_diet','child_diet')
results_list <- list()
dataframes <- list()

for (mod in mod_list) {
  mod_INTER <- paste0(mod, "_INTER")
  for (outcome in out_list) {
    GenR_results <- GenR_file[[mod_INTER]]
    colnames(GenR_results)[colnames(GenR_results) == 'std.error'] <- 'ses'
    GenR_results$cohort <- 'GenR'
    GenR_results <- GenR_results[GenR_results$outcome == outcome,]
    INTER_results <- GenR_results
    for (i in 1:nrow(INTER_results)) {
      INTER_results$conf.low[i] <- 
        INTER_results$estimate[i]-1.96*INTER_results$ses[i]
      INTER_results$conf.high[i] <- 
        INTER_results$estimate[i]+1.96*INTER_results$ses[i]
    }
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
    if (outcome == 'inter_YSR_z') {
      results_list$int_ <- INTER_results
    }
    if (outcome == 'exter_YSR_z') {
      results_list$ext_ <- INTER_results
    }
    if (outcome == 'ADHD_YSR_z') {
      results_list$adhd_ <- INTER_results
    }
  }
  dataframes[[mod]] <- results_list
}

labels_df <- data.frame(
  term = c("p_dep_18wg_bin1: Depression","p_dep_3y_bin1: Depression","friendship_z",
           "pre_LE_domain_score_z","pre_CR_domain_score_z","pre_IR_domain_score_z",
           "pos_LE_domain_score_z","pos_CR_domain_score_z","pos_IR_domain_score_z",
           "pos_DV_domain_score_z","preg_alc1: Any alcohol",
           "preg_smk1: Any smoking","med_diet_mat","med_diet_child"), 
  label = c('Pren. partner depression','Post. partner depression','Friendship',
            'Pren. Life Events', 'Pren. Contextual Risk','Pren. Interpersonal Risk',
            'Post. Life Events','Post. Contextual Risk','Post. Interpersonal Risk', 
            'Post. Direct Victimization','Pren. maternal drinking',
            'Pren. maternal smoking','Maternal Med. diet','Child Med. diet')
)

# Obtain all total effects for Supplementary Table
combined_total <- map_dfr(
  names(dataframes),
  function(mod) {
    map_dfr(
      names(dataframes[[mod]]),
      function(outcome) {
        dataframes[[mod]][[outcome]] %>%
          filter(cohort == "GenR")
      }
    )
  }
)

# Obtain labels
combined_total$term <- gsub('preg_dep_bin1: Depression:','',combined_total$term)

combined_total$moderator <- labels_df$label[match(combined_total$term, labels_df$term)]

combined_total <- combined_total[,c("moderator","estimate","outcome",
                                    "conf.low","conf.high","p.value")]

inter_table <- combined_total[combined_total$outcome == "inter_YSR_z",]
exter_table <- combined_total[combined_total$outcome == "exter_YSR_z",]
ADHD_table <- combined_total[combined_total$outcome == "ADHD_YSR_z",]

format_result <- function(df, moderators) {
  df <- df %>%
    mutate(
      `β` = sprintf("%.2f", estimate),
      `95% CI` = paste0("[", sprintf("%.2f", conf.low), ", ", sprintf("%.2f", conf.high), "]"),
      p = round(p.value, 3)
    ) %>%
    select(Moderator = moderator, `β`, `95% CI`, p)
  
  # Keep all moderators, even if missing in this table
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
  colwidths <- c(1, rep(3, n_outcomes))
  ft <- add_header_row(
    ft,
    values = c("", top_labels),
    colwidths = colwidths
  )
  
  # Add bottom header row (subheaders)
  pattern <- c("β", "95% CI", "p")
  subheaders <- c("Moderator", rep(pattern, times = n_outcomes))
  subheaders <- subheaders[seq_len(n_cols)]
  
  ft <- set_header_labels(ft, values = setNames(subheaders, names(combined_df)))
  
  # Compose subheaders: italic p
  for (j in seq_along(names(combined_df))) {
    col <- names(combined_df)[j]
    header_name <- subheaders[j]
    
    if (header_name == "p") {
      ft <- compose(ft, j = col, part = "header", value = as_paragraph(as_i("p")))
    } 
    # β and 95% CI remain plain text
  }
  
  # Style table
  ft <- ft %>%
    set_table_properties(layout = "autofit") %>%
    fontsize(size = 8, part = "all") %>%         # smaller font
    font(fontname = "Times New Roman", part = "all") %>%
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

write_combined_results_to_word(results_list, paste0(outdir,"STable_YSR.docx"),
                               moderators)


#-----------------------------COMPLETE-CASE ANALYSES----------------------------
filename_ALSPAC <- "res_completecases_ALSPAC2025-11-10.RData"
filename_GenR <- "res_completecases_GenR2025-11-10.RData"
ALSPAC_file <- readRDS(file.path(results_path,filename_ALSPAC))
GenR_file <- readRDS(file.path(results_path,filename_GenR))

# Meta-analysis
out_list <- c('exter_z', 'inter_z', 'ADHD_z')
mod_list <- c('p18w', 'p3y','friendship','preLE','preCR','preIR','posLE','posCR', 
              'posIR', 'posDV','alcohol', 'smoking','mat_diet','child_diet')
results_list <- list()
dataframes <- list()

for (mod in mod_list) {
  mod_INTER <- paste0(mod, "_INTER")
  for (outcome in out_list) {
    ALSPAC_results <- ALSPAC_file[[mod_INTER]]
    colnames(ALSPAC_results)[colnames(ALSPAC_results) == 'estimate'] <- 'preg_dep_betas'
    colnames(ALSPAC_results)[colnames(ALSPAC_results) == 'std.error'] <- 'ses'
    ALSPAC_results$cohort <- 'ALSPAC'
    ALSPAC_results <- ALSPAC_results[ALSPAC_results$outcome == outcome,]
    GenR_results <- GenR_file[[mod_INTER]]
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
           "pos_DV_domain_score_z","preg_alc1: Any alcohol",
           "preg_smk1: Any smoking","med_diet_mat","med_diet_child"), 
  label = c('Pren. partner depression','Post. partner depression','Friendship',
            'Pren. Life Events', 'Pren. Contextual Risk','Pren. Interpersonal Risk',
            'Post. Life Events','Post. Contextual Risk','Post. Interpersonal Risk', 
            'Post. Direct Victimization','Pren. maternal drinking',
            'Pren. maternal smoking','Maternal Med. diet','Child Med. diet')
)

# Obtain all total effects for Supplementary Table
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

combined_total$p.value_adj <- p.adjust(combined_total$p.value, method = "BH")

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

write_combined_results_to_word(results_list, paste0(outdir,"STable_completecases.docx"),
                               moderators)


#--------------------------------WITHOUT OUTLIERS-------------------------------
filename_ALSPAC <- "res_outliers_ALSPAC2025-12-08.RData"
filename_GenR <- "res_outliers_GenR2025-12-08.RData"
ALSPAC_file <- readRDS(file.path(results_path,filename_ALSPAC))
GenR_file <- readRDS(file.path(results_path,filename_GenR))

# Meta-analysis
out_list <- c('exter_z', 'inter_z', 'ADHD_z')
mod_list <- c('p18w', 'p3y','friendship','preLE','preCR','preIR','posLE','posCR', 
              'posIR', 'posDV','alcohol', 'smoking','mat_diet','child_diet')
results_list <- list()
dataframes <- list()

for (mod in mod_list) {
  mod_INTER <- paste0(mod, "_OUTLIERS")
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
    total_outliers <- sum(INTER_results$num_outliers)
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
    INTER_results[INTER_results$cohort == 'TOTAL',]$num_outliers <- total_outliers
    INTER_results <- INTER_results[,c("cohort","term","estimate","outcome",
                                      "conf.low","conf.high","p.value","num_outliers")]
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
           "pos_DV_domain_score_z","preg_alc1: Any alcohol",
           "preg_smk1: Any smoking","med_diet_mat","med_diet_child"), 
  label = c('Pren. partner depression','Post. partner depression','Friendship',
            'Pren. Life Events', 'Pren. Contextual Risk','Pren. Interpersonal Risk',
            'Post. Life Events','Post. Contextual Risk','Post. Interpersonal Risk', 
            'Post. Direct Victimization','Pren. maternal drinking',
            'Pren. maternal smoking','Maternal Med. diet','Child Med. diet')
)

# Obtain all total effects for Supplementary Table
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

inter_table <- combined_total[combined_total$outcome == "inter_z",]
exter_table <- combined_total[combined_total$outcome == "exter_z",]
ADHD_table <- combined_total[combined_total$outcome == "ADHD_z",]

format_result <- function(df, moderators) {
  df <- df %>%
    mutate(
      `β` = sprintf("%.2f", estimate),
      `95% CI` = paste0("[", sprintf("%.2f", conf.low), ", ", sprintf("%.2f", conf.high), "]"),
      p = round(p.value, 3)
    ) %>%
    select(Moderator = moderator, `β`, `95% CI`, p)
  
  # Keep all moderators, even if missing in this table
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
  colwidths <- c(1, rep(3, n_outcomes))
  ft <- add_header_row(
    ft,
    values = c("", top_labels),
    colwidths = colwidths
  )
  
  # Add bottom header row (subheaders)
  pattern <- c("β", "95% CI", "p")
  subheaders <- c("Moderator", rep(pattern, times = n_outcomes))
  subheaders <- subheaders[seq_len(n_cols)]
  
  ft <- set_header_labels(ft, values = setNames(subheaders, names(combined_df)))
  
  # Compose subheaders: italic p
  for (j in seq_along(names(combined_df))) {
    col <- names(combined_df)[j]
    header_name <- subheaders[j]
    
    if (header_name == "p") {
      ft <- compose(ft, j = col, part = "header", value = as_paragraph(as_i("p")))
    } 
    # β and 95% CI remain plain text
  }
  
  # Style table
  ft <- ft %>%
    set_table_properties(layout = "autofit") %>%
    fontsize(size = 8, part = "all") %>%         # smaller font
    font(fontname = "Times New Roman", part = "all") %>%
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

write_combined_results_to_word(results_list, paste0(outdir,"STable_outliers.docx"),
                               moderators)

