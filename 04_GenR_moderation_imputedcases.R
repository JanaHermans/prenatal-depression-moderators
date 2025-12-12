########################################################################
# Title:    Main analyses with full imputed sample (GenR)
# Author:   Jana Hermans
# Date:     Last modification November 2025
# Paper:    Prenatal maternal depression, child mental health, and the 
#           role of lifestyle and psychosocial factors: results from two 
#           population-based birth cohorts.
########################################################################

# Set up R environment for package libraries
rm(list=ls()) #clear environment 
set.seed(2025) #set seed for reproducibility script 
date <- format(Sys.Date(), '%d%m%y')
projectpath <- dirname(file.choose())
rlibpath <- dirname(file.choose())
setwd(projectpath)   # Set your working directory to your project folder
.libPaths(paste0(rlibpath, '/renv/library/R-4.3/x86_64-w64-mingw32'))
libraries <- c("haven","lme4","dplyr","mice","ggplot2","grid","gridExtra","car",
               "tidyr","stringr","broom.mixed","sjPlot","splines","reghelper",
               "patchwork","car")
invisible(lapply(libraries, require, character.only = T))

# Set working directory for where you keep your data. Choose any file, the goal 
# is to select the correct path
datapath <- dirname(file.choose())

# Load imputed data
filename <- 'GenR_impset_2025-10-23.Rdata'
load(file.path(datapath, filename))
ELS_50_theshold <- 'no'
#---------------------------------DATA PREP-------------------------------------
imputed.dfs <- mice::complete(imp_rf, "all")

imputed.dfs <- lapply(imputed.dfs, function (x)
  x <- x %>%
    dplyr::mutate(preg_dep_bin = dplyr::case_when(
      preg_dep <= 0.75 ~ "0: No depression",
      preg_dep > 0.75 ~ "1: Depression",
      TRUE ~ NA_character_
    ))
)

imputed.dfs <- lapply(imputed.dfs, function (x)
  x <- x %>%
    dplyr::mutate(p_dep_18wg_bin = dplyr::case_when(
      is.na(p_dep_18wg) ~ NA_character_,
      !is.na(p_dep_18wg) & p_dep_18wg <= 0.75  ~ "0: No depression",
      !is.na(p_dep_18wg) & p_dep_18wg > 0.75 ~ "1: Depression",
      TRUE ~ NA_character_
    ),
    p_dep_18wg_bin = factor(p_dep_18wg_bin)
    )
)

imputed.dfs <- lapply(imputed.dfs, function (x)
  x <- x %>%
    dplyr::mutate(p_dep_3y_bin = dplyr::case_when(
      is.na(p_dep_3y) ~ NA_character_,
      !is.na(p_dep_3y) & p_dep_3y <= 0.75  ~ "0: No depression",
      !is.na(p_dep_3y) & p_dep_3y > 0.75 ~ "1: Depression",
      TRUE ~ NA_character_
    ),
    p_dep_3y_bin = factor(p_dep_3y_bin)
    )
)

imputed.dfs <- lapply(imputed.dfs, function (x)
  x <- x %>%
    dplyr::mutate(prepreg_dep_bin = dplyr::case_when(
      prepreg_dep %in% c(1:2) ~ "0: No prior depression",
      prepreg_dep %in% c(3:4) ~ "1: Prior depression",
      TRUE ~ NA_character_
    ))
)

imputed.dfs <- lapply(imputed.dfs, function (x)
  x <- x %>%
    dplyr::mutate(natorig = dplyr::case_when(
      ETHNMv2 == 1 ~ "1: Dutch",
      ETHNMv2 %in% c(4,7) ~ "2: Turkish-Moroccan",
      ETHNMv2 %in% c(5,6) ~ "3: Surinamese-Antillean",
      ETHNMv2 == 700 ~ "4: European",
      ETHNMv2 %in% c(2,3,200,300,400,500,600,800) ~ "5: Other",
      TRUE ~ NA_character_
    ))
)

imputed.dfs <- lapply(imputed.dfs, function (x)
  x <- x %>%
    dplyr::mutate(edu_cat = dplyr::case_when(
      EDUCM == 5 ~ "1: University degree",   
      EDUCM == 4 ~ "2: 'Higher' vocational training",   
      EDUCM == 3 ~ "3: Three or more years of secondary school",   
      EDUCM %in% c(0,1,2) ~ "4: Primary or no education", 
      TRUE ~ NA_character_  # NA if none of the conditions are met
    ))
)

imputed.dfs <- lapply(imputed.dfs, function (x)
  x <- x %>%
    dplyr::mutate(preg_alc = dplyr::case_when(
      mdrink_updated == 0 ~ "0: No alcohol",
      mdrink_updated %in% c(1,2,3,4) ~ "1: Any alcohol",
      TRUE ~ NA_character_
    ))
)

imputed.dfs <- lapply(imputed.dfs, function (x)
  x <- x %>%
    dplyr::mutate(preg_smk = dplyr::case_when(
      smoke_all == 1 ~ "0: Never smoked",
      smoke_all %in% c(2,3) ~ "1: Any smoking",
      TRUE ~ NA_character_
    ))
)

imputed.dfs <- lapply(imputed.dfs, function(x) {
  x %>%
    mutate(
      med_diet_mat_ord = case_when(
        med_diet_mat == 0 ~ "0",
        med_diet_mat == 1 ~ "1",
        med_diet_mat == 2 ~ "2",
        med_diet_mat == 3 ~ "3",
        med_diet_mat == 4 ~ "4",
        med_diet_mat == 5 ~ "5",
        med_diet_mat == 6 ~ "6",
        med_diet_mat == 7 ~ "7",
        TRUE ~ NA_character_
      ),
      med_diet_mat_ord = factor(med_diet_mat_ord,
                            levels = as.character(0:7),
                            ordered = TRUE)
    )
})

# To align best with ALSPAC (1: very happy, 2: quite happy, 3: quite unhappy, 4: unhappy),
# the score is divided into four groups "10–<15", "15–<20", "20–<25", "25–30"

breaks  <- c(10, 15, 20, 25, 30)
labels  <- c("10–<15", "15–<20", "20–<25", "25–30")

imputed.dfs <- lapply(imputed.dfs, function (x) {
  #x$friendship <- as.factor(dplyr::ntile(as.numeric(x$friendship_sum), 4))
  #x$friendship <- cut(
  #  x$friendship_sum,
  #  breaks = breaks,
  #  right = FALSE,          # [10,15), [15,20), [20,25), [25,30]
  #  include.lowest = TRUE,  # include 10 in first bin, 30 in last
  #  labels = labels
  #)
  #x$friendship <- as.numeric(x$friendship_sum)
  return(x)
})

imputed.dfs <- lapply(imputed.dfs, function(x) {
  # Convert to percentiles
  x$ADHD_pct <- as.numeric(x$ADHD_symptoms) %>%
    dplyr::percent_rank()*100
  x$inter_pct <- as.numeric(x$inter_symptoms) %>%
    dplyr::percent_rank()*100
  x$exter_pct <- as.numeric(x$exter_symptoms) %>%
    dplyr::percent_rank()*100
  ## Convert to z-scores
  x$preg_dep_z <- as.numeric(x$preg_dep) %>%
    scale()
  x$preg_dep_z <- as.numeric(x$preg_dep_z) %>%
    as.vector()
  x$pre_LE_domain_score_z <- as.numeric(x$pre_LE_domain_score) %>%
    scale()
  x$pre_LE_domain_score_z <- as.numeric(x$pre_LE_domain_score_z) %>%
    as.vector()
  x$pre_CR_domain_score_z <- as.numeric(x$pre_CR_domain_score) %>%
    scale()
  x$pre_CR_domain_score_z <- as.numeric(x$pre_CR_domain_score_z) %>%
    as.vector()
  x$pre_IR_domain_score_z <- as.numeric(x$pre_IR_domain_score) %>%
    scale()
  x$pre_IR_domain_score_z <- as.numeric(x$pre_IR_domain_score_z) %>%
    as.vector()
  x$pos_LE_domain_score_z <- as.numeric(x$pos_LE_domain_score) %>%
    scale()
  x$pos_LE_domain_score_z <- as.numeric(x$pos_LE_domain_score_z) %>%
    as.vector()
  x$pos_CR_domain_score_z <- as.numeric(x$pos_CR_domain_score) %>%
    scale()
  x$pos_CR_domain_score_z <- as.numeric(x$pos_CR_domain_score_z) %>%
    as.vector()
  x$pos_IR_domain_score_z <- as.numeric(x$pos_IR_domain_score) %>%
    scale()
  x$pos_IR_domain_score_z <- as.numeric(x$pos_IR_domain_score_z) %>%
    as.vector()
  x$pos_DV_domain_score_z <- as.numeric(x$pos_DV_domain_score) %>%
    scale()
  x$pos_DV_domain_score_z <- as.numeric(x$pos_DV_domain_score_z) %>%
    as.vector()
  x$p_dep_18wg_z <- as.numeric(x$p_dep_18wg) %>%
    scale()
  x$p_dep_18wg_z <- as.numeric(x$p_dep_18wg_z) %>%
    as.vector()
  x$p_dep_3y_z <- as.numeric(x$p_dep_3y) %>%
    scale()
  x$p_dep_3y_z <- as.numeric(x$p_dep_3y_z) %>%
    as.vector()
  x$ADHD_z <- as.numeric(x$ADHD_symptoms) %>%
    scale()
  x$ADHD_z <- as.numeric(x$ADHD_z) %>%
    as.vector()
  x$inter_z <- as.numeric(x$inter_symptoms) %>%
    scale()
  x$inter_z <- as.numeric(x$inter_z) %>%
    as.vector()
  x$exter_z <- as.numeric(x$exter_symptoms) %>%
    scale()
  x$exter_z <- as.numeric(x$exter_z) %>%
    as.vector()
  x$ADHD_YSR_z <- as.numeric(x$ADHD_symptoms_YSR) %>%
    scale()
  x$ADHD_YSR_z <- as.numeric(x$ADHD_YSR_z) %>%
    as.vector()
  x$inter_YSR_z <- as.numeric(x$inter_symptoms_YSR) %>%
    scale()
  x$inter_YSR_z <- as.numeric(x$inter_YSR_z) %>%
    as.vector()
  x$exter_YSR_z <- as.numeric(x$exter_symptoms_YSR) %>%
    scale()
  x$exter_YSR_z <- as.numeric(x$exter_YSR_z) %>%
    as.vector()
  x$friendship_z <- as.numeric(x$friendship_sum) %>%
    scale()
  x$friendship_z <- as.numeric(x$friendship_z) %>%
    as.vector()
  
  return(x)
})

# Check for NAs in each dataset
lapply(seq_along(imputed.dfs), function(i) {
  n_na <- sum(is.na(imputed.dfs[[i]]))
  cat("Dataset", i, "has", n_na, "missing values\n")
})

#---------------------Include participants with >10pct data---------------------
incomplete_data <- imp_rf$data
check_df <- dplyr::select(incomplete_data, c("IDC","preg_dep","assigned_sex","EDUCM","m_age_birth",
                                           "ETHNMv2","prepreg_BMI","mdrink_updated","smoke_all","friendship_sum",
                                           "med_diet_mat","med_diet_child","p_dep_18wg","p_dep_3y",
                                           "pre_LE_domain_score","pre_CR_domain_score","pre_IR_domain_score",
                                           "pos_LE_domain_score","pos_CR_domain_score","pos_IR_domain_score",
                                           "pos_DV_domain_score","inter_symptoms","exter_symptoms",
                                           "ADHD_symptoms","outcome_age"))
threshold <- 0.1  # 10%
n_columns <- ncol(check_df) - 1  # Subtract 1 for the ID column
min_non_na <- ceiling(threshold * n_columns)  # Round up to the nearest whole number

# Count non-NA values per row, excluding the ID column
rows_with_10pct_data <- rowSums(!is.na(check_df[, -which(names(check_df) == "IDC")])) >= min_non_na

# Extract IDs of those rows
IDs_10pct <- as.data.frame(check_df[rows_with_10pct_data, "IDC"])
names(IDs_10pct) <- 'IDC_suf'

imputed.dfs <- lapply(imputed.dfs, function(x) {
  # Make sure the relevant columns are numeric
  suf_data <- x[x$IDC %in% IDs_10pct$IDC_suf,]
  x <- suf_data
  return(x)
})
#--------------------Include participants with >50pct of ELS--------------------
if (ELS_50_theshold == 'yes') {
  pren_stress <- readRDS(file.path(datapath, 'GenR_ELS/prenatal_stress_GENR.rds'))
  post_stress <- readRDS(file.path(datapath, 'GenR_ELS/postnatal_stress_GENR.rds'))
  pren_stress$IDM <- NULL
  post_stress$IDM <- NULL
  
  threshold <- 0.5  # 50%
  n_columns <- ncol(pren_stress) - 1  # Subtract 1 for the IDC column
  min_non_na <- ceiling(threshold * n_columns)  # Round up to the nearest whole number
  rows_with_50pct_prenELS <- rowSums(!is.na(pren_stress[, -which(names(pren_stress) == "IDC")])) >= min_non_na
  
  threshold <- 0.5  # 50%
  n_columns <- ncol(post_stress) - 1  # Subtract 1 for the IDC column
  min_non_na <- ceiling(threshold * n_columns)  # Round up to the nearest whole number
  rows_with_50pct_postELS <- rowSums(!is.na(post_stress[, -which(names(post_stress) == "IDC")])) >= min_non_na
  
  # Extract IDs of those rows
  IDs_50pct_prenELS <- as.data.frame(pren_stress[rows_with_50pct_prenELS, "IDC"])
  IDs_50pct_postELS <- as.data.frame(post_stress[rows_with_50pct_postELS, "IDC"])
  
  names(IDs_50pct_prenELS) <- 'IDC_suf'
  names(IDs_50pct_postELS) <- 'IDC_suf'
  
  imputed.dfs <- lapply(imputed.dfs, function(x) {
    # Make sure the relevant columns are numeric
    suf_data <- x[x$IDC %in% IDs_50pct_prenELS$IDC_suf,]
    x <- suf_data
    return(x)
  })
  imputed.dfs <- lapply(imputed.dfs, function(x) {
    # Make sure the relevant columns are numeric
    suf_data <- x[x$IDC %in% IDs_50pct_postELS$IDC_suf,]
    x <- suf_data
    return(x)
  })
}
#--------------------------Randomly include one sibling-------------------------
imputed.dfs <- lapply(imputed.dfs, function(df) {
  df %>%
    group_by(IDM) %>%
    slice_sample(n = 1) %>%
    ungroup()
})

# Recreate mids object
new_imp_rf <- miceadds::datlist2mids(imputed.dfs)  # Converts list to imputations for analysis
#--------------------------------MAIN ANALYSES-----------------------------------
covariates <- c("assigned_sex","outcome_age","preg_alc","preg_smk","m_age_birth",
                "prepreg_BMI","natorig","edu_cat")
outcome_list <- c('ADHD_z', 'inter_z', 'exter_z')

# Create empty results dfs for all moderators
result_names <- c("friendship", "alcohol", "smoking", "mat_diet", "child_diet", 
                  "preLE", "preCR", "preIR", "posLE", "posCR", "posIR", "posDV", 
                  "p18w", "p3y")
results_list <- setNames(vector("list", length(result_names)), result_names)

# Create the list with data frames
for (name in result_names) {
  results_list[[name]] <- data.frame(matrix(NA, nrow = 3, ncol = 7))
}

run_analysis <- function(outcome_list, new_imp_rf, exposure, variable, term, 
                         results_storage, covariates, residual_plots, 
                         residual_hists, residual_qqss, vif_values) {
  # Loop through all outcomes in outcome_list
  for (i in 1:3) {
    outcome <- outcome_list[i]
    
    # Construct the covariate part of the formula
    covariate_formula <- paste(covariates, collapse = " + ")
    
    # Create the formula dynamically based on the variable (e.g., "p_dep_18wg", "preg_alc", etc.)
    formula <- paste0(outcome, " ~ ",exposure,"*", variable, " + ", covariate_formula)
    
    # Run the model
    mod1_f <- with(new_imp_rf, lm(as.formula(formula)))
    pooled_mod1_f <- mice::pool(mod1_f)
    
    # Get the summary of the pooled model
    output <- summary(pooled_mod1_f, conf.int = TRUE)
    results <- as.data.frame(output[c("term", "estimate","std.error","p.value","2.5 %","97.5 %")])
    
    # Extract the specific term of interest
    results$term <- as.character(results$term)
    results <- results[results$term == term, ]
    results <- as.data.frame(results[c("term", "estimate","std.error","p.value","2.5 %","97.5 %")])
    
    # Add outcome variable to the results
    results$outcome <- outcome
    
    # Store the results in the appropriate results storage (e.g., results_p18w, results_p3y, etc.)
    results_storage[i, ] <- results
    
    # Plots
    fit1 <- mod1_f$analyses[[1]]
    
    resid1 <- resid(fit1)
    fitted1 <- fitted(fit1)
    
    df1 <- data.frame(residuals = resid1, fitted = fitted1)
    
    # Residual plots
    residual_plots[[paste(variable, outcome)]] <- ggplot(df1, aes(x = fitted, y = residuals)) +
      geom_point(alpha = 0.4) +
      geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
      labs(x = "Fitted values", y = "Residuals") +
      theme_minimal() +
      ggtitle(paste0("Residuals vs Fitted -",
                     "\nModerator: ", variable,
                     "\nOutcome: ", outcome)) 
    residual_hists[[paste(variable, outcome)]] <- ggplot(df1, aes(x=residuals)) + 
      geom_histogram(bins = 50) +
      labs(x = "", y = "Residuals") +
      theme_minimal() +
      ggtitle(paste0("Distribution residuals",
                     "\nModerator: ", variable,
                     "\nOutcome: ", outcome)) 
    residual_qqss[[paste(variable, outcome)]] <- ggplot(df1, aes(sample = residuals)) +
      stat_qq() +
      stat_qq_line(color = "red") +  # Add a reference line for normality
      labs(title = paste0("Q-Q Plot of Residuals",
                          "\nModerator: ", variable,
                          "\nOutcome: ", outcome)) +
      theme_minimal()
    vif_values[[paste(variable, outcome)]] <- lapply(mod1_f$analyses, function(model) vif(model, type = "predictor"))
  }
  
  # Set column names for the final results storage
  colnames(results_storage) <- colnames(results)
  
  return(list(
    results_storage = results_storage,
    residual_plots = residual_plots,
    residual_hists = residual_hists,
    residual_qqss = residual_qqss,
    vif_values = vif_values
  ))
}

res <- list()
residual_plots <- list()
residual_hists <- list()
residual_qqss <- list()
vif_values <- list()

res$mat_diet_INTER <- run_analysis(
  outcome_list = outcome_list, 
  new_imp_rf = new_imp_rf, 
  exposure = "preg_dep_bin",
  variable = "med_diet_mat", 
  term = "preg_dep_bin1: Depression:med_diet_mat", 
  results_storage = results_list$mat_diet, 
  covariates = covariates,
  residual_plots = residual_plots,
  residual_hists = residual_hists,
  residual_qqss = residual_qqss,
  vif_values = vif_values
)

res$friendship_INTER <- run_analysis(
  outcome_list = outcome_list, 
  new_imp_rf = new_imp_rf,
  exposure = "preg_dep_bin",
  variable = "friendship_z", 
  term = "preg_dep_bin1: Depression:friendship_z", 
  results_storage = results_list$friendship, 
  covariates = covariates,
  residual_plots = residual_plots,
  residual_hists = residual_hists,
  residual_qqss = residual_qqss,
  vif_values = vif_values
)

res$child_diet_INTER <- run_analysis(
  outcome_list = outcome_list, 
  new_imp_rf = new_imp_rf, 
  exposure = "preg_dep_bin",
  variable = "med_diet_child", 
  term = "preg_dep_bin1: Depression:med_diet_child", 
  results_storage = results_list$child_diet, 
  covariates = covariates,
  residual_plots = residual_plots,
  residual_hists = residual_hists,
  residual_qqss = residual_qqss,
  vif_values = vif_values
)

res$p18w_INTER <- run_analysis(
  outcome_list = outcome_list, 
  new_imp_rf = new_imp_rf, 
  exposure = "preg_dep_bin",
  variable = "p_dep_18wg_bin", 
  term = "preg_dep_bin1: Depression:p_dep_18wg_bin1: Depression", 
  results_storage = results_list$p18w, 
  covariates = covariates,
  residual_plots = residual_plots,
  residual_hists = residual_hists,
  residual_qqss = residual_qqss,
  vif_values = vif_values
)

res$p3y_INTER <- run_analysis(
  outcome_list = outcome_list, 
  new_imp_rf = new_imp_rf, 
  exposure = "preg_dep_bin",
  variable = "p_dep_3y_bin", 
  term = "preg_dep_bin1: Depression:p_dep_3y_bin1: Depression", 
  results_storage = results_list$p3y, 
  covariates = covariates,
  residual_plots = residual_plots,
  residual_hists = residual_hists,
  residual_qqss = residual_qqss,
  vif_values = vif_values
)

res$preLE_INTER <- run_analysis(
  outcome_list = outcome_list, 
  new_imp_rf = new_imp_rf, 
  exposure = "preg_dep_bin",
  variable = "pre_LE_domain_score_z", 
  term = "preg_dep_bin1: Depression:pre_LE_domain_score_z", 
  results_storage = results_list$preLE, 
  covariates = covariates,
  residual_plots = residual_plots,
  residual_hists = residual_hists,
  residual_qqss = residual_qqss,
  vif_values = vif_values
)

res$posLE_INTER <- run_analysis(
  outcome_list = outcome_list, 
  new_imp_rf = new_imp_rf, 
  exposure = "preg_dep_bin",
  variable = "pos_LE_domain_score_z", 
  term = "preg_dep_bin1: Depression:pos_LE_domain_score_z", 
  results_storage = results_list$posLE, 
  covariates = covariates,
  residual_plots = residual_plots,
  residual_hists = residual_hists,
  residual_qqss = residual_qqss,
  vif_values = vif_values
)

res$preIR_INTER <- run_analysis(
  outcome_list = outcome_list, 
  new_imp_rf = new_imp_rf, 
  exposure = "preg_dep_bin",
  variable = "pre_IR_domain_score_z", 
  term = "preg_dep_bin1: Depression:pre_IR_domain_score_z", 
  results_storage = results_list$preIR, 
  covariates = covariates,
  residual_plots = residual_plots,
  residual_hists = residual_hists,
  residual_qqss = residual_qqss,
  vif_values = vif_values
)

res$posIR_INTER <- run_analysis(
  outcome_list = outcome_list, 
  new_imp_rf = new_imp_rf, 
  exposure = "preg_dep_bin",
  variable = "pos_IR_domain_score_z", 
  term = "preg_dep_bin1: Depression:pos_IR_domain_score_z", 
  results_storage = results_list$posIR, 
  covariates = covariates,
  residual_plots = residual_plots,
  residual_hists = residual_hists,
  residual_qqss = residual_qqss,
  vif_values = vif_values
)

res$posDV_INTER <- run_analysis(
  outcome_list = outcome_list, 
  new_imp_rf = new_imp_rf, 
  exposure = "preg_dep_bin",
  variable = "pos_DV_domain_score_z", 
  term = "preg_dep_bin1: Depression:pos_DV_domain_score_z", 
  results_storage = results_list$posDV, 
  covariates = covariates,
  residual_plots = residual_plots,
  residual_hists = residual_hists,
  residual_qqss = residual_qqss,
  vif_values = vif_values
)

res$preCR_INTER <- run_analysis(
  outcome_list = outcome_list, 
  new_imp_rf = new_imp_rf, 
  exposure = "preg_dep_bin",
  variable = "pre_CR_domain_score_z", 
  term = "preg_dep_bin1: Depression:pre_CR_domain_score_z", 
  results_storage = results_list$preCR, 
  covariates = c("assigned_sex", "outcome_age", "preg_alc", "preg_smk", 
                 "m_age_birth", "prepreg_BMI", "natorig"), # excl. edu_cat which is part of CR
  residual_plots = residual_plots,
  residual_hists = residual_hists,
  residual_qqss = residual_qqss,
  vif_values = vif_values
)

res$posCR_INTER <- run_analysis(
  outcome_list = outcome_list, 
  new_imp_rf = new_imp_rf, 
  exposure = "preg_dep_bin",
  variable = "pos_CR_domain_score_z", 
  term = "preg_dep_bin1: Depression:pos_CR_domain_score_z", 
  results_storage = results_list$posCR, 
  covariates = c("assigned_sex", "outcome_age", "preg_alc", "preg_smk", 
                 "m_age_birth", "prepreg_BMI", "natorig"), # excl. edu_cat which is part of CR
  residual_plots = residual_plots,
  residual_hists = residual_hists,
  residual_qqss = residual_qqss,
  vif_values = vif_values
)

res$alcohol_INTER <- run_analysis(
  outcome_list = outcome_list, 
  new_imp_rf = new_imp_rf, 
  exposure = "preg_dep_bin",
  variable = "preg_alc", 
  term = "preg_dep_bin1: Depression:preg_alc1: Any alcohol", 
  results_storage = results_list$alcohol, 
  covariates = c("assigned_sex","outcome_age","preg_smk","m_age_birth",
                 "prepreg_BMI","natorig","edu_cat"), # exclu. preg_alc since it's included
  residual_plots = residual_plots,
  residual_hists = residual_hists,
  residual_qqss = residual_qqss,
  vif_values = vif_values
)

res$smoking_INTER <- run_analysis(
  outcome_list = outcome_list, 
  new_imp_rf = new_imp_rf, 
  exposure = "preg_dep_bin",
  variable = "preg_smk", 
  term = "preg_dep_bin1: Depression:preg_smk1: Any smoking", 
  results_storage = results_list$smoking, 
  covariates = c("assigned_sex","outcome_age","preg_alc","m_age_birth",
                 "prepreg_BMI","natorig","edu_cat"), # exclu. preg_smk since it's included
  residual_plots = residual_plots,
  residual_hists = residual_hists,
  residual_qqss = residual_qqss,
  vif_values = vif_values
)

saveRDS(res, file=file.path(projectpath,"output", 
                            paste0('res_main_imputed_GenR',
                                   as.character(Sys.Date()),".Rdata")))

# Combine all residual plots into one list
all_residual_plots <- do.call(c, lapply(res, function(x) x$residual_plots))
all_residual_hists <- do.call(c, lapply(res, function(x) x$residual_hists))
all_residual_qqss  <- do.call(c, lapply(res, function(x) x$residual_qqss))

big_resid_plot <- wrap_plots(all_residual_plots, ncol = 6) +
  plot_annotation(title = "Residuals vs Fitted for all Moderators and Outcomes")

big_resid_hist <- wrap_plots(all_residual_hists, ncol = 6) +
  plot_annotation(title = "Residual Distributions for all Moderators and Outcomes")

big_resid_qq <- wrap_plots(all_residual_qqss, ncol = 6) +
  plot_annotation(title = "Q-Q Plots for all Moderators and Outcomes")

ggsave(paste0(projectpath, "/output/model_checks/Residuals_vs_Fitted_GenR.png"), 
       big_resid_plot, width = 20, height = 18, dpi = 300)
ggsave(paste0(projectpath, "/output/model_checks/Residual_Histograms_GenR.png"), 
       big_resid_hist, width = 20, height = 18, dpi = 300)
ggsave(paste0(projectpath, "/output/model_checks/QQ_Plots_GenR.png"), 
       big_resid_qq, width = 20, height = 18, dpi = 300)

# Combine all VIFs
all_vifs <- lapply(res, function(x) x$vif_values)

# Check adjusted GVIF:
for (mod_name in names(all_vifs)) {
  cat("\n============================\n")
  cat("Moderator:", mod_name, "\n")
  cat("============================\n")
  
  vif_mod <- all_vifs[[mod_name]]
  
  for (outcome_name in names(vif_mod)) {
    cat("\nOutcome:", outcome_name, "\n")
    
    vif_list <- vif_mod[[outcome_name]]
    
    for (imp_index in seq_along(vif_list)) {
      vif_df <- vif_list[[imp_index]]
      
      # Some safety checks
      if (is.null(vif_df) || !"GVIF^(1/(2*Df))" %in% colnames(vif_df)) next
      
      high_vif <- vif_df[vif_df[["GVIF^(1/(2*Df))"]] > 1.6, , drop = FALSE]
      
      if (nrow(high_vif) > 0) {
        cat("  Imputation:", imp_index, "\n")
        print(high_vif)
      }
    }
  }
}

#-----------------MODERATER STRATIFIED ANALYSES (for plotting)------------------
run_mod_stratified_analysis <- function(outcome_list, new_imp_rf, exposure, 
                                        variable, mod_upper, mod_lower, term, 
                                        results_storage_exposed, 
                                        results_storage_unexposed, covariates) {
  
  # Loop through all outcomes
  for (i in seq_along(outcome_list)) {
    outcome <- outcome_list[i]
    
    # Construct covariate formula
    covariate_formula <- paste(covariates, collapse = " + ")
    
    # Full model formula without interaction (just main effect)
    formula <- paste0(outcome, " ~ ", exposure, " + ", covariate_formula)
    
    # --- Model for EXPOSED group (variable == exposed)
    if (is.numeric(mod_upper) == T) { 
      mod_exposed <- with(new_imp_rf, lm(as.formula(formula), 
                                         subset = get(variable) > mod_upper))
    }
    else if (is.numeric(mod_upper) == F) { 
      mod_exposed <- with(new_imp_rf, lm(as.formula(formula), 
                                         subset = get(variable) == mod_upper))
    }
    pooled_exposed <- mice::pool(mod_exposed)
    output_exposed <- summary(pooled_exposed, conf.int = TRUE)
    
    results_exposed <- as.data.frame(output_exposed[c("term", "estimate", "std.error", "p.value", "2.5 %", "97.5 %")])
    results_exposed$term <- as.character(results_exposed$term)
    results_exposed <- results_exposed[results_exposed$term == term, ]
    results_exposed$outcome <- outcome
    
    # Store in appropriate row of exposed results
    results_storage_exposed[i, ] <- results_exposed
    
    # --- Model for UNEXPOSED group (variable == unexposed)
    if (is.numeric(mod_lower) == T) { 
      mod_unexposed <- with(new_imp_rf, lm(as.formula(formula), 
                                           subset = get(variable) <= mod_lower))
    }
    else if (is.numeric(mod_lower) == F) { 
      mod_unexposed <- with(new_imp_rf, lm(as.formula(formula), 
                                           subset = get(variable) == mod_lower))
    }
    pooled_unexposed <- mice::pool(mod_unexposed)
    output_unexposed <- summary(pooled_unexposed, conf.int = TRUE)
    
    results_unexposed <- as.data.frame(output_unexposed[c("term", "estimate", "std.error", "p.value", "2.5 %", "97.5 %")])
    results_unexposed$term <- as.character(results_unexposed$term)
    results_unexposed <- results_unexposed[results_unexposed$term == term, ]
    results_unexposed$outcome <- outcome
    
    # Store in appropriate row of unexposed results
    results_storage_unexposed[i, ] <- results_unexposed
  }
  
  # Set consistent column names
  colnames(results_storage_exposed) <- colnames(results_exposed)
  colnames(results_storage_unexposed) <- colnames(results_unexposed)
  
  return(list(exposed = results_storage_exposed,
              unexposed = results_storage_unexposed))
}

covariates <- c("assigned_sex","outcome_age","preg_alc","preg_smk","m_age_birth","prepreg_BMI","natorig","edu_cat")

outcome_list <- c('ADHD_z', 'inter_z', 'exter_z')

res <- list()

# Prepare empty data.frames to collect results
results_exposed <- data.frame(matrix(NA, nrow = length(outcome_list), ncol = 7))
results_unexposed <- data.frame(matrix(NA, nrow = length(outcome_list), ncol = 7))

# Run stratified analysis
res$mat_diet <- run_mod_stratified_analysis(
  outcome_list = outcome_list,
  new_imp_rf = new_imp_rf,
  exposure = "preg_dep_bin",     
  variable = "med_diet_mat",         
  term = "preg_dep_bin1: Depression",   
  mod_upper = 3,
  mod_lower = 3,
  results_storage_exposed = results_exposed,
  results_storage_unexposed = results_unexposed,
  covariates = covariates  
)

res$child_diet <- run_mod_stratified_analysis(
  outcome_list = outcome_list,
  new_imp_rf = new_imp_rf,
  exposure = "preg_dep_bin",     
  variable = "med_diet_child",   
  mod_upper = 3,
  mod_lower = 3,
  term = "preg_dep_bin1: Depression",             
  results_storage_exposed = results_exposed,
  results_storage_unexposed = results_unexposed,
  covariates = covariates  
)

res$friendship <- run_mod_stratified_analysis(
  outcome_list = outcome_list,
  new_imp_rf = new_imp_rf,
  exposure = "preg_dep_bin",     
  variable = "friendship_z",
  mod_upper = 0,
  mod_lower = 0,
  term = "preg_dep_bin1: Depression",             
  results_storage_exposed = results_exposed,
  results_storage_unexposed = results_unexposed,
  covariates = covariates  
)

res$p18w <- run_mod_stratified_analysis(
  outcome_list = outcome_list,
  new_imp_rf = new_imp_rf,
  exposure = "preg_dep_bin",     
  variable = "p_dep_18wg_bin",     
  mod_upper = "1: Depression",
  mod_lower = "0: No depression",
  term = "preg_dep_bin1: Depression",             
  results_storage_exposed = results_exposed,
  results_storage_unexposed = results_unexposed,
  covariates = covariates  
)

res$p3y <- run_mod_stratified_analysis(
  outcome_list = outcome_list,
  new_imp_rf = new_imp_rf,
  exposure = "preg_dep_bin",     
  variable = "p_dep_3y_bin",         
  mod_upper = "1: Depression",
  mod_lower = "0: No depression",
  term = "preg_dep_bin1: Depression",             
  results_storage_exposed = results_exposed,
  results_storage_unexposed = results_unexposed,
  covariates = covariates  
)

res$preLE <- run_mod_stratified_analysis(
  outcome_list = outcome_list,
  new_imp_rf = new_imp_rf,
  exposure = "preg_dep_bin",     
  variable = "pre_LE_domain_score_z",         
  term = "preg_dep_bin1: Depression",        
  mod_upper = 0,
  mod_lower = 0,
  results_storage_exposed = results_exposed,
  results_storage_unexposed = results_unexposed,
  covariates = covariates  
)

res$preIR <- run_mod_stratified_analysis(
  outcome_list = outcome_list,
  new_imp_rf = new_imp_rf,
  exposure = "preg_dep_bin",     
  variable = "pre_IR_domain_score_z",         
  term = "preg_dep_bin1: Depression",   
  mod_upper = 0,
  mod_lower = 0,
  results_storage_exposed = results_exposed,
  results_storage_unexposed = results_unexposed,
  covariates = covariates  
)

res$preCR <- run_mod_stratified_analysis(
  outcome_list = outcome_list,
  new_imp_rf = new_imp_rf,
  exposure = "preg_dep_bin",     
  variable = "pre_CR_domain_score_z",         
  term = "preg_dep_bin1: Depression",   
  mod_upper = 0,
  mod_lower = 0,
  results_storage_exposed = results_exposed,
  results_storage_unexposed = results_unexposed,
  covariates = c("assigned_sex", "outcome_age", "preg_alc", "preg_smk", 
                 "m_age_birth", "prepreg_BMI", "natorig") # excl. edu_cat which is part of CR  
)

res$posLE <- run_mod_stratified_analysis(
  outcome_list = outcome_list,
  new_imp_rf = new_imp_rf,
  exposure = "preg_dep_bin",     
  variable = "pos_LE_domain_score_z",         
  term = "preg_dep_bin1: Depression",      
  mod_upper = 0,
  mod_lower = 0,
  results_storage_exposed = results_exposed,
  results_storage_unexposed = results_unexposed,
  covariates = covariates 
)

res$posIR <- run_mod_stratified_analysis(
  outcome_list = outcome_list,
  new_imp_rf = new_imp_rf,
  exposure = "preg_dep_bin",     
  variable = "pos_IR_domain_score_z",         
  term = "preg_dep_bin1: Depression",     
  mod_upper = 0,
  mod_lower = 0,
  results_storage_exposed = results_exposed,
  results_storage_unexposed = results_unexposed,
  covariates = covariates 
)

res$posCR <- run_mod_stratified_analysis(
  outcome_list = outcome_list,
  new_imp_rf = new_imp_rf,
  exposure = "preg_dep_bin",     
  variable = "pos_CR_domain_score_z",         
  term = "preg_dep_bin1: Depression",   
  mod_upper = 0,
  mod_lower = 0,
  results_storage_exposed = results_exposed,
  results_storage_unexposed = results_unexposed,
  covariates = c("assigned_sex", "outcome_age", "preg_alc", "preg_smk", 
                 "m_age_birth", "prepreg_BMI", "natorig") # excl. edu_cat which is part of CR  
)

res$posDV <- run_mod_stratified_analysis(
  outcome_list = outcome_list,
  new_imp_rf = new_imp_rf,
  exposure = "preg_dep_bin",     
  variable = "pos_DV_domain_score_z",         
  term = "preg_dep_bin1: Depression",  
  mod_upper = 0,
  mod_lower = 0,
  results_storage_exposed = results_exposed,
  results_storage_unexposed = results_unexposed,
  covariates = covariates 
)

res$alcohol <- run_mod_stratified_analysis(
  outcome_list = outcome_list,
  new_imp_rf = new_imp_rf,
  exposure = "preg_dep_bin",     
  variable = "preg_alc",
  mod_upper = "1: Any alcohol",
  mod_lower = "0: No alcohol",
  term = "preg_dep_bin1: Depression", 
  results_storage_exposed = results_exposed,
  results_storage_unexposed = results_unexposed,
  covariates = c("assigned_sex","outcome_age","preg_smk","m_age_birth",
                 "prepreg_BMI","natorig","edu_cat") # exclu. preg_alc since it's included 
)

res$smoking <- run_mod_stratified_analysis(
  outcome_list = outcome_list,
  new_imp_rf = new_imp_rf,
  exposure = "preg_dep_bin",     
  variable = "preg_smk",         
  mod_upper = "1: Any smoking",
  mod_lower = "0: Never smoked",
  term = "preg_dep_bin1: Depression", 
  results_storage_exposed = results_exposed,
  results_storage_unexposed = results_unexposed,
  covariates = c("assigned_sex","outcome_age","preg_alc","m_age_birth",
                 "prepreg_BMI","natorig","edu_cat") # exclu. preg_smk since it's included
)

saveRDS(res, file=file.path(projectpath,"output", 
                            paste0('res_mod_stratified_imputed_GenR',
                                   as.character(Sys.Date()),".Rdata")))

#--------------------------------DIRECT ANALYSES--------------------------------
run_direct_analysis <- function(outcome_list, new_imp_rf, exposure, 
                                term, results_storage, covariates) {
  
  # Loop through all outcomes
  for (i in seq_along(outcome_list)) {
    outcome <- outcome_list[i]
    
    # Construct covariate formula
    covariate_formula <- paste(covariates, collapse = " + ")
    
    # Full model formula without interaction (just main effect)
    formula <- paste0(outcome, " ~ ", exposure, " + ", covariate_formula)
    
    # --- Model for EXPOSED group
    mod <- with(new_imp_rf, lm(as.formula(formula)))
    
    pooled <- mice::pool(mod)
    output <- summary(pooled, conf.int = TRUE)
    
    results <- as.data.frame(output[c("term", "estimate", "std.error", "p.value", "2.5 %", "97.5 %")])
    results$term <- as.character(results$term)
    results <- results[results$term == term, ]
    results$outcome <- outcome
    
    # Store in appropriate row of exposed results
    results_storage[i, ] <- results
  }
  
  # Set consistent column names
  colnames(results_storage) <- colnames(results)
  
  return(results_storage)
}

covariates <- c("assigned_sex","outcome_age","preg_alc","preg_smk","m_age_birth","prepreg_BMI","natorig","edu_cat")

outcome_list <- c('ADHD_z', 'inter_z', 'exter_z')

res_direct <- list()

# Prepare empty data.frames to collect results
results <- data.frame(matrix(NA, nrow = length(outcome_list), ncol = 7))

# Run stratified analysis
res_direct$preg_dep <- run_direct_analysis(
  outcome_list = outcome_list,
  new_imp_rf = new_imp_rf,
  exposure = "preg_dep_bin",     
  term = "preg_dep_bin1: Depression",   
  results_storage = results,
  covariates = covariates  
)

res_direct$mat_diet <- run_direct_analysis(
  outcome_list = outcome_list,
  new_imp_rf = new_imp_rf,
  exposure = "med_diet_mat",     
  term = "med_diet_mat",   
  results_storage = results,
  covariates = covariates  
)

res_direct$child_diet <- run_direct_analysis(
  outcome_list = outcome_list,
  new_imp_rf = new_imp_rf,
  exposure = "med_diet_child",     
  term = "med_diet_child",             
  results_storage = results,
  covariates = covariates  
)

res_direct$friendship <- run_direct_analysis(
  outcome_list = outcome_list,
  new_imp_rf = new_imp_rf,
  exposure = "friendship_z",     
  term = "friendship_z",             
  results_storage = results,
  covariates = covariates  
)

res_direct$p18w <- run_direct_analysis(
  outcome_list = outcome_list,
  new_imp_rf = new_imp_rf,
  exposure = "p_dep_18wg_bin",     
  term = "p_dep_18wg_bin1: Depression",             
  results_storage = results,
  covariates = covariates  
)

res_direct$p3y <- run_direct_analysis(
  outcome_list = outcome_list,
  new_imp_rf = new_imp_rf,
  exposure = "p_dep_3y_bin",     
  term = "p_dep_3y_bin1: Depression",             
  results_storage = results,
  covariates = covariates  
)

res_direct$preLE <- run_direct_analysis(
  outcome_list = outcome_list,
  new_imp_rf = new_imp_rf,
  exposure = "pre_LE_domain_score_z",     
  term = "pre_LE_domain_score_z",             
  results_storage = results,
  covariates = covariates  
)

res_direct$preIR <- run_direct_analysis(
  outcome_list = outcome_list,
  new_imp_rf = new_imp_rf,
  exposure = "pre_IR_domain_score_z",     
  term = "pre_IR_domain_score_z",             
  results_storage = results,
  covariates = covariates  
)

res_direct$preCR <- run_direct_analysis(
  outcome_list = outcome_list,
  new_imp_rf = new_imp_rf,
  exposure = "pre_CR_domain_score_z",     
  term = "pre_CR_domain_score_z",             
  results_storage = results,
  covariates = c("assigned_sex", "outcome_age", "preg_alc", "preg_smk", 
                 "m_age_birth", "prepreg_BMI", "natorig") # excl. edu_cat which is part of CR  
)

res_direct$posLE <- run_direct_analysis(
  outcome_list = outcome_list,
  new_imp_rf = new_imp_rf,
  exposure = "pos_LE_domain_score_z",     
  term = "pos_LE_domain_score_z",             
  results_storage = results,
  covariates = covariates  
)

res_direct$posIR <- run_direct_analysis(
  outcome_list = outcome_list,
  new_imp_rf = new_imp_rf,
  exposure = "pos_IR_domain_score_z",     
  term = "pos_IR_domain_score_z",             
  results_storage = results,
  covariates = covariates  
)

res_direct$posCR <- run_direct_analysis(
  outcome_list = outcome_list,
  new_imp_rf = new_imp_rf,
  exposure = "pos_CR_domain_score_z",     
  term = "pos_CR_domain_score_z",             
  results_storage = results,
  covariates = c("assigned_sex", "outcome_age", "preg_alc", "preg_smk", 
                 "m_age_birth", "prepreg_BMI", "natorig") # excl. edu_cat which is part of CR  
)

res_direct$posDV <- run_direct_analysis(
  outcome_list = outcome_list,
  new_imp_rf = new_imp_rf,
  exposure = "pos_DV_domain_score_z",     
  term = "pos_DV_domain_score_z",             
  results_storage = results,
  covariates = covariates  
)

res_direct$alcohol <- run_direct_analysis(
  outcome_list = outcome_list,
  new_imp_rf = new_imp_rf,
  exposure = "preg_alc",     
  term = "preg_alc1: Any alcohol",             
  results_storage = results,
  covariates = c("assigned_sex","outcome_age","preg_smk","m_age_birth",
                 "prepreg_BMI","natorig","edu_cat") # exclu. preg_alc since it's included  
)

res_direct$smoking <- run_direct_analysis(
  outcome_list = outcome_list,
  new_imp_rf = new_imp_rf,
  exposure = "preg_smk",     
  term = "preg_smk1: Any smoking",             
  results_storage = results,
  covariates = c("assigned_sex","outcome_age","preg_alc","m_age_birth",
                 "prepreg_BMI","natorig","edu_cat") # exclu. preg_smk since it's included  
)

saveRDS(res_direct, file=file.path(projectpath,"output", 
                            paste0('res_direct_imputed_GenR',
                                   as.character(Sys.Date()),".Rdata")))

#--------------------SENSITIVTY: PRE-PREGNANCY DEPRESSION-----------------------
covariates <- c("assigned_sex","outcome_age","preg_alc","preg_smk","m_age_birth",
                "prepreg_BMI","natorig","edu_cat","prepreg_dep_bin")

outcome_list <- c('ADHD_z', 'inter_z', 'exter_z')

# Create empty results dfs for all moderators
result_names <- c("friendship", "alcohol", "smoking", "mat_diet", "child_diet", 
                  "preLE", "preCR", "preIR", "posLE", "posCR", "posIR", "posDV", 
                  "p18w", "p3y")
results_list <- setNames(vector("list", length(result_names)), result_names)
# Populate the list with data frames
for (name in result_names) {
  results_list[[name]] <- data.frame(matrix(NA, nrow = 3, ncol = 7))
}

run_analysis <- function(outcome_list, new_imp_rf, exposure, variable, term, results_storage, covariates) {
  # Loop through all outcomes in outcome_list
  for (i in 1:3) {
    outcome <- outcome_list[i]
    
    # Construct the covariate part of the formula
    covariate_formula <- paste(covariates, collapse = " + ")
    
    # Create the formula dynamically based on the variable (e.g., "p_dep_18wg", "preg_alc", etc.)
    formula <- paste0(outcome, " ~ ",exposure,"*", variable, " + ", covariate_formula)
    
    # Run the model
    mod1_f <- with(new_imp_rf, lm(as.formula(formula)))
    pooled_mod1_f <- mice::pool(mod1_f)
    
    # Get the summary of the pooled model
    output <- summary(pooled_mod1_f, conf.int = TRUE)
    results <- as.data.frame(output[c("term", "estimate","std.error","p.value","2.5 %","97.5 %")])
    
    # Extract the specific term of interest
    results$term <- as.character(results$term)
    results <- results[results$term == term, ]
    results <- as.data.frame(results[c("term", "estimate","std.error","p.value","2.5 %","97.5 %")])
    
    # Add outcome variable to the results
    results$outcome <- outcome
    
    # Store the results in the appropriate results storage (e.g., results_p18w, results_p3y, etc.)
    results_storage[i, ] <- results
  }
  
  # Set column names for the final results storage
  colnames(results_storage) <- colnames(results)
  
  return(results_storage)
}

res <- list()

res$mat_diet_INTER <- run_analysis(
  outcome_list = outcome_list, 
  new_imp_rf = new_imp_rf, 
  exposure = "preg_dep_bin",
  variable = "med_diet_mat", 
  term = "preg_dep_bin1: Depression:med_diet_mat", 
  results_storage = results_list$mat_diet, 
  covariates = covariates
)

res$friendship_INTER <- run_analysis(
  outcome_list = outcome_list, 
  new_imp_rf = new_imp_rf,
  exposure = "preg_dep_bin",
  variable = "friendship_z", 
  term = "preg_dep_bin1: Depression:friendship_z", 
  results_storage = results_list$friendship, 
  covariates = covariates
)

res$child_diet_INTER <- run_analysis(
  outcome_list = outcome_list, 
  new_imp_rf = new_imp_rf, 
  exposure = "preg_dep_bin",
  variable = "med_diet_child", 
  term = "preg_dep_bin1: Depression:med_diet_child", 
  results_storage = results_list$child_diet, 
  covariates = covariates
)

res$p18w_INTER <- run_analysis(
  outcome_list = outcome_list, 
  new_imp_rf = new_imp_rf, 
  exposure = "preg_dep_bin",
  variable = "p_dep_18wg_bin", 
  term = "preg_dep_bin1: Depression:p_dep_18wg_bin1: Depression", 
  results_storage = results_list$p18w, 
  covariates = covariates
)

res$p3y_INTER <- run_analysis(
  outcome_list = outcome_list, 
  new_imp_rf = new_imp_rf, 
  exposure = "preg_dep_bin",
  variable = "p_dep_3y_bin", 
  term = "preg_dep_bin1: Depression:p_dep_3y_bin1: Depression", 
  results_storage = results_list$p3y, 
  covariates = covariates
)

res$preLE_INTER <- run_analysis(
  outcome_list = outcome_list, 
  new_imp_rf = new_imp_rf, 
  exposure = "preg_dep_bin",
  variable = "pre_LE_domain_score_z", 
  term = "preg_dep_bin1: Depression:pre_LE_domain_score_z", 
  results_storage = results_list$preLE, 
  covariates = covariates
)

res$posLE_INTER <- run_analysis(
  outcome_list = outcome_list, 
  new_imp_rf = new_imp_rf, 
  exposure = "preg_dep_bin",
  variable = "pos_LE_domain_score_z", 
  term = "preg_dep_bin1: Depression:pos_LE_domain_score_z", 
  results_storage = results_list$posLE, 
  covariates = covariates
)

res$preIR_INTER <- run_analysis(
  outcome_list = outcome_list, 
  new_imp_rf = new_imp_rf, 
  exposure = "preg_dep_bin",
  variable = "pre_IR_domain_score_z", 
  term = "preg_dep_bin1: Depression:pre_IR_domain_score_z", 
  results_storage = results_list$preIR, 
  covariates = covariates
)

res$posIR_INTER <- run_analysis(
  outcome_list = outcome_list, 
  new_imp_rf = new_imp_rf, 
  exposure = "preg_dep_bin",
  variable = "pos_IR_domain_score_z", 
  term = "preg_dep_bin1: Depression:pos_IR_domain_score_z", 
  results_storage = results_list$posIR, 
  covariates = covariates
)

res$posDV_INTER <- run_analysis(
  outcome_list = outcome_list, 
  new_imp_rf = new_imp_rf, 
  exposure = "preg_dep_bin",
  variable = "pos_DV_domain_score_z", 
  term = "preg_dep_bin1: Depression:pos_DV_domain_score_z", 
  results_storage = results_list$posDV, 
  covariates = covariates
)

res$preCR_INTER <- run_analysis(
  outcome_list = outcome_list, 
  new_imp_rf = new_imp_rf, 
  exposure = "preg_dep_bin",
  variable = "pre_CR_domain_score_z", 
  term = "preg_dep_bin1: Depression:pre_CR_domain_score_z", 
  results_storage = results_list$preCR, 
  covariates = c("assigned_sex", "outcome_age", "preg_alc", "preg_smk", 
                 "m_age_birth", "prepreg_BMI", "natorig", "prepreg_dep_bin") # excl. edu_cat which is part of CR
)

res$posCR_INTER <- run_analysis(
  outcome_list = outcome_list, 
  new_imp_rf = new_imp_rf, 
  exposure = "preg_dep_bin",
  variable = "pos_CR_domain_score_z", 
  term = "preg_dep_bin1: Depression:pos_CR_domain_score_z", 
  results_storage = results_list$posCR, 
  covariates = c("assigned_sex", "outcome_age", "preg_alc", "preg_smk", 
                 "m_age_birth", "prepreg_BMI", "natorig", "prepreg_dep_bin") # excl. edu_cat which is part of CR
)

res$alcohol_INTER <- run_analysis(
  outcome_list = outcome_list, 
  new_imp_rf = new_imp_rf, 
  exposure = "preg_dep_bin",
  variable = "preg_alc", 
  term = "preg_dep_bin1: Depression:preg_alc1: Any alcohol", 
  results_storage = results_list$alcohol, 
  covariates = c("assigned_sex","outcome_age","preg_smk","m_age_birth",
                 "prepreg_BMI","natorig","edu_cat", "prepreg_dep_bin") # exclu. preg_alc since it's included
)

res$smoking_INTER <- run_analysis(
  outcome_list = outcome_list, 
  new_imp_rf = new_imp_rf, 
  exposure = "preg_dep_bin",
  variable = "preg_smk", 
  term = "preg_dep_bin1: Depression:preg_smk1: Any smoking", 
  results_storage = results_list$smoking, 
  covariates = c("assigned_sex","outcome_age","preg_alc","m_age_birth",
                 "prepreg_BMI","natorig","edu_cat", "prepreg_dep_bin") # exclu. preg_smk since it's included
)

saveRDS(res, file=file.path(projectpath,"output", 
                            paste0('res_prepregdep_imputed_GenR',as.character(Sys.Date()),".Rdata")))


#-----------------SENSITIVTY: CONTINUOUS PRENATAL DEPRESSION--------------------
covariates <- c("assigned_sex","outcome_age","preg_alc","preg_smk","m_age_birth",
                "prepreg_BMI","natorig","edu_cat")

outcome_list <- c('ADHD_z', 'inter_z', 'exter_z')

# Create empty results dfs for all moderators
result_names <- c("friendship", "alcohol", "smoking", "mat_diet", "child_diet", 
                  "preLE", "preCR", "preIR", "posLE", "posCR", "posIR", "posDV", 
                  "p18w", "p3y")
results_list <- setNames(vector("list", length(result_names)), result_names)
# Populate the list with data frames
for (name in result_names) {
  results_list[[name]] <- data.frame(matrix(NA, nrow = 3, ncol = 7))
}

run_analysis <- function(outcome_list, new_imp_rf, exposure, variable, term, results_storage, covariates) {
  # Loop through all outcomes in outcome_list
  for (i in 1:3) {
    outcome <- outcome_list[i]
    
    # Construct the covariate part of the formula
    covariate_formula <- paste(covariates, collapse = " + ")
    
    # Create the formula dynamically based on the variable (e.g., "p_dep_18wg", "preg_alc", etc.)
    formula <- paste0(outcome, " ~ ",exposure,"*", variable, " + ", covariate_formula)
    
    # Run the model
    mod1_f <- with(new_imp_rf, lm(as.formula(formula)))
    pooled_mod1_f <- mice::pool(mod1_f)
    
    # Get the summary of the pooled model
    output <- summary(pooled_mod1_f, conf.int = TRUE)
    results <- as.data.frame(output[c("term", "estimate","std.error","p.value","2.5 %","97.5 %")])
    
    # Extract the specific term of interest
    results$term <- as.character(results$term)
    results <- results[results$term == term, ]
    results <- as.data.frame(results[c("term", "estimate","std.error","p.value","2.5 %","97.5 %")])
    
    # Add outcome variable to the results
    results$outcome <- outcome
    
    # Store the results in the appropriate results storage (e.g., results_p18w, results_p3y, etc.)
    results_storage[i, ] <- results
  }
  
  # Set column names for the final results storage
  colnames(results_storage) <- colnames(results)
  
  return(results_storage)
}

res <- list()

res$mat_diet_INTER <- run_analysis(
  outcome_list = outcome_list, 
  new_imp_rf = new_imp_rf, 
  exposure = "preg_dep",
  variable = "med_diet_mat", 
  term = "preg_dep:med_diet_mat", 
  results_storage = results_list$mat_diet, 
  covariates = covariates
)

res$friendship_INTER <- run_analysis(
  outcome_list = outcome_list, 
  new_imp_rf = new_imp_rf,
  exposure = "preg_dep",
  variable = "friendship_z", 
  term = "preg_dep:friendship_z", 
  results_storage = results_list$friendship, 
  covariates = covariates
)

res$child_diet_INTER <- run_analysis(
  outcome_list = outcome_list, 
  new_imp_rf = new_imp_rf, 
  exposure = "preg_dep",
  variable = "med_diet_child", 
  term = "preg_dep:med_diet_child", 
  results_storage = results_list$child_diet, 
  covariates = covariates
)

res$p18w_INTER <- run_analysis(
  outcome_list = outcome_list, 
  new_imp_rf = new_imp_rf, 
  exposure = "preg_dep",
  variable = "p_dep_18wg_bin", 
  term = "preg_dep:p_dep_18wg_bin1: Depression", 
  results_storage = results_list$p18w, 
  covariates = covariates
)

res$p3y_INTER <- run_analysis(
  outcome_list = outcome_list, 
  new_imp_rf = new_imp_rf, 
  exposure = "preg_dep",
  variable = "p_dep_3y_bin", 
  term = "preg_dep:p_dep_3y_bin1: Depression", 
  results_storage = results_list$p3y, 
  covariates = covariates
)

res$preLE_INTER <- run_analysis(
  outcome_list = outcome_list, 
  new_imp_rf = new_imp_rf, 
  exposure = "preg_dep",
  variable = "pre_LE_domain_score_z", 
  term = "preg_dep:pre_LE_domain_score_z", 
  results_storage = results_list$preLE, 
  covariates = covariates
)

res$posLE_INTER <- run_analysis(
  outcome_list = outcome_list, 
  new_imp_rf = new_imp_rf, 
  exposure = "preg_dep",
  variable = "pos_LE_domain_score_z", 
  term = "preg_dep:pos_LE_domain_score_z", 
  results_storage = results_list$posLE, 
  covariates = covariates
)

res$preIR_INTER <- run_analysis(
  outcome_list = outcome_list, 
  new_imp_rf = new_imp_rf, 
  exposure = "preg_dep",
  variable = "pre_IR_domain_score_z", 
  term = "preg_dep:pre_IR_domain_score_z", 
  results_storage = results_list$preIR, 
  covariates = covariates
)

res$posIR_INTER <- run_analysis(
  outcome_list = outcome_list, 
  new_imp_rf = new_imp_rf, 
  exposure = "preg_dep",
  variable = "pos_IR_domain_score_z", 
  term = "preg_dep:pos_IR_domain_score_z", 
  results_storage = results_list$posIR, 
  covariates = covariates
)

res$posDV_INTER <- run_analysis(
  outcome_list = outcome_list, 
  new_imp_rf = new_imp_rf, 
  exposure = "preg_dep",
  variable = "pos_DV_domain_score_z", 
  term = "preg_dep:pos_DV_domain_score_z", 
  results_storage = results_list$posDV, 
  covariates = covariates
)

res$preCR_INTER <- run_analysis(
  outcome_list = outcome_list, 
  new_imp_rf = new_imp_rf, 
  exposure = "preg_dep",
  variable = "pre_CR_domain_score_z", 
  term = "preg_dep:pre_CR_domain_score_z", 
  results_storage = results_list$preCR, 
  covariates = c("assigned_sex", "outcome_age", "preg_alc", "preg_smk", 
                 "m_age_birth", "prepreg_BMI", "natorig") # excl. edu_cat which is part of CR
)

res$posCR_INTER <- run_analysis(
  outcome_list = outcome_list, 
  new_imp_rf = new_imp_rf, 
  exposure = "preg_dep",
  variable = "pos_CR_domain_score_z", 
  term = "preg_dep:pos_CR_domain_score_z", 
  results_storage = results_list$posCR, 
  covariates = c("assigned_sex", "outcome_age", "preg_alc", "preg_smk", 
                 "m_age_birth", "prepreg_BMI", "natorig") # excl. edu_cat which is part of CR
)

res$alcohol_INTER <- run_analysis(
  outcome_list = outcome_list, 
  new_imp_rf = new_imp_rf, 
  exposure = "preg_dep",
  variable = "preg_alc", 
  term = "preg_dep:preg_alc1: Any alcohol", 
  results_storage = results_list$alcohol, 
  covariates = c("assigned_sex","outcome_age","preg_smk","m_age_birth",
                 "prepreg_BMI","natorig","edu_cat") # exclu. preg_alc since it's included
)

res$smoking_INTER <- run_analysis(
  outcome_list = outcome_list, 
  new_imp_rf = new_imp_rf, 
  exposure = "preg_dep",
  variable = "preg_smk", 
  term = "preg_dep:preg_smk1: Any smoking", 
  results_storage = results_list$smoking, 
  covariates = c("assigned_sex","outcome_age","preg_alc","m_age_birth",
                 "prepreg_BMI","natorig","edu_cat") # exclu. preg_smk since it's included
)

saveRDS(res, file=file.path(projectpath,"output", 
                            paste0('res_continuous_imputed_GenR',as.character(Sys.Date()),".Rdata")))


#------------------------SENSITIVTY: YOUTH SELF-REPORT--------------------------
covariates <- c("assigned_sex","outcome_age","preg_alc","preg_smk","m_age_birth",
                "prepreg_BMI","natorig","edu_cat")

outcome_list <- c('ADHD_YSR_z', 'inter_YSR_z', 'exter_YSR_z')

# Create empty results dfs for all moderators
result_names <- c("friendship", "alcohol", "smoking", "mat_diet", "child_diet", 
                  "preLE", "preCR", "preIR", "posLE", "posCR", "posIR", "posDV", 
                  "p18w", "p3y")
results_list <- setNames(vector("list", length(result_names)), result_names)
# Populate the list with data frames
for (name in result_names) {
  results_list[[name]] <- data.frame(matrix(NA, nrow = 3, ncol = 7))
}

run_analysis <- function(outcome_list, new_imp_rf, exposure, variable, term, results_storage, covariates) {
  # Loop through all outcomes in outcome_list
  for (i in 1:3) {
    outcome <- outcome_list[i]
    
    # Construct the covariate part of the formula
    covariate_formula <- paste(covariates, collapse = " + ")
    
    # Create the formula dynamically based on the variable (e.g., "p_dep_18wg", "preg_alc", etc.)
    formula <- paste0(outcome, " ~ ",exposure,"*", variable, " + ", covariate_formula)
    
    # Run the model
    mod1_f <- with(new_imp_rf, lm(as.formula(formula)))
    pooled_mod1_f <- mice::pool(mod1_f)
    
    # Get the summary of the pooled model
    output <- summary(pooled_mod1_f, conf.int = TRUE)
    results <- as.data.frame(output[c("term", "estimate","std.error","p.value","2.5 %","97.5 %")])
    
    # Extract the specific term of interest
    results$term <- as.character(results$term)
    results <- results[results$term == term, ]
    results <- as.data.frame(results[c("term", "estimate","std.error","p.value","2.5 %","97.5 %")])
    
    # Add outcome variable to the results
    results$outcome <- outcome
    
    # Store the results in the appropriate results storage (e.g., results_p18w, results_p3y, etc.)
    results_storage[i, ] <- results
  }
  
  # Set column names for the final results storage
  colnames(results_storage) <- colnames(results)
  
  return(results_storage)
}

res <- list()

res$mat_diet_INTER <- run_analysis(
  outcome_list = outcome_list, 
  new_imp_rf = new_imp_rf, 
  exposure = "preg_dep_bin",
  variable = "med_diet_mat", 
  term = "preg_dep_bin1: Depression:med_diet_mat", 
  results_storage = results_list$mat_diet, 
  covariates = covariates
)

res$friendship_INTER <- run_analysis(
  outcome_list = outcome_list, 
  new_imp_rf = new_imp_rf,
  exposure = "preg_dep_bin",
  variable = "friendship_z", 
  term = "preg_dep_bin1: Depression:friendship_z", 
  results_storage = results_list$friendship, 
  covariates = covariates
)

res$child_diet_INTER <- run_analysis(
  outcome_list = outcome_list, 
  new_imp_rf = new_imp_rf, 
  exposure = "preg_dep_bin",
  variable = "med_diet_child", 
  term = "preg_dep_bin1: Depression:med_diet_child", 
  results_storage = results_list$child_diet, 
  covariates = covariates
)

res$p18w_INTER <- run_analysis(
  outcome_list = outcome_list, 
  new_imp_rf = new_imp_rf, 
  exposure = "preg_dep_bin",
  variable = "p_dep_18wg_bin", 
  term = "preg_dep_bin1: Depression:p_dep_18wg_bin1: Depression", 
  results_storage = results_list$p18w, 
  covariates = covariates
)

res$p3y_INTER <- run_analysis(
  outcome_list = outcome_list, 
  new_imp_rf = new_imp_rf, 
  exposure = "preg_dep_bin",
  variable = "p_dep_3y_bin", 
  term = "preg_dep_bin1: Depression:p_dep_3y_bin1: Depression", 
  results_storage = results_list$p3y, 
  covariates = covariates
)

res$preLE_INTER <- run_analysis(
  outcome_list = outcome_list, 
  new_imp_rf = new_imp_rf, 
  exposure = "preg_dep_bin",
  variable = "pre_LE_domain_score_z", 
  term = "preg_dep_bin1: Depression:pre_LE_domain_score_z", 
  results_storage = results_list$preLE, 
  covariates = covariates
)

res$posLE_INTER <- run_analysis(
  outcome_list = outcome_list, 
  new_imp_rf = new_imp_rf, 
  exposure = "preg_dep_bin",
  variable = "pos_LE_domain_score_z", 
  term = "preg_dep_bin1: Depression:pos_LE_domain_score_z", 
  results_storage = results_list$posLE, 
  covariates = covariates
)

res$preIR_INTER <- run_analysis(
  outcome_list = outcome_list, 
  new_imp_rf = new_imp_rf, 
  exposure = "preg_dep_bin",
  variable = "pre_IR_domain_score_z", 
  term = "preg_dep_bin1: Depression:pre_IR_domain_score_z", 
  results_storage = results_list$preIR, 
  covariates = covariates
)

res$posIR_INTER <- run_analysis(
  outcome_list = outcome_list, 
  new_imp_rf = new_imp_rf, 
  exposure = "preg_dep_bin",
  variable = "pos_IR_domain_score_z", 
  term = "preg_dep_bin1: Depression:pos_IR_domain_score_z", 
  results_storage = results_list$posIR, 
  covariates = covariates
)

res$posDV_INTER <- run_analysis(
  outcome_list = outcome_list, 
  new_imp_rf = new_imp_rf, 
  exposure = "preg_dep_bin",
  variable = "pos_DV_domain_score_z", 
  term = "preg_dep_bin1: Depression:pos_DV_domain_score_z", 
  results_storage = results_list$posDV, 
  covariates = covariates
)

res$preCR_INTER <- run_analysis(
  outcome_list = outcome_list, 
  new_imp_rf = new_imp_rf, 
  exposure = "preg_dep_bin",
  variable = "pre_CR_domain_score_z", 
  term = "preg_dep_bin1: Depression:pre_CR_domain_score_z", 
  results_storage = results_list$preCR, 
  covariates = c("assigned_sex", "outcome_age", "preg_alc", "preg_smk", 
                 "m_age_birth", "prepreg_BMI", "natorig") # excl. edu_cat which is part of CR
)

res$posCR_INTER <- run_analysis(
  outcome_list = outcome_list, 
  new_imp_rf = new_imp_rf, 
  exposure = "preg_dep_bin",
  variable = "pos_CR_domain_score_z", 
  term = "preg_dep_bin1: Depression:pos_CR_domain_score_z", 
  results_storage = results_list$posCR, 
  covariates = c("assigned_sex", "outcome_age", "preg_alc", "preg_smk", 
                 "m_age_birth", "prepreg_BMI", "natorig") # excl. edu_cat which is part of CR
)

res$alcohol_INTER <- run_analysis(
  outcome_list = outcome_list, 
  new_imp_rf = new_imp_rf, 
  exposure = "preg_dep_bin",
  variable = "preg_alc", 
  term = "preg_dep_bin1: Depression:preg_alc1: Any alcohol", 
  results_storage = results_list$alcohol, 
  covariates = c("assigned_sex","outcome_age","preg_smk","m_age_birth",
                 "prepreg_BMI","natorig","edu_cat") # exclu. preg_alc since it's included
)

res$smoking_INTER <- run_analysis(
  outcome_list = outcome_list, 
  new_imp_rf = new_imp_rf, 
  exposure = "preg_dep_bin",
  variable = "preg_smk", 
  term = "preg_dep_bin1: Depression:preg_smk1: Any smoking", 
  results_storage = results_list$smoking, 
  covariates = c("assigned_sex","outcome_age","preg_alc","m_age_birth",
                 "prepreg_BMI","natorig","edu_cat") # exclu. preg_smk since it's included
)

saveRDS(res, file=file.path(projectpath,"output", 
                            paste0('res_YSR_imputed_GenR',
                                   as.character(Sys.Date()),".Rdata")))



#----------------------------SENSTIVITY: OUTLIERS-------------------------------
covariates <- c("assigned_sex","outcome_age","preg_alc","preg_smk","m_age_birth",
                "prepreg_BMI","natorig","edu_cat")
outcome_list <- c('ADHD_z', 'inter_z', 'exter_z')

# Create empty results dfs for all moderators
result_names <- c("friendship", "alcohol", "smoking", "mat_diet", "child_diet", 
                  "preLE", "preCR", "preIR", "posLE", "posCR", "posIR", "posDV", 
                  "p18w", "p3y")
results_list <- setNames(vector("list", length(result_names)), result_names)

# Create the list with data frames
for (name in result_names) {
  results_list[[name]] <- data.frame(matrix(NA, nrow = 3, ncol = 8))
}

outlier_analysis <- function(outcome_list, new_imp_rf, exposure, variable, term, 
                         results_storage, covariates) {
  # Loop through all outcomes in outcome_list
  for (i in 1:3) {
    outcome <- outcome_list[i]
    
    # Construct the covariate part of the formula
    covariate_formula <- paste(covariates, collapse = " + ")
    
    # Create the formula dynamically based on the variable (e.g., "p_dep_18wg", "preg_alc", etc.)
    formula <- paste0(outcome, " ~ ",exposure,"*", variable, " + ", covariate_formula)
    
    # Run the model
    mod1_f <- with(new_imp_rf, lm(as.formula(formula)))
    
    # Outliers
    models <- mod1_f$analyses
    out_list <- lapply(models, car::outlierTest)             # outliers per imp.
    m      <- length(models)
    n      <- nrow(mice::complete(new_imp_rf, 1))           # number of original rows
    flag   <- matrix(FALSE, nrow = n, ncol = m)
    
    for (j in seq_len(m)) {
      mod      <- models[[j]]
      r        <- rstudent(mod)
      p        <- 2 * pt(-abs(r), df = df.residual(mod))
      p_bonf   <- p.adjust(p, method = "bonferroni")
      flag[ , j] <- p_bonf < 0.05               # logical: outlier in imputation j
    }
    
    outlier_counts <- rowSums(flag)
    # e.g. define “problematic” as flagged in >= 50% of imputations:
    #problem_ids <- which(outlier_counts >= (0.5 * m))
    problem_ids <- which(outlier_counts >= 1)
    
    include <- !(seq_len(nrow(mice::complete(new_imp_rf, 1))) %in% problem_ids)
    new_imp_rf_clean <- mice::filter(new_imp_rf, include)
    
    mod1_f <- with(new_imp_rf_clean, lm(as.formula(formula)))
    
    pooled_mod1_f <- mice::pool(mod1_f)
    
    # Get the summary of the pooled model
    output <- summary(pooled_mod1_f, conf.int = TRUE)
    results <- as.data.frame(output[c("term", "estimate","std.error","p.value","2.5 %","97.5 %")])
    
    # Extract the specific term of interest
    results$term <- as.character(results$term)
    results <- results[results$term == term, ]
    results <- as.data.frame(results[c("term", "estimate","std.error","p.value","2.5 %","97.5 %")])
    
    # Add number of outliers and outcome variable to the results
    results$num_outliers <- length(problem_ids)
    results$outcome <- outcome
    row.names(results) <- NULL
    
    # Store the results in the appropriate results storage (e.g., results_p18w, results_p3y, etc.)
    results_storage[i, ] <- results
  }
  
  # Set column names for the final results storage
  colnames(results_storage) <- colnames(results)
  
  return(list(
    results_storage = results_storage
  ))
}

res <- list()

res$mat_diet_OUTLIERS <- outlier_analysis(
  outcome_list = outcome_list, 
  new_imp_rf = new_imp_rf, 
  exposure = "preg_dep_bin",
  variable = "med_diet_mat", 
  term = "preg_dep_bin1: Depression:med_diet_mat", 
  results_storage = results_list$mat_diet, 
  covariates = covariates
)

res$friendship_OUTLIERS <- outlier_analysis(
  outcome_list = outcome_list, 
  new_imp_rf = new_imp_rf,
  exposure = "preg_dep_bin",
  variable = "friendship_z", 
  term = "preg_dep_bin1: Depression:friendship_z", 
  results_storage = results_list$friendship, 
  covariates = covariates
)

res$child_diet_OUTLIERS <- outlier_analysis(
  outcome_list = outcome_list, 
  new_imp_rf = new_imp_rf, 
  exposure = "preg_dep_bin",
  variable = "med_diet_child", 
  term = "preg_dep_bin1: Depression:med_diet_child", 
  results_storage = results_list$child_diet, 
  covariates = covariates
)

res$p18w_OUTLIERS <- outlier_analysis(
  outcome_list = outcome_list, 
  new_imp_rf = new_imp_rf, 
  exposure = "preg_dep_bin",
  variable = "p_dep_18wg_bin", 
  term = "preg_dep_bin1: Depression:p_dep_18wg_bin1: Depression", 
  results_storage = results_list$p18w, 
  covariates = covariates
)

res$p3y_OUTLIERS <- outlier_analysis(
  outcome_list = outcome_list, 
  new_imp_rf = new_imp_rf, 
  exposure = "preg_dep_bin",
  variable = "p_dep_3y_bin", 
  term = "preg_dep_bin1: Depression:p_dep_3y_bin1: Depression", 
  results_storage = results_list$p3y, 
  covariates = covariates
)

res$preLE_OUTLIERS <- outlier_analysis(
  outcome_list = outcome_list, 
  new_imp_rf = new_imp_rf, 
  exposure = "preg_dep_bin",
  variable = "pre_LE_domain_score_z", 
  term = "preg_dep_bin1: Depression:pre_LE_domain_score_z", 
  results_storage = results_list$preLE, 
  covariates = covariates
)

res$posLE_OUTLIERS <- outlier_analysis(
  outcome_list = outcome_list, 
  new_imp_rf = new_imp_rf, 
  exposure = "preg_dep_bin",
  variable = "pos_LE_domain_score_z", 
  term = "preg_dep_bin1: Depression:pos_LE_domain_score_z", 
  results_storage = results_list$posLE, 
  covariates = covariates
)

res$preIR_OUTLIERS <- outlier_analysis(
  outcome_list = outcome_list, 
  new_imp_rf = new_imp_rf, 
  exposure = "preg_dep_bin",
  variable = "pre_IR_domain_score_z", 
  term = "preg_dep_bin1: Depression:pre_IR_domain_score_z", 
  results_storage = results_list$preIR, 
  covariates = covariates
)

res$posIR_OUTLIERS <- outlier_analysis(
  outcome_list = outcome_list, 
  new_imp_rf = new_imp_rf, 
  exposure = "preg_dep_bin",
  variable = "pos_IR_domain_score_z", 
  term = "preg_dep_bin1: Depression:pos_IR_domain_score_z", 
  results_storage = results_list$posIR, 
  covariates = covariates
)

res$posDV_OUTLIERS <- outlier_analysis(
  outcome_list = outcome_list, 
  new_imp_rf = new_imp_rf, 
  exposure = "preg_dep_bin",
  variable = "pos_DV_domain_score_z", 
  term = "preg_dep_bin1: Depression:pos_DV_domain_score_z", 
  results_storage = results_list$posDV, 
  covariates = covariates
)

res$preCR_OUTLIERS <- outlier_analysis(
  outcome_list = outcome_list, 
  new_imp_rf = new_imp_rf, 
  exposure = "preg_dep_bin",
  variable = "pre_CR_domain_score_z", 
  term = "preg_dep_bin1: Depression:pre_CR_domain_score_z", 
  results_storage = results_list$preCR, 
  covariates = c("assigned_sex", "outcome_age", "preg_alc", "preg_smk", 
                 "m_age_birth", "prepreg_BMI", "natorig") # excl. edu_cat which is part of CR
)

res$posCR_OUTLIERS <- outlier_analysis(
  outcome_list = outcome_list, 
  new_imp_rf = new_imp_rf, 
  exposure = "preg_dep_bin",
  variable = "pos_CR_domain_score_z", 
  term = "preg_dep_bin1: Depression:pos_CR_domain_score_z", 
  results_storage = results_list$posCR, 
  covariates = c("assigned_sex", "outcome_age", "preg_alc", "preg_smk", 
                 "m_age_birth", "prepreg_BMI", "natorig") # excl. edu_cat which is part of CR
)

res$alcohol_OUTLIERS <- outlier_analysis(
  outcome_list = outcome_list, 
  new_imp_rf = new_imp_rf, 
  exposure = "preg_dep_bin",
  variable = "preg_alc", 
  term = "preg_dep_bin1: Depression:preg_alc1: Any alcohol", 
  results_storage = results_list$alcohol, 
  covariates = c("assigned_sex","outcome_age","preg_smk","m_age_birth",
                 "prepreg_BMI","natorig","edu_cat") # exclu. preg_alc since it's included
)

res$smoking_OUTLIERS <- outlier_analysis(
  outcome_list = outcome_list, 
  new_imp_rf = new_imp_rf, 
  exposure = "preg_dep_bin",
  variable = "preg_smk", 
  term = "preg_dep_bin1: Depression:preg_smk1: Any smoking", 
  results_storage = results_list$smoking, 
  covariates = c("assigned_sex","outcome_age","preg_alc","m_age_birth",
                 "prepreg_BMI","natorig","edu_cat") # exclu. preg_smk since it's included
)

saveRDS(res, file=file.path(projectpath,"output", 
                            paste0('res_outliers_GenR',
                                   as.character(Sys.Date()),".Rdata")))

