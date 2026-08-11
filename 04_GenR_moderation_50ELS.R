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

#--------------------Include participants with >50pct of ELS--------------------
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
#---------------------------Randomly include one twin---------------------------
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
    
  }
  
  # Set column names for the final results storage
  colnames(results_storage) <- colnames(results)
  
  return(list(
    results_storage = results_storage
  ))
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
                            paste0('res_50ELS_GenR',
                                   as.character(Sys.Date()),".Rdata")))



