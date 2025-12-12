########################################################################
# Title:    Main analyses with complete cases
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
#install.packages("renv")
projectpath <- dirname(file.choose())
rlibpath <- dirname(file.choose())
setwd(projectpath)   # Set your working directory to your project folder
.libPaths(paste0(rlibpath, '/renv/library/R-4.3/x86_64-w64-mingw32'))
#renv::load()
libraries <- c("haven","lme4","dplyr","mice","ggplot2","grid","gridExtra","car",
               "tidyr","stringr","broom.mixed","sjPlot","splines","reghelper",
               "flextable","officer","reshape2","poolr")
invisible(lapply(libraries, require, character.only = T))

# Set working directory for where you keep your data. Choose any file, the goal 
# is to select the correct path
datapath <- dirname(file.choose())
N_total <- list()
descriptives <- list()
complete_datasets <- list()
#-------------------------------DATA PREP GenR----------------------------------
filename <- 'GenR_impset_2025-10-23.Rdata'

load(file.path(datapath, filename))

incomplete_data <- imp_rf$data
N_total$GenR <- nrow(imp_rf$data)

incomplete_data <- incomplete_data %>% 
  dplyr::mutate(preg_dep_bin = dplyr::case_when(
    preg_dep <= 0.75 ~ "0: No depression",
    preg_dep > 0.75 ~ "1: Depression",
    TRUE ~ NA_character_
    ),
    preg_dep_bin = factor(preg_dep_bin)
)

incomplete_data <- incomplete_data %>%
  dplyr::mutate(p_dep_18wg_bin = dplyr::case_when(
    is.na(p_dep_18wg) ~ NA_character_,
    !is.na(p_dep_18wg) & p_dep_18wg <= 0.75  ~ "0: No depression",
    !is.na(p_dep_18wg) & p_dep_18wg > 0.75 ~ "1: Depression",
    TRUE ~ NA_character_
    ),
    p_dep_18wg_bin = factor(p_dep_18wg_bin)
    )


incomplete_data <- incomplete_data %>%
  dplyr::mutate(p_dep_3y_bin = dplyr::case_when(
    is.na(p_dep_3y) ~ NA_character_,
    !is.na(p_dep_3y) & p_dep_3y <= 0.75  ~ "0: No depression",
    !is.na(p_dep_3y) & p_dep_3y > 0.75 ~ "1: Depression",
    TRUE ~ NA_character_
    ),
    p_dep_3y_bin = factor(p_dep_3y_bin)
    )

incomplete_data <- incomplete_data %>%
  dplyr::mutate(prepreg_dep_bin = dplyr::case_when(
    prepreg_dep %in% c(1:2) ~ "0: No prior depression",
    prepreg_dep %in% c(3:4) ~ "1: Prior depression",
    TRUE ~ NA_character_
    )
)

incomplete_data <- incomplete_data %>%
  dplyr::mutate(natorig = dplyr::case_when(
    ETHNMv2 == 1 ~ "1: Dutch",
    ETHNMv2 %in% c(4,7) ~ "2: Turkish-Moroccan",
    ETHNMv2 %in% c(5,6) ~ "3: Surinamese-Antillean",
    ETHNMv2 == 700 ~ "4: European",
    ETHNMv2 %in% c(2,3,200,300,400,500,600,800) ~ "5: Other",
    TRUE ~ NA_character_
    )
)

incomplete_data <- incomplete_data %>%
  dplyr::mutate(edu_cat = dplyr::case_when(
    EDUCM == 5 ~ "1: University degree",
    EDUCM == 4 ~ "2: 'Higher' vocational training",
    EDUCM == 3 ~ "3: Three or more years of secondary school",
    EDUCM %in% c(0,1,2) ~ "4: Primary or no education",
    TRUE ~ NA_character_  # NA if none of the conditions are met
    )
)
incomplete_data <- incomplete_data %>%
  dplyr::mutate(preg_alc = dplyr::case_when(
    mdrink_updated == 0 ~ "0: No alcohol",
    mdrink_updated %in% c(1,2,3,4) ~ "1: Any alcohol",
    TRUE ~ NA_character_
    )
)

incomplete_data <- incomplete_data %>%
  dplyr::mutate(preg_smk = dplyr::case_when(
    smoke_all == 1 ~ "0: Never smoked",
    smoke_all %in% c(2,3) ~ "1: Any smoking",
    TRUE ~ NA_character_
    )
)

incomplete_data <- incomplete_data %>%
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


# Convert to percentiles
incomplete_data$ADHD_pct <- as.numeric(incomplete_data$ADHD_symptoms) %>%
  dplyr::percent_rank()*100
incomplete_data$inter_pct <- as.numeric(incomplete_data$inter_symptoms) %>%
  dplyr::percent_rank()*100
incomplete_data$exter_pct <- as.numeric(incomplete_data$exter_symptoms) %>%
  dplyr::percent_rank()*100
# Convert to z-scores
incomplete_data$preg_dep_z <- as.numeric(incomplete_data$preg_dep) %>%
  scale()
incomplete_data$preg_dep_z <- as.numeric(incomplete_data$preg_dep_z) %>%
  as.vector()
incomplete_data$pre_LE_domain_score_z <- as.numeric(incomplete_data$pre_LE_domain_score) %>%
  scale()
incomplete_data$pre_LE_domain_score_z <- as.numeric(incomplete_data$pre_LE_domain_score_z) %>%
  as.vector()
incomplete_data$pre_CR_domain_score_z <- as.numeric(incomplete_data$pre_CR_domain_score) %>%
  scale()
incomplete_data$pre_CR_domain_score_z <- as.numeric(incomplete_data$pre_CR_domain_score_z) %>%
  as.vector()
incomplete_data$pre_IR_domain_score_z <- as.numeric(incomplete_data$pre_IR_domain_score) %>%
  scale()
incomplete_data$pre_IR_domain_score_z <- as.numeric(incomplete_data$pre_IR_domain_score_z) %>%
  as.vector()
incomplete_data$pos_LE_domain_score_z <- as.numeric(incomplete_data$pos_LE_domain_score) %>%
  scale()
incomplete_data$pos_LE_domain_score_z <- as.numeric(incomplete_data$pos_LE_domain_score_z) %>%
  as.vector()
incomplete_data$pos_CR_domain_score_z <- as.numeric(incomplete_data$pos_CR_domain_score) %>%
  scale()
incomplete_data$pos_CR_domain_score_z <- as.numeric(incomplete_data$pos_CR_domain_score_z) %>%
  as.vector()
incomplete_data$pos_IR_domain_score_z <- as.numeric(incomplete_data$pos_IR_domain_score) %>%
  scale()
incomplete_data$pos_IR_domain_score_z <- as.numeric(incomplete_data$pos_IR_domain_score_z) %>%
  as.vector()
incomplete_data$pos_DV_domain_score_z <- as.numeric(incomplete_data$pos_DV_domain_score) %>%
  scale()
incomplete_data$pos_DV_domain_score_z <- as.numeric(incomplete_data$pos_DV_domain_score_z) %>%
  as.vector()
incomplete_data$p_dep_18wg_z <- as.numeric(incomplete_data$p_dep_18wg) %>%
  scale()
incomplete_data$p_dep_18wg_z <- as.numeric(incomplete_data$p_dep_18wg_z) %>%
  as.vector()
incomplete_data$p_dep_3y_z <- as.numeric(incomplete_data$p_dep_3y) %>%
  scale()
incomplete_data$p_dep_3y_z <- as.numeric(incomplete_data$p_dep_3y_z) %>%
  as.vector()
incomplete_data$ADHD_z <- as.numeric(incomplete_data$ADHD_symptoms) %>%
  scale()
incomplete_data$ADHD_z <- as.numeric(incomplete_data$ADHD_z) %>%
  as.vector()
incomplete_data$inter_z <- as.numeric(incomplete_data$inter_symptoms) %>%
  scale()
incomplete_data$inter_z <- as.numeric(incomplete_data$inter_z) %>%
  as.vector()
incomplete_data$exter_z <- as.numeric(incomplete_data$exter_symptoms) %>%
  scale()
incomplete_data$exter_z <- as.numeric(incomplete_data$exter_z) %>%
  as.vector()
incomplete_data$ADHD_YSR_z <- as.numeric(incomplete_data$ADHD_symptoms_YSR) %>%
  scale()
incomplete_data$ADHD_YSR_z <- as.numeric(incomplete_data$ADHD_YSR_z) %>%
  as.vector()
incomplete_data$inter_YSR_z <- as.numeric(incomplete_data$inter_symptoms_YSR) %>%
  scale()
incomplete_data$inter_YSR_z <- as.numeric(incomplete_data$inter_YSR_z) %>%
  as.vector()
incomplete_data$exter_YSR_z <- as.numeric(incomplete_data$exter_symptoms_YSR) %>%
  scale()
incomplete_data$exter_YSR_z <- as.numeric(incomplete_data$exter_YSR_z) %>%
  as.vector()
incomplete_data$friendship_z <- as.numeric(incomplete_data$friendship_sum) %>%
  scale()
incomplete_data$friendship_z <- as.numeric(incomplete_data$friendship_z) %>%
  as.vector()
incomplete_data$m_age_birth <- as.numeric(incomplete_data$m_age_birth) %>%
  as.vector()
#---------------------Include participants with >10pct data---------------------
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

suf_data <- incomplete_data[incomplete_data$IDC %in% IDs_10pct$IDC_suf,]
incomplete_data <- suf_data
#--------------------------Randomly include one sibling-------------------------
incomplete_data <- incomplete_data %>%
  group_by(IDM) %>%
  slice_sample(n = 1) %>%
  ungroup()

complete_cases <- dplyr::select(incomplete_data, 
                                c("IDC","preg_dep_bin","assigned_sex","edu_cat","m_age_birth",
                                  "natorig","prepreg_BMI","preg_alc","preg_smk",
                                  "friendship_z",
                                  "med_diet_mat","med_diet_child",
                                  "p_dep_18wg_bin","p_dep_3y_bin",
                                  "pre_LE_domain_score_z","pre_CR_domain_score_z",
                                  "pre_IR_domain_score_z","pos_LE_domain_score_z",
                                  "pos_CR_domain_score_z","pos_IR_domain_score_z",
                                  "pos_DV_domain_score_z",
                                  "inter_z","exter_z",
                                  "ADHD_z","outcome_age"))

complete_cases <- complete_cases[complete.cases(complete_cases),]
descriptives$GenR <- incomplete_data
complete_datasets$GenR <- complete_cases
#------------------------------DATA PREP ALSPAC---------------------------------
filename <- 'ALSPAC_impset_2025-10-23.Rdata'

load(file.path(datapath, filename))

incomplete_data <- imp_rf$data
N_total$ALSPAC <- nrow(imp_rf$data)

incomplete_data <- incomplete_data %>%
  mutate(IDM = sub("_.*", "", IDC)
)

incomplete_data <- incomplete_data %>%
  dplyr::mutate(preg_dep = dplyr::case_when(
    is.na(preg_dep18w) & is.na(preg_dep32w) ~ NA_real_, # Both missing → NA
    !is.na(preg_dep18w) &  is.na(preg_dep32w) ~ preg_dep18w, # If 18w available, use this
    is.na(preg_dep18w) &  !is.na(preg_dep32w) ~ preg_dep32w, # If 32w available, use this
    !is.na(preg_dep18w) &  !is.na(preg_dep32w) ~ rowMeans(incomplete_data[,c('preg_dep18w','preg_dep32w')])# Otherwise take mean
    )
)

incomplete_data <- incomplete_data %>%
  dplyr::mutate(preg_dep_bin = dplyr::case_when(
    is.na(preg_dep18w) & is.na(preg_dep32w) ~ NA_character_,
    !is.na(preg_dep18w) & !is.na(preg_dep32w) & preg_dep18w <= 12  &
      preg_dep32w <= 12 ~ "0: No depression",
    !is.na(preg_dep18w) & preg_dep18w > 12 ~ "1: Depression",
    !is.na(preg_dep32w) & preg_dep32w > 12 ~ "1: Depression",
    TRUE ~ NA_character_
    ),
    preg_dep_bin = factor(preg_dep_bin)
)


incomplete_data <- incomplete_data %>%
  dplyr::mutate(p_dep_18wg_bin = dplyr::case_when(
    is.na(p_dep_18wg) ~ NA_character_,
    !is.na(p_dep_18wg) & p_dep_18wg <= 12  ~ "0: No depression",
    !is.na(p_dep_18wg) & p_dep_18wg > 12 ~ "1: Depression",
    TRUE ~ NA_character_
    ),
    p_dep_18wg_bin = factor(p_dep_18wg_bin)
)

incomplete_data <- incomplete_data %>%
  dplyr::mutate(p_dep_3y_bin = dplyr::case_when(
    is.na(p_dep_3y) ~ NA_character_,
    !is.na(p_dep_3y) & p_dep_3y <= 12  ~ "0: No depression",
    !is.na(p_dep_3y) & p_dep_3y > 12 ~ "1: Depression",
    TRUE ~ NA_character_
    ),
    p_dep_3y_bin = factor(p_dep_3y_bin)
)

incomplete_data <- incomplete_data %>%
  dplyr::mutate(prepreg_dep_bin = dplyr::case_when(
    prepreg_dep == 3 ~ "0: No prior depression",
    prepreg_dep %in% c(1:2) ~ "1: Prior depression",
    TRUE ~ NA_character_
    )
)

incomplete_data <- incomplete_data %>%
  dplyr::mutate(edu_cat = dplyr::case_when(
    c645a == 5 ~ "1: University degree",
    c645a %in% c(2:4) ~ "2: Vocational, O level, A level",
    c645a == 1 ~ "3: CSE", 
    TRUE ~ NA_character_  # NA if none of the conditions are met
    )
)

incomplete_data <- incomplete_data %>%
  dplyr::mutate(natorig = dplyr::case_when(
    ethn_group_m == 1 ~ "White",
    ethn_group_m %in% c(2:9) ~ "Non-white",
    TRUE ~ NA_character_  # NA if none of the conditions are met
    )
)

incomplete_data <- incomplete_data %>%
  dplyr::mutate(preg_alc = dplyr::case_when(
    alc_t1 == 1 & alc_t3 == 1 ~ "0: No alcohol",
    alc_t1 %in% c(2:6) | alc_t3 %in% c(2:6) ~ "1: Any alcohol",
    TRUE ~ NA_character_
    )
)

incomplete_data <- incomplete_data %>%
  dplyr::mutate(preg_smk = dplyr::case_when(
    b670 == 0 | b671 == 0 | c482 == 0 ~ "0: Never smoked",
    as.numeric(b670) >= 1 | as.numeric(b671) >= 1 | as.numeric(c482) >= 1 ~ "1: Any smoking",
    TRUE ~ NA_character_
    )
)

incomplete_data <- incomplete_data %>%
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


# Convert to percentiles
incomplete_data$ADHD_pct <- as.numeric(incomplete_data$ADHD_symptoms) %>%
  dplyr::percent_rank()*100
incomplete_data$inter_pct <- as.numeric(incomplete_data$inter_symptoms) %>%
  dplyr::percent_rank()*100
incomplete_data$exter_pct <- as.numeric(incomplete_data$exter_symptoms) %>%
  dplyr::percent_rank()*100
# Convert to z-scores
incomplete_data$preg_dep_z <- as.numeric(incomplete_data$preg_dep) %>%
  scale()
incomplete_data$preg_dep_z <- as.numeric(incomplete_data$preg_dep_z) %>%
  as.vector()
incomplete_data$pre_LE_domain_score_z <- as.numeric(incomplete_data$pre_LE_domain_score) %>%
  scale()
incomplete_data$pre_LE_domain_score_z <- as.numeric(incomplete_data$pre_LE_domain_score_z) %>%
  as.vector()
incomplete_data$pre_CR_domain_score_z <- as.numeric(incomplete_data$pre_CR_domain_score) %>%
  scale()
incomplete_data$pre_CR_domain_score_z <- as.numeric(incomplete_data$pre_CR_domain_score_z) %>%
  as.vector()
incomplete_data$pre_IR_domain_score_z <- as.numeric(incomplete_data$pre_IR_domain_score) %>%
  scale()
incomplete_data$pre_IR_domain_score_z <- as.numeric(incomplete_data$pre_IR_domain_score_z) %>%
  as.vector()
incomplete_data$pos_LE_domain_score_z <- as.numeric(incomplete_data$pos_LE_domain_score) %>%
  scale()
incomplete_data$pos_LE_domain_score_z <- as.numeric(incomplete_data$pos_LE_domain_score_z) %>%
  as.vector()
incomplete_data$pos_CR_domain_score_z <- as.numeric(incomplete_data$pos_CR_domain_score) %>%
  scale()
incomplete_data$pos_CR_domain_score_z <- as.numeric(incomplete_data$pos_CR_domain_score_z) %>%
  as.vector()
incomplete_data$pos_IR_domain_score_z <- as.numeric(incomplete_data$pos_IR_domain_score) %>%
  scale()
incomplete_data$pos_IR_domain_score_z <- as.numeric(incomplete_data$pos_IR_domain_score_z) %>%
  as.vector()
incomplete_data$pos_DV_domain_score_z <- as.numeric(incomplete_data$pos_DV_domain_score) %>%
  scale()
incomplete_data$pos_DV_domain_score_z <- as.numeric(incomplete_data$pos_DV_domain_score_z) %>%
  as.vector()
incomplete_data$p_dep_18wg_z <- as.numeric(incomplete_data$p_dep_18wg) %>%
  scale()
incomplete_data$p_dep_18wg_z <- as.numeric(incomplete_data$p_dep_18wg_z) %>%
  as.vector()
incomplete_data$p_dep_3y_z <- as.numeric(incomplete_data$p_dep_3y) %>%
  scale()
incomplete_data$p_dep_3y_z <- as.numeric(incomplete_data$p_dep_3y_z) %>%
  as.vector()
incomplete_data$ADHD_z <- as.numeric(incomplete_data$ADHD_symptoms) %>%
  scale()
incomplete_data$ADHD_z <- as.numeric(incomplete_data$ADHD_z) %>%
  as.vector()
incomplete_data$inter_z <- as.numeric(incomplete_data$inter_symptoms) %>%
  scale()
incomplete_data$inter_z <- as.numeric(incomplete_data$inter_z) %>%
  as.vector()
incomplete_data$exter_z <- as.numeric(incomplete_data$exter_symptoms) %>%
  scale()
incomplete_data$exter_z <- as.numeric(incomplete_data$exter_z) %>%
  as.vector()
incomplete_data$friendship_z <- as.numeric(incomplete_data$friendship) %>%
  scale()
incomplete_data$friendship_z <- as.numeric(incomplete_data$friendship_z) %>%
  as.vector()
incomplete_data$m_age_birth <- as.numeric(incomplete_data$m_age_birth) %>%
  as.vector()
#---------------------Include participants with >10pct data---------------------
check_df <- dplyr::select(incomplete_data, c("IDC","preg_dep18w","preg_dep32w","assigned_sex","c645a",
                                             "m_age_birth","ethn_group_m","prepreg_BMI", "alc_t1",
                                             "alc_t3","b670","b671","c482","friendship","med_diet_mat",
                                             "med_diet_child","p_dep_18wg","p_dep_3y","pre_LE_domain_score",
                                             "pre_CR_domain_score","pre_IR_domain_score","pos_LE_domain_score",
                                             "pos_CR_domain_score","pos_IR_domain_score","pos_DV_domain_score",
                                             "inter_symptoms","exter_symptoms","ADHD_symptoms","outcome_age"))

threshold <- 0.1  # 10%
n_columns <- ncol(check_df) - 1  # Subtract 1 for the ID column
min_non_na <- ceiling(threshold * n_columns)  # Round up to the nearest whole number

# Count non-NA values per row, excluding the ID column
rows_with_10pct_data <- rowSums(!is.na(check_df[, -which(names(check_df) == "IDC")])) >= min_non_na

# Extract IDs of those rows
IDs_10pct <- as.data.frame(check_df[rows_with_10pct_data, "IDC"])
names(IDs_10pct) <- 'IDC_suf'

suf_data <- incomplete_data[incomplete_data$IDC %in% IDs_10pct$IDC_suf,]
incomplete_data <- suf_data
#--------------------------Randomly include one sibling-------------------------
incomplete_data <- incomplete_data %>%
  group_by(IDM) %>%
  slice_sample(n = 1) %>%
  ungroup()

complete_cases <- dplyr::select(incomplete_data, 
                                c("IDC","preg_dep_bin","assigned_sex","edu_cat","m_age_birth",
                                  "natorig","prepreg_BMI","preg_alc","preg_smk",
                                  "friendship_z",
                                  "med_diet_mat","med_diet_child",
                                  "p_dep_18wg_bin","p_dep_3y_bin",
                                  "pre_LE_domain_score_z","pre_CR_domain_score_z",
                                  "pre_IR_domain_score_z","pos_LE_domain_score_z",
                                  "pos_CR_domain_score_z","pos_IR_domain_score_z",
                                  "pos_DV_domain_score_z",
                                  "inter_z","exter_z",
                                  "ADHD_z","outcome_age"))

complete_cases <- complete_cases[complete.cases(complete_cases),]
descriptives$ALSPAC <- incomplete_data
complete_datasets$ALSPAC <- complete_cases
#-------------------------------Correlation plots-------------------------------
numeric_GenR <- dplyr::select(descriptives$GenR, c("preg_dep","m_age_birth","prepreg_BMI",
                                                 "friendship_z","med_diet_mat",
                                                 "med_diet_child","p_dep_18wg",
                                                 "p_dep_3y","pre_LE_domain_score",
                                                 "pre_CR_domain_score","pre_IR_domain_score",
                                                 "pos_LE_domain_score","pos_CR_domain_score",
                                                 "pos_IR_domain_score","pos_DV_domain_score",
                                                 "inter_symptoms","exter_symptoms",
                                                 "ADHD_symptoms","outcome_age",
                                                 "inter_symptoms_YSR","exter_symptoms_YSR",
                                                 "ADHD_symptoms_YSR","outcome_age_YSR"))

# Calculate number of effective tests
eff_GenR <- dplyr::select(descriptives$GenR, c("inter_symptoms","exter_symptoms","ADHD_symptoms"))

#eff_GenR$preg_alc_num <- as.numeric(factor(eff_GenR$preg_alc))
#eff_GenR$preg_smk_num <- as.numeric(factor(eff_GenR$preg_smk))

meff_df_GenR <- meff(cor(eff_GenR, use = 'pairwise.complete.obs'), method = 'galwey')

cormat_genr <- round(cor(numeric_GenR, use = "pairwise.complete.obs"),2)

full_names <- c("Pren. mat. depression","Mat. age at childbirth","Pre-pregnancy BMI",
                "Child friendship score","Pren. mat. diet","Child diet",
                "Pren. partner depression","Post. partner depression","Pren. life events",
                "Pren. contextual risk","Pren interpersonal risk","Post. life events",
                "Post. contextual risk","Post interpersonal risk","Post. direct vict.",
                "Inter. symptoms","Exter. symptoms","ADHD symptoms","Child age at outcome",
                "Inter. symptoms (YSR)","Exter. symptoms (YSR)","ADHD symptoms (YSR)",
                "Child age at outcome (YSR)")
colnames(cormat_genr) <- full_names
rownames(cormat_genr) <- full_names

cormat_genr[upper.tri(cormat_genr, diag = TRUE)] <- NA  # Removes top triangle and diagonal
df <- melt(cormat_genr, na.rm = TRUE)

df$Var1 <- factor(df$Var1, levels = colnames(cormat_genr))
df$Var2 <- factor(df$Var2, levels = rownames(cormat_genr))

jpeg(file=file.path('output/cormat_GenR.jpeg'),
     width=800, height=800, pointsize = 10, quality = 500)
ggplot(df, aes(Var2, Var1, fill = value, label = round(value, 2))) +
  geom_tile(color = "white") +
  geom_text(size = 4) +
  scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0, limits = c(-1, 1)) +
  theme_minimal() +
  labs(x = NULL, y = NULL, fill = "Correlation") +
  coord_flip() +
  theme(
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  )
dev.off()

numeric_ALSP <- dplyr::select(descriptives$ALSPAC, c("preg_dep","m_age_birth","prepreg_BMI",
                                                   "friendship_z","med_diet_mat",
                                                   "med_diet_child","p_dep_18wg",
                                                   "p_dep_3y","pre_LE_domain_score",
                                                   "pre_CR_domain_score","pre_IR_domain_score",
                                                   "pos_LE_domain_score","pos_CR_domain_score",
                                                   "pos_IR_domain_score","pos_DV_domain_score",
                                                   "inter_symptoms","exter_symptoms",
                                                   "ADHD_symptoms","outcome_age"))

# Calculate number of effective tests
eff_ALSP <- dplyr::select(descriptives$ALSPAC, c("inter_symptoms","exter_symptoms","ADHD_symptoms"))

#eff_ALSP$preg_alc_num <- as.numeric(factor(eff_ALSP$preg_alc))
#eff_ALSP$preg_smk_num <- as.numeric(factor(eff_ALSP$preg_smk))

meff_df_ALSP <- meff(cor(eff_ALSP, use = 'pairwise.complete.obs'), method = 'galwey')

cormat_alsp <- round(cor(numeric_ALSP, use = "pairwise.complete.obs"),2)
full_names <- c("Pren. mat. depression","Mat. age at childbirth","Pre-pregnancy BMI",
                "Child friendship score","Pren. mat. diet","Child diet",
                "Pren. partner depression","Post. partner depression","Pren. life events",
                "Pren. contextual risk","Pren interpersonal risk","Post. life events",
                "Post. contextual risk","Post interpersonal risk","Post. direct vict.",
                "Inter. symptoms","Exter. symptoms","ADHD symptoms","Child age at outcome")
colnames(cormat_alsp) <- full_names
rownames(cormat_alsp) <- full_names

cormat_alsp[upper.tri(cormat_alsp, diag = TRUE)] <- NA  # Removes top triangle and diagonal
df <- melt(cormat_alsp, na.rm = TRUE)

df$Var1 <- factor(df$Var1, levels = colnames(cormat_alsp))
df$Var2 <- factor(df$Var2, levels = rownames(cormat_alsp))

jpeg(file=file.path('output/cormat_ALSPAC.jpeg'),
     width=800, height=800, pointsize = 10, quality = 500)
ggplot(df, aes(Var2, Var1, fill = value, label = round(value, 2))) +
  geom_tile(color = "white") +
  geom_text(size = 4) +
  scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0, limits = c(-1, 1)) +
  theme_minimal() +
  labs(x = NULL, y = NULL, fill = "Correlation") +
  coord_flip() +
  theme(
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  )
dev.off()

#-------------------------------Table 1 full sample-----------------------------
make_descriptives_table <- function(descriptives, cohorts) {
  
  get_n_pct_total <- function(cohort) {
    total_val <- N_total[[cohort]]
    pct <- if (total_val > 0) round(n_val / total_val * 100, 1) else NA
    sprintf("%d (%.1f%%)", n_val, pct)
  }
  
  get_count_pct_with_missing <- function(variable, cohort, code) {
    
    total_val <- N_total[[cohort]] 
    
    # Count for codes of interest
    count <- sum(cohort_data[[variable]]== code,na.rm = T)
    table(cohort_data[[variable]])
    
    n_missing <- sum(is.na(cohort_data[[variable]]))

    # Percentages
    pct <- if (total_val > 0) round(count / total_val * 100, 1) else NA
    miss_pct <- if (total_val > 0) round(n_missing / total_val * 100, 1) else NA
    
    sprintf("%d (%.1f%%)", count, pct)
  }
  
  get_mean_sd <- function(variable, cohort) {
    total_val <- N_total[[cohort]] 
    
    mean_val <- mean(as.numeric(descriptives[[cohort]][[variable]]), na.rm = T)
    sd_val <- sd(as.numeric(descriptives[[cohort]][[variable]]), na.rm = T)
    
    n_missing <- sum(is.na(cohort_data[[variable]]))
    miss_pct <- if (total_val > 0) round(n_missing / total_val * 100, 1) else NA
    
    if (is.na(mean_val) || is.na(sd_val)) return(NA_character_)
    sprintf("%.1f (%.1f)", mean_val, sd_val)
  }
  
  # Define row structure with section headers and variables
  row_labels <- c(
    "n (% of original sample)",
    "Male, n (%)",
    "Female, n (%)",
    "Maternal age at childbirth, years, mean (SD)",
    "University degree, n (%)",
    "'Higher' vocational training, n (%)",
    "Three or more years of secondary school, n (%)",
    "Primary or no education, n (%)",
    "Vocational, O level, A level, n (%)",
    "CSE, n (%)",
    "Dutch, n (%)",
    "Surinamese-Antillean, n (%)",
    "Turkish-Moroccan, n (%)",
    "Non-Dutch European, n (%)",
    "Other, n (%)",
    "White, n (%)",
    "Non-white, n (%)",
    "Prenatal maternal depression, n (%)",
    "No prenatal maternal depression, n (%)",
    "Prenatal partner depression, n (%)",
    "No prenatal partner depression, n (%)",
    "Postnatal partner depression, n (%)",
    "No postnatal partner depression, n (%)",
    "Pre-pregnancy depression, n (%)",
    "No pre-pregnancy depression, n (%)",
    "Any alcohol use in pregnancy, yes, n (%)",
    "No alcohol use in pregnancy, yes, n (%)",
    "Any smoking in pregnancy, yes, n (%)",
    "No smoking in pregnancy, yes, n (%)",
    "Pre-pregnancy BMI, mean (SD)",
    "Maternal Mediterranean diet, mean (SD)",
    "Child Mediterranean diet, mean (SD)",
    "Friendship z-score  child, mean (SD)",
    "Prenatal life events, mean (SD)",
    "Prenatal contextual risk, mean (SD)",
    "Prenatal interpersonal risk, mean (SD)",
    "Postnatal life events, mean (SD)",
    "Postnatal contextual risk, mean (SD)",
    "Postnatal interpersonal risk, mean (SD)",
    "Postnatal direct victimization, mean (SD)",
    "Internalising symptoms z-score, mean (SD)",
    "Externalising symptoms z-score, mean (SD)",
    "ADHD symptoms z-score, mean (SD)",
    "Age at outcome, mean (SD)",
    "Internalising z-score (YSR), mean (SD)",
    "Externalising z-score (YSR), mean (SD)",
    "ADHD z-score (YSR), mean (SD)",
    "Age at outcome (YSR), mean (SD)"
  )
  
  results <- matrix(NA_character_, nrow = length(row_labels), ncol = length(cohorts),
                    dimnames = list(row_labels, cohorts))
  
  for (cohort in cohorts) {
    cohort_data <- descriptives[[cohort]]
    n_val <- nrow(cohort_data)
    
    results["n (% of original sample)", cohort] <- get_n_pct_total(cohort)
    results["Male, n (%)", cohort] <- get_count_pct_with_missing("assigned_sex", cohort, "1")
    results["Female, n (%)", cohort] <- get_count_pct_with_missing("assigned_sex", cohort, "2")
    results["Maternal age at childbirth, years, mean (SD)", cohort] <- get_mean_sd("m_age_birth", cohort)
    results["University degree, n (%)", cohort] <- get_count_pct_with_missing("edu_cat", cohort, "1: University degree")
    if (cohort == 'GenR')
    {
      results["'Higher' vocational training, n (%)", cohort] <- get_count_pct_with_missing("edu_cat", cohort, "2: 'Higher' vocational training")
      results["Three or more years of secondary school, n (%)", cohort] <- get_count_pct_with_missing("edu_cat", cohort, "3: Three or more years of secondary school")
      results["Primary or no education, n (%)", cohort] <- get_count_pct_with_missing("edu_cat", cohort, "4: Primary or no education")
      results["Dutch, n (%)", cohort] <- get_count_pct_with_missing("natorig", cohort, "1: Dutch")
      results["Surinamese-Antillean, n (%)", cohort] <- get_count_pct_with_missing("natorig", cohort, "2: Turkish-Moroccan")
      results["Turkish-Moroccan, n (%)", cohort] <- get_count_pct_with_missing("natorig", cohort, "3: Surinamese-Antillean")
      results["Non-Dutch European, n (%)", cohort] <- get_count_pct_with_missing("natorig", cohort, "4: European")
      results["Other, n (%)", cohort] <- get_count_pct_with_missing("natorig", cohort, "5: Other")
      results["Internalising z-score (YSR), mean (SD)", cohort] <- get_mean_sd("inter_YSR_z", cohort)
      results["Externalising z-score (YSR), mean (SD)", cohort] <- get_mean_sd("exter_YSR_z", cohort)
      results["ADHD z-score (YSR), mean (SD)", cohort] <- get_mean_sd("ADHD_YSR_z", cohort)
      results["Age at outcome (YSR), mean (SD)", cohort] <- get_mean_sd("outcome_age_YSR", cohort)
      
    }
    if (cohort == 'ALSPAC')
    {
      results["Vocational, O level, A level, n (%)", cohort] <- get_count_pct_with_missing("edu_cat", cohort, "2: Vocational, O level, A level")
      results["CSE, n (%)", cohort] <- get_count_pct_with_missing("edu_cat", cohort, "3: CSE")
      results["White, n (%)", cohort] <- get_count_pct_with_missing("natorig", cohort, "White")
      results["Non-white, n (%)", cohort] <- get_count_pct_with_missing("natorig", cohort, "Non-white")
      }
    results["Prenatal maternal depression, n (%)", cohort] <- get_count_pct_with_missing("preg_dep_bin", cohort, "1: Depression")
    results["No prenatal maternal depression, n (%)", cohort] <- get_count_pct_with_missing("preg_dep_bin", cohort, "0: No depression")
    results["Prenatal partner depression, n (%)", cohort] <- get_count_pct_with_missing("p_dep_18wg_bin", cohort, "1: Depression")
    results["No prenatal partner depression, n (%)", cohort] <- get_count_pct_with_missing("p_dep_18wg_bin", cohort, "0: No depression")
    results["Postnatal partner depression, n (%)", cohort] <- get_count_pct_with_missing("p_dep_3y_bin", cohort, "1: Depression")
    results["No postnatal partner depression, n (%)", cohort] <- get_count_pct_with_missing("p_dep_3y_bin", cohort, "0: No depression")
    results["Pre-pregnancy depression, n (%)", cohort] <- get_count_pct_with_missing("prepreg_dep_bin", cohort, "1: Prior depression")
    results["No pre-pregnancy depression, n (%)", cohort] <- get_count_pct_with_missing("prepreg_dep_bin", cohort, "0: No prior depression")
    results["Any alcohol use in pregnancy, yes, n (%)", cohort] <- get_count_pct_with_missing("preg_alc", cohort, "1: Any alcohol")
    results["No alcohol use in pregnancy, yes, n (%)", cohort] <- get_count_pct_with_missing("preg_alc", cohort, "0: No alcohol")
    results["Any smoking in pregnancy, yes, n (%)", cohort] <- get_count_pct_with_missing("preg_smk", cohort, "1: Any smoking")
    results["No smoking in pregnancy, yes, n (%)", cohort] <- get_count_pct_with_missing("preg_smk", cohort, "0: Never smoked")
    results["Pre-pregnancy BMI, mean (SD)", cohort] <- get_mean_sd("prepreg_BMI", cohort)
    results["Maternal Mediterranean diet, mean (SD)", cohort] <- get_mean_sd("med_diet_mat", cohort)
    results["Child Mediterranean diet, mean (SD)", cohort] <- get_mean_sd("med_diet_child", cohort)
    results["Friendship z-score  child, mean (SD)", cohort] <- get_mean_sd("friendship_z", cohort)
    results["Prenatal life events, mean (SD)", cohort] <- get_mean_sd("pre_LE_domain_score_z", cohort)
    results["Prenatal contextual risk, mean (SD)", cohort] <- get_mean_sd("pre_CR_domain_score_z", cohort)
    results["Prenatal interpersonal risk, mean (SD)", cohort] <- get_mean_sd("pre_IR_domain_score_z", cohort)
    results["Postnatal life events, mean (SD)", cohort] <- get_mean_sd("pos_LE_domain_score_z", cohort)
    results["Postnatal contextual risk, mean (SD)", cohort] <- get_mean_sd("pos_CR_domain_score_z", cohort)
    results["Postnatal interpersonal risk, mean (SD)", cohort] <- get_mean_sd("pos_IR_domain_score_z", cohort)
    results["Postnatal direct victimization, mean (SD)", cohort] <- get_mean_sd("pos_DV_domain_score_z", cohort)
    results["Internalising symptoms z-score, mean (SD)", cohort] <- get_mean_sd("inter_z", cohort)
    results["Externalising symptoms z-score, mean (SD)", cohort] <- get_mean_sd("exter_z", cohort)
    results["ADHD symptoms z-score, mean (SD)", cohort] <- get_mean_sd("ADHD_z", cohort)
    results["Age at outcome, mean (SD)", cohort] <- get_mean_sd("outcome_age", cohort)
    
  }
  as.data.frame(results, stringsAsFactors = FALSE)
}

# --- Generate and format table ---
cohorts <- sort(c("ALSPAC","GenR"))
desc_table <- make_descriptives_table(descriptives, cohorts)

# Add rownames as a column
desc_table$Value <- c(
  "(% of original sample)",
  "Male, n (%)",
  "Female, n (%)",
  "Mean (SD)",
  "University degree, n (%)",
  "'Higher' vocational training, n (%)",
  "Three or more years of secondary school, n (%)",
  "Primary or no education, n (%)",
  "Vocational, O level, A level, n (%)",
  "CSE, n (%)",
  "Dutch, n (%)",
  "Surinamese-Antillean, n (%)",
  "Turkish-Moroccan, n (%)",
  "Non-Dutch European, n (%)",
  "Other, n (%)",
  "White, n (%)",
  "Non-white, n (%)",
  "Depression, n (%)",
  "No depression, n (%)",
  "Depression, n (%)",
  "No depression, n (%)",
  "Depression, n (%)",
  "No depression, n (%)",
  "Depression, n (%)",
  "No depression, n (%)",
  "Any alcohol, n (%)",
  "No alcohol, n (%)",
  "Any smoking, n (%)",
  "No smoking, n (%)",
  "Mean (SD)",
  "Mean (SD)",
  "Mean (SD)",
  "Mean (SD)",
  "Life events, mean (SD)",
  "Contextual risk, mean (SD)",
  "Interpersonal risk, mean (SD)",
  "Life events, mean (SD)",
  "Contextual risk, mean (SD)",
  "Interpersonal risk, mean (SD)",
  "Direct victimization, mean (SD)",
  "Mean (SD)",
  "Mean (SD)",
  "Mean (SD)",
  "Mean (SD)",
  "Mean (SD)",
  "Mean (SD)",
  "Mean (SD)",
  "Mean (SD)"
)
desc_table$Variable <- c(
    "Sample size",
    "Child's assigned sex at birth",
    "",
    "Maternal age at childbirth, years",
    "Maternal education",
    "",
    "",
    "",
    "",
    "",
    "Maternal national origin",
    "",
    "",
    "",
    "",
    "Maternal ethnicity",
    "",
    "Prenatal maternal depression",
    "",
    "Prenatal partner depression",
    "",
    "Postnatal partner depression",
    "",
    "Pre-pregnancy depression",
    "",
    "Alcohol use in pregnancy",
    "",
    "Smoking in pregnancy",
    "",
    "Pre-pregnancy BMI",
    "Maternal Mediterranean diet",
    "Child Mediterranean diet",
    "Friendship z-score  child",
    "Prenatal ELS",
    "",
    "",
    "Postnatal ELS",
    "",
    "",
    "",
    "Internalising symptoms z-score",
    "Externalising symptoms z-score",
    "ADHD symptoms z-score",
    "Age at outcome",
    "Internalising z-score (YSR)",
    "Externalising z-score (YSR)",
    "ADHD z-score (YSR)",
    "Age at outcome (YSR)"
  )

desc_table <- desc_table[, c("Variable", "Value", cohorts)]

save(desc_table, file = file.path(projectpath, 'output/Table_1_project3.Rdata'))

# Missing value proportions
make_missing_table <- function(descriptives, cohorts) {
  
  get_missing <- function(variable, cohort) {
    
    total_val <- N_total[[cohort]] 
    n_missing <- sum(is.na(cohort_data[[variable]]))
    
    # Percentages
    miss_pct <- if (total_val > 0) round(n_missing / total_val * 100, 1) else NA
    
    sprintf("%.1f%% ", miss_pct)
  }
  
  # Define row structure with section headers and variables
  row_labels <- c(
    "child's assigned sex at birth",
    "maternal age at childbirth",
    "maternal education",
    "maternal national origin",
    "maternal ethnicity",
    "prenatal maternal depression",
    "prenatal partner depression",
    "postnatal partner depression",
    "pre-pregnancy depression",
    "alcohol use in pregnancy",
    "smoking in pregnancy",
    "pre-pregnancy BMI",
    "maternal Mediterranean diet",
    "child Mediterranean diet",
    "friendship z-score  child",
    "ELS prenatal life events",
    "ELS prenatal contextual risk",
    "ELS prenatal interpersonal risk",
    "ELS postnatal life events",
    "ELS postnatal contextual risk",
    "ELS postnatal interpersonal risk",
    "ELS postnatal direct victimization",
    "internalising symptoms z-score",
    "externalising symptoms z-score",
    "ADHD symptoms z-score",
    "Age at outcome",
    "internalising z-score (YSR)",
    "externalising z-score (YSR)",
    "ADHD z-score (YSR)",
    "Age at outcome (YSR)"
  )
  
  results <- matrix(NA_character_, nrow = length(row_labels), ncol = length(cohorts),
                    dimnames = list(row_labels, cohorts))
  
  for (cohort in cohorts) {
    cohort_data <- descriptives[[cohort]]
    n_val <- nrow(cohort_data)
    results["child's assigned sex at birth", cohort] <- get_missing("assigned_sex", cohort)
    results["maternal age at childbirth", cohort] <- get_missing("m_age_birth", cohort)
    results["maternal education", cohort] <- get_missing("edu_cat", cohort)
    results["prenatal maternal depression", cohort] <- get_missing("preg_dep_bin", cohort)
    results["prenatal partner depression", cohort] <- get_missing("p_dep_18wg_bin", cohort)
    results["postnatal partner depression", cohort] <- get_missing("p_dep_3y_bin", cohort)
    results["pre-pregnancy depression", cohort] <- get_missing("prepreg_dep_bin", cohort)
    results["alcohol use in pregnancy", cohort] <- get_missing("preg_alc", cohort)
    results["smoking in pregnancy", cohort] <- get_missing("preg_smk", cohort)
    results["pre-pregnancy BMI", cohort] <- get_missing("prepreg_BMI", cohort)
    results["maternal Mediterranean diet", cohort] <- get_missing("med_diet_mat", cohort)
    results["child Mediterranean diet", cohort] <- get_missing("med_diet_child", cohort)
    results["friendship z-score  child", cohort] <- get_missing("friendship_z", cohort)
    results["ELS prenatal life events", cohort] <- get_missing("pre_LE_domain_score_z", cohort)
    results["ELS prenatal contextual risk", cohort] <- get_missing("pre_CR_domain_score_z", cohort)
    results["ELS prenatal interpersonal risk", cohort] <- get_missing("pre_IR_domain_score_z", cohort)
    results["ELS postnatal life events", cohort] <- get_missing("pos_LE_domain_score_z", cohort)
    results["ELS postnatal contextual risk", cohort] <- get_missing("pos_CR_domain_score_z", cohort)
    results["ELS postnatal interpersonal risk", cohort] <- get_missing("pos_IR_domain_score_z", cohort)
    results["ELS postnatal direct victimization", cohort] <- get_missing("pos_DV_domain_score_z", cohort)
    results["internalising symptoms z-score", cohort] <- get_missing("inter_z", cohort)
    results["externalising symptoms z-score", cohort] <- get_missing("exter_z", cohort)
    results["ADHD symptoms z-score", cohort] <- get_missing("ADHD_z", cohort)
    results["Age at outcome", cohort] <- get_missing("outcome_age", cohort)
    if (cohort == 'GenR')
    {
      results["maternal national origin", cohort] <- get_missing("natorig", cohort)
      results["internalising z-score (YSR)", cohort] <- get_missing("inter_YSR_z", cohort)
      results["externalising z-score (YSR)", cohort] <- get_missing("exter_YSR_z", cohort)
      results["ADHD z-score (YSR)", cohort] <- get_missing("ADHD_YSR_z", cohort)
      results["Age at outcome (YSR)", cohort] <- get_missing("outcome_age_YSR", cohort)
    }
    if (cohort == 'ALSPAC')
    {
      results["maternal ethnicity", cohort] <- get_missing("natorig", cohort)
    }
  }
  as.data.frame(results, stringsAsFactors = FALSE)
}

# --- Generate and format table ---
cohorts <- sort(c("ALSPAC","GenR"))
miss_table <- make_missing_table(descriptives, cohorts)

save(miss_table, file = file.path(projectpath, 'output/missingness_Table1_project3.Rdata'))
#-----------------------------Table 1 complete cases----------------------------
make_descriptives_table <- function(descriptives, cohorts) {
  
  get_n_pct_total <- function(cohort) {
    total_val <- N_total[[cohort]]
    pct <- if (total_val > 0) round(n_val / total_val * 100, 1) else NA
    sprintf("%d (%.1f%%)", n_val, pct)
  }
  
  get_count_pct_with_missing <- function(variable, cohort, code) {
    
    total_val <- nrow(complete_datasets[[cohort]]) 
    
    # Count for codes of interest
    count <- sum(cohort_data[[variable]]== code,na.rm = T)
    table(cohort_data[[variable]])
    
    n_missing <- sum(is.na(cohort_data[[variable]]))
    
    # Percentages
    pct <- if (total_val > 0) round(count / total_val * 100, 1) else NA
    miss_pct <- if (total_val > 0) round(n_missing / total_val * 100, 1) else NA
    
    sprintf("%d (%.1f%%)", count, pct)
  }
  
  get_mean_sd <- function(variable, cohort) {
    total_val <- N_total[[cohort]] 
    
    mean_val <- mean(as.numeric(descriptives[[cohort]][[variable]]), na.rm = T)
    sd_val <- sd(as.numeric(descriptives[[cohort]][[variable]]), na.rm = T)
    
    n_missing <- sum(is.na(cohort_data[[variable]]))
    miss_pct <- if (total_val > 0) round(n_missing / total_val * 100, 1) else NA
    
    if (is.na(mean_val) || is.na(sd_val)) return(NA_character_)
    sprintf("%.1f (%.1f)", mean_val, sd_val)
  }
  
  # Define row structure with section headers and variables
  row_labels <- c(
    "n (% of original sample)",
    "Male, n (%)",
    "Female, n (%)",
    "Maternal age at childbirth, years, mean (SD)",
    "University degree, n (%)",
    "'Higher' vocational training, n (%)",
    "Three or more years of secondary school, n (%)",
    "Primary or no education, n (%)",
    "Vocational, O level, A level, n (%)",
    "CSE, n (%)",
    "Dutch, n (%)",
    "Surinamese-Antillean, n (%)",
    "Turkish-Moroccan, n (%)",
    "Non-Dutch European, n (%)",
    "Other, n (%)",
    "White, n (%)",
    "Non-white, n (%)",
    "Prenatal maternal depression, n (%)",
    "No prenatal maternal depression, n (%)",
    "Prenatal partner depression, n (%)",
    "No prenatal partner depression, n (%)",
    "Postnatal partner depression, n (%)",
    "No postnatal partner depression, n (%)",
    "Any alcohol use in pregnancy, yes, n (%)",
    "No alcohol use in pregnancy, yes, n (%)",
    "Any smoking in pregnancy, yes, n (%)",
    "No smoking in pregnancy, yes, n (%)",
    "Pre-pregnancy BMI, mean (SD)",
    "Maternal Mediterranean diet, mean (SD)",
    "Child Mediterranean diet, mean (SD)",
    "Friendship z-score  child, mean (SD)",
    "Prenatal life events, mean (SD)",
    "Prenatal contextual risk, mean (SD)",
    "Prenatal interpersonal risk, mean (SD)",
    "Postnatal life events, mean (SD)",
    "Postnatal contextual risk, mean (SD)",
    "Postnatal interpersonal risk, mean (SD)",
    "Postnatal direct victimization, mean (SD)",
    "Internalising symptoms z-score, mean (SD)",
    "Externalising symptoms z-score, mean (SD)",
    "ADHD symptoms z-score, mean (SD)",
    "Age at outcome, mean (SD)"
  )
  
  results <- matrix(NA_character_, nrow = length(row_labels), ncol = length(cohorts),
                    dimnames = list(row_labels, cohorts))
  
  for (cohort in cohorts) {
    cohort_data <- descriptives[[cohort]]
    n_val <- nrow(cohort_data)
    
    results["n (% of original sample)", cohort] <- get_n_pct_total(cohort)
    results["Male, n (%)", cohort] <- get_count_pct_with_missing("assigned_sex", cohort, "1")
    results["Female, n (%)", cohort] <- get_count_pct_with_missing("assigned_sex", cohort, "2")
    results["Maternal age at childbirth, years, mean (SD)", cohort] <- get_mean_sd("m_age_birth", cohort)
    results["University degree, n (%)", cohort] <- get_count_pct_with_missing("edu_cat", cohort, "1: University degree")
    if (cohort == 'GenR')
    {
      results["'Higher' vocational training, n (%)", cohort] <- get_count_pct_with_missing("edu_cat", cohort, "2: 'Higher' vocational training")
      results["Three or more years of secondary school, n (%)", cohort] <- get_count_pct_with_missing("edu_cat", cohort, "3: Three or more years of secondary school")
      results["Primary or no education, n (%)", cohort] <- get_count_pct_with_missing("edu_cat", cohort, "4: Primary or no education")
      results["Dutch, n (%)", cohort] <- get_count_pct_with_missing("natorig", cohort, "1: Dutch")
      results["Surinamese-Antillean, n (%)", cohort] <- get_count_pct_with_missing("natorig", cohort, "2: Turkish-Moroccan")
      results["Turkish-Moroccan, n (%)", cohort] <- get_count_pct_with_missing("natorig", cohort, "3: Surinamese-Antillean")
      results["Non-Dutch European, n (%)", cohort] <- get_count_pct_with_missing("natorig", cohort, "4: European")
      results["Other, n (%)", cohort] <- get_count_pct_with_missing("natorig", cohort, "5: Other")
    }
    if (cohort == 'ALSPAC')
    {
      results["Vocational, O level, A level, n (%)", cohort] <- get_count_pct_with_missing("edu_cat", cohort, "2: Vocational, O level, A level")
      results["CSE, n (%)", cohort] <- get_count_pct_with_missing("edu_cat", cohort, "3: CSE")
      results["White, n (%)", cohort] <- get_count_pct_with_missing("natorig", cohort, "White")
      results["Non-white, n (%)", cohort] <- get_count_pct_with_missing("natorig", cohort, "Non-white")
    }
    results["Prenatal maternal depression, n (%)", cohort] <- get_count_pct_with_missing("preg_dep_bin", cohort, "1: Depression")
    results["No prenatal maternal depression, n (%)", cohort] <- get_count_pct_with_missing("preg_dep_bin", cohort, "0: No depression")
    results["Prenatal partner depression, n (%)", cohort] <- get_count_pct_with_missing("p_dep_18wg_bin", cohort, "1: Depression")
    results["No prenatal partner depression, n (%)", cohort] <- get_count_pct_with_missing("p_dep_18wg_bin", cohort, "0: No depression")
    results["Postnatal partner depression, n (%)", cohort] <- get_count_pct_with_missing("p_dep_3y_bin", cohort, "1: Depression")
    results["No postnatal partner depression, n (%)", cohort] <- get_count_pct_with_missing("p_dep_3y_bin", cohort, "0: No depression")
    results["Any alcohol use in pregnancy, yes, n (%)", cohort] <- get_count_pct_with_missing("preg_alc", cohort, "1: Any alcohol")
    results["No alcohol use in pregnancy, yes, n (%)", cohort] <- get_count_pct_with_missing("preg_alc", cohort, "0: No alcohol")
    results["Any smoking in pregnancy, yes, n (%)", cohort] <- get_count_pct_with_missing("preg_smk", cohort, "1: Any smoking")
    results["No smoking in pregnancy, yes, n (%)", cohort] <- get_count_pct_with_missing("preg_smk", cohort, "0: Never smoked")
    results["Pre-pregnancy BMI, mean (SD)", cohort] <- get_mean_sd("prepreg_BMI", cohort)
    results["Maternal Mediterranean diet, mean (SD)", cohort] <- get_mean_sd("med_diet_mat", cohort)
    results["Child Mediterranean diet, mean (SD)", cohort] <- get_mean_sd("med_diet_child", cohort)
    results["Friendship z-score  child, mean (SD)", cohort] <- get_mean_sd("friendship_z", cohort)
    results["Prenatal life events, mean (SD)", cohort] <- get_mean_sd("pre_LE_domain_score_z", cohort)
    results["Prenatal contextual risk, mean (SD)", cohort] <- get_mean_sd("pre_CR_domain_score_z", cohort)
    results["Prenatal interpersonal risk, mean (SD)", cohort] <- get_mean_sd("pre_IR_domain_score_z", cohort)
    results["Postnatal life events, mean (SD)", cohort] <- get_mean_sd("pos_LE_domain_score_z", cohort)
    results["Postnatal contextual risk, mean (SD)", cohort] <- get_mean_sd("pos_CR_domain_score_z", cohort)
    results["Postnatal interpersonal risk, mean (SD)", cohort] <- get_mean_sd("pos_IR_domain_score_z", cohort)
    results["Postnatal direct victimization, mean (SD)", cohort] <- get_mean_sd("pos_DV_domain_score_z", cohort)
    results["Internalising symptoms z-score, mean (SD)", cohort] <- get_mean_sd("inter_z", cohort)
    results["Externalising symptoms z-score, mean (SD)", cohort] <- get_mean_sd("exter_z", cohort)
    results["ADHD symptoms z-score, mean (SD)", cohort] <- get_mean_sd("ADHD_z", cohort)
    results["Age at outcome, mean (SD)", cohort] <- get_mean_sd("outcome_age", cohort)
    
  }
  as.data.frame(results, stringsAsFactors = FALSE)
}

# --- Generate and format table ---
cohorts <- sort(c("ALSPAC","GenR"))
desc_table <- make_descriptives_table(complete_datasets, cohorts)

# Add rownames as a column
desc_table$Value <- c(
  "(% of original sample)",
  "Male, n (%)",
  "Female, n (%)",
  "Mean (SD)",
  "University degree, n (%)",
  "'Higher' vocational training, n (%)",
  "Three or more years of secondary school, n (%)",
  "Primary or no education, n (%)",
  "Vocational, O level, A level, n (%)",
  "CSE, n (%)",
  "Dutch, n (%)",
  "Surinamese-Antillean, n (%)",
  "Turkish-Moroccan, n (%)",
  "Non-Dutch European, n (%)",
  "Other, n (%)",
  "White, n (%)",
  "Non-white, n (%)",
  "Depression, n (%)",
  "No depression, n (%)",
  "Depression, n (%)",
  "No depression, n (%)",
  "Depression, n (%)",
  "No depression, n (%)",
  "Any alcohol, n (%)",
  "No alcohol, n (%)",
  "Any smoking, n (%)",
  "No smoking, n (%)",
  "Mean (SD)",
  "Mean (SD)",
  "Mean (SD)",
  "Mean (SD)",
  "Life events, mean (SD)",
  "Contextual risk, mean (SD)",
  "Interpersonal risk, mean (SD)",
  "Life events, mean (SD)",
  "Contextual risk, mean (SD)",
  "Interpersonal risk, mean (SD)",
  "Direct victimization, mean (SD)",
  "Mean (SD)",
  "Mean (SD)",
  "Mean (SD)",
  "Mean (SD)"
)
desc_table$Variable <- c(
  "Sample size",
  "Child's assigned sex at birth",
  "",
  "Maternal age at childbirth, years",
  "Maternal education",
  "",
  "",
  "",
  "",
  "",
  "Maternal national origin",
  "",
  "",
  "",
  "",
  "Maternal ethnicity",
  "",
  "Prenatal maternal depression",
  "",
  "Prenatal partner depression",
  "",
  "Postnatal partner depression",
  "",
  "Alcohol use in pregnancy",
  "",
  "Smoking in pregnancy",
  "",
  "Pre-pregnancy BMI",
  "Maternal Mediterranean diet",
  "Child Mediterranean diet",
  "Friendship z-score  child",
  "Prenatal ELS",
  "",
  "",
  "Postnatal ELS",
  "",
  "",
  "",
  "Internalising symptoms z-score",
  "Externalising symptoms z-score",
  "ADHD symptoms z-score",
  "Age at outcome"
)

desc_table <- desc_table[, c("Variable", "Value", cohorts)]

save(desc_table, file = file.path(projectpath, 'output/Stable_completecases_project3.Rdata'))

#---------------------------COMPLETE-CASE ANALYSES------------------------------
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
  results_list[[name]] <- data.frame(matrix(NA, nrow = 3, ncol = 4))
}

run_analysis <- function(outcome_list, data, exposure, variable, term, 
                         results_storage, covariates) {
  # Loop through all outcomes in outcome_list
  for (i in 1:3) {
    outcome <- outcome_list[i]
    
    # Construct the covariate part of the formula
    covariate_formula <- paste(covariates, collapse = " + ")
    
    # Create the formula dynamically based on the variable (e.g., "p_dep_18wg", "preg_alc", etc.)
    formula <- paste0(outcome, " ~ ",exposure,"*", variable, " + ", covariate_formula)
    
    # Run the model
    mod1_f <- lm(as.formula(formula), data = data)
    
    # Get the summary of the pooled model
    output <- summary(mod1_f, conf.int = TRUE)
    results <- as.data.frame(as.data.frame(output$coefficients)[c("Estimate", "Std. Error")])
    results <- results[rownames(results) == term, ]

    # Add term and outcome variable to the results
    colnames(results)[colnames(results) == "Estimate"] <- "estimate"
    colnames(results)[colnames(results) == "Std. Error"] <- "std.error"
    results$term <- term
    results$outcome <- outcome
    
    # Store the results in the appropriate results storage (e.g., results_p18w, results_p3y, etc.)
    results_storage[i, ] <- results
  }
  
  # Set column names for the final results storage
  colnames(results_storage) <- colnames(results)
  
  return(results_storage)
}

#-------------------------------------GenR-------------------------------------- 
res <- list()

res$mat_diet_INTER <- run_analysis(
  outcome_list = outcome_list, 
  data = complete_datasets$GenR, 
  exposure = "preg_dep_bin",
  variable = "med_diet_mat", 
  term = "preg_dep_bin1: Depression:med_diet_mat", 
  results_storage = results_list$mat_diet, 
  covariates = covariates
)

res$friendship_INTER <- run_analysis(
  outcome_list = outcome_list, 
  data = complete_datasets$GenR,
  exposure = "preg_dep_bin",
  variable = "friendship_z", 
  term = "preg_dep_bin1: Depression:friendship_z", 
  results_storage = results_list$friendship, 
  covariates = covariates
)

res$child_diet_INTER <- run_analysis(
  outcome_list = outcome_list, 
  data = complete_datasets$GenR, 
  exposure = "preg_dep_bin",
  variable = "med_diet_child", 
  term = "preg_dep_bin1: Depression:med_diet_child", 
  results_storage = results_list$child_diet, 
  covariates = covariates
)

res$p18w_INTER <- run_analysis(
  outcome_list = outcome_list, 
  data = complete_datasets$GenR, 
  exposure = "preg_dep_bin",
  variable = "p_dep_18wg_bin", 
  term = "preg_dep_bin1: Depression:p_dep_18wg_bin1: Depression", 
  results_storage = results_list$p18w, 
  covariates = covariates
)

res$p3y_INTER <- run_analysis(
  outcome_list = outcome_list, 
  data = complete_datasets$GenR, 
  exposure = "preg_dep_bin",
  variable = "p_dep_3y_bin", 
  term = "preg_dep_bin1: Depression:p_dep_3y_bin1: Depression", 
  results_storage = results_list$p3y, 
  covariates = covariates
)

res$preLE_INTER <- run_analysis(
  outcome_list = outcome_list, 
  data = complete_datasets$GenR, 
  exposure = "preg_dep_bin",
  variable = "pre_LE_domain_score_z", 
  term = "preg_dep_bin1: Depression:pre_LE_domain_score_z", 
  results_storage = results_list$preLE, 
  covariates = covariates
)

res$posLE_INTER <- run_analysis(
  outcome_list = outcome_list, 
  data = complete_datasets$GenR, 
  exposure = "preg_dep_bin",
  variable = "pos_LE_domain_score_z", 
  term = "preg_dep_bin1: Depression:pos_LE_domain_score_z", 
  results_storage = results_list$posLE, 
  covariates = covariates
)

res$preIR_INTER <- run_analysis(
  outcome_list = outcome_list, 
  data = complete_datasets$GenR, 
  exposure = "preg_dep_bin",
  variable = "pre_IR_domain_score_z", 
  term = "preg_dep_bin1: Depression:pre_IR_domain_score_z", 
  results_storage = results_list$preIR, 
  covariates = covariates
)

res$posIR_INTER <- run_analysis(
  outcome_list = outcome_list, 
  data = complete_datasets$GenR, 
  exposure = "preg_dep_bin",
  variable = "pos_IR_domain_score_z", 
  term = "preg_dep_bin1: Depression:pos_IR_domain_score_z", 
  results_storage = results_list$posIR, 
  covariates = covariates
)

res$posDV_INTER <- run_analysis(
  outcome_list = outcome_list, 
  data = complete_datasets$GenR, 
  exposure = "preg_dep_bin",
  variable = "pos_DV_domain_score_z", 
  term = "preg_dep_bin1: Depression:pos_DV_domain_score_z", 
  results_storage = results_list$posDV, 
  covariates = covariates
)

res$preCR_INTER <- run_analysis(
  outcome_list = outcome_list, 
  data = complete_datasets$GenR, 
  exposure = "preg_dep_bin",
  variable = "pre_CR_domain_score_z", 
  term = "preg_dep_bin1: Depression:pre_CR_domain_score_z", 
  results_storage = results_list$preCR, 
  covariates = c("assigned_sex", "outcome_age", "preg_alc", "preg_smk", 
                 "m_age_birth", "prepreg_BMI", "natorig") # excl. edu_cat which is part of CR
)

res$posCR_INTER <- run_analysis(
  outcome_list = outcome_list, 
  data = complete_datasets$GenR, 
  exposure = "preg_dep_bin",
  variable = "pos_CR_domain_score_z", 
  term = "preg_dep_bin1: Depression:pos_CR_domain_score_z", 
  results_storage = results_list$posCR, 
  covariates = c("assigned_sex", "outcome_age", "preg_alc", "preg_smk", 
                 "m_age_birth", "prepreg_BMI", "natorig") # excl. edu_cat which is part of CR
)

res$alcohol_INTER <- run_analysis(
  outcome_list = outcome_list, 
  data = complete_datasets$GenR, 
  exposure = "preg_dep_bin",
  variable = "preg_alc", 
  term = "preg_dep_bin1: Depression:preg_alc1: Any alcohol", 
  results_storage = results_list$alcohol, 
  covariates = c("assigned_sex","outcome_age","preg_smk","m_age_birth",
                 "prepreg_BMI","natorig","edu_cat") # exclu. preg_alc since it's included
)

res$smoking_INTER <- run_analysis(
  outcome_list = outcome_list, 
  data = complete_datasets$GenR, 
  exposure = "preg_dep_bin",
  variable = "preg_smk", 
  term = "preg_dep_bin1: Depression:preg_smk1: Any smoking", 
  results_storage = results_list$smoking, 
  covariates = c("assigned_sex","outcome_age","preg_alc","m_age_birth",
                 "prepreg_BMI","natorig","edu_cat") # exclu. preg_smk since it's included
)

saveRDS(res, file=file.path(projectpath,"output", 
                            paste0('res_completecases_GenR',as.character(Sys.Date()),".Rdata")))

#------------------------------------ALSPAC------------------------------------- 
res <- list()

res$mat_diet_INTER <- run_analysis(
  outcome_list = outcome_list, 
  data = complete_datasets$ALSPAC, 
  exposure = "preg_dep_bin",
  variable = "med_diet_mat", 
  term = "preg_dep_bin1: Depression:med_diet_mat", 
  results_storage = results_list$mat_diet, 
  covariates = covariates
)

res$friendship_INTER <- run_analysis(
  outcome_list = outcome_list, 
  data = complete_datasets$ALSPAC,
  exposure = "preg_dep_bin",
  variable = "friendship_z", 
  term = "preg_dep_bin1: Depression:friendship_z", 
  results_storage = results_list$friendship, 
  covariates = covariates
)

res$child_diet_INTER <- run_analysis(
  outcome_list = outcome_list, 
  data = complete_datasets$ALSPAC, 
  exposure = "preg_dep_bin",
  variable = "med_diet_child", 
  term = "preg_dep_bin1: Depression:med_diet_child", 
  results_storage = results_list$child_diet, 
  covariates = covariates
)

res$p18w_INTER <- run_analysis(
  outcome_list = outcome_list, 
  data = complete_datasets$ALSPAC, 
  exposure = "preg_dep_bin",
  variable = "p_dep_18wg_bin", 
  term = "preg_dep_bin1: Depression:p_dep_18wg_bin1: Depression", 
  results_storage = results_list$p18w, 
  covariates = covariates
)

res$p3y_INTER <- run_analysis(
  outcome_list = outcome_list, 
  data = complete_datasets$ALSPAC, 
  exposure = "preg_dep_bin",
  variable = "p_dep_3y_bin", 
  term = "preg_dep_bin1: Depression:p_dep_3y_bin1: Depression", 
  results_storage = results_list$p3y, 
  covariates = covariates
)

res$preLE_INTER <- run_analysis(
  outcome_list = outcome_list, 
  data = complete_datasets$ALSPAC, 
  exposure = "preg_dep_bin",
  variable = "pre_LE_domain_score_z", 
  term = "preg_dep_bin1: Depression:pre_LE_domain_score_z", 
  results_storage = results_list$preLE, 
  covariates = covariates
)

res$posLE_INTER <- run_analysis(
  outcome_list = outcome_list, 
  data = complete_datasets$ALSPAC, 
  exposure = "preg_dep_bin",
  variable = "pos_LE_domain_score_z", 
  term = "preg_dep_bin1: Depression:pos_LE_domain_score_z", 
  results_storage = results_list$posLE, 
  covariates = covariates
)

res$preIR_INTER <- run_analysis(
  outcome_list = outcome_list, 
  data = complete_datasets$ALSPAC, 
  exposure = "preg_dep_bin",
  variable = "pre_IR_domain_score_z", 
  term = "preg_dep_bin1: Depression:pre_IR_domain_score_z", 
  results_storage = results_list$preIR, 
  covariates = covariates
)

res$posIR_INTER <- run_analysis(
  outcome_list = outcome_list, 
  data = complete_datasets$ALSPAC, 
  exposure = "preg_dep_bin",
  variable = "pos_IR_domain_score_z", 
  term = "preg_dep_bin1: Depression:pos_IR_domain_score_z", 
  results_storage = results_list$posIR, 
  covariates = covariates
)

res$posDV_INTER <- run_analysis(
  outcome_list = outcome_list, 
  data = complete_datasets$ALSPAC, 
  exposure = "preg_dep_bin",
  variable = "pos_DV_domain_score_z", 
  term = "preg_dep_bin1: Depression:pos_DV_domain_score_z", 
  results_storage = results_list$posDV, 
  covariates = covariates
)

res$preCR_INTER <- run_analysis(
  outcome_list = outcome_list, 
  data = complete_datasets$ALSPAC, 
  exposure = "preg_dep_bin",
  variable = "pre_CR_domain_score_z", 
  term = "preg_dep_bin1: Depression:pre_CR_domain_score_z", 
  results_storage = results_list$preCR, 
  covariates = c("assigned_sex", "outcome_age", "preg_alc", "preg_smk", 
                 "m_age_birth", "prepreg_BMI", "natorig") # excl. edu_cat which is part of CR
)

res$posCR_INTER <- run_analysis(
  outcome_list = outcome_list, 
  data = complete_datasets$ALSPAC, 
  exposure = "preg_dep_bin",
  variable = "pos_CR_domain_score_z", 
  term = "preg_dep_bin1: Depression:pos_CR_domain_score_z", 
  results_storage = results_list$posCR, 
  covariates = c("assigned_sex", "outcome_age", "preg_alc", "preg_smk", 
                 "m_age_birth", "prepreg_BMI", "natorig") # excl. edu_cat which is part of CR
)

res$alcohol_INTER <- run_analysis(
  outcome_list = outcome_list, 
  data = complete_datasets$ALSPAC, 
  exposure = "preg_dep_bin",
  variable = "preg_alc", 
  term = "preg_dep_bin1: Depression:preg_alc1: Any alcohol", 
  results_storage = results_list$alcohol, 
  covariates = c("assigned_sex","outcome_age","preg_smk","m_age_birth",
                 "prepreg_BMI","natorig","edu_cat") # exclu. preg_alc since it's included
)

res$smoking_INTER <- run_analysis(
  outcome_list = outcome_list, 
  data = complete_datasets$ALSPAC, 
  exposure = "preg_dep_bin",
  variable = "preg_smk", 
  term = "preg_dep_bin1: Depression:preg_smk1: Any smoking", 
  results_storage = results_list$smoking, 
  covariates = c("assigned_sex","outcome_age","preg_alc","m_age_birth",
                 "prepreg_BMI","natorig","edu_cat") # exclu. preg_smk since it's included
)

saveRDS(res, file=file.path(projectpath,"output", 
                            paste0('res_completecases_ALSPAC',as.character(Sys.Date()),".Rdata")))



