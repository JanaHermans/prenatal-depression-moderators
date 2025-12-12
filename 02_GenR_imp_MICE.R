########################################################################
# Title:    Imputation with MICE (GenR)
# Author:   Jana Hermans
# Date:     Last modification November 2025
# Paper:    Prenatal maternal depression, child mental health, and the 
#           role of lifestyle and psychosocial factors: results from two 
#           population-based birth cohorts.
########################################################################

# Clean console and environment
rm(list=ls())
cat("\014")

library(dplyr)
library(mice)
library(haven)

# Date (for marking output file)
date <- format(Sys.Date(), '%d%m%y')
#--------------------------------1) Load data-----------------------------------
dataset_full <- readRDS(file.path(datapath, 'GenR_moderator_vars_toimpute_1011.rds'))
print("Loading data was successful")
#-------------------------------2) MICE set-up----------------------------------
# Only selected necessary data (i.e. get rid of all sub_items)
selected_cols <- c("IDC","IDM","preg_dep","assigned_sex","EDUCM","m_age_birth","ETHNMv2",
                   "prepreg_BMI","prepreg_dep","mdrink_updated","smoke_all",
                   "friendship_sum","med_diet_mat","med_diet_child","p_dep_18wg","p_dep_3y",
                   "pre_LE_domain_score","pre_CR_domain_score","pre_IR_domain_score",
                   "prenatal_stress","pos_LE_domain_score","pos_CR_domain_score",
                   "pos_IR_domain_score","pos_DV_domain_score","postnatal_stress",
                   "inter_symptoms","exter_symptoms","ADHD_symptoms","outcome_age",
                   "inter_symptoms_YSR","exter_symptoms_YSR","ADHD_symptoms_YSR",
                   "outcome_age_YSR"
                   )

data <- dataset_full[, selected_cols]

# Specify the factor columns
factor_cols <- c("mdrink_updated", "smoke_all", "assigned_sex", "ETHNMv2", "EDUCM", "prepreg_dep")

# Set the rest of the columns to numeric, excluding 'IDC'
numeric_cols <- setdiff(names(data), c(factor_cols, "IDC"))

# Convert factor columns to factors
data[factor_cols] <- lapply(data[factor_cols], factor)

# Convert remaining columns to numeric (excluding IDC)
data[numeric_cols] <- lapply(data[numeric_cols], function(x) as.numeric(as.character(x)))
#sapply(data, class)
#-------------------------------2) MICE set-up----------------------------------
imp0 <- mice(data, maxit = 0, method = 'rf')
outlist <- as.character(imp0$loggedEvents[,'out'])
pred <- imp0$predictorMatrix
pred[, outlist] <- 0
pred[, 'IDC'] <- 0
meth <- imp0$method
meth[outlist] <- ""
meth['IDC'] <- ""
print("Variables excluded:")
print(outlist)
print("Set-up run done")
#---------------------------------3) Run MICE-----------------------------------
# Random forest imputation 
start.time <- Sys.time()
imp_rf <- futuremice(data, predictorMatrix = pred, method = meth, m = 20, maxit = 60, ntree = 10,
                     parallelseed = 721, n.core = 8) 
end.time <- Sys.time()
time.taken <- round(end.time - start.time,2)

print("MICE ran successfully!")
print("Time taken:")
print(time.taken)
#-----------------------------------4) Save-------------------------------------
save(imp_rf, file = file.path(datapath, paste0('GenR_impset_',as.character(Sys.Date()),".Rdata")))

#print("Imputation finished")