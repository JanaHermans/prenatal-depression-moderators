########################################################################
# Title:    Preparation of dataset for imputation (GenR)
# Author:   Jana Hermans
# Date:     Last modification November 2025
# Paper:    Prenatal maternal depression, child mental health, and the 
#           role of lifestyle and psychosocial factors: results from two 
#           population-based birth cohorts.
########################################################################

projectpath <- dirname(file.choose())
setwd(file.path(projectpath, "code/moderator_project"))
.libPaths(
  file.path(projectpath, 
            'code/moderator_project/renv/library/windows/R-4.4/x86_64-w64-mingw32'))
library(dplyr)
library(tidyverse)
library(haven)
library(naniar)
library(summarytools)
library(labelled)
library(magrittr)
library(sjPlot)
library(patchwork)
library(ALSPAC.helpR)
library(corrplot)

#---------------------------------DATA PREP-------------------------------------
pers_folder <- dirname(file.choose())
setwd(pers_folder)
# Load datasets - moderators
diet_child <- read_sav('./data/CHILDNUTRITIONFOODGROUPS_26042015.sav');
diet_mat <- read_sav('./data/MATERNALNUTRITION_22112016.sav');
colnames(diet_mat)[colnames(diet_mat) == 'idm'] <- 'IDM'
friendship  <- read_sav('./data/GR1084-B2_09082017.sav');
friendship$sum <- rowSums(sapply(friendship[,2:11], as.numeric))
smoking <- read_sav('./data/MATERNALSMOKING_22112016.sav');
colnames(smoking)[colnames(smoking) == 'idm'] <- 'IDM'
alcohol <- read_sav('./data/GEDRAGSGROEP_MaternalDrinking_22112016.sav');
BSI3_partner <- read_sav("./data/BSI 3 years of age_GR1065 G1-GR1066 C1_22112016.sav");
BSI18wg_partner <- read_sav("./data/GR1004-BSI G1_22112016.sav") 
colnames(BSI18wg_partner)[colnames(BSI18wg_partner) == 'idm'] <- 'IDM'
BSI18wg_partner <- BSI18wg_partner[c("IDM", "dep_p")]
# Load exposure information 
BSI_data <- read_sav('./data/GR1003-BSI D1_22112016.sav');
colnames(BSI_data)[colnames(BSI_data) == 'idm'] <- 'IDM'
pre_dep_df <- BSI_data[c("IDM", "dep")]
# Load outcome and covariate information 
cbcl13  <- read_sav('./data/GR1093-E1_CBCL_18062020.sav');
YSR <- read_sav("./data/GR1095_F-Child-YSR_18062020.sav")
YSR <- YSR[c("IDC","sum_internalizing_13s","sum_externalizing_13s",
             "sum_attention_13s","AGECHILD_GR1095")]
lifetime_dep <- read_sav("./data/imputation_data/GR1003-D2-20_01072012.sav")
lifetime_dep <- lifetime_dep[c("IDM","d0200103")]
colnames(lifetime_dep)[colnames(lifetime_dep) == 'd0200103'] <- 'prepreg_dep'
lifetime_dep$prepreg_dep[lifetime_dep$prepreg_dep == 5] <- NA
general_data <- read_sav("./data/FETALPERIOD-ALLGENERALDATA_10022025.sav")
gen_df <- general_data[c("IDM","AGE_M_v2","MARDICH","INCOME","EDUCM","BMI_0",
                         "PARITY","RemoveData")]
child_general_data <- read_sav("./data/CHILD-ALLGENERALDATA_10022025.sav")
ID_match <- child_general_data[c("IDM","IDC","GENDER","OUTCOMECHILD")]

# MERGE
full <- merge(pre_dep_df, ID_match, by = "IDM", all.x = TRUE)
full <- merge(full, general_data, by = "IDM", all.x = TRUE)
full <- merge(full, diet_child, by = "IDC", all.x = TRUE)
full <- merge(full, friendship, by = "IDC", all.x = TRUE)
full <- merge(full, lifetime_dep, by = "IDM", all.x = TRUE)
full <- merge(full, YSR, by = "IDC", all.x = TRUE)
full <- merge(full, smoking, by = "IDM", all.x = TRUE)
full <- merge(full, alcohol, by = "IDM", all.x = TRUE)
full <- merge(full, diet_mat, by = "IDM", all.x = TRUE)
full <- merge(full, BSI18wg_partner, by = "IDM", all.x = TRUE)
full <- merge(full, BSI3_partner, by = "IDC", all.x = TRUE)
full <- merge(full, cbcl13, by = "IDC", all.x = TRUE)

# Add ELS
pren_stress <- readRDS('./data/GenR_ELS/prenatal_stress_GENR.rds')
post_stress <- readRDS('./data/GenR_ELS/postnatal_stress_GENR.rds')

full <- Reduce(function(x,y) merge(x = x, y = y, by = 'IDC', all.x = T),
               list(pren_stress, post_stress, full) ) 

sum(is.na(full$IDC))
sum(duplicated(full$IDC))

# Initiate dataframe and set up unique child ID
data <- data.frame('IDC' = full$IDC)

# PRENATAL: diet, substance use, stress, prenatal partner depression

# POSTNATAL: diet, social support, stress, postnatal partner depression

#--------------------------------2) EXPOSURE------------------------------------

data$preg_dep <- full$dep

#----------------------------3a) MOD-DIET MOTHER--------------------------------
# Nutrients mean weight (g/day) [ FFQ ]
diet <- data.frame( # Extract form main dataset and rename
  # CEREAL
  'cer_1' = full[,'p06'], # CEREALS AND CEREAL PRODUCTS
  
  # FISH 
  'fis_1' = full[,'p08'], # FISH AND SHELLFISH 
  
  # DAIRY 
  'dai_1' = full[,'p05'], # DAIRY PRODUCTS
  
  # MEAT 
  'mea_1' = full[,'p07'], # MEAT AND MEAT PRODUCTS
  
  # VEGETABLES 
  'veg_1' = full[,'p02'], # VEGETABLES
  
  # LEGUMES 
  'leg_1' = full[,'p03'], # LEGUMES
  
  # FRUIT 
  'fru_1' = full[,'p04'] # FRUITS
)

FFQ_names <- c('cer_1','fis_1','dai_1','mea_1','veg_1','leg_1','fru_1')

minmax_norm <- function(x, ...) { return( (x - min(x,...)) / (max(x,...) - min(x,...)) )}
diet[,c(FFQ_names)] <- sapply(diet[,c(FFQ_names)], minmax_norm, na.rm = TRUE)

foodgroups <- c('veg','leg','fru','cer','fis','mea','dai')

# Compute a sum score per food group and the corresponding binary variable
for (foodgroup in foodgroups) {
  message('\n', toupper(foodgroup))
  
  name_c <- paste0('med_diet_',foodgroup) # Continuous 
  name_b <- paste0('med_diet_',foodgroup,'_bin') # Binary
  
  # Compute continuous (i.e., sum of food group components)
  #if (foodgroup=='leg') { 
    diet[, name_c] <- diet[,paste0(foodgroup,'_1')] # only one item in the group
  #} else { diet[, name_c] <- rowSums(diet[, grepl(foodgroup, names(diet))], na.rm = F) }
  
  print(summary(diet[, name_c]))
  
  # Dichotomize at median intake
  med <- median(diet[, name_c], na.rm = T)
  if (foodgroup %in% c('mea','dai')) { # there are reverse coded (i.e. non beneficial)
    diet[, name_b] <- ifelse(diet[, name_c] < med, 1, 0)
  } else {
    diet[, name_b] <- ifelse(diet[, name_c] >= med, 1, 0)
  }
  message("Dichotomized ", foodgroup, ' at median intake = ', med)
  print(summary(as.factor(diet[, name_b])))
}

# Sum of binary diet components
data$med_diet_mat <- rowSums(diet[, paste0('med_diet_',foodgroups,'_bin')]) 
summary(data$med_diet_mat)

# Dichotomized diet score (at median)
data$med_diet_bin_mat <- as.factor(ifelse(data$med_diet_mat >= median(data$med_diet_mat, na.rm=T), 1, 0))
summary(data$med_diet_bin_mat)
#-----------------------------3b) MOD-DIET CHILD--------------------------------
# Nutrients mean weight (g/day) [ FFQ ]
diet <- data.frame( # Extract form main dataset and rename
  # CEREAL
  'cer_1' = full[,'FG60_04_Bread_whole'], # Brown/wholegrain bread
  'cer_2' = full[,'FG60_06_Grainproducts_whole'], 
  'cer_3' = full[,'FG60_34_Porridge'], 
  # FG60_05_Bread_white / FG60_07_Grainproducts_white
  # FG60_09_Cereals_white_sugar
  # FG60_41_Sweetsnacks # Sweets, sweet snacks, and cookies
  # FG60_42_Savorysnacks # Savory snacks, excl potato crisp
  # FG60_50_Sweettoppings # Sweet toppings or sandwich fillings
  # FG60_51_Potatoes_crisps
  # FG60_52_Pizza_savorypie # Pizza, savory pie, quiche
  # FG60_53_Pastadishes
  # FG60_56_Nasi_bami_dishes # Stir-fried Indonesian rice or noodles
  
  # FISH 
  'fis_1' = full[,'FG60_10_Fish_fat'], # Fatty fish (>10% fat) 
  'fis_2' = full[,'FG60_11_Shellfish'], # Shellfish
  'fis_3' = full[,'FG60_12_Fish_lowfat'], # Lean fish (<2% fat) 
  'fis_4' = full[,'FG60_13_Fish_modfat'], # Moderately fat fish (2-10%)  
  'fis_5' = full[,'FG60_14_Fish_fat_canned'], # Canned fatty fish
  # FG60_08_Fish_breaded
  
  # DAIRY 
  'dai_1' = full[,'FG60_24_Milk_lowfat'], # Skimmed or semi skimmed milk, buttermilk, no added sugar
  'dai_2' = full[,'FG60_25_Yogurt_lowfat'], # Skimmed or semi-skimmed yoghurt or quark, no added sugar
  'dai_3' = full[,'FG60_26_Milk_fullfat'], # Full-fat milk
  'dai_4' = full[,'FG60_27_Yoghurt_fullfat'], # Full-fat yoghurt or quark
  'dai_5' = full[,'FG60_28_Milk_beverages_sugar'], # Milk-based beverages with added sugar 
  'dai_6' = full[,'FG60_29_Yogurt_sugar'], # Yoghurt or quark with added sugar
  'dai_7' = full[,'FG60_30_Cheese_lowfat'], # Low-fat cheese (<=30+)
  'dai_8' = full[,'FG60_31_Cheese_fullfat'], # Full-fat cheese (> 30+)
  # FG60_32_Dairy_desserts # Dairy based desserts (incl ice-cream)
  # FG60_33_Soymilks # Soy milk/ soy dessert
  
  # MEAT 
  'mea_1' = full[,'FG60_15_Meat_red_unprocessed_lowfat'], # Red meat, unprocessed, low-fat (<=5% SFA) 
  'mea_2' = full[,'FG60_16_Meat_red_processed'], # Red meat, processed
  'mea_3' = full[,'FG60_17_Meat_white_processed'], # White meat, processed
  'mea_4' = full[,'FG60_18_Meat_white_unprocessed_lowfat'], # White meat, unprocessed, low-fat (<=5% SFA) 
  'mea_5' = full[,'FG60_35_Fats_hard'], # Hard fats, butter
  'mea_6' = full[,'FG60_45_Meat_red_unprocessed_highfat'], # Red meat, unprocessed, high-fat (>5% SFA) 
  'mea_7' = full[,'FG60_47_Sausagerolls'], # Sausage rolls (worstebroodje, sausijzenbroodje)
  'mea_8' = full[,'FG60_48_Meat_fastfood'], # Fast food meat (kroket frikande)
  # FG60_19_Meatreplacing_products
  # FG60_36_Fats_soft_lowSFA # Soft fats(>=30% saturated fat of total fat), oils
  # FG60_40_Eggs
  # FG60_49_Sauces_fat # Fat- containing sauses
  
  # VEGETABLES 
  'veg_1' = full[,'FG60_02_Vegetables_cooked'],
  'veg_2' = full[,'FG60_03_Vegetables_raw'],
  # FG60_43_Potatoes / FG60_57_Potatoes_frenchfries
  
  # LEGUMES 
  'leg_1' = full[,'FG60_20_Pulses_canned'], # Pulses, canned
  
  # FRUIT 
  'fru_1' = full[,'FG60_01_Fruit_fresh'], # Fresh fruit
  'fru_2' = full[,'FG60_21_Nuts_unsalted'], # Nuts, unsalted
  'fru_3' = full[,'FG60_44_Fruit_dried'] # Dried fruit (raisins) 
  # FG60_37_Fruitjuice / FG60_22_Peanuts_peanutbutter
)

FFQ_names <- c('cer_1','cer_2','cer_3','fis_1','fis_2','fis_3','fis_4','fis_5',
               'dai_1','dai_2','dai_3','dai_4','dai_5','dai_6','dai_7','dai_8',
               'mea_1','mea_2','mea_3','mea_4','mea_5','mea_6','mea_7','mea_8',
               'veg_1','veg_2','leg_1','fru_1','fru_2','fru_3')

minmax_norm <- function(x, ...) { return( (x - min(x,...)) / (max(x,...) - min(x,...)) )}
diet[,c(FFQ_names)] <- sapply(diet[,c(FFQ_names)], minmax_norm, na.rm = TRUE)

foodgroups <- c('veg','leg','fru','cer','fis','mea','dai')

# Compute a sum score per food group and the corresponding binary variable
for (foodgroup in foodgroups) {
  message('\n', toupper(foodgroup))
  
  name_c <- paste0('med_diet_',foodgroup) # Continuous 
  name_b <- paste0('med_diet_',foodgroup,'_bin') # Binary
  
  # Compute continuous (i.e., sum of food group components)
  if (foodgroup=='leg') { diet[, name_c] <- diet[,'leg_1'] # only one item in the group
  } else { diet[, name_c] <- rowSums(diet[, grepl(foodgroup, names(diet))], na.rm = F) }
  
  print(summary(diet[, name_c]))
  
  # Dichotomize at median intake
  med <- median(diet[, name_c], na.rm = T)
  if (foodgroup %in% c('mea','dai')) { # there are reverse coded (i.e. non beneficial)
    diet[, name_b] <- ifelse(diet[, name_c] < med, 1, 0)
  } else {
    diet[, name_b] <- ifelse(diet[, name_c] >= med, 1, 0)
  }
  message("Dichotomized ", foodgroup, ' at median intake = ', med)
  print(summary(as.factor(diet[, name_b])))
}

# Sum of binary diet components
data$med_diet_child <- rowSums(diet[, paste0('med_diet_',foodgroups,'_bin')]) 
summary(data$med_diet_child)

# Dichotomized diet score (at median)
data$med_diet_bin_child <- as.factor(ifelse(data$med_diet_child >= median(data$med_diet_child, na.rm=T), 1, 0))
summary(data$med_diet_bin_child)

data$med_diet_age_child <- full[,'agechild_GR1080']
#------------------------------3c) MOD-FRIENDSHIP-------------------------------
# Create quantile so it corresponds with ALSPAC
data$friendship_sum <- full$sum

#-------------------------------3d) MOD-STRESS----------------------------------
data <- Reduce(function(x,y) merge(x = x, y = y, by = 'IDC', all.x = T),
               list(pren_stress, post_stress, data) ) 

names(data)[names(data) == 'IDM.y'] <- 'IDM'
data$IDM.x <- NULL

names(data)[names(data) == "pre_life_events"] <- "pre_LE_domain_score"
names(data)[names(data) == "pre_contextual_risk"] <- "pre_CR_domain_score"
names(data)[names(data) == "pre_interpersonal_risk"] <- "pre_IR_domain_score"
names(data)[names(data) == "post_life_events"] <- "pos_LE_domain_score"
names(data)[names(data) == "post_contextual_risk"] <- "pos_CR_domain_score"
names(data)[names(data) == "post_interpersonal_risk"] <- "pos_IR_domain_score"
names(data)[names(data) == "post_direct_victimization"] <- "pos_DV_domain_score"

#-----------------------------3e) MOD-PARTNER DEP-------------------------------
#  (BSI)
bsi_scores <- function(items, threshold=length(items)-1, filledinby, cutoff, persons=c(1:5)){
  it <- data.frame(full[,items])
  no_na_sum <- rowSums(!is.na(it))
  cont_score <- ifelse(no_na_sum >= threshold, yes = (rowSums(it)/no_na_sum)-1, no = NA)
}

data$p_dep_18wg <- full$dep_p
data$p_dep_3y <- bsi_scores(items = c('G0100365', 'G0100665', 'G0100765', 'G0100865', 'G0101365', 'G0102165'))

#----------------------------3f) MOD-SUBSTANCE USE------------------------------
#  (Alcohol)
data$mdrink_updated <- full[,'mdrink_updated'] 

#  (Smoking)
data$smoke_all <- full[,'SMOKE_ALL'] 
#--------------------------------4) COVARIATES----------------------------------
data$assigned_sex <- full$GENDER 

# maternal education
data$EDUCM <- full$EDUCM

# maternal age
data$m_age_birth <- full$AGE_M_Birth

data$ETHNMv2 <- full$ETHNMv2

# BMI
data$prepreg_BMI <- as.numeric(full$BMI_0)

# prepreg_dep
data$prepreg_dep <- full$prepreg_dep

# age child at outcome
data$outcome_age <- full$AGECHILD_GR1093
data$outcome_age_YSR <- full$AGECHILD_GR1095
#---------------------------------5) OUTCOMES-----------------------------------
## Select CBCL outcomes
data$ADHD_symptoms <- full$sum_att_14 # hyperactivity scale
data$inter_symptoms <- full$sum_int_14 # emotional scale
data$exter_symptoms <- full$sum_ext_14 # conduct scale
## Select YSR outcomes
data$ADHD_symptoms_YSR <- full$sum_attention_13s 
data$inter_symptoms_YSR <- full$sum_internalizing_13s 
data$exter_symptoms_YSR <- full$sum_externalizing_13s 
#--------------------Remove participants who requested this---------------------
data$OUTCOMECHILD <- as.character(full$OUTCOMECHILD)
data$RemoveData <- full$RemoveData
data <- data[is.na(data$RemoveData),]
nrow(data)
data <- data[data$OUTCOMECHILD == '1' & !is.na(data$OUTCOMECHILD), ] # keep only live births
nrow(data)
#-------------------------INCOMPLETE CASE DATASET-------------------------------
data_to_impute <- data
data_to_impute$RemoveData <- NULL
data_to_impute$OUTCOMECHILD <- NULL
#saveRDS(data_to_impute, file.path(pers_folder, 'data/GenR_moderator_vars_toimpute_1011.rds'))

