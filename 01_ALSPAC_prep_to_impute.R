########################################################################
# Title:    Preparation of dataset for imputation (ALSPAC)
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
# File name and location 
filename <- 'Neumann_B4611_20Sep24.sav' # change if needed
datapath <- dirname(file.choose()) 

full <- foreign::read.spss(file.path(datapath, 'ALSPAC', filename), 
                           use.value.labels=F, to.data.frame=T)
names(full) <- tolower(names(full)) # all column names to lower case

# Initiate dataframe and set up unique child ID
data <- data.frame('IDC' = paste(full$cidb4611, # mother ID
                                 gsub('\\s+', '', full$qlet), # sibling ID (A or B)
                                 sep = "_")) # --> format "1_A"

# Initiate dataframe and set up unique child ID
#data <- data.frame('IDC' = make_idc(mom.id = 'cidb4611', data = full))

# Merge with ELS files
pren_stress <- readRDS(file.path(datapath, 'ALSPAC', 'ALSPAC_pre_ELS.rds'))
post_stress <- readRDS(file.path(datapath, 'ALSPAC', 'ALSPAC_post_ELS.rds'))

data <- Reduce(function(x,y) merge(x = x, y = y, by = 'IDC', all.x = T),
               list(pren_stress, post_stress, data) ) 

# PRENATAL: substance use, maternal diet, stress, prenatal partner depression
# POSTNATAL: maternal diet, social support, stress, postnatal partner depression

#--------------------------------2) EXPOSURE------------------------------------
#data$preg_dep18w <- full$b371
#data$preg_dep32w <- full$c600

full$preg_dep18w <- epds_score(set = c('b360','b361','b363'),
                              revset = c('b362','b364','b365','b366','b367','b368','b369'),
                              data = full)
full$preg_dep32w <- epds_score(set = c('c590','c591','c593'),
                              revset = c('c592','c594','c595','c596','c597','c598','c599'),
                              data = full)

data$preg_dep18w <- as.numeric(full$preg_dep18w)
data$preg_dep32w <- as.numeric(full$preg_dep32w)
# Will be harmonised to preg_dep after imputation
#----------------------------3a) MOD-DIET MOTHER--------------------------------
# Nutrients mean weight (g/day) [ FFQ ]
diet <- data.frame( # Extract form main dataset and rename
  # CEREAL
  'cer_1' = full[,'c253'], # Eat brown or granary bread
  'cer_2' = full[,'c254'], # Eat wholemeal bread
  'cer_3' = full[,'c233'], # FREQ of eating oat cereals
  'cer_4' = full[,'c234'], # FREQ of eating bran cereals
  'cer_5' = full[,'c235'], # FREQ of eating other cereals
  
  # FISH 
  'fis_1' = full[,'c206'], # FREQ of eating oily fish
  'fis_2' = full[,'c207'], # FREQ of eating shellfish
  'fis_3' = full[,'c205'], # FREQ of eating white fish
  
  # DAIRY 
  'dai_1' = full[,'c276'], # Use full fat milk
  'dai_2' = full[,'c277'], # Use semi skimmed milk
  'dai_3' = full[,'c278'], # Use skimmed milk
  'dai_4' = full[,'c279'], # Use sterilised milk
  'dai_5' = full[,'c280'], # Use dried milk
  'dai_6' = full[,'c281'], # Use goat or sheep milk
  'dai_7' = full[,'c209'], # FREQ of eating cheese
  
  # MEAT 
  'mea_1' = full[,'c202'], # FREQ of eating meat
  'mea_2' = full[,'c203'], # FREQ of eating poultry
  'mea_3' = full[,'c204'], # FREQ of eating offal
  'mea_4' = full[,'c200'], # FREQ of eating sausages or burgers
  
  # VEGETABLES 
  'veg_1' = full[,'c224'], # FREQ of eating cabbage or SIM
  'veg_2' = full[,'c225'], # FREQ of eating other green VEG
  'veg_3' = full[,'c226'], # FREQ of eating carrots
  'veg_4' = full[,'c227'], # FREQ of eating root VEG not INC carrots
  'veg_5' = full[,'c228'], # FREQ of eating salad
  
  # LEGUMES 
  'leg_1' = full[,'c223'], # FREQ of eating peas corn or SIM
  'leg_2' = full[,'c240'], # FREQ of eating pulses
  
  # FRUIT 
  'fru_1' = full[,'c229'], # FREQ of eating fresh fruit
  'fru_2' = full[,'c241'] # FREQ of eating nuts
)

FFQ_names <- c('cer_1','cer_2','cer_3','cer_4','cer_5','fis_1','fis_2','fis_3',
               'dai_1','dai_2','dai_3','dai_4','dai_5','dai_6','dai_7','mea_1',
               'mea_2','mea_3','mea_4','veg_1','veg_2','veg_3','veg_4', 'veg_5',
               'leg_1','leg_2','fru_1','fru_2')

minmax_norm <- function(x, ...) { return( (x - min(x,...)) / (max(x,...) - min(x,...)) )}
diet[,c(FFQ_names)] <- sapply(diet[,c(FFQ_names)], minmax_norm, na.rm = TRUE)

foodgroups <- c('veg','leg','fru','cer','fis','mea','dai')

# Compute a sum score per food group and the corresponding binary variable
for (foodgroup in foodgroups) {
  message('\n', toupper(foodgroup))
  
  name_c <- paste0('med_diet_',foodgroup) # Continuous 
  name_b <- paste0('med_diet_',foodgroup,'_bin') # Binary
  
  # Compute continuous (i.e., sum of food group components)
  #if (foodgroup=='leg') { diet[, name_c] <- diet[,'leg_1'] # only one item in the group
  #} else { 
  diet[, name_c] <- rowSums(diet[, grepl(foodgroup, names(diet))], na.rm = F) 
  #  }
  
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
  'cer_1' = full[,'fddd200'], # High fibre breakfast cereals
  'cer_2' = full[,'fddd227'], # Brown and granary bread
  'cer_3' = full[,'fddd229'], # Wholemeal bread
  
  # FISH 
  'fis_1' = full[,'fddd204'], # Other white fish, shellfish, fish dishes
  'fis_2' = full[,'fddd205'], # Oily fish
  
  # DAIRY
  'dai_1' = full[,'fddd206'], # Yoghurt and fromage frais
  'dai_2' = full[,'fddd252'], # Cheese (NOTE: typo in variable label)
  'dai_3' = full[,'fddd254'], # Whole milk 
  'dai_4' = full[,'fddd255'], # Semi-skimmed milk
  'dai_5' = full[,'fddd256'], # Skimmed milk
  'dai_6' = full[,'fddd257'], # Goats and sheeps milk
  
  # MEAT 
  'mea_1' = full[,'fddd215'], # Coated chicken and turkey
  'mea_2' = full[,'fddd216'], # Chicken, turkey and dishes
  'mea_3' = full[,'fddd217'], # Liver and dishes
  'mea_4' = full[,'fddd218'], # Lamb and dishes
  'mea_5' = full[,'fddd219'], # Pork and dishes
  'mea_6' = full[,'fddd220'], # Beef and dishes 
  'mea_7' = full[,'fddd221'], # Burgers and kebabs
  'mea_8' = full[,'fddd222'], # Sausages
  'mea_9' = full[,'fddd223'], # Offal (excluding liver)
  'mea_10' = full[,'fddd224'], # Other meat and meat products
  'mea_11' = full[,'fddd231'], # Butter
  'mea_12' = full[,'fddd238'], # Ham and bacon
  
  # VEGETABLES 
  'veg_1' = full[,'fddd241'], # Raw carrots
  'veg_2' = full[,'fddd242'], # Cooked carrots
  'veg_3' = full[,'fddd243'], # Green leafy vegetables
  'veg_4' = full[,'fddd248'], # Other salad and raw vegetables
  'veg_5' = full[,'fddd249'], # Other cooked vegetables
  'veg_6' = full[,'fddd251'], # Vegetable dishes
  
  # LEGUMES
  'leg_1' = full[,'fddd213'], # Baked beans
  'leg_2' = full[,'fddd244'], # Peas
  'leg_3' = full[,'fddd245'], # Green and runner beans
  'leg_4' = full[,'fddd250'], # Legumes
  
  # FRUIT
  'fru_1' = full[,'fddd246'], # Cooked and canned tomatoes
  'fru_2' = full[,'fddd247'], # Raw tomatoes
  'fru_3' = full[,'fddd261'], # Fruit canned in juice
  'fru_4' = full[,'fddd262'], # Citrus fruit
  'fru_5' = full[,'fddd263'], # Apples and pears
  'fru_6' = full[,'fddd264'], # Bananas
  'fru_7' = full[,'fddd265'], # Other fruit 
  'fru_8' = full[,'fddd267'] # Nuts
)

FFQ_names <- c('cer_1','cer_2','cer_3','fis_1','fis_2','dai_1','dai_2','dai_3',
               'dai_4','dai_5','dai_6','mea_1','mea_2','mea_3','mea_4','mea_5',
               'mea_6','mea_7','mea_8','mea_9','mea_10','mea_11','mea_12','veg_1',
               'veg_2','veg_3','veg_4', 'veg_5','veg_6','leg_1','leg_2','leg_3',
               'leg_4','fru_1','fru_2','fru_3','fru_4','fru_5','fru_6','fru_7','fru_8')

minmax_norm <- function(x, ...) { return( (x - min(x,...)) / (max(x,...) - min(x,...)) )}
diet[,c(FFQ_names)] <- sapply(diet[,c(FFQ_names)], minmax_norm, na.rm = TRUE)

foodgroups <- c('veg','leg','fru','cer','fis','mea','dai')

# Compute a sum score per food group and the corresponding binary variable
for (foodgroup in foodgroups) {
  message('\n', toupper(foodgroup))
  
  name_c <- paste0('med_diet_',foodgroup) # Continuous 
  name_b <- paste0('med_diet_',foodgroup,'_bin') # Binary
  
  # Compute continuous (i.e., sum of food group components)
  #if (foodgroup=='leg') { diet[, name_c] <- diet[,'leg_1'] # only one item in the group
  #} else { 
  diet[, name_c] <- rowSums(diet[, grepl(foodgroup, names(diet))], na.rm = F) 
  #  }
  
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
#------------------------------3c) MOD-FRIENDSHIP-------------------------------
data$f8fs120 <- full[,'f8fs120'] 
# 0: Positive friends score, 15: Negative friends score
# flip
data$friendship <- rev(as.numeric(data$f8fs120))
#-------------------------------3d) MOD-STRESS----------------------------------
# Already merged, no further dataprep needed
#-----------------------------3e) MOD-PARTNER DEP-------------------------------
#  (EPDS)
full$p_dep_18wg <- epds_score(set = c('pb250','pb251','pb253'),
                              revset = c('pb252','pb254','pb255','pb256','pb257','pb258','pb259'),
                              data = full)
full$p_dep_3y <- epds_score(set = c('pf4030','pf4031','pf4033'),
                            revset = c('pf4032','pf4034','pf4035','pf4036','pf4037','pf4038','pf4039'),
                            data = full)
data$p_dep_18wg <- full$p_dep_18wg
data$p_dep_3y <- full$p_dep_3y
#----------------------------3f) MOD-SUBSTANCE USE------------------------------
#  (Alcohol)
data$alc_t1 <- full[,'b721'] #Alcohol consumption in 1-3MTHS, 1 = never
data$alc_t3 <- full[,'e220'] #FREQ of alcohol use in last 2MTHS of PREG, 1 = not at all

#  (Smoking)
data$b670 <- full[,'b670'] 
data$b671 <- full[,'b671'] 
data$c482 <- full[,'c482'] 
#--------------------------------4) COVARIATES----------------------------------
data$assigned_sex <- full$kz021 

# maternal education
data$c645a <- full$c645a

# maternal age
data$m_age_birth <- full$c772

# COB_m/ethnicity
data$ethn_group_m <- full$c800

# BMI
data$height_m <- full$dw021
data$prepreg_weight <- full$dw002
data$prepreg_BMI <- (data$prepreg_weight/((data$height_m/100)^2))
data$prepreg_BMI <- as.numeric(data$prepreg_BMI)

# prepreg_dep
data$prepreg_dep <- full$d171

#age child at outcome
data$outcome_age <- full$ta9991a/12
#---------------------------------5) OUTCOMES-----------------------------------
## Create prorated variables
data$ADHD_symptoms <- full$ta7025c # hyperactivity scale
data$inter_symptoms <- full$ta7025a # emotional scale
data$exter_symptoms <- full$ta7025b # conduct scale
#-------------------------INCOMPLETE CASE DATASET-------------------------------
data_to_impute <- data
datapath <- dirname(file.choose())
#saveRDS(data_to_impute, file.path(datapath, 'ALSPAC_moderator_vars_toimpute_0711.rds'))

