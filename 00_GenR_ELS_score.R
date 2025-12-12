# ------------------------------------------------------------------------------
# All credits go to Serena Defina. Original script is available at 
# https://github.com/SereDef/cumulative-ELS-score
################################################################################
############################  PRENATAL ELS SCORE  ##############################
################################################################################

# The following script builds a dataset with all indicators and domain scores in
# the prenatal cumulative risk score used in Rijlaarsdam et al. (2016) and 
# Schuumans (in preparation), here referred to as : Jolie and Isabel.

# First, let's point to the necessary libraries and define all the functions that 
# are going to be used: readquick, percent_missing, domainscore (also repmeas, 
# bsi_scores, fad_scores that are used in addition in the postnatal stress script)
source("./project_3/code/GENR.0-Setup_and_functions.R")

# ATTENTION! You will be prompted with a window, please navigate to the directory
# where the input data files are stored. Choose any file in the directory to continue.
# Note: the code assumes that all (raw) data is stored in ONE folder. 

# For this version of the score
# You will need the following files (or updated versions from data.managemet)
# GR1001-A_22112016.sav; GR1001-H_22112016.sav; GR1003-A1-7_02092013.sav; GR1003-A8-11_02092013.sav;
# GR1003-C_08042016.sav; GR1003-Family Assessment Device J1-J12_22112016.sav; GR1003-G_01072012.sav;
# GR1003-BSI D1_22112016.sav; GR1005-A_22112016.sav; GR1005-E_22112016.sav
# FETALPERIOD-ALLGENERALDATA_10022025.sav & CHILD-ALLGENERALDATA_10022025.sav

#-------------------------------------------------------------------------------
# I first read in the data and select the necessary columns (i.e. indicators), 
# and dichotomize when necessary. Then I merge them, calculate the five domain 
# scores and save two files: (1) the full dataset and (2) a summary overview of 
# how many risk, no-risk and missing values are present for each indicator. 

# LE = Life Events, CR = Contextual Risk, PR = Parental risk, IR = Interpers. risk

################################################################################
#### ------------------------ reading and merging ------------------------- ####
################################################################################

# NOTE, before we get going: when you call readquick on a file, the function will 
# replace values of 777, 888 or 999 with NAs unless they are IDCs or IDMs (see 
# repleacenas function). If you do NOT want this to happen for any other column 
# (for some reason) use the exclude_col argument with the name of the column that 
# has "real" 777, 888, or 999 values. 

#-------------------------------------------------------------------------------
GR1001v1A<- readquick("GR1001-A_22112016.sav") # 9778 obs. of 17 var

# Construct GR1001A # used for LE domain.
GR1001A <- data.frame('IDM' = GR1001v1A$idm, # pregnancy id
                      'pregnancy_planned' = car::recode(GR1001v1A$a0500101, '0=1; 1=0')) 
# Was this pregnancy planned? (1 = no; 0 = yes)

#-------------------------------------------------------------------------------
GR1001v1H<- readquick("GR1001-H_22112016.sav") # 9778 obs. of 25 var

# Construct GR1001H # used for LE domain.
GR1001H <- data.frame('IDM' = GR1001v1H$idm,
                      'pregnancy_worried' = ifelse(GR1001v1H$h0700701 < 3, yes = 1, no = 0), 
                      # I worry whether the pregnancy will go well or not.
                      # DICH: "Almost always"/"Often" = risk. "Few"/"Sometimes"/"Never" = no risk.
                      'baby_worried' = ifelse(GR1001v1H$h0700301 < 3, yes = 1, no = 0)) 
# I am worried about the health of my baby.
# DICH: "Almost always"/"Often" = risk. "Few"/"Sometimes"/"Never" = no risk.

#-------------------------------------------------------------------------------
GR1003v1A<- readquick("GR1003-A1-7_02092013.sav") # 9778 obs. of 17 var

# Construct GR1003A # used for LE domain.
GR1003A <- data.frame('IDM' = GR1003v1A$idm, 
                      'health' = ifelse(GR1003v1A$a0100103 > 3, yes = 1, no = 0)) 
# Generally how would you describe your health?
# DICH: "Poor"/"Moderate" = risk. "Good"/"Very Good"/"Excellent" = no risk.

#-------------------------------------------------------------------------------
GR1003v2A<- readquick("GR1003-A8-11_02092013.sav") # 9778 obs. of 72 var

# used for LE domain.
GR1003A8 <- data.frame('IDM' = GR1003v2A$idm, 
                       'blood_loss' = ifelse(GR1003v2A$a0801703 < 5, yes = 1, no = 0)) 
# Have you suffered from vaginal blood loss in the past 2 months?
# DICH: "Daily"/"Few days a week"/"Once a week"/"Less than once a week" = risk. "Never" = no risk. 

#-------------------------------------------------------------------------------
GR1003v1C<- readquick("GR1003-C_08042016.sav") # 9778 obs. of 90 var

# First I need to recode some "Not applicable" answers into NAs.
problem_vars <- c('c1200103', # Have you had problems with or at work?
                  'c1300103', # Problems with school or with your studies?
                  'c0600103', # Difficulties between you and your parents in law?
                  'c0800103', # Difficulties between you and one or more brothers or sisters? 
                  'c1000103', # Have there been problems with people who are your friends?
                  'c1500103', # Have you had housing problems in the past year?
                  'c1400103', # Have you had any financial problems in the past year?
                  'c0700103', # Difficulties between you and your partner?
                  'c0500103') # Difficulties in contact with others in the past year?
for (var in problem_vars) { GR1003v1C[, var] <- ifelse(GR1003v1C[, var] == 5, NA, GR1003v1C[, var]) }

# Finally, construct GR1003C
GR1003C <- data.frame(  'IDM' = GR1003v1C$idm, 
                        'family_member_died' = ifelse(GR1003v1C$c0100803 == 0 | GR1003v1C$c0100903 == 0, yes = 1, no = 0), # LE
                        # One of your children died in the last 12 months? YES | OR | Your partner died in the last 12 months? YES
                        'family_member_ill_pregnancy' = ifelse(GR1003v1C$c0100603 == 0 | GR1003v1C$c0100703 == 0, yes = 1, no = 0), # LE
                        # One or more of your children seriously been ill in the last 12 months? YES | OR | Other family member seriously been ill in the last 12 months? YES
                        'work_study_problems' = ifelse(GR1003v1C$c1200103 > 1 | GR1003v1C$c1300103 > 1, yes = 1, no = 0),   # LE
                        # Have you had problems with or at work? | OR | Problems with school or with your studies in the past year? 
                        # DICH: "Slight"/"Moderate"/"Serious" = risk. "No" = no risk.
                        'friend_relative_died' = car::recode(GR1003v1C$c0101003,'0=1; 1=0'), # LE
                        # Member of your family or a good friend died in the last 12 months? (1=yes; 0=no) 
                        'victim_robbery' = car::recode(GR1003v1C$c0100303,'0=1; 1=0'), # LE
                        # Have you been a victim of robbery in the last 12 months? (1=yes; 0=no) 
                        'unemployed' = car::recode(GR1003v1C$c0100403,'0=1; 1=0'), # LE
                        # Have you become unemployed in the last 12 months? (1=yes; 0=no) 
                        'moved_house' = car::recode(GR1003v1C$c0100103,'0=1; 1=0'), # LE
                        # Have you yourself moved house in the last 12 months? (1=yes; 0=no) 
                        'housing_adequacy' = ifelse(GR1003v1C$c1500103 > 1, yes = 1, no = 0), # CR
                        # Have you had housing problems in the past year? 
                        # DICH: "Slight"/"Moderate"/"Serious" = risk. "No" = no risk.
                        'financial_problems' = ifelse(GR1003v1C$c1400103 > 1, yes = 1, no = 0), # CR
                        # Have you had any financial problems in the past year? 
                        # DICH: "Slight"/"Moderate"/"Serious" = risk. "No" = no risk.
                        'income_reduced' = car::recode(GR1003v1C$c0100203,'0=1; 1=0'),  # CR
                        # Downturn in financial situation in the last 12 months? (1=yes; 0=no) 
                        'difficulties_partner' = ifelse(GR1003v1C$c0700103 > 1, yes = 1, no = 0),  # IR
                        # Difficulties between you and your partner?
                        # DICH: "Slight"/"Moderate"/"Serious" = risk. "No" = no risk.
                        'difficulties_family_friend' = ifelse(GR1003v1C$c0600103 > 1 | GR1003v1C$c0800103 > 1 | GR1003v1C$c1000103 > 1, yes = 1, no = 0), # IR
                        # Difficulties between you and your parents in law? | OR |Difficulties between you and one or more brothers or sisters? | OR |
                        # Have there been problems with people who are your friends? 
                        # DICH: "Slight"/"Moderate"/"Serious" = risk. "No" = no risk.
                        'difficulties_contacts' = ifelse(GR1003v1C$c0500103 > 1, yes = 1, no = 0), # IR
                        # Difficulties in contact with others in the past year? 
                        # DICH: "Slight"/"Moderate"/"Serious" = risk. "No" = no risk.
                        'divorce_pregnancy' = car::recode(GR1003v1C$c0101103,'0=1; 1=0'), # IR
                        # Have you had a divorce in the last 12 months? (1=yes; 0=no) 
                        'family_size_pregnancy' = ifelse(GR1003v1C$crowding > 3, yes = 1, no = 0) ) # IR
# With how many people does the pregnant woman live? 
# DICH: more than 3 people = risk. 3 or less people = no risk. 

#-------------------------------------------------------------------------------
GR1003v1J<- readquick("GR1003-Family Assessment Device J1-J12_22112016.sav") # 9778 obs. of 13 vars

# Construct GR1003J
# DICH: |"Totally disagree"/"Disagree" (= NO)| vs |"Fully agree"/"Agree" (= YES)|
GR1003J <- data.frame('IDM' = GR1003v1J$idm,
                      'family_affection' = ifelse(GR1003v1J$j0500103 < 3, yes = 1, no = 0), 
                      # We can express our feelings to each other. NO # IR
                      'family_decisions_problems' = ifelse(GR1003v1J$j0900103 < 3, yes = 1, no = 0), 
                      # We are able to make decisions. NO # IR
                      'family_acceptance' = ifelse(GR1003v1J$j0300103 < 3, yes = 1, no = 0), 
                      # People in our family accept each other. NO # IR
                      'family_acception' = ifelse(GR1003v1J$j0700103 < 3, yes = 1, no = 0), 
                      # People in our family feel accepted. NO # IR
                      'family_trust' = ifelse(GR1003v1J$j1100103 < 3, yes = 1, no = 0), 
                      # We trust each other. NO # IR
                      'family_painful_feelings' = ifelse(GR1003v1J$j0800103 > 2, yes = 1, no = 0), 
                      # A lot of unpleasant and painful feelings. YES # IR
                      'family_decisions' = ifelse(GR1003v1J$j1000103 > 2, yes = 1, no = 0), 
                      # Decision making is a problem in our family. YES # IR
                      'family_conflict' = ifelse(GR1003v1J$j1200103 > 2, yes = 1, no = 0), 
                      # We do not get on well with each other. YES # IR
                      'family_plans' = ifelse(GR1003v1J$j0200103 > 2, yes = 1, no = 0), 
                      # Difficult making plans to do something with family. YES # IR
                      'family_talk_sadness' = ifelse(GR1003v1J$j0400103 > 2, yes = 1, no = 0), 
                      # We cannot talk to each other about any sadness. YES # IR
                      'family_talk_worries' = ifelse(GR1003v1J$j0600103 > 2, yes = 1, no = 0), 
                      # We avoid talking about worries and problems. YES # IR
                      'family_support' = ifelse(GR1003v1J$j0100103 < 3, yes = 1, no = 0) ) 
# If there are problems we can count on each other. NO # IR

#-------------------------------------------------------------------------------
# Psychopathology - mother & father
GR1003dep <- readquick("GR1003-BSI D1_22112016.sav") # 9778 obs of 261 vars
GR1004dep <- readquick("GR1004-BSI G1_22112016.sav") # 9778 obs of 261 

# DICH: cut-off for maternal psychopathology in as provided in addendum to the BSI manual (De Beurs, 2009).
GR1003BSI <- data.frame(   'IDM' = GR1003dep$idm, 
                           'm_depression_pregnancy' = ifelse(GR1003dep$dep >= 0.80, yes = 1, no = 0), # depression for PR
                           'm_anxiety_pregnancy' = ifelse(GR1003dep$anx >= 0.71, yes = 1, no = 0), # anxiety for PR
                           'm_interp_sensitivity_pregnancy' = ifelse(GR1003dep$i_s >= 0.95, yes = 1, no = 0)) # interpersonal sensitivity for PR

# DICH: cut-off for paternal psychopathology in as provided in addendum to the BSI manual (De Beurs, 2009).
GR1004BSI <- data.frame(   'IDM' = GR1003dep$idm, 
                           'p_depression_pregnancy' = ifelse(GR1004dep$dep_p >= 0.71, yes = 1, no = 0), # depression for PR
                           'p_anxiety_pregnancy' = ifelse(GR1004dep$anx_p >= 0.65, yes = 1, no = 0), # anxiety for PR
                           'p_interp_sensitivity_pregnancy' = ifelse(GR1004dep$i_s_p >= 0.78, yes = 1, no = 0)) # interpersonal sensitivity for PR

#-------------------------------------------------------------------------------
GR1003v1G<- readquick("GR1003-G_01072012.sav") # 9778 obs of 22 vars

# First recode a "Do not know" answer into NA.
GR1003v1G$g0300103[GR1003v1G$g0300103 == 2] <- NA # Do you have a criminal record?

# Construct GR1003G
GR1003G <- data.frame('IDM' = GR1003v1G$idm, 
                      'm_criminal_record' = GR1003v1G$g0300103, # Do you have a criminal record? YES # PR
                      'm_violence_property' = ifelse((GR1003v1G$g0100503 + GR1003v1G$g0100603  - 2) > 0, yes = 1, no = 0), # PR
                      # Effectively: Deliberately damaged or vandalized in the past two years ("Once" or "2-3 times") 
                      # | AND/OR | Damaged property of another person in the past two years ("Once" or "2-3 times").
                      # In GenR sample, none of the mothers committed neither crime "4-5 times" or "more than 6 times" 
                      # but both of those options were possible answers in the questionnaire.
                      # DICH: "Once"/"2-3 times"/"4-5 times"/"more than 6 times" = risk. "Never" = no risk.
                      'm_violence_people' = ifelse((GR1003v1G$g0101603 + GR1003v1G$g0101703 + GR1003v1G$g0101803 - 3) > 0, yes = 1, no = 0)) # PR
# Threatened anyone in the past two years | AND/OR | Hit anyone so hard that he or 
# she was injured in the past two years | AND/OR | Injured anyone with a knife or 
# weapon in the past two years ("Once"/"2-3 times"/"4-5 times"/"more than 6 times")
# DICH: "Once"/"2-3 times"/"4-5 times"/"more than 6 times" = risk. "Never" = no risk.

# Paternal variables are NOT USED IN THIS SCORE but can be integrated provided 
GR1004v1I <- readquick("GR1004-I_01072012.sav")
# Construct GR1003G
GR1004I <- data.frame('IDM' = GR1004v1I$idm, 
                      'p_criminal_record' = ifelse(GR1004v1I$i0300104 == 1 | GR1004v1I$i0200104 == 1, 1, 
                                                   ifelse(GR1004v1I$i0300104 == 0 & GR1004v1I$i0200104 == 0, 0, NA)))
# Do you have a criminal record? | Have you ever been arrested by the police? YES # PR

#-------------------------------------------------------------------------------
GR1005v1A<- readquick("GR1005-A_22112016.sav") # 9778 obs of 17 vars

# Construct GR1005A # used for LE domain.
GR1005A <- data.frame('IDM' = GR1005v1A$idm,
                      'admitted_to_hospital' = GR1005v1A$a0300105, 
                      # Admitted to a hospital (>24 hours) during this pregnancy? YES
                      'examination' = ifelse(GR1005v1A$a0100105 == 1 | GR1005v1A$a0100205 == 1 | GR1005v1A$a0100305 == 1 | GR1005v1A$a0100405 == 1, yes = 1, no = 0),
                      # Chorionic villus sampling test during this pregnancy? YES | OR | Aminiocentesis 
                      # test during this pregnancy? YES | OR | Triple (maternal serum) test during this 
                      # pregnancy? YES | OR | Ultrasound for nuchal translucency test during this pregnancy? YES
                      'obstetric_care' = ifelse(GR1005v1A$a0600105 > 2, yes = 1, no = 0)) 
# General satisfaction about the obstetric care so far
# DICH: "Very dissatisfied"/"Somewhat dissatisfied" = risk. "Very satisfied"/"Fairly satisfied" = no risk.


#-------------------------------------------------------------------------------
GR1005v1E <- readquick("GR1005-E_22112016.sav") # 9778 obs of 40 vars

# Construct GR1005E # used for CR domain.
GR1005E <- data.frame('IDM' = GR1005v1E$idm,
                      'housing_basic_living' = ifelse(GR1005v1E$e0500305 == 2 | GR1005v1E$e0501105 == 2 | GR1005v1E$e0501305 == 2, yes = 1, no = 0), 
                      # Possession of adequate heating in your home in cold weather. NO | OR |
                      # Possession of a washing machine. NO | OR | Possession of a refrigerator. NO. 
                      'housing_defects' = ifelse(GR1005v1E$e0800505 == 1 | GR1005v1E$e0800605 == 1 | GR1005v1E$e0800705 == 1, yes = 1, no = 0), 
                      # Often had problems with cold or draft in your home in the last year? YES | OR | 
                      # Often had problems with misted windows in your sitting room in the last year? YES | OR | 
                      # Often had problems with damp patches, mould on the wall, furniture in the last year? YES
                      'trouble_pay_pregnancy' = ifelse(GR1005v1E$e0400105 > 1, yes = 1, no = 0)) 
# Difficulty in paying for food, rent, electricity bill and suchlike in the past year?
# DICH: "Great difficulty"/"Some difficulty" = risk. "No difficulty at all" = no risk. 

#-------------------------------------------------------------------------------
fetal_general <- readquick("FETALPERIOD-ALLGENERALDATA_10022025.sav") # 9778 obs of 95 var

demogr <- data.frame('IDM' = fetal_general$idm, 
                     'early_pregnancy' = ifelse(fetal_general$age_m_v2 < 19, yes = 1, no = 0), 
                     # Mother younger than 19 years at intake (for PR)
                     # DICH: based on Cecil et al. (2014); Rijlaarsdam et al. (2016).
                     'marital_status_pregnancy' = fetal_general$mardich, # Marital status: "No partner" (for IR)
                     'm_education_pregnancy' = ifelse(fetal_general$educm <= 3, yes = 1, no = 0), # Highest education finished mother (for CR)
                     'p_education_pregnancy' = ifelse(fetal_general$educp <= 3, yes = 1, no = 0)) # Highest education finished partner (for CR)
# DICH: "No education"/"Primary"/"Secondary-phase 1"/"Secondary-phase 2" = risk. "Higher-phase 1"/"Higher-phase 2" = no risk.
# based on Centraal Bureau voor de Statistiek (2016). 

#-------------------------------------------------------------------------------
# This function merges together all separate dataframes according to the IDM 
# results in a dataframe with all relevant items for prenatal stress.
# tech-tip: because merge can only take two dataframes at the time and I am lazy, I use Reduce.
prenatal_stress <- Reduce(function(x,y) merge(x = x, y = y, by = 'IDM',  all.x = T),
                          list(GR1001A, GR1001H, GR1003A, GR1003A8, GR1003C, GR1003J, 
                               GR1003BSI, GR1004BSI, GR1003G, GR1004I, GR1005A, GR1005E, demogr)) 

#-------------------------------------------------------------------------------
# In order to later merge the dataset with postnatal variables, I link the pregnancy
# id (idm) to the child id (idc). 
child_general <- readquick("CHILD-ALLGENERALDATA_10022025.sav") # 9901 obs of 122 
child_id <- data.frame('IDM' = child_general$idm, 'IDC' = child_general$idc)

# ATTENTION! Only need to run this line if you are using the prenatal score together
# with postnatal outcomes/scores. Note: it will change the number of observations (from 9778 to 9901)
prenatal_stress <- merge(child_id, prenatal_stress, by = 'IDM', all.x = T)

################################################################################
# Before we get to the final scores, some summary stats that may come in handy #

# Let's have a look at risk distribution and missing data per indicator.
prenatal_summary = data.frame(row.names=c("no risk","risk","NA","%risk","%miss"))
for (i in 3:ncol(prenatal_stress)) { # ATTENTION, if not merged with child_id, count from 2.
  s = summary(as.factor(prenatal_stress[,i]))
  c = colnames(prenatal_stress)[i]
  prenatal_summary[1:3,c] <- s
  prenatal_summary[4,c] <- round((prenatal_summary[2,c] / nrow(prenatal_stress))*100, 2)
  prenatal_summary[5,c] <- round((prenatal_summary[3,c] / nrow(prenatal_stress))*100, 2)
}

#-------------------------------------------------------------------------------
# Apply the percent_missing function to the rows (1) of the entire dataset (total 50 indicators)
prenatal_stress$pre_percent_missing = apply(prenatal_stress[,3:ncol(prenatal_stress)], # Same as above, if not merged with child_id, count from 2.
                                            1, percent_missing)

################################################################################
#### -------------- create the (un-weighted) domain scores ---------------- ####
################################################################################

# ATTENTION! Here we use de default argument of domainscore function: calculating  a 
# *mean domain score* (range = 0 to 1) when missingness is < 25%. If you prefer 
# working with the actual number of risks (i.e. sum score) or the weighted version 
# of it as in e.g. Rijlaarsdam et al. (2016), you can set the argument score_type
# to 'sum_simple' or 'sum_weighted' respectively (see Setup and functions script
# for calculation details). 

# LE
prenatal_stress[,c('pre_LE_percent_missing','pre_life_events')] <- domainscore(prenatal_stress[,c(
  "family_member_died", # One of your children |OR| Your partner died in the last 12 months? YES
  "friend_relative_died", # Member of your family or a good friend died in the last 12 months? YES
  "family_member_ill_pregnancy", # One or more of your children |OR| Other family member seriously been ill in the last 12 months? YES
  "admitted_to_hospital", # Admitted to a hospital (>24 hours) during this pregnancy? YES
  "health", # Generally how would you describe your health? "Poor"/"Moderate" = risk. "Good"/"Very Good"/"Excellent" = no risk.
  "unemployed", # Have you become unemployed in the last 12 months? YES
  "work_study_problems", # Have you had problems with or at work? |OR| Problems with school or with your studies in the past year? "Slight"/"Moderate"/"Serious".
  "moved_house", # Have you yourself moved house in the last 12 months? YES
  "blood_loss", # Have you suffered from vaginal blood loss in the past 2 months? "Daily"/"Few days a week"/"Once a week"/"Less than once a week" = risk. "Never" = no risk. 
  "examination", # Chorionic villus sampling test |OR| Aminiocentesis test |OR| Triple (maternal serum) test |OR| Ultrasound for nuchal translucency test during this pregnancy? YES
  "baby_worried", # I am worried about the health of my baby. "Almost always"/"Often" = risk. "Few"/"Sometimes"/"Never" = no risk.
  "pregnancy_worried", # I worry whether the pregnancy will go well or not. "Almost always"/"Often" = risk. "Few"/"Sometimes"/"Never" = no risk.
  "obstetric_care", # General satisfaction about the obstetric care so far. "Very dissatisfied"/"Somewhat dissatisfied" = risk. "Very satisfied"/"Fairly satisfied" = no risk.
  "pregnancy_planned", # Was this pregnancy planned? NO
  "victim_robbery")]) # Have you been a victim of robbery in the last 12 months? (1=yes; 0=no) # LE

# CR
prenatal_stress[,c('pre_CR_percent_missing','pre_contextual_risk')] <- domainscore(prenatal_stress[,c(
  "financial_problems", # Have you had any financial problems in the past year? Slight"/"Moderate"/"Serious" = risk. "No" = no risk.
  "trouble_pay_pregnancy", # Difficulty in paying for food, rent, electricity bill and suchlike in the past year? "Great difficulty"/"Some difficulty" = risk. "No difficulty at all" = no risk.
  "income_reduced", # Downturn in financial situation in the last 12 months? YES
  "housing_defects", # Often had problems with cold or draft in your home |OR| misted windows in your sitting room |OR| damp patches, mold on the wall, furniture in the last year? YES
  "housing_adequacy", # Have you had housing problems in the past year? "Slight"/"Moderate"/"Serious" = risk. "No" = no risk.
  "housing_basic_living", # Possession of adequate heating in your home in cold weather |OR| a washing machine |OR| a refrigerator
  "m_education_pregnancy", # Highest education finished "No education"/"Primary"/"Secondary-phase 1"/"Secondary-phase 2" = risk. "Higher-phase 1"/"Higher-phase 2" = no risk.
  "p_education_pregnancy")]) # Highest education finished "No education"/"Primary"/"Secondary-phase 1"/"Secondary-phase 2" = risk. "Higher-phase 1"/"Higher-phase 2" = no risk.

# PR
prenatal_stress[,c('pre_PR_percent_missing','pre_parental_risk')] <- domainscore(prenatal_stress[,c(
  "early_pregnancy", # Mother younger than 19 years at intake. 
  "m_depression_pregnancy", # BSI depression score > 0.80 (De Beurs, 2009).
  "m_anxiety_pregnancy", # BSI anxiety score > 0.71 (De Beurs, 2009).
  "m_interp_sensitivity_pregnancy", # BSI interpersonal sensitivity score > 0.95 (De Beurs, 2009).
  "p_depression_pregnancy", # BSI depression score > 0.78 (De Beurs, 2009).
  "p_anxiety_pregnancy", # BSI anxiety score > 0.65 (De Beurs, 2009).
  "p_interp_sensitivity_pregnancy", # BSI interpersonal sensitivity score > 0.71 (De Beurs, 2009).
  "m_violence_people", # Threatened anyone |OR| Hit anyone so hard that he or she was injured |OR| Injured anyone with a knife or weapon in the past two years ("Once"/"2-3 times"/"4-5 times"/"more than 6 times")
  "m_violence_property", # Deliberately damaged or vandalised |OR| Damaged property of another person in the past two years ("Once" or "2-3 times").
  "m_criminal_record", # Do you have a criminal record? YES
  "p_criminal_record")]) # Do you have a criminal record? | Have you ever been arrested by the police? YES # PR

# IR
prenatal_stress[,c('pre_IR_percent_missing','pre_interpersonal_risk')] <- domainscore(prenatal_stress[,c(
  "difficulties_contacts", # Difficulties in contact with others in the past year? "Slight"/"Moderate"/"Serious" = risk. "No" = no risk.
  "difficulties_partner", # Difficulties between you and your partner? "Slight"/"Moderate"/"Serious" = risk. "No" = no risk.
  "difficulties_family_friend", # Difficulties between you and your parents in law? |OR| between you and one or more brothers or sisters? |OR| problems with people who are your friends? "Slight"/"Moderate"/"Serious"
  "marital_status_pregnancy", # Marital status: "No partner"
  "divorce_pregnancy", # Have you had a divorce in the last 12 months? YES
  "family_support", # If there are problems we can count on each other. NO
  "family_acceptance", # People in our family accept each other. NO
  "family_affection", # We can express our feelings to each other. NO
  "family_acception", # People in our family feel accepted. NO
  "family_trust", # We trust each other. NO
  "family_painful_feelings", # A lot of unpleasant and painful feelings. YES
  "family_decisions", # Decision making is a problem in our family. YES
  "family_conflict", # We do not get on well with each other. YES
  "family_decisions_problems", # We are able to make decisions. NO
  "family_plans", # Difficult making plans to do something with family. YES
  "family_talk_sadness", # We cannot talk to each other about any sadness. YES
  "family_talk_worries", # We avoid talking about worries and problems. YES
  "family_size_pregnancy")]) # With how many people does the pregnant woman live? >3 people = risk. <= 3 people = no risk.

################################################################################
# Set all binary variables to class: factor
# nonbinary_cols <- c("IDC", "IDM", "pre_percent_missing", "pre_LE_percent_missing", "pre_life_events",           
#                     "pre_CR_percent_missing", "pre_contextual_risk", "pre_PR_percent_missing", "pre_parental_risk",      
#                     "pre_IR_percent_missing", "pre_interpersonal_risk")
# 
# prenatal_stress[!names(prenatal_stress) %in% nonbinary_cols] <- 
#   lapply(prenatal_stress[!names(prenatal_stress) %in% nonbinary_cols], factor) 


################################################################################
#### -------------- Construct CUMULATIVE STRESS variables ----------------- ####
################################################################################

# compute sum scores for prenatal stress exposure - EXL. PARENTAL RISK
prenatal_stress$prenatal_stress <- rowSums(prenatal_stress[,c("pre_life_events", 
                                                              "pre_contextual_risk", 
                                                              "pre_interpersonal_risk")], na.rm = F)

################################################################################
#### --------------------------- save and run ----------------------------- ####
################################################################################

# Save the dataset in an .rds file, in the directory where you have the raw data
#saveRDS(prenatal_stress, file.path(pathtoresults,'prenatal_stress_GENR.rds'))
#saveRDS(prenatal_summary, file.path(pathtoresults,'prenatal_stress_GENR_summary.rds'))

################################################################################
###########################  POSTNATAL ELS SCORE  ##############################
################################################################################

# The following script builds a dataset with all indicators and domain scores in
# the postnatal cumulative risk score used in Schuumans (in preparation). 

# First, let's point to the necessary libraries and define all the functions that 
# are going to be used: readquick, percent_missing, repmeas, domainscore, 
# bsi_scores & fad_scores.
source("./project_3/code/GENR.0-Setup_and_functions.R") 

# ATTENTION! You will be prompted with a window, please navigate to the directory
# where the input data files are stored. Choose any file in the directory to continue.
# Note: the code assumes that all (raw) data is stored in ONE folder. 

# For this version of the score
# You will need the following files (or updated versions from data.managemet)
# Parenting 3 years of age_GR1065 F1F6 -GR1066 B1-B5_22112016.sav
###### !!! ATTENTION !!! Before you run this, you need to run the one SPSS Syntax. 
# (V:\Promovendi\Datasets\Gedragsgroep\Questionnaire data\After birth\0-3 years\Harsh parenting_Parent Child Conflict Tactics Scale\Syntax harsh parenting 3jr_PJ10012011)
# or most recent version, on this file. 
# BSI 3 years of age_GR1065 G1-GR1066 C1_22112016.sav; 
# GR1065-G2_01072012.sav; GR1065-G3-6_01072012.sav; GR1065-X_01072012.sav; 
# SESFOLLOWUP_03022020.sav;  PARENTEMPLOYMENT5_13082012.sav; GR1075-B3_17072015.sav; GR1075-B4_17072015.sav;
# CHILDTRF_incl_Tscores_20201111.sav; GR1080-C10-11_04042017.sav; GR1080-E_Bullying_17072015.sav;
# GR1081_E1-3_5-6_08082016.sav; GR1081_E4_30082016.sav; GR1081_I1-9_08082016.sav;
# GR1081-GR1083_D1_BSI_19042017.sav; GR1082_C1-5_22092016.sav; MOTHERTRAUMAINTERVIEW9_24112016.sav
# GR1083_E7_19102016.sav and FETALPERIOD-ALLGENERALDATA_10022025.sav

#-------------------------------------------------------------------------------
# I first read in the data and select the necessary columns (i.e. indicators), 
# and dichotomize when necessary. Then I merge them, calculate the five domain 
# scores and save two files: (1) the full dataset and (2) a summary overview of 
# how many risk, no-risk and missing values are present for each indicator. 

# LE = Life Events, CR = Contextual Risk, PR = Parental risk, IR = Interpersonal risk
# DV = Direct Victimization

################################################################################
#### ----------------------- reading in the data  ------------------------- ####
################################################################################

# NOTE, before we get going: when you call readquick on a file, the function will 
# replace values of 777, 888 or 999 with NAs unless they are IDCs or IDMs (see 
# repleacenas function). If you do NOT want this to happen for any other column 
# (for some reason) use the exclude_col argument with the name of the column that 
# has "real" 777, 888, or 999 values. 

#-------------------------------------------------------------------------------
## Life Events Interview (9 yr)
# some of the tot 26 items are used mainly for lE domain but also for IR, DV, CR. 
LE_interview <- readquick("MOTHERTRAUMAINTERVIEW9_24112016.sav") # 5862 obs of 96 columns

# Exclude unreliable interviews
LE_interview <- subset(LE_interview, LE_interview$unreliable == 0) # 5570 obs of 96 columns

# Select the necessary item 
life_events <- data.frame('IDC' = LE_interview$idc, # child id 
                          'sick_or_accident' = LE_interview$le1,  # Did your child get seriously sick or did he/she have an accident? Yes
                          'family_member_ill' = LE_interview$le2,  # Did a family member get seriously sick or a serious accident? Yes
                          'smbd_important_ill' = LE_interview$le3,  # Did somebody else, who is important to your child, get seriously sick or have a serious accident? Yes
                          'parent_died' = LE_interview$le4,  # Is the father/mother or other caretaker still alive? No 
                          'smbd_important_died' = LE_interview$le5,  # Did somebody else, who your child cared about a lot, die? Yes
                          'pet_died' = LE_interview$le6,  # Did ever a pet, who you child cared about a lot, die? Yes
                          'school_workload' = LE_interview$le7,  # Does or did your child have to deal with a high workload at school? Yes
                          'rep_grade_9yrs' = LE_interview$le8,  # Did your child ever repeat a grade? Yes
                          'neiborhood_problems' = LE_interview$le9,  # Are/were there problems in the neighborhood? (e.g. vandalism or insecurity); yes
                          'fidi_9yrs' = LE_interview$le10, # Does your family have or ever had financial difficulties? Yes 
                          'conflict_family_member' = LE_interview$le11, # Has your child ongoing conflicts with a family member (or ever had them)? Yes
                          'conflict_smbd_else' = LE_interview$le12, # Has your child ongoing conflict with somebody else (or ever had them)? Yes 
                          'conflict_in_family' = LE_interview$le13, # Do other family members have ongoing conflicts with each other (or ever had them)? Yes
                          'divorce_childhood' = LE_interview$le14, # Are you and your partner divorced or separated? Yes
                          'argument_friend' = LE_interview$le16, # Did your child lose a good friend due to an argument? Yes
                          'lost_smth_important' = LE_interview$le17, # Did your child ever lose something which was important to him/her? Yes
                          'physical_violence' = LE_interview$le18, # Did someone ever use physical violence against your child? For example, beating him/her up. Yes
                          'physical_threats' = LE_interview$le19, # Did somebody use almost physical violence against your child? So that it did not actually happen, but the child was frightened. Yes
                          'sexual_harrasment' = LE_interview$le20, # Did anybody make sexual comments or movements towards your child? Yes 
                          'sexual_behavior' = LE_interview$le21, # Did your child participate in inappropriate sexual behavior? Yes
                          'rumors_or_gossip' = LE_interview$le22, # Did somebody spread mean rumors or gossip about your child? Yes
                          'moved' = LE_interview$le23, # Did your child move? Yes 
                          'changed_school' = LE_interview$le24) # Did your child change schools? Yes

#-------------------------------------------------------------------------------
## Parenting behavior (3 yr)

# ATTENTION! The code assumes you have run the SPSS Syntax that calculates 80th percentile harsh parenting scores.
# (V:\Promovendi\Datasets\Gedragsgroep\Questionnaire data\After birth\0-3 years\Harsh parenting_Parent Child Conflict Tactics Scale\Syntax harsh parenting 3jr_PJ10012011)
# or most recent version.
parentingv1 <- readquick("New Parenting 3 years of age_GR1065 F1F6 -GR1066 B1-B5_22112016.sav") # 9897 obs of 130 columns

# NOTE: There are no official guidelines for dichotomization of the Parent Child Conflict Tactics scale. 
# We based dichotomization on previous work in Gen R (Jansen et al., 2017): i.e. 80th percentile split. 

# Construct GR1065 # used for DV domain.
parenting <- data.frame('IDC' = parentingv1$idc, 
                        'p_harsh_parent' = parentingv1$harsh80_p, 
                        'm_harsh_parent' = parentingv1$harsh80_m)

#-------------------------------------------------------------------------------
## GR1065-G2 (3 yrs)
GR1065v1 <- readquick("GR1065-G2_01072012.sav") # 9901 obs. of  28 

# Construct GR1065 # items used for LE (mainly), CR and IR.
GR1065 <- data.frame('IDC' = GR1065v1$idc, 
                     'friend_moved' = GR1065v1$g0200365, # Did one of your child’s friends move to a new house? Yes
                     'fire_or_burglary' = GR1065v1$g0201165, # Was there a fire or burglary? Yes 
                     'fidi_3yrs' = GR1065v1$g0200765, # financial problems (with le10)
                     'empl_3yrs' = GR1065v1$g0202365, # unemployment in thew last 2 years (with GR075 and GR1081)
                     'tension_at_work' = GR1065v1$g0200565, # Tension at the parents’ work that has been felt at home; yes
                     'marital_problems' = GR1065v1$g0201965) # Problems with marriage relations; yes

#-------------------------------------------------------------------------------
## GR1065-G3-6 (3 yrs)
GR1065v1G <- readquick("GR1065-G3-6_01072012.sav") # 9901 obs. of  9 

# Construct GR1065 # items used for CR and IR.
GR1065G <- data.frame('IDC' = GR1065v1G$idc, 
                      'trouble_pay_3yrs' = ifelse(GR1065v1G$g0600165 > 1, yes = 1, no = 0), # difficulty paying rent/bills (with GR1081)
                      # DICH: ‘no problems at all’ = no risk, ‘few problems’ to ‘a lot problems’ = risk.
                      'income_3yrs' = ifelse(GR1065v1G$g0300165 < 5, yes = 1, no = 0),  # household income
                      # DICH: according to the Central Statistic Netherlands (2013). 
                      # Net household income below 1600 €/month (basic needs level) = risk. 
                      'fam_size_3yrs' = ifelse(GR1065v1G$g0400265 > 3, yes = 1, no = 0)) # how many children in the household
# DICH: based on Cecil et al. (2014); Rijlaarsdam et al, (2016).
# Family of more than three children = risk. 

#-------------------------------------------------------------------------------
## GR1065X (3 yrs)
GR1065v1X <- readquick("GR1065-X_01072012.sav") # 9901 obs. of 5

# Construct GR1065 # item used for IR.
GR1065X <- data.frame('IDC' = GR1065v1X$idc, 
                      'marital_status_3yrs' = ifelse(GR1065v1X$x0500165 > 1, yes = 1, no = 0)) # do you have a partner present?
# DICH: based on Cecil et al. (2014); Rijlaarsdam et al, (2016).
# Having no partner or having a partner without living together = risk.

#-------------------------------------------------------------------------------
## GR1075 (3-6 yrs)
GR1075v1 <- readquick("SESFOLLOWUP_03022020.sav") # 9901 obs. of  10 variables # ATTENTION: this is different from the file Isabel used

GR1075 <- data.frame('IDC' = GR1075v1$idc, 
                     'm_educ_3yrs' = ifelse(GR1075v1$educm3 <= 3, yes = 1, no = 0), # maternal education 3 yrs (for PR)
                     'm_educ_5yrs' = ifelse(GR1075v1$educm5 <= 3, yes = 1, no = 0), # maternal education 5 yrs (for PR)
                     'p_educ_3yrs' = ifelse(GR1075v1$educp3 <= 3, yes = 1, no = 0), # paternal education 3 yrs (for PR)
                     'p_educ_5yrs' = ifelse(GR1075v1$educp5 <= 3, yes = 1, no = 0), # paternal education 5 yrs (for PR)
                     # DICH: according to Statistics Netherlands (2016), 
                     # an educational level of secondary phase 2 or below = risk.
                     'marital_status_5yrs' = GR1075v1$mar_dich5, # Marital status: single (with GR1065 and le14 in IR)
                     'income_5yrs' = ifelse(GR1075v1$income5 < 4, yes = 1, no = 0)) # household income (for CR)
# DICH: according to the Central Statistic Netherlands (2013). 
# Net household income below 1600 €/month (basic needs level) = risk.

#-------------------------------------------------------------------------------
## GR1075B3 (5 yrs)
GR1075v1B3 <- readquick("GR1075-B3_17072015.sav") # 9901 obs. of  10 : family composition

# Construct GR1075 # used in IR
GR1075B3 <- data.frame('IDC' = GR1075v1B3$idc, 
                       'fam_size_5yrs' = ifelse(GR1075v1B3$children_household_clean > 3, yes = 1, no = 0)) # Family size > 3 children
# DICH: based on Cecil et al. (2014); Rijlaarsdam et al, (2016).

#-------------------------------------------------------------------------------
## GR1075 - employment (5 yrs)
GR1075v1E <- readquick("PARENTEMPLOYMENT5_13082012.sav") # 9901 obs. of  7 

# Construct GR1075
GR1075E <- data.frame('IDC' = GR1075v1E$idc, 
                      'm_empl_5yrs' = ifelse(GR1075v1E$b0500175_clean <= 3, yes = 0, no = 1), # mother occupation
                      'p_empl_5yrs' = ifelse(GR1075v1E$b1200175_clean <= 3, yes = 0, no = 1)) # father occupation
# DICH: unemployed if described themselves other than ‘paid worker’ or ‘self-employed’ 
# (e.g. 'student', 'welfare recipient', 'disabled', 'volunteer work' = risk)

#-------------------------------------------------------------------------------
## GR1079 (7 yrs) - bullying assessed by teacher
GR1079v1 <- readquick("CHILDTRF_incl_Tscores_20201111.sav") # 7580 obs. of  336

# Construct GR1079 # used in DV score 
GR1079 <- data.frame('IDC' = GR1079v1$idc, 
                     'bully_physical_t' = ifelse(GR1079v1$d0100179 >= 3, yes = 1, no = 0), # physical bullying (hit, kick, pinch, bite) according to the teacher
                     'bully_verbal_t' = ifelse(GR1079v1$d0100279 >= 3, yes = 1, no = 0), # verbal bullying (laugh at, insult, tease) according to the teacher
                     'bully_excluded_t' = ifelse(GR1079v1$d0100379 >= 3, yes = 1, no = 0)) # excluded, according to the teacher
# DICH: based on previous work on bullying in Gen R (Muetzel et al., 2019).
# If child was verbally/physically/relationally bullied once a week or more = risk. 

#-------------------------------------------------------------------------------
## GR1080E (8 yrs) - bullying assessed by main caregiver
GR1080v1E <- readquick("GR1080-E_Bullying_17072015.sav") # 9901 obs. of  11 

# First I need to recode some weird SPSS missing codes into NAs
problem_vars <- c('e0100280', 'e0100380', 'e0100480')
for (var in problem_vars) { GR1080v1E[, var] <- ifelse(GR1080v1E[, var] == 88, NA, GR1080v1E[, var]) }

# Construct GR1080 # used in DV score
GR1080E <- data.frame('IDC' = GR1080v1E$idc, 
                      'bully_physical_m' = ifelse(GR1080v1E$e0100280 >= 4, yes = 1, no = 0), # how often your bullied (insulted, calling names or laughed at)
                      'bully_verbal_m' = ifelse(GR1080v1E$e0100380 >= 4, yes = 1, no = 0), # how often your bullied (spitting, hitting, kicking or pinching)
                      'bully_excluded_m' = ifelse(GR1080v1E$e0100480 >= 4, yes = 1, no = 0)) # how often your bullied (excluded, ignored or gossiped about)
# DICH: based on previous work on bullying in Gen R (Muetzel et al., 2019).
# If child was verbally/physically/relationally bullied once a week or more = risk. 

#-------------------------------------------------------------------------------
## GR1080 (8 yrs)
GR1080v1C <- readquick("GR1080-C10-11_04042017.sav") # 9901 obs. of  23 (academic performance)

# Construct GR1080 # used in LE
GR1080C <- data.frame('IDC' = GR1080v1C$idc,
                      'rep_grade_8yrs' = GR1080v1C$cleaned_c1001280v2) # has your child repeated any grades?

#-------------------------------------------------------------------------------
## GR1082 (10 yrs)
GR1082v1 <- readquick("GR1082_C1-5_22092016.sav") # 9901 obs. of  16 (academic performance)

# Construct GR1082 # used in LE
GR1082 <- data.frame('IDC' = GR1082v1$idc, 
                     'rep_grade_10yrs' = GR1082v1$c0300182_cleaned) # has your child repeated any grades?

#-------------------------------------------------------------------------------
## GR1081 E3-6 (10 yrs)
GR1081v1 <- readquick("GR1081_E1-3_5-6_08082016.sav") # 9901 obs. of  35 

# Construct GR1081 # used in CR (mainly) and in IR (family size)
GR1081 <- data.frame('IDC' = GR1081v1$idc, 
                     'trouble_pay_9yrs' = ifelse(GR1081v1$e0600181_v2 > 1, yes = 1, no = 0), # trouble paying for food, rent, electricity
                     # DICH: ‘no problems at all’ = no risk, ‘few problems’ to ‘a lot problems’ = risk.
                     'mat_depr_heating' = car::recode(GR1081v1$e0100281_v2,'0=1; 1=0'), # Sufficient heating in house during cold weather # INVERSE
                     'mat_depr_rent' = car::recode(GR1081v1$e0100381_v2,'0=1; 1=0'), # pay rent or mortgage without problems # INVERSE
                     'mat_depr_meal' = car::recode(GR1081v1$e0100481_v2,'0=1; 1=0'), # on average one hot meal per day # INVERSE
                     'mat_depr_car' = car::recode(GR1081v1$e0100581_v2,'0=1; 1=0'), # own or lease a car # INVERSE
                     'mat_depr_washingmachine' = car::recode(GR1081v1$e0100681_v2,'0=1; 1=0'), # own a washing machine # INVERSE
                     'mat_depr_refrigerator' = car::recode(GR1081v1$e0100781_v2,'0=1; 1=0'), # own a refrigerator # INVERSE
                     'mat_depr_telephone' = car::recode(GR1081v1$e0100881_v2,'0=1; 1=0'), # own a telephone # INVERSE
                     'mat_depr_holidays' = car::recode(GR1081v1$e0101181_v2,'0=1; 1=0'), # holidays away from home 1 or more weeks per year # INVERSE
                     # the above items are taken from the Material deprivation questionnaire.
                     'income_9yrs' = ifelse(GR1081v1$e0200181_v2 < 4, yes = 1, no = 0), # household income per month
                     # DICH: according to the Central Statistic Netherlands (2013). 
                     # Net household income below 1600 €/month (basic needs level) = risk.
                     'fam_size_9yrs' = ifelse(GR1081v1$e0200381_v2 > 3, yes = 1, no = 0)) # how many children live with this income.
# DICH: based on Cecil et al. (2014); Rijlaarsdam et al, (2016).
# Family of more than three children = risk. 

#-------------------------------------------------------------------------------
## GR1081 - employment (10 yrs)
GR1081v1E4 <- readquick("GR1081_E4_30082016.sav")

# Construct GR1081E
GR1081E4 <- data.frame('IDC' = GR1081v1E4$idc, 
                       'empl_9yrs' = ifelse(GR1081v1E4$e0400181_v2 > 1, yes = 1, no = 0)) # do you receive any benefits?
# any kind of benefit (social, unemployment or disability allowances) = risk.

#-------------------------------------------------------------------------------
## GR1081 I - TV (10 yrs)
GR1081v1I <- readquick("GR1081_I1-9_08082016.sav") # 9901 obs. of  29

# Construct GR1081 I # used in CR
GR1081I <- data.frame('IDC' = GR1081v1I$idc, 
                      'mat_depr_tv' = ifelse(GR1081v1I$i0100181_cleaned < 2, yes = 1, no = 0)) # how many televisions in the house?
# DICH: 1 = no television, 0 = at least one television.
# This item used in the material deprivation index, together with variables from the 
# Material deprivation questionnaire, to increase resemblance with EU-SILC guidelines.

#-------------------------------------------------------------------------------
# Brief Symptoms Inventory (3 and 9 yrs)
BSI3 <- readquick("BSI 3 years of age_GR1065 G1-GR1066 C1_22112016.sav")
BSI9 <- readquick("GR1081-GR1083_D1_BSI_19042017.sav")
BSI  <- merge( BSI3, BSI9, by = 'idc', all.y = T)

# Psychopathology includes three syndrome domains: interpersonal sensitivity, 
# depression and anxiety, measured in main caregivers and partners at the ages 3 and 9.
# DICH: according to the Dutch BSI manual (De Beurs, 2009). Cutoffs values denote the 
# transition from health to sickness and are gender and scale specific. 

# Construct the set to add to the dataset
BSI_dich <- data.frame('IDC' = BSI$idc, 'IDM' = BSI$idm,
                       # INTERPERSONAL SENSITIVITY: Items 20, 21, 22, and 42
                       'm_is_3yrs' = bsi_scores(items = c('g0101065', 'g0101165', 'g0101265', 'g0101765'), 
                                                filledinby = BSI$gr1065_filledinby,
                                                cutoff = c(.95, .78, .78, 1.12, 999)), 
                       'm_is_9yrs' = bsi_scores(items = c('d0101081_cleaned', 'd0101181_cleaned', 'd0101281_cleaned', 'd0101781_cleaned'),
                                                filledinby = BSI$gr1081_filledinby,
                                                cutoff = c(.95, .95, .78, .78, 1.12)), 
                       'f_is_3yrs' = bsi_scores(items = c('c0101066', 'c0101166', 'c0101266', 'c0101766'),
                                                filledinby = BSI$gr1066_filledinby,
                                                cutoff = c(.78, .78, .95, 1.12, 999)), 
                       'f_is_9yrs' = bsi_scores(items = c('d0101083_cleaned', 'd0101183_cleaned', 'd0101283_cleaned', 'd0101783_cleaned'),
                                                filledinby = BSI$gr1083_filledinby,
                                                cutoff = c(.78, .78, .95, .95, 1.12)), 
                       # DEPRESSION: Items 9, 16, 17, 18, 35, and 50
                       'm_dep_3yrs' = bsi_scores(items = c('g0100365', 'g0100665', 'g0100765', 'g0100865', 'g0101365', 'g0102165'),
                                                 filledinby = BSI$gr1065_filledinby,
                                                 cutoff = c(.80, .71, .71, .75, 999)),
                       'm_dep_9yrs' = bsi_scores(items = c('d0100381_cleaned', 'd0100681_cleaned', 'd0100781_cleaned', 'd0100881_cleaned', 'd0101381_cleaned', 'd0102181_cleaned'),
                                                 filledinby = BSI$gr1081_filledinby,
                                                 cutoff = c(.80, .80, .71, .71, .75)), 
                       'f_dep_3yrs' = bsi_scores(items = c('c0100366', 'c0100666', 'c0100766', 'c0100866', 'c0101366', 'c0102166'),
                                                 filledinby = BSI$gr1066_filledinby,
                                                 cutoff = c(.71, .71, .80, .75, 999)), 
                       'f_dep_9yrs' = bsi_scores(items = c('d0100383_cleaned', 'd0100683_cleaned', 'd0100783_cleaned', 'd0100883_cleaned', 'd0101383_cleaned', 'd0102183_cleaned'),
                                                 filledinby = BSI$gr1083_filledinby,
                                                 cutoff = c(.71, .71, .80, .80, .75)),
                       # ANXIETY: Items 1, 12, 19, 38, 45, and 49 
                       'm_anx_3yrs' =bsi_scores(items = c('g0100165', 'g0100465', 'g0100965', 'g0101465', 'g0101865', 'g0102065'),
                                                filledinby = BSI$gr1065_filledinby,
                                                cutoff = c(.71, .65, .65, .75, 999)),
                       'm_anx_9yrs' = bsi_scores(items = c('d0100181_cleaned', 'd0100481_cleaned', 'd0100981_cleaned', 'd0101481_cleaned', 'd0101881_cleaned', 'd0102081_cleaned'),
                                                 filledinby = BSI$gr1081_filledinby,
                                                 cutoff = c(.71, .71, .65, .65, .75)), 
                       'f_anx_3yrs' = bsi_scores(items = c('c0100166', 'c0100466', 'c0100966', 'c0101466', 'c0101866', 'c0102066'),
                                                 filledinby = BSI$gr1066_filledinby,
                                                 cutoff = c(.71, .71, .80, .75, 999)), 
                       'f_anx_9yrs' = bsi_scores(items = c('d0100183_cleaned', 'd0100483_cleaned', 'd0100983_cleaned', 'd0101483_cleaned', 'd0101883_cleaned', 'd0102083_cleaned'),
                                                 filledinby = BSI$gr1083_filledinby,
                                                 cutoff = c(.71, .71, .80, .80, .75)) )

#-------------------------------------------------------------------------------
# Family Assessment Device 
FAD5  <- readquick("GR1075-B4_17072015.sav")
FAD9m <- readquick("GR1081_E1-3_5-6_08082016.sav") # We used this file already but let's give it a nicer name
FAD9f <- readquick("GR1083_E7_19102016.sav")
FAD   <- Reduce(function(x,y) merge(x = x, y = y, by = 'idc',  all = T),
                list(FAD5, FAD9m, FAD9f)) 

# The total score at age 5 is already dichotomous, age 9 need to be dichotomized manually.
m_fad_9yrs <- data.frame(5 - FAD$e0500181_v2, # Recode inverse item
                         5 - FAD$e0500381_v2, # Recode inverse item
                         5 - FAD$e0500581_v2, # Recode inverse item
                         5 - FAD$e0500781_v2, # Recode inverse item
                         5 - FAD$e0500981_v2, # Recode inverse item
                         5 - FAD$e0501181_v2, # Recode inverse item
                         FAD$e0500281_v2, 
                         FAD$e0500481_v2, 
                         FAD$e0500681_v2, 
                         FAD$e0500881_v2, 
                         FAD$e0501081_v2, 
                         FAD$e0501281_v2)

p_fad_9yrs <- data.frame(5 - FAD$e0700183v2, # Recode inverse item
                         5 - FAD$e0700383v2, # Recode inverse item
                         5 - FAD$e0700583v2, # Recode inverse item
                         5 - FAD$e0700783v2, # Recode inverse item
                         5 - FAD$e0700983v2, # Recode inverse item
                         5 - FAD$e0701183v2, # Recode inverse item
                         FAD$e0700283v2, 
                         FAD$e0700483v2, 
                         FAD$e0700683v2, 
                         FAD$e0700883v2, 
                         FAD$e0701083v2, 
                         FAD$e0701283v2) 

FAD_dich <- data.frame('IDC' = FAD$idc, 
                       'm_fad_5yrs' = FAD$fad_5y, 
                       'm_fad_9yrs' = fad_scores(m_fad_9yrs), 
                       'p_fad_9yrs' = fad_scores(p_fad_9yrs) )

#-------------------------------------------------------------------------------
# This function merges together all separate data.frames according to the IDC
# results in a dataframe with all relevant items for postnatal stress.
# tech-tip: because merge can only take two data.frames at the time and I am lazy, I used Reduce.
postnatal_stress <- Reduce(function(x,y) merge(x = x, y = y, by = 'IDC', all.x = T),
                           list(BSI_dich, FAD_dich, life_events, parenting, GR1065, 
                                GR1065G, GR1065X, GR1075, GR1075B3, GR1075E, GR1079, 
                                GR1080E, GR1080C, GR1082, GR1081, GR1081E4, GR1081I)) 
#-------------------------------------------------------------------------------
fetal_general <- readquick("FETALPERIOD-ALLGENERALDATA_10022025.sav") # 9778 obs of 95 var

early_parent <- data.frame('IDM' = fetal_general$idm, 
                           'm_age' = ifelse(fetal_general$age_m_v2  < 19, yes = 1, no = 0),  # mother younger than 19 years at intake (for PR)
                           # DICH: based on Cecil et al. (2014); Rijlaarsdam et al. (2016).
                           'partner_age' = ifelse(fetal_general$age_p_v2  < 19, yes = 1, no = 0),  # partner younger than 19 years (for PR)
                           'biofather_age' = ifelse(fetal_general$age_bf_v2 < 19, yes = 1, no = 0)) # biological father younger than 19 years (for PR). 
# In many cases only one of the last two was filled, but cases 
# which had both variables available will be combined later.

postnatal_stress <- merge(postnatal_stress, early_parent, by = 'IDM', all = T)

################################################################################
#### -------------------- Combine repeated measures  ---------------------- ####
################################################################################

# Repeating a grade was measured by maternal report at age 8 (GR1080), 9 (le8) 
# and 10 (GR1082). # Once a risk, always a risk strategy:
postnatal_stress$repeated_grade <- repmeas(postnatal_stress[,c('rep_grade_8yrs','rep_grade_9yrs','rep_grade_10yrs')])

# Financial difficulties were measured at age 3 (GR1065) and 9 (le10). 
# Once a risk, always a risk strategy:
postnatal_stress$financial_difficulties <- repmeas(postnatal_stress[,c('fidi_3yrs', 'fidi_9yrs')]) 

# Trouble paying for food was measured at age 3 (GR1065) and 9 (GR1081). 
# Once a risk, always a risk strategy:
postnatal_stress$trouble_pay_childhood <- repmeas(postnatal_stress[,c('trouble_pay_3yrs','trouble_pay_9yrs')]) 

# Income was assessed at age 3 (GR1065), 5 (GR1075) and 9 (GR1081).
# Two items: once-always + chronic risk strategy.
postnatal_stress$income_once <- repmeas(postnatal_stress[,c('income_3yrs', 'income_5yrs', 'income_9yrs')])
postnatal_stress$income_chronic <- repmeas(postnatal_stress[,c('income_3yrs', 'income_5yrs', 'income_9yrs')], 
                                           strategy = 'chronic')

# Unemployment was measured at age 3 (GR1065), 5 (GR1075, by mother and partner)
# and 9 (GR1081). # Two items: once-always + chronic risk strategy.
postnatal_stress$unemployed_once <- repmeas(postnatal_stress[,c('empl_3yrs','m_empl_5yrs', 'p_empl_5yrs', 'empl_9yrs')]) 
postnatal_stress$unemployed_chronic <- repmeas(postnatal_stress[,c('empl_3yrs','m_empl_5yrs', 'p_empl_5yrs', 'empl_9yrs')],
                                               strategy = 'chronic')

# Material deprivation measure is based on Statistics Netherlands and EU-SILC,
# (e.g. family possessed a car or a fridge, yes/no). To increase resemblance with EU-SILC, 
# we added an item about how many televisions the family possessed. 
# Combine material deprivation + television items. 
made <- postnatal_stress[, c("mat_depr_heating", "mat_depr_rent", "mat_depr_meal", "mat_depr_car", 
                             "mat_depr_washingmachine", "mat_depr_refrigerator", "mat_depr_telephone", 
                             "mat_depr_holidays","mat_depr_tv")]
# material_deprivation reflects the family’s ability to afford basic needs and services.
# If ‘possession rate’ is lower than 75%, risk is present. Rate is based on EU-SILC (2009).
postnatal_stress$material_deprivation <- ifelse((rowMeans(is.na(made)) < .25),
                                                yes = ifelse(rowMeans(made, na.rm = T) >= .25, yes = 1, no = 0), no = NA) 
# NOTE: we set material deprivation values to NA if the missing frequency within the questionnaire is >25%

# Education level of main caregiver and partner was assessed at age 3 and 5 (GR1075).
# Education level of each parent is included separately in the domain score. 
# Once a risk, always a risk strategy:
postnatal_stress$m_education <- repmeas(postnatal_stress[,c("m_educ_3yrs", "m_educ_5yrs")])
postnatal_stress$p_education <- repmeas(postnatal_stress[,c("p_educ_3yrs", "p_educ_5yrs")])

# Early parenthood. Age of father (partner or biological father) was assessed during intake. 
# In many cases only one of these was filled, but for cases which had both variables 
# available (and different values), we apply the once-always strategy.
postnatal_stress$p_age <- repmeas(postnatal_stress[,c("partner_age","biofather_age")])

# Psychopathology included three syndrome domains: interpersonal sensitivity, 
# depression and anxiety, measured in main caregivers and partners at the ages 3 and 9.
postnatal_stress$m_interpersonal_sensitivity <- repmeas(postnatal_stress[,c("m_is_3yrs", "m_is_9yrs")])
postnatal_stress$p_interpersonal_sensitivity <- repmeas(postnatal_stress[,c("f_is_3yrs", "f_is_9yrs")])
postnatal_stress$m_depression <- repmeas(postnatal_stress[,c("m_dep_3yrs", "m_dep_9yrs")])
postnatal_stress$p_depression <- repmeas(postnatal_stress[,c("f_dep_3yrs", "f_dep_9yrs")])
postnatal_stress$m_anxiety <- repmeas(postnatal_stress[,c("m_anx_3yrs", "m_anx_9yrs")])
postnatal_stress$p_anxiety <- repmeas(postnatal_stress[,c("f_anx_3yrs", "f_anx_9yrs")])

# Marital status was measured at age 3 (GR1065X), 5 (GR1075) and 9 (le14).
# Once a risk, always a risk strategy:
postnatal_stress$marital_status <- repmeas(postnatal_stress[,c('marital_status_3yrs','marital_status_5yrs','divorce_childhood')])

# Family size was measured at age 3 (GR1065G), 5 (GR1075B) and 9 (GR1081)
# Once a risk, always a risk strategy:
postnatal_stress$family_size <- repmeas(postnatal_stress[,c('fam_size_3yrs', 'fam_size_5yrs', 'fam_size_9yrs')])

# Bullying was assessed by main caregiver and teacher. 
# Once a risk, always a risk strategy: if child was verbally/physically/relationally 
# bullied once a week or more, as reported by the main caregiver or the teacher = risk. 
postnatal_stress$bullying <- repmeas(postnatal_stress[,c('bully_physical_m','bully_verbal_m','bully_excluded_m', 
                                                         'bully_physical_t','bully_verbal_t','bully_excluded_t')])

################################################################################
#------------------------------------------------------------------------------#
# Before we get to the final scores, some summary stats that may come in handy #
#------------------------------------------------------------------------------#

# Let's have a look at risk distribution and missing data per indicator.
postnatal_summary = data.frame(row.names=c("no risk","risk","NA","%risk","%miss"))
for (i in 3:ncol(postnatal_stress)) { # because the third column is not dichotomous (BSI_age)
  s = summary(as.factor(postnatal_stress[,i]))
  if (length(s) < 3) { # I am doing this ugly thing as a quick & dirty fix for lack of risk in fam_size_9yrs
    s = c(s[1], 0, s[2])
    names(s)[2] = "1"
  }
  c = colnames(postnatal_stress)[i]
  postnatal_summary[1:3,c] <- s 
  postnatal_summary[4,c] <- round((postnatal_summary[2,c] / nrow(postnatal_stress))*100, 2)
  postnatal_summary[5,c] <- round((postnatal_summary[3,c] / nrow(postnatal_stress))*100, 2)
}

#-------------------------------------------------------------------------------
# Apply the percent_missing function to the rows (1) of the entire dataset (total
# 51 indicators). NOTE: this is a little less straightforward compared to the prenatal 
# score, so to be safe, I will list all the indicator names. 
postnatal_stress$post_percent_missing = apply(postnatal_stress[,c(
  'sick_or_accident','family_member_ill','smbd_important_ill','parent_died','smbd_important_died','pet_died','school_workload','repeated_grade','lost_smth_important','moved','changed_school','friend_moved','fire_or_burglary', # LE
  'material_deprivation','financial_difficulties','neiborhood_problems','trouble_pay_childhood','income_once','income_chronic','unemployed_once','unemployed_chronic','m_education','p_education', # CR
  'tension_at_work','m_interpersonal_sensitivity','p_interpersonal_sensitivity','m_depression','p_depression','m_anxiety','p_anxiety','m_age','p_age', # PR
  'marital_problems','marital_status','family_size','m_fad_5yrs','m_fad_9yrs','p_fad_9yrs','conflict_family_member','conflict_smbd_else','conflict_in_family','divorce_childhood','argument_friend', # IR
  'm_harsh_parent','p_harsh_parent','bullying','physical_violence','physical_threats','sexual_harrasment','sexual_behavior','rumors_or_gossip')], # DV
  1, percent_missing)

#-------------------------------------------------------------------------------

################################################################################
#### -------------- create the (un-weighted) domain scores ---------------- ####
################################################################################

# ATTENTION! Here we use de default argument of domainscore function: calculating  a 
# *mean domain score* (range = 0 to 1) that is NOT adjusted for 25% missingness as in 
# e.g. Rijlaarsdam et al. (2016). If you prefer working with the actual number of risks
# (i.e. sum score) or the weighted version of it, you can set the argument score_type
# to 'sum_simple' or 'sum_weighted' respectively (see Setup and functions script
# for calculation details). 

# LE
postnatal_stress[,c('post_LE_percent_missing','post_life_events')] <- domainscore(postnatal_stress[,c(
  'sick_or_accident', # Did your child get seriously sick or did he/she have an accident? Yes
  'family_member_ill', # Did a family member get seriously sick or a serious accident? Yes
  'smbd_important_ill', # Did somebody else, who is important to your child, get seriously sick or have a serious accident? Yes
  'parent_died', # Is the father/mother or other caretaker still alive? No 
  'smbd_important_died', # Did somebody else, who your child cared about a lot, die? Yes
  'pet_died', # Did ever a pet, who you child cared about a lot, die? Yes
  'school_workload', # Does or did your child have to deal with a high workload at school? Yes
  'repeated_grade', # Did your child ever repeat a grade? Yes
  'lost_smth_important', # Did your child ever lose something which was important to him/her? Yes
  'moved', # Did your child move? Yes 
  'changed_school', # Did your child change schools? Yes
  'friend_moved', # Did one of your child’s friends move to a new house? Yes
  'fire_or_burglary' # Was there a fire or burglary? Yes 
)])

# CR
postnatal_stress[,c('post_CR_percent_missing','post_contextual_risk')] <- domainscore(postnatal_stress[,c(
  'material_deprivation', # Material deprivation; yes
  'financial_difficulties', # Does your family have or ever had financial difficulties? Yes 
  'neiborhood_problems', # Are/were there problems in the neighborhood? (e.g. vandalism or insecurity); yes
  'trouble_pay_childhood', # Trouble paying for your food, rent, electricity bill and such in the past year? Yes
  'income_once', # Income household; < 1600 euros p/m; once 
  'income_chronic', # Income household; < 1600 euros p/m; chronically
  'unemployed_once', # Unemployment within the family, once
  'unemployed_chronic', # Unemployment within the family, chronically
  'm_education', # Education main caregiver; < phase 1 (higher) education, once 
  'p_education')]) # Education partner; < phase 1 (higher) education , once

# PR
postnatal_stress[,c('post_PR_percent_missing','post_parental_risk')] <- domainscore(postnatal_stress[,c(
  'tension_at_work', # Tension at the parents’ work that has been felt at home; yes
  'm_interpersonal_sensitivity', # Interpersonal sensitivity main caregiver; >= 0.95
  'p_interpersonal_sensitivity', # Interpersonal sensitivity partner; >= 0.78
  'm_depression', # Depression main caregiver; >= 0.80
  'p_depression', # Depression partner; >= 0.71
  'm_anxiety', # Anxiety main caregiver; >= 0.71
  'p_anxiety', # Anxiety partner; >= 0.65
  'm_age', # Early parenthood; age mother < 19 yrs
  'p_age' # Early parenthood; age partner < 19 yrs
)])

# IR
postnatal_stress[,c('post_IR_percent_missing','post_interpersonal_risk')] <- domainscore(postnatal_stress[,c(
  'marital_problems', # Problems with marriage relations; yes
  'marital_status', # Marital status; single
  'family_size', # Family size; > 3 children
  'm_fad_5yrs', # Family distress according to main caregiver; > 2.17
  'm_fad_9yrs', # Family distress according to main caregiver; > 2.17
  'p_fad_9yrs', # Family distress according to partner; > 2.17
  'conflict_family_member', # Has your child ongoing conflicts with a family member (or ever had them)? Yes
  'conflict_smbd_else', # Has your child ongoing conflict with somebody else (or ever had them)? Yes 
  'conflict_in_family', # Do other family members have ongoing conflicts with each other (or ever had them)? Yes
  'divorce_childhood', # Are you and your partner divorced or separated? Yes
  'argument_friend' # Did your child lose a good friend due to an argument? Yes
)])

# DV
postnatal_stress[,c('post_DV_percent_missing','post_direct_victimization')] <- domainscore(postnatal_stress[,c(
  'm_harsh_parent', # Harsh parenting main caregiver > 80th percentile
  'p_harsh_parent', # Harsh parenting partner > 80th percentile
  'bullying', # Bullying more than once a week according to teacher or main caregiver; yes
  'physical_violence', # Did someone ever use physical violence against your child? For example, beating him/her up. Yes
  'physical_threats', # Did somebody use almost physical violence against your child? So that it did not actually happen, but the child was frightened. Yes
  'sexual_harrasment', # Did anybody make sexual comments or movements towards your child? Yes 
  'sexual_behavior', # Did your child participate in inappropriate sexual behavior? Yes
  'rumors_or_gossip' # Did somebody spread mean rumors or gossip about your child? Yes
)])

################################################################################
# Set all binary variables to class: factor
# nonbinary_cols <- c("IDC", "IDM", "post_percent_missing", "post_LE_percent_missing", "post_life_events",           
#                     "post_CR_percent_missing", "post_contextual_risk", "post_PR_percent_missing", "post_parental_risk",      
#                     "post_IR_percent_missing", "post_interpersonal_risk", "post_DV_percent_missing", "post_direct_victimization")
# 
# postnatal_stress[!names(postnatal_stress) %in% nonbinary_cols] <- 
#   lapply(postnatal_stress[!names(postnatal_stress) %in% nonbinary_cols], factor) 

################################################################################
#### -------------- Construct CUMULATIVE STRESS variables ----------------- ####
################################################################################

# compute sum scores for postnatal stress exposure - EXCL. PARENTAL RISK
postnatal_stress$postnatal_stress <- rowSums(postnatal_stress[,c("post_life_events", 
                                                                 "post_contextual_risk", 
                                                                 "post_interpersonal_risk", 
                                                                 "post_direct_victimization")], na.rm = F)

################################################################################
#### --------------------------- save and run ----------------------------- ####
################################################################################

# Save the dataset in an .rds file, in the directory where the raw data are stored
#saveRDS(postnatal_stress, file.path(pathtoresults,'postnatal_stress_GENR.rds'))
#saveRDS(postnatal_summary, file.path(pathtoresults,'postnatal_stress_GENR_summary.rds'))



