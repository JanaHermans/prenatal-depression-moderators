# prenatal-depression-moderators
This repository hosts the R code of the project "**Prenatal maternal depression, child mental health, and the moderating role of lifestyle and psychosocial factors: results from two population-based birth cohorts.**".

## Analytical pipeline 
- **`00_ALSPAC_ELS_score.R`**: Calculates the ELS score in the ALSPAC dataset.
- **`00_GenR_ELS_score`**: Calculates the ELS score in the GenR dataset.
- **`01_ALSPAC_prep_to_impute.R`**: Prepares the ALSPAC dataset for imputation.
- **`01_GenR_prep_to_impute.R`**: Prepares the GenR dataset for imputation.
- **`02_ALSPAC_imp_MICE.R`**: Imputation with MICE of missing data in the ALSPAC dataset (run on server).
- **`02_GenR_imp_MICE.R`**: Imputation with MICE of missing data in the GenR dataset (run on server).
- **`03_ALSPAC_MICE_QC.R`**: Performs quality control of the imputation in the ALSPAC dataset and creates QC document.
- **`03_GenR_MICE_QC.R`**: Performs quality control of the imputation in the GenR dataset and creates QC document.
- **`04_ALSPAC_moderation_imputedcases.R`**: Performs the project's analyses in the imputed ALSPAC dataset.
- **`04_GenR_moderation_imputedcases.R`**: Performs the project's analyses in the imputed GenR dataset.
- **`04_moderation_completecases.R`**: Performs the project's analyses in the complete cases (both ALSPAC and GenR).
- **`05_descriptives.R`**: Script that generates descriptive tables for the full dataset and complete cases (both ALSPAC and GenR).
- **`05_meta_analyses_direct.R`**: Meta-analyses of the direct associations.
- **`05_meta_analyses_main.R`**: Meta-analyses of moderation effects (main analyses).
- **`05_meta_analyses_mod_strat.R`**: Meta-analyses stratified by moderators (for visualisation only).
- **`05_sensitivity_analyses.R`**: Sensitivity analyses.
a): Investigating moderation effects when adjusting for pre-pregnancy depression.
b): Investigating moderation effects with continuous maternal depressive symptoms as exposure.
c): Investigating moderation effects with Youth Self-Report as outcome.
d): Investigating moderation effects in complete cases.
e): Investigating moderation effects when excluding outliers from analyses.