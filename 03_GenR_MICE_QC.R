########################################################################
# Title:    Quality check of imputation (GenR)
# Author:   Jana Hermans
# Date:     Last modification November 2025
# Paper:    Prenatal maternal depression, child mental health, and the 
#           role of lifestyle and psychosocial factors: results from two 
#           population-based birth cohorts.
########################################################################

# Set up R environment for package libraries
rm(list=ls()) #clear environment 
set.seed(2025) #set seed for reproducibility script 
#install.packages("renv")
projectpath <- dirname(file.choose())
setwd(projectpath)   # Set your working directory to your project folder
.libPaths(paste0(projectpath, '/renv/library/R-4.3/x86_64-w64-mingw32'))
libraries <- c("haven","dplyr","mice","ggplot2","grid","gridExtra")
invisible(lapply(libraries, require, character.only = T))

# Set working directory for where you keep your data. Choose any file, the goal 
# is to select the correct path
datapath <- dirname(file.choose())
filename <- 'GenR_impset_2025-10-23.Rdata'
load(file.path(datapath, filename))
#----------------------------------Save MICE QC---------------------------------
var_names <- setdiff(names(imp_rf$data), "IDC")

pdf('V:/medewerkers/093307 Hermans A.P.C/project_3/QC-imputation_project3_rf_GenR.pdf')
for (v in var_names) { if (nrow(imp_rf$imp[[v]]) > 1) {
  message(v)
  
  nmiss <- sum(is.na(imp_rf$data[v]))
  nmiss <- paste0('\n n missing = ', nmiss, ' (',round(nmiss/nrow(imp_rf$data)*100,1),'%)')
  
  try(print(mice::densityplot(imp_rf, as.formula(paste('~',v)), main=paste(v, nmiss))))
  try(print(plot(imp_rf, v,main=paste(v, nmiss))))
}
}
dev.off()