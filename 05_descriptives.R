################################################################################
# Title:    Descriptives moderator project
# Author:   Jana Hermans
# Date:     Created in February 2025
################################################################################
library(flextable)
library(officer)
library(dplyr)

results_path <- dirname(file.choose())
load(file.path(results_path,'Table_1_project3.Rdata'))

ft <- flextable(desc_table)
ft <- ft %>%
  flextable::set_table_properties(layout = "autofit") %>%
  flextable::fontsize(size = 8, part = "all") %>%         # smaller font
  flextable::font(fontname = "Times New Roman", part = "all") %>%
  flextable::bold(i = 1, part = "header")              # only top header row bold

doc <- read_docx()
doc <- body_add_flextable(doc, value = ft)
print(doc, target = file.path(results_path,"tables/Table1_raw.docx"))

load(file.path(results_path,'missingness_Table1_project3.Rdata'))

# Proportion of missing data (for in table note)
# ALSPAC
vals <- paste0(miss_table$ALSPAC, row.names(miss_table))
cat(vals, sep = ", ")  
# GenR
vals <- paste0(miss_table$GenR, row.names(miss_table))
cat(vals, sep = ", ")  

# Complete cases
library(flextable)
library(officer)
library(dplyr)

load(file.path(results_path,'Stable_completecases_project3.Rdata'))

ft <- flextable(desc_table)
ft <- ft %>%
  flextable::set_table_properties(layout = "autofit") %>%
  flextable::fontsize(size = 8, part = "all") %>%         # smaller font
  flextable::font(fontname = "Times New Roman", part = "all") %>%
  flextable::bold(i = 1, part = "header")              # only top header row bold

doc <- read_docx()
doc <- body_add_flextable(doc, value = ft)
print(doc, target = file.path(results_path,"tables/STable_desc_completecases.docx"))