library(openxlsx)
library(reshape2)
#library(tidyr)
library(tidyverse)
library(readxl)

holpa_survey <- read_excel(
  "C:/Users/andreasanchez/OneDrive - CGIAR/Bioversity/AI/HOLPA/analysis/HOLPA/HOLPA_global_household_survey_20231204_mapped_to_indicators.xlsx",
                   sheet = "survey")%>%
  select("module","indicator", "subindicator", "label_english_code",
         "type", "name","label::English ((en))")

names(holpa_survey)
sort(unique(holpa_survey$module))
holpa_choices <- read_excel(
  "C:/Users/andreasanchez/OneDrive - CGIAR/Bioversity/AI/HOLPA/analysis/HOLPA/HOLPA_global_household_survey_20231204_mapped_to_indicators.xlsx",
  sheet = "choices")%>%
  select("list_name","name","label::English ((en))")
