#library(openxlsx)
library(tidyr)
library(tidyverse)
library(readxl)
library(dplyr)
library(summarytools)

#### Set file paths ####
# TO CHECK: I need to connect directly to the share point, I already asked Sebastien for permision
#For now I will leave it like this to continue working

#Sarah
global.data.path <- "D:/02_Bioversity/46_Agroecology_Initiative/holpa_results/"
zwe.data.path <- "D:/02_Bioversity/46_Agroecology_Initiative/holpa_results/zwe/"

#### Import data ####
# Each dataset contains a survey worksheet with the questions and responses for text, open and numeric questions, and
# a choices worksheet with the response options for multiple choice questions (single or multiple).
# These need to be imported and combined.

### Country databases ####
read_and_process_survey_xlsx <- function(sheet_name, column_id_rename, data_path, country_name, index) {
  survey_data <- read_excel(path = data_path, sheet = sheet_name) %>%
    mutate(country = country_name,
           sheet_id = sheet_name) %>%
    rename("kobo_farmer_id" := !!column_id_rename,
           "index" := !!index) %>%
    slice(-1)
  
  # Automatically rename columns for begin_repeat groups
  if (grepl("begin_repeat", tolower(sheet_name))) {
    survey_data <- survey_data %>%
      rename("parent_table_name" = "_parent_table_name",
             "parent_index" = "_parent_index")
  }
  
  return(survey_data)
}

### ZIMBABWE ----
#Household survey
zwe.h.data.path <-"C:/Users/andreasanchez/OneDrive - CGIAR/Bioversity/AI/HOLPA/HOLPA_data/Zimbabwe/zimbabwe_data_clean/zwe_household_database_2024.04.18_clean.xlsx" #Andrea

zwe_household_survey <- read_and_process_survey_xlsx("Final HOLPA_Zimbabwe_Household", "_id", zwe.h.data.path,"zimbabwe","_index")%>%
  #Remove respondents that are not farmers
  filter(kobo_farmer_id!="274186917")
#Fieldwork survey
zwe.f.data.path<-"C:/Users/andreasanchez/OneDrive - CGIAR/Bioversity/AI/HOLPA/HOLPA_data/Zimbabwe/zimbabwe_data_clean/zwe_fieldwork_database_2024.05.22_clean.xlsx" #Andrea
zwe_fieldwork_survey <- read_and_process_survey_xlsx("Final HOLPA_Zimbabwe_Field", "_id", zwe.f.data.path,"zimbabwe","_index")

names(zwe_fieldwork_survey)

#Add household survey kobo_farmer_id to fieldwork survey
zwe_fieldwork<- zwe_household_survey%>%
  select(kobo_farmer_id, household_id,farmer )%>%
  right_join(zwe_fieldwork_survey,by=c("household_id","farmer"))%>%
  # filter(farmer=="mudzimbabwe _ thomas")%>%
  rename("kobo_farmer_id"="kobo_farmer_id.x",
        "kobo_farmer_id.fieldwork"="kobo_farmer_id.y" )%>%
  #Remove the farmers that do not match in the household survey
  filter(!is.na(kobo_farmer_id))

names(zwe_fieldwork)

write.csv(zwe_fieldwork,file="C:/Users/andreasanchez/OneDrive - CGIAR/Bioversity/AI/HOLPA/analysis/HOLPA/HOLPA/zwe/zwe_fieldwork_format.csv",row.names=FALSE)

zwe_error_fieldwork<- zwe_household_survey%>%
  select(kobo_farmer_id, household_id,farmer )%>%
  right_join(zwe_fieldwork_survey,by=c("household_id","farmer"))%>%
 # filter(farmer=="mudzimbabwe _ thomas")
  filter(is.na(kobo_farmer_id.x))

write.csv(zwe_error_fieldwork,file="C:/Users/andreasanchez/OneDrive - CGIAR/Bioversity/AI/HOLPA/analysis/HOLPA/HOLPA/zwe/zwe_error_fieldwork.csv",row.names=FALSE)


### TUNISIA ----
#ERROR: Tunisia does not have a household_id that matches the household and fieldwork survey.
#Household survey
tun.h.data.path <-"C:/Users/andreasanchez/OneDrive - CGIAR/Bioversity/AI/HOLPA/HOLPA_data/Tunisia/tunisia_data_clean/tun_holpa_household_survey_clean.xlsx" #Andrea

tun_household_survey <- read_and_process_survey_xlsx("HOLPA_Tunisia_household_surv", "_id", tun.h.data.path,"tunisia","_index")%>%
  #Remove respondents that did not wanted to complete the survey
  filter(consent_2!="No")
#Fieldwork survey
tun.f.data.path<-"C:/Users/andreasanchez/OneDrive - CGIAR/Bioversity/AI/HOLPA/HOLPA_data/Tunisia/tunisia_data_clean/tun_holpa_fieldwork_survey_clean.xlsx" #Andrea
tun_fieldwork_survey <- read_and_process_survey_xlsx("HOLPA_Tunisia_field_survey_2", "_id", tun.f.data.path,"tunisia","_index")

names(tun_fieldwork_survey)

#Add household survey kobo_farmer_id to fieldwork survey
tun_fieldwork<- tun_household_survey%>%
  select(kobo_farmer_id, household_id,"_1_2_1_4_1",	"_1_2_1_4_2"  )%>%
  right_join(tun_fieldwork_survey,by=c("_1_2_1_4_1",	"_1_2_1_4_2"))%>%
  rename("kobo_farmer_id"="kobo_farmer_id.x",
         "kobo_farmer_id.fieldwork"="kobo_farmer_id.y" )%>%
  #Remove the farmers that do not match in the household survey
  filter(!is.na(kobo_farmer_id))

names(tun_fieldwork)

write.csv(zwe_fieldwork,file="C:/Users/andreasanchez/OneDrive - CGIAR/Bioversity/AI/HOLPA/analysis/HOLPA/HOLPA/zwe/zwe_fieldwork_format.csv",row.names=FALSE)

tun_error_fieldwork<- tun_household_survey%>%
  select(kobo_farmer_id, household_id,"_1_2_1_4_1",	"_1_2_1_4_2"  )%>%
  right_join(tun_fieldwork_survey,by=c("_1_2_1_4_1",	"_1_2_1_4_2"))%>%
  filter(is.na(kobo_farmer_id.x))

write.csv(tun_error_fieldwork,file="C:/Users/andreasanchez/OneDrive - CGIAR/Bioversity/AI/HOLPA/analysis/HOLPA/HOLPA/zwe/zwe_error_fieldwork.csv",row.names=FALSE)
