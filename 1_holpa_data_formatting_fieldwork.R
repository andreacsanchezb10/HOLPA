#library(openxlsx)
library(tidyr)
library(tidyverse)
library(readxl)
library(dplyr)
library(summarytools)

#### Set file paths ####
# TO CHECK: I need to connect directly to the share point, I already asked Sebastien for permision
#For now I will leave it like this to continue working

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
zwe.data.path <-"C:/Users/andreasanchez/OneDrive - CGIAR/Bioversity/AI/HOLPA/HOLPA_data/Zimbabwe/zimbabwe_data_clean/" #path Andrea

#Household survey
zwe_household_survey <- read_and_process_survey_xlsx("Final HOLPA_Zimbabwe_Household", "_id", paste0(zwe.data.path,"zwe_holpa_household_survey_clean.xlsx"),"zimbabwe","_index")%>%
  #Remove respondents that are not farmers
  filter(kobo_farmer_id!="274186917")

#Fieldwork survey
zwe_fieldwork_survey <- read_and_process_survey_xlsx("zwe_holpa_fieldwork_survey", "_id", paste0(zwe.data.path,"zwe_holpa_fieldwork_survey_clean.xlsx"),"zimbabwe","_index")

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

write.csv(zwe_fieldwork,file=paste0(zwe.data.path,"zwe/zwe_fieldwork_format.csv"),row.names=FALSE)

#7 farmers do not match with the household survey
zwe_error_fieldwork<- zwe_household_survey%>%
  select(kobo_farmer_id, household_id,farmer )%>%
  right_join(zwe_fieldwork_survey,by=c("household_id","farmer"))%>%
 # filter(farmer=="mudzimbabwe _ thomas")
  filter(is.na(kobo_farmer_id.x))

write.csv(zwe_error_fieldwork,file=paste0(zwe.data.path,"zwe/zwe_error_fieldwork.csv"),row.names=FALSE)

### TUNISIA ----
#ERROR: Tunisia does not have a household_id that matches the household and fieldwork survey.
tun.data.path <-"C:/Users/andreasanchez/OneDrive - CGIAR/Bioversity/AI/HOLPA/HOLPA_data/Tunisia/tunisia_data_clean/" #Andrea

#Household survey
tun_household_survey <- read_and_process_survey_xlsx("HOLPA_Tunisia_household_surv", "_id", paste0(tun.data.path,"tun_holpa_household_survey_clean.xlsx"),"tunisia","_index")%>%
  #Remove respondents that did not wanted to complete the survey
  filter(consent_2!="No")

#Fieldwork survey
tun_fieldwork_survey <- read_and_process_survey_xlsx("tun_holpa_fieldwork_survey", "_id", paste0(tun.data.path,"tun_holpa_fieldwork_survey_clean.xlsx"),"tunisia","_index")

names(tun_fieldwork_survey)

#Add household survey kobo_farmer_id to fieldwork survey
tun_fieldwork<- tun_household_survey%>%
  select(kobo_farmer_id, tun_household_id )%>%
  right_join(tun_fieldwork_survey,by=c("tun_household_id"))%>%
  rename("kobo_farmer_id"="kobo_farmer_id.x",
         "kobo_farmer_id.fieldwork"="kobo_farmer_id.y" )%>%
  #Remove the farmers that do not match in the household survey
  filter(!is.na(kobo_farmer_id))

names(tun_fieldwork)

write.csv(tun_fieldwork,file="C:/Users/andreasanchez/OneDrive - CGIAR/Bioversity/AI/HOLPA/analysis/HOLPA/HOLPA/tun/tun_fieldwork_format.csv",row.names=FALSE)

#6 farmers do not match with the household survey
tun_error_fieldwork<- tun_household_survey%>%
  select(kobo_farmer_id, tun_household_id )%>%
  right_join(tun_fieldwork_survey,by=c("tun_household_id"))%>%
  rename("kobo_farmer_id"="kobo_farmer_id.x",
         "kobo_farmer_id.fieldwork"="kobo_farmer_id.y" )%>%
  #Remove the farmers that do not match in the household survey
  filter(is.na(kobo_farmer_id))

### KENYA ----
ken_data_path <- "C:/Users/andreasanchez/OneDrive - CGIAR/Bioversity/AI/HOLPA/HOLPA_data/Kenya/kenya_data_clean/" #path andrea

#Household survey
ken_household_survey <- read_and_process_survey_xlsx("Holpa_global_household_surve", "_id", paste0(ken_data_path,"ken_holpa_household_survey_clean.xlsx"),"kenya","_index")%>%
  filter(kobo_farmer_id!="274186917") #Remove respondents that are not farmers

names(ken_household_survey)
sort(unique(ken_household_survey$kobo_farmer_id))

#Fieldwork survey
ken_fieldwork_survey <- read_and_process_survey_xlsx("ken_holpa_fieldwork_survey", "_id", paste0(ken_data_path,"ken_holpa_fieldwork_survey_clean.xlsx"),"kenya","_index")%>%
  filter(kobo_farmer_id!="274186917") #Remove respondents that are not farmers

names(ken_fieldwork_survey)

names(ken_fieldwork_survey) <- sapply(names(ken_fieldwork_survey), function(x) {
  # If there is a "/", return the substring after the last "/", otherwise return the original name
  if (grepl("/", x)) {
    return(sub(".*/", "", x))
  } else {
    return(x)
  }
})

# Check the renamed columns
names(ken_fieldwork_survey)
sort(unique(ken_fieldwork_survey$kobo_farmer_id))

#Add household survey kobo_farmer_id to fieldwork survey
ken_fieldwork<- ken_household_survey%>%
  select(kobo_farmer_id, household_id )%>%
  right_join(ken_fieldwork_survey,by=c("household_id","kobo_farmer_id"))
rename("kobo_farmer_id"="kobo_farmer_id.x",
       "kobo_farmer_id.fieldwork"="kobo_farmer_id.y" )%>%
  #Remove the farmers that do not match in the household survey
  filter(!is.na(kobo_farmer_id))

names(ken_fieldwork)

write.csv(ken_fieldwork,file=paste0(ken.data.path,"ken/ken_fieldwork_format.csv"),row.names=FALSE)

#No errors for ken
ken_error_fieldwork<- ken_household_survey%>%
  select(kobo_farmer_id, household_id )%>%
  right_join(ken_fieldwork_survey,by=c("household_id","kobo_farmer_id"))%>%
  filter(is.na(kobo_farmer_id))

write.csv(ken_error_fieldwork,file=paste0(ken.data.path,"ken/ken_error_fieldwork.csv"),row.names=FALSE)


### LAOS ----
lao.data.path <-"C:/Users/andreasanchez/OneDrive - CGIAR/Bioversity/AI/HOLPA/HOLPA_data/Laos/laos_data_clean/" #Andrea

#Household survey
lao_household_survey <- read_and_process_survey_xlsx("Final_HOLPA_Laos", "_id", paste0(lao.data.path,"lao_holpa_household_survey_clean.xlsx"),"laos","_index")
names(lao_household_survey)

#Fieldwork survey
lao_fieldwork_survey <- read_and_process_survey_xlsx("lao_holpa_fieldwork_survey", "_id", paste0(lao.data.path,"lao_holpa_fieldwork_survey_clean.xlsx"),"laos","_index")

names(lao_fieldwork_survey)

#Add household survey kobo_farmer_id to fieldwork survey
lao_fieldwork<- lao_household_survey%>%
  select(kobo_farmer_id, household_id )%>%
  right_join(lao_fieldwork_survey,by=c("household_id","kobo_farmer_id"))
  rename("kobo_farmer_id"="kobo_farmer_id.x",
         "kobo_farmer_id.fieldwork"="kobo_farmer_id.y" )%>%
  #Remove the farmers that do not match in the household survey
  filter(!is.na(kobo_farmer_id))

names(lao_fieldwork)

write.csv(lao_fieldwork,file=paste0(lao.data.path,"lao/lao_fieldwork_format.csv"),row.names=FALSE)

#No errors for Laos
lao_error_fieldwork<- lao_household_survey%>%
  select(kobo_farmer_id, household_id )%>%
  right_join(lao_fieldwork_survey,by=c("household_id","kobo_farmer_id"))%>%
  filter(is.na(kobo_farmer_id))

write.csv(lao_error_fieldwork,file=paste0(lao.data.path,"lao/lao_error_fieldwork.csv"),row.names=FALSE)

