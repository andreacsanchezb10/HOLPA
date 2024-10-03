#library(openxlsx)
library(tidyr)
library(tidyverse)
library(readxl)
library(dplyr)
library(summarytools)
 

#INSTRUCTION: run the following code

### Country databases ####
# Read excel files----
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


#### GLOBAL DATABASES ----
#global_survey contains the global form with questions
#global_choices contains the global form with choices

#INSTRUCTIONS: Please download HOLPA_global_household_survey_20231204_mapped_to_indicators_master.xlsx in your computer from: https://github.com/andreacsanchezb10/HOLPA
#Replace global.data.path path with your path and run the code

global.data.path <-"C:/Users/andreasanchez/OneDrive - CGIAR/Bioversity/AI/HOLPA/analysis/HOLPA/HOLPA/" #path andrea

global_survey <- read_excel(paste0(global.data.path,"HOLPA_global_household_survey_20231204_mapped_to_indicators_master.xlsx"),
                            sheet = "survey")%>%
  #select only the necessary columns
  select("module","indicator", "subindicator", 
         "type", "name","label::English ((en))")%>%
  #rename columns names
  rename("label_question" = "label::English ((en))")%>%
  rename("name_question" = "name")%>%
  #remove rows without questions
  filter(type!="begin_group")%>%
  filter(type!="begin_repeat")%>%
  filter(type!="end_repeat")%>%
  #separate question type components
  mutate(type_question = ifelse(substr(type,1,10)=="select_one","select_one",
                                ifelse(substr(type,1,10)=="select_mul","select_multiple",type)))%>%
  #create column with list_name codes matching the choices worksheet
  mutate(list_name = if_else(type_question== "select_one"|type_question== "select_multiple", 
                             str_replace(.$type, paste0(".*", .$type_question), ""),NA))%>%
  mutate(list_name = str_replace_all(list_name, " ", ""))  #%>% mutate(global_r_list_name =  sub('*_', "", name_question)) %>%mutate(global_r_list_name = ifelse(grepl("_", global_r_list_name, fixed = TRUE)==TRUE,global_r_list_name,""))

global_choices <- read_excel(paste0(global.data.path,"HOLPA_global_household_survey_20231204_mapped_to_indicators_master.xlsx"),
                             sheet = "choices")%>%
  select("list_name","name","label::English ((en))","country")%>%
  rename("label_choice" = "label::English ((en))")%>%
  rename("name_choice" = "name")


#### Import data ####
#country_holpa_household_survey_clean contains the clean household survey with the questions and responses 
#country_holpa_household_form_clean contains the clean household form with the questions (this form is similar to the one uploaded in koboCollect, but some changes were made to match the global survey [e.g. some countries changed the code of some global questions])
#INTRUCTION: Go to your country section

### ZIMBABWE ----
#link to zwe data: https://cgiar-my.sharepoint.com/:f:/r/personal/andrea_sanchez_cgiar_org/Documents/Bioversity/AI/HOLPA/HOLPA_data/Zimbabwe/zimbabwe_data_clean?csf=1&web=1&e=azqxKc
#INSTRUCTION: Replace zwe_data_path path with your path, run the code and then go #### CONTEXT MODULE
zwe_data_path <- "C:/Users/andreasanchez/OneDrive - CGIAR/Bioversity/AI/HOLPA/HOLPA_data/Zimbabwe/zimbabwe_data_clean/" #path andrea

zwe_h_survey_file <- paste0(zwe_data_path, "zwe_holpa_household_survey_clean.xlsx")
zwe_h_choices_file <- paste0(zwe_data_path, "zwe_holpa_household_form_clean.xlsx")

zwe_survey_main <- read_and_process_survey_xlsx("Final HOLPA_Zimbabwe_Household", "_id", zwe_h_survey_file,"zimbabwe","_index")%>%
  filter(kobo_farmer_id!="274186917") #Remove respondents that are not farmers
zwe_survey_1_4_2_7_begin_repeat <- read_and_process_survey_xlsx("_1_4_2_7_begin_repeat", "_submission__id", zwe_h_survey_file,"zimbabwe","_index")%>% # Section: Crop production
  filter(kobo_farmer_id!="274186917") #Remove respondents that are not farmers

zwe_choices <- read_excel(zwe_h_choices_file,sheet = "choices")%>%
  mutate(country= "zimbabwe")%>%
  select("list_name","name","label::English ((en))","country")%>%
  rename("label_choice" = "label::English ((en))")%>%
  rename("name_choice" = "name")%>%
  distinct(list_name,name_choice,label_choice, .keep_all = TRUE)

#Add country choices to global choices
zwe_global_choices<-global_choices%>%
  rbind(zwe_choices)%>%
  arrange(desc(country == "global")) %>%
  #Removing duplicates
  distinct(list_name,name_choice, .keep_all = TRUE) %>%
  right_join(global_survey,by="list_name",relationship="many-to-many")%>%
  mutate(label_choice.country=NA)

#Indicator: climate_drought and climate_flood
#Zimbabwe error: the choices options should be "4_2_1_3" (1=yes, 0=no, notsure), but the country put "4_2_1_1" (increased, nochange, decreased, notsure)
zwe_global_choices.x<- zwe_global_choices%>%
  filter(list_name=="4_2_1_1")%>%
  mutate(name_question= case_when(
    name_question=="_4_2_1_1_1"~"_4_2_1_3_1",
    name_question=="_4_2_1_1_2"~"_4_2_1_3_2",
    TRUE ~ name_question))%>%
  mutate(subindicator= case_when(
    subindicator== "climate_temp"~ "climate_flood",
    subindicator== "climate_rainfall_change"~ "climate_drought",
    TRUE ~ subindicator))%>%
  mutate(label_question = case_when(
    name_question=="_4_2_1_3_1" ~"In the last 12 months [add country meaning], have you experienced any flood conditions on your farmland?",
    name_question=="_4_2_1_3_2"~"In the last 12 months [add country meaning], have you experienced any drought conditions on your farmland?",
    TRUE ~ label_question))
    
zwe_global_choices<-rbind(zwe_global_choices,zwe_global_choices.x)

### TUNISIA -----
#link to tun data: https://cgiar-my.sharepoint.com/:f:/r/personal/andrea_sanchez_cgiar_org/Documents/Bioversity/AI/HOLPA/HOLPA_data/Tunisia/tunisia_data_clean?csf=1&web=1&e=07Lc0e
#INSTRUCTION: Replace tun_data_path path with your path, run the code and then go #### CONTEXT MODULE

tun_data_path <- "C:/Users/andreasanchez/OneDrive - CGIAR/Bioversity/AI/HOLPA/HOLPA_data/Tunisia/tunisia_data_clean/" #path andrea

tun_h_survey_file <- paste0(tun_data_path, "tun_holpa_household_survey_clean.xlsx")
tun_h_choices_file <- paste0(tun_data_path, "tun_holpa_household_form_clean.xlsx")

tun_survey_main <- read_and_process_survey_xlsx("HOLPA_Tunisia_household_surv", "_id", tun_h_survey_file,"tunisia","_index")%>%
  filter(consent_2!="No") #Remove respondents that did not wanted to complete the survey

#does not exist for tunisia tun_survey_1_4_2_7_begin_repeat <- read_and_process_survey_xlsx("_1_4_2_7_begin_repeat", "_submission__id", tun.data.path,"tunisia","_index") # Section: Crop production
tun_choices <- read_excel(tun_h_choices_file, sheet = "choices")%>%
  mutate(country= "tunisia")%>%
  select("list_name","name","label::English ((en))","country")%>%
  rename("label_choice" = "label::English ((en))")%>%
  rename("name_choice" = "name")%>%
  distinct(list_name,name_choice,label_choice, .keep_all = TRUE)

#Add country choices to global choices
tun_global_choices<-global_choices%>%
  rbind(tun_choices)%>%
  arrange(desc(country == "global")) %>%
  #Removing duplicates
  distinct(list_name,name_choice, .keep_all = TRUE) %>%
  right_join(global_survey,by="list_name",relationship="many-to-many")%>%
  mutate(label_choice.country=NA)

### KENYA ----
#link to ken data: https://cgiar-my.sharepoint.com/:f:/r/personal/andrea_sanchez_cgiar_org/Documents/Bioversity/AI/HOLPA/HOLPA_data/Kenya/kenya_data_clean?csf=1&web=1&e=D7sIkb
#INSTRUCTION: Replace ken_data_path path with your path, run the code and then go #### CONTEXT MODULE

ken_data_path <- "C:/Users/andreasanchez/OneDrive - CGIAR/Bioversity/AI/HOLPA/HOLPA_data/Kenya/kenya_data_clean/" #path andrea

ken_h_survey_file <- paste0(ken_data_path, "ken_holpa_household_survey_clean.xlsx")
ken_h_choices_file <- paste0(ken_data_path, "ken_holpa_household_form_clean.xlsx")

ken_survey_main <- read_and_process_survey_xlsx("Holpa_global_household_surve", "_id", ken_h_survey_file,"kenya","_index")%>%
  filter(kobo_farmer_id!="274186917") #Remove respondents that are not farmers

#does not exist for kenya ken_survey_1_4_2_7_begin_repeat <- read_and_process_survey_xlsx("_1_4_2_7_begin_repeat", "_submission__id", ken_h_survey_file,"tunisia","_index") # Section: Crop production
ken_choices <- read_excel(ken_h_choices_file,sheet = "choices")%>%
  mutate(country= "kenya")%>%
  select("list_name","name","label::English ((en))","country")%>%
  rename("label_choice" = "label::English ((en))")%>%
  rename("name_choice" = "name")%>%
  distinct(list_name,name_choice,label_choice, .keep_all = TRUE)

#Add country choices to global choices
ken_global_choices<-global_choices%>%
  rbind(ken_choices)%>%
  arrange(desc(country == "global")) %>%
  #Removing duplicates
  distinct(list_name,name_choice, .keep_all = TRUE) %>%
  right_join(global_survey,by="list_name",relationship="many-to-many")%>%
  mutate(label_choice.country=NA)

### SENEGAL ----
#link to sen data: https://cgiar-my.sharepoint.com/:f:/r/personal/andrea_sanchez_cgiar_org/Documents/Bioversity/AI/HOLPA/HOLPA_data/Senegal/senegal_data_clean?csf=1&web=1&e=bT58Tm
#INSTRUCTION: Replace sen_data_path path with your path, run the code and then go #### CONTEXT MODULE
sen_data_path <- "C:/Users/andreasanchez/OneDrive - CGIAR/Bioversity/AI/HOLPA/HOLPA_data/Senegal/senegal_data_clean/" #path andrea

sen_h_survey_file <- paste0(sen_data_path, "sen_holpa_household_survey_clean.xlsx")
sen_h_choices_file <- paste0(sen_data_path, "sen_holpa_household_form_clean.xlsx")

sen_survey_main <- read_and_process_survey_xlsx("HOLPA Senegal_version finale", "_id", sen_h_survey_file,"senegal","_index")%>%
  #Remove respondents that did not wanted to complete the survey
  filter(consent_2!="No")%>%
  slice(-1)%>%
  mutate(x_2_6_1_3= NA)%>%
  rename("x_2_6_1_3_1"= "_2_6_1_3_1",
         "x_2_6_1_3_2"= "_2_6_1_3_2")%>%
  mutate(x_2_6_1_3= x_2_6_1_3_1,
         x_2_6_1_3= if_else(is.na(x_2_6_1_3),x_2_6_1_3_2,x_2_6_1_3))%>%
  rename("_2_6_1_3"="x_2_6_1_3")

sen_survey_1_4_2_7_begin_repeat <- read_and_process_survey_xlsx("_1_4_2_7_begin_repeat", "_submission__id", sen_h_survey_file,"senegal","_index")%>% # Section: Crop production
  slice(-1)

sen_choices <- read_excel(sen_h_choices_file,sheet = "choices")%>%
  mutate(country= "senegal")%>%
  select("list_name","name","label::English ((en))","country")%>%
  rename("label_choice" = "label::English ((en))")%>%
  rename("name_choice" = "name")%>%
  distinct(list_name,name_choice,label_choice, .keep_all = TRUE)

q_4_1_1_6_3<-sen_choices%>%
  filter(list_name== "4_1_1_6_3")%>%
  mutate(module="context",
         indicator= "farm_characteristics",
         subindicator= "climate_resilience_adaptative_capacity",
         type= "select_one 4_1_1_6_3",
         name_question= "_4_1_1_6_1",
         label_question= "What is covered, e.g. losses to crops/livestock/buildings from weather events, pest outbreaks, market shocks?",
         type_question="select_one",
         label_choice.country=c("Les  pertes dues aux événements météorologiques.",
                                "Les pertes dues aux épidémies de ravageurs.",
                                "Les pertes dues aux chocs du marché.",
                                "Autre (veuillez préciser"))
         
  
#Add country choices to global choices
sen_global_choices<-global_choices%>%
  rbind(sen_choices)%>%
  arrange(desc(country == "global"))%>% 
  #Removing duplicates
  distinct(list_name,name_choice, .keep_all = TRUE) %>%
  right_join(global_survey,by="list_name",relationship="many-to-many")%>%
  left_join(sen_choices,by=c("list_name","name_choice"))%>%
  rename("label_choice"="label_choice.x",
         "label_choice.country"="label_choice.y",
         "country"="country.x")%>%
  select(-country.y)%>%
  rbind(q_4_1_1_6_3)
names(sen_global_choices)

### LAOS ----
#link to ken data: https://cgiar-my.sharepoint.com/:f:/r/personal/andrea_sanchez_cgiar_org/Documents/Bioversity/AI/HOLPA/HOLPA_data/Laos/laos_data_clean?csf=1&web=1&e=D7sIkb
#INSTRUCTION: Replace lao_data_path path with your path, run the code and then go #### CONTEXT MODULE 
lao_data_path <- "C:/Users/andreasanchez/OneDrive - CGIAR/Bioversity/AI/HOLPA/HOLPA_data/Laos/laos_data_clean/" #path andrea

lao_h_survey_file <- paste0(lao_data_path, "lao_holpa_household_survey_clean.xlsx")
lao_h_choices_file <- paste0(lao_data_path, "lao_holpa_household_form_clean.xlsx")

lao_survey_main <- read_and_process_survey_xlsx("Final_HOLPA_Laos", "_id", lao_h_survey_file,"laos","_index")
lao_survey_1_4_2_7_begin_repeat <- read_and_process_survey_xlsx("_1_4_2_7_begin_repeat", "_submission__id", lao_h_survey_file,"laos","_index") # Section: Crop production

lao_choices <- read_excel(lao_h_choices_file,sheet = "choices")%>%
  mutate(country= "laos")%>%
  select("list_name","name","label::English ((en))","country")%>%
  rename("label_choice" = "label::English ((en))")%>%
  rename("name_choice" = "name")%>%
  distinct(list_name,name_choice,label_choice, .keep_all = TRUE)

#Add country choices to global choices
lao_global_choices<-global_choices%>%
  rbind(lao_choices)%>%
  arrange(desc(country == "global")) %>%
  #Removing duplicates
  distinct(list_name,name_choice, .keep_all = TRUE) %>%
  right_join(global_survey,by="list_name",relationship="many-to-many")%>%
  mutate(label_choice.country=NA)

### PERU ----
#link to zwe data: https://cgiar-my.sharepoint.com/:f:/r/personal/andrea_sanchez_cgiar_org/Documents/Bioversity/AI/HOLPA/HOLPA_data/Peru/peru_data_clean?csf=1&web=1&e=azqxKc
#INSTRUCTION: Replace per_data_path path with your own path, run the code and then go ####  CONTEXT MODULE
per_read_and_process_survey_xlsx <- function(sheet_name, column_id_rename, data_path, country_name,name_parent_table,index_column) {
  survey_data <- read_excel(path = data_path, sheet = sheet_name) %>%
    mutate(country = country_name,
           sheet_id = sheet_name) %>%
    rename("kobo_farmer_id" := !!column_id_rename)%>%
    mutate(index= kobo_farmer_id)%>%
    slice(-1)
  # Automatically rename columns for begin_groups groups
  if (grepl("begin_group", tolower(sheet_name))) {
    survey_data <- survey_data %>%
      mutate(parent_table_name= name_parent_table,
             parent_index = kobo_farmer_id)
  }
  # Automatically rename columns for begin_repeat groups
  if (grepl("begin_repeat", tolower(sheet_name))) {
    survey_data <- survey_data %>%
      mutate(parent_table_name= name_parent_table) %>%
      dplyr::rename("parent_index" = "index",
                    "index" := !!index_column)
  }
  
  return(survey_data)
}

per_data_path <- "C:/Users/andreasanchez/OneDrive - CGIAR/Bioversity/AI/HOLPA/HOLPA_data/Peru/peru_data_clean/" #path andrea

per_h_survey_file <- paste0(per_data_path, "per_holpa_household_survey_clean.xlsx")
per_h_choices_file <- paste0(per_data_path, "per_holpa_household_form_clean.xlsx")

per_survey_main<-per_read_and_process_survey_xlsx("maintable", "hid", per_h_survey_file,"peru") #main survey
per_survey_4_2_1_begin_group<-per_read_and_process_survey_xlsx("_4_2_1_begin_group", "hid", per_h_survey_file,"peru","maintable")  # climate perception _4_2_1_begin_group
per_survey_1_4_1_begin_group<-per_read_and_process_survey_xlsx("_1_4_1_begin_group", "hid", per_h_survey_file,"peru","maintable")  # land tenure _1_4_1_begin_group
per_survey_1_4_2_begin_group<-per_read_and_process_survey_xlsx("_1_4_2_begin_group", "hid", per_h_survey_file,"peru","rowuuid")  # farm characteristics _1_4_2_begin_group
per_survey_4_1_7_1_begin_group<-per_read_and_process_survey_xlsx("_4_1_7_1_begin_group", "hid", per_h_survey_file,"peru","maintable") # soil characteristics  _4_1_7_1_begin_group
per_survey_3_3_4_begin_group<- per_read_and_process_survey_xlsx("_3_3_4_begin_group", "hid", per_h_survey_file,"peru","maintable") # water section _3_3_4_begin_group
per_survey_4_1_3_begin_group<-per_read_and_process_survey_xlsx("_4_1_3_begin_group", "hid", per_h_survey_file,"peru","maintable") # resilience section _4_1_3_begin_group
per_survey_3_2_1_3_1_begin_group<-per_read_and_process_survey_xlsx("_3_2_1_3_1_begin_group", "hid", per_h_survey_file,"peru","maintable")%>% # socio-economic section _2_8_4_begin_group
  mutate("_1_2_1_17"="minutes",
         "_4_1_5_2_1"="minutes",
         "_4_1_5_2_2"="minutes",
         "_4_1_5_2_3"="minutes",
         "_4_1_5_2_4"="minutes",
         "_4_1_5_2_5"="minutes",
         "_4_1_5_2_6"="minutes",
         "_4_1_5_2_7"="minutes")
per_survey_4_1_1_5_begin_group<-per_read_and_process_survey_xlsx("_4_1_1_5_begin_group", "hid", per_h_survey_file,"peru","maintable") # access to credit _4_1_1_5_begin_group
per_survey_1_2_1_begin_group<-per_read_and_process_survey_xlsx("_1_2_1_begin_group", "hid", per_h_survey_file,"peru","maintable") # household information section _1_2_1_begin_group
per_survey_2_4_1_begin_group<-per_read_and_process_survey_xlsx("_2_4_1_begin_group", "hid", per_h_survey_file,"peru","maintable") #income section _2_4_1_begin_group
per_survey_1_3_1_1_begin_group<-per_read_and_process_survey_xlsx("_1_3_1_1_begin_group", "hid", per_h_survey_file,"peru","maintable") #personal factors section _1_3_1_1_begin_group



per_survey_3_3_4_1_3_begin_repeat<- per_read_and_process_survey_xlsx("_3_3_4_1_3_begin_repeat", "hid", per_h_survey_file,"peru","maintable","_3_3_4_1_3_begin_repeat_rowid") # irrigation section _3_3_4_1_3_begin_repeat
per_survey_3_1_3_begin_group<-per_read_and_process_survey_xlsx("_3_1_3_begin_group", "hid", per_h_survey_file,"peru","maintable")  # diet _3_1_3_begin_group
per_survey_3_1_2_begin_group<-per_read_and_process_survey_xlsx("_3_1_2_begin_group", "hid", per_h_survey_file,"peru","maintable")  # farmer agency _3_1_2_begin_group
per_survey_3_1_1_begin_group<-per_read_and_process_survey_xlsx("_3_1_1_begin_group", "hid", per_h_survey_file,"peru","maintable")  # human well being _3_1_1_begin_group
per_survey_2_8_4_begin_group<-per_read_and_process_survey_xlsx("_2_8_4_begin_group", "hid", per_h_survey_file,"peru","maintable") # energy section _2_8_4_begin_group
per_survey_3_3_1_begin_group<-per_read_and_process_survey_xlsx("_3_3_1_begin_group", "hid", per_h_survey_file,"peru","maintable") # biodiversity section _3_3_1_begin_group
per_survey_3_4_3_1_2_begin_repeat<-per_read_and_process_survey_xlsx("_3_4_3_1_2_begin_repeat", "hid", per_h_survey_file,"peru","maintable","_3_4_3_1_2_begin_repeat_rowid") #crop production _3_4_3_1_2_begin_repeat
per_survey_3_4_2_2_2_begin_repeat<- per_read_and_process_survey_xlsx("_3_4_2_2_2_begin_repeat", "hid", per_h_survey_file,"peru","maintable","_3_4_2_2_2_begin_repeat_rowid") #livestock production 1 _3_4_2_2_2_begin_repeat
per_survey_3_4_2_2_6_begin_repeat<-per_read_and_process_survey_xlsx("_3_4_2_2_6_begin_repeat", "hid", per_h_survey_file,"peru","_3_4_2_2_2_begin_repeat","_3_4_2_2_6_begin_repeat_rowid")%>% #livestock production 2 _3_4_2_2_6_begin_repeat
  select(-parent_index)%>%rename("parent_index" ="_3_4_2_2_2_begin_repeat_rowid")
per_survey_3_4_2_3_2_begin_repeat<-per_read_and_process_survey_xlsx("_3_4_2_3_2_begin_repeat", "hid", per_h_survey_file,"peru","maintable","_3_4_2_3_2_begin_repeat_rowid") #fish production 1 _3_4_2_3_2_begin_repeat
per_survey_3_4_2_3_2_4_begin_repeat<-per_read_and_process_survey_xlsx("_3_4_2_3_2_4_begin_repeat", "hid", per_h_survey_file,"peru","_3_4_2_3_2_begin_repeat","_3_4_2_3_2_4_begin_repeat_rowid")%>% #fish production 2 _3_4_2_3_2_4_begin_repeat
  select(-parent_index)%>%rename("parent_index" ="_3_4_2_3_2_begin_repeat_rowid")
per_survey_3_4_1_1_7_1_begin_repeat<-per_read_and_process_survey_xlsx("_3_4_1_1_7_1_begin_repeat", "hid", per_h_survey_file,"peru","maintable","_3_4_1_1_7_1_begin_repeat_rowid") ##_3_4_1_1_7_1_begin_repeat: household members permanent workers
per_survey_3_4_1_1_7_2_begin_repeat<-per_read_and_process_survey_xlsx("_3_4_1_1_7_2_begin_repeat", "hid", per_h_survey_file,"peru","maintable","_3_4_1_1_7_2_begin_repeat_rowid") ##_3_4_1_1_7_2_begin_repeat: household members seasonal workers 1
per_survey_3_4_1_2_7_2_1_begin_repeat<-per_read_and_process_survey_xlsx("_3_4_1_2_7_2_1_begin_repeat", "hid", per_h_survey_file,"peru","_3_4_1_1_7_2_begin_repeat","_3_4_1_2_7_2_1_begin_repeat_rowid")%>% ##_3_4_1_2_7_2_1_begin_repeat: household members seasonal workers 2
  select(-parent_index)%>%rename("parent_index" ="_3_4_1_1_7_2_begin_repeat_rowid")
per_survey_3_4_1_2_1_1_begin_repeat<-per_read_and_process_survey_xlsx("_3_4_1_2_1_1_begin_repeat", "hid", per_h_survey_file,"peru","maintable","_3_4_1_2_1_1_begin_repeat_rowid") ##_3_4_1_2_1_1_begin_repeat: labour Hired/Free/Exchange Labourers permanent workers
per_survey_3_4_1_2_1_2_begin_repeat<-per_read_and_process_survey_xlsx("_3_4_1_2_1_2_begin_repeat", "hid", per_h_survey_file,"peru","maintable","_3_4_1_2_1_2_begin_repeat_rowid")  ##_3_4_1_2_1_2_begin_repeat: labour Hired/Free/Exchange Labourers seasonal workers 1
per_survey_3_4_1_2_1_2_1_begin_repeat<-per_read_and_process_survey_xlsx("_3_4_1_2_1_2_1_begin_repeat", "hid", per_h_survey_file,"peru","_3_4_1_2_1_2_begin_repeat","_3_4_1_2_1_2_1_begin_repeat_rowid")%>%  ##_3_4_1_2_1_2_1_begin_repeat: labour Hired/Free/Exchange Labourers seasonal workers 2
  select(-parent_index)%>%rename("parent_index" ="_3_4_1_2_1_2_begin_repeat_rowid")



per_survey_4_1_4_begin_group<-per_read_and_process_survey_xlsx("_4_1_4_begin_group", "hid", per_h_survey_file,"peru","maintable") # assets section _4_1_4_begin_group
per_survey_1_2_1_4_begin_group<-per_read_and_process_survey_xlsx("_1_2_1_4_begin_group", "hid", per_h_survey_file,"peru","maintable") # farmer information _1_2_1_4_begin_group
per_survey_4_1_1_7_begin_group<-per_read_and_process_survey_xlsx("_4_1_1_7_begin_group", "hid", per_h_survey_file,"peru","maintable") # inputs subsidy  _4_1_1_7_begin_group


per_survey_3_3_3_2_begin_repeat<-per_read_and_process_survey_xlsx("_3_3_3_2_begin_repeat", "hid", per_h_survey_file,"peru","maintable","_3_3_3_2_begin_repeat_rowid") # Section: area of land per agricultural practice  _3_3_3_2_begin_repeat


per_choices <- read_excel(per_h_choices_file, sheet = "choices")%>%
  mutate(country= "peru")%>%
  select("list_name","name","label::English (en)","country")%>%
  rename("label_choice" = "label::English (en)")%>%
  rename("name_choice" = "name")%>%
  distinct(list_name,name_choice,label_choice, .keep_all = TRUE)

#Add country choices to global choices
per_global_choices<-global_choices%>%
  rbind(per_choices)%>%
  arrange(desc(country == "global")) %>%
  #Removing duplicates
  distinct(list_name,name_choice, .keep_all = TRUE) %>%
  right_join(global_survey,by="list_name",relationship="many-to-many")%>%
  mutate(label_choice.country=NA)%>%
  dplyr::bind_rows(data.frame(
    list_name= c(rep("3_3_1_2",8)),
    name_choice= c(rep(c("high","medium","low","none"),2)),
    label_choice= c(rep(c("High: five or more species with different heights, woodiness or flowering seasons.","Medium: two to four species.","Low: only one species.","None"),2)),
    country= c(rep("peru",8)),module=c(rep("agroecology",8)),indicator=c(rep( "5_biodiversity")),subindicator=c(rep( "5_biodiversity")),
    type= c(rep("select_one 3_3_1_2",8)),
    type_question=c(rep("select_one",8)),
    name_question= c(rep("_3_3_1_2_10",4),rep("_3_3_1_2_11",4)),
    label_question = c("How would you describe the plant diversity (i.e., number of plant species) in: Young fallow (less than 10 years)",
                       "How would you describe the plant diversity (i.e., number of plant species) in: Old fallow (more or equal than 10 years)"),
    label_choice.country= c(rep(NA,8)),
    stringsAsFactors = FALSE))%>%
  
  mutate(label_choice = case_when(
    type =="select_one 1_2_1_12_1"& name_choice %in%c(1,2)~"Primary",
    type =="select_one 1_2_1_12_1"& name_choice %in%c(3,4)~"Seconday",
    type =="select_one 1_2_1_12_1"& name_choice %in%c(5:10)~"Higher",
    type =="select_one 1_2_1_12_1"& name_choice==11~"None",
    TRUE ~ label_choice))%>%
  #filter(!is.na(list_name))
  filter(!(name_question == "_1_2_1_1" & type == "text"))%>%
  filter(!(name_question == "_1_2_1_2" & type == "text"))


  


#### CONTEX MODULE ####
#INSTRUCTION: Continue running the code from here
fun_context_choices<- function(country_global_choices) {
  # Filter and mutate the data frame
  context_choices <- country_global_choices %>%
  filter(str_detect(module, "context"))%>%
  mutate(module= "context")

  #"production_end_use/production_systems" these question is part of two indicators
  duplicate_rows <-  context_choices%>% filter(str_detect(subindicator, "production_end_use/production_systems"))
  duplicate_rows$subindicator <- "production_end_use"
  context_choices <- rbind(context_choices, duplicate_rows)
  
  context_choices$subindicator[str_detect(context_choices$subindicator,"respondent_characteristics")]<- "respondent_characteristics"
  context_choices$subindicator[str_detect(context_choices$subindicator,"household_characteristic")]<- "household_characteristics"
  context_choices$subindicator[str_detect(context_choices$subindicator,"context_all")]<- "context_all"
  context_choices$subindicator[str_detect(context_choices$subindicator,"farm_characteristic")]<- "farm_characteristics"
  context_choices$subindicator[str_detect(context_choices$subindicator,"inputs")]<- "inputs"
  context_choices$subindicator[str_detect(context_choices$subindicator,"production_systems")]<- "production_systems"
  context_choices$subindicator[str_detect(context_choices$subindicator,"production_end_use")]<- "production_end_use"
  
  context_choices$subindicator[str_detect(context_choices$subindicator,"household_labour")]<- "household_labour"
  context_choices$subindicator[str_detect(context_choices$subindicator,"credit_access")]<- "credit_access"
  context_choices$subindicator[str_detect(context_choices$subindicator,"income")]<- "income"
  context_choices$subindicator[str_detect(context_choices$subindicator,"household_labour")]<- "household_labour"
  context_choices$subindicator[str_detect(context_choices$subindicator,"training")]<- "training"
  context_choices$subindicator[str_detect(context_choices$subindicator,"education")]<- "education"
  context_choices$subindicator[str_detect(context_choices$subindicator,"literacy")]<- "literacy"
  context_choices$subindicator[str_detect(context_choices$subindicator,"membership")]<- "membership"
  context_choices$subindicator[str_detect(context_choices$subindicator,"accessibility")]<- "accessibility"

  context_choices$indicator[str_detect(context_choices$indicator,"farm_characteristics")]<- "farm_characteristics"
  context_choices$indicator[str_detect(context_choices$indicator,"farm_characteristic")]<- "farm_characteristics"
  context_choices$indicator[str_detect(context_choices$indicator,"household_characteristic")]<- "household_characteristics"
  context_choices$indicator[str_detect(context_choices$indicator,"household_characteristics")]<- "household_characteristics"
  context_choices$indicator[str_detect(context_choices$indicator,"respondent_characteristics")]<- "respondent_characteristics"
  context_choices$indicator[str_detect(context_choices$indicator,"inputs")]<- "inputs"
  context_choices$indicator[str_detect(context_choices$indicator,"context_all")]<- "context_all"
  
  context_choices<- context_choices%>%
  rename(theme = indicator,
         indicator = subindicator)%>%
  mutate(name_question_choice= if_else(type_question=="select_multiple",
                                     paste(name_question,"/",name_choice, sep=""),
                                     name_question))
  return(context_choices)
}     
                                                                                     

fun_context_questions_columns<- function(country_context_choices) {
  context_questions_columns<- country_context_choices%>% 
    dplyr::select(label_question, name_question_choice)%>%
    dplyr::distinct(name_question_choice, .keep_all = TRUE)%>%
    spread(key = name_question_choice, value = label_question)%>%
    mutate("kobo_farmer_id"="kobo_farmer_id",
           "country"="country_name",
           "sheet_id"="sheet_id",
           "parent_table_name"="_parent_table_name",
           "index"="index",
           "parent_index"="_parent_index")
  
  context_questions_columns <- colnames(context_questions_columns)
  
  return(context_questions_columns)
  
}

fun_context_left_join <- function(context_choices, gathered_data ) {
  
  # Left join for "calculate" and "integer"
  continuous <- gathered_data  %>%
    dplyr::left_join(select(context_choices,
                            c(name_question, module, theme, indicator,"name_choice", label_choice, label_question,type, type_question, list_name)), 
                     by = "name_question")%>%
    filter(type_question =="calculate"|type_question =="integer"|type_question =="note"|
             type_question =="text"|type_question =="audio"|type_question =="decimal"| type_question =="geopoint")%>%
    select(-name_choice.y)%>%
    rename("name_choice"="name_choice.x")
  
  # Left join for "select_multiple"
  select_multiple <- gathered_data  %>%
    left_join(select(context_choices,
                     c(name_question, name_choice, module, theme, indicator, label_choice, label_question, type, type_question, list_name,name_question_choice)), 
              by = c("name_question"="name_question_choice"))%>%
    filter(type_question=="select_multiple")%>%
    select(-name_choice.y,-name_question.y)%>%
    rename("name_choice"="name_choice.x")%>%
    #Remove answers == "0" or NA
    filter(type_question == "select_multiple" & !is.na(name_choice) & name_choice != 0)
  
  # Left join for "select_one" for country== "zwe"
  select_one1 <- gathered_data  %>%
    left_join(select(context_choices,
                     c(name_question, name_choice, module, theme, indicator, label_choice, label_question, type, type_question, list_name)), 
              by = c("name_question"="name_question", "name_choice"="name_choice"))%>%
    filter(type_question=="select_one")%>%
    filter(country=="zimbabwe"|
             country=="kenya"|
             country=="laos"|
             country=="peru")
  
  # Left join for "select_one" for country== "tun"
  select_one2 <- gathered_data  %>%
    left_join(select(context_choices,
                     c(name_question, name_choice, module, theme, indicator, label_choice, label_question, type, type_question, list_name)), 
              by = c("name_question"="name_question", "name_choice"="label_choice"))%>%
    dplyr::rename("label_choice"="name_choice")%>%
    dplyr::rename("name_choice"="name_choice.y")%>%
    filter(type_question=="select_one")%>%
    filter(country=="tunisia")
  
  select_one3 <- gathered_data  %>%
    left_join(select(context_choices,
                     c(name_question, name_choice, module, theme, indicator, label_choice, label_question, type, type_question, list_name,label_choice.country)), 
              by = c("name_question"="name_question", "name_choice"="label_choice.country"))%>%
    select(-name_choice)%>%
    dplyr::rename("name_choice"="name_choice.y")%>%
    filter(type_question=="select_one")%>%
    filter(country== "senegal")
  
  result<- rbind(continuous,select_multiple,select_one1,select_one2,select_one3)
  
  return(result)
}

### Function to get answers from the following sections ---- 
## Main survey and begin_repeat  ----
fun_context_main<- function(country_global_choices,country_survey_main){
  country_context_choices<-  fun_context_choices(country_global_choices)
  country_context_question_columns<- fun_context_questions_columns(country_context_choices)
  
  country_context_columns <- intersect(country_context_question_columns, colnames(country_survey_main))
  
  country_context <- country_survey_main %>%
    select(all_of(country_context_columns))%>%
    mutate_all(as.character)
  
  # Identify columns with only NA values
  na_columns <- colSums(is.na(country_context)) == nrow(country_context)
  
  # Remove columns with only NA values
  country_context <- country_context[, !na_columns]
  
  result_main_survey <- country_context%>%
    gather(key = "name_question", value = "name_choice", -kobo_farmer_id, -country,-sheet_id,-index)%>%
    fun_context_left_join(country_context_choices,.)%>%
    mutate(name_question_recla= name_question)%>%
    mutate(parent_table_name= NA,
           parent_index=NA)
  
  if (grepl("begin_repeat", tolower(country_context$sheet_id[1]))) {
    result_main_survey <- country_context%>%
      gather(key = "name_question", value = "name_choice", -kobo_farmer_id, -country,-sheet_id,-index,-parent_table_name,-parent_index)%>%
      fun_context_left_join(country_context_choices,.)%>%
      mutate(name_question_recla= name_question)
  }
  
  if (grepl("_3_3_4_1_3_begin_repeat", tolower(country_context$sheet_id[1]))) {
    result_main_survey <- country_context%>%
      gather(key = "name_question", value = "name_choice", -kobo_farmer_id, -country,-sheet_id,-index,-parent_table_name,-parent_index)%>%
      fun_context_left_join(country_context_choices,.)%>%
      mutate(name_question_recla= name_question)%>%
      mutate(name_question_recla = str_remove(name_question_recla, "/.*"))
  }
  
  return(result_main_survey)
}

# For Peru select only the select_multiple columns
per_fun_context_questions_columns<- function(context_choices) {
  per_context_questions_columns<- context_choices%>% 
    filter(type_question=="select_multiple")%>%
    dplyr::select(name_question, label_question)%>%
    dplyr::distinct(name_question, .keep_all = TRUE)%>%
    spread(key = name_question, value = label_question)%>%
    mutate("kobo_farmer_id"="kobo_farmer_id",
           "country"="country_name",
           "sheet_id"="sheet_id",
           "parent_table_name"="_parent_table_name",
           "index"="index",
           "parent_index"="_parent_index")
  
  per_context_questions_columns <- colnames(per_context_questions_columns)
  per_context_questions_columns
  
  return(per_context_questions_columns)
  
}

# For peru select_multiple questions
per_fun_context_main<- function(country_global_choices, country_survey_main) {
  
  # Step 1: Apply per_fun_context_main logic
  per_country_context_choices <- fun_context_choices(country_global_choices)
  per_country_context_question_columns <- per_fun_context_questions_columns(per_country_context_choices)
  
  per_country_context_columns <- intersect(per_country_context_question_columns, colnames(country_survey_main))
  mismatched_columns <- setdiff(per_country_context_question_columns, per_country_context_columns)
  
  per_country_context <- country_survey_main %>%
    select(all_of(per_country_context_columns)) %>%
    mutate_all(as.character)
  
  # Remove columns with only NA values
  na_columns <- colSums(is.na(per_country_context)) == nrow(per_country_context)
  per_country_context <- per_country_context[, !na_columns]
  
  # Step 2: Apply generate_binary_matrix logic
  # Initialize the result dataframe with kobo_farmer_id
  result <- country_survey_main %>% select(kobo_farmer_id,"country", "sheet_id", "index")
  
  if (grepl("begin_group|begin_repeat", tolower(country_survey_main$sheet_id[1]))) {
    result <- country_survey_main %>% select(kobo_farmer_id,"country", "sheet_id", "index","parent_table_name","parent_index")
  }
  
  cols_to_process <- names(per_country_context)[!names(per_country_context) %in% c("kobo_farmer_id", "country", "sheet_id", "index")]
  
  for (col in cols_to_process) {
    
    # Separate rows based on comma-separated values
    data_long <- per_country_context %>%
      select(kobo_farmer_id,index, all_of(col)) %>%
      separate_rows(all_of(col), sep = ",") %>%
      mutate(!!col := paste0(col, "/", !!sym(col)))  # Append the column name
    
    # Create binary columns for each value
    data_wide <- data_long %>%
      mutate(value = 1) %>%
      pivot_wider(names_from = all_of(col), values_from = value, values_fill = 0)
    
    # Merge the result with the wide data (binary columns)
    result <- result %>%
      left_join(data_wide, by = c("kobo_farmer_id","index"))
  }
  
  country_context_choices<-  fun_context_choices(country_global_choices)
  
  per_result_main_survey <- result%>%
    gather(key = "name_question", value = "name_choice", -kobo_farmer_id, -country,-sheet_id,-index)%>%
    left_join(select(country_context_choices,
                     c(name_question, name_choice, module, theme, indicator, label_choice, label_question, type, type_question, list_name,name_question_choice)), 
              by = c("name_question"="name_question_choice"))%>%
    select(-name_choice.y,-name_question.y)%>%
    rename("name_choice"="name_choice.x")%>%
    #Remove answers == "0" or NA
    filter(type_question == "select_multiple" & !is.na(name_choice))%>%
    mutate(name_question_recla= name_question)%>%
    filter(name_choice!=0)%>%
    mutate(parent_table_name= NA,
           parent_index=NA)
  
  # Automatically rename columns for begin_groups groups
  if (grepl("begin_group|begin_repeat", tolower(result$sheet_id[1]))) {
    per_result_main_survey <- result%>%
      gather(key = "name_question", value = "name_choice", -kobo_farmer_id, -country,-sheet_id,-index,-parent_table_name,-parent_index)%>%
      left_join(select(country_context_choices,
                       c(name_question, name_choice, module, theme, indicator, label_choice, label_question, type, type_question, list_name,name_question_choice)), 
                by = c("name_question"="name_question_choice"))%>%
      select(-name_choice.y,-name_question.y)%>%
      rename("name_choice"="name_choice.x")%>%
      #Remove answers == "0" or NA
      filter(type_question == "select_multiple" & !is.na(name_choice))%>%
      mutate(name_question_recla= name_question)%>%
      filter(name_choice!=0)
    
  }
  
  # Return the final binary matrix
  return(per_result_main_survey)
}

### Function to combine answers from all context sections ---- 
fun_context_data<- function(country_global_choices,
                                country_survey_main, #main survey
                                country_survey_1_4_2_7_begin_repeat   ##production_end_use for OTHER NON-FARM PRODUCT
                                ) {
  context_data<- rbind(
    fun_context_main(country_global_choices, country_survey_main), ## Main survey
    fun_context_begin_repeat(country_global_choices, country_survey_1_4_2_7_begin_repeat)  ##production_end_use for OTHER NON-FARM PRODUCT
  )
  return(context_data)
}

fun_context<- function(country_context_data){
  
  country_context<-country_context_data%>%
  
  #Indicator: climate_drought and climate_flood
  #Zimbabwe error: the choices options should be "4_2_1_3" (1=yes, 0=no, notsure), but the country put "4_2_1_1" (increased, nochange, decreased, notsure)
  #So I reclassified those options into 1=yes, 0=no, notsure
  mutate( name_choice=case_when(
    country== "zimbabwe"& name_question %in% c("_4_2_1_3_2","_4_2_1_3_1") & name_choice%in% c("increase","decrease") ~ "1",
    country== "zimbabwe"&name_question %in% c("_4_2_1_3_2","_4_2_1_3_1") & name_choice%in% c("nochange") ~ "0",
    TRUE ~ name_choice))%>%
  mutate(label_choice=case_when(
    country== "zimbabwe"& name_question %in% c("_4_2_1_3_2","_4_2_1_3_1") & name_choice=="1" ~ "Yes",
    country== "zimbabwe"&name_question %in% c("_4_2_1_3_2","_4_2_1_3_1") & name_choice=="0" ~ "No",
    TRUE ~ label_choice))%>%
  #Indicator: "farmer_relation" 
  mutate(name_question_recla = case_when(name_question_recla== "_1_2_1_6_1"~ "_1_2_1_6",TRUE ~name_question_recla))%>%
  filter(!(name_question== "_1_2_1_6"& name_choice== "other"))%>%
  #Indicator: "housing"
  mutate(name_question_recla = case_when(
    name_question_recla== "_3_2_1_3_1_1"~ "_3_2_1_3_1",
    name_question_recla== "_3_2_1_3_2_1"~ "_3_2_1_3_2",
    TRUE ~name_question_recla))%>%
  filter(!(name_question%in% c("_3_2_1_3_1/other","_3_2_1_3_2/other")))%>%
  #Indicator: "membership"
  mutate(name_question_recla = case_when(name_question_recla==  "_2_3_1_1_1"~"_2_3_1_1",TRUE ~name_question_recla))%>%
  filter(!(name_question%in% c("_2_3_1_1_1/other")))%>%
  #Inicator: primary_occupation
  mutate(name_question_recla = case_when(name_question_recla==  "_1_2_1_13_1_1"~"_1_2_1_13_1",TRUE ~name_question_recla))%>%
  filter(!(name_question=="_1_2_1_13_1"&name_choice=="other"))%>%
  #Inicator: secondary_occupation
  mutate(name_question_recla = case_when(name_question_recla==  "_1_2_1_13_2_2_1"~"_1_2_1_13_2_2",TRUE ~name_question_recla))%>%
  filter(!(name_question=="_1_2_1_13_2_2/other"))%>%
  #all indicators
    
    #For the countries that translated the name of the crops, livestock and fish to English separated with "//"
    mutate(name_choice= case_when(
      name_question %in%c("_3_2_1_3_2_1","_3_2_1_2_1","_2_3_1_1_1","_1_2_1_15_1","_4_1_1_4_4_1","_1_4_3_3_1_calculate",
                                "_1_4_3_6_4","_1_4_3_6_1_calculate","_1_4_3_7_1_calculate","_1_4_3_8_1","_1_4_2_3_7_1",
                                "_1_4_2_7_calculate","_1_4_2_7_7_1","_1_2_1_6_1","_1_2_1_13_1_1","_1_2_1_13_2_2_1",
                                "_4_1_1_6_1","_1_4_2_2_6_1")& grepl("//", name_choice)~ sub(".*//", "", name_choice),
      TRUE ~ name_choice))%>%
  #Put the unit of area for all necessary questions
  mutate(label_choice= case_when(
    name_question %in% c("_1_4_1_1_1", "_1_4_1_1_2", "_1_4_1_1_3","_3_4_2_1_3", "_1_4_3_2_3","_1_4_3_3_3","_1_4_3_4_3","_1_4_3_6_3","_3_3_3_2_2","_1_4_4_4_1","_1_4_3_7_3")& country== "kenya" & kobo_farmer_id == "286844609"~"hectares",
    name_question%in% c( "_1_4_1_1_1", "_1_4_1_1_2", "_1_4_1_1_3","_3_4_2_1_3", "_1_4_3_2_3","_1_4_3_3_3","_1_4_3_4_3","_1_4_3_6_3","_3_3_3_2_2","_1_4_4_4_1","_1_4_3_7_3")& country== "senegal" & kobo_farmer_id == "308802823"~"metres square",
    name_question%in% c( "_1_4_1_1_1", "_1_4_1_1_2", "_1_4_1_1_3","_3_4_2_1_3", "_1_4_3_2_3","_1_4_3_3_3","_1_4_3_4_3","_1_4_3_6_3","_3_3_3_2_2","_1_4_4_4_1","_1_4_3_7_3")& country %in%c("zimbabwe","kenya")~"acres",
    name_question%in% c( "_1_4_1_1_1", "_1_4_1_1_2", "_1_4_1_1_3","_3_4_2_1_3", "_1_4_3_2_3","_1_4_3_3_3","_1_4_3_4_3","_1_4_3_6_3","_3_3_3_2_2","_1_4_4_4_1","_1_4_3_7_3")& country %in% c("tunisia","senegal")~"hectares",
    TRUE ~ label_choice))%>%
  
  mutate(name_question_recla = case_when(
    type_question == "select_multiple"~str_replace(name_question_recla, "/.*", ""),
    TRUE ~ name_question_recla))%>%
  mutate(name_choice = case_when(
    type_question == "select_multiple"~ sub("^.*/", "", name_question), # replace name_question by the type of energy
    TRUE ~ name_choice))%>%
  # Remove rows name_choice == NA
  filter(!is.na(name_choice))%>%
    filter(!(country =="laos"& name_question_recla == "_1_2_1_1" & type == "text"))%>%
    filter(!(country =="laos"& name_question_recla == "_1_2_1_2" & type == "text"))%>%
    mutate(name_choice=case_when(
      name_choice== "Kilogrammes"~"Kilograms",
      name_choice%in% c("Sacs","Sac")~"Bags",
      name_choice%in%c("Charette","Charette_1")~"Cart",
      name_choice=="Litres"~"Liters",
      name_choice== "50kg sac"~"50kg bag",
      name_choice=="Metre cube" ~"Cubic meter",
      name_choice=="laisser les animaux divaguer à l,intérieur"~  "let the animals roam inside",
      name_choice=="Chargement voiture"~  "Car loading",
      name_choice== "Grammes"~"Grams",
      TRUE ~ name_choice))
  
  
  return(country_context)
}

#INSTRUCTION: Go to your country section

## CONTEXT DATA FORMAT BY COUNTRY -----
## If the farmers doesn't know the answer put 9999-----
# ZIMBABWE -----
zwe_context_data<-fun_context_data(zwe_global_choices, #country_global_choices
                                   zwe_survey_main,  #Main survey 
                                   zwe_survey_1_4_2_7_begin_repeat)  #production_end_use for OTHER NON-FARM PRODUCT
    
zwe_context<-fun_context(zwe_context_data)
write.csv(zwe_context,paste0(zwe_data_path,"/zwe/zwe_context_format.csv"),row.names=FALSE)

# TUNISIA-----
#tun_survey_1_4_2_7_begin_repeat,  ## tunisia does not have this begin_repeat production_end_use for OTHER NON-FARM PRODUCT  
tun_context_data<-    fun_context_main(tun_global_choices, #country_global_choices
                                       tun_survey_main) # Main survey

tun_context<-fun_context(tun_context_data)
write.csv(tun_context,paste0(tun_data_path,"/tun/tun_context_format.csv"),row.names=FALSE)

# KENYA-----
#ken_survey_1_4_2_7_begin_repeat,  ## kenya does not have this begin_repeat production_end_use for OTHER NON-FARM PRODUCT  
ken_context_data<-    fun_context_main(ken_global_choices, #country_global_choices
                                       ken_survey_main) # Main survey


ken_context<-fun_context(ken_context_data)
write.csv(ken_context,paste0(ken_data_path,"/ken/ken_context_format.csv"),row.names=FALSE)

# SENEGAL -----
sen_context_data<-fun_context_data(sen_global_choices, #country_global_choices
                                   sen_survey_main,  #Main survey 
                                   sen_survey_1_4_2_7_begin_repeat)  #production_end_use for OTHER NON-FARM PRODUCT

sen_context<-fun_context(sen_context_data)
write.csv(sen_context,paste0(sen_data_path,"/sen/sen_context_format.csv"),row.names=FALSE)



# LAOS-----
lao_context_data<-fun_context_data(lao_global_choices, #country_global_choices
                                   lao_survey_main,  #Main survey 
                                   lao_survey_1_4_2_7_begin_repeat)  #production_end_use for OTHER NON-FARM PRODUCT

lao_context<-fun_context(lao_context_data)
write.csv(lao_context,paste0(lao_data_path,"/lao/lao_context_format.csv"),row.names=FALSE)

# PERU -----
names(per_survey_main)
per_context_data<-rbind(
  fun_context_main(per_global_choices,per_survey_main), ## Main survey
  fun_context_main(per_global_choices,per_survey_4_2_1_begin_group),  # climate perception _4_2_1_begin_group
  fun_context_main(per_global_choices,per_survey_1_4_1_begin_group), # land tenure _1_4_1_begin_group
  fun_context_main(per_global_choices,per_survey_1_4_2_begin_group),  # farm characteristics _1_4_2_begin_group
  fun_context_main(per_global_choices,per_survey_4_1_7_1_begin_group), # soil characteristics  _4_1_7_1_begin_group
  fun_context_main(per_global_choices,per_survey_3_3_4_begin_group), # water section _3_3_4_begin_group
  fun_context_main(per_global_choices,per_survey_4_1_3_begin_group), # resilience section _4_1_3_begin_group
  fun_context_main(per_global_choices,per_survey_3_2_1_3_1_begin_group), # socio-economic section _2_8_4_begin_group
  fun_context_main(per_global_choices,per_survey_4_1_1_5_begin_group), # access to credit _4_1_1_5_begin_group
  fun_context_main(per_global_choices,per_survey_1_2_1_begin_group), # household information section _1_2_1_begin_group
  fun_context_main(per_global_choices,per_survey_2_4_1_begin_group), #income section _2_4_1_begin_group
  fun_context_main(per_global_choices,per_survey_1_3_1_1_begin_group), #personal factors section _1_3_1_1_begin_group
  per_fun_context_main(per_global_choices,per_survey_main), # Main survey
  per_fun_context_main(per_global_choices,per_survey_4_2_1_begin_group),  # climate perception _4_2_1_begin_group
  per_fun_context_main(per_global_choices,per_survey_1_4_2_begin_group),  # farm characteristics _1_4_2_begin_group
  per_fun_context_main(per_global_choices,per_survey_3_2_1_3_1_begin_group)) # socio-economic section _2_8_4_begin_group
  
  
  
  

x<-fun_context_main(per_global_choices,per_survey_4_2_1_begin_group)


  fun_performance_main(per_global_choices,per_survey_3_1_3_begin_group), ##_3_1_3_begin_group: diet quality
  fun_performance_main(per_global_choices,per_survey_3_1_2_begin_group),   # farmer agency _3_1_2_begin_group
  fun_performance_main(per_global_choices,per_survey_3_1_1_begin_group),  # human well being _3_1_1_begin_group
  fun_performance_main(per_global_choices,per_survey_2_8_4_begin_group), # energy section _2_8_4_begin_group
  fun_performance_main(per_global_choices,per_survey_3_3_1_begin_group), # biodiversity section _3_3_1_begin_group
  fun_performance_main(per_global_choices,per_survey_3_3_3_2_begin_repeat), # Section: area of land per agricultural practice  
  fun_performance_main(per_global_choices,per_survey_3_4_3_1_2_begin_repeat), #crop production section _3_4_3_1_2_begin_repeat
  fun_performance_main(per_global_choices,per_survey_3_4_2_2_2_begin_repeat), #livestock production 1 _3_4_2_2_2_begin_repeat
  fun_performance_main(per_global_choices,per_survey_3_4_2_2_6_begin_repeat), #livestock production 2 _3_4_2_2_6_begin_repeat
  fun_performance_main(per_global_choices,per_survey_3_4_2_3_2_begin_repeat), #fish production 1 _3_4_2_3_2_begin_repeat
  fun_performance_main(per_global_choices,per_survey_3_4_2_3_2_4_begin_repeat), #fish production 2 _3_4_2_3_2_4_begin_repeat
  fun_performance_main(per_global_choices,per_survey_3_4_1_1_7_1_begin_repeat), ##_3_4_1_1_7_1_begin_repeat: household members permanent workers
  fun_performance_main(per_global_choices,per_survey_3_4_1_1_7_2_begin_repeat), ##_3_4_1_1_7_2_begin_repeat: household members seasonal workers 1
  fun_performance_main(per_global_choices,per_survey_3_4_1_2_7_2_1_begin_repeat), ##_3_4_1_2_7_2_1_begin_repeat: household members seasonal workers 2
  fun_performance_main(per_global_choices,per_survey_3_4_1_2_1_1_begin_repeat), ###_3_4_1_2_1_1_begin_repeat: labour Hired/Free/Exchange Labourers permanent workers
  fun_performance_main(per_global_choices,per_survey_3_4_1_2_1_2_begin_repeat),  ##_3_4_1_2_1_2_begin_repeat: labour Hired/Free/Exchange Labourers seasonal workers 1
  fun_performance_main(per_global_choices,per_survey_3_4_1_2_1_2_1_begin_repeat),  ##_3_4_1_2_1_2_1_begin_repeat: labour Hired/Free/Exchange Labourers seasonal workers 2
  fun_performance_main(per_global_choices,per_survey_4_1_4_begin_group), # assets section _4_1_4_begin_group
  fun_performance_main(per_global_choices,per_survey_1_2_1_4_begin_group), # farmer information _1_2_1_4_begin_group
  
  fun_performance_main(per_global_choices,per_survey_4_1_1_7_begin_group), # inputs subsidy  _4_1_1_7_begin_group
  
  
  
  per_fun_performance_main(per_global_choices,per_survey_2_8_4_begin_group), # energy section _2_8_4_begin_group
  per_fun_performance_main(per_global_choices,per_survey_3_3_4_begin_group), # water section _3_3_4_begin_group
  per_fun_performance_main(per_global_choices,per_survey_3_4_1_2_7_2_1_begin_repeat), ##_3_4_1_2_7_2_1_begin_repeat: household members seasonal workers 2
  per_fun_performance_main(per_global_choices,per_survey_3_4_1_2_1_2_1_begin_repeat),  ##_3_4_1_2_1_2_1_begin_repeat: labour Hired/Free/Exchange Labourers seasonal workers 2
  per_fun_performance_main(per_global_choices,per_survey_2_4_1_begin_group), #income section _2_4_1_begin_group
  per_fun_performance_main(per_global_choices,per_survey_4_1_1_5_begin_group), # access to credit _4_1_1_5_begin_group
  per_fun_performance_main(per_global_choices,per_survey_4_1_3_begin_group), # resilience section _4_1_3_begin_group
  
  per_fun_performance_main(per_global_choices,per_survey_3_3_4_1_3_begin_repeat), # irrigation section _3_3_4_1_3_begin_repeat
  per_fun_performance_main(per_global_choices,per_survey_3_4_3_1_2_begin_repeat) #crop production section _3_4_3_1_2_begin_repeat
  
)


per_context<-fun_context(per_context_data)
write.csv(per_context,paste0(per_data_path,"/per/per_context_format.csv"),row.names=FALSE)
