#library(openxlsx)
library(tidyr)
library(tidyverse)
library(readxl) 
library(dplyr)
library(summarytools)


#INSTRUCTION: run the following code

# Read excel files
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

# INSTRUCTIONS: Please download HOLPA_global_household_survey_20231204_mapped_to_indicators_master.xlsx in your computer from: https://github.com/andreacsanchezb10/HOLPA
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


#### Import county survey and form ####
#country_holpa_household_survey_clean contains the clean household survey with the questions and responses 
#country_holpa_household_form_clean contains the clean household form with the questions (this form is similar to the one uploaded in koboCollect, but some changes were made to match the global survey [e.g. some countries changed the code of some global questions])
#INTRUCTION: Go to your country section

### ZIMBABWE ----
#link to zwe data: https://cgiar-my.sharepoint.com/:f:/r/personal/andrea_sanchez_cgiar_org/Documents/Bioversity/AI/HOLPA/HOLPA_data/Zimbabwe/zimbabwe_data_clean?csf=1&web=1&e=azqxKc
#INSTRUCTION: Replace zwe_data_path path with your own path, run the code and then go #### AGROECOLOGY MODULE ####
zwe_data_path <- "C:/Users/andreasanchez/OneDrive - CGIAR/Bioversity/AI/HOLPA/HOLPA_data/Zimbabwe/zimbabwe_data_clean/" #path andrea

zwe_h_survey_file <- paste0(zwe_data_path, "zwe_holpa_household_survey_clean.xlsx")
zwe_h_choices_file <- paste0(zwe_data_path, "zwe_holpa_household_form_clean.xlsx")

zwe_survey_main <- read_and_process_survey_xlsx("Final HOLPA_Zimbabwe_Household", "_id", zwe_h_survey_file,"zimbabwe","_index")%>%
  #Remove respondents that are not farmers
  filter(kobo_farmer_id!="274186917")
zwe_survey_1_4_2_7_begin_repeat <- read_and_process_survey_xlsx("_1_4_2_7_begin_repeat", "_submission__id", zwe_h_survey_file,"zimbabwe","_index")%>% # Section: Farm production OTHER
  #Remove respondents that are not farmers
  filter(kobo_farmer_id!="274186917")
zwe_survey_3_3_3_2_begin_repeat<- read_and_process_survey_xlsx("_3_3_3_2_begin_repeat", "_submission__id", zwe_h_survey_file,"zimbabwe","_index") # Section: area of land per agricultural practice


zwe_choices <- read_excel(zwe_h_choices_file, sheet = "choices")%>%
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


### TUNISIA -----
#link to tun data: https://cgiar-my.sharepoint.com/:f:/r/personal/andrea_sanchez_cgiar_org/Documents/Bioversity/AI/HOLPA/HOLPA_data/Tunisia/tunisia_data_clean?csf=1&web=1&e=07Lc0e
#INSTRUCTION: Replace tun_data_path path with your own path, run the code and then go #### AGROECOLOGY MODULE ####
tun_data_path <- "C:/Users/andreasanchez/OneDrive - CGIAR/Bioversity/AI/HOLPA/HOLPA_data/Tunisia/tunisia_data_clean/" #path andrea

tun_h_survey_file <- paste0(tun_data_path, "tun_holpa_household_survey_clean.xlsx")
tun_h_choices_file <- paste0(tun_data_path, "tun_holpa_household_form_clean.xlsx")

tun_survey_main <- read_and_process_survey_xlsx("HOLPA_Tunisia_household_surv", "_id", tun_h_survey_file,"tunisia","_index")%>%
  #Remove respondents that did not wanted to complete the survey
  filter(consent_2!="No")

# Section: Farm production OTHER #Tunisia doesn't have this section
tun_survey_3_3_3_2_begin_repeat<- read_and_process_survey_xlsx("_3_3_3_2_begin_repeat", "_submission__id", tun_h_survey_file,"tunisia","_index") # Section: area of land per agricultural practice

tun_choices <- read_excel(tun_h_choices_file,sheet = "choices")%>%
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
#INSTRUCTION: Replace ken_data_path path with your own path, run the code and then go #### AGROECOLOGY MODULE ####
ken_data_path <- "C:/Users/andreasanchez/OneDrive - CGIAR/Bioversity/AI/HOLPA/HOLPA_data/Kenya/kenya_data_clean/" #path andrea

ken_h_survey_file <- paste0(ken_data_path, "ken_holpa_household_survey_clean.xlsx")
ken_h_choices_file <- paste0(ken_data_path, "ken_holpa_household_form_clean.xlsx")


ken_survey_main <- read_and_process_survey_xlsx("Holpa_global_household_surve", "_id", ken_h_survey_file,"kenya","_index")%>%
  #Remove respondents that did not wanted to complete the survey
  filter(consent_2!="No")%>%
  rename()%>%
  rename("x_2_1_1_7_001" = "_2_1_1_7_001",
    "x_2_1_1_7_002" = "_2_1_1_7_002")%>%
  mutate(across(c("x_2_1_1_7_001", "x_2_1_1_7_002"), as.numeric))%>%
  mutate(x_2_1_1_7= x_2_1_1_7_001+ x_2_1_1_7_002)%>%
  rename("_2_1_1_7"="x_2_1_1_7")

# Section: Farm production OTHER #Kenya doesn't have this section
ken_survey_3_3_3_2_begin_repeat<- read_and_process_survey_xlsx("_3_3_3_2_begin_repeat", "_submission__id", ken_h_survey_file,"kenya","_index") # Section: area of land per agricultural practice

ken_choices <- read_excel(ken_h_choices_file, sheet = "choices")%>%
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
#INSTRUCTION: Replace sen_data_path path with your own path, run the code and then go #### AGROECOLOGY MODULE ####
sen_data_path <- "C:/Users/andreasanchez/OneDrive - CGIAR/Bioversity/AI/HOLPA/HOLPA_data/Senegal/senegal_data_clean/" #path andrea

sen_h_survey_file <- paste0(sen_data_path, "sen_holpa_household_survey_clean.xlsx")
sen_h_choices_file <- paste0(sen_data_path, "sen_holpa_household_form_clean.xlsx")

sen_survey_main <- read_and_process_survey_xlsx("HOLPA Senegal_version finale", "_id", sen_h_survey_file,"senegal","_index")%>%
  #Remove respondents that did not wanted to complete the survey
  filter(consent_2!="No")%>%
  slice(-1)

sen_survey_1_4_2_7_begin_repeat <- read_and_process_survey_xlsx("_1_4_2_7_begin_repeat", "_submission__id", sen_h_survey_file,"senegal","_index")%>% # Section: Crop production
  slice(-1)
sen_survey_3_3_3_2_begin_repeat<- read_and_process_survey_xlsx("_3_3_3_2_begin_repeat", "_submission__id", sen_h_survey_file,"senegal","_index")%>% # Section: area of land per agricultural practice
  slice(-1)

sen_survey_3_4_3_1_1_begin_repeat<- read_and_process_survey_xlsx("_3_4_3_1_1_begin_repeat", "_submission__id", sen_h_survey_file,"senegal","_index") # Section: Crop list

sen_choices <- read_excel(sen_h_choices_file, sheet = "choices")%>%
  mutate(country= "senegal")%>%
  select("list_name","name","label::English ((en))","country")%>%
  rename("label_choice" = "label::English ((en))")%>%
  rename("name_choice" = "name")%>%
  distinct(list_name,name_choice,label_choice, .keep_all = TRUE)

#Add country choices to global choices
sen_global_choices<-global_choices%>%
  rbind(sen_choices)%>%
  arrange(desc(country == "global")) %>%
  #Removing duplicates
  distinct(list_name,name_choice, .keep_all = TRUE) %>%
  right_join(global_survey,by="list_name",relationship="many-to-many")%>%
  left_join(sen_choices,by=c("list_name","name_choice"))%>%
  rename("label_choice"="label_choice.x",
         "label_choice.country"="label_choice.y",
         "country"="country.x")%>%
  select(-country.y)


#### AGROECOLOGY MODULE #### -----
#INSTRUCTION: Continue running the code from here

fun_agroecology_choices<- function(country_global_choices) {
  # Filter and mutate the data frame
  agroecology_choices <- country_global_choices %>%
    filter(str_detect(module, "agroecology")) %>%
    mutate(module = "agroecology") 
  
  #"3_soil_health/6_synergy/environmental" this question is part of two indicators
  duplicate_rows <-  agroecology_choices%>% filter(str_detect(indicator, "3_soil_health/6_synergy"))
  duplicate_rows$indicator <- "6_synergy"
  agroecology_choices <- rbind(agroecology_choices, duplicate_rows)
  
  #"3_soil_health/6_synergy/environmental" this question is part of two indicators
  duplicate_rows <-  agroecology_choices%>% filter(str_detect(indicator, "4_animal_health/6_synergy"))
  duplicate_rows$indicator <- "6_synergy"
  agroecology_choices <- rbind(agroecology_choices, duplicate_rows)
  
  agroecology_choices$indicator[str_detect(agroecology_choices$indicator, "1_recycling")]<- "1_recycling"
  agroecology_choices$indicator[str_detect(agroecology_choices$indicator, "2_input_reduction")]<- "2_input_reduction"
  agroecology_choices$indicator[str_detect(agroecology_choices$indicator, "3_soil_health")]<- "3_soil_health"
  agroecology_choices$indicator[str_detect(agroecology_choices$indicator, "4_animal_health")]<- "4_animal_health"
  agroecology_choices$indicator[str_detect(agroecology_choices$indicator, "5_biodiversity")]<- "5_biodiversity"
  agroecology_choices$indicator[str_detect(agroecology_choices$indicator, "6_synergy")]<- "6_synergy"
  agroecology_choices$indicator[str_detect(agroecology_choices$indicator, "7_economic_diversification")]<- "7_economic_diversification"
  agroecology_choices$indicator[str_detect(agroecology_choices$indicator, "8_knowledge")]<- "8_knowledge"
  agroecology_choices$indicator[str_detect(agroecology_choices$indicator, "9_social_values")]<- "9_social_values"
  agroecology_choices$indicator[str_detect(agroecology_choices$indicator, "10_fairness")]<- "10_fairness"
  agroecology_choices$indicator[str_detect(agroecology_choices$indicator, "11_connectivity")]<- "11_connectivity"
  agroecology_choices$indicator[str_detect(agroecology_choices$indicator, "12_governance")]<- "12_governance"
  agroecology_choices$indicator[str_detect(agroecology_choices$indicator, "13_participation")]<- "13_participation"
  
  agroecology_choices <- agroecology_choices %>%
    mutate(subindicator=indicator)%>%
    rename(theme = indicator,
          indicator = subindicator)%>%
    mutate(name_question_choice= if_else(type_question=="select_multiple",
                                         paste(name_question,"/",name_choice, sep=""),
                                         name_question))
  return(agroecology_choices)
 }


fun_agroecology_questions_columns<- function(country_agroecology_choices) {
  agroecology_questions_columns<- country_agroecology_choices%>% 
    dplyr::select(label_question, name_question_choice)%>%
    dplyr::distinct(name_question_choice, .keep_all = TRUE)%>%
    spread(key = name_question_choice, value = label_question)%>%
    mutate("kobo_farmer_id"="kobo_farmer_id",
           "country"="country_name",
           "sheet_id"="sheet_id",
           "parent_table_name"="_parent_table_name",
           "index"="index",
           "parent_index"="_parent_index")
  
  agroecology_questions_columns <- colnames(agroecology_questions_columns)
  
  return(agroecology_questions_columns)
  
}

fun_agroecology_left_join <- function(agroecology_choices, gathered_data ) {
  
  # Left join for "calculate" and "integer"
  continuous <- gathered_data  %>%
    dplyr::left_join(select(agroecology_choices,
                            c(name_question, module, theme, indicator,"name_choice", label_choice, label_question,type, type_question, list_name)), 
                     by = "name_question")%>%
    filter(type_question =="calculate"|type_question =="integer"|type_question =="note"|
             type_question =="text"|type_question =="audio"|type_question =="decimal")%>%
    select(-name_choice.y)%>%
    rename("name_choice"="name_choice.x")
  
  # Left join for "select_multiple"
  select_multiple <- gathered_data  %>%
    left_join(select(agroecology_choices,
                     c(name_question, name_choice, module, theme, indicator, label_choice, label_question, type, type_question, list_name,name_question_choice)), 
              by = c("name_question"="name_question_choice"))%>%
    filter(type_question=="select_multiple")%>%
    select(-name_choice.y,-name_question.y)%>%
    rename("name_choice"="name_choice.x")%>%
    #Remove answers == "0" or NA
    filter(type_question == "select_multiple" & !is.na(name_choice))
  
  # Left join for "select_one" for country== "zwe"(Zimbabwe downloaded the database with label_name)
  select_one1 <- gathered_data  %>%
    left_join(select(agroecology_choices,
                     c(name_question, name_choice, module, theme, indicator, label_choice, label_question, type, type_question, list_name)), 
              by = c("name_question"="name_question", "name_choice"="name_choice"))%>%
    filter(type_question=="select_one")%>%
    filter(country=="zimbabwe"|
             country=="kenya")
  
  # Left join for "select_one" for country== "tun" (Tunisia downloaded the database with label_choices)
  select_one2 <- gathered_data  %>%
    left_join(select(agroecology_choices,
                     c(name_question, name_choice, module, theme, indicator, label_choice, label_question, type, type_question, list_name)), 
              by = c("name_question"="name_question", "name_choice"="label_choice"))%>%
    dplyr::rename("label_choice"="name_choice")%>%
    dplyr::rename("name_choice"="name_choice.y")%>%
    filter(type_question=="select_one")%>%
    filter(country=="tunisia")
  
  # Left join for "select_one" for countries that downloaded the survey with the name_label version in country language (country== "sen")
  select_one3 <- gathered_data  %>%
    left_join(select(agroecology_choices,
                     c(name_question, name_choice, module, theme, indicator, label_choice, label_question, type, type_question, list_name,label_choice.country)), 
              by = c("name_question"="name_question", "name_choice"="label_choice.country"))%>%
    select(-name_choice)%>%
    dplyr::rename("name_choice"="name_choice.y")%>%
    filter(type_question=="select_one")%>%
    filter(country== "senegal")
  
  
  result<- rbind(continuous,select_multiple,select_one1,select_one2,select_one3)
  
  
  return(result)
}

### Function to get answers from the following sections ---- # I can combine all this functions, but lets see
## Main survey ----
fun_agroecology_main<- function(country_global_choices,country_survey_main){
  country_agroecology_choices<-  fun_agroecology_choices(country_global_choices)
  country_agroecology_question_columns<- fun_agroecology_questions_columns(country_agroecology_choices)
  
  country_agroecology_columns <- intersect(country_agroecology_question_columns, colnames(country_survey_main))
  mismatched_columns <- setdiff(country_agroecology_question_columns, country_agroecology_columns)
  
  country_agroecology <- country_survey_main %>%
    select(all_of(country_agroecology_columns))%>%
    mutate_all(as.character)
  
  # Identify columns with only NA values
  na_columns <- colSums(is.na(country_agroecology)) == nrow(country_agroecology)

  # Remove columns with only NA values
  country_agroecology <- country_agroecology[, !na_columns]
  
  result_main_survey <- country_agroecology%>%
    gather(key = "name_question", value = "name_choice", -kobo_farmer_id, -country,-sheet_id,-index)%>%
    fun_agroecology_left_join(country_agroecology_choices,.)%>%
    mutate(name_question_recla= name_question)%>%
    mutate(parent_table_name= NA,
           parent_index=NA)
  return(result_main_survey)
}

## begin_repeat ----
fun_agroecology_begin_repeat<- function(country_global_choices,country_survey_begin_repeat){
  
  country_agroecology_choices<-  fun_agroecology_choices(country_global_choices)
  country_agroecology_question_columns<- fun_agroecology_questions_columns(country_agroecology_choices)
  country_agroecology_columns_repeat <- intersect(country_agroecology_question_columns, colnames(country_survey_begin_repeat))

  country_agroecology_begin_repeat <- country_survey_begin_repeat %>%
    select(all_of(country_agroecology_columns_repeat))%>%
    mutate_all(as.character)
  
  # Identify columns with only NA values
  na_columns_begin_repeat <- colSums(is.na(country_agroecology_begin_repeat)) == nrow(country_agroecology_begin_repeat)
  
  # Remove columns with only NA values
  country_agroecology_begin_repeat <- country_agroecology_begin_repeat[, !na_columns_begin_repeat]
  
  result_1_4_2_7_begin_repeat <- country_agroecology_begin_repeat%>%
    gather(key = "name_question", value = "name_choice", -kobo_farmer_id, -country,-sheet_id,-index,-parent_table_name,-parent_index)%>%
    fun_agroecology_left_join(country_agroecology_choices,.)%>%
    mutate(name_question_recla= name_question)
  return(result_1_4_2_7_begin_repeat)
}


### Function to combine answers from all agroecology sections ---- 
fun_agroecology_data<- function(
    country_global_choices,
    country_survey_main, #main survey
    country_survey_1_4_2_7_begin_repeat,  ## Other on-farm product Farm characteristics
    country_survey_3_3_3_2_begin_repeat ## area of land per agricultural practice
    ) {
  agroecology_data<- rbind(
    fun_agroecology_main(country_global_choices, country_survey_main), ## Main survey
    fun_agroecology_begin_repeat(country_global_choices, country_survey_1_4_2_7_begin_repeat),  ## Other on-farm product Farm characteristics ----
    fun_agroecology_begin_repeat(country_global_choices, country_survey_3_3_3_2_begin_repeat) ## area of land per agricultural practice
    )
  return(agroecology_data)
}

fun_agroecology<- function(country_agroecology_data){
  country_agroecology<-country_agroecology_data%>%
  #Indicator: 5_biodiversity
  mutate(name_question_recla  = case_when(
    name_question %in% c("c1", "c2", "c3", "c4", "c5", "c6", "c7", "c8","c9", "c10", "c11", "c12", "c13", "c14", "c15", "c16", "c17", "c18", "c19", "c20")~"_3_4_3_1_1_2",
    name_question %in% c("l1", "l2", "l3", "l4", "l5", "l6", "l7", "l8","l9","l10") ~ "_3_4_3_3_1",
    name_question %in% c("f1", "f2", "f3", "f4", "f5", "f6", "f7", "f8", "f9", "f10") ~ "_3_4_3_4_2",
    TRUE ~ name_question_recla))%>%
    
    #For the countries that translated the name of the crops, livestock and fish, agricultural to English separated with "//"
    mutate(name_choice= case_when(
      name_question_recla %in%c("_3_4_3_1_1_2", "_3_4_3_4_2","_3_3_3_1_calculate_2") & grepl("//", name_choice)~ sub(".*//", "", name_choice),
      TRUE ~ name_choice))%>%
    
    mutate(label_choice= case_when(
      name_question_recla %in% c("_3_4_3_1_1_2","_3_4_3_3_1")~ name_choice,
      TRUE ~label_choice))%>%
    filter(name_question!="_3_4_3_3_1/other")%>%
    
    #Indicator: 6_synergy
    mutate(label_choice= case_when(
      name_question_recla %in% c("_3_3_3_1_calculate_2")~ name_choice,
      TRUE ~label_choice))%>%
    # Indicator: all principles
    mutate(name_question_recla = case_when(
      type_question == "select_multiple"~str_replace(name_question_recla, "/.*", ""),
      TRUE ~ name_question_recla))%>%
    
    mutate(name_choice = case_when(
      type_question == "select_multiple"&name_choice=="0"~ NA,
      type_question == "select_multiple"& name_choice == "1" ~ sub("^.*/", "", name_question), # replace name_question by the type of energy
      TRUE ~ name_choice))%>%
    # Remove rows name_choice == NA
    filter(!is.na(name_choice))%>%
    mutate(label_choice= case_when(
      name_question %in% c("_1_4_1_1_1", "_1_4_1_1_2", "_1_4_1_1_3","_3_4_2_1_1","_3_4_2_2_1_1","_3_4_2_2_1_2","_3_4_2_3_2")& country== "kenya" & kobo_farmer_id == "286844609"~"hectares",
      name_question%in% c("_1_4_1_1_1", "_1_4_1_1_2", "_1_4_1_1_3","_3_4_2_1_1","_3_4_2_2_1_1","_3_4_2_2_1_2","_3_4_2_3_2")& country== "senegal" & kobo_farmer_id == "308802823"~"metres square",
      name_question%in% c("_1_4_1_1_1", "_1_4_1_1_2", "_1_4_1_1_3","_3_4_2_1_1","_3_4_2_2_1_1","_3_4_2_2_1_2","_3_4_2_3_2")& country %in%c("zimbabwe","kenya")~"acres",
      name_question%in% c("_1_4_1_1_1", "_1_4_1_1_2", "_1_4_1_1_3","_3_4_2_1_1","_3_4_2_2_1_1","_3_4_2_2_1_2","_3_4_2_3_2")& country %in% c("tunisia","senegal")~"hectares",
      TRUE ~ label_choice))
  

return(country_agroecology)
}
  
## AGROECOLOGY DATA BY COUNTRY -----
## If the farmers doesn't know the answer put 9999-----
# INTRUCTION: Go to your country section

# ZIMBABWE -----
zwe_agroecology_data<-fun_agroecology_data(zwe_global_choices,
                                           zwe_survey_main,  ## Main survey 
                                           zwe_survey_1_4_2_7_begin_repeat, ## _1_4_2_7_begin_repeat: Other on-farm product Farm characteristics 
                                           zwe_survey_3_3_3_2_begin_repeat) # Section: area of land per agricultural practice
                                           
zwe_agroecology<-fun_agroecology(zwe_agroecology_data) 
write.csv(zwe_agroecology,paste0(zwe_data_path,"/zwe/zwe_agroecology_format.csv"),row.names=FALSE)

# TUNISIA-----
## Tunisia doesn't have this section _1_4_2_7_begin_repeat: Other on-farm product Farm characteristics
tun_agroecology_data<-rbind(
  fun_agroecology_main(tun_global_choices, tun_survey_main), ## Main survey 
  fun_agroecology_begin_repeat(tun_global_choices, tun_survey_3_3_3_2_begin_repeat)) # Section: area of land per agricultural practice
  
tun_agroecology<-fun_agroecology(tun_agroecology_data) 
write.csv(tun_agroecology,paste0(tun_data_path,"/tun/tun_agroecology_format.csv"),row.names=FALSE)

# KENYA -----
ken_agroecology_data<-rbind(
    fun_agroecology_main(ken_global_choices, ken_survey_main), ## Main survey 
    fun_agroecology_begin_repeat(ken_global_choices, ken_survey_3_3_3_2_begin_repeat)) # Section: area of land per agricultural practice

ken_agroecology<-fun_agroecology(ken_agroecology_data) 
write.csv(ken_agroecology,paste0(ken_data_path,"/ken/ken_agroecology_format.csv"),row.names=FALSE)

# SENEGAL -----
sen_agroecology_data<-fun_agroecology_data(sen_global_choices,
                                             sen_survey_main,  ## Main survey 
                                             sen_survey_1_4_2_7_begin_repeat, ## _1_4_2_7_begin_repeat: Other on-farm product Farm characteristics 
                                             sen_survey_3_3_3_2_begin_repeat) %>% # Section: area of land per agricultural practice
  #Remove the list of crops from the main survey, because the complete list of crops for Senegal is in sen_survey_3_4_3_1_1_begin_repeat
  filter(!name_question %in% c('c1', 'c2', 'c3', 'c4', 'c5', 'c6', 'c7', 'c8', 'c9', 'c10','c11', 'c12', 'c13', 'c14', 'c15', 'c16', 'c17', 'c18', 'c19', 'c20'))%>%
  rbind(fun_agroecology_begin_repeat(sen_global_choices,sen_survey_3_4_3_1_1_begin_repeat)) ##_3_4_2_3_2_repeat_group:  Crop list 

sen_agroecology<-fun_agroecology(sen_agroecology_data) 

write.csv(sen_agroecology,paste0(sen_data_path,"/sen/sen_agroecology_format.csv"),row.names=FALSE)
