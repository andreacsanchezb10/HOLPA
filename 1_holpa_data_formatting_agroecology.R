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

#Andrea 
global.data.path <-"C:/Users/andreasanchez/OneDrive - CGIAR/Bioversity/AI/HOLPA/analysis/HOLPA/HOLPA/"
zwe.data.path <-"C:/Users/andreasanchez/OneDrive - CGIAR/Bioversity/AI/HOLPA/HOLPA_data/Zimbabwe/zimbabwe_data_clean/household_database_2024.04.18_clean.xlsx"

#### Import data ####
# Each dataset contains a survey worksheet with the questions and responses for text, open and numeric questions, and
# a choices worksheet with the response options for multiple choice questions (single or multiple).
# These need to be imported and combined.

### Country databases ####
read_and_process_survey <- function(sheet_name, column_id_rename, data_path, country_name, index) {
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

#Zimbabwe
zwe_survey <- read_and_process_survey("Final HOLPA_Zimbabwe_Household", "_id", zwe.data.path,"zimbabwe","_index")%>%
  #Remove respondents that are not farmers
  filter(kobo_farmer_id!="274186917")
zwe_survey_1_4_2_7_begin_repeat <- read_and_process_survey("_1_4_2_7_begin_repeat", "_submission__id", zwe.data.path,"zimbabwe","_index")%>% # Section: Farm production OTHER
  #Remove respondents that are not farmers
  filter(kobo_farmer_id!="274186917")

zwe_choices <- read_excel("C:/Users/andreasanchez/OneDrive - CGIAR/Bioversity/AI/HOLPA/HOLPA_data/Zimbabwe/Zimbabwe_monitoring/holpa_household_form.xlsx",
                             sheet = "choices")%>%
  mutate(country= "Zimbabwe")%>%
  select("list_name","name","label::English ((en))","country")%>%
  rename("label_choice" = "label::English ((en))")%>%
  rename("name_choice" = "name")%>%
  distinct(list_name,name_choice,label_choice, .keep_all = TRUE)

#### Global databases ####
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
  rename("name_choice" = "name")%>%
  #Rbind country specific choices 
  rbind(zwe_choices)%>%
  arrange(desc(country == "global")) %>%
  #Removing duplicates
  distinct(list_name,name_choice, .keep_all = TRUE) %>%
  right_join(global_survey,by="list_name",relationship="many-to-many")

#### Agroecology module ####
agroecology_survey <-  global_survey %>%
  filter(str_detect(module, "agroecology"))%>%
  mutate(module= "agroecology")%>%
  mutate(subindicator=indicator)

#"3_soil_health/6_synergy/environmental" this question is part of two indicators
duplicate_rows <-  agroecology_survey%>% filter(str_detect(subindicator, "3_soil_health/6_synergy/environmental"))
duplicate_rows$indicator <- "6_synergy"
agroecology_survey <- rbind(agroecology_survey, duplicate_rows)

#"3_soil_health/6_synergy/environmental" this question is part of two indicators
duplicate_rows <-  agroecology_survey%>% filter(str_detect(subindicator, "4_animal_health/6_synergy"))
duplicate_rows$indicator <- "6_synergy"
agroecology_survey <- rbind(agroecology_survey, duplicate_rows)

sort(unique(agroecology_survey$indicator))
agroecology_survey$indicator[str_detect(agroecology_survey$indicator, "1_recycling")]<- "1_recycling"
agroecology_survey$indicator[str_detect(agroecology_survey$indicator, "2_input_reduction")]<- "2_input_reduction"
agroecology_survey$indicator[str_detect(agroecology_survey$indicator, "3_soil_health")]<- "3_soil_health"
agroecology_survey$indicator[str_detect(agroecology_survey$indicator, "4_animal_health")]<- "4_animal_health"
agroecology_survey$indicator[str_detect(agroecology_survey$indicator, "5_biodiversity")]<- "5_biodiversity"
agroecology_survey$indicator[str_detect(agroecology_survey$indicator, "6_synergy")]<- "6_synergy"
agroecology_survey$indicator[str_detect(agroecology_survey$indicator, "7_economic_diversification")]<- "7_economic_diversification"
agroecology_survey$indicator[str_detect(agroecology_survey$indicator, "8_knowledge")]<- "8_knowledge"
agroecology_survey$indicator[str_detect(agroecology_survey$indicator, "9_social_values")]<- "9_social_values"
agroecology_survey$indicator[str_detect(agroecology_survey$indicator, "10_fairness")]<- "10_fairness"
agroecology_survey$indicator[str_detect(agroecology_survey$indicator, "11_connectivity")]<- "11_connectivity"
agroecology_survey$indicator[str_detect(agroecology_survey$indicator, "12_governance")]<- "12_governance"
agroecology_survey$indicator[str_detect(agroecology_survey$indicator, "13_participation")]<- "13_participation"
sort(unique(agroecology_survey$indicator))

agroecology_choices <- global_choices %>%
  filter(str_detect(module, "agroecology"))%>%
  mutate(module= "agroecology") %>% 
  mutate(subindicator=indicator)%>%
  mutate(name_question_choice= if_else(type_question=="select_multiple",
                                       paste(name_question,"/",name_choice, sep=""),
                                       name_question))

#"3_soil_health/6_synergy/environmental" this question is part of two indicators
duplicate_rows <-  agroecology_choices%>% filter(str_detect(subindicator, "3_soil_health/6_synergy/environmental"))
duplicate_rows$indicator <- "6_synergy"
agroecology_choices <- rbind(agroecology_choices, duplicate_rows)
  
#"3_soil_health/6_synergy/environmental" this question is part of two indicators
duplicate_rows <-  agroecology_choices%>% filter(str_detect(subindicator, "4_animal_health/6_synergy"))
duplicate_rows$indicator <- "6_synergy"
agroecology_choices <- rbind(agroecology_choices, duplicate_rows)


sort(unique(agroecology_choices$indicator))
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
sort(unique(agroecology_choices$indicator))

agroecology_choices<- agroecology_choices%>%
  mutate(theme= indicator)

## PLEASE DO NOT MODIFY THE CODE FROM HERE. (IMPROVEMENTS IN PROGRESS)
# IF YOU FIND AN ERROR, IS NOT THE CODE, IS THE DATABASE!!
sort(unique(agroecology_choices$indicator))

agroecology_questions_columns<- agroecology_choices%>% 
   filter(
     indicator=="5_biodiversity"
       #"6_synergy"
       #"13_participation" 
     #"12_governance"
   #"11_connectivity"
   #"10_fairness"
   #"9_social_values"
     #"8_knowledge"
   #"7_economic_diversification"
     #"4_animal_health"
   #"3_soil_health"
     #"2_input_reduction"
   #"1_recycling"
    )%>%  
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
agroecology_questions_columns

# Zimbabwe 
colnames(zwe_survey)
zwe_agroecology_columns <- intersect(agroecology_questions_columns, colnames(zwe_survey))
zwe_agroecology_columns
agroecology_questions_columns
mismatched_columns <- setdiff(agroecology_questions_columns, zwe_agroecology_columns)
print(mismatched_columns)

agroecology_left_join <- function(agroecology_choices, gathered_data ) {
  
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
    filter(type_question == "select_multiple" & !is.na(name_choice) & name_choice != 0)
  
  # Left join for "select_one"
  select_one <- gathered_data  %>%
    left_join(select(agroecology_choices,
                     c(name_question, name_choice, module, theme, indicator, label_choice, label_question, type, type_question, list_name)), 
              by = c("name_question"="name_question", "name_choice"="name_choice"))%>%
    filter(type_question=="select_one")
  
  result<- rbind(continuous,select_multiple,select_one)
  
  
  return(result)
}

# Final HOLPA_Zimbabwe_Household
zwe_agroecology <- zwe_survey %>%
  select(all_of(zwe_agroecology_columns))%>%
  mutate_all(as.character)

view(dfSummary(zwe_agroecology))

# Identify columns with only NA values
na_columns <- colSums(is.na(zwe_agroecology)) == nrow(zwe_agroecology)
na_columns

# Remove columns with only NA values
zwe_agroecology <- zwe_agroecology[, !na_columns]

view(dfSummary(zwe_agroecology))

result_zwe_agroecology <- zwe_agroecology%>%
  gather(key = "name_question", value = "name_choice", -kobo_farmer_id, -country,-sheet_id,-index)%>%
  agroecology_left_join(agroecology_choices,.)%>%
  mutate(name_question_recla= name_question)%>%
  mutate(parent_table_name= NA,
         parent_index=NA)
names(result_zwe_agroecology)

## _1_4_2_7_begin_repeat: Other on-farm product Farm characteristics ----
zwe_agroecology_columns_1_4_2_7_begin_repeat <- intersect(agroecology_questions_columns, colnames(zwe_survey_1_4_2_7_begin_repeat))
zwe_agroecology_columns_1_4_2_7_begin_repeat
agroecology_questions_columns
mismatched_columns_1_4_2_7_begin_repeat <- setdiff(agroecology_questions_columns, zwe_agroecology_columns_1_4_2_7_begin_repeat)
print(mismatched_columns_1_4_2_7_begin_repeat)

zwe_agroecology_1_4_2_7_begin_repeat <- zwe_survey_1_4_2_7_begin_repeat %>%
  select(all_of(zwe_agroecology_columns_1_4_2_7_begin_repeat))%>%
  mutate_all(as.character)

view(dfSummary(zwe_agroecology_1_4_2_7_begin_repeat))

# Identify columns with only NA values
na_columns_1_4_2_7_begin_repeat <- colSums(is.na(zwe_agroecology_1_4_2_7_begin_repeat)) == nrow(zwe_agroecology_1_4_2_7_begin_repeat)
na_columns_1_4_2_7_begin_repeat

# Remove columns with only NA values
zwe_agroecology_1_4_2_7_begin_repeat <- zwe_agroecology_1_4_2_7_begin_repeat[, !na_columns_1_4_2_7_begin_repeat]

view(dfSummary(zwe_agroecology_1_4_2_7_begin_repeat))

result_1_4_2_7_begin_repeat <- zwe_agroecology_1_4_2_7_begin_repeat%>%
  gather(key = "name_question", value = "name_choice", -kobo_farmer_id, -country,-sheet_id,-index,-parent_table_name,-parent_index)%>%
  agroecology_left_join(agroecology_choices,.)%>%
  mutate(name_question_recla= name_question)

# Combine all----
zwe_agroecology_all<- #rbind(
  result_zwe_agroecology%>%#,
                            # Indicator: "11_connectivity"
                     #       result_1_4_2_7_begin_repeat)%>%
  # Indicator: "1_recycling"
  mutate(name_choice = case_when(
    name_question %in% c("_2_8_2_1","_2_8_3_1") &is.na(name_choice)  ~ "0",
    TRUE ~ name_choice))%>%
    # Indicator: "2_input_reduction"
  mutate(name_question_recla = case_when(
    str_detect(name_question_recla,"_1_4_3_1/")~str_replace(name_question_recla, "/.*",""),
    str_detect(name_question_recla,"_1_4_3_5/")~str_replace(name_question_recla, "/.*", ""),
    str_detect(name_question_recla,"_1_4_3_8/")~str_replace(name_question_recla, "/.*", ""),
    TRUE ~ name_question_recla))%>%
    # Indicator: "3_soil_health"
  mutate(name_question_recla = case_when(
    str_detect(name_question_recla,"_2_9_1_1/")~str_replace(name_question_recla, "/.*", ""),
    TRUE ~ name_question_recla))%>%
    # Indicator: "4_animal_health"
    mutate(name_question_recla = case_when(
    str_detect(name_question_recla,"_2_10_1_2/")~str_replace(name_question_recla, "/.*", ""),
    TRUE ~ name_question_recla))%>%
  #Indicator: 5_biodiversity
  mutate(name_question_recla = case_when(
    str_detect(name_question_recla,"_3_4_3_3_1/")~str_replace(name_question_recla, "/.*", ""),
    TRUE ~ name_question_recla))%>%
  mutate(name_choice = case_when(
    str_detect(name_question,"_3_4_3_3_1/")~str_extract(name_question, "(?<=/).*"),
    TRUE ~ name_choice))%>%
  mutate(name_question_recla  = case_when(
    name_question %in% c("c1", "c2", "c3", "c4", "c5", "c6", "c7", "c8","c9", "c10", "c11", "c12", "c13", "c14", "c15", "c16", "c17", "c18", "c19", "c20")~"_3_4_3_1_1_2",
    name_question %in% c("l1", "l2", "l3", "l4", "l5", "l6", "l7", "l8") ~ "_3_4_3_3_1",
    TRUE ~ name_question_recla))%>%
  filter(name_question!="_3_4_3_3_1/other")%>%
    #Indicator: "6_synergy"
  mutate(name_question_recla = case_when(
    str_detect(name_question_recla,"_2_12_1/")~str_replace(name_question_recla, "/.*", ""),
    str_detect(name_question_recla,"_3_3_1_7/")~str_replace(name_question_recla, "/.*", ""),
    str_detect(name_question_recla,"_3_3_3_1/")~str_replace(name_question_recla, "/.*", ""),
    str_detect(name_question_recla,"_3_3_3_3/")~str_replace(name_question_recla, "/.*", ""),
    TRUE ~ name_question_recla))%>%
    # Indicator: "7_economic_diversification"
  mutate(name_question_recla = case_when(
    str_detect(name_question_recla,"_2_4_1/")~str_replace(name_question_recla, "/.*", ""),
    TRUE ~ name_question_recla))%>%
    # Indicator: "11_connectivity"
  mutate(name_question_recla = case_when(
    str_detect(name_question_recla,"_2_7_1_1/")~str_replace(name_question_recla, "/.*", ""),
    str_detect(name_question_recla,"_2_7_1_2/")~str_replace(name_question_recla, "/.*", ""),
    str_detect(name_question_recla,"_2_7_1_5/")~str_replace(name_question_recla, "/.*", ""),
    str_detect(name_question_recla,"_2_7_1_6/")~str_replace(name_question_recla, "/.*", ""),
    TRUE ~ name_question_recla))

  
sort(unique(zwe_agroecology_all$indicator))
sort(unique(zwe_agroecology_all$name_question_recla))

sort(unique(zwe_agroecology_all$label_question))
sort(unique(zwe_agroecology_all$name_question))

table(zwe_agroecology_all$label_question,zwe_agroecology_all$name_question_recla)
unique(zwe_agroecology_all$sheet_id)
names(zwe_agroecology_all)
view(dfSummary(zwe_agroecology_all))


#--------------------
#library(openxlsx)
library(tidyr)
library(tidyverse)
library(readxl)
library(dplyr)

##Country databases
# Completed surveys
zimbabwe_survey <- read_excel("zimbabwe_household_name_2023.11.27.xlsx",
                              sheet = "Final HOLPA_Zimbabwe_Household") %>%
  rename("kobo_id"="_id")%>%
  rename("country"="_1_2_1_3")%>%
  mutate("_2_6_1_4_6"= NA)%>%
  mutate("_2_7_1_6/other"= NA)


# Country forms
zimbabwe_choices <- read_excel("zimbabwe_household_form.xlsx",sheet = "choices")%>%
  select("list_name","name","label::English ((en))","score_agroecology_module")%>%
  mutate(country="zimbabwe")%>%
  rename("label_choice" = "label::English ((en))")%>%
  rename("name_choice" = "name")

names(zimbabwe_choices)
#### Global databases
global_form <- read_excel("HOLPA_global_household_survey_20231204_mapped_to_indicators.xlsx",
                          sheet = "survey")%>%
  #select only the necessary columns
  select("module","indicator", "subindicator", 
         "type", "name","label::English ((en))")%>%
  #rename columns names
  rename("label_question" = "label::English ((en))")%>%
  rename("name_question" = "name")%>%
  #remove rows without questions
  filter(type!="begin_group")%>%
  filter(type!="end_group")%>%
  #separate question type components
  mutate(type_question = ifelse(substr(type,1,10)=="select_one","select_one",
                                ifelse(substr(type,1,10)=="select_mul","select_multiple",type)))%>%
  mutate(list_name = if_else(type_question== "select_one"|type_question== "select_multiple", 
                             str_replace(.$type, paste0(".*", .$type_question), ""),NA))%>%
  mutate(list_name = str_replace_all(list_name, " ", ""))


global_choices <- read_excel("HOLPA_global_household_survey_20231204_mapped_to_indicators.xlsx",
                             sheet = "choices")%>%
  select("list_name","name","label::English ((en))","score_agroecology_module","country")%>%
  rename("label_choice" = "label::English ((en))")%>%
  rename("name_choice" = "name")%>%
  #Rbind country specific choices 
  distinct(list_name,name_choice,label_choice, .keep_all = TRUE)%>%
  right_join(global_form,by="list_name")

######Agroecology module
agroecology_form<-  global_form%>%
  filter(str_detect(module, "agroecology"))%>%
  select(-subindicator)%>%
  mutate(module= "agroecology")%>%
  mutate(indicator=if_else(str_detect(indicator, "1_recycling"),"1_recycling",
                           if_else(str_detect(indicator, "2_input_reduction"),"2_input_reduction",
                                   if_else(str_detect(indicator, "3_soil_health"), "3_soil_health",
                                           if_else(str_detect(indicator, "4_animal_health"), "4_animal_health",
                                                   if_else(str_detect(indicator, "5_biodiversity"), "5_biodiversity",
                                                           if_else(str_detect(indicator, "6_synergy"), "6_synergy",
                                                                   if_else(str_detect(indicator, "7_economic_diversification"), "7_economic_diversification",
                                                                           if_else(str_detect(indicator, "8_knowledge"), "8_knowledge",
                                                                                   if_else(str_detect(indicator,"9_social_values"),"9_social_values",
                                                                                           if_else(str_detect(indicator,"10_fairness"),"10_fairness",
                                                                                                   if_else(str_detect(indicator,"11_connectivity"),"11_connectivity",
                                                                                                           if_else(str_detect(indicator, "12_governance"), "12_governance",
                                                                                                                   if_else(str_detect(indicator, "13_participation"), "13_participation",
                                                                                                                           indicator))))))))))))))%>%
  mutate(type_question = case_when(
    name_question %in% c(
      "_2_9_1_1","_2_4_1","_2_10_1_2",
      #6_synergy                   
      "_2_9_1_1","_3_3_1_7","_3_3_3_3","_3_3_3_4","_2_12_1") ~ "count",
    TRUE ~ type_question))

agroecology_options <- global_choices%>%
  filter(str_detect(module, "agroecology"))%>%
  select(-subindicator)%>%
  #rbind(read_excel("HOLPA_global_household_survey_20231204_mapped_to_indicators.xlsx",sheet = "agroecology_look_up"))%>%
  mutate(module= "agroecology")%>%
  mutate(indicator=if_else(str_detect(indicator, "1_recycling"),"1_recycling",
                           if_else(str_detect(indicator, "2_input_reduction"),"2_input_reduction",
                                   if_else(str_detect(indicator, "3_soil_health"), "3_soil_health",
                                           if_else(str_detect(indicator, "4_animal_health"), "4_animal_health",
                                                   if_else(str_detect(indicator, "5_biodiversity"), "5_biodiversity",
                                                           if_else(str_detect(indicator, "6_synergy"), "6_synergy",
                                                                   
                                                                   if_else(str_detect(indicator, "7_economic_diversification"), "7_economic_diversification",
                                                                           if_else(str_detect(indicator, "8_knowledge"), "8_knowledge",
                                                                                   if_else(str_detect(indicator,"9_social_values"),"9_social_values",
                                                                                           if_else(str_detect(indicator,"10_fairness"),"10_fairness",
                                                                                                   if_else(str_detect(indicator,"11_connectivity"),"11_connectivity",
                                                                                                           if_else(str_detect(indicator, "12_governance"), "12_governance",
                                                                                                                   if_else(str_detect(indicator, "13_participation"), "13_participation",
                                                                                                                           
                                                                                                                           indicator))))))))))))))%>%
  mutate(name_question_choice= if_else(type_question=="select_multiple",
                                       paste(name_question,"/",name_choice, sep=""),
                                       name_question))%>%
  mutate(type_question = case_when(
    name_question %in% c(
      "_2_9_1_1","_2_4_1","_2_10_1_2",
      #6_synergy                   
      "_2_9_1_1","_3_3_1_7","_3_3_3_3","_3_3_3_4","_2_12_1") ~ "count",
    TRUE ~ type_question))


agrocology_choices<-agroecology_options%>%
  filter(type_question!="count")%>%
  select(-name_question_choice)%>%
  rbind(read_excel("HOLPA_global_household_survey_20231204_mapped_to_indicators.xlsx",sheet = "agroecology_look_up"))

names(agrocology_choices)

## Principles structure
recycling<- agroecology_options%>%
  filter(indicator== "1_recycling"|
           indicator=="2_input_reduction"|
           indicator== "3_soil_health"|
           indicator==  "4_animal_health"|
           indicator=="5_biodiversity"|
           indicator=="6_synergy"|
           
           indicator== "7_economic_diversification"|
           indicator=="8_knowledge"|
           indicator=="9_social_values"|
           indicator== "10_fairness"|
           indicator=="11_connectivity"|
           indicator== "12_governance"|
           indicator== "13_participation"
  )%>%
  select(label_question, name_question_choice)%>%
  dplyr::distinct(name_question_choice, .keep_all = TRUE)%>%
  spread(key = name_question_choice, value = label_question)%>%
  mutate("kobo_id"="kobo_farmer_id",
         "country"="country_name")


recycling_columns <- colnames(recycling)
recycling_columns


#Function to combine the answer from all select_multiple questions  
process_columns_regex <- function(data, prefixes) {
  for (prefix in prefixes) {
    cols_to_paste <- grep(paste0("^", prefix), names(data), value = TRUE)
    
    data <- data %>%
      mutate(across(all_of(cols_to_paste), as.character)) %>%
      mutate(!!prefix := do.call(paste, c(select(data, all_of(cols_to_paste)), sep = "_")))
  }
  return(data)
}

# Prefixes of select_multiple questions
select_multiple<- agroecology_form%>%
  filter(indicator == "1_recycling"|
           indicator == "2_input_reduction"|
           indicator== "3_soil_health"|
           indicator==  "4_animal_health"|
           indicator=="5_biodiversity"|
           indicator=="6_synergy"|
           indicator== "7_economic_diversification"|
           indicator=="8_knowledge"|
           indicator == "9_social_values"|
           indicator== "10_fairness"|
           indicator=="11_connectivity"|
           indicator== "12_governance"|
           indicator== "13_participation"
  )%>%
  filter(type_question == "select_multiple")%>%
  select(name_question,type_question)%>%
  #mutate(name_question= paste(name_question,"/",sep = ""))%>%
  spread(key = name_question, value = type_question)

select_multiple<- colnames(select_multiple)
select_multiple

#Function to count number of selected answers 
count_columns_regex <- function(data, prefixes) {
  for (prefix in prefixes) {
    cols_to_count <- grep(paste0("^", prefix), names(data), value = TRUE)
    
    data <- data %>%
      mutate(across(all_of(cols_to_count), as.numeric))%>% 
      mutate(!!prefix := rowSums(across(all_of(cols_to_count))))
  }
  return(data)
}

# Prefixes of count questions
count<- agroecology_form%>%
  filter(indicator == "1_recycling"|
           indicator == "2_input_reduction"|
           indicator== "3_soil_health"|
           indicator==  "4_animal_health"|
           indicator=="5_biodiversity"|
           indicator=="6_synergy"|
           indicator== "7_economic_diversification"|
           indicator=="8_knowledge"|
           indicator == "9_social_values"|
           indicator== "10_fairness"|
           indicator=="11_connectivity"|
           indicator== "12_governance"|
           indicator== "13_participation"
  )%>%
  filter(type_question == "count")%>%
  select(name_question,type_question)%>%
  mutate(name_question= paste(name_question,"/",sep = ""))%>%
  spread(key = name_question, value = type_question)

count<- colnames(count)
count

existing_columns <- intersect(recycling_columns, colnames(zimbabwe_survey))
existing_columns

names(agroecology_zimbabwe)

agroecology_zimbabwe <- zimbabwe_survey %>%
  select(all_of(existing_columns))%>%
  mutate_all(as.character)%>%
  process_columns_regex(select_multiple)%>%
  count_columns_regex(count) %>%
  rename(
    "x_2_9_1_1"="_2_9_1_1/",
    "x_2_4_1"="_2_4_1/",
    "x_2_10_1_2"="_2_10_1_2/",
    
    #5_biodiversity
    "x_3_4_2_2_2_1_calculate"="_3_4_2_2_2_1_calculate",
    "x_3_4_2_2_2_2_calculate" ="_3_4_2_2_2_2_calculate",
    "x_3_3_1_7"="_3_3_1_7/",
    "x_3_3_3_3"="_3_3_3_3/",
    "x_3_3_3_4"="_3_3_3_4/",
    "x_2_12_1"="_2_12_1/",
    "x_3_3_3_1xAgroforestry"="_3_3_3_1/Agroforestry",
    "x_3_3_3_1xFallow"="_3_3_3_1/Fallow",
    "x_3_3_3_1xHedgerowsxLive_fences"="_3_3_3_1/Hedgerows/Live_fences",
    "x_3_3_3_1xHomegarden"="_3_3_3_1/Homegarden",
    "x_3_3_3_1xNatural_stripsxvegetation"="_3_3_3_1/Natural_strips/vegetation",
    "x_3_3_3_1xPollinatorxFlower_strips"="_3_3_3_1/Pollinator/Flower_strips",
    "x_3_3_3_1xPushxpull"="_3_3_3_1/Push-pull",
    #10_fairness
    "x_2_6_1_4_1"="_2_6_1_4_1",
    "x_2_6_1_4_2"="_2_6_1_4_2",
    "x_2_6_1_4_3"="_2_6_1_4_3",
    "x_2_6_1_4_4"="_2_6_1_4_4",
    "x_2_6_1_4_5"="_2_6_1_4_5",
    "x_2_6_1_4_6"="_2_6_1_4_6",
    
    #11_connectity 
    "x_2_7_1_1"="_2_7_1_1",
    "x_1_4_2_1xCrops" ="_1_4_2_1/Crops",
    "x_2_7_1_2"="_2_7_1_2",
    "x_1_4_2_1xLivestock" ="_1_4_2_1/Livestock",
    "x_2_7_1_3"="_2_7_1_3",
    "x_1_4_2_1xFish" ="_1_4_2_1/Fish",
    "x_2_7_1_4"="_2_7_1_4",
    "x_1_4_2_1xTrees" ="_1_4_2_1/Trees",
    "x_2_7_1_5"="_2_7_1_5",
    "x_1_4_2_1xHoney" ="_1_4_2_1/Honey",
    "x_2_7_1_6"="_2_7_1_6",
    "x_1_4_2_1xother" ="_1_4_2_1/other")%>%
  
  #5_biodiversity
  mutate(across(c(x_3_4_2_2_2_1_calculate,x_3_4_2_2_2_2_calculate,
                  x_2_9_1_1,x_3_3_1_7,x_3_3_3_3,x_3_3_3_4,x_2_12_1,
                  x_3_3_3_1xAgroforestry,x_3_3_3_1xFallow,
                  x_3_3_3_1xHedgerowsxLive_fences,x_3_3_3_1xHomegarden,
                  x_3_3_3_1xNatural_stripsxvegetation,
                  x_3_3_3_1xPollinatorxFlower_strips,x_3_3_3_1xPushxpull,
                  "_3_4_3_1_1","_3_4_3_4_1"), as.numeric ))%>%
  mutate(across(c(
    #3_soil_health
    x_2_9_1_1,
    #4_animal_health
    x_2_10_1_2,
    #7_economic_diversification
    x_2_4_1,
    #5_biodiversity
    x_3_4_2_2_2_1_calculate,x_3_4_2_2_2_2_calculate,
    x_3_3_1_7,x_3_3_3_3,x_3_3_3_4,x_2_12_1,
    x_3_3_3_1xAgroforestry,x_3_3_3_1xFallow,
    x_3_3_3_1xHedgerowsxLive_fences,x_3_3_3_1xHomegarden,
    x_3_3_3_1xNatural_stripsxvegetation,x_3_3_3_1xPollinatorxFlower_strips,x_3_3_3_1xPushxpull,
    "_3_4_3_1_1","_3_4_3_4_1"), ~replace_na(., 0)))%>%
  #5_biodiversity (sum )
  mutate(x_3_4_2_2_2_2_calculate= if_else(x_3_4_2_2_2_2_calculate>0,x_3_4_2_2_2_2_calculate-1,x_3_4_2_2_2_2_calculate))%>%
  mutate("_3_4_3_3_1"= x_3_4_2_2_2_1_calculate+x_3_4_2_2_2_2_calculate)%>%
  #6_synergy
  mutate("6_synergy" = x_2_9_1_1+x_3_3_1_7+x_3_3_3_3+x_3_3_3_4+x_2_12_1+
           x_3_3_3_1xAgroforestry+x_3_3_3_1xFallow+x_3_3_3_1xHedgerowsxLive_fences+
           x_3_3_3_1xHomegarden+x_3_3_3_1xNatural_stripsxvegetation+
           x_3_3_3_1xPollinatorxFlower_strips+x_3_3_3_1xPushxpull)%>%
  #10_fairness
  mutate(x_2_6_1_4_1= if_else(is.na(x_2_6_1_4_1)&x_1_4_2_1xCrops=="1",paste("produce_crop",sep = "" ),paste("NO_produce_crop",sep = "" )))%>%
  mutate(x_2_6_1_4_2= if_else(is.na(x_2_6_1_4_2)&x_1_4_2_1xLivestock=="1",paste("produce_livestock",sep = "" ),paste("NO_produce_livestock",sep = "" )))%>%
  mutate(x_2_6_1_4_3= if_else(is.na(x_2_6_1_4_3)&x_1_4_2_1xFish=="1",paste("produce_fish",sep = "" ),paste("NO_produce_fish",sep = "" )))%>%
  mutate(x_2_6_1_4_4= if_else(is.na(x_2_6_1_4_4)&x_1_4_2_1xTrees=="1",paste("produce_trees",sep = "" ),paste("NO_produce_trees",sep = "" )))%>%
  mutate(x_2_6_1_4_5= if_else(is.na(x_2_6_1_4_5)&x_1_4_2_1xHoney=="1",paste("produce_honey",sep = "" ),paste("NO_produce_honey",sep = "" )))%>%
  mutate(x_2_6_1_4_6= if_else(is.na(x_2_6_1_4_6)&x_1_4_2_1xother=="1",paste("produce_other",sep = "" ),paste("NO_produce_other",sep = "" )))%>%
  
  #11_connectivity
  mutate(x_2_7_1_1= if_else(x_1_4_2_1xCrops=="1",paste("crop_",x_2_7_1_1,sep = "" ),paste("NOcrop_",x_2_7_1_1,sep = "" )))%>%
  mutate(x_2_7_1_2= if_else(x_1_4_2_1xLivestock=="1",paste("livestock_",x_2_7_1_2,sep = "" ),paste("NOlivestock_",x_2_7_1_2,sep = "" )))%>%
  mutate(x_2_7_1_3= if_else(x_1_4_2_1xFish=="1",paste("fish_",x_2_7_1_3,sep = "" ),paste("NOfish_",x_2_7_1_3,sep = "" )))%>%
  mutate(x_2_7_1_4= if_else(x_1_4_2_1xTrees=="1",paste("trees_",x_2_7_1_4,sep = "" ),paste("NOtrees_",x_2_7_1_4,sep = "" )))%>%
  mutate(x_2_7_1_5= if_else(x_1_4_2_1xHoney=="1",paste("honey_",x_2_7_1_5,sep = "" ),paste("NOhoney_",x_2_7_1_5,sep = "" )))%>%
  mutate(x_2_7_1_6= if_else(x_1_4_2_1xother=="1",paste("other_",x_2_7_1_6,sep = "" ),paste("NOother_",x_2_7_1_6,sep = "" )))%>%
  
  rename(
    #3_soil_health
    "_2_9_1_1"="x_2_9_1_1",
    #4_animal_health
    "_2_10_1_2"="x_2_10_1_2",
    #7_economic_diversification
    "_2_4_1"="x_2_4_1",
    #5_biodiversity
    "x_3_4_2_2_2_1_calculate/"="x_3_4_2_2_2_1_calculate",
    "x_3_4_2_2_2_2_calculate/"="x_3_4_2_2_2_2_calculate",
    "x_3_3_1_7/"="x_3_3_1_7",
    "x_3_3_3_3/"="x_3_3_3_3",
    "x_3_3_3_4/"="x_3_3_3_4",
    "x_2_12_1/"="x_2_12_1",
    "_3_3_3_1/"="_3_3_3_1",
    "x_3_3_3_1xAgroforestry/"="x_3_3_3_1xAgroforestry",
    "x_3_3_3_1xFallow/"="x_3_3_3_1xFallow",
    "x_3_3_3_1xHedgerowsxLive_fences/"="x_3_3_3_1xHedgerowsxLive_fences",
    "x_3_3_3_1xHomegarden/"="x_3_3_3_1xHomegarden",
    "x_3_3_3_1xNatural_stripsxvegetation/"="x_3_3_3_1xNatural_stripsxvegetation",
    "x_3_3_3_1xPollinatorxFlower_strips/"="x_3_3_3_1xPollinatorxFlower_strips",
    "x_3_3_3_1xPushxpull/"="x_3_3_3_1xPushxpull",
    #10_fairness
    "_2_6_1_4_1"="x_2_6_1_4_1",
    "_2_6_1_4_2"="x_2_6_1_4_2",
    "_2_6_1_4_3"="x_2_6_1_4_3",
    "_2_6_1_4_4"="x_2_6_1_4_4",
    "_2_6_1_4_5"="x_2_6_1_4_5",
    "_2_6_1_4_6"="x_2_6_1_4_6",
    
    #11_connectivity 
    "x_1_4_2_1xCrops/"="x_1_4_2_1xCrops",
    "x_1_4_2_1xLivestock/"="x_1_4_2_1xLivestock",
    "x_1_4_2_1xFish/"="x_1_4_2_1xFish",
    "x_1_4_2_1xHoney/"="x_1_4_2_1xHoney",
    "x_1_4_2_1xTrees/"="x_1_4_2_1xTrees",
    "x_1_4_2_1xother/"="x_1_4_2_1xother",
    "_1_4_2_1/"="_1_4_2_1",
    "_2_7_1_1"="x_2_7_1_1",
    "_2_7_1_2"="x_2_7_1_2",
    "_2_7_1_3"="x_2_7_1_3",
    "_2_7_1_4"="x_2_7_1_4",
    "_2_7_1_5"="x_2_7_1_5",
    "_2_7_1_6"="x_2_7_1_6")%>%
  select(-contains("/"))%>% 
  gather(key = "name_question", value = "name_choice",-kobo_id, -country)%>%
  dplyr::left_join(select(agrocology_choices,c(name_question,module,indicator,name_choice,score_agroecology_module,label_choice,
                                               label_question,type,type_question,list_name)), 
                   by= c("name_question"="name_question",
                         "name_choice"="name_choice"))%>%
  select("module", "indicator","country",
         "kobo_id","name_question", "type", "type_question"  , "list_name" , 
         "label_question","label_choice", "name_choice" ,"score_agroecology_module")

write.csv(agroecology_zimbabwe,file='agroecology_module/agroecology_zimbabwe.csv',quote=FALSE)


sort(unique(agroecology_zimbabwe$name_question))
sort(unique(agroecology_zimbabwe$name_choice))  
sort(unique(agroecology_zimbabwe$indicator))