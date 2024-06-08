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
   #filter(
  # indicator=="5_biodiversity"
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
# )%>%  
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
zwe_agroecology_all<- rbind(
  result_zwe_agroecology,
  # Indicator: "11_connectivity"
  result_1_4_2_7_begin_repeat)%>%
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
    name_question_recla=="_2_9_1_1_1"~"_2_9_1_1",
    TRUE ~ name_question_recla))%>%
  filter(name_question!="_2_9_1_1/other")%>% #Remove the rows with "_2_9_1_1/other"== NA
  filter(!(name_question %in% c("_2_9_1_1_1") & is.na(name_choice)))%>%
  mutate(label_choice = case_when(
    name_question=="_2_9_1_1_1"~"Specify other practice:",
    TRUE ~ label_choice))%>%
  mutate(label_question = case_when(
    name_question=="_2_9_1_1_1"~"Which ecological practices do you use on cropland to improve soil quality and health?", #Put the same label_question to the specify other question
    TRUE ~ label_question))%>%
  mutate(name_choice = case_when(
    str_detect(name_question,"_2_9_1_1/")~label_choice, #Replace code in name_question by label_choice for analysis
    TRUE ~ name_choice))%>%
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
    str_detect(name_question_recla,"_1_4_2_1/")~str_replace(name_question_recla, "/.*", ""),
    TRUE ~ name_question_recla))

  
sort(unique(zwe_agroecology_all$indicator))
sort(unique(zwe_agroecology_all$name_question_recla))

sort(unique(zwe_agroecology_all$label_question))
sort(unique(zwe_agroecology_all$name_question))

table(zwe_agroecology_all$label_question,zwe_agroecology_all$name_question_recla)
unique(zwe_agroecology_all$sheet_id)
names(zwe_agroecology_all)
view(dfSummary(zwe_agroecology_all))

write.csv(zwe_agroecology_all,file="C:/Users/andreasanchez/OneDrive - CGIAR/Bioversity/AI/HOLPA/analysis/HOLPA/HOLPA/zwe/zwe_agroecology2.csv",row.names=FALSE)


