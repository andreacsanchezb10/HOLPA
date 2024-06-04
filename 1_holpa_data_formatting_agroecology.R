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
     indicator==
       #"13_participation" 
     #"12_governance"
   "11_connectivity"
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
  mutate(name_question_recla = case_when(
    # Indicator: "2_input_reduction"
    str_detect(name_question_recla,"_1_4_3_1/")~str_replace(name_question_recla, "/.*",""),
    str_detect(name_question_recla,"_1_4_3_5/")~str_replace(name_question_recla, "/.*", ""),
    str_detect(name_question_recla,"_1_4_3_8/")~str_replace(name_question_recla, "/.*", ""),
    # Indicator: "3_soil_health"
    str_detect(name_question_recla,"_2_9_1_1/")~str_replace(name_question_recla, "/.*", ""),
    # Indicator: "4_animal_health"
    str_detect(name_question_recla,"_2_10_1_2/")~str_replace(name_question_recla, "/.*", ""),
    # Indicator: "7_economic_diversification"
    str_detect(name_question_recla,"_2_4_1/")~str_replace(name_question_recla, "/.*", ""),
    # Indicator: "11_connectivity"
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

  ## THEME: ENVIRONMENTAL----
# Indicator: biodiversity_agrobiodiversity
m
  mutate(label_choice = case_when(
    name_question == "_3_4_3_1_1" ~ paste("produce", name_choice, "crop species", sep = " "),
    name_question == "_3_4_2_2_2_3_calculate" ~ paste("produce", name_choice, "livestock species", sep = " "),
    name_question == "_3_4_3_4_1" ~ paste("produce", name_choice, "fish species", sep = " "),
    TRUE ~ label_choice))%>%
  mutate(name_question_recla  = case_when(
    label_question==  "In the last 12 months [add country meaning], how many different crops species (including perennial crops) were produced on your farm"~ "_3_4_3_1_1" ,
    label_question==  "In the last 12 months [add country meaning], how many different livestock species were produced in your farm?"~ "_3_4_2_2_2_3_calculate" ,
    name_question %in% c("c1", "c2", "c3", "c4", "c5", "c6", "c7", "c8","c9", "c10", "c11", "c12", "c13", "c14", "c15", "c16", "c17", "c18", "c19", "c20")~paste("_3_4_3_1_1_2/",name_question,sep=""),
    name_question %in% c("l1", "l2", "l3", "l4", "l5", "l6", "l7", "l8") ~ paste("_3_4_3_3_1/other",name_question,sep=""),
    TRUE ~ name_question_recla))%>%
  
  mutate(name_choice = case_when(
    type_question == "select_multiple" & name_choice == "1" ~ str_extract(name_question, "(?<=/).*"),
    #name_question_recla == "_3_4_3_3_1" & name_choice == "1" ~ str_extract(name_question, "(?<=/).*"),
    TRUE ~ name_choice))%>%
  
  filter(!(str_detect(name_question_recla, "_3_4_3_1_1_2/c")& is.na(name_choice)))%>%
  
  mutate(label_choice = case_when(name_question %in% c("l1", "l2", "l3", "l4", "l5", "l6", "l7", "l8",
                                                       "_2_8_4_3_4",
                                                       "_4_1_1_5_1_1","_4_1_1_5_2_1",
                                                       "_4_1_1_4_4_1","_4_1_2_1_10",
                                                       "_4_1_3_1_1","_4_1_3_1_2_1",
                                                       "_2_3_1_1_1","_4_1_3_2_13_1",
                                                       "_3_4_1_1_7_1_2_1",
                                                       "_3_3_4_4_1",
                                                       "_3_4_2_1_6_1_1") ~ "Other (please specify)",TRUE ~ label_choice))%>%
  
  mutate(label_question = case_when(
    name_question_recla== "_3_4_3_3_1" ~  "In the last 12 months [add country meaning], which different livestock species did you keep?",
    # Indicator: biodiversity_practices
    name_question_recla== "_2_9_1_1_1" ~ "Which ecological practices do you use on cropland to improve soil quality and health?",
    name_question_recla== "_3_3_1_7_1" ~ "What ecological practices did you apply in the last 12 months [add country meaning] on the farm to manage crop pests?",
    # Indicator: energy
    name_question_recla== "_2_8_4_1_1"~ "What types of energy do you use for: Irrigation",
    name_question_recla=="_2_8_4_2_1" ~ "What types of energy do you use for: Tillage, sowing or harvesting",
    name_question_recla== "_2_8_4_3_4" ~ "What types of energy do you use for: Cleaning, processing or transporting harvested food",
    name_question_recla=="_2_8_4_3_1"~"What types of energy do you use for: Cooking",
    # Indicator: water
    name_question_recla=="_3_3_4_1_1_1"~"What methods of irrigation do you use",
    name_question_recla=="_3_3_4_1_2_1"~"Where do you source your water for irrigation?",
    TRUE ~ label_question))%>%
  
  ## Indicator: biodiversity_diversity
  filter(!(name_question_recla == "_3_3_1_1_9_1" & is.na(name_choice)))%>% #Remove the rows with **Specify other landscape features:** == NA 
  
  mutate(label_choice = case_when(name_question_recla%in% c("_2_9_1_1_1","_3_3_1_7_1") ~ "other ecological practice",TRUE ~label_choice))%>%
  
  mutate(name_question_recla = case_when(
    name_question_recla== "_2_9_1_1_1" ~ "_2_9_1_1/other",
    name_question_recla== "_3_3_1_7_1" ~ "_3_3_1_7/other",
    # Indicator: energy
    name_question_recla=="_2_8_4_1_1"~ "_2_8_4_1/other",
    name_question_recla=="_2_8_4_2_1"~ "_2_8_4_2/other",
    name_question_recla== "_2_8_4_3_4" ~ "_2_8_4_4/other",
    name_question_recla=="_2_8_4_3_1"~ "_2_8_4_3/other",
    # Indicator: water
    name_question_recla== "_3_3_4_1_1_1"~ "_3_3_4_1_1/other",
    name_question_recla=="_3_3_4_1_2_1"~"_3_3_4_1_2/other",
    TRUE ~name_question_recla))%>%
  
  # Indicator: water
  mutate(name_choice = case_when(
    name_question_recla == "_3_4_1_2_7_2_2_1" ~ str_replace(name_choice, "//1$", paste0("//", label_choice)),TRUE ~ name_choice))%>%
  
  filter(!(str_ends(name_question_recla, "//0") & name_question_recla == "_3_4_1_2_7_2_2_1"))%>%
  
  ## THEME: SOCIAL----
# Indicator: 
mutate(name_question_recla = str_replace(name_question_recla, "_audio", ""))%>%
  
  # Indicator: land tenure
  mutate(name_choice = case_when(name_question_recla%in% c("_1_4_4_1_1", "_1_4_4_1_2", "_1_4_4_2_1" ,"_1_4_4_2_2", "_1_4_4_3_1", "_1_4_4_3_2",
                                                           "_1_4_4_4_1") &
                                   is.na(name_choice)~ "0",TRUE ~name_choice))%>%
  mutate(label_choice = case_when(name_question_recla%in% c("_1_4_4_1_1", "_1_4_4_1_2", "_1_4_4_2_1" ,"_1_4_4_2_2", "_1_4_4_3_1", "_1_4_4_3_2",
                                                            "_1_4_4_4_1",
                                                            "_1_4_1_1_1","_1_4_1_1_2","_1_4_1_1_3") ~ "acres",TRUE ~label_choice))%>%
  ## THEME: ECONOMIC ----
mutate(label_question = case_when(
  # Indicator: "climate_resilience"
  name_question == "_4_1_1_5_2_1" ~ "Credit for what types of investment",
  name_question == "_4_1_1_5_1_1" ~"Please indicate the source of the credit you obtained for your farming business",
  # Indicator: climate_resilience_shocks
  name_question =="_4_1_3_1_1"~"In the last 12 months, what were the most severe shocks faced by the household",
  name_question =="_4_1_3_1_2_1"~"What did the household members do to cope with the shocks",
  # Indicator: climate_resilience_social_network
  name_question =="_2_3_1_1_1"~"Select all the associations/organizations of which you or other HH members are a part of.",
  TRUE ~label_question))%>%
  
  mutate(name_question_recla = case_when(
    # Indicator: "climate_resilience"
    name_question == "_4_1_1_5_2_1" ~ "_4_1_1_5_2/other",
    name_question =="_4_1_1_5_1_1" ~ "_4_1_1_5_1",
    # Indicator: climate_resilience_shocks
    name_question =="_4_1_3_1_1" ~ "_4_1_3_1/other",
    name_question =="_4_1_3_1_2_1" ~ "_4_1_3_1_2/other",
    # Indicator: climate_resilience_social_network
    name_question =="_2_3_1_1_1" ~ "_2_3_1_1/other",
    # Indicator: labour_productivity
    name_question =="_3_4_1_1_7_1_2_1"~ "_3_4_1_1_7_1_2/other",
    TRUE ~name_question_recla))%>%
  
  ## THEME: AGRICULTURAL ----
mutate(label_question = case_when(
  # Indicator: animal_health
  name_question =="_3_3_4_4_1"~"Where do you source your water for drinking water for livestock?",
  # Indicator: productivity_crops
  name_question =="_3_4_2_1_5_1_2"~"Select crop production unit:",
  name_question =="_3_4_2_1_6_1_1"~"What does the produced crop get used for?",
  # Indicator: productivity_livestock
  name_question =="_3_4_2_2_5_1"~"What does the livestock get used for?",
  name_question =="_3_4_2_2_6_1_1"~"Select livestock production unit:",
  name_question =="_3_4_2_2_6_3_1"~ "What does the livestock production get used for?",
  TRUE ~label_question))%>%
  
  mutate(name_question_recla = case_when(
    # Indicator: animal_health
    name_question =="_3_3_4_4_1"~"_3_3_4_4",
    # Indicator: productivity_crops
    name_question =="_3_4_2_1_5_1_2"~"_3_4_2_1_5_1",
    name_question =="_3_4_2_1_6_1_1"~"_3_4_2_1_6_1/other",
    # Indicator: productivity_livestock
    name_question =="_3_4_2_2_5_1"~"_3_4_2_2_5/other",
    name_question =="_3_4_2_2_6_1_1"~"_3_4_2_2_6_1",
    name_question =="_3_4_2_2_6_3_1"~"_3_4_2_2_6_3/other",
    str_detect(name_question_recla,"_3_4_2_2_5/")~str_replace(name_question_recla, "_[^_]*$", ""),
    str_detect(name_question_recla,"_3_4_2_2_6_3/")~str_replace(name_question_recla, "_[^_]*$", ""),
    # Indicator: nutrient_use
    str_detect(name_question_recla,"_1_4_3_1/")~"_1_4_3_1",
    TRUE ~name_question_recla))%>%
  
  mutate(name_choice = case_when(
    # Indicator: productivity_livestock
    str_detect(name_question, "_3_4_2_2_5/")~str_replace(name_choice, "_[^_]*$", ""),
    str_detect(name_question, "_3_4_2_2_6_3/")~str_replace(name_choice, "_[^_]*$", ""),
    TRUE ~name_choice))%>%
  
  #All----

#Replace ${_1_4_1_1} in label_choice by the hectares or acres
mutate(label_choice = gsub("\\$\\{_1_4_1_1\\}", "acres", label_choice))%>%
  mutate(label_choice =case_when(name_question %in% c("_3_4_2_1_1","_3_4_2_2_1_1","_3_4_2_2_1_2","_3_4_2_1_3","_3_3_3_2_2")~"in acres",TRUE ~label_choice))%>%
  
  
  #mutate(label_question = gsub("\\$\\{_1_4_1_1_calculate\\}", "acres", label_question))%>%
  filter(!(name_question %in% c("_3_3_1_1_9_1", "_2_9_1_1_1", "_3_3_1_7_1","_2_8_4_3_4",
                                "_3_1_2_2_1","_3_1_2_8",
                                "_4_1_1_5_2_1","_4_1_1_5_1_1","_4_1_1_6_1",
                                "_4_1_1_4_4_1","_2_4_1_2","_4_1_2_1_10","_4_1_2_1_2","_4_1_2_1_4","_4_1_2_1_5","_4_1_2_1_6","_4_1_2_1_7","_4_1_2_1_9",
                                "_4_1_3_1_1","_4_1_3_1_2_1",
                                "_2_3_1_1_1","_4_1_3_2_13_1",
                                "_3_2_1_2_1",
                                "_3_4_1_1_1_1","_3_4_1_1_1_2","_3_4_1_1_2_1","_3_4_1_1_2_2","_3_4_1_1_3_1","_3_4_1_1_3_2","_3_4_1_1_4_1","_3_4_1_1_4_2","_3_4_1_1_5_1","_3_4_1_1_5_2","_3_4_1_1_6_1","_3_4_1_1_6_2",
                                "_3_4_1_1_7_1_2_1",
                                "_3_3_4_4_1",
                                "_3_4_2_1_8_2","_3_4_2_1_8_3",
                                "_1_4_3_2_2","_1_4_3_2_3","_1_4_3_3_2","_1_4_3_3_3","_1_4_3_4_2","_1_4_3_4_3",
                                "_3_4_2_1_1",
                                "_3_4_2_2_1_1",
                                "_3_4_2_1_5_1_2","_3_4_2_1_6_1_1","_3_4_2_1_6_2_9","_3_4_2_1_6_2_1","_3_4_2_1_6_2_2",
                                "_3_4_2_1_6_2_3","_3_4_2_1_6_2_4","_3_4_2_1_6_2_5","_3_4_2_1_6_2_6",
                                "_3_4_2_1_6_2_7","_3_4_2_1_6_2_8",  "_3_4_2_1_6_2_9","_3_4_2_1_7_1","_3_4_2_1_7_2","_3_4_2_1_7_3","_3_4_3_1_3_calculate",
                                "_3_4_2_2_1_2","_3_4_2_2_5_1",
                                "_3_4_2_1_7_4","_3_4_2_2_6_1_1","_3_4_2_2_6_3_1",
                                "_4_1_4_13_1",
                                "_4_1_4_12",
                                "_2_4_1_1","_3_1_2_3_1","_3_1_2_4_1","_3_1_2_5_1","_3_1_2_6_1","_3_1_2_7_1"
                                
                                
  ) & is.na(name_choice)))%>% #Remove the rows with **Specify other:** == NA 
  filter(!(str_detect(name_question, "audio") & is.na(name_choice)))%>% #Remove the rows with **Specify other:** == NA 
  filter(!(name_question %in% c("_3_4_3_3_1/other", "_2_9_1_1/other", "_3_3_1_7/other","_2_8_4_4/other",
                                "_4_1_1_5_1/other","_4_1_1_5_2/other",
                                "_4_1_3_1/other","_4_1_3_1_2/other",
                                "_2_3_1_1/other",
                                "_3_4_1_1_7_1_2/other",
                                "_3_3_4_4/other",
                                "_3_4_2_1_6_1/other", "_3_4_2_2_5/other","_3_4_2_2_5/other_1",
                                "_3_4_2_2_6_3/other_10","_3_4_2_2_6_3/other_2","_3_4_2_2_6_3/other_3","_3_4_2_2_6_3/other_4","_3_4_2_2_6_3/other_6",
                                "_3_3_4_1_1/other")))%>%
  filter(!(name_question %in%c("_3_4_2_1_5_1")& name_choice=="other"))

write.csv(result2,file="C:/Users/andreasanchez/OneDrive - CGIAR/Bioversity/AI/HOLPA/analysis/HOLPA/HOLPA/zwe/zwe_performance.csv",row.names=FALSE)




#_______________________
### Country databases ####
zwe_survey <- read_excel(path=paste0(zwe.data.path,"household_database_2024.04.18_clean.xlsx"),
                              sheet = "Final HOLPA_Zimbabwe_Household")%>%
  rename("kobo_farmer_id"="_id")%>%
  rename("country"="_1_2_1_3")%>%
  mutate("_2_6_1_4_6"= NA)%>%
  mutate("_2_7_1_6/other"= NA)

zwe_choices <- read_excel(path=paste0(zwe.data.path,"zimbabwe_household_survey.xlsx"),
                               sheet = "choices")%>%
  select("list_name","name","label::English ((en))","score_agroecology_module")%>%
  mutate(country="zimbabwe")%>%
  rename("label_choice" = "label::English ((en))")%>%
  rename("name_choice" = "name")

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
  filter(type!="end_group")%>%
  #separate question type components
  mutate(type_question = ifelse(substr(type,1,10)=="select_one","select_one",
                                ifelse(substr(type,1,10)=="select_mul","select_multiple",type)))%>%
  #create column with list_name codes matching the choices worksheet
  mutate(list_name = if_else(type_question== "select_one"|type_question== "select_multiple", 
                             str_replace(.$type, paste0(".*", .$type_question), ""),NA))%>%
  mutate(list_name = str_replace_all(list_name, " ", ""))  #%>% mutate(global_r_list_name =  sub('*_', "", name_question)) %>%mutate(global_r_list_name = ifelse(grepl("_", global_r_list_name, fixed = TRUE)==TRUE,global_r_list_name,""))

global_choices <- read_excel(paste0(global.data.path,"HOLPA_global_household_survey_20231204_mapped_to_indicators_master.xlsx"),
                             sheet = "choices")%>%
  select("list_name","name","label::English ((en))","score_agroecology_module","country")%>%
  rename("label_choice" = "label::English ((en))")%>%
  rename("name_choice" = "name")%>%
  #Rbind country specific choices 
  distinct(list_name,name_choice,label_choice, .keep_all = TRUE) %>%
  right_join(global_survey,by="list_name",relationship="many-to-many")

#### Agroecology module ####
agroecology_survey <-  global_survey %>%
  filter(str_detect(module, "agroecology"))%>%
  #select(-subindicator)%>%
  mutate(module= "agroecology")%>%
  mutate(subindicator=indicator) %>%
  mutate(type_question = case_when(
    name_question %in% c(
      "_2_9_1_1","_2_4_1","_2_10_1_2",
      #6_synergy                   
      "_2_9_1_1","_3_3_1_7","_3_3_3_3","_3_3_3_4","_2_12_1") ~ "count",
    TRUE ~ type_question))

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
  #select(-subindicator)%>%
  mutate(module= "agroecology")%>%
  mutate(subindicator=indicator) %>%
  mutate(name_question_choice= if_else(type_question=="select_multiple",
                                       paste(name_question,"/",name_choice, sep=""),
                                       name_question))%>%
  mutate(type_question = case_when(
    name_question %in% c(
      "_2_9_1_1","_2_4_1","_2_10_1_2",
      #6_synergy                   
      "_2_9_1_1","_3_3_1_7","_3_3_3_3","_3_3_3_4","_2_12_1") ~ "count",
    TRUE ~ type_question))

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


#### Assign scores (1-5) to agroecology question responses using a pre-made lookup table #### 
agrocology_choices_multiple<-agroecology_choices%>%
  filter(type_question!="count")%>%
  select(-name_question_choice)%>%
  rbind(read_excel(paste0(global.data.path,"HOLPA_global_household_survey_20231204_mapped_to_indicators_master.xlsx"),
                   sheet = "agroecology_look_up"))

names(agrocology_choices_multiple)

#### Principles structure ####
sort(unique(agroecology_choices$indicator))

agroecology_questions_columns<- agroecology_choices%>% 
  filter(!str_detect(indicator, "extra"))%>%
  select(label_question, name_question_choice)%>%
  dplyr::distinct(name_question_choice, .keep_all = TRUE)%>%
  spread(key = name_question_choice, value = label_question)%>%
  mutate("kobo_farmer_id"="kobo_farmer_id",
         "country"="country_name")

agroecology_questions_columns <- colnames(agroecology_questions_columns)
agroecology_questions_columns

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
agroecology_select_multiple_columns<- agroecology_survey%>%
  filter(!str_detect(indicator, "extra"))%>%
  filter(type_question == "select_multiple")%>%
  select(name_question,type_question)%>%
  #mutate(name_question= paste(name_question,"/",sep = ""))%>%
  spread(key = name_question, value = type_question)

agroecology_select_multiple_columns<- colnames(agroecology_select_multiple_columns)
agroecology_select_multiple_columns

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
agroecology_count_columns<- agroecology_survey%>%
  filter(!str_detect(indicator, "extra"))%>%
  filter(type_question == "count")%>%
  select(name_question,type_question)%>%
  mutate(name_question= paste(name_question,"/",sep = ""))%>%
  spread(key = name_question, value = type_question)

agroecology_count_columns<- colnames(agroecology_count_columns)
agroecology_count_columns


## ANALYSIS BY COUNTRY
# Zimbabwe
zwe_agroecology_columns <- intersect(agroecology_questions_columns, colnames(zwe_survey))
zwe_agroecology_columns

names(zwe_agroecology_columns)
zwe_agroecology <- zwe_survey %>%
  select(all_of(zwe_agroecology_columns))%>%
  mutate_all(as.character)%>%
  process_columns_regex(agroecology_select_multiple_columns)%>%
  count_columns_regex(agroecology_count_columns)%>%
  dplyr::rename_with(~ paste0("x", gsub("/", "", .)), .cols = c(
    "_2_9_1_1/","_2_4_1/","_2_10_1_2/",
    #5_biodiversity
    "_3_3_1_7/","_3_3_3_3/", "_3_3_3_4/","_2_12_1/",
    "_3_4_2_2_2_3_calculate","_3_4_2_2_2_2_calculate",
    #10_fairness
    "_2_6_1_4_1","_2_6_1_4_2","_2_6_1_4_3","_2_6_1_4_4","_2_6_1_4_5","_2_6_1_4_6",
    #11_connectity
    "_2_7_1_1","_2_7_1_2","_2_7_1_3","_2_7_1_4","_2_7_1_5","_2_7_1_6"))%>%
  dplyr::rename_with(~ paste0("x", gsub("/", "x", .)), .cols = c(
    #5_biodiversity
    "_3_3_3_1/Agroforestry","_3_3_3_1/Fallow","_3_3_3_1/Hedgerows/Live_fences","_3_3_3_1/Homegarden",
    "_3_3_3_1/Natural_strips/vegetation","_3_3_3_1/Pollinator/Flower_strips","_3_3_3_1/Push-pull",
    #11_connectity 
    "_1_4_2_1/Crops","_1_4_2_1/Livestock","_1_4_2_1/Fish","_1_4_2_1/Trees","_1_4_2_1/Honey","_1_4_2_1/other"))%>%
  dplyr::rename_with(~ paste0(gsub("-", "x", .)), .cols = c("x_3_3_3_1xPush-pull"))%>%
  
  #5_biodiversity
  mutate(across(c(x_3_4_2_2_2_3_calculate,x_3_4_2_2_2_2_calculate,
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
    x_3_4_2_2_2_3_calculate,x_3_4_2_2_2_2_calculate,
    x_3_3_1_7,x_3_3_3_3,x_3_3_3_4,x_2_12_1,
    x_3_3_3_1xAgroforestry,x_3_3_3_1xFallow,
    x_3_3_3_1xHedgerowsxLive_fences,x_3_3_3_1xHomegarden,
    x_3_3_3_1xNatural_stripsxvegetation,x_3_3_3_1xPollinatorxFlower_strips,x_3_3_3_1xPushxpull,
    "_3_4_3_1_1","_3_4_3_4_1"), ~replace_na(., 0)))%>%
  #5_biodiversity (sum )
  mutate(x_3_4_2_2_2_2_calculate= if_else(x_3_4_2_2_2_2_calculate>0,x_3_4_2_2_2_2_calculate-1,x_3_4_2_2_2_2_calculate))%>%
  mutate("_3_4_3_3_1"= x_3_4_2_2_2_3_calculate+x_3_4_2_2_2_2_calculate)%>%
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
  
  dplyr::rename_with(~ paste0( gsub("x", "", .)), .cols = c(
    #3_soil_health
    "x_2_9_1_1",
    #4_animal_health
    "x_2_10_1_2",
    #7_economic_diversification
    "x_2_4_1",
    #10_fairness
    "x_2_6_1_4_1","x_2_6_1_4_2","x_2_6_1_4_3","x_2_6_1_4_4","x_2_6_1_4_5","x_2_6_1_4_6",
    #11_connectivity
    "x_2_7_1_1","x_2_7_1_2","x_2_7_1_3","x_2_7_1_4","x_2_7_1_5","x_2_7_1_6"
  ))%>%
  dplyr::rename_with(~ paste0(., "/"), .cols = c(
    #5_biodiversity
    "x_3_4_2_2_2_3_calculate","x_3_4_2_2_2_2_calculate","x_3_3_1_7","x_3_3_3_3","x_3_3_3_4","x_2_12_1","_3_3_3_1",
    "x_3_3_3_1xAgroforestry","x_3_3_3_1xFallow","x_3_3_3_1xHedgerowsxLive_fences","x_3_3_3_1xHomegarden",
    "x_3_3_3_1xNatural_stripsxvegetation","x_3_3_3_1xPollinatorxFlower_strips","x_3_3_3_1xPushxpull",
    #11_connectivity
    "x_1_4_2_1xCrops","x_1_4_2_1xLivestock","x_1_4_2_1xFish","x_1_4_2_1xHoney","x_1_4_2_1xTrees","x_1_4_2_1xother","_1_4_2_1"
  ))%>%
  select(-contains("/"))%>%
  gather(key = "name_question", value = "name_choice",-kobo_farmer_id, -country)%>%
  dplyr::left_join(select(agrocology_choices_multiple,c(name_question,module,indicator,name_choice,score_agroecology_module,label_choice,
                                                        label_question,type,type_question,list_name)), 
                   by= c("name_question"="name_question",
                         "name_choice"="name_choice"))%>%
  select("module", "indicator","country",
         "kobo_farmer_id","name_question", "type", "type_question"  , "list_name" , 
         "label_question","label_choice", "name_choice" ,"score_agroecology_module")

names(zwe_agroecology)
write.csv(zwe_agroecology,file=paste0(zwe.data.path,"zwe_agroecology.csv"),row.names=FALSE)

file=paste0(zwe.data.path,"zwe_agroecology.xlsx")

sort(unique(agroecology_zwe$name_question))
sort(unique(agroecology_zwe$name_choice))  
sort(unique(agroecology_zwe$indicator))