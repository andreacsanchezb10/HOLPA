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

zwe_survey_1_4_2_7_begin_repeat <- read_and_process_survey("_1_4_2_7_begin_repeat", "_submission__id", zwe.data.path,"zimbabwe","_index")%>% # Section: "production_end_use": other on-farm produce
  #Remove respondents that are not farmers
  filter(kobo_farmer_id!="274186917")

  
zwe_choices <- read_excel(path=paste0(zwe.data.path,"`zimbabwe_household_survey.xlsx`"),
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

#### Context module ####
context_survey <-  global_survey %>%
  filter(str_detect(module, "context"))%>%
  mutate(module= "context")

sort(unique(context_survey$subindicator))
context_survey$subindicator[str_detect(context_survey$subindicator,"respondent_characteristics")]<- "respondent_characteristics"
context_survey$subindicator[str_detect(context_survey$subindicator,"household_characteristic")]<- "household_characteristics"
context_survey$subindicator[str_detect(context_survey$subindicator,"context_all")]<- "context_all"
context_survey$subindicator[str_detect(context_survey$subindicator,"farm_characteristic")]<- "farm_characteristics"
context_survey$subindicator[str_detect(context_survey$subindicator,"inputs")]<- "inputs"
context_survey$subindicator[str_detect(context_survey$subindicator,"production_systems")]<- "production_systems"
context_survey$subindicator[str_detect(context_survey$subindicator,"household_labour")]<- "household_labour"
context_survey$subindicator[str_detect(context_survey$subindicator,"credit_access")]<- "credit_access"
context_survey$subindicator[str_detect(context_survey$subindicator,"income")]<- "income"
context_survey$subindicator[str_detect(context_survey$subindicator,"household_labour")]<- "household_labour"
context_survey$subindicator[str_detect(context_survey$subindicator,"training")]<- "training"
context_survey$subindicator[str_detect(context_survey$subindicator,"education")]<- "education"
context_survey$subindicator[str_detect(context_survey$subindicator,"literacy")]<- "literacy"
context_survey$subindicator[str_detect(context_survey$subindicator,"membership")]<- "membership"
context_survey$subindicator[str_detect(context_survey$subindicator,"accessibility")]<- "accessibility"

sort(unique(context_survey$subindicator))


context_choices1 <- global_choices %>%
  filter(str_detect(module, "context"))%>%
  mutate(module= "context")

sort(unique(context_choices1$subindicator))
# CORRECT: LAND TENURE SHOULD BE UNDER SOCIAL PERFORMANCE
context_choices1$subindicator[str_detect(context_choices1$subindicator,"respondent_characteristics")]<- "respondent_characteristics"
context_choices1$subindicator[str_detect(context_choices1$subindicator,"household_characteristic")]<- "household_characteristics"
context_choices1$subindicator[str_detect(context_choices1$subindicator,"context_all")]<- "context_all"
context_choices1$subindicator[str_detect(context_choices1$subindicator,"farm_characteristic")]<- "farm_characteristics"
context_choices1$subindicator[str_detect(context_choices1$subindicator,"inputs")]<- "inputs"
context_choices1$subindicator[str_detect(context_choices1$subindicator,"production_systems")]<- "production_systems"
context_choices1$subindicator[str_detect(context_choices1$subindicator,"household_labour")]<- "household_labour"
context_choices1$subindicator[str_detect(context_choices1$subindicator,"credit_access")]<- "credit_access"
context_choices1$subindicator[str_detect(context_choices1$subindicator,"income")]<- "income"
context_choices1$subindicator[str_detect(context_choices1$subindicator,"household_labour")]<- "household_labour"
context_choices1$subindicator[str_detect(context_choices1$subindicator,"training")]<- "training"
context_choices1$subindicator[str_detect(context_choices1$subindicator,"education")]<- "education"
context_choices1$subindicator[str_detect(context_choices1$subindicator,"literacy")]<- "literacy"
context_choices1$subindicator[str_detect(context_choices1$subindicator,"membership")]<- "membership"
context_choices1$subindicator[str_detect(context_choices1$subindicator,"accessibility")]<- "accessibility"
sort(unique(context_choices1$subindicator))

context_choices<- context_choices1%>%
  rename(theme = indicator,
         indicator = subindicator)%>%
  mutate(name_question_choice= if_else(type_question=="select_multiple",
                                     paste(name_question,"/",name_choice, sep=""),
                                     name_question))%>%
  filter(indicator==
           "production_systems")       
                                                                                     
         
         [37]  "climate_resilience"  "household_labour"

"climate_rainfall_timing"  #check what I did with select multiple questions for performance
"context_all"
"production_end_use" #ok
"training"
"structure"#ok
"inputs"#ok
"income" #ok
"id_site" #ok
"societal_factor"#ok
"project_involvement"  #ok
"secondary_occupation" #ok
"primary_occupation" #ok
"photo" #ok
"name"#ok
"personal_factors"#ok
"membership"#ok
"location" #ok
"marital_status" #ok
"literacy" #ok
"housing"#ok
"gender"#ok
"farmer_relation"#ok
"hh_head_relation" #ok
"farm_layout"#ok
"ethnicity"#ok
"enumerator"#ok
"education" #ok
"credit_access"#ok
"consent"#ok
"climate_temp"#ok
"climate_rainfall_change"#ok   
"climate_flood" #ok        
"climate_drought"  #ok
"agroecology_knowledge"#ok
"age_dob"#ok
"age_community" #ok
"accessibility"  #ok
 "photo" 
sort(unique(context_choices$indicator))

#### Context module ####
context_questions_columns<- context_choices%>% 
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
context_questions_columns


## ANALYSIS BY COUNTRY
# Zimbabwe
colnames(zwe_survey)
zwe_context_columns <- intersect(context_questions_columns, colnames(zwe_survey))
zwe_context_columns
context_questions_columns
mismatched_columns <- setdiff(context_questions_columns, zwe_context_columns)
print(mismatched_columns)

context_left_join <- function(context_choices, gathered_data ) {
  # Left join for "calculate" and "integer"
  continuous <- gathered_data  %>%
    dplyr::left_join(select(context_choices,
                            c(name_question, module, theme, indicator,"name_choice", label_choice, label_question,type, type_question, list_name)), 
                     by = "name_question")%>%
    filter(type_question =="calculate"|type_question =="integer"|type_question =="note"|
             type_question =="text"|type_question =="audio"|type_question =="decimal"|type_question =="geopoint"|type_question =="image")%>%
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
  
  # Left join for "select_one"
  select_one <- gathered_data  %>%
    left_join(select(context_choices,
                     c(name_question, name_choice, module, theme, indicator, label_choice, label_question, type, type_question, list_name)), 
              by = c("name_question"="name_question", "name_choice"="name_choice"))%>%
    filter(type_question=="select_one")
  
  result<- rbind(continuous,select_multiple,select_one)
  
  
  return(result)
}

# Final HOLPA_Zimbabwe_Household
zwe_context <- zwe_survey %>%
  select(all_of(zwe_context_columns))%>%
  mutate_all(as.character)

view(dfSummary(zwe_context))

# Identify columns with only NA values
na_columns <- colSums(is.na(zwe_context)) == nrow(zwe_context)
na_columns

# Remove columns with only NA values
zwe_context <- zwe_context[, !na_columns]

view(dfSummary(zwe_context))
zwe_survey
result <- zwe_context%>%
  gather(key = "name_question", value = "name_choice", -kobo_farmer_id, -country,-sheet_id,-index)%>%
  #Indicator: climate_drought and climate_flood
  #Error: the choices options should be "4_2_1_3" (1=yes, 0=no, notsure), but the country put "4_2_1_1" (increased, nochange, decreased, notsure)
  #So I reclassified those options into 1=yes, 0=no, notsure
  mutate(
    name_choice=case_when(name_question %in% c("_4_2_1_3_2","_4_2_1_3_1") & name_choice%in% c("increase","decrease") ~ "1",TRUE ~ name_choice),
    name_choice=case_when(name_question %in% c("_4_2_1_3_2","_4_2_1_3_1") & name_choice%in% c("nochange") ~ "0",TRUE ~ name_choice))%>%
  context_left_join(context_choices,.)%>%
  mutate(name_question_recla= name_question)%>%
  mutate(parent_table_name= NA,
         parent_index=NA)
names(result)
sort(unique(result$name_choice))

## _1_4_2_7_begin_repeat: production_end_use for OTHER NON-FARM PRODUCT  ----
zwe_context_columns_1_4_2_7_begin_repeat <- intersect(context_questions_columns, colnames(zwe_survey_1_4_2_7_begin_repeat))
zwe_context_columns_1_4_2_7_begin_repeat
context_questions_columns
mismatched_columns_1_4_2_7_begin_repeat <- setdiff(context_questions_columns, zwe_context_columns_1_4_2_7_begin_repeat)
print(mismatched_columns_1_4_2_7_begin_repeat)

zwe_context_1_4_2_7_begin_repeat <- zwe_survey_1_4_2_7_begin_repeat %>%
  select(all_of(zwe_context_columns_1_4_2_7_begin_repeat))%>%
  mutate_all(as.character)

view(dfSummary(zwe_context_1_4_2_7_begin_repeat))

# Identify columns with only NA values
na_columns_1_4_2_7_begin_repeat <- colSums(is.na(zwe_context_1_4_2_7_begin_repeat)) == nrow(zwe_context_1_4_2_7_begin_repeat)
na_columns_1_4_2_7_begin_repeat

# Remove columns with only NA values
zwe_context_1_4_2_7_begin_repeat <- zwe_context_1_4_2_7_begin_repeat[, !na_columns_1_4_2_7_begin_repeat]

view(dfSummary(zwe_context_1_4_2_7_begin_repeat))

result_1_4_2_7_begin_repeat <- zwe_context_1_4_2_7_begin_repeat%>%
  gather(key = "name_question", value = "name_choice", -kobo_farmer_id, -country,-sheet_id,-index,-parent_table_name,-parent_index)%>%
  context_left_join(context_choices,.)%>%
  mutate(name_question_recla= name_question)


# Combine all----
result2<- #rbind(
  result%>%#,
  # Indicator: production_end_use for OTHER NON-FARM PRODUCT
  #result_1_4_2_7_begin_repeat
  # )

  mutate(
    name_question_recla = case_when(
      #Indicator: "hh_head_relation"
      name_question_recla== "_1_2_1_5_1" ~ "_1_2_1_5",
      #Indicator: "farmer_relation" 
      name_question_recla== "_1_2_1_6_1"~ "_1_2_1_6",
      #Indicator: "housing"
      name_question_recla== "_3_2_1_3_1_1"~ "_3_2_1_3_1/other",
      name_question_recla== "_3_2_1_3_2_1"~ "_3_2_1_3_2/other",
      #Indicator: "membership"
      name_question_recla=="_2_3_1_1_1"~"_2_3_1_1/other",
      #Indicator: "primary_occupation"
      name_question_recla=="_1_2_1_13_1_1"~ "_1_2_1_13_1",
      #Indicator: "secondary_occupation"
      name_question_recla=="_1_2_1_13_2_2_1"~ "_1_2_1_13_2_2/other",
      #Indicator: "inputs"
      str_detect(name_question_recla,"_1_4_3_1/")~"_1_4_3_1",
      str_detect(name_question_recla, "_1_4_3_5/")~"_1_4_3_5",
      TRUE ~name_question_recla))%>%
  
  mutate(label_question = case_when(
    #Indicator: "hh_head_relation"
    name_question_recla== "_1_2_1_5" ~  "What is your relationship with the head of household",
    #Indicator: "farmer_relation"
    name_question_recla== "_1_2_1_6" ~  "What is your relationship with the person responsible for making decision on farm activities?",
    #Indicator: "housing"
    name_question_recla== "_3_2_1_3_1/other" ~  "Please observe the farmer's main household and ask, what material is the roof of the house made of?",
    name_question_recla== "_3_2_1_3_2/other" ~  "Please observe the farmer's main household and ask, what material are the walls of the house made of?",
    #Indicator: "membership"
    name_question_recla==  "_2_3_1_1/other"~"Select all the associations/organizations of which you or other HH members are part of",
    #Indicator: "primary_occupation"
    name_question_recla=="_1_2_1_13_1"~"What was your main occupation in the last 12 months?",
    #Indicator: "secondary_occupation"
    name_question_recla=="_1_2_1_13_2_2/other"~ "What was your other occupation(s) in the last 12 months?",
    TRUE ~ label_question))%>%
  
  #Indicator: "hh_head_relation"
  filter(!(name_question %in%c("_1_2_1_5")& name_choice=="other relative"))%>%
  #Indicator: "farmer_relation"
  filter(!(name_question %in%c("_1_2_1_6")& name_choice=="other"))%>%
    #Indicator: "housing"
  filter(!(name_question %in%c("_3_2_1_3_1/other")))%>%
  filter(!(name_question %in%c("_3_2_1_3_2/other")))%>%
  #Indicator: "primary_occupation"
  filter(!(name_question %in%c("_1_2_1_13_1")& name_choice=="other"))%>%
  #Indicator: "secondary_occupation"
  filter(!(name_question %in% c("_1_2_1_13_2_2/other")))%>%
           
  mutate(label_choice = case_when(
    #Indicator: "housing"
    name_question %in% c("_3_2_1_3_1_1","_3_2_1_3_2_1","_2_3_1_1_1","_1_2_1_13_1_1","_1_2_1_13_2_2_1") ~ "other",
    #Indicator: "location"
    name_question %in% c("_1_3")~ "latitude longitude altitude",
    #Indicator: "inputs"
    name_question %in% c( 
      #Indicator: "inputs"
      "_1_4_3_2_3","_1_4_3_3_3","_1_4_3_4_3","_1_4_3_6_3","_1_4_3_7_3",
      #Indicator: "context_all"
      "_1_4_1_1_1")~"acres",
    TRUE ~ label_choice))%>%
  filter(!(name_question%in% c("_1_2_1_5_1","_1_2_1_6_1","_3_2_1_3_1_1","_3_2_1_3_2_1","_1_3_1","_2_3_1_1_1",
                               "_1_2_1_13_1_1","_1_2_1_13_2_2_1","_1_2_1_15_1","_3_2_1_2_1",
                               "_1_4_3_2_2","_1_4_3_2_3","_1_4_3_3_2","_1_4_3_3_3","_1_4_3_4_2",
                               "_1_4_3_4_3","_1_4_3_6_4","_1_4_3_6_2","_1_4_3_6_3","_1_4_3_6_1_calculate",
                               "_4_1_1_4_4_1","_1_4_2_2_6_1","_1_4_2_3_7_1") & is.na(name_choice)))%>%
  filter(!(str_detect(name_question, "hh_photo") & is.na(name_choice))) #Remove the rows with **Specify other:** == NA 
  

sort(unique(result2$indicator))

sort(unique(result2$label_question))
sort(unique(result2$name_question))
sort(unique(result2$name_question_recla))
sort(unique(result2$name_choice)) 
