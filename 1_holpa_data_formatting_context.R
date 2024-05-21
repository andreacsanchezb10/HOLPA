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
  select(sheet_id)

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
context_survey$subindicator[str_detect(context_survey$subindicator,"land_tenure")]<- "land_tenure"
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
context_choices1$subindicator[str_detect(context_choices1$subindicator,"land_tenure")]<- "land_tenure"
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
           "inputs"  )       
                                                                                     
         [19]                                                                                 
         [25] "land_tenure"                                                                                  
         [31]                               "production_end_use"      "production_systems"         
         [37]             "structure"               "training" "climate_resilience"  "household_labour"

"climate_rainfall_timing"  #check what I did with select multiple questions for performance
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
# Combine all----
result2<- #rbind(
  result%>%
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
    TRUE ~ label_choice))%>%
  filter(!(name_question%in% c("_1_2_1_5_1","_1_2_1_6_1","_3_2_1_3_1_1","_3_2_1_3_2_1","_1_3_1","_2_3_1_1_1",
                               "_1_2_1_13_1_1","_1_2_1_13_2_2_1","_1_2_1_15_1","_3_2_1_2_1") & is.na(name_choice)))%>%
  filter(!(str_detect(name_question, "hh_photo") & is.na(name_choice))) #Remove the rows with **Specify other:** == NA 
  

sort(unique(result2$indicator))

sort(unique(result2$label_question))
sort(unique(result2$name_question))
sort(unique(result2$name_question_recla))
sort(unique(result2$name_choice)) 



## _3_4_3_1_2_begin_repeat: Crop production ----
zwe_performance_columns_3_4_3_1_2_begin_repeat <- intersect(performance_questions_columns, colnames(zwe_survey_3_4_3_1_2_begin_repeat))
zwe_performance_columns_3_4_3_1_2_begin_repeat
performance_questions_columns
mismatched_columns_3_4_3_1_2_begin_repeat <- setdiff(performance_questions_columns, zwe_performance_columns_3_4_3_1_2_begin_repeat)
print(mismatched_columns_3_4_3_1_2_begin_repeat)

zwe_performance_3_4_3_1_2_begin_repeat <- zwe_survey_3_4_3_1_2_begin_repeat %>%
  select(all_of(zwe_performance_columns_3_4_3_1_2_begin_repeat))%>%
  mutate_all(as.character)

view(dfSummary(zwe_performance_3_4_3_1_2_begin_repeat))

# Identify columns with only NA values
na_columns_3_4_3_1_2_begin_repeat <- colSums(is.na(zwe_performance_3_4_3_1_2_begin_repeat)) == nrow(zwe_performance_3_4_3_1_2_begin_repeat)
na_columns_3_4_3_1_2_begin_repeat

# Remove columns with only NA values
zwe_performance_3_4_3_1_2_begin_repeat <- zwe_performance_3_4_3_1_2_begin_repeat[, !na_columns_3_4_3_1_2_begin_repeat]

view(dfSummary(zwe_performance_3_4_3_1_2_begin_repeat))

result_3_4_3_1_2_begin_repeat <- zwe_performance_3_4_3_1_2_begin_repeat%>%
  gather(key = "name_question", value = "name_choice", -kobo_farmer_id, -country,-sheet_id,-index,-parent_table_name,-parent_index)%>%
  perform_left_join(performance_choices,.)%>%
  mutate(name_question_recla= name_question)

##_3_4_2_2_2_begin_repeat: Livestock production 1 ----
zwe_performance_columns_3_4_2_2_2_begin_repeat <- intersect(performance_questions_columns, colnames(zwe_survey_3_4_2_2_2_begin_repeat))
zwe_performance_columns_3_4_2_2_2_begin_repeat
performance_questions_columns
mismatched_columns_3_4_2_2_2_begin_repeat <- setdiff(performance_questions_columns, zwe_performance_columns_3_4_2_2_2_begin_repeat)
print(mismatched_columns_3_4_2_2_2_begin_repeat)

zwe_performance_3_4_2_2_2_begin_repeat <- zwe_survey_3_4_2_2_2_begin_repeat %>%
  select(all_of(zwe_performance_columns_3_4_2_2_2_begin_repeat))%>%
  mutate_all(as.character)

names(zwe_performance_3_4_2_2_2_begin_repeat)
view(dfSummary(zwe_performance_3_4_2_2_2_begin_repeat))

# Identify columns with only NA values
na_columns_3_4_2_2_2_begin_repeat <- colSums(is.na(zwe_performance_3_4_2_2_2_begin_repeat)) == nrow(zwe_performance_3_4_2_2_2_begin_repeat)
na_columns_3_4_2_2_2_begin_repeat

# Remove columns with only NA values
zwe_performance_3_4_2_2_2_begin_repeat <- zwe_performance_3_4_2_2_2_begin_repeat[, !na_columns_3_4_2_2_2_begin_repeat]

view(dfSummary(zwe_performance_3_4_2_2_2_begin_repeat))

result_3_4_2_2_2_begin_repeat <- zwe_performance_3_4_2_2_2_begin_repeat%>%
  gather(key = "name_question", value = "name_choice", -kobo_farmer_id, -country,-sheet_id,-index,-parent_table_name,-parent_index)%>%
  perform_left_join(performance_choices,.)%>%
  mutate(name_question_recla= name_question)
#mutate(name_question_recla = str_remove(name_question_recla, "/.*"))

##_3_4_2_2_6_begin_repeat: Livestock production 2 ----
zwe_performance_columns_3_4_2_2_6_begin_repeat <- intersect(performance_questions_columns, colnames(zwe_survey_3_4_2_2_6_begin_repeat))
zwe_performance_columns_3_4_2_2_6_begin_repeat
performance_questions_columns
mismatched_columns_3_4_2_2_6_begin_repeat <- setdiff(performance_questions_columns, zwe_performance_columns_3_4_2_2_6_begin_repeat)
print(mismatched_columns_3_4_2_2_6_begin_repeat)

zwe_performance_3_4_2_2_6_begin_repeat <- zwe_survey_3_4_2_2_6_begin_repeat %>%
  select(all_of(zwe_performance_columns_3_4_2_2_6_begin_repeat))%>%
  mutate_all(as.character)

names(zwe_performance_3_4_2_2_6_begin_repeat)
view(dfSummary(zwe_performance_3_4_2_2_6_begin_repeat))

# Identify columns with only NA values
na_columns_3_4_2_2_6_begin_repeat <- colSums(is.na(zwe_performance_3_4_2_2_6_begin_repeat)) == nrow(zwe_performance_3_4_2_2_6_begin_repeat)
na_columns_3_4_2_2_6_begin_repeat

# Remove columns with only NA values
zwe_performance_3_4_2_2_6_begin_repeat <- zwe_performance_3_4_2_2_6_begin_repeat[, !na_columns_3_4_2_2_6_begin_repeat]

view(dfSummary(zwe_performance_3_4_2_2_6_begin_repeat))

result_3_4_2_2_6_begin_repeat <- zwe_performance_3_4_2_2_6_begin_repeat%>%
  gather(key = "name_question", value = "name_choice", -kobo_farmer_id, -country,-sheet_id,-index,-parent_table_name,-parent_index)%>%
  perform_left_join(performance_choices,.)%>%
  mutate(name_question_recla= name_question)

##_3_3_4_1_3_begin_repeat: Irrigation ----
zwe_performance_columns_3_3_4_1_3_begin_repeat <- intersect(performance_questions_columns, colnames(zwe_survey_3_3_4_1_3_begin_repeat))
zwe_performance_columns_3_3_4_1_3_begin_repeat
performance_questions_columns
mismatched_columns_3_3_4_1_3_begin_repeat <- setdiff(performance_questions_columns, zwe_performance_columns_3_3_4_1_3_begin_repeat)
print(mismatched_columns_3_3_4_1_3_begin_repeat)

zwe_performance_3_3_4_1_3_begin_repeat <- zwe_survey_3_3_4_1_3_begin_repeat %>%
  select(all_of(zwe_performance_columns_3_3_4_1_3_begin_repeat))%>%
  mutate_all(as.character)
names(zwe_performance_3_3_4_1_3_begin_repeat)
view(dfSummary(zwe_performance_3_3_4_1_3_begin_repeat))

# Identify columns with only NA values
na_columns_3_3_4_1_3_begin_repeat <- colSums(is.na(zwe_performance_3_3_4_1_3_begin_repeat)) == nrow(zwe_performance_3_3_4_1_3_begin_repeat)
na_columns_3_3_4_1_3_begin_repeat

# Remove columns with only NA values
zwe_performance_3_3_4_1_3_begin_repeat <- zwe_performance_3_3_4_1_3_begin_repeat[, !na_columns_3_3_4_1_3_begin_repeat]

view(dfSummary(zwe_performance_3_3_4_1_3_begin_repeat))

result_3_3_4_1_3_begin_repeat <- zwe_performance_3_3_4_1_3_begin_repeat%>%
  gather(key = "name_question", value = "name_choice", -kobo_farmer_id, -country,-sheet_id,-index,-parent_table_name,-parent_index)%>%
  perform_left_join(performance_choices,.)%>%
  mutate(name_question_recla= name_question)%>%
  mutate(name_question_recla = str_remove(name_question_recla, "/.*"))

##_3_4_1_1_7_1_begin_repeat: household members permanent workers ----
zwe_performance_columns_3_4_1_1_7_1_begin_repeat <- intersect(performance_questions_columns, colnames(zwe_survey_3_4_1_1_7_1_begin_repeat))
zwe_performance_columns_3_4_1_1_7_1_begin_repeat
performance_questions_columns
mismatched_columns_3_4_1_1_7_1_begin_repeat <- setdiff(performance_questions_columns, zwe_performance_columns_3_4_1_1_7_1_begin_repeat)
print(mismatched_columns_3_4_1_1_7_1_begin_repeat)

zwe_performance_3_4_1_1_7_1_begin_repeat <- zwe_survey_3_4_1_1_7_1_begin_repeat %>%
  select(all_of(zwe_performance_columns_3_4_1_1_7_1_begin_repeat))%>%
  mutate_all(as.character)

names(zwe_performance_3_4_1_1_7_1_begin_repeat)
view(dfSummary(zwe_performance_3_4_1_1_7_1_begin_repeat))

# Identify columns with only NA values
na_columns_3_4_1_1_7_1_begin_repeat <- colSums(is.na(zwe_performance_3_4_1_1_7_1_begin_repeat)) == nrow(zwe_performance_3_4_1_1_7_1_begin_repeat)
na_columns_3_4_1_1_7_1_begin_repeat

# Remove columns with only NA values
zwe_performance_3_4_1_1_7_1_begin_repeat <- zwe_performance_3_4_1_1_7_1_begin_repeat[, !na_columns_3_4_1_1_7_1_begin_repeat]

view(dfSummary(zwe_performance_3_4_1_1_7_1_begin_repeat))

result_3_4_1_1_7_1_begin_repeat <- zwe_performance_3_4_1_1_7_1_begin_repeat%>%
  gather(key = "name_question", value = "name_choice", -kobo_farmer_id, -country,-sheet_id,-index,-parent_table_name,-parent_index)%>%
  perform_left_join(performance_choices,.)%>%
  mutate(name_question_recla= name_question)
#mutate(name_question_recla = str_remove(name_question_recla, "/.*"))

##_3_4_1_1_7_2_begin_repeat: household members seasonal workers 1 ----
zwe_performance_columns_3_4_1_1_7_2_begin_repeat <- intersect(performance_questions_columns, colnames(zwe_survey_3_4_1_1_7_2_begin_repeat))
zwe_performance_columns_3_4_1_1_7_2_begin_repeat
performance_questions_columns
mismatched_columns_3_4_1_1_7_2_begin_repeat <- setdiff(performance_questions_columns, zwe_performance_columns_3_4_1_1_7_2_begin_repeat)
print(mismatched_columns_3_4_1_1_7_2_begin_repeat)

zwe_performance_3_4_1_1_7_2_begin_repeat <- zwe_survey_3_4_1_1_7_2_begin_repeat %>%
  select(all_of(zwe_performance_columns_3_4_1_1_7_2_begin_repeat))%>%
  mutate_all(as.character)

names(zwe_performance_3_4_1_1_7_2_begin_repeat)
view(dfSummary(zwe_performance_3_4_1_1_7_2_begin_repeat))

# Identify columns with only NA values
na_columns_3_4_1_1_7_2_begin_repeat <- colSums(is.na(zwe_performance_3_4_1_1_7_2_begin_repeat)) == nrow(zwe_performance_3_4_1_1_7_2_begin_repeat)
na_columns_3_4_1_1_7_2_begin_repeat

# Remove columns with only NA values
zwe_performance_3_4_1_1_7_2_begin_repeat <- zwe_performance_3_4_1_1_7_2_begin_repeat[, !na_columns_3_4_1_1_7_2_begin_repeat]

unique(zwe_performance_3_4_1_1_7_2_begin_repeat$merge_id2)
view(dfSummary(zwe_performance_3_4_1_1_7_2_begin_repeat))

result_3_4_1_1_7_2_begin_repeat<- zwe_performance_3_4_1_1_7_2_begin_repeat%>%
  gather(key = "name_question", value = "name_choice", -kobo_farmer_id, -country,-sheet_id,-index,-parent_table_name,-parent_index)%>%
  perform_left_join(performance_choices,.)%>%
  mutate(name_question_recla= name_question)

##_3_4_1_2_7_2_1_begin_repeat : household members seasonal workers 2 ----
zwe_performance_columns_3_4_1_2_7_2_1_begin_repeat <- intersect(performance_questions_columns, colnames(zwe_survey_3_4_1_2_7_2_1_begin_repeat))
zwe_performance_columns_3_4_1_2_7_2_1_begin_repeat
performance_questions_columns
mismatched_columns_3_4_1_2_7_2_1_begin_repeat <- setdiff(performance_questions_columns, zwe_performance_columns_3_4_1_2_7_2_1_begin_repeat)
print(mismatched_columns_3_4_1_2_7_2_1_begin_repeat)

zwe_performance_3_4_1_2_7_2_1_begin_repeat <- zwe_survey_3_4_1_2_7_2_1_begin_repeat %>%
  select(all_of(zwe_performance_columns_3_4_1_2_7_2_1_begin_repeat))%>%
  mutate_all(as.character)

names(zwe_performance_3_4_1_2_7_2_1_begin_repeat)
unique(zwe_performance_3_4_1_2_7_2_1_begin_repeat$index)
view(dfSummary(zwe_performance_3_4_1_2_7_2_1_begin_repeat))

# Identify columns with only NA values
na_columns_3_4_1_2_7_2_1_begin_repeat <- colSums(is.na(zwe_performance_3_4_1_2_7_2_1_begin_repeat)) == nrow(zwe_performance_3_4_1_2_7_2_1_begin_repeat)
na_columns_3_4_1_2_7_2_1_begin_repeat

# Remove columns with only NA values
zwe_performance_3_4_1_2_7_2_1_begin_repeat <- zwe_performance_3_4_1_2_7_2_1_begin_repeat[, !na_columns_3_4_1_2_7_2_1_begin_repeat]

names(zwe_performance_3_4_1_2_7_2_1_begin_repeat)
view(dfSummary(zwe_performance_3_4_1_2_7_2_1_begin_repeat))

result_3_4_1_1_7_2_1_begin_repeat<- zwe_performance_3_4_1_2_7_2_1_begin_repeat%>%
  gather(key = "name_question", value = "name_choice", -kobo_farmer_id, -country,-sheet_id,-index,-parent_table_name,-parent_index)%>%
  perform_left_join(performance_choices,.)%>%
  mutate(name_question_recla= name_question)

names(result_3_4_1_1_7_2_1_begin_repeat)
unique(result_3_4_1_1_7_2_1_begin_repeat$parent_table_name)

##_3_4_1_2_1_1_begin_repeat: labour Hired/Free/Exchange Labourers permanent workers ----
zwe_performance_columns_3_4_1_2_1_1_begin_repeat <- intersect(performance_questions_columns, colnames(zwe_survey_3_4_1_2_1_1_begin_repeat))
zwe_performance_columns_3_4_1_2_1_1_begin_repeat
performance_questions_columns
mismatched_columns_3_4_1_2_1_1_begin_repeat <- setdiff(performance_questions_columns, zwe_performance_columns_3_4_1_2_1_1_begin_repeat)
print(mismatched_columns_3_4_1_2_1_1_begin_repeat)

zwe_performance_3_4_1_2_1_1_begin_repeat <- zwe_survey_3_4_1_2_1_1_begin_repeat %>%
  select(all_of(zwe_performance_columns_3_4_1_2_1_1_begin_repeat))%>%
  mutate_all(as.character)

view(dfSummary(zwe_performance_3_4_1_2_1_1_begin_repeat))

# Identify columns with only NA values
na_columns_3_4_1_2_1_1_begin_repeat <- colSums(is.na(zwe_performance_3_4_1_2_1_1_begin_repeat)) == nrow(zwe_performance_3_4_1_2_1_1_begin_repeat)
na_columns_3_4_1_2_1_1_begin_repeat

# Remove columns with only NA values
zwe_performance_3_4_1_2_1_1_begin_repeat <- zwe_performance_3_4_1_2_1_1_begin_repeat[, !na_columns_3_4_1_2_1_1_begin_repeat]

view(dfSummary(zwe_performance_3_4_1_2_1_1_begin_repeat))

result_3_4_1_2_1_1_begin_repeat<- zwe_performance_3_4_1_2_1_1_begin_repeat%>%
  gather(key = "name_question", value = "name_choice", -kobo_farmer_id, -country,-sheet_id,-index,-parent_table_name,-parent_index)%>%
  perform_left_join(performance_choices,.)%>%
  mutate(name_question_recla= name_question)


##_3_4_1_2_1_2_begin_repeat: labour Hired/Free/Exchange Labourers seasonal workers 1 ----
zwe_performance_columns_3_4_1_2_1_2_begin_repeat <- intersect(performance_questions_columns, colnames(zwe_survey_3_4_1_2_1_2_begin_repeat))
zwe_performance_columns_3_4_1_2_1_2_begin_repeat
performance_questions_columns
mismatched_columns_3_4_1_2_1_2_begin_repeat <- setdiff(performance_questions_columns, zwe_performance_columns_3_4_1_2_1_2_begin_repeat)
print(mismatched_columns_3_4_1_2_1_2_begin_repeat)

zwe_performance_3_4_1_2_1_2_begin_repeat <- zwe_survey_3_4_1_2_1_2_begin_repeat%>%
  select(all_of(zwe_performance_columns_3_4_1_2_1_2_begin_repeat))%>%
  mutate_all(as.character)

view(dfSummary(zwe_performance_3_4_1_2_1_2_begin_repeat))

# Identify columns with only NA values
na_columns_3_4_1_2_1_2_begin_repeat <- colSums(is.na(zwe_performance_3_4_1_2_1_2_begin_repeat)) == nrow(zwe_performance_3_4_1_2_1_2_begin_repeat)
na_columns_3_4_1_2_1_2_begin_repeat

# Remove columns with only NA values
zwe_performance_3_4_1_2_1_2_begin_repeat <- zwe_performance_3_4_1_2_1_2_begin_repeat[, !na_columns_3_4_1_2_1_2_begin_repeat]

view(dfSummary(zwe_performance_3_4_1_2_1_2_begin_repeat))

result_3_4_1_2_1_2_begin_repeat<- zwe_performance_3_4_1_2_1_2_begin_repeat%>%
  gather(key = "name_question", value = "name_choice", -kobo_farmer_id, -country,-sheet_id,-index,-parent_table_name,-parent_index)%>%
  perform_left_join(performance_choices,.)%>%
  mutate(name_question_recla= name_question)

##_3_4_1_2_1_2_1_begin_repeat: labour Hired/Free/Exchange Labourers seasonal workers 2 ----
zwe_performance_columns_3_4_1_2_1_2_1_begin_repeat <- intersect(performance_questions_columns, colnames(zwe_survey_3_4_1_2_1_2_1_begin_repeat))
zwe_performance_columns_3_4_1_2_1_2_1_begin_repeat
performance_questions_columns
mismatched_columns_3_4_1_2_1_2_1_begin_repeat <- setdiff(performance_questions_columns, zwe_performance_columns_3_4_1_2_1_2_1_begin_repeat)
print(mismatched_columns_3_4_1_2_1_2_1_begin_repeat)

zwe_performance_3_4_1_2_1_2_1_begin_repeat <- zwe_survey_3_4_1_2_1_2_1_begin_repeat%>%
  select(all_of(zwe_performance_columns_3_4_1_2_1_2_1_begin_repeat))%>%
  mutate_all(as.character)

view(dfSummary(zwe_performance_3_4_1_2_1_2_1_begin_repeat))

# Identify columns with only NA values
na_columns_3_4_1_2_1_2_1_begin_repeat <- colSums(is.na(zwe_performance_3_4_1_2_1_2_1_begin_repeat)) == nrow(zwe_performance_3_4_1_2_1_2_1_begin_repeat)
na_columns_3_4_1_2_1_2_1_begin_repeat

# Remove columns with only NA values
zwe_performance_3_4_1_2_1_2_1_begin_repeat <- zwe_performance_3_4_1_2_1_2_1_begin_repeat[, !na_columns_3_4_1_2_1_2_1_begin_repeat]

view(dfSummary(zwe_performance_3_4_1_2_1_2_1_begin_repeat))

result_3_4_1_2_1_2_1_begin_repeat<- zwe_performance_3_4_1_2_1_2_1_begin_repeat%>%
  gather(key = "name_question", value = "name_choice", -kobo_farmer_id, -country,-sheet_id,-index,-parent_table_name,-parent_index)%>%
  perform_left_join(performance_choices,.)%>%
  mutate(name_question_recla= name_question)

##_3_3_3_2_begin_repeat:  area of land per agricultural practice ----
zwe_performance_columns_3_3_3_2_begin_repeat <- intersect(performance_questions_columns, colnames(zwe_survey_3_3_3_2_begin_repeat))
zwe_performance_columns_3_3_3_2_begin_repeat

performance_questions_columns
mismatched_columns_3_3_3_2_begin_repeat <- setdiff(performance_questions_columns, zwe_performance_columns_3_3_3_2_begin_repeat)
print(mismatched_columns_3_3_3_2_begin_repeat)

zwe_performance_3_3_3_2_begin_repeat <- zwe_survey_3_3_3_2_begin_repeat %>%
  select(all_of(zwe_performance_columns_3_3_3_2_begin_repeat))%>%
  mutate_all(as.character)

names(zwe_performance_3_3_3_2_begin_repeat)
view(dfSummary(zwe_performance_3_3_3_2_begin_repeat))

# Identify columns with only NA values
na_columns_3_3_3_2_begin_repeat <- colSums(is.na(zwe_performance_3_3_3_2_begin_repeat)) == nrow(zwe_performance_3_3_3_2_begin_repeat)
na_columns_3_3_3_2_begin_repeat

# Remove columns with only NA values
zwe_performance_3_3_3_2_begin_repeat <- zwe_performance_3_3_3_2_begin_repeat[, !na_columns_3_3_3_2_begin_repeat]

view(dfSummary(zwe_performance_3_3_3_2_begin_repeat))

result_3_3_3_2_begin_repeat <- zwe_performance_3_3_3_2_begin_repeat%>%
  gather(key = "name_question", value = "name_choice", -kobo_farmer_id, -country,-sheet_id,-index,-parent_table_name,-parent_index)%>%
  perform_left_join(performance_choices,.)%>%
  mutate(name_question_recla= name_question)



 

    
  filter(!(name_question %in% c("_1_2_1_5_1")))
#,
                # Indicator: productivity_crops
                # result_3_4_3_1_2_begin_repeat,
                # Indicator: productivity_livestock
                # result_3_4_2_2_2_begin_repeat,result_3_4_2_2_6_begin_repeat,
                # Indicator:labour_productivity
                #result_3_4_1_1_7_1_begin_repeat,result_3_4_1_1_7_2_1_begin_repeat,result_3_4_1_1_7_2_begin_repeat ,
                #result_3_4_1_2_1_1_begin_repeat,result_3_4_1_2_1_2_begin_repeat,result_3_4_1_2_1_2_1_begin_repeat,
                # Indicator: biodiversity_climate_mitigation
                #result_3_3_3_2_begin_repeat,
                # Indicator: irrigation
                #result_3_3_4_1_3_begin_repeat
#)
  # Indicator: biodiversity_agrobiodiversity

mutate(name_choice = case_when(
  name_question %in% c("_3_4_3_1_1", "_3_4_2_2_2_3_calculate", "_3_4_3_4_1") & is.na(name_choice) ~ "0",
  TRUE ~ name_choice))%>%
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

#write.csv(result2,file="C:/Users/andreasanchez/OneDrive - CGIAR/Bioversity/AI/HOLPA/analysis/HOLPA/HOLPA/zwe/zwe_performance.csv",row.names=FALSE)



table(result2$label_question,result2$name_question_recla)
unique(result2$sheet_id)
names(result2)
view(dfSummary(result2))

length(unique(result2$label_question))
length(unique(result2$name_question_recla))
table( result2$name_question_recla, result2$label_question)

x<-result2%>%
  mutate(x=paste(name_question_recla,label_question,sep="_"))

sort(unique(x$x))


