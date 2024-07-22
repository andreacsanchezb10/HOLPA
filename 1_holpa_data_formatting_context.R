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
global.data.path <-"C:/Users/andreasanchez/OneDrive - CGIAR/Bioversity/AI/HOLPA/analysis/HOLPA/HOLPA/" #Andrea

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

### ZIMBABWE ----
zwe.data.path <-"C:/Users/andreasanchez/OneDrive - CGIAR/Bioversity/AI/HOLPA/HOLPA_data/Zimbabwe/zimbabwe_data_clean/zwe_holpa_household_survey_clean.xlsx" #path: Andrea

zwe_survey_main <- read_and_process_survey_xlsx("Final HOLPA_Zimbabwe_Household", "_id", zwe.data.path,"zimbabwe","_index")%>%
  #Remove respondents that are not farmers
  filter(kobo_farmer_id!="274186917")
zwe_survey_1_4_2_7_begin_repeat <- read_and_process_survey_xlsx("_1_4_2_7_begin_repeat", "_submission__id", zwe.data.path,"zimbabwe","_index")%>% # Section: Crop production
filter(kobo_farmer_id!="274186917")

zwe_choices <- read_excel("C:/Users/andreasanchez/OneDrive - CGIAR/Bioversity/AI/HOLPA/HOLPA_data/Zimbabwe/zimbabwe_data_clean/zwe_holpa_household_form_clean.xlsx",
                          sheet = "choices")%>%
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
  right_join(global_survey,by="list_name",relationship="many-to-many")

names(zwe_global_choices)

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
tun.data.path <-"C:/Users/andreasanchez/OneDrive - CGIAR/Bioversity/AI/HOLPA/HOLPA_data/Tunisia/tunisia_data_clean/tun_holpa_household_survey_clean.xlsx" #path: Andrea

tun_survey_main <- read_and_process_survey_xlsx("HOLPA_Tunisia_household_surv", "_id", tun.data.path,"tunisia","_index")%>%
  #Remove respondents that did not wanted to complete the survey
  filter(consent_2!="No")

#does not exist for tunisia tun_survey_1_4_2_7_begin_repeat <- read_and_process_survey_xlsx("_1_4_2_7_begin_repeat", "_submission__id", tun.data.path,"tunisia","_index") # Section: Crop production


tun_choices <- read_excel("C:/Users/andreasanchez/OneDrive - CGIAR/Bioversity/AI/HOLPA/HOLPA_data/Tunisia/tunisia_data_clean/tun_holpa_household_form_clean.xlsx",
                          sheet = "choices")%>%
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
  right_join(global_survey,by="list_name",relationship="many-to-many")

#### CONTEX MODULE ####
fun_context_survey<- function(global_survey) {
  context_survey <-  global_survey %>%
  filter(str_detect(indicator, "context"))%>%
  mutate(indicator= "context")
  
  #"production_end_use/production_systems" these question is part of two indicators
  duplicate_rows <-  context_survey%>% filter(str_detect(subindicator, "production_end_use/production_systems"))
  duplicate_rows$subindicator <- "production_end_use"
  context_survey <- rbind(context_survey, duplicate_rows)

  context_survey$subindicator[str_detect(context_survey$subindicator,"respondent_characteristics")]<- "respondent_characteristics"
  context_survey$subindicator[str_detect(context_survey$subindicator,"household_characteristic")]<- "household_characteristics"
  context_survey$subindicator[str_detect(context_survey$subindicator,"context_all")]<- "context_all"
  context_survey$subindicator[str_detect(context_survey$subindicator,"farm_characteristic")]<- "farm_characteristics"
  context_survey$subindicator[str_detect(context_survey$subindicator,"inputs")]<- "inputs"
  context_survey$subindicator[str_detect(context_survey$subindicator,"production_systems")]<- "production_systems"
  context_survey$subindicator[str_detect(context_survey$subindicator,"production_end_use")]<- "production_end_use"
  context_survey$subindicator[str_detect(context_survey$subindicator,"household_labour")]<- "household_labour"
  context_survey$subindicator[str_detect(context_survey$subindicator,"credit_access")]<- "credit_access"
  context_survey$subindicator[str_detect(context_survey$subindicator,"income")]<- "income"
  context_survey$subindicator[str_detect(context_survey$subindicator,"household_labour")]<- "household_labour"
  context_survey$subindicator[str_detect(context_survey$subindicator,"training")]<- "training"
  context_survey$subindicator[str_detect(context_survey$subindicator,"education")]<- "education"
  context_survey$subindicator[str_detect(context_survey$subindicator,"literacy")]<- "literacy"
  context_survey$subindicator[str_detect(context_survey$subindicator,"membership")]<- "membership"
  context_survey$subindicator[str_detect(context_survey$subindicator,"accessibility")]<- "accessibility"
  
  context_survey$indicator[str_detect(context_survey$indicator,"farm_characteristics")]<- "farm_characteristics"
  context_survey$indicator[str_detect(context_survey$indicator,"farm_characteristic")]<- "farm_characteristics"
  context_survey$indicator[str_detect(context_survey$indicator,"household_characteristic")]<- "household_characteristics"
  context_survey$indicator[str_detect(context_survey$indicator,"household_characteristics")]<- "household_characteristics"
  context_survey$indicator[str_detect(context_survey$indicator,"respondent_characteristics")]<- "respondent_characteristics"
  context_survey$indicator[str_detect(context_survey$indicator,"inputs")]<- "inputs"
  context_survey$indicator[str_detect(context_survey$indicator,"context_all")]<- "context_all"
  
  context_survey <-context_survey%>%
    rename(theme = indicator,
           indicator = subindicator)%>%
  filter(!str_detect(indicator, "end_repeat"))
  
  return(context_survey)
}

fun_context_choices<- function(country_global_choices) {
  # Filter and mutate the data frame
  context_choices <- country_global_choices %>%
  filter(str_detect(module, "context"))%>%
  mutate(module= "context")

  context_choices$subindicator[str_detect(context_choices$subindicator,"respondent_characteristics")]<- "respondent_characteristics"
  context_choices$subindicator[str_detect(context_choices$subindicator,"household_characteristic")]<- "household_characteristics"
  context_choices$subindicator[str_detect(context_choices$subindicator,"context_all")]<- "context_all"
  context_choices$subindicator[str_detect(context_choices$subindicator,"farm_characteristic")]<- "farm_characteristics"
  context_choices$subindicator[str_detect(context_choices$subindicator,"inputs")]<- "inputs"
  context_choices$subindicator[str_detect(context_choices$subindicator,"production_systems")]<- "production_systems"
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
    filter(country=="zimbabwe")
  
  # Left join for "select_one" for country== "tun"
  select_one2 <- gathered_data  %>%
    left_join(select(context_choices,
                     c(name_question, name_choice, module, theme, indicator, label_choice, label_question, type, type_question, list_name)), 
              by = c("name_question"="name_question", "name_choice"="label_choice"))%>%
    dplyr::rename("label_choice"="name_choice")%>%
    dplyr::rename("name_choice"="name_choice.y")%>%
    filter(type_question=="select_one")%>%
    filter(country=="tunisia")
  
  result<- rbind(continuous,select_multiple,select_one1,select_one2)
  
  return(result)
}

### Function to get answers from the following sections ---- 
## Main survey ----
fun_context_main<- function(country_global_choices,country_survey_main){
  country_context_choices<-  fun_context_choices(country_global_choices)
  country_context_question_columns<- fun_context_questions_columns(country_context_choices)
  
  country_context_columns <- intersect(country_context_question_columns, colnames(country_survey_main))
  mismatched_columns <- setdiff(country_context_question_columns, country_context_columns)
  
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
  return(result_main_survey)
}

## begin_repeat: questions located in begin_repeat groups  ---- 
fun_context_begin_repeat<- function(country_global_choices,country_survey_begin_repeat){
  country_context_choices<-  fun_context_choices(country_global_choices)
  country_context_question_columns<- fun_context_questions_columns(country_context_choices)
  
  country_context_columns_begin_repeat <- intersect(country_context_question_columns, colnames(country_survey_begin_repeat))
  
  country_context_begin_repeat <- country_survey_begin_repeat %>%
    select(all_of(country_context_columns_begin_repeat))%>%
    mutate_all(as.character)
  
  # Identify columns with only NA values
  na_columns_begin_repeat <- colSums(is.na(country_context_begin_repeat)) == nrow(country_context_begin_repeat)
  
  # Remove columns with only NA values
  country_context_begin_repeat <- country_context_begin_repeat[, !na_columns_begin_repeat]
  
  result_begin_repeat <- country_context_begin_repeat%>%
    gather(key = "name_question", value = "name_choice", -kobo_farmer_id, -country,-sheet_id,-index,-parent_table_name,-parent_index)%>%
    fun_context_left_join(country_context_choices,.)%>%
    mutate(name_question_recla= name_question)
  return(result_begin_repeat)
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


## CONTEXT DATA FORMAT BY COUNTRY -----
# Zimbabwe -----
zwe_context_data<-fun_context_data(zwe_global_choices,
                                           zwe_survey_main,  ## Main survey 
                                           zwe_survey_1_4_2_7_begin_repeat  ##production_end_use for OTHER NON-FARM PRODUCT
                                          
)%>%

  filter(
    indicator== "membership")
    theme== "climate_perception")

                                               
             
[9] "climate_resilience_adaptative_capacity"  "hh_head_relation"                                             
                                                                                      
                                           
[29] "name"                                   "personal_factors"                       "primary_occupation"                     "production_end_use"                    
[33] "production_end_use/11_connectivity"     "production_systems"                     "project_involvement"                    "secondary_occupation"                  
[37] "societal_factor"                        "structure"                              "training"                          


         "context_all"                "economic"       "farm_characteristics"            "farm_characteristics"       "general"                    "household_characteristics" 
[7] "inputs"                     "motivations_agroecology"    "respondent_characteristics"


sort(unique(zwe_context_data$indicator))
# Tunisia-----
tun_context_data<-fun_context_data(tun_global_choices,
                                           tun_survey_main, ## Main survey 
                                           tun_survey_1_4_2_7_begin_repeat,  ##production_end_use for OTHER NON-FARM PRODUCT  
)%>%
  filter(
    indicator==   "climate_resilience_assets" )

sort(unique(tun_context_data$indicator))

## If the farmers doesn't know the answer put 9999-----
result2<- zwe_context_data%>%
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
  
  #all indicators
  mutate(name_question_recla = case_when(
    type_question == "select_multiple"~str_replace(name_question_recla, "/.*", ""),
    TRUE ~ name_question_recla))%>%
    mutate(name_choice = case_when(
    type_question == "select_multiple"~ sub("^.*/", "", name_question), # replace name_question by the type of energy
    TRUE ~ name_choice))%>%
  # Remove rows name_choice == NA
  filter(!is.na(name_choice))

sort(unique(result2$label_question))
sort(unique(result2$name_question))
sort(unique(result2$name_question_recla))
sort(unique(result2$name_choice)) 
sort(unique(result2$theme)) 



  mutate(
    name_question_recla = case_when(
      #Indicator: "hh_head_relation"
      name_question_recla== "_1_2_1_5_1" ~ "_1_2_1_5",
      
      
      #Indicator: "membership"
      name_question_recla=="_2_3_1_1_1"~"_2_3_1_1/other",
      #Indicator: "primary_occupation"
      name_question_recla=="_1_2_1_13_1_1"~ "_1_2_1_13_1",
      #Indicator: "secondary_occupation"
      name_question_recla=="_1_2_1_13_2_2_1"~ "_1_2_1_13_2_2/other",
      #Indicator: "inputs"
      str_detect(name_question_recla,"_1_4_3_1/")~"_1_4_3_1",
      str_detect(name_question_recla, "_1_4_3_5/")~"_1_4_3_5",
      # Indicator: "production_systems"
      str_detect(name_question_recla,"_1_4_2_1/")~ "_1_4_2_1",
      name_question_recla=="_1_4_2_7_calculate"~ "_1_4_2_1",
      # Indicator:climate_rainfall_timing
      str_detect(name_question_recla,"_4_2_1_2_1/")~"_4_2_1_2_1",
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
    # Indicator: "production_systems"
    name_question %in%c("_1_4_2_7_calculate")~"other",
    
    TRUE ~ label_choice))%>%
  filter(!is.na(name_choice))%>%
  #filter(!(name_question%in% c("_1_2_1_5_1","_1_2_1_6_1","_3_2_1_3_1_1","_3_2_1_3_2_1","_1_3_1","_2_3_1_1_1",
  #                             "_1_2_1_13_1_1","_1_2_1_13_2_2_1","_1_2_1_15_1","_3_2_1_2_1",
  #                            "_1_4_3_2_2","_1_4_3_2_3","_1_4_3_3_2","_1_4_3_3_3","_1_4_3_4_2",
  #                            "_1_4_3_4_3","_1_4_3_6_4","_1_4_3_6_2","_1_4_3_6_3","_1_4_3_6_1_calculate",
  #                            "_4_1_1_4_4_1","_1_4_2_2_6_1","_1_4_2_3_7_1",
  #                            "_3_4_1_1_4_1","_3_4_1_1_6_2","_3_4_1_1_6_1","_3_4_1_1_5_2","_3_4_1_1_5_1","_3_4_1_1_4_2","_3_4_1_1_3_2",
  #                            "_3_4_1_1_3_1") & is.na(name_choice)))%>%
  filter(!(str_detect(name_question, "hh_photo") & is.na(name_choice))) #Remove the rows with **Specify other:** == NA 

write.csv(zwe_context_all,file="C:/Users/andreasanchez/OneDrive - CGIAR/Bioversity/AI/HOLPA/analysis/HOLPA/HOLPA/zwe/zwe_context_format.csv",row.names=FALSE)




  
  
  
#___________________________
result_zwe_context <- zwe_context%>%
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
names(result_zwe_context)
sort(unique(result_zwe_context$name_choice))

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

result_zwe_context_1_4_2_7_begin_repeat <- zwe_context_1_4_2_7_begin_repeat%>%
  gather(key = "name_question", value = "name_choice", -kobo_farmer_id, -country,-sheet_id,-index,-parent_table_name,-parent_index)%>%
  context_left_join(context_choices,.)%>%
  mutate(name_question_recla= name_question)


# Combine all----
zwe_context_all<- rbind(
  result_zwe_context,
  # Indicator: production_end_use for OTHER NON-FARM PRODUCT
  result_zwe_context_1_4_2_7_begin_repeat
   )%>%

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
      # Indicator: "production_systems"
      str_detect(name_question_recla,"_1_4_2_1/")~ "_1_4_2_1",
      name_question_recla=="_1_4_2_7_calculate"~ "_1_4_2_1",
      # Indicator:climate_rainfall_timing
      str_detect(name_question_recla,"_4_2_1_2_1/")~"_4_2_1_2_1",
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
    # Indicator: "production_systems"
    name_question %in%c("_1_4_2_7_calculate")~"other",
    
    TRUE ~ label_choice))%>%
  filter(!is.na(name_choice))%>%
  #filter(!(name_question%in% c("_1_2_1_5_1","_1_2_1_6_1","_3_2_1_3_1_1","_3_2_1_3_2_1","_1_3_1","_2_3_1_1_1",
  #                             "_1_2_1_13_1_1","_1_2_1_13_2_2_1","_1_2_1_15_1","_3_2_1_2_1",
  #                            "_1_4_3_2_2","_1_4_3_2_3","_1_4_3_3_2","_1_4_3_3_3","_1_4_3_4_2",
  #                            "_1_4_3_4_3","_1_4_3_6_4","_1_4_3_6_2","_1_4_3_6_3","_1_4_3_6_1_calculate",
  #                            "_4_1_1_4_4_1","_1_4_2_2_6_1","_1_4_2_3_7_1",
  #                            "_3_4_1_1_4_1","_3_4_1_1_6_2","_3_4_1_1_6_1","_3_4_1_1_5_2","_3_4_1_1_5_1","_3_4_1_1_4_2","_3_4_1_1_3_2",
  #                            "_3_4_1_1_3_1") & is.na(name_choice)))%>%
  filter(!(str_detect(name_question, "hh_photo") & is.na(name_choice))) #Remove the rows with **Specify other:** == NA 
  
write.csv(zwe_context_all,file="C:/Users/andreasanchez/OneDrive - CGIAR/Bioversity/AI/HOLPA/analysis/HOLPA/HOLPA/zwe/zwe_context_format.csv",row.names=FALSE)

sort(unique(result2$indicator))

sort(unique(result2$label_question))
sort(unique(result2$name_question))
sort(unique(result2$name_question_recla))
sort(unique(result2$name_choice)) 
sort(unique(result2$theme)) 

