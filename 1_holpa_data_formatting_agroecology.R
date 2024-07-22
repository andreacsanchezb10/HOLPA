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


#### Import data ####
# Each dataset contains a survey worksheet with the questions and responses for text, open and numeric questions, and
# a choices worksheet with the response options for multiple choice questions (single or multiple).
# These need to be imported and combined.

### Country databases ####
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
zwe.data.path <-"C:/Users/andreasanchez/OneDrive - CGIAR/Bioversity/AI/HOLPA/HOLPA_data/Zimbabwe/zimbabwe_data_clean/household_database_2024.04.18_clean.xlsx" #path: Andrea

zwe_survey_main <- read_and_process_survey_xlsx("Final HOLPA_Zimbabwe_Household", "_id", zwe.data.path,"zimbabwe","_index")%>%
  #Remove respondents that are not farmers
  filter(kobo_farmer_id!="274186917")
zwe_survey_1_4_2_7_begin_repeat <- read_and_process_survey_xlsx("_1_4_2_7_begin_repeat", "_submission__id", zwe.data.path,"zimbabwe","_index")%>% # Section: Farm production OTHER
  #Remove respondents that are not farmers
  filter(kobo_farmer_id!="274186917")

zwe_survey_3_3_3_2_begin_repeat<- read_and_process_survey_xlsx("_3_3_3_2_begin_repeat", "_submission__id", zwe.data.path,"zimbabwe","_index") # Section: area of land per agricultural practice


zwe_choices <- read_excel("C:/Users/andreasanchez/OneDrive - CGIAR/Bioversity/AI/HOLPA/HOLPA_data/Zimbabwe/Zimbabwe_monitoring/holpa_household_form.xlsx",
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


### TUNISIA -----
tun.data.path <-"C:/Users/andreasanchez/OneDrive - CGIAR/Bioversity/AI/HOLPA/HOLPA_data/Tunisia/tunisia_data_clean/tun_holpa_household_survey_clean.xlsx" #path: Andrea
tun_survey_main <- read_and_process_survey_xlsx("HOLPA_Tunisia_household_surv", "_id", tun.data.path,"tunisia","_index")%>%
  #Remove respondents that did not wanted to complete the survey
  filter(consent_2!="No")

# Section: Farm production OTHER #Tunisia doesn't have this section

tun_survey_3_3_3_2_begin_repeat<- read_and_process_survey_xlsx("_3_3_3_2_begin_repeat", "_submission__id", tun.data.path,"tunisia","_index") # Section: area of land per agricultural practice

tun_choices <- read_excel("C:/Users/andreasanchez/OneDrive - CGIAR/Bioversity/AI/HOLPA/HOLPA_data/Tunisia/tunisia_data_clean/holpa_household_form_clean.xlsx",
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


### KENYA ----
ken.data.path <-"C:/Users/andreasanchez/OneDrive - CGIAR/Bioversity/AI/HOLPA/HOLPA_data/Kenya/kenya_data_clean/holpa_household_name_2024.06.23.xlsx" #path: Andrea

ken_survey_main <- read_and_process_survey_xlsx("Holpa_global_household_surve", "_id", ken.data.path,"kenya","_index")%>%
  #Remove respondents that did not wanted to complete the survey
  filter(consent_2!="No")

ken_survey_1_4_2_7_begin_repeat <- read_and_process_survey_xlsx("_1_4_2_7_begin_repeat", "_submission__id", ken.data.path,"kenya","_index") # Section: Crop production
ken_survey_3_3_3_2_begin_repeat<- read_and_process_survey_xlsx("_3_3_3_2_begin_repeat", "_submission__id", ken.data.path,"kenya","_index") # Section: area of land per agricultural practice

ken_choices <- read_excel("C:/Users/andreasanchez/OneDrive - CGIAR/Bioversity/AI/HOLPA/HOLPA_data/Kenya/kenya_monitoring/holpa_household_form.xlsx",
                          sheet = "choices")%>%
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
  right_join(global_survey,by="list_name",relationship="many-to-many")

#### AGROECOLOGY MODULE #### -----
fun_agroecology_survey<-function(global_survey) {
  agroecology_survey <-  global_survey %>%
    filter(str_detect(module, "agroecology"))%>%
    mutate(module= "agroecology")
  
  #"3_soil_health/6_synergy/environmental" this question is part of two indicators
  duplicate_rows <-  agroecology_survey%>% filter(str_detect(subindicator, "3_soil_health/6_synergy/environmental"))
  duplicate_rows$indicator <- "6_synergy"
  agroecology_survey <- rbind(agroecology_survey, duplicate_rows)
  
  #"3_soil_health/6_synergy/environmental" this question is part of two indicators
  duplicate_rows <-  agroecology_survey%>% filter(str_detect(subindicator, "4_animal_health/6_synergy"))
  duplicate_rows$indicator <- "6_synergy"
  agroecology_survey <- rbind(agroecology_survey, duplicate_rows)
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

 agroecology_survey <- agroecology_survey %>%
   mutate(subindicator=indicator)%>%
   rename(theme = indicator,
          indicator = subindicator)%>%
   filter(!str_detect(indicator, "end_repeat"))
 
 return(agroecology_survey)
}
 
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
    filter(country=="zimbabwe")
  
  # Left join for "select_one" for country== "tun" (Tunisia downloaded the database with label_choices)
  select_one2 <- gathered_data  %>%
    left_join(select(agroecology_choices,
                     c(name_question, name_choice, module, theme, indicator, label_choice, label_question, type, type_question, list_name)), 
              by = c("name_question"="name_question", "name_choice"="label_choice"))%>%
    dplyr::rename("label_choice"="name_choice")%>%
    dplyr::rename("name_choice"="name_choice.y")%>%
    filter(type_question=="select_one")%>%
    filter(country=="tunisia")
  
  
  result<- rbind(continuous,select_multiple,select_one1,select_one2)
  
  
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

## AGROECOLOGY DATA BY COUNTRY -----
# Zimbabwe -----
zwe_agroecology_data<-fun_agroecology_data(zwe_global_choices,
                                           zwe_survey_main,  ## Main survey 
                                           zwe_survey_1_4_2_7_begin_repeat, ## _1_4_2_7_begin_repeat: Other on-farm product Farm characteristics 
                                           zwe_survey_3_3_3_2_begin_repeat # Section: area of land per agricultural practice
                                           
)
filter(
  #theme=="1_recycling"
  #theme=="2_input_reduction"
  #theme=="3_soil_health"
  #theme=="5_biodiversity"
  theme=="6_synergy"
  
)

# Tunisia-----
## Tunisia doesn't have this section _1_4_2_7_begin_repeat: Other on-farm product Farm characteristics
tun_agroecology_data<-rbind(
  fun_agroecology_main(tun_global_choices, tun_survey_main),
  fun_agroecology_begin_repeat(tun_global_choices, tun_survey_3_3_3_2_begin_repeat))
  
  filter(
    #theme=="1_recycling"
    #theme=="2_input_reduction"
 #theme=="3_soil_health"
    #theme=="4_animal_health"
    #theme=="5_biodiversity"
    #theme=="6_synergy"
    #theme=="7_economic_diversification"
    #theme=="8_knowledge"
    #theme=="9_social_values"
    #theme=="10_fairness"
    theme=="11_connectivity"
    #theme=="12_governance"
    #theme=="13_participation"
)


## If the farmers doesn't know the answer put 9999-----
  result2<- tun_agroecology_data%>%
  
    #  result2<- zwe_agroecology_data%>%
  
  #Indicator: 5_biodiversity
  mutate(name_question_recla  = case_when(
    name_question %in% c("c1", "c2", "c3", "c4", "c5", "c6", "c7", "c8","c9", "c10", "c11", "c12", "c13", "c14", "c15", "c16", "c17", "c18", "c19", "c20")~"_3_4_3_1_1_2",
    name_question %in% c("l1", "l2", "l3", "l4", "l5", "l6", "l7", "l8","l9","l10") ~ "_3_4_3_3_1",
    TRUE ~ name_question_recla))%>%
    filter(name_question!="_3_4_3_3_1/other")%>%
    mutate(label_choice= case_when(
      name_question_recla %in% c("_3_4_3_1_1_2","_3_4_3_3_1")~ name_choice,
      TRUE ~label_choice))%>%
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
    name_question %in% c("_1_4_1_1_1", "_1_4_1_1_2", "_1_4_1_1_3","_3_4_2_1_1","_3_4_2_2_1_1","_3_4_2_2_1_2")&country== "zimbabwe"~"in acres",
    name_question %in% c("_1_4_1_1_1", "_1_4_1_1_2", "_1_4_1_1_3","_3_4_2_1_1","_3_4_2_2_1_1","_3_4_2_2_1_2")&country== "tunisia"~"in hectares",
    TRUE ~ label_choice
  ))

  
sort(unique(result2$indicator))
sort(unique(result2$name_question_recla))

sort(unique(result2$label_question))
sort(unique(result2$name_question))

write.csv(result2,file="C:/Users/andreasanchez/OneDrive - CGIAR/Bioversity/AI/HOLPA/analysis/HOLPA/HOLPA/zwe/zwe_agroecology_format.csv",row.names=FALSE)
write.csv(result2,file="C:/Users/andreasanchez/OneDrive - CGIAR/Bioversity/AI/HOLPA/analysis/HOLPA/HOLPA/tun/tun_agroecology_format.csv",row.names=FALSE)



