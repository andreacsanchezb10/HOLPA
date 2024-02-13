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
                                                           if_else(str_detect(indicator, "7_economic_diversification"), "7_economic_diversification",
                                                                   if_else(str_detect(indicator, "8_knowledge"), "8_knowledge",
                                                                           if_else(str_detect(indicator,"9_social_values"),"9_social_values",
                                                                                   if_else(str_detect(indicator,"10_fairness"),"10_fairness",
                                                                                           if_else(str_detect(indicator,"11_connectivity"),"11_connectivity",
                                                                   if_else(str_detect(indicator, "12_governance"), "12_governance",
                                                                           if_else(str_detect(indicator, "13_participation"), "13_participation",
                                                                                   indicator)))))))))))))%>%


  mutate(type_question = case_when(
    name_question %in% c("_2_9_1_1","_2_4_1","_2_10_1_2") ~ "count",
    TRUE ~ type_question))

agroecology_options <- global_choices%>%
  select(-subindicator)%>%
  #rbind(read_excel("HOLPA_global_household_survey_20231204_mapped_to_indicators.xlsx",sheet = "agroecology_look_up"))%>%
  mutate(module= "agroecology")%>%
  mutate(indicator=if_else(str_detect(indicator, "1_recycling"),"1_recycling",
                           if_else(str_detect(indicator, "2_input_reduction"),"2_input_reduction",
                                   if_else(str_detect(indicator, "3_soil_health"), "3_soil_health",
                                           if_else(str_detect(indicator, "4_animal_health"), "4_animal_health",
                                                   if_else(str_detect(indicator, "5_biodiversity"), "5_biodiversity",
                                                           
                                                   if_else(str_detect(indicator, "7_economic_diversification"), "7_economic_diversification",
                                                   if_else(str_detect(indicator, "8_knowledge"), "8_knowledge",
                                                   if_else(str_detect(indicator,"9_social_values"),"9_social_values",
                                                   if_else(str_detect(indicator,"10_fairness"),"10_fairness",
                                                           if_else(str_detect(indicator,"11_connectivity"),"11_connectivity",
                                                                   if_else(str_detect(indicator, "12_governance"), "12_governance",
                                                                           if_else(str_detect(indicator, "13_participation"), "13_participation",
                                                                                   
                                                                           indicator)))))))))))))%>%
  mutate(name_question_choice= if_else(type_question=="select_multiple",
                                       paste(name_question,"/",name_choice, sep=""),
                                        name_question))%>%
  mutate(type_question = case_when(
    name_question %in% c("_2_9_1_1","_2_4_1","_2_10_1_2") ~ "count",
    TRUE ~ type_question))

  
names(agrocology_choices)

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
  rename("x_2_9_1_1"="_2_9_1_1/",
         "x_2_4_1"="_2_4_1/",
         "x_2_10_1_2"="_2_10_1_2/")%>%
  mutate(across(c(x_2_9_1_1,x_2_4_1,x_2_10_1_2), ~replace_na(., 0)))%>%
  rename("_2_9_1_1"="x_2_9_1_1",
       "_2_4_1"="x_2_4_1",
       "_2_10_1_2"="x_2_10_1_2")%>%
  select(-contains("/"))%>%
  
  gather(key = "name_question", value = "name_choice",-kobo_id, -country)%>%
  dplyr::left_join(select(agrocology_choices,c(name_question,module,indicator,name_choice,score_agroecology_module,label_choice,
                                               label_question,type,type_question,list_name)), 
                   by= c("name_question"="name_question",
                         "name_choice"="name_choice"))%>%
  select("module", "indicator","country",
         "kobo_id","name_question", "type", "type_question"  , "list_name" , 
         "label_question","label_choice", "name_choice" ,"score_agroecology_module")%>%
  filter(name_question=="_3_4_3_1_1")

sort(unique(agroecology_zimbabwe$name_question))
sort(unique(agroecology_zimbabwe$name_choice))  
sort(unique(agroecology_zimbabwe$indicator))



