#library(openxlsx)
library(tidyr)
library(tidyverse)
library(readxl)
library(dplyr)

global_form <- read_excel(
  "C:/Users/andreasanchez/OneDrive - CGIAR/Bioversity/AI/HOLPA/analysis/HOLPA/HOLPA_global_household_survey_20231204_mapped_to_indicators.xlsx",
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
  mutate(type_q = ifelse(substr(type,1,10)=="select_one","select_one",
                         ifelse(substr(type,1,10)=="select_mul","select_multiple",type)))%>%
  mutate(list_name = if_else(type_q== "select_one"|type_q== "select_multiple", 
                             str_replace(.$type, paste0(".*", .$type_q), ""),NA))%>%
  mutate(list_name = str_replace_all(list_name, " ", ""))


global_choices <- read_excel(
  "C:/Users/andreasanchez/OneDrive - CGIAR/Bioversity/AI/HOLPA/analysis/HOLPA/HOLPA_global_household_survey_20231204_mapped_to_indicators.xlsx",
  sheet = "choices")%>%
  select("list_name","name","label::English ((en))","score_agroecology_module")%>%
  rename("label_choice" = "label::English ((en))")%>%
  rename("name_choice" = "name")

  
######Agroecology module
agroecology<-  global_form%>%
  filter(str_detect(module, "agroecology"))%>%
  select(-subindicator)%>%
  mutate(module= "agroecology")%>%
  mutate(indicator=if_else(str_detect(indicator, "1_recycling"),"1_recycling",
                           if_else(str_detect(indicator, "2_input_reduction"),"2_input_reduction",
                                   indicator)))
  left_join(global_choices,by= "list_name")



## 1- Recycling structure
recycling<- agroecology%>%
  filter(str_detect(indicator, "1_recycling"))%>%
  select(label_question, name_question)%>%
  spread(key = name_question, value = label_question)%>%
  mutate("kobo_id"="kobo_farmer_id",
         "country"="country_name")
sort(unique(recycling$type_q))  

## 2- Input reduction structure
input_reduction<- agroecology%>%
  filter(str_detect(indicator, "2_input_reduction"))%>%
  left_join(global_choices,by= "list_name")%>%
  mutate(name_question= if_else(type_q=="select_multiple",
                                paste(name_question,"/",name_choice, sep=""),
                                name_question))%>%
  dplyr::select(label_question, name_question)%>%
  distinct(., .keep_all = TRUE)%>%
  spread(key = name_question, value = label_question)
  
sort(unique(input_reduction$type_q))  


#####Prueba  
zimbabwe_survey <- read_excel(
    "C:/Users/andreasanchez/OneDrive - CGIAR/Bioversity/AI/HOLPA/analysis/HOLPA/zimbabwe_household_name_2023.11.27.xlsx",
    sheet = "Final HOLPA_Zimbabwe_Household") %>%
  rename("kobo_id"="_id")%>%
  rename("country"="_1_2_1_3")


recycling_columns <- colnames(recycling)

recycling_zimbabwe <- select(zimbabwe_survey, all_of(recycling_columns))%>%
  gather(key = "name_question", value = "name_answer",-kobo_id, -country)%>%
  left_join(agroecology, by="name_question")

length(unique(recycling_zimbabwe$kobo_id))
improve_indicator <- function(row, col_0, col_1, col_2, col_3) {
  case_when(
    row[col_0] == 0 & row[col_1] == 1 & row[col_2] == 0 & row[col_3] == 0 ~ 1,
    row[col_0] == 0 & row[col_1] == 0 & row[col_2] == 0 & row[col_3] == 1 ~ 5,
    row[col_0] == 0 & row[col_1] == 0 & row[col_2] == 1 & row[col_3] == 1 ~ 4,
    row[col_0] == 0 & row[col_1] == 1 & row[col_2] == 1 & row[col_3] == 1 ~ 3,
    row[col_0] == 0 & row[col_1] == 1 & row[col_2] == 1 & row[col_3] == 0 ~ 2,
    TRUE ~ 9
  )
}


input_reduction_columns <- colnames(input_reduction)
input_reduction_zimbabwe <- select(zimbabwe_survey, all_of(input_reduction_columns))%>%
  #_1_4_3_1:**Over the past 12 months [add country meaning], what did you do to improve the soil fertility of cropland?**
  mutate(
    new_column_soil_fertility = improve_indicator(c_across(starts_with("_1_4_3_1")), 
                                                  col_0 = "/0", 
                                                  col_1 = "_1_4_3_1/1", 
                                                  col_2 = "_1_4_3_1/2", 
                                                  col_3 = "_1_4_3_1/3"))
    
    
    "_1_4_3_1"=improve_indicator("_1_4_3_1/0","_1_4_3_1/1","_1_4_3_1/2","_1_4_3_1/3"))

  mutate("_1_4_3_1"= if_else("_1_4_3_1/0"==0&"_1_4_3_1/1"==1&"_1_4_3_1/2"==0&"_1_4_3_1/3"==0,1,
                             if_else("_1_4_3_1/0"==0&"_1_4_3_1/1"==0&"_1_4_3_1/2"==0&"_1_4_3_1/3"==1,5,
                                     if_else("_1_4_3_1/0"==0&"_1_4_3_1/1"==0&"_1_4_3_1/2"==1&"_1_4_3_1/3"==1,4,
                                             if_else("_1_4_3_1/0"==0&"_1_4_3_1/1"==1&"_1_4_3_1/2"==1&"_1_4_3_1/3"==1,3,
                                                     if_else("_1_4_3_1/0"==0&"_1_4_3_1/1"==1&"_1_4_3_1/2"==1&"_1_4_3_1/3"==0,2,9))))))
                                                             
                    
  gather(key = "name_question", value = "name_choice",-kobo_id, -country)%>%
  left_join(agroecology, by="name_question")

length(unique(recycling_zimbabwe$kobo_id))