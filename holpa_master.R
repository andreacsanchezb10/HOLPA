#library(openxlsx)
library(tidyr)
library(tidyverse)
library(readxl)
library(dplyr)

##Country databases
# Completed surveys
zimbabwe_survey <- read_excel(
  "C:/Users/andreasanchez/OneDrive - CGIAR/Bioversity/AI/HOLPA/analysis/HOLPA/zimbabwe_household_name_2023.11.27.xlsx",
  sheet = "Final HOLPA_Zimbabwe_Household") %>%
  rename("kobo_id"="_id")%>%
  rename("country"="_1_2_1_3")

# Country forms
zimbabwe_choices <- read_excel(
  "C:/Users/andreasanchez/OneDrive - CGIAR/Bioversity/AI/HOLPA/analysis/HOLPA/zimbabwe_household_form.xlsx",
  sheet = "choices")%>%
  select("list_name","name","label::English ((en))","score_agroecology_module")%>%
  mutate(country="zimbabwe")%>%
  rename("label_choice" = "label::English ((en))")%>%
  rename("name_choice" = "name")


#### Global databases
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
  rename("name_choice" = "name")%>%
  mutate(country="global")%>%
  #Rbind country specific choices 
  rbind(zimbabwe_choices)%>%
  distinct(list_name,name_choice,label_choice, .keep_all = TRUE)%>%
  left_join(global_form,by="list_name")
  

 
######Agroecology module
agroecology_form<-  global_form%>%
  filter(str_detect(module, "agroecology"))%>%
  select(-subindicator)%>%
  mutate(module= "agroecology")%>%
  mutate(indicator=if_else(str_detect(indicator, "1_recycling"),"1_recycling",
                           if_else(str_detect(indicator, "2_input_reduction"),"2_input_reduction",
                                   indicator)))

agroecology_options <- global_choices%>%
  select(-subindicator)%>%
  rbind(read_excel(
    "C:/Users/andreasanchez/OneDrive - CGIAR/Bioversity/AI/HOLPA/analysis/HOLPA/HOLPA_global_household_survey_20231204_mapped_to_indicators.xlsx",
    sheet = "agroecology_look_up"))%>%
  mutate(module= "agroecology")%>%
  mutate(indicator=if_else(str_detect(indicator, "1_recycling"),"1_recycling",
                           if_else(str_detect(indicator, "2_input_reduction"),"2_input_reduction",
                                   indicator)))


agrocology_choices<-global_choices%>%
  filter(str_detect(module, "agroecology"))%>%
  select(-subindicator)%>%
  mutate(module= "agroecology")%>%
  mutate(indicator=if_else(str_detect(indicator, "1_recycling"),"1_recycling",
                           if_else(str_detect(indicator, "2_input_reduction"),"2_input_reduction",
                                   indicator)))%>%
  mutate(name_question= if_else(type_q=="select_multiple",
                                paste(name_question,"/",name_choice, sep=""),
                                name_question))
  

  
## 1- Recycling structure
recycling<- agrocology_choices%>%
  filter(str_detect(indicator, "1_recycling"))%>%
  select(label_question, name_question)%>%
  distinct(., .keep_all = TRUE)%>%
  spread(key = name_question, value = label_question)%>%
  mutate("kobo_id"="kobo_farmer_id",
         "country"="country_name")

## 2- Input reduction structure
input_reduction<- agrocology_choices%>%
  filter(str_detect(indicator, "2_input_reduction"))%>%
  dplyr::select(label_question, name_question)%>%
  distinct(., .keep_all = TRUE)%>%
  spread(key = name_question, value = label_question)%>%
  mutate("kobo_id"="kobo_farmer_id",
         "country"="country_name")
  

#####Prueba  
recycling_columns <- colnames(recycling)

recycling_zimbabwe <- select(zimbabwe_survey, all_of(recycling_columns))%>%
  gather(key = "name_question", value = "name_choice",-kobo_id, -country)%>%
  mutate(name_choice= as.character(name_choice))%>%
  dplyr::left_join(agrocology_choices, by=c("name_question"="name_question",
                                            "name_choice"="name_choice"))%>%
  select("module", "indicator",country.x,"kobo_id","name_question", "type", "type_q"  , "list_name" , 
         "label_question","label_choice", "name_choice" ,"score_agroecology_module")
  

sort(unique(recycling_zimbabwe$name_choice))
names(recycling_zimbabwe)  
  
input_reduction_columns

input_reduction_columns <- colnames(input_reduction)



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

# Example prefixes
prefixes<- c(sort(unique(agroecology_form$name_question[agroecology_form$type_q %in% "select_multiple"])))

prefixes <- c("x_1_4_3_1", "x_1_4_3_5", "x_1_4_3_8")
prefixes
# Create columns dynamically
input_reduction_zimbabwe <- zimbabwe_survey %>%
  select(all_of(input_reduction_columns)) %>%
  mutate_all(as.character) %>%
  rename_at(vars(1:29), ~paste0("x", .)) %>%
  rename_at(vars(1:29), ~gsub("/", "x", .)) %>%
  process_columns_regex(prefixes)%>%
  rename_at(vars(1:29), ~gsub("x", "/", .))%>%
  rename_at(vars(1:29), ~gsub("/_", "_", .))%>%
  rename_at(vars(32:34), ~gsub("x_", "_", .))%>%
  select(29:34)
  gather(key = "name_question", value = "name_choice",-kobo_id, -country)%>%
  dplyr::left_join(select(agroecology_options,c(name_question,name_choice,score_agroecology_module)), 
                   by= c("name_question"="name_question",
                         "name_choice"="name_choice"))

  

  

  
  
  
  
  
  
  




