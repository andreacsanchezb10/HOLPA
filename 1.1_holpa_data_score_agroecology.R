#--------------------
#library(openxlsx)
library(tidyr)
library(tidyverse)
library(readxl)
library(dplyr)

#### Set file paths ####
# TO CHECK: I need to connect directly to the share point, I already asked Sebastien for permision
#For now I will leave it like this to continue working
#Sarah
global.data.path <- "D:/02_Bioversity/46_Agroecology_Initiative/holpa_results/"
zwe.data.path <- "D:/02_Bioversity/46_Agroecology_Initiative/holpa_results/zwe/"

#Andrea 
global.data.path <-"C:/Users/andreasanchez/OneDrive - CGIAR/Bioversity/AI/HOLPA/analysis/HOLPA/HOLPA/"
zwe.data.path <-"C:/Users/andreasanchez/OneDrive - CGIAR/Bioversity/AI/HOLPA/analysis/HOLPA/HOLPA/zwe/zwe_agroecology2.csv"

#### Import data ####
# Each dataset contains a survey worksheet with the questions and responses for text, open and numeric questions, and
# a choices worksheet with the response options for multiple choice questions (single or multiple).
# These need to be imported and combined.

### Country databases ####
#Zimbabwe
zwe_agroecology_data <- read.csv(zwe.data.path)


#### Global databases ####
agroecology_global_choices <- read_excel(paste0(global.data.path,"HOLPA_global_household_survey_20231204_mapped_to_indicators_master.xlsx"),
                             sheet = "choices")%>%
  select("list_name","name","label::English ((en))","country", "score_agroecology_module")%>%
  rename("label_choice" = "label::English ((en))")%>%
  rename("name_choice" = "name")%>%
  filter(!is.na(score_agroecology_module))
names(agroecology_global_choices)

add_row_to_database <- function(database, list_name, name_choice, label_choice, country, score_agroecology_module) {
  new_row <- data.frame(
    list_name = list_name,
    name_choice = name_choice,
    label_choice = label_choice,
    country = country,
    score_agroecology_module = score_agroecology_module,
    stringsAsFactors = FALSE
  )
  database <- rbind(database, new_row)
  return(database)
}

agroecology_country_choices<- data.frame(
  list_name = character(),
  name_choice = character(),
  label_choice = character(),
  country = character(),
  score_agroecology_module = numeric(),
  stringsAsFactors = FALSE)

### Country: Zimbabwe
#1-Recycling
agroecology_country_choices <- add_row_to_database(agroecology_country_choices, "2_8_1_1", "6", "All seeds are given by the government", "zimbabwe", 1)
agroecology_country_choices <- add_row_to_database(agroecology_country_choices, "2_8_1_1", "7", "75% of seeds are given by the government, the other 25% are self-purchased.", "zimbabwe",1)
agroecology_country_choices <- add_row_to_database(agroecology_country_choices, "2_8_1_1", "8", "50% of seeds are given by the government, the other 50% are self-purchased.", "zimbabwe", 1)
agroecology_country_choices <- add_row_to_database(agroecology_country_choices, "2_8_1_1", "9", "25% of seeds are given by the government, the other 75% are self-purchased.", "zimbabwe", 1)


agroecology_all_choices<-rbind(agroecology_global_choices,agroecology_country_choices)


# AGROECOLOGY: SCORE ------

## 1- Recycling
sort(unique(zwe_agroecology_data$theme))
recycling<- zwe_agroecology_data%>%
  filter(theme=="1_recycling" )%>%
  #filter(name_question_recla == "_2_8_1_1")%>%
  mutate(name_label_choice=if_else(type_question=="select_one", paste(name_choice,"_",label_choice,sep=""),NA))%>%
  dplyr::left_join(select(agroecology_all_choices,c(list_name,name_choice,label_choice,score_agroecology_module )), 
                   by= c("list_name"="list_name",
                         "name_choice"="name_choice",
                         "label_choice"="label_choice"))
  

  dplyr::left_join(agroecology_country_choices, 
                   by= c("list_name"="list_name",
                         "name_choice"="name_choice",
                         "label_choice"="label_choice",
                         "country"= "country"))

result <- recycling %>%
  mutate(is_na_score = if_else(is.na(score_agroecology_module), 
                               left_join(agroecology_country_choices, 
                   by= c("list_name"="list_name",
                         "name_choice"="name_choice",
                         "label_choice"="label_choice",
                         "country"= "country")),score_agroecology_module))


  
  dplyr::left_join(select(agrocology_country_choices,c(name_question,module,indicator,name_choice,score_agroecology_module,label_choice,
                                                       label_question,type,type_question,list_name)), 
                   by= c("name_question"="name_question",
                         "name_choice"="name_choice"))%>%

recycling$nb_obs <- country_specific_choices$score_agroecology_module[match(recycling$list_name,country_specific_choices$list_name)]


sort(unique(recycling$name_question_recla))
sort(unique(recycling$name_label_choice))

sort(unique(recycling$name_choice))
sort(unique(recycling$label_choice))



input_reduction<- zwe_agroecology_data%>%
  filter(theme=="11_connectivity" )

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