#library(openxlsx)
library(tidyr)
library(tidyverse)
library(readxl)
library(dplyr)

#### ZIMBABWE
#Data
zimbabweData <- read_excel("C:/Users/andreasanchez/OneDrive - CGIAR/Bioversity/AI/HOLPA/HOLPA_data/Zimbabwe/zimbabwe_data_clean/household_database_2024.04.18_clean.xlsx",
                              sheet = "Final HOLPA_Zimbabwe_Household")%>%
  rename("kobo_farmer_id"="_id")%>%
  rename("country"="_1_2_1_3")%>%
  mutate("_2_6_1_4_6"= NA)%>%
  mutate("_2_7_1_6/other"= NA)

sort(unique(zimbabweData$country))
sort(unique(zimbabweData$kobo_farmer_id))

# Form
zimbabweChoices <- read_excel("C:/Users/andreasanchez/OneDrive - CGIAR/Bioversity/AI/HOLPA/analysis/HOLPA/holpa_household_form/zimbabwe_holpa_household_form.xlsx",
                              sheet = "choices")%>%
  select("list_name","name","label::English ((en))","score_agroecology_module")%>%
  mutate(country="zimbabwe")%>%
  rename("label_choice" = "label::English ((en))")%>%
  rename("name_choice" = "name")


#### GLOBAL
# Form
globalSurvey <- read_excel("C:/Users/andreasanchez/OneDrive - CGIAR/Bioversity/AI/HOLPA/analysis/HOLPA/holpa_household_form/global_holpa_household_form.xlsx",
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


globalChoices <- read_excel("C:/Users/andreasanchez/OneDrive - CGIAR/Bioversity/AI/HOLPA/analysis/HOLPA/holpa_household_form/global_holpa_household_form.xlsx",
                             sheet = "choices")%>%
  select("list_name","name","label::English ((en))","score_agroecology_module","country")%>%
  rename("label_choice" = "label::English ((en))")%>%
  rename("name_choice" = "name")%>%
  #Rbind country specific choices 
  distinct(list_name,name_choice,label_choice, .keep_all = TRUE)%>%
  right_join(globalSurvey,by="list_name")

#Atencion! EL RIGHT JOIN PUEDE SER REEMPLAZADO SI ES QUE NO NECESITO TODAS LAS COLUMNAS DE globalSurvey

#### AGROECOLOGY MODULE
agroecologySurvey<-  globalSurvey%>%
  filter(str_detect(module, "agroecology"))%>%
  select(-subindicator)%>%
  mutate(module= "agroecology")%>%
  mutate(type_question = case_when(
    name_question %in% c(
      "_2_9_1_1","_2_4_1","_2_10_1_2",
      #6_synergy                   
      "_2_9_1_1","_3_3_1_7","_3_3_3_3","_3_3_3_4","_2_12_1") ~ "count",
    TRUE ~ type_question))
  
sort(unique(agroecologySurvey$indicator))
agroecologySurvey$indicator[str_detect(agroecologySurvey$indicator, "1_recycling")]<- "1_recycling"
agroecologySurvey$indicator[str_detect(agroecologySurvey$indicator, "2_input_reduction")]<- "2_input_reduction"
agroecologySurvey$indicator[str_detect(agroecologySurvey$indicator, "3_soil_health")]<- "3_soil_health"
agroecologySurvey$indicator[str_detect(agroecologySurvey$indicator, "4_animal_health")]<- "4_animal_health"
agroecologySurvey$indicator[str_detect(agroecologySurvey$indicator, "5_biodiversity")]<- "5_biodiversity"
agroecologySurvey$indicator[str_detect(agroecologySurvey$indicator, "6_synergy")]<- "6_synergy"
agroecologySurvey$indicator[str_detect(agroecologySurvey$indicator, "7_economic_diversification")]<- "7_economic_diversification"
agroecologySurvey$indicator[str_detect(agroecologySurvey$indicator, "8_knowledge")]<- "8_knowledge"
agroecologySurvey$indicator[str_detect(agroecologySurvey$indicator, "9_social_values")]<- "9_social_values"
agroecologySurvey$indicator[str_detect(agroecologySurvey$indicator, "10_fairness")]<- "10_fairness"
agroecologySurvey$indicator[str_detect(agroecologySurvey$indicator, "11_connectivity")]<- "11_connectivity"
agroecologySurvey$indicator[str_detect(agroecologySurvey$indicator, "12_governance")]<- "12_governance"
agroecologySurvey$indicator[str_detect(agroecologySurvey$indicator, "13_participation")]<- "13_participation"

sort(unique(agroecologySurvey$indicator))

agroecologyChoices <- globalChoices%>%
  filter(str_detect(module, "agroecology"))%>%
  select(-subindicator)%>%
  mutate(module= "agroecology")%>%
  mutate(name_question_choice= if_else(type_question=="select_multiple",
                                       paste(name_question,"/",name_choice, sep=""),
                                       name_question))%>%
  mutate(type_question = case_when(
    name_question %in% c(
      "_2_9_1_1","_2_4_1","_2_10_1_2",
      #6_synergy                   
      "_2_9_1_1","_3_3_1_7","_3_3_3_3","_3_3_3_4","_2_12_1") ~ "count",
    TRUE ~ type_question))

sort(unique(agroecologyChoices$indicator))
agroecologyChoices$indicator[str_detect(agroecologyChoices$indicator, "1_recycling")]<- "1_recycling"
agroecologyChoices$indicator[str_detect(agroecologyChoices$indicator, "2_input_reduction")]<- "2_input_reduction"
agroecologyChoices$indicator[str_detect(agroecologyChoices$indicator, "3_soil_health")]<- "3_soil_health"
agroecologyChoices$indicator[str_detect(agroecologyChoices$indicator, "4_animal_health")]<- "4_animal_health"
agroecologyChoices$indicator[str_detect(agroecologyChoices$indicator, "5_biodiversity")]<- "5_biodiversity"
agroecologyChoices$indicator[str_detect(agroecologyChoices$indicator, "6_synergy")]<- "6_synergy"
agroecologyChoices$indicator[str_detect(agroecologyChoices$indicator, "7_economic_diversification")]<- "7_economic_diversification"
agroecologyChoices$indicator[str_detect(agroecologyChoices$indicator, "8_knowledge")]<- "8_knowledge"
agroecologyChoices$indicator[str_detect(agroecologyChoices$indicator, "9_social_values")]<- "9_social_values"
agroecologyChoices$indicator[str_detect(agroecologyChoices$indicator, "10_fairness")]<- "10_fairness"
agroecologyChoices$indicator[str_detect(agroecologyChoices$indicator, "11_connectivity")]<- "11_connectivity"
agroecologyChoices$indicator[str_detect(agroecologyChoices$indicator, "12_governance")]<- "12_governance"
agroecologyChoices$indicator[str_detect(agroecologyChoices$indicator, "13_participation")]<- "13_participation"
sort(unique(agroecologyChoices$indicator))

  
agrocologyChoicesMultiple<-agroecologyChoices%>%
  filter(type_question!="count")%>%
  select(-name_question_choice)%>%
  rbind(read_excel("C:/Users/andreasanchez/OneDrive - CGIAR/Bioversity/AI/HOLPA/analysis/HOLPA/holpa_household_form/global_holpa_household_form.xlsx",
                   sheet = "agroecology_look_up"))

names(agrocologyChoicesMultiple)

## Principles structure
sort(unique(agroecologyChoices$indicator))
sort(unique(agroecologyQuestionsColumns$indicator))

agroecologyQuestionsColumns<- agroecologyChoices%>% 
  filter(!str_detect(indicator, "extra"))%>%
  select(label_question, name_question_choice)%>%
  dplyr::distinct(name_question_choice, .keep_all = TRUE)%>%
  spread(key = name_question_choice, value = label_question)%>%
  mutate("kobo_farmer_id"="kobo_farmer_id",
         "country"="country_name")

agroecologyQuestionsColumns <- colnames(agroecologyQuestionsColumns)
agroecologyQuestionsColumns


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
agroecologySelectMultipleColumns<- agroecologySurvey%>%
  filter(!str_detect(indicator, "extra"))%>%
  filter(type_question == "select_multiple")%>%
  select(name_question,type_question)%>%
  #mutate(name_question= paste(name_question,"/",sep = ""))%>%
  spread(key = name_question, value = type_question)

agroecologySelectMultipleColumns<- colnames(agroecologySelectMultipleColumns)
agroecologySelectMultipleColumns

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
agroecologyCountColumns<- agroecologySurvey%>%
  filter(!str_detect(indicator, "extra"))%>%
  filter(type_question == "count")%>%
  select(name_question,type_question)%>%
  mutate(name_question= paste(name_question,"/",sep = ""))%>%
  spread(key = name_question, value = type_question)

agroecologyCountColumns<- colnames(agroecologyCountColumns)
agroecologyCountColumns


## ANALYSIS BY COUNTRY
# Zimbabwe
agroecologyZimbabweColumns <- intersect(agroecologyQuestionsColumns, colnames(zimbabweData))
agroecologyZimbabweColumns

names(agroecologyZimbabwe)
agroecologyZimbabwe <- zimbabweData %>%
  select(all_of(agroecologyZimbabweColumns))%>%
  mutate_all(as.character)%>%
  process_columns_regex(agroecologySelectMultipleColumns)%>%
  count_columns_regex(agroecologyCountColumns)%>%
  dplyr::rename_with(~ paste0("x", gsub("/", "", .)), .cols = c(
    "_2_9_1_1/","_2_4_1/","_2_10_1_2/",
    #5_biodiversity
    "_3_3_1_7/","_3_3_3_3/", "_3_3_3_4/","_2_12_1/",
    "_3_4_2_2_2_1_calculate","_3_4_2_2_2_2_calculate",
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
    "x_3_4_2_2_2_1_calculate","x_3_4_2_2_2_2_calculate","x_3_3_1_7","x_3_3_3_3","x_3_3_3_4","x_2_12_1","_3_3_3_1",
    "x_3_3_3_1xAgroforestry","x_3_3_3_1xFallow","x_3_3_3_1xHedgerowsxLive_fences","x_3_3_3_1xHomegarden",
    "x_3_3_3_1xNatural_stripsxvegetation","x_3_3_3_1xPollinatorxFlower_strips","x_3_3_3_1xPushxpull",
    #11_connectivity
    "x_1_4_2_1xCrops","x_1_4_2_1xLivestock","x_1_4_2_1xFish","x_1_4_2_1xHoney","x_1_4_2_1xTrees","x_1_4_2_1xother","_1_4_2_1"
    ))%>%
  select(-contains("/"))%>%
  gather(key = "name_question", value = "name_choice",-kobo_farmer_id, -country)%>%
  dplyr::left_join(select(agrocologyChoicesMultiple,c(name_question,module,indicator,name_choice,score_agroecology_module,label_choice,
                                               label_question,type,type_question,list_name)), 
                   by= c("name_question"="name_question",
                         "name_choice"="name_choice"))%>%
  select("module", "indicator","country",
         "kobo_farmer_id","name_question", "type", "type_question"  , "list_name" , 
         "label_question","label_choice", "name_choice" ,"score_agroecology_module")

names(agroecologyZimbabwe)
write.csv(agroecologyZimbabwe,file='agroecologyZimbabwe.csv',row.names=FALSE)

###############