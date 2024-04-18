#library(openxlsx)
library(tidyr)
library(tidyverse)
library(readxl)
library(dplyr)

data.path <- "D:/02_Bioversity/46_Agroecology_Initiative/holpa_results/zwe/"

##Country databases
# Completed surveys
zimbabwe_survey <- read_excel(path=paste0(data.path,"holpa_household_name_2023.11.27.xlsx"),
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
  right_join(global_form,by="list_name",relationship="many-to-many")


#### Context module ####
unique(context_form$subindicator)

context_form <-  global_form %>%
  filter(str_detect(module, "context"))%>%
  mutate(module= "context")%>%
  mutate(indicator=if_else(str_detect(indicator, "respondent_characteristics"),"respondent_characteristics",
                           if_else(str_detect(indicator, "household_characteristic"),"household_characteristics",
                                   if_else(str_detect(indicator, "land_tenure"), "land_tenure",
                                           if_else(str_detect(indicator, "land tenure"), "land_tenure",
                                                   if_else(str_detect(indicator, "farm_characteristic"), "farm_characteristics",
                                                        if_else(str_detect(indicator, "inputs"), "inputs",indicator))))))) %>%
  mutate(subindicator=if_else(str_detect(subindicator, "production_systems"),"production_systems",
                              if_else(str_detect(subindicator, "inputs"),"inputs",
                                      if_else(str_detect(subindicator, "household_labour"),"household_labour",
                                              if_else(str_detect(subindicator, "credit_access"),"credit_access",
                                                      if_else(str_detect(subindicator, "income"),"income",
                                                              if_else(str_detect(subindicator, "land_tenure"),"land_tenure",
                                                                      if_else(str_detect(subindicator, "training"),"training",
                                                                              if_else(str_detect(subindicator, "education"),"education",
                                                                                      if_else(str_detect(subindicator, "literacy"),"literacy",subindicator))))))))))

context_options <- global_choices %>%
  filter(str_detect(module, "context"))%>%
  mutate(module= "context")%>%
  mutate(indicator=if_else(str_detect(indicator, "respondent_characteristic"),"respondent_characteristics",
                           if_else(str_detect(indicator, "household_characteristic"),"household_characteristics",
                                   
                                   # CORRECT: LAND TENURE SHOULD BE UNDER SOCIAL PERFORMANCE
                                   
                                   if_else(str_detect(indicator, "land_tenure"), "land_tenure",
                                           if_else(str_detect(indicator, "land tenure"), "land_tenure",
                                                   if_else(str_detect(indicator, "farm_characteristic"), "farm_characteristics",
                                                          if_else(str_detect(indicator, "inputs"), "inputs",indicator))))))) %>%
  mutate(subindicator=if_else(str_detect(subindicator, "production_systems"),"production_systems",
                              if_else(str_detect(subindicator, "inputs"),"inputs",
                                      if_else(str_detect(subindicator, "household_labour"),"household_labour",
                                              if_else(str_detect(subindicator, "credit_access"),"credit_access",
                                                      if_else(str_detect(subindicator, "income"),"income",
                                                              if_else(str_detect(subindicator, "land_tenure"),"land_tenure",
                                                                      if_else(str_detect(subindicator, "training"),"training",
                                                                              if_else(str_detect(subindicator, "education"),"education",
                                                                                      if_else(str_detect(subindicator, "literacy"),"literacy",subindicator))))))))))
                              
                              

#### Performance module ####
unique(performance_eco_form$indicator)
unique(performance_eco_form$subindicator)
unique(performance_eco_options$subindicator)

performance_eco_form <-  global_form %>%
  filter(str_detect(module, "performance"))%>%
  mutate(module= "performance")%>%
  filter(str_detect(indicator, "economic"))%>%
  mutate(indicator="economic") %>%
  mutate(subindicator=if_else(str_detect(subindicator, "climate_resilience_adaptative_capacity"),"climate_resilience_adaptative_capacity",
                              if_else(str_detect(subindicator, "climate_resilience_social_network"),"climate_resilience_social_network",
                                      if_else(str_detect(subindicator, "climate_resilience_assets"),"climate_resilience_assets",
                                              if_else(str_detect(subindicator, "income"),"income",
                                                      if_else(str_detect(subindicator, "climate_resilience_food_security"),"climate_resilience_food_security",
                                                              if_else(str_detect(subindicator, "credit_access"),"credit_access",
                                                                      if_else(str_detect(subindicator, "climate_resilience_basic_services"),"climate_resilience_basic_services",
                                                                              if_else(str_detect(subindicator, "labour_productivity"),"labour_productivity",subindicator)))))))))

performance_eco_options <- global_choices %>%
  filter(str_detect(module, "performance"))%>%
  mutate(module= "performance") %>% 
  filter(str_detect(indicator, "economic"))%>%
  mutate(indicator="economic") %>% 
  mutate(subindicator=if_else(str_detect(subindicator, "climate_resilience_adaptative_capacity"),"climate_resilience_adaptative_capacity",
                              if_else(str_detect(subindicator, "climate_resilience_social_network"),"climate_resilience_social_network",
                                      if_else(str_detect(subindicator, "climate_resilience_assets"),"climate_resilience_assets",
                                              if_else(str_detect(subindicator, "income"),"income",
                                                      if_else(str_detect(subindicator, "climate_resilience_food_security"),"climate_resilience_food_security",
                                                              if_else(str_detect(subindicator, "credit_access"),"credit_access",
                                                                      if_else(str_detect(subindicator, "climate_resilience_basic_services"),"climate_resilience_basic_services",
                                                                              if_else(str_detect(subindicator, "labour_productivity"),"labour_productivity",subindicator)))))))))
unique(performance_soc_form$indicator)
unique(performance_soc_form$subindicator)
unique(performance_soc_options$subindicator)

performance_soc_form <-  global_form %>%
  filter(str_detect(module, "performance"))%>%
  mutate(module= "performance")%>%
  filter(str_detect(indicator, "social"))%>%
  mutate(indicator="social") %>%
  mutate(subindicator=if_else(str_detect(subindicator, "nutrition"),"nutrition",subindicator))

performance_soc_options <- global_choices %>%
  filter(str_detect(module, "performance"))%>%
  mutate(module= "performance") %>% 
  filter(str_detect(indicator, "social"))%>%
  mutate(indicator="social") %>% 
  mutate(subindicator=if_else(str_detect(subindicator, "nutrition"),"nutrition",subindicator))

unique(performance_agr_form$indicator)
unique(performance_agr_form$subindicator)
unique(performance_agr_options$subindicator)

performance_agr_form <-  global_form %>%
  filter(str_detect(module, "performance"))%>%
  mutate(module= "performance")%>%
  filter(str_detect(indicator, "agricult"))%>%
  mutate(indicator="agricultal") %>%
  mutate(subindicator=if_else(str_detect(subindicator, "nutrient_use"),"nutrient_use",
                              if_else(str_detect(subindicator, "soil_health"),"nutrient_use", # CHECK THIS ONE
                              if_else(str_detect(subindicator, "animal_health"),"animal health",
                              if_else(str_detect(subindicator, "productivity_livestock"),"productivity_livestock",
                                      if_else(str_detect(subindicator, "productivity_fish"),"productivity_fish",subindicator))))))

performance_agr_options <- global_choices %>%
  filter(str_detect(module, "performance"))%>%
  mutate(module= "performance") %>% 
  filter(str_detect(indicator, "agricult"))%>%
  mutate(indicator="agricult") %>% 
  mutate(subindicator=if_else(str_detect(subindicator, "soil_health"),"nutrient_use",
                              if_else(str_detect(subindicator, "animal health"),"animal health",
                                      if_else(str_detect(subindicator, "productivity_livestock"),"productivity_livestock",
                                              if_else(str_detect(subindicator, "productivity_fish"),"productivity_fish",subindicator))))) %>%
  mutate(subindicator=if_else(str_detect(subindicator, "nutrient_use"),"nutrient_use",
                              
                              # CHECK THE NEXT ONE - should not have soil health here !!!!
                              
                              if_else(str_detect(subindicator, "soil_health"),"nutrient_use", 
                                      if_else(str_detect(subindicator, "animal_health"),"animal health",
                                              if_else(str_detect(subindicator, "productivity_livestock"),"productivity_livestock",
                                                      if_else(str_detect(subindicator, "productivity_fish"),"productivity_fish",subindicator))))))


unique(performance_env_form$indicator)
unique(performance_env_form$subindicator)
unique(performance_env_options$subindicator)

performance_env_form <-  global_form %>%
  filter(str_detect(module, "performance"))%>%
  mutate(module= "performance")%>%
  filter(str_detect(indicator, "environment"))%>%
  filter(!(is.na(subindicator))) %>%
  mutate(indicator="environmental") %>%
  
  # CHECK THE BIODIVERSITY ONES - should have three in total !!!!
  
  mutate(subindicator=if_else(str_detect(subindicator, "biodiversity_diversity"),"biodiversity_diversity",
                            if_else(str_detect(subindicator, "biodiversity_abundance"),"biodiversity_abundance",
                                    if_else(str_detect(subindicator, "biodiversity_climate_mitigation"),"biodiversity_practices",
                                            if_else(str_detect(subindicator, "biodiversity_practices"),"biodiversity_practices",
                                            if_else(str_detect(subindicator, "biodiversity_agrobiodiversity"),"biodiversity_agrobiodiversity",
                                                    if_else(str_detect(subindicator, "energy"),"energy",
                                                            if_else(str_detect(subindicator, "water"),"water",
                                                                  if_else(str_detect(subindicator, "climate_mitigation"),"biodiversity_practices",subindicator)))))))))

performance_env_options <- global_choices %>%
  filter(str_detect(module, "performance"))%>%
  mutate(module= "performance") %>% 
  filter(str_detect(indicator, "environment"))%>%
  mutate(indicator="environmental") %>%
  mutate(subindicator=if_else(str_detect(subindicator, "biodiversity_diversity"),"biodiversity_diversity",
                              if_else(str_detect(subindicator, "biodiversity_abundance"),"biodiversity_abundance",
                                      if_else(str_detect(subindicator, "biodiversity_climate_mitigation"),"biodiversity_practices",
                                              if_else(str_detect(subindicator, "biodiversity_practices"),"biodiversity_practices",
                                                      if_else(str_detect(subindicator, "biodiversity_agrobiodiversity"),"biodiversity_agrobiodiversity",
                                                              if_else(str_detect(subindicator, "energy"),"energy",
                                                                      if_else(str_detect(subindicator, "water"),"water",
                                                                              if_else(str_detect(subindicator, "climate_mitigation"),"biodiversity_practices",subindicator)))))))))


performance_form <- rbind(performance_agr_form,performance_soc_form,performance_eco_form,performance_env_form) %>%
  rename(theme = indicator,
         indicator = subindicator)

performance_options <- rbind(performance_agr_options,performance_soc_options,performance_eco_options,performance_env_options) %>%
  rename(theme = indicator,
         indicator = subindicator)

#### Agroecology module ####
agroecology_form <-  global_form %>%
  filter(str_detect(module, "agroecology"))%>%
  #select(-subindicator)%>%
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
                                                                                                                   if_else(str_detect(indicator, "13_participation"), "13_participation",indicator)))))))))))))) %>%
  mutate(subindicator=indicator) %>%
  mutate(type_question = case_when(
    name_question %in% c(
      "_2_9_1_1","_2_4_1","_2_10_1_2",
      #6_synergy                   
      "_2_9_1_1","_3_3_1_7","_3_3_3_3","_3_3_3_4","_2_12_1") ~ "count",
    TRUE ~ type_question))

agroecology_options <- global_choices %>%
  filter(str_detect(module, "agroecology"))%>%
  #select(-subindicator)%>%
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