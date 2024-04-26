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
global.data.path <-"C:/Users/andreasanchez/OneDrive - CGIAR/Bioversity/AI/HOLPA/analysis/HOLPA/holpa_household_form/"
zwe.data.path <-"C:/Users/andreasanchez/OneDrive - CGIAR/Bioversity/AI/HOLPA/analysis/HOLPA/zwe/"

#### Import data ####
# Each dataset contains a survey worksheet with the questions and responses for text, open and numeric questions, and
# a choices worksheet with the response options for multiple choice questions (single or multiple).
# These need to be imported and combined.

### Country databases ####
zwe_survey <- read_excel(path=paste0(zwe.data.path,"household_database_2024.04.18_clean.xlsx"),
                              sheet = "Final HOLPA_Zimbabwe_Household")%>%
  rename("kobo_farmer_id"="_id")%>%
  rename("country"="_1_2_1_3")%>%
  mutate("_2_6_1_4_6"= NA)%>%
  mutate("_2_7_1_6/other"= NA)

zwe_choices <- read_excel(path=paste0(zwe.data.path,"zimbabwe_household_survey.xlsx"),
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
unique(context_survey$subindicator)

context_survey <-  global_survey %>%
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

context_choices <- global_choices %>%
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
unique(performance_eco_survey$indicator)
unique(performance_eco_survey$subindicator)
unique(performance_eco_choices$subindicator)

performance_eco_survey <-  global_survey %>%
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

performance_eco_choices <- global_choices %>%
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
unique(performance_soc_survey$indicator)
unique(performance_soc_survey$subindicator)
unique(performance_soc_choices$subindicator)

performance_soc_survey <-  global_survey %>%
  filter(str_detect(module, "performance"))%>%
  mutate(module= "performance")%>%
  filter(str_detect(indicator, "social"))%>%
  mutate(indicator="social") %>%
  mutate(subindicator=if_else(str_detect(subindicator, "nutrition"),"nutrition",subindicator))

performance_soc_choices <- global_choices %>%
  filter(str_detect(module, "performance"))%>%
  mutate(module= "performance") %>% 
  filter(str_detect(indicator, "social"))%>%
  mutate(indicator="social") %>% 
  mutate(subindicator=if_else(str_detect(subindicator, "nutrition"),"nutrition",subindicator))

unique(performance_agr_survey$indicator)
unique(performance_agr_survey$subindicator)
unique(performance_agr_choices$subindicator)

performance_agr_survey <-  global_survey %>%
  filter(str_detect(module, "performance"))%>%
  mutate(module= "performance")%>%
  filter(str_detect(indicator, "agricult"))%>%
  mutate(indicator="agricultal") %>%
  mutate(subindicator=if_else(str_detect(subindicator, "nutrient_use"),"nutrient_use",
                              if_else(str_detect(subindicator, "soil_health"),"nutrient_use", # CHECK THIS ONE
                              if_else(str_detect(subindicator, "animal_health"),"animal health",
                              if_else(str_detect(subindicator, "productivity_livestock"),"productivity_livestock",
                                      if_else(str_detect(subindicator, "productivity_fish"),"productivity_fish",subindicator))))))

performance_agr_choices <- global_choices %>%
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


unique(performance_env_survey$indicator)
unique(performance_env_survey$subindicator)
unique(performance_env_choices$subindicator)

performance_env_survey <-  global_survey %>%
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

performance_env_choices <- global_choices %>%
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


performance_survey <- rbind(performance_agr_survey,performance_soc_survey,performance_eco_survey,performance_env_survey) %>%
  rename(theme = indicator,
         indicator = subindicator)

performance_choices <- rbind(performance_agr_choices,performance_soc_choices,performance_eco_choices,performance_env_choices) %>%
  rename(theme = indicator,
         indicator = subindicator)

#### Agroecology module ####
agroecology_survey <-  global_survey %>%
  filter(str_detect(module, "agroecology"))%>%
  #select(-subindicator)%>%
  mutate(module= "agroecology")%>%
  mutate(subindicator=indicator) %>%
  mutate(type_question = case_when(
    name_question %in% c(
      "_2_9_1_1","_2_4_1","_2_10_1_2",
      #6_synergy                   
      "_2_9_1_1","_3_3_1_7","_3_3_3_3","_3_3_3_4","_2_12_1") ~ "count",
    TRUE ~ type_question))

sort(unique(agroecology_survey$indicator))
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
sort(unique(agroecology_survey$indicator))

agroecology_choices <- global_choices %>%
  filter(str_detect(module, "agroecology"))%>%
  #select(-subindicator)%>%
  mutate(module= "agroecology")%>%
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
  
sort(unique(agroecology_choices$indicator))


#### Assign scores (1-5) to agroecology question responses using a pre-made lookup table #### 
agrocology_choices_multiple<-agroecology_choices%>%
  filter(type_question!="count")%>%
  select(-name_question_choice)%>%
  rbind(read_excel(paste0(global.data.path,"HOLPA_global_household_survey_20231204_mapped_to_indicators_master.xlsx"),
                   sheet = "agroecology_look_up"))

names(agrocology_choices_multiple)

#### Principles structure ####
sort(unique(agroecology_choices$indicator))

agroecology_questions_columns<- agroecology_choices%>% 
  filter(!str_detect(indicator, "extra"))%>%
  select(label_question, name_question_choice)%>%
  dplyr::distinct(name_question_choice, .keep_all = TRUE)%>%
  spread(key = name_question_choice, value = label_question)%>%
  mutate("kobo_farmer_id"="kobo_farmer_id",
         "country"="country_name")

agroecology_questions_columns <- colnames(agroecology_questions_columns)
agroecology_questions_columns

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
agroecology_select_multiple_columns<- agroecology_survey%>%
  filter(!str_detect(indicator, "extra"))%>%
  filter(type_question == "select_multiple")%>%
  select(name_question,type_question)%>%
  #mutate(name_question= paste(name_question,"/",sep = ""))%>%
  spread(key = name_question, value = type_question)

agroecology_select_multiple_columns<- colnames(agroecology_select_multiple_columns)
agroecology_select_multiple_columns

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
agroecology_count_columns<- agroecology_survey%>%
  filter(!str_detect(indicator, "extra"))%>%
  filter(type_question == "count")%>%
  select(name_question,type_question)%>%
  mutate(name_question= paste(name_question,"/",sep = ""))%>%
  spread(key = name_question, value = type_question)

agroecology_count_columns<- colnames(agroecology_count_columns)
agroecology_count_columns


## ANALYSIS BY COUNTRY
# Zimbabwe
zwe_agroecology_columns <- intersect(agroecology_questions_columns, colnames(zwe_survey))
zwe_agroecology_columns

names(zwe_agroecology_columns)
zwe_agroecology <- zwe_survey %>%
  select(all_of(zwe_agroecology_columns))%>%
  mutate_all(as.character)%>%
  process_columns_regex(agroecology_select_multiple_columns)%>%
  count_columns_regex(agroecology_count_columns)%>%
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
  dplyr::left_join(select(agrocology_choices_multiple,c(name_question,module,indicator,name_choice,score_agroecology_module,label_choice,
                                                        label_question,type,type_question,list_name)), 
                   by= c("name_question"="name_question",
                         "name_choice"="name_choice"))%>%
  select("module", "indicator","country",
         "kobo_farmer_id","name_question", "type", "type_question"  , "list_name" , 
         "label_question","label_choice", "name_choice" ,"score_agroecology_module")

names(zwe_agroecology)
write.csv(zwe_agroecology,file=paste0(zwe.data.path,"zwe_agroecology.csv"),row.names=FALSE)

file=paste0(zwe.data.path,"zwe_agroecology.xlsx")

sort(unique(agroecology_zwe$name_question))
sort(unique(agroecology_zwe$name_choice))  
sort(unique(agroecology_zwe$indicator))