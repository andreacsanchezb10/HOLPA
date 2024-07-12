#library(openxlsx)
library(tidyr)
library(tidyverse)
library(readxl)
library(dplyr)
library(summarytools)

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

# Read csv files----
read_and_process_survey_csv <- function(sheet_name, column_id_rename, data_path, country_name, index) {
  survey_data<- read.csv(path = data_path, header = TRUE, sep = ",")%>%
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
zwe_survey_3_4_3_1_2_begin_repeat <- read_and_process_survey_xlsx("_3_4_3_1_2_begin_repeat", "_submission__id", zwe.data.path,"zimbabwe","_index") # Section: Crop production
zwe_survey_3_4_2_2_2_begin_repeat<-read_and_process_survey_xlsx("_3_4_2_2_2_begin_repeat", "_submission__id", zwe.data.path,"zimbabwe","_index") # Section: Livestock production 1
zwe_survey_3_4_2_2_6_begin_repeat<-read_and_process_survey_xlsx("_3_4_2_2_6_begin_repeat", "_submission__id", zwe.data.path,"zimbabwe","_index") # Section: Livestock production 2
zwe_survey_3_4_1_1_7_1_begin_repeat<-read_and_process_survey_xlsx("_3_4_1_1_7_1_begin_repeat", "_submission__id", zwe.data.path,"zimbabwe","_index")%>% # Section:labour household members permanent workers
  #Remove respondents that are not farmers
  filter(kobo_farmer_id!="274186917")
zwe_survey_3_4_1_1_7_2_begin_repeat<-read_and_process_survey_xlsx("_3_4_1_1_7_2_begin_repeat", "_submission__id", zwe.data.path,"zimbabwe","_index") # Section: labour household members seasonal workers 1
zwe_survey_3_4_1_2_7_2_1_begin_repeat<-read_and_process_survey_xlsx("_3_4_1_2_7_2_1_begin_repeat", "_submission__id", zwe.data.path,"zimbabwe","_index") # Section: labour household members seasonal workers 2
zwe_survey_3_4_1_2_1_1_begin_repeat<- read_and_process_survey_xlsx("_3_4_1_2_1_1_begin_repeat", "_submission__id", zwe.data.path,"zimbabwe","_index") # Section: labour Hired/Free/Exchange Labourers permanent workers
zwe_survey_3_4_1_2_1_2_begin_repeat<-read_and_process_survey_xlsx("_3_4_1_2_1_2_begin_repeat", "_submission__id", zwe.data.path,"zimbabwe","_index") # Section: labour Hired/Free/Exchange Labourers seasonal workers 1
zwe_survey_3_4_1_2_1_2_1_begin_repeat<-read_and_process_survey_xlsx("_3_4_1_2_1_2_1_begin_repeat", "_submission__id", zwe.data.path,"zimbabwe","_index") # Section: labour Hired/Free/Exchange Labourers seasonal workers 2
zwe_survey_3_3_3_2_begin_repeat<- read_and_process_survey_xlsx("_3_3_3_2_begin_repeat", "_submission__id", zwe.data.path,"zimbabwe","_index") # Section: area of land per agricultural practice
zwe_survey_3_3_4_1_3_begin_repeat<- read_and_process_survey_xlsx("_3_3_4_1_3_begin_repeat", "_submission__id", zwe.data.path,"zimbabwe","_index") # Section: Irrigation

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


### KENYA ----
ken.data.path <-"C:/Users/andreasanchez/OneDrive - CGIAR/Bioversity/AI/HOLPA/HOLPA_data/Kenya/kenya_data_clean/holpa_household_name_2024.06.23.xlsx" #path: Andrea

ken_survey_main <- read_and_process_survey_xlsx("Holpa_global_household_surve", "_id", ken.data.path,"kenya","_index")%>%
  #Remove respondents that did not wanted to complete the survey
  filter(consent_2!="No")

ken_survey_3_4_3_1_2_begin_repeat <- read_and_process_survey_xlsx("_3_4_3_1_2_begin_repeat", "_submission__id", ken.data.path,"kenya","_index") # Section: Crop production
ken_survey_3_4_2_2_2_begin_repeat<-read_and_process_survey_xlsx("_3_4_2_2_2_begin_repeat", "_submission__id", ken.data.path,"kenya","_index") # Section: Livestock production 1
ken_survey_3_4_2_2_6_begin_repeat<-read_and_process_survey_xlsx("_3_4_2_2_6_begin_repeat", "_submission__id", ken.data.path,"kenya","_index") # Section: Livestock production 2
ken_survey_3_4_1_1_7_1_begin_repeat<-read_and_process_survey_xlsx("_3_4_1_1_7_1_begin_repeat", "_submission__id", ken.data.path,"kenya","_index") # Section:labour household members permanent workers
ken_survey_3_4_1_1_7_2_begin_repeat<-read_and_process_survey_xlsx("_3_4_1_1_7_2_begin_repeat", "_submission__id", ken.data.path,"kenya","_index") # Section: labour household members seasonal workers 1
ken_survey_3_4_1_2_7_2_1_begin_repeat<-read_and_process_survey_xlsx("_3_4_1_2_7_2_1_begin_repeat", "_submission__id", ken.data.path,"kenya","_index") # Section: labour household members seasonal workers 2
ken_survey_3_4_1_2_1_1_begin_repeat<- read_and_process_survey_xlsx("_3_4_1_2_1_1_begin_repeat", "_submission__id", ken.data.path,"kenya","_index") # Section: labour Hired/Free/Exchange Labourers permanent workers
ken_survey_3_4_1_2_1_2_begin_repeat<-read_and_process_survey_xlsx("_3_4_1_2_1_2_begin_repeat", "_submission__id", ken.data.path,"kenya","_index") # Section: labour Hired/Free/Exchange Labourers seasonal workers 1
ken_survey_3_4_1_2_1_2_1_begin_repeat<-read_and_process_survey_xlsx("_3_4_1_2_1_2_1_begin_repeat", "_submission__id", ken.data.path,"kenya","_index") # Section: labour Hired/Free/Exchange Labourers seasonal workers 2
ken_survey_3_3_3_2_begin_repeat<- read_and_process_survey_xlsx("_3_3_3_2_begin_repeat", "_submission__id", ken.data.path,"kenya","_index") # Section: area of land per agricultural practice
ken_survey_3_3_4_1_3_begin_repeat<- read_and_process_survey_xlsx("_3_3_4_1_3_begin_repeat", "_submission__id", ken.data.path,"kenya","_index") # Section: Irrigation

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


### TUNISIA -----
tun.data.path <-"C:/Users/andreasanchez/OneDrive - CGIAR/Bioversity/AI/HOLPA/HOLPA_data/Tunisia/tunisia_data_clean/holpa_household_CLEAN_2024.04.02.xlsx" #path: Andrea

tun_survey_main <- read_and_process_survey_xlsx("HOLPA_Tunisia_household_surv", "_id", tun.data.path,"tunisia","_index")%>%
  #Remove respondents that did not wanted to complete the survey
  filter(consent_2!="No")
 
tun_survey_3_4_3_1_2_begin_repeat <- read_and_process_survey_xlsx("_3_4_3_1_2_begin_repeat", "_submission__id", tun.data.path,"tunisia","_index") # Section: Crop production
tun_survey_3_4_2_2_2_begin_repeat<-read_and_process_survey_xlsx("_3_4_2_2_2_begin_repeat", "_submission__id", tun.data.path,"tunisia","_index") # Section: Livestock production 1
tun_survey_3_4_2_2_6_begin_repeat<-read_and_process_survey_xlsx("_3_4_2_2_6_begin_repeat", "_submission__id", tun.data.path,"tunisia","_index") # Section: Livestock production 2
tun_survey_3_4_1_1_7_1_begin_repeat<-read_and_process_survey_xlsx("_3_4_1_1_7_1_begin_repeat", "_submission__id", tun.data.path,"tunisia","_index") # Section:labour household members permanent workers
tun_survey_3_4_1_1_7_2_begin_repeat<-read_and_process_survey_xlsx("_3_4_1_1_7_2_begin_repeat", "_submission__id", tun.data.path,"tunisia","_index") # Section: labour household members seasonal workers 1
tun_survey_3_4_1_2_7_2_1_begin_repeat<-read_and_process_survey_xlsx("_3_4_1_2_7_2_1_begin_repeat", "_submission__id", tun.data.path,"tunisia","_index") # Section: labour household members seasonal workers 2
tun_survey_3_4_1_2_1_1_begin_repeat<- read_and_process_survey_xlsx("_3_4_1_2_1_1_begin_repeat", "_submission__id", tun.data.path,"tunisia","_index") # Section: labour Hired/Free/Exchange Labourers permanent workers
tun_survey_3_4_1_2_1_2_begin_repeat<-read_and_process_survey_xlsx("_3_4_1_2_1_2_begin_repeat", "_submission__id", tun.data.path,"tunisia","_index") # Section: labour Hired/Free/Exchange Labourers seasonal workers 1
tun_survey_3_4_1_2_1_2_1_begin_repeat<-read_and_process_survey_xlsx("_3_4_1_2_1_2_1_begin_repeat", "_submission__id", tun.data.path,"tunisia","_index") # Section: labour Hired/Free/Exchange Labourers seasonal workers 2
tun_survey_3_3_3_2_begin_repeat<- read_and_process_survey_xlsx("_3_3_3_2_begin_repeat", "_submission__id", tun.data.path,"tunisia","_index") # Section: area of land per agricultural practice
tun_survey_3_3_4_1_3_begin_repeat<- read_and_process_survey_xlsx("_3_3_4_1_3_begin_repeat", "_submission__id", tun.data.path,"tunisia","_index") # Section: Irrigation

tun_choices <- read_excel("C:/Users/andreasanchez/OneDrive - CGIAR/Bioversity/AI/HOLPA/HOLPA_data/Tunisia/Tunisia_monitoring/holpa_household_form.xlsx",
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


#### PERFORMANCE MODULE ####
fun_performance_survey<- function(global_survey) {
  ## Theme: Economic 
  performance_eco_survey <-  global_survey %>%
    filter(str_detect(module, "performance"))%>%
    mutate(module= "performance")%>%
    filter(str_detect(indicator, "economic"))%>%
    mutate(indicator="economic") 
  
  #"income/climate_resilience_adaptative_capacity" these question is part of two indicators
  eco_duplicate_rows <-  performance_eco_survey%>% filter(str_detect(subindicator, "income/climate_resilience_adaptative_capacity"))
  eco_duplicate_rows$subindicator <- "income"
  performance_eco_survey <- rbind(performance_eco_survey, eco_duplicate_rows)
  
  performance_eco_survey$subindicator[str_detect(performance_eco_survey$subindicator, "climate_resilience_adaptative_capacity")]<- "climate_resilience_adaptative_capacity"
  performance_eco_survey$subindicator[str_detect(performance_eco_survey$subindicator, "climate_resilience_social_network")]<- "climate_resilience_social_network"
  performance_eco_survey$subindicator[str_detect(performance_eco_survey$subindicator, "climate_resilience_assets")]<- "climate_resilience_assets"
  performance_eco_survey$subindicator[str_detect(performance_eco_survey$subindicator, "income")]<- "income"
  performance_eco_survey$subindicator[str_detect(performance_eco_survey$subindicator, "climate_resilience_food_security")]<- "climate_resilience_food_security"
  performance_eco_survey$subindicator[str_detect(performance_eco_survey$subindicator, "credit_access/climate_resilience")]<- "climate_resilience"
  performance_eco_survey$subindicator[str_detect(performance_eco_survey$subindicator, "credit_access")]<- "credit_access"
  performance_eco_survey$subindicator[str_detect(performance_eco_survey$subindicator, "climate_resilience_basic_services")]<- "climate_resilience_basic_services"
  performance_eco_survey$subindicator[str_detect(performance_eco_survey$subindicator, "labour_productivity")]<- "labour_productivity"
  performance_eco_survey$subindicator[str_detect(performance_eco_survey$subindicator, "economic_all")]<- "economic_all"
 
  ## Theme: Social 
  performance_soc_survey <-  global_survey %>%
    filter(str_detect(module, "performance"))%>%
    mutate(module= "performance")%>%
    filter(str_detect(indicator, "social"))%>%
    mutate(indicator="social")
  performance_soc_survey$subindicator[str_detect(performance_soc_survey$subindicator, "nutrition")]<- "nutrition"
  performance_soc_survey$subindicator[str_detect(performance_soc_survey$subindicator, "social_all")]<- "social_all"
  
  ## Theme: Agricultural 
  performance_agr_survey <-  global_survey %>%
    filter(str_detect(module, "performance"))%>%
    mutate(module= "performance")%>%
    filter(str_detect(indicator, "agricult"))%>%
    mutate(indicator="agricultural")
  #_3_4_2_1_8_2 this question is part of two indicators
  duplicate_rows <- performance_agr_survey[performance_agr_survey$subindicator == "productivity_crops/crop_health", ]
  duplicate_rows$subindicator <- "crop_health"
  performance_agr_survey <- rbind(performance_agr_survey, duplicate_rows)
  
  performance_agr_survey$subindicator[str_detect(performance_agr_survey$subindicator, "nutrient_use")]<- "nutrient_use"
  performance_agr_survey$subindicator[str_detect(performance_agr_survey$subindicator, "soil_health")]<- "soil_health" 
  performance_agr_survey$subindicator[str_detect(performance_agr_survey$subindicator, "animal_health")]<- "animal_health"
  performance_agr_survey$subindicator[str_detect(performance_agr_survey$subindicator, "productivity_crops")]<- "productivity_crops"
  performance_agr_survey$subindicator[str_detect(performance_agr_survey$subindicator, "productivity_livestock")]<- "productivity_livestock"
  performance_agr_survey$subindicator[str_detect(performance_agr_survey$subindicator, "productivity_fish")]<- "productivity_fish"
  performance_agr_survey$subindicator[str_detect(performance_agr_survey$subindicator, "crop_health")]<- "crop_health"
  performance_agr_survey$subindicator[str_detect(performance_agr_survey$subindicator, "agricultural_all")]<- "agricultural_all"
  
  ## Theme: Environmental 
  performance_env_survey <-  global_survey %>%
    filter(str_detect(module, "performance"))%>%
    mutate(module= "performance")%>%
    filter(str_detect(indicator, "environment"))%>%
    filter(!(is.na(subindicator))) %>%
    mutate(indicator="environmental") 
  
  # CHECK THE BIODIVERSITY ONES - should have three in total !!!!
  performance_env_survey$subindicator[str_detect(performance_env_survey$subindicator, "energy")]<- "energy"
  performance_env_survey$subindicator[str_detect(performance_env_survey$subindicator, "biodiversity_practices")]<- "biodiversity_practices"
  performance_env_survey$subindicator[str_detect(performance_env_survey$subindicator, "biodiversity_abundance")]<- "biodiversity_abundance"
  performance_env_survey$subindicator[str_detect(performance_env_survey$subindicator, "biodiversity_diversity")]<- "biodiversity_diversity"
  performance_env_survey$subindicator[str_detect(performance_env_survey$subindicator, "biodiversity_agrobiodiversity")]<- "biodiversity_agrobiodiversity"
  performance_env_survey$subindicator[str_detect(performance_env_survey$subindicator, "climate_mitigation")]<- "biodiversity_climate_mitigation"
  performance_env_survey$subindicator[str_detect(performance_env_survey$subindicator, "water")]<- "water"
  performance_env_survey$subindicator[str_detect(performance_env_survey$subindicator, "environmental_all")]<- "environmental_all"
  
  performance_survey <- rbind(performance_agr_survey,performance_soc_survey,performance_eco_survey,performance_env_survey) %>%
    rename(theme = indicator,
           indicator = subindicator)%>%
    filter(!str_detect(indicator, "end_repeat"))
  
  return(performance_survey)
}

fun_performance_choices<- function(country_global_choices) {
  ## Theme: Economic
    # Filter and mutate the data frame
    performance_eco_choices <- country_global_choices %>%
      filter(str_detect(module, "performance")) %>%
      mutate(module = "performance") %>%
      filter(str_detect(indicator, "economic")) %>%
      mutate(indicator = "economic")
    
    #"income/climate_resilience_adaptative_capacity" this question is part of two indicators
    eco_duplicate_rows <- performance_eco_choices %>% filter(str_detect(subindicator, "income/climate_resilience_adaptative_capacity"))
    eco_duplicate_rows$subindicator <- "income"
    performance_eco_choices <- rbind(performance_eco_choices, eco_duplicate_rows)
    
    # Update subindicator values
    performance_eco_choices$subindicator[str_detect(performance_eco_choices$subindicator, "climate_resilience_adaptative_capacity")]<- "climate_resilience_adaptative_capacity"
    performance_eco_choices$subindicator[str_detect(performance_eco_choices$subindicator, "climate_resilience_social_network")]<- "climate_resilience_social_network"
    performance_eco_choices$subindicator[str_detect(performance_eco_choices$subindicator, "climate_resilience_assets")]<- "climate_resilience_assets"
    performance_eco_choices$subindicator[str_detect(performance_eco_choices$subindicator, "income")]<- "income"
    performance_eco_choices$subindicator[str_detect(performance_eco_choices$subindicator, "climate_resilience_food_security")]<- "climate_resilience_food_security"
    performance_eco_choices$subindicator[str_detect(performance_eco_choices$subindicator, "credit_access/climate_resilience")]<- "climate_resilience"
    performance_eco_choices$subindicator[str_detect(performance_eco_choices$subindicator, "credit_access")]<- "credit_access"
    performance_eco_choices$subindicator[str_detect(performance_eco_choices$subindicator, "climate_resilience_basic_services")]<- "climate_resilience_basic_services"
    performance_eco_choices$subindicator[str_detect(performance_eco_choices$subindicator, "labour_productivity")]<- "labour_productivity"
    performance_eco_choices$subindicator[str_detect(performance_eco_choices$subindicator, "economic_all")]<- "economic_all"
    
  ## Theme: Social
  # Filter and mutate the data frame
  performance_soc_choices <- country_global_choices %>%
    filter(str_detect(module, "performance")) %>%
    mutate(module = "performance") %>%
    filter(str_detect(indicator, "social")) %>%
    mutate(indicator = "social")
  
  # Update subindicator values
  performance_soc_choices$subindicator[str_detect(performance_soc_choices$subindicator, "nutrition")]<- "nutrition"
  performance_soc_choices$subindicator[str_detect(performance_soc_choices$subindicator, "social_all")]<- "social_all"
  
  ## Theme: Agricultural
    # Filter and mutate the data frame
    performance_agr_choices <- country_global_choices %>%
      filter(str_detect(module, "performance")) %>%
      mutate(module = "performance") %>%
      filter(str_detect(indicator, "agricult")) %>%
      mutate(indicator = "agricultural")
    
    #_3_4_2_1_8_2 this question is part of two indicators
    duplicate_rows <- performance_agr_choices[performance_agr_choices$subindicator == "productivity_crops/crop_health", ]
    duplicate_rows$subindicator <- "crop_health"
    performance_agr_choices <- rbind(performance_agr_choices, duplicate_rows)
    
    # Update subindicator values
    performance_agr_choices$subindicator[str_detect(performance_agr_choices$subindicator, "nutrient_use")]<- "nutrient_use"
    performance_agr_choices$subindicator[str_detect(performance_agr_choices$subindicator, "soil_health")]<- "soil_health" # CHECK THIS ONE
    performance_agr_choices$subindicator[str_detect(performance_agr_choices$subindicator, "animal_health")]<- "animal_health"
    performance_agr_choices$subindicator[str_detect(performance_agr_choices$subindicator, "productivity_crops")]<- "productivity_crops"
    performance_agr_choices$subindicator[str_detect(performance_agr_choices$subindicator, "productivity_livestock")]<- "productivity_livestock"
    performance_agr_choices$subindicator[str_detect(performance_agr_choices$subindicator, "productivity_fish")]<- "productivity_fish"
    performance_agr_choices$subindicator[str_detect(performance_agr_choices$subindicator, "crop_health")]<- "crop_health"
    performance_agr_choices$subindicator[str_detect(performance_agr_choices$subindicator, "agricultural_all")]<- "agricultural_all"
    
  ## Theme: Environmental ----
      # Filter and mutate the data frame
    performance_env_choices <- country_global_choices %>%
        filter(str_detect(module, "performance")) %>%
        mutate(module = "performance") %>%
        filter(str_detect(indicator, "environment")) %>%
        mutate(indicator = "environmental")
      
      # Update subindicator values
    performance_env_choices$subindicator[str_detect(performance_env_choices$subindicator, "energy")]<- "energy"
    performance_env_choices$subindicator[str_detect(performance_env_choices$subindicator, "biodiversity_practices")]<- "biodiversity_practices"
    performance_env_choices$subindicator[str_detect(performance_env_choices$subindicator, "biodiversity_abundance")]<- "biodiversity_abundance"
    performance_env_choices$subindicator[str_detect(performance_env_choices$subindicator, "biodiversity_diversity")]<- "biodiversity_diversity"
    performance_env_choices$subindicator[str_detect(performance_env_choices$subindicator, "biodiversity_agrobiodiversity")]<- "biodiversity_agrobiodiversity"
    performance_env_choices$subindicator[str_detect(performance_env_choices$subindicator, "climate_mitigation")]<- "biodiversity_climate_mitigation"
    performance_env_choices$subindicator[str_detect(performance_env_choices$subindicator, "water")]<- "water"
    performance_env_choices$subindicator[str_detect(performance_env_choices$subindicator, "environmental_all")]<- "environmental_all"
      
    
    performance_choices <- rbind(performance_agr_choices,performance_soc_choices,performance_eco_choices,performance_env_choices) %>%
      rename(theme = indicator,
             indicator = subindicator)%>%
      mutate(name_question_choice= if_else(type_question=="select_multiple",
                                           paste(name_question,"/",name_choice, sep=""),
                                           name_question))
      return(performance_choices)
    }
    
fun_performance_questions_columns<- function(country_performance_choices) {
  performance_questions_columns<- country_performance_choices%>% 
    # filter(
    # theme=="environmental"
    #  theme=="social"
    #theme== "economic"
    # theme=="agricultural"
    
    # )%>%
    # filter(
    #   indicator==   "land_tenure"
    
    #  )%>%  
  dplyr::select(label_question, name_question_choice)%>%
    dplyr::distinct(name_question_choice, .keep_all = TRUE)%>%
    spread(key = name_question_choice, value = label_question)%>%
    mutate("kobo_farmer_id"="kobo_farmer_id",
           "country"="country_name",
           "sheet_id"="sheet_id",
           "parent_table_name"="_parent_table_name",
           "index"="index",
           "parent_index"="_parent_index")
  
  performance_questions_columns <- colnames(performance_questions_columns)
  
  return(performance_questions_columns)
  
}

fun_perform_left_join <- function(performance_choices, gathered_data ) {
  
  # Left join for "calculate" and "integer"
  continuous <- gathered_data  %>%
    dplyr::left_join(select(performance_choices,
                            c(name_question, module, theme, indicator,"name_choice", label_choice, label_question,type, type_question, list_name)), 
                     by = "name_question")%>%
    filter(type_question =="calculate"|type_question =="integer"|type_question =="note"|
             type_question =="text"|type_question =="audio"|type_question =="decimal")%>%
    select(-name_choice.y)%>%
    rename("name_choice"="name_choice.x")
  
  # Left join for "select_multiple"
  select_multiple <- gathered_data  %>%
    left_join(select(performance_choices,
                     c(name_question, name_choice, module, theme, indicator, label_choice, label_question, type, type_question, list_name,name_question_choice)), 
              by = c("name_question"="name_question_choice"))%>%
    filter(type_question=="select_multiple")%>%
    select(-name_choice.y,-name_question.y)%>%
    rename("name_choice"="name_choice.x")%>%
    #Remove answers == "0" or NA
    filter(type_question == "select_multiple" & !is.na(name_choice) & name_choice != 0)
  
  # Left join for "select_one" for country== "zwe"
  select_one1 <- gathered_data  %>%
    left_join(select(performance_choices,
                     c(name_question, name_choice, module, theme, indicator, label_choice, label_question, type, type_question, list_name)), 
              by = c("name_question"="name_question", "name_choice"="name_choice"))%>%
    filter(type_question=="select_one")%>%
    filter(country=="zimbabwe")

  # Left join for "select_one" for country== "tun"
  select_one2 <- gathered_data  %>%
    left_join(select(performance_choices,
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
fun_performance_main<- function(country_global_choices,country_survey_main){
  country_performance_choices<-  fun_performance_choices(country_global_choices)
  country_performance_question_columns<- fun_performance_questions_columns(country_performance_choices)
  
  country_performance_columns <- intersect(country_performance_question_columns, colnames(country_survey_main))
  mismatched_columns <- setdiff(country_performance_question_columns, country_performance_columns)
  
  country_performance <- country_survey_main %>%
    select(all_of(country_performance_columns))%>%
    mutate_all(as.character)
  
  # Identify columns with only NA values
  na_columns <- colSums(is.na(country_performance)) == nrow(country_performance)
  na_columns
  
  # Remove columns with only NA values
  country_performance <- country_performance[, !na_columns]
  
  result_main_survey <- country_performance%>%
    gather(key = "name_question", value = "name_choice", -kobo_farmer_id, -country,-sheet_id,-index)%>%
    fun_perform_left_join(country_performance_choices,.)%>%
    mutate(name_question_recla= name_question)%>%
    mutate(parent_table_name= NA,
           parent_index=NA)
  return(result_main_survey)
}

## _3_4_3_1_2_begin_repeat: Crop production ----
fun_performance_3_4_3_1_2_begin_repeat<- function(country_global_choices,country_survey_3_4_3_1_2_begin_repeat){
  
  country_performance_choices<-  fun_performance_choices(country_global_choices)
  country_performance_question_columns<- fun_performance_questions_columns(country_performance_choices)
  
  
  country_performance_columns_3_4_3_1_2_begin_repeat <- intersect(country_performance_question_columns, colnames(country_survey_3_4_3_1_2_begin_repeat))
  mismatched_columns_3_4_3_1_2_begin_repeat <- setdiff(country_performance_question_columns, country_performance_columns_3_4_3_1_2_begin_repeat)
  
  country_performance_3_4_3_1_2_begin_repeat <- country_survey_3_4_3_1_2_begin_repeat %>%
    select(all_of(country_performance_columns_3_4_3_1_2_begin_repeat))%>%
    mutate_all(as.character)
  
  # Identify columns with only NA values
  na_columns_3_4_3_1_2_begin_repeat <- colSums(is.na(country_performance_3_4_3_1_2_begin_repeat)) == nrow(country_performance_3_4_3_1_2_begin_repeat)
  
  # Remove columns with only NA values
  country_performance_3_4_3_1_2_begin_repeat <- country_performance_3_4_3_1_2_begin_repeat[, !na_columns_3_4_3_1_2_begin_repeat]
  
  result_3_4_3_1_2_begin_repeat <- country_performance_3_4_3_1_2_begin_repeat%>%
    gather(key = "name_question", value = "name_choice", -kobo_farmer_id, -country,-sheet_id,-index,-parent_table_name,-parent_index)%>%
    fun_perform_left_join(country_performance_choices,.)%>%
    mutate(name_question_recla= name_question)
  return(result_3_4_3_1_2_begin_repeat)
}

##_3_4_2_2_2_begin_repeat: Livestock production 1 ----
fun_performance_3_4_2_2_2_begin_repeat<- function(country_global_choices,country_survey_3_4_2_2_2_begin_repeat){
  country_performance_choices<-  fun_performance_choices(country_global_choices)
  country_performance_question_columns<- fun_performance_questions_columns(country_performance_choices)
  
  country_performance_columns_3_4_2_2_2_begin_repeat <- intersect(country_performance_question_columns, colnames(country_survey_3_4_2_2_2_begin_repeat))
  
  mismatched_columns_3_4_2_2_2_begin_repeat <- setdiff(country_performance_question_columns, country_performance_columns_3_4_2_2_2_begin_repeat)
  
  country_performance_3_4_2_2_2_begin_repeat <- country_survey_3_4_2_2_2_begin_repeat %>%
    select(all_of(country_performance_columns_3_4_2_2_2_begin_repeat))%>%
    mutate_all(as.character)
  
  # Identify columns with only NA values
  na_columns_3_4_2_2_2_begin_repeat <- colSums(is.na(country_performance_3_4_2_2_2_begin_repeat)) == nrow(country_performance_3_4_2_2_2_begin_repeat)
  na_columns_3_4_2_2_2_begin_repeat
  
  # Remove columns with only NA values
  country_performance_3_4_2_2_2_begin_repeat <- country_performance_3_4_2_2_2_begin_repeat[, !na_columns_3_4_2_2_2_begin_repeat]
  
  result_3_4_2_2_2_begin_repeat <- country_performance_3_4_2_2_2_begin_repeat%>%
    gather(key = "name_question", value = "name_choice", -kobo_farmer_id, -country,-sheet_id,-index,-parent_table_name,-parent_index)%>%
    fun_perform_left_join(country_performance_choices,.)%>%
    mutate(name_question_recla= name_question)
  
  return(result_3_4_2_2_2_begin_repeat)
}

##_3_4_2_2_6_begin_repeat: Livestock production 2 ----
fun_performance_3_4_2_2_6_begin_repeat<- function(country_global_choices,country_survey_3_4_2_2_6_begin_repeat){
  country_performance_choices<-  fun_performance_choices(country_global_choices)
  country_performance_question_columns<- fun_performance_questions_columns(country_performance_choices)
  
  country_performance_columns_3_4_2_2_6_begin_repeat <- intersect(country_performance_question_columns, colnames(country_survey_3_4_2_2_6_begin_repeat))
  mismatched_columns_3_4_2_2_6_begin_repeat <- setdiff(country_performance_question_columns, country_performance_columns_3_4_2_2_6_begin_repeat)
  
  country_performance_3_4_2_2_6_begin_repeat <- country_survey_3_4_2_2_6_begin_repeat %>%
    select(all_of(country_performance_columns_3_4_2_2_6_begin_repeat))%>%
    mutate_all(as.character)
 
  # Identify columns with only NA values
  na_columns_3_4_2_2_6_begin_repeat <- colSums(is.na(country_performance_3_4_2_2_6_begin_repeat)) == nrow(country_performance_3_4_2_2_6_begin_repeat)
  na_columns_3_4_2_2_6_begin_repeat
  
  # Remove columns with only NA values
  country_performance_3_4_2_2_6_begin_repeat <- country_performance_3_4_2_2_6_begin_repeat[, !na_columns_3_4_2_2_6_begin_repeat]
  
  result_3_4_2_2_6_begin_repeat <- country_performance_3_4_2_2_6_begin_repeat%>%
    gather(key = "name_question", value = "name_choice", -kobo_farmer_id, -country,-sheet_id,-index,-parent_table_name,-parent_index)%>%
    fun_perform_left_join(country_performance_choices,.)%>%
    mutate(name_question_recla= name_question)
  return(result_3_4_2_2_6_begin_repeat)
}

##_3_3_4_1_3_begin_repeat: Irrigation ----
fun_performance_3_3_4_1_3_begin_repeat<- function(country_global_choices,country_survey_3_3_4_1_3_begin_repeat){
  country_performance_choices<-  fun_performance_choices(country_global_choices)
  country_performance_question_columns<- fun_performance_questions_columns(country_performance_choices)
  
  country_performance_columns_3_3_4_1_3_begin_repeat <- intersect(country_performance_question_columns, colnames(country_survey_3_3_4_1_3_begin_repeat))
  mismatched_columns_3_3_4_1_3_begin_repeat <- setdiff(country_performance_question_columns, country_performance_columns_3_3_4_1_3_begin_repeat)
  
  country_performance_3_3_4_1_3_begin_repeat <- zwe_survey_3_3_4_1_3_begin_repeat %>%
    select(all_of(country_performance_columns_3_3_4_1_3_begin_repeat))%>%
    mutate_all(as.character)
  
  # Identify columns with only NA values
  na_columns_3_3_4_1_3_begin_repeat <- colSums(is.na(country_performance_3_3_4_1_3_begin_repeat)) == nrow(country_performance_3_3_4_1_3_begin_repeat)
  na_columns_3_3_4_1_3_begin_repeat
  
  # Remove columns with only NA values
  country_performance_3_3_4_1_3_begin_repeat <- country_performance_3_3_4_1_3_begin_repeat[, !na_columns_3_3_4_1_3_begin_repeat]
  
  result_3_3_4_1_3_begin_repeat <- country_performance_3_3_4_1_3_begin_repeat%>%
    gather(key = "name_question", value = "name_choice", -kobo_farmer_id, -country,-sheet_id,-index,-parent_table_name,-parent_index)%>%
    perform_left_join(country_performance_choices,.)%>%
    mutate(name_question_recla= name_question)%>%
    mutate(name_question_recla = str_remove(name_question_recla, "/.*"))
  
  return(result_3_3_4_1_3_begin_repeat)
}

##_3_4_1_1_7_1_begin_repeat: household members permanent workers ----
fun_performance_3_4_1_1_7_1_begin_repeat<- function(country_global_choices,country_survey_3_4_1_1_7_1_begin_repeat){
  country_performance_choices<-  fun_performance_choices(country_global_choices)
  country_performance_question_columns<- fun_performance_questions_columns(country_performance_choices)
  
  country_performance_columns_3_4_1_1_7_1_begin_repeat <- intersect(country_performance_question_columns, colnames(country_survey_3_4_1_1_7_1_begin_repeat))
  mismatched_columns_3_4_1_1_7_1_begin_repeat <- setdiff(country_performance_question_columns, country_performance_columns_3_4_1_1_7_1_begin_repeat)
  print(mismatched_columns_3_4_1_1_7_1_begin_repeat)
  
  country_performance_3_4_1_1_7_1_begin_repeat <- country_survey_3_4_1_1_7_1_begin_repeat %>%
    select(all_of(country_performance_columns_3_4_1_1_7_1_begin_repeat))%>%
    mutate_all(as.character)
  
  
  # Identify columns with only NA values
  na_columns_3_4_1_1_7_1_begin_repeat <- colSums(is.na(country_performance_3_4_1_1_7_1_begin_repeat)) == nrow(country_performance_3_4_1_1_7_1_begin_repeat)
  na_columns_3_4_1_1_7_1_begin_repeat
  
  # Remove columns with only NA values
  country_performance_3_4_1_1_7_1_begin_repeat <- country_performance_3_4_1_1_7_1_begin_repeat[, !na_columns_3_4_1_1_7_1_begin_repeat]
  
  result_3_4_1_1_7_1_begin_repeat <- country_performance_3_4_1_1_7_1_begin_repeat%>%
    gather(key = "name_question", value = "name_choice", -kobo_farmer_id, -country,-sheet_id,-index,-parent_table_name,-parent_index)%>%
    fun_perform_left_join(country_performance_choices,.)%>%
    mutate(name_question_recla= name_question)
  return(result_3_4_1_1_7_1_begin_repeat)
}

##_3_4_1_1_7_2_begin_repeat: household members seasonal workers 1 ----
fun_performance_3_4_1_1_7_2_begin_repeat<- function(country_global_choices,country_survey_3_4_1_1_7_2_begin_repeat){
  country_performance_choices<-  fun_performance_choices(country_global_choices)
  country_performance_question_columns<- fun_performance_questions_columns(country_performance_choices)
  
  country_performance_columns_3_4_1_1_7_2_begin_repeat <- intersect(country_performance_question_columns, colnames(country_survey_3_4_1_1_7_2_begin_repeat))
  mismatched_columns_3_4_1_1_7_2_begin_repeat <- setdiff(country_performance_question_columns, country_performance_columns_3_4_1_1_7_2_begin_repeat)
  
  country_performance_3_4_1_1_7_2_begin_repeat <- country_survey_3_4_1_1_7_2_begin_repeat %>%
    select(all_of(country_performance_columns_3_4_1_1_7_2_begin_repeat))%>%
    mutate_all(as.character)
  
  # Identify columns with only NA values
  na_columns_3_4_1_1_7_2_begin_repeat <- colSums(is.na(country_performance_3_4_1_1_7_2_begin_repeat)) == nrow(country_performance_3_4_1_1_7_2_begin_repeat)
  na_columns_3_4_1_1_7_2_begin_repeat
  
  # Remove columns with only NA values
  country_performance_3_4_1_1_7_2_begin_repeat <- country_performance_3_4_1_1_7_2_begin_repeat[, !na_columns_3_4_1_1_7_2_begin_repeat]
  
  result_3_4_1_1_7_2_begin_repeat<- country_performance_3_4_1_1_7_2_begin_repeat%>%
    gather(key = "name_question", value = "name_choice", -kobo_farmer_id, -country,-sheet_id,-index,-parent_table_name,-parent_index)%>%
    fun_perform_left_join(country_performance_choices,.)%>%
    mutate(name_question_recla= name_question)
  return(result_3_4_1_1_7_2_begin_repeat)
}

##_3_4_1_2_7_2_1_begin_repeat : household members seasonal workers 2 ----
fun_performance_3_4_1_2_7_2_1_begin_repeat<- function(country_global_choices,country_survey_3_4_1_2_7_2_1_begin_repeat){
  country_performance_choices<-  fun_performance_choices(country_global_choices)
  country_performance_question_columns<- fun_performance_questions_columns(country_performance_choices)
  
  country_performance_columns_3_4_1_2_7_2_1_begin_repeat <- intersect(country_performance_question_columns, colnames(country_survey_3_4_1_2_7_2_1_begin_repeat))
  mismatched_columns_3_4_1_2_7_2_1_begin_repeat <- setdiff(country_performance_question_columns, country_performance_columns_3_4_1_2_7_2_1_begin_repeat)
  
  country_performance_3_4_1_2_7_2_1_begin_repeat <- country_survey_3_4_1_2_7_2_1_begin_repeat %>%
    select(all_of(country_performance_columns_3_4_1_2_7_2_1_begin_repeat))%>%
    mutate_all(as.character)
  
  # Identify columns with only NA values
  na_columns_3_4_1_2_7_2_1_begin_repeat <- colSums(is.na(country_performance_3_4_1_2_7_2_1_begin_repeat)) == nrow(country_performance_3_4_1_2_7_2_1_begin_repeat)
  na_columns_3_4_1_2_7_2_1_begin_repeat
  
  # Remove columns with only NA values
  country_performance_3_4_1_2_7_2_1_begin_repeat <- country_performance_3_4_1_2_7_2_1_begin_repeat[, !na_columns_3_4_1_2_7_2_1_begin_repeat]
  
  
  result_3_4_1_1_7_2_1_begin_repeat<- country_performance_3_4_1_2_7_2_1_begin_repeat%>%
    gather(key = "name_question", value = "name_choice", -kobo_farmer_id, -country,-sheet_id,-index,-parent_table_name,-parent_index)%>%
    fun_perform_left_join(country_performance_choices,.)%>%
    mutate(name_question_recla= name_question)
  return(result_3_4_1_1_7_2_1_begin_repeat)
}

##_3_4_1_2_1_1_begin_repeat: labour Hired/Free/Exchange Labourers permanent workers ----
fun_performance_3_4_1_2_1_1_begin_repeat<- function(country_global_choices,country_survey_3_4_1_2_1_1_begin_repeat){
  country_performance_choices<-  fun_performance_choices(country_global_choices)
  country_performance_question_columns<- fun_performance_questions_columns(country_performance_choices)
  
  country_performance_columns_3_4_1_2_1_1_begin_repeat <- intersect(country_performance_question_columns, colnames(country_survey_3_4_1_2_1_1_begin_repeat))
  mismatched_columns_3_4_1_2_1_1_begin_repeat <- setdiff(country_performance_question_columns, country_performance_columns_3_4_1_2_1_1_begin_repeat)
  
  country_performance_3_4_1_2_1_1_begin_repeat <- country_survey_3_4_1_2_1_1_begin_repeat %>%
    select(all_of(country_performance_columns_3_4_1_2_1_1_begin_repeat))%>%
    mutate_all(as.character)
  
  # Identify columns with only NA values
  na_columns_3_4_1_2_1_1_begin_repeat <- colSums(is.na(country_performance_3_4_1_2_1_1_begin_repeat)) == nrow(country_performance_3_4_1_2_1_1_begin_repeat)
  na_columns_3_4_1_2_1_1_begin_repeat
  
  # Remove columns with only NA values
  country_performance_3_4_1_2_1_1_begin_repeat <- country_performance_3_4_1_2_1_1_begin_repeat[, !na_columns_3_4_1_2_1_1_begin_repeat]
  
  result_3_4_1_2_1_1_begin_repeat<- country_performance_3_4_1_2_1_1_begin_repeat%>%
    gather(key = "name_question", value = "name_choice", -kobo_farmer_id, -country,-sheet_id,-index,-parent_table_name,-parent_index)%>%
    fun_perform_left_join(country_performance_choices,.)%>%
    mutate(name_question_recla= name_question)
  
  return(result_3_4_1_2_1_1_begin_repeat)
}

##_3_4_1_2_1_2_begin_repeat: labour Hired/Free/Exchange Labourers seasonal workers 1 ----
fun_performance_3_4_1_2_1_2_begin_repeat<- function(country_global_choices,country_survey_3_4_1_2_1_2_begin_repeat){
  country_performance_choices<-  fun_performance_choices(country_global_choices)
  country_performance_question_columns<- fun_performance_questions_columns(country_performance_choices)
  
  country_performance_columns_3_4_1_2_1_2_begin_repeat <- intersect(country_performance_question_columns, colnames(country_survey_3_4_1_2_1_2_begin_repeat))
  mismatched_columns_3_4_1_2_1_2_begin_repeat <- setdiff(country_performance_question_columns, country_performance_columns_3_4_1_2_1_2_begin_repeat)
  
  country_performance_3_4_1_2_1_2_begin_repeat <- country_survey_3_4_1_2_1_2_begin_repeat%>%
    select(all_of(country_performance_columns_3_4_1_2_1_2_begin_repeat))%>%
    mutate_all(as.character)
  
  # Identify columns with only NA values
  na_columns_3_4_1_2_1_2_begin_repeat <- colSums(is.na(country_performance_3_4_1_2_1_2_begin_repeat)) == nrow(country_performance_3_4_1_2_1_2_begin_repeat)
  
  # Remove columns with only NA values
  country_performance_3_4_1_2_1_2_begin_repeat <- country_performance_3_4_1_2_1_2_begin_repeat[, !na_columns_3_4_1_2_1_2_begin_repeat]
  
  result_3_4_1_2_1_2_begin_repeat<- country_performance_3_4_1_2_1_2_begin_repeat%>%
    gather(key = "name_question", value = "name_choice", -kobo_farmer_id, -country,-sheet_id,-index,-parent_table_name,-parent_index)%>%
    fun_perform_left_join(country_performance_choices,.)%>%
    mutate(name_question_recla= name_question)
  
  return(result_3_4_1_2_1_2_begin_repeat)
}
##_3_4_1_2_1_2_1_begin_repeat: labour Hired/Free/Exchange Labourers seasonal workers 2 ----
fun_performance_3_4_1_2_1_2_1_begin_repeat<- function(country_global_choices,country_survey_3_4_1_2_1_2_1_begin_repeat){
  country_performance_choices<-  fun_performance_choices(country_global_choices)
  country_performance_question_columns<- fun_performance_questions_columns(country_performance_choices)
  
  country_performance_columns_3_4_1_2_1_2_1_begin_repeat <- intersect(country_performance_question_columns, colnames(country_survey_3_4_1_2_1_2_1_begin_repeat))
  mismatched_columns_3_4_1_2_1_2_1_begin_repeat <- setdiff(country_performance_question_columns, country_performance_columns_3_4_1_2_1_2_1_begin_repeat)
  
  country_performance_3_4_1_2_1_2_1_begin_repeat <- country_survey_3_4_1_2_1_2_1_begin_repeat%>%
    select(all_of(country_performance_columns_3_4_1_2_1_2_1_begin_repeat))%>%
    mutate_all(as.character)
  
  # Identify columns with only NA values
  na_columns_3_4_1_2_1_2_1_begin_repeat <- colSums(is.na(country_performance_3_4_1_2_1_2_1_begin_repeat)) == nrow(country_performance_3_4_1_2_1_2_1_begin_repeat)
  
  # Remove columns with only NA values
  country_performance_3_4_1_2_1_2_1_begin_repeat <- country_performance_3_4_1_2_1_2_1_begin_repeat[, !na_columns_3_4_1_2_1_2_1_begin_repeat]
  
  result_3_4_1_2_1_2_1_begin_repeat<- country_performance_3_4_1_2_1_2_1_begin_repeat%>%
    gather(key = "name_question", value = "name_choice", -kobo_farmer_id, -country,-sheet_id,-index,-parent_table_name,-parent_index)%>%
    fun_perform_left_join(country_performance_choices,.)%>%
    mutate(name_question_recla= name_question)
  return(result_3_4_1_2_1_2_1_begin_repeat)
}
##_3_3_3_2_begin_repeat:  area of land per agricultural practice ----
fun_performance_3_3_3_2_begin_repeat<- function(country_global_choices,country_survey_3_3_3_2_begin_repeat){
  country_performance_choices<-  fun_performance_choices(country_global_choices)
  country_performance_question_columns<- fun_performance_questions_columns(country_performance_choices)
  
  country_performance_columns_3_3_3_2_begin_repeat <- intersect(country_performance_question_columns, colnames(country_survey_3_3_3_2_begin_repeat))
  mismatched_columns_3_3_3_2_begin_repeat <- setdiff(country_performance_question_columns, country_performance_columns_3_3_3_2_begin_repeat)
  
  country_performance_3_3_3_2_begin_repeat <- country_survey_3_3_3_2_begin_repeat %>%
    select(all_of(country_performance_columns_3_3_3_2_begin_repeat))%>%
    mutate_all(as.character)
  
  # Identify columns with only NA values
  na_columns_3_3_3_2_begin_repeat <- colSums(is.na(country_performance_3_3_3_2_begin_repeat)) == nrow(country_performance_3_3_3_2_begin_repeat)
  
  # Remove columns with only NA values
  country_performance_3_3_3_2_begin_repeat <- country_performance_3_3_3_2_begin_repeat[, !na_columns_3_3_3_2_begin_repeat]
  
  result_3_3_3_2_begin_repeat <- country_performance_3_3_3_2_begin_repeat%>%
    gather(key = "name_question", value = "name_choice", -kobo_farmer_id, -country,-sheet_id,-index,-parent_table_name,-parent_index)%>%
    fun_perform_left_join(country_performance_choices,.)%>%
    mutate(name_question_recla= name_question)
  return(result_3_3_3_2_begin_repeat)
}
### Function to combine answers from all performance sections ---- 
fun_performance_data<- function(country_global_choices,
                                country_survey_main, #main survey
                                #country_survey_3_4_3_1_2_begin_repeat,  
                                #country_survey_3_4_2_2_2_begin_repeat,  
                                #country_survey_3_4_2_2_6_begin_repeat,  
                                country_survey_3_3_4_1_3_begin_repeat, #Irrigation  
                                #country_survey_3_4_1_1_7_1_begin_repeat,  
                                #country_survey_3_4_1_1_7_2_begin_repeat,  
                                #country_survey_3_4_1_2_7_2_1_begin_repeat,  
                                #country_survey_3_4_1_2_1_1_begin_repeat, 
                                #country_survey_3_4_1_2_1_2_begin_repeat, 
                                #country_survey_3_4_1_2_1_2_1_begin_repeat,  
                                country_survey_3_3_3_2_begin_repeat #area of land per agricultural practice
                                ) {
  performance_data<- rbind(
    ## Main survey 
    fun_performance_main(country_global_choices, country_survey_main),
    ## _3_4_3_1_2_begin_repeat: Crop production
    #fun_performance_main(country_global_choices, country_survey_3_4_3_1_2_begin_repeat) , 
    ##_3_4_2_2_2_begin_repeat: Livestock production 1
    #fun_performance_3_4_2_2_2_begin_repeat(country_global_choices, country_survey_3_4_2_2_2_begin_repeat),  
    ##_3_4_2_2_6_begin_repeat: Livestock production 2 
    #fun_performance_3_4_2_2_6_begin_repeat(country_global_choices, country_survey_3_4_2_2_6_begin_repeat),  
    ##_3_3_4_1_3_begin_repeat: Irrigation 
    fun_performance_3_4_2_2_6_begin_repeat(country_global_choices, country_survey_3_3_4_1_3_begin_repeat) , 
    ##_3_4_1_1_7_1_begin_repeat: household members permanent workers 
    #fun_performance_3_4_1_1_7_1_begin_repeat(country_global_choices, country_survey_3_4_1_1_7_1_begin_repeat)  ,
    ##_3_4_1_1_7_2_begin_repeat: household members seasonal workers 1 ----
    #fun_performance_3_4_1_1_7_2_begin_repeat(country_global_choices, country_survey_3_4_1_1_7_2_begin_repeat),  
    ##_3_4_1_2_7_2_1_begin_repeat : household members seasonal workers 2 ----
    #fun_performance_3_4_1_2_7_2_1_begin_repeat(country_global_choices, country_survey_3_4_1_2_7_2_1_begin_repeat) , 
    ##_3_4_1_2_1_1_begin_repeat: labour Hired/Free/Exchange Labourers permanent workers ----
    #fun_performance_3_4_1_2_1_1_begin_repeat(country_global_choices, country_survey_3_4_1_2_1_1_begin_repeat) , 
    ##_3_4_1_2_1_2_begin_repeat: labour Hired/Free/Exchange Labourers seasonal workers 1 ----
    #fun_performance_3_4_1_2_1_2_begin_repeat(country_global_choices, country_survey_3_4_1_2_1_2_begin_repeat) , 
    ##_3_4_1_2_1_2_1_begin_repeat: labour Hired/Free/Exchange Labourers seasonal workers 2 ----
    #fun_performance_3_4_1_2_1_2_1_begin_repeat(country_global_choices, country_survey_3_4_1_2_1_2_1_begin_repeat)  ,
    ##_3_3_3_2_begin_repeat:  area of land per agricultural practice ----
    fun_performance_3_3_3_2_begin_repeat(country_global_choices, country_survey_3_3_3_2_begin_repeat)
  )
  return(performance_data)
}

### ----  
performance_survey<-  fun_performance_survey(global_survey)

## PERFORMACE DATA BY COUNTRY -----
# Zimbabwe -----
zwe_performance_data<-fun_performance_data(zwe_global_choices,
                                           zwe_survey_main,  ## Main survey 
                                           #zwe_survey_3_4_3_1_2_begin_repeat, ## _3_4_3_1_2_begin_repeat: Crop production 
                                           #zwe_survey_3_4_2_2_2_begin_repeat, ##_3_4_2_2_2_begin_repeat: Livestock production 1 
                                           #zwe_survey_3_4_2_2_6_begin_repeat, ##_3_4_2_2_6_begin_repeat: Livestock production 2  
                                           zwe_survey_3_3_4_1_3_begin_repeat,  ##_3_3_4_1_3_begin_repeat: Irrigation
                                           #zwe_survey_3_4_1_1_7_1_begin_repeat,  ##_3_4_1_1_7_1_begin_repeat: household members permanent workers
                                           #zwe_survey_3_4_1_1_7_2_begin_repeat,  ##_3_4_1_1_7_2_begin_repeat: household members seasonal workers 1 
                                           #zwe_survey_3_4_1_2_7_2_1_begin_repeat,  ##_3_4_1_2_7_2_1_begin_repeat : household members seasonal workers 2 
                                           #zwe_survey_3_4_1_2_1_1_begin_repeat,  ##_3_4_1_2_1_1_begin_repeat: labour Hired/Free/Exchange Labourers permanent workers 
                                           #zwe_survey_3_4_1_2_1_2_begin_repeat, ##_3_4_1_2_1_2_begin_repeat: labour Hired/Free/Exchange Labourers seasonal workers 1 
                                           #zwe_survey_3_4_1_2_1_2_1_begin_repeat,  ##_3_4_1_2_1_2_1_begin_repeat: labour Hired/Free/Exchange Labourers seasonal workers 2 
                                           zwe_survey_3_3_3_2_begin_repeat ##_3_3_3_2_begin_repeat:  area of land per agricultural practice 
)%>%
  filter(
    theme=="social"
  )%>%
  filter(
    indicator==   "land_tenure_security" )


# Tunisia-----
tun.data.path <-"C:/Users/andreasanchez/OneDrive - CGIAR/Bioversity/AI/HOLPA/HOLPA_data/Tunisia/tunisia_data_clean/holpa_household_CLEAN_2024.04.02.xlsx" #path: Andrea

tun_survey_main <- read_and_process_survey_xlsx("HOLPA_Tunisia_household_surv", "_id", tun.data.path,"tunisia","_index")%>%
  #Remove respondents that did not wanted to complete the survey
  filter(consent_2!="No")
tun_survey_3_3_3_2_begin_repeat<- read_and_process_survey_xlsx("_3_3_3_2_begin_repeat", "_submission__id", tun.data.path,"tunisia","_index") # Section: area of land per agricultural practice

tun_performance_data<-fun_performance_data(tun_global_choices,
                                           tun_survey_main, ## Main survey 
                                           #tun_survey_3_4_3_1_2_begin_repeat, ## _3_4_3_1_2_begin_repeat: Crop production 
                                           #tun_survey_3_4_2_2_2_begin_repeat, ##_3_4_2_2_2_begin_repeat: Livestock production 1 
                                           #tun_survey_3_4_2_2_6_begin_repeat, ##_3_4_2_2_6_begin_repeat: Livestock production 2  
                                           tun_survey_3_3_4_1_3_begin_repeat,  ##_3_3_4_1_3_begin_repeat: Irrigation
                                           #tun_survey_3_4_1_1_7_1_begin_repeat,  ##_3_4_1_1_7_1_begin_repeat: household members permanent workers
                                           #tun_survey_3_4_1_1_7_2_begin_repeat,  ##_3_4_1_1_7_2_begin_repeat: household members seasonal workers 1 
                                           #tun_survey_3_4_1_2_7_2_1_begin_repeat,  ##_3_4_1_2_7_2_1_begin_repeat : household members seasonal workers 2 
                                           #tun_survey_3_4_1_2_1_1_begin_repeat,  ##_3_4_1_2_1_1_begin_repeat: labour Hired/Free/Exchange Labourers permanent workers 
                                           #tun_survey_3_4_1_2_1_2_begin_repeat, ##_3_4_1_2_1_2_begin_repeat: labour Hired/Free/Exchange Labourers seasonal workers 1 
                                           #tun_survey_3_4_1_2_1_2_1_begin_repeat,  ##_3_4_1_2_1_2_1_begin_repeat: labour Hired/Free/Exchange Labourers seasonal workers 2 
                                           tun_survey_3_3_3_2_begin_repeat ##_3_3_3_2_begin_repeat:  area of land per agricultural practice 
)%>%
  filter(
    theme=="social"
  )%>%
  filter(
    indicator==   "land_tenure_security" )
  
sort(unique(tun_performance_data$indicator))

## If the farmers doesn't know the answer put 9999-----
#result2<- tun_performance_data%>%
  
    result2<- zwe_performance_data%>%

### THEME: AGRICULTURAL----
## Indicator: Crop health
# Remove rows NA for other practices
filter(!(name_question %in% c("_3_4_2_1_8_2","_3_4_2_1_8_3") & is.na(name_choice)))%>%
## Indicator: Nutrient use
  mutate(label_choice=case_when(
    name_question %in% c("_1_4_3_2_3","_1_4_3_3_3","_1_4_3_4_3")& country== "zimbabwe"~"in acres",
    name_question%in% c( "_1_4_3_2_3","_1_4_3_3_3","_1_4_3_4_3")& country== "tunisia"~"in hectares",
    TRUE ~ label_choice))%>%
  mutate(name_question_recla = case_when(
    # Rename name_question_recla (other text)
    name_question_recla=="_1_4_3_2_1_calculate"~ "_1_4_3_2_1",
    name_question_recla=="_1_4_3_3_1_calculate"~ "_1_4_3_3_1",
    name_question_recla=="_1_4_3_4_1_calculate"~"_1_4_3_4_1",
    TRUE ~ name_question_recla))%>%
  # Remove rows NA for other practices
  filter(!(name_question %in% c("_1_4_3_2_1_calculate","_1_4_3_3_1_calculate","_1_4_3_2_2","_1_4_3_2_3","_1_4_3_3_2","_1_4_3_3_3","_1_4_3_4_1_calculate","_1_4_3_4_2","_1_4_3_4_3") & is.na(name_choice)))%>%

### THEME: ENVIRONMENTAL----
## Indicator: Biodiversity abundance
mutate(label_choice = case_when(
  country == "zimbabwe" ~ gsub("\\$\\{_1_4_1_1\\}", "acre", label_choice),
  country == "tunisia" ~ gsub("\\$\\{_1_4_1_1\\}", "hectare", label_choice),
  TRUE ~ label_choice))%>%
  ## Indicator: Agrobiodiversity
  mutate(name_question_recla  = case_when(
    name_question %in% c("c1", "c2", "c3", "c4", "c5", "c6", "c7", "c8","c9", "c10", "c11", "c12", "c13", "c14", "c15", "c16", "c17", "c18", "c19", "c20")~"_3_4_3_1_1_2",
    name_question %in% c("l1", "l2", "l3", "l4", "l5", "l6", "l7", "l8", "l9", "l10") ~ "_3_4_3_3_1",
    str_detect(name_question_recla,"_3_4_3_3_1/")~str_replace(name_question_recla, "/.*", ""),
    TRUE ~ name_question_recla))%>%
  mutate(name_choice = case_when(
    str_detect(name_question,"_3_4_3_3_1/")~str_extract(name_question, "(?<=/).*"), # replace name_question by the name of the livestock
    TRUE ~ name_choice))%>%
  # Remove rows NA for other practices
  filter(!(name_question %in% c("_3_4_3_1_1") & is.na(name_choice)))%>%
  filter(!(name_question_recla %in% c("_3_4_3_1_1_2","_3_4_3_3_1","_3_4_2_2_2_3_calculate","_3_4_3_3_1") & is.na(name_choice)))%>%
## Indicator: biodiversity_cover
    # Remove rows NA for other practices
filter(!(name_question %in% c("_3_3_1_1_9_1") & is.na(name_choice)))%>%
## Indicator: biodiversity_climate_mitigation
mutate(label_choice=case_when(
  name_question %in% c("_3_3_3_2_2")& country== "zimbabwe"~"in acres",
  name_question%in% c( "_3_3_3_2_2")& country== "tunisia"~"in hectares",
  TRUE ~ label_choice))%>%
  mutate(name_question_recla = case_when(
    str_detect(name_question_recla,"_3_3_3_3/")~str_replace(name_question_recla, "/.*", ""),
    name_question_recla=="_3_3_3_3_1"~ "_3_3_3_3",
    TRUE ~ name_question_recla))%>%
  mutate(name_choice = case_when(
    str_detect(name_question,"_3_3_3_3/")~str_extract(name_question, "(?<=/).*"), # replace name_question by the name of the practice
    TRUE ~ name_choice))%>%
  # Remove rows NA for other practices
  filter(!(name_question %in% c("_3_3_3_3_1") & is.na(name_choice)))%>%
## Indicator: water
  mutate(name_question_recla = case_when(
  str_detect(name_question_recla,"_3_3_4_3_1/")~str_replace(name_question_recla, "/.*", ""),
  str_detect(name_question_recla,"_3_3_4_3_2/")~str_replace(name_question_recla, "/.*", ""),
  str_detect(name_question_recla,"_3_3_4_3_3/")~str_replace(name_question_recla, "/.*", ""),
  str_detect(name_question_recla,"_3_3_4_1_1/")~str_replace(name_question_recla, "/.*", ""),
  str_detect(name_question_recla,"_3_3_4_1_2/")~str_replace(name_question_recla, "/.*", ""),
  str_detect(name_question_recla,"_3_3_4_4/")~str_replace(name_question_recla, "/.*", ""),
  str_detect(name_question_recla,"_3_4_1_2_7_2_2_1/")~str_replace(name_question_recla, "/.*", ""),
  str_detect(name_question_recla,"_3_3_4_1_1_1")~"_3_3_4_1_1",
  str_detect(name_question_recla,"_3_3_4_1_2_1")~"_3_3_4_1_2",
  str_detect(name_question_recla,"_3_3_4_4_1")~"_3_3_4_4",
  TRUE ~ name_question_recla))%>%
  mutate(name_choice = case_when(
    str_detect(name_question,"_3_3_4_3_1/")~ label_choice, # replace name_question by the name of the month
    str_detect(name_question,"_3_3_4_3_2/")~ label_choice, # replace name_question by the name of the month
    str_detect(name_question,"_3_3_4_3_3/")~ label_choice, # replace name_question by the name of the month
    str_detect(name_question,"_3_3_4_1_1/")~ label_choice, # replace name_question by the name of the irrigation methods
    str_detect(name_question,"_3_3_4_1_2/")~ label_choice, # replace name_question by the name of the source of water for irrigation 
    str_detect(name_question,"_3_3_4_4/")~ label_choice, # replace name_question by the name of the source of water for livestock 
    str_detect(name_question,"_3_4_1_2_7_2_2_1/")~ label_choice, # replace name_question by the name of the month 
    TRUE ~ name_choice))%>%
  mutate(label_choice = case_when(
    name_question%in% c("_3_3_4_1_1_1","_3_3_4_1_2_1","_3_3_4_4_1")~"other", 
  TRUE ~ label_choice))%>%
  # Remove rows NA for other practices
  filter(!(name_question %in% c("_3_3_4_1_1_1","_3_3_4_1_2_1","_3_3_4_4_1") & is.na(name_choice)))%>%
  filter(!(name_question%in%c("_3_3_4_1_1/other","_3_3_4_1_2/other","_3_3_4_4/other")))%>%
## Indicator: energy
  mutate(name_question_recla = case_when(
    str_detect(name_question_recla,"_2_8_4_1/")~str_replace(name_question_recla, "/.*", ""),
    str_detect(name_question_recla,"_2_8_4_2/")~str_replace(name_question_recla, "/.*", ""),
    str_detect(name_question_recla,"_2_8_4_3/")~str_replace(name_question_recla, "/.*", ""),
    str_detect(name_question_recla,"_2_8_4_4/")~str_replace(name_question_recla, "/.*", ""),
    str_detect(name_question_recla,"_2_8_4_1_1")~"_2_8_4_1",
    str_detect(name_question_recla,"_2_8_4_2_1")~"_2_8_4_2",
    str_detect(name_question_recla,"_2_8_4_3_1")~"_2_8_4_3",
    str_detect(name_question_recla,"_2_8_4_3_4")~"_2_8_4_4",
    TRUE ~ name_question_recla))%>%
  
mutate(name_choice = case_when(
  str_detect(name_question,"_2_8_4_1/")~ label_choice, # replace name_question by the type of energy
  str_detect(name_question,"_2_8_4_2/")~ label_choice, # replace name_question by the type of energy
  str_detect(name_question,"_2_8_4_3/")~ label_choice, # replace name_question by the type of energy
  str_detect(name_question,"_2_8_4_4/")~ label_choice, # replace name_question by the type of energy
  TRUE ~ name_choice))%>%
  mutate(label_choice = case_when(
    name_question%in% c("_2_8_4_1_1","_2_8_4_2_1","_2_8_4_3_1","_2_8_4_3_4")~"other", 
    TRUE ~ label_choice))%>%
# Remove rows NA for other practices
filter(!(name_question %in% c("_2_8_4_1_1","_2_8_4_2_1","_2_8_4_3_1","_2_8_4_3_4") & is.na(name_choice)))%>%
  filter(!(name_question%in%c("_2_8_4_4/other")))%>%

### THEME: SOCIAL----
## Indicator: farmer_agency
# Remove rows NA for other practices
filter(!(name_question %in% c("_3_1_2_2_1","_3_1_2_3_1","_3_1_2_4_1","_3_1_2_5_1","_3_1_2_6_1","_3_1_2_7_1","_3_1_2_8",
                              "_3_1_2_2_1_audio" ,"_3_1_2_3_1_audio","_3_1_2_4_1_audio","_3_1_2_5_1_audio", "_3_1_2_7_1_audio",
                              "_3_1_2_6_1_audio","_3_1_2_8_audio","_3_1_2_8_audio","_3_1_2_3_1_audio") & is.na(name_choice)))%>%
## Indicator: land_tenure_security
  mutate(label_choice=case_when(
    name_question %in% c("_1_4_4_4_1")& country== "zimbabwe"~"in acres",
    name_question%in% c( "_1_4_4_4_1")& country== "tunisia"~"in hectares",
    TRUE ~ label_choice))%>%
  filter(!(name_question %in% c("_1_4_4_4_1") & is.na(name_choice)))

sort(unique(result2$label_question))
sort(unique(result2$name_question))
sort(unique(result2$name_question_recla))
sort(unique(result2$name_choice))

  
  ### THEME: ENVIRONMENTAL----
  ## Indicator: biodiversity_climate_mitigation
mutate(name_question_recla = case_when(
  #Remove answer code from name_question_recla
  str_detect(name_question_recla,"_3_3_3_1/")~str_replace(name_question_recla, "/.*",""),
  str_detect(name_question_recla,"_3_3_3_3/")~str_replace(name_question_recla, "/.*",""),
  # Rename name_question_recla (other text)
  name_question_recla=="_3_3_3_1_1"~ "_3_3_3_1",
  name_question_recla=="_3_3_3_3_1"~"_3_3_3_3",
  TRUE ~ name_question_recla))%>%
  # Remove rows question/other for other practices
  filter(name_question!="_3_3_3_1/other")%>%
  filter(name_question!="_3_3_3_3/other")%>%
  # replace name_choice by the name of practice
  mutate(name_choice = case_when(
    str_detect(name_question,"_3_3_3_1/")~str_extract(name_question, "(?<=/).*"), 
    str_detect(name_question,"_3_3_3_3/")~str_extract(name_question, "(?<=/).*"), 
    TRUE ~ name_choice))%>%
  
  #Replace label_question
  mutate(label_question  = case_when(
    name_question =="_3_3_3_1_1" ~ "In the past [localised description of 12 months], did you use any of these practices on your cropland?",
    name_question =="_3_3_3_3_1"~"On the grazing land (owned, leased or shared), did you apply in the last 12 months any of the following practices?",
    TRUE ~ label_question))%>%
  # Remove rows NA for other practices
  filter(!(name_question %in% c("_3_3_3_1_1") & is.na(name_choice)))%>%
  filter(!(name_question %in% c("_3_3_3_3_1") & is.na(name_choice)))%>%
  
mutate(label_choice=case_when(
    name_question =="_3_3_3_1_1"~"other practice",
    name_question =="_3_3_3_3_1"~"other practice",
    name_question== "_3_3_3_2_2"& country== "zimbabwe"~"acres",
    name_question== "_3_3_3_2_2"& country== "tunisia"~"hectares",
    TRUE ~ label_choice))

         
sort(unique(result2$indicator))

sort(unique(result2$label_question))
sort(unique(result2$name_question))
sort(unique(result2$name_question_recla))

listo
[5] "Since when has your household been farming this land?"                                                             
[6] "What was the main previous land cover?"    
[1] "In the past [localised description of 12 months], did you use any of these practices on your cropland?"  
[2] "On the grazing land (owned, leased or shared), did you apply in the last 12 months any of the following practices?"
[3] "Practices used on your cropland"                                                                                   

[4] "Provide the approximate area of land under this practice in hectares or acres:"                                    

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



##DESDE ACA ---  
  ## THEME: ENVIRONMENTAL----
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

  mutate(label_choice = case_when(name_question %in% c("l1", "l2", "l3", "l4", "l5", "l6", "l7", "l8",
                                                       "_2_8_4_3_4",
                                                       "_4_1_1_5_1_1","_4_1_1_5_2_1",
                                                       "_4_1_1_4_4_1","_4_1_2_1_10",
                                                       "_4_1_3_1_1","_4_1_3_1_2_1",
                                                       "_2_3_1_1_1","_4_1_3_2_13_1",
                                                       "_3_4_1_1_7_1_2_1",
                                                       "_3_3_4_4_1",
                                                       "_3_4_2_1_6_1_1") ~ "Other (please specify)",TRUE ~ label_choice))%>%
  
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
  
  mutate(name_question_recla = case_when(
    # Indicator: animal_health
    name_question =="_3_3_4_4_1"~"_3_3_4_4",
    # Indicator: productivity_crops
    name_question =="_3_4_2_1_5_1_2"~"_3_4_2_1_5_1",
    name_question =="_3_4_2_1_6_1_1"~"_3_4_2_1_6_1/other",
    # Indicator: productivity_livestock
    name_question =="_3_4_2_2_5_1"~"_3_4_2_2_5/other",
    name_question =="_3_4_2_2_6_1_1"~"_3_4_2_2_6_1",
    name_question =="_3_4_2_2_6_3_1"~"_3_4_2_2_6_3/other",
    str_detect(name_question_recla,"_3_4_2_2_5/")~str_replace(name_question_recla, "_[^_]*$", ""),
    str_detect(name_question_recla,"_3_4_2_2_6_3/")~str_replace(name_question_recla, "_[^_]*$", ""),
    # Indicator: nutrient_use
    str_detect(name_question_recla,"_1_4_3_1/")~"_1_4_3_1",
    TRUE ~name_question_recla))%>%
 
   mutate(name_choice = case_when(
     # Indicator: productivity_livestock
     str_detect(name_question, "_3_4_2_2_5/")~str_replace(name_choice, "_[^_]*$", ""),
     str_detect(name_question, "_3_4_2_2_6_3/")~str_replace(name_choice, "_[^_]*$", ""),
     TRUE ~name_choice))%>%

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
  
write.csv(result2,file="C:/Users/andreasanchez/OneDrive - CGIAR/Bioversity/AI/HOLPA/analysis/HOLPA/HOLPA/zwe/zwe_performance_format.csv",row.names=FALSE)


sort(unique(result2$indicator))

sort(unique(result2$label_question))
sort(unique(result2$name_question))
sort(unique(result2$name_question_recla))

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






                                                               
