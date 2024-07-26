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
      dplyr::rename("parent_table_name" = "_parent_table_name",
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
  right_join(global_survey,by="list_name",relationship="many-to-many")%>%
  mutate(label_choice.country=NA)


### TUNISIA -----
tun.data.path <-"C:/Users/andreasanchez/OneDrive - CGIAR/Bioversity/AI/HOLPA/HOLPA_data/Tunisia/tunisia_data_clean/tun_holpa_household_survey_clean.xlsx" #path: Andrea

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
  right_join(global_survey,by="list_name",relationship="many-to-many")%>%
  mutate(label_choice.country=NA)


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
ken_survey_3_4_2_3_2_begin_repeat<- read_and_process_survey_xlsx("_3_4_2_3_2_begin_repeat", "_submission__id", ken.data.path,"kenya","_index") # Section: Fish production 1

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
  right_join(global_survey,by="list_name",relationship="many-to-many")%>%
  mutate(label_choice.country=NA)

### SENEGAL ----
sen.data.path <-"C:/Users/andreasanchez/OneDrive - CGIAR/Bioversity/AI/HOLPA/HOLPA_data/Senegal/senegal_data_clean/HOLPA_Senegal_BDD_v020724.xlsx" #path: Andrea

sen_survey_main <- read_and_process_survey_xlsx("HOLPA Senegal_version finale", "_id", sen.data.path,"senegal","_index")%>%
  #Remove respondents that did not wanted to complete the survey
  filter(consent_2!="No")%>%
  slice(-1)

sen_survey_3_4_3_1_2_begin_repeat <- read_and_process_survey_xlsx("_3_4_3_1_2_begin_repeat", "_submission__id", sen.data.path,"senegal","_index") # Section: Crop production
sen_survey_3_4_2_2_2_begin_repeat<-read_and_process_survey_xlsx("_3_4_2_2_2_begin_repeat", "_submission__id", sen.data.path,"senegal","_index")%>% # Section: Livestock production 1
  slice(-1)

sen_survey_3_4_2_2_6_begin_repeat<-read_and_process_survey_xlsx("_3_4_2_2_6_begin_repeat", "_submission__id", sen.data.path,"senegal","_index")%>% # Section: Livestock production 2
  slice(-1)
sen_survey_3_4_1_1_7_1_begin_repeat<-read_and_process_survey_xlsx("_3_4_1_1_7_1_begin_repeat", "_submission__id", sen.data.path,"senegal","_index") # Section:labour household members permanent workers
sen_survey_3_4_1_1_7_2_begin_repeat<-read_and_process_survey_xlsx("_3_4_1_1_7_2_begin_repeat", "_submission__id", sen.data.path,"senegal","_index") # Section: labour household members seasonal workers 1
sen_survey_3_4_1_2_7_2_1_begin_repeat<-read_and_process_survey_xlsx("_3_4_1_2_7_2_1_begin_repeat", "_submission__id", sen.data.path,"senegal","_index") # Section: labour household members seasonal workers 2
sen_survey_3_4_1_2_1_1_begin_repeat<- read_and_process_survey_xlsx("_3_4_1_2_1_1_begin_repeat", "_submission__id", sen.data.path,"senegal","_index") # Section: labour Hired/Free/Exchange Labourers permanent workers
sen_survey_3_4_1_2_1_2_begin_repeat<-read_and_process_survey_xlsx("_3_4_1_2_1_2_begin_repeat", "_submission__id", sen.data.path,"senegal","_index") # Section: labour Hired/Free/Exchange Labourers seasonal workers 1
sen_survey_3_4_1_2_1_2_1_begin_repeat<-read_and_process_survey_xlsx("_3_4_1_2_1_2_1_begin_repeat", "_submission__id", sen.data.path,"senegal","_index") # Section: labour Hired/Free/Exchange Labourers seasonal workers 2
sen_survey_3_3_3_2_begin_repeat<- read_and_process_survey_xlsx("_3_3_3_2_begin_repeat", "_submission__id", sen.data.path,"senegal","_index")%>% # Section: area of land per agricultural practice
  slice(-1)
sen_survey_3_3_4_1_3_begin_repeat<- read_and_process_survey_xlsx("_3_3_4_1_3_begin_repeat", "_submission__id", sen.data.path,"senegal","_index")%>% # Section: Irrigation
  slice(-1)
sen_survey_3_4_2_3_2_begin_repeat<- read_and_process_survey_xlsx("_3_4_2_3_2_begin_repeat", "_submission__id", sen.data.path,"senegal","_index")%>% # Section: Fish production 1
  slice(-1)
sen_survey_3_4_2_3_2_4_begin_repeat<- read_and_process_survey_xlsx("_3_4_2_3_2_4_begin_repeat", "_submission__id", sen.data.path,"senegal","_index")%>% # Section: Fish production 2
  slice(-1)

sen_choices <- read_excel("C:/Users/andreasanchez/OneDrive - CGIAR/Bioversity/AI/HOLPA/HOLPA_data/Senegal/senegal_data_clean/holpa_household_form_clean.xlsx",
                          sheet = "choices")%>%
  mutate(country= "senegal")%>%
  select("list_name","name","label::English ((en))","country")%>%
  rename("label_choice" = "label::English ((en))")%>%
  rename("name_choice" = "name")%>%
  distinct(list_name,name_choice,label_choice, .keep_all = TRUE)

#Add country choices to global choices
sen_global_choices<-global_choices%>%
  rbind(sen_choices)%>%
  arrange(desc(country == "global")) %>%
  #Removing duplicates
  distinct(list_name,name_choice, .keep_all = TRUE) %>%
  right_join(global_survey,by="list_name",relationship="many-to-many")%>%
  left_join(sen_choices,by=c("list_name","name_choice"))%>%
  rename("label_choice"="label_choice.x",
         "label_choice.country"="label_choice.y",
         "country"="country.x")%>%
  select(-country.y)


#### PERFORMANCE MODULE ####
fun_performance_choices<- function(country_global_choices) {
  ## Theme: Economic ----
    # Filter and mutate the data frame
    performance_eco_choices <- country_global_choices %>%
      filter(str_detect(module, "performance")) %>%
      mutate(module = "performance") %>%
      filter(str_detect(indicator, "economic")) %>%
      mutate(indicator = "economic")
    
    eco_duplicate_rows2 <- performance_eco_choices %>% filter(str_detect(subindicator, "productivity_livestock/climate_resilience_assets/biodiversity_agrobiodiversity")|
                                                                str_detect(subindicator, "productivity_livestock/climate_resilience_assets"))
    eco_duplicate_rows2$subindicator <- "productivity_livestock"
    eco_duplicate_rows3 <- performance_eco_choices %>% filter(str_detect(subindicator, "productivity_fish/climate_resilience_assets"))
    eco_duplicate_rows3$subindicator <- "productivity_fish"
    performance_eco_choices <- rbind(performance_eco_choices,eco_duplicate_rows2,eco_duplicate_rows3)
    
    # Update subindicator values
    performance_eco_choices$subindicator[str_detect(performance_eco_choices$subindicator, "income")]<- "income"
    performance_eco_choices$subindicator[str_detect(performance_eco_choices$subindicator, "productivity_livestock")]<- "productivity_livestock"
    
    
    performance_eco_choices$subindicator[str_detect(performance_eco_choices$subindicator, "climate_resilience_adaptative_capacity")]<- "climate_resilience_adaptative_capacity"
    performance_eco_choices$subindicator[str_detect(performance_eco_choices$subindicator, "climate_resilience_social_network")]<- "climate_resilience_social_network"
    performance_eco_choices$subindicator[str_detect(performance_eco_choices$subindicator, "climate_resilience_assets")]<- "climate_resilience_assets"
    performance_eco_choices$subindicator[str_detect(performance_eco_choices$subindicator, "climate_resilience_food_security")]<- "climate_resilience_food_security"
    performance_eco_choices$subindicator[str_detect(performance_eco_choices$subindicator, "credit_access/climate_resilience")]<- "climate_resilience"
    performance_eco_choices$subindicator[str_detect(performance_eco_choices$subindicator, "credit_access")]<- "credit_access"
    performance_eco_choices$subindicator[str_detect(performance_eco_choices$subindicator, "climate_resilience_basic_services")]<- "climate_resilience_basic_services"
    performance_eco_choices$subindicator[str_detect(performance_eco_choices$subindicator, "labour_productivity")]<- "labour_productivity"
    performance_eco_choices$subindicator[str_detect(performance_eco_choices$subindicator, "economic_all")]<- "economic_all"
    performance_eco_choices$subindicator[str_detect(performance_eco_choices$subindicator, "productivity_crops")]<- "productivity_crops"
    performance_eco_choices$subindicator[str_detect(performance_eco_choices$subindicator, "productivity_fish")]<- "productivity_fish"

  ## Theme: Social ----
  # Filter and mutate the data frame
  performance_soc_choices <- country_global_choices %>%
    filter(str_detect(module, "performance")) %>%
    mutate(module = "performance") %>%
    filter(str_detect(indicator, "social")) %>%
    mutate(indicator = "social")
  
  # Update subindicator values
  performance_soc_choices$subindicator[str_detect(performance_soc_choices$subindicator, "nutrition")]<- "nutrition"
  performance_soc_choices$subindicator[str_detect(performance_soc_choices$subindicator, "social_all")]<- "social_all"
  performance_soc_choices$subindicator[str_detect(performance_soc_choices$subindicator, "land_tenure_security")]<- "land_tenure_security"
  
  ## Theme: Agricultural ----
    # Filter and mutate the data frame
    performance_agr_choices <- country_global_choices %>%
      filter(str_detect(module, "performance")) %>%
      mutate(module = "performance") %>%
      filter(str_detect(indicator, "agricult")) %>%
      mutate(indicator = "agricultural")
    
    # Update subindicator values
    performance_agr_choices$subindicator[str_detect(performance_agr_choices$subindicator, "nutrient_use")]<- "nutrient_use"
    performance_agr_choices$subindicator[str_detect(performance_agr_choices$subindicator, "soil_health")]<- "soil_health" # CHECK THIS ONE
    performance_agr_choices$subindicator[str_detect(performance_agr_choices$subindicator, "animal_health")]<- "animal_health"
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
    performance_env_choices$subindicator[str_detect(performance_env_choices$subindicator, "biodiversity_agrobiodiversity")]<- "biodiversity_agrobiodiversity"
    performance_env_choices$subindicator[str_detect(performance_env_choices$subindicator, "biodiversity_abundance")]<- "biodiversity_abundance"
    performance_env_choices$subindicator[str_detect(performance_env_choices$subindicator, "biodiversity_diversity")]<- "biodiversity_diversity"
    performance_env_choices$subindicator[str_detect(performance_env_choices$subindicator, "landscape_complexity")]<- "landscape_complexity"
    performance_env_choices$subindicator[str_detect(performance_env_choices$subindicator, "water")]<- "water"
    performance_env_choices$subindicator[str_detect(performance_env_choices$subindicator, "climate_mitigation")]<- "biodiversity_climate_mitigation"
    
    performance_env_choices$subindicator[str_detect(performance_env_choices$subindicator, "biodiversity_practices")]<- "biodiversity_practices"
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
  
  # Left join for "select_one" for countries that downloaded the survey with the name_choice version (country== "zwe","ken"
  select_one1 <- gathered_data  %>%
    left_join(select(performance_choices,
                     c(name_question, name_choice, module, theme, indicator, label_choice, label_question, type, type_question, list_name)), 
              by = c("name_question"="name_question", "name_choice"="name_choice"))%>%
    filter(type_question=="select_one")%>%
    filter(country=="zimbabwe"|
             country=="kenya")
  
  # Left join for "select_one" for countries that downloaded the survey with the name_label version in English (country== "tun")
  select_one2 <- gathered_data  %>%
    left_join(select(performance_choices,
                     c(name_question, name_choice, module, theme, indicator, label_choice, label_question, type, type_question, list_name)), 
              by = c("name_question"="name_question", "name_choice"="label_choice"))%>%
    dplyr::rename("label_choice"="name_choice")%>%
    dplyr::rename("name_choice"="name_choice.y")%>%
    filter(type_question=="select_one")%>%
    filter(country=="tunisia")
  
  # Left join for "select_one" for countries that downloaded the survey with the name_label version in country language (country== "sen")
  select_one3 <- gathered_data  %>%
    left_join(select(performance_choices,
                     c(name_question, name_choice, module, theme, indicator, label_choice, label_question, type, type_question, list_name,label_choice.country)), 
              by = c("name_question"="name_question", "name_choice"="label_choice.country"))%>%
    select(-name_choice)%>%
    dplyr::rename("name_choice"="name_choice.y")%>%
    filter(type_question=="select_one")%>%
    filter(country== "senegal")

  result<- rbind(continuous,select_multiple,select_one1,select_one2,select_one3)
  
  return(result)
}

### Function to get answers from the following sections ---- 
## Main survey ----
fun_performance_main<- function(country_global_choices,country_survey_main){
  country_performance_choices<-  fun_performance_choices(country_global_choices)
  country_performance_question_columns<- fun_performance_questions_columns(country_performance_choices)
  
  country_performance_columns <- intersect(country_performance_question_columns, colnames(country_survey_main))

  country_performance <- country_survey_main %>%
    select(all_of(country_performance_columns))%>%
    mutate_all(as.character)
  
  # Identify columns with only NA values
  na_columns <- colSums(is.na(country_performance)) == nrow(country_performance)

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

## begin_repeat: questions located in begin_repeat groups  ---- 
fun_performance_begin_repeat<- function(country_global_choices,country_survey_begin_repeat){
  country_performance_choices<-  fun_performance_choices(country_global_choices)
  country_performance_question_columns<- fun_performance_questions_columns(country_performance_choices)
  
  country_performance_columns_begin_repeat <- intersect(country_performance_question_columns, colnames(country_survey_begin_repeat))

  country_performance_begin_repeat <- country_survey_begin_repeat %>%
    select(all_of(country_performance_columns_begin_repeat))%>%
    mutate_all(as.character)
  
  # Identify columns with only NA values
  na_columns_begin_repeat <- colSums(is.na(country_performance_begin_repeat)) == nrow(country_performance_begin_repeat)
  
  # Remove columns with only NA values
  country_performance_begin_repeat <- country_performance_begin_repeat[, !na_columns_begin_repeat]
  
  result_begin_repeat <- country_performance_begin_repeat%>%
    gather(key = "name_question", value = "name_choice", -kobo_farmer_id, -country,-sheet_id,-index,-parent_table_name,-parent_index)%>%
    fun_perform_left_join(country_performance_choices,.)%>%
    mutate(name_question_recla= name_question)
  return(result_begin_repeat)
}

##_3_3_4_1_3_begin_repeat: Irrigation ----
fun_performance_3_3_4_1_3_begin_repeat<- function(country_global_choices,country_survey_3_3_4_1_3_begin_repeat){
  country_performance_choices<-  fun_performance_choices(country_global_choices)
  country_performance_question_columns<- fun_performance_questions_columns(country_performance_choices)
  country_performance_columns_3_3_4_1_3_begin_repeat <- intersect(country_performance_question_columns, colnames(country_survey_3_3_4_1_3_begin_repeat))
  mismatched_columns_3_3_4_1_3_begin_repeat <- setdiff(country_performance_question_columns, country_performance_columns_3_3_4_1_3_begin_repeat)
  
  country_performance_3_3_4_1_3_begin_repeat <- country_survey_3_3_4_1_3_begin_repeat %>%
    select(all_of(country_performance_columns_3_3_4_1_3_begin_repeat))%>%
    mutate_all(as.character)
  
  # Identify columns with only NA values
  na_columns_3_3_4_1_3_begin_repeat <- colSums(is.na(country_performance_3_3_4_1_3_begin_repeat)) == nrow(country_performance_3_3_4_1_3_begin_repeat)

  # Remove columns with only NA values
  country_performance_3_3_4_1_3_begin_repeat <- country_performance_3_3_4_1_3_begin_repeat[, !na_columns_3_3_4_1_3_begin_repeat]
  
  result_3_3_4_1_3_begin_repeat <- country_performance_3_3_4_1_3_begin_repeat%>%
    gather(key = "name_question", value = "name_choice", -kobo_farmer_id, -country,-sheet_id,-index,-parent_table_name,-parent_index)%>%
    fun_perform_left_join(country_performance_choices,.)%>%
    mutate(name_question_recla= name_question)%>%
    mutate(name_question_recla = str_remove(name_question_recla, "/.*"))
  
  return(result_3_3_4_1_3_begin_repeat)
}


### Function to combine answers from all performance sections ---- 
fun_performance_data<- function(country_global_choices,
                                country_survey_main, #main survey
                                country_survey_3_4_3_1_2_begin_repeat,  ## _3_4_3_1_2_begin_repeat: Crop production
                                country_survey_3_4_2_2_2_begin_repeat,  ##_3_4_2_2_2_begin_repeat: Livestock production 1
                                country_survey_3_4_2_2_6_begin_repeat,  ##_3_4_2_2_6_begin_repeat: Livestock production 2
                                country_survey_3_3_4_1_3_begin_repeat, ##_3_3_4_1_3_begin_repeat: Irrigation  
                                country_survey_3_4_1_1_7_1_begin_repeat,   ##_3_4_1_1_7_1_begin_repeat: household members permanent workers
                                country_survey_3_4_1_1_7_2_begin_repeat,  #_3_4_1_1_7_2_begin_repeat: household members seasonal workers 1
                                country_survey_3_4_1_2_7_2_1_begin_repeat,  #_3_4_1_2_7_2_1_begin_repeat : household members seasonal workers 2
                                country_survey_3_4_1_2_1_1_begin_repeat, #_3_4_1_2_1_1_begin_repeat: labour Hired/Free/Exchange Labourers permanent workers
                                country_survey_3_4_1_2_1_2_begin_repeat, #_3_4_1_2_1_2_begin_repeat: labour Hired/Free/Exchange Labourers seasonal workers 1
                                country_survey_3_4_1_2_1_2_1_begin_repeat,  #_3_4_1_2_1_2_1_begin_repeat: labour Hired/Free/Exchange Labourers seasonal workers 2
                                country_survey_3_3_3_2_begin_repeat, #area of land per agricultural practice
                                country_survey_3_4_2_3_2_begin_repeat #_3_4_2_3_2_repeat_group: Fish production
                                ) {
  performance_data<- rbind(
    fun_performance_main(country_global_choices, country_survey_main), ## Main survey
    fun_performance_begin_repeat(country_global_choices, country_survey_3_4_3_1_2_begin_repeat) , ## _3_4_3_1_2_begin_repeat: Crop production 
    fun_performance_begin_repeat(country_global_choices, country_survey_3_4_2_2_2_begin_repeat),  ##_3_4_2_2_2_begin_repeat: Livestock production 1 
    fun_performance_begin_repeat(country_global_choices, country_survey_3_4_2_2_6_begin_repeat),  ##_3_4_2_2_6_begin_repeat: Livestock production 2
    fun_performance_3_3_4_1_3_begin_repeat(country_global_choices, country_survey_3_3_4_1_3_begin_repeat), ##_3_3_4_1_3_begin_repeat: Irrigation
    fun_performance_begin_repeat(country_global_choices, country_survey_3_4_1_1_7_1_begin_repeat), ##_3_4_1_1_7_1_begin_repeat: household members permanent workers
    fun_performance_begin_repeat(country_global_choices, country_survey_3_4_1_1_7_2_begin_repeat),  ##_3_4_1_1_7_2_begin_repeat: household members seasonal workers 1 ----
    fun_performance_begin_repeat(country_global_choices, country_survey_3_4_1_2_7_2_1_begin_repeat), ##_3_4_1_2_7_2_1_begin_repeat : household members seasonal workers 2 ----
    fun_performance_begin_repeat(country_global_choices, country_survey_3_4_1_2_1_1_begin_repeat), ##_3_4_1_2_1_1_begin_repeat: labour Hired/Free/Exchange Labourers permanent workers ----
    fun_performance_begin_repeat(country_global_choices, country_survey_3_4_1_2_1_2_begin_repeat), ##_3_4_1_2_1_2_begin_repeat: labour Hired/Free/Exchange Labourers seasonal workers 1 ----
    fun_performance_begin_repeat(country_global_choices, country_survey_3_4_1_2_1_2_1_begin_repeat), ##_3_4_1_2_1_2_1_begin_repeat: labour Hired/Free/Exchange Labourers seasonal workers 2 ----
    fun_performance_begin_repeat(country_global_choices, country_survey_3_3_3_2_begin_repeat), ##_3_3_3_2_begin_repeat:  area of land per agricultural practice ----
    fun_performance_begin_repeat(country_global_choices, country_survey_3_4_2_3_2_begin_repeat) ##_3_4_2_3_2_repeat_group:  Fish production ----
    
    )
  return(performance_data)
}

## PERFORMACE DATA FORMAT BY COUNTRY -----
# Zimbabwe -----
zwe_performance_data<-fun_performance_data(zwe_global_choices,
                                           zwe_survey_main,  ## Main survey 
                                           zwe_survey_3_4_3_1_2_begin_repeat, ## _3_4_3_1_2_begin_repeat: Crop production 
                                           zwe_survey_3_4_2_2_2_begin_repeat, ##_3_4_2_2_2_begin_repeat: Livestock production 1 
                                           zwe_survey_3_4_2_2_6_begin_repeat, ##_3_4_2_2_6_begin_repeat: Livestock production 2  
                                           zwe_survey_3_3_4_1_3_begin_repeat,  ##_3_3_4_1_3_begin_repeat: Irrigation
                                           zwe_survey_3_4_1_1_7_1_begin_repeat,  ##_3_4_1_1_7_1_begin_repeat: household members permanent workers
                                           zwe_survey_3_4_1_1_7_2_begin_repeat,  ##_3_4_1_1_7_2_begin_repeat: household members seasonal workers 1 
                                           zwe_survey_3_4_1_2_7_2_1_begin_repeat,  ##_3_4_1_2_7_2_1_begin_repeat : household members seasonal workers 2 
                                           zwe_survey_3_4_1_2_1_1_begin_repeat,  ##_3_4_1_2_1_1_begin_repeat: labour Hired/Free/Exchange Labourers permanent workers 
                                           zwe_survey_3_4_1_2_1_2_begin_repeat, ##_3_4_1_2_1_2_begin_repeat: labour Hired/Free/Exchange Labourers seasonal workers 1 
                                           zwe_survey_3_4_1_2_1_2_1_begin_repeat,  ##_3_4_1_2_1_2_1_begin_repeat: labour Hired/Free/Exchange Labourers seasonal workers 2 
                                           zwe_survey_3_3_3_2_begin_repeat ##_3_3_3_2_begin_repeat:  area of land per agricultural practice 
)

# Tunisia-----
tun_performance_data<-fun_performance_data(tun_global_choices,
                                           tun_survey_main, ## Main survey 
                                           tun_survey_3_4_3_1_2_begin_repeat, ## _3_4_3_1_2_begin_repeat: Crop production 
                                           tun_survey_3_4_2_2_2_begin_repeat, ##_3_4_2_2_2_begin_repeat: Livestock production 1 
                                           tun_survey_3_4_2_2_6_begin_repeat, ##_3_4_2_2_6_begin_repeat: Livestock production 2  
                                           tun_survey_3_3_4_1_3_begin_repeat,  ##_3_3_4_1_3_begin_repeat: Irrigation
                                           tun_survey_3_4_1_1_7_1_begin_repeat,  ##_3_4_1_1_7_1_begin_repeat: household members permanent workers
                                           tun_survey_3_4_1_1_7_2_begin_repeat,  ##_3_4_1_1_7_2_begin_repeat: household members seasonal workers 1 
                                           tun_survey_3_4_1_2_7_2_1_begin_repeat,  ##_3_4_1_2_7_2_1_begin_repeat : household members seasonal workers 2 
                                           tun_survey_3_4_1_2_1_1_begin_repeat,  ##_3_4_1_2_1_1_begin_repeat: labour Hired/Free/Exchange Labourers permanent workers 
                                           tun_survey_3_4_1_2_1_2_begin_repeat, ##_3_4_1_2_1_2_begin_repeat: labour Hired/Free/Exchange Labourers seasonal workers 1 
                                           tun_survey_3_4_1_2_1_2_1_begin_repeat,  ##_3_4_1_2_1_2_1_begin_repeat: labour Hired/Free/Exchange Labourers seasonal workers 2 
                                           tun_survey_3_3_3_2_begin_repeat ##_3_3_3_2_begin_repeat:  area of land per agricultural practice 
)


# Kenya -----
ken_performance_data<-fun_performance_data(ken_global_choices,
                                           ken_survey_main,  ## Main survey 
                                           ken_survey_3_4_3_1_2_begin_repeat, ## _3_4_3_1_2_begin_repeat: Crop production 
                                           ken_survey_3_4_2_2_2_begin_repeat, ##_3_4_2_2_2_begin_repeat: Livestock production 1 
                                           ken_survey_3_4_2_2_6_begin_repeat, ##_3_4_2_2_6_begin_repeat: Livestock production 2  
                                           ken_survey_3_3_4_1_3_begin_repeat,  ##_3_3_4_1_3_begin_repeat: Irrigation
                                           ken_survey_3_4_1_1_7_1_begin_repeat,  ##_3_4_1_1_7_1_begin_repeat: household members permanent workers
                                           ken_survey_3_4_1_1_7_2_begin_repeat,  ##_3_4_1_1_7_2_begin_repeat: household members seasonal workers 1 
                                           ken_survey_3_4_1_2_7_2_1_begin_repeat,  ##_3_4_1_2_7_2_1_begin_repeat : household members seasonal workers 2 
                                           ken_survey_3_4_1_2_1_1_begin_repeat,  ##_3_4_1_2_1_1_begin_repeat: labour Hired/Free/Exchange Labourers permanent workers 
                                           ken_survey_3_4_1_2_1_2_begin_repeat, ##_3_4_1_2_1_2_begin_repeat: labour Hired/Free/Exchange Labourers seasonal workers 1 
                                           ken_survey_3_4_1_2_1_2_1_begin_repeat,  ##_3_4_1_2_1_2_1_begin_repeat: labour Hired/Free/Exchange Labourers seasonal workers 2 
                                           ken_survey_3_3_3_2_begin_repeat, ##_3_3_3_2_begin_repeat:  area of land per agricultural practice 
                                           ken_survey_3_4_2_3_2_begin_repeat ##_3_4_2_3_2_repeat_group:  Fish production 1
                                           
)%>%

filter(
  theme=="economic"
)%>%
  filter(
    indicator==   "labour_productivity" )

[1] "1_recycling"                            "2_input_reduction"                      "agricultural_all"                       ""                         
[5] ""                 ""          ""        ""                    
[9] ""                 "biodiversity_practices"                 "climate_resilience"                     "climate_resilience_adaptative_capacity"
[13] "climate_resilience_assets"              "climate_resilience_basic_services"      "climate_resilience_shocks"              "climate_resilience_social_network"     
[17] ""                            ""                           ""                                 "environmental_all"                     
[21] ""                          ""                                 ""                    "land_tenure"                           
[25] ""                   ""                   "membership"                             ""                          

#Cosas para hacer lunes:
# arreglar los codigos de senegal si me mandan la cuenta de kobo
# arreglar el codigo para importar c1c2c3 desde el repeat group para senegal
# arreglar el codigo para que fish production sea agregado solo si el pais es kenya y senegal #priority

crop_health
animal_health
soil_health
nutrient_use
biodiversity_diversity
biodiversity_abundance
nutrition
farmer_agency
land_tenure_security
wellbeing
social_all
income
productivity_livestock #it is not ready for senegal
productivity_fish #it is not ready for senegal
productivity_crops #it is not ready for senegal
economic_all
labour_productivity

energy
biodiversity_agrobiodiversity #it is not ready for senegal falta crops
biodiversity_cover
landscape_complexity
water
biodiversity_climate_mitigation

sort(unique(ken_performance_data$indicator))
# Senegal -----
# crop_health missing indicator
sen_performance_data<-fun_performance_data(sen_global_choices,
                                           sen_survey_main,  ## Main survey 
                                           sen_survey_3_4_3_1_2_begin_repeat, ## _3_4_3_1_2_begin_repeat: Crop production 
                                           sen_survey_3_4_2_2_2_begin_repeat, ##_3_4_2_2_2_begin_repeat: Livestock production 1 
                                           sen_survey_3_4_2_2_6_begin_repeat, ##_3_4_2_2_6_begin_repeat: Livestock production 2  
                                           sen_survey_3_3_4_1_3_begin_repeat,  ##_3_3_4_1_3_begin_repeat: Irrigation
                                           sen_survey_3_4_1_1_7_1_begin_repeat,  ##_3_4_1_1_7_1_begin_repeat: household members permanent workers
                                           sen_survey_3_4_1_1_7_2_begin_repeat,  ##_3_4_1_1_7_2_begin_repeat: household members seasonal workers 1 
                                           sen_survey_3_4_1_2_7_2_1_begin_repeat,  ##_3_4_1_2_7_2_1_begin_repeat : household members seasonal workers 2 
                                           sen_survey_3_4_1_2_1_1_begin_repeat,  ##_3_4_1_2_1_1_begin_repeat: labour Hired/Free/Exchange Labourers permanent workers 
                                           sen_survey_3_4_1_2_1_2_begin_repeat, ##_3_4_1_2_1_2_begin_repeat: labour Hired/Free/Exchange Labourers seasonal workers 1 
                                           sen_survey_3_4_1_2_1_2_1_begin_repeat,  ##_3_4_1_2_1_2_1_begin_repeat: labour Hired/Free/Exchange Labourers seasonal workers 2 
                                           sen_survey_3_3_3_2_begin_repeat, ##_3_3_3_2_begin_repeat:  area of land per agricultural practice 
                                           sen_survey_3_4_2_3_2_begin_repeat ##_3_4_2_3_2_repeat_group:  Fish production 1
                                           

)%>%
  
  filter(
    theme=="economic"
  )%>%
  filter(
    indicator==   "productivity_livestock" )
 
## If the farmers doesn't know the answer put 9999-----
#result2<- tun_performance_data%>%
  
  #  result2<- zwe_performance_data%>%

 result2<- ken_performance_data%>%
  #   result2<- sen_performance_data%>%
   
### THEME: ENVIRONMENTAL----
## Indicator: biodiversity_abundance
mutate(label_choice = case_when(
  country== "kenya" & kobo_farmer_id == "286844609"~ gsub("\\$\\{_1_4_1_1\\}", "hectare", label_choice),
  country== "senegal" & kobo_farmer_id == "308802823"~gsub("\\$\\{_1_4_1_1\\}", "acre", label_choice),
  country %in% c("zimbabwe","kenya")~ gsub("\\$\\{_1_4_1_1\\}", "acre", label_choice),
  country %in% c("tunisia","senegal") ~ gsub("\\$\\{_1_4_1_1\\}", "hectare", label_choice),
  TRUE ~ label_choice))%>%
  
## Indicator: biodiversity_agrobiodiversity
  mutate(name_question_recla  = case_when(
    name_question %in% c("c1", "c2", "c3", "c4", "c5", "c6", "c7", "c8","c9", "c10", "c11", "c12", "c13", "c14", "c15", "c16", "c17", "c18", "c19", "c20")~"_3_4_3_1_1_2",
    name_question %in% c("l1", "l2", "l3", "l4", "l5", "l6", "l7", "l8", "l9", "l10") ~ "_3_4_3_3_1",
    name_question %in% c("f1", "f2", "f3", "f4", "f5", "f6", "f7", "f8", "f9", "f10") ~ "_3_4_3_4_2",
    TRUE ~ name_question_recla))%>%
  #specify the area unit for cropland, grassland, fish production area
  mutate(label_choice= case_when(
    name_question %in% c("_3_4_2_1_1","_3_4_2_2_1_1", "_3_4_2_2_1_2","_3_4_2_3_2")& country== "kenya" & kobo_farmer_id == "286844609"~"hectares",
    name_question%in% c("_3_4_2_1_1", "_3_4_2_2_1_1", "_3_4_2_2_1_2","_3_4_2_3_2")& country== "senegal" & kobo_farmer_id == "308802823"~"metres square",
    name_question%in% c("_3_4_2_1_1", "_3_4_2_2_1_1", "_3_4_2_2_1_2","_3_4_2_3_2")& country %in%c("zimbabwe","kenya")~"acres",
    name_question%in% c("_3_4_2_1_1", "_3_4_2_2_1_1", "_3_4_2_2_1_2","_3_4_2_3_2")& country %in% c("tunisia","senegal")~"hectares",
    TRUE ~ label_choice))%>%
  #For the countries that translated the name of the crops, livestock and fish to English separated with "//"
  mutate(name_choice= case_when(
    country %in% c("senegal")&name_question_recla == "_3_4_3_4_2" & grepl("//", name_choice)~ sub(".*//", "", name_choice),
    TRUE ~ name_choice))%>%
  #Remove _3_4_3_3_1/other
  filter(name_question!="_3_4_3_3_1/other")%>%

  ## Indicator: biodiversity_climate_mitigation
mutate(label_choice=case_when(
  name_question %in% c("_3_3_3_2_2")& country== "kenya" & kobo_farmer_id == "286844609"~"hectares",
  name_question%in% c("_3_3_3_2_2")& country== "senegal" & kobo_farmer_id == "308802823"~"metres square",
  name_question%in% c("_3_3_3_2_2")& country %in%c("zimbabwe","kenya")~"acres",
  name_question%in% c("_3_3_3_2_2")& country %in% c("tunisia","senegal")~"hectares",
  TRUE ~ label_choice))%>%
  mutate(name_question_recla = case_when(
    name_question_recla=="_3_3_3_3_1"~ "_3_3_3_3",
    TRUE ~ name_question_recla))%>%
  mutate(label_choice = case_when(name_question%in% c("_3_3_3_3_1")~"other", TRUE ~ label_choice))%>%
  
## Indicator: energy
  mutate(name_question_recla = case_when(
    str_detect(name_question_recla,"_2_8_4_1_1")~"_2_8_4_1",
    str_detect(name_question_recla,"_2_8_4_2_1")~"_2_8_4_2",
    str_detect(name_question_recla,"_2_8_4_3_1")~"_2_8_4_3",
    str_detect(name_question_recla,"_2_8_4_3_4")~"_2_8_4_4",
    TRUE ~ name_question_recla))%>%
 mutate(label_choice = case_when(
    name_question%in% c("_2_8_4_1_1","_2_8_4_2_1","_2_8_4_3_1","_2_8_4_3_4")~"other", 
    TRUE ~ label_choice))%>%
# Remove rows for other practices
  filter(!(name_question%in%c("_2_8_4_4/other","_2_8_4_1/other","_2_8_4_3/other","_2_8_4_2/other")))%>%
### THEME: SOCIAL----
## Indicator: land_tenure_security
  mutate(label_choice=case_when(
    name_question%in% c( "_1_4_4_4_1")& country== "kenya" & kobo_farmer_id == "286844609"~"hectares",
    name_question%in% c( "_1_4_4_4_1")& country== "senegal" & kobo_farmer_id == "308802823"~"metres square",
    name_question%in% c( "_1_4_4_4_1")& country %in%c("zimbabwe","kenya")~"acres",
    name_question%in% c( "_1_4_4_4_1")& country %in% c("tunisia","senegal")~"hectares",
    TRUE ~ label_choice))%>%

  ### THEME: ECONOMIC----
##Indicator: productivity_livestock
#For the countries that translated the name of the crops, livestock and fish to English separated with "//"
  mutate(name_choice= case_when(
    country %in% c("senegal")&name_question_recla %in%c("_3_4_2_2_2_calculate","_3_4_2_2_5_2_calculate" )& grepl("//", name_choice)~ sub(".*//", "", name_choice),
    TRUE ~ name_choice))%>%
  
### THEME: ENVIRONMENTAL, AGRONOMIC, SOCIAL, ECONOMIC----
  ##Indicator: environmental_all, economic_all, productivity_crops, nutrient_use
#Put the unit of area for all necessary questions
mutate(label_choice= case_when(
  name_question %in% c("_1_4_1_1_1", "_1_4_1_1_2", "_1_4_1_1_3","_3_4_2_1_3", "_1_4_3_2_3","_1_4_3_3_3","_1_4_3_4_3")& country== "kenya" & kobo_farmer_id == "286844609"~"hectares",
  name_question%in% c( "_1_4_1_1_1", "_1_4_1_1_2", "_1_4_1_1_3","_3_4_2_1_3", "_1_4_3_2_3","_1_4_3_3_3","_1_4_3_4_3")& country== "senegal" & kobo_farmer_id == "308802823"~"metres square",
  name_question%in% c( "_1_4_1_1_1", "_1_4_1_1_2", "_1_4_1_1_3","_3_4_2_1_3", "_1_4_3_2_3","_1_4_3_3_3","_1_4_3_4_3")& country %in%c("zimbabwe","kenya")~"acres",
  name_question%in% c( "_1_4_1_1_1", "_1_4_1_1_2", "_1_4_1_1_3","_3_4_2_1_3", "_1_4_3_2_3","_1_4_3_3_3","_1_4_3_4_3")& country %in% c("tunisia","senegal")~"hectares",
  TRUE ~ label_choice))%>%
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
sort(unique(result2$type_question))

  
write.csv(result2,file="C:/Users/andreasanchez/OneDrive - CGIAR/Bioversity/AI/HOLPA/analysis/HOLPA/HOLPA/zwe/zwe_performance_format.csv",row.names=FALSE)
write.csv(result2,file="C:/Users/andreasanchez/OneDrive - CGIAR/Bioversity/AI/HOLPA/analysis/HOLPA/HOLPA/tun/tun_performance_format.csv",row.names=FALSE)
write.csv(result2,file="C:/Users/andreasanchez/OneDrive - CGIAR/Bioversity/AI/HOLPA/analysis/HOLPA/HOLPA/sen/sen_performance_format.csv",row.names=FALSE)
write.csv(result2,file="C:/Users/andreasanchez/OneDrive - CGIAR/Bioversity/AI/HOLPA/analysis/HOLPA/HOLPA/ken/ken_performance_format.csv",row.names=FALSE)


#calculate max yield per main crop
main_crop_yield<- result2%>%
  filter(indicator=="productivity_crops")%>%
  filter(name_question_recla=="_3_4_3_1_3_calculate"|
           name_question_recla== "_3_4_2_1_5_1_calculate"|
           name_question_recla=="_3_4_2_1_5_2"|
           name_question_recla=="_3_4_2_1_3")%>%
  mutate(merge_id = case_when(sheet_id=="Final HOLPA_Zimbabwe_Household"~paste(kobo_farmer_id,sheet_id,index,sep = "_"),
                              TRUE ~paste(kobo_farmer_id,parent_table_name,parent_index,index,sep="_")))%>%
  select(sheet_id,name_choice,name_question_recla,kobo_farmer_id,index,country)

x_3_4_3_1_3_calculate<-main_crop_yield%>%
  filter(name_question_recla=="_3_4_3_1_3_calculate")
x_3_4_2_1_5_1_calculate<- main_crop_yield%>%
  filter(name_question_recla== "_3_4_2_1_5_1_calculate")
x_3_4_2_1_5_2<-main_crop_yield%>%
  filter(name_question_recla=="_3_4_2_1_5_2")
x_3_4_2_1_3<-main_crop_yield%>%
  filter(name_question_recla=="_3_4_2_1_3")%>%
  mutate(name_choice= as.numeric(name_choice))%>%
  mutate(name_choice= case_when(
    country== "zimbabwe"~ (name_choice*0.404686),
    TRUE ~name_choice))

max_yield_crops<-
  dplyr::left_join(x_3_4_3_1_3_calculate,x_3_4_2_1_5_1_calculate,by=c("sheet_id","kobo_farmer_id","index","country"))%>%
  left_join(x_3_4_2_1_5_2, by= c("sheet_id","kobo_farmer_id","index","country"))%>%
  left_join(x_3_4_2_1_3, by= c("sheet_id","kobo_farmer_id","index","country"))%>%
  rename("crop_name"="name_choice.x",
         "crop_production_unit"="name_choice.y",
         "crop_production"="name_choice.x.x",
         "cropland_area"= "name_choice.y.y")%>%
  mutate(crop_production=as.numeric(crop_production),
         yield= crop_production/cropland_area)%>%
  group_by(crop_name,crop_production_unit) %>%
  summarize(max_crop_yield_ha = max(yield, na.rm = TRUE))%>%
  ungroup()
  
  
sort(unique(max_yield_crops$label_question))
sort(unique(main_crop_yield$name_question_recla))
sort(unique(main_crop_yield$sheet_id))
sort(unique(main_crop_yield$merge_id))
write.csv(max_yield_crops,file='tun/tun_crops_list.csv',row.names=FALSE)
write.csv(max_yield_crops,file='zwe/zwe_crops_list.csv',row.names=FALSE)

