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
zwe.data.path <- "D:/02_Bioversity/46_Agroecology_Initiative/holpa_results/zwe/"

#Andrea 
global.data.path <-"C:/Users/andreasanchez/OneDrive - CGIAR/Bioversity/AI/HOLPA/analysis/HOLPA/HOLPA/"
zwe.data.path <-"C:/Users/andreasanchez/OneDrive - CGIAR/Bioversity/AI/HOLPA/HOLPA_data/Zimbabwe/zimbabwe_data_clean/household_database_2024.04.18_clean.xlsx"

#### Import data ####
# Each dataset contains a survey worksheet with the questions and responses for text, open and numeric questions, and
# a choices worksheet with the response options for multiple choice questions (single or multiple).
# These need to be imported and combined.

### Country databases ####
read_and_process_survey <- function(sheet_name, column_id_rename, data_path, country_name, index) {
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


#Zimbabwe
zwe_survey <- read_and_process_survey("Final HOLPA_Zimbabwe_Household", "_id", zwe.data.path,"zimbabwe","_index")%>%
  #Remove respondents that are not farmers
  filter(kobo_farmer_id!="274186917")
zwe_survey_3_4_3_1_2_begin_repeat <- read_and_process_survey("_3_4_3_1_2_begin_repeat", "_submission__id", zwe.data.path,"zimbabwe","_index") # Section: Crop production
zwe_survey_3_4_2_2_2_begin_repeat<-read_and_process_survey("_3_4_2_2_2_begin_repeat", "_submission__id", zwe.data.path,"zimbabwe","_index") # Section: Livestock production 1
zwe_survey_3_4_2_2_6_begin_repeat<-read_and_process_survey("_3_4_2_2_6_begin_repeat", "_submission__id", zwe.data.path,"zimbabwe","_index") # Section: Livestock production 2

zwe_survey_3_4_1_1_7_1_begin_repeat<-read_and_process_survey("_3_4_1_1_7_1_begin_repeat", "_submission__id", zwe.data.path,"zimbabwe","_index")%>% # Section:labour household members permanent workers
  #Remove respondents that are not farmers
  filter(kobo_farmer_id!="274186917")
zwe_survey_3_4_1_1_7_2_begin_repeat<-read_and_process_survey("_3_4_1_1_7_2_begin_repeat", "_submission__id", zwe.data.path,"zimbabwe","_index") # Section: labour household members seasonal workers 1
zwe_survey_3_4_1_2_7_2_1_begin_repeat<-read_and_process_survey("_3_4_1_2_7_2_1_begin_repeat", "_submission__id", zwe.data.path,"zimbabwe","_index") # Section: labour household members seasonal workers 2


zwe_survey_3_4_1_2_1_1_begin_repeat<- read_and_process_survey("_3_4_1_2_1_1_begin_repeat", "_submission__id", zwe.data.path,"zimbabwe","_index") # Section: labour Hired/Free/Exchange Labourers permanent workers
zwe_survey_3_4_1_2_1_2_begin_repeat<-read_and_process_survey("_3_4_1_2_1_2_begin_repeat", "_submission__id", zwe.data.path,"zimbabwe","_index") # Section: labour Hired/Free/Exchange Labourers seasonal workers 1
zwe_survey_3_4_1_2_1_2_1_begin_repeat<-read_and_process_survey("_3_4_1_2_1_2_1_begin_repeat", "_submission__id", zwe.data.path,"zimbabwe","_index") # Section: labour Hired/Free/Exchange Labourers seasonal workers 2

zwe_survey_3_3_4_1_3_begin_repeat<- read_and_process_survey("_3_3_4_1_3_begin_repeat", "_submission__id", zwe.data.path,"zimbabwe","_index") # Section: Irrigation



#choices
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
  select("list_name","name","label::English ((en))","score_agroecology_module","country")%>%
  rename("label_choice" = "label::English ((en))")%>%
  rename("name_choice" = "name")%>%
  #Rbind country specific choices 
  distinct(list_name,name_choice,label_choice, .keep_all = TRUE) %>%
  right_join(global_survey,by="list_name",relationship="many-to-many")

#### Performance module ####
performance_eco_survey <-  global_survey %>%
  filter(str_detect(module, "performance"))%>%
  mutate(module= "performance")%>%
  filter(str_detect(indicator, "economic"))%>%
  mutate(indicator="economic") 

# CHECK land_productivity only one calculate question related
sort(unique(performance_eco_survey$subindicator))
performance_eco_survey$subindicator[str_detect(performance_eco_survey$subindicator, "climate_resilience_adaptative_capacity")]<- "climate_resilience_adaptative_capacity"
performance_eco_survey$subindicator[str_detect(performance_eco_survey$subindicator, "climate_resilience_social_network")]<- "climate_resilience_social_network"
performance_eco_survey$subindicator[str_detect(performance_eco_survey$subindicator, "climate_resilience_assets")]<- "climate_resilience_assets"
performance_eco_survey$subindicator[str_detect(performance_eco_survey$subindicator, "income")]<- "income"
performance_eco_survey$subindicator[str_detect(performance_eco_survey$subindicator, "climate_resilience_food_security")]<- "climate_resilience_food_security"
performance_eco_survey$subindicator[str_detect(performance_eco_survey$subindicator, "credit_access")]<- "credit_access"
performance_eco_survey$subindicator[str_detect(performance_eco_survey$subindicator, "climate_resilience_basic_services")]<- "climate_resilience_basic_services"
performance_eco_survey$subindicator[str_detect(performance_eco_survey$subindicator, "labour_productivity")]<- "labour_productivity"
performance_eco_survey$subindicator[str_detect(performance_eco_survey$subindicator, "economic_all")]<- "economic_all"
sort(unique(performance_eco_survey$subindicator))

performance_eco_choices <- global_choices %>%
  filter(str_detect(module, "performance"))%>%
  mutate(module= "performance") %>% 
  filter(str_detect(indicator, "economic"))%>%
  mutate(indicator="economic")

unique(performance_eco_survey$indicator)
unique(performance_eco_choices$subindicator)
performance_eco_choices$subindicator[str_detect(performance_eco_choices$subindicator, "climate_resilience_adaptative_capacity")]<- "climate_resilience_adaptative_capacity"
performance_eco_choices$subindicator[str_detect(performance_eco_choices$subindicator, "climate_resilience_social_network")]<- "climate_resilience_social_network"
performance_eco_choices$subindicator[str_detect(performance_eco_choices$subindicator, "climate_resilience_assets")]<- "climate_resilience_assets"
performance_eco_choices$subindicator[str_detect(performance_eco_choices$subindicator, "income")]<- "income"
performance_eco_choices$subindicator[str_detect(performance_eco_choices$subindicator, "climate_resilience_food_security")]<- "climate_resilience_food_security"
performance_eco_choices$subindicator[str_detect(performance_eco_choices$subindicator, "credit_access")]<- "credit_access"
performance_eco_choices$subindicator[str_detect(performance_eco_choices$subindicator, "climate_resilience_basic_services")]<- "climate_resilience_basic_services"
performance_eco_choices$subindicator[str_detect(performance_eco_choices$subindicator, "labour_productivity")]<- "labour_productivity"
performance_eco_choices$subindicator[str_detect(performance_eco_choices$subindicator, "economic_all")]<- "economic_all"
unique(performance_eco_choices$subindicator)


performance_soc_survey <-  global_survey %>%
  filter(str_detect(module, "performance"))%>%
  mutate(module= "performance")%>%
  filter(str_detect(indicator, "social"))%>%
  mutate(indicator="social")

unique(performance_soc_survey$subindicator)
performance_soc_survey$subindicator[str_detect(performance_soc_survey$subindicator, "nutrition")]<- "nutrition"
performance_soc_survey$subindicator[str_detect(performance_soc_survey$subindicator, "social_all")]<- "social_all"
unique(performance_soc_survey$subindicator)

performance_soc_choices <- global_choices %>%
  filter(str_detect(module, "performance"))%>%
  mutate(module= "performance") %>% 
  filter(str_detect(indicator, "social"))%>%
  mutate(indicator="social")

sort(unique(performance_soc_choices$subindicator))
performance_soc_choices$subindicator[str_detect(performance_soc_choices$subindicator, "nutrition")]<- "nutrition"
performance_soc_choices$subindicator[str_detect(performance_soc_choices$subindicator, "social_all")]<- "social_all"
unique(performance_soc_choices$subindicator)

performance_agr_survey <-  global_survey %>%
  filter(str_detect(module, "performance"))%>%
  mutate(module= "performance")%>%
  filter(str_detect(indicator, "agricult"))%>%
  mutate(indicator="agricultural")

unique(performance_agr_survey$indicator)
unique(performance_agr_survey$subindicator)
performance_agr_survey$subindicator[str_detect(performance_agr_survey$subindicator, "nutrient_use")]<- "nutrient_use"
performance_agr_survey$subindicator[str_detect(performance_agr_survey$subindicator, "soil_health")]<- "soil_health" # CHECK THIS ONE
performance_agr_survey$subindicator[str_detect(performance_agr_survey$subindicator, "animal_health")]<- "animal_health"
performance_agr_survey$subindicator[str_detect(performance_agr_survey$subindicator, "productivity_crops")]<- "productivity_crops"

performance_agr_survey$subindicator[str_detect(performance_agr_survey$subindicator, "productivity_livestock")]<- "productivity_livestock"
performance_agr_survey$subindicator[str_detect(performance_agr_survey$subindicator, "productivity_fish")]<- "productivity_fish"
performance_agr_survey$subindicator[str_detect(performance_agr_survey$subindicator, "crop_health")]<- "crop_health"
performance_agr_survey$subindicator[str_detect(performance_agr_survey$subindicator, "agricultural_all")]<- "agricultural_all"

sort(unique(performance_agr_survey$subindicator))

performance_agr_choices <- global_choices %>%
  filter(str_detect(module, "performance"))%>%
  mutate(module= "performance") %>% 
  filter(str_detect(indicator, "agricult"))%>%
  mutate(indicator="agricultural") 
  
# CHECK THE NEXT ONE - should not have soil health here !!!!
unique(performance_agr_choices$indicator)
unique(performance_agr_choices$subindicator)
performance_agr_choices$subindicator[str_detect(performance_agr_choices$subindicator, "nutrient_use")]<- "nutrient_use"
performance_agr_choices$subindicator[str_detect(performance_agr_choices$subindicator, "soil_health")]<- "soil_health" # CHECK THIS ONE
performance_agr_choices$subindicator[str_detect(performance_agr_choices$subindicator, "animal_health")]<- "animal_health"
performance_agr_choices$subindicator[str_detect(performance_agr_choices$subindicator, "productivity_crops")]<- "productivity_crops"
performance_agr_choices$subindicator[str_detect(performance_agr_choices$subindicator, "productivity_livestock")]<- "productivity_livestock"
performance_agr_choices$subindicator[str_detect(performance_agr_choices$subindicator, "productivity_fish")]<- "productivity_fish"
performance_agr_choices$subindicator[str_detect(performance_agr_choices$subindicator, "crop_health")]<- "crop_health"
performance_agr_choices$subindicator[str_detect(performance_agr_choices$subindicator, "agricultural_all")]<- "agricultural_all"
unique(performance_agr_choices$subindicator)


performance_env_survey <-  global_survey %>%
  filter(str_detect(module, "performance"))%>%
  mutate(module= "performance")%>%
  filter(str_detect(indicator, "environment"))%>%
  filter(!(is.na(subindicator))) %>%
  mutate(indicator="environmental") 
  
# CHECK THE BIODIVERSITY ONES - should have three in total !!!!
unique(performance_env_survey$indicator)
unique(performance_env_survey$subindicator)
performance_env_survey$subindicator[str_detect(performance_env_survey$subindicator, "energy")]<- "energy"
performance_env_survey$subindicator[str_detect(performance_env_survey$subindicator, "biodiversity_practices")]<- "biodiversity_practices"
performance_env_survey$subindicator[str_detect(performance_env_survey$subindicator, "biodiversity_abundance")]<- "biodiversity_abundance"
performance_env_survey$subindicator[str_detect(performance_env_survey$subindicator, "biodiversity_diversity")]<- "biodiversity_diversity"
performance_env_survey$subindicator[str_detect(performance_env_survey$subindicator, "biodiversity_agrobiodiversity")]<- "biodiversity_agrobiodiversity"
performance_env_survey$subindicator[str_detect(performance_env_survey$subindicator, "climate_mitigation")]<- "biodiversity_climate_mitigation"
performance_env_survey$subindicator[str_detect(performance_env_survey$subindicator, "water")]<- "water"
performance_env_survey$subindicator[str_detect(performance_env_survey$subindicator, "environmental_all")]<- "environmental_all"
unique(performance_env_survey$subindicator)
 

performance_env_choices <- global_choices %>%
  filter(str_detect(module, "performance"))%>%
  mutate(module= "performance") %>% 
  filter(str_detect(indicator, "environment"))%>%
  mutate(indicator="environmental") 

# CHECK THE BIODIVERSITY ONES - should have three in total !!!!
unique(performance_env_choices$subindicator)
performance_env_choices$subindicator[str_detect(performance_env_choices$subindicator, "energy")]<- "energy"
performance_env_choices$subindicator[str_detect(performance_env_choices$subindicator, "biodiversity_practices")]<- "biodiversity_practices"
performance_env_choices$subindicator[str_detect(performance_env_choices$subindicator, "biodiversity_abundance")]<- "biodiversity_abundance"
performance_env_choices$subindicator[str_detect(performance_env_choices$subindicator, "biodiversity_diversity")]<- "biodiversity_diversity"
performance_env_choices$subindicator[str_detect(performance_env_choices$subindicator, "biodiversity_agrobiodiversity")]<- "biodiversity_agrobiodiversity"
performance_env_choices$subindicator[str_detect(performance_env_choices$subindicator, "climate_mitigation")]<- "biodiversity_climate_mitigation"
performance_env_choices$subindicator[str_detect(performance_env_choices$subindicator, "water")]<- "water"
performance_env_choices$subindicator[str_detect(performance_env_choices$subindicator, "environmental_all")]<- "environmental_all"
unique(performance_env_choices$subindicator)

  
performance_survey <- rbind(performance_agr_survey,performance_soc_survey,performance_eco_survey,performance_env_survey) %>%
  rename(theme = indicator,
         indicator = subindicator)%>%
  filter(!str_detect(indicator, "end_repeat"))
  

performance_choices <- rbind(performance_agr_choices,performance_soc_choices,performance_eco_choices,performance_env_choices) %>%
  rename(theme = indicator,
         indicator = subindicator)%>%
  mutate(name_question_choice= if_else(type_question=="select_multiple",
                                       paste(name_question,"/",name_choice, sep=""),
                                       name_question))%>%
  filter(
    #theme=="environmental"| #ok
    #      theme=="social"|#ok
    theme== "economic"
      #theme=="agricultural"#ok
    ) %>%
  filter(
    indicator=="labour_productivity"
    )
names(performance_choices)
sort(unique(performance_choices$theme))

sort(unique(performance_choices$indicator))

sort(unique(performance_choices$type_question))
sort(unique(performance_choices$type))
sort(unique(performance_choices$name_question))

performance_questions_columns<- performance_choices%>% 
  filter(
    #theme=="environmental"|
     # theme=="social"|
      theme== "economic"
        #theme=="agricultural"
    
    )%>%
  filter(
    indicator=="labour_productivity" #questions from  _3_4_1_2_1_2_begin_repeat _3_4_1_2_1_2_1_begin_repeat _3_4_1_1_7_2_begin_repeat

  )%>%  
  dplyr::select(label_question, name_question_choice)%>%
  dplyr::distinct(name_question_choice, .keep_all = TRUE)%>%
  spread(key = name_question_choice, value = label_question)%>%
  mutate("kobo_farmer_id"="kobo_farmer_id",
         "country"="country_name",
         #"farmer_repeat_group_id"="farmer_repeat_group_id",
         "sheet_id"="sheet_id",
         "parent_table_name"="_parent_table_name",
         "index"="index",
         "parent_index"="_parent_index")


performance_questions_columns <- colnames(performance_questions_columns)
performance_questions_columns

## ANALYSIS BY COUNTRY
# Zimbabwe 
colnames(zwe_survey)
zwe_performance_columns <- intersect(performance_questions_columns, colnames(zwe_survey))
zwe_performance_columns
performance_questions_columns
mismatched_columns <- setdiff(performance_questions_columns, zwe_performance_columns)
print(mismatched_columns)

perform_left_join <- function(performance_choices, gathered_data ) {
  
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

  # Left join for "select_one"
  select_one <- gathered_data  %>%
    left_join(select(performance_choices,
                     c(name_question, name_choice, module, theme, indicator, label_choice, label_question, type, type_question, list_name)), 
              by = c("name_question"="name_question", "name_choice"="name_choice"))%>%
    filter(type_question=="select_one")

  result<- rbind(continuous,select_multiple,select_one)
  

  return(result)
}

zwe_performance <- zwe_survey %>%
  select(all_of(zwe_performance_columns))%>%
  mutate_all(as.character)

view(dfSummary(zwe_performance))

# Identify columns with only NA values
na_columns <- colSums(is.na(zwe_performance)) == nrow(zwe_performance)
na_columns

# Remove columns with only NA values
zwe_performance <- zwe_performance[, !na_columns]

view(dfSummary(zwe_performance))

result <- zwe_performance%>%
  gather(key = "name_question", value = "name_choice", -kobo_farmer_id, -country,-sheet_id,-index)%>%
  perform_left_join(performance_choices,.)%>%
  mutate(name_question_recla= name_question)%>%
  #mutate(name_question_recla = str_remove(name_question_recla, "/.*"))%>%
  mutate(parent_table_name= NA,
         parent_index=NA)
names(result)

### CHECK ALL THE BEGIN_REPEAT GROUPS PARTICULARLY LABOUR PRODUCTIVITY FROM ECONOMIC THEME
## _3_4_3_1_2_begin_repeat: Crop production ----
zwe_performance_columns_3_4_3_1_2_begin_repeat <- intersect(performance_questions_columns, colnames(zwe_survey_3_4_3_1_2_begin_repeat))
zwe_performance_columns_3_4_3_1_2_begin_repeat
performance_questions_columns
mismatched_columns_3_4_3_1_2_begin_repeat <- setdiff(performance_questions_columns, zwe_performance_columns_3_4_3_1_2_begin_repeat)
print(mismatched_columns_3_4_3_1_2_begin_repeat)

zwe_performance_3_4_3_1_2_begin_repeat <- zwe_survey_3_4_3_1_2_begin_repeat %>%
  select(all_of(zwe_performance_columns_3_4_3_1_2_begin_repeat))%>%
  mutate_all(as.character)

view(dfSummary(zwe_performance_3_4_3_1_2_begin_repeat))

# Identify columns with only NA values
na_columns_3_4_3_1_2_begin_repeat <- colSums(is.na(zwe_performance_3_4_3_1_2_begin_repeat)) == nrow(zwe_performance_3_4_3_1_2_begin_repeat)
na_columns_3_4_3_1_2_begin_repeat

# Remove columns with only NA values
zwe_performance_3_4_3_1_2_begin_repeat <- zwe_performance_3_4_3_1_2_begin_repeat[, !na_columns_3_4_3_1_2_begin_repeat]

view(dfSummary(zwe_performance_3_4_3_1_2_begin_repeat))

result_3_4_3_1_2_begin_repeat <- zwe_performance_3_4_3_1_2_begin_repeat%>%
  gather(key = "name_question", value = "name_choice", -kobo_farmer_id, -country,-sheet_id,-index,-parent_table_name,-parent_index)%>%
  perform_left_join(performance_choices,.)%>%
  mutate(name_question_recla= name_question)
  mutate(name_question_recla = str_remove(name_question_recla, "/.*"))

##_3_4_2_2_2_begin_repeat: Livestock production 1 ----
zwe_performance_columns_3_4_2_2_2_begin_repeat <- intersect(performance_questions_columns, colnames(zwe_survey_3_4_2_2_2_begin_repeat))
zwe_performance_columns_3_4_2_2_2_begin_repeat
performance_questions_columns
mismatched_columns_3_4_2_2_2_begin_repeat <- setdiff(performance_questions_columns, zwe_performance_columns_3_4_2_2_2_begin_repeat)
print(mismatched_columns_3_4_2_2_2_begin_repeat)

zwe_performance_3_4_2_2_2_begin_repeat <- zwe_survey_3_4_2_2_2_begin_repeat %>%
  select(all_of(zwe_performance_columns_3_4_2_2_2_begin_repeat))%>%
  mutate_all(as.character)

names(zwe_performance_3_4_2_2_2_begin_repeat)
view(dfSummary(zwe_performance_3_4_2_2_2_begin_repeat))

# Identify columns with only NA values
na_columns_3_4_2_2_2_begin_repeat <- colSums(is.na(zwe_performance_3_4_2_2_2_begin_repeat)) == nrow(zwe_performance_3_4_2_2_2_begin_repeat)
na_columns_3_4_2_2_2_begin_repeat

# Remove columns with only NA values
zwe_performance_3_4_2_2_2_begin_repeat <- zwe_performance_3_4_2_2_2_begin_repeat[, !na_columns_3_4_2_2_2_begin_repeat]

view(dfSummary(zwe_performance_3_4_2_2_2_begin_repeat))

result_3_4_2_2_2_begin_repeat <- zwe_performance_3_4_2_2_2_begin_repeat%>%
  gather(key = "name_question", value = "name_choice", -kobo_farmer_id, -country,-sheet_id,-index,-parent_table_name,-parent_index)%>%
  perform_left_join(performance_choices,.)%>%
  mutate(name_question_recla= name_question)
  #mutate(name_question_recla = str_remove(name_question_recla, "/.*"))

##_3_4_2_2_6_begin_repeat: Livestock production 2 ----
zwe_performance_columns_3_4_2_2_6_begin_repeat <- intersect(performance_questions_columns, colnames(zwe_survey_3_4_2_2_6_begin_repeat))
zwe_performance_columns_3_4_2_2_6_begin_repeat
performance_questions_columns
mismatched_columns_3_4_2_2_6_begin_repeat <- setdiff(performance_questions_columns, zwe_performance_columns_3_4_2_2_6_begin_repeat)
print(mismatched_columns_3_4_2_2_6_begin_repeat)

zwe_performance_3_4_2_2_6_begin_repeat <- zwe_survey_3_4_2_2_6_begin_repeat %>%
  select(all_of(zwe_performance_columns_3_4_2_2_6_begin_repeat))%>%
  mutate_all(as.character)

names(zwe_performance_3_4_2_2_6_begin_repeat)
view(dfSummary(zwe_performance_3_4_2_2_6_begin_repeat))

# Identify columns with only NA values
na_columns_3_4_2_2_6_begin_repeat <- colSums(is.na(zwe_performance_3_4_2_2_6_begin_repeat)) == nrow(zwe_performance_3_4_2_2_6_begin_repeat)
na_columns_3_4_2_2_6_begin_repeat

# Remove columns with only NA values
zwe_performance_3_4_2_2_6_begin_repeat <- zwe_performance_3_4_2_2_6_begin_repeat[, !na_columns_3_4_2_2_6_begin_repeat]

view(dfSummary(zwe_performance_3_4_2_2_6_begin_repeat))

result_3_4_2_2_6_begin_repeat <- zwe_performance_3_4_2_2_6_begin_repeat%>%
  gather(key = "name_question", value = "name_choice", -kobo_farmer_id, -country,-sheet_id,-index,-parent_table_name,-parent_index)%>%
  perform_left_join(performance_choices,.)%>%
  mutate(name_question_recla= name_question)
#mutate(name_question_recla = str_remove(name_question_recla, "/.*"))

##_3_3_4_1_3_begin_repeat: Irrigation ----
zwe_performance_columns_3_3_4_1_3_begin_repeat <- intersect(performance_questions_columns, colnames(zwe_survey_3_3_4_1_3_begin_repeat))
zwe_performance_columns_3_3_4_1_3_begin_repeat
performance_questions_columns
mismatched_columns_3_3_4_1_3_begin_repeat <- setdiff(performance_questions_columns, zwe_performance_columns_3_3_4_1_3_begin_repeat)
print(mismatched_columns_3_3_4_1_3_begin_repeat)

zwe_performance_3_3_4_1_3_begin_repeat <- zwe_survey_3_3_4_1_3_begin_repeat %>%
  select(all_of(zwe_performance_columns_3_3_4_1_3_begin_repeat))%>%
  mutate_all(as.character)
names(zwe_performance_3_3_4_1_3_begin_repeat)
view(dfSummary(zwe_performance_3_3_4_1_3_begin_repeat))

# Identify columns with only NA values
na_columns_3_3_4_1_3_begin_repeat <- colSums(is.na(zwe_performance_3_3_4_1_3_begin_repeat)) == nrow(zwe_performance_3_3_4_1_3_begin_repeat)
na_columns_3_3_4_1_3_begin_repeat

# Remove columns with only NA values
zwe_performance_3_3_4_1_3_begin_repeat <- zwe_performance_3_3_4_1_3_begin_repeat[, !na_columns_3_3_4_1_3_begin_repeat]

view(dfSummary(zwe_performance_3_3_4_1_3_begin_repeat))

result_3_3_4_1_3_begin_repeat <- zwe_performance_3_3_4_1_3_begin_repeat%>%
  gather(key = "name_question", value = "name_choice", -kobo_farmer_id, -country,-farmer_repeat_group_id)%>%
  perform_left_join(performance_choices,.)%>%
  mutate(name_question_recla= name_question)%>%
  mutate(name_question_recla = str_remove(name_question_recla, "/.*"))

##_3_4_1_1_7_1_begin_repeat: household members permanent workers ----
zwe_performance_columns_3_4_1_1_7_1_begin_repeat <- intersect(performance_questions_columns, colnames(zwe_survey_3_4_1_1_7_1_begin_repeat))
zwe_performance_columns_3_4_1_1_7_1_begin_repeat
performance_questions_columns
mismatched_columns_3_4_1_1_7_1_begin_repeat <- setdiff(performance_questions_columns, zwe_performance_columns_3_4_1_1_7_1_begin_repeat)
print(mismatched_columns_3_4_1_1_7_1_begin_repeat)

zwe_performance_3_4_1_1_7_1_begin_repeat <- zwe_survey_3_4_1_1_7_1_begin_repeat %>%
  select(all_of(zwe_performance_columns_3_4_1_1_7_1_begin_repeat))%>%
  mutate_all(as.character)

names(zwe_performance_3_4_1_1_7_1_begin_repeat)
view(dfSummary(zwe_performance_3_4_1_1_7_1_begin_repeat))

# Identify columns with only NA values
na_columns_3_4_1_1_7_1_begin_repeat <- colSums(is.na(zwe_performance_3_4_1_1_7_1_begin_repeat)) == nrow(zwe_performance_3_4_1_1_7_1_begin_repeat)
na_columns_3_4_1_1_7_1_begin_repeat

# Remove columns with only NA values
zwe_performance_3_4_1_1_7_1_begin_repeat <- zwe_performance_3_4_1_1_7_1_begin_repeat[, !na_columns_3_4_1_1_7_1_begin_repeat]

view(dfSummary(zwe_performance_3_4_1_1_7_1_begin_repeat))

result_3_4_1_1_7_1_begin_repeat <- zwe_performance_3_4_1_1_7_1_begin_repeat%>%
  gather(key = "name_question", value = "name_choice", -kobo_farmer_id, -country,-sheet_id,-index,-parent_table_name,-parent_index)%>%
  perform_left_join(performance_choices,.)%>%
  mutate(name_question_recla= name_question)
  #mutate(name_question_recla = str_remove(name_question_recla, "/.*"))

##_3_4_1_1_7_2_begin_repeat: household members seasonal workers 1 ----
zwe_performance_columns_3_4_1_1_7_2_begin_repeat <- intersect(performance_questions_columns, colnames(zwe_survey_3_4_1_1_7_2_begin_repeat))
zwe_performance_columns_3_4_1_1_7_2_begin_repeat
performance_questions_columns
mismatched_columns_3_4_1_1_7_2_begin_repeat <- setdiff(performance_questions_columns, zwe_performance_columns_3_4_1_1_7_2_begin_repeat)
print(mismatched_columns_3_4_1_1_7_2_begin_repeat)

zwe_performance_3_4_1_1_7_2_begin_repeat <- zwe_survey_3_4_1_1_7_2_begin_repeat %>%
  select(all_of(zwe_performance_columns_3_4_1_1_7_2_begin_repeat))%>%
  mutate_all(as.character)

names(zwe_performance_3_4_1_1_7_2_begin_repeat)
view(dfSummary(zwe_performance_3_4_1_1_7_2_begin_repeat))

# Identify columns with only NA values
na_columns_3_4_1_1_7_2_begin_repeat <- colSums(is.na(zwe_performance_3_4_1_1_7_2_begin_repeat)) == nrow(zwe_performance_3_4_1_1_7_2_begin_repeat)
na_columns_3_4_1_1_7_2_begin_repeat

# Remove columns with only NA values
zwe_performance_3_4_1_1_7_2_begin_repeat <- zwe_performance_3_4_1_1_7_2_begin_repeat[, !na_columns_3_4_1_1_7_2_begin_repeat]

unique(zwe_performance_3_4_1_1_7_2_begin_repeat$merge_id2)
view(dfSummary(zwe_performance_3_4_1_1_7_2_begin_repeat))

result_3_4_1_1_7_2_begin_repeat<- zwe_performance_3_4_1_1_7_2_begin_repeat%>%
  gather(key = "name_question", value = "name_choice", -kobo_farmer_id, -country,-sheet_id,-index,-parent_table_name,-parent_index)%>%
  perform_left_join(performance_choices,.)%>%
  mutate(name_question_recla= name_question)

##_3_4_1_2_7_2_1_begin_repeat : household members seasonal workers 2 ----
zwe_performance_columns_3_4_1_2_7_2_1_begin_repeat <- intersect(performance_questions_columns, colnames(zwe_survey_3_4_1_2_7_2_1_begin_repeat))
zwe_performance_columns_3_4_1_2_7_2_1_begin_repeat
performance_questions_columns
mismatched_columns_3_4_1_2_7_2_1_begin_repeat <- setdiff(performance_questions_columns, zwe_performance_columns_3_4_1_2_7_2_1_begin_repeat)
print(mismatched_columns_3_4_1_2_7_2_1_begin_repeat)

zwe_performance_3_4_1_2_7_2_1_begin_repeat <- zwe_survey_3_4_1_2_7_2_1_begin_repeat %>%
  select(all_of(zwe_performance_columns_3_4_1_2_7_2_1_begin_repeat))%>%
  mutate_all(as.character)

names(zwe_performance_3_4_1_2_7_2_1_begin_repeat)
unique(zwe_performance_3_4_1_2_7_2_1_begin_repeat$index)
view(dfSummary(zwe_performance_3_4_1_2_7_2_1_begin_repeat))

# Identify columns with only NA values
na_columns_3_4_1_2_7_2_1_begin_repeat <- colSums(is.na(zwe_performance_3_4_1_2_7_2_1_begin_repeat)) == nrow(zwe_performance_3_4_1_2_7_2_1_begin_repeat)
na_columns_3_4_1_2_7_2_1_begin_repeat

# Remove columns with only NA values
zwe_performance_3_4_1_2_7_2_1_begin_repeat <- zwe_performance_3_4_1_2_7_2_1_begin_repeat[, !na_columns_3_4_1_2_7_2_1_begin_repeat]

names(zwe_performance_3_4_1_2_7_2_1_begin_repeat)
view(dfSummary(zwe_performance_3_4_1_2_7_2_1_begin_repeat))

result_3_4_1_1_7_2_1_begin_repeat<- zwe_performance_3_4_1_2_7_2_1_begin_repeat%>%
  gather(key = "name_question", value = "name_choice", -kobo_farmer_id, -country,-sheet_id,-index,-parent_table_name,-parent_index)%>%
  perform_left_join(performance_choices,.)%>%
  mutate(name_question_recla= name_question)

names(result_3_4_1_1_7_2_1_begin_repeat)
unique(result_3_4_1_1_7_2_1_begin_repeat$parent_table_name)

##_3_4_1_2_1_1_begin_repeat: labour Hired/Free/Exchange Labourers permanent workers ----
zwe_performance_columns_3_4_1_2_1_1_begin_repeat <- intersect(performance_questions_columns, colnames(zwe_survey_3_4_1_2_1_1_begin_repeat))
zwe_performance_columns_3_4_1_2_1_1_begin_repeat
performance_questions_columns
mismatched_columns_3_4_1_2_1_1_begin_repeat <- setdiff(performance_questions_columns, zwe_performance_columns_3_4_1_2_1_1_begin_repeat)
print(mismatched_columns_3_4_1_2_1_1_begin_repeat)

zwe_performance_3_4_1_2_1_1_begin_repeat <- zwe_survey_3_4_1_2_1_1_begin_repeat %>%
  select(all_of(zwe_performance_columns_3_4_1_2_1_1_begin_repeat))%>%
  mutate_all(as.character)

view(dfSummary(zwe_performance_3_4_1_2_1_1_begin_repeat))

# Identify columns with only NA values
na_columns_3_4_1_2_1_1_begin_repeat <- colSums(is.na(zwe_performance_3_4_1_2_1_1_begin_repeat)) == nrow(zwe_performance_3_4_1_2_1_1_begin_repeat)
na_columns_3_4_1_2_1_1_begin_repeat

# Remove columns with only NA values
zwe_performance_3_4_1_2_1_1_begin_repeat <- zwe_performance_3_4_1_2_1_1_begin_repeat[, !na_columns_3_4_1_2_1_1_begin_repeat]

view(dfSummary(zwe_performance_3_4_1_2_1_1_begin_repeat))

result_3_4_1_2_1_1_begin_repeat<- zwe_performance_3_4_1_2_1_1_begin_repeat%>%
  gather(key = "name_question", value = "name_choice", -kobo_farmer_id, -country,-sheet_id,-index,-parent_table_name,-parent_index)%>%
  perform_left_join(performance_choices,.)%>%
  mutate(name_question_recla= name_question)


##_3_4_1_2_1_2_begin_repeat: labour Hired/Free/Exchange Labourers seasonal workers 1 ----
zwe_performance_columns_3_4_1_2_1_2_begin_repeat <- intersect(performance_questions_columns, colnames(zwe_survey_3_4_1_2_1_2_begin_repeat))
zwe_performance_columns_3_4_1_2_1_2_begin_repeat
performance_questions_columns
mismatched_columns_3_4_1_2_1_2_begin_repeat <- setdiff(performance_questions_columns, zwe_performance_columns_3_4_1_2_1_2_begin_repeat)
print(mismatched_columns_3_4_1_2_1_2_begin_repeat)

zwe_performance_3_4_1_2_1_2_begin_repeat <- zwe_survey_3_4_1_2_1_2_begin_repeat%>%
  select(all_of(zwe_performance_columns_3_4_1_2_1_2_begin_repeat))%>%
  mutate_all(as.character)

view(dfSummary(zwe_performance_3_4_1_2_1_2_begin_repeat))

# Identify columns with only NA values
na_columns_3_4_1_2_1_2_begin_repeat <- colSums(is.na(zwe_performance_3_4_1_2_1_2_begin_repeat)) == nrow(zwe_performance_3_4_1_2_1_2_begin_repeat)
na_columns_3_4_1_2_1_2_begin_repeat

# Remove columns with only NA values
zwe_performance_3_4_1_2_1_2_begin_repeat <- zwe_performance_3_4_1_2_1_2_begin_repeat[, !na_columns_3_4_1_2_1_2_begin_repeat]

view(dfSummary(zwe_performance_3_4_1_2_1_2_begin_repeat))

result_3_4_1_2_1_2_begin_repeat<- zwe_performance_3_4_1_2_1_2_begin_repeat%>%
  gather(key = "name_question", value = "name_choice", -kobo_farmer_id, -country,-sheet_id,-index,-parent_table_name,-parent_index)%>%
  perform_left_join(performance_choices,.)%>%
  mutate(name_question_recla= name_question)

##_3_4_1_2_1_2_1_begin_repeat: labour Hired/Free/Exchange Labourers seasonal workers 2 ----
_3_4_1_2_1_2_1_begin_repeat
zwe_performance_columns_3_4_1_2_1_2_1_begin_repeat <- intersect(performance_questions_columns, colnames(zwe_survey_3_4_1_2_1_2_1_begin_repeat))
zwe_performance_columns_3_4_1_2_1_2_1_begin_repeat
performance_questions_columns
mismatched_columns_3_4_1_2_1_2_1_begin_repeat <- setdiff(performance_questions_columns, zwe_performance_columns_3_4_1_2_1_2_1_begin_repeat)
print(mismatched_columns_3_4_1_2_1_2_1_begin_repeat)

zwe_performance_3_4_1_2_1_2_1_begin_repeat <- zwe_survey_3_4_1_2_1_2_1_begin_repeat%>%
  select(all_of(zwe_performance_columns_3_4_1_2_1_2_1_begin_repeat))%>%
  mutate_all(as.character)

view(dfSummary(zwe_performance_3_4_1_2_1_2_1_begin_repeat))

# Identify columns with only NA values
na_columns_3_4_1_2_1_2_1_begin_repeat <- colSums(is.na(zwe_performance_3_4_1_2_1_2_1_begin_repeat)) == nrow(zwe_performance_3_4_1_2_1_2_1_begin_repeat)
na_columns_3_4_1_2_1_2_1_begin_repeat

# Remove columns with only NA values
zwe_performance_3_4_1_2_1_2_1_begin_repeat <- zwe_performance_3_4_1_2_1_2_1_begin_repeat[, !na_columns_3_4_1_2_1_2_1_begin_repeat]

view(dfSummary(zwe_performance_3_4_1_2_1_2_1_begin_repeat))

result_3_4_1_2_1_2_1_begin_repeat<- zwe_performance_3_4_1_2_1_2_1_begin_repeat%>%
  gather(key = "name_question", value = "name_choice", -kobo_farmer_id, -country,-sheet_id,-index,-parent_table_name,-parent_index)%>%
  perform_left_join(performance_choices,.)%>%
  mutate(name_question_recla= name_question)

 # Combine all----
result2<- rbind(result,
                #labour_productivity
                result_3_4_1_1_7_1_begin_repeat,result_3_4_1_1_7_2_1_begin_repeat,result_3_4_1_1_7_2_begin_repeat ,
                result_3_4_1_2_1_1_begin_repeat,result_3_4_1_2_1_2_begin_repeat,result_3_4_1_2_1_2_1_begin_repeat)%>%
  #  rbind(result_3_4_2_2_2_begin_repeat, result_3_4_2_2_6_begin_repeat,result_3_4_3_1_2_begin_repeat,result_3_3_4_1_3_begin_repeat) 
  #rbind(,,result_3_4_1_1_7_2_1_begin_repeat)%>%
  ####THEME: ENVIRONMENTAL
  ###Sub-indicator: biodiversity_agrobiodiversity
  ##Number of crop/livestock/fish species produced
  mutate(name_choice = case_when(
    name_question %in% c("_3_4_3_1_1", "_3_4_2_2_2_3_calculate", "_3_4_3_4_1") & is.na(name_choice) ~ "0",
    TRUE ~ name_choice))%>%
  mutate(label_choice = case_when(
    name_question == "_3_4_3_1_1" ~ paste("produce", name_choice, "crop species", sep = " "),
    name_question == "_3_4_2_2_2_3_calculate" ~ paste("produce", name_choice, "livestock species", sep = " "),
    name_question == "_3_4_3_4_1" ~ paste("produce", name_choice, "fish species", sep = " "),
    TRUE ~ label_choice))%>%
  mutate(name_question_recla  = case_when(
    label_question==  "**In the last 12 months [add country meaning], how many different crops species (including perennial crops) were produced on your farm**"~ "_3_4_3_1_1" ,
    label_question==  "In the last 12 months [add country meaning], how many different livestock species were produced in your farm?"~ "_3_4_2_2_2_3_calculate" ,
    TRUE ~ name_question_recla))%>%

  ##Name crop species
  mutate(name_question_recla = if_else(label_question == "**In the last 12 months [add country meaning], which different crop crops species (including perennial crops) were produced on your farm**",
                      "_3_4_3_1_1_2", name_question_recla))%>%
  filter(!(name_question_recla == "_3_4_3_1_1_2" & is.na(name_choice)))%>% #Remove the rows with crop_species_name == NA 
  ##Name livestock species
  mutate(name_choice = case_when(
    type_question == "select_multiple" & name_choice == "1" ~ str_extract(name_question, "(?<=/).*"),
    #name_question_recla == "_3_4_3_3_1" & name_choice == "1" ~ str_extract(name_question, "(?<=/).*"),
    TRUE ~ name_choice))%>%
  mutate(name_question_recla = case_when(name_question %in% c("l1", "l2", "l3", "l4", "l5", "l6", "l7", "l8") ~ "_3_4_3_3_1",TRUE ~ name_question_recla))%>%
  mutate(label_choice = case_when(name_question %in% c("l1", "l2", "l3", "l4", "l5", "l6", "l7", "l8",
                                                       "_2_8_4_3_4",
                                                       "_4_1_1_5_1_1","_4_1_1_5_2_1","_4_1_1_6_1",
                                                       "_4_1_1_4_4_1","_4_1_2_1_10",
                                                       "_4_1_3_1_1","_4_1_3_1_2_1",
                                                       "_2_3_1_1_1","_4_1_3_2_13_1",
                                                       "_3_4_1_1_7_1_2_1",
                                                       "_3_3_4_4_1",
                                                       "_3_4_2_1_6_1_1") ~ "Other (please specify)",TRUE ~ label_choice))%>%
  
  mutate(label_question = case_when(
    name_question_recla== "_3_4_3_3_1" ~  "**In the last 12 months [add country meaning], which different livestock species did you keep?**",
    TRUE ~ label_question))%>%
  
  ## Indicator: biodiversity_diversity
  filter(!(name_question_recla == "_3_3_1_1_9_1" & is.na(name_choice)))%>% #Remove the rows with **Specify other landscape features:** == NA 
  
  # Indicator: biodiversity_practices
  mutate(label_choice = case_when(name_question_recla%in% c("_2_9_1_1_1","_3_3_1_7_1") ~ "other ecological practice",TRUE ~label_choice))%>%
  mutate(label_question = case_when(
    name_question_recla== "_2_9_1_1_1" ~ "**Which ecological practices do you use on cropland to improve soil quality and health?**",
    name_question_recla== "_3_3_1_7_1" ~ "**What ecological practices did you apply in the last 12 months [add country meaning] on the farm to manage crop pests?**",
    TRUE ~label_question))%>%
  mutate(name_question_recla = case_when(
    name_question_recla== "_2_9_1_1_1" ~ "_2_9_1_1",
    name_question_recla== "_3_3_1_7_1" ~ "_3_3_1_7",
    TRUE ~name_question_recla))%>%

  # Indicator: energy
  mutate(label_question = case_when(name_question_recla== "_2_8_4_3_4" ~ "**What types of energy do you use for: Cleaning, processing or transporting harvested food**",TRUE ~label_question))%>%
  mutate(name_question_recla = case_when(name_question_recla== "_2_8_4_3_4" ~ "_2_8_4_4",TRUE ~name_question_recla))%>%
  # Indicator: water
  mutate(name_choice = case_when(
    name_question_recla == "_3_4_1_2_7_2_2_1" ~ str_replace(name_choice, "//1$", paste0("//", label_choice)),TRUE ~ name_choice))%>%
  filter(!(str_ends(name_choice, "//0") & name_question_recla == "_3_4_1_2_7_2_2_1"))%>%

  ####THEME: SOCIAL
  # Indicator: 
  mutate(name_question_recla = str_replace(name_question_recla, "_audio", ""))%>%
  
  # Indicator: land tenure
  mutate(name_choice = case_when(name_question_recla%in% c("_1_4_4_1_1", "_1_4_4_1_2", "_1_4_4_2_1" ,"_1_4_4_2_2", "_1_4_4_3_1", "_1_4_4_3_2",
                                                           "_1_4_4_4_1") &
                                   is.na(name_choice)~ "0",TRUE ~name_choice))%>%
  mutate(label_choice = case_when(name_question_recla%in% c("_1_4_4_1_1", "_1_4_4_1_2", "_1_4_4_2_1" ,"_1_4_4_2_2", "_1_4_4_3_1", "_1_4_4_3_2",
                                                           "_1_4_4_4_1",
                                                           "_1_4_1_1_1","_1_4_1_1_2","_1_4_1_1_3") ~ "acres",TRUE ~label_choice))%>%
  ####THEME: ECONOMIC
  # Indicator: "climate_mitigation"
  mutate(label_question = case_when(
    name_question == "_4_1_1_5_2_1" ~ "**Credit for what types of investment**",
    name_question == "_4_1_1_5_1_1" ~"**Please indicate the source of the credit you obtained for your farming business**",
    TRUE ~label_question))%>%
  mutate(name_question_recla = case_when(
    name_question == "_4_1_1_5_2_1" ~ "_4_1_1_5_2",
    name_question =="_4_1_1_5_1_1" ~ "_4_1_1_5_1",
    TRUE ~name_question_recla))%>%

  # Indicator: climate_resilience_shocks
  mutate(label_question = case_when(
    name_question =="_4_1_3_1_1"~"**In the last 12 months, what were the most severe shocks faced by the household**",
    name_question =="_4_1_3_1_2_1"~"**What did the household members do to cope with the shocks**",
    TRUE ~label_question))%>%
  mutate(name_question_recla = case_when(
    name_question =="_4_1_3_1_1" ~ "_4_1_3_1",
    name_question =="_4_1_3_1_2_1" ~ "_4_1_3_1_2",
    TRUE ~name_question_recla))%>%

  # Indicator:
  mutate(label_question = case_when(name_question =="_2_3_1_1_1"~"**Select all the associations/organizations of which you or other HH members are a part of.**",TRUE ~label_question))%>%
  mutate(name_question_recla = case_when(name_question =="_2_3_1_1_1" ~ "_2_3_1_1",TRUE ~name_question_recla))%>%
                                           
  # Indicator: labour_productivity
  mutate(name_question_recla = case_when(
    name_question =="_3_4_1_1_7_1_2_1"~ "_3_4_1_1_7_1_2/other",
    TRUE ~name_question_recla))%>%
  
  # THEME: AGRICULTURAL
  # Indicator: animal_health
  mutate(label_question = case_when(name_question =="_3_3_4_4_1"~"**Where do you source your water for drinking water for livestock?**",TRUE ~label_question))%>%
  mutate(name_question_recla = case_when(name_question =="_3_3_4_4_1"~"_3_3_4_4",TRUE ~name_question_recla))%>%
  
  # Indicator: productivity_crops
  mutate(label_question = case_when(
    name_question =="_3_4_2_1_5_1_2"~"**Select crop production unit:**",
    name_question =="_3_4_2_1_6_1_1"~"What does the produced crop get used for?",
    TRUE ~label_question))%>%
  mutate(name_question_recla = case_when(
    name_question =="_3_4_2_1_5_1_2"~"_3_4_2_1_5_1",
    name_question =="_3_4_2_1_6_1_1"~"_3_4_2_1_6_1/other",
    TRUE ~name_question_recla))%>%

  # Indicator: productivity_livestock
   mutate(name_choice = case_when(
     str_detect(name_question, "_3_4_2_2_5/")~str_replace(name_choice, "_[^_]*$", ""),
     str_detect(name_question, "_3_4_2_2_6_3/")~str_replace(name_choice, "_[^_]*$", ""),
     TRUE ~name_choice))%>%
  mutate(label_question = case_when(
    name_question =="_3_4_2_2_5_1"~"What does the livestock get used for?",
    name_question =="_3_4_2_2_6_1_1"~"Select livestock production unit:",
    name_question =="_3_4_2_2_6_3_1"~ "What does the livestock production get used for?",
    TRUE ~label_question))%>%
  mutate(name_question_recla = case_when(
    name_question =="_3_4_2_2_5_1"~"_3_4_2_2_5/other",
    name_question =="_3_4_2_2_6_1_1"~"_3_4_2_2_6_1",
    name_question =="_3_4_2_2_6_3_1"~"_3_4_2_2_6_3/other",
    str_detect(name_question_recla,"_3_4_2_2_5/")~str_replace(name_question_recla, "_[^_]*$", ""),
    str_detect(name_question_recla,"_3_4_2_2_6_3/")~str_replace(name_question_recla, "_[^_]*$", ""),
    TRUE ~name_question_recla))%>%
  
  
  #Replace ${_1_4_1_1} in label_choice by the hectares or acres
  mutate(label_choice = gsub("\\$\\{_1_4_1_1\\}", "acres", label_choice))%>%
  mutate(label_choice =case_when(name_question %in% c("_3_4_2_1_1","_3_4_2_2_1_1","_3_4_2_2_1_2","_3_4_2_1_3")~"in acres",TRUE ~label_choice))%>%
         
                           
  #mutate(label_question = gsub("\\$\\{_1_4_1_1_calculate\\}", "acres", label_question))%>%
  #All
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
                               "_3_4_2_1_7_4","_3_4_2_2_6_1_1","_3_4_2_2_6_3_1"
                              
                               ) & is.na(name_choice)))%>% #Remove the rows with **Specify other:** == NA 
  filter(!(str_detect(name_question, "audio") & is.na(name_choice)))%>% #Remove the rows with **Specify other:** == NA 
  filter(!(name_question %in% c("_3_4_3_3_1/other", "_2_9_1_1/other", "_3_3_1_7/other","_2_8_4_4/other",
                                "_4_1_1_5_1/other","_4_1_1_5_2/other",
                                "_4_1_3_1/other","_4_1_3_1_2/other",
                                "_2_3_1_1/other",
                                "_3_4_1_1_7_1_2/other",
                                "_3_3_4_4/other",
                                "_3_4_2_1_6_1/other", "_3_4_2_2_5/other","_3_4_2_2_5/other_1",
                                "_3_4_2_2_6_3/other_10","_3_4_2_2_6_3/other_2","_3_4_2_2_6_3/other_3","_3_4_2_2_6_3/other_4","_3_4_2_2_6_3/other_6")))%>%
  filter(!(name_question %in%c("_3_4_2_1_5_1")& name_choice=="other"))
  
               


sort(unique(result2$label_question))
sort(unique(result2$name_question))
table(result2$label_question,result2$name_question)
unique(result2$sheet_id)
names(result2)
view(dfSummary(result2))

length(unique(result2$label_question))
length(unique(result2$name_question_recla))
table( result2$name_question_recla, result2$label_question)

x<-result2%>%
  mutate(x=paste(name_question_recla,label_question,sep="_"))

sort(unique(x$x))


write.csv(result2,file="C:/Users/andreasanchez/OneDrive - CGIAR/Bioversity/AI/HOLPA/analysis/HOLPA/HOLPA/zwe/zwe_performance.csv",row.names=FALSE)


#


### OBSERVATIONS
## Zimbabwe 
#ENVIRONMENTAL
#does not have data for THEME ENVIRONMENT - indicator: biodiversity (10 questions)
# questions missing  "_3_4_3_4_2" fishing production
  
# SOCIAL
#missing questions: "_3_1_2_4_1_audio" "_3_1_2_5_1_audio" "_3_1_2_6_1_audio" "_3_1_2_7_1_audio" "_3_1_3_1_2"

#ENVIRONMENTAL
#missing questions: "_3_4_2_3_2_2" fishing production; "_3_1_3_1_2"

# AGRICULTURAL
#missing: Indicator "productivity_fish"; 
#missing: indicator: animal_health _3_4_2_3_3 _1_4_3_9
#missing: crop production section area produced by diversified farming system

                                                               
