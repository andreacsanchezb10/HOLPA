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
read_and_process_survey <- function(sheet_name, column_id_rename, data_path,country_name,index) {
  survey_data <- read_excel(path = data_path,
                            sheet = sheet_name) %>%
    mutate(country = country_name) %>%
    rename("kobo_farmer_id" := !!column_id_rename ) %>%
    rename("repeat_group_id" := !!index ) %>%
    mutate(farmer_repeat_group_id= paste(kobo_farmer_id,repeat_group_id,sep = "_" ))%>%
    slice(-1)
  return(survey_data)
}

#Zimbabwe
zwe_survey <- read_and_process_survey("Final HOLPA_Zimbabwe_Household", "_id", zwe.data.path,"zimbabwe","_index")%>%
  mutate("_2_6_1_4_6"= NA)%>%
  mutate("_2_7_1_6/other"= NA)
  
zwe_survey_3_4_3_1_2_begin_repeat <- read_and_process_survey("_3_4_3_1_2_begin_repeat", "_submission__id", zwe.data.path,"zimbabwe","_index") # Section: Crop production
zwe_survey_3_3_4_1_3_begin_repeat<- read_and_process_survey("_3_3_4_1_3_begin_repeat", "_submission__id", zwe.data.path,"zimbabwe","_index") # Section: Irrigation
zwe_survey_3_4_2_2_2_begin_repeat<-read_and_process_survey("_3_4_2_2_2_begin_repeat", "_submission__id", zwe.data.path,"zimbabwe","_index") # Section: Livestock production
zwe_survey_3_4_1_1_7_1_begin_repeat<-read_and_process_survey("_3_4_1_1_7_1_begin_repeat", "_submission__id", zwe.data.path,"zimbabwe","_index") # Section: household members permanent workers


zwe_survey_3_4_1_1_7_2_begin_repeat<-read_and_process_survey("_3_4_1_1_7_2_begin_repeat", "_submission__id", zwe.data.path,"zimbabwe","_index") # Section: household members seasonal workers

zwe_survey_3_4_1_2_1_2_begin_repeat<-read_and_process_survey("_3_4_1_2_1_2_begin_repeat", "_submission__id", zwe.data.path,"zimbabwe","_index") 



names(zwe_survey_3_4_1_1_7_1_begin_repeat)

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
performance_eco_survey$subindicator[str_detect(performance_eco_survey$subindicator, "land_tenure/economic_all/environmental_all/social_all")]<- "economic_all"

unique(performance_eco_survey$indicator)
unique(performance_eco_survey$subindicator)

performance_eco_choices <- global_choices %>%
  filter(str_detect(module, "performance"))%>%
  mutate(module= "performance") %>% 
  filter(str_detect(indicator, "economic"))%>%
  mutate(indicator="economic")

unique(performance_eco_choices$subindicator)
performance_eco_choices$subindicator[str_detect(performance_eco_choices$subindicator, "climate_resilience_adaptative_capacity")]<- "climate_resilience_adaptative_capacity"
performance_eco_choices$subindicator[str_detect(performance_eco_choices$subindicator, "climate_resilience_social_network")]<- "climate_resilience_social_network"
performance_eco_choices$subindicator[str_detect(performance_eco_choices$subindicator, "climate_resilience_assets")]<- "climate_resilience_assets"
performance_eco_choices$subindicator[str_detect(performance_eco_choices$subindicator, "income")]<- "income"
performance_eco_choices$subindicator[str_detect(performance_eco_choices$subindicator, "climate_resilience_food_security")]<- "climate_resilience_food_security"
performance_eco_choices$subindicator[str_detect(performance_eco_choices$subindicator, "credit_access")]<- "credit_access"
performance_eco_choices$subindicator[str_detect(performance_eco_choices$subindicator, "climate_resilience_basic_services")]<- "climate_resilience_basic_services"
performance_eco_choices$subindicator[str_detect(performance_eco_choices$subindicator, "labour_productivity")]<- "labour_productivity"
performance_eco_choices$subindicator[str_detect(performance_eco_choices$subindicator, "land_tenure/economic_all/environmental_all/social_all")]<- "economic_all"
unique(performance_eco_choices$subindicator)


performance_soc_survey <-  global_survey %>%
  filter(str_detect(module, "performance"))%>%
  mutate(module= "performance")%>%
  filter(str_detect(indicator, "social"))%>%
  mutate(indicator="social")

unique(performance_soc_survey$subindicator)
performance_soc_survey$subindicator[str_detect(performance_soc_survey$subindicator, "nutrition")]<- "nutrition"
performance_soc_survey$subindicator[str_detect(performance_soc_survey$subindicator, "land_tenure/economic_all/environmental_all/social_all")]<- "social_all"

unique(performance_soc_survey$indicator)
unique(performance_soc_survey$subindicator)
unique(performance_soc_choices$subindicator)

performance_soc_choices <- global_choices %>%
  filter(str_detect(module, "performance"))%>%
  mutate(module= "performance") %>% 
  filter(str_detect(indicator, "social"))%>%
  mutate(indicator="social")

sort(unique(performance_soc_choices$subindicator))
performance_soc_choices$subindicator[str_detect(performance_soc_choices$subindicator, "nutrition")]<- "nutrition"
performance_soc_choices$subindicator[str_detect(performance_soc_choices$subindicator, "land_tenure/economic_all/environmental_all/social_all")]<- "social_all"
unique(performance_soc_choices$subindicator)



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
  mutate(indicator="environmental") 
  
# CHECK THE BIODIVERSITY ONES - should have three in total !!!!
sort(unique(performance_env_survey$subindicator))
performance_env_survey$subindicator[str_detect(performance_env_survey$subindicator, "energy")]<- "energy"
performance_env_survey$subindicator[str_detect(performance_env_survey$subindicator, "biodiversity_practices")]<- "biodiversity_practices"
performance_env_survey$subindicator[str_detect(performance_env_survey$subindicator, "biodiversity_abundance")]<- "biodiversity_abundance"
performance_env_survey$subindicator[str_detect(performance_env_survey$subindicator, "biodiversity_diversity")]<- "biodiversity_diversity"
performance_env_survey$subindicator[str_detect(performance_env_survey$subindicator, "biodiversity_agrobiodiversity")]<- "biodiversity_agrobiodiversity"
performance_env_survey$subindicator[str_detect(performance_env_survey$subindicator, "climate_mitigation")]<- "biodiversity_climate_mitigation"
performance_env_survey$subindicator[str_detect(performance_env_survey$subindicator, "water")]<- "water"
performance_env_survey$subindicator[str_detect(performance_env_survey$subindicator, "land_tenure/economic_all/environmental_all/social_all")]<- "environmental_all"

sort(unique(performance_env_survey$subindicator))
 

performance_env_choices <- global_choices %>%
  filter(str_detect(module, "performance"))%>%
  mutate(module= "performance") %>% 
  filter(str_detect(indicator, "environment"))%>%
  mutate(indicator="environmental") 

# CHECK THE BIODIVERSITY ONES - should have three in total !!!!
sort(unique(performance_env_choices$subindicator))
performance_env_choices$subindicator[str_detect(performance_env_choices$subindicator, "energy")]<- "energy"
performance_env_choices$subindicator[str_detect(performance_env_choices$subindicator, "biodiversity_practices")]<- "biodiversity_practices"
performance_env_choices$subindicator[str_detect(performance_env_choices$subindicator, "biodiversity_abundance")]<- "biodiversity_abundance"
performance_env_choices$subindicator[str_detect(performance_env_choices$subindicator, "biodiversity_diversity")]<- "biodiversity_diversity"
performance_env_choices$subindicator[str_detect(performance_env_choices$subindicator, "biodiversity_agrobiodiversity")]<- "biodiversity_agrobiodiversity"
performance_env_choices$subindicator[str_detect(performance_env_choices$subindicator, "climate_mitigation")]<- "biodiversity_climate_mitigation"
performance_env_choices$subindicator[str_detect(performance_env_choices$subindicator, "water")]<- "water"
performance_env_choices$subindicator[str_detect(performance_env_choices$subindicator, "land_tenure/economic_all/environmental_all/social_all")]<- "environmental_all"

sort(unique(performance_env_choices$subindicator))

  
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
    )%>%
  filter(
    #indicator=="climate_resilience"|
    #indicator=="climate_resilience_adaptative_capacity"|
    #indicator=="climate_resilience_assets"|
    #indicator=="climate_resilience_basic_services"
      #indicator=="climate_resilience_food_security"|
      #indicator=="climate_resilience_shocks"|
      #indicator=="climate_resilience_social_network"|
      #        indicator=="credit_access"|
      #indicator=="income"
    indicator=="labour_productivity"
      #indicator=="land_productivity"
    )
names(performance_choices)
sort(unique(performance_choices$theme))

sort(unique(performance_choices$indicator))

sort(unique(performance_choices$type_question))
sort(unique(performance_choices$type))
sort(unique(performance_choices$name_question))

performance_questions_columns<- performance_choices%>% 
  filter(
    theme=="environmental"|
      theme=="social"|
      theme== "economic"
    )%>%
  filter(
    #indicator=="climate_resilience"|#ok
    #indicator=="climate_resilience_adaptative_capacity"|#ok
    #indicator=="climate_resilience_assets" |#ok   
    #indicator=="climate_resilience_basic_services"#ok
    #indicator=="climate_resilience_food_security"|#ok
    # indicator=="climate_resilience_shocks"|#ok
    #indicator=="climate_resilience_social_network"|#ok
    #indicator=="credit_access"|#ok
    #indicator=="income"#ok
    indicator=="labour_productivity" #questions from  _3_4_1_2_1_2_begin_repeat _3_4_1_2_1_2_1_begin_repeat _3_4_1_1_7_2_begin_repeat
  )%>%  
  dplyr::select(label_question, name_question_choice)%>%
  dplyr::distinct(name_question_choice, .keep_all = TRUE)%>%
  spread(key = name_question_choice, value = label_question)%>%
  mutate("kobo_farmer_id"="kobo_farmer_id",
         "country"="country_name",
         "farmer_repeat_group_id"="farmer_repeat_group_id")


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
  gather(key = "name_question", value = "name_choice", -kobo_farmer_id, -country,-farmer_repeat_group_id)%>%
  perform_left_join(performance_choices,.)%>%
  mutate(name_question_recla= name_question)%>%
  mutate(name_question_recla = str_remove(name_question_recla, "/.*"))%>%
  mutate(farmer_repeat_group_id= NA)
    
names(result)
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
  gather(key = "name_question", value = "name_choice", -kobo_farmer_id, -country,-farmer_repeat_group_id)%>%
  perform_left_join(performance_choices,.)%>%
  mutate(name_question_recla= name_question)%>%
  mutate(name_question_recla = str_remove(name_question_recla, "/.*"))

##_3_4_2_2_2_begin_repeat: Livestock production ----
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
  gather(key = "name_question", value = "name_choice", -kobo_farmer_id, -country,-farmer_repeat_group_id)%>%
  perform_left_join(performance_choices,.)%>%
  mutate(name_question_recla= name_question)%>%
  mutate(name_question_recla = str_remove(name_question_recla, "/.*"))

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
  gather(key = "name_question", value = "name_choice", -kobo_farmer_id, -country,-farmer_repeat_group_id)%>%
  perform_left_join(performance_choices,.)%>%
  mutate(name_question_recla= name_question)%>%
  mutate(name_question_recla = str_remove(name_question_recla, "/.*"))

##_3_4_1_1_7_2_begin_repeat: household members seasonal workers ----
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

view(dfSummary(zwe_performance_3_4_1_1_7_2_begin_repeat))

result_3_4_1_1_7_2_begin_repeat<- zwe_performance_3_4_1_1_7_2_begin_repeat%>%
  gather(key = "name_question", value = "name_choice", -kobo_farmer_id, -country,-farmer_repeat_group_id)%>%
  perform_left_join(performance_choices,.)%>%
  mutate(name_question_recla= name_question)%>%
  mutate(name_question_recla = str_remove(name_question_recla, "/.*"))


result2<- result%>%
  #rbind(result_3_4_2_2_2_begin_repeat,result_3_4_3_1_2_begin_repeat,result_3_3_4_1_3_begin_repeat) 
  rbind(result_3_4_1_1_7_1_begin_repeat)%>%
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
    TRUE ~ name_question_recla))%>%
  mutate(name_question_recla  = case_when(
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
                                                       "_3_4_1_1_7_1_2_1") ~ "other",TRUE ~ label_choice))%>%
  
  mutate(label_question = case_when(
    name_question_recla== "_3_4_3_3_1" ~  "**In the last 12 months [add country meaning], which different livestock species did you keep?**",
    TRUE ~ label_question))%>%
  
  ## Indicator: biodiversity_diversity
  filter(!(name_question_recla == "_3_3_1_1_9_1" & is.na(name_choice)))%>% #Remove the rows with **Specify other landscape features:** == NA 
  # Indicator: biodiversity_practices
  mutate(label_choice = case_when(name_question_recla%in% c("_2_9_1_1_1","_3_3_1_7_1") ~ "other ecological practice",TRUE ~label_choice))%>%
  mutate(label_question = case_when(name_question_recla== "_2_9_1_1_1" ~ "**Which ecological practices do you use on cropland to improve soil quality and health?**",TRUE ~label_question))%>%
  mutate(name_question_recla = case_when(name_question_recla== "_2_9_1_1_1" ~ "_2_9_1_1",TRUE ~name_question_recla))%>%
  mutate(label_question = case_when(name_question_recla== "_3_3_1_7_1" ~ "**What ecological practices did you apply in the last 12 months [add country meaning] on the farm to manage crop pests?**",TRUE ~label_question))%>%
  mutate(name_question_recla = case_when(name_question_recla== "_3_3_1_7_1" ~ "_3_3_1_7",TRUE ~name_question_recla))%>%

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
  mutate(label_question = case_when(name_question == "_4_1_1_5_2_1" ~ "**Credit for what types of investment**",TRUE ~label_question))%>%
  mutate(name_question_recla = case_when(name_question == "_4_1_1_5_2_1" ~ "_4_1_1_5_2",TRUE ~name_question_recla))%>%
  mutate(label_question = case_when(name_question == "_4_1_1_5_1_1" ~"**Please indicate the source of the credit you obtained for your farming business**",TRUE ~label_question))%>%
  mutate(name_question_recla = case_when(name_question =="_4_1_1_5_1_1" ~ "_4_1_1_5_1",TRUE ~name_question_recla))%>%
  
  # Indicator: climate_resilience_shocks
  mutate(label_question = case_when(name_question =="_4_1_3_1_1"~"**In the last 12 months, what were the most severe shocks faced by the household**",TRUE ~label_question))%>%
  mutate(name_question_recla = case_when(name_question =="_4_1_3_1_1" ~ "_4_1_3_1",TRUE ~name_question_recla))%>%
  mutate(label_question = case_when(name_question =="_4_1_3_1_2_1"~"**What did the household members do to cope with the shocks**",TRUE ~label_question))%>%
  mutate(name_question_recla = case_when(name_question =="_4_1_3_1_2_1" ~ "_4_1_3_1_2",TRUE ~name_question_recla))%>%
  
  # Indicator:
  mutate(label_question = case_when(name_question =="_2_3_1_1_1"~"**Select all the associations/organizations of which you or other HH members are a part of.**",TRUE ~label_question))%>%
  mutate(name_question_recla = case_when(name_question =="_2_3_1_1_1" ~ "_2_3_1_1",TRUE ~name_question_recla))%>%
                                           
  # Indicator: labour_productivity
  mutate(label_question = case_when(name_question =="_3_4_1_1_7_1_2_1"~"In what production activities did these hh members work?",TRUE ~label_question))%>%
  mutate(name_question_recla = case_when(name_question =="_3_4_1_1_7_1_2_1"~ "_3_4_1_1_7_1_2",TRUE ~name_question_recla))%>%
#Replace ${_1_4_1_1} in label_choice by the hectares or acres
  mutate(label_choice = gsub("\\$\\{_1_4_1_1\\}", "acres", label_choice))%>%
  mutate(label_question = gsub("\\$\\{_1_4_1_1_calculate\\}", "acres", label_question))%>%

  #All
  filter(!(name_question %in% c("_3_3_1_1_9_1", "_2_9_1_1_1", "_3_3_1_7_1","_2_8_4_3_4",
                                "_3_1_2_2_1","_3_1_2_8",
                                "_4_1_1_5_2_1","_4_1_1_5_1_1","_4_1_1_6_1",
                                "_4_1_1_4_4_1","_2_4_1_2","_4_1_2_1_10","_4_1_2_1_2","_4_1_2_1_4","_4_1_2_1_5","_4_1_2_1_6","_4_1_2_1_7","_4_1_2_1_9",
                                "_4_1_3_1_1","_4_1_3_1_2_1",
                                "_2_3_1_1_1","_4_1_3_2_13_1",
                                "_3_2_1_2_1",
                                "_3_4_1_1_1_1","_3_4_1_1_1_2","_3_4_1_1_2_1","_3_4_1_1_2_2","_3_4_1_1_3_1","_3_4_1_1_3_2","_3_4_1_1_4_1","_3_4_1_1_4_2","_3_4_1_1_5_1","_3_4_1_1_5_2","_3_4_1_1_6_1","_3_4_1_1_6_2",
                               "_3_4_1_1_7_1_2_1") & is.na(name_choice)))%>% #Remove the rows with **Specify other:** == NA 
  filter(!(str_detect(name_question, "audio") & is.na(name_choice)))%>% #Remove the rows with **Specify other:** == NA 
  filter(!(name_question %in% c("_3_4_3_3_1/other", "_2_9_1_1/other", "_3_3_1_7/other","_2_8_4_4/other",
                                "_4_1_1_5_1/other","_4_1_1_5_2/other",
                                "_4_1_3_1/other","_4_1_3_1_2/other",
                                "_2_3_1_1/other",
                                "_3_4_1_1_7_1_2/other")))


sort(unique(result2$label_question))
sort(unique(result2$name_question))

view(dfSummary(result2))

length(unique(result2$label_question))
length(unique(result2$name_question_recla))
table( result2$name_question_recla, result2$label_question)

x<-result2%>%
  mutate(x=paste(name_question_recla,label_question,sep="_"))

sort(unique(x$x))




write.csv(result2,file="C:/Users/andreasanchez/OneDrive - CGIAR/Bioversity/AI/HOLPA/analysis/HOLPA/HOLPA/zwe/zwe_performance.csv",row.names=FALSE)



### OBSERVATIONS
## Zimbabwe 
#ENVIRONMENTAL
#does not have data for THEME ENVIRONMENT - indicator: biodiversity (10 questions)
# questions missing  "_3_4_3_4_2" fishing production
  
# SOCIAL
#missing questions: "_3_1_2_4_1_audio" "_3_1_2_5_1_audio" "_3_1_2_6_1_audio" "_3_1_2_7_1_audio" "_3_1_3_1_2"

#ENVIRONMENTAL
#missing questions: "_3_4_2_3_2_2" fishing production; "_3_1_3_1_2"



> print(mismatched_columns)
                                                                  
                                                                     
                                                                     
[68] "_3_4_1_1_7_2_1"                                                                        
[69] "_3_4_1_1_7_2_calculate"                                                                
[70] "_3_4_1_2_1_1_1"                                                                        
[71] "_3_4_1_2_1_1_2"                                                                        
[72] "_3_4_1_2_1_1_3/Crops"                                                                  
[73] "_3_4_1_2_1_1_3/Fish"                                                                   
[74] "_3_4_1_2_1_1_3/Honey"                                                                  
[75] "_3_4_1_2_1_1_3/Livestock"                                                              
[76] "_3_4_1_2_1_1_3/other"                                                                  
[77] "_3_4_1_2_1_1_3/Trees"                                                                  
[78] "_3_4_1_2_1_1_3_1/Cleaning"                                                             
[79] "_3_4_1_2_1_1_3_1/Drying"                                                               
[80] "_3_4_1_2_1_1_3_1/Harvesting"                                                           
[81] "_3_4_1_2_1_1_3_1/Land_preparation"                                                     
[82] "_3_4_1_2_1_1_3_1/other"                                                                
[83] "_3_4_1_2_1_1_3_1/Planting"                                                             
[84] "_3_4_1_2_1_1_3_1/Processing_(milling,_grinding,_grating,_etc)"                         
[85] "_3_4_1_2_1_1_3_1/Protection_from_losses"                                               
[86] "_3_4_1_2_1_1_3_1/Ridging,_fertilizing,_other_non-harvested_activities"                 
[87] "_3_4_1_2_1_1_3_1/Shelling/Threshing/Peeling"                                           
[88] "_3_4_1_2_1_1_3_1/Sorting"                                                              
[89] "_3_4_1_2_1_1_3_1/Supervision"                                                          
[90] "_3_4_1_2_1_1_3_1/Weeding"                                                              
[91] "_3_4_1_2_1_1_3_1_1"                                                                    
[92] "_3_4_1_2_1_1_3_2/Animal_feeding"                                                       
[93] "_3_4_1_2_1_1_3_2/Egg_collection"                                                       
[94] "_3_4_1_2_1_1_3_2/Health_care_and_veterinary_services"                                  
[95] "_3_4_1_2_1_1_3_2/Herd_management"                                                      
[96] "_3_4_1_2_1_1_3_2/Management_of_housing_and_shelter_(e.g.,_cleaning,_repairs)"          
[97] "_3_4_1_2_1_1_3_2/Marketing"                                                            
[98] "_3_4_1_2_1_1_3_2/Milking_and_dairy_management"                                         
[99] "_3_4_1_2_1_1_3_2/other"                                                                
[100] "_3_4_1_2_1_1_3_2/Pasture_management"                                                   
[101] "_3_4_1_2_1_1_3_2/Waste_management"                                                     
[102] "_3_4_1_2_1_1_3_2/Wool_management"                                                      
[103] "_3_4_1_2_1_1_3_2_1"                                                                    
[104] "_3_4_1_2_1_1_3_3/Disease_prevention_and_health_management"                             
[105] "_3_4_1_2_1_1_3_3/Feeding_and_nutrition"                                                
[106] "_3_4_1_2_1_1_3_3/Filleting_and_processing"                                             
[107] "_3_4_1_2_1_1_3_3/Harvesting"                                                           
[108] "_3_4_1_2_1_1_3_3/Marketing_and_sales"                                                  
[109] "_3_4_1_2_1_1_3_3/other"                                                                
[110] "_3_4_1_2_1_1_3_3/Record_keeping"                                                       
[111] "_3_4_1_2_1_1_3_3/Site_preparation_(e.g.,_prepare_ponds,_tanks,_or_cages_accordingly)"  
[112] "_3_4_1_2_1_1_3_3/Stocking"                                                             
[113] "_3_4_1_2_1_1_3_3/Water_quality_management"                                             
[114] "_3_4_1_2_1_1_3_3_1"                                                                    
[115] "_3_4_1_2_1_1_3_4/Fertilization_and_nutrient_management"                                
[116] "_3_4_1_2_1_1_3_4/Irrigation_and_water_management"                                      
[117] "_3_4_1_2_1_1_3_4/Market_and_sales"                                                     
[118] "_3_4_1_2_1_1_3_4/other"                                                                
[119] "_3_4_1_2_1_1_3_4/Pest_and_disease_management"                                          
[120] "_3_4_1_2_1_1_3_4/Planting_and_transplanting"                                           
[121] "_3_4_1_2_1_1_3_4/Pruning_and_training"                                                 
[122] "_3_4_1_2_1_1_3_4/Seedling_production_or_sourcing"                                      
[123] "_3_4_1_2_1_1_3_4/Soil_preparation"                                                     
[124] "_3_4_1_2_1_1_3_4/Weed_control"                                                         
[125] "_3_4_1_2_1_1_3_4_1"                                                                    
[126] "_3_4_1_2_1_1_3_5/Bee_colony_management"                                                
[127] "_3_4_1_2_1_1_3_5/Bee_health_management"                                                
[128] "_3_4_1_2_1_1_3_5/Hive_management"                                                      
[129] "_3_4_1_2_1_1_3_5/Honey_extraction:"                                                    
[130] "_3_4_1_2_1_1_3_5/Honey_processing"                                                     
[131] "_3_4_1_2_1_1_3_5/Marketing_and_sales"                                                  
[132] "_3_4_1_2_1_1_3_5/other"                                                                
[133] "_3_4_1_2_1_1_3_5/Packaging_and_labeling"                                               
[134] "_3_4_1_2_1_1_3_5_1"                                                                    
[135] "_3_4_1_2_1_1_3_6"                                                                      
[136] "_3_4_1_2_1_1_calculate"                                                                
[137] "_3_4_1_2_1_2_1"                                                                        
[138] "_3_4_1_2_1_2_1_2/1"                                                                    
[139] "_3_4_1_2_1_2_1_2/10"                                                                   
[140] "_3_4_1_2_1_2_1_2/11"                                                                   
[141] "_3_4_1_2_1_2_1_2/12"                                                                   
[142] "_3_4_1_2_1_2_1_2/2"                                                                    
[143] "_3_4_1_2_1_2_1_2/3"                                                                    
[144] "_3_4_1_2_1_2_1_2/4"                                                                    
[145] "_3_4_1_2_1_2_1_2/5"                                                                    
[146] "_3_4_1_2_1_2_1_2/6"                                                                    
[147] "_3_4_1_2_1_2_1_2/7"                                                                    
[148] "_3_4_1_2_1_2_1_2/8"                                                                    
[149] "_3_4_1_2_1_2_1_2/9"                                                                    
[150] "_3_4_1_2_1_2_1_2_join"                                                                 
[151] "_3_4_1_2_1_2_1_3"                                                                      
[152] "_3_4_1_2_1_2_1_4"                                                                      
[153] "_3_4_1_2_1_2_1_5/Crops"                                                                
[154] "_3_4_1_2_1_2_1_5/Fish"                                                                 
[155] "_3_4_1_2_1_2_1_5/Honey"                                                                
[156] "_3_4_1_2_1_2_1_5/Livestock"                                                            
[157] "_3_4_1_2_1_2_1_5/other"                                                                
[158] "_3_4_1_2_1_2_1_5/Trees"                                                                
[159] "_3_4_1_2_1_2_1_5_1/Cleaning"                                                           
[160] "_3_4_1_2_1_2_1_5_1/Drying"                                                             
[161] "_3_4_1_2_1_2_1_5_1/Harvesting"                                                         
[162] "_3_4_1_2_1_2_1_5_1/Land_preparation"                                                   
[163] "_3_4_1_2_1_2_1_5_1/other"                                                              
[164] "_3_4_1_2_1_2_1_5_1/Planting"                                                           
[165] "_3_4_1_2_1_2_1_5_1/Processing_(milling,_grinding,_grating,_etc)"                       
[166] "_3_4_1_2_1_2_1_5_1/Protection_from_losses"                                             
[167] "_3_4_1_2_1_2_1_5_1/Ridging,_fertilizing,_other_non-harvested_activities"               
[168] "_3_4_1_2_1_2_1_5_1/Shelling/Threshing/Peeling"                                         
[169] "_3_4_1_2_1_2_1_5_1/Sorting"                                                            
[170] "_3_4_1_2_1_2_1_5_1/Supervision"                                                        
[171] "_3_4_1_2_1_2_1_5_1/Weeding"                                                            
[172] "_3_4_1_2_1_2_1_5_1_1"                                                                  
[173] "_3_4_1_2_1_2_1_5_2/Animal_feeding"                                                     
[174] "_3_4_1_2_1_2_1_5_2/Egg_collection"                                                     
[175] "_3_4_1_2_1_2_1_5_2/Health_care_and_veterinary_services"                                
[176] "_3_4_1_2_1_2_1_5_2/Herd_management"                                                    
[177] "_3_4_1_2_1_2_1_5_2/Management_of_housing_and_shelter_(e.g.,_cleaning,_repairs)"        
[178] "_3_4_1_2_1_2_1_5_2/Marketing"                                                          
[179] "_3_4_1_2_1_2_1_5_2/Milking_and_dairy_management"                                       
[180] "_3_4_1_2_1_2_1_5_2/other"                                                              
[181] "_3_4_1_2_1_2_1_5_2/Pasture_management"                                                 
[182] "_3_4_1_2_1_2_1_5_2/Waste_management"                                                   
[183] "_3_4_1_2_1_2_1_5_2/Wool_management"                                                    
[184] "_3_4_1_2_1_2_1_5_2_1"                                                                  
[185] "_3_4_1_2_1_2_1_5_3/Disease_prevention_and_health_management"                           
[186] "_3_4_1_2_1_2_1_5_3/Feeding_and_nutrition"                                              
[187] "_3_4_1_2_1_2_1_5_3/Filleting_and_processing"                                           
[188] "_3_4_1_2_1_2_1_5_3/Harvesting"                                                         
[189] "_3_4_1_2_1_2_1_5_3/Marketing_and_sales"                                                
[190] "_3_4_1_2_1_2_1_5_3/other"                                                              
[191] "_3_4_1_2_1_2_1_5_3/Record_keeping"                                                     
[192] "_3_4_1_2_1_2_1_5_3/Site_preparation_(e.g.,_prepare_ponds,_tanks,_or_cages_accordingly)"
[193] "_3_4_1_2_1_2_1_5_3/Stocking"                                                           
[194] "_3_4_1_2_1_2_1_5_3/Water_quality_management"                                           
[195] "_3_4_1_2_1_2_1_5_3_1"                                                                  
[196] "_3_4_1_2_1_2_1_5_4/Fertilization_and_nutrient_management"                              
[197] "_3_4_1_2_1_2_1_5_4/Irrigation_and_water_management"                                    
[198] "_3_4_1_2_1_2_1_5_4/Market_and_sales"                                                   
[199] "_3_4_1_2_1_2_1_5_4/other"                                                              
[200] "_3_4_1_2_1_2_1_5_4/Pest_and_disease_management"                                        
[201] "_3_4_1_2_1_2_1_5_4/Planting_and_transplanting"                                         
[202] "_3_4_1_2_1_2_1_5_4/Pruning_and_training"                                               
[203] "_3_4_1_2_1_2_1_5_4/Seedling_production_or_sourcing"                                    
[204] "_3_4_1_2_1_2_1_5_4/Soil_preparation"                                                   
[205] "_3_4_1_2_1_2_1_5_4/Weed_control"                                                       
[206] "_3_4_1_2_1_2_1_5_4_1"                                                                  
[207] "_3_4_1_2_1_2_1_5_5/Bee_colony_management"                                              
[208] "_3_4_1_2_1_2_1_5_5/Bee_health_management"                                              
[209] "_3_4_1_2_1_2_1_5_5/Hive_management"                                                    
[210] "_3_4_1_2_1_2_1_5_5/Honey_extraction:"                                                  
[211] "_3_4_1_2_1_2_1_5_5/Honey_processing"                                                   
[212] "_3_4_1_2_1_2_1_5_5/Marketing_and_sales"                                                
[213] "_3_4_1_2_1_2_1_5_5/other"                                                              
[214] "_3_4_1_2_1_2_1_5_5/Packaging_and_labeling"                                             
[215] "_3_4_1_2_1_2_1_5_5_1"                                                                  
[216] "_3_4_1_2_1_2_note"                                                                     
[217] "_3_4_1_2_7_2_1_calculate"                                                              
[218] "_3_4_1_2_7_2_1_note"                                                                   
[219] "_3_4_1_2_7_2_10/Bee_colony_management"                                                 
[220] "_3_4_1_2_7_2_10/Bee_health_management"                                                 
[221] "_3_4_1_2_7_2_10/Hive_management"                                                       
[222] "_3_4_1_2_7_2_10/Honey_extraction:"                                                     
[223] "_3_4_1_2_7_2_10/Honey_processing"                                                      
[224] "_3_4_1_2_7_2_10/Marketing_and_sales"                                                   
[225] "_3_4_1_2_7_2_10/other"                                                                 
[226] "_3_4_1_2_7_2_10/Packaging_and_labeling"                                                
[227] "_3_4_1_2_7_2_10_1"                                                                     
[228] "_3_4_1_2_7_2_2/1"                                                                      
[229] "_3_4_1_2_7_2_2/10"                                                                     
[230] "_3_4_1_2_7_2_2/11"                                                                     
[231] "_3_4_1_2_7_2_2/12"                                                                     
[232] "_3_4_1_2_7_2_2/2"                                                                      
[233] "_3_4_1_2_7_2_2/3"                                                                      
[234] "_3_4_1_2_7_2_2/4"                                                                      
[235] "_3_4_1_2_7_2_2/5"                                                                      
[236] "_3_4_1_2_7_2_2/6"                                                                      
[237] "_3_4_1_2_7_2_2/7"                                                                      
[238] "_3_4_1_2_7_2_2/8"                                                                      
[239] "_3_4_1_2_7_2_2/9"                                                                      
[240] "_3_4_1_2_7_2_2_join"                                                                   
[241] "_3_4_1_2_7_2_3"                                                                        
[242] "_3_4_1_2_7_2_4"                                                                        
[243] "_3_4_1_2_7_2_5/Crops"                                                                  
[244] "_3_4_1_2_7_2_5/Fish"                                                                   
[245] "_3_4_1_2_7_2_5/Honey"                                                                  
[246] "_3_4_1_2_7_2_5/Livestock"                                                              
[247] "_3_4_1_2_7_2_5/other"                                                                  
[248] "_3_4_1_2_7_2_5/Trees"                                                                  
[249] "_3_4_1_2_7_2_5_1"                                                                      
[250] "_3_4_1_2_7_2_6/Cleaning"                                                               
[251] "_3_4_1_2_7_2_6/Drying"                                                                 
[252] "_3_4_1_2_7_2_6/Harvesting"                                                             
[253] "_3_4_1_2_7_2_6/Land_preparation"                                                       
[254] "_3_4_1_2_7_2_6/other"                                                                  
[255] "_3_4_1_2_7_2_6/Planting"                                                               
[256] "_3_4_1_2_7_2_6/Processing_(milling,_grinding,_grating,_etc)"                           
[257] "_3_4_1_2_7_2_6/Protection_from_losses"                                                 
[258] "_3_4_1_2_7_2_6/Ridging,_fertilizing,_other_non-harvested_activities"                   
[259] "_3_4_1_2_7_2_6/Shelling/Threshing/Peeling"                                             
[260] "_3_4_1_2_7_2_6/Sorting"                                                                
[261] "_3_4_1_2_7_2_6/Supervision"                                                            
[262] "_3_4_1_2_7_2_6/Weeding"                                                                
[263] "_3_4_1_2_7_2_6_1"                                                                      
[264] "_3_4_1_2_7_2_7/Animal_feeding"                                                         
[265] "_3_4_1_2_7_2_7/Egg_collection"                                                         
[266] "_3_4_1_2_7_2_7/Health_care_and_veterinary_services"                                    
[267] "_3_4_1_2_7_2_7/Herd_management"                                                        
[268] "_3_4_1_2_7_2_7/Management_of_housing_and_shelter_(e.g.,_cleaning,_repairs)"            
[269] "_3_4_1_2_7_2_7/Marketing"                                                              
[270] "_3_4_1_2_7_2_7/Milking_and_dairy_management"                                           
[271] "_3_4_1_2_7_2_7/other"                                                                  
[272] "_3_4_1_2_7_2_7/Pasture_management"                                                     
[273] "_3_4_1_2_7_2_7/Waste_management"                                                       
[274] "_3_4_1_2_7_2_7/Wool_management"                                                        
[275] "_3_4_1_2_7_2_7_1"                                                                      
[276] "_3_4_1_2_7_2_8/Disease_prevention_and_health_management"                               
[277] "_3_4_1_2_7_2_8/Feeding_and_nutrition"                                                  
[278] "_3_4_1_2_7_2_8/Filleting_and_processing"                                               
[279] "_3_4_1_2_7_2_8/Harvesting"                                                             
[280] "_3_4_1_2_7_2_8/Marketing_and_sales"                                                    
[281] "_3_4_1_2_7_2_8/other"                                                                  
[282] "_3_4_1_2_7_2_8/Record_keeping"                                                         
[283] "_3_4_1_2_7_2_8/Site_preparation_(e.g.,_prepare_ponds,_tanks,_or_cages_accordingly)"    
[284] "_3_4_1_2_7_2_8/Stocking"                                                               
[285] "_3_4_1_2_7_2_8/Water_quality_management"                                               
[286] "_3_4_1_2_7_2_8_1"                                                                      
[287] "_3_4_1_2_7_2_9/Fertilization_and_nutrient_management"                                  
[288] "_3_4_1_2_7_2_9/Irrigation_and_water_management"                                        
[289] "_3_4_1_2_7_2_9/Market_and_sales"                                                       
[290] "_3_4_1_2_7_2_9/other"                                                                  
[291] "_3_4_1_2_7_2_9/Pest_and_disease_management"                                            
[292] "_3_4_1_2_7_2_9/Planting_and_transplanting"                                             
[293] "_3_4_1_2_7_2_9/Pruning_and_training"                                                   
[294] "_3_4_1_2_7_2_9/Seedling_production_or_sourcing"                                        
[295] "_3_4_1_2_7_2_9/Soil_preparation"                                                       
[296] "_3_4_1_2_7_2_9/Weed_control"                                                           
[297] "_3_4_1_2_7_2_9_1"                                                                      
[298] "_3_7_2_2_3_6"  