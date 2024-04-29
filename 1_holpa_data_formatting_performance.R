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
global.data.path <-"C:/Users/andreasanchez/OneDrive - CGIAR/Bioversity/AI/HOLPA/analysis/HOLPA/"
zwe.data.path <-"C:/Users/andreasanchez/OneDrive - CGIAR/Bioversity/AI/HOLPA/HOLPA_data/Zimbabwe/zimbabwe_data_clean/"

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
zwe_survey <- zwe_survey[-1,]

# Crop production section
zwe_3_4_3_1_2_begin_repeat <- read_excel(path=paste0(zwe.data.path,"household_database_2024.04.18_clean.xlsx"),
                         sheet = "_3_4_3_1_2_begin_repeat")%>%
  rename("kobo_farmer_id"="_submission__id")


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
                                      if_else(str_detect(subindicator, "biodiversity_climate_mitigation"),"biodiversity_climate_mitigation",
                                              if_else(str_detect(subindicator, "biodiversity_practices"),"biodiversity_practices",
                                                      if_else(str_detect(subindicator, "biodiversity_agrobiodiversity"),"biodiversity_agrobiodiversity",
                                                              if_else(str_detect(subindicator, "energy"),"energy",
                                                                      if_else(str_detect(subindicator, "water"),"water",
                                                                              if_else(str_detect(subindicator, "climate_mitigation"),"biodiversity_climate_mitigation",subindicator)))))))))

performance_env_choices <- global_choices %>%
  filter(str_detect(module, "performance"))%>%
  mutate(module= "performance") %>% 
  filter(str_detect(indicator, "environment"))%>%
  mutate(indicator="environmental") %>%
  mutate(subindicator=if_else(str_detect(subindicator, "biodiversity_diversity"),"biodiversity_diversity",
                              if_else(str_detect(subindicator, "biodiversity_abundance"),"biodiversity_abundance",
                                      if_else(str_detect(subindicator, "biodiversity_climate_mitigation"),"biodiversity_climate_mitigation",
                                              if_else(str_detect(subindicator, "biodiversity_practices"),"biodiversity_practices",
                                                      if_else(str_detect(subindicator, "biodiversity_agrobiodiversity"),"biodiversity_agrobiodiversity",
                                                              if_else(str_detect(subindicator, "energy"),"energy",
                                                                      if_else(str_detect(subindicator, "water"),"water",
                                                                              if_else(str_detect(subindicator, "climate_mitigation"),"biodiversity_climate_mitigation",subindicator)))))))))


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
  filter(theme=="environmental")
  filter(indicator== "biodiversity"
          #indicator== "biodiversity_abundance" |
           #indicator=="biodiversity_agrobiodiversity"| 
         #indicator=="biodiversity_climate_mitigation"|
           #indicator== "biodiversity_cover"|
           #indicator=="biodiversity_diversity"|
           #indicator=="biodiversity_practices" |
    #indicator=="energy"|
      indicator=="water"
      )
names(performance_choices)
sort(unique(performance_choices$indicator))

sort(unique(performance_choices$type_question))
sort(unique(performance_choices$type))
sort(unique(performance_choices$name_question))


performance_questions_columns<- performance_choices%>% 
  filter(theme=="environmental")%>%
 # filter(indicator== "biodiversity" #no data from zimbabwe
         #indicator== "biodiversity_abundance"| #ok
     #indicator=="biodiversity_agrobiodiversity"|#2 questions missing  "_3_4_3_1_3" "_3_4_3_4_2" _3_4_3_1_2_begin_repeat
    #indicator=="biodiversity_climate_mitigation"| #missing questions related to area of diversified farming systems
    
      #     indicator== "biodiversity_cover"| #ok
    #indicator=="biodiversity_diversity"| #ok
    # indicator=="biodiversity_practices" #ok
    #indicator=="energy"| #ok
  # indicator=="water" #missing questions _3_3_4_1_3_begin_repeat

    
 #)%>%
  dplyr::select(label_question, name_question_choice)%>%
  dplyr::distinct(name_question_choice, .keep_all = TRUE)%>%
  spread(key = name_question_choice, value = label_question)%>%
  mutate("kobo_farmer_id"="kobo_farmer_id",
         "country"="country_name")

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
    filter(type_question =="calculate"|type_question =="integer"|type_question =="note"|type_question =="text")%>%
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
           #"name_question"="name_question.y")%>%
    #Remove answers == "0" or NA
    filter(type_question == "select_multiple" & !is.na(name_choice) & name_choice != 0)
    #Replace answer code by label

  # Left join for "select_one"
  select_one <- gathered_data  %>%
    left_join(select(performance_choices,
                     c(name_question, name_choice, module, theme, indicator, label_choice, label_question, type, type_question, list_name)), 
              by = c("name_question"="name_question", "name_choice"="name_choice"))%>%
    filter(type_question=="select_one")

  result<- rbind(continuous,select_multiple,select_one)
  

  return(result)
}


# Zimbabwe
zwe_performance <- zwe_survey %>%
  select(all_of(zwe_performance_columns))%>%
  mutate_all(as.character)

library(summarytools)
view(dfSummary(zwe_performance))

# Identify columns with only NA values
na_columns <- colSums(is.na(zwe_performance)) == nrow(zwe_performance)
na_columns

# Remove columns with only NA values
zwe_performance <- zwe_performance[, !na_columns]

view(dfSummary(zwe_performance))

result <- zwe_performance%>%
  gather(key = "name_question", value = "name_choice", -kobo_farmer_id, -country)%>%
  perform_left_join(performance_choices,.)%>%
  mutate(name_question_recla= name_question)%>%
  mutate(name_question_recla = str_remove(name_question_recla, "/.*"))

result2<- result%>%
  #Replace ${_1_4_1_1} in label_choice by the hectares or acres
  mutate(label_choice = gsub("\\$\\{_1_4_1_1\\}", "hectares", label_choice))%>%
  
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
                                                       "_2_8_4_3_4") ~ "other",TRUE ~ label_choice))%>%
  
  mutate(label_question = case_when(
    name_question_recla== "_3_4_3_3_1" ~  "**In the last 12 months [add country meaning], which different livestock species did you keep?**",
    TRUE ~ label_question))%>%
  
  ## Indicator: biodiversity_diversity
  filter(!(name_question_recla == "_3_3_1_1_9_1" & is.na(name_choice)))%>% #Remove the rows with **Specify other landscape features:** == NA 
  
  #filter(name_question != "_3_4_3_3_1/other")%>%
  # Indicator: biodiversity_practices
  mutate(label_choice = case_when(name_question_recla%in% c("_2_9_1_1_1","_3_3_1_7_1") ~ "other ecological practice",TRUE ~label_choice))%>%
  #filter(!(name_question_recla == "_2_9_1_1_1" & is.na(name_choice)))%>% #Remove the rows with **Specify other landscape features:** == NA 
  #filter(name_question != "_2_9_1_1/other")%>%
  mutate(label_question = case_when(name_question_recla== "_2_9_1_1_1" ~ "**Which ecological practices do you use on cropland to improve soil quality and health?**",TRUE ~label_question))%>%
  mutate(name_question_recla = case_when(name_question_recla== "_2_9_1_1_1" ~ "_2_9_1_1",TRUE ~name_question_recla))%>%
  mutate(label_question = case_when(name_question_recla== "_3_3_1_7_1" ~ "**What ecological practices did you apply in the last 12 months [add country meaning] on the farm to manage crop pests?**",TRUE ~label_question))%>%
  mutate(name_question_recla = case_when(name_question_recla== "_3_3_1_7_1" ~ "_3_3_1_7",TRUE ~name_question_recla))%>%
  #filter(!(name_question_recla == "_3_3_1_7_1" & is.na(name_choice)))%>% #Remove the rows with **Specify other** == NA 
  
  # Indicator: energy
  mutate(label_question = case_when(name_question_recla== "_2_8_4_3_4" ~ "**What types of energy do you use for: Cleaning, processing or transporting harvested food**",TRUE ~label_question))%>%
  #filter(!(name_question_recla == "_2_8_4_3_4" & is.na(name_choice)))%>% #Remove the rows with **Specify other landscape features:** == NA 
  
  filter(!(name_question %in% c("_3_3_1_1_9_1", "_2_9_1_1_1", "_3_3_1_7_1","_2_8_4_3_4") & is.na(name_choice)))%>% #Remove the rows with **Specify other:** == NA 
  filter(!(name_question %in% c("_3_4_3_3_1/other", "_2_9_1_1/other", "_3_3_1_7/other","_2_8_4_4/other")))


view(dfSummary(result2))
  
  
sort(unique(result2$label_question))
sort(unique(result2$name_question))
sort(unique(result2$name_question_recla))


  
write.csv(result2,file="C:/Users/andreasanchez/OneDrive - CGIAR/Bioversity/AI/HOLPA/analysis/HOLPA/zwe/zwe_performance.csv",row.names=FALSE)


##OBSERVATIONS
# Zimbabwe does not have data for THEME ENVIRONMENT - indicator: biodiversity (10 questions)

  