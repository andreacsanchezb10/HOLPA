#--------------------
#library(openxlsx)
library(tidyr)
library(tidyverse)
library(readxl)
library(dplyr)
library("summarytools")

#### Set file paths ####
# TO CHECK: I need to connect directly to the share point, I already asked Sebastien for permision
#For now I will leave it like this to continue working
#Sarah
global.data.path <- "D:/02_Bioversity/46_Agroecology_Initiative/holpa_results/"
zwe.data.path <- "D:/02_Bioversity/46_Agroecology_Initiative/holpa_results/zwe/"

#### Import data ####
# Each dataset contains a survey worksheet with the questions and responses for text, open and numeric questions, and
# a choices worksheet with the response options for multiple choice questions (single or multiple).
# These need to be imported and combined.


#### Global databases ####----
global.data.path <-"C:/Users/andreasanchez/OneDrive - CGIAR/Bioversity/AI/HOLPA/analysis/HOLPA/HOLPA/" #andrea
global_agroecology_choices <- read_excel(paste0(global.data.path,"HOLPA_global_household_survey_20231204_mapped_to_indicators_master.xlsx"),
                                         sheet = "choices")%>%
  select("list_name","name","label::English ((en))","country", "score_agroecology_module")%>%
  rename("label_choice" = "label::English ((en))")%>%
  rename("name_choice" = "name")%>%
  filter(!is.na(score_agroecology_module))
names(global_agroecology_choices)

agroecology_global_score <- read_excel(paste0(global.data.path,"HOLPA_global_household_survey_20231204_mapped_to_indicators_master.xlsx"),
                                       sheet = "agroecology_look_up")%>%
  #Retain only the necessary columns
  select("list_name","name_question_recla","multiple_responses_label_choice","label_score_agroecology_module", "score_agroecology_module")
names(global_agroecology_choices)

#Function to add response options that countries added to the agroecological questions
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

country_agroecology_choices<- data.frame(
  list_name = character(),
  name_choice = character(),
  label_choice = character(),
  country = character(),
  score_agroecology_module = numeric(),
  stringsAsFactors = FALSE)

### Country databases ####----
#Zimbabwe----
zwe.data.path <-"C:/Users/andreasanchez/OneDrive - CGIAR/Bioversity/AI/HOLPA/analysis/HOLPA/HOLPA/zwe/zwe_agroecology_format.csv" #andrea
zwe_agroecology_data <- read.csv(zwe.data.path)

#1-Recycling
zwe_agroecology_choices <- add_row_to_database(country_agroecology_choices, "2_8_1_1", "6", "All seeds are given by the government", "zimbabwe", 1)
zwe_agroecology_choices <- add_row_to_database(zwe_agroecology_choices, "2_8_1_1", "7", "75% of seeds are given by the government, the other 25% are self-purchased.", "zimbabwe",1)
zwe_agroecology_choices <- add_row_to_database(zwe_agroecology_choices, "2_8_1_1", "8", "50% of seeds are given by the government, the other 50% are self-purchased.", "zimbabwe", 1)
zwe_agroecology_choices <- add_row_to_database(zwe_agroecology_choices, "2_8_1_1", "9", "25% of seeds are given by the government, the other 75% are self-purchased.", "zimbabwe", 1)

zwe_global_agroecology_choices<-rbind(global_agroecology_choices,zwe_agroecology_choices)

#Tunisia----
tun.data.path <-"C:/Users/andreasanchez/OneDrive - CGIAR/Bioversity/AI/HOLPA/analysis/HOLPA/HOLPA/tun/tun_agroecology_format.csv" #andrea
tun_agroecology_data <- read.csv(tun.data.path)

#### AGROECOLOGY: SCORING ------
pre_processing<- zwe_agroecology_data%>%
  #Extra questions we need to score produced crops/livestock/honey/wood but did not sell them 11_connectivity
  mutate(type_question = case_when(
    name_question_recla %in%c("_1_4_2_2_3", "_1_4_2_3_4", "_1_4_2_4_3", "_1_4_2_5_5", "_1_4_2_6_3", "_1_4_2_7_4")~"select_multiple",
    TRUE ~ type_question))%>%
  #Rename type_question for questions we need for counting  
  mutate(type_question = case_when(
      name_question_recla %in% c(
      "_2_9_1_1", #3_soil_health
      "_2_10_1_2","_3_3_3_4", #4_animal_health
      "_3_4_3_1_1_2", "_3_4_3_3_1",#5_biodiversity
      "_3_3_3_1_calculate_2","_2_9_1_1","_3_3_1_7","_3_3_3_3","_3_3_3_4","_2_12_1", #6_synergy
      "_2_4_1" #7_economic_diversification
    ) ~ "counting",
     TRUE ~ type_question))
    # mutate(label_choice= case_when(
    #label_choice%in%c("Specify other practice:", "Other (please specify)")~name_choice,
    #name_question_recla%in%c("_3_4_3_1_1_2", "_3_4_3_3_1")~name_choice,
    #TRUE ~ label_choice))
  
sort(unique(pre_processing$label_choice))

## type_question: select_one----
## The scores to the answers from select one questions are in doc:HOLPA_global_household_survey_20231204_mapped_to_indicators_master, sheet: choices, column: score_agroecology_module
# 1_recycling: "_2_8_1_1"  "_2_8_2_1"  "_2_8_3_1" "_2_8_5_1" "_2_8_4_5" = 5/5
# 2_input_reduction: "_3_4_4_2" = 1/6
# 4_animal_health: "_2_10_1_1" = 1/3
# 5_biodiversity:  "_3_3_1_2_1" "_3_3_1_2_2" "_3_3_1_2_3" "_3_3_1_2_4" "_3_3_1_2_6" "_3_3_1_2_7" "_3_3_1_2_8" "_3_3_1_6" = 8/arreglar el total
# 9_social_values:"_2_5_1_1"  "_2_5_1_2"   "_2_5_1_3"   "_2_5_1_4"  = 4/4
# 10_fairness: "_2_6_1_4_1" "_2_6_1_4_2" "_2_6_1_4_3" "_2_6_1_4_4" "_2_6_1_4_5" "_2_6_1_4_6" = 6/6
# 12_governance: "_2_2_1_1" "_2_2_1_2" "_2_2_1_3" = 3/3
# 13_participation: "_2_3_1_4" = 1/1
#total = 29 questions
#Zimbabwe = 26 questions (missing: _2_8_5_1, _2_6_1_4_3, _2_6_1_4_4)
select_one<- pre_processing%>%
  filter(type_question == "select_one")%>%
  mutate(name_label_choice=if_else(type_question=="select_one", paste(name_choice,"_",label_choice,sep=""),NA))%>%
  dplyr::left_join(select(zwe_global_agroecology_choices,c(list_name,name_choice,label_choice,score_agroecology_module )), 
                   by= c("list_name"="list_name",
                         "name_choice"="name_choice",
                         "label_choice"="label_choice"))%>%
  select(!name_label_choice)%>%
  mutate(label_score_agroecology_module= label_choice)

view(dfSummary(select_one))

## type_question: select_multiple ----
## The scores to the answers from select one questions are in doc:HOLPA_global_household_survey_20231204_mapped_to_indicators_master, sheet: agroecology_look_up, column: score_agroecology_module
# 2_input_reduction:"_1_4_3_1", "_1_4_3_5",   "_2_8_5_3","_1_4_3_8", "_1_4_3_9" = 5/6
# 11_connectivity: "_2_7_1_1","_2_7_1_2","_2_7_1_3","_2_7_1_4", "_2_7_1_5","_2_7_1_6""_1_4_2_2_3", "_1_4_2_3_4", "_1_4_2_4_3", "_1_4_2_5_5", "_1_4_2_6_3", "_1_4_2_7_4" = 12/12
# total = 17 questions
# zimbabwe = 11 questions (missing: "_2_8_5_3", "_1_4_3_9","_2_7_1_3","_2_7_1_4", "_1_4_2_4_3", "_1_4_2_5_5")
select_multiple<- pre_processing%>%
  filter(type_question == "select_multiple")%>%
  filter(!(name_question_recla %in% c("_1_4_2_2_3", "_1_4_2_3_4", "_1_4_2_4_3", "_1_4_2_5_5", "_1_4_2_6_3", "_1_4_2_7_4") & name_choice != 0))
  
  sort(unique(select_multiple$name_question_recla))
  
select_multiple2<-select_multiple%>%
  arrange(label_choice) %>%
  group_by(kobo_farmer_id, theme, name_question_recla) %>%
  summarise(multiple_responses_label_choice = paste(label_choice, collapse = "//", sep=""),
            multiple_responses_name_choice = paste(name_choice, collapse = "//", sep="")) %>%
  ungroup()%>%
  left_join(agroecology_global_score, by=c("name_question_recla",  "multiple_responses_label_choice"))%>%
  mutate(score_agroecology_module = case_when(
    name_question_recla %in% c("_1_4_2_2_3", "_1_4_2_3_4", "_1_4_2_4_3", "_1_4_2_5_5", "_1_4_2_6_3", "_1_4_2_7_4") ~ 1,
    TRUE ~ as.numeric(score_agroecology_module)))
  filter(is.na(score_agroecology_module))
  
sort(unique(select_multiple2$label_score_agroecology_module))
sort(unique(select_multiple2$multiple_responses_label_choice))
  
select_multiple3<- select_multiple%>%
  left_join(select_multiple2, by=c("kobo_farmer_id", "theme","list_name","name_question_recla" ))%>%
  distinct(across(-c(name_question, name_choice, label_choice)), .keep_all = TRUE)%>%
  mutate(label_choice= multiple_responses_label_choice,
         name_choice= multiple_responses_name_choice)%>%
  select(-multiple_responses_label_choice,-multiple_responses_name_choice)

view(dfSummary(select_multiple3))
sort(unique(select_multiple3$name_question_recla))

## type_question: integer ----
# 8 - knowledge: "_2_1_1_1" "_2_1_1_2" "_2_1_1_3" "_2_1_1_4" "_2_1_1_5" "_2_1_1_6" "_2_1_1_7" = 7/7
#Total = 7 questions
#Zimbabwe = 7 questions
integer<- pre_processing%>%
  filter(type_question == "integer")%>%
  mutate(label_score_agroecology_module=name_choice,
         score_agroecology_module=NA,
         label_score_agroecology_module=as.numeric(label_score_agroecology_module))%>%
  mutate(score_agroecology_module= case_when(
    # 10_knowlege: In the last 12 months, how many times has your household exchanged information with the following food system stakeholders to create new or improved solutions to your or others' farming problems?
    label_score_agroecology_module == 0 ~ 1, #Never
    label_score_agroecology_module == 1 ~ 2, #1 time per year.
    label_score_agroecology_module == 2 ~ 3, #2 to 3 times per year.
    label_score_agroecology_module == 3 ~ 3, #2 to 3 times per year.
    label_score_agroecology_module == 4 ~ 4, #4 times per year.
    label_score_agroecology_module == 5 ~ 4, #5 or more times per year.
    label_score_agroecology_module> 5 ~ 5,   #5 or more times per year.
    TRUE ~ score_agroecology_module))%>%
  mutate(label_score_agroecology_module= paste(name_choice, "times per year"))

view(dfSummary(integer))

## type_question: counting----
# 3- soil_health: "_2_9_1_1" = 1/1
# 4_animal_health: "_2_10_1_2","_3_3_3_4" = 2/3
# 5_biodiversity: "_3_4_3_3_1",_3_4_3_1_1_2
# 6- synergy: "_3_3_3_1_calculate_2","_2_9_1_1","_3_3_1_7","_3_3_3_3","_3_3_3_4","_2_12_1" = 6/6
# 7- economic_diversification: "_2_4_1" = 1/1
# Total = 10 questions (2 duplicated across principles)
#zimbabwe = 9 questions (missing: _3_3_3_4)
area <- pre_processing %>%
  #Retain questions related to area of land for crop, livestock and fish production and total area of land managed by household
  filter(name_question_recla %in% c("_3_4_2_1_1", "_3_4_2_2_1_1", "_3_4_2_2_1_2", "_3_4_2_3_2",
                                    "_1_4_1_1_1","_1_4_1_1_2","_1_4_1_1_3")) %>%
  mutate(name_choice = as.numeric(name_choice))%>% 
  # Remove rows with 9999, or 0
  filter(!(name_choice %in% c(9999, 0))) %>%
  # Convert acres to hectares
  mutate(name_choice = case_when(label_choice == "in acres" ~ name_choice * 0.404686, TRUE ~ name_choice))%>%
  mutate(name_question_recla= case_when(
    #Rename area of owned and leased land for livestock production to get total land for livestock production
    name_question_recla %in%c("_3_4_2_2_1_1", "_3_4_2_2_1_2")~ "_3_4_2_2_1_3",
    #Rename total area of owned, leased or shared land  to get total land managed by household
    name_question_recla %in%c( "_1_4_1_1_1","_1_4_1_1_2","_1_4_1_1_3")~ "_1_4_1_1_4",
    TRUE ~ name_question_recla))%>%
  group_by(kobo_farmer_id,name_question_recla,theme) %>%
  summarize(production_area_ha = sum(name_choice, na.rm = TRUE))%>%
  ungroup()%>%
  mutate(name_question_recla= case_when(
    name_question_recla == "_3_4_2_1_1"~ "_3_4_3_1_1_2",
    name_question_recla == "_3_4_2_2_1_3"~ "_3_4_3_3_1",
    TRUE ~ name_question_recla))

area_total<- area%>%
  filter(name_question_recla %in% c("_1_4_1_1_4"))
 
sort(unique(area$name_question_recla))

counting<- pre_processing%>%
  filter(type_question == "counting")

sort(unique(counting$name_choice))

counting2<-counting%>%
  filter(!(name_choice%in%c("None","none","No action taken")))%>%
  arrange(label_choice) %>%
  group_by(kobo_farmer_id, name_question_recla, theme) %>%
  summarise(multiple_responses_label_choice = paste(label_choice, collapse = "//"),
            label_score_agroecology_module = n_distinct(name_choice))%>%
  ungroup()%>%
  #Left join area of land for crop, livestock, fish production for calculation (number of species/ha)
  left_join(area, by= c("kobo_farmer_id", "name_question_recla","theme"))%>%
  #Retain the rows with production_area_ha==NA (farmer did not know the area of land)
  filter((name_question_recla %in%c("_3_4_3_1_1_2","_3_4_3_3_1")& is.na(production_area_ha)))%>%
  select(-production_area_ha)%>%
  #Replace NA in production_area_ha by total land managed by household
  left_join(area_total, by=c("kobo_farmer_id","theme"))%>%
  select(-name_question_recla.y)%>%
  rename("name_question_recla"="name_question_recla.x")
names(counting2)

counting3<-counting%>%
  arrange(label_choice) %>%
  group_by(kobo_farmer_id, name_question_recla, theme) %>%
    summarise(multiple_responses_label_choice = paste(label_choice, collapse = "//"),
              label_score_agroecology_module = n_distinct(name_choice))%>% 
  ungroup()%>%
  mutate(label_score_agroecology_module= case_when(str_detect(multiple_responses_label_choice,"None")~(label_score_agroecology_module-1),TRUE ~ label_score_agroecology_module))%>% #this code is particularly for zwe (error in completing the database)
  mutate(label_score_agroecology_module= case_when(str_detect(multiple_responses_label_choice,"none")~(label_score_agroecology_module-1),TRUE ~ label_score_agroecology_module))%>% #this code is particularly for zwe (error in completing the database)
  mutate(label_score_agroecology_module= case_when(str_detect(multiple_responses_label_choice,"No action taken")~(label_score_agroecology_module-1),TRUE ~ label_score_agroecology_module))%>% #this code is particularly for zwe (error in completing the database)
  
  #Remove from the counting unsustainable practices from questions c(_3_3_3_1_calculate_2, _3_3_3_3
  mutate(label_score_agroecology_module= case_when(str_detect(multiple_responses_label_choice,"Monoculture with annual crops")~(label_score_agroecology_module-1),TRUE ~ label_score_agroecology_module))%>%
  mutate(label_score_agroecology_module= case_when(str_detect(multiple_responses_label_choice,"Monoculture with perennial crops")~(label_score_agroecology_module-1),TRUE ~ label_score_agroecology_module))%>%
  mutate(label_score_agroecology_module= case_when(str_detect(multiple_responses_label_choice,"Burning crop residues")~(label_score_agroecology_module-1),TRUE ~ label_score_agroecology_module))%>%
  mutate(label_score_agroecology_module= case_when(str_detect(multiple_responses_label_choice,"Land clearing for agriculture")~(label_score_agroecology_module-1),TRUE ~ label_score_agroecology_module))%>%
  mutate(label_score_agroecology_module= case_when(str_detect(multiple_responses_label_choice,"Overgrazing")~(label_score_agroecology_module-1),TRUE ~ label_score_agroecology_module))%>%
  
  #Left join area of land for crop, livestock, fish production for calculation (number of species/ha)
  left_join(area, by= c("kobo_farmer_id", "name_question_recla","theme"))%>%
  #Filter the rows with production_area_ha==NA (because farmer did not know the area of land for production)
  filter(!(name_question_recla %in%c("_3_4_3_1_1_2","_3_4_3_3_1")& is.na(production_area_ha)))%>%
  #Rbind total area managed by household for farmers that did not know the area of land for production
  rbind(counting2)%>%
  mutate(score_agroecology_module=NA)%>%
  mutate(label_score_agroecology_module= case_when(
    name_question_recla %in%c("_3_4_3_1_1_2","_3_4_3_3_1")~round(label_score_agroecology_module/production_area_ha,4),TRUE ~ label_score_agroecology_module))%>%
  mutate(score_agroecology_module= case_when(
    # 4_animal_health
    name_question_recla %in% c("_2_10_1_2", "_3_3_3_4") ~ case_when(
      label_score_agroecology_module == 0 ~ 1,
      label_score_agroecology_module == 1 ~ 2,
      label_score_agroecology_module == 2 ~ 3,
      label_score_agroecology_module == 3 ~ 4,
      label_score_agroecology_module >= 4 ~ 5),
    # 5_biodiversity
    name_question_recla %in% c("_3_4_3_1_1_2", "_3_4_3_3_1") ~ case_when(
      is.na(label_score_agroecology_module) ~ 1, #zwe has a farmer that does not provide any farmland and cropland size
      label_score_agroecology_module <= 1 ~ 1,
      label_score_agroecology_module > 1 & label_score_agroecology_module <= 2 ~ 2.33333,
      label_score_agroecology_module > 2 & label_score_agroecology_module <= 3 ~ 3.66666,
      label_score_agroecology_module > 3 ~ 5),
    # 6_synergy and 3_soil_health
    name_question_recla %in% c("_3_3_3_1_calculate_2", "_2_9_1_1", "_3_3_1_7", "_3_3_3_3", "_3_3_3_4", "_2_12_1") ~ case_when(
      label_score_agroecology_module == 0 ~ 1,
      label_score_agroecology_module == 1 ~ 2,
      label_score_agroecology_module == 2 ~ 3,
      label_score_agroecology_module == 3 ~ 4,
      label_score_agroecology_module >= 4 ~ 5),
    # 7_economic_diversification
    name_question_recla %in% c("_2_4_1") ~ case_when(
      label_score_agroecology_module == 1 ~ 1,
      label_score_agroecology_module == 2 ~ 2,
      label_score_agroecology_module == 3 ~ 3,
      label_score_agroecology_module == 4 ~ 3,
      label_score_agroecology_module > 4 ~ 5),
    TRUE ~ score_agroecology_module))%>%
  select(-production_area_ha)
    
view(dfSummary(counting3))

#_2_9_1_1: Get farmers that did not implement any ecological practice to improve soil fertility of cropland in question _1_4_3_1
counting_1_4_3_1 <- select_multiple3 %>%
  filter(name_question_recla == "_1_4_3_1") %>%
  filter(!str_detect(label_choice, "Use of Ecological practices \\(e\\.g\\., cover crops, legume intercropping, mulching, etc\\.\\)\\."))%>%
  mutate(name_question_recla= "_2_9_1_1",
        label_question= "Which ecological practices do you use on cropland to improve soil quality and health?",
        type_question= "counting",
        label_score_agroecology_module= "0 practices implemented",
        score_agroecology_module= 1)

#_3_3_1_7: Get farmers that did not implement any ecological practice to  to manage pest in  cropland in question  _3_3_1_7
counting_3_3_1_7 <- pre_processing %>%
  filter(name_question_recla=="_3_4_3_1_1_2")%>%
  select(kobo_farmer_id,country, sheet_id,module,type_question,parent_table_name,parent_index) %>%
  anti_join(
    counting3 %>%
      filter(name_question_recla == "_3_3_1_7") %>%
      select(kobo_farmer_id,),
    by = "kobo_farmer_id")%>%
  distinct()%>%
  mutate(index=NA,
         name_question= "_3_3_1_7",
         label_question= "What ecological practices did you apply in the last 12 months [add country meaning] on the farm to manage crop pests?",
         name_question_recla= "_3_3_1_7",
         theme="6_synergy",
         indicator="6_synergy",
         name_choice= 0,
         label_choice= "0 practices implemented",
         type= "select_multiple",
         list_name= "3_3_1_7",
         label_score_agroecology_module= "0 practices implemented",
         score_agroecology_module= 1)
         
names(counting_1_4_3_5)
sort(unique(counting_1_4_3_5$name_question_recla))
sort(unique(counting_1_4_3_5$label_score_agroecology_module))
sort(unique(counting_1_4_3_5$label_choice))

counting5<- counting%>%
  left_join(counting3, by=c("kobo_farmer_id", "theme","name_question_recla" ))%>%
  distinct(across(c(kobo_farmer_id, module, theme, indicator, label_question, type_question, name_question_recla,
                    multiple_responses_label_choice, label_score_agroecology_module, score_agroecology_module)), .keep_all = TRUE)%>%
  mutate(label_choice= multiple_responses_label_choice,
         name_choice= label_score_agroecology_module)%>%
  select(-multiple_responses_label_choice)%>%
  mutate(label_score_agroecology_module = as.character(label_score_agroecology_module))%>%
  mutate(label_score_agroecology_module= case_when(
    name_question_recla %in% 
      #4_animal_health
      c("_2_10_1_2","_3_3_3_4",
        #6_synergy
        "_3_3_3_1_calculate_2","_2_9_1_1","_3_3_1_7","_3_3_3_3","_3_3_3_4","_2_12_1")~ paste(name_choice, "practices implemented"),
    #5_biodiversity
    name_question_recla %in% c("_3_4_3_1_1_2")~paste(name_choice, "crop species per ha"),
    name_question_recla %in% c("_3_4_3_3_1")~paste(name_choice, "livestock species per ha"),
    #7_economic_diversification
    name_question_recla %in% c("_2_4_1")~ paste(name_choice, "sources of income"),
    TRUE ~ label_score_agroecology_module))%>%
  #_2_9_1_1: add farmers that did not implement any ecological practice to improve soil fertility of cropland in question _1_4_3_1
  # 3_soil_health
  rbind(counting_1_4_3_1)%>%
    mutate(theme= case_when(theme == "2_input_reduction"~ "3_soil_health",TRUE ~ theme))%>%
  mutate(indicator= case_when(theme == "2_input_reduction"~ "3_soil_health",TRUE ~ indicator))%>%
  # 6_synergy
  rbind(counting_1_4_3_1)%>%
  mutate(theme= case_when(theme == "2_input_reduction"~ "6_synergy",TRUE ~ theme))%>%
  mutate(indicator= case_when(theme == "2_input_reduction"~ "6_synergy",TRUE ~ indicator))%>%
  #_3_3_1_7: add farmers that did not implement any ecological practice to  to manage pest in  cropland in question  _3_3_1_7
  rbind(counting_3_3_1_7)

view(dfSummary(counting5))

agroecology_module_score<-   rbind(select_one,
                                   select_multiple3,
                                   counting5,
                                   integer)

view(dfSummary(agroecology_module_score))
names(select_one)  
names(select_multiple3)
sort(unique(integer$label_score_agroecology_module))
sort(unique(integer$name_question))


write.csv(agroecology_module_score,file='zwe/zwe_agroecology_score.csv',row.names=FALSE)




