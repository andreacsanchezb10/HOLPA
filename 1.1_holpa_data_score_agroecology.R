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
zwe.data.path <-"C:/Users/andreasanchez/OneDrive - CGIAR/Bioversity/AI/HOLPA/analysis/HOLPA/HOLPA/zwe/zwe_agroecology_format.csv"

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

agroecology_global_score <- read_excel(paste0(global.data.path,"HOLPA_global_household_survey_20231204_mapped_to_indicators_master.xlsx"),
                                         sheet = "agroecology_look_up")%>%
  #Retain only the necessary columns
  select("list_name","name_question_recla","multiple_responses","label_score_agroecology_module", "score_agroecology_module")
names(agroecology_global_choices)

#Function to add response options that countries added to agroecological questions
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

# AGROECOLOGY: SCORING ------
## type_question: select_one
# 1- Recycling: "_2_8_1_1"  "_2_8_2_1"  "_2_8_3_1" "_2_8_5_1" "_2_8_4_5" = 5/5
# 2- Input reduction: "_3_4_4_2" = 1/6
# 4- Animal health: "_2_10_1_1" = 1/3
# 5- Biodiversity:  "_3_3_1_5"  "_3_3_1_6" = 2/5
# 9- Social_values:"_2_5_1_1"  "_2_5_1_2"   "_2_5_1_3"   "_2_5_1_4"  
# 10- Fairness: "_2_6_1_1" "_2_6_1_2" "_2_6_1_4_1" "_2_6_1_4_2" "_2_6_1_4_5" "_2_6_1_4_6" 
# 12- Governance: "_2_2_1_1" "_2_2_1_2" "_2_2_1_3"
# 13- Participation: "_2_3_1_4" "2_6_1_4"  

## type_question: select_multiple
# TO CHECK: NO ACTION TAKEN TO MANAGE LIVESTOCK DISEASES IS SCORES AS 5 (MAXIMUM),I THINK WE SHOULD USE THIS QUESTIO _1_4_3_8 FOR ANIMAL HEALTH TOO
# 2- Input reduction:"_1_4_3_1", "_1_4_3_5", "_1_4_3_8",  "_2_8_5_3", "_1_4_3_9" = 5/6
# 11- Connectivity: "_2_7_1_1","_2_7_1_2","_2_7_1_5","_2_7_1_6" 

## type_question: count
# 3- soil_health: "_2_9_1_1" = 1/1
# 4- animal_health: "_2_10_1_2","_3_3_3_4" = 2/3
# 5- biodiversity: "_3_4_3_3_1",_3_4_3_1_1_2
# 6- synergy: "_3_3_3_1","_2_9_1_1","_3_3_1_7","_3_3_3_3","_3_3_3_4","_2_12_1" = 6/6
# 7- economic_diversification: "_2_4_1" = 1/1

## type_question: integer
# 8 - knowledge: "_2_1_1_1" "_2_1_1_2" "_2_1_1_3" "_2_1_1_4" "_2_1_1_5" "_2_1_1_6" "_2_1_1_7" = 7/7

sort(unique(zwe_agroecology_data$theme))

pre_processing<- zwe_agroecology_data%>%
  mutate(type_question = case_when(
    #11_connectivity
    str_detect(name_question_recla,"_1_4_2_2_3")~"select_multiple",
    str_detect(name_question_recla,"_1_4_2_3_4")~"select_multiple",
    str_detect(name_question_recla,"_1_4_2_4_3")~"select_multiple",
    str_detect(name_question_recla,"_1_4_2_5_5")~"select_multiple",
    str_detect(name_question_recla,"_1_4_2_6_3")~"select_multiple",
    str_detect(name_question_recla,"_1_4_2_7_4")~"select_multiple",
    TRUE ~ type_question))%>%
  mutate(type_question = case_when(
    name_question_recla %in% c(
      #3_soil_health
      "_2_9_1_1",
      #4_animal_health
      "_2_10_1_2","_3_3_3_4",
      #5_biodiversity
      "_3_4_3_1_1_2", "_3_4_3_3_1",
      #6_synergy
      "_3_3_3_1","_2_9_1_1","_3_3_1_7","_3_3_3_3","_3_3_3_4","_2_12_1",
      #7_economic_diversification
      "_2_4_1") ~ "count",
    TRUE ~ type_question))%>%
  mutate(label_choice= case_when(
    label_choice%in%c("Specify other practice:", "Other (please specify)")~name_choice,
    name_question_recla%in%c("_3_4_3_1_1_2", "_3_4_3_3_1")~name_choice,
    
    TRUE ~ label_choice))
  
sort(unique(pre_processing$label_choice))


select_one<- pre_processing%>%
  filter(type_question == "select_one")%>%
  mutate(name_label_choice=if_else(type_question=="select_one", paste(name_choice,"_",label_choice,sep=""),NA))%>%
  dplyr::left_join(select(agroecology_all_choices,c(list_name,name_choice,label_choice,score_agroecology_module )), 
                   by= c("list_name"="list_name",
                         "name_choice"="name_choice",
                         "label_choice"="label_choice"))%>%
  select(!name_label_choice)%>%
  mutate(label_score_agroecology_module= label_choice)

sort(unique(select_one$list_name))
sort(unique(select_one$label_choice))
sort(unique(select_one$name_question_recla))
  
## type_question: select_multiple
select_multiple<- pre_processing%>%
  filter(type_question == "select_multiple")%>%
  #Retain farmers that produce crops, livestock, honey, wood, or others but do not sell them
  filter(!(name_question_recla %in% c("_1_4_2_2_3", "_1_4_2_3_4", "_1_4_2_4_3", "_1_4_2_5_5", "_1_4_2_6_3", "_1_4_2_7_4") & name_choice != 0))
  
select_multiple2<-select_multiple%>%
  arrange(label_choice) %>%
  group_by(kobo_farmer_id, theme, name_question_recla) %>%
  summarise(multiple_responses = paste(label_choice, collapse = "//", sep="")) %>%
  ungroup()%>%
  left_join(agroecology_global_score, by=c("name_question_recla",  "multiple_responses"))%>%
  mutate(score_agroecology_module = case_when(
    name_question_recla %in% c("_1_4_2_2_3", "_1_4_2_3_4", "_1_4_2_4_3", "_1_4_2_5_5", "_1_4_2_6_3", "_1_4_2_7_4") ~ 1,
    TRUE ~ as.numeric(score_agroecology_module)))
  filter(is.na(score_agroecology_module))
  
select_multiple3<- select_multiple%>%
  left_join(select_multiple2, by=c("kobo_farmer_id", "theme","list_name","name_question_recla" ))%>%
  distinct(across(-c(name_question, name_choice, label_choice)), .keep_all = TRUE)%>%
  mutate(label_choice= multiple_responses)%>%
  select(-multiple_responses)

#Counting----
area <- pre_processing %>%
  #Retain questions related to area of land for crop, livestock and fish production and total area of land managed by household
  filter(name_question_recla %in% c("_3_4_2_1_1", "_3_4_2_2_1_1", "_3_4_2_2_1_2", "_3_4_2_3_2",
                                    "_1_4_1_1_1","_1_4_1_1_2","_1_4_1_1_3")) %>%
  mutate(name_choice = as.numeric(name_choice))%>% 
  # Remove rows with 999, 9999, or 0
  filter(!(name_choice %in% c(999, 9999, 0))) %>%
  # Convert acres to hectares
  mutate(name_choice = case_when(label_choice == "acres" ~ name_choice * 0.404686, TRUE ~ name_choice))%>%
  mutate(name_question_recla= case_when(
    #Rename area of owned and leased land for livestock production to get total land for livestock production
    name_question_recla %in%c("_3_4_2_2_1_1", "_3_4_2_2_1_2")~ "_3_4_2_2_1_3",
    #Rename total area of owned, leased or shared land  to get total land managed by household
    name_question_recla %in%c( "_1_4_1_1_1","_1_4_1_1_2","_1_4_1_1_3")~ "_1_4_1_1_4",
    TRUE ~ name_question_recla))%>%
  group_by(kobo_farmer_id,name_question_recla) %>%
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
  filter(type_question == "count")
#Check if there are duplicate crops or livestock species for each farmer
  #distinct(across(-c(name_question)), .keep_all = TRUE)
  
sort(unique(counting$name_choice))

counting2<-counting%>%
  filter(!(name_choice%in%c("None","none","No action taken")))%>%
  arrange(label_choice) %>%
  group_by(kobo_farmer_id, name_question_recla, theme) %>%
  summarise(multiple_responses = paste(label_choice, collapse = "//"),
            label_score_agroecology_module = n_distinct(name_choice))%>% 
  ungroup()%>%
  #Left join area of land for crop, livestock, fish production for calculation (number of species/ha)
  left_join(area, by= c("kobo_farmer_id", "name_question_recla"))%>%
  #Retain the rows with production_area_ha==NA (farmer did not know the area of land)
  filter((name_question_recla %in%c("_3_4_3_1_1_2","_3_4_3_3_1")& is.na(production_area_ha)))%>%
  select(-production_area_ha)%>%
  #Replace NA in production_area_ha by total land managed by household
  left_join(area_total, by="kobo_farmer_id")%>%
  select(-name_question_recla.y)%>%
  rename("name_question_recla"="name_question_recla.x")
names(counting2)

counting3<-counting%>%
  filter(!(name_choice%in%c("None","none","No action taken")))%>%
  arrange(label_choice) %>%
  group_by(kobo_farmer_id, name_question_recla, theme) %>%
    summarise(multiple_responses = paste(label_choice, collapse = "//"),
              label_score_agroecology_module = n_distinct(name_choice))%>% 
  ungroup()%>%
  #Left join area of land for crop, livestock, fish production for calculation (number of species/ha)
  left_join(area, by= c("kobo_farmer_id", "name_question_recla"))%>%
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
      label_score_agroecology_module == 1 ~ 2,
      label_score_agroecology_module == 2 ~ 3,
      label_score_agroecology_module == 3 ~ 4,
      label_score_agroecology_module >= 4 ~ 5),
    # 5_biodiversity
    name_question_recla %in% c("_3_4_3_1_1_2", "_3_4_3_3_1") ~ case_when(
      label_score_agroecology_module <= 1 ~ 1,
      label_score_agroecology_module > 1 & label_score_agroecology_module <= 2 ~ 2.33333,
      label_score_agroecology_module > 2 & label_score_agroecology_module <= 3 ~ 3.66666,
      label_score_agroecology_module > 3 ~ 5),
    # 6_synergy
    name_question_recla %in% c("_3_3_3_1", "_2_9_1_1", "_3_3_1_7", "_3_3_3_3", "_3_3_3_4", "_2_12_1") ~ case_when(
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
    
counting4<-counting%>%
  #Retain farmers that applied None practices for .....
  filter(name_choice%in%c("None","none","No action taken"))%>%
  mutate(score_agroecology_module=1,
         label_score_agroecology_module="No practices taken")

sort(unique(counting4$name_choice))
sort(unique(counting3$name_question_recla))
names(counting3)

counting5<- counting%>%
  filter(!(name_choice %in% c("None","none","No action taken")))%>%
  left_join(counting3, by=c("kobo_farmer_id", "theme","name_question_recla" ))%>%
  distinct(across(-c(name_question, name_choice, label_choice)), .keep_all = TRUE)%>%
  mutate(label_choice= multiple_responses,
         name_choice= label_score_agroecology_module)%>%
  select(-multiple_responses)%>%
  mutate(label_score_agroecology_module = as.character(label_score_agroecology_module))%>%
  mutate(label_score_agroecology_module= case_when(
    name_question_recla %in% 
      #4_animal_health
      c("_2_10_1_2","_3_3_3_4",
        #6_synergy
        "_3_3_3_1","_2_9_1_1","_3_3_1_7","_3_3_3_3","_3_3_3_4","_2_12_1")~ paste(name_choice, "practices implemented"),
    #5_biodiversity
    name_question_recla %in% c("_3_4_3_1_1_2")~paste(name_choice, "crop species per ha"),
    name_question_recla %in% c("_3_4_3_3_1")~paste(name_choice, "livestock species per ha"),
    #7_economic_diversification
    name_question_recla %in% c("_2_4_1")~ paste(name_choice, "sources of income"),
    TRUE ~ label_score_agroecology_module))%>%
  rbind(counting4)
  filter(is.na(score_agroecology_module))

#Integer
integer<- pre_processing%>%
    filter(type_question == "integer")%>%
  mutate(label_score_agroecology_module=name_choice,
         score_agroecology_module=NA,
         label_score_agroecology_module=as.numeric(label_score_agroecology_module))%>%
  mutate(score_agroecology_module= case_when(
    # 10_knowlege
      label_score_agroecology_module == 0 ~ 1,
      label_score_agroecology_module == 1 ~ 2,
      label_score_agroecology_module == 2 ~ 3,
      label_score_agroecology_module == 3 ~ 3,
      label_score_agroecology_module == 4 ~ 4,
      label_score_agroecology_module == 5 ~ 4,
      label_score_agroecology_module> 5 ~ 5,
      TRUE ~ score_agroecology_module))%>%
  mutate(label_score_agroecology_module= paste(name_choice, "times per year"))
   
agroecology_module_score<-   rbind(select_one,
                                   select_multiple3,
                                   counting5,
                                   integer)
names(select_one)  
names(select_multiple3)
sort(unique(integer$label_score_agroecology_module))

  
write.csv(agroecology_module_score,file='zwe/zwe_agroecology_score.csv',row.names=FALSE)
