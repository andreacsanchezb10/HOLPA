#--------------------
#library(openxlsx)
library(tidyr)
library(tidyverse)
library(readxl)
library(dplyr)
library("summarytools")

#### AGROECOLOGY: COUNTRY REPORT ------
## type_question: select_one----
# 1_recycling: "_2_8_1_1"  "_2_8_2_1"  "_2_8_3_1" "_2_8_5_1" "_2_8_4_5" = 5/5
# 2_input_reduction: "_3_4_4_2" = 1/6
# 4_animal_health: "_2_10_1_1" = 1/3
# 5_biodiversity:  "_3_3_1_2_1" "_3_3_1_2_2" "_3_3_1_2_3" "_3_3_1_2_4" "_3_3_1_2_6" "_3_3_1_2_7" "_3_3_1_2_8" "_3_3_1_6" = 8/arreglar el total
# 9_social_values:"_2_5_1_1"  "_2_5_1_2"   "_2_5_1_3"   "_2_5_1_4"  = 4/4
# 10_fairness: "_2_6_1_4_1" "_2_6_1_4_2" "_2_6_1_4_3" "_2_6_1_4_4" "_2_6_1_4_5" "_2_6_1_4_6" = 6/6
# 12_governance: "_2_2_1_1" "_2_2_1_2" "_2_2_1_3" = 3/3
# 13_participation: "_2_3_1_4" = 1/1
#total = 29 questions
#Zimbabwe = 26/29 questions (missing: "_2_8_5_1", "_2_6_1_4_3", "_2_6_1_4_4")
#Tunisia = 25/29 questions (missing: "_2_8_5_1","_2_6_1_4_1","_2_6_1_4_3","_2_6_1_4_6")
#Kenya = 28/29 questions (missing: "_2_6_1_4_6")
#Senegal = 29/29 questions 
#Laos = 26/29 questions (missing: "_2_6_1_4_1", "_2_6_1_4_4", "_2_6_1_4_5")

## type_question: select_multiple ----
# 2_input_reduction:"_1_4_3_1", "_1_4_3_5",   "_2_8_5_3","_1_4_3_8", "_1_4_3_9" = 5/6
# 11_connectivity: "_2_7_1_1","_2_7_1_2","_2_7_1_3","_2_7_1_4", "_2_7_1_5","_2_7_1_6","_1_4_2_2_3", "_1_4_2_3_4", "_1_4_2_4_3", "_1_4_2_5_5", "_1_4_2_6_3", "_1_4_2_7_4" = 12/12
#Total = 17 questions
#Zimbabwe = 11/17 questions (missing: "_2_8_5_3", "_1_4_3_9","_2_7_1_3","_2_7_1_4", "_1_4_2_4_3", "_1_4_2_5_5")
#Tunisia = 10/17 questions (missing: "_2_8_5_3","_1_4_3_9","_2_7_1_1","_2_7_1_3","_1_4_2_4_3","_1_4_2_7_4","_2_7_1_6")
#Kenya = 15/17 questions (missing:"_2_7_1_6", "_1_4_2_7_4")
#Senegal= 16/17 questions (missing:"_1_4_2_7_4")
#Laos = 11/17 questions ("_2_7_1_1","_2_7_1_4", "_2_7_1_5",_1_4_2_2_3", "_1_4_2_5_5", "_1_4_2_6_3")

## type_question: integer ----
# 8 - knowledge: "_2_1_1_1" "_2_1_1_2" "_2_1_1_3" "_2_1_1_4" "_2_1_1_5" "_2_1_1_6" "_2_1_1_7" = 7/7
#Total = 7 questions
#Zimbabwe = 7/7 questions
#Tunisia =  7/7 questions
#Kenya =  7/7 questions
#Senegal = 7/7 questions
#Laos = 7/7 questions

## type_question: counting----
# 3- soil_health: "_2_9_1_1" = 1/1
# 4_animal_health: "_2_10_1_2","_3_3_3_4" = 2/3
# 5_biodiversity: "_3_4_3_3_1",_3_4_3_1_1_2
# 6- synergy: "_3_3_3_1_calculate_2","_2_9_1_1","_3_3_1_7","_3_3_3_3","_3_3_3_4","_2_12_1" = 6/6
# 7- economic_diversification: "_2_4_1" = 1/1
#Total = 10 questions (2 duplicated across principles)
#Zimbabwe = 9/10 questions (missing: _3_3_3_4)
#Tunisia = 9/10 questions (missing: _3_3_3_4)
#Kenya = 10/10 questions
#Senegal = 10/10 questions
#Laos = 10/10 questions

#### AGROECOLOGY: SCORING FUNCTION ------
fun_agroecology_data<- function(country_agroecology_data){
  
  pre_processing<- country_agroecology_data%>%
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
  
  select_one<- pre_processing%>%
    filter(type_question == "select_one")%>%
    mutate(name_label_choice=if_else(type_question=="select_one", paste(name_choice,"_",label_choice,sep=""),NA))%>%
    select(!name_label_choice)%>%
    mutate(score_agroecology_module= name_choice,
           label_score_agroecology_module= label_choice)%>%
    mutate(score_agroecology_module= case_when(
      name_question %in%c("_3_3_1_2_1","_3_3_1_2_2","_3_3_1_2_3","_3_3_1_2_4","_3_3_1_2_6","_3_3_1_2_7","_3_3_1_2_8","_3_3_1_6")&score_agroecology_module=="none"~"1",
      name_question %in%c("_3_3_1_2_1","_3_3_1_2_2","_3_3_1_2_3","_3_3_1_2_4","_3_3_1_2_6","_3_3_1_2_7","_3_3_1_2_8","_3_3_1_6")&score_agroecology_module=="low"~"2.34",
      name_question %in%c("_3_3_1_2_1","_3_3_1_2_2","_3_3_1_2_3","_3_3_1_2_4","_3_3_1_2_6","_3_3_1_2_7","_3_3_1_2_8","_3_3_1_6")&score_agroecology_module=="medium"~"3.67",
      name_question %in%c("_3_3_1_2_1","_3_3_1_2_2","_3_3_1_2_3","_3_3_1_2_4","_3_3_1_2_6","_3_3_1_2_7","_3_3_1_2_8","_3_3_1_6")&score_agroecology_module=="high"~"5",
      name_question %in%c("_2_6_1_4_1","_2_6_1_4_2","_2_6_1_4_3","_2_6_1_4_4","_2_6_1_4_5","_2_6_1_4_6")&score_agroecology_module== "0"~"1",
      name_question==  "_2_3_1_4" &score_agroecology_module== "i-dont-know"~"1",
      
      country=="zimbabwe" &name_question =="_2_8_1_1"& name_choice=="6"& label_choice== "All seeds are given by the government" ~"1",
      country=="zimbabwe" &name_question =="_2_8_1_1"& name_choice=="7"& label_choice== "75% of seeds are given by the government, the other 25% are self-purchased." ~"1",
      country=="zimbabwe" &name_question =="_2_8_1_1"& name_choice=="8"& label_choice== "50% of seeds are given by the government, the other 50% are self-purchased." ~"1",
      country=="zimbabwe" &name_question =="_2_8_1_1"& name_choice=="9"& label_choice== "25% of seeds are given by the government, the other 75% are self-purchased." ~"1",
      TRUE ~ score_agroecology_module))
  
  select_multiple<- pre_processing%>%
    filter(type_question == "select_multiple")%>%
    filter(!(name_question_recla %in% c("_1_4_2_2_3", "_1_4_2_3_4", "_1_4_2_4_3", "_1_4_2_5_5", "_1_4_2_6_3", "_1_4_2_7_4") & name_choice != 0))%>%
    mutate(name_question_recla= case_when(
      name_question_recla =="_1_4_2_2_3"~"_2_7_1_1",
      name_question_recla =="_1_4_2_3_4"~"_2_7_1_2",
      name_question_recla == "_1_4_2_4_3"~"_2_7_1_3",
      name_question_recla == "_1_4_2_5_5"~"_2_7_1_4",
      name_question_recla == "_1_4_2_6_3"~"_2_7_1_5",
      name_question_recla == "_1_4_2_7_4"~ "_2_7_1_6",
      TRUE ~ name_question_recla))
  
  select_multiple2<-select_multiple%>%
    arrange(label_choice) %>%
    group_by(kobo_farmer_id, theme, name_question_recla) %>%
    summarise(multiple_responses_label_choice = paste(label_choice, collapse = "//", sep=""),
              multiple_responses_name_choice = paste(name_choice, collapse = "//", sep="")) %>%
    ungroup()
  
  select_multiple3<- select_multiple%>%
    left_join(select_multiple2, by=c("kobo_farmer_id", "theme","name_question_recla" ))%>% #"list_name",
    distinct(across(-c(name_question, name_choice, label_choice)), .keep_all = TRUE)%>%
    mutate(label_choice= multiple_responses_label_choice,
           name_choice= multiple_responses_name_choice)%>%
    select(-multiple_responses_label_choice,-multiple_responses_name_choice)%>%
    mutate(label_score_agroecology_module= label_choice)%>%
    mutate(label_score_agroecology_module = str_replace_all(label_score_agroecology_module, "[()]", ""))%>%
    mutate(label_score_agroecology_module = str_replace_all(label_score_agroecology_module, 
                                                            c("Application of Chemical fertilizers." = "chemical", 
                                                              "Chemical fungicides/pesticides/herbicides." = "chemical",
                                                              "Prepared chemical feeds (i.e., feeds that contain specific chemical compounds or additives)"= "chemical",
                                                              "Antibiotics"= "chemical",
                                                              "Vaccination"= "chemical",
                                                              "Application of Organic fertilizers or Manure."= "organic",
                                                              "Non-chemical fungicides/pesticides/herbicides."="organic",
                                                              "Herbal remedies or traditional medicine"="organic",
                                                              "Organic treatments"="organic",
                                                              "Prepared organic feeds (i.e., feeds that are produced using natural ingredients)"="organic",
                                                              "Use of Ecological practices (e.g., cover crops, legume intercropping, mulching, etc.)."="ecological",
                                                              "Ecological practices (e.g., crop rotation, planting repelling plants)."="ecological",
                                                              "Genetic selection for disease resistance"="ecological",
                                                              "Quarantine measures"="ecological",
                                                              
                                                              "Natural feed (i.e., feeds that occur naturally or introduced in the aquatic environment)"="natural",
                                                              
                                                              "No ecological practices, chemical or organic fertilizer were applied."="none",
                                                              "No ecological practices, chemical or non-chemical pesticides were applied."="none",
                                                              "No action taken"="none",
                                                              "None"="none")))%>%
    mutate(score_agroecology_module= case_when(
      name_question_recla %in% c("_1_4_3_1","_1_4_3_5","_1_4_3_8","_1_4_3_9")&label_score_agroecology_module=="none"~ 5,
      label_score_agroecology_module%in%c("ecological")~ 5,
      label_score_agroecology_module%in%c("organic","organic//organic")~ 4,
      grepl("chemical", label_score_agroecology_module) & grepl("organic", label_score_agroecology_module) &grepl("ecological", label_score_agroecology_module) ~ 3,
      grepl("organic", label_score_agroecology_module) & grepl("ecological", label_score_agroecology_module) ~ 4,
      grepl("chemical", label_score_agroecology_module) & grepl("ecological", label_score_agroecology_module) ~ 3,
      grepl("chemical", label_score_agroecology_module) & grepl("organic", label_score_agroecology_module) ~ 2,
      grepl("chemical", label_score_agroecology_module) & grepl("Other please specify", label_score_agroecology_module) ~ 1,
      label_score_agroecology_module %in% c("chemical","chemical//chemical","Other please specify")~ 1,
      
      label_score_agroecology_module%in%c("natural")~ 5,
      country=="senegal" & name_question_recla=="_2_8_5_3" & name_choice== "other"~ 1,
      country=="laos" & name_question_recla=="_2_8_5_3" & name_choice== "other"~ 5,
      
      grepl("Directly to consumers.", label_score_agroecology_module) ~ 5,
      grepl("Farmers organization/cooperative", label_score_agroecology_module) ~ 4,
      grepl("Trader or supermarket.", label_score_agroecology_module) ~ 3,
      grepl("Aux détaillants tels que les supermarchés, les épiceries ou les restaurants.", label_score_agroecology_module) ~ 3,
      grepl("Local market", label_score_agroecology_module) ~ 3,
      
      
      grepl("To a middle man / aggregator.", label_score_agroecology_module) ~ 2,
      country=="kenya" & name_question_recla == "_2_7_1_1"& label_score_agroecology_module=="Other please, specify"~ 4,
      label_score_agroecology_module=="none"&name_question_recla %in% c("_2_7_1_1","_2_7_1_2","_2_7_1_3","_2_7_1_4", "_2_7_1_5","_2_7_1_6") ~ 1,
      TRUE ~ NA))%>%
    mutate(label_score_agroecology_module= case_when(
      label_score_agroecology_module %in% c("_1_4_3_1","_1_4_3_5","_1_4_3_8","_1_4_3_9")&label_score_agroecology_module=="none"~ "No action taken",
      label_score_agroecology_module%in%c("ecological")~"Only ecological practices/treatments applied",
      label_score_agroecology_module%in%c("organic","organic//organic")~ "Only organic inputs/treatments applied",
      grepl("chemical", label_score_agroecology_module) & grepl("organic", label_score_agroecology_module) &grepl("ecological", label_score_agroecology_module) ~ "Combination of chemical and organic inputs/treatments, and ecological practices/treatments applied",
      grepl("organic", label_score_agroecology_module) & grepl("ecological", label_score_agroecology_module) ~ "Combination of organic inputs/treatments, and ecological practices/treatments applied",
      grepl("chemical", label_score_agroecology_module) & grepl("ecological", label_score_agroecology_module) ~ "Combination of chemical inputs/treatments, and ecological practices/treatments applied",
      grepl("chemical", label_score_agroecology_module) & grepl("organic", label_score_agroecology_module) ~ "Combination of chemical and organic inputs/treatments",
      grepl("chemical", label_score_agroecology_module) & grepl("Other please specify", label_score_agroecology_module) ~ "Only chemical inputs/treatments applied",
      label_score_agroecology_module %in% c("chemical","chemical//chemical","Other please specify")~ "Only chemical inputs/treatments applied",
      label_score_agroecology_module=="none"& name_question_recla%in%c("_1_4_3_1","_1_4_3_5")~ "No ecological practices, chemical or organic inputs were applied.",
      label_score_agroecology_module=="none"& name_question_recla%in%c("_1_4_3_8","_1_4_3_9")~ "No action taken",
      
      label_score_agroecology_module%in%c("natural")~ "Only natural feeds used",
      country=="senegal" & name_question_recla=="_2_8_5_3" & name_choice== "other"~ "Farmer doesn't know",
      country=="laos" & name_question_recla=="_2_8_5_3" & name_choice== "other"~ "Rice bran",
      
      
      grepl("Directly to consumers.", label_score_agroecology_module) ~ "Sold on-farm production directly to consumers",
      grepl("Farmers organization/cooperative", label_score_agroecology_module) ~ "Sold on-farm production to farmers organization/cooperative",
      grepl("Trader or supermarket.", label_score_agroecology_module) ~ "Sold on-farm production to trader or supermarket",
      grepl("Local market", label_score_agroecology_module) ~ "Sold on-farm production to local market",
      
      country=="senegal" & grepl("Aux détaillants tels que les supermarchés, les épiceries ou les restaurants.", label_score_agroecology_module) ~ "Sold on-farm production to retailers such us supermarkets, grocery stores, or restaurants.",
      grepl("To a middle man / aggregator.", label_score_agroecology_module) ~ "Sold on-farm production to a middle man / aggregator.",
      
      country=="kenya" & name_question_recla == "_2_7_1_1"& label_score_agroecology_module=="Other please, specify"~ "Sold on-farm production to schools",
      label_score_agroecology_module=="none"&name_question_recla %in% c("_2_7_1_1","_2_7_1_2","_2_7_1_3","_2_7_1_4", "_2_7_1_5","_2_7_1_6") ~ "Produce on-farm production but did not sold them",
      TRUE ~ NA))
  
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
  
  area <- pre_processing %>%
    #Retain questions related to area of land for crop, livestock and fish production and total area of land managed by household
    filter(name_question_recla %in% c("_3_4_2_1_1", #cropland area
                                      "_3_4_2_2_1_1", "_3_4_2_2_1_2", #livestock production land area
                                      "_3_4_2_3_2", #fish production land area
                                      "_1_4_1_1_1","_1_4_1_1_2","_1_4_1_1_3")) %>% #farmland area
    mutate(name_choice = as.numeric(name_choice))%>% 
    # Remove rows with 9999, or 0
    filter(!(name_choice %in% c(9999, 0))) %>%
    # Convert acres to hectares
    mutate(name_choice = case_when(label_choice == "acres" ~ name_choice * 0.404686, TRUE ~ name_choice))%>%
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
  
  counting<- pre_processing%>%
    filter(type_question == "counting")
  
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
  
  #_2_9_1_1: Get farmers that did not implement any ecological practice to improve soil fertility of cropland in question _1_4_3_1
  counting_1_4_3_1 <- select_multiple3 %>%
    filter(name_question_recla == "_1_4_3_1") %>%
    filter(!str_detect(label_choice, "Use of Ecological practices \\(e\\.g\\., cover crops, legume intercropping, mulching, etc\\.\\)\\."))%>%
    mutate(name_question_recla= "_2_9_1_1",
           label_question= "Which ecological practices do you use on cropland to improve soil quality and health?",
           type_question= "counting",
           label_score_agroecology_module= "0 practices implemented",
           score_agroecology_module= 1)
  
  sort(unique(counting_1_4_3_1$label_choice))
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
    mutate(indicator= case_when(indicator == "2_input_reduction"~ "3_soil_health",TRUE ~ indicator))%>%
    # 6_synergy
    rbind(counting_1_4_3_1)%>%
    mutate(theme= case_when(theme == "2_input_reduction"~ "6_synergy",TRUE ~ theme))%>%
    mutate(indicator= case_when(indicator == "2_input_reduction"~ "6_synergy",TRUE ~ indicator))%>%
    #_3_3_1_7: add farmers that did not implement any ecological practice to  to manage pest in  cropland in question  _3_3_1_7
    rbind(counting_3_3_1_7)

  agroecology_module_score<-   rbind(select_one,
                                     select_multiple3,
                                     counting5,
                                     integer)
  return(agroecology_module_score)
}




#### Set file paths ####
# TO CHECK: I need to connect directly to the share point, I already asked Sebastien for permision
#For now I will leave it like this to continue working


#### Import agroecology data ####
# Data for each county is the outcome from 1_holpa_data_formatting_agroecology.R
### Country databases ####----
#ZIMBABWE----
zwe.data.path <-"C:/Users/andreasanchez/OneDrive - CGIAR/Bioversity/AI/HOLPA/HOLPA_data/Zimbabwe/zimbabwe_data_clean/zwe/zwe_agroecology_format.csv" #andrea path
zwe_agroecology_data <- read.csv(zwe.data.path)

zwe_agroecology<- fun_agroecology_data(zwe_agroecology_data)
sort(unique(zwe_agroecology$theme))
view(dfSummary(zwe_agroecology))

write.csv(zwe_agroecology,file='C:/Users/andreasanchez/OneDrive - CGIAR/Bioversity/AI/HOLPA/HOLPA_data/Zimbabwe/zimbabwe_data_clean/zwe/zwe_agroecology_score.csv',row.names=FALSE)

#TUNISIA----
tun.data.path <-"C:/Users/andreasanchez/OneDrive - CGIAR/Bioversity/AI/HOLPA/HOLPA_data/Tunisia/tunisia_data_clean/tun/tun_agroecology_format.csv" #andrea path

tun_agroecology_data <- read.csv(tun.data.path)

tun_agroecology<- fun_agroecology_data(tun_agroecology_data)
sort(unique(tun_agroecology$theme))
view(dfSummary(tun_agroecology))

write.csv(tun_agroecology,file='C:/Users/andreasanchez/OneDrive - CGIAR/Bioversity/AI/HOLPA/HOLPA_data/Tunisia/tunisia_data_clean/tun/tun_agroecology_score.csv',row.names=FALSE)

#KENYA----
ken.data.path <-"C:/Users/andreasanchez/OneDrive - CGIAR/Bioversity/AI/HOLPA/HOLPA_data/Kenya/kenya_data_clean/ken/ken_agroecology_format.csv" #andrea path

ken_agroecology_data <- read.csv(ken.data.path)
ken_agroecology<- fun_agroecology_data(ken_agroecology_data)

sort(unique(ken_agroecology$theme))
view(dfSummary(ken_agroecology))

write.csv(ken_agroecology,file='C:/Users/andreasanchez/OneDrive - CGIAR/Bioversity/AI/HOLPA/HOLPA_data/Kenya/kenya_data_clean/ken/ken_agroecology_score.csv',row.names=FALSE)

#SENEGAL----
sen.data.path <-"C:/Users/andreasanchez/OneDrive - CGIAR/Bioversity/AI/HOLPA/HOLPA_data/Senegal/senegal_data_clean/sen/sen_agroecology_format.csv" #andrea
sen_agroecology_data <- read.csv(sen.data.path)

sen_agroecology<- fun_agroecology_data(sen_agroecology_data)
sort(unique(sen_agroecology$theme))
view(dfSummary(sen_agroecology))

write.csv(sen_agroecology_data,file='C:/Users/andreasanchez/OneDrive - CGIAR/Bioversity/AI/HOLPA/HOLPA_data/Senegal/senegal_data_clean/sen/sen_agroecology_score.csv',row.names=FALSE)


#LAOS----
#PENDING: Waiting for Somphasith to answer what means ຮໍສin question _2_8_5_3/other
lao.data.path <-"C:/Users/andreasanchez/OneDrive - CGIAR/Bioversity/AI/HOLPA/HOLPA_data/Laos/laos_data_clean/lao/lao_agroecology_format.csv" #andrea
lao_agroecology_data <- read.csv(lao.data.path)

lao_agroecology<- fun_agroecology_data(lao_agroecology_data)
sort(unique(lao_agroecology$theme))
view(dfSummary(lao_agroecology))

write.csv(lao_agroecology_data,file='C:/Users/andreasanchez/OneDrive - CGIAR/Bioversity/AI/HOLPA/HOLPA_data/Laos/laos_data_clean/lao/lao_agroecology_score.csv',row.names=FALSE)
