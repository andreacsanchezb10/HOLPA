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
#Peru = 26/29 questions (missing:"_2_6_1_4_6","_3_3_1_2_4","_2_6_1_4_5" ) (extra questions: "_3_3_1_2_10","_3_3_1_2_11")
#Burkina Faso =  25/29 questions (missing: "_2_8_5_1", "_2_6_1_4_3", "_2_6_1_4_5", "_2_6_1_4_6")


## type_question: select_multiple ----
# 2_input_reduction:"_1_4_3_1", "_1_4_3_5",   "_2_8_5_3","_1_4_3_8", "_1_4_3_9" = 5/6
# 11_connectivity: "_2_7_1_1","_2_7_1_2","_2_7_1_3","_2_7_1_4", "_2_7_1_5","_2_7_1_6","_1_4_2_2_3", "_1_4_2_3_4", "_1_4_2_4_3", "_1_4_2_5_5", "_1_4_2_6_3", "_1_4_2_7_4" = 12/12
#Total = 17 questions
#Zimbabwe = 11/17 questions (missing: "_2_8_5_3", "_1_4_3_9","_2_7_1_3","_2_7_1_4", "_1_4_2_4_3", "_1_4_2_5_5")
#Tunisia = 10/17 questions (missing: "_2_8_5_3","_1_4_3_9","_2_7_1_1","_2_7_1_3","_1_4_2_4_3","_1_4_2_7_4","_2_7_1_6")
#Kenya = 15/17 questions (missing:"_2_7_1_6", "_1_4_2_7_4")
#Senegal= 16/17 questions (missing:"_1_4_2_7_4")
#Laos = 11/17 questions (missing: "_2_7_1_1","_2_7_1_4", "_2_7_1_5",_1_4_2_2_3", "_1_4_2_5_5", "_1_4_2_6_3")
#Peru = 13/17 questions (missing: "_2_7_1_6", "_1_4_2_4_3", "_1_4_2_5_5", "_1_4_2_7_4")
#Burkina Faso =  11/17  (missing:"_2_8_5_3", "_1_4_3_9","_2_7_1_3","_2_7_1_5", "_1_4_2_4_3", "_1_4_2_7_4")


## type_question: integer ----
# 8 - knowledge: "_2_1_1_1" "_2_1_1_2" "_2_1_1_3" "_2_1_1_4" "_2_1_1_5" "_2_1_1_6" "_2_1_1_7" = 7/7
#Total = 7 questions
#Zimbabwe = 7/7 questions
#Tunisia =  7/7 questions
#Kenya =  7/7 questions
#Senegal = 7/7 questions
#Laos = 7/7 questions
#Peru = 7/7 questions
#Burkina Faso = 7/7 questions


## type_question: counting----
# 3- soil_health: "_2_9_1_1" = 1/1
# 4_animal_health: "_2_10_1_2","_3_3_3_4" = 2/3
# 5_biodiversity: "_3_4_3_3_1",_3_4_3_1_1_2,_3_4_3_4_2
# 6- synergy: "_3_3_3_1_calculate_2","_2_9_1_1","_3_3_1_7","_3_3_3_3","_3_3_3_4","_2_12_1" = 6/6
# 7- economic_diversification: "_2_4_1" = 1/1
#Total = 11 questions (2 duplicated across principles)
#Zimbabwe = 9/10 questions (missing: _3_3_3_4)
#Tunisia = 9/10 questions (missing: _3_3_3_4)
#Kenya = 10/10 questions
#Senegal = 10/10 questions
#Laos = 10/10 questions
#Peru = 10/10 questions
#Burkina Faso 9/11 questions= (missing: "_3_3_3_4" ,"_3_4_3_4_2")


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
  
  ##### type_question: select_one----
  select_one<- pre_processing%>%
    filter(type_question == "select_one")%>%
    mutate(name_label_choice=if_else(type_question=="select_one", paste(name_choice,"_",label_choice,sep=""),NA))%>%
    select(!name_label_choice)%>%
    
    ###1_recycling: 
    ##"_2_8_1_1": Where do you source most of your seeds?	
    ##"_2_8_2_1": Where do you source most of your manure and compost?	
    ##"_2_8_3_1":Where do you source most of your livestock?	
    ##"_2_8_5_1": Where do you source most of your spawn, fry or fingerling species and varieties?
    ##"_2_8_4_5":Where do you source most of your energy?	
    #Score 1:All seeds/manure and compost/livestock/sprawn, fry or fingerling/energy are purchased from the market (e.g., agrovet, seed stores, farmers' cooperatives, seed suppliers, etc.).
    #Score 2:75% of seeds/manure and compost/livestock/sprawn, fry or fingerling/energy are purchased from the market, the other 25% are self-produced or exchanged.
    #Score 3:50% of seeds/manure and compost/livestock/sprawn, fry or fingerling/energy are purchased from the market, the other 50% is self-produced or exchanged.
    #Score 4:25% of seeds/manure and compost/livestock/sprawn, fry or fingerling/energy are purchased from the market, the other 75% is self-produced or exchanged.
  #Score 5:All seeds/manure and compost/livestock/sprawn, fry or fingerling/energy are self-produced, exchanged with other farmers or managed collectively.
  
  ### 2_input_reduction 
  ##"_3_4_4_2": Are livestock fed with dry feed (e.g., grains, hay)?
  #Score 1: All the time
  #Score 2: Often
  #Score 3: Sometimes
  #Score 4: Rarely
  #Score 5: Never
  
  ### 4_animal_health 
  ##"_2_10_1_1": Are the animals of your farm healthy and happy?	
  #Score 1: Animals suffer from hunger and thirst, stress and diseases all year long, and are slaughtered without avoiding unnecessary pain.
  #Score 2: Animals suffer periodically/seasonally from hunger and thirst, stress or diseases, and are slaughtered without avoiding unnecessary pain.
  #Score 3: Animals do not suffer from hunger or thirst, but suffer from stress, may be prone to diseases and can suffer from pain at slaughter.
  #Score 4: Animals do not suffer from hunger, thirst or diseases but can experience stress, especially at slaughter.
  #Score 5: Animals do not suffer from stress, hunger, thirst, pain, or diseases, and are slaughtered in a way to avoid unnecessary pain.
  
  ### 9_social_values
  ##"_2_5_1_1":Do you and your family have access to enough healthy food?	
  ##"_2_5_1_2":Do you and your family have access to enough diversified food?
  ##"_2_5_1_3":Do you and your family have access to enough seasonal foods?	
  ##"_2_5_1_4":Do you and your family have access to enough traditional food?	
  #Score 1:No access at all.
  #Score 2:Limited access.
  #Score 3:Moderate access.
  #Score 4:Fairly good.
  #Score 5:Good access.
  
  ### 10_fairness: 
  ##"_2_6_1_4_1":Do you get a fair price for your produced CROPS?	
  ##"_2_6_1_4_2":Do you get a fair price for your produced LIVESTOCK?
  ##"_2_6_1_4_3": Do you get a fair price for your produced FISH?	
  ##"_2_6_1_4_4":Do you get a fair price for your produced wood, bark, rubber, etc. (from TREES)?	
  ##"_2_6_1_4_5":Do you get a fair price for your produced HONEY?	
  ##"_2_6_1_4_6":Do you get a fair price for your produced OTHER products?
  #Score 1:Never get a fair price./I don't know
  #Score 2:Rarely get a fair price.
  #Score 3:Occasionally get a fair price, depending on the product.
  #Score 4:Usually get a fair price, depending on the product.
  #Score 5:Always get a fair price.
  
  
  ### 12_governance 
  ##"_2_2_1_1": How often does your household participate in activities and meetings related to the management of your community's land and natural resources?
  #Score 1:Never participates.
  #Score 2:Rarely participates.
  #Score 3:Sometimes participates.
  #Score 4:Most of the times participates.
  #Score 5:Always participates.
  ##"_2_2_1_2": How often does your household influence the decision-making that goes into the management of your community's land and natural resources?
  #Score 1:Did not contribute to any decision.
  #Score 2:Contribute to few decisions.
  #Score 3:Contribute to some decisions.
  #Score 4:Contribute to almost all the decisions.
  #Score 5:Contribute to all the decisions.
  ##"_2_2_1_3": In your opinion, are your community's land and natural resources well-managed?
  #Score 1:Not at all well-managed.
  #Score 2:Poorly managed.
  #Score 3:Moderately managed.
  #Score 4:Well-managed.
  #Score 5:Extremely well-managed.
  
  ### 13_participation 
  ##"_2_3_1_4":In your opinion, how effective are farmer associations/organizations at supporting farmers in business?
  #Score 1:I don't know.
  #Score 1:Associations/organizations offer no support to farmers' businesses.
  #Score 2:Associations/organizations provide limited support to farmers in business, with marginal impact on their overall success.
  #Score 3:Associations/organizations offer satisfactory support to farmers, aiding them in various aspects of their businesses (e.g., market access, information sharing, and capacity development).
  #Score 4:Associations/organizations play a significant role in supporting farmers' businesses, providing valuable resources, market opportunities, and essential services.
  #Score 5:Associations/organizations demonstrate exceptional effectiveness in supporting farmers' business ventures, offering comprehensive assistance, fostering growth, and ensuring long-term success.
  
  # For this questions, the score_agroecology_module is the same as name_choice
  mutate(score_agroecology_module= name_choice,
         label_score_agroecology_module= label_choice)%>%
    
    mutate(score_agroecology_module= case_when(
      ### 5_biodiversity  
      ##"_3_3_1_2_1": How would you describe the plant diversity (i.e., number of plant species) in: Bushland patches
      ##"_3_3_1_2_2": How would you describe the plant diversity (i.e., number of plant species) in: Fallow land which is unproductive for at least 1 year
      ##"_3_3_1_2_3": How would you describe the plant diversity (i.e., number of plant species) in: Hedgerows/Live fences
      ##"_3_3_1_2_4": How would you describe the plant diversity (i.e., number of plant species) in: Natural grassland
      ##"_3_3_1_2_6": How would you describe the plant diversity (i.e., number of plant species) in: Remnant forest patches
      ##"_3_3_1_2_7": How would you describe the plant diversity (i.e., number of plant species) in: Wetlands
      ##"_3_3_1_2_8": How would you describe the plant diversity (i.e., number of plant species) in: Woodlots
      ##"_3_3_1_6": How would you describe the diversity of trees (or perennial woody plants) on your farm?	 
      #Score 1: None
      #Score 2.34: Low: only one species.
      #Score 3.67: Medium: two to four species.
      #Score 5: High: five or more species with different heights, woodiness or flowering seasons.
      name_question %in%c("_3_3_1_2_1","_3_3_1_2_2","_3_3_1_2_3","_3_3_1_2_4","_3_3_1_2_6","_3_3_1_2_7","_3_3_1_2_8","_3_3_1_6")&score_agroecology_module=="none"~"1",
      name_question %in%c("_3_3_1_2_1","_3_3_1_2_2","_3_3_1_2_3","_3_3_1_2_4","_3_3_1_2_6","_3_3_1_2_7","_3_3_1_2_8","_3_3_1_6")&score_agroecology_module=="low"~"2.34",
      name_question %in%c("_3_3_1_2_1","_3_3_1_2_2","_3_3_1_2_3","_3_3_1_2_4","_3_3_1_2_6","_3_3_1_2_7","_3_3_1_2_8","_3_3_1_6")&score_agroecology_module=="medium"~"3.67",
      name_question %in%c("_3_3_1_2_1","_3_3_1_2_2","_3_3_1_2_3","_3_3_1_2_4","_3_3_1_2_6","_3_3_1_2_7","_3_3_1_2_8","_3_3_1_6")&score_agroecology_module=="high"~"5",
      name_question %in%c("_2_6_1_4_1","_2_6_1_4_2","_2_6_1_4_3","_2_6_1_4_4","_2_6_1_4_5","_2_6_1_4_6")&score_agroecology_module== "0"~"1",
      name_question==  "_2_3_1_4" &score_agroecology_module== "i-dont-know"~"1",
      
      ##### Only for Zimbabwe
      ### 1_recycling
      ##"_2_8_1_1": Where do you source most of your seeds?	
      country=="zimbabwe" &name_question =="_2_8_1_1"& name_choice=="6"& label_choice== "All seeds are given by the government" ~"1",
      country=="zimbabwe" &name_question =="_2_8_1_1"& name_choice=="7"& label_choice== "75% of seeds are given by the government, the other 25% are self-purchased." ~"1",
      country=="zimbabwe" &name_question =="_2_8_1_1"& name_choice=="8"& label_choice== "50% of seeds are given by the government, the other 50% are self-purchased." ~"1",
      country=="zimbabwe" &name_question =="_2_8_1_1"& name_choice=="9"& label_choice== "25% of seeds are given by the government, the other 75% are self-purchased." ~"1",
      TRUE ~ score_agroecology_module))
  
  ##### type_question: select_multiple ----
  
  select_multiple<- pre_processing%>%
    filter(type_question == "select_multiple")%>%
    ##Get farmers that produced CROPS, LIVESTOCK, FISH, WOOD, HONEY, OTHER products, but do not sell them
    filter(!(name_question_recla %in% c("_1_4_2_2_3", "_1_4_2_3_4", "_1_4_2_4_3", "_1_4_2_5_5", "_1_4_2_6_3", "_1_4_2_7_4") & name_choice != 0))%>%
    ##Rename name_question_recla to then use it for 11_connectivity scores
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
    group_by(kobo_farmer_id,country, name_question_recla,module, theme,indicator,label_question) %>%
    summarise(multiple_responses_label_choice = paste(label_choice, collapse = "//", sep=""),
              multiple_responses_name_choice = paste(name_choice, collapse = "//", sep="")) %>%
    ungroup()
  
  select_multiple3<- select_multiple%>%
    left_join(select_multiple2, by=c("kobo_farmer_id","country", "module","theme","indicator","name_question_recla","label_question"  ))%>% 
    distinct(across(-c(name_question, name_choice, label_choice)), .keep_all = TRUE)%>%
    mutate(label_choice= multiple_responses_label_choice,
           name_choice= multiple_responses_name_choice)%>%
    select(-multiple_responses_label_choice,-multiple_responses_name_choice)%>%
    mutate(label_score_agroecology_module= label_choice)%>%
    mutate(label_score_agroecology_module = str_replace_all(label_score_agroecology_module, "[()]", ""))%>%
    
    #Reclassification of responses for easy scoring
    mutate(label_score_agroecology_module = 
             str_replace_all(label_score_agroecology_module,c(
               ### 2_input_reduction:
               ##"_1_4_3_1":Over the past 12 months, what did you do to improve soil fertility of cropland?
               "Application of Chemical fertilizers." = "chemical",
               "Application of Organic fertilizers or Manure."= "organic",
               "Use of Ecological practices (e.g., cover crops, legume intercropping, mulching, etc.)."="ecological",
               "No ecological practices, chemical or organic fertilizer were applied."="none",
               
               ##"_1_4_3_5": Over the past 12 months, what did you do to manage pests in cropland?	
               "Chemical fungicides/pesticides/herbicides." = "chemical",
               "Non-chemical fungicides/pesticides/herbicides."="organic",
               "Ecological practices (e.g., crop rotation, planting repelling plants)."="ecological",
               "No ecological practices, chemical or non-chemical pesticides were applied."="none",
               
               ##"_2_8_5_3": What types of fish feed did you primarily use in the last 12 months?	
               "Prepared chemical feeds (i.e., feeds that contain specific chemical compounds or additives)"= "chemical",
               "Prepared organic feeds (i.e., feeds that are produced using natural ingredients)"="organic",
               "Natural feed (i.e., feeds that occur naturally or introduced in the aquatic environment)"="natural",
               
               ##"_1_4_3_8":Over the past 12 months [add country meaning], how did you manage livestock diseases?	
               ##"_1_4_3_9": Over the past 12 months [add country meaning], how did you manage fish diseases?	
               "Antibiotics"= "chemical",
               "Vaccination"= "chemical",
               "Herbal remedies or traditional medicine"="organic",
               "Organic treatments"="organic",
               "Genetic selection for disease resistance"="ecological",
               "Quarantine measures"="ecological",
               "No action taken"="none",
               "None"="none")))%>%
    
    #Scoring select_multiple responses
    mutate(score_agroecology_module= case_when(
      ### 2_input_reduction:
      ##"_1_4_3_1":Over the past 12 months, what did you do to improve soil fertility of cropland?
      ##"_1_4_3_5": Over the past 12 months, what did you do to manage pests in cropland?	
      ##"_2_8_5_3": What types of fish feed did you primarily use in the last 12 months?	
      ##"_1_4_3_8":Over the past 12 months [add country meaning], how did you manage livestock diseases?	
      ##"_1_4_3_9": Over the past 12 months [add country meaning], how did you manage fish diseases?
      
      #Score 5:No ecological practices, chemical or organic inputs are used
      #Score 5:Only ecological practices are applied.
      #Score 4:Combination of ecological practices and organic inputs are applied
      #Score 4:Only organic inputs area applied
      #Score 3:Combination of ecological practices and organic inputs and chemical inputs are applied
      #Score 3:Combination of ecological practices and chemical inputs are applied
      #Score 2:Combination of chemical and organic inputs are applied
      #Score 1:Only chemical inputs applied
      name_question_recla%in%c("_1_4_3_1","_1_4_3_5","_2_8_5_3","_1_4_3_8","_1_4_3_9")&label_score_agroecology_module=="none"~ 5,
      name_question_recla%in%c("_1_4_3_1","_1_4_3_5","_2_8_5_3","_1_4_3_8","_1_4_3_9")&label_score_agroecology_module%in%c("ecological","natural")~ 5,
      name_question_recla%in%c("_1_4_3_1","_1_4_3_5","_2_8_5_3","_1_4_3_8","_1_4_3_9")&label_score_agroecology_module%in%c("organic","organic//organic")~ 4,
      name_question_recla%in%c("_1_4_3_1","_1_4_3_5","_2_8_5_3","_1_4_3_8","_1_4_3_9")&grepl("chemical", label_score_agroecology_module) & grepl("organic", label_score_agroecology_module) &grepl("ecological", label_score_agroecology_module) ~ 3,
      name_question_recla%in%c("_1_4_3_1","_1_4_3_5","_2_8_5_3","_1_4_3_8","_1_4_3_9")&grepl("organic", label_score_agroecology_module) & grepl("ecological", label_score_agroecology_module) ~ 4,
      name_question_recla%in%c("_1_4_3_1","_1_4_3_5","_2_8_5_3","_1_4_3_8","_1_4_3_9")&grepl("chemical", label_score_agroecology_module) & grepl("ecological", label_score_agroecology_module) ~ 3,
      name_question_recla%in%c("_1_4_3_1","_1_4_3_5","_2_8_5_3","_1_4_3_8","_1_4_3_9")&grepl("chemical", label_score_agroecology_module) & grepl("organic", label_score_agroecology_module) ~ 2,
      name_question_recla%in%c("_1_4_3_1","_1_4_3_5","_2_8_5_3","_1_4_3_8","_1_4_3_9")&grepl("chemical", label_score_agroecology_module) & grepl("Other please specify", label_score_agroecology_module) ~ 1,
      name_question_recla%in%c("_1_4_3_1","_1_4_3_5","_2_8_5_3","_1_4_3_8","_1_4_3_9")&label_score_agroecology_module %in% c("chemical","chemical//chemical","Other please specify")~ 1,
      
      name_question_recla=="_2_8_5_3"&country=="senegal" &  name_choice== "other"~ 1,
      name_question_recla=="_2_8_5_3"&country=="laos" & name_choice== "other"~ 5,
      name_question_recla=="_1_4_3_8"&country=="peru"&kobo_farmer_id=="20231106_AnG"&name_choice=="6//other"~2,
      name_question_recla=="_1_4_3_8"&country=="peru"&kobo_farmer_id=="20231106_ClM"&name_choice=="other//1"~1,
      
      ### 11_connectivity: 
      ##"_2_7_1_1":Do you get a fair price for your produced CROPS?	
      ##"_2_7_1_2": Do you get a fair price for your produced LIVESTOCK?	
      ##"_2_7_1_3":Do you get a fair price for your produced FISH?
      ##"_2_7_1_4": Do you get a fair price for your produced wood, bark, rubber, etc. (from TREES)?	
      ##"_2_7_1_5":Do you get a fair price for your produced HONEY?	
      ##"_2_7_1_6":Do you get a fair price for your produced OTHER products?
      ##"_1_4_2_2_3": 
      ##"_1_4_2_3_4": 
      ##"_1_4_2_4_3": 
      ##"_1_4_2_5_5": 
      ##"_1_4_2_6_3": 
      ##"_1_4_2_7_4" 
      name_question_recla%in%c("_2_7_1_1","_2_7_1_2","_2_7_1_3","_2_7_1_4", "_2_7_1_5","_2_7_1_6","_1_4_2_2_3", "_1_4_2_3_4", "_1_4_2_4_3", "_1_4_2_5_5", "_1_4_2_6_3", "_1_4_2_7_4")&grepl("Directly to consumers.", label_score_agroecology_module) ~ 5,
      name_question_recla%in%c("_2_7_1_1","_2_7_1_2","_2_7_1_3","_2_7_1_4", "_2_7_1_5","_2_7_1_6","_1_4_2_2_3", "_1_4_2_3_4", "_1_4_2_4_3", "_1_4_2_5_5", "_1_4_2_6_3", "_1_4_2_7_4")&grepl("Farmers organization/cooperative", label_score_agroecology_module) ~ 4,
      name_question_recla%in%c("_2_7_1_1","_2_7_1_2","_2_7_1_3","_2_7_1_4", "_2_7_1_5","_2_7_1_6","_1_4_2_2_3", "_1_4_2_3_4", "_1_4_2_4_3", "_1_4_2_5_5", "_1_4_2_6_3", "_1_4_2_7_4")&grepl("Trader or supermarket.", label_score_agroecology_module) ~ 3,
      name_question_recla%in%c("_2_7_1_1","_2_7_1_2","_2_7_1_3","_2_7_1_4", "_2_7_1_5","_2_7_1_6","_1_4_2_2_3", "_1_4_2_3_4", "_1_4_2_4_3", "_1_4_2_5_5", "_1_4_2_6_3", "_1_4_2_7_4")&grepl("Aux détaillants tels que les supermarchés, les épiceries ou les restaurants.", label_score_agroecology_module) ~ 3,
      name_question_recla%in%c("_2_7_1_1","_2_7_1_2","_2_7_1_3","_2_7_1_4", "_2_7_1_5","_2_7_1_6","_1_4_2_2_3", "_1_4_2_3_4", "_1_4_2_4_3", "_1_4_2_5_5", "_1_4_2_6_3", "_1_4_2_7_4")&grepl("Local market", label_score_agroecology_module) ~ 3,
      name_question_recla%in%c("_2_7_1_1","_2_7_1_2","_2_7_1_3","_2_7_1_4", "_2_7_1_5","_2_7_1_6","_1_4_2_2_3", "_1_4_2_3_4", "_1_4_2_4_3", "_1_4_2_5_5", "_1_4_2_6_3", "_1_4_2_7_4")&country=="peru" & grepl("retailers", label_score_agroecology_module) ~ 3,
      name_question_recla%in%c("_2_7_1_1","_2_7_1_2","_2_7_1_3","_2_7_1_4", "_2_7_1_5","_2_7_1_6","_1_4_2_2_3", "_1_4_2_3_4", "_1_4_2_4_3", "_1_4_2_5_5", "_1_4_2_6_3", "_1_4_2_7_4")&grepl("To a middle man / aggregator.", label_score_agroecology_module) ~ 2,
      name_question_recla %in% c("_2_7_1_1","_2_7_1_2","_2_7_1_3","_2_7_1_4", "_2_7_1_5","_2_7_1_6")&label_score_agroecology_module%in%c("none","None//None") ~ 1,
      
      name_question_recla == "_2_7_1_1"&country=="kenya" &  label_score_agroecology_module=="Other please, specify"~ 4,
      TRUE ~ NA))%>%
    #Add labels to agroecology scores
    mutate(label_score_agroecology_module= case_when(
      ### 2_input_reduction:
      name_question_recla%in%c("_1_4_3_1","_1_4_3_5","_2_8_5_3")&label_score_agroecology_module=="none" ~ "No ecological practices, chemical or organic inputs were applied.",
      name_question_recla%in%c("_1_4_3_8","_1_4_3_9")&label_score_agroecology_module=="none" ~ "No action taken",
      name_question_recla%in%c("_1_4_3_1","_1_4_3_5","_2_8_5_3","_1_4_3_8","_1_4_3_9")&label_score_agroecology_module%in%c("ecological")~ "Only ecological practices/treatments are applied",
      name_question_recla%in%c("_1_4_3_1","_1_4_3_5","_2_8_5_3","_1_4_3_8","_1_4_3_9")&label_score_agroecology_module%in%c("natural")~ "Only natural feeds are used",
      name_question_recla%in%c("_1_4_3_1","_1_4_3_5","_2_8_5_3","_1_4_3_8","_1_4_3_9")&label_score_agroecology_module%in%c("organic","organic//organic")~ "Only organic inputs/treatments are applied",
      name_question_recla%in%c("_1_4_3_1","_1_4_3_5","_2_8_5_3","_1_4_3_8","_1_4_3_9")&grepl("chemical", label_score_agroecology_module) & grepl("organic", label_score_agroecology_module) &grepl("ecological", label_score_agroecology_module) ~ "Combination of chemical and organic inputs/treatments, and ecological practices/treatments are applied",
      name_question_recla%in%c("_1_4_3_1","_1_4_3_5","_2_8_5_3","_1_4_3_8","_1_4_3_9")&grepl("organic", label_score_agroecology_module) & grepl("ecological", label_score_agroecology_module) ~ "Combination of organic inputs/treatments, and ecological practices/treatments are applied",
      name_question_recla%in%c("_1_4_3_1","_1_4_3_5","_2_8_5_3","_1_4_3_8","_1_4_3_9")&grepl("chemical", label_score_agroecology_module) & grepl("ecological", label_score_agroecology_module) ~ "Combination of chemical inputs/treatments, and ecological practices/treatments are applied",
      name_question_recla%in%c("_1_4_3_1","_1_4_3_5","_2_8_5_3","_1_4_3_8","_1_4_3_9")&grepl("chemical", label_score_agroecology_module) & grepl("organic", label_score_agroecology_module) ~  "Combination of chemical and organic inputs/treatments are applied",
      name_question_recla%in%c("_1_4_3_1","_1_4_3_5","_2_8_5_3","_1_4_3_8","_1_4_3_9")&grepl("chemical", label_score_agroecology_module) & grepl("Other please specify", label_score_agroecology_module) ~ "Only chemical inputs/treatments are applied",
      name_question_recla%in%c("_1_4_3_1","_1_4_3_5","_2_8_5_3","_1_4_3_8","_1_4_3_9")&label_score_agroecology_module %in% c("chemical","chemical//chemical","Other please specify")~ "Only chemical inputs/treatments are applied",
      
      name_question_recla=="_1_4_3_8"&country=="peru"&kobo_farmer_id=="20231106_AnG"&name_choice=="6//other"~"Combination of chemical and organic inputs/treatments",
      name_question_recla=="_1_4_3_8"&country=="peru"&kobo_farmer_id=="20231106_ClM"&name_choice=="other//1"~"Only chemical inputs/treatments applied",
      name_question_recla=="_2_8_5_3" & country=="senegal" & name_choice== "other"~ "Farmer doesn't know",
      name_question_recla=="_2_8_5_3" & country=="laos" &  name_choice== "other"~ "Rice bran",
      
      
      
      ### 11_connectivity: 
      name_question_recla%in%c("_2_7_1_1","_2_7_1_2","_2_7_1_3","_2_7_1_4", "_2_7_1_5","_2_7_1_6","_1_4_2_2_3", "_1_4_2_3_4", "_1_4_2_4_3", "_1_4_2_5_5", "_1_4_2_6_3", "_1_4_2_7_4")&grepl("Directly to consumers.", label_score_agroecology_module) ~ "On-farm production sold directly to consumers",
      name_question_recla%in%c("_2_7_1_1","_2_7_1_2","_2_7_1_3","_2_7_1_4", "_2_7_1_5","_2_7_1_6","_1_4_2_2_3", "_1_4_2_3_4", "_1_4_2_4_3", "_1_4_2_5_5", "_1_4_2_6_3", "_1_4_2_7_4")&grepl("Farmers organization/cooperative", label_score_agroecology_module) ~ "On-farm production sold to farmers organization/cooperative",
      name_question_recla%in%c("_2_7_1_1","_2_7_1_2","_2_7_1_3","_2_7_1_4", "_2_7_1_5","_2_7_1_6","_1_4_2_2_3", "_1_4_2_3_4", "_1_4_2_4_3", "_1_4_2_5_5", "_1_4_2_6_3", "_1_4_2_7_4")&grepl("Trader or supermarket.", label_score_agroecology_module) ~ "On-farm production sold to trader or supermarket",
      name_question_recla%in%c("_2_7_1_1","_2_7_1_2","_2_7_1_3","_2_7_1_4", "_2_7_1_5","_2_7_1_6","_1_4_2_2_3", "_1_4_2_3_4", "_1_4_2_4_3", "_1_4_2_5_5", "_1_4_2_6_3", "_1_4_2_7_4")&grepl("Local market", label_score_agroecology_module) ~ "On-farm production sold to local market",
      
      name_question_recla%in%c("_2_7_1_1","_2_7_1_2","_2_7_1_3","_2_7_1_4", "_2_7_1_5","_2_7_1_6","_1_4_2_2_3", "_1_4_2_3_4", "_1_4_2_4_3", "_1_4_2_5_5", "_1_4_2_6_3", "_1_4_2_7_4")&country=="senegal" & grepl("Aux détaillants tels que les supermarchés, les épiceries ou les restaurants.", label_score_agroecology_module) ~ "Sold on-farm production to retailers such us supermarkets, grocery stores, or restaurants.",
      name_question_recla%in%c("_2_7_1_1","_2_7_1_2","_2_7_1_3","_2_7_1_4", "_2_7_1_5","_2_7_1_6","_1_4_2_2_3", "_1_4_2_3_4", "_1_4_2_4_3", "_1_4_2_5_5", "_1_4_2_6_3", "_1_4_2_7_4")&grepl("To a middle man / aggregator.", label_score_agroecology_module) ~ "Sold on-farm production to a middle man / aggregator.",
      name_question_recla%in%c("_2_7_1_1","_2_7_1_2","_2_7_1_3","_2_7_1_4", "_2_7_1_5","_2_7_1_6","_1_4_2_2_3", "_1_4_2_3_4", "_1_4_2_4_3", "_1_4_2_5_5", "_1_4_2_6_3", "_1_4_2_7_4")&country=="peru" & name_choice=="retailers" ~ "Sold on-farm production to retailers such us supermarkets, grocery stores, or restaurants.",
      
      name_question_recla == "_2_7_1_1"&country=="kenya" &  label_score_agroecology_module=="Other please, specify"~ "Sold on-farm production to schools",
      name_question_recla %in% c("_2_7_1_1","_2_7_1_2","_2_7_1_3","_2_7_1_4", "_2_7_1_5","_2_7_1_6")&label_score_agroecology_module%in%c("none","None//None") ~ "Produce on-farm production but did not sold them",
      TRUE ~ NA))
  select_multiple3
  
  ## type_question: integer ----
  integer<- pre_processing%>%
    filter(type_question == "integer")%>%
    mutate(label_score_agroecology_module=name_choice,
           label_score_agroecology_module=as.numeric(label_score_agroecology_module))%>%
    mutate(score_agroecology_module= case_when(
      ### 8_knowledge: 
      ##In the last 12 months, how many times has your household exchanged information with the following food system stakeholders to create new or improved solutions to your or others' farming problems?**
      ##_2_1_1_1: Agricultural extension workers
      ##_2_1_1_2: Consumers
      ##_2_1_1_3: Food traders
      ##_2_1_1_4: Government
      ##_2_1_1_5: NGOs
      ##_2_1_1_6:Other farmers
      ##_2_1_1_7: Researchers
      #Score 1:Never.
      #Score 2:1 time per year.
      #Score 3:2 to 3 times per year.
      #Score 4:4 times per year.
      #Score 5: 5 or more times per year.
      label_score_agroecology_module == 0 ~ 1, 
      label_score_agroecology_module == 1 ~ 2, 
      label_score_agroecology_module == 2 ~ 3, 
      label_score_agroecology_module == 3 ~ 3, 
      label_score_agroecology_module == 4 ~ 4,
      label_score_agroecology_module == 5 ~ 5, 
      label_score_agroecology_module> 5 ~ 5,   
      TRUE ~ NA))%>%
    mutate(label_score_agroecology_module= paste(name_choice, "times per year"))
  
  
  ## type_question: counting----
  counting<- pre_processing%>%
    filter(type_question == "counting")
  
  ### 5_biodiversity
  ##_3_4_3_1_1_2: In the last 12 months, how many different crops species (including perennial crops) were produced on your farm?	
  ##"_3_4_3_3_1": In the last 12 months, which different livestock species did you keep?	
  ##"_3_4_3_4_2": Over the past 12 months, how many different fish species did you produce?	
  #Score 1: 0–20th percentile (low diversity)
  #Score 2: 21st–40th percentile
  #Score 3: 41st–60th percentile (around the median)
  #Score 4: 61st–80th percentile
  #Score 5: 81st–100th percentile (high diversity)
  counting1<- counting%>%
    filter(name_question_recla=="_3_4_3_1_1_2"|
             name_question_recla==  "_3_4_3_3_1"|
             name_question_recla=="_3_4_3_4_2")%>%
    group_by(kobo_farmer_id,country, name_question_recla,module, theme,indicator,label_question) %>%
    summarise(multiple_responses_label_choice = paste(label_choice, collapse = "//"),
              label_score_agroecology_module = n_distinct(name_choice))%>%
    ungroup()%>%
    mutate(
      # Calculate the percentile cutoffs
      richness_20th = quantile(label_score_agroecology_module, 0.20, na.rm = TRUE),
      richness_40th = quantile(label_score_agroecology_module, 0.40, na.rm = TRUE),
      richness_60th = quantile(label_score_agroecology_module, 0.60, na.rm = TRUE),
      richness_80th = quantile(label_score_agroecology_module, 0.80, na.rm = TRUE),
      
      # Assign scores based on where each crop and livestock richness falls relative to the cutoffs
      score_agroecology_module = case_when(
        label_score_agroecology_module <= richness_20th ~ 1,
        label_score_agroecology_module <= richness_40th ~ 2,
        label_score_agroecology_module <= richness_60th ~ 3,
        label_score_agroecology_module <= richness_80th ~ 4,
        TRUE ~ 5))%>%
    select(-richness_20th, -richness_40th, -richness_60th, -richness_80th)  # Clean up cutoffs from final output
  
  
  counting2<-counting%>%
    #filter(!(name_choice%in%c("None","none","No action taken")))%>%
    arrange(label_choice) %>%
    group_by(kobo_farmer_id,country, name_question_recla,module, theme,indicator,label_question) %>%
    summarise(multiple_responses_label_choice = paste(label_choice, collapse = "//"),
              label_score_agroecology_module = n_distinct(name_choice))%>%
    ungroup()%>%
    #mutate(label_score_agroecology_module= case_when(str_detect(multiple_responses_label_choice,"none")~(label_score_agroecology_module-1),TRUE ~ label_score_agroecology_module))%>% #this code is particularly for zwe (error in completing the database)
    #mutate(label_score_agroecology_module= case_when(str_detect(multiple_responses_label_choice,"No action taken")~(label_score_agroecology_module-1),TRUE ~ label_score_agroecology_module))%>% #this code is particularly for zwe (error in completing the database)
    
    ### 6_synergy and 3_soil_health
    #Remove unsustainable practices from counting in question "_3_3_3_1_calculate_2"
    mutate(label_score_agroecology_module= case_when(str_detect(multiple_responses_label_choice,"Monoculture with annual crops")~(label_score_agroecology_module-1),TRUE ~ label_score_agroecology_module))%>%
    mutate(label_score_agroecology_module= case_when(str_detect(multiple_responses_label_choice,"Monoculture with perennial crops")~(label_score_agroecology_module-1),TRUE ~ label_score_agroecology_module))%>%
    mutate(label_score_agroecology_module= case_when(str_detect(multiple_responses_label_choice,"Burning crop residues")~(label_score_agroecology_module-1),TRUE ~ label_score_agroecology_module))%>%
    mutate(label_score_agroecology_module= case_when(str_detect(multiple_responses_label_choice,"Land clearing for agriculture")~(label_score_agroecology_module-1),TRUE ~ label_score_agroecology_module))%>%
    #Remove unsustainable practices from counting in question "_3_3_3_3"
    mutate(label_score_agroecology_module= case_when(str_detect(multiple_responses_label_choice,"Overgrazing")~(label_score_agroecology_module-1),TRUE ~ label_score_agroecology_module))%>%
    mutate(label_score_agroecology_module= case_when(str_detect(multiple_responses_label_choice,"No manure management")~(label_score_agroecology_module-1),TRUE ~ label_score_agroecology_module))%>%
    mutate(label_score_agroecology_module= case_when(str_detect(multiple_responses_label_choice,"None")~(label_score_agroecology_module-1),TRUE ~ label_score_agroecology_module))%>% 
    
    mutate(score_agroecology_module= case_when(
      ### 3_soil_health
      ##"_2_9_1_1": Which practices do you use on cropland to improve soil quality and health?	
      #Score 1: No action taken.
      #Score 2: Implementing 1 practice.
      #Score 3: Implementing 2 practices.
      #Score 4: Implementing 3 practices.
      #Score 5: Implementing 4 or more practices.
      theme=="3_soil_health" & name_question_recla %in% c( "_2_9_1_1") ~ case_when(
        label_score_agroecology_module == 0 ~ 1,
        label_score_agroecology_module == 1 ~ 2,
        label_score_agroecology_module == 2 ~ 3,
        label_score_agroecology_module == 3 ~ 4,
        label_score_agroecology_module >= 4 ~ 5),
      
      ### 4_animal_health
      ##"_2_10_1_2": What do you do on the farm to keep animals healthy and happy?
      ##"_3_3_3_4": On the fish production land, did you apply in the last 12 months any of the following practices?
      #Score 1: No action taken.
      #Score 2: Implementing 1 practice.
      #Score 3: Implementing 2 practices.
      #Score 4: Implementing 3 practices.
      #Score 5: Implementing 4 or more practices.
      theme=="4_animal_health" & name_question_recla %in% c("_2_10_1_2", "_3_3_3_4") ~ case_when(
        label_score_agroecology_module == 0 ~ 1,
        label_score_agroecology_module == 1 ~ 2,
        label_score_agroecology_module == 2 ~ 3,
        label_score_agroecology_module == 3 ~ 4,
        label_score_agroecology_module >= 4 ~ 5),
      
      ### 6_synergy
      ##"_3_3_3_1_calculate_2": In the past [localised description of 12 months], did you use any of these practices on your cropland?	
      ##"_2_9_1_1": Which practices do you use on cropland to improve soil quality and health?	
      ##"_3_3_1_7": What practices did you apply in the last 12 months [add country meaning] on the farm to manage crop pests?	
      ##"_3_3_3_3": On the grazing land (owned, leased or shared), did you apply in the last 12 months any of the following practices?	
      ##"_3_3_3_4": On the fish production land, did you apply in the last 12 months any of the following practices?	
      ##"_2_12_1": In addition to actions you mentioned previously, is there anything else you do on your farm to make sure there are positive relationships between animals, crops, trees, soil and water?	
      #Score 1: No action taken.
      #Score 2: Implementing 1 practice.
      #Score 3: Implementing 2 practices.
      #Score 4: Implementing 3 practices.
      #Score 5: Implementing 4 or more practices.
      theme=="6_synergy" & name_question_recla %in% c("_3_3_3_1_calculate_2", "_2_9_1_1", "_3_3_1_7", "_3_3_3_3", "_3_3_3_4", "_2_12_1") ~ case_when(
        label_score_agroecology_module == 0 ~ 1,
        label_score_agroecology_module == 1 ~ 2,
        label_score_agroecology_module == 2 ~ 3,
        label_score_agroecology_module == 3 ~ 4,
        label_score_agroecology_module >= 4 ~ 5),
      
      ### 7_economic_diversification
      ##"_2_4_1": Please select all the income sources your household has	
      #Score 1: One method of income generation.
      #Score 2: Two methods of income generation.
      #Score 3: Three methods of income generation.
      #Score 4: Four methods of income generation.
      #Score 5: Five or more methods of income generation.
      theme=="7_economic_diversification" & name_question_recla %in% c("_2_4_1") ~ case_when(
        label_score_agroecology_module == 1 ~ 1,
        label_score_agroecology_module == 2 ~ 2,
        label_score_agroecology_module == 3 ~ 3,
        label_score_agroecology_module == 4 ~ 4,
        label_score_agroecology_module > 4 ~ 5),
      TRUE ~ NA))%>%
    filter(name_question_recla!="_3_4_3_1_1_2")%>%
    filter(name_question_recla!=  "_3_4_3_3_1")%>%
    filter(name_question_recla!="_3_4_3_4_2")%>%
    ## Add scores from ### 5_biodiversity
    rbind(counting1)
  
  ### 3_soil_health and 6_synergy
  ##Get farmers that did not implement any ecological practice to improve soil fertility of cropland from question _1_4_3_1
  ##_1_4_3_1: Over the past 12 months, what did you do to improve soil fertility of cropland?	
  ##Get the responses != "Use of Ecological practices \\(e\\.g\\., cover crops, legume intercropping, mulching, etc\\.\\)\\."
  #Score 1: No action taken. 
  counting_1_4_3_1 <- select_multiple3 %>%
    filter(name_question_recla == "_1_4_3_1") %>%
    filter(!str_detect(label_choice, "Use of Ecological practices \\(e\\.g\\., cover crops, legume intercropping, mulching, etc\\.\\)\\."))%>%
    mutate(name_question_recla= "_2_9_1_1",
           label_question= "Which ecological practices do you use on cropland to improve soil quality and health?",
           type_question= "counting",
           label_score_agroecology_module= "0 practices implemented",
           score_agroecology_module= 1)
  sort(unique(counting_1_4_3_1$label_choice))
  
  ### 6_synergy
  ## Get farmers that did not implement any ecological practice to  to manage pest in cropland from question  _1_4_3_5
  #_1_4_3_5: What practices did you apply in the last 12 months [add country meaning] on the farm to manage crop pests?	
  ##Get the responses != "Ecological practices \\(e\\.g\\., crop rotation, planting repelling plants\\)\\."
  #Score 1: No action taken. 
  counting_1_4_3_5 <- select_multiple3%>%
    filter(name_question_recla == "_1_4_3_5")%>%
    filter(!str_detect(label_choice, "Ecological practices \\(e\\.g\\., crop rotation, planting repelling plants\\)\\."))%>%
    mutate(name_question_recla= "_3_3_1_7",
           theme="6_synergy",
           indicator="6_synergy",
           label_question= "What ecological practices did you apply in the last 12 months [add country meaning] on the farm to manage crop pests?",
           type_question= "counting",
           label_score_agroecology_module= "0 practices implemented",
           score_agroecology_module= 1)
  sort(unique(counting_1_4_3_5$label_choice))
  
  counting3<- counting%>%
    left_join(counting2, by=c("kobo_farmer_id","country", "module","theme","indicator","name_question_recla","label_question" ))%>%
    distinct(across(c(kobo_farmer_id, country,module, theme, indicator, label_question, type_question, name_question_recla,
                      multiple_responses_label_choice, label_score_agroecology_module, score_agroecology_module)), .keep_all = TRUE)%>%
    mutate(label_choice= multiple_responses_label_choice,
           name_choice= label_score_agroecology_module)%>%
    select(-multiple_responses_label_choice)%>%
    
    ##Add label to the agroecology module scores
    mutate(label_score_agroecology_module= case_when(
      ### 4_animal_health
      name_question_recla %in%c("_2_10_1_2","_3_3_3_4")~ paste(name_choice, "ecological practices are implemented on the farm"),
      ### 5_biodiversity
      name_question_recla %in% c("_3_4_3_1_1_2")~paste(name_choice, "crop species are growing on the farm"),
      name_question_recla %in% c("_3_4_3_3_1")~paste(name_choice, "livestock species are kept on the farm"),
      ### 6_synergy
      name_question_recla %in%c("_3_3_3_1_calculate_2","_2_9_1_1","_3_3_1_7","_3_3_3_3","_3_3_3_4","_2_12_1")~ paste(name_choice, "ecological practices are implemented on the farm"),
      ### 7_economic_diversification
      name_question_recla %in% c("_2_4_1")~ paste(name_choice, "sources of income"),
      TRUE ~ NA))%>%
    
    ## Add farmers that did not implement any ecological practice to improve soil fertility of cropland from question _1_4_3_1
    # 3_soil_health
    rbind(counting_1_4_3_1)%>%
    mutate(theme= case_when(theme == "2_input_reduction"~ "3_soil_health",TRUE ~ theme))%>%
    mutate(indicator= case_when(indicator == "2_input_reduction"~ "3_soil_health",TRUE ~ indicator))%>%
    # 6_synergy
    rbind(counting_1_4_3_1)%>%
    mutate(theme= case_when(theme == "2_input_reduction"~ "6_synergy",TRUE ~ theme))%>%
    mutate(indicator= case_when(indicator == "2_input_reduction"~ "6_synergy",TRUE ~ indicator))
  
  ## Add farmers that did not implement any ecological practice to  to manage pest in cropland from question  _1_4_3_5
  # Check if counting_3_3_1_7 exists before binding it
  if (exists("counting_3_3_1_7")) {
    # Assuming 'existing_data' is the data frame you're binding to
    counting3 <- rbind(counting3, counting_3_3_1_7)
  } else {
    # If it doesn't exist, just use 'counting3' as is
    counting3 <- counting3
  }
  
  ## Combine all agroecology_module_score----
  agroecology_module_score<-   rbind(select_one,
                                     select_multiple3,
                                     integer,
                                     counting3)
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

write.csv(sen_agroecology,file='C:/Users/andreasanchez/OneDrive - CGIAR/Bioversity/AI/HOLPA/HOLPA_data/Senegal/senegal_data_clean/sen/sen_agroecology_score.csv',row.names=FALSE)

#LAOS----
#PENDING: Waiting for Somphasith to answer what means ຮໍສin question _2_8_5_3/other
lao.data.path <-"C:/Users/andreasanchez/OneDrive - CGIAR/Bioversity/AI/HOLPA/HOLPA_data/Laos/laos_data_clean/lao/lao_agroecology_format.csv" #andrea
lao_agroecology_data <- read.csv(lao.data.path)

lao_agroecology<- fun_agroecology_data(lao_agroecology_data)
sort(unique(lao_agroecology$theme))
view(dfSummary(lao_agroecology))

write.csv(lao_agroecology,file='C:/Users/andreasanchez/OneDrive - CGIAR/Bioversity/AI/HOLPA/HOLPA_data/Laos/laos_data_clean/lao/lao_agroecology_score.csv',row.names=FALSE)

#PERU ----
per.data.path <-"C:/Users/andreasanchez/OneDrive - CGIAR/Bioversity/AI/HOLPA/HOLPA_data/Peru/peru_data_clean/per/per_agroecology_format.csv" #andrea path
per_agroecology_data <- read.csv(per.data.path)

per_agroecology<- fun_agroecology_data(per_agroecology_data)
sort(unique(per_agroecology$theme))
view(dfSummary(per_agroecology))

write.csv(per_agroecology,file='C:/Users/andreasanchez/OneDrive - CGIAR/Bioversity/AI/HOLPA/HOLPA_data/Peru/peru_data_clean/per/per_agroecology_score.csv',row.names=FALSE)

#BURKINA FASO ----
bfa.data.path <-"C:/Users/andreasanchez/OneDrive - CGIAR/Bioversity/AI/HOLPA/HOLPA_data/Burkina_Faso/burkina_faso_data_clean/bfa/bfa_agroecology_format.csv" #andrea path
bfa_agroecology_data <- read.csv(bfa.data.path)

bfa_agroecology<- fun_agroecology_data(bfa_agroecology_data)
sort(unique(bfa_agroecology$theme))
view(dfSummary(bfa_agroecology))

write.csv(bfa_agroecology,file='C:/Users/andreasanchez/OneDrive - CGIAR/Bioversity/AI/HOLPA/HOLPA_data/Burkina_Faso/burkina_faso_data_clean/bfa/bfa_agroecology_score.csv',row.names=FALSE)
