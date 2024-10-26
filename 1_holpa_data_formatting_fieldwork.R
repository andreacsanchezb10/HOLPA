#library(openxlsx)
library(tidyr)
library(tidyverse)
library(readxl)
library(dplyr)
library(summarytools)



# INSTRUCTIONS:  if you are not from SENEGAL or PERU, run the function read_and_process_survey_xlsx and go to your country section

### Country databases ####
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


# INSTRUCTIONS ONLY FOR SENEGAL AND PERU: Please download HOLPA_global_field_survey_20231106.xlsx in your computer from: https://github.com/andreacsanchezb10/HOLPA

#Global databases ####
#fieldwork_global_survey contains the global form with questions
#global_choices contains the global form with choices

#Replace global.data.path path with your path and run the code
fieldwork.global.data.path <-"C:/Users/andreasanchez/OneDrive - CGIAR/Bioversity/AI/HOLPA/analysis/HOLPA/HOLPA/" #path andrea

fieldwork_global_survey <- read_excel(paste0(fieldwork.global.data.path,"HOLPA_global_field_survey_20231106.xlsx"),
                                      sheet = "survey")%>%
  #select only the necessary columns
  select(
    #"module","indicator", "subindicator", 
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

fieldwork_global_choices <- read_excel(paste0(fieldwork.global.data.path,"HOLPA_global_field_survey_20231106.xlsx"),
                                       sheet = "choices")%>%
  select("list_name","name","label::English ((en))")%>%
  rename("label_choice" = "label::English ((en))")%>%
  rename("name_choice" = "name")%>%
  mutate(country= "global")



### ZIMBABWE ----
zwe_data_path <-"C:/Users/andreasanchez/OneDrive - CGIAR/Bioversity/AI/HOLPA/HOLPA_data/Zimbabwe/zimbabwe_data_clean/" #path Andrea

#Household survey
zwe_household_survey <- read_and_process_survey_xlsx("Final HOLPA_Zimbabwe_Household", "_id", paste0(zwe_data_path,"zwe_holpa_household_survey_clean.xlsx"),"zimbabwe","_index")%>%
  #Remove respondents that are not farmers
  filter(kobo_farmer_id!="274186917")

#Fieldwork survey
zwe_fieldwork_survey <- read_and_process_survey_xlsx("zwe_holpa_fieldwork_survey", "_id", paste0(zwe_data_path,"zwe_holpa_fieldwork_survey_clean.xlsx"),"zimbabwe","_index")

names(zwe_fieldwork_survey)

#Add household survey kobo_farmer_id to fieldwork survey
zwe_fieldwork<- zwe_household_survey%>%
  select(kobo_farmer_id, household_id,farmer )%>%
  right_join(zwe_fieldwork_survey,by=c("household_id","farmer"))%>%
  # filter(farmer=="mudzimbabwe _ thomas")%>%
  rename("kobo_farmer_id"="kobo_farmer_id.x",
        "kobo_farmer_id.fieldwork"="kobo_farmer_id.y" )%>%
  #Remove the farmers that do not match in the household survey
  filter(!is.na(kobo_farmer_id))

names(zwe_fieldwork)

write.csv(zwe_fieldwork,file=paste0(zwe_data_path,"zwe/zwe_fieldwork_format.csv"),row.names=FALSE)

#7 farmers do not match with the household survey
zwe_error_fieldwork<- zwe_household_survey%>%
  select(kobo_farmer_id, household_id,farmer )%>%
  right_join(zwe_fieldwork_survey,by=c("household_id","farmer"))%>%
 # filter(farmer=="mudzimbabwe _ thomas")
  filter(is.na(kobo_farmer_id.x))

write.csv(zwe_error_fieldwork,file=paste0(zwe_data_path,"zwe/zwe_error_fieldwork.csv"),row.names=FALSE)

### TUNISIA ----
#ERROR: Tunisia does not have a household_id that matches the household and fieldwork survey.
tun_data_path <-"C:/Users/andreasanchez/OneDrive - CGIAR/Bioversity/AI/HOLPA/HOLPA_data/Tunisia/tunisia_data_clean/" #Andrea

#Household survey
tun_household_survey <- read_and_process_survey_xlsx("HOLPA_Tunisia_household_surv", "_id", paste0(tun_data_path,"tun_holpa_household_survey_clean.xlsx"),"tunisia","_index")%>%
  #Remove respondents that did not wanted to complete the survey
  filter(consent_2!="No")

#Fieldwork survey
tun_fieldwork_survey <- read_and_process_survey_xlsx("tun_holpa_fieldwork_survey", "_id", paste0(tun_data_path,"tun_holpa_fieldwork_survey_clean.xlsx"),"tunisia","_index")

names(tun_fieldwork_survey)

#Add household survey kobo_farmer_id to fieldwork survey
tun_fieldwork<- tun_household_survey%>%
  select(kobo_farmer_id, tun_household_id )%>%
  right_join(tun_fieldwork_survey,by=c("tun_household_id"))%>%
  rename("kobo_farmer_id"="kobo_farmer_id.x",
         "kobo_farmer_id.fieldwork"="kobo_farmer_id.y" )%>%
  #Remove the farmers that do not match in the household survey
  filter(!is.na(kobo_farmer_id))

names(tun_fieldwork)

write.csv(tun_fieldwork,file= paste0(tun_data_path,"tun/tun_fieldwork_format.csv"),row.names=FALSE)

#2 farmers do not match with the household survey
tun_error_fieldwork<- tun_household_survey%>%
  select(kobo_farmer_id, tun_household_id )%>%
  right_join(tun_fieldwork_survey,by=c("tun_household_id"))%>%
  rename("kobo_farmer_id"="kobo_farmer_id.x",
         "kobo_farmer_id.fieldwork"="kobo_farmer_id.y" )%>%
  #Remove the farmers that do not match in the household survey
  filter(is.na(kobo_farmer_id))

write.csv(tun_error_fieldwork,file=paste0(tun_data_path,"tun/tun_error_fieldwork.csv"),row.names=FALSE)

### KENYA ----
ken_data_path <- "C:/Users/andreasanchez/OneDrive - CGIAR/Bioversity/AI/HOLPA/HOLPA_data/Kenya/kenya_data_clean/" #path andrea

#Household survey
ken_household_survey <- read_and_process_survey_xlsx("Holpa_global_household_surve", "_id", paste0(ken_data_path,"ken_holpa_household_survey_clean.xlsx"),"kenya","_index")%>%
  filter(kobo_farmer_id!="274186917") #Remove respondents that are not farmers

names(ken_household_survey)
length(unique(ken_household_survey$kobo_farmer_id))

#Fieldwork survey
ken_fieldwork_survey <- read_and_process_survey_xlsx("ken_holpa_fieldwork_survey", "_id", paste0(ken_data_path,"ken_holpa_fieldwork_survey_clean.xlsx"),"kenya","_index")%>%
  filter(kobo_farmer_id!="274186917") #Remove respondents that are not farmers

names(ken_fieldwork_survey)

names(ken_fieldwork_survey) <- sapply(names(ken_fieldwork_survey), function(x) {
  # If there are at least two "/", return the substring after the second "/", otherwise return the original name
  if (grepl("/", x)) {
    return(sub("^([^/]*/){2}", "", x))  # This replaces everything up to and including the second "/"
  } else {
    return(x)
  }
})

# Check the renamed columns
names(ken_fieldwork_survey)

names(ken_fieldwork_survey) <- sapply(names(ken_fieldwork_survey), function(x) {
  # If the column name contains "begin_group/", extract everything after the first "/"
  if (grepl("begin_group/", x)) {
    return(sub(".*/", "", x))  # This replaces everything up to and including the first "/"
  } else {
    return(x)  # Keep the original column name if it doesn't contain "begin_group/"
  }
})

# Check the renamed columns
names(ken_fieldwork_survey)
length(unique(ken_fieldwork_survey$kobo_farmer_id))

#Add household survey kobo_farmer_id to fieldwork survey
ken_fieldwork<- ken_household_survey%>%
  select(kobo_farmer_id, household_id )%>%
  right_join(ken_fieldwork_survey,by=c("household_id","kobo_farmer_id"))%>%
  #Remove the farmers that do not match in the household survey
  filter(!is.na(kobo_farmer_id))

write.csv(ken_fieldwork,file=paste0(ken_data_path,"ken/ken_fieldwork_format.csv"),row.names=FALSE)

#No errors for ken
ken_error_fieldwork<- ken_household_survey%>%
  select(kobo_farmer_id, household_id )%>%
  right_join(ken_fieldwork_survey,by=c("household_id","kobo_farmer_id"))%>%
  filter(is.na(kobo_farmer_id))

write.csv(ken_error_fieldwork,file=paste0(ken_data_path,"ken/ken_error_fieldwork.csv"),row.names=FALSE)

### SENEGAL ----
## Import data
#link to sen data: https://cgiar-my.sharepoint.com/:f:/r/personal/andrea_sanchez_cgiar_org/Documents/Bioversity/AI/HOLPA/HOLPA_data/Senegal/senegal_data_clean?csf=1&web=1&e=bT58Tm
#INSTRUCTION: Replace sen_data_path path with your path, run the code and then run the code 
#Senegal household and fieldwork databases were developed in the same form
sen_data_path <- "C:/Users/andreasanchez/OneDrive - CGIAR/Bioversity/AI/HOLPA/HOLPA_data/Senegal/senegal_data_clean/" #path andrea
#sen_data_path <- "C:/Users/sjones/CGIAR/Sanchez, Andrea Cecilia (Alliance Bioversity-CIAT) - HOLPA_data/Senegal/senegal_data_clean/" #path sarah

sen_f_survey_file <- paste0(sen_data_path, "sen_holpa_household_survey_clean.xlsx")
sen_f_choices_file <- paste0(sen_data_path, "sen_holpa_household_form_clean.xlsx")

sen_fieldwork_survey <- read_and_process_survey_xlsx("HOLPA Senegal_version finale", "_id", sen_f_survey_file,"senegal","_index")%>%
  #Remove respondents that did not wanted to complete the survey
  filter(consent_2!="No")%>%
  #Remove respondents that are not farmers
  filter(kobo_farmer_id!="309270221") %>%
  slice(-1)%>%
  mutate(x_2_6_1_3= NA)%>%
  rename("x_2_6_1_3_1"= "_2_6_1_3_1",
         "x_2_6_1_3_2"= "_2_6_1_3_2")%>%
  mutate(x_2_6_1_3= x_2_6_1_3_1,
         x_2_6_1_3= if_else(is.na(x_2_6_1_3),x_2_6_1_3_2,x_2_6_1_3))%>%
  rename("_2_6_1_3"="x_2_6_1_3")%>%
  select(sheet_id,1397:1824)

names(sen_fieldwork_survey)
sen_choices <- read_excel(sen_f_choices_file, sheet = "choices")%>%
  mutate(country= "senegal")%>%
  select("list_name","name","label::English ((en))","country")%>%
  rename("label_choice" = "label::English ((en))")%>%
  rename("name_choice" = "name")%>%
  distinct(list_name,name_choice,label_choice, .keep_all = TRUE)

#Add country choices to global choices
sen_fieldwork_global_choices<-fieldwork_global_choices%>%
  rbind(sen_choices)%>%
  arrange(desc(country == "global")) %>%
  #Removing duplicates
  distinct(list_name,name_choice, .keep_all = TRUE) %>%
  right_join(fieldwork_global_survey,by="list_name",relationship="many-to-many")%>%
  left_join(sen_choices,by=c("list_name","name_choice"))%>%
  rename("label_choice"="label_choice.x",
         "label_choice.country"="label_choice.y",
         "country"="country.x")%>%
  select(-country.y)%>%
  mutate(name_question_choice= if_else(type_question=="select_multiple",
                                       paste(name_question,"/",name_choice, sep=""),
                                       name_question))


#INSTRUCTION: Continue running the code from here
fun_f_questions_columns<- function(country_f_choices) {
  performance_questions_columns<- country_f_choices%>% 
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

fun_f_left_join <- function(performance_choices, gathered_data ) {
  
  # Left join for "calculate" and "integer"
  continuous <- gathered_data  %>%
    dplyr::left_join(select(performance_choices,
                            c(name_question, "name_choice", label_choice, label_question,type, type_question, list_name)), 
                     by = "name_question")%>%
    filter(type_question =="calculate"|type_question =="integer"|type_question =="note"|
             type_question =="text"|type_question =="audio"|type_question =="decimal")%>%
    select(-name_choice.y)%>%
    rename("name_choice"="name_choice.x")
  
  # Left join for "select_multiple"
  select_multiple <- gathered_data  %>%
    left_join(select(performance_choices,
                     c(name_question, name_choice, label_choice, label_question, type, type_question, list_name,name_question_choice)), 
              by = c("name_question"="name_question_choice"))%>%
    filter(type_question=="select_multiple")%>%
    select(-name_choice.y,-name_question.y)%>%
    rename("name_choice"="name_choice.x")%>%
    #Remove answers == "0" or NA
    filter(type_question == "select_multiple" & !is.na(name_choice) & name_choice != 0)
  
  # Left join for "select_one" for countries that downloaded the survey with the name_label version in country language (country== "sen")
  select_one3 <- gathered_data  %>%
    left_join(select(performance_choices,
                     c(name_question, name_choice, label_choice, label_question, type, type_question, list_name,label_choice.country)), 
              by = c("name_question"="name_question", "name_choice"="label_choice.country"))%>%
    select(-name_choice)%>%
    dplyr::rename("name_choice"="name_choice.y")%>%
    filter(type_question=="select_one")%>%
    filter(country== "senegal")
  
  result<- rbind(continuous,select_multiple,select_one3)
  
  return(result)
}

## Function to get answers from the following sections 
# Main survey
fun_f_main<- function(country_global_choices,country_f_survey){
  country_f_choices<-  country_global_choices
  country_f_question_columns<- fun_f_questions_columns(country_f_choices)
  
  country_f_columns <- intersect(country_f_question_columns, colnames(country_f_survey))
  
  country_f <- country_f_survey %>%
    select(all_of(country_f_columns))%>%
    mutate_all(as.character)
  
  # Identify columns with only NA values
  na_columns <- colSums(is.na(country_f)) == nrow(country_f)
  
  # Remove columns with only NA values
  country_f <- country_f[, !na_columns]
  
  result_main_survey <- country_f%>%
    gather(key = "name_question", value = "name_choice", -kobo_farmer_id, -country,-sheet_id,-index)%>%
    fun_f_left_join(country_f_choices,.)%>%
    mutate(name_question_recla= name_question)%>%
    mutate(parent_table_name= NA,
           parent_index=NA)
  return(result_main_survey)
}


fun_performance<- function(country_f_data){
  country_f<- country_f_data%>%
    #For the countries that translated the name of the crops, livestock and fish to English separated with "//"
    mutate(name_choice= case_when(
      name_question_recla %in%c("_1_111_2_other","_1_113_2_other","_1_115_3_other","_1_201_1_other",             
                                "_1_202_1_other","_1_203_1_other") & grepl("//", name_choice)~ sub(".*//", "", name_choice),
      TRUE ~ name_choice))%>%
    
    
    return(country_f)
}

sen_fieldwork<-fun_f_main(sen_fieldwork_global_choices,sen_fieldwork_survey)
sen_fieldwork<-fun_performance(sen_fieldwork)%>%
  pivot_wider(id_cols = "kobo_farmer_id",names_from = "name_question", values_from = name_choice)
names(sen_fieldwork)

write.csv(sen_fieldwork,file=paste0(sen_data_path,"sen/sen_fieldwork_format.csv"),row.names=FALSE)


### LAOS ----
lao_data_path <-"C:/Users/andreasanchez/OneDrive - CGIAR/Bioversity/AI/HOLPA/HOLPA_data/Laos/laos_data_clean/" #Andrea

#Household survey
lao_household_survey <- read_and_process_survey_xlsx("Final_HOLPA_Laos", "_id", paste0(lao_data_path,"lao_holpa_household_survey_clean.xlsx"),"laos","_index")
names(lao_household_survey)

#Fieldwork survey
lao_fieldwork_survey <- read_and_process_survey_xlsx("lao_holpa_fieldwork_survey", "_id", paste0(lao_data_path,"lao_holpa_fieldwork_survey_clean.xlsx"),"laos","_index")

names(lao_fieldwork_survey)

#Add household survey kobo_farmer_id to fieldwork survey
lao_fieldwork<- lao_household_survey%>%
  select(kobo_farmer_id, household_id )%>%
  right_join(lao_fieldwork_survey,by=c("household_id","kobo_farmer_id"))
  rename("kobo_farmer_id"="kobo_farmer_id.x",
         "kobo_farmer_id.fieldwork"="kobo_farmer_id.y" )%>%
  #Remove the farmers that do not match in the household survey
  filter(!is.na(kobo_farmer_id))

names(lao_fieldwork)
lao_fieldwork%>%select(sheet_id)
write.csv(lao_fieldwork,file=paste0(lao_data_path,"lao/lao_fieldwork_format.csv"),row.names=FALSE)

#No errors for Laos
lao_error_fieldwork<- lao_household_survey%>%
  select(kobo_farmer_id, household_id )%>%
  right_join(lao_fieldwork_survey,by=c("household_id","kobo_farmer_id"))%>%
  filter(is.na(kobo_farmer_id))

#write.csv(lao_error_fieldwork,file=paste0(lao_data_path,"lao/lao_error_fieldwork.csv"),row.names=FALSE)


### PERU ----
per_data_path <- "C:/Users/andreasanchez/OneDrive - CGIAR/Bioversity/AI/HOLPA/HOLPA_data/Peru/peru_data_clean/" #path andrea

#Fieldwork household codes
code_fieldwork_household <- read_excel(paste0(per_data_path,"farmer_id_houshold_fieldwork.xlsx"),
                                      sheet = "Cod campo_hogar")%>%
  rename("f_kobo_farmer_id" = "id_CAMPO",
         "kobo_farmer_id" = "id_HOGAR")

#Fieldwork survey
per_fieldwork_survey <- paste0(per_data_path,"per_holpa_fieldwork_survey_clean.xlsx")

# Read the sheets and set the common key column name
per_fieldwork_sheet_names <- excel_sheets(per_fieldwork_survey)
common_key <- "hid"  # Replace with the actual common key in your data

# Assuming the first sheet is the main table
per_fieldwork <- read_excel(per_fieldwork_survey, sheet = per_fieldwork_sheet_names[1])

# Loop over the other sheets (excluding the main one) and left join them with the main table
for (sheet in per_fieldwork_sheet_names[-1]) {
  # Create the name of the sheet prefixed with "per_survey"
  sheet_df_name <- paste0("per_survey_", sheet)
  
  # Read the sheet
  temp_sheet <- read_excel(per_fieldwork_survey, sheet = sheet)
  
  # Check if the common key exists in the sheet
  if (common_key %in% colnames(temp_sheet)) {
    # Perform the left join
    per_fieldwork <- left_join(per_fieldwork, temp_sheet, by = common_key)
    print(paste("Joined with", sheet, "- resulting columns:", ncol(per_fieldwork)))
  } else {
    print(paste("Skipping", sheet, "- common key", common_key, "not found"))
  }
}

per_fieldwork_survey<-per_fieldwork%>%
  mutate(country = "peru")%>%
  dplyr::rename("f_kobo_farmer_id" = "hid")
                

per_fieldwork_global_choices<-fieldwork_global_choices%>%
  right_join(fieldwork_global_survey,by="list_name",relationship="many-to-many")%>%
  mutate(name_question_choice= if_else(type_question=="select_multiple",
                                       paste(name_question,"/",name_choice, sep=""),
                                       name_question))

# For Peru select only the select_multiple columns
per_fun_questions_columns<- function(fieldwork_choices) {
  per_fieldwork_questions_columns<- fieldwork_choices%>% 
    filter(type_question=="select_multiple")%>%
    dplyr::select(name_question, label_question)%>%
    dplyr::distinct(name_question, .keep_all = TRUE)%>%
    spread(key = name_question, value = label_question)%>%
    mutate("f_kobo_farmer_id"="f_kobo_farmer_id",
           "country"="country_name",
           "sheet_id"="sheet_id",
           "parent_table_name"="_parent_table_name",
           "index"="index",
           "parent_index"="_parent_index")
  
  per_fieldwork_questions_columns <- colnames(per_fieldwork_questions_columns)
  per_fieldwork_questions_columns
  
  return(per_fieldwork_questions_columns)
  
}

per_fun_fieldwork_main<- function(country_global_choices, country_survey_main) {
  
  # Step 1: Apply per_fun_performance_main logic
  per_country_fieldwork_choices <- country_global_choices
  per_country_fieldwork_question_columns <- per_fun_questions_columns(per_country_fieldwork_choices)
  per_country_fieldwork_question_columns
  
  per_country_performance_columns <- intersect(per_country_fieldwork_question_columns, colnames(country_survey_main))
  mismatched_columns <- setdiff(per_country_fieldwork_question_columns, per_country_performance_columns)
  mismatched_columns
  
  per_country_performance <- country_survey_main %>%
    select(all_of(per_country_performance_columns)) %>%
    mutate_all(as.character)
  
  # Remove columns with only NA values
  na_columns <- colSums(is.na(per_country_performance)) == nrow(per_country_performance)
  per_country_performance <- per_country_performance[, !na_columns]
  
  # Step 2: Apply generate_binary_matrix logic
  # Initialize the result dataframe with f_kobo_farmer_id
  result <- country_survey_main %>% select(f_kobo_farmer_id,"country")
  
  cols_to_process <- names(per_country_performance)[!names(per_country_performance) %in% c("f_kobo_farmer_id", "country")]
  cols_to_process
  
  for (col in cols_to_process) {
    
    # Separate rows based on comma-separated values
    data_long <- per_country_performance %>%
      select(f_kobo_farmer_id, all_of(col)) %>%
      separate_rows(all_of(col), sep = ",") %>%
      mutate(!!col := paste0(col, "/", !!sym(col)))  # Append the column name
    
    # Create binary columns for each value
    data_wide <- data_long %>%
      mutate(value = 1) %>%
      pivot_wider(names_from = all_of(col), values_from = value, values_fill = 0)
    
    # Merge the result with the wide data (binary columns)
    result <- result %>%
      left_join(data_wide, by = c("f_kobo_farmer_id"))
  }
  
  # Return the final binary matrix
  return(result)
}


per_fieldwork<-per_fun_fieldwork_main(per_fieldwork_global_choices,per_fieldwork_survey)%>%
  left_join(per_fieldwork_survey,by=c("f_kobo_farmer_id","country"))%>%
  left_join(code_fieldwork_household%>% select(kobo_farmer_id,f_kobo_farmer_id),by="f_kobo_farmer_id")%>%
  filter(!is.na(kobo_farmer_id))


write.csv(per_fieldwork,file=paste0(per_data_path,"per/per_fieldwork_format.csv"),row.names=FALSE)


### BURKINA FASO ----
bfa_data_path <-"C:/Users/andreasanchez/OneDrive - CGIAR/Bioversity/AI/HOLPA/HOLPA_data/Burkina_Faso/burkina_faso_data_clean/" #path Andrea

#Household survey
bfa_household_survey <- read_and_process_survey_xlsx("HOLPA_global_household_survey", "_id", paste0(bfa_data_path,"bfa_holpa_household_survey_clean.xlsx"),"burkina_faso","_index")

#Fieldwork survey
bfa_fieldwork_survey <- read_and_process_survey_xlsx("HOLPA_Enquete_exploitation_B", "_id", paste0(bfa_data_path,"bfa_holpa_fieldwork_survey_clean.xlsx"),"burkina_faso","_index")

names(bfa_fieldwork_survey)

#Add household survey kobo_farmer_id to fieldwork survey
bfa_fieldwork<- bfa_household_survey%>%
  select(kobo_farmer_id, household_id )%>%
  right_join(bfa_fieldwork_survey,by=c("household_id"))%>%
  rename("kobo_farmer_id"="kobo_farmer_id.x",
         "kobo_farmer_id.fieldwork"="kobo_farmer_id.y" )%>%
  #Remove the farmers that do not match in the household survey
  filter(!is.na(kobo_farmer_id))

names(bfa_fieldwork)

write.csv(bfa_fieldwork,file=paste0(bfa_data_path,"bfa/bfa_fieldwork_format.csv"),row.names=FALSE)

#54 farmers do not match with the household survey
bfa_error_fieldwork<- bfa_household_survey%>%
  select(kobo_farmer_id, household_id )%>%
  right_join(bfa_fieldwork_survey,by=c("household_id"))%>%
  filter(is.na(kobo_farmer_id.x))

write.csv(bfa_error_fieldwork,file=paste0(bfa_data_path,"bfa/bfa_error_fieldwork.csv"),row.names=FALSE)
