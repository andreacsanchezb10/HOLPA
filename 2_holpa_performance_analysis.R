#Code to convert the long version into the short version
#Code to combine the databases from main dataset and repeat groups

# Important considerations
#Area metric
zbw =="acres"
# currency 
zbw == "US dollars" #???


#### MODULE: PERFORMANCE
## THEME: ECONOMIC
# Indicator: labour_productivity
labour_productivity_long<- result2%>%
  filter(indicator=="labour_productivity")%>%
  mutate(merge_id = case_when(sheet_id=="Final HOLPA_Zimbabwe_Household"~paste(kobo_farmer_id,sheet_id,index,sep = "_"),
                              TRUE ~paste(kobo_farmer_id,parent_table_name,parent_index,index,sep="_"))) 

sort(unique(labour_productivity_long$label_question))
sort(unique(labour_productivity_long$name_question_recla))
sort(unique(labour_productivity_long$sheet_id))
     "_3_4_1_2_7_2_1_begin_repeat"    

sort(unique(labour_productivity_long$merge_id))

labour_productivity_final<- labour_productivity_long%>%
  filter(sheet_id=="Final HOLPA_Zimbabwe_Household")%>%
  select("kobo_farmer_id",    "country","module",  name_question_recla ,name_choice,     "theme","merge_id",             "indicator")%>%
  spread(key = name_question_recla, value = name_choice)

names(labour_productivity_final)
length(unique(labour_productivity_final$kobo_farmer_id))
length(unique(labour_productivity_final$merge_id))
sort(unique(labour_productivity_final$merge_id))

##_3_4_1_1_7_1_begin_repeat: household members permanent workers
labour_productivity_repeat1<- labour_productivity_long%>%
  filter(sheet_id=="_3_4_1_1_7_1_begin_repeat")%>%
  #mutate(merge_id2= paste(kobo_farmer_id,sheet_id,index,sep = "_"))%>%
  select( merge_id,merge_id2,name_question_recla,name_choice)%>%
  #filter(!is.na(name_choice))%>%
  tidyr::spread(key = name_question_recla, value = name_choice)%>%
  mutate(index_id=merge_id,
         merge_id= sub("_[^_]*$", "", merge_id))

##_3_4_1_1_7_2_begin_repeat: household members seasonal workers 1 
labour_productivity_repeat2<- labour_productivity_long%>%
  filter(sheet_id=="_3_4_1_1_7_2_begin_repeat")%>%
  mutate(merge_id2= paste(kobo_farmer_id,sheet_id,index,sep = "_"))%>%
  select( merge_id,merge_id2 ,name_question_recla,name_choice)%>%
  #filter(!is.na(name_choice))%>%
  tidyr::spread(key = name_question_recla, value = name_choice)%>%
  mutate(index_id=merge_id,
         merge_id= sub("_[^_]*$", "", merge_id))

##_3_4_1_2_7_2_1_begin_repeat : household members seasonal workers 2 ----
labour_productivity_repeat3<- labour_productivity_long%>%
  filter(sheet_id=="_3_4_1_2_7_2_1_begin_repeat")%>%
  select( merge_id,name_question_recla,name_choice)%>%
  #filter(!is.na(name_choice))%>%
  tidyr::spread(key = name_question_recla, value = name_choice)%>%
  mutate(index_id=merge_id,
         merge_id= sub("_[^_]*$", "", merge_id))

#labour_productivity_repeat3 is embedded in labour_productivity_repeat2
labour_productivity_short<-left_join(labour_productivity_repeat2,labour_productivity_repeat3,by=c("merge_id2"="merge_id"))
sort(unique(productivity_livestock_short$merge_id))

#repeat 1 is embedded in main dataset
labour_productivity_short2<-left_join(labour_productivity_final,labour_productivity_repeat1,by="merge_id" )%>%
  left_join(labour_productivity_short,by="merge_id")


## THEME: AGRICULTURAL
# Indicator: productivity_crops ----
productivity_crops_long<- result2%>%
  filter(indicator=="productivity_crops")%>%
  mutate(merge_id = case_when(sheet_id=="Final HOLPA_Zimbabwe_Household"~paste(kobo_farmer_id,sheet_id,index,sep = "_"),TRUE ~paste(kobo_farmer_id,parent_table_name,parent_index,index,sep="_"))) 

sort(unique(productivity_crops_long$sheet_id))

productivity_crops_final<- productivity_crops_long%>%
  filter(sheet_id=="Final HOLPA_Zimbabwe_Household")%>%
  select("kobo_farmer_id",    "country","module",  name_question_recla ,name_choice,     "theme","merge_id",             "indicator")%>%
  spread(key = name_question_recla, value = name_choice)

names(productivity_crops_final)
length(unique(productivity_crops_final$kobo_farmer_id))
length(unique(productivity_crops_final$merge_id))

productivity_crops_repeat<- productivity_crops_long%>%
  filter(sheet_id!="Final HOLPA_Zimbabwe_Household")%>%
  select( merge_id,name_question_recla,name_choice   )%>%
  filter(!is.na(name_choice))%>%
  tidyr::spread(key = name_question_recla, value = name_choice)%>%
  mutate(index_id=merge_id,
         merge_id= sub("_[^_]*$", "", merge_id))

productivity_crops_short<-left_join(productivity_crops_final,productivity_crops_repeat,by="merge_id" )

# Indicator: productivity_livestock ----
productivity_livestock_long<- result2%>%
  filter(indicator=="productivity_livestock")%>%
  mutate(merge_id = case_when(sheet_id=="Final HOLPA_Zimbabwe_Household"~paste(kobo_farmer_id,sheet_id,index,sep = "_"),
                              TRUE ~paste(kobo_farmer_id,parent_table_name,parent_index,index,sep="_"))) 

sort(unique(productivity_livestock_long$label_question))
sort(unique(productivity_livestock_long$name_question_recla))
sort(unique(productivity_livestock_long$sheet_id))
sort(unique(productivity_livestock_long$merge_id))

productivity_livestock_final<- productivity_livestock_long%>%
  filter(sheet_id=="Final HOLPA_Zimbabwe_Household")%>%
  select("kobo_farmer_id",    "country","module",  name_question_recla ,name_choice,     "theme","merge_id",             "indicator")%>%
  spread(key = name_question_recla, value = name_choice)

names(productivity_livestock_final)
length(unique(productivity_livestock_final$kobo_farmer_id))
length(unique(productivity_livestock_final$merge_id))
sort(unique(productivity_livestock_final$merge_id))

productivity_livestock_repeat1<- productivity_livestock_long%>%
  filter(sheet_id=="_3_4_2_2_2_begin_repeat")%>%
  mutate(merge_id2= paste(kobo_farmer_id,sheet_id,index,sep = "_"))%>%
  select( merge_id,merge_id2,name_question_recla,name_choice)%>%
  #filter(!is.na(name_choice))%>%
  tidyr::spread(key = name_question_recla, value = name_choice)%>%
  mutate(index_id=merge_id,
         merge_id= sub("_[^_]*$", "", merge_id))
         
productivity_livestock_repeat2<- productivity_livestock_long%>%
  filter(sheet_id=="_3_4_2_2_6_begin_repeat")%>%
  select( merge_id,name_question_recla,name_choice)%>%
  #filter(!is.na(name_choice))%>%
  tidyr::spread(key = name_question_recla, value = name_choice)%>%
  mutate(index_id=merge_id,
         merge_id= sub("_[^_]*$", "", merge_id))

#Repeat2 is embedded in repeat1
productivity_livestock_short<-left_join(productivity_livestock_repeat1,productivity_livestock_repeat2,by=c("merge_id2"="merge_id"))
sort(unique(productivity_livestock_short$merge_id))

#repeat 1 is embedded in main dataset
productivity_livestock_short2<-left_join(productivity_livestock_final,productivity_livestock_short,by="merge_id" )
