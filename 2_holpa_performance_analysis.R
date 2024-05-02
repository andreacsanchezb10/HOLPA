#Code to convert the long version into the short version
#Code to combine the databases from main dataset and repeat groups

# Indicator: productivity_crops 
productivity_crops_long<- result2%>%
  filter(indicator=="productivity_crops")%>%
  mutate(merge_id = case_when(sheet_id=="Final HOLPA_Zimbabwe_Household"~paste(kobo_farmer_id,sheet_id,index,sep = "_"),TRUE ~paste(kobo_farmer_id,parent_table_name,parent_index,index,sep="_"))) 

sort(unique(productivity_crops$label_question))
sort(unique(productivity_crops$name_question_recla))
sort(unique(productivity_crops$sheet_id))
sort(unique(productivity_crops$merge_id))

productivity_crops_final<- productivity_crops%>%
  filter(indicator=="productivity_crops")%>%
  filter(sheet_id=="Final HOLPA_Zimbabwe_Household")%>%
  select("kobo_farmer_id",    "country","module",  name_question_recla ,name_choice,     "theme","merge_id",             "indicator")%>%
  spread(key = name_question_recla, value = name_choice)

names(productivity_crops_final)
length(unique(productivity_crops_final$kobo_farmer_id))
length(unique(productivity_crops_final$merge_id))


productivity_crops_repeat<- productivity_crops%>%
  filter(indicator=="productivity_crops")%>%
  filter(sheet_id!="Final HOLPA_Zimbabwe_Household")%>%
  select( merge_id,name_question,name_choice   )%>%
  filter(!is.na(name_choice))%>%
  tidyr::spread(key = name_question, value = name_choice)%>%
  mutate(index_id=merge_id,
         merge_id= sub("_[^_]*$", "", merge_id))

productivity_crops_short<-left_join(productivity_crops_final,productivity_crops_repeat,by="merge_id" )


