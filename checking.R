#Check if total cropland match with dissagregated information by systems
cropland_main<-zwe_survey_main%>%  #zimbabwe
  #tun_survey_main%>% #tunisia
  mutate(merge_id = paste(kobo_farmer_id,sheet_id,index,sep="_"))%>%
select(merge_id,"_3_4_2_1_1")
  
names(cropland_main)
length(unique(cropland_main$kobo_farmer_id))
length(unique(cropland_main$merge_id))
sort(unique(cropland_main$merge_id))

##_3_3_3_2_begin_repeat:  area of land per agricultural practice 
cropland_3_3_3_2_begin_repeat<- result2%>%
  filter(indicator=="biodiversity_climate_mitigation")%>%
  mutate(merge_id = case_when(sheet_id=="Final HOLPA_Zimbabwe_Household"~paste(kobo_farmer_id,sheet_id,index,sep = "_"),#zimbabwe
                              #mutate(merge_id = case_when(sheet_id=="HOLPA_Tunisia_household_surv"~paste(kobo_farmer_id,sheet_id,index,sep = "_"),#tunisia
                              TRUE ~paste(kobo_farmer_id,parent_table_name,parent_index,index,sep="_")))%>%
  filter(sheet_id=="_3_3_3_2_begin_repeat")%>%
  select(kobo_farmer_id, merge_id,name_question_recla,name_choice)%>%
  tidyr::spread(key = name_question_recla, value = name_choice)%>%
  mutate(index_id=merge_id,
         merge_id= sub("_[^_]*$", "", merge_id))

#_3_3_3_2_begin_repeat is embedded in survey_main
cropland<-left_join(cropland_main,cropland_3_3_3_2_begin_repeat,by=c("merge_id"="merge_id"))%>%
  mutate_at(vars("_3_3_3_2_2", "_3_4_2_1_1"), as.numeric)%>%
  rename("x_3_3_3_2_2"="_3_3_3_2_2")%>%
  rename("x_3_4_2_1_1"="_3_4_2_1_1")%>%
  mutate(please_check= if_else(x_3_3_3_2_2>x_3_4_2_1_1,1,0 ))

-names(cropland)
