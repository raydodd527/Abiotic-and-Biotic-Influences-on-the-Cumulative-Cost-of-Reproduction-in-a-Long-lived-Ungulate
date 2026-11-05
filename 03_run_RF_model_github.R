rm(list=ls())#

library(randomForest)
library(rpart)
##library(devtools)
library(dplyr)
library(tidyverse)
library(beepr)
library(ROCR)
library(pROC)##build an ROC
library(randomForestExplainer)
library(ggplot2)
library(pdp)
library(plotly)
library(datasets)
#library(caret)
library(lubridate)
#remotes::install_github("jeffreyevans/rfUtilities")
library(rfUtilities)
library(zoo)

###all data merging for ## out. I only added the final file with movement data and no location data 


##read in 24, 3, 5, 7 days for data#####
#######################################################
cow_use<-read_csv('D:/R code/Tioga_elk_project/Tioga_elk_study_collar_clean_up/outputs/01_collar_checking_outputs/checking.csv')
exp_date<-cow_use %>%dplyr::mutate(date_time_ex = paste(Date, Time, sep = " ")) %>%
  filter(use_after_points == 'y') %>%
  dplyr::select(c(cow_id, date_time_ex)) %>%
  dplyr::rename(CollarID = cow_id)


##read in cow data with known outcomes
gps_attached<- read_csv("outputs/01_collar_data_prep_part_info/gpsdata_part_info_20240930.csv")
View(gpsdata_part_info_20240930)
gps_attached_min<-gps_attached %>%
  dplyr::filter(j_date >= 91 & j_date <= 243 ) %>%
  dplyr::select(c(CollarID, UTC_Date_time,part_status, j_date))%>%
  dplyr::mutate(CollarID = as.factor(CollarID),
                part_status = as.factor(part_status)) %>%
  rename(timestamps=UTC_Date_time)
names(gps_attached_min)

gps_attached_non_4<-read_csv('outputs/01_collar_data_prep_part_info/gpsdata_NONpart_info_4hr20240924.csv')
gps_attched_non_4_min<-gps_attached_non_4 %>% dplyr::filter(j_date >= 91 & j_date <= 243 & !is.na(j_date)) %>%
  dplyr::select(c(CollarID, UTC_Date_time, tod_end_ ))%>%
  dplyr::mutate(CollarID = as.factor(CollarID)) %>%
  rename(timestamps=UTC_Date_time)

#################################################################################
########################read in movement covariates#####################

##read in 24, 3, 5, 7 days for data#####

#cow_cluster_3<-read.csv('outputs/02_variable_GLM_model/cluster_covariates_3_20241006.csv')

#cow_rMCP_3<-read_csv('outputs/02_variable_GLM_model/rMCP_3_20241003.csv')

#cow_72hr<-read_csv('outputs/02_variable_outputs/rf_covariates_variables_72hr_20241003.csv')

#cow_4hr<-read_csv('outputs/02_variable_outputs/rf_covariates_variables_4hr_20241003.csv')
cow_cluster_1<-read.csv('outputs/02_variable_GLM_model/cluster_covariates_1_20250429.csv')

cow_cluster_3<-read.csv('outputs/02_variable_GLM_model/cluster_covariates_3_20250429.csv')

cow_rMCP_3<-read_csv('outputs/02_variable_GLM_model/rMCP_3_20250424.csv')

#cow_72hr<-read_csv('outputs/02_variable_outputs/rf_covariates_variables_72hr_20250424.csv')#when only 48
cow_72hr<-read_csv('outputs/02_variable_outputs/rf_covariates_variables_72hr_20250903.csv')

cow_4hr<-read_csv('outputs/02_variable_outputs/rf_covariates_variables_4hr_20250429.csv')

cow_192hr<-read_csv("outputs/02_variable_outputs/rf_covariates_variables_192hr_20250829.csv")

#cow_168hr<-read_csv('outputs/02_variable_outputs/rf_covariates_variables_168hr_20241016.csv')

table(cow_4hr$CollarID)

cow_cluster_3<-cow_cluster_3 %>%
  dplyr::mutate(timestamps= ymd_hms(timestamps)) %>%
  dplyr::mutate(cluster_3 = if_else(is.na(clus_ID_3), 0, 1)) %>%
  replace(is.na(.), 0) %>%
  dplyr::select(-c('Latitude', 'Longitude'))

cow_cluster_1<-cow_cluster_1 %>%
  dplyr::mutate(timestamps= ymd_hms(timestamps)) %>%
  dplyr::mutate(cluster_1 = if_else(is.na(clus_ID_1), 0, 1)) %>%
  replace(is.na(.), 0) %>%
  dplyr::select(-c('Latitude', 'Longitude'))

summary(cow_cluster_3)


##remove unneed columns in rMCP

cow_rMCP_3<-cow_rMCP_3 %>% dplyr::select(-c('date_expelled', 'part_status'))


##combine the data sets
cow_all_location<-cow_cluster_3 %>%
  #inner_join(gps_attached_min,  by=c('CollarID', 'timestamps')) %>%
  full_join(cow_rMCP_3, by=c('CollarID', 'timestamps')) %>%
  full_join(cow_72hr,by=c('CollarID', 'timestamps')) %>%
 # full_join(cow_192hr,by=c('CollarID', 'timestamps') ) %>%
  right_join(exp_date, by=c('CollarID')) %>%
  full_join(cow_4hr,by=c('CollarID', 'timestamps') ) %>%
  full_join(cow_cluster_1,by=c('CollarID', 'timestamps') )
table(cow_72hr$part_status)
table(cow_all_location$part_status)

tester_2<-cow_all_location%>% dplyr::filter(part_status == 'neonate <48hr'|
                                              part_status == 'part_date')

test<-tester_2 %>% summarise_all(funs(sum(is.na(.))))


##change the response variable to binary and have it be 48 hrs
part_loc_data<-cow_all_location  %>%
  mutate(part_status = case_when(part_status == 'neonate <48hr'~"1",##make it binary
                                 part_status == 'neonate >48hr'~"0",
                                 part_status == 'pre-part'~"0",
                                 part_status == 'part_date'~"1",
                                 part_status == 'neonate <72hr'~"0",
                                part_status == 'neonate <168hr'~"0")) %>%
  dplyr::mutate(part_status = as.numeric(part_status),
                CollarID = as.factor(CollarID),
                date_time_ex=mdy_hms(date_time_ex)) %>%
  dplyr::filter(part_status != "3")

test<-cow_all_location  %>%
  dplyr::select(c( "CollarID","timestamps", "part_status"  ,   "date_time_ex"))

####################add is known nonpart cows that are 4 hour fixes################


cow_72hr_4N<-read_csv('outputs/02_variable_outputs/rf_covariates_variables_72hr_NONPART_4_20250506.csv')
cow_192hr_4N<- read_csv("outputs/02_variable_outputs/rf_covariates_variables_192hr_4HR_NONPART20250829.csv")
cow_cluster_1_4N<-read.csv('outputs/02_variable_GLM_model/cluster_covariates_1_4H_NONPART20250506.csv')
cow_cluster_3_4N<-read.csv('outputs/02_variable_GLM_model/cluster_covariates_3_4H_NONPART20250506.csv')
cow_rMCP_3_4N<-read_csv('outputs/02_variable_GLM_model/rMCP_3_NONPART_4H_20250506.csv')
cow_4hr_4N<-read_csv('outputs/02_variable_outputs/rf_covariates_variables_4hr_NONPART_4hr_20250506.csv')
#cow_168hr_4N<-read_csv('outputs/02_variable_outputs/rf_covariates_variables_168hr_4HR_NONPART_20241016.csv')

cow_cluster_3_3N<-read.csv('outputs/02_variable_GLM_model/cluster_covariates_3_NONPART_3HR_20250506.csv')
cow_cluster_1_3N<-read.csv('outputs/02_variable_GLM_model/cluster_covariates_1_NONPART_3HR_20250506.csv')
cow_rMCP_3N_3N<-read_csv('outputs/02_variable_GLM_model/rMCP_3_NONPART_3HR_20250506.csv')
cow_72hr_3N<-read_csv('outputs/02_variable_outputs/rf_covariates_variables_72hr_NONPART_3hr20250506.csv')
cow_192hr_3N<-read_csv("outputs/02_variable_outputs/rf_covariates_variables_192hr_3HR_NONPART20250829.csv")
#cow_168hr_3N<-read_csv('outputs/02_variable_outputs/rf_covariates_variables_168hr_3HR_NONPART_20241217.csv')
cow_3hr_3N<-read_csv('outputs/02_variable_outputs/rf_covariates_variables_4hr_NONPART_3hr_20250506.csv')



cow_cluster_3_4N<-cow_cluster_3_4N %>%
  dplyr::mutate(timestamps= ymd_hms(timestamps)) %>%
  dplyr::mutate(cluster_3 = if_else(is.na(clus_ID_3), 0, 1)) %>%
  replace(is.na(.), 0) %>%
  dplyr::select(-c('Latitude', 'Longitude'))

cow_cluster_1_4N<-cow_cluster_1_4N %>%
  dplyr::mutate(timestamps= ymd_hms(timestamps)) %>%
  dplyr::mutate(cluster_1 = if_else(is.na(clus_ID_1), 0, 1)) %>%
  replace(is.na(.), 0) %>%
  dplyr::select(-c('Latitude', 'Longitude'))

table(cow_cluster_3_4N$CollarID)

cow_cluster_3_3N<-cow_cluster_3_3N%>%
  dplyr::mutate(timestamps= ymd_hms(timestamps)) %>%
  dplyr::mutate(cluster_3 = if_else(is.na(clus_ID_3), 0, 1)) %>%
  replace(is.na(.), 0) %>%
  dplyr::select(-c('Latitude', 'Longitude'))

cow_cluster_1_3N<-cow_cluster_1_3N%>%
  dplyr::mutate(timestamps= ymd_hms(timestamps)) %>%
  dplyr::mutate(cluster_1 = if_else(is.na(clus_ID_1), 0, 1)) %>%
  replace(is.na(.), 0) %>%
  dplyr::select(-c('Latitude', 'Longitude'))


##combine all the df for 4 hour fix nonPart cows
cow_all_location_4N<-cow_rMCP_3_4N %>%
  full_join(cow_cluster_3_4N, by=c('CollarID', 'timestamps')) %>%
  full_join(cow_72hr_4N,by=c('CollarID', 'timestamps')) %>%
 # full_join(cow_192hr_4N,by=c('CollarID', 'timestamps')) %>%
  #inner_join(gps_attched_non_4_min,  by=c('CollarID', 'timestamps')) %>%
  full_join(cow_4hr_4N, by=c('CollarID', 'timestamps')) %>%
  full_join(cow_cluster_1_4N, by=c('CollarID', 'timestamps'))


##add in part status, julian date, and date_time of expelled vit so columns match up.

cow_all_location_4N<-cow_all_location_4N %>%
  dplyr::mutate(part_status = 0,
                date_time_ex = '2021-05-05 09:20:42',
                date_time_ex= ydm_hms(date_time_ex),
                j_date = yday(timestamps))# %>%


cow_all_location_3N<-cow_72hr_3N%>%
 # full_join(cow_192hr_3N,by=c('CollarID', 'timestamps')) %>%
  full_join(cow_cluster_3_3N,  by=c('CollarID', 'timestamps')) %>%
  full_join(cow_rMCP_3N_3N, by=c('CollarID', 'timestamps')) %>%
  full_join(cow_cluster_1_3N, by=c('CollarID', 'timestamps')) %>%
  full_join(cow_3hr_3N, by=c('CollarID', 'timestamps')) %>%
  dplyr::mutate(part_status = 0,
                date_time_ex = '2021-05-05 09:20:42',
                date_time_ex= ydm_hms(date_time_ex),
                j_date = yday(timestamps))
#na.omit

##combine 4 hour and 3 hour fix rates for non part cows. Right now ony have 4 hour fixes
cow_all_location_N<-cow_all_location_4N%>% bind_rows(cow_all_location_3N)

head(cow_all_location_N)
names(cow_all_location_N)
table(cow_all_location$part_status)
table(cow_all_location_N$CollarID)
table(part_loc_data$CollarID)


###############merge the nonpart and part cow data##############################
cow_model_data<-cow_all_location_N %>% bind_rows(part_loc_data) %>%
  dplyr::filter(j_date >= 121 & j_date <= 183 & !is.na(j_date))##have model data be May-June

cow_training_data<-part_loc_data %>% bind_rows(cow_all_location_N) %>%
  dplyr::filter(j_date >= 121 & j_date <= 183 & !is.na(j_date))#have training date be april-july

cow_training_data<-cow_training_data %>% mutate(timestamps_pst = with_tz(timestamps, tzone = "America/Los_Angeles"),
                                                time=hour(timestamps_pst)) %>%
  dplyr::mutate(cluster_3 =as.character(cluster_3))

cow_model_data<-cow_model_data %>% mutate(timestamps_pst = with_tz(timestamps, tzone = "America/Los_Angeles"),
                                          time=hour(timestamps_pst)) %>%
  dplyr::mutate(cluster_3 =as.character(cluster_3))
###########################test for cov before scaling############################
cov_mov<-cow_training_data%>%
  dplyr:: select(-c(CollarID, part_status,  timestamps, part_status, cluster_3, j_date,
                    date_time_ex, timestamps_pst, time))

names(cov_mov)

test_cov_mov<-cor(cov_mov, use = 'complete.obs')



table(cow_model_data$CollarID)
table(cow_training_data$CollarID)
##remove 1 week before part date and two weeks after to prevent misclaffication of data
cow_non_part<- unique(cow_all_location_N$CollarID)
cow_training_data_nons<-cow_training_data %>% dplyr::filter(CollarID %in% c('cow_all_location_N'))

cow_training_data_1<-cow_training_data %>%
  dplyr::filter(!CollarID %in% c('cow_all_location_N')) %>%
###keeping only the part date and removing 72 after
  dplyr::mutate(part_remove = case_when(timestamps > (date_time_ex - hours(72)) & timestamps <= date_time_ex ~ "remove", #changed from 168 to 72
                                         (timestamps)< date_time_ex ~"keep",#
                                         timestamps <= date_time_ex + hours(48) ~ "keep",
                                         #timestamps <=date_time_ex+ hours(336) ~ "remove",##changed to keep
                                         timestamps >= date_time_ex + hours(48)~  "keep"
                                        )) %>%
  dplyr::filter(part_remove!='remove') %>%

  bind_rows(cow_training_data_nons) %>%
  # dplyr::select(-c(part_remove)) %>%
  dplyr::filter(CollarID != '42216_2021')

#na.omit()
cow_training_data_test<-cow_training_data_1 %>%
  dplyr::select(c("CollarID"   ,"date_time_ex"   ,"timestamps", part_status,part_remove ))

table(cow_training_data_1$part_status)

names(cow_training_data)

#cow_training_data<- cow_training_data %>%
# fill(everything(), .direction = "down")

#cow_model_data<-cow_model_data %>%
# fill(everything(), .direction = "down")

test<-cow_training_data %>%
  dplyr::filter(part_status==1,
                if_any(everything(), is.na))

table(cow_training_data_1$part_status)
table(cow_model_data$part_status)

table(cow_training_data$CollarID)
table(cow_training_data$part_status)
write_csv(cow_training_data_1, 'outputs/02_model_outputs/traingdate.csv')
##########################################################

cow_training_data_1<-read_csv('outputs/02_model_outputs/traingdate.csv')


rf_data_1<-cow_training_data_1 %>% dplyr::mutate(part_status=as.factor(part_status),
                                                 cluster_3=as.factor(cluster_3)) %>% 
  dplyr::select(-c(CollarID,timestamps,timestamps_pst,time,clus_ID_3,j_date,date_time_ex)) %>% 
  ##this are the variables after checkinf for calculated Pearson’s pairwise correlation to ensure that covariates with |r| > 0.70 
  dplyr::select(c(sl_med_72, para_sd_med_72, area_3, or_sd_sd_72, sl_max_72, fid_3,
                  max_foray_3,  clus_radius_3, abs_mean_72, test.paraSd_4,##last of the selected ones
                  rel_mean_72, rel_sd_72, 
                  #clus_dur_hr_1, test.orthSd_4,
                  euc_72, abs_sd_72,
                  #fid_1, sl_4, abs_ta_4, rel_ta_4, 
                  
                  part_status#,
                  # euc_192,
                  #  sl_sum_192 , sl_max_192 , sl_mean_192 , sl_sd_192 , sl_med_192 ,  rel_sum_192 ,
                  #  rel_max_192 , rel_mean_192 , rel_sd_192 , rel_med_192 , abs_sum_192 , abs_max_192 ,
                  #  abs_mean_192 , abs_sd_192 , abs_med_192 , or_sd_sum_192 , or_sd_max_192 , or_sd_mean_192 ,
                  #  or_sd_sd_192 , or_sd_med_192 , para_sd_sum_192 , para_sd_max_192 , para_sd_mean_192 ,
                  #  para_sd_sd_192 , para_sd_med_192
  )) %>%
  dplyr::select(part_status, everything()) %>% 
  na.omit()

##check for cor
preds <- rf_data_1 %>% 
  dplyr:: select(area_3, sl_med_72, or_sd_sd_72, sl_max_72, test.paraSd_4, rel_mean_72,
         abs_mean_72, abs_sd_72, clus_radius_3, fid_3, max_foray_3,
         para_sd_med_72, rel_sd_72, euc_72)
cor_mat <- cor(preds, method = "pearson", use = "pairwise.complete.obs")







# rf_selection <- rf.modelSel(rf_data_1[,2:15], rf_data_1[,"part_status"],seed=1234
#                             # To ensure reproducible subset selection
# )
# 
# names(rf_data_1)
# plot(rf_selection)
# plot(rf_selection, imp= 'all')
# 
# vars <- rf_selection$selvars
# vars2 <- rf_selection$parameters[[3]]

# rf_data_2<-cow_training_data_1 %>% dplyr::mutate(part_status=as.factor(part_status),
#                                                cluster_3=as.factor(cluster_3)) %>% 
#   dplyr::select(-c(CollarID,timestamps,timestamps_pst,time,clus_ID_3,j_date,date_time_ex)) %>% 
#   
#   dplyr::select(c(all_of(vars), part_status)) %>% 
#   dplyr::select(part_status, everything()) %>% 
#   na.omit()
# 
# table(rf_data_2$part_status)
# 
# cov_mov <- rf_data_1 %>% dplyr::select(c(vars))

names(cov_mov)




# 
# test_cov_mov<-cor(rf_data_1, use = 'complete.obs')


set.seed(42)
##only need if there are NA
 #md1<-rfImpute(part_status~.,data=rf_data_2, iter=6)
 

##no na in model

#d1<-rfImpute(part_status~.,data=rf_data_1 , iter=6)
md1<-randomForest(part_status~., data=rf_data_1, mtree=1000, mtry=2, importance=TRUE, proximity=TRUE, sampsize = c(400,400))
print(md1)

md2<-randomForest(part_status~., data=rf_data_1, mtree=1000, mtry=3, importance=TRUE, proximity=TRUE, sampsize = c(400,400))
print(md2)

md3<-randomForest(part_status~., data=rf_data_1, mtree=1000, mtry=4, importance=TRUE, proximity=TRUE, sampsize = c(400,400))
print(md3)

varImpPlot(md3)

md4<-randomForest(part_status~., data=rf_data_1, mtree=1000, mtry=5, importance=TRUE, proximity=TRUE, sampsize = c(400,400))
print(md4)

md5<-randomForest(part_status~., data=rf_data_1, mtree=1000, mtry=6, importance=TRUE, proximity=TRUE, sampsize = c(400,400))
print(md5)

md5_<-randomForest(part_status~., data=rf_data_1, mtree=1000, mtry=9, importance=TRUE, proximity=TRUE, sampsize = c(400,400))
print(md5_)

##################################################
####filtered data set


md7<-randomForest(part_status~., data=rf_data_2, mtree=1000, mtry=10, importance=TRUE, proximity=TRUE, sampsize = c(300,300))
print(md7)

md8<-randomForest(part_status~., data=rf_data_2, mtree=1000, mtry=7, importance=TRUE, proximity=TRUE, sampsize = c(300,300))
print(md8)



md12<-randomForest(part_status~., data=rf_data_2, mtree=1000, mtry=5, importance=TRUE, proximity=TRUE, sampsize = c(300,300))
print(md12)

md9<-randomForest(part_status~., data=rf_data_2, mtree=1000, mtry=6, importance=TRUE, proximity=TRUE, sampsize = c(300,300))
print(md9)

md10<-randomForest(part_status~., data=rf_data_2, mtree=1000, mtry=4, importance=TRUE, proximity=TRUE, sampsize = c(300,300))
print(md10)

md11<-randomForest(part_status~., data=rf_data_2, mtree=1000, mtry=2, importance=TRUE, proximity=TRUE, sampsize = c(400,400))
print(md11)
varImpPlot(md11)


md13<-randomForest(part_status~., data=rf_data_2, mtree=1000, mtry=3, importance=TRUE, proximity=TRUE, sampsize = c(400,400))
print(md13)


md15<-randomForest(part_status~., data=rf_data_2, mtree=1000, mtry=4, importance=TRUE, proximity=TRUE,sampsize = c(400,400))
print(md15)

md16<-randomForest(part_status~., data=rf_data_2, mtree=1000, mtry=5, importance=TRUE, proximity=TRUE,sampsize = c(400,400))
print(md16)

md17<-randomForest(part_status~., data=rf_data_2, mtree=1000, mtry=6, importance=TRUE, proximity=TRUE, sampsize = c(400,400))
print(md17)


table(rf_data_1$part_status)


names(rf_data_1)
saveRDS(md3, file = 'outputs/02_model_outputs/model_mtry_4_final_this_one_oct092926.RData')


#########################################################################

#model_cow<-read_csv('data/rf_cleaned_up_data_model.csv')
#model_cow<-model_cow %>% 
#dplyr::mutate(part_status=as.factor(part_status),
#cluster_3=as.factor(cluster_3))

#names(cow_model_data)
names(rf_data_1)
unique_ids <- unique(cow_training_data$CollarID)#get unique ids for all cow collars
#unique_ids <- unique(part_loc_test$CollarID)
RF_model_L<-NULL
cow_training_data_L<-NULL
part_loc_test_L<-NULL
model_list<-NULL
model_list_all<-NULL

output_list<-NULL
output_list_all<-NULL

output_list_all_TRUE<-NULL
output_list<-TRUE
unique_ids

# cow_training_data_1<-cow_training_data %>% 
# dplyr::mutate(part_status=as.factor(part_status),
#               cluster_3=as.factor(cluster_3),
#               CollarID=as.factor(CollarID)) %>% 
#   dplyr::select(-c(timestamps,timestamps_pst,time,clus_ID_3,j_date,date_time_ex)) %>% 
#   
#   dplyr::select(c(all_of(vars), part_status, CollarID)) %>% 
#   dplyr::select(part_status, everything())
# md1<-rfImpute(part_status~.,data=cow_training_data_1, iter=6)

test_cov_mov<-cor(cov_mov, use = 'complete.obs')


##convert amt package for each unique ID
for (id in unique_ids) {
  set.seed(42)
  

  
  
  # part_loc_data_L<-cow_training_data_1 %>% 
  #    dplyr::filter(CollarID != id) %>% 
  #   dplyr::mutate(part_status=as.factor(part_status),
  #                 cluster_3=as.factor(cluster_3)) %>%
  #   dplyr::select(-c(CollarID,timestamps,timestamps_pst,time,clus_ID_3,j_date,date_time_ex)) %>%
  # 
  #   dplyr::select(c(all_of(vars), 
  #     part_status))%>%
  # 
  #   dplyr::select(part_status, everything()) %>% 
  #   na.omit()

  part_loc_data_L<-cow_training_data_1 %>% dplyr::mutate(part_status=as.factor(part_status),
                                          cluster_3=as.factor(cluster_3)) %>% 
    dplyr::select(-c(CollarID,timestamps,timestamps_pst,time,clus_ID_3,j_date,date_time_ex)) %>% 
    
    dplyr::select(c(sl_med_72, para_sd_med_72, area_3, or_sd_sd_72, sl_max_72, fid_3,
                    max_foray_3,  clus_radius_3, abs_mean_72, test.paraSd_4,##last of the selected ones
                    rel_mean_72, rel_sd_72, 
                    #clus_dur_hr_1, test.orthSd_4,
                    euc_72, abs_sd_72,
                    #fid_1, sl_4, abs_ta_4, rel_ta_4, 
                    
                    part_status#,
                    # euc_192,
                    #  sl_sum_192 , sl_max_192 , sl_mean_192 , sl_sd_192 , sl_med_192 ,  rel_sum_192 ,
                    #  rel_max_192 , rel_mean_192 , rel_sd_192 , rel_med_192 , abs_sum_192 , abs_max_192 ,
                    #  abs_mean_192 , abs_sd_192 , abs_med_192 , or_sd_sum_192 , or_sd_max_192 , or_sd_mean_192 ,
                    #  or_sd_sd_192 , or_sd_med_192 , para_sd_sum_192 , para_sd_max_192 , para_sd_mean_192 ,
                    #  para_sd_sd_192 , para_sd_med_192
    )) %>%
    dplyr::select(part_status, everything()) %>% 
    na.omit()


  
  ##changing the response variable to  to binary
  part_loc_test_L<-cow_model_data %>% dplyr::filter(CollarID == id) %>% 
    dplyr::select(-c(part_status))
  
  RF_model_L<-randomForest(part_status~., part_loc_data_L, mtree=1000, mtry=4, importance=TRUE, proximity=TRUE, sampsize = c(380,380)) ##change back to 2
  
  
  testL<-predict(RF_model_L, newdata=part_loc_test_L,type = 'prob')
  #test2<-as.data.frame(predict(RF_model_L, newdata=part_loc_test_L,type="response"))
  
 # testL<-as.data.frame(testL) %>% dplyr::rename(prob = 2)
  head(testL)
  testL_date<-testL %>% dplyr::bind_cols(part_loc_test_L)
  testL_date$date_time_ex <- as.POSIXct(testL_date$date_time_ex, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
  
  plot2 <- ggplot(testL_date, aes(x = timestamps)) +
    geom_line(aes(y = `1`), color = "black", linewidth = 1) +
    #geom_line(aes(y = `2`), color = "blue", linewidth = 1) +
    #geom_line(aes(y = `3`), color = "red", linewidth = 1) +
    #geom_line(aes(y = `4`), color = "green", linewidth = 1) +
    geom_vline(aes(xintercept = date_time_ex[1]), 
               color = "sienna3", linetype = "dashed", linewidth = 2) + ylim(0,1)+
    geom_hline(yintercept = 0.65, color = "red", linetype = "dashed", linewidth = 1) +  # Added horizontal line
    ggtitle(paste("Predict Part Model md6", testL_date$CollarID[1], 
                  "date from VIT", format(testL_date$date_time_ex[1], "%Y-%m-%d %H:%M:%S"))) +
    theme_minimal() +
    labs(x = "Timestamps", y = "testL Value")
  
  print(plot2)
  
  output_list <- testL_date %>%
    dplyr::select(`1`, CollarID, timestamps, date_time_ex) %>%
    dplyr::mutate(
      time_dif = difftime(date_time_ex, timestamps, units = 'days'),
      threshold = 0.65,
      success = `1` > threshold
    ) %>%
    mutate(
      # Rolling check for 3 consecutive TRUEs
      rolling_all_true = zoo::rollapply(
        success,
        width = 4,
        FUN = all,
        align = "left",
        partial = FALSE,
        fill = FALSE
      ),
      # Initialize flag column
      run_flag = FALSE
    ) %>%
    # Mark all 3 rows in the run
    mutate(
      run_flag = replace(
        run_flag,
        which(rolling_all_true),
        TRUE
      ),
      run_flag = replace(
        run_flag,
        which(rolling_all_true) + 1,
        TRUE
      ),
      run_flag = replace(
        run_flag,
        which(rolling_all_true) + 2,
        TRUE
      ),
      run_flag = replace(
        run_flag,
        which(rolling_all_true) + 3,
        TRUE
      )
  #
  #
  #
  #   #               #%>%
  #   #               consecutive2 = success & lag(success, n=3, default = FALSE)) %>%
  #    # mutate(any_true_3 = rollapply(
  #    #   consecutive2,
  #    #   width = 3,
  #    #   FUN = any,          # checks if any element is TRUE
  #    #   align = "left",
  #    #   partial = TRUE,
  #    #   fill = NA
  #    # )
     ) %>%
    dplyr::filter(run_flag == TRUE) %>% 
    dplyr::arrange(desc(`1`)) %>%  # Sort by highest probability
    dplyr::slice(1) 
    
  output_list_all<-rbind(output_list_all, output_list)




  #
  # output_list_1<- testL_date %>%
  #   dplyr::select(`1`, CollarID, timestamps, date_time_ex) %>%
  #   dplyr::mutate(
  #     time_dif = difftime(date_time_ex, timestamps, units = 'days'),
  #     threshold = 0.50,
  #     success = `1` > threshold
  #   ) %>%
  #   mutate(
  #     # Rolling check for 3 consecutive TRUEs
  #     rolling_all_true = zoo::rollapply(
  #       success,
  #       width = 2,
  #       FUN = all,
  #       align = "left",
  #       partial = FALSE,
  #       fill = FALSE
  #     ),
  #     # Initialize flag column
  #     run_flag = FALSE
  #   ) %>%
  #   # Mark all 3 rows in the run
  #   mutate(
  #     run_flag = replace(
  #       run_flag,
  #       which(rolling_all_true),
  #       TRUE
  #     ),
  #     run_flag = replace(
  #       run_flag,
  #       which(rolling_all_true) + 1,
  #       TRUE
  #     )
  #   ) #%>% 
  #   
  #   output_list_2<- testL_date %>%
  #   dplyr::select(`2`, CollarID, timestamps, date_time_ex) %>%
  #   dplyr::mutate(
  #     time_dif_2 = difftime(date_time_ex, timestamps, units = 'days'),
  #     threshold = 0.50,
  #     success_2 = `2` > threshold
  #   ) %>%
  #   mutate(
  #     # Rolling check for 3 consecutive TRUEs
  #     rolling_all_true_2 = zoo::rollapply(
  #       success_2,
  #       width = 2,
  #       FUN = all,
  #       align = "left",
  #       partial = FALSE,
  #       fill = FALSE
  #     ),
  #     # Initialize flag column
  #     run_flag_2 = FALSE
  #   ) %>%
  #   # Mark all 3 rows in the run
  #   mutate(
  #     run_flag_2 = replace(
  #       run_flag_2,
  #       which(rolling_all_true_2),
  #       TRUE
  #     )
   
  #) #%>% 
    
# output_list_join<-output_list_2 %>% 
#   full_join(output_list_1, by =c("timestamps","CollarID", "date_time_ex", "threshold"  ))
# head(output_list_join)
#     
# 
#   
#   
#   output_list_join_2 <-output_list_join %>%
#     group_by(CollarID) %>%  # Group by collar if checking within same collar
#     mutate(
#       both_flags_within_14days = map_lgl(row_number(), function(i) {
#         current_time <- timestamps[i]
#         
#         # Find rows where run_flag_2 is TRUE
#         flag2_times <- timestamps[run_flag_2 == TRUE]
#         # Find rows where run_flag is TRUE
#         flag1_times <- timestamps[run_flag == TRUE]
#         
#         # Check if current row has either flag TRUE and the other flag is TRUE within 14 days
#         if (run_flag_2[i]) {
#           any(abs(as.numeric(difftime(flag1_times, current_time, units = "days"))) <= 7)
#         } else if (run_flag[i]) {
#           any(abs(as.numeric(difftime(flag2_times, current_time, units = "days"))) <= 7)
#         } else {
#           FALSE
#         }
#       })
#     ) %>%
#     ungroup() %>% 
#     filter( both_flags_within_14days == "TRUE") %>% 
#     dplyr::arrange(desc(`1`)) %>%  # Sort by highest probability
#     dplyr::slice(1)
#   output_list_all_TRUE<-rbind(output_list_all_TRUE,  output_list_join_2)

}


    check_trues<- setdiff((unique(cow_training_data$CollarID)), output_list_all$CollarID)
print(check_trues)  
   write_csv(output_list_all, "outputs/03_RF_outputs/outputs_mtry4_all_data_92926.csv")

output_list_all$time_dif<-as.numeric(output_list_all $time_dif)

non_part<-c("42771_2023", "46436_2023", "46439_2021", "84543_2023",
            "84544_2022", "42708_2021", "42732_2021",
            "42739_2022", "42748_2021", "42752_2021", "42752_2022",
            "42772_2022", "42773_2021", "42775_2022", "42794_2021")

known_part<-output_list_all %>% 
  dplyr::filter(! CollarID %in% non_part)
mean(abs(known_part$time_dif))
median(abs(known_part$time_dif))

known_non_part<-output_list_all %>% 
  dplyr::filter(CollarID %in% non_part)


model_list_all<-output_list_all %>% 
  rbind(model_list_all, model_list)
names(model_cow)
output<- output_list_all %>% 
  dplyr::filter(success == "TRUE") %>% 
  slice_max(prob, n=1)
