# Download & Install Nimble here: https://r-nimble.org/download
# Make sure you have XCode installed on Mac (https://r-nimble.org/html_manual/cha-installing-nimble.html)
# or RTools on Windows: https://cran.r-project.org/bin/windows/Rtools/

#Make sure all of the below packages are installed:
##packages from course

rm(list=ls())#
library(nimble)
library(tidyverse)
library(mcmcplots)
library(MCMCvis)
library(ggplot2)
library(palmerpenguins)
library(coda)
library(data.table)
source('attach.nimble_v2.R')
library(dplyr)
library(tidybayes)
library(loo)
library(boot)
library(patchwork)
library(knitr)
library(kableExtra)
library(beepr)

###read in dataset

###read in dataset

#cow_capture_all_2<- read_csv("outputs/cleaned_up_data_set/20251104_cap_cows_all_time_2_model_03_04_times_prep.csv")
cow_capture_all_1<- read_csv("outputs/cleaned_up_data_set/20261013_cap_cows_all_time_2_model_03_04_times_prep.csv")

test <- cow_capture_all_1 %>%
  dplyr::filter(CollarID == "46438_2021")
                        
test_2<-cow_capture_all_1 %>% 
  select(c(CollarID,time_2_preg, time_1_preg, j_expelled ))
test_3<-cow_capture_all_1 %>% 
  select(c(CollarID,time_2_preg ))


mean(test$est_bm, na.rm = TRUE)
range(test$est_bm, na.rm = TRUE)
sd(test$est_bm, na.rm = TRUE)


merge_all<-test_2 %>% 
  full_join(test_3, by = "CollarID", suffix = c("_old", "_new")) %>% 

mutate(preg_mismatch = coalesce(time_2_preg_old != time_2_preg_new, FALSE) |
         xor(is.na(time_2_preg_old), is.na(time_2_preg_new)))
  

summary(test)
ggplot(cow_capture_all_1, aes(x= j_expelled, y =age_estimate_2))+
  geom_point()
sd(test$est_bm, na.rm = TRUE)

summary(test)
# cow_capture_all<-cow_capture_all_1 %>% 
#   #filter(!is.na(sex)) %>% #keeps only cows that had a calf
#   #filter(!(is.na(sex) & time_1_preg == 1)) %>% ##keeps cows that did not have a calf in time-1 
#   #but removes cows where we didnt get calf
#   #mutate(trunk_days = ifelse(is.na(trunk_days), 0, trunk_days)) %>%  #makes days calf alive 0 for those cows
#   select(-c(`VIT Placed`)) %>% 
#   mutate(time_1_preg = as.factor(time_1_preg),
#          time_2_preg = as.factor(time_2_preg),
#          capture_year = as.factor(capture_year),
#          age_estimate_2 = age_estimate_2 +1,
#          caught_calf=as.factor(dplyr::if_else(mass_days_bag > 1, 1L, 0L, missing = 0L)),
#          sex_correct  = ifelse(time_1_preg  == 1 & caught_calf == 1, sex, 0),
#          
#          across(
#            .cols = c(where(is.numeric)#,
#                      #-age_estimate_2
#            ),  # optional: if you want to keep raw years unscaled
#            .fns = ~ as.numeric(scale(.))
#          ),
#          lac_time = ifelse(time_1_preg == 1 & caught_calf == 1, trunk_days, 0),
#          mass_calf = ifelse(time_1_preg == 1 & caught_calf == 1, mass_days_bag,0),
#          birt_date = ifelse(time_1_preg == 1 #& caught_calf == 1
#                             , j_expelled, 0 ),
#          calf_ind = as.numeric(as.character(time_1_preg) == "1" &
#                                  as.character(caught_calf) == "1")) #%>% 
  #filter(!is.na(sex))  ##remove any where calf was not captured
# Standardize x among a subset of cows (keep == TRUE); everyone else gets 0
z_within <- function(x, keep) {
  m <- mean(x[keep], na.rm = TRUE)
  s <- sd(x[keep],   na.rm = TRUE)
  if_else(keep, (x - m) / s, 0)
}
cow_capture_all <- cow_capture_all_1 %>% 
  select(-c(`VIT Placed`)) %>% 
  mutate(time_1_preg    = as.factor(time_1_preg),
         time_2_preg    = as.factor(time_2_preg),
         capture_year   = as.factor(capture_year),
         age_estimate_2 = age_estimate_2 + 1,
         caught_calf    = as.factor(dplyr::if_else(mass_days_bag > 1, 1L, 0L, missing = 0L)),
         
         # 1 = pregnant last year AND calf caught
         calf_ind = as.numeric(time_1_preg == 1 & caught_calf == 1),
         # NEW: 1 = has a VIT expelled date (calf caught or not)
         vit_ind  = as.numeric(!is.na(j_expelled)),
         
         sex_correct = ifelse(calf_ind == 1, as.character(sex), "0"),
         
         # Calf covariates: standardized among caught-calf cows only
         lac_time  = z_within(trunk_days,    calf_ind == 1),
         mass_calf = z_within(mass_days_bag, calf_ind == 1),
         # NEW: calving date standardized among ALL cows with a VIT expelled date
         birt_date = z_within(j_expelled,    vit_ind == 1),
         
         across(
           .cols = c(where(is.numeric), -lac_time, -mass_calf, -birt_date,
                     -calf_ind, -vit_ind),   # NEW: -vit_ind
           .fns  = ~ as.numeric(scale(.))
         )) 

##this does remove miscarriage
##this also removed cow elk that did not have a calf time-1###NEED TO CHANGE
  
names(cow_capture_all) <- names(cow_capture_all) %>%
  gsub("[ ,()/]", "_", .) %>%       # Replace space, comma, parentheses with underscore
  gsub("_+", "_", .) %>%           # Collapse multiple underscores into one
  gsub("_$", "", .)   
str(cow_capture_all)
names(cow_capture_all)
cow_capture_all$caught_calf

cow_capture_all_2<-cow_capture_all_1 %>% 
  select(c("est_bm", "age_estimate_2"))
cor_matrix <- cor(cow_capture_all_2, use = "pairwise.complete.obs")
plot(cow_capture_all_2)
set.seed(123)
# problem_rows <- which(
#   is.na(cow_capture_all$lac_score) |
#     is.na(cow_capture_all$est_bm) |
#     is.na(cow_capture_all$area_DDE_gt_mean) |
#     is.na(cow_capture_all$IFBF) |
#     is.na(cow_capture_all$age_estimate_2) |
#     is.na(cow_capture_all$total_year_prior) |
#     is.na(cow_capture_all$Preg_lab)
# )


# 
# model_preg_lac <-  nimbleCode({
#   
#   #Priors
#   Intercept  ~ dnorm(0, sd = 5)
# 
# 
#    time_1_preg_effect ~ dt(mu=0, sigma=1, df=1)
# 
#   
#   for(i in 1:n.obs){
#     
#     #Process Model
#     logit(preg_prob[i])<-Intercept+
# 
#       (time_1_preg[i]* time_1_preg_effect)#+
# 
#     
#     
#     
#     
#     
#     #Observation Model (Likelihood)
#     preg_status[i] ~ dbern(preg_prob[i])
#     
#     #Impute missing data values
# 
#   
#     
#   }#i
#    
# })#model5
# parameters <- c(
#                 "Intercept", 
#                 "time_1_preg_effect",
#                 "logProb_preg_status")
# 
# 
# # MCMC Settings
# ni <- 400000
# nt <- 400
# nb <- 200000
# nc <- 3
# 
# nimble.data = list(
#   preg_status = as.numeric(cow_capture_all$time_2_preg) - 1,
# 
#   time_1_preg= as.numeric(cow_capture_all$time_1_preg)-1#
# 
#  
# )
# 
# nimble.constants = list(n.obs = nrow(cow_capture_all))
# 
# 
# mcmc.output_preg_lac <- nimbleMCMC(code = model_preg_lac,
#                             data = nimble.data,
#                             constants=nimble.constants,
#                             monitors = parameters,
#                             niter = ni,
#                             nburnin = nb,
#                             nchains = nc,
#                             thin=nt,
#                             summary=TRUE,
#                             samplesAsCodaMCMC = TRUE)
# 
# attach.nimble(mcmc.output_preg_lac$samples)
# 
# 
# mata_pl  <- do.call(rbind, lapply(mcmc.output_preg_lac$samples, as.matrix))
# lpa_pl  <- grep("^logProb_preg_status\\[", colnames(mata_pl), value = TRUE)
# lla_pl   <- mata_pl[, lpa_pl, drop = FALSE]
# stopifnot(all(is.finite(lla_pl) | (is.infinite(lla_pl) & lla_pl < 0)))
# loo_pl <- loo(lla_pl)
# 
# 
# 
# attach.nimble(mcmc.output_preg_lac$samples)
# 
# MCMCtrace(object = mcmc.output_preg_lac$samples,
#           pdf = FALSE, # no export to PDF
#           ind = TRUE, # separate density lines per chain
#           params = "time_1_preg_effect")
# # Gelman-Rubin diagnostic (AKA RHat or PSRF)
# gelman.diag(mcmc.output_preg_lac$samples, multivariate = FALSE)
# 
# 
# 
# mcmcplot(mcmc.output_preg_lac$samples)
# # 
# 
# 
# post_mat <- as.matrix(mcmc.output_preg_lac$samples)
# # 2. Specify the parameters you care about
# params <- c(
#             "Intercept", 
#             "time_1_preg_effect"
# 
#             )
# 
# # 3. Summarize each one
# summary_df_bc_1 <- tibble(parameter = params) %>%
#   rowwise() %>%
#   mutate(
#     median   = median(post_mat[, parameter]),
#     lower95  = quantile(post_mat[, parameter], 0.025),
#     upper95  = quantile(post_mat[, parameter], 0.975),
#     # posterior probability effect > 0
#     p_gt_zero = mean(post_mat[, parameter] > 0),
#     # if you expect a negative effect, you can also compute P(<0):
#     p_lt_zero = mean(post_mat[, parameter] < 0)
#   ) %>%
#   ungroup()
# 
# print(summary_df_bc_1)
# get_variables(mcmc.output_preg_lac$samples)
# 
# summary_df_bc_1 %>%
#   kbl(digits = 3, caption = "Posterior Summary Statistics for Model bc_2") %>%
#   kable_styling(full_width = FALSE, position = "center")
# 
# # Full posterior distributions:
# 
# 
# 
# mod_1_fig<-mcmc.output_preg_lac$samples%>%
#   spread_draws(
#                time_1_preg_effect
# ) %>%
#   pivot_longer(-c(.chain, .iteration, .draw),
#                names_to = 'Parameter',
#                values_to = 'Estimate') %>%
#   ggplot(aes(y = Parameter, x = Estimate, fill = Parameter)) +
#   stat_halfeye(show.legend = FALSE) +
#   geom_vline(xintercept = 0, linetype = 2, color = 'black') +
#   scale_fill_manual(values = c(
#     precip_effect = "grey60",  # make precip_effect blue
#     lac_effect = "red",
#     dde_effect_95 = "blue3",
#     bod_con_effect = "red4",
#     bm_effect = "blue",
#     age_effect = "blue4"
#   )) +
#   theme_minimal()
# mod_1_fig
# 
# #################Model 2 lac effect########################################
# ###decrease to just lac effect
# 
# model_lac_time_only <-  nimbleCode({
#   
#   #Priors
#   Intercept  ~ dnorm(0, sd = 5)
# 
#   
#   lac_time_effect ~dt(mu=0, sigma=1, df=1)
# 
#   
#   for(i in 1:n.obs){
#     
#     #Process Model
#     logit(preg_prob[i])<-Intercept+
# 
#       
# 
#       (lac_time[i]* lac_time_effect)
#     
#     
#     #Observation Model (Likelihood)
#     preg_status[i] ~ dbern(preg_prob[i])
#     
# 
#     
#     
#   }#i
#   
# })#model5
# parameters <- c(
#   "Intercept", 
# 
#   "lac_time_effect",
# 
#   "logProb_preg_status"
# 
# )
# 
# # MCMC Settings
# ni <- 400000
# nt <- 400
# nb <- 200000
# nc <- 3
# 
# nimble.data = list(
#   preg_status = as.numeric(cow_capture_all$time_2_preg) - 1,
# 
#   sex_correct = ifelse(cow_capture_all$sex_correct== "m", 1,0),
#   lac_time= as.numeric(cow_capture_all$lac_time), 
#   time_1_preg= as.numeric(cow_capture_all$time_1_preg)-1#,#
# 
#   
# )
# 
# nimble.constants = list(n.obs = nrow(cow_capture_all))
# 
# 
# mcmc.output_calf_only <- nimbleMCMC(code = model_lac_time_only,
#                                    data = nimble.data,
#                                    constants=nimble.constants,
#                                    monitors = parameters,
#                                    niter = ni,
#                                    nburnin = nb,
#                                    nchains = nc,
#                                    thin=nt,
#                                    summary=TRUE,
#                                    samplesAsCodaMCMC = TRUE)
# 
# attach.nimble(mcmc.output_calf_only$samples)
# 
# 
# mata_calf  <- do.call(rbind, lapply(mcmc.output_calf_only$samples, as.matrix))
# lpa_calf  <- grep("^logProb_preg_status\\[", colnames(mata_calf), value = TRUE)
# lla_calf   <- mata_calf[, lpa_calf, drop = FALSE]
# stopifnot(all(is.finite(lla_calf) | (is.infinite(lla_calf) & lla_calf < 0)))
# loo_calf <- loo(lla_calf)
# 
# 
# 
# attach.nimble(mcmc.output_calf_only$samples)
# 
# MCMCtrace(object = mcmc.output_calf_only$samples,
#           pdf = FALSE, # no export to PDF
#           ind = TRUE, # separate density lines per chain
#           params = "lac_time_effect")
# # Gelman-Rubin diagnostic (AKA RHat or PSRF)
# gelman.diag(mcmc.output_calf_only$samples, multivariate = FALSE)
# 
# mcmcplot(mcmc.output_calf_only$samples)
# 
# 
# ##how to get effects size
# ##jow to make the differnt charts in PP
# ##how to get time on deck chart
# ##uses zscore to preduct value. Use mean for all the other values
# ##will use for calf age ##what the fuck is a z-score?
# post_mat <- as.matrix(mcmc.output_calf_only$samples)
# # 2. Specify the parameters you care about
# params <- c(
#   "Intercept", 
#   "lac_time_effect")
# 
# 
# # 3. Summarize each one
# summary_df_calf_1 <- tibble(parameter = params) %>%
#   rowwise() %>%
#   mutate(
#     median   = median(post_mat[, parameter]),
#     lower95  = quantile(post_mat[, parameter], 0.025),
#     upper95  = quantile(post_mat[, parameter], 0.975),
#     # posterior probability effect > 0
#     p_gt_zero = mean(post_mat[, parameter] > 0),
#     # if you expect a negative effect, you can also compute P(<0):
#     p_lt_zero = mean(post_mat[, parameter] < 0)
#   ) %>%
#   ungroup()
# 
# print(summary_df_calf_1)
# get_variables(mcmc.output_calf_only$samples)
# 
# 
# summary_df_calf_1  %>%
#   kbl(digits = 3, caption = "Posterior Summary Statistics for Model bc_2") %>%
#   kable_styling(full_width = FALSE, position = "center")
# 
# # Full posterior distributions:
# 
# 
# 
# mod_2_fig<-mcmc.output_calf_only$samples%>%
#   spread_draws(
#     lac_time_effect) %>%
#   pivot_longer(-c(.chain, .iteration, .draw),
#                names_to = 'Parameter',
#                values_to = 'Estimate') %>%
#   ggplot(aes(y = Parameter, x = Estimate, fill = Parameter)) +
#   stat_halfeye(show.legend = FALSE) +
#   geom_vline(xintercept = 0, linetype = 2, color = 'black') +
#   scale_fill_manual(values = c(
#     #lac_time_effect = "grey60",  # make precip_effect blue
#     sex_effect = "red",
#     time_1_preg_effect = "grey60",
#     bod_con_effect = "red4",
#     lac_time_effect = "grey60",
#     age_effect = "blue4"
#   )) +
#   theme_minimal()
# mod_2_fig
# 
# ###########################Model 3##############################################
# ##DDE effect
# 
# model_DDE <-  nimbleCode({
#   
#   #Priors
#   Intercept  ~ dnorm(0, sd = 5)
# 
#   dde_effect_m ~ dt(mu=0, sigma=1, df=1) 
# 
# 
# 
#   
#   for(i in 1:n.obs){
#     
#     #Process Model
#     logit(preg_prob[i])<-Intercept+
# 
#       (dde_95[i]*dde_effect_m)
# 
#     # (lac_score[i]*time_1_preg[i]*lac_score_preg_effect)+
#     
#     
#     
#     
#     
#     
#     #Observation Model (Likelihood)
#     preg_status[i] ~ dbern(preg_prob[i])
#     
#     #Impute missing data values
# 
#     #dde_95[i] ~ dnorm(mean=0,sd=1)
# 
#     
#     
#   }#i
#   
# })#model5
# parameters <- c(
#   "dde_effect_m",
#   "Intercept", 
#   "logProb_preg_status"
# 
# )
# 
# # MCMC Settings
# ni <- 400000
# nt <- 400
# nb <- 200000
# nc <- 3
# 
# nimble.data = list(
#   preg_status = as.numeric(cow_capture_all$time_2_preg) - 1,
#   dde_95 = as.numeric((cow_capture_all$area_DDE_gt_95))
# 
#   
# )
# 
# nimble.constants = list(n.obs = nrow(cow_capture_all))
# 
# 
# mcmc.output_dde <- nimbleMCMC(code = model_DDE,
#                                    data = nimble.data,
#                                    constants=nimble.constants,
#                                    monitors = parameters,
#                                    niter = ni,
#                                    nburnin = nb,
#                                    nchains = nc,
#                                    thin=nt,
#                                    summary=TRUE,
#                                    samplesAsCodaMCMC = TRUE)
# 
# attach.nimble(mcmc.output_dde$samples)
# beep(sound = 2)
# 
# mata_all_dde  <- do.call(rbind, lapply(mcmc.output_dde$samples, as.matrix))
# lpa_all_dde  <- grep("^logProb_preg_status\\[", colnames(mata_all_dde), value = TRUE)
# lla_all_dde   <- mata_all_dde[, lpa_all_dde, drop = FALSE]
# stopifnot(all(is.finite(lla_all_dde) | (is.infinite(lla_all_dde) & lla_all_dde < 0)))
# loo_all_dde <- loo(lla_all_dde)
# 
# 
# 
# attach.nimble(mcmc.output_dde$samples)
# 
# MCMCtrace(object = mcmc.output_dde$samples,
#           pdf = FALSE, # no export to PDF
#           ind = TRUE, # separate density lines per chain
#           params = "dde_effect_m")
# # Gelman-Rubin diagnostic (AKA RHat or PSRF)
# gelman.diag(mcmc.output_dde$samples, multivariate = FALSE)
# 
# 
# mcmcplot(mcmc.output_dde$samples)
# 
# 
# ##how to get effects size
# ##jow to make the differnt charts in PP
# ##how to get time on deck chart
# ##uses zscore to preduct value. Use mean for all the other values
# ##will use for calf age ##what the fuck is a z-score?
# post_mat <- as.matrix(mcmc.output_dde$samples)
# # 2. Specify the parameters you care about
# params <- c("Intercept",
#   "dde_effect_m"
# 
# )
# 
# # 3. Summarize each one
# summary_df_dde <- tibble(parameter = params) %>%
#   rowwise() %>%
#   mutate(
#     median   = median(post_mat[, parameter]),
#     lower95  = quantile(post_mat[, parameter], 0.025),
#     upper95  = quantile(post_mat[, parameter], 0.975),
#     # posterior probability effect > 0
#     p_gt_zero = mean(post_mat[, parameter] > 0),
#     # if you expect a negative effect, you can also compute P(<0):
#     p_lt_zero = mean(post_mat[, parameter] < 0)
#   ) %>%
#   ungroup()
# 
# print(summary_df_dde)
# get_variables(mcmc.output_dde$samples)
# 
# summary_df_bc_1 %>%
#   kbl(digits = 3, caption = "Posterior Summary Statistics for Model bc_2") %>%
#   kable_styling(full_width = FALSE, position = "center")
# 
# # Full posterior distributions:
# 
# 
# 
# mod_3_fig<-mcmc.output_dde$samples%>%
#   spread_draws(
#     dde_effect_m,
# ) %>%
#   pivot_longer(-c(.chain, .iteration, .draw),
#                names_to = 'Parameter',
#                values_to = 'Estimate') %>%
#   ggplot(aes(y = Parameter, x = Estimate, fill = Parameter)) +
#   stat_halfeye(show.legend = FALSE) +
#   geom_vline(xintercept = 0, linetype = 2, color = 'black') +
#   scale_fill_manual(values = c(
#     dde_effect_m = "grey60"  # make precip_effect blue
# 
#   )) +
#   theme_minimal()
# mod_3_fig
# 
# 
# ###############################model SPEI 12
# ##
# 
# model_all_spei <-  nimbleCode({
#   
#   #Priors
#   Intercept  ~ dnorm(0, sd = 5)
# 
# 
#   spei_12_effect ~ dt(mu = 0 , sigma = 1, df = 1)
# 
# 
#   
#   for(i in 1:n.obs){
#     
#     #Process Model
#     logit(preg_prob[i])<-Intercept+
#      
#       (spei_12[i]*spei_12_effect)#+
#    
#     
#     
#     
#     
#     
#     #Observation Model (Likelihood)
#     preg_status[i] ~ dbern(preg_prob[i])
#     
#     #Impute missing data values
# 
#     spei_12[i] ~ dnorm(mean = 0, sd = 1)
#     
#     
#     
#   }#i
#   
# })#model5
# parameters <- c(
#   "Intercept", 
# 
#   "logProb_preg_status",#
# 
#   "spei_12_effect"#,
# 
# )
# 
# # MCMC Settings
# ni <- 400000
# nt <- 400
# nb <- 200000
# nc <- 3
# 
# nimble.data = list(
#   preg_status = as.numeric(cow_capture_all$time_2_preg) - 1,
# 
#   spei_12 = as.numeric((cow_capture_all$sept_year_prior_12M))
#   
# )
# 
# nimble.constants = list(n.obs = nrow(cow_capture_all))
# 
# 
# mcmc.output_spei <- nimbleMCMC(code = model_all_spei,
#                                        data = nimble.data,
#                                        constants=nimble.constants,
#                                        monitors = parameters,
#                                        niter = ni,
#                                        nburnin = nb,
#                                        nchains = nc,
#                                        thin=nt,
#                                        summary=TRUE,
#                                        samplesAsCodaMCMC = TRUE)
# 
# attach.nimble(mcmc.output_spei$samples)
# beep(sound =3)
# 
# mata_all_spei  <- do.call(rbind, lapply(mcmc.output_spei$samples, as.matrix))
# lpa_all_spei  <- grep("^logProb_preg_status\\[", colnames(mata_all_spei), value = TRUE)
# lla_all_spei   <- mata_all_spei[, lpa_all_spei, drop = FALSE]
# stopifnot(all(is.finite(lla_all_spei) | (is.infinite(lla_all_spei) & lla_all_spei < 0)))
# loo_all_spei <- loo(lla_all_spei)
# 
# 
# 
# attach.nimble(mcmc.output_spei$samples)
# 
# MCMCtrace(object = mcmc.output_spei$samples,
#           pdf = FALSE, # no export to PDF
#           ind = TRUE, # separate density lines per chain
#           params = "spei_12_effect")
# # Gelman-Rubin diagnostic (AKA RHat or PSRF)
# gelman.diag(mcmc.output_spei$samples, multivariate = FALSE)
# 
# mcmcplot(mcmc.output_spei$samples)
# 
# 
# ##how to get effects size
# ##jow to make the differnt charts in PP
# ##how to get time on deck chart
# ##uses zscore to preduct value. Use mean for all the other values
# ##will use for calf age ##what the fuck is a z-score?
# post_mat <- as.matrix(mcmc.output_spei$samples)
# # 2. Specify the parameters you care about
# params <- c(
# 
#   "spei_12_effect"#,
#   
# )
# 
# # 3. Summarize each one
# summary_df_spei <- tibble(parameter = params) %>%
#   rowwise() %>%
#   mutate(
#     median   = median(post_mat[, parameter]),
#     lower95  = quantile(post_mat[, parameter], 0.025),
#     upper95  = quantile(post_mat[, parameter], 0.975),
#     # posterior probability effect > 0
#     p_gt_zero = mean(post_mat[, parameter] > 0),
#     # if you expect a negative effect, you can also compute P(<0):
#     p_lt_zero = mean(post_mat[, parameter] < 0)
#   ) %>%
#   ungroup()
# 
# print(summary_df_spei)
# get_variables(mcmc.output_spei$samples)
# 
# summary_df_bc_1 %>%
#   kbl(digits = 3, caption = "Posterior Summary Statistics for Model bc_2") %>%
#   kable_styling(full_width = FALSE, position = "center")
# 
# # Full posterior distributions:
# 
# 
# 
# mod_4_fig<-mcmc.output_spei$samples%>%
#   spread_draws( 
#     spei_12_effect) %>%
#   pivot_longer(-c(.chain, .iteration, .draw),
#                names_to = 'Parameter',
#                values_to = 'Estimate') %>%
#   ggplot(aes(y = Parameter, x = Estimate, fill = Parameter)) +
#   stat_halfeye(show.legend = FALSE) +
#   geom_vline(xintercept = 0, linetype = 2, color = 'black') +
#   scale_fill_manual(values = c(
#     precip_effect = "grey60",  # make precip_effect blue
#     lac_effect = "red",
#     dde_effect_95 = "blue3",
#     bod_con_effect = "red4",
#     bm_effect = "blue",
#     age_effect = "blue4"
#   )) +
#   theme_minimal()
# mod_4_fig
# 
# ##################end model 5
# #################Model 2 95 DDE########################################
# ###full model DDE, SPEI, time_1_preg, calf_sex, lac time, age, BM, lac_score
# 
# model_full <-  nimbleCode({
#   
#   #Priors
#   Intercept  ~ dnorm(0, sd = 5)
#   
#   lac_effect~ dt(mu=0, sigma=1, df=1)
#    bm_effect ~ dt(mu=0, sigma=1, df=1)
#    dde_effect_m ~ dt(mu=0, sigma=1, df=1) 
#    age_effect ~ dt(mu=0, sigma=1, df=1)
#   
#   lac_time_effect ~dt(mu=0, sigma=1, df=1)
#   sex_effect ~ dt(mu=0, sigma=1, df=1)
#   
#   time_1_preg_effect ~ dt(mu=0, sigma=1, df=1)
#   # spei_3_effect ~ dt(mu = 0 , sigma = 1, df = 1)
#   #spei_6_effect ~ dt(mu = 0 , sigma = 1, df = 1)
#   spei_12_effect ~ dt(mu = 0 , sigma = 1, df = 1)
#   # age2_effect ~ dt(mu = 0, sigma = 1, df = 1)
#   # lac_score_preg_effect ~ dt(mu = 0, sigma = 1, df = 1)
#    #sex_lac_effect ~ dt(mu = 0, sigma = 1, df = 1)
#   
#   #lac_effect~ dnorm(0, sd = 5)
#   #bm_effect ~ dnorm(0, sd = 5)
#   #dde_effect_m ~ dnorm(0, sd = 2) 
#   #age_effect ~ dnorm(0, sd = 5)
#   #precip_effect ~ dnorm(0, sd = 2)
#   #bod_con_effect ~ dnorm(0, sd = 2)
#   #time_1_days_effect ~ dnorm(0, sd = 2)
#   #sex_effect ~ dnorm(0, sd = 2)
#   #time_1_preg_effect ~ dnorm(0, sd = 5)
#   
#   
#   for(i in 1:n.obs){
#     
#     #Process Model
#     logit(preg_prob[i])<-Intercept+
#        (lac_score[i]*lac_effect)+
#       (est_bm[i]*bm_effect)+
#       (dde_95[i]*dde_effect_m)+
#       (age[i]*age_effect)+
#       # (pow(age[i], 2)*age2_effect)+
#       
#       (sex_correct[i] * sex_effect) +
#       (lac_time[i]* lac_time_effect)+
#       (time_1_preg[i]* time_1_preg_effect)+
#     # (spei_3[i]*spei_3_effect)+
#     #(spei_6[i]*spei_6_effect)+
#     (spei_12[i]*spei_12_effect)#+
#     # (lac_score[i]*time_1_preg[i]*lac_score_preg_effect)+
#     
#     
#     
#     
#     
#     
#     #Observation Model (Likelihood)
#     preg_status[i] ~ dbern(preg_prob[i])
#     
#     #Impute missing data values
#     lac_score[i] ~ dnorm(mean=0,sd=1)
#     # precip_year_prior[i] ~ dnorm(mean = 0, sd = 1)
#     dde_95[i] ~ dnorm(mean=0,sd=1)
#      age[i] ~ dnorm(mean=0,sd=1)
#     # est_body_cond[i] ~ dnorm(mean=0,sd=1)
#      est_bm[i] ~ dnorm(mean=0,sd=1)
#     
#     lac_time[i] ~ dnorm(mean=0,sd=1)##does this matrer
#     #spei_3[i] ~ dnorm(mean = 0, sd = 1)
#     # spei_6[i] ~ dnorm(mean = 0, sd = 1)
#      spei_12[i] ~ dnorm(mean = 0, sd = 1)
#     
#     
#     
#   }#i
#   
# })#model5
# parameters <- c("lac_effect",
#   "bm_effect",
#   "dde_effect_m",
#    "age_effect",
#   # "age2_effect",
#   # "precip_effect",
#   "Intercept", 
#   # "bod_con_effect",
#   "lac_time_effect",
#   "sex_effect",
#   "time_1_preg_effect",
#   "logProb_preg_status",#
#   # "spei_3_effect",
#   #"spei_6_effect",
#   "spei_12_effect"#,
#   #"lac_score_preg_effect"#,
#   #"sex_lac_effect"
# )
# 
# # MCMC Settings
# ni <- 400000
# nt <- 400
# nb <- 200000
# nc <- 3
# 
# nimble.data = list(
#   preg_status = as.numeric(cow_capture_all$time_2_preg) - 1,
#   lac_score = as.numeric((cow_capture_all$lac_score)),
#   est_bm = as.numeric((cow_capture_all$est_bm)),
#   dde_95 = as.numeric((cow_capture_all$area_DDE_gt_95)),
#    age = as.numeric((cow_capture_all$age_estimate_2)),
#   #precip_year_prior = as.numeric((cow_capture_all$total_year_prior)),
#   sex_correct = ifelse(cow_capture_all$sex_correct== "m", 1,0),
#   lac_time= as.numeric(cow_capture_all$lac_time), 
#   time_1_preg= as.numeric(cow_capture_all$time_1_preg)-1,#
#   
#   #spei_3 = as.numeric((cow_capture_all$sept_year_prior_3M)),
#   # spei_6 = as.numeric((cow_capture_all$sept_year_prior_6M)),
# spei_12 = as.numeric((cow_capture_all$sept_year_prior_12M))
#   
# )
# 
# nimble.constants = list(n.obs = nrow(cow_capture_all))
# 
# 
# mcmc.output_full<- nimbleMCMC(code = model_full,
#                                     data = nimble.data,
#                                     constants=nimble.constants,
#                                     monitors = parameters,
#                                     niter = ni,
#                                     nburnin = nb,
#                                     nchains = nc,
#                                     thin=nt,
#                                     summary=TRUE,
#                                     samplesAsCodaMCMC = TRUE)
# 
# attach.nimble(mcmc.output_full$samples)
# 
# 
# mata_full  <- do.call(rbind, lapply(mcmc.output_full$samples, as.matrix))
# lpa_full <- grep("^logProb_preg_status\\[", colnames(mata_full), value = TRUE)
# lla_full  <- mata_full[, lpa_full, drop = FALSE]
# stopifnot(all(is.finite(lla_full) | (is.infinite(lla_full) & lla_full < 0)))
# loo_full <- loo(lla_full)
# 
# beep(sound = 1)
# 
# attach.nimble(mcmc.output_full$samples)
# 
# MCMCtrace(object = mcmc.output_full$samples,
#           pdf = FALSE, # no export to PDF
#           ind = TRUE, # separate density lines per chain
#           params = "time_1_preg_effect")
# # Gelman-Rubin diagnostic (AKA RHat or PSRF)
# gelman.diag(mcmc.output_full$samples, multivariate = FALSE)
# 
# 
# 
# mcmcplot(mcmc.output_full$samples)
# 
# 
# 
# 
# ##how to get effects size
# ##jow to make the differnt charts in PP
# ##how to get time on deck chart
# ##uses zscore to preduct value. Use mean for all the other values
# ##will use for calf age ##what the fuck is a z-score?
# post_mat <- as.matrix(mcmc.output_full$samples)
# # 2. Specify the parameters you care about
# params <- c("lac_effect",
#   "bm_effect",
#   "dde_effect_m",
#   "age_effect",
#   #"age2_effect",
#   # "precip_effect",
#   "Intercept", 
#   #"bod_con_effect",
#   "lac_time_effect",
#   "sex_effect",
#   "time_1_preg_effect",
#   #"spei_3_effect",
#   #"spei_6_effect",
#   "spei_12_effect"#,
#   #"lac_score_preg_effect", "sex_lac_effect"
# )
# 
# # 3. Summarize each one
# summary_df_full <- tibble(parameter = params) %>%
#   rowwise() %>%
#   mutate(
#     median   = median(post_mat[, parameter]),
#     lower95  = quantile(post_mat[, parameter], 0.025),
#     upper95  = quantile(post_mat[, parameter], 0.975),
#     # posterior probability effect > 0
#     p_gt_zero = mean(post_mat[, parameter] > 0),
#     # if you expect a negative effect, you can also compute P(<0):
#     p_lt_zero = mean(post_mat[, parameter] < 0)
#   ) %>%
#   ungroup()
# 
# print(summary_df_full)
# get_variables(mcmc.output_full$samples)
# 
# 
# summary_df_full  %>%
#   kbl(digits = 3, caption = "Posterior Summary Statistics for Model bc_2") %>%
#   kable_styling(full_width = FALSE, position = "center")
# 
# # Full posterior distributions:
# 
# 
# 
# mod_6_fig<-mcmc.output_full$samples%>%
#   spread_draws(lac_effect,
#     bm_effect,
#     dde_effect_m,
#     age_effect,
#     #age2_effect,
#     #precip_effect,
#     
#     # bod_con_effect,
#     lac_time_effect,
#     sex_effect,
#     time_1_preg_effect,
#     #spei_3_effect,
#     #spei_6_effect, 
#     spei_12_effect
#   ) %>%
#   pivot_longer(-c(.chain, .iteration, .draw),
#                names_to = 'Parameter',
#                values_to = 'Estimate') %>%
#   ggplot(aes(y = Parameter, x = Estimate, fill = Parameter)) +
#   stat_halfeye(show.legend = FALSE) +
#   geom_vline(xintercept = 0, linetype = 2, color = 'black') +
#   scale_fill_manual(values = c(
#     #lac_time_effect = "grey60",  # make precip_effect blue
#     sex_effect = "red",
#     time_1_preg_effect = "grey60",
#     bod_con_effect = "red4",
#     lac_time_effect = "grey60",
#     age_effect = "blue4"
#   )) +
#   theme_minimal()
# mod_6_fig
# ##########################################################end model 5
# ####preg effect
# 
# model_calf_only_lac1 <-  nimbleCode({
#   
#   #Priors
#   Intercept  ~ dnorm(0, sd = 5)
#   
#   #lac_effect~ dt(mu=0, sigma=1, df=1)
#   # bm_effect ~ dt(mu=0, sigma=1, df=1)
#   # dde_effect_m ~ dt(mu=0, sigma=1, df=1) 
#   # age_effect ~ dt(mu=0, sigma=1, df=1)
#   
#   lac_time_effect ~dt(mu=0, sigma=1, df=1)
#   sex_effect ~ dt(mu=0, sigma=1, df=1)
#   
#   time_1_preg_effect ~ dt(mu=0, sigma=1, df=1)
#   # spei_3_effect ~ dt(mu = 0 , sigma = 1, df = 1)
#   #spei_6_effect ~ dt(mu = 0 , sigma = 1, df = 1)
#   #spei_12_effect ~ dt(mu = 0 , sigma = 1, df = 1)
#   # age2_effect ~ dt(mu = 0, sigma = 1, df = 1)
#   # lac_score_preg_effect ~ dt(mu = 0, sigma = 1, df = 1)
#    sex_lac_effect ~ dt(mu = 0, sigma = 1, df = 1)
#   
#   #lac_effect~ dnorm(0, sd = 5)
#   #bm_effect ~ dnorm(0, sd = 5)
#   #dde_effect_m ~ dnorm(0, sd = 2) 
#   #age_effect ~ dnorm(0, sd = 5)
#   #precip_effect ~ dnorm(0, sd = 2)
#   #bod_con_effect ~ dnorm(0, sd = 2)
#   #time_1_days_effect ~ dnorm(0, sd = 2)
#   #sex_effect ~ dnorm(0, sd = 2)
#   #time_1_preg_effect ~ dnorm(0, sd = 5)
#   
#   
#   for(i in 1:n.obs){
#     
#     #Process Model
#     logit(preg_prob[i])<-Intercept+
#       #(lac_score[i]*lac_effect)+
#       #(est_bm[i]*bm_effect)+
#       #(dde_95[i]*dde_effect_m)+
#       # (age[i]*age_effect)+
#       # (pow(age[i], 2)*age2_effect)+
#       
#       (sex_correct[i] * sex_effect) +
#       (lac_time[i]* lac_time_effect)+
#       (time_1_preg[i]* time_1_preg_effect)+
#     # (spei_3[i]*spei_3_effect)+
#     #(spei_6[i]*spei_6_effect)+
#     #(spei_12[i]*spei_12_effect)#+
#      (lac_time[i]*sex_correct[i]*sex_lac_effect)
#     
#     
#     
#     
#     
#     
#     #Observation Model (Likelihood)
#     preg_status[i] ~ dbern(preg_prob[i])
#     
#     #Impute missing data values
#     #lac_score[i] ~ dnorm(mean=0,sd=1)
#     # precip_year_prior[i] ~ dnorm(mean = 0, sd = 1)
#     #dde_95[i] ~ dnorm(mean=0,sd=1)
#     # age[i] ~ dnorm(mean=0,sd=1)
#     # est_body_cond[i] ~ dnorm(mean=0,sd=1)
#     # est_bm[i] ~ dnorm(mean=0,sd=1)
#     
#     #lac_time[i] ~ dnorm(mean=0,sd=1)##does this matrer
#     #spei_3[i] ~ dnorm(mean = 0, sd = 1)
#     # spei_6[i] ~ dnorm(mean = 0, sd = 1)
#     # spei_12[i] ~ dnorm(mean = 0, sd = 1)
#     
#     
#     
#   }#i
#   
# })#model5
# parameters <- c(#"lac_effect",
#                 #"bm_effect",
#                 #"dde_effect_m",
#                 # "age_effect",
#                 # "age2_effect",
#                 # "precip_effect",
#                 "Intercept", 
#                 # "bod_con_effect",
#                 "lac_time_effect",
#                 "sex_effect",
#                 "time_1_preg_effect",
#                 "logProb_preg_status",#
#                 # "spei_3_effect",
#                 #"spei_6_effect",
#                 #"spei_12_effect"#,
#                 #"lac_score_preg_effect"#,
#                 "sex_lac_effect"
# )
# 
# # MCMC Settings
# ni <- 400000
# nt <- 400
# nb <- 200000
# nc <- 3
# 
# nimble.data = list(
#   preg_status = as.numeric(cow_capture_all$time_2_preg) - 1,
#   #lac_score = as.numeric((cow_capture_all$lac_score)),
#   #est_bm = as.numeric((cow_capture_all$est_bm)),
#   #dde_95 = as.numeric((cow_capture_all$area_DDE_gt_95)),
#   # age = as.numeric((cow_capture_all$age_estimate_2)),
#   #precip_year_prior = as.numeric((cow_capture_all$total_year_prior)),
#   sex_correct = ifelse(cow_capture_all$sex_correct== "m", 1,0),
#   lac_time= as.numeric(cow_capture_all$lac_time), 
#   time_1_preg= as.numeric(cow_capture_all$time_1_preg)-1#,#
#   
#   #spei_3 = as.numeric((cow_capture_all$sept_year_prior_3M)),
#   # spei_6 = as.numeric((cow_capture_all$sept_year_prior_6M)),
#   #spei_12 = as.numeric((cow_capture_all$sept_year_prior_12M))
#   
# )
# 
# nimble.constants = list(n.obs = nrow(cow_capture_all))
# 
# 
# mcmc.output_calf_lac <- nimbleMCMC(code = model_calf_only_lac1,
#                                    data = nimble.data,
#                                    constants=nimble.constants,
#                                    monitors = parameters,
#                                    niter = ni,
#                                    nburnin = nb,
#                                    nchains = nc,
#                                    thin=nt,
#                                    summary=TRUE,
#                                    samplesAsCodaMCMC = TRUE)
# 
# attach.nimble(mcmc.output_calf_lac$samples)
# beep(sound = 1)
# 
# mata_calf_lac  <- do.call(rbind, lapply(mcmc.output_calf_lac$samples, as.matrix))
# lpa_calf_lac  <- grep("^logProb_preg_status\\[", colnames(mata_calf_lac), value = TRUE)
# lla_calf_lac  <- mata_calf_lac[, lpa_calf_lac, drop = FALSE]
# stopifnot(all(is.finite(lla_calf_lac) | (is.infinite(lla_calf_lac) & lla_calf_lac < 0)))
# loo_calf_lac <- loo(lla_calf_lac)
# 
# 
# 
# attach.nimble(mcmc.output_calf_lac$samples)
# 
# MCMCtrace(object = mcmc.output_calf_lac$samples,
#           pdf = FALSE, # no export to PDF
#           ind = TRUE, # separate density lines per chain
#           params = "time_1_preg_effect")
# # Gelman-Rubin diagnostic (AKA RHat or PSRF)
# gelman.diag(mcmc.output_calf_lac$samples, multivariate = FALSE)
# 
# 
# 
# mcmcplot(mcmc.output_calf_lac$samples)
# 
# 
# 
# ##how to get effects size
# ##jow to make the differnt charts in PP
# ##how to get time on deck chart
# ##uses zscore to preduct value. Use mean for all the other values
# ##will use for calf age ##what the fuck is a z-score?
# post_mat <- as.matrix(mcmc.output_calf_lac$samples)
# # 2. Specify the parameters you care about
# params <- c(#"lac_effect",
#             #"bm_effect",
#             #"dde_effect_m",
#             #"age_effect",
#             #"age2_effect",
#             # "precip_effect",
#             "Intercept", 
#             #"bod_con_effect",
#             "lac_time_effect",
#             "sex_effect",
#             "time_1_preg_effect",
#             #"spei_3_effect",
#             #"spei_6_effect",
#             #"spei_12_effect"#,
#             #"lac_score_preg_effect", 
#             "sex_lac_effect"
# )
# 
# # 3. Summarize each one
# summary_df_calf_lac <- tibble(parameter = params) %>%
#   rowwise() %>%
#   mutate(
#     median   = median(post_mat[, parameter]),
#     lower95  = quantile(post_mat[, parameter], 0.025),
#     upper95  = quantile(post_mat[, parameter], 0.975),
#     # posterior probability effect > 0
#     p_gt_zero = mean(post_mat[, parameter] > 0),
#     # if you expect a negative effect, you can also compute P(<0):
#     p_lt_zero = mean(post_mat[, parameter] < 0)
#   ) %>%
#   ungroup()
# 
# print(summary_df_calf_lac)
# get_variables(mcmc.output_calf_lac$samples)
# 
# 
# summary_df_calf_1  %>%
#   kbl(digits = 3, caption = "Posterior Summary Statistics for Model bc_2") %>%
#   kable_styling(full_width = FALSE, position = "center")
# 
# # Full posterior distributions:
# 
# 
# 
# mod_6_fig<-mcmc.output_calf_lac$samples%>%
#   spread_draws(#lac_effect,
#                #bm_effect,
#                #dde_effect_m,
#                #age_effect,
#                #age2_effect,
#                #precip_effect,
#                
#                # bod_con_effect,
#                lac_time_effect,
#                sex_effect,
#                time_1_preg_effect,
#                sex_lac_effect
#                #spei_3_effect,
#                #spei_6_effect, 
#                #spei_12_effect
#   ) %>%
#   pivot_longer(-c(.chain, .iteration, .draw),
#                names_to = 'Parameter',
#                values_to = 'Estimate') %>%
#   ggplot(aes(y = Parameter, x = Estimate, fill = Parameter)) +
#   stat_halfeye(show.legend = FALSE) +
#   geom_vline(xintercept = 0, linetype = 2, color = 'black') +
#   scale_fill_manual(values = c(
#     #lac_time_effect = "grey60",  # make precip_effect blue
#     sex_effect = "red",
#     time_1_preg_effect = "grey60",
#     bod_con_effect = "red4",
#     lac_time_effect = "grey60",
#     age_effect = "blue4"
#   )) +
#   theme_minimal()
# mod_6_fig
# 
# ##########################################################end model 6
# ####half full################
# model_half <-  nimbleCode({
#   
#   #Priors
#   Intercept  ~ dnorm(0, sd = 5)
#   
#   #lac_effect~ dt(mu=0, sigma=1, df=1)
#    bm_effect ~ dt(mu=0, sigma=1, df=1)
#    dde_effect_m ~ dt(mu=0, sigma=1, df=1) 
#    age_effect ~ dt(mu=0, sigma=1, df=1)
#   
#  # lac_time_effect ~dt(mu=0, sigma=1, df=1)
#  # sex_effect ~ dt(mu=0, sigma=1, df=1)
#   
#   time_1_preg_effect ~ dt(mu=0, sigma=1, df=1)
#   # spei_3_effect ~ dt(mu = 0 , sigma = 1, df = 1)
#   #spei_6_effect ~ dt(mu = 0 , sigma = 1, df = 1)
#   spei_12_effect ~ dt(mu = 0 , sigma = 1, df = 1)
#   # age2_effect ~ dt(mu = 0, sigma = 1, df = 1)
#    #lac_score_preg_effect ~ dt(mu = 0, sigma = 1, df = 1)
#    #sex_lac_effect ~ dt(mu = 0, sigma = 1, df = 1)
# 
#   
#   
#   for(i in 1:n.obs){
#     
#     #Process Model
#     logit(preg_prob[i])<-Intercept+
#       #(lac_score[i]*lac_effect)+
#       (est_bm[i]*bm_effect)+
#       (dde_95[i]*dde_effect_m)+
#        (age[i]*age_effect)+
#       # (pow(age[i], 2)*age2_effect)+
#       
#       #(sex_correct[i] * sex_effect) +
#       #(lac_time[i]* lac_time_effect)+
#       (time_1_preg[i]* time_1_preg_effect)+
#     # (spei_3[i]*spei_3_effect)+
#     #(spei_6[i]*spei_6_effect)+
#     (spei_12[i]*spei_12_effect)#+
#      #(lac_score[i]*lac_time[i]*lac_score_preg_effect)
#     
#     
#     
#     
#     
#     
#     #Observation Model (Likelihood)
#     preg_status[i] ~ dbern(preg_prob[i])
#     
#     #Impute missing data values
#     lac_score[i] ~ dnorm(mean=0,sd=1)
#     # precip_year_prior[i] ~ dnorm(mean = 0, sd = 1)
#     #dde_95[i] ~ dnorm(mean=0,sd=1)
#      age[i] ~ dnorm(mean=0,sd=1)
#     # est_body_cond[i] ~ dnorm(mean=0,sd=1)
#      est_bm[i] ~ dnorm(mean=0,sd=1)
#     
#     #lac_time[i] ~ dnorm(mean=0,sd=1)##does this matrer
#     #spei_3[i] ~ dnorm(mean = 0, sd = 1)
#     # spei_6[i] ~ dnorm(mean = 0, sd = 1)
#     # spei_12[i] ~ dnorm(mean = 0, sd = 1)
#     
#     
#     
#   }#i
#   
# })#model5
# parameters <- c(#"lac_effect",
#                 "bm_effect",
#                 "dde_effect_m",
#                  "age_effect",
#                 # "age2_effect",
#                 # "precip_effect",
#                 "Intercept", 
#                 # "bod_con_effect",
#                # "lac_time_effect",
#                 #"sex_effect",
#                 "time_1_preg_effect",
#                 "logProb_preg_status",#
#                 # "spei_3_effect",
#                 #"spei_6_effect",
#                 "spei_12_effect"#,
#                 #"lac_score_preg_effect"#,
#                 ##"sex_lac_effect"
# )
# 
# # MCMC Settings
# ni <- 400000
# nt <- 400
# nb <- 200000
# nc <- 3
# 
# nimble.data = list(
#   preg_status = as.numeric(cow_capture_all$time_2_preg) - 1,
#   lac_score = as.numeric((cow_capture_all$lac_score)),
#   est_bm = as.numeric((cow_capture_all$est_bm)),
#   dde_95 = as.numeric((cow_capture_all$area_DDE_gt_95)),
#    age = as.numeric((cow_capture_all$age_estimate_2)),
#   #precip_year_prior = as.numeric((cow_capture_all$total_year_prior)),
#   sex_correct = ifelse(cow_capture_all$sex_correct== "m", 1,0),
#   lac_time= as.numeric(cow_capture_all$lac_time), 
#   time_1_preg= as.numeric(cow_capture_all$time_1_preg)-1,#
#   
#   #spei_3 = as.numeric((cow_capture_all$sept_year_prior_3M)),
#   # spei_6 = as.numeric((cow_capture_all$sept_year_prior_6M)),
#   spei_12 = as.numeric((cow_capture_all$sept_year_prior_12M))
#   
# )
# 
# nimble.constants = list(n.obs = nrow(cow_capture_all))
# 
# 
# mcmc.output_half <- nimbleMCMC(code = model_half,
#                                    data = nimble.data,
#                                    constants=nimble.constants,
#                                    monitors = parameters,
#                                    niter = ni,
#                                    nburnin = nb,
#                                    nchains = nc,
#                                    thin=nt,
#                                    summary=TRUE,
#                                    samplesAsCodaMCMC = TRUE)
# 
# attach.nimble(mcmc.output_half$samples)
# beep(sound = 1)
# 
# mata_half  <- do.call(rbind, lapply(mcmc.output_half$samples, as.matrix))
# lpa_half  <- grep("^logProb_preg_status\\[", colnames(mata_half), value = TRUE)
# lla_half  <- mata_half[, lpa_half, drop = FALSE]
# stopifnot(all(is.finite(lla_half) | (is.infinite(lla_half) & lla_half < 0)))
# loo_half <- loo(lla_half)
# 
# 
# 
# attach.nimble(mcmc.output_half$samples)
# 
# MCMCtrace(object = mcmc.output_half$samples,
#           pdf = FALSE, # no export to PDF
#           ind = TRUE, # separate density lines per chain
#           params = "time_1_preg_effect")
# # Gelman-Rubin diagnostic (AKA RHat or PSRF)
# gelman.diag(mcmc.output_half$samples, multivariate = FALSE)
# 
# 
# 
# mcmcplot(mcmc.output_half$samples)
# 
# 
# 
# ##how to get effects size
# ##jow to make the differnt charts in PP
# ##how to get time on deck chart
# ##uses zscore to preduct value. Use mean for all the other values
# ##will use for calf age ##what the fuck is a z-score?
# post_mat <- as.matrix(mcmc.output_half$samples)
# # 2. Specify the parameters you care about
# params <- c(#"lac_effect",
#             "bm_effect",
#             "dde_effect_m",
#             "age_effect",
#             #"age2_effect",
#             # "precip_effect",
#             "Intercept", 
#             #"bod_con_effect",
#             #"lac_time_effect",
#             #"sex_effect",
#             "time_1_preg_effect",
#             #"spei_3_effect",
#             #"spei_6_effect",
#             "spei_12_effect"#,
#             #"lac_score_preg_effect"#,
#             #"sex_lac_effect"
# )
# 
# # 3. Summarize each one
# summary_df_half <- tibble(parameter = params) %>%
#   rowwise() %>%
#   mutate(
#     median   = median(post_mat[, parameter]),
#     lower95  = quantile(post_mat[, parameter], 0.025),
#     upper95  = quantile(post_mat[, parameter], 0.975),
#     # posterior probability effect > 0
#     p_gt_zero = mean(post_mat[, parameter] > 0),
#     # if you expect a negative effect, you can also compute P(<0):
#     p_lt_zero = mean(post_mat[, parameter] < 0)
#   ) %>%
#   ungroup()
# 
# print(summary_df_half)
# get_variables(mcmc.output_half$samples)
# 
# 
# summary_df_half  %>%
#   kbl(digits = 3, caption = "Posterior Summary Statistics for Model bc_2") %>%
#   kable_styling(full_width = FALSE, position = "center")
# 
# # Full posterior distributions:
# 
# 
# 
# mod_7_fig<-mcmc.output_half$samples%>%
#   spread_draws(#lac_effect,
#                bm_effect,
#                dde_effect_m,
#                age_effect,
#                #age2_effect,
#                #precip_effect,
#                
#                # bod_con_effect,
#                #lac_time_effect,
#                #sex_effect,
#                #lac_score_preg_effect,
#                time_1_preg_effect,
#                #spei_3_effect,
#                #spei_6_effect, 
#                spei_12_effect
#   ) %>%
#   pivot_longer(-c(.chain, .iteration, .draw),
#                names_to = 'Parameter',
#                values_to = 'Estimate') %>%
#   ggplot(aes(y = Parameter, x = Estimate, fill = Parameter)) +
#   stat_halfeye(show.legend = FALSE) +
#   geom_vline(xintercept = 0, linetype = 2, color = 'black') +
#   scale_fill_manual(values = c(
#     #lac_time_effect = "grey60",  # make precip_effect blue
#     sex_effect = "red",
#     time_1_preg_effect = "grey60",
#     bod_con_effect = "red4",
#     lac_time_effect = "grey60",
#     age_effect = "blue4"
#   )) +
#   theme_minimal()
# mod_7_fig
# ##########################################################end model 7
# 
# 
# ####calf stuff with SPEI ################
# model_calf_spei <-  nimbleCode({
#   
#   #Priors
#   Intercept  ~ dnorm(0, sd = 5)
#   
#   #lac_effect~ dt(mu=0, sigma=1, df=1)
#   # bm_effect ~ dt(mu=0, sigma=1, df=1)
#   # dde_effect_m ~ dt(mu=0, sigma=1, df=1) 
#   # age_effect ~ dt(mu=0, sigma=1, df=1)
#   
#   #lac_time_effect ~dt(mu=0, sigma=1, df=1)
#   #sex_effect ~ dt(mu=0, sigma=1, df=1)
#   
#   time_1_preg_effect ~ dt(mu=0, sigma=1, df=1)
#   # spei_3_effect ~ dt(mu = 0 , sigma = 1, df = 1)
#   #spei_6_effect ~ dt(mu = 0 , sigma = 1, df = 1)
#   spei_12_effect ~ dt(mu = 0 , sigma = 1, df = 1)
#   # age2_effect ~ dt(mu = 0, sigma = 1, df = 1)
#   #lac_score_preg_effect ~ dt(mu = 0, sigma = 1, df = 1)
#   #sex_lac_effect ~ dt(mu = 0, sigma = 1, df = 1)
#   
#   #lac_effect~ dnorm(0, sd = 5)
#   #bm_effect ~ dnorm(0, sd = 5)
#   #dde_effect_m ~ dnorm(0, sd = 2) 
#   #age_effect ~ dnorm(0, sd = 5)
#   #precip_effect ~ dnorm(0, sd = 2)
#   #bod_con_effect ~ dnorm(0, sd = 2)
#   #time_1_days_effect ~ dnorm(0, sd = 2)
#   #sex_effect ~ dnorm(0, sd = 2)
#   #time_1_preg_effect ~ dnorm(0, sd = 5)
#   
#   
#   for(i in 1:n.obs){
#     
#     #Process Model
#     logit(preg_prob[i])<-Intercept+
#      # (lac_score[i]*lac_effect)+
#       #(est_bm[i]*bm_effect)+
#       #(dde_95[i]*dde_effect_m)+
#       # (age[i]*age_effect)+
#       # (pow(age[i], 2)*age2_effect)+
#       
#      # (sex_correct[i] * sex_effect) +
#       #(lac_time[i]* lac_time_effect)+
#       (time_1_preg[i]* time_1_preg_effect)+
#       # (spei_3[i]*spei_3_effect)+
#       #(spei_6[i]*spei_6_effect)+
#       (spei_12[i]*spei_12_effect)#+
#       #(lac_score[i]*lac_time[i]*lac_score_preg_effect)
#     
#     
#     
#     
#     
#     
#     #Observation Model (Likelihood)
#     preg_status[i] ~ dbern(preg_prob[i])
#     
#     #Impute missing data values
#     lac_score[i] ~ dnorm(mean=0,sd=1)
# 
#      spei_12[i] ~ dnorm(mean = 0, sd = 1)
#     
#     
#     
#   }#i
#   
# })#model5
# parameters <- c(
#                 #"bm_effect",
#                 #"dde_effect_m",
#                 # "age_effect",
#                 # "age2_effect",
#                 # "precip_effect",
#                 "Intercept", 
#             
#                 "time_1_preg_effect",
#                 "logProb_preg_status",#
#                 # "spei_3_effect",
#                 #"spei_6_effect",
#                 "spei_12_effect"#,
#                 #"lac_score_preg_effect"#,
#                 #"sex_lac_effect"
# )
# 
# # MCMC Settings
# ni <- 400000
# nt <- 400
# nb <- 200000
# nc <- 3
# 
# nimble.data = list(
#   preg_status = as.numeric(cow_capture_all$time_2_preg) - 1,
#   #lac_score = as.numeric((cow_capture_all$lac_score)),
#   #est_bm = as.numeric((cow_capture_all$est_bm)),
#   #dde_95 = as.numeric((cow_capture_all$area_DDE_gt_95)),
#   # age = as.numeric((cow_capture_all$age_estimate_2)),
#   #precip_year_prior = as.numeric((cow_capture_all$total_year_prior)),
#   #sex_correct = ifelse(cow_capture_all$sex_correct== "m", 1,0),
#   #lac_time= as.numeric(cow_capture_all$lac_time), 
#   time_1_preg= as.numeric(cow_capture_all$time_1_preg)-1,#
#   
#   #spei_3 = as.numeric((cow_capture_all$sept_year_prior_3M)),
#   # spei_6 = as.numeric((cow_capture_all$sept_year_prior_6M)),
#   spei_12 = as.numeric((cow_capture_all$sept_year_prior_12M))
#   
# )
# 
# nimble.constants = list(n.obs = nrow(cow_capture_all))
# 
# 
# mcmc.output_spei <- nimbleMCMC(code = model_calf_spei,
#                                     data = nimble.data,
#                                     constants=nimble.constants,
#                                     monitors = parameters,
#                                     niter = ni,
#                                     nburnin = nb,
#                                     nchains = nc,
#                                     thin=nt,
#                                     summary=TRUE,
#                                     samplesAsCodaMCMC = TRUE)
# beep(sound = 1)
# attach.nimble(mcmc.output_spei$samples)
# 
# 
# mata_calf_spei  <- do.call(rbind, lapply(mcmc.output_spei$samples, as.matrix))
# lpa_calf_spei  <- grep("^logProb_preg_status\\[", colnames(mata_calf_spei), value = TRUE)
# lla_calf_spei  <- mata_calf_spei[, lpa_calf_spei, drop = FALSE]
# stopifnot(all(is.finite(lla_calf_spei) | (is.infinite(lla_calf_spei) & lla_calf_spei < 0)))
# loo_calf_spei <- loo(lla_calf_spei)
# 
# 
# 
# attach.nimble(mcmc.output_spei$samples)
# 
# MCMCtrace(object = mcmc.output_spei$samples,
#           pdf = FALSE, # no export to PDF
#           ind = TRUE, # separate density lines per chain
#           params = "time_1_preg_effect")
# # Gelman-Rubin diagnostic (AKA RHat or PSRF)
# gelman.diag(mcmc.output_spei$samples, multivariate = FALSE)
# 
# # psrf_df <- tribble(
# #   ~Parameter,         ~`Point Estimate`, ~`Upper C.I.`,
# #   "Intercept",             1.01,              1.03,
# #   "age_effect",            1.01,              1.02,
# #   "bm_effect",             1.00,              1.01,
# #   "bod_con_effect",        1.00,              1.00,
# #   "dde_effect_95",         1.01,              1.02,
# #   "lac_effect",            1.00,              1.00,
# #   "precip_effect",         1.00,              1.00
# # )
# 
# # Display as formatted table
# # psrf_df %>%
# #   kbl(digits = 2, caption = "Potential Scale Reduction Factors (Gelman-Rubin R̂)") %>%
# #   kable_styling(full_width = FALSE, position = "center")
# 
# mcmcplot(mcmc.output_spei$samples)
# 
# 
# 
# 
# ##how to get effects size
# ##jow to make the differnt charts in PP
# ##how to get time on deck chart
# ##uses zscore to preduct value. Use mean for all the other values
# ##will use for calf age ##what the fuck is a z-score?
# post_mat <- as.matrix(mcmc.output_spei$samples)
# # 2. Specify the parameters you care about
# params <- c(
#             "Intercept", 
#           
#             "time_1_preg_effect",
#             #"spei_3_effect",
#             #"spei_6_effect",
#             "spei_12_effect"#,
#             #"lac_score_preg_effect"#,
#             #"sex_lac_effect"
# )
# 
# # 3. Summarize each one
# summary_df_calf_spei <- tibble(parameter = params) %>%
#   rowwise() %>%
#   mutate(
#     median   = median(post_mat[, parameter]),
#     lower95  = quantile(post_mat[, parameter], 0.025),
#     upper95  = quantile(post_mat[, parameter], 0.975),
#     # posterior probability effect > 0
#     p_gt_zero = mean(post_mat[, parameter] > 0),
#     # if you expect a negative effect, you can also compute P(<0):
#     p_lt_zero = mean(post_mat[, parameter] < 0)
#   ) %>%
#   ungroup()
# 
# print(summary_df_calf_spei)
# get_variables(mcmc.output_spei$samples)
# 
# 
# summary_df_calf_spei  %>%
#   kbl(digits = 3, caption = "Posterior Summary Statistics for Model bc_2") %>%
#   kable_styling(full_width = FALSE, position = "center")
# 
# # Full posterior distributions:
# 
# 
# 
# mod_8_fig<-mcmc.output_spei$samples%>%
#   spread_draws(
#                time_1_preg_effect,
# 
#                spei_12_effect
#   ) %>%
#   pivot_longer(-c(.chain, .iteration, .draw),
#                names_to = 'Parameter',
#                values_to = 'Estimate') %>%
#   ggplot(aes(y = Parameter, x = Estimate, fill = Parameter)) +
#   stat_halfeye(show.legend = FALSE) +
#   geom_vline(xintercept = 0, linetype = 2, color = 'black') +
#   scale_fill_manual(values = c(
#     #lac_time_effect = "grey60",  # make precip_effect blue
#     sex_effect = "red",
#     time_1_preg_effect = "grey60",
#     bod_con_effect = "red4",
#     lac_time_effect = "grey60",
#     age_effect = "blue4"
#   )) +
#   theme_minimal()
# mod_8_fig
# 
# 
# ##########################################################end model 8
# ###full model DDE, SPEI, time_1_preg, calf_sex, lac time, age, BM, lac_score
# 
# model_full_3 <-  nimbleCode({
#   
#   #Priors
#   Intercept  ~ dnorm(0, sd = 5)
#   
#   lac_effect~ dt(mu=0, sigma=1, df=1)
#   bm_effect ~ dt(mu=0, sigma=1, df=1)
#   dde_effect_m ~ dt(mu=0, sigma=1, df=1) 
#   age_effect ~ dt(mu=0, sigma=1, df=1)
#   
#   lac_time_effect ~dt(mu=0, sigma=1, df=1)
#   sex_effect ~ dt(mu=0, sigma=1, df=1)
#   
#   time_1_preg_effect ~ dt(mu=0, sigma=1, df=1)
#    spei_3_effect ~ dt(mu = 0 , sigma = 1, df = 1)
#   #spei_6_effect ~ dt(mu = 0 , sigma = 1, df = 1)
#   #spei_12_effect ~ dt(mu = 0 , sigma = 1, df = 1)
#   # age2_effect ~ dt(mu = 0, sigma = 1, df = 1)
#   # lac_score_preg_effect ~ dt(mu = 0, sigma = 1, df = 1)
#   #sex_lac_effect ~ dt(mu = 0, sigma = 1, df = 1)
#   
#   #lac_effect~ dnorm(0, sd = 5)
#   #bm_effect ~ dnorm(0, sd = 5)
#   #dde_effect_m ~ dnorm(0, sd = 2) 
#   #age_effect ~ dnorm(0, sd = 5)
#   #precip_effect ~ dnorm(0, sd = 2)
#   #bod_con_effect ~ dnorm(0, sd = 2)
#   #time_1_days_effect ~ dnorm(0, sd = 2)
#   #sex_effect ~ dnorm(0, sd = 2)
#   #time_1_preg_effect ~ dnorm(0, sd = 5)
#   
#   
#   for(i in 1:n.obs){
#     
#     #Process Model
#     logit(preg_prob[i])<-Intercept+
#       (lac_score[i]*lac_effect)+
#       (est_bm[i]*bm_effect)+
#       (dde_95[i]*dde_effect_m)+
#       (age[i]*age_effect)+
#       # (pow(age[i], 2)*age2_effect)+
#       
#       (sex_correct[i] * sex_effect) +
#       (lac_time[i]* lac_time_effect)+
#       (time_1_preg[i]* time_1_preg_effect)+
#        (spei_3[i]*spei_3_effect)#+
#       #(spei_6[i]*spei_6_effect)+
#       #(spei_12[i]*spei_12_effect)#+
#     # (lac_score[i]*time_1_preg[i]*lac_score_preg_effect)+
#     
#     
#     
#     
#     
#     
#     #Observation Model (Likelihood)
#     preg_status[i] ~ dbern(preg_prob[i])
#     
#     #Impute missing data values
#     lac_score[i] ~ dnorm(mean=0,sd=1)
#     # precip_year_prior[i] ~ dnorm(mean = 0, sd = 1)
#     dde_95[i] ~ dnorm(mean=0,sd=1)
#     age[i] ~ dnorm(mean=0,sd=1)
#     # est_body_cond[i] ~ dnorm(mean=0,sd=1)
#     est_bm[i] ~ dnorm(mean=0,sd=1)
#     
#     lac_time[i] ~ dnorm(mean=0,sd=1)##does this matrer
#     spei_3[i] ~ dnorm(mean = 0, sd = 1)
#     # spei_6[i] ~ dnorm(mean = 0, sd = 1)
#     #spei_12[i] ~ dnorm(mean = 0, sd = 1)
#     
#     
#     
#   }#i
#   
# })#model5
# parameters <- c("lac_effect",
#                 "bm_effect",
#                 "dde_effect_m",
#                 "age_effect",
#                 # "age2_effect",
#                 # "precip_effect",
#                 "Intercept", 
#                 # "bod_con_effect",
#                 "lac_time_effect",
#                 "sex_effect",
#                 "time_1_preg_effect",
#                 "logProb_preg_status",#
#                  "spei_3_effect"#,
#                 #"spei_6_effect",
#                 #"spei_12_effect"#,
#                 #"lac_score_preg_effect"#,
#                 #"sex_lac_effect"
# )
# 
# # MCMC Settings
# ni <- 400000
# nt <- 400
# nb <- 200000
# nc <- 3
# 
# nimble.data = list(
#   preg_status = as.numeric(cow_capture_all$time_2_preg) - 1,
#   lac_score = as.numeric((cow_capture_all$lac_score)),
#   est_bm = as.numeric((cow_capture_all$est_bm)),
#   dde_95 = as.numeric((cow_capture_all$area_DDE_gt_95)),
#   age = as.numeric((cow_capture_all$age_estimate_2)),
#   #precip_year_prior = as.numeric((cow_capture_all$total_year_prior)),
#   sex_correct = ifelse(cow_capture_all$sex_correct== "m", 1,0),
#   lac_time= as.numeric(cow_capture_all$lac_time), 
#   time_1_preg= as.numeric(cow_capture_all$time_1_preg)-1,#
#   
#   spei_3 = as.numeric((cow_capture_all$sept_year_prior_3M))#,
#   # spei_6 = as.numeric((cow_capture_all$sept_year_prior_6M)),
#   #spei_12 = as.numeric((cow_capture_all$sept_year_prior_12M))
#   
# )
# 
# nimble.constants = list(n.obs = nrow(cow_capture_all))
# 
# 
# mcmc.output_full_3<- nimbleMCMC(code = model_full_3,
#                               data = nimble.data,
#                               constants=nimble.constants,
#                               monitors = parameters,
#                               niter = ni,
#                               nburnin = nb,
#                               nchains = nc,
#                               thin=nt,
#                               summary=TRUE,
#                               samplesAsCodaMCMC = TRUE)
# 
# attach.nimble(mcmc.output_full_3$samples)
# 
# 
# mata_full <- do.call(rbind, lapply(mcmc.output_full_3$samples, as.matrix))
# lpa_full <- grep("^logProb_preg_status\\[", colnames(mata_full), value = TRUE)
# lla_full  <- mata_full[, lpa_full, drop = FALSE]
# stopifnot(all(is.finite(lla_full) | (is.infinite(lla_full) & lla_full < 0)))
# loo_full_spei3 <- loo(lla_full)
# 
# 
# 
# attach.nimble(mcmc.output_full_3$samples)
# 
# MCMCtrace(object = mcmc.output_full_3$samples,
#           pdf = FALSE, # no export to PDF
#           ind = TRUE, # separate density lines per chain
#           params = "time_1_preg_effect")
# # Gelman-Rubin diagnostic (AKA RHat or PSRF)
# gelman.diag(mcmc.output_full_3$samples, multivariate = FALSE)
# 
# 
# 
# mcmcplot(mcmc.output_full_3$samples)
# 
# 
# 
# 
# ##how to get effects size
# ##jow to make the differnt charts in PP
# ##how to get time on deck chart
# ##uses zscore to preduct value. Use mean for all the other values
# ##will use for calf age ##what the fuck is a z-score?
# post_mat <- as.matrix(mcmc.output_full_3$samples)
# # 2. Specify the parameters you care about
# params <- c("lac_effect",
#             "bm_effect",
#             "dde_effect_m",
#             "age_effect",
#             #"age2_effect",
#             # "precip_effect",
#             "Intercept", 
#             #"bod_con_effect",
#             "lac_time_effect",
#             "sex_effect",
#             "time_1_preg_effect",
#             "spei_3_effect"
#             #"spei_6_effect",
#             #"spei_12_effect"#,
#             #"lac_score_preg_effect", "sex_lac_effect"
# )
# 
# # 3. Summarize each one
# summary_df_full <- tibble(parameter = params) %>%
#   rowwise() %>%
#   mutate(
#     median   = median(post_mat[, parameter]),
#     lower95  = quantile(post_mat[, parameter], 0.025),
#     upper95  = quantile(post_mat[, parameter], 0.975),
#     # posterior probability effect > 0
#     p_gt_zero = mean(post_mat[, parameter] > 0),
#     # if you expect a negative effect, you can also compute P(<0):
#     p_lt_zero = mean(post_mat[, parameter] < 0)
#   ) %>%
#   ungroup()
# 
# print(summary_df_full)
# 
# 
# 
# summary_df_full  %>%
#   kbl(digits = 3, caption = "Posterior Summary Statistics for Model bc_2") %>%
#   kable_styling(full_width = FALSE, position = "center")
# 
# # Full posterior distributions:
# 
# 
# 
# mod_9_fig<-mcmc.output_full_3$samples%>%
#   spread_draws(lac_effect,
#                bm_effect,
#                dde_effect_m,
#                age_effect,
#                #age2_effect,
#                #precip_effect,
#                
#                # bod_con_effect,
#                lac_time_effect,
#                sex_effect,
#                time_1_preg_effect,
#                spei_3_effect
#                #spei_6_effect, 
#                #spei_12_effect
#   ) %>%
#   pivot_longer(-c(.chain, .iteration, .draw),
#                names_to = 'Parameter',
#                values_to = 'Estimate') %>%
#   ggplot(aes(y = Parameter, x = Estimate, fill = Parameter)) +
#   stat_halfeye(show.legend = FALSE) +
#   geom_vline(xintercept = 0, linetype = 2, color = 'black') +
#   scale_fill_manual(values = c(
#     #lac_time_effect = "grey60",  # make precip_effect blue
#     sex_effect = "red",
#     time_1_preg_effect = "grey60",
#     bod_con_effect = "red4",
#     lac_time_effect = "grey60",
#     age_effect = "blue4"
#   )) +
#   theme_minimal()
# mod_9_fig
# ##########################################################end model 9
# 
# 
# ###full model DDE, SPEI 6 month, time_1_preg, calf_sex, lac time, age, BM, lac_score
# 
# model_full_6 <-  nimbleCode({
#   
#   #Priors
#   Intercept  ~ dnorm(0, sd = 5)
#   
#   lac_effect~ dt(mu=0, sigma=1, df=1)
#   bm_effect ~ dt(mu=0, sigma=1, df=1)
#   dde_effect_m ~ dt(mu=0, sigma=1, df=1) 
#   age_effect ~ dt(mu=0, sigma=1, df=1)
#   
#   lac_time_effect ~dt(mu=0, sigma=1, df=1)
#   sex_effect ~ dt(mu=0, sigma=1, df=1)
#   precip_effect ~ dt(mu=0, sigma=1, df=1)
#   time_1_preg_effect ~ dt(mu=0, sigma=1, df=1)
#   #spei_3_effect ~ dt(mu = 0 , sigma = 1, df = 1)
#   spei_6_effect ~ dt(mu = 0 , sigma = 1, df = 1)
#   #spei_12_effect ~ dt(mu = 0 , sigma = 1, df = 1)
#    age2_effect ~ dt(mu = 0, sigma = 1, df = 1)
#   # lac_score_preg_effect ~ dt(mu = 0, sigma = 1, df = 1)
#   #sex_lac_effect ~ dt(mu = 0, sigma = 1, df = 1)
#   
#   #lac_effect~ dnorm(0, sd = 5)
#   #bm_effect ~ dnorm(0, sd = 5)
#   #dde_effect_m ~ dnorm(0, sd = 2) 
#   #age_effect ~ dnorm(0, sd = 5)
#   #precip_effect ~ dnorm(0, sd = 2)
#   #bod_con_effect ~ dnorm(0, sd = 2)
#   #time_1_days_effect ~ dnorm(0, sd = 2)
#   #sex_effect ~ dnorm(0, sd = 2)
#   #time_1_preg_effect ~ dnorm(0, sd = 5)
#   
#   
#   for(i in 1:n.obs){
#     
#     #Process Model
#     logit(preg_prob[i])<-Intercept+
#       (lac_score[i]*lac_effect)+
#       (est_bm[i]*bm_effect)+
#       (dde_95[i]*dde_effect_m)+
#       (age[i]*age_effect)+
#       (precip_year_prior[i] * precip_effect)+
#       (pow(age[i], 2)*age2_effect)+
#       
#       (sex_correct[i] * sex_effect) +
#       (lac_time[i]* lac_time_effect)+
#       (time_1_preg[i]* time_1_preg_effect)+
#       #(spei_3[i]*spei_3_effect)#+
#     (spei_6[i]*spei_6_effect)#+
#     #(spei_12[i]*spei_12_effect)#+
#     # (lac_score[i]*time_1_preg[i]*lac_score_preg_effect)+
#     
#     
#     
#     
#     
#     
#     #Observation Model (Likelihood)
#     preg_status[i] ~ dbern(preg_prob[i])
#     
#     #Impute missing data values
#     lac_score[i] ~ dnorm(mean=0,sd=1)
#      precip_year_prior[i] ~ dnorm(mean = 0, sd = 1)
#     dde_95[i] ~ dnorm(mean=0,sd=1)
#     age[i] ~ dnorm(mean=0,sd=1)
#     # est_body_cond[i] ~ dnorm(mean=0,sd=1)
#     est_bm[i] ~ dnorm(mean=0,sd=1)
#     
#     lac_time[i] ~ dnorm(mean=0,sd=1)##does this matrer
#     #spei_3[i] ~ dnorm(mean = 0, sd = 1)
#     spei_6[i] ~ dnorm(mean = 0, sd = 1)
#     #spei_12[i] ~ dnorm(mean = 0, sd = 1)
#     
#     
#     
#   }#i
#   
# })#model5
# parameters <- c("lac_effect",
#                 "bm_effect",
#                 "dde_effect_m",
#                 "age_effect",
#                 # "age2_effect",
#                  "precip_effect",
#                 "Intercept", 
#                 # "bod_con_effect",
#                 "lac_time_effect",
#                 "sex_effect",
#                 "time_1_preg_effect",
#                 "logProb_preg_status",#
#                # "spei_3_effect"#,
#                  "spei_6_effect"#,
#                 #"spei_12_effect"#,
#                 #"lac_score_preg_effect"#,
#                 #"sex_lac_effect"
# )
# 
# # MCMC Settings
# ni <- 400000
# nt <- 400
# nb <- 200000
# nc <- 3
# 
# nimble.data = list(
#   preg_status = as.numeric(cow_capture_all$time_2_preg) - 1,
#   lac_score = as.numeric((cow_capture_all$lac_score)),
#   est_bm = as.numeric((cow_capture_all$est_bm)),
#   dde_95 = as.numeric((cow_capture_all$area_DDE_gt_95)),
#   age = as.numeric((cow_capture_all$age_estimate_2)),
#   precip_year_prior = as.numeric((cow_capture_all$total_year_prior)),
#   sex_correct = ifelse(cow_capture_all$sex_correct== "m", 1,0),
#   lac_time= as.numeric(cow_capture_all$lac_time), 
#   time_1_preg= as.numeric(cow_capture_all$time_1_preg)-1,#
#   
#   #spei_3 = as.numeric((cow_capture_all$sept_year_prior_3M))#,
#   spei_6 = as.numeric((cow_capture_all$sept_year_prior_6M))#,
#   #spei_12 = as.numeric((cow_capture_all$sept_year_prior_12M))
#   
# )
# 
# nimble.constants = list(n.obs = nrow(cow_capture_all))
# 
# 
# mcmc.output_full_6<- nimbleMCMC(code = model_full_6,
#                                 data = nimble.data,
#                                 constants=nimble.constants,
#                                 monitors = parameters,
#                                 niter = ni,
#                                 nburnin = nb,
#                                 nchains = nc,
#                                 thin=nt,
#                                 summary=TRUE,
#                                 samplesAsCodaMCMC = TRUE)
# 
# attach.nimble(mcmc.output_full_6$samples)
# 
# 
# mata_full <- do.call(rbind, lapply(mcmc.output_full_6$samples, as.matrix))
# lpa_full <- grep("^logProb_preg_status\\[", colnames(mata_full), value = TRUE)
# lla_full  <- mata_full[, lpa_full, drop = FALSE]
# stopifnot(all(is.finite(lla_full) | (is.infinite(lla_full) & lla_full < 0)))
# loo_full_spei6 <- loo(lla_full)
# beep(sound =1)
# 
# 
# attach.nimble(mcmc.output_full_6$samples)
# 
# MCMCtrace(object = mcmc.output_full_6$samples,
#           pdf = FALSE, # no export to PDF
#           ind = TRUE, # separate density lines per chain
#           params = "time_1_preg_effect")
# # Gelman-Rubin diagnostic (AKA RHat or PSRF)
# gelman.diag(mcmc.output_full_3$samples, multivariate = FALSE)
# 
# 
# 
# mcmcplot(mcmc.output_full_6$samples)
# 
# 
# 
# 
# ##how to get effects size
# ##jow to make the differnt charts in PP
# ##how to get time on deck chart
# ##uses zscore to preduct value. Use mean for all the other values
# ##will use for calf age ##what the fuck is a z-score?
# post_mat <- as.matrix(mcmc.output_full_6$samples)
# # 2. Specify the parameters you care about
# params <- c("lac_effect",
#             "bm_effect",
#             "dde_effect_m",
#             "age_effect",
#             #"age2_effect",
#             # "precip_effect",
#             "Intercept", 
#             #"bod_con_effect",
#             "lac_time_effect",
#             "sex_effect",
#             "time_1_preg_effect",
#             #"spei_3_effect"
#             "spei_6_effect"#,
#             #"spei_12_effect"#,
#             #"lac_score_preg_effect", "sex_lac_effect"
# )
# 
# # 3. Summarize each one
# summary_df_full <- tibble(parameter = params) %>%
#   rowwise() %>%
#   mutate(
#     median   = median(post_mat[, parameter]),
#     lower95  = quantile(post_mat[, parameter], 0.025),
#     upper95  = quantile(post_mat[, parameter], 0.975),
#     # posterior probability effect > 0
#     p_gt_zero = mean(post_mat[, parameter] > 0),
#     # if you expect a negative effect, you can also compute P(<0):
#     p_lt_zero = mean(post_mat[, parameter] < 0)
#   ) %>%
#   ungroup()
# 
# print(summary_df_full)
# get_variables(mcmc.output_full_6$samples)
# 
# 
# summary_df_full  %>%
#   kbl(digits = 3, caption = "Posterior Summary Statistics for Model bc_2") %>%
#   kable_styling(full_width = FALSE, position = "center")
# 
# # Full posterior distributions:
# 
# 
# 
# mod_10_fig<-mcmc.output_full_6$samples%>%
#   spread_draws(lac_effect,
#                bm_effect,
#                dde_effect_m,
#                age_effect,
#                #age2_effect,
#                #precip_effect,
#                
#                # bod_con_effect,
#                lac_time_effect,
#                sex_effect,
#                time_1_preg_effect,
#                #spei_3_effect
#                spei_6_effect#, 
#                #spei_12_effect
#   ) %>%
#   pivot_longer(-c(.chain, .iteration, .draw),
#                names_to = 'Parameter',
#                values_to = 'Estimate') %>%
#   ggplot(aes(y = Parameter, x = Estimate, fill = Parameter)) +
#   stat_halfeye(show.legend = FALSE) +
#   geom_vline(xintercept = 0, linetype = 2, color = 'black') +
#   scale_fill_manual(values = c(
#     #lac_time_effect = "grey60",  # make precip_effect blue
#     sex_effect = "red",
#     time_1_preg_effect = "grey60",
#     bod_con_effect = "red4",
#     lac_time_effect = "grey60",
#     age_effect = "blue4"
#   )) +
#   theme_minimal()
# mod_10_fig
# ##########################################################end model 10
# 
# 
# ####half full################
# model_half_75 <-  nimbleCode({
#   
#   #Priors
#   Intercept  ~ dnorm(0, sd = 5)
#   
#   #lac_effect~ dt(mu=0, sigma=1, df=1)
#   bm_effect ~ dt(mu=0, sigma=1, df=1)
#   dde_effect_m ~ dt(mu=0, sigma=1, df=1) 
#   age_effect ~ dt(mu=0, sigma=1, df=1)
#   
#   # lac_time_effect ~dt(mu=0, sigma=1, df=1)
#   # sex_effect ~ dt(mu=0, sigma=1, df=1)
#   
#   time_1_preg_effect ~ dt(mu=0, sigma=1, df=1)
#   # spei_3_effect ~ dt(mu = 0 , sigma = 1, df = 1)
#   #spei_6_effect ~ dt(mu = 0 , sigma = 1, df = 1)
#   spei_12_effect ~ dt(mu = 0 , sigma = 1, df = 1)
#   # age2_effect ~ dt(mu = 0, sigma = 1, df = 1)
#   #lac_score_preg_effect ~ dt(mu = 0, sigma = 1, df = 1)
#   #sex_lac_effect ~ dt(mu = 0, sigma = 1, df = 1)
#   
#   
#   
#   for(i in 1:n.obs){
#     
#     #Process Model
#     logit(preg_prob[i])<-Intercept+
#       #(lac_score[i]*lac_effect)+
#       (est_bm[i]*bm_effect)+
#       (dde_75[i]*dde_effect_m)+
#       (age[i]*age_effect)+
#       # (pow(age[i], 2)*age2_effect)+
#       
#       #(sex_correct[i] * sex_effect) +
#       #(lac_time[i]* lac_time_effect)+
#       (time_1_preg[i]* time_1_preg_effect)+
#       # (spei_3[i]*spei_3_effect)+
#       #(spei_6[i]*spei_6_effect)+
#       (spei_12[i]*spei_12_effect)#+
#     #(lac_score[i]*lac_time[i]*lac_score_preg_effect)
#     
#     
#     
#     
#     
#     
#     #Observation Model (Likelihood)
#     preg_status[i] ~ dbern(preg_prob[i])
#     
#     #Impute missing data values
#     lac_score[i] ~ dnorm(mean=0,sd=1)
#     # precip_year_prior[i] ~ dnorm(mean = 0, sd = 1)
#     #dde_95[i] ~ dnorm(mean=0,sd=1)
#      age[i] ~ dnorm(mean=0,sd=1)
#     # est_body_cond[i] ~ dnorm(mean=0,sd=1)
#     est_bm[i] ~ dnorm(mean=0,sd=1)
#     
#     #lac_time[i] ~ dnorm(mean=0,sd=1)##does this matrer
#     #spei_3[i] ~ dnorm(mean = 0, sd = 1)
#     # spei_6[i] ~ dnorm(mean = 0, sd = 1)
#     # spei_12[i] ~ dnorm(mean = 0, sd = 1)
#     
#     
#     
#   }#i
#   
# })#model5
# parameters <- c(#"lac_effect",
#   "bm_effect",
#   "dde_effect_m",
#   "age_effect",
#   # "age2_effect",
#   # "precip_effect",
#   "Intercept", 
#   # "bod_con_effect",
#   # "lac_time_effect",
#   #"sex_effect",
#   "time_1_preg_effect",
#   "logProb_preg_status",#
#   # "spei_3_effect",
#   #"spei_6_effect",
#   "spei_12_effect"#,
#   #"lac_score_preg_effect"#,
#   ##"sex_lac_effect"
# )
# 
# # MCMC Settings
# ni <- 400000
# nt <- 400
# nb <- 200000
# nc <- 3
# 
# nimble.data = list(
#   preg_status = as.numeric(cow_capture_all$time_2_preg) - 1,
#   lac_score = as.numeric((cow_capture_all$lac_score)),
#   est_bm = as.numeric((cow_capture_all$est_bm)),
#   #dde_95 = as.numeric((cow_capture_all$area_DDE_gt_95)),
#   dde_75 = as.numeric((cow_capture_all$area_DDE_gt_75)),
#   age = as.numeric((cow_capture_all$age_estimate_2)),
#   #precip_year_prior = as.numeric((cow_capture_all$total_year_prior)),
#   sex_correct = ifelse(cow_capture_all$sex_correct== "m", 1,0),
#   lac_time= as.numeric(cow_capture_all$lac_time), 
#   time_1_preg= as.numeric(cow_capture_all$time_1_preg)-1,#
#   
#   #spei_3 = as.numeric((cow_capture_all$sept_year_prior_3M)),
#   # spei_6 = as.numeric((cow_capture_all$sept_year_prior_6M)),
#   spei_12 = as.numeric((cow_capture_all$sept_year_prior_12M))
#   
# )
# 
# nimble.constants = list(n.obs = nrow(cow_capture_all))
# 
# 
# mcmc.output_half_75 <- nimbleMCMC(code = model_half_75,
#                                data = nimble.data,
#                                constants=nimble.constants,
#                                monitors = parameters,
#                                niter = ni,
#                                nburnin = nb,
#                                nchains = nc,
#                                thin=nt,
#                                summary=TRUE,
#                                samplesAsCodaMCMC = TRUE)
# 
# attach.nimble(mcmc.output_half_75$samples)
# 
# 
# mata_half  <- do.call(rbind, lapply(mcmc.output_half_75$samples, as.matrix))
# lpa_half  <- grep("^logProb_preg_status\\[", colnames(mata_half), value = TRUE)
# lla_half  <- mata_half[, lpa_half, drop = FALSE]
# stopifnot(all(is.finite(lla_half) | (is.infinite(lla_half) & lla_half < 0)))
# loo_half_75 <- loo(lla_half)
# 
# 
# 
# attach.nimble(mcmc.output_half_75$samples)
# 
# MCMCtrace(object = mcmc.output_half_75$samples,
#           pdf = FALSE, # no export to PDF
#           ind = TRUE, # separate density lines per chain
#           params = "time_1_preg_effect")
# # Gelman-Rubin diagnostic (AKA RHat or PSRF)
# gelman.diag(mcmc.output_half_75$samples, multivariate = FALSE)
# 
# 
# 
# mcmcplot(mcmc.output_half_75$samples)
# 
# 
# 
# ##how to get effects size
# ##jow to make the differnt charts in PP
# ##how to get time on deck chart
# ##uses zscore to preduct value. Use mean for all the other values
# ##will use for calf age ##what the fuck is a z-score?
# post_mat <- as.matrix(mcmc.output_half_75$samples)
# # 2. Specify the parameters you care about
# params <- c(#"lac_effect",
#   "bm_effect",
#   "dde_effect_m",
#   "age_effect",
#   #"age2_effect",
#   # "precip_effect",
#   "Intercept", 
#   #"bod_con_effect",
#   #"lac_time_effect",
#   #"sex_effect",
#   "time_1_preg_effect",
#   #"spei_3_effect",
#   #"spei_6_effect",
#   "spei_12_effect"#,
#   #"lac_score_preg_effect"#,
#   #"sex_lac_effect"
# )
# 
# # 3. Summarize each one
# summary_df_half <- tibble(parameter = params) %>%
#   rowwise() %>%
#   mutate(
#     median   = median(post_mat[, parameter]),
#     lower95  = quantile(post_mat[, parameter], 0.025),
#     upper95  = quantile(post_mat[, parameter], 0.975),
#     # posterior probability effect > 0
#     p_gt_zero = mean(post_mat[, parameter] > 0),
#     # if you expect a negative effect, you can also compute P(<0):
#     p_lt_zero = mean(post_mat[, parameter] < 0)
#   ) %>%
#   ungroup()
# 
# print(summary_df_half)
# get_variables(mcmc.output_half_75$samples)
# 
# 
# summary_df_half  %>%
#   kbl(digits = 3, caption = "Posterior Summary Statistics for Model bc_2") %>%
#   kable_styling(full_width = FALSE, position = "center")
# 
# # Full posterior distributions:
# 
# 
# 
# mod_11_fig<-mcmc.output_half_75$samples%>%
#   spread_draws(#lac_effect,
#     bm_effect,
#     dde_effect_m,
#     age_effect,
#     #age2_effect,
#     #precip_effect,
#     
#     # bod_con_effect,
#     #lac_time_effect,
#     #sex_effect,
#     #lac_score_preg_effect,
#     time_1_preg_effect,
#     #spei_3_effect,
#     #spei_6_effect, 
#     spei_12_effect
#   ) %>%
#   pivot_longer(-c(.chain, .iteration, .draw),
#                names_to = 'Parameter',
#                values_to = 'Estimate') %>%
#   ggplot(aes(y = Parameter, x = Estimate, fill = Parameter)) +
#   stat_halfeye(show.legend = FALSE) +
#   geom_vline(xintercept = 0, linetype = 2, color = 'black') +
#   scale_fill_manual(values = c(
#     #lac_time_effect = "grey60",  # make precip_effect blue
#     sex_effect = "red",
#     time_1_preg_effect = "grey60",
#     bod_con_effect = "red4",
#     lac_time_effect = "grey60",
#     age_effect = "blue4"
#   )) +
#   theme_minimal()
# mod_11_fig
# 
# ####end of 11
# 
# 
# 
# ####half full################mean dde, spei_6
# model_half_m6 <-  nimbleCode({
#   
#   #Priors
#   Intercept  ~ dnorm(0, sd = 5)
#   
#   #lac_effect~ dt(mu=0, sigma=1, df=1)
#   bm_effect ~ dt(mu=0, sigma=1, df=1)
#   dde_effect_m ~ dt(mu=0, sigma=1, df=1) 
#   age_effect ~ dt(mu=0, sigma=1, df=1)
#   
#   # lac_time_effect ~dt(mu=0, sigma=1, df=1)
#   # sex_effect ~ dt(mu=0, sigma=1, df=1)
#   
#   time_1_preg_effect ~ dt(mu=0, sigma=1, df=1)
#   # spei_3_effect ~ dt(mu = 0 , sigma = 1, df = 1)
#   spei_6_effect ~ dt(mu = 0 , sigma = 1, df = 1)
#   #spei_12_effect ~ dt(mu = 0 , sigma = 1, df = 1)
#   # age2_effect ~ dt(mu = 0, sigma = 1, df = 1)
#   #lac_score_preg_effect ~ dt(mu = 0, sigma = 1, df = 1)
#   #sex_lac_effect ~ dt(mu = 0, sigma = 1, df = 1)
#   
#   
#   
#   for(i in 1:n.obs){
#     
#     #Process Model
#     logit(preg_prob[i])<-Intercept+
#       #(lac_score[i]*lac_effect)+
#       (est_bm[i]*bm_effect)+
#       (dde_m[i]*dde_effect_m)+
#       (age[i]*age_effect)+
#       # (pow(age[i], 2)*age2_effect)+
#       
#       #(sex_correct[i] * sex_effect) +
#       #(lac_time[i]* lac_time_effect)+
#       (time_1_preg[i]* time_1_preg_effect)+
#       # (spei_3[i]*spei_3_effect)+
#       (spei_6[i]*spei_6_effect)#+
#       #(spei_12[i]*spei_12_effect)#+
#     #(lac_score[i]*lac_time[i]*lac_score_preg_effect)
#     
#     
#     
#     
#     
#     
#     #Observation Model (Likelihood)
#     preg_status[i] ~ dbern(preg_prob[i])
#     
#     #Impute missing data values
#     lac_score[i] ~ dnorm(mean=0,sd=1)
#     # precip_year_prior[i] ~ dnorm(mean = 0, sd = 1)
#     #dde_95[i] ~ dnorm(mean=0,sd=1)
#      age[i] ~ dnorm(mean=0,sd=1)
#     # est_body_cond[i] ~ dnorm(mean=0,sd=1)
#     est_bm[i] ~ dnorm(mean=0,sd=1)
#     
#     #lac_time[i] ~ dnorm(mean=0,sd=1)##does this matrer
#     #spei_3[i] ~ dnorm(mean = 0, sd = 1)
#     # spei_6[i] ~ dnorm(mean = 0, sd = 1)
#     # spei_12[i] ~ dnorm(mean = 0, sd = 1)
#     
#     
#     
#   }#i
#   
# })#model5
# parameters <- c(#"lac_effect",
#   "bm_effect",
#   "dde_effect_m",
#   "age_effect",
#   # "age2_effect",
#   # "precip_effect",
#   "Intercept", 
#   # "bod_con_effect",
#   # "lac_time_effect",
#   #"sex_effect",
#   "time_1_preg_effect",
#   "logProb_preg_status",#
#   # "spei_3_effect",
#   "spei_6_effect"#,
#   #"spei_12_effect"#,
#   #"lac_score_preg_effect"#,
#   ##"sex_lac_effect"
# )
# 
# # MCMC Settings
# ni <- 400000
# nt <- 400
# nb <- 200000
# nc <- 3
# 
# nimble.data = list(
#   preg_status = as.numeric(cow_capture_all$time_2_preg) - 1,
#   lac_score = as.numeric((cow_capture_all$lac_score)),
#   est_bm = as.numeric((cow_capture_all$est_bm)),
#   #dde_95 = as.numeric((cow_capture_all$area_DDE_gt_95)),
#   #dde_75 = as.numeric((cow_capture_all$area_DDE_gt_75)),
#   dde_m = as.numeric((cow_capture_all$area_DDE_gt_mean)),
#   age = as.numeric((cow_capture_all$age_estimate_2)),
#   #precip_year_prior = as.numeric((cow_capture_all$total_year_prior)),
#   sex_correct = ifelse(cow_capture_all$sex_correct== "m", 1,0),
#   lac_time= as.numeric(cow_capture_all$lac_time), 
#   time_1_preg= as.numeric(cow_capture_all$time_1_preg)-1,#
#   
#   #spei_3 = as.numeric((cow_capture_all$sept_year_prior_3M)),
#    spei_6 = as.numeric((cow_capture_all$sept_year_prior_6M))#,
#   #spei_12 = as.numeric((cow_capture_all$sept_year_prior_12M))
#   
# )
# 
# nimble.constants = list(n.obs = nrow(cow_capture_all))
# 
# 
# mcmc.output_half_m6 <- nimbleMCMC(code = model_half_m6,
#                                   data = nimble.data,
#                                   constants=nimble.constants,
#                                   monitors = parameters,
#                                   niter = ni,
#                                   nburnin = nb,
#                                   nchains = nc,
#                                   thin=nt,
#                                   summary=TRUE,
#                                   samplesAsCodaMCMC = TRUE)
# 
# attach.nimble(mcmc.output_half_m6$samples)
# 
# 
# mata_half  <- do.call(rbind, lapply(mcmc.output_half_m6$samples, as.matrix))
# lpa_half  <- grep("^logProb_preg_status\\[", colnames(mata_half), value = TRUE)
# lla_half  <- mata_half[, lpa_half, drop = FALSE]
# stopifnot(all(is.finite(lla_half) | (is.infinite(lla_half) & lla_half < 0)))
# loo_half_m6 <- loo(lla_half)
# 
# 
# 
# attach.nimble(mcmc.output_half_m6$samples)
# 
# MCMCtrace(object = mcmc.output_half_m6$samples,
#           pdf = FALSE, # no export to PDF
#           ind = TRUE, # separate density lines per chain
#           params = "time_1_preg_effect")
# # Gelman-Rubin diagnostic (AKA RHat or PSRF)
# gelman.diag(mcmc.output_half_m6$samples, multivariate = FALSE)
# 
# 
# 
# mcmcplot(mcmc.output_half_m6$samples)
# 
# 
# 
# ##how to get effects size
# ##jow to make the differnt charts in PP
# ##how to get time on deck chart
# ##uses zscore to preduct value. Use mean for all the other values
# ##will use for calf age ##what the fuck is a z-score?
# post_mat <- as.matrix(mcmc.output_half_m6$samples)
# # 2. Specify the parameters you care about
# params <- c(#"lac_effect",
#   "bm_effect",
#   "dde_effect_m",
#   "age_effect",
#   #"age2_effect",
#   # "precip_effect",
#   "Intercept", 
#   #"bod_con_effect",
#   #"lac_time_effect",
#   #"sex_effect",
#   "time_1_preg_effect",
#   #"spei_3_effect",
#   "spei_6_effect"#,
#   #"spei_12_effect"#,
#   #"lac_score_preg_effect"#,
#   #"sex_lac_effect"
# )
# 
# # 3. Summarize each one
# summary_df_half_m6 <- tibble(parameter = params) %>%
#   rowwise() %>%
#   mutate(
#     median   = median(post_mat[, parameter]),
#     lower95  = quantile(post_mat[, parameter], 0.025),
#     upper95  = quantile(post_mat[, parameter], 0.975),
#     # posterior probability effect > 0
#     p_gt_zero = mean(post_mat[, parameter] > 0),
#     # if you expect a negative effect, you can also compute P(<0):
#     p_lt_zero = mean(post_mat[, parameter] < 0)
#   ) %>%
#   ungroup()
# 
# print(summary_df_half_m6)
# get_variables(mcmc.output_half_m6$samples)
# 
# 
# summary_df_half_m6  %>%
#   kbl(digits = 3, caption = "Posterior Summary Statistics for Model bc_2") %>%
#   kable_styling(full_width = FALSE, position = "center")
# 
# # Full posterior distributions:
# 
# 
# 
# mod_12_fig<-mcmc.output_half_m6$samples%>%
#   spread_draws(#lac_effect,
#     bm_effect,
#     dde_effect_m,
#     age_effect,
#     #age2_effect,
#     #precip_effect,
#     
#     # bod_con_effect,
#     #lac_time_effect,
#     #sex_effect,
#     #lac_score_preg_effect,
#     time_1_preg_effect,
#     #spei_3_effect,
#     spei_6_effect#, 
#     #spei_12_effect
#   ) %>%
#   pivot_longer(-c(.chain, .iteration, .draw),
#                names_to = 'Parameter',
#                values_to = 'Estimate') %>%
#   ggplot(aes(y = Parameter, x = Estimate, fill = Parameter)) +
#   stat_halfeye(show.legend = FALSE) +
#   geom_vline(xintercept = 0, linetype = 2, color = 'black') +
#   scale_fill_manual(values = c(
#     #lac_time_effect = "grey60",  # make precip_effect blue
#     sex_effect = "red",
#     time_1_preg_effect = "grey60",
#     bod_con_effect = "red4",
#     lac_time_effect = "grey60",
#     age_effect = "blue4"
#   )) +
#   theme_minimal()
# mod_12_fig
# 
# ####end model 12#####
# 
# 
# ####half full################mean dde
# model_half_m <-  nimbleCode({
#   
#   #Priors
#   Intercept  ~ dnorm(0, sd = 5)
#   
#   #lac_effect~ dt(mu=0, sigma=1, df=1)
#   bm_effect ~ dt(mu=0, sigma=1, df=1)
#   dde_effect_m ~ dt(mu=0, sigma=1, df=1) 
#   age_effect ~ dt(mu=0, sigma=1, df=1)
#   
#   # lac_time_effect ~dt(mu=0, sigma=1, df=1)
#   # sex_effect ~ dt(mu=0, sigma=1, df=1)
#   
#   time_1_preg_effect ~ dt(mu=0, sigma=1, df=1)
#   # spei_3_effect ~ dt(mu = 0 , sigma = 1, df = 1)
#   #spei_6_effect ~ dt(mu = 0 , sigma = 1, df = 1)
#   spei_12_effect ~ dt(mu = 0 , sigma = 1, df = 1)
#   # age2_effect ~ dt(mu = 0, sigma = 1, df = 1)
#   #lac_score_preg_effect ~ dt(mu = 0, sigma = 1, df = 1)
#   #sex_lac_effect ~ dt(mu = 0, sigma = 1, df = 1)
#   
#   
#   
#   for(i in 1:n.obs){
#     
#     #Process Model
#     logit(preg_prob[i])<-Intercept+
#       #(lac_score[i]*lac_effect)+
#       (est_bm[i]*bm_effect)+
#       (dde_m[i]*dde_effect_m)+
#       (age[i]*age_effect)+
#       # (pow(age[i], 2)*age2_effect)+
#       
#       #(sex_correct[i] * sex_effect) +
#       #(lac_time[i]* lac_time_effect)+
#       (time_1_preg[i]* time_1_preg_effect)+
#       # (spei_3[i]*spei_3_effect)+
#       #(spei_6[i]*spei_6_effect)+
#       (spei_12[i]*spei_12_effect)#+
#     #(lac_score[i]*lac_time[i]*lac_score_preg_effect)
#     
#     
#     
#     
#     
#     
#     #Observation Model (Likelihood)
#     preg_status[i] ~ dbern(preg_prob[i])
#     
#     #Impute missing data values
#     lac_score[i] ~ dnorm(mean=0,sd=1)
#     # precip_year_prior[i] ~ dnorm(mean = 0, sd = 1)
#     #dde_95[i] ~ dnorm(mean=0,sd=1)
#      age[i] ~ dnorm(mean=0,sd=1)
#     # est_body_cond[i] ~ dnorm(mean=0,sd=1)
#     est_bm[i] ~ dnorm(mean=0,sd=1)
#     
#     #lac_time[i] ~ dnorm(mean=0,sd=1)##does this matrer
#     #spei_3[i] ~ dnorm(mean = 0, sd = 1)
#     # spei_6[i] ~ dnorm(mean = 0, sd = 1)
#     # spei_12[i] ~ dnorm(mean = 0, sd = 1)
#     
#     
#     
#   }#i
#   
# })#model5
# parameters <- c(#"lac_effect",
#   "bm_effect",
#   "dde_effect_m",
#   "age_effect",
#   # "age2_effect",
#   # "precip_effect",
#   "Intercept", 
#   # "bod_con_effect",
#   # "lac_time_effect",
#   #"sex_effect",
#   "time_1_preg_effect",
#   "logProb_preg_status",#
#   # "spei_3_effect",
#   #"spei_6_effect",
#   "spei_12_effect"#,
#   #"lac_score_preg_effect"#,
#   ##"sex_lac_effect"
# )
# 
# # MCMC Settings
# ni <- 400000
# nt <- 400
# nb <- 200000
# nc <- 3
# 
# nimble.data = list(
#   preg_status = as.numeric(cow_capture_all$time_2_preg) - 1,
#   lac_score = as.numeric((cow_capture_all$lac_score)),
#   est_bm = as.numeric((cow_capture_all$est_bm)),
#   #dde_95 = as.numeric((cow_capture_all$area_DDE_gt_95)),
#   #dde_75 = as.numeric((cow_capture_all$area_DDE_gt_75)),
#   dde_m = as.numeric((cow_capture_all$area_DDE_gt_mean)),
#   age = as.numeric((cow_capture_all$age_estimate_2)),
#   #precip_year_prior = as.numeric((cow_capture_all$total_year_prior)),
#   sex_correct = ifelse(cow_capture_all$sex_correct== "m", 1,0),
#   lac_time= as.numeric(cow_capture_all$lac_time), 
#   time_1_preg= as.numeric(cow_capture_all$time_1_preg)-1,#
#   
#   #spei_3 = as.numeric((cow_capture_all$sept_year_prior_3M)),
#   # spei_6 = as.numeric((cow_capture_all$sept_year_prior_6M)),
#   spei_12 = as.numeric((cow_capture_all$sept_year_prior_12M))
#   
# )
# 
# nimble.constants = list(n.obs = nrow(cow_capture_all))
# 
# 
# mcmc.output_half_m <- nimbleMCMC(code = model_half_m,
#                                  data = nimble.data,
#                                  constants=nimble.constants,
#                                  monitors = parameters,
#                                  niter = ni,
#                                  nburnin = nb,
#                                  nchains = nc,
#                                  thin=nt,
#                                  summary=TRUE,
#                                  samplesAsCodaMCMC = TRUE)
# 
# attach.nimble(mcmc.output_half_m$samples)
# 
# 
# mata_half  <- do.call(rbind, lapply(mcmc.output_half_m$samples, as.matrix))
# lpa_half  <- grep("^logProb_preg_status\\[", colnames(mata_half), value = TRUE)
# lla_half  <- mata_half[, lpa_half, drop = FALSE]
# stopifnot(all(is.finite(lla_half) | (is.infinite(lla_half) & lla_half < 0)))
# loo_half_m <- loo(lla_half)
# 
# 
# 
# attach.nimble(mcmc.output_half_m$samples)
# 
# MCMCtrace(object = mcmc.output_half_m$samples,
#           pdf = FALSE, # no export to PDF
#           ind = TRUE, # separate density lines per chain
#           params = "time_1_preg_effect")
# # Gelman-Rubin diagnostic (AKA RHat or PSRF)
# gelman.diag(mcmc.output_half_m$samples, multivariate = FALSE)
# 
# 
# 
# mcmcplot(mcmc.output_half_m$samples)
# 
# 
# 
# ##how to get effects size
# ##jow to make the differnt charts in PP
# ##how to get time on deck chart
# ##uses zscore to preduct value. Use mean for all the other values
# ##will use for calf age ##what the fuck is a z-score?
# post_mat <- as.matrix(mcmc.output_half_m$samples)
# # 2. Specify the parameters you care about
# params <- c(#"lac_effect",
#   "bm_effect",
#   "dde_effect_m",
#   "age_effect",
#   #"age2_effect",
#   # "precip_effect",
#   "Intercept", 
#   #"bod_con_effect",
#   #"lac_time_effect",
#   #"sex_effect",
#   "time_1_preg_effect",
#   #"spei_3_effect",
#   #"spei_6_effect",
#   "spei_12_effect"#,
#   #"lac_score_preg_effect"#,
#   #"sex_lac_effect"
# )
# 
# # 3. Summarize each one
# summary_df_half_m <- tibble(parameter = params) %>%
#   rowwise() %>%
#   mutate(
#     median   = median(post_mat[, parameter]),
#     lower95  = quantile(post_mat[, parameter], 0.025),
#     upper95  = quantile(post_mat[, parameter], 0.975),
#     # posterior probability effect > 0
#     p_gt_zero = mean(post_mat[, parameter] > 0),
#     # if you expect a negative effect, you can also compute P(<0):
#     p_lt_zero = mean(post_mat[, parameter] < 0)
#   ) %>%
#   ungroup()
# 
# print(summary_df_half_m)
# get_variables(mcmc.output_half_m$samples)
# 
# 
# summary_df_half_m  %>%
#   kbl(digits = 3, caption = "Posterior Summary Statistics for Model bc_2") %>%
#   kable_styling(full_width = FALSE, position = "center")
# 
# # Full posterior distributions:
# 
# 
# 
# mod_13_fig<-mcmc.output_half_m$samples%>%
#   spread_draws(#lac_effect,
#     bm_effect,
#     dde_effect_m,
#     age_effect,
#     #age2_effect,
#     #precip_effect,
#     
#     # bod_con_effect,
#     #lac_time_effect,
#     #sex_effect,
#     #lac_score_preg_effect,
#     time_1_preg_effect,
#     #spei_3_effect,
#     #spei_6_effect, 
#     spei_12_effect
#   ) %>%
#   pivot_longer(-c(.chain, .iteration, .draw),
#                names_to = 'Parameter',
#                values_to = 'Estimate') %>%
#   ggplot(aes(y = Parameter, x = Estimate, fill = Parameter)) +
#   stat_halfeye(show.legend = FALSE) +
#   geom_vline(xintercept = 0, linetype = 2, color = 'black') +
#   scale_fill_manual(values = c(
#     #lac_time_effect = "grey60",  # make precip_effect blue
#     sex_effect = "red",
#     time_1_preg_effect = "grey60",
#     bod_con_effect = "red4",
#     lac_time_effect = "grey60",
#     age_effect = "blue4"
#   )) +
#   theme_minimal()
# mod_13_fig
# 
# ####end model 13#####
# 
# 
# ####half full################mean dde, spei_6
# model_half_m6_l <-  nimbleCode({
#   
#   #Priors
#   Intercept  ~ dnorm(0, sd = 5)
#   
#   #lac_effect~ dt(mu=0, sigma=1, df=1)
#   bm_effect ~ dt(mu=0, sigma=1, df=1)
#   dde_effect_m ~ dt(mu=0, sigma=1, df=1) 
#   age_effect ~ dt(mu=0, sigma=1, df=1)
#   
#    lac_time_effect ~dt(mu=0, sigma=1, df=1)
#   # sex_effect ~ dt(mu=0, sigma=1, df=1)
#   
#   time_1_preg_effect ~ dt(mu=0, sigma=1, df=1)
#   # spei_3_effect ~ dt(mu = 0 , sigma = 1, df = 1)
#   spei_6_effect ~ dt(mu = 0 , sigma = 1, df = 1)
#   #spei_12_effect ~ dt(mu = 0 , sigma = 1, df = 1)
#   # age2_effect ~ dt(mu = 0, sigma = 1, df = 1)
#   #lac_score_preg_effect ~ dt(mu = 0, sigma = 1, df = 1)
#   #sex_lac_effect ~ dt(mu = 0, sigma = 1, df = 1)
#   
#   
#   
#   for(i in 1:n.obs){
#     
#     #Process Model
#     logit(preg_prob[i])<-Intercept+
#       #(lac_score[i]*lac_effect)+
#       (est_bm[i]*bm_effect)+
#       (dde_m[i]*dde_effect_m)+
#       (age[i]*age_effect)+
#       # (pow(age[i], 2)*age2_effect)+
#       
#       #(sex_correct[i] * sex_effect) +
#       (lac_time[i]* lac_time_effect)+
#       (time_1_preg[i]* time_1_preg_effect)+
#       # (spei_3[i]*spei_3_effect)+
#       (spei_6[i]*spei_6_effect)#+
#     #(spei_12[i]*spei_12_effect)#+
#     #(lac_score[i]*lac_time[i]*lac_score_preg_effect)
#     
#     
#     
#     
#     
#     
#     #Observation Model (Likelihood)
#     preg_status[i] ~ dbern(preg_prob[i])
#     
#     #Impute missing data values
#     lac_score[i] ~ dnorm(mean=0,sd=1)
#     # precip_year_prior[i] ~ dnorm(mean = 0, sd = 1)
#     #dde_95[i] ~ dnorm(mean=0,sd=1)
#      age[i] ~ dnorm(mean=0,sd=1)
#     # est_body_cond[i] ~ dnorm(mean=0,sd=1)
#     est_bm[i] ~ dnorm(mean=0,sd=1)
#     
#     #lac_time[i] ~ dnorm(mean=0,sd=1)##does this matrer
#     #spei_3[i] ~ dnorm(mean = 0, sd = 1)
#     # spei_6[i] ~ dnorm(mean = 0, sd = 1)
#     # spei_12[i] ~ dnorm(mean = 0, sd = 1)
#     
#     
#     
#   }#i
#   
# })#model5
# parameters <- c(#"lac_effect",
#   "bm_effect",
#   "dde_effect_m",
#   "age_effect",
#   # "age2_effect",
#   # "precip_effect",
#   "Intercept", 
#   # "bod_con_effect",
#    "lac_time_effect",
#   #"sex_effect",
#   "time_1_preg_effect",
#   "logProb_preg_status",#
#   # "spei_3_effect",
#   "spei_6_effect"#,
#   #"spei_12_effect"#,
#   #"lac_score_preg_effect"#,
#   ##"sex_lac_effect"
# )
# 
# # MCMC Settings
# ni <- 400000
# nt <- 400
# nb <- 200000
# nc <- 3
# 
# nimble.data = list(
#   preg_status = as.numeric(cow_capture_all$time_2_preg) - 1,
#   lac_score = as.numeric((cow_capture_all$lac_score)),
#   est_bm = as.numeric((cow_capture_all$est_bm)),
#   #dde_95 = as.numeric((cow_capture_all$area_DDE_gt_95)),
#   #dde_75 = as.numeric((cow_capture_all$area_DDE_gt_75)),
#   dde_m = as.numeric((cow_capture_all$area_DDE_gt_mean)),
#   age = as.numeric((cow_capture_all$age_estimate_2)),
#   #precip_year_prior = as.numeric((cow_capture_all$total_year_prior)),
#   sex_correct = ifelse(cow_capture_all$sex_correct== "m", 1,0),
#   lac_time= as.numeric(cow_capture_all$lac_time), 
#   time_1_preg= as.numeric(cow_capture_all$time_1_preg)-1,#
#   
#   #spei_3 = as.numeric((cow_capture_all$sept_year_prior_3M)),
#   spei_6 = as.numeric((cow_capture_all$sept_year_prior_6M))#,
#   #spei_12 = as.numeric((cow_capture_all$sept_year_prior_12M))
#   
# )
# 
# nimble.constants = list(n.obs = nrow(cow_capture_all))
# 
# 
# mcmc.output_half_m6_l <- nimbleMCMC(code = model_half_m6_l,
#                                   data = nimble.data,
#                                   constants=nimble.constants,
#                                   monitors = parameters,
#                                   niter = ni,
#                                   nburnin = nb,
#                                   nchains = nc,
#                                   thin=nt,
#                                   summary=TRUE,
#                                   samplesAsCodaMCMC = TRUE)
# 
# attach.nimble(mcmc.output_half_m6_l$samples)
# 
# 
# mata_half  <- do.call(rbind, lapply(mcmc.output_half_m6_l$samples, as.matrix))
# lpa_half  <- grep("^logProb_preg_status\\[", colnames(mata_half), value = TRUE)
# lla_half  <- mata_half[, lpa_half, drop = FALSE]
# stopifnot(all(is.finite(lla_half) | (is.infinite(lla_half) & lla_half < 0)))
# loo_half_m6_l <- loo(lla_half)
# 
# 
# 
# attach.nimble(mcmc.output_half_m6_l$samples)
# 
# MCMCtrace(object = mcmc.output_half_m6_l$samples,
#           pdf = FALSE, # no export to PDF
#           ind = TRUE, # separate density lines per chain
#           params = "time_1_preg_effect")
# # Gelman-Rubin diagnostic (AKA RHat or PSRF)
# gelman.diag(mcmc.output_half_m6$samples, multivariate = FALSE)
# 
# 
# 
# mcmcplot(mcmc.output_half_m6_l$samples)
# 
# 
# 
# ##how to get effects size
# ##jow to make the differnt charts in PP
# ##how to get time on deck chart
# ##uses zscore to preduct value. Use mean for all the other values
# ##will use for calf age ##what the fuck is a z-score?
# post_mat <- as.matrix(mcmc.output_half_m6_l$samples)
# # 2. Specify the parameters you care about
# params <- c(#"lac_effect",
#   "bm_effect",
#   "dde_effect_m",
#   "age_effect",
#   #"age2_effect",
#   # "precip_effect",
#   "Intercept", 
#   #"bod_con_effect",
#   "lac_time_effect",
#   #"sex_effect",
#   "time_1_preg_effect",
#   #"spei_3_effect",
#   "spei_6_effect"#,
#   #"spei_12_effect"#,
#   #"lac_score_preg_effect"#,
#   #"sex_lac_effect"
# )
# 
# # 3. Summarize each one
# summary_df_half_m6_l <- tibble(parameter = params) %>%
#   rowwise() %>%
#   mutate(
#     median   = median(post_mat[, parameter]),
#     lower95  = quantile(post_mat[, parameter], 0.025),
#     upper95  = quantile(post_mat[, parameter], 0.975),
#     # posterior probability effect > 0
#     p_gt_zero = mean(post_mat[, parameter] > 0),
#     # if you expect a negative effect, you can also compute P(<0):
#     p_lt_zero = mean(post_mat[, parameter] < 0)
#   ) %>%
#   ungroup()
# 
# print(summary_df_half_m6_l)
# get_variables(mcmc.output_half_m6_l$samples)
# 
# 
# summary_df_half_m6_l  %>%
#   kbl(digits = 3, caption = "Posterior Summary Statistics for Model bc_2") %>%
#   kable_styling(full_width = FALSE, position = "center")
# 
# # Full posterior distributions:
# 
# 
# 
# mod_14_fig<-mcmc.output_half_m6_l$samples%>%
#   spread_draws(#lac_effect,
#     bm_effect,
#     dde_effect_m,
#     age_effect,
#     #age2_effect,
#     #precip_effect,
#     
#     # bod_con_effect,
#     lac_time_effect,
#     #sex_effect,
#     #lac_score_preg_effect,
#     time_1_preg_effect,
#     #spei_3_effect,
#     spei_6_effect#, 
#     #spei_12_effect
#   ) %>%
#   pivot_longer(-c(.chain, .iteration, .draw),
#                names_to = 'Parameter',
#                values_to = 'Estimate') %>%
#   ggplot(aes(y = Parameter, x = Estimate, fill = Parameter)) +
#   stat_halfeye(show.legend = FALSE) +
#   geom_vline(xintercept = 0, linetype = 2, color = 'black') +
#   scale_fill_manual(values = c(
#     #lac_time_effect = "grey60",  # make precip_effect blue
#     sex_effect = "red",
#     time_1_preg_effect = "grey60",
#     bod_con_effect = "red4",
#     lac_time_effect = "grey60",
#     age_effect = "blue4"
#   )) +
#   theme_minimal()
# mod_14_fig
# 
# ####end model 14#####
# 
# ####half full################mean dde, spei_6
# model_half_m6_ls <-  nimbleCode({
#   
#   #Priors
#   Intercept  ~ dnorm(0, sd = 5)
#   
#   #lac_effect~ dt(mu=0, sigma=1, df=1)
#   bm_effect ~ dt(mu=0, sigma=1, df=1)
#   dde_effect_m ~ dt(mu=0, sigma=1, df=1) 
#   age_effect ~ dt(mu=0, sigma=1, df=1)
#   
#   lac_time_effect ~dt(mu=0, sigma=1, df=1)
#    sex_effect ~ dt(mu=0, sigma=1, df=1)
#   
#   time_1_preg_effect ~ dt(mu=0, sigma=1, df=1)
#    spei_3_effect ~ dt(mu = 0 , sigma = 1, df = 1)
#   spei_6_effect ~ dt(mu = 0 , sigma = 1, df = 1)
#   #spei_12_effect ~ dt(mu = 0 , sigma = 1, df = 1)
#   # age2_effect ~ dt(mu = 0, sigma = 1, df = 1)
#   #lac_score_preg_effect ~ dt(mu = 0, sigma = 1, df = 1)
#   #sex_lac_effect ~ dt(mu = 0, sigma = 1, df = 1)
#   
#   
#   
#   for(i in 1:n.obs){
#     
#     #Process Model
#     logit(preg_prob[i])<-Intercept+
#       #(lac_score[i]*lac_effect)+
#       (est_bm[i]*bm_effect)+
#       (dde_m[i]*dde_effect_m)+
#       (age[i]*age_effect)+
#       # (pow(age[i], 2)*age2_effect)+
#       
#       (sex_correct[i] * sex_effect) +
#       (lac_time[i]* lac_time_effect)+
#       (time_1_preg[i]* time_1_preg_effect)+
#       # (spei_3[i]*spei_3_effect)+
#       (spei_6[i]*spei_6_effect)#+
#     #(spei_12[i]*spei_12_effect)#+
#     #(lac_score[i]*lac_time[i]*lac_score_preg_effect)
#     
#     
#     
#     
#     
#     
#     #Observation Model (Likelihood)
#     preg_status[i] ~ dbern(preg_prob[i])
#     
#     #Impute missing data values
#     lac_score[i] ~ dnorm(mean=0,sd=1)
#     # precip_year_prior[i] ~ dnorm(mean = 0, sd = 1)
#     #dde_95[i] ~ dnorm(mean=0,sd=1)
#      age[i] ~ dnorm(mean=0,sd=1)
#     # est_body_cond[i] ~ dnorm(mean=0,sd=1)
#     est_bm[i] ~ dnorm(mean=0,sd=1)
#     
#     #lac_time[i] ~ dnorm(mean=0,sd=1)##does this matrer
#     #spei_3[i] ~ dnorm(mean = 0, sd = 1)
#     # spei_6[i] ~ dnorm(mean = 0, sd = 1)
#     # spei_12[i] ~ dnorm(mean = 0, sd = 1)
#     
#     
#     
#   }#i
#   
# })#model5
# parameters <- c(#"lac_effect",
#   "bm_effect",
#   "dde_effect_m",
#   "age_effect",
#   # "age2_effect",
#   # "precip_effect",
#   "Intercept", 
#   # "bod_con_effect",
#   "lac_time_effect",
#   "sex_effect",
#   "time_1_preg_effect",
#   "logProb_preg_status",#
#   # "spei_3_effect",
#   "spei_6_effect"#,
#   #"spei_12_effect"#,
#   #"lac_score_preg_effect"#,
#   ##"sex_lac_effect"
# )
# 
# # MCMC Settings
# ni <- 400000
# nt <- 400
# nb <- 200000
# nc <- 3
# 
# nimble.data = list(
#   preg_status = as.numeric(cow_capture_all$time_2_preg) - 1,
#   lac_score = as.numeric((cow_capture_all$lac_score)),
#   est_bm = as.numeric((cow_capture_all$est_bm)),
#   #dde_95 = as.numeric((cow_capture_all$area_DDE_gt_95)),
#   #dde_75 = as.numeric((cow_capture_all$area_DDE_gt_75)),
#   dde_m = as.numeric((cow_capture_all$area_DDE_gt_mean)),
#   age = as.numeric((cow_capture_all$age_estimate_2)),
#   #precip_year_prior = as.numeric((cow_capture_all$total_year_prior)),
#   sex_correct = ifelse(cow_capture_all$sex_correct== "m", 1,0),
#   lac_time= as.numeric(cow_capture_all$lac_time), 
#   time_1_preg= as.numeric(cow_capture_all$time_1_preg)-1,#
#   
#   #spei_3 = as.numeric((cow_capture_all$sept_year_prior_3M)),
#   spei_6 = as.numeric((cow_capture_all$sept_year_prior_6M))#,
#   #spei_12 = as.numeric((cow_capture_all$sept_year_prior_12M))
#   
# )
# 
# nimble.constants = list(n.obs = nrow(cow_capture_all))
# 
# 
# mcmc.output_half_m6_ls <- nimbleMCMC(code = model_half_m6_ls,
#                                     data = nimble.data,
#                                     constants=nimble.constants,
#                                     monitors = parameters,
#                                     niter = ni,
#                                     nburnin = nb,
#                                     nchains = nc,
#                                     thin=nt,
#                                     summary=TRUE,
#                                     samplesAsCodaMCMC = TRUE)
# 
# attach.nimble(mcmc.output_half_m6_ls$samples)
# 
# 
# mata_half  <- do.call(rbind, lapply(mcmc.output_half_m6_ls$samples, as.matrix))
# lpa_half  <- grep("^logProb_preg_status\\[", colnames(mata_half), value = TRUE)
# lla_half  <- mata_half[, lpa_half, drop = FALSE]
# stopifnot(all(is.finite(lla_half) | (is.infinite(lla_half) & lla_half < 0)))
# loo_half_m6_ls <- loo(lla_half)
# 
# 
# 
# attach.nimble(mcmc.output_half_m6_ls$samples)
# 
# MCMCtrace(object = mcmc.output_half_m6_ls$samples,
#           pdf = FALSE, # no export to PDF
#           ind = TRUE, # separate density lines per chain
#           params = "time_1_preg_effect")
# # Gelman-Rubin diagnostic (AKA RHat or PSRF)
# gelman.diag(mcmc.output_half_m6_ls$samples, multivariate = FALSE)
# 
# 
# 
# mcmcplot(mcmc.output_half_m6_ls$samples)
# 
# 
# 
# ##how to get effects size
# ##jow to make the differnt charts in PP
# ##how to get time on deck chart
# ##uses zscore to preduct value. Use mean for all the other values
# ##will use for calf age ##what the fuck is a z-score?
# post_mat <- as.matrix(mcmc.output_half_m6_ls$samples)
# # 2. Specify the parameters you care about
# params <- c(#"lac_effect",
#   "bm_effect",
#   "dde_effect_m",
#   "age_effect",
#   #"age2_effect",
#   # "precip_effect",
#   "Intercept", 
#   #"bod_con_effect",
#   "lac_time_effect",
#   "sex_effect",
#   "time_1_preg_effect",
#   #"spei_3_effect",
#   "spei_6_effect"#,
#   #"spei_12_effect"#,
#   #"lac_score_preg_effect"#,
#   #"sex_lac_effect"
# )
# 
# # 3. Summarize each one
# summary_df_half_m6_ls <- tibble(parameter = params) %>%
#   rowwise() %>%
#   mutate(
#     median   = median(post_mat[, parameter]),
#     lower95  = quantile(post_mat[, parameter], 0.025),
#     upper95  = quantile(post_mat[, parameter], 0.975),
#     # posterior probability effect > 0
#     p_gt_zero = mean(post_mat[, parameter] > 0),
#     # if you expect a negative effect, you can also compute P(<0):
#     p_lt_zero = mean(post_mat[, parameter] < 0)
#   ) %>%
#   ungroup()
# 
# print(summary_df_half_m6_ls)
# get_variables(mcmc.output_half_m6_ls$samples)
# 
# 
# summary_df_half_m6_ls  %>%
#   kbl(digits = 3, caption = "Posterior Summary Statistics for Model bc_2") %>%
#   kable_styling(full_width = FALSE, position = "center")
# 
# # Full posterior distributions:
# 
# 
# 
# mod_15_fig<-mcmc.output_half_m6_ls$samples%>%
#   spread_draws(#lac_effect,
#     bm_effect,
#     dde_effect_m,
#     age_effect,
#     #age2_effect,
#     #precip_effect,
#     
#     # bod_con_effect,
#     lac_time_effect,
#     sex_effect,
#     #lac_score_preg_effect,
#     time_1_preg_effect,
#     #spei_3_effect,
#     spei_6_effect#, 
#     #spei_12_effect
#   ) %>%
#   pivot_longer(-c(.chain, .iteration, .draw),
#                names_to = 'Parameter',
#                values_to = 'Estimate') %>%
#   ggplot(aes(y = Parameter, x = Estimate, fill = Parameter)) +
#   stat_halfeye(show.legend = FALSE) +
#   geom_vline(xintercept = 0, linetype = 2, color = 'black') +
#   scale_fill_manual(values = c(
#     #lac_time_effect = "grey60",  # make precip_effect blue
#     sex_effect = "red",
#     time_1_preg_effect = "grey60",
#     bod_con_effect = "red4",
#     lac_time_effect = "grey60",
#     age_effect = "blue4"
#   )) +
#   theme_minimal()
# mod_15_fig
# 
# ####end model 15#####
# 
# ####half full################mean dde, spei_6
# model_half_m6_ls_nbm <-  nimbleCode({
#   
#   #Priors
#   Intercept  ~ dnorm(0, sd = 5)
#   
#   #lac_effect~ dt(mu=0, sigma=1, df=1)
#   #bm_effect ~ dt(mu=0, sigma=1, df=1)
#   dde_effect_m ~ dt(mu=0, sigma=1, df=1) 
#   age_effect ~ dt(mu=0, sigma=1, df=1)
#   
#   lac_time_effect ~dt(mu=0, sigma=1, df=1)
#   sex_effect ~ dt(mu=0, sigma=1, df=1)
#   
#   time_1_preg_effect ~ dt(mu=0, sigma=1, df=1)
#   spei_3_effect ~ dt(mu = 0 , sigma = 1, df = 1)
#   spei_6_effect ~ dt(mu = 0 , sigma = 1, df = 1)
#   #spei_12_effect ~ dt(mu = 0 , sigma = 1, df = 1)
#   # age2_effect ~ dt(mu = 0, sigma = 1, df = 1)
#   #lac_score_preg_effect ~ dt(mu = 0, sigma = 1, df = 1)
#   #sex_lac_effect ~ dt(mu = 0, sigma = 1, df = 1)
#   
#   
#   
#   for(i in 1:n.obs){
#     
#     #Process Model
#     logit(preg_prob[i])<-Intercept+
#       #(lac_score[i]*lac_effect)+
#       #(est_bm[i]*bm_effect)+
#       (dde_m[i]*dde_effect_m)+
#       (age[i]*age_effect)+
#       # (pow(age[i], 2)*age2_effect)+
#       
#       (sex_correct[i] * sex_effect) +
#       (lac_time[i]* lac_time_effect)+
#       (time_1_preg[i]* time_1_preg_effect)+
#       # (spei_3[i]*spei_3_effect)+
#       (spei_6[i]*spei_6_effect)#+
#     #(spei_12[i]*spei_12_effect)#+
#     #(lac_score[i]*lac_time[i]*lac_score_preg_effect)
#     
#     
#     
#     
#     
#     
#     #Observation Model (Likelihood)
#     preg_status[i] ~ dbern(preg_prob[i])
#     
#     #Impute missing data values
#     lac_score[i] ~ dnorm(mean=0,sd=1)
#     # precip_year_prior[i] ~ dnorm(mean = 0, sd = 1)
#     #dde_95[i] ~ dnorm(mean=0,sd=1)
#      age[i] ~ dnorm(mean=0,sd=1)
#     # est_body_cond[i] ~ dnorm(mean=0,sd=1)
#     est_bm[i] ~ dnorm(mean=0,sd=1)
#     
#     #lac_time[i] ~ dnorm(mean=0,sd=1)##does this matrer
#     #spei_3[i] ~ dnorm(mean = 0, sd = 1)
#     # spei_6[i] ~ dnorm(mean = 0, sd = 1)
#     # spei_12[i] ~ dnorm(mean = 0, sd = 1)
#     
#     
#     
#   }#i
#   
# })#model5
# parameters <- c(#"lac_effect",
#  # "bm_effect",
#   "dde_effect_m",
#   "age_effect",
#   # "age2_effect",
#   # "precip_effect",
#   "Intercept", 
#   # "bod_con_effect",
#   "lac_time_effect",
#   "sex_effect",
#   "time_1_preg_effect",
#   "logProb_preg_status",#
#   # "spei_3_effect",
#   "spei_6_effect"#,
#   #"spei_12_effect"#,
#   #"lac_score_preg_effect"#,
#   ##"sex_lac_effect"
# )
# 
# # MCMC Settings
# ni <- 400000
# nt <- 400
# nb <- 200000
# nc <- 3
# 
# nimble.data = list(
#   preg_status = as.numeric(cow_capture_all$time_2_preg) - 1,
#   lac_score = as.numeric((cow_capture_all$lac_score)),
#   #est_bm = as.numeric((cow_capture_all$est_bm)),
#   #dde_95 = as.numeric((cow_capture_all$area_DDE_gt_95)),
#   #dde_75 = as.numeric((cow_capture_all$area_DDE_gt_75)),
#   dde_m = as.numeric((cow_capture_all$area_DDE_gt_mean)),
#   age = as.numeric((cow_capture_all$age_estimate_2)),
#   #precip_year_prior = as.numeric((cow_capture_all$total_year_prior)),
#   sex_correct = ifelse(cow_capture_all$sex_correct== "m", 1,0),
#   lac_time= as.numeric(cow_capture_all$lac_time), 
#   time_1_preg= as.numeric(cow_capture_all$time_1_preg)-1,#
#   
#   #spei_3 = as.numeric((cow_capture_all$sept_year_prior_3M)),
#   spei_6 = as.numeric((cow_capture_all$sept_year_prior_6M))#,
#   #spei_12 = as.numeric((cow_capture_all$sept_year_prior_12M))
#   
# )
# 
# nimble.constants = list(n.obs = nrow(cow_capture_all))
# 
# 
# mcmc.output_half_m6_ls_nbm <- nimbleMCMC(code = model_half_m6_ls_nbm,
#                                      data = nimble.data,
#                                      constants=nimble.constants,
#                                      monitors = parameters,
#                                      niter = ni,
#                                      nburnin = nb,
#                                      nchains = nc,
#                                      thin=nt,
#                                      summary=TRUE,
#                                      samplesAsCodaMCMC = TRUE)
# 
# attach.nimble(mcmc.output_half_m6_ls_nbm$samples)
# 
# 
# mata_half  <- do.call(rbind, lapply(mcmc.output_half_m6_ls_nbm$samples, as.matrix))
# lpa_half  <- grep("^logProb_preg_status\\[", colnames(mata_half), value = TRUE)
# lla_half  <- mata_half[, lpa_half, drop = FALSE]
# stopifnot(all(is.finite(lla_half) | (is.infinite(lla_half) & lla_half < 0)))
# loo_half_m6_ls_bm <- loo(lla_half)
# 
# 
# 
# attach.nimble(mcmc.output_half_m6_ls_nbm$samples)
# 
# MCMCtrace(object = mcmc.output_half_m6_ls_nbm$samples,
#           pdf = FALSE, # no export to PDF
#           ind = TRUE, # separate density lines per chain
#           params = "time_1_preg_effect")
# # Gelman-Rubin diagnostic (AKA RHat or PSRF)
# gelman.diag(mcmc.output_half_m6_ls_nbm$samples, multivariate = FALSE)
# 
# 
# 
# mcmcplot(mcmc.output_half_m6_ls_nbm$samples)
# 
# 
# 
# ##how to get effects size
# ##jow to make the differnt charts in PP
# ##how to get time on deck chart
# ##uses zscore to preduct value. Use mean for all the other values
# ##will use for calf age ##what the fuck is a z-score?
# post_mat <- as.matrix(mcmc.output_half_m6_ls_nbm$samples)
# # 2. Specify the parameters you care about
# params <- c(#"lac_effect",
#   #"bm_effect",
#   "dde_effect_m",
#   "age_effect",
#   #"age2_effect",
#   # "precip_effect",
#   "Intercept", 
#   #"bod_con_effect",
#   "lac_time_effect",
#   "sex_effect",
#   "time_1_preg_effect",
#   #"spei_3_effect",
#   "spei_6_effect"#,
#   #"spei_12_effect"#,
#   #"lac_score_preg_effect"#,
#   #"sex_lac_effect"
# )
# 
# # 3. Summarize each one
# summary_df_half_m6_ls_bm <- tibble(parameter = params) %>%
#   rowwise() %>%
#   mutate(
#     median   = median(post_mat[, parameter]),
#     lower95  = quantile(post_mat[, parameter], 0.025),
#     upper95  = quantile(post_mat[, parameter], 0.975),
#     # posterior probability effect > 0
#     p_gt_zero = mean(post_mat[, parameter] > 0),
#     # if you expect a negative effect, you can also compute P(<0):
#     p_lt_zero = mean(post_mat[, parameter] < 0)
#   ) %>%
#   ungroup()
# 
# print(summary_df_half_m6_ls_bm)
# get_variables(mcmc.output_half_m6_ls_nbm$samples)
# 
# 
# summary_df_half_m6_ls_bm  %>%
#   kbl(digits = 3, caption = "Posterior Summary Statistics for Model bc_2") %>%
#   kable_styling(full_width = FALSE, position = "center")
# 
# # Full posterior distributions:
# 
# 
# 
# mod_16_fig<-mcmc.output_half_m6_ls_nbm$samples%>%
#   spread_draws(#lac_effect,
#    # bm_effect,
#     dde_effect_m,
#     age_effect,
#     #age2_effect,
#     #precip_effect,
#     
#     # bod_con_effect,
#     lac_time_effect,
#     sex_effect,
#     #lac_score_preg_effect,
#     time_1_preg_effect,
#     #spei_3_effect,
#     spei_6_effect#, 
#     #spei_12_effect
#   ) %>%
#   pivot_longer(-c(.chain, .iteration, .draw),
#                names_to = 'Parameter',
#                values_to = 'Estimate') %>%
#   ggplot(aes(y = Parameter, x = Estimate, fill = Parameter)) +
#   stat_halfeye(show.legend = FALSE) +
#   geom_vline(xintercept = 0, linetype = 2, color = 'black') +
#   scale_fill_manual(values = c(
#     #lac_time_effect = "grey60",  # make precip_effect blue
#     sex_effect = "red",
#     time_1_preg_effect = "grey60",
#     bod_con_effect = "red4",
#     lac_time_effect = "grey60",
#     age_effect = "blue4"
#   )) +
#   theme_minimal()
# mod_16_fig
# 
# ####end model 16#####
# 
# 
# ####half full################mean dde, spei_6
# model_half_m6_ls_nbm_a2 <-  nimbleCode({
#   
#   #Priors
#   Intercept  ~ dnorm(0, sd = 5)
#   
#   #lac_effect~ dt(mu=0, sigma=1, df=1)
#   #bm_effect ~ dt(mu=0, sigma=1, df=1)
#   dde_effect_m ~ dt(mu=0, sigma=1, df=1) 
#   age_effect ~ dt(mu=0, sigma=1, df=1)
#   
#   lac_time_effect ~dt(mu=0, sigma=1, df=1)
#   sex_effect ~ dt(mu=0, sigma=1, df=1)
#   
#   time_1_preg_effect ~ dt(mu=0, sigma=1, df=1)
#   spei_3_effect ~ dt(mu = 0 , sigma = 1, df = 1)
#   spei_6_effect ~ dt(mu = 0 , sigma = 1, df = 1)
#   #spei_12_effect ~ dt(mu = 0 , sigma = 1, df = 1)
#    age2_effect ~ dt(mu = 0, sigma = 1, df = 1)
#   #lac_score_preg_effect ~ dt(mu = 0, sigma = 1, df = 1)
#   #sex_lac_effect ~ dt(mu = 0, sigma = 1, df = 1)
#   
#   
#   
#   for(i in 1:n.obs){
#     
#     #Process Model
#     logit(preg_prob[i])<-Intercept+
#       #(lac_score[i]*lac_effect)+
#       #(est_bm[i]*bm_effect)+
#       (dde_m[i]*dde_effect_m)+
#       (age[i]*age_effect)+
#        (pow(age[i], 2)*age2_effect)+
#       
#       (sex_correct[i] * sex_effect) +
#       (lac_time[i]* lac_time_effect)+
#       (time_1_preg[i]* time_1_preg_effect)+
#       # (spei_3[i]*spei_3_effect)+
#       (spei_6[i]*spei_6_effect)#+
#     #(spei_12[i]*spei_12_effect)#+
#     #(lac_score[i]*lac_time[i]*lac_score_preg_effect)
#     
#     
#     
#     
#     
#     
#     #Observation Model (Likelihood)
#     preg_status[i] ~ dbern(preg_prob[i])
#     
#     #Impute missing data values
#     lac_score[i] ~ dnorm(mean=0,sd=1)
#     # precip_year_prior[i] ~ dnorm(mean = 0, sd = 1)
#     #dde_95[i] ~ dnorm(mean=0,sd=1)
#      age[i] ~ dnorm(mean=0,sd=1)
#     # est_body_cond[i] ~ dnorm(mean=0,sd=1)
#     est_bm[i] ~ dnorm(mean=0,sd=1)
#     
#     #lac_time[i] ~ dnorm(mean=0,sd=1)##does this matrer
#     #spei_3[i] ~ dnorm(mean = 0, sd = 1)
#     # spei_6[i] ~ dnorm(mean = 0, sd = 1)
#     # spei_12[i] ~ dnorm(mean = 0, sd = 1)
#     
#     
#     
#   }#i
#   
# })#model5
# parameters <- c(#"lac_effect",
#   # "bm_effect",
#   "dde_effect_m",
#   "age_effect",
#    "age2_effect",
#   # "precip_effect",
#   "Intercept", 
#   # "bod_con_effect",
#   "lac_time_effect",
#   "sex_effect",
#   "time_1_preg_effect",
#   "logProb_preg_status",#
#   # "spei_3_effect",
#   "spei_6_effect"#,
#   #"spei_12_effect"#,
#   #"lac_score_preg_effect"#,
#   ##"sex_lac_effect"
# )
# 
# # MCMC Settings
# ni <- 400000
# nt <- 400
# nb <- 200000
# nc <- 3
# 
# nimble.data = list(
#   preg_status = as.numeric(cow_capture_all$time_2_preg) - 1,
#   lac_score = as.numeric((cow_capture_all$lac_score)),
#   #est_bm = as.numeric((cow_capture_all$est_bm)),
#   #dde_95 = as.numeric((cow_capture_all$area_DDE_gt_95)),
#   #dde_75 = as.numeric((cow_capture_all$area_DDE_gt_75)),
#   dde_m = as.numeric((cow_capture_all$area_DDE_gt_mean)),
#   age = as.numeric((cow_capture_all$age_estimate_2)),
#   #precip_year_prior = as.numeric((cow_capture_all$total_year_prior)),
#   sex_correct = ifelse(cow_capture_all$sex_correct== "m", 1,0),
#   lac_time= as.numeric(cow_capture_all$lac_time), 
#   time_1_preg= as.numeric(cow_capture_all$time_1_preg)-1,#
#   
#   #spei_3 = as.numeric((cow_capture_all$sept_year_prior_3M)),
#   spei_6 = as.numeric((cow_capture_all$sept_year_prior_6M))#,
#   #spei_12 = as.numeric((cow_capture_all$sept_year_prior_12M))
#   
# )
# 
# nimble.constants = list(n.obs = nrow(cow_capture_all))
# 
# 
# mcmc.output_half_m6_ls_nbm_a2 <- nimbleMCMC(code = model_half_m6_ls_nbm_a2,
#                                          data = nimble.data,
#                                          constants=nimble.constants,
#                                          monitors = parameters,
#                                          niter = ni,
#                                          nburnin = nb,
#                                          nchains = nc,
#                                          thin=nt,
#                                          summary=TRUE,
#                                          samplesAsCodaMCMC = TRUE)
# 
# attach.nimble(mcmc.output_half_m6_ls_nbm_a2$samples)
# 
# 
# mata_half  <- do.call(rbind, lapply(mcmc.output_half_m6_ls_nbm_a2$samples, as.matrix))
# lpa_half  <- grep("^logProb_preg_status\\[", colnames(mata_half), value = TRUE)
# lla_half  <- mata_half[, lpa_half, drop = FALSE]
# stopifnot(all(is.finite(lla_half) | (is.infinite(lla_half) & lla_half < 0)))
# loo_half_m6_ls_bm_a2 <- loo(lla_half)
# 
# 
# 
# attach.nimble(mcmc.output_half_m6_ls_nbm_a2$samples)
# 
# MCMCtrace(object = mcmc.output_half_m6_ls_nbm_a2$samples,
#           pdf = FALSE, # no export to PDF
#           ind = TRUE, # separate density lines per chain
#           params = "time_1_preg_effect")
# # Gelman-Rubin diagnostic (AKA RHat or PSRF)
# gelman.diag(mcmc.output_half_m6_ls_nbm_a2$samples, multivariate = FALSE)
# 
# 
# 
# mcmcplot(mcmc.output_half_m6_ls_nbm_a2$samples)
# 
# 
# 
# ##how to get effects size
# ##jow to make the differnt charts in PP
# ##how to get time on deck chart
# ##uses zscore to preduct value. Use mean for all the other values
# ##will use for calf age ##what the fuck is a z-score?
# post_mat <- as.matrix(mcmc.output_half_m6_ls_nbm_a2$samples)
# # 2. Specify the parameters you care about
# params <- c(#"lac_effect",
#   #"bm_effect",
#   "dde_effect_m",
#   "age_effect",
#   "age2_effect",
#   # "precip_effect",
#   "Intercept", 
#   #"bod_con_effect",
#   "lac_time_effect",
#   "sex_effect",
#   "time_1_preg_effect",
#   #"spei_3_effect",
#   "spei_6_effect"#,
#   #"spei_12_effect"#,
#   #"lac_score_preg_effect"#,
#   #"sex_lac_effect"
# )
# 
# # 3. Summarize each one
# summary_df_half_m6_ls_bm_a2 <- tibble(parameter = params) %>%
#   rowwise() %>%
#   mutate(
#     median   = median(post_mat[, parameter]),
#     lower95  = quantile(post_mat[, parameter], 0.025),
#     upper95  = quantile(post_mat[, parameter], 0.975),
#     # posterior probability effect > 0
#     p_gt_zero = mean(post_mat[, parameter] > 0),
#     # if you expect a negative effect, you can also compute P(<0):
#     p_lt_zero = mean(post_mat[, parameter] < 0)
#   ) %>%
#   ungroup()
# 
# print(summary_df_half_m6_ls_bm_a2)
# get_variables(mcmc.output_half_m6_ls_nbm_a2$samples)
# 
# 
# summary_df_half_m6_ls_bm_a2  %>%
#   kbl(digits = 3, caption = "Posterior Summary Statistics for Model bc_2") %>%
#   kable_styling(full_width = FALSE, position = "center")
# 
# # Full posterior distributions:
# 
# 
# 
# mod_17_fig<-mcmc.output_half_m6_ls_nbm_a2$samples%>%
#   spread_draws(#lac_effect,
#     # bm_effect,
#     dde_effect_m,
#     age_effect,
#     age2_effect,
#     #precip_effect,
#     
#     # bod_con_effect,
#     lac_time_effect,
#     sex_effect,
#     #lac_score_preg_effect,
#     time_1_preg_effect,
#     #spei_3_effect,
#     spei_6_effect#, 
#     #spei_12_effect
#   ) %>%
#   pivot_longer(-c(.chain, .iteration, .draw),
#                names_to = 'Parameter',
#                values_to = 'Estimate') %>%
#   ggplot(aes(y = Parameter, x = Estimate, fill = Parameter)) +
#   stat_halfeye(show.legend = FALSE) +
#   geom_vline(xintercept = 0, linetype = 2, color = 'black') +
#   scale_fill_manual(values = c(
#     #lac_time_effect = "grey60",  # make precip_effect blue
#     sex_effect = "red",
#     time_1_preg_effect = "grey60",
#     bod_con_effect = "red4",
#     lac_time_effect = "grey60",
#     age_effect = "blue4"
#   )) +
#   theme_minimal()
# mod_17_fig
# 
# ####end model 17#####
# 
# 
# ####half full################mean dde, spei_6
# model_half_m6_a2 <-  nimbleCode({
#   
#   #Priors
#   Intercept  ~ dnorm(0, sd = 5)
#   
#   #lac_effect~ dt(mu=0, sigma=1, df=1)
#   bm_effect ~ dt(mu=0, sigma=1, df=1)
#   dde_effect_m ~ dt(mu=0, sigma=1, df=1) 
#   age_effect ~ dt(mu=0, sigma=1, df=1)
#   
#   # lac_time_effect ~dt(mu=0, sigma=1, df=1)
#   # sex_effect ~ dt(mu=0, sigma=1, df=1)
#   
#   time_1_preg_effect ~ dt(mu=0, sigma=1, df=1)
#   # spei_3_effect ~ dt(mu = 0 , sigma = 1, df = 1)
#   spei_6_effect ~ dt(mu = 0 , sigma = 1, df = 1)
#   #spei_12_effect ~ dt(mu = 0 , sigma = 1, df = 1)
#    age2_effect ~ dt(mu = 0, sigma = 1, df = 1)
#   #lac_score_preg_effect ~ dt(mu = 0, sigma = 1, df = 1)
#   #sex_lac_effect ~ dt(mu = 0, sigma = 1, df = 1)
#   
#   
#   
#   for(i in 1:n.obs){
#     
#     #Process Model
#     logit(preg_prob[i])<-Intercept+
#       #(lac_score[i]*lac_effect)+
#       (est_bm[i]*bm_effect)+
#       (dde_m[i]*dde_effect_m)+
#       (age[i]*age_effect)+
#        (pow(age[i], 2)*age2_effect)+
#       
#       #(sex_correct[i] * sex_effect) +
#       #(lac_time[i]* lac_time_effect)+
#       (time_1_preg[i]* time_1_preg_effect)+
#       # (spei_3[i]*spei_3_effect)+
#       (spei_6[i]*spei_6_effect)#+
#     #(spei_12[i]*spei_12_effect)#+
#     #(lac_score[i]*lac_time[i]*lac_score_preg_effect)
#     
#     
#     
#     
#     
#     
#     #Observation Model (Likelihood)
#     preg_status[i] ~ dbern(preg_prob[i])
#     
#     #Impute missing data values
#     lac_score[i] ~ dnorm(mean=0,sd=1)
#     # precip_year_prior[i] ~ dnorm(mean = 0, sd = 1)
#     #dde_95[i] ~ dnorm(mean=0,sd=1)
#      age[i] ~ dnorm(mean=0,sd=1)
#     # est_body_cond[i] ~ dnorm(mean=0,sd=1)
#     est_bm[i] ~ dnorm(mean=0,sd=1)
#     
#     #lac_time[i] ~ dnorm(mean=0,sd=1)##does this matrer
#     #spei_3[i] ~ dnorm(mean = 0, sd = 1)
#     # spei_6[i] ~ dnorm(mean = 0, sd = 1)
#     # spei_12[i] ~ dnorm(mean = 0, sd = 1)
#     
#     
#     
#   }#i
#   
# })#model5
# parameters <- c(#"lac_effect",
#   "bm_effect",
#   "dde_effect_m",
#   "age_effect",
#    "age2_effect",
#   # "precip_effect",
#   "Intercept", 
#   # "bod_con_effect",
#   # "lac_time_effect",
#   #"sex_effect",
#   "time_1_preg_effect",
#   "logProb_preg_status",#
#   # "spei_3_effect",
#   "spei_6_effect"#,
#   #"spei_12_effect"#,
#   #"lac_score_preg_effect"#,
#   ##"sex_lac_effect"
# )
# 
# # MCMC Settings
# ni <- 400000
# nt <- 400
# nb <- 200000
# nc <- 3
# 
# nimble.data = list(
#   preg_status = as.numeric(cow_capture_all$time_2_preg) - 1,
#   lac_score = as.numeric((cow_capture_all$lac_score)),
#   est_bm = as.numeric((cow_capture_all$est_bm)),
#   #dde_95 = as.numeric((cow_capture_all$area_DDE_gt_95)),
#   #dde_75 = as.numeric((cow_capture_all$area_DDE_gt_75)),
#   dde_m = as.numeric((cow_capture_all$area_DDE_gt_mean)),
#   age = as.numeric((cow_capture_all$age_estimate_2)),
#   #precip_year_prior = as.numeric((cow_capture_all$total_year_prior)),
#   sex_correct = ifelse(cow_capture_all$sex_correct== "m", 1,0),
#   lac_time= as.numeric(cow_capture_all$lac_time), 
#   time_1_preg= as.numeric(cow_capture_all$time_1_preg)-1,#
#   
#   #spei_3 = as.numeric((cow_capture_all$sept_year_prior_3M)),
#   spei_6 = as.numeric((cow_capture_all$sept_year_prior_6M))#,
#   #spei_12 = as.numeric((cow_capture_all$sept_year_prior_12M))
#   
# )
# 
# nimble.constants = list(n.obs = nrow(cow_capture_all))
# 
# 
# mcmc.output_half_m6_a2 <- nimbleMCMC(code = model_half_m6_a2,
#                                   data = nimble.data,
#                                   constants=nimble.constants,
#                                   monitors = parameters,
#                                   niter = ni,
#                                   nburnin = nb,
#                                   nchains = nc,
#                                   thin=nt,
#                                   summary=TRUE,
#                                   samplesAsCodaMCMC = TRUE)
# 
# attach.nimble(mcmc.output_half_m6_a2$samples)
# 
# 
# mata_half  <- do.call(rbind, lapply(mcmc.output_half_m6_a2$samples, as.matrix))
# lpa_half  <- grep("^logProb_preg_status\\[", colnames(mata_half), value = TRUE)
# lla_half  <- mata_half[, lpa_half, drop = FALSE]
# stopifnot(all(is.finite(lla_half) | (is.infinite(lla_half) & lla_half < 0)))
# loo_half_m6_a2 <- loo(lla_half)
# 
# 
# 
# attach.nimble(mcmc.output_half_m6_a2$samples)
# 
# MCMCtrace(object = mcmc.output_half_m6_a2$samples,
#           pdf = FALSE, # no export to PDF
#           ind = TRUE, # separate density lines per chain
#           params = "time_1_preg_effect")
# # Gelman-Rubin diagnostic (AKA RHat or PSRF)
# gelman.diag(mcmc.output_half_m6_a2$samples, multivariate = FALSE)
# 
# 
# 
# mcmcplot(mcmc.output_half_m6_a2$samples)
# 
# 
# 
# ##how to get effects size
# ##jow to make the differnt charts in PP
# ##how to get time on deck chart
# ##uses zscore to preduct value. Use mean for all the other values
# ##will use for calf age ##what the fuck is a z-score?
# post_mat <- as.matrix(mcmc.output_half_m6_a2$samples)
# # 2. Specify the parameters you care about
# params <- c(#"lac_effect",
#   "bm_effect",
#   "dde_effect_m",
#   "age_effect",
#   "age2_effect",
#   # "precip_effect",
#   "Intercept", 
#   #"bod_con_effect",
#   #"lac_time_effect",
#   #"sex_effect",
#   "time_1_preg_effect",
#   #"spei_3_effect",
#   "spei_6_effect"#,
#   #"spei_12_effect"#,
#   #"lac_score_preg_effect"#,
#   #"sex_lac_effect"
# )
# 
# # 3. Summarize each one
# summary_df_half_m6_a2 <- tibble(parameter = params) %>%
#   rowwise() %>%
#   mutate(
#     median   = median(post_mat[, parameter]),
#     lower95  = quantile(post_mat[, parameter], 0.025),
#     upper95  = quantile(post_mat[, parameter], 0.975),
#     # posterior probability effect > 0
#     p_gt_zero = mean(post_mat[, parameter] > 0),
#     # if you expect a negative effect, you can also compute P(<0):
#     p_lt_zero = mean(post_mat[, parameter] < 0)
#   ) %>%
#   ungroup()
# 
# print(summary_df_half_m6_a2)
# get_variables(mcmc.output_half_m6_a2$samples)
# 
# 
# summary_df_half_m6_a2  %>%
#   kbl(digits = 3, caption = "Posterior Summary Statistics for Model bc_2") %>%
#   kable_styling(full_width = FALSE, position = "center")
# 
# # Full posterior distributions:
# 
# 
# 
# mod_18_fig<-mcmc.output_half_m6_a2$samples%>%
#   spread_draws(#lac_effect,
#     bm_effect,
#     dde_effect_m,
#     age_effect,
#     age2_effect,
#     #precip_effect,
#     
#     # bod_con_effect,
#     #lac_time_effect,
#     #sex_effect,
#     #lac_score_preg_effect,
#     time_1_preg_effect,
#     #spei_3_effect,
#     spei_6_effect#, 
#     #spei_12_effect
#   ) %>%
#   pivot_longer(-c(.chain, .iteration, .draw),
#                names_to = 'Parameter',
#                values_to = 'Estimate') %>%
#   ggplot(aes(y = Parameter, x = Estimate, fill = Parameter)) +
#   stat_halfeye(show.legend = FALSE) +
#   geom_vline(xintercept = 0, linetype = 2, color = 'black') +
#   scale_fill_manual(values = c(
#     #lac_time_effect = "grey60",  # make precip_effect blue
#     sex_effect = "red",
#     time_1_preg_effect = "grey60",
#     bod_con_effect = "red4",
#     lac_time_effect = "grey60",
#     age_effect = "blue4"
#   )) +
#   theme_minimal()
# mod_18_fig
# 
# ##########################end model 18
# 
# 
# ####half full################mean dde, spei_6
# model_half_m6_s <-  nimbleCode({
#   
#   #Priors
#   Intercept  ~ dnorm(0, sd = 5)
#   
#   #lac_effect~ dt(mu=0, sigma=1, df=1)
#   bm_effect ~ dt(mu=0, sigma=1, df=1)
#   dde_effect_m ~ dt(mu=0, sigma=1, df=1) 
#   age_effect ~ dt(mu=0, sigma=1, df=1)
#   
#   # lac_time_effect ~dt(mu=0, sigma=1, df=1)
#    sex_effect ~ dt(mu=0, sigma=1, df=1)
#   
#   time_1_preg_effect ~ dt(mu=0, sigma=1, df=1)
#   # spei_3_effect ~ dt(mu = 0 , sigma = 1, df = 1)
#   spei_6_effect ~ dt(mu = 0 , sigma = 1, df = 1)
#   #spei_12_effect ~ dt(mu = 0 , sigma = 1, df = 1)
#   age2_effect ~ dt(mu = 0, sigma = 1, df = 1)
#   #lac_score_preg_effect ~ dt(mu = 0, sigma = 1, df = 1)
#   #sex_lac_effect ~ dt(mu = 0, sigma = 1, df = 1)
#   
#   
#   
#   for(i in 1:n.obs){
#     
#     #Process Model
#     logit(preg_prob[i])<-Intercept+
#       #(lac_score[i]*lac_effect)+
#       (est_bm[i]*bm_effect)+
#       (dde_m[i]*dde_effect_m)+
#       (age[i]*age_effect)+
#       #(pow(age[i], 2)*age2_effect)+
#       
#       (sex_correct[i] * sex_effect) +
#       #(lac_time[i]* lac_time_effect)+
#       (time_1_preg[i]* time_1_preg_effect)+
#       # (spei_3[i]*spei_3_effect)+
#       (spei_6[i]*spei_6_effect)#+
#     #(spei_12[i]*spei_12_effect)#+
#     #(lac_score[i]*lac_time[i]*lac_score_preg_effect)
#     
#     
#     
#     
#     
#     
#     #Observation Model (Likelihood)
#     preg_status[i] ~ dbern(preg_prob[i])
#     
#     #Impute missing data values
#     lac_score[i] ~ dnorm(mean=0,sd=1)
#     # precip_year_prior[i] ~ dnorm(mean = 0, sd = 1)
#     #dde_95[i] ~ dnorm(mean=0,sd=1)
#      age[i] ~ dnorm(mean=0,sd=1)
#     # est_body_cond[i] ~ dnorm(mean=0,sd=1)
#     est_bm[i] ~ dnorm(mean=0,sd=1)
#     
#     #lac_time[i] ~ dnorm(mean=0,sd=1)##does this matrer
#     #spei_3[i] ~ dnorm(mean = 0, sd = 1)
#     # spei_6[i] ~ dnorm(mean = 0, sd = 1)
#     # spei_12[i] ~ dnorm(mean = 0, sd = 1)
#     
#     
#     
#   }#i
#   
# })#model5
# parameters <- c(#"lac_effect",
#   "bm_effect",
#   "dde_effect_m",
#   "age_effect",
#   #"age2_effect",
#   # "precip_effect",
#   "Intercept", 
#   # "bod_con_effect",
#   # "lac_time_effect",
#   "sex_effect",
#   "time_1_preg_effect",
#   "logProb_preg_status",#
#   # "spei_3_effect",
#   "spei_6_effect"#,
#   #"spei_12_effect"#,
#   #"lac_score_preg_effect"#,
#   ##"sex_lac_effect"
# )
# 
# # MCMC Settings
# ni <- 400000
# nt <- 400
# nb <- 200000
# nc <- 3
# 
# nimble.data = list(
#   preg_status = as.numeric(cow_capture_all$time_2_preg) - 1,
#   lac_score = as.numeric((cow_capture_all$lac_score)),
#   est_bm = as.numeric((cow_capture_all$est_bm)),
#   #dde_95 = as.numeric((cow_capture_all$area_DDE_gt_95)),
#   #dde_75 = as.numeric((cow_capture_all$area_DDE_gt_75)),
#   dde_m = as.numeric((cow_capture_all$area_DDE_gt_mean)),
#   age = as.numeric((cow_capture_all$age_estimate_2)),
#   #precip_year_prior = as.numeric((cow_capture_all$total_year_prior)),
#   sex_correct = ifelse(cow_capture_all$sex_correct== "m", 1,0),
#   lac_time= as.numeric(cow_capture_all$lac_time), 
#   time_1_preg= as.numeric(cow_capture_all$time_1_preg)-1,#
#   
#   #spei_3 = as.numeric((cow_capture_all$sept_year_prior_3M)),
#   spei_6 = as.numeric((cow_capture_all$sept_year_prior_6M))#,
#   #spei_12 = as.numeric((cow_capture_all$sept_year_prior_12M))
#   
# )
# 
# nimble.constants = list(n.obs = nrow(cow_capture_all))
# 
# 
# mcmc.output_half_m6_s <- nimbleMCMC(code = model_half_m6_s,
#                                      data = nimble.data,
#                                      constants=nimble.constants,
#                                      monitors = parameters,
#                                      niter = ni,
#                                      nburnin = nb,
#                                      nchains = nc,
#                                      thin=nt,
#                                      summary=TRUE,
#                                      samplesAsCodaMCMC = TRUE)
# 
# attach.nimble(mcmc.output_half_m6_s$samples)
# 
# 
# mata_half  <- do.call(rbind, lapply(mcmc.output_half_m6_s$samples, as.matrix))
# lpa_half  <- grep("^logProb_preg_status\\[", colnames(mata_half), value = TRUE)
# lla_half  <- mata_half[, lpa_half, drop = FALSE]
# stopifnot(all(is.finite(lla_half) | (is.infinite(lla_half) & lla_half < 0)))
# loo_half_m6_s <- loo(lla_half)
# 
# 
# 
# attach.nimble(mcmc.output_half_m6_s$samples)
# 
# MCMCtrace(object = mcmc.output_half_m6_s$samples,
#           pdf = FALSE, # no export to PDF
#           ind = TRUE, # separate density lines per chain
#           params = "time_1_preg_effect")
# # Gelman-Rubin diagnostic (AKA RHat or PSRF)
# gelman.diag(mcmc.output_half_m6_s$samples, multivariate = FALSE)
# 
# 
# 
# mcmcplot(mcmc.output_half_m6_s$samples)
# 
# 
# 
# ##how to get effects size
# ##jow to make the differnt charts in PP
# ##how to get time on deck chart
# ##uses zscore to preduct value. Use mean for all the other values
# ##will use for calf age ##what the fuck is a z-score?
# post_mat <- as.matrix(mcmc.output_half_m6_s$samples)
# # 2. Specify the parameters you care about
# params <- c(#"lac_effect",
#   "bm_effect",
#   "dde_effect_m",
#   "age_effect",
#   #"age2_effect",
#   # "precip_effect",
#   "Intercept", 
#   #"bod_con_effect",
#   #"lac_time_effect",
#   "sex_effect",
#   "time_1_preg_effect",
#   #"spei_3_effect",
#   "spei_6_effect"#,
#   #"spei_12_effect"#,
#   #"lac_score_preg_effect"#,
#   #"sex_lac_effect"
# )
# 
# # 3. Summarize each one
# summary_df_half_m6_s <- tibble(parameter = params) %>%
#   rowwise() %>%
#   mutate(
#     median   = median(post_mat[, parameter]),
#     lower95  = quantile(post_mat[, parameter], 0.025),
#     upper95  = quantile(post_mat[, parameter], 0.975),
#     # posterior probability effect > 0
#     p_gt_zero = mean(post_mat[, parameter] > 0),
#     # if you expect a negative effect, you can also compute P(<0):
#     p_lt_zero = mean(post_mat[, parameter] < 0)
#   ) %>%
#   ungroup()
# 
# print(summary_df_half_m6_s)
# get_variables(mcmc.output_half_m6_s$samples)
# 
# 
# summary_df_half_m6_s  %>%
#   kbl(digits = 3, caption = "Posterior Summary Statistics for Model bc_2") %>%
#   kable_styling(full_width = FALSE, position = "center")
# 
# # Full posterior distributions:
# 
# 
# 
# mod_19_fig<-mcmc.output_half_m6_s$samples%>%
#   spread_draws(#lac_effect,
#     bm_effect,
#     dde_effect_m,
#     age_effect,
#     #age2_effect,
#     #precip_effect,
#     
#     # bod_con_effect,
#     #lac_time_effect,
#     sex_effect,
#     #lac_score_preg_effect,
#     time_1_preg_effect,
#     #spei_3_effect,
#     spei_6_effect#, 
#     #spei_12_effect
#   ) %>%
#   pivot_longer(-c(.chain, .iteration, .draw),
#                names_to = 'Parameter',
#                values_to = 'Estimate') %>%
#   ggplot(aes(y = Parameter, x = Estimate, fill = Parameter)) +
#   stat_halfeye(show.legend = FALSE) +
#   geom_vline(xintercept = 0, linetype = 2, color = 'black') +
#   scale_fill_manual(values = c(
#     #lac_time_effect = "grey60",  # make precip_effect blue
#     sex_effect = "red",
#     time_1_preg_effect = "grey60",
#     bod_con_effect = "red4",
#     lac_time_effect = "grey60",
#     age_effect = "blue4"
#   )) +
#   theme_minimal()
# mod_19_fig
# 
# ##########################end model 19
# 
# ####half full################mean dde, spei_6
# model_half_m6_sb <-  nimbleCode({
#   
#   #Priors
#   Intercept  ~ dnorm(0, sd = 5)
#   
#   #lac_effect~ dt(mu=0, sigma=1, df=1)
#   #bm_effect ~ dt(mu=0, sigma=1, df=1)
#   dde_effect_m ~ dt(mu=0, sigma=1, df=1) 
#   age_effect ~ dt(mu=0, sigma=1, df=1)
#   
#   # lac_time_effect ~dt(mu=0, sigma=1, df=1)
#   # sex_effect ~ dt(mu=0, sigma=1, df=1)
#   
#   time_1_preg_effect ~ dt(mu=0, sigma=1, df=1)
#   # spei_3_effect ~ dt(mu = 0 , sigma = 1, df = 1)
#   spei_6_effect ~ dt(mu = 0 , sigma = 1, df = 1)
#   #spei_12_effect ~ dt(mu = 0 , sigma = 1, df = 1)
#   # age2_effect ~ dt(mu = 0, sigma = 1, df = 1)
#   #lac_score_preg_effect ~ dt(mu = 0, sigma = 1, df = 1)
#   #sex_lac_effect ~ dt(mu = 0, sigma = 1, df = 1)
#   
#   
#   
#   for(i in 1:n.obs){
#     
#     #Process Model
#     logit(preg_prob[i])<-Intercept+
#       #(lac_score[i]*lac_effect)+
#      # (est_bm[i]*bm_effect)+
#       (dde_m[i]*dde_effect_m)+
#       (age[i]*age_effect)+
#       # (pow(age[i], 2)*age2_effect)+
#       
#       #(sex_correct[i] * sex_effect) +
#       #(lac_time[i]* lac_time_effect)+
#       (time_1_preg[i]* time_1_preg_effect)+
#       # (spei_3[i]*spei_3_effect)+
#       (spei_6[i]*spei_6_effect)#+
#     #(spei_12[i]*spei_12_effect)#+
#     #(lac_score[i]*lac_time[i]*lac_score_preg_effect)
#     
#     
#     
#     
#     
#     
#     #Observation Model (Likelihood)
#     preg_status[i] ~ dbern(preg_prob[i])
#     
#     #Impute missing data values
#     lac_score[i] ~ dnorm(mean=0,sd=1)
#     # precip_year_prior[i] ~ dnorm(mean = 0, sd = 1)
#     #dde_95[i] ~ dnorm(mean=0,sd=1)
#      age[i] ~ dnorm(mean=0,sd=1)
#     # est_body_cond[i] ~ dnorm(mean=0,sd=1)
#    # est_bm[i] ~ dnorm(mean=0,sd=1)
#     
#     #lac_time[i] ~ dnorm(mean=0,sd=1)##does this matrer
#     #spei_3[i] ~ dnorm(mean = 0, sd = 1)
#     # spei_6[i] ~ dnorm(mean = 0, sd = 1)
#     # spei_12[i] ~ dnorm(mean = 0, sd = 1)
#     
#     
#     
#   }#i
#   
# })#model5
# parameters <- c(#"lac_effect",
#  # "bm_effect",
#   "dde_effect_m",
#   "age_effect",
#   # "age2_effect",
#   # "precip_effect",
#   "Intercept", 
#   # "bod_con_effect",
#   # "lac_time_effect",
#   #"sex_effect",
#   "time_1_preg_effect",
#   "logProb_preg_status",#
#   # "spei_3_effect",
#   "spei_6_effect"#,
#   #"spei_12_effect"#,
#   #"lac_score_preg_effect"#,
#   ##"sex_lac_effect"
# )
# 
# # MCMC Settings
# ni <- 400000
# nt <- 400
# nb <- 200000
# nc <- 3
# 
# nimble.data = list(
#   preg_status = as.numeric(cow_capture_all$time_2_preg) - 1,
#   lac_score = as.numeric((cow_capture_all$lac_score)),
#  # est_bm = as.numeric((cow_capture_all$est_bm)),
#   #dde_95 = as.numeric((cow_capture_all$area_DDE_gt_95)),
#   #dde_75 = as.numeric((cow_capture_all$area_DDE_gt_75)),
#   dde_m = as.numeric((cow_capture_all$area_DDE_gt_mean)),
#   age = as.numeric((cow_capture_all$age_estimate_2)),
#   #precip_year_prior = as.numeric((cow_capture_all$total_year_prior)),
#   sex_correct = ifelse(cow_capture_all$sex_correct== "m", 1,0),
#   lac_time= as.numeric(cow_capture_all$lac_time), 
#   time_1_preg= as.numeric(cow_capture_all$time_1_preg)-1,#
#   
#   #spei_3 = as.numeric((cow_capture_all$sept_year_prior_3M)),
#   spei_6 = as.numeric((cow_capture_all$sept_year_prior_6M))#,
#   #spei_12 = as.numeric((cow_capture_all$sept_year_prior_12M))
#   
# )
# 
# nimble.constants = list(n.obs = nrow(cow_capture_all))
# 
# 
# mcmc.output_half_m6_sb <- nimbleMCMC(code = model_half_m6_sb,
#                                   data = nimble.data,
#                                   constants=nimble.constants,
#                                   monitors = parameters,
#                                   niter = ni,
#                                   nburnin = nb,
#                                   nchains = nc,
#                                   thin=nt,
#                                   summary=TRUE,
#                                   samplesAsCodaMCMC = TRUE)
# 
# attach.nimble(mcmc.output_half_m6$samples)
# 
# 
# mata_half  <- do.call(rbind, lapply(mcmc.output_half_m6_sb$samples, as.matrix))
# lpa_half  <- grep("^logProb_preg_status\\[", colnames(mata_half), value = TRUE)
# lla_half  <- mata_half[, lpa_half, drop = FALSE]
# stopifnot(all(is.finite(lla_half) | (is.infinite(lla_half) & lla_half < 0)))
# loo_half_m6_sb <- loo(lla_half)
# 
# 
# 
# attach.nimble(mcmc.output_half_m6_sb$samples)
# 
# MCMCtrace(object = mcmc.output_half_m6_sb$samples,
#           pdf = FALSE, # no export to PDF
#           ind = TRUE, # separate density lines per chain
#           params = "time_1_preg_effect")
# # Gelman-Rubin diagnostic (AKA RHat or PSRF)
# gelman.diag(mcmc.output_half_m6_sb$samples, multivariate = FALSE)
# 
# 
# 
# mcmcplot(mcmc.output_half_m6_sb$samples)
# 
# 
# 
# ##how to get effects size
# ##jow to make the differnt charts in PP
# ##how to get time on deck chart
# ##uses zscore to preduct value. Use mean for all the other values
# ##will use for calf age ##what the fuck is a z-score?
# post_mat <- as.matrix(mcmc.output_half_m6_sb$samples)
# # 2. Specify the parameters you care about
# params <- c(#"lac_effect",
#   #"bm_effect",
#   "dde_effect_m",
#   "age_effect",
#   #"age2_effect",
#   # "precip_effect",
#   "Intercept", 
#   #"bod_con_effect",
#   #"lac_time_effect",
#   #"sex_effect",
#   "time_1_preg_effect",
#   #"spei_3_effect",
#   "spei_6_effect"#,
#   #"spei_12_effect"#,
#   #"lac_score_preg_effect"#,
#   #"sex_lac_effect"
# )
# 
# # 3. Summarize each one
# summary_df_half_m6_sb <- tibble(parameter = params) %>%
#   rowwise() %>%
#   mutate(
#     median   = median(post_mat[, parameter]),
#     lower95  = quantile(post_mat[, parameter], 0.025),
#     upper95  = quantile(post_mat[, parameter], 0.975),
#     # posterior probability effect > 0
#     p_gt_zero = mean(post_mat[, parameter] > 0),
#     # if you expect a negative effect, you can also compute P(<0):
#     p_lt_zero = mean(post_mat[, parameter] < 0)
#   ) %>%
#   ungroup()
# 
# print(summary_df_half_m6_sb)
# get_variables(mcmc.output_half_m6_sb$samples)
# 
# 
# summary_df_half_m6_sb  %>%
#   kbl(digits = 3, caption = "Posterior Summary Statistics for Model bc_2") %>%
#   kable_styling(full_width = FALSE, position = "center")
# 
# # Full posterior distributions:
# 
# 
# 
# mod_20_fig<-mcmc.output_half_m6_sb$samples%>%
#   spread_draws(#lac_effect,
#    # bm_effect,
#     dde_effect_m,
#     age_effect,
#     #age2_effect,
#     #precip_effect,
#     
#     # bod_con_effect,
#     #lac_time_effect,
#     #sex_effect,
#     #lac_score_preg_effect,
#     time_1_preg_effect,
#     #spei_3_effect,
#     spei_6_effect#, 
#     #spei_12_effect
#   ) %>%
#   pivot_longer(-c(.chain, .iteration, .draw),
#                names_to = 'Parameter',
#                values_to = 'Estimate') %>%
#   ggplot(aes(y = Parameter, x = Estimate, fill = Parameter)) +
#   stat_halfeye(show.legend = FALSE) +
#   geom_vline(xintercept = 0, linetype = 2, color = 'black') +
#   scale_fill_manual(values = c(
#     #lac_time_effect = "grey60",  # make precip_effect blue
#     sex_effect = "red",
#     time_1_preg_effect = "grey60",
#     bod_con_effect = "red4",
#     lac_time_effect = "grey60",
#     age_effect = "blue4"
#   )) +
#   theme_minimal()
# mod_20_fig
# 
# ####end model 20#####
# 
# 
# 
# ####half full################mean dde, spei_6
# model_half_m6_sb_bd <-  nimbleCode({
#   
#   #Priors
#   Intercept  ~ dnorm(0, sd = 5)
#   
#   #lac_effect~ dt(mu=0, sigma=1, df=1)
#   #bm_effect ~ dt(mu=0, sigma=1, df=1)
#   dde_effect_m ~ dt(mu=0, sigma=1, df=1) 
#   age_effect ~ dt(mu=0, sigma=1, df=1)
#   date_effect~ dt(mu=0, sigma=1, df=1)
#   #date_effect~ dnorm(0, sd = 5)
#   # lac_time_effect ~dt(mu=0, sigma=1, df=1)
#   # sex_effect ~ dt(mu=0, sigma=1, df=1)
#   
#   time_1_preg_effect ~ dt(mu=0, sigma=1, df=1)
#   # spei_3_effect ~ dt(mu = 0 , sigma = 1, df = 1)
#   spei_6_effect ~ dt(mu = 0 , sigma = 1, df = 1)
#   #spei_12_effect ~ dt(mu = 0 , sigma = 1, df = 1)
#   # age2_effect ~ dt(mu = 0, sigma = 1, df = 1)
#   #lac_score_preg_effect ~ dt(mu = 0, sigma = 1, df = 1)
#   #sex_lac_effect ~ dt(mu = 0, sigma = 1, df = 1)
#   
#   
#   
#   for(i in 1:n.obs){
#     
#     #Process Model
#     logit(preg_prob[i])<-Intercept+
#       #(lac_score[i]*lac_effect)+
#       # (est_bm[i]*bm_effect)+
#       (dde_m[i]*dde_effect_m)+
#       (age[i]*age_effect)+
#       # (pow(age[i], 2)*age2_effect)+
#       (calving_date[i] *date_effect )+
#       #(sex_correct[i] * sex_effect) +
#       #(lac_time[i]* lac_time_effect)+
#       (time_1_preg[i]* time_1_preg_effect)+
#       # (spei_3[i]*spei_3_effect)+
#       (spei_6[i]*spei_6_effect)#+
#     #(spei_12[i]*spei_12_effect)#+
#     #(lac_score[i]*lac_time[i]*lac_score_preg_effect)
#     
#     
#     
#     
#     
#     
#     #Observation Model (Likelihood)
#     preg_status[i] ~ dbern(preg_prob[i])
#     
#     #Impute missing data values
#     lac_score[i] ~ dnorm(mean=0,sd=1)
#     # precip_year_prior[i] ~ dnorm(mean = 0, sd = 1)
#     #dde_95[i] ~ dnorm(mean=0,sd=1)
#      age[i] ~ dnorm(mean=0,sd=1)
#     # est_body_cond[i] ~ dnorm(mean=0,sd=1)
#     # est_bm[i] ~ dnorm(mean=0,sd=1)
#     
#     #lac_time[i] ~ dnorm(mean=0,sd=1)##does this matrer
#     #spei_3[i] ~ dnorm(mean = 0, sd = 1)
#     # spei_6[i] ~ dnorm(mean = 0, sd = 1)
#     # spei_12[i] ~ dnorm(mean = 0, sd = 1)
#      calving_date[i]~ dnorm(mean = 0, sd = 1)
#     
#     
#   }#i
#   
# })#model5
# parameters <- c(#"lac_effect",
#   # "bm_effect",
#   "dde_effect_m",
#   "age_effect",
#   "date_effect",
#   # "age2_effect",
#   # "precip_effect",
#   "Intercept", 
#   # "bod_con_effect",
#   # "lac_time_effect",
#   #"sex_effect",
#   "time_1_preg_effect",
#   "logProb_preg_status",#
#   # "spei_3_effect",
#   "spei_6_effect"#,
#   #"spei_12_effect"#,
#   #"lac_score_preg_effect"#,
#   ##"sex_lac_effect"
# )
# 
# # MCMC Settings
# ni <- 400000
# nt <- 400
# nb <- 200000
# nc <- 3
# 
# nimble.data = list(
#   preg_status = as.numeric(cow_capture_all$time_2_preg) - 1,
#   lac_score = as.numeric((cow_capture_all$lac_score)),
#   # est_bm = as.numeric((cow_capture_all$est_bm)),
#   #dde_95 = as.numeric((cow_capture_all$area_DDE_gt_95)),
#   #dde_75 = as.numeric((cow_capture_all$area_DDE_gt_75)),
#   dde_m = as.numeric((cow_capture_all$area_DDE_gt_mean)),
#   age = as.numeric((cow_capture_all$age_estimate_2)),
#   #precip_year_prior = as.numeric((cow_capture_all$total_year_prior)),
#   sex_correct = ifelse(cow_capture_all$sex_correct== "m", 1,0),
#   lac_time= as.numeric(cow_capture_all$lac_time), 
#   time_1_preg= as.numeric(cow_capture_all$time_1_preg)-1,#
#   calving_date = as.numeric(cow_capture_all$birt_date),
#   #spei_3 = as.numeric((cow_capture_all$sept_year_prior_3M)),
#   spei_6 = as.numeric((cow_capture_all$sept_year_prior_6M))#,
#   #spei_12 = as.numeric((cow_capture_all$sept_year_prior_12M))
#   
# )
# 
# nimble.constants = list(n.obs = nrow(cow_capture_all))
# 
# 
# mcmc.output_half_m6_sb_bd <- nimbleMCMC(code = model_half_m6_sb_bd,
#                                      data = nimble.data,
#                                      constants=nimble.constants,
#                                      monitors = parameters,
#                                      niter = ni,
#                                      nburnin = nb,
#                                      nchains = nc,
#                                      thin=nt,
#                                      summary=TRUE,
#                                      samplesAsCodaMCMC = TRUE)
# 
# attach.nimble(mcmc.output_half_m6_sb_bd$samples)
# 
# 
# mata_half  <- do.call(rbind, lapply(mcmc.output_half_m6_sb_bd$samples, as.matrix))
# lpa_half  <- grep("^logProb_preg_status\\[", colnames(mata_half), value = TRUE)
# lla_half  <- mata_half[, lpa_half, drop = FALSE]
# stopifnot(all(is.finite(lla_half) | (is.infinite(lla_half) & lla_half < 0)))
# loo_half_m6_sb_bd <- loo(lla_half)
# 
# 
# 
# attach.nimble(mcmc.output_half_m6_sb_bd$samples)
# 
# MCMCtrace(object = mcmc.output_half_m6_sb_bd$samples,
#           pdf = FALSE, # no export to PDF
#           ind = TRUE, # separate density lines per chain
#           params = "time_1_preg_effect")
# # Gelman-Rubin diagnostic (AKA RHat or PSRF)
# gelman.diag(mcmc.output_half_m6_sb_bd$samples, multivariate = FALSE)
# 
# 
# 
# mcmcplot(mcmc.output_half_m6_sb_bd$samples)
# 
# 
# 
# ##how to get effects size
# ##jow to make the differnt charts in PP
# ##how to get time on deck chart
# ##uses zscore to preduct value. Use mean for all the other values
# ##will use for calf age ##what the fuck is a z-score?
# post_mat <- as.matrix(mcmc.output_half_m6_sb_bd$samples)
# # 2. Specify the parameters you care about
# params <- c(#"lac_effect",
#   #"bm_effect",
#   "dde_effect_m",
#   "age_effect",
#   "date_effect",
#   #"age2_effect",
#   # "precip_effect",
#   "Intercept", 
#   #"bod_con_effect",
#   #"lac_time_effect",
#   #"sex_effect",
#   "time_1_preg_effect",
#   #"spei_3_effect",
#   "spei_6_effect"#,
#   #"spei_12_effect"#,
#   #"lac_score_preg_effect"#,
#   #"sex_lac_effect"
# )
# 
# # 3. Summarize each one
# summary_df_half_m6_sb_bd <- tibble(parameter = params) %>%
#   rowwise() %>%
#   mutate(
#     median   = median(post_mat[, parameter]),
#     lower95  = quantile(post_mat[, parameter], 0.025),
#     upper95  = quantile(post_mat[, parameter], 0.975),
#     # posterior probability effect > 0
#     p_gt_zero = mean(post_mat[, parameter] > 0),
#     # if you expect a negative effect, you can also compute P(<0):
#     p_lt_zero = mean(post_mat[, parameter] < 0)
#   ) %>%
#   ungroup()
# 
# print(summary_df_half_m6_sb_bd)
# get_variables(mcmc.output_half_m6_sb_bd$samples)
# 
# 
# summary_df_half_m6_sb_bd  %>%
#   kbl(digits = 3, caption = "Posterior Summary Statistics for Model bc_2") %>%
#   kable_styling(full_width = FALSE, position = "center")
# 
# # Full posterior distributions:
# 
# 
# 
# mod_21_fig<-mcmc.output_half_m6_sb_bd$samples%>%
#   spread_draws(#lac_effect,
#     # bm_effect,
#     dde_effect_m,
#     age_effect,
#     date_effect,
#     #age2_effect,
#     #precip_effect,
#     
#     # bod_con_effect,
#     #lac_time_effect,
#     #sex_effect,
#     #lac_score_preg_effect,
#     time_1_preg_effect,
#     #spei_3_effect,
#     spei_6_effect#, 
#     #spei_12_effect
#   ) %>%
#   pivot_longer(-c(.chain, .iteration, .draw),
#                names_to = 'Parameter',
#                values_to = 'Estimate') %>%
#   ggplot(aes(y = Parameter, x = Estimate, fill = Parameter)) +
#   stat_halfeye(show.legend = FALSE) +
#   geom_vline(xintercept = 0, linetype = 2, color = 'black') +
#   scale_fill_manual(values = c(
#     #lac_time_effect = "grey60",  # make precip_effect blue
#     sex_effect = "red",
#     time_1_preg_effect = "grey60",
#     bod_con_effect = "red4",
#     lac_time_effect = "grey60",
#     age_effect = "blue4"
#   )) +
#   theme_minimal()
# mod_21_fig
# 
# ####end model 21#####
# 
# model_half_m6_lac_bd <-  nimbleCode({
#   
#   #Priors
#   Intercept  ~ dnorm(0, sd = 5)
#   
#   #lac_effect~ dt(mu=0, sigma=1, df=1)
#   #bm_effect ~ dt(mu=0, sigma=1, df=1)
#   dde_effect_m ~ dt(mu=0, sigma=1, df=1) 
#   age_effect ~ dt(mu=0, sigma=1, df=1)
#   date_effect~ dt(mu=0, sigma=1, df=1)
#   #date_effect~ dnorm(0, sd = 5)
#    lac_time_effect ~dt(mu=0, sigma=1, df=1)
#   # sex_effect ~ dt(mu=0, sigma=1, df=1)
#   
#   time_1_preg_effect ~ dt(mu=0, sigma=1, df=1)
#   # spei_3_effect ~ dt(mu = 0 , sigma = 1, df = 1)
#   spei_6_effect ~ dt(mu = 0 , sigma = 1, df = 1)
#   #spei_12_effect ~ dt(mu = 0 , sigma = 1, df = 1)
#   # age2_effect ~ dt(mu = 0, sigma = 1, df = 1)
#   #lac_score_preg_effect ~ dt(mu = 0, sigma = 1, df = 1)
#   #sex_lac_effect ~ dt(mu = 0, sigma = 1, df = 1)
#   
#   
#   
#   for(i in 1:n.obs){
#     
#     #Process Model
#     logit(preg_prob[i])<-Intercept+
#       #(lac_score[i]*lac_effect)+
#       # (est_bm[i]*bm_effect)+
#       (dde_m[i]*dde_effect_m)+
#       (age[i]*age_effect)+
#       # (pow(age[i], 2)*age2_effect)+
#       (calving_date[i] *date_effect )+
#       #(sex_correct[i] * sex_effect) +
#       (lac_time[i]* lac_time_effect)+
#       (time_1_preg[i]* time_1_preg_effect)+
#       # (spei_3[i]*spei_3_effect)+
#       (spei_6[i]*spei_6_effect)#+
#     #(spei_12[i]*spei_12_effect)#+
#     #(lac_score[i]*lac_time[i]*lac_score_preg_effect)
#     
#     
#     
#     
#     
#     
#     #Observation Model (Likelihood)
#     preg_status[i] ~ dbern(preg_prob[i])
#     
#     #Impute missing data values
#     lac_score[i] ~ dnorm(mean=0,sd=1)
#     # precip_year_prior[i] ~ dnorm(mean = 0, sd = 1)
#     #dde_95[i] ~ dnorm(mean=0,sd=1)
#      age[i] ~ dnorm(mean=0,sd=1)
#      calving_date[i]~ dnorm(mean=0,sd=1)
#     # est_body_cond[i] ~ dnorm(mean=0,sd=1)
#     # est_bm[i] ~ dnorm(mean=0,sd=1)
#     
#     #lac_time[i] ~ dnorm(mean=0,sd=1)##does this matrer
#     #spei_3[i] ~ dnorm(mean = 0, sd = 1)
#     # spei_6[i] ~ dnorm(mean = 0, sd = 1)
#     # spei_12[i] ~ dnorm(mean = 0, sd = 1)
#     
#     
#     
#   }#i
#   
# })#model5
# parameters <- c(#"lac_effect",
#   # "bm_effect",
#   "dde_effect_m",
#   "age_effect",
#   "date_effect",
#   # "age2_effect",
#   # "precip_effect",
#   "Intercept", 
#   # "bod_con_effect",
#    "lac_time_effect",
#   #"sex_effect",
#   "time_1_preg_effect",
#   "logProb_preg_status",#
#   # "spei_3_effect",
#   "spei_6_effect"#,
#   #"spei_12_effect"#,
#   #"lac_score_preg_effect"#,
#   ##"sex_lac_effect"
# )
# 
# # MCMC Settings
# ni <- 400000
# nt <- 400
# nb <- 200000
# nc <- 3
# 
# nimble.data = list(
#   preg_status = as.numeric(cow_capture_all$time_2_preg) - 1,
#   lac_score = as.numeric((cow_capture_all$lac_score)),
#   # est_bm = as.numeric((cow_capture_all$est_bm)),
#   #dde_95 = as.numeric((cow_capture_all$area_DDE_gt_95)),
#   #dde_75 = as.numeric((cow_capture_all$area_DDE_gt_75)),
#   dde_m = as.numeric((cow_capture_all$area_DDE_gt_mean)),
#   age = as.numeric((cow_capture_all$age_estimate_2)),
#   #precip_year_prior = as.numeric((cow_capture_all$total_year_prior)),
#   sex_correct = ifelse(cow_capture_all$sex_correct== "m", 1,0),
#   lac_time= as.numeric(cow_capture_all$lac_time), 
#   time_1_preg= as.numeric(cow_capture_all$time_1_preg)-1,#
#   calving_date = as.numeric(cow_capture_all$birt_date),
#   #spei_3 = as.numeric((cow_capture_all$sept_year_prior_3M)),
#   spei_6 = as.numeric((cow_capture_all$sept_year_prior_6M))#,
#   #spei_12 = as.numeric((cow_capture_all$sept_year_prior_12M))
#   
# )
# 
# nimble.constants = list(n.obs = nrow(cow_capture_all))
# 
# 
# mcmc.output_half_m6_lac_bd<- nimbleMCMC(code = model_half_m6_lac_bd,
#                                         data = nimble.data,
#                                         constants=nimble.constants,
#                                         monitors = parameters,
#                                         niter = ni,
#                                         nburnin = nb,
#                                         nchains = nc,
#                                         thin=nt,
#                                         summary=TRUE,
#                                         samplesAsCodaMCMC = TRUE)
# 
# attach.nimble(mcmc.output_half_m6_lac_bd$samples)
# 
# 
# mata_half  <- do.call(rbind, lapply(mcmc.output_half_m6_lac_bd$samples, as.matrix))
# lpa_half  <- grep("^logProb_preg_status\\[", colnames(mata_half), value = TRUE)
# lla_half  <- mata_half[, lpa_half, drop = FALSE]
# stopifnot(all(is.finite(lla_half) | (is.infinite(lla_half) & lla_half < 0)))
# loo_half_m6_lac_bd<- loo(lla_half)
# 
# 
# 
# attach.nimble(mcmc.output_half_m6_lac_bd$samples)
# 
# MCMCtrace(object = mcmc.output_half_m6_lac_bd$samples,
#           pdf = FALSE, # no export to PDF
#           ind = TRUE, # separate density lines per chain
#           params = "time_1_preg_effect")
# # Gelman-Rubin diagnostic (AKA RHat or PSRF)
# gelman.diag(mcmc.output_half_m6_lac_bd$samples, multivariate = FALSE)
# 
# 
# 
# mcmcplot(mcmc.output_half_m6_lac_bd$samples)
# 
# 
# 
# ##how to get effects size
# ##jow to make the differnt charts in PP
# ##how to get time on deck chart
# ##uses zscore to preduct value. Use mean for all the other values
# ##will use for calf age ##what the fuck is a z-score?
# post_mat <- as.matrix(mcmc.output_half_m6_lac_bd$samples)
# # 2. Specify the parameters you care about
# params <- c(#"lac_effect",
#   #"bm_effect",
#   "dde_effect_m",
#   "age_effect",
#   "date_effect",
#   #"age2_effect",
#   # "precip_effect",
#   "Intercept", 
#   #"bod_con_effect",
#   "lac_time_effect",
#   #"sex_effect",
#   "time_1_preg_effect",
#   #"spei_3_effect",
#   "spei_6_effect"#,
#   #"spei_12_effect"#,
#   #"lac_score_preg_effect"#,
#   #"sex_lac_effect"
# )
# 
# # 3. Summarize each one
# summary_df_half_m6_lac_bd <- tibble(parameter = params) %>%
#   rowwise() %>%
#   mutate(
#     median   = median(post_mat[, parameter]),
#     lower95  = quantile(post_mat[, parameter], 0.025),
#     upper95  = quantile(post_mat[, parameter], 0.975),
#     # posterior probability effect > 0
#     p_gt_zero = mean(post_mat[, parameter] > 0),
#     # if you expect a negative effect, you can also compute P(<0):
#     p_lt_zero = mean(post_mat[, parameter] < 0)
#   ) %>%
#   ungroup()
# 
# print(summary_df_half_m6_lac_bd)
# get_variables(mcmc.output_half_m6_lac_bd$samples)
# 
# 
# summary_df_half_m6_sb_bd  %>%
#   kbl(digits = 3, caption = "Posterior Summary Statistics for Model bc_2") %>%
#   kable_styling(full_width = FALSE, position = "center")
# 
# # Full posterior distributions:
# 
# 
# 
# mod_22_fig<-mcmc.output_half_m6_lac_bd$samples%>%
#   spread_draws(#lac_effect,
#     # bm_effect,
#     dde_effect_m,
#     age_effect,
#     date_effect,
#     #age2_effect,
#     #precip_effect,
#     
#     # bod_con_effect,
#     lac_time_effect,
#     #sex_effect,
#     #lac_score_preg_effect,
#     time_1_preg_effect,
#     #spei_3_effect,
#     spei_6_effect#, 
#     #spei_12_effect
#   ) %>%
#   pivot_longer(-c(.chain, .iteration, .draw),
#                names_to = 'Parameter',
#                values_to = 'Estimate') %>%
#   ggplot(aes(y = Parameter, x = Estimate, fill = Parameter)) +
#   stat_halfeye(show.legend = FALSE) +
#   geom_vline(xintercept = 0, linetype = 2, color = 'black') +
#   scale_fill_manual(values = c(
#     #lac_time_effect = "grey60",  # make precip_effect blue
#     sex_effect = "red",
#     time_1_preg_effect = "grey60",
#     bod_con_effect = "red4",
#     lac_time_effect = "grey60",
#     age_effect = "blue4"
#   )) +
#   theme_minimal()
# mod_22_fig
# 
# ####end model 22#####
# 
# 
# model_half_m6_lac_X_bd <-  nimbleCode({
#   
#   #Priors
#   Intercept  ~ dnorm(0, sd = 5)
#   
#   #lac_effect~ dt(mu=0, sigma=1, df=1)
#   #bm_effect ~ dt(mu=0, sigma=1, df=1)
#   dde_effect_m ~ dt(mu=0, sigma=1, df=1) 
#   age_effect ~ dt(mu=0, sigma=1, df=1)
#   date_effect~ dt(mu=0, sigma=1, df=1)
#   #date_effect~ dnorm(0, sd = 5)
#   lac_time_effect ~dt(mu=0, sigma=1, df=1)
#   # sex_effect ~ dt(mu=0, sigma=1, df=1)
#   
#   time_1_preg_effect ~ dt(mu=0, sigma=1, df=1)
#   # spei_3_effect ~ dt(mu = 0 , sigma = 1, df = 1)
#   spei_6_effect ~ dt(mu = 0 , sigma = 1, df = 1)
#   #spei_12_effect ~ dt(mu = 0 , sigma = 1, df = 1)
#   # age2_effect ~ dt(mu = 0, sigma = 1, df = 1)
#   #lac_score_preg_effect ~ dt(mu = 0, sigma = 1, df = 1)
#   #sex_lac_effect ~ dt(mu = 0, sigma = 1, df = 1)
#   date_lac_effect ~ dt(mu = 0, sigma = 1, df = 1)
#   
#   
#   for(i in 1:n.obs){
#     
#     #Process Model
#     logit(preg_prob[i])<-Intercept+
#       #(lac_score[i]*lac_effect)+
#       # (est_bm[i]*bm_effect)+
#       (dde_m[i]*dde_effect_m)+
#       (age[i]*age_effect)+
#       # (pow(age[i], 2)*age2_effect)+
#       (calving_date[i] *date_effect )+
#       #(sex_correct[i] * sex_effect) +
#       (lac_time[i]* lac_time_effect)+
#       (time_1_preg[i]* time_1_preg_effect)+
#       # (spei_3[i]*spei_3_effect)+
#       (spei_6[i]*spei_6_effect)+
#     #(spei_12[i]*spei_12_effect)#+
#     (calving_date[i]*lac_time[i]*date_lac_effect)
#     
#     
#     
#     
#     
#     
#     #Observation Model (Likelihood)
#     preg_status[i] ~ dbern(preg_prob[i])
#     
#     #Impute missing data values
#     lac_score[i] ~ dnorm(mean=0,sd=1)
#     # precip_year_prior[i] ~ dnorm(mean = 0, sd = 1)
#     #dde_95[i] ~ dnorm(mean=0,sd=1)
#      age[i] ~ dnorm(mean=0,sd=1)
#     # est_body_cond[i] ~ dnorm(mean=0,sd=1)
#     # est_bm[i] ~ dnorm(mean=0,sd=1)
#     calving_date[i]~ dnorm(mean=0,sd=1)
#     #lac_time[i] ~ dnorm(mean=0,sd=1)##does this matrer
#     #spei_3[i] ~ dnorm(mean = 0, sd = 1)
#     # spei_6[i] ~ dnorm(mean = 0, sd = 1)
#     # spei_12[i] ~ dnorm(mean = 0, sd = 1)
#     
#     
#     
#   }#i
#   
# })#model5
# parameters <- c(#"lac_effect",
#   # "bm_effect",
#   "dde_effect_m",
#   "age_effect",
#   "date_effect",
#   "date_lac_effect",
#   # "age2_effect",
#   # "precip_effect",
#   "Intercept", 
#   # "bod_con_effect",
#   "lac_time_effect",
#   #"sex_effect",
#   "time_1_preg_effect",
#   "logProb_preg_status",#
#   # "spei_3_effect",
#   "spei_6_effect"#,
#   #"spei_12_effect"#,
#   #"lac_score_preg_effect"#,
#   ##"sex_lac_effect"
# )
# 
# # MCMC Settings
# ni <- 400000
# nt <- 400
# nb <- 200000
# nc <- 3
# 
# nimble.data = list(
#   preg_status = as.numeric(cow_capture_all$time_2_preg) - 1,
#   lac_score = as.numeric((cow_capture_all$lac_score)),
#   # est_bm = as.numeric((cow_capture_all$est_bm)),
#   #dde_95 = as.numeric((cow_capture_all$area_DDE_gt_95)),
#   #dde_75 = as.numeric((cow_capture_all$area_DDE_gt_75)),
#   dde_m = as.numeric((cow_capture_all$area_DDE_gt_mean)),
#   age = as.numeric((cow_capture_all$age_estimate_2)),
#   #precip_year_prior = as.numeric((cow_capture_all$total_year_prior)),
#   sex_correct = ifelse(cow_capture_all$sex_correct== "m", 1,0),
#   lac_time= as.numeric(cow_capture_all$lac_time), 
#   time_1_preg= as.numeric(cow_capture_all$time_1_preg)-1,#
#   calving_date = as.numeric(cow_capture_all$birt_date),
#   #spei_3 = as.numeric((cow_capture_all$sept_year_prior_3M)),
#   spei_6 = as.numeric((cow_capture_all$sept_year_prior_6M))#,
#   #spei_12 = as.numeric((cow_capture_all$sept_year_prior_12M))
#   
# )
# 
# nimble.constants = list(n.obs = nrow(cow_capture_all))
# 
# 
# mcmc.output_half_m6_lac_X_bd<- nimbleMCMC(code = model_half_m6_lac_X_bd,
#                                         data = nimble.data,
#                                         constants=nimble.constants,
#                                         monitors = parameters,
#                                         niter = ni,
#                                         nburnin = nb,
#                                         nchains = nc,
#                                         thin=nt,
#                                         summary=TRUE,
#                                         samplesAsCodaMCMC = TRUE)
# 
# attach.nimble(mcmc.output_half_m6_lac_X_bd$samples)
# 
# 
# mata_half  <- do.call(rbind, lapply(mcmc.output_half_m6_lac_X_bd$samples, as.matrix))
# lpa_half  <- grep("^logProb_preg_status\\[", colnames(mata_half), value = TRUE)
# lla_half  <- mata_half[, lpa_half, drop = FALSE]
# stopifnot(all(is.finite(lla_half) | (is.infinite(lla_half) & lla_half < 0)))
# loo_half_m6_lac_X_bd<- loo(lla_half)
# 
# 
# 
# attach.nimble(mcmc.output_half_m6_lac_X_bd$samples)
# 
# MCMCtrace(object = mcmc.output_half_m6_lac_X_bd$samples,
#           pdf = FALSE, # no export to PDF
#           ind = TRUE, # separate density lines per chain
#           params = "time_1_preg_effect")
# # Gelman-Rubin diagnostic (AKA RHat or PSRF)
# gelman.diag(mcmc.output_half_m6_lac_X_bd$samples, multivariate = FALSE)
# 
# 
# 
# mcmcplot(mcmc.output_half_m6_lac_X_bd$samples)
# 
# 
# 
# ##how to get effects size
# ##jow to make the differnt charts in PP
# ##how to get time on deck chart
# ##uses zscore to preduct value. Use mean for all the other values
# ##will use for calf age ##what the fuck is a z-score?
# post_mat <- as.matrix(mcmc.output_half_m6_lac_X_bd$samples)
# # 2. Specify the parameters you care about
# params <- c(#"lac_effect",
#   #"bm_effect",
#   "dde_effect_m",
#   "age_effect",
#   "date_effect",
#   "date_lac_effect",
#   #"age2_effect",
#   # "precip_effect",
#   "Intercept", 
#   #"bod_con_effect",
#   "lac_time_effect",
#   #"sex_effect",
#   "time_1_preg_effect",
#   #"spei_3_effect",
#   "spei_6_effect"#,
#   #"spei_12_effect"#,
#   #"lac_score_preg_effect"#,
#   #"sex_lac_effect"
# )
# 
# # 3. Summarize each one
# summary_df_half_m6_lac_X_bd<- tibble(parameter = params) %>%
#   rowwise() %>%
#   mutate(
#     median   = median(post_mat[, parameter]),
#     lower95  = quantile(post_mat[, parameter], 0.025),
#     upper95  = quantile(post_mat[, parameter], 0.975),
#     # posterior probability effect > 0
#     p_gt_zero = mean(post_mat[, parameter] > 0),
#     # if you expect a negative effect, you can also compute P(<0):
#     p_lt_zero = mean(post_mat[, parameter] < 0)
#   ) %>%
#   ungroup()
# 
# print(summary_df_half_m6_lac_X_bd)
# get_variables(mcmc.output_half_m6_lac_X_bd$samples)
# 
# 
# summary_df_half_m6_lac_X_bd %>%
#   kbl(digits = 3, caption = "Posterior Summary Statistics for Model bc_2") %>%
#   kable_styling(full_width = FALSE, position = "center")
# 
# # Full posterior distributions:
# 
# 
# 
# mod_23_fig<-mcmc.output_half_m6_lac_X_bd$samples%>%
#   spread_draws(#lac_effect,
#     # bm_effect,
#     dde_effect_m,
#     date_lac_effect,
#     age_effect,
#     date_effect,
#     #age2_effect,
#     #precip_effect,
#     
#     # bod_con_effect,
#     lac_time_effect,
#     #sex_effect,
#     #lac_score_preg_effect,
#     time_1_preg_effect,
#     #spei_3_effect,
#     spei_6_effect#, 
#     #spei_12_effect
#   ) %>%
#   pivot_longer(-c(.chain, .iteration, .draw),
#                names_to = 'Parameter',
#                values_to = 'Estimate') %>%
#   ggplot(aes(y = Parameter, x = Estimate, fill = Parameter)) +
#   stat_halfeye(show.legend = FALSE) +
#   geom_vline(xintercept = 0, linetype = 2, color = 'black') +
#   scale_fill_manual(values = c(
#     #lac_time_effect = "grey60",  # make precip_effect blue
#     sex_effect = "red",
#     time_1_preg_effect = "grey60",
#     bod_con_effect = "red4",
#     lac_time_effect = "grey60",
#     age_effect = "blue4"
#   )) +
#   theme_minimal()
# mod_23_fig
# 
# ####end model 23
# 
# ####half full################mean dde, spei_6
# model_half_m6_sb_bd_s <-  nimbleCode({
#   
#   #Priors
#   Intercept  ~ dnorm(0, sd = 5)
#   
#   #lac_effect~ dt(mu=0, sigma=1, df=1)
#   #bm_effect ~ dt(mu=0, sigma=1, df=1)
#   dde_effect_m ~ dt(mu=0, sigma=1, df=1) 
#   age_effect ~ dt(mu=0, sigma=1, df=1)
#   date_effect~ dt(mu=0, sigma=1, df=1)
#   #date_effect~ dnorm(0, sd = 5)
#   # lac_time_effect ~dt(mu=0, sigma=1, df=1)
#    sex_effect ~ dt(mu=0, sigma=1, df=1)
#   
#   time_1_preg_effect ~ dt(mu=0, sigma=1, df=1)
#   # spei_3_effect ~ dt(mu = 0 , sigma = 1, df = 1)
#   spei_6_effect ~ dt(mu = 0 , sigma = 1, df = 1)
#   #spei_12_effect ~ dt(mu = 0 , sigma = 1, df = 1)
#   # age2_effect ~ dt(mu = 0, sigma = 1, df = 1)
#   #lac_score_preg_effect ~ dt(mu = 0, sigma = 1, df = 1)
#   #sex_lac_effect ~ dt(mu = 0, sigma = 1, df = 1)
#   
#   
#   
#   for(i in 1:n.obs){
#     
#     #Process Model
#     logit(preg_prob[i])<-Intercept+
#       #(lac_score[i]*lac_effect)+
#       # (est_bm[i]*bm_effect)+
#       (dde_m[i]*dde_effect_m)+
#       (age[i]*age_effect)+
#       # (pow(age[i], 2)*age2_effect)+
#       (calving_date[i] *date_effect )+
#       (sex_correct[i] * sex_effect) +
#       #(lac_time[i]* lac_time_effect)+
#       (time_1_preg[i]* time_1_preg_effect)+
#       # (spei_3[i]*spei_3_effect)+
#       (spei_6[i]*spei_6_effect)#+
#     #(spei_12[i]*spei_12_effect)#+
#     #(lac_score[i]*lac_time[i]*lac_score_preg_effect)
#     
#     
#     
#     
#     
#     
#     #Observation Model (Likelihood)
#     preg_status[i] ~ dbern(preg_prob[i])
#     
#     #Impute missing data values
#     lac_score[i] ~ dnorm(mean=0,sd=1)
#     # precip_year_prior[i] ~ dnorm(mean = 0, sd = 1)
#     #dde_95[i] ~ dnorm(mean=0,sd=1)
#      age[i] ~ dnorm(mean=0,sd=1)
#      calving_date[i] ~ dnorm(mean=0,sd=1)
#     # est_body_cond[i] ~ dnorm(mean=0,sd=1)
#     # est_bm[i] ~ dnorm(mean=0,sd=1)
#     
#     #lac_time[i] ~ dnorm(mean=0,sd=1)##does this matrer
#     #spei_3[i] ~ dnorm(mean = 0, sd = 1)
#     # spei_6[i] ~ dnorm(mean = 0, sd = 1)
#     # spei_12[i] ~ dnorm(mean = 0, sd = 1)
#     
#     
#     
#   }#i
#   
# })#model5
# parameters <- c(#"lac_effect",
#   # "bm_effect",
#   "dde_effect_m",
#   "age_effect",
#   "date_effect",
#   # "age2_effect",
#   # "precip_effect",
#   "Intercept", 
#   # "bod_con_effect",
#   # "lac_time_effect",
#   "sex_effect",
#   "time_1_preg_effect",
#   "logProb_preg_status",#
#   # "spei_3_effect",
#   "spei_6_effect"#,
#   #"spei_12_effect"#,
#   #"lac_score_preg_effect"#,
#   ##"sex_lac_effect"
# )
# 
# # MCMC Settings
# ni <- 400000
# nt <- 400
# nb <- 200000
# nc <- 3
# 
# nimble.data = list(
#   preg_status = as.numeric(cow_capture_all$time_2_preg) - 1,
#   lac_score = as.numeric((cow_capture_all$lac_score)),
#   # est_bm = as.numeric((cow_capture_all$est_bm)),
#   #dde_95 = as.numeric((cow_capture_all$area_DDE_gt_95)),
#   #dde_75 = as.numeric((cow_capture_all$area_DDE_gt_75)),
#   dde_m = as.numeric((cow_capture_all$area_DDE_gt_mean)),
#   age = as.numeric((cow_capture_all$age_estimate_2)),
#   #precip_year_prior = as.numeric((cow_capture_all$total_year_prior)),
#   sex_correct = ifelse(cow_capture_all$sex_correct== "m", 1,0),
#   lac_time= as.numeric(cow_capture_all$lac_time), 
#   time_1_preg= as.numeric(cow_capture_all$time_1_preg)-1,#
#   calving_date = as.numeric(cow_capture_all$birt_date),
#   #spei_3 = as.numeric((cow_capture_all$sept_year_prior_3M)),
#   spei_6 = as.numeric((cow_capture_all$sept_year_prior_6M))#,
#   #spei_12 = as.numeric((cow_capture_all$sept_year_prior_12M))
#   
# )
# 
# nimble.constants = list(n.obs = nrow(cow_capture_all))
# 
# 
# mcmc.output_half_m6_sb_bd_s <- nimbleMCMC(code = model_half_m6_sb_bd_s,
#                                         data = nimble.data,
#                                         constants=nimble.constants,
#                                         monitors = parameters,
#                                         niter = ni,
#                                         nburnin = nb,
#                                         nchains = nc,
#                                         thin=nt,
#                                         summary=TRUE,
#                                         samplesAsCodaMCMC = TRUE)
# 
# attach.nimble(mcmc.output_half_m6_sb_bd_s$samples)
# 
# 
# mata_half  <- do.call(rbind, lapply(mcmc.output_half_m6_sb_bd_s$samples, as.matrix))
# lpa_half  <- grep("^logProb_preg_status\\[", colnames(mata_half), value = TRUE)
# lla_half  <- mata_half[, lpa_half, drop = FALSE]
# stopifnot(all(is.finite(lla_half) | (is.infinite(lla_half) & lla_half < 0)))
# loo_half_m6_sb_bd_s <- loo(lla_half)
# 
# 
# 
# attach.nimble(mcmc.output_half_m6_sb_bd_s$samples)
# 
# MCMCtrace(object = mcmc.output_half_m6_sb_bd_s$samples,
#           pdf = FALSE, # no export to PDF
#           ind = TRUE, # separate density lines per chain
#           params = "time_1_preg_effect")
# # Gelman-Rubin diagnostic (AKA RHat or PSRF)
# gelman.diag(mcmc.output_half_m6_sb_bd_s$samples, multivariate = FALSE)
# 
# 
# 
# mcmcplot(mcmc.output_half_m6_sb_bd_s$samples)
# 
# 
# 
# ##how to get effects size
# ##jow to make the differnt charts in PP
# ##how to get time on deck chart
# ##uses zscore to preduct value. Use mean for all the other values
# ##will use for calf age ##what the fuck is a z-score?
# post_mat <- as.matrix(mcmc.output_half_m6_sb_bd_s$samples)
# # 2. Specify the parameters you care about
# params <- c(#"lac_effect",
#   #"bm_effect",
#   "dde_effect_m",
#   "age_effect",
#   "date_effect",
#   #"age2_effect",
#   # "precip_effect",
#   "Intercept", 
#   #"bod_con_effect",
#   #"lac_time_effect",
#   "sex_effect",
#   "time_1_preg_effect",
#   #"spei_3_effect",
#   "spei_6_effect"#,
#   #"spei_12_effect"#,
#   #"lac_score_preg_effect"#,
#   #"sex_lac_effect"
# )
# 
# # 3. Summarize each one
# summary_df_half_m6_sb_bd_s <- tibble(parameter = params) %>%
#   rowwise() %>%
#   mutate(
#     median   = median(post_mat[, parameter]),
#     lower95  = quantile(post_mat[, parameter], 0.025),
#     upper95  = quantile(post_mat[, parameter], 0.975),
#     # posterior probability effect > 0
#     p_gt_zero = mean(post_mat[, parameter] > 0),
#     # if you expect a negative effect, you can also compute P(<0):
#     p_lt_zero = mean(post_mat[, parameter] < 0)
#   ) %>%
#   ungroup()
# 
# print(summary_df_half_m6_sb_bd_s)
# get_variables(mcmc.output_half_m6_sb_bd_s$samples)
# 
# 
# summary_df_half_m6_sb_bd_s  %>%
#   kbl(digits = 3, caption = "Posterior Summary Statistics for Model bc_2") %>%
#   kable_styling(full_width = FALSE, position = "center")
# 
# # Full posterior distributions:
# 
# 
# 
# mod_24_fig<-mcmc.output_half_m6_sb_bd_s$samples%>%
#   spread_draws(#lac_effect,
#     # bm_effect,
#     dde_effect_m,
#     age_effect,
#     date_effect,
#     #age2_effect,
#     #precip_effect,
#     
#     # bod_con_effect,
#     #lac_time_effect,
#     sex_effect,
#     #lac_score_preg_effect,
#     time_1_preg_effect,
#     #spei_3_effect,
#     spei_6_effect#, 
#     #spei_12_effect
#   ) %>%
#   pivot_longer(-c(.chain, .iteration, .draw),
#                names_to = 'Parameter',
#                values_to = 'Estimate') %>%
#   ggplot(aes(y = Parameter, x = Estimate, fill = Parameter)) +
#   stat_halfeye(show.legend = FALSE) +
#   geom_vline(xintercept = 0, linetype = 2, color = 'black') +
#   scale_fill_manual(values = c(
#     #lac_time_effect = "grey60",  # make precip_effect blue
#     sex_effect = "red",
#     time_1_preg_effect = "grey60",
#     bod_con_effect = "red4",
#     lac_time_effect = "grey60",
#     age_effect = "blue4"
#   )) +
#   theme_minimal()
# mod_24_fig
# 
# ####end model 24#####
# 
# ####half full################mean dde, spei_6
# model_half_m6_bd_cm <-  nimbleCode({
#   
#   #Priors
#   Intercept  ~ dnorm(0, sd = 5)
#   
#   #lac_effect~ dt(mu=0, sigma=1, df=1)
#   #bm_effect ~ dt(mu=0, sigma=1, df=1)
#   dde_effect_m ~ dt(mu=0, sigma=1, df=1) 
#   age_effect ~ dt(mu=0, sigma=1, df=1)
#   date_effect~ dt(mu=0, sigma=1, df=1)
#   #date_effect~ dnorm(0, sd = 5)
#   # lac_time_effect ~dt(mu=0, sigma=1, df=1)
#   # sex_effect ~ dt(mu=0, sigma=1, df=1)
#   c_mass_effect ~ dt(mu = 0 , sigma = 1, df = 1)
#   time_1_preg_effect ~ dt(mu=0, sigma=1, df=1)
#   # spei_3_effect ~ dt(mu = 0 , sigma = 1, df = 1)
#   spei_6_effect ~ dt(mu = 0 , sigma = 1, df = 1)
#   #spei_12_effect ~ dt(mu = 0 , sigma = 1, df = 1)
#   # age2_effect ~ dt(mu = 0, sigma = 1, df = 1)
#   #lac_score_preg_effect ~ dt(mu = 0, sigma = 1, df = 1)
#   #sex_lac_effect ~ dt(mu = 0, sigma = 1, df = 1)
#   
#   
#   
#   for(i in 1:n.obs){
#     
#     #Process Model
#     logit(preg_prob[i])<-Intercept+
#       #(lac_score[i]*lac_effect)+
#       # (est_bm[i]*bm_effect)+
#       (dde_m[i]*dde_effect_m)+
#       (c_mass[i] * c_mass_effect)+
#       (age[i]*age_effect)+
#       # (pow(age[i], 2)*age2_effect)+
#       (calving_date[i] *date_effect )+
#       #(sex_correct[i] * sex_effect) +
#       #(lac_time[i]* lac_time_effect)+
#       (time_1_preg[i]* time_1_preg_effect)+
#       # (spei_3[i]*spei_3_effect)+
#       (spei_6[i]*spei_6_effect)#+
#     #(spei_12[i]*spei_12_effect)#+
#     #(lac_score[i]*lac_time[i]*lac_score_preg_effect)
#     
#     
#     
#     
#     
#     
#     #Observation Model (Likelihood)
#     preg_status[i] ~ dbern(preg_prob[i])
#     
#     #Impute missing data values
#     lac_score[i] ~ dnorm(mean=0,sd=1)
#     # precip_year_prior[i] ~ dnorm(mean = 0, sd = 1)
#     #dde_95[i] ~ dnorm(mean=0,sd=1)
#      age[i] ~ dnorm(mean=0,sd=1)
#      calving_date[i] ~ dnorm(mean=0,sd=1)
#     # est_body_cond[i] ~ dnorm(mean=0,sd=1)
#     # est_bm[i] ~ dnorm(mean=0,sd=1)
#     
#     #lac_time[i] ~ dnorm(mean=0,sd=1)##does this matrer
#     #spei_3[i] ~ dnorm(mean = 0, sd = 1)
#     # spei_6[i] ~ dnorm(mean = 0, sd = 1)
#     # spei_12[i] ~ dnorm(mean = 0, sd = 1)
#     
#     
#     
#   }#i
#   
# })#model5
# parameters <- c(#"lac_effect",
#   # "bm_effect",
#   "dde_effect_m",
#   "age_effect",
#   "date_effect",
#   "c_mass_effect",
#   # "age2_effect",
#   # "precip_effect",
#   "Intercept", 
#   # "bod_con_effect",
#   # "lac_time_effect",
#   #"sex_effect",
#   "time_1_preg_effect",
#   "logProb_preg_status",#
#   # "spei_3_effect",
#   "spei_6_effect"#,
#   #"spei_12_effect"#,
#   #"lac_score_preg_effect"#,
#   ##"sex_lac_effect"
# )
# 
# # MCMC Settings
# ni <- 400000
# nt <- 400
# nb <- 200000
# nc <- 3
# 
# nimble.data = list(
#   preg_status = as.numeric(cow_capture_all$time_2_preg) - 1,
#   lac_score = as.numeric((cow_capture_all$lac_score)),
#   c_mass = as.numeric((cow_capture_all$mass_calf)),
#   # est_bm = as.numeric((cow_capture_all$est_bm)),
#   #dde_95 = as.numeric((cow_capture_all$area_DDE_gt_95)),
#   #dde_75 = as.numeric((cow_capture_all$area_DDE_gt_75)),
#   dde_m = as.numeric((cow_capture_all$area_DDE_gt_mean)),
#   age = as.numeric((cow_capture_all$age_estimate_2)),
#   #precip_year_prior = as.numeric((cow_capture_all$total_year_prior)),
#   #sex_correct = ifelse(cow_capture_all$sex_correct== "m", 1,0),
#   lac_time= as.numeric(cow_capture_all$lac_time), 
#   time_1_preg= as.numeric(cow_capture_all$time_1_preg)-1,#
#   calving_date = as.numeric(cow_capture_all$birt_date),
#   #spei_3 = as.numeric((cow_capture_all$sept_year_prior_3M)),
#   spei_6 = as.numeric((cow_capture_all$sept_year_prior_6M))#,
#   #spei_12 = as.numeric((cow_capture_all$sept_year_prior_12M))
#   
# )
# 
# nimble.constants = list(n.obs = nrow(cow_capture_all))
# 
# 
# mcmc.output_half_m6_bd_cm<- nimbleMCMC(code = model_half_m6_bd_cm,
#                                         data = nimble.data,
#                                         constants=nimble.constants,
#                                         monitors = parameters,
#                                         niter = ni,
#                                         nburnin = nb,
#                                         nchains = nc,
#                                         thin=nt,
#                                         summary=TRUE,
#                                         samplesAsCodaMCMC = TRUE)
# 
# attach.nimble(mcmc.output_half_m6_bd_cm$samples)
# 
# 
# mata_half  <- do.call(rbind, lapply(mcmc.output_half_m6_bd_cm$samples, as.matrix))
# lpa_half  <- grep("^logProb_preg_status\\[", colnames(mata_half), value = TRUE)
# lla_half  <- mata_half[, lpa_half, drop = FALSE]
# stopifnot(all(is.finite(lla_half) | (is.infinite(lla_half) & lla_half < 0)))
# loo_half_m6_bd_cm <- loo(lla_half)
# 
# 
# 
# attach.nimble(mcmc.output_half_m6_bd_cm$samples)
# 
# MCMCtrace(object = mcmc.output_half_m6_bd_cm$samples,
#           pdf = FALSE, # no export to PDF
#           ind = TRUE, # separate density lines per chain
#           params = "time_1_preg_effect")
# # Gelman-Rubin diagnostic (AKA RHat or PSRF)
# gelman.diag(mcmc.output_half_m6_bd_cm$samples, multivariate = FALSE)
# 
# 
# 
# mcmcplot(mcmc.output_half_m6_bd_cm$samples)
# 
# 
# 
# ##how to get effects size
# ##jow to make the differnt charts in PP
# ##how to get time on deck chart
# ##uses zscore to preduct value. Use mean for all the other values
# ##will use for calf age ##what the fuck is a z-score?
# post_mat <- as.matrix(mcmc.output_half_m6_bd_cm$samples)
# # 2. Specify the parameters you care about
# params <- c(#"lac_effect",
#   #"bm_effect",
#   "dde_effect_m",
#   "age_effect",
#   "date_effect",
#   "c_mass_effect",
#   #"age2_effect",
#   # "precip_effect",
#   "Intercept", 
#   #"bod_con_effect",
#   #"lac_time_effect",
#   #"sex_effect",
#   "time_1_preg_effect",
#   #"spei_3_effect",
#   "spei_6_effect"#,
#   #"spei_12_effect"#,
#   #"lac_score_preg_effect"#,
#   #"sex_lac_effect"
# )
# 
# # 3. Summarize each one
# summary_df_half_m6_bd_cm <- tibble(parameter = params) %>%
#   rowwise() %>%
#   mutate(
#     median   = median(post_mat[, parameter]),
#     lower95  = quantile(post_mat[, parameter], 0.025),
#     upper95  = quantile(post_mat[, parameter], 0.975),
#     # posterior probability effect > 0
#     p_gt_zero = mean(post_mat[, parameter] > 0),
#     # if you expect a negative effect, you can also compute P(<0):
#     p_lt_zero = mean(post_mat[, parameter] < 0)
#   ) %>%
#   ungroup()
# 
# print(summary_df_half_m6_bd_cm)
# get_variables(mcmc.output_half_m6_bd_cm$samples)
# 
# 
# summary_df_half_m6_bd_cm %>%
#   kbl(digits = 3, caption = "Posterior Summary Statistics for Model bc_2") %>%
#   kable_styling(full_width = FALSE, position = "center")
# 
# # Full posterior distributions:
# 
# 
# 
# mod_25_fig<-mcmc.output_half_m6_bd_cm$samples%>%
#   spread_draws(#lac_effect,
#     # bm_effect,
#     dde_effect_m,
#     age_effect,
#     date_effect,
#     c_mass_effect,
#     #age2_effect,
#     #precip_effect,
#     
#     # bod_con_effect,
#     #lac_time_effect,
#     #sex_effect,
#     #lac_score_preg_effect,
#     time_1_preg_effect,
#     #spei_3_effect,
#     spei_6_effect#, 
#     #spei_12_effect
#   ) %>%
#   pivot_longer(-c(.chain, .iteration, .draw),
#                names_to = 'Parameter',
#                values_to = 'Estimate') %>%
#   ggplot(aes(y = Parameter, x = Estimate, fill = Parameter)) +
#   stat_halfeye(show.legend = FALSE) +
#   geom_vline(xintercept = 0, linetype = 2, color = 'black') +
#   scale_fill_manual(values = c(
#     #lac_time_effect = "grey60",  # make precip_effect blue
#     sex_effect = "red",
#     time_1_preg_effect = "grey60",
#     bod_con_effect = "red4",
#     lac_time_effect = "grey60",
#     age_effect = "blue4"
#   )) +
#   theme_minimal()
# mod_25_fig
# 
# ####end model 25#####
# 
# ####half full################mean dde, spei_6
# model_half_m6_sb_bd_axpreg <-  nimbleCode({
#   
#   #Priors
#   Intercept  ~ dnorm(0, sd = 5)
#   
#   #lac_effect~ dt(mu=0, sigma=1, df=1)
#   #bm_effect ~ dt(mu=0, sigma=1, df=1)
#   dde_effect_m ~ dt(mu=0, sigma=1, df=1) 
#   age_effect ~ dt(mu=0, sigma=1, df=1)
#   date_effect~ dt(mu=0, sigma=1, df=1)
#   #date_effect~ dnorm(0, sd = 5)
#   # lac_time_effect ~dt(mu=0, sigma=1, df=1)
#   # sex_effect ~ dt(mu=0, sigma=1, df=1)
#   
#   time_1_preg_effect ~ dt(mu=0, sigma=1, df=1)
#   # spei_3_effect ~ dt(mu = 0 , sigma = 1, df = 1)
#   spei_6_effect ~ dt(mu = 0 , sigma = 1, df = 1)
#   #spei_12_effect ~ dt(mu = 0 , sigma = 1, df = 1)
#   # age2_effect ~ dt(mu = 0, sigma = 1, df = 1)
#   lac_score_preg_effect ~ dt(mu = 0, sigma = 1, df = 1)
#   age_preg_effect ~ dt(mu = 0, sigma = 1, df = 1)
#   #sex_lac_effect ~ dt(mu = 0, sigma = 1, df = 1)
#   
#   
#   
#   for(i in 1:n.obs){
#     
#     #Process Model
#     logit(preg_prob[i])<-Intercept+
#       #(lac_score[i]*lac_effect)+
#       # (est_bm[i]*bm_effect)+
#       (dde_m[i]*dde_effect_m)+
#       (age[i]*age_effect)+
#       # (pow(age[i], 2)*age2_effect)+
#       (calving_date[i] *date_effect )+
#       #(sex_correct[i] * sex_effect) +
#       #(lac_time[i]* lac_time_effect)+
#       (time_1_preg[i]* time_1_preg_effect)+
#       # (spei_3[i]*spei_3_effect)+
#       (spei_6[i]*spei_6_effect)+
#     #(spei_12[i]*spei_12_effect)#+
#     (age[i]*time_1_preg[i]*age_preg_effect)
#     
#     
#     
#     
#     
#     
#     #Observation Model (Likelihood)
#     preg_status[i] ~ dbern(preg_prob[i])
#     
#     #Impute missing data values
#     lac_score[i] ~ dnorm(mean=0,sd=1)
#     calving_date[i] ~ dnorm(mean=0,sd=1) 
#     # precip_year_prior[i] ~ dnorm(mean = 0, sd = 1)
#     #dde_95[i] ~ dnorm(mean=0,sd=1)
#      age[i] ~ dnorm(mean=0,sd=1)
#     # est_body_cond[i] ~ dnorm(mean=0,sd=1)
#     # est_bm[i] ~ dnorm(mean=0,sd=1)
#     
#     #lac_time[i] ~ dnorm(mean=0,sd=1)##does this matrer
#     #spei_3[i] ~ dnorm(mean = 0, sd = 1)
#     # spei_6[i] ~ dnorm(mean = 0, sd = 1)
#     # spei_12[i] ~ dnorm(mean = 0, sd = 1)
#     
#     
#     
#   }#i
#   
# })#model5
# parameters <- c(#"lac_effect",
#   # "bm_effect",
#   "dde_effect_m",
#   "age_effect",
#   "date_effect",
#   "age_preg_effect",
#   # "age2_effect",
#   # "precip_effect",
#   "Intercept", 
#   # "bod_con_effect",
#   # "lac_time_effect",
#   #"sex_effect",
#   "time_1_preg_effect",
#   "logProb_preg_status",#
#   # "spei_3_effect",
#   "spei_6_effect"#,
#   #"spei_12_effect"#,
#   #"lac_score_preg_effect"#,
#   ##"sex_lac_effect"
# )
# 
# # MCMC Settings
# ni <- 400000
# nt <- 400
# nb <- 200000
# nc <- 3
# 
# nimble.data = list(
#   preg_status = as.numeric(cow_capture_all$time_2_preg) - 1,
#   lac_score = as.numeric((cow_capture_all$lac_score)),
#   # est_bm = as.numeric((cow_capture_all$est_bm)),
#   #dde_95 = as.numeric((cow_capture_all$area_DDE_gt_95)),
#   #dde_75 = as.numeric((cow_capture_all$area_DDE_gt_75)),
#   dde_m = as.numeric((cow_capture_all$area_DDE_gt_mean)),
#   age = as.numeric((cow_capture_all$age_estimate_2)),
#   #precip_year_prior = as.numeric((cow_capture_all$total_year_prior)),
#   sex_correct = ifelse(cow_capture_all$sex_correct== "m", 1,0),
#   lac_time= as.numeric(cow_capture_all$lac_time), 
#   time_1_preg= as.numeric(cow_capture_all$time_1_preg)-1,#
#   calving_date = as.numeric(cow_capture_all$birt_date),
#   #spei_3 = as.numeric((cow_capture_all$sept_year_prior_3M)),
#   spei_6 = as.numeric((cow_capture_all$sept_year_prior_6M))#,
#   #spei_12 = as.numeric((cow_capture_all$sept_year_prior_12M))
#   
# )
# 
# nimble.constants = list(n.obs = nrow(cow_capture_all))
# 
# 
# mcmc.output_half_m6_sb_bd_axpreg <- nimbleMCMC(code = model_half_m6_sb_bd_axpreg ,
#                                         data = nimble.data,
#                                         constants=nimble.constants,
#                                         monitors = parameters,
#                                         niter = ni,
#                                         nburnin = nb,
#                                         nchains = nc,
#                                         thin=nt,
#                                         summary=TRUE,
#                                         samplesAsCodaMCMC = TRUE)
# 
# attach.nimble(mcmc.output_half_m6_sb_bd_axpreg $samples)
# 
# 
# mata_half  <- do.call(rbind, lapply(mcmc.output_half_m6_sb_bd_axpreg $samples, as.matrix))
# lpa_half  <- grep("^logProb_preg_status\\[", colnames(mata_half), value = TRUE)
# lla_half  <- mata_half[, lpa_half, drop = FALSE]
# stopifnot(all(is.finite(lla_half) | (is.infinite(lla_half) & lla_half < 0)))
# loo_half_m6_sb_bd_axpreg  <- loo(lla_half)
# 
# 
# 
# attach.nimble(mcmc.output_half_m6_sb_bd_axpreg$samples)
# 
# MCMCtrace(object = mcmc.output_half_m6_sb_bd_axpreg$samples,
#           pdf = FALSE, # no export to PDF
#           ind = TRUE, # separate density lines per chain
#           params = "time_1_preg_effect")
# # Gelman-Rubin diagnostic (AKA RHat or PSRF)
# gelman.diag(mcmc.output_half_m6_sb_bd_axpreg$samples, multivariate = FALSE)
# 
# 
# 
# mcmcplot(mcmc.output_half_m6_sb_bd_axpreg$samples)
# 
# 
# 
# ##how to get effects size
# ##jow to make the differnt charts in PP
# ##how to get time on deck chart
# ##uses zscore to preduct value. Use mean for all the other values
# ##will use for calf age ##what the fuck is a z-score?
# post_mat <- as.matrix(mcmc.output_half_m6_sb_bd_axpreg$samples)
# # 2. Specify the parameters you care about
# params <- c(#"lac_effect",
#   #"bm_effect",
#   "dde_effect_m",
#   "age_effect",
#   "date_effect",
#   "age_preg_effect",
#   #"age2_effect",
#   # "precip_effect",
#   "Intercept", 
#   #"bod_con_effect",
#   #"lac_time_effect",
#   #"sex_effect",
#   "time_1_preg_effect",
#   #"spei_3_effect",
#   "spei_6_effect"#,
#   #"spei_12_effect"#,
#   #"lac_score_preg_effect"#,
#   #"sex_lac_effect"
# )
# 
# # 3. Summarize each one
# summary_df_half_m6_sb_bd_axpreg <- tibble(parameter = params) %>%
#   rowwise() %>%
#   mutate(
#     median   = median(post_mat[, parameter]),
#     lower95  = quantile(post_mat[, parameter], 0.025),
#     upper95  = quantile(post_mat[, parameter], 0.975),
#     # posterior probability effect > 0
#     p_gt_zero = mean(post_mat[, parameter] > 0),
#     # if you expect a negative effect, you can also compute P(<0):
#     p_lt_zero = mean(post_mat[, parameter] < 0)
#   ) %>%
#   ungroup()
# 
# print(summary_df_half_m6_sb_bd_axpreg)
# get_variables(mcmc.output_half_m6_sb_bd_axpreg$samples)
# 
# 
# summary_df_half_m6_sb_bd_axpreg  %>%
#   kbl(digits = 3, caption = "Posterior Summary Statistics for Model bc_2") %>%
#   kable_styling(full_width = FALSE, position = "center")
# 
# # Full posterior distributions:
# 
# 
# 
# mod_26_fig<-mcmc.output_half_m6_sb_bd_axpreg$samples%>%
#   spread_draws(#lac_effect,
#     # bm_effect,
#     dde_effect_m,
#     age_effect,
#     date_effect,
#     age_preg_effect,
#     #age2_effect,
#     #precip_effect,
#     
#     # bod_con_effect,
#     #lac_time_effect,
#     #sex_effect,
#     #lac_score_preg_effect,
#     time_1_preg_effect,
#     #spei_3_effect,
#     spei_6_effect#, 
#     #spei_12_effect
#   ) %>%
#   pivot_longer(-c(.chain, .iteration, .draw),
#                names_to = 'Parameter',
#                values_to = 'Estimate') %>%
#   ggplot(aes(y = Parameter, x = Estimate, fill = Parameter)) +
#   stat_halfeye(show.legend = FALSE) +
#   geom_vline(xintercept = 0, linetype = 2, color = 'black') +
#   scale_fill_manual(values = c(
#     #lac_time_effect = "grey60",  # make precip_effect blue
#     sex_effect = "red",
#     time_1_preg_effect = "grey60",
#     bod_con_effect = "red4",
#     lac_time_effect = "grey60",
#     age_effect = "blue4"
#   )) +
#   theme_minimal()
# mod_26_fig
# 
# ##end model 26
# 
# ####half full################mean dde, spei_6
# model_half_m6_sb_bd_bm_age <-  nimbleCode({
#   
#   #Priors
#   Intercept  ~ dnorm(0, sd = 5)
#   
#   #lac_effect~ dt(mu=0, sigma=1, df=1)
#   bm_effect ~ dt(mu=0, sigma=1, df=1)
#   dde_effect_m ~ dt(mu=0, sigma=1, df=1) 
#   age_effect ~ dt(mu=0, sigma=1, df=1)
#   date_effect~ dt(mu=0, sigma=1, df=1)
#   #date_effect~ dnorm(0, sd = 5)
#   # lac_time_effect ~dt(mu=0, sigma=1, df=1)
#   # sex_effect ~ dt(mu=0, sigma=1, df=1)
#   
#   time_1_preg_effect ~ dt(mu=0, sigma=1, df=1)
#   # spei_3_effect ~ dt(mu = 0 , sigma = 1, df = 1)
#   spei_6_effect ~ dt(mu = 0 , sigma = 1, df = 1)
#   #spei_12_effect ~ dt(mu = 0 , sigma = 1, df = 1)
#   # age2_effect ~ dt(mu = 0, sigma = 1, df = 1)
#   lac_score_preg_effect ~ dt(mu = 0, sigma = 1, df = 1)
#   age_bm_effect ~ dt(mu = 0, sigma = 1, df = 1)
#   #sex_lac_effect ~ dt(mu = 0, sigma = 1, df = 1)
#   
#   
#   
#   for(i in 1:n.obs){
#     
#     #Process Model
#     logit(preg_prob[i])<-Intercept+
#       #(lac_score[i]*lac_effect)+
#        (est_bm[i]*bm_effect)+
#       (dde_m[i]*dde_effect_m)+
#       (age[i]*age_effect)+
#       # (pow(age[i], 2)*age2_effect)+
#       (calving_date[i] *date_effect )+
#       #(sex_correct[i] * sex_effect) +
#       #(lac_time[i]* lac_time_effect)+
#       (time_1_preg[i]* time_1_preg_effect)+
#       # (spei_3[i]*spei_3_effect)+
#       (spei_6[i]*spei_6_effect)+
#       #(spei_12[i]*spei_12_effect)#+
#       (age[i]*est_bm[i]*age_bm_effect)
#     
#     
#     
#     
#     
#     
#     #Observation Model (Likelihood)
#     preg_status[i] ~ dbern(preg_prob[i])
#     
#     #Impute missing data values
#     lac_score[i] ~ dnorm(mean=0,sd=1)
#     calving_date[i] ~ dnorm(mean=0,sd=1)
#     # precip_year_prior[i] ~ dnorm(mean = 0, sd = 1)
#     #dde_95[i] ~ dnorm(mean=0,sd=1)
#      age[i] ~ dnorm(mean=0,sd=1)
#     # est_body_cond[i] ~ dnorm(mean=0,sd=1)
#      est_bm[i] ~ dnorm(mean=0,sd=1)
#     
#     #lac_time[i] ~ dnorm(mean=0,sd=1)##does this matrer
#     #spei_3[i] ~ dnorm(mean = 0, sd = 1)
#     # spei_6[i] ~ dnorm(mean = 0, sd = 1)
#     # spei_12[i] ~ dnorm(mean = 0, sd = 1)
#     
#     
#     
#   }#i
#   
# })#model5
# parameters <- c(#"lac_effect",
#    "bm_effect",
#   "dde_effect_m",
#   "age_effect",
#   "date_effect",
#   "age_bm_effect",
#   # "age2_effect",
#   # "precip_effect",
#   "Intercept", 
#   # "bod_con_effect",
#   # "lac_time_effect",
#   #"sex_effect",
#   "time_1_preg_effect",
#   "logProb_preg_status",#
#   # "spei_3_effect",
#   "spei_6_effect"#,
#   #"spei_12_effect"#,
#   #"lac_score_preg_effect"#,
#   ##"sex_lac_effect"
# )
# 
# # MCMC Settings
# ni <- 400000
# nt <- 400
# nb <- 200000
# nc <- 3
# 
# nimble.data = list(
#   preg_status = as.numeric(cow_capture_all$time_2_preg) - 1,
#   lac_score = as.numeric((cow_capture_all$lac_score)),
#    est_bm = as.numeric((cow_capture_all$est_bm)),
#   #dde_95 = as.numeric((cow_capture_all$area_DDE_gt_95)),
#   #dde_75 = as.numeric((cow_capture_all$area_DDE_gt_75)),
#   dde_m = as.numeric((cow_capture_all$area_DDE_gt_mean)),
#   age = as.numeric((cow_capture_all$age_estimate_2)),
#   #precip_year_prior = as.numeric((cow_capture_all$total_year_prior)),
#   sex_correct = ifelse(cow_capture_all$sex_correct== "m", 1,0),
#   lac_time= as.numeric(cow_capture_all$lac_time), 
#   time_1_preg= as.numeric(cow_capture_all$time_1_preg)-1,#
#   calving_date = as.numeric(cow_capture_all$birt_date),
#   #spei_3 = as.numeric((cow_capture_all$sept_year_prior_3M)),
#   spei_6 = as.numeric((cow_capture_all$sept_year_prior_6M))#,
#   #spei_12 = as.numeric((cow_capture_all$sept_year_prior_12M))
#   
# )
# 
# nimble.constants = list(n.obs = nrow(cow_capture_all))
# 
# 
# mcmc.output_half_m6_sb_bd_bm_age <- nimbleMCMC(code = model_half_m6_sb_bd_bm_age,
#                                                data = nimble.data,
#                                                constants=nimble.constants,
#                                                monitors = parameters,
#                                                niter = ni,
#                                                nburnin = nb,
#                                                nchains = nc,
#                                                thin=nt,
#                                                summary=TRUE,
#                                                samplesAsCodaMCMC = TRUE)
# 
# attach.nimble(mcmc.output_half_m6_sb_bd_bm_age$samples)
# 
# 
# mata_half  <- do.call(rbind, lapply(mcmc.output_half_m6_sb_bd_bm_age$samples, as.matrix))
# lpa_half  <- grep("^logProb_preg_status\\[", colnames(mata_half), value = TRUE)
# lla_half  <- mata_half[, lpa_half, drop = FALSE]
# stopifnot(all(is.finite(lla_half) | (is.infinite(lla_half) & lla_half < 0)))
# loo_half_m6_sb_bd_bm_age <- loo(lla_half)
# 
# 
# 
# attach.nimble(mcmc.output_half_m6_sb_bd_bm_age$samples)
# 
# MCMCtrace(object = mcmc.output_half_m6_sb_bd_bm_age$samples,
#           pdf = FALSE, # no export to PDF
#           ind = TRUE, # separate density lines per chain
#           params = "time_1_preg_effect")
# # Gelman-Rubin diagnostic (AKA RHat or PSRF)
# gelman.diag(mcmc.output_half_m6_sb_bd_bm_age$samples, multivariate = FALSE)
# 
# 
# 
# mcmcplot(mcmc.output_half_m6_sb_bd_axpreg$samples)
# 
# 
# 
# ##how to get effects size
# ##jow to make the differnt charts in PP
# ##how to get time on deck chart
# ##uses zscore to preduct value. Use mean for all the other values
# ##will use for calf age ##what the fuck is a z-score?
# post_mat <- as.matrix(mcmc.output_half_m6_sb_bd_bm_age$samples)
# # 2. Specify the parameters you care about
# params <- c(#"lac_effect",
#   "bm_effect",
#   "dde_effect_m",
#   "age_effect",
#   "date_effect",
#   "age_bm_effect",
#   #"age2_effect",
#   # "precip_effect",
#   "Intercept", 
#   #"bod_con_effect",
#   #"lac_time_effect",
#   #"sex_effect",
#   "time_1_preg_effect",
#   #"spei_3_effect",
#   "spei_6_effect"#,
#   #"spei_12_effect"#,
#   #"lac_score_preg_effect"#,
#   #"sex_lac_effect"
# )
# 
# # 3. Summarize each one
# summary_df_half_m6_sb_bd_bm_age <- tibble(parameter = params) %>%
#   rowwise() %>%
#   mutate(
#     median   = median(post_mat[, parameter]),
#     lower95  = quantile(post_mat[, parameter], 0.025),
#     upper95  = quantile(post_mat[, parameter], 0.975),
#     # posterior probability effect > 0
#     p_gt_zero = mean(post_mat[, parameter] > 0),
#     # if you expect a negative effect, you can also compute P(<0):
#     p_lt_zero = mean(post_mat[, parameter] < 0)
#   ) %>%
#   ungroup()
# 
# print(summary_df_half_m6_sb_bd_bm_age)
# get_variables(mcmc.output_half_m6_sb_bd_bm_age$samples)
# 
# 
# summary_df_half_m6_sb_bd_bm_age  %>%
#   kbl(digits = 3, caption = "Posterior Summary Statistics for Model bc_2") %>%
#   kable_styling(full_width = FALSE, position = "center")
# 
# # Full posterior distributions:
# 
# 
# 
# mod_27_fig<-mcmc.output_half_m6_sb_bd_bm_age$samples%>%
#   spread_draws(#lac_effect,
#      bm_effect,
#     dde_effect_m,
#     age_effect,
#     date_effect,
#     age_bm_effect,
#     #age2_effect,
#     #precip_effect,
#     
#     # bod_con_effect,
#     #lac_time_effect,
#     #sex_effect,
#     #lac_score_preg_effect,
#     time_1_preg_effect,
#     #spei_3_effect,
#     spei_6_effect#, 
#     #spei_12_effect
#   ) %>%
#   pivot_longer(-c(.chain, .iteration, .draw),
#                names_to = 'Parameter',
#                values_to = 'Estimate') %>%
#   ggplot(aes(y = Parameter, x = Estimate, fill = Parameter)) +
#   stat_halfeye(show.legend = FALSE) +
#   geom_vline(xintercept = 0, linetype = 2, color = 'black') +
#   scale_fill_manual(values = c(
#     #lac_time_effect = "grey60",  # make precip_effect blue
#     sex_effect = "red",
#     time_1_preg_effect = "grey60",
#     bod_con_effect = "red4",
#     lac_time_effect = "grey60",
#     age_effect = "blue4"
#   )) +
#   theme_minimal()
# mod_27_fig
# 
# ##end model 27
# ####half full################mean dde, spei_6
# model_half_m6_sb_bd_bm_preg <-  nimbleCode({
#   
#   #Priors
#   Intercept  ~ dnorm(0, sd = 5)
#   
#   #lac_effect~ dt(mu=0, sigma=1, df=1)
#   bm_effect ~ dt(mu=0, sigma=1, df=1)
#   dde_effect_m ~ dt(mu=0, sigma=1, df=1) 
#   age_effect ~ dt(mu=0, sigma=1, df=1)
#   date_effect~ dt(mu=0, sigma=1, df=1)
#   #date_effect~ dnorm(0, sd = 5)
#   # lac_time_effect ~dt(mu=0, sigma=1, df=1)
#   # sex_effect ~ dt(mu=0, sigma=1, df=1)
#   
#   time_1_preg_effect ~ dt(mu=0, sigma=1, df=1)
#   # spei_3_effect ~ dt(mu = 0 , sigma = 1, df = 1)
#   spei_6_effect ~ dt(mu = 0 , sigma = 1, df = 1)
#   #spei_12_effect ~ dt(mu = 0 , sigma = 1, df = 1)
#   # age2_effect ~ dt(mu = 0, sigma = 1, df = 1)
#   lac_score_preg_effect ~ dt(mu = 0, sigma = 1, df = 1)
#   preg_bm_effect ~ dt(mu = 0, sigma = 1, df = 1)
#   #sex_lac_effect ~ dt(mu = 0, sigma = 1, df = 1)
#   
#   
#   
#   for(i in 1:n.obs){
#     
#     #Process Model
#     logit(preg_prob[i])<-Intercept+
#       #(lac_score[i]*lac_effect)+
#       (est_bm[i]*bm_effect)+
#       (dde_m[i]*dde_effect_m)+
#       (age[i]*age_effect)+
#       # (pow(age[i], 2)*age2_effect)+
#       (calving_date[i] *date_effect )+
#       #(sex_correct[i] * sex_effect) +
#       #(lac_time[i]* lac_time_effect)+
#       (time_1_preg[i]* time_1_preg_effect)+
#       # (spei_3[i]*spei_3_effect)+
#       (spei_6[i]*spei_6_effect)+
#       #(spei_12[i]*spei_12_effect)#+
#       (time_1_preg[i]*est_bm[i]*preg_bm_effect)
#     
#     
#     
#     
#     
#     
#     #Observation Model (Likelihood)
#     preg_status[i] ~ dbern(preg_prob[i])
#     
#     #Impute missing data values
#     lac_score[i] ~ dnorm(mean=0,sd=1)
#     # precip_year_prior[i] ~ dnorm(mean = 0, sd = 1)
#     #dde_95[i] ~ dnorm(mean=0,sd=1)
#     age[i] ~ dnorm(mean=0,sd=1)
#     calving_date[i] ~ dnorm(mean=0,sd=1)
#     # est_body_cond[i] ~ dnorm(mean=0,sd=1)
#     est_bm[i] ~ dnorm(mean=0,sd=1)
#     
#     #lac_time[i] ~ dnorm(mean=0,sd=1)##does this matrer
#     #spei_3[i] ~ dnorm(mean = 0, sd = 1)
#     # spei_6[i] ~ dnorm(mean = 0, sd = 1)
#     # spei_12[i] ~ dnorm(mean = 0, sd = 1)
#     
#     
#     
#   }#i
#   
# })#model5
# parameters <- c(#"lac_effect",
#   "bm_effect",
#   "dde_effect_m",
#   "age_effect",
#   "date_effect",
#   "preg_bm_effect",
#   # "age2_effect",
#   # "precip_effect",
#   "Intercept", 
#   # "bod_con_effect",
#   # "lac_time_effect",
#   #"sex_effect",
#   "time_1_preg_effect",
#   "logProb_preg_status",#
#   # "spei_3_effect",
#   "spei_6_effect"#,
#   #"spei_12_effect"#,
#   #"lac_score_preg_effect"#,
#   ##"sex_lac_effect"
# )
# 
# # MCMC Settings
# ni <- 400000
# nt <- 400
# nb <- 200000
# nc <- 3
# 
# nimble.data = list(
#   preg_status = as.numeric(cow_capture_all$time_2_preg) - 1,
#   lac_score = as.numeric((cow_capture_all$lac_score)),
#   est_bm = as.numeric((cow_capture_all$est_bm)),
#   #dde_95 = as.numeric((cow_capture_all$area_DDE_gt_95)),
#   #dde_75 = as.numeric((cow_capture_all$area_DDE_gt_75)),
#   dde_m = as.numeric((cow_capture_all$area_DDE_gt_mean)),
#   age = as.numeric((cow_capture_all$age_estimate_2)),
#   #precip_year_prior = as.numeric((cow_capture_all$total_year_prior)),
#   sex_correct = ifelse(cow_capture_all$sex_correct== "m", 1,0),
#   lac_time= as.numeric(cow_capture_all$lac_time), 
#   time_1_preg= as.numeric(cow_capture_all$time_1_preg)-1,#
#   calving_date = as.numeric(cow_capture_all$birt_date),
#   #spei_3 = as.numeric((cow_capture_all$sept_year_prior_3M)),
#   spei_6 = as.numeric((cow_capture_all$sept_year_prior_6M))#,
#   #spei_12 = as.numeric((cow_capture_all$sept_year_prior_12M))
#   
# )
# 
# nimble.constants = list(n.obs = nrow(cow_capture_all))
# 
# 
# mcmc.output_half_m6_sb_bd_bm_preg  <- nimbleMCMC(code = model_half_m6_sb_bd_bm_preg ,
#                                                data = nimble.data,
#                                                constants=nimble.constants,
#                                                monitors = parameters,
#                                                niter = ni,
#                                                nburnin = nb,
#                                                nchains = nc,
#                                                thin=nt,
#                                                summary=TRUE,
#                                                samplesAsCodaMCMC = TRUE)
# 
# attach.nimble(mcmc.output_half_m6_sb_bd_bm_preg $samples)
# 
# 
# mata_half  <- do.call(rbind, lapply(mcmc.output_half_m6_sb_bd_bm_preg $samples, as.matrix))
# lpa_half  <- grep("^logProb_preg_status\\[", colnames(mata_half), value = TRUE)
# lla_half  <- mata_half[, lpa_half, drop = FALSE]
# stopifnot(all(is.finite(lla_half) | (is.infinite(lla_half) & lla_half < 0)))
# loo_half_m6_sb_bd_bm_preg  <- loo(lla_half)
# 
# 
# 
# attach.nimble(mcmc.output_half_m6_sb_bd_bm_preg$samples)
# 
# MCMCtrace(object = mcmc.output_half_m6_sb_bd_bm_preg$samples,
#           pdf = FALSE, # no export to PDF
#           ind = TRUE, # separate density lines per chain
#           params = "time_1_preg_effect")
# # Gelman-Rubin diagnostic (AKA RHat or PSRF)
# gelman.diag(mcmc.output_half_m6_sb_bd_bm_preg$samples, multivariate = FALSE)
# 
# 
# 
# mcmcplot(mcmc.output_half_m6_sb_bd_bm_preg$samples)
# 
# 
# 
# ##how to get effects size
# ##jow to make the differnt charts in PP
# ##how to get time on deck chart
# ##uses zscore to preduct value. Use mean for all the other values
# ##will use for calf age ##what the fuck is a z-score?
# post_mat <- as.matrix(mcmc.output_half_m6_sb_bd_bm_preg$samples)
# # 2. Specify the parameters you care about
# params <- c(#"lac_effect",
#   "bm_effect",
#   "dde_effect_m",
#   "age_effect",
#   "date_effect",
#   "preg_bm_effect",
#   #"age2_effect",
#   # "precip_effect",
#   "Intercept", 
#   #"bod_con_effect",
#   #"lac_time_effect",
#   #"sex_effect",
#   "time_1_preg_effect",
#   #"spei_3_effect",
#   "spei_6_effect"#,
#   #"spei_12_effect"#,
#   #"lac_score_preg_effect"#,
#   #"sex_lac_effect"
# )
# 
# # 3. Summarize each one
# summary_df_half_m6_sb_bd_bm_preg <- tibble(parameter = params) %>%
#   rowwise() %>%
#   mutate(
#     median   = median(post_mat[, parameter]),
#     lower95  = quantile(post_mat[, parameter], 0.025),
#     upper95  = quantile(post_mat[, parameter], 0.975),
#     # posterior probability effect > 0
#     p_gt_zero = mean(post_mat[, parameter] > 0),
#     # if you expect a negative effect, you can also compute P(<0):
#     p_lt_zero = mean(post_mat[, parameter] < 0)
#   ) %>%
#   ungroup()
# 
# print(summary_df_half_m6_sb_bd_bm_preg)
# get_variables(mcmc.output_half_m6_sb_bd_bm_age$samples)
# 
# 
# summary_df_half_m6_sb_bd_bm_preg %>%
#   kbl(digits = 3, caption = "Posterior Summary Statistics for Model bc_2") %>%
#   kable_styling(full_width = FALSE, position = "center")
# 
# # Full posterior distributions:
# 
# 
# 
# mod_28_fig<-mcmc.output_half_m6_sb_bd_bm_preg$samples%>%
#   spread_draws(#lac_effect,
#     bm_effect,
#     dde_effect_m,
#     age_effect,
#     date_effect,
#     preg_bm_effect,
#     #age2_effect,
#     #precip_effect,
#     
#     # bod_con_effect,
#     #lac_time_effect,
#     #sex_effect,
#     #lac_score_preg_effect,
#     time_1_preg_effect,
#     #spei_3_effect,
#     spei_6_effect#, 
#     #spei_12_effect
#   ) %>%
#   pivot_longer(-c(.chain, .iteration, .draw),
#                names_to = 'Parameter',
#                values_to = 'Estimate') %>%
#   ggplot(aes(y = Parameter, x = Estimate, fill = Parameter)) +
#   stat_halfeye(show.legend = FALSE) +
#   geom_vline(xintercept = 0, linetype = 2, color = 'black') +
#   scale_fill_manual(values = c(
#     #lac_time_effect = "grey60",  # make precip_effect blue
#     sex_effect = "red",
#     time_1_preg_effect = "grey60",
#     bod_con_effect = "red4",
#     lac_time_effect = "grey60",
#     age_effect = "blue4"
#   )) +
#   theme_minimal()
# mod_28_fig
# 
# ##end model 28
# ##add calf mass
# 
# ####half full################mean dde, spei_6
# model_half_add_cm <-  nimbleCode({
#   
#   #Priors
#   Intercept  ~ dnorm(0, sd = 5)
#   
#   #lac_effect~ dt(mu=0, sigma=1, df=1)
#   bm_effect ~ dt(mu=0, sigma=1, df=1)
#   dde_effect_m ~ dt(mu=0, sigma=1, df=1) 
#   age_effect ~ dt(mu=0, sigma=1, df=1)
#   date_effect~ dt(mu=0, sigma=1, df=1)
#   c_m_effect~ dt(mu=0, sigma=1, df=1)
#   #date_effect~ dnorm(0, sd = 5)
#   # lac_time_effect ~dt(mu=0, sigma=1, df=1)
#   # sex_effect ~ dt(mu=0, sigma=1, df=1)
#   
#   time_1_preg_effect ~ dt(mu=0, sigma=1, df=1)
#   # spei_3_effect ~ dt(mu = 0 , sigma = 1, df = 1)
#   spei_6_effect ~ dt(mu = 0 , sigma = 1, df = 1)
#   #spei_12_effect ~ dt(mu = 0 , sigma = 1, df = 1)
#   # age2_effect ~ dt(mu = 0, sigma = 1, df = 1)
#   lac_score_preg_effect ~ dt(mu = 0, sigma = 1, df = 1)
#   age_bm_effect ~ dt(mu = 0, sigma = 1, df = 1)
#   #sex_lac_effect ~ dt(mu = 0, sigma = 1, df = 1)
#   
#   
#   
#   for(i in 1:n.obs){
#     
#     #Process Model
#     logit(preg_prob[i])<-Intercept+
#       #(lac_score[i]*lac_effect)+
#       (est_bm[i]*bm_effect)+
#       (dde_m[i]*dde_effect_m)+
#       (age[i]*age_effect)+
#       (calf_mass[i]* c_m_effect)+
#       # (pow(age[i], 2)*age2_effect)+
#       (calving_date[i] *date_effect )+
#       #(sex_correct[i] * sex_effect) +
#       #(lac_time[i]* lac_time_effect)+
#       (time_1_preg[i]* time_1_preg_effect)+
#       # (spei_3[i]*spei_3_effect)+
#       (spei_6[i]*spei_6_effect)+
#       #(spei_12[i]*spei_12_effect)#+
#       (age[i]*est_bm[i]*age_bm_effect)
#     
#     
#     
#     
#     
#     
#     #Observation Model (Likelihood)
#     preg_status[i] ~ dbern(preg_prob[i])
#     
#     #Impute missing data values
#     lac_score[i] ~ dnorm(mean=0,sd=1)
#     calving_date[i] ~ dnorm(mean=0,sd=1)
#     # precip_year_prior[i] ~ dnorm(mean = 0, sd = 1)
#     #dde_95[i] ~ dnorm(mean=0,sd=1)
#      age[i] ~ dnorm(mean=0,sd=1)
#     # est_body_cond[i] ~ dnorm(mean=0,sd=1)
#     est_bm[i] ~ dnorm(mean=0,sd=1)
#     
#     #lac_time[i] ~ dnorm(mean=0,sd=1)##does this matrer
#     #spei_3[i] ~ dnorm(mean = 0, sd = 1)
#     # spei_6[i] ~ dnorm(mean = 0, sd = 1)
#     # spei_12[i] ~ dnorm(mean = 0, sd = 1)
#     
#     
#     
#   }#i
#   
# })#model5
# parameters <- c("c_m_effect",
#   "bm_effect",
#   "dde_effect_m",
#   "age_effect",
#   "date_effect",
#   "age_bm_effect",
#   # "age2_effect",
#   # "precip_effect",
#   "Intercept", 
#   # "bod_con_effect",
#   # "lac_time_effect",
#   #"sex_effect",
#   "time_1_preg_effect",
#   "logProb_preg_status",#
#   # "spei_3_effect",
#   "spei_6_effect"#,
#   #"spei_12_effect"#,
#   #"lac_score_preg_effect"#,
#   ##"sex_lac_effect"
# )
# 
# # MCMC Settings
# ni <- 400000
# nt <- 400
# nb <- 200000
# nc <- 3
# 
# nimble.data = list(
#   calf_mass = as.numeric(cow_capture_all$mass_calf),
#   preg_status = as.numeric(cow_capture_all$time_2_preg) - 1,
#   lac_score = as.numeric((cow_capture_all$lac_score)),
#   est_bm = as.numeric((cow_capture_all$est_bm)),
#   #dde_95 = as.numeric((cow_capture_all$area_DDE_gt_95)),
#   #dde_75 = as.numeric((cow_capture_all$area_DDE_gt_75)),
#   dde_m = as.numeric((cow_capture_all$area_DDE_gt_mean)),
#   age = as.numeric((cow_capture_all$age_estimate_2)),
#   #precip_year_prior = as.numeric((cow_capture_all$total_year_prior)),
#   sex_correct = ifelse(cow_capture_all$sex_correct== "m", 1,0),
#   lac_time= as.numeric(cow_capture_all$lac_time), 
#   time_1_preg= as.numeric(cow_capture_all$time_1_preg)-1,#
#   calving_date = as.numeric(cow_capture_all$birt_date),
#   #spei_3 = as.numeric((cow_capture_all$sept_year_prior_3M)),
#   spei_6 = as.numeric((cow_capture_all$sept_year_prior_6M))#,
#   #spei_12 = as.numeric((cow_capture_all$sept_year_prior_12M))
#   
# )
# 
# nimble.constants = list(n.obs = nrow(cow_capture_all))
# 
# 
# mcmc.output_half_add_cm<- nimbleMCMC(code = model_half_add_cm,
#                                                data = nimble.data,
#                                                constants=nimble.constants,
#                                                monitors = parameters,
#                                                niter = ni,
#                                                nburnin = nb,
#                                                nchains = nc,
#                                                thin=nt,
#                                                summary=TRUE,
#                                                samplesAsCodaMCMC = TRUE)
# 
# attach.nimble(mcmc.output_half_add_cm$samples)
# 
# 
# mata_half  <- do.call(rbind, lapply(mcmc.output_half_add_cm$samples, as.matrix))
# lpa_half  <- grep("^logProb_preg_status\\[", colnames(mata_half), value = TRUE)
# lla_half  <- mata_half[, lpa_half, drop = FALSE]
# stopifnot(all(is.finite(lla_half) | (is.infinite(lla_half) & lla_half < 0)))
# loo_half_add_cm <- loo(lla_half)
# 
# 
# 
# attach.nimble(mcmc.output_half_add_cm$samples)
# 
# MCMCtrace(object = mcmc.output_half_add_cm$samples,
#           pdf = FALSE, # no export to PDF
#           ind = TRUE, # separate density lines per chain
#           params = "time_1_preg_effect")
# # Gelman-Rubin diagnostic (AKA RHat or PSRF)
# gelman.diag(mcmc.output_half_add_cm$samples, multivariate = FALSE)
# 
# 
# 
# mcmcplot(mcmc.output_half_add_cm$samples)
# 
# 
# 
# ##how to get effects size
# ##jow to make the differnt charts in PP
# ##how to get time on deck chart
# ##uses zscore to preduct value. Use mean for all the other values
# ##will use for calf age ##what the fuck is a z-score?
# post_mat <- as.matrix(mcmc.output_half_add_cm$samples)
# # 2. Specify the parameters you care about
# params <- c(#"lac_effect",
#   "c_m_effect",
#   "bm_effect",
#   "dde_effect_m",
#   "age_effect",
#   "date_effect",
#   "age_bm_effect",
#   #"age2_effect",
#   # "precip_effect",
#   "Intercept", 
#   #"bod_con_effect",
#   #"lac_time_effect",
#   #"sex_effect",
#   "time_1_preg_effect",
#   #"spei_3_effect",
#   "spei_6_effect"#,
#   #"spei_12_effect"#,
#   #"lac_score_preg_effect"#,
#   #"sex_lac_effect"
# )
# 
# # 3. Summarize each one
# summary_df_half_add_cm <- tibble(parameter = params) %>%
#   rowwise() %>%
#   mutate(
#     median   = median(post_mat[, parameter]),
#     lower95  = quantile(post_mat[, parameter], 0.025),
#     upper95  = quantile(post_mat[, parameter], 0.975),
#     # posterior probability effect > 0
#     p_gt_zero = mean(post_mat[, parameter] > 0),
#     # if you expect a negative effect, you can also compute P(<0):
#     p_lt_zero = mean(post_mat[, parameter] < 0)
#   ) %>%
#   ungroup()
# 
# print(summary_df_half_add_cm)
# get_variables(mcmc.output_half_add_cm$samples)
# 
# 
# summary_df_half_add_cm %>%
#   kbl(digits = 3, caption = "Posterior Summary Statistics for Model bc_2") %>%
#   kable_styling(full_width = FALSE, position = "center")
# 
# # Full posterior distributions:
# 
# 
# 
# mod_29_fig<-mcmc.output_half_add_cm$samples%>%
#   spread_draws(#lac_effect,
#     c_m_effect,
#     bm_effect,
#     dde_effect_m,
#     age_effect,
#     date_effect,
#     age_bm_effect,
#     time_1_preg_effect,
#     spei_6_effect
#     
#   ) %>%
#   pivot_longer(-c(.chain, .iteration, .draw),
#                names_to = 'Parameter',
#                values_to = 'Estimate') %>%
#   ggplot(aes(y = Parameter, x = Estimate, fill = Parameter)) +
#   stat_halfeye(show.legend = FALSE) +
#   geom_vline(xintercept = 0, linetype = 2, color = 'black') +
#   scale_fill_manual(values = c(
#     date_effect = "#505050",  # make precip_effect blue
#     dde_effect_m = "#CCCCCC",
#     time_1_preg_effect = "grey60",
#     c_m_effect = "#CCCCCC",
#     age_bm_effect = "#505050",
#     age_effect = "grey60",
#     spei_6_effect = "grey60",
#     bm_effect ="grey60"
#   )) +
#   scale_y_discrete(labels = c(
#     date_effect = "Calving Date",
#     bm_effect = "Body Mass",
#     dde_effect_m = "Mean DDE",
#     age_effect = "Age",
#     time_1_preg_effect = "Pregnant Year Prior",
#     spei_6_effect = "6-month SPEI",
#     c_m_effect = "Calf Mass",
#     age_bm_effect = "Age * Body Mass"
#   ))+
#   labs(y = "Covariate", x = "Estimate",
#        title = "Predicated Calf Model")+
#   theme_minimal()+
#   theme(axis.title = element_text(size = 23),# Dark x and y axis lines
#         axis.text = element_text(size = 17),
#         plot.title = element_text(size = 17,  hjust = -.9),
#         panel.background = element_rect(fill = "white", color = NA),
#         plot.background = element_rect(fill = "white", color = NA))+ 
#    scale_x_continuous(limits = c(-10, 10))
# mod_29_fig
# 
# ##end model 29
# 
# 
# ####half full################mean dde, spei_6
# model_half_add_xx <-  nimbleCode({
#   
#   #Priors
#   Intercept  ~ dnorm(0, sd = 5)
#   
#   #lac_effect~ dt(mu=0, sigma=1, df=1)
#   bm_effect ~ dt(mu=0, sigma=1, df=1)
#   dde_effect_m ~ dt(mu=0, sigma=1, df=1) 
#   age_effect ~ dt(mu=0, sigma=1, df=1)
#   date_effect~ dt(mu=0, sigma=1, df=1)
#   c_m_effect~ dt(mu=0, sigma=1, df=1)
#   #date_effect~ dnorm(0, sd = 5)
#    #lac_time_effect ~dt(mu=0, sigma=1, df=1)
#    sex_effect ~ dt(mu=0, sigma=1, df=1)
#   
#   time_1_preg_effect ~ dt(mu=0, sigma=1, df=1)
#   # spei_3_effect ~ dt(mu = 0 , sigma = 1, df = 1)
#   spei_6_effect ~ dt(mu = 0 , sigma = 1, df = 1)
#   #spei_12_effect ~ dt(mu = 0 , sigma = 1, df = 1)
#   # age2_effect ~ dt(mu = 0, sigma = 1, df = 1)
#   lac_score_preg_effect ~ dt(mu = 0, sigma = 1, df = 1)
#   age_bm_effect ~ dt(mu = 0, sigma = 1, df = 1)
#   #sex_lac_effect ~ dt(mu = 0, sigma = 1, df = 1)
#   
#   
#   
#   for(i in 1:n.obs){
#     
#     #Process Model
#     logit(preg_prob[i])<-Intercept+
#       #(lac_score[i]*lac_effect)+
#       (est_bm[i]*bm_effect)+
#       (dde_m[i]*dde_effect_m)+
#       (age[i]*age_effect)+
#       (calf_mass[i]* c_m_effect)+
#       # (pow(age[i], 2)*age2_effect)+
#       (calving_date[i] *date_effect )+
#       (sex_correct[i] * sex_effect) +
#       #(lac_time[i]* lac_time_effect)+
#       (time_1_preg[i]* time_1_preg_effect)+
#       # (spei_3[i]*spei_3_effect)+
#       (spei_6[i]*spei_6_effect)+
#       #(spei_12[i]*spei_12_effect)#+
#       (age[i]*est_bm[i]*age_bm_effect)
#     
#     
#     
#     
#     
#     
#     #Observation Model (Likelihood)
#     preg_status[i] ~ dbern(preg_prob[i])
#     
#     #Impute missing data values
#     lac_score[i] ~ dnorm(mean=0,sd=1)
#     calving_date[i]  ~ dnorm(mean=0,sd=1)
#     # precip_year_prior[i] ~ dnorm(mean = 0, sd = 1)
#     #dde_95[i] ~ dnorm(mean=0,sd=1)
#      age[i] ~ dnorm(mean=0,sd=1)
#     # est_body_cond[i] ~ dnorm(mean=0,sd=1)
#     est_bm[i] ~ dnorm(mean=0,sd=1)
#     
#     #lac_time[i] ~ dnorm(mean=0,sd=1)##does this matrer
#     #spei_3[i] ~ dnorm(mean = 0, sd = 1)
#     # spei_6[i] ~ dnorm(mean = 0, sd = 1)
#     # spei_12[i] ~ dnorm(mean = 0, sd = 1)
#     
#     
#     
#   }#i
#   
# })#model5
# parameters <- c("c_m_effect",
#                 "bm_effect",
#                 "dde_effect_m",
#                 "age_effect",
#                 "date_effect",
#                 "age_bm_effect",
#                 # "age2_effect",
#                 # "precip_effect",
#                 "Intercept", 
#                 # "bod_con_effect",
#                  #"lac_time_effect",
#                 "sex_effect",
#                 "time_1_preg_effect",
#                 "logProb_preg_status",#
#                 # "spei_3_effect",
#                 "spei_6_effect"#,
#                 #"spei_12_effect"#,
#                 #"lac_score_preg_effect"#,
#                 ##"sex_lac_effect"
# )
# 
# # MCMC Settings
# ni <- 400000
# nt <- 400
# nb <- 200000
# nc <- 3
# 
# nimble.data = list(
#   calf_mass = as.numeric(cow_capture_all$mass_calf),
#   preg_status = as.numeric(cow_capture_all$time_2_preg) - 1,
#   lac_score = as.numeric((cow_capture_all$lac_score)),
#   est_bm = as.numeric((cow_capture_all$est_bm)),
#   #dde_95 = as.numeric((cow_capture_all$area_DDE_gt_95)),
#   #dde_75 = as.numeric((cow_capture_all$area_DDE_gt_75)),
#   dde_m = as.numeric((cow_capture_all$area_DDE_gt_mean)),
#   age = as.numeric((cow_capture_all$age_estimate_2)),
#   #precip_year_prior = as.numeric((cow_capture_all$total_year_prior)),
#   sex_correct = ifelse(cow_capture_all$sex_correct== "m", 1,0),
#   lac_time= as.numeric(cow_capture_all$lac_time), 
#   time_1_preg= as.numeric(cow_capture_all$time_1_preg)-1,#
#   calving_date = as.numeric(cow_capture_all$birt_date),
#   #spei_3 = as.numeric((cow_capture_all$sept_year_prior_3M)),
#   spei_6 = as.numeric((cow_capture_all$sept_year_prior_6M))#,
#   #spei_12 = as.numeric((cow_capture_all$sept_year_prior_12M))
#   
# )
# 
# nimble.constants = list(n.obs = nrow(cow_capture_all))
# 
# 
# mcmc.output_half_add_xx<- nimbleMCMC(code = model_half_add_xx,
#                                      data = nimble.data,
#                                      constants=nimble.constants,
#                                      monitors = parameters,
#                                      niter = ni,
#                                      nburnin = nb,
#                                      nchains = nc,
#                                      thin=nt,
#                                      summary=TRUE,
#                                      samplesAsCodaMCMC = TRUE)
# 
# attach.nimble(mcmc.output_half_add_xx$samples)
# 
# 
# mata_half  <- do.call(rbind, lapply(mcmc.output_half_add_xx$samples, as.matrix))
# lpa_half  <- grep("^logProb_preg_status\\[", colnames(mata_half), value = TRUE)
# lla_half  <- mata_half[, lpa_half, drop = FALSE]
# stopifnot(all(is.finite(lla_half) | (is.infinite(lla_half) & lla_half < 0)))
# loo_half_add_xx <- loo(lla_half)
# 
# 
# 
# attach.nimble(mcmc.output_half_add_xx$samples)
# 
# MCMCtrace(object = mcmc.output_half_add_xx$samples,
#           pdf = FALSE, # no export to PDF
#           ind = TRUE, # separate density lines per chain
#           params = "time_1_preg_effect")
# # Gelman-Rubin diagnostic (AKA RHat or PSRF)
# gelman.diag(mcmc.output_half_add_xx$samples, multivariate = FALSE)
# 
# 
# 
# mcmcplot(mcmc.output_half_add_xx$samples)
# 
# 
# 
# ##how to get effects size
# ##jow to make the differnt charts in PP
# ##how to get time on deck chart
# ##uses zscore to preduct value. Use mean for all the other values
# ##will use for calf age ##what the fuck is a z-score?
# post_mat <- as.matrix(mcmc.output_half_add_xx$samples)
# # 2. Specify the parameters you care about
# params <- c("c_m_effect",
#             "bm_effect",
#             "dde_effect_m",
#             "age_effect",
#             "date_effect",
#             "age_bm_effect",
#             # "age2_effect",
#             # "precip_effect",
#            
#             # "bod_con_effect",
#             #"lac_time_effect",
#             "sex_effect",
#             "time_1_preg_effect",
#             
#             # "spei_3_effect",
#             "spei_6_effect"#,
#             #"spei_12_effect"#,
#             #"lac_score_preg_effect"#,
#             ##"sex_lac_effect"
# )
# 
# # 3. Summarize each one
# summary_df_half_add_xx <- tibble(parameter = params) %>%
#   rowwise() %>%
#   mutate(
#     median   = median(post_mat[, parameter]),
#     lower95  = quantile(post_mat[, parameter], 0.025),
#     upper95  = quantile(post_mat[, parameter], 0.975),
#     # posterior probability effect > 0
#     p_gt_zero = mean(post_mat[, parameter] > 0),
#     # if you expect a negative effect, you can also compute P(<0):
#     p_lt_zero = mean(post_mat[, parameter] < 0)
#   ) %>%
#   ungroup()
# 
# print(summary_df_half_add_xx)
# get_variables(mcmc.output_half_add_cm$samples)
# 
# 
# summary_df_half_add_cm %>%
#   kbl(digits = 3, caption = "Posterior Summary Statistics for Model bc_2") %>%
#   kable_styling(full_width = FALSE, position = "center")
# 
# # Full posterior distributions:
# 
# 
# 
# mod_29_fig<-mcmc.output_half_add_cm$samples%>%
#   spread_draws(#lac_effect,
#     c_m_effect,
#     bm_effect,
#     dde_effect_m,
#     age_effect,
#     date_effect,
#     age_bm_effect,
#     #age2_effect,
#     #precip_effect,
#     
#     # bod_con_effect,
#     #lac_time_effect,
#     #sex_effect,
#     #lac_score_preg_effect,
#     time_1_preg_effect,
#     #spei_3_effect,
#     spei_6_effect#, 
#     #spei_12_effect
#   ) %>%
#   pivot_longer(-c(.chain, .iteration, .draw),
#                names_to = 'Parameter',
#                values_to = 'Estimate') %>%
#   ggplot(aes(y = Parameter, x = Estimate, fill = Parameter)) +
#   stat_halfeye(show.legend = FALSE) +
#   geom_vline(xintercept = 0, linetype = 2, color = 'black') +
#   scale_fill_manual(values = c(
#     #lac_time_effect = "grey60",  # make precip_effect blue
#     sex_effect = "red",
#     time_1_preg_effect = "grey60",
#     bod_con_effect = "red4",
#     lac_time_effect = "grey60",
#     age_effect = "blue4"
#   )) +
#   theme_minimal()
# mod_29_fig
# ############################################Global######################
# ###full model DDE, SPEI 6 month, time_1_preg, calf_sex, lac time, age, BM, lac_score
# 
# model_full_global <-  nimbleCode({
#   
#   #Priors
#   Intercept  ~ dnorm(0, sd = 5)
#   
#   lac_effect~ dt(mu=0, sigma=1, df=1)
#   bm_effect ~ dt(mu=0, sigma=1, df=1)
#   dde_effect_m ~ dt(mu=0, sigma=1, df=1) 
#   age_effect ~ dt(mu=0, sigma=1, df=1)
#   date_effect~dt(mu=0, sigma=1, df=1)
#   lac_time_effect ~dt(mu=0, sigma=1, df=1)
#   sex_effect ~ dt(mu=0, sigma=1, df=1)
#   precip_effect ~ dt(mu=0, sigma=1, df=1)
#   time_1_preg_effect ~ dt(mu=0, sigma=1, df=1)
#   #spei_3_effect ~ dt(mu = 0 , sigma = 1, df = 1)
#   spei_6_effect ~ dt(mu = 0 , sigma = 1, df = 1)
#   #spei_12_effect ~ dt(mu = 0 , sigma = 1, df = 1)
#   age2_effect ~ dt(mu = 0, sigma = 1, df = 1)
#   # lac_score_preg_effect ~ dt(mu = 0, sigma = 1, df = 1)
#   sex_lac_effect ~ dt(mu = 0, sigma = 1, df = 1)
#   c_mass_effect ~ dt(mu = 0 , sigma = 1, df = 1)
#   #lac_effect~ dnorm(0, sd = 5)
#   #bm_effect ~ dnorm(0, sd = 5)
#   #dde_effect_m ~ dnorm(0, sd = 2) 
#   #age_effect ~ dnorm(0, sd = 5)
#   #precip_effect ~ dnorm(0, sd = 2)
#   #bod_con_effect ~ dnorm(0, sd = 2)
#   #time_1_days_effect ~ dnorm(0, sd = 2)
#   #sex_effect ~ dnorm(0, sd = 2)
#   #time_1_preg_effect ~ dnorm(0, sd = 5)
#   
#   
#   for(i in 1:n.obs){
#     
#     #Process Model
#     logit(preg_prob[i])<-Intercept+
#       (lac_score[i]*lac_effect)+
#       (est_bm[i]*bm_effect)+
#       (dde_95[i]*dde_effect_m)+
#       (age[i]*age_effect)+
#       (precip_year_prior[i] * precip_effect)+
#       (pow(age[i], 2)*age2_effect)+
#       (c_mass[i] * c_mass_effect)+
#       (sex_correct[i] * sex_effect) +
#       (lac_time[i]* lac_time_effect)+
#       (time_1_preg[i]* time_1_preg_effect)+
#       (calving_date[i] *date_effect )+
#       #(spei_3[i]*spei_3_effect)#+
#       (spei_6[i]*spei_6_effect)#+
#     #(spei_12[i]*spei_12_effect)#+
#     # (lac_score[i]*time_1_preg[i]*lac_score_preg_effect)+
#     
#     
#     
#     
#     
#     
#     #Observation Model (Likelihood)
#     preg_status[i] ~ dbern(preg_prob[i])
#     
#     #Impute missing data values
#     lac_score[i] ~ dnorm(mean=0,sd=1)
#     precip_year_prior[i] ~ dnorm(mean = 0, sd = 1)
#     calving_date[i] ~ dnorm(mean = 0, sd = 1)
#     dde_95[i] ~ dnorm(mean=0,sd=1)
#     age[i] ~ dnorm(mean=0,sd=1)
#     # est_body_cond[i] ~ dnorm(mean=0,sd=1)
#     est_bm[i] ~ dnorm(mean=0,sd=1)
#     
#     lac_time[i] ~ dnorm(mean=0,sd=1)##does this matrer
#     #spei_3[i] ~ dnorm(mean = 0, sd = 1)
#     spei_6[i] ~ dnorm(mean = 0, sd = 1)
#     #spei_12[i] ~ dnorm(mean = 0, sd = 1)
#     
#     
#     
#   }#i
#   
# })#model5
# parameters <- c("lac_effect",
#                 "bm_effect",
#                 "dde_effect_m",
#                 "age_effect",
#                  "age2_effect",
#                 "precip_effect",
#                 "Intercept", 
#                 # "bod_con_effect",
#                 "date_effect",
#                 "lac_time_effect",
#                 "sex_effect",
#                 "time_1_preg_effect",
#                 "logProb_preg_status",#
#                  "c_mass_effect",
#                 "spei_6_effect"#,
#                 #"spei_12_effect"#,
#                 #"lac_score_preg_effect"#,
#                 #"sex_lac_effect"
# )
# 
# # MCMC Settings
# ni <- 400000
# nt <- 400
# nb <- 200000
# nc <- 3
# 
# nimble.data = list(
#   preg_status = as.numeric(cow_capture_all$time_2_preg) - 1,
#   lac_score = as.numeric((cow_capture_all$lac_score)),
#   est_bm = as.numeric((cow_capture_all$est_bm)),
#   dde_95 = as.numeric((cow_capture_all$area_DDE_gt_mean)),
#   age = as.numeric((cow_capture_all$age_estimate_2)),
#   precip_year_prior = as.numeric((cow_capture_all$total_year_prior)),
#   sex_correct = ifelse(cow_capture_all$sex_correct== "m", 1,0),
#   lac_time= as.numeric(cow_capture_all$lac_time), 
#   time_1_preg= as.numeric(cow_capture_all$time_1_preg)-1,#
#   c_mass =  as.numeric(cow_capture_all$mass_calf),
#   calving_date = as.numeric(cow_capture_all$birt_date),
#   #spei_3 = as.numeric((cow_capture_all$sept_year_prior_3M))#,
#   spei_6 = as.numeric((cow_capture_all$sept_year_prior_6M))#,
#   #spei_12 = as.numeric((cow_capture_all$sept_year_prior_12M))
#   
# )
# 
# nimble.constants = list(n.obs = nrow(cow_capture_all))
# 
# 
# mcmc.output_global<- nimbleMCMC(code = model_full_global,
#                                 data = nimble.data,
#                                 constants=nimble.constants,
#                                 monitors = parameters,
#                                 niter = ni,
#                                 nburnin = nb,
#                                 nchains = nc,
#                                 thin=nt,
#                                 summary=TRUE,
#                                 samplesAsCodaMCMC = TRUE)
# beep(sound = 3)
# 
# attach.nimble(mcmc.output_global$samples)
# 
# 
# mata_full <- do.call(rbind, lapply(mcmc.output_global$samples, as.matrix))
# lpa_full <- grep("^logProb_preg_status\\[", colnames(mata_full), value = TRUE)
# lla_full  <- mata_full[, lpa_full, drop = FALSE]
# stopifnot(all(is.finite(lla_full) | (is.infinite(lla_full) & lla_full < 0)))
# loo_global <- loo(lla_full)
# beep(sound =1)
# 
# 
# attach.nimble(mcmc.output_global$samples)
# 
# MCMCtrace(object = mcmc.output_global$samples,
#           pdf = FALSE, # no export to PDF
#           ind = TRUE, # separate density lines per chain
#           params = "time_1_preg_effect")
# # Gelman-Rubin diagnostic (AKA RHat or PSRF)
# gelman.diag(mcmc.output_global$samples, multivariate = FALSE)
# 
# 
# 
# mcmcplot(mcmc.output_global$samples)
# 
# 
# 
# 
# ##how to get effects size
# ##jow to make the differnt charts in PP
# ##how to get time on deck chart
# ##uses zscore to preduct value. Use mean for all the other values
# ##will use for calf age ##what the fuck is a z-score?
# post_mat <- as.matrix(mcmc.output_global$samples)
# # 2. Specify the parameters you care about
# params <- c("lac_effect",
#             "bm_effect",
#             "dde_effect_m",
#             "age_effect",
#              "age2_effect",
#             "precip_effect",
#             
#            
#             "date_effect",
#             "lac_time_effect",
#             "sex_effect",
#             "time_1_preg_effect",
#             
#             "c_mass_effect",
#             "spei_6_effect"
#             
# )
# 
# # 3. Summarize each one
# summary_df_full <- tibble(parameter = params) %>%
#   rowwise() %>%
#   mutate(
#     median   = median(post_mat[, parameter]),
#     lower95  = quantile(post_mat[, parameter], 0.025),
#     upper95  = quantile(post_mat[, parameter], 0.975),
#     # posterior probability effect > 0
#     p_gt_zero = mean(post_mat[, parameter] > 0),
#     # if you expect a negative effect, you can also compute P(<0):
#     p_lt_zero = mean(post_mat[, parameter] < 0)
#   ) %>%
#   ungroup()
# 
# print(summary_df_full)
# get_variables(mcmc.output_global$samples)
# 
# 
# summary_df_full  %>%
#   kbl(digits = 3, caption = "Posterior Summary Statistics for Model bc_2") %>%
#   kable_styling(full_width = FALSE, position = "center")
# 
# # Full posterior distributions:
# 
# 
# 
# ##########################################################global
# 
# ###top model plus age
# ####half full################mean dde, spei_6
# model_half_add_a2 <-  nimbleCode({
#   
#   #Priors
#   Intercept  ~ dnorm(0, sd = 5)
#   
#   #lac_effect~ dt(mu=0, sigma=1, df=1)
#   bm_effect ~ dt(mu=0, sigma=1, df=1)
#   dde_effect_m ~ dt(mu=0, sigma=1, df=1) 
#   age_effect ~ dt(mu=0, sigma=1, df=1)
#   date_effect~ dt(mu=0, sigma=1, df=1)
#   c_m_effect~ dt(mu=0, sigma=1, df=1)
#   #date_effect~ dnorm(0, sd = 5)
#   #lac_time_effect ~dt(mu=0, sigma=1, df=1)
#   sex_effect ~ dt(mu=0, sigma=1, df=1)
#   
#   time_1_preg_effect ~ dt(mu=0, sigma=1, df=1)
#   # spei_3_effect ~ dt(mu = 0 , sigma = 1, df = 1)
#   spei_6_effect ~ dt(mu = 0 , sigma = 1, df = 1)
#   #spei_12_effect ~ dt(mu = 0 , sigma = 1, df = 1)
#    age2_effect ~ dt(mu = 0, sigma = 1, df = 1)
#   lac_score_preg_effect ~ dt(mu = 0, sigma = 1, df = 1)
#   age_bm_effect ~ dt(mu = 0, sigma = 1, df = 1)
#   #sex_lac_effect ~ dt(mu = 0, sigma = 1, df = 1)
#   
#   
#   
#   for(i in 1:n.obs){
#     
#     #Process Model
#     logit(preg_prob[i])<-Intercept+
#       #(lac_score[i]*lac_effect)+
#       (est_bm[i]*bm_effect)+
#       (dde_m[i]*dde_effect_m)+
#       (age[i]*age_effect)+
#       (calf_mass[i]* c_m_effect)+
#        (pow(age[i], 2)*age2_effect)+
#       (calving_date[i] *date_effect )+
#       (sex_correct[i] * sex_effect) +
#       #(lac_time[i]* lac_time_effect)+
#       (time_1_preg[i]* time_1_preg_effect)+
#       # (spei_3[i]*spei_3_effect)+
#       (spei_6[i]*spei_6_effect)+
#       #(spei_12[i]*spei_12_effect)#+
#       (age[i]*est_bm[i]*age_bm_effect)
#     
#     
#     
#     
#     
#     
#     #Observation Model (Likelihood)
#     preg_status[i] ~ dbern(preg_prob[i])
#     
#     #Impute missing data values
#     lac_score[i] ~ dnorm(mean=0,sd=1)
#     sex_correct[i] ~ dnorm(mean=0,sd=1)
#     # precip_year_prior[i] ~ dnorm(mean = 0, sd = 1)
#     #dde_95[i] ~ dnorm(mean=0,sd=1)
#      age[i] ~ dnorm(mean=0,sd=1)
#      calving_date[i]~ dnorm(mean=0,sd=1)
#     # est_body_cond[i] ~ dnorm(mean=0,sd=1)
#     est_bm[i] ~ dnorm(mean=0,sd=1)
#     
#     #lac_time[i] ~ dnorm(mean=0,sd=1)##does this matrer
#     #spei_3[i] ~ dnorm(mean = 0, sd = 1)
#     # spei_6[i] ~ dnorm(mean = 0, sd = 1)
#     # spei_12[i] ~ dnorm(mean = 0, sd = 1)
#     
#     
#     
#   }#i
#   
# })#model5
# parameters <- c("c_m_effect",
#                 "bm_effect",
#                 "dde_effect_m",
#                 "age_effect",
#                 "date_effect",
#                 "age_bm_effect",
#                  "age2_effect",
#                 # "precip_effect",
#                 "Intercept", 
#                 # "bod_con_effect",
#                 #"lac_time_effect",
#                 "sex_effect",
#                 "time_1_preg_effect",
#                 "logProb_preg_status",#
#                 # "spei_3_effect",
#                 "spei_6_effect"#,
#                 #"spei_12_effect"#,
#                 #"lac_score_preg_effect"#,
#                 ##"sex_lac_effect"
# )
# 
# # MCMC Settings
# ni <- 400000
# nt <- 400
# nb <- 200000
# nc <- 3
# 
# nimble.data = list(
#   calf_mass = as.numeric(cow_capture_all$mass_calf),
#   preg_status = as.numeric(cow_capture_all$time_2_preg) - 1,
#   lac_score = as.numeric((cow_capture_all$lac_score)),
#   est_bm = as.numeric((cow_capture_all$est_bm)),
#   #dde_95 = as.numeric((cow_capture_all$area_DDE_gt_95)),
#   #dde_75 = as.numeric((cow_capture_all$area_DDE_gt_75)),
#   dde_m = as.numeric((cow_capture_all$area_DDE_gt_mean)),
#   age = as.numeric((cow_capture_all$age_estimate_2)),
#   #precip_year_prior = as.numeric((cow_capture_all$total_year_prior)),
#   sex_correct = ifelse(cow_capture_all$sex_correct== "m", 1,0),
#   lac_time= as.numeric(cow_capture_all$lac_time), 
#   time_1_preg= as.numeric(cow_capture_all$time_1_preg)-1,#
#   calving_date = as.numeric(cow_capture_all$birt_date),
#   #spei_3 = as.numeric((cow_capture_all$sept_year_prior_3M)),
#   spei_6 = as.numeric((cow_capture_all$sept_year_prior_6M))#,
#   #spei_12 = as.numeric((cow_capture_all$sept_year_prior_12M))
#   
# )
# 
# nimble.constants = list(n.obs = nrow(cow_capture_all))
# 
# 
# mcmc.output_half_add_a2<- nimbleMCMC(code = model_half_add_a2,
#                                      data = nimble.data,
#                                      constants=nimble.constants,
#                                      monitors = parameters,
#                                      niter = ni,
#                                      nburnin = nb,
#                                      nchains = nc,
#                                      thin=nt,
#                                      summary=TRUE,
#                                      samplesAsCodaMCMC = TRUE)
# 
# attach.nimble(mcmc.output_half_add_a2$samples)
# 
# 
# mata_half  <- do.call(rbind, lapply(mcmc.output_half_add_a2$samples, as.matrix))
# lpa_half  <- grep("^logProb_preg_status\\[", colnames(mata_half), value = TRUE)
# lla_half  <- mata_half[, lpa_half, drop = FALSE]
# stopifnot(all(is.finite(lla_half) | (is.infinite(lla_half) & lla_half < 0)))
# loo_half_add_a2 <- loo(lla_half)
# 
# 
# 
# attach.nimble(mcmc.output_half_add_a2$samples)
# 
# MCMCtrace(object = mcmc.output_half_add_a2$samples,
#           pdf = FALSE, # no export to PDF
#           ind = TRUE, # separate density lines per chain
#           params = "time_1_preg_effect")
# # Gelman-Rubin diagnostic (AKA RHat or PSRF)
# gelman.diag(mcmc.output_half_add_a2$samples, multivariate = FALSE)
# 
# 
# 
# mcmcplot(mcmc.output_half_add_a2$samples)
# 
# 
# 
# ##how to get effects size
# ##jow to make the differnt charts in PP
# ##how to get time on deck chart
# ##uses zscore to preduct value. Use mean for all the other values
# ##will use for calf age ##what the fuck is a z-score?
# post_mat <- as.matrix(mcmc.output_half_add_a2$samples)
# # 2. Specify the parameters you care about
# params <- c("c_m_effect",
#             "bm_effect",
#             "dde_effect_m",
#             "age_effect",
#             "date_effect",
#             "age_bm_effect",
#             "age2_effect",
# 
#             "sex_effect",
#             "time_1_preg_effect",
#        
#             "spei_6_effect"#,
# 
# )
# 
# # 3. Summarize each one
# summary_df_half_add_a2 <- tibble(parameter = params) %>%
#   rowwise() %>%
#   mutate(
#     median   = median(post_mat[, parameter]),
#     lower95  = quantile(post_mat[, parameter], 0.025),
#     upper95  = quantile(post_mat[, parameter], 0.975),
#     # posterior probability effect > 0
#     p_gt_zero = mean(post_mat[, parameter] > 0),
#     # if you expect a negative effect, you can also compute P(<0):
#     p_lt_zero = mean(post_mat[, parameter] < 0)
#   ) %>%
#   ungroup()
# 
# print(summary_df_half_add_a2)
# 
# 
# 
# summary_df_half_add_cm %>%
#   kbl(digits = 3, caption = "Posterior Summary Statistics for Model bc_2") %>%
#   kable_styling(full_width = FALSE, position = "center")
# 
# ##################################
# 
# ##########################################################global
# 
# ###top model plus age
# ####half full################mean dde, spei_6
# model_half_add_a2_lactime <-  nimbleCode({
#   
#   #Priors
#   Intercept  ~ dnorm(0, sd = 5)
#   
#   #lac_effect~ dt(mu=0, sigma=1, df=1)
#   bm_effect ~ dt(mu=0, sigma=1, df=1)
#   dde_effect_m ~ dt(mu=0, sigma=1, df=1) 
#   age_effect ~ dt(mu=0, sigma=1, df=1)
#   date_effect~ dt(mu=0, sigma=1, df=1)
#   c_m_effect~ dt(mu=0, sigma=1, df=1)
#   #date_effect~ dnorm(0, sd = 5)
#   lac_time_effect ~dt(mu=0, sigma=1, df=1)
#   sex_effect ~ dt(mu=0, sigma=1, df=1)
#   
#   time_1_preg_effect ~ dt(mu=0, sigma=1, df=1)
#   # spei_3_effect ~ dt(mu = 0 , sigma = 1, df = 1)
#   spei_6_effect ~ dt(mu = 0 , sigma = 1, df = 1)
#   #spei_12_effect ~ dt(mu = 0 , sigma = 1, df = 1)
#   age2_effect ~ dt(mu = 0, sigma = 1, df = 1)
#   lac_score_preg_effect ~ dt(mu = 0, sigma = 1, df = 1)
#   age_bm_effect ~ dt(mu = 0, sigma = 1, df = 1)
#   #sex_lac_effect ~ dt(mu = 0, sigma = 1, df = 1)
#   
#   
#   
#   for(i in 1:n.obs){
#     
#     #Process Model
#     logit(preg_prob[i])<-Intercept+
#       #(lac_score[i]*lac_effect)+
#       (est_bm[i]*bm_effect)+
#       (dde_m[i]*dde_effect_m)+
#       (age[i]*age_effect)+
#       (calf_mass[i]* c_m_effect)+
#       (pow(age[i], 2)*age2_effect)+
#       (calving_date[i] *date_effect )+
#       (sex_correct[i] * sex_effect) +
#       (lac_time[i]* lac_time_effect)+
#       (time_1_preg[i]* time_1_preg_effect)+
#       # (spei_3[i]*spei_3_effect)+
#       (spei_6[i]*spei_6_effect)+
#       #(spei_12[i]*spei_12_effect)#+
#       (age[i]*est_bm[i]*age_bm_effect)
#     
#     
#     
#     
#     
#     
#     #Observation Model (Likelihood)
#     preg_status[i] ~ dbern(preg_prob[i])
#     
#     #Impute missing data values
#     lac_score[i] ~ dnorm(mean=0,sd=1)
#     # precip_year_prior[i] ~ dnorm(mean = 0, sd = 1)
#     #dde_95[i] ~ dnorm(mean=0,sd=1)
#      age[i] ~ dnorm(mean=0,sd=1)
#      time_1_preg[i]~ dnorm(mean=0,sd=1)
#      calving_date[i] ~ dnorm(mean=0,sd=1)
#     est_bm[i] ~ dnorm(mean=0,sd=1)
#     
#     #lac_time[i] ~ dnorm(mean=0,sd=1)##does this matrer
#     #spei_3[i] ~ dnorm(mean = 0, sd = 1)
#     # spei_6[i] ~ dnorm(mean = 0, sd = 1)
#     # spei_12[i] ~ dnorm(mean = 0, sd = 1)
#     
#     
#     
#   }#i
#   
# })#model5
# parameters <- c("c_m_effect",
#                 "bm_effect",
#                 "dde_effect_m",
#                 "age_effect",
#                 "date_effect",
#                 "age_bm_effect",
#                 "age2_effect",
#                 # "precip_effect",
#                 "Intercept", 
#                 # "bod_con_effect",
#                 "lac_time_effect",
#                 "sex_effect",
#                 "time_1_preg_effect",
#                 "logProb_preg_status",#
#                 # "spei_3_effect",
#                 "spei_6_effect"#,
#                 #"spei_12_effect"#,
#                 #"lac_score_preg_effect"#,
#                 ##"sex_lac_effect"
# )
# 
# # MCMC Settings
# ni <- 400000
# nt <- 400
# nb <- 200000
# nc <- 3
# 
# nimble.data = list(
#   calf_mass = as.numeric(cow_capture_all$mass_calf),
#   preg_status = as.numeric(cow_capture_all$time_2_preg) - 1,
#   lac_score = as.numeric((cow_capture_all$lac_score)),
#   est_bm = as.numeric((cow_capture_all$est_bm)),
#   #dde_95 = as.numeric((cow_capture_all$area_DDE_gt_95)),
#   #dde_75 = as.numeric((cow_capture_all$area_DDE_gt_75)),
#   dde_m = as.numeric((cow_capture_all$area_DDE_gt_mean)),
#   age = as.numeric((cow_capture_all$age_estimate_2)),
#   #precip_year_prior = as.numeric((cow_capture_all$total_year_prior)),
#   sex_correct = ifelse(cow_capture_all$sex_correct== "m", 1,0),
#   lac_time= as.numeric(cow_capture_all$lac_time), 
#   time_1_preg= as.numeric(cow_capture_all$time_1_preg)-1,#
#   calving_date = as.numeric(cow_capture_all$birt_date),
#   #spei_3 = as.numeric((cow_capture_all$sept_year_prior_3M)),
#   spei_6 = as.numeric((cow_capture_all$sept_year_prior_6M))#,
#   #spei_12 = as.numeric((cow_capture_all$sept_year_prior_12M))
#   
# )
# 
# nimble.constants = list(n.obs = nrow(cow_capture_all))
# 
# 
# mcmc.output_half_add_a2_lactime<- nimbleMCMC(code = model_half_add_a2_lactime,
#                                      data = nimble.data,
#                                      constants=nimble.constants,
#                                      monitors = parameters,
#                                      niter = ni,
#                                      nburnin = nb,
#                                      nchains = nc,
#                                      thin=nt,
#                                      summary=TRUE,
#                                      samplesAsCodaMCMC = TRUE)
# 
# attach.nimble(mcmc.output_half_add_a2_lactime$samples)
# 
# 
# mata_half  <- do.call(rbind, lapply(mcmc.output_half_add_a2_lactime$samples, as.matrix))
# lpa_half  <- grep("^logProb_preg_status\\[", colnames(mata_half), value = TRUE)
# lla_half  <- mata_half[, lpa_half, drop = FALSE]
# stopifnot(all(is.finite(lla_half) | (is.infinite(lla_half) & lla_half < 0)))
# loo_half_add_a2_lactime <- loo(lla_half)
# 
# 
# 
# attach.nimble(mcmc.output_half_add_a2_lactime$samples)
# 
# MCMCtrace(object = mcmc.output_half_add_a2_lactime$samples,
#           pdf = FALSE, # no export to PDF
#           ind = TRUE, # separate density lines per chain
#           params = "time_1_preg_effect")
# # Gelman-Rubin diagnostic (AKA RHat or PSRF)
# gelman.diag(mcmc.output_half_add_a2_lactime$samples, multivariate = FALSE)
# 
# 
# 
# mcmcplot(mcmc.output_half_add_a2_lactime$samples)
# 
# 
# 
# ##how to get effects size
# ##jow to make the differnt charts in PP
# ##how to get time on deck chart
# ##uses zscore to preduct value. Use mean for all the other values
# ##will use for calf age ##what the fuck is a z-score?
# post_mat <- as.matrix(mcmc.output_half_add_a2_lactime$samples)
# # 2. Specify the parameters you care about
# params <- c("c_m_effect",
#             "bm_effect",
#             "dde_effect_m",
#             "age_effect",
#             "date_effect",
#             "age_bm_effect",
#             "age2_effect",
#             "lac_time_effect",
#             "sex_effect",
#             "time_1_preg_effect",
#             
#             "spei_6_effect"#,
#             
# )
# 
# # 3. Summarize each one
# summary_df_half_add_a2_lactime <- tibble(parameter = params) %>%
#   rowwise() %>%
#   mutate(
#     median   = median(post_mat[, parameter]),
#     lower95  = quantile(post_mat[, parameter], 0.025),
#     upper95  = quantile(post_mat[, parameter], 0.975),
#     # posterior probability effect > 0
#     p_gt_zero = mean(post_mat[, parameter] > 0),
#     # if you expect a negative effect, you can also compute P(<0):
#     p_lt_zero = mean(post_mat[, parameter] < 0)
#   ) %>%
#   ungroup()
# 
# print(summary_df_half_add_a2_lactime)

######################################################################################
####removed weak effect
###top model plus age
####half full################mean dde, spei_6
model_half_add_a2_lactime_reduced <-  nimbleCode({
  
  #Priors
  Intercept  ~ dnorm(0, sd = 5)
  
  #lac_effect~ dt(mu=0, sigma=1, df=1)
  bm_effect ~ dt(mu=0, sigma=1, df=1)
  dde_effect_m ~ dt(mu=0, sigma=1, df=1) 
  age_effect ~ dt(mu=0, sigma=1, df=1)
  date_effect~ dt(mu=0, sigma=1, df=1)
  c_m_effect~ dt(mu=0, sigma=1, df=1)
  #date_effect~ dnorm(0, sd = 5)
  lac_time_effect ~dt(mu=0, sigma=1, df=1)
  sex_effect ~ dt(mu=0, sigma=1, df=1)
  calf_ind_effect ~ dt(mu = 0, sigma = 1, df = 1) 
  time_1_preg_effect ~ dt(mu=0, sigma=1, df=1)
  # spei_3_effect ~ dt(mu = 0 , sigma = 1, df = 1)
  spei_6_effect ~ dt(mu = 0 , sigma = 1, df = 1)
  #spei_12_effect ~ dt(mu = 0 , sigma = 1, df = 1)
  age2_effect ~ dt(mu = 0, sigma = 1, df = 1)
  lac_score_preg_effect ~ dt(mu = 0, sigma = 1, df = 1)
  age_bm_effect ~ dt(mu = 0, sigma = 1, df = 1)
  #sex_lac_effect ~ dt(mu = 0, sigma = 1, df = 1)
  
  
  
  for(i in 1:n.obs){
    
    #Process Model
    logit(preg_prob[i])<-Intercept+
      #(lac_score[i]*lac_effect)+
      (est_bm[i]*bm_effect)+
      (dde_m[i]*dde_effect_m)+
      (age[i]*age_effect)+
      (calf_ind[i] * calf_ind_effect) +
      (calf_mass[i]* c_m_effect)+
      (pow(age[i], 2)*age2_effect)+
      (calving_date[i] *date_effect )+
     # (sex_correct[i] * sex_effect) +
      (lac_time[i]* lac_time_effect)+
      #(time_1_preg[i]* time_1_preg_effect)+
      # (spei_3[i]*spei_3_effect)+
      (spei_6[i]*spei_6_effect)+
      #(spei_12[i]*spei_12_effect)#+
      (age[i]*est_bm[i]*age_bm_effect)
    
    
    
    
    
    
    #Observation Model (Likelihood)
    preg_status[i] ~ dbern(preg_prob[i])
    
    #Impute missing data values
    lac_score[i] ~ dnorm(mean=0,sd=1)
    # precip_year_prior[i] ~ dnorm(mean = 0, sd = 1)
    #dde_95[i] ~ dnorm(mean=0,sd=1)
     age[i] ~ dnorm(mean=0,sd=1)
    # est_body_cond[i] ~ dnorm(mean=0,sd=1)
    est_bm[i] ~ dnorm(mean=0,sd=1)
    
    #lac_time[i] ~ dnorm(mean=0,sd=1)##does this matrer
    calving_date[i] ~ dnorm(mean = 0, sd = 1)
    # spei_6[i] ~ dnorm(mean = 0, sd = 1)
    # spei_12[i] ~ dnorm(mean = 0, sd = 1)
    
    
    
  }#i
  
})#model5
parameters <- c("c_m_effect",
                "bm_effect",
                "dde_effect_m",
                "age_effect",
                "date_effect",
                "age_bm_effect",
                "age2_effect",
                "calf_ind_effect",
                # "precip_effect",
                "Intercept", 
                # "bod_con_effect",
                "lac_time_effect",
               # "sex_effect",
                #"time_1_preg_effect",
                "logProb_preg_status",#
                # "spei_3_effect",
                "spei_6_effect"#,
                #"spei_12_effect"#,
                #"lac_score_preg_effect"#,
                ##"sex_lac_effect"
)

# MCMC Settings
ni <- 400000
nt <- 400
nb <- 200000
nc <- 3

nimble.data = list(
  calf_mass = as.numeric(cow_capture_all$mass_calf),
  preg_status = as.numeric(cow_capture_all$time_2_preg) - 1,
  lac_score = as.numeric((cow_capture_all$lac_score)),
  est_bm = as.numeric((cow_capture_all$est_bm)),
  calf_ind =  as.numeric(cow_capture_all$calf_ind),
  #dde_95 = as.numeric((cow_capture_all$area_DDE_gt_95)),
  #dde_75 = as.numeric((cow_capture_all$area_DDE_gt_75)),
  dde_m = as.numeric((cow_capture_all$area_DDE_gt_mean)),
  age = as.numeric((cow_capture_all$age_estimate_2)),
  #precip_year_prior = as.numeric((cow_capture_all$total_year_prior)),
  sex_correct = ifelse(cow_capture_all$sex_correct== "m", 1,0),
  lac_time= as.numeric(cow_capture_all$lac_time), 
  time_1_preg= as.numeric(cow_capture_all$time_1_preg)-1,#
  calving_date = as.numeric(cow_capture_all$birt_date),
  #spei_3 = as.numeric((cow_capture_all$sept_year_prior_3M)),
  spei_6 = as.numeric((cow_capture_all$sept_year_prior_6M))#,
  #spei_12 = as.numeric((cow_capture_all$sept_year_prior_12M))
  
)

nimble.constants = list(n.obs = nrow(cow_capture_all))


mcmc.output_half_add_a2_lactime_reduced<- nimbleMCMC(code = model_half_add_a2_lactime_reduced,
                                             data = nimble.data,
                                             constants=nimble.constants,
                                             monitors = parameters,
                                             niter = ni,
                                             nburnin = nb,
                                             nchains = nc,
                                             thin=nt,
                                             summary=TRUE,
                                             samplesAsCodaMCMC = TRUE)

attach.nimble(mcmc.output_half_add_a2_lactime_reduced$samples)


mata_half  <- do.call(rbind, lapply(mcmc.output_half_add_a2_lactime_reduced$samples, as.matrix))
lpa_half  <- grep("^logProb_preg_status\\[", colnames(mata_half), value = TRUE)
lla_half  <- mata_half[, lpa_half, drop = FALSE]
stopifnot(all(is.finite(lla_half) | (is.infinite(lla_half) & lla_half < 0)))
loo_half_add_a2_lactime_r2 <- loo(lla_half)



attach.nimble(mcmc.output_half_add_a2_lactime_reduced$samples)


# Gelman-Rubin diagnostic (AKA RHat or PSRF)
gelman.diag(mcmc.output_half_add_a2_lactime_reduced$samples, multivariate = FALSE)



mcmcplot(mcmc.output_half_add_a2_lactime_reduced$samples)



##how to get effects size
##jow to make the differnt charts in PP
##how to get time on deck chart
##uses zscore to preduct value. Use mean for all the other values
##will use for calf age ##what the fuck is a z-score?
post_mat <- as.matrix(mcmc.output_half_add_a2_lactime_reduced$samples)
# 2. Specify the parameters you care about
params <- c("c_m_effect",
            "bm_effect",
            "dde_effect_m",
            "age_effect",
            "date_effect",
            "age_bm_effect",
            "age2_effect",
            # "precip_effect",
            "Intercept", 
            # "bod_con_effect",
            "lac_time_effect",
            "calf_ind_effect",
            # "sex_effect",
            #"time_1_preg_effect",
           
            # "spei_3_effect",
            "spei_6_effect"#,
            
)

# 3. Summarize each one
summary_df_half_add_a2_lactime_r <- tibble(parameter = params) %>%
  rowwise() %>%
  mutate(
    median   = median(post_mat[, parameter]),
    lower95  = quantile(post_mat[, parameter], 0.025),
    upper95  = quantile(post_mat[, parameter], 0.975),
    # posterior probability effect > 0
    p_gt_zero = mean(post_mat[, parameter] > 0),
    # if you expect a negative effect, you can also compute P(<0):
    p_lt_zero = mean(post_mat[, parameter] < 0)
  ) %>%
  ungroup()

print(summary_df_half_add_a2_lactime_r)

##############################

##########################75
####half full################mean dde, spei_6
model_half_add_a2_lactime_reduced_75_12p <-  nimbleCode({
  
  #Priors
  Intercept  ~ dnorm(0, sd = 5)
  
  #lac_effect~ dt(mu=0, sigma=1, df=1)
  bm_effect ~ dt(mu=0, sigma=1, df=1)
  dde_effect_m ~ dt(mu=0, sigma=1, df=1) 
  age_effect ~ dt(mu=0, sigma=1, df=1)
  date_effect~ dt(mu=0, sigma=1, df=1)
  c_m_effect~ dt(mu=0, sigma=1, df=1)
  #date_effect~ dnorm(0, sd = 5)
  lac_time_effect ~dt(mu=0, sigma=1, df=1)
  sex_effect ~ dt(mu=0, sigma=1, df=1)
  
  time_1_preg_effect ~ dt(mu=0, sigma=1, df=1)
  # spei_3_effect ~ dt(mu = 0 , sigma = 1, df = 1)
  #spei_6_effect ~ dt(mu = 0 , sigma = 1, df = 1)
  spei_12_effect ~ dt(mu = 0 , sigma = 1, df = 1)
  age2_effect ~ dt(mu = 0, sigma = 1, df = 1)
  lac_score_preg_effect ~ dt(mu = 0, sigma = 1, df = 1)
  age_bm_effect ~ dt(mu = 0, sigma = 1, df = 1)
  #sex_lac_effect ~ dt(mu = 0, sigma = 1, df = 1)
  
  
  
  for(i in 1:n.obs){
    
    #Process Model
    logit(preg_prob[i])<-Intercept+
      #(lac_score[i]*lac_effect)+
      (est_bm[i]*bm_effect)+
      (dde_75[i]*dde_effect_m)+
      (age[i]*age_effect)+
      (calf_mass[i]* c_m_effect)+
      (pow(age[i], 2)*age2_effect)+
      (calving_date[i] *date_effect )+
      # (sex_correct[i] * sex_effect) +
      (lac_time[i]* lac_time_effect)+
      #(time_1_preg[i]* time_1_preg_effect)+
      # (spei_3[i]*spei_3_effect)+
      #(spei_6[i]*spei_6_effect)+
      (spei_12[i]*spei_12_effect)+
      (age[i]*est_bm[i]*age_bm_effect)
    
    
    
    
    
    
    #Observation Model (Likelihood)
    preg_status[i] ~ dbern(preg_prob[i])
    
    #Impute missing data values
    lac_score[i] ~ dnorm(mean=0,sd=1)
    # precip_year_prior[i] ~ dnorm(mean = 0, sd = 1)
    #dde_95[i] ~ dnorm(mean=0,sd=1)
     age[i] ~ dnorm(mean=0,sd=1)
    # est_body_cond[i] ~ dnorm(mean=0,sd=1)
    est_bm[i] ~ dnorm(mean=0,sd=1)
    
    calving_date[i] ~ dnorm(mean=0,sd=1)
    #spei_3[i] ~ dnorm(mean = 0, sd = 1)
    lac_time[i] ~ dnorm(mean = 0, sd = 1)
    # spei_12[i] ~ dnorm(mean = 0, sd = 1)
    
    
    
  }#i
  
})#model5
parameters <- c("c_m_effect",
                "bm_effect",
                "dde_effect_m",
                "age_effect",
                "date_effect",
                "age_bm_effect",
                "age2_effect",
                # "precip_effect",
                "Intercept", 
                # "bod_con_effect",
                "lac_time_effect",
                # "sex_effect",
                #"time_1_preg_effect",
                "logProb_preg_status",#
                # "spei_3_effect",
                #"spei_6_effect"#,
                "spei_12_effect"#,
                #"lac_score_preg_effect"#,
                ##"sex_lac_effect"
)

# MCMC Settings
ni <- 400000
nt <- 400
nb <- 200000
nc <- 3

nimble.data = list(
  calf_mass = as.numeric(cow_capture_all$mass_calf),
  preg_status = as.numeric(cow_capture_all$time_2_preg) - 1,
  lac_score = as.numeric((cow_capture_all$lac_score)),
  est_bm = as.numeric((cow_capture_all$est_bm)),
  #dde_95 = as.numeric((cow_capture_all$area_DDE_gt_95)),
  dde_75 = as.numeric((cow_capture_all$area_DDE_gt_75)),
  #dde_m = as.numeric((cow_capture_all$area_DDE_gt_mean)),
  age = as.numeric((cow_capture_all$age_estimate_2)),
  #precip_year_prior = as.numeric((cow_capture_all$total_year_prior)),
  sex_correct = ifelse(cow_capture_all$sex_correct== "m", 1,0),
  lac_time= as.numeric(cow_capture_all$lac_time), 
  time_1_preg= as.numeric(cow_capture_all$time_1_preg)-1,#
  calving_date = as.numeric(cow_capture_all$birt_date),
  #spei_3 = as.numeric((cow_capture_all$sept_year_prior_3M)),
  #spei_6 = as.numeric((cow_capture_all$sept_year_prior_6M))#,
  spei_12 = as.numeric((cow_capture_all$sept_year_prior_12M))
  
)

nimble.constants = list(n.obs = nrow(cow_capture_all))


mcmc.output_half_add_a2_lactime_reduced_75_12p<- nimbleMCMC(code = model_half_add_a2_lactime_reduced_75_12p,
                                                     data = nimble.data,
                                                     constants=nimble.constants,
                                                     monitors = parameters,
                                                     niter = ni,
                                                     nburnin = nb,
                                                     nchains = nc,
                                                     thin=nt,
                                                     summary=TRUE,
                                                     samplesAsCodaMCMC = TRUE)

attach.nimble(mcmc.output_half_add_a2_lactime_reduced_75_12p$samples)


mata_half  <- do.call(rbind, lapply(mcmc.output_half_add_a2_lactime_reduced_75_12p$samples, as.matrix))
lpa_half  <- grep("^logProb_preg_status\\[", colnames(mata_half), value = TRUE)
lla_half  <- mata_half[, lpa_half, drop = FALSE]
stopifnot(all(is.finite(lla_half) | (is.infinite(lla_half) & lla_half < 0)))
loo_half_add_a2_lactime_r_75_12p <- loo(lla_half)



attach.nimble(mcmc.output_half_add_a2_lactime_reduced_75_12p$samples)


# Gelman-Rubin diagnostic (AKA RHat or PSRF)
gelman.diag(mcmc.output_half_add_a2_lactime_reduced_75_12p$samples, multivariate = FALSE)



mcmcplot(mcmc.output_half_add_a2_lactime_reduced_75_12p$samples)



##how to get effects size
##jow to make the differnt charts in PP
##how to get time on deck chart
##uses zscore to preduct value. Use mean for all the other values
##will use for calf age ##what the fuck is a z-score?
post_mat <- as.matrix(mcmc.output_half_add_a2_lactime_reduced_75_12p$samples)
# 2. Specify the parameters you care about
params <- c("c_m_effect",
            "bm_effect",
            "dde_effect_m",
            "age_effect",
            "date_effect",
            "age_bm_effect",
            "age2_effect",
            # "precip_effect",
            "Intercept", 
            # "bod_con_effect",
            "lac_time_effect",
            # "sex_effect",
            #"time_1_preg_effect",
            
            # "spei_3_effect",
            "spei_12_effect"#,
            
)

# 3. Summarize each one
summary_df_half_add_a2_lactime_r_75_12p <- tibble(parameter = params) %>%
  rowwise() %>%
  mutate(
    median   = median(post_mat[, parameter]),
    lower95  = quantile(post_mat[, parameter], 0.025),
    upper95  = quantile(post_mat[, parameter], 0.975),
    # posterior probability effect > 0
    p_gt_zero = mean(post_mat[, parameter] > 0),
    # if you expect a negative effect, you can also compute P(<0):
    p_lt_zero = mean(post_mat[, parameter] < 0)
  ) %>%
  ungroup()

print(summary_df_half_add_a2_lactime_r_75_12p)

#################################add interaction
######################################################################################
####removed weak effect
###top model plus age
####half full################mean dde, spei_6
model_half_add_a2_lactime_reduced_combo <-  nimbleCode({
  
  #Priors
  Intercept  ~ dnorm(0, sd = 5)
  
  #lac_effect~ dt(mu=0, sigma=1, df=1)
  bm_effect ~ dt(mu=0, sigma=1, df=1)
  dde_effect_m ~ dt(mu=0, sigma=1, df=1) 
  age_effect ~ dt(mu=0, sigma=1, df=1)
  date_effect~ dt(mu=0, sigma=1, df=1)
  c_m_effect~ dt(mu=0, sigma=1, df=1)
  #date_effect~ dnorm(0, sd = 5)
  lac_time_effect ~dt(mu=0, sigma=1, df=1)
  sex_effect ~ dt(mu=0, sigma=1, df=1)
  
  time_1_preg_effect ~ dt(mu=0, sigma=1, df=1)
  # spei_3_effect ~ dt(mu = 0 , sigma = 1, df = 1)
  spei_6_effect ~ dt(mu = 0 , sigma = 1, df = 1)
  #spei_12_effect ~ dt(mu = 0 , sigma = 1, df = 1)
  age2_effect ~ dt(mu = 0, sigma = 1, df = 1)
  lac_score_preg_effect ~ dt(mu = 0, sigma = 1, df = 1)
  age_bm_effect ~ dt(mu = 0, sigma = 1, df = 1)
  
  lactime_cdate_effect ~ dt(mu = 0, sigma = 1, df = 1)
  #sex_lac_effect ~ dt(mu = 0, sigma = 1, df = 1)
  
  
  
  for(i in 1:n.obs){
    
    #Process Model
    logit(preg_prob[i])<-Intercept+
      #(lac_score[i]*lac_effect)+
      (est_bm[i]*bm_effect)+
      (dde_m[i]*dde_effect_m)+
      (age[i]*age_effect)+
      (calf_mass[i]* c_m_effect)+
      (pow(age[i], 2)*age2_effect)+
      (calving_date[i] *date_effect )+
      # (sex_correct[i] * sex_effect) +
      (lac_time[i]* lac_time_effect)+
      #(time_1_preg[i]* time_1_preg_effect)+
      # (spei_3[i]*spei_3_effect)+
      (spei_6[i]*spei_6_effect)+
      (calving_date[i]*lac_time[i]*lactime_cdate_effect)+
      #(spei_12[i]*spei_12_effect)#+
      (age[i]*est_bm[i]*age_bm_effect)
    
    
    
    
    
    
    #Observation Model (Likelihood)
    preg_status[i] ~ dbern(preg_prob[i])
    
    #Impute missing data values
    lac_score[i] ~ dnorm(mean=0,sd=1)
    # precip_year_prior[i] ~ dnorm(mean = 0, sd = 1)
    #dde_95[i] ~ dnorm(mean=0,sd=1)
     age[i] ~ dnorm(mean=0,sd=1)
    # est_body_cond[i] ~ dnorm(mean=0,sd=1)
    est_bm[i] ~ dnorm(mean=0,sd=1)
    
    #lac_time[i] ~ dnorm(mean=0,sd=1)##does this matrer
    calving_date[i] ~ dnorm(mean = 0, sd = 1)
    # spei_6[i] ~ dnorm(mean = 0, sd = 1)
    # spei_12[i] ~ dnorm(mean = 0, sd = 1)
    
    
    
  }#i
  
})#model5
parameters <- c("c_m_effect",
                "bm_effect",
                "dde_effect_m",
                "age_effect",
                "date_effect",
                "age_bm_effect",
                "age2_effect",
                # "precip_effect",
                "Intercept", 
                # "bod_con_effect",
                "lac_time_effect",
                # "sex_effect",
                #"time_1_preg_effect",
                "logProb_preg_status",#
                "lactime_cdate_effect",
                # "spei_3_effect",
                "spei_6_effect"#,
                #"spei_12_effect"#,
                #"lac_score_preg_effect"#,
                ##"sex_lac_effect"
)

# MCMC Settings
ni <- 400000
nt <- 400
nb <- 200000
nc <- 3

nimble.data = list(
  calf_mass = as.numeric(cow_capture_all$mass_calf),
  preg_status = as.numeric(cow_capture_all$time_2_preg) - 1,
  lac_score = as.numeric((cow_capture_all$lac_score)),
  est_bm = as.numeric((cow_capture_all$est_bm)),
  #dde_95 = as.numeric((cow_capture_all$area_DDE_gt_95)),
  #dde_75 = as.numeric((cow_capture_all$area_DDE_gt_75)),
  dde_m = as.numeric((cow_capture_all$area_DDE_gt_mean)),
  age = as.numeric((cow_capture_all$age_estimate_2)),
  #precip_year_prior = as.numeric((cow_capture_all$total_year_prior)),
  sex_correct = ifelse(cow_capture_all$sex_correct== "m", 1,0),
  lac_time= as.numeric(cow_capture_all$lac_time), 
  time_1_preg= as.numeric(cow_capture_all$time_1_preg)-1,#
  calving_date = as.numeric(cow_capture_all$birt_date),
  #spei_3 = as.numeric((cow_capture_all$sept_year_prior_3M)),
  spei_6 = as.numeric((cow_capture_all$sept_year_prior_6M))#,
  #spei_12 = as.numeric((cow_capture_all$sept_year_prior_12M))
  
)

nimble.constants = list(n.obs = nrow(cow_capture_all))


mcmc.output_half_add_a2_lactime_reduced_combo<- nimbleMCMC(code = model_half_add_a2_lactime_reduced_combo,
                                                     data = nimble.data,
                                                     constants=nimble.constants,
                                                     monitors = parameters,
                                                     niter = ni,
                                                     nburnin = nb,
                                                     nchains = nc,
                                                     thin=nt,
                                                     summary=TRUE,
                                                     samplesAsCodaMCMC = TRUE)

attach.nimble(mcmc.output_half_add_a2_lactime_reduced_combo$samples)


mata_half  <- do.call(rbind, lapply(mcmc.output_half_add_a2_lactime_reduced_combo$samples, as.matrix))
lpa_half  <- grep("^logProb_preg_status\\[", colnames(mata_half), value = TRUE)
lla_half  <- mata_half[, lpa_half, drop = FALSE]
stopifnot(all(is.finite(lla_half) | (is.infinite(lla_half) & lla_half < 0)))
loo_half_add_a2_lactime_r_combo <- loo(lla_half)



attach.nimble(mcmc.output_half_add_a2_lactime_reduced_combo$samples)


# Gelman-Rubin diagnostic (AKA RHat or PSRF)
gelman.diag(mcmc.output_half_add_a2_lactime_reduced_combo$samples, multivariate = FALSE)



mcmcplot(mcmc.output_half_add_a2_lactime_reduced_combo$samples)



##how to get effects size
##jow to make the differnt charts in PP
##how to get time on deck chart
##uses zscore to preduct value. Use mean for all the other values
##will use for calf age ##what the fuck is a z-score?
post_mat <- as.matrix(mcmc.output_half_add_a2_lactime_reduced_combo$samples)
# 2. Specify the parameters you care about
params <- c("c_m_effect",
            "bm_effect",
            "dde_effect_m",
            "age_effect",
            "date_effect",
            "age_bm_effect",
            "age2_effect",
            # "precip_effect",
            "Intercept", 
            "lactime_cdate_effect",
            # "bod_con_effect",
            "lac_time_effect",
            # "sex_effect",
            #"time_1_preg_effect",
            
            # "spei_3_effect",
            "spei_6_effect"#,
            
)

# 3. Summarize each one
summary_df_half_add_a2_lactime_r_combo <- tibble(parameter = params) %>%
  rowwise() %>%
  mutate(
    median   = median(post_mat[, parameter]),
    lower95  = quantile(post_mat[, parameter], 0.025),
    upper95  = quantile(post_mat[, parameter], 0.975),
    # posterior probability effect > 0
    p_gt_zero = mean(post_mat[, parameter] > 0),
    # if you expect a negative effect, you can also compute P(<0):
    p_lt_zero = mean(post_mat[, parameter] < 0)
  ) %>%
  ungroup()

print(summary_df_half_add_a2_lactime_r_combo)
#############################################


####half full################mean dde, spei_6
model_half_add_a2_lactime_aged <-  nimbleCode({
  
  #Priors
  Intercept  ~ dnorm(0, sd = 5)
  
  #lac_effect~ dt(mu=0, sigma=1, df=1)
  bm_effect ~ dt(mu=0, sigma=1, df=1)
  dde_effect_m ~ dt(mu=0, sigma=1, df=1) 
  age_effect ~ dt(mu=0, sigma=1, df=1)
  date_effect~ dt(mu=0, sigma=1, df=1)
  c_m_effect~ dt(mu=0, sigma=1, df=1)
  #date_effect~ dnorm(0, sd = 5)
  lac_time_effect ~dt(mu=0, sigma=1, df=1)
  sex_effect ~ dt(mu=0, sigma=1, df=1)
  age_date_effect~dt(mu=0, sigma=1, df=1)
  time_1_preg_effect ~ dt(mu=0, sigma=1, df=1)
  # spei_3_effect ~ dt(mu = 0 , sigma = 1, df = 1)
  spei_6_effect ~ dt(mu = 0 , sigma = 1, df = 1)
  #spei_12_effect ~ dt(mu = 0 , sigma = 1, df = 1)
  age2_effect ~ dt(mu = 0, sigma = 1, df = 1)
  lac_score_preg_effect ~ dt(mu = 0, sigma = 1, df = 1)
  age_bm_effect ~ dt(mu = 0, sigma = 1, df = 1)
  #sex_lac_effect ~ dt(mu = 0, sigma = 1, df = 1)
  
  
  
  for(i in 1:n.obs){
    
    #Process Model
    logit(preg_prob[i])<-Intercept+
      #(lac_score[i]*lac_effect)+
      (est_bm[i]*bm_effect)+
      (dde_m[i]*dde_effect_m)+
      (age[i]*age_effect)+
      (calf_mass[i]* c_m_effect)+
      (pow(age[i], 2)*age2_effect)+
      (calving_date[i] *date_effect )+
      # (sex_correct[i] * sex_effect) +
      (lac_time[i]* lac_time_effect)+
      (calving_date[i]*age[i] *age_date_effect)+
      #(time_1_preg[i]* time_1_preg_effect)+
      # (spei_3[i]*spei_3_effect)+
      (spei_6[i]*spei_6_effect)+
      #(spei_12[i]*spei_12_effect)#+
      (age[i]*est_bm[i]*age_bm_effect)
    
    
    
    
    
    
    #Observation Model (Likelihood)
    preg_status[i] ~ dbern(preg_prob[i])
    
    #Impute missing data values
    lac_score[i] ~ dnorm(mean=0,sd=1)
    # precip_year_prior[i] ~ dnorm(mean = 0, sd = 1)
    #dde_95[i] ~ dnorm(mean=0,sd=1)
     age[i] ~ dnorm(mean=0,sd=1)
    # est_body_cond[i] ~ dnorm(mean=0,sd=1)
    est_bm[i] ~ dnorm(mean=0,sd=1)
    
    #lac_time[i] ~ dnorm(mean=0,sd=1)##does this matrer
    calving_date[i] ~ dnorm(mean = 0, sd = 1)
    # spei_6[i] ~ dnorm(mean = 0, sd = 1)
    # spei_12[i] ~ dnorm(mean = 0, sd = 1)
    
    
    
  }#i
  
})#model5
parameters <- c("c_m_effect",
                "bm_effect",
                "dde_effect_m",
                "age_effect",
                "date_effect",
                "age_bm_effect",
                "age2_effect",
                "age_date_effect",
                # "precip_effect",
                "Intercept", 
                # "bod_con_effect",
                "lac_time_effect",
                # "sex_effect",
                #"time_1_preg_effect",
                "logProb_preg_status",#
                # "spei_3_effect",
                "spei_6_effect"#,
                #"spei_12_effect"#,
                #"lac_score_preg_effect"#,
                ##"sex_lac_effect"
)

# MCMC Settings
ni <- 400000
nt <- 400
nb <- 200000
nc <- 3

nimble.data = list(
  calf_mass = as.numeric(cow_capture_all$mass_calf),
  preg_status = as.numeric(cow_capture_all$time_2_preg) - 1,
  lac_score = as.numeric((cow_capture_all$lac_score)),
  est_bm = as.numeric((cow_capture_all$est_bm)),
  #dde_95 = as.numeric((cow_capture_all$area_DDE_gt_95)),
  #dde_75 = as.numeric((cow_capture_all$area_DDE_gt_75)),
  dde_m = as.numeric((cow_capture_all$area_DDE_gt_mean)),
  age = as.numeric((cow_capture_all$age_estimate_2)),
  #precip_year_prior = as.numeric((cow_capture_all$total_year_prior)),
  sex_correct = ifelse(cow_capture_all$sex_correct== "m", 1,0),
  lac_time= as.numeric(cow_capture_all$lac_time), 
  time_1_preg= as.numeric(cow_capture_all$time_1_preg)-1,#
  calving_date = as.numeric(cow_capture_all$birt_date),
  #spei_3 = as.numeric((cow_capture_all$sept_year_prior_3M)),
  spei_6 = as.numeric((cow_capture_all$sept_year_prior_6M))#,
  #spei_12 = as.numeric((cow_capture_all$sept_year_prior_12M))
  
)

nimble.constants = list(n.obs = nrow(cow_capture_all))


mcmc.output_half_add_a2_lactime_aged<- nimbleMCMC(code = model_half_add_a2_lactime_aged,
                                                     data = nimble.data,
                                                     constants=nimble.constants,
                                                     monitors = parameters,
                                                     niter = ni,
                                                     nburnin = nb,
                                                     nchains = nc,
                                                     thin=nt,
                                                     summary=TRUE,
                                                     samplesAsCodaMCMC = TRUE)

attach.nimble(mcmc.output_half_add_a2_lactime_aged$samples)


mata_half  <- do.call(rbind, lapply(mcmc.output_half_add_a2_lactime_aged$samples, as.matrix))
lpa_half  <- grep("^logProb_preg_status\\[", colnames(mata_half), value = TRUE)
lla_half  <- mata_half[, lpa_half, drop = FALSE]
stopifnot(all(is.finite(lla_half) | (is.infinite(lla_half) & lla_half < 0)))
loo_half_add_a2_lactime_aged <- loo(lla_half)



attach.nimble(mcmc.output_half_add_a2_lactime_aged$samples)


# Gelman-Rubin diagnostic (AKA RHat or PSRF)
gelman.diag(mcmc.output_half_add_a2_lactime_aged$samples, multivariate = FALSE)



mcmcplot(mcmc.output_half_add_a2_lactime_aged$samples)



##how to get effects size
##jow to make the differnt charts in PP
##how to get time on deck chart
##uses zscore to preduct value. Use mean for all the other values
##will use for calf age ##what the fuck is a z-score?
post_mat <- as.matrix(mcmc.output_half_add_a2_lactime_aged$samples)
# 2. Specify the parameters you care about
params <- c("c_m_effect",
            "bm_effect",
            "dde_effect_m",
            "age_effect",
            "date_effect",
            "age_bm_effect",
            "age_date_effect",
            "age2_effect",
            # "precip_effect",
            "Intercept", 
            # "bod_con_effect",
            "lac_time_effect",
            # "sex_effect",
            #"time_1_preg_effect",
            
            # "spei_3_effect",
            "spei_6_effect"#,
            
)

# 3. Summarize each one
summary_df_half_add_a2_lactime_r_aged <- tibble(parameter = params) %>%
  rowwise() %>%
  mutate(
    median   = median(post_mat[, parameter]),
    lower95  = quantile(post_mat[, parameter], 0.025),
    upper95  = quantile(post_mat[, parameter], 0.975),
    # posterior probability effect > 0
    p_gt_zero = mean(post_mat[, parameter] > 0),
    # if you expect a negative effect, you can also compute P(<0):
    p_lt_zero = mean(post_mat[, parameter] < 0)
  ) %>%
  ungroup()

print(summary_df_half_add_a2_lactime_r_aged)

##############################
####removed weak effect
###top model plus age
####half full################mean dde, spei_6
model_half_add_a2_lactime_reduced <-  nimbleCode({
  
  #Priors
  Intercept  ~ dnorm(0, sd = 5)
  
  #lac_effect~ dt(mu=0, sigma=1, df=1)
  bm_effect ~ dt(mu=0, sigma=1, df=1)
  dde_effect_m ~ dt(mu=0, sigma=1, df=1) 
  age_effect ~ dt(mu=0, sigma=1, df=1)
  date_effect~ dt(mu=0, sigma=1, df=1)
  c_m_effect~ dt(mu=0, sigma=1, df=1)
  #date_effect~ dnorm(0, sd = 5)
  lac_time_effect ~dt(mu=0, sigma=1, df=1)
  sex_effect ~ dt(mu=0, sigma=1, df=1)
  calf_ind_effect ~ dt(mu = 0, sigma = 1, df = 1) 
  time_1_preg_effect ~ dt(mu=0, sigma=1, df=1)
  # spei_3_effect ~ dt(mu = 0 , sigma = 1, df = 1)
  spei_6_effect ~ dt(mu = 0 , sigma = 1, df = 1)
  #spei_12_effect ~ dt(mu = 0 , sigma = 1, df = 1)
  age2_effect ~ dt(mu = 0, sigma = 1, df = 1)
  lac_score_preg_effect ~ dt(mu = 0, sigma = 1, df = 1)
  age_bm_effect ~ dt(mu = 0, sigma = 1, df = 1)
  #sex_lac_effect ~ dt(mu = 0, sigma = 1, df = 1)
  
  
  
  for(i in 1:n.obs){
    
    #Process Model
    logit(preg_prob[i])<-Intercept+
      #(lac_score[i]*lac_effect)+
      (est_bm[i]*bm_effect)+
      (dde_m[i]*dde_effect_m)+
      (age[i]*age_effect)+
      (calf_ind[i] * calf_ind_effect) +
      (calf_mass[i]* c_m_effect)+
      (pow(age[i], 2)*age2_effect)+
      (calving_date[i] *date_effect )+
     # (sex_correct[i] * sex_effect) +
      (lac_time[i]* lac_time_effect)+
      #(time_1_preg[i]* time_1_preg_effect)+
      # (spei_3[i]*spei_3_effect)+
      (spei_6[i]*spei_6_effect)+
      #(spei_12[i]*spei_12_effect)#+
      (age[i]*est_bm[i]*age_bm_effect)
    
    
    
    
    
    
    #Observation Model (Likelihood)
    preg_status[i] ~ dbern(preg_prob[i])
    
    #Impute missing data values
    lac_score[i] ~ dnorm(mean=0,sd=1)
    # precip_year_prior[i] ~ dnorm(mean = 0, sd = 1)
    #dde_95[i] ~ dnorm(mean=0,sd=1)
     age[i] ~ dnorm(mean=0,sd=1)
    # est_body_cond[i] ~ dnorm(mean=0,sd=1)
    est_bm[i] ~ dnorm(mean=0,sd=1)
    
    #lac_time[i] ~ dnorm(mean=0,sd=1)##does this matrer
    calving_date[i] ~ dnorm(mean = 0, sd = 1)
    # spei_6[i] ~ dnorm(mean = 0, sd = 1)
    # spei_12[i] ~ dnorm(mean = 0, sd = 1)
    
    
    
  }#i
  
})#model5
parameters <- c("c_m_effect",
                "bm_effect",
                "dde_effect_m",
                "age_effect",
                "date_effect",
                "age_bm_effect",
                "age2_effect",
                "calf_ind_effect",
                # "precip_effect",
                "Intercept", 
                # "bod_con_effect",
                "lac_time_effect",
               # "sex_effect",
                #"time_1_preg_effect",
                "logProb_preg_status",#
                # "spei_3_effect",
                "spei_6_effect"#,
                #"spei_12_effect"#,
                #"lac_score_preg_effect"#,
                ##"sex_lac_effect"
)

# MCMC Settings
ni <- 400000
nt <- 400
nb <- 200000
nc <- 3

nimble.data = list(
  calf_mass = as.numeric(cow_capture_all$mass_calf),
  preg_status = as.numeric(cow_capture_all$time_2_preg) - 1,
  lac_score = as.numeric((cow_capture_all$lac_score)),
  est_bm = as.numeric((cow_capture_all$est_bm)),
  calf_ind =  as.numeric(cow_capture_all$calf_ind),
  #dde_95 = as.numeric((cow_capture_all$area_DDE_gt_95)),
  #dde_75 = as.numeric((cow_capture_all$area_DDE_gt_75)),
  dde_m = as.numeric((cow_capture_all$area_DDE_gt_mean)),
  age = as.numeric((cow_capture_all$age_estimate_2)),
  #precip_year_prior = as.numeric((cow_capture_all$total_year_prior)),
  sex_correct = ifelse(cow_capture_all$sex_correct== "m", 1,0),
  lac_time= as.numeric(cow_capture_all$lac_time), 
  time_1_preg= as.numeric(cow_capture_all$time_1_preg)-1,#
  calving_date = as.numeric(cow_capture_all$birt_date),
  #spei_3 = as.numeric((cow_capture_all$sept_year_prior_3M)),
  spei_6 = as.numeric((cow_capture_all$sept_year_prior_6M))#,
  #spei_12 = as.numeric((cow_capture_all$sept_year_prior_12M))
  
)

nimble.constants = list(n.obs = nrow(cow_capture_all))


mcmc.output_half_add_a2_lactime_reduced<- nimbleMCMC(code = model_half_add_a2_lactime_reduced,
                                             data = nimble.data,
                                             constants=nimble.constants,
                                             monitors = parameters,
                                             niter = ni,
                                             nburnin = nb,
                                             nchains = nc,
                                             thin=nt,
                                             summary=TRUE,
                                             samplesAsCodaMCMC = TRUE)

attach.nimble(mcmc.output_half_add_a2_lactime_reduced$samples)


mata_half  <- do.call(rbind, lapply(mcmc.output_half_add_a2_lactime_reduced$samples, as.matrix))
lpa_half  <- grep("^logProb_preg_status\\[", colnames(mata_half), value = TRUE)
lla_half  <- mata_half[, lpa_half, drop = FALSE]
stopifnot(all(is.finite(lla_half) | (is.infinite(lla_half) & lla_half < 0)))
loo_half_add_a2_lactime_r <- loo(lla_half)



attach.nimble(mcmc.output_half_add_a2_lactime_reduced$samples)


# Gelman-Rubin diagnostic (AKA RHat or PSRF)
gelman.diag(mcmc.output_half_add_a2_lactime_reduced$samples, multivariate = FALSE)



mcmcplot(mcmc.output_half_add_a2_lactime_reduced$samples)



##how to get effects size
##jow to make the differnt charts in PP
##how to get time on deck chart
##uses zscore to preduct value. Use mean for all the other values
##will use for calf age ##what the fuck is a z-score?
post_mat <- as.matrix(mcmc.output_half_add_a2_lactime_reduced$samples)
# 2. Specify the parameters you care about
params <- c("c_m_effect",
            "bm_effect",
            "dde_effect_m",
            "age_effect",
            "date_effect",
            "age_bm_effect",
            "age2_effect",
            # "precip_effect",
            "Intercept", 
            # "bod_con_effect",
            "lac_time_effect",
            "calf_ind_effect",
            # "sex_effect",
            #"time_1_preg_effect",
           
            # "spei_3_effect",
            "spei_6_effect"#,
            
)

# 3. Summarize each one
summary_df_half_add_a2_lactime_r <- tibble(parameter = params) %>%
  rowwise() %>%
  mutate(
    median   = median(post_mat[, parameter]),
    lower95  = quantile(post_mat[, parameter], 0.025),
    upper95  = quantile(post_mat[, parameter], 0.975),
    # posterior probability effect > 0
    p_gt_zero = mean(post_mat[, parameter] > 0),
    # if you expect a negative effect, you can also compute P(<0):
    p_lt_zero = mean(post_mat[, parameter] < 0)
  ) %>%
  ungroup()

print(summary_df_half_add_a2_lactime_r)

##############################
####removed weak effect
###top model plus age, removed more weak effects added preg time 1, 
####half full################mean dde, spei_6
model_half_add_a2_lactime_reduced_preg <-  nimbleCode({
  
  #Priors
  Intercept  ~ dnorm(0, sd = 5)
  
  #lac_effect~ dt(mu=0, sigma=1, df=1)
  bm_effect ~ dt(mu=0, sigma=1, df=1)
  dde_effect_m ~ dt(mu=0, sigma=1, df=1) 
  age_effect ~ dt(mu=0, sigma=1, df=1)
  date_effect~ dt(mu=0, sigma=1, df=1)
  c_m_effect~ dt(mu=0, sigma=1, df=1)
  #date_effect~ dnorm(0, sd = 5)
  lac_time_effect ~dt(mu=0, sigma=1, df=1)
  sex_effect ~ dt(mu=0, sigma=1, df=1)
  calf_ind_effect ~ dt(mu = 0, sigma = 1, df = 1) 
  time_1_preg_effect ~ dt(mu=0, sigma=1, df=1)
  # spei_3_effect ~ dt(mu = 0 , sigma = 1, df = 1)
  spei_6_effect ~ dt(mu = 0 , sigma = 1, df = 1)
  #spei_12_effect ~ dt(mu = 0 , sigma = 1, df = 1)
  age2_effect ~ dt(mu = 0, sigma = 1, df = 1)
  lac_score_preg_effect ~ dt(mu = 0, sigma = 1, df = 1)
  age_bm_effect ~ dt(mu = 0, sigma = 1, df = 1)
  #sex_lac_effect ~ dt(mu = 0, sigma = 1, df = 1)
  
  
  
  for(i in 1:n.obs){
    
    #Process Model
    logit(preg_prob[i])<-Intercept+
      #(lac_score[i]*lac_effect)+
      (est_bm[i]*bm_effect)+
      (dde_m[i]*dde_effect_m)+
      (age[i]*age_effect)+
      (calf_ind[i] * calf_ind_effect) +
      (calf_mass[i]* c_m_effect)+
      (pow(age[i], 2)*age2_effect)+
      (calving_date[i] *date_effect )+
      # (sex_correct[i] * sex_effect) +
      (lac_time[i]* lac_time_effect)+
      (time_1_preg[i]* time_1_preg_effect)+
      # (spei_3[i]*spei_3_effect)+
      (spei_6[i]*spei_6_effect)+
      #(spei_12[i]*spei_12_effect)#+
      (age[i]*est_bm[i]*age_bm_effect)
    
    
    
    
    
    
    #Observation Model (Likelihood)
    preg_status[i] ~ dbern(preg_prob[i])
    
    #Impute missing data values
    lac_score[i] ~ dnorm(mean=0,sd=1)
    # precip_year_prior[i] ~ dnorm(mean = 0, sd = 1)
    #dde_95[i] ~ dnorm(mean=0,sd=1)
    age[i] ~ dnorm(mean=0,sd=1)
    # est_body_cond[i] ~ dnorm(mean=0,sd=1)
    est_bm[i] ~ dnorm(mean=0,sd=1)
    
    #lac_time[i] ~ dnorm(mean=0,sd=1)##does this matrer
    calving_date[i] ~ dnorm(mean = 0, sd = 1)
    # spei_6[i] ~ dnorm(mean = 0, sd = 1)
    # spei_12[i] ~ dnorm(mean = 0, sd = 1)
    
    
    
  }#i
  
})#model5
parameters <- c("c_m_effect",
                "bm_effect",
                "dde_effect_m",
                "age_effect",
                "date_effect",
                "age_bm_effect",
                "age2_effect",
                "calf_ind_effect",
                # "precip_effect",
                "Intercept", 
                # "bod_con_effect",
                "lac_time_effect",
                # "sex_effect",
                "time_1_preg_effect",
                "logProb_preg_status",#
                # "spei_3_effect",
                "spei_6_effect"#,
                #"spei_12_effect"#,
                #"lac_score_preg_effect"#,
                ##"sex_lac_effect"
)

# MCMC Settings
ni <- 400000
nt <- 400
nb <- 200000
nc <- 3

nimble.data = list(
  calf_mass = as.numeric(cow_capture_all$mass_calf),
  preg_status = as.numeric(cow_capture_all$time_2_preg) - 1,
  lac_score = as.numeric((cow_capture_all$lac_score)),
  est_bm = as.numeric((cow_capture_all$est_bm)),
  calf_ind =  as.numeric(cow_capture_all$calf_ind),
  #dde_95 = as.numeric((cow_capture_all$area_DDE_gt_95)),
  #dde_75 = as.numeric((cow_capture_all$area_DDE_gt_75)),
  dde_m = as.numeric((cow_capture_all$area_DDE_gt_mean)),
  age = as.numeric((cow_capture_all$age_estimate_2)),
  #precip_year_prior = as.numeric((cow_capture_all$total_year_prior)),
  sex_correct = ifelse(cow_capture_all$sex_correct== "m", 1,0),
  lac_time= as.numeric(cow_capture_all$lac_time), 
  time_1_preg= as.numeric(cow_capture_all$time_1_preg)-1,#
  calving_date = as.numeric(cow_capture_all$birt_date),
  #spei_3 = as.numeric((cow_capture_all$sept_year_prior_3M)),
  spei_6 = as.numeric((cow_capture_all$sept_year_prior_6M))#,
  #spei_12 = as.numeric((cow_capture_all$sept_year_prior_12M))
  
)

nimble.constants = list(n.obs = nrow(cow_capture_all))


mcmc.output_half_add_a2_lactime_reduced_preg<- nimbleMCMC(code = model_half_add_a2_lactime_reduced_preg,
                                                     data = nimble.data,
                                                     constants=nimble.constants,
                                                     monitors = parameters,
                                                     niter = ni,
                                                     nburnin = nb,
                                                     nchains = nc,
                                                     thin=nt,
                                                     summary=TRUE,
                                                     samplesAsCodaMCMC = TRUE)

attach.nimble(mcmc.output_half_add_a2_lactime_reduced$samples)


mata_half  <- do.call(rbind, lapply(mcmc.output_half_add_a2_lactime_reduced_preg$samples, as.matrix))
lpa_half  <- grep("^logProb_preg_status\\[", colnames(mata_half), value = TRUE)
lla_half  <- mata_half[, lpa_half, drop = FALSE]
stopifnot(all(is.finite(lla_half) | (is.infinite(lla_half) & lla_half < 0)))
loo_half_add_a2_lactime_preg <- loo(lla_half)



attach.nimble(mcmc.output_half_add_a2_lactime_reduced_preg$samples)


# Gelman-Rubin diagnostic (AKA RHat or PSRF)
gelman.diag(mcmc.output_half_add_a2_lactime_reduced_preg$samples, multivariate = FALSE)



mcmcplot(mcmc.output_half_add_a2_lactime_reduced_preg$samples)



##how to get effects size
##jow to make the differnt charts in PP
##how to get time on deck chart
##uses zscore to preduct value. Use mean for all the other values
##will use for calf age ##what the fuck is a z-score?
post_mat <- as.matrix(mcmc.output_half_add_a2_lactime_reduced_preg$samples)
# 2. Specify the parameters you care about
params <- c("c_m_effect",
            "bm_effect",
            "dde_effect_m",
            "age_effect",
            "date_effect",
            "age_bm_effect",
            "age2_effect",
            # "precip_effect",
            "Intercept", 
            # "bod_con_effect",
            "lac_time_effect",
            "calf_ind_effect",
            # "sex_effect",
            "time_1_preg_effect",
            
            # "spei_3_effect",
            "spei_6_effect"#,
            
)

# 3. Summarize each one
summary_df_half_add_a2_lactime_preg <- tibble(parameter = params) %>%
  rowwise() %>%
  mutate(
    median   = median(post_mat[, parameter]),
    lower95  = quantile(post_mat[, parameter], 0.025),
    upper95  = quantile(post_mat[, parameter], 0.975),
    # posterior probability effect > 0
    p_gt_zero = mean(post_mat[, parameter] > 0),
    # if you expect a negative effect, you can also compute P(<0):
    p_lt_zero = mean(post_mat[, parameter] < 0)
  ) %>%
  ungroup()

print(summary_df_half_add_a2_lactime_preg)

##############################add interaction
##############################
####removed weak effect
###top model plus age, removed more weak effects added preg time 1, 
####half full################mean dde, spei_6
model_half_add_a2_lactime_reduced_preg_lac <-  nimbleCode({
  
  #Priors
  Intercept  ~ dnorm(0, sd = 5)
  
  #lac_effect~ dt(mu=0, sigma=1, df=1)
  bm_effect ~ dt(mu=0, sigma=1, df=1)
  dde_effect_m ~ dt(mu=0, sigma=1, df=1) 
  age_effect ~ dt(mu=0, sigma=1, df=1)
  date_effect~ dt(mu=0, sigma=1, df=1)
  c_m_effect~ dt(mu=0, sigma=1, df=1)
  #date_effect~ dnorm(0, sd = 5)
  lac_time_effect ~dt(mu=0, sigma=1, df=1)
  sex_effect ~ dt(mu=0, sigma=1, df=1)
  calf_ind_effect ~ dt(mu = 0, sigma = 1, df = 1) 
  time_1_preg_effect ~ dt(mu=0, sigma=1, df=1)
  # spei_3_effect ~ dt(mu = 0 , sigma = 1, df = 1)
  spei_6_effect ~ dt(mu = 0 , sigma = 1, df = 1)
  #spei_12_effect ~ dt(mu = 0 , sigma = 1, df = 1)
  age2_effect ~ dt(mu = 0, sigma = 1, df = 1)
  lac_score_preg_effect ~ dt(mu = 0, sigma = 1, df = 1)
  age_bm_effect ~ dt(mu = 0, sigma = 1, df = 1)
  date_lac_effect ~ dt(mu = 0, sigma = 1, df = 1)
  #sex_lac_effect ~ dt(mu = 0, sigma = 1, df = 1)
  
  
  
  for(i in 1:n.obs){
    
    #Process Model
    logit(preg_prob[i])<-Intercept+
      #(lac_score[i]*lac_effect)+
      (est_bm[i]*bm_effect)+
      (dde_m[i]*dde_effect_m)+
      (age[i]*age_effect)+
      (calf_ind[i] * calf_ind_effect) +
      (calf_mass[i]* c_m_effect)+
      (pow(age[i], 2)*age2_effect)+
      (calving_date[i] *date_effect )+
       (sex_correct[i] * sex_effect) +
      (lac_time[i]* lac_time_effect)+
      (time_1_preg[i]* time_1_preg_effect)+
      # (spei_3[i]*spei_3_effect)+
      (spei_6[i]*spei_6_effect)+
      (date_lac_effect *lac_time[i]*calving_date[i] )+
      #(spei_12[i]*spei_12_effect)#+
      (age[i]*est_bm[i]*age_bm_effect)
    
    
    
    
    
    
    #Observation Model (Likelihood)
    preg_status[i] ~ dbern(preg_prob[i])
    
    #Impute missing data values
    lac_score[i] ~ dnorm(mean=0,sd=1)
    # precip_year_prior[i] ~ dnorm(mean = 0, sd = 1)
    #dde_95[i] ~ dnorm(mean=0,sd=1)
    age[i] ~ dnorm(mean=0,sd=1)
    # est_body_cond[i] ~ dnorm(mean=0,sd=1)
    est_bm[i] ~ dnorm(mean=0,sd=1)
    
    #lac_time[i] ~ dnorm(mean=0,sd=1)##does this matrer
    calving_date[i] ~ dnorm(mean = 0, sd = 1)
    # spei_6[i] ~ dnorm(mean = 0, sd = 1)
    # spei_12[i] ~ dnorm(mean = 0, sd = 1)
    
    
    
  }#i
  
})#model5
parameters <- c("c_m_effect",
                "bm_effect",
                "dde_effect_m",
                "age_effect",
                "date_effect",
                "age_bm_effect",
                "age2_effect",
                "calf_ind_effect",
                "date_lac_effect",
                # "precip_effect",
                "Intercept", 
                # "bod_con_effect",
                "lac_time_effect",
                 "sex_effect",
                "time_1_preg_effect",
                "logProb_preg_status",#
                # "spei_3_effect",
                "spei_6_effect"#,
                #"spei_12_effect"#,
                #"lac_score_preg_effect"#,
                ##"sex_lac_effect"
)

# MCMC Settings
ni <- 400000
nt <- 400
nb <- 200000
nc <- 3

nimble.data = list(
  calf_mass = as.numeric(cow_capture_all$mass_calf),
  preg_status = as.numeric(cow_capture_all$time_2_preg) - 1,
  lac_score = as.numeric((cow_capture_all$lac_score)),
  est_bm = as.numeric((cow_capture_all$est_bm)),
  calf_ind =  as.numeric(cow_capture_all$calf_ind),
  #dde_95 = as.numeric((cow_capture_all$area_DDE_gt_95)),
  #dde_75 = as.numeric((cow_capture_all$area_DDE_gt_75)),
  dde_m = as.numeric((cow_capture_all$area_DDE_gt_mean)),
  age = as.numeric((cow_capture_all$age_estimate_2)),
  #precip_year_prior = as.numeric((cow_capture_all$total_year_prior)),
  sex_correct = ifelse(cow_capture_all$sex_correct== "m", 1,0),
  lac_time= as.numeric(cow_capture_all$lac_time), 
  time_1_preg= as.numeric(cow_capture_all$time_1_preg)-1,#
  calving_date = as.numeric(cow_capture_all$birt_date),
  #spei_3 = as.numeric((cow_capture_all$sept_year_prior_3M)),
  spei_6 = as.numeric((cow_capture_all$sept_year_prior_6M))#,
  #spei_12 = as.numeric((cow_capture_all$sept_year_prior_12M))
  
)

nimble.constants = list(n.obs = nrow(cow_capture_all))


mcmc.output_half_add_a2_lactime_reduced_preg_lac<- nimbleMCMC(code = model_half_add_a2_lactime_reduced_preg_lac,
                                                          data = nimble.data,
                                                          constants=nimble.constants,
                                                          monitors = parameters,
                                                          niter = ni,
                                                          nburnin = nb,
                                                          nchains = nc,
                                                          thin=nt,
                                                          summary=TRUE,
                                                          samplesAsCodaMCMC = TRUE)

attach.nimble(mcmc.output_half_add_a2_lactime_reduced_preg_lac$samples)


mata_half  <- do.call(rbind, lapply(mcmc.output_half_add_a2_lactime_reduced_preg_lac$samples, as.matrix))
lpa_half  <- grep("^logProb_preg_status\\[", colnames(mata_half), value = TRUE)
lla_half  <- mata_half[, lpa_half, drop = FALSE]
stopifnot(all(is.finite(lla_half) | (is.infinite(lla_half) & lla_half < 0)))
loo_half_add_a2_lactime_preg_lac <- loo(lla_half)



attach.nimble(mcmc.output_half_add_a2_lactime_reduced_preg_lac$samples)


# Gelman-Rubin diagnostic (AKA RHat or PSRF)
gelman.diag(mcmc.output_half_add_a2_lactime_reduced_preg_lac$samples, multivariate = FALSE)



mcmcplot(mcmc.output_half_add_a2_lactime_reduced_preg_lac$samples)



##how to get effects size
##jow to make the differnt charts in PP
##how to get time on deck chart
##uses zscore to preduct value. Use mean for all the other values
##will use for calf age ##what the fuck is a z-score?
post_mat <- as.matrix(mcmc.output_half_add_a2_lactime_reduced_preg_lac$samples)
# 2. Specify the parameters you care about
params <- c("c_m_effect",
            "bm_effect",
            "dde_effect_m",
            "age_effect",
            "date_effect",
            "age_bm_effect",
            "age2_effect",
            "date_lac_effect",
            # "precip_effect",
            "Intercept", 
            # "bod_con_effect",
            "lac_time_effect",
            "calf_ind_effect",
            # "sex_effect",
            "time_1_preg_effect",
            
            # "spei_3_effect",
            "spei_6_effect"#,
            
)

# 3. Summarize each one
summary_df_half_add_a2_lactime_preg_lac <- tibble(parameter = params) %>%
  rowwise() %>%
  mutate(
    median   = median(post_mat[, parameter]),
    lower95  = quantile(post_mat[, parameter], 0.025),
    upper95  = quantile(post_mat[, parameter], 0.975),
    # posterior probability effect > 0
    p_gt_zero = mean(post_mat[, parameter] > 0),
    # if you expect a negative effect, you can also compute P(<0):
    p_lt_zero = mean(post_mat[, parameter] < 0)
  ) %>%
  ungroup()

print(summary_df_half_add_a2_lactime_preg_lac)

#########################
##########global


####################mean dde, spei_6
model_global<-  nimbleCode({
  
  #Priors
  Intercept  ~ dnorm(0, sd = 5)
  
  lac_effect~ dt(mu=0, sigma=1, df=1)
  bm_effect ~ dt(mu=0, sigma=1, df=1)
  dde_effect_m ~ dt(mu=0, sigma=1, df=1) 
  age_effect ~ dt(mu=0, sigma=1, df=1)
  date_effect~ dt(mu=0, sigma=1, df=1)
  c_m_effect~ dt(mu=0, sigma=1, df=1)
  #date_effect~ dnorm(0, sd = 5)
  lac_time_effect ~dt(mu=0, sigma=1, df=1)
  sex_effect ~ dt(mu=0, sigma=1, df=1)
  calf_ind_effect ~ dt(mu = 0, sigma = 1, df = 1) 
  time_1_preg_effect ~ dt(mu=0, sigma=1, df=1)
  # spei_3_effect ~ dt(mu = 0 , sigma = 1, df = 1)
  spei_6_effect ~ dt(mu = 0 , sigma = 1, df = 1)
  #spei_12_effect ~ dt(mu = 0 , sigma = 1, df = 1)
  age2_effect ~ dt(mu = 0, sigma = 1, df = 1)
  lac_score_preg_effect ~ dt(mu = 0, sigma = 1, df = 1)
  age_bm_effect ~ dt(mu = 0, sigma = 1, df = 1)
  date_lac_effect ~ dt(mu = 0, sigma = 1, df = 1)
  #sex_lac_effect ~ dt(mu = 0, sigma = 1, df = 1)
  
  
  
  for(i in 1:n.obs){
    
    #Process Model
    logit(preg_prob[i])<-Intercept+
      (lac_score[i]*lac_effect)+
      (est_bm[i]*bm_effect)+
      (dde_m[i]*dde_effect_m)+
      (age[i]*age_effect)+
      (calf_ind[i] * calf_ind_effect) +
      (calf_mass[i]* c_m_effect)+
      (pow(age[i], 2)*age2_effect)+
      (calving_date[i] *date_effect )+
      (sex_correct[i] * sex_effect) +
      (lac_time[i]* lac_time_effect)+
      (time_1_preg[i]* time_1_preg_effect)+
      # (spei_3[i]*spei_3_effect)+
      (spei_6[i]*spei_6_effect)+
      (date_lac_effect *lac_time[i]*calving_date[i] )+
      #(spei_12[i]*spei_12_effect)#+
      (age[i]*est_bm[i]*age_bm_effect)
    
    
    
    
    
    
    #Observation Model (Likelihood)
    preg_status[i] ~ dbern(preg_prob[i])
    
    #Impute missing data values
    lac_score[i] ~ dnorm(mean=0,sd=1)
    # precip_year_prior[i] ~ dnorm(mean = 0, sd = 1)
    #dde_95[i] ~ dnorm(mean=0,sd=1)
    age[i] ~ dnorm(mean=0,sd=1)
    # est_body_cond[i] ~ dnorm(mean=0,sd=1)
    est_bm[i] ~ dnorm(mean=0,sd=1)
    
    #lac_time[i] ~ dnorm(mean=0,sd=1)##does this matrer
    calving_date[i] ~ dnorm(mean = 0, sd = 1)
     spei_6[i] ~ dnorm(mean = 0, sd = 1)
    # spei_12[i] ~ dnorm(mean = 0, sd = 1)
    
    
    
  }#i
  
})#model5
parameters <- c("c_m_effect",
                "bm_effect",
                "dde_effect_m",
                "age_effect",
                "date_effect",
                "age_bm_effect",
                "age2_effect",
                "calf_ind_effect",
                "date_lac_effect",
                "lac_effect",
                # "precip_effect",
                "Intercept", 
                # "bod_con_effect",
                "lac_time_effect",
                "sex_effect",
                "time_1_preg_effect",
                "logProb_preg_status",
                # "spei_3_effect",
                "spei_6_effect"#,
                #"spei_12_effect"#,
                #"lac_score_preg_effect"#,
                ##"sex_lac_effect"
)

# MCMC Settings
ni <- 400000
nt <- 400
nb <- 200000
nc <- 3

nimble.data = list(
  calf_mass = as.numeric(cow_capture_all$mass_calf),
  preg_status = as.numeric(cow_capture_all$time_2_preg) - 1,
  lac_score = as.numeric((cow_capture_all$lac_score)),
  est_bm = as.numeric((cow_capture_all$est_bm)),
  calf_ind =  as.numeric(cow_capture_all$calf_ind),
  dde_m = as.numeric((cow_capture_all$area_DDE_gt_95)),
  #dde_m = as.numeric((cow_capture_all$area_DDE_gt_75)),
  #dde_m = as.numeric((cow_capture_all$area_DDE_gt_mean)),
  age = as.numeric((cow_capture_all$age_estimate_2)),
  #precip_year_prior = as.numeric((cow_capture_all$total_year_prior)),
  sex_correct = ifelse(cow_capture_all$sex_correct== "m", 1,0),
  lac_time= as.numeric(cow_capture_all$lac_time), 
  time_1_preg= as.numeric(cow_capture_all$time_1_preg)-1,#
  calving_date = as.numeric(cow_capture_all$birt_date),
  #spei_3 = as.numeric((cow_capture_all$sept_year_prior_3M))#,
  spei_6 = as.numeric((cow_capture_all$sept_year_prior_6M))#,
  #spei_6 = as.numeric((cow_capture_all$sept_year_prior_12M))
  
)

nimble.constants = list(n.obs = nrow(cow_capture_all))


mcmc.output_global<- nimbleMCMC(code = model_global,
                                                              data = nimble.data,
                                                              constants=nimble.constants,
                                                              monitors = parameters,
                                                              niter = ni,
                                                              nburnin = nb,
                                                              nchains = nc,
                                                              thin=nt,
                                                              summary=TRUE,
                                                              samplesAsCodaMCMC = TRUE)

attach.nimble(mcmc.output_global$samples)


mata_half  <- do.call(rbind, lapply(mcmc.output_global$samples, as.matrix))
lpa_half  <- grep("^logProb_preg_status\\[", colnames(mata_half), value = TRUE)
lla_half  <- mata_half[, lpa_half, drop = FALSE]
stopifnot(all(is.finite(lla_half) | (is.infinite(lla_half) & lla_half < 0)))
loo_global<- loo(lla_half)



attach.nimble(mcmc.output_global$samples)


# Gelman-Rubin diagnostic (AKA RHat or PSRF)
gelman.diag(mcmc.output_global$samples, multivariate = FALSE)



mcmcplot(mcmc.output_global$samples)



##how to get effects size
##jow to make the differnt charts in PP
##how to get time on deck chart
##uses zscore to preduct value. Use mean for all the other values
##will use for calf age ##what the fuck is a z-score?
post_mat <- as.matrix(mcmc.output_global$samples)
# 2. Specify the parameters you care about
params <- c("c_m_effect",
            "bm_effect",
            "dde_effect_m",
            "age_effect",
            "date_effect",
            "age_bm_effect",
            "age2_effect",
            "calf_ind_effect",
            "date_lac_effect",
            "lac_effect",
            # "precip_effect",
            "Intercept", 
            # "bod_con_effect",
            "lac_time_effect",
            "sex_effect",
            "time_1_preg_effect",
            
            # "spei_3_effect",
            "spei_6_effect"#,
            #"spei_12_effect"#,
            #"lac_score_preg_effect"#,
            ##"sex_lac_effect"
            
)

# 3. Summarize each one
summary_df_global <- tibble(parameter = params) %>%
  rowwise() %>%
  mutate(
    median   = median(post_mat[, parameter]),
    lower95  = quantile(post_mat[, parameter], 0.025),
    upper95  = quantile(post_mat[, parameter], 0.975),
    # posterior probability effect > 0
    p_gt_zero = mean(post_mat[, parameter] > 0),
    # if you expect a negative effect, you can also compute P(<0):
    p_lt_zero = mean(post_mat[, parameter] < 0)
  ) %>%
  ungroup()

print(summary_df_global)

beep(2)
####removed weak effect
###top model plus age, removed more weak effects added preg time 1, 
####half full################mean dde, spei_6
model_half_add_a2_lactime_reduced_preg_lac_r <-  nimbleCode({
  
  #Priors
  Intercept  ~ dnorm(0, sd = 5)
  
  #lac_effect~ dt(mu=0, sigma=1, df=1)
  bm_effect ~ dt(mu=0, sigma=1, df=1)
 # dde_effect_m ~ dt(mu=0, sigma=1, df=1) 
  age_effect ~ dt(mu=0, sigma=1, df=1)
  date_effect~ dt(mu=0, sigma=1, df=1)
  c_m_effect~ dt(mu=0, sigma=1, df=1)
  #date_effect~ dnorm(0, sd = 5)
  lac_time_effect ~dt(mu=0, sigma=1, df=1)
  sex_effect ~ dt(mu=0, sigma=1, df=1)
  calf_ind_effect ~ dt(mu = 0, sigma = 1, df = 1) 
  time_1_preg_effect ~ dt(mu=0, sigma=1, df=1)
  # spei_3_effect ~ dt(mu = 0 , sigma = 1, df = 1)
 # spei_6_effect ~ dt(mu = 0 , sigma = 1, df = 1)
  #spei_12_effect ~ dt(mu = 0 , sigma = 1, df = 1)
  age2_effect ~ dt(mu = 0, sigma = 1, df = 1)
  lac_score_preg_effect ~ dt(mu = 0, sigma = 1, df = 1)
  age_bm_effect ~ dt(mu = 0, sigma = 1, df = 1)
  date_lac_effect ~ dt(mu = 0, sigma = 1, df = 1)
  #sex_lac_effect ~ dt(mu = 0, sigma = 1, df = 1)
  
  
  
  for(i in 1:n.obs){
    
    #Process Model
    logit(preg_prob[i])<-Intercept+
      #(lac_score[i]*lac_effect)+
      (est_bm[i]*bm_effect)+
     # (dde_m[i]*dde_effect_m)+
      (age[i]*age_effect)+
      (calf_ind[i] * calf_ind_effect) +
      (calf_mass[i]* c_m_effect)+
      (pow(age[i], 2)*age2_effect)+
      (calving_date[i] *date_effect )+
      # (sex_correct[i] * sex_effect) +
      (lac_time[i]* lac_time_effect)+
      (time_1_preg[i]* time_1_preg_effect)+
      # (spei_3[i]*spei_3_effect)+
      # (spei_6[i]*spei_6_effect)+
      (date_lac_effect *lac_time[i]*calving_date[i] )+
      #(spei_12[i]*spei_12_effect)#+
      (age[i]*est_bm[i]*age_bm_effect)
    
    
    
    
    
    
    #Observation Model (Likelihood)
    preg_status[i] ~ dbern(preg_prob[i])
    
    #Impute missing data values
    lac_score[i] ~ dnorm(mean=0,sd=1)
    # precip_year_prior[i] ~ dnorm(mean = 0, sd = 1)
    #dde_95[i] ~ dnorm(mean=0,sd=1)
    age[i] ~ dnorm(mean=0,sd=1)
    # est_body_cond[i] ~ dnorm(mean=0,sd=1)
    est_bm[i] ~ dnorm(mean=0,sd=1)
    
    #lac_time[i] ~ dnorm(mean=0,sd=1)##does this matrer
    calving_date[i] ~ dnorm(mean = 0, sd = 1)
    # spei_6[i] ~ dnorm(mean = 0, sd = 1)
    # spei_12[i] ~ dnorm(mean = 0, sd = 1)
    
    
    
  }#i
  
})#model5
parameters <- c("c_m_effect",
                "bm_effect",
                #"dde_effect_m",
                "age_effect",
                "date_effect",
                "age_bm_effect",
                "age2_effect",
                "calf_ind_effect",
                "date_lac_effect",
                # "precip_effect",
                "Intercept", 
                # "bod_con_effect",
                "lac_time_effect",
                # "sex_effect",
                "time_1_preg_effect",
                "logProb_preg_status"#,
                # "spei_3_effect",
                # "spei_6_effect"#,
                #"spei_12_effect"#,
                #"lac_score_preg_effect"#,
                ##"sex_lac_effect"
)

# MCMC Settings
ni <- 400000
nt <- 400
nb <- 200000
nc <- 3

nimble.data = list(
  calf_mass = as.numeric(cow_capture_all$mass_calf),
  preg_status = as.numeric(cow_capture_all$time_2_preg) - 1,
  lac_score = as.numeric((cow_capture_all$lac_score)),
  est_bm = as.numeric((cow_capture_all$est_bm)),
  calf_ind =  as.numeric(cow_capture_all$calf_ind),
  #dde_95 = as.numeric((cow_capture_all$area_DDE_gt_95)),
  #dde_75 = as.numeric((cow_capture_all$area_DDE_gt_75)),
  dde_m = as.numeric((cow_capture_all$area_DDE_gt_mean)),
  age = as.numeric((cow_capture_all$age_estimate_2)),
  #precip_year_prior = as.numeric((cow_capture_all$total_year_prior)),
  sex_correct = ifelse(cow_capture_all$sex_correct== "m", 1,0),
  lac_time= as.numeric(cow_capture_all$lac_time), 
  time_1_preg= as.numeric(cow_capture_all$time_1_preg)-1,#
  calving_date = as.numeric(cow_capture_all$birt_date),
  #spei_3 = as.numeric((cow_capture_all$sept_year_prior_3M)),
  spei_6 = as.numeric((cow_capture_all$sept_year_prior_6M))#,
  #spei_12 = as.numeric((cow_capture_all$sept_year_prior_12M))
  
)

nimble.constants = list(n.obs = nrow(cow_capture_all))


mcmc.output_half_add_a2_lactime_reduced_preg_lac_r<- nimbleMCMC(code = model_half_add_a2_lactime_reduced_preg_lac_r,
                                                                data = nimble.data,
                                                                constants=nimble.constants,
                                                                monitors = parameters,
                                                                niter = ni,
                                                                nburnin = nb,
                                                                nchains = nc,
                                                                thin=nt,
                                                                summary=TRUE,
                                                                samplesAsCodaMCMC = TRUE)

attach.nimble(mcmc.output_half_add_a2_lactime_reduced_preg_lac_r$samples)


mata_half  <- do.call(rbind, lapply(mcmc.output_half_add_a2_lactime_reduced_preg_lac_r$samples, as.matrix))
lpa_half  <- grep("^logProb_preg_status\\[", colnames(mata_half), value = TRUE)
lla_half  <- mata_half[, lpa_half, drop = FALSE]
stopifnot(all(is.finite(lla_half) | (is.infinite(lla_half) & lla_half < 0)))
loo_half_add_a2_lactime_preg_lac_r <- loo(lla_half) #top?



attach.nimble(mcmc.output_half_add_a2_lactime_reduced_preg_lac_r$samples)


# Gelman-Rubin diagnostic (AKA RHat or PSRF)
gelman.diag(mcmc.output_half_add_a2_lactime_reduced_preg_lac_r$samples, multivariate = FALSE)



mcmcplot(mcmc.output_half_add_a2_lactime_reduced_preg_lac_r$samples)



##how to get effects size
##jow to make the differnt charts in PP
##how to get time on deck chart
##uses zscore to preduct value. Use mean for all the other values
##will use for calf age ##what the fuck is a z-score?
post_mat <- as.matrix(mcmc.output_half_add_a2_lactime_reduced_preg_lac_r$samples)
# 2. Specify the parameters you care about
params <- c("c_m_effect",
            "bm_effect",
            #"dde_effect_m",
            "age_effect",
            "date_effect",
            "age_bm_effect",
            "age2_effect",
            #"sex_effect",
            "date_lac_effect",
            # "precip_effect",
            "Intercept", 
            # "bod_con_effect",
            "lac_time_effect",
            "calf_ind_effect",
            # "sex_effect",
            "time_1_preg_effect"#,
            
            # "spei_3_effect",
            # "spei_6_effect"#,
            
)

# 3. Summarize each one
summary_df_half_add_a2_lactime_preg_lac_r <- tibble(parameter = params) %>%
  rowwise() %>%
  mutate(
    median   = median(post_mat[, parameter]),
    lower95  = quantile(post_mat[, parameter], 0.025),
    upper95  = quantile(post_mat[, parameter], 0.975),
    # posterior probability effect > 0
    p_gt_zero = mean(post_mat[, parameter] > 0),
    # if you expect a negative effect, you can also compute P(<0):
    p_lt_zero = mean(post_mat[, parameter] < 0)
  ) %>%
  ungroup()

print(summary_df_half_add_a2_lactime_preg_lac_r)

#########################reduced globa
####################mean dde, spei_6
model_global_r<-  nimbleCode({
  
  #Priors
  Intercept  ~ dnorm(0, sd = 5)
  
  lac_effect~ dt(mu=0, sigma=1, df=1)
  bm_effect ~ dt(mu=0, sigma=1, df=1)
  dde_effect_m ~ dt(mu=0, sigma=1, df=1) 
  age_effect ~ dt(mu=0, sigma=1, df=1)
  date_effect~ dt(mu=0, sigma=1, df=1)
  c_m_effect~ dt(mu=0, sigma=1, df=1)
  #date_effect~ dnorm(0, sd = 5)
  lac_time_effect ~dt(mu=0, sigma=1, df=1)
  sex_effect ~ dt(mu=0, sigma=1, df=1)
  calf_ind_effect ~ dt(mu = 0, sigma = 1, df = 1) 
  time_1_preg_effect ~ dt(mu=0, sigma=1, df=1)
  # spei_3_effect ~ dt(mu = 0 , sigma = 1, df = 1)
  spei_6_effect ~ dt(mu = 0 , sigma = 1, df = 1)
  #spei_12_effect ~ dt(mu = 0 , sigma = 1, df = 1)
  age2_effect ~ dt(mu = 0, sigma = 1, df = 1)
  lac_score_preg_effect ~ dt(mu = 0, sigma = 1, df = 1)
  age_bm_effect ~ dt(mu = 0, sigma = 1, df = 1)
  date_lac_effect ~ dt(mu = 0, sigma = 1, df = 1)
  #sex_lac_effect ~ dt(mu = 0, sigma = 1, df = 1)
  
  
  
  for(i in 1:n.obs){
    
    #Process Model
    logit(preg_prob[i])<-Intercept+
      (lac_score[i]*lac_effect)+
      (est_bm[i]*bm_effect)+
      (dde_m[i]*dde_effect_m)+
      (age[i]*age_effect)+
      (calf_ind[i] * calf_ind_effect) +
      (calf_mass[i]* c_m_effect)+
      (pow(age[i], 2)*age2_effect)+
      (calving_date[i] *date_effect )+
     # (sex_correct[i] * sex_effect) +
      (lac_time[i]* lac_time_effect)+
      (time_1_preg[i]* time_1_preg_effect)+
      # (spei_3[i]*spei_3_effect)+
     # (spei_6[i]*spei_6_effect)+
      (date_lac_effect *lac_time[i]*calving_date[i] )+
      #(spei_12[i]*spei_12_effect)#+
      (age[i]*est_bm[i]*age_bm_effect)
    
    
    
    
    
    
    #Observation Model (Likelihood)
    preg_status[i] ~ dbern(preg_prob[i])
    
    #Impute missing data values
    lac_score[i] ~ dnorm(mean=0,sd=1)
    # precip_year_prior[i] ~ dnorm(mean = 0, sd = 1)
    #dde_95[i] ~ dnorm(mean=0,sd=1)
    age[i] ~ dnorm(mean=0,sd=1)
    # est_body_cond[i] ~ dnorm(mean=0,sd=1)
    est_bm[i] ~ dnorm(mean=0,sd=1)
    
    #lac_time[i] ~ dnorm(mean=0,sd=1)##does this matrer
    calving_date[i] ~ dnorm(mean = 0, sd = 1)
    spei_6[i] ~ dnorm(mean = 0, sd = 1)
    # spei_12[i] ~ dnorm(mean = 0, sd = 1)
    
    
    
  }#i
  
})#model5
parameters <- c("c_m_effect",
                "bm_effect",
                "dde_effect_m",
                "age_effect",
                "date_effect",
                "age_bm_effect",
                "age2_effect",
                "calf_ind_effect",
                "date_lac_effect",
                "lac_effect",
                # "precip_effect",
                "Intercept", 
                # "bod_con_effect",
                "lac_time_effect",
               # "sex_effect",
                "time_1_preg_effect",
                "logProb_preg_status"#,
                # "spei_3_effect",
                #"spei_6_effect"#,
                #"spei_12_effect"#,
                #"lac_score_preg_effect"#,
                ##"sex_lac_effect"
)

# MCMC Settings
ni <- 400000
nt <- 400
nb <- 200000
nc <- 3

nimble.data = list(
  calf_mass = as.numeric(cow_capture_all$mass_calf),
  preg_status = as.numeric(cow_capture_all$time_2_preg) - 1,
  lac_score = as.numeric((cow_capture_all$lac_score)),
  est_bm = as.numeric((cow_capture_all$est_bm)),
  calf_ind =  as.numeric(cow_capture_all$calf_ind),
  dde_m = as.numeric((cow_capture_all$area_DDE_gt_95)),
  #dde_m = as.numeric((cow_capture_all$area_DDE_gt_75)),
  #dde_m = as.numeric((cow_capture_all$area_DDE_gt_mean)),
  age = as.numeric((cow_capture_all$age_estimate_2)),
  #precip_year_prior = as.numeric((cow_capture_all$total_year_prior)),
  sex_correct = ifelse(cow_capture_all$sex_correct== "m", 1,0),
  lac_time= as.numeric(cow_capture_all$lac_time), 
  time_1_preg= as.numeric(cow_capture_all$time_1_preg)-1,#
  calving_date = as.numeric(cow_capture_all$birt_date),
  spei_3 = as.numeric((cow_capture_all$sept_year_prior_3M))#,
  #spei_6 = as.numeric((cow_capture_all$sept_year_prior_6M))#,
  #spei_6 = as.numeric((cow_capture_all$sept_year_prior_12M))
  
)

nimble.constants = list(n.obs = nrow(cow_capture_all))


mcmc.output_global_r<- nimbleMCMC(code = model_global_r,
                                data = nimble.data,
                                constants=nimble.constants,
                                monitors = parameters,
                                niter = ni,
                                nburnin = nb,
                                nchains = nc,
                                thin=nt,
                                summary=TRUE,
                                samplesAsCodaMCMC = TRUE)

attach.nimble(mcmc.output_global$samples)


mata_half  <- do.call(rbind, lapply(mcmc.output_global_r$samples, as.matrix))
lpa_half  <- grep("^logProb_preg_status\\[", colnames(mata_half), value = TRUE)
lla_half  <- mata_half[, lpa_half, drop = FALSE]
stopifnot(all(is.finite(lla_half) | (is.infinite(lla_half) & lla_half < 0)))
loo_global_95_re <- loo(lla_half)



attach.nimble(mcmc.output_global_r$samples)


# Gelman-Rubin diagnostic (AKA RHat or PSRF)
gelman.diag(mcmc.output_global_r$samples, multivariate = FALSE)



mcmcplot(mcmc.output_global_r$samples)



##how to get effects size
##jow to make the differnt charts in PP
##how to get time on deck chart
##uses zscore to preduct value. Use mean for all the other values
##will use for calf age ##what the fuck is a z-score?
post_mat <- as.matrix(mcmc.output_global_r$samples)
# 2. Specify the parameters you care about
params <- c("c_m_effect",
            "bm_effect",
            "dde_effect_m",
            "age_effect",
            "date_effect",
            "age_bm_effect",
            "age2_effect",
            "calf_ind_effect",
            "date_lac_effect",
            "lac_effect",
            # "precip_effect",
            "Intercept", 
            # "bod_con_effect",
            "lac_time_effect",
            #"sex_effect",
            "time_1_preg_effect"#,
            
            # "spei_3_effect",
            #"spei_6_effect"#,
            #"spei_12_effect"#,
            #"lac_score_preg_effect"#,
            ##"sex_lac_effect"
            
)

# 3. Summarize each one
summary_df_global_r <- tibble(parameter = params) %>%
  rowwise() %>%
  mutate(
    median   = median(post_mat[, parameter]),
    lower95  = quantile(post_mat[, parameter], 0.025),
    upper95  = quantile(post_mat[, parameter], 0.975),
    # posterior probability effect > 0
    p_gt_zero = mean(post_mat[, parameter] > 0),
    # if you expect a negative effect, you can also compute P(<0):
    p_lt_zero = mean(post_mat[, parameter] < 0)
  ) %>%
  ungroup()

print(summary_df_global_r)

beep(2)

###########################




#####################null
######################################## NULL MODEL (intercept only)
model_null <- nimbleCode({
  
  #Priors
  Intercept ~ dnorm(0, sd = 5)
  
  for(i in 1:n.obs){
    
    #Process Model
    logit(preg_prob[i]) <- Intercept
    
    #Observation Model (Likelihood)
    preg_status[i] ~ dbern(preg_prob[i])
    
  }#i
  
})

parameters <- c("Intercept",
                "logProb_preg_status")

# MCMC Settings
ni <- 400000
nt <- 400
nb <- 200000
nc <- 3

nimble.data <- list(
  preg_status = as.numeric(cow_capture_all$time_2_preg) - 1
)

nimble.constants <- list(n.obs = nrow(cow_capture_all))

mcmc.output_null <- nimbleMCMC(code = model_null,
                               data = nimble.data,
                               constants = nimble.constants,
                               inits = function() list(Intercept = 0),
                               monitors = parameters,
                               niter = ni,
                               nburnin = nb,
                               nchains = nc,
                               thin = nt,
                               summary = TRUE,
                               samplesAsCodaMCMC = TRUE)

mata_null <- do.call(rbind, lapply(mcmc.output_null$samples, as.matrix))
lpa_null  <- grep("^logProb_preg_status\\[", colnames(mata_null), value = TRUE)
lla_null  <- mata_null[, lpa_null, drop = FALSE]
stopifnot(all(is.finite(lla_null)))
loo_null <- loo(lla_null)

##########################reduced 3
model_half_add_a2_lactime_reduced_preg_lac_r3 <-  nimbleCode({
  
  #Priors
  Intercept  ~ dnorm(0, sd = 5)
  
  #lac_effect~ dt(mu=0, sigma=1, df=1)
  bm_effect ~ dt(mu=0, sigma=1, df=1)
  # dde_effect_m ~ dt(mu=0, sigma=1, df=1) 
  age_effect ~ dt(mu=0, sigma=1, df=1)
  date_effect~ dt(mu=0, sigma=1, df=1)
  c_m_effect~ dt(mu=0, sigma=1, df=1)
  #date_effect~ dnorm(0, sd = 5)
  lac_time_effect ~dt(mu=0, sigma=1, df=1)
  sex_effect ~ dt(mu=0, sigma=1, df=1)
  calf_ind_effect ~ dt(mu = 0, sigma = 1, df = 1) 
  time_1_preg_effect ~ dt(mu=0, sigma=1, df=1)
  # spei_3_effect ~ dt(mu = 0 , sigma = 1, df = 1)
  # spei_6_effect ~ dt(mu = 0 , sigma = 1, df = 1)
  #spei_12_effect ~ dt(mu = 0 , sigma = 1, df = 1)
  age2_effect ~ dt(mu = 0, sigma = 1, df = 1)
  lac_score_preg_effect ~ dt(mu = 0, sigma = 1, df = 1)
  age_bm_effect ~ dt(mu = 0, sigma = 1, df = 1)
  date_lac_effect ~ dt(mu = 0, sigma = 1, df = 1)
  #sex_lac_effect ~ dt(mu = 0, sigma = 1, df = 1)
  
  
  
  for(i in 1:n.obs){
    
    #Process Model
    logit(preg_prob[i])<-Intercept+
      #(lac_score[i]*lac_effect)+
      (est_bm[i]*bm_effect)+
      # (dde_m[i]*dde_effect_m)+
      (age[i]*age_effect)+
      (calf_ind[i] * calf_ind_effect) +
      #(calf_mass[i]* c_m_effect)+
      (pow(age[i], 2)*age2_effect)+
      (calving_date[i] *date_effect )+
      # (sex_correct[i] * sex_effect) +
      (lac_time[i]* lac_time_effect)+
      (time_1_preg[i]* time_1_preg_effect)+
      # (spei_3[i]*spei_3_effect)+
      # (spei_6[i]*spei_6_effect)+
      (date_lac_effect *lac_time[i]*calving_date[i] )+
      #(spei_12[i]*spei_12_effect)#+
      (age[i]*est_bm[i]*age_bm_effect)
    
    
    
    
    
    
    #Observation Model (Likelihood)
    preg_status[i] ~ dbern(preg_prob[i])
    
    #Impute missing data values
    lac_score[i] ~ dnorm(mean=0,sd=1)
    # precip_year_prior[i] ~ dnorm(mean = 0, sd = 1)
    #dde_95[i] ~ dnorm(mean=0,sd=1)
    age[i] ~ dnorm(mean=0,sd=1)
    # est_body_cond[i] ~ dnorm(mean=0,sd=1)
    est_bm[i] ~ dnorm(mean=0,sd=1)
    
    #lac_time[i] ~ dnorm(mean=0,sd=1)##does this matrer
    calving_date[i] ~ dnorm(mean = 0, sd = 1)
    # spei_6[i] ~ dnorm(mean = 0, sd = 1)
    # spei_12[i] ~ dnorm(mean = 0, sd = 1)
    
    
    
  }#i
  
})#model5
parameters <- c(#"c_m_effect",
                "bm_effect",
                #"dde_effect_m",
                "age_effect",
                "date_effect",
                "age_bm_effect",
                "age2_effect",
                "calf_ind_effect",
                "date_lac_effect",
                # "precip_effect",
                "Intercept", 
                # "bod_con_effect",
                "lac_time_effect",
                # "sex_effect",
                "time_1_preg_effect",
                "logProb_preg_status"#,
                # "spei_3_effect",
                # "spei_6_effect"#,
                #"spei_12_effect"#,
                #"lac_score_preg_effect"#,
                ##"sex_lac_effect"
)

# MCMC Settings
ni <- 400000
nt <- 400
nb <- 200000
nc <- 3

nimble.data = list(
  #calf_mass = as.numeric(cow_capture_all$mass_calf),
  preg_status = as.numeric(cow_capture_all$time_2_preg) - 1,
  lac_score = as.numeric((cow_capture_all$lac_score)),
  est_bm = as.numeric((cow_capture_all$est_bm)),
  calf_ind =  as.numeric(cow_capture_all$calf_ind),
  #dde_95 = as.numeric((cow_capture_all$area_DDE_gt_95)),
  #dde_75 = as.numeric((cow_capture_all$area_DDE_gt_75)),
  dde_m = as.numeric((cow_capture_all$area_DDE_gt_mean)),
  age = as.numeric((cow_capture_all$age_estimate_2)),
  #precip_year_prior = as.numeric((cow_capture_all$total_year_prior)),
  sex_correct = ifelse(cow_capture_all$sex_correct== "m", 1,0),
  lac_time= as.numeric(cow_capture_all$lac_time), 
  time_1_preg= as.numeric(cow_capture_all$time_1_preg)-1,#
  calving_date = as.numeric(cow_capture_all$birt_date),
  #spei_3 = as.numeric((cow_capture_all$sept_year_prior_3M)),
  spei_6 = as.numeric((cow_capture_all$sept_year_prior_6M))#,
  #spei_12 = as.numeric((cow_capture_all$sept_year_prior_12M))
  
)

nimble.constants = list(n.obs = nrow(cow_capture_all))


mcmc.output_half_add_a2_lactime_reduced_preg_lac_r3<- nimbleMCMC(code = model_half_add_a2_lactime_reduced_preg_lac_r3,
                                                                data = nimble.data,
                                                                constants=nimble.constants,
                                                                monitors = parameters,
                                                                niter = ni,
                                                                nburnin = nb,
                                                                nchains = nc,
                                                                thin=nt,
                                                                summary=TRUE,
                                                                samplesAsCodaMCMC = TRUE)

attach.nimble(mcmc.output_half_add_a2_lactime_reduced_preg_lac_r$samples)


mata_half  <- do.call(rbind, lapply(mcmc.output_half_add_a2_lactime_reduced_preg_lac_r3$samples, as.matrix))
lpa_half  <- grep("^logProb_preg_status\\[", colnames(mata_half), value = TRUE)
lla_half  <- mata_half[, lpa_half, drop = FALSE]
stopifnot(all(is.finite(lla_half) | (is.infinite(lla_half) & lla_half < 0)))
loo_half_add_a2_lactime_preg_lac_r3 <- loo(lla_half) 



attach.nimble(mcmc.output_half_add_a2_lactime_reduced_preg_lac_r3$samples)


# Gelman-Rubin diagnostic (AKA RHat or PSRF)
gelman.diag(mcmc.output_half_add_a2_lactime_reduced_preg_lac_r3$samples, multivariate = FALSE)



mcmcplot(mcmc.output_half_add_a2_lactime_reduced_preg_lac_r3$samples)



##how to get effects size
##jow to make the differnt charts in PP
##how to get time on deck chart
##uses zscore to preduct value. Use mean for all the other values
##will use for calf age ##what the fuck is a z-score?
post_mat <- as.matrix(mcmc.output_half_add_a2_lactime_reduced_preg_lac_r$samples)
# 2. Specify the parameters you care about
params <- c(#"c_m_effect",
            "bm_effect",
            #"dde_effect_m",
            "age_effect",
            "date_effect",
            "age_bm_effect",
            "age2_effect",
            #"sex_effect",
            "date_lac_effect",
            # "precip_effect",
            "Intercept", 
            # "bod_con_effect",
            "lac_time_effect",
            "calf_ind_effect",
            # "sex_effect",
            "time_1_preg_effect"#,
            
            # "spei_3_effect",
            # "spei_6_effect"#,
            
)

# 3. Summarize each one
summary_df_half_add_a2_lactime_preg_lac_r3 <- tibble(parameter = params) %>%
  rowwise() %>%
  mutate(
    median   = median(post_mat[, parameter]),
    lower95  = quantile(post_mat[, parameter], 0.025),
    upper95  = quantile(post_mat[, parameter], 0.975),
    # posterior probability effect > 0
    p_gt_zero = mean(post_mat[, parameter] > 0),
    # if you expect a negative effect, you can also compute P(<0):
    p_lt_zero = mean(post_mat[, parameter] < 0)
  ) %>%
  ungroup()

print(summary_df_half_add_a2_lactime_preg_lac_r3)


#######################################
##########################reduced 4
model_half_add_a2_lactime_reduced_preg_lac_r4 <-  nimbleCode({
  
  #Priors
  Intercept  ~ dnorm(0, sd = 5)
  
  #lac_effect~ dt(mu=0, sigma=1, df=1)
  bm_effect ~ dt(mu=0, sigma=1, df=1)
  # dde_effect_m ~ dt(mu=0, sigma=1, df=1) 
  age_effect ~ dt(mu=0, sigma=1, df=1)
  date_effect~ dt(mu=0, sigma=1, df=1)
  c_m_effect~ dt(mu=0, sigma=1, df=1)
  #date_effect~ dnorm(0, sd = 5)
  lac_time_effect ~dt(mu=0, sigma=1, df=1)
  sex_effect ~ dt(mu=0, sigma=1, df=1)
  calf_ind_effect ~ dt(mu = 0, sigma = 1, df = 1) 
  time_1_preg_effect ~ dt(mu=0, sigma=1, df=1)
  # spei_3_effect ~ dt(mu = 0 , sigma = 1, df = 1)
  # spei_6_effect ~ dt(mu = 0 , sigma = 1, df = 1)
  #spei_12_effect ~ dt(mu = 0 , sigma = 1, df = 1)
  age2_effect ~ dt(mu = 0, sigma = 1, df = 1)
  lac_score_preg_effect ~ dt(mu = 0, sigma = 1, df = 1)
  age_bm_effect ~ dt(mu = 0, sigma = 1, df = 1)
  date_lac_effect ~ dt(mu = 0, sigma = 1, df = 1)
  #sex_lac_effect ~ dt(mu = 0, sigma = 1, df = 1)
  
  
  
  for(i in 1:n.obs){
    
    #Process Model
    logit(preg_prob[i])<-Intercept+
      #(lac_score[i]*lac_effect)+
      (est_bm[i]*bm_effect)+
      # (dde_m[i]*dde_effect_m)+
      (age[i]*age_effect)+
      (calf_ind[i] * calf_ind_effect) +
      #(calf_mass[i]* c_m_effect)+
      (pow(age[i], 2)*age2_effect)+
      (calving_date[i] *date_effect )+
      # (sex_correct[i] * sex_effect) +
      (lac_time[i]* lac_time_effect)+
     # (time_1_preg[i]* time_1_preg_effect)+
      # (spei_3[i]*spei_3_effect)+
      # (spei_6[i]*spei_6_effect)+
      (date_lac_effect *lac_time[i]*calving_date[i] )+
      #(spei_12[i]*spei_12_effect)#+
      (age[i]*est_bm[i]*age_bm_effect)
    
    
    
    
    
    
    #Observation Model (Likelihood)
    preg_status[i] ~ dbern(preg_prob[i])
    
    #Impute missing data values
    lac_score[i] ~ dnorm(mean=0,sd=1)
    # precip_year_prior[i] ~ dnorm(mean = 0, sd = 1)
    #dde_95[i] ~ dnorm(mean=0,sd=1)
    age[i] ~ dnorm(mean=0,sd=1)
    # est_body_cond[i] ~ dnorm(mean=0,sd=1)
    est_bm[i] ~ dnorm(mean=0,sd=1)
    
    #lac_time[i] ~ dnorm(mean=0,sd=1)##does this matrer
    calving_date[i] ~ dnorm(mean = 0, sd = 1)
    # spei_6[i] ~ dnorm(mean = 0, sd = 1)
    # spei_12[i] ~ dnorm(mean = 0, sd = 1)
    
    
    
  }#i
  
})#model5
parameters <- c(#"c_m_effect",
  "bm_effect",
  #"dde_effect_m",
  "age_effect",
  "date_effect",
  "age_bm_effect",
  "age2_effect",
  "calf_ind_effect",
  "date_lac_effect",
  # "precip_effect",
  "Intercept", 
  # "bod_con_effect",
  "lac_time_effect",
  # "sex_effect",
 # "time_1_preg_effect",
  "logProb_preg_status"#,
  # "spei_3_effect",
  # "spei_6_effect"#,
  #"spei_12_effect"#,
  #"lac_score_preg_effect"#,
  ##"sex_lac_effect"
)

# MCMC Settings
ni <- 400000
nt <- 400
nb <- 200000
nc <- 3

nimble.data = list(
  #calf_mass = as.numeric(cow_capture_all$mass_calf),
  preg_status = as.numeric(cow_capture_all$time_2_preg) - 1,
  lac_score = as.numeric((cow_capture_all$lac_score)),
  est_bm = as.numeric((cow_capture_all$est_bm)),
  calf_ind =  as.numeric(cow_capture_all$calf_ind),
  #dde_95 = as.numeric((cow_capture_all$area_DDE_gt_95)),
  #dde_75 = as.numeric((cow_capture_all$area_DDE_gt_75)),
  dde_m = as.numeric((cow_capture_all$area_DDE_gt_mean)),
  age = as.numeric((cow_capture_all$age_estimate_2)),
  #precip_year_prior = as.numeric((cow_capture_all$total_year_prior)),
  sex_correct = ifelse(cow_capture_all$sex_correct== "m", 1,0),
  lac_time= as.numeric(cow_capture_all$lac_time), 
  time_1_preg= as.numeric(cow_capture_all$time_1_preg)-1,#
  calving_date = as.numeric(cow_capture_all$birt_date),
  #spei_3 = as.numeric((cow_capture_all$sept_year_prior_3M)),
  spei_6 = as.numeric((cow_capture_all$sept_year_prior_6M))#,
  #spei_12 = as.numeric((cow_capture_all$sept_year_prior_12M))
  
)

nimble.constants = list(n.obs = nrow(cow_capture_all))


mcmc.output_half_add_a2_lactime_reduced_preg_lac_r4<- nimbleMCMC(code = model_half_add_a2_lactime_reduced_preg_lac_r4,
                                                                 data = nimble.data,
                                                                 constants=nimble.constants,
                                                                 monitors = parameters,
                                                                 niter = ni,
                                                                 nburnin = nb,
                                                                 nchains = nc,
                                                                 thin=nt,
                                                                 summary=TRUE,
                                                                 samplesAsCodaMCMC = TRUE)

attach.nimble(mcmc.output_half_add_a2_lactime_reduced_preg_lac_r4$samples)


mata_half  <- do.call(rbind, lapply(mcmc.output_half_add_a2_lactime_reduced_preg_lac_r4$samples, as.matrix))
lpa_half  <- grep("^logProb_preg_status\\[", colnames(mata_half), value = TRUE)
lla_half  <- mata_half[, lpa_half, drop = FALSE]
stopifnot(all(is.finite(lla_half) | (is.infinite(lla_half) & lla_half < 0)))
loo_half_add_a2_lactime_preg_lac_r4 <- loo(lla_half) 



attach.nimble(mcmc.output_half_add_a2_lactime_reduced_preg_lac_r4$samples)


# Gelman-Rubin diagnostic (AKA RHat or PSRF)
gelman.diag(mcmc.output_half_add_a2_lactime_reduced_preg_lac_r4$samples, multivariate = FALSE)



mcmcplot(mcmc.output_half_add_a2_lactime_reduced_preg_lac_r4$samples)



##how to get effects size
##jow to make the differnt charts in PP
##how to get time on deck chart
##uses zscore to preduct value. Use mean for all the other values
##will use for calf age ##what the fuck is a z-score?
post_mat <- as.matrix(mcmc.output_half_add_a2_lactime_reduced_preg_lac_r4$samples)
# 2. Specify the parameters you care about
params <- c(#"c_m_effect",
  "bm_effect",
  #"dde_effect_m",
  "age_effect",
  "date_effect",
  "age_bm_effect",
  "age2_effect",
  #"sex_effect",
  "date_lac_effect",
  # "precip_effect",
  "Intercept", 
  # "bod_con_effect",
  "lac_time_effect",
  "calf_ind_effect"#,
  # "sex_effect",
  #"time_1_preg_effect"#,
  
  # "spei_3_effect",
  # "spei_6_effect"#,
  
)

# 3. Summarize each one
summary_df_half_add_a2_lactime_preg_lac_r4 <- tibble(parameter = params) %>%
  rowwise() %>%
  mutate(
    median   = median(post_mat[, parameter]),
    lower95  = quantile(post_mat[, parameter], 0.025),
    upper95  = quantile(post_mat[, parameter], 0.975),
    # posterior probability effect > 0
    p_gt_zero = mean(post_mat[, parameter] > 0),
    # if you expect a negative effect, you can also compute P(<0):
    p_lt_zero = mean(post_mat[, parameter] < 0)
  ) %>%
  ungroup()

print(summary_df_half_add_a2_lactime_preg_lac_r4)


#######################################add dde
##########################reduced 4
model_half_add_a2_lactime_reduced_preg_lac_r4dde <-  nimbleCode({
  
  #Priors
  Intercept  ~ dnorm(0, sd = 5)
  
  #lac_effect~ dt(mu=0, sigma=1, df=1)
  bm_effect ~ dt(mu=0, sigma=1, df=1)
   dde_effect_m ~ dt(mu=0, sigma=1, df=1) 
  age_effect ~ dt(mu=0, sigma=1, df=1)
  date_effect~ dt(mu=0, sigma=1, df=1)
  c_m_effect~ dt(mu=0, sigma=1, df=1)
  #date_effect~ dnorm(0, sd = 5)
  lac_time_effect ~dt(mu=0, sigma=1, df=1)
  sex_effect ~ dt(mu=0, sigma=1, df=1)
  calf_ind_effect ~ dt(mu = 0, sigma = 1, df = 1) 
  time_1_preg_effect ~ dt(mu=0, sigma=1, df=1)
  # spei_3_effect ~ dt(mu = 0 , sigma = 1, df = 1)
  # spei_6_effect ~ dt(mu = 0 , sigma = 1, df = 1)
  #spei_12_effect ~ dt(mu = 0 , sigma = 1, df = 1)
  age2_effect ~ dt(mu = 0, sigma = 1, df = 1)
  lac_score_preg_effect ~ dt(mu = 0, sigma = 1, df = 1)
  age_bm_effect ~ dt(mu = 0, sigma = 1, df = 1)
  date_lac_effect ~ dt(mu = 0, sigma = 1, df = 1)
  #sex_lac_effect ~ dt(mu = 0, sigma = 1, df = 1)
  
  
  
  for(i in 1:n.obs){
    
    #Process Model
    logit(preg_prob[i])<-Intercept+
      #(lac_score[i]*lac_effect)+
      (est_bm[i]*bm_effect)+
       (dde_m[i]*dde_effect_m)+
      (age[i]*age_effect)+
      (calf_ind[i] * calf_ind_effect) +
      #(calf_mass[i]* c_m_effect)+
      (pow(age[i], 2)*age2_effect)+
      (calving_date[i] *date_effect )+
      # (sex_correct[i] * sex_effect) +
      (lac_time[i]* lac_time_effect)+
      # (time_1_preg[i]* time_1_preg_effect)+
      # (spei_3[i]*spei_3_effect)+
      # (spei_6[i]*spei_6_effect)+
      (date_lac_effect *lac_time[i]*calving_date[i] )+
      #(spei_12[i]*spei_12_effect)#+
      (age[i]*est_bm[i]*age_bm_effect)
    
    
    
    
    
    
    #Observation Model (Likelihood)
    preg_status[i] ~ dbern(preg_prob[i])
    
    #Impute missing data values
    lac_score[i] ~ dnorm(mean=0,sd=1)
    # precip_year_prior[i] ~ dnorm(mean = 0, sd = 1)
    dde_m[i] ~ dnorm(mean=0,sd=1)
    age[i] ~ dnorm(mean=0,sd=1)
    # est_body_cond[i] ~ dnorm(mean=0,sd=1)
    est_bm[i] ~ dnorm(mean=0,sd=1)
    
    #lac_time[i] ~ dnorm(mean=0,sd=1)##does this matrer
    calving_date[i] ~ dnorm(mean = 0, sd = 1)
    # spei_6[i] ~ dnorm(mean = 0, sd = 1)
    # spei_12[i] ~ dnorm(mean = 0, sd = 1)
    
    
    
  }#i
  
})#model5
parameters <- c(#"c_m_effect",
  "bm_effect",
  "dde_effect_m",
  "age_effect",
  "date_effect",
  "age_bm_effect",
  "age2_effect",
  "calf_ind_effect",
  "date_lac_effect",
  # "precip_effect",
  "Intercept", 
  # "bod_con_effect",
  "lac_time_effect",
  # "sex_effect",
  # "time_1_preg_effect",
  "logProb_preg_status"#,
  # "spei_3_effect",
  # "spei_6_effect"#,
  #"spei_12_effect"#,
  #"lac_score_preg_effect"#,
  ##"sex_lac_effect"
)

# MCMC Settings
ni <- 400000
nt <- 400
nb <- 200000
nc <- 3

nimble.data = list(
  #calf_mass = as.numeric(cow_capture_all$mass_calf),
  preg_status = as.numeric(cow_capture_all$time_2_preg) - 1,
  lac_score = as.numeric((cow_capture_all$lac_score)),
  est_bm = as.numeric((cow_capture_all$est_bm)),
  calf_ind =  as.numeric(cow_capture_all$calf_ind),
  #dde_95 = as.numeric((cow_capture_all$area_DDE_gt_95)),
  #dde_75 = as.numeric((cow_capture_all$area_DDE_gt_75)),
  dde_m = as.numeric((cow_capture_all$area_DDE_gt_mean)),
  age = as.numeric((cow_capture_all$age_estimate_2)),
  #precip_year_prior = as.numeric((cow_capture_all$total_year_prior)),
  sex_correct = ifelse(cow_capture_all$sex_correct== "m", 1,0),
  lac_time= as.numeric(cow_capture_all$lac_time), 
  time_1_preg= as.numeric(cow_capture_all$time_1_preg)-1,#
  calving_date = as.numeric(cow_capture_all$birt_date),
  #spei_3 = as.numeric((cow_capture_all$sept_year_prior_3M)),
  spei_6 = as.numeric((cow_capture_all$sept_year_prior_6M))#,
  #spei_12 = as.numeric((cow_capture_all$sept_year_prior_12M))
  
)

nimble.constants = list(n.obs = nrow(cow_capture_all))


mcmc.output_half_add_a2_lactime_reduced_preg_lac_r4dde<- nimbleMCMC(code = model_half_add_a2_lactime_reduced_preg_lac_r4dde,
                                                                 data = nimble.data,
                                                                 constants=nimble.constants,
                                                                 monitors = parameters,
                                                                 niter = ni,
                                                                 nburnin = nb,
                                                                 nchains = nc,
                                                                 thin=nt,
                                                                 summary=TRUE,
                                                                 samplesAsCodaMCMC = TRUE)

attach.nimble(mcmc.output_half_add_a2_lactime_reduced_preg_lac_r4dde$samples)


mata_half  <- do.call(rbind, lapply(mcmc.output_half_add_a2_lactime_reduced_preg_lac_r4dde$samples, as.matrix))
lpa_half  <- grep("^logProb_preg_status\\[", colnames(mata_half), value = TRUE)
lla_half  <- mata_half[, lpa_half, drop = FALSE]
stopifnot(all(is.finite(lla_half) | (is.infinite(lla_half) & lla_half < 0)))
loo_half_add_a2_lactime_preg_lac_r4dde <- loo(lla_half) 



attach.nimble(mcmc.output_half_add_a2_lactime_reduced_preg_lac_r4dde$samples)


# Gelman-Rubin diagnostic (AKA RHat or PSRF)
gelman.diag(mcmc.output_half_add_a2_lactime_reduced_preg_lac_r4dde$samples, multivariate = FALSE)



mcmcplot(mcmc.output_half_add_a2_lactime_reduced_preg_lac_r4dde$samples)



##how to get effects size
##jow to make the differnt charts in PP
##how to get time on deck chart
##uses zscore to preduct value. Use mean for all the other values
##will use for calf age ##what the fuck is a z-score?
post_mat <- as.matrix(mcmc.output_half_add_a2_lactime_reduced_preg_lac_r4dde$samples)
# 2. Specify the parameters you care about
params <- c(#"c_m_effect",
  "bm_effect",
  "dde_effect_m",
  "age_effect",
  "date_effect",
  "age_bm_effect",
  "age2_effect",
  #"sex_effect",
  "date_lac_effect",
  # "precip_effect",
  "Intercept", 
  # "bod_con_effect",
  "lac_time_effect",
  "calf_ind_effect"#,
  # "sex_effect",
  #"time_1_preg_effect"#,
  
  # "spei_3_effect",
  # "spei_6_effect"#,
  
)

# 3. Summarize each one
summary_df_half_add_a2_lactime_preg_lac_r4dde <- tibble(parameter = params) %>%
  rowwise() %>%
  mutate(
    median   = median(post_mat[, parameter]),
    lower95  = quantile(post_mat[, parameter], 0.025),
    upper95  = quantile(post_mat[, parameter], 0.975),
    # posterior probability effect > 0
    p_gt_zero = mean(post_mat[, parameter] > 0),
    # if you expect a negative effect, you can also compute P(<0):
    p_lt_zero = mean(post_mat[, parameter] < 0)
  ) %>%
  ungroup()

print(summary_df_half_add_a2_lactime_preg_lac_r4dde)
###################################################
#######################################add dde
##########################reduced 4
model_half_add_a2_lactime_reduced_preg_lac_r5 <-  nimbleCode({
  
  #Priors
  Intercept  ~ dnorm(0, sd = 5)
  
  #lac_effect~ dt(mu=0, sigma=1, df=1)
  bm_effect ~ dt(mu=0, sigma=1, df=1)
  dde_effect_m ~ dt(mu=0, sigma=1, df=1) 
  age_effect ~ dt(mu=0, sigma=1, df=1)
  date_effect~ dt(mu=0, sigma=1, df=1)
  c_m_effect~ dt(mu=0, sigma=1, df=1)
  #date_effect~ dnorm(0, sd = 5)
  lac_time_effect ~dt(mu=0, sigma=1, df=1)
  sex_effect ~ dt(mu=0, sigma=1, df=1)
  calf_ind_effect ~ dt(mu = 0, sigma = 1, df = 1) 
  time_1_preg_effect ~ dt(mu=0, sigma=1, df=1)
  # spei_3_effect ~ dt(mu = 0 , sigma = 1, df = 1)
  # spei_6_effect ~ dt(mu = 0 , sigma = 1, df = 1)
  #spei_12_effect ~ dt(mu = 0 , sigma = 1, df = 1)
  age2_effect ~ dt(mu = 0, sigma = 1, df = 1)
  lac_score_preg_effect ~ dt(mu = 0, sigma = 1, df = 1)
  age_bm_effect ~ dt(mu = 0, sigma = 1, df = 1)
  date_lac_effect ~ dt(mu = 0, sigma = 1, df = 1)
  #sex_lac_effect ~ dt(mu = 0, sigma = 1, df = 1)
  
  
  
  for(i in 1:n.obs){
    
    #Process Model
    logit(preg_prob[i])<-Intercept+
      #(lac_score[i]*lac_effect)+
      #(est_bm[i]*bm_effect)+
     # (dde_m[i]*dde_effect_m)+
     # (age[i]*age_effect)+
      (calf_ind[i] * calf_ind_effect) +
      #(calf_mass[i]* c_m_effect)+
      #(pow(age[i], 2)*age2_effect)+
      (calving_date[i] *date_effect )+
      # (sex_correct[i] * sex_effect) +
      (lac_time[i]* lac_time_effect)+
      # (time_1_preg[i]* time_1_preg_effect)+
      # (spei_3[i]*spei_3_effect)+
      # (spei_6[i]*spei_6_effect)+
      (date_lac_effect *lac_time[i]*calving_date[i] )#+
      #(spei_12[i]*spei_12_effect)#+
     # (age[i]*est_bm[i]*age_bm_effect)
    
    
    
    
    
    
    #Observation Model (Likelihood)
    preg_status[i] ~ dbern(preg_prob[i])
    
    #Impute missing data values
    lac_score[i] ~ dnorm(mean=0,sd=1)
    # precip_year_prior[i] ~ dnorm(mean = 0, sd = 1)
    dde_m[i] ~ dnorm(mean=0,sd=1)
    age[i] ~ dnorm(mean=0,sd=1)
    # est_body_cond[i] ~ dnorm(mean=0,sd=1)
    est_bm[i] ~ dnorm(mean=0,sd=1)
    
    #lac_time[i] ~ dnorm(mean=0,sd=1)##does this matrer
    calving_date[i] ~ dnorm(mean = 0, sd = 1)
    # spei_6[i] ~ dnorm(mean = 0, sd = 1)
    # spei_12[i] ~ dnorm(mean = 0, sd = 1)
    
    
    
  }#i
  
})#model5
parameters <- c(#"c_m_effect",
  #"bm_effect",
  #"dde_effect_m",
  #"age_effect",
  "date_effect",
  #"age_bm_effect",
 # "age2_effect",
  "calf_ind_effect",
  "date_lac_effect",
  # "precip_effect",
  "Intercept", 
  # "bod_con_effect",
  "lac_time_effect",
  # "sex_effect",
  # "time_1_preg_effect",
  "logProb_preg_status"#,
  # "spei_3_effect",
  # "spei_6_effect"#,
  #"spei_12_effect"#,
  #"lac_score_preg_effect"#,
  ##"sex_lac_effect"
)

# MCMC Settings
ni <- 400000
nt <- 400
nb <- 200000
nc <- 3

nimble.data = list(
  #calf_mass = as.numeric(cow_capture_all$mass_calf),
  preg_status = as.numeric(cow_capture_all$time_2_preg) - 1,
  lac_score = as.numeric((cow_capture_all$lac_score)),
  est_bm = as.numeric((cow_capture_all$est_bm)),
  calf_ind =  as.numeric(cow_capture_all$calf_ind),
  #dde_95 = as.numeric((cow_capture_all$area_DDE_gt_95)),
  #dde_75 = as.numeric((cow_capture_all$area_DDE_gt_75)),
  dde_m = as.numeric((cow_capture_all$area_DDE_gt_mean)),
  age = as.numeric((cow_capture_all$age_estimate_2)),
  #precip_year_prior = as.numeric((cow_capture_all$total_year_prior)),
  sex_correct = ifelse(cow_capture_all$sex_correct== "m", 1,0),
  lac_time= as.numeric(cow_capture_all$lac_time), 
  time_1_preg= as.numeric(cow_capture_all$time_1_preg)-1,#
  calving_date = as.numeric(cow_capture_all$birt_date),
  #spei_3 = as.numeric((cow_capture_all$sept_year_prior_3M)),
  spei_6 = as.numeric((cow_capture_all$sept_year_prior_6M))#,
  #spei_12 = as.numeric((cow_capture_all$sept_year_prior_12M))
  
)

nimble.constants = list(n.obs = nrow(cow_capture_all))


mcmc.output_half_add_a2_lactime_reduced_preg_lac_r5<- nimbleMCMC(code = model_half_add_a2_lactime_reduced_preg_lac_r5,
                                                                    data = nimble.data,
                                                                    constants=nimble.constants,
                                                                    monitors = parameters,
                                                                    niter = ni,
                                                                    nburnin = nb,
                                                                    nchains = nc,
                                                                    thin=nt,
                                                                    summary=TRUE,
                                                                    samplesAsCodaMCMC = TRUE)

attach.nimble(mcmc.output_half_add_a2_lactime_reduced_preg_lac_r4dde$samples)


mata_half  <- do.call(rbind, lapply(mcmc.output_half_add_a2_lactime_reduced_preg_lac_r5$samples, as.matrix))
lpa_half  <- grep("^logProb_preg_status\\[", colnames(mata_half), value = TRUE)
lla_half  <- mata_half[, lpa_half, drop = FALSE]
stopifnot(all(is.finite(lla_half) | (is.infinite(lla_half) & lla_half < 0)))
loo_half_add_a2_lactime_preg_lac_r5 <- loo(lla_half) 



attach.nimble(mcmc.output_half_add_a2_lactime_reduced_preg_lac_r5$samples)


# Gelman-Rubin diagnostic (AKA RHat or PSRF)
gelman.diag(mcmc.output_half_add_a2_lactime_reduced_preg_lac_r5$samples, multivariate = FALSE)



mcmcplot(mcmc.output_half_add_a2_lactime_reduced_preg_lac_r5$samples)



##how to get effects size
##jow to make the differnt charts in PP
##how to get time on deck chart
##uses zscore to preduct value. Use mean for all the other values
##will use for calf age ##what the fuck is a z-score?
post_mat <- as.matrix(mcmc.output_half_add_a2_lactime_reduced_preg_lac_r5$samples)
# 2. Specify the parameters you care about
params <- c(#"c_m_effect",
  #"bm_effect",
  #"dde_effect_m",
  #"age_effect",
  "date_effect",
  #"age_bm_effect",
 # "age2_effect",
  #"sex_effect",
  "date_lac_effect",
  # "precip_effect",
  "Intercept", 
  # "bod_con_effect",
  "lac_time_effect",
  "calf_ind_effect"#,
  # "sex_effect",
  #"time_1_preg_effect"#,
  
  # "spei_3_effect",
  # "spei_6_effect"#,
  
)

# 3. Summarize each one
summary_mcmc.output_half_add_a2_lactime_reduced_preg_lac_r5<- tibble(parameter = params) %>%
  rowwise() %>%
  mutate(
    median   = median(post_mat[, parameter]),
    lower95  = quantile(post_mat[, parameter], 0.025),
    upper95  = quantile(post_mat[, parameter], 0.975),
    # posterior probability effect > 0
    p_gt_zero = mean(post_mat[, parameter] > 0),
    # if you expect a negative effect, you can also compute P(<0):
    p_lt_zero = mean(post_mat[, parameter] < 0)
  ) %>%
  ungroup()

print(summary_mcmc.output_half_add_a2_lactime_reduced_preg_lac_r5)
################################################


model_half_add_a2_lactime_reduced_preg_lac_r5_age <-  nimbleCode({
  
  #Priors
  Intercept  ~ dnorm(0, sd = 5)
  
  #lac_effect~ dt(mu=0, sigma=1, df=1)
  bm_effect ~ dt(mu=0, sigma=1, df=1)
  dde_effect_m ~ dt(mu=0, sigma=1, df=1) 
  age_effect ~ dt(mu=0, sigma=1, df=1)
  date_effect~ dt(mu=0, sigma=1, df=1)
  c_m_effect~ dt(mu=0, sigma=1, df=1)
  #date_effect~ dnorm(0, sd = 5)
  lac_time_effect ~dt(mu=0, sigma=1, df=1)
  sex_effect ~ dt(mu=0, sigma=1, df=1)
  calf_ind_effect ~ dt(mu = 0, sigma = 1, df = 1) 
  time_1_preg_effect ~ dt(mu=0, sigma=1, df=1)
  # spei_3_effect ~ dt(mu = 0 , sigma = 1, df = 1)
  # spei_6_effect ~ dt(mu = 0 , sigma = 1, df = 1)
  #spei_12_effect ~ dt(mu = 0 , sigma = 1, df = 1)
  age2_effect ~ dt(mu = 0, sigma = 1, df = 1)
  lac_score_preg_effect ~ dt(mu = 0, sigma = 1, df = 1)
  age_bm_effect ~ dt(mu = 0, sigma = 1, df = 1)
  date_lac_effect ~ dt(mu = 0, sigma = 1, df = 1)
  date_age_effect ~ dt(mu = 0, sigma = 1, df = 1)
  
  
  
  for(i in 1:n.obs){
    
    #Process Model
    logit(preg_prob[i])<-Intercept+
      #(lac_score[i]*lac_effect)+
      #(est_bm[i]*bm_effect)+
       (dde_m[i]*dde_effect_m)+
       (age[i]*age_effect)+
      (calf_ind[i] * calf_ind_effect) +
      #(calf_mass[i]* c_m_effect)+
      #(pow(age[i], 2)*age2_effect)+
      (calving_date[i] *date_effect )+
      # (sex_correct[i] * sex_effect) +
      (lac_time[i]* lac_time_effect)+
      # (time_1_preg[i]* time_1_preg_effect)+
      # (spei_3[i]*spei_3_effect)+
     # (date_age_effect *age[i]*calving_date[i] )+
      # (spei_6[i]*spei_6_effect)+
      (date_lac_effect *lac_time[i]*calving_date[i] )#+
    #(spei_12[i]*spei_12_effect)#+
    # (age[i]*est_bm[i]*age_bm_effect)
    
    
    
    
    
    
    #Observation Model (Likelihood)
    preg_status[i] ~ dbern(preg_prob[i])
    
    #Impute missing data values
    lac_score[i] ~ dnorm(mean=0,sd=1)
    # precip_year_prior[i] ~ dnorm(mean = 0, sd = 1)
    dde_m[i] ~ dnorm(mean=0,sd=1)
    age[i] ~ dnorm(mean=0,sd=1)
    # est_body_cond[i] ~ dnorm(mean=0,sd=1)
    est_bm[i] ~ dnorm(mean=0,sd=1)
    
    #lac_time[i] ~ dnorm(mean=0,sd=1)##does this matrer
    calving_date[i] ~ dnorm(mean = 0, sd = 1)
    # spei_6[i] ~ dnorm(mean = 0, sd = 1)
    # spei_12[i] ~ dnorm(mean = 0, sd = 1)
    
    
    
  }#i
  
})#model5
parameters <- c(#"c_m_effect",
  #"bm_effect",
  "dde_effect_m",
  #"age_effect",
  "date_effect",
  #"age_bm_effect",
  # "age2_effect",
  "calf_ind_effect",
  "date_lac_effect",
  #"date_age_effect",
  # "precip_effect",
  "Intercept", 
  # "bod_con_effect",
  "lac_time_effect",
  # "sex_effect",
  # "time_1_preg_effect",
  "logProb_preg_status"#,
  # "spei_3_effect",
  # "spei_6_effect"#,
  #"spei_12_effect"#,
  #"lac_score_preg_effect"#,
  ##"sex_lac_effect"
)

# MCMC Settings
ni <- 400000
nt <- 400
nb <- 200000
nc <- 3

nimble.data = list(
  #calf_mass = as.numeric(cow_capture_all$mass_calf),
  preg_status = as.numeric(cow_capture_all$time_2_preg) - 1,
  lac_score = as.numeric((cow_capture_all$lac_score)),
  est_bm = as.numeric((cow_capture_all$est_bm)),
  calf_ind =  as.numeric(cow_capture_all$calf_ind),
  #dde_95 = as.numeric((cow_capture_all$area_DDE_gt_95)),
  #dde_75 = as.numeric((cow_capture_all$area_DDE_gt_75)),
  dde_m = as.numeric((cow_capture_all$area_DDE_gt_mean)),
  age = as.numeric((cow_capture_all$age_estimate_2)),
  #precip_year_prior = as.numeric((cow_capture_all$total_year_prior)),
  sex_correct = ifelse(cow_capture_all$sex_correct== "m", 1,0),
  lac_time= as.numeric(cow_capture_all$lac_time), 
  time_1_preg= as.numeric(cow_capture_all$time_1_preg)-1,#
  calving_date = as.numeric(cow_capture_all$birt_date),
  #spei_3 = as.numeric((cow_capture_all$sept_year_prior_3M)),
  spei_6 = as.numeric((cow_capture_all$sept_year_prior_6M))#,
  #spei_12 = as.numeric((cow_capture_all$sept_year_prior_12M))
  
)

nimble.constants = list(n.obs = nrow(cow_capture_all))


mcmc.output_half_add_a2_lactime_reduced_preg_lac_r5_age<- nimbleMCMC(code = model_half_add_a2_lactime_reduced_preg_lac_r5_age,
                                                                 data = nimble.data,
                                                                 constants=nimble.constants,
                                                                 monitors = parameters,
                                                                 niter = ni,
                                                                 nburnin = nb,
                                                                 nchains = nc,
                                                                 thin=nt,
                                                                 summary=TRUE,
                                                                 samplesAsCodaMCMC = TRUE)

attach.nimble(mcmc.output_half_add_a2_lactime_reduced_preg_lac_r5_age$samples)


mata_half  <- do.call(rbind, lapply(mcmc.output_half_add_a2_lactime_reduced_preg_lac_r5_age$samples, as.matrix))
lpa_half  <- grep("^logProb_preg_status\\[", colnames(mata_half), value = TRUE)
lla_half  <- mata_half[, lpa_half, drop = FALSE]
stopifnot(all(is.finite(lla_half) | (is.infinite(lla_half) & lla_half < 0)))
loo_half_add_a2_lactime_preg_lac_r5_age <- loo(lla_half) 



attach.nimble(mcmc.output_half_add_a2_lactime_reduced_preg_lac_r5_age$samples)


# Gelman-Rubin diagnostic (AKA RHat or PSRF)
gelman.diag(mcmc.output_half_add_a2_lactime_reduced_preg_lac_r5_age$samples, multivariate = FALSE)



mcmcplot(mcmc.output_half_add_a2_lactime_reduced_preg_lac_r5_age$samples)



##how to get effects size
##jow to make the differnt charts in PP
##how to get time on deck chart
##uses zscore to preduct value. Use mean for all the other values
##will use for calf age ##what the fuck is a z-score?
post_mat <- as.matrix(mcmc.output_half_add_a2_lactime_reduced_preg_lac_r5_age$samples)
# 2. Specify the parameters you care about
params <- c(#"c_m_effect",
  #"bm_effect",
  "dde_effect_m",
  #"age_effect",
  "date_effect",
  #"date_age_effect",
  #"age_bm_effect",
  # "age2_effect",
  #"sex_effect",
  "date_lac_effect",
  # "precip_effect",
  "Intercept", 
  # "bod_con_effect",
  "lac_time_effect",
  "calf_ind_effect"#,
  # "sex_effect",
  #"time_1_preg_effect"#,
  
  # "spei_3_effect",
  # "spei_6_effect"#,
  
)

# 3. Summarize each one
summary_mcmc.output_half_add_a2_lactime_reduced_preg_lac_r5_age<- tibble(parameter = params) %>%
  rowwise() %>%
  mutate(
    median   = median(post_mat[, parameter]),
    lower95  = quantile(post_mat[, parameter], 0.025),
    upper95  = quantile(post_mat[, parameter], 0.975),
    # posterior probability effect > 0
    p_gt_zero = mean(post_mat[, parameter] > 0),
    # if you expect a negative effect, you can also compute P(<0):
    p_lt_zero = mean(post_mat[, parameter] < 0)
  ) %>%
  ungroup()

print(summary_mcmc.output_half_add_a2_lactime_reduced_preg_lac_r5_age)

#######################################

# Diagnostics
gelman.diag(mcmc.output_null$samples[, "Intercept"], multivariate = FALSE)
print(loo_null)

# Sanity check: back-transformed intercept should be close to 33/50 = 0.66
median(plogis(mata_null[, "Intercept"]))
quantile(plogis(mata_null[, "Intercept"]), c(0.025, 0.975))

###########compare

comp <- loo_compare(list(
  reduced        = loo_half_add_a2_lactime_r,
  reduced_75_12p = loo_half_add_a2_lactime_r_75_12p,
  combo          = loo_half_add_a2_lactime_r_combo,
  aged           = loo_half_add_a2_lactime_aged,
  preg           = loo_half_add_a2_lactime_preg,
  preg_lac       = loo_half_add_a2_lactime_preg_lac,
  preg_lac_r     = loo_half_add_a2_lactime_preg_lac_r,
  preg_lac_r3     = loo_half_add_a2_lactime_preg_lac_r3,
  preg_lac_r4     = loo_half_add_a2_lactime_preg_lac_r4,
  preg_lac_r4dee     = loo_half_add_a2_lactime_preg_lac_r4dde,
  global         = loo_global,
  global_r       = loo_global_95_re,
  preg_lac_r5_dde = loo_half_add_a2_lactime_preg_lac_r5_age,
  
  preg_lac_r5     = loo_half_add_a2_lactime_preg_lac_r5 ,
 
  null       = loo_null   # once you fit the intercept-only model
))
print(comp,  digits = 2,  simplify = FALSE)




loo_model_weights(list(
  preg_lac_r3 = loo_half_add_a2_lactime_preg_lac_r3,  # use your object name
  preg_lac_r  = loo_half_add_a2_lactime_preg_lac_r,
  null        = loo_null,
  global_r    = loo_global_95_re
), method = "stacking")
