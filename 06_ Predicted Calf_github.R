# Download & Install Nimble here: https://r-nimble.org/download
# Make sure you have XCode installed on Mac (https://r-nimble.org/html_manual/cha-installing-nimble.html)
# or RTools on Windows: https://cran.r-project.org/bin/windows/Rtools/

#Make sure all of the below packages are installed:

##this code read in cow data from random forest recults and covaraties. 
###creates The logistic process model for "Predicted Calf” 

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

cow_capture_all_1<- read_csv("outputs/cleaned_up_data_set/Predicted Calf_randomforest_cows.csv")
names(cow_capture_all_1)



cow_capture_all<-cow_capture_all_1 %>% 
  #filter(!is.na(sex)) %>% #keeps only cows that had a calf
  #filter(!(is.na(sex) & time_1_preg == 1)) %>% ##keeps cows that did not have a calf in time-1 
  #but removes cows where we didnt get calf
  #mutate(trunk_days = ifelse(is.na(trunk_days), 0, trunk_days)) %>%  #makes days calf alive 0 for those cows
  select(-c(`VIT Placed`)) %>% 
  mutate(time_1_preg = as.factor(time_1_preg),
         time_2_preg = as.factor(time_2_preg),
         capture_year = as.factor(capture_year),
         age_estimate_2 = age_estimate_2 +1,
         caught_calf=as.factor(dplyr::if_else(mass_days_bag > 1, 1L, 0L, missing = 0L)),
         sex_correct  = ifelse(time_1_preg  == 1 & caught_calf == 1, sex, 0),
         across(
           .cols = c(where(is.numeric)#,
                     #-age_estimate_2
           ),  # optional: if you want to keep raw years unscaled
           .fns = ~ as.numeric(scale(.))
         ),
         lac_time = ifelse(time_1_preg == 1 & caught_calf == 1, trunk_days, 0),
         mass_calf = ifelse(time_1_preg == 1 & caught_calf == 1, mass_days_bag,0),
         birt_date = ifelse(time_1_preg == 1 & caught_calf == 1, j_expelled, 0 )) #%>% 
#filter(!is.na(sex))  ##remove any where calf was not captured


names(cow_capture_all) <- names(cow_capture_all) %>%
  gsub("[ ,()/]", "_", .) %>%       # Replace space, comma, parentheses with underscore
  gsub("_+", "_", .) %>%           # Collapse multiple underscores into one
  gsub("_$", "", .)   



cow_capture_all_2<-cow_capture_all_1 %>% 
  select(c("est_bm", "age_estimate_2"))
cor_matrix <- cor(cow_capture_all_2, use = "pairwise.complete.obs")
plot(cow_capture_all_2)
set.seed(123)




model_preg_lac <-  nimbleCode({
  
  #Priors
  Intercept  ~ dnorm(0, sd = 5)
  
  
  time_1_preg_effect ~ dt(mu=0, sigma=1, df=1)
  
  
  for(i in 1:n.obs){
    
    #Process Model
    logit(preg_prob[i])<-Intercept+
      
      (time_1_preg[i]* time_1_preg_effect)#+
    
    
    
    
    
    
    #Observation Model (Likelihood)
    preg_status[i] ~ dbern(preg_prob[i])
    
    #Impute missing data values
    
    
    
  }#i
  
})#model5
parameters <- c(
  "Intercept", 
  "time_1_preg_effect",
  "logProb_preg_status")


# MCMC Settings
ni <- 400000
nt <- 400
nb <- 200000
nc <- 3

nimble.data = list(
  preg_status = as.numeric(cow_capture_all$time_2_preg) - 1,
  
  time_1_preg= as.numeric(cow_capture_all$time_1_preg)-1#
  
  
)

nimble.constants = list(n.obs = nrow(cow_capture_all))


mcmc.output_preg_lac <- nimbleMCMC(code = model_preg_lac,
                                   data = nimble.data,
                                   constants=nimble.constants,
                                   monitors = parameters,
                                   niter = ni,
                                   nburnin = nb,
                                   nchains = nc,
                                   thin=nt,
                                   summary=TRUE,
                                   samplesAsCodaMCMC = TRUE)

attach.nimble(mcmc.output_preg_lac$samples)


mata_pl  <- do.call(rbind, lapply(mcmc.output_preg_lac$samples, as.matrix))
lpa_pl  <- grep("^logProb_preg_status\\[", colnames(mata_pl), value = TRUE)
lla_pl   <- mata_pl[, lpa_pl, drop = FALSE]
stopifnot(all(is.finite(lla_pl) | (is.infinite(lla_pl) & lla_pl < 0)))
loo_pl <- loo(lla_pl)




#################Model 2 lac effect########################################
###decrease to just lac effect

model_lac_time_only <-  nimbleCode({
  
  #Priors
  Intercept  ~ dnorm(0, sd = 5)
  
  
  lac_time_effect ~dt(mu=0, sigma=1, df=1)
  
  
  for(i in 1:n.obs){
    
    #Process Model
    logit(preg_prob[i])<-Intercept+
      
      
      
      (lac_time[i]* lac_time_effect)
    
    
    #Observation Model (Likelihood)
    preg_status[i] ~ dbern(preg_prob[i])
    
    
    
    
  }#i
  
})#model5
parameters <- c(
  "Intercept", 
  
  "lac_time_effect",
  
  "logProb_preg_status"
  
)

# MCMC Settings
ni <- 400000
nt <- 400
nb <- 200000
nc <- 3

nimble.data = list(
  preg_status = as.numeric(cow_capture_all$time_2_preg) - 1,
  
  sex_correct = ifelse(cow_capture_all$sex_correct== "m", 1,0),
  lac_time= as.numeric(cow_capture_all$lac_time), 
  time_1_preg= as.numeric(cow_capture_all$time_1_preg)-1#,#
  
  
)

nimble.constants = list(n.obs = nrow(cow_capture_all))


mcmc.output_calf_only <- nimbleMCMC(code = model_lac_time_only,
                                    data = nimble.data,
                                    constants=nimble.constants,
                                    monitors = parameters,
                                    niter = ni,
                                    nburnin = nb,
                                    nchains = nc,
                                    thin=nt,
                                    summary=TRUE,
                                    samplesAsCodaMCMC = TRUE)

attach.nimble(mcmc.output_calf_only$samples)


mata_calf  <- do.call(rbind, lapply(mcmc.output_calf_only$samples, as.matrix))
lpa_calf  <- grep("^logProb_preg_status\\[", colnames(mata_calf), value = TRUE)
lla_calf   <- mata_calf[, lpa_calf, drop = FALSE]
stopifnot(all(is.finite(lla_calf) | (is.infinite(lla_calf) & lla_calf < 0)))
loo_calf <- loo(lla_calf)





###########################Model 3##############################################
##DDE effect

model_DDE <-  nimbleCode({
  
  #Priors
  Intercept  ~ dnorm(0, sd = 5)
  
  dde_effect_m ~ dt(mu=0, sigma=1, df=1) 
  
  
  
  
  for(i in 1:n.obs){
    
    #Process Model
    logit(preg_prob[i])<-Intercept+
      
      (dde_95[i]*dde_effect_m)
    

    #Observation Model (Likelihood)
    preg_status[i] ~ dbern(preg_prob[i])
    
    #Impute missing data values
    
    #dde_95[i] ~ dnorm(mean=0,sd=1)
    
    
    
  }#i
  
})
parameters <- c(
  "dde_effect_m",
  "Intercept", 
  "logProb_preg_status"
  
)

# MCMC Settings
ni <- 400000
nt <- 400
nb <- 200000
nc <- 3

nimble.data = list(
  preg_status = as.numeric(cow_capture_all$time_2_preg) - 1,
  dde_95 = as.numeric((cow_capture_all$area_DDE_gt_95))
  
  
)

nimble.constants = list(n.obs = nrow(cow_capture_all))


mcmc.output_dde <- nimbleMCMC(code = model_DDE,
                              data = nimble.data,
                              constants=nimble.constants,
                              monitors = parameters,
                              niter = ni,
                              nburnin = nb,
                              nchains = nc,
                              thin=nt,
                              summary=TRUE,
                              samplesAsCodaMCMC = TRUE)

attach.nimble(mcmc.output_dde$samples)
beep(sound = 2)

mata_all_dde  <- do.call(rbind, lapply(mcmc.output_dde$samples, as.matrix))
lpa_all_dde  <- grep("^logProb_preg_status\\[", colnames(mata_all_dde), value = TRUE)
lla_all_dde   <- mata_all_dde[, lpa_all_dde, drop = FALSE]
stopifnot(all(is.finite(lla_all_dde) | (is.infinite(lla_all_dde) & lla_all_dde < 0)))
loo_all_dde <- loo(lla_all_dde)





###############################model SPEI 12
##

model_all_spei <-  nimbleCode({
  
  #Priors
  Intercept  ~ dnorm(0, sd = 5)
  
  
  spei_12_effect ~ dt(mu = 0 , sigma = 1, df = 1)
  
  
  
  for(i in 1:n.obs){
    
    #Process Model
    logit(preg_prob[i])<-Intercept+
      
      (spei_12[i]*spei_12_effect)#+
    
    
    
    
    
    
    #Observation Model (Likelihood)
    preg_status[i] ~ dbern(preg_prob[i])
    
    #Impute missing data values
    
    spei_12[i] ~ dnorm(mean = 0, sd = 1)
    
    
    
  }#i
  
})
parameters <- c(
  "Intercept", 
  
  "logProb_preg_status",#
  
  "spei_12_effect"#,
  
)

# MCMC Settings
ni <- 400000
nt <- 400
nb <- 200000
nc <- 3

nimble.data = list(
  preg_status = as.numeric(cow_capture_all$time_2_preg) - 1,
  
  spei_12 = as.numeric((cow_capture_all$sept_year_prior_12M))
  
)

nimble.constants = list(n.obs = nrow(cow_capture_all))


mcmc.output_spei <- nimbleMCMC(code = model_all_spei,
                               data = nimble.data,
                               constants=nimble.constants,
                               monitors = parameters,
                               niter = ni,
                               nburnin = nb,
                               nchains = nc,
                               thin=nt,
                               summary=TRUE,
                               samplesAsCodaMCMC = TRUE)

attach.nimble(mcmc.output_spei$samples)
beep(sound =3)

mata_all_spei  <- do.call(rbind, lapply(mcmc.output_spei$samples, as.matrix))
lpa_all_spei  <- grep("^logProb_preg_status\\[", colnames(mata_all_spei), value = TRUE)
lla_all_spei   <- mata_all_spei[, lpa_all_spei, drop = FALSE]
stopifnot(all(is.finite(lla_all_spei) | (is.infinite(lla_all_spei) & lla_all_spei < 0)))
loo_all_spei <- loo(lla_all_spei)




##################end model 5
#################Model 2 95 DDE########################################
###full model DDE, SPEI, time_1_preg, calf_sex, lac time, age, BM, lac_score

model_full <-  nimbleCode({
  
  #Priors
  Intercept  ~ dnorm(0, sd = 5)
  
  lac_effect~ dt(mu=0, sigma=1, df=1)
  bm_effect ~ dt(mu=0, sigma=1, df=1)
  dde_effect_m ~ dt(mu=0, sigma=1, df=1) 
  age_effect ~ dt(mu=0, sigma=1, df=1)
  
  lac_time_effect ~dt(mu=0, sigma=1, df=1)
  sex_effect ~ dt(mu=0, sigma=1, df=1)
  
  time_1_preg_effect ~ dt(mu=0, sigma=1, df=1)

  spei_12_effect ~ dt(mu = 0 , sigma = 1, df = 1)

  
  
  for(i in 1:n.obs){
    
    #Process Model
    logit(preg_prob[i])<-Intercept+
      (lac_score[i]*lac_effect)+
      (est_bm[i]*bm_effect)+
      (dde_95[i]*dde_effect_m)+
      (age[i]*age_effect)+
 
      
      (sex_correct[i] * sex_effect) +
      (lac_time[i]* lac_time_effect)+
      (time_1_preg[i]* time_1_preg_effect)+
   
      (spei_12[i]*spei_12_effect)
    
    
    
    
    
    
    
    #Observation Model (Likelihood)
    preg_status[i] ~ dbern(preg_prob[i])
    
    #Impute missing data values
    lac_score[i] ~ dnorm(mean=0,sd=1)
   
    dde_95[i] ~ dnorm(mean=0,sd=1)
    age[i] ~ dnorm(mean=0,sd=1)
   
    est_bm[i] ~ dnorm(mean=0,sd=1)
    
    lac_time[i] ~ dnorm(mean=0,sd=1)

    spei_12[i] ~ dnorm(mean = 0, sd = 1)
    
    
    
  }#i
  
})#model5
parameters <- c("lac_effect",
                "bm_effect",
                "dde_effect_m",
                "age_effect",

                "Intercept", 
 
                "lac_time_effect",
                "sex_effect",
                "time_1_preg_effect",
                "logProb_preg_status",#
 
                "spei_12_effect"

)

# MCMC Settings
ni <- 400000
nt <- 400
nb <- 200000
nc <- 3

nimble.data = list(
  preg_status = as.numeric(cow_capture_all$time_2_preg) - 1,
  lac_score = as.numeric((cow_capture_all$lac_score)),
  est_bm = as.numeric((cow_capture_all$est_bm)),
  dde_95 = as.numeric((cow_capture_all$area_DDE_gt_95)),
  age = as.numeric((cow_capture_all$age_estimate_2)),

  sex_correct = ifelse(cow_capture_all$sex_correct== "m", 1,0),
  lac_time= as.numeric(cow_capture_all$lac_time), 
  time_1_preg= as.numeric(cow_capture_all$time_1_preg)-1,#

  spei_12 = as.numeric((cow_capture_all$sept_year_prior_12M))
  
)

nimble.constants = list(n.obs = nrow(cow_capture_all))


mcmc.output_full<- nimbleMCMC(code = model_full,
                              data = nimble.data,
                              constants=nimble.constants,
                              monitors = parameters,
                              niter = ni,
                              nburnin = nb,
                              nchains = nc,
                              thin=nt,
                              summary=TRUE,
                              samplesAsCodaMCMC = TRUE)

attach.nimble(mcmc.output_full$samples)


mata_full  <- do.call(rbind, lapply(mcmc.output_full$samples, as.matrix))
lpa_full <- grep("^logProb_preg_status\\[", colnames(mata_full), value = TRUE)
lla_full  <- mata_full[, lpa_full, drop = FALSE]
stopifnot(all(is.finite(lla_full) | (is.infinite(lla_full) & lla_full < 0)))
loo_full <- loo(lla_full)


##########################################################end model 5
####preg effect

model_calf_only_lac1 <-  nimbleCode({
  
  #Priors
  Intercept  ~ dnorm(0, sd = 5)
  

  
  lac_time_effect ~dt(mu=0, sigma=1, df=1)
  sex_effect ~ dt(mu=0, sigma=1, df=1)
  
  time_1_preg_effect ~ dt(mu=0, sigma=1, df=1)

  sex_lac_effect ~ dt(mu = 0, sigma = 1, df = 1)

  
  
  for(i in 1:n.obs){
    
    #Process Model
    logit(preg_prob[i])<-Intercept+

      
      (sex_correct[i] * sex_effect) +
      (lac_time[i]* lac_time_effect)+
      (time_1_preg[i]* time_1_preg_effect)+

      (lac_time[i]*sex_correct[i]*sex_lac_effect)
    
    
    
    
    
    
    #Observation Model (Likelihood)
    preg_status[i] ~ dbern(preg_prob[i])

    
    
    
  }#i
  
})
parameters <- c(
  "Intercept", 
 
  "lac_time_effect",
  "sex_effect",
  "time_1_preg_effect",
  "logProb_preg_status",#

  "sex_lac_effect"
)

# MCMC Settings
ni <- 400000
nt <- 400
nb <- 200000
nc <- 3

nimble.data = list(
  preg_status = as.numeric(cow_capture_all$time_2_preg) - 1,

  sex_correct = ifelse(cow_capture_all$sex_correct== "m", 1,0),
  lac_time= as.numeric(cow_capture_all$lac_time), 
  time_1_preg= as.numeric(cow_capture_all$time_1_preg)-1#,#

)

nimble.constants = list(n.obs = nrow(cow_capture_all))


mcmc.output_calf_lac <- nimbleMCMC(code = model_calf_only_lac1,
                                   data = nimble.data,
                                   constants=nimble.constants,
                                   monitors = parameters,
                                   niter = ni,
                                   nburnin = nb,
                                   nchains = nc,
                                   thin=nt,
                                   summary=TRUE,
                                   samplesAsCodaMCMC = TRUE)

attach.nimble(mcmc.output_calf_lac$samples)
beep(sound = 1)

mata_calf_lac  <- do.call(rbind, lapply(mcmc.output_calf_lac$samples, as.matrix))
lpa_calf_lac  <- grep("^logProb_preg_status\\[", colnames(mata_calf_lac), value = TRUE)
lla_calf_lac  <- mata_calf_lac[, lpa_calf_lac, drop = FALSE]
stopifnot(all(is.finite(lla_calf_lac) | (is.infinite(lla_calf_lac) & lla_calf_lac < 0)))
loo_calf_lac <- loo(lla_calf_lac)





##########################################################end model 6
####half full################
model_half <-  nimbleCode({
  
  #Priors
  Intercept  ~ dnorm(0, sd = 5)

  bm_effect ~ dt(mu=0, sigma=1, df=1)
  dde_effect_m ~ dt(mu=0, sigma=1, df=1) 
  age_effect ~ dt(mu=0, sigma=1, df=1)

  
  time_1_preg_effect ~ dt(mu=0, sigma=1, df=1)

  spei_12_effect ~ dt(mu = 0 , sigma = 1, df = 1)

  
  
  for(i in 1:n.obs){
    
    #Process Model
    logit(preg_prob[i])<-Intercept+
    
      (est_bm[i]*bm_effect)+
      (dde_95[i]*dde_effect_m)+
      (age[i]*age_effect)+

      (time_1_preg[i]* time_1_preg_effect)+

      (spei_12[i]*spei_12_effect)#+

    
    
    
    
    
    
    #Observation Model (Likelihood)
    preg_status[i] ~ dbern(preg_prob[i])
    
    #Impute missing data values
    lac_score[i] ~ dnorm(mean=0,sd=1)

    est_bm[i] ~ dnorm(mean=0,sd=1)

    
    
    
  }#i
  
})
parameters <- c(#"lac_effect",
  "bm_effect",
  "dde_effect_m",
  "age_effect",

  "Intercept", 

  "time_1_preg_effect",
  "logProb_preg_status",

  "spei_12_effect"

)

# MCMC Settings
ni <- 400000
nt <- 400
nb <- 200000
nc <- 3

nimble.data = list(
  preg_status = as.numeric(cow_capture_all$time_2_preg) - 1,
  lac_score = as.numeric((cow_capture_all$lac_score)),
  est_bm = as.numeric((cow_capture_all$est_bm)),
  dde_95 = as.numeric((cow_capture_all$area_DDE_gt_95)),
  age = as.numeric((cow_capture_all$age_estimate_2)),
  #precip_year_prior = as.numeric((cow_capture_all$total_year_prior)),
  sex_correct = ifelse(cow_capture_all$sex_correct== "m", 1,0),
  lac_time= as.numeric(cow_capture_all$lac_time), 
  time_1_preg= as.numeric(cow_capture_all$time_1_preg)-1,#

  spei_12 = as.numeric((cow_capture_all$sept_year_prior_12M))
  
)

nimble.constants = list(n.obs = nrow(cow_capture_all))


mcmc.output_half <- nimbleMCMC(code = model_half,
                               data = nimble.data,
                               constants=nimble.constants,
                               monitors = parameters,
                               niter = ni,
                               nburnin = nb,
                               nchains = nc,
                               thin=nt,
                               summary=TRUE,
                               samplesAsCodaMCMC = TRUE)

attach.nimble(mcmc.output_half$samples)
beep(sound = 1)

mata_half  <- do.call(rbind, lapply(mcmc.output_half$samples, as.matrix))
lpa_half  <- grep("^logProb_preg_status\\[", colnames(mata_half), value = TRUE)
lla_half  <- mata_half[, lpa_half, drop = FALSE]
stopifnot(all(is.finite(lla_half) | (is.infinite(lla_half) & lla_half < 0)))
loo_half <- loo(lla_half)




##########################################################end model 7


####calf stuff with SPEI ################
model_calf_spei <-  nimbleCode({
  
  #Priors
  Intercept  ~ dnorm(0, sd = 5)

  
  time_1_preg_effect ~ dt(mu=0, sigma=1, df=1)

  spei_12_effect ~ dt(mu = 0 , sigma = 1, df = 1)

  
  
  for(i in 1:n.obs){
    
    #Process Model
    logit(preg_prob[i])<-Intercept+

      (time_1_preg[i]* time_1_preg_effect)+

      (spei_12[i]*spei_12_effect)

    #Observation Model (Likelihood)
    preg_status[i] ~ dbern(preg_prob[i])
    
    #Impute missing data values
    lac_score[i] ~ dnorm(mean=0,sd=1)
    
    spei_12[i] ~ dnorm(mean = 0, sd = 1)
    
    
    
  }#i
  
})#model5
parameters <- c(

  "Intercept", 
  
  "time_1_preg_effect",
  "logProb_preg_status",#

  "spei_12_effect"#,

)

# MCMC Settings
ni <- 400000
nt <- 400
nb <- 200000
nc <- 3

nimble.data = list(
  preg_status = as.numeric(cow_capture_all$time_2_preg) - 1,

  time_1_preg= as.numeric(cow_capture_all$time_1_preg)-1,

  spei_12 = as.numeric((cow_capture_all$sept_year_prior_12M))
  
)

nimble.constants = list(n.obs = nrow(cow_capture_all))


mcmc.output_spei <- nimbleMCMC(code = model_calf_spei,
                               data = nimble.data,
                               constants=nimble.constants,
                               monitors = parameters,
                               niter = ni,
                               nburnin = nb,
                               nchains = nc,
                               thin=nt,
                               summary=TRUE,
                               samplesAsCodaMCMC = TRUE)
beep(sound = 1)
attach.nimble(mcmc.output_spei$samples)


mata_calf_spei  <- do.call(rbind, lapply(mcmc.output_spei$samples, as.matrix))
lpa_calf_spei  <- grep("^logProb_preg_status\\[", colnames(mata_calf_spei), value = TRUE)
lla_calf_spei  <- mata_calf_spei[, lpa_calf_spei, drop = FALSE]
stopifnot(all(is.finite(lla_calf_spei) | (is.infinite(lla_calf_spei) & lla_calf_spei < 0)))
loo_calf_spei <- loo(lla_calf_spei)




##########################################################end model 8
###full model DDE, SPEI, time_1_preg, calf_sex, lac time, age, BM, lac_score

model_full_3 <-  nimbleCode({
  
  #Priors
  Intercept  ~ dnorm(0, sd = 5)
  
  lac_effect~ dt(mu=0, sigma=1, df=1)
  bm_effect ~ dt(mu=0, sigma=1, df=1)
  dde_effect_m ~ dt(mu=0, sigma=1, df=1) 
  age_effect ~ dt(mu=0, sigma=1, df=1)
  
  lac_time_effect ~dt(mu=0, sigma=1, df=1)
  sex_effect ~ dt(mu=0, sigma=1, df=1)
  
  time_1_preg_effect ~ dt(mu=0, sigma=1, df=1)
  spei_3_effect ~ dt(mu = 0 , sigma = 1, df = 1)

  
  for(i in 1:n.obs){
    
    #Process Model
    logit(preg_prob[i])<-Intercept+
      (lac_score[i]*lac_effect)+
      (est_bm[i]*bm_effect)+
      (dde_95[i]*dde_effect_m)+
      (age[i]*age_effect)+
    
      
      (sex_correct[i] * sex_effect) +
      (lac_time[i]* lac_time_effect)+
      (time_1_preg[i]* time_1_preg_effect)+
      (spei_3[i]*spei_3_effect)

    
    
    
    
    
    
    #Observation Model (Likelihood)
    preg_status[i] ~ dbern(preg_prob[i])
    
    #Impute missing data values
    lac_score[i] ~ dnorm(mean=0,sd=1)
    
    dde_95[i] ~ dnorm(mean=0,sd=1)
    age[i] ~ dnorm(mean=0,sd=1)
   
    est_bm[i] ~ dnorm(mean=0,sd=1)
    
    lac_time[i] ~ dnorm(mean=0,sd=1)
    spei_3[i] ~ dnorm(mean = 0, sd = 1)

    
    
    
  }
  
})
parameters <- c("lac_effect",
                "bm_effect",
                "dde_effect_m",
                "age_effect",

                "Intercept", 

                "lac_time_effect",
                "sex_effect",
                "time_1_preg_effect",
                "logProb_preg_status",
                "spei_3_effect"


# MCMC Settings
ni <- 400000
nt <- 400
nb <- 200000
nc <- 3

nimble.data = list(
  preg_status = as.numeric(cow_capture_all$time_2_preg) - 1,
  lac_score = as.numeric((cow_capture_all$lac_score)),
  est_bm = as.numeric((cow_capture_all$est_bm)),
  dde_95 = as.numeric((cow_capture_all$area_DDE_gt_95)),
  age = as.numeric((cow_capture_all$age_estimate_2)),
  
  sex_correct = ifelse(cow_capture_all$sex_correct== "m", 1,0),
  lac_time= as.numeric(cow_capture_all$lac_time), 
  time_1_preg= as.numeric(cow_capture_all$time_1_preg)-1,
  
  spei_3 = as.numeric((cow_capture_all$sept_year_prior_3M))

)

nimble.constants = list(n.obs = nrow(cow_capture_all))


mcmc.output_full_3<- nimbleMCMC(code = model_full_3,
                                data = nimble.data,
                                constants=nimble.constants,
                                monitors = parameters,
                                niter = ni,
                                nburnin = nb,
                                nchains = nc,
                                thin=nt,
                                summary=TRUE,
                                samplesAsCodaMCMC = TRUE)

attach.nimble(mcmc.output_full_3$samples)


mata_full <- do.call(rbind, lapply(mcmc.output_full_3$samples, as.matrix))
lpa_full <- grep("^logProb_preg_status\\[", colnames(mata_full), value = TRUE)
lla_full  <- mata_full[, lpa_full, drop = FALSE]
stopifnot(all(is.finite(lla_full) | (is.infinite(lla_full) & lla_full < 0)))
loo_full_spei3 <- loo(lla_full)

##########################################################end model 9


###full model DDE, SPEI 6 month, time_1_preg, calf_sex, lac time, age, BM, lac_score

model_full_6 <-  nimbleCode({
  
  #Priors
  Intercept  ~ dnorm(0, sd = 5)
  
  lac_effect~ dt(mu=0, sigma=1, df=1)
  bm_effect ~ dt(mu=0, sigma=1, df=1)
  dde_effect_m ~ dt(mu=0, sigma=1, df=1) 
  age_effect ~ dt(mu=0, sigma=1, df=1)
  
  lac_time_effect ~dt(mu=0, sigma=1, df=1)
  sex_effect ~ dt(mu=0, sigma=1, df=1)
  precip_effect ~ dt(mu=0, sigma=1, df=1)
  time_1_preg_effect ~ dt(mu=0, sigma=1, df=1)

  spei_6_effect ~ dt(mu = 0 , sigma = 1, df = 1)
  
  age2_effect ~ dt(mu = 0, sigma = 1, df = 1)


  
  
  for(i in 1:n.obs){
    
    #Process Model
    logit(preg_prob[i])<-Intercept+
      (lac_score[i]*lac_effect)+
      (est_bm[i]*bm_effect)+
      (dde_95[i]*dde_effect_m)+
      (age[i]*age_effect)+
      (precip_year_prior[i] * precip_effect)+
      (pow(age[i], 2)*age2_effect)+
      
      (sex_correct[i] * sex_effect) +
      (lac_time[i]* lac_time_effect)+
      (time_1_preg[i]* time_1_preg_effect)+
      
      (spei_6[i]*spei_6_effect)
    

    #Observation Model (Likelihood)
    preg_status[i] ~ dbern(preg_prob[i])
    
    #Impute missing data values
    lac_score[i] ~ dnorm(mean=0,sd=1)
    precip_year_prior[i] ~ dnorm(mean = 0, sd = 1)
    dde_95[i] ~ dnorm(mean=0,sd=1)
    age[i] ~ dnorm(mean=0,sd=1)
    
    est_bm[i] ~ dnorm(mean=0,sd=1)
    
    lac_time[i] ~ dnorm(mean=0,sd=1)
    spei_6[i] ~ dnorm(mean = 0, sd = 1)
    
    
    
    
  }#i
  
})#model5
parameters <- c("lac_effect",
                "bm_effect",
                "dde_effect_m",
                "age_effect",
                # "age2_effect",
                "precip_effect",
                "Intercept", 
                # "bod_con_effect",
                "lac_time_effect",
                "sex_effect",
                "time_1_preg_effect",
                "logProb_preg_status",#
                # "spei_3_effect"#,
                "spei_6_effect"#,
                #"spei_12_effect"#,
                #"lac_score_preg_effect"#,
                #"sex_lac_effect"
)

# MCMC Settings
ni <- 400000
nt <- 400
nb <- 200000
nc <- 3

nimble.data = list(
  preg_status = as.numeric(cow_capture_all$time_2_preg) - 1,
  lac_score = as.numeric((cow_capture_all$lac_score)),
  est_bm = as.numeric((cow_capture_all$est_bm)),
  dde_95 = as.numeric((cow_capture_all$area_DDE_gt_95)),
  age = as.numeric((cow_capture_all$age_estimate_2)),
  precip_year_prior = as.numeric((cow_capture_all$total_year_prior)),
  sex_correct = ifelse(cow_capture_all$sex_correct== "m", 1,0),
  lac_time= as.numeric(cow_capture_all$lac_time), 
  time_1_preg= as.numeric(cow_capture_all$time_1_preg)-1,#
  
  #spei_3 = as.numeric((cow_capture_all$sept_year_prior_3M))#,
  spei_6 = as.numeric((cow_capture_all$sept_year_prior_6M))#,
  #spei_12 = as.numeric((cow_capture_all$sept_year_prior_12M))
  
)

nimble.constants = list(n.obs = nrow(cow_capture_all))


mcmc.output_full_6<- nimbleMCMC(code = model_full_6,
                                data = nimble.data,
                                constants=nimble.constants,
                                monitors = parameters,
                                niter = ni,
                                nburnin = nb,
                                nchains = nc,
                                thin=nt,
                                summary=TRUE,
                                samplesAsCodaMCMC = TRUE)

attach.nimble(mcmc.output_full_6$samples)


mata_full <- do.call(rbind, lapply(mcmc.output_full_6$samples, as.matrix))
lpa_full <- grep("^logProb_preg_status\\[", colnames(mata_full), value = TRUE)
lla_full  <- mata_full[, lpa_full, drop = FALSE]
stopifnot(all(is.finite(lla_full) | (is.infinite(lla_full) & lla_full < 0)))
loo_full_spei6 <- loo(lla_full)

##########################################################end model 10


####half full################
model_half_75 <-  nimbleCode({
  
  #Priors
  Intercept  ~ dnorm(0, sd = 5)

  bm_effect ~ dt(mu=0, sigma=1, df=1)
  dde_effect_m ~ dt(mu=0, sigma=1, df=1) 
  age_effect ~ dt(mu=0, sigma=1, df=1)

  
  time_1_preg_effect ~ dt(mu=0, sigma=1, df=1)

  spei_12_effect ~ dt(mu = 0 , sigma = 1, df = 1)

  
  
  for(i in 1:n.obs){
    
    #Process Model
    logit(preg_prob[i])<-Intercept+
      
      (est_bm[i]*bm_effect)+
      (dde_75[i]*dde_effect_m)+
      (age[i]*age_effect)+

      (time_1_preg[i]* time_1_preg_effect)+

      (spei_12[i]*spei_12_effect)#+
    
    
    
    #Observation Model (Likelihood)
    preg_status[i] ~ dbern(preg_prob[i])
    
    #Impute missing data values
    lac_score[i] ~ dnorm(mean=0,sd=1)

    est_bm[i] ~ dnorm(mean=0,sd=1)

    
  }
  
})
parameters <- c(
  "bm_effect",
  "dde_effect_m",
  "age_effect",
  "Intercept", 

  "time_1_preg_effect",
  "logProb_preg_status",
  "spei_12_effect"#,

)

# MCMC Settings
ni <- 400000
nt <- 400
nb <- 200000
nc <- 3

nimble.data = list(
  preg_status = as.numeric(cow_capture_all$time_2_preg) - 1,
  lac_score = as.numeric((cow_capture_all$lac_score)),
  est_bm = as.numeric((cow_capture_all$est_bm)),
 
  dde_75 = as.numeric((cow_capture_all$area_DDE_gt_75)),
  age = as.numeric((cow_capture_all$age_estimate_2)),
 
  sex_correct = ifelse(cow_capture_all$sex_correct== "m", 1,0),
  lac_time= as.numeric(cow_capture_all$lac_time), 
  time_1_preg= as.numeric(cow_capture_all$time_1_preg)-1,
  spei_12 = as.numeric((cow_capture_all$sept_year_prior_12M))
  
)

nimble.constants = list(n.obs = nrow(cow_capture_all))


mcmc.output_half_75 <- nimbleMCMC(code = model_half_75,
                                  data = nimble.data,
                                  constants=nimble.constants,
                                  monitors = parameters,
                                  niter = ni,
                                  nburnin = nb,
                                  nchains = nc,
                                  thin=nt,
                                  summary=TRUE,
                                  samplesAsCodaMCMC = TRUE)

attach.nimble(mcmc.output_half_75$samples)


mata_half  <- do.call(rbind, lapply(mcmc.output_half_75$samples, as.matrix))
lpa_half  <- grep("^logProb_preg_status\\[", colnames(mata_half), value = TRUE)
lla_half  <- mata_half[, lpa_half, drop = FALSE]
stopifnot(all(is.finite(lla_half) | (is.infinite(lla_half) & lla_half < 0)))
loo_half_75 <- loo(lla_half)


####end of 11

####half full################mean dde, spei_6
model_half_m6 <-  nimbleCode({
  
  #Priors
  Intercept  ~ dnorm(0, sd = 5)

  bm_effect ~ dt(mu=0, sigma=1, df=1)
  dde_effect_m ~ dt(mu=0, sigma=1, df=1) 
  age_effect ~ dt(mu=0, sigma=1, df=1)

  
  time_1_preg_effect ~ dt(mu=0, sigma=1, df=1)

  spei_6_effect ~ dt(mu = 0 , sigma = 1, df = 1)

  
  for(i in 1:n.obs){
    
    #Process Model
    logit(preg_prob[i])<-Intercept+
     
      (est_bm[i]*bm_effect)+
      (dde_m[i]*dde_effect_m)+
      (age[i]*age_effect)+

      (time_1_preg[i]* time_1_preg_effect)+
      (spei_6[i]*spei_6_effect)#+


    
    #Observation Model (Likelihood)
    preg_status[i] ~ dbern(preg_prob[i])
    
    #Impute missing data values
    lac_score[i] ~ dnorm(mean=0,sd=1)

    est_bm[i] ~ dnorm(mean=0,sd=1)

    
    
  }
  
})
parameters <- c(#"lac_effect",
  "bm_effect",
  "dde_effect_m",
  "age_effect",

  "Intercept", 

  "time_1_preg_effect",
  "logProb_preg_status",

  "spei_6_effect"

)

# MCMC Settings
ni <- 400000
nt <- 400
nb <- 200000
nc <- 3

nimble.data = list(
  preg_status = as.numeric(cow_capture_all$time_2_preg) - 1,
  lac_score = as.numeric((cow_capture_all$lac_score)),
  est_bm = as.numeric((cow_capture_all$est_bm)),

  dde_m = as.numeric((cow_capture_all$area_DDE_gt_mean)),
  age = as.numeric((cow_capture_all$age_estimate_2)),

  sex_correct = ifelse(cow_capture_all$sex_correct== "m", 1,0),
  lac_time= as.numeric(cow_capture_all$lac_time), 
  time_1_preg= as.numeric(cow_capture_all$time_1_preg)-1,#

  spei_6 = as.numeric((cow_capture_all$sept_year_prior_6M))#,

  
)

nimble.constants = list(n.obs = nrow(cow_capture_all))


mcmc.output_half_m6 <- nimbleMCMC(code = model_half_m6,
                                  data = nimble.data,
                                  constants=nimble.constants,
                                  monitors = parameters,
                                  niter = ni,
                                  nburnin = nb,
                                  nchains = nc,
                                  thin=nt,
                                  summary=TRUE,
                                  samplesAsCodaMCMC = TRUE)

attach.nimble(mcmc.output_half_m6$samples)


mata_half  <- do.call(rbind, lapply(mcmc.output_half_m6$samples, as.matrix))
lpa_half  <- grep("^logProb_preg_status\\[", colnames(mata_half), value = TRUE)
lla_half  <- mata_half[, lpa_half, drop = FALSE]
stopifnot(all(is.finite(lla_half) | (is.infinite(lla_half) & lla_half < 0)))
loo_half_m6 <- loo(lla_half)

####end model 12#####


####half full################mean dde
model_half_m <-  nimbleCode({
  
  #Priors
  Intercept  ~ dnorm(0, sd = 5)

  bm_effect ~ dt(mu=0, sigma=1, df=1)
  dde_effect_m ~ dt(mu=0, sigma=1, df=1) 
  age_effect ~ dt(mu=0, sigma=1, df=1)

  
  time_1_preg_effect ~ dt(mu=0, sigma=1, df=1)

  spei_12_effect ~ dt(mu = 0 , sigma = 1, df = 1)

  
  for(i in 1:n.obs){
    
    #Process Model
    logit(preg_prob[i])<-Intercept+
     
      (est_bm[i]*bm_effect)+
      (dde_m[i]*dde_effect_m)+
      (age[i]*age_effect)+

      (time_1_preg[i]* time_1_preg_effect)+

      (spei_12[i]*spei_12_effect)
    
    
    
    
    
    
    #Observation Model (Likelihood)
    preg_status[i] ~ dbern(preg_prob[i])
    
    #Impute missing data values
    lac_score[i] ~ dnorm(mean=0,sd=1)

    est_bm[i] ~ dnorm(mean=0,sd=1)

  }
  
})
parameters <- c(
  "bm_effect",
  "dde_effect_m",
  "age_effect",

  "Intercept", 

  "time_1_preg_effect",
  "logProb_preg_status",
  "spei_12_effect"

# MCMC Settings
ni <- 400000
nt <- 400
nb <- 200000
nc <- 3

nimble.data = list(
  preg_status = as.numeric(cow_capture_all$time_2_preg) - 1,
  lac_score = as.numeric((cow_capture_all$lac_score)),
  est_bm = as.numeric((cow_capture_all$est_bm)),

  dde_m = as.numeric((cow_capture_all$area_DDE_gt_mean)),
  age = as.numeric((cow_capture_all$age_estimate_2)),

  sex_correct = ifelse(cow_capture_all$sex_correct== "m", 1,0),
  lac_time= as.numeric(cow_capture_all$lac_time), 
  time_1_preg= as.numeric(cow_capture_all$time_1_preg)-1,#

  spei_12 = as.numeric((cow_capture_all$sept_year_prior_12M))
  
)

nimble.constants = list(n.obs = nrow(cow_capture_all))


mcmc.output_half_m <- nimbleMCMC(code = model_half_m,
                                 data = nimble.data,
                                 constants=nimble.constants,
                                 monitors = parameters,
                                 niter = ni,
                                 nburnin = nb,
                                 nchains = nc,
                                 thin=nt,
                                 summary=TRUE,
                                 samplesAsCodaMCMC = TRUE)

attach.nimble(mcmc.output_half_m$samples)


mata_half  <- do.call(rbind, lapply(mcmc.output_half_m$samples, as.matrix))
lpa_half  <- grep("^logProb_preg_status\\[", colnames(mata_half), value = TRUE)
lla_half  <- mata_half[, lpa_half, drop = FALSE]
stopifnot(all(is.finite(lla_half) | (is.infinite(lla_half) & lla_half < 0)))
loo_half_m <- loo(lla_half)


####end model 13#####


####half full################mean dde, spei_6
model_half_m6_l <-  nimbleCode({
  
  #Priors
  Intercept  ~ dnorm(0, sd = 5)

  bm_effect ~ dt(mu=0, sigma=1, df=1)
  dde_effect_m ~ dt(mu=0, sigma=1, df=1) 
  age_effect ~ dt(mu=0, sigma=1, df=1)
  
  lac_time_effect ~dt(mu=0, sigma=1, df=1)

  
  time_1_preg_effect ~ dt(mu=0, sigma=1, df=1)

  spei_6_effect ~ dt(mu = 0 , sigma = 1, df = 1)
  
  
  for(i in 1:n.obs){
    
    #Process Model
    logit(preg_prob[i])<-Intercept+

      (est_bm[i]*bm_effect)+
      (dde_m[i]*dde_effect_m)+
      (age[i]*age_effect)+

      (lac_time[i]* lac_time_effect)+
      (time_1_preg[i]* time_1_preg_effect)+

      (spei_6[i]*spei_6_effect)

    #Observation Model (Likelihood)
    preg_status[i] ~ dbern(preg_prob[i])
    
    #Impute missing data values
    lac_score[i] ~ dnorm(mean=0,sd=1)

    est_bm[i] ~ dnorm(mean=0,sd=1)

    
    
  }
  
})
parameters <- c(
  "bm_effect",
  "dde_effect_m",
  "age_effect",
  "Intercept", 

  "lac_time_effect",

  "time_1_preg_effect",
  "logProb_preg_status",
  "spei_6_effect"
)

# MCMC Settings
ni <- 400000
nt <- 400
nb <- 200000
nc <- 3

nimble.data = list(
  preg_status = as.numeric(cow_capture_all$time_2_preg) - 1,
  lac_score = as.numeric((cow_capture_all$lac_score)),
  est_bm = as.numeric((cow_capture_all$est_bm)),

  dde_m = as.numeric((cow_capture_all$area_DDE_gt_mean)),
  age = as.numeric((cow_capture_all$age_estimate_2)),

  sex_correct = ifelse(cow_capture_all$sex_correct== "m", 1,0),
  lac_time= as.numeric(cow_capture_all$lac_time), 
  time_1_preg= as.numeric(cow_capture_all$time_1_preg)-1,
  spei_6 = as.numeric((cow_capture_all$sept_year_prior_6M))
  
)

nimble.constants = list(n.obs = nrow(cow_capture_all))


mcmc.output_half_m6_l <- nimbleMCMC(code = model_half_m6_l,
                                    data = nimble.data,
                                    constants=nimble.constants,
                                    monitors = parameters,
                                    niter = ni,
                                    nburnin = nb,
                                    nchains = nc,
                                    thin=nt,
                                    summary=TRUE,
                                    samplesAsCodaMCMC = TRUE)

attach.nimble(mcmc.output_half_m6_l$samples)


mata_half  <- do.call(rbind, lapply(mcmc.output_half_m6_l$samples, as.matrix))
lpa_half  <- grep("^logProb_preg_status\\[", colnames(mata_half), value = TRUE)
lla_half  <- mata_half[, lpa_half, drop = FALSE]
stopifnot(all(is.finite(lla_half) | (is.infinite(lla_half) & lla_half < 0)))
loo_half_m6_l <- loo(lla_half)

####end model 14#####

####half full################mean dde, spei_6
model_half_m6_ls <-  nimbleCode({
  
  #Priors
  Intercept  ~ dnorm(0, sd = 5)

  bm_effect ~ dt(mu=0, sigma=1, df=1)
  dde_effect_m ~ dt(mu=0, sigma=1, df=1) 
  age_effect ~ dt(mu=0, sigma=1, df=1)
  
  lac_time_effect ~dt(mu=0, sigma=1, df=1)
  sex_effect ~ dt(mu=0, sigma=1, df=1)
  
  time_1_preg_effect ~ dt(mu=0, sigma=1, df=1)
  spei_3_effect ~ dt(mu = 0 , sigma = 1, df = 1)
  spei_6_effect ~ dt(mu = 0 , sigma = 1, df = 1)

  
  for(i in 1:n.obs){
    
    #Process Model
    logit(preg_prob[i])<-Intercept+
      (est_bm[i]*bm_effect)+
      (dde_m[i]*dde_effect_m)+
      (age[i]*age_effect)+

      
      (sex_correct[i] * sex_effect) +
      (lac_time[i]* lac_time_effect)+
      (time_1_preg[i]* time_1_preg_effect)+
      (spei_6[i]*spei_6_effect)

    
    
    
    
    
    #Observation Model (Likelihood)
    preg_status[i] ~ dbern(preg_prob[i])
    
    #Impute missing data values
    lac_score[i] ~ dnorm(mean=0,sd=1)

    est_bm[i] ~ dnorm(mean=0,sd=1)

    
    
  }

})
parameters <- c(
  "bm_effect",
  "dde_effect_m",
  "age_effect",

  "Intercept", 

  "lac_time_effect",
  "sex_effect",
  "time_1_preg_effect",
  "logProb_preg_status",
  "spei_6_effect"
)

# MCMC Settings
ni <- 400000
nt <- 400
nb <- 200000
nc <- 3

nimble.data = list(
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
  
  #spei_3 = as.numeric((cow_capture_all$sept_year_prior_3M)),
  spei_6 = as.numeric((cow_capture_all$sept_year_prior_6M))#,
  #spei_12 = as.numeric((cow_capture_all$sept_year_prior_12M))
  
)

nimble.constants = list(n.obs = nrow(cow_capture_all))


mcmc.output_half_m6_ls <- nimbleMCMC(code = model_half_m6_ls,
                                     data = nimble.data,
                                     constants=nimble.constants,
                                     monitors = parameters,
                                     niter = ni,
                                     nburnin = nb,
                                     nchains = nc,
                                     thin=nt,
                                     summary=TRUE,
                                     samplesAsCodaMCMC = TRUE)

attach.nimble(mcmc.output_half_m6_ls$samples)


mata_half  <- do.call(rbind, lapply(mcmc.output_half_m6_ls$samples, as.matrix))
lpa_half  <- grep("^logProb_preg_status\\[", colnames(mata_half), value = TRUE)
lla_half  <- mata_half[, lpa_half, drop = FALSE]
stopifnot(all(is.finite(lla_half) | (is.infinite(lla_half) & lla_half < 0)))
loo_half_m6_ls <- loo(lla_half)


####end model 15#####

####half full################mean dde, spei_6
model_half_m6_ls_nbm <-  nimbleCode({
  
  #Priors
  Intercept  ~ dnorm(0, sd = 5)

  dde_effect_m ~ dt(mu=0, sigma=1, df=1) 
  age_effect ~ dt(mu=0, sigma=1, df=1)
  
  lac_time_effect ~dt(mu=0, sigma=1, df=1)
  sex_effect ~ dt(mu=0, sigma=1, df=1)
  
  time_1_preg_effect ~ dt(mu=0, sigma=1, df=1)
  spei_3_effect ~ dt(mu = 0 , sigma = 1, df = 1)
  spei_6_effect ~ dt(mu = 0 , sigma = 1, df = 1)

  
  
  
  for(i in 1:n.obs){
    
    #Process Model
    logit(preg_prob[i])<-Intercept+

      (dde_m[i]*dde_effect_m)+
      (age[i]*age_effect)+

      
      (sex_correct[i] * sex_effect) +
      (lac_time[i]* lac_time_effect)+
      (time_1_preg[i]* time_1_preg_effect)+
      
      (spei_6[i]*spei_6_effect)#+

    
    
    #Observation Model (Likelihood)
    preg_status[i] ~ dbern(preg_prob[i])
    
    #Impute missing data values
    lac_score[i] ~ dnorm(mean=0,sd=1)

    est_bm[i] ~ dnorm(mean=0,sd=1)

    
    
  }
  
})
parameters <- c(
  "dde_effect_m",
  "age_effect",

  "Intercept", 

  "lac_time_effect",
  "sex_effect",
  "time_1_preg_effect",
  "logProb_preg_status",
  
  "spei_6_effect"

# MCMC Settings
ni <- 400000
nt <- 400
nb <- 200000
nc <- 3

nimble.data = list(
  preg_status = as.numeric(cow_capture_all$time_2_preg) - 1,
  lac_score = as.numeric((cow_capture_all$lac_score)),

  dde_m = as.numeric((cow_capture_all$area_DDE_gt_mean)),
  age = as.numeric((cow_capture_all$age_estimate_2)),

  sex_correct = ifelse(cow_capture_all$sex_correct== "m", 1,0),
  lac_time= as.numeric(cow_capture_all$lac_time), 
  time_1_preg= as.numeric(cow_capture_all$time_1_preg)-1,
  spei_6 = as.numeric((cow_capture_all$sept_year_prior_6M))#,

  
)

nimble.constants = list(n.obs = nrow(cow_capture_all))


mcmc.output_half_m6_ls_nbm <- nimbleMCMC(code = model_half_m6_ls_nbm,
                                         data = nimble.data,
                                         constants=nimble.constants,
                                         monitors = parameters,
                                         niter = ni,
                                         nburnin = nb,
                                         nchains = nc,
                                         thin=nt,
                                         summary=TRUE,
                                         samplesAsCodaMCMC = TRUE)

attach.nimble(mcmc.output_half_m6_ls_nbm$samples)


mata_half  <- do.call(rbind, lapply(mcmc.output_half_m6_ls_nbm$samples, as.matrix))
lpa_half  <- grep("^logProb_preg_status\\[", colnames(mata_half), value = TRUE)
lla_half  <- mata_half[, lpa_half, drop = FALSE]
stopifnot(all(is.finite(lla_half) | (is.infinite(lla_half) & lla_half < 0)))
loo_half_m6_ls_bm <- loo(lla_half)


####end model 16#####


####half full################mean dde, spei_6
model_half_m6_ls_nbm_a2 <-  nimbleCode({
  
  #Priors

  dde_effect_m ~ dt(mu=0, sigma=1, df=1) 
  age_effect ~ dt(mu=0, sigma=1, df=1)
  
  lac_time_effect ~dt(mu=0, sigma=1, df=1)
  sex_effect ~ dt(mu=0, sigma=1, df=1)
  
  time_1_preg_effect ~ dt(mu=0, sigma=1, df=1)
  spei_3_effect ~ dt(mu = 0 , sigma = 1, df = 1)
  spei_6_effect ~ dt(mu = 0 , sigma = 1, df = 1)
  age2_effect ~ dt(mu = 0, sigma = 1, df = 1)

  
  
  
  for(i in 1:n.obs){
    
    #Process Model
    logit(preg_prob[i])<-Intercept+

      (dde_m[i]*dde_effect_m)+
      (age[i]*age_effect)+
      (pow(age[i], 2)*age2_effect)+
      
      (sex_correct[i] * sex_effect) +
      (lac_time[i]* lac_time_effect)+
      (time_1_preg[i]* time_1_preg_effect)+

      (spei_6[i]*spei_6_effect)
    
    
    #Observation Model (Likelihood)
    preg_status[i] ~ dbern(preg_prob[i])
    
    #Impute missing data values
    lac_score[i] ~ dnorm(mean=0,sd=1)

    est_bm[i] ~ dnorm(mean=0,sd=1)
    
    
  }
  
})
parameters <- c(
  "dde_effect_m",
  "age_effect",
  "age2_effect",
  
  "Intercept", 
  
  "lac_time_effect",
  "sex_effect",
  "time_1_preg_effect",
  "logProb_preg_status",
 
  "spei_6_effect"
)

# MCMC Settings
ni <- 400000
nt <- 400
nb <- 200000
nc <- 3

nimble.data = list(
  preg_status = as.numeric(cow_capture_all$time_2_preg) - 1,
  lac_score = as.numeric((cow_capture_all$lac_score)),

  dde_m = as.numeric((cow_capture_all$area_DDE_gt_mean)),
  age = as.numeric((cow_capture_all$age_estimate_2)),
 
  sex_correct = ifelse(cow_capture_all$sex_correct== "m", 1,0),
  lac_time= as.numeric(cow_capture_all$lac_time), 
  time_1_preg= as.numeric(cow_capture_all$time_1_preg)-1,#

  spei_6 = as.numeric((cow_capture_all$sept_year_prior_6M))
  
)

nimble.constants = list(n.obs = nrow(cow_capture_all))


mcmc.output_half_m6_ls_nbm_a2 <- nimbleMCMC(code = model_half_m6_ls_nbm_a2,
                                            data = nimble.data,
                                            constants=nimble.constants,
                                            monitors = parameters,
                                            niter = ni,
                                            nburnin = nb,
                                            nchains = nc,
                                            thin=nt,
                                            summary=TRUE,
                                            samplesAsCodaMCMC = TRUE)

attach.nimble(mcmc.output_half_m6_ls_nbm_a2$samples)


mata_half  <- do.call(rbind, lapply(mcmc.output_half_m6_ls_nbm_a2$samples, as.matrix))
lpa_half  <- grep("^logProb_preg_status\\[", colnames(mata_half), value = TRUE)
lla_half  <- mata_half[, lpa_half, drop = FALSE]
stopifnot(all(is.finite(lla_half) | (is.infinite(lla_half) & lla_half < 0)))
loo_half_m6_ls_bm_a2 <- loo(lla_half)


####end model 17#####


####half full################mean dde, spei_6
model_half_m6_a2 <-  nimbleCode({
  
  #Priors
  Intercept  ~ dnorm(0, sd = 5)

  bm_effect ~ dt(mu=0, sigma=1, df=1)
  dde_effect_m ~ dt(mu=0, sigma=1, df=1) 
  age_effect ~ dt(mu=0, sigma=1, df=1)

  time_1_preg_effect ~ dt(mu=0, sigma=1, df=1)

  spei_6_effect ~ dt(mu = 0 , sigma = 1, df = 1)

  age2_effect ~ dt(mu = 0, sigma = 1, df = 1)

  
  
  
  for(i in 1:n.obs){
    
    #Process Model
    logit(preg_prob[i])<-Intercept+
      #(lac_score[i]*lac_effect)+
      (est_bm[i]*bm_effect)+
      (dde_m[i]*dde_effect_m)+
      (age[i]*age_effect)+
      (pow(age[i], 2)*age2_effect)+

      (time_1_preg[i]* time_1_preg_effect)+
 
      (spei_6[i]*spei_6_effect)

    
    #Observation Model (Likelihood)
    preg_status[i] ~ dbern(preg_prob[i])
    
    #Impute missing data values
    lac_score[i] ~ dnorm(mean=0,sd=1)

    est_bm[i] ~ dnorm(mean=0,sd=1)

    
    
    
  }#i
  
})#model5
parameters <- c(#"lac_effect",
  "bm_effect",
  "dde_effect_m",
  "age_effect",
  "age2_effect",

  "Intercept", 

  "time_1_preg_effect",
  "logProb_preg_status",
  "spei_6_effect"
)

# MCMC Settings
ni <- 400000
nt <- 400
nb <- 200000
nc <- 3

nimble.data = list(
  preg_status = as.numeric(cow_capture_all$time_2_preg) - 1,
  lac_score = as.numeric((cow_capture_all$lac_score)),
  est_bm = as.numeric((cow_capture_all$est_bm)),

  dde_m = as.numeric((cow_capture_all$area_DDE_gt_mean)),
  age = as.numeric((cow_capture_all$age_estimate_2)),

  sex_correct = ifelse(cow_capture_all$sex_correct== "m", 1,0),
  lac_time= as.numeric(cow_capture_all$lac_time), 
  time_1_preg= as.numeric(cow_capture_all$time_1_preg)-1,
  spei_6 = as.numeric((cow_capture_all$sept_year_prior_6M))
  
)

nimble.constants = list(n.obs = nrow(cow_capture_all))


mcmc.output_half_m6_a2 <- nimbleMCMC(code = model_half_m6_a2,
                                     data = nimble.data,
                                     constants=nimble.constants,
                                     monitors = parameters,
                                     niter = ni,
                                     nburnin = nb,
                                     nchains = nc,
                                     thin=nt,
                                     summary=TRUE,
                                     samplesAsCodaMCMC = TRUE)

attach.nimble(mcmc.output_half_m6_a2$samples)


mata_half  <- do.call(rbind, lapply(mcmc.output_half_m6_a2$samples, as.matrix))
lpa_half  <- grep("^logProb_preg_status\\[", colnames(mata_half), value = TRUE)
lla_half  <- mata_half[, lpa_half, drop = FALSE]
stopifnot(all(is.finite(lla_half) | (is.infinite(lla_half) & lla_half < 0)))
loo_half_m6_a2 <- loo(lla_half)



##########################end model 18


####half full################mean dde, spei_6
model_half_m6_s <-  nimbleCode({
  
  #Priors
  Intercept  ~ dnorm(0, sd = 5)

  bm_effect ~ dt(mu=0, sigma=1, df=1)
  dde_effect_m ~ dt(mu=0, sigma=1, df=1) 
  age_effect ~ dt(mu=0, sigma=1, df=1)

  sex_effect ~ dt(mu=0, sigma=1, df=1)
  
  time_1_preg_effect ~ dt(mu=0, sigma=1, df=1)

  spei_6_effect ~ dt(mu = 0 , sigma = 1, df = 1)

  age2_effect ~ dt(mu = 0, sigma = 1, df = 1)

  
  for(i in 1:n.obs){
    
    #Process Model
    logit(preg_prob[i])<-Intercept+

      (est_bm[i]*bm_effect)+
      (dde_m[i]*dde_effect_m)+
      (age[i]*age_effect)+

      
      (sex_correct[i] * sex_effect) +

      (time_1_preg[i]* time_1_preg_effect)+

      (spei_6[i]*spei_6_effect)
    
    
    
    
    
    #Observation Model (Likelihood)
    preg_status[i] ~ dbern(preg_prob[i])
    
    #Impute missing data values
    lac_score[i] ~ dnorm(mean=0,sd=1)

    est_bm[i] ~ dnorm(mean=0,sd=1)

    
    
  }
  
})
parameters <- c
  "bm_effect",
  "dde_effect_m",
  "age_effect",

  "Intercept", 

  "sex_effect",
  "time_1_preg_effect",
  "logProb_preg_status",
  "spei_6_effect"
)

# MCMC Settings
ni <- 400000
nt <- 400
nb <- 200000
nc <- 3

nimble.data = list(
  preg_status = as.numeric(cow_capture_all$time_2_preg) - 1,
  lac_score = as.numeric((cow_capture_all$lac_score)),
  est_bm = as.numeric((cow_capture_all$est_bm)),

  dde_m = as.numeric((cow_capture_all$area_DDE_gt_mean)),
  age = as.numeric((cow_capture_all$age_estimate_2)),

  sex_correct = ifelse(cow_capture_all$sex_correct== "m", 1,0),
  lac_time= as.numeric(cow_capture_all$lac_time), 
  time_1_preg= as.numeric(cow_capture_all$time_1_preg)-1,#

  spei_6 = as.numeric((cow_capture_all$sept_year_prior_6M))
  
)

nimble.constants = list(n.obs = nrow(cow_capture_all))


mcmc.output_half_m6_s <- nimbleMCMC(code = model_half_m6_s,
                                    data = nimble.data,
                                    constants=nimble.constants,
                                    monitors = parameters,
                                    niter = ni,
                                    nburnin = nb,
                                    nchains = nc,
                                    thin=nt,
                                    summary=TRUE,
                                    samplesAsCodaMCMC = TRUE)

attach.nimble(mcmc.output_half_m6_s$samples)


mata_half  <- do.call(rbind, lapply(mcmc.output_half_m6_s$samples, as.matrix))
lpa_half  <- grep("^logProb_preg_status\\[", colnames(mata_half), value = TRUE)
lla_half  <- mata_half[, lpa_half, drop = FALSE]
stopifnot(all(is.finite(lla_half) | (is.infinite(lla_half) & lla_half < 0)))
loo_half_m6_s <- loo(lla_half)



##########################end model 19

####half full################mean dde, spei_6
model_half_m6_sb <-  nimbleCode({
  
  #Priors
  Intercept  ~ dnorm(0, sd = 5)

  dde_effect_m ~ dt(mu=0, sigma=1, df=1) 
  age_effect ~ dt(mu=0, sigma=1, df=1)
  time_1_preg_effect ~ dt(mu=0, sigma=1, df=1)

  spei_6_effect ~ dt(mu = 0 , sigma = 1, df = 1)

  
  
  for(i in 1:n.obs){
    
    #Process Model
    logit(preg_prob[i])<-Intercept+

      (dde_m[i]*dde_effect_m)+
      (age[i]*age_effect)+

      (time_1_preg[i]* time_1_preg_effect)+

      (spei_6[i]*spei_6_effect)
    
    
    
    
    #Observation Model (Likelihood)
    preg_status[i] ~ dbern(preg_prob[i])
    
    #Impute missing data values
    lac_score[i] ~ dnorm(mean=0,sd=1)


    
  }
  
})
parameters <- c(
  "dde_effect_m",
  "age_effect",

  "Intercept", 

  "time_1_preg_effect",
  "logProb_preg_status",
  "spei_6_effect"#,

)

# MCMC Settings
ni <- 400000
nt <- 400
nb <- 200000
nc <- 3

nimble.data = list(
  preg_status = as.numeric(cow_capture_all$time_2_preg) - 1,
  lac_score = as.numeric((cow_capture_all$lac_score)),

  dde_m = as.numeric((cow_capture_all$area_DDE_gt_mean)),
  age = as.numeric((cow_capture_all$age_estimate_2)),
  sex_correct = ifelse(cow_capture_all$sex_correct== "m", 1,0),
  lac_time= as.numeric(cow_capture_all$lac_time), 
  time_1_preg= as.numeric(cow_capture_all$time_1_preg)-1,
  spei_6 = as.numeric((cow_capture_all$sept_year_prior_6M))#,

  
)

nimble.constants = list(n.obs = nrow(cow_capture_all))


mcmc.output_half_m6_sb <- nimbleMCMC(code = model_half_m6_sb,
                                     data = nimble.data,
                                     constants=nimble.constants,
                                     monitors = parameters,
                                     niter = ni,
                                     nburnin = nb,
                                     nchains = nc,
                                     thin=nt,
                                     summary=TRUE,
                                     samplesAsCodaMCMC = TRUE)

attach.nimble(mcmc.output_half_m6$samples)


mata_half  <- do.call(rbind, lapply(mcmc.output_half_m6_sb$samples, as.matrix))
lpa_half  <- grep("^logProb_preg_status\\[", colnames(mata_half), value = TRUE)
lla_half  <- mata_half[, lpa_half, drop = FALSE]
stopifnot(all(is.finite(lla_half) | (is.infinite(lla_half) & lla_half < 0)))
loo_half_m6_sb <- loo(lla_half)



####end model 20#####



####half full################mean dde, spei_6
model_half_m6_sb_bd <-  nimbleCode({
  
  #Priors
  Intercept  ~ dnorm(0, sd = 5)

  dde_effect_m ~ dt(mu=0, sigma=1, df=1) 
  age_effect ~ dt(mu=0, sigma=1, df=1)
  date_effect~ dt(mu=0, sigma=1, df=1)

  
  time_1_preg_effect ~ dt(mu=0, sigma=1, df=1)

  spei_6_effect ~ dt(mu = 0 , sigma = 1, df = 1)

  
  for(i in 1:n.obs){
    
    #Process Model
    logit(preg_prob[i])<-Intercept+

      (dde_m[i]*dde_effect_m)+
      (age[i]*age_effect)+

      (calving_date[i] *date_effect )+

      (time_1_preg[i]* time_1_preg_effect)+
      (spei_6[i]*spei_6_effect)
    
    
    
    #Observation Model (Likelihood)
    preg_status[i] ~ dbern(preg_prob[i])
    
    #Impute missing data values
    lac_score[i] ~ dnorm(mean=0,sd=1)

  }
  
})

parameters <- c(
  "dde_effect_m",
  "age_effect",
  "date_effect",
  "Intercept", 
  "time_1_preg_effect",
  "logProb_preg_status",
  "spei_6_effect"
)

# MCMC Settings
ni <- 400000
nt <- 400
nb <- 200000
nc <- 3

nimble.data = list(
  preg_status = as.numeric(cow_capture_all$time_2_preg) - 1,
  lac_score = as.numeric((cow_capture_all$lac_score)),

  dde_m = as.numeric((cow_capture_all$area_DDE_gt_mean)),
  age = as.numeric((cow_capture_all$age_estimate_2)),
 
  sex_correct = ifelse(cow_capture_all$sex_correct== "m", 1,0),
  lac_time= as.numeric(cow_capture_all$lac_time), 
  time_1_preg= as.numeric(cow_capture_all$time_1_preg)-1,
  calving_date = as.numeric(cow_capture_all$birt_date),
  
  spei_6 = as.numeric((cow_capture_all$sept_year_prior_6M))

  
)

nimble.constants = list(n.obs = nrow(cow_capture_all))


mcmc.output_half_m6_sb_bd <- nimbleMCMC(code = model_half_m6_sb_bd,
                                        data = nimble.data,
                                        constants=nimble.constants,
                                        monitors = parameters,
                                        niter = ni,
                                        nburnin = nb,
                                        nchains = nc,
                                        thin=nt,
                                        summary=TRUE,
                                        samplesAsCodaMCMC = TRUE)

attach.nimble(mcmc.output_half_m6_sb_bd$samples)


mata_half  <- do.call(rbind, lapply(mcmc.output_half_m6_sb_bd$samples, as.matrix))
lpa_half  <- grep("^logProb_preg_status\\[", colnames(mata_half), value = TRUE)
lla_half  <- mata_half[, lpa_half, drop = FALSE]
stopifnot(all(is.finite(lla_half) | (is.infinite(lla_half) & lla_half < 0)))
loo_half_m6_sb_bd <- loo(lla_half)



####end model 21#####

model_half_m6_lac_bd <-  nimbleCode({
  
  #Priors
  Intercept  ~ dnorm(0, sd = 5)

  dde_effect_m ~ dt(mu=0, sigma=1, df=1) 
  age_effect ~ dt(mu=0, sigma=1, df=1)
  date_effect~ dt(mu=0, sigma=1, df=1)

  lac_time_effect ~dt(mu=0, sigma=1, df=1)

  
  time_1_preg_effect ~ dt(mu=0, sigma=1, df=1)

  spei_6_effect ~ dt(mu = 0 , sigma = 1, df = 1)

  for(i in 1:n.obs){
    
    #Process Model
    logit(preg_prob[i])<-Intercept+

      (dde_m[i]*dde_effect_m)+
      (age[i]*age_effect)+

      (calving_date[i] *date_effect )+

      (lac_time[i]* lac_time_effect)+
      (time_1_preg[i]* time_1_preg_effect)+

      (spei_6[i]*spei_6_effect)
    
    
    
    
    
    
    #Observation Model (Likelihood)
    preg_status[i] ~ dbern(preg_prob[i])
    
    #Impute missing data values
    lac_score[i] ~ dnorm(mean=0,sd=1)

  }
  
})

parameters <- c(
  "dde_effect_m",
  "age_effect",
  "date_effect",
  "Intercept", 
  "lac_time_effect",
  "time_1_preg_effect",
  "logProb_preg_status",
  "spei_6_effect"
)

# MCMC Settings
ni <- 400000
nt <- 400
nb <- 200000
nc <- 3

nimble.data = list(
  preg_status = as.numeric(cow_capture_all$time_2_preg) - 1,
  lac_score = as.numeric((cow_capture_all$lac_score)),
  dde_m = as.numeric((cow_capture_all$area_DDE_gt_mean)),
  age = as.numeric((cow_capture_all$age_estimate_2)),
  sex_correct = ifelse(cow_capture_all$sex_correct== "m", 1,0),
  lac_time= as.numeric(cow_capture_all$lac_time), 
  time_1_preg= as.numeric(cow_capture_all$time_1_preg)-1,
  calving_date = as.numeric(cow_capture_all$birt_date),
  spei_6 = as.numeric((cow_capture_all$sept_year_prior_6M))
  
)

nimble.constants = list(n.obs = nrow(cow_capture_all))


mcmc.output_half_m6_lac_bd<- nimbleMCMC(code = model_half_m6_lac_bd,
                                        data = nimble.data,
                                        constants=nimble.constants,
                                        monitors = parameters,
                                        niter = ni,
                                        nburnin = nb,
                                        nchains = nc,
                                        thin=nt,
                                        summary=TRUE,
                                        samplesAsCodaMCMC = TRUE)

attach.nimble(mcmc.output_half_m6_lac_bd$samples)


mata_half  <- do.call(rbind, lapply(mcmc.output_half_m6_lac_bd$samples, as.matrix))
lpa_half  <- grep("^logProb_preg_status\\[", colnames(mata_half), value = TRUE)
lla_half  <- mata_half[, lpa_half, drop = FALSE]
stopifnot(all(is.finite(lla_half) | (is.infinite(lla_half) & lla_half < 0)))
loo_half_m6_lac_bd<- loo(lla_half)


####end model 22#####


model_half_m6_lac_X_bd <-  nimbleCode({
  
  #Priors
  Intercept  ~ dnorm(0, sd = 5)

  dde_effect_m ~ dt(mu=0, sigma=1, df=1) 
  age_effect ~ dt(mu=0, sigma=1, df=1)
  date_effect~ dt(mu=0, sigma=1, df=1)
  lac_time_effect ~dt(mu=0, sigma=1, df=1)
  time_1_preg_effect ~ dt(mu=0, sigma=1, df=1)
  spei_6_effect ~ dt(mu = 0 , sigma = 1, df = 1)
  date_lac_effect ~ dt(mu = 0, sigma = 1, df = 1)
  
  
  for(i in 1:n.obs){
    
    #Process Model
    logit(preg_prob[i])<-Intercept+

      (dde_m[i]*dde_effect_m)+
      (age[i]*age_effect)+
      (calving_date[i] *date_effect )+
      (lac_time[i]* lac_time_effect)+
      (time_1_preg[i]* time_1_preg_effect)+
      (spei_6[i]*spei_6_effect)+
      (calving_date[i]*lac_time[i]*date_lac_effect)
    
    
    
    
    
    
    #Observation Model (Likelihood)
    preg_status[i] ~ dbern(preg_prob[i])
    
    #Impute missing data values
    lac_score[i] ~ dnorm(mean=0,sd=1)

    
  }
  
})
parameters <- c(
  "dde_effect_m",
  "age_effect",
  "date_effect",
  "date_lac_effect",
  "Intercept", 
  "lac_time_effect",
  "time_1_preg_effect",
  "logProb_preg_status",
  "spei_6_effect"
)

# MCMC Settings
ni <- 400000
nt <- 400
nb <- 200000
nc <- 3

nimble.data = list(
  preg_status = as.numeric(cow_capture_all$time_2_preg) - 1,
  lac_score = as.numeric((cow_capture_all$lac_score)),

  dde_m = as.numeric((cow_capture_all$area_DDE_gt_mean)),
  age = as.numeric((cow_capture_all$age_estimate_2)),

  sex_correct = ifelse(cow_capture_all$sex_correct== "m", 1,0),
  lac_time= as.numeric(cow_capture_all$lac_time), 
  time_1_preg= as.numeric(cow_capture_all$time_1_preg)-1,#
  calving_date = as.numeric(cow_capture_all$birt_date),

  spei_6 = as.numeric((cow_capture_all$sept_year_prior_6M))
  )

nimble.constants = list(n.obs = nrow(cow_capture_all))


mcmc.output_half_m6_lac_X_bd<- nimbleMCMC(code = model_half_m6_lac_X_bd,
                                          data = nimble.data,
                                          constants=nimble.constants,
                                          monitors = parameters,
                                          niter = ni,
                                          nburnin = nb,
                                          nchains = nc,
                                          thin=nt,
                                          summary=TRUE,
                                          samplesAsCodaMCMC = TRUE)

attach.nimble(mcmc.output_half_m6_lac_X_bd$samples)


mata_half  <- do.call(rbind, lapply(mcmc.output_half_m6_lac_X_bd$samples, as.matrix))
lpa_half  <- grep("^logProb_preg_status\\[", colnames(mata_half), value = TRUE)
lla_half  <- mata_half[, lpa_half, drop = FALSE]
stopifnot(all(is.finite(lla_half) | (is.infinite(lla_half) & lla_half < 0)))
loo_half_m6_lac_X_bd<- loo(lla_half)


####end model 23

####half full################mean dde, spei_6
model_half_m6_sb_bd_s <-  nimbleCode({
  
  #Priors
  Intercept  ~ dnorm(0, sd = 5)

  dde_effect_m ~ dt(mu=0, sigma=1, df=1) 
  age_effect ~ dt(mu=0, sigma=1, df=1)
  date_effect~ dt(mu=0, sigma=1, df=1)
  sex_effect ~ dt(mu=0, sigma=1, df=1)
  time_1_preg_effect ~ dt(mu=0, sigma=1, df=1)
  spei_6_effect ~ dt(mu = 0 , sigma = 1, df = 1)

  
  
  for(i in 1:n.obs){
    
    #Process Model
    logit(preg_prob[i])<-Intercept+
      (dde_m[i]*dde_effect_m)+
      (age[i]*age_effect)+
      (calving_date[i] *date_effect )+
      (sex_correct[i] * sex_effect) +
      (time_1_preg[i]* time_1_preg_effect)+
      (spei_6[i]*spei_6_effect)
    
    
    #Observation Model (Likelihood)
    preg_status[i] ~ dbern(preg_prob[i])
    
    #Impute missing data values
    lac_score[i] ~ dnorm(mean=0,sd=1)

    
  }
  
})
parameters <- c(
  "dde_effect_m",
  "age_effect",
  "date_effect",
  "Intercept", 
  "sex_effect",
  "time_1_preg_effect",
  "logProb_preg_status",
  "spei_6_effect"
)

# MCMC Settings
ni <- 400000
nt <- 400
nb <- 200000
nc <- 3

nimble.data = list(
  preg_status = as.numeric(cow_capture_all$time_2_preg) - 1,
  lac_score = as.numeric((cow_capture_all$lac_score)),
  dde_m = as.numeric((cow_capture_all$area_DDE_gt_mean)),
  age = as.numeric((cow_capture_all$age_estimate_2)),
  sex_correct = ifelse(cow_capture_all$sex_correct== "m", 1,0),
  lac_time= as.numeric(cow_capture_all$lac_time), 
  time_1_preg= as.numeric(cow_capture_all$time_1_preg)-1,
  calving_date = as.numeric(cow_capture_all$birt_date),
  spei_6 = as.numeric((cow_capture_all$sept_year_prior_6M))
)

nimble.constants = list(n.obs = nrow(cow_capture_all))


mcmc.output_half_m6_sb_bd_s <- nimbleMCMC(code = model_half_m6_sb_bd_s,
                                          data = nimble.data,
                                          constants=nimble.constants,
                                          monitors = parameters,
                                          niter = ni,
                                          nburnin = nb,
                                          nchains = nc,
                                          thin=nt,
                                          summary=TRUE,
                                          samplesAsCodaMCMC = TRUE)

attach.nimble(mcmc.output_half_m6_sb_bd_s$samples)


mata_half  <- do.call(rbind, lapply(mcmc.output_half_m6_sb_bd_s$samples, as.matrix))
lpa_half  <- grep("^logProb_preg_status\\[", colnames(mata_half), value = TRUE)
lla_half  <- mata_half[, lpa_half, drop = FALSE]
stopifnot(all(is.finite(lla_half) | (is.infinite(lla_half) & lla_half < 0)))
loo_half_m6_sb_bd_s <- loo(lla_half)

####end model 24#####

####half full################mean dde, spei_6
model_half_m6_bd_cm <-  nimbleCode({
  
  #Priors
  Intercept  ~ dnorm(0, sd = 5)
  dde_effect_m ~ dt(mu=0, sigma=1, df=1) 
  age_effect ~ dt(mu=0, sigma=1, df=1)
  date_effect~ dt(mu=0, sigma=1, df=1)
  c_mass_effect ~ dt(mu = 0 , sigma = 1, df = 1)
  time_1_preg_effect ~ dt(mu=0, sigma=1, df=1)
  spei_6_effect ~ dt(mu = 0 , sigma = 1, df = 1)

  
  for(i in 1:n.obs){
    
    #Process Model
    logit(preg_prob[i])<-Intercept+
      (dde_m[i]*dde_effect_m)+
      (c_mass[i] * c_mass_effect)+
      (age[i]*age_effect)+
      (calving_date[i] *date_effect )++
      (time_1_preg[i]* time_1_preg_effect)+
      (spei_6[i]*spei_6_effect)

    
    #Observation Model (Likelihood)
    preg_status[i] ~ dbern(preg_prob[i])
    
    #Impute missing data values
    lac_score[i] ~ dnorm(mean=0,sd=1)

    
  }
  
})
parameters <- c(
  "dde_effect_m",
  "age_effect",
  "date_effect",
  "c_mass_effect",
  "Intercept", 
  "time_1_preg_effect",
  "logProb_preg_status",
  "spei_6_effect"
)

# MCMC Settings
ni <- 400000
nt <- 400
nb <- 200000
nc <- 3

nimble.data = list(
  preg_status = as.numeric(cow_capture_all$time_2_preg) - 1,
  lac_score = as.numeric((cow_capture_all$lac_score)),
  c_mass = as.numeric((cow_capture_all$mass_calf)),
  dde_m = as.numeric((cow_capture_all$area_DDE_gt_mean)),
  age = as.numeric((cow_capture_all$age_estimate_2)),
  lac_time= as.numeric(cow_capture_all$lac_time), 
  time_1_preg= as.numeric(cow_capture_all$time_1_preg)-1,#
  calving_date = as.numeric(cow_capture_all$birt_date),
  spei_6 = as.numeric((cow_capture_all$sept_year_prior_6M))
  
)

nimble.constants = list(n.obs = nrow(cow_capture_all))


mcmc.output_half_m6_bd_cm<- nimbleMCMC(code = model_half_m6_bd_cm,
                                       data = nimble.data,
                                       constants=nimble.constants,
                                       monitors = parameters,
                                       niter = ni,
                                       nburnin = nb,
                                       nchains = nc,
                                       thin=nt,
                                       summary=TRUE,
                                       samplesAsCodaMCMC = TRUE)

attach.nimble(mcmc.output_half_m6_bd_cm$samples)


mata_half  <- do.call(rbind, lapply(mcmc.output_half_m6_bd_cm$samples, as.matrix))
lpa_half  <- grep("^logProb_preg_status\\[", colnames(mata_half), value = TRUE)
lla_half  <- mata_half[, lpa_half, drop = FALSE]
stopifnot(all(is.finite(lla_half) | (is.infinite(lla_half) & lla_half < 0)))
loo_half_m6_bd_cm <- loo(lla_half)


####end model 25#####

####half full################mean dde, spei_6
model_half_m6_sb_bd_axpreg <-  nimbleCode({
  
  #Priors
  Intercept  ~ dnorm(0, sd = 5)

  dde_effect_m ~ dt(mu=0, sigma=1, df=1) 
  age_effect ~ dt(mu=0, sigma=1, df=1)
  date_effect~ dt(mu=0, sigma=1, df=1)
  time_1_preg_effect ~ dt(mu=0, sigma=1, df=1)
  spei_6_effect ~ dt(mu = 0 , sigma = 1, df = 1)
  lac_score_preg_effect ~ dt(mu = 0, sigma = 1, df = 1)
  age_preg_effect ~ dt(mu = 0, sigma = 1, df = 1)

  
  for(i in 1:n.obs){
    
    #Process Model
    logit(preg_prob[i])<-Intercept+
      (dde_m[i]*dde_effect_m)+
      (age[i]*age_effect)+
      (calving_date[i] *date_effect )+
      (time_1_preg[i]* time_1_preg_effect)+
      (spei_6[i]*spei_6_effect)+
      (age[i]*time_1_preg[i]*age_preg_effect)
    
    
    
    #Observation Model (Likelihood)
    preg_status[i] ~ dbern(preg_prob[i])
    
    #Impute missing data values
    lac_score[i] ~ dnorm(mean=0,sd=1)

    
    
  }
  
})
parameters <- c(
  "dde_effect_m",
  "age_effect",
  "date_effect",
  "age_preg_effect",
  "Intercept",
  "time_1_preg_effect",
  "logProb_preg_status",
  "spei_6_effect"
)

# MCMC Settings
ni <- 400000
nt <- 400
nb <- 200000
nc <- 3

nimble.data = list(
  preg_status = as.numeric(cow_capture_all$time_2_preg) - 1,
  lac_score = as.numeric((cow_capture_all$lac_score)),
  dde_m = as.numeric((cow_capture_all$area_DDE_gt_mean)),
  age = as.numeric((cow_capture_all$age_estimate_2)),
  sex_correct = ifelse(cow_capture_all$sex_correct== "m", 1,0),
  lac_time= as.numeric(cow_capture_all$lac_time), 
  time_1_preg= as.numeric(cow_capture_all$time_1_preg)-1,
  calving_date = as.numeric(cow_capture_all$birt_date),
  spei_6 = as.numeric((cow_capture_all$sept_year_prior_6M))
)

nimble.constants = list(n.obs = nrow(cow_capture_all))


mcmc.output_half_m6_sb_bd_axpreg <- nimbleMCMC(code = model_half_m6_sb_bd_axpreg ,
                                               data = nimble.data,
                                               constants=nimble.constants,
                                               monitors = parameters,
                                               niter = ni,
                                               nburnin = nb,
                                               nchains = nc,
                                               thin=nt,
                                               summary=TRUE,
                                               samplesAsCodaMCMC = TRUE)

attach.nimble(mcmc.output_half_m6_sb_bd_axpreg $samples)


mata_half  <- do.call(rbind, lapply(mcmc.output_half_m6_sb_bd_axpreg $samples, as.matrix))
lpa_half  <- grep("^logProb_preg_status\\[", colnames(mata_half), value = TRUE)
lla_half  <- mata_half[, lpa_half, drop = FALSE]
stopifnot(all(is.finite(lla_half) | (is.infinite(lla_half) & lla_half < 0)))
loo_half_m6_sb_bd_axpreg  <- loo(lla_half)

##end model 26

####half full################mean dde, spei_6
model_half_m6_sb_bd_bm_age <-  nimbleCode({
  
  #Priors
  Intercept  ~ dnorm(0, sd = 5)
  bm_effect ~ dt(mu=0, sigma=1, df=1)
  dde_effect_m ~ dt(mu=0, sigma=1, df=1) 
  age_effect ~ dt(mu=0, sigma=1, df=1)
  date_effect~ dt(mu=0, sigma=1, df=1)
  time_1_preg_effect ~ dt(mu=0, sigma=1, df=1)
  spei_6_effect ~ dt(mu = 0 , sigma = 1, df = 1)
  lac_score_preg_effect ~ dt(mu = 0, sigma = 1, df = 1)
  age_bm_effect ~ dt(mu = 0, sigma = 1, df = 1)
  
  for(i in 1:n.obs){
    
    #Process Model
    logit(preg_prob[i])<-Intercept+
      (est_bm[i]*bm_effect)+
      (dde_m[i]*dde_effect_m)+
      (age[i]*age_effect)+
      (calving_date[i] *date_effect )+
      (time_1_preg[i]* time_1_preg_effect)+
      (spei_6[i]*spei_6_effect)+
      (age[i]*est_bm[i]*age_bm_effect)
    
    
    
    #Observation Model (Likelihood)
    preg_status[i] ~ dbern(preg_prob[i])
    
    #Impute missing data values
    lac_score[i] ~ dnorm(mean=0,sd=1)
    est_bm[i] ~ dnorm(mean=0,sd=1)

    
  }
  
})
parameters <- c(
  "bm_effect",
  "dde_effect_m",
  "age_effect",
  "date_effect",
  "age_bm_effect",
  "Intercept", 
  "time_1_preg_effect",
  "logProb_preg_status",
  "spei_6_effect"
)

# MCMC Settings
ni <- 400000
nt <- 400
nb <- 200000
nc <- 3

nimble.data = list(
  preg_status = as.numeric(cow_capture_all$time_2_preg) - 1,
  lac_score = as.numeric((cow_capture_all$lac_score)),
  est_bm = as.numeric((cow_capture_all$est_bm)),

  dde_m = as.numeric((cow_capture_all$area_DDE_gt_mean)),
  age = as.numeric((cow_capture_all$age_estimate_2)),
  sex_correct = ifelse(cow_capture_all$sex_correct== "m", 1,0),
  lac_time= as.numeric(cow_capture_all$lac_time), 
  time_1_preg= as.numeric(cow_capture_all$time_1_preg)-1,
  calving_date = as.numeric(cow_capture_all$birt_date),
  spei_6 = as.numeric((cow_capture_all$sept_year_prior_6M))
  
)

nimble.constants = list(n.obs = nrow(cow_capture_all))


mcmc.output_half_m6_sb_bd_bm_age <- nimbleMCMC(code = model_half_m6_sb_bd_bm_age,
                                               data = nimble.data,
                                               constants=nimble.constants,
                                               monitors = parameters,
                                               niter = ni,
                                               nburnin = nb,
                                               nchains = nc,
                                               thin=nt,
                                               summary=TRUE,
                                               samplesAsCodaMCMC = TRUE)

attach.nimble(mcmc.output_half_m6_sb_bd_bm_age$samples)


mata_half  <- do.call(rbind, lapply(mcmc.output_half_m6_sb_bd_bm_age$samples, as.matrix))
lpa_half  <- grep("^logProb_preg_status\\[", colnames(mata_half), value = TRUE)
lla_half  <- mata_half[, lpa_half, drop = FALSE]
stopifnot(all(is.finite(lla_half) | (is.infinite(lla_half) & lla_half < 0)))
loo_half_m6_sb_bd_bm_age <- loo(lla_half)


##end model 27
####half full################mean dde, spei_6
model_half_m6_sb_bd_bm_preg <-  nimbleCode({
  
  #Priors
  Intercept  ~ dnorm(0, sd = 5)
  
  bm_effect ~ dt(mu=0, sigma=1, df=1)
  dde_effect_m ~ dt(mu=0, sigma=1, df=1) 
  age_effect ~ dt(mu=0, sigma=1, df=1)
  date_effect~ dt(mu=0, sigma=1, df=1)
  
  time_1_preg_effect ~ dt(mu=0, sigma=1, df=1)
  spei_6_effect ~ dt(mu = 0 , sigma = 1, df = 1)
  lac_score_preg_effect ~ dt(mu = 0, sigma = 1, df = 1)
  preg_bm_effect ~ dt(mu = 0, sigma = 1, df = 1)
  
  
  
  for(i in 1:n.obs){
    
    #Process Model
    logit(preg_prob[i])<-Intercept+
      (est_bm[i]*bm_effect)+
      (dde_m[i]*dde_effect_m)+
      (age[i]*age_effect)+
      (calving_date[i] *date_effect )+
      (time_1_preg[i]* time_1_preg_effect)+
      (spei_6[i]*spei_6_effect)+
      (time_1_preg[i]*est_bm[i]*preg_bm_effect)
    
    
    
    #Observation Model (Likelihood)
    preg_status[i] ~ dbern(preg_prob[i])
    
    #Impute missing data values
    lac_score[i] ~ dnorm(mean=0,sd=1)
    est_bm[i] ~ dnorm(mean=0,sd=1)

    
  }

})
parameters <- c(
  "bm_effect",
  "dde_effect_m",
  "age_effect",
  "date_effect",
  "preg_bm_effect",
  "Intercept", 
  "time_1_preg_effect",
  "logProb_preg_status",
  "spei_6_effect"
)

# MCMC Settings
ni <- 400000
nt <- 400
nb <- 200000
nc <- 3

nimble.data = list(
  preg_status = as.numeric(cow_capture_all$time_2_preg) - 1,
  lac_score = as.numeric((cow_capture_all$lac_score)),
  est_bm = as.numeric((cow_capture_all$est_bm)),
  dde_m = as.numeric((cow_capture_all$area_DDE_gt_mean)),
  age = as.numeric((cow_capture_all$age_estimate_2)),
  sex_correct = ifelse(cow_capture_all$sex_correct== "m", 1,0),
  lac_time= as.numeric(cow_capture_all$lac_time), 
  time_1_preg= as.numeric(cow_capture_all$time_1_preg)-1,
  calving_date = as.numeric(cow_capture_all$birt_date),
  spei_6 = as.numeric((cow_capture_all$sept_year_prior_6M))
  
)

nimble.constants = list(n.obs = nrow(cow_capture_all))


mcmc.output_half_m6_sb_bd_bm_preg  <- nimbleMCMC(code = model_half_m6_sb_bd_bm_preg ,
                                                 data = nimble.data,
                                                 constants=nimble.constants,
                                                 monitors = parameters,
                                                 niter = ni,
                                                 nburnin = nb,
                                                 nchains = nc,
                                                 thin=nt,
                                                 summary=TRUE,
                                                 samplesAsCodaMCMC = TRUE)

attach.nimble(mcmc.output_half_m6_sb_bd_bm_preg $samples)


mata_half  <- do.call(rbind, lapply(mcmc.output_half_m6_sb_bd_bm_preg $samples, as.matrix))
lpa_half  <- grep("^logProb_preg_status\\[", colnames(mata_half), value = TRUE)
lla_half  <- mata_half[, lpa_half, drop = FALSE]
stopifnot(all(is.finite(lla_half) | (is.infinite(lla_half) & lla_half < 0)))
loo_half_m6_sb_bd_bm_preg  <- loo(lla_half)

##end model 28


##TOP MODEL#################

####half full################mean dde, spei_6
model_half_add_cm <-  nimbleCode({
  
  #Priors
  Intercept  ~ dnorm(0, sd = 5)

  bm_effect ~ dt(mu=0, sigma=1, df=1)
  dde_effect_m ~ dt(mu=0, sigma=1, df=1) 
  age_effect ~ dt(mu=0, sigma=1, df=1)
  date_effect~ dt(mu=0, sigma=1, df=1)
  c_m_effect~ dt(mu=0, sigma=1, df=1)
  time_1_preg_effect ~ dt(mu=0, sigma=1, df=1)
  spei_6_effect ~ dt(mu = 0 , sigma = 1, df = 1)
  lac_score_preg_effect ~ dt(mu = 0, sigma = 1, df = 1)
  age_bm_effect ~ dt(mu = 0, sigma = 1, df = 1)

  
  
  
  for(i in 1:n.obs){
    
    #Process Model
    logit(preg_prob[i])<-Intercept+
      (est_bm[i]*bm_effect)+
      (dde_m[i]*dde_effect_m)+
      (age[i]*age_effect)+
      (calf_mass[i]* c_m_effect)+
      (calving_date[i] *date_effect )+
      (time_1_preg[i]* time_1_preg_effect)+
      (spei_6[i]*spei_6_effect)+
      (age[i]*est_bm[i]*age_bm_effect)
    
 
    
    #Observation Model (Likelihood)
    preg_status[i] ~ dbern(preg_prob[i])
    
    #Impute missing data values
    lac_score[i] ~ dnorm(mean=0,sd=1)

    est_bm[i] ~ dnorm(mean=0,sd=1)

  }
  
})
parameters <- c("c_m_effect",
                "bm_effect",
                "dde_effect_m",
                "age_effect",
                "date_effect",
                "age_bm_effect",

                "Intercept", 
                "time_1_preg_effect",
                "logProb_preg_status",
                "spei_6_effect"
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

  dde_m = as.numeric((cow_capture_all$area_DDE_gt_mean)),
  age = as.numeric((cow_capture_all$age_estimate_2)),

  sex_correct = ifelse(cow_capture_all$sex_correct== "m", 1,0),
  lac_time= as.numeric(cow_capture_all$lac_time), 
  time_1_preg= as.numeric(cow_capture_all$time_1_preg)-1,
  calving_date = as.numeric(cow_capture_all$birt_date),
  spei_6 = as.numeric((cow_capture_all$sept_year_prior_6M))
  
)

nimble.constants = list(n.obs = nrow(cow_capture_all))


mcmc.output_half_add_cm<- nimbleMCMC(code = model_half_add_cm,
                                     data = nimble.data,
                                     constants=nimble.constants,
                                     monitors = parameters,
                                     niter = ni,
                                     nburnin = nb,
                                     nchains = nc,
                                     thin=nt,
                                     summary=TRUE,
                                     samplesAsCodaMCMC = TRUE)

attach.nimble(mcmc.output_half_add_cm$samples)


mata_half  <- do.call(rbind, lapply(mcmc.output_half_add_cm$samples, as.matrix))
lpa_half  <- grep("^logProb_preg_status\\[", colnames(mata_half), value = TRUE)
lla_half  <- mata_half[, lpa_half, drop = FALSE]
stopifnot(all(is.finite(lla_half) | (is.infinite(lla_half) & lla_half < 0)))
loo_half_add_cm <- loo(lla_half)

##TOP MODEL

attach.nimble(mcmc.output_half_add_cm$samples)

MCMCtrace(object = mcmc.output_half_add_cm$samples,
          pdf = FALSE, # no export to PDF
          ind = TRUE, # separate density lines per chain
          params = "time_1_preg_effect")
# Gelman-Rubin diagnostic (AKA RHat or PSRF)
gelman.diag(mcmc.output_half_add_cm$samples, multivariate = FALSE)



mcmcplot(mcmc.output_half_add_cm$samples)



##how to get effects size
##jow to make the differnt charts in PP
##how to get time on deck chart
##uses zscore to preduct value. Use mean for all the other values
post_mat <- as.matrix(mcmc.output_half_add_cm$samples)
# 2. Specify the parameters you care about
params <- c(#"lac_effect",
  "c_m_effect",
  "bm_effect",
  "dde_effect_m",
  "age_effect",
  "date_effect",
  "age_bm_effect",
  "Intercept", 
  "time_1_preg_effect",
  "spei_6_effect"
)

# 3. Summarize each one
summary_df_half_add_cm <- tibble(parameter = params) %>%
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

print(summary_df_half_add_cm)
get_variables(mcmc.output_half_add_cm$samples)

##end model 29


####half full################mean dde, spei_6
model_half_add_xx <-  nimbleCode({
  
  #Priors
  Intercept  ~ dnorm(0, sd = 5)

  bm_effect ~ dt(mu=0, sigma=1, df=1)
  dde_effect_m ~ dt(mu=0, sigma=1, df=1) 
  age_effect ~ dt(mu=0, sigma=1, df=1)
  date_effect~ dt(mu=0, sigma=1, df=1)
  c_m_effect~ dt(mu=0, sigma=1, df=1)

  sex_effect ~ dt(mu=0, sigma=1, df=1)
  
  time_1_preg_effect ~ dt(mu=0, sigma=1, df=1)
  spei_6_effect ~ dt(mu = 0 , sigma = 1, df = 1)

  lac_score_preg_effect ~ dt(mu = 0, sigma = 1, df = 1)
  age_bm_effect ~ dt(mu = 0, sigma = 1, df = 1)

  
  
  
  for(i in 1:n.obs){
    
    #Process Model
    logit(preg_prob[i])<-Intercept+
      (est_bm[i]*bm_effect)+
      (dde_m[i]*dde_effect_m)+
      (age[i]*age_effect)+
      (calf_mass[i]* c_m_effect)+
      (calving_date[i] *date_effect )+
      (sex_correct[i] * sex_effect) +
      (time_1_preg[i]* time_1_preg_effect)+
      (spei_6[i]*spei_6_effect)+
      (age[i]*est_bm[i]*age_bm_effect)
    

    
    #Observation Model (Likelihood)
    preg_status[i] ~ dbern(preg_prob[i])
    
    #Impute missing data values
    lac_score[i] ~ dnorm(mean=0,sd=1)

    est_bm[i] ~ dnorm(mean=0,sd=1)

    
  }
  
})
parameters <- c("c_m_effect",
                "bm_effect",
                "dde_effect_m",
                "age_effect",
                "date_effect",
                "age_bm_effect",
                "Intercept", 
                "sex_effect",
                "time_1_preg_effect",
                "logProb_preg_status",
                "spei_6_effect"
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
  dde_m = as.numeric((cow_capture_all$area_DDE_gt_mean)),
  age = as.numeric((cow_capture_all$age_estimate_2)),
  sex_correct = ifelse(cow_capture_all$sex_correct== "m", 1,0),
  lac_time= as.numeric(cow_capture_all$lac_time), 
  time_1_preg= as.numeric(cow_capture_all$time_1_preg)-1,
  calving_date = as.numeric(cow_capture_all$birt_date),
  spei_6 = as.numeric((cow_capture_all$sept_year_prior_6M))
  
)

nimble.constants = list(n.obs = nrow(cow_capture_all))


mcmc.output_half_add_xx<- nimbleMCMC(code = model_half_add_xx,
                                     data = nimble.data,
                                     constants=nimble.constants,
                                     monitors = parameters,
                                     niter = ni,
                                     nburnin = nb,
                                     nchains = nc,
                                     thin=nt,
                                     summary=TRUE,
                                     samplesAsCodaMCMC = TRUE)

attach.nimble(mcmc.output_half_add_xx$samples)


mata_half  <- do.call(rbind, lapply(mcmc.output_half_add_xx$samples, as.matrix))
lpa_half  <- grep("^logProb_preg_status\\[", colnames(mata_half), value = TRUE)
lla_half  <- mata_half[, lpa_half, drop = FALSE]
stopifnot(all(is.finite(lla_half) | (is.infinite(lla_half) & lla_half < 0)))
loo_half_add_xx <- loo(lla_half)

############################################Global######################
###full model DDE, SPEI 6 month, time_1_preg, calf_sex, lac time, age, BM, lac_score

model_full_global <-  nimbleCode({
  
  #Priors
  Intercept  ~ dnorm(0, sd = 5)
  
  lac_effect~ dt(mu=0, sigma=1, df=1)
  bm_effect ~ dt(mu=0, sigma=1, df=1)
  dde_effect_m ~ dt(mu=0, sigma=1, df=1) 
  age_effect ~ dt(mu=0, sigma=1, df=1)
  date_effect~dt(mu=0, sigma=1, df=1)
  lac_time_effect ~dt(mu=0, sigma=1, df=1)
  sex_effect ~ dt(mu=0, sigma=1, df=1)
  precip_effect ~ dt(mu=0, sigma=1, df=1)
  time_1_preg_effect ~ dt(mu=0, sigma=1, df=1)
  spei_6_effect ~ dt(mu = 0 , sigma = 1, df = 1)
  age2_effect ~ dt(mu = 0, sigma = 1, df = 1)
  sex_lac_effect ~ dt(mu = 0, sigma = 1, df = 1)
  c_mass_effect ~ dt(mu = 0 , sigma = 1, df = 1)

  
  
  for(i in 1:n.obs){
    
    #Process Model
    logit(preg_prob[i])<-Intercept+
      (lac_score[i]*lac_effect)+
      (est_bm[i]*bm_effect)+
      (dde_95[i]*dde_effect_m)+
      (age[i]*age_effect)+
      (precip_year_prior[i] * precip_effect)+
      (pow(age[i], 2)*age2_effect)+
      (c_mass[i] * c_mass_effect)+
      (sex_correct[i] * sex_effect) +
      (lac_time[i]* lac_time_effect)+
      (time_1_preg[i]* time_1_preg_effect)+
      (calving_date[i] *date_effect )+

      (spei_6[i]*spei_6_effect)

    
    
    #Observation Model (Likelihood)
    preg_status[i] ~ dbern(preg_prob[i])
    
    #Impute missing data values
    lac_score[i] ~ dnorm(mean=0,sd=1)
    precip_year_prior[i] ~ dnorm(mean = 0, sd = 1)
    dde_95[i] ~ dnorm(mean=0,sd=1)
    age[i] ~ dnorm(mean=0,sd=1)
    est_bm[i] ~ dnorm(mean=0,sd=1)
    
    lac_time[i] ~ dnorm(mean=0,sd=1)
    spei_6[i] ~ dnorm(mean = 0, sd = 1)

  }
  
})
parameters <- c("lac_effect",
                "bm_effect",
                "dde_effect_m",
                "age_effect",
                "age2_effect",
                "precip_effect",
                "Intercept", 
                "date_effect",
                "lac_time_effect",
                "sex_effect",
                "time_1_preg_effect",
                "logProb_preg_status",
                "c_mass_effect",
                "spei_6_effect"
)

# MCMC Settings
ni <- 400000
nt <- 400
nb <- 200000
nc <- 3

nimble.data = list(
  preg_status = as.numeric(cow_capture_all$time_2_preg) - 1,
  lac_score = as.numeric((cow_capture_all$lac_score)),
  est_bm = as.numeric((cow_capture_all$est_bm)),
  dde_95 = as.numeric((cow_capture_all$area_DDE_gt_mean)),
  age = as.numeric((cow_capture_all$age_estimate_2)),
  precip_year_prior = as.numeric((cow_capture_all$total_year_prior)),
  sex_correct = ifelse(cow_capture_all$sex_correct== "m", 1,0),
  lac_time= as.numeric(cow_capture_all$lac_time), 
  time_1_preg= as.numeric(cow_capture_all$time_1_preg)-1,
  c_mass =  as.numeric(cow_capture_all$mass_calf),
  calving_date = as.numeric(cow_capture_all$birt_date),
  spei_6 = as.numeric((cow_capture_all$sept_year_prior_6M))
  
)

nimble.constants = list(n.obs = nrow(cow_capture_all))


mcmc.output_global<- nimbleMCMC(code = model_full_global,
                                data = nimble.data,
                                constants=nimble.constants,
                                monitors = parameters,
                                niter = ni,
                                nburnin = nb,
                                nchains = nc,
                                thin=nt,
                                summary=TRUE,
                                samplesAsCodaMCMC = TRUE)
beep(sound = 3)

attach.nimble(mcmc.output_global$samples)


mata_full <- do.call(rbind, lapply(mcmc.output_global$samples, as.matrix))
lpa_full <- grep("^logProb_preg_status\\[", colnames(mata_full), value = TRUE)
lla_full  <- mata_full[, lpa_full, drop = FALSE]
stopifnot(all(is.finite(lla_full) | (is.infinite(lla_full) & lla_full < 0)))
loo_global <- loo(lla_full)



##########################################################global

###top model plus age
####half full################mean dde, spei_6
model_half_add_a2 <-  nimbleCode({
  
  #Priors
  Intercept  ~ dnorm(0, sd = 5)

  bm_effect ~ dt(mu=0, sigma=1, df=1)
  dde_effect_m ~ dt(mu=0, sigma=1, df=1) 
  age_effect ~ dt(mu=0, sigma=1, df=1)
  date_effect~ dt(mu=0, sigma=1, df=1)
  c_m_effect~ dt(mu=0, sigma=1, df=1)
  sex_effect ~ dt(mu=0, sigma=1, df=1)
  time_1_preg_effect ~ dt(mu=0, sigma=1, df=1)
  spei_6_effect ~ dt(mu = 0 , sigma = 1, df = 1)
  age2_effect ~ dt(mu = 0, sigma = 1, df = 1)
  lac_score_preg_effect ~ dt(mu = 0, sigma = 1, df = 1)
  age_bm_effect ~ dt(mu = 0, sigma = 1, df = 1)
  
  
  
  for(i in 1:n.obs){
    
    #Process Model
    logit(preg_prob[i])<-Intercept+
      (est_bm[i]*bm_effect)+
      (dde_m[i]*dde_effect_m)+
      (age[i]*age_effect)+
      (calf_mass[i]* c_m_effect)+
      (pow(age[i], 2)*age2_effect)+
      (calving_date[i] *date_effect )+
      (sex_correct[i] * sex_effect) +
      (time_1_preg[i]* time_1_preg_effect)+
      (spei_6[i]*spei_6_effect)+
      (age[i]*est_bm[i]*age_bm_effect)
    

    
    #Observation Model (Likelihood)
    preg_status[i] ~ dbern(preg_prob[i])
    
    #Impute missing data values
    lac_score[i] ~ dnorm(mean=0,sd=1)

    est_bm[i] ~ dnorm(mean=0,sd=1)

    
  }
  
})
parameters <- c("c_m_effect",
                "bm_effect",
                "dde_effect_m",
                "age_effect",
                "date_effect",
                "age_bm_effect",
                "age2_effect",
                "Intercept", 
                "sex_effect",
                "time_1_preg_effect",
                "logProb_preg_status",
                "spei_6_effect"

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
  dde_m = as.numeric((cow_capture_all$area_DDE_gt_mean)),
  age = as.numeric((cow_capture_all$age_estimate_2)),
  sex_correct = ifelse(cow_capture_all$sex_correct== "m", 1,0),
  lac_time= as.numeric(cow_capture_all$lac_time), 
  time_1_preg= as.numeric(cow_capture_all$time_1_preg)-1,
  calving_date = as.numeric(cow_capture_all$birt_date),

  spei_6 = as.numeric((cow_capture_all$sept_year_prior_6M))#,

  
)

nimble.constants = list(n.obs = nrow(cow_capture_all))


mcmc.output_half_add_a2<- nimbleMCMC(code = model_half_add_a2,
                                     data = nimble.data,
                                     constants=nimble.constants,
                                     monitors = parameters,
                                     niter = ni,
                                     nburnin = nb,
                                     nchains = nc,
                                     thin=nt,
                                     summary=TRUE,
                                     samplesAsCodaMCMC = TRUE)

attach.nimble(mcmc.output_half_add_a2$samples)


mata_half  <- do.call(rbind, lapply(mcmc.output_half_add_a2$samples, as.matrix))
lpa_half  <- grep("^logProb_preg_status\\[", colnames(mata_half), value = TRUE)
lla_half  <- mata_half[, lpa_half, drop = FALSE]
stopifnot(all(is.finite(lla_half) | (is.infinite(lla_half) & lla_half < 0)))
loo_half_add_a2 <- loo(lla_half)




##############################



loo_compare(
  loo_calf, 
  loo_all_dde, 
  loo_all_spei,
  loo_full,
  loo_calf_lac, #5
  loo_half,
  loo_pl,
  loo_calf_spei,
  loo_full_spei3,
  loo_full_spei6, #10
  loo_half_75, 
  loo_half_m,
  loo_half_m6,
  loo_half_m6_l,
  loo_half_m6_ls,#15
  loo_half_m6_ls_bm, 
  loo_half_m6_ls_bm_a2,
  loo_half_m6_s,
  loo_half_m6_sb,
  loo_half_m6_sb_bd, #20
  loo_half_m6_lac_bd,#21
  loo_half_m6_lac_bd,
  loo_half_m6_lac_X_bd,
  loo_half_m6_sb_bd_s,
  loo_half_m6_bd_cm,#25
  loo_half_m6_sb_bd_axpreg,
  loo_half_m6_sb_bd_bm_age,#27
  loo_half_m6_sb_bd_bm_preg,
  loo_half_add_cm,#1
  loo_half_add_xx,
  loo_global,#31
  loo_half_add_a2
)

