# Download & Install Nimble here: https://r-nimble.org/download
# Make sure you have XCode installed on Mac (https://r-nimble.org/html_manual/cha-installing-nimble.html)
# or RTools on Windows: https://cran.r-project.org/bin/windows/Rtools/

#Make sure all of the below packages are installed:
##this code read in cow capture data and covaraties. 
  ###creates The logistic process model for “Predicted at Capture” 

rm(list=ls())#
rm()
gc()
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


cow_capture_all_1<-read_csv("outputs/cleaned_up_data_set/captured_cows_public.csv")
names(cow_capture_all_1)


cow_capture_all<-cow_capture_all_1 %>% 
  select(-c(`VIT Placed`)) %>% 
  mutate(Preg_lab = as.factor(Preg_lab),
         capture_year = as.factor(capture_year),
         across(
           .cols = c(where(is.numeric)#,
                     #-age_estimate_2
           ),  # optional: if you want to keep raw years unscaled
           .fns = ~ as.numeric(scale(.))
         ))
names(cow_capture_all) <- names(cow_capture_all) %>%
  gsub("[ ,()/]", "_", .) %>%       # Replace space, comma, parentheses with underscore
  gsub("_+", "_", .) %>%           # Collapse multiple underscores into one
  gsub("_$", "", .)   
str(cow_capture_all)
names(cow_capture_all)

set.seed(123)

###mean dde
model_spei_3 <-  nimbleCode({
  
  #Priors
  Intercept  ~ dnorm(0, sd = 5)

  lac_effect~ dt(mu=0, sigma=1, df=1)
  bm_effect ~ dt(mu=0, sigma=1, df=1)
  dde_effect_m ~ dt(mu=0, sigma=1, df=1) 
  age_effect ~ dt(mu=0, sigma=1, df=1)
  precip_effect ~ dt(mu=0, sigma=1, df=1)
  bod_con_effect ~ dt(mu=0, sigma=1, df=1)
  spei_3_effect ~ dt(mu = 0 , sigma = 1, df = 1)

  
  
  for(i in 1:n.obs){
    
    #Process Model
    logit(preg_prob[i])<-Intercept+
      (lac_score[i]*lac_effect)+
      (est_bm[i]*bm_effect)+
      (dde_95[i]*dde_effect_m)+
      (age[i]*age_effect)+
      (precip_year_prior[i]*precip_effect)+
      (est_body_cond[i]*bod_con_effect)+
      (spei_3[i]*spei_3_effect)
    
    #Observation Model (Likelihood)
    preg_status[i] ~ dbern(preg_prob[i])
    
    #Impute missing data values
    lac_score[i] ~ dnorm(mean=0,sd=1)
    precip_year_prior[i] ~ dnorm(mean = 0, sd = 1)
    spei_3[i] ~ dnorm(mean = 0, sd = 1)
    
    dde_95[i] ~ dnorm(mean=0,sd=1)
    age[i] ~ dnorm(mean=0,sd=1)
    est_body_cond[i] ~ dnorm(mean=0,sd=1)
    est_bm[i] ~ dnorm(mean=0,sd=1)
  }#i
  
})#model5

parameters <- c("lac_effect","bm_effect",
                "dde_effect_m","age_effect",
                "precip_effect", "Intercept", "bod_con_effect",
                "logProb_preg_status", "spei_3_effect"
)

# MCMC Settings
ni <- 400000
nt <- 400
nb <- 200000
nc <- 3

nimble.data = list(
  preg_status = as.numeric(cow_capture_all$Preg_lab) - 1,
  lac_score = as.numeric((cow_capture_all$lac_score)),
  est_bm = as.numeric((cow_capture_all$est_bm)),
  dde_95 = as.numeric((cow_capture_all$area_DDE_gt_mean)),
  est_body_cond=as.numeric((cow_capture_all$IFBF)),
  age = as.numeric((cow_capture_all$age_estimate_2)),
  precip_year_prior = as.numeric((cow_capture_all$total_year_prior)),
  spei_3 = as.numeric((cow_capture_all$sept_year_prior_3M))
)

nimble.constants = list(n.obs = nrow(cow_capture_all))


mcmc.output_spei_3 <- nimbleMCMC(code = model_spei_3,
                                 data = nimble.data,
                                 constants=nimble.constants,
                                 monitors = parameters,
                                 niter = ni,
                                 nburnin = nb,
                                 nchains = nc,
                                 thin=nt,
                                 summary=TRUE,
                                 samplesAsCodaMCMC = TRUE)

attach.nimble(mcmc.output_spei_3$samples)

## SPEI-3
mat3  <- do.call(rbind, lapply(mcmc.output_spei_3$samples, as.matrix))
lp3   <- grep("^logProb_preg_status\\[", colnames(mat3), value = TRUE)
ll3   <- mat3[, lp3, drop = FALSE]             # draws x observations
# optional sanity check
stopifnot(all(is.finite(ll3) | (is.infinite(ll3) & ll3 < 0)))
loo_spei_3 <- loo(ll3)




#################Model spei_6########################################

###mean dde
model_spei_6 <-  nimbleCode({
  
  #Priors
  Intercept  ~ dnorm(0, sd = 5)

  
  lac_effect~ dt(mu=0, sigma=1, df=1)
  bm_effect ~ dt(mu=0, sigma=1, df=1)
  dde_effect_m ~ dt(mu=0, sigma=1, df=1) 
  age_effect ~ dt(mu=0, sigma=1, df=1)
  precip_effect ~ dt(mu=0, sigma=1, df=1)
  bod_con_effect ~ dt(mu=0, sigma=1, df=1)
  #spei_3_effect ~ dt(mu = 0 , sigma = 1, df = 1)
  spei_6_effect ~ dt(mu = 0 , sigma = 1, df = 1)
  # spei_12_effect ~ dt(mu = 0 , sigma = 1, df = 1)
  
  
  for(i in 1:n.obs){
    
    #Process Model
    logit(preg_prob[i])<-Intercept+
      (lac_score[i]*lac_effect)+
      (est_bm[i]*bm_effect)+
      (dde_95[i]*dde_effect_m)+
      (age[i]*age_effect)+
      (precip_year_prior[i]*precip_effect)+
      (est_body_cond[i]*bod_con_effect)+
     
      (spei_6[i]*spei_6_effect)
  
    
    #Observation Model (Likelihood)
    preg_status[i] ~ dbern(preg_prob[i])
    
    #Impute missing data values
    lac_score[i] ~ dnorm(mean=0,sd=1)
    precip_year_prior[i] ~ dnorm(mean = 0, sd = 1)
    
    spei_6[i] ~ dnorm(mean = 0, sd = 1)
   
    dde_95[i] ~ dnorm(mean=0,sd=1)
    age[i] ~ dnorm(mean=0,sd=1)
    est_body_cond[i] ~ dnorm(mean=0,sd=1)
    est_bm[i] ~ dnorm(mean=0,sd=1)
  }#i
  
})#model5

parameters <- c("lac_effect","bm_effect",
                "dde_effect_m","age_effect",
                "precip_effect", "Intercept", "bod_con_effect",
                "logProb_preg_status", 
                #"spei_3_effect"#,
                "spei_6_effect"#, 
                # "spei_12_effect"
)

# MCMC Settings
ni <- 400000
nt <- 400
nb <- 200000
nc <- 3

nimble.data = list(
  preg_status = as.numeric(cow_capture_all$Preg_lab) - 1,
  lac_score = as.numeric((cow_capture_all$lac_score)),
  est_bm = as.numeric((cow_capture_all$est_bm)),
  dde_95 = as.numeric((cow_capture_all$area_DDE_gt_mean)),
  est_body_cond=as.numeric((cow_capture_all$IFBF)),
  age = as.numeric((cow_capture_all$age_estimate_2)),
  precip_year_prior = as.numeric((cow_capture_all$total_year_prior)),
  
  spei_6 = as.numeric((cow_capture_all$sept_year_prior_6M))
  
)

nimble.constants = list(n.obs = nrow(cow_capture_all))


mcmc.output_spei_6 <- nimbleMCMC(code = model_spei_6,
                                 data = nimble.data,
                                 constants=nimble.constants,
                                 monitors = parameters,
                                 niter = ni,
                                 nburnin = nb,
                                 nchains = nc,
                                 thin=nt,
                                 summary=TRUE,
                                 samplesAsCodaMCMC = TRUE)

attach.nimble(mcmc.output_spei_6$samples)

mat6  <- do.call(rbind, lapply(mcmc.output_spei_6$samples, as.matrix))
lp6   <- grep("^logProb_preg_status\\[", colnames(mat6), value = TRUE)
ll6   <- mat6[, lp6, drop = FALSE]
stopifnot(all(is.finite(ll6) | (is.infinite(ll6) & ll6 < 0)))
loo_spei_6 <- loo(ll6)






###########################Model SPEI 12##############################################

#################Model spei_6########################################

###mean dde
model_spei_12 <-  nimbleCode({
  
  #Priors
  Intercept  ~ dnorm(0, sd = 5)

  
  lac_effect~ dt(mu=0, sigma=1, df=1)
  bm_effect ~ dt(mu=0, sigma=1, df=1)
  dde_effect_m ~ dt(mu=0, sigma=1, df=1) 
  age_effect ~ dt(mu=0, sigma=1, df=1)
  precip_effect ~ dt(mu=0, sigma=1, df=1)
  bod_con_effect ~ dt(mu=0, sigma=1, df=1)

  spei_12_effect ~ dt(mu = 0 , sigma = 1, df = 1)
  
  
  for(i in 1:n.obs){
    
    #Process Model
    logit(preg_prob[i])<-Intercept+
      (lac_score[i]*lac_effect)+
      (est_bm[i]*bm_effect)+
      (dde_95[i]*dde_effect_m)+
      (age[i]*age_effect)+
      (precip_year_prior[i]*precip_effect)+
      (est_body_cond[i]*bod_con_effect)+

      (spei_12[i]*spei_12_effect)
    
    
    
    #Observation Model (Likelihood)
    preg_status[i] ~ dbern(preg_prob[i])
    
    #Impute missing data values
    lac_score[i] ~ dnorm(mean=0,sd=1)
    precip_year_prior[i] ~ dnorm(mean = 0, sd = 1)

    spei_12[i] ~ dnorm(mean = 0, sd = 1)
    dde_95[i] ~ dnorm(mean=0,sd=1)
    age[i] ~ dnorm(mean=0,sd=1)
    est_body_cond[i] ~ dnorm(mean=0,sd=1)
    est_bm[i] ~ dnorm(mean=0,sd=1)
  }#i
  
})#model5

parameters <- c("lac_effect","bm_effect",
                "dde_effect_m","age_effect",
                "precip_effect", "Intercept", "bod_con_effect",
                "logProb_preg_status", 

                "spei_12_effect"
)

# MCMC Settings
ni <- 400000
nt <- 400
nb <- 200000
nc <- 3

nimble.data = list(
  preg_status = as.numeric(cow_capture_all$Preg_lab) - 1,
  lac_score = as.numeric((cow_capture_all$lac_score)),
  est_bm = as.numeric((cow_capture_all$est_bm)),
  dde_95 = as.numeric((cow_capture_all$area_DDE_gt_mean)),
  est_body_cond=as.numeric((cow_capture_all$IFBF)),
  age = as.numeric((cow_capture_all$age_estimate_2)),
  precip_year_prior = as.numeric((cow_capture_all$total_year_prior)),

  spei_12 = as.numeric((cow_capture_all$sept_year_prior_12M))

)

nimble.constants = list(n.obs = nrow(cow_capture_all))


mcmc.output_spei_12 <- nimbleMCMC(code = model_spei_12,
                                  data = nimble.data,
                                  constants=nimble.constants,
                                  monitors = parameters,
                                  niter = ni,
                                  nburnin = nb,
                                  nchains = nc,
                                  thin=nt,
                                  summary=TRUE,
                                  samplesAsCodaMCMC = TRUE)

attach.nimble(mcmc.output_spei_12$samples)

mat12  <- do.call(rbind, lapply(mcmc.output_spei_12$samples, as.matrix))
lp12   <- grep("^logProb_preg_status\\[", colnames(mat12), value = TRUE)
ll12   <- mat12[, lp12, drop = FALSE]
stopifnot(all(is.finite(ll12) | (is.infinite(ll12) & ll12 < 0)))
loo_spei_12 <- loo(ll12)



#############################################################################################
#######################model with different DDE#############################################
###########################################################################################
##DDE 75th
##SPEI 12
model_dde_75 <-  nimbleCode({
  
  #Priors
  Intercept  ~ dnorm(0, sd = 5)
  

  
  lac_effect~ dt(mu=0, sigma=1, df=1)
  bm_effect ~ dt(mu=0, sigma=1, df=1)
  dde_effect_m ~ dt(mu=0, sigma=1, df=1) 
  age_effect ~ dt(mu=0, sigma=1, df=1)
  precip_effect ~ dt(mu=0, sigma=1, df=1)
  bod_con_effect ~ dt(mu=0, sigma=1, df=1)

  spei_12_effect ~ dt(mu = 0 , sigma = 1, df = 1)
  
  
  for(i in 1:n.obs){
    
    #Process Model
    logit(preg_prob[i])<-Intercept+
      (lac_score[i]*lac_effect)+
      (est_bm[i]*bm_effect)+
      (dde_95[i]*dde_effect_m)+
      (age[i]*age_effect)+
      (precip_year_prior[i]*precip_effect)+
      (est_body_cond[i]*bod_con_effect)+
    
      (spei_12[i]*spei_12_effect)
    
    
    #Observation Model (Likelihood)
    preg_status[i] ~ dbern(preg_prob[i])
    
    #Impute missing data values
    lac_score[i] ~ dnorm(mean=0,sd=1)
    precip_year_prior[i] ~ dnorm(mean = 0, sd = 1)

    spei_12[i] ~ dnorm(mean = 0, sd = 1)
    dde_95[i] ~ dnorm(mean=0,sd=1)
    age[i] ~ dnorm(mean=0,sd=1)
    est_body_cond[i] ~ dnorm(mean=0,sd=1)
    est_bm[i] ~ dnorm(mean=0,sd=1)
  }#i
  
})#model5

parameters <- c("lac_effect","bm_effect",
                "dde_effect_m","age_effect",
                "precip_effect", "Intercept", "bod_con_effect",
                "logProb_preg_status", 
                "spei_12_effect"
)

# MCMC Settings
ni <- 400000
nt <- 400
nb <- 200000
nc <- 3

nimble.data = list(
  preg_status = as.numeric(cow_capture_all$Preg_lab) - 1,
  lac_score = as.numeric((cow_capture_all$lac_score)),
  est_bm = as.numeric((cow_capture_all$est_bm)),
  dde_95 = as.numeric((cow_capture_all$area_DDE_gt_75)),
  est_body_cond=as.numeric((cow_capture_all$IFBF)),
  age = as.numeric((cow_capture_all$age_estimate_2)),
  precip_year_prior = as.numeric((cow_capture_all$total_year_prior)),

  spei_12 = as.numeric((cow_capture_all$sept_year_prior_12M))
  
)

nimble.constants = list(n.obs = nrow(cow_capture_all))


mcmc.output_dde_75 <- nimbleMCMC(code = model_dde_75,
                                 data = nimble.data,
                                 constants=nimble.constants,
                                 monitors = parameters,
                                 niter = ni,
                                 nburnin = nb,
                                 nchains = nc,
                                 thin=nt,
                                 summary=TRUE,
                                 samplesAsCodaMCMC = TRUE)

attach.nimble(mcmc.output_dde_75$samples)

mat75  <- do.call(rbind, lapply(mcmc.output_dde_75$samples, as.matrix))
lp75  <- grep("^logProb_preg_status\\[", colnames(mat75), value = TRUE)
ll75   <- mat75[, lp75, drop = FALSE]
stopifnot(all(is.finite(ll75) | (is.infinite(ll75) & ll75 < 0)))
loo_dde_75 <- loo(ll75)


##################################################################
##DDE 95th
##SPEI 12
model_dde_95 <-  nimbleCode({
  
  #Priors
  Intercept  ~ dnorm(0, sd = 5)
  
  
  lac_effect~ dt(mu=0, sigma=1, df=1)
  bm_effect ~ dt(mu=0, sigma=1, df=1)
  dde_effect_m ~ dt(mu=0, sigma=1, df=1) 
  age_effect ~ dt(mu=0, sigma=1, df=1)
  precip_effect ~ dt(mu=0, sigma=1, df=1)
  bod_con_effect ~ dt(mu=0, sigma=1, df=1)

  spei_12_effect ~ dt(mu = 0 , sigma = 1, df = 1)
  
  
  for(i in 1:n.obs){
    
    #Process Model
    logit(preg_prob[i])<-Intercept+
      (lac_score[i]*lac_effect)+
      (est_bm[i]*bm_effect)+
      (dde_95[i]*dde_effect_m)+
      (age[i]*age_effect)+
      (precip_year_prior[i]*precip_effect)+
      (est_body_cond[i]*bod_con_effect)+

      (spei_12[i]*spei_12_effect)
    
    
    #Observation Model (Likelihood)
    preg_status[i] ~ dbern(preg_prob[i])
    
    #Impute missing data values
    lac_score[i] ~ dnorm(mean=0,sd=1)
    precip_year_prior[i] ~ dnorm(mean = 0, sd = 1)

    spei_12[i] ~ dnorm(mean = 0, sd = 1)
    dde_95[i] ~ dnorm(mean=0,sd=1)
    age[i] ~ dnorm(mean=0,sd=1)
    est_body_cond[i] ~ dnorm(mean=0,sd=1)
    est_bm[i] ~ dnorm(mean=0,sd=1)
  }#i
  
})#model5

parameters <- c("lac_effect","bm_effect",
                "dde_effect_m","age_effect",
                "precip_effect", "Intercept", "bod_con_effect",
                "logProb_preg_status", 

                "spei_12_effect"
)

# MCMC Settings
ni <- 400000
nt <- 400
nb <- 200000
nc <- 3

nimble.data = list(
  preg_status = as.numeric(cow_capture_all$Preg_lab) - 1,
  lac_score = as.numeric((cow_capture_all$lac_score)),
  est_bm = as.numeric((cow_capture_all$est_bm)),
  dde_95 = as.numeric((cow_capture_all$area_DDE_gt_95)),
  est_body_cond=as.numeric((cow_capture_all$IFBF)),
  age = as.numeric((cow_capture_all$age_estimate_2)),
  precip_year_prior = as.numeric((cow_capture_all$total_year_prior)),

  spei_12 = as.numeric((cow_capture_all$sept_year_prior_12M))
 
)

nimble.constants = list(n.obs = nrow(cow_capture_all))


mcmc.output_dde_95 <- nimbleMCMC(code = model_dde_95,
                                 data = nimble.data,
                                 constants=nimble.constants,
                                 monitors = parameters,
                                 niter = ni,
                                 nburnin = nb,
                                 nchains = nc,
                                 thin=nt,
                                 summary=TRUE,
                                 samplesAsCodaMCMC = TRUE)

attach.nimble(mcmc.output_dde_95$samples)

mat95  <- do.call(rbind, lapply(mcmc.output_dde_95$samples, as.matrix))
lp95  <- grep("^logProb_preg_status\\[", colnames(mat95), value = TRUE)
ll95   <- mat95[, lp95, drop = FALSE]
stopifnot(all(is.finite(ll95) | (is.infinite(ll95) & ll95 < 0)))
loo_dde_95 <- loo(ll95)



###################################################################

##################################################################
##DDE 95th
##SPEI 12
##body condition score
model_bc <-  nimbleCode({
  
  #Priors
  Intercept  ~ dnorm(0, sd = 5)
  

  
  lac_effect~ dt(mu=0, sigma=1, df=1)
  bm_effect ~ dt(mu=0, sigma=1, df=1)
  dde_effect_m ~ dt(mu=0, sigma=1, df=1) 
  age_effect ~ dt(mu=0, sigma=1, df=1)
  precip_effect ~ dt(mu=0, sigma=1, df=1)
  bod_con_effect ~ dt(mu=0, sigma=1, df=1)

  spei_12_effect ~ dt(mu = 0 , sigma = 1, df = 1)
  
  
  for(i in 1:n.obs){
    
    #Process Model
    logit(preg_prob[i])<-Intercept+
      (lac_score[i]*lac_effect)+
      (est_bm[i]*bm_effect)+
      (dde_95[i]*dde_effect_m)+
      (age[i]*age_effect)+
      (precip_year_prior[i]*precip_effect)+
      (est_body_cond[i]*bod_con_effect)+

      (spei_12[i]*spei_12_effect)
    
    
    
    
    #Observation Model (Likelihood)
    preg_status[i] ~ dbern(preg_prob[i])
    
    #Impute missing data values
    lac_score[i] ~ dnorm(mean=0,sd=1)
    precip_year_prior[i] ~ dnorm(mean = 0, sd = 1)

    spei_12[i] ~ dnorm(mean = 0, sd = 1)
    dde_95[i] ~ dnorm(mean=0,sd=1)
    age[i] ~ dnorm(mean=0,sd=1)
    est_body_cond[i] ~ dnorm(mean=0,sd=1)
    est_bm[i] ~ dnorm(mean=0,sd=1)
  }#i
  
})#model5

parameters <- c("lac_effect","bm_effect",
                "dde_effect_m","age_effect",
                "precip_effect", "Intercept", "bod_con_effect",
                "logProb_preg_status", 

                "spei_12_effect"
)

# MCMC Settings
ni <- 400000
nt <- 400
nb <- 200000
nc <- 3

nimble.data = list(
  preg_status = as.numeric(cow_capture_all$Preg_lab) - 1,
  lac_score = as.numeric((cow_capture_all$lac_score)),
  est_bm = as.numeric((cow_capture_all$est_bm)),
  dde_95 = as.numeric((cow_capture_all$area_DDE_gt_95)),
  est_body_cond=as.numeric((cow_capture_all$BCS)),
  age = as.numeric((cow_capture_all$age_estimate_2)),
  precip_year_prior = as.numeric((cow_capture_all$total_year_prior)),

  spei_12 = as.numeric((cow_capture_all$sept_year_prior_12M))

)

nimble.constants = list(n.obs = nrow(cow_capture_all))


mcmc.output_bc <- nimbleMCMC(code = model_bc,
                             data = nimble.data,
                             constants=nimble.constants,
                             monitors = parameters,
                             niter = ni,
                             nburnin = nb,
                             nchains = nc,
                             thin=nt,
                             summary=TRUE,
                             samplesAsCodaMCMC = TRUE)


attach.nimble(mcmc.output_bc$samples)

matbc <- do.call(rbind, lapply(mcmc.output_bc$samples, as.matrix))
lpbc  <- grep("^logProb_preg_status\\[", colnames(matbc), value = TRUE)
llbc   <- matbc[, lpbc, drop = FALSE]
stopifnot(all(is.finite(llbc) | (is.infinite(llbc) & llbc < 0)))
loo_bc <- loo(llbc)



###################################################################
##################################################################
##DDE 95th
##SPEI 12
##age2
model_age2 <-  nimbleCode({
  
  #Priors
  Intercept  ~ dnorm(0, sd = 5)

  
  lac_effect~ dt(mu=0, sigma=1, df=1)
  bm_effect ~ dt(mu=0, sigma=1, df=1)
  dde_effect_m ~ dt(mu=0, sigma=1, df=1) 
  age_effect ~ dt(mu=0, sigma=1, df=1)
  age2_effect ~ dt(mu=0, sigma=1, df=1)
  precip_effect ~ dt(mu=0, sigma=1, df=1)
  bod_con_effect ~ dt(mu=0, sigma=1, df=1)

  spei_12_effect ~ dt(mu = 0 , sigma = 1, df = 1)
  
  
  for(i in 1:n.obs){
    
    #Process Model
    logit(preg_prob[i])<-Intercept+
      (lac_score[i]*lac_effect)+
      (est_bm[i]*bm_effect)+
      (dde_95[i]*dde_effect_m)+
      (age[i]*age_effect)+
      (pow(age[i], 2)*age2_effect) +
      (precip_year_prior[i]*precip_effect)+
      (est_body_cond[i]*bod_con_effect)+

      (spei_12[i]*spei_12_effect)
    
    
    #Observation Model (Likelihood)
    preg_status[i] ~ dbern(preg_prob[i])
    
    #Impute missing data values
    lac_score[i] ~ dnorm(mean=0,sd=1)
    precip_year_prior[i] ~ dnorm(mean = 0, sd = 1)

    spei_12[i] ~ dnorm(mean = 0, sd = 1)
    dde_95[i] ~ dnorm(mean=0,sd=1)
    age[i] ~ dnorm(mean=0,sd=1)
    est_body_cond[i] ~ dnorm(mean=0,sd=1)
    est_bm[i] ~ dnorm(mean=0,sd=1)
  }#i
  
})#model5

parameters <- c("lac_effect","bm_effect",
                "dde_effect_m","age_effect",
                "age2_effect",
                "precip_effect", "Intercept", "bod_con_effect",
                "logProb_preg_status", 
                #"spei_3_effect"#,
                #"spei_6_effect"#, 
                "spei_12_effect"
)

# MCMC Settings
ni <- 400000
nt <- 400
nb <- 200000
nc <- 3

nimble.data = list(
  preg_status = as.numeric(cow_capture_all$Preg_lab) - 1,
  lac_score = as.numeric((cow_capture_all$lac_score)),
  est_bm = as.numeric((cow_capture_all$est_bm)),
  dde_95 = as.numeric((cow_capture_all$area_DDE_gt_95)),
  est_body_cond=as.numeric((cow_capture_all$IFBF)),
  age = as.numeric((cow_capture_all$age_estimate_2)),
  precip_year_prior = as.numeric((cow_capture_all$total_year_prior)),

  spei_12 = as.numeric((cow_capture_all$sept_year_prior_12M))

)

nimble.constants = list(n.obs = nrow(cow_capture_all))


mcmc.output_age2 <- nimbleMCMC(code = model_age2,
                               data = nimble.data,
                               constants=nimble.constants,
                               monitors = parameters,
                               niter = ni,
                               nburnin = nb,
                               nchains = nc,
                               thin=nt,
                               summary=TRUE,
                               samplesAsCodaMCMC = TRUE)

attach.nimble(mcmc.output_age2$samples)

mata2  <- do.call(rbind, lapply(mcmc.output_age2$samples, as.matrix))
lpa2  <- grep("^logProb_preg_status\\[", colnames(mata2), value = TRUE)
lla2   <- mata2[, lpa2, drop = FALSE]
stopifnot(all(is.finite(lla2) | (is.infinite(lla2) & lla2 < 0)))
loo_a2 <- loo(lla2)


##############################################################################
###remove body condtion and precip
###################################################################
##################################################################
##DDE 95th
##SPEI 12
##age2
model_reduced <-  nimbleCode({
  
  #Priors
  Intercept  ~ dnorm(0, sd = 5)
  

  
  lac_effect~ dt(mu=0, sigma=1, df=1)
  bm_effect ~ dt(mu=0, sigma=1, df=1)
  dde_effect_m ~ dt(mu=0, sigma=1, df=1) 
  age_effect ~ dt(mu=0, sigma=1, df=1)
  age2_effect ~ dt(mu=0, sigma=1, df=1)

  spei_12_effect ~ dt(mu = 0 , sigma = 1, df = 1)
  
  
  for(i in 1:n.obs){
    
    #Process Model
    logit(preg_prob[i])<-Intercept+
      (lac_score[i]*lac_effect)+
      (est_bm[i]*bm_effect)+
      (dde_95[i]*dde_effect_m)+
      (age[i]*age_effect)+
      (pow(age[i], 2)*age2_effect) +

      (spei_12[i]*spei_12_effect)
    
    
    #Observation Model (Likelihood)
    preg_status[i] ~ dbern(preg_prob[i])
    
    #Impute missing data values
    lac_score[i] ~ dnorm(mean=0,sd=1)

    spei_12[i] ~ dnorm(mean = 0, sd = 1)
    dde_95[i] ~ dnorm(mean=0,sd=1)
    age[i] ~ dnorm(mean=0,sd=1)
   
    est_bm[i] ~ dnorm(mean=0,sd=1)
  }#i
  
})#model5

parameters <- c("lac_effect","bm_effect",
                "dde_effect_m","age_effect",
                "age2_effect",
                "Intercept", 
                
                "logProb_preg_status", 
                "spei_12_effect"
)

# MCMC Settings
ni <- 400000
nt <- 400
nb <- 200000
nc <- 3

nimble.data = list(
  preg_status = as.numeric(cow_capture_all$Preg_lab) - 1,
  lac_score = as.numeric((cow_capture_all$lac_score)),
  est_bm = as.numeric((cow_capture_all$est_bm)),
  dde_95 = as.numeric((cow_capture_all$area_DDE_gt_95)),

  age = as.numeric((cow_capture_all$age_estimate_2)),

  spei_12 = as.numeric((cow_capture_all$sept_year_prior_12M))
 
)

nimble.constants = list(n.obs = nrow(cow_capture_all))


mcmc.output_reduced <- nimbleMCMC(code = model_reduced,
                                  data = nimble.data,
                                  constants=nimble.constants,
                                  monitors = parameters,
                                  niter = ni,
                                  nburnin = nb,
                                  nchains = nc,
                                  thin=nt,
                                  summary=TRUE,
                                  samplesAsCodaMCMC = TRUE)

attach.nimble(mcmc.output_reduced$samples)

mata_r  <- do.call(rbind, lapply(mcmc.output_reduced$samples, as.matrix))
lpa_r  <- grep("^logProb_preg_status\\[", colnames(mata_r), value = TRUE)
lla_r   <- mata_r[, lpa_r, drop = FALSE]
stopifnot(all(is.finite(lla_r) | (is.infinite(lla_r) & lla_r < 0)))
loo_a_r <- loo(lla_r)


##############################################################################
###remove body condtion and precip
##add interaction
###################################################################
##################################################################
##DDE 95th
##SPEI 12
##age2
model_interaction <-  nimbleCode({
  
  #Priors
  Intercept  ~ dnorm(0, sd = 5)
  
 
  
  lac_effect~ dt(mu=0, sigma=1, df=1)
  bm_effect ~ dt(mu=0, sigma=1, df=1)
  dde_effect_m ~ dt(mu=0, sigma=1, df=1) 
  age_effect ~ dt(mu=0, sigma=1, df=1)
  age2_effect ~ dt(mu=0, sigma=1, df=1)

  spei_12_effect ~ dt(mu = 0 , sigma = 1, df = 1)
  spei_12_dde_effect~ dt(mu = 0 , sigma = 1, df = 1)
  
  
  for(i in 1:n.obs){
    
    #Process Model
    logit(preg_prob[i])<-Intercept+
      (lac_score[i]*lac_effect)+
      (est_bm[i]*bm_effect)+
      (dde_95[i]*dde_effect_m)+
      (age[i]*age_effect)+
      (pow(age[i], 2)*age2_effect) +

      spei_12[i]*dde_95[i]*spei_12_dde_effect+
      (spei_12[i]*spei_12_effect)
    
    
    #Observation Model (Likelihood)
    preg_status[i] ~ dbern(preg_prob[i])
    
    #Impute missing data values
    lac_score[i] ~ dnorm(mean=0,sd=1)

    spei_12[i] ~ dnorm(mean = 0, sd = 1)
    dde_95[i] ~ dnorm(mean=0,sd=1)
    age[i] ~ dnorm(mean=0,sd=1)
    est_bm[i] ~ dnorm(mean=0,sd=1)
  }#i
  
})

parameters <- c("lac_effect","bm_effect",
                "dde_effect_m","age_effect",
                "age2_effect",
                
                "Intercept", 
                
                "logProb_preg_status", 
                "spei_12_dde_effect",
               
                "spei_12_effect"
)

# MCMC Settings
ni <- 400000
nt <- 400
nb <- 200000
nc <- 3

nimble.data = list(
  preg_status = as.numeric(cow_capture_all$Preg_lab) - 1,
  lac_score = as.numeric((cow_capture_all$lac_score)),
  est_bm = as.numeric((cow_capture_all$est_bm)),
  dde_95 = as.numeric((cow_capture_all$area_DDE_gt_95)),
  
  age = as.numeric((cow_capture_all$age_estimate_2)),

  spei_12 = as.numeric((cow_capture_all$sept_year_prior_12M))
  
)

nimble.constants = list(n.obs = nrow(cow_capture_all))


mcmc.output_interaction <- nimbleMCMC(code = model_interaction,
                                      data = nimble.data,
                                      constants=nimble.constants,
                                      monitors = parameters,
                                      niter = ni,
                                      nburnin = nb,
                                      nchains = nc,
                                      thin=nt,
                                      summary=TRUE,
                                      samplesAsCodaMCMC = TRUE)

attach.nimble(mcmc.output_interaction $samples)

mata_i  <- do.call(rbind, lapply(mcmc.output_interaction $samples, as.matrix))
lpa_i  <- grep("^logProb_preg_status\\[", colnames(mata_i), value = TRUE)
lla_i   <- mata_i[, lpa_i, drop = FALSE]
stopifnot(all(is.finite(lla_i) | (is.infinite(lla_i) & lla_i < 0)))
loo_i <- loo(lla_i)



##########################################################################
##################################################################
##DDE 95th
##SPEI 12
##body condition score
##age2
model_bc_a2 <-  nimbleCode({
  
  #Priors
  Intercept  ~ dnorm(0, sd = 5)

  
  lac_effect~ dt(mu=0, sigma=1, df=1)
  bm_effect ~ dt(mu=0, sigma=1, df=1)
  dde_effect_m ~ dt(mu=0, sigma=1, df=1) 
  age2_effect ~ dt(mu=0, sigma=1, df=1)
  age_effect ~ dt(mu=0, sigma=1, df=1)
  precip_effect ~ dt(mu=0, sigma=1, df=1)
  bod_con_effect ~ dt(mu=0, sigma=1, df=1)

  spei_12_effect ~ dt(mu = 0 , sigma = 1, df = 1)
  
  
  for(i in 1:n.obs){
    
    #Process Model
    logit(preg_prob[i])<-Intercept+
      (lac_score[i]*lac_effect)+
      (est_bm[i]*bm_effect)+
      (dde_95[i]*dde_effect_m)+
      (age[i]*age_effect)+
      (pow(age[i], 2)*age2_effect) +
      (precip_year_prior[i]*precip_effect)+
      (est_body_cond[i]*bod_con_effect)+
    
      (spei_12[i]*spei_12_effect)
    
    
    
    
    #Observation Model (Likelihood)
    preg_status[i] ~ dbern(preg_prob[i])
    
    #Impute missing data values
    lac_score[i] ~ dnorm(mean=0,sd=1)
    precip_year_prior[i] ~ dnorm(mean = 0, sd = 1)

    spei_12[i] ~ dnorm(mean = 0, sd = 1)
    dde_95[i] ~ dnorm(mean=0,sd=1)
    age[i] ~ dnorm(mean=0,sd=1)
    est_body_cond[i] ~ dnorm(mean=0,sd=1)
    est_bm[i] ~ dnorm(mean=0,sd=1)
  }#i
  
})#model5

parameters <- c("lac_effect","bm_effect",
                "dde_effect_m","age_effect",
                "precip_effect", "Intercept", "bod_con_effect",
                "logProb_preg_status", 
                "age2_effect",

                "spei_12_effect"
)

# MCMC Settings
ni <- 400000
nt <- 400
nb <- 200000
nc <- 3

nimble.data = list(
  preg_status = as.numeric(cow_capture_all$Preg_lab) - 1,
  lac_score = as.numeric((cow_capture_all$lac_score)),
  est_bm = as.numeric((cow_capture_all$est_bm)),
  dde_95 = as.numeric((cow_capture_all$area_DDE_gt_95)),
  est_body_cond=as.numeric((cow_capture_all$BCS)),
  age = as.numeric((cow_capture_all$age_estimate_2)),
  precip_year_prior = as.numeric((cow_capture_all$total_year_prior)),
 
  spei_12 = as.numeric((cow_capture_all$sept_year_prior_12M))
  
)

nimble.constants = list(n.obs = nrow(cow_capture_all))


mcmc.output_bc_a2 <- nimbleMCMC(code = model_bc_a2,
                                data = nimble.data,
                                constants=nimble.constants,
                                monitors = parameters,
                                niter = ni,
                                nburnin = nb,
                                nchains = nc,
                                thin=nt,
                                summary=TRUE,
                                samplesAsCodaMCMC = TRUE)


attach.nimble(mcmc.output_bc_a2$samples)

matbc_a2 <- do.call(rbind, lapply(mcmc.output_bc_a2$samples, as.matrix))
lpbc_a2  <- grep("^logProb_preg_status\\[", colnames(matbc_a2), value = TRUE)
llbc_a2   <- matbc_a2[, lpbc_a2, drop = FALSE]
stopifnot(all(is.finite(llbc_a2) | (is.infinite(llbc_a2) & llbc_a2 < 0)))
loo_bc_a2 <- loo(llbc_a2)





##################################################

##########################################################################
##################################################################
##DDE 75th
##SPEI 12
##body condition score
##age2
model_bc_a2_75 <-  nimbleCode({
  
  #Priors
  Intercept  ~ dnorm(0, sd = 5)
  

  lac_effect~ dt(mu=0, sigma=1, df=1)
  bm_effect ~ dt(mu=0, sigma=1, df=1)
  dde_effect_m ~ dt(mu=0, sigma=1, df=1) 
  age2_effect ~ dt(mu=0, sigma=1, df=1)
  age_effect ~ dt(mu=0, sigma=1, df=1)
  precip_effect ~ dt(mu=0, sigma=1, df=1)
  bod_con_effect ~ dt(mu=0, sigma=1, df=1)

  spei_12_effect ~ dt(mu = 0 , sigma = 1, df = 1)
  
  
  for(i in 1:n.obs){
    
    #Process Model
    logit(preg_prob[i])<-Intercept+
      (lac_score[i]*lac_effect)+
      (est_bm[i]*bm_effect)+
      (dde_95[i]*dde_effect_m)+
      (age[i]*age_effect)+
      (pow(age[i], 2)*age2_effect) +
      (precip_year_prior[i]*precip_effect)+
      (est_body_cond[i]*bod_con_effect)+

      (spei_12[i]*spei_12_effect)
    
    
    
    
    #Observation Model (Likelihood)
    preg_status[i] ~ dbern(preg_prob[i])
    
    #Impute missing data values
    lac_score[i] ~ dnorm(mean=0,sd=1)
    precip_year_prior[i] ~ dnorm(mean = 0, sd = 1)

    spei_12[i] ~ dnorm(mean = 0, sd = 1)
    dde_95[i] ~ dnorm(mean=0,sd=1)
    age[i] ~ dnorm(mean=0,sd=1)
    est_body_cond[i] ~ dnorm(mean=0,sd=1)
    est_bm[i] ~ dnorm(mean=0,sd=1)
  }#i
  
})#model5

parameters <- c("lac_effect","bm_effect",
                "dde_effect_m","age_effect",
                "precip_effect", "Intercept", "bod_con_effect",
                "logProb_preg_status", 
                "age2_effect",

                "spei_12_effect"
)

# MCMC Settings
ni <- 400000
nt <- 400
nb <- 200000
nc <- 3

nimble.data = list(
  preg_status = as.numeric(cow_capture_all$Preg_lab) - 1,
  lac_score = as.numeric((cow_capture_all$lac_score)),
  est_bm = as.numeric((cow_capture_all$est_bm)),
  dde_95 = as.numeric((cow_capture_all$area_DDE_gt_75)),
  est_body_cond=as.numeric((cow_capture_all$BCS)),
  age = as.numeric((cow_capture_all$age_estimate_2)),
  precip_year_prior = as.numeric((cow_capture_all$total_year_prior)),

  spei_12 = as.numeric((cow_capture_all$sept_year_prior_12M))

)

nimble.constants = list(n.obs = nrow(cow_capture_all))


mcmc.output_bc_75 <- nimbleMCMC(code = model_bc_a2_75,
                                data = nimble.data,
                                constants=nimble.constants,
                                monitors = parameters,
                                niter = ni,
                                nburnin = nb,
                                nchains = nc,
                                thin=nt,
                                summary=TRUE,
                                samplesAsCodaMCMC = TRUE)


attach.nimble(mcmc.output_bc_a2$samples)

matbc_a2_75 <- do.call(rbind, lapply(mcmc.output_bc_75$samples, as.matrix))
lpbc_a2_75  <- grep("^logProb_preg_status\\[", colnames(matbc_a2_75), value = TRUE)
llbc_a2_75   <- matbc_a2_75[, lpbc_a2_75, drop = FALSE]
stopifnot(all(is.finite(llbc_a2_75) | (is.infinite(llbc_a2_75) & llbc_a2_75 < 0)))
loo_bc_75 <- loo(llbc_a2_75)




##############################################################################

##age2
#mean DDE
model_bc_a2_mean <-  nimbleCode({
  
  #Priors
  Intercept  ~ dnorm(0, sd = 5)

  
  lac_effect~ dt(mu=0, sigma=1, df=1)
  bm_effect ~ dt(mu=0, sigma=1, df=1)
  dde_effect_m ~ dt(mu=0, sigma=1, df=1) 
  age2_effect ~ dt(mu=0, sigma=1, df=1)
  age_effect ~ dt(mu=0, sigma=1, df=1)
  precip_effect ~ dt(mu=0, sigma=1, df=1)
  bod_con_effect ~ dt(mu=0, sigma=1, df=1)

  spei_12_effect ~ dt(mu = 0 , sigma = 1, df = 1)
  
  
  for(i in 1:n.obs){
    
    #Process Model
    logit(preg_prob[i])<-Intercept+
      (lac_score[i]*lac_effect)+
      (est_bm[i]*bm_effect)+
      (dde_95[i]*dde_effect_m)+
      (age[i]*age_effect)+
      (pow(age[i], 2)*age2_effect) +
      (precip_year_prior[i]*precip_effect)+
      (est_body_cond[i]*bod_con_effect)+
   
      (spei_12[i]*spei_12_effect)
    
    
    
    
    #Observation Model (Likelihood)
    preg_status[i] ~ dbern(preg_prob[i])
    
    #Impute missing data values
    lac_score[i] ~ dnorm(mean=0,sd=1)
    precip_year_prior[i] ~ dnorm(mean = 0, sd = 1)

    spei_12[i] ~ dnorm(mean = 0, sd = 1)
    dde_95[i] ~ dnorm(mean=0,sd=1)
    age[i] ~ dnorm(mean=0,sd=1)
    est_body_cond[i] ~ dnorm(mean=0,sd=1)
    est_bm[i] ~ dnorm(mean=0,sd=1)
  }#i
  
})#model5

parameters <- c("lac_effect","bm_effect",
                "dde_effect_m","age_effect",
                "precip_effect", "Intercept", "bod_con_effect",
                "logProb_preg_status", 
                "age2_effect",
     
                "spei_12_effect"
)

# MCMC Settings
ni <- 400000
nt <- 400
nb <- 200000
nc <- 3

nimble.data = list(
  preg_status = as.numeric(cow_capture_all$Preg_lab) - 1,
  lac_score = as.numeric((cow_capture_all$lac_score)),
  est_bm = as.numeric((cow_capture_all$est_bm)),
  dde_95 = as.numeric((cow_capture_all$area_DDE_gt_mean)),
  est_body_cond=as.numeric((cow_capture_all$BCS)),
  age = as.numeric((cow_capture_all$age_estimate_2)),
  precip_year_prior = as.numeric((cow_capture_all$total_year_prior)),

  spei_12 = as.numeric((cow_capture_all$sept_year_prior_12M))
  
)

nimble.constants = list(n.obs = nrow(cow_capture_all))


mcmc.output_bc_mean <- nimbleMCMC(code = model_bc_a2_mean,
                                  data = nimble.data,
                                  constants=nimble.constants,
                                  monitors = parameters,
                                  niter = ni,
                                  nburnin = nb,
                                  nchains = nc,
                                  thin=nt,
                                  summary=TRUE,
                                  samplesAsCodaMCMC = TRUE)


attach.nimble(mcmc.output_bc_mean$samples)

matbc_a2_m <- do.call(rbind, lapply(mcmc.output_bc_mean$samples, as.matrix))
lpbc_a2_m  <- grep("^logProb_preg_status\\[", colnames(matbc_a2_m), value = TRUE)
llbc_a2_m   <- matbc_a2_m[, lpbc_a2_m, drop = FALSE]
stopifnot(all(is.finite(llbc_a2_m) | (is.infinite(llbc_a2_m) & llbc_a2_m < 0)))
loo_bc_m <- loo(llbc_a2_m)




############################################################################
##################################################################
##DDE 75th
##SPEI 12
##body condition score
##age2
model_bc_75 <-  nimbleCode({
  
  #Priors
  Intercept  ~ dnorm(0, sd = 5)

  
  lac_effect~ dt(mu=0, sigma=1, df=1)
  bm_effect ~ dt(mu=0, sigma=1, df=1)
  dde_effect_m ~ dt(mu=0, sigma=1, df=1) 
 
  age_effect ~ dt(mu=0, sigma=1, df=1)
  precip_effect ~ dt(mu=0, sigma=1, df=1)
  bod_con_effect ~ dt(mu=0, sigma=1, df=1)

  spei_12_effect ~ dt(mu = 0 , sigma = 1, df = 1)
  
  
  for(i in 1:n.obs){
    
    #Process Model
    logit(preg_prob[i])<-Intercept+
      (lac_score[i]*lac_effect)+
      (est_bm[i]*bm_effect)+
      (dde_95[i]*dde_effect_m)+
      (age[i]*age_effect)+
    
      (precip_year_prior[i]*precip_effect)+
      (est_body_cond[i]*bod_con_effect)+
   
      (spei_12[i]*spei_12_effect)
    
    
    
    
    #Observation Model (Likelihood)
    preg_status[i] ~ dbern(preg_prob[i])
    
    #Impute missing data values
    lac_score[i] ~ dnorm(mean=0,sd=1)
    precip_year_prior[i] ~ dnorm(mean = 0, sd = 1)
  
    spei_12[i] ~ dnorm(mean = 0, sd = 1)
    dde_95[i] ~ dnorm(mean=0,sd=1)
    age[i] ~ dnorm(mean=0,sd=1)
    est_body_cond[i] ~ dnorm(mean=0,sd=1)
    est_bm[i] ~ dnorm(mean=0,sd=1)
  }#i
  
})#model5

parameters <- c("lac_effect","bm_effect",
                "dde_effect_m","age_effect",
                "precip_effect", "Intercept", "bod_con_effect",
                "logProb_preg_status", 
         
                "spei_12_effect"
)

# MCMC Settings
ni <- 400000
nt <- 400
nb <- 200000
nc <- 3

nimble.data = list(
  preg_status = as.numeric(cow_capture_all$Preg_lab) - 1,
  lac_score = as.numeric((cow_capture_all$lac_score)),
  est_bm = as.numeric((cow_capture_all$est_bm)),
  dde_95 = as.numeric((cow_capture_all$area_DDE_gt_75)),
  est_body_cond=as.numeric((cow_capture_all$BCS)),
  age = as.numeric((cow_capture_all$age_estimate_2)),
  precip_year_prior = as.numeric((cow_capture_all$total_year_prior)),

  spei_12 = as.numeric((cow_capture_all$sept_year_prior_12M))

)

nimble.constants = list(n.obs = nrow(cow_capture_all))


mcmc.output_bc_75_s <- nimbleMCMC(code = model_bc_75,
                                  data = nimble.data,
                                  constants=nimble.constants,
                                  monitors = parameters,
                                  niter = ni,
                                  nburnin = nb,
                                  nchains = nc,
                                  thin=nt,
                                  summary=TRUE,
                                  samplesAsCodaMCMC = TRUE)


attach.nimble(mcmc.output_bc_a2$samples)

matbc_75 <- do.call(rbind, lapply(mcmc.output_bc_75_s$samples, as.matrix))
lpbc_75  <- grep("^logProb_preg_status\\[", colnames(matbc_75), value = TRUE)
llbc_75   <- matbc_75[, lpbc_a2_75, drop = FALSE]
stopifnot(all(is.finite(llbc_75) | (is.infinite(llbc_75) & llbc_75 < 0)))
loo_bc_75_s <- loo(llbc_75)


################################################################################
####remove precip 
############################################################################
##################################################################
##DDE 75th
##SPEI 12
##body condition score
##remove prec
model_bc_75_san_p <-  nimbleCode({
  
  #Priors
  Intercept  ~ dnorm(0, sd = 5)

  
  lac_effect~ dt(mu=0, sigma=1, df=1)
  bm_effect ~ dt(mu=0, sigma=1, df=1)
  dde_effect_m ~ dt(mu=0, sigma=1, df=1) 
  
  age_effect ~ dt(mu=0, sigma=1, df=1)
  
  bod_con_effect ~ dt(mu=0, sigma=1, df=1)

  spei_12_effect ~ dt(mu = 0 , sigma = 1, df = 1)
  
  
  for(i in 1:n.obs){
    
    #Process Model
    logit(preg_prob[i])<-Intercept+
      (lac_score[i]*lac_effect)+
      (est_bm[i]*bm_effect)+
      (dde_95[i]*dde_effect_m)+
      (age[i]*age_effect)+
 
      (est_body_cond[i]*bod_con_effect)+

      (spei_12[i]*spei_12_effect)
    
    
    
    
    #Observation Model (Likelihood)
    preg_status[i] ~ dbern(preg_prob[i])
    
    #Impute missing data values
    lac_score[i] ~ dnorm(mean=0,sd=1)
  
    spei_12[i] ~ dnorm(mean = 0, sd = 1)
    dde_95[i] ~ dnorm(mean=0,sd=1)
    age[i] ~ dnorm(mean=0,sd=1)
    est_body_cond[i] ~ dnorm(mean=0,sd=1)
    est_bm[i] ~ dnorm(mean=0,sd=1)
  }#i
  
})#model5

parameters <- c("lac_effect","bm_effect",
                "dde_effect_m","age_effect",
              
                "Intercept", "bod_con_effect",
                "logProb_preg_status", 
               
                "spei_12_effect"
)

# MCMC Settings
ni <- 400000
nt <- 400
nb <- 200000
nc <- 3

nimble.data = list(
  preg_status = as.numeric(cow_capture_all$Preg_lab) - 1,
  lac_score = as.numeric((cow_capture_all$lac_score)),
  est_bm = as.numeric((cow_capture_all$est_bm)),
  dde_95 = as.numeric((cow_capture_all$area_DDE_gt_75)),
  est_body_cond=as.numeric((cow_capture_all$BCS)),
  age = as.numeric((cow_capture_all$age_estimate_2)),

  spei_12 = as.numeric((cow_capture_all$sept_year_prior_12M))

)

nimble.constants = list(n.obs = nrow(cow_capture_all))


mcmc.output_bc_75_san_p <- nimbleMCMC(code = model_bc_75_san_p ,
                                      data = nimble.data,
                                      constants=nimble.constants,
                                      monitors = parameters,
                                      niter = ni,
                                      nburnin = nb,
                                      nchains = nc,
                                      thin=nt,
                                      summary=TRUE,
                                      samplesAsCodaMCMC = TRUE)


attach.nimble(mcmc.output_bc_75_san_p $samples)

matbc_75 <- do.call(rbind, lapply(mcmc.output_bc_75_san_p$samples, as.matrix))
lpbc_75  <- grep("^logProb_preg_status\\[", colnames(matbc_75), value = TRUE)
llbc_75   <- matbc_75[, lpbc_a2_75, drop = FALSE]
stopifnot(all(is.finite(llbc_75) | (is.infinite(llbc_75) & llbc_75 < 0)))
loo_bc_75_sans_p <- loo(llbc_75)

model_dde_bc_a2_75_s<- loo(logProb_preg_status)




##################################################################################
##san prec + a2
##DDE 75th
##SPEI 12
##body condition score
##remove prec
model_bc_75_san_pa2 <-  nimbleCode({
  
  #Priors
  Intercept  ~ dnorm(0, sd = 5)
  

  lac_effect~ dt(mu=0, sigma=1, df=1)
  bm_effect ~ dt(mu=0, sigma=1, df=1)
  dde_effect_m ~ dt(mu=0, sigma=1, df=1) 
  age2_effect ~ dt(mu=0, sigma=1, df=1)
  age_effect ~ dt(mu=0, sigma=1, df=1)

  bod_con_effect ~ dt(mu=0, sigma=1, df=1)

  spei_12_effect ~ dt(mu = 0 , sigma = 1, df = 1)
  
  
  for(i in 1:n.obs){
    
    #Process Model
    logit(preg_prob[i])<-Intercept+
      (lac_score[i]*lac_effect)+
      (est_bm[i]*bm_effect)+
      (dde_95[i]*dde_effect_m)+
      (age[i]*age_effect)+
      (pow(age[i], 2)*age2_effect) +
      
      (est_body_cond[i]*bod_con_effect)+

      (spei_12[i]*spei_12_effect)
    
    
    
    
    #Observation Model (Likelihood)
    preg_status[i] ~ dbern(preg_prob[i])
    
    #Impute missing data values
    lac_score[i] ~ dnorm(mean=0,sd=1)

    spei_12[i] ~ dnorm(mean = 0, sd = 1)
    dde_95[i] ~ dnorm(mean=0,sd=1)
    age[i] ~ dnorm(mean=0,sd=1)
    est_body_cond[i] ~ dnorm(mean=0,sd=1)
    est_bm[i] ~ dnorm(mean=0,sd=1)
  }#i
  
})

parameters <- c("lac_effect","bm_effect",
                "dde_effect_m","age_effect",
                
                "Intercept", "bod_con_effect",
                "logProb_preg_status", 
                "age2_effect",
             
                "spei_12_effect"
)

# MCMC Settings
ni <- 400000
nt <- 400
nb <- 200000
nc <- 3

nimble.data = list(
  preg_status = as.numeric(cow_capture_all$Preg_lab) - 1,
  lac_score = as.numeric((cow_capture_all$lac_score)),
  est_bm = as.numeric((cow_capture_all$est_bm)),
  dde_95 = as.numeric((cow_capture_all$area_DDE_gt_75)),
  est_body_cond=as.numeric((cow_capture_all$BCS)),
  age = as.numeric((cow_capture_all$age_estimate_2)),

  spei_12 = as.numeric((cow_capture_all$sept_year_prior_12M))
  
)

nimble.constants = list(n.obs = nrow(cow_capture_all))


mcmc.output_bc_75_san_pa2 <- nimbleMCMC(code = model_bc_75_san_pa2 ,
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
attach.nimble(mcmc.output_bc_75_san_pa2 $samples)


matbc_75 <- do.call(rbind, lapply(mcmc.output_bc_75_san_pa2$samples, as.matrix))
lpbc_75  <- grep("^logProb_preg_status\\[", colnames(matbc_75), value = TRUE)
llbc_75   <- matbc_75[, lpbc_a2_75, drop = FALSE]
stopifnot(all(is.finite(llbc_75) | (is.infinite(llbc_75) & llbc_75 < 0)))
loo_bc_75_sans_pa2 <- loo(llbc_75)

model_dde_bc_a2_75_s<- loo(logProb_preg_status)

##top model leave

MCMCtrace(object = mcmc.output_bc_75_san_pa2$samples,
          pdf = FALSE, # no export to PDF
          ind = TRUE, # separate density lines per chain
          params = "lac_effect")
# Gelman-Rubin diagnostic (AKA RHat or PSRF)
gelman.diag(mcmc.output_bc_75_san_pa2$samples, multivariate = FALSE)


#################################################################################
##global
model_global <-  nimbleCode({
  
  #Priors
  Intercept  ~ dnorm(0, sd = 5)
  

  
  lac_effect~ dt(mu=0, sigma=1, df=1)
  bm_effect ~ dt(mu=0, sigma=1, df=1)
  dde_effect_m ~ dt(mu=0, sigma=1, df=1) 
  age2_effect ~ dt(mu=0, sigma=1, df=1)
  age_effect ~ dt(mu=0, sigma=1, df=1)
  precip_effect ~ dt(mu=0, sigma=1, df=1)
  bod_con_effect ~ dt(mu=0, sigma=1, df=1)

  spei_12_effect ~ dt(mu = 0 , sigma = 1, df = 1)
  ifbf_effect ~ dt(mu = 0 , sigma = 1, df = 1)
  
  
  for(i in 1:n.obs){
    
    #Process Model
    logit(preg_prob[i])<-Intercept+
      (lac_score[i]*lac_effect)+
      (est_bm[i]*bm_effect)+
      (dde_95[i]*dde_effect_m)+
      (age[i]*age_effect)+
      (pow(age[i], 2)*age2_effect) +
      (precip_year_prior[i]*precip_effect)+
      (est_body_cond[i]*bod_con_effect)+

      (spei_12[i]*spei_12_effect)+
      (est_body_cond_ifbf[i] * ifbf_effect)
    
    
    
    
    #Observation Model (Likelihood)
    preg_status[i] ~ dbern(preg_prob[i])
    
    #Impute missing data values
    lac_score[i] ~ dnorm(mean=0,sd=1)
    precip_year_prior[i] ~ dnorm(mean = 0, sd = 1)
   
    est_body_cond_ifbf[i] ~ dnorm(mean = 0, sd = 1)
    spei_12[i] ~ dnorm(mean = 0, sd = 1)
    dde_95[i] ~ dnorm(mean=0,sd=1)
    age[i] ~ dnorm(mean=0,sd=1)
    est_body_cond[i] ~ dnorm(mean=0,sd=1)
    est_bm[i] ~ dnorm(mean=0,sd=1)
  }#i
  
})#model5

parameters <- c("lac_effect","bm_effect",
                "dde_effect_m","age_effect",
                "precip_effect", 
                "Intercept", "bod_con_effect",
                "logProb_preg_status", 
                "age2_effect",
                "ifbf_effect",

                "spei_12_effect"
)

# MCMC Settings
ni <- 400000
nt <- 400
nb <- 200000
nc <- 3

nimble.data = list(
  preg_status = as.numeric(cow_capture_all$Preg_lab) - 1,
  lac_score = as.numeric((cow_capture_all$lac_score)),
  est_bm = as.numeric((cow_capture_all$est_bm)),
  dde_95 = as.numeric((cow_capture_all$area_DDE_gt_75)),
  est_body_cond=as.numeric((cow_capture_all$BCS)),
  est_body_cond_ifbf = as.numeric(cow_capture_all$IFBF),
  age = as.numeric((cow_capture_all$age_estimate_2)),
  precip_year_prior = as.numeric((cow_capture_all$total_year_prior)),

  spei_12 = as.numeric((cow_capture_all$sept_year_prior_12M))

)

nimble.constants = list(n.obs = nrow(cow_capture_all))


mcmc.output_global<- nimbleMCMC(code = model_global ,
                                data = nimble.data,
                                constants=nimble.constants,
                                monitors = parameters,
                                niter = ni,
                                nburnin = nb,
                                nchains = nc,
                                thin=nt,
                                summary=TRUE,
                                samplesAsCodaMCMC = TRUE)


attach.nimble(mcmc.output_global $samples)

matbc_75 <- do.call(rbind, lapply(mcmc.output_global$samples, as.matrix))
lpbc_75  <- grep("^logProb_preg_status\\[", colnames(matbc_75), value = TRUE)
llbc_75   <- matbc_75[, lpbc_a2_75, drop = FALSE]
stopifnot(all(is.finite(llbc_75) | (is.infinite(llbc_75) & llbc_75 < 0)))
loo_gobal <- loo(llbc_75)


################################################################
##age2
#mean DDE
model_bc_a2_mean_san_p <-  nimbleCode({
  
  #Priors
  Intercept  ~ dnorm(0, sd = 5)
  

  
  lac_effect~ dt(mu=0, sigma=1, df=1)
  bm_effect ~ dt(mu=0, sigma=1, df=1)
  dde_effect_m ~ dt(mu=0, sigma=1, df=1) 
  age2_effect ~ dt(mu=0, sigma=1, df=1)
  age_effect ~ dt(mu=0, sigma=1, df=1)

  bod_con_effect ~ dt(mu=0, sigma=1, df=1)

  spei_12_effect ~ dt(mu = 0 , sigma = 1, df = 1)
  
  
  for(i in 1:n.obs){
    
    #Process Model
    logit(preg_prob[i])<-Intercept+
      (lac_score[i]*lac_effect)+
      (est_bm[i]*bm_effect)+
      (dde_95[i]*dde_effect_m)+
      (age[i]*age_effect)+
      (pow(age[i], 2)*age2_effect) +
     
      (est_body_cond[i]*bod_con_effect)+

      (spei_12[i]*spei_12_effect)
    
    
    
    
    #Observation Model (Likelihood)
    preg_status[i] ~ dbern(preg_prob[i])
    
    #Impute missing data values
    lac_score[i] ~ dnorm(mean=0,sd=1)

    spei_12[i] ~ dnorm(mean = 0, sd = 1)
    dde_95[i] ~ dnorm(mean=0,sd=1)
    age[i] ~ dnorm(mean=0,sd=1)
    est_body_cond[i] ~ dnorm(mean=0,sd=1)
    est_bm[i] ~ dnorm(mean=0,sd=1)
  }#i
  
})

parameters <- c("lac_effect","bm_effect",
                "dde_effect_m","age_effect",
                #"precip_effect",
                "Intercept", "bod_con_effect",
                "logProb_preg_status", 
                "age2_effect",
                #"spei_3_effect"#,
                #"spei_6_effect"#, 
                "spei_12_effect"
)

# MCMC Settings
ni <- 400000
nt <- 400
nb <- 200000
nc <- 3

nimble.data = list(
  preg_status = as.numeric(cow_capture_all$Preg_lab) - 1,
  lac_score = as.numeric((cow_capture_all$lac_score)),
  est_bm = as.numeric((cow_capture_all$est_bm)),
  dde_95 = as.numeric((cow_capture_all$area_DDE_gt_mean)),
  est_body_cond=as.numeric((cow_capture_all$BCS)),
  age = as.numeric((cow_capture_all$age_estimate_2)),

  spei_12 = as.numeric((cow_capture_all$sept_year_prior_12M))
  
)

nimble.constants = list(n.obs = nrow(cow_capture_all))


mcmc.output_bc_mean_san_p <- nimbleMCMC(code = model_bc_a2_mean_san_p,
                                        data = nimble.data,
                                        constants=nimble.constants,
                                        monitors = parameters,
                                        niter = ni,
                                        nburnin = nb,
                                        nchains = nc,
                                        thin=nt,
                                        summary=TRUE,
                                        samplesAsCodaMCMC = TRUE)


attach.nimble(mcmc.output_bc_mean_san_p$samples)

matbc_a2_m <- do.call(rbind, lapply(mcmc.output_bc_mean_san_p$samples, as.matrix))
lpbc_a2_m  <- grep("^logProb_preg_status\\[", colnames(matbc_a2_m), value = TRUE)
llbc_a2_m   <- matbc_a2_m[, lpbc_a2_m, drop = FALSE]
stopifnot(all(is.finite(llbc_a2_m) | (is.infinite(llbc_a2_m) & llbc_a2_m < 0)))
loo_bc_m_san_p <- loo(llbc_a2_m)


########################################

##age
#mean DDE
model_bc_a_mean_san_p <-  nimbleCode({
  
  #Priors
  Intercept  ~ dnorm(0, sd = 5)

  lac_effect~ dt(mu=0, sigma=1, df=1)
  bm_effect ~ dt(mu=0, sigma=1, df=1)
  dde_effect_m ~ dt(mu=0, sigma=1, df=1) 

  age_effect ~ dt(mu=0, sigma=1, df=1)

  bod_con_effect ~ dt(mu=0, sigma=1, df=1)

  spei_12_effect ~ dt(mu = 0 , sigma = 1, df = 1)
  
  
  for(i in 1:n.obs){
    
    #Process Model
    logit(preg_prob[i])<-Intercept+
      (lac_score[i]*lac_effect)+
      (est_bm[i]*bm_effect)+
      (dde_95[i]*dde_effect_m)+
      (age[i]*age_effect)+

      (est_body_cond[i]*bod_con_effect)+

      (spei_12[i]*spei_12_effect)

    
    #Observation Model (Likelihood)
    preg_status[i] ~ dbern(preg_prob[i])
    
    #Impute missing data values
    lac_score[i] ~ dnorm(mean=0,sd=1)

    spei_12[i] ~ dnorm(mean = 0, sd = 1)
    dde_95[i] ~ dnorm(mean=0,sd=1)
    age[i] ~ dnorm(mean=0,sd=1)
    est_body_cond[i] ~ dnorm(mean=0,sd=1)
    est_bm[i] ~ dnorm(mean=0,sd=1)
  }
  
})

parameters <- c("lac_effect","bm_effect",
                "dde_effect_m","age_effect",
             
                "Intercept", "bod_con_effect",
                "logProb_preg_status", 

                "spei_12_effect"
)

# MCMC Settings
ni <- 400000
nt <- 400
nb <- 200000
nc <- 3

nimble.data = list(
  preg_status = as.numeric(cow_capture_all$Preg_lab) - 1,
  lac_score = as.numeric((cow_capture_all$lac_score)),
  est_bm = as.numeric((cow_capture_all$est_bm)),
  dde_95 = as.numeric((cow_capture_all$area_DDE_gt_mean)),
  est_body_cond=as.numeric((cow_capture_all$BCS)),
  age = as.numeric((cow_capture_all$age_estimate_2)),

  spei_12 = as.numeric((cow_capture_all$sept_year_prior_12M))

)

nimble.constants = list(n.obs = nrow(cow_capture_all))


mcmc.output_bc_mean_a1_san_p <- nimbleMCMC(code = model_bc_a_mean_san_p,
                                           data = nimble.data,
                                           constants=nimble.constants,
                                           monitors = parameters,
                                           niter = ni,
                                           nburnin = nb,
                                           nchains = nc,
                                           thin=nt,
                                           summary=TRUE,
                                           samplesAsCodaMCMC = TRUE)


attach.nimble(mcmc.output_bc_mean_a1_san_p$samples)

matbc_a2_m <- do.call(rbind, lapply(mcmc.output_bc_mean_a1_san_p$samples, as.matrix))
lpbc_a2_m  <- grep("^logProb_preg_status\\[", colnames(matbc_a2_m), value = TRUE)
llbc_a2_m   <- matbc_a2_m[, lpbc_a2_m, drop = FALSE]
stopifnot(all(is.finite(llbc_a2_m) | (is.infinite(llbc_a2_m) & llbc_a2_m < 0)))
loo_bc_m_san_p_a1 <- loo(llbc_a2_m)
beep(sound = 1)


########################################interactions######################

model_bc_75_san_pa2_bcLS <-  nimbleCode({
  
  #Priors
  Intercept  ~ dnorm(0, sd = 5)
  

  
  lac_effect~ dt(mu=0, sigma=1, df=1)
  bm_effect ~ dt(mu=0, sigma=1, df=1)
  dde_effect_m ~ dt(mu=0, sigma=1, df=1) 
  age2_effect ~ dt(mu=0, sigma=1, df=1)
  age_effect ~ dt(mu=0, sigma=1, df=1)

  bod_con_effect ~ dt(mu=0, sigma=1, df=1)

  spei_12_effect ~ dt(mu = 0 , sigma = 1, df = 1)
  bod_con_lac_effect ~dt(mu = 0 , sigma = 1, df = 1)
  
  
  for(i in 1:n.obs){
    
    #Process Model
    logit(preg_prob[i])<-Intercept+
      (lac_score[i]*lac_effect)+
      (est_bm[i]*bm_effect)+
      (dde_95[i]*dde_effect_m)+
      (age[i]*age_effect)+
      (pow(age[i], 2)*age2_effect) +
     
      (est_body_cond[i]*bod_con_effect)+
      (est_body_cond[i]*est_bm[i]*bod_con_lac_effect)+

      (spei_12[i]*spei_12_effect)
    #Observation Model (Likelihood)
    preg_status[i] ~ dbern(preg_prob[i])
    
    #Impute missing data values
    lac_score[i] ~ dnorm(mean=0,sd=1)

    spei_12[i] ~ dnorm(mean = 0, sd = 1)
    dde_95[i] ~ dnorm(mean=0,sd=1)
    age[i] ~ dnorm(mean=0,sd=1)
    est_body_cond[i] ~ dnorm(mean=0,sd=1)
    est_bm[i] ~ dnorm(mean=0,sd=1)
  }#i
  
})#model5

parameters <- c("lac_effect","bm_effect",
                "dde_effect_m","age_effect",
               
                "Intercept", "bod_con_effect",
                "logProb_preg_status", 
                "age2_effect",
                "bod_con_lac_effect",

                "spei_12_effect"
)

# MCMC Settings
ni <- 400000
nt <- 400
nb <- 200000
nc <- 3

nimble.data = list(
  preg_status = as.numeric(cow_capture_all$Preg_lab) - 1,
  lac_score = as.numeric((cow_capture_all$lac_score)),
  est_bm = as.numeric((cow_capture_all$est_bm)),
  dde_95 = as.numeric((cow_capture_all$area_DDE_gt_75)),
  est_body_cond=as.numeric((cow_capture_all$BCS)),
  age = as.numeric((cow_capture_all$age_estimate_2)),

  spei_12 = as.numeric((cow_capture_all$sept_year_prior_12M))
 
)

nimble.constants = list(n.obs = nrow(cow_capture_all))


mcmc.output_bc_75_san_pa2_bcLS <- nimbleMCMC(code = model_bc_75_san_pa2_bcLS ,
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
attach.nimble(mcmc.output_bc_75_san_pa2_bcLS$samples)


matbc_75 <- do.call(rbind, lapply(mcmc.output_bc_75_san_pa2_bcLS$samples, as.matrix))
lpbc_75  <- grep("^logProb_preg_status\\[", colnames(matbc_75), value = TRUE)
llbc_75   <- matbc_75[, lpbc_a2_75, drop = FALSE]
stopifnot(all(is.finite(llbc_75) | (is.infinite(llbc_75) & llbc_75 < 0)))
loo_bc_75_sans_pa2_bcLS <- loo(llbc_75)

loo_compare(loo_spei_3,
            loo_spei_6,#2
            loo_spei_12,#3
            loo_dde_75,#4
            loo_dde_95,#5
            loo_bc,#6
            loo_a2,#7
            loo_a_r,#8
            loo_i,#9
            loo_bc_a2,#10
            loo_bc_75,#11
            loo_bc_m,#12
            loo_bc_75_s,#13
            loo_bc_75_sans_p,#14
            loo_bc_75_sans_pa2,#15
            loo_gobal,#16
            loo_bc_m_san_p,
            loo_bc_m_san_p_a1,
            loo_bc_75_sans_pa2_bcLS)#18
