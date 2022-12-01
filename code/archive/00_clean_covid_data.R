# import and clean data

library(openxlsx)
library(stringr)
library(tidyverse)
library(plyr)

# read in data sets
dat_1_baseline <- openxlsx::read.xlsx("Canada_Hosp1_COVID_InpatientData.xlsx",sheet = 1)
dat_1_outcome <- openxlsx::read.xlsx("Canada_Hosp1_COVID_InpatientData.xlsx",sheet = 3)
dat_2_baseline <- openxlsx::read.xlsx("Canada_Hosp2_COVID_InpatientData.xlsx",sheet = 1)
dat_2_outcome <- openxlsx::read.xlsx("Canada_Hosp2_COVID_InpatientData.xlsx",sheet = 3)

# renumber duplicated IDs
dat_2_baseline$id=dat_2_baseline$id+max(dat_1_baseline$id)
dat_2_outcome$id=dat_2_outcome$id+max(dat_1_outcome$id)

# bind data sets
dat_baseline=rbind(dat_1_baseline,dat_2_baseline)
dat_outcome=rbind(dat_1_outcome,dat_2_outcome)

# split out string variables

splitStringVar <- function(mystring,ids=dat_baseline$id){
  expandvars=str_split(string = mystring,pattern = ',') 
  expandvars=lapply(X = expandvars,FUN = gsub,pattern = "[^[:alnum:] ]",replacement= "")
  expandvars_all=unlist(expandvars)
  expandvars_all=table(expandvars_all) %>% as.data.frame()
  expandvars_mat=plyr::ldply(expandvars, rbind)
  expandvars_mat$id=ids
  expandvars_mat=expandvars_mat %>% 
    pivot_longer(cols = -id) %>% 
    dplyr::select(-name) %>% 
    filter(!is.na(value), value !="") %>% 
    pivot_wider(id,names_from = value,values_from = value,
                values_fn = list(value = length), 
                values_fill = list(value = 0)) %>% 
    as.data.frame()
  return(expandvars_mat)
}
comorbidities=splitStringVar(mystring=dat_baseline$comorbidities,
                             ids=dat_baseline$id)


### get medications
medications=dat_baseline$medications
medications=lapply(X = medications,FUN = gsub,pattern = "[^[:alnum:] ]",replacement= "")
medications=str_split(string = medications,pattern = 'medications') 
medications_mat=plyr::ldply(medications, rbind)
medications_mat=apply(X = as.matrix(medications_mat),MARGIN = 2,FUN = gsub,
                     pattern = "dosage.*",replacement= "") %>% as.matrix()
medications_mat=apply(X = as.matrix(medications_mat),MARGIN = 2,FUN = tolower) %>% 
  as.data.frame()
medications_mat$id=dat_baseline$id
medications_mat_2=medications_mat %>% 
  pivot_longer(cols = -id) %>% 
  dplyr::select(-name) %>% 
  filter(!is.na(value), value !="") %>% 
  pivot_wider(id,names_from = value,values_from = value,
              values_fn = list(value = length), 
              values_fill = list(value = 0)) %>% 
  as.data.frame()
# let's only take medications which >10 people are using
medications_mat_2 <- medications_mat_2[,(colSums(medications_mat_2)>10)]
# binary only
medications_mat_2[,2:ncol(medications_mat_2)][medications_mat_2[,2:ncol(medications_mat_2)]>0] <- 1


# Join all data -----------------------------------------------------------

dat_baseline <- dat_baseline %>% left_join(comorbidities, by="id")
dat_baseline <- dat_baseline %>% left_join(medications_mat_2, by="id")

# get BMI
dat_baseline$bmi <- (dat_baseline$weight/((dat_baseline$height/100)^2))

# join with outcomes
dat <- dat_baseline %>% left_join(dat_outcome, by = "id")
dat$y=as.numeric(dat$did_the_patient_expire_in_hospital=="Yes")

write_csv(dat,file = "covid_deaths.csv")




# Use covid data ----------------------------------------------------------

X=dat[!is.na(dat$y),2:150]
y=dat$y[!is.na(dat$y)]

### Run bootstrap on example data
results=stabilityshap::SHAPStability(X = X,y = y,my_params = NULL,nruns = 100,
                                     subsample_training_size = 0.5,replace = F,
                                     multipleTestAdjustment = "bonferroni")



### extract results from output object
bootstrap_results=results$results_df
shaps_full=results$shaps_full
results_raw=results$results_raw


### Plot
myplot=plotSHAPbootstrap(X = X,bootstrap_results = bootstrap_results,
                         # orderBy = "prop_pos_cor",
                         shaps_full = shaps_full,
                         topn = 100,thresh = 0.999999)
myplot

