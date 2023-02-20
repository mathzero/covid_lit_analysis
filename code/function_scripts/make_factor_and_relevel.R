
# Make all vars categorical -----------------------------------------------



 # define function to relevel specific variable
releveller <- function(mydat,var,ref){
  mydat <- mydat %>% 
    dplyr::mutate(across(all_of(var), as.factor)) %>% 
    dplyr::mutate(across(all_of(var), relevel, ref=ref))
  return(mydat)
}



# define function to relevel all relevant factors
makeVarsFactors <- function(mydat){
  
  # define list of all reference cats for common variables
  reflist <- list("region_named" = "South East",
                  "edu_cat"= "GCSE",
                  "shielding" = "No",
                  "smokenow" = "Not current cigarette smoker",
                  "vapenow" = "Not current vaper",
                  "sex" = "Male",
                  "ethnic_new" = "White",
                  "ethnic19_char" = "English/Welsh/Scottish/Northern Irish/British",
                  "imd_quintile_cat" = "3",
                  "covida_cat" = "No COVID",
                  "mask_indoors" = "Some of the time",
                  "mask_outdoors" = "Some of the time",
                  "face_covering" = "No",
                  "hh_size_cat" = "2",
                  "carehome" = "No",
                  "covidc_cat" = "No symptoms",
                  "clin_vulnerable" = "No",
                  "age_group_named" = "55-64",
                  "hospitalised_covid" = "No",
                  "bmi_cat"="Normal weight",
                  "covid_severity" = "No medical attention sought")
  
  # get index of variables to relevel
  varindx <- which(names(reflist) %in% colnames(mydat))
  
  
  print("These variables will be factorised and relevelled:")
  print(unlist(names(reflist)[varindx]))
  

  # loop to relevel
  for (i in varindx){
    var=names(reflist)[[i]]
    ref=(reflist)[[i]]
    mydat <- releveller(mydat = mydat,var = var, ref = ref)
  }
  
  
  # also replace health conditions
  healthvars <- c(paste0("healtha_",c(1:4,6:9)),
                  paste0("healtha_1",0:7))
  if(all(healthvars%in%colnames(mydat))){
    mydat <- mydat %>% 
      mutate(across(all_of(healthvars), as.factor))
  }
    
  return(mydat)
}

