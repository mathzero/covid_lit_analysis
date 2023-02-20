orderMyTable <- function(tab, Variable="Variable", Category="Category"){
  out.list <- list()
  
  if("Level" %in% colnames(tab)){
    tab$Category <-  tab$Level
  }
  # define some common lists of categories #
  freqs=stringr::word(c("All the time","Some of the time","Hardly ever","Never","Don't know"),1)
  ynpns=stringr::word(c("Yes", "No", "Prefer not to say"),1)
  yn=stringr::word(c("Yes", "No"),1)
  fags = stringr::word(c("Current cigarette smoker","Not current cigarette smoker","Prefer not to say"),1)
  vapes = stringr::word(c("Current vaper","Not current vaper","Prefer not to say"),1)
  covid = stringr::word(c("COVID confirmed by test","COVID suspected by doctor",
                          "COVID suspected by respondent","No COVID"),-1)
  adiposity = stringr::word(c("Underweight","Normal weight","Overweight", "Obese"),1)
  education = stringr::word(c("No qualification","GCSE","Post-GCSE qualification","Degree or above","Other"),1)
  facecov = stringr::word(c("No" ,"Yes, at work/school only",
                            "Yes, in other situations only (public transport, shops)",
                            "Yes, both at work/school and in other situations",           
                            "My face is already covered for other reasons (eg religious)"),-1)
  
  catslist=c(freqs,ynpns,yn, fags, vapes, covid)
  # done #
  
  i <- 1
  for(i in 1:length(unique(pull(tab,(Variable))))){
    varr=unique(pull(tab,(Variable)))[[i]]
    df_subset <- tab %>% filter((Variable) == varr)
    levs=stringr::word(unique(pull(tab,(Category))),1)
    revlevs=stringr::word(unique(pull(tab,(Category))),-1)
    
    mycats = stringr::word(pull(tab,(Category)),1)
    
    if(grepl("age",tolower(varr)) | grepl("imd",tolower(varr))){
      x <- gsub("^(.*?)[[:punct:]].*","\\1",pull(tab,(Category))[pull(tab,(Variable)) == varr])
      out.list[[varr]] <- df_subset[order(as.numeric(x)),]
    }else if(length(setdiff(levs,ynpns))==0){
      out.list[[varr]] <-df_subset[order(match(mycats,ynpns)),]
    }else if(length(setdiff(levs,yn))==0){
      out.list[[varr]] <-df_subset[order(match(mycats,yn)),]
    }else if(length(setdiff(levs,freqs))==0){
      out.list[[varr]] <-df_subset[order(match(mycats,freqs)),]
    }else if(length(setdiff(levs,fags))==0){
      out.list[[varr]] <-df_subset[order(match(mycats,fags)),]
    }else if(length(setdiff(levs,vapes))==0){
      out.list[[varr]] <-df_subset[order(match(mycats,vapes)),]
    }else if(length(setdiff(revlevs,covid))==0){
      out.list[[varr]] <-df_subset[order(match(stringr::word(pull(df_subset,(Category)),-1),
                                               covid)),]
    }else if(length(setdiff(levs,adiposity))==0){
      out.list[[varr]] <-df_subset[order(match(stringr::word(pull(df_subset,(Category)),1),
                                               adiposity)),]
    }else if(length(setdiff(levs,education))==0){
      out.list[[varr]] <-df_subset[order(match(stringr::word(pull(df_subset,(Category)),1),
                                               education)),]
    }else if(length(setdiff(revlevs,facecov))==0){
      out.list[[varr]] <-df_subset[order(match(stringr::word(pull(df_subset,(Category)),-1),
                                               facecov)),]
    }else{
      out.list[[varr]] <- df_subset
    }
  }
  out <- bind_rows(out.list)
  return(out)
}

