#' This function takes a list of data frames and joins them all together. No data is lost.
#' For variables that are not present in one or more of the data frames, these variables will be added as 
#' NA values for those data sets in the final data set.
#' Created because the smartbind function from gtools seems to be unreliable

#' Created by Matt Whitaker, 23/10/2020


joinMyDatasets <- function(listofdata){
  
  if(class(listofdata) != "list"){
    print("This function takes a list of data frames as an input")
    break
  }
  
  ### How many data frames
  numdfs <- length(listofdata)
  
  ### Get list of all variables across all dfs
  vars.all <- c()
  for(i in 1:numdfs){
    vars.all <- unique(union(vars.all,names(listofdata[[i]])))
  }
  
  ### loop through to create common cols in all dfs
  for (i in 1:numdfs){
    missvars <- setdiff(vars.all,names(listofdata[[i]]))
    listofdata[[i]][, missvars] <- NA
    listofdata[[i]] <- listofdata[[i]][,vars.all] 
  }
  
  ### finally, bind all data sets together
  output.df <- do.call(rbind,listofdata)
  
  ### return
  return(output.df)
}

