### Install packages
list.of.packages <- c("XML", "xml2", "rcrossref", "stringr","dplyr", "rjson", "httr","rAltmetric","purrr","magrittr","ggmap",
                      "tidyverse", "medrxivr","gtools") ### add all the packages that are used in your script here
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])] ### check if they are installed
if(length(new.packages)) install.packages(new.packages) ### install any that need installing
lapply(list.of.packages, require, character.only = TRUE)
library(medrxivr)
library(rAltmetric)
library(magrittr)
library(purrr)
library(foreach)
library(doParallel)
library(furrr)

# 3a. Altmetrics ----------------------------------------------------------

dat_2020 <- read_csv(file="data/bq-results-20221124-2020.csv")
dat_2021 <- read_csv(file="data/bq-results-20221124-2021.csv")
dat_2022 <- read_csv(file="data/bq-results-20221124-2022.csv")

# bind all
dat_main <- rbind(dat_2020,dat_2021,dat_2022)


# new altmetrics function -------------------------------------------------

altmetrics_new <-
  function(doi = NULL,
           apikey = NULL,
           ...) {
    
    # if (is.null(apikey))
    #   apikey <- '37c9ae22b7979124ea650f3412255bf9'
    
    base_url <- "https://api.altmetric.com/v1/"
    args <- list(key = apikey)
    request <-
      httr::GET(paste0(base_url, "doi/",doi))
    if(httr::status_code(request) == 404) {
      stop("No metrics found for object")
    } else {
      httr::warn_for_status(request)
      results <-
        jsonlite::fromJSON(httr::content(request, as = "text"), flatten = TRUE)
      results <- rlist::list.flatten(results)
      class(results) <- "altmetric"
      results
      
    }
  }


### Batch query function
alm <- function(x,progress=F){
  if(progress){
    pb$tick()$print()
  }
  out <- suppressWarnings(try(altmetric_data(altmetrics_new(doi = x,
                                                            apikey = '37c9ae22b7979124ea650f3412255bf9')),silent = TRUE))
  if(class(out) == "try-error"){
    return(NULL)
  }else{
    return(out)
  }
}  


# Run functions -----------------------------------------------------------

ids <- dat_main$doi[!is.na(dat_main$doi)] %>% as.list()

# for parallel processing
n.cores <- availableCores() - 2
my.cluster <- parallel::makeCluster(n.cores)
doParallel::registerDoParallel(cl = my.cluster)
# x=ids[1]

my_results_list <- list()
n=10000

# define function to do everything
func=function(i){
  myfloor=1+(i-1)*n
  myceiling = n*i
  results <- map_df(ids[myfloor:myceiling], alm)
  saveRDS(object = results,file = paste0("data/altmetrics_harvest/altmetric_res_",i,".rds"))
}

# run in parallel
foreach(i=1:floor(length(ids)/n), .packages = c("rAltmetric","purrr","magrittr","ggmap",
                                                "dplyr")) %dopar% {
  func(i)
}


#stop cluster
stopCluster(my.cluster)



# Non-parallel version (if needed) ----------------------------------------


# 
# for (i in 1:floor(length(ids)/n)){
#   print(paste("Batch number",i))
#   pb <- progress_estimated(n)
#   myfloor=1+(i-1)*n
#   myceiling = n*i
#   results <- map_df(ids[myfloor:myceiling], alm)
#   saveRDS(object = results,file = paste0("data/altmetrics_harvest/altmetric_res_",i,".rds"))
# }
# 


# Final run (remainders) --------------------------------------------------


i=145
myfloor=1+(10000*i)
myceiling = length(ids)
# x=ids[1]
alm <- function(x){
  out <- suppressWarnings(try(altmetric_data(altmetrics_new(doi = x,
                                                            apikey = '37c9ae22b7979124ea650f3412255bf9')),
                              silent = TRUE))
  if(class(out) == "try-error"){
    return(NULL)
  }else{
    return(out)
  }
}  
results <- map_df(ids[myfloor:myceiling], alm)
i <- i+1
saveRDS(object = results,file = paste0("data/altmetrics_harvest/altmetric_res_",i,".rds"))

# Now combine all results -------------------------------------------------

filenames <- list.files(path = "data/altmetrics_harvest/",pattern="altmetric_res")
filenames <- paste0("data/altmetrics_harvest/",filenames)
myreslist <- lapply(filenames,readRDS)

allvars <- names(myreslist[[1]])
i=2
for(i in 2:length(myreslist)){
  vars <-names(myreslist[[i]])
  allvars <- intersect(vars,allvars)
}

# Get vars to keep
allvars <- allvars[!grepl("author",allvars)]
allvars <- allvars[!grepl("editor",allvars)]
allvars <- allvars[!grepl("publisher_subjects",allvars)]
allvars <- allvars[!endsWith(allvars,"2")]
allvars <- allvars[!endsWith(allvars,"3")]
allvars <- allvars[!endsWith(allvars,"4")]
allvars <- allvars[!endsWith(allvars,"5")]
allvars <- allvars[!endsWith(allvars,"6")]
allvars <- allvars[!endsWith(allvars,"7")]
allvars <- allvars[!endsWith(allvars,"8")]
allvars <- allvars[!endsWith(allvars,"9")]



# define a function to extract only the useful columns
selectVars <-  function(dat,allvars){
  dat_out <- dat %>% select(all_of(allvars))
  return(dat_out)
}

# apply function over all results
myreslist_select <- lapply(myreslist,selectVars,allvars=allvars)

# bind into one DF
myresults <- bind_rows(myreslist_select,.id = "column_label")

# Convert to numeric using function
myresults=OverReact::checkConvertNumeric(df = myresults)

# save
saveRDS(object = myresults,file = paste0("data/altmetrics_harvest/altmetric_res_all.rds"))




# Concepts ----------------------------------------------------------------

concepts=read_csv("data/bq-results-20221124-concepts.csv")

# filter na concepts
concepts <- concepts[complete.cases(concepts),]

# get summary
concepts_summary=concepts %>% 
  group_by(concept) %>% 
  summarise(n=n()) %>% 
  arrange(-n)

# get df of concepts with more than 50 papers 
concepts_50=concepts_summary$concept[concepts_summary$n>=50]

# get wider df of concepts
concepts_wide=concepts %>% 
  filter(concept%in%concepts_50) %>% 
  group_by(id,concept) %>% 
  summarise(n=n(),
            rel=mean(relevance,na.rm=T)) %>% 
  pivot_wider(id_cols = id,names_from = concept,values_from = rel)

# hisogram of concepts
concepts_summary %>% 
  filter(concept%in%concepts_50) %>% 
  ggplot(aes(x=n)) +
  geom_histogram(bins=100) +
  OverReact::theme_react()

saveRDS(object = concepts_wide,file = "data/concepts_wide.rds")




# Old code ----------------------------------------------------------------


# This results in a data.frame with one row per identifier.
# You can now see some citation data for these papers.
# 
# library(dplyr)
# knitr::kable(results %>% select(title, doi,  starts_with("cited")))
# 
# 
# 
# ### create empty data frame
# df.altmetric <- data.frame(matrix(nrow=0, ncol=0, dimnames=list(NULL,NULL)))
# 
# ### loop over all dois extracting altmetrics using rAltmetric package
# 
# for (i in 1:length(ids)){
#   print(paste("Processing paper number",i))
#   dat <- suppressWarnings(try(altmetric_data(altmetrics(doi = ids[i])),silent = TRUE))
#   if(class(dat) == "try-error"){
#     df.altmetric[i,] <- NA
#   }
#   else{
#     df.altmetric <- smartbind(df.altmetric,dat)
#   }
# }
# 
# ### drop some variables
# altmetrics.keepvars  <-   names(df.altmetric)[!grepl("autho",names(df.altmetric))]
# altmetrics.keepvars <- setdiff(altmetrics.keepvars,c("title","doi", "journal","type"))
# 
# df.full <- cbind(df.full,df.altmetric[,altmetrics.keepvars])

