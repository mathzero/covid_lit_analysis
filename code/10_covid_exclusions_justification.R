
#' First clear the environment of variables
rm(list=ls(all=TRUE))

outpath <- paste0(getwd(),"/output/")
figpath <- paste0(getwd(),"/plots/")
datapath <- paste0(getwd(),"/data/")

# determine appropriate path depending on where code is being run
if(Sys.info()[["sysname"]]=="Windows"){
  function_script_path ="E:/home/mw418/function_scripts/"
}else{
  function_script_path ="/Users/mw418/codebase/misc/function_scripts/"
  
}

#' Source any functions from the local file
source(paste0(function_script_path,"/load_packages.R"))
source(paste0(function_script_path,"/cats_and_covs.R"))
source(paste0(function_script_path,"/create_subfolder.R"))
source(paste0(function_script_path,"/forest_plot.R"))

source(file = "code/0_functions.R")

#' Pull in packages needed
package.list <- c("knitr","dplyr","tidyr",
                  "ggplot2","gdata","ggsci", "RColorBrewer", "tidyverse", "lubridate", 
                  "ComplexHeatmap",
                  "ggthemes", "gridExtra","scales","ggpubr",
                  "patchwork", "OverReact")
load_packages(package.list)
# pacman::p_load(package.list)

# create subfolder
createMySubfolder(subfolderName = "covid_exclusions_justification")

# no scientific notation
options(scipen = 999)

# import fonts
# extrafont::font_import(prompt =F )



# define data path
datapath="data/bq_exports_mar_2023/"


# Load dimensions dataset (without titles)
dat_cov <- readRDS(paste0(datapath,"clean/notfnal/covid_eng_articles_prefilt.rds"))

# read in all data
dat_all=readRDS(paste0(datapath,"clean/dat_all.rds"))

#load article titles
dat_titles=readRDS("data/bq_exports_mar_2023/clean/titles_all.rds")

# filter
# dat_all_titles <- dat_titles %>% filter(id%in%dat$id)

# load final filtered data set
dat_cov_filt <- readRDS("data/bq_exports_mar_2023/clean/dat_main.rds")


# Conduct random sample ---------------------------------------------------

# get id lists
ids_cov_filt=dat_cov_filt$id
ids_cov_unfilt=dat_cov$id
ids_all=dat_all$id[dat_all$year>=2020]
  
# check correct ratio of inclusion
table(ids_cov_unfilt%in%ids_cov_filt) # checks out
table(ids_cov_unfilt%in%ids_all) # checks out (all)


# take a random sample of the unfiltered papers
set.seed(123)
ids_samp=sample(ids_all,100,replace = F)

# get titles of sampled papers
titles_samp=dat_titles %>% filter(id%in%ids_samp)

# Go through and manually check papers
askIfCOVID <- function(paper_title){
  print(paper_title)
  verdict=readline(prompt = "Is this paper primarily about COVID-19?")
  return(verdict)
}

# cheate empty var
titles_samp$manual_verdict=NA_character_

# Loop over full sample of 100
for (i in 1:100){
  print(i)
  titles_samp$manual_verdict[[i]] <- askIfCOVID(titles_samp$preferred[[i]])
}

# add column for whether paper is in filtered data set
titles_samp$in_filtered_data <- case_when(titles_samp$id%in%ids_cov_filt ~ "Y",
                                          T ~ "N")

# add column for whether paper is in filtered data set
titles_samp$in_dimensions_data <- case_when(titles_samp$id%in%ids_cov_unfilt ~ "Y",
                                          T ~ "N")

# analyse accuracy
# titles_samp$manual_verdict[titles_samp$manual_verdict=="C"] <- "N"

# confusion matrix
table(titles_samp$in_filtered_data)
table(titles_samp$manual_verdict)
table(titles_samp$in_dimensions_data)
conf_mat_whit <- table(titles_samp$manual_verdict,titles_samp$in_filtered_data) %>% as.data.frame.matrix()
conf_mat_dim <- table(titles_samp$manual_verdict,titles_samp$in_dimensions_data) %>% as.data.frame.matrix()

conf_mat_whit
conf_mat_dim

# get TP/FPs etc
tp=conf_mat_dim[2,2]
tn=conf_mat_dim[1,1]
fp=conf_mat_dim[1,2]
fn=conf_mat_dim[2,1]

# accuracy = 
acc=(tn+tp)/100
acc
# precision
precision=tp/(tp+fp)
precision
# recall
recall=tp/(tp+fn)
recall
# F1 score
f1=2*(precision*recall)/(precision+recall)
f1

# create data set to export and save
titles_samp <- titles_samp %>% left_join(dat_all %>% select(id,journal_title))
OverReact::saveREACTtable(tab = titles_samp,outpath = outpath,filename = "list_of_sample_papers")



