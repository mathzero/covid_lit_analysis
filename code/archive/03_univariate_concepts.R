
#' First clear the environment of variables
rm(list=ls(all=TRUE))

outpath <- paste0(getwd(),"/output/")
figpath <- paste0(getwd(),"/plots/")
datapath <- paste0(getwd(),"/data/")
function_script_path ="E:/home/mw418/function_scripts/"
function_script_path="/Users/mathzero/Library/CloudStorage/GoogleDrive-mattdwhitaker@gmail.com/My Drive/Imperial/PhD/REACT2_analysis_in_progress/RESULTS/function_scripts/"
function_script_path ="/Users/mw418/analysis/RESULTS/function_scripts/"


#' Source any functions from the local file
source(paste0(function_script_path,"/load_packages.R"))
source(paste0(function_script_path,"/cats_and_covs.R"))
source(paste0(function_script_path,"/create_subfolder.R"))
source(paste0(function_script_path,"/forest_plot.R"))

source(file = "code/0_functions.R")

#' Pull in packages needed
package.list <- c("knitr","dplyr","tidyr",
                  "ggplot2","gdata","ggsci", "RColorBrewer", "tidyverse", "lubridate", 
                  "ComplexHeatmap","parallel","snow","doParallel",
                  "ggthemes", "gridExtra","scales","ggpubr",
                  "patchwork", "OverReact")
# load_packages(package.list)
pacman::p_load(knitr,dplyr,tidyr,
               ggplot2,gdata,ggsci, RColorBrewer, tidyverse, lubridate, 
               ComplexHeatmap,parallel,snow,doParallel,
               ggthemes, gridExtra,scales,ggpubr,
               patchwork, OverReact)

library(doParallel)
library(tidytext)

 # create subfolder
createMySubfolder(subfolderName = "univariate_concepts")

# no scientific notation
options(scipen = 999)

# import fonts
extrafont::font_import(prompt =F )




# Save
dat <- readRDS("data/dat_main.rds")
concepts <- read_csv("data/bq-results-20221124-concepts.csv")
concepts_wide <- readRDS("data/concepts_wide.rds")

table(dat$type,dat$published)
table(dat$type)

# get wordlist for function
wordlist = names(concepts_wide)[2:ncol(concepts_wide)]

# Na to zero in concepts
concepts_wide <- concepts_wide %>% 
  mutate_if(is.numeric, ~replace(., is.na(.), 0))



# create holdout data set
df_mod <- dat %>% 
  # mutate(unique_id=as.numeric(unique_id)) %>% 
  # filter(date <= as.Date("2020-06-01")) %>% 
  dplyr::select(title_preferred,published,type,cites_per_day,
                altmetrics_score,unique_id,date,id) %>% 
  rename(date_published=date,
         published_in_journal=published
  ) %>% 
  rename(article_type=type) %>% 
  left_join(concepts_wide, by="id") %>% 
  dplyr::select(-unique_id,-id)

# complete cases
df_mod <- df_mod[rowSums(is.na(df_mod),na.rm=T)==0,]




# Run analyses ------------------------------------------------------------

df_mod[,wordlist]
# Run analysis on cites_per_day
univ_cpd_journal <- runUnivariate(mydat = df_mod, preprint_filter=c("article"),
                                  var = "cites_per_day",wordlist = wordlist,
                                  family = "gaussian",wordcount_threshold = 100,
                                  n = 4)
univ_cpd_journal$journal <- "Journal article"

# Run analysis on cites_per_day
univ_cpd_preprint <- runUnivariate(mydat = df_mod, preprint_filter=c("Preprint"),
                                   var = "cites_per_day",wordlist = wordlist,
                                   family = "gaussian",wordcount_threshold = 10,n = 4)
univ_cpd_preprint$journal <- "Preprint"

# Run analysis on cites_per_day
univ_alt_journal <- runUnivariate(mydat = df_mod,  preprint_filter=c("Journal article"), var = "altmetrics_score",
                                  family = "gaussian",wordcount_threshold = 100,
                                  wordlist = wordlist,n = 4)
univ_alt_journal$journal <- "Journal article"

# Run analysis on cites_per_day
univ_alt_preprint <- runUnivariate(mydat = df_mod,  preprint_filter=c("Preprint"),var = "altmetrics_score",
                                   wordlist = wordlist,
                                   family = "gaussian",wordcount_threshold = 10,n = 4)
univ_alt_preprint$journal <- "Preprint"



# Run analysis on published
univ_pub_preprint <- runUnivariate(mydat = df_mod %>% filter(date<="2020-06-01"),  preprint_filter=c("Preprint"),
                                   var = "published",wordlist = wordlist,
                                   family = "binomial",wordcount_threshold = 20,n = 4)
univ_pub_preprint$journal <- "Preprint"



