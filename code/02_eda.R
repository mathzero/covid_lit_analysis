
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
package.list <- c("prevalence","mgcv","knitr","dplyr","factoextra","tidyr",
                  "ggplot2","gdata","ggsci", "RColorBrewer", "tidyverse", "lubridate", 
                  "ComplexHeatmap","gganimate","ggnetwork","ppcor","topicmodels",
                  "tidytext", "quanteda", "widyr", "igraph", "ggraph","stm","ldatuning",
                  "readr","ggthemes", "questionr", "gridExtra","scales","ggpubr",
                  "Rtsne","rsvd", "geometry","Rcpp","furrr","future","rjags","sysfonts",
                  "patchwork", "OverReact")
load_packages(package.list)

# create subfolder
createMySubfolder(subfolderName = "eda")

# no scientific notation
options(scipen = 999)

# Load data ---------------------------------------------------------------


# Save
dat <- readRDS("data/dat_main.rds")

dat <- dat %>% mutate(abstract_available = case_when(is.na(abstract_preferred) ~ "Yes",
                                                     T ~ "No"),
                      dummy="nobs",
                      year_factor=as.factor(year)) %>% 
  mutate(across(c("citations_count","altmetrics_score","cited_by_tweeters_count","cited_by_msm_count"),
                ~replace_na(.x,0)))

rowvar_list=c("dummy","year_factor","citations_count","published",
              "altmetrics_score","cited_by_tweeters_count","cited_by_msm_count")
# cov_names=as.list(rowvar_list)
cov_names=list("Papers published","Year","Number of citations",
               "Published in journal","Altmetrics score","Number of tweets","Number of media mentions")
names(cov_names)=rowvar_list
tab1 <- OverReact::crossTabMulti(dat=dat,rowvar_list = rowvar_list,colvar = "type",
                                 cov_names = cov_names,include_percentages = T,
                                 rowwise_precentages = F,confint = F,comma_thousands = T)

OverReact::saveREACTtable(tab = tab1,outpath = outpath,filename = "table_one")



# Get summary of papers by topic ------------------------------------------

dat$title_preferred

summary_category=dat %>% 
  filter(!is.na(journal_category_1),journal_category_1!="") %>% 
  group_by(journal_category_1) %>% 
  summarise(n_papers=n(),
            n_journals=n_distinct(journal_title),
            mean_altmetric_score=mean(altmetrics_score, na.rm=T),
            mean_citations=mean(citations_count, na.rm=T),
            mean_journal_IF=mean(if_2022, na.rm=T)
  ) %>% 
  arrange(-n_papers) %>% slice_head(n=20)



summary_journal=dat %>% 
  mutate(if_2022=case_when(is.na(if_2022) ~ 0,
                           T ~ if_2022)) %>% 
  # filter(!is.na(journal_category_1),journal_category_1!="") %>% 
  group_by(journal_title) %>% 
  summarise(n_papers=n(),
            mean_altmetric_score=mean(altmetrics_score, na.rm=T),
            mean_citations=mean(citations_count, na.rm=T),
            journal_IF=mean(if_2022, na.rm=T)
  ) %>% 
  arrange(-n_papers) %>% 
  slice_head(n=20)

OverReact::saveREACTtable(tab = summary_category,outpath = outpath,filename = "journal_subject_metrics_summary")
OverReact::saveREACTtable(tab = summary_journal,outpath = outpath,filename = "journal_metrics_summary")

