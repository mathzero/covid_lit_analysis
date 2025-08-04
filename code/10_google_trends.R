
#' First clear the environment of variables
rm(list=ls(all=TRUE))
# devtools::install_github(repo = "mathzero/OverReact")

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
source(paste0(function_script_path,"/stability_selection.R"))
# source(paste0(function_script_path,"/forest_plot.R"))

source("code/0_functions.R")


# source("E:/Group/react2_study5/report_phases_combined/projects/function_scripts/create_subfolder.R")
# update.packages("Rcpp",repos="http://se-r.sm.med.ic.ac.uk", dependencies = TRUE )
# devtools::install_github("eclarke/ggbeeswarm")

# install.packages("readr")
# install.packages("colorspace")
# install.packages("vctrs")
# install.packages("gtrendsR")
# devtools::install_github("PMassicotte/gtrendsR")
# install.packages("curl")

#' Pull in packages needed
package.list <- c("dplyr","factoextra","tidyr","gtrendsR",
                  "ggplot2","gdata","ggsci", "RColorBrewer", "tidyverse", "lubridate", 
                  "jtools","sysfonts","stabilityshap","sharp",
                  "tidytext", "quanteda", "widyr", "igraph", "ggraph","stm",
                  "patchwork", "OverReact", "topicmodels","ldatuning", "catboost",
                  "stabilityshap")
load_packages(package.list)

# create subfolder
createMySubfolder(subfolderName = "google_trends")

# no scientific notation
options(scipen = 999)


# some colours

myCols=c("#960078","#003E74","#02893B","#DD2501")

myColsExt=c("#002147", "#003E74", "#EBEEEE", "#9D9D9D", "#D4EFFC", "#373A36", 
            "#006EAF", "#0091D4", "#00ACD7", "#0f8291", "#009CBC", "#379f9f", 
            "#02893B", "#66a40a", "#BBCE00", "#D24000", "#EC7300", "#FFDD00", 
            "#A51900", "#DD2501", "#E40043", "#9F004E", "#C81E78", "#751E66", 
            "#960078", "#321E6D", "#653098")



# Analysis ----------------------------------------------------------------

words_variants=c("COVID-19","Alpha Variant","Beta Variant","Delta variant","Omicron variant")
words_treatments=c("COVID-19","Vaccine","Hydroxychloroquine","Ivermectin","Dexamethasone")

res_web_vars <- gtrendsR::gtrends(keyword = words_variants)
res_news_vars <- gtrendsR::gtrends(keyword = words_variants, time = "2019-01-01 2023-01-01",gprop = "news")


res_news <- gtrendsR::gtrends(keyword = words, time = "2019-01-01 2023-01-01",geo = c("all"),gprop = "news")


