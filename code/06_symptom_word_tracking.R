
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
                  "ComplexHeatmap",
                  "ggthemes", "gridExtra","scales","ggpubr",
                  "patchwork", "OverReact")
# load_packages(package.list)
pacman::p_load(package.list)

# create subfolder
createMySubfolder(subfolderName = "eda")

# no scientific notation
options(scipen = 999)

# import fonts
extrafont::font_import(prompt =F )



# Load data ---------------------------------------------------------------


# Save
dat <- readRDS("data/dat_main.rds")
concepts_wide <- readRDS("data/concepts_wide.rds")
text<- readRDS("data/sparse_dfs_1000_min.rds")
text_df <- text$df_text_sparse_abstract_1000
words_all=text_df@Dimnames[[2]]
sympwords=c("smell","taste","cough","chest","muscle","fever","anosmia","ageusia","tiredness","fatigue","chill","shortness",
            "throat","sneezing","sneeze","hoarse","fog","brain","confusion","memory","dizzy","runny",
            "headache","insomnia","anxiety","depression")
intersect(sympwords,words_all)

text_df[,intersect(sympwords,words_all)]

