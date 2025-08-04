
#' First clear the environment of variables
rm(list=ls(all=TRUE))

outpath <- paste0(getwd(),"/output/")
figpath <- paste0(getwd(),"/plots/")
datapath <- paste0(getwd(),"/data/")
# function_script_path ="E:/home/mw418/function_scripts/"
# function_script_path="/Users/mathzero/Library/CloudStorage/GoogleDrive-mattdwhitaker@gmail.com/My Drive/Imperial/PhD/REACT2_analysis_in_progress/RESULTS/function_scripts/"
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
load_packages(package.list)
# pacman::p_load(package.list)

# create subfolder
createMySubfolder(subfolderName = "uk_gov_cases")

# no scientific notation
options(scipen = 999)

# load data
dat=read_csv("data/ukgov_dash_data_cases.csv") %>% 
  as.data.frame() %>% 
  arrange(date)


# manipulate
p_cases=dat %>% 
  filter(date>=as.Date("2020-05-01"),date<=as.Date("2023-01-01")) %>% 
  ggplot(aes(x=date,y=newCasesBySpecimenDate)) +
  geom_col(col="#0091D4") +
  theme_clean() +
  scale_x_date(date_breaks = "month",date_labels = "%B",)+
  labs(x="",y="Cases recorded by UK Government")

p_cases
OverReact::saveREACTplot(p = p_cases,figpath = figpath,filename = "uk_cases_plot",width = 30,height = 4,savePDF = T,filetypes = c("png","jpg"))
