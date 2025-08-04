
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
createMySubfolder(subfolderName = "eda")

# no scientific notation
options(scipen = 999)

# import fonts
# extrafont::font_import(prompt =F )



# Load data ---------------------------------------------------------------


# Save
dat <- readRDS("data/bq_exports_mar_2023/clean/dat_main.rds")
# concepts_wide <- readRDS("data/concepts_wide.rds")
text<- readRDS("data/sparse_dfs_1000_min.rds")
text_df <- text$df_text_sparse_abstract_1000
words_all=text_df@Dimnames[[2]]
sympwords=c("smell","taste","cough","chest","muscle","fever","anosmia","ageusia","tiredness","fatigue","chill","shortness","shortness of breath",
            "throat","sneezing","sneeze","hoarse","fog","brain","brain fog","confusion","memory","memory loss","dizzy","runny",
            "headache","insomnia","anxiety","depression")
foundwords=intersect(sympwords,words_all)
text_df <- text_df %>% as.matrix() %>% as.data.frame()
text_df_symps=text_df[,foundwords]
text_df_symps$id=rownames(text_df_symps)

# join with dat to get metadata
text_df_symps <- text_df_symps %>% left_join(dat, by="id")



# descriptive analysis ------------------------------------------------------------

symp_summary_df=text_df_symps %>% 
  filter(date>as.Date("2020-01-01")) %>% 
  group_by(mon=zoo::as.yearmon(date)) %>% 
  summarise_at(foundwords,mean,na.rm = TRUE)
  
# create simple plot
symp_summary_df %>% 
  select(mon,smell,taste,cough,throat,headache
         # anxiety,depression
         ) %>% 
  pivot_longer(cols = -mon) %>% 
  ggplot(aes(x=mon,y=value, col=name)) +
  geom_point()+
  geom_line()+
  OverReact::theme_react()


tttt=text_df_symps %>% 
  filter(date>as.Date("2020-01-01"),headache==1) %>% 
  group_by(mon=zoo::as.yearmon(date)) %>% 
  summarise(n=n(),
            sum_alt=sum(score,na.rm=T),
            sum_cite=sum(citations_count,na.rm=T))



