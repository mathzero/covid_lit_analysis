
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
                  "ComplexHeatmap","tidytext",
                  "ggthemes", "gridExtra","scales","ggpubr",
                  "patchwork", "OverReact")
load_packages(package.list)
library(tidyverse)  # data manipulation
library(cluster)    # clustering algorithms
library(factoextra) # clustering algorithms & visualization


# create subfolder
createMySubfolder(subfolderName = "topic_clustering")

# no scientific notation
options(scipen = 999)

# load topic data
ntopic=140
topic_model <- readRDS(paste0("data/topic_model_k_",ntopic,"_all_abstract_10000.rds"))
topic_model@Dim

# get beta
td_beta <- tidy(topic_model)
td_beta


# get wide gamma
td_beta_wide=td_beta %>% filter(term!="topic") %>% 
  pivot_wider(id_cols = topic,
                                     names_prefix = "",
                                     # names_repair = T,
                                     names_from = term,
                                     values_from = beta)

# get gamma
td_gamma <- tidy(topic_model, matrix = "gamma",
                 document_names = rownames(df_text_sparse))

# get wide td_gamma for modelling
td_gamma_wide=td_gamma %>% pivot_wider(id_cols = document,
                                       values_from = gamma,
                                       names_from = topic)

# get sample to test runtime
td_gamma_wide_samp=td_gamma_wide[1:100000,2:(ntopic+1)]
df=td_gamma_wide
k.values=c(2:15)
km_list=list()
for(k in k.values){
  print(k)
  km_res=kmeans(td_gamma_wide,centers = k, nstart = 25)
  km_list[[k]]=  km_res
}


# get within sum of squares
wss_values <- c()
for(i in 2:15){
  wss_values <- c(wss_values,km_list[[i]]$tot.withinss)
  
}

plot(k.values, wss_values,
     type="b", pch = 19, frame = FALSE, 
     xlab="Number of clusters K",
     ylab="Total within-clusters sum of squares")


# create plot
p_clust_5=fviz_cluster(km_list[[5]], data = df)
p_clust_5











# Scratch -----------------------------------------------------------------




km_res$tot.withinss

set.seed(123)

# function to compute total within-cluster sum of square 
wss <- function(k) {
  kmeans(df, k, nstart = 10 )$tot.withinss
}

# Compute and plot wss for k = 1 to k = 15
k.values <- 1:15

# extract wss for 2-15 clusters
wss_values <- map_dbl(k.values, wss)

plot(k.values, wss_values,
     type="b", pch = 19, frame = FALSE, 
     xlab="Number of clusters K",
     ylab="Total within-clusters sum of squares")







xmeans_res=Xmeans(
  td_gamma_wide_samp,
  kmax,
  nrow = -1,
  ncol = -1,
  iter.max = 20,
  nthread = -1,
  init = c("forgy"),
  tolerance = 1e-06,
  dist.type = c("eucl", "cos", "taxi"),
  min.clust.size = 1
)

library("RWeka") # requires Java SE Runtime
WPM("refresh-cache") # Build Weka package metadata cache
WPM("install-package", "XMeans") # Install XMeans package if not previously installed

weka_ctrl <- Weka_control( # Create a Weka control object to specify our parameters
  I = 100, # max no iterations overall
  M = 100, # max no iterations in the kmeans loop
  L = 2,   # min no clusters
  H = 5,   # max no clusters
  D = "weka.core.EuclideanDistance", # distance metric
  C = 0.4, S = 1)

x_means <- XMeans(df, control = weka_ctrl) # run algorithm on data



