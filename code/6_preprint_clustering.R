
#' First clear the environment of variables
rm(list=ls(all=TRUE))

outpath <- paste0(getwd(),"/output/")
figpath <- paste0(getwd(),"/plots/")
datapath <- paste0(getwd(),"/data/")
function_script_path ="E:/home/mw418/function_scripts/"
function_script_path="/Users/mathzero/Library/CloudStorage/GoogleDrive-mattdwhitaker@gmail.com/My Drive/Imperial/PhD/REACT2_analysis_in_progress/RESULTS/function_scripts/"


#' Source any functions from the local file
source(paste0(function_script_path,"/load_packages.R"))
source(paste0(function_script_path,"/cats_and_covs.R"))
source(paste0(function_script_path,"/create_subfolder.R"))
source(paste0(function_script_path,"/stability_selection.R"))

source("code/0_functions.R")


#' Pull in packages needed
package.list <- c("dplyr","tidyr","ggbeeswarm",
                  "ggplot2","gdata","ggsci", "RColorBrewer", "tidyverse", "lubridate", 
                  "ComplexHeatmap","gganimate","ggnetwork","ppcor","topicmodels",
                  "tidytext", "quanteda", "widyr", "igraph", "ggraph","stm","ldatuning",
                  "readr","ggthemes", "questionr", "gridExtra","qdapDictionaries","sharp",
                  "doParallel","ggpubr",
                  "patchwork", "OverReact", "topicmodels","ldatuning")
load_packages(package.list)

# create subfolder
createMySubfolder(subfolderName = "preprint_clustering")

# no scientific notation
options(scipen = 999)

# Load data ---------------------------------------------------------------

dat <- readRDS("data/dat_main.rds")
topic_model <- readRDS("data/topic_model_k_70_all_abstract_10000.rds")
ntopic=70

# preprints
dat_preprint <- dat %>% filter(preprint=="Preprint")

# get gamma matrix
gamma_mat <- data.frame(unique_id=topic_model@documents,topic_model@gamma)
colnames(gamma_mat) <- c("unique_id", paste0("topic_",1:ntopic))

# get preprint mat
gamma_mat_preprint=gamma_mat %>% filter(unique_id %in% dat_preprint$unique_id)

prop.table(table(dat$preprint,dat$published),1)


# Clustering  ----------------------------------------------------------
hist((mymat)[,1])
mymat=(gamma_mat_preprint[,2:(ntopic+1)])
mymat=scale(mymat) %>% as.data.frame()
# sample 50k
set.seed(123)
mymat=mymat[sample(1:nrow(mymat),size = 50000,replace = F),]
distmat=dist(mymat)
res_list=list()
res_list[[1]]=NULL
sils=list()
sils[[1]]=NULL
sil_res=data.frame(k=2:20,sil_kmeans=NA_real_, sil_kmed=NA_real_)
for(k in 2:20){
  print(k)
  res_list[[k]] <- kmeans(x = mymat,centers = k)
  kmed=cluster::pam(x = distmat,k = k,)
  sils[[k]]=cluster::silhouette(x = res_list[[k]]$cluster,dist=distmat)
  sils_kmed=cluster::silhouette(x = res_list[[k]]$cluster,dist=distmat)
  sil_res[k-1,2] <- sils[[k]][,3] %>% mean()
  sil_res[k-1,3]=kmed$silinfo$avg.width
}

### Hierarchical clustering
hclust_obj=hclust(d = distmat,method = "average")
plot(hclust_obj)
plot(mymat$topic_1,mymat$topic_22)



