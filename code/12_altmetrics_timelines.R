
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

#' Pull in packages needed
package.list <- c("dplyr","factoextra","tidyr",
                  "ggplot2","gdata","ggsci", "RColorBrewer", "tidyverse", "lubridate", 
                  "jtools","sysfonts","stabilityshap","sharp",
                  "tidytext", "quanteda", "widyr", "igraph", "ggraph","stm",
                  "patchwork", "OverReact", "topicmodels","ldatuning", "catboost",
                  "stabilityshap")
load_packages(package.list)

# create subfolder
createMySubfolder(subfolderName = "altmetrics_time_curves")

# no scientific notation
options(scipen = 999)

datapath="output/altmetrics_timelines/"
import_files=list.files(datapath)
timelines_list=lapply(X = paste0(datapath,import_files),read.csv)
datapasta::vector_paste()


pubdates = as.Date(c("2021-11-02", "2020-03-17", "2021-08-25", "2020-11-18", 
                     "2021-06-21", "2020-12-10", "2021-02-02", "2022-02-25", 
                     "2020-03-17", "2021-09-30", "2020-06-01", "2021-11-20", 
                     "2020-11-20", "2020-05-22", "2021-04-15", 
                     "2020-06-11", "2022-09-26", "2022-02-07", "2021-04-21", 
                     "2021-08-06"))
x=2
res_list=list()
for (x in 1:length(import_files)){
  df=timelines_list[[x]]
  df <- df %>% as.data.frame() %>% janitor::clean_names()
  df$date <- as.Date(df$date)
  df <- df %>% filter(date>=pubdates[[x]])
  df <- df %>% 
    mutate(
      social_media_mentions=twitter_mentions+facebook_mentions+reddit_mentions+linked_in_mentions+
        weibo_mentions+pinterest_mentions+video_mentions
    )
  totals=colSums(df[,2:ncol(df)])
  # for(i in 2:ncol(df)){
  #   df[[i]]=cumsum(df[[i]])/totals[[i-1]]
  # }
  df$news_mentions_cumsum=cumsum(df$news_mentions)/totals[["news_mentions"]]
  df$social_media_mentions_cumsum=cumsum(df$social_media_mentions)/totals[["social_media_mentions"]]
  
  
  df$days_since_pub=0:(nrow(df)-1)
  df$paper = import_files[[x]]
  res_list[[x]]=df
}

res_all=bind_rows(res_list)
res_summary=res_all %>% 
  group_by(days_since_pub) %>% 
  summarise(social_media_mentions=mean(social_media_mentions),
            news_mentions=mean(news_mentions))
res_summary$paper="All papers"
res_summary$news_mentions_cumsum=cumsum(res_summary$news_mentions)/sum(res_summary$news_mentions)
res_summary$social_media_mentions_cumsum=cumsum(res_summary$social_media_mentions)/
  sum(res_summary$social_media_mentions)


# combine all  
res_comb=res_all %>% select(names(res_summary)) %>% rbind(res_summary)

# ggplot
p_soc_med=res_all %>% ggplot(aes(x=days_since_pub,y=social_media_mentions_cumsum,group=paper)) +
  geom_path(col="grey90") +
  geom_path(data=res_summary, col="black") +
  OverReact::theme_react() +
  geom_vline(xintercept = 30, linetype="dotted", col="red")+
  geom_point(data=res_summary[res_summary$days_since_pub==30,], col="red") +
  geom_hline(yintercept = res_summary$social_media_mentions_cumsum[which(res_summary$days_since_pub==30)],
             linetype="dotted",col="red") +
  geom_text(data=res_summary[res_summary$days_since_pub==30,],
            aes(label=paste0(round(100*social_media_mentions_cumsum,1),
                             "% of mentions within 30 days")),
            nudge_x = 230,nudge_y = 0.02,col="red")+
  labs(title = "Social media mentions",subtitle = "Cumulative sum over time since publication",
       x="Days since publication", y="Cumulative sum of social media mentions") 

p_soc_med

OverReact::saveREACTplot(p = p_soc_med,figpath = figpath,filename = "soc_med_mentions_cumsum",
                         width = 6,height = 6,savePDF = T,filetypes = c("jpg","png"))


# ggplot
p_news=res_all %>% ggplot(aes(x=days_since_pub,y=news_mentions_cumsum,group=paper)) +
  geom_path(col="grey90") +
  geom_path(data=res_summary, col="black") +
  OverReact::theme_react() +
  geom_vline(xintercept = 30, linetype="dotted", col="red")+
  geom_point(data=res_summary[res_summary$days_since_pub==30,], col="red") +
  geom_hline(yintercept = res_summary$news_mentions_cumsum[which(res_summary$days_since_pub==30)],
             linetype="dotted",col="red") +
  
  labs(title = "News coverage",subtitle = "Cumulative sum over time since publication",
    x="Days since publication", y="Cumulative sum of news mentions") 
OverReact::saveREACTplot(p = p_news,figpath = figpath,filename = "news_mentions_cumsum",
                         width = 6,height = 6,savePDF = T,filetypes = c("jpg","png"))



p_comb=p_news/p_soc_med
OverReact::saveREACTplot(p = p_comb,figpath = figpath,filename = "news_soc_med_mentions_cumsum",
                         width = 6,height = 12,savePDF = T,filetypes = c("jpg","png"))

