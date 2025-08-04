
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

# .libPaths()

#' Pull in packages needed
# remove.packages("rlang")
load_packages("rlang")

package.list <- c("prevalence","mgcv","knitr","dplyr","factoextra","tidyr","tidyverse",
                  "ggplot2","gdata","ggsci", "RColorBrewer", "tidyverse", "lubridate", 
                  "ComplexHeatmap","gganimate","ggnetwork","ppcor","tidytext",
                  "readr","ggthemes", "questionr", "gridExtra","scales","ggpubr",
                  "Rtsne","rsvd", "geometry","Rcpp","furrr","future","sharp","foreach",
                  "doParallel","stabilityshap","tvReg",
                  "patchwork", "OverReact")
load_packages(package.list)
pacman::p_load(prevalence,mgcv,knitr,dplyr,factoextra,tidyr,tidyverse,
               ggplot2,gdata,ggsci, RColorBrewer, tidyverse, lubridate, 
               ComplexHeatmap,gganimate,ggnetwork,ppcor,tidytext,
               readr,ggthemes, questionr, gridExtra,scales,ggpubr,
               Rtsne,rsvd, geometry,Rcpp,furrr,future,sharp,foreach,
               doParallel,stabilityshap,tvReg,broom,
               patchwork, OverReact)


# create subfolder
createMySubfolder(subfolderName = "univariate_topics_over_time")
suffix="topics"
# no scientific notation
options(scipen = 999)
# nice colour palette
mynicepalette=c("#455054","#308695","#D45769","#E69D45","#D4CFC9")


# load data
dat <- readRDS("data/dat_main.rds")
ntopic=140
topic_model <- readRDS(paste0("data/topic_model_k_",ntopic,"_all_abstract_10000.rds"))

# get gamma
td_gamma <- tidy(topic_model, matrix = "gamma",
                 document_names = rownames(df_text_sparse))

# get wide td_gamma for modelling
td_gamma_wide=td_gamma %>% pivot_wider(id_cols = document,
                                       values_from = gamma,
                                       names_from = topic)


# get beta
td_beta <- tidy(topic_model)
td_beta
top_terms <- td_beta %>%
  arrange(beta) %>%
  dplyr::group_by(topic) %>%
  top_n(4, beta) %>%
  arrange(-beta) %>%
  dplyr::select(topic, term) %>%
  summarise(terms = list(term)) %>%
  mutate(terms = map(terms, paste, collapse = ", ")) %>% 
  unnest()
top_terms

gamma_terms <- td_gamma %>%
  group_by(topic) %>%
  summarise(gamma = mean(gamma)) %>%
  # arrange(desc(gamma)) %>%
  left_join(top_terms, by = "topic") %>%
  mutate(topic = paste0("Topic ", topic),
         topic = reorder(topic, gamma))

# create modelling df
wordlist=topic_names=paste0(gamma_terms$topic,": ",gamma_terms$terms)
names(td_gamma_wide)[2:ncol(td_gamma_wide)] <- topic_names
df_mod <- dat %>% inner_join(td_gamma_wide, by=c("id"="document"))
df_mod$article_type <- df_mod$type



# Altmetrics score over time ----------------------------------------------
df_mod <- df_mod %>% arrange(date)
df_mod$week <- df_mod$date %>% week()
df_mod$month <- df_mod$date %>% month()
df_mod$year <- df_mod$date %>% year()

df_mod$month_year=paste0(df_mod$year," ",df_mod$month)
df_mod$week_year=paste0(df_mod$year," ",df_mod$week)

allweeks=unique(df_mod$week_year[df_mod$year>=2020])
nweeks=length(allweeks)

df_mod$month_year_factor=factor(df_mod$month_year, levels=unique(df_mod$month_year))
df_mod$`Topic 100: survey, respondents, online, questionnaire`
f=as.formula("altmetrics_score ~ month_year_factor *`Topic 100: survey, respondents, online, questionnaire`")
mod=lm(formula = f,data = df_mod)
summary(mod)





# Loop --------------------------------------------------------------------


# load interesting topics
goodtopics=readRDS("output/characterising_topic_models/topics_of_interest.rds")

# loop over all months to gauge interest changing by month
res_all <- list()
i=1
for(i in 1:(nweeks-12)){
  print(i)
  
  # Run analysis on cites_per_day
  univ_alt_journal <- runUnivariate(mydat = df_mod %>% filter(week_year%in%allweeks[i:(i+12)]),  
                                    preprint_filter=c("article","preprint"), var = "altmetrics_score",
                                    family = "gaussian",wordcount_threshold = 0,
                                    wordlist = wordlist,n = 5)
  univ_alt_journal$week <- allweeks[[i]]
  res_all[[i]] <- univ_alt_journal
  
  }

# closeAllConnections()

# bind all dfs in list
res_all_df=bind_rows(res_all)

# run on all time
univ_alt_journal <- runUnivariate(mydat = df_mod,
                                  preprint_filter=c("article","preprint"), var = "altmetrics_score",
                                  family = "gaussian",wordcount_threshold = 0,
                                  wordlist = wordlist,n = 4)
# get significant topics
univ_alt_journal$variable[univ_alt_journal$significant=="FDR<0.05 (Bonferroni)"]


# convert to factor
res_all_df$significant <- factor(res_all_df$significant,levels=c("Not significant","p<0.05","FDR<0.05 (Bonferroni)"))

# create date varuable
res_all_df$date=as.Date(paste(res_all_df$week, 'Sun'), '%Y %U %a')

# identify changing betas
beta_summary=res_all_df %>% 
  mutate(estimate=case_when(p.value>0.05~NA_real_,
                            T ~ estimate)) %>% 
  group_by(variable) %>% 
  summarise(mean_beta=mean(estimate,na.rm=T),
            min_beta=min(estimate,na.rm=T),
            max_beta=max(estimate,na.rm=T)) %>% 
  mutate(beta_change=sign(min_beta*max_beta))


### Add changing gamme values
topic_summary=df_mod %>% 
  group_by(week_year) %>% 
  summarise_at(.vars = wordlist, .funs = "sum") %>% 
  pivot_longer(cols = -week_year ) %>% 
  rename(week=week_year,
         variable=name)

# join
res_all_df <- res_all_df %>% left_join(topic_summary) %>% left_join(beta_summary)


library(colorspace)

# plot
p_heatmap=topic_summary %>% 
  # filter(variable%in%beta_summary$variable[beta_summary$beta_change<0]) %>%
  filter(variable%in%topic_names[goodtopics]) %>%
  mutate(estimate=case_when(p.value>0.05~NA_real_,
                            T ~ estimate)) %>% 
  ggplot(aes(x=date, y=reorder(variable,mean_beta))) + 
  geom_tile(aes(col=estimate,fill=estimate))+
  # geom_point(aes(size=value), shape =1)+
  # scale_colour_continuous_divergingx(mid = 0,palette = "RdBu",rev=F,p3=1000)+
  # scale_fill_continuous_divergingx(mid = 0,palette = "RdBu",rev=F,p3=1000)+
  scale_fill_gradientn(limits = c(-1000,4000),values = c(0,0.2,1),colours=c("#D45769","white","#308695"),
                       oob = scales::squish,na.value = "transparent")+
  scale_colour_gradientn(limits = c(-1000,4000),values = c(0,0.2,1),colours=c("#D45769","white","#308695"),
                         oob = scales::squish,na.value = "transparent")+
  
  # scale_colour_gradientn(limits = c(-1000,2000),colours=c("#D45769","white","#308695"),oob = scales::squish,na.value = "white")+
  # scale_fill_gradientn(limits = c(-1000,2000),colours=c("#D45769","white","#308695"),oob = scales::squish,na.value = "white")+
  # scale_fill_gradient2(low = "#D45769",mid="white",high="#308695",oob = scales::squish,na.value = "white")+
  # scale_colour_gradient2(low = "#D45769",mid="white",high="#308695",oob = scales::squish,na.value = "white")+
  
  ggthemes::theme_clean() +
  labs(y="",x="", col="\u03b2",fill="\u03b2",
       title="Topics that drive media attention",
       subtitle="\u03b2 coefficients over time in models associating topics with Altmetrics score. Rolling 8-week window. \u03b2s shown where p<0.05") +
  theme(plot.subtitle = element_text(size=10),
        plot.background  = element_blank())
p_heatmap

# save
OverReact::saveREACTplot(p = p_heatmap,figpath = figpath,filename = "heatmap_betas_over_time_altmetrics",
                         width = 12,height = 6,savePDF = T,filetypes = c("jpg","png"))



# plot variation in altmetrics over time

# articles published over time
p_alt_over_time_mean_type <- dat %>% 
  mutate(year=lubridate::year(date),
         week=lubridate::week(date),
         ym=format_ISO8601(date,precision="ym")) %>% 
  filter(date>=as.Date("2020-01-02")) %>%
  filter(date != as.Date("2021-01-01"),date != as.Date("2022-01-01")) %>%
  group_by(ym) %>% 
  summarise(n=n(),
            mean_alt = sum(altmetrics_score, na.rm=T)) %>% 
  # mutate(roll_mean_alt=zoo::rollmean(mean_alt,30, align="right", fill = "extend", na.pad = F)) %>% 
  # filter(date > as.Date("2020-01-01"), date < as.Date("2022-11-24")) %>% 
  ggplot(aes(x=ym, y=mean_alt,group=1)) + 
  geom_point(size=0.6) +
  geom_line(size=0.2)+  
  geom_path()+
  labs(x="Date", 
       # y="Average media and social media attention (Altmetrics score)", 
       y="",
       title="Total media attention",  
       # subtitle = "January 2020 - December 2022",
       col="") +
  ggthemes::theme_clean() +
  theme(plot.title = element_text(size=10))+
  theme(legend.position = c(0.9,0.9),axis.text.x = element_text(angle=70,vjust=1,hjust=1),
        plot.background  = element_blank())

p_alt_over_time_mean_type


p_comb=p_heatmap/p_alt_over_time_mean_type + plot_layout(heights = c(5,1))
p_comb

# save
OverReact::saveREACTplot(p = p_comb,figpath = figpath,filename = "heatmap_betas_over_time_altmetrics_panel",
                         width = 12,height = 9,savePDF = T,filetypes = c("jpg","png"))



# save all results
saveRDS(object = list(topic_summary=topic_summary,
                      p_heatmap=p_heatmap,
                      p_alt_over_time_mean_type=p_alt_over_time_mean_type,
                      p_comb=p_comb),
        file = paste0(outpath,"time_varying_topics_association_res.rds"))

### load topics over time plot to create Panel
p_topics=readRDS("plots/characterising_topic_models/topics_over_time_plot.rds")



p_topics <- p_topics+  theme(plot.title = element_text(size=10))+
  labs(title="Changing topic enrichment over time")

p_comb_2=p_heatmap/p_topics + plot_layout(heights = c(5,3))
p_comb_2

# save
OverReact::saveREACTplot(p = p_comb_2,figpath = figpath,
                         filename = "heatmap_betas_over_time_topic_enrichment_2_panel",
                         width = 14,height = 12,savePDF = T,filetypes = c("jpg","png"))


p_comb_3=p_heatmap/p_topics/p_alt_over_time_mean_type + plot_layout(heights = c(5,3,1))
p_comb_3

# save
OverReact::saveREACTplot(p = p_comb_3,figpath = figpath,
                         filename = "heatmap_betas_over_time_topic_enrichment_3_panel",
                         width = 14,height = 14,savePDF = T,filetypes = c("jpg","png"))





# # plot
# res_all_df %>% 
#   filter(variable%in%beta_summary$variable[beta_summary$beta_change<0]) %>%
#   ggplot(aes(x=date, y=estimate, col=variable,fill=variable)) + 
#   geom_line()+
#   geom_point()+
#   OverReact::theme_react()  +
#   labs(y="",x="")
# 
# 
# 
# 









