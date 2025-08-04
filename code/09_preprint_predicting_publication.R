
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
createMySubfolder(subfolderName = "preprint_predicting_publication")

# no scientific notation
options(scipen = 999)


# some colours

myCols=c("#960078","#003E74","#02893B","#DD2501")

myColsExt=c("#002147", "#003E74", "#EBEEEE", "#9D9D9D", "#D4EFFC", "#373A36", 
            "#006EAF", "#0091D4", "#00ACD7", "#0f8291", "#009CBC", "#379f9f", 
            "#02893B", "#66a40a", "#BBCE00", "#D24000", "#EC7300", "#FFDD00", 
            "#A51900", "#DD2501", "#E40043", "#9F004E", "#C81E78", "#751E66", 
            "#960078", "#321E6D", "#653098")



# function to recolour x axis variables
colorVectoriser=function(var,myvars_list){
  if(var %in% myvars_list[[1]]){
    out="#02893B"
  }else if(var %in% data.dict$desc[1:4]){
    out="#DD2501"
  }else{
    out="#003E74"
  }
}

# aply colour vectoriser
recolorAxis=function(p,nvars=20){
  colour_vector=lapply(X = p$data$var[1:nvars],FUN = colorVectoriser,myvars=myvars_list)
  p <- p+
    theme(axis.text.y = element_text(colour = rev(unlist(colour_vector))))
  p
  
}

# aply colour vectoriser
recolorAxisVect=function(p,nvars=20,vect){
  colour_vector=lapply(X = vect,FUN = colorVectoriser,myvars=myvars_list)
  p <- p+
    theme(axis.text.y = element_text(colour = rev(unlist(colour_vector))))
  p
  
}


recolorAxisUniv=function(p){
  colour_vector=lapply(X = p$data$desc,FUN = colorVectoriser,myvars=myvars_list)
  p <- p+
    theme(axis.text.y = element_text(colour = rev(unlist(colour_vector))))
  p
}


# fake data for legend
fakedat=data.frame(vartype=c("Altmetrics","Citations","Topics"), 
                   cols=c("#003E74","#DD2501","#02893B"),
                   var="Total citations",featimp=0)


# fake data for legend
fakedat_univ=data.frame(vartype=c("Altmetrics","Citations","Topics"), 
                   cols=c("#003E74","#DD2501","#02893B"),
                   var="Total citations",featimp=0,
                   desc="Total citations",exp_est=0,
                   p=0)


# Load data ---------------------------------------------------------------

# Save
dat <- readRDS("data/bq_exports_mar_2023/clean/dat_main.rds")
bow <- readRDS("data/sparse_dfs_1000_min.rds")
dat_text_titles <- bow$df_text_sparse_title %>% as.matrix() %>% as.data.frame()
dat_text_titles$id=rownames(dat_text_titles)
rm(bow)
ntopic=140
topic_model <- readRDS(paste0("data/topic_model_k_",ntopic,"_all_abstract_10000.rds"))


# get beta
td_beta <- tidy(topic_model)
td_beta
# dat_text_titles$unique_id <- rownames(dat_text_titles)


# get gamma
td_gamma <- tidy(topic_model, matrix = "gamma",
                 document_names = rownames(df_text_sparse))

td_gamma

# # get wide td_gamma for modelling
# td_gamma_wide=td_gamma %>% pivot_wider(id_cols = document,
#                                        values_from = gamma,
#                                        names_from = topic)
# 
# head(td_gamma_wide)


# # get wide gamma
td_gamma_wide=td_gamma %>% pivot_wider(id_cols = document,
                                       names_prefix = "topic_",
                                       names_from = topic,values_from = gamma)



# td_gamma_wide$document <- as.numeric(td_gamma_wide$document)

dat$type %>% table(exclude="none")
# join with main data
dat_mod <- dat %>% 
  # mutate(published_in_journal=ifelse(!is.na(resulting_publication_doi),1,0)) %>% 
  left_join(td_gamma_wide, by =c("id"="document")) %>% 
  # left_join(dat_text_titles, by =c("unique_id")) %>% 
  rename(date_published=date
  ) %>% 
  rename(article_type=type) %>% 
  filter(year==2020, 
         !is.na(topic_1),
         # journal_title %in% c("medRxiv","bioRxiv","arXiv"),
         article_type=="preprint") %>% 
  filter(grepl("rxiv",tolower(journal_title))) # remove all the proprietary preprint servers


# add title word data (remove text columns that are the same as columns in dat_mod)
commonnmes=intersect(names(dat_text_titles), names(dat_mod))
commonnmes <- setdiff(commonnmes,"unique_id")
dat_mod <- dat_mod %>%  left_join(dat_text_titles, by =c("id")) 




#  Define families of features --------------------------------------------



# Get top beta terms
top_terms <- td_beta %>%
  arrange(beta) %>%
  dplyr::group_by(topic) %>%
  top_n(4, beta) %>%
  arrange(-beta) %>%
  dplyr::select(topic, term) %>%
  summarise(terms = list(term)) %>%
  mutate(terms = map(terms, paste, collapse = ", ")) %>% 
  unnest()

top_terms$var <- paste0("topic_",top_terms$topic)
top_terms$var_comb <- paste0(top_terms$var,": ",top_terms$terms)


names(dat_mod)[grepl("topic_",names(dat_mod))] <- top_terms$var_comb
topiccvars=top_terms$var_comb
citationvars=c("citations_count","cites_per_day","relative_citation_ratio","field_citation_ratio")
altmetricvars=c(names(dat_mod)[grepl("cited_by",names(dat_mod))],
                names(dat_mod)[grepl("cohorts",names(dat_mod))],
                names(dat_mod)[grepl("readers",names(dat_mod))],
                names(dat_mod)[grepl("_rank",names(dat_mod))],
                names(dat_mod)[grepl("_pct",names(dat_mod))],
                "altmetrics_score")
altmetricvars <- altmetricvars[!grepl("topic_",altmetricvars)]
altmetricvars <- altmetricvars[!grepl("connotea",altmetricvars)]
altmetricvars <- altmetricvars[!grepl("citeul",altmetricvars)]

covars=c("days_since_publication")
titlevars=setdiff(names(dat_text_titles),c(commonnmes,"unique_id"))
myvars=c(
  topiccvars,
  citationvars,
  altmetricvars
  # titlevars
)
myvars_list=list(
  topiccvars,
  citationvars,
  altmetricvars
  # titlevars
)


dat_mod %>% 
  dplyr::select(all_of(c(citationvars,altmetricvars))) %>% is.na() %>% sum()




# Univariable modelling ---------------------------------------------------

# function to scale variables
myscaler=function(x){
  (x-mean(x,na.rm=T))/ (sd(x,na.rm=T))
}

univ_mods=dat_mod %>% 
  dplyr::select(all_of(c(citationvars,altmetricvars))) %>%  # exclude outcome, leave only predictors 
  # rename(altmetrics_score=score) %>% 
  mutate(across(c(citationvars,altmetricvars),
                ~replace_na(.x,0))) %>%
  mutate(across(c(citationvars,altmetricvars),
                ~myscaler(.x))) %>%
  map(~glm(dat_mod$published_in_journal ~ .x, data = dat_mod, family="binomial")) %>% 
  map(jtools::summ, exp=T,confint = TRUE,digits = 3)


getModSummary <- function(md){
  coeftab=as.data.frame(md$coeftable)
  summm=summary(md$model)
  coeftab$r2 <- summm$r.squared
  return(coeftab[2,])
}

univ_mods_coefs=lapply(univ_mods,getModSummary)
univ_df=bind_rows(univ_mods_coefs)
univ_df$predictor=names(univ_mods_coefs)
univ_df <- univ_df %>% janitor::clean_names()
# # univ_df$est <- exp(univ_df$est)
# univ_df$x2_5_percent <- exp(univ_df$x2_5_percent)
# univ_df$x97_5_percent <- exp(univ_df$x97_5_percent)

data.dict=data.frame(var=univ_df$predictor)
data.dict$var
data.dict$desc=c("Total citations","Citations / day","Relative citation ratio",
                 "Field citation ratio", "Total mentions online","Twitter mentions",
                 # "Recent citations", 
                 
                 "Number of online accounts mentioning paper","Wikipedia mentions","Mainstream media mentions",
                 "Blog mentions","Peer review site mentions","Facebook mentions",
                 
                 "Reddit user mentions","Policy document mentions", "YouTube channel mentions",
                 
                 "Cohort: scientist mentions","Cohort: general public mentions","Cohort: science communicator mentions",
                 "Cohort: doctors and medics mentions",
                 # "Readers on Citeulike",
                 "Readers on Mendeley", 
                 
                 # "Readers on Connotea",
                 "Readers on all reference managers","Altmetrics score rank: all journals",
                 "Altmetrics score rank: within journal", "Altmetrics score rank: among similarly aged papers", "Altmetrics score rank: among similarly aged papers within journal",
                 
                 "Altmetrics score percentile: all journals", "Percentile: within journal","Altmetrics score percentile: among similarly aged papers",
                 "Altmetrics score percentile: among similarly aged papers within journal","Altmetrics score")


write_csv(x = data.dict,"data/data_dictionary.csv")
univ_df <-univ_df %>%  left_join(data.dict,by=c("predictor"="var"))
univ_df$source=c(rep("Citations",4),rep("Altmetrics",nrow(univ_df)-4))
univ_df <- univ_df %>% 
  rename(lower=x2_5_percent,
         upper=x97_5_percent)

# Create forest plot
p_univ_ors=univ_df %>% 
  filter(!is.na(lower)) %>% 
  arrange(-exp_est) %>% 
  ggplot(aes(x=reorder(desc,exp_est),y=exp_est,shape=p<0.05, col=source))+
  geom_point() +
  scale_shape_manual(values = c(1,15),guide="none")+
  geom_errorbar(aes(ymin=lower,ymax=upper))+
  OverReact::theme_react() +
  OverReact::scale_color_imperial(palette = "short", rev=F)+
  coord_flip() +
  # geom_col(data=fakedat_univ[1:2,], aes(fill=vartype), alpha=1)  +
  scale_fill_manual(values = c("#003E74","#DD2501","#02893B"))+  
  scale_colour_manual(values = c("#003E74","#DD2501","#02893B"))+  
  
  scale_y_log10(breaks=seq(0.8,1.5,0.1))+
  geom_hline(yintercept=1, linetype="dashed", col="grey40",linewidth=0.5) +
  labs(y="Odds ratio",x="", col="Data source",fill=element_blank(),
       title = "Univariable odds ratios for subsequent publication",
       subtitle = paste0("Among N=",nrow(dat_mod)," preprints published in 2020")) +
  theme(legend.position = c(0.85,0.15),
        legend.background = element_rect(fill = "white", color = "black"))


p_univ_ors <- recolorAxisUniv(p_univ_ors)
p_univ_ors





### save plot
OverReact::saveREACTplot(p = p_univ_ors,
                         figpath = figpath,filetypes=c("jpg","png"),
                         filename = "univ_preprint_publication",
                         width = 8,height = 7,savePDF = T)





### same analysis for publication in top journal ###

dat_mod$subs_pub_journal_if_5[is.na(dat_mod$subs_pub_journal_if_5)] <- 0
dat_mod$subs_pub_journal_if_10[is.na(dat_mod$subs_pub_journal_if_10)] <- 0

univ_mods_5=dat_mod %>% 
  dplyr::select(all_of(c(citationvars,altmetricvars))) %>%  # exclude outcome, leave only predictors 
  mutate(across(c(citationvars,altmetricvars),
                ~replace_na(.x,0))) %>%
  mutate(across(c(citationvars,altmetricvars),
                ~myscaler(.x))) %>%
  map(~glm(dat_mod$subs_pub_journal_if_5 ~ .x, data = dat_mod, family="binomial")) %>% 
  map(jtools::summ, exp=T,confint = TRUE,digits = 3)


univ_mods_coefs_5=lapply(univ_mods_5,getModSummary)
univ_df_5=bind_rows(univ_mods_coefs)
univ_df_5$predictor=names(univ_mods_coefs)
univ_df_5 <- univ_df_5 %>% janitor::clean_names()
# univ_df_5$est <- exp(univ_df_5$est)
# univ_df_5$x2_5_percent <- exp(univ_df_5$x2_5_percent)
# univ_df_5$x97_5_percent <- exp(univ_df_5$x97_5_percent)
univ_df_5 <-univ_df_5 %>%  left_join(data.dict,by=c("predictor"="var"))
univ_df_5$source=c(rep("Citations",4),rep("Altmetrics",nrow(univ_df_5)-4))
univ_df_5 <- univ_df_5 %>% 
  rename(lower=x2_5_percent,
         upper=x97_5_percent)

# Create forest plot
p_univ_ors_IF5=univ_df_5 %>% 
  filter(!is.na(lower)) %>% 
  arrange(-exp_est) %>% 
  ggplot(aes(x=reorder(desc,exp_est),y=exp_est, shape=p<0.05, col=source))+
  geom_point() +
  scale_shape_manual(values = c(1,15),guide="none")+
  geom_errorbar(aes(ymin=lower,ymax=upper))+
  OverReact::theme_react() +
  OverReact::scale_color_imperial(palette = "short", rev=F)+
  coord_flip() +
  # geom_col(data=fakedat_univ[1:2,], aes(fill=vartype), alpha=1)  +
  scale_fill_manual(values = c("#003E74","#DD2501","#02893B"))+  
  scale_colour_manual(values = c("#003E74","#DD2501","#02893B"))+  
  scale_y_log10(breaks=c(seq(0.6,3.6,0.2)))+
  geom_hline(yintercept=1, linetype="dashed", col="grey40",linewidth=0.5) +
  labs(y="Odds ratio",x="", col="Data source",fill=element_blank(),
       title = "Univariable odds ratios for subsequent \npublication in a high-impact journal (IF 5+)",
       subtitle = paste0("Among N=",nrow(dat_mod)," preprints published in 2020")) +
  theme(legend.position = c(0.85,0.15),
        legend.background = element_rect(fill = "white", color = "black"))

p_univ_ors_IF5 <- recolorAxisUniv(p_univ_ors_IF5)

p_univ_ors_IF5

### save plot
OverReact::saveREACTplot(p = p_univ_ors_IF5,
                         figpath = figpath,filetypes=c("jpg","png"),
                         filename = "univ_preprint_publication_IF5",
                         width = 8,height = 7,savePDF = T)





### same analysis for publication in top journal ###
univ_mods_10=dat_mod %>% 
  dplyr::select(all_of(c(citationvars,altmetricvars))) %>%  # exclude outcome, leave only predictors 
  mutate(across(c(citationvars,altmetricvars),
                ~replace_na(.x,0))) %>%
  mutate(across(c(citationvars,altmetricvars),
                ~myscaler(.x))) %>%
  map(~glm(dat_mod$subs_pub_journal_if_10 ~ .x, data = dat_mod, family="binomial")) %>% 
  map(jtools::summ, exp=T,confint = TRUE,digits = 3)


univ_mods_coefs_10=lapply(univ_mods_10,getModSummary)
univ_df_10=bind_rows(univ_mods_coefs_10)
univ_df_10$predictor=names(univ_mods_coefs_10)
univ_df_10 <- univ_df_10 %>% janitor::clean_names()
# univ_df$est <- exp(univ_df$est)
# univ_df$x2_5_percent <- exp(univ_df$x2_5_percent)
# univ_df$x97_5_percent <- exp(univ_df$x97_5_percent)
univ_df_10 <-univ_df_10 %>%  left_join(data.dict,by=c("predictor"="var"))
univ_df_10$source=c(rep("Citations",4),rep("Altmetrics",nrow(univ_df_10)-4))
univ_df_10 <- univ_df_10 %>% 
  rename(lower=x2_5_percent,
         upper=x97_5_percent)

# Create forest plot
p_univ_ors_IF10=univ_df_10 %>% 
  filter(!is.na(lower)) %>% 
  arrange(-exp_est) %>% 
  ggplot(aes(x=reorder(desc,exp_est),y=exp_est,shape=p<0.05, col=source))+
  geom_point() +
  scale_shape_manual(values = c(1,15),guide="none")+
  geom_errorbar(aes(ymin=lower,ymax=upper))+
  OverReact::theme_react() +
  OverReact::scale_color_imperial(palette = "short", rev=F)+
  coord_flip() +
  # geom_col(data=fakedat_univ[1:2,], aes(fill=vartype), alpha=1)  +
  scale_fill_manual(values = c("#003E74","#DD2501","#02893B"))+
  scale_colour_manual(values = c("#003E74","#DD2501","#02893B"))+  
  
  scale_y_log10(breaks=c(seq(0.6,3.6,0.2)))+
  geom_hline(yintercept=1, linetype="dashed", col="grey40",linewidth=0.5) +
  labs(y="Odds ratio",x="", col="Data source",fill=element_blank(),      
       title = "Univariable odds ratios for subsequent \npublication in a high-impact journal (IF 10+)",
       subtitle = paste0("Among N=",nrow(dat_mod)," preprints published in 2020")) +
  theme(legend.position = c(0.85,0.15),
        legend.background = element_rect(fill = "white", color = "black"))


p_univ_ors_IF10
p_univ_ors_IF10 <- recolorAxisUniv(p_univ_ors_IF10)
p_univ_ors_IF10

### save plot
OverReact::saveREACTplot(p = p_univ_ors_IF10,
                         figpath = figpath,filetypes=c("jpg","png"),
                         filename = "univ_preprint_publication_IF10",
                         width = 8,height = 7,savePDF = T)










# Run models --------------------------------------------------------------


### define params
n_cores=6
loss_function="Logloss"
eval_metric="AUC"
### if params are not supplied, use these defaults
my_params = list(
  thread_count = n_cores,
  loss_function = loss_function,
  eval_metric = eval_metric,
  iterations = 10^5, # Train up to 10^5 rounds
  early_stopping_rounds = 50, # Stop after X rounds of no improvement
  border_count = 254,
  depth = 7,
  learning_rate = 0.1,
  l2_leaf_reg = 5, ### 5 was optimal in training
  rsm =0.1,
  logging_level = "Verbose"
  # per_float_feature_quantization = c('0:border_count=1024', '1:border_count=1024')
)

xval_altmetrics=stabilityshap::catboostCrossvalidate(X=dat_mod %>% dplyr::select(all_of(altmetricvars)),
                                                     y=dat_mod$published_in_journal,
                                                     my_params = my_params,k = 10,myseed = 123,
                                                     loss_function = "Logloss",eval_metric = "AUC")

xval_topics=stabilityshap::catboostCrossvalidate(X=dat_mod %>% dplyr::select(all_of(topiccvars)),
                                                 y=dat_mod$published_in_journal,
                                                 my_params = my_params,k = 10,myseed = 123,
                                                 loss_function = "Logloss",eval_metric = "AUC")

xval_citation=stabilityshap::catboostCrossvalidate(X=dat_mod %>% dplyr::select(all_of(citationvars)),
                                                 y=dat_mod$published_in_journal,
                                                 my_params = my_params,k = 5,myseed = 123,
                                                 loss_function = "Logloss",eval_metric = "AUC")

xval_title=stabilityshap::catboostCrossvalidate(X=dat_mod %>% dplyr::select(all_of(titlevars)),
                                                   y=dat_mod$published_in_journal,
                                                   my_params = my_params,k = 10,myseed = 123,
                                                   loss_function = "Logloss",eval_metric = "AUC")

xval_allvars=stabilityshap::catboostCrossvalidate(X=dat_mod %>% dplyr::select(all_of(myvars)),
                                                 y=dat_mod$published_in_journal,
                                                 my_params = my_params,k = 10,myseed = 123,
                                                 loss_function = "Logloss",eval_metric = "AUC")


# create ROC curve from all predictions
p_roc=plotReactROC(list_of_preds=list(
  # `5. Title text data` = xval_title$predictions,
                                      `4. Citation data` = xval_citation$predictions,
                                      `3. Topics` = xval_topics$predictions,
                                      `2. Altmetrics` = xval_altmetrics$predictions,
                                      `1. All variables` = xval_allvars$predictions),
                   events=xval_allvars$y,
                   title ="Multivariable model: ROC curve",
  palette = "default",
                   subtitle =paste0("Predicting subsequent publication"))

p_roc

OverReact::saveREACTplot(p = p_roc,figpath = figpath,filename = "roc_curve_preprint_publication",
                         width = 5,height = 5,savePDF = T)

xval_allvars$feature_importance_summary %>% arrange(-featimp)






### FEATURE IMPORTANCE ANALYSIS ### 





# function to get nice df out of list of feature importances
extractFeatimps=function(l,data.dict){
  out=NULL
  for(i in 1:length(l)){
    df=as.data.frame(l[[i]])
    df$var=rownames(df)
    out=rbind(out,df)
  }
  out <- out %>% rename(featimp=V1)
  out <- out %>% select(var,featimp) %>% 
    left_join(data.dict) %>% 
    mutate(var=case_when(is.na(desc)~ var,
                         T ~ desc)) %>%  
    arrange(-featimp)
  
  return(out)
}


### get feature importances 
feat_imps=xval_allvars$feature_importance_summary %>% arrange(-featimp) %>% left_join(data.dict)



### get feature importances 
feat_imps=xval_allvars$feature_importance_summary %>% arrange(-featimp)%>% left_join(data.dict) %>% 
  mutate(var=case_when(is.na(desc)~ var,
                       T ~ desc)) %>%  
  arrange(-featimp)


# run function to get df
featimps_folds=extractFeatimps(xval_allvars$feature_importance_by_fold,data.dict=data.dict)

# get top imps for plotting
top_imps=feat_imps$var[1:20]

featimps_folds$type="Single-fold \nfeature \nimportance"
feat_imps$type="Average \nfeature \nimportance \nacross 10 folds"

# add 
feat_imps <- feat_imps %>% mutate(vartype=case_when(grepl("topic_",var)~"Topics",
                                                    grepl("itation",var)~"Citations",
                                                    T ~ "Altmetrics"))

# add 
featimps_folds <- featimps_folds %>% mutate(vartype=case_when(grepl("topic_",var)~"Topics",
                                                    grepl("itation",var)~"Citations",
                                                    T ~ "Altmetrics"))



# plot
p_fimps=feat_imps  %>% 
  select(var,featimp,desc, type,vartype) %>% 
  rbind(featimps_folds) %>% 
  filter(var%in%top_imps) %>% 
  arrange(-featimp) %>% 
  mutate(var=factor(var,levels=unique(var))) %>% 
  ggplot(aes(x=reorder(var,featimp), y =featimp)) +
  # geom_point(data = featimps_folds_IF10%>% 
  #              filter(var%in%top_imps),
  #            col="grey70", alpha=0.5, shape=1) +
  # scale_fill_manual(values = (fakedat$cols))+
  geom_point(aes(shape=type,alpha=type, fill=vartype,col=vartype)) +
  scale_shape_manual(values=c(19,1))+
  # scale_color_manual(values=c("black","grey70"),guide="none")+
  scale_alpha_manual(values=c(1,0.2),guide="none")+
  scale_fill_manual(values = c("#003E74","#DD2501","#02893B"),guide="none")+
  scale_color_manual(values = c("#003E74","#DD2501","#02893B"))+
  
  # geom_point(data=feat_imps %>% 
  #              filter(var%in%top_imps), 
  #            aes(fill=vartype), alpha=1)+
  # geom_errorbar(aes(ymin=featimp_lower, ymax = featimp_upper), width=0.3) +
  OverReact::theme_react(subtitle_size = 10) +
  coord_flip() +
  geom_hline(yintercept=0, linetype="dotted", col="grey50") +
  labs(x="", y="Feature importance (∆ AUC without feature)", 
       col="",alpha="",shape="", fill="",
       title="Multivariable model: Feature importance",
       subtitle = "Predicting preprint publication in journals") +
  theme(legend.position = c(0.8,0.25),
        legend.text = element_text(
          margin = margin(r = 1, unit = "pt"))) +
  theme(legend.spacing.y = unit(-0.05, 'cm')) +
  guides(fill = guide_legend(byrow = TRUE))
p_fimps

# get vector of ordered vars for recolouring
varorder=feat_imps  %>% 
  filter(var%in%top_imps) %>% 
  arrange(-featimp) %>% 
  select(var) %>% unlist() %>% 
  as.character()

# recolour
p_fimps=recolorAxisVect(p_fimps,vect = varorder)
p_fimps

# save
OverReact::saveREACTplot(p = p_fimps,figpath = figpath,filename = "featimps",
                         width = 8,height = 6,savePDF = T,filetypes = c("jpg"))


# combine plots and roc
p_roc_fimps=p_fimps+p_roc
p_roc_fimps

# save
OverReact::saveREACTplot(p = p_roc_fimps,figpath = figpath,filename = "roc_featimps",
                         width = 12,height = 6,savePDF = T,filetypes = c("jpg"))





# Repeat in IF5 top-tier journal publication only -----------------------------


xval_altmetrics_top=stabilityshap::catboostCrossvalidate(X=dat_mod %>% dplyr::select(all_of(altmetricvars)),
                                                     y=dat_mod$subs_pub_journal_if_5,
                                                     my_params = my_params,k = 10,myseed = 123,
                                                     loss_function = "Logloss",eval_metric = "AUC")

xval_topics_top=stabilityshap::catboostCrossvalidate(X=dat_mod %>% dplyr::select(all_of(topiccvars)),
                                                 y=dat_mod$subs_pub_journal_if_5,
                                                 my_params = my_params,k = 10,myseed = 123,
                                                 loss_function = "Logloss",eval_metric = "AUC")

xval_citation_top=stabilityshap::catboostCrossvalidate(X=dat_mod %>% dplyr::select(all_of(citationvars)),
                                                   y=dat_mod$subs_pub_journal_if_5,
                                                   my_params = my_params,k = 5,myseed = 123,
                                                   loss_function = "Logloss",eval_metric = "AUC")

xval_title_top=stabilityshap::catboostCrossvalidate(X=dat_mod %>% dplyr::select(all_of(titlevars)),
                                                y=dat_mod$subs_pub_journal_if_5,
                                                my_params = my_params,k = 10,myseed = 123,
                                                loss_function = "Logloss",eval_metric = "AUC")

xval_allvars_top=stabilityshap::catboostCrossvalidate(X=dat_mod %>% dplyr::select(all_of(myvars)),
                                                  y=dat_mod$subs_pub_journal_if_5,
                                                  my_params = my_params,k = 10,myseed = 123,
                                                  loss_function = "Logloss",eval_metric = "AUC")


# create ROC curve from all predictions
p_roc_top=plotReactROC(list_of_preds=list(
  # `5. Title text data` = xval_title_top$predictions,
                                      `4. Citation data` = xval_citation_top$predictions,
                                      `3. Topics` = xval_topics_top$predictions,
                                      `2. Altmetrics` = xval_altmetrics_top$predictions,
                                      `1. All variables` = xval_allvars_top$predictions),
                   events=xval_allvars_top$y,palette = "default",
                   title ="Multivariable model: ROC curve",
                   subtitle =paste0("Predicting publication in top journals in ",nrow(dat_mod)," preprints"))

p_roc_top

OverReact::saveREACTplot(p = p_roc_top,figpath = figpath,filename = "roc_curve_preprint_publication_IF5_journals",
                         width = 5,height = 5,savePDF = T)




# Repeat in IF10 top-tier journal publication only -----------------------------
dat_mod$subs_pub_journal_if_5[is.na(dat_mod$subs_pub_journal_if_5)] <- 0
dat_mod$subs_pub_journal_if_10[is.na(dat_mod$subs_pub_journal_if_10)] <- 0

xval_altmetrics_top10=stabilityshap::catboostCrossvalidate(X=dat_mod %>% dplyr::select(all_of(altmetricvars)),
                                                         y=dat_mod$subs_pub_journal_if_10,
                                                         my_params = my_params,k = 10,myseed = 123,
                                                         loss_function = "Logloss",eval_metric = "AUC")

xval_topics_top10=stabilityshap::catboostCrossvalidate(X=dat_mod %>% dplyr::select(all_of(topiccvars)),
                                                     y=dat_mod$subs_pub_journal_if_10,
                                                     my_params = my_params,k = 10,myseed = 123,
                                                     loss_function = "Logloss",eval_metric = "AUC")

xval_citation_top10=stabilityshap::catboostCrossvalidate(X=dat_mod %>% dplyr::select(all_of(citationvars)),
                                                       y=dat_mod$subs_pub_journal_if_10,
                                                       my_params = my_params,k = 5,myseed = 123,
                                                       loss_function = "Logloss",eval_metric = "AUC")

xval_title_top10=stabilityshap::catboostCrossvalidate(X=dat_mod %>% dplyr::select(all_of(titlevars)),
                                                    y=dat_mod$subs_pub_journal_if_10,
                                                    my_params = my_params,k = 10,myseed = 123,
                                                    loss_function = "Logloss",eval_metric = "AUC")

xval_allvars_top10=stabilityshap::catboostCrossvalidate(X=dat_mod %>% dplyr::select(all_of(myvars)),
                                                      y=dat_mod$subs_pub_journal_if_10,
                                                      my_params = my_params,k = 10,myseed = 123,
                                                      loss_function = "Logloss",eval_metric = "AUC")



# create ROC curve from all predictions
p_roc_top10=plotReactROC(list_of_preds=list(
  # `5. Title text data` = xval_title_top10$predictions,
                                          `4. Citation data` = xval_citation_top10$predictions,
                                          `3. Topics` = xval_topics_top10$predictions,
                                          `2. Altmetrics` = xval_altmetrics_top10$predictions,
                                          `1. All variables` = xval_allvars_top10$predictions),
                       events=xval_allvars_top10$y,palette = "default",
                       title ="Multivariable model: ROC curve",
                       subtitle =paste0("Predicting publication in top journals (IF>10)"))

p_roc_top10

OverReact::saveREACTplot(p = p_roc_top10,figpath = figpath,filename = "roc_curve_preprint_publication_IF10_journals",
                         width = 5,height = 5,savePDF = T)






### get feature importances 
feat_imps_IF10=xval_allvars_top10$feature_importance_summary %>% arrange(-featimp)%>% left_join(data.dict) %>% 
  mutate(var=case_when(is.na(desc)~ var,
                       T ~ desc)) %>%  
  arrange(-featimp)


# run function to get df
featimps_folds_IF10=extractFeatimps(xval_allvars_top10$feature_importance_by_fold,data.dict=data.dict)

# get top imps for plotting
top_imps_IF10=feat_imps_IF10$var[1:20]

featimps_folds_IF10$type="Single-fold \nfeature importance"
feat_imps_IF10$type="Average feature importance \nacross 10 folds"


# add 
feat_imps_IF10 <- feat_imps_IF10 %>% mutate(vartype=case_when(grepl("topic_",var)~"Topics",
                                                    grepl("itation",var)~"Citations",
                                                    T ~ "Altmetrics"))

# add 
featimps_folds_IF10 <- featimps_folds_IF10 %>% mutate(vartype=case_when(grepl("topic_",var)~"Topics",
                                                              grepl("itation",var)~"Citations",
                                                              T ~ "Altmetrics"))



# plot
p_fimps_IF10=feat_imps_IF10  %>% 
  select(var,featimp,desc, type,vartype) %>% 
  rbind(featimps_folds_IF10) %>% 
  filter(var%in%top_imps) %>% 
  arrange(-featimp) %>% 
  # mutate(var=factor(var,levels=unique(var))) %>% 
  ggplot(aes(x=reorder(var,featimp), y =featimp)) +
  # geom_point(data = featimps_folds_IF10%>% 
  #              filter(var%in%top_imps),
  #            col="grey70", alpha=0.5, shape=1) +
  # scale_fill_manual(values = (fakedat$cols))+
  geom_point(aes(shape=type,alpha=type, fill=vartype,col=vartype)) +
  scale_shape_manual(values=c(19,1))+
  # scale_color_manual(values=c("black","grey70"),guide="none")+
  scale_alpha_manual(values=c(1,0.2),guide="none")+
  scale_fill_manual(values = c("#003E74","#DD2501","#02893B"),guide="none")+
  scale_color_manual(values = c("#003E74","#DD2501","#02893B"))+
  
  # geom_point(data=feat_imps %>% 
  #              filter(var%in%top_imps), 
  #            aes(fill=vartype), alpha=1)+
  # geom_errorbar(aes(ymin=featimp_lower, ymax = featimp_upper), width=0.3) +
  OverReact::theme_react(subtitle_size = 10) +
  coord_flip() +
  geom_hline(yintercept=0, linetype="dotted", col="grey50") +
  labs(x="", y="Feature importance (∆ AUC without feature)", 
       col="",alpha="",shape="", fill="",
       title="Multivariable model: Feature importance",
       subtitle = "Predicting preprint publication in journals") +
  theme(legend.position = c(0.8,0.25),
        legend.text = element_text(
          margin = margin(r = 1, unit = "pt"))) +
  theme(legend.spacing.y = unit(-0.05, 'cm')) +
  guides(fill = guide_legend(byrow = TRUE))
p_fimps_IF10



# get vector of ordered vars for recolouring
varorder_10=feat_imps_IF10  %>% 
  filter(var%in%top_imps) %>% 
  arrange(-featimp) %>% 
  select(var) %>% unlist() %>% 
  as.character()

# recolour
p_fimps_IF10=recolorAxisVect(p_fimps_IF10,vect = varorder_10)
p_fimps_IF10

# save
OverReact::saveREACTplot(p = p_fimps_IF10,figpath = figpath,filename = "featimps_IF10",
                         width = 8,height = 6,savePDF = T,filetypes = c("jpg"))




# combine plots and roc
p_roc_fimps_IF10=p_fimps_IF10+p_roc_top10
p_roc_fimps_IF10

# save
OverReact::saveREACTplot(p = p_roc_fimps_IF10,figpath = figpath,filename = "roc_featimps_IF10",
                         width = 12,height = 6,savePDF = T,filetypes = c("jpg"))




# Create three-panel plots for all ----------------------------------------



# IF Anly
p_roc_fimps_univ=p_univ_ors/p_roc_fimps +plot_layout(heights = c(4,3))
p_roc_fimps_univ


# save
OverReact::saveREACTplot(p = p_roc_fimps_univ,figpath = figpath,filename = "roc_featimps_univ_3_panel",
                         width = 12,height = 11,savePDF = T,filetypes = c("jpg","png"))


# IF10
p_roc_fimps_IF10_univ=p_univ_ors_IF10/p_roc_fimps_IF10+plot_layout(heights = c(4,3))
p_roc_fimps_IF10_univ

# save
OverReact::saveREACTplot(p = p_roc_fimps_IF10_univ,figpath = figpath,filename = "roc_featimps_univ_3_panel_IF10",
                         width = 12,height = 10,savePDF = T,filetypes = c("jpg","png"))






# Create tables of paper with highest metrics -----------------------------


# altmetrics
tab_1_altmetrics <- dat_mod %>% 
  arrange(-altmetrics_score) %>% slice_head(n=20) %>% 
  select(title_preferred,journal_title,date_published,subs_pub_journal_title,subs_pub_journal_if_2022,altmetrics_score,)

# scientist mentions
tab_1_scientist <- dat_mod %>% 
  arrange(-cohorts_sci) %>% slice_head(n=20) %>% 
  select(title_preferred,journal_title,date_published,resulting_publication_doi,
         subs_pub_journal_title,subs_pub_journal_if_2022,
         cohorts_sci)
# mendeley readers
tab_1_mendeley <- dat_mod %>% 
  arrange(-readers_mendeley) %>% slice_head(n=20) %>% 
  select(title_preferred,journal_title,date_published,resulting_publication_doi,
         subs_pub_journal_title,subs_pub_journal_if_2022,
         readers_mendeley)

# topic 105
tab_1_topic105 <- dat_mod %>% 
  arrange(-`topic_105: protein, spike, sars, cov`) %>% slice_head(n=20) %>% 
  select(title_preferred,journal_title,date_published,resulting_publication_doi,
         subs_pub_journal_title,subs_pub_journal_if_2022,
         `topic_105: protein, spike, sars, cov`)


# save
OverReact::savePrettyExcelWorkbook(listOfTables = list(tab_1_altmetrics=tab_1_altmetrics,tab_1_scientist=tab_1_scientist,tab_1_mendeley=tab_1_mendeley,
                                                       tab_1_topic105=tab_1_topic105),
                                   workbookName = "top_20s",outpath = outpath,noDecimalsColumns = c("cohorts_sci","readers_mendeley","altmetrics_score"),
                                   numFmt = c("cohorts_sci","readers_mendeley","altmetrics_score","subs_pub_journal_if_2022"))


# Repeat ROC analysis on Medrxiv only --------------------------------------

dat_mod_medrxiv=dat_mod %>% filter(journal_title%in%c("medRxiv","bioRxiv"))

xval_altmetrics_medrxiv=stabilityshap::catboostCrossvalidate(X=dat_mod_medrxiv %>% dplyr::select(all_of(altmetricvars)),
                                                     y=dat_mod_medrxiv$subs_pub_journal_if_10,
                                                     my_params = NULL,k = 10,myseed = 123,
                                                     loss_function = "Logloss",eval_metric = "AUC")

xval_topics_medrxiv=stabilityshap::catboostCrossvalidate(X=dat_mod_medrxiv %>% dplyr::select(all_of(topiccvars)),
                                                 y=dat_mod_medrxiv$subs_pub_journal_if_10,
                                                 my_params = NULL,k = 10,myseed = 123,
                                                 loss_function = "Logloss",eval_metric = "AUC")

xval_citation_medrxiv=stabilityshap::catboostCrossvalidate(X=dat_mod_medrxiv %>% dplyr::select(all_of(citationvars)),
                                                           y=dat_mod_medrxiv$subs_pub_journal_if_10,
                                                           my_params = NULL,k = 5,myseed = 121,
                                                           loss_function = "Logloss",eval_metric = "AUC")


xval_title_medrxiv=stabilityshap::catboostCrossvalidate(X=dat_mod_medrxiv %>% dplyr::select(all_of(titlevars)),
                                                           y=dat_mod_medrxiv$subs_pub_journal_if_10,
                                                           my_params = NULL,k = 5,myseed = 121,
                                                           loss_function = "Logloss",eval_metric = "AUC")

xval_allvars_medrxiv=stabilityshap::catboostCrossvalidate(X=dat_mod_medrxiv %>% dplyr::select(all_of(myvars)),
                                                  y=dat_mod_medrxiv$subs_pub_journal_if_10,
                                                  my_params = NULL,k = 10,myseed = 123,
                                                  loss_function = "Logloss",eval_metric = "AUC")


# create ROC curve from all predictions
p_roc_medrxiv=plotReactROC(list_of_preds=list(`5. Title text data` = xval_title_medrxiv$predictions,
                                              `4. Citation data` = xval_citation_medrxiv$predictions,
                                              `3. Topics` = xval_topics_medrxiv$predictions,
                                              `2. Altmetrics` = xval_altmetrics_medrxiv$predictions,
                                              `1. All variables` = xval_allvars_medrxiv$predictions),
                   events=xval_allvars_medrxiv$y,
                   title ="ROC curve",palette = "default",myCols =c("#960078","#003E74","#00ACD7","#02893B","#DD2501"),

                   subtitle =paste0("Predicting publication in top journals (IF>10) \nin ",nrow(dat_mod_medrxiv),
                                    " medRxiv and bioRxiv preprints"))

p_roc_medrxiv
OverReact::imperial_palettes
OverReact::saveREACTplot(p = p_roc_medrxiv,figpath = figpath,filename = "roc_curve_preprint_publication_medrxiv_biorxiv_top10",
                         width = 5,height = 5,savePDF = T)

p_roc_medrxiv+labs(subtitle =paste0("Predicting publication in top journals (IF>10)\nin ",nrow(dat_mod_medrxiv),
                                    "medRxiv and bioRxiv preprints"))


xval_allvars_medrxiv$feature_importance_summary %>% arrange(-featimp)



# Time to event stability -------------------------------------------------



maxdate=max(dat$date,na.rm=T)
table(dat_mod$published_in_journal) %>% prop.table()


# create data set
df_mod <- dat_mod  %>% 
  mutate(case=published_in_journal,
         time=case_when(case==1 ~ as.numeric(subs_pub_date-date_published),
                        case==0 ~ as.numeric(maxdate-date_published)),
         time=case_when(time<0 ~ NA_real_,
                        T ~time )
  ) %>% 
  dplyr::select(all_of(myvars), case,time)  %>% 
  filter(time>0)

# replace all NAs with zero
df_mod[is.na(df_mod)] <- 0
# df_mod <- df_mod %>% mutate(across(myvars,scale))%>% mutate_all(~ifelse(is.nan(.), NA, .))
df_mod[is.na(df_mod)] <- 0
df_mod <- df_mod[complete.cases(df_mod),]

df_mod <- df_mod %>%
  rename_at(vars(as.character(data.dict$var)), ~ as.character(data.dict$desc))


# define outcome
outcome="case"
# create holdout data set
set.seed(123)
split <- caret::createDataPartition(pull(df_mod,outcome),times = 1,p = 0.7, list =F)
splitindex <- 1:nrow(df_mod) %in% split
table(splitindex)
train=df_mod[splitindex,]
test=df_mod[!splitindex,]

# X and Y split
xdata = train %>% dplyr::select(-time,-case) %>% as.matrix()
ydata = train %>%  dplyr::select(time,case) %>% as.matrix()


# run variable selection
stab_out=sharp::VariableSelection(xdata = xdata,ydata = ydata,
                                  pi_list = seq(0.6,0.99,0.01),
                                  family = "cox",K = 100)

sharp::CalibrationPlot(stab_out)

# get test data
X_test=test %>% dplyr::select(-time,-case) %>% as.matrix()
y_test=test %>% dplyr::select(time,case)  %>% as.matrix()
opt_iter=which.max(stab_out$S)

# get incremental results
results_df <- getIncrementalSummary(stab_out = stab_out,
                                    ydata = y_test,
                                    xdata = X_test,
                                    reorder_by_normalised_beta = T,
                                    K = 20,
                                    n_thr = min(20,ncol(X_test)),
                                    family = "cox",
                                    getLoss = F)


# plot!
myplot=plotStabResultsStripedFlipped(results_df, opt_thresh=stab_out$P[opt_iter], 
                                     stab_out = stab_out,
                                     plotOnlyTopN = 100,family = "cox",
                                     plotOnlySelected = F,
                                     plot_aucs = F) 
myplot
plotvarsordered=myplot$patches$plots[[1]]$data$variable


colour_vector=lapply(X = plotvarsordered,FUN = colorVectoriser,myvars=myvars_list)

# assign colors to plot
myplot$patches$plots[[1]] <- myplot$patches$plots[[1]]+
  theme(axis.text.y = element_text(colour = rev(unlist(colour_vector))))

# check plot
myplot

OverReact::saveREACTplot(p = myplot,figpath = figpath,
                         filename = paste0("stability_selection_plot_coxmodel_",outcome),
                         width = 10,height = 8)



# plot!
myplot20=plotStabResultsStripedFlipped(results_df, opt_thresh=stab_out$P[opt_iter], 
                                       stab_out = stab_out,
                                       plotOnlyTopN = 30,family = "cox",
                                       plotOnlySelected = F,
                                       plot_aucs = F) 
myplot20
plotvarsordered=myplot20$patches$plots[[1]]$data$variable
colour_vector=lapply(X = plotvarsordered,FUN = colorVectoriser,myvars=myvars_list)

# assign colors to plot
myplot20$patches$plots[[1]] <- myplot20$patches$plots[[1]]+
  theme(axis.text.y = element_text(colour = rev(unlist(colour_vector))))

# check plot
myplot20

OverReact::saveREACTplot(p = myplot20,figpath = figpath,
                         filename = paste0("stability_selection_plot_coxmodel_top30_",outcome),
                         width = 10,height = 5)







# Binary stability -------------------------------------------------


# X and Y split
xdata = train %>% dplyr::select(-time,-case) %>% as.matrix()
ydata = train %>%  dplyr::select(case) %>% as.matrix()


# run variable selection
stab_out=sharp::VariableSelection(xdata = xdata,ydata = ydata,
                                  pi_list = seq(0.6,0.99,0.01),
                                  family = "binomial",K = 100)

sharp::CalibrationPlot(stab_out)

# get test data
X_test=test %>% dplyr::select(-time,-case) %>% as.matrix()
y_test=test %>% dplyr::select(case)  %>% as.matrix()
opt_iter=which.max(stab_out$S)

# get incremental results
results_df <- getIncrementalSummary(stab_out = stab_out,
                                    ydata = y_test,
                                    xdata = X_test,
                                    reorder_by_normalised_beta = T,
                                    K = 20,
                                    n_thr = min(20,ncol(X_test)),
                                    family = "cox",
                                    getLoss = F)




# plot!
myplot_2=plotStabResultsStripedFlipped(results_df, opt_thresh=stab_out$P[opt_iter], 
                                     stab_out = stab_out,
                                     plotOnlyTopN = 100,family = "cox",
                                     plotOnlySelected = F,
                                     plot_aucs = F) 
myplot_2
# plotvarsordered=myplot_2$patches$plots[[1]]$data$variable
colorVectoriser=function(var,myvars_list){
  if(var %in% myvars_list[[1]]){
    out=OverReact::imperial_palettes$extended[[1]]
  }else if(var %in% myvars_list[[2]]){
    out=OverReact::imperial_palettes$extended[[3]]
  }else{
    out=OverReact::imperial_palettes$extended[[7]]
  }
}


colour_vector=lapply(X = plotvarsordered,FUN = colorVectoriser,myvars=myvars_list)

# assign colors to plot
myplot_2$patches$plots[[1]] <- myplot_2$patches$plots[[1]]+
  theme(axis.text.y = element_text(colour = rev(unlist(colour_vector))))

# check plot
myplot_2

OverReact::saveREACTplot(p = myplot_2,figpath = figpath,
                         filename = paste0("stability_selection_plot_binomialmodel_",outcome),
                         width = 10,height = 8)




# 
# 
# Stability shap ----------------------------------------------------------


### Run bootstrap on example data
results=stabilityshap::SHAPStability(X = X,y = y,my_params = NULL,nruns = 100,
                                     subsample_training_size = 0.1,replace = F,
                                     loss_function = "Logloss",eval_metric = "AUC",
                                     multipleTestAdjustment = "bonferroni")


### extract results from output object
bootstrap_results=results$results_df
shaps_full=results$shaps_full
results_raw=results$results_raw

# join with data dict
bootstrap_results <- bootstrap_results %>% left_join(data.dict)
# replace with desc names where poss
bootstrap_results$var[!is.na(bootstrap_results$desc)] <- bootstrap_results$desc[!is.na(bootstrap_results$desc)]

# plot
p_stab=stabilityshap::plotAllMetrics(bootstrap_results = bootstrap_results,topn = 30,
                                     metricsToPlot = c("featimp","shap_beehive"),
                                     orderBy = "featimp",pValVar = "featimp_pval",
                                     X = X,shaps_full = shaps_full,thresh=0.95)
p_stab


# Get variables in order from leftmost plot and colourise them according to variable family
plotvarsordered=p_stab$patches$plots[[1]]$data$var
# colorVectoriser=function(var,myvars_list){
#   if(var %in% myvars_list[[1]]){
#     out=OverReact::imperial_palettes$core[[4]]
#   }else if(var %in% myvars_list[[2]] | var %in% data.dict$desc[c(1:3)]){
#     out=OverReact::imperial_palettes$default[[5]]
#   }else{
#     out=OverReact::imperial_palettes$core[[1]]
#   }
# }
colour_vector=lapply(X = plotvarsordered,FUN = colorVectoriser,myvars=myvars_list)

# assign colors to plot
p_stab$patches$plots[[1]] <- p_stab$patches$plots[[1]]+
  theme(axis.text.y = element_text(colour = rev(unlist(colour_vector))))

# check plot
p_stab

### save plot
OverReact::saveREACTplot(p = p_stab,
                         figpath = figpath,
                         filename = "preprint_publication_stab_shap",
                         width = 9,height = 6,savePDF = T)

# Plot interactions
p_interacts=stabilityshap::plotInteractionStrength(results$interactions_df)
p_interacts

### save plot
OverReact::saveREACTplot(p = p_interacts,
                         figpath = figpath,
                         filename = "preprint_publication_interaction",
                         width = 16,height = 16,savePDF = T)



# Stability shap IF10 ----------------------------------------------------------

y=dat_mod$subs_pub_journal_if_10
### Run bootstrap on example data
results_IF10=stabilityshap::SHAPStability(X = X,y = y,my_params = NULL,nruns = 100,
                                          subsample_training_size = 0.2,replace = F,
                                          loss_function = "Logloss",eval_metric = "AUC",
                                          multipleTestAdjustment = "bonferroni")


### extract results from output object
bootstrap_results_if10=results_IF10$results_df
shaps_full_if10=results_IF10$shaps_full
results_raw_if10=results_IF10$results_raw

# join with data dict
bootstrap_results_if10 <- bootstrap_results_if10 %>% left_join(data.dict)
# replace with desc names where poss
bootstrap_results_if10$var[!is.na(bootstrap_results_if10$desc)] <- bootstrap_results_if10$desc[!is.na(bootstrap_results_if10$desc)]

# plot
p_stab_if10=stabilityshap::plotAllMetrics(bootstrap_results = bootstrap_results_if10,topn = 30,
                                          metricsToPlot = c("featimp","shap_beehive"),
                                          orderBy = "featimp",pValVar = "featimp_pval",
                                          X = X,shaps_full = shaps_full,thresh=0.95)
p_stab_if10


# Get variables in order from leftmost plot and colourise them according to variable family
plotvarsordered_if10=p_stab_if10$patches$plots[[1]]$data$var
# colorVectoriser=function(var,myvars_list){
#   if(var %in% myvars_list[[1]]){
#     out=OverReact::imperial_palettes$core[[4]]
#   }else if(var %in% myvars_list[[2]] | var %in% data.dict$desc[c(1:3)]){
#     out=OverReact::imperial_palettes$default[[5]]
#   }else{
#     out=OverReact::imperial_palettes$core[[1]]
#   }
# }
colour_vector=lapply(X = plotvarsordered_if10,FUN = colorVectoriser,myvars=myvars_list)

# assign colors to plot
p_stab_if10$patches$plots[[1]] <- p_stab_if10$patches$plots[[1]]+
  theme(axis.text.y = element_text(colour = rev(unlist(colour_vector))))

# check plot
p_stab_if10

### save plot
OverReact::saveREACTplot(p = p_stab_if10,
                         figpath = figpath,
                         filename = "preprint_publication_stab_shap_if10",
                         width = 9,height = 6,savePDF = T)

# Plot interactions
p_interacts=stabilityshap::plotInteractionStrength(results$interactions_df)
p_interacts

### save plot
OverReact::saveREACTplot(p = p_interacts,
                         figpath = figpath,
                         filename = "preprint_publication_interaction",
                         width = 16,height = 16,savePDF = T)



# # Boruta selection ----------------------------------------------------------

library(stabilityshap)

X=dat_mod %>% dplyr::select(all_of(myvars))
y=dat_mod$published_in_journal
boruta=stabilityshap::catBoruta(X = X,y = y,nruns = 200,myseed = 123,my_params = NULL,
                                loss_function = "Logloss",eval_metric = "AUC")




