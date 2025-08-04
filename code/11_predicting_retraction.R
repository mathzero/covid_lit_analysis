
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
createMySubfolder(subfolderName = "predicting_retraction")

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
dat_ret=openxlsx::read.xlsx("data/Covid List Rns Reinstatements 5082023 (1).xlsx")



# Engineer a feature that finds a ratio of Altmetrics to Citations --------


# create feature
dat <- dat %>% 
  mutate(citations_count_offset=case_when(cites_per_day==0 ~ 0.01,
                                          T ~ cites_per_day),
         rcr_alt_ratio=case_when(altmetrics_score==0 ~ 0,
                                 T ~log(altmetrics_score/citations_count_offset)))


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
  mutate(retracted=case_when(doi%in%dat_ret$OriginalPaperDOI ~ 1,
                             T~ 0))
 


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
                "altmetrics_score","rcr_alt_ratio")
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
  map(~glm(dat_mod$retracted ~ .x, data = dat_mod, family="binomial")) %>% 
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
                 "Altmetrics score percentile: among similarly aged papers within journal","Altmetrics score","Ratio of Altmetrics score to RCR")


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
  
  scale_y_log10(breaks=seq(0.8,3,0.2))+
  geom_hline(yintercept=1, linetype="dashed", col="grey40",linewidth=0.5) +
  labs(y="Odds ratio",x="", col="Data source",fill=element_blank(),
       title = "Univariable odds ratios for retraction",
       subtitle = paste0("Among N=",nrow(dat_mod)," articles")) +
  theme(legend.position = c(0.85,0.15),
        legend.background = element_rect(fill = "white", color = "black"))


p_univ_ors <- recolorAxisUniv(p_univ_ors)
p_univ_ors





### save plot
OverReact::saveREACTplot(p = p_univ_ors,
                         figpath = figpath,filetypes=c("jpg","png"),
                         filename = "univ_retraction",
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
                                                     y=dat_mod$retracted,
                                                     my_params = my_params,k = 3,myseed = 123,
                                                     loss_function = "Logloss",eval_metric = "AUC")

xval_topics=stabilityshap::catboostCrossvalidate(X=dat_mod %>% dplyr::select(all_of(topiccvars)),
                                                 y=dat_mod$retracted,
                                                 my_params = my_params,k = 3,myseed = 123,
                                                 loss_function = "Logloss",eval_metric = "AUC")

xval_citation=stabilityshap::catboostCrossvalidate(X=dat_mod %>% dplyr::select(all_of(citationvars)),
                                                   y=dat_mod$retracted,
                                                   my_params = my_params,k = 3,myseed = 123,
                                                   loss_function = "Logloss",eval_metric = "AUC")

xval_title=stabilityshap::catboostCrossvalidate(X=dat_mod %>% dplyr::select(all_of(titlevars)),
                                                y=dat_mod$retracted,
                                                my_params = my_params,k = 3,myseed = 123,
                                                loss_function = "Logloss",eval_metric = "AUC")

xval_allvars=stabilityshap::catboostCrossvalidate(X=dat_mod %>% dplyr::select(all_of(myvars)),
                                                  y=dat_mod$retracted,
                                                  my_params = my_params,k = 3,myseed = 123,
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
  subtitle =paste0("Predicting retraction"))

p_roc

OverReact::saveREACTplot(p = p_roc,figpath = figpath,filename = "roc_curve_retraction",
                         width = 5,height = 5,savePDF = T)







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
       subtitle = "Predicting retraction") +
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


















