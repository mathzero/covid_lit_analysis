
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

# Load data ---------------------------------------------------------------

# read in main data set
# dat <- readRDS("data/dat_main.rds")
# topic_model <- readRDS("data/topic_model_k_160_all_abstract_1000.rds")

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
univ_df$source=c(rep("Crossref",4),rep("Altmetrics",nrow(univ_df)-4))
univ_df <- univ_df %>% 
  rename(lower=x2_5_percent,
         upper=x97_5_percent)

# Create forest plot
p_univ_ors=univ_df %>% 
  filter(!is.na(lower)) %>% 
  ggplot(aes(x=reorder(desc,exp_est),y=exp_est, col=source,shape=p<0.05))+
  geom_point() +
  scale_shape_manual(values = c(1,15),)+
  geom_errorbar(aes(ymin=lower,ymax=upper))+
  OverReact::theme_react() +
  OverReact::scale_color_imperial(palette = "short", rev=F)+
  coord_flip() +
  # ylim(c(0.8,2.8)) +
  scale_y_log10(breaks=seq(0.8,1.5,0.1))+
  geom_hline(yintercept=1, linetype="dashed", col="grey40",linewidth=0.5) +
  labs(y="Odds ratio",x="", col="Data source",
       title = "Univariable odds ratios for subsequent publication",
       subtitle = paste0("Among N=",nrow(dat_mod)," preprints published in 2020")) +
  theme(legend.position = c(0.85,0.15),
        legend.background = element_rect(fill = "white", color = "black"))
p_univ_ors

### save plot
OverReact::saveREACTplot(p = p_univ_ors,
                         figpath = figpath,filetypes=c("jpg","png"),
                         filename = "univ_preprint_publication",
                         width = 8,height = 7,savePDF = T)



### same analysis for publication in top journal ###

dat_mod$subs_pub_journal_if_5[is.na(dat_mod$subs_pub_journal_if_5)] <- 0
dat_mod$subs_pub_journal_if_10[is.na(dat_mod$subs_pub_journal_if_10)] <- 0

univ_mods=dat_mod %>% 
  dplyr::select(all_of(c(citationvars,altmetricvars))) %>%  # exclude outcome, leave only predictors 
  mutate(across(c(citationvars,altmetricvars),
                ~replace_na(.x,0))) %>%
  mutate(across(c(citationvars,altmetricvars),
                ~myscaler(.x))) %>%
  map(~glm(dat_mod$subs_pub_journal_if_5 ~ .x, data = dat_mod, family="binomial")) %>% 
  map(jtools::summ, exp=T,confint = TRUE,digits = 3)


univ_mods_coefs=lapply(univ_mods,getModSummary)
univ_df=bind_rows(univ_mods_coefs)
univ_df$predictor=names(univ_mods_coefs)
univ_df <- univ_df %>% janitor::clean_names()
# univ_df$est <- exp(univ_df$est)
# univ_df$x2_5_percent <- exp(univ_df$x2_5_percent)
# univ_df$x97_5_percent <- exp(univ_df$x97_5_percent)
univ_df <-univ_df %>%  left_join(data.dict,by=c("predictor"="var"))
univ_df$source=c(rep("Crossref",4),rep("Altmetrics",nrow(univ_df)-4))
univ_df <- univ_df %>% 
  rename(lower=x2_5_percent,
         upper=x97_5_percent)

# Create forest plot
p_univ_ors_IF5=univ_df %>% 
  filter(!is.na(lower)) %>% 
  ggplot(aes(x=reorder(desc,exp_est),y=exp_est, col=source,shape=p<0.05))+
  geom_point() +
  scale_shape_manual(values = c(1,15),)+
  geom_errorbar(aes(ymin=lower,ymax=upper))+
  OverReact::theme_react() +
  OverReact::scale_color_imperial(palette = "short", rev=F)+
  coord_flip() +
  # ylim(c(0.8,2.8)) +
  scale_y_log10(breaks=c(seq(0.6,3.6,0.2)))+
  geom_hline(yintercept=1, linetype="dashed", col="grey40",linewidth=0.5) +
  labs(y="Odds ratio",x="", col="Data source",
       title = "Univariable odds ratios for subsequent \npublication in a high-impact journal (IF 5+)",
       subtitle = paste0("Among N=",nrow(dat_mod)," preprints published in 2020")) +
  theme(legend.position = c(0.85,0.15),
        legend.background = element_rect(fill = "white", color = "black"))


p_univ_ors_IF5

### save plot
OverReact::saveREACTplot(p = p_univ_ors_IF5,
                         figpath = figpath,filetypes=c("jpg","png"),
                         filename = "univ_preprint_publication_IF5",
                         width = 8,height = 7,savePDF = T)





### same analysis for publication in top journal ###

univ_mods=dat_mod %>% 
  dplyr::select(all_of(c(citationvars,altmetricvars))) %>%  # exclude outcome, leave only predictors 
  mutate(across(c(citationvars,altmetricvars),
                ~replace_na(.x,0))) %>%
  mutate(across(c(citationvars,altmetricvars),
                ~myscaler(.x))) %>%
  map(~glm(dat_mod$subs_pub_journal_if_10 ~ .x, data = dat_mod, family="binomial")) %>% 
  map(jtools::summ, exp=T,confint = TRUE,digits = 3)


univ_mods_coefs=lapply(univ_mods,getModSummary)
univ_df=bind_rows(univ_mods_coefs)
univ_df$predictor=names(univ_mods_coefs)
univ_df <- univ_df %>% janitor::clean_names()
# univ_df$est <- exp(univ_df$est)
# univ_df$x2_5_percent <- exp(univ_df$x2_5_percent)
# univ_df$x97_5_percent <- exp(univ_df$x97_5_percent)
univ_df <-univ_df %>%  left_join(data.dict,by=c("predictor"="var"))
univ_df$source=c(rep("Crossref",4),rep("Altmetrics",nrow(univ_df)-4))
univ_df <- univ_df %>% 
  rename(lower=x2_5_percent,
         upper=x97_5_percent)

# Create forest plot
p_univ_ors_IF10=univ_df %>% 
  filter(!is.na(lower)) %>% 
  ggplot(aes(x=reorder(desc,exp_est),y=exp_est, col=source,shape=p<0.05))+
  geom_point() +
  scale_shape_manual(values = c(1,15),)+
  geom_errorbar(aes(ymin=lower,ymax=upper))+
  OverReact::theme_react() +
  OverReact::scale_color_imperial(palette = "short", rev=F)+
  coord_flip() +
  # ylim(c(0.8,2.8)) +
  scale_y_log10(breaks=c(seq(0.6,3.6,0.2)))+
  geom_hline(yintercept=1, linetype="dashed", col="grey40",linewidth=0.5) +
  labs(y="Odds ratio",x="", col="Data source",
       title = "Univariable odds ratios for subsequent \npublication in a high-impact journal (IF 10+)",
       subtitle = paste0("Among N=",nrow(dat_mod)," preprints published in 2020")) +
  theme(legend.position = c(0.85,0.15),
        legend.background = element_rect(fill = "white", color = "black"))


p_univ_ors_IF10

### save plot
OverReact::saveREACTplot(p = p_univ_ors_IF10,
                         figpath = figpath,filetypes=c("jpg","png"),
                         filename = "univ_preprint_publication_IF10",
                         width = 8,height = 7,savePDF = T)








# Boruta selection ----------------------------------------------------------

library(stabilityshap)

X=dat_mod %>% dplyr::select(all_of(myvars)) 
y=dat_mod$published_in_journal
boruta=stabilityshap::catBoruta(X = X,y = y,nruns = 200,myseed = 123,my_params = NULL,
                                loss_function = "Logloss",eval_metric = "AUC")



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
colorVectoriser=function(var,myvars_list){
  if(var %in% myvars_list[[1]]){
    out=OverReact::imperial_palettes$core[[4]]
  }else if(var %in% myvars_list[[2]] | var %in% data.dict$desc[c(1:4)]){
    out=OverReact::imperial_palettes$default[[5]]
  }else{
    out=OverReact::imperial_palettes$core[[1]]
  }
}
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




# Run models --------------------------------------------------------------


getCatboostPreds=function(myvars, myseed=123){
  
  set.seed(myseed)
  
  X=dat_mod %>% dplyr::select(all_of(myvars))
  y=dat_mod$published_in_journal
  
  # get full sample size
  fullsampsize=nrow(X)
  
  
  
  # Convert X variables to factor
  X[sapply(X, is.character)] <- lapply(X[sapply(X, is.character)],
                                       as.factor)
  
  
  ### create watchlist
  split <- caret::createDataPartition(y, times = 1, p =0.7, list =F)
  splitindex <- !(1:nrow(X) %in% split)
  
  # get test and eval
  Xtrain= X[split,]
  Ytrain= y[split]
  Xtest= X[splitindex,]
  Ytest= y[splitindex]
  
  ### create watchlist
  split_2 <- caret::createDataPartition(Ytrain, times = 1, p =0.8, list =F)
  splitindex_2 <- !(1:nrow(Xtrain) %in% split_2)
  
  
  Xvalid= Xtrain[splitindex_2,]
  Yvalid= Ytrain[splitindex_2]
  Xtrain= Xtrain[split_2,]
  Ytrain= Ytrain[split_2]
  
  
  learn_pool = catboost::catboost.load_pool(Xtrain, label = Ytrain)
  test_pool = catboost::catboost.load_pool(Xvalid, label = Yvalid)
  eval_pool = catboost::catboost.load_pool(Xtest, label = Ytest)
  
  full_pool = catboost::catboost.load_pool(data = X,label = y)
  
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
    early_stopping_rounds = 10, # Stop after X rounds of no improvement
    border_count = 254,
    depth = 7,
    learning_rate = 0.1,
    l2_leaf_reg = 5, ### 5 was optimal in training
    rsm =0.1,
    logging_level = "Verbose"
    # per_float_feature_quantization = c('0:border_count=1024', '1:border_count=1024')
  )
  
  
  ### train model
  cb_model = catboost::catboost.train(
    learn_pool = learn_pool,
    test_pool = test_pool,
    params = my_params
  )
  
  
  preds=catboost::catboost.predict(model = cb_model,pool = eval_pool,prediction_type = "Probability")
  
  myauc=round(MLmetrics::AUC(y_pred = preds,y_true = Ytest),4)
  print(paste("AUC =",myauc))
  
  
  auc_ci_mod <- pROC::ci.auc(Ytest,preds)
  auc_mod <- pROC::auc(Ytest,preds)
  roc_mod <- pROC::roc(Ytest,preds)
  ci.sp.obj <- pROC::ci.sp(roc_mod,sensitivities=seq(0,1,0.01), boot.n=500)
  
  return(list(preds=preds,
              y=Ytest,
              auc_ci_mod=auc_ci_mod,
              auc_mod=auc_mod,
              roc_mod=roc_mod,
              ci.sp.obj=ci.sp.obj))
}



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
                   title ="ROC curve",
                   subtitle =paste0("Predicting subsequent publication in ",nrow(dat_mod)," preprints"))

p_roc

OverReact::saveREACTplot(p = p_roc,figpath = figpath,filename = "roc_curve_preprint_publication",
                         width = 5,height = 5,savePDF = T)



# Repeat in top-tier journal publication only -----------------------------


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
                   events=xval_allvars_top$y,
                   title ="ROC curve",
                   subtitle =paste0("Predicting publication in top journals in ",nrow(dat_mod)," preprints"))

p_roc_top

OverReact::saveREACTplot(p = p_roc_top,figpath = figpath,filename = "roc_curve_preprint_publication_IF5_journals",
                         width = 5,height = 5,savePDF = T)




# Repeat in top-tier journal publication only -----------------------------
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

xval_allvars_top10$loss_by_fold

# create ROC curve from all predictions
p_roc_top10=plotReactROC(list_of_preds=list(
  # `5. Title text data` = xval_title_top10$predictions,
                                          `4. Citation data` = xval_citation_top10$predictions,
                                          `3. Topics` = xval_topics_top10$predictions,
                                          `2. Altmetrics` = xval_altmetrics_top10$predictions,
                                          `1. All variables` = xval_allvars_top10$predictions),
                       events=xval_allvars_top10$y,
                       title ="ROC curve",
                       subtitle =paste0("Predicting publication in top journals (IF>10) in ",nrow(dat_mod)," preprints"))

p_roc_top10

OverReact::saveREACTplot(p = p_roc_top10,figpath = figpath,filename = "roc_curve_preprint_publication_IF10_journals",
                         width = 5,height = 5,savePDF = T)






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
                   title ="ROC curve",
                   subtitle =paste0("Predicting publication in top journals (IF>10) \nin ",nrow(dat_mod_medrxiv),
                                    " medRxiv and bioRxiv preprints"))

OverReact::saveREACTplot(p = p_roc_medrxiv,figpath = figpath,filename = "roc_curve_preprint_publication_medrxiv_biorxiv_top10",
                         width = 5,height = 5,savePDF = T)

p_roc_medrxiv+labs(subtitle =paste0("Predicting publication in top journals (IF>10)\nin ",nrow(dat_mod_medrxiv),
                                    "medRxiv and bioRxiv preprints"))







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
plotvarsordered=myplot_2$patches$plots[[1]]$data$variable
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







# Time to event stability -------------------------------------------------

maxdate=max(dat$date)
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
df_mod <- df_mod %>% mutate(across(myvars,scale))%>% mutate_all(~ifelse(is.nan(.), NA, .))
df_mod[is.na(df_mod)] <- 0

df_mod <- df_mod[complete.cases(df_mod),]

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




# In depth topic modelling no 63 ------------------------------------------

dat_mod %>% ggplot(aes(x=`topic_63: cells, cell, immune, inflammatory`,
                       col=factor(published_in_journal))) +
  scale_x_log10()+
  geom_density()+
  OverReact::theme_react() +
  labs(y="Density", x="\u03B3 topic 63 (cells, cell, immune, inflammatory ...)")

