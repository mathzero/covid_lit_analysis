rm(list=ls())
source("E:/Group/functions/load_packages.R")
source("E:/Group/functions/cats_and_covs.R")
source("E:/Group/react2_study5/report_phases_combined/projects/delta_symptom_prediction/code/00_functions.R", local = T)

load_packages(c("dplyr", "OverReact","tidyverse","tm","wordcloud","ggplot2","ggthemes","stats",
                "text2map","tidytext","focus","gridExtra", "future.apply", "lubridate","ggbeeswarm","broom",
                "foreach","doParallel","catboost","ggnetwork","ICoLour","ppcor","zoo","scales","ggpubr","Kendall",
                "tidytext", "quanteda", "widyr", "igraph", "ggraph", "patchwork"))
outpath <- "E:/Group/react2_study5/report_phases_combined/projects/covid_lit_analysis/output/catboost/"
figpath <- "E:/Group/react2_study5/report_phases_combined/projects/covid_lit_analysis/plots/catboost/"


# Load data ---------------------------------------------------------------

# Save
dat <- readRDS("data/dat_main.rds")

dat$altmetrics.score
#### STABILITY SELECTION ###

outcome="published"
# create holdout data set
df_mod <- dat %>% filter(preprint=="Preprint", date <= as.Date("2020-06-01")) %>% 
  dplyr::select(-c(id,document,cites_per_day,research_org_country_names ,citations_count,
                   altmetrics.id,resulting_publication_doi,date,preprint,altmetrics.score,
                   `Days since publication`,title.preferred, doi))
# replace greater thans with 1
df_mod[df_mod>1] <- 1
table(df_mod$published, exclude = "none")
df_mod <- df_mod[colSums(df_mod)>50]


set.seed(123)
split <- caret::createDataPartition(pull(df_mod,outcome),times = 1,p = 0.7, list =F)
splitindex <- 1:nrow(df_mod) %in% split
table(splitindex)
train=df_mod[splitindex,]
test=df_mod[!splitindex,]


# Variable selection with FOcus stability selection -----------------------
X=train %>% dplyr::select(-c(published))
y=train %>% dplyr::select(published) %>% unlist()


# Catboost ----------------------------------------------------------------

# load catboost functions from LC paper
# source("E:/Group/react2_study5/report_phases_combined/projects/long_covid_2021/code/00_functions.R", local = T)
source("E:/Group/react2_study5/report_phases_combined/projects/covid_lit_analysis/code/0_functions.R", local = T)

# split training again for anti-overfitting pool
split2 <- caret::createDataPartition(pull(train,outcome),times = 1,p = 0.8, list =F)
splitindex2 <- 1:nrow(train) %in% split2
train_learn <- train[splitindex2,]
train_valid=train[!splitindex2,]



# Set parameters
ncores=6
my_params=list(thread_count=ncores,
               loss_function="Logloss",
               eval_metric = "Logloss",
               iterations =10^5,
               early_stopping_rounds=100,
               border_count=254,
               depth =7,
               learning_rate = 0.01)


X_learn=train_learn %>% dplyr::select(-{{ outcome }})
X_valid=train_valid %>% dplyr::select(-{{ outcome }})
X_test=test %>% dplyr::select(-{{ outcome }})

# create learn and test pools
learn_pool = catboost.load_pool(data = X_learn,label = pull(train_learn,{{outcome}}))
test_pool = catboost.load_pool(data = X_valid,label = pull(train_valid,{{outcome}}))
validation_pool = catboost.load_pool(data =X_test,label = pull(test,{{outcome}}))


# train model
cb_model = catboost.train(learn_pool = learn_pool,test_pool = test_pool, params = my_params)


# save.image(file = "prediction.rdata")
# load("prediction.rdata")
# 
# # get variable importances
# basemodel <- getFeatureImpAndSHAP(cb_model = cb_model,validation_pool = validation_pool,
#                                   learn_pool=learn_pool,
#                                   modelvars = "covariates",
#                                   X = test %>% dplyr::select(-{{ outcome }}),
#                                   y = pull(test,{{outcome}}),
#                                   nperm = 100,
#                                   myheight = 6.5,
#                                   bandwidth_scaler = 15,
#                                   train_learn = train_learn,
#                                   outcome = outcome,
#                                   parallelise = T,nclust = 100)
# 
# predictors_df



# Get importances ---------------------------------------------------------


# get feature importances
cb_varimps=catboost.get_feature_importance(model = cb_model, pool = validation_pool, type = "LossFunctionChange") %>% as.data.frame()
cb_varimps$feature =rownames(cb_varimps)
colnames(cb_varimps)[[1]] <- "Mean_delta_logloss"

plot_varimps <- cb_varimps %>% 
  arrange(desc(Mean_delta_logloss)) %>% 
  slice_head(n = 20) %>% 
  # left_join(predictors_df, by = c("feature" = "variable_desc")) %>% 
  ggplot(aes(x=reorder(feature,Mean_delta_logloss), y=Mean_delta_logloss)) + 
  geom_col(width=0.03) +
  geom_point() +
  coord_flip() +
  scale_fill_manual(values = myCols) +
  scale_colour_manual(values = myCols) +
  theme_bw() +
  labs(x="Variable",y="Mean change in logloss \nafter variable removal", col = "Variable type",  fill = "Variable type")
plot_varimps

OverReact::saveREACTplot(p = plot_varimps, figpath = figpath,
                         filename = paste0("varimps_plot_",outcome),width =7, height=6)





###### INTERACTIONS ######

### Get feature interactions
interacts_basemod <- catboost.get_feature_importance(model = cb_model,
                                                     pool = learn_pool,
                                                     type = "Interaction") %>% 
  as.data.frame()

### get varnames to replace indexes
varnames_basemod <- X %>% colnames()
varnames_basemod_df=data.frame(varnames=varnames_basemod,
                               varindex=0:(length(varnames_basemod)-1))
interacts_basemod$feature1_name <- NA
interacts_basemod$feature2_name <- NA

i <- 1
for (i in 1:nrow(interacts_basemod)){
  indx <- interacts_basemod[i,]$feature1_index
  interacts_basemod[i,]$feature1_name <- varnames_basemod_df[indx+1,]$varnames
}
for (i in 1:nrow(interacts_basemod)){
  indx <- interacts_basemod[i,]$feature2_index
  interacts_basemod[i,]$feature2_name <- varnames_basemod_df[indx+1,]$varnames
}

### Concatenate names
interacts_basemod$features_comb <- paste0(interacts_basemod$feature1_name, " x ",
                                          interacts_basemod$feature2_name)

###plot
### seconds plot, with distributions
plot_interacts <- interacts_basemod%>% 
  arrange(desc(score)) %>% 
  slice_head(n = 20) %>% 
  ggplot(aes(y=reorder(features_comb,score ), x=score )) + 
  geom_col(fill=myCols[2]) +
  theme_bw() +
  scale_y_discrete(labels=function(x) str_wrap(x, width=33)) +
  labs(y="Variable",x="Interaction strength", 
       col = "",  fill = "")

plot_interacts
OverReact::saveREACTplot(p = plot_interacts, figpath = figpath,
                         filename = paste0("interactions_plot",outcome),
                         width =6, height=7)


###### SHAP PLOT ######


# get SHAP values
cb_shaps=catboost.get_feature_importance(model = cb_model, pool = learn_pool, type = "ShapValues") %>% as.data.frame()

colnames(cb_shaps) <- c(cb_varimps$feature,"Bias")
cb_shaps <- cb_shaps %>% dplyr::select(-Bias)
cb_shaps_long=cb_shaps %>% mutate(ID=row_number()) %>% 
  pivot_longer(-ID) %>% 
  rename(variable=name)
cb_shaps_long_means=cb_shaps %>% mutate(ID=row_number()) %>% 
  pivot_longer(-ID) %>% 
  rename(variable=name) %>% 
  group_by(variable) %>% 
  summarise(mean_value=mean(abs(value))) %>% 
  arrange(desc(mean_value)) %>% 
  mutate(indx=row_number())
cb_shaps_long <- left_join(cb_shaps_long,cb_shaps_long_means)

# add values from original data
## quick function for 0/1 scaling
scaleZeroOne <- function(x){
  (x-min(x,na.rm=T))/(max(x,na.rm=T)-min(x,na.rm=T))
}

cb_shaps_stdfvalues=train_learn %>% 
  dplyr::select(-outcome) %>%
  # mutate(across(where(is.numeric),scale)) %>% 
  mutate_all(as.numeric) %>% 
  mutate_all(scaleZeroOne) %>% 
  mutate(ID=row_number()) %>% 
  pivot_longer(-ID) %>% 
  rename(variable=name,
         stdfvalue=value)

cb_shaps_long=left_join(cb_shaps_long,cb_shaps_stdfvalues)

# cb_shaps_long <- cb_shaps_long %>% 
#   arrange(variable,-mean_value) %>% 
#   group_by(variable) %>% 
#   mutate(indx=row_number()) %>% ungroup()


# create plot
x_bound <- max(abs(cb_shaps_long$value)) * 1.1
label_format = "%.3f"
plot1 <- cb_shaps_long %>% 
  filter(indx <=20) %>% 
  ggplot() + coord_flip(ylim = c(-x_bound, 
                                                            x_bound)) + 
  geom_hline(yintercept = 0) + ggforce::geom_sina(aes(x = reorder(variable,mean_value), 
                                                      y = value, color = stdfvalue), method = "counts", 
                                                  maxwidth = 0.7, alpha = 0.7) + 
  geom_text(data = unique(cb_shaps_long[cb_shaps_long$indx <=20, c("variable", "mean_value")]), aes(x =  reorder(variable,mean_value), y = -Inf, 
                                                                             label = sprintf(label_format, mean_value)),
            size = 3, alpha = 0.7, hjust = -0.2, fontface = "bold") + 
  scale_color_gradient(low = myCols[[1]], high = myCols[[3]], 
                       breaks = c(0, 1), labels = c(" Low", "High "), 
                       guide = guide_colorbar(barwidth = 12, barheight = 0.3)) + 
  theme_bw() + theme(axis.line.y = element_blank(), axis.ticks.y = element_blank(), 
                     legend.position = "bottom", legend.title = element_text(size = 10), 
                     legend.text = element_text(size = 8), axis.title.x = element_text(size = 10)) + 
  # scale_x_discrete(limits = rev(levels(cb_shaps_long$variable)), 
  #                  labels = label.feature(rev(levels(cb_shaps_long$variable)))) + 
  labs(y = "SHAP value (impact on model output)", 
       x = "", color = "Feature value  ")

plot1

OverReact::saveREACTplot(p = plot1, figpath = figpath,
                         filename = paste0("shap_plot_",outcome),width =8, height=6)



# Permutations analysis ---------------------------------------------------





parallelise=T
nclust=100
nperm=10
bonferroni=F
myheight=10
## Get permutation varimps
set.seed(12345)
cb_varimps_permute=PermuteCatboostParallel(mymodel = cb_model, X = X,y = y,nperm = nperm,
                                           parallelise=parallelise,nclust=nclust) 

# Get bonferroni adjusted pvalue
bonfpval=0.05/ncol(X)
if(bonferroni){
  mypval=bonfpval
}else{
  mypval=0.05
}

cb_varimps_permute$summary_stats <- cb_varimps_permute$summary_stats %>% 
  mutate(selected=case_when(prop_permutes_greater_than_unpermute < mypval ~ "Selected",
                            TRUE ~ "Not selected"))



### seconds plot, with distributions
plot_varimps_permute_boxplot <- cb_varimps_permute$full_results %>% 
  rename(predictor=name,
         loss_permute_delta=value) %>% 
  # left_join(predictors_df, by = c("predictor" = "variable_desc")) %>% 
  left_join(cb_varimps_permute$summary_stats) %>% 
  arrange(selected,mean_loss_permute_delta) %>% 
  mutate(indx=row_number()) %>% 
  top_n(100) %>% 
  ggplot(aes(y=reorder(predictor,indx), x=loss_permute_delta , 
             # height=loss_permute_delta,
             col=selected)) + 
  geom_boxplot() +
  geom_vline(xintercept = 0, linetype="dashed", col="grey50") +
  scale_fill_manual(values = myCols[c(3)]) +
  scale_colour_manual(values = myCols[c(3)]) +
  theme_bw() +
  scale_y_discrete(labels=function(x) str_wrap(x, width=33)) +
  labs(y="Variable",x="Change in AUC \nafter variable permutation", 
       col = "",  fill = "")

plot_varimps_permute_boxplot

OverReact::saveREACTplot(p = plot_varimps_permute_boxplot, figpath = figpath,
                         filename = paste0("permutation_varimps_boxplot_",outcome),
                         width =5, height=3)




# ROC analysis ------------------------------------------------------------


### Prediction evaluation ###

### FULL MODEL ###
preds <- catboost.predict(model = cb_model, pool = validation_pool,verbose = T,prediction_type = "Probability")

auc_ci_mod <- pROC::ci.auc(pull(test,outcome),preds)
auc_mod <- pROC::auc(pull(test,outcome),preds)
roc_mod <- pROC::roc(pull(test,outcome),preds)
ci.sp.obj <- pROC::ci.sp(roc_mod,sensitivities=seq(0,1,0.01), boot.n=500)

### Evaluate and plot
png(filename = paste0(figpath,"roc_",outcome,".png"), width = 9,height = 9,units = "in",res = 300)
plot(roc_mod, print.auc=F, grid = T, legacy.axes=T)
plot(ci.sp.obj, type="shape", col =  rgb(0.1,0.1,0.1, alpha = 0.2))
# plot(ci.sp.obj_symps, type = "shape", col = rgb(1,0.1,0.1, alpha = 0.2),
#      lwd = 0.01,
#      lty = 1)
# 
# plot(ci.sp.obj_clust, type = "shape", col = rgb(0.1,0.1,1, alpha = 0.2),
#      lwd = 0.01,
#      lty = 1)

lines(pROC::roc(pull(test,outcome),preds),lty=1, col = rgb(0.1,0.1,0.1, alpha = 1))
# lines(pROC::roc(pull(test,outcome),preds_symps),lty=1, col = rgb(1,0.1,0.1, alpha = 1))
# lines(pROC::roc(pull(test,outcome),preds_clust),lty=1, col = rgb(0.1,0.1,1, alpha = 1))

legend("bottomright", inset = c(0,0.05),lty=c(1,1,1), bty="n",
       col = c(col = rgb(0.1,0.1,0.1, alpha = 1), 
               rgb(0.1,0.1,1, alpha = 1),rgb(1,0.1,0.1, alpha = 1)),
       legend=c(paste0("AUC = ",
                       round(auc_ci_mod[2],2)," [",
                       round(auc_ci_mod[1],2),"-",
                       round(auc_ci_mod[3],2),"]")
                
                # paste0("Covariates + acute symptom cluster: AUC = ",
                #        round(auc_ci_mod_clust[2],2)," [",
                #        round(auc_ci_mod_clust[1],2),"-",
                #        round(auc_ci_mod_clust[3],2),"]"),
                # paste0("Covariates + acute symptoms: AUC = ",
                #        round(auc_ci_mod_symps[2],2)," [",
                #        round(auc_ci_mod_symps[1],2),"-",
                #        round(auc_ci_mod_symps[3],2),"]"))
       ))

dev.off()



