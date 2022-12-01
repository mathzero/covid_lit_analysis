rm(list=ls())
source("E:/Group/functions/load_packages.R")
source("E:/Group/functions/cats_and_covs.R")
source("E:/Group/react2_study5/report_phases_combined/projects/delta_symptom_prediction/code/00_functions.R", local = T)
source("E:/Group/react2_study5/report_phases_combined/projects/function_scripts/stability_selection.R", local = T)

load_packages(c("dplyr", "OverReact","tidyverse","tm","wordcloud","ggplot2","ggthemes","stats",
                "text2map","tidytext","focus","gridExtra", "future.apply", "lubridate","ggbeeswarm","broom",
                "foreach","doParallel","catboost","ggnetwork","ICoLour","ppcor","zoo","scales","ggpubr","Kendall",
                "tidytext", "quanteda", "widyr", "igraph", "ggraph", "patchwork"))
outpath <- "E:/Group/react2_study5/report_phases_combined/projects/covid_lit_analysis/output/stability_analysis/"
figpath <- "E:/Group/react2_study5/report_phases_combined/projects/covid_lit_analysis/plots/stability_analysis/"


# Load data ---------------------------------------------------------------

# Save
dat <- readRDS("data/dat_main.rds")

dat$date %>% table()




#### STABILITY SELECTION ###

outcome="published"
# create holdout data set
df_mod <- dat %>% filter(preprint=="Preprint", date <= as.Date("2020-06-01")) %>% 
  dplyr::select(-c(id,document,cites_per_day,research_org_country_names ,
            altmetrics.id,resulting_publication_doi,date,preprint,
            `Days since publication`,title.preferred, doi))

table(df_mod$published, exclude = "none")
df_mod <- df_mod[colSums(df_mod)>10]


set.seed(123)
split <- caret::createDataPartition(pull(df_mod,outcome),times = 1,p = 0.7, list =F)
splitindex <- 1:nrow(df_mod) %in% split
table(splitindex)
train=df_mod[splitindex,]
test=df_mod[!splitindex,]


# Variable selection with FOcus stability selection -----------------------
X=train %>% dplyr::select(-c(published))
y=train %>% dplyr::select(published) %>% unlist()
myK=200
myseed=123
stab_out <- focus::VariableSelection(xdata = X, 
                                     ydata = y,
                                     # penalty.factor = penalty.factor,
                                     # pi_list = c(0.5,0.6,0.7,0.8),
                                     K = myK,
                                     seed = myseed,
                                     family = "binomial",
                                     PFER_method = "MB",
                                     # n_cores = 30,
                                     verbose = T,
                                     resampling = "bootstrap",
                                     output_data = T
)

### Save results
OverReact::saveREACT(stab_out,outpath = outpath,filename = paste0("stability_selection_results_",outcome))

# Calibrationplot
png(paste0(figpath,"stability_selection_calbration_plot_",outcome,".png"),width = 7,height = 7, units="in", res = 300)
par(mar=c(7,5,7,6))
focus::CalibrationPlot(stab_out)
dev.off()


# Plot results ------------------------------------------------------------
X_test=test %>% dplyr::select(-published)
y_test=test %>% dplyr::select(published) %>% unlist()
opt_iter=which.max(stab_out$S)

# get incremental results
results_df <- getIncrementalSummary(stab_out = stab_out,
                                    ydata = y_test,
                                    xdata = X_test,
                                    reorder_by_normalised_beta = F,
                                    K = 20,
                                    n_thr = min(20,ncol(X_test)),
                                    family = "binomial")
# plot!
myplot=plotStabResultsStripedFlipped(results_df, opt_thresh=stab_out$P[opt_iter], 
                                     stab_out = stab_out,plotOnlyTopN = 100,plotOnlySelected = F)

myplot


OverReact::saveREACTplot(p = myplot,figpath = figpath,
                         filename = paste0("stability_selection_plot_",outcome),
                         width = 9,height = 8)



# Catboost ----------------------------------------------------------------

# load catboost functions from LC paper
source("E:/Group/react2_study5/report_phases_combined/projects/long_covid_2021/code/00_functions.R", local = T)
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
learn_pool = catboost.load_pool(data = X_learn,label = pull(train_learn,{{outcome}}), cat_features = 3:(ncol(X_learn)-1))
test_pool = catboost.load_pool(data = X_valid,label = pull(train_valid,{{outcome}}), cat_features = 3:(ncol(X_valid)-1))
validation_pool = catboost.load_pool(data =X_test,label = pull(test,{{outcome}}), cat_features = 3:(ncol(X_test)-1))


# train model
cb_model = catboost.train(learn_pool = learn_pool,test_pool = test_pool, params = my_params)


# save.image(file = "prediction.rdata")
load("prediction.rdata")

# get variable importances
basemodel <- getFeatureImpAndSHAP(cb_model = cb_model,validation_pool = validation_pool,
                                  learn_pool=learn_pool,
                                  modelvars = "covariates",
                                  X = test %>% dplyr::select(-{{ outcome }}),
                                  y = pull(test,{{outcome}}),
                                  nperm = 100,
                                  myheight = 6.5,
                                  bandwidth_scaler = 15,
                                  train_learn = train_learn,
                                  outcome = outcome,
                                  parallelise = T,nclust = 100)

predictors_df