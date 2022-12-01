
#' First clear the environment of variables
rm(list=ls(all=TRUE))

outpath <- paste0(getwd(),"/output/")
figpath <- paste0(getwd(),"/plots/")



#' Source any functions from the local file
source("E:/Group/functions/model_table.R")
source("E:/Group/functions/load_packages.R")
source("E:/Group/functions/model_maker.R")
source("E:/Group/functions/cats_and_covs.R")
source("E:/Group/functions/save_pheatmap.R")
source("E:/Group/functions/hamming_distance.R")
source("E:/Group/functions/longCovidPrevTables.R")
source("E:/Group/functions/LassoSatabilitySelection.R")
source("E:/Group/functions/relevel_all_categorical_vars.R")
source("E:/Group/functions/create_react_week_number.R")
source(file = "code/0_functions.R")

source("E:/Group/react2_study5/report_phases_combined/projects/function_scripts/create_subfolder.R")
source("E:/Group/react2_study5/report_phases_combined/projects/function_scripts/forest_plot.R")
source("E:/Group/react2_study5/report_phases_combined/projects/function_scripts/stability_selection.R")

#' Pull in packages needed
package.list <- c("prevalence","mgcv","knitr","dplyr","factoextra","tidyr",
                  "ggplot2","gdata","ggsci", "RColorBrewer", "tidyverse", "lubridate", 
                  "ComplexHeatmap","gganimate","ggnetwork","ppcor",
                  "readr","ggthemes", "questionr", "gridExtra","scales","ggpubr",
                  "Rtsne","rsvd", "geometry","Rcpp","furrr","future","focus",
                  "patchwork", "OverReact")
load_packages(package.list)

# create subfolder
createMySubfolder(subfolderName = "stability_selection")

# no scientific notation
options(scipen = 999)


# Load data ---------------------------------------------------------------

# Save
dat <- readRDS("data/dat_main.rds")
dat_text_titles <- readRDS("data/dat_text_abstracts_bow.rds")
head(dat_text_titles)
suffix="abstracts"



#### STABILITY SELECTION ###

outcome="published"

# create holdout data set
df_mod <- dat %>% filter(preprint=="Preprint", date <= as.Date("2020-06-01")) %>% 
  mutate(unique_id=as.numeric(unique_id)) %>% 
  dplyr::select(published,unique_id) %>% 
  left_join(dat_text_titles) %>% 
  dplyr::select(-unique_id)

df_mod <- df_mod[rowSums(is.na(df_mod),na.rm=T)==0,]

table(df_mod$published, exclude = "none")
df_mod <- df_mod[colSums(df_mod,na.rm=T)>10]


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
OverReact::saveREACT(stab_out,outpath = outpath,filename = paste0("stability_selection_results_",outcome,"_",suffix))

# Calibrationplot
png(paste0(figpath,"stability_selection_calbration_plot_",outcome,"_",suffix,".png"),width = 7,height = 7, units="in", res = 300)
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
                                    family = "binomial",getLoss = F)

# plot!
myplot=plotStabResultsStripedFlipped(results_df, opt_thresh=stab_out$P[opt_iter], 
                                     stab_out = stab_out,
                                     plotOnlyTopN = 100,
                                     plotOnlySelected = F,
                                     plot_aucs = F)

myplot


OverReact::saveREACTplot(p = myplot,figpath = figpath,
                         filename = paste0("stability_selection_plot_",outcome,"_",suffix),
                         width = 9,height = 8)


