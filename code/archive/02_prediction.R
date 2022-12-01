rm(list=ls())
source("E:/Group/functions/load_packages.R")
load_packages(c("dplyr", "OverReact","tidyverse","qdap","tm","wordcloud","ggplot2","ggthemes", 
                "text2map","tidytext","focus","gridExtra", "future.apply", 
                "foreach","doParallel","catboost","ggnetwork","ICoLour",
                "tidytext", "quanteda", "widyr", "igraph", "ggraph", "patchwork"))
outpath <- "E:/Group/react2_study5/report_phases_combined/projects/covid_lit_analysis/output/"
figpath <- "E:/Group/react2_study5/report_phases_combined/projects/covid_lit_analysis/plots/"
#Source functions from children prediction project folder
source("E:/Group/react2_study5/report_phases_combined/projects/delta_symptom_prediction/code/00_functions.R", local = T)

# Load data ---------------------------------------------------------------

# Save
dat <- readRDS("data/dat_main.rds")
titles_dtm_tidy_wide <- readRDS("data/dat_text.rds")
titles_dtm_tidy_wide[is.na(titles_dtm_tidy_wide)] <- 0

df_mod <- cbind(altmetrics_score=as.numeric(dat$altmetrics.score),titles_dtm_tidy_wide %>% select(-id,-document))
# Remove NAs
df_mod <- df_mod[!is.na(df_mod$altmetrics_score),]


# create holdout data set
set.seed(123)
split <- caret::createDataPartition(pull(df_mod,"outcome"),times = 1,p = 0.7, list =F)
splitindex <- 1:nrow(df_mod) %in% split
table(splitindex)
train=df_mod[splitindex,]
test=df_mod[!splitindex,]

# Variable selection with FOcus stability selection -----------------------

X=train %>% select(-altmetrics_score)
y=train %>% select(altmetrics_score) %>% unlist()
myK=100
myseed=123
stab_out <- focus::VariableSelection(xdata = X, 
                                     ydata = y,
                                     # penalty.factor = penalty.factor,
                                     # pi_list = c(0.5,0.6,0.7,0.8),
                                     K = myK,
                                     seed = myseed,
                                     family = "gaussian",
                                     PFER_method = "MB",
                                     # n_cores = 30,
                                     verbose = T,
                                     resampling = "bootstrap",
                                     output_data = T
)

### Save results
OverReact::saveREACT(stab_out,outpath = outpath,filename = paste0("stability_selection_results"))

# Calibrationplot
png(paste0(figpath,"stability_selection_calbration_plot.png"),width = 7,height = 7, units="in", res = 300)
par(mar=c(7,5,7,6))
focus::CalibrationPlot(stab_out)
dev.off()


# Plot results ------------------------------------------------------------
X_test=test %>% select(-altmetrics_score)
y_test=test %>% select(altmetrics_score) %>% unlist()
opt_iter=which.max(stab_out$S)
results_df <- getStabSummary(stab_out = stab_out,y_test = y_test,X_test = X_test,family = "gaussian")
myplot=plotStabResults(results_df, opt_thresh=stab_out$P[opt_iter])
myplot

OverReact::saveREACTplot(p = myplot,figpath = figpath,
                         filename = "stability_selection_plot",
                         width = 3+nrow(results_df)*0.15,height = 8)


