rm(list=ls())
source("E:/Group/functions/load_packages.R")
source("E:/Group/functions/cats_and_covs.R")
source("E:/Group/react2_study5/report_phases_combined/projects/delta_symptom_prediction/code/00_functions.R", local = T)

load_packages(c("dplyr", "OverReact","tidyverse","tm","wordcloud","ggplot2","ggthemes", "Rfast","stats",
                "text2map","tidytext","focus","gridExtra", "future.apply", "lubridate","ggbeeswarm","broom",
                "foreach","doParallel","catboost","ggnetwork","ICoLour","ppcor","zoo","scales","ggpubr",
                "tidytext", "quanteda", "widyr", "igraph", "ggraph", "patchwork"))
outpath <- "E:/Group/react2_study5/report_phases_combined/projects/covid_lit_analysis/output/eda/"
figpath <- "E:/Group/react2_study5/report_phases_combined/projects/covid_lit_analysis/plots/eda/"


# Load data ---------------------------------------------------------------

# Save
dat <- readRDS("data/dat_main.rds")
titles_dtm_tidy_wide <- readRDS("data/dat_text.rds")


# Univariate modelling ----------------------------------------------------

# create index for filtering papers
indx=!is.na(dat$citations_count) & dat$preprint=="Journal article"

### All papers ###
regression_dat <- dat[indx,] 
regression_dat_words <- titles_dtm_tidy_wide[indx,3:ncol(titles_dtm_tidy_wide)]
 

# Univaariate regressions for citation count and rate
univ_citecount_reg <- cbind(x=regression_dat$cites_per_day,
                            regression_dat_words
                            ) %>% 
  gather(measure,value, -x) %>% 
  nest(-measure) %>% 
  mutate(fit=map(data, ~ lm(value ~ x, data = .x)),
         tidied = map(fit, tidy)) %>% 
  unnest(tidied)


# attempt 2
univ_citecount_reg_2 <- cbind(x=regression_dat$cites_per_day,
      regression_dat_words) %>% 
  as.tibble() %>% 
  gather(key = "column", value = "value")



univ_citecount <- Rfast::allbetas(x = as.matrix(regression_dat_words),
                                  y=-regression_dat$citations_count,pvalue = T) %>% as.data.frame()
univ_citerate <- Rfast::allbetas(x = as.matrix(regression_dat_words),
                                 regression_dat$cites_per_day,pvalue = T)%>% as.data.frame()
univ_citecount$word <- rownames(univ_citecount)
univ_citerate$word <- rownames(univ_citerate)
univ_citecount$wordcount <- colSums(regression_dat_words)
univ_citerate$wordcount <- colSums(regression_dat_words)

lm(x=as.matrix(regression_dat_words)[,1],y=regression_dat$citations_count,method = "gaussian", 
   data=cbind(as.matrix(regression_dat_words)[,1],regression_dat$citations_count))


# Adjust pvalues
univ_citecount$pvalue_bonf <- stats::p.adjust(p = univ_citecount$pvalue,method = "bonferroni")
univ_citecount$significant = case_when(univ_citecount$pvalue_bonf <= 0.05 ~ "p<2.76e-05 (Bonferroni)",
                                       univ_citecount$pvalue<=0.05 ~ "p<0.05",
                                       TRUE ~ "Not significant")
table(univ_citecount$significant)

univ_citerate$pvalue_bonf <- stats::p.adjust(p = univ_citerate$pvalue,method = "bonferroni")
univ_citerate$significant = case_when(univ_citerate$pvalue_bonf <= 0.05 ~ "p<2.76e-05 (Bonferroni)",
                                       univ_citerate$pvalue<=0.05 ~ "p<0.05",
                                       TRUE ~ "Not significant")
table(univ_citerate$significant)
  
# Plot univariate
univ_citerate %>% 
ggplot(aes(x=(be),y=-log10(pvalue))) +
  geom_point()



#### STABILITY SELECTION ###

outcome="cites_per_day"
# create holdout data set
df_mod <- cbind(cites_per_day=regression_dat$cites_per_day,regression_dat_words)
set.seed(123)
split <- caret::createDataPartition(pull(df_mod,outcome),times = 1,p = 0.7, list =F)
splitindex <- 1:nrow(df_mod) %in% split
table(splitindex)
train=df_mod[splitindex,]
test=df_mod[!splitindex,]

# Variable selection with FOcus stability selection -----------------------

X=train %>% dplyr::select(-cites_per_day)
y=train %>% dplyr::select(cites_per_day) %>% unlist()
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
OverReact::saveREACT(stab_out,outpath = outpath,filename = paste0("stability_selection_results_",outcome))

# Calibrationplot
png(paste0(figpath,"stability_selection_calbration_plot_",outcome,".png"),width = 7,height = 7, units="in", res = 300)
par(mar=c(7,5,7,6))
focus::CalibrationPlot(stab_out)
dev.off()


# Plot results ------------------------------------------------------------
X_test=test %>% dplyr::select(-cites_per_day)
y_test=test %>% dplyr::select(cites_per_day) %>% unlist()
opt_iter=which.max(stab_out$S)
results_df <- getStabSummary(stab_out = stab_out,y_test = y_test,X_test = X_test,family = "gaussian")
myplot=plotStabResults(results_df, opt_thresh=stab_out$P[opt_iter],plotOnlySelected = T,loss = "RMSE")
myplot

OverReact::saveREACTplot(p = myplot,figpath = figpath,
                         filename = "stability_selection_plot",
                         width = 3+nrow(results_df)*0.15,height = 8)





# ALtmetrics prediction ---------------------------------------------------


# same again for altmetrics
regression_dat <- dat[!is.na(dat$altmetrics.score),]
regression_dat_words <- titles_dtm_tidy_wide[!is.na(dat$altmetrics.score),3:ncol(titles_dtm_tidy_wide)]
univ_altmetrics <- Rfast::allbetas(x = as.matrix(regression_dat_words),
                                   regression_dat$altmetrics.score,pvalue = T)%>% as.data.frame()
univ_altmetrics$word <- rownames(univ_altmetrics)
univ_altmetrics$wordcount <- colSums(regression_dat_words)


