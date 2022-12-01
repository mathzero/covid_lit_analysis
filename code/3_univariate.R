
#' First clear the environment of variables
rm(list=ls(all=TRUE))

outpath <- paste0(getwd(),"/output/")
figpath <- paste0(getwd(),"/plots/")
datapath <- paste0(getwd(),"/data/")
function_script_path ="E:/home/mw418/function_scripts/"
function_script_path="/Users/mathzero/Library/CloudStorage/GoogleDrive-mattdwhitaker@gmail.com/My Drive/Imperial/PhD/REACT2_analysis_in_progress/RESULTS/function_scripts/"


#' Source any functions from the local file
source(paste0(function_script_path,"/load_packages.R"))
source(paste0(function_script_path,"/cats_and_covs.R"))
source(paste0(function_script_path,"/create_subfolder.R"))
source(paste0(function_script_path,"/forest_plot.R"))

source(file = "code/0_functions.R")

#' Pull in packages needed
package.list <- c("prevalence","mgcv","knitr","dplyr","factoextra","tidyr","tidyverse",
                  "ggplot2","gdata","ggsci", "RColorBrewer", "tidyverse", "lubridate", 
                  "ComplexHeatmap","gganimate","ggnetwork","ppcor","tidytext",
                  "readr","ggthemes", "questionr", "gridExtra","scales","ggpubr",
                  "Rtsne","rsvd", "geometry","Rcpp","furrr","future","sharp","foreach",
                  "doParallel","stabilityshap",
                  "patchwork", "OverReact")
load_packages(package.list)

# create subfolder
createMySubfolder(subfolderName = "univariate")

# no scientific notation
options(scipen = 999)


# Load data ---------------------------------------------------------------

# Save
dat <- readRDS("data/dat_main.rds")
sparse_dfs <- readRDS("data/sparse_dfs_1000_min.rds")
dat_text_abstracts <- sparse_dfs$df_text_sparse_abstract %>% as.matrix() %>% as.data.frame()
dat_text_titles <- sparse_dfs$df_text_sparse_title %>% as.matrix() %>% as.data.frame()
dat_text_abstracts$unique_id=as.character(rownames(dat_text_abstracts))
dat_text_titles$unique_id=as.character(rownames(dat_text_titles))

# define suffix
suffix="abstracts"
dat_text=dat_text_titles %>% dplyr::select(unique_id, everything())


# replace all vals above 1 with 1
dat_text[,2:ncol(dat_text)] <- replace(x = dat_text[,2:ncol(dat_text)] ,
                                                               dat_text[,2:ncol(dat_text)] >1,1)

# get wordlist for function
wordlist = names(dat_text)[2:ncol(dat_text)]

# create holdout data set
df_mod <- dat %>% 
  # mutate(unique_id=as.numeric(unique_id)) %>% 
  # filter(date <= as.Date("2020-06-01")) %>% 
  dplyr::select(title_preferred,published,preprint,cites_per_day,
                altmetrics_score,unique_id,date) %>% 
  rename(date_published=date,
         published_in_journal=published
         ) %>% 
  rename(article_type=preprint) %>% 
  left_join(dat_text, by="unique_id") %>% 
  dplyr::select(-unique_id)
df_mod <- df_mod[rowSums(is.na(df_mod),na.rm=T)==0,]
rm(dat_text_abstracts)
rm(dat_text)
rm(dat_text_titles)

wordfreqs=colSums(df_mod)
# mydat=df_mod


# Univariate modelling ----------------------------------------------------


# Run analyses ------------------------------------------------------------



# Run analysis on cites_per_day
univ_cpd_journal <- runUnivariate(mydat = df_mod, preprint_filter=c("Journal article"),
                                  var = "cites_per_day",wordlist = wordlist,
                                  family = "gaussian",wordcount_threshold = 100,
                                  n = 20)
univ_cpd_journal$journal <- "Journal article"

# Run analysis on cites_per_day
univ_cpd_preprint <- runUnivariate(mydat = df_mod, preprint_filter=c("Preprint"),
                                   var = "cites_per_day",wordlist = wordlist,
                                   family = "gaussian",wordcount_threshold = 10,n = 10)
univ_cpd_preprint$journal <- "Preprint"

# Run analysis on cites_per_day
univ_alt_journal <- runUnivariate(mydat = df_mod,  preprint_filter=c("Journal article"), var = "altmetrics_score",
                                  family = "gaussian",wordcount_threshold = 100,
                                  wordlist = wordlist,n = 20)
univ_alt_journal$journal <- "Journal article"

# Run analysis on cites_per_day
univ_alt_preprint <- runUnivariate(mydat = df_mod,  preprint_filter=c("Preprint"),var = "altmetrics_score",
                                   wordlist = wordlist,
                                   family = "gaussian",wordcount_threshold = 10,n = 10)
univ_alt_preprint$journal <- "Preprint"



# Run analysis on published
univ_pub_preprint <- runUnivariate(mydat = df_mod %>% filter(date<="2020-06-01"),  preprint_filter=c("Preprint"),
                                   var = "published",wordlist = wordlist,
                                   family = "binomial",wordcount_threshold = 20,n = 10)
univ_pub_preprint$journal <- "Preprint"



# dat_top_altmetrics <- dat[order(dat$altmetrics.score,na.last = T,decreasing = T),][1:20,]

# Plot --------------------------------------------------------------------

# cites per day
univ_cpd_comb <- rbind(univ_cpd_journal,univ_cpd_preprint)
univ_cpd_comb$variable <- as.character(univ_cpd_comb$variable)
p_univ_cpd_comb <- plotUnivariate(univ_citerate_reg = univ_cpd_comb)
p_univ_cpd_comb

# altmetrics
univ_alt_comb <- rbind(univ_alt_journal,univ_alt_preprint)
univ_alt_comb$variable <- as.character(univ_alt_comb$variable)
p_univ_alt_comb <- plotUnivariate(univ_citerate_reg = univ_alt_comb)
p_univ_alt_comb

# published in journal
p_univ_pub <- plotUnivariate(univ_citerate_reg = univ_pub_preprint %>% 
                               filter(mean_outcome_val>0, variable!="published"))
p_univ_pub

# save
OverReact::saveREACTplot(p = p_univ_cpd_comb,figpath = figpath,
                         filename = paste0("univariate_citerate_combined",suffix),
                         width = 10,height = 8)
# save
OverReact::saveREACTplot(p = p_univ_alt_comb,figpath = figpath,
                         filename = paste0("univariate_altmetrics_combined",suffix),
                         width = 10,height = 8)

# save
OverReact::saveREACTplot(p = p_univ_pub,figpath = figpath,
                         filename = paste0("univariate_published",suffix),
                         width = 6,height = 8)

# PLots -------------------------------------------------------------------

# add lower and upper bounds
univ_cpd_comb$lower=univ_cpd_comb$estimate-1.96*univ_cpd_comb$std.error
univ_cpd_comb$upper=univ_cpd_comb$estimate+1.96*univ_cpd_comb$std.error
univ_alt_comb$lower=univ_alt_comb$estimate-1.96*univ_alt_comb$std.error
univ_alt_comb$upper=univ_alt_comb$estimate+1.96*univ_alt_comb$std.error

# combined all
univall_comb=full_join(univ_cpd_comb,univ_alt_comb,by = "variable", suffix = c("_cpd", "_alt"))

# scatter of effect sizes
p_univ_compare <- univall_comb %>% 
  arrange(desc(estimate_alt)) %>% 
  # group_by(journal) %>% 
  mutate(indx=row_number())  %>% 
  arrange(desc(estimate_cpd)) %>% 
  # group_by(journal) %>% 
  mutate(indx2=row_number())  %>% 
  # ungroup() %>%
  mutate(label=case_when(indx >2 | indx2 > 4 ~ NA_character_,
                         TRUE ~ variable))%>% 
  # filter(pvalue_bonf_cpd <=0.05 | pvalue_bonf_alt <= 0.05) %>% 
  ggplot(aes(x=estimate_alt, y= estimate_cpd, label = variable)) + 
  geom_point(alpha=0.6) +
  ggrepel::geom_text_repel(col = "black", size = 3.5) +
  theme_bw() +
  theme_adjust +
  geom_smooth(method = "lm")


# save
OverReact::saveREACTplot(p = p_univ_compare,figpath = figpath,
                         filename = paste0("univariate_alt_cpd_compare_",suffix),
                         width = 6,height = 5)

