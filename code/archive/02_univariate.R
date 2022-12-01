rm(list=ls())
source("E:/Group/functions/load_packages.R")
source("E:/Group/functions/cats_and_covs.R")
source("E:/Group/react2_study5/report_phases_combined/projects/delta_symptom_prediction/code/00_functions.R", local = T)

load_packages(c("dplyr", "OverReact","tidyverse","ggplot2","ggthemes", "stats",
                "text2map","tidytext","focus","gridExtra", "future.apply", "lubridate","ggbeeswarm","broom",
                "foreach","doParallel","catboost","ggnetwork","ppcor","zoo","scales","ggpubr",
                "tidytext","patchwork"))
outpath <- "E:/Group/react2_study5/report_phases_combined/projects/covid_lit_analysis/output/eda/"
figpath <- "E:/Group/react2_study5/report_phases_combined/projects/covid_lit_analysis/plots/eda/"
options(scipen=999)

# Load data ---------------------------------------------------------------

dat <- readRDS("data/dat_main.rds")
titles_dtm_tidy_wide <- readRDS("data/dat_text.rds")

# replace all vals above 1 with 1
dat[,15:ncol(dat)] <- replace(x = dat[,15:ncol(dat)] ,
                             dat[,15:ncol(dat)] >1,1)


# Univariate modelling ----------------------------------------------------
# mydat=dat[1:1000,]
# function to run univariate analysis
runUnivariate <- function(mydat, preprint_filter=c("Journal article","Preprint"),
                          var = "cites_per_day", family="gaussian",
                          wordlist = names(titles_dtm_tidy_wide)[3:ncol(titles_dtm_tidy_wide)],
                          wordcount_threshold = 10){
  
  
  # create index for filtering papers
  mydat <- mydat %>% dplyr::filter(preprint %in% preprint_filter)
  
  # add wordcount
  wordcounts <- colSums(mydat[,wordlist], na.rm=T)
  excludewords=names(wordcounts)[(wordcounts<wordcount_threshold)]
  newwordlist=names(wordcounts)[(wordcounts>=wordcount_threshold)]
  
  
  # filter
  mydat <- mydat %>% dplyr::select(-excludewords)
  
  ### All papers ###
  univ_df <- cbind(y=pull(.data = mydat,var = get(var)), mydat[,newwordlist])
    
  df_out <- univ_df %>% 
    reshape2::melt(id.vars = "y") %>% 
    # mutate(y = ifelse(family=="binomial", factor(y, levels = c("0","1")),y)) %>% 
    group_by(variable) %>% 
    do(tidy(glm(y ~ value, data = .,family = family)))
  univ_citerate_reg <- df_out %>% 
    filter(term != "(Intercept)")%>% 
    dplyr::select(-term) 
  
  # Add wordcount
  univ_citerate_reg$wordcount <- wordcounts[wordcounts>=wordcount_threshold]

  # convert y to numeric 
  univ_df$y <- univ_df$y %>% as.character()%>% as.numeric() 
  
  # get average outcome val
  meanval=c()
  meanval_without=c()
  sdval=c()
  # table(pull(df,(outcome))[df[,i]==1])
# i <- 614
  for(i in 2:ncol(univ_df)){
    meanval <- c(meanval,mean(pull(univ_df,y)[univ_df[,i]>0], na.rm=T))
    meanval_without <- c(meanval_without,mean(pull(univ_df,y)[univ_df[,i]==0], na.rm=T))
    sdval <- c(sdval,sd(pull(univ_df,(y))[univ_df[,i]>0], na.rm=T))
  }
  
  univ_citerate_reg$mean_outcome_val <- meanval
  univ_citerate_reg$se_outcome_val <- sdval/sqrt(wordcounts[wordcounts>=wordcount_threshold])
  univ_citerate_reg$mean_outcome_val_lower <- meanval - 1.96*univ_citerate_reg$se_outcome_val
  univ_citerate_reg$mean_outcome_val_upper <- meanval + 1.96*univ_citerate_reg$se_outcome_val
  univ_citerate_reg$mean_outcome_val_without <- meanval_without
  
  #adjust p-values
  univ_citerate_reg$pvalue_bonf <- stats::p.adjust(p = univ_citerate_reg$p.value,method = "bonferroni")
  univ_citerate_reg$significant = case_when(univ_citerate_reg$pvalue_bonf <= 0.05 ~ "p<2.76e-05 (Bonferroni)",
                                            univ_citerate_reg$p.value<=0.05 ~ "p<0.05",
                                            TRUE ~ "Not significant")
  #convert to character
  univ_citerate_reg$variable <- as.character(univ_citerate_reg$variable)
  return(univ_citerate_reg)

  
}
# univ_df %>% group_by(outcome) %>% summarise(n=n(),
#                                       meanval=mean(y, na.rm=T))


# which(colnames(univ_df)=="outcome")

univ_citerate_reg$journal
# Plot univariate
plotUnivariate <- function(univ_citerate_reg){
  # univ_citerate_reg <- univ_citerate_reg  %>% 
  #   arrange(desc(p.value)) %>% 
  #   group_by(journal) %>% 
  #   mutate(indx=row_number())  %>% 
  #   ungroup() %>%
  #   mutate(label=case_when(indx >10 ~ NA_character_,
  #                          TRUE ~ variable))
    
  p_univ <- univ_citerate_reg  %>% 
    arrange((p.value)) %>% 
    group_by(journal) %>% 
    mutate(indx=row_number())  %>% 
    ungroup() %>%
    mutate(label=case_when(indx >20 ~ NA_character_,
                           TRUE ~ variable))%>% 
    ggplot(aes(x=(estimate),y=-log10(p.value), col = significant, label=label)) +
    geom_point() +
    ggrepel::geom_text_repel(col = "black", size = 3.5) +
    theme_adjust+
    theme_bw() +
    {if("journal"%in%colnames(univ_citerate_reg))
      facet_wrap(.~journal, scales="free_x")
    } +
    scale_color_manual(values = c("grey60", "orange", "firebrick3")) +
    labs(x= "Beta coefficient", y="-log10 p-value", col = "") +
    theme_adjust
  p_univ
  return(p_univ)
}


# Run analyses ------------------------------------------------------------



# Run analysis on cites_per_day
univ_cpd_journal <- runUnivariate(dat, preprint_filter=c("Journal article"),var = "cites_per_day",
                                  family = "gaussian",wordcount_threshold = 100)
univ_cpd_journal$journal <- "Journal article"

# Run analysis on cites_per_day
univ_cpd_preprint <- runUnivariate(dat, preprint_filter=c("Preprint"),var = "cites_per_day",
                                   family = "gaussian",wordcount_threshold = 10)
univ_cpd_preprint$journal <- "Preprint"

# Run analysis on cites_per_day
univ_alt_journal <- runUnivariate(dat, preprint_filter=c("Journal article"), var = "altmetrics.score",
                                  family = "gaussian",wordcount_threshold = 100)
univ_alt_journal$journal <- "Journal article"

# Run analysis on cites_per_day
univ_alt_preprint <- runUnivariate(mydat = dat, preprint_filter=c("Preprint"),var = "altmetrics.score",
                                   family = "gaussian",wordcount_threshold = 10)
univ_alt_preprint$journal <- "Preprint"


table(dat[dat$preprint=="Preprint",]$iverm)
dat_top_altmetrics <- dat[order(dat$altmetrics.score,na.last = T,decreasing = T),][1:20,]

# Plot --------------------------------------------------------------------

univ_cpd_comb <- rbind(univ_cpd_journal,univ_cpd_preprint)
univ_cpd_comb$variable <- as.character(univ_cpd_comb$variable)
p_univ_cpd_comb <- plotUnivariate(univ_citerate_reg = univ_cpd_comb)
p_univ_cpd_comb

univ_alt_comb <- rbind(univ_alt_journal,univ_alt_preprint)
univ_alt_comb$variable <- as.character(univ_alt_comb$variable)
p_univ_alt_comb <- plotUnivariate(univ_citerate_reg = univ_alt_comb)
p_univ_alt_comb

# save
OverReact::saveREACTplot(p = p_univ_cpd_comb,figpath = figpath,
                         filename = "univariate_citerate_combined",
                         width = 10,height = 8)
# save
OverReact::saveREACTplot(p = p_univ_alt_comb,figpath = figpath,
                         filename = "univariate_altmetrics_combined",
                         width = 10,height = 8)


# PLots -------------------------------------------------------------------

# add lower and upper bounds
univ_cpd_comb$lower=univ_cpd_comb$estimate-1.96*univ_cpd_comb$std.error
univ_cpd_comb$upper=univ_cpd_comb$estimate+1.96*univ_cpd_comb$std.error
univ_alt_comb$lower=univ_alt_comb$estimate-1.96*univ_alt_comb$std.error
univ_alt_comb$upper=univ_alt_comb$estimate+1.96*univ_alt_comb$std.error

# combined all
univall_comb=full_join(univ_cpd_comb,univ_alt_comb,by = "variable", suffix = c("_cpd", "_alt"))

# scatter of effect sizes
univall_comb %>% 
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



# Preprint publication analysis -------------------------------------------

# get preprints
dat_preprints <- dat %>% filter(preprint=="Preprint")
dat_preprints$published_factor <- factor(dat_preprints$published,levels = c("0","1"))

#run univ
univ_pub_preprint <- runUnivariate(mydat = dat_preprints, preprint_filter=c("Preprint"),
                                   var = "published_factor",wordcount_threshold = 100,family = "binomial")

