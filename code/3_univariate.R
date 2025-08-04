
#' First clear the environment of variables
rm(list=ls(all=TRUE))

outpath <- paste0(getwd(),"/output/")
figpath <- paste0(getwd(),"/plots/")
datapath <- paste0(getwd(),"/data/")
function_script_path="/Users/mathzero/Library/CloudStorage/GoogleDrive-mattdwhitaker@gmail.com/My Drive/Imperial/PhD/REACT2_analysis_in_progress/RESULTS/function_scripts/"
function_script_path ="/Users/mw418/analysis/RESULTS/function_scripts/"
function_script_path ="E:/home/mw418/function_scripts/"


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
dat <- readRDS("data/bq_exports_mar_2023/clean/dat_main.rds")
sparse_dfs <- readRDS("data/sparse_dfs_1000_min.rds")
dat_text_abstracts <- sparse_dfs$df_text_sparse_abstract %>% as.matrix() %>% as.data.frame()
dat_text_titles <- sparse_dfs$df_text_sparse_title %>% as.matrix() %>% as.data.frame()
dat_text_abstracts$unique_id=as.character(rownames(dat_text_abstracts))
dat_text_titles$unique_id=as.character(rownames(dat_text_titles))


suffix="abstracts"

if(suffix=="abstracts"){
  dat_text=dat_text_abstracts %>% dplyr::select(unique_id, everything())
  
}else{
  dat_text=dat_text_titles %>% dplyr::select(unique_id, everything())
  
}

# convert to numeric where possible
dat <- OverReact::checkConvertNumeric(dat)
# dat_text <- OverReact::checkConvertNumeric(dat_text)
# dat_text$unique_id <- as.numeric(dat_text$unique_id)

# define 
# 
# # replace all vals above 1 with 1
# dat_text[,2:ncol(dat_text)] <- replace(x = dat_text[,2:ncol(dat_text)] ,
#                                                                dat_text[,2:ncol(dat_text)] >1,1)

# get wordlist for function
wordlist = names(dat_text)[2:ncol(dat_text)]
wordlist=setdiff(wordlist,"id")


# anchor date
extractdate=as.Date("2023-03-20")


# create holdout data set
df_mod <- dat %>% 
  mutate(published=case_when(!is.na(resulting_publication_doi)~1,
                             T ~0),
         days_since_publication=as.numeric(extractdate-date),
         cites_per_day=citations_count/days_since_publication) %>% 
  # mutate(unique_id=as.numeric(unique_id)) %>% 
  # filter(date <= as.Date("2020-06-01")) %>% 
  dplyr::select(title_preferred,published,type,cites_per_day,
                score,id,date) %>% 
  
  rename(date_published=date,
         altmetrics_score=score,
         published_in_journal=published
         ) %>% 
  rename(article_type=type) %>% 
  left_join(dat_text, by=c("id"="unique_id")) %>% 
  dplyr::select(-id)


# \ delete fully missing rows
df_mod <- df_mod[rowSums(is.na(df_mod),na.rm=T)==0,]

rm(dat_text_abstracts)
rm(dat_text)
rm(dat_text_titles)

# wordfreqs=colSums(df_mod)
# mydat=df_mod




# Run on preprints and articles combined ----------------------------------


# Run analysis on cites_per_day
univ_cpd <- runUnivariate(mydat = df_mod, preprint_filter=c("article","preprint"),
                          var = "cites_per_day",wordlist = wordlist,
                          family = "gaussian",wordcount_threshold = 100,
                          n = 20)
univ_cpd$journal <- "Citation rate"



# Run analysis on cites_per_day
univ_alt <- runUnivariate(mydat = df_mod,  preprint_filter=c("article","preprint"), 
                          var = "altmetrics_score",
                                  family = "gaussian",wordcount_threshold = 100,
                                  wordlist = wordlist,n = 20)
univ_alt$journal <-  "Altmetrics score"



# cites per day
univ_comb <- rbind(univ_cpd,univ_alt)
univ_comb$variable <- as.character(univ_comb$variable)
p_univ_comb <- plotUnivariate(univ_citerate_reg = univ_comb,textsize = 3)
p_univ_comb

# save
OverReact::saveREACTplot(p = p_univ_comb,figpath = figpath,
                         filename = paste0("univariate_alt_cpd_combined"),
                         width = 13,height = 7)



# add lower and upper bounds
univ_cpd$lower=univ_cpd$estimate-1.96*univ_cpd$std.error
univ_cpd$upper=univ_cpd$estimate+1.96*univ_cpd$std.error
univ_alt$lower=univ_alt$estimate-1.96*univ_alt$std.error
univ_alt$upper=univ_alt$estimate+1.96*univ_alt$std.error

# combined all
univall_comb_1=full_join(univ_cpd,univ_alt,by = c("variable"), suffix = c("_cpd", "_alt"))


# scatter of effect sizes
univ_plot_df_1= univall_comb_1 %>% 
  filter(pvalue_bonf_cpd<0.05 |pvalue_bonf_alt<0.05) %>%
  as.data.frame() %>% 
  mutate(beta_product=estimate_cpd*estimate_alt,
         beta_agree=case_when(estimate_cpd*estimate_alt>0 ~ "Betas agree",
                              T ~ "Betas disagree")) %>% 
  arrange(desc(estimate_alt)) %>% 
  # group_by(journal) %>% 
  mutate(indx=row_number())  %>% 
  arrange(desc(estimate_cpd)) %>% 
  # group_by(journal) %>% 
  mutate(indx2=row_number())  %>% 
  arrange(beta_product) %>% 
  # group_by(journal) %>% 
  mutate(indx3=row_number())  %>% 
  # ungroup() %>%
  mutate(label=case_when(indx3 <2 ~variable,
                         indx >5 & indx2 > 5 ~ NA_character_,
                         TRUE ~ variable))


# plot
p_univ_compare_1 <- univ_plot_df_1%>% 
  # filter(pvalue_bonf_cpd <=0.05 | pvalue_bonf_alt <= 0.05) %>% 
  ggplot(aes(x=estimate_alt, y= estimate_cpd, label = label)) + 
  geom_point(alpha=0.6, aes(col=beta_agree)) +
  geom_smooth(method = "lm", col="red",linewidth=0.5) +
  ggrepel::geom_text_repel(aes(col=beta_agree), size = 3.5,show.legend = F) +
  OverReact::theme_react()+
  # facet_wrap(.~journal)+
  scale_colour_manual(values = c(OverReact::imperial_colours[["imperial blue"]],OverReact::imperial_colours[["orange"]])) +
  # OverReact::scale_color_imperial(palette = "extended")+
  geom_hline(yintercept = 0,linetype="dashed",linewidth=0.5) +
  geom_vline(xintercept = 0,linetype="dashed",linewidth=0.5) +
  annotate(geom="text",x = 380,y = -0.025,label=paste0("Proportion \u03b2s agree = ", 
                                                      round(100*mean(univ_plot_df_1$beta_agree=="Betas agree"),1),"%"))+
  labs(x="\u03b2 Altmetrics",y="\u03b2 Citations / day", col="",
       title="Comparison of \u03b2 coefficients",
       subtitle = "In models regressing impact metrics (citations or Altmetrics) \nonto abstract text word frequency") +
  theme(legend.position = "bottom")
p_univ_compare_1



# save
OverReact::saveREACTplot(p = p_univ_compare_1,figpath = figpath,
                         filename = paste0("univariate_alt_cpd_compare_combined"),
                         width = 7,height = 7)



p_panel=(p_univ_comb + theme(legend.position = "bottom"))+(p_univ_compare_1 + labs(title="",subtitle="")) 
p_panel

# save panel
OverReact::saveREACTplot(p = p_panel,figpath = figpath,
                         filename = paste0("univariate_alt_cpd_panel"),
                         width = 15,height = 7)




# Univariate modelling ----------------------------------------------------



# Run analysis on cites_per_day
univ_cpd_journal <- runUnivariate(mydat = df_mod, preprint_filter=c("article") ,
                                  var = "cites_per_day",wordlist = wordlist ,
                                  family = "gaussian",wordcount_threshold = 100 ,
                                  n = 40)
univ_cpd_journal$journal <- "Journal article" 

# Run analysis on cites_per_day
univ_cpd_preprint <- runUnivariate(mydat = df_mod, preprint_filter=c("preprint"),
                                   var = "cites_per_day",wordlist = wordlist,
                                   family = "gaussian",wordcount_threshold = 10,n = 40)
univ_cpd_preprint$journal <- "Preprint"

# Run analysis on cites_per_day
univ_alt_journal <- runUnivariate(mydat = df_mod,  preprint_filter=c("article"), var = "altmetrics_score",
                                  family = "gaussian",wordcount_threshold = 100,
                                  wordlist = wordlist,n = 40)
univ_alt_journal$journal <- "Journal article"

# Run analysis on cites_per_day
univ_alt_preprint <- runUnivariate(mydat = df_mod,  preprint_filter=c("preprint"),var = "altmetrics_score",
                                   wordlist = wordlist,
                                   family = "gaussian",wordcount_threshold = 10,n = 40)
univ_alt_preprint$journal <- "Preprint"


# Run analysis on published
univ_pub_preprint <- runUnivariate(mydat = df_mod[df_mod$date <as.Date("2020-06-01"),],  preprint_filter=c("preprint"),
                                   var = "published_in_journal",wordlist = setdiff(wordlist,"preprint"),
                                   family = "binomial",wordcount_threshold = 20,n = 40)
univ_pub_preprint$journal <- "Preprint"






# Plot --------------------------------------------------------------------

# cites per day
univ_cpd_comb <- rbind(univ_cpd_journal,univ_cpd_preprint)
univ_cpd_comb$variable <- as.character(univ_cpd_comb$variable)
p_univ_cpd_comb <- plotUnivariate(univ_citerate_reg = univ_cpd_comb,textsize = 3)
p_univ_cpd_comb

# altmetrics
univ_alt_comb <- rbind(univ_alt_journal,univ_alt_preprint)
univ_alt_comb$variable <- as.character(univ_alt_comb$variable)
p_univ_alt_comb <- plotUnivariate(univ_citerate_reg = univ_alt_comb,textsize = 3)
p_univ_alt_comb

# published in journal
p_univ_pub <- plotUnivariate(univ_citerate_reg = univ_pub_preprint %>% 
                               filter(mean_outcome_val>0, variable!="published"),textsize = 3)
p_univ_pub

# save
OverReact::saveREACTplot(p = p_univ_cpd_comb,figpath = figpath,
                         filename = paste0("univariate_citerate_combined",suffix),
                         width = 13,height = 8)
# save
OverReact::saveREACTplot(p = p_univ_alt_comb,figpath = figpath,
                         filename = paste0("univariate_altmetrics_combined",suffix),
                         width = 13,height = 8)

# save
OverReact::saveREACTplot(p = p_univ_pub,figpath = figpath,
                         filename = paste0("univariate_published",suffix),
                         width = 8,height = 8)

# PLots -------------------------------------------------------------------

# add lower and upper bounds
univ_cpd_comb$lower=univ_cpd_comb$estimate-1.96*univ_cpd_comb$std.error
univ_cpd_comb$upper=univ_cpd_comb$estimate+1.96*univ_cpd_comb$std.error
univ_alt_comb$lower=univ_alt_comb$estimate-1.96*univ_alt_comb$std.error
univ_alt_comb$upper=univ_alt_comb$estimate+1.96*univ_alt_comb$std.error

# combined all
univall_comb=full_join(univ_cpd_comb,univ_alt_comb,by = c("variable","journal"), suffix = c("_cpd", "_alt"))

# scatter of effect sizes
p_univ_compare <- univall_comb %>% 
  filter(journal=="Journal article",
         pvalue_bonf_cpd<0.05 |pvalue_bonf_alt<0.05) %>%
  as.data.frame() %>% 
  arrange(desc(estimate_alt)) %>% 
  # group_by(journal) %>% 
  mutate(indx=row_number())  %>% 
  arrange(desc(estimate_cpd)) %>% 
  # group_by(journal) %>% 
  mutate(indx2=row_number())  %>% 
  # ungroup() %>%
  mutate(label=case_when(indx >5 & indx2 > 5 ~ NA_character_,
                         TRUE ~ variable),
         beta_agree=case_when(estimate_cpd*estimate_alt>0 ~ "Betas agree",
                              T ~ "Betas disagree"),
         label=case_when(indx >5 & indx2 > 5 ~ NA_character_,
                         TRUE ~ variable))%>% 
  # filter(pvalue_bonf_cpd <=0.05 | pvalue_bonf_alt <= 0.05) %>% 
  ggplot(aes(x=estimate_alt, y= estimate_cpd, label = label)) + 
  geom_point(alpha=0.6, aes(col=beta_agree)) +
  geom_smooth(method = "lm", col="red",linewidth=0.5) +
  ggrepel::geom_text_repel(aes(col=beta_agree), size = 3.5,show.legend = F) +
  OverReact::theme_react()+
  # facet_wrap(.~journal)+
  OverReact::scale_color_imperial(palette = "core")+
  geom_hline(yintercept = 0,linetype="dashed",linewidth=0.5) +
  geom_vline(xintercept = 0,linetype="dashed",linewidth=0.5) +
  labs(x="\u03b2 Altmetrics",y="\u03b2 Citations / day", col="",
       title="Comparison of \u03b2 coefficients in journals",
       subtitle = "In models regressing impact metrics (citations or Altmetrics) \nonto abstract text word frequency") +
  theme(legend.position = "bottom")
p_univ_compare


# save
OverReact::saveREACTplot(p = p_univ_compare,figpath = figpath,
                         filename = paste0("univariate_alt_cpd_compare_",suffix),
                         width = 7,height = 7)



# scatter of effect sizes
p_univ_compare_preprint <- univall_comb %>% 
  filter(journal!="Journal article",
         pvalue_bonf_cpd<0.05 |pvalue_bonf_alt<0.05) %>%
  as.data.frame() %>% 
  arrange(desc(estimate_alt)) %>% 
  # group_by(journal) %>% 
  mutate(indx=row_number())  %>% 
  arrange(desc(estimate_cpd)) %>% 
  # group_by(journal) %>% 
  mutate(indx2=row_number())  %>% 
  # ungroup() %>%
  mutate(label=case_when(indx >5 & indx2 > 5 ~ NA_character_,
                         TRUE ~ variable),
         beta_agree=case_when(estimate_cpd*estimate_alt>0 ~ "Betas agree",
                              T ~ "Betas disagree"),
         label=case_when(indx >5 & indx2 > 5 ~ NA_character_,
                         TRUE ~ variable))%>% 
  # filter(pvalue_bonf_cpd <=0.05 | pvalue_bonf_alt <= 0.05) %>% 
  ggplot(aes(x=estimate_alt, y= estimate_cpd, label = label)) + 
  geom_point(alpha=0.6, aes(col=beta_agree)) +
  geom_smooth(method = "lm", col="red",linewidth=0.5) +
  ggrepel::geom_text_repel(aes(col=beta_agree), size = 3.5,show.legend = F) +
  OverReact::theme_react()+
  # facet_wrap(.~journal)+
  OverReact::scale_color_imperial(palette = "core")+
  geom_hline(yintercept = 0,linetype="dashed",linewidth=0.5) +
  geom_vline(xintercept = 0,linetype="dashed",linewidth=0.5) +
  labs(x="\u03b2 Altmetrics",y="\u03b2 Citations / day", col="",
       title="Comparison of \u03b2 coefficients in preprints",
       subtitle = "In models regressing impact metrics (citations or Altmetrics) \nonto abstract text word frequency") +
  theme(legend.position = "bottom")
p_univ_compare_preprint


# save
OverReact::saveREACTplot(p = p_univ_compare_preprint,figpath = figpath,
                         filename = paste0("univariate_alt_cpd_compare_preprint_",suffix),
                         width = 7,height = 7)
