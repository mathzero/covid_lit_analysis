
#' First clear the environment of variables
rm(list=ls(all=TRUE))

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
createMySubfolder(subfolderName = "univariate_topics")
suffix="topics"
# no scientific notation
options(scipen = 999)

# load data
dat <- readRDS("data/bq_exports_mar_2023/clean/dat_main.rds")
topic_model=readRDS("data/topic_model_k_140_all_abstract_10000.rds")

# get gamma
td_gamma <- tidy(topic_model, matrix = "gamma",
                 document_names = rownames(df_text_sparse))
length(unique(td_gamma$document))


# get wide td_gamma for modelling
td_gamma_wide=td_gamma %>% pivot_wider(id_cols = document,
                                       values_from = gamma,
                                       names_from = topic)


# get beta
td_beta <- tidy(topic_model)
td_beta
top_terms <- td_beta %>%
  arrange(beta) %>%
  dplyr::group_by(topic) %>%
  top_n(4, beta) %>%
  arrange(-beta) %>%
  dplyr::select(topic, term) %>%
  summarise(terms = list(term)) %>%
  mutate(terms = map(terms, paste, collapse = ", ")) %>% 
  unnest()
top_terms

gamma_terms <- td_gamma %>%
  group_by(topic) %>%
  summarise(gamma = mean(gamma)) %>%
  # arrange(desc(gamma)) %>%
  left_join(top_terms, by = "topic") %>%
  mutate(topic = paste0("Topic ", topic),
         topic = reorder(topic, gamma))

# create modelling df
wordlist=topic_names=paste0(gamma_terms$topic,": ",gamma_terms$terms)
names(td_gamma_wide)[2:ncol(td_gamma_wide)] <- topic_names
df_mod <- dat %>% inner_join(td_gamma_wide, by=c("id"="document"))
df_mod$article_type <- df_mod$type


# Run analyses ------------------------------------------------------------

# Run analysis on cites_per_day
univ_cpd_journal <- runUnivariate(mydat = df_mod, preprint_filter=c("article"),
                                  var = "cites_per_day",wordlist = wordlist,
                                  family = "gaussian",wordcount_threshold = 0,
                                  n = 30)
univ_cpd_journal$journal <- "Journal article"

# Run analysis on cites_per_day
univ_cpd_preprint <- runUnivariate(mydat = df_mod, preprint_filter=c("preprint"),
                                   var = "cites_per_day",wordlist = wordlist,
                                   family = "gaussian",wordcount_threshold = 0,n = 30)
univ_cpd_preprint$journal <- "Preprint"

# Run analysis on cites_per_day
univ_alt_journal <- runUnivariate(mydat = df_mod,  preprint_filter=c("article"), var = "altmetrics_score",
                                  family = "gaussian",wordcount_threshold = 0,
                                  wordlist = wordlist,n = 30)
univ_alt_journal$journal <- "Journal article"

# Run analysis on cites_per_day
univ_alt_preprint <- runUnivariate(mydat = df_mod,  preprint_filter=c("preprint"),var = "altmetrics_score",
                                   wordlist = wordlist,
                                   family = "gaussian",wordcount_threshold = 0,n = 30)
univ_alt_preprint$journal <- "Preprint"


# Run analysis on published
univ_pub_preprint <- runUnivariate(mydat = df_mod[df_mod$date <as.Date("2020-06-01"),],  
                                   preprint_filter=c("preprint"),
                                   var = "published_in_journal",
                                   wordlist = setdiff(wordlist,"preprint"),
                                   family = "binomial",wordcount_threshold = 0,n = 30)
univ_pub_preprint$journal <- "Preprint"
table(df_mod$date)

# Plot --------------------------------------------------------------------

# cites per day
univ_cpd_comb <- rbind(univ_cpd_journal,univ_cpd_preprint)
univ_cpd_comb$variable <- as.character(univ_cpd_comb$variable)
p_univ_cpd_comb <- plotUnivariate(univ_citerate_reg = univ_cpd_comb,textsize = 3,wraplength=15)
p_univ_cpd_comb

# altmetrics
univ_alt_comb <- rbind(univ_alt_journal,univ_alt_preprint)
univ_alt_comb$variable <- as.character(univ_alt_comb$variable)
p_univ_alt_comb <- plotUnivariate(univ_citerate_reg = univ_alt_comb,textsize = 3,wraplength=15)
p_univ_alt_comb

# published in journal
p_univ_pub <- plotUnivariate(univ_citerate_reg = univ_pub_preprint %>% 
                               filter(mean_outcome_val>0, variable!="published"),
                             textsize = 3,wraplength=15)
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
  mutate(label=case_when(indx >3 & indx2 > 3 ~ NA_character_,
                         TRUE ~ variable),
         beta_agree=case_when(estimate_cpd*estimate_alt>0 ~ "Betas agree",
                              T ~ "Betas disagree"),
         label=case_when(indx >5 & indx2 > 5 ~ NA_character_,
                         TRUE ~ variable))%>% 
  # filter(pvalue_bonf_cpd <=0.05 | pvalue_bonf_alt <= 0.05) %>% 
  ggplot(aes(x=estimate_alt, y= estimate_cpd, label = stringr::str_wrap(label,18))) + 
  geom_point(alpha=0.6, aes(col=beta_agree)) +
  geom_smooth(method = "lm", col="red",linewidth=0.5) +
  ggrepel::geom_text_repel(aes(col=beta_agree), size = 2.5,show.legend = F) +
  OverReact::theme_react()+
  # facet_wrap(.~journal)+
  OverReact::scale_color_imperial(palette = "core")+
  geom_hline(yintercept = 0,linetype="dashed",linewidth=0.5) +
  geom_vline(xintercept = 0,linetype="dashed",linewidth=0.5) +
  labs(x="\u03b2 Altmetrics",y="\u03b2 Citations / day", col="",
       title="Comparison of \u03b2 coefficients in journals",
       subtitle = "In models regressing impact metrics (citations or Altmetrics) \nonto abstract topic \u03b3") +
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
  ggplot(aes(x=estimate_alt, y= estimate_cpd, label = stringr::str_wrap(label,18))) + 
  geom_point(alpha=0.6, aes(col=beta_agree)) +
  geom_smooth(method = "lm", col="red",linewidth=0.5) +
  ggrepel::geom_text_repel(aes(col=beta_agree), size = 2.5,show.legend = F) +
  OverReact::theme_react()+
  # facet_wrap(.~journal)+
  OverReact::scale_color_imperial(palette = "core")+
  geom_hline(yintercept = 0,linetype="dashed",linewidth=0.5) +
  geom_vline(xintercept = 0,linetype="dashed",linewidth=0.5) +
  labs(x="\u03b2 Altmetrics",y="\u03b2 Citations / day", col="",
       title="Comparison of \u03b2 coefficients in preprints",
       subtitle = "In models regressing impact metrics (citations or Altmetrics) \nonto abstract topic \u03b3") +
  theme(legend.position = "bottom")
p_univ_compare_preprint


# save
OverReact::saveREACTplot(p = p_univ_compare_preprint,figpath = figpath,
                         filename = paste0("univariate_alt_cpd_compare_preprint_",suffix),
                         width = 7,height = 7)






# Updated -----------------------------------------------------------------





# Article and preprint combined -------------------------------------------

# Run analysis on cites_per_day
univ_cpd <- runUnivariate(mydat = df_mod, preprint_filter=c("article","preprint"),
                                  var = "cites_per_day",wordlist = wordlist,
                                  family = "gaussian",wordcount_threshold = 0,
                                  n = 30)
univ_cpd$journal <- "Citations per day"


# Run analysis on cites_per_day
univ_alt <- runUnivariate(mydat = df_mod,  preprint_filter=c("article","preprint"), 
                                  var = "altmetrics_score",
                                  family = "gaussian",wordcount_threshold = 0,
                                  wordlist = wordlist,n = 30)
univ_alt$journal <- "Altmetrics score"



# cites per day
univ_comb <- rbind(univ_cpd,univ_alt)
univ_comb$variable <- as.character(univ_comb$variable)
p_univ_comb <- plotUnivariate(univ_citerate_reg = univ_comb,textsize = 3,wraplength=15) +
  theme(legend.position = "bottom")




# combined all
univall_comb_1=full_join(univ_cpd,univ_alt,by = c("variable"), suffix = c("_cpd", "_alt"))


# scatter of effect sizes
univ_plot_df_1= univall_comb_1 %>% 
  filter(pvalue_bonf_cpd<0.05  & pvalue_bonf_alt<0.05) %>%
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
  mutate(label=case_when(indx3 <10 ~variable,
                         indx >5 & indx2 > 5 ~ NA_character_,
                         TRUE ~ variable))


# plot
p_univ_compare_1 <- univ_plot_df_1%>% 
  # filter(pvalue_bonf_cpd <=0.05 | pvalue_bonf_alt <= 0.05) %>% 
  ggplot(aes(x=estimate_alt, y= estimate_cpd, label = label)) + 
  geom_point(alpha=0.6, aes(col=beta_agree)) +
  # geom_smooth(method = "lm", col="red",linewidth=0.5) +
  ggrepel::geom_text_repel(aes(col=beta_agree), size = 2.5,show.legend = F) +
  OverReact::theme_react()+
  # facet_wrap(.~journal)+
  scale_colour_manual(values = c(OverReact::imperial_colours[["imperial blue"]],OverReact::imperial_colours[["orange"]])) +
  # OverReact::scale_color_imperial(palette = "extended")+
  geom_hline(yintercept = 0,linetype="dashed",linewidth=0.5) +
  geom_vline(xintercept = 0,linetype="dashed",linewidth=0.5) +
  
  
  annotate(geom="text",x = 1180,y = -0.225,hjust=0,
           label=paste0("N significant \u03b2s =",nrow(univ_plot_df_1),
    "\nProportion \u03b2s agree = ", 
                                                        round(100*mean(univ_plot_df_1$beta_agree=="Betas agree"),1),"%"))+
  labs(x="\u03b2 Altmetrics",y="\u03b2 Citations / day", col="",
       title="Comparison of \u03b2 coefficients",
       subtitle = "In models regressing impact metrics (citations or Altmetrics) \nonto abstract text topic \u03b3") +
  theme(legend.position = "bottom")
p_univ_compare_1


# save
OverReact::saveREACTplot(p = p_univ_compare_1,figpath = figpath,
                         filename = paste0("univariate_alt_cpd_compare_combined"),
                         width = 8,height = 8)




# Combine plots -----------------------------------------------------------

p_panel=p_univ_comb+(p_univ_compare_1 + labs(title = element_blank(), subtitle = element_blank())) +
  plot_layout(widths = c(5,4))
p_panel


# save
OverReact::saveREACTplot(p = p_panel,figpath = figpath,
                         filename = paste0("univariate_alt_cpd_volcano_compare_panel"),
                         width = 16,height = 8)






# Article and preprint combined wave 1-------------------------------------------

# Run analysis on cites_per_day
univ_cpd_wave1 <- runUnivariate(mydat = df_mod %>% filter(year==2020), preprint_filter=c("article","preprint"),
                          var = "cites_per_day",wordlist = wordlist,
                          family = "gaussian",wordcount_threshold = 0,
                          n = 30)
univ_cpd_wave1$journal <- "Citations per day"


# Run analysis on cites_per_day
univ_alt_wave1 <- runUnivariate(mydat = df_mod %>% filter(year==2020),  preprint_filter=c("article","preprint"), 
                          var = "altmetrics_score",
                          family = "gaussian",wordcount_threshold = 0,
                          wordlist = wordlist,n = 30)
univ_alt_wave1$journal <- "Altmetrics score"



# cites per day
univ_comb_wave1 <- rbind(univ_cpd_wave1,univ_alt_wave1)
univ_comb_wave1$variable <- as.character(univ_comb_wave1$variable)
p_univ_comb_wave1 <- plotUnivariate(univ_citerate_reg = univ_comb_wave1,
                                    textsize = 3,wraplength=15) +
  theme(legend.position = "bottom")




# combined all
univall_comb_1_wave1=full_join(univ_cpd_wave1,univ_alt_wave1,by = c("variable"), suffix = c("_cpd", "_alt"))


# scatter of effect sizes
univ_plot_df_1_wave1= univall_comb_1_wave1 %>% 
  filter(pvalue_bonf_cpd<0.05  & pvalue_bonf_alt<0.05) %>%
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
  mutate(label=case_when(indx3 <10 ~variable,
                         indx >5 & indx2 > 5 ~ NA_character_,
                         TRUE ~ variable))


# plot
p_univ_compare_wave_1 <- univ_plot_df_1_wave1%>% 
  # filter(pvalue_bonf_cpd <=0.05 | pvalue_bonf_alt <= 0.05) %>% 
  ggplot(aes(x=estimate_alt, y= estimate_cpd, label = label)) + 
  geom_point(alpha=0.6, aes(col=beta_agree)) +
  # geom_smooth(method = "lm", col="red",linewidth=0.5) +
  ggrepel::geom_text_repel(aes(col=beta_agree), size = 2.5,show.legend = F) +
  OverReact::theme_react()+
  # facet_wrap(.~journal)+
  scale_colour_manual(values = c(OverReact::imperial_colours[["imperial blue"]],OverReact::imperial_colours[["orange"]])) +
  # OverReact::scale_color_imperial(palette = "extended")+
  geom_hline(yintercept = 0,linetype="dashed",linewidth=0.5) +
  geom_vline(xintercept = 0,linetype="dashed",linewidth=0.5) +
  
  
  annotate(geom="text",x = 1180,y = -0.485,hjust=0,
           label=paste0("N significant \u03b2s =",nrow(univ_plot_df_1),
                        "\nProportion \u03b2s agree = ", 
                        round(100*mean(univ_plot_df_1$beta_agree=="Betas agree"),1),"%"))+
  labs(x="\u03b2 Altmetrics",y="\u03b2 Citations / day", col="",
       title="Comparison of \u03b2 coefficients",
       subtitle = "In models regressing impact metrics (citations or Altmetrics) \nonto abstract text topic \u03b3") +
  theme(legend.position = "bottom")
p_univ_compare_wave_1


# save
OverReact::saveREACTplot(p = p_univ_compare_wave_1,figpath = figpath,
                         filename = paste0("univariate_alt_cpd_compare_combined_wave_1"),
                         width = 8,height = 8)




# Combine plots -----------------------------------------------------------

p_panel_wave1=p_univ_comb_wave1+(p_univ_compare_wave_1 + labs(title = element_blank(), subtitle = element_blank())) +
  plot_layout(widths = c(5,4))
p_panel_wave1


# save
OverReact::saveREACTplot(p = p_panel_wave1,figpath = figpath,
                         filename = paste0("univariate_alt_cpd_volcano_compare_panel_wave_1"),
                         width = 16,height = 8)


