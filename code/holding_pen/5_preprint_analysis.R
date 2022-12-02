

#' First clear the environment of variables
rm(list=ls(all=TRUE))

outpath <- paste0(getwd(),"/output/")
figpath <- paste0(getwd(),"/plots/")
datapath <- paste0(getwd(),"/data/")
function_script_path ="E:/home/mw418/function_scripts/forest_plot.R"
function_script_path="/Users/mathzero/Library/CloudStorage/GoogleDrive-mattdwhitaker@gmail.com/My Drive/Imperial/PhD/REACT2_analysis_in_progress/RESULTS/function_scripts/"


#' Source any functions from the local file
source(paste0(function_script_path,"/load_packages.R"))
source(paste0(function_script_path,"/cats_and_covs.R"))
source(paste0(function_script_path,"/create_subfolder.R"))
source(paste0(function_script_path,"/stability_selection.R"))
source(paste0(function_script_path,"/forest_plot.R"))
source(paste0(function_script_path,"/save_styled_table.R"))

source("code/0_functions.R")

# 
# source("E:/Group/react2_study5/report_phases_combined/projects/function_scripts/create_subfolder.R")
# update.packages("Rcpp",repos="http://se-r.sm.med.ic.ac.uk", dependencies = TRUE )

#' Pull in packages needed
package.list <- c("knitr","dplyr","factoextra","tidyr","ggbeeswarm",
                  "ggplot2","gdata","ggsci", "RColorBrewer", "tidyverse", "lubridate", 
                  "ComplexHeatmap",
                  "readr","ggthemes", "gridExtra","qdapDictionaries","sharp",
                  "geometry","Rcpp","furrr","future","caret","parallel",
                  "doParallel","ggpubr","jtools",
                  "patchwork", "OverReact")
load_packages(package.list)


# create subfolder
createMySubfolder(subfolderName = "preprint_analysis")

# no scientific notation
options(scipen = 999)

# Load data ---------------------------------------------------------------

dat <- readRDS("data/dat_main.rds")


topic_model <- readRDS("data/topic_model_k_70_all_abstract_10000.rds")
ntopic=70

# get beta
td_beta <- tidytext::tidy(topic_model)
td_beta

# get wide gamma
td_beta_wide=td_beta %>% pivot_wider(id_cols = topic,
                                       names_prefix = "",names_from = term,values_from = beta)


# get gamma
td_gamma <- tidytext::tidy(topic_model, matrix = "gamma",
                 document_names = rownames(df_text_sparse))

td_gamma

# get wide gamma
td_gamma_wide=td_gamma %>% pivot_wider(id_cols = document,
                                       names_prefix = "topic_",
                                       names_from = topic,
                                       values_from = gamma)

# 
# # get principal components of Gamma scores
# td_gamma_wide_pca=prcomp(td_gamma_wide[,2:71])
# pca_var_perc=round(100*(td_gamma_wide_pca$sdev^2)/sum(td_gamma_wide_pca$sdev^2),2)
# barplot(pca_var_perc, main="Variance explained by principal components of Gamma matrix",
#         xlab="PCs",ylab="% variance")



# Get df of top terms by topic
top_terms <- td_beta %>%
  arrange(beta) %>%
  dplyr::group_by(topic) %>%
  top_n(10, beta) %>%
  arrange(-beta) %>%
  dplyr::select(topic, term) %>%
  summarise(terms = list(term)) %>%
  mutate(terms = map(terms, paste, collapse = ", ")) %>% 
  unnest()
top_terms

# 
gamma_terms <- td_gamma %>%
  group_by(topic) %>%
  summarise(gamma = mean(gamma)) %>%
  arrange(desc(gamma)) %>%
  left_join(top_terms, by = "topic") %>%
  mutate(topic = paste0("Topic ", topic),
         topic = reorder(topic, gamma))

# Plot terms by topic
topics_plot=gamma_terms %>%
  ggplot(aes(topic, gamma, label = terms, fill = gamma)) +
  geom_col(show.legend = FALSE) +
  geom_text(hjust = 0, nudge_y = 0.0002, size = 3) +
  coord_flip() +
  scale_y_continuous(expand = c(0,0),
                     limits = c(0.00, 0.022),
                     labels = scales::percent_format()) +
  OverReact::theme_react() +
  theme(plot.title = element_text(size = 16,
                                  family="IBMPlexSans-Bold"),
        plot.subtitle = element_text(size = 13)) +
  labs(x = NULL, y = expression(gamma),
       title = "Topics by prevalence in published COVID-19 preprint abstracts",
       subtitle = "With the top 10 words that contribute to each topic")

# view plot
topics_plot

# save
OverReact::saveREACTplot(p = topics_plot,figpath = figpath,filename = "topic_gamma_plot",
                         width = 12,height = 16,savePDF = F)




# Plot terms by topic
topics_plot_top10=gamma_terms %>%
  slice_head(n=10) %>% 
  ggplot(aes(topic, gamma, label = terms, fill = gamma)) +
  geom_col(show.legend = FALSE) +
  geom_text(hjust = 1, nudge_y = -0.0002, size = 3, col="white") +
  coord_flip() +
  scale_y_continuous(expand = c(0,0),
                     limits = c(0.00, 0.022),
                     labels = scales::percent_format()) +
  OverReact::theme_react() +
  theme(plot.title = element_text(size = 16,
                                  family="IBMPlexSans-Bold"),
        plot.subtitle = element_text(size = 13)) +
  labs(x = NULL, y = expression(gamma),
       title = "Topics by prevalence in published COVID-19 preprint abstracts",
       subtitle = "With the top 10 words that contribute to each topic")

# view plot
topics_plot_top10

# save
OverReact::saveREACTplot(p = topics_plot_top10,figpath = figpath,filename = "topic_gamma_plot_top10",
                         width = 12,height = 5,savePDF = T)

# Table one of preprints --------------------------------------------------

dat <- dat%>% mutate(abstract_available = case_when(is.na(abstract_preferred) ~ "Yes",
                                                 T ~ "No"),
                  dummy="nobs",
                  year_factor=as.factor(year)) %>% 
  mutate(across(c("citations_count","metrics_recent_citations","altmetrics_score","cited_by_tweeters_count","cited_by_msm_count"),
                ~replace_na(.x,0)))
rowvar_list=c("dummy","year_factor","journal_title","citations_count","metrics_recent_citations",
              "altmetrics_score","cited_by_tweeters_count","cited_by_msm_count")

# cov_names=as.list(rowvar_list)
cov_names=list("Papers published","Year","Server","Number of citations","Recent citations",
               "Altmetrics score","Number of tweets","Number of media mentions")
names(cov_names)=rowvar_list
tab1 <- OverReact::crossTabMulti(dat=dat %>% filter(preprint=="Preprint"),rowvar_list = rowvar_list,colvar = "published",
                                 cov_names = cov_names,include_percentages = T,
                                 rowwise_precentages = T,confint = F,
                                 comma_thousands = T,statistical_test = F)
# reorder servers by magnitude
ordervec=order(as.numeric(gsub(",","",tab1$`Sum / mean(SD)`)[tab1$Variable=="Server"]),decreasing = T)
tab1[tab1$Variable=="Server",] <- tab1[tab1$Variable=="Server",][ordervec,]

# new names
names(tab1)[c(3,4)] <- c("Unpublished","Published")

# remove the rare servers
removevec<- !(as.numeric(gsub(",","",tab1$`Sum / mean(SD)`))<50)
removevec[is.na(removevec)] <- T
tab1=tab1[removevec,]

OverReact::saveREACTtable(tab = tab1,outpath = outpath,filename = "table_one_preprints")
savePrettyExcelWorkbook(listOfTables = list(table_one_preprints=tab1),workbookName = "table_one_preprints",outpath = outpath)


# Univariate  modelling by topic ------------------------------------------

dat_mod <- dat %>% left_join(td_gamma_wide, by =c("unique_id"="document")) %>% 
  rename(date_published=date,
         published_in_journal=published
  ) %>% 
  rename(article_type=preprint) %>% 
  filter(grepl("rxiv",tolower(journal_title))) # remove all the proprietary preprint servers


# Run analysis on cites_per_day
univ_cpd_preprint <- runUnivariate(mydat = dat_mod%>% filter(date_published<="2021-01-01"), preprint_filter=c("Preprint"),
                                   var = "cites_per_day",wordlist = paste0("topic_",1:ntopic),
                                   family = "gaussian",wordcount_threshold = 10,n = 10)
univ_cpd_preprint$journal <- "Citations per day"


# Run analysis on altmetrics
univ_alt_preprint <- runUnivariate(mydat = dat_mod%>% filter(date_published<="2021-01-01"),  preprint_filter=c("Preprint"),var = "altmetrics_score",
                                   wordlist = paste0("topic_",1:ntopic),
                                   family = "gaussian",wordcount_threshold = 10,n = 10)
univ_alt_preprint$journal <- "Altmetrics score"



# Run analysis on published
univ_pub_preprint <- runUnivariate(mydat = dat_mod %>% filter(date_published<="2021-01-01"),  preprint_filter=c("Preprint"),
                                   var = "published_in_journal",wordlist = paste0("topic_",1:ntopic),
                                   family = "binomial",wordcount_threshold = 20,n = 10)
univ_pub_preprint$journal <- "Published in journal"


# Run analysis on h-index of journal published
univ_pub_hindex_preprint <- runUnivariate(mydat = dat_mod %>% filter(date_published<="2021-01-01", published_in_journal==1),
                                   preprint_filter=c("Preprint"),
                                   var = "subs_pub_h_index",wordlist = paste0("topic_",1:ntopic),
                                   family = "gaussian",wordcount_threshold = 20,n = 10)
univ_pub_hindex_preprint$journal <- "Publishing journal H-Index"

# Run analysis on IF of journal published
univ_pub_if_preprint <- runUnivariate(mydat = dat_mod %>% filter(date_published<="2021-01-01", published_in_journal==1),
                                          preprint_filter=c("Preprint"),
                                          var = "subs_pub_if_2022",wordlist = paste0("topic_",1:ntopic),
                                          family = "gaussian",wordcount_threshold = 20,n = 10)
univ_pub_if_preprint$journal <- "Publishing journal Impact Factor"


# Plot
univ_comb <- rbind(univ_cpd_preprint,univ_alt_preprint,univ_pub_preprint,univ_pub_if_preprint)
univ_comb$variable <- as.character(univ_comb$variable)
top_terms$variable=paste0("topic_",top_terms$topic)
univ_comb <- univ_comb %>% left_join(top_terms)
univ_comb
p_univ  <- univ_comb  %>% 
  arrange((p.value)) %>% 
  group_by(journal) %>% 
  mutate(indx=row_number())  %>% 
  ungroup() %>%
  mutate(label=case_when(indx >30 ~ NA_character_,
                         indx <8 ~ paste0(topic,": ",terms),
                         TRUE ~ as.character(topic)),
         sig=factor(case_when(pvalue_bonf <=0.05 ~ "FDR<0.05",
                              p.value <=0.05 ~ "p<0.05",
                              T ~"Not significant"), 
                    levels = c("Not significant","p<0.05","FDR<0.05")))%>% 
  ggplot(aes(x=(estimate),y=-log10(p.value), col = sig, label=str_wrap(string = label,width = 20))) +
  geom_point() +
  ggrepel::geom_text_repel(col = "black", size = 1.7, lineheight=0.8) +
  theme_adjust+
  theme_bw() +
  {if("journal"%in%colnames(univ_comb))
    facet_wrap(.~journal, scales="free_x",nrow=1)
  } +
  # OverReact::scale_color_imperial(palette = "warm", reverse = T) +
  scale_color_manual(values = c("grey60", "orange", "firebrick3")) +
  labs(x= "Beta coefficient", y="-log10 p-value", col = "") +
  theme_adjust
p_univ

OverReact::saveREACTplot(p = p_univ,figpath = figpath,filename = "topic_univariate",
                         width = 16,height = 8,savePDF = T)



### Published in journal only

# Plot
univ_comb_2 <- rbind(univ_pub_preprint,univ_pub_if_preprint)
univ_comb_2$variable <- as.character(univ_comb_2$variable)
top_terms$variable=paste0("topic_",top_terms$topic)
univ_comb_2 <- univ_comb_2 %>% left_join(top_terms)
univ_comb_2
p_univ_2  <- univ_comb_2  %>% 
  arrange((p.value)) %>% 
  group_by(journal) %>% 
  mutate(indx=row_number())  %>% 
  ungroup() %>%
  mutate(label=case_when(indx >30 ~ NA_character_,
                         indx <8 ~ paste0(topic,": ",terms),
                         TRUE ~ as.character(topic)),
         sig=factor(case_when(pvalue_bonf <=0.05 ~ "FDR<0.05",
                              p.value <=0.05 ~ "p<0.05",
                              T ~"Not significant"), 
                    levels = c("Not significant","p<0.05","FDR<0.05")))%>% 
  ggplot(aes(x=(estimate),y=-log10(p.value), col = sig, label=str_wrap(string = label,width = 20))) +
  geom_point() +
  ggrepel::geom_text_repel(col = "black", size = 1.7, lineheight=0.8) +
  theme_adjust+
  theme_bw() +
  {if("journal"%in%colnames(univ_comb_2))
    facet_wrap(.~journal, scales="free_x",nrow=1)
  } +
  # OverReact::scale_color_imperial(palette = "warm", reverse = T) +
  scale_color_manual(values = c("grey60", "orange", "firebrick3")) +
  labs(x= "Beta coefficient", y="-log10 p-value", col = "") +
  theme_adjust
p_univ_2

OverReact::saveREACTplot(p = p_univ_2,figpath = figpath,filename = "topic_univariate_published_only",
                         width = 8,height = 6,savePDF = T)




# Compare effects ---------------------------------------------------------


### Altmetrics vs cites per day


# combined all
univall_comb=full_join(univ_cpd_preprint,univ_alt_preprint,by = "variable", 
                       suffix = c("_cpd", "_alt"))
univall_comb$topic=1:ntopic
formula=y~x

# scatter of effect sizes
p_univ_compare <- univall_comb %>% 
  mutate(agreement=case_when(estimate_cpd*estimate_alt>=0 ~ "Yes",
                             T~"No")) %>% 
  ggplot(aes(x=estimate_alt, y= estimate_cpd, label = topic, col=agreement)) + 
  geom_text(size=3)+
  OverReact::theme_react() +
  OverReact::scale_color_imperial(palette = "two_col_grey_orng", reverse = F) +
  geom_hline(yintercept = 0,linetype="dashed", size=0.5)+
  geom_vline(xintercept = 0,linetype="dashed", size=0.5)+
  geom_smooth(aes(x=estimate_alt, y= estimate_cpd),
              method = "lm",formula=formula,inherit.aes = F, 
              col = OverReact::imperial_colours[[6]], size=0.3) +
  stat_cor(aes(x=estimate_alt, y= estimate_cpd),inherit.aes = F,label.y=0.08,
           p.accuracy = 0.00001,r.accuracy = 0.001,
           size=3)  +
  labs(x="\u03B2 Altmetrics score",y="\u03B2 Citations per day") +
  theme(legend.position = "none")

p_univ_compare


OverReact::saveREACTplot(p = p_univ_compare,figpath = figpath,
                         filename = "univariate_betas_cpd_alt_scatter",
                         width = 5,height = 5,savePDF = F)





### Altmetrics vs publishing in journal

# combined all
univall_comb_2=full_join(univ_pub_preprint,univ_alt_preprint,by = "variable", 
                         suffix = c("_pub", "_alt"))
univall_comb_2$topic=1:ntopic
formula=y~x

# scatter of effect sizes
p_univ_compare_2 <- univall_comb_2 %>% 
  mutate(agreement=case_when(estimate_pub*estimate_alt>=0 ~ "Yes",
                             T~"No")) %>% 
  ggplot(aes(x=estimate_alt, y= estimate_pub, label = topic, col=agreement)) + 
  geom_text(size=3)+
  OverReact::theme_react() +
  OverReact::scale_color_imperial(palette = "two_col_grey_orng", reverse = F) +
  geom_hline(yintercept = 0,linetype="dashed", size=0.5)+
  geom_vline(xintercept = 0,linetype="dashed", size=0.5)+
  geom_smooth(aes(x=estimate_alt, y= estimate_pub),
              method = "lm",formula=formula,inherit.aes = F, 
              col = OverReact::imperial_colours[[6]], size=0.3) +
  stat_cor(aes(x=estimate_alt, y= estimate_pub),inherit.aes = F,label.y=8,
           p.accuracy = 0.00001,r.accuracy = 0.001,size=3)  +
  labs(x="\u03B2 Altmetrics score",y="\u03B2 Published in journal") +
  theme(legend.position = "none")

p_univ_compare_2


OverReact::saveREACTplot(p = p_univ_compare_2,figpath = figpath,
                         filename = "univariate_betas_pub_alt_scatter",
                         width = 5,height = 5,savePDF = F)



### CPD vs publishing in journal

# combined all
univall_comb_3=full_join(univ_pub_preprint,univ_cpd_preprint,by = "variable", 
                         suffix = c("_pub", "_cpd"))
univall_comb_3$topic=1:ntopic
formula=y~x

# scatter of effect sizes
p_univ_compare_3 <- univall_comb_3 %>% 
  mutate(agreement=case_when(estimate_pub*estimate_cpd>=0 ~ "Yes",
                             T~"No")) %>% 
  ggplot(aes(x=estimate_cpd, y= (estimate_pub), label = topic, col=agreement)) + 
  geom_text(size=3)+
  OverReact::theme_react() +
  OverReact::scale_color_imperial(palette = "two_col_grey_orng", reverse = F) +
  geom_hline(yintercept = 0,linetype="dashed", size=0.5)+
  geom_vline(xintercept = 0,linetype="dashed", size=0.5)+
  geom_smooth(aes(x=estimate_cpd, y= estimate_pub),
              method = "lm",formula=formula,inherit.aes = F, 
              col = OverReact::imperial_colours[[6]], size=0.3) +
  stat_cor(aes(x=estimate_cpd, y= estimate_pub),inherit.aes = F,label.y=10,
           p.accuracy = 0.00001,r.accuracy = 0.001,size=3)  +
  labs(x="\u03B2 Citations per day",y="\u03B2 Published in journal") +
  theme(legend.position = "none")

p_univ_compare_3


OverReact::saveREACTplot(p = p_univ_compare_3,figpath = figpath,
                         filename = "univariate_betas_pub_cpd_scatter",
                         width = 5,height = 5,savePDF = F)



### Publishing in journal vs publishing journal IF

# combined all
univall_comb_3=full_join(univ_pub_preprint,univ_pub_if_preprint,by = "variable", 
                         suffix = c("_pub", "_if"))
univall_comb_3$topic=1:ntopic
formula=y~x

# scatter of effect sizes
p_univ_compare_4 <- univall_comb_3 %>% 
  mutate(agreement=case_when(estimate_pub*estimate_if>=0 ~ "Yes",
                             T~"No")) %>% 
  ggplot(aes(x=estimate_pub, y= (estimate_if), label = topic, col=agreement)) + 
  geom_text(size=3)+
  OverReact::theme_react() +
  OverReact::scale_color_imperial(palette = "two_col_grey_orng", reverse = F) +
  geom_hline(yintercept = 0,linetype="dashed", size=0.5)+
  geom_vline(xintercept = 0,linetype="dashed", size=0.5)+
  geom_smooth(aes(x=estimate_pub, y= estimate_if),
              method = "lm",formula=formula,inherit.aes = F, 
              col = OverReact::imperial_colours[[6]], size=0.3) +
  stat_cor(aes(x=estimate_pub, y= estimate_if),inherit.aes = F,label.y=100,
           p.accuracy = 0.00001,r.accuracy = 0.001,size=3)  +
  labs(y="\u03B2 Impact factor of publishing journal",x="\u03B2 Published in journal") +
  theme(legend.position = "none")

p_univ_compare_4


OverReact::saveREACTplot(p = p_univ_compare_4,figpath = figpath,
                         filename = "univariate_betas_pub_pub_if_scatter",
                         width = 5,height = 5,savePDF = F)



# Simple regression model -------------------------------------------------
dat_mod$cohorts_doc %>% table()
dat_mod$cited_by_msm_count

df_mod=dat_mod %>% filter(article_type=="Preprint") %>% 
  dplyr::select(altmetrics_score,cites_per_day,
                cohorts_com,cohorts_sci,cohorts_pub,cohorts_doc,cited_by_msm_count,
                published_in_journal
  ) %>% mutate(altmetrics_score=replace_na(altmetrics_score,0),
               cites_per_day=replace_na(cites_per_day,0),
               cohorts_com=replace_na(cohorts_com,0),
               cohorts_sci=replace_na(cohorts_sci,0),
               cohorts_pub=replace_na(cohorts_pub,0),
               cohorts_doc=replace_na(cohorts_doc,0),
               cited_by_msm_count=replace_na(cited_by_msm_count,0))
df_mod[,1:(ncol(df_mod)-1)] <- scale(df_mod[,1:(ncol(df_mod)-1)])
df_mod <- df_mod[complete.cases(df_mod),]
f <- as.formula("published_in_journal ~ cites_per_day+cohorts_com+cohorts_pub+cohorts_sci+cohorts_doc+cited_by_msm_count")
glm_mod <- glm(formula = f,family = "binomial",data =df_mod )
jtools::summ(glm_mod,exp=T)
myvars=c("cites_per_day","cohorts_com","cohorts_pub","cohorts_sci","cohorts_doc","cited_by_msm_count")

cov_name_list[myvars] <- c("Citations per day","Attention from science communicators",
                           "Attention from general public",
                           "Attention from scientists",
                           "Attention from medics",
                           "Coverage by mainstream media"
                           
)


# scale variables
df_mod[,myvars] <- as.data.frame(lapply(df_mod[,myvars], scale))

mods_published=OverReact::ModelMakerMulti(dat = df_mod,
                                          list_of_variables_of_interest = myvars,
                                          outcome = "published_in_journal",
                                          sf = 3,simpleround = T,
                                          remove_intercept_from_results = T,
                                          joint_adjustment_vars = myvars,
                                          cov_name_list = cov_name_list)
mods_res=mods_published$df_output %>% filter(!grepl("Inter",Category))
saveREACTtable(tab =mods_res,outpath = outpath,filename = "ors_pub_in_journal")

# ~ Model of IF of subseqent publication
mods_impact_subs_journal=OverReact::ModelMakerMulti(dat = dat %>% 
                                                      filter(published==1,grepl("rxiv",tolower(journal_title)),
                                                             preprint=="Preprint"),
                                                    list_of_variables_of_interest = myvars,
                                                    outcome = "subs_pub_h_index",
                                                    sf = 3,simpleround = T,
                                                    remove_intercept_from_results = T,
                                                    joint_adjustment_vars = myvars,
                                                    cov_name_list = cov_name_list)
mods_impact_subs_journal_res=mods_impact_subs_journal$df_output %>% filter(!grepl("Inter",Category))
saveREACTtable(tab =mods_impact_subs_journal_res,outpath = outpath,filename = "betas_publishing_journal_hindex")



# xval predictors ---------------------------------------------------------








# 
# 
# # Stability selection -----------------------------------------------------
# 
# dat_mod$cohorts_com <- replace_na(data = dat_mod$cohorts_com,0)
# dat_mod$cohorts_doc <- replace_na(data = dat_mod$cohorts_doc,0)
# dat_mod$cohorts_sci <- replace_na(data = dat_mod$cohorts_sci,0)
# dat_mod$cohorts_pub <- replace_na(data = dat_mod$cohorts_pub,0)
# dat_mod$cited_by_msm_count <- replace_na(data = dat_mod$cited_by_msm_count,0)
# 
# outcome="published_in_journal"
# df_mod=dat_mod %>%
#   filter(article_type=="Preprint",year==2020) %>% 
#   dplyr::select(paste0("topic_",1:ntopic),
#                 cited_by_msm_count,cohorts_com,cohorts_doc,
#          cohorts_sci,cohorts_pub,
#          cites_per_day,published_in_journal
#          )
# df_mod <- df_mod[complete.cases(df_mod),]
# 
# top_terms$comb_name=paste0("Topic ",as.character(top_terms$topic), " (",top_terms$terms,")")
# 
# colnames(df_mod)[1:ntopic] <- top_terms$comb_name
# colnames(df_mod) [colnames(df_mod) =="cited_by_msm_count"] <- "News media mentions"
# colnames(df_mod) [colnames(df_mod) =="cohorts_com"] <- "Social media mentions: science communicators"
# colnames(df_mod) [colnames(df_mod) =="cohorts_sci"] <- "Social media mentions: scientists"
# colnames(df_mod) [colnames(df_mod) =="cohorts_pub"] <- "Social media mentions: general public"
# colnames(df_mod) [colnames(df_mod) =="cohorts_doc"] <- "Social media mentions: medical practitioners"
# colnames(df_mod) [colnames(df_mod) =="cites_per_day"] <- "Citations per day"
# 
# 
# set.seed(123)
# split <- caret::createDataPartition(pull(df_mod,outcome),times = 1,p = 0.7, list =F)
# splitindex <- 1:nrow(df_mod) %in% split
# table(splitindex)
# train=df_mod[splitindex,]
# test=df_mod[!splitindex,]
# 
# 
# # Variable selection with sharp stability selection -----------------------
# X=train %>% dplyr::select(-c(all_of(outcome)))
# y=train %>% dplyr::select(all_of(outcome)) %>% unlist()
# myK=500
# myseed=123
# stab_out <- sharp::VariableSelection(xdata = X, 
#                                      ydata = y,
#                                      # penalty.factor = penalty.factor,
#                                      # pi_list = c(0.5,0.6,0.7,0.8),
#                                      K = myK,
#                                      pi_list = seq(0.6,0.99,0.01),
#                                      seed = myseed,
#                                      family = "binomial",
#                                      PFER_method = "MB",
#                                      # n_cores = 30,
#                                      verbose = T,
#                                      resampling = "bootstrap",
#                                      output_data = T
# )
# 
# ### Save results
# suffix="abstract"
# # OverReact::saveREACT(stab_out,outpath = outpath,filename = paste0("stability_selection_results_",outcome,"_",suffix))
# saveRDS(object = stab_out,file = paste0(outpath,"stability_selection_results_",outcome,"_",suffix,".rds") )
# # Calibrationplot
# png(paste0(figpath,"stability_selection_calbration_plot_",outcome,"_",suffix,".png"),width = 7,height = 7, units="in", res = 300)
# par(mar=c(7,5,7,6))
# sharp::CalibrationPlot(stab_out)
# dev.off()
# 
# 
# # Plot results ------------------------------------------------------------
# 
# X_test=test %>% dplyr::select(-c(all_of(outcome)))
# y_test=test  %>% dplyr::select(all_of(outcome)) %>% unlist()
# opt_iter=which.max(stab_out$S)
# 
# # get incremental results
# results_df <- getIncrementalSummary(stab_out = stab_out,
#                                     ydata = y_test,
#                                     xdata = X_test,
#                                     reorder_by_normalised_beta = F,
#                                     K = 20,
#                                     n_thr = min(20,ncol(X_test)),
#                                     family = "binomial",
#                                     getLoss = T)
# 
# 
# 
# results_df_2$variable[results_df_2$variable=="Altmetrics score (NA)"] <- "Altmetrics score"
# results_df_2$variable[results_df_2$variable=="Cites per day (NA)"] <- "Cites per day"
# results_df_2$variable <- gsub(pattern = "topic_",replacement = "Topic ",results_df_2$variable)
# 
# # plot!
# myplot=plotStabResultsStripedFlipped(results_df,
#                                      opt_thresh=stab_out$P[opt_iter], 
#                                      stab_out = stab_out,
#                                      plotOnlyTopN = 50,
#                                      plotOnlySelected = F,
#                                      plot_aucs = T)
# 
# myplot
# 
# 
# OverReact::saveREACTplot(p = myplot,figpath = figpath,
#                          filename = paste0("stability_selection_plot_",outcome,"_",suffix),
#                          width = 14,height = 8)
# 
# 
# 
# 
# 
# 
# # Time to event stability -------------------------------------------------
# 
# maxdate=max(dat$date)
# dat_mod <- dat_mod %>% 
#   mutate(case=published_in_journal,
#          time=case_when(case==1 ~ as.numeric(subs_pub_date-date_published),
#                         case==0 ~ as.numeric(maxdate-date_published)),
#          time=case_when(time<0 ~ NA_real_,
#                         T ~time )
#          )
# 
# 
# df_mod=dat_mod %>%
#   filter(article_type=="Preprint", !(time ==0)) %>% 
#   dplyr::select(paste0("topic_",1:ntopic),
#                 altmetrics_score,cites_per_day,time,case
#   )
# df_mod <- df_mod[complete.cases(df_mod),]
# 
# top_terms$comb_name=paste0("Topic ",as.character(top_terms$topic), " (",top_terms$terms,")")
# 
# colnames(df_mod)[1:ntopic] <- top_terms$comb_name
# colnames(df_mod) [colnames(df_mod) =="altmetrics_score"] <- "Altmetrics score"
# colnames(df_mod) [colnames(df_mod) =="cites_per_day"] <- "Cites per day"
# 
# 
# outcome="case"
# # create holdout data set
# set.seed(123)
# split <- caret::createDataPartition(pull(df_mod,outcome),times = 1,p = 0.7, list =F)
# splitindex <- 1:nrow(df_mod) %in% split
# table(splitindex)
# train=df_mod[splitindex,]
# test=df_mod[!splitindex,]
# 
# # X and Y split
# xdata = train %>% dplyr::select(-time,-case) %>% as.matrix()
# ydata = train %>%  dplyr::select(time,case) %>% as.matrix()
# 
# # run variable selection
# stab_out=sharp::VariableSelection(xdata = xdata,ydata = ydata,
#                                   pi_list = seq(0.6,0.99,0.01),
#                                   family = "cox",K = 100)
# 
# sharp::CalibrationPlot(stab_out)
# 
# # get test data
# X_test=test %>% dplyr::select(-time,-case) %>% as.matrix()
# y_test=test %>% dplyr::select(time,case)  %>% as.matrix()
# opt_iter=which.max(stab_out$S)
# 
# # get incremental results
# results_df <- getIncrementalSummary(stab_out = stab_out,
#                                     ydata = y_test,
#                                     xdata = X_test,
#                                     reorder_by_normalised_beta = T,
#                                     K = 20,
#                                     n_thr = min(20,ncol(X_test)),
#                                     family = "cox",
#                                     getLoss = F)
# 
# 
# # plot!
# myplot=plotStabResultsStripedFlipped(results_df, opt_thresh=stab_out$P[opt_iter], 
#                                      stab_out = stab_out,
#                                      plotOnlyTopN = 50,family = "cox",
#                                      plotOnlySelected = F,
#                                      plot_aucs = F) 
# myplot
# 
# 
# OverReact::saveREACTplot(p = myplot,figpath = figpath,
#                          filename = paste0("stability_selection_plot_coxmodel_",outcome,"_",suffix),
#                          width = 10,height = 8)
# 
# 
# 
# 
# 

