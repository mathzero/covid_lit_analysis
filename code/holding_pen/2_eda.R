
#' First clear the environment of variables
rm(list=ls(all=TRUE))

outpath <- paste0(getwd(),"/output/")
figpath <- paste0(getwd(),"/plots/")
datapath <- paste0(getwd(),"/data/")
function_script_path ="E:/home/mw418/function_scripts/"
function_script_path="/Users/mathzero/Library/CloudStorage/GoogleDrive-mattdwhitaker@gmail.com/My Drive/Imperial/PhD/REACT2_analysis_in_progress/RESULTS/function_scripts/"
function_script_path ="/Users/mw418/analysis/RESULTS/function_scripts/"


#' Source any functions from the local file
source(paste0(function_script_path,"/load_packages.R"))
source(paste0(function_script_path,"/cats_and_covs.R"))
source(paste0(function_script_path,"/create_subfolder.R"))
source(paste0(function_script_path,"/forest_plot.R"))

source(file = "code/0_functions.R")


# update.packages("Rcpp",repos="http://se-r.sm.med.ic.ac.uk", dependencies = TRUE )

#' Pull in packages needed
package.list <- c("prevalence","mgcv","knitr","dplyr","factoextra","tidyr",
                  "ggplot2","gdata","ggsci", "RColorBrewer", "tidyverse", "lubridate", 
                  "ComplexHeatmap","gganimate","ggnetwork","ppcor","topicmodels",
                  "tidytext", "quanteda", "widyr", "igraph", "ggraph","stm","ldatuning",
                  "readr","ggthemes", "questionr", "gridExtra","scales","ggpubr",
                  "Rtsne","rsvd", "geometry","Rcpp","furrr","future","rjags","sysfonts",
                  "patchwork", "OverReact")
load_packages(package.list)

# create subfolder
createMySubfolder(subfolderName = "eda")

# no scientific notation
options(scipen = 999)

# Load data ---------------------------------------------------------------


# Save
dat <- readRDS("data/dat_main.rds")
sparse_dfs <- readRDS("data/sparse_dfs_1000_min.rds")
dat_text_abs <- sparse_dfs$df_text_sparse_abstract %>% as.matrix() %>% as.data.frame()
dat_text_titles <- sparse_dfs$df_text_sparse_title %>% as.matrix() %>% as.data.frame()
dat_text_abs$unique_id=as.character(rownames(dat_text_abs))
dat_text_titles$unique_id=as.character(rownames(dat_text_titles))

# just check all unique IDs are present in the main data file
setdiff(dat_text_titles$unique_id,dat$unique_id) # yep
 
sarswords=c("2019-nCoV", "COVID-19","SARS-CoV-2", "HCoV-2019","hcov" , "NCOVID-19", 
"severe acute respiratory syndrome coronavirus 2","SarsCoV2","COVID19","2019-nCov",
"severe acute respiratory syndrome corona virus 2", "Wuhan coronavirus","China coronavirus",
"Wuhan corona virus","China corona virus", "novel coronavirus","novel corona virus") 

falsepos=grepl(pattern = paste(tolower(sarswords),collapse = "|"),x =dat$title_preferred,ignore.case = T )
table(falsepos,dat$preprint)

# 

# 
# ### Propely identify preprints
# preprint_names=unique(dat$journal_title[grepl("rxiv",tolower(dat$journal_title))])
# preprint_names <- c(preprint_names,
#                     unique(dat$journal_title[grepl("preprint",tolower(dat$journal_title))]))
# preprint_names <- c(preprint_names,
#                     unique(dat$journal_title[grepl("archive",tolower(dat$journal_title))]))
# preprint_names <- c(preprint_names,"Research Square","Advance","ESSOAr","advance","EGUsphere",
#                     "Beilstein Archives","Cambridge Open Engage","Frenxiv","Arabixiv")
# preprint_names <- unique(preprint_names)
# dat$preprint <- case_when(dat$journal_title%in%c(preprint_names) ~ "Preprint",
#                           T ~ "Journal article")


# Table One ---------------------------------------------------------------

dat <- dat %>% mutate(abstract_available = case_when(is.na(abstract_preferred) ~ "Yes",
                                                           T ~ "No"),
                      dummy="nobs",
                      year_factor=as.factor(year)) %>% 
  mutate(across(c("citations_count","metrics_recent_citations","altmetrics_score","cited_by_tweeters_count","cited_by_msm_count"),
                ~replace_na(.x,0)))

rowvar_list=c("dummy","year_factor","region","citations_count","metrics_recent_citations","published",
              "altmetrics_score","cited_by_tweeters_count","cited_by_msm_count")
# cov_names=as.list(rowvar_list)
cov_names=list("Papers published","Year","Region","Number of citations","Recent citations",
                   "Published in journal","Altmetrics score","Number of tweets","Number of media mentions")
names(cov_names)=rowvar_list
tab1 <- OverReact::crossTabMulti(dat=dat,rowvar_list = rowvar_list,colvar = "preprint",
                                 cov_names = cov_names,include_percentages = T,
                                 rowwise_precentages = F,confint = F,comma_thousands = T)

OverReact::saveREACTtable(tab = tab1,outpath = outpath,filename = "table_one")


# Get summary of papers by topic ------------------------------------------

summary_category=dat %>% 
  filter(!is.na(Category_1),Category_1!="") %>% 
  group_by(Category_1) %>% 
  summarise(n_papers=n(),
            n_journals=n_distinct(journal_title),
            mean_altmetric_score=mean(altmetrics_score, na.rm=T),
            mean_citations=mean(citations_count, na.rm=T),
            mean_journal_IF=mean(if_2022, na.rm=T)
            ) %>% 
  arrange(-n_papers) %>% slice_head(n=20)

summary_journal=dat %>% 
  mutate(if_2022=case_when(is.na(if_2022) ~ 0,
                           T ~ if_2022)) %>% 
  # filter(!is.na(Category_1),Category_1!="") %>% 
  group_by(journal_title) %>% 
  summarise(n_papers=n(),
            mean_altmetric_score=mean(altmetrics_score, na.rm=T),
            mean_citations=mean(citations_count, na.rm=T),
            journal_IF=mean(if_2022, na.rm=T)
  ) %>% 
  arrange(-n_papers) %>% 
  slice_head(n=20)

OverReact::saveREACTtable(tab = summary_category,outpath = outpath,filename = "journal_subject_metrics_summary")
OverReact::saveREACTtable(tab = summary_journal,outpath = outpath,filename = "journal_metrics_summary")


# Articles published over time ---------------------------


p_pub_over_time <- dat %>% 
  mutate(year=lubridate::year(date),
         week=lubridate::week(date)) %>% 
  # filter(date != as.Date("2021-01-01"),date != as.Date("2022-01-01")) %>% 
  group_by(date,preprint) %>% 
  summarise(n=n()) %>% 
  group_by(preprint) %>% 
  mutate(roll_n=zoo::rollapplyr(n,width=30, mean, align="right",partial=T)) %>% 
  ungroup() %>% 
  filter(date > as.Date("2020-03-30"), date < as.Date("2022-03-01")) %>%
  ggplot(aes(x=date, y=roll_n)) + 
  geom_point(size=0.1) +
  # geom_line()+
  # geom_smooth(col =OverReact::imperial_colours[[16]], size=0.4,method = "gam") +
  theme_bw() +
  facet_wrap(.~preprint,scales = "free_x", ncol=1)+
  scale_x_date(labels = date_format("%b %Y"))+
  labs(x="Date", y="Number of articles published (30-day average)") +
  theme_bw() + theme_adjust
  p_pub_over_time
OverReact::saveREACTplot(p = p_pub_over_time,figpath = figpath,filename = "articles_published_over_time",
                         width = 6,height = 5
)


# Media attenton over time ------------------------------------------------


# articles published over time
p_alt_over_time_preprint <- dat %>% 
  mutate(year=lubridate::year(date),
         week=lubridate::week(date)) %>% 
  # filter(date != as.Date("2021-01-01")) %>% 
  group_by(preprint,date) %>% 
  summarise(n=n(),
            mean_alt = sum(altmetrics_score, na.rm=T)) %>% 
  mutate(roll_mean_alt=zoo::rollmean(mean_alt,30, align="right", fill = NA, na.pad = F)) %>% 
  filter(date > as.Date("2020-03-01"), date < as.Date("2022-03-01")) %>% 
  ggplot(aes(x=date, y=roll_mean_alt)) + 
  geom_point(size=0.3) +
  # geom_line()+
  theme_bw() +
  scale_x_date(labels = date_format("%b %Y"))+
  facet_wrap(.~preprint, ncol=1, scales="free_x") +
  labs(x="Date", y="Total media and social media attention (Altmetrics score)") +
  theme_adjust
p_alt_over_time_preprint
OverReact::saveREACTplot(p = p_alt_over_time_preprint,figpath = figpath,filename = "media_attention_over_time_preprint_facet",
                         width = 6,height = 5
)

# articles published over time
p_alt_over_time_mean_preprint <- dat %>% 
  mutate(year=lubridate::year(date),
         week=lubridate::week(date)) %>% 
  # filter(date != as.Date("2021-01-01"),date != as.Date("2022-01-01")) %>% 
  group_by(preprint,date) %>% 
  summarise(n=n(),
            mean_alt = mean(altmetrics_score, na.rm=T)) %>% 
  # mutate(roll_mean_alt=zoo::rollmean(mean_alt,30, align="right", fill = NA, na.pad = F)) %>% 
  mutate(roll_mean_alt=zoo::rollapplyr(mean_alt,width=30, mean, align="right",partial=T)) %>% 
  
  filter(date > as.Date("2020-03-01"), date < as.Date("2022-03-01")) %>% 
  ggplot(aes(x=date, y=roll_mean_alt)) + 
  geom_point(size=0.1) +
  # geom_line()+
  # geom_smooth(col =OverReact::imperial_colours[[16]], size=0.4,method = "gam", inherit.aes = T) +
  theme_bw() +
  scale_x_date(labels = date_format("%b %Y"))+
  facet_wrap(.~preprint, ncol=1, scales="free_x") +
  labs(x="Date", y="Average media and social media attention per paper (Altmetrics score)") +
  theme_adjust
p_alt_over_time_mean_preprint
OverReact::saveREACTplot(p = p_alt_over_time_mean_preprint,figpath = figpath,filename = "media_attention_over_time_average_preprint_facet",
                         width = 6,height = 5
)


# Preprints published over time, by publication status --------------------


# articles published over time
p_pub_over_time_preprint_published <- dat %>% 
  filter( preprint == "Preprint") %>% 
  mutate(year=lubridate::year(date),
         week=lubridate::week(date)) %>% 
  group_by(date,published) %>% 
  summarise(n=n()) %>% 
  group_by(published) %>% 
  arrange(published, date) %>% 
  mutate(roll_n=zoo::rollmean(n,30, align="right", fill = NA, na.pad = F)) %>% 
  filter(date > as.Date("2020-03-30"), date < as.Date("2022-03-01")) %>% 
  mutate(published = case_when(published ==1 ~ "Published in journal",
                               T~"Not yet published")) %>% 
  ggplot(aes(x=date, y=roll_n, fill = factor(published))) + 
  geom_area( stat="identity", col="white",size=0.01)+
  # OverReact::scale_color_imperial(palette = "two_col_grey_orng", reverse = T)+
  OverReact::scale_fill_imperial(palette = "two_col_grey_orng", reverse = T)+
  # scale_fill_manual(values = myCols[c(5,2)]) +
  # scale_color_manual(values = myCols[c(5,2)]) +
  scale_x_date(labels = date_format("%b %Y"))+
  labs(x="Date", y="Number of preprints published (30-day average)", col ="", fill ="") +
  OverReact::theme_react() +
  theme(legend.position = c(0.8,0.9))
p_pub_over_time_preprint_published
OverReact::saveREACTplot(p = p_pub_over_time_preprint_published,figpath = figpath,
                         filename = "preprints_published_over_time_by_pub_status",
                         width = 7,height = 4.5
)



# Altmetrics vs cites per day ---------------------------------------------
p_altmetrics_citation_scatter=dat %>% 
  filter(cites_per_day>=0) %>% 
  ggplot(aes(x=cites_per_day, y=altmetrics_score, fill = factor(preprint),
             col = factor(preprint), size=citations_count)) + 
  geom_point(alpha=0.5)+
  # scale_x_log10()+
  # scale_y_log10()+
  geom_smooth(method="lm",inherit.aes = T)+
  scale_size_continuous(guide="none") +
  OverReact::scale_fill_imperial(palette = "two_col_grey_orng", reverse = T)+
  OverReact::scale_color_imperial(palette = "two_col_grey_orng", reverse = T)+
  labs(x="Citations per day", y="Altmetrics score", col ="", fill ="", size="") +
  OverReact::theme_react()
p_altmetrics_citation_scatter
OverReact::saveREACTplot(p = p_altmetrics_citation_scatter,figpath = figpath,
                         filename = "altmetrics_vc_citation_scatter",
                         width = 7,height = 7
)






# Preprints ---------------------------------------------------------------

dat_preprint <- dat %>% filter(preprint=="Preprint")
dat_preprint$pub_in_journal %>% table()
dat_preprint$pub_in_journal <- case_when(is.na(dat_preprint$resulting_publication_doi) ~ "Not published",
                                         TRUE ~ "Published")
# remove duplicates
dat_preprint <- dat_preprint[!duplicated(dat_preprint$unique_id),]

p_altmetric <- dat_preprint %>% 
  # filter(date <=as.Date("2021-01-01")) %>% 
  ggplot(aes(col=pub_in_journal,y=altmetrics_score, x=pub_in_journal)) +
  geom_boxplot() +
  # geom_quasirandom(alpha=0.5) +
  scale_y_log10() +
  theme_bw() +
  scale_color_imperial(palette = "blue") +
  theme_adjust +
  labs(col="", y="", x="Altmetrics score") +
  theme(legend.position = "none") +
  ggpubr::stat_compare_means(comparisons = list(c("Not published","Published")))

p_altmetric

OverReact::saveREACTplot(p = p_altmetric,figpath = figpath,
                         filename = "Preprint_altmetrics_boxplot_by_published_status",
                         width = 6,height = 3
)


p_preprint_cites_vs_altmetrics<- dat_preprint %>% 
  filter(date <=as.Date("2021-01-01")) %>%
  ggplot(aes(y=cites_per_day,x=altmetrics_score, size= citations_count)) +
  geom_point(alpha=0.3) +
  scale_x_log10() +
  scale_y_log10() +
  facet_wrap(.~preprint) +
  theme_bw() +
  geom_smooth(method ="lm",show.legend = F) +
  theme_adjust +
  labs(x="Altmetrics score", y= "Average citations per day", size = "Total citations")
p_preprint_cites_vs_altmetrics

# save
OverReact::saveREACTplot(p = p_preprint_cites_vs_altmetrics,figpath = figpath,
                         filename = "preprint_citesPerDay_vs_altmetrics",
                         width = 6,height = 6
)

# in all including non-preprints
p_cites_vs_altmetrics<- dat %>% 
  filter(date <=as.Date("2021-01-01")) %>%
  ggplot(aes(y=cites_per_day,x=altmetrics_score, size= citations_count)) +
  geom_point(alpha=0.3) +
  scale_x_log10() +
  scale_y_log10() +
  facet_wrap(.~preprint) +
  theme_bw() +
  geom_smooth(method ="lm",show.legend = F) +
  theme_adjust +
  labs(x="Altmetrics score", y= "Average citations per day", size = "Total citations")
p_cites_vs_altmetrics

# save
OverReact::saveREACTplot(p = p_cites_vs_altmetrics,figpath = figpath,
                         filename = "citesPerDay_vs_altmetrics",
                         width = 9,height = 6
)






# Plot of citations vs time since publication -----------------------------



p_cites_over_time <- dat %>% 
  filter(days_since_publication>=0) %>% 
  ggplot(aes(x=days_since_publication,y=citations_count)) +
  geom_point(size=0.1, alpha=0.5)+
  OverReact::theme_react() +
  facet_wrap(.~ preprint) +
  # scale_y_log10() +
  geom_smooth(show.legend = F)

p_cites_over_time

# save
OverReact::saveREACTplot(p = p_cites_over_time,figpath = figpath,
                         filename = "cites_over_time",
                         width = 9,height = 6
)




# correlation analysis ----------------------------------------------------

dat$subs_pub_h_index
names(dat)
myvars <- c("altmetrics_score","cited_by_msm_count",
            "cited_by_tweeters_count","cited_by_fbwalls_count",
            "cohorts_sci","cohorts_doc","cohorts_com","cohorts_pub",
            "cites_per_day","citations_count",
            "subs_pub_h_index","subs_pub_sjr",
            "published"
)
myvarnames=c("All media and social media","Mainstream media",
             "Twitter","Facebook",
             "Social media (scientists)","Social media (medics)",
             "Social media (science communicators)",
             "Social media (general public)",
             "Citation rate", "Total citations count",
             "H-index of publishing journal",
             "Impact score of publishing journal",
             "Published in journal"
)

cormat_preprints <- dat %>% filter(preprint=="Preprint") %>% 
  dplyr::select(all_of(myvars)) %>% 
  mutate(across(all_of(myvars[1:10]),
                ~replace_na(.x,0))) %>% 
  cor(use = "pairwise.complete.obs") %>% as.matrix()
colnames(cormat_preprints) <- rownames(cormat_preprints) <- myvarnames
maxval=0.5
col_fun=circlize::colorRamp2(breaks = c(0,0.5,1), colors = c("white",myCols[[2]],myCols[[1]]))
split=c(rep("Altmetrics",8),rep("Citation \ndata",2), rep("Subsequent \npublication",3))
heatmap_preprints <- ComplexHeatmap::Heatmap(cormat_preprints,name = "Correlation",
                                             column_title = "Preprints",
                                             column_title_gp = gpar(fontsize=15,fontface="bold"),
                                             
                                             # cluster_columns = F,
                                             split = split,
                                             column_split = split, row_split=split,cluster_rows = F,cluster_columns = F,
                                             rect_gp = gpar(col="white", lwd=1),
                                             # heatmap_legend_param = list(heatmap_legend_side="bottom"),
                                            
                                             cell_fun = function(j,i,x,y,width, height, fill){
                                               grid.text(sprintf("%.2f", cormat_preprints[i,j]), x, y,
                                                         gp=gpar(fontsize=10,
                                                                 col = 
                                                                   ifelse(cormat_preprints[i,j]<0 |
                                                                            cormat_preprints[i,j] == 0,
                                                                          "red",  
                                                                          ifelse(cormat_preprints[i,j]>0.25 |
                                                                                   cormat_preprints[i,j] == 0,
                                                                                 "white","black"))))
                                             },
                                             col = col_fun)

heatmap_preprints




# Heatmap (journal articles only) -----------------------------------------

myvars_journal <- c("altmetrics_score","cited_by_msm_count",
            "cited_by_tweeters_count","cited_by_fbwalls_count",
            "cohorts_sci","cohorts_doc","cohorts_com","cohorts_pub",
            "cites_per_day","citations_count",
            "h_index","sjr"
)
myvarnames_journal=c("All media and social media","Mainstream media",
             "Twitter","Facebook",
             "Social media (scientists)","Social media (medics)",
             "Social media (science communicators)",
             "Social media (general public)",
             "Citation rate", "Total citations count",
             "Journal H-index",
             "Journal impact score"
)







cormat_journal <- dat %>% filter(preprint!="Preprint") %>% 
  dplyr::select(all_of(myvars_journal)) %>% 
  mutate(across(everything(),
                ~replace_na(.x,0))) %>% 
  cor(use = "pairwise.complete.obs") %>% as.matrix()
colnames(cormat_journal) <- rownames(cormat_journal) <- myvarnames_journal
# cormat_journal[9,] <- cormat_journal[,9] <- 0
split_journal=c(rep("Altmetrics",8),rep("Citation \ndata",2), rep("Journal \ndata",2))


heatmap_journals <- ComplexHeatmap::Heatmap(cormat_journal,name = "Correlation",
                                            column_title = "Journal articles",
                                            column_title_gp = gpar(fontsize=15,fontface="bold"),
                                            # cluster_rows = F,
                                            # cluster_columns = F,
                                            split = split_journal,
                                            rect_gp = gpar(col="white", lwd=1),
                                            column_split = split_journal, row_split=split_journal,cluster_rows = F,cluster_columns = F,
                                            
                                            cell_fun = function(j,i,x,y,width, height, fill){
                                              grid.text(sprintf("%.2f", cormat_journal[i,j]), x, y,
                                                        gp=gpar(fontsize=10,
                                                                col = 
                                                                  ifelse(cormat_journal[i,j]<0 |
                                                                           cormat_journal[i,j] == 0,
                                                                         "red",  
                                                                         ifelse(cormat_journal[i,j]>0.25 |
                                                                                  cormat_journal[i,j] == 0,
                                                                                "white","black"))))
                                            },
                                            col = col_fun)

heatmap_journals
# combined_heatmap <- draw((heatmap_preprints+heatmap_journals), ht_gap =unit(1.3,"cm"))
# combined_heatmap

png(paste0(figpath,"heatmap_attention_correlation_preprints.png"), width = 9,
    height = 8, units = "in", res = 300)
heatmap_preprints
dev.off()
png(paste0(figpath,"heatmap_attention_correlation_journals.png"), width = 9,
    height = 8, units = "in", res = 300)
heatmap_journals
dev.off()






# Highest altmetrics papers -----------------------------------------------

top_100_altmetrics=dat %>% arrange(-cited_by_tweeters_count) %>% head(100)

dat$cited_by_tweeters_count


grepl("Santa",top_100_altmetrics$abstract_preferred)




# Tracking use of specific terms over time --------------------------------

alt_mean=dat$altmetrics_score %>%  mean(na.rm=T)
cite_mean=dat$citations_count %>%  mean(na.rm=T)
cpd_mean=dat$cites_per_day %>%  mean(na.rm=T)
# hcq_summary_dat$week %>% min()

dat_text_titles_2 <- dat %>% right_join(dat_text_titles, by="unique_id")
dat_text_titles_2$date

# Get summary data about hcq
hcq_summary_dat <- dat_text_titles_2 %>% 
  # filter(preprint.x!="Preprint") %>% 
  mutate(week=lubridate::week(date),
               week_name = case_when(lubridate::year(date) == 2020 ~as.Date("2020-01-01") + 7*(week-1),
                                     T~ as.Date("2022-03-01") + 7*(week-1)
                 )) %>% 
  filter(date >= as.Date("2020-03-01"), date < as.Date("2022-03-01")) %>% 
  group_by( hcq = ifelse(hydroxychloroquine >0,"HCQ in title", "HCQ not in title"), date,preprint.x)  %>% 
  summarise(n=n(),
            mean_cites = mean(citations_count, na.rm=T),
            mean_cites_per_day = mean(cites_per_day, na.rm=T),
            mean_altmetrics = mean(altmetrics_score, na.rm=T),
            adj_mean_cites = mean_cites/cite_mean,
            adj_mean_cpd = mean_cites_per_day/cpd_mean,
            adj_mean_altmetrics = mean_altmetrics/alt_mean,
            media_attention_scientific_attention_ratio =mean_altmetrics/cpd_mean
  ) %>% 
  group_by(hcq,preprint.x) %>% 
  mutate(rolln=zoo::rollmean(n,7, align="right", fill = NA, na.pad = F),
         rollmean_cites = zoo::rollmean(mean_cites,7, align="right", fill = NA, na.pad = F),
         rollmean_cites_per_day = zoo::rollmean(mean_cites_per_day,7, align="right", fill = NA, na.pad = F),
         rollmean_altmetrics = zoo::rollmean(mean_altmetrics,7, align="right", fill = NA, na.pad = F),
         rollmedia_attention_scientific_attention_ratio =zoo::rollmean(media_attention_scientific_attention_ratio,7, align="right", fill = NA, na.pad = F)
  )

hcq_summary_dat

# PLot altmetrics
p_hcq_alt <- hcq_summary_dat %>% 
  filter(!grepl("not",hcq)) %>% 
  ggplot(aes(x=date,y=rolln, col = preprint.x)) +
  geom_line() +
  theme_adjust +
  OverReact::theme_react() +
  scale_colour_manual(values = myCols[c(3,1)]) +
  scale_x_date(date_labels = "%b %Y",breaks = "3 month") +
  # facet_wrap(.~preprint.x,nrow=2)+
  labs(x="Date", y="Media/social media attention score", col = "")

p_hcq_alt

# save
OverReact::saveREACTplot(p = p_hcq_alt,figpath = figpath,
                         filename = "hydroxychloroquine_altmetrics_over_timw",
                         width = 9,height = 9
)


# plot cites per day
hcq_summary_dat%>% 
  ggplot(aes(x=week_name,y=adj_mean_cpd, col = hcq)) +
  geom_line() +
  theme_adjust +
  scale_colour_manual(values = myCols[c(3,1)]) +
  scale_x_date(date_labels = "%b %Y",breaks = "3 month") +
  theme_bw() +
  labs(x="Date", y="Citations per day", col = "")




# 
# devtools::install("E:/Group/react2_study5/report_phases_combined/misc/ICoLour-master-mew/ICoLour-master/")
#   devtools::install_local(path = "E:/Group/react2_study5/report_phases_combined/misc/ICoLour-master-mew/ICoLour-master/",dependencies = T)
#   ICoLour::imperial_palettes
  


# by country --------------------------------------------------------------

# plot countries
p_countries <- dat %>% group_by(research_org_country_names) %>% 
  summarise(n=n(),
            total_cites = sum(citations_count, na.rm=T)) %>% 
  arrange(-n) %>% 
  top_n(20) %>% 
  ggplot(aes(x=reorder(research_org_country_names,n), y=n)) +
  geom_col() +
  coord_flip() +
  theme_bw() +
  theme_adjust +
  labs(x="", y="Papers published")
# save
OverReact::saveREACTplot(p = p_countries,figpath = figpath,
                         filename = "total_papers_by_country",
                         width = 7,height = 5
)


# plot countries
p_countries_cites <- dat %>% group_by(research_org_country_names) %>% 
  summarise(n=n(),
            total_cites = sum(citations_count, na.rm=T),
            total_media = sum(altmetrics_score, na.rm=T)) %>% 
  arrange(-n) %>% 
  top_n(20) %>% 
  ggplot(aes(x=reorder(research_org_country_names,n), y=total_cites)) +
  geom_col() +
  coord_flip() +
  theme_bw() +
  theme_adjust +
  labs(x="", y="Total citations")
p_countries_cites
# save
OverReact::saveREACTplot(p = p_countries_cites,figpath = figpath,
                         filename = "total_cites_by_country",
                         width = 7,height = 5
)

# Combine
p_country_comb_2 <- p_countries+p_countries_cites +plot_layout(guides="collect")


# plot countries
p_countries_media <- dat %>% group_by(research_org_country_names) %>% 
  summarise(n=n(),
            total_cites = sum(citations_count, na.rm=T),
            total_media = sum(altmetrics_score, na.rm=T)) %>% 
  arrange(-n) %>% 
  top_n(20) %>% 
  ggplot(aes(x=reorder(research_org_country_names,n), y=total_media)) +
  geom_col() +
  coord_flip() +
  theme_bw() +
  theme_adjust +
  labs(x="", y="Total media attention")
p_countries_media
# save
OverReact::saveREACTplot(p = p_countries_media,figpath = figpath,
                         filename = "total_media_by_country",
                         width = 7,height = 5
)
p_country_comb_3 <- p_countries+p_countries_cites +p_countries_media+plot_layout(guides="collect")
# save
OverReact::saveREACTplot(p = p_country_comb_3,figpath = figpath,
                         filename = "total_stats_by_country_3panel",
                         width = 12,height = 5
)


#### Average citations per day ###

# plot countries
p_countries_cpd <- dat %>% 
  group_by(research_org_country_names) %>% 
  summarise(n=n(),
            total_cites = sum(citations_count, na.rm=T),
            mean_cites = mean(citations_count, na.rm=T),
            mean_cpd = mean(cites_per_day, na.rm=T),
            total_media = sum(altmetrics_score, na.rm=T)) %>% 
  arrange(-n) %>% 
  top_n(20) %>% 
  ggplot(aes(x=reorder(research_org_country_names,n), y=mean_cpd)) +
  geom_col() +
  coord_flip() +
  theme_bw() +
  theme_adjust +
  labs(x="", y="Average cites per paper per day")
p_countries_cpd


# plot cites average
p_countries_cites_mean <- dat %>% 
  group_by(research_org_country_names) %>% 
  summarise(n=n(),
            total_cites = sum(citations_count, na.rm=T),
            mean_cites = mean(citations_count, na.rm=T),
            mean_cpd = mean(cites_per_day, na.rm=T),
            total_media = sum(altmetrics_score, na.rm=T)) %>% 
  arrange(-n) %>% 
  top_n(20) %>% 
  ggplot(aes(x=reorder(research_org_country_names,n), y=mean_cites)) +
  geom_col() +
  coord_flip() +
  theme_bw() +
  theme_adjust +
  labs(x="", y="Average cites per paper")

p_cites_cpd_comb <- p_countries_cites_mean + p_countries_cpd

# save
OverReact::saveREACTplot(p = p_cites_cpd_comb,figpath = figpath,
                         filename = "mean_cites_cpd_by_country",
                         width = 9,height = 5
)




# Highest altmetric papers ------------------------------------------------

dat_top_altmetrics <- dat[order(dat$altmetrics_score,na.last = T,decreasing = T),][1:20,] %>% 
  dplyr::select(doi,title_preferred,journal_title,altmetrics_score,citations_count,date_normal,published)
OverReact::saveREACTtable(tab = dat_top_altmetrics,outpath = outpath,filename = "top_20_altmetrics",save_rds = F)


dat_top_altmetrics_preprint <- dat[dat$preprint=="Preprint",][order(dat$altmetrics_score[dat$preprint=="Preprint"],na.last = T,decreasing = T),][1:20,] %>% 
  dplyr::select(doi,title_preferred,journal_title,altmetrics_score,citations_count,date_normal,published)
OverReact::saveREACTtable(tab = dat_top_altmetrics_preprint,outpath = outpath,filename = "top_20_altmetrics_preprint",save_rds = F)





# by citations
dat_top_cites<-dat[order(dat$citations_count,na.last = T,decreasing = T),][1:20,] %>% 
  dplyr::select(doi,title_preferred,journal_title,altmetrics_score,citations_count,date_normal,published)
OverReact::saveREACTtable(tab = dat_top_cites,outpath = outpath,filename = "top_20_cites",save_rds = F)



# Word frequency plots ----------------------------------------------------

numeric_vect=lapply(dat_text_titles,is.numeric) %>% unlist()
word_freqs <- as.data.frame(colSums(dat_text_titles[,numeric_vect]))
word_freqs$word = rownames(word_freqs)
names(word_freqs)[[1]] <- "word_freqs"


p_freq <- word_freqs %>% 
  arrange(desc(word_freqs)) %>% 
  slice_head(n = 20) %>% 
  ggplot(aes(x=reorder(word, word_freqs), y=word_freqs)) + geom_col() +
  theme_bw() + theme_adjust +
  coord_flip() +
  labs(x="", y="Number of papers with word in title")
p_freq

# save
OverReact::saveREACTplot(p = p_freq,figpath = figpath,
                         filename = "word_freq",
                         width = 6,height = 5
)



