
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

#' Pull in packages needed
package.list <- c("knitr","dplyr","tidyr",
                  "ggplot2","gdata","ggsci", "RColorBrewer", "tidyverse", "lubridate", 
                  "ComplexHeatmap",
                  "ggthemes", "gridExtra","scales","ggpubr",
                  "patchwork", "OverReact")
# load_packages(package.list)
pacman::p_load(package.list)

# create subfolder
createMySubfolder(subfolderName = "eda")

# no scientific notation
options(scipen = 999)

# import fonts
extrafont::font_import(prompt =F )

# Load data ---------------------------------------------------------------


# Save
dat <- readRDS("data/dat_main.rds")
concepts <- read_csv("data/bq-results-20221124-concepts.csv")
concepts_wide <- readRDS("data/concepts_wide.rds")

# wrangle dat
dat <- dat %>% mutate(abstract_available = case_when(is.na(abstract_preferred) ~ "Yes",
                                                     T ~ "No"),
                      dummy="nobs",
                      year_factor=as.factor(year)) %>% 
  mutate(across(c("citations_count","altmetrics_score","cited_by_tweeters_count","cited_by_msm_count"),
                ~replace_na(.x,0)))

rowvar_list=c("dummy","year_factor","citations_count","published",
              "altmetrics_score","cited_by_tweeters_count","cited_by_msm_count")
# cov_names=as.list(rowvar_list)
cov_names=list("Papers published","Year","Number of citations",
               "Published in journal","Altmetrics score","Number of tweets","Number of media mentions")
names(cov_names)=rowvar_list
tab1 <- OverReact::crossTabMulti(dat=dat,rowvar_list = rowvar_list,colvar = "type",
                                 cov_names = cov_names,include_percentages = T,
                                 rowwise_precentages = F,confint = F,comma_thousands = T)

OverReact::saveREACTtable(tab = tab1,outpath = outpath,filename = "table_one")



# Get summary of papers by topic ------------------------------------------


summary_category=dat %>% 
  filter(!is.na(journal_category_1),journal_category_1!="") %>% 
  group_by(journal_category_1) %>% 
  summarise(n_papers=n(),
            n_journals=n_distinct(journal_title),
            mean_altmetric_score=mean(altmetrics_score, na.rm=T),
            mean_citations=mean(citations_count, na.rm=T),
            mean_journal_IF=mean(journal_if_2022, na.rm=T)
  ) %>% 
  arrange(-n_papers) %>% slice_head(n=20)


summary_journal=dat %>% 
  mutate(journal_if_2022=case_when(is.na(journal_if_2022) ~ 0,
                           T ~ journal_if_2022)) %>% 
  group_by(journal_title) %>% 
  summarise(n_papers=n(),
            mean_altmetric_score=round(mean(altmetrics_score, na.rm=T),1),
            mean_citations=round(mean(citations_count, na.rm=T),1),
            journal_IF=round(mean(journal_if_2022, na.rm=T),2)
  ) %>% 
  arrange(-journal_IF) %>% 
  slice_head(n=20)


# Get summary by concept
summary_concept=concepts %>% 
  left_join(dat %>% select(id,altmetrics_score,citations_count,journal_if_2022)) %>% 
  mutate(journal_if_2022=case_when(is.na(journal_if_2022) ~ 0,
                                   T ~ journal_if_2022)) %>% 
  filter(!is.na(concept)) %>%
  group_by(concept) %>% 
  summarise(n_papers=n(),
            mean_altmetric_score=round(mean(altmetrics_score, na.rm=T),1),
            mean_citations=round(mean(citations_count, na.rm=T),1),
            journal_IF=round(mean(journal_if_2022, na.rm=T),2)
  ) %>% 
  arrange(-n_papers)


OverReact::savePrettyExcelWorkbook(listOfTables = list(table_one=tab1,
                                                       top_categories=summary_category,
                                                       top_journals=summary_journal,
                                                       top_concepts=summary_concept %>% 
                                                         slice_head(n=20)),
                                   workbookName = "summary_tables",outpath = outpath,
                                   noDecimalsColumns = c("n_papers","n_journals"))
OverReact::saveREACTtable(tab = summary_category,outpath = outpath,filename = "journal_subject_metrics_summary")
OverReact::saveREACTtable(tab = summary_journal,outpath = outpath,filename = "journal_metrics_summary")
OverReact::saveREACTtable(tab = summary_concept,outpath = outpath,filename = "concept_metrics_summary")




# Papers published over time ----------------------------------------------


p_pub_over_time <- dat %>% 
  mutate(year=lubridate::year(date),
         week=lubridate::week(date)) %>% 
  # filter(date != as.Date("2021-01-01"),date != as.Date("2022-01-01")) %>% 
  group_by(date,type) %>% 
  summarise(n=n()) %>% 
  group_by(type) %>% 
  mutate(roll_n=zoo::rollapplyr(n,width=30, mean, align="right",partial=T)) %>% 
  ungroup() %>% 
  filter(date > as.Date("2020-01-01"), date < as.Date("2022-11-24")) %>%
  ggplot(aes(x=date, y=roll_n, col=type)) + 
  # geom_point(size=0.1) +
  geom_line(size=1)+
  # facet_wrap(.~type,scales = "free_x", ncol=1)+
  scale_x_date(labels = date_format("%b %Y"))+
  labs(x="Date", y="Number of articles published (30-day average)", col="", 
       title="Papers published per week", subtitle = "30 day moving average since January 2020",) +
  OverReact::scale_color_imperial(palette = "cool")+
  OverReact::theme_react()+
  theme(legend.position = c(0.1,0.9))

p_pub_over_time
OverReact::saveREACTplot(p = p_pub_over_time,figpath = figpath,filename = "articles_published_over_time",
                         width = 6,height = 5
)


### Media attention over time


# Media attenton over time ------------------------------------------------


# articles published over time
p_alt_over_time_type <- dat %>% 
  mutate(year=lubridate::year(date),
         week=lubridate::week(date)) %>% 
  # filter(date != as.Date("2021-01-01")) %>% 
  group_by(type,date) %>% 
  summarise(n=n(),
            mean_alt = sum(altmetrics_score, na.rm=T)) %>% 
  mutate(roll_mean_alt=zoo::rollmean(mean_alt,30, align="right", fill = NA, na.pad = F)) %>% 
  filter(date > as.Date("2020-01-01"), date < as.Date("2022-11-24")) %>% 
  ggplot(aes(x=date, y=roll_mean_alt,col=type)) + 
  geom_line(size=1)+
  # geom_line()+
  scale_x_date(labels = date_format("%b %Y"))+
  # facet_wrap(.~type, ncol=1, scales="free_x") +
  labs(x="Date", y="Total media and social media attention (Altmetrics score)", 
       title="Total media attention", subtitle = "30 day moving average since January 2020",
       col="") +
  OverReact::scale_color_imperial(palette = "cool")+
  OverReact::theme_react()+
  theme(legend.position = c(0.9,0.9)) 


p_alt_over_time_type
OverReact::saveREACTplot(p = p_alt_over_time_type,figpath = figpath,filename = "media_attention_over_time_type_facet",
                         width = 6,height = 5
)

# articles published over time
p_alt_over_time_mean_type <- dat %>% 
  mutate(year=lubridate::year(date),
         week=lubridate::week(date)) %>% 
  # filter(date != as.Date("2021-01-01")) %>% 
  group_by(type,date) %>% 
  summarise(n=n(),
            mean_alt = mean(altmetrics_score, na.rm=T)) %>% 
  mutate(roll_mean_alt=zoo::rollmean(mean_alt,30, align="right", fill = NA, na.pad = F)) %>% 
  filter(date > as.Date("2020-01-01"), date < as.Date("2022-11-24")) %>% 
  ggplot(aes(x=date, y=roll_mean_alt,col=type)) + 
  geom_line(size=1)+
  # geom_line()+
  scale_x_date(labels = date_format("%b %Y"))+
  # facet_wrap(.~type, ncol=1, scales="free_x") +
  labs(x="Date", y="Average media and social media attention (Altmetrics score)", 
       title="Average media attention per paper", subtitle = "30 day moving average since January 2020",
       col="") +
  OverReact::scale_color_imperial(palette = "cool")+
  OverReact::theme_react()+
  theme(legend.position = c(0.9,0.9)) 

p_alt_over_time_mean_type
OverReact::saveREACTplot(p = p_alt_over_time_mean_type,figpath = figpath,filename = "media_attention_over_time_average_type_facet",
                         width = 6,height = 5
)


### Combine plots

p_alt_comb=p_alt_over_time_type/p_alt_over_time_mean_type
p_alt_comb
OverReact::saveREACTplot(p = p_alt_comb,figpath = figpath,filename = "media_attention_over_time_two_panel",
                         width = 12,height = 10
)



# Concepts over time ------------------------------------------------------


myconcepts=c("COVID-19 vaccine","case fatality rate",
             "personal protective equipment","long COVID")
p_concepts_over_time=concepts %>% 
  left_join(dat %>% select(id,type,date)) %>% 
  filter(!is.na(concept), concept%in%myconcepts) %>% 
  mutate(year=lubridate::year(date),
         week=lubridate::week(date)) %>% 
  group_by(concept,date) %>% 
  summarise(n=n(),
            mean_relevance = sum(relevance, na.rm=T)) %>% 
  mutate(roll_mean_relevance=zoo::rollmean(mean_relevance,30, align="right", fill = NA, na.pad = F)) %>% 
  filter(date > as.Date("2020-01-01"), date < as.Date("2023-11-24")) %>% 
  ggplot(aes(x=date, y=roll_mean_relevance, col=concept,group=concept)) + 
  geom_line(size=1)+
  # ggrepel::geom_label_repel(aes(label = concept),
  #                                    nudge_x = 1,
  #                                    na.rm = TRUE)+
  # # geom_line
  # directlabels::geom_dl(aes(label=concept), method = list(directlabels::dl.trans(x = x - 0.2), "lastt.polygons", cex = 0.8)) +
  scale_x_date(labels = date_format("%b %Y"))+
  # facet_wrap(.~type, ncol=1, scales="free_y") +
  labs(x="Date", y="Sum of concept relevance across all papers", 
       title="Evolution of concept relevance in papers over time", subtitle = "30 day moving average since January 2020",
       col="") +
  OverReact::scale_color_imperial(palette = "default")+
  OverReact::theme_react()+
  theme(legend.position = c(0.2,0.8)) 

p_concepts_over_time



OverReact::saveREACTplot(p = p_concepts_over_time,figpath = figpath,
                         filename = "concepts_over_time",
                         width = 7,height = 4.5
)


# Preprints by publication status -----------------------------------------



# articles published over time
p_pub_over_time_preprint_published <- dat %>% 
  filter( type == "preprint") %>% 
  mutate(year=lubridate::year(date),
         week=lubridate::week(date)) %>% 
  group_by(date,published) %>% 
  summarise(n=n()) %>% 
  group_by(published) %>% 
  arrange(published, date) %>% 
  mutate(roll_n=zoo::rollmean(n,30, align="right", fill = NA, na.pad = F)) %>% 
  filter(date > as.Date("2020-01-01"), date < as.Date("2022-11-01")) %>% 
  mutate(published = case_when(published ==1 ~ "Published in journal",
                               T~"Not yet published")) %>% 
  ggplot(aes(x=date, y=roll_n, fill = factor(published))) + 
  geom_area( stat="identity", col="white")+
  # OverReact::scale_color_imperial(palette = "two_col_grey_orng", reverse = T)+
  OverReact::scale_fill_imperial(palette = "two_col_grey_orng", reverse = T)+
  # scale_fill_manual(values = myCols[c(5,2)]) +
  # scale_color_manual(values = myCols[c(5,2)]) +
  scale_x_date(labels = date_format("%b %Y"))+
  labs(x="Date", y="Number of preprints published (30-day average)", col ="", fill ="") +
  OverReact::theme_react() +
  # OverReact::scale_fill_imperial(palette = "two_col_grey_blue",reverse = T)+
  theme(legend.position = c(0.8,0.9))
p_pub_over_time_preprint_published


OverReact::saveREACTplot(p = p_pub_over_time_preprint_published,figpath = figpath,
                         filename = "preprints_published_over_time_by_pub_status",
                         width = 7,height = 4.5
)



# Correlation analysis ----------------------------------------------------

myvars <- c("altmetrics_score","cited_by_msm_count",
            "cited_by_tweeters_count","cited_by_fbwalls_count",
            "cohorts_sci","cohorts_doc","cohorts_com","cohorts_pub",
            "cites_per_day","citations_count",
            # "subs_pub_sjr",
            "published",
            "subs_pub_journal_if_2022"
)
myvarnames=c("All media and social media","Mainstream media",
             "Twitter","Facebook",
             "Social media (scientists)","Social media (medics)",
             "Social media (science communicators)",
             "Social media (general public)",
             "Citation rate", "Total citations count",
             "Published in journal","Impact factor of publishing journal"
             # "Impact score of publishing journal",
             
)

cormat_preprints <- dat %>% filter(type=="preprint") %>% 
  dplyr::select(all_of(myvars)) %>% 
  mutate(across(all_of(myvars[1:10]),
                ~replace_na(.x,0))) %>% 
  cor(use = "pairwise.complete.obs") %>% as.matrix()
colnames(cormat_preprints) <- rownames(cormat_preprints) <- myvarnames
maxval=0.5
col_fun=circlize::colorRamp2(breaks = c(0,0.5,1), colors = c("white",myCols[[2]],myCols[[1]]))
split=c(rep("Altmetrics",8),rep("Citation \ndata",2), rep("Subsequent \npublication",2))
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



#### Journal
dat$journal_jci
myvars <- c("altmetrics_score","cited_by_msm_count",
            "cited_by_tweeters_count","cited_by_fbwalls_count",
            "cohorts_sci","cohorts_doc","cohorts_com","cohorts_pub",
            "cites_per_day","citations_count",
            "journal_if_2022","journal_jci"
)
myvarnames=c("All media and social media","Mainstream media",
             "Twitter","Facebook",
             "Social media (scientists)","Social media (medics)",
             "Social media (science communicators)",
             "Social media (general public)",
             "Citation rate", "Total citations count",
             "Journal Impact Factor (2022)",
             "Journal Citation Indicator (2022)"
             
)

cormat_journals <- dat %>% filter(type!="preprint") %>% 
  dplyr::select(all_of(myvars)) %>% 
  mutate(across(all_of(myvars[1:10]),
                ~replace_na(.x,0))) %>% 
  cor(use = "pairwise.complete.obs") %>% as.matrix()
colnames(cormat_journals) <- rownames(cormat_journals) <- myvarnames
maxval=0.5
col_fun=circlize::colorRamp2(breaks = c(0,0.5,1), colors = c("white",myCols[[2]],myCols[[1]]))
split=c(rep("Altmetrics",8),rep("Citation \ndata",4))
heatmap_journals <- ComplexHeatmap::Heatmap(cormat_journals,name = "Correlation",
                                             column_title = "Journal articles",
                                             column_title_gp = gpar(fontsize=15,fontface="bold"),
                                             
                                             # cluster_columns = F,
                                             split = split,
                                             column_split = split, row_split=split,cluster_rows = F,cluster_columns = F,
                                             rect_gp = gpar(col="white", lwd=1),
                                             # heatmap_legend_param = list(heatmap_legend_side="bottom"),
                                             
                                             cell_fun = function(j,i,x,y,width, height, fill){
                                               grid.text(sprintf("%.2f", cormat_journals[i,j]), x, y,
                                                         gp=gpar(fontsize=10,
                                                                 col = 
                                                                   ifelse(cormat_journals[i,j]<0 |
                                                                            cormat_journals[i,j] == 0,
                                                                          "red",  
                                                                          ifelse(cormat_journals[i,j]>0.25 |
                                                                                   cormat_journals[i,j] == 0,
                                                                                 "white","black"))))
                                             },
                                             col = col_fun)

heatmap_journals


png(paste0(figpath,"heatmap_attention_correlation_preprints.png"), width = 9,
    height = 8, units = "in", res = 300)
heatmap_preprints
dev.off()
png(paste0(figpath,"heatmap_attention_correlation_journals.png"), width = 9,
    height = 8, units = "in", res = 300)
heatmap_journals
dev.off()




