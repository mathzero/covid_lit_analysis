
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
package.list <- c("knitr","dplyr","tidyr",
                  "ggplot2","gdata","ggsci", "RColorBrewer", "tidyverse", "lubridate", 
                  "ComplexHeatmap",
                  "ggthemes", "gridExtra","scales","ggpubr",
                  "patchwork", "OverReact")
load_packages(package.list)
# pacman::p_load(package.list)

# create subfolder
createMySubfolder(subfolderName = "eda")

# no scientific notation
options(scipen = 999)

# import fonts
# extrafont::font_import(prompt =F )

# Load data ---------------------------------------------------------------


# Save
dat <- readRDS("data/bq_exports_mar_2023/clean/dat_main.rds")
# concepts <- read_csv("data/bq-results-20221124-concepts.csv")
# concepts_wide <- readRDS("data/concepts_wide.rds")

dat <- dat %>% 
  # rename(altmetrics_score=score) %>% 
  mutate(
  # abstract_available = case_when(is.na(abstract_preferred) ~ "Yes",
  #                                                    T ~ "No"),
                      dummy="nobs",
                      year_factor=as.factor(year)) %>% 
  mutate(across(c("citations_count","altmetrics_score","cited_by_tweeters_count","cited_by_msm_count"),
                ~replace_na(.x,0)))


rowvar_list=c("dummy","year_factor","citations_count","published_in_journal",
              "altmetrics_score","cited_by_tweeters_count","cited_by_msm_count")
# cov_names=as.list(rowvar_list)
cov_names=list("Papers published","Year","Number of citations",
               "Published in journal","Altmetrics score","Number of tweets","Number of media mentions")
names(cov_names)=rowvar_list
tab1 <- OverReact::crossTabMulti(dat=dat,rowvar_list = rowvar_list,colvar = "type",
                                 cov_names = cov_names,include_percentages = T,
                                 rowwise_precentages = F,confint = F,comma_thousands = T)

OverReact::saveREACTtable(tab = tab1,outpath = outpath,filename = "table_one_covid")


# Identify differences in papers that were preprinted ---------------------

# get dois of papers that were preprinted
preprinted_papers_subs_pub_dois=dat$resulting_publication_doi[dat$type=="preprint"]
preprinted_papers_subs_pub_dois <- preprinted_papers_subs_pub_dois[!is.na(preprinted_papers_subs_pub_dois)]

dat <- dat %>% mutate(paper_preprinted=case_when(doi%in%preprinted_papers_subs_pub_dois~ "Preprinted",
                                                 T ~ "Not preprinted"))

tab2 <- OverReact::crossTabMulti(dat=dat %>% filter(type=="article"),
                                 rowvar_list = setdiff(rowvar_list,"published_in_journal"),
                                 colvar = "paper_preprinted",
                                 cov_names = cov_names,include_percentages = T,
                                 rowwise_precentages = F,confint = F,comma_thousands = T)





# Papers published over time ----------------------------------------------

# tinker theme
mytheme= theme(plot.title =element_text(size=13, face="bold"), 
               plot.subtitle =element_text(size=10))


repdates=c(as.Date("2019-01-01"),as.Date("2020-01-01"),as.Date("2021-01-01"),as.Date("2022-01-01"))
set.seed(123)


p_pub_over_time <- dat %>% 
  
  mutate(
    randnum=sample(c(rep(1,10),1:365),size =nrow(.),replace = T),
    date=case_when(date%in%repdates ~ date+randnum,
                   T ~ date),
    # date=as.Date(date_normal),
    year=lubridate::year(date),
    week=lubridate::week(date),
    ym=format_ISO8601(date,precision="ym")
  ) %>%
  filter(date<as.Date("2023-01-01")) %>%
  # filter(date>=as.Date("2020-01-02")) %>%
  # filter(date != as.Date("2021-01-01"),date != as.Date("2022-01-01")) %>%
  group_by(ym,type) %>% 
  summarise(n=n()) %>% 
  filter(!is.na(n)) %>% 
  group_by(type) %>% 
  mutate(roll_n=n) %>% 
  ungroup() %>% 
  ggplot(aes(x=ym, y=roll_n, col=type,group=type)) + 
  geom_point(size=0.6) +
  geom_line(size=0.2)+
  labs(x="Month", y="Number of papers published", col="", 
       title="Papers published per month",
       subtitle = "January 2020 - December 2022") +
  OverReact::scale_color_imperial(palette = "cool")+
  OverReact::theme_react()+
  theme(legend.position = c(0.1,0.9),
        axis.text.x = element_text(angle=70,vjust=1,hjust=1)) +
  mytheme

p_pub_over_time

OverReact::saveREACTplot(p = p_pub_over_time,figpath = figpath,filename = "articles_published_over_time",
                         width = 6,height = 5
)


### Media attention over time


# Media attenton over time ------------------------------------------------


# articles published over time
p_alt_over_time_type <- dat %>% 
  
  mutate(
    randnum=sample(c(rep(1,10),1:365),size =nrow(.),replace = T),
    date=case_when(date%in%repdates ~ date+randnum,
                   T ~ date),
    # date=as.Date(date_normal),
    year=lubridate::year(date),
    week=lubridate::week(date),
    ym=format_ISO8601(date,precision="ym")
  ) %>%
  filter(date<as.Date("2023-01-01")) %>%
  # filter(date>=as.Date("2020-01-02")) %>%
  # filter(date != as.Date("2021-01-01"),date != as.Date("2022-01-01")) %>%
  # filter(date != as.Date("2021-01-01")) %>% 
  group_by(type,ym) %>% 
  summarise(n=n(),
            mean_alt = sum(altmetrics_score, na.rm=T)) %>% 
  mutate(roll_mean_alt=zoo::rollmean(mean_alt,30, align="right", fill = "extend", na.pad = F)) %>% 
  # filter(date > as.Date("2020-01-01"), date < as.Date("2022-11-24")) %>% 
  ggplot(aes(x=ym, y=mean_alt,col=type,group=type)) + 
  geom_point(size=0.6) +
  geom_line(size=0.2)+  
  labs(x="Date", y="Total media and social media attention \n(Altmetrics score)", 
       title="Total media attention", subtitle = "January 2020 - December 2022",
       col="") +
  OverReact::scale_color_imperial(palette = "cool")+
  OverReact::theme_react()+
  theme(legend.position = c(0.9,0.9),axis.text.x = element_text(angle=70,vjust=1,hjust=1))  +
  mytheme

p_alt_over_time_type
OverReact::saveREACTplot(p = p_alt_over_time_type,figpath = figpath,filename = "media_attention_over_time_type_facet",
                         width = 6,height = 5
)

# articles published over time
p_alt_over_time_mean_type <- dat %>% 
  
  
  mutate(
    randnum=sample(c(rep(1,10),1:365),size =nrow(.),replace = T),
    date=case_when(date%in%repdates ~ date+randnum,
                   T ~ date),
    # date=as.Date(date_normal),
    year=lubridate::year(date),
    week=lubridate::week(date),
    ym=format_ISO8601(date,precision="ym")
  ) %>%
  filter(date<as.Date("2023-01-01")) %>%
  # filter(date>=as.Date("2020-01-02")) %>%
  # filter(date != as.Date("2021-01-01"),date != as.Date("2022-01-01")) %>%
  group_by(type,ym) %>% 
  summarise(n=n(),
            mean_alt = mean(altmetrics_score, na.rm=T)) %>% 
  # mutate(roll_mean_alt=zoo::rollmean(mean_alt,30, align="right", fill = "extend", na.pad = F)) %>% 
  # filter(date > as.Date("2020-01-01"), date < as.Date("2022-11-24")) %>% 
  ggplot(aes(x=ym, y=mean_alt,col=type,group=type)) + 
  geom_point(size=0.6) +
  geom_line(size=0.2)+  
  labs(x="Date", y="Average media and social media attention \n(Altmetrics score)", 
       title="Average media attention per paper",  subtitle = "January 2020 - December 2022",
       col="") +
  OverReact::scale_color_imperial(palette = "cool")+
  OverReact::theme_react()+
  theme(legend.position = c(0.9,0.9),axis.text.x = element_text(angle=70,vjust=1,hjust=1))  +
  mytheme

p_alt_over_time_mean_type

OverReact::saveREACTplot(p = p_alt_over_time_mean_type,figpath = figpath,
                         filename = "media_attention_over_time_average_type_facet",
                         width = 6,height = 5
)


### Combine plots

p_alt_comb=p_pub_over_time/p_alt_over_time_type/p_alt_over_time_mean_type + 
  plot_layout(guides="collect") & theme(legend.position =c(0.1,0.9))
p_alt_comb
OverReact::saveREACTplot(p = p_alt_comb,figpath = figpath,filename = "media_attention_over_time_three_panel",
                         width = 12,height = 14
)














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




OverReact::savePrettyExcelWorkbook(listOfTables = list(table_one=tab1,
                                                       top_categories=summary_category,
                                                       top_journals=summary_journal),
                                   workbookName = "summary_tables",outpath = outpath,
                                   noDecimalsColumns = c("n_papers","n_journals"))
OverReact::saveREACTtable(tab = summary_category,outpath = outpath,filename = "journal_subject_metrics_summary")
OverReact::saveREACTtable(tab = summary_journal,outpath = outpath,filename = "journal_metrics_summary")
# OverReact::saveREACTtable(tab = summary_concept,outpath = outpath,filename = "concept_metrics_summary")




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

dat$cited_by_wikipedia_count
myvars <- c("altmetrics_score","cited_by_msm_count",
            "cited_by_tweeters_count","cited_by_fbwalls_count",
           "cited_by_videos_count","cited_by_wikipedia_count", "cited_by_policies_count",
            "cohorts_sci","cohorts_doc","cohorts_com","cohorts_pub",
            "cites_per_day","citations_count",
            # "subs_pub_sjr",
            "published_in_journal",
            "subs_pub_journal_if_2022"
)
myvarnames=c("All media and social media","Mainstream media",
             "Twitter","Facebook","Youtube","Wikipedia","Policy documents",
             "Social media (scientists)","Social media (medics)",
             "Social media (science communicators)",
             "Social media (general public)",
             "Citation rate", "Total citations count",
             "Published in journal","Impact factor of publishing journal"
             # "Impact score of publishing journal",
             
)
dat$published_in_journal
cormat_preprints <- dat %>% 
  filter(type=="preprint", year==2020) %>% 
  dplyr::select(all_of(myvars)) %>% 
  mutate(across(all_of(myvars[1:13]),
                ~replace_na(.x,0))) %>% 
  cor(use = "pairwise.complete.obs") %>% 
  as.matrix()


colnames(cormat_preprints) <- rownames(cormat_preprints) <- myvarnames
maxval=0.5
col_fun=circlize::colorRamp2(breaks = c(0,0.5,1), colors = c("white",myCols[[2]],myCols[[1]]))
split=c(rep("Altmetrics",11),rep("Citation \ndata",2), rep("Subsequent \npublication",2))

heatmap_preprints <- ComplexHeatmap::Heatmap(cormat_preprints,name = "Correlation",
                                             column_title = "Preprints",
                                             column_title_gp = gpar(fontsize=15,fontface="bold"),
                                             
                                             # cluster_columns = F,
                                             split = split,
                                             column_split = split, row_split=split,
                                             cluster_rows = F,cluster_columns = F,
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
            "cited_by_videos_count","cited_by_wikipedia_count", "cited_by_policies_count",
            
            "cohorts_sci","cohorts_doc","cohorts_com","cohorts_pub",
            "cites_per_day","citations_count",
            "journal_if_2022","journal_jci"
)
myvarnames=c("All media and social media","Mainstream media",
             "Twitter","Facebook","Youtube","Wikipedia","Policy documents",
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
split=c(rep("Altmetrics",11),rep("Citation \ndata",4))
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



# Correlation analysis scatter --------------------------------------------



cc=cor.test(dat$altmetrics_score,dat$citations_count,method="pearson",conf.level = 0.95)
cor=as.numeric(cc$estimate)
lower=as.numeric(cc$conf.int[1])
upper=as.numeric(cc$conf.int[2])













# Top papers by wikipedia, youtube policy, etc ----------------------------


# wikipedia
top10_wiki <- dat %>% arrange(-cited_by_wikipedia_count) %>% 
  slice_head(n = 10) %>% select(title_preferred,date,journal_title,
                               cited_by_wikipedia_count,
                               altmetrics_score,citations_count,doi)

# youtube
top10_youtube <- dat %>% arrange(-cited_by_videos_count) %>% 
  slice_head(n = 10) %>% select(title_preferred,date,journal_title,
                                cited_by_videos_count,
                                altmetrics_score,citations_count,doi)


# youtube
top10_policy <- dat %>% arrange(-cited_by_policies_count) %>% 
  slice_head(n = 10) %>% select(title_preferred,date,journal_title,
                                cited_by_policies_count,
                                altmetrics_score,citations_count,doi)

# altmetrics
top10_altmetrics <- dat %>% arrange(-altmetrics_score) %>% 
  slice_head(n = 10) %>% select(title_preferred,date,journal_title,
                                altmetrics_score,citations_count,doi)


# citations
top10_citations <- dat %>% arrange(-citations_count) %>% 
  slice_head(n = 10) %>% select(title_preferred,date,journal_title,
                                altmetrics_score,citations_count,doi)




# citations preprint
top10_citations_preprint <- dat %>% filter(type=="preprint") %>% 
  arrange(-citations_count) %>% 
  slice_head(n = 10) %>% select(title_preferred,date,journal_title,
                                altmetrics_score,citations_count,doi)


# altmetrics preprint
top10_altmetrics_preprint <- dat %>% filter(type=="preprint") %>% 
  arrange(-altmetrics_score) %>% 
  slice_head(n = 10) %>% select(title_preferred,date,journal_title,
                                altmetrics_score,citations_count,doi)




# Save all tables
OverReact::savePrettyExcelWorkbook(listOfTables = list(tab1=tab1,
                                                       prepreinted_compare=tab2,
                                                       top10_altmetrics=top10_altmetrics,
                                                       top10_citations=top10_citations,
                                                       top10_wiki=top10_wiki,
                                                       top10_youtube=top10_youtube,
                                                       top10_policy=top10_policy,
                                                       top10_citations_preprint=top10_citations_preprint,
                                                       top10_altmetrics_preprint=top10_altmetrics_preprint
                                                       ),
                                   workbookName = "tables_covid_papers",outpath =outpath )



# Plot correlation between altmetrics and citation rates ------------------

doi_alt=top10_altmetrics$doi
doi_cite=top10_citations$doi
doi_both=intersect(doi_alt,doi_cite)
doi_either=union(doi_alt,doi_cite)

dat_plot=dat %>% 
  mutate(top_paper=factor(case_when(doi%in%doi_both ~ "Top 10 in both Altmetrics \nand citations",
                                    doi%in%doi_cite ~ "Top 10 in citations",
                                    doi%in%doi_alt ~ "Top 10 in Altmetrics",
                             T ~ "Not in either top 10"),
                          levels = c("Top 10 in both Altmetrics \nand citations",
                                     "Top 10 in citations", "Top 10 in Altmetrics",
                                     "Not in either top 10")
  ),
  label=case_when(doi %in% doi_either~ title_preferred,
                  T ~ NA_character_)) 

# data for highlight
dat_plot_hl = dat_plot %>% 
  filter(doi%in%doi_both &type=="article",
         citations_count>1000)

# plot
p_cor=dat_plot %>% 
  filter(citations_count>0 & altmetrics_score>0) %>% # only papers with citations and altmetrics
  ggplot(aes(x=citations_count,y=altmetrics_score, col=top_paper,
             alpha=top_paper,size=top_paper,shape=top_paper)) +
  geom_point()+
  scale_alpha_manual(values = c(1,1,1,0.5))+
  scale_colour_manual(values = c("firebrick1","orange","orange","grey40"))+
  scale_size_manual(values = c(1,1,1,0.8))+
  scale_shape_manual(values = c(1,4,6,16))+
  OverReact::theme_react() +
  ggrepel::geom_text_repel(aes(label=stringr::str_wrap(label,width = 20)),data=dat_plot_hl,
                           inherit.aes = T,size=2,col="black",show.legend = F)+
  labs(x="Citations", y="Altmetrics score", col="",shape="",size="",alpha="") +
  theme(legend.position = c(0.8,0.85),
        # panel.background  = element_rect(fill = "white"),
        plot.background  = element_rect(fill = "white"))

p_cor

# save
OverReact::saveREACTplot(p = p_cor,figpath = figpath,
                         filename = "corr_cites_altmetrics_scatter",
                         width = 5,height =5,savePDF = T,filetypes=c("jpg","png"))


