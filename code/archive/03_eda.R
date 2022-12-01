rm(list=ls())
source("E:/Group/functions/load_packages.R")
source("E:/Group/functions/cats_and_covs.R")

load_packages(c("dplyr", "OverReact","tidyverse","tm","wordcloud","ggplot2","ggthemes", "ComplexHeatmap",
                "text2map","tidytext","focus","gridExtra", "future.apply", "lubridate","ggbeeswarm",
                "foreach","doParallel","catboost","ggnetwork","ICoLour","ppcor","zoo","scales","ggpubr",
                "tidytext", "quanteda", "widyr", "igraph", "ggraph", "patchwork"))
outpath <- "E:/Group/react2_study5/report_phases_combined/projects/covid_lit_analysis/output/eda/"
figpath <- "E:/Group/react2_study5/report_phases_combined/projects/covid_lit_analysis/plots/eda/"

# no scientific notation
options(scipen = 999)

# Load data ---------------------------------------------------------------

# Save
dat <- readRDS("data/dat_main.rds")
# titles_dtm_tidy_wide <- readRDS("data/dat_text.rds")


# Relationship between altmetrics and citations ---------------------------

# articles published over time
p_pub_over_time <- dat %>% 
  mutate(year=lubridate::year(date),
         week=lubridate::week(date)) %>% 
  group_by(date) %>% 
  summarise(n=n()) %>% 
  mutate(roll_n=zoo::rollmean(n,30, align="right", fill = NA, na.pad = F)) %>% 
  filter(date > as.Date("2020-03-30"), date < as.Date("2021-09-01")) %>% 
  ggplot(aes(x=date, y=roll_n)) + 
  geom_point(size=0.3) +
  geom_line()+
  theme_bw() +
  scale_x_date(labels = date_format("%b %Y"))+
  labs(x="Date", y="Number of articles published (30-day average)") +theme_adjust
OverReact::saveREACTplot(p = p_pub_over_time,figpath = figpath,filename = "articles_published_over_time",
                         width = 6,height = 3.5
)

# articles published over time
p_pub_over_time_preprint <- dat %>% 
  mutate(year=lubridate::year(date),
         week=lubridate::week(date)) %>% 
  filter(date != as.Date("2021-01-01")) %>% 
  group_by(preprint,date) %>% 
  summarise(n=n()) %>% 
  mutate(roll_n=zoo::rollmean(n,30, align="right", fill = NA, na.pad = F)) %>% 
  filter(date > as.Date("2020-03-30"), date < as.Date("2021-09-01")) %>% 
  ggplot(aes(x=date, y=roll_n)) + 
  geom_point(size=0.3) +
  geom_line()+
  theme_bw() +
  scale_x_date(labels = date_format("%b %Y"))+
  facet_wrap(.~preprint, ncol=1, scales="free_y") +
  labs(x="Date", y="Number of articles published (30-day average)") +
  theme_adjust
p_pub_over_time_preprint
OverReact::saveREACTplot(p = p_pub_over_time_preprint,figpath = figpath,filename = "articles_published_over_time_preprint_facet",
                         width = 6,height = 5
)


# Preprints published over time, by publication status --------------------


# articles published over time
p_pub_over_time_preprint_published <- dat %>% 
  filter(date != as.Date("2021-01-01"), preprint == "Preprint") %>% 
  mutate(year=lubridate::year(date),
         week=lubridate::week(date)) %>% 
  group_by(date,published) %>% 
  summarise(n=n()) %>% 
  group_by(published) %>% 
  arrange(published, date) %>% 
  mutate(roll_n=zoo::rollmean(n,30, align="right", fill = NA, na.pad = F)) %>% 
  filter(date > as.Date("2020-03-30"), date < as.Date("2021-09-01")) %>% 
  mutate(published = case_when(published ==1 ~ "Published in journal",
                               T~"Not yet published")) %>% 
  ggplot(aes(x=date, y=roll_n, fill = factor(published),col = factor(published))) + 
  geom_bar(position = "dodge", stat="identity")+
  theme_bw() +
  scale_fill_manual(values = myCols[c(5,2)]) +
  scale_color_manual(values = myCols[c(5,2)]) +
  scale_x_date(labels = date_format("%b %Y"))+
  labs(x="Date", y="Number of preprints published (30-day average)", col ="", fill ="") +theme_adjust
p_pub_over_time_preprint_published
OverReact::saveREACTplot(p = p_pub_over_time_preprint_published,figpath = figpath,
                         filename = "preprints_published_over_time_by_pub_status",
                         width = 8,height = 4.5
)






# Media attenton over time ------------------------------------------------



# articles published over time
p_alt_over_time_mean_preprint <- dat %>% 
  mutate(year=lubridate::year(date),
         week=lubridate::week(date)) %>% 
  filter(date != as.Date("2021-01-01")) %>% 
  group_by(preprint,date) %>% 
  summarise(n=n(),
            mean_alt = mean(altmetrics.score, na.rm=T)) %>% 
  mutate(roll_mean_alt=zoo::rollmean(mean_alt,30, align="right", fill = NA, na.pad = F)) %>% 
  filter(date > as.Date("2020-03-01"), date < as.Date("2021-09-01")) %>% 
  ggplot(aes(x=date, y=roll_mean_alt)) + 
  geom_point(size=0.3) +
  geom_line()+
  theme_bw() +
  scale_x_date(labels = date_format("%b %Y"))+
  facet_wrap(.~preprint, ncol=1, scales="free_y") +
  labs(x="Date", y="Average media and social media attention per paper (Altmetrics score)") +
  theme_adjust
p_alt_over_time_mean_preprint
OverReact::saveREACTplot(p = p_alt_over_time_mean_preprint,figpath = figpath,filename = "media_attention_over_time_average_preprint_facet",
                         width = 6,height = 5
)

# articles published over time
p_alt_over_time_preprint <- dat %>% 
  mutate(year=lubridate::year(date),
         week=lubridate::week(date)) %>% 
  filter(date != as.Date("2021-01-01")) %>% 
  group_by(preprint,date) %>% 
  summarise(n=n(),
            mean_alt = sum(altmetrics.score, na.rm=T)) %>% 
  mutate(roll_mean_alt=zoo::rollmean(mean_alt,30, align="right", fill = NA, na.pad = F)) %>% 
  filter(date > as.Date("2020-03-01"), date < as.Date("2021-09-01")) %>% 
  ggplot(aes(x=date, y=roll_mean_alt)) + 
  geom_point(size=0.3) +
  geom_line()+
  theme_bw() +
  scale_x_date(labels = date_format("%b %Y"))+
  facet_wrap(.~preprint, ncol=1, scales="free_y") +
  labs(x="Date", y="Total media and social media attention (Altmetrics score)") +
  theme_adjust
p_alt_over_time_preprint
OverReact::saveREACTplot(p = p_alt_over_time_preprint,figpath = figpath,filename = "media_attention_over_time_preprint_facet",
                         width = 6,height = 5
)



# Preprints ---------------------------------------------------------------

dat_preprint <- dat %>% filter(preprint=="Preprint")
dat_preprint$pub_in_journal <- case_when(is.na(dat_preprint$resulting_publication_doi) ~ "Not published",
                                         TRUE ~ "Published")
# remove duplicates
dat_preprint <- dat_preprint[!duplicated(dat_preprint$id),]


p_altmetric <- dat_preprint %>% 
  # filter(date <=as.Date("2021-01-01")) %>% 
  ggplot(aes(col=pub_in_journal,x=altmetrics.score, y=pub_in_journal)) +
  geom_boxplot() +
  # geom_quasirandom(alpha=0.5) +
  scale_x_log10() +
  theme_bw() +
  scale_color_imperial(palette = "blue") +
  stat_compare_means() +
  theme_adjust +
  labs(col="", y="", x="Altmetrics score") +
  theme(legend.position = "none")

OverReact::saveREACTplot(p = p_altmetric,figpath = figpath,
                         filename = "Preprint_altmetrics_boxplot_by_published_status",
                         width = 6,height = 3
)


p_preprint_cites_vs_altmetrics<- dat_preprint %>% 
  filter(date <=as.Date("2021-01-01")) %>%
  ggplot(aes(y=cites_per_day,x=altmetrics.score, size= citations_count)) +
  geom_point(alpha=0.3) +
  scale_x_log10() +
  scale_y_log10() +
  facet_wrap(.~preprint) +
  theme_bw() +
  geom_smooth(method ="lm",show.legend = F) +
  theme_adjust +
  labs(x="Altmetrics score", y= "Average citations per day", size = "Total citations")


# save
OverReact::saveREACTplot(p = p_preprint_cites_vs_altmetrics,figpath = figpath,
                         filename = "preprint_citesPerDay_vs_altmetrics",
                         width = 6,height = 6
)

# in all including non-preprints
p_cites_vs_altmetrics<- dat %>% 
  filter(date <=as.Date("2021-01-01")) %>%
  ggplot(aes(y=cites_per_day,x=altmetrics.score, size= citations_count)) +
  geom_point(alpha=0.3) +
  scale_x_log10() +
  scale_y_log10() +
  facet_wrap(.~preprint) +
  theme_bw() +
  geom_smooth(method ="lm",show.legend = F) +
  theme_adjust +
  labs(x="Altmetrics score", y= "Average citations per day", size = "Total citations")

# save
OverReact::saveREACTplot(p = p_cites_vs_altmetrics,figpath = figpath,
                         filename = "citesPerDay_vs_altmetrics",
                         width = 9,height = 6
)



# Tracking use of specific terms over time --------------------------------
alt_mean=dat$altmetrics.score %>%  mean(na.rm=T)
cite_mean=dat$citations_count %>%  mean(na.rm=T)
cpd_mean=dat$cites_per_day %>%  mean(na.rm=T)
hcq_summary_dat$week %>% min()

# Get summary data about hcq
hcq_summary_dat <- dat %>% 
  filter(preprint!="Preprint") %>% 
  
  mutate(week=lubridate::week(date),
               week_name = case_when(lubridate::year(date) == 2020 ~as.Date("2020-01-01") + 7*(week-1),
                                     T~ as.Date("2021-01-01") + 7*(week-1)
                 )) %>% 
  filter(date >= as.Date("2020-03-01"), date < as.Date("2021-09-01")) %>% 
  group_by( hcq = ifelse(hydroxychloroquine >0,"HCQ in title", "HCQ not in title"), week_name)  %>% 
  summarise(n=n(),
            mean_cites = mean(citations_count, na.rm=T),
            mean_cites_per_day = mean(cites_per_day, na.rm=T),
            mean_altmetrics = mean(altmetrics.score, na.rm=T),
            adj_mean_cites = mean_cites/cite_mean,
            adj_mean_cpd = mean_cites_per_day/cpd_mean,
            adj_mean_altmetrics = mean_altmetrics/alt_mean,
            media_attention_scientific_attention_ratio =mean_altmetrics/cpd_mean
  ) 


# Get summary data about hcq
hcq_summary_dat_pp <- dat %>% 
  filter(preprint=="Preprint") %>% 
  mutate(week=lubridate::week(date),
         week_name = case_when(lubridate::year(date) == 2020 ~as.Date("2020-01-01") + 7*(week-1),
                               T~ as.Date("2021-01-01") + 7*(week-1)
         )) %>% 
  filter(date >= as.Date("2020-03-01"), date < as.Date("2021-09-01")) %>% 
  group_by( hcq = ifelse(hydroxychloroquine >0,"HCQ in title", "HCQ not in title"), week_name)  %>% 
  summarise(n=n(),
            mean_cites = mean(citations_count, na.rm=T),
            mean_cites_per_day = mean(cites_per_day, na.rm=T),
            mean_altmetrics = mean(altmetrics.score, na.rm=T),
            adj_mean_cites = mean_cites/cite_mean,
            adj_mean_cpd = mean_cites_per_day/cpd_mean,
            adj_mean_altmetrics = mean_altmetrics/alt_mean,
            media_attention_scientific_attention_ratio =mean_altmetrics/cpd_mean
  ) 

# PLot altmetrics
p_hcq_alt_preprint <- hcq_summary_dat_pp %>% 
  ggplot(aes(x=week_name,y=adj_mean_altmetrics, col = hcq)) +
  geom_line() +
  theme_adjust +
  theme_bw() +
  scale_colour_manual(values = myCols[c(3,1)]) +
  scale_x_date(date_labels = "%b %Y",breaks = "3 month") +
  labs(x="Date", y="Media/social media attention score", col = "")
  

p_hcq_alt / p_hcq_alt_preprint

# save
OverReact::saveREACTplot(p = p_hcq_alt,figpath = figpath,
                         filename = "hydroxychloroquine_altmetrics_over_timw",
                         width = 9,height = 6
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
            total_media = sum(altmetrics.score, na.rm=T)) %>% 
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
            total_media = sum(altmetrics.score, na.rm=T)) %>% 
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
            total_media = sum(altmetrics.score, na.rm=T)) %>% 
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
            total_media = sum(altmetrics.score, na.rm=T)) %>% 
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

dat_top_altmetrics <- dat_preprint[order(dat_preprint$altmetrics.score,na.last = T,decreasing = T),][1:20,]
OverReact::saveREACTtable(tab = dat_top_altmetrics,outpath = outpath,filename = "top_20_preprints_altmetrics",save_rds = F)

# by citations
dat_top_cites<- dat_preprint[order(dat_preprint$citations_count,na.last = T,decreasing = T),][1:20,1:14]
OverReact::saveREACTtable(tab = dat_top_cites,outpath = outpath,filename = "top_20_preprints_cites",save_rds = F)


dat$myocarditis %>% table()
dat %>% 
  # filter(preprint=="Preprint") %>% 
  group_by(myocarditis =myocarditis>0,preprint) %>% 
  summarise(n=n(),
            mean_alt=mean(altmetrics.score, na.rm=T))

table(dat$preprint)
dat_top_altmetrics$myocarditis %>% table()


# Correlation analysis ----------------------------------------------------
dat_altmetrics <- readRDS("data/altmetric_res_all.rds")
dat_altmetrics <- dat_altmetrics %>% left_join(dat[,1:14], by = c("altmetric_id"="altmetrics.id","doi"))
# convert to numberic
dat_altmetrics[,c(8:31,36:42)] <- dat_altmetrics[,c(8:31,36:42)] %>% mutate_all(.funs = as.numeric)
# replace NA with 0
dat_altmetrics[,c(8:31,36:42)][is.na(dat_altmetrics[,c(8:31,36:42)])] <- 0

names(dat_altmetrics)
myvars <- c("altmetrics.score","cited_by_tweeters_count","cited_by_fbwalls_count",
            "cited_by_msm_count",
            "cohorts.sci","cohorts.pub",
            "cites_per_day","citations_count")
myvarnames=c("All media and social media",
             "Twitter","Facebook","Mainstream media",
             "Social media (scientists)",
             "Social media (general public)",
             "Citations", "Total citations count"
             )

cormat_preprints <- dat_altmetrics %>% filter(preprint=="Preprint") %>% 
  dplyr::select(all_of(myvars)) %>% cor() %>% as.matrix()
colnames(cormat_preprints) <- rownames(cormat_preprints) <- myvarnames
maxval=0.5
col_fun=circlize::colorRamp2(breaks = c(0,0.5,1), colors = c("white",myCols[[2]],myCols[[1]]))

heatmap_preprints <- ComplexHeatmap::Heatmap(cormat_preprints,name = "Correlation",
                                             column_title = "Preprints",
                                             column_title_gp = gpar(fontsize=15,fontface="bold"),
                    
                                             # cluster_columns = F,
                        split = 2,column_split = 2, row_split=2,
                        
                        cell_fun = function(j,i,x,y,width, height, fill){
                          grid.text(sprintf("%.2f", cormat_preprints[i,j]), x, y,
                                    gp=gpar(fontsize=10,
                                            col = 
                                              ifelse(cormat_preprints[i,j]>0.25 |
                                                       cormat_preprints[i,j] == 0,
                                                     "white",  "black")))
                        },
                        col = col_fun)
  



cormat_journal <- dat_altmetrics %>% filter(preprint!="Preprint") %>% 
  dplyr::select(all_of(myvars)) %>% cor() %>% as.matrix()
colnames(cormat_journal) <- rownames(cormat_journal) <- myvarnames

heatmap_journals <- ComplexHeatmap::Heatmap(cormat_journal,name = "Correlation",
                                            column_title = "Journal articles",
                                            column_title_gp = gpar(fontsize=15,fontface="bold"),
                        # cluster_rows = F,
                        # cluster_columns = F,
                        split = 2,column_split = 2, row_split=2,
                        
                        cell_fun = function(j,i,x,y,width, height, fill){
                          grid.text(sprintf("%.2f", cormat_journal[i,j]), x, y,
                                    gp=gpar(fontsize=10,
                                            col = 
                                              ifelse(cormat_journal[i,j]>0.25 |
                                                       cormat_journal[i,j] == 0,
                                                     "white",  "black")))
                        },
                        col = col_fun)


combined_heatmap <- draw((heatmap_preprints+heatmap_journals), ht_gap =unit(1.3,"cm"))


png(paste0(figpath,"heatmap_attention_correlation.jpg"), width = 12,
    height = 7, units = "in", res = 300)
combined_heatmap
dev.off()



# Word frequency plots ----------------------------------------------------

word_freqs <- colSums(dat[,15:ncol(dat)])
word_freqs <- as.data.frame(word_freqs)
word_freqs$word = rownames(word_freqs)

p_freq <- word_freqs %>% 
  arrange(desc(word_freqs)) %>% 
  slice_head(n = 20) %>% 
  ggplot(aes(x=reorder(word, word_freqs), y=word_freqs)) + geom_col() +
  theme_bw() + theme_adjust +
  coord_flip() +
  labs(x="", y="Number of papers with word in title")
# save
OverReact::saveREACTplot(p = p_freq,figpath = figpath,
                         filename = "word_freq",
                         width = 6,height = 5
)
