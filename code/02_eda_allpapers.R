
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
# package.list <- c("knitr","dplyr","tidyr","slider",
#                   "ggplot2","gdata","ggsci", "RColorBrewer", "tidyverse", "lubridate", 
#                   "ComplexHeatmap","survminer","survival",
#                   "ggthemes", "gridExtra","scales","ggpubr",
#                   "patchwork", "OverReact")
# load_packages(package.list)
pacman::p_load(knitr,dplyr,tidyr,slider,
                 ggplot2,gdata,ggsci, RColorBrewer, tidyverse, lubridate, 
                 ComplexHeatmap,survminer,survival,
                 ggthemes, gridExtra,scales,ggpubr,
                 patchwork, OverReact)

# create subfolder
createMySubfolder(subfolderName = "eda_allpapers")

# no scientific notation
options(scipen = 999)

# import fonts
# extrafont::font_import(prompt =F )

# Load data ---------------------------------------------------------------


# define data path
datapath="data/bq_exports_mar_2023/clean/"
dat=readRDS(paste0(datapath,"dat_all.rds"))
# titles=readRDS(paste0(datapath,"titles_all.rds"))
dat_cov=readRDS(paste0(datapath,"dat_main.rds"))
dat <- dat %>% 
  mutate(in_covid_data=case_when(id%in%dat_cov$id ~ 1,
                                 T ~ 0))


# wrangle dat
dat <- dat %>% mutate(dummy="nobs",
                      year_factor=as.factor(year)) %>% 
  # rename(altmetrics_score=score) %>% 
  mutate(across(c("citations_count","altmetrics_score"),
                ~replace_na(.x,0))) %>% 
  mutate(published=case_when(!is.na(resulting_publication_doi) ~ "Yes",
                             T ~ "No"),
         covid_paper=case_when(in_covid_data==1 ~ "COVID-19 paper",
                               T ~ "Non-COVID-19 paper"),
         era=case_when(year==2019 ~ "2019 (Pre-pandemic)",
                            in_covid_data == 0 ~ "2020-2022 (Non-COVID-19)",
                            T ~ "COVID-19 papers"
                            ),
         type_detail=case_when(in_covid_data==1 & type=="preprint"~"COVID-19 preprint",
                               in_covid_data==1 & type!="preprint"~"COVID-19 journal article",
                               in_covid_data==0 & type=="preprint"~"Preprint",
                               in_covid_data==0 & type!="preprint"~"Journal article"),
         era_type=case_when(in_covid_data==1 & type=="preprint"~"COVID-19 preprint",
                            in_covid_data==1 & type!="preprint"~"COVID-19 journal article",
                            in_covid_data==0 & type=="preprint" & year==2019 ~ "Preprint (2019)",
                            in_covid_data==0 & type!="preprint" & year==2019 ~ "Journal article (2019)",
                            in_covid_data==0 & type=="preprint"~"Non-COVID-19 \nPreprint (2020-2022)",
                            in_covid_data==0 & type!="preprint"~"Non-COVID-19 \nJournal article (2020-2022)",
                            ),
         time_to_pub=as.numeric(subs_pub_date-date),
         published_within_year=case_when(as.numeric(subs_pub_date-date) <= 365 ~ 1,
                                         T ~0),
         published_within_two_years=case_when(as.numeric(subs_pub_date-date) <= 730 ~ 1,
                                         T ~0),
         
         )
# check distributions
table(dat$era_type)
table(dat$type_detail)
table(dat$published_within_year)


# Engineer a feature that finds a ratio of Altmetrics to Citations --------


# create feature
dat <- dat %>% 
  mutate(citations_count_offset=case_when(cites_per_day==0 ~ 0.01,
                                          T ~ cites_per_day),
    rcr_alt_ratio=case_when(altmetrics_score==0 ~ 0,
                            T ~log(altmetrics_score/citations_count_offset)))


# Distributions of citations and altmetrics by COVID-19 status ------------

p_cite=dat %>% 
  mutate(citations_count=citations_count+0.001) %>%
  ggplot(aes(x=era,y=citations_count,col=era))+
  geom_boxplot()+
  scale_y_log10()+
  scale_colour_manual(values = c(imperial_colours[["pool blue"]],
                                 imperial_colours[["dark grey"]],
                                 imperial_colours[["orange"]]))+
  OverReact::theme_mw() +
  facet_wrap(.~type)+
  coord_flip() +
  labs(y="Citations",subtitle="Citations",x="", col="")


p_alt=dat %>% 
  mutate(altmetrics_score=altmetrics_score+0.001) %>%
  ggplot(aes(x=era,y=altmetrics_score,col=era))+
  geom_boxplot()+
  scale_y_log10()+
  scale_colour_manual(values = c(imperial_colours[["pool blue"]],
                                 imperial_colours[["dark grey"]],
                                 imperial_colours[["orange"]]))+
  OverReact::theme_mw() +
  facet_wrap(.~type)+
  coord_flip() +
  labs(y="Altmetrics score",subtitle="Altmetrics score",x="", col="") 

p_cite_alt=(p_cite/p_alt)+plot_layout(guides = "collect")


# save
OverReact::saveREACTplot(p = p_cite_alt,figpath = figpath,
                         filename = "cites_altmetrics_boxplot",
                         width = 9,height = 6,savePDF = T,
                         filetypes = c("jpg","png"))

# Create table one --------------------------------------------------------



rowvar_list=c("dummy","year_factor","published","published_within_year","published_within_two_years",
              "citations_count","relative_citation_ratio","field_citation_ratio",
              "altmetrics_score","rcr_alt_ratio","time_from_preprint_to_pub")
# cov_names=as.list(rowvar_list)
cov_names=list("Papers published","Year","Published in journal",
               "Published in journal within one year","Published in journal within two years",
               "Number of citations",
               "Relative citation ratio","Field citation ratio",
               "Altmetrics score","Altmetrics:citations ratio (logged)",
               "Time from preprint to article publication")
names(cov_names)=rowvar_list
tab1_median <- OverReact::crossTabMulti(dat=dat,rowvar_list = rowvar_list,colvar = "type",
                                 cov_names = cov_names,include_percentages = T,summary_stat = "median",
                                 rowwise_precentages = F,confint = F,comma_thousands = T)

tab1_mean <- OverReact::crossTabMulti(dat=dat,rowvar_list = rowvar_list,colvar = "type",
                                 cov_names = cov_names,include_percentages = T,summary_stat = "mean",
                                 rowwise_precentages = F,confint = F,comma_thousands = T)

tab2_median <- OverReact::crossTabMulti(dat=dat,rowvar_list = rowvar_list,colvar = "era_type",
                                 cov_names = cov_names,include_percentages = T,summary_stat = "median",
                                 rowwise_precentages = F,confint = F,comma_thousands = T)

tab2_mean <- OverReact::crossTabMulti(dat=dat,rowvar_list = rowvar_list,colvar = "era_type",
                                 cov_names = cov_names,include_percentages = T,summary_stat = "mean",
                                 rowwise_precentages = F,confint = F,comma_thousands = T)


tab3_med_median <- OverReact::crossTabMulti(dat=dat %>% filter(journal_category_1%in% c("Medicine, general & internal",
                                                                                 "Public, environmental & occupational health",
                                                                                 "Biochemistry & molecular biology",
                                                                                 "Cell biology") | 
                                                          journal_title %in% c("bioRxiv","medRxiv")),
                                     rowvar_list = rowvar_list,colvar = "era_type",summary_stat = "median",
                                 cov_names = cov_names,include_percentages = T,
                                 rowwise_precentages = F,confint = F,comma_thousands = T)

tab3_med_mean <- OverReact::crossTabMulti(dat=dat %>% filter(journal_category_1%in% c("Medicine, general & internal",
                                                                                 "Public, environmental & occupational health",
                                                                                 "Biochemistry & molecular biology",
                                                                                 "Cell biology") | 
                                                          journal_title %in% c("bioRxiv","medRxiv")),
                                     rowvar_list = rowvar_list,colvar = "era_type",
                                     cov_names = cov_names,include_percentages = T,
                                     rowwise_precentages = F,confint = F,comma_thousands = T)

colorder=c("Variable", "Category", "Preprint (2019)", "Journal article (2019)",
           "Non-COVID-19 \nPreprint (2020-2022)", "Non-COVID-19 \nJournal article (2020-2022)", 
           "COVID-19 preprint", "COVID-19 journal article", 
           "Pooled")

# rearrange to group preprints
tab2_median <- tab2_median %>% select(all_of(colorder))
tab2_mean <- tab2_mean %>% select(all_of(colorder))

tab3_med_median <- tab3_med_median %>% select(all_of(colorder))
tab3_med_mean <- tab3_med_mean %>% select(all_of(colorder))


# # Top 100 papers by whackiness metric --------------------------------------
# 
# # get list of top 200 papers by 'whackiness' rating
# top200_rcr_alt_ratio=dat %>% 
#   arrange(-rcr_alt_ratio) %>% 
#   slice_head(n = 200) %>% 
#   left_join(titles) %>% 
#   select(preferred,rcr_alt_ratio,citations_count,
#          altmetrics_score,relative_citation_ratio,
#          type_detail,covid_paper,
#          everything())
#   
# 
# # export
# OverReact::savePrettyExcelWorkbook(listOfTables = list(top200_rcr_alt_ratio=
#                                                          as.data.frame(top200_rcr_alt_ratio[,1:10])),
#                                    workbookName = "top_200_cite_alt_raito",
#                                    outpath = outpath)

# Get summary of papers by topic ------------------------------------------


summary_category=dat %>% 
  filter(!is.na(journal_category_1),journal_category_1!="") %>% 
  group_by(journal_category_1,era) %>% 
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
  group_by(journal_title,era) %>% 
  summarise(n_papers=n(),
            mean_altmetric_score=round(mean(altmetrics_score, na.rm=T),1),
            mean_citations=round(mean(citations_count, na.rm=T),1),
            journal_IF=round(mean(journal_if_2022, na.rm=T),2)
  ) %>% 
  arrange(-journal_IF) %>% 
  slice_head(n=20)




# Save all tabs -----------------------------------------------------------

# save all tabs
savePrettyExcelWorkbook(listOfTables = list(table_one_by_preprint_mean=tab1_mean,
                                            table_one_by_preprint_median=tab1_median,
                                            table_one_by_pp_covid_mean=tab2_mean,
                                            table_one_by_pp_covid_median=tab2_median,
                                            table_one_med_mean=tab3_med_mean,
                                            table_one_med_median=tab3_med_median,
                                                       summary_by_category=summary_category,
                                                       summary_journal=summary_journal
                                                       ),
                                   workbookName = "summary_tables_allpapers",outpath = outpath,
                                   noDecimalsColumns = c("n_papers","n_journals"))






# 
# # Correlation with altmetrics and citations -------------------------------
# 
# # plot
# p_cor=dat %>% 
#   filter(cites_per_day>0 & altmetrics_score>0) %>% # only papers with citations and altmetrics
#   ggplot(aes(x=cites_per_day,y=altmetrics_score)) +
#     facet_wrap(.~era_type) +
#     geom_point(size=0.5,alpha=0.2)+
#     geom_smooth(method="lm", linewidth=0.8,col="red")+
#     OverReact::theme_react() +
#   scale_x_log10()+
#   scale_y_log10()+
#   ggpmisc::stat_poly_eq(ggpmisc::use_label(c("eq", "R2")))+
#   # ggpubr::stat_cor()
#   labs(x="Citation rate", y="Altmetrics score")
# 
# 
# # save
# OverReact::saveREACTplot(p = p_cor,figpath = figpath,
#                          filename = "corr_cites_altmetrics_scatter_six_panel",
#                          width = 9,height = 5.7,savePDF = T,filetypes=c("jpg","png"))
# 



# Kaplan Meier plots ------------------------------------------------------


# unique(dat$time)
datelimit=as.Date("2022-12-31")


# create variables in data
dat_km <- dat %>%
  filter(type=="preprint") %>% 
  mutate(status=case_when(published=="Yes" ~ 1,
                                       T ~0),
         surv_time = case_when(status==1~ as.numeric(time_to_pub),
                                            T ~ as.numeric(datelimit-date)))



table(dat_km$status,dat_km$era_type,exclude="NONE")


# dat_km$status <- 1

# create survival object
fit <- survfit(Surv(surv_time,status)~era_type,
               data=dat_km) 
names(fit$strata) <- gsub("era_type=","",names(fit$strata))
palette = rev(c( OverReact::imperial_colours[[6]],
                 OverReact::imperial_colours[[11]],
                 OverReact::imperial_colours[[19]]))
palette=rev(as.character(OverReact::imperial_palettes[["short"]]))

# plot
p=ggsurvplot(fit,
             conf.int = T, 
             # fun ="cumhaz",
             risk.table = T,
             palette = palette,
             # test.for.trend=T,
             tables.theme = theme_cleantable(),
             size=0.2,
             legend.title="",
             # pval=T,
             # facet.by="Age",
             data=dat_km,
             # palette = c(myCols[[7]],myCols[[2]]),
             ggtheme = OverReact::theme_react(base_size = 10), 
             fontsize=3)  +
  labs(col="",title = "Kaplan-Meier plot", x="Time (days)", y="1-P(Publication)",
       subtitle = "Comparing time to publication in preprints")

p$plot <- p$plot+guides(fill="none")+
  theme(legend.position = c(0.8,0.2))
p

### Death
OverReact::saveREACTplot(p = p$plot,figpath = figpath,
                         filename = "km_plot_all_preprints",
                         width = 6,height = 5,savePDF = T)


dat_km$journal_title %>% table()

#### As above for medrxiv and biorxiv
dat_km_med=dat_km %>% filter(journal_title%in%c("bioRxiv","medRxiv"))
table(dat_km_med$status,dat_km_med$era_type,exclude="NONE")

# create survival object
fit2 <- survfit(Surv(surv_time,status)~era_type,
               data=dat_km_med) 
names(fit2$strata) <- gsub("era_type=","",names(fit2$strata))


# plot
p2=ggsurvplot(fit2,
             conf.int = T, 
             # fun ="cumhaz",
             risk.table = T,
             palette = palette,
             # test.for.trend=T,
             tables.theme = theme_cleantable(),
             size=0.2,
             legend.title="",
             # pval=T,
             # facet.by="Age",
             data=dat_km_med,
             # palette = c(myCols[[7]],myCols[[2]]),
             ggtheme = OverReact::theme_react(base_size = 10), 
             fontsize=3)  +
  labs(col="",title = "Kaplan-Meier plot", x="Time (days)", y="1-P(Publication)",
       subtitle = "Comparing time to publication in medRxiv/biorXiv preprints")

p2$plot <- p2$plot+guides(fill="none") +
  theme(legend.position = c(0.8,0.2))
p2

### Death
OverReact::saveREACTplot(p = p2$plot,figpath = figpath,
                         filename = "km_plot_med_biorxiv",
                         width =6,height = 5,savePDF = T)


# combine
p_comb=p$plot+p2$plot
p_comb
### Death
OverReact::saveREACTplot(p = p_comb,figpath = figpath,
                         filename = "km_plot_comb",
                         width =11,height = 5,savePDF = T)








# Plots of time to publication --------------------------------------------

dat %>% 
  group_by(era_type) %>% 
  filter(time_to_pub>0) %>% 
  summarise(mean_time_to_pub=mean(time_to_pub,na.rm=T))
dat %>%   filter(time_to_pub>0) %>% 
  ggplot(aes(x=time_to_pub, col=era_type)) +
  geom_density() +
  OverReact::theme_react() +
  OverReact::scale_color_imperial(palette = "short", rev=T) +
  labs(title="Time to publication",subtitle = "From preprint publication to ")




# get top journal and categories for covid papers
top_journals=summary_journal %>% arrange(-n_papers) %>% filter(!is.na(journal_title),era=="COVID-19 papers") %>% head(5) %>% select(journal_title) %>% unlist() %>% as.character()
top_categories=summary_category %>% arrange(-n_papers) %>% filter(!is.na(journal_category_1),era=="COVID-19 papers") %>% head(5) %>% select(journal_category_1) %>% unlist() %>% as.character()

# plot time to publication
dat %>% 
  filter(subs_pub_journal_title%in%c("The BMJ","Scientific Reports","Frontiers in Public Health","Science","Nature"),type=="preprint",time_from_preprint_to_pub>0) %>% 
  ggplot(aes(x=covid_paper,y=time_from_preprint_to_pub,col=covid_paper)) +
  geom_boxplot() +
  facet_wrap(.~subs_pub_journal_title) +
  OverReact::theme_react() +
  labs(y="Days from preprint publication to subsequent publication",x="",col="")

# plot time to publication
dat %>% filter(subs_pub_journal_category_1%in%top_categories,type=="preprint",time_from_preprint_to_pub>0) %>% 
  ggplot(aes(x=era_type,y=time_from_preprint_to_pub,col=era_type)) +
  geom_boxplot() +
  facet_wrap(.~subs_pub_journal_category_1) +
  OverReact::theme_react() +
  # OverReact::scale_color_imperial(palette = "short") +
  labs(y="Days from preprint publication to subsequent publication")


# Boxplot of altmetrics
p_box_altmetrics=dat %>% filter(journal_category_1%in%top_categories) %>% 
  ggplot(aes(x=era_type,y=altmetrics_score,col=era_type)) +
  geom_boxplot() +
  facet_wrap(.~journal_category_1,nrow=1,labeller = labeller(journal_category_1 = label_wrap_gen(10))) +
  OverReact::theme_react() +
  scale_y_log10()+
  scale_x_discrete(labels=wrap_format(10))+
  OverReact::scale_color_imperial(palette = "core",rev=T) +
  labs(y="Altmetrics score",col="",x="", subtitle="Altmetrics score") +
  theme(legend.position = "bottom") +
  stat_summary(fun=mean, colour="red",geom="text", show.legend=F,
               vjust=-0.7, aes(label=round(..y.., digits =2)), size=2)
p_box_altmetrics

# Boxplot of citations
p_box_citations=dat %>% 
  filter(journal_category_1%in%top_categories) %>% 
  ggplot(aes(x=era_type,y=citations_count,col=era_type)) +
  geom_boxplot() +
  facet_wrap(.~journal_category_1,nrow=1,labeller = labeller(journal_category_1 = label_wrap_gen(10))) +
  OverReact::theme_react() +
  scale_y_log10()+
  scale_x_discrete(labels=wrap_format(10))+
  OverReact::scale_color_imperial(palette = "core",rev=T) +
  labs(y="Number of citations",col="",x="", subtitle="Citations") +
  theme(legend.position = "bottom") +
  stat_summary(fun=mean, colour="red",geom="text", show.legend=F,
               vjust=-0.7, aes(label=round(..y.., digits =2)), size=2)
p_box_citations


OverReact::saveREACTplot(p = p_box_citations,figpath = figpath,filename = "box_covid_noncovid_compare_citations",
                         # filetypes = c("png","jpg"),
                         width = 13,height = 5)

OverReact::saveREACTplot(p = p_box_altmetrics,figpath = figpath,filename = "box_covid_noncovid_compare_altmetrics",
                         # filetypes = c("png","jpg"),
                         width = 13,height = 5)


### combine both plots
p_box_comb <- (p_box_citations/p_box_altmetrics)+ plot_layout(guides="collect") & 
  # labs(title="Comparison of citations and Altmetrics scores") &
  theme(legend.position = "bottom")
p_box_comb
OverReact::saveREACTplot(p = p_box_comb,figpath = figpath,
                         filename = "box_covid_noncovid_compare_comb",
                         # filetypes = c("png","jpg"),
                         width = 13,height = 9)



# Publishing trends over time ---------------------------------------------

# dates_online=read_csv("data/bq_exports_mar_2023/date_online_2019_2022.csv")
# dat <- dat %>% left_join(dates_online)
# table(as.Date(dat$date_online)==as.Date(dat$date))


# define colours
colcov=imperial_colours[["orange"]]
noncolcov=imperial_colours[["dark grey"]]

# tinker theme
mytheme= theme(axis.title.y.left = element_text(colour = noncolcov),
               axis.text.y.left = element_text(colour = noncolcov),
               plot.title =element_text(size=13, face="bold"), 
               plot.subtitle =element_text(size=10), 
               axis.title.y.right = element_text(colour = colcov),
               axis.text.y.right = element_text(colour = colcov))



repdates=c(as.Date("2019-01-01"),as.Date("2020-01-01"),as.Date("2021-01-01"),as.Date("2022-01-01"))
set.seed(123)
df_summ <- dat %>% 
  # mutate(date=as.Date(date_online)) %>% 
  mutate(
    randnum=sample(c(rep(1,30),1:365),size =nrow(.),replace = T),
    date=case_when(date%in%repdates ~ date+randnum,
                   T ~ date),
    # date=as.Date(date_normal),
    year=lubridate::year(date),
    week=lubridate::week(date),
    ym=format_ISO8601(date,precision="ym")
  ) %>%
  filter(date<as.Date("2023-01-01")) %>%
  # filter(date != as.Date("2021-01-01"),date != as.Date("2022-01-01")) %>%
  group_by(ym,type,covid_paper) %>% 
  summarise(n=n()) %>% 
  ungroup() %>% 
  filter(!is.na(ym),!is.na(n)) %>% 
  pivot_wider(id_cols = c(ym, type),names_from =covid_paper,values_from = n) %>% 
  janitor::clean_names()  %>% 
  group_by(type) %>% 
  mutate(roll_covid_19_paper=covid_19_paper,
         roll_non_covid_19_paper=non_covid_19_paper) %>% 
  ungroup()



p_journal=df_summ %>% 
  filter(type=="article") %>% 
  ggplot(aes(x=ym,group=type))+
  geom_point(aes(y=roll_non_covid_19_paper),col=noncolcov,size=0.6) +
  geom_point(aes(y=roll_covid_19_paper*10), col=colcov,size=0.6) +
  geom_line(aes(y=roll_non_covid_19_paper),col=noncolcov)+
  geom_line(aes(y=roll_covid_19_paper*10), col=colcov)+
  scale_y_continuous(
    name="Journal articles (excl. COVID-19)",
    sec.axis =sec_axis(~./10, name="COVID-19 Journal articles")
  )+
  OverReact::theme_react()+
  theme(legend.position = c(0.1,0.9),
        axis.text.x = element_text(angle=70,vjust=1,hjust=1), axis.title.x = element_blank()) +
  mytheme +
  labs(title="Journal articles published per month",subtitle = "January 2019 - December 2022")
p_journal


p_preprint=df_summ %>% 
  filter(type!="article") %>% 
  ggplot(aes(x=ym,group=type))+
  geom_point(aes(y=roll_non_covid_19_paper),col=noncolcov,size=0.6) +
  geom_point(aes(y=roll_covid_19_paper*10), col=colcov,size=0.6) +
  geom_line(aes(y=roll_non_covid_19_paper),col=noncolcov)+
  geom_line(aes(y=roll_covid_19_paper*10), col=colcov)+
  scale_y_continuous(
    # expand = F,
    name="Preprints (excl. COVID-19)",
    sec.axis =sec_axis(~./10, name="COVID-19 preprints")
  )+
  OverReact::theme_react()+
  theme(legend.position = c(0.1,0.9),
        axis.text.x = element_text(angle=70,vjust=1,hjust=1), axis.title.x = element_blank()) +
  mytheme +
  labs(title="Preprints published per month",subtitle = "January 2019 - December 2022")
  
p_preprint

p_joint=p_journal/p_preprint
p_joint

# save
OverReact::saveREACTplot(p = p_joint,figpath = figpath,
                         filename = "articles_over_time_covid_compare",
                         width = 13,height = 9
)






# Publishing trends over time medical only ---------------------------------------------




df_summ_med <- dat %>% 
  # mutate(date=as.Date(date_online)) %>% 
  mutate(
    randnum=sample(c(rep(1,30),1:365),size =nrow(.),replace = T),
    date=case_when(date%in%repdates ~ date+randnum,
                   T ~ date),
    # date=as.Date(date_normal),
    year=lubridate::year(date),
    week=lubridate::week(date),
    ym=format_ISO8601(date,precision="ym")
  ) %>%
  filter(date<as.Date("2023-01-01"),
         journal_category_1%in% c("Medicine, general & internal",
                                  "Public, environmental & occupational health",
                                  "Biochemistry & molecular biology",
                                  "Cell biology") | 
           journal_title %in% c("bioRxiv","medRxiv")) %>%
  # filter(date != as.Date("2021-01-01"),date != as.Date("2022-01-01")) %>%
  group_by(ym,type,covid_paper) %>% 
  summarise(n=n()) %>% 
  ungroup() %>% 
  filter(!is.na(ym),!is.na(n)) %>% 
  pivot_wider(id_cols = c(ym, type),names_from =covid_paper,values_from = n) %>% 
  janitor::clean_names()  %>% 
  group_by(type) %>% 
  mutate(roll_covid_19_paper=covid_19_paper,
         roll_non_covid_19_paper=non_covid_19_paper) %>% 
  ungroup()



p_journal_med=df_summ_med %>% 
  filter(type=="article") %>% 
  ggplot(aes(x=ym,group=type))+
  geom_point(aes(y=roll_non_covid_19_paper),col=noncolcov,size=0.6) +
  geom_point(aes(y=roll_covid_19_paper*10), col=colcov,size=0.6) +
  geom_line(aes(y=roll_non_covid_19_paper),col=noncolcov)+
  geom_line(aes(y=roll_covid_19_paper*10), col=colcov)+
  scale_y_continuous(
    name="Journal articles (excl. COVID-19)",
    sec.axis =sec_axis(~./10, name="COVID-19 Journal articles")
  )+
  OverReact::theme_react()+
  theme(legend.position = c(0.1,0.9),
        axis.text.x = element_text(angle=70,vjust=1,hjust=1), axis.title.x = element_blank()) +
  mytheme +
  labs(title="Medical journal articles published per month",subtitle = "January 2019 - December 2022")
p_journal_med


p_preprint_med=df_summ_med %>% 
  filter(type!="article") %>% 
  ggplot(aes(x=ym,group=type))+
  geom_point(aes(y=roll_non_covid_19_paper),col=noncolcov,size=0.6) +
  geom_point(aes(y=roll_covid_19_paper*10), col=colcov,size=0.6) +
  geom_line(aes(y=roll_non_covid_19_paper),col=noncolcov)+
  geom_line(aes(y=roll_covid_19_paper*10), col=colcov)+
  scale_y_continuous(
    name="BioRxiv/medRxiv preprints (excl. COVID-19)",
    sec.axis =sec_axis(~./10, name="COVID-19 preprints")
  )+
  OverReact::theme_react()+
  theme(legend.position = c(0.1,0.9),
        axis.text.x = element_text(angle=70,vjust=1,hjust=1), axis.title.x = element_blank()) +
  mytheme +
  labs(title="BioRxiv/medRxiv preprints published per month",
       subtitle = "January 2019 - December 2022")


p_joint_med=p_journal_med/p_preprint_med
p_joint_med

# save
OverReact::saveREACTplot(p = p_joint_med,figpath = figpath,
                         filename = "articles_over_time_covid_compare_med",
                         width = 13,height = 9
)




# Combine into panel ------------------------------------------------------

design <- 
  "AAAAAA
   BBBBCC"

p_panel=p_journal+p_preprint+p$plot +
  plot_layout(design = design)

# save
OverReact::saveREACTplot(p = p_panel,figpath = figpath,
                         filename = "articles_per_month_km_panel",
                         width = 14,height = 9,savePDF = T,
                         filetypes = c("jpg","png"))



design <- 
  "AAAACC
   BBBB##"

p_panel_v2=p_journal+p_preprint+p$plot +
  plot_layout(design = design)

# save
OverReact::saveREACTplot(p = p_panel_v2,figpath = figpath,
                         filename = "articles_per_month_km_panel_v2",
                         width = 14,height = 9,savePDF = T,
                         filetypes = c("jpg","png"))

# save RDS for future work
saveRDS(object = list(p_journal=p_journal,
        p_preprint=p_preprint,km_plot=p$plot),
        file = paste0(figpath,"over_time_km_plots_for_panel.rds"))








# Proportion of papers to do with COVID-19 --------------------------------



  
p_pub_prop_covid <- dat %>% 
  filter(date >= as.Date("2020-01-02"),date < as.Date("2023-01-01")) %>%
  group_by(date,type_detail) %>% 
  summarise(n=n()) %>% 
  ungroup() %>% 
  pivot_wider(id_cols = date,names_from = type_detail,values_from = n) %>% 
  mutate(perc_covid_preprint=100*(`COVID-19 preprint`/(`COVID-19 preprint`+`Preprint`)),
         perc_covid_article=100*(`COVID-19 journal article`/(`COVID-19 journal article`+`Journal article`))) %>% 
  mutate(roll_prop_preprint=zoo::rollapplyr(perc_covid_preprint,width=30, mean, align="right",partial=T),
         roll_prop_article=zoo::rollapplyr(perc_covid_article,width=30, mean, align="right",partial=T),
         `Preprints`=ifelse(is.na(roll_prop_preprint),0,roll_prop_preprint),
         `Journal articles`=ifelse(is.na(roll_prop_article),0,roll_prop_article)) %>% 
  select(date,`Preprints`,`Journal articles`) %>% 
    pivot_longer(cols=c(`Journal articles`,`Preprints`)) %>% 
  ggplot(aes(x=date, y=value, col=name)) + 
  geom_line(size=0.7)+
  # facet_wrap(.~value,scales = "free_y", ncol=1)+
  scale_x_date(labels = date_format("%b %Y"))+
  labs(x="Date", y="% of papers relating to COVID-19 (30-day average)", col="", 
       title="% of papers relating to COVID-19", subtitle = "30 day moving average since January 2020") +
  OverReact::scale_color_imperial(palette = "cool",rev=F)+
  OverReact::theme_react()+
  theme(legend.position = c(0.8,0.9))

p_pub_prop_covid

# save
OverReact::saveREACTplot(p = p_pub_prop_covid,figpath = figpath,filename = "prop_articles_covid_over_time",
                         width = 7,height = 6
)



### prop covid related in medicin only
p_pub_prop_covid_med <- dat %>% 
  filter(journal_category_1%in% c("Medicine, general & internal",
                                  "Public, environmental & occupational health",
                                  "Biochemistry & molecular biology",
                                  "Cell biology") | 
           journal_title %in% c("bioRxiv","medRxiv")) %>% 
  filter(date >= as.Date("2020-01-02"),date < as.Date("2023-01-01")) %>%
  group_by(date,type_detail) %>% 
  summarise(n=n()) %>% 
  ungroup() %>% 
  pivot_wider(id_cols = date,names_from = type_detail,values_from = n) %>% 
  mutate(perc_covid_preprint=100*(`COVID-19 preprint`/(`COVID-19 preprint`+`Preprint`)),
         perc_covid_article=100*(`COVID-19 journal article`/(`COVID-19 journal article`+`Journal article`))) %>% 
  mutate(roll_prop_preprint=zoo::rollapplyr(perc_covid_preprint,width=30, mean, na.rm=T,align="right",partial=T),
         roll_prop_article=zoo::rollapplyr(perc_covid_article,width=30, mean,na.rm=T, align="right",partial=T),
         `MedrXiv / BiorXiv`=ifelse(is.na(roll_prop_preprint),0,roll_prop_preprint),
         `Journal articles (medical/biological)`=ifelse(is.na(roll_prop_article),0,roll_prop_article)) %>% 
  select(date,`MedrXiv / BiorXiv`,`Journal articles (medical/biological)`) %>% 
  pivot_longer(cols=c(`Journal articles (medical/biological)`,`MedrXiv / BiorXiv`)) %>% 
  ggplot(aes(x=date, y=value, col=name)) + 
  geom_line(size=0.7)+
  # facet_wrap(.~value,scales = "free_y", ncol=1)+
  scale_x_date(labels = date_format("%b %Y"))+
  labs(x="Date", y="% of medical papers relating to COVID-19 (30-day average)", col="", 
       title="% of medical papers relating to COVID-19", subtitle = "30 day moving average since January 2020") +
  OverReact::scale_color_imperial(palette = "cool",rev=F)+
  OverReact::theme_react()+
  theme(legend.position = c(0.8,0.9))

p_pub_prop_covid_med

# save
OverReact::saveREACTplot(p = p_pub_prop_covid_med,figpath = figpath,filename = "prop_articles_covid_over_time_med",
                         width = 7,height = 6
)





# Quality over time -------------------------------------------------------


# dates_online=read_csv("data/bq_exports_mar_2023/date_online_2019_2022.csv")
# dat <- dat %>% left_join(dates_online)
# table(as.Date(dat$date_online)==as.Date(dat$date))
df_summ_qual <- dat %>% 
  # mutate(date=as.Date(date_online)) %>% 
  # filter(date != as.Date("2019-01-01"),date != as.Date("2020-01-01"),
  #        date != as.Date("2021-01-01"),date != as.Date("2022-01-01")) %>%
  group_by(date,type,type_detail) %>% 
  summarise(n=n(),
            mean_rcr_alt_ratio=mean(rcr_alt_ratio,na.rm=T)) %>% 
  ungroup() %>% 
  # pivot_wider(id_cols = c(date, type),names_from =covid_paper,values_from = mean_rcr_alt_ratio) %>% 
  # janitor::clean_names()
  group_by(type_detail) %>%
  mutate(roll_mean_rcr_alt_ratio=zoo::rollapplyr(mean_rcr_alt_ratio,
                                                 width=30, mean,
                                                 align="right",partial=T)) %>%
  ungroup()



p_journal_qual=df_summ_qual %>%
  mutate(covidy=case_when(grepl("COVID", type_detail) ~ "COVID-19",
                          T ~ "Non-COVID-19")) %>% 
  # filter(type=="article") %>% 
  ggplot(aes(x=date, y=roll_mean_rcr_alt_ratio,col=covidy))+
  geom_line()+
  OverReact::theme_react() +
  OverReact::scale_color_imperial(palette = "two_col_grey_blue")+
  facet_wrap(.~type)+
  labs(title="Altmetrics:citation rate ratio",y="log(Altmetrics:citations/day ratio)",
       subtitle = "30-day rolling average, 2019-2022",col="") +
  theme(legend.position = c(0.9,0.1))
p_journal_qual



# save
OverReact::saveREACTplot(p = p_journal_qual,figpath = figpath,
                         filename = "bulls_index_over_time_compare",
                         width = 13,height = 9
)


















# Correlation analysis ----------------------------------------------------



createCorPlot <- function(cordat=dat %>% filter(type=="preprint"),
                          maxval=0.5,
                          column_title = "Preprints",
                          split=c(rep("Altmetrics",1),rep("Article \ncitation \ndata",3),
                                  rep("Journal \ndata",2)),
    myvars=c("altmetrics_score","citations_count",
             "field_citation_ratio",  "relative_citation_ratio",
                "journal_if_2022","journal_jci"
    ),
    myvarnames=c("All media and social media",
                "Total citations count","Field citation ratio","Relative citation ratio",
                "Journal impact factor","Journal Citation Indicator"

                 
    )){
  
  

  cormat_preprints <- cordat %>% 
    dplyr::select(all_of(myvars)) %>% 
    mutate(across(all_of(myvars),
                  ~replace_na(.x,0))) %>% 
    cor(use = "complete.obs") %>% as.matrix()
  colnames(cormat_preprints) <- rownames(cormat_preprints) <- myvarnames
  
  col_fun=circlize::colorRamp2(breaks = c(0,0.5,1), colors = c("white",myCols[[2]],myCols[[1]]))
  
  heatmap_preprints <- ComplexHeatmap::Heatmap(cormat_preprints,name = "Correlation",
                                               column_title = column_title,
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
  return(heatmap_preprints)
  
  
}


### Loop over all sections to create heatmaps
tps=unique(dat$era_type)
tps_pp=c("Preprint (2019)", "Non-COVID-19 \nPreprint (2020-2022)", "COVID-19 preprint")
tps_art=c("Journal article (2019)", "Non-COVID-19 \nJournal article (2020-2022)","COVID-19 journal article")


# Loop over preprints
hm_list_pp=NULL
for (t in tps_pp){
  hm=createCorPlot(cordat=dat %>% filter(era_type==t),column_title = t,
                   myvars=c("altmetrics_score","citations_count",
                            "field_citation_ratio",  "relative_citation_ratio",
                            "published_in_journal",
                            "subs_pub_journal_if_2022","subs_pub_journal_jci"
                   ),
                   myvarnames=c("All media and social media",
                                "Total citations count","Field citation ratio","Relative citation ratio",
                                "Subsequently published",
                                "Publishing journal impact factor",
                                "Publishing journal Citation Indicator"
                                
                                
                   ),
                   split=c(rep("Altmetrics",1),rep("Article \ncitation \ndata",3),
                           rep("Publishing \njournal \ndata",3))
                   )
  
  png(paste0(figpath,"heatmap_attention_correlation_",janitor::make_clean_names(t),".png"), 
      width = 7,height = 6, units = "in", res = 300)
  draw(hm)
  dev.off()
  hm_list_pp=hm_list_pp+hm
}
png(paste0(figpath,"heatmap_attention_correlation_preprints.png"), 
    width = 13,height = 5.5, units = "in", res = 300)
draw(hm_list_pp,gap=unit(1,"cm"))
dev.off()


# Loop over artiles
hm_list_art=NULL
for (t in tps_art){
  hm=createCorPlot(cordat=dat %>% filter(era_type==t),column_title = t)
  
  png(paste0(figpath,"heatmap_attention_correlation_",janitor::make_clean_names(t),".png"), 
      width = 7,height = 6, units = "in", res = 300)
  draw(hm)
  dev.off()
  hm_list_art=hm_list_art+hm
}
png(paste0(figpath,"heatmap_attention_correlation_articles.png"), 
    width = 13,height = 5.5, units = "in", res = 300)
draw(hm_list_art,gap=unit(1,"cm"))
dev.off()















# Repeat above in medical journals only -----------------------------------





tps_pp=c("Preprint (2019)", "Non-COVID-19 \nPreprint (2020-2022)", "COVID-19 preprint")
tps_art=c("Journal article (2019)", "Non-COVID-19 \nJournal article (2020-2022)","COVID-19 journal article")


# Loop over preprints
hm_list_pp=NULL
for (t in tps_pp){
  hm=createCorPlot(cordat=dat %>% filter(era_type==t,
                                         journal_category_1%in% c("Medicine, general & internal",
                                                                  "Public, environmental & occupational health",
                                                                  "Biochemistry & molecular biology",
                                                                  "Cell biology") | 
                                           journal_title %in% c("bioRxiv","medRxiv")),column_title = t,
                   myvars=c("altmetrics_score","citations_count",
                            "field_citation_ratio",  "relative_citation_ratio",
                            "published_in_journal",
                            "subs_pub_journal_if_2022","subs_pub_journal_jci"
                   ),
                   myvarnames=c("All media and social media",
                                "Total citations count","Field citation ratio","Relative citation ratio",
                                "Subsequently published",
                                "Publishing journal impact factor",
                                "Publishing journal Citation Indicator"
                                
                                
                   ),
                   split=c(rep("Altmetrics",1),rep("Article \ncitation \ndata",3),
                           rep("Publishing \njournal \ndata",3))
  )
  
  png(paste0(figpath,"heatmap_attention_correlation_",janitor::make_clean_names(t),".png"), 
      width = 7,height = 6, units = "in", res = 300)
  draw(hm)
  dev.off()
  hm_list_pp=hm_list_pp+hm
}

png(paste0(figpath,"heatmap_attention_correlation_medical_preprints.png"), 
    width = 13,height = 5.5, units = "in", res = 300)
draw(hm_list_pp,gap=unit(1,"cm"))
dev.off()


# Loop over artiles
hm_list_art=NULL
for (t in tps_art){
  hm=createCorPlot(cordat=dat %>% filter(era_type==t,
                                         journal_category_1%in% c("Medicine, general & internal",
                                                                  "Public, environmental & occupational health",
                                                                  "Biochemistry & molecular biology",
                                                                  "Cell biology") | 
                                           journal_title %in% c("bioRxiv","medRxiv")),column_title = t)
  
  png(paste0(figpath,"heatmap_attention_correlation_",janitor::make_clean_names(t),".png"), 
      width = 7,height = 6, units = "in", res = 300)
  draw(hm)
  dev.off()
  hm_list_art=hm_list_art+hm
}

png(paste0(figpath,"heatmap_attention_correlation_medical_articles.png"), 
    width = 13,height = 5.5, units = "in", res = 300)
draw(hm_list_art,gap=unit(1,"cm"))
dev.off()






# 
# 
# 
# 
# # # Save
# # dat <- readRDS("data/dat_main.rds")
# # concepts <- read_csv("data/bq-results-20221124-concepts.csv")
# # concepts_wide <- readRDS("data/concepts_wide.rds")
# 
# # wrangle dat
# dat <- dat %>% mutate(abstract_available = case_when(is.na(abstract_preferred) ~ "Yes",
#                                                      T ~ "No"),
#                       dummy="nobs",
#                       year_factor=as.factor(year)) %>% 
#   mutate(across(c("citations_count","altmetrics_score","cited_by_tweeters_count","cited_by_msm_count",
#                   "cohorts_com","cohorts_pub","cohorts_sci","cohorts_doc"),
#                 ~replace_na(.x,0)))
# 
# rowvar_list=c("dummy","year_factor","published","citations_count","relative_citation_ratio","field_citation_ratio",
#               "altmetrics_score","cited_by_tweeters_count","cited_by_msm_count",
#               "cohorts_com","cohorts_pub","cohorts_sci","cohorts_doc")
# # cov_names=as.list(rowvar_list)
# cov_names=list("Papers published","Year","Published in journal","Number of citations",
#                "Relative citation ratio","Field citation ratio",
#                "Altmetrics score","Number of tweets","Number of media mentions",
#                "Cohort: science communicator mentions","Cohort: general public mentions","Cohort: scientist mentions",
#                "Cohort: doctors and medics mentions")
# names(cov_names)=rowvar_list
# tab1 <- OverReact::crossTabMulti(dat=dat,rowvar_list = rowvar_list,colvar = "type",
#                                  cov_names = cov_names,include_percentages = T,
#                                  rowwise_precentages = F,confint = F,comma_thousands = T)
# 
# OverReact::saveREACTtable(tab = tab1,outpath = outpath,filename = "table_one")
# 
# 
# 
# # Get summary of papers by topic ------------------------------------------
# 
# 
# summary_category=dat %>% 
#   filter(!is.na(journal_category_1),journal_category_1!="") %>% 
#   group_by(journal_category_1) %>% 
#   summarise(n_papers=n(),
#             n_journals=n_distinct(journal_title),
#             mean_altmetric_score=mean(altmetrics_score, na.rm=T),
#             mean_citations=mean(citations_count, na.rm=T),
#             mean_journal_IF=mean(journal_if_2022, na.rm=T)
#   ) %>% 
#   arrange(-n_papers) %>% slice_head(n=20)
# 
# 
# summary_journal=dat %>% 
#   mutate(journal_if_2022=case_when(is.na(journal_if_2022) ~ 0,
#                            T ~ journal_if_2022)) %>% 
#   group_by(journal_title) %>% 
#   summarise(n_papers=n(),
#             mean_altmetric_score=round(mean(altmetrics_score, na.rm=T),1),
#             mean_citations=round(mean(citations_count, na.rm=T),1),
#             journal_IF=round(mean(journal_if_2022, na.rm=T),2)
#   ) %>% 
#   arrange(-journal_IF) %>% 
#   slice_head(n=20)
# 
# 
# # Get summary by concept
# summary_concept=concepts %>% 
#   left_join(dat %>% select(id,altmetrics_score,citations_count,journal_if_2022)) %>% 
#   mutate(journal_if_2022=case_when(is.na(journal_if_2022) ~ 0,
#                                    T ~ journal_if_2022)) %>% 
#   filter(!is.na(concept)) %>%
#   group_by(concept) %>% 
#   summarise(n_papers=n(),
#             mean_altmetric_score=round(mean(altmetrics_score, na.rm=T),1),
#             mean_citations=round(mean(citations_count, na.rm=T),1),
#             journal_IF=round(mean(journal_if_2022, na.rm=T),2)
#   ) %>% 
#   arrange(-n_papers)
# 
# 
# OverReact::savePrettyExcelWorkbook(listOfTables = list(table_one=tab1,
#                                                        top_categories=summary_category,
#                                                        top_journals=summary_journal,
#                                                        top_concepts=summary_concept %>% 
#                                                          slice_head(n=20)),
#                                    workbookName = "summary_tables",outpath = outpath,
#                                    noDecimalsColumns = c("n_papers","n_journals"))
# OverReact::saveREACTtable(tab = summary_category,outpath = outpath,filename = "journal_subject_metrics_summary")
# OverReact::saveREACTtable(tab = summary_journal,outpath = outpath,filename = "journal_metrics_summary")
# OverReact::saveREACTtable(tab = summary_concept,outpath = outpath,filename = "concept_metrics_summary")
# 
# 
# 
# 
# # Papers published over time ----------------------------------------------
# 
# 
# p_pub_over_time <- dat %>% 
#   mutate(year=lubridate::year(date),
#          week=lubridate::week(date)) %>% 
#   # filter(date != as.Date("2021-01-01"),date != as.Date("2022-01-01")) %>% 
#   group_by(date,type) %>% 
#   summarise(n=n()) %>% 
#   group_by(type) %>% 
#   mutate(roll_n=zoo::rollapplyr(n,width=30, mean, align="right",partial=T)) %>% 
#   ungroup() %>% 
#   filter(date > as.Date("2020-01-01"), date < as.Date("2022-11-24")) %>%
#   ggplot(aes(x=date, y=roll_n, col=type)) + 
#   # geom_point(size=0.1) +
#   geom_line(size=1)+
#   # facet_wrap(.~type,scales = "free_x", ncol=1)+
#   scale_x_date(labels = date_format("%b %Y"))+
#   labs(x="Date", y="Number of articles published (30-day average)", col="", 
#        title="Papers published per week", subtitle = "30 day moving average since January 2020",) +
#   OverReact::scale_color_imperial(palette = "cool")+
#   OverReact::theme_react()+
#   theme(legend.position = c(0.1,0.9))
# 
# p_pub_over_time
# OverReact::saveREACTplot(p = p_pub_over_time,figpath = figpath,filename = "articles_published_over_time",
#                          width = 6,height = 5
# )
# 
# 
# 
# # Papers published over time Medicine only ----------------------------------------------
# 
# 
# p_pub_over_time_med <- dat %>% 
#   mutate(year=lubridate::year(date),
#          week=lubridate::week(date)) %>% 
#   filter(journal_category_1%in% c("Medicine, general & internal",
#                                   "Public, environmental & occupational health",
#                                   "Biochemistry & molecular biology",
#                                   "Cell biology") | 
#            journal_title %in% c("bioRxiv","medRxiv")) %>% 
#   # filter(date != as.Date("2021-01-01"),date != as.Date("2022-01-01")) %>% 
#   group_by(date,type) %>% 
#   summarise(n=n()) %>% 
#   group_by(type) %>% 
#   mutate(roll_n=zoo::rollapplyr(n,width=30, mean, align="right",partial=T)) %>% 
#   ungroup() %>% 
#   filter(date > as.Date("2020-01-01"), date < as.Date("2022-11-24")) %>%
#   ggplot(aes(x=date, y=roll_n, col=type)) + 
#   # geom_point(size=0.1) +
#   geom_line(size=1)+
#   # facet_wrap(.~type,scales = "free_x", ncol=1)+
#   scale_x_date(labels = date_format("%b %Y"))+
#   labs(x="Date", y="Number of articles published (30-day average)", col="", 
#        title="Papers published per week", subtitle = "30 day moving average since January 2020",) +
#   OverReact::scale_color_imperial(palette = "cool")+
#   OverReact::theme_react()+
#   theme(legend.position = c(0.1,0.9))
# 
# p_pub_over_time
# OverReact::saveREACTplot(p = p_pub_over_time,figpath = figpath,filename = "articles_published_over_time",
#                          width = 6,height = 5
# )
# 
# 
# 
# 
# 
# 
# 
# 
# ### Media attention over time
# 
# 
# # Media attenton over time ------------------------------------------------
# 
# 
# # articles published over time
# p_alt_over_time_type <- dat %>% 
#   mutate(year=lubridate::year(date),
#          week=lubridate::week(date)) %>% 
#   # filter(date != as.Date("2021-01-01")) %>% 
#   group_by(type,date) %>% 
#   summarise(n=n(),
#             mean_alt = sum(altmetrics_score, na.rm=T)) %>% 
#   mutate(roll_mean_alt=zoo::rollmean(mean_alt,30, align="right", fill = NA, na.pad = F)) %>% 
#   filter(date > as.Date("2020-01-01"), date < as.Date("2022-11-24")) %>% 
#   ggplot(aes(x=date, y=roll_mean_alt,col=type)) + 
#   geom_line(size=1)+
#   # geom_line()+
#   scale_x_date(labels = date_format("%b %Y"))+
#   # facet_wrap(.~type, ncol=1, scales="free_x") +
#   labs(x="Date", y="Total media and social media attention (Altmetrics score)", 
#        title="Total media attention", subtitle = "30 day moving average since January 2020",
#        col="") +
#   OverReact::scale_color_imperial(palette = "cool")+
#   OverReact::theme_react()+
#   theme(legend.position = c(0.9,0.9)) 
# 
# 
# p_alt_over_time_type
# OverReact::saveREACTplot(p = p_alt_over_time_type,figpath = figpath,filename = "media_attention_over_time_type_facet",
#                          width = 6,height = 5
# )
# 
# # articles published over time
# p_alt_over_time_mean_type <- dat %>% 
#   mutate(year=lubridate::year(date),
#          week=lubridate::week(date)) %>% 
#   # filter(date != as.Date("2021-01-01")) %>% 
#   group_by(type,date) %>% 
#   summarise(n=n(),
#             mean_alt = mean(altmetrics_score, na.rm=T)) %>% 
#   mutate(roll_mean_alt=zoo::rollmean(mean_alt,30, align="right", fill = NA, na.pad = F)) %>% 
#   filter(date > as.Date("2020-01-01"), date < as.Date("2022-11-24")) %>% 
#   ggplot(aes(x=date, y=roll_mean_alt,col=type)) + 
#   geom_line(size=1)+
#   # geom_line()+
#   scale_x_date(labels = date_format("%b %Y"))+
#   # facet_wrap(.~type, ncol=1, scales="free_x") +
#   labs(x="Date", y="Average media and social media attention (Altmetrics score)", 
#        title="Average media attention per paper", subtitle = "30 day moving average since January 2020",
#        col="") +
#   OverReact::scale_color_imperial(palette = "cool")+
#   OverReact::theme_react()+
#   theme(legend.position = c(0.9,0.9)) 
# 
# p_alt_over_time_mean_type
# OverReact::saveREACTplot(p = p_alt_over_time_mean_type,figpath = figpath,filename = "media_attention_over_time_average_type_facet",
#                          width = 6,height = 5
# )
# 
# 
# ### Combine plots
# 
# p_alt_comb=p_alt_over_time_type/p_alt_over_time_mean_type
# p_alt_comb
# OverReact::saveREACTplot(p = p_alt_comb,figpath = figpath,filename = "media_attention_over_time_two_panel",
#                          width = 12,height = 10
# )
# 
# 
# 
# # Concepts over time ------------------------------------------------------
# 
# 
# myconcepts=c("COVID-19 vaccine","case fatality rate",
#              "personal protective equipment","long COVID")
# p_concepts_over_time=concepts %>% 
#   left_join(dat %>% select(id,type,date)) %>% 
#   filter(!is.na(concept), concept%in%myconcepts) %>% 
#   mutate(year=lubridate::year(date),
#          week=lubridate::week(date)) %>% 
#   group_by(concept,date) %>% 
#   summarise(n=n(),
#             mean_relevance = sum(relevance, na.rm=T)) %>% 
#   mutate(roll_mean_relevance=zoo::rollmean(mean_relevance,30, align="right", fill = NA, na.pad = F)) %>% 
#   filter(date > as.Date("2020-01-01"), date < as.Date("2023-11-24")) %>% 
#   ggplot(aes(x=date, y=roll_mean_relevance, col=concept,group=concept)) + 
#   geom_line(size=1)+
#   # ggrepel::geom_label_repel(aes(label = concept),
#   #                                    nudge_x = 1,
#   #                                    na.rm = TRUE)+
#   # # geom_line
#   # directlabels::geom_dl(aes(label=concept), method = list(directlabels::dl.trans(x = x - 0.2), "lastt.polygons", cex = 0.8)) +
#   scale_x_date(labels = date_format("%b %Y"))+
#   # facet_wrap(.~type, ncol=1, scales="free_y") +
#   labs(x="Date", y="Sum of concept relevance across all papers", 
#        title="Evolution of concept relevance in papers over time", subtitle = "30 day moving average since January 2020",
#        col="") +
#   OverReact::scale_color_imperial(palette = "default")+
#   OverReact::theme_react()+
#   theme(legend.position = c(0.2,0.8)) 
# 
# p_concepts_over_time
# 
# 
# 
# OverReact::saveREACTplot(p = p_concepts_over_time,figpath = figpath,
#                          filename = "concepts_over_time",
#                          width = 7,height = 4.5
# )
# 
# 
# # Preprints by publication status -----------------------------------------
# 
# 
# 
# # articles published over time
# p_pub_over_time_preprint_published <- dat %>% 
#   filter( type == "preprint") %>% 
#   mutate(year=lubridate::year(date),
#          week=lubridate::week(date)) %>% 
#   group_by(date,published) %>% 
#   summarise(n=n()) %>% 
#   group_by(published) %>% 
#   arrange(published, date) %>% 
#   mutate(roll_n=zoo::rollmean(n,30, align="right", fill = NA, na.pad = F)) %>% 
#   filter(date > as.Date("2020-01-01"), date < as.Date("2022-11-01")) %>% 
#   mutate(published = case_when(published ==1 ~ "Published in journal",
#                                T~"Not yet published")) %>% 
#   ggplot(aes(x=date, y=roll_n, fill = factor(published))) + 
#   geom_area( stat="identity", col="white")+
#   # OverReact::scale_color_imperial(palette = "two_col_grey_orng", reverse = T)+
#   OverReact::scale_fill_imperial(palette = "two_col_grey_orng", reverse = T)+
#   # scale_fill_manual(values = myCols[c(5,2)]) +
#   # scale_color_manual(values = myCols[c(5,2)]) +
#   scale_x_date(labels = date_format("%b %Y"))+
#   labs(x="Date", y="Number of preprints published (30-day average)", col ="", fill ="") +
#   OverReact::theme_react() +
#   # OverReact::scale_fill_imperial(palette = "two_col_grey_blue",reverse = T)+
#   theme(legend.position = c(0.8,0.9))
# p_pub_over_time_preprint_published
# 
# 
# OverReact::saveREACTplot(p = p_pub_over_time_preprint_published,figpath = figpath,
#                          filename = "preprints_published_over_time_by_pub_status",
#                          width = 7,height = 4.5
# )
# 
# 
# 
# # Correlation analysis ----------------------------------------------------
# 
# myvars <- c("altmetrics_score","cited_by_msm_count",
#             "cited_by_tweeters_count","cited_by_fbwalls_count",
#             "cohorts_sci","cohorts_doc","cohorts_com","cohorts_pub",
#             "cites_per_day","citations_count",
#             # "subs_pub_sjr",
#             "published",
#             "subs_pub_journal_if_2022"
# )
# myvarnames=c("All media and social media","Mainstream media",
#              "Twitter","Facebook",
#              "Social media (scientists)","Social media (medics)",
#              "Social media (science communicators)",
#              "Social media (general public)",
#              "Citation rate", "Total citations count",
#              "Published in journal","Impact factor of publishing journal"
#              # "Impact score of publishing journal",
#              
# )
# 
# cormat_preprints <- dat %>% filter(type=="preprint") %>% 
#   dplyr::select(all_of(myvars)) %>% 
#   mutate(across(all_of(myvars[1:10]),
#                 ~replace_na(.x,0))) %>% 
#   cor(use = "pairwise.complete.obs") %>% as.matrix()
# colnames(cormat_preprints) <- rownames(cormat_preprints) <- myvarnames
# maxval=0.5
# col_fun=circlize::colorRamp2(breaks = c(0,0.5,1), colors = c("white",myCols[[2]],myCols[[1]]))
# split=c(rep("Altmetrics",8),rep("Citation \ndata",2), rep("Subsequent \npublication",2))
# heatmap_preprints <- ComplexHeatmap::Heatmap(cormat_preprints,name = "Correlation",
#                                              column_title = "Preprints",
#                                              column_title_gp = gpar(fontsize=15,fontface="bold"),
#                                              
#                                              # cluster_columns = F,
#                                              split = split,
#                                              column_split = split, row_split=split,cluster_rows = F,cluster_columns = F,
#                                              rect_gp = gpar(col="white", lwd=1),
#                                              # heatmap_legend_param = list(heatmap_legend_side="bottom"),
#                                              
#                                              cell_fun = function(j,i,x,y,width, height, fill){
#                                                grid.text(sprintf("%.2f", cormat_preprints[i,j]), x, y,
#                                                          gp=gpar(fontsize=10,
#                                                                  col = 
#                                                                    ifelse(cormat_preprints[i,j]<0 |
#                                                                             cormat_preprints[i,j] == 0,
#                                                                           "red",  
#                                                                           ifelse(cormat_preprints[i,j]>0.25 |
#                                                                                    cormat_preprints[i,j] == 0,
#                                                                                  "white","black"))))
#                                              },
#                                              col = col_fun)
# 
# heatmap_preprints
# 
# 
# 
# #### Journal
# dat$journal_jci
# myvars <- c("altmetrics_score","cited_by_msm_count",
#             "cited_by_tweeters_count","cited_by_fbwalls_count",
#             "cohorts_sci","cohorts_doc","cohorts_com","cohorts_pub",
#             "cites_per_day","citations_count",
#             "journal_if_2022","journal_jci"
# )
# myvarnames=c("All media and social media","Mainstream media",
#              "Twitter","Facebook",
#              "Social media (scientists)","Social media (medics)",
#              "Social media (science communicators)",
#              "Social media (general public)",
#              "Citation rate", "Total citations count",
#              "Journal Impact Factor (2022)",
#              "Journal Citation Indicator (2022)"
#              
# )
# 
# cormat_journals <- dat %>% filter(type!="preprint") %>% 
#   dplyr::select(all_of(myvars)) %>% 
#   mutate(across(all_of(myvars[1:10]),
#                 ~replace_na(.x,0))) %>% 
#   cor(use = "pairwise.complete.obs") %>% as.matrix()
# colnames(cormat_journals) <- rownames(cormat_journals) <- myvarnames
# maxval=0.5
# col_fun=circlize::colorRamp2(breaks = c(0,0.5,1), colors = c("white",myCols[[2]],myCols[[1]]))
# split=c(rep("Altmetrics",8),rep("Citation \ndata",4))
# heatmap_journals <- ComplexHeatmap::Heatmap(cormat_journals,name = "Correlation",
#                                              column_title = "Journal articles",
#                                              column_title_gp = gpar(fontsize=15,fontface="bold"),
#                                              
#                                              # cluster_columns = F,
#                                              split = split,
#                                              column_split = split, row_split=split,cluster_rows = F,cluster_columns = F,
#                                              rect_gp = gpar(col="white", lwd=1),
#                                              # heatmap_legend_param = list(heatmap_legend_side="bottom"),
#                                              
#                                              cell_fun = function(j,i,x,y,width, height, fill){
#                                                grid.text(sprintf("%.2f", cormat_journals[i,j]), x, y,
#                                                          gp=gpar(fontsize=10,
#                                                                  col = 
#                                                                    ifelse(cormat_journals[i,j]<0 |
#                                                                             cormat_journals[i,j] == 0,
#                                                                           "red",  
#                                                                           ifelse(cormat_journals[i,j]>0.25 |
#                                                                                    cormat_journals[i,j] == 0,
#                                                                                  "white","black"))))
#                                              },
#                                              col = col_fun)
# 
# heatmap_journals
# 
# 
# png(paste0(figpath,"heatmap_attention_correlation_preprints.png"), width = 9,
#     height = 8, units = "in", res = 300)
# heatmap_preprints
# dev.off()
# png(paste0(figpath,"heatmap_attention_correlation_journals.png"), width = 9,
#     height = 8, units = "in", res = 300)
# heatmap_journals
# dev.off()
# 
# 
# 
# # Non covid data ----------------------------------------------------------
# 
# # load up data sets from dimensions
# non_covid_dat_2020=read_csv("data/2020_noncovid_bq-results-20230316-183308-1678991610383.csv")
# non_covid_dat_2021=read_csv("data/2021_noncovid_bq-results-20230316-184935-1678992581983.csv")
# non_covid_dat_2022=read_csv("data/2022_noncovid_bq-results-20230316-185623-1678993013936.csv")
# 
# # bind together
# non_covid_dat=rbind(non_covid_dat_2020,non_covid_dat_2021,non_covid_dat_2022)
# rm(non_covid_dat_2020)
# rm(non_covid_dat_2021)
# rm(non_covid_dat_2022)
# 
# # wrangle dat
# non_covid_dat <- non_covid_dat %>% 
#   mutate( dummy="nobs",
#           published=case_when(!is.na(resulting_publication_doi) ~ "Yes",
#                               T ~ "No"),
#           altmetrics_score=score,
#           year_factor=as.factor(year)) %>% 
#   mutate(across(c("citations_count","altmetrics_score"),
#                 ~replace_na(.x,0)))
# 
# # shorter list of variables for table
# rowvar_list=c("dummy","year_factor","published","citations_count","relative_citation_ratio","field_citation_ratio",
#               "altmetrics_score")
# 
# tab1_nonocovid <- OverReact::crossTabMulti(dat=non_covid_dat,rowvar_list = rowvar_list,colvar = "type",
#                                  cov_names = cov_names,include_percentages = T,
#                                  rowwise_precentages = F,confint = F,comma_thousands = T)
# 
# OverReact::saveREACTtable(tab = tab1_nonocovid,outpath = outpath,filename = "table_one_noncovid")
# 
# 
# # re-save table one excel file
# OverReact::savePrettyExcelWorkbook(listOfTables = list(table_one=tab1,
#                                                        table_one_nonocovid=tab1_nonocovid,
#                                                        top_categories=summary_category,
#                                                        top_journals=summary_journal,
#                                                        top_concepts=summary_concept %>% 
#                                                          slice_head(n=20)),
#                                    workbookName = "summary_tables",outpath = outpath,
#                                    noDecimalsColumns = c("n_papers","n_journals"))

