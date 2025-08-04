# clear env
rm(list=ls())
# source("E:/Group/functions/load_packages.R")

function_script_path ="/Users/mw418/codebase/misc/function_scripts/"
function_script_path ="E:/home/mw418/function_scripts/"

### Install packages
list.of.packages <- c("XML", "xml2", "rcrossref", "stringr","dplyr", "rjson", "httr","rAltmetric","purrr","magrittr","ggmap",
                      "tidyverse", "medrxivr","gtools") ### add all the packages that are used in your script here
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])] ### check if they are installed
if(length(new.packages)) install.packages(new.packages) ### install any that need installing
lapply(list.of.packages, require, character.only = TRUE)
library(medrxivr)
# library(rAltmetric)
library(magrittr)
library(purrr)
library(foreach)
library(doParallel)
library(furrr)
library(tidyverse)
source(file = "code/0_functions.R")


# 3a. Altmetrics ----------------------------------------------------------


# define data path
datapath="data/bq_exports_mar_2023/"


# get non-covid exports
dat_2019_nc <- read_csv(file=paste0(datapath,"2019_noncovid_dat.csv"))
dat_2020_nc <- read_csv(file=paste0(datapath,"2020_noncovid_dat.csv"))
dat_2021_nc <- read_csv(file=paste0(datapath,"2021_noncovid_dat.csv"))
dat_2022_nc <- read_csv(file=paste0(datapath,"2022_noncovid_dat.csv"))

# bind all
dat_nc <- rbind(dat_2019_nc,dat_2020_nc,dat_2021_nc,dat_2022_nc)

# remove
rm(dat_2019_nc,dat_2020_nc,dat_2021_nc,dat_2022_nc)

# download dates (date_normal is the only good date to use)
dat_nc_dates <- read_csv(file=paste0(datapath,"dates_2019_2022.csv"))
dat_nc_dates <- dat_nc_dates %>% as.data.frame()
dat_nc <- dat_nc %>% left_join(dat_nc_dates)
rm(dat_nc_dates)

# remove unnecessary date vars
dat_nc <- dat_nc %>% mutate(date=as.Date(date_normal)) %>% 
  select(-date_inserted,-date_normal)



# Load titles

##### TITLES ######

# get non-covid titles (too big to join with main data)
dat_2019_nc_t <- read_csv(file=paste0(datapath,"2019_noncovid_titles_bq-results-20230322-192201-1679513691391.csv"))
dat_2020_nc_t <- read_csv(file=paste0(datapath,"2020_noncovid_titles_bq-results-20230316-183308-1678991610383.csv"))
dat_2021_nc_t <- read_csv(file=paste0(datapath,"2021_noncovid_titles_bq-results-20230317-124816-1679057331605.csv"))
dat_2022_nc_t <- read_csv(file=paste0(datapath,"2022_noncovid_titles_bq-results-20230317-131347-1679058872810.csv"))

# bind all
dat_titles <- rbind(dat_2019_nc_t,dat_2020_nc_t,dat_2021_nc_t,dat_2022_nc_t)
dat_titles <- dat_titles %>% as.data.frame()

rm(dat_2019_nc_t,dat_2020_nc_t,dat_2021_nc_t,dat_2022_nc_t)

# dat_titles=readRDS(paste0(datapath,"clean/titles_english.rds"))

# filter main dat by this
ids_joins=intersect(dat_titles$id,dat_nc$id)
dat_nc <- dat_nc %>% filter(id%in%dat_titles$id)

# filter for articles and preprints only
dat_nc <- dat_nc %>% filter(type%in%c("article","preprint"))
dat_titles <- dat_titles %>% filter(id %in% dat_nc$id)

# deduplicate
dat_nc <- dat_nc[!duplicated(dat_nc$id),]
dat_titles <- dat_titles[!duplicated(dat_titles$id),]

# order titles by main data
dat_titles <- dat_titles %>%
  arrange(match(id, dat_nc$id))

all(dat_titles$id==dat_nc$id) # TRUE! Ideal

# Cites per day -----------------------------------------------------------


# create cites per day
extractdate=as.Date("2023-03-18")

# create new variables
dat_nc <- dat_nc %>% rename(citations_count=times_cited) %>% 
  mutate(published_in_journal=case_when(!is.na(resulting_publication_doi) ~ 1,
                                        T ~0),
         days_since_publication=as.numeric(extractdate-date),
         cites_per_day=citations_count/days_since_publication)




# Add impact factor -------------------------------------------------------

if_data <- read_csv("data/Impactfactor2022 2.csv")
if_data$journal_issn <- gsub("-","",if_data$issn)
if_data$journal_issn[if_data$journal_issn=="N/A" ] <- NA_character_
if_data$category_1 <- if_data$category_1 %>% stringr::str_to_sentence()
if_data$category_2 <- if_data$category_2 %>% stringr::str_to_sentence()
if_data$category_3 <- if_data$category_3 %>% stringr::str_to_sentence()
if_data$category_4 <- if_data$category_4 %>% stringr::str_to_sentence()
if_data <- if_data %>% filter(!is.na(journal_issn)) %>% 
  select(journal_issn,citations,if_2022,jci,category_1,category_2) %>% 
  rename(journal_citations=citations,
         journal_if_2022=if_2022,
         journal_jci=jci,
         journal_category_1=category_1,
         journal_category_2=category_2)

# conv ert to numeric where possible
if_data <- OverReact::checkConvertNumeric(if_data)
if_data$journal_if_2022 <-  as.numeric(if_data$journal_if_2022)
if_data$journal_jci <-  as.numeric(if_data$journal_jci)

# Add a couple of impact-factor related variables
if_data <- if_data %>% 
  mutate(journal_if_10=case_when(journal_if_2022>=10 ~ 1,
                                 T ~0),
         journal_if_5=case_when(journal_if_2022>=5 ~ 1,
                                T ~0),
         journal_if_1=case_when(journal_if_2022>=1 ~ 1,
                                T ~0),
         journal_if_any=case_when(!is.na(journal_if_2022) ~ 1,
                                  T ~0)
  )


# extract scimago qaurtiles
split_cat=stringr::str_split_fixed(if_data$journal_category_1," - ",2) 
split_cat <- split_cat %>% as.data.frame()
names(split_cat) <- c("journal_category_1","scimago_Q")
split_cat$scimago_Q <- gsub(".*[(]","",split_cat$scimago_Q)
split_cat$scimago_Q <- gsub("[)]","",split_cat$scimago_Q)
split_cat$scimago_Q <- gsub("q","",split_cat$scimago_Q)
split_cat$scimago_Q[split_cat$scimago_Q=="n/a"] <- NA_character_
split_cat$scimago_Q <- as.numeric(split_cat$scimago_Q)


# bind
if_data <- cbind(if_data %>% select(-journal_category_1),
                 journal_category_1=split_cat$journal_category_1,
                 scimago_Q=split_cat$scimago_Q)

# how much IF data do we have?
# remove hyphens in issn
dat_nc$issn <- gsub("-","",dat_nc$issn)

# nearly half. Let's join
dat_nc <- dat_nc %>% left_join(if_data, by=c("issn"="journal_issn"))

# filter to unique (again)
dat_nc <- dat_nc %>% distinct(id, .keep_all = TRUE)

all(dat_titles$id==dat_nc$id) # STILL TRUE! Ideal


# Add subsequent publication ----------------------------------------------

# get info on subsequent publication of preprints #
dois_pub=unique(dat_nc$resulting_publication_doi)
dois_pub <- dois_pub[!is.na(dois_pub)]
dat_nc <- dat_nc %>% rename(altmetrics_score=score)
dat_nc <- dat_nc %>% rename(journal_title=title)

# get journal articles that were preprints
dat_published_preprints=dat_nc %>% filter(doi %in% dois_pub)

dat_published_preprints <- dat_published_preprints %>% select(doi,journal_title,altmetrics_score,
                                                              date,citations_count,
                                                              cites_per_day,journal_if_2022,journal_jci,
                                                              journal_if_10,journal_if_5,
                                                              journal_if_1,journal_if_any,
                                                              journal_category_1)
colnames(dat_published_preprints) <- paste0("subs_pub_",colnames(dat_published_preprints))

# join with original data
dat_nc <- dat_nc %>% left_join(dat_published_preprints, by = c("resulting_publication_doi"="subs_pub_doi"))

# unique only
dat_nc <- dat_nc %>% distinct(id, .keep_all = TRUE)
all(dat_titles$id==dat_nc$id) # STILL TRUE! Ideal

# get time to publicataion
dat_nc <- dat_nc %>% mutate(time_from_preprint_to_pub=as.numeric(subs_pub_date-date))

# save
saveRDS(dat_nc,paste0(datapath,"clean/dat_all.rds"))
saveRDS(dat_titles,paste0(datapath,"clean/titles_all.rds"))

# clean up
# rm(dat_published_preprints,dat_titles)
rm(dat_published_preprints)


# Import and clean COVID data ---------------------------------------------

# get covid exports
dat_2020 <- read_csv(file=paste0(datapath,"covid_abs_2020_bq-results-20230316-202628-1678998507926.csv"))
dat_2021 <- read_csv(file=paste0(datapath,"covid_abs_2021_bq-results-20230316-202908-1678998617852.csv"))
dat_2022 <- read_csv(file=paste0(datapath,"covid_abs_2022_bq-results-20230316-203443-1678998952698.csv"))

# bind all
dat <- rbind(dat_2020,dat_2021,dat_2022)

# remove
rm(dat_2020,dat_2021,dat_2022)

# check overlap with main data , in terms of variables
intersect(names(dat),names(dat_nc))
setdiff(names(dat),names(dat_nc))

# find ids that are common to both data sets
ids_keep=unique(intersect(dat$id,dat_nc$id))

# filter covid data
dat <- dat %>% filter(id%in%ids_keep)

# we only want articles and preprints, so,let's filter <-  but this should no do anything now
dat <- dat %>% filter(type%in%c("article","preprint"))

# create DF of abstracts only
dat_abs=dat %>% 
  select(id,preferred) %>% 
  filter(!is.na(preferred)) %>% 
  rename(abstract=preferred)
  
# remove abstracts and other unnecessary data from dat, and join with main data
dat <- dat %>% select(-year,-doi,-id_1,-preferred,-pmid,-type) %>% 
  left_join(dat_nc)  %>% distinct(id, .keep_all = TRUE)

# # save for later
# saveRDS(dat,paste0(datapath,"clean/notfnal/covid_eng_articles_prefilt.rds"))
# saveRDS(dat_abs,paste0(datapath,"clean/notfnal/covid_eng_abstracts_prefilt.rds"))

# remove dat_nc to speed up script
rm(dat_nc)
rm(dat_abs)

# free unused memory
gc()

# Some more filtering: on COVIDyness -----------------------------------------------------

#load titles
# dat_titles=readRDS("data/bq_exports_mar_2023/clean/titles_all.rds")

# filter
dat_titles <- dat_titles %>% filter(id%in%ids_keep)
dat_titles <- dat_titles %>% rename(title_preferred=preferred)
library(stringr)

# Function to escape special regex characters
escape_special_chars <- function(x) {
  gsub("([\\.|()\\[\\]{}+*?^$\\\\])", "\\\\\\1", x, perl = TRUE)
}
# Define all inclusion terms
all_inclusion_terms <- c(
  # Original COVID-19 terms
  "2019-nCoV", "COVID-19", "SARS-CoV-2", "HCoV-2019", "hcov", "NCOVID-19",
  "SARS-CoV‑2",
  "coronavirus disease 2019", "corona virus disease 2019",
  "severe acute respiratory syndrome coronavirus 2",
  "SarsCoV2", "COVID19", "2019-nCov", "COVID 19",
  "severe acute respiratory syndrome corona virus 2",
  "Wuhan coronavirus", "China coronavirus",
  "Wuhan corona virus", "China corona virus",
  "novel coronavirus", "novel corona virus",
  "COVID", "covid", "CoV2", "coronavirus",
  "nCoV-2019", "ncov-2019", "nCoV19", "nCoV",
  "SARS2", "SARS CoV2",
  
  # Long COVID terms
  "long COVID", "long COVID-19", "long covid", "long-haul COVID",
  "long-haul COVID-19", "post-acute sequelae of SARS-CoV-2 infection",
  "PASC", "post-COVID syndrome", "post-COVID-19 syndrome",
  "chronic COVID syndrome", "long-term effects of COVID-19",
  
  # Variant terms
  "Alpha variant", "B.1.1.7", "Beta variant", "B.1.351",
  "Gamma variant", "P.1", "Delta variant", "B.1.617.2",
  "Omicron variant", "B.1.1.529", "SARS-CoV-2 variant",
  "COVID-19 variant", "variant of concern", "XBB.1.5",
  "BA.4", "BA.5", "XBB", "BA.2.75", "C.37",
  
  # Vaccine-related terms
   "Pfizer vaccine", "Moderna vaccine", "AstraZeneca vaccine",
  "Johnson & Johnson vaccine"
)

# Escape special characters
escaped_inclusion_terms <- escape_special_chars(all_inclusion_terms)

# Create the regex pattern
covid_pattern <- paste0("\\b(", paste(escaped_inclusion_terms, collapse = "|"), ")\\b")


# Apply the search using stringr's str_detect
matches <- str_detect(dat_titles$title_preferred, regex(covid_pattern, ignore_case = TRUE))
table(matches)
dat_titles$include=as.numeric(matches)
write_csv(dat_titles,file = "output/covid_paper_filtering_list.csv")


# visual inspection of the titles that have been filtered 
dat_fp_titles <- dat_titles$title_preferred[!matches] # yeah the vast majority of these look like false positives

# filter down dat to only those papers that meet the more stringent criteria
dat_titles <- dat_titles[matches,]

# filter main data set
dat <- dat %>% filter(id%in%dat_titles$id)

# load abstracts to filter too
# dat_abs=readRDS("data/bq_exports_mar_2023/clean/notfnal/covid_eng_abstracts_prefilt.rds")
dat_abs <- dat_abs %>% filter(id%in%dat_titles$id)

# convert to numeric where appropriate
dat <- OverReact::checkConvertNumeric(dat)





# CLeaning COVID-19 words  --------------------------------------------------

# filter down abstract data
dat_abs <- dat_abs[!is.na(dat_abs$abstract),]
dat_abs <- dat_abs %>% rename(abstract_preferred=abstract)

### Tidy up a few of the common words so they don't get picked up in the text analysis
sarswords <- unique(c("SARS-CoV-2","SARS-COV-2","Sars-CoV-2","sars-cov-2","SARS CoV-2","Sars COV-2","SARS‐CoV‐2",
                      "SARS CoV 2","Sars-CoV 2","SARS-CoV2","Sars-Cov-2","SarsCov2","SarsCov-2","Sars Cov2"))
dat_titles$title_preferred=sapply(X = 1:length(dat_titles$title_preferred), function(x) stringi::stri_replace_all_regex(dat_titles$title_preferred[x],
                                                                                                          pattern = sarswords,
                                                                                                          replacement = rep("SarsCoV2",length(sarswords)),
                                                                                                          vectorize_all = F))

dat_abs$abstract_preferred=sapply(X = 1:length(dat_abs$abstract_preferred), function(x) stringi::stri_replace_all_regex(dat_abs$abstract_preferred[x],
                                                                                                                        pattern = sarswords,
                                                                                                                        replacement = rep("SarsCoV2",length(sarswords)),
                                                                                                                        vectorize_all = F))

### COVID WORDS ###
covidwords <- unique(c("COVID-19","Covid-19","COVID 19","covid-19","covid 19", "COVID--19",
                       "Covid - 19","COVID- 19","Covid -19", "COVID‐19"))
dat_titles$title_preferred=sapply(X = 1:length(dat_titles$title_preferred), function(x) stringi::stri_replace_all_regex(dat_titles$title_preferred[x],
                                                                                                          pattern = covidwords,
                                                                                                          replacement = rep("COVID19",length(covidwords)),
                                                                                                          vectorize_all = F))

dat_abs$abstract_preferred=sapply(X = 1:length(dat_abs$abstract_preferred), function(x) stringi::stri_replace_all_regex(dat_abs$abstract_preferred[x],
                                                                                                                        pattern = covidwords,
                                                                                                                        replacement = rep("COVID19",length(covidwords)),
                                                                                                                        vectorize_all = F))


# save
saveRDS(dat_abs,paste0(datapath,"clean/dat_main_abs.rds"))
saveRDS(dat_titles,paste0(datapath,"clean/dat_main_titles.rds"))

# join with main data
dat <- dat %>% left_join(dat_titles)


# save
saveRDS(dat,paste0(datapath,"clean/dat_main.rds"))


# # Add altmetrics ----------------------------------------------------------
# 
# 
# 
# dat_alt=readRDS("data/altmetric_res_all.rds")
# 
# # UNCOMMENT TO AVOID RUNNING WHOLE PREVIOUS SCRIPT ###
# # dat <- readRDS(paste0(datapath,"/clean/dat_main.rds"))
# # commonvars=c("score",  "pmid", "column_label", "title", "altmetric_jid", 
# # "issns1", "journal", "cohorts_sci", "context_all_count", "context_all_mean", 
# # "context_all_rank", "context_all_pct", "context_all_higher_than", 
# # "context_journal_count", "context_journal_mean", "context_journal_rank", 
# # "context_journal_pct", "context_journal_higher_than", "context_similar_age_3m_count", 
# # "context_similar_age_3m_mean", "context_similar_age_3m_rank", 
# # "context_similar_age_3m_pct", "context_similar_age_3m_higher_than", 
# # "context_similar_age_journal_3m_count", "context_similar_age_journal_3m_mean", 
# # "context_similar_age_journal_3m_rank", "context_similar_age_journal_3m_pct", 
# # "context_similar_age_journal_3m_higher_than", "epubdate", "altmetric_id", 
# # "schema", "is_oa", "cited_by_posts_count", "cited_by_tweeters_count", 
# # "cited_by_accounts_count", "last_updated", "history_1y", "history_6m", 
# # "history_3m", "history_1m", "history_1w", "history_6d", "history_5d", 
# # "history_4d", "history_3d", "history_2d", "history_1d", "history_at", 
# # "url", "added_on", "published_on", "scopus_subjects1", "readers_citeulike", 
# # "readers_mendeley", "readers_connotea", "readers_count", "images_small", 
# # "images_medium", "images_large", "details_url", "issns", "cohorts_pub", 
# # "cohorts_com", "pubdate", "cited_by_wikipedia_count", "cited_by_msm_count", 
# # "cohorts_doc", "cited_by_feeds_count", "cited_by_peer_review_sites_count", 
# # "cited_by_fbwalls_count", "cited_by_rdts_count", "handles", "handle", 
# # "scopus_subjects", "cited_by_policies_count", "abstract", "cited_by_videos_count", 
# # "arxiv_id", "ads_id", "subjects1", "subjects", "pmc")
# 
# common_vars=intersect(names(dat_alt),names(dat))
# 
# # join
# dat_out <- dat %>% 
#   select(-all_of(commonvars))%>% 
#   left_join(dat_alt %>% select(-type))
# 
# # comvert to numeric
# dat_out <- OverReact::checkConvertNumeric(dat_out)
# 
# # filter to unique (again)
# dat_out <- dat_out %>% distinct(id, .keep_all = TRUE)
# 
# 
# # save
# saveRDS(dat_out,paste0(datapath,"clean/dat_main.rds"))
# 
# 
# 




