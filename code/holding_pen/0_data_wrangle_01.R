# clear env
rm(list=ls())
function_script_path ="E:/home/mw418/function_scripts/"
function_script_path="/Users/mathzero/Library/CloudStorage/GoogleDrive-mattdwhitaker@gmail.com/My Drive/Imperial/PhD/REACT2_analysis_in_progress/RESULTS/function_scripts/"


#' Source any functions from the local file
source(paste0(function_script_path,"/load_packages.R"))

load_packages(c("tidyverse","ggplot2","ggthemes", "tm",
                "tidytext","topicmodels","Rtsne","rsvd", 
                "Rcpp","furrr","future", "OverReact"))

source(file = "code/0_functions.R")
# # # devtools::install_github(repo = "mathzero/OverReact")
# pacman::p_load(dplyr, OverReact,tidyverse,qdap,tm,wordcloud,ggplot2,ggthemes,
#                text2map,tidytext)
# load_packages("vctrs")
# read in data
dat_1 <- read_csv("data/COVID publications table_Tab 1_Table_2020.csv")
dat_2 <- read_csv("data/COVID publications table_Tab 1_Table_2021_Q1_Q2.csv")
dat_3 <- read_csv("data/COVID publications table_Tab 1_Table_2021_Q3_Q4.csv")
dat_4 <- read_csv("data/COVID publications table_Tab 1_Table_2022_Q1_Q2.csv")


# combine all data
dat <- rbind(dat_1,dat_2,dat_3,dat_4)

# save for external altmetrics harvesting
saveRDS(dat,file = "data/dat_comb.rds")


# define data frame of data filtering stages
filtering_df=data.frame(stage="Raw data set",n=nrow(dat))

### Main data cleaning ###

### Apply functions
dat <- dat %>% mutate_all(funs(makeNA)) %>% janitor::clean_names()
dat <- checkConvertNumeric(df = dat)

head(dat)

# make dates be not bad
dat$date_inserted <- str_replace(string = dat$date_inserted,pattern = "Sept",replacement = "Sep")
dat$date_inserted <- gsub(",.*","",dat$date_inserted)
dat$date <- parse_date(dat$date_inserted,format = "%d %b %Y")
dat <- dat %>% arrange(date)

# ore accurate cites per dat
dat$cites_per_day <- dat$citations_count/dat$days_since_publication


# replace NAs with 0
dat$citations_count[is.na(dat$citations_count)] <- 0
dat$cites_per_day[is.na(dat$cites_per_day)] <- 0
dat$altmetrics_score[is.na(dat$altmetrics_score)] <- 0

# add document ID
dat$unique_id <- as.character(1:nrow(dat))

# dat$resulting_publication_doi %>% is.na() %>% table(exclude="none")

# Create published var
dat <- dat %>% dplyr::mutate(published = case_when(!is.na(resulting_publication_doi) ~ 1,
                                                   TRUE ~ 0))


# Some filtering #

## Let's get rid of articles with no journal title (assuming they are non-journal articles)
is.na(dat$journal_title) %>% table(dat$preprint)
dat <- dat %>% filter(!is.na(journal_title))
filtering_df <- rbind(filtering_df,c("Removed non-journal articles",nrow(dat)))

#  remove unicode
dat$title_preferred <- lapply(dat$title_preferred,stringi::stri_trans_general,"latin-ascii")
dat$abstract_preferred <- lapply(dat$abstract_preferred,stringi::stri_trans_general,"latin-ascii")


# Properly idetify preprints ----------------------------------------------

### Propely identify preprints
preprint_names=unique(dat$journal_title[grepl("rxiv",tolower(dat$journal_title))])
preprint_names <- c(preprint_names,
                    unique(dat$journal_title[grepl("preprint",tolower(dat$journal_title))]))
# preprint_names <- c(preprint_names,
#                     unique(dat$journal_title[grepl("archive",tolower(dat$journal_title))]))
preprint_names <- c(preprint_names,"Research Square","Advance","ESSOAr","advance","EGUsphere",
                    "Beilstein Archives","Cambridge Open Engage","Frenxiv","Arabixiv")
preprint_names <- unique(preprint_names)
dat$preprint <- case_when(dat$journal_title%in%c(preprint_names) ~ "Preprint",
                          T ~ "Journal article")




# Add journal data --------------------------------------------------------

# read in data
scimaj=read.csv2("data/scimagojr 2021.csv") %>% janitor::clean_names()

# split ISSNs
issns=data.frame(do.call("rbind",strsplit(scimaj$issn,",",fixed=T)))
scimaj$issn_1=issns$X1
scimaj$issn_2=issns$X2

# split subjects
subjects=data.frame(do.call("rbind",strsplit(scimaj$categories,";",fixed=T)))
# remove (Q1)
subjects$X1=gsub(" [(]Q1[)]","",subjects$X1)
subjects$X1=gsub(" [(]Q2[)]","",subjects$X1)
subjects$X1=gsub(" [(]Q3[)]","",subjects$X1)
subjects$X1=gsub(" [(]Q4[)]","",subjects$X1)
# add subject to df
scimaj$journal_subject=subjects$X1

# select useful cols
scimaj <- scimaj %>% dplyr::select(-sourceid,-title,-type,-sjr_best_quartile,-coverage)

# remove hyphens in issn
dat$journal_issn <- gsub("-","",dat$journal_issn)

# how many issns do we have
table(dat$journal_issn[!is.na(dat$journal_issn)] %in% scimaj$issn_1)
issns_miss1=unique(setdiff(dat$journal_issn,scimaj$issn_1))
length(issns_miss1)

# OK let's join
dat <- dat %>% left_join(scimaj, by = c("journal_issn"="issn_1"))


# Add impact factor -------------------------------------------------------


if_data <- read_csv("data/Impactfactor2022.csv")
if_data$journal_issn <- gsub("-","",if_data$issn)
if_data$journal_issn[if_data$journal_issn=="N/A" ] <- NA_character_
if_data$category_1 <- if_data$category_1 %>% stringr::str_to_sentence()
if_data$category_2 <- if_data$category_2 %>% stringr::str_to_sentence()
if_data$category_3 <- if_data$category_3 %>% stringr::str_to_sentence()
if_data$category_4 <- if_data$category_4 %>% stringr::str_to_sentence()
if_data <- if_data %>% filter(!is.na(journal_issn)) %>% 
  select(journal_issn,citations,if_2022,jci,percentageOAGold,category_1,category_2)


# how much IF data do we have?
table(dat$journal_issn%in%if_data$journal_issn)

# nearly half. Let's join
dat <- dat %>% left_join(if_data)

# change key vars to numeric
dat$jci <- dat$jci %>% as.numeric()
dat$if_2022 <- dat$if_2022 %>% as.numeric()
dat$citations <- dat$citations %>% as.numeric()
dat <- dat %>% rename(journal_all_time_citations=citations)
dat$percentageOAGold <- dat$percentageOAGold %>% as.numeric()

# check IF distribution
hist(dat$if_2022, breaks=100)


# Add a couple of impact-factor related variables
dat <- dat %>% 
  mutate(if_10=case_when(if_2022>=10 ~ 1,
                         T ~0),
         if_5=case_when(if_2022>=5 ~ 1,
                        T ~0),
         if_1=case_when(if_2022>=1 ~ 1,
                        T ~0),
         if_any=case_when(!is.na(if_2022) ~ 1,
                          T ~0)
         # ,
         # subs_pub_if_10=case_when(subs_pub_if_2022>=10 ~ 1,
         #                          T ~0),
         # subs_pub_if_5=case_when(subs_pub_if_2022>=5 ~ 1,
         #                         T ~0),
         # subs_pub_if_1=case_when(subs_pub_if_2022>=1 ~ 1,
         #                         T ~0),
         # subs_pub_if_any=case_when(!is.na(subs_pub_if_2022) ~ 1,
         #                           T ~0)
         )


split_cat=stringr::str_split_fixed(dat$category_1," - ",2) 
split_cat <- split_cat %>% as.data.frame()
names(split_cat) <- c("Category_1","scimago_Q")
split_cat$scimago_Q
split_cat$scimago_Q <- gsub(".*[(]","",split_cat$scimago_Q)
split_cat$scimago_Q <- gsub("[)]","",split_cat$scimago_Q)
split_cat$scimago_Q <- gsub("q","",split_cat$scimago_Q)
split_cat$scimago_Q[split_cat$scimago_Q=="n/a"] <- NA_character_
split_cat$scimago_Q <- as.numeric(split_cat$scimago_Q)

dat <- cbind(dat,split_cat)

dat <- dat %>% select(-journal_subject,category_1,category_2)






# Subsequent publication --------------------------------------------------


# get info on subsequent publication of preprints #
dois_pub=unique(dat$resulting_publication_doi)
dois_pub <- dois_pub[!is.na(dois_pub)]

# get journal articles that were preprints
dat_published_preprints=dat %>% filter(doi %in% dois_pub)
names(dat_published_preprints)
dat_published_preprints <- dat_published_preprints %>% select(doi,journal_title,altmetrics_score,date,citations_count,
                                                              cites_per_day,sjr,h_index,jci,if_2022,
                                                              if_10,if_5,if_1,if_any,Category_1,scimago_Q)
colnames(dat_published_preprints) <- paste0("subs_pub_",colnames(dat_published_preprints))
# dat_published_preprints <- dat_published_preprints %>% 
#   dplyr::select(-subs_pub_title_preferred,-subs_pub_abstract_preferred,-subs_pub_altmetrics_id,-subs_pub_journal_issn,
#                 -subs_pub_resulting_publication_doi,-subs_pub_preprint,-subs_pub_record_count,-subs_pub_published,
#                 -subs_pub_date_normal,-subs_pub_date_inserted,-subs_pub_published)

# join with original data
dat <- dat %>% left_join(dat_published_preprints, by = c("resulting_publication_doi"="subs_pub_doi"))



# CLeaning and filtering --------------------------------------------------



### Tidy up a few of the common words so they don't get picked up in the text analysis
sarswords <- unique(c("SARS-CoV-2","SARS-COV-2","Sars-CoV-2","sars-cov-2","SARS CoV-2","Sars COV-2",
                      "SARS CoV 2","Sars-CoV 2","SARS-CoV2","Sars-Cov-2"))
dat$title_preferred=sapply(X = 1:length(dat$title_preferred), function(x) stringi::stri_replace_all_regex(dat$title_preferred[x],
                                                                                                          pattern = sarswords,
                                                                                                          replacement = rep("SarsCoV2",length(sarswords)),
                                                                                                          vectorize_all = F))

dat$abstract_preferred=sapply(X = 1:length(dat$title_preferred), function(x) stringi::stri_replace_all_regex(dat$abstract_preferred[x],
                                                                                                             pattern = sarswords,
                                                                                                             replacement = rep("SarsCoV2",length(sarswords)),
                                                                                                             vectorize_all = F))

### COVID WORDS ###
covidwords <- unique(c("COVID-19","Covid-19","COVID 19","covid-19","covid 19", "COVID--19",
                       "Covid - 19","COVID- 19","Covid -19"))
dat$title_preferred=sapply(X = 1:length(dat$title_preferred), function(x) stringi::stri_replace_all_regex(dat$title_preferred[x],
                                                                                                          pattern = covidwords,
                                                                                                          replacement = rep("COVID19",length(covidwords)),
                                                                                                          vectorize_all = F))

dat$abstract_preferred=sapply(X = 1:length(dat$title_preferred), function(x) stringi::stri_replace_all_regex(dat$abstract_preferred[x],
                                                                                                             pattern = covidwords,
                                                                                                             replacement = rep("COVID19",length(covidwords)),
                                                                                                             vectorize_all = F))



# Identify papers not written primarily in English ------------------------

# get an english dictionary and add a couple of obvious pandemic-y words to it
engdict <- qdapDictionaries::GradyAugmented
engdict <- c(engdict,"covid19","sarscov2","coronavirus")


# create vector of lower case titles

t0=Sys.time()
vect_titles=lapply(dat$title_preferred,tolower)
print(Sys.time()-t0) # 4 seconds

t0=Sys.time()
tttt <- gsub('[[:punct:] ]+',' ',vect_titles[c(1:1000)])
print(Sys.time()-t0) # 0.02 seconds

# Function to quantify Englishness
checkEnglishness <- function(mystring){
  mywords=str_split(mystring,pattern=" ",simplify = F)[[1]]
  propeng=sum(mywords%in%engdict)/length(mywords)
  return(propeng)
}


# run over 1000 titles
t0=Sys.time()
vect_englishness=lapply(X = tttt,FUN = checkEnglishness)
print(Sys.time()-t0) # 10 seconds for 1000 papers

# get data frame of scores for visual inspection
englighness_df=data.frame(titles=unlist(dat$title_preferred[c(1:1000)]),
                          clean_titles=tttt,
                          eng=vect_englishness)

## From a visual inspection of false positives vs false negatives, 0.7 is an appropriate
## Threshold for Englishness


# Get Englishness scores for whole data set
t0=Sys.time()
vect_titles <- gsub('[[:punct:] ]+',' ',vect_titles)
print(Sys.time()-t0) # 10 seconds for 1000 papers
vect_englishness=lapply(X = vect_titles, FUN = checkEnglishness)
print(Sys.time()-t0) # 2.2 hours

# quick plot
unlist(vect_englishness) %>% hist(breaks=10)

# how many below 70
table(unlist(vect_englishness)<0.7) # 142445 

# filter data set by Englishness
dat <- dat[unlist(vect_englishness)>0.7,]

# update filter df
filtering_df <- rbind(filtering_df,c("Removed non-English articles",nrow(dat)))


# Bring in altmetrics -----------------------------------------------------

dat_alt <- readRDS("data/altmetric_res_all.rds") %>% janitor::clean_names()


# remove some spurious vars
dat_alt <- dat_alt %>% select(-column_label,-images_medium,-images_small,-images_large)


# Run function over main data frames
dat_alt <- checkConvertNumeric(df = dat_alt)
dat_alt <- dat_alt %>% rename(altmetrics_id=altmetric_id)


# join with main data
dat_main=dat %>% left_join(dat_alt)


# Save
saveRDS(dat_main,file = "data/dat_main.rds")
write_csv(x = filtering_df,file = "data/data_filtering.csv")
