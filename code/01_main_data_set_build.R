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

dat_2020 <- read_csv(file="data/bq-results-20221124-2020.csv")
dat_2021 <- read_csv(file="data/bq-results-20221124-2021.csv")
dat_2022 <- read_csv(file="data/bq-results-20221124-2022.csv")


# bind all
dat <- rbind(dat_2020,dat_2021,dat_2022)
# remove
rm(dat_2020,dat_2021,dat_2022)

# define data frame of data filtering stages
filtering_df=data.frame(stage="Raw data set",n=nrow(dat))


# Main data cleaning ------------------------------------------------------


### Apply functions
dat <- dat %>% mutate_all(funs(makeNA)) %>% janitor::clean_names()
dat <- OverReact::checkConvertNumeric(df = dat)


# make dates be not bad
dat$date <- as.Date(dat$date_inserted)
dat <- dat %>% arrange(date)

# create days since publication
extractdate=as.Date("2022-11-24" )
dat$days_since_publication=as.numeric(extractdate-dat$date)

# ore accurate cites per dat
dat$cites_per_day <- dat$citations_count/dat$days_since_publication


# replace NAs with 0
dat$citations_count[is.na(dat$citations_count)] <- 0
dat$cites_per_day[is.na(dat$cites_per_day)] <- 0
dat <- dat %>% rename(altmetrics_score=score)
dat$altmetrics_score[is.na(dat$altmetrics_score)] <- 0

# add document ID
dat$unique_id <- as.character(1:nrow(dat))

head(dat)

# Create published var
dat <- dat %>% dplyr::mutate(published = case_when(!is.na(resulting_publication_doi) ~ 1,
                                                   TRUE ~ 0))


# Some filtering #

## Let's get rid of articles with no journal title (assuming they are non-journal articles)
is.na(dat$name) %>% table(dat$type)
dat <- dat %>% rename(journal_title=title)
dat <- dat %>% filter(!is.na(journal_title))
filtering_df <- rbind(filtering_df,c("Removed non-journal articles",nrow(dat)))



#  remove unicode (and rename abstract and title)
dat <- dat %>% rename(title_preferred=preferred_1)
dat$title_preferred <- lapply(dat$title_preferred,stringi::stri_trans_general,"latin-ascii")
dat <- dat %>% rename(abstract_preferred=preferred)
dat$abstract_preferred <- lapply(dat$abstract_preferred,stringi::stri_trans_general,"latin-ascii")
dat$title_preferred=as.character(dat$title_preferred)
dat$abstract_preferred=as.character(dat$abstract_preferred)

# Preprints ---------------------------------------------------------------

# The dimensions designation of preprint is imperfect, so we will identify others on the basis of journal name (and some prior knowledge)


### Propely identify preprints
preprint_names=unique(dat$journal_title[grepl("rxiv",tolower(dat$journal_title))])
preprint_names <- c(preprint_names,
                    unique(dat$journal_title[grepl("preprint",tolower(dat$journal_title))]))
# preprint_names <- c(preprint_names,
#                     unique(dat$journal_title[grepl("archive",tolower(dat$journal_title))]))
preprint_names <- c(preprint_names,"Research Square","Advance","ESSOAr","advance","EGUsphere",
                    "Beilstein Archives","Cambridge Open Engage","Frenxiv","Arabixiv")
preprint_names <- unique(c(preprint_names,dat$journal_title[dat$type=="preprint"]))
newpreprints <- case_when(dat$journal_title%in%c(preprint_names) ~ "Preprint",
                          T ~ "Journal article")

# check overlap
table(dat$type,newpreprints, exclude="none")
# Well that was a total waste of time



# Add impact factor -------------------------------------------------------


if_data <- read_csv("data/Impactfactor2022.csv")
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
dat$issn <- gsub("-","",dat$issn)
table(dat$issn%in%if_data$journal_issn)

# nearly half. Let's join
dat <- dat %>% left_join(if_data, by=c("issn"="journal_issn"))


# Some more filtering: on COVIDyness -----------------------------------------------------


sarswords=c("2019-nCoV", "COVID-19","SARS-CoV-2", "HCoV-2019","hcov" , "NCOVID-19", "coronavirus disease 2019",
            "corona virus disease 2019","severe acute respiratory syndrome coronavirus 2",
            "SarsCoV2","COVID19","2019-nCov","COVID 19", 
            "severe acute respiratory syndrome corona virus 2", "Wuhan coronavirus","China coronavirus",
            "Wuhan corona virus","China corona virus", "novel coronavirus","novel corona virus") 

falsepos=grepl(pattern = paste(tolower(sarswords),collapse = "|"),x =dat$title_preferred,ignore.case = T )
table(falsepos)
table(falsepos,dat$type)

# visual inspection of the titles that have been filtered 
dat_fp_titles <- dat$title_preferred[!falsepos] # yeah the vast majority of these look like false positives

# filter down dat to only those papers that meet the more stringent criteria
dat <- dat[falsepos,]

# define data frame of data filtering stages
filtering_df <- rbind(filtering_df,c("Removed non-COVID-19-relevant articles",nrow(dat)))



# Identify papers not written primarily in English ------------------------

# get an english dictionary and add a couple of obvious pandemic-y words to it
engdict <- qdapDictionaries::GradyAugmented
engdict <- c(engdict,"covid19","sarscov2","coronavirus")


# create vector of lower case titles

t0=Sys.time()
vect_titles=lapply(dat$title_preferred,tolower)
print(Sys.time()-t0) # 14 seconds

t0=Sys.time()
tttt <- gsub('[[:punct:] ]+',' ',vect_titles[c(1:1000)])
print(Sys.time()-t0) # 0.5 seconds

# Function to quantify Englishness
checkEnglishness <- function(mystring){
  mywords=str_split(mystring,pattern=" ",simplify = F)[[1]]
  propeng=sum(mywords%in%engdict)/length(mywords)
  return(propeng)
}


# run over 1000 titles
t0=Sys.time()
vect_englishness=lapply(X = tttt,FUN = checkEnglishness)
print(Sys.time()-t0) # 7.7 seconds for 1000 papers

# get data frame of scores for visual inspection
englighness_df=data.frame(titles=unlist(dat$title_preferred[c(1:1000)]),
                          clean_titles=tttt,
                          eng=unlist(vect_englishness))

## From a visual inspection of false positives vs false negatives, 0.5 is an appropriate
## Threshold for Englishness
thresh=0.5

# Get Englishness scores for whole data set
t0=Sys.time()
vect_titles <- gsub('[[:punct:] ]+',' ',vect_titles)
print(Sys.time()-t0) # 10 seconds for 1000 papers
vect_englishness=lapply(X = vect_titles, FUN = checkEnglishness)
print(Sys.time()-t0) # 2.2 hours

# quick plot
unlist(vect_englishness) %>% hist(breaks=10)

# how many below 70
table(unlist(vect_englishness)<thresh) # 142445 

# filter data set by Englishness
dat_non_eng <- dat[unlist(vect_englishness)<=thresh,]
# visual inspection shows very few false negatives in this data
# let's go ahead and remove them
dat <- dat[unlist(vect_englishness)>thresh,]
rm(dat_non_eng)

# update filter df
filtering_df <- rbind(filtering_df,c("Removed non-English articles",nrow(dat)))





# CLeaning COVID-19 words  --------------------------------------------------


### Tidy up a few of the common words so they don't get picked up in the text analysis
sarswords <- unique(c("SARS-CoV-2","SARS-COV-2","Sars-CoV-2","sars-cov-2","SARS CoV-2","Sars COV-2",
                      "SARS CoV 2","Sars-CoV 2","SARS-CoV2","Sars-Cov-2","SarsCov2","SarsCov-2","Sars Cov2"))
dat$title_preferred=sapply(X = 1:length(dat$title_preferred), function(x) stringi::stri_replace_all_regex(dat$title_preferred[x],
                                                                                                          pattern = sarswords,
                                                                                                          replacement = rep("SarsCoV2",length(sarswords)),
                                                                                                          vectorize_all = F))

dat$abstract_preferred=sapply(X = 1:length(dat$abstract_preferred), function(x) stringi::stri_replace_all_regex(dat$abstract_preferred[x],
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

dat$abstract_preferred=sapply(X = 1:length(dat$abstract_preferred), function(x) stringi::stri_replace_all_regex(dat$abstract_preferred[x],
                                                                                                                pattern = covidwords,
                                                                                                                replacement = rep("COVID19",length(covidwords)),
                                                                                                                vectorize_all = F))








# Bring in altmetrics -----------------------------------------------------

dat_alt <- readRDS("data/altmetrics_harvest/altmetric_res_all.rds") %>% janitor::clean_names()


# remove some spurious vars
dat_alt <- dat_alt %>% select(-column_label,-images_medium,-images_small,-images_large)


# Run function over main data frames
dat_alt <- checkConvertNumeric(df = dat_alt)
dat <- dat %>% rename(altmetric_id=id_1)

# join with main data
dat_main=dat %>% left_join(dat_alt %>% select(-pmid, -type, -arxiv_id, -altmetric_id))

dat_main$score %>% table(exclude="none")
dat_main %>% ggplot(aes(x=score,y=altmetrics_score)) +geom_point()
cor((dat_main$score),(dat_main$altmetrics_score),use = "complete.obs")

# Save
saveRDS(dat_main,file = "data/dat_main.rds")
write_csv(x = filtering_df,file = "data/data_filtering.csv")

# dat_main=readRDS("data/dat_main.rds")


# Citations -----------------------------------------------------------------


# Import citations (in horrible hacked json-csv hybrid format, because I can't use bigquery competently)
citations <- read_csv("data/citations.csv")

citations$cleaned=gsub("\\[\\{'id': '","",citations$citations)
citations$cleaned=gsub("', 'year': [0-9]*\\}","",citations$cleaned)
citations$cleaned=gsub("\\{'id': '","",citations$cleaned)
citations$cleaned=gsub("\\[","",citations$cleaned)
citations$cleaned=gsub("\\]","",citations$cleaned)




# split json column out
citations_splitlist=str_split(citations$cleaned, pattern="\n ", n = Inf, simplify = FALSE)
names(citations_splitlist)=citations$id
listobj=citations_splitlist[[1]]
nm=names(citations_splitlist)[[1]]

# function to turn lists into dfs with the cited paper as node_1
makeDf <- function(listobj){
  out=data.frame(node_1=rep("Dummy",length(listobj)),node_2=unlist(listobj))
  return(out)
}

# run function over citations_splitlist
citations_splitlist_dfs=lapply(X = citations_splitlist,FUN = makeDf)

# Loop and replace Dummy with names of cited paper
for(i in 1:length(citations_splitlist_dfs)){
  print(i)
  citations_splitlist_dfs[[i]]$node_1=names(citations_splitlist_dfs)[[i]]
}

citations_long=bind_rows(citations_splitlist_dfs)

# summarise by frequncy of appearance in citation data
citations_long_suumary_1=citations_long %>% group_by(node_1) %>% summarise(n=n()) %>% arrange(-n)
citations_long_suumary_2=citations_long %>% group_by(node_2) %>% summarise(n=n())%>% arrange(-n)

# save
saveRDS(citations_long,file = "data/citations_long.rds")
# citations_long <- readRDS("data/citations_long.rds")













# Messing about with graphs -----------------------------------------------




library(stringr)
library(igraph)
library(tidygraph)
library(ggraph)
library(magrittr)

# join with main data set
citations_long_join <- citations_long %>%
  left_join(dat_main %>% select(id,citations_count,altmetrics_score,type,year), by=c("node_1"="id"))

# create data set for graph plotting
citations_long_g=citations_long_join[1:100000,] %>%
  filter(year==2020,!is.na(type))


# create graph
g <- citations_long_g %>% 
  select(node_1,node_2) %>% 
  as.matrix() %>%
  graph.edgelist(directed = T) %>%
  as_tbl_graph() %>%
  activate("edges") %>%
  mutate(type=citations_long_g$type) %>%
  activate("nodes") %>%
  rename(Name = name) %>%
  mutate(Component = group_components()) %>%
  filter(Component == names(table(Component))[which.max(table(Component))])

# make plot
p <- ggraph(g, layout = 'lgl') +
  geom_edge_fan(alpha = 0.1, aes(color=factor(type))) +
  theme_graph() +
  labs(edge_color="")   + 
  scale_edge_colour_manual(values = c('black', 'red'))

p

ggsave(filename = "plots/citation_network.png",p, width = 10,height = 10,units = "in",dpi = 300)
ggsave(filename = "plots/citation_network.pdf",p)

OverReact::saveREACTplot(p = p,figpath = "/plots/",
                         filename = "citation_network",
                         width = 10,height = 10,savePDF = T)




