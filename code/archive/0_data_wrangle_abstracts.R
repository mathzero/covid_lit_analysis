# clear env
rm(list=ls())
source("E:/Group/functions/load_packages.R")
load_packages(c("dplyr", "OverReact","tidyverse","qdap","tm","wordcloud","ggplot2","ggthemes", 
                "text2map","tidytext"))

# # devtools::install_github(repo = "mathzero/OverReact")
# pacman::p_load(dplyr, OverReact,tidyverse,qdap,tm,wordcloud,ggplot2,ggthemes, 
#                text2map,tidytext)

# read in data
dat_1 <- read_csv("data/COVID publications table_Tab 1_Table_2020.csv")
dat_2 <- read_csv("data/COVID publications table_Tab 1_Table_2021_Q1_Q2.csv")
dat_3 <- read_csv("data/COVID publications table_Tab 1_Table_2021_Q3_Q4.csv")
dat_4 <- read_csv("data/COVID publications table_Tab 1_Table_2022_Q1.csv")


# combine all data
dat <- rbind(dat_1,dat_2,dat_3,dat_4)
dat$document <- as.character(1:nrow(dat))
# replace null values in dat with NA
makeNA <- function(x) str_replace(x,"null",NA_character_)
dat <- dat %>% mutate_all(funs(makeNA))

dat$altmetrics.score <- as.numeric(dat$altmetrics.score)
dat$`Days since publication` <- as.numeric(dat$`Days since publication`)
dat$citations_count <- as.numeric(dat$citations_count)
dat$`Citations per day` <- as.numeric(dat$`Citations per day`)
# dat$metrics.relative_citation_ratio <- as.numeric(dat$metrics.relative_citation_ratio)

# make dates be not bad
dat$date_normal <- str_replace(string = dat$date_normal,pattern = "Sept",replacement = "Sep")
dat$date <- parse_date(dat$date_normal,format = "%d %b %Y")
dat <- dat %>% arrange(date)

# ore accurate cites per dat
dat$cites_per_day <- dat$citations_count/dat$`Days since publication`


# replace NAs with 0
dat$citations_count[is.na(dat$citations_count)] <- 0
dat$cites_per_day[is.na(dat$cites_per_day)] <- 0
dat$altmetrics.score[is.na(dat$altmetrics.score)] <- 0

# Abstract analysis -------------------------------------------------------

dat_filter <- dat %>% filter(date < as.Date("2020-07-01"),date >= as.Date("2020-03-01"))

# Make vector
titles <- VectorSource(dat_filter$abstract.preferred)

# Make corpus
titles_corpus <- VCorpus(titles)

# clean corpus function
clean_corpus <- function(corpus){
  corpus <- tm_map(corpus, stripWhitespace)
  corpus <- tm_map(corpus, removePunctuation)
  corpus <- tm_map(corpus, content_transformer(tolower))
  corpus <- tm_map(corpus, removeWords, c(stopwords("en")))
  return(corpus)
}

# Apply function
titles_corpus <- clean_corpus(titles_corpus)

# Convert to document-term matrix
titles_dtm <- DocumentTermMatrix(titles_corpus)
titles_dtm_tidy <- tidytext::tidy(titles_dtm)
max(titles_dtm_tidy$document, na.rm=T)
min(titles_dtm_tidy$document, na.rm=T)
# length(unique(titles_dtm_tidy$term))

# get frequency table of terms
titles_dtm_tidy_tab <- titles_dtm_tidy$term %>% table()
titles_dtm_tidy_df <- as.data.frame.matrix(as.matrix(titles_dtm_tidy_tab))
titles_dtm_tidy_df$word = rownames(titles_dtm_tidy_df)
colnames(titles_dtm_tidy_df) <- c("count","word")
# get frequent words (more than 50 papers)
freq_words <- titles_dtm_tidy_df %>%  filter(count>=50) %>% 
  select(word) %>% unlist() %>% as.character()

# get a df with only commonly occurring words
titles_dtm_tidy_wide <- titles_dtm_tidy %>% 
  rename(document_number=document) %>% 
  filter(term %in% freq_words) %>% 
  tidyr::pivot_wider(id_cols = "document_number",  
                     names_from = "term",
                     values_from = "count",
                     names_repair="unique")

dat_filter$document_number <- as.character(1:nrow(dat_filter))



# Join with main data set
titles_dtm_tidy_wide <- dat_filter %>%select(id,document_number) %>% 
  left_join(titles_dtm_tidy_wide, by = "document_number")
titles_dtm_tidy_wide[is.na(titles_dtm_tidy_wide)] <- 0
head(titles_dtm_tidy_wide)

# Tidy up 
# COmbine all the covids
titles_dtm_tidy_wide$covid19 <- titles_dtm_tidy_wide$covid+titles_dtm_tidy_wide$covid19
titles_dtm_tidy_wide <- titles_dtm_tidy_wide %>% select(-covid)


# join with words
dat_all <- dat_filter %>% dplyr::mutate(published = case_when(!is.na(resulting_publication_doi) ~ 1,
                                                              TRUE ~ 0)) %>% 
  dplyr::select(id,document_number, published, cites_per_day,
                citations_count,altmetrics.score,
                resulting_publication_doi,date,preprint,`Days since publication`,
                title.preferred)  %>% 
  rename(date_published=date) %>% 
  left_join(titles_dtm_tidy_wide,by=c("id","document_number"))



# reorder by date
dat_all <- dat_all[order(dat_all$date_published,na.last = T,decreasing = T),]

# remove dupes
dat_all <- dat_all[!duplicated(dat_all$id),]

# reorder correctly by date
dat_all <- dat_all[order(dat_all$date_published,na.last = T,decreasing = F),]

# save
saveRDS(dat_all,file = "data/dat_abstracts_slice.rds")


