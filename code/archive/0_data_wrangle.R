# clear env
rm(list=ls())
source("E:/Group/functions/load_packages.R")
load_packages(c("tidyverse","ggplot2","ggthemes", 
                "text2map","tidytext","topicmodels","Rtsne","rsvd", 
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
dat_4 <- read_csv("data/COVID publications table_Tab 1_Table_2022_Q1.csv")


# combine all data
dat <- rbind(dat_1,dat_2,dat_3,dat_4)

### Main data cleaning ###

### Aply functions
dat <- dat %>% mutate_all(funs(makeNA)) %>% janitor::clean_names()
dat <- checkConvertNumeric(df = dat)


# make dates be not bad
dat$date_normal <- str_replace(string = dat$date_normal,pattern = "Sept",replacement = "Sep")
dat$date <- parse_date(dat$date_normal,format = "%d %b %Y")
dat <- dat %>% arrange(date)

# ore accurate cites per dat
dat$cites_per_day <- dat$citations_count/dat$days_since_publication



# replace NAs with 0
dat$citations_count[is.na(dat$citations_count)] <- 0
dat$cites_per_day[is.na(dat$cites_per_day)] <- 0
dat$altmetrics_score[is.na(dat$altmetrics_score)] <- 0

# add document ID
dat$unique_id <- as.character(1:nrow(dat))


# Create published var
dat <- dat %>% dplyr::mutate(published = case_when(!is.na(resulting_publication_doi) ~ 1,
                                                   TRUE ~ 0))


# Some filtering #

## Let's get rid of articles with no journal title (assuming they are non-journal articles)
is.na(dat$journal_title) %>% table(dat$preprint)
dat <- dat %>% filter(!is.na(journal_title))



### Tidy up a few of the common words so they don't get picked up in the text analysis
sarswords <- unique(c("SARS-CoV-2","SARS-COV-2","Sars-CoV-2","sars-cov-2",
                      "SARS CoV 2","Sars-CoV 2","sars-cov-2","SARS-CoV2"))
dat$title_preferred=sapply(X = 1:length(dat$title_preferred), function(x) stringi::stri_replace_all_regex(dat$title_preferred[x],
                                                                                                          pattern = sarswords,
                                                                                                          replacement = rep("SarsCoV2",length(sarswords)),
                                                                                                          vectorize_all = F))

dat$abstract_preferred=sapply(X = 1:length(dat$title_preferred), function(x) stringi::stri_replace_all_regex(dat$abstract_preferred[x],
                                                                                                             pattern = sarswords,
                                                                                                             replacement = rep("SarsCoV2",length(sarswords)),
                                                                                                             vectorize_all = F))

### COVID WORDS ###
covidwords <- unique(c("COVID-19","Covid-19","COVID 19","covid-19","covid 19"))
dat$title_preferred=sapply(X = 1:length(dat$title_preferred), function(x) stringi::stri_replace_all_regex(dat$title_preferred[x],
                                                                                                          pattern = covidwords,
                                                                                                          replacement = rep("COVID19",length(covidwords)),
                                                                                                          vectorize_all = F))

dat$abstract_preferred=sapply(X = 1:length(dat$title_preferred), function(x) stringi::stri_replace_all_regex(dat$abstract_preferred[x],
                                                                                                             pattern = covidwords,
                                                                                                             replacement = rep("COVID19",length(covidwords)),
                                                                                                             vectorize_all = F))


#  remove unicode
dat$title_preferred <- lapply(dat$title_preferred,stringi::stri_trans_general,"latin-ascii")
dat$abstract_preferred <- lapply(dat$abstract_preferred,stringi::stri_trans_general,"latin-ascii")



# Bring in altmetrics -----------------------------------------------------

dat_alt <- readRDS("data/altmetric_res_all.rds") %>% janitor::clean_names()

# remove some spurious vars
dat_alt <- dat_alt %>% select(-column_label,-images_medium,-images_small,-images_large)


# Run function over main data frames
dat_alt <- checkConvertNumeric(df = dat_alt)
dat_alt <- dat_alt %>% rename(altmetrics_id=altmetric_id)


# join with main data
dat_main=dat %>% left_join(dat_alt)



# Bring in impact factor --------------------------------------------------


# read in Impact Factor data
if_df=read.csv("data/Impactfactor2022.csv")
if_df$journal_issn <- gsub("-","",if_df$issn)
if_df$journal_issn[if_df$journal_issn=="N/A" ] <- NA_character_
if_df <- if_df %>% filter(!is.na(journal_issn))

# Join on eissn
dat_main <- dat_main %>% left_join(if_df,by=c("journal_issn"))

# Save
saveRDS(dat_main,file = "data/dat_main.rds")

# Bags of words -----------------------------------------------------------

# clean corpus function
clean_corpus <- function(corpus){
  corpus <- tm_map(corpus, stripWhitespace)
  corpus <- tm_map(corpus, removePunctuation)
  corpus <- tm_map(corpus, content_transformer(tolower))
  corpus <- tm_map(corpus, removeWords, c(stopwords("en")))
  
  return(corpus)
}

# Create a function to turn the text data into a bag of words (will take ages for abstracts)
createBagOfWords <- function(dat,textvar="title_preferred",minfreq=1000){
  
  # assign identifier
  dat$document=as.character(1:nrow(dat))
  
  t0=Sys.time()
  # Make vector
  titles <- VectorSource(pull(dat,textvar))
  
  # Make corpus
  titles_corpus <- VCorpus(titles)
  
  # Apply function
  titles_corpus <- clean_corpus(titles_corpus)
  
  print("Stage 1 complete")
  print(Sys.time()-t0)
  
  # Convert to document-term matrix
  titles_dtm <- DocumentTermMatrix(titles_corpus)
  titles_dtm_tidy <- tidytext::tidy(titles_dtm)
  # remove unicodes just in case
  titles_dtm_tidy <- titles_dtm_tidy[!grepl("X[^0-9]*",titles_dtm_tidy$term),]
  titles_dtm_tidy <- titles_dtm_tidy %>% left_join(dat %>% select(unique_id, document))
  max(titles_dtm_tidy$document, na.rm=T)
  min(titles_dtm_tidy$document, na.rm=T)
  # length(unique(titles_dtm_tidy$term))
  # get frequency table of terms
  # titles_dtm_tidy_tab <- 
  titles_dtm_tidy_df <- titles_dtm_tidy$term %>% table() %>% as.matrix() %>% as.data.frame.matrix()
  titles_dtm_tidy_df$word = rownames(titles_dtm_tidy_df)
  colnames(titles_dtm_tidy_df) <- c("count","word")
  
  # minfreq=1000
  # get frequent words (more than 50 papers)
  freq_words <- titles_dtm_tidy_df %>%  filter(count>=minfreq) %>% 
    select(word) %>% unlist() %>% as.character()
  
  print("Stage 2 complete")
  print(Sys.time()-t0)
  
  freq_words <- setdiff(freq_words,"document")
  freq_words_list <- split(x = freq_words,f = ceiling(seq_along(freq_words)/1000))
  
  # get a df with only commonly occurring words
  
  ## Loop over list to avoid overloading memory
  res_list=list()
  for (i in 1: length(freq_words_list)){
    print(i)
    res_list[[i]] <- titles_dtm_tidy %>% filter(term %in% freq_words_list[[i]]) %>% 
      tidyr::pivot_wider(id_cols = "unique_id",  
                         names_from = "term",
                         values_from = "count")
    
  }
  
  # join all rows back together
  titles_dtm_tidy_wide <- (res_list) %>% reduce(full_join, by ="unique_id")
  
  
  print("Stage 3 complete")
  print(Sys.time()-t0)
  
  # Tidy up 
  # COmbine all the covids
  # titles_dtm_tidy_wide$covid19 <- titles_dtm_tidy_wide$covid+titles_dtm_tidy_wide$covid19+titles_dtm_tidy_wide$`covid‐19`
  # titles_dtm_tidy_wide <- titles_dtm_tidy_wide %>% select(-covid, -`covid‐19`)
  
  # Replace NAs with 0
  titles_dtm_tidy_wide[is.na(titles_dtm_tidy_wide)] <- 0
  
  titles_dtm_tidy_wide <-  checkConvertNumeric(df = titles_dtm_tidy_wide)
  
  
  
  return(titles_dtm_tidy_wide)
}

# Run on abstracts
abstract_dtm_tidy_wide=createBagOfWords(dat = dat,textvar = "abstract_preferred",minfreq = 10000)
abstract_dtm_tidy_wide <- abstract_dtm_tidy_wide[,!grepl("X[^0-9]*",names(abstract_dtm_tidy_wide))]
names(abstract_dtm_tidy_wide)
# Run on titles
titles_dtm_tidy_wide=createBagOfWords(dat = dat,textvar = "title_preferred",minfreq = 1000)
# titles_dtm_tidy_wide <- checkConvertNumeric(df = titles_dtm_tidy_wide)
names(titles_dtm_tidy_wide)


# Save data  --------------------------------------------------------------



saveRDS(abstract_dtm_tidy_wide,file = "data/dat_text_abstracts_bow.rds")
saveRDS(titles_dtm_tidy_wide,file = "data/dat_text_titles_bow.rds")




# Create sparse matrices of titles ----------------------------------------
mincount=1
# mydat=dat[1000,]
makeTidyDF <- function(mydat,textvar,mincount=100){
  
  # assign new var
  mydat$dummy =pull(mydat,textvar)
  
  # get tidy df of titles
  df_text_tidy_title <- mydat %>% 
    unnest_tokens(word,dummy) %>% 
    anti_join(get_stopwords()) %>% 
    anti_join(get_stopwords("es")) %>% 
    anti_join(get_stopwords("de")) %>% 
    anti_join(get_stopwords("fr")) %>% 
    anti_join(get_stopwords("nl")) %>% 
    anti_join(get_stopwords("ru")) %>% 
    filter(!str_detect(word,"[0-9]+"))
  
  print("Phase 1 complete")
  
  # get word counts
  df_summ=df_text_tidy_title %>% 
    count(word)%>% 
    filter(n>mincount)
  
  # Filter by word counts
  df_text_tidy_title <- df_text_tidy_title %>% filter(word %in% df_summ$word)
  
  print("Phase 2 complete")

  # get sparse df
  df_text_sparse_title <- 
    df_text_tidy_title %>% 
    count(unique_id,word) %>% 
    cast_sparse(unique_id,word,n)
  
  # output
  return(df_text_sparse_title)
}

mincount = 10000
# Run functions over all data
df_text_sparse_title <- makeTidyDF(mydat = dat,textvar = "title_preferred",mincount = 1000)
df_text_sparse_abstract <- makeTidyDF(mydat = dat,textvar = "abstract_preferred",mincount = 1000)
df_text_sparse_title_2020<- makeTidyDF(mydat = dat %>% filter(date<as.Date("2020-06-01")) ,
                                       textvar = "title_preferred",mincount = 1000)
df_text_sparse_abstract_2020<- makeTidyDF(mydat = dat %>% filter(date<as.Date("2020-06-01")) ,
                                          textvar = "abstract_preferred",mincount = 1000)
df_text_sparse_abstract_preprint<- makeTidyDF(mydat = dat %>% filter(preprint=="Preprint") ,
                                              textvar = "abstract_preferred",mincount = 1000)
df_text_sparse_abstract_preprint_2020<- makeTidyDF(mydat = dat %>% filter(preprint=="Preprint",
                                                                          date<as.Date("2020-06-01")) ,
                                              textvar = "abstract_preferred",mincount = 1000)


### Save ###
saveRDS(list(df_text_sparse_title=df_text_sparse_title,
             df_text_sparse_abstract=df_text_sparse_abstract,
             df_text_sparse_title_2020=df_text_sparse_title_2020,
             df_text_sparse_abstract_2020=df_text_sparse_abstract_2020,
             df_text_sparse_abstract_preprint=df_text_sparse_abstract_preprint,
             df_text_sparse_abstract_preprint_2020=df_text_sparse_abstract_preprint_2020),
        file = "data/sparse_dfs_1000_min.rds")

