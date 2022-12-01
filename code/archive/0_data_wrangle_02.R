# clear env
rm(list=ls())
source("E:/Group/functions/load_packages.R")
load_packages(c("tidyverse","ggplot2","ggthemes", "tm",
                "text2map","tidytext","topicmodels","Rtsne","rsvd", 
                "Rcpp","furrr","future", "OverReact"))

source(file = "code/0_functions.R")

#load data

# Save
dat <- readRDS("data/dat_main.rds")




# Identify papers not written primarily in English ------------------------


# clean corpus function
clean_corpus <- function(corpus){
  corpus <- tm_map(corpus, stripWhitespace)
  corpus <- tm_map(corpus, removePunctuation)
  corpus <- tm_map(corpus, content_transformer(tolower))
  corpus <- tm_map(corpus, removeWords, c(stopwords("en")))
  corpus <- tm_map(corpus, removeWords, c(stopwords("de")))
  corpus <- tm_map(corpus, removeWords, c(stopwords("fr")))
  corpus <- tm_map(corpus, removeWords, c(stopwords("nl")))
  corpus <- tm_map(corpus, removeWords, c(stopwords("ru")))

  return(corpus)
}

# Bags of words -----------------------------------------------------------


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

# Run on titles
titles_dtm_tidy_wide=createBagOfWords(dat = dat,textvar = "title_preferred",minfreq = 1000)
titles_dtm_tidy_wide <- titles_dtm_tidy_wide[,!grepl("X[^0-9]*",names(titles_dtm_tidy_wide))]

# titles_dtm_tidy_wide <- checkConvertNumeric(df = titles_dtm_tidy_wide)


# Save data  --------------------------------------------------------------



saveRDS(abstract_dtm_tidy_wide,file = "data/dat_text_abstracts_bow.rds")
saveRDS(titles_dtm_tidy_wide,file = "data/dat_text_titles_bow.rds")


