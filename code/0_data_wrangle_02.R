# clear env
rm(list=ls())
source("E:/Group/functions/load_packages.R")
source("E:/home/mw418/function_scripts/load_packages.R")

load_packages(c("tidyverse","ggplot2","ggthemes", "tm",
                "text2map","tidytext","topicmodels","Rtsne","rsvd", 
                "Rcpp","furrr","future", "OverReact"))

source(file = "code/0_functions.R")

#load data

# Save
dat <- readRDS("data/dat_main.rds")




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

# Create sparse matrices of titles ----------------------------------------
# mincount=1
# mydat=dat[1:10000,]

makeTidyDF <- function(mydat,textvar,mincount=100, 
                       removewords=c("introduction","methods","results","discussion","conclusion")
                       ){
  
  # assign new var
  mydat$dummy =pull(mydat,textvar)
  

  # get tidy df of titles
  df_text_tidy_title <- mydat %>% 
    dplyr::select(all_of("dummy"),unique_id) %>% 
    unnest_tokens(word,dummy) %>% 
    anti_join(get_stopwords()) %>% 
    anti_join(get_stopwords("es")) %>% 
    anti_join(get_stopwords("de")) %>% 
    anti_join(get_stopwords("fr")) %>% 
    anti_join(get_stopwords("nl")) %>% 
    anti_join(get_stopwords("ru")) %>% 
    filter(!str_detect(word,"[0-9]+"))  %>% 
    filter(!word%in%removewords)

  print("Phase 1 complete")
  
  # # get word counts
  # df_summ=df_text_tidy_title %>% 
  #   count(word)%>% 
  #   filter(n>mincount)
  # 
  
  # get word counts
  df_summ=df_text_tidy_title %>% 
    group_by(unique_id,word) %>% 
    summarise(n=n()) %>% 
    ungroup() %>% 
    group_by(word) %>% 
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

# mincount = 10000
# Run functions over all data
df_text_sparse_title <- makeTidyDF(mydat = dat,textvar = "title_preferred",mincount = 1000)
df_text_sparse_abstract <- makeTidyDF(mydat = dat,textvar = "abstract_preferred",mincount = 10000)
df_text_sparse_abstract_1000 <- makeTidyDF(mydat = dat,textvar = "abstract_preferred",mincount = 1000)
df_text_sparse_abstract_preprint<- makeTidyDF(mydat = dat %>% filter(preprint=="Preprint") ,
                                              textvar = "abstract_preferred",mincount = 1000)


df_text_sparse_abstract_1000@Dim
df_text_sparse_abstract@Dim


### Save ###
saveRDS(list(df_text_sparse_title=df_text_sparse_title,
             df_text_sparse_abstract=df_text_sparse_abstract,
             df_text_sparse_abstract_1000=df_text_sparse_abstract_1000,
             df_text_sparse_abstract_preprint=df_text_sparse_abstract_preprint),
        file = "data/sparse_dfs_1000_min.rds")



