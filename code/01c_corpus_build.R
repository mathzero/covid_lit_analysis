# clear env
rm(list=ls())
# source("E:/Group/functions/load_packages.R")

function_script_path ="/Users/mw418/codebase/misc/function_scripts/"
# function_script_path ="E:/home/mw418/function_scripts/"

#' Source any functions from the local file
source(paste0(function_script_path,"/load_packages.R"))
load_packages(c("tidyverse","ggplot2","ggthemes", "tm",
                "text2map","tidytext","topicmodels","Rtsne","rsvd", 
                "Rcpp","furrr","future", "OverReact"))

pacman::p_load(tidyverse,ggplot2,ggthemes, tm,
                               text2map,tidytext,topicmodels,Rtsne,rsvd, 
                               Rcpp,furrr,future, OverReact)

source(file = "code/0_functions.R")

#load data

# Save
dat <- readRDS("data/bq_exports_mar_2023/clean/dat_main.rds")
dat_abs <- readRDS("data/bq_exports_mar_2023/clean/dat_main_abs.rds")
# dat <- dat %>% left_join(dat_abs %>% select(id,abstract))


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

# function for cleaning out hytml tags and other crap
cleanFun <- function(htmlString) {
  out=gsub("<.*?>", "", htmlString)
  out=gsub("\n", "", out)
  out=gsub("  ", " ", out)
  return(out)
}


makeTidyDF <- function(mydat,textvar = "abstract_preferred",mincount=100, 
                       removewords=c("introduction","methods","results","method","results","discussion","background",
                                     "conclusion","conclusions","objective","abstract","summary")
                       ){
  
  # assign new var and clean
  mydat$dummy =cleanFun(pull(mydat,textvar))

  
  
  # get tidy df of titles
  df_text_tidy_title_1 <- mydat %>% 
    dplyr::select(all_of("dummy"),id) %>% 
    unnest_tokens(word,dummy) %>% 
    anti_join(get_stopwords()) %>% 
    anti_join(get_stopwords("es")) %>% 
    anti_join(get_stopwords("de")) %>% 
    anti_join(get_stopwords("fr")) %>% 
    anti_join(get_stopwords("nl")) %>% 
    anti_join(get_stopwords("ru")) %>% 
    filter(!str_detect(word,"[0-9]+"))  %>% 
    filter(!word%in%removewords)
  
  # Bigrams
  # get tidy df of titles
  df_text_tidy_title_2 <- mydat %>% 
    dplyr::select(all_of("dummy"),id) %>% 
    unnest_tokens(word,dummy,token = "ngrams",n=2) %>% 
    anti_join(get_stopwords()) %>% 
    anti_join(get_stopwords("es")) %>% 
    anti_join(get_stopwords("de")) %>% 
    anti_join(get_stopwords("fr")) %>% 
    anti_join(get_stopwords("nl")) %>% 
    anti_join(get_stopwords("ru")) %>% 
    filter(!str_detect(word,"[0-9]+"))  %>% 
    filter(!word%in%removewords)

  # filter bigrams with common stopwords
  swc <- paste(paste0("\\b", stopwords::stopwords(), "\\b"), collapse = "|")
  df_text_tidy_title_2 <- df_text_tidy_title_2[str_detect(df_text_tidy_title_2$word, swc) == FALSE, ] #select rows without stopwords
  
  # Trigrams..?
  # 
  # # get tidy df of titles
  # df_text_tidy_title_3 <- mydat %>% 
  #   dplyr::select(all_of("dummy"),id) %>% 
  #   unnest_tokens(word,dummy,token = "ngrams",n=3) %>% 
  #   anti_join(get_stopwords()) %>% 
  #   anti_join(get_stopwords("es")) %>% 
  #   anti_join(get_stopwords("de")) %>% 
  #   anti_join(get_stopwords("fr")) %>% 
  #   anti_join(get_stopwords("nl")) %>% 
  #   anti_join(get_stopwords("ru")) %>% 
  #   filter(!str_detect(word,"[0-9]+"))  %>% 
  #   filter(!word%in%removewords)
  # 
  # # remove trigrams featuring common words  
  # df_text_tidy_title_3 <- df_text_tidy_title_3[str_detect(df_text_tidy_title_3$word, swc) == FALSE, ] #select rows without stopwords
  # 
  
  # bind together
  df_text_tidy_title <- rbind(df_text_tidy_title_1,
                              df_text_tidy_title_2
                              # df_text_tidy_title_3
                              )
  
  
  
  print("Phase 1 complete")
  
  # # get word counts
  # df_summ=df_text_tidy_title %>% 
  #   count(word)%>% 
  #   filter(n>mincount)
  # 
  
  # get word counts
  df_summ=df_text_tidy_title %>% 
    group_by(id,word) %>% 
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
    count(id,word) %>% 
    cast_sparse(id,word,n)
  
  # output
  return(df_text_sparse_title)
}


# Run functions over all data
df_text_sparse_title <- makeTidyDF(mydat = dat,textvar = "title_preferred",mincount = 1000)
# df_text_sparse_abstract <- makeTidyDF(mydat = dat,textvar = "abstract_preferred",mincount = 10000)
df_text_sparse_abstract_1000 <- makeTidyDF(mydat = dat_abs,textvar = "abstract_preferred",mincount = 1000)
# df_text_sparse_abstract_preprint<- makeTidyDF(mydat = dat %>% filter(preprint=="Preprint") ,
#                                               textvar = "abstract_preferred",mincount = 1000)


df_text_sparse_title@Dim
# df_text_sparse_abstract@Dim
df_text_sparse_abstract_1000@Dim

### Save ###
saveRDS(list(df_text_sparse_title=df_text_sparse_title,
             # df_text_sparse_abstract=df_text_sparse_abstract,
             df_text_sparse_abstract_1000=df_text_sparse_abstract_1000
             # df_text_sparse_abstract_preprint=df_text_sparse_abstract_preprint
             ),
        file = "data/sparse_dfs_1000_min.rds")

