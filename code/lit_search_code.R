install.packages("pubmedR")
install.packages("rcrossref")
install.packages("rAltmetric")

library(pubmedR)
library(rcrossref)
library(tidyverse)
library(janitor)
library(rAltmetric)


#  Conduct pubmed search --------------------------------------------------

# Pubmed API key

#' Instructions get an API key:
#' https://ncbiinsights.ncbi.nlm.nih.gov/2017/11/02/new-api-keys-for-the-e-utilities/
#' Register here: https://account.ncbi.nlm.nih.gov/

api_key <- ""

# Write query
query <- "COVID-19*[Title/Abstract] OR SARS-CoV-2*[Title/Abstract] 
AND symptoms*[Title/Abstract] AND persistent*[Title/Abstract] OR  
long COVID*[Title/Abstract] OR  post-COVID syndrome*[Title/Abstract] 
AND english[LA] AND Journal Article[PT] AND 2020:2022[DP]"

query <- "COVID-19*[Title/Abstract] OR SARS-CoV-2*[Title/Abstract] 
AND follow-up*[Title/Abstract] AND death*[Title/Abstract] OR  hospitalisation*[Title/Abstract] AND
follow-up*[Title/Abstract] OR  HES*[Title/Abstract] 
AND english[LA] AND Journal Article[PT] AND 2020:2022[DP]"

# How many docs?
res <- pmQueryTotalCount(query = query, api_key = api_key)
res$total_count

# API request for docs
docs <- pmApiRequest(query = query, limit = res$total_count, api_key = api_key)

# Convert to DF
df <- pmApi2df(docs) %>% janitor::clean_names()


# Query crossref for citation data ----------------------------------------

# Get citation counts
cites_df=cr_citation_count(df$di,url = "http://www.crossref.org/openurl/",
  key = "cboettig@ropensci.org",async = FALSE)

# Join
df <- cbind(df,cites_df) 
df <- df %>% rename(citations_count=count)


# Add journal impact factor data (not in public domain, from csv) ---------


# read in Impact Factor data
if_df=read_csv("data/Impactfactor2022 2.csv")

# Join on eissn
df <- df %>% left_join(if_df,by=c("sn"="eissn"))


# Add altmetrics data -----------------------------------------------------

# Function for querying altmetrics API
altmetrics_new <-
  function(doi = NULL,
           apikey = NULL,
           ...) {
    base_url <- "https://api.altmetric.com/v1/"
    args <- list(key = apikey)
    request <-
      httr::GET(paste0(base_url, "doi/",doi))
    if(httr::status_code(request) == 404) {
      stop("No metrics found for object")
    } else {
      httr::warn_for_status(request)
      results <-
        jsonlite::fromJSON(httr::content(request, as = "text"), flatten = TRUE)
      results <- rlist::list.flatten(results)
      class(results) <- "altmetric"
      results
      
    }
  }


### Batch altmetrics query function
alm <- function(x){
  out <- suppressWarnings(try(altmetric_data(altmetrics_new(doi = x,
                                                            apikey = '37c9ae22b7979124ea650f3412255bf9')),silent = TRUE))
  if(class(out) == "try-error"){
    return(NULL)
  }else{
    return(out)
  }
}  

### Get doi list
dois <- df$di[!is.na(df$di)] %>% as.list()

# pull altmetrics
alt_df <- map_df(dois, alm)

# get rid of all these authors!
nms=alt_df %>% colnames()
nms <- grep("authors",x = nms,value = T)
nms <- nms[3:length(nms)]
alt_df <- alt_df %>% select(-nms)

# join with original data
df <- df %>% left_join(alt_df,by=c("di"="doi"))

# impute the title 
df$ti <- stringr::str_to_sentence(df$ti)
df$title[is.na(df$title)] <- df$ti[is.na(df$title)]

# score to numeric
df$score <- as.numeric(df$score)

# Summary top papers ------------------------------------------------------

# Citations top 10
df %>% 
  arrange(-citations_count) %>% 
  slice_head(n = 10) %>% select(title,journal_name,di,authors1,citations_count)

# Altmetrics top 10
df %>% 
  arrange(-score) %>% 
  slice_head(n = 10) %>% select(title,journal_name,di,authors1,score)

