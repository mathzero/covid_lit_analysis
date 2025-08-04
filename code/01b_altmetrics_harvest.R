# ----------------------------------------------------------------------------------
# Altmetrics Data Harvesting Script with Resumption, Enhanced Features, and Progress Bar
# ----------------------------------------------------------------------------------

### 1. Install and Load Required Packages
required_packages <- c(
  "XML", "xml2", "rcrossref", "stringr", "dplyr", "rjson", 
  "httr", "rAltmetric", "purrr", "magrittr", "ggmap",
  "tidyverse", "medrxivr", "gtools", "doParallel", "foreach", 
  "rlist", "janitor", "devtools", "progressr"
)

# Install any missing packages
installed_packages <- installed.packages()[, "Package"]
missing_packages <- setdiff(required_packages, installed_packages)
if(length(missing_packages)) install.packages(missing_packages)

# Load all required packages
lapply(required_packages, require, character.only = TRUE)

# Install rAltmetric from GitHub if not already installed
if(!"rAltmetric" %in% installed_packages){
  devtools::install_github("ropensci/rAltmetric")
  library(rAltmetric)
}

# Load progressr package
library(progressr)

### 2. Define Data Paths and API Key
datapath <- "data/bq_exports_mar_2023/clean/"
output_path <- "data/altmetrics_harvest/"

# Create output directory if it doesn't exist
if(!dir.exists(output_path)) dir.create(output_path, recursive = TRUE)

# Load main data
dat_main <- readRDS(file.path(datapath, "dat_main.rds"))

# Define Altmetrics API key
altmetric_api_key <- '37c9ae22b7979124ea650f3412255bf9'  # Replace with your actual API key if different

### 3. Define Functions for Data Fetching and Processing

# Function to fetch altmetrics data for a single DOI
fetch_altmetrics <- function(doi, apikey) {
  base_url <- "https://api.altmetric.com/v1/doi/"
  request <- httr::GET(url = paste0(base_url, doi), query = list(key = apikey))
  
  if(httr::status_code(request) == 404) {
    return(NULL)  # No metrics found for this DOI
  }
  
  if(httr::http_error(request)) {
    warning(paste("HTTP error for DOI:", doi, "Status code:", httr::status_code(request)))
    return(NULL)
  }
  
  # Parse and flatten JSON response
  results <- jsonlite::fromJSON(httr::content(request, as = "text"), flatten = TRUE)
  results <- rlist::list.flatten(results)
  class(results) <- "altmetric"
  return(results)
}

# Wrapper function with error handling
alm <- function(doi) {
  tryCatch({
    data <- fetch_altmetrics(doi, apikey = altmetric_api_key)
    if(!is.null(data)) {
      altmetric_data <- altmetric::altmetric_data(data)
      return(altmetric_data)
    } else {
      return(NULL)
    }
  }, error = function(e) {
    message(paste("Error fetching DOI:", doi, " - ", e$message))
    return(NULL)
  })
}

### 4. Prepare List of DOIs
ids <- dat_main$doi %>% 
  na.omit() %>% 
  unique() %>% 
  as.list()

total_ids <- length(ids)

### 5. Determine Batching Parameters
batch_size <- 1000  # Decreased from 10,000 to 1,000
num_batches <- ceiling(total_ids / batch_size)

### 6. Check Existing RDS Files to Resume Processing
existing_files <- list.files(
  path = output_path, 
  pattern = "^altmetric_res_\\d+\\.rds$", 
  full.names = TRUE
)

# Extract batch numbers from existing filenames
processed_batches <- existing_files %>%
  basename() %>%
  str_extract("\\d+") %>%
  as.integer()

# Determine which batches need to be processed
batches_to_process <- setdiff(1:num_batches, processed_batches)

if(length(batches_to_process) == 0){
  message("All batches have already been processed.")
} else {
  ### 7. Setup Parallel Processing
  n_cores <- parallel::detectCores() - 2  # Reserve 2 cores for other tasks
  cl <- makeCluster(n_cores)
  registerDoParallel(cl)
  
  ### 8. Setup Progress Bar
  handlers(global = TRUE)
  handlers("progress")
  
  # Initialize the progress handler
  with_progress({
    p <- progressr::progressor(along = batches_to_process)
    
    ### 9. Process Batches in Parallel with Progress Bar
    foreach(i = batches_to_process, .packages = c("rAltmetric", "purrr", "magrittr", 
                                                  "ggmap", "dplyr", "rlist")) %dopar% {
                                                    myfloor <- (i - 1) * batch_size + 1
                                                    myceiling <- min(i * batch_size, total_ids)
                                                    current_ids <- ids[myfloor:myceiling]
                                                    
                                                    # Fetch altmetrics data for the current batch
                                                    results <- map_df(current_ids, alm)
                                                    
                                                    # Save results as RDS
                                                    saveRDS(object = results, file = file.path(output_path, paste0("altmetric_res_", i, ".rds")))
                                                    
                                                    # Update progress bar
                                                    p()
                                                  }
  })  # Removed 'progress = TRUE' argument
  
  ### 10. Stop Parallel Cluster
  stopCluster(cl)
}






### 11. Combine All Results

# List all RDS files in the output directory
filenames <- list.files(
  path = output_path, 
  pattern = "^altmetric_res_\\d+\\.rds$", 
  full.names = TRUE
)

# Read all RDS files into a list
myreslist <- lapply(filenames, readRDS)

# Find common variables across all data frames
common_vars <- Reduce(intersect, lapply(myreslist, names))

# Remove unwanted variables based on patterns
unwanted_patterns <- c("author", "editor", "publisher_subjects")
common_vars <- common_vars[!grepl(paste(unwanted_patterns, collapse = "|"), common_vars)]
common_vars <- common_vars[!grepl("[2-9]$", common_vars)]  # Remove variables ending with digits 2-9

# Function to select useful columns
selectVars <- function(dat, vars){
  dat %>% select(all_of(vars))
}

# Apply selection over all results
myreslist_select <- lapply(myreslist, selectVars, vars = common_vars)

# Bind all data frames into one
myresults <- bind_rows(myreslist_select, .id = "batch")

### 12. Convert Columns to Numeric Where Applicable

# Check if OverReact package is installed for numeric conversion
if("OverReact" %in% installed_packages){
  myresults <- OverReact::checkConvertNumeric(df = myresults)
} else {
  # Define a simple numeric conversion function if OverReact is not available
  myresults <- myresults %>% mutate(across(where(is.character), ~ as.numeric(.x)))
}

### 13. Clean Column Names
myresults <- myresults %>% janitor::clean_names()

### 14. Save Combined Results
saveRDS(object = myresults, file = file.path(output_path, "altmetric_res_all.rds"))

### 15. Generate Summary Report

# Calculate the number of records recovered and searched for
recovered_records <- nrow(myresults)
searched_records <- total_ids

# Create summary text
summary_text <- paste0(
  "Altmetrics Harvest Summary - ", Sys.Date(), "\n",
  "Total DOIs searched for: ", searched_records, "\n",
  "Total records recovered: ", recovered_records, "\n"
)

# Write summary to a text file
writeLines(summary_text, con = file.path(output_path, "harvest_summary.txt"))

# Print summary to console
cat(summary_text)