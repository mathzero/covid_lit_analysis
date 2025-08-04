
#' First clear the environment of variables
rm(list=ls(all=TRUE))

outpath <- paste0(getwd(),"/output/")
figpath <- paste0(getwd(),"/plots/")
datapath <- paste0(getwd(),"/data/")
# function_script_path="/Users/mathzero/Library/CloudStorage/GoogleDrive-mattdwhitaker@gmail.com/My Drive/Imperial/PhD/REACT2_analysis_in_progress/RESULTS/function_scripts/"
function_script_path ="/Users/mw418/analysis/RESULTS/function_scripts/"
# function_script_path ="E:/home/mw418/function_scripts/"

#' Source any functions from the local file
source(paste0(function_script_path,"/load_packages.R"))
source(paste0(function_script_path,"/cats_and_covs.R"))
source(paste0(function_script_path,"/create_subfolder.R"))
# 
# source("E:/Group/react2_study5/report_phases_combined/projects/function_scripts/create_subfolder.R")
# update.packages("Rcpp",repos="http://se-r.sm.med.ic.ac.uk", dependencies = TRUE )
#' 
#' #' Pull in packages needed
#' package.list <- c("prevalence","mgcv","knitr","dplyr","factoextra","tidyr",
#'                   "ggplot2","gdata","ggsci", "RColorBrewer", "tidyverse", "lubridate", 
#'                   "ComplexHeatmap","gganimate","ggnetwork","ppcor","topicmodels",
#'                   "tidytext", "quanteda", "widyr", "igraph", "ggraph","stm","ldatuning",
#'                   "readr","ggthemes", "questionr", "gridExtra","qdapDictionaries",
#'                   "Rtsne","rsvd", "geometry","Rcpp","furrr","future","tictoc",
#'                   "patchwork", "OverReact", "topicmodels","ldatuning")
pacman::p_load(prevalence,mgcv,knitr,dplyr,factoextra,tidyr,
                 ggplot2,gdata,ggsci, RColorBrewer, tidyverse, lubridate, 
                 ComplexHeatmap,gganimate,ggnetwork,ppcor,topicmodels,
                 tidytext, quanteda, widyr, igraph, ggraph,stm,ldatuning,
                 readr,ggthemes, questionr, gridExtra,qdapDictionaries,
                 Rtsne,rsvd, geometry,Rcpp,furrr,future,tictoc,
                 patchwork, OverReact, topicmodels,ldatuning)

#' Create subfolder for topic modeling
createMySubfolder(subfolderName = "topic_modelling")


#' Load and preprocess data
sparse_dfs <- readRDS(file.path(datapath, "sparse_dfs_1000_min.rds"))
df_text_sparse <- sparse_dfs$df_text_sparse_abstract
suffix <- "all_abstract_1000"

#' Diagnostic Checks
if (is.null(df_text_sparse)) {
  stop("df_text_sparse is NULL. Please check the RDS file and the 'df_text_sparse_abstract' object.")
}

if (!inherits(df_text_sparse, "dgCMatrix")) {
  stop("df_text_sparse is not a dgCMatrix. Please ensure it is correctly formatted as a sparse matrix.")
}

cat(sprintf("df_text_sparse has %d rows and %d columns.\n", nrow(df_text_sparse), ncol(df_text_sparse)))

#' Preprocess DTM: remove rare terms (appearing in <=5 documents)
library(Matrix)
df_text_sparse <- df_text_sparse[, Matrix::colSums(df_text_sparse) > 5]
cat(sprintf("After removing rare terms, df_text_sparse has %d rows and %d columns.\n", 
            nrow(df_text_sparse), ncol(df_text_sparse)))

#' Define Sampling Parameters
num_samples <- 10      # Number of subsamples
sample_size <- 10000   # Number of documents per subsample
set.seed(123)          # For reproducibility

#' Ensure sample size does not exceed the number of available documents
if (sample_size > nrow(df_text_sparse)) {
  stop(sprintf("Sample size (%d) exceeds the number of available documents (%d).", 
               sample_size, nrow(df_text_sparse)))
}

#' Define the range of topics to evaluate
topics <- c(10, 20, 30, 40, 50, 60, 80, 100, 120, 160, 200,300,400)

#' Initialize a list to store results from each sample
results_list <- vector("list", num_samples)

#' Set up logging
log_file <- file.path(outpath, "topic_modeling_log.txt")
# Redirect output and messages to the log file
sink(log_file, append = TRUE, split = TRUE)

cat("=== Topic Modeling Process Started ===\n")
cat(sprintf("Timestamp: %s\n\n", Sys.time()))

#' Start timing the sampling and modeling process
t0_total <- Sys.time()

#' Perform Sampling and Topic Modeling in Serial
for (i in 1:num_samples) {
  
  set.seed(123 + i)  # Different seed for each sample for variability
  
  cat(sprintf("Processing Sample %d/%d\n", i, num_samples))
  
  tryCatch({
    #' Sample indices without replacement
    sample_indices <- sample(1:nrow(df_text_sparse), size = sample_size, replace = FALSE)
    
    #' Subset the DTM for the current sample
    df_text_subset <- df_text_sparse[sample_indices, ]
    
    #' Run FindTopicsNumber on the subset with mc.cores = 12
    result <- FindTopicsNumber(
      dtm = df_text_subset,
      topics = topics,
      metrics = c( "CaoJuan2009", "Arun2010", "Deveaud2014"),
      method = "VEM",  # Faster method compared to Gibbs
      control = list(seed = 123),
      mc.cores = 13,    # Utilize 12 cores within the function
      return_models = FALSE,  # Set to FALSE to save memory
      verbose = TRUE
    )
    
    #' Store the result for this sample
    results_list[[i]] <- result
    
    #' Save the result immediately to prevent data loss in case of crashes
    saveRDS(
      object = result, 
      file = file.path(datapath, paste0("sample_", i, "_topic_models_k_10_to_200_", suffix, ".rds"))
    )
    cat(sprintf("Sample %d results saved successfully.\n", i))
    
    #' Clean up to free memory
    rm(df_text_subset, sample_indices, result)
    gc()
    
  }, error = function(e) {
    warning(sprintf("Error in processing sample %d: %s", i, e$message))
    # Optionally, save the error message to a separate file or handle accordingly
  })
  
  cat("\n")  # Add a newline for readability in the log
}

#' End timing
t1_total <- Sys.time()
cat(sprintf("Total Sampling and Modeling Time: %.2f minutes\n\n", 
            as.numeric(difftime(t1_total, t0_total, units = "mins"))))

#' Aggregate Results Across All Samples
# Initialize data frame to store aggregated metrics
aggregated_metrics <- data.frame(K = topics)

# Define metrics to aggregate
metrics <- c("Griffiths2004", "CaoJuan2009", "Arun2010", "Deveaud2014")

# Initialize columns for each metric
for (metric in metrics) {
  aggregated_metrics[[metric]] <- 0
}

# Sum metrics across all samples
for (i in 1:num_samples) {
  result <- results_list[[i]]
  
  # Check if the result exists and is not NULL
  if (!is.null(result)) {
    for (metric in metrics) {
      # Check if the metric exists in the result
      if (metric %in% names(result)) {
        aggregated_metrics[[metric]] <- aggregated_metrics[[metric]] + result[[metric]]
      } else {
        warning(sprintf("Metric '%s' not found in sample %d.", metric, i))
      }
    }
  } else {
    warning(sprintf("Result for sample %d is NULL. Skipping aggregation for this sample.", i))
  }
}

# Calculate the average metrics
aggregated_metrics[, metrics] <- aggregated_metrics[, metrics] / num_samples

# Rescale metrics for comparison (0 to 1)
library(scales)
aggregated_metrics_rescaled <- aggregated_metrics
for (metric in metrics) {
  aggregated_metrics_rescaled[[metric]] <- rescale(aggregated_metrics_rescaled[[metric]], to = c(0, 1))
}

#' Plot Aggregated Optimization Results
library(ggplot2)
library(tidyr)

# Reshape data for plotting
plot_data <- aggregated_metrics_rescaled %>%
  pivot_longer(cols = all_of(metrics), names_to = "Metric", values_to = "Score")

# Create the optimization plot
optmisation_plot <- ggplot(as.data.frame(plot_data), aes(x = K, y = Score, color = Metric)) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  theme_minimal() +
  labs(
    title = "Aggregated Topic Model Optimization Metrics",
    x = "Number of Topics (K)",
    y = "Rescaled Score",
    color = "Metric"
  ) +
  theme(
    text = element_text(size = 14),
    plot.title = element_text(hjust = 0.5)
  )

# Save the optimization plot
ggsave(filename = file.path(figpath, "aggregated_topic_model_optimization.png"),
       plot = optmisation_plot, width = 10, height = 6)
cat("Aggregated optimization plot saved successfully.\n\n")

# Display the plot
print(optmisation_plot)

#' Determine the Optimal Number of Topics (K)
# Here, we'll choose K based on the 'Deveaud2014' metric as an example
opt_k <- aggregated_metrics_rescaled$K[which.max(aggregated_metrics_rescaled$Deveaud2014)]
cat(sprintf("Optimal number of topics (K) based on Deveaud2014: %d\n\n", opt_k))

#' Fit the Final Topic Model on the Entire Dataset with Optimal K
t0_final_model <- Sys.time()

topic_model_final <- LDA(
  df_text_sparse,
  k = opt_k,
  method = "VEM",
  control = list(seed = 123)
)

t1_final_model <- Sys.time()
cat(sprintf("Final Model Fitting Time: %.2f minutes\n\n", 
            as.numeric(difftime(t1_final_model, t0_final_model, units = "mins"))))

#' Save Aggregated Metrics and Final Model
# Save aggregated optimization data
write.csv(aggregated_metrics, 
          file = file.path(datapath, paste0("aggregated_topic_models_optimization_data_", suffix, ".csv")), 
          row.names = FALSE)
cat("Aggregated optimization data saved successfully.\n")

# Save the final topic model using OverReact
OverReact::saveREACT(
  file = topic_model_final,
  outpath = datapath,
  filename = paste0("final_topic_model_k_", opt_k, "_", suffix)
)
cat("Final topic model saved using OverReact::saveREACT.\n")

# Save the final topic model as RDS
saveRDS(
  object = topic_model_final, 
  file = file.path(datapath, paste0("final_topic_model_k_", opt_k, "_", suffix, ".rds"))
)
cat("Final topic model saved as RDS.\n")

#' Optionally, save individual sample results if not already saved
# (Redundant since each sample is saved within the loop)

#' Save session information for reproducibility
writeLines(capture.output(sessionInfo()), 
           con = file.path(outpath, "session_info.txt"))
cat("Session information saved successfully.\n\n")

#' Clean up
rm(list = ls())
gc()

cat("=== Topic Modeling Process Completed ===\n")
sink()  # Stop redirecting output to the log file