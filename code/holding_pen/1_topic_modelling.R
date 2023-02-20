
#' First clear the environment of variables
rm(list=ls(all=TRUE))

outpath <- paste0(getwd(),"/output/")
figpath <- paste0(getwd(),"/plots/")
datapath <- paste0(getwd(),"/data/")
function_script_path ="E:/home/mw418/function_scripts/"
# function_script_path="/Users/mathzero/Library/CloudStorage/GoogleDrive-mattdwhitaker@gmail.com/My Drive/Imperial/PhD/REACT2_analysis_in_progress/RESULTS/function_scripts/"


#' Source any functions from the local file
source(paste0(function_script_path,"/load_packages.R"))
source(paste0(function_script_path,"/cats_and_covs.R"))
source(paste0(function_script_path,"/create_subfolder.R"))
# 
# source("E:/Group/react2_study5/report_phases_combined/projects/function_scripts/create_subfolder.R")
# update.packages("Rcpp",repos="http://se-r.sm.med.ic.ac.uk", dependencies = TRUE )

#' Pull in packages needed
package.list <- c("prevalence","mgcv","knitr","dplyr","factoextra","tidyr",
                  "ggplot2","gdata","ggsci", "RColorBrewer", "tidyverse", "lubridate", 
                  "ComplexHeatmap","gganimate","ggnetwork","ppcor","topicmodels",
                  "tidytext", "quanteda", "widyr", "igraph", "ggraph","stm","ldatuning",
                  "readr","ggthemes", "questionr", "gridExtra","qdapDictionaries",
                  "Rtsne","rsvd", "geometry","Rcpp","furrr","future","tictoc",
                  "patchwork", "OverReact", "topicmodels","ldatuning")
load_packages(package.list)

# create subfolder
createMySubfolder(subfolderName = "topic_modelling")

### Load data
sparse_dfs=readRDS("data/sparse_dfs_1000_min.rds")
df_text_sparse=sparse_dfs$df_text_sparse_abstract
suffix="all_abstract_10000"



# Run models --------------------------------------------------------------
# tic()
# # evaluate different values of K
# closeAllConnections()
# plan(multisession(workers = 17))
# many_models <- data_frame(K = c(4, 5, 6, 7, 8,10,15,20,25,30,35,40,50)) %>%
#   mutate(topic_model = future_map(K, ~stm(df_text_sparse, K = .,
#                                           verbose = T)))
# toc()
# ### assess best model fit ###
# heldout <- make.heldout(df_text_sparse)
# 
# k_result <- many_models %>%
#   mutate(exclusivity = map(topic_model, exclusivity),
#          semantic_coherence = map(topic_model, semanticCoherence, df_text_sparse),
#          eval_heldout = map(topic_model, eval.heldout, heldout$missing),
#          residual = map(topic_model, checkResiduals, df_text_sparse),
#          bound =  map_dbl(topic_model, function(x) max(x$convergence$bound)),
#          lfact = map_dbl(topic_model, function(x) lfactorial(x$settings$dim$K)),
#          lbound = bound + lfact,
#          iterations = map_dbl(topic_model, function(x) length(x$convergence$bound)))
# 
# k_result
# 
# ### Plot model metrics ###
# k_result %>%
#   transmute(K,
#             `Lower bound` = lbound,
#             Residuals = map_dbl(residual, "dispersion"),
#             `Semantic coherence` = map_dbl(semantic_coherence, mean),
#             `Held-out likelihood` = map_dbl(eval_heldout, "expected.heldout")) %>%
#   gather(Metric, Value, -K) %>%
#   ggplot(aes(K, Value, color = Metric)) +
#   theme_react() +
#   geom_line(size = 1.5, alpha = 0.7, show.legend = FALSE) +
#   facet_wrap(~Metric, scales = "free_y") +
#   labs(x = "K (number of topics)",
#        y = NULL,
#        title = "Model diagnostics by number of topics",
#        subtitle = "These diagnostics indicate that a good number of topics would be around 7")
# 
# 
# 
# # Another method of choosing K --------------------------------------------
# closeAllConnections()
# result <- FindTopicsNumber(dtm = df_text_sparse,
#                            topics = c(4, 5, 6, 7, 8,10,15,20,25,30,35,40,50),
#                            metrics = c("Griffiths2004","CaoJuan2009","Arun2010","Deveaud2014"),
#                            method="Gibbs",
#                            control = list(seed=123),
#                            mc.cores=as.integer(20),
#                            verbose = T
#                             )
# 
# result_extended <- FindTopicsNumber(dtm = df_text_sparse,
#                            topics = c(60,70,80,90,100,120,140,160,180,200),
#                            metrics = c("Griffiths2004","CaoJuan2009","Arun2010","Deveaud2014"),
#                            method="Gibbs",
#                            control = list(seed=123),
#                            mc.cores=as.integer(20),
#                            verbose = T
# )
# 
t0=Sys.time()
result_extended_again <- FindTopicsNumber(dtm = df_text_sparse,
                                          topics = c(5, 10,15,20,25,30,40,50,60,70,80,90,
                                                     100,120,140,160,180,200,250,300,400),
                                          metrics = c("Griffiths2004","CaoJuan2009","Arun2010","Deveaud2014"),
                                          method="Gibbs",
                                          control = list(seed=123),
                                          mc.cores=as.integer(80),
                                          return_models = T,
                                          verbose = 10
)
Sys.time()-t0


# Get plot
results_df=data.frame(K=result_extended_again$topics,
                      Griffiths2004=scales::rescale(result_extended_again$Griffiths2004,to = c(0,1)),
                      CaoJuan2009=scales::rescale(result_extended_again$CaoJuan2009,to = c(0,1)),
                      Arun2010=scales::rescale(result_extended_again$Arun2010,to = c(0,1)),
                      Deveaud2014=scales::rescale(result_extended_again$Deveaud2014,to = c(0,1)))


optmisation_plot <- FindTopicsNumber_plot(result_extended_again)



# Personal optimisation plot
opt_plot=results_df %>% ggplot(aes(x=K, y=Deveaud2014)) +
  geom_point() +
  geom_point(data = results_df[which.max(results_df$Deveaud2014),],shape=1, size=5, col="red") +
  geom_line() +
  geom_hline(yintercept = 1,linetype="dashed",col="red") +
  geom_vline(xintercept = results_df$K[which.max(results_df$Deveaud2014)],linetype="dashed",col="red") +
  OverReact::theme_react() +
  labs(x="Number of topics (K)",y="Jenson-Shannon divergence (normalised to 1-max)")

# save
OverReact::saveREACTplot(p = opt_plot,figpath =figpath,filename ="topic_model_opt_K_plot",width =  5,height = 4)


# extract modes
mods <- result_extended_again$LDA_model

# get optimal K
opt_k_indx=which.max(results_df$Deveaud2014)

# optimal number of topics
opt_k=results_df$K[opt_k_indx]

# choose 60-topic model
topic_model <-mods[[opt_k_indx]]


### Save models and optimisations
OverReact::saveREACTtable(tab = results_df,outpath = datapath,
                     filename = paste0("topic_models_k_5_to_300_optimisation_data_",suffix))
OverReact::saveREACT(file = mods,outpath = datapath,
                     filename = paste0("topic_models_k_5_to_300_",suffix))
OverReact::saveREACT(file = topic_model,outpath = datapath,
                     filename = paste0("topic_model_k_",opt_k,"_",suffix))

saveRDS(object = mods,file = paste0(datapath,"topic_models_k_5_to_300_",suffix,".rds"))
saveRDS(object = topic_model,file =paste0(datapath,"topic_model_k_",opt_k,"_",suffix,".rds"))

 
# get beta
td_beta <- tidy(topic_model)
td_beta


# get gamma
td_gamma <- tidy(topic_model, matrix = "gamma",
                 document_names = rownames(df_text_sparse))


td_gamma

# get wide td_gamma for modelling
td_gamma_wide=td_gamma %>% pivot_wider()


top_terms <- td_beta %>%
  arrange(beta) %>%
  dplyr::group_by(topic) %>%
  top_n(6, beta) %>%
  arrange(-beta) %>%
  dplyr::select(topic, term) %>%
  summarise(terms = list(term)) %>%
  mutate(terms = map(terms, paste, collapse = ", ")) %>% 
  unnest()
top_terms

gamma_terms <- td_gamma %>%
  group_by(topic) %>%
  summarise(gamma = mean(gamma)) %>%
  arrange(desc(gamma)) %>%
  left_join(top_terms, by = "topic") %>%
  mutate(topic = paste0("Topic ", topic),
         topic = reorder(topic, gamma))

gamma_terms %>%
  top_n(40, gamma) %>%
  ggplot(aes(topic, gamma, label = terms, fill = topic)) +
  geom_col(show.legend = FALSE) +
  geom_text(hjust = 0, nudge_y = 0.0005, size = 3,
            family = "IBMPlexSans") +
  coord_flip() +
  scale_y_continuous(expand = c(0,0),
                     limits = c(0, 0.04),
                     labels = scales::percent_format()) +
  OverReact::theme_react(base_family = "IBMPlexSans", ticks = FALSE) +
  theme(plot.title = element_text(size = 16,
                                  family="IBMPlexSans-Bold"),
        plot.subtitle = element_text(size = 13)) +
  labs(x = NULL, y = expression(gamma),
       title = "Top 40 topics by prevalence in published COVID-19 abstracts",
       subtitle = "With the top 6 words that contribute to each topic")






