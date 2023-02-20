
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
source(paste0(function_script_path,"/forest_plot.R"))

source(file = "code/0_functions.R")


# update.packages("Rcpp",repos="http://se-r.sm.med.ic.ac.uk", dependencies = TRUE )

#' Pull in packages needed
package.list <- c("prevalence","mgcv","knitr","dplyr","factoextra","tidyr",
                  "ggplot2","gdata","ggsci", "RColorBrewer", "tidyverse", "lubridate", 
                  "ComplexHeatmap","gganimate","ggnetwork","ppcor","topicmodels",
                  "tidytext", "quanteda", "widyr", "igraph", "ggraph","stm","ldatuning",
                  "readr","ggthemes", "questionr", "gridExtra","scales","ggpubr",
                  "Rtsne","rsvd", "geometry","Rcpp","furrr","future","rjags","sysfonts",
                  "patchwork", "OverReact")
load_packages(package.list)

# create subfolder
createMySubfolder(subfolderName = "characterising_topic_models")

# no scientific notation
options(scipen = 999)



# Pull in data ------------------------------------------------------------


dat <- readRDS("data/dat_main.rds")
topic_model <- readRDS("data/topic_model_k_60_all_abstract_10000.rds")
ntopic=60

# get beta
td_beta <- tidy(topic_model)
td_beta


# get wide gamma
td_beta_wide=td_beta %>% pivot_wider(id_cols = topic,
                                     names_prefix = "",names_from = term,values_from = beta)


# get gamma
td_gamma <- tidy(topic_model, matrix = "gamma",
                 document_names = rownames(df_text_sparse))

td_gamma

# get wide gamma
td_gamma_wide=td_gamma %>% pivot_wider(id_cols = document,
                                       names_prefix = "topic_",names_from = topic,values_from = gamma)

# 
# # get principal components of Gamma scores
# td_gamma_wide_pca=prcomp(td_gamma_wide[,2:71])
# pca_var_perc=round(100*(td_gamma_wide_pca$sdev^2)/sum(td_gamma_wide_pca$sdev^2),2)
# barplot(pca_var_perc, main="Variance explained by principal components of Gamma matrix",
#         xlab="PCs",ylab="% variance")


# Get df of top terms by topic
top_terms <- td_beta %>%
  arrange(beta) %>%
  dplyr::group_by(topic) %>%
  top_n(5, beta) %>%
  arrange(-beta) %>%
  dplyr::select(topic, term) %>%
  summarise(terms = list(term)) %>%
  mutate(terms = map(terms, paste, collapse = ", ")) %>% 
  unnest()
top_terms

# 
gamma_terms <- td_gamma %>%
  group_by(topic) %>%
  summarise(mean_gamma = mean(gamma)) %>%
  arrange(desc(mean_gamma)) %>%
  left_join(top_terms, by = "topic") %>%
  mutate(topic = paste0("Topic ", topic),
         topic = reorder(topic, mean_gamma))

# devtools::install_github("mathzero/OverReact")
library(OverReact)

# Plot terms by topic
topics_plot=gamma_terms %>%
  slice_head(n=20) %>% 
  ggplot(aes(topic, mean_gamma, label = terms, fill = mean_gamma)) +
  geom_col(show.legend = FALSE) +
  geom_text(hjust = 1, nudge_y = -0.0002, size = 2, col="white") +
  coord_flip() +
  scale_y_continuous(expand = c(0,0),
                     limits = c(0.00, 0.022),
                     labels = scales::percent_format()) +
  OverReact::theme_react( ticks = FALSE) +
  theme(plot.title = element_text(size = 16,
                                  family="IBMPlexSans-Bold"),
        plot.subtitle = element_text(size = 13)) +
  labs(x = NULL, y = expression(gamma),
       title = "Topics by prevalence in published COVID-19 papers",
       subtitle = "With the top 6 words that contribute to each topic")

# view plot
topics_plot

# save
OverReact::saveREACTplot(p = topics_plot,figpath = figpath,filename = "topic_gamma_plot",
                         width = 8,height = 5,savePDF = T)



# Violin plot of topics ---------------------------------------------------

class(gamma_terms$topic)
# Build plot (takes ages)
p_topics_violin=td_gamma %>% 
  left_join(top_terms, by = "topic") %>%
  mutate(term_concat=(paste0("Topic ",topic,": ",terms)),
         term_concat=factor(term_concat, levels=unique(term_concat)),
         topic=factor(topic)) %>% 
  left_join(gamma_terms, by = "topic") %>% 
  ggplot(aes(y=reorder(term_concat,-mean_gamma), x=gamma)) +
  geom_violin() + 
  OverReact::theme_react() +
  scale_x_log10() +
  labs(y="", x= expression(gamma))


# save
OverReact::saveREACTplot(p = p_topics_violin,figpath = figpath,filename = "topic_gamma_violin_plot",
                         width = 7,height = 15,savePDF = T)







# Heatmap -----------------------------------------------------------------

# get gamma matrix
gamma_mat <- data.frame(unique_id=topic_model@documents,topic_model@gamma)
colnames(gamma_mat) <- c("unique_id", paste0("topic_",1:ntopic))

# get preprint matrix
dat_preprint <- dat %>% filter(type=="preprint")

# get preprint mat
gamma_mat_preprint=gamma_mat %>% filter(unique_id %in% dat_preprint$unique_id)


# get preprint mat
gamma_mat_preprint=gamma_mat %>% filter(unique_id %in% dat_preprint$unique_id)


# get correlation matrix
cormat=cor((gamma_mat[,2:61]))

# add names
rownames(cormat) = colnames(cormat) = paste0("Topic ",1:60,": ", top_terms$terms)

myheatmap=ComplexHeatmap::Heatmap(cormat,row_dend_width = unit(1,"cm"),column_dend_height  = unit(1,"cm"))
myheatmap

png(filename = paste0(figpath,"topic_heatmap.png"),width = 15,height = 15,units = "in",res = 300)
myheatmap
dev.off()



# get correlation matrix
cormat=cor((gamma_mat_preprint[,2:61]))

# add names
rownames(cormat) = colnames(cormat) = paste0("Topic ",1:60,": ", top_terms$terms)

myheatmap=ComplexHeatmap::Heatmap(cormat,row_dend_width = unit(1,"cm"),column_dend_height  = unit(1,"cm"))
myheatmap

png(filename = paste0(figpath,"topic_heatmap_preprint.png"),width = 15,height = 15,units = "in",res = 300)
myheatmap
dev.off()

OverReact::saveREACTtable(tab = top_terms6,outpath = outpath,filename = "top_terms_6")



# Topics over time --------------------------------------------------------

# Join standard data with toic gamma data
topics=paste0("topic_",1:ntopic)
dat_join=dat %>% 
  left_join(gamma_mat)

# summarise means of all topics by Y-M
topic_summary_df=dat_join %>% 
  filter(!is.na(topic_1)) %>% 
  group_by(month=lubridate::month(as.Date(date)),year=lubridate::year(as.Date(date))) %>% 
  summarise(across(topics,list(mean),na.rm=T))
topic_summary_df$date <- lubridate::as_date(paste0(topic_summary_df$year,"-",topic_summary_df$month,"-1"),format="%Y-%m-%d")
colnames(topic_summary_df)[3:62] <- paste0("Topic ",1:60,": ",top_terms$terms)

# Pivot for plotting
topic_summary_df_long <- topic_summary_df %>% 
  pivot_longer(cols =colnames(topic_summary_df)[3:62])

# forate as date
topic_summary_df_long$date=as.Date(topic_summary_df_long$date)

# Plot
p_topics=topic_summary_df_long %>% 
  dplyr::filter(date>"2020-03-01", (grepl("23",name)| grepl("21",name) |  grepl("59",name))) %>% 
  ggplot2::ggplot(aes(x=date,y=value, col=name)) +
  geom_point()+
  geom_line() +
  OverReact::theme_react() +
  OverReact::scale_color_imperial(palette = "cool") +
  labs(x="",y="Mean Topic Gamma", col="Topic") +
  theme(legend.position = c(0.6,0.8))
p_topics
OverReact::saveREACTplot(p = p_topics,figpath =figpath ,
                         filename = "topics_over_time",
                         width = 7,4,savePDF = T)



# Topic model clustering --------------------------------------------------


dat_mod <- dat %>% left_join(td_gamma_wide, by =c("unique_id"="document")) %>% 
  rename(date_published=date,
         published_in_journal=published
  ) %>% 
  rename(article_type=type)

# get sample of paper for clustering by 
clust_mat <- td_gamma_wide  %>% 
  dplyr::select(-document) %>%
  dplyr::sample_n(size = 100) %>% 
  t() %>% 
  as.data.frame()

cooccurmat <- crossprod(as.matrix(clust_mat))

marg_occurrence=diag(cooccurmat)
diag(cooccurmat) <- 0
scaleZeroOne <- function(x){(x-min(x))/(max(x)-min(x))}
cooccurmat <- scaleZeroOne(cooccurmat)
n <- cooccurmat %>% graph_from_adjacency_matrix(mode="undirected", diag=F, weighted=T)
cl <-igraph::cluster_optimal(n)
V(n)$group <- cl %>% membership() %>% as.character()
V(n)$degree <-as.numeric(marg_occurrence)
# V(n)$name <- str_wrap(V(n)$name,width = 20)
# do weights
weights <- ifelse(crossing(cl,n),1.5,1)

#plot
p_network <- ggplot(ggnetwork(n, layout=igraph::with_kk(weights = weights)),
                    aes(x,y, xend=xend, yend=yend),
                    label_wrap_gen(width = 15,multi_line = T)
) +
  geom_edges(aes(alpha=weight), size=0.2) +
  geom_node_point(aes(size=degree, color = group))+
  geom_node_text(aes(label = name), check_overlap = T,nudge_y = -0.03) +
  # geom_nodelabel(aes(label = name, size=degree, color = group)) +
  scale_alpha_continuous(guide=F,range = c(0.05,1))+
  OverReact::scale_color_imperial(palette = "cool",guide=F) +
  scale_size_continuous(range = c(1,10), guide=F) +
  theme_blank() +
  coord_cartesian(clip="off")

p_network
OverReact::saveREACTplot(p = p_network,figpath =figpath ,
                         filename = "symptom_network",
                         width = 7,7,savePDF = F)





# Univariate topic modelling ----------------------------------------------


# Run analysis on cites_per_day
univ_cpd_preprint <- runUnivariate(mydat = dat_mod, preprint_filter=c("Preprint"),
                                   var = "cites_per_day",wordlist = paste0("topic_",1:ntopic),
                                   family = "gaussian",wordcount_threshold = 10,n = 10)
univ_cpd_preprint$journal <- "Citations per day"


# Run analysis on altmetrics
univ_alt_preprint <- runUnivariate(mydat = dat_mod,  preprint_filter=c("Preprint"),var = "altmetrics_score",
                                   wordlist = paste0("topic_",1:ntopic),
                                   family = "gaussian",wordcount_threshold = 10,n = 10)
univ_alt_preprint$journal <- "Altmetrics score"



# Run analysis on published
univ_pub_preprint <- runUnivariate(mydat = dat_mod %>% filter(date_published<="2021-01-01"),  preprint_filter=c("Preprint"),
                                   var = "published_in_journal",wordlist = paste0("topic_",1:ntopic),
                                   family = "binomial",wordcount_threshold = 20,n = 10)
univ_pub_preprint$journal <- "Published in journal"



# Run analysis on h-index of journal published
univ_pub_hindex_preprint <- runUnivariate(mydat = dat_mod %>% filter(published_in_journal==1),
                                          preprint_filter=c("Preprint"),
                                          var = "subs_pub_h_index",wordlist = paste0("topic_",1:ntopic),
                                          family = "gaussian",wordcount_threshold = 20,n = 10)
univ_pub_hindex_preprint$journal <- "Publishing journal H-Index"




# Plot
univ_comb <- rbind(univ_cpd_preprint,univ_alt_preprint,univ_pub_preprint,univ_pub_hindex_preprint)
univ_comb$variable <- as.character(univ_comb$variable)
top_terms$variable=paste0("topic_",top_terms$topic)
univ_comb <- univ_comb %>% left_join(top_terms)
univ_comb
p_univ  <- univ_comb  %>% 
  arrange((p.value)) %>% 
  group_by(journal) %>% 
  mutate(indx=row_number())  %>% 
  ungroup() %>%
  mutate(label=case_when(indx >30 ~ NA_character_,
                         indx <8 ~ paste0(topic,": ",terms),
                         TRUE ~ as.character(topic)),
         sig=factor(case_when(pvalue_bonf <=0.05 ~ "FDR<0.05",
                              p.value <=0.05 ~ "p<0.05",
                              T ~"Not significant"), 
                    levels = c("Not significant","p<0.05","FDR<0.05")))%>% 
  ggplot(aes(x=(estimate),y=-log10(p.value), col = sig, label=str_wrap(string = label,width = 20))) +
  geom_point() +
  ggrepel::geom_text_repel(col = "black", size = 1.7, lineheight=0.8) +
  theme_adjust+
  theme_bw() +
  {if("journal"%in%colnames(univ_comb))
    facet_wrap(.~journal, scales="free_x",nrow=1)
  } +
  # OverReact::scale_color_imperial(palette = "warm", reverse = T) +
  scale_color_manual(values = c("grey60", "orange", "firebrick3")) +
  labs(x= "Beta coefficient", y="-log10 p-value", col = "") +
  theme_adjust
p_univ

OverReact::saveREACTplot(p = p_univ,figpath = figpath,filename = "topic_univariate_preprint",
                         width = 16,height = 8,savePDF = T)




# Run on non-preprints ----------------------------------------------------


# Run analysis on cites_per_day
univ_cpd_journal <- runUnivariate(mydat = dat_mod, preprint_filter=c("Journal article"),
                                  var = "cites_per_day",wordlist = paste0("topic_",1:ntopic),
                                  family = "gaussian",wordcount_threshold = 10,n = 10)
univ_cpd_journal$journal <- "Citations per day"


# Run analysis on altmetrics
univ_alt_journal <- runUnivariate(mydat = dat_mod,  preprint_filter=c("Journal article"),var = "altmetrics_score",
                                  wordlist = paste0("topic_",1:ntopic),
                                  family = "gaussian",wordcount_threshold = 10,n = 10)
univ_alt_journal$journal <- "Altmetrics score"

### Plot in journal articles


# Plot
univ_comb_journal <- rbind(univ_cpd_journal,univ_alt_journal)
univ_comb_journal$variable <- as.character(univ_comb_journal$variable)
top_terms$variable=paste0("topic_",top_terms$topic)
univ_comb_journal <- univ_comb_journal %>% left_join(top_terms)

p_univ_journal  <- univ_comb_journal  %>% 
  arrange((p.value)) %>% 
  group_by(journal) %>% 
  mutate(indx=row_number())  %>% 
  ungroup() %>%
  mutate(label=case_when(indx >30 ~ NA_character_,
                         indx <8 ~ paste0(topic,": ",terms),
                         TRUE ~ as.character(topic)),
         sig=factor(case_when(pvalue_bonf <=0.05 ~ "FDR<0.05",
                              p.value <=0.05 ~ "p<0.05",
                              T ~"Not significant"), 
                    levels = c("Not significant","p<0.05","FDR<0.05")))%>% 
  ggplot(aes(x=(estimate),y=-log10(p.value), col = sig, label=str_wrap(string = label,width = 20))) +
  geom_point() +
  ggrepel::geom_text_repel(col = "black", size = 1.7, lineheight=0.8) +
  theme_adjust+
  theme_bw() +
  {if("journal"%in%colnames(univ_comb))
    facet_wrap(.~journal, scales="free_x",nrow=1)
  } +
  # OverReact::scale_color_imperial(palette = "warm", reverse = T) +
  scale_color_manual(values = c("grey60", "orange", "firebrick3")) +
  labs(x= "Beta coefficient", y="-log10 p-value", col = "") +
  theme_adjust
p_univ_journal

OverReact::saveREACTplot(p = p_univ_journal,figpath = figpath,filename = "topic_univariate_journal",
                         width = 8,height = 6,savePDF = T)

