rm(list=ls())
source("E:/Group/functions/load_packages.R")
load_packages(c("dplyr", "OverReact","tidyverse","qdap","tm","wordcloud","ggplot2","ggthemes", 
                "text2map","tidytext","focus","gridExtra", "future.apply", 
                "foreach","doParallel","catboost","ggnetwork","ICoLour","ppcor",
                "tidytext", "quanteda", "widyr", "igraph", "ggraph", "patchwork"))
outpath <- "E:/Group/react2_study5/report_phases_combined/projects/covid_lit_analysis/output/"
figpath <- "E:/Group/react2_study5/report_phases_combined/projects/covid_lit_analysis/plots/networks/"


# Load data ---------------------------------------------------------------

# Save
dat <- readRDS("data/dat_main.rds")
titles_dtm_tidy_wide <- readRDS("data/dat_text.rds")
titles_dtm_tidy_wide[is.na(titles_dtm_tidy_wide)] <- 0

word_freqs <- colSums(titles_dtm_tidy_wide[,3:ncol(titles_dtm_tidy_wide)],na.rm=T)
word_freqs_df <- data.frame(word=colnames(titles_dtm_tidy_wide)[3:ncol(titles_dtm_tidy_wide)],
                            word_freqs=as.numeric(word_freqs))
freq_words <- word_freqs_df %>% arrange(word_freqs) %>% top_n(50)%>% select(word) %>% 
  unlist() %>% as.character()


# Create network ----------------------------------------------------------
cooccurmat <- crossprod(as.matrix(titles_dtm_tidy_wide[,freq_words]))
diag(cooccurmat) <- 0
scaleZeroOne <- function(x){(x-min(x))/(max(x)-min(x))}
cooccurmat <- scaleZeroOne(cooccurmat)
pcormat <- pcor(as.matrix(titles_dtm_tidy_wide[,freq_words]),method = "pearson")
pcormat$estimate

pcormat[is.infinite(pcormat)] <- max(pcormat[!is.infinite(pcormat)])
rownames(pcormat) <- colnames(pcormat) <- rownames(cooccurmat)
# cooccurmat <- log10(cooccurmat)
n <- cooccurmat %>% graph_from_adjacency_matrix(mode="undirected", diag=F, weighted=T)
cl <-cluster_optimal(n)
V(n)$group <- cl %>% membership() %>% as.character()
V(n)$degree <-as.numeric(diag(cooccurmat))

# do weights
weights <- ifelse(crossing(cl,n),1,15)

#plot
p_network <- ggplot(ggnetwork(n, layout=igraph::with_fr(weights = weights)),
                    aes(x,y, xend=xend, yend=yend)) +
  geom_edges(aes(alpha=weight), size=0.2) +
  geom_nodelabel(aes(label = name, size=degree, color = group)) +
  scale_alpha_continuous(guide=F,range = c(0.05,1))+
  scale_color_imperial(palette = "extended", guide=F)+
  scale_size_continuous(range = c(3,6), guide=F) +
  theme_blank() +
  coord_cartesian(clip="off")

# theme(plot.margin = unit(c(1,1,1,1),"in"))
p_network

OverReact::saveREACTplot(p = p_network,figpath = figpath,
                         filename = "network_titles",width = 6,height=6)





