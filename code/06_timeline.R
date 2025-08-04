
#' First clear the environment of variables
rm(list=ls(all=TRUE))

outpath <- paste0(getwd(),"/output/")
figpath <- paste0(getwd(),"/plots/")
datapath <- paste0(getwd(),"/data/")
function_script_path="/Users/mathzero/Library/CloudStorage/GoogleDrive-mattdwhitaker@gmail.com/My Drive/Imperial/PhD/REACT2_analysis_in_progress/RESULTS/function_scripts/"
function_script_path ="/Users/mw418/codebase/misc/function_scripts/"
function_script_path ="E:/home/mw418/function_scripts/"


#' Source any functions from the local file
source(paste0(function_script_path,"/load_packages.R"))
source(paste0(function_script_path,"/cats_and_covs.R"))
source(paste0(function_script_path,"/create_subfolder.R"))
source(paste0(function_script_path,"/forest_plot.R"))

source(file = "code/0_functions.R")

#' Pull in packages needed
package.list <- c("knitr","dplyr","tidyr",
                  "ggplot2","gdata","ggsci", "RColorBrewer", "tidyverse", "lubridate", 
                  "ComplexHeatmap",
                  "ggthemes", "gridExtra","scales","ggpubr",
                  "patchwork", "OverReact")
load_packages(package.list)
# pacman::p_load(package.list)

# create subfolder
createMySubfolder(subfolderName = "timeline")

# no scientific notation
options(scipen = 999)

# Save
dat <- readRDS("data/bq_exports_mar_2023/clean/dat_main.rds")

# Create timeline  -------------------------------------------------------------


dat$datetime_new %>% summary
dat <- dat %>% group_by(date) %>% 
  mutate(datetime_new=as_datetime(date),
         datecount=row_number(),
         datetime_new=datetime_new+datecount) %>% 
  ungroup()

class(dat$datetime_new)
dat$cites_per_day %>% summary()
datefilter=as.Date("2020-01-01")
scorefilter=25000
citefilter=10

labelpapers_cite=c("pub.1124290823","pub.1125146281","pub.1134476360","pub.1144584288","pub.1147732893","pub.1144911111")
labelpapers_alt=c("pub.1125686004","pub.1132688603","pub.1133379610","1142337986","pub.1140641988","pub.1139010213",
                  "pub.1145882086","pub.1147199913","pub.1145332042","pub.1151265522")
labelpapers=c(labelpapers_cite,labelpapers_alt)



dat %>% filter(year==2022) %>% 
  # filter(type=="preprint") %>% 
  arrange(-relative_citation_ratio) %>%
  dplyr::select(id,date)


# add variables determining highlighting
dat <- dat %>% mutate(highlight=case_when(id%in%labelpapers ~ T,
                                          T~F),
                      highlight_preprint=case_when(highlight & type=="preprint"~ T,
                                                   T~F),
                      highlight_article=case_when(highlight & type=="article"~ T,
                                                  T~F),
                      highlight_col=case_when(highlight_preprint ~ "red",
                                              highlight_article ~ "blue",
                                              T ~ "grey60"
                      ))
dat$highlight %>% table(dat$type)

            
# massive timeline plot
wraplength=40
dat %>% 
  filter(date>datefilter) %>% 
  ggplot(aes()) +
  geom_segment(aes(x=datetime_new,
                   xend=datetime_new, 
                   y=0,
                   yend=altmetrics_score, col = highlight_col)) +
  geom_segment(aes(x=datetime_new,
                   xend=datetime_new, 
                   y=0,
                   yend=-cites_per_day*1000, col = highlight_col)) +
  ylim(c(-35000,55000))+
  # scale_alpha_manual(values = c(0.1,1))+
  
  geom_point(data = dat %>% filter(id %in% labelpapers_alt),
             aes(x=datetime_new, y=altmetrics_score,col = highlight_col), 
             shape=1, size=4) +
  geom_point(data = dat %>% filter(id %in% labelpapers_cite),
             aes(x=datetime_new, y=altmetrics_score,col = highlight_col), 
             shape=15, size=1) +
  
  geom_point(data = dat %>% filter(id %in% labelpapers_cite),
             aes(x=datetime_new, y=-(cites_per_day*1000),col = highlight_col), 
             shape=1, size=4) +
  geom_point(data = dat %>% filter(id %in% labelpapers_alt),
             aes(x=datetime_new, y=-(cites_per_day*1000),col = highlight_col), 
             shape=15, size=1) +
  
  # # labels
  # geom_label(data = dat %>% 
  #                            filter(id %in% labelpapers_alt),
  #            aes(x=datetime_new, y=altmetrics_score,
  #                label=stringr::str_wrap(title_preferred,wraplength)),
  #            size=2.6,
  #            lineheight = .7,
  #            label.size = NA      # No border
  #            
  #            ) +
  # geom_label(data = dat %>% 
  #              filter(id %in% labelpapers_alt),
  #            aes(x=datetime_new, y=altmetrics_score,col = highlight_col,
  #                label=stringr::str_wrap(title_preferred,wraplength)),
  #            size=2.6,
  #            lineheight = .7,
  #            label.size = NA      # No border
  #            
  # ) +
  # geom_label(data = dat %>% filter(id %in% labelpapers_cite),
  #                          aes(x=datetime_new, y=-cites_per_day*1000,col = highlight_col,
  #                              label=stringr::str_wrap(title_preferred,wraplength)),
  #            size=2.6,
  #            lineheight = .7,
  #            label.size = NA)+
  ggthemes::theme_clean() +
  scale_color_manual(values=c("red","grey60","blue")) +
  scale_y_continuous(breaks=c(-20000,-10000,0,20000, 40000),
                     labels=c("20","10","0", "20000","40000")) +
  geom_hline(yintercept = 0,linetype="dotted", col)+
  theme(legend.position = "none")+
  labs(x="",y="Altmetrics score")


