
# Load up some fonts
extrafont::loadfonts()
# extrafont::font_import()
extrafont::loadfonts(device = "win")

plotReactForest <- function(univ_df_plot, 
                            adjustment_numbers = c(3,6),
                            adjustment_descriptions = c("Crude","Age and sex adjusted"),
                            xrange = c(0.25,16),
                            legend.position = "bottom",
                            strip_borders = T,
                            insignificant_results_greyed_out = T,
                            palette = "core",
                            alpha_val=0.2,
                            strip_text_size = 8,
                            arrangeByOR=T
                            ){
  
  
  # detect any repeated level names and add spaces to solve the duplication problem,
  # as it messes up the row shading
  alllevs=c()
  for(u in unique(univ_df_plot$Variable)){
    levs=unique(univ_df_plot$Category[univ_df_plot$Variable==u])
    if(length(intersect(levs,alllevs))>0){
      print(u)
      duplev=intersect(levs,alllevs)
      univ_df_plot$Category[univ_df_plot$Variable==u][univ_df_plot$Category[univ_df_plot$Variable==u] == duplev] <- paste0(" ",duplev)
    }
    alllevs=c(alllevs,levs)
  }
  
  if(sum(duplicated(alllevs))>0){
    print("Category names are duplicated, which may mess up the alternating stripes")
    print("Consider renaming:")
    alllevs[duplicated(alllevs)]
    print("For now, a horrible hack has been applied to solve the problem :)")
  }
  
  
  # dodge #
  if(length(adjustment_numbers)==1){
    dodger = position_dodge2()
  }else{
    dodger = position_dodge2(width=0.5,reverse = T)
    
  }
  
  # x-axis seq #
  x_seq=2^seq(log2(xrange[[1]]),log2(xrange[[2]]),1)
  
  # create dummy df of adjustment levels for simple joining rather than complex case_When
  adj_df <- data.frame(adjustment = as.character(adjustment_numbers), 
                       adjust_desc = factor(adjustment_descriptions,
                                            levels = adjustment_descriptions))
  
  
  stripes_df <- univ_df_plot %>%  
    group_by(Category) %>% 
    summarise(mean_or=mean(OR, na.rm=T))
    if (arrangeByOR){
      stripes_df <- stripes_df %>% arrange(mean_or)
    }
  
  stripes_df <- stripes_df %>%
    ungroup() %>% 
    mutate(stripe_col = case_when(row_number() %%2 ==0 ~ "NA",
                                                    TRUE ~ "grey90"))
  
  stripes_df$Category <- factor(stripes_df$Category, levels=unique(stripes_df$Category))

  
  # make plot
  p_univ <- univ_df_plot %>% 
    dplyr::filter(adjustment %in% adjustment_numbers) %>% 
    dplyr::mutate(Category=factor(Category, levels = unique(Category)),
      Variable=factor(Variable, levels = unique(Variable)),
                  myshape=case_when(!insignificant_results_greyed_out ~ "dark",
                                  P_value >0.05 ~ "light",
                                  TRUE ~ "dark")) %>%
    left_join(adj_df) %>% 
    left_join(stripes_df) %>% 
    ggplot(aes(x=(Category), y = OR, col = adjust_desc, group= myshape)) +
    geom_tile(data = stripes_df, aes(x=Category, y =1, height = Inf,
                  fill = stripe_col),inherit.aes = F,
              alpha=0.8,
              col = if(strip_borders) "grey70" else NA,
              linetype = "dashed",
              show.legend = F) +
    scale_fill_manual(values = c("white","grey96")) +
    ggnewscale::new_scale_fill() +
    geom_hline(yintercept = 1, linetype = "dashed", col = "grey50" )+
    geom_point(position = dodger, size=0.9, aes(shape = myshape)) +
    geom_errorbar(aes(ymin = Lower, ymax = Upper), 
                  position = dodger, width = 0.5,
                  show.legend = F, size=0.2) +
    coord_flip() +
    # scale_alpha_manual("alpha",values = c("light" = alpha_val, "dark" = 1),
    #                    guide = "none") +
    scale_shape_manual("myshape",values = c("light" = 0, "dark" = 15),
                       guide = "none") +
    # scale_y_continuous(trans = "log10",breaks = x_seq) +
    scale_y_continuous(trans = "log2", #breaks = x_seq, 
                       labels =function(x) MASS::fractions(x))+ 
    # scale_color_manual(values = myCols) +
    OverReact::scale_color_imperial(palette = palette) +
    OverReact::theme_react(strip_text_size = strip_text_size) +
    ggforce::facet_col(.~Variable,  scales = "free", space = "free") +
    labs(x="", y="Odds ratio", col ="") +
    theme(legend.position = legend.position,
          panel.grid = element_blank(),
          panel.grid.major.x = element_line(size = rel(0.1),linetype = "dashed")
    )
  p_univ
  return(p_univ)
}


# devtools::install(pkg = "ICoLour-master/ICoLour-master/")
