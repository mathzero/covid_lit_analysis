
# replace null values in dat with NA
makeNA <- function(x) str_replace(x,"null",NA_character_)

# Function to test for numeric
canBeNumeric <- function(x){
  stopifnot(is.atomic(x) || is.list(x)) # check that x is vector
  numNAs <- sum(is.na(x))
  numNAsNew <- suppressWarnings(sum(is.na(as.numeric(as.character(x)))))
  return(numNAs==numNAsNew)
}

# convert all possible cols to numeric
checkConvertNumeric <- function(df){
  # convert all possible cols to numeric
  df_out <- as.data.frame(lapply(df, function(col){
    if(canBeNumeric(col)){
      as.numeric(as.character(col))
    }else{
      col
    }
  }))
  return(df_out)
}




# UNivariate --------------------------------------------------------------

# mydat=dat[1:1000,]
# function to run univariate analysis
runUnivariate <- function(mydat, preprint_filter=c("Journal article","Preprint"),
                          var = "cites_per_day", family="gaussian",
                          wordlist=wordlist,
                          wordcount_threshold = 10,
                          n=10){
  
  
  # create index for filtering papers
  mydat <- mydat %>% dplyr::filter(article_type %in% preprint_filter)
  
  # add wordcount
  wordcounts <- colSums(mydat[,wordlist], na.rm=T)
  excludewords=names(wordcounts)[(wordcounts<wordcount_threshold)]
  newwordlist=names(wordcounts)[(wordcounts>=wordcount_threshold)]
  
  
  # filter
  mydat <- mydat %>% dplyr::select(-excludewords)
  
  ### All papers ###
  univ_df <- cbind(y=pull(.data = mydat,var = get(var)), mydat[,newwordlist])
  
  # Split columns into 5 to prevent dictionary overload
  list_res=list()
  x=2:(ncol(univ_df))
  splits=split(x, f =sort(x%%n))
  
  cores = n
  cl=parallel::makeCluster(cores)
  registerDoParallel(cl)
  
  univ_citerate_reg <- foreach(i=1:n, .combine = rbind, 
                               .packages = c("tidyverse","reshape2",
                                             "tidytext","tidyr")) %dopar% {
                                               df_out <- univ_df[,c(1,splits[[i]])] %>% 
                                                 reshape2::melt(id.vars = "y") %>% 
                                                 group_by(variable) %>% 
                                                 do(tidy(glm(y ~ value, data = .,family = family)))
                                               df_out %>% 
                                                 filter(term != "(Intercept)")%>% 
                                                 dplyr::select(-term) 
                                             }
  stopCluster(cl)
  closeAllConnections()
  # 
  # 
  # 
  # 
  # for (i in 1:length(splits)){
  #   print(paste("Processing block",i))
  #   df_out <- univ_df[,c(1,splits[[i]])] %>% 
  #     reshape2::melt(id.vars = "y") %>% 
  #     
  #     # mutate(y = ifelse(family=="binomial", factor(y, levels = c("0","1")),y)) %>% 
  #     group_by(variable) %>% 
  #     do(tidy(glm(y ~ value, data = .,family = family)))
  #   univ_citerate_reg <- df_out %>% 
  #     filter(term != "(Intercept)")%>% 
  #     dplyr::select(-term) 
  #   list_res[[i]] <- univ_citerate_reg
  # }
  
  ### bind all rows
  # univ_citerate_reg <- bind_rows(list_res)
  
  # Add wordcount
  univ_citerate_reg$wordcount <- wordcounts[wordcounts>=wordcount_threshold]
  
  # convert y to numeric 
  univ_df$y <- univ_df$y %>% as.character()%>% as.numeric() 
  
  # get average outcome val
  meanval=c()
  meanval_without=c()
  sdval=c()
  # table(pull(df,(outcome))[df[,i]==1])
  # i <- 614
  for(i in 2:ncol(univ_df)){
    meanval <- c(meanval,mean(pull(univ_df,y)[univ_df[,i]>0], na.rm=T))
    meanval_without <- c(meanval_without,mean(pull(univ_df,y)[univ_df[,i]==0], na.rm=T))
    sdval <- c(sdval,sd(pull(univ_df,(y))[univ_df[,i]>0], na.rm=T))
  }
  
  univ_citerate_reg$mean_outcome_val <- meanval
  univ_citerate_reg$se_outcome_val <- sdval/sqrt(wordcounts[wordcounts>=wordcount_threshold])
  univ_citerate_reg$mean_outcome_val_lower <- meanval - 1.96*univ_citerate_reg$se_outcome_val
  univ_citerate_reg$mean_outcome_val_upper <- meanval + 1.96*univ_citerate_reg$se_outcome_val
  univ_citerate_reg$mean_outcome_val_without <- meanval_without
  
  pval_bonf=scales::scientific(0.05/(ncol(univ_df)-1),digits = 3)
  
  #adjust p-values
  univ_citerate_reg$pvalue_bonf <- stats::p.adjust(p = univ_citerate_reg$p.value,method = "bonferroni")
  univ_citerate_reg$significant = case_when(univ_citerate_reg$pvalue_bonf <= 0.05 ~ paste0("FDR<0.05 ",
                                                                                           "(Bonferroni)"),
                                            univ_citerate_reg$p.value<=0.05 ~ "p<0.05",
                                            TRUE ~ "Not significant")
  #convert to character
  univ_citerate_reg$variable <- as.character(univ_citerate_reg$variable)
  return(univ_citerate_reg)
  
  
}
# univ_df %>% group_by(outcome) %>% summarise(n=n(),
#                                       meanval=mean(y, na.rm=T))


# which(colnames(univ_df)=="outcome")
# Plot univariate
plotUnivariate <- function(univ_citerate_reg){
  # univ_citerate_reg <- univ_citerate_reg  %>% 
  #   arrange(desc(p.value)) %>% 
  #   group_by(journal) %>% 
  #   mutate(indx=row_number())  %>% 
  #   ungroup() %>%
  #   mutate(label=case_when(indx >10 ~ NA_character_,
  #                          TRUE ~ variable))
  
  p_univ <- univ_citerate_reg  %>% 
    arrange((p.value)) %>% 
    group_by(journal) %>% 
    mutate(indx=row_number())  %>% 
    ungroup() %>%
    mutate(label=case_when(indx >30 ~ NA_character_,
                           TRUE ~ variable),
           sig=factor(case_when(pvalue_bonf <=0.05 ~ "FDR<0.05",
                                p.value <=0.05 ~ "p<0.05",
                                T ~"Not significant"), 
                      levels = c("Not significant","p<0.05","FDR<0.05")))%>% 
    ggplot(aes(x=(estimate),y=-log10(p.value), col = sig, label=label)) +
    geom_point() +
    ggrepel::geom_text_repel(col = "black", size = 1.5) +
    theme_adjust+
    theme_bw() +
    {if("journal"%in%colnames(univ_citerate_reg))
      facet_wrap(.~journal, scales="free_x")
    } +
    # OverReact::scale_color_imperial(palette = "warm", reverse = T) +
    scale_color_manual(values = c("grey60", "orange", "firebrick3")) +
    labs(x= "Beta coefficient", y="-log10 p-value", col = "") +
    theme_adjust
  p_univ
  return(p_univ)
}





# modells -----------------------------------------------------------------






getFeatureImpAndSHAP <- function(cb_model, validation_pool, learn_pool,
         modelvars="covariates",myheight=5,
         train_learn,figpath_new=figpath,
         X,y, bandwidth_scaler=5,nperm=100,outcome,bonferroni=FALSE,
         parallelise=T,nclust=20){
  
  
  
  # get feature importances
  cb_varimps=catboost.get_feature_importance(model = cb_model, pool = validation_pool, type = "LossFunctionChange") %>% as.data.frame()
  cb_varimps$feature =rownames(cb_varimps)
  colnames(cb_varimps)[[1]] <- "Mean_delta_logloss"
  
  plot_varimps <- cb_varimps %>% 
    # left_join(predictors_df, by = c("feature" = "variable_desc")) %>% 
    ggplot(aes(x=reorder(feature,Mean_delta_logloss), y=Mean_delta_logloss)) + 
    geom_col(width=0.03) +
    geom_point() +
    coord_flip() +
    scale_fill_manual(values = myCols) +
    scale_colour_manual(values = myCols) +
    theme_bw() +
    labs(x="Variable",y="Mean change in logloss \nafter variable removal", col = "Variable type",  fill = "Variable type")
  
  
  OverReact::saveREACTplot(p = plot_varimps, figpath = figpath_new,
                           filename = paste0("varimps_plot_",outcome,"_",modelvars),width =7, height=myheight)
  
  
# 
  # parallelise=T
#   nclust=100
#   nperm=10
  # bonferroni=F
  ### Get permutation varimps
  set.seed(12345)
  cb_varimps_permute=PermuteCatboostParallel(mymodel = cb_model, X = X,y = y,nperm = nperm,
                                             parallelise=parallelise,nclust=nclust) 
 
  # Get bonferroni adjusted pvalue
  bonfpval=0.05/ncol(X)
  if(bonferroni){
    mypval=bonfpval
  }else{
    mypval=0.05
  }
  
  cb_varimps_permute$summary_stats <- cb_varimps_permute$summary_stats %>% 
    mutate(selected=case_when(prop_permutes_greater_than_unpermute < mypval ~ "Selected",
                              TRUE ~ "Not selected"))
  
  
  
  ### seconds plot, with distributions
  plot_varimps_permute_boxplot <- cb_varimps_permute$full_results %>% 
    rename(predictor=name,
           loss_permute_delta=value) %>% 
    # left_join(predictors_df, by = c("predictor" = "variable_desc")) %>% 
    left_join(cb_varimps_permute$summary_stats) %>% 
    arrange(selected,mean_loss_permute_delta) %>% 
    mutate(indx=row_number()) %>% 
    top_n(10000) %>% 
    ggplot(aes(y=reorder(predictor,indx), x=loss_permute_delta , 
               # height=loss_permute_delta,
               col=selected)) + 
    geom_boxplot() +
    geom_vline(xintercept = 0, linetype="dashed", col="grey50") +
    scale_fill_manual(values = myCols[c(5,3)]) +
    scale_colour_manual(values = myCols[c(5,3)]) +
    theme_bw() +
    scale_y_discrete(labels=function(x) str_wrap(x, width=33)) +
    labs(y="Variable",x="Change in AUC \nafter variable permutation", 
         col = "",  fill = "")
  
  plot_varimps_permute_boxplot
  
  OverReact::saveREACTplot(p = plot_varimps_permute_boxplot, figpath = figpath_new,
                           filename = paste0("permutation_varimps_boxplot_",outcome,"_",modelvars),
                           width =8, height=myheight)
  
  
  
  ### Get feature interactions
  interacts_basemod <- catboost.get_feature_importance(model = cb_model,
                                                       pool = learn_pool,
                                                       type = "Interaction") %>% 
    as.data.frame()
  
  ### get varnames to replace indexes
  varnames_basemod <- X %>% colnames()
  varnames_basemod_df=data.frame(varnames=varnames_basemod,
                                 varindex=0:(length(varnames_basemod)-1))
  interacts_basemod$feature1_name <- NA
  interacts_basemod$feature2_name <- NA
  
  i <- 1
  for (i in 1:nrow(interacts_basemod)){
    indx <- interacts_basemod[i,]$feature1_index
    interacts_basemod[i,]$feature1_name <- varnames_basemod_df[indx+1,]$varnames
  }
  for (i in 1:nrow(interacts_basemod)){
    indx <- interacts_basemod[i,]$feature2_index
    interacts_basemod[i,]$feature2_name <- varnames_basemod_df[indx+1,]$varnames
  }
  
  ### Concatenate names
  interacts_basemod$features_comb <- paste0(interacts_basemod$feature1_name, " x ",
                                            interacts_basemod$feature2_name)
  
  ###plot
  ### seconds plot, with distributions
  plot_interacts <- interacts_basemod%>% 
    arrange(score) %>% 
    top_n(20) %>% 
    ggplot(aes(y=reorder(features_comb,score ), x=score )) + 
    geom_col(fill=myCols[2]) +
    theme_bw() +
    scale_y_discrete(labels=function(x) str_wrap(x, width=33)) +
    labs(y="Variable",x="Interaction strength", 
         col = "",  fill = "")
  
  plot_interacts
  OverReact::saveREACTplot(p = plot_interacts, figpath = figpath_new,
                           filename = paste0("interactions_plot",outcome,"_",modelvars),
                           width =6, height=6)
  
  
  
  # get SHAP values
  cb_shaps=catboost.get_feature_importance(model = cb_model, pool = learn_pool, type = "ShapValues") %>% as.data.frame()
  
  colnames(cb_shaps) <- c(cb_varimps$feature,"Bias")
  cb_shaps <- cb_shaps %>% dplyr::select(-Bias)
  cb_shaps_long=cb_shaps %>% mutate(ID=row_number()) %>% 
    pivot_longer(-ID) %>% 
    rename(variable=name)
  cb_shaps_long_means=cb_shaps %>% mutate(ID=row_number()) %>% 
    pivot_longer(-ID) %>% 
    rename(variable=name) %>% 
    group_by(variable) %>% 
    summarise(mean_value=mean(abs(value)))
  cb_shaps_long <- left_join(cb_shaps_long,cb_shaps_long_means)
  
  # add values from original data
  ## quick function for 0/1 scaling
  scaleZeroOne <- function(x){
    (x-min(x,na.rm=T))/(max(x,na.rm=T)-min(x,na.rm=T))
  }
  
  cb_shaps_stdfvalues=train_learn %>% 
    dplyr::select(-outcome) %>%
    # mutate(across(where(is.numeric),scale)) %>% 
    mutate_all(as.numeric) %>% 
    mutate_all(scaleZeroOne) %>% 
    mutate(ID=row_number()) %>% 
    pivot_longer(-ID) %>% 
    rename(variable=name,
           stdfvalue=value)
  
  cb_shaps_long=left_join(cb_shaps_long,cb_shaps_stdfvalues)
  
  
  # create plot
  x_bound <- max(abs(cb_shaps_long$value)) * 1.1
  label_format = "%.3f"
  plot1 <- ggplot(data = cb_shaps_long) + coord_flip(ylim = c(-x_bound, 
                                                              x_bound)) + 
    geom_hline(yintercept = 0) + ggforce::geom_sina(aes(x = reorder(variable,mean_value), 
                                                        y = value, color = stdfvalue), method = "counts", 
                                                    maxwidth = 0.7, alpha = 0.7) + 
    geom_text(data = unique(cb_shaps_long[, c("variable", "mean_value")]), aes(x =  reorder(variable,mean_value), y = -Inf, 
                                                                               label = sprintf(label_format, mean_value)),
              size = 3, alpha = 0.7, hjust = -0.2, fontface = "bold") + 
    scale_color_gradient(low = myCols[[1]], high = myCols[[3]], 
                         breaks = c(0, 1), labels = c(" Low", "High "), 
                         guide = guide_colorbar(barwidth = 12, barheight = 0.3)) + 
    theme_bw() + theme(axis.line.y = element_blank(), axis.ticks.y = element_blank(), 
                       legend.position = "bottom", legend.title = element_text(size = 10), 
                       legend.text = element_text(size = 8), axis.title.x = element_text(size = 10)) + 
    # scale_x_discrete(limits = rev(levels(cb_shaps_long$variable)), 
    #                  labels = label.feature(rev(levels(cb_shaps_long$variable)))) + 
    labs(y = "SHAP value (impact on model output)", 
         x = "", color = "Feature value  ")
  
  plot1
  
  OverReact::saveREACTplot(p = plot1, figpath = figpath_new,
                           filename = paste0("shap_plot_",outcome,"_",modelvars),width =8, height=myheight)
  
  
  
  ### Final absolute shap value plot
  p_shap_mean <- cb_shaps_long %>% distinct(variable, .keep_all = T) %>% 
    ggplot(aes(x=mean_value, y=reorder(variable,mean_value))) +
    geom_col(fill = myCols[[2]])+
    labs(x="Mean SHAP value \n(variable importance in multivariable model)", y= "Predictor") +
    theme_bw()
  
  
  
  return(list(varimps=cb_varimps,
              varimps_plot=plot_varimps,
              plot_interacts=plot_interacts,
              permutation_varimps=cb_varimps_permute,
              # permutation_varimps_plot=plot_varimps_permute,
              # permutation_varimps_dist_plot=plot_varimps_permute_distribution,
              permutation_varimps_boxplot=plot_varimps_permute_boxplot,
              shaps=plot1,
              shap_mean=p_shap_mean))
}

# Permute catboost --------------------------------------------------------
# mymodel=cb_model

PermuteCatboostParallel <- function(mymodel,X, y, nperm = 100, lossmeasure="auc", 
         parallelise=T,nclust=20){
  predictors <- colnames(X)
  # perm_df= data.frame(matrix(data=NA, nrow=nperm, 
  #                            ncol = ncol(X), 
  #                            dimnames = list(1:nperm,predictors)))
  pool_unperm = catboost.load_pool(data =X,label = y, cat_features = 3:ncol(X))
  preds_full=catboost.predict(model = mymodel, pool = pool_unperm,verbose = T,prediction_type = "Probability")
  
  logloss_full= ModelMetrics::logLoss(as.vector(y),as.vector(preds_full))
  AUC_full=ModelMetrics::auc(as.vector(y),as.vector(preds_full))
  if(lossmeasure=="auc"){
    loss_full=AUC_full
  }else if(lossmeasure=="logloss"){
    loss_full=logloss_full
  }else{
    print("Please set lossmeasure to 'auc' or 'logloss'" )
  }
  if(parallelise){
    ### parallelised loop for getting stability coefficients
    cl <- parallel::makeCluster(nclust)
    doParallel::registerDoParallel(cl)
    foreach::getDoParWorkers()
    pb <- txtProgressBar(min=1, max = length(predictors), style = 3)
    progress <- function(n) setTxtProgressBar(pb,n)
    opts = list(progress=progress)
    perm_df <- foreach(i = 1:length(predictors), .options.snow = opts, .combine = cbind, 
                       .packages = c("caret", "pROC", "ModelMetrics", 
                                     "catboost","gtools")) %dopar% {
                                       perm_vec=(1:nperm)
                                       print(paste("Permuting", predictors[[i]]))
                                       for(j in 1:nperm){
                                         # print(paste0("Permutation ", j, " complete"))
                                         newX=X
                                         newX[,predictors[[i]]]=gtools::permute((unlist(X[,predictors[[i]]])))
                                         pool_perm = catboost.load_pool(data =newX,label = y, cat_features = 3:ncol(X))
                                         preds_new=catboost.predict(model = mymodel, pool = pool_perm,verbose = T,prediction_type = "Probability")
                                         logloss_new= ModelMetrics::logLoss(as.vector(y),as.vector(preds_new))
                                         auc_new= ModelMetrics::auc(as.vector(y),as.vector(preds_new))
                                         if(lossmeasure=="auc"){
                                           loss_new=auc_new
                                         }else if(lossmeasure=="logloss"){
                                           loss_new=logloss_new
                                         }
                                         perm_vec[j] = loss_new
                                       }                   
                                       
                                       return(perm_vec)
                                       
                                     }
    close(pb)
    stopCluster(cl)
  }else{
    # Unparallelised
    for(i in 1:length(predictors)){
      print(paste("Permuting", predictors[[i]]))
      for(j in 1:nperm){
        print(paste0("Permutation ", j, " complete"))
        newX=X
        newX[,predictors[[i]]]=gtools::permute((unlist(X[,predictors[[i]]])))
        pool_perm = catboost.load_pool(data =newX,label = y, cat_features = 3:ncol(X))
        preds_new=catboost.predict(model = mymodel, pool = pool_perm,verbose = T,prediction_type = "Probability")
        logloss_new= ModelMetrics::logLoss(as.vector(y),as.vector(preds_new))
        auc_new= ModelMetrics::auc(as.vector(y),as.vector(preds_new))
        if(lossmeasure=="auc"){
          loss_new=auc_new
        }else if(lossmeasure=="logloss"){
          loss_new=logloss_new
        }
        perm_df[j,i] = loss_new
      }
    }
  }
  
  # wrangle output data frame
  perm_df <- perm_df %>% as.data.frame()
  colnames(perm_df) <- predictors
  perm_df_long <- perm_df %>% tidyr::pivot_longer(cols = predictors,names_repair ="minimal")
  if(lossmeasure=="auc"){
    perm_df_long$value=loss_full-perm_df_long$value
  }else if(lossmeasure=="logloss"){
    perm_df_long$value=perm_df_long$value-loss_full
  }
  lossname=paste0("mean_",lossmeasure,"_permute")
  lossname_sd=paste0("SD_",lossmeasure,"_permute")
  
  results.df = data.frame(predictor=predictors, 
                          mean_loss_permute = NA_real_,
                          SD_loss_permute = NA_real_,
                          prop_permutes_greater_than_unpermute=NA_real_,
                          mean_loss_permute_delta = NA_real_)
  
  results.df$mean_loss_permute <- colMeans(perm_df, na.rm = T)
  results.df$SD_loss_permute <- sapply(perm_df, sd)
  
  ### Calculate proportion of permuted runs that had a superior log loss to the unpermuted
  results.df$prop_permutes_greater_than_unpermute <- colSums(perm_df>=loss_full) /nperm
  
  ### Get deltas
  results.df$mean_loss_permute_delta =abs(loss_full-results.df$mean_loss_permute)
  results.df$mean_loss_permute_delta_lower <-abs(results.df$mean_loss_permute_delta- 1.96*(
    results.df$SD_loss_permute) / sqrt(nperm))
  results.df$mean_loss_permute_delta_upper <- abs(results.df$mean_loss_permute_delta+ 1.96*(
    results.df$SD_loss_permute) / sqrt(nperm))
  
  return(list(summary_stats=results.df,
              full_results=perm_df_long))
}





# function to plot ROCS
plotReactROC <- function(list_of_preds=NULL,
                         events=NULL,
                         title ="A ROC curve",
                         subtitle ="Plotted on some dummy data"){
  
  if(is.null(list_of_preds)){
    print("Please supply a list of predicted values. Each list item should be a vector of
          predicted values, of the same length as the events vectors")
    break
  }
  if(is.null(events)){
    print("Please supply 'events'. Events should be a vector of ones and zeros corresponding 
          to binary event outcomes")
    break
  }
  
  i=1
  res_list=list()
  for(i in 1:length(list_of_preds)){
    auc_ci_mod <- pROC::ci.auc(events,list_of_preds[[i]])
    auc_mod <- pROC::auc(events,list_of_preds[[i]])
    roc_mod <- pROC::roc(events,list_of_preds[[i]])
    ci.sp.obj <- pROC::ci.sp(roc_mod,sensitivities=seq(0,1,0.01), boot.n=500)
    
    dat_ci=as.data.frame(ci.sp.obj) %>% 
      rownames_to_column(var = "sp") %>% 
      mutate(sp=as.numeric(sp),
             lower = `2.5%`, se=`50%`,
             upper=`97.5%`)
    dat_ci$model = paste0(names(list_of_preds)[[i]],", AUC = ",
                          round(auc_ci_mod[[2]],3)," [",
                          round(auc_ci_mod[[1]],3),"-",
                          round(auc_ci_mod[[3]],3),"]"
    )
    res_list[[i]] = dat_ci
  }
  out_df=bind_rows(res_list)
  
  p=out_df %>% ggplot() +
    geom_line(aes(x=sp, y=se, col = model)) +
    scale_x_reverse() +
    OverReact::theme_react(subtitle_size = 10) +
    OverReact::scale_color_imperial(palette = "cool") +
    OverReact::scale_fill_imperial(palette = "cool") +
    geom_ribbon(aes(x=sp, ymin=lower, ymax = upper, fill = model), alpha=0.2)+
    labs(x="Specificity", y="Sensitivity", col = "", fill = "") +
    theme(legend.position = c(0.7,0.19)) +
    geom_abline(slope = 1,intercept = 1, linetype = "dashed", size=0.2) +
    labs(title=title,subtitle=stringr::str_wrap(subtitle,width = 80))
  p
  return(p)
  
}
