
# define myCols
myCols =c("#002147", "#006EAF", "#00ACD7", "#379f9f", "#9D9D9D", "#BBCE00", "#D24000", "#E40043", "#960078", "#FFDD00")

# Summary of stability selection results ----------------------------------

getStabSummary=function(stab_out,y_test,X_test,family="binomial",maxval=30){
  
  # get optimal parameters
  opt_iter=which.max(stab_out$S)
  opt_q=stab_out$Q[opt_iter]
  opt_thresh=stab_out$P[opt_iter]
  
  # selection proportions
  selprop=focus::SelectionProportions(stab_out)
 
  #extract sds
  
  # Extract betas from optimal model
  betasExp=apply(stab_out$Beta[ArgmaxId(stab_out)[1],,],1,FUN = function(x) {mean(exp(x)[x!=0])})
  sdsExp=apply(stab_out$Beta[ArgmaxId(stab_out)[1],,],1,FUN = function(x) {sd(exp(x)[x!=0])})
  # Extract betas from optimal model with no exponent
  betas_noExp=apply(stab_out$Beta[ArgmaxId(stab_out)[1],,],1,FUN = function(x) {mean(x[x!=0])})
  sds_noExp=apply(stab_out$Beta[ArgmaxId(stab_out)[1],,],1,FUN = function(x) {sd((x)[x!=0])})
  
  # If we are using a logistic model, convert betas to odds ratio
  if(family=="binomial"){
    # Extract betas from optimal model
    betas=betasExp
    sds=sdsExp
    
  }else{
    # Extract betas from optimal model
    betas=betas_noExp
    sds=sds_noExp
  }
  
  # filter to only non-penalty.factor vars
  betas=betas[names(selprop)]
  sds=sds[names(selprop)]
  

  #bind into results df
  results_df=data.frame(selprop=selprop,beta=betas,sd=sds, selected=selprop>=opt_thresh)
  results_df$variable=rownames(results_df)
  
  # get z score (ish) of betas
  results_df$z_score=abs(betas_noExp[names(selprop)])/sds_noExp[names(selprop)]
  
  # reorder by z-score
  results_df <- results_df %>% arrange(-selprop,-z_score)
  
  ### get aucs/rmse
  res=getIncrementalLoss(stab_out,y_test = y_test,X_test = X_test,family=family,maxval=maxval)
  
  
  # join
  results_df=res %>% 
    left_join(results_df)
  
  return(results_df)
}


# Summary of stability selection results ----------------------------------

getIncrementalSummary=function(stab_out,ydata,xdata,family="binomial",n_predictors=NULL,
                               quantiles = c(0.05, 0.95),
                               K = 100,
                               n_thr = 10){
  
  if(is.null(n_predictors)){
    n_predictors=ncol(xdata)
  }
  
  
  # get optimal parameters
  opt_iter=which.max(stab_out$S)
  opt_q=stab_out$Q[opt_iter]
  opt_thresh=stab_out$P[opt_iter]
  
  # selection proportions
  selprop=focus::SelectionProportions(stab_out)
  
  #extract sds
  
  # Extract betas from optimal model
  betasExp=apply(stab_out$Beta[ArgmaxId(stab_out)[1],,],1,FUN = function(x) {mean(exp(x)[x!=0])})
  sdsExp=apply(stab_out$Beta[ArgmaxId(stab_out)[1],,],1,FUN = function(x) {sd(exp(x)[x!=0])})
  # Extract betas from optimal model with no exponent
  betas_noExp=apply(stab_out$Beta[ArgmaxId(stab_out)[1],,],1,FUN = function(x) {mean(x[x!=0])})
  sds_noExp=apply(stab_out$Beta[ArgmaxId(stab_out)[1],,],1,FUN = function(x) {sd((x)[x!=0])})
  
  # If we are using a logistic model, convert betas to odds ratio
  if(family=="binomial"){
    # Extract betas from optimal model
    betas=betasExp
    sds=sdsExp
    
  }else{
    # Extract betas from optimal model
    betas=betas_noExp
    sds=sds_noExp
  }
  
  # # filter to only non-penalty.factor vars
  # betas=betas[names(selprop)]
  # sds=sds[names(selprop)]
  
  
  #bind into results df
  results_df=data.frame(beta=betas,sd=sds)
  results_df_sel=data.frame(selprop=selprop,selected=selprop>=opt_thresh)
  
  results_df$variable=rownames(results_df)
  results_df_sel$variable=rownames(results_df_sel)
  
  # get penalty vars
  pen_vars <- setdiff(results_df$variable,results_df_sel$variable)

  # get z score (ish) of betas
  results_df_sel$z_score=abs(betas_noExp[names(selprop)])/sds_noExp[names(selprop)]
  
  # Join dfs together
  results_df <- results_df %>% left_join(results_df_sel)
  
  results_df[results_df$variable %in% pen_vars,c(4,6)] <- c(1,Inf)
  results_df[results_df$variable %in% pen_vars,5] <- TRUE
  
  # reorder by z-score
  results_df <- results_df %>% arrange(-selprop,-z_score)
  
  
  
  ### get aucs/rmse
  perf <- focus::Incremental(xdata = xdata,
                                 ydata = ydata,
                                 stability = stab_out,
                                 family = family,
                             n_thr = n_thr,
                             K = K,
                                 n_predictors = n_predictors)
  
  perf_df <- as.data.frame(perf$AUC)
  colnames(perf_df) <- perf$names  
 
  
  quantiles <- sort(quantiles)
  if ("concordance" %in% names(perf)) {
    if ("lower" %in% names(perf)) {
      x <- perf$concordance
      xlower <- perf$lower
      xupper <- perf$upper
    }
    else {
      x <- sapply(perf$concordance, stats::median)
      xlower <- sapply(perf$concordance, stats::quantile, 
                       probs = quantiles[1], na.rm=T)
      xupper <- sapply(perf$concordance, stats::quantile, 
                       probs = quantiles[2], na.rm=T)
    }
  }
  if ("AUC" %in% names(perf)) {
    x <- sapply(perf$AUC, stats::median)
    xlower <- sapply(perf$AUC, stats::quantile, probs = quantiles[1], na.rm=T)
    xupper <- sapply(perf$AUC, stats::quantile, probs = quantiles[2], na.rm=T)
  }
  if ("Q_squared" %in% names(perf)) {
    x <- sapply(perf$Q_squared, stats::median)
    xlower <- sapply(perf$Q_squared, stats::quantile, probs = quantiles[1], na.rm=T)
    xupper <- sapply(perf$Q_squared, stats::quantile, probs = quantiles[2], na.rm=T)
  }
  
  
  res=data.frame(variable = perf$names,
                 loss=x,
                 loss_lower = xlower,
                 loss_upper=xupper)
  
  # join
  results_df=res %>% 
    left_join(results_df)
  
  return(results_df)
}








# Predictions with stability selection model ------------------------------

predictWithStabModel=function(stab_out, X_test,numvars=NULL,y_test,family="binomial"){
  
  # get optimal parameters
  opt_iter=which.max(stab_out$S)
  opt_q=stab_out$Q[opt_iter]
  opt_thresh=stab_out$P[opt_iter]
  
  # If no numvars is supplied, default to the optimal selected
  if(is.null(numvars)){
    numvars=opt_q
  }
  
  # selection proportions
  selprop=focus::SelectionProportions(stab_out)
  
  # Extract betas from optimal model
  betas=apply(stab_out$Beta[ArgmaxId(stab_out)[1],,],1,FUN = function(x) {mean(x[x!=0])})
  sds=apply(stab_out$Beta[ArgmaxId(stab_out)[1],,],1,FUN = function(x) {sd(x[x!=0])})
  betas=betas[names(selprop)]
  sds=sds[names(selprop)]
  beta_over_sd=abs(betas)/sds
 
  
  #get order
  selprop_nonzero=selprop[selprop>0]
  beta_over_sd_nonzero=beta_over_sd[selprop>0]
  df_order=data.frame(as.data.frame(selprop_nonzero),beta_over_sd_nonzero)
  myorder=rownames(arrange(df_order,-selprop_nonzero,-beta_over_sd_nonzero))
  
  #create model
  mymodel=glm(y_test~offset(as.matrix(X_test[,myorder[1:numvars],drop=FALSE])%*%matrix(betas[myorder[1:numvars]], ncol=1)), 
              family=family)
  out=as.vector(mymodel$fitted.values)
  return(out)
}




# Get AUCs from adding variables one by one -------------------------------

getIncrementalLoss=function(stab_out,y_test, X_test, family = "gaussian",maxval=30){
  # selection proportions
  # selection proportions
  selprop=focus::SelectionProportions(stab_out)
  
  # Extract betas from optimal model
  betas=apply(stab_out$Beta[ArgmaxId(stab_out)[1],,],1,FUN = function(x) {mean(x[x!=0])})
  sds=apply(stab_out$Beta[ArgmaxId(stab_out)[1],,],1,FUN = function(x) {sd(x[x!=0])})
  betas=betas[names(selprop)]
  sds=sds[names(selprop)]
  beta_over_sd=abs(betas)/sds
  
  #get order
  selprop_nonzero=selprop[selprop>0]
  beta_over_sd_nonzero=beta_over_sd[selprop>0]
  df_order=data.frame(as.data.frame(selprop_nonzero),beta_over_sd_nonzero)
  myorder=rownames(arrange(df_order,-selprop_nonzero,-beta_over_sd_nonzero))
  
  numvars=length(selprop_nonzero)
  res=data.frame(variable=myorder,loss=NA_real_,loss_lower=NA_real_,loss_upper=NA_real_)
  i <- 7
  # maxval=5
  # set upper threshold on number of variables to assess
  mymax <- min(maxval,numvars)
  
  for(i in 1:mymax){
    print(paste0("Processing variable ",i,": ",myorder[[i]]))
    myvars=myorder[1:i]
    # xdat=(stab_out$params$xdata)[,myvars] %>% as.data.frame()
    # colnames(xdat)=myvars
    # dat=cbind(y=unlist(as.data.frame(stab_out$params$ydata)[,1]),xdat)
    # mymod=glm(formula = as.formula(paste0("y ~ `",paste(myvars,collapse = "`+`"),"`")),data = dat,family = "binomial")
    mypreds=predictWithStabModel(stab_out = stab_out, X_test =X_test,numvars = i,y_test = y_test,family = family)
    # mypredsrefit=predict.glm(object = mymod,newdata = X_test[,myvars],type = "response")
    # plot(mypreds,mypredsrefit)
    if(family == "gaussian"){
      errors <- (as.vector(mypreds)-as.vector(y_test))
      rmse=sqrt(mean(errors^2))
      sd_errors = sd(errors)
      se_errors=sd_errors/sqrt(length(errors))
      rmse_lower = rmse-1.96*se_errors
      rmse_upper = rmse+1.96*se_errors
      res[i,2:4]=c(rmse,rmse_lower,rmse_upper)
    }else{
      auc_test <- pROC::auc(response=as.vector(y_test), predictor=as.vector(mypreds))
      roc_test<- pROC::roc(response=as.vector(y_test), predictor=as.vector(mypreds))
      auc_ci_test <- pROC::ci.auc(roc_test, conf.level = 0.95, method="bootstrap",
                                  boot.n =1000,boot.stratified = T, 
                                  reuse.auc=T, progress = "none")
      res[i,2:4]=c(auc_ci_test[2],auc_ci_test[1],auc_ci_test[3])
    }
    
    
    
  }
  return(res)
}


# PLot results ------------------------------------------------------------

plotStabResults=function(results_df,
                         opt_thresh=0.9, 
                         loss="AUC", 
                         plotOnlySelected=F, 
                         coefficient = "regression coefficient",
                         plotOnlyTopN = NULL){
  
  losslabel=loss
  
  if(plotOnlySelected){
    results_df <- results_df[results_df$selected,]
  }
  
  if(!is.null(plotOnlyTopN)){
    results_df <- results_df %>% arrange(-selprop) %>% top_n(plotOnlyTopN)
  }
  
  betaymin = min(results_df$beta - 1.96*results_df$sd)
  betaymax=max(results_df$beta + 1.96*results_df$sd)
  betayrange = betaymax-betaymin
  betaymin_new = betaymin-0.1*betayrange
  betaymax_new = betaymax+0.1*betayrange
  
  
  # create bold label vector
  boldlabel = case_when(results_df$selected ~ "black",
                        TRUE ~ "grey40")
  
  p1 <- results_df %>% 
    mutate(variable = factor(variable, levels = unique(variable)),
           mean_OR=exp(beta),
           Sign = ifelse(beta>0, ">1", "<1")) %>% 
    ggplot(aes(x=reorder(variable,-selprop), y= beta, col = Sign, alpha= selected)) +
    geom_point() +
    scale_alpha_manual(values = c(0.3,1)) +
    # geom_col( width=0.01) +
    geom_errorbar(aes(ymin = beta - 1.96*sd, ymax =beta + 1.96*sd),
                  width=0.2, size =0.5) +
    scale_colour_manual("Sign",values = c(">1"=myCols[c(4)],"<1"=myCols[c(7)]),
                        drop=FALSE,aesthetics = c("color","fill")) +
    OverReact::theme_react() +
    ylim(betaymin_new,betaymax_new) +
    geom_hline(yintercept = 0, linetype = "dashed")+
    theme(axis.text.x =element_text(angle=70,hjust = 1, colour =  boldlabel),
          axis.title.x = element_blank(),
          plot.subtitle = element_text(face="bold"),
          panel.border = element_rect(fill = "transparent", col = "transparent", colour = NA),
          legend.position = "none"
    ) +
    labs(y = paste0("Mean beta"), fill = "OR", subtitle = paste0("Average ",coefficient))
  p1
  
  
  
  p2 <- results_df %>% 
    mutate(variable = factor(variable, levels = unique(variable))) %>% 
    ggplot(aes(x=reorder(variable,-selprop), y= selprop, col = selected)) +
    geom_col( width=0.01) +
    geom_point() +
    geom_hline(yintercept = opt_thresh, linetype = "dashed") +
    OverReact::theme_react() +
    ylim(c(0,1)) +
    scale_color_manual(values = c(myCols[c(5)], "black"))  +
    theme(axis.text.x = element_blank(),
          axis.title.x = element_blank(),
          plot.subtitle = element_text(face="bold"),
          panel.border = element_rect(fill = "transparent", col = "transparent", colour = NA),
          legend.position = "none"
    ) +
    labs(y = "Selection prop.", subtitle = "Selection proportion") 
  p2
  
  ylimlower = min(results_df$loss_lower, na.rm=T)
  ylimupper = max(results_df$loss_upper, na.rm=T)
  
  
  p3 <- results_df %>% 
    mutate(index=row_number()) %>% 
    ggplot(aes(x=reorder(variable,index), y= loss, alpha=selected)) +
    geom_point() +
    scale_alpha_manual(values = c(0.3,1)) +
    # geom_col( width=0.01) +
    geom_errorbar(aes(ymin = loss_lower, ymax =loss_upper),
                  width=0.2, size =0.5) +
    scale_colour_manual(values = myCols[c(7,4)], drop=F) +
    OverReact::theme_react() +
    ylim(ylimlower,ylimupper) +
    # geom_hline(yintercept = 0, linetype = "dashed")+
    theme(axis.text.x = element_blank(),
          axis.title.x = element_blank(),
          plot.subtitle = element_text(face="bold"),
          panel.border = element_rect(fill = "transparent", col = "transparent", colour = NA),
          legend.position = "none"
    ) +
    labs(y = losslabel, fill = "OR", subtitle = paste0(losslabel, " of models adding each variable incrementally"))
  p3
  # Combine plots
  plot.comb <- p3/plot_spacer()/p2 / plot_spacer()/p1 + plot_layout(heights = c(4,0.2,3,0.2,4))
  plot.comb
  return(plot.comb)
}



# 
# 
# # define myCols
# myCols =c("#002147", "#006EAF", "#00ACD7", "#379f9f", "#9D9D9D", "#BBCE00", "#D24000", "#E40043", "#960078", "#FFDD00")
# 
# # Summary of stability selection results ----------------------------------
# 
# getStabSummary=function(stab_out,y_test,X_test,family="binomial"){
#   
#   # get optimal parameters
#   opt_iter=which.max(stab_out$S)
#   opt_q=stab_out$Q[opt_iter]
#   opt_thresh=stab_out$P[opt_iter]
#   # selection proportions
#   selprop=focus::SelectionProportions(stab_out)
#   # Extract betas from optimal model
#   betas=apply(stab_out$Beta[ArgmaxId(stab_out)[1],,],1,FUN = function(x) {mean(x[x!=0])})
#   # filter to only non-penalty.factor vars
#   betas=betas[names(selprop)]
#   #extract sds
#   sds=apply(stab_out$Beta[ArgmaxId(stab_out)[1],,],1,FUN = function(x) {sd(x[x!=0])})
#   sds=sds[names(selprop)]
#   
#   #bind into results df
#   results_df=data.frame(selprop=selprop,beta=betas,sd=sds, selected=selprop>=opt_thresh)
#   results_df$variable=rownames(results_df)
#   
#   
#   ### get aucs/rmse
#   res=getIncrementalLoss(stab_out,y_test = y_test,X_test = X_test,family=family)
#   
#   
#   # join
#   results_df=res %>% 
#     left_join(results_df)
#   
#   return(results_df)
# }
# 
# 
# # Predictions with stability selection model ------------------------------
# 
# 
# 
# predictWithStabModel=function(stab_out, X_test,numvars=NULL,y_test,family="binomial"){
#   # get optimal parameters
#   opt_iter=which.max(stab_out$S)
#   opt_q=stab_out$Q[opt_iter]
#   opt_thresh=stab_out$P[opt_iter]
#   
#   # If no numvars is supplied, default to the optimal selected
#   if(is.null(numvars)){
#     numvars=opt_q
#   }
#   
#   # selection proportions
#   selprop=focus::SelectionProportions(stab_out)
#   # Extract betas from optimal model
#   betas=apply(stab_out$Beta[ArgmaxId(stab_out)[1],,],1,FUN = function(x) {mean(x[x!=0])})
#   #create model
#   selprop_nonzero=selprop[selprop>0]
#   myorder=names(selprop_nonzero)[sort.list(selprop_nonzero, decreasing = TRUE)]
#   mymodel=glm(y_test~offset(as.matrix(X_test[,myorder[1:numvars],drop=FALSE])%*%matrix(betas[myorder[1:numvars]], ncol=1)), 
#               family=family)
#   out=as.vector(mymodel$fitted.values)
#   return(out)
# }
# 
# 
# 
# 
# # Get AUCs from adding variables one by one -------------------------------
# 
# getIncrementalLoss=function(stab_out,y_test, X_test, family = "gaussian",maxval=30){
#   # selection proportions
#   # selection proportions
#   selprop=focus::SelectionProportions(stab_out)
#   
#   # Extract betas from optimal model
#   betas=apply(stab_out$Beta[ArgmaxId(stab_out)[1],,],1,FUN = function(x) {mean(x[x!=0])})
#   sds=apply(stab_out$Beta[ArgmaxId(stab_out)[1],,],1,FUN = function(x) {sd(x[x!=0])})
#   betas=betas[names(selprop)]
#   sds=sds[names(selprop)]
#   beta_over_sd=abs(betas)/sds
#   
#   #get order
#   selprop_nonzero=selprop[selprop>0]
#   beta_over_sd_nonzero=beta_over_sd[selprop>0]
#   df_order=data.frame(as.data.frame(selprop_nonzero),beta_over_sd_nonzero)
#   myorder=rownames(arrange(df_order,-selprop_nonzero,-beta_over_sd_nonzero))
#   
#   numvars=length(selprop_nonzero)
#   res=data.frame(variable=myorder,loss=NA_real_,loss_lower=NA_real_,loss_upper=NA_real_)
#   # i <- 1
#   
#   # set upper threshold on number of variables to assess
#   mymax <- min(maxval,numvars)
#   
#   for(i in 1:mymax){
#     print(paste0("Processing variable ",i))
#     myvars=myorder[1:i]
#     # xdat=(stab_out$params$xdata)[,myvars] %>% as.data.frame()
#     # colnames(xdat)=myvars
#     # dat=cbind(y=unlist(as.data.frame(stab_out$params$ydata)[,1]),xdat)
#     # mymod=glm(formula = as.formula(paste0("y ~ `",paste(myvars,collapse = "`+`"),"`")),data = dat,family = "binomial")
#     mypreds=predictWithStabModel(stab_out = stab_out, X_test =X_test,numvars = i,y_test = y_test,family = family)
#     # mypredsrefit=predict.glm(object = mymod,newdata = X_test[,myvars],type = "response")
#     # plot(mypreds,mypredsrefit)
#     if(family == "gaussian"){
#       errors <- (as.vector(mypreds)-as.vector(y_test))
#       rmse=sqrt(mean(errors^2))
#       sd_errors = sd(errors)
#       se_errors=sd_errors/sqrt(length(errors))
#       rmse_lower = rmse-1.96*se_errors
#       rmse_upper = rmse+1.96*se_errors
#       res[i,2:4]=c(rmse,rmse_lower,rmse_upper)
#     }else{
#       auc_test <- pROC::auc(response=as.vector(y_test), predictor=as.vector(mypreds))
#       roc_test<- pROC::roc(response=as.vector(y_test), predictor=as.vector(mypreds))
#       auc_ci_test <- pROC::ci.auc(roc_test, conf.level = 0.95, method="bootstrap",
#                                   boot.n =1000,boot.stratified = T, reuse.auc=T, progress = "none")
#       res[i,2:4]=c(auc_ci_test[2],auc_ci_test[1],auc_ci_test[3])
#     }
#     
#     
#     
#   }
#   return(res)
# }
# 

# PLot results ------------------------------------------------------------
# 
# plotStabResultsStriped=function(results_df,opt_thresh=0.9, loss="AUC", 
#                          plotOnlySelected=F, coefficient = "regression coefficient",
#                          plotOnlyTopN = NULL){
#   
#   losslabel=loss
#   
#   if(plotOnlySelected){
#     results_df <- results_df[results_df$selected,]
#   }
#   
#   if(!is.null(plotOnlyTopN)){
#     results_df_2 <- results_df %>% arrange(-selprop) %>% top_n(plotOnlyTopN)
#   }
#   
#   betaymin = min(results_df$beta - 1.96*results_df$sd)
#   betaymax=max(results_df$beta + 1.96*results_df$sd)
#   betayrange = betaymax-betaymin
#   betaymin_new = betaymin-0.1*betayrange
#   betaymax_new = betaymax+0.1*betayrange
#   
#   
#   # create bold label vector
#   boldlabel = case_when(results_df$selected ~ "black",
#                         TRUE ~ "grey40")
#   
#   p1 <- results_df %>% 
#     mutate(variable = factor(variable, levels = unique(variable)),
#            mean_OR=exp(beta),
#            Sign = ifelse(beta>0, ">1", "<1"),
#            stripe_col = case_when(row_number() %%2 ==0 ~ "NA",
#                                   TRUE ~ "grey90")) %>% 
#     ggplot(aes(x=reorder(variable,-selprop, -z_score), y= beta, col = Sign, alpha= selected)) +
#     geom_tile(aes(x=reorder(variable, -selprop), y =1, height = Inf,
#                   col = NA, fill = stripe_col),alpha=0.4,
#               show.legend = F) +
#     geom_point() +
#     scale_alpha_manual(values = c(0.3,1)) +
#     scale_fill_manual(values = c("white","grey90")) +
#     # geom_col( width=0.01) +
#     geom_errorbar(aes(ymin = beta - 1.96*sd, ymax =beta + 1.96*sd),
#                   width=0.2, size =0.5) +
#     scale_colour_manual("Sign",values = c(">1"=myCols[c(4)],"<1"=myCols[c(7)]),
#                         drop=FALSE,aesthetics = c("col")) +
#     OverReact::theme_react() +
#     ylim(betaymin_new,betaymax_new) +
#     geom_hline(yintercept = 0, linetype = "dashed")+
#     theme(axis.text.x =element_text(angle=70,hjust = 1, colour =  boldlabel),
#           axis.title.x = element_blank(),
#           plot.subtitle = element_text(face="bold"),
#           panel.border = element_rect(fill = "transparent", col = "transparent", colour = NA),
#           legend.position = "none",
#           panel.grid.major.x  = element_blank(),
#           panel.grid.minor.x  = element_blank(),
#           axis.ticks.x = element_blank()
#           
#           
#     ) +
#     labs(y = paste0("Mean beta"), fill = "OR", subtitle = paste0("Average ",coefficient))
#   p1
#   
#   
#   
#   p2 <- results_df %>% 
#     mutate(variable = factor(variable, levels = unique(variable)),
#            stripe_col = case_when(row_number() %%2 ==0 ~ "NA",
#                                   TRUE ~ "grey90")) %>% 
#     ggplot(aes(x=reorder(variable,-selprop), y= selprop, col = selected)) +
#     geom_tile(aes(x=reorder(variable, -selprop), y =1, height = Inf,
#                   col = NA, fill = stripe_col),alpha=0.4,
#               show.legend = F) +
#     geom_col( width=0.01) +
#     geom_point() +
#     geom_hline(yintercept = opt_thresh, linetype = "dashed") +
#     OverReact::theme_react() +
#     ylim(c(0,1)) +
#     scale_color_manual(values = c(myCols[c(5)], "black"))  +
#     scale_fill_manual(values = c("white","grey90")) +
#     theme(axis.text.x = element_blank(),
#           axis.title.x = element_blank(),
#           plot.subtitle = element_text(face="bold"),
#           panel.border = element_rect(fill = "transparent", col = "transparent", colour = NA),
#           legend.position = "none",
#           panel.grid.major.x  = element_blank(),
#           panel.grid.minor.x  = element_blank(),
#           axis.ticks.x = element_blank()
#           
#     ) +
#     labs(y = "Selection prop.", subtitle = "Selection proportion") 
#   p2
#   
#   ylimlower = min(results_df$loss_lower, na.rm=T)
#   ylimupper = max(results_df$loss_upper, na.rm=T)
#   
#   
#   p3 <- results_df %>% 
#     mutate(variable = factor(variable, levels = unique(variable)),
#            index=row_number(),
#            stripe_col = case_when(row_number() %%2 ==0 ~ "NA",
#                                   TRUE ~ "grey90")) %>% 
#     ggplot(aes(x=reorder(variable,index), y= loss, alpha=selected)) +
#     geom_tile(aes(x=reorder(variable, index), y =ylimlower, height = Inf,
#                   col = NA, fill = stripe_col),alpha=0.4,
#               show.legend = F) +
#     geom_point() +
#     scale_alpha_manual(values = c(0.3,1)) +
#     # geom_col( width=0.01) +
#     geom_errorbar(aes(ymin = loss_lower, ymax =loss_upper),
#                   width=0.2, size =0.5) +
#     scale_fill_manual(values = c("white","grey90")) +
#     scale_colour_manual(values = myCols[c(7,4)], drop=F) +
#     OverReact::theme_react() +
#     ylim(ylimlower,ylimupper) +
#     # geom_hline(yintercept = 0, linetype = "dashed")+
#     theme(axis.text.x = element_blank(),
#           axis.title.x = element_blank(),
#           plot.subtitle = element_text(face="bold"),
#           panel.border = element_rect(fill = "transparent", col = "transparent", colour = NA),
#           legend.position = "none",
#           panel.grid.major.x  = element_blank(),
#           panel.grid.minor.x  = element_blank(),
#           axis.ticks.x = element_blank()
#     ) +
#     labs(y = losslabel, fill = "OR", subtitle = paste0(losslabel, " of models adding each variable incrementally"))
#   p3
#   # Combine plots
#   plot.comb <- p3/plot_spacer()/p2 / plot_spacer()/p1 + plot_layout(heights = c(4,0.1,3,0.1,4))
#   plot.comb
#   return(plot.comb)
# }
# 
# 


# Load up some fonts
extrafont::loadfonts()
# extrafont::font_import()
extrafont::loadfonts(device = "win")

# PLot results ------------------------------------------------------------

plotStabResultsStripedFlipped=function(results_df,
                                       stab_out,
                                       opt_thresh=0.9, 
                                       loss="AUC", 
                                plotOnlySelected=F, 
                                family = "binomial",
                                plotOnlyTopN = NULL,
                                lower_bound = 2^-3){
  
  losslabel=loss
  binary_switch=family == "binomial"
  
  # plotOnlyTopN=20
  if(plotOnlySelected){
    results_df <- results_df[results_df$selected,]
  }
  
  if(!is.null(plotOnlyTopN)){
    results_df <- results_df %>% arrange(-selprop,-z_score) %>% slice_head(n = plotOnlyTopN)
  }
  
  betaymin = min(results_df$beta - 1.96*results_df$sd, na.rm= T)
  betaymax=max(results_df$beta + 1.96*results_df$sd, na.rm= T)
  betayrange = betaymax-betaymin
  betaymin_new = betaymin-0.00001*betayrange
  betaymax_new = betaymax+0.00001*betayrange
  
  if(binary_switch){
    betaymin_new =max(betaymin_new,lower_bound)
  }
  
  nruns=stab_out$params$K
  # GET BETAS FOR DISTRIBUTION PLOT #
  # Extract betas from optimal model
  betasExp=exp(stab_out$Beta[ArgmaxId(stab_out)[1],,])
  # Extract betas from optimal model with no exponent
  betas_noExp=stab_out$Beta[ArgmaxId(stab_out)[1],,]

  # If we are using a logistic model, convert betas to odds ratio
  if(binary_switch){
    # Extract betas from optimal model
    betas=betasExp

  }else{
    # Extract betas from optimal model
    betas=betas_noExp
  }
  
  # get betas of non-penalty factor variables
  betas <- t(betas)[,as.character(results_df$variable)] %>%
    as.data.frame() %>%  
    pivot_longer(cols = as.character(results_df$variable)) %>% 
    rename(variable = name)
  
  
  
  # x-axis seq #
  x_seq=2^seq(log2((max(lower_bound,betaymin_new))),log2(ceiling(betaymax_new)),1)
  
  # create bold label vector
  boldlabel = rev(case_when(results_df$selected ~ "black",
                        TRUE ~ "grey40"))
  
  thresh=ifelse(binary_switch,1,0)
  
  
  # join betas with results df
  results_df <- results_df %>%  mutate(variable = factor(variable, levels = rev(unique(variable))),
                                       # mean_OR=exp(beta),
                                       Sign = ifelse(beta>thresh, "pos", "neg"),
                                       stripe_col = case_when(row_number() %%2 ==0 ~ "NA",
                                                              TRUE ~ "grey90"),
                                       lower = pmax(beta - 1.96*sd,betaymin_new, na.rm=T),
                                       upper= beta + 1.96*sd)
  betas <- betas %>% left_join(results_df, by = c("variable"="variable"))
  
  
  p1 <- betas %>% 
    mutate(variable = factor(variable, levels = rev(unique(variable))),
           alpha = case_when(selected == T ~ 1,
                             T ~ 0)) %>% 
    ggplot(aes(x=(variable), y= beta, col = Sign)) +
    geom_tile(data = results_df,aes(x=(variable), y =1, height = Inf,
                  col = NA, fill = stripe_col), alpha=0.4,
              show.legend = F) +
    ggbeeswarm::geom_quasirandom(aes(x=(variable), y= value, col = Sign), 
                                 inherit.aes = F, alpha = 10/nruns) +
    geom_boxplot(aes(x=(variable), y= value, col = Sign, alpha= alpha), fill = NA, inherit.aes = F) +
    # scale_alpha_manual(values = alpha, drop = F) +
    scale_fill_manual(values = c("white","grey90")) +
    coord_flip()+
    scale_colour_manual("Sign",values = c("pos"=myCols[c(4)],"neg"=myCols[c(7)]),
                        drop=FALSE,aesthetics = c("col")) +
    OverReact::theme_react() +
    # scale_y_continuous()
    {if(binary_switch)scale_y_continuous(trans = "log2", #breaks = x_seq, 
                                         labels =function(x) sprintf("%g", x))}+    
    {if(!binary_switch) ylim(betaymin_new,betaymax_new)}+
    geom_hline(yintercept = ifelse(binary_switch,1,0), linetype = "dashed")+
    theme(axis.text.y =element_text(colour =  boldlabel),
          axis.title.y = element_blank(),
          plot.subtitle = element_text(face="bold"),
          panel.border = element_rect(fill = "transparent", col = "transparent", colour = NA),
          legend.position = "none",
          panel.grid.major.y  = element_blank(),
          panel.grid.minor.y  = element_blank(),
          axis.ticks.y = element_blank()
    ) +
    labs(y = paste0(ifelse(binary_switch,"OR","beta")), fill = "OR", 
         subtitle = paste0("Distribution of ",ifelse(binary_switch,"OR",expression(beta)),"s"))
  p1

  
  p2 <- betas %>% 
    mutate(variable = factor(variable, levels = rev(unique(variable)))) %>%
    ggplot() +
    geom_tile(data = results_df,aes(x=(variable), y =1, height = Inf,
                                    col = NA, fill = stripe_col), alpha=0.4,
              show.legend = F) +
    geom_segment(aes(x=variable,xend=(variable), y= 0,yend= selprop, col = selected)) +
    geom_point(aes(x=(variable), y= selprop, col = selected)) +
    geom_hline(yintercept = opt_thresh, linetype = "dashed") +
    OverReact::theme_react() +
    ylim(c(0,1)) +
    scale_color_manual(values = c(myCols[c(5)], "black"), drop = F)  +
    scale_fill_manual(values = c("white","grey90")) +
    theme(axis.text.y  = element_blank(),
          axis.title.y = element_blank(),
          plot.subtitle = element_text(face="bold"),
          panel.border = element_rect(fill = "transparent", col = "transparent", colour = NA),
          legend.position = "none",
          panel.grid.major.y  = element_blank(),
          panel.grid.minor.y  = element_blank(),
          axis.ticks.y = element_blank() 
          
    ) +
    labs(y = "Selection prop.", subtitle = "Selection proportion")+
    coord_flip() 
  p2
  
  ylimlower = min(results_df$loss_lower, na.rm=T)
  ylimupper = max(results_df$loss_upper, na.rm=T)
  
  
  p3 <- betas %>% 
    mutate(variable = factor(variable, levels = rev(unique(variable)))) %>%
    ggplot(aes(x=(variable), y= loss, alpha=selected)) +
    geom_tile(data = results_df,aes(x=(variable), y =1, height = Inf,
                                    col = NA, fill = stripe_col), alpha=0.4,
              show.legend = F) +
    geom_point(aes(col=selected)) +
    # scale_alpha_manual(values = c(0.3,1)) +
    # geom_col( width=0.01) +
    geom_errorbar(aes(ymin = loss_lower, ymax =loss_upper,col=selected),
                  width=0.2, size =0.5) +
    scale_fill_manual(values = c("white","grey90")) +
    scale_color_manual(values = c(myCols[c(5)], "black"), drop = F)  +
    OverReact::theme_react() +
    ylim(ylimlower,ylimupper) +
    # geom_hline(yintercept = 0, linetype = "dashed")+
    theme(axis.text.y = element_blank(),
          axis.title.y = element_blank(),
          plot.subtitle = element_text(face="bold"),
          panel.border = element_rect(fill = "transparent", col = "transparent", colour = NA),
          legend.position = "none",
          panel.grid.major.y  = element_blank(),
          panel.grid.minor.y  = element_blank(),
          axis.ticks.y = element_blank()
    ) +
    labs(y = losslabel, fill = "OR",
         subtitle = paste0(losslabel, " of models adding \neach variable incrementally")) +
    coord_flip()
  p3
  # Combine plots
  plot.comb <- p1+plot_spacer()+p2 + plot_spacer()+p3 + plot_layout(widths = c(4,0.1,3,0.1,4))
  plot.comb
  return(plot.comb)
}






# 
# # Plot stability results violin ------------------------------------------
# 
# 
# plotStabResultsStripedFlipped=function(results_df,
#                                        stab_out,
#                                        opt_thresh=0.9, 
#                                        loss="AUC", 
#                                        plotOnlySelected=F, 
#                                        family = "binomial",
#                                        plotOnlyTopN = NULL,
#                                        lower_bound = 2^-3){
#   
#   losslabel=loss
#   binary_switch=family == "binomial"
#   
#   # plotOnlyTopN=20
#   if(plotOnlySelected){
#     results_df <- results_df[results_df$selected,]
#   }
#   
#   if(!is.null(plotOnlyTopN)){
#     results_df <- results_df %>% arrange(-selprop,-z_score) %>% slice_head(n = plotOnlyTopN)
#   }
#   
#   betaymin = min(results_df$beta - 1.96*results_df$sd, na.rm= T)
#   betaymax=max(results_df$beta + 1.96*results_df$sd, na.rm= T)
#   betayrange = betaymax-betaymin
#   betaymin_new = betaymin-0.00001*betayrange
#   betaymax_new = betaymax+0.00001*betayrange
#   
#   if(binary_switch){
#     betaymin_new =max(betaymin_new,lower_bound)
#   }
#   
#   nruns=stab_out$params$K
#   # GET BETAS FOR DISTRIBUTION PLOT #
#   # Extract betas from optimal model
#   betasExp=exp(stab_out$Beta[ArgmaxId(stab_out)[1],,])
#   # Extract betas from optimal model with no exponent
#   betas_noExp=stab_out$Beta[ArgmaxId(stab_out)[1],,]
#   
#   # If we are using a logistic model, convert betas to odds ratio
#   if(binary_switch){
#     # Extract betas from optimal model
#     betas=betasExp
#     
#   }else{
#     # Extract betas from optimal model
#     betas=betas_noExp
#   }
#   
#   # get betas of non-penalty factor variables
#   betas <- t(betas)[,as.character(results_df$variable)] %>%
#     as.data.frame() %>%  
#     pivot_longer(cols = as.character(results_df$variable)) %>% 
#     rename(variable = name)
#   
#   
#   
#   # x-axis seq #
#   x_seq=2^seq(log2((max(lower_bound,betaymin_new))),log2(ceiling(betaymax_new)),1)
#   
#   # create bold label vector
#   boldlabel = rev(case_when(results_df$selected ~ "black",
#                             TRUE ~ "grey40"))
#   
#   thresh=ifelse(binary_switch,1,0)
#   
#   
#   # join betas with results df
#   results_df <- results_df %>%  mutate(variable = factor(variable, levels = rev(unique(variable))),
#                                        # mean_OR=exp(beta),
#                                        Sign = ifelse(beta>thresh, "pos", "neg"),
#                                        stripe_col = case_when(row_number() %%2 ==0 ~ "NA",
#                                                               TRUE ~ "grey90"),
#                                        lower = pmax(beta - 1.96*sd,betaymin_new, na.rm=T),
#                                        upper= beta + 1.96*sd)
#   betas <- betas %>% left_join(results_df, by = c("variable"="variable"))
#   
#   
#   p1 <- betas %>% 
#     mutate(variable = factor(variable, levels = rev(unique(variable))),
#            alpha = case_when(selected == T ~ 1,
#                              T ~ 0)) %>% 
#     ggplot(aes(x=(variable), y= beta, col = Sign)) +
#     geom_tile(data = results_df,aes(x=(variable), y =1, height = Inf,
#                   col = NA, fill = stripe_col), alpha=0.3,
#               show.legend = F) +
#     scale_fill_manual(values = c("white","grey90")) +
#     ggnewscale::new_scale_fill() +
#     geom_violin(aes(x=(variable), y= value, 
#                                        col = Sign, alpha= alpha, fill =Sign), 
#                 inherit.aes = F,scale = "width") +
#     # scale_alpha_manual(values = alpha, drop = F) +
#     coord_flip()+
#     scale_fill_manual("Sign",values = c("pos"=myCols[c(4)],"neg"=myCols[c(7)]),
#                         drop=FALSE,aesthetics = c("col")) +
#     scale_colour_manual("Sign",values = c("pos"=myCols[c(4)],"neg"=myCols[c(7)]),
#                         drop=FALSE,aesthetics = c("col")) +
#     OverReact::theme_react() +
#     # scale_y_continuous()
#     {if(binary_switch)scale_y_continuous(trans = "log2", #breaks = x_seq, 
#                                          labels =function(x) sprintf("%g", x))}+    
#     {if(!binary_switch) ylim(betaymin_new,betaymax_new)}+
#     geom_hline(yintercept = ifelse(binary_switch,1,0), linetype = "dashed")+
#     theme(axis.text.y =element_text(colour =  boldlabel),
#           axis.title.y = element_blank(),
#           plot.subtitle = element_text(face="bold"),
#           panel.border = element_rect(fill = "transparent", col = "transparent", colour = NA),
#           legend.position = "none",
#           panel.grid.major.y  = element_blank(),
#           panel.grid.minor.y  = element_blank(),
#           axis.ticks.y = element_blank()
#     ) +
#     labs(y = paste0(ifelse(binary_switch,"OR","beta")), fill = "OR", 
#          subtitle = paste0("Distribution of ",ifelse(binary_switch,"OR",expression(beta)),"s"))
#   p1
#   
#   
#   p2 <- betas %>% 
#     mutate(variable = factor(variable, levels = rev(unique(variable)))) %>%
#     ggplot() +
#     geom_tile(aes(x=(variable), y =1, height = Inf,
#                   col = NA, fill = stripe_col),alpha=4/nruns,
#               show.legend = F) +
#     geom_segment(aes(x=variable,xend=(variable), y= 0,yend= selprop, col = selected)) +
#     geom_point(aes(x=(variable), y= selprop, col = selected)) +
#     geom_hline(yintercept = opt_thresh, linetype = "dashed") +
#     OverReact::theme_react() +
#     ylim(c(0,1)) +
#     scale_color_manual(values = c(myCols[c(5)], "black"), drop = F)  +
#     scale_fill_manual(values = c("white","grey90")) +
#     theme(axis.text.y  = element_blank(),
#           axis.title.y = element_blank(),
#           plot.subtitle = element_text(face="bold"),
#           panel.border = element_rect(fill = "transparent", col = "transparent", colour = NA),
#           legend.position = "none",
#           panel.grid.major.y  = element_blank(),
#           panel.grid.minor.y  = element_blank(),
#           axis.ticks.y = element_blank() 
#           
#     ) +
#     labs(y = "Selection prop.", subtitle = "Selection proportion")+
#     coord_flip() 
#   p2
#   
#   ylimlower = min(results_df$loss_lower, na.rm=T)
#   ylimupper = max(results_df$loss_upper, na.rm=T)
#   
#   
#   p3 <- betas %>% 
#     mutate(variable = factor(variable, levels = rev(unique(variable)))) %>%
#     ggplot(aes(x=(variable), y= loss, alpha=selected)) +
#     geom_tile(aes(x=(variable), y =ylimlower, height = Inf,
#                   col = NA, fill = stripe_col),alpha=4/nruns,
#               show.legend = F) +
#     geom_point(aes(col=selected)) +
#     scale_alpha_manual(values = c(0.3,1)) +
#     # geom_col( width=0.01) +
#     geom_errorbar(aes(ymin = loss_lower, ymax =loss_upper,col=selected),
#                   width=0.2, size =0.5) +
#     scale_fill_manual(values = c("white","grey90")) +
#     scale_color_manual(values = c(myCols[c(5)], "black"), drop = F)  +
#     OverReact::theme_react() +
#     ylim(ylimlower,ylimupper) +
#     # geom_hline(yintercept = 0, linetype = "dashed")+
#     theme(axis.text.y = element_blank(),
#           axis.title.y = element_blank(),
#           plot.subtitle = element_text(face="bold"),
#           panel.border = element_rect(fill = "transparent", col = "transparent", colour = NA),
#           legend.position = "none",
#           panel.grid.major.y  = element_blank(),
#           panel.grid.minor.y  = element_blank(),
#           axis.ticks.y = element_blank()
#     ) +
#     labs(y = losslabel, fill = "OR",
#          subtitle = paste0(losslabel, " of models adding \neach variable incrementally")) +
#     coord_flip()
#   p3
#   # Combine plots
#   plot.comb <- p1+plot_spacer()+p2 + plot_spacer()+p3 + plot_layout(widths = c(4,0.1,3,0.1,4))
#   plot.comb
#   return(plot.comb)
# }
# 
# 
# 








