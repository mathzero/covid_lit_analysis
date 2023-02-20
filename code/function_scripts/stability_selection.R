
# define myCols
myCols =c("#002147", "#006EAF", "#00ACD7", "#379f9f", "#9D9D9D", "#BBCE00", "#D24000", "#E40043", "#960078", "#FFDD00")

# Summary of stability selection results ----------------------------------

getStabSummary=function(stab_out,y_test,X_test,family="binomial",maxval=30,
                        reorder_by_normalised_beta = F){
  
  # get optimal parameters
  opt_iter=which.max(stab_out$S)
  opt_q=stab_out$Q[opt_iter]
  opt_thresh=stab_out$P[opt_iter]
  
  # selection proportions
  selprop=sharp::SelectionProportions(stab_out)
 
  #extract sds
  
  # Extract betas from optimal model
  betasExp=apply(stab_out$Beta[ArgmaxId(stab_out)[1],,],1,FUN = function(x) {mean(exp(x)[x!=0])})
  sdsExp=apply(stab_out$Beta[ArgmaxId(stab_out)[1],,],1,FUN = function(x) {sd(exp(x)[x!=0])})
  # Extract betas from optimal model with no exponent
  betas_noExp=apply(stab_out$Beta[ArgmaxId(stab_out)[1],,],1,FUN = function(x) {mean(x[x!=0])})
  sds_noExp=apply(stab_out$Beta[ArgmaxId(stab_out)[1],,],1,FUN = function(x) {sd((x)[x!=0])})
  
  # If we are using a logistic model, convert betas to odds ratio
  if(family=="binomial" ){
    # Extract betas from optimal model
    betas=betasExp
    sds=sdsExp
    
  }else if(family=="cox"){
    # Extract betas from optimal model
    betas=betasExp
    sds=sdsExp
  }
  else{
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
  if(reorder_by_normalised_beta){
    results_df <- results_df %>% arrange(-selprop,-z_score)
  }else{
    results_df <- results_df %>% arrange(-selprop,-beta)
  }
  ### get aucs/rmse
  res=getIncrementalLoss(stab_out,y_test = y_test,X_test = X_test,
                         family=family,maxval=maxval)
  
  
  # join
  results_df=res %>% 
    left_join(results_df)
  
  return(results_df)
}


# Summary of stability selection results ----------------------------------

getIncrementalSummary=function(stab_out,ydata,xdata,
                               family="binomial",
                               n_predictors=NULL,
                               quantiles = c(0.05, 0.95),
                               K = 100,
                               n_thr = 10,
                               reorder_by_normalised_beta = F, getLoss=T){
  
  if(is.null(n_predictors)){
    n_predictors=ncol(xdata)
  }
  
  
  # get optimal parameters
  opt_iter=which.max(stab_out$S)
  opt_q=stab_out$Q[opt_iter]
  opt_thresh=stab_out$P[opt_iter]
  
  # selection proportions
  selprop=sharp::SelectionProportions(stab_out)
  
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
    
  }else if(family=="cox"){
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
  
  # Insert 'dummy' results for penalty vars
  results_df$selprop[results_df$variable %in% pen_vars] <- 1
  results_df$z_score[results_df$variable %in% pen_vars] <- Inf
  results_df$selected[results_df$variable %in% pen_vars] <- TRUE
  
  # reorder by z-score
  if(reorder_by_normalised_beta){
    results_df <- results_df %>% arrange(-selprop,-z_score)
  }else{
    results_df <- results_df %>% arrange(-selprop,-beta)
  }
  
  
  if(getLoss){

  ### get aucs/rmse
  perf <- IncrementalMW(xdata = xdata,
                                 ydata = ydata,
                                 stability = stab_out,
                                 family = family,
                             n_thr = n_thr,
                             tau = 0.5,
                             K = K,
                            time = 200,
                            n_predictors = n_predictors)

  if(family == "binomial"){
    perf_df <- as.data.frame(perf$AUC)
  }else if(family == "cox"){
    perf_df <- as.data.frame(perf$concordance)
  }else{
    perf_df <- as.data.frame(perf$Q_squared)
    
  }
  # colnames(perf_df) <- perf$names  
 
  # sort quantiles
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
  
  
}
  
  return(results_df)
}



# Amended Incremental Function --------------------------------------------

IncrementalMW <- function (xdata, ydata, stability = NULL, family = NULL, 
                           n_predictors = NULL, 
                           K = 100, tau = 0.8, seed = 1, 
                           n_thr = NULL, ij_method = FALSE, 
                           time = 1000) 
{
  if (!is.null(stability)) {
    if (class(stability) != "variable_selection") {
      stop("Argument 'stability' is not of class 'variable_selection'. This function can only be applied on the output of VariableSelection().")
    }
    if (!is.null(family)) {
      if (family != stability$methods$family) {
        warning(paste0("Arguments 'stability' and 'family' are not consistent. The family specified in argument stability was used: ", 
                       stability$methods$family))
      }
    }
    family <- stability$methods$family
    if (!family %in% c("cox", "binomial", "gaussian")) {
      stop("This function can only be applied with the following families: 'binomial', 'cox' or 'gaussian'.")
    }
  }
  else {
    if (is.null(family)) {
      stop("Argument 'family' must be provided. Possible values are: 'binomial', 'cox' or 'gaussian'.")
    }
  }
  if (is.null(n_predictors)) {
    if (!is.null(stability)) {
      n_predictors <- sum(SelectedVariables(stability))
      n_predictors <- n_predictors + sum(!colnames(xdata) %in% 
                                           names(SelectedVariables(stability)))
    }
    else {
      n_predictors <- ncol(xdata)
    }
    if (n_predictors == 0) {
      n_predictors <- 10
    }
  }
  n_predictors <- min(n_predictors, ncol(xdata))
  if (is.null(stability)) {
    myorder <- colnames(xdata)
  }
  else {
    
    #### NEW BIT ###
    betas= as.data.frame(apply(stability$Beta[ArgmaxId(stability)[1],,],1,FUN = function(x) {mean(x[x!=0])})) ### NEW
    names(betas)[[1]] <- "beta"
    betas$var=rownames(betas)
    
    selprops <- data.frame(var =names(SelectionProportions(stability)),selprop=as.numeric(SelectionProportions(stability)))
    selprops <- selprops %>% left_join(betas)
    myorder <- selprops %>% arrange(-selprop,-beta) %>%  
      # filter(selprop>0) %>% 
      dplyr::select(var) %>% unlist() %>% as.character()
    
    # myorder <- names(SelectionProportions(stability))[sort.list(SelectionProportions(stability), 
    #                                                             decreasing = TRUE)]
    myorder <- c(colnames(xdata)[!colnames(xdata) %in% names(SelectedVariables(stability))], 
                 myorder)
    mylen=length(colnames(xdata)[!colnames(xdata) %in% names(SelectedVariables(stability))])
    my.lower.limits = c(rep(0,mylen),rep(-Inf,length(myorder)-mylen))
    
    
    ### END NEW BIT ###
  }
  if (family == "binomial") {
    TPR <- FPR <- AUC <- list()
  }
  if (family == "cox") {
    if (ij_method) {
      concordance <- lower <- upper <- NULL
    }
    else {
      concordance <- list()
    }
  }
  if (family == "gaussian") {
    Q_squared <- list()
  }
  for (k in 1:n_predictors) {
    perf <- ExplanatoryPerformanceMW(xdata = xdata[, myorder[1:k], 
                                                 drop = FALSE], ydata = ydata, stability = NULL, family = family, 
                                   K = K, tau = tau, seed = seed, n_thr = n_thr, ij_method = ij_method, 
                                   time = time,lower.limits = my.lower.limits[[1:k]])
    if (family == "binomial") {
      FPR <- c(FPR, list(perf$FPR))
      TPR <- c(TPR, list(perf$TPR))
      AUC <- c(AUC, list(perf$AUC))
    }
    if (family == "cox") {
      if (ij_method) {
        concordance <- c(concordance, perf$concordance)
        lower <- c(lower, perf$lower)
        upper <- c(upper, perf$upper)
      }
      else {
        concordance <- c(concordance, list(perf$concordance))
      }
    }
    if (family == "gaussian") {
      Q_squared <- c(Q_squared, list(perf$Q_squared))
    }
  }
  if (family == "binomial") {
    out <- list(FPR = FPR, TPR = TPR, AUC = AUC)
  }
  if (family == "cox") {
    if (ij_method) {
      out <- list(concordance = concordance, lower = lower, 
                  upper = upper)
    }
    else {
      out <- list(concordance = concordance)
    }
  }
  if (family == "gaussian") {
    out <- list(Q_squared = Q_squared)
  }
  out <- c(out, names = list(myorder[1:n_predictors]))
  return(out)
}






# Explanatory performance (MW version for constrained betas) --------------

ExplanatoryPerformanceMW <- function (xdata, ydata, stability = NULL, family = NULL, K = 1, 
          tau = 0.8, seed = 1, n_thr = NULL, ij_method = FALSE, time = 1000,lower.limits =NULL) 
{
  if (!is.null(stability)) {
    if (class(stability) != "variable_selection") {
      stop("Argument 'stability' is not of class 'variable_selection'. This function can only be applied on the output of VariableSelection().")
    }
    if (!stability$methods$family %in% c("cox", "binomial", 
                                         "gaussian")) {
      stop("This function can only be applied with the following families: 'binomial', 'cox' or 'gaussian'.")
    }
    if (!is.null(family)) {
      if (family != stability$methods$family) {
        warning(paste0("Arguments 'stability' and 'family' are not consistent. The family specified in argument stability was used: ", 
                       stability$methods$family))
      }
    }
    family <- stability$methods$family
  }
  else {
    if (is.null(family)) {
      stop("Argument 'family' must be provided. Possible values are: 'gaussian', 'cox' or 'binomial'.")
    }
  }
  if (is.vector(ydata)) {
    ydata <- cbind(ydata)
  }
  if (ij_method) {
    K <- 1
  }
  if (family == "binomial") {
    metric <- "roc"
  }
  if (family == "cox") {
    metric <- "concordance"
  }
  if (family == "gaussian") {
    metric <- "q2"
  }
  n_folds <- 1
  withr::local_seed(seed)
  iter <- 0
  for (k in 1:K) {
    for (fold_id in 1:n_folds) {
      iter <- iter + 1
      if (n_folds == 1) {
        ids_test <- Resample(data = ydata, tau = 1 - 
                               tau, family = family)
      }
      else {
        if (fold_id == 1) {
          ids_folds <- Folds(data = ydata, n_folds = n_folds)
        }
        ids_test <- ids_folds[[fold_id]]
      }
      xtrain <- xdata[-ids_test, , drop = FALSE]
      ytrain <- ydata[-ids_test, , drop = FALSE]
      xtest <- xdata[ids_test, , drop = FALSE]
      ytest <- ydata[ids_test, , drop = FALSE]
      recalibrated <- RecalibrateMW(xdata = xtrain, ydata = ytrain, 
                                  stability = stability, family = family,
                                  lower.limits = lower.limits)
      
      
      if (tolower(metric) == "roc") {
        predicted <- stats::predict.glm(recalibrated, 
                                        newdata = as.data.frame(xtest), type = "response")
        roc <- ROC(predicted = predicted, observed = ytest, 
                   n_thr = n_thr)
        if (iter == 1) {
          n_thr <- length(roc$FPR) - 2
          FPR <- TPR <- matrix(NA, nrow = K * n_folds, 
                               ncol = length(roc$TPR))
          AUC <- rep(NA, K * n_folds)
        }
        FPR[iter, ] <- roc$FPR
        TPR[iter, ] <- roc$TPR
        AUC[iter] <- roc$AUC
      }
      if (tolower(metric) == "concordance") {
        predicted <- stats::predict(recalibrated, newdata = as.data.frame(xtest), 
                                    type = "lp")
        survobject <- survival::Surv(ytest[, "time"], 
                                     ytest[, "case"])
        S0 <- summary(survival::survfit(recalibrated), 
                      times = time, extend = TRUE)$surv
        S <- S0^exp(predicted)
        cstat <- survival::concordance(survobject ~ S)
        if (ij_method) {
          cindex <- cstat$concordance
          lower <- cindex - 1.96 * sqrt(cstat$var)
          upper <- cindex + 1.96 * sqrt(cstat$var)
        }
        else {
          if (iter == 1) {
            cindex <- rep(NA, K * n_folds)
          }
          cindex[iter] <- cstat$concordance
        }
      }
      if (tolower(metric) == "q2") {
        predicted <- stats::predict.lm(recalibrated, 
                                       newdata = as.data.frame(xtest))
        if (iter == 1) {
          Q_squared <- rep(NA, K * n_folds)
        }
        Q_squared[iter] <- stats::cor(predicted, ytest)^2
      }
    }
  }
  if (tolower(metric) == "roc") {
    out <- list(FPR = FPR, TPR = TPR, AUC = AUC)
  }
  if (tolower(metric) == "concordance") {
    if (ij_method) {
      out <- list(concordance = cindex, lower = lower, 
                  upper = upper)
    }
    else {
      out <- list(concordance = cindex)
    }
  }
  if (tolower(metric) == "q2") {
    out <- list(Q_squared = Q_squared)
  }
  return(out)
}










# Recalibrate MW ----------------------------------------------------------

RecalibrateMW <- function (xdata, ydata, stability = NULL, family = NULL, lower.limits = NULL,...) 
{
  use_pls <- FALSE
  if (!is.null(stability)) {
    if (!class(stability) %in% c("variable_selection", 
                                 "bi_selection")) {
      stop("Argument 'stability' is not of class 'variable_selection' or 'bi_selection'. This function can only be applied on the output of (i) VariableSelection() or (ii) BiSelection() for PLS models.")
    }
    if (class(stability) == "bi_selection") {
      CheckPackageInstalled("mixOmics")
      use_pls <- TRUE
      if (!stability$methods$family %in% c("gaussian")) {
        stop("This function can only be applied with the 'gaussian' family for PLS models.")
      }
    }
    else {
      if (!stability$methods$family %in% c("gaussian", 
                                           "cox", "binomial", "multinomial")) {
        stop("This function can only be applied with the following families for regression models: 'gaussian', 'cox', 'binomial', or 'multinomial'.")
      }
    }
    if (!is.null(family)) {
      if (family != stability$methods$family) {
        warning(paste0("Arguments 'stability' and 'family' are not consistent. The family specified in argument stability was used: ", 
                       stability$methods$family))
      }
    }
    family <- stability$methods$family
  }
  else {
    if (is.null(family)) {
      stop("Argument 'family' must be provided. Possible values are: 'gaussian', 'cox', 'binomial', or 'multinomial'.")
    }
  }
  if (is.vector(xdata)) {
    xdata <- cbind(xdata)
    colnames(xdata) <- "var"
  }
  if (family %in% c("binomial", "multinomial")) {
    if (!is.factor(ydata)) {
      if (!is.vector(ydata)) {
        if (ncol(ydata) != 1) {
          ydata <- DummyToCategories(ydata)
        }
      }
    }
  }
  if (use_pls) {
    mymodel <- PLS(xdata = xdata, ydata = ydata, selectedX = stability$selectedX, 
                   selectedY = stability$selectedY, family = family, 
                   ncomp = NULL, scale = stability$methods$scale)
  }
  else {
    if (is.null(stability)) {
      selected <- rep(1, ncol(xdata))
      names(selected) <- colnames(xdata)
    }
    else {
      selected <- SelectedVariables(stability)
    }
    ids <- c(names(selected)[which(selected == 1)], colnames(xdata)[!colnames(xdata) %in% 
                                                                      names(selected)])
    ids <- gsub("`", "", ids)
    colnames(xdata) <- gsub("`", "", colnames(xdata))
    if (length(ids) == 0) {
      message("No stably selected variables. Running a model with intercept only.")
      myformula <- stats::as.formula("ydata ~ 1")
    }
    else {
      myformula <- stats::as.formula(paste0("ydata ~ ", 
                                            paste(paste0("`", ids, "`"), collapse = " + ")))
    }
    if (family == "gaussian") {
      mymodel <- stats::lm(myformula, data = as.data.frame(xdata), 
                           ...)
    }
    if (family == "cox") {
      ydata <- survival::Surv(ydata[, "time"], ydata[, 
                                                     "case"])
      mymodel <- survival::coxph(myformula, data = as.data.frame(xdata), 
                                 ...)
    }
    if (family == "binomial") {
      if(ncol(xdata<2)){ 
        mymodel <- stats::glm(myformula, data = as.data.frame(xdata), 
                              family = stats::binomial(link = "logit", 
                                                       ...))
      }else{
        #### NEW BIT ####
        mymodel <- glmnet::glmnet(x = xdata, y=ydata, 
                                  family = "binomial",
                                  penalty.factor = rep(0,ncol(xdata)),
                                  lower.limits = lower.limits
        )
        ####### ENDS ####
      }
     
    }
    if (family == "multinomial") {
      mymodel <- nnet::multinom(myformula, data = as.data.frame(xdata), 
                                ...)
    }
  }
  return(mymodel)
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
  selprop=sharp::SelectionProportions(stab_out)
  
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
  selprop=sharp::SelectionProportions(stab_out)
  
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

# Load up some fonts
extrafont::loadfonts()
# extrafont::font_import()
extrafont::loadfonts(device = "win")


plotStabResultsStripedFlipped=function(results_df,
                                       stab_out,
                                       opt_thresh=0.9, 
                                       loss="AUC", 
                                       # left_panel_x_axis="OR",
                                plotOnlySelected=F, 
                                family = "binomial",
                                plotOnlyTopN = NULL,
                                lower_bound = 2^-3,
                                plot_aucs = T,
                                title = NULL,
                                subtitle_size = 9,
                                reorder_by_normalised_beta = F,
                                ...){
  
  losslabel=loss
  binary_switch=family%in% c("binomial", "cox")
  
  # plotOnlyTopN=20
  if(plotOnlySelected){
    results_df <- results_df[results_df$selected,]
  }
  
  if(reorder_by_normalised_beta){
    results_df <- results_df %>% 
      mutate(pentalty_factor_vars = case_when(is.infinite(z_score) ~ 1,
                                              T ~0))%>% 
      arrange(-selprop,-z_score)
  }else{
    results_df <- results_df %>% 
      mutate(pentalty_factor_vars = case_when(is.infinite(z_score) ~ 1,
                                              T ~0)) %>% 
      arrange(-selprop,-pentalty_factor_vars,-beta)
  }
  
  if(!is.null(plotOnlyTopN)){
    results_df <- results_df %>% slice_head(n = plotOnlyTopN)
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
  
  # get medians for plot shading
  beta_medians=betas %>% group_by(variable) %>% 
    summarise(median_beta=median(value, na.rm=T))
  
  # join back
  betas <-betas %>%  left_join(beta_medians)
  
  
  # x-axis seq #
  x_seq=2^seq(log2((max(lower_bound,betaymin_new))),log2(ceiling(betaymax_new)),1)
  
  # create bold label vector
  boldlabel = rev(case_when(results_df$selected ~ "black",
                        TRUE ~ "grey40"))
  
  thresh=ifelse(binary_switch,1,0)
  
  
  # join betas with results df
  results_df <- results_df %>%  mutate(variable = factor(variable, levels = rev(unique(variable))),
                                       # mean_OR=exp(beta),
                                       stripe_col = case_when(row_number() %%2 ==0 ~ "NA",
                                                              TRUE ~ "grey90"),
                                       lower = pmax(beta - 1.96*sd,betaymin_new, na.rm=T),
                                       upper= beta + 1.96*sd)
  betas <- betas %>% left_join(results_df, by = c("variable"="variable"))  %>% 
    mutate(Sign = case_when(median_beta>thresh ~  "pos", 
                            median_beta<thresh ~  "neg", 
                            median_beta==thresh ~ "neutral",
                            T ~ "neutral"))
  
  
  # %>% 
  #   mutate(variable=case_when(pentalty_factor_vars==1 ~ paste(variable,"*"),
  #                             T ~ variable))
  # results_df <- results_df %>%     
  #   mutate(variable=as.factor(case_when(pentalty_factor_vars==1 ~ paste(as.character(variable),"*"),
  #                             T~ as.character(variable))))

  
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
    scale_fill_manual(values = c("white","grey90")) +
    coord_flip()+
    scale_colour_manual("Sign",values = c("pos"=myCols[c(4)],"neg"=myCols[c(7)],
                                          "neutral" = myCols[c(5)]),
                        drop=FALSE,aesthetics = c("col")) +
    OverReact::theme_react(subtitle_size = subtitle_size) +
    {if(binary_switch)scale_y_continuous(trans = "log2", #breaks = x_seq, 
                                         labels =function(x) MASS::fractions(x))}+    
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
    labs(y = paste0(ifelse(family=="binomial","OR",ifelse(family=="cox","Hazard ratio","beta"))), fill = "OR", title = title,
         subtitle = paste0("Distribution of ",ifelse(family=="binomial","OR",ifelse(family=="cox","Hazard ratio","beta")),"s"))
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
    OverReact::theme_react(subtitle_size = subtitle_size) +
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
    geom_tile(data = results_df,aes(x=(variable), y =ylimlower, height = Inf,
                                    col = NA, fill = stripe_col), alpha=0.4,
              show.legend = F) +
    geom_point(aes(col=selected)) +
    # scale_alpha_manual(values = c(0.3,1)) +
    # geom_col( width=0.01) +
    geom_errorbar(aes(ymin = loss_lower, ymax =loss_upper,col=selected),
                  width=0.2, size =0.5) +
    OverReact::theme_react(subtitle_size = subtitle_size) +
    scale_fill_manual(values = c("white","grey90")) +
    scale_color_manual(values = c(myCols[c(5)], "black"), drop = F)  +
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
  if(plot_aucs){
    plot.comb <- p1+plot_spacer()+p2 + plot_spacer()+p3 + 
      plot_layout(widths = c(4,0.1,3,0.1,4))
  }else{
    plot.comb <- p1+plot_spacer()+p2  +
      plot_layout(widths = c(4,0.1,3)) 
  }
  plot.comb
  return(plot.comb)
}










