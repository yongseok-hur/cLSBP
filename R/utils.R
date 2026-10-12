# Helper functions used by the scripts: interval summaries (coverage, length, RMSE) of posterior draws,
# propensity score estimation, and the logistic stick-breaking weights used for predictive densities.

coverage_length_RMSE_function_LSBP <- function(target, test, ret, what, what_name, confi, Y_sc, Y_ctr){
  what_post_df <- ret %>% sapply(function(iter) test[[iter]][what_name][[1]]*Y_sc+ifelse(what_name == "mu", Y_ctr, 0))
  if(target == "CATE"){
    c(what_post_df %>% apply(1, .%>% quantile(probs = c(0.5 - confi/2/100, 0.5 + confi/2/100))) %>% sweep(2, what, FUN = "<") %>% apply(2, function(row){(row[1]==T)&&(row[2]==F)}) %>% mean,
      what_post_df %>% apply(1, .%>% quantile(probs = c(0.5 - confi/2/100, 0.5 + confi/2/100))) %>% apply(2, function(row){row[2]-row[1]}) %>% mean,
      sqrt(mean((what_post_df %>% apply(1, .%>% quantile(probs = 0.5)) - what)^2))
    )}
  else if(target == "ATE"){
    interval_ATE = what_post_df %>% apply(2, mean) %>% quantile(probs = c(0.5 - confi/2/100, 0.5 + confi/2/100)) %>% as.vector
    c((interval_ATE[1] < mean(what)) & (interval_ATE[2] > mean(what)),
      interval_ATE[2]-interval_ATE[1],
      abs(what_post_df %>% apply(2, mean) %>% median - mean(what))
     )
  }
  else{print("not ATE or CATE")}
}

coverage_length_RMSE_function_bcf <- function(target, test, ret, what, what_name, confi, n_chain = 4){
  # consider # of chain.
  what_post_df <- test[[what_name]][rep(ret, n_chain)+rep(0:(n_chain-1), each = length(ret))*last(ret), ]
  if(target == "CATE"){
    c(what_post_df %>% t %>% apply(1, .%>% quantile(probs = c(0.5 - confi/2/100, 0.5 + confi/2/100))) %>% sweep(2, what, FUN = "<") %>% apply(2, function(row){(row[1]==T)&&(row[2]==F)}) %>% mean,
      what_post_df %>% t %>% apply(1, .%>% quantile(probs = c(0.5 - confi/2/100, 0.5 + confi/2/100))) %>% apply(2, function(row){row[2]-row[1]}) %>% mean,
      sqrt(mean((what_post_df %>% t %>% apply(1, .%>% quantile(probs = c(0.5))) - what)^2))
    )}
  else if(target == "ATE"){
    interval_ATE = what_post_df %>% t %>% apply(2, mean) %>% quantile(probs = c(0.5 - confi/2/100, 0.5 + confi/2/100)) %>% as.vector
    c((interval_ATE[1] < mean(what)) & (interval_ATE[2] > mean(what)),
      interval_ATE[2]-interval_ATE[1],
      abs(what_post_df %>% t %>% apply(2, mean) %>% median - mean(what))
     )
  }
  else{print("not ATE or CATE")}
}

coverage_length_RMSE_function <- function(model, target, test, ret, what, what_name, confi, Y_sc, Y_ctr){
  if(model == "BCF"){
    coverage_length_RMSE_function_bcf(target, test, ret, what, what_name, confi)
  }else{
    coverage_length_RMSE_function_LSBP(target, test, ret, what, what_name, confi, Y_sc, Y_ctr)
  }
}

## Propensity score estimation
estimate_pi <- function(method, z, x){
  if(method == "BART"){
    fit <- BayesTree::bart(x, z, verbose = FALSE)
    # It automatically fits the probit propensity model

    pnorm(colMeans(fit$yhat.train))
  }
  else if(method == "CBPS"){
    fit <- CBPS::CBPS(z ~ x, ATT = 1, method = "exact")
    fit$fitted.values
  }
  else if(method == "LASSO"){
    # (the 2024 ACIC runs also called set.seed(533) here)
    fit_cv <- cv.glmnet(x, z, alpha = 1, family = "binomial")
    predict(fit_cv, s = fit_cv$lambda.min, newx = x, type = "response")[,1]
  }
  else if(method == "GBM"){
    # (the 2024 ACIC runs also called set.seed(533) here)
    df <- data.frame(Z = z, as.data.frame(x))
    fit_gbm <- gbm::gbm(
      formula = Z ~ .,
      data = df,
      distribution = "bernoulli",  # For binary outcome
      n.trees = 100,               # Number of trees
      interaction.depth = 3,       # Depth of each tree
      n.minobsinnode = 10,         # Minimum number of observations in terminal nodes
      shrinkage = 0.01,            # Learning rate
      cv.folds = 5,                # Number of cross-validation folds
      n.cores = 1,                # one core
      verbose = FALSE              # Do not print progress
    )
    predict(fit_gbm, as.data.frame(x), n.trees = fit_gbm$n.trees, type = "response")
  }
  else if(method == "GLM"){
    # standard logistic-regression PS (x already contains an intercept column -> no glm intercept)
    fit <- glm(z ~ x - 1, family = binomial)
    fit$fitted.values
  }
  else{
    print("Not appropriate input for PS estimation")
  }
}

logSigmoid_r <- function(product){
  if (product < 0){product + logSigmoid_r(-product)}
  else if (product > 18){exp(-2.0 * product) / 2.0 - exp(-product)}
  else{-log(1.0 + exp(-product))}
}

get_prob_from_logsigmoid <- function(probs){
  probs = probs - max(probs)
  probs = exp(probs)
  probs = probs/sum(probs)
  probs
}
