## ============================================================
## Table 5, step 3: RMSE, coverage and interval length over the five folds (in 1000 USD).
## Usage: Rscript scripts/pension/pension_cv_eval.R
## ============================================================

source("R/requirements.R") # including RcppArmadillo
source("R/wrappers.R")

case_list <- list(c(10, 0.2, 0.95), c(10, 0.1, 0.95), c(10, 0.05, 0.95), c(10, 0.02, 0.95),
                  c(20, 0.1, 0.95), c(20, 0.05, 0.95), c(20, 0.025, 0.95), c(20, 0.01, 0.95),
                  c(10, 0.2, 0.99), c(10, 0.1, 0.99), c(10, 0.05, 0.99), c(10, 0.02, 0.99),
                  c(20, 0.1, 0.99), c(20, 0.05, 0.99), c(20, 0.025, 0.99), c(20, 0.01, 0.99))

case <- 7

IG_DOF <- case_list[[case]][1]
IG_SCALE <- case_list[[case]][2]
Y_TRUNC <- case_list[[case]][3]
print(paste0("IG_DOF : ", IG_DOF, "IG_SCALE : ", IG_SCALE, ", Y_TRUNC : ", Y_TRUNC))

require(hdm)
data(pension)
pension <- pension %>% filter(e401==1)

Y <- pension$net_tfa
TRUNC_IDX <- abs(Y) < quantile(abs(Y), Y_TRUNC)
Z <- pension$p401[TRUNC_IDX] %>% as.numeric()
X <- pension %>% dplyr::select(a1, a2, a3, a4, i1, i2, i3, i4, i5, i6, fsize, nohs, hs, smcol, marr, twoearn, db, pira, hown) %>%
  mutate(intercept = 1) %>% as.matrix
X <- X[TRUNC_IDX, ]
X[, 11] <- scale(X[, 11]) # scale fsize. the only numerical value.

Y <- Y[TRUNC_IDX]
load(file = "config/pension_cv_folds.Rdata")

# fold specification

pension_CV_result <- rbind %>% do.call(1:5 %>% lapply(function(fold_num){
  train_idx <- which(cv_folds != fold_num)
  test_idx <- which(cv_folds == fold_num)

  X_train <- X[train_idx, ]
  Y_train <- Y[train_idx]
  Z_train <- Z[train_idx]

  Y_train <- scale(Y_train)
  centerY_train <- attr(Y_train, "scaled:center")
  scaleY_train <- attr(Y_train, "scaled:scale")
  attributes(Y_train) <- NULL

  X_test <- X[test_idx, ]
  Y_test <- Y[test_idx]
  Z_test <- Z[test_idx]

  # Test outcomes are standardized with the training mean and sd.

  Y_test <- (Y_test-centerY_train)/scaleY_train

  load(file = paste0("results/pension/CV_5fold/fold", fold_num, "/pihat_", "test", "_BART_Y", 100*Y_TRUNC, ".Rdata"))
  Xpi_test <- cbind(X_test, pihat_test)

  # BCF

  load(file = paste0("results/pension/CV_5fold/pred_bcf_CV_", fold_num, ".Rdata"))
  print(dim(pred_bcf_CV$yhat))

  # tau, mu, yhat : 60000 * 700 (or 699)

  n_train <- length(Y_train)
  n_test <- length(Y_test)

  ypreds_BCF <- data.frame(x     = Y_test,
                           Mean  = colMeans(pred_bcf_CV$yhat),
                           Median= apply(pred_bcf_CV$yhat, 2, median),
                           Low95 = apply(pred_bcf_CV$yhat, 2, quantile, 0.025),
                           Up95  = apply(pred_bcf_CV$yhat, 2, quantile, 0.975),
                           group = factor(rep("testing", n_test)),
                           idx   = test_idx)

  bcf_ypreds_result <- ypreds_BCF %>% mutate(
    lengths = Up95-Low95,
    bias = Median - x,
    covered = (Up95 > x) & (Low95 < x),
  ) %>% group_by(group) %>% summarise(
    length = mean(lengths),
    length_sd = sd(lengths),
    RMSE = sqrt(mean(bias^2)),
    coverage = mean(covered)
  ) %>% filter(group == "testing") %>% select(-group) %>% mutate(model = "BCF", fold_num = fold_num)

  # cLSBP

  y_posterior_predictive_cLSBP <- which(cv_folds == fold_num) %>% sapply(function(subject_idx){
    readRDS(file = paste0("results/pension/CV_5fold/fold", fold_num, "/pred_cLSBP_CV_", subject_idx, ".rds"))
  })

  ypreds_cLSBP <- data.frame(x     = Y_test,
                             Mean  = colMeans(y_posterior_predictive_cLSBP),
                             Median= apply(y_posterior_predictive_cLSBP, 2, median),
                             Low95 = apply(y_posterior_predictive_cLSBP, 2, quantile, 0.025),
                             Up95  = apply(y_posterior_predictive_cLSBP, 2, quantile, 0.975),
                             idx   = c(test_idx))

  cLSBP_ypreds_result <- ypreds_cLSBP %>% mutate(
    lengths = Up95-Low95,
    bias = Median - x,
    covered = (Up95 > x) & (Low95 < x),
  ) %>% summarise(
    length = mean(lengths),
    length_sd = sd(lengths),
    RMSE = sqrt(mean(bias^2)),
    coverage = mean(covered)
  ) %>% mutate(model = "cLSBP", fold_num = fold_num)
  print(fold_num)

  rbind(bcf_ypreds_result, cLSBP_ypreds_result)
}))

pension_CV_result %>% arrange(model, fold_num) %>% mutate(
  scales = rep(pension_fold_scales/1000, 2)
) %>% mutate(
  length_original_scale = length * scales,
  length_sd_original_scale = length_sd * scales,
  RMSE_original_scale = RMSE * scales
) %>% group_by(model) %>% summarise(
  RMSE_f = mean(RMSE_original_scale),
  RMSE_f_sd = sd(RMSE_original_scale),
  coverage_f = mean(coverage),
  coverage_f_sd = sd(coverage),
  length_f = mean(length_original_scale),
  length_f_sd = mean(length_sd_original_scale)
)

pension_fold_scales <- 1:5 %>% sapply(function(fold_num){
  train_idx <- which(cv_folds != fold_num)
  test_idx <- which(cv_folds == fold_num)

  X_train <- X[train_idx, ]
  Y_train <- Y[train_idx]
  Z_train <- Z[train_idx]

  Y_train <- scale(Y_train)
  centerY_train <- attr(Y_train, "scaled:center")
  attr(Y_train, "scaled:scale")})

pension_fold_scales

pension_CV_result %>% group_by(model) %>% summarise(
  length_f = mean(length),
  length_f_sd = sd(length),
  RMSE_f = mean(RMSE),
  RMSE_f_sd = sd(RMSE),
  coverage_f = mean(coverage),
  coverage_f_sd = sd(coverage)
)
